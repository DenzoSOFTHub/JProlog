package it.denzosoft.jprolog.builtin.persistence;

// START_CHANGE: ISS-2025-0126 - Persistence package for saving/loading clause database
import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.engine.SolverContext;
import it.denzosoft.jprolog.core.engine.Rule;
import it.denzosoft.jprolog.core.engine.KnowledgeBase;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;

/**
 * Persistence predicates for saving/loading the clause database:
 *   db_save/1           - Save entire clause database to a Prolog file
 *   db_load/1           - Load clauses from a file into the database
 *   db_save_predicate/2 - Save clauses of a specific predicate to a file
 *   persist/1           - Mark a predicate as persistent (auto-save on assert/retract)
 *   unpersist/1         - Remove persistence marking
 *   db_export_json/1    - Export clause database as JSON
 *   db_import_json/1    - Import clauses from JSON
 *   db_snapshot/1       - Take a snapshot of the current database state
 *   db_restore/1        - Restore database to a previously saved snapshot
 *   db_clear/0          - Clear all dynamic clauses from the database
 *   db_transaction/1    - Execute Goal within a transaction (commit on success, rollback on failure)
 *   db_sync/0           - Flush any cached/persistent data to disk
 *   db_batch_assert/1   - Assert a list of terms in a single batch operation
 */
public class PersistencePredicates implements BuiltInWithContext {

    public enum OperationType {
        DB_SAVE, DB_LOAD, DB_SAVE_PREDICATE,
        PERSIST, UNPERSIST,
        DB_EXPORT_JSON, DB_IMPORT_JSON,
        DB_SNAPSHOT, DB_RESTORE, DB_CLEAR,
        // START_CHANGE: ISS-2025-0175 - Transaction and batch support
        DB_TRANSACTION, DB_SYNC, DB_BATCH_ASSERT
        // END_CHANGE: ISS-2025-0175
    }

    private final OperationType operationType;

    /** Persistent predicates: predicate indicator (functor/arity) -> backing file path */
    private static final Map<String, String> persistentPredicates = new ConcurrentHashMap<>();

    /** Snapshots: handle -> serialized database content (Prolog text) */
    private static final Map<String, String> snapshots = new ConcurrentHashMap<>();

    /** Counter for generating unique snapshot handles */
    private static final AtomicLong snapshotCounter = new AtomicLong(0);

    // START_CHANGE: ISS-2025-0175 - Transaction state for buffered writes
    /** Whether a transaction is currently active */
    private static volatile boolean transactionActive = false;

    /** Buffered writes during an active transaction (list of Prolog term text) */
    private static final List<String> pendingWrites = Collections.synchronizedList(new ArrayList<>());

    /** Snapshot handle taken at transaction start for rollback */
    private static String transactionSnapshotHandle = null;
    // END_CHANGE: ISS-2025-0175

    public PersistencePredicates(OperationType operationType) {
        this.operationType = operationType;
    }

    @Override
    public boolean executeWithContext(SolverContext solver, Term query, Map<String, Term> bindings,
                                     List<Map<String, Term>> solutions) {
        Prolog engine = solver.getPrologContext();
        if (engine == null) {
            throw new PrologEvaluationException(operationType.name().toLowerCase()
                    + ": Prolog engine context not available.");
        }

        try {
            switch (operationType) {
                case DB_SAVE:            return doDbSave(engine, query, bindings, solutions);
                case DB_LOAD:            return doDbLoad(engine, query, bindings, solutions);
                case DB_SAVE_PREDICATE:  return doDbSavePredicate(engine, query, bindings, solutions);
                case PERSIST:            return doPersist(query, bindings, solutions);
                case UNPERSIST:          return doUnpersist(query, bindings, solutions);
                case DB_EXPORT_JSON:     return doDbExportJson(engine, query, bindings, solutions);
                case DB_IMPORT_JSON:     return doDbImportJson(engine, query, bindings, solutions);
                case DB_SNAPSHOT:        return doDbSnapshot(engine, query, bindings, solutions);
                case DB_RESTORE:         return doDbRestore(engine, query, bindings, solutions);
                case DB_CLEAR:           return doDbClear(engine, query, bindings, solutions);
                // START_CHANGE: ISS-2025-0175 - Transaction and batch operations
                case DB_TRANSACTION:     return doDbTransaction(engine, solver, query, bindings, solutions);
                case DB_SYNC:            return doDbSync(engine, query, bindings, solutions);
                case DB_BATCH_ASSERT:    return doDbBatchAssert(engine, solver, query, bindings, solutions);
                // END_CHANGE: ISS-2025-0175
                default:
                    throw new PrologEvaluationException("Unknown persistence operation: " + operationType);
            }
        } catch (IOException e) {
            throw new PrologEvaluationException(operationType.name().toLowerCase() + ": " + e.getMessage());
        }
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException(
                "Context-dependent built-in '" + operationType.name().toLowerCase()
                        + "' must be invoked with context");
    }

    // ----------------------------------------------------------------
    // db_save/1 - Save entire clause database to a Prolog file
    // ----------------------------------------------------------------
    private boolean doDbSave(Prolog engine, Term query, Map<String, Term> bindings,
                             List<Map<String, Term>> solutions) throws IOException {
        checkArity(query, 1, "db_save/1");
        String filename = resolveAtom(query.getArguments().get(0), bindings, "db_save/1");
        List<Rule> rules = engine.getRules();
        String content = rulesToPrologText(rules);
        Files.write(Paths.get(filename), content.getBytes());
        solutions.add(new HashMap<>(bindings));
        return true;
    }

    // ----------------------------------------------------------------
    // db_load/1 - Load clauses from a file into the database
    // ----------------------------------------------------------------
    private boolean doDbLoad(Prolog engine, Term query, Map<String, Term> bindings,
                             List<Map<String, Term>> solutions) throws IOException {
        checkArity(query, 1, "db_load/1");
        String filename = resolveAtom(query.getArguments().get(0), bindings, "db_load/1");
        String content = new String(Files.readAllBytes(Paths.get(filename)));
        engine.consult(content);
        solutions.add(new HashMap<>(bindings));
        return true;
    }

    // ----------------------------------------------------------------
    // db_save_predicate/2 - Save clauses of a specific predicate
    // ----------------------------------------------------------------
    private boolean doDbSavePredicate(Prolog engine, Term query, Map<String, Term> bindings,
                                      List<Map<String, Term>> solutions) throws IOException {
        checkArity(query, 2, "db_save_predicate/2");
        Term specTerm = query.getArguments().get(0).resolveBindings(bindings);
        String filename = resolveAtom(query.getArguments().get(1), bindings, "db_save_predicate/2");

        // Parse Functor/Arity from the first argument
        String functor;
        int arity;
        if (specTerm instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) specTerm;
            if ("/".equals(ct.getFunctor().getName()) && ct.getArguments().size() == 2) {
                Term functorTerm = ct.getArguments().get(0).resolveBindings(bindings);
                Term arityTerm = ct.getArguments().get(1).resolveBindings(bindings);
                if (!(functorTerm instanceof Atom)) {
                    throw new PrologEvaluationException(
                            "db_save_predicate/2: functor must be an atom, got: " + functorTerm);
                }
                if (!(arityTerm instanceof Number)) {
                    throw new PrologEvaluationException(
                            "db_save_predicate/2: arity must be a number, got: " + arityTerm);
                }
                functor = ((Atom) functorTerm).getName();
                arity = ((Number) arityTerm).getValue().intValue();
            } else {
                throw new PrologEvaluationException(
                        "db_save_predicate/2: first argument must be Functor/Arity, got: " + specTerm);
            }
        } else {
            throw new PrologEvaluationException(
                    "db_save_predicate/2: first argument must be Functor/Arity, got: " + specTerm);
        }

        List<Rule> allRules = engine.getRules();
        List<Rule> filtered = new ArrayList<>();
        for (Rule rule : allRules) {
            if (matchesPredicate(rule.getHead(), functor, arity)) {
                filtered.add(rule);
            }
        }

        String content = rulesToPrologText(filtered);
        Files.write(Paths.get(filename), content.getBytes());
        solutions.add(new HashMap<>(bindings));
        return true;
    }

    // ----------------------------------------------------------------
    // persist/1 - Mark a predicate as persistent
    // ----------------------------------------------------------------
    private boolean doPersist(Term query, Map<String, Term> bindings,
                              List<Map<String, Term>> solutions) {
        checkArity(query, 1, "persist/1");
        Term specTerm = query.getArguments().get(0).resolveBindings(bindings);
        String[] parsed = parseFunctorArity(specTerm, "persist/1");
        String key = parsed[0] + "/" + parsed[1];
        String backingFile = parsed[0] + "_" + parsed[1] + ".pl";
        persistentPredicates.put(key, backingFile);
        solutions.add(new HashMap<>(bindings));
        return true;
    }

    // ----------------------------------------------------------------
    // unpersist/1 - Remove persistence marking
    // ----------------------------------------------------------------
    private boolean doUnpersist(Term query, Map<String, Term> bindings,
                                List<Map<String, Term>> solutions) {
        checkArity(query, 1, "unpersist/1");
        Term specTerm = query.getArguments().get(0).resolveBindings(bindings);
        String[] parsed = parseFunctorArity(specTerm, "unpersist/1");
        String key = parsed[0] + "/" + parsed[1];
        persistentPredicates.remove(key);
        solutions.add(new HashMap<>(bindings));
        return true;
    }

    // ----------------------------------------------------------------
    // db_export_json/1 - Export clause database as JSON
    // ----------------------------------------------------------------
    private boolean doDbExportJson(Prolog engine, Term query, Map<String, Term> bindings,
                                   List<Map<String, Term>> solutions) throws IOException {
        checkArity(query, 1, "db_export_json/1");
        String filename = resolveAtom(query.getArguments().get(0), bindings, "db_export_json/1");
        List<Rule> rules = engine.getRules();

        StringBuilder json = new StringBuilder();
        json.append("[\n");
        for (int i = 0; i < rules.size(); i++) {
            Rule rule = rules.get(i);
            json.append("  {\"head\": ");
            json.append(escapeJsonString(rule.getHead().toString()));
            json.append(", \"body\": [");
            List<Term> body = rule.getBody();
            for (int j = 0; j < body.size(); j++) {
                json.append(escapeJsonString(body.get(j).toString()));
                if (j < body.size() - 1) {
                    json.append(", ");
                }
            }
            json.append("]}");
            if (i < rules.size() - 1) {
                json.append(",");
            }
            json.append("\n");
        }
        json.append("]");

        Files.write(Paths.get(filename), json.toString().getBytes());
        solutions.add(new HashMap<>(bindings));
        return true;
    }

    // ----------------------------------------------------------------
    // db_import_json/1 - Import clauses from JSON
    // ----------------------------------------------------------------
    private boolean doDbImportJson(Prolog engine, Term query, Map<String, Term> bindings,
                                   List<Map<String, Term>> solutions) throws IOException {
        checkArity(query, 1, "db_import_json/1");
        String filename = resolveAtom(query.getArguments().get(0), bindings, "db_import_json/1");
        String content = new String(Files.readAllBytes(Paths.get(filename)));

        // Simple JSON array parser - extract head and body from each object
        StringBuilder prologText = new StringBuilder();
        int pos = 0;
        while (pos < content.length()) {
            // Find next "head" key
            int headKeyIdx = content.indexOf("\"head\"", pos);
            if (headKeyIdx < 0) break;

            // Extract head value (a JSON string)
            int headValStart = content.indexOf('"', headKeyIdx + 6); // skip past "head"
            headValStart = content.indexOf('"', headValStart + 1); // skip past colon whitespace to opening quote
            // Actually, need to find the colon first, then the string
            int colonIdx = content.indexOf(':', headKeyIdx + 6);
            String headStr = extractJsonString(content, colonIdx + 1);
            if (headStr == null) break;

            // Find "body" key
            int bodyKeyIdx = content.indexOf("\"body\"", colonIdx);
            if (bodyKeyIdx < 0) break;
            int bodyColonIdx = content.indexOf(':', bodyKeyIdx + 6);
            int bracketStart = content.indexOf('[', bodyColonIdx);
            int bracketEnd = findMatchingBracket(content, bracketStart);
            if (bracketEnd < 0) break;

            String bodyArrayStr = content.substring(bracketStart + 1, bracketEnd).trim();
            List<String> bodyGoals = extractJsonStrings(bodyArrayStr);

            if (bodyGoals.isEmpty()) {
                prologText.append(headStr).append(".\n");
            } else {
                prologText.append(headStr).append(" :- ");
                prologText.append(String.join(", ", bodyGoals));
                prologText.append(".\n");
            }

            pos = bracketEnd + 1;
        }

        if (prologText.length() > 0) {
            engine.consult(prologText.toString());
        }
        solutions.add(new HashMap<>(bindings));
        return true;
    }

    // ----------------------------------------------------------------
    // db_snapshot/1 - Take a snapshot of the current database state
    // ----------------------------------------------------------------
    private boolean doDbSnapshot(Prolog engine, Term query, Map<String, Term> bindings,
                                  List<Map<String, Term>> solutions) {
        checkArity(query, 1, "db_snapshot/1");
        List<Rule> rules = engine.getRules();
        String content = rulesToPrologText(rules);
        String handle = "snapshot_" + snapshotCounter.incrementAndGet();
        snapshots.put(handle, content);

        // Unify the argument with the handle atom
        Term handleArg = query.getArguments().get(0).resolveBindings(bindings);
        Atom handleAtom = new Atom(handle);
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (handleArg.unify(handleAtom, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }

    // ----------------------------------------------------------------
    // db_restore/1 - Restore database to a previously saved snapshot
    // ----------------------------------------------------------------
    private boolean doDbRestore(Prolog engine, Term query, Map<String, Term> bindings,
                                 List<Map<String, Term>> solutions) {
        checkArity(query, 1, "db_restore/1");
        String handle = resolveAtom(query.getArguments().get(0), bindings, "db_restore/1");
        String content = snapshots.get(handle);
        if (content == null) {
            throw new PrologEvaluationException("db_restore/1: unknown snapshot handle '" + handle + "'");
        }

        // Clear existing rules and load the snapshot
        clearAllRules(engine);
        engine.consult(content);
        solutions.add(new HashMap<>(bindings));
        return true;
    }

    // ----------------------------------------------------------------
    // db_clear/0 - Clear all dynamic clauses from the database
    // ----------------------------------------------------------------
    private boolean doDbClear(Prolog engine, Term query, Map<String, Term> bindings,
                              List<Map<String, Term>> solutions) {
        // db_clear/0 takes no arguments
        if (query.getArguments() != null && !query.getArguments().isEmpty()) {
            throw new PrologEvaluationException("db_clear/0 takes no arguments.");
        }
        clearAllRules(engine);
        solutions.add(new HashMap<>(bindings));
        return true;
    }

    // START_CHANGE: ISS-2025-0175 - Transaction, sync, and batch assert operations

    // ----------------------------------------------------------------
    // db_transaction/1 - Execute Goal within a transaction
    // ----------------------------------------------------------------
    /**
     * db_transaction(Goal): Takes a snapshot, calls Goal. On success, commits any
     * buffered writes. On failure or exception, rolls back to the snapshot.
     */
    private boolean doDbTransaction(Prolog engine, SolverContext solver, Term query,
                                     Map<String, Term> bindings,
                                     List<Map<String, Term>> solutions) {
        checkArity(query, 1, "db_transaction/1");
        Term goal = query.getArguments().get(0).resolveBindings(bindings);

        // Take a snapshot for rollback
        List<Rule> rules = engine.getRules();
        String snapshotContent = rulesToPrologText(rules);
        String handle = "txn_" + snapshotCounter.incrementAndGet();
        snapshots.put(handle, snapshotContent);

        // Set transaction state
        transactionActive = true;
        pendingWrites.clear();
        transactionSnapshotHandle = handle;

        boolean success = false;
        try {
            // Execute the goal using the solver
            List<Map<String, Term>> goalSolutions = new ArrayList<>();
            success = solver.solveMeta(goal, new HashMap<>(bindings), goalSolutions);   // ISS-2025-0485

            if (success && !goalSolutions.isEmpty()) {
                // Commit: flush any pending buffered writes
                commitTransaction(engine);
                // Add the first goal solution as our solution
                solutions.add(goalSolutions.get(0));
            } else {
                // Rollback: restore the snapshot
                rollbackTransaction(engine, handle);
            }
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            // Rollback on exception
            rollbackTransaction(engine, handle);
            throw e;
        } finally {
            // Clear transaction state
            transactionActive = false;
            pendingWrites.clear();
            transactionSnapshotHandle = null;
            snapshots.remove(handle);
        }

        return success;
    }

    /**
     * Commit a transaction: apply all pending buffered writes to the engine.
     */
    private void commitTransaction(Prolog engine) {
        if (!pendingWrites.isEmpty()) {
            StringBuilder allWrites = new StringBuilder();
            for (String clause : pendingWrites) {
                allWrites.append(clause);
                if (!clause.endsWith(".")) {
                    allWrites.append(".");
                }
                allWrites.append("\n");
            }
            engine.consult(allWrites.toString());
        }
    }

    /**
     * Rollback a transaction: restore the engine to the pre-transaction snapshot.
     */
    private void rollbackTransaction(Prolog engine, String handle) {
        String content = snapshots.get(handle);
        if (content != null) {
            clearAllRules(engine);
            if (!content.isEmpty()) {
                engine.consult(content);
            }
        }
    }

    // ----------------------------------------------------------------
    // db_sync/0 - Flush any cached persistent data to disk
    // ----------------------------------------------------------------
    /**
     * db_sync: Forces all persistent predicates to be written to their backing files.
     */
    private boolean doDbSync(Prolog engine, Term query, Map<String, Term> bindings,
                              List<Map<String, Term>> solutions) throws IOException {
        // db_sync/0 takes no arguments
        if (query.getArguments() != null && !query.getArguments().isEmpty()) {
            throw new PrologEvaluationException("db_sync/0 takes no arguments.");
        }

        // Write all persistent predicates to their backing files
        for (Map.Entry<String, String> entry : persistentPredicates.entrySet()) {
            String predIndicator = entry.getKey();
            String backingFile = entry.getValue();
            String[] parts = predIndicator.split("/");
            String functor = parts[0];
            int arity = Integer.parseInt(parts[1]);

            List<Rule> allRules = engine.getRules();
            List<Rule> filtered = new ArrayList<>();
            for (Rule rule : allRules) {
                if (matchesPredicate(rule.getHead(), functor, arity)) {
                    filtered.add(rule);
                }
            }

            String content = rulesToPrologText(filtered);
            Files.write(Paths.get(backingFile), content.getBytes());
        }

        solutions.add(new HashMap<>(bindings));
        return true;
    }

    // ----------------------------------------------------------------
    // db_batch_assert/1 - Assert a list of terms in a single batch
    // ----------------------------------------------------------------
    /**
     * db_batch_assert(List): Takes a Prolog list of terms and asserts them all
     * at once into the database. More efficient than individual assert calls
     * as it constructs the Prolog text in one pass and consults it.
     */
    private boolean doDbBatchAssert(Prolog engine, SolverContext solver, Term query,
                                     Map<String, Term> bindings,
                                     List<Map<String, Term>> solutions) {
        checkArity(query, 1, "db_batch_assert/1");
        Term listTerm = query.getArguments().get(0).resolveBindings(bindings);

        // Parse the Prolog list into individual terms
        List<Term> elements = termToList(listTerm);
        if (elements == null) {
            throw new PrologEvaluationException(
                    "db_batch_assert/1: argument must be a list, got: " + listTerm);
        }

        if (elements.isEmpty()) {
            solutions.add(new HashMap<>(bindings));
            return true;
        }

        // Build a single Prolog text block with all clauses
        StringBuilder prologText = new StringBuilder();
        for (Term elem : elements) {
            Term resolved = elem.resolveBindings(bindings);
            String clauseStr = resolved.toString();

            if (transactionActive) {
                // Buffer the write during a transaction
                pendingWrites.add(clauseStr);
            } else {
                prologText.append(clauseStr);
                if (!clauseStr.endsWith(".")) {
                    prologText.append(".");
                }
                prologText.append("\n");
            }
        }

        // Consult all at once (unless in transaction mode where writes are buffered)
        if (!transactionActive && prologText.length() > 0) {
            engine.consult(prologText.toString());
        }

        solutions.add(new HashMap<>(bindings));
        return true;
    }

    /**
     * Check if a transaction is currently active.
     * Can be used by other predicates to decide whether to buffer writes.
     */
    public static boolean isTransactionActive() {
        return transactionActive;
    }

    /**
     * Add a pending write to the transaction buffer.
     * Used by external predicates that need to participate in transactions.
     */
    public static void addPendingWrite(String clause) {
        if (transactionActive) {
            pendingWrites.add(clause);
        }
    }

    /**
     * Convert a Prolog list term to a Java list of Terms.
     */
    private List<Term> termToList(Term term) {
        List<Term> result = new ArrayList<>();
        Term current = term;
        while (current instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) current;
            if (!".".equals(ct.getFunctor().getName()) || ct.getArguments().size() != 2) {
                return null; // Not a proper list
            }
            result.add(ct.getArguments().get(0));
            current = ct.getArguments().get(1);
        }
        if (current instanceof Atom && "[]".equals(((Atom) current).getName())) {
            return result;
        }
        return null; // Improper list
    }

    // END_CHANGE: ISS-2025-0175

    // ================================================================
    // Helper methods
    // ================================================================

    /**
     * Clear all user-defined rules from the knowledge base by retracting
     * all predicates found in the current predicate set.
     */
    private void clearAllRules(Prolog engine) {
        List<Rule> currentRules = new ArrayList<>(engine.getRules());
        // Collect all predicate indicators
        Set<String> predicates = new HashSet<>();
        for (Rule rule : currentRules) {
            Term head = rule.getHead();
            String functor;
            int arity;
            if (head instanceof CompoundTerm) {
                CompoundTerm ct = (CompoundTerm) head;
                functor = ct.getFunctor().getName();
                arity = ct.getArguments().size();
            } else if (head instanceof Atom) {
                functor = ((Atom) head).getName();
                arity = 0;
            } else {
                continue;
            }
            predicates.add(functor + "/" + arity);
        }
        // Use retractAll via constructing a general term for each predicate
        for (Rule rule : currentRules) {
            engine.getRules(); // Ensure we still have access
        }
        // Abolish each predicate
        KnowledgeBase kb = getKnowledgeBase(engine);
        if (kb != null) {
            for (String predInd : predicates) {
                String[] parts = predInd.split("/");
                kb.abolishPredicate(parts[0], Integer.parseInt(parts[1]));
            }
        }
    }

    /**
     * Access the KnowledgeBase from the Prolog engine via reflection,
     * since there is no public getter.
     */
    private KnowledgeBase getKnowledgeBase(Prolog engine) {
        try {
            java.lang.reflect.Field field = Prolog.class.getDeclaredField("knowledgeBase");
            field.setAccessible(true);
            return (KnowledgeBase) field.get(engine);
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            throw new PrologEvaluationException("Cannot access knowledge base: " + e.getMessage());
        }
    }

    /**
     * Convert a list of rules to Prolog source text.
     */
    private String rulesToPrologText(List<Rule> rules) {
        StringBuilder sb = new StringBuilder();
        for (Rule rule : rules) {
            List<Term> body = rule.getBody();
            if (body.isEmpty()) {
                sb.append(rule.getHead().toString()).append(".\n");
            } else {
                sb.append(rule.getHead().toString()).append(" :- ");
                sb.append(body.stream().map(Term::toString).collect(Collectors.joining(", ")));
                sb.append(".\n");
            }
        }
        return sb.toString();
    }

    /**
     * Check if a term matches a given functor/arity.
     */
    private boolean matchesPredicate(Term head, String functor, int arity) {
        if (head instanceof Atom) {
            return ((Atom) head).getName().equals(functor) && arity == 0;
        } else if (head instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) head;
            return ct.getFunctor().getName().equals(functor)
                    && ct.getArguments().size() == arity;
        }
        return false;
    }

    /**
     * Parse a Functor/Arity term and return [functor, arity] as strings.
     */
    private String[] parseFunctorArity(Term specTerm, String predicateName) {
        if (specTerm instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) specTerm;
            if ("/".equals(ct.getFunctor().getName()) && ct.getArguments().size() == 2) {
                Term functorTerm = ct.getArguments().get(0);
                Term arityTerm = ct.getArguments().get(1);
                if (functorTerm instanceof Atom && arityTerm instanceof Number) {
                    return new String[]{
                            ((Atom) functorTerm).getName(),
                            String.valueOf(((Number) arityTerm).getValue().intValue())
                    };
                }
            }
        }
        throw new PrologEvaluationException(
                predicateName + ": argument must be Functor/Arity, got: " + specTerm);
    }

    /**
     * Check expected arity of the query term.
     */
    private void checkArity(Term query, int expected, String predicateName) {
        List<Term> args = query.getArguments();
        int actual = (args == null) ? 0 : args.size();
        if (actual != expected) {
            throw new PrologEvaluationException(
                    predicateName + " requires " + expected + " argument(s), got " + actual + ".");
        }
    }

    /**
     * Resolve a term to an atom string value.
     */
    private String resolveAtom(Term term, Map<String, Term> bindings, String predicateName) {
        Term resolved = term.resolveBindings(bindings);
        if (!(resolved instanceof Atom)) {
            throw new PrologEvaluationException(predicateName + ": argument must be an atom, got: " + resolved);
        }
        return ((Atom) resolved).getName();
    }

    /**
     * Escape a string for JSON output.
     */
    private String escapeJsonString(String s) {
        StringBuilder sb = new StringBuilder("\"");
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            switch (c) {
                case '"':  sb.append("\\\""); break;
                case '\\': sb.append("\\\\"); break;
                case '\n': sb.append("\\n"); break;
                case '\r': sb.append("\\r"); break;
                case '\t': sb.append("\\t"); break;
                default:   sb.append(c);
            }
        }
        sb.append("\"");
        return sb.toString();
    }

    /**
     * Extract a JSON string value starting from the given position.
     * Skips whitespace, finds opening quote, reads until closing quote.
     */
    private String extractJsonString(String content, int startPos) {
        int pos = startPos;
        // Skip whitespace
        while (pos < content.length() && Character.isWhitespace(content.charAt(pos))) {
            pos++;
        }
        if (pos >= content.length() || content.charAt(pos) != '"') {
            return null;
        }
        pos++; // skip opening quote
        StringBuilder sb = new StringBuilder();
        while (pos < content.length()) {
            char c = content.charAt(pos);
            if (c == '\\' && pos + 1 < content.length()) {
                char next = content.charAt(pos + 1);
                switch (next) {
                    case '"':  sb.append('"'); break;
                    case '\\': sb.append('\\'); break;
                    case 'n':  sb.append('\n'); break;
                    case 'r':  sb.append('\r'); break;
                    case 't':  sb.append('\t'); break;
                    default:   sb.append(next);
                }
                pos += 2;
            } else if (c == '"') {
                return sb.toString();
            } else {
                sb.append(c);
                pos++;
            }
        }
        return null;
    }

    /**
     * Find the matching closing bracket for an opening bracket.
     */
    private int findMatchingBracket(String content, int openPos) {
        if (openPos < 0 || openPos >= content.length()) return -1;
        char open = content.charAt(openPos);
        char close;
        if (open == '[') close = ']';
        else if (open == '{') close = '}';
        else return -1;

        int depth = 1;
        boolean inString = false;
        for (int i = openPos + 1; i < content.length(); i++) {
            char c = content.charAt(i);
            if (c == '\\' && inString) {
                i++; // skip escaped character
                continue;
            }
            if (c == '"') {
                inString = !inString;
                continue;
            }
            if (!inString) {
                if (c == open) depth++;
                else if (c == close) {
                    depth--;
                    if (depth == 0) return i;
                }
            }
        }
        return -1;
    }

    /**
     * Extract all JSON string values from a comma-separated list within brackets.
     */
    private List<String> extractJsonStrings(String arrayContent) {
        List<String> result = new ArrayList<>();
        int pos = 0;
        while (pos < arrayContent.length()) {
            // Skip whitespace and commas
            while (pos < arrayContent.length()
                    && (Character.isWhitespace(arrayContent.charAt(pos))
                    || arrayContent.charAt(pos) == ',')) {
                pos++;
            }
            if (pos >= arrayContent.length()) break;
            if (arrayContent.charAt(pos) == '"') {
                String val = extractJsonString(arrayContent, pos);
                if (val != null) {
                    result.add(val);
                    // Advance past the extracted string
                    pos++; // skip opening quote
                    int skip = 0;
                    boolean escaped = false;
                    for (int i = pos; i < arrayContent.length(); i++) {
                        char c = arrayContent.charAt(i);
                        if (escaped) { escaped = false; continue; }
                        if (c == '\\') { escaped = true; continue; }
                        if (c == '"') { pos = i + 1; break; }
                    }
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        return result;
    }

    /**
     * Get the set of persistent predicates (for external access, e.g., by assert/retract).
     *
     * @return Unmodifiable map of predicate indicator to backing file path
     */
    public static Map<String, String> getPersistentPredicates() {
        return Collections.unmodifiableMap(persistentPredicates);
    }
}
// END_CHANGE: ISS-2025-0126
