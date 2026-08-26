package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.builtin.AbstractBuiltInWithContext;
import it.denzosoft.jprolog.core.engine.SolverContext;
import it.denzosoft.jprolog.core.parser.TermParser;
import it.denzosoft.jprolog.core.terms.*;

import it.denzosoft.jprolog.util.TermUtils;
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;

/**
 * Implementation of read_term/2 predicate for advanced term reading.
 *
 * read_term(+Stream, -Term)
 * read_term(-Term, +Options)
 *
 * Supports ISO options:
 * - variable_names(Vars): unifies Vars with list of Name=Var pairs for all named variables
 * - singletons(Singles): unifies Singles with list of Name=Var pairs for singleton variables
 * - variables(VarList): unifies VarList with list of all variables in the read term
 */
public class ReadTerm extends AbstractBuiltInWithContext {

    /**
     * Create read_term predicate.
     *
     * @param solver The query solver
     */
    public ReadTerm(SolverContext solver) {
        super(solver);
    }

    @Override
    public boolean execute(Term term, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        // START_CHANGE: ISS-2025-0352 - extract arguments from the query and report success via solutions
        return executeWithContext(solver, term, bindings, solutions);
        // END_CHANGE: ISS-2025-0352
    }

    @Override
    public boolean solve(SolverContext solver, Map<String, Term> bindings) {
        Term[] args = getArguments();

        if (args.length == 2) {
            // Check if first argument is stream or term
            // START_CHANGE: ISS-2025-0352 - resolve the first argument so a stream bound via a variable routes correctly
            Term first = args[0].resolveBindings(bindings);
            // END_CHANGE: ISS-2025-0352
            Term second = args[1];

            if (isStream(first)) {
                // read_term(+Stream, -Term)
                return readTermFromStream(first, second, new ArrayList<>(), bindings);
            } else {
                // read_term(-Term, +Options)
                return readTermWithOptions(args[0], second, bindings);
            }
        }

        // START_CHANGE: ISS-2025-0354 - read_term(+Stream, -Term, +Options): the primary ISO 8.14.1 form
        if (args.length == 3) {
            List<ReadOption> options = parseReadOptions(args[2].resolveBindings(bindings), bindings);
            return readTermFromStream(args[0].resolveBindings(bindings), args[1], options, bindings);
        }
        // END_CHANGE: ISS-2025-0354

        return false;
    }

    /**
     * Read term from specified stream. Resolves the stream alias via {@link StreamManager}
     * (ISS-2025-0202 / v2.8.3). For aliases not registered, falls back to stdin.
     */
    private boolean readTermFromStream(Term streamTerm, Term termVar, List<ReadOption> options, Map<String, Term> bindings) {
        // START_CHANGE: ISS-2025-0204 - read syntax_errors option upfront
        String syntaxErrorsMode = "error";
        for (ReadOption opt : options) {
            if (opt.getType() == ReadOption.Type.SYNTAX_ERRORS && opt.getValue() instanceof Atom) {
                String v = ((Atom) opt.getValue()).getName();
                if ("error".equals(v) || "fail".equals(v) || "quiet".equals(v)) syntaxErrorsMode = v;
            }
        }
        // END_CHANGE: ISS-2025-0204
        try {
            // START_CHANGE: ISS-2025-0473 - the stream position BEFORE the term, for term_position/1
            it.denzosoft.jprolog.core.engine.v4.PrologStream posStream = StreamManager.stream(streamTerm);
            Term startPosition = (posStream == null) ? null : StreamProperty.positionTerm(posStream);
            // END_CHANGE: ISS-2025-0473
            // START_CHANGE: ISS-2025-0202 - honor stream argument; resolve to actual InputStream via StreamManager
            java.io.Reader reader = resolveReader(streamTerm);
            // END_CHANGE: ISS-2025-0202

            // START_CHANGE: ISS-2025-0408 - consume characters up to the ISO end token instead of
            // one physical line: multi-line terms, leading comments and several terms on one line
            // now all work (the shared reader persists, so the stream position is preserved).
            String input = Read.readTermText(reader);
            if (input == null) {
                // End of file
                return unifyTerm(termVar, new Atom("end_of_file"), bindings);
            }

            // The end token '.' is already consumed by readTermText
            String trimmed = input.trim();
            // END_CHANGE: ISS-2025-0408

            // Parse the input
            TermParser parser = new TermParser();
            Term parsedTerm = parser.parseTerm(trimmed);

            // START_CHANGE: LIM-009 - Collect variable information for read_term options
            // Collect all variables from the parsed term
            Map<String, Variable> namedVars = new LinkedHashMap<>();
            Map<String, Integer> varCounts = new LinkedHashMap<>();
            collectVariables(parsedTerm, namedVars, varCounts);

            // Process options
            for (ReadOption option : options) {
                switch (option.getType()) {
                    case VARIABLES: {
                        // Build list of all variables
                        Term varList = buildVariableList(namedVars);
                        if (option.getValue() != null) {
                            if (!unifyTerm(option.getValue(), varList, bindings)) {
                                return false;
                            }
                        }
                        break;
                    }
                    case VARIABLE_NAMES: {
                        // Build list of Name=Var pairs for all named (non-anonymous) variables
                        Term namesList = buildVariableNamesList(namedVars);
                        if (option.getValue() != null) {
                            if (!unifyTerm(option.getValue(), namesList, bindings)) {
                                return false;
                            }
                        }
                        break;
                    }
                    case SINGLETONS: {
                        // Build list of Name=Var pairs for singleton variables
                        Term singletonsList = buildSingletonsList(namedVars, varCounts);
                        if (option.getValue() != null) {
                            if (!unifyTerm(option.getValue(), singletonsList, bindings)) {
                                return false;
                            }
                        }
                        break;
                    }
                    // START_CHANGE: ISS-2025-0473 - term_position(Pos): the '$stream_position'/4
                    // term for the FIRST character of the term just read (SWI semantics).
                    case TERM_POSITION: {
                        if (option.getValue() != null && startPosition != null) {
                            if (!unifyTerm(option.getValue(), startPosition, bindings)) return false;
                        }
                        break;
                    }
                    // END_CHANGE: ISS-2025-0473
                    default:
                        break;
                }
            }
            // END_CHANGE: LIM-009

            // Unify with the term variable
            return unifyTerm(termVar, parsedTerm, bindings);

        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            // START_CHANGE: ISS-2025-0204 - honor syntax_errors option
            if ("error".equals(syntaxErrorsMode)) {
                throw new it.denzosoft.jprolog.core.exceptions.PrologEvaluationException(
                    "syntax_error(" + e.getMessage() + ")");
            }
            // fail or quiet -> return false silently
            return false;
            // END_CHANGE: ISS-2025-0204
        }
    }

    /**
     * Read term with options from current input.
     */
    private boolean readTermWithOptions(Term termVar, Term optionsTerm, Map<String, Term> bindings) {
        try {
            // Parse options
            List<ReadOption> options = parseReadOptions(optionsTerm, bindings);

            // Read from current input stream
            return readTermFromStream(new Atom("current_input"), termVar, options, bindings);

        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            return false;
        }
    }

    // START_CHANGE: LIM-009 - Variable collection for read_term options
    /**
     * Collect all variables from a term with occurrence counts.
     */
    private void collectVariables(Term term, Map<String, Variable> namedVars, Map<String, Integer> varCounts) {
        if (term instanceof Variable) {
            Variable var = (Variable) term;
            String name = var.getName();
            // Skip anonymous variables
            // START_CHANGE: ISS-2025-0193 - Fix operator precedence bug in variable classification
            if ((name != null && !name.startsWith("_")) || (name != null && name.length() > 1 && name.startsWith("_") && Character.isUpperCase(name.charAt(1)))) {
            // END_CHANGE: ISS-2025-0193
                namedVars.putIfAbsent(name, var);
                varCounts.merge(name, 1, Integer::sum);
            } else if (name != null && name.startsWith("_") && name.length() == 1) {
                // True anonymous variable (_) - skip from named vars
                // but still count for completeness
            } else if (name != null) {
                namedVars.putIfAbsent(name, var);
                varCounts.merge(name, 1, Integer::sum);
            }
        } else if (term instanceof CompoundTerm) {
            for (Term arg : term.getArguments()) {
                collectVariables(arg, namedVars, varCounts);
            }
        }
    }

    /**
     * Build a Prolog list of all variables.
     */
    private Term buildVariableList(Map<String, Variable> namedVars) {
        Term list = new Atom("[]");
        List<Variable> vars = new ArrayList<>(namedVars.values());
        // Build list in reverse order so it comes out in order
        for (int i = vars.size() - 1; i >= 0; i--) {
            list = new CompoundTerm(new Atom("."), Arrays.asList(vars.get(i), list));
        }
        return list;
    }

    /**
     * Build a Prolog list of Name=Var pairs for all named variables.
     */
    private Term buildVariableNamesList(Map<String, Variable> namedVars) {
        Term list = new Atom("[]");
        List<Map.Entry<String, Variable>> entries = new ArrayList<>(namedVars.entrySet());
        for (int i = entries.size() - 1; i >= 0; i--) {
            Map.Entry<String, Variable> entry = entries.get(i);
            Term pair = new CompoundTerm(new Atom("="),
                Arrays.asList(new Atom(entry.getKey()), entry.getValue()));
            list = new CompoundTerm(new Atom("."), Arrays.asList(pair, list));
        }
        return list;
    }

    /**
     * Build a Prolog list of Name=Var pairs for singleton variables.
     * Singleton variables appear exactly once in the term.
     */
    private Term buildSingletonsList(Map<String, Variable> namedVars, Map<String, Integer> varCounts) {
        Term list = new Atom("[]");
        List<Map.Entry<String, Variable>> singletons = new ArrayList<>();
        for (Map.Entry<String, Variable> entry : namedVars.entrySet()) {
            String name = entry.getKey();
            // Anonymous variables (starting with _) are not reported as singletons
            if (!name.startsWith("_") && varCounts.getOrDefault(name, 0) == 1) {
                singletons.add(entry);
            }
        }
        for (int i = singletons.size() - 1; i >= 0; i--) {
            Map.Entry<String, Variable> entry = singletons.get(i);
            Term pair = new CompoundTerm(new Atom("="),
                Arrays.asList(new Atom(entry.getKey()), entry.getValue()));
            list = new CompoundTerm(new Atom("."), Arrays.asList(pair, list));
        }
        return list;
    }
    // END_CHANGE: LIM-009

    /**
     * Parse read options from term.
     */
    // START_CHANGE: LIM-009 - Enhanced option parsing for read_term
    private List<ReadOption> parseReadOptions(Term optionsTerm, Map<String, Term> bindings) {
        List<ReadOption> options = new ArrayList<>();

        if (optionsTerm instanceof CompoundTerm && ".".equals(TermUtils.getFunctorName(optionsTerm))) {
            // Parse list of options
            List<Term> optionTerms = extractListElements(optionsTerm);

            for (Term optionTerm : optionTerms) {
                ReadOption option = parseReadOption(optionTerm);
                if (option != null) {
                    options.add(option);
                }
            }
        }

        return options;
    }
    // END_CHANGE: LIM-009

    /**
     * Parse a single read option.
     */
    private ReadOption parseReadOption(Term optionTerm) {
        if (optionTerm instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) optionTerm;
            String functor = TermUtils.getFunctorName(compound);

            if (TermUtils.getArity(compound) == 1) {
                Term arg = TermUtils.getArgument(compound, 0);

                switch (functor) {
                    case "variables":
                        return new ReadOption(ReadOption.Type.VARIABLES, arg);
                    case "variable_names":
                        return new ReadOption(ReadOption.Type.VARIABLE_NAMES, arg);
                    case "singletons":
                        return new ReadOption(ReadOption.Type.SINGLETONS, arg);
                    case "module":
                        return new ReadOption(ReadOption.Type.MODULE, arg);
                    // START_CHANGE: ISS-2025-0204 - syntax_errors(error|fail|quiet) option
                    case "syntax_errors":
                        return new ReadOption(ReadOption.Type.SYNTAX_ERRORS, arg);
                    // START_CHANGE: ISS-2025-0473 - engine v4 wave W7 (design B.11): term_position
                    case "term_position":
                        return new ReadOption(ReadOption.Type.TERM_POSITION, arg);
                    // END_CHANGE: ISS-2025-0473
                    // END_CHANGE: ISS-2025-0204
                    default:
                        return null;
                }
            }
        }

        return null;
    }

    /**
     * Check if a term represents a stream.
     */
    private boolean isStream(Term term) {
        // START_CHANGE: ISS-2025-0472 - wave W7: '$stream'(N) is the canonical stream term now, so
        // read_term(S, T) must recognise it or it silently becomes read_term(-Term, +Options) and
        // blocks on stdin.
        return IOStreamUtils.isStreamTerm(term);
        // END_CHANGE: ISS-2025-0472
    }

    // START_CHANGE: ISS-2025-0173 - Cache stdin BufferedReader to prevent resource leak
    /** Cached BufferedReader for System.in - must not be closed as that would close System.in */
    private static final BufferedReader STDIN_READER = new BufferedReader(new InputStreamReader(System.in));
    // END_CHANGE: ISS-2025-0173

    /**
     * Get current input stream.
     */
    private BufferedReader getCurrentInputStream() {
        // START_CHANGE: ISS-2025-0173 - Reuse cached reader instead of creating new one each call
        return STDIN_READER;
        // END_CHANGE: ISS-2025-0173
    }

    // START_CHANGE: ISS-2025-0202 - resolve stream argument to a Reader
    // START_CHANGE: ISS-2025-0472 - wave W7: through the STREAM's own decoder, not a process-global
    // per-alias BufferedReader cache that buffered ahead of every other I/O built-in (limit L-07).
    private java.io.Reader resolveReader(Term streamTerm) {
        it.denzosoft.jprolog.core.engine.v4.PrologStream ps = StreamManager.stream(streamTerm);
        if (ps == null) {
            String alias = null;
            if (streamTerm instanceof Atom) alias = ((Atom) streamTerm).getName();
            if (alias == null || "current_input".equals(alias) || "user_input".equals(alias)) return STDIN_READER;
            // unknown stream — fall back to stdin to preserve legacy behaviour
            return STDIN_READER;
        }
        if (ps == StreamManager.streams().userInput()) return STDIN_READER;
        if (!ps.isInput()) return STDIN_READER;
        return StreamManager.reader(ps);
    }
    // END_CHANGE: ISS-2025-0472
    // END_CHANGE: ISS-2025-0202

    /**
     * Unify a term with a variable.
     */
    private boolean unifyTerm(Term var, Term value, Map<String, Term> bindings) {
        if (var instanceof Variable) {
            String varName = ((Variable) var).getName();
            bindings.put(varName, value);
            return true;
        } else {
            // Try to unify with existing term
            return var.unify(value, bindings);
        }
    }

    /**
     * Extract elements from a Prolog list.
     */
    private List<Term> extractListElements(Term list) {
        List<Term> elements = new ArrayList<>();
        Term current = list;

        while (current instanceof CompoundTerm && ".".equals(TermUtils.getFunctorName(current)) && TermUtils.getArity(current) == 2) {
            CompoundTerm cons = (CompoundTerm) current;
            elements.add(TermUtils.getArgument(cons, 0));
            current = TermUtils.getArgument(cons, 1);
        }

        return elements;
    }

    /**
     * Represents a read option for read_term/2.
     */
    private static class ReadOption {
        enum Type {
            VARIABLES,
            VARIABLE_NAMES,
            SINGLETONS,
            MODULE,
            // START_CHANGE: ISS-2025-0204
            SYNTAX_ERRORS,
            // END_CHANGE: ISS-2025-0204
            // START_CHANGE: ISS-2025-0473
            TERM_POSITION
            // END_CHANGE: ISS-2025-0473
        }

        private final Type type;
        private final Term value;

        ReadOption(Type type, Term value) {
            this.type = type;
            this.value = value;
        }

        Type getType() { return type; }
        Term getValue() { return value; }
    }
}
