package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.builtin.AbstractBuiltInWithContext;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.parser.TermParser;
import it.denzosoft.jprolog.core.terms.*;

import it.denzosoft.jprolog.util.TermUtils;
import java.io.BufferedReader;
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
    public ReadTerm(QuerySolver solver) {
        super(solver);
    }

    @Override
    public boolean execute(Term term, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        return solve(solver, bindings);
    }

    @Override
    public boolean solve(QuerySolver solver, Map<String, Term> bindings) {
        Term[] args = getArguments();

        if (args.length == 2) {
            // Check if first argument is stream or term
            Term first = args[0];
            Term second = args[1];

            if (isStream(first)) {
                // read_term(+Stream, -Term)
                return readTermFromStream(first, second, new ArrayList<>(), bindings);
            } else {
                // read_term(-Term, +Options)
                return readTermWithOptions(first, second, bindings);
            }
        }

        return false;
    }

    /**
     * Read term from specified stream.
     * START_CHANGE: ISS-2025-0182 - Built-in predicate bug fixes
     * KNOWN LIMITATION: The stream parameter (streamTerm) is currently ignored.
     * read_term/3 always reads from the current input stream (System.in) regardless
     * of which stream is specified. Proper stream handling requires integration with
     * the stream management subsystem which is not yet fully implemented.
     * END_CHANGE: ISS-2025-0182
     */
    private boolean readTermFromStream(Term streamTerm, Term termVar, List<ReadOption> options, Map<String, Term> bindings) {
        try {
            // Get stream (currently always returns current input - stream parameter is ignored)
            BufferedReader reader = getCurrentInputStream();

            // Read and parse term
            String input = reader.readLine();
            if (input == null) {
                // End of file
                return unifyTerm(termVar, new Atom("end_of_file"), bindings);
            }

            // Remove trailing period if present (Prolog term terminator)
            String trimmed = input.trim();
            if (trimmed.endsWith(".")) {
                trimmed = trimmed.substring(0, trimmed.length() - 1).trim();
            }

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
                    default:
                        break;
                }
            }
            // END_CHANGE: LIM-009

            // Unify with the term variable
            return unifyTerm(termVar, parsedTerm, bindings);

        } catch (Exception e) {
            return false;
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
        // Simplified stream detection
        if (term instanceof Atom) {
            String name = ((Atom) term).getName();
            return name.equals("current_input") || name.equals("current_output") ||
                   name.equals("user_input") || name.equals("user_output");
        }

        // Could also be a stream handle (compound term)
        return term instanceof CompoundTerm && "stream".equals(TermUtils.getFunctorName(term));
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
            MODULE
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
