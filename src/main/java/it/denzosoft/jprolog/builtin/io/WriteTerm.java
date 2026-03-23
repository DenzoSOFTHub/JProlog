package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.builtin.AbstractBuiltInWithContext;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.terms.*;

import java.io.PrintWriter;
import it.denzosoft.jprolog.util.TermUtils;
import java.util.*;

/**
 * Implementation of write_term/2 predicate for advanced term writing.
 *
 * write_term(+Stream, +Term)
 * write_term(+Term, +Options)
 *
 * Supports ISO options:
 * - quoted(true/false): quote atoms that need quoting
 * - numbervars(true/false): print '$VAR'(N) terms as variable names
 * - ignore_ops(true/false): write in functional notation ignoring operators
 * - max_depth(N): limit output depth (0 = unlimited)
 */
public class WriteTerm extends AbstractBuiltInWithContext {

    /**
     * Create write_term predicate.
     *
     * @param solver The query solver
     */
    public WriteTerm(QuerySolver solver) {
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
            Term first = args[0];
            Term second = args[1];

            if (isStream(first)) {
                // write_term(+Stream, +Term)
                return writeTermToStream(first, second, getDefaultWriteOptions(), bindings);
            } else {
                // write_term(+Term, +Options)
                return writeTermWithOptions(first, second, bindings);
            }
        }

        return false;
    }

    /**
     * Write term to specified stream.
     */
    private boolean writeTermToStream(Term streamTerm, Term term, WriteOptions options, Map<String, Term> bindings) {
        try {
            // Get output writer (simplified - in full implementation, use StreamManager)
            PrintWriter writer = getCurrentOutputStream();

            // Resolve the term through bindings before formatting
            Term resolvedTerm = term.resolveBindings(bindings);

            // Format and write the term
            String output = formatTerm(resolvedTerm, options, bindings);
            writer.print(output);
            writer.flush();

            return true;

        } catch (Exception e) {
            return false;
        }
    }

    /**
     * Write term with options to current output.
     */
    private boolean writeTermWithOptions(Term term, Term optionsTerm, Map<String, Term> bindings) {
        try {
            // Parse options
            WriteOptions options = parseWriteOptions(optionsTerm, bindings);

            // Write to current output stream
            return writeTermToStream(new Atom("current_output"), term, options, bindings);

        } catch (Exception e) {
            return false;
        }
    }

    /**
     * Format a term according to write options.
     */
    private String formatTerm(Term term, WriteOptions options, Map<String, Term> bindings) {
        return formatTermRecursive(term, options, bindings, 0);
    }

    /**
     * Recursively format a term with depth tracking.
     */
    // START_CHANGE: LIM-009 - Enhanced write_term options with depth tracking
    private String formatTermRecursive(Term term, WriteOptions options, Map<String, Term> bindings, int depth) {
        // Check max_depth limit
        if (options.maxDepth > 0 && depth >= options.maxDepth) {
            return "...";
        }

        if (term instanceof Atom) {
            return formatAtom((Atom) term, options);
        } else if (term instanceof it.denzosoft.jprolog.core.terms.Number) {
            return formatNumber((it.denzosoft.jprolog.core.terms.Number) term, options);
        } else if (term instanceof Variable) {
            return formatVariable((Variable) term, options, bindings, depth);
        } else if (term instanceof CompoundTerm) {
            return formatCompoundTerm((CompoundTerm) term, options, bindings, depth);
        } else {
            return term.toString();
        }
    }
    // END_CHANGE: LIM-009

    /**
     * Format an atom.
     */
    private String formatAtom(Atom atom, WriteOptions options) {
        String name = atom.getName();

        if (options.quoted && needsQuoting(name)) {
            return "'" + escapeAtom(name) + "'";
        } else {
            return name;
        }
    }

    /**
     * Format a number.
     */
    private String formatNumber(it.denzosoft.jprolog.core.terms.Number number, WriteOptions options) {
        double value = number.getValue();

        if (value == Math.floor(value) && !Double.isInfinite(value)) {
            // Integer
            return String.valueOf((long) value);
        } else {
            // Float
            return String.valueOf(value);
        }
    }

    /**
     * Format a variable.
     */
    private String formatVariable(Variable variable, WriteOptions options, Map<String, Term> bindings, int depth) {
        String name = variable.getName();

        // Check if variable is bound
        if (bindings.containsKey(name)) {
            Term value = bindings.get(name);
            return formatTermRecursive(value, options, bindings, depth);
        }

        return "_" + name;
    }

    // START_CHANGE: LIM-009 - numbervars support: convert '$VAR'(N) to variable names
    /**
     * Convert a numbervars index to a variable name.
     * 0='A', 1='B', ..., 25='Z', 26='A1', 27='B1', etc.
     */
    static String numberVarName(int n) {
        if (n < 0) return "_";
        int letter = n % 26;
        int suffix = n / 26;
        char ch = (char) ('A' + letter);
        if (suffix == 0) {
            return String.valueOf(ch);
        } else {
            return String.valueOf(ch) + suffix;
        }
    }
    // END_CHANGE: LIM-009

    /**
     * Format a compound term.
     */
    // START_CHANGE: LIM-009 - Enhanced compound term formatting with numbervars and ignore_ops
    private String formatCompoundTerm(CompoundTerm compound, WriteOptions options, Map<String, Term> bindings, int depth) {
        String functor = TermUtils.getFunctorName(compound);
        int arity = TermUtils.getArity(compound);

        // numbervars support: if numbervars(true) and term is '$VAR'(N), print as variable name
        if (options.numbervars && "$VAR".equals(functor) && arity == 1) {
            Term arg = TermUtils.getArgument(compound, 0).resolveBindings(bindings);
            if (arg instanceof it.denzosoft.jprolog.core.terms.Number) {
                int n = ((it.denzosoft.jprolog.core.terms.Number) arg).getValue().intValue();
                return numberVarName(n);
            }
        }

        // Special cases for lists
        if (arity == 2 && ".".equals(functor)) {
            // List notation
            if (!options.ignoreOps) {
                if (options.writeStrings && isCharacterList(compound)) {
                    return formatCharacterList(compound, options, bindings, depth);
                } else {
                    return formatList(compound, options, bindings, depth);
                }
            }
        }

        // Check for operator notation (only if not ignoring ops)
        if (!options.ignoreOps && isOperator(functor, arity)) {
            return formatOperator(compound, options, bindings, depth);
        }

        // Standard functor(args) notation
        StringBuilder sb = new StringBuilder();
        sb.append(formatAtom(new Atom(functor), options));

        if (arity > 0) {
            sb.append("(");
            for (int i = 0; i < arity; i++) {
                if (i > 0) sb.append(", ");
                sb.append(formatTermRecursive(TermUtils.getArgument(compound, i), options, bindings, depth + 1));
            }
            sb.append(")");
        }

        return sb.toString();
    }
    // END_CHANGE: LIM-009

    /**
     * Format a list in [a,b,c] notation.
     */
    private String formatList(CompoundTerm list, WriteOptions options, Map<String, Term> bindings, int depth) {
        StringBuilder sb = new StringBuilder();
        sb.append("[");

        Term current = list;
        boolean first = true;

        while (current instanceof CompoundTerm && ".".equals(TermUtils.getFunctorName(current)) && TermUtils.getArity(current) == 2) {
            if (!first) sb.append(", ");
            first = false;

            CompoundTerm cons = (CompoundTerm) current;
            sb.append(formatTermRecursive(TermUtils.getArgument(cons, 0), options, bindings, depth + 1));
            current = TermUtils.getArgument(cons, 1);
        }

        if (!(current instanceof Atom && "[]".equals(((Atom) current).getName()))) {
            // Non-empty tail
            sb.append("|");
            sb.append(formatTermRecursive(current, options, bindings, depth + 1));
        }

        sb.append("]");
        return sb.toString();
    }

    /**
     * Format character list as string.
     */
    private String formatCharacterList(CompoundTerm list, WriteOptions options, Map<String, Term> bindings, int depth) {
        StringBuilder sb = new StringBuilder();
        sb.append("\"");

        Term current = list;
        while (current instanceof CompoundTerm && ".".equals(TermUtils.getFunctorName(current)) && TermUtils.getArity(current) == 2) {
            CompoundTerm cons = (CompoundTerm) current;
            Term head = TermUtils.getArgument(cons, 0);

            if (head instanceof Atom) {
                String ch = ((Atom) head).getName();
                if (ch.length() == 1) {
                    sb.append(escapeCharacter(ch.charAt(0)));
                } else {
                    // Not a character - fall back to list notation
                    return formatList(list, options, bindings, depth);
                }
            } else {
                return formatList(list, options, bindings, depth);
            }

            current = TermUtils.getArgument(cons, 1);
        }

        sb.append("\"");
        return sb.toString();
    }

    // START_CHANGE: LIM-009 - Parse ISO write_term options: quoted/1, numbervars/1, ignore_ops/1, max_depth/1
    /**
     * Parse write options from term.
     */
    private WriteOptions parseWriteOptions(Term optionsTerm, Map<String, Term> bindings) {
        WriteOptions options = new WriteOptions();

        if (optionsTerm instanceof CompoundTerm && ".".equals(TermUtils.getFunctorName(optionsTerm))) {
            List<Term> optionTerms = extractListElements(optionsTerm);

            for (Term optionTerm : optionTerms) {
                Term resolved = optionTerm.resolveBindings(bindings);
                parseWriteOption(resolved, options);
            }
        }

        return options;
    }

    /**
     * Parse a single write option into the options object.
     */
    private void parseWriteOption(Term optionTerm, WriteOptions options) {
        if (optionTerm instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) optionTerm;
            String functor = TermUtils.getFunctorName(compound);
            int arity = TermUtils.getArity(compound);

            if (arity == 1) {
                Term arg = TermUtils.getArgument(compound, 0);
                boolean boolValue = isTrueAtom(arg);

                switch (functor) {
                    case "quoted":
                        options.quoted = boolValue;
                        break;
                    case "numbervars":
                        options.numbervars = boolValue;
                        break;
                    case "ignore_ops":
                        options.ignoreOps = boolValue;
                        break;
                    case "max_depth":
                        if (arg instanceof it.denzosoft.jprolog.core.terms.Number) {
                            options.maxDepth = ((it.denzosoft.jprolog.core.terms.Number) arg).getValue().intValue();
                        }
                        break;
                    default:
                        // Unknown option - ignore
                        break;
                }
            }
        } else if (optionTerm instanceof Atom) {
            // Legacy single-atom options
            String name = ((Atom) optionTerm).getName();
            switch (name) {
                case "quoted": options.quoted = true; break;
                case "numbervars": options.numbervars = true; break;
                case "ignore_ops": options.ignoreOps = true; break;
                default: break;
            }
        }
    }

    private boolean isTrueAtom(Term term) {
        return term instanceof Atom && "true".equals(((Atom) term).getName());
    }
    // END_CHANGE: LIM-009

    /**
     * Get default write options.
     */
    private WriteOptions getDefaultWriteOptions() {
        WriteOptions options = new WriteOptions();
        options.quoted = true;
        return options;
    }

    // Helper methods
    private boolean isStream(Term term) {
        return term instanceof Atom &&
               (((Atom) term).getName().equals("current_output") ||
                ((Atom) term).getName().equals("user_output"));
    }

    private PrintWriter getCurrentOutputStream() {
        return new PrintWriter(System.out);
    }

    private boolean needsQuoting(String atom) {
        if (atom.isEmpty()) return true;
        if ("[]".equals(atom) || "{}".equals(atom) || "!".equals(atom)) return false;
        // Operators and special atoms don't need quoting in standard contexts
        // An atom needs quoting if it doesn't match the lowercase-identifier pattern
        // and isn't a standard symbolic atom
        if (atom.matches("[a-z][a-zA-Z0-9_]*")) return false;
        // Symbolic atoms like +, -, *, / etc.
        if (atom.matches("[#&*+\\-./:<=>?@\\\\^~]+")) return false;
        return true;
    }

    private String escapeAtom(String atom) {
        return atom.replace("\\", "\\\\").replace("'", "\\'");
    }

    private String escapeCharacter(char ch) {
        switch (ch) {
            case '"': return "\\\"";
            case '\\': return "\\\\";
            case '\n': return "\\n";
            case '\t': return "\\t";
            case '\r': return "\\r";
            default: return String.valueOf(ch);
        }
    }

    private boolean isCharacterList(CompoundTerm list) {
        // Check if list contains only single-character atoms
        Term current = list;
        while (current instanceof CompoundTerm && ".".equals(TermUtils.getFunctorName(current))) {
            CompoundTerm cons = (CompoundTerm) current;
            Term head = TermUtils.getArgument(cons, 0);
            if (!(head instanceof Atom && ((Atom) head).getName().length() == 1)) {
                return false;
            }
            current = TermUtils.getArgument(cons, 1);
        }
        return current instanceof Atom && "[]".equals(((Atom) current).getName());
    }

    private boolean isOperator(String functor, int arity) {
        // Simplified operator check
        return false; // In full implementation, check operator table
    }

    private String formatOperator(CompoundTerm compound, WriteOptions options, Map<String, Term> bindings, int depth) {
        // Simplified operator formatting
        return formatCompoundTerm(compound, options, bindings, depth);
    }

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

    // START_CHANGE: LIM-009 - WriteOptions class for ISO write_term options
    /**
     * Mutable options container for write_term options.
     */
    private static class WriteOptions {
        boolean quoted = false;
        boolean numbervars = false;
        boolean ignoreOps = false;
        boolean writeStrings = false;
        int maxDepth = 0; // 0 = unlimited
    }
    // END_CHANGE: LIM-009
}
