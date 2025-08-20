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
     * 
     * @param streamTerm The stream term
     * @param term The term to write
     * @param options Write options
     * @param bindings Variable bindings
     * @return true if successful
     */
    private boolean writeTermToStream(Term streamTerm, Term term, List<WriteOption> options, Map<String, Term> bindings) {
        try {
            // Get output writer (simplified - in full implementation, use StreamManager)
            PrintWriter writer = getCurrentOutputStream();
            
            // Format and write the term
            String output = formatTerm(term, options, bindings);
            writer.print(output);
            writer.flush();
            
            return true;
            
        } catch (Exception e) {
            return false;
        }
    }
    
    /**
     * Write term with options to current output.
     * 
     * @param term The term to write
     * @param optionsTerm The options list
     * @param bindings Variable bindings
     * @return true if successful
     */
    private boolean writeTermWithOptions(Term term, Term optionsTerm, Map<String, Term> bindings) {
        try {
            // Parse options
            List<WriteOption> options = parseWriteOptions(optionsTerm);
            
            // Write to current output stream
            return writeTermToStream(new Atom("current_output"), term, options, bindings);
            
        } catch (Exception e) {
            return false;
        }
    }
    
    /**
     * Format a term according to write options.
     * 
     * @param term The term to format
     * @param options Write options
     * @param bindings Variable bindings
     * @return Formatted string
     */
    private String formatTerm(Term term, List<WriteOption> options, Map<String, Term> bindings) {
        WriteContext context = new WriteContext(options);
        return formatTermRecursive(term, context, bindings);
    }
    
    /**
     * Recursively format a term.
     * 
     * @param term The term to format
     * @param context Write context
     * @param bindings Variable bindings
     * @return Formatted string
     */
    private String formatTermRecursive(Term term, WriteContext context, Map<String, Term> bindings) {
        if (term instanceof Atom) {
            return formatAtom((Atom) term, context);
        } else if (term instanceof it.denzosoft.jprolog.core.terms.Number) {
            return formatNumber((it.denzosoft.jprolog.core.terms.Number) term, context);
        } else if (term instanceof Variable) {
            return formatVariable((Variable) term, context, bindings);
        } else if (term instanceof CompoundTerm) {
            return formatCompoundTerm((CompoundTerm) term, context, bindings);
        } else {
            return term.toString();
        }
    }
    
    /**
     * Format an atom.
     */
    private String formatAtom(Atom atom, WriteContext context) {
        String name = atom.getName();
        
        if (context.isQuoted() && needsQuoting(name)) {
            return "'" + escapeAtom(name) + "'";
        } else {
            return name;
        }
    }
    
    /**
     * Format a number.
     */
    private String formatNumber(it.denzosoft.jprolog.core.terms.Number number, WriteContext context) {
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
    private String formatVariable(Variable variable, WriteContext context, Map<String, Term> bindings) {
        String name = variable.getName();
        
        // Check if variable is bound
        if (bindings.containsKey(name)) {
            Term value = bindings.get(name);
            return formatTermRecursive(value, context, bindings);
        }
        
        if (context.isNumbervars()) {
            // Use numbervars format: $VAR(N)
            return "$VAR(" + context.getVariableNumber(name) + ")";
        } else {
            return "_" + name;
        }
    }
    
    /**
     * Format a compound term.
     */
    private String formatCompoundTerm(CompoundTerm compound, WriteContext context, Map<String, Term> bindings) {
        String functor = TermUtils.getFunctorName(compound);
        int arity = TermUtils.getArity(compound);
        
        // Special cases
        if (arity == 2 && ".".equals(functor)) {
            // List notation
            if (context.isWriteStrings() && isCharacterList(compound)) {
                return formatCharacterList(compound, context, bindings);
            } else {
                return formatList(compound, context, bindings);
            }
        }
        
        // Check for operator notation
        if (context.isOps() && isOperator(functor, arity)) {
            return formatOperator(compound, context, bindings);
        }
        
        // Standard functor(args) notation
        StringBuilder sb = new StringBuilder();
        sb.append(formatAtom(new Atom(functor), context));
        
        if (arity > 0) {
            sb.append("(");
            for (int i = 0; i < arity; i++) {
                if (i > 0) sb.append(", ");
                sb.append(formatTermRecursive(TermUtils.getArgument(compound, i), context, bindings));
            }
            sb.append(")");
        }
        
        return sb.toString();
    }
    
    /**
     * Format a list in [a,b,c] notation.
     */
    private String formatList(CompoundTerm list, WriteContext context, Map<String, Term> bindings) {
        StringBuilder sb = new StringBuilder();
        sb.append("[");
        
        Term current = list;
        boolean first = true;
        
        while (current instanceof CompoundTerm && ".".equals(TermUtils.getFunctorName(current)) && TermUtils.getArity(current) == 2) {
            if (!first) sb.append(", ");
            first = false;
            
            CompoundTerm cons = (CompoundTerm) current;
            sb.append(formatTermRecursive(TermUtils.getArgument(cons, 0), context, bindings));
            current = TermUtils.getArgument(cons, 1);
        }
        
        if (!(current instanceof Atom && "[]".equals(((Atom) current).getName()))) {
            // Non-empty tail
            sb.append("|");
            sb.append(formatTermRecursive(current, context, bindings));
        }
        
        sb.append("]");
        return sb.toString();
    }
    
    /**
     * Format character list as string.
     */
    private String formatCharacterList(CompoundTerm list, WriteContext context, Map<String, Term> bindings) {
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
                    // Not a character
                    return formatList(list, context, bindings);
                }
            } else {
                // Not a character
                return formatList(list, context, bindings);
            }
            
            current = TermUtils.getArgument(cons, 1);
        }
        
        sb.append("\"");
        return sb.toString();
    }
    
    /**
     * Parse write options from term.
     */
    private List<WriteOption> parseWriteOptions(Term optionsTerm) {
        List<WriteOption> options = new ArrayList<>();
        
        if (optionsTerm instanceof CompoundTerm && ".".equals(TermUtils.getFunctorName(optionsTerm))) {
            List<Term> optionTerms = extractListElements(optionsTerm);
            
            for (Term optionTerm : optionTerms) {
                WriteOption option = parseWriteOption(optionTerm);
                if (option != null) {
                    options.add(option);
                }
            }
        }
        
        return options;
    }
    
    /**
     * Parse a single write option.
     */
    private WriteOption parseWriteOption(Term optionTerm) {
        if (optionTerm instanceof Atom) {
            String optionName = ((Atom) optionTerm).getName();
            return WriteOption.fromName(optionName);
        }
        
        return null;
    }
    
    /**
     * Get default write options.
     */
    private List<WriteOption> getDefaultWriteOptions() {
        return Arrays.asList(
            WriteOption.QUOTED,
            WriteOption.OPS,
            WriteOption.WRITE_STRINGS
        );
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
        // Simplified - check if atom needs quoting
        return !atom.matches("[a-z][a-zA-Z0-9_]*");
    }
    
    private String escapeAtom(String atom) {
        return atom.replace("'", "\\'");
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
    
    private String formatOperator(CompoundTerm compound, WriteContext context, Map<String, Term> bindings) {
        // Simplified operator formatting
        return formatCompoundTerm(compound, context, bindings);
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
    
    /**
     * Write options for term formatting.
     */
    private enum WriteOption {
        QUOTED,
        OPS,
        WRITE_STRINGS,
        NUMBERVARS;
        
        static WriteOption fromName(String name) {
            switch (name) {
                case "quoted": return QUOTED;
                case "ops": return OPS;
                case "write_strings": return WRITE_STRINGS;
                case "numbervars": return NUMBERVARS;
                default: return null;
            }
        }
    }
    
    /**
     * Context for writing terms.
     */
    private static class WriteContext {
        private final Set<WriteOption> options;
        private final Map<String, Integer> variableNumbers = new HashMap<>();
        private int nextVariableNumber = 0;
        
        WriteContext(List<WriteOption> options) {
            this.options = new HashSet<>(options);
        }
        
        boolean isQuoted() { return options.contains(WriteOption.QUOTED); }
        boolean isOps() { return options.contains(WriteOption.OPS); }
        boolean isWriteStrings() { return options.contains(WriteOption.WRITE_STRINGS); }
        boolean isNumbervars() { return options.contains(WriteOption.NUMBERVARS); }
        
        int getVariableNumber(String varName) {
            return variableNumbers.computeIfAbsent(varName, k -> nextVariableNumber++);
        }
    }
}