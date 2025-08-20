package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.builtin.AbstractBuiltInWithContext;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.terms.*;

import java.io.PrintWriter;
import it.denzosoft.jprolog.util.TermUtils;
import java.util.*;

/**
 * Implementation of format/2 and format/3 predicates.
 * 
 * format(+Format, +Arguments)
 * format(+Stream, +Format, +Arguments)
 */
public class Format extends AbstractBuiltInWithContext {
    
    /**
     * Create format predicate.
     * 
     * @param solver The query solver
     */
    public Format(QuerySolver solver) {
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
            // format(+Format, +Arguments)
            return format2(args[0], args[1], bindings);
        } else if (args.length == 3) {
            // format(+Stream, +Format, +Arguments)
            return format3(args[0], args[1], args[2], bindings);
        }
        
        return false;
    }
    
    /**
     * format/2 implementation.
     */
    private boolean format2(Term formatTerm, Term argumentsTerm, Map<String, Term> bindings) {
        return format3(new Atom("current_output"), formatTerm, argumentsTerm, bindings);
    }
    
    /**
     * format/3 implementation.
     */
    private boolean format3(Term streamTerm, Term formatTerm, Term argumentsTerm, Map<String, Term> bindings) {
        try {
            // Get output writer
            PrintWriter writer = getOutputStream(streamTerm);
            
            // Get format string
            String formatString = getFormatString(formatTerm);
            if (formatString == null) {
                return false;
            }
            
            // Get arguments
            List<Term> arguments = getArgumentList(argumentsTerm);
            
            // Process format string
            String output = processFormat(formatString, arguments, bindings);
            
            // Write output
            writer.print(output);
            writer.flush();
            
            return true;
            
        } catch (Exception e) {
            return false;
        }
    }
    
    /**
     * Process format string with arguments.
     * 
     * @param formatString The format string
     * @param arguments The format arguments
     * @param bindings Variable bindings
     * @return The formatted output
     */
    private String processFormat(String formatString, List<Term> arguments, Map<String, Term> bindings) {
        StringBuilder result = new StringBuilder();
        int argIndex = 0;
        
        for (int i = 0; i < formatString.length(); i++) {
            char ch = formatString.charAt(i);
            
            if (ch == '~') {
                if (i + 1 < formatString.length()) {
                    char formatChar = formatString.charAt(i + 1);
                    String formatted = processFormatCode(formatChar, arguments, argIndex, bindings);
                    result.append(formatted);
                    
                    // Advance argument index for most format codes
                    if (consumesArgument(formatChar)) {
                        argIndex++;
                    }
                    
                    i++; // Skip the format character
                } else {
                    result.append(ch); // Lone ~ at end
                }
            } else {
                result.append(ch);
            }
        }
        
        return result.toString();
    }
    
    /**
     * Process a single format code.
     * 
     * @param formatChar The format character
     * @param arguments The arguments list
     * @param argIndex Current argument index
     * @param bindings Variable bindings
     * @return The formatted string
     */
    private String processFormatCode(char formatChar, List<Term> arguments, int argIndex, Map<String, Term> bindings) {
        Term arg = null;
        if (argIndex < arguments.size()) {
            arg = arguments.get(argIndex);
            // Resolve variable if needed
            if (arg instanceof Variable && bindings.containsKey(((Variable) arg).getName())) {
                arg = bindings.get(((Variable) arg).getName());
            }
        }
        
        switch (formatChar) {
            case 'a': // Atom
                return arg != null ? formatAtom(arg) : "";
                
            case 'd': // Decimal integer
                return arg != null ? formatInteger(arg) : "0";
                
            case 'f': // Float
                return arg != null ? formatFloat(arg) : "0.0";
                
            case 's': // String/list of characters
                return arg != null ? formatString(arg) : "";
                
            case 'w': // Write term
                return arg != null ? formatTerm(arg) : "";
                
            case 'q': // Quoted term
                return arg != null ? formatQuoted(arg) : "";
                
            case 'n': // Newline
                return "\n";
                
            case 't': // Tab
                return "\t";
                
            case '~': // Literal ~
                return "~";
                
            case 'i': // Ignore argument
                return "";
                
            case 'p': // Print (same as write)
                return arg != null ? formatTerm(arg) : "";
                
            case 'c': // Character code
                return arg != null ? formatCharacter(arg) : "";
                
            case 'r': // Radix (base conversion)
                return arg != null ? formatRadix(arg) : "";
                
            default:
                return "~" + formatChar; // Unknown format code
        }
    }
    
    /**
     * Format a term as atom.
     */
    private String formatAtom(Term term) {
        if (term instanceof Atom) {
            return ((Atom) term).getName();
        } else {
            return term.toString();
        }
    }
    
    /**
     * Format a term as integer.
     */
    private String formatInteger(Term term) {
        if (term instanceof it.denzosoft.jprolog.core.terms.Number) {
            return String.valueOf(((it.denzosoft.jprolog.core.terms.Number) term).getValue().longValue());
        } else {
            return "0";
        }
    }
    
    /**
     * Format a term as float.
     */
    private String formatFloat(Term term) {
        if (term instanceof it.denzosoft.jprolog.core.terms.Number) {
            return String.valueOf(((it.denzosoft.jprolog.core.terms.Number) term).getValue());
        } else {
            return "0.0";
        }
    }
    
    /**
     * Format a term as string.
     */
    private String formatString(Term term) {
        if (term instanceof Atom) {
            return ((Atom) term).getName();
        } else if (term instanceof CompoundTerm && ".".equals(TermUtils.getFunctorName(term))) {
            // Character list
            return formatCharacterList((CompoundTerm) term);
        } else {
            return term.toString();
        }
    }
    
    /**
     * Format character list as string.
     */
    private String formatCharacterList(CompoundTerm list) {
        StringBuilder sb = new StringBuilder();
        Term current = list;
        
        while (current instanceof CompoundTerm && ".".equals(TermUtils.getFunctorName(current)) && TermUtils.getArity(current) == 2) {
            CompoundTerm cons = (CompoundTerm) current;
            Term head = TermUtils.getArgument(cons, 0);
            
            if (head instanceof it.denzosoft.jprolog.core.terms.Number) {
                int charCode = ((it.denzosoft.jprolog.core.terms.Number) head).getValue().intValue();
                sb.append((char) charCode);
            } else if (head instanceof Atom && ((Atom) head).getName().length() == 1) {
                sb.append(((Atom) head).getName());
            } else {
                break; // Not a character list
            }
            
            current = TermUtils.getArgument(cons, 1);
        }
        
        return sb.toString();
    }
    
    /**
     * Format a term in its natural representation.
     */
    private String formatTerm(Term term) {
        return term.toString();
    }
    
    /**
     * Format a term with quotes if necessary.
     */
    private String formatQuoted(Term term) {
        if (term instanceof Atom) {
            String name = ((Atom) term).getName();
            if (needsQuoting(name)) {
                return "'" + escapeAtom(name) + "'";
            } else {
                return name;
            }
        } else {
            return term.toString();
        }
    }
    
    /**
     * Format a term as character.
     */
    private String formatCharacter(Term term) {
        if (term instanceof it.denzosoft.jprolog.core.terms.Number) {
            int charCode = ((it.denzosoft.jprolog.core.terms.Number) term).getValue().intValue();
            return String.valueOf((char) charCode);
        } else {
            return "";
        }
    }
    
    /**
     * Format a number in specified radix.
     */
    private String formatRadix(Term term) {
        if (term instanceof it.denzosoft.jprolog.core.terms.Number) {
            int value = ((it.denzosoft.jprolog.core.terms.Number) term).getValue().intValue();
            return Integer.toString(value, 16); // Default to hex
        } else {
            return "";
        }
    }
    
    /**
     * Check if format code consumes an argument.
     */
    private boolean consumesArgument(char formatChar) {
        switch (formatChar) {
            case 'n':
            case 't':
            case '~':
                return false;
            default:
                return true;
        }
    }
    
    /**
     * Get format string from term.
     */
    private String getFormatString(Term formatTerm) {
        if (formatTerm instanceof Atom) {
            return ((Atom) formatTerm).getName();
        } else if (formatTerm instanceof CompoundTerm && ".".equals(TermUtils.getFunctorName(formatTerm))) {
            // Character list
            return formatCharacterList((CompoundTerm) formatTerm);
        } else {
            return null;
        }
    }
    
    /**
     * Get argument list from term.
     */
    private List<Term> getArgumentList(Term argumentsTerm) {
        if (argumentsTerm instanceof CompoundTerm && ".".equals(TermUtils.getFunctorName(argumentsTerm))) {
            return extractListElements(argumentsTerm);
        } else {
            return Collections.singletonList(argumentsTerm);
        }
    }
    
    /**
     * Get output stream.
     */
    private PrintWriter getOutputStream(Term streamTerm) {
        // Simplified - in full implementation, use StreamManager
        return new PrintWriter(System.out);
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
     * Check if atom needs quoting.
     */
    private boolean needsQuoting(String atom) {
        return !atom.matches("[a-z][a-zA-Z0-9_]*");
    }
    
    /**
     * Escape atom for quoting.
     */
    private String escapeAtom(String atom) {
        return atom.replace("'", "\\'");
    }
}