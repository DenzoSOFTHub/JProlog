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
        int[] argIdxRef = {0}; // boxed for helper
        // START_CHANGE: R4 - column tabbing state
        java.util.List<Integer> tabMarks = new java.util.ArrayList<>();
        int segmentStart = 0;
        int segmentBaseCol = 0;
        // END_CHANGE: R4

        for (int i = 0; i < formatString.length(); i++) {
            char ch = formatString.charAt(i);

            if (ch == '~') {
                // START_CHANGE: ISS-2025-0249 - parse optional numeric prefix or '*' for width/precision/radix
                int j = i + 1;
                Integer numArg = null;
                boolean starArg = false;
                if (j < formatString.length() && formatString.charAt(j) == '*') {
                    // Take count from next argument
                    starArg = true;
                    if (argIdxRef[0] < arguments.size()) {
                        Term a = arguments.get(argIdxRef[0]).resolveBindings(bindings);
                        if (a instanceof it.denzosoft.jprolog.core.terms.Number) {
                            numArg = (int) ((it.denzosoft.jprolog.core.terms.Number) a).longValue();
                        }
                        argIdxRef[0]++;
                    }
                    j++;
                } else {
                    StringBuilder num = new StringBuilder();
                    while (j < formatString.length() && Character.isDigit(formatString.charAt(j))) {
                        num.append(formatString.charAt(j));
                        j++;
                    }
                    if (num.length() > 0) {
                        try { numArg = Integer.parseInt(num.toString()); } catch (NumberFormatException e) { /* ignore */ }
                    }
                }
                if (j < formatString.length()) {
                    char formatChar = formatString.charAt(j);
                    // START_CHANGE: R4 - column tabbing specifiers (~t / ~N| / ~N+)
                    if (formatChar == 't') {
                        tabMarks.add(result.length());
                        i = j;
                        continue;
                    }
                    if (formatChar == '|' || formatChar == '+') {
                        int currentColInSegment = result.length() - segmentStart;
                        int targetColInSegment;
                        if (formatChar == '|') {
                            int absTarget = (numArg != null) ? numArg : (segmentBaseCol + currentColInSegment);
                            targetColInSegment = absTarget - segmentBaseCol;
                        } else {
                            int rel = (numArg != null) ? numArg : currentColInSegment;
                            targetColInSegment = rel;
                        }
                        int padding = targetColInSegment - currentColInSegment;
                        if (padding > 0 && !tabMarks.isEmpty()) {
                            int slots = tabMarks.size();
                            int perTab = padding / slots;
                            int remainder = padding - perTab * slots;
                            StringBuilder pad = new StringBuilder();
                            for (int k = 0; k < perTab; k++) pad.append(' ');
                            String padStr = pad.toString();
                            for (int t = tabMarks.size() - 1; t >= 0; t--) {
                                int pos = tabMarks.get(t);
                                if (t == tabMarks.size() - 1 && remainder > 0) {
                                    StringBuilder r = new StringBuilder(padStr);
                                    for (int k = 0; k < remainder; k++) r.append(' ');
                                    result.insert(pos, r);
                                } else {
                                    result.insert(pos, padStr);
                                }
                            }
                        } else if (padding > 0) {
                            for (int k = 0; k < padding; k++) result.append(' ');
                        }
                        tabMarks.clear();
                        segmentBaseCol = segmentBaseCol + Math.max(currentColInSegment, targetColInSegment);
                        segmentStart = result.length();
                        i = j;
                        continue;
                    }
                    // END_CHANGE: R4
                    String formatted = processFormatCode(formatChar, arguments, argIdxRef[0], numArg, bindings);
                    result.append(formatted);
                    if (consumesArgument(formatChar)) {
                        argIdxRef[0]++;
                    }
                    // START_CHANGE: R4 - reset column tracking on newline output
                    if (formatted.indexOf('\n') >= 0) {
                        segmentBaseCol = 0;
                        segmentStart = result.length();
                        tabMarks.clear();
                    }
                    // END_CHANGE: R4
                    i = j; // Skip the format character
                } else {
                    result.append(ch); // Lone ~ at end
                }
                // END_CHANGE: ISS-2025-0249
            } else {
                result.append(ch);
                // START_CHANGE: R4 - literal newline resets column
                if (ch == '\n') {
                    segmentBaseCol = 0;
                    segmentStart = result.length();
                    tabMarks.clear();
                }
                // END_CHANGE: R4
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
    private String processFormatCode(char formatChar, List<Term> arguments, int argIndex, Integer numArg, Map<String, Term> bindings) {
        Term arg = null;
        if (argIndex < arguments.size()) {
            arg = arguments.get(argIndex);
            if (arg instanceof Variable && bindings.containsKey(((Variable) arg).getName())) {
                arg = bindings.get(((Variable) arg).getName());
            }
        }

        switch (formatChar) {
            case 'a': // Atom
                return arg != null ? formatAtom(arg) : "";

            case 'd': // Decimal integer (with optional width N or N=decimal positions)
                if (arg == null) return "0";
                if (numArg != null && numArg > 0) {
                    // ~Nd: insert decimal point N digits from right
                    String s = formatInteger(arg);
                    boolean neg = s.startsWith("-");
                    if (neg) s = s.substring(1);
                    while (s.length() < numArg + 1) s = "0" + s;
                    s = s.substring(0, s.length() - numArg) + "." + s.substring(s.length() - numArg);
                    return neg ? "-" + s : s;
                }
                return formatInteger(arg);

            case 'D': // Decimal with comma grouping (SWI extension)
                if (arg == null) return "0";
                return formatIntegerGrouped(arg);

            case 'f': // Float (~Nf with N decimals)
                if (arg == null) return "0.0";
                return formatFloatPrec(arg, numArg);

            case 'e': // Exponential (~Ne)
                if (arg == null) return "0.0";
                return formatExponential(arg, numArg);

            case 'g': // General float
                if (arg == null) return "0.0";
                return formatGeneral(arg, numArg);

            case 's': // String/list of characters
                return arg != null ? formatString(arg) : "";

            case 'w': // Write term (with optional width)
                if (arg == null) return "";
                return padToWidth(formatTerm(arg), numArg);

            case 'q': // Quoted term
                if (arg == null) return "";
                return padToWidth(formatQuoted(arg), numArg);

            case 'n': // Newline
                return "\n";

            case 't': // Fill char placeholder (column tabbing). Without column tab, just tab char.
                return "\t";

            case '~': // Literal ~
                return "~";

            case 'i': // Ignore argument
                return "";

            case 'p': // Print - try portray/1 hook, fall back to write
                // START_CHANGE: R4 - portray hook
                if (arg == null) return "";
                return formatViaPortray(arg, bindings);
                // END_CHANGE: R4

            case 'c': // Character code (repeat N times if numArg given)
                if (arg == null) return "";
                int reps = (numArg != null && numArg > 0) ? numArg : 1;
                StringBuilder sb = new StringBuilder();
                String ch = formatCharacter(arg);
                for (int k = 0; k < reps; k++) sb.append(ch);
                return sb.toString();

            case 'r': // Radix N (e.g. ~2r → binary)
                if (arg == null || numArg == null) return arg != null ? formatRadix(arg) : "";
                return formatRadixBase(arg, numArg, false);
            case 'R': // Radix N uppercase
                if (arg == null || numArg == null) return arg != null ? formatRadix(arg) : "";
                return formatRadixBase(arg, numArg, true);

            default:
                return "~" + formatChar; // Unknown format code
        }
    }

    // START_CHANGE: R4 - portray hook: invoke user-defined portray/1 capturing its output
    private String formatViaPortray(Term arg, Map<String, Term> bindings) {
        if (solver == null || solver.getKnowledgeBase() == null) return formatTerm(arg);
        java.util.List<it.denzosoft.jprolog.core.engine.Rule> rules =
            solver.getKnowledgeBase().getRulesForPredicate("portray", 1);
        if (rules == null || rules.isEmpty()) return formatTerm(arg);

        // START_CHANGE: Round5 final - capture System.out output during portray execution
        java.io.PrintStream origOut = System.out;
        // Also redirect StreamManager's user_output to capture writes
        java.io.OutputStream prevUserOutput = StreamManager.getOutputStream("user_output");
        java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
        java.io.PrintStream wrapped = new java.io.PrintStream(baos);
        System.setOut(wrapped);
        if (prevUserOutput != null) {
            StreamManager.setOutputStreamRaw("user_output", baos);
        }
        try {
            Term portrayGoal = new it.denzosoft.jprolog.core.terms.CompoundTerm(
                new it.denzosoft.jprolog.core.terms.Atom("portray"),
                java.util.Arrays.asList(arg));
            java.util.List<Map<String, Term>> sols = new java.util.ArrayList<>();
            boolean ok = solver.solve(portrayGoal, new java.util.HashMap<>(bindings), sols,
                it.denzosoft.jprolog.core.engine.CutStatus.notOccurred());
            wrapped.flush();
            if (ok && baos.size() > 0) {
                return baos.toString();
            }
        } catch (Exception e) {
            // fall through to default
        } finally {
            System.setOut(origOut);
            if (prevUserOutput != null) {
                StreamManager.setOutputStreamRaw("user_output", prevUserOutput);
            }
        }
        return formatTerm(arg);
        // END_CHANGE: Round5 final
    }
    // END_CHANGE: R4

    // START_CHANGE: ISS-2025-0249 - format helpers for width/precision/radix
    private String padToWidth(String s, Integer width) {
        if (width == null || s.length() >= width) return s;
        StringBuilder sb = new StringBuilder();
        for (int k = 0; k < width - s.length(); k++) sb.append(' ');
        sb.append(s);
        return sb.toString();
    }
    private String formatFloatPrec(Term t, Integer prec) {
        double v = (t instanceof it.denzosoft.jprolog.core.terms.Number)
            ? ((it.denzosoft.jprolog.core.terms.Number) t).doubleValue()
            : 0.0;
        int p = (prec != null && prec >= 0) ? prec : 6;
        return String.format("%." + p + "f", v);
    }
    private String formatExponential(Term t, Integer prec) {
        double v = (t instanceof it.denzosoft.jprolog.core.terms.Number)
            ? ((it.denzosoft.jprolog.core.terms.Number) t).doubleValue()
            : 0.0;
        int p = (prec != null && prec >= 0) ? prec : 6;
        return String.format("%." + p + "e", v);
    }
    private String formatGeneral(Term t, Integer prec) {
        double v = (t instanceof it.denzosoft.jprolog.core.terms.Number)
            ? ((it.denzosoft.jprolog.core.terms.Number) t).doubleValue()
            : 0.0;
        int p = (prec != null && prec >= 0) ? prec : 6;
        return String.format("%." + p + "g", v);
    }
    private String formatIntegerGrouped(Term t) {
        if (!(t instanceof it.denzosoft.jprolog.core.terms.Number)) return "0";
        long v = ((it.denzosoft.jprolog.core.terms.Number) t).longValue();
        return String.format("%,d", v);
    }
    private String formatRadixBase(Term t, int base, boolean upper) {
        if (!(t instanceof it.denzosoft.jprolog.core.terms.Number)) return "";
        if (base < 2 || base > 36) return t.toString();
        long v = ((it.denzosoft.jprolog.core.terms.Number) t).longValue();
        String s = Long.toString(v, base);
        return upper ? s.toUpperCase() : s;
    }
    // END_CHANGE: ISS-2025-0249
    
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
                // START_CHANGE: ISS-2025-0193 - Handle supplementary Unicode codepoints
                int charCode = ((it.denzosoft.jprolog.core.terms.Number) head).getValue().intValue();
                sb.append(Character.toChars(charCode));
                // END_CHANGE: ISS-2025-0193
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
        // START_CHANGE: ISS-2025-0242 - operator-aware
        return it.denzosoft.jprolog.core.util.TermFormatter.format(term, false, false, false, 1200);
        // END_CHANGE: ISS-2025-0242
    }

    /**
     * Format a term with quotes if necessary.
     */
    private String formatQuoted(Term term) {
        // START_CHANGE: ISS-2025-0242 - operator-aware quoted
        return it.denzosoft.jprolog.core.util.TermFormatter.format(term, true, false, false, 1200);
        // END_CHANGE: ISS-2025-0242
    }
    
    /**
     * Format a term as character.
     */
    private String formatCharacter(Term term) {
        if (term instanceof it.denzosoft.jprolog.core.terms.Number) {
            // START_CHANGE: ISS-2025-0251 - codepoint-aware (Character.toChars handles supplementary plane)
            int charCode = (int) ((it.denzosoft.jprolog.core.terms.Number) term).longValue();
            if (charCode < 0 || charCode > 0x10FFFF) return "";
            return new String(Character.toChars(charCode));
            // END_CHANGE: ISS-2025-0251
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
        // ISS-2025-0327: honour the thread-local / current output stream instead of raw System.out.
        return new PrintWriter(StreamManager.out());
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