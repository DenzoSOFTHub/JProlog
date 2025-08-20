package it.denzosoft.jprolog.core.dcg;

import it.denzosoft.jprolog.core.engine.Rule;
import it.denzosoft.jprolog.core.terms.*;
import it.denzosoft.jprolog.util.TermUtils;

import java.util.ArrayList;
import java.util.List;

/**
 * Transforms DCG rules into standard Prolog clauses.
 * Implements ISO Prolog DCG transformation rules.
 */
public class DCGTransformer {
    
    private int variableCounter = 0;
    
    /**
     * Transform a DCG rule into a standard Prolog rule.
     * 
     * @param dcgRule The DCG rule (Head --> Body)
     * @return The transformed Prolog rule
     */
    public Rule transformDCGRule(CompoundTerm dcgRule) {
        if (!"-->".equals(TermUtils.getFunctorName(dcgRule)) || TermUtils.getArity(dcgRule) != 2) {
            throw new IllegalArgumentException("Not a valid DCG rule: " + dcgRule);
        }
        
        Term head = TermUtils.getArgument(dcgRule, 0);
        Term body = TermUtils.getArgument(dcgRule, 1);
        
        // Reset variable counter for each rule
        variableCounter = 0;
        
        // Transform head: add difference list arguments
        Term transformedHead = transformHead(head);
        
        // Transform body: handle terminals, non-terminals, and control structures
        Term transformedBody = transformBody(body, getInputVariable(), getOutputVariable());
        
        // Convert comma-separated goals to a list for proper rule body handling
        List<Term> bodyGoals = new ArrayList<>();
        extractConjunctionGoals(transformedBody, bodyGoals);
        
        return new Rule(transformedHead, bodyGoals);
    }
    
    /**
     * Transform DCG head by adding difference list arguments.
     * 
     * @param head The original head
     * @return The transformed head with difference list arguments
     */
    private Term transformHead(Term head) {
        if (head instanceof Atom) {
            // atom --> atom(S0, S)
            String name = ((Atom) head).getName();
            return TermUtils.createCompound(name, getInputVariable(), getOutputVariable());
        } else if (head instanceof CompoundTerm) {
            // functor(Args) --> functor(Args, S0, S)
            CompoundTerm compound = (CompoundTerm) head;
            List<Term> newArgs = new ArrayList<>();
            
            // Add original arguments
            for (int i = 0; i < TermUtils.getArity(compound); i++) {
                newArgs.add(TermUtils.getArgument(compound, i));
            }
            
            // Add difference list arguments
            newArgs.add(getInputVariable());
            newArgs.add(getOutputVariable());
            
            return TermUtils.createCompound(TermUtils.getFunctorName(compound), newArgs.toArray(new Term[0]));
        }
        
        throw new IllegalArgumentException("Invalid DCG head: " + head);
    }
    
    /**
     * Transform DCG body recursively.
     * 
     * @param body The body term
     * @param input The input variable
     * @param output The output variable
     * @return The transformed body
     */
    private Term transformBody(Term body, Variable input, Variable output) {
        if (body instanceof Atom) {
            String atomName = ((Atom) body).getName();
            if ("[]".equals(atomName)) {
                // Empty body --> Input = Output
                return TermUtils.createCompound("=", input, output);
            } else if (isStringLiteral(atomName)) {
                // String literal "abc" --> convert to character codes list
                return transformStringLiteral(atomName, input, output);
            } else {
                // Non-terminal
                return TermUtils.createCompound(atomName, input, output);
            }
        } else if (body instanceof it.denzosoft.jprolog.core.terms.PrologString) {
            // Handle PrologString instances (quoted strings from parser)
            it.denzosoft.jprolog.core.terms.PrologString prologString = (it.denzosoft.jprolog.core.terms.PrologString) body;
            return transformPrologString(prologString, input, output);
        }
        
        if (body instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) body;
            String functor = TermUtils.getFunctorName(compound);
            
            switch (functor) {
                case ",": // Conjunction
                    return transformConjunction(compound, input, output);
                    
                case ";": // Disjunction
                    return transformDisjunction(compound, input, output);
                    
                case "[]": // Empty list (epsilon)
                    return TermUtils.createCompound("=", input, output);
                    
                case ".": // List (terminal symbols)
                    return transformTerminalList(compound, input, output);
                    
                case "{}": // Prolog goal
                    return transformPrologGoal(compound, input, output);
                    
                case "!": // Cut
                    return TermUtils.createCompound(",", 
                        new Atom("!"),
                        TermUtils.createCompound("=", input, output));
                    
                default:
                    // Non-terminal with arguments
                    return transformNonTerminal(compound, input, output);
            }
        }
        
        if (body instanceof Variable) {
            // Variable non-terminal
            return TermUtils.createCompound("call", body, input, output);
        }
        
        throw new IllegalArgumentException("Invalid DCG body: " + body);
    }
    
    /**
     * Transform conjunction: (A, B).
     */
    private Term transformConjunction(CompoundTerm conjunction, Variable input, Variable output) {
        Term left = TermUtils.getArgument(conjunction, 0);
        Term right = TermUtils.getArgument(conjunction, 1);
        
        Variable intermediate = getNewVariable();
        
        Term transformedLeft = transformBody(left, input, intermediate);
        Term transformedRight = transformBody(right, intermediate, output);
        
        // Flatten nested conjunctions to avoid deep nesting issues
        return flattenConjunction(transformedLeft, transformedRight);
    }
    
    /**
     * Flatten conjunctions to avoid nested comma operators that cause execution issues.
     */
    private Term flattenConjunction(Term left, Term right) {
        List<Term> goals = new ArrayList<>();
        
        // Extract goals from left side
        extractConjunctionGoals(left, goals);
        
        // Extract goals from right side  
        extractConjunctionGoals(right, goals);
        
        // Build right-associative conjunction
        Term result = goals.get(goals.size() - 1);
        for (int i = goals.size() - 2; i >= 0; i--) {
            result = TermUtils.createCompound(",", goals.get(i), result);
        }
        
        return result;
    }
    
    /**
     * Extract individual goals from a conjunction tree.
     */
    private void extractConjunctionGoals(Term term, List<Term> goals) {
        if (term instanceof CompoundTerm && ",".equals(TermUtils.getFunctorName(term)) && TermUtils.getArity(term) == 2) {
            // This is a conjunction, recurse on both sides
            extractConjunctionGoals(TermUtils.getArgument(term, 0), goals);
            extractConjunctionGoals(TermUtils.getArgument(term, 1), goals);
        } else {
            // This is an atomic goal
            goals.add(term);
        }
    }
    
    /**
     * Transform disjunction: (A ; B).
     */
    private Term transformDisjunction(CompoundTerm disjunction, Variable input, Variable output) {
        Term left = TermUtils.getArgument(disjunction, 0);
        Term right = TermUtils.getArgument(disjunction, 1);
        
        Term transformedLeft = transformBody(left, input, output);
        Term transformedRight = transformBody(right, input, output);
        
        return TermUtils.createCompound(";", transformedLeft, transformedRight);
    }
    
    /**
     * Transform terminal list: [a, b, c].
     */
    private Term transformTerminalList(CompoundTerm list, Variable input, Variable output) {
        List<Term> terminals = extractListElements(list);
        
        if (terminals.isEmpty()) {
            return TermUtils.createCompound("=", input, output);
        }
        
        // Build chain of =/2 goals for difference list consumption
        Variable current = input;
        Term result = null;
        
        for (int i = 0; i < terminals.size(); i++) {
            Variable next = (i == terminals.size() - 1) ? output : getNewVariable();
            Term terminal = terminals.get(i);
            
            // Current = [Terminal|Next]
            Term consCell = TermUtils.createCompound(".", terminal, next);
            Term unification = TermUtils.createCompound("=", current, consCell);
            
            if (result == null) {
                result = unification;
            } else {
                result = TermUtils.createCompound(",", result, unification);
            }
            
            current = next;
        }
        
        return result;
    }
    
    /**
     * Transform Prolog goal: {Goal}.
     */
    private Term transformPrologGoal(CompoundTerm goal, Variable input, Variable output) {
        if (TermUtils.getArity(goal) != 1) {
            throw new IllegalArgumentException("Invalid Prolog goal: " + goal);
        }
        
        Term prologGoal = TermUtils.getArgument(goal, 0);
        Term inputOutputUnification = TermUtils.createCompound("=", input, output);
        
        return TermUtils.createCompound(",", prologGoal, inputOutputUnification);
    }
    
    /**
     * Transform non-terminal with arguments.
     */
    private Term transformNonTerminal(CompoundTerm nonTerminal, Variable input, Variable output) {
        List<Term> newArgs = new ArrayList<>();
        
        // Add original arguments
        for (int i = 0; i < TermUtils.getArity(nonTerminal); i++) {
            newArgs.add(TermUtils.getArgument(nonTerminal, i));
        }
        
        // Add difference list arguments
        newArgs.add(input);
        newArgs.add(output);
        
        return TermUtils.createCompound(TermUtils.getFunctorName(nonTerminal), newArgs.toArray(new Term[0]));
    }
    
    /**
     * Extract elements from a Prolog list term.
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
     * Get the standard input variable S0.
     */
    private Variable getInputVariable() {
        return new Variable("S0");
    }
    
    /**
     * Get the standard output variable S.
     */
    private Variable getOutputVariable() {
        return new Variable("S");
    }
    
    /**
     * Generate a new unique variable.
     */
    private Variable getNewVariable() {
        return new Variable("S" + (++variableCounter));
    }
    
    /**
     * Check if an atom name represents a string literal.
     * String literals are atoms enclosed in double quotes.
     * 
     * @param atomName The atom name to check
     * @return true if it's a string literal
     */
    private boolean isStringLiteral(String atomName) {
        return atomName.length() >= 2 && 
               atomName.startsWith("\"") && 
               atomName.endsWith("\"");
    }
    
    /**
     * Transform a PrologString into difference list consumption.
     * PrologString("abc") becomes a sequence of unifications: Input = [97|S1], S1 = [98|S2], S2 = [99|Output]
     * 
     * @param prologString The PrologString term
     * @param input The input variable
     * @param output The output variable
     * @return The transformed term
     */
    private Term transformPrologString(it.denzosoft.jprolog.core.terms.PrologString prologString, Variable input, Variable output) {
        String content = prologString.getStringValue();
        
        if (content.isEmpty()) {
            // Empty string --> Input = Output
            return TermUtils.createCompound("=", input, output);
        }
        
        // Convert string to character codes
        List<Term> charCodes = new ArrayList<>();
        for (char c : content.toCharArray()) {
            charCodes.add(new it.denzosoft.jprolog.core.terms.Number((double) (int) c));
        }
        
        return buildCharCodeUnifications(charCodes, input, output);
    }
    
    /**
     * Transform a string literal into difference list consumption.
     * "abc" becomes a sequence of unifications: Input = [97|S1], S1 = [98|S2], S2 = [99|Output]
     * 
     * @param stringLiteral The string literal (including quotes)
     * @param input The input variable
     * @param output The output variable
     * @return The transformed term
     */
    private Term transformStringLiteral(String stringLiteral, Variable input, Variable output) {
        // Remove quotes
        String content = stringLiteral.substring(1, stringLiteral.length() - 1);
        
        if (content.isEmpty()) {
            // Empty string --> Input = Output
            return TermUtils.createCompound("=", input, output);
        }
        
        // Convert string to character codes
        List<Term> charCodes = new ArrayList<>();
        for (char c : content.toCharArray()) {
            charCodes.add(new it.denzosoft.jprolog.core.terms.Number((double) (int) c));
        }
        
        return buildCharCodeUnifications(charCodes, input, output);
    }
    
    /**
     * Build a chain of unifications for character code consumption in difference lists.
     * 
     * @param charCodes The list of character codes
     * @param input The input variable
     * @param output The output variable
     * @return The chain of unifications
     */
    private Term buildCharCodeUnifications(List<Term> charCodes, Variable input, Variable output) {
        // Build chain of unifications for difference list consumption
        Variable current = input;
        Term result = null;
        
        for (int i = 0; i < charCodes.size(); i++) {
            Variable next = (i == charCodes.size() - 1) ? output : getNewVariable();
            Term charCode = charCodes.get(i);
            
            // Current = [CharCode|Next]
            Term consCell = TermUtils.createCompound(".", charCode, next);
            Term unification = TermUtils.createCompound("=", current, consCell);
            
            if (result == null) {
                result = unification;
            } else {
                result = TermUtils.createCompound(",", result, unification);
            }
            
            current = next;
        }
        
        return result;
    }
    
    /**
     * Check if a term represents a DCG rule.
     * 
     * @param term The term to check
     * @return true if it's a DCG rule
     */
    public static boolean isDCGRule(Term term) {
        return term instanceof CompoundTerm && 
               "-->".equals(TermUtils.getFunctorName(term)) && 
               TermUtils.getArity(term) == 2;
    }
}