package it.denzosoft.jprolog.builtin.dcg;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.*;

import java.util.*;

/**
 * Utility predicates for enhanced DCG support per ISO/IEC DTS 13211-3
 */
public class DCGUtils {
    
    /**
     * call_dcg/3 - Call DCG rule with non-list terms
     * call_dcg(DCGBody, InputTerm, OutputTerm)
     */
    public static class CallDCG implements BuiltIn {
        @Override
        public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
            if (query.getArguments().size() != 3) {
                throw new PrologEvaluationException("call_dcg/3 requires exactly 3 arguments");
            }
            
            Term dcgBody = query.getArguments().get(0).resolveBindings(bindings);
            Term inputTerm = query.getArguments().get(1).resolveBindings(bindings);
            Term outputTerm = query.getArguments().get(2).resolveBindings(bindings);
            
            // This would transform the DCG body to work with arbitrary terms
            // For now, we'll provide a basic implementation
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (inputTerm.unify(outputTerm, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            
            return false;
        }
    }
    
    /**
     * dcg_translate_rule/2 - Translate DCG rule to standard Prolog
     * dcg_translate_rule(+DCGRule, -PrologRule)
     */
    public static class DCGTranslateRule implements BuiltIn {
        @Override
        public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
            if (query.getArguments().size() != 2) {
                throw new PrologEvaluationException("dcg_translate_rule/2 requires exactly 2 arguments");
            }
            
            Term dcgRule = query.getArguments().get(0).resolveBindings(bindings);
            Term prologRule = query.getArguments().get(1).resolveBindings(bindings);
            
            try {
                Term translatedRule = translateDCGRule(dcgRule);
                
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (prologRule.unify(translatedRule, newBindings)) {
                    solutions.add(newBindings);
                    return true;
                }
            } catch (Exception e) {
                throw new PrologEvaluationException("DCG translation error: " + e.getMessage());
            }
            
            return false;
        }
        
        private Term translateDCGRule(Term dcgRule) {
            if (!(dcgRule instanceof CompoundTerm)) {
                throw new IllegalArgumentException("DCG rule must be a compound term");
            }
            
            CompoundTerm compound = (CompoundTerm) dcgRule;
            if (!"-->".equals(compound.getName()) || compound.getArguments().size() != 2) {
                throw new IllegalArgumentException("DCG rule must use --> operator");
            }
            
            Term head = compound.getArguments().get(0);
            Term body = compound.getArguments().get(1);
            
            // Transform head by adding difference list arguments
            Term transformedHead = transformDCGHead(head);
            
            // Transform body
            Variable inputVar = new Variable("S0");
            Variable outputVar = new Variable("S");
            Term transformedBody = transformDCGBody(body, inputVar, outputVar);
            
            // Create the translated rule: transformedHead :- transformedBody
            return new CompoundTerm(
                new Atom(":-"),
                Arrays.asList(transformedHead, transformedBody)
            );
        }
        
        private Term transformDCGHead(Term head) {
            if (head instanceof Atom) {
                // Simple non-terminal: nt --> nt(S0, S)
                String functor = ((Atom) head).getName();
                return new CompoundTerm(
                    new Atom(functor),
                    Arrays.asList(new Variable("S0"), new Variable("S"))
                );
            }
            
            if (head instanceof CompoundTerm) {
                // Complex non-terminal: nt(Args) --> nt(Args, S0, S)
                CompoundTerm compound = (CompoundTerm) head;
                List<Term> newArgs = new ArrayList<>(compound.getArguments());
                newArgs.add(new Variable("S0"));
                newArgs.add(new Variable("S"));
                
                return new CompoundTerm(new Atom(compound.getName()), newArgs);
            }
            
            throw new IllegalArgumentException("Invalid DCG head: " + head);
        }
        
        private Term transformDCGBody(Term body, Variable input, Variable output) {
            if (body instanceof Atom) {
                if ("[]".equals(((Atom) body).getName())) {
                    // Empty production: [] --> S0 = S
                    return new CompoundTerm(new Atom("="), Arrays.asList(input, output));
                } else {
                    // Non-terminal: nt --> nt(S0, S)
                    String functor = ((Atom) body).getName();
                    return new CompoundTerm(
                        new Atom(functor),
                        Arrays.asList(input, output)
                    );
                }
            }
            
            if (body instanceof CompoundTerm) {
                CompoundTerm compound = (CompoundTerm) body;
                String functor = compound.getName();
                
                switch (functor) {
                    case ",":
                        // Conjunction: A, B
                        if (compound.getArguments().size() == 2) {
                            Variable intermediate = new Variable("S" + System.currentTimeMillis());
                            Term transformedA = transformDCGBody(compound.getArguments().get(0), input, intermediate);
                            Term transformedB = transformDCGBody(compound.getArguments().get(1), intermediate, output);
                            return new CompoundTerm(new Atom(","), Arrays.asList(transformedA, transformedB));
                        }
                        break;
                        
                    case ";":
                        // Disjunction: A ; B
                        if (compound.getArguments().size() == 2) {
                            Term transformedA = transformDCGBody(compound.getArguments().get(0), input, output);
                            Term transformedB = transformDCGBody(compound.getArguments().get(1), input, output);
                            return new CompoundTerm(new Atom(";"), Arrays.asList(transformedA, transformedB));
                        }
                        break;
                        
                    case "{}":
                        // Prolog goal: {Goal}
                        if (compound.getArguments().size() == 1) {
                            Term goal = compound.getArguments().get(0);
                            Term unify = new CompoundTerm(new Atom("="), Arrays.asList(input, output));
                            return new CompoundTerm(new Atom(","), Arrays.asList(goal, unify));
                        }
                        break;
                        
                    case ".":
                        // Terminal list: [a, b, c]
                        return transformTerminalList(body, input, output);
                        
                    default:
                        // Non-terminal with arguments
                        List<Term> newArgs = new ArrayList<>(compound.getArguments());
                        newArgs.add(input);
                        newArgs.add(output);
                        return new CompoundTerm(new Atom(functor), newArgs);
                }
            }
            
            // Handle variables and other terms
            if (body instanceof Variable) {
                // Meta-call: call(Body, S0, S)
                return new CompoundTerm(new Atom("call"), Arrays.asList(body, input, output));
            }
            
            throw new IllegalArgumentException("Invalid DCG body: " + body);
        }
        
        private Term transformTerminalList(Term listTerm, Variable input, Variable output) {
            // Transform [a, b, c] to S0 = [a, b, c | S]
            List<Term> elements = extractListElements(listTerm);
            Term expectedInput = buildListWithTail(elements, output);
            return new CompoundTerm(new Atom("="), Arrays.asList(input, expectedInput));
        }
        
        private List<Term> extractListElements(Term listTerm) {
            List<Term> elements = new ArrayList<>();
            Term current = listTerm;
            
            while (current instanceof CompoundTerm) {
                CompoundTerm compound = (CompoundTerm) current;
                if (!".".equals(compound.getName()) || compound.getArguments().size() != 2) {
                    break;
                }
                
                elements.add(compound.getArguments().get(0));
                current = compound.getArguments().get(1);
            }
            
            return elements;
        }
        
        private Term buildListWithTail(List<Term> elements, Term tail) {
            Term result = tail;
            
            for (int i = elements.size() - 1; i >= 0; i--) {
                result = new CompoundTerm(
                    new Atom("."),
                    Arrays.asList(elements.get(i), result)
                );
            }
            
            return result;
        }
    }
    
    /**
     * dcg_body//2 - Create DCG body with specific input/output
     * This is a higher-order DCG predicate
     */
    public static class DCGBody implements BuiltIn {
        @Override
        public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
            if (query.getArguments().size() != 4) { // DCG predicates get extra args
                throw new PrologEvaluationException("dcg_body//2 requires 2 DCG arguments plus difference list");
            }
            
            // This is a meta-DCG predicate that would need special handling
            // For now, we'll provide a basic implementation
            solutions.add(new HashMap<>(bindings));
            return true;
        }
    }
}