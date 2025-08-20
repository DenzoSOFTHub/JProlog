package it.denzosoft.jprolog.builtin.database;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.engine.Rule;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * clause/2 - ISO Prolog predicate for clause inspection
 * clause(?Head, ?Body)
 * 
 * Succeeds if there is a clause in the database with the given Head and Body.
 * For facts (clauses with no body), Body is unified with 'true'.
 */
public class Clause implements BuiltInWithContext {

    private final QuerySolver querySolver;

    public Clause(QuerySolver querySolver) {
        this.querySolver = querySolver;
    }

    @Override
    public boolean executeWithContext(QuerySolver solver, Term query,
                                    Map<String, Term> bindings,
                                    List<Map<String, Term>> solutions) {

        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("clause/2 requires exactly 2 arguments");
        }

        Term headPattern = query.getArguments().get(0);
        Term bodyPattern = query.getArguments().get(1);

        boolean foundSolution = false;

        // Get all rules from the knowledge base
        List<Rule> rules = solver.getKnowledgeBase().getRules();

        for (Rule rule : rules) {
            Term ruleHead = rule.getHead();
            List<Term> ruleBody = rule.getBody();

            // Create body term - 'true' for facts, compound term for rules with body
            Term bodyTerm;
            if (ruleBody.isEmpty()) {
                // Fact - body is 'true'
                bodyTerm = new Atom("true");
            } else if (ruleBody.size() == 1) {
                // Single goal body
                bodyTerm = ruleBody.get(0);
            } else {
                // Multiple goal body - create conjunction using ','
                bodyTerm = createConjunction(ruleBody);
            }

            // Try to unify head and body patterns with this rule
            Map<String, Term> newBindings = new HashMap<>(bindings);
            
            // Make fresh copies of the rule head and body to avoid variable conflicts
            Term freshHead = ruleHead.copy();
            Term freshBody = bodyTerm.copy();
            
            if (headPattern.unify(freshHead, newBindings) && 
                bodyPattern.unify(freshBody, newBindings)) {
                solutions.add(newBindings);
                foundSolution = true;
            }
        }

        return foundSolution;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("clause/2 requires context");
    }

    /**
     * Create a conjunction term from a list of terms.
     * For multiple goals, creates nested ',' compounds: ','(Goal1, ','(Goal2, Goal3))
     */
    private Term createConjunction(List<Term> goals) {
        if (goals.isEmpty()) {
            return new Atom("true");
        }
        
        if (goals.size() == 1) {
            return goals.get(0);
        }
        
        // Build right-associative conjunction
        Term result = goals.get(goals.size() - 1);
        for (int i = goals.size() - 2; i >= 0; i--) {
            result = new CompoundTerm(new Atom(","), Arrays.asList(goals.get(i), result));
        }
        
        return result;
    }
}