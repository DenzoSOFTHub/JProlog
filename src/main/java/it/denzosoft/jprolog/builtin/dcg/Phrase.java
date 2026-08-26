package it.denzosoft.jprolog.builtin.dcg;

import it.denzosoft.jprolog.builtin.AbstractBuiltInWithContext;
import it.denzosoft.jprolog.core.engine.SolverContext;
import it.denzosoft.jprolog.core.terms.*;

import java.util.*;
import it.denzosoft.jprolog.util.TermUtils;

/**
 * Implementation of phrase/2 and phrase/3 predicates for DCG support.
 * 
 * phrase(RuleSet, List) - Parse List using RuleSet
 * phrase(RuleSet, List, Rest) - Parse List using RuleSet, leaving Rest
 */
public class Phrase extends AbstractBuiltInWithContext {
    
    /**
     * Create a phrase predicate.
     * 
     * @param solver The query solver for context
     */
    public Phrase(SolverContext solver) {
        super(solver);
    }
    
    @Override
    public boolean execute(Term term, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        boolean result = solve(solver, bindings);
        if (result) {
            // Add the successful binding to solutions
            solutions.add(new HashMap<>(bindings));
        }
        return result;
    }
    
    @Override
    public boolean executeWithContext(SolverContext solver, Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        this.solver = solver;

        // Extract arguments from query
        List<Term> qargs = (query instanceof CompoundTerm)
            ? ((CompoundTerm) query).getArguments()
            : java.util.Collections.<Term>emptyList();

        Term ruleSet, list, rest;
        if (qargs.size() == 2) {
            ruleSet = qargs.get(0); list = qargs.get(1); rest = new Atom("[]");
        } else if (qargs.size() == 3) {
            ruleSet = qargs.get(0); list = qargs.get(1); rest = qargs.get(2);
        } else {
            return false;
        }

        // START_CHANGE: ISS-2025-0253 - phrase/2,3 must be MULTI-solution. Previously it
        // committed to solutionList.get(0), so phrase behaved like once(phrase(...)) and could
        // not enumerate alternative parses / Rest splittings on backtracking. Mirror call/N:
        // solve the expanded DCG goal and propagate every solution to the caller.
        // START_CHANGE: ISS-2025-0394 - ISO 13211-3 error clauses: type_error(list, L) for a
        // non-list input/rest, type_error(callable, B) for a non-callable body (raised inside
        // createDCGGoal) — instead of silently failing / swallowing IllegalArgumentException.
        String indicator = qargs.size() == 2 ? "phrase/2" : "phrase/3";
        checkListArgument(list.resolveBindings(bindings), indicator);
        checkListArgument(rest.resolveBindings(bindings), indicator);
        Term goal = createDCGGoal(ruleSet.resolveBindings(bindings), list, rest, indicator);
        // END_CHANGE: ISS-2025-0394
        List<Map<String, Term>> goalSolutions = new ArrayList<>();
        boolean success = solver.solveMeta(goal, new HashMap<>(bindings), goalSolutions);   // ISS-2025-0485
        if (success) {
            solutions.addAll(goalSolutions);
            return true;
        }
        return false;
        // END_CHANGE: ISS-2025-0253
    }
    
    @Override
    public boolean solve(SolverContext solver, Map<String, Term> bindings) {
        Term[] args = getArguments();
        
        if (args.length == 2) {
            return phrase2(args[0], args[1], bindings);
        } else if (args.length == 3) {
            return phrase3(args[0], args[1], args[2], bindings);
        }
        
        return false;
    }
    
    /**
     * phrase/2 implementation: phrase(RuleSet, List)
     */
    private boolean phrase2(Term ruleSet, Term list, Map<String, Term> bindings) {
        // phrase(RuleSet, List) is equivalent to phrase(RuleSet, List, [])
        return phrase3(ruleSet, list, new Atom("[]"), bindings);
    }
    
    /**
     * phrase/3 implementation: phrase(RuleSet, List, Rest)
     */
    // START_CHANGE: ISS-2025-0190 - Fix destructive bindings modification and exception masking
    private boolean phrase3(Term ruleSet, Term list, Term rest, Map<String, Term> bindings) {
        try {
            // START_CHANGE: ISS-2025-0394 - same ISO error clauses on the legacy path
            checkListArgument(list.resolveBindings(bindings), "phrase/3");
            checkListArgument(rest.resolveBindings(bindings), "phrase/3");
            // END_CHANGE: ISS-2025-0394
            // Create a goal: RuleSet(List, Rest)
            Term goal = createDCGGoal(ruleSet, list, rest, "phrase/3");

            // Solve the DCG goal using a copy of bindings to avoid destructive modification
            Map<String, Term> solveBindings = new HashMap<>(bindings);
            List<Map<String, Term>> solutionList = new ArrayList<>();
            boolean success = this.solver.solveMeta(goal, solveBindings, solutionList);   // ISS-2025-0485

            // Return true if successful and propagate bindings correctly
            if (success && !solutionList.isEmpty()) {
                // Merge solution bindings into original map without losing pre-existing bindings
                bindings.putAll(solutionList.get(0));
                return true;
            }

            return false;

        } catch (it.denzosoft.jprolog.core.exceptions.PrologException e) {
            throw e; // Propagate Prolog exceptions (system errors, etc.)
        } catch (RuntimeException e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            throw e; // Propagate programming errors
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            return false;
        }
    }
    // END_CHANGE: ISS-2025-0190
    
    // START_CHANGE: ISS-2025-0391 - phrase/2,3 must apply the FULL grammar-body translation to
    // its first argument. The old code blindly appended (List, Rest) to ANY compound, producing
    // nonsense goals like ','(a, b, [a,b], []) — so (A,B), (A;B), (A->B), \+A, !, {G}, terminal
    // lists [a,b] / [] and strings all silently failed. Route the body through the same
    // DCGTranslator machinery used for --> rules (a plain atom/compound non-terminal degenerates
    // to the previous nt(List, Rest) shape).
    /** Sequence for fresh-variable prefixes so runtime translations never collide with caller variables. */
    private static final java.util.concurrent.atomic.AtomicLong PHRASE_VAR_SEQ =
        new java.util.concurrent.atomic.AtomicLong();

    /**
     * Create a DCG goal from the grammar body and arguments.
     *
     * @param ruleSet The DCG grammar body
     * @param list The input list
     * @param rest The rest list
     * @param indicator The predicate indicator for error contexts (phrase/2 or phrase/3)
     * @return The created goal term
     */
    private Term createDCGGoal(Term ruleSet, Term list, Term rest, String indicator) {
        // START_CHANGE: ISS-2025-0394 - a number or string is not a grammar body
        if (ruleSet instanceof it.denzosoft.jprolog.core.terms.Number || ruleSet instanceof PrologString) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("callable", ruleSet, indicator));
        }
        // END_CHANGE: ISS-2025-0394
        if (ruleSet instanceof Variable) {
            // Unbound body: route through call/3, which raises instantiation_error when still
            // unbound (must NOT go through translateBody -> phrase/3, which would loop).
            return TermUtils.createCompound("call", ruleSet, list, rest);
        }
        it.denzosoft.jprolog.core.dcg.v2.DCGTranslator translator =
            new it.denzosoft.jprolog.core.dcg.v2.DCGTranslator("_PhraseS" + PHRASE_VAR_SEQ.getAndIncrement() + "_");
        return translator.body(ruleSet, list, rest);
    }
    // END_CHANGE: ISS-2025-0391

    // START_CHANGE: ISS-2025-0394 - input/rest must be a variable, a proper list or a partial
    // list; otherwise type_error(list, Arg) per ISO 13211-3 (strings are tolerated for backward
    // compatibility with code-list inputs written as "...").
    private static void checkListArgument(Term t, String indicator) {
        Term cur = t;
        while (cur instanceof CompoundTerm && ".".equals(TermUtils.getFunctorName(cur)) && TermUtils.getArity(cur) == 2) {
            cur = TermUtils.getArgument((CompoundTerm) cur, 1);
        }
        if (cur instanceof Variable) return;                                    // variable / partial list
        if (cur instanceof Atom && "[]".equals(((Atom) cur).getName())) return; // proper list
        if (cur instanceof PrologString) return;                                // legacy string input
        throw new it.denzosoft.jprolog.core.exceptions.PrologException(
            it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("list", t, indicator));
    }
    // END_CHANGE: ISS-2025-0394
    
    /**
     * Validate that a term is a proper list.
     * 
     * @param term The term to validate
     * @return true if it's a proper list
     */
    private boolean isProperList(Term term) {
        Term current = term;
        
        while (current instanceof CompoundTerm && ".".equals(TermUtils.getFunctorName(current)) && TermUtils.getArity(current) == 2) {
            CompoundTerm cons = (CompoundTerm) current;
            current = TermUtils.getArgument(cons, 1);
        }
        
        return current instanceof Atom && "[]".equals(((Atom) current).getName());
    }
    
    /**
     * Convert a Prolog list to a Java list.
     * 
     * @param term The Prolog list term
     * @return Java list of terms
     */
    private List<Term> termToList(Term term) {
        List<Term> result = new ArrayList<>();
        Term current = term;
        
        while (current instanceof CompoundTerm && ".".equals(TermUtils.getFunctorName(current)) && TermUtils.getArity(current) == 2) {
            CompoundTerm cons = (CompoundTerm) current;
            result.add(TermUtils.getArgument(cons, 0));
            current = TermUtils.getArgument(cons, 1);
        }
        
        return result;
    }
    
    /**
     * Convert a Java list to a Prolog list term.
     * 
     * @param elements The list elements
     * @param tail The list tail (usually [])
     * @return The Prolog list term
     */
    private Term listToTerm(List<Term> elements, Term tail) {
        Term result = tail;
        
        for (int i = elements.size() - 1; i >= 0; i--) {
            result = TermUtils.createCompound(".", elements.get(i), result);
        }
        
        return result;
    }
}