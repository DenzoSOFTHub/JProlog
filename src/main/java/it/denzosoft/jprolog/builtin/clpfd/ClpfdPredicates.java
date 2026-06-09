// START_CHANGE: ISS-2025-0123 - CLP(FD) Constraint Logic Programming over Finite Domains
package it.denzosoft.jprolog.builtin.clpfd;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.terms.*;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.builtin.clpfd.ConstraintStore.*;

import java.util.*;

/**
 * CLP(FD) built-in predicates for constraint logic programming over finite domains.
 * <p>
 * Supports: in/2, #=/2, #\=/2, #&lt;/2, #&gt;/2, #=&lt;/2, #&gt;=/2,
 * all_different/1, label/1, labeling/2, indomain/1, fd_dom/2, fd_size/2.
 */
public class ClpfdPredicates implements BuiltInWithContext {

    /** Operation type for each CLP(FD) predicate. */
    public enum OperationType {
        IN,             // X in 1..10 or X in [1,3,5]
        HASH_EQ,        // X #= Y + 3
        HASH_NEQ,       // X #\= Y
        HASH_LT,        // X #< Y
        HASH_GT,        // X #> Y
        HASH_LEQ,       // X #=< Y
        HASH_GEQ,       // X #>= Y
        ALL_DIFFERENT,  // all_different([X,Y,Z])
        LABEL,          // label([X,Y,Z])
        LABELING,       // labeling([ff], [X,Y,Z])
        INDOMAIN,       // indomain(X)
        FD_DOM,         // fd_dom(X, Dom)
        FD_SIZE         // fd_size(X, Size)
    }

    private final QuerySolver solver;
    private final OperationType opType;

    // START_CHANGE: ISS-2025-0263 - cap on an explicitly-enumerated finite domain range.
    private static final long MAX_ENUMERATED_DOMAIN = 10_000_000L;
    // END_CHANGE: ISS-2025-0263

    public ClpfdPredicates(QuerySolver solver, OperationType opType) {
        this.solver = solver;
        this.opType = opType;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException(
                "Context-dependent CLP(FD) built-in must be invoked with context");
    }

    @Override
    public boolean executeWithContext(QuerySolver solver, Term query,
                                      Map<String, Term> bindings,
                                      List<Map<String, Term>> solutions) {
        Term resolved = query.resolveBindings(bindings);
        List<Term> args = resolved.getArguments();

        switch (opType) {
            case IN:            return executeIn(args, bindings, solutions);
            case HASH_EQ:       return executeConstraint(args, bindings, solutions, BinaryConstraint.Type.EQ);
            case HASH_NEQ:      return executeConstraint(args, bindings, solutions, BinaryConstraint.Type.NEQ);
            case HASH_LT:       return executeConstraint(args, bindings, solutions, BinaryConstraint.Type.LT);
            case HASH_GT:       return executeConstraint(args, bindings, solutions, BinaryConstraint.Type.GT);
            case HASH_LEQ:      return executeConstraint(args, bindings, solutions, BinaryConstraint.Type.LEQ);
            case HASH_GEQ:      return executeConstraint(args, bindings, solutions, BinaryConstraint.Type.GEQ);
            case ALL_DIFFERENT: return executeAllDifferent(args, bindings, solutions);
            case LABEL:         return executeLabel(args, bindings, solutions);
            case LABELING:      return executeLabeling(args, bindings, solutions);
            case INDOMAIN:      return executeIndomain(args, bindings, solutions);
            case FD_DOM:        return executeFdDom(args, bindings, solutions);
            case FD_SIZE:       return executeFdSize(args, bindings, solutions);
            default:            return false;
        }
    }

    // ================================================================
    // in/2 : X in 1..10 or X in [1,3,5,7]
    // ================================================================

    private boolean executeIn(List<Term> args, Map<String, Term> bindings,
                              List<Map<String, Term>> solutions) {
        if (args == null || args.size() != 2) return false;

        Term varTerm = args.get(0).resolveBindings(bindings);
        Term domTerm = args.get(1).resolveBindings(bindings);

        String varName = getVariableName(varTerm);
        if (varName == null) {
            // If already ground, check membership
            if (varTerm instanceof Number) {
                int val = (int) ((Number) varTerm).getValue().doubleValue();
                Collection<Integer> domValues = parseDomain(domTerm);
                if (domValues != null && domValues.contains(val)) {
                    solutions.add(new HashMap<>(bindings));
                    return true;
                }
            }
            return false;
        }

        Collection<Integer> values = parseDomain(domTerm);
        if (values == null || values.isEmpty()) return false;

        ConstraintStore store = ConstraintStore.getInstance();
        if (store.hasDomain(varName)) {
            // Intersect with existing domain
            if (!store.restrictDomain(varName, values)) {
                return false; // Domain wipeout
            }
        } else {
            store.setDomainValues(varName, values);
        }

        if (!store.propagate()) return false;

        solutions.add(new HashMap<>(bindings));
        return true;
    }

    /**
     * Parse a domain specification: either '..'(Min, Max) or a Prolog list of integers.
     */
    private Collection<Integer> parseDomain(Term domTerm) {
        // Check for range: '..'(Min, Max)
        if (domTerm instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) domTerm;
            String functor = ct.getFunctor().getName();
            List<Term> dArgs = ct.getArguments();

            if ("..".equals(functor) && dArgs.size() == 2) {
                Term minTerm = dArgs.get(0);
                Term maxTerm = dArgs.get(1);
                if (minTerm instanceof Number && maxTerm instanceof Number) {
                    int min = (int) ((Number) minTerm).getValue().doubleValue();
                    int max = (int) ((Number) maxTerm).getValue().doubleValue();
                    // START_CHANGE: ISS-2025-0263 - Domains are materialized as an explicit set of
                    // boxed Integers, so a huge range (e.g. 1..2147483647) would exhaust the heap
                    // (and the int loop counter would overflow at Integer.MAX_VALUE and never
                    // terminate). Reject ranges beyond a sane cap with a resource_error.
                    long span = (long) max - (long) min + 1L;
                    if (span > MAX_ENUMERATED_DOMAIN) {
                        throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                            it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.resourceError(
                                "clpfd_domain_too_large", "in/2"));
                    }
                    // END_CHANGE: ISS-2025-0263
                    List<Integer> vals = new ArrayList<>();
                    for (int i = min; i <= max; i++) {
                        vals.add(i);
                    }
                    return vals;
                }
            }

            // Check for Prolog list [1,3,5,7]
            List<Integer> listVals = termToIntList(domTerm);
            if (listVals != null) {
                return listVals;
            }
        }

        // Try as atom "[]" (empty list)
        if (domTerm instanceof Atom && "[]".equals(((Atom) domTerm).getName())) {
            return Collections.emptyList();
        }

        return null;
    }

    // ================================================================
    // Constraint predicates: #=, #\=, #<, #>, #=<, #>=
    // ================================================================

    private boolean executeConstraint(List<Term> args, Map<String, Term> bindings,
                                       List<Map<String, Term>> solutions,
                                       BinaryConstraint.Type cType) {
        if (args == null || args.size() != 2) return false;

        Term leftTerm = args.get(0).resolveBindings(bindings);
        Term rightTerm = args.get(1).resolveBindings(bindings);

        ConstraintStore store = ConstraintStore.getInstance();

        // Try to evaluate arithmetic expressions on each side
        ExprResult leftResult = evaluateExpr(leftTerm, bindings, store);
        ExprResult rightResult = evaluateExpr(rightTerm, bindings, store);

        // Case 1: Both sides are ground numbers
        if (leftResult.isGround() && rightResult.isGround()) {
            boolean holds = checkRelation(leftResult.groundValue, rightResult.groundValue, cType);
            if (holds) {
                solutions.add(new HashMap<>(bindings));
                return true;
            }
            return false;
        }

        // Case 2: Simple variable vs constant/variable constraints
        if (leftResult.isSimpleVar() && rightResult.isGround()) {
            BinaryConstraint bc = new BinaryConstraint(
                    leftResult.varName, null, null, rightResult.groundValue, cType);
            if (!store.addConstraint(bc)) return false;
            solutions.add(new HashMap<>(bindings));
            return true;
        }

        if (leftResult.isGround() && rightResult.isSimpleVar()) {
            BinaryConstraint bc = new BinaryConstraint(
                    null, leftResult.groundValue, rightResult.varName, null, cType);
            if (!store.addConstraint(bc)) return false;
            solutions.add(new HashMap<>(bindings));
            return true;
        }

        if (leftResult.isSimpleVar() && rightResult.isSimpleVar()) {
            BinaryConstraint bc = new BinaryConstraint(
                    leftResult.varName, null, rightResult.varName, null, cType);
            if (!store.addConstraint(bc)) return false;
            solutions.add(new HashMap<>(bindings));
            return true;
        }

        // Case 3: Arithmetic expression on one or both sides
        // e.g., X #= Y + 3: leftResult is var X, rightResult is expr Y+3
        if (cType == BinaryConstraint.Type.EQ) {
            return handleArithmeticEqConstraint(leftResult, rightResult, bindings, solutions, store);
        }

        // For non-EQ with expressions, we create auxiliary constraints
        // Introduce a temp variable for the expression side if needed
        if (rightResult.isExpr()) {
            String tempVar = freshVar();
            ensureDomainFromExpr(tempVar, rightResult, store);
            ArithmeticConstraint ac = new ArithmeticConstraint(
                    tempVar,
                    rightResult.exprLeftVar, rightResult.exprLeftConst,
                    rightResult.exprRightVar, rightResult.exprRightConst,
                    rightResult.exprOp);
            if (!store.addConstraint(ac)) return false;

            String lVar = leftResult.isSimpleVar() ? leftResult.varName : null;
            Integer lConst = leftResult.isGround() ? leftResult.groundValue : null;
            BinaryConstraint bc = new BinaryConstraint(lVar, lConst, tempVar, null, cType);
            if (!store.addConstraint(bc)) return false;
            solutions.add(new HashMap<>(bindings));
            return true;
        }

        if (leftResult.isExpr()) {
            String tempVar = freshVar();
            ensureDomainFromExpr(tempVar, leftResult, store);
            ArithmeticConstraint ac = new ArithmeticConstraint(
                    tempVar,
                    leftResult.exprLeftVar, leftResult.exprLeftConst,
                    leftResult.exprRightVar, leftResult.exprRightConst,
                    leftResult.exprOp);
            if (!store.addConstraint(ac)) return false;

            String rVar = rightResult.isSimpleVar() ? rightResult.varName : null;
            Integer rConst = rightResult.isGround() ? rightResult.groundValue : null;
            BinaryConstraint bc = new BinaryConstraint(tempVar, null, rVar, rConst, cType);
            if (!store.addConstraint(bc)) return false;
            solutions.add(new HashMap<>(bindings));
            return true;
        }

        // Fallback: couldn't interpret constraint
        return false;
    }

    /**
     * Handle X #= Expr where Expr might be an arithmetic expression.
     */
    private boolean handleArithmeticEqConstraint(ExprResult left, ExprResult right,
                                                   Map<String, Term> bindings,
                                                   List<Map<String, Term>> solutions,
                                                   ConstraintStore store) {
        // X #= Y + 3
        if (left.isSimpleVar() && right.isExpr()) {
            ensureDomainFromExpr(left.varName, right, store);
            ArithmeticConstraint ac = new ArithmeticConstraint(
                    left.varName,
                    right.exprLeftVar, right.exprLeftConst,
                    right.exprRightVar, right.exprRightConst,
                    right.exprOp);
            if (!store.addConstraint(ac)) return false;
            solutions.add(new HashMap<>(bindings));
            return true;
        }

        // Y + 3 #= X
        if (left.isExpr() && right.isSimpleVar()) {
            ensureDomainFromExpr(right.varName, left, store);
            ArithmeticConstraint ac = new ArithmeticConstraint(
                    right.varName,
                    left.exprLeftVar, left.exprLeftConst,
                    left.exprRightVar, left.exprRightConst,
                    left.exprOp);
            if (!store.addConstraint(ac)) return false;
            solutions.add(new HashMap<>(bindings));
            return true;
        }

        // Both expressions or other complex cases: introduce temp vars
        if (left.isExpr()) {
            String tempLeft = freshVar();
            ensureDomainFromExpr(tempLeft, left, store);
            ArithmeticConstraint acLeft = new ArithmeticConstraint(
                    tempLeft, left.exprLeftVar, left.exprLeftConst,
                    left.exprRightVar, left.exprRightConst, left.exprOp);
            if (!store.addConstraint(acLeft)) return false;

            if (right.isExpr()) {
                String tempRight = freshVar();
                ensureDomainFromExpr(tempRight, right, store);
                ArithmeticConstraint acRight = new ArithmeticConstraint(
                        tempRight, right.exprLeftVar, right.exprLeftConst,
                        right.exprRightVar, right.exprRightConst, right.exprOp);
                if (!store.addConstraint(acRight)) return false;
                BinaryConstraint bc = new BinaryConstraint(tempLeft, null, tempRight, null,
                        BinaryConstraint.Type.EQ);
                if (!store.addConstraint(bc)) return false;
            } else {
                String rVar = right.isSimpleVar() ? right.varName : null;
                Integer rConst = right.isGround() ? right.groundValue : null;
                BinaryConstraint bc = new BinaryConstraint(tempLeft, null, rVar, rConst,
                        BinaryConstraint.Type.EQ);
                if (!store.addConstraint(bc)) return false;
            }
            solutions.add(new HashMap<>(bindings));
            return true;
        }

        // Simple var #= simple var or const #= const already handled
        return false;
    }

    // ================================================================
    // all_different/1
    // ================================================================

    private boolean executeAllDifferent(List<Term> args, Map<String, Term> bindings,
                                         List<Map<String, Term>> solutions) {
        if (args == null || args.size() != 1) return false;

        Term listTerm = args.get(0).resolveBindings(bindings);
        List<Term> elements = termToList(listTerm);
        if (elements == null) return false;

        List<String> varNames = new ArrayList<>();
        for (Term elem : elements) {
            Term resolved = elem.resolveBindings(bindings);
            String vn = getVariableName(resolved);
            if (vn != null) {
                varNames.add(vn);
            }
            // Ground elements are fine - they'll be handled during labeling
        }

        if (varNames.isEmpty()) {
            // All ground - check if all different
            Set<Integer> seen = new HashSet<>();
            for (Term elem : elements) {
                Term resolved = elem.resolveBindings(bindings);
                if (resolved instanceof Number) {
                    int val = (int) ((Number) resolved).getValue().doubleValue();
                    if (!seen.add(val)) return false;
                }
            }
            solutions.add(new HashMap<>(bindings));
            return true;
        }

        ConstraintStore store = ConstraintStore.getInstance();
        AllDifferentConstraint adc = new AllDifferentConstraint(varNames);
        if (!store.addConstraint(adc)) return false;

        solutions.add(new HashMap<>(bindings));
        return true;
    }

    // ================================================================
    // label/1 : label([X,Y,Z]) - find concrete values
    // ================================================================

    private boolean executeLabel(List<Term> args, Map<String, Term> bindings,
                                  List<Map<String, Term>> solutions) {
        if (args == null || args.size() != 1) return false;
        return doLabeling(args.get(0), bindings, solutions, false);
    }

    // ================================================================
    // labeling/2 : labeling([ff], [X,Y,Z])
    // ================================================================

    private boolean executeLabeling(List<Term> args, Map<String, Term> bindings,
                                     List<Map<String, Term>> solutions) {
        if (args == null || args.size() != 2) return false;

        Term optionsTerm = args.get(0).resolveBindings(bindings);
        boolean firstFail = false;

        // Parse options
        List<Term> options = termToList(optionsTerm);
        if (options != null) {
            for (Term opt : options) {
                if (opt instanceof Atom && "ff".equals(((Atom) opt).getName())) {
                    firstFail = true;
                }
            }
        }

        return doLabeling(args.get(1), bindings, solutions, firstFail);
    }

    /**
     * Core labeling algorithm using backtracking search.
     */
    private boolean doLabeling(Term varListTerm, Map<String, Term> bindings,
                                List<Map<String, Term>> solutions, boolean firstFail) {
        Term resolved = varListTerm.resolveBindings(bindings);
        List<Term> elements = termToList(resolved);
        if (elements == null) return false;

        // Collect variable names and their original terms
        List<VarInfo> vars = new ArrayList<>();
        for (Term elem : elements) {
            Term res = elem.resolveBindings(bindings);
            String vn = getVariableName(res);
            if (vn != null) {
                vars.add(new VarInfo(vn, res));
            }
            // Skip already-ground terms
        }

        ConstraintStore store = ConstraintStore.getInstance();

        // Verify all variables have domains
        for (VarInfo vi : vars) {
            if (!store.hasDomain(vi.name)) {
                return false; // Cannot label a variable without a domain
            }
        }

        // Search for all solutions
        List<Map<String, Integer>> assignments = new ArrayList<>();
        ConstraintStoreSnapshot snapshot = store.snapshot();
        search(store, vars, 0, new HashMap<>(), assignments, firstFail, snapshot);
        store.restore(snapshot);

        if (assignments.isEmpty()) return false;

        // Generate one solution per valid assignment
        for (Map<String, Integer> assignment : assignments) {
            Map<String, Term> sol = new HashMap<>(bindings);
            for (Map.Entry<String, Integer> entry : assignment.entrySet()) {
                sol.put(entry.getKey(), new Number(entry.getValue()));
            }
            solutions.add(sol);
        }
        return true;
    }

    /**
     * Recursive backtracking search for labeling.
     */
    private void search(ConstraintStore store, List<VarInfo> vars, int idx,
                         Map<String, Integer> currentAssignment,
                         List<Map<String, Integer>> results,
                         boolean firstFail,
                         ConstraintStoreSnapshot originalSnapshot) {
        if (idx >= vars.size()) {
            // All variables assigned
            results.add(new HashMap<>(currentAssignment));
            return;
        }

        // Select next variable (first-fail: pick smallest domain)
        int chosenIdx = idx;
        if (firstFail) {
            int minSize = Integer.MAX_VALUE;
            for (int i = idx; i < vars.size(); i++) {
                TreeSet<Integer> dom = store.getDomain(vars.get(i).name);
                if (dom != null && dom.size() < minSize) {
                    minSize = dom.size();
                    chosenIdx = i;
                }
            }
            // Swap chosen to current position
            if (chosenIdx != idx) {
                VarInfo temp = vars.get(idx);
                vars.set(idx, vars.get(chosenIdx));
                vars.set(chosenIdx, temp);
            }
        }

        VarInfo vi = vars.get(idx);
        TreeSet<Integer> domain = store.getDomain(vi.name);
        if (domain == null || domain.isEmpty()) return;

        // Try each value in the domain
        for (int val : new ArrayList<>(domain)) {
            ConstraintStoreSnapshot snap = store.snapshot();

            // Assign variable: set domain to singleton
            store.setDomainValues(vi.name, Collections.singleton(val));
            currentAssignment.put(vi.name, val);

            // Propagate
            if (store.propagate()) {
                // Check for wipeout in remaining variables
                boolean wipeout = false;
                for (int i = idx + 1; i < vars.size(); i++) {
                    TreeSet<Integer> d = store.getDomain(vars.get(i).name);
                    if (d != null && d.isEmpty()) {
                        wipeout = true;
                        break;
                    }
                }
                if (!wipeout) {
                    search(store, vars, idx + 1, currentAssignment, results, firstFail, originalSnapshot);
                }
            }

            // Backtrack
            store.restore(snap);
            currentAssignment.remove(vi.name);
        }
    }

    // ================================================================
    // indomain/1 : indomain(X) - nondeterministically assign values
    // ================================================================

    private boolean executeIndomain(List<Term> args, Map<String, Term> bindings,
                                     List<Map<String, Term>> solutions) {
        if (args == null || args.size() != 1) return false;

        Term varTerm = args.get(0).resolveBindings(bindings);
        String varName = getVariableName(varTerm);
        if (varName == null) {
            // Already ground - succeed once
            solutions.add(new HashMap<>(bindings));
            return true;
        }

        ConstraintStore store = ConstraintStore.getInstance();
        TreeSet<Integer> domain = store.getDomain(varName);
        if (domain == null || domain.isEmpty()) return false;

        // START_CHANGE: ISS-2025-0264 - Only emit values that are locally consistent: for each
        // candidate, assign it and propagate; skip any value that wipes out a linked variable's
        // domain (propagate() returns false on wipeout). Previously every domain value was emitted
        // blindly, yielding solutions that violate the posted constraints. The store is restored
        // after each trial so indomain itself does not commit a value.
        // NOTE: this enforces single-goal local consistency; full cross-goal soundness of
        // `indomain(X), indomain(Y)` still needs store/solver trail integration (tracked, LIM-022).
        boolean any = false;
        for (int val : new ArrayList<>(domain)) {
            ConstraintStoreSnapshot snap = store.snapshot();
            store.setDomainValues(varName, Collections.singleton(val));
            boolean consistent = store.propagate();
            store.restore(snap);
            if (consistent) {
                Map<String, Term> sol = new HashMap<>(bindings);
                sol.put(varName, new Number(val));
                solutions.add(sol);
                any = true;
            }
        }
        return any;
        // END_CHANGE: ISS-2025-0264
    }

    // ================================================================
    // fd_dom/2 : fd_dom(X, Dom) - get the current domain
    // ================================================================

    private boolean executeFdDom(List<Term> args, Map<String, Term> bindings,
                                  List<Map<String, Term>> solutions) {
        if (args == null || args.size() != 2) return false;

        Term varTerm = args.get(0).resolveBindings(bindings);
        Term domResultTerm = args.get(1);

        String varName = getVariableName(varTerm);
        if (varName == null) return false;

        ConstraintStore store = ConstraintStore.getInstance();
        TreeSet<Integer> domain = store.getDomain(varName);
        if (domain == null) return false;

        // Build a Prolog list of domain values
        Term domList = intListToTerm(new ArrayList<>(domain));

        Map<String, Term> sol = new HashMap<>(bindings);
        if (domResultTerm.unify(domList, sol)) {
            solutions.add(sol);
            return true;
        }
        return false;
    }

    // ================================================================
    // fd_size/2 : fd_size(X, Size) - get domain size
    // ================================================================

    private boolean executeFdSize(List<Term> args, Map<String, Term> bindings,
                                   List<Map<String, Term>> solutions) {
        if (args == null || args.size() != 2) return false;

        Term varTerm = args.get(0).resolveBindings(bindings);
        Term sizeTerm = args.get(1);

        String varName = getVariableName(varTerm);
        if (varName == null) return false;

        ConstraintStore store = ConstraintStore.getInstance();
        TreeSet<Integer> domain = store.getDomain(varName);
        if (domain == null) return false;

        Term sizeNumber = new Number(domain.size());

        Map<String, Term> sol = new HashMap<>(bindings);
        if (sizeTerm.unify(sizeNumber, sol)) {
            solutions.add(sol);
            return true;
        }
        return false;
    }

    // ================================================================
    // Helper: expression evaluation for constraint arguments
    // ================================================================

    /**
     * Result of evaluating an expression in a constraint context.
     */
    private static class ExprResult {
        Integer groundValue;        // Non-null if fully evaluated
        String varName;             // Non-null if simple variable
        // For expressions like Y + 3:
        String exprLeftVar;
        Integer exprLeftConst;
        String exprRightVar;
        Integer exprRightConst;
        ArithmeticConstraint.Op exprOp;

        boolean isGround() { return groundValue != null; }
        boolean isSimpleVar() { return varName != null && exprOp == null; }
        boolean isExpr() { return exprOp != null; }
    }

    /**
     * Evaluate a term that appears in a constraint.
     * Returns ground value, variable name, or expression decomposition.
     */
    private ExprResult evaluateExpr(Term term, Map<String, Term> bindings,
                                     ConstraintStore store) {
        ExprResult result = new ExprResult();

        if (term instanceof Number) {
            result.groundValue = (int) ((Number) term).getValue().doubleValue();
            return result;
        }

        if (term instanceof Variable) {
            String vn = ((Variable) term).getName();
            // Check if bound to a number
            Term bound = bindings.get(vn);
            if (bound != null) {
                bound = bound.resolveBindings(bindings);
                if (bound instanceof Number) {
                    result.groundValue = (int) ((Number) bound).getValue().doubleValue();
                    return result;
                }
            }
            result.varName = vn;
            return result;
        }

        if (term instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) term;
            String functor = ct.getFunctor().getName();
            List<Term> cArgs = ct.getArguments();

            if (cArgs.size() == 2) {
                ArithmeticConstraint.Op op = null;
                if ("+".equals(functor)) op = ArithmeticConstraint.Op.ADD;
                else if ("-".equals(functor)) op = ArithmeticConstraint.Op.SUB;
                else if ("*".equals(functor)) op = ArithmeticConstraint.Op.MUL;

                if (op != null) {
                    ExprResult leftR = evaluateExpr(cArgs.get(0), bindings, store);
                    ExprResult rightR = evaluateExpr(cArgs.get(1), bindings, store);

                    // If both sub-expressions are ground, compute
                    if (leftR.isGround() && rightR.isGround()) {
                        result.groundValue = computeOp(leftR.groundValue, rightR.groundValue, op);
                        return result;
                    }

                    // Return as expression
                    result.exprOp = op;
                    if (leftR.isGround()) {
                        result.exprLeftConst = leftR.groundValue;
                    } else if (leftR.isSimpleVar()) {
                        result.exprLeftVar = leftR.varName;
                    } else {
                        // Nested expression - introduce temp var
                        String tempVar = freshVar();
                        ensureDomainFromExpr(tempVar, leftR, store);
                        ArithmeticConstraint ac = new ArithmeticConstraint(
                                tempVar, leftR.exprLeftVar, leftR.exprLeftConst,
                                leftR.exprRightVar, leftR.exprRightConst, leftR.exprOp);
                        store.addConstraint(ac);
                        result.exprLeftVar = tempVar;
                    }
                    if (rightR.isGround()) {
                        result.exprRightConst = rightR.groundValue;
                    } else if (rightR.isSimpleVar()) {
                        result.exprRightVar = rightR.varName;
                    } else {
                        String tempVar = freshVar();
                        ensureDomainFromExpr(tempVar, rightR, store);
                        ArithmeticConstraint ac = new ArithmeticConstraint(
                                tempVar, rightR.exprLeftVar, rightR.exprLeftConst,
                                rightR.exprRightVar, rightR.exprRightConst, rightR.exprOp);
                        store.addConstraint(ac);
                        result.exprRightVar = tempVar;
                    }
                    return result;
                }
            }
        }

        return result;
    }

    private int computeOp(int left, int right, ArithmeticConstraint.Op op) {
        switch (op) {
            case ADD: return left + right;
            case SUB: return left - right;
            case MUL: return left * right;
            default: return 0;
        }
    }

    /**
     * Ensure a temporary variable has a reasonable domain based on expression operands.
     */
    private void ensureDomainFromExpr(String varName, ExprResult expr, ConstraintStore store) {
        if (store.hasDomain(varName)) return;

        // Compute bounds from operand domains
        Collection<Integer> leftVals = getOperandValues(expr.exprLeftVar, expr.exprLeftConst, store);
        Collection<Integer> rightVals = getOperandValues(expr.exprRightVar, expr.exprRightConst, store);

        if (leftVals != null && rightVals != null && expr.exprOp != null) {
            TreeSet<Integer> possibleValues = new TreeSet<>();
            for (int lv : leftVals) {
                for (int rv : rightVals) {
                    possibleValues.add(computeOp(lv, rv, expr.exprOp));
                }
            }
            if (!possibleValues.isEmpty()) {
                store.setDomainValues(varName, possibleValues);
                return;
            }
        }

        // Fallback: use a default domain
        store.setDomain(varName, -1000, 1000);
    }

    private Collection<Integer> getOperandValues(String var, Integer constant, ConstraintStore store) {
        if (constant != null) return Collections.singleton(constant);
        if (var != null) {
            TreeSet<Integer> dom = store.getDomain(var);
            if (dom != null) return dom;
        }
        return null;
    }

    // ================================================================
    // Helper: variable and list utilities
    // ================================================================

    private static int tempVarCounter = 0;

    private static String freshVar() {
        return "_clpfd_tmp_" + (++tempVarCounter);
    }

    /**
     * Get the variable name from a term, or null if the term is not a variable.
     */
    private String getVariableName(Term term) {
        if (term instanceof Variable) {
            return ((Variable) term).getName();
        }
        return null;
    }

    /**
     * Convert a Prolog list term to a Java list of Terms.
     */
    private List<Term> termToList(Term term) {
        List<Term> result = new ArrayList<>();
        Term current = term;
        while (current instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) current;
            if (!".".equals(ct.getFunctor().getName()) || ct.getArguments().size() != 2) {
                return null; // Not a proper list
            }
            result.add(ct.getArguments().get(0));
            current = ct.getArguments().get(1);
        }
        if (current instanceof Atom && "[]".equals(((Atom) current).getName())) {
            return result;
        }
        return null; // Improper list
    }

    /**
     * Convert a Prolog list of numbers to a Java list of integers.
     */
    private List<Integer> termToIntList(Term term) {
        List<Term> terms = termToList(term);
        if (terms == null) return null;
        List<Integer> result = new ArrayList<>();
        for (Term t : terms) {
            if (t instanceof Number) {
                result.add((int) ((Number) t).getValue().doubleValue());
            } else {
                return null; // Non-integer in list
            }
        }
        return result;
    }

    /**
     * Convert a Java list of integers to a Prolog list term.
     */
    private Term intListToTerm(List<Integer> values) {
        Term result = new Atom("[]");
        for (int i = values.size() - 1; i >= 0; i--) {
            result = new CompoundTerm(new Atom("."),
                    Arrays.asList(new Number(values.get(i)), result));
        }
        return result;
    }

    private boolean checkRelation(int left, int right, BinaryConstraint.Type type) {
        switch (type) {
            case EQ:  return left == right;
            case NEQ: return left != right;
            case LT:  return left < right;
            case GT:  return left > right;
            case LEQ: return left <= right;
            case GEQ: return left >= right;
            default:  return false;
        }
    }

    /**
     * Helper class to track variable info during labeling.
     */
    private static class VarInfo {
        final String name;
        final Term originalTerm;

        VarInfo(String name, Term originalTerm) {
            this.name = name;
            this.originalTerm = originalTerm;
        }
    }
}
// END_CHANGE: ISS-2025-0123
