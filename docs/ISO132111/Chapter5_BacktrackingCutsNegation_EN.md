# Chapter 5: Backtracking, Cuts and Negation

## Overview

This chapter provides comprehensive documentation of JProlog's implementation of backtracking mechanisms, cut operators, and negation-as-failure. These control constructs form the foundation of Prolog's execution model and enable sophisticated program control flow.

## 5.1 Backtracking and Cuts

### 5.1.1 Backtracking Revisited

#### Core Backtracking Mechanism

JProlog implements backtracking through a systematic search mechanism that explores alternative solutions when the current path fails. The backtracking mechanism is centralized in the `QuerySolver` class.

```java
// QuerySolver.java - Core backtracking implementation
public class QuerySolver {
    
    /**
     * Solve a goal with current bindings, exploring all choice points.
     * 
     * @param goal The goal to solve
     * @param bindings Current variable bindings
     * @param solutions List to add successful solutions to
     * @param cutStatus Cut control status for backtracking control
     * @return true if any solutions were found
     */
    public boolean solve(Term goal, Map<String, Term> bindings, 
                        List<Map<String, Term>> solutions, CutStatus cutStatus) {
        
        // Recursion protection with depth limiting
        Integer depth = recursionDepth.get();
        if (depth == null) depth = 0;
        
        if (depth > MAX_RECURSION_DEPTH) {
            System.err.println("WARNING: Maximum recursion depth " + 
                             MAX_RECURSION_DEPTH + " reached for goal: " + goal);
            return false;
        }
        
        // Process choice points systematically
        return solveInternalProtected(goal, bindings, solutions, cutStatus);
    }
}
```

#### Choice Point Management

Choice points are created when multiple rules or facts can match a goal. JProlog manages choice points through systematic exploration:

```java
// Example: Multiple facts create choice points
// Database:
// color(red).
// color(blue). 
// color(green).

// Query: ?- color(X).
// Creates choice points for X = red, X = blue, X = green

public boolean solveGoal(Term goal, Map<String, Term> bindings, 
                        List<Map<String, Term>> solutions) {
    boolean foundSolution = false;
    
    // Try all matching clauses (creates choice points)
    for (Clause clause : knowledgeBase.getClauses(goal.getFunctor())) {
        // Create new binding context for this choice point
        Map<String, Term> newBindings = new HashMap<>(bindings);
        
        if (unifyAndSolve(goal, clause, newBindings, solutions)) {
            foundSolution = true;
            // Continue to explore other choice points unless cut encountered
        }
    }
    
    return foundSolution;
}
```

#### Variable Binding and Unbinding

During backtracking, variable bindings must be properly managed:

```java
// Example: Backtracking with variable binding management
// Goal: append([1,2], [3,4], X), member(Y, X).

public boolean solveConjunction(List<Term> goals, Map<String, Term> bindings,
                               List<Map<String, Term>> solutions) {
    if (goals.isEmpty()) {
        solutions.add(new HashMap<>(bindings));
        return true;
    }
    
    Term firstGoal = goals.get(0);
    List<Term> remainingGoals = goals.subList(1, goals.size());
    
    boolean success = false;
    List<Map<String, Term>> firstGoalSolutions = new ArrayList<>();
    
    // Solve first goal - may create multiple choice points
    if (solve(firstGoal, bindings, firstGoalSolutions, CutStatus.notOccurred())) {
        
        // For each solution of first goal, try remaining goals
        for (Map<String, Term> solution : firstGoalSolutions) {
            // Recursive call with new bindings
            if (solveConjunction(remainingGoals, solution, solutions)) {
                success = true;
            }
            // Automatic unbinding on backtrack - new iteration creates fresh context
        }
    }
    
    return success;
}
```

### 5.1.2 Problems with Backtracking

#### Infinite Loops

Unconstrained backtracking can lead to infinite loops, especially with left-recursive rules:

```prolog
% Problematic left-recursive rule
path(X, Y) :- path(X, Z), edge(Z, Y).
path(X, Y) :- edge(X, Y).

% Database
edge(a, b).
edge(b, c).

% Query: ?- path(a, c).
% This creates infinite recursion: path(a,c) -> path(a,Z1) -> path(a,Z2) -> ...
```

JProlog addresses this with recursion depth limiting:

```java
// QuerySolver.java - Recursion protection
private static final ThreadLocal<Integer> recursionDepth = new ThreadLocal<>();
private static final int MAX_RECURSION_DEPTH = 100;

private boolean solveInternal(Term goal, Map<String, Term> bindings, 
                             List<Map<String, Term>> solutions, CutStatus cutStatus) {
    Integer depth = recursionDepth.get();
    if (depth == null) depth = 0;
    
    if (depth > MAX_RECURSION_DEPTH) {
        System.err.println("WARNING: Maximum recursion depth " + 
                         MAX_RECURSION_DEPTH + " reached for goal: " + goal);
        return false; // Prevent stack overflow
    }
    
    try {
        recursionDepth.set(depth + 1);
        return solveInternalProtected(goal, bindings, solutions, cutStatus);
    } finally {
        // Proper cleanup of recursion tracking
        if (depth == 0) {
            recursionDepth.remove();
        } else {
            recursionDepth.set(depth);
        }
    }
}
```

#### Performance Issues

Excessive backtracking can lead to exponential time complexity:

```prolog
% Example: Fibonacci with naive recursion
fib(0, 1).
fib(1, 1).
fib(N, F) :- 
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fib(N1, F1),
    fib(N2, F2),
    F is F1 + F2.

% Query: ?- fib(30, X).
% Creates exponential number of choice points
```

#### Redundant Solutions

Multiple paths can lead to the same solution:

```prolog
% Example: Multiple paths to same solution
connected(X, Y) :- edge(X, Y).
connected(X, Y) :- edge(Y, X).
connected(X, Z) :- connected(X, Y), connected(Y, Z).

% Database
edge(a, b).
edge(b, c).

% Query: ?- connected(a, c).
% May find: connected(a,c) via edge(a,b), edge(b,c)
%          connected(a,c) via edge(b,a), edge(b,c) [if symmetric interpretation]
```

### 5.1.3 Introducing Cuts

#### Cut Operator Implementation

The cut operator (!) prevents backtracking to choice points created before it in the current rule:

```java
// Cut.java - Cut operator implementation
public class Cut implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, 
                          List<Map<String, Term>> solutions) {
        if (query.getArguments() != null && query.getArguments().size() != 0) {
            throw new PrologEvaluationException("cut/0 takes no arguments.");
        }
        
        // Cut always succeeds and adds current bindings
        solutions.add(new HashMap<>(bindings));
        
        // Cut behavior is implemented by:
        // 1. Only returning one solution (first success)  
        // 2. QuerySolver handles cut detection for rule choice points
        
        return true;
    }
}
```

#### Cut Semantics in Rule Processing

Cut affects the processing of alternative clauses:

```java
// QuerySolver.java - Cut handling in rule processing
private boolean solveGoal(Term goal, Map<String, Term> bindings,
                         List<Map<String, Term>> solutions, CutStatus cutStatus) {
    
    boolean foundSolution = false;
    List<Clause> matchingClauses = knowledgeBase.getClauses(goal.getFunctor());
    
    for (int i = 0; i < matchingClauses.size(); i++) {
        Clause clause = matchingClauses.get(i);
        Map<String, Term> clauseBindings = new HashMap<>(bindings);
        
        // Try to unify and solve this clause
        if (unifyGoalWithClause(goal, clause, clauseBindings)) {
            
            // Create mutable cut status for this clause
            MutableCutStatus mutableCutStatus = new MutableCutStatus();
            
            if (solveRuleBody(clause, clauseBindings, solutions, mutableCutStatus)) {
                foundSolution = true;
                
                // Check if cut was encountered
                if (mutableCutStatus.wasCutEncountered()) {
                    // Cut prevents backtracking to remaining clauses
                    break;
                }
            }
        }
    }
    
    return foundSolution;
}
```

#### Green Cut vs Red Cut

**Green Cut** - Removes redundant choice points without affecting correctness:

```prolog
% Green cut example - removes redundant solutions
max(X, Y, X) :- X >= Y, !.
max(X, Y, Y).

% Without cut: ?- max(5, 3, Z). produces Z = 5 twice
% With cut:    ?- max(5, 3, Z). produces Z = 5 once
```

**Red Cut** - Changes program semantics and logical meaning:

```prolog
% Red cut example - changes logical meaning  
p(X) :- q(X), !, r(X).
p(X) :- s(X).

% Database
q(a). r(a).
q(b). 
s(b).

% Without cut: ?- p(X). would produce X = a, X = b
% With cut:    ?- p(X). produces only X = a
% The cut prevents trying s(b) even when r(b) fails
```

#### Cut Implementation with CutStatus

JProlog uses a `CutStatus` system to track cut occurrences:

```java
// CutStatus.java - Cut tracking interface
public interface CutStatus {
    boolean wasCutEncountered();
    
    static CutStatus occurred() {
        return new SimpleCutStatus(true);
    }
    
    static CutStatus notOccurred() {
        return new SimpleCutStatus(false);
    }
}

// MutableCutStatus.java - Mutable cut tracking
public class MutableCutStatus implements CutStatus {
    private boolean cutEncountered = false;
    
    public void setCutEncountered() {
        this.cutEncountered = true;
    }
    
    @Override
    public boolean wasCutEncountered() {
        return cutEncountered;
    }
}
```

### 5.1.4 Problems with Cuts

#### Loss of Logical Purity

Cuts can make programs procedural rather than declarative:

```prolog
% Without cut - pure logical relation
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

% With cut - procedural interpretation
member(X, [X|_]) :- !.
member(X, [_|T]) :- member(X, T).

% The cut version fails to generate all solutions on backtracking
```

#### Order Dependency

Cut introduces dependency on clause ordering:

```prolog
% Version 1 - cut makes clause order critical
classify(X, negative) :- X < 0, !.
classify(X, zero) :- X =:= 0, !.
classify(X, positive).

% Version 2 - reordered clauses give different behavior
classify(X, positive).
classify(X, negative) :- X < 0, !.  % This cut now unreachable for positive X
classify(X, zero) :- X =:= 0, !.
```

#### Debugging Difficulties

Cuts make program behavior harder to trace and debug:

```java
// Debug-friendly implementation without cuts
public boolean solveWithTrace(Term goal, Map<String, Term> bindings,
                             List<Map<String, Term>> solutions) {
    if (traceEnabled) {
        System.out.println("Trying goal: " + goal + " with bindings: " + bindings);
    }
    
    // All choice points are explored and visible in trace
    boolean success = solveAllChoicePoints(goal, bindings, solutions);
    
    if (traceEnabled) {
        System.out.println("Goal " + goal + " " + (success ? "succeeded" : "failed") + 
                          " with " + solutions.size() + " solutions");
    }
    
    return success;
}
```

## 5.2 Negation as Failure

### 5.2.1 The Closed World Assumption

JProlog implements negation based on the Closed World Assumption (CWA): if a fact cannot be proven from the current knowledge base, it is assumed to be false.

```java
// Example of closed world reasoning
// Database:
// bird(robin).
// bird(sparrow).

// Query: ?- \+ bird(penguin).
// Succeeds because penguin cannot be proven to be a bird
// This assumes the knowledge base is complete for the bird predicate
```

#### Implementation of Closed World Assumption

```java
// NegationAsFailure.java - CWA implementation
public class NegationAsFailure implements BuiltInWithContext {
    
    @Override
    public boolean executeWithContext(QuerySolver solver, Term query, Map<String, Term> bindings, 
                                     List<Map<String, Term>> solutions) {
        
        Term goal = query.getArguments().get(0).resolveBindings(bindings);
        
        // Try to solve the goal under CWA
        List<Map<String, Term>> goalSolutions = new ArrayList<>();
        boolean goalSucceeds = solver.solve(goal, new HashMap<>(bindings), 
                                          goalSolutions, CutStatus.notOccurred());
        
        // CWA: succeed if goal cannot be proven (fails)
        if (!goalSucceeds || goalSolutions.isEmpty()) {
            solutions.add(new HashMap<>(bindings));
            return true; // Goal unprovable, so negation succeeds
        } else {
            return false; // Goal provable, so negation fails
        }
    }
}
```

#### Limitations of Closed World Assumption

```prolog
% Example showing CWA limitations
mortal(X) :- human(X).
human(socrates).

% Query: ?- \+ mortal(aristotle).
% Succeeds under CWA, but may be incorrect if knowledge is incomplete
% We cannot prove aristotle is mortal, but he might be human (unknown)
```

### 5.2.2 The \+ Operator

#### Syntax and Semantics

The `\+` operator implements negation as failure:

```prolog
% Syntax: \+ Goal
% Semantics: Succeeds if Goal fails, fails if Goal succeeds

% Examples:
?- \+ fail.        % Succeeds (fail always fails)
?- \+ true.        % Fails (true always succeeds)
?- \+ (1 = 2).     % Succeeds (unification fails)
?- \+ (1 = 1).     % Fails (unification succeeds)
```

#### Implementation Details

```java
// NegationAsFailure.java - Complete implementation
public class NegationAsFailure implements BuiltInWithContext {
    
    private QuerySolver solver;
    
    public NegationAsFailure(QuerySolver solver) {
        this.solver = solver;
    }
    
    @Override
    public boolean executeWithContext(QuerySolver solver, Term query, 
                                     Map<String, Term> bindings, 
                                     List<Map<String, Term>> solutions) {
        
        // Validate argument structure
        if (query.getArguments() == null || query.getArguments().size() != 1) {
            throw new IllegalArgumentException("\\+ requires exactly one argument");
        }
        
        Term goal = query.getArguments().get(0).resolveBindings(bindings);
        
        // Create fresh binding context to avoid variable pollution
        List<Map<String, Term>> goalSolutions = new ArrayList<>();
        boolean goalSucceeds = solver.solve(goal, new HashMap<>(bindings), 
                                          goalSolutions, CutStatus.notOccurred());
        
        // Negation as failure logic
        if (!goalSucceeds || goalSolutions.isEmpty()) {
            // Goal failed - negation succeeds with original bindings
            solutions.add(new HashMap<>(bindings));
            return true;
        } else {
            // Goal succeeded - negation fails
            return false;
        }
    }
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, 
                          List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("\\+ requires context");
    }
}
```

#### Variable Scoping in Negation

Variables in negated goals have special scoping rules:

```prolog
% Example: Variable scoping in negation
?- \+ (X = 1), X = 2.
% Succeeds with X = 2
% The binding X = 1 inside \+ does not escape

% Implementation ensures proper variable isolation:
```

```java
// Variable isolation in negation
public boolean executeNegation(Term goal, Map<String, Term> bindings,
                              List<Map<String, Term>> solutions) {
    // Create isolated binding context
    Map<String, Term> isolatedBindings = new HashMap<>(bindings);
    List<Map<String, Term>> goalSolutions = new ArrayList<>();
    
    boolean goalSucceeds = solver.solve(goal, isolatedBindings, goalSolutions, 
                                       CutStatus.notOccurred());
    
    if (!goalSucceeds) {
        // Return original bindings (not isolated ones)
        solutions.add(new HashMap<>(bindings));
        return true;
    }
    
    return false;
}
```

## 5.3 Disjunction

### 5.3.1 Semicolon Operator (;)

The semicolon operator implements disjunction (OR logic):

```prolog
% Syntax: Goal1 ; Goal2
% Semantics: Succeeds if Goal1 OR Goal2 succeeds
% Both goals are tried, collecting all solutions

% Examples:
?- (X = 1 ; X = 2).     % X = 1, X = 2
?- (fail ; true).       % Succeeds
?- (true ; fail).       % Succeeds
```

#### Implementation of Simple Disjunction

```java
// IfThenElse.java - Disjunction implementation
private boolean executeDisjunction(Term leftTerm, Term rightTerm, 
                                 Map<String, Term> bindings, 
                                 List<Map<String, Term>> solutions, 
                                 QuerySolver solver) {
    boolean success = false;
    
    // Try left alternative first
    List<Map<String, Term>> leftSolutions = new ArrayList<>();
    boolean leftSuccess = solver.solve(leftTerm, new HashMap<>(bindings), 
                                      leftSolutions, CutStatus.notOccurred());
    if (leftSuccess) {
        solutions.addAll(leftSolutions);
        success = true;
    }
    
    // Try right alternative
    List<Map<String, Term>> rightSolutions = new ArrayList<>();
    boolean rightSuccess = solver.solve(rightTerm, new HashMap<>(bindings), 
                                       rightSolutions, CutStatus.notOccurred());
    if (rightSuccess) {
        solutions.addAll(rightSolutions);
        success = true;
    }
    
    return success;
}
```

### 5.3.2 If-Then-Else Construct

The if-then-else construct combines conditional execution with disjunction:

```prolog
% Syntax: (Condition -> Then ; Else)
% Semantics: If Condition succeeds, execute Then; otherwise execute Else

% Examples:
?- (1 = 1 -> write(true) ; write(false)).    % Outputs: true
?- (1 = 2 -> write(true) ; write(false)).    % Outputs: false
```

#### Implementation of If-Then-Else

```java
// IfThenElse.java - Complete if-then-else implementation
public class IfThenElse implements BuiltInWithContext {
    
    private boolean executeIfThenElse(Term condition, Term thenTerm, Term elseTerm, 
                                    Map<String, Term> bindings, 
                                    List<Map<String, Term>> solutions, 
                                    QuerySolver solver) {
        
        // Try to solve the condition
        List<Map<String, Term>> conditionSolutions = new ArrayList<>();
        boolean conditionSuccess = solver.solve(condition, new HashMap<>(bindings), 
                                               conditionSolutions, CutStatus.notOccurred());
        
        if (conditionSuccess && !conditionSolutions.isEmpty()) {
            // Condition succeeded - execute Then part for each solution
            boolean success = false;
            for (Map<String, Term> conditionBinding : conditionSolutions) {
                List<Map<String, Term>> thenSolutions = new ArrayList<>();
                boolean thenSuccess = solver.solve(thenTerm, new HashMap<>(conditionBinding), 
                                                 thenSolutions, CutStatus.notOccurred());
                if (thenSuccess) {
                    solutions.addAll(thenSolutions);
                    success = true;
                }
            }
            return success;
        } else {
            // Condition failed - execute Else part
            List<Map<String, Term>> elseSolutions = new ArrayList<>();
            boolean elseSuccess = solver.solve(elseTerm, new HashMap<>(bindings), 
                                             elseSolutions, CutStatus.notOccurred());
            if (elseSuccess) {
                solutions.addAll(elseSolutions);
            }
            return elseSuccess;
        }
    }
}
```

#### Deterministic vs Non-Deterministic If-Then-Else

```prolog
% Deterministic: Condition commits to first solution
?- (member(X, [1,2,3]) -> Y = X ; Y = none).
% Results: Y = 1 (commits to first solution of member/2)

% Non-deterministic version would need:
?- member(X, [1,2,3]), Y = X.
% Results: Y = 1, Y = 2, Y = 3 (all solutions)
```

## 5.4 Example: Evaluating Logic Formulas

### 5.4.1 Boolean Formula Representation

```prolog
% Represent boolean formulas as Prolog terms
% and(X, Y)     - Logical AND
% or(X, Y)      - Logical OR  
% not(X)        - Logical NOT
% true, false   - Boolean constants
% Variables: p, q, r, etc.

% Example formulas:
% F1 = and(p, or(q, not(r)))
% F2 = or(and(p, q), and(not(p), r))
```

### 5.4.2 Formula Evaluation Implementation

```prolog
% evaluate(Formula, Assignment, Result)
% Evaluates Formula under variable Assignment, producing Result

% Base cases - constants
evaluate(true, _, true).
evaluate(false, _, false).

% Variable lookup
evaluate(Var, Assignment, Value) :-
    atom(Var),
    member(Var = Value, Assignment).

% Logical operators
evaluate(and(X, Y), Assignment, Result) :-
    evaluate(X, Assignment, XVal),
    evaluate(Y, Assignment, YVal),
    and_truth(XVal, YVal, Result).

evaluate(or(X, Y), Assignment, Result) :-
    evaluate(X, Assignment, XVal),
    evaluate(Y, Assignment, YVal), 
    or_truth(XVal, YVal, Result).

evaluate(not(X), Assignment, Result) :-
    evaluate(X, Assignment, XVal),
    not_truth(XVal, Result).

% Truth tables
and_truth(true, true, true).
and_truth(true, false, false).
and_truth(false, true, false).
and_truth(false, false, false).

or_truth(true, true, true).
or_truth(true, false, true).
or_truth(false, true, true).
or_truth(false, false, false).

not_truth(true, false).
not_truth(false, true).
```

### 5.4.3 Java Implementation

```java
// LogicFormulaEvaluator.java - Java implementation for JProlog
public class LogicFormulaEvaluator implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, 
                          List<Map<String, Term>> solutions) {
        
        // evaluate(Formula, Assignment, Result)
        if (query.getArguments().size() != 3) {
            throw new PrologEvaluationException("evaluate/3 requires 3 arguments");
        }
        
        Term formula = query.getArguments().get(0).resolveBindings(bindings);
        Term assignment = query.getArguments().get(1).resolveBindings(bindings);
        Term result = query.getArguments().get(2);
        
        try {
            Term evaluatedResult = evaluateFormula(formula, assignment);
            
            // Try to unify with result
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (result.unify(evaluatedResult, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            
        } catch (EvaluationException e) {
            // Formula evaluation failed
            return false;
        }
        
        return false;
    }
    
    private Term evaluateFormula(Term formula, Term assignment) throws EvaluationException {
        
        // Handle constants
        if (formula instanceof Atom) {
            String name = ((Atom) formula).getName();
            if ("true".equals(name)) return new Atom("true");
            if ("false".equals(name)) return new Atom("false");
            
            // Variable lookup in assignment
            return lookupVariable(formula, assignment);
        }
        
        // Handle compound formulas
        if (formula instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) formula;
            String functor = compound.getFunctor().getName();
            
            switch (functor) {
                case "and":
                    return evaluateAnd(compound.getArguments(), assignment);
                case "or":
                    return evaluateOr(compound.getArguments(), assignment);
                case "not":
                    return evaluateNot(compound.getArguments(), assignment);
                default:
                    throw new EvaluationException("Unknown operator: " + functor);
            }
        }
        
        throw new EvaluationException("Invalid formula: " + formula);
    }
    
    private Term evaluateAnd(List<Term> args, Term assignment) throws EvaluationException {
        if (args.size() != 2) {
            throw new EvaluationException("and/2 requires exactly 2 arguments");
        }
        
        Term left = evaluateFormula(args.get(0), assignment);
        Term right = evaluateFormula(args.get(1), assignment);
        
        boolean leftTrue = isTrue(left);
        boolean rightTrue = isTrue(right);
        
        return new Atom(leftTrue && rightTrue ? "true" : "false");
    }
    
    private Term evaluateOr(List<Term> args, Term assignment) throws EvaluationException {
        if (args.size() != 2) {
            throw new EvaluationException("or/2 requires exactly 2 arguments");
        }
        
        Term left = evaluateFormula(args.get(0), assignment);
        Term right = evaluateFormula(args.get(1), assignment);
        
        boolean leftTrue = isTrue(left);
        boolean rightTrue = isTrue(right);
        
        return new Atom(leftTrue || rightTrue ? "true" : "false");
    }
    
    private Term evaluateNot(List<Term> args, Term assignment) throws EvaluationException {
        if (args.size() != 1) {
            throw new EvaluationException("not/1 requires exactly 1 argument");
        }
        
        Term operand = evaluateFormula(args.get(0), assignment);
        boolean operandTrue = isTrue(operand);
        
        return new Atom(!operandTrue ? "true" : "false");
    }
    
    private boolean isTrue(Term term) {
        return term instanceof Atom && "true".equals(((Atom) term).getName());
    }
}
```

### 5.4.4 Example Usage

```prolog
% Example 1: Simple AND evaluation
?- evaluate(and(true, false), [], Result).
Result = false.

% Example 2: Variable assignment
?- evaluate(and(p, q), [p = true, q = false], Result).
Result = false.

% Example 3: Complex formula
?- evaluate(or(and(p, q), not(p)), [p = true, q = false], Result).
Result = false.  % (true AND false) OR (NOT true) = false OR false = false

% Example 4: Tautology checking
?- evaluate(or(p, not(p)), [p = true], Result).
Result = true.

?- evaluate(or(p, not(p)), [p = false], Result).  
Result = true.
```

### 5.4.5 Applications in Program Analysis

The logic formula evaluator can be used for program analysis:

```prolog
% Check if formula is satisfiable
satisfiable(Formula) :-
    variable_assignment(Assignment),
    evaluate(Formula, Assignment, true).

% Generate all variable assignments
variable_assignment([]).
variable_assignment([Var = true | Rest]) :-
    variable_assignment(Rest).
variable_assignment([Var = false | Rest]) :-
    variable_assignment(Rest).

% Check if formula is a tautology
tautology(Formula) :-
    \+ (variable_assignment(Assignment),
        evaluate(Formula, Assignment, false)).

% Example: Check tautology
?- tautology(or(p, not(p))).     % Yes - always true
?- tautology(and(p, not(p))).    % No - contradiction
```

## 5.5 Advanced Control Constructs

### 5.5.1 Once Predicate

The `once/1` predicate executes its argument exactly once, cutting choice points:

```java
// Once.java - Implementation of once/1
public class Once implements BuiltInWithContext {
    
    @Override
    public boolean executeWithContext(QuerySolver solver, Term query, 
                                     Map<String, Term> bindings,
                                     List<Map<String, Term>> solutions) {
        
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("once/1 requires exactly one argument");
        }
        
        Term goal = query.getArguments().get(0).resolveBindings(bindings);
        
        // Solve goal but take only first solution
        List<Map<String, Term>> goalSolutions = new ArrayList<>();
        boolean success = solver.solve(goal, new HashMap<>(bindings), 
                                     goalSolutions, CutStatus.notOccurred());
        
        if (success && !goalSolutions.isEmpty()) {
            // Take only first solution - this implements the "once" semantics
            solutions.add(goalSolutions.get(0));
            return true;
        }
        
        return false;
    }
}
```

### 5.5.2 Ignore Predicate

The `ignore/1` predicate always succeeds, whether its argument succeeds or fails:

```java
// Ignore.java - Implementation of ignore/1
public class Ignore implements BuiltInWithContext {
    
    @Override
    public boolean executeWithContext(QuerySolver solver, Term query,
                                     Map<String, Term> bindings,
                                     List<Map<String, Term>> solutions) {
        
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("ignore/1 requires exactly one argument");
        }
        
        Term goal = query.getArguments().get(0).resolveBindings(bindings);
        
        // Try to solve goal
        List<Map<String, Term>> goalSolutions = new ArrayList<>();
        boolean success = solver.solve(goal, new HashMap<>(bindings),
                                     goalSolutions, CutStatus.notOccurred());
        
        if (success && !goalSolutions.isEmpty()) {
            // Goal succeeded - use its solutions
            solutions.addAll(goalSolutions);
        } else {
            // Goal failed - ignore failure and succeed with original bindings
            solutions.add(new HashMap<>(bindings));
        }
        
        return true; // ignore/1 always succeeds
    }
}
```

## Conclusion

This chapter covered the essential control constructs in JProlog:

1. **Backtracking**: Systematic exploration of solution spaces with proper choice point management and recursion protection
2. **Cut Operator**: Prevention of backtracking with careful distinction between green and red cuts
3. **Negation as Failure**: Implementation of closed-world reasoning with proper variable scoping
4. **Disjunction**: Support for alternative execution paths through semicolon operator and if-then-else constructs
5. **Advanced Constructs**: Meta-predicates like `once/1` and `ignore/1` for specialized control flow

These constructs provide the foundation for sophisticated Prolog programming while maintaining the declarative nature of the language where possible. The implementation emphasizes correctness, performance, and debugging support.

---

*This documentation reflects JProlog's implementation of ISO Prolog control constructs with Java-specific optimizations and extensions.*