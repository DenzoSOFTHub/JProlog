package it.denzosoft.jprolog.core.engine.v2;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.BuiltInRegistry;
import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.CutStatus;
import it.denzosoft.jprolog.core.engine.KnowledgeBase;
import it.denzosoft.jprolog.core.engine.QuerySolver;
import it.denzosoft.jprolog.core.engine.Rule;
import it.denzosoft.jprolog.core.engine.TableStore;
import it.denzosoft.jprolog.core.module.Module;
import it.denzosoft.jprolog.core.module.ModuleManager;
import it.denzosoft.jprolog.core.module.PredicateSignature;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Clean-room prototype of a new resolution engine core, addressing the architectural debt of the
 * eager {@code QuerySolver} (LIM-023/024). Three design changes, validated by {@code MachineSolverTest}:
 *
 * <ol>
 *   <li><b>Mutable bindings + trail</b> instead of copying a {@code Map<String,Term>} substitution
 *       per step: unification binds into one store and records each binding on a trail; backtracking
 *       undoes to a mark in O(changes), not O(depth × bindings).</li>
 *   <li><b>Lazy enumeration</b>: solutions are produced one at a time through a {@link SolutionSink};
 *       a caller can stop after the first, so cut prunes correctly and {@code repeat}-style generators
 *       do not blow up memory (the eager model collects every solution up-front).</li>
 *   <li><b>Iterative SLD machine</b> with an explicit goal stack + choice-point stack on the heap, so
 *       deep recursion does <b>not</b> overflow the Java call stack (the eager solver recurses in Java
 *       and dies with {@code StackOverflowError}).</li>
 * </ol>
 *
 * <p>This is a focused prototype (true / fail / {@code ,}/2 / {@code ;}/2 / {@code =}/2 / {@code !} /
 * {@code call/1} / user clauses) — enough to demonstrate the model end-to-end. It is NOT wired into
 * the engine; full builtin coverage + integration is the multi-step v3-engine effort.
 */
public final class MachineSolver {

    // ----------------------------------------------------------------- knowledge base
    private final Map<String, List<Rule>> kb = new HashMap<>();   // used when liveKb == null
    private final KnowledgeBase liveKb;        // when set, clause lookup + assert/retract delegate here
    private final BuiltInRegistry registry;    // nullable: when set, non-native goals delegate here
    private final QuerySolver contextSolver;   // nullable: solver handed to BuiltInWithContext builtins
    private final ModuleManager modules;       // nullable: when set, clause lookup is module-aware
    private final TableStore tableStore;       // nullable: when set, tabled predicates delegate to the legacy solver
    private int renameCounter = 0;

    public MachineSolver(List<Rule> rules) { this(rules, null); }

    public MachineSolver(List<Rule> rules, BuiltInRegistry registry) {
        for (Rule r : rules) kb.computeIfAbsent(key(r.getHead()), k -> new ArrayList<>()).add(r);
        this.registry = registry;
        this.liveKb = null;
        this.contextSolver = null;
        this.modules = null;
        this.tableStore = null;
    }

    /** Engine-integrated mode: read clauses from and assert/retract to the live {@link KnowledgeBase}
     *  (module-aware via {@code modules}); delegate {@link BuiltInWithContext} built-ins to {@code contextSolver}. */
    public MachineSolver(KnowledgeBase liveKb, BuiltInRegistry registry, QuerySolver contextSolver,
                         ModuleManager modules, TableStore tableStore) {
        this.liveKb = liveKb;
        this.registry = registry;
        this.contextSolver = contextSolver;
        this.modules = modules;
        this.tableStore = tableStore;
    }

    private List<Rule> clausesFor(Term lookup) {
        // Use the module manager for a Module:Goal qualified call (always), and for unqualified goals
        // ONLY when user-defined modules exist (>1 module incl. "user") — so it enforces import/export
        // visibility for module programs. Plain (no-module) programs use the flat KB, because routing
        // every lookup through the module manager changes clause-set/assert semantics and destabilises
        // them (ISS-2025-0314).
        if (modules != null) {
            boolean qualified = lookup instanceof CompoundTerm
                && ":".equals(((CompoundTerm) lookup).getName()) && ((CompoundTerm) lookup).getArguments().size() == 2;
            if (qualified) {
                return qualifiedClauses((CompoundTerm) lookup);     // with export enforcement
            }
            if (modules.getAllModuleNames().size() > 1) {
                try {
                    return modules.getRulesForPredicate(lookup);    // unqualified: current module + imports
                } catch (RuntimeException e) {
                    return null;
                }
            }
        }
        if (liveKb != null) {
            String f; int ar;
            if (lookup instanceof Atom) { f = ((Atom) lookup).getName(); ar = 0; }
            else { CompoundTerm c = (CompoundTerm) lookup; f = c.getName(); ar = c.getArguments().size(); }
            return liveKb.getRulesForPredicate(f, ar);
        }
        return kb.get(key(lookup));
    }

    /** Clauses for a {@code Module:Goal} call, enforcing export visibility when the caller is a
     *  different module (ISS-2025-0314): a non-exported predicate is invisible from outside. */
    private List<Rule> qualifiedClauses(CompoundTerm qc) {
        Term mt = deref(qc.getArguments().get(0));
        Term g = deref(qc.getArguments().get(1));
        if (!(mt instanceof Atom) || !(g instanceof Atom || g instanceof CompoundTerm)) return null;
        Module mod = modules.getModule(((Atom) mt).getName());
        if (mod == null) return null;                              // unknown module -> fail
        String f = (g instanceof Atom) ? ((Atom) g).getName() : ((CompoundTerm) g).getName();
        int ar = (g instanceof Atom) ? 0 : ((CompoundTerm) g).getArguments().size();
        PredicateSignature sig = new PredicateSignature(f, ar);
        // A qualified Module:Goal enforces export visibility: a non-exported predicate is invisible.
        if (mod.resolvePredicateForExternalAccess(sig) == null) {
            return new ArrayList<>();                              // not exported -> not visible
        }
        return mod.getRulesForPredicate(sig);
    }

    private static String key(Term head) {
        if (head instanceof Atom) return ((Atom) head).getName() + "/0";
        if (head instanceof CompoundTerm) return ((CompoundTerm) head).getName() + "/" + ((CompoundTerm) head).getArguments().size();
        return "?";
    }

    // ----------------------------------------------------------------- bindings + trail
    private final Map<String, Term> binding = new HashMap<>();
    private final ArrayList<String> trail = new ArrayList<>();

    private int mark() { return trail.size(); }
    private void undo(int m) {
        for (int i = trail.size() - 1; i >= m; i--) binding.remove(trail.get(i));
        if (m < trail.size()) trail.subList(m, trail.size()).clear();
    }
    private void bind(String var, Term val) { binding.put(var, val); trail.add(var); }

    private Term deref(Term t) {
        while (t instanceof Variable) {
            Term b = binding.get(((Variable) t).getName());
            if (b == null) return t;
            t = b;
        }
        return t;
    }

    /** Does the variable {@code name} occur in {@code term}? (iterative, derefs through bindings.) */
    private boolean occurs(String name, Term term) {
        java.util.ArrayDeque<Term> stack = new java.util.ArrayDeque<>();
        stack.push(term);
        while (!stack.isEmpty()) {
            Term t = deref(stack.pop());
            if (t instanceof Variable) {
                if (((Variable) t).getName().equals(name)) return true;
            } else if (t instanceof CompoundTerm) {
                for (Term arg : ((CompoundTerm) t).getArguments()) stack.push(arg);
            }
        }
        return false;
    }

    /** Coroutining wake-up under v2 (ISS-2025-0318): when an attributed variable {@code v} is bound to
     *  {@code value}, invoke the engine's attribute-unify hook (set by {@code Prolog.solveWithV2Engine})
     *  so freeze/when/dif goals fire (or re-suspend). Returns false if the hook fails the unification. */
    private boolean wakeAttrs(Variable v, Term value) {
        if (!v.hasAttributes()) return true;
        Variable.AttributeUnifyHook hook = Variable.getAttributeUnifyHook();
        if (hook == null) return true;
        return hook.onAttributeUnify(v, value, binding);
    }

    private boolean unify(Term a, Term b) {
        a = deref(a); b = deref(b);
        if (a instanceof Variable) {
            if (b instanceof Variable && ((Variable) a).getName().equals(((Variable) b).getName())) return true;
            if (Variable.isOccursCheckEnabled() && occurs(((Variable) a).getName(), b)) return false;
            bind(((Variable) a).getName(), b);
            if (!(b instanceof Variable) && ((Variable) a).hasAttributes()) return wakeAttrs((Variable) a, b);
            return true;
        }
        if (b instanceof Variable) {
            if (Variable.isOccursCheckEnabled() && occurs(((Variable) b).getName(), a)) return false;
            bind(((Variable) b).getName(), a);
            if (((Variable) b).hasAttributes()) return wakeAttrs((Variable) b, a);
            return true;
        }
        if (a instanceof Atom && b instanceof Atom) return ((Atom) a).getName().equals(((Atom) b).getName());
        if (a instanceof Number && b instanceof Number) return a.equals(b);
        if (a instanceof CompoundTerm && b instanceof CompoundTerm) {
            CompoundTerm ca = (CompoundTerm) a, cb = (CompoundTerm) b;
            if (!ca.getName().equals(cb.getName())) return false;
            if (ca.getArguments().size() != cb.getArguments().size()) return false;
            for (int i = 0; i < ca.getArguments().size(); i++) {
                if (!unify(ca.getArguments().get(i), cb.getArguments().get(i))) return false;
            }
            return true;
        }
        return false;
    }

    // ----------------------------------------------------------------- machine state
    /** A pending goal plus the choice-point height a {@code !} in it should cut back to. An optional
     *  {@code action} (with no term) runs an internal side effect — used by soft-cut. */
    private static final class Goal {
        final Term term; final int cutBarrier; final Goal next; final Runnable action;
        Goal(Term term, int cutBarrier, Goal next) { this.term = term; this.cutBarrier = cutBarrier; this.next = next; this.action = null; }
        Goal(Runnable action, Goal next) { this.term = null; this.cutBarrier = 0; this.next = next; this.action = action; }
    }
    /** An alternative: install the next goal stack, or return FAILED if it doesn't apply. */
    private interface Alt { Goal apply(); }
    private static final Goal FAILED = new Goal(null, -1, null);

    private static final class CP {
        final List<Alt> alts; int idx; final int trailMark;
        final int legacyMark;   // ISS-2025-0316: snapshot of the legacy backtrackable Trail (b_setval, op/3, setarg)
        // catch-frame payload (isCatch == true => no alternatives; used by throw/1 unwinding)
        final boolean isCatch; final Term catcher, recovery; final Goal cont; final int cutBarrier;
        CP(List<Alt> alts, int trailMark) {
            this.alts = alts; this.trailMark = trailMark;
            this.legacyMark = it.denzosoft.jprolog.core.engine.Trail.mark();
            this.isCatch = false; this.catcher = null; this.recovery = null; this.cont = null; this.cutBarrier = 0;
        }
        CP(int trailMark, Term catcher, Term recovery, Goal cont, int cutBarrier) {
            this.alts = null; this.trailMark = trailMark;
            this.legacyMark = it.denzosoft.jprolog.core.engine.Trail.mark();
            this.isCatch = true; this.catcher = catcher; this.recovery = recovery; this.cont = cont; this.cutBarrier = cutBarrier;
        }
    }

    private Goal goalStack;
    private final ArrayList<CP> cps = new ArrayList<>();

    public interface SolutionSink { boolean onSolution(Map<String, Term> solution); }

    /** Solve {@code query}, streaming each solution; the sink returns false to stop. */
    public void solve(Term query, SolutionSink sink) {
        binding.clear(); trail.clear(); cps.clear();
        List<String> queryVars = new ArrayList<>();
        collectVars(query, queryVars);
        goalStack = new Goal(query, 0, null);
        drive(() -> sink.onSolution(snapshot(queryVars)), 0);
    }

    private interface Driver { boolean onSolution(); }

    /** Run the machine until exhausted. {@code floor} is the choice-point height below which this
     *  run must not backtrack, so a nested run (findall/catch) leaves the caller's choice points. */
    private void drive(Driver onSol, int floor) {
        while (true) {
          try {
            if (goalStack == null) {                                   // all goals solved -> a solution
                if (!onSol.onSolution() || !backtrack(floor)) return;
                continue;
            }
            Goal g = goalStack;
            goalStack = g.next;
            if (g.action != null) { g.action.run(); continue; }       // soft-cut side effect
            Term t = deref(g.term);

            if (t instanceof Atom) {
                String n = ((Atom) t).getName();
                if ("true".equals(n)) continue;
                if ("fail".equals(n) || "false".equals(n)) { if (!backtrack(floor)) return; continue; }
                if ("!".equals(n)) { cut(g.cutBarrier); continue; }
                int rb0 = bridgeBuiltin(t, n, 0);
                if (rb0 == 1) continue;
                if (rb0 == 0) { if (!backtrack(floor)) return; continue; }
                if (!callUser(t, t)) { if (!backtrack(floor)) return; }
                continue;
            }
            if (t instanceof CompoundTerm) {
                CompoundTerm c = (CompoundTerm) t;
                String f = c.getName(); List<Term> a = c.getArguments();
                if (",".equals(f) && a.size() == 2) {
                    goalStack = new Goal(a.get(0), g.cutBarrier, new Goal(a.get(1), g.cutBarrier, goalStack));
                    continue;
                }
                if (";".equals(f) && a.size() == 2) {
                    Term left = deref(a.get(0));
                    if (left instanceof CompoundTerm && ((CompoundTerm) left).getArguments().size() == 2
                            && "->".equals(((CompoundTerm) left).getName())) {       // (C -> T ; E)
                        CompoundTerm arrow = (CompoundTerm) left;
                        ite(arrow.getArguments().get(0), arrow.getArguments().get(1), a.get(1), g.cutBarrier);
                    } else if (left instanceof CompoundTerm && ((CompoundTerm) left).getArguments().size() == 2
                            && "*->".equals(((CompoundTerm) left).getName())) {      // (C *-> T ; E)
                        CompoundTerm sc = (CompoundTerm) left;
                        softCut(sc.getArguments().get(0), sc.getArguments().get(1), a.get(1), g.cutBarrier);
                    } else {
                        disjunction(a.get(0), a.get(1), g.cutBarrier);
                    }
                    continue;
                }
                if ("->".equals(f) && a.size() == 2) {                // (C -> T)  ==  (C -> T ; fail)
                    ite(a.get(0), a.get(1), new Atom("fail"), g.cutBarrier);
                    continue;
                }
                if ("=".equals(f) && a.size() == 2) {
                    if (!unify(a.get(0), a.get(1))) { if (!backtrack(floor)) return; }
                    continue;
                }
                if (("\\+".equals(f) || "not".equals(f)) && a.size() == 1) {     // negation as failure
                    ite(a.get(0), new Atom("fail"), new Atom("true"), g.cutBarrier);
                    continue;
                }
                if ("call".equals(f) && a.size() >= 1) {              // call/N is opaque to cut
                    Term goal = (a.size() == 1) ? a.get(0)
                        : addArgs(deref(a.get(0)), a.subList(1, a.size()));
                    goalStack = new Goal(goal, cps.size(), goalStack);
                    continue;
                }
                if (":".equals(f) && a.size() == 2) {                 // Module:Goal
                    Term inner = deref(a.get(1));
                    if (modules != null && (inner instanceof Atom || inner instanceof CompoundTerm)) {
                        // module-aware: find Goal's clauses in the named module, unify against Goal
                        if (!callUser(inner, t)) { if (!backtrack(floor)) return; }
                    } else {
                        goalStack = new Goal(inner, cps.size(), goalStack);   // no module system: just run Goal
                    }
                    continue;
                }
                if ("findall".equals(f) && a.size() == 3) {
                    Term list = makeList(findAll(a.get(0), a.get(1)));
                    if (!unify(a.get(2), list)) { if (!backtrack(floor)) return; }
                    continue;
                }
                if ("catch".equals(f) && a.size() == 3) {            // install a catch frame, then run Goal
                    cps.add(new CP(mark(), a.get(1), a.get(2), goalStack, g.cutBarrier));
                    goalStack = new Goal(a.get(0), cps.size(), goalStack);   // opaque to cut
                    continue;
                }
                if ("throw".equals(f) && a.size() == 1) {            // raised as a Java exception,
                    throw new it.denzosoft.jprolog.core.exceptions.PrologException(   // caught by drive()
                        rename(resolve(a.get(0)), renameCounter++, new HashMap<>()));
                }
                if (("assertz".equals(f) || "assert".equals(f)) && a.size() == 1) { assertClause(a.get(0), false); continue; }
                if ("asserta".equals(f) && a.size() == 1) { assertClause(a.get(0), true); continue; }
                if ("retract".equals(f) && a.size() == 1) {
                    if (!retractClause(a.get(0))) { if (!backtrack(floor)) return; }
                    continue;
                }
                int r = solveBuiltin(t, f, a);
                if (r == 1) continue;
                if (r == 0) { if (!backtrack(floor)) return; continue; }
                int rb = bridgeBuiltin(t, f, a.size());
                if (rb == 1) continue;
                if (rb == 0) { if (!backtrack(floor)) return; continue; }
                if (!callUser(t, t)) { if (!backtrack(floor)) return; }
                continue;
            }
            // variable / number in goal position -> not callable
            if (!backtrack(floor)) return;
          } catch (it.denzosoft.jprolog.core.exceptions.PrologException e) {
            // route the ball to the nearest catch frame within this run's floor; if none, re-throw
            // so an enclosing drive (lower floor) — e.g. a catch/3 around findall/3 — can handle it.
            Term ball = e.getErrorTerm();
            if (ball == null) throw e;
            if (!handleBall(rename(ball, renameCounter++, new HashMap<>()), floor)) throw e;
          }
        }
    }

    // ----------------------------------------------------------------- control
    private void cut(int barrier) {
        while (cps.size() > barrier) cps.remove(cps.size() - 1);
    }

    private void disjunction(Term left, Term right, int cutBarrier) {
        final Goal cont = goalStack;
        List<Alt> alts = Arrays.asList(
            () -> new Goal(left, cutBarrier, cont),
            () -> new Goal(right, cutBarrier, cont));
        CP cp = new CP(alts, mark());
        cps.add(cp);
        advance(cp);                                                   // first branch always installs
    }

    private static final Atom CUT = new Atom("!");

    /** (Cond -> Then ; Else): commit to Cond's first solution, then Then; else Else. */
    private void ite(Term cond, Term then, Term els, int cutBarrier) {
        final Goal cont = goalStack;
        final int barrier = cps.size();                               // cut target = this ITE choice point
        final Goal alt1 = new Goal(cond, barrier, new Goal(CUT, barrier, new Goal(then, cutBarrier, cont)));
        final Goal alt2 = new Goal(els, cutBarrier, cont);
        List<Alt> alts = Arrays.asList(() -> alt1, () -> alt2);
        CP cp = new CP(alts, mark());
        cps.add(cp);
        advance(cp);
    }

    /** (Cond *-> Then ; Else): if Cond has any solution, run Then for EACH (no commit); else Else. */
    private void softCut(Term cond, Term then, Term els, int cutBarrier) {
        final Goal cont = goalStack;
        final boolean[] found = {false};
        final Goal alt1 = new Goal(cond, cps.size(), new Goal(() -> found[0] = true, new Goal(then, cutBarrier, cont)));
        List<Alt> alts = Arrays.asList(
            () -> alt1,
            () -> found[0] ? FAILED : new Goal(els, cutBarrier, cont));
        CP cp = new CP(alts, mark());
        cps.add(cp);
        advance(cp);
    }

    /** Deterministic builtins: 1 = succeeded, 0 = failed, -1 = not a builtin (try user clauses). */
    private int solveBuiltin(Term t, String f, List<Term> a) {
        int n = a.size();
        if (n == 2) {
            switch (f) {
                case "is":   return unify(a.get(0), evalNum(a.get(1))) ? 1 : 0;
                case "<": case ">": case "=<": case ">=": case "=:=": case "=\\=":
                    return numRel(f, evalNum(a.get(0)), evalNum(a.get(1))) ? 1 : 0;
                case "==":   return structuralEqual(resolve(a.get(0)), resolve(a.get(1))) ? 1 : 0;
                case "\\==": return structuralEqual(resolve(a.get(0)), resolve(a.get(1))) ? 0 : 1;
                case "\\=": { int m = mark(); boolean u = unify(a.get(0), a.get(1)); undo(m); return u ? 0 : 1; }
                default: return -1;
            }
        }
        if (n == 1) {
            Term x = deref(a.get(0));
            switch (f) {
                case "var":      return x instanceof Variable ? 1 : 0;
                case "nonvar":   return x instanceof Variable ? 0 : 1;
                case "atom":     return x instanceof Atom ? 1 : 0;
                case "atomic":   return (x instanceof Atom || x instanceof Number) ? 1 : 0;
                case "number":   return x instanceof Number ? 1 : 0;
                case "integer":  return (x instanceof Number && ((Number) x).isInteger()) ? 1 : 0;
                case "float":    return (x instanceof Number && !((Number) x).isInteger()) ? 1 : 0;
                case "compound": return x instanceof CompoundTerm ? 1 : 0;
                case "callable": return (x instanceof Atom || x instanceof CompoundTerm) ? 1 : 0;
                default: return -1;
            }
        }
        return -1;
    }

    private Number evalNum(Term t) {
        return it.denzosoft.jprolog.core.arith.v2.ArithEvaluator.eval(resolve(t), new HashMap<>());
    }

    /** ISO arithmetic comparison with IEEE float semantics (-0.0 =:= 0.0, NaN =\= NaN). Integers
     *  compare exactly via BigInteger; otherwise primitive double comparison. */
    private boolean numRel(String op, Number a, Number b) {
        if (a.isInteger() && b.isInteger()) {
            int c = a.bigIntegerValue().compareTo(b.bigIntegerValue());
            switch (op) {
                case "<": return c < 0;  case ">": return c > 0;  case "=<": return c <= 0;
                case ">=": return c >= 0; case "=:=": return c == 0; case "=\\=": return c != 0;
            }
        }
        double x = a.doubleValue(), y = b.doubleValue();
        switch (op) {
            case "<": return x < y;  case ">": return x > y;  case "=<": return x <= y;
            case ">=": return x >= y; case "=:=": return x == y; case "=\\=": return x != y;
        }
        return false;
    }

    private boolean structuralEqual(Term a, Term b) {
        if (a instanceof Variable && b instanceof Variable) return ((Variable) a).getName().equals(((Variable) b).getName());
        if (a instanceof Atom && b instanceof Atom) return ((Atom) a).getName().equals(((Atom) b).getName());
        if (a instanceof Number && b instanceof Number) return a.equals(b);
        if (a instanceof CompoundTerm && b instanceof CompoundTerm) {
            CompoundTerm ca = (CompoundTerm) a, cb = (CompoundTerm) b;
            if (!ca.getName().equals(cb.getName()) || ca.getArguments().size() != cb.getArguments().size()) return false;
            for (int i = 0; i < ca.getArguments().size(); i++) {
                if (!structuralEqual(ca.getArguments().get(i), cb.getArguments().get(i))) return false;
            }
            return true;
        }
        return false;
    }

    /**
     * Delegate a non-native goal to the existing {@link BuiltInRegistry} (reusing the 200+ builtin
     * implementations). Deterministic builtins yield one solution; nondeterministic ones (e.g.
     * {@code between/3}) yield several → a choice point. Returns 1 success / 0 fail / -1 not-a-builtin.
     * Context-dependent builtins (findall/catch/…) throw without a solver and fall through (-1) for
     * now — they will be handled natively by the machine in a later step.
     */
    private int bridgeBuiltin(Term goal, String functor, int arity) {
        if (registry == null || !registry.isBuiltIn(functor, arity)) return -1;
        BuiltIn b = registry.getBuiltIn(functor);
        if (b == null) return -1;
        // Pass the goal UNRESOLVED (with the bindings map) rather than a deep copy: built-ins resolve
        // their own arguments via resolveBindings, which preserves shared term objects — so destructive
        // built-ins (setarg/3, nb_setarg) mutate the actual bound term, not a copy (ISS-2025-0317).
        Map<String, Term> inMap = new HashMap<>(binding);
        List<Map<String, Term>> sols = new ArrayList<>();
        boolean ok;
        try {
            // BuiltInWithContext builtins (findall-adapter, setup_call_cleanup, predsort, format, ...)
            // need a solver to run their sub-goals; hand them the engine's solver (ISS-2025-0312).
            if (b instanceof BuiltInWithContext && contextSolver != null) {
                ok = ((BuiltInWithContext) b).executeWithContext(contextSolver, goal, inMap, sols);
            } else {
                ok = b.execute(goal, inMap, sols);
            }
        } catch (it.denzosoft.jprolog.core.exceptions.PrologException pe) {
            throw pe;    // ISS-2025-0309: a real ISO error must reach catch/3, not be swallowed
        } catch (RuntimeException e) {
            return -1;   // needs solver context / not bridgeable yet -> let the caller try user clauses
        }
        if (!ok || sols.isEmpty()) return 0;
        final Goal cont = goalStack;
        List<Alt> alts = new ArrayList<>(sols.size());
        for (Map<String, Term> sol : sols) {
            final Map<String, Term> fsol = sol;
            alts.add(() -> { applySolution(fsol); return cont; });
        }
        CP cp = new CP(alts, mark());
        cps.add(cp);
        if (advance(cp)) return 1;
        cps.remove(cps.size() - 1);
        return 0;
    }

    /** findall/3: collect a (renamed-apart) copy of Template for every solution of Goal. */
    private List<Term> findAll(Term template, Term goal) {
        Goal savedGoals = goalStack;
        int floor = cps.size();
        int m = mark();
        List<Term> results = new ArrayList<>();
        goalStack = new Goal(goal, floor, null);
        try {
            drive(() -> { results.add(rename(resolve(template), renameCounter++, new HashMap<>())); return true; }, floor);
        } finally {
            // restore even if a ball unwinds through the nested drive (ISS-2025-0308)
            undo(m);                                               // findall is opaque: discard Goal's bindings
            goalStack = savedGoals;
        }
        return results;
    }

    /** Handle a thrown ball: unwind choice points down to {@code floor} looking for a catch frame
     *  whose catcher unifies with the ball; if found, install its recovery and return true. If none
     *  is found within this run's floor, return false — drive() re-throws so an OUTER drive (lower
     *  floor) gets a chance, and only an uncaught ball at floor 0 escapes as a real exception. This
     *  is what makes throw/1 transparent across findall/3's nested drive (ISS-2025-0308). */
    private boolean handleBall(Term ball, int floor) {
        while (cps.size() > floor) {
            CP top = cps.remove(cps.size() - 1);
            if (top.isCatch) {
                undo(top.trailMark);
                it.denzosoft.jprolog.core.engine.Trail.rollbackTo(top.legacyMark);   // ISS-0316
                int m = mark();
                if (unify(top.catcher, ball)) {
                    goalStack = new Goal(top.recovery, top.cutBarrier, top.cont);
                    return true;
                }
                undo(m);                                          // catcher didn't match; keep unwinding
            }
        }
        return false;
    }

    private static Term makeList(List<Term> elems) {
        Term list = new Atom("[]");
        for (int i = elems.size() - 1; i >= 0; i--) {
            list = new CompoundTerm(new Atom("."), Arrays.asList(elems.get(i), list));
        }
        return list;
    }

    // ----------------------------------------------------------------- database (assert/retract)
    /** assert a (copied) clause at the front (asserta) or back (assertz) of its predicate. */
    private void assertClause(Term clause, boolean front) {
        Rule r = toRule(rename(resolve(clause), renameCounter++, new HashMap<>()));   // copy_term
        if (liveKb != null) {
            if (front) liveKb.asserta(r); else liveKb.addRule(r);
        } else {
            List<Rule> list = kb.computeIfAbsent(key(r.getHead()), k -> new ArrayList<>());
            if (front) list.add(0, r); else list.add(r);
        }
    }

    /** retract the first clause that unifies with {@code clause}; first-match (semi-det). Both the
     *  query and each stored clause are normalised to (Head :- Body) form, so a fact retracts via
     *  either {@code retract(Head)} or {@code retract((Head :- true))} (ISS-2025-0310). */
    private boolean retractClause(Term clause) {
        Term q = deref(clause);
        Term head;
        Term queryClause;
        if (q instanceof CompoundTerm && ":-".equals(((CompoundTerm) q).getName())
                && ((CompoundTerm) q).getArguments().size() == 2) {
            head = ((CompoundTerm) q).getArguments().get(0);
            queryClause = q;
        } else {
            head = q;
            queryClause = new CompoundTerm(new Atom(":-"), Arrays.asList(q, new Atom("true")));
        }
        List<Rule> list = (liveKb != null) ? clausesFor(deref(head)) : kb.get(key(deref(head)));
        if (list == null) return false;
        for (int i = 0; i < list.size(); i++) {
            Rule original = list.get(i);
            int m = mark();
            Term stored = makeClauseTerm(renameRule(original));     // always (Head :- Body); facts -> (Head :- true)
            if (unify(stored, queryClause)) {
                if (liveKb != null) liveKb.retract(original); else list.remove(i);
                return true;
            }
            undo(m);
        }
        return false;
    }

    private Rule toRule(Term c) {
        if (c instanceof CompoundTerm && ":-".equals(((CompoundTerm) c).getName())
                && ((CompoundTerm) c).getArguments().size() == 2) {
            CompoundTerm cc = (CompoundTerm) c;
            return new Rule(cc.getArguments().get(0), flattenBody(cc.getArguments().get(1)));
        }
        return new Rule(c, new ArrayList<>());
    }

    private List<Term> flattenBody(Term body) {
        List<Term> gs = new ArrayList<>();
        Term cur = body;
        while (cur instanceof CompoundTerm && ",".equals(((CompoundTerm) cur).getName())
                && ((CompoundTerm) cur).getArguments().size() == 2) {
            gs.add(((CompoundTerm) cur).getArguments().get(0));
            cur = ((CompoundTerm) cur).getArguments().get(1);
        }
        gs.add(cur);
        return gs;
    }

    private Term makeClauseTerm(Rule r) {
        List<Term> gs = r.getBody();
        Term body = new Atom("true");
        if (!gs.isEmpty()) {
            body = gs.get(gs.size() - 1);
            for (int i = gs.size() - 2; i >= 0; i--) body = new CompoundTerm(new Atom(","), Arrays.asList(gs.get(i), body));
        }
        return new CompoundTerm(new Atom(":-"), Arrays.asList(r.getHead(), body));
    }

    /** Install a builtin solution map: bind every variable the builtin introduced (on the trail). */
    private void applySolution(Map<String, Term> sol) {
        for (Map.Entry<String, Term> e : sol.entrySet()) {
            if (!binding.containsKey(e.getKey())) bind(e.getKey(), e.getValue());
        }
    }

    private Term addArgs(Term goal, List<Term> extra) {
        if (goal instanceof Atom) return new CompoundTerm((Atom) goal, new ArrayList<>(extra));
        if (goal instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) goal;
            List<Term> args = new ArrayList<>(c.getArguments());
            args.addAll(extra);
            return new CompoundTerm(new Atom(c.getName()), args);
        }
        return goal;
    }

    /** Resolve and run a user predicate. {@code lookup} is used to find clauses (module-aware: it may
     *  be a {@code Module:Goal} term); {@code unifyGoal} is the (unqualified) goal each clause head
     *  unifies with. For ordinary calls the two are identical. */
    private boolean callUser(Term unifyGoal, Term lookup) {
        // ISS-2025-0319: tabled predicates (:- table p/n) need SLG resolution (memoization + loop
        // detection). Delegate the whole call to the legacy solver, which implements tabling, and
        // surface its solutions as a choice point — the v2 iterative SLD has no tabling.
        if (tableStore != null && contextSolver != null && isTabled(deref(unifyGoal))) {
            return tabledDelegate(deref(unifyGoal));
        }
        // ISS-2025-0315: feed the profiler (zero overhead when disabled), like the legacy QuerySolver
        if (it.denzosoft.jprolog.core.engine.Profiler.isEnabled()) {
            Term gg = deref(unifyGoal);
            if (gg instanceof Atom) it.denzosoft.jprolog.core.engine.Profiler.recordCall(((Atom) gg).getName(), 0);
            else if (gg instanceof CompoundTerm) it.denzosoft.jprolog.core.engine.Profiler.recordCall(
                ((CompoundTerm) gg).getName(), ((CompoundTerm) gg).getArguments().size());
        }
        List<Rule> rules = clausesFor(lookup);
        if (rules == null || rules.isEmpty()) return false;
        final Goal cont = goalStack;
        final int barrier = cps.size();                               // this CP's index = cut target for the body
        final Term g = unifyGoal;
        List<Alt> alts = new ArrayList<>(rules.size());
        for (Rule rule : rules) {
            final Rule fr = rule;
            alts.add(() -> {
                Rule r = renameRule(fr);
                if (!unify(r.getHead(), g)) return FAILED;
                return pushBody(r.getBody(), barrier, cont);
            });
        }
        CP cp = new CP(alts, mark());
        cps.add(cp);
        if (advance(cp)) return true;
        cps.remove(cps.size() - 1);
        return false;
    }

    private boolean isTabled(Term goal) {
        if (goal instanceof Atom) return tableStore.isTabled(((Atom) goal).getName(), 0);
        if (goal instanceof CompoundTerm) return tableStore.isTabled(((CompoundTerm) goal).getName(), ((CompoundTerm) goal).getArguments().size());
        return false;
    }

    /** Run a tabled call through the legacy solver (SLG) and expose its solutions as a choice point.
     *  Uses the top-level {@code solve(Term)}, which establishes the tabling context (loop detection +
     *  memoization) the recursive solve does not. */
    private boolean tabledDelegate(Term goal) {
        List<Map<String, Term>> sols;
        try {
            sols = contextSolver.solve(resolve(goal));   // ground the known args; sets up tabling
        } catch (RuntimeException e) {
            return false;
        }
        if (sols.isEmpty()) return false;
        final Goal cont = goalStack;
        List<Alt> alts = new ArrayList<>(sols.size());
        for (Map<String, Term> sol : sols) {
            final Map<String, Term> fsol = sol;
            alts.add(() -> { applySolution(fsol); return cont; });
        }
        CP cp = new CP(alts, mark());
        cps.add(cp);
        if (advance(cp)) return true;
        cps.remove(cps.size() - 1);
        return false;
    }

    /** Try the next alternative of {@code cp}, undoing the trail first; sets {@link #goalStack}. */
    private boolean advance(CP cp) {
        while (cp.idx < cp.alts.size()) {
            undo(cp.trailMark);
            it.denzosoft.jprolog.core.engine.Trail.rollbackTo(cp.legacyMark);   // ISS-0316: undo b_setval etc.
            Goal gs = cp.alts.get(cp.idx++).apply();
            if (gs != FAILED) { goalStack = gs; return true; }
        }
        return false;
    }

    private boolean backtrack(int floor) {
        while (cps.size() > floor) {
            CP cp = cps.get(cps.size() - 1);
            if (cp.isCatch) { cps.remove(cps.size() - 1); continue; }  // a catch frame has no alternatives
            if (advance(cp)) return true;
            cps.remove(cps.size() - 1);
        }
        return false;
    }

    private static Goal pushBody(List<Term> body, int barrier, Goal cont) {
        Goal gs = cont;
        for (int i = body.size() - 1; i >= 0; i--) gs = new Goal(body.get(i), barrier, gs);
        return gs;
    }

    // ----------------------------------------------------------------- variable renaming
    private Rule renameRule(Rule rule) {
        int id = renameCounter++;
        Map<String, Variable> map = new HashMap<>();
        Term head = rename(rule.getHead(), id, map);
        List<Term> body = new ArrayList<>();
        for (Term b : rule.getBody()) body.add(rename(b, id, map));
        return new Rule(head, body);
    }

    private Term rename(Term t, int id, Map<String, Variable> map) {
        if (t instanceof Variable) {
            String name = ((Variable) t).getName();
            return map.computeIfAbsent(name, nm -> new Variable("_R" + id + "_" + nm));
        }
        if (t instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) t;
            List<Term> args = new ArrayList<>(c.getArguments().size());
            for (Term a : c.getArguments()) args.add(rename(a, id, map));
            return new CompoundTerm(new Atom(c.getName()), args);
        }
        return t;
    }

    // ----------------------------------------------------------------- result extraction
    private void collectVars(Term root, List<String> out) {
        java.util.ArrayDeque<Term> stack = new java.util.ArrayDeque<>();
        stack.push(root);
        while (!stack.isEmpty()) {
            Term t = stack.pop();
            if (t instanceof Variable) {
                String n = ((Variable) t).getName();
                if (!out.contains(n)) out.add(n);
            } else if (t instanceof CompoundTerm) {
                for (Term a : ((CompoundTerm) t).getArguments()) stack.push(a);
            }
        }
    }

    private Map<String, Term> snapshot(List<String> vars) {
        Map<String, Term> m = new HashMap<>();
        for (String v : vars) m.put(v, resolve(new Variable(v)));
        return m;
    }

    private Term resolve(Term t) { return resolve(t, new java.util.HashSet<>()); }

    /** Fully dereference {@code t}, detecting cyclic terms (e.g. X = f(X) with occurs_check off) so a
     *  rational tree raises a controlled representation_error instead of a {@link StackOverflowError}
     *  (ISS-2025-0313). {@code active} holds the variable names on the current resolution path. */
    private Term resolve(Term t, java.util.Set<String> active) {
        if (t instanceof Variable) {
            String n = ((Variable) t).getName();
            Term b = binding.get(n);
            if (b == null) return t;                                  // unbound
            if (!active.add(n)) {                                     // already on the path -> cycle
                throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                    it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.representationError("cyclic_term", "resolve"));
            }
            Term r = resolve(b, active);
            active.remove(n);
            return r;
        }
        if (t instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) t;
            List<Term> args = new ArrayList<>(c.getArguments().size());
            for (Term a : c.getArguments()) args.add(resolve(a, active));
            return new CompoundTerm(new Atom(c.getName()), args);
        }
        return t;
    }
}
