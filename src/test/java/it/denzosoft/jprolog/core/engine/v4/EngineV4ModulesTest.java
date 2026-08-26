package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

// START_CHANGE: ISS-2025-0466..0471 - engine v4 wave W6 (modules and the Prolog prelude, B.10).
/**
 * Acceptance tests for wave W6 of the v4 engine: the {@link Modules} owner, {@code Module:Goal},
 * {@code meta_predicate/1}, the autoloaded {@code prelude/*.pl} library modules and the cell-based
 * {@code label/1}.
 *
 * <p>Every test here fails on the pre-W6 engine: {@code lists:append([1],[2],L)} was false,
 * {@code apply:partition/4} did not exist, two modules exporting the same name resolved to
 * whichever one the {@code ModuleManager} happened to return, a meta-argument lost its caller's
 * module, {@code append(X,Y,Z)} gave one solution instead of enumerating, and
 * {@code current_module/1} raised {@code existence_error}.
 *
 * <p>Like the other {@code EngineV4*Test} classes, every test selects v4 in {@link #setUp} and
 * restores the previous selection in {@link #tearDown}, so the class behaves identically under the
 * default profile and under {@code -Pengine-v4}.
 */
public class EngineV4ModulesTest {

    private Prolog prolog;
    private boolean prevV4;
    private boolean prevV2;

    @Before
    public void setUp() {
        prevV4 = Prolog.isUsingV4Engine();
        prevV2 = Prolog.isUsingV2Engine();
        Prolog.setUseV4Engine(true);
        prolog = new Prolog();
    }

    @After
    public void tearDown() {
        Prolog.setUseV4Engine(prevV4);
        Prolog.setUseV2Engine(prevV2);
    }

    private void ok(String query) {
        assertFalse("expected a solution for: " + query, prolog.solve(query).isEmpty());
    }

    private void no(String query) {
        assertTrue("expected no solution for: " + query, prolog.solve(query).isEmpty());
    }

    private int count(String query) { return prolog.solve(query).size(); }

    /** Every solution's NAMED bindings, in a stable order — fresh variables print with a serial
     *  that differs between two runs of the same query, so they cannot be compared literally. */
    private List<String> named(String query) {
        List<String> out = new ArrayList<String>();
        for (Map<String, Term> sol : prolog.solve(query + ".")) {
            java.util.TreeMap<String, String> m = new java.util.TreeMap<String, String>();
            for (Map.Entry<String, Term> e : sol.entrySet()) {
                if (e.getKey().startsWith("_")) continue;
                m.put(e.getKey(), e.getValue().toString().replaceAll("_G[0-9]+", "_"));
            }
            out.add(m.toString());
        }
        return out;
    }

    // ==================================================================
    // ISS-2025-0466 — the Modules owner and Module:Goal
    // ==================================================================

    @Test
    public void testISS0466_LibraryQualifiedBuiltinWorks() {
        // The headline row of scratchpad/bench/Limits.java: false on every engine before W6.
        ok("lists:append([1], [2], L), L == [1,2]");
        ok("lists:member(b, [a,b,c])");
        ok("lists:length([a,b], 2)");             // exported by `lists`, implemented as a native
        ok("lists:reverse([1,2,3], [3,2,1])");
    }

    @Test
    public void testISS0466_SystemAndUserQualification() {
        ok("system:atom_length(abc, 3)");
        ok("system:(X is 1 + 2), X == 3");
        prolog.consult("foo(7).\n");
        ok("user:foo(7)");
        ok("user:atom_length(abc, 3)");           // `user` sees the built-ins too
    }

    @Test
    public void testISS0466_NestedQualificationResolvesToTheInnermostModule() {
        prolog.consult("bar(1).\n");
        ok("user:user:bar(1)");
        ok("lists:user:bar(1)");                  // innermost wins: `user`, not `lists`
        ok("user:lists:append([1], [], [1])");    // innermost wins: `lists`
    }

    @Test
    public void testISS0466_ExportEnforcementOnQualifiedCalls() {
        // ISS-2025-0314 behaviour, preserved: a non-exported predicate is invisible from outside.
        prolog.consult(":- module(secret_w6, [pub/1]).\npub(1).\npriv(2).\n:- module(user, []).\n");
        ok("secret_w6:pub(1)");
        no("secret_w6:priv(2)");
    }

    @Test
    public void testISS0466_UnqualifiedResolutionOrder() {
        // M -> M's imports -> user -> autoload. `shared/1` is defined in user AND in the module;
        // the module's own definition must win for the module, user's for user.
        prolog.consult("shared(from_user).\n"
                     + ":- module(mw6, [go/1]).\nshared(from_module).\ngo(X) :- shared(X).\n"
                     + ":- module(user, []).\n");
        List<Map<String, Term>> r = prolog.solve("mw6:go(X).");
        assertEquals(1, r.size());
        assertEquals("from_module", r.get(0).get("X").toString());
        r = prolog.solve("shared(X).");
        assertEquals(1, r.size());
        assertEquals("from_user", r.get(0).get("X").toString());
    }

    @Test
    public void testISS0466_UserIsTheFlatStoreEvenWithSeveralModules() {
        // Before W6 the machine diverted EVERY unqualified call through ModuleManager the moment a
        // second module existed (the `modules.size() > 1` special case). A user predicate asserted
        // at run time then became unreachable from a module context.
        prolog.consult("base(1).\n:- module(other_w6, []).\n:- module(user, []).\n");
        ok("assertz(base(2)), base(2)");
        assertEquals(2, count("base(_X)."));
        assertEquals(2, count("other_w6:base(_X)."));   // `other_w6` sees `user` too
    }

    @Test
    public void testISS0466_TwoModulesSameNameDifferentClients() {
        prolog.consult(
            ":- module(m1w6, [p/1]).\np(one).\n"
          + ":- module(m2w6, [p/1]).\np(two).\n"
          + ":- module(c1w6, [t1/1]).\n:- use_module(m1w6).\nt1(X) :- p(X).\n"
          + ":- module(c2w6, [t2/1]).\n:- use_module(m2w6).\nt2(X) :- p(X).\n"
          + ":- module(user, []).\n");
        List<Map<String, Term>> a = prolog.solve("c1w6:t1(X).");
        List<Map<String, Term>> b = prolog.solve("c2w6:t2(X).");
        assertEquals(1, a.size());
        assertEquals(1, b.size());
        assertEquals("one", a.get(0).get("X").toString());
        assertEquals("two", b.get(0).get("X").toString());
    }

    @Test
    public void testISS0466_UnknownProcedureStillRaisesWithModulesPresent() {
        // The `size() > 1 -> never raise` escape hatch of raiseUnknownIfRequired is gone.
        prolog.consult(":- module(anymod_w6, []).\n:- module(user, []).\n");
        try {
            prolog.solve("no_such_predicate_w6(_X).");
            fail("expected existence_error");
        } catch (RuntimeException e) {
            assertTrue(String.valueOf(e.getMessage()), String.valueOf(e.getMessage()).contains("existence_error"));
        }
    }

    // ==================================================================
    // ISS-2025-0467 — autoload by predicate indicator
    // ==================================================================

    @Test
    public void testISS0467_LibrariesAreAutoloadedNotEagerlyLoaded() {
        Engine e = prolog.getV4Engine();
        assertNotNull(e);
        Modules ms = e.modules4();
        assertFalse("no library may be parsed before it is referenced", ms.isLoaded("lists"));
        assertFalse(ms.isLoaded("apply"));
        assertFalse(ms.isLoaded("pairs"));
        prolog.consult("twice(X, Y) :- Y is X * 2.\n");
        ok("maplist(twice, [1,2], [2,4])");
        assertTrue("maplist/3 must have pulled in library(apply)", ms.isLoaded("apply"));
        assertFalse("and nothing else", ms.isLoaded("pairs"));
        assertFalse(ms.isLoaded("lists"));
        ok("pairs_keys_values([a-1], [a], [1])");
        assertTrue(ms.isLoaded("pairs"));
        ok("lists:append([1], [2], [1,2])");
        assertTrue(ms.isLoaded("lists"));
    }

    @Test
    public void testISS0467_PreludeHeadersMatchTheClauses() {
        // The autoload index is built from the `:- module(Name, [...])` headers by a textual scan,
        // so it must stay in step with what the files actually define.
        for (String resource : Prelude.resources()) {
            Prelude.Parsed parsed = Prelude.parse(resource);
            assertNotNull(resource, parsed);
            Prelude.Lib lib = null;
            for (Prelude.Lib l : Prelude.libraries()) if (l.resource.equals(resource)) lib = l;
            assertNotNull("no module header in " + resource, lib);
            Set<String> defined = new TreeSet<String>(parsed.byIndicator.keySet());
            for (String pi : defined) {
                if (pi.startsWith("$")) continue;              // private helper: must NOT be exported
                assertTrue(resource + " defines " + pi + " but does not export it",
                    lib.exports.contains(pi));
            }
            for (String pi : lib.exports) {
                if (defined.contains(pi)) continue;
                // An export with no clauses is a natively implemented library predicate.
                int slash = pi.lastIndexOf('/');
                String f = pi.substring(0, slash);
                int n = Integer.parseInt(pi.substring(slash + 1));
                boolean known = prolog.getV4Engine().natives().isNative(f, n)
                             || prolog.getBuiltInRegistry().isBuiltIn(f, n);
                assertTrue(resource + " exports " + pi + " but nothing implements it", known);
            }
            for (String pi : defined) {
                if (!pi.startsWith("$")) continue;
                // The one engine entry point that must be visible from `user`: the wake queue
                // pushes '$attr_hook'/4 from whatever context made the binding.
                if ("$attr_hook/4".equals(pi)) continue;
                assertFalse(resource + " must not export the private helper " + pi,
                    lib.exports.contains(pi));
            }
        }
    }

    @Test
    public void testISS0467_LibraryModulesDoNotClaimTheSameIndicator() {
        // `Prelude.owner` keeps the FIRST module that exports an indicator; an accidental overlap
        // (lists exporting exclude/3 while apply implements it) silently disables the real one.
        java.util.Map<String, String> seen = new java.util.HashMap<String, String>();
        for (Prelude.Lib lib : Prelude.libraries()) {
            for (String pi : lib.exports) {
                String prev = seen.put(pi, lib.module);
                if (prev != null) fail(pi + " is exported by both " + prev + " and " + lib.module);
            }
        }
    }

    // ==================================================================
    // ISS-2025-0468 — the prelude payoff (the two W3 deviations)
    // ==================================================================

    @Test
    public void testISS0468_AppendEnumeratesInEveryMode() {
        assertEquals(3, count("append(X, Y, [1,2])."));
        ok("append([1,2], [3], [1,2,3])");
        ok("append([1,2], Y, Z), Y = [], Z == [1,2]");
        // fully open: the first solution is the standard one, and it does not stop there
        ok("append(X, _Y, _Z), X == [], !");
        assertEquals(1, count("append(X, _Y, _Z), length(X, 2), !."));
        assertEquals(1, count("append(X, _Y, _Z), length(X, 7), !."));
    }

    @Test
    public void testISS0468_MemberExtendsAPartialList() {
        // the Limits.java row: false before W6, because the eager built-in stopped at the open tail
        ok("once((member(X, L), L = [a|_])), X == a");
        ok("once((L = [1|T], member(2, L))), T = [H|_], H == 2");
        assertEquals(3, count("member(_X, [a,b,c])."));
        no("member(z, [a,b,c])");
    }

    @Test
    public void testISS0468_NativeAndPreludeListPredicatesAgree() {
        // member/2 and append/3 exist twice on v4: as the two-clause definitions of module `lists`
        // (which `lists:...` runs, and which a user could override) and as the native generators
        // the unqualified call takes for speed. They must be observationally identical.
        String[][] pairs = {
            { "append([1,2], [3], R)",        "lists:append([1,2], [3], R)" },
            { "append(X, Y, [1,2,3])",        "lists:append(X, Y, [1,2,3])" },
            { "append([1|T], [9], [1,2,9])",  "lists:append([1|T], [9], [1,2,9])" },
            { "member(X, [a,b,c])",           "lists:member(X, [a,b,c])" },
            { "member(b, [a,b,c])",           "lists:member(b, [a,b,c])" },
            { "member(z, [a,b,c])",           "lists:member(z, [a,b,c])" },
        };
        for (String[] pair : pairs) {
            assertEquals(pair[0] + " vs " + pair[1], named(pair[0]), named(pair[1]));
        }
        // the open modes agree too, taken with a cut because both are infinite generators
        assertEquals(named("append(X, _, _), length(X, 2), !"),
                     named("lists:append(X, _, _), length(X, 2), !"));
        assertEquals(named("once((member(X, L), L = [a|_]))"),
                     named("once((lists:member(X, L), L = [a|_]))"));
    }

    @Test
    public void testISS0468_MemberchkExtendsAPartialList() {
        ok("memberchk(a, L), L = [a|_]");
        ok("memberchk(b, [a,b,c])");
        no("memberchk(z, [a,b,c])");
    }

    @Test
    public void testISS0468_TheOtherListPredicatesKeepTheirModes() {
        assertEquals(3, count("select(_X, [a,b,c], _R)."));
        assertEquals(3, count("select(x, _L, [1,2])."));
        ok("nth0(1, [a,b,c], b)");
        ok("nth1(1, [a,b,c], a)");
        assertEquals(3, count("nth0(_I, [a,b,c], _E)."));
        ok("last([1,2,3], 3)");
        no("last([a|b], _X)");
        ok("reverse([1,2,3], [3,2,1])");
        ok("reverse(L, [1,2]), L == [2,1]");
    }

    @Test
    public void testISS0468_UserDefinitionOverridesTheLibraryOnlyForItsOwnContext() {
        prolog.consult("partition(a, b, c, d).\n");
        ok("partition(a, b, c, d)");                              // the user's definition
        ok("apply:partition([X]>>(X > 2), [1,2,3,4], I, E), I == [3,4], E == [1,2]");
        // a definition inside a module overrides only for that module
        prolog.consult(":- module(pm_w6, [go/1]).\npartition(mine).\ngo(X) :- partition(X).\n"
                     + ":- module(user, []).\n");
        List<Map<String, Term>> r = prolog.solve("pm_w6:go(X).");
        assertEquals(1, r.size());
        assertEquals("mine", r.get(0).get("X").toString());
        ok("partition(a, b, c, d)");                              // user's 4-arity one is untouched
    }

    // ==================================================================
    // ISS-2025-0469 — meta_predicate/1
    // ==================================================================

    @Test
    public void testISS0469_MetaArgumentRunsInTheCallersModule() {
        // Two modules with the SAME helper names, both calling the library maplist/3. Without the
        // meta_predicate declarations of prelude/apply.pl both would resolve mk/2 and helper/1 in
        // `apply` (and then in `user`), so one of the two answers would be wrong.
        prolog.consult(
            ":- module(mm1_w6, [go1/1]).\nhelper(m1).\ngo1(L) :- maplist(mk, [x], L).\nmk(_, R) :- helper(R).\n"
          + ":- module(mm2_w6, [go2/1]).\nhelper(m2).\ngo2(L) :- maplist(mk, [x], L).\nmk(_, R) :- helper(R).\n"
          + ":- module(user, []).\n");
        List<Map<String, Term>> a = prolog.solve("mm1_w6:go1(L).");
        List<Map<String, Term>> b = prolog.solve("mm2_w6:go2(L).");
        assertEquals(1, a.size());
        assertEquals(1, b.size());
        assertEquals("[m1]", a.get(0).get("L").toString());
        assertEquals("[m2]", b.get(0).get("L").toString());
    }

    @Test
    public void testISS0469_MetaArgumentsStillWorkFromUserAndWithLambdas() {
        prolog.consult("dbl(X, Y) :- Y is X * 2.\n");
        ok("maplist(dbl, [1,2,3], [2,4,6])");
        ok("maplist([X,Y]>>(Y is X * 2), [1,2], [2,4])");
        ok("foldl([X,A0,A]>>(A is A0 + X), [1,2,3,4], 0, 10)");
        ok("include([X]>>(X > 1), [1,2,3], [2,3])");
        ok("exclude([X]>>(X > 1), [1,2,3], [1])");
        ok("partition([X]>>(X > 1), [1,2,3], [2,3], [1])");
        // an explicitly qualified meta-argument is left exactly as written
        ok("maplist(user:dbl, [1,2], [2,4])");
    }

    @Test
    public void testISS0469_MetaCallInsideFindallAndForallKeepsTheContext() {
        prolog.consult(":- module(mf_w6, [all/1, every/0]).\nitem(1).\nitem(2).\n"
                     + "all(L) :- findall(X, item(X), L).\n"
                     + "every :- forall(item(X), integer(X)).\n"
                     + ":- module(user, []).\n");
        List<Map<String, Term>> r = prolog.solve("mf_w6:all(L).");
        assertEquals(1, r.size());
        assertEquals("[1, 2]", r.get(0).get("L").toString());
        ok("mf_w6:every");
    }

    // ==================================================================
    // ISS-2025-0470 — current_module/1 and predicate_property/2
    // ==================================================================

    @Test
    public void testISS0470_CurrentModule() {
        ok("current_module(user)");
        ok("current_module(system)");
        ok("current_module(lists)");
        no("current_module(no_such_module_w6)");
        List<Map<String, Term>> all = prolog.solve("current_module(M).");
        assertTrue("at least user, system and the four libraries", all.size() >= 6);
        Set<String> names = new TreeSet<String>();
        for (Map<String, Term> m : all) names.add(m.get("M").toString());
        assertTrue(names.contains("user"));
        assertTrue(names.contains("apply"));
        assertTrue(names.contains("coroutining"));
    }

    @Test
    public void testISS0470_PredicatePropertyModuleProperties() {
        ok("predicate_property(append(_, _, _), imported_from(lists))");
        ok("predicate_property(append(_, _, _), defined_in(lists))");
        ok("predicate_property(append(_, _, _), exported)");
        prolog.consult("mine_w6(1).\n");
        ok("predicate_property(mine_w6(_), defined_in(user))");
        // the registry's own properties still answer
        ok("predicate_property(atom_length(_, _), built_in)");
    }

    // ==================================================================
    // ISS-2025-0471 — label/1 and labeling/2 as cell-based generators
    // ==================================================================

    @Test
    public void testISS0471_LabelBindsFunctionallyDeterminedVariables() {
        // ISS-2025-0357, now without exportSingletons/1 and without the name -> cell hop
        List<Map<String, Term>> r = prolog.solve("C in 1..3, D #= C*2+1, label([C]).");
        assertEquals(3, r.size());
        assertEquals("1", r.get(0).get("C").toString());
        assertEquals("3", r.get(0).get("D").toString());
        assertEquals("3", r.get(2).get("C").toString());
        assertEquals("7", r.get(2).get("D").toString());
    }

    @Test
    public void testISS0471_LabelingOptionsAndErrors() {
        List<Map<String, Term>> up = prolog.solve("X in 1..5, labeling([max(X)], [X]).");
        assertEquals(5, up.size());
        assertEquals("5", up.get(0).get("X").toString());
        List<Map<String, Term>> down = prolog.solve("X in 1..5, labeling([min(X)], [X]).");
        assertEquals("1", down.get(0).get("X").toString());
        ok("catch(labeling([bogus_w6], [_X]), error(domain_error(labeling_option, bogus_w6), _), true)");
        ok("catch(label([a]), error(type_error(integer, a), _), true)");
        assertEquals(2, count("A in 1..4, B in 1..4, C in 1..6, A*A+B*B #= C*C, label([A,B,C])."));
    }

    @Test
    public void testISS0471_LabelIsANativeGeneratorNotTheRegistryBuiltIn() {
        assertTrue(prolog.getV4Engine().natives().isNative("label", 1));
        assertTrue(prolog.getV4Engine().natives().isNative("labeling", 2));
        // deterministic labeling leaves no choice point behind (the trust-me pop)
        ok("X in 3..3, label([X]), X == 3");
    }

    // ==================================================================
    // sandbox, budget and trace still hold with modules in play
    // ==================================================================

    @Test
    public void testISS0466_SafeModeStillStripsTheHostBuiltinsFromSystem() {
        Prolog safe = new Prolog();
        safe.enableSafeMode();
        try {
            safe.solve("system:shell('true', _S).");
            fail("safe mode must strip shell/2 from `system`");
        } catch (RuntimeException e) {
            assertTrue(String.valueOf(e.getMessage()), String.valueOf(e.getMessage()).contains("existence_error"));
        }
        assertFalse(safe.solve("system:atom_length(abc, 3).").isEmpty());
    }

    @Test
    public void testISS0466_InferenceBudgetStopsALibraryLoop() {
        prolog.setInferenceBudget(20000);
        try {
            prolog.solve("append(X, _Y, _Z), length(X, 100000), !.");
            fail("the budget must abort a runaway prelude enumeration");
        } catch (it.denzosoft.jprolog.core.engine.InferenceLimitException expected) {
            // the control exception must NOT be a PrologException the program could catch
            assertFalse(((Object) expected) instanceof it.denzosoft.jprolog.core.exceptions.PrologException);
        }
    }

    @Test
    public void testISS0466_QualifiedGoalsAreTracedWithTheirQualification() {
        java.io.ByteArrayOutputStream out = new java.io.ByteArrayOutputStream();
        java.io.PrintStream ps = new java.io.PrintStream(out);
        it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(ps);
        try {
            prolog.consult(":- module(tm_w6, [t/1]).\nt(1).\n:- module(user, []).\n");
            prolog.setTracing(true);
            prolog.solve("tm_w6:t(_X).");
        } finally {
            prolog.setTracing(false);
            it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(null);
        }
        String trace = out.toString();
        assertTrue("the trace must show the qualified goal: " + trace, trace.contains("tm_w6:t("));
        assertTrue(trace.contains("Call:"));
        assertTrue(trace.contains("Exit:"));
    }

    @Test
    public void testISS0466_ModulesAreIsolatedPerEngine() {
        prolog.consult(":- module(iso_w6, [only/0]).\nonly.\n:- module(user, []).\n");
        ok("current_module(iso_w6)");
        Prolog other = new Prolog();
        assertTrue(other.solve("current_module(iso_w6).").isEmpty());
        List<String> names = new ArrayList<String>();
        for (Map<String, Term> m : other.solve("current_module(M).")) names.add(m.get("M").toString());
        assertTrue(names.contains("user"));
        assertFalse(names.contains("iso_w6"));
    }
}
// END_CHANGE: ISS-2025-0466..0471
