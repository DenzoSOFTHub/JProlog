package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.builtin.io.StreamManager;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

// START_CHANGE: ISS-2025-0499 / ISS-2025-0501 - 4.1 wave B acceptance: the database, global
// variable, flag and halt built-ins on the v4 SPI, and the widened procedure protection.
/**
 * 4.1 wave B, step 4: {@code current_predicate/1}, {@code retractall/1}, {@code abolish/1},
 * {@code dynamic/1}, {@code listing/0,1}, the global variables, the ISO flags, {@code halt/0,1}
 * and {@code findall/4} are v4 natives — plus ISS-2025-0501, which restores
 * {@code permission_error(modify, static_procedure, PI)} for the seven coroutining predicates
 * that 4.1 wave A left unprotected.
 */
public class EngineV4DatabaseTest {

    private Prolog prolog;
    private ByteArrayOutputStream buffer;

    @Before
    public void setUp() {
        prolog = new Prolog();
        buffer = new ByteArrayOutputStream();
        StreamManager.setThreadLocalOutput(new PrintStream(buffer, true));
        prolog.consult(":- dynamic(foo/1).\nfoo(1).\nfoo(2).\nbar(a,b).\nbaz :- true.\n");
    }

    @After
    public void tearDown() {
        StreamManager.setThreadLocalOutput(null);
    }

    private String text(Term t) {
        return it.denzosoft.jprolog.core.util.TermFormatter.format(t, true, false, true, 1200);
    }

    private String all(String goal, String var) {
        List<Map<String, Term>> sols = prolog.solve(goal + ".");
        if (sols.isEmpty()) return "false";
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < sols.size(); i++) {
            if (i > 0) sb.append(';');
            sb.append(text(sols.get(i).get(var)));
        }
        return sb.toString();
    }

    private String yn(String goal) {
        return prolog.solve(goal + ".").isEmpty() ? "false" : "true";
    }

    private String err(String goal) {
        try {
            prolog.solve(goal + ".");
        } catch (PrologEvaluationException e) {
            return "eval:" + e.getMessage();
        } catch (PrologException e) {
            return text(e.getErrorTerm());
        }
        fail("expected an error from: " + goal);
        return null;
    }

    // ================================================================ registration

    @Test
    public void testISS0499_TheDatabaseFamilyIsNative() {
        BuiltinTable t = prolog.getV4Engine().natives();
        String[][] expected = {
            {"current_predicate", "1"}, {"retractall", "1"}, {"abolish", "1"}, {"dynamic", "1"},
            {"listing", "0"}, {"listing", "1"},
            {"nb_setval", "2"}, {"b_setval", "2"}, {"nb_current", "2"}, {"nb_delete", "1"},
            {"current_prolog_flag", "2"}, {"set_prolog_flag", "2"},
            {"halt", "0"}, {"halt", "1"}, {"findall", "4"},
        };
        for (String[] e : expected) {
            assertTrue(e[0] + "/" + e[1] + " must be a v4 native",
                t.isNative(e[0], Integer.parseInt(e[1])));
        }
    }

    // ================================================================ current_predicate/1

    @Test
    public void testISS0499_CurrentPredicateModesAndErrors() {
        assertEquals("true", yn("current_predicate(foo/1)"));
        assertEquals("false", yn("current_predicate(foo/2)"));
        assertEquals("1", all("current_predicate(foo/N)", "N"));
        assertEquals("foo", all("current_predicate(N/1)", "N"));
        assertEquals("error(type_error(predicate_indicator,3),'current_predicate/1')",
            err("current_predicate(3)"));
        assertEquals("error(type_error(predicate_indicator,a+b),'current_predicate/1')",
            err("current_predicate(a+b)"));
    }

    /** ISS-2025-0499: the enumeration is lazy — once/1 takes one answer, not a whole list. */
    @Test
    public void testISS0499_CurrentPredicateIsLazy() {
        List<Map<String, Term>> one = prolog.solve("once(current_predicate(_)).");
        assertEquals(1, one.size());
        assertEquals(3, prolog.solve("current_predicate(_).").size());
    }

    // ================================================================ retractall/1, abolish/1

    @Test
    public void testISS0499_RetractallAndAbolish() {
        assertEquals("[2]", all("retractall(foo(1)), findall(X, foo(X), L)", "L"));
        assertEquals("true", yn("retractall(nosuch(_))"));
        assertEquals("error(instantiation_error,'retractall/1: clause head must be instantiated')",
            err("retractall(X)"));
        assertEquals("error(type_error(callable,3),'retractall/1')", err("retractall(3)"));
        assertEquals("error(permission_error(modify,static_procedure,write/1),'retractall/1')",
            err("retractall(write(_))"));
        assertEquals("no", all("abolish(bar/2), (current_predicate(bar/2) -> R = yes ; R = no)", "R"));
        assertEquals("error(instantiation_error,'abolish/1: predicate indicator must be instantiated')",
            err("abolish(X)"));
        assertEquals("error(type_error(predicate_indicator,foo),'abolish/1: argument must be Functor/Arity')",
            err("abolish(foo)"));
        assertEquals("error(type_error(integer,a),'abolish/1: arity must be an integer')",
            err("abolish(foo/a)"));
        assertEquals("error(domain_error(not_less_than_zero,-1),'abolish/1: arity must be non-negative')",
            err("abolish(foo/(-1))"));
        assertEquals("error(permission_error(modify,static_procedure,write/1),'abolish/1')",
            err("abolish(write/1)"));
    }

    // ================================================================ dynamic/1

    @Test
    public void testISS0499_DynamicAcceptsEveryShape() {
        assertEquals("true", yn("dynamic(qux/1)"));
        assertEquals("true", yn("dynamic([q1/1, q2/2])"));
        assertEquals("true", yn("dynamic((q3/1, q4/2))"));
        assertEquals("error(instantiation_error,'dynamic/1')", err("dynamic(X)"));
        assertEquals("error(type_error(predicate_indicator,3),'dynamic/1')", err("dynamic(3)"));
    }

    // ================================================================ listing/0,1

    /**
     * ISS-2025-0499: {@code listing/1} never worked. {@code BuiltInFactory} binds one class per
     * NAME, and the name {@code listing} was bound to {@code Listing0}, which rejects any argument.
     */
    @Test
    public void testISS0499_ListingOneWorksAndIsCaptured() {
        buffer.reset();
        assertEquals("true", yn("listing(baz/0)"));
        String out = buffer.toString();
        // ISS-2025-0570 (P3.7): listing is portray_clause/1 — `baz.`, SWI's form, re-readable
        assertTrue("listing/1 must print the clause, got: " + out, out.contains("baz."));
        assertFalse("no Rule.toString() rendering, got: " + out, out.contains(":- true"));
        // START_CHANGE: ISS-2025-0508 - 4.3 wave D: a real instantiation_error.
        assertEquals("error(instantiation_error,'listing/1')", err("listing(X)"));
        // END_CHANGE: ISS-2025-0508
    }

    /** A bare name lists every arity, as the reference has always documented. */
    @Test
    public void testISS0499_ListingByNameCoversEveryArity() {
        buffer.reset();
        assertEquals("true", yn("listing(foo)"));
        String out = buffer.toString();
        assertTrue(out, out.contains("foo(1)."));
        assertTrue(out, out.contains("foo(2)."));
        assertFalse("a clause must not end in a doubled full stop", out.contains("foo(1).."));
        buffer.reset();
        assertEquals("true", yn("listing(nosuchpredicate)"));
        assertTrue(buffer.toString().contains("No clauses found for nosuchpredicate"));
    }

    /** listing/0 goes through StreamManager.out() (invariant 11), so with_output_to captures it. */
    @Test
    public void testISS0499_ListingZeroIsCaptured() {
        buffer.reset();
        assertEquals("true", yn("listing"));
        assertTrue(buffer.toString().contains("foo(1)"));
    }

    // ================================================================ the global variables

    @Test
    public void testISS0499_GlobalVariables() {
        assertEquals("42", all("nb_setval(k, 42), nb_getval(k, V)", "V"));
        assertEquals("1", all("nb_setval(k, 1), nb_current(k, V)", "V"));
        assertEquals("error(instantiation_error,'nb_setval/2')", err("nb_setval(X, 1)"));
        assertEquals("error(type_error(atom,3),'nb_setval/2')", err("nb_setval(3, 1)"));
        assertEquals("error(instantiation_error,'nb_delete/1')", err("nb_delete(X)"));
        assertEquals("1", all("b_setval(bk, 1), b_getval(bk, V)", "V"));
    }

    /** b_setval/2 is undone on backtracking — through the machine's own trail, not through Undo. */
    @Test
    public void testISS0499_BSetvalIsBacktrackable() {
        assertEquals("error(existence_error(variable,bk2),'b_getval/2')",
            all("(b_setval(bk2, 1), fail ; true), catch(b_getval(bk2, V), E, true)", "E"));
    }

    /** nb_current/2 enumerates lazily over the global variables. */
    @Test
    public void testISS0499_NbCurrentEnumerates() {
        prolog.solve("nb_setval(g1, 1).");
        prolog.solve("nb_setval(g2, 2).");
        assertEquals(2, prolog.solve("nb_current(_, _).").size());
        assertEquals(1, prolog.solve("once(nb_current(_, _)).").size());
    }

    // ================================================================ the ISO flags

    @Test
    public void testISS0499_PrologFlags() {
        // START_CHANGE: ISS-2025-0512 - bounded is FALSE: JProlog's integers are unbounded.
        assertEquals("false", all("current_prolog_flag(bounded, X)", "X"));
        assertEquals("1000000000000000000000000000000",
            all("X is 10^30", "X"));                      // and that is why
        // END_CHANGE: ISS-2025-0512
        // START_CHANGE: ISS-2025-0508 - ISO 8.17.2.3: type_error(atom, F) / domain_error.
        assertEquals("error(domain_error(prolog_flag,nosuchflag),'current_prolog_flag/2')",
            err("current_prolog_flag(nosuchflag, _)"));
        assertEquals("error(type_error(atom,3),'current_prolog_flag/2')",
            err("current_prolog_flag(3, _)"));
        // END_CHANGE: ISS-2025-0508
        assertTrue(prolog.solve("current_prolog_flag(_, _).").size() > 10);
        assertEquals(1, prolog.solve("once(current_prolog_flag(_, _)).").size());
        assertEquals("codes",
            all("set_prolog_flag(double_quotes, codes), current_prolog_flag(double_quotes, X)", "X"));
        // START_CHANGE: ISS-2025-0508 - ISO 8.17.1.3, all five clauses.
        assertEquals("error(instantiation_error,'set_prolog_flag/2')", err("set_prolog_flag(X, 1)"));
        assertEquals("error(type_error(atom,5),'set_prolog_flag/2')", err("set_prolog_flag(5, off)"));
        assertEquals("error(permission_error(modify,flag,bounded),'set_prolog_flag/2')",
            err("set_prolog_flag(bounded, junk)"));
        assertEquals("error(domain_error(prolog_flag,nosuchflag),'set_prolog_flag/2')",
            err("set_prolog_flag(nosuchflag, off)"));
        assertEquals("error(domain_error(flag_value,unknown+5),'set_prolog_flag/2')",
            err("set_prolog_flag(unknown, 5)"));
        // END_CHANGE: ISS-2025-0508
    }

    // ================================================================ halt/0,1

    @Test
    public void testISS0499_HaltArgumentErrors() {
        assertEquals("error(type_error(integer,a),'halt/1: exit code must be an integer')",
            err("halt(a)"));
        assertEquals("error(instantiation_error,'halt/1: exit code must be instantiated')",
            err("halt(X)"));
    }

    // ================================================================ findall/4

    /** findall/4 is NEW: findall/3 with an open tail. */
    @Test
    public void testISS0499_Findall4() {
        assertEquals("[1,2|t]", all("findall(X, member(X,[1,2]), L, t)", "L"));
        assertEquals("t", all("findall(X, fail, L, t)", "L"));
        assertEquals("[1,2,3]", all("findall(X, member(X,[1,2]), L, [3])", "L"));
        assertEquals("error(instantiation_error,'findall/4')", err("findall(X, G, L, [])"));
        assertEquals("error(type_error(callable,3),'findall/4')", err("findall(X, 3, L, [])"));
    }

    // ================================================================ ISS-2025-0501

    /**
     * 4.1-A deviation 4, paid off: the seven coroutining predicates raise
     * {@code permission_error(modify, static_procedure, PI)} again. Wave A deleted their Java
     * classes and with them their registry entries, and {@code isBuiltIn} needs a registration AND
     * an arity entry — so {@code assertz(freeze(X, Y))} had quietly become legal.
     */
    @Test
    public void testISS0501_TheCoroutiningPredicatesAreProtectedAgain() {
        String[][] seven = {
            {"freeze(x,y)", "freeze/2"}, {"when(x,y)", "when/2"}, {"dif(x,y)", "dif/2"},
            {"put_attr(a,b,c)", "put_attr/3"}, {"get_attr(a,b,c)", "get_attr/3"},
            {"del_attr(a,b)", "del_attr/2"}, {"attvar(a)", "attvar/1"},
        };
        for (String[] p : seven) {
            assertEquals("error(permission_error(modify,static_procedure," + p[1] + "),'assertz/1')",
                err("assertz(" + p[0] + ")"));
            assertEquals("error(permission_error(modify,static_procedure," + p[1] + "),'retract/1')",
                err("retract(" + p[0] + ")"));
            assertEquals("error(permission_error(access,private_procedure," + p[1] + "),'clause/2')",
                err("clause(" + p[0] + ", _)"));
        }
    }

    /** The same rule covers every v4 native and every prelude export, not only those seven. */
    @Test
    public void testISS0501_NativesAndLibraryExportsAreProtected() {
        assertEquals("error(permission_error(modify,static_procedure,memberchk/2),'assertz/1')",
            err("assertz(memberchk(a,b))"));
        assertEquals("error(permission_error(modify,static_procedure,term_string/2),'assertz/1')",
            err("assertz(term_string(a,b))"));
        assertEquals("error(permission_error(modify,static_procedure,findall/4),'assertz/1')",
            err("assertz(findall(a,b,c,d))"));
        assertEquals("error(permission_error(modify,static_procedure,partition/4),'assertz/1')",
            err("assertz(partition(a,b,c,d))"));
        assertEquals("error(permission_error(modify,static_procedure,freeze/2),'retractall/1')",
            err("retractall(freeze(_,_))"));
        assertEquals("error(permission_error(modify,static_procedure,freeze/2),'abolish/1')",
            err("abolish(freeze/2)"));
    }

    /** And a plain user predicate is still perfectly modifiable. */
    @Test
    public void testISS0501_UserPredicatesAreUnaffected() {
        assertEquals("true", yn("assertz(myown(1))"));
        assertEquals("1", all("myown(X)", "X"));
        assertEquals("true", yn("retract(myown(1))"));
        assertEquals("false", yn("myown(_)"));
        assertFalse("the coroutining predicates must still RUN",
            prolog.solve("freeze(X, true), X = 1.").isEmpty());
        assertFalse(prolog.solve("dif(A,B), A=1, B=2.").isEmpty());
    }
}
// END_CHANGE: ISS-2025-0499
