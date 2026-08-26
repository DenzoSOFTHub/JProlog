package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.Before;
import org.junit.Test;

import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

// START_CHANGE: ISS-2025-0498 - 4.1 wave B acceptance: term construction/inspection on the v4 SPI.
/**
 * 4.1 wave B, step 3: {@code functor/3}, {@code arg/3}, {@code =../2}, {@code atom_to_term/3}, the
 * remaining type checks, {@code succ/2}, {@code plus/3} and {@code unify_with_occurs_check/2} are
 * v4 natives.
 *
 * <p>Two of these tests pin behaviour the registry versions did NOT have — see
 * {@link #testISS0498_FunctorDecomposesANonGroundCompound} and
 * {@link #testISS0498_ArgEnumeratesWithAnUnboundIndex}; the others pin what they did.
 */
public class EngineV4TermTest {

    private Prolog prolog;

    @Before
    public void setUp() {
        prolog = new Prolog();
    }

    private String text(Term t) {
        return it.denzosoft.jprolog.core.util.TermFormatter.format(t, true, false, true, 1200);
    }

    /** All solutions of a one-variable query, rendered as "V1;V2;..." ("false" when there are none). */
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

    private String all(String goal, String... vars) {
        List<Map<String, Term>> sols = prolog.solve(goal + ".");
        if (sols.isEmpty()) return "false";
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < sols.size(); i++) {
            if (i > 0) sb.append(';');
            for (int v = 0; v < vars.length; v++) {
                if (v > 0) sb.append(',');
                sb.append(vars[v]).append('=').append(text(sols.get(i).get(vars[v])));
            }
        }
        return sb.toString();
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
    public void testISS0498_TheTermFamilyIsNative() {
        BuiltinTable t = prolog.getV4Engine().natives();
        String[][] expected = {
            {"functor", "3"}, {"arg", "3"}, {"=..", "2"}, {"atom_to_term", "3"},
            {"number_vars", "3"}, {"succ", "2"}, {"plus", "3"},
            {"is_list", "1"}, {"proper_list", "1"}, {"partial_list", "1"},
            {"simple", "1"}, {"string", "1"}, {"must_be", "2"},
            {"unify_with_occurs_check", "2"},
        };
        for (String[] e : expected) {
            assertTrue(e[0] + "/" + e[1] + " must be a v4 native",
                t.isNative(e[0], Integer.parseInt(e[1])));
        }
    }

    // ================================================================ functor/3

    @Test
    public void testISS0498_FunctorModesAndErrors() {
        assertEquals("N=f,A=2", all("functor(f(a,b), N, A)", "N", "A"));
        assertEquals("N=foo,A=0", all("functor(foo, N, A)", "N", "A"));
        assertEquals("N=1.5,A=0", all("functor(1.5, N, A)", "N", "A"));
        assertEquals("N='.',A=2", all("functor([a], N, A)", "N", "A"));
        assertEquals("foo", all("functor(T, foo, 0)", "T"));
        assertEquals("1", all("functor(T, 1, 0)", "T"));
        assertEquals("false", all("functor(f(a), foo, 1)", "T"));
        assertEquals("error(instantiation_error,'functor/3')", err("functor(T, N, 2)"));
        assertEquals("error(type_error(atom,1),'functor/3')", err("functor(T, 1, 2)"));
        assertEquals("error(type_error(atomic,f(a)),'functor/3')", err("functor(T, f(a), 1)"));
        assertEquals("error(type_error(integer,a),'functor/3')", err("functor(T, foo, a)"));
        assertEquals("error(domain_error(not_less_than_zero,-1),'functor/3')",
            err("functor(T, foo, -1)"));
    }

    /** Construct mode makes fresh, unnamed cells (the registry version made named {@code _G0}s). */
    @Test
    public void testISS0498_FunctorConstructsFreshVariables() {
        assertEquals("[{X=1,Y=2}]", oneSolution("functor(T, foo, 2), arg(1, T, X), arg(2, T, Y), X = 1, Y = 2"));
    }

    private String oneSolution(String goal) {
        List<Map<String, Term>> sols = prolog.solve(goal + ".");
        assertEquals(1, sols.size());
        java.util.TreeMap<String, Term> t = new java.util.TreeMap<String, Term>();
        for (Map.Entry<String, Term> e : sols.get(0).entrySet()) {
            if (!e.getKey().startsWith("_") && !"T".equals(e.getKey())) t.put(e.getKey(), e.getValue());
        }
        StringBuilder sb = new StringBuilder("[{");
        boolean first = true;
        for (Map.Entry<String, Term> e : t.entrySet()) {
            if (!first) sb.append(',');
            first = false;
            sb.append(e.getKey()).append('=').append(text(e.getValue()));
        }
        return sb.append("}]").toString();
    }

    /**
     * ISS-2025-0498 correction: the registry version chose its mode with {@code Term.isGround()},
     * so a compound holding a variable took the CONSTRUCT branch and raised instantiation_error.
     * ISO 8.5.1 decomposes any non-variable first argument.
     */
    @Test
    public void testISS0498_FunctorDecomposesANonGroundCompound() {
        assertEquals("N=f,A=2", all("functor(f(X,b), N, A)", "N", "A"));
        assertEquals("N=g,A=1", all("functor(g(_), N, A)", "N", "A"));
    }

    // ================================================================ arg/3

    @Test
    public void testISS0498_ArgModesAndErrors() {
        assertEquals("a", all("arg(1, f(a,b), X)", "X"));
        assertEquals("b", all("arg(2, f(a,b), X)", "X"));
        assertEquals("false", all("arg(0, f(a), X)", "X"));
        assertEquals("false", all("arg(3, f(a), X)", "X"));
        assertEquals("error(instantiation_error,'arg/3')", err("arg(1, T, X)"));
        assertEquals("error(type_error(compound,foo),'arg/3')", err("arg(1, foo, X)"));
        assertEquals("error(type_error(integer,a),'arg/3')", err("arg(a, f(x), X)"));
        assertEquals("error(domain_error(not_less_than_zero,-1),'arg/3')", err("arg(-1, f(x), X)"));
    }

    /** ISS-2025-0498 correction: an unbound index ENUMERATES (it raised instantiation_error). */
    @Test
    public void testISS0498_ArgEnumeratesWithAnUnboundIndex() {
        assertEquals("N=1,X=a;N=2,X=b", all("arg(N, f(a,b), X)", "N", "X"));
        assertEquals("N=1,X=a", all("once(arg(N, f(a,b), X))", "N", "X"));
        // an atom is not a compound term: type_error, in either index mode
        assertEquals("error(type_error(compound,f),'arg/3')", err("arg(N, f, X)"));
    }

    /** arg/3 SHARES the argument, it does not copy it. */
    @Test
    public void testISS0498_ArgSharesTheArgumentCell() {
        assertEquals("7", all("T = f(A), arg(1, T, X), X = 7", "A"));
    }

    // ================================================================ =../2

    @Test
    public void testISS0498_UnivModesAndErrors() {
        assertEquals("[f,a,b]", all("f(a,b) =.. L", "L"));
        assertEquals("[foo]", all("foo =.. L", "L"));
        assertEquals("[1]", all("1 =.. L", "L"));
        assertEquals("['.',a,[b]]", all("[a,b] =.. L", "L"));
        assertEquals("foo(a,b)", all("T =.. [foo,a,b]", "T"));
        assertEquals("foo", all("T =.. [foo]", "T"));
        assertEquals("1", all("T =.. [1]", "T"));
        assertEquals("error(domain_error(non_empty_list,[]),'=../2')", err("T =.. []"));
        assertEquals("error(instantiation_error,'=../2')", err("T =.. L"));
        assertEquals("error(instantiation_error,'=../2')", err("T =.. [foo|X]"));
        assertEquals("error(type_error(atomic,f(a)),'=../2')", err("T =.. [f(a)]"));
        assertEquals("error(type_error(atom,f(a)),'=../2')", err("T =.. [f(a),b]"));
    }

    // ================================================================ atom_to_term/3

    @Test
    public void testISS0498_AtomToTerm() {
        assertEquals("f(X,Y)", all("atom_to_term('f(X,Y)', T, B)", "T"));
        assertEquals("['X'=X,'Y'=Y]", all("atom_to_term('f(X,Y)', T, B)", "B"));
        assertEquals("[]", all("atom_to_term('foo', T, B)", "B"));
        assertTrue(err("atom_to_term(3, T, B)").startsWith("eval:"));
    }

    // ================================================================ succ/2, plus/3

    @Test
    public void testISS0498_SuccAndPlus() {
        assertEquals("4", all("succ(3, X)", "X"));
        assertEquals("2", all("succ(X, 3)", "X"));
        assertEquals("false", all("succ(X, 0)", "X"));
        assertEquals("false", all("succ(a, X)", "X"));
        assertEquals("false", all("succ(-1, X)", "X"));
        assertTrue(err("succ(_, _)").startsWith("eval:"));
        assertEquals("3", all("plus(1, 2, X)", "X"));
        assertEquals("2", all("plus(1, X, 3)", "X"));
        assertEquals("1", all("plus(X, 2, 3)", "X"));
        assertEquals("false", all("plus(1, 2, 4)", "X"));
        assertEquals("false", all("plus(a, 2, X)", "X"));
        assertTrue(err("plus(_, _, _)").startsWith("eval:"));
    }

    // ================================================================ the remaining type checks

    @Test
    public void testISS0498_TypeChecks() {
        assertEquals("true", yn("is_list([a,b])"));
        assertEquals("false", yn("is_list([a|_])"));
        assertEquals("false", yn("is_list(a)"));
        assertEquals("true", yn("proper_list([a])"));
        assertEquals("true", yn("partial_list([a|_])"));
        assertEquals("false", yn("partial_list([a])"));
        assertEquals("true", yn("simple(a)"));
        assertEquals("false", yn("simple(f(x))"));
        assertEquals("true", yn("string(\"s\")"));
        assertEquals("false", yn("string(s)"));
    }

    private String yn(String goal) {
        return prolog.solve(goal + ".").isEmpty() ? "false" : "true";
    }

    @Test
    public void testISS0498_MustBe() {
        assertEquals("true", yn("must_be(integer, 3)"));
        assertEquals("true", yn("must_be(list, [a])"));
        assertEquals("error(type_error(integer,a),'must_be/2')", err("must_be(integer, a)"));
        assertEquals("error(instantiation_error,'must_be/2')", err("must_be(atom, X)"));
        assertEquals("error(domain_error(type,nosuch),'must_be/2')", err("must_be(nosuch, a)"));
        assertEquals("error(type_error(atom,3),'must_be/2')", err("must_be(3, a)"));
    }

    // ================================================================ unify_with_occurs_check/2

    @Test
    public void testISS0498_UnifyWithOccursCheck() {
        assertEquals("false", yn("unify_with_occurs_check(X, f(X))"));
        assertEquals("f(a)", all("unify_with_occurs_check(X, f(a))", "X"));
        assertEquals("true", yn("unify_with_occurs_check(f(X,Y), f(Y,X))"));
        assertEquals("false", yn("unify_with_occurs_check(a, b)"));
        // and it leaves NO binding behind when it fails
        assertEquals("unbound", all("(unify_with_occurs_check(X, f(X)) -> true ; true), "
            + "(var(X) -> Y = unbound ; Y = bound)", "Y"));
    }
}
// END_CHANGE: ISS-2025-0498
