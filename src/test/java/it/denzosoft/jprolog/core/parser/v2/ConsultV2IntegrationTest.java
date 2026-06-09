package it.denzosoft.jprolog.core.parser.v2;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.Test;

import java.util.List;
import java.util.Map;

import static org.junit.Assert.*;

/**
 * End-to-end validation that the clean-room v2 parser works through the live engine
 * (via {@link Prolog#consultV2(String)}): real programs are loaded and queried, exercising
 * facts, rules, operators, lists, arithmetic, recursion, DCG, directives, and the parsing
 * constructs the legacy parser gets wrong (canonical functor, operator-as-atom, quoted atoms).
 */
public class ConsultV2IntegrationTest {

    private Prolog engine() { return new Prolog(); }

    @Test public void factsAndRules() {
        Prolog p = engine();
        p.consultV2("parent(tom, bob).\nparent(bob, ann).\n"
            + "grandparent(X, Z) :- parent(X, Y), parent(Y, Z).");
        List<Map<String, Term>> s = p.solve("grandparent(tom, W).");
        assertEquals(1, s.size());
        assertEquals("ann", s.get(0).get("W").toString());
    }

    @Test public void arithmeticRecursion() {
        Prolog p = engine();
        p.consultV2("fact(0, 1).\nfact(N, F) :- N > 0, N1 is N - 1, fact(N1, F1), F is N * F1.");
        List<Map<String, Term>> s = p.solve("fact(5, F).");
        assertEquals("120", s.get(0).get("F").toString());
    }

    @Test public void listsAndOperators() {
        Prolog p = engine();
        p.consultV2("len([], 0).\nlen([_|T], N) :- len(T, M), N is M + 1.");
        assertEquals("3", p.solve("len([a,b,c], N).").get(0).get("N").toString());
        // operator-precedence inside a query parsed by the legacy query path is fine;
        // here we check a program clause with nested operators loaded by v2.
        p.consultV2("calc(X) :- X is 2 + 3 * 4 - 1.");
        assertEquals("13", p.solve("calc(X).").get(0).get("X").toString());
    }

    @Test public void canonicalFunctorAndOperatorAtomsLoad() {
        Prolog p = engine();
        // -(5,2) must load as -/2 (so it evaluates to 3); '-' and '+' must load as plain atoms.
        // (NOTE: the query path still uses the legacy parser, so we avoid (-)-in-a-query here —
        // the v2 consult itself handles all of these; that's what this test validates.)
        // Note: `X = -.` is one graphic atom `-.` under ISO maximal-munch; to load the atom '-'
        // you write `(-)`. The v2 parser implements this correctly.
        p.consultV2("diff(R) :- R = -(5, 2).\nsym(-).\nsym(+).\nis_minus(X) :- X = (-).");
        assertEquals("3", p.solve("diff(R), V is R.").get(0).get("V").toString());
        List<Map<String, Term>> syms = p.solve("sym(S).");
        assertEquals(2, syms.size());
        assertEquals("-", syms.get(0).get("S").toString());   // operator '-' loaded as an atom
        assertEquals("+", syms.get(1).get("S").toString());
        assertEquals("-", p.solve("is_minus(M).").get(0).get("M").toString());
    }

    @Test public void quotedAtomsAndDoubledQuotes() {
        Prolog p = engine();
        p.consultV2("msg('don''t panic').\nname('a. b').");
        assertEquals("don't panic", p.solve("msg(M).").get(0).get("M").toString());
        assertEquals("a. b", p.solve("name(N).").get(0).get("N").toString());
    }

    @Test public void directivesAndInitialization() {
        Prolog p = engine();
        p.consultV2(":- op(700, xfx, ===).\nequal(X, Y) :- X === Y.\n:- initialization(assertz(loaded)).");
        // the === operator defined by the directive must let the next clause parse
        assertFalse(p.solve("loaded.").isEmpty());
    }

    @Test public void dcgRule() {
        Prolog p = engine();
        p.consultV2("greeting --> [hello], [world].");
        assertFalse(p.solve("phrase(greeting, [hello, world]).").isEmpty());
    }

    @Test public void backtrackingMultipleSolutions() {
        Prolog p = engine();
        p.consultV2("color(red).\ncolor(green).\ncolor(blue).");
        assertEquals(3, p.solve("color(C).").size());
    }

    @Test public void parseErrorResyncKeepsOtherClauses() {
        // A syntax error in one clause must not drop the rest of the file (per-clause resync).
        Prolog p = engine();
        try { p.consultV2("good1(a).\nbad(X) :- X = ) bad .\ngood2(b)."); } catch (RuntimeException ignore) {}
        assertFalse("good1 must load despite the bad middle clause", p.solve("good1(a).").isEmpty());
        assertFalse("good2 must load despite the bad middle clause", p.solve("good2(b).").isEmpty());
    }
}
