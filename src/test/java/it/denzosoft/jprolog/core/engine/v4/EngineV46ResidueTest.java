package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.builtin.io.StreamManager;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.terms.Term;
import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

// START_CHANGE: ISS-2025-0790..0797 - 4.6 wave Q7: the residue sweep.
/**
 * Wave Q7 of the 4.6 completeness program (report-completeness-4.6-2026-09-23.md, section 16):
 * the SWI library(lists)/library(ordsets) predicates JProlog lacked, the recorded database and
 * flag/3, the prelude export-list scanner, the multifile reload order, the FFI argument checks and
 * the library inputs that failed silently where SWI raises.
 */
public class EngineV46ResidueTest {

    private Prolog prolog;

    @Before
    public void setUp() {
        prolog = new Prolog();
    }

    private void ok(String goal) {
        assertFalse("expected success: " + goal, prolog.solve(goal + ".").isEmpty());
    }

    private void no(String goal) {
        assertTrue("expected failure: " + goal, prolog.solve(goal + ".").isEmpty());
    }

    private String first(String goal, String var) {
        List<Map<String, Term>> r = prolog.solve(goal + ".");
        assertFalse("expected success: " + goal, r.isEmpty());
        return Writer.format(r.get(0).get(var), new Writer.Options(), 1200);
    }

    /** The error term {@code goal} raises, printed unquoted (or "none" / "failed"). */
    private String error(String goal) {
        return first("catch((" + goal + ", E = none ; E = failed), E0, E = E0)", "E");
    }

    private void det(String goal) {
        ok("setup_call_cleanup(true, (" + goal + "), Det = true), Det == true");
    }

    // ------------------------------------------------------------------ library(lists)

    @Test
    public void testISS0790_ListsAdditions() {
        assertEquals("[a,b,c]", first("append([[a],[b,c],[]], L)", "L"));
        assertEquals("[[]-[a],[a]-[]]", first("findall(X-Y, append([X,Y], [a]), L)", "L"));
        assertEquals("error(type_error(list,foo),must_be/2)", error("append(foo, _)"));
        assertEquals("[1-2,2-3]", first("findall(X-Y, nextto(X, Y, [1,2,3]), L)", "L"));
        assertEquals("f(x)", first("max_member(M, [3,1,f(x),a])", "M"));
        assertEquals("1", first("min_member(M, [3,1,f(x),a])", "M"));
        assertEquals("4", first("max_member(@=<, M, [3,1,4])", "M"));
        assertEquals("1", first("min_member(@=<, M, [3,1,4])", "M"));
        no("max_member(_, [])");
        // first occurrences kept, compared with ==
        ok("list_to_set([a,b,a,X,c,X,b,Y], S), S = [A,B,C,D,E], A == a, B == b, C == X, D == c, E == Y");
        assertEquals("2", first("proper_length([a,b], N)", "N"));
        no("proper_length([a|_], _)");
        det("max_member(M, [3,1,4])");
        det("list_to_set([a,b,a], S)");
        det("append([[a],[b]], L)");
    }

    @Test
    public void testISS0790_Ordsets() {
        assertEquals("[a,b,c,d,e]", first("ord_union([a,c,e], [b,c,d], U)", "U"));
        assertEquals("[a,b,c]", first("ord_union([[c,a],[b],[a]], U)", "U"));
        assertEquals("[a,c]", first("ord_subtract([a,b,c,d], [b,d,e], D)", "D"));
        ok("ord_memberchk(c, [a,b,c])");
        no("ord_memberchk(x, [a,b,c])");
        assertEquals("[a,b,c]", first("ord_add_element([a,c], b, S)", "S"));
        assertEquals("[a,c,d]", first("ord_insert([a,c], d, S)", "S"));
        assertEquals("[a,c]", first("ord_del_element([a,b,c], b, S)", "S"));
        assertEquals("[b,d]", first("ord_intersection([a,b,c,d], [b,d,e], I)", "I"));
        assertEquals("[c]", first("ord_intersection([[a,b,c],[b,c],[c,d]], I)", "I"));
        ok("ord_subset([b,d], [a,b,c,d])");
        no("ord_subset([b,x], [a,b,c,d])");
        ok("ord_empty([])");
        assertEquals("[a,b,c]", first("list_to_ord_set([c,a,b,a], S)", "S"));
        assertEquals("[a,d]", first("ord_symdiff([a,b,c], [b,c,d], S)", "S"));
        ok("ord_disjoint([a], [b])");
        ok("ord_intersect([a,b], [b,c])");
        assertEquals("[a,c]", first("ord_selectchk(b, [a,b,c], R)", "R"));
        ok("is_ordset([a,b])");
        no("is_ordset([b,a])");
        ok("ord_seteq([a], [a])");
        det("ord_union([a,c,e], [b,c,d], _)");
        det("ord_subtract([a,b,c,d], [b,d,e], _)");
        det("ord_intersection([a,b,c,d], [b,d,e], _)");
        det("ord_memberchk(c, [a,b,c,d,e])");
        // library(ordsets) loads (a prelude module) and the predicates are not user predicates
        ok("use_module(library(ordsets))");
    }

    // ------------------------------------------------------------------ recorded database, flag/3

    @Test
    public void testISS0791_RecordedDatabase() {
        ok("recorda(k, f(_), _), recordz(k, g(1)), recorda(k, h)");
        assertEquals("[h,f(_),g(1)]", first("findall(V, recorded(k, V), L)", "L").replaceAll("_[A-Z0-9]*", "_"));
        // a record is a copy: later bindings of the caller do not reach it
        ok("X = p(Y), recordz(c, X), Y = 1, recorded(c, p(Z)), var(Z)");
        // references, erase/1, the logical update view
        ok("recorded(k, g(Z), R), erase(R), \\+ recorded(k, g(_))");
        no("recordz(e, x, R), erase(R), erase(R)");
        ok("recordz(e2, x, R), recorded(K, V, R), K == e2, V == x");
        // compound keys count by name/arity; unbound key enumerates
        ok("recordz(foo(1,2), bar), recorded(foo(a,b), V), V == bar");
        assertEquals("[k,c,e2,foo(_,_)]", first("findall(K, current_key(K), L)", "L").replaceAll("_[A-Z0-9]*", "_"));
        // errors
        assertEquals("error(instantiation_error,recorda/2)", error("recorda(_, x)"));
        assertEquals("error(type_error(key,1.5),recorded/2)", error("recorded(1.5, _)"));
        assertEquals("error(type_error(db_reference,foo),erase/1)", error("erase(foo)"));
        det("recorded(k, h)");
        // shared by the engine's threads, private to the engine
        Prolog other = new Prolog();
        assertTrue(other.solve("recorded(k, _).").isEmpty());
    }

    @Test
    public void testISS0791_Flag() {
        assertEquals("0", first("flag(cnt, Old, Old+1)", "Old"));
        assertEquals("1", first("flag(cnt, Old, Old*10)", "Old"));
        assertEquals("10", first("flag(cnt, Old, Old)", "Old"));
        assertEquals("abc", first("flag(a, _, abc), flag(a, V, V)", "V"));
        // Old does not unify: nothing changes
        no("flag(cnt, 99, 0)");
        assertEquals("10", first("flag(cnt, V, V)", "V"));
        assertEquals("error(type_error(evaluable,foo/0),flag/3)", error("flag(k, _, foo+1)"));
        assertEquals("error(instantiation_error,flag/3)", error("flag(_, _, 1)"));
    }

    // ------------------------------------------------------------------ the prelude header scanner

    @Test
    public void testISS0792_PreludeHeaderSkipsComments() {
        String src = "% a leading comment mentioning :- module(fake, [x/1]).\n"
            + ":- module(synthetic, [a/1,   % the first export\n"
            + "                      b/2,   /* a block\n"
            + "                                comment, c/9 */ c/3,\n"
            + "                      '%odd'/1, % the quoted atom is not a comment\n"
            + "                      d/0]).\n"
            + "a(0'%).\n";
        Prelude.Lib lib = Prelude.header("/synthetic.pl", src);
        assertEquals("synthetic", lib.module);
        assertEquals("[a/1, b/2, c/3, %odd/1, d/0]", lib.exports.toString());
        // the real prelude still indexes every module
        assertEquals("ordsets", Prelude.owner("ord_union", 3));
        assertEquals("lists", Prelude.owner("append", 2));
    }

    // ------------------------------------------------------------------ the example program

    @Test
    public void testISS0793_ClpfdExampleDomainFormat() throws Exception {
        File f = new File("examples/test_41_clpfd.pl");
        if (!f.isFile()) return;                             // examples not in this checkout
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        StreamManager.setThreadLocalOutput(new PrintStream(out, true, "UTF-8"));
        try {
            prolog.solve("consult('" + f.getAbsolutePath().replace("\\", "/") + "').");
        } finally {
            StreamManager.setThreadLocalOutput(null);
        }
        String text = new String(out.toByteArray(), StandardCharsets.UTF_8);
        assertTrue(text, text.contains("PASS: fd_dom_basic"));
        assertTrue(text, text.contains("PASS: fd_dom_holes"));
        assertTrue(text, text.contains("Failed: 0"));
    }

    // ------------------------------------------------------------------ multifile reload order

    @Test
    public void testISS0794_MultifileReloadKeepsClauseOrder() throws Exception {
        File dir = Files.createTempDirectory("jprolog-q7-mf").toFile();
        File f1 = new File(dir, "f1.pl"), f2 = new File(dir, "f2.pl");
        Files.write(f1.toPath(), ":- multifile p/1.\np(a).\np(a2).\n".getBytes(StandardCharsets.UTF_8));
        Files.write(f2.toPath(), ":- multifile p/1.\np(b).\n".getBytes(StandardCharsets.UTF_8));
        String c1 = "consult('" + f1.getAbsolutePath().replace("\\", "/") + "')";
        String c2 = "consult('" + f2.getAbsolutePath().replace("\\", "/") + "')";
        ok(c1);
        ok(c2);
        assertEquals("[a,a2,b]", first("findall(X, p(X), L)", "L"));
        // SWI (manual 4.3.2): the reloaded file's clauses keep their place — was [b,a,a2]
        ok(c1);
        assertEquals("[a,a2,b]", first("findall(X, p(X), L)", "L"));
        // a changed file: its new clauses take the old place, other files' clauses stay after them
        Files.write(f1.toPath(), ":- multifile p/1.\np(z).\n".getBytes(StandardCharsets.UTF_8));
        ok(c1);
        assertEquals("[z,b]", first("findall(X, p(X), L)", "L"));
        ok(c2);
        assertEquals("[z,b]", first("findall(X, p(X), L)", "L"));
    }

    // ------------------------------------------------------------------ FFI argument checks

    @Test
    public void testISS0796_FfiArgumentFaultsRaise() {
        assertEquals("error(instantiation_error,context(java_new/3,the class name must be bound))",
            error("java_new(_, [], _)"));
        assertEquals("error(existence_error(class,no.Such),context(java_class/2,unknown Java class no.Such))",
            error("java_class('no.Such', _)"));
        assertTrue(error("java_new('java.lang.StringBuilder', foo, _)").startsWith("error(type_error(list,foo)"));
        assertTrue(error("java_call(f(x), toString, [], _)").startsWith("error(type_error(java_object,f(x))"));
        assertTrue(error("java_call('java.lang.Math', nosuch, [], _)").startsWith("error(existence_error(method,nosuch)"));
        assertTrue(error("java_call('java.lang.Integer', parseInt, [abc], _)")
            .startsWith("error(java_exception(java.lang.NumberFormatException)"));
        assertTrue(error("java_set_field('java.lang.Integer', 'MAX_VALUE', 1)")
            .startsWith("error(permission_error(modify,final_field,MAX_VALUE)"));
        assertTrue(error("java_array_new(int, -1, _)").startsWith("error(domain_error(not_less_than_zero,-1)"));
        assertTrue(error("java_array_get(foo, 0, _)").startsWith("error(type_error(java_array,foo)"));
        assertTrue(error("java_release_ref(_)").startsWith("error(instantiation_error"));
        // the happy paths are unchanged; an index outside the array fails like arg/3
        ok("java_new('java.lang.StringBuilder', [abc], O), java_call(O, toString, [], S), S == abc");
        ok("java_call('java.lang.Math', max, [3,4], M), M == 4");
        ok("java_array_new(int, 3, A), java_array_set(A, 1, 7), java_array_get(A, 1, E), E == 7");
        no("java_array_new(int, 3, A), java_array_get(A, 5, _)");
        no("java_instanceof(null, 'String')");
    }

    // ------------------------------------------------------------------ library inputs that failed

    @Test
    public void testISS0797_UnboundLibraryInputsRaise() {
        assertTrue(error("graph_vertices(_, V)").startsWith("error(instantiation_error"));
        assertTrue(error("graph_vertices(f(x), V)").startsWith("error(type_error(list,f(x))"));
        assertTrue(error("graph_path(_, a, b, P)").startsWith("error(instantiation_error"));
        assertEquals("[a,b]", first("graph_vertices([edge(a,b)], V)", "V"));
        assertTrue(error("json_get(_, a, V)").startsWith("error(instantiation_error"));
        assertTrue(error("spy(_)").startsWith("error(instantiation_error"));
        assertTrue(error("string_to_atom(_, _)").startsWith("error(instantiation_error"));
        assertTrue(error("atom_to_number(_, _)").startsWith("error(instantiation_error"));
        assertTrue(error("to_codes(_, _)").startsWith("error(instantiation_error"));
        // enhanced_phrase/2,3 run the grammar (they answered true without running it)
        prolog.consult("greeting --> [hello], name.\nname --> [world].\n");
        ok("enhanced_phrase(greeting, [hello, world])");
        no("enhanced_phrase(greeting, [hello, there])");
        assertEquals("[x]", first("enhanced_phrase(greeting, [hello, world, x], R)", "R"));
        assertTrue(error("enhanced_phrase(_, [a])").startsWith("error(instantiation_error"));
    }
}
// END_CHANGE: ISS-2025-0790..0797
