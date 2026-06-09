package it.denzosoft.jprolog.core.compiled;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.engine.Rule;
import it.denzosoft.jprolog.core.terms.*;
import it.denzosoft.jprolog.core.operator.OperatorTable;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.*;

import static org.junit.Assert.*;

/**
 * Tests for the JPC binary compiled format.
 */
// START_CHANGE: ISS-2025-0085 - JPC format tests
public class JpcFormatTest {

    @Test
    public void testRoundTripSimpleFacts() throws Exception {
        // Create rules: parent(tom, bob). parent(bob, ann).
        List<Rule> rules = new ArrayList<>();
        rules.add(new Rule(
            new CompoundTerm(new Atom("parent"), Arrays.asList(new Atom("tom"), new Atom("bob"))),
            Collections.emptyList()));
        rules.add(new Rule(
            new CompoundTerm(new Atom("parent"), Arrays.asList(new Atom("bob"), new Atom("ann"))),
            Collections.emptyList()));

        // Write
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        new JpcWriter().write(rules, null, 12345L, baos);

        // Read
        JpcReader.CompiledProgram program = new JpcReader().read(new ByteArrayInputStream(baos.toByteArray()));

        assertEquals(12345L, program.sourceHash);
        assertEquals(2, program.rules.size());
        assertEquals("parent", program.rules.get(0).getHead().getName());
        assertEquals("tom", program.rules.get(0).getHead().getArguments().get(0).getName());
        assertEquals("bob", program.rules.get(0).getHead().getArguments().get(1).getName());
    }

    @Test
    public void testRoundTripRuleWithBody() throws Exception {
        // grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
        Term head = new CompoundTerm(new Atom("grandparent"),
            Arrays.asList(new Variable("X"), new Variable("Z")));
        List<Term> body = Arrays.asList(
            new CompoundTerm(new Atom("parent"), Arrays.asList(new Variable("X"), new Variable("Y"))),
            new CompoundTerm(new Atom("parent"), Arrays.asList(new Variable("Y"), new Variable("Z")))
        );
        List<Rule> rules = Collections.singletonList(new Rule(head, body));

        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        new JpcWriter().write(rules, null, 0L, baos);

        JpcReader.CompiledProgram program = new JpcReader().read(new ByteArrayInputStream(baos.toByteArray()));

        assertEquals(1, program.rules.size());
        Rule r = program.rules.get(0);
        assertEquals("grandparent", r.getHead().getName());
        assertEquals(2, r.getBody().size());
        assertEquals("parent", r.getBody().get(0).getName());
    }

    @Test
    public void testRoundTripNumbers() throws Exception {
        // age(tom, 42).
        List<Rule> rules = Collections.singletonList(new Rule(
            new CompoundTerm(new Atom("age"),
                Arrays.asList(new Atom("tom"), new it.denzosoft.jprolog.core.terms.Number(42.0))),
            Collections.emptyList()));

        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        new JpcWriter().write(rules, null, 0L, baos);

        JpcReader.CompiledProgram program = new JpcReader().read(new ByteArrayInputStream(baos.toByteArray()));

        assertEquals(1, program.rules.size());
        Term numTerm = program.rules.get(0).getHead().getArguments().get(1);
        assertTrue(numTerm instanceof it.denzosoft.jprolog.core.terms.Number);
        assertEquals(42.0, ((it.denzosoft.jprolog.core.terms.Number) numTerm).getValue(), 0.001);
    }

    // START_CHANGE: ISS-2025-0261 - int/float type and BigInteger precision must round-trip
    @Test
    public void testRoundTripNumberTypePreservation() throws Exception {
        java.math.BigInteger big = new java.math.BigInteger("123456789012345678901234567890");
        List<Rule> rules = Collections.singletonList(new Rule(
            new CompoundTerm(new Atom("nums"), Arrays.asList(
                new it.denzosoft.jprolog.core.terms.Number(42L),        // integer
                new it.denzosoft.jprolog.core.terms.Number(2.0, false), // float 2.0
                new it.denzosoft.jprolog.core.terms.Number(big))),      // BigInteger
            Collections.emptyList()));

        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        new JpcWriter().write(rules, null, 0L, baos);
        JpcReader.CompiledProgram program = new JpcReader().read(new ByteArrayInputStream(baos.toByteArray()));

        List<Term> args = program.rules.get(0).getHead().getArguments();
        it.denzosoft.jprolog.core.terms.Number n0 = (it.denzosoft.jprolog.core.terms.Number) args.get(0);
        it.denzosoft.jprolog.core.terms.Number n1 = (it.denzosoft.jprolog.core.terms.Number) args.get(1);
        it.denzosoft.jprolog.core.terms.Number n2 = (it.denzosoft.jprolog.core.terms.Number) args.get(2);

        assertTrue("42 must round-trip as integer", n0.isInteger());
        assertEquals(42L, n0.longValue());
        assertTrue("2.0 must round-trip as float (not collapse to integer)", n1.isFloat());
        assertEquals("2.0", n1.toString());
        assertTrue("BigInteger must round-trip as integer", n2.isInteger());
        assertEquals("BigInteger precision must be preserved", big, n2.bigIntegerValue());
    }
    // END_CHANGE: ISS-2025-0261

    @Test
    public void testRoundTripPrologString() throws Exception {
        // greeting("hello world").
        List<Rule> rules = Collections.singletonList(new Rule(
            new CompoundTerm(new Atom("greeting"),
                Collections.singletonList(new PrologString("hello world"))),
            Collections.emptyList()));

        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        new JpcWriter().write(rules, null, 0L, baos);

        JpcReader.CompiledProgram program = new JpcReader().read(new ByteArrayInputStream(baos.toByteArray()));

        Term strTerm = program.rules.get(0).getHead().getArguments().get(0);
        assertTrue(strTerm instanceof PrologString);
        assertEquals("hello world", ((PrologString) strTerm).getStringValue());
    }

    @Test
    public void testRoundTripOperators() throws Exception {
        OperatorTable ops = new OperatorTable();
        ops.defineOperator(700, it.denzosoft.jprolog.core.operator.Operator.Type.XFX, "means");

        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        new JpcWriter().write(Collections.emptyList(), ops, 0L, baos);

        JpcReader.CompiledProgram program = new JpcReader().read(new ByteArrayInputStream(baos.toByteArray()));

        boolean found = false;
        for (it.denzosoft.jprolog.core.operator.Operator op : program.operators) {
            if ("means".equals(op.getName()) && op.getPrecedence() == 700) {
                found = true;
                break;
            }
        }
        assertTrue("Custom operator 'means' should be in compiled output", found);
    }

    @Test
    public void testStringInterning() throws Exception {
        // parent(tom, bob). parent(tom, ann). — 'tom' and 'parent' are shared
        List<Rule> rules = new ArrayList<>();
        rules.add(new Rule(
            new CompoundTerm(new Atom("parent"), Arrays.asList(new Atom("tom"), new Atom("bob"))),
            Collections.emptyList()));
        rules.add(new Rule(
            new CompoundTerm(new Atom("parent"), Arrays.asList(new Atom("tom"), new Atom("ann"))),
            Collections.emptyList()));

        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        new JpcWriter().write(rules, null, 0L, baos);
        byte[] bytes = baos.toByteArray();

        // Verify size is reasonable (string interning should keep it small)
        assertTrue("Binary should be compact", bytes.length < 100);

        // Verify round-trip
        JpcReader.CompiledProgram program = new JpcReader().read(new ByteArrayInputStream(bytes));
        assertEquals(2, program.rules.size());
    }

    @Test
    public void testSourceHashValidation() throws Exception {
        String source = "parent(tom, bob).";
        long hash = JpcWriter.computeSourceHash(source);
        assertTrue("Hash should be non-zero", hash != 0);

        // Same source -> same hash
        assertEquals(hash, JpcWriter.computeSourceHash(source));

        // Different source -> different hash
        assertNotEquals(hash, JpcWriter.computeSourceHash("parent(tom, ann)."));
    }

    @Test
    public void testEndToEndCompileAndLoad() throws Exception {
        String source = "parent(tom, bob).\nparent(bob, ann).\ngrandparent(X,Z) :- parent(X,Y), parent(Y,Z).";

        // Compile using Prolog engine
        Prolog compiler = new Prolog();
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        compiler.compile(source, baos);

        // Load into fresh engine
        Prolog loader = new Prolog();
        loader.consultCompiled(new ByteArrayInputStream(baos.toByteArray()));

        // Query should work
        List<Map<String, Term>> solutions = loader.solve("grandparent(tom, Z).");
        assertFalse("Should find grandparent", solutions.isEmpty());
        assertEquals("ann", solutions.get(0).get("Z").toString());
    }

    @Test(expected = java.io.IOException.class)
    public void testInvalidMagicBytes() throws Exception {
        byte[] bad = { 0x00, 0x00, 0x00, 0x01, 0, 0, 0, 0, 0, 0, 0, 0 };
        new JpcReader().read(new ByteArrayInputStream(bad));
    }
}
// END_CHANGE: ISS-2025-0085
