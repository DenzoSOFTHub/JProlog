package it.denzosoft.jprolog;

import org.junit.Test;
import static org.junit.Assert.*;



public class PrologEngineTest {

    @Test
    public void testConsultAndQuery() {
        // This is a placeholder test.  We'll flesh it out later.
        PrologEngine engine = new SimplePrologEngine(); // Assuming a simple engine implementation.
        engine.consult("father(john, mary).");
        Object result = engine.query("father(john, X).");
        assertNotNull(result);
    }

}
