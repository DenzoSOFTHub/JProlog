package it.denzosoft.jprolog.test.core;
import it.denzosoft.jprolog.core.engine.Prolog;

import it.denzosoft.jprolog.core.terms.Term;
import org.junit.Test;
import static org.junit.Assert.*;

import java.util.List;
import java.util.Map;

public class PrologEngineTest {

    @Test
    public void testConsultAndQuery() {
        // Use the working Prolog class instead of the stub SimplePrologEngine
        Prolog prolog = new Prolog();
        prolog.consult("father(john, mary).");
        List<Map<String, Term>> result = prolog.solve("father(john, X).");
        assertNotNull(result);
        assertEquals(1, result.size());
        assertEquals("mary", result.get(0).get("X").toString());
    }

}
