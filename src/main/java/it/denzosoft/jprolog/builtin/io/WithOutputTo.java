package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.SolverContext;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * with_output_to(+Target, :Goal) - Execute Goal capturing output.
 * Target = atom(X) captures output as atom X.
 */
public class WithOutputTo implements BuiltInWithContext {

    private final SolverContext querySolver;

    public WithOutputTo(SolverContext querySolver) {
        this.querySolver = querySolver;
    }

    @Override
    public boolean executeWithContext(SolverContext solver, Term query,
                                     Map<String, Term> bindings,
                                     List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) return false;

        Term target = query.getArguments().get(0).resolveBindings(bindings);
        Term goal = query.getArguments().get(1).resolveBindings(bindings);

        // START_CHANGE: ISS-2025-0472 - engine v4 wave W7 closes LIM-025: the capture is the
        // thread-local output override, not a process-wide System.setOut. Every built-in writes
        // through StreamManager.out()/resolveOutput now, so two threads can capture concurrently.
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        PrintStream capture;
        try {
            capture = new PrintStream(baos, true, "UTF-8");
        } catch (java.io.UnsupportedEncodingException e) {
            capture = new PrintStream(baos, true);
        }
        PrintStream prevThread = StreamManager.threadLocalOutput();

        List<Map<String, Term>> temp = new ArrayList<>();
        boolean ok;
        try {
            StreamManager.setThreadLocalOutput(capture);
            ok = solver.solveMeta(goal, new HashMap<>(bindings), temp)   /* ISS-2025-0431 - ENG-04 */;
        } finally {
            capture.flush();
            StreamManager.setThreadLocalOutput(prevThread);
        }
        if (!ok || temp.isEmpty()) return false;

        String captured;
        try {
            captured = baos.toString("UTF-8");
        } catch (java.io.UnsupportedEncodingException e) {
            captured = baos.toString();
        }

        if (!(target instanceof CompoundTerm) || ((CompoundTerm) target).getArguments().size() != 1) {
            return false;
        }
        CompoundTerm ct = (CompoundTerm) target;
        Term sink = ct.getArguments().get(0);
        Term value;
        switch (ct.getName()) {
            case "atom":   value = new Atom(captured); break;
            case "string": value = new it.denzosoft.jprolog.core.terms.PrologString(captured); break;
            case "codes":  value = codes(captured); break;
            case "chars":  value = chars(captured); break;
            default: return false;
        }
        Map<String, Term> newBindings = new HashMap<>(temp.get(0));
        if (sink.unify(value, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
        // END_CHANGE: ISS-2025-0472
    }

    private static Term codes(String s) {
        Term list = new Atom("[]");
        for (int i = s.length() - 1; i >= 0; i--) {
            list = new CompoundTerm(new Atom("."), java.util.Arrays.asList(
                (Term) new it.denzosoft.jprolog.core.terms.Number((long) s.charAt(i)), list));
        }
        return list;
    }

    private static Term chars(String s) {
        Term list = new Atom("[]");
        for (int i = s.length() - 1; i >= 0; i--) {
            list = new CompoundTerm(new Atom("."), java.util.Arrays.asList(
                (Term) new Atom(String.valueOf(s.charAt(i))), list));
        }
        return list;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("with_output_to/2 requires context");
    }
}
