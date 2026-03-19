package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.CutStatus;
import it.denzosoft.jprolog.core.engine.QuerySolver;
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

    private final QuerySolver querySolver;

    public WithOutputTo(QuerySolver querySolver) {
        this.querySolver = querySolver;
    }

    @Override
    public boolean executeWithContext(QuerySolver solver, Term query,
                                     Map<String, Term> bindings,
                                     List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) return false;

        Term target = query.getArguments().get(0).resolveBindings(bindings);
        Term goal = query.getArguments().get(1).resolveBindings(bindings);

        // Capture output
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        PrintStream capture = new PrintStream(baos);
        PrintStream oldOut = System.out;

        try {
            System.setOut(capture);
            List<Map<String, Term>> temp = new ArrayList<>();
            boolean ok = solver.solve(goal, new HashMap<>(bindings), temp, CutStatus.notOccurred());
            System.out.flush();
            System.setOut(oldOut);

            if (!ok || temp.isEmpty()) return false;

            String captured = baos.toString();

            // Handle target type
            if (target instanceof CompoundTerm) {
                CompoundTerm ct = (CompoundTerm) target;
                if (ct.getName().equals("atom") && ct.getArguments().size() == 1) {
                    Term atomVar = ct.getArguments().get(0);
                    Map<String, Term> newBindings = new HashMap<>(temp.get(0));
                    if (atomVar.unify(new Atom(captured), newBindings)) {
                        solutions.add(newBindings);
                        return true;
                    }
                }
            }
            return false;
        } finally {
            System.setOut(oldOut);
        }
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("with_output_to/2 requires context");
    }
}
