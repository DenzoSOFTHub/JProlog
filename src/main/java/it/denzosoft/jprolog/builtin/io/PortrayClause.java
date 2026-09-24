package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.builtin.LibArgs;
import it.denzosoft.jprolog.core.engine.v4.Errors;
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.v4.Writer;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.PrintStream;
import java.util.List;
import java.util.Map;

// START_CHANGE: ISS-2025-0475 - engine v4 wave W7 (design B.12)
/**
 * portray_clause(+Clause) / portray_clause(+Stream, +Clause).
 *
 * <p>Writes {@code Clause} the way {@code listing/1} does: quoted, variables numbered {@code A},
 * {@code B}, ..., the body one goal per indented line, terminated by a full stop and a newline.
 */
public class PortrayClause implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        int arity = (query.getArguments() == null) ? 0 : query.getArguments().size();
        if (arity != 1 && arity != 2) {
            throw LibArgs.unknownArity(query);   // ISS-2025-0697
        }
        PrintStream out;
        Term clause;
        if (arity == 1) {
            out = StreamManager.out();
            clause = query.getArguments().get(0);
        } else {
            out = IOStreamUtils.resolveOutputStream(query.getArguments().get(0), bindings, "portray_clause/2");
            clause = query.getArguments().get(1);
        }
        out.print(Writer.portrayClause(clause.resolveBindings(bindings), null));
        out.flush();
        solutions.add(bindings);
        return true;
    }
}
// END_CHANGE: ISS-2025-0475
