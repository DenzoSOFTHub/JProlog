package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.v4.Writer;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.PrintStream;
import java.util.List;
import java.util.Map;

// START_CHANGE: ISS-2025-0475 - engine v4 wave W7 (design B.12)
/**
 * print_message(+Kind, +Message) — the minimal always-available form.
 *
 * <p>{@code print_message(error, error(Formal, Context))} renders the ISO error readably
 * ({@code ERROR: Type error: `integer' expected, found `a'}); anything else is written quoted.
 * {@code error} and {@code warning} go to {@code user_error}, everything else to the current output.
 * {@code silent} prints nothing. A user-defined {@code message_hook/3} is not consulted.
 */
public class PrintMessage implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments() == null || query.getArguments().size() != 2) {
            throw new PrologEvaluationException("print_message/2 requires exactly 2 arguments");
        }
        Term kindTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term message = query.getArguments().get(1).resolveBindings(bindings);
        String kind = (kindTerm instanceof Atom) ? ((Atom) kindTerm).getName() : "informational";
        if ("silent".equals(kind)) {
            solutions.add(bindings);
            return true;
        }
        PrintStream out = ("error".equals(kind) || "warning".equals(kind))
            ? StreamManager.streams().writerFor(StreamManager.streams().userError())
            : StreamManager.out();
        out.println(Writer.message(kind, message));
        out.flush();
        solutions.add(bindings);
        return true;
    }
}
// END_CHANGE: ISS-2025-0475
