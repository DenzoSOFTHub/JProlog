package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.builtin.AbstractBuiltInWithContext;
import it.denzosoft.jprolog.core.engine.SolverContext;
import it.denzosoft.jprolog.core.engine.v4.Writer;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.PrintStream;
import java.util.List;
import java.util.Map;

/**
 * write_term/2 - write_term(+Term, +Options) and the legacy write_term(+Stream, +Term).
 * write_term/3 - write_term(+Stream, +Term, +Options) — ISO 8.14.2.
 *
 * <p>START_CHANGE: ISS-2025-0475 - engine v4 wave W7: the whole option set is implemented, over
 * {@link Writer}: {@code quoted}, {@code ignore_ops}, {@code numbervars}, {@code max_depth},
 * {@code portray}, {@code cycles}, {@code variable_names}, {@code spacing(next_argument)}. The
 * private half-formatter this class used to carry (which ignored operators, cycles and portray, and
 * recursed on every argument) is gone.
 */
public class WriteTerm extends AbstractBuiltInWithContext {

    public WriteTerm(SolverContext solver) {
        super(solver);
    }

    @Override
    public boolean execute(Term term, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        // START_CHANGE: ISS-2025-0352 - extract arguments from the query and report success via solutions
        return executeWithContext(solver, term, bindings, solutions);
        // END_CHANGE: ISS-2025-0352
    }

    @Override
    public boolean solve(SolverContext solver, Map<String, Term> bindings) {
        Term[] args = getArguments();

        if (args.length == 2) {
            Term first = args[0].resolveBindings(bindings);
            if (isStream(first)) {
                // legacy write_term(+Stream, +Term)
                return writeTo(first, args[1], defaults(), bindings, "write_term/2");
            }
            Writer.Options o = WriteOptions.parse(args[1], bindings, solver, "write_term/2");
            return writeTo(null, args[0], o, bindings, "write_term/2");
        }
        if (args.length == 3) {
            Writer.Options o = WriteOptions.parse(args[2], bindings, solver, "write_term/3");
            return writeTo(args[0].resolveBindings(bindings), args[1], o, bindings, "write_term/3");
        }
        return false;
    }

    private Writer.Options defaults() {
        Writer.Options o = new Writer.Options();
        o.numbervars = false;
        return o;
    }

    private boolean writeTo(Term streamTerm, Term term, Writer.Options o,
                            Map<String, Term> bindings, String context) {
        PrintStream out = (streamTerm == null)
            ? StreamManager.out()
            : IOStreamUtils.resolveOutputStream(streamTerm, bindings, context);
        out.print(Writer.format(term.resolveBindings(bindings), o, 1200));
        out.flush();
        return true;
    }

    private boolean isStream(Term term) {
        // ISS-2025-0472 - wave W7: recognise '$stream'(N) as well as the reserved atom aliases
        if (term instanceof it.denzosoft.jprolog.core.terms.Atom) {
            String n = ((it.denzosoft.jprolog.core.terms.Atom) term).getName();
            return "current_output".equals(n) || "user_output".equals(n) || "user_error".equals(n);
        }
        return IOStreamUtils.isStreamTerm(term);
    }
}
