package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.builtin.AbstractBuiltInWithContext;
import it.denzosoft.jprolog.core.engine.SolverContext;
import it.denzosoft.jprolog.core.engine.v4.Writer;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.PrintStream;
import java.util.List;
import java.util.Map;

// START_CHANGE: ISS-2025-0378 - print/1,2 (universal SWI/GNU/SICStus practice)
/**
 * print(+Term) / print(+Stream, +Term).
 *
 * <p>{@code write_term(Term, [portray(true), numbervars(true)])} — ISS-2025-0475 (wave W7) added
 * the {@code portray/1} hook that the first implementation documented as unsupported.
 */
public class Print extends AbstractBuiltInWithContext {

    public Print() { super(null); }

    public Print(SolverContext solver) { super(solver); }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        return executeWithContext(solver, query, bindings, solutions);
    }

    @Override
    public boolean solve(SolverContext solver, Map<String, Term> bindings) {
        Term[] args = getArguments();
        if (args.length != 1 && args.length != 2) {
            throw new PrologEvaluationException("print/1 or print/2 expected.");
        }
        PrintStream out;
        Term termToWrite;
        if (args.length == 1) {
            out = StreamManager.out();
            termToWrite = args[0];
        } else {
            out = IOStreamUtils.resolveOutputStream(args[0], bindings, "print/2");
            termToWrite = args[1];
        }
        Writer.Options o = new Writer.Options();
        o.numbervars = true;
        o.portray = true;
        o.portrayHook = WriteOptions.portrayHook(solver, bindings);
        out.print(Writer.format(termToWrite.resolveBindings(bindings), o, 1200));
        out.flush();
        return true;
    }
}
// END_CHANGE: ISS-2025-0378
