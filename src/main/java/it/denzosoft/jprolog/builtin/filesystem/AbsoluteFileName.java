package it.denzosoft.jprolog.builtin.filesystem;

import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.FileSearch;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.engine.SolverContext;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

// START_CHANGE: ISS-2025-0737 - 4.6 wave Q3.3: absolute_file_name/2,3 with the SWI file search
/**
 * {@code absolute_file_name(+Spec, -Abs)} and {@code absolute_file_name(+Spec, -Abs, +Options)}:
 * {@code Alias(Path)} specifications through {@code file_search_path/2} and the built-in
 * defaults, the options {@code extensions/1}, {@code file_type/1}, {@code access/1},
 * {@code relative_to/1}, {@code solutions/1} and {@code file_errors/1} (see {@link FileSearch}).
 * In {@code builtin.filesystem}, so safe mode denies it.
 */
public class AbsoluteFileName implements BuiltInWithContext {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        return executeWithContext(null, query, bindings, solutions);
    }

    @Override
    public boolean executeWithContext(SolverContext solver, Term query, Map<String, Term> bindings,
                                      List<Map<String, Term>> solutions) {
        List<Term> args = query.getArguments();
        String ctx = "absolute_file_name/" + args.size();
        FileSearch.Options o = args.size() == 3 ? FileSearch.parseOptions(args.get(2), ctx) : new FileSearch.Options();
        String base = it.denzosoft.jprolog.core.engine.v4.EngineState.current().workingDirectory();   // ISS-2025-0745
        Prolog p = solver != null ? solver.getPrologContext() : null;
        if (p != null) {
            Prolog.LoadContext lc = p.currentLoad();
            if (lc != null && lc.directory() != null) base = lc.directory();
        }
        final SolverContext sv = solver;
        List<String> found = FileSearch.resolve(args.get(0), o,
            sv == null ? null : g -> sv.solve(g), base, ctx);
        if (found.isEmpty()) {
            if (!o.errors) return false;
            throw new PrologException(ISOErrorTerms.existenceError(
                "directory".equals(o.fileType) ? "directory" : "source_sink", args.get(0), ctx));
        }
        boolean any = false;
        for (String f : found) {
            Map<String, Term> nb = new HashMap<>(bindings);
            if (args.get(1).resolveBindings(bindings).unify(new Atom(f), nb)) {
                solutions.add(nb);
                any = true;
                if (!o.all) break;
            }
        }
        return any;
    }
}
// END_CHANGE: ISS-2025-0737
