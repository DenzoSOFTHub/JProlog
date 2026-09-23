package it.denzosoft.jprolog.builtin.system;

import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.engine.SolverContext;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

// START_CHANGE: ISS-2025-0576 - 4.5 wave P3.1: source_file/1,2 and prolog_load_context/2.
/**
 * What the loader knows, read-only: {@code source_file(?File)} (the files loaded, absolute paths),
 * {@code source_file(?Head, ?File)} (the user predicates a file defined) and
 * {@code prolog_load_context(?Key, ?Value)} for the keys {@code module}, {@code file},
 * {@code source}, {@code directory} and {@code dialect} — only while a load is running.
 */
public class SourceFiles implements BuiltInWithContext {

    public enum Mode { SOURCE_FILE, LOAD_CONTEXT }

    private final Mode mode;

    public SourceFiles(Mode mode) { this.mode = mode; }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new PrologException(ISOErrorTerms.systemError("needs a solver context", "source_file/1"));
    }

    @Override
    public boolean executeWithContext(SolverContext solver, Term query, Map<String, Term> bindings,
                                      List<Map<String, Term>> solutions) {
        Prolog p = solver.getPrologContext();
        List<Term> args = query.getArguments();
        if (mode == Mode.SOURCE_FILE && args.size() == 1) {
            for (String f : p.loadedSourceFiles()) add(solutions, bindings, args.get(0), new Atom(f));
            return !solutions.isEmpty();
        }
        if (mode == Mode.SOURCE_FILE) {
            for (Map.Entry<String, Set<String>> e : p.loadedSourcePredicates().entrySet()) {
                for (String pi : e.getValue()) {
                    int slash = pi.lastIndexOf('/');
                    String name = pi.substring(0, slash);
                    int arity = Integer.parseInt(pi.substring(slash + 1));
                    Term head;
                    if (arity == 0) {
                        head = new Atom(name);
                    } else {
                        List<Term> vs = new ArrayList<>(arity);
                        for (int i = 0; i < arity; i++) vs.add(new Variable());
                        head = new CompoundTerm(new Atom(name), vs);
                    }
                    Map<String, Term> b = new HashMap<>(bindings);
                    if (args.get(0).unify(head, b) && args.get(1).unify(new Atom(e.getKey()), b)) solutions.add(b);
                }
            }
            return !solutions.isEmpty();
        }
        Prolog.LoadContext lc = p.currentLoad();
        if (lc == null) return false;
        Map<String, Term> kv = new LinkedHashMap<>();
        kv.put("module", new Atom(p.getModuleManager().getCurrentModule().getName()));
        if (lc.file() != null) kv.put("file", new Atom(lc.file()));
        if (lc.source() != null) kv.put("source", new Atom(lc.source()));
        if (lc.directory() != null) kv.put("directory", new Atom(lc.directory()));
        kv.put("dialect", new Atom("swi"));
        for (Map.Entry<String, Term> e : kv.entrySet()) {
            Map<String, Term> b = new HashMap<>(bindings);
            if (args.get(0).unify(new Atom(e.getKey()), b) && args.get(1).unify(e.getValue(), b)) solutions.add(b);
        }
        return !solutions.isEmpty();
    }

    private static void add(List<Map<String, Term>> solutions, Map<String, Term> bindings, Term arg, Term value) {
        Map<String, Term> b = new HashMap<>(bindings);
        if (arg.unify(value, b)) solutions.add(b);
    }
}
// END_CHANGE: ISS-2025-0576
