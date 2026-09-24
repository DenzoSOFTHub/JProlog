package it.denzosoft.jprolog.builtin.filesystem;

import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.engine.SolverContext;
import it.denzosoft.jprolog.core.engine.v4.Unify;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.List;
import java.util.Map;

// START_CHANGE: ISS-2025-0574 - 4.5 wave P3.1: loading files from Prolog.
/**
 * {@code consult/1}, {@code ensure_loaded/1}, {@code load_files/1,2}, {@code [File|Files]}
 * ({@code '.'/2}) and {@code make/0}. The work is {@link Prolog#loadFromGoal}; this class only
 * decodes the arguments and hands the engine a runner that executes the loaded file's directives
 * on the calling query's machine (its inference budget, its cancellation).
 *
 * <p>It lives in {@code builtin.filesystem} on purpose: that package is on the safe-mode deny list
 * ({@code Prolog.UNSAFE_BUILTIN_PACKAGES}), so a sandboxed engine cannot load host files.
 */
public class LoadFiles implements BuiltInWithContext {

    public enum Mode { CONSULT, ENSURE_LOADED, LOAD_FILES, LIST, MAKE, USE_MODULE }   // ISS-2025-0735: + use_module/1,2

    private final Mode mode;

    public LoadFiles(Mode mode) { this.mode = mode; }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        throw new PrologException(ISOErrorTerms.systemError("load_files needs a solver context", name()));
    }

    private String name() {
        switch (mode) {
            case CONSULT: return "consult/1";
            case ENSURE_LOADED: return "ensure_loaded/1";
            case LOAD_FILES: return "load_files/2";
            case LIST: return "'.'/2";
            case USE_MODULE: return "use_module/1";                             // ISS-2025-0735
            default: return "make/0";
        }
    }

    @Override
    public boolean executeWithContext(SolverContext solver, Term query, Map<String, Term> bindings,
                                      List<Map<String, Term>> solutions) {
        Prolog p = solver.getPrologContext();
        java.util.function.Function<Term, Boolean> runner = g -> !solver.solve(g).isEmpty();
        List<Term> args = query.getArguments();
        String ctx = name();
        switch (mode) {
            case MAKE:
                p.make(runner);
                break;
            // START_CHANGE: ISS-2025-0735 - use_module/1,2 as goals
            case USE_MODULE:
                p.useModule(args.get(0), args.size() == 2 ? args.get(1) : null, runner);
                break;
            // END_CHANGE: ISS-2025-0735
            case LIST: {
                // [F|Fs] as a goal: consult F and every file of Fs
                Term list = new CompoundTerm(new Atom("."), java.util.Arrays.asList(args.get(0), args.get(1)));
                p.loadFromGoal(list, "true", false, runner, "consult/1");
                break;
            }
            case CONSULT:
                p.loadFromGoal(args.get(0), "true", false, runner, ctx);
                break;
            case ENSURE_LOADED:
                p.loadFromGoal(args.get(0), "not_loaded", false, runner, ctx);
                break;
            case LOAD_FILES: {
                String ifMode = "true";
                boolean mustBeModule = false;
                if (args.size() == 2) {
                    Term cur = Unify.deref(args.get(1));
                    if (cur instanceof Variable) throw new PrologException(ISOErrorTerms.instantiationError(ctx));
                    while (cur instanceof CompoundTerm && ".".equals(((CompoundTerm) cur).getName())
                            && ((CompoundTerm) cur).getArguments().size() == 2) {
                        Term opt = Unify.deref(((CompoundTerm) cur).getArguments().get(0));
                        if (opt instanceof Variable) throw new PrologException(ISOErrorTerms.instantiationError(ctx));
                        if (opt instanceof CompoundTerm && ((CompoundTerm) opt).getArguments().size() == 1) {
                            Term v = Unify.deref(((CompoundTerm) opt).getArguments().get(0));
                            String on = ((CompoundTerm) opt).getName();
                            if ("if".equals(on)) {
                                if (!(v instanceof Atom) || !java.util.Arrays.asList("true", "changed", "not_loaded")
                                        .contains(((Atom) v).getName())) {
                                    throw new PrologException(ISOErrorTerms.domainError("load_files_option", opt, ctx));
                                }
                                ifMode = ((Atom) v).getName();
                            } else if ("must_be_module".equals(on)) {
                                mustBeModule = v instanceof Atom && "true".equals(((Atom) v).getName());
                            }
                            // every other option is accepted and ignored (SWI has dozens)
                        }
                        cur = Unify.deref(((CompoundTerm) cur).getArguments().get(1));
                    }
                    if (cur instanceof Variable) throw new PrologException(ISOErrorTerms.instantiationError(ctx));
                    if (!(cur instanceof Atom && "[]".equals(((Atom) cur).getName()))) {
                        throw new PrologException(ISOErrorTerms.typeError("list", args.get(1), ctx));
                    }
                }
                p.loadFromGoal(args.get(0), ifMode, mustBeModule, runner, ctx);
                break;
            }
            default:
                return false;
        }
        solutions.add(bindings);
        return true;
    }
}
// END_CHANGE: ISS-2025-0574
