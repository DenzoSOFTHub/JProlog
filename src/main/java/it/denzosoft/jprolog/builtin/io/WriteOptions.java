package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.core.engine.SolverContext;
import it.denzosoft.jprolog.core.engine.v4.Writer;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import it.denzosoft.jprolog.core.util.ListUtils;

import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

// START_CHANGE: ISS-2025-0475 - engine v4 wave W7 (design B.12): the write_term/2,3 option parser
// and the portray/1 hook, shared by write_term, print, portray_clause and the console.
/**
 * Parses the ISO {@code write_term/2,3} option list into {@link Writer.Options} and builds the
 * {@code portray/1} hook.
 */
public final class WriteOptions {

    private WriteOptions() { }

    /**
     * Parse an option list. Unknown options raise {@code domain_error(write_option, O)};
     * a partial list or an unbound option raises {@code instantiation_error}.
     */
    public static Writer.Options parse(Term optionsTerm, Map<String, Term> bindings,
                                       SolverContext solver, String context) {
        Writer.Options o = new Writer.Options();
        o.numbervars = false;                    // ISO default for write_term/2,3
        Term t = (optionsTerm == null) ? new Atom("[]") : optionsTerm.resolveBindings(bindings);
        if (t instanceof Variable) throw new PrologException(ISOErrorTerms.instantiationError(context));
        // START_CHANGE: ISS-2025-0505 - 4.3 wave D. ListUtils.extractElements never returns null:
        // it stops at the first non-cons and hands back what it has, so the type_error(list, T)
        // below was dead code and write_term(a, foo) / write_term(a, [quoted(true)|_]) silently
        // wrote with default options. ISO 8.14.2.3 (b)/(c): a partial list is instantiation_error,
        // anything that is neither a partial list nor a list is type_error(list, Options).
        List<Term> opts = new java.util.ArrayList<Term>();
        Term cur = t;
        while (cur instanceof CompoundTerm && ".".equals(cur.getName())
                && cur.getArguments() != null && cur.getArguments().size() == 2) {
            opts.add(cur.getArguments().get(0));
            cur = cur.getArguments().get(1).resolveBindings(bindings);
        }
        if (cur instanceof Variable) throw new PrologException(ISOErrorTerms.instantiationError(context));
        if (!(cur instanceof Atom) || !"[]".equals(((Atom) cur).getName())) {
            throw new PrologException(ISOErrorTerms.typeError("list", t, context));
        }
        // END_CHANGE: ISS-2025-0505
        for (Term raw : opts) {
            Term opt = raw.resolveBindings(bindings);
            if (opt instanceof Variable) throw new PrologException(ISOErrorTerms.instantiationError(context));
            if (!(opt instanceof CompoundTerm) || opt.getArguments() == null
                    || opt.getArguments().size() != 1) {
                throw new PrologException(ISOErrorTerms.domainError("write_option", opt, context));
            }
            String name = opt.getName();
            Term v = opt.getArguments().get(0).resolveBindings(bindings);
            switch (name) {
                case "quoted":      o.quoted = isTrue(v, opt, context); break;
                case "ignore_ops":  o.ignoreOps = isTrue(v, opt, context); break;
                case "numbervars":  o.numbervars = isTrue(v, opt, context); break;
                case "portray":     o.portray = isTrue(v, opt, context); break;
                case "cycles":      o.cycles = isTrue(v, opt, context); break;
                case "max_depth":
                    if (!(v instanceof Number) || !((Number) v).isInteger()) {
                        throw new PrologException(ISOErrorTerms.domainError("write_option", opt, context));
                    }
                    o.maxDepth = (int) ((Number) v).longValue();
                    break;
                case "spacing":
                    if (v instanceof Atom && "next_argument".equals(((Atom) v).getName())) {
                        o.spacingNextArgument = true;
                    } else if (!(v instanceof Atom) || !"standard".equals(((Atom) v).getName())) {
                        throw new PrologException(ISOErrorTerms.domainError("write_option", opt, context));
                    }
                    break;
                case "variable_names":
                    o.variableNames = parseVariableNames(v, bindings);
                    break;
                case "attributes":
                case "priority":
                case "partial":
                case "blobs":
                case "character_escapes":
                    break;                        // accepted and ignored
                default:
                    throw new PrologException(ISOErrorTerms.domainError("write_option", opt, context));
            }
        }
        if (o.portray && solver != null) o.portrayHook = portrayHook(solver, bindings);
        return o;
    }

    private static boolean isTrue(Term v, Term opt, String context) {
        if (v instanceof Atom) {
            String n = ((Atom) v).getName();
            if ("true".equals(n)) return true;
            if ("false".equals(n)) return false;
        }
        throw new PrologException(ISOErrorTerms.domainError("write_option", opt, context));
    }

    private static IdentityHashMap<Variable, String> parseVariableNames(Term v, Map<String, Term> bindings) {
        IdentityHashMap<Variable, String> names = new IdentityHashMap<>();
        List<Term> pairs = ListUtils.extractElements(v);
        if (pairs == null) return names;
        for (Term p : pairs) {
            Term pair = p.resolveBindings(bindings);
            if (!(pair instanceof CompoundTerm) || !"=".equals(pair.getName())
                    || pair.getArguments().size() != 2) continue;
            Term nameT = pair.getArguments().get(0).resolveBindings(bindings);
            Term varT = pair.getArguments().get(1).resolveBindings(bindings);
            if (nameT instanceof Atom && varT instanceof Variable) {
                names.put((Variable) varT, ((Atom) nameT).getName());
            }
        }
        return names;
    }

    /**
     * The {@code portray/1} hook: runs the user's {@code portray/1} on a subterm and returns what it
     * printed, or null when there is no {@code portray/1} or it fails. Output is captured through
     * the thread-local override, so nothing reaches the console (LIM-025).
     */
    public static Writer.Portray portrayHook(final SolverContext solver, final Map<String, Term> bindings) {
        if (solver == null || solver.getKnowledgeBase() == null) return null;
        List<it.denzosoft.jprolog.core.engine.Rule> rules =
            solver.getKnowledgeBase().getRulesForPredicate("portray", 1);
        if (rules == null || rules.isEmpty()) return null;
        return new Writer.Portray() {
            @Override
            public String portray(Term t) {
                java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
                java.io.PrintStream capture = new java.io.PrintStream(baos, true);
                java.io.PrintStream prev = StreamManager.threadLocalOutput();
                try {
                    StreamManager.setThreadLocalOutput(capture);
                    List<Map<String, Term>> sols = new java.util.ArrayList<>();
                    boolean ok = solver.solveMeta(   // ISS-2025-0485
                        new CompoundTerm(new Atom("portray"), java.util.Arrays.asList(t)),
                        new java.util.HashMap<>(bindings), sols);
                    capture.flush();
                    if (ok && baos.size() > 0) return baos.toString();
                } catch (RuntimeException e) {
                    it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);
                } finally {
                    StreamManager.setThreadLocalOutput(prev);
                }
                return null;
            }
        };
    }
}
// END_CHANGE: ISS-2025-0475
