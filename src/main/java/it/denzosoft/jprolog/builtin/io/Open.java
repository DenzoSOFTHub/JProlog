package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.v4.PrologStream;
import it.denzosoft.jprolog.core.engine.v4.Streams;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.util.ListUtils;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * open/3 - open(+File, +Mode, -Stream)
 * open/4 - open(+File, +Mode, -Stream, +Options)
 *
 * <p>Options: {@code alias(Name)}, {@code type(text|binary)}, {@code encoding(E)},
 * {@code eof_action(error|eof_code|reset)}, {@code reposition(true|false)}.
 *
 * <p>START_CHANGE: ISS-2025-0472 - wave W7 (design B.11): the stream is opened in the ENGINE's own
 * table and {@code Stream} is unified with the canonical term {@code '$stream'(N)}; atom aliases
 * (including the {@code stream_<id>} handle and any {@code alias(Name)}) keep working everywhere a
 * stream argument is accepted.
 */
public class Open implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        int arity = query.getArguments().size();
        if (arity != 3 && arity != 4) {
            throw new PrologEvaluationException("open/3 or open/4 expected.");
        }

        Term fileTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term modeTerm = query.getArguments().get(1).resolveBindings(bindings);
        Term streamTerm = query.getArguments().get(2);

        // START_CHANGE: ISS-2025-0505 - 4.3 wave D: ISO 8.11.5.3 (a)/(c)/(e). An unbound source
        // or mode is instantiation_error and a non-atom one is a type/domain error; both used to
        // raise a PrologEvaluationException carrying an English sentence, which catch/3 could only
        // match with a bare variable catcher.
        String ctx0 = "open/" + arity;
        if (fileTerm instanceof it.denzosoft.jprolog.core.terms.Variable
                || modeTerm instanceof it.denzosoft.jprolog.core.terms.Variable) {
            throw new PrologException(ISOErrorTerms.instantiationError(ctx0));
        }
        if (!(fileTerm instanceof Atom)) {
            throw new PrologException(ISOErrorTerms.domainError("source_sink", fileTerm, ctx0));
        }
        if (!(modeTerm instanceof Atom)) {
            throw new PrologException(ISOErrorTerms.typeError("atom", modeTerm, ctx0));
        }
        // END_CHANGE: ISS-2025-0505
        // START_CHANGE: ISS-2025-0605 - P4.12: ISO 8.11.5.3 (f): a bound Stream argument is
        // uninstantiation_error(Stream); open(F, read, s) used to fail silently.
        Term streamNow = streamTerm.resolveBindings(bindings);
        if (!(streamNow instanceof it.denzosoft.jprolog.core.terms.Variable)) {
            throw new PrologException(ISOErrorTerms.error(
                new it.denzosoft.jprolog.core.terms.CompoundTerm(new Atom("uninstantiation_error"),
                    java.util.Collections.singletonList(streamNow)), new Atom(ctx0)));
        }
        // END_CHANGE: ISS-2025-0605

        String filename = ((Atom) fileTerm).getName();
        String mode = ((Atom) modeTerm).getName();

        // START_CHANGE: ISS-2025-0377 - an invalid mode is error(domain_error(io_mode, Mode), _) (8.11.5.3)
        if (!"read".equals(mode) && !"write".equals(mode) && !"append".equals(mode)) {
            throw new PrologException(ISOErrorTerms.domainError("io_mode", modeTerm, "open/" + arity));
        }
        // END_CHANGE: ISS-2025-0377

        String aliasName = null;
        String typeOpt = null;
        String encodingOpt = null;
        String eofActionOpt = null;
        Boolean repositionOpt = null;
        if (arity == 4) {
            Term optsTerm = query.getArguments().get(3).resolveBindings(bindings);
            List<Term> opts = ListUtils.extractElements(optsTerm);
            if (opts != null) {
                for (Term opt : opts) {
                    if (!(opt instanceof CompoundTerm)) continue;
                    CompoundTerm c = (CompoundTerm) opt;
                    if (c.getArguments() == null || c.getArguments().size() != 1) continue;
                    Term v = c.getArguments().get(0).resolveBindings(bindings);
                    if (!(v instanceof Atom)) continue;
                    String vs = ((Atom) v).getName();
                    switch (c.getName()) {
                        case "alias": aliasName = vs; break;
                        case "type": typeOpt = vs; break;
                        case "encoding": encodingOpt = vs; break;
                        case "eof_action": eofActionOpt = vs; break;
                        case "reposition": repositionOpt = Boolean.valueOf("true".equals(vs)); break;
                        default: break;
                    }
                }
            }
        }

        try {
            Streams st = StreamManager.streams();
            PrologStream s = st.open(filename, mode, aliasName, typeOpt, encodingOpt, eofActionOpt, repositionOpt);
            if (aliasName != null) st.addAlias(s, aliasName);

            Term resolvedStreamTerm = streamTerm.resolveBindings(bindings);
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (resolvedStreamTerm.unify(Streams.termFor(s), newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            st.close(s);
            return false;
        // START_CHANGE: ISS-2025-0377 - ISO error/2 terms (8.11.5.3) instead of plain-atom balls
        } catch (java.io.FileNotFoundException e) {
            String ctx = "open/" + arity;
            if ("read".equals(mode) && !new java.io.File(filename).exists()) {
                throw new PrologException(ISOErrorTerms.existenceError("source_sink", fileTerm, ctx));
            }
            throw new PrologException(ISOErrorTerms.permissionError("open", "source_sink", fileTerm, ctx));
        } catch (IOException e) {
            throw new PrologException(
                ISOErrorTerms.permissionError("open", "source_sink", fileTerm, "open/" + arity));
        }
        // END_CHANGE: ISS-2025-0377
    }
}
