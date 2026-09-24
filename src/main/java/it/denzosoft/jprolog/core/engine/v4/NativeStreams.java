package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.builtin.io.IOStreamUtils;
import it.denzosoft.jprolog.builtin.io.StreamManager;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * 4.6 wave Q6.3 (ISS-2025-0785, LIM-037): the stream half of the io family as v4 natives —
 * {@code open/3,4}, {@code close/1,2}, {@code stream_property/2}, {@code current_stream/3},
 * {@code set_stream/2}, {@code seek/4}, {@code set_stream_position/2}, {@code stream_position/2},
 * {@code stream_position_data/3}, {@code character_count/2}, {@code line_count/2},
 * {@code line_position/2}, {@code get_byte/1,2}, {@code peek_byte/1,2}, {@code put_byte/1,2} and
 * {@code portray_clause/1,2}.
 *
 * <p>Each is a faithful port of its {@code builtin.io} registry class (same checks, same ISO error
 * terms, same stream table), reading dereferenced cells instead of a resolved goal and a
 * {@code Map<String,Term>}. The two enumerating ones are GENERATORS: they no longer materialise
 * every (stream, property) solution map before the first answer; the candidates are the stream
 * table's entries filtered by what is bound, and the last one is announced
 * ({@link Machine#lastSolution()}), so {@code stream_property(S, alias(user_output))} stays
 * deterministic.
 */
// START_CHANGE: ISS-2025-0785 - 4.6 wave Q6.3
final class NativeStreams {

    private NativeStreams() {}

    private static final Map<String, Term> NO_BINDINGS = Collections.emptyMap();

    static void register(BuiltinTable t) {
        t.register("open", 3, new OpenB());
        t.register("open", 4, new OpenB());
        t.register("close", 1, new CloseB());
        t.register("close", 2, new CloseB());
        t.register("stream_property", 2, new StreamPropertyB());
        t.register("current_stream", 3, new CurrentStreamB());
        t.register("set_stream", 2, new SetStreamB());
        t.register("seek", 4, new SeekB());
        t.register("set_stream_position", 2, new SetStreamPositionB());
        t.register("stream_position", 2, new StreamPositionB());
        t.register("stream_position_data", 3, new PositionDataB());
        t.register("character_count", 2, new CounterB('c', "character_count/2"));
        t.register("line_count", 2, new CounterB('l', "line_count/2"));
        t.register("line_position", 2, new CounterB('p', "line_position/2"));
        t.register("get_byte", 1, new ByteInB(false));
        t.register("get_byte", 2, new ByteInB(false));
        t.register("peek_byte", 1, new ByteInB(true));
        t.register("peek_byte", 2, new ByteInB(true));
        t.register("put_byte", 1, new PutByteB());
        t.register("put_byte", 2, new PutByteB());
        t.register("portray_clause", 1, new PortrayClauseB());
        t.register("portray_clause", 2, new PortrayClauseB());
    }

    private static PrologException err(Term formal) { return new PrologException(formal); }

    private static boolean isStreamHandle(Term t) {
        return t instanceof CompoundTerm && "$stream".equals(((CompoundTerm) t).getName())
            && ((CompoundTerm) t).arity() == 1;
    }

    // ------------------------------------------------------------------ open/3,4

    private static final class OpenB implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            int arity = args.length;
            String ctx0 = "open/" + arity;
            Term fileTerm = m.deref(args[0]);
            Term modeTerm = m.deref(args[1]);
            // ISS-2025-0505: ISO 8.11.5.3 (a)/(c)/(e)
            if (fileTerm instanceof Variable || modeTerm instanceof Variable) {
                throw err(ISOErrorTerms.instantiationError(ctx0));
            }
            if (!(fileTerm instanceof Atom)) throw err(ISOErrorTerms.domainError("source_sink", m.resolve(fileTerm), ctx0));
            if (!(modeTerm instanceof Atom)) throw err(ISOErrorTerms.typeError("atom", m.resolve(modeTerm), ctx0));
            // ISS-2025-0605: (f) a bound Stream argument is uninstantiation_error(Stream)
            Term streamNow = m.deref(args[2]);
            if (!(streamNow instanceof Variable)) {
                throw err(ISOErrorTerms.error(new CompoundTerm(new Atom("uninstantiation_error"),
                    new Term[] {m.resolve(streamNow)}), new Atom(ctx0)));
            }
            String filename = ((Atom) fileTerm).getName();
            String mode = ((Atom) modeTerm).getName();
            // ISS-2025-0377
            if (!"read".equals(mode) && !"write".equals(mode) && !"append".equals(mode)) {
                throw err(ISOErrorTerms.domainError("io_mode", modeTerm, ctx0));
            }
            String aliasName = null, typeOpt = null, encodingOpt = null, eofActionOpt = null;
            Boolean repositionOpt = null;
            if (arity == 4) {
                List<Term> opts = it.denzosoft.jprolog.core.util.ListUtils.extractElements(m.resolve(args[3]));
                if (opts != null) {
                    for (Term opt : opts) {
                        if (!(opt instanceof CompoundTerm) || ((CompoundTerm) opt).arity() != 1) continue;
                        CompoundTerm c = (CompoundTerm) opt;
                        Term v = c.arg(0);
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
                if (m.unify(args[2], Streams.termFor(s))) return Outcome.SUCCESS;
                st.close(s);
                return Outcome.FAILURE;
            } catch (java.io.FileNotFoundException e) {
                if ("read".equals(mode) && !new java.io.File(filename).exists()) {
                    throw err(ISOErrorTerms.existenceError("source_sink", fileTerm, ctx0));
                }
                throw err(ISOErrorTerms.permissionError("open", "source_sink", fileTerm, ctx0));
            } catch (IOException e) {
                throw err(ISOErrorTerms.permissionError("open", "source_sink", fileTerm, ctx0));
            }
        }
    }

    // ------------------------------------------------------------------ close/1,2

    private static final class CloseB implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            String ctx = "close/" + args.length;
            Term streamTerm = m.resolve(args[0]);
            if (streamTerm instanceof Variable) throw err(ISOErrorTerms.instantiationError(ctx));
            if (IOStreamUtils.streamAlias(streamTerm) == null && !isStreamHandle(streamTerm)) {
                throw err(ISOErrorTerms.domainError("stream_or_alias", streamTerm, ctx));
            }
            boolean force = false;
            if (args.length == 2) {
                List<Term> optList = it.denzosoft.jprolog.core.util.ListUtils.extractElements(m.resolve(args[1]));
                if (optList != null) {
                    for (Term opt : optList) {
                        if (opt instanceof CompoundTerm && "force".equals(((CompoundTerm) opt).getName())
                                && ((CompoundTerm) opt).arity() == 1) {
                            Term v = ((CompoundTerm) opt).arg(0);
                            if (v instanceof Atom && "true".equals(((Atom) v).getName())) force = true;
                        }
                    }
                }
            }
            PrologStream s = StreamManager.stream(streamTerm);
            if (s != null && StreamManager.streams().close(s)) return Outcome.SUCCESS;
            if (force) return Outcome.SUCCESS;
            if (s == null) throw err(ISOErrorTerms.existenceError("stream", streamTerm, ctx));
            throw err(ISOErrorTerms.permissionError("close", "stream", streamTerm, ctx));   // ISS-2025-0697
        }
    }

    // ------------------------------------------------------------------ stream_property/2

    /**
     * Could {@code prop} match {@code pattern} (dereferenced)? A cheap structural pre-filter: the
     * same principal functor and equal atomic arguments where the pattern has them; the real test
     * is the unification.
     */
    private static boolean mayMatch(Term pattern, Term prop) {
        if (pattern instanceof Variable) return true;
        if (pattern instanceof Atom) return prop instanceof Atom && ((Atom) pattern).getName().equals(((Atom) prop).getName());
        if (!(pattern instanceof CompoundTerm) || !(prop instanceof CompoundTerm)) return false;
        CompoundTerm p = (CompoundTerm) pattern, q = (CompoundTerm) prop;
        if (p.arity() != q.arity() || !p.getName().equals(q.getName())) return false;
        for (int i = 0; i < p.arity(); i++) {
            Term a = Unify.deref(p.arg(i)), b = q.arg(i);
            if (a instanceof Atom && !(b instanceof Atom && ((Atom) a).getName().equals(((Atom) b).getName()))) return false;
            if (a instanceof Number && !(b instanceof Number && a.equals(b))) return false;
        }
        return true;
    }

    /** Unify every pair (a[i], b[i]) in ONE extent: all or nothing (invariant 12). */
    private static boolean unifyAll(Machine m, Term[] a, Term[] b, int n) {
        Bindings bs = m.bindings();
        int mark = bs.mark();
        bs.forceTrail++;
        try {
            for (int i = 0; i < n; i++) {
                if (!Unify.unify(a[i], b[i], bs)) { bs.undo(mark); return false; }
            }
            return true;
        } finally {
            bs.forceTrail--;
        }
    }

    private static final class StreamPropertyB implements Builtin {
        @Override public Outcome call(Machine m, final Term[] args) {
            Term streamTerm = m.deref(args[0]);
            Term propertyTerm = m.deref(args[1]);
            if (!(propertyTerm instanceof Variable) && !isStreamProperty(propertyTerm)) {
                throw err(ISOErrorTerms.domainError("stream_property", m.resolve(propertyTerm), "stream_property/2"));
            }
            final boolean bindStream = streamTerm instanceof Variable;
            List<PrologStream> streams;
            if (bindStream) {
                streams = StreamManager.streams().all();
            } else {
                PrologStream s = StreamManager.stream(m.resolve(streamTerm));
                if (s == null) throw err(ISOErrorTerms.existenceError("stream", m.resolve(streamTerm), "stream_property/2"));
                streams = Collections.singletonList(s);
            }
            // the candidate (stream, property) pairs, filtered by what the pattern binds
            final List<Term[]> cands = new ArrayList<Term[]>();
            for (PrologStream s : streams) {
                Term sv = null;
                for (Term p : it.denzosoft.jprolog.builtin.io.StreamProperty.propertiesOf(s)) {
                    if (!mayMatch(propertyTerm, p)) continue;
                    if (sv == null) sv = Streams.termFor(s);
                    cands.add(new Term[] {sv, p});
                }
            }
            if (cands.isEmpty()) return Outcome.FAILURE;
            final int[] i = {0};
            return m.pushGenerator(new Generator() {
                @Override public boolean next(Machine mm) {
                    while (i[0] < cands.size()) {
                        Term[] c = cands.get(i[0]++);
                        if (i[0] >= cands.size()) mm.lastSolution();
                        boolean ok = bindStream
                            ? unifyAll(mm, new Term[] {args[0], args[1]}, c, 2)
                            : unifyAll(mm, new Term[] {args[1]}, new Term[] {c[1]}, 1);
                        if (ok) return true;
                    }
                    return false;
                }
            }) ? Outcome.SUSPENDED : Outcome.FAILURE;
        }
    }

    private static final java.util.Set<String> UNARY_PROPERTIES = new java.util.HashSet<String>(java.util.Arrays.asList(
        "file_name", "mode", "alias", "position", "end_of_stream", "eof_action", "reposition",
        "type", "encoding", "line_count", "buffer", "buffer_size", "bom", "close_on_abort",
        "newline", "representation_errors", "timeout", "tty", "file_no", "locale", "nlink",
        "write_errors", "close_on_exec"));

    private static boolean isStreamProperty(Term p) {
        if (p instanceof Atom) return "input".equals(((Atom) p).getName()) || "output".equals(((Atom) p).getName());
        return p instanceof CompoundTerm && ((CompoundTerm) p).arity() == 1
            && UNARY_PROPERTIES.contains(((CompoundTerm) p).getName());
    }

    // ------------------------------------------------------------------ current_stream/3

    private static final class CurrentStreamB implements Builtin {
        @Override public Outcome call(Machine m, final Term[] args) {
            Term fileT = m.deref(args[0]), modeT = m.deref(args[1]), streamT = m.deref(args[2]);
            PrologStream only = (streamT instanceof Variable) ? null : StreamManager.stream(m.resolve(streamT));
            final List<Term[]> cands = new ArrayList<Term[]>();
            for (PrologStream s : StreamManager.streams().all()) {
                if (s.fileName() == null) continue;             // SWI: only file streams
                if (only != null && only != s) continue;
                if (!(streamT instanceof Variable) && only == null) continue;   // unknown stream: no match
                Term f = new Atom(s.fileName()), md = new Atom(s.mode());
                if (fileT instanceof Atom && !((Atom) fileT).getName().equals(s.fileName())) continue;
                if (modeT instanceof Atom && !((Atom) modeT).getName().equals(s.mode())) continue;
                cands.add(new Term[] {f, md, Streams.termFor(s)});
            }
            if (cands.isEmpty()) return Outcome.FAILURE;
            final boolean bindStream = only == null;
            final int[] i = {0};
            return m.pushGenerator(new Generator() {
                @Override public boolean next(Machine mm) {
                    while (i[0] < cands.size()) {
                        Term[] c = cands.get(i[0]++);
                        if (i[0] >= cands.size()) mm.lastSolution();
                        if (unifyAll(mm, args, c, bindStream ? 3 : 2)) return true;
                    }
                    return false;
                }
            }) ? Outcome.SUSPENDED : Outcome.FAILURE;
        }
    }

    // ------------------------------------------------------------------ set_stream/2 and friends

    private static PrologStream streamArg(Machine m, Term t, String ctx) {
        Term st = m.resolve(t);
        if (st instanceof Variable) throw err(ISOErrorTerms.instantiationError(ctx));
        PrologStream s = StreamManager.stream(st);
        if (s == null) throw err(ISOErrorTerms.existenceError("stream", st, ctx));
        return s;
    }

    private static final class SetStreamB implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            PrologStream s = streamArg(m, args[0], "set_stream/2");
            Term prop = m.resolve(args[1]);
            if (prop instanceof Variable) throw err(ISOErrorTerms.instantiationError("set_stream/2"));
            if (!(prop instanceof CompoundTerm) || ((CompoundTerm) prop).arity() != 1) {
                throw err(ISOErrorTerms.domainError("stream_property", prop, "set_stream/2"));
            }
            Term v = ((CompoundTerm) prop).arg(0);
            String value = (v instanceof Atom) ? ((Atom) v).getName() : null;
            String name = ((CompoundTerm) prop).getName();
            if ("alias".equals(name)) {
                if (value == null) throw err(ISOErrorTerms.typeError("atom", v, "set_stream/2"));
                StreamManager.streams().addAlias(s, value);
            } else if ("type".equals(name) || "eof_action".equals(name) || "encoding".equals(name)) {
                if (value == null) throw err(ISOErrorTerms.typeError("atom", v, "set_stream/2"));
                Streams.setProperty(s, name, value);
            } else {
                throw err(ISOErrorTerms.domainError("stream_property", prop, "set_stream/2"));
            }
            return Outcome.SUCCESS;
        }
    }

    private static final class PositionDataB implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            Term field = m.deref(args[0]);
            Term pos = m.resolve(args[1]);
            if (field instanceof Variable) throw err(ISOErrorTerms.instantiationError("stream_position_data/3"));
            if (!(pos instanceof CompoundTerm) || !"$stream_position".equals(((CompoundTerm) pos).getName())
                    || ((CompoundTerm) pos).arity() != 4) {
                throw err(ISOErrorTerms.domainError("stream_position", pos, "stream_position_data/3"));
            }
            String f = (field instanceof Atom) ? ((Atom) field).getName() : "";
            CompoundTerm p = (CompoundTerm) pos;
            Term value;
            if ("char_count".equals(f)) value = p.arg(0);
            else if ("line_count".equals(f)) value = p.arg(1);
            else if ("line_position".equals(f)) value = p.arg(2);
            else if ("byte_count".equals(f)) value = p.arg(3);
            else throw err(ISOErrorTerms.domainError("stream_position_field", m.resolve(field), "stream_position_data/3"));
            return m.unify(args[2], value) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    private static final class CounterB implements Builtin {
        private final char which;
        private final String ind;
        CounterB(char which, String ind) { this.which = which; this.ind = ind; }
        @Override public Outcome call(Machine m, Term[] args) {
            PrologStream s = streamArg(m, args[0], ind);
            long v = (which == 'c') ? s.charCount() : (which == 'l') ? s.lineCount() : s.linePosition();
            return m.unify(args[1], Number.valueOf(v)) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    // ------------------------------------------------------------------ seek/4, positions

    private static final class SeekB implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            Term streamTerm = m.resolve(args[0]);
            Term offsetTerm = m.resolve(args[1]);
            Term methodTerm = m.resolve(args[2]);
            if (!(offsetTerm instanceof Number) || !((Number) offsetTerm).isInteger()) {
                throw err(ISOErrorTerms.typeError("integer", offsetTerm, "seek/4"));
            }
            if (!(methodTerm instanceof Atom)) throw err(ISOErrorTerms.typeError("atom", methodTerm, "seek/4"));
            long offset = ((Number) offsetTerm).longValue();
            String method = ((Atom) methodTerm).getName();
            PrologStream s = StreamManager.stream(streamTerm);
            if (s == null) throw err(ISOErrorTerms.existenceError("stream", streamTerm, "seek/4"));
            if (!s.canReposition()) throw err(ISOErrorTerms.permissionError("reposition", "stream", streamTerm, "seek/4"));
            try {
                long newPos;
                if ("bof".equals(method)) newPos = offset;
                else if ("current".equals(method)) newPos = s.bytePosition() + offset;
                else if ("eof".equals(method)) newPos = s.size() + offset;
                else throw err(ISOErrorTerms.domainError("seek_method", methodTerm, "seek/4"));
                if (newPos < 0) throw err(ISOErrorTerms.domainError("position", new Number(newPos), "seek/4"));
                s.reposition(newPos);
                return m.unify(args[3], new Number(newPos)) ? Outcome.SUCCESS : Outcome.FAILURE;
            } catch (PrologException pe) {
                throw pe;
            } catch (Exception e) {
                it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
                throw Errors.host(e, "reposition", "stream", null, "seek", 4);    // ISS-2025-0697
            }
        }
    }

    private static final class SetStreamPositionB implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            Term streamTerm = m.resolve(args[0]);
            Term positionTerm = m.resolve(args[1]);
            String ctx = "set_stream_position/2";
            PrologStream s = StreamManager.stream(streamTerm);
            if (s == null) throw err(ISOErrorTerms.existenceError("stream", streamTerm, ctx));
            long position;
            if (positionTerm instanceof Number) {
                position = ((Number) positionTerm).longValue();
            } else if (positionTerm instanceof CompoundTerm
                    && "$stream_position".equals(((CompoundTerm) positionTerm).getName())
                    && ((CompoundTerm) positionTerm).arity() == 4) {
                Term b = ((CompoundTerm) positionTerm).arg(3);
                if (!(b instanceof Number)) throw err(ISOErrorTerms.domainError("stream_position", positionTerm, ctx));
                position = ((Number) b).longValue();
            } else {
                throw err(ISOErrorTerms.domainError("stream_position", positionTerm, ctx));
            }
            if (position < 0) throw err(ISOErrorTerms.domainError("stream_position", positionTerm, ctx));   // ISS-2025-0697
            if (!s.canReposition()) throw err(ISOErrorTerms.permissionError("reposition", "stream", streamTerm, ctx));
            try {
                s.reposition(position);
                return Outcome.SUCCESS;
            } catch (PrologException pe) {
                throw pe;
            } catch (Exception e) {
                it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
                throw Errors.host(e, "reposition", "stream", null, "set_stream_position", 2);
            }
        }
    }

    private static final class StreamPositionB implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            Term streamTerm = m.resolve(args[0]);
            PrologStream s = StreamManager.stream(streamTerm);
            if (s == null) throw err(ISOErrorTerms.existenceError("stream", streamTerm, "stream_position/2"));
            if (!s.canReposition()) {
                throw err(ISOErrorTerms.permissionError("reposition", "stream", streamTerm, "stream_position/2"));
            }
            return m.unify(args[1], new Number(s.bytePosition())) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    // ------------------------------------------------------------------ byte I/O

    private static final class ByteInB implements Builtin {
        private final boolean peek;
        ByteInB(boolean peek) { this.peek = peek; }
        @Override public Outcome call(Machine m, Term[] args) {
            int arity = args.length;
            String name = peek ? "peek_byte" : "get_byte";
            String ctx = name + "/" + arity;
            Term streamArg = (arity == 2) ? m.resolve(args[0]) : null;
            PrologStream s = IOStreamUtils.inputStream(streamArg, NO_BINDINGS, ctx);
            IOStreamUtils.checkStreamType(s, true, "input", ctx);
            IOStreamUtils.beforeRead(s, ctx);
            try {
                int b = peek ? s.peekByte() : s.getByte();
                return m.unify(args[arity - 1], Number.valueOf((long) b)) ? Outcome.SUCCESS : Outcome.FAILURE;
            } catch (IOException e) {
                throw Errors.host(e, "read", "stream", null, name, arity);   // ISS-2025-0697
            }
        }
    }

    private static final class PutByteB implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            int arity = args.length;
            String ctx = "put_byte/" + arity;
            Term byteTerm = m.resolve(args[arity - 1]);
            if (byteTerm instanceof Variable) throw err(ISOErrorTerms.instantiationError(ctx));
            if (!(byteTerm instanceof Number) || !((Number) byteTerm).isInteger()) {
                throw err(ISOErrorTerms.typeError("byte", byteTerm, ctx));
            }
            int byteValue = ((Number) byteTerm).getValue().intValue();
            if (byteValue < 0 || byteValue > 255) throw err(ISOErrorTerms.typeError("byte", byteTerm, ctx));
            Term streamArg = (arity == 2) ? m.resolve(args[0]) : null;
            PrologStream s = IOStreamUtils.outputStream(streamArg, NO_BINDINGS, ctx);
            try {
                if (s.rawOutput() != null) {
                    s.rawOutput().write(byteValue);
                    s.rawOutput().flush();
                } else {
                    StreamManager.streams().writerFor(s).write(byteValue);
                    StreamManager.streams().writerFor(s).flush();
                }
                return Outcome.SUCCESS;
            } catch (IOException e) {
                throw Errors.host(e, "write", "stream", null, "put_byte", arity);   // ISS-2025-0697
            }
        }
    }

    // ------------------------------------------------------------------ portray_clause/1,2

    private static final class PortrayClauseB implements Builtin {
        @Override public Outcome call(Machine m, Term[] args) {
            PrintStream out;
            Term clause;
            if (args.length == 1) {
                out = StreamManager.out();
                clause = args[0];
            } else {
                out = IOStreamUtils.resolveOutputStream(m.resolve(args[0]), NO_BINDINGS, "portray_clause/2");
                clause = args[1];
            }
            // the clause is resolved as the registry version received it: portray_clause names
            // the variables of that copy (A, B, ... and _ for a singleton)
            out.print(Writer.portrayClause(m.resolve(clause), null));
            out.flush();
            return Outcome.SUCCESS;
        }
    }
}
// END_CHANGE: ISS-2025-0785
