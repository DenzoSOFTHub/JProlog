package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

// START_CHANGE: ISS-2025-0791 - wave Q7: the recorded database and flag/3 (SWI-Prolog 9,
// section 4.14.2 "The recorded database" and 4.14.3 "Flags").
/**
 * {@code recorda/2,3}, {@code recordz/2,3}, {@code recorded/2,3}, {@code erase/1},
 * {@code current_key/1} and {@code flag/3}.
 *
 * <p>The store belongs to the {@link Engine} and is shared by every thread of it, as SWI's is
 * shared by every thread of the process. A key is an atom, an integer or a compound term; for a
 * compound only the name and arity count (SWI). Values are stored as copies (no binding made later
 * by the caller reaches them) and every retrieval returns a fresh copy. A database reference is
 * the term {@code '$record'(N)}. {@code recorded/2,3} follows the logical update view: the records
 * of the key are snapshotted at the call, and one erased meanwhile is skipped.
 */
final class NativeRecords {

    private NativeRecords() {}

    private static final Atom REF = new Atom("$record");

    static void register(BuiltinTable t) {
        t.register("recorda", 2, new RecordB(false, 2));
        t.register("recorda", 3, new RecordB(false, 3));
        t.register("recordz", 2, new RecordB(true, 2));
        t.register("recordz", 3, new RecordB(true, 3));
        t.register("recorded", 2, new RecordedB(2));
        t.register("recorded", 3, new RecordedB(3));
        t.register("erase", 1, new EraseB());
        t.register("current_key", 1, new CurrentKeyB());
        t.register("flag", 3, new FlagB());
    }

    // ------------------------------------------------------------------ the per-engine store

    static final class Rec {
        final String key;
        final Term keyTerm;
        final Term value;
        final long id;
        volatile boolean erased;
        Rec(String key, Term keyTerm, Term value, long id) {
            this.key = key; this.keyTerm = keyTerm; this.value = value; this.id = id;
        }
    }

    /** The recorded database and the flag table of one engine. */
    static final class Store {
        private final LinkedHashMap<String, ArrayList<Rec>> byKey = new LinkedHashMap<String, ArrayList<Rec>>();
        private final Map<Long, Rec> byId = new HashMap<Long, Rec>();
        private final LinkedHashMap<String, Term> flags = new LinkedHashMap<String, Term>();
        private long nextId = 1;

        synchronized Rec add(String key, Term keyTerm, Term value, boolean atEnd) {
            Rec r = new Rec(key, keyTerm, value, nextId++);
            ArrayList<Rec> l = byKey.get(key);
            if (l == null) { l = new ArrayList<Rec>(); byKey.put(key, l); }
            if (atEnd) l.add(r); else l.add(0, r);
            byId.put(r.id, r);
            return r;
        }

        synchronized List<Rec> snapshot(String key) {
            List<Rec> out = new ArrayList<Rec>();
            if (key == null) {
                for (ArrayList<Rec> l : byKey.values()) out.addAll(l);
            } else {
                ArrayList<Rec> l = byKey.get(key);
                if (l != null) out.addAll(l);
            }
            return out;
        }

        synchronized Rec byId(long id) { return byId.get(id); }

        synchronized boolean erase(long id) {
            Rec r = byId.remove(id);
            if (r == null) return false;
            r.erased = true;
            ArrayList<Rec> l = byKey.get(r.key);
            if (l != null) {
                l.remove(r);
                if (l.isEmpty()) byKey.remove(r.key);
            }
            return true;
        }

        synchronized List<Term> keys() {
            List<Term> out = new ArrayList<Term>();
            for (ArrayList<Rec> l : byKey.values()) if (!l.isEmpty()) out.add(l.get(0).keyTerm);
            return out;
        }

        synchronized Term flag(String key) {
            Term v = flags.get(key);
            return v == null ? Number.valueOf(0) : v;
        }

        synchronized void setFlag(String key, Term v) { flags.put(key, v); }
    }

    // ------------------------------------------------------------------ helpers

    /** The key's identity string, or null for an unbound key; raises on a bad key. */
    private static String keyId(Term k, String ctx) {
        if (k instanceof Variable) return null;
        if (k instanceof Atom) return "a" + ((Atom) k).getName();
        if (k instanceof Number && ((Number) k).isInteger()) return "i" + ((Number) k).bigIntegerValue();
        if (k instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) k;
            return "c" + c.getArguments().size() + "/" + c.getName();
        }
        throw Errors.type("key", k, ctx);
    }

    /** The key as reported by recorded/3 and current_key/1: a compound keeps only name/arity. */
    private static Term keyTerm(Term k) {
        if (!(k instanceof CompoundTerm)) return k;
        CompoundTerm c = (CompoundTerm) k;
        Term[] args = new Term[c.getArguments().size()];
        for (int i = 0; i < args.length; i++) args[i] = new Variable();
        return new CompoundTerm(new Atom(c.getName()), args);
    }

    private static Term ref(Rec r) {
        return new CompoundTerm(REF, new Term[] { Number.valueOf(r.id) });
    }

    /** The record id of a reference term, -1 for a well-formed but impossible id. */
    private static long refId(Term t, String ctx) {
        if (t instanceof Variable) throw Errors.instantiation(ctx);
        if (t instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) t;
            if (c.getName().equals("$record") && c.getArguments().size() == 1) {
                Term a = Unify.deref(c.getArguments().get(0));
                if (a instanceof Number && ((Number) a).isInteger()) return ((Number) a).longValue();
            }
        }
        throw Errors.type("db_reference", t, ctx);
    }

    // ------------------------------------------------------------------ recorda / recordz

    private static final class RecordB implements Builtin {
        private final boolean atEnd;
        private final int arity;
        private final String ctx;
        RecordB(boolean atEnd, int arity) {
            this.atEnd = atEnd; this.arity = arity;
            this.ctx = (atEnd ? "recordz/" : "recorda/") + arity;
        }

        @Override
        public Outcome call(Machine m, Term[] args) {
            Term k = m.deref(args[0]);
            String key = keyId(k, ctx);
            if (key == null) throw Errors.instantiation(ctx);
            if (arity == 3 && !(m.deref(args[2]) instanceof Variable)) {
                throw Errors.type("variable", m.resolve(args[2]), ctx);   // uninstantiation
            }
            Term value = Unify.copyAcyclic(args[1], new IdentityHashMap<Variable, Variable>(), m.guard());
            if (value == null) throw Errors.representation("cyclic_term", ctx);
            Rec r = m.engine().records().add(key, keyTerm(k), value, atEnd);
            if (arity == 3) return m.unify(args[2], ref(r)) ? Outcome.SUCCESS : Outcome.FAILURE;
            return Outcome.SUCCESS;
        }
    }

    // ------------------------------------------------------------------ recorded

    private static final class RecordedB implements Builtin {
        private final int arity;
        private final String ctx;
        RecordedB(int arity) { this.arity = arity; this.ctx = "recorded/" + arity; }

        @Override
        public Outcome call(Machine m, final Term[] args) {
            Store st = m.engine().records();
            Term k = m.deref(args[0]);
            final String key = keyId(k, ctx);
            final Term pattern = arity == 3
                ? new CompoundTerm(new Atom("r"), new Term[] { args[0], args[1], args[2] })
                : new CompoundTerm(new Atom("r"), new Term[] { args[0], args[1] });
            if (arity == 3) {
                Term rt = m.deref(args[2]);
                if (!(rt instanceof Variable)) {
                    Rec r = st.byId(refId(rt, ctx));
                    if (r == null || r.erased) return Outcome.FAILURE;
                    return m.unify(pattern, instance(m, r)) ? Outcome.SUCCESS : Outcome.FAILURE;
                }
            }
            final List<Rec> recs = st.snapshot(key);
            if (recs.isEmpty()) return Outcome.FAILURE;
            final int[] i = {0};
            Generator gen = new Generator() {
                @Override public boolean next(Machine mm) {
                    while (i[0] < recs.size()) {
                        Rec r = recs.get(i[0]++);
                        if (i[0] >= recs.size()) mm.lastSolution();
                        if (r.erased || !mayMatch(args[1], r.value)) continue;
                        // no later record can match (checked BEFORE this answer binds the
                        // pattern): this answer is the last one, no choice point is left
                        int k = i[0];
                        while (k < recs.size() && !mayMatch(args[1], recs.get(k).value)) k++;
                        if (k >= recs.size()) mm.lastSolution();
                        if (mm.unifyOrUndo(pattern, instance(mm, r))) return true;
                    }
                    return false;
                }
            };
            return m.pushGenerator(gen) ? Outcome.SUSPENDED : Outcome.FAILURE;
        }

        /** A cheap pre-filter: could the value pattern unify with the stored value? */
        private boolean mayMatch(Term pattern, Term stored) {
            Term p = Unify.deref(pattern);
            if (p instanceof Variable || stored instanceof Variable) return true;
            if (p instanceof CompoundTerm) {
                return stored instanceof CompoundTerm
                    && ((CompoundTerm) p).getName().equals(((CompoundTerm) stored).getName())
                    && ((CompoundTerm) p).getArguments().size() == ((CompoundTerm) stored).getArguments().size();
            }
            if (p instanceof Atom) return stored instanceof Atom && ((Atom) p).getName().equals(((Atom) stored).getName());
            return !(stored instanceof CompoundTerm) && !(stored instanceof Atom);
        }

        private Term instance(Machine m, Rec r) {
            Term kt = m.copy(r.keyTerm);
            Term v = m.copy(r.value);
            return arity == 3
                ? new CompoundTerm(new Atom("r"), new Term[] { kt, v, ref(r) })
                : new CompoundTerm(new Atom("r"), new Term[] { kt, v });
        }
    }

    // ------------------------------------------------------------------ erase / current_key

    private static final class EraseB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            long id = refId(m.deref(args[0]), "erase/1");
            return m.engine().records().erase(id) ? Outcome.SUCCESS : Outcome.FAILURE;
        }
    }

    private static final class CurrentKeyB implements Builtin {
        @Override
        public Outcome call(Machine m, final Term[] args) {
            final List<Term> keys = m.engine().records().keys();
            if (keys.isEmpty()) return Outcome.FAILURE;
            final int[] i = {0};
            Generator gen = new Generator() {
                @Override public boolean next(Machine mm) {
                    while (i[0] < keys.size()) {
                        Term k = keys.get(i[0]++);
                        if (i[0] >= keys.size()) mm.lastSolution();
                        if (mm.unifyOrUndo(args[0], mm.copy(k))) return true;
                    }
                    return false;
                }
            };
            return m.pushGenerator(gen) ? Outcome.SUSPENDED : Outcome.FAILURE;
        }
    }

    // ------------------------------------------------------------------ flag/3

    private static final class FlagB implements Builtin {
        @Override
        public Outcome call(Machine m, Term[] args) {
            Term k = m.deref(args[0]);
            String key = keyId(k, "flag/3");
            if (key == null) throw Errors.instantiation("flag/3");
            Store st = m.engine().records();
            synchronized (st) {
                if (!m.unify(args[1], st.flag(key))) return Outcome.FAILURE;
                Term nv = m.deref(args[2]);
                Term stored;
                if (nv instanceof Atom) stored = nv;                  // SWI keeps atom values too
                else stored = m.evalNum(nv, "flag/3");
                st.setFlag(key, stored);
            }
            return Outcome.SUCCESS;
        }
    }
}
// END_CHANGE: ISS-2025-0791
