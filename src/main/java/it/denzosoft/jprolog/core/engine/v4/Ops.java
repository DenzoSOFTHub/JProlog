package it.denzosoft.jprolog.core.engine.v4;

import it.denzosoft.jprolog.core.operator.Operator;
import it.denzosoft.jprolog.core.operator.OperatorTable;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

// START_CHANGE: ISS-2025-0474 - engine v4 wave W7 (design B.12): ONE operator store per engine,
// scoped by module. It replaces the three process-global stores of LIM-034 —
// OperatorDefinition.OPERATORS, OperatorDefinition.OP_MODULE and
// OperatorDefinition.sharedOperatorTable — with a single object hanging off the engine, read by the
// parser, current_op/3, the writer, the .jpc writer and the IDE formatter. That is what makes
// current_op/3 see the operators a consulted file declared with `:- op/3` (it saw none before,
// because Prolog.processOpDirective only ever touched the parser's OperatorTable).
/**
 * The operator store of one engine.
 *
 * <p>One {@link OperatorTable} — the object {@code Prolog.getOperatorTable()} returns, which the
 * parser, {@code write_term/2,3}, the {@code .jpc} writer and the IDE formatter all read — plus an
 * <b>ownership</b> map recording which module declared each non-standard operator. An {@code op/3}
 * executed while a module file is being consulted is <b>local to that module</b> for
 * {@code current_op/3}: another module does not see it (ISO 13211-2 / SWI). It is still installed
 * in the shared parser table, because JProlog consults a whole session into one operator space and
 * every already-consulted clause was read with it — narrowing that would change how existing
 * programs parse, which is a cross-engine decision, not a W7 one (see the wave's deviations).
 *
 * <p>Thread-safety: the ownership map is concurrent and {@code OperatorTable} is already backed by
 * concurrent maps; operators change at consult time only.
 */
public final class Ops {

    /** One operator definition, as {@code current_op/3} reports it. */
    public static final class Def {
        public final int precedence;
        public final String type;    // fx fy xfx xfy yfx xf yf
        public final String name;
        public final String module;  // the module it was declared in ("user" == global)

        Def(int precedence, String type, String name, String module) {
            this.precedence = precedence;
            this.type = type;
            this.name = name;
            this.module = module;
        }

        @Override
        public String toString() {
            return "op(" + precedence + ", " + type + ", " + name + ")@" + module;
        }
    }

    /** The engine-global table: the standard ISO operators plus everything {@code op/3} declared. */
    private final OperatorTable global = new OperatorTable();

    /**
     * {@code name:class -> module} for operators declared inside a module file. Absent means the
     * operator is global (module {@code user} / {@code system}).
     *
     * <p>Note that a module-local operator IS installed in the shared parser table (that is what the
     * engine has always done and what already-consulted files depend on); what is module-scoped is
     * its <b>visibility</b> to {@code current_op/3}, which is the observable the ISO/SWI rule is
     * about and what {@code RefactorIssuesTest.testR2_operatorLocalToModule} pins.
     */
    private final Map<String, String> owner = new ConcurrentHashMap<String, String>();

    // START_CHANGE: ISS-2025-0748 - 4.6 wave Q4 (extra): the module an op/3 is attributed to is
    // PER THREAD, like the module being loaded (ModuleManager's per-thread current module since
    // ISS-2025-0739). It was one engine-wide field, so a thread loading a module file made an
    // op/3 run at the same moment by another thread module-local to that module.
    private final ThreadLocal<String> moduleContext = new ThreadLocal<String>();
    // END_CHANGE: ISS-2025-0748

    /** The store of the engine current on this thread. */
    public static Ops current() { return EngineState.current().ops(); }

    /** The engine-global operator table (the one the parser and the {@code .jpc} writer take). */
    public OperatorTable globalTable() { return global; }

    /** The module an {@code op/3} is currently attributed to; {@code "user"} means global. */
    public String moduleContext() {
        String m = moduleContext.get();                                       // ISS-2025-0748
        return (m == null) ? "user" : m;
    }

    /** Set by {@code Prolog} when a {@code :- module/2} directive is consulted (this thread only). */
    public void setModuleContext(String module) {
        if (module == null || module.isEmpty() || "user".equals(module)) moduleContext.remove();   // ISS-2025-0748
        else moduleContext.set(module);
    }

    /** True when {@code module} is the global scope. */
    private static boolean isGlobal(String module) {
        return module == null || "user".equals(module) || "system".equals(module);
    }

    /** The table the parser and the writer use in {@code module}. */
    public OperatorTable tableFor(String module) { return global; }

    /** The table for the module currently in context — what the parser and the writer read. */
    public OperatorTable table() { return global; }

    // ------------------------------------------------------------------
    // op/3
    // ------------------------------------------------------------------

    /**
     * Define (or, with {@code precedence == 0}, remove) an operator in the module currently in
     * context. Returns an undo action so the caller can record it on the backtracking trail
     * ({@code op/3} under a choice point must be undone on failure — R1).
     */
    public Runnable define(final int precedence, final String type, final String name) {
        return define(precedence, type, name, moduleContext());             // ISS-2025-0748
    }

    /** Define/remove an operator explicitly in {@code module}. */
    public Runnable define(final int precedence, final String type, final String name, final String module) {
        final String mod = (module == null || module.isEmpty()) ? "user" : module;
        final String k = key(name, type);
        final List<Operator> previous = sameClass(global, type, name);
        final String prevOwner = owner.get(k);
        Runnable undo = new Runnable() {
            public void run() {
                replaceClass(global, type, name);
                for (Operator op : previous) global.defineOperator(op.getPrecedence(), op.getType(), op.getName());
                if (prevOwner == null) owner.remove(k); else owner.put(k, prevOwner);
            }
        };
        replaceClass(global, type, name);
        if (precedence == 0) {
            owner.remove(k);
        } else {
            global.defineOperator(precedence, Operator.parseType(type), name);
            if (isGlobal(mod)) owner.remove(k); else owner.put(k, mod);
        }
        return undo;
    }

    // ------------------------------------------------------------------
    // char_conversion/2 and current_char_conversion/2  (ISS-2025-0500)
    // ------------------------------------------------------------------

    // START_CHANGE: ISS-2025-0500 - 4.2 wave C: the character-conversion table is per ENGINE, like
    // the operator table next to it. It used to be a `static final ConcurrentHashMap` inside
    // {@code builtin.system.CharConversion}, so two Prolog instances in one JVM shared it and a
    // conversion declared under a choice point was never undone. Both defects are the same one the
    // operator store fixed in W7; this is the same fix, in the same object, because both are
    // read-time syntax state of one engine.
    private final Map<Character, Character> charConversions = new ConcurrentHashMap<Character, Character>();

    /**
     * Declare {@code from -> to} ({@code from == to} removes the entry, ISO 8.14.5). Returns the
     * undo action, so a {@code char_conversion/2} executed under a choice point can be rolled back
     * with the rest of the trail.
     */
    public Runnable convert(final char from, final char to) {
        final Character prev = charConversions.get(Character.valueOf(from));
        Runnable undo = new Runnable() {
            public void run() {
                if (prev == null) charConversions.remove(Character.valueOf(from));
                else charConversions.put(Character.valueOf(from), prev);
            }
        };
        if (from == to) charConversions.remove(Character.valueOf(from));
        else charConversions.put(Character.valueOf(from), Character.valueOf(to));
        return undo;
    }

    /** What {@code c} converts to; {@code c} itself when no conversion is declared. */
    public char converted(char c) {
        Character t = charConversions.get(Character.valueOf(c));
        return (t == null) ? c : t.charValue();
    }

    /** The declared (non-identity) conversions, in declaration-independent key order. */
    public Map<Character, Character> conversions() {
        return new java.util.TreeMap<Character, Character>(charConversions);
    }
    // END_CHANGE: ISS-2025-0500

    // ------------------------------------------------------------------
    // current_op/3 and the lookup facade
    // ------------------------------------------------------------------

    /** Every operator visible in the module currently in context, in a stable order. */
    public List<Def> visible() { return visibleIn(moduleContext()); }

    /**
     * Every operator visible in {@code module}: the global ones plus the ones that module declared,
     * and none of the ones another module declared.
     */
    public List<Def> visibleIn(String module) {
        String ctx = (module == null || module.isEmpty()) ? "user" : module;
        List<Def> defs = new ArrayList<Def>();
        for (Operator op : global.getCurrentOperators()) {
            String t = op.getType().name().toLowerCase();
            String o = owner.get(key(op.getName(), t));
            if (o != null && !o.equals(ctx)) continue;
            defs.add(new Def(op.getPrecedence(), t, op.getName(), o == null ? "user" : o));
        }
        Collections.sort(defs, new Comparator<Def>() {
            public int compare(Def a, Def b) {
                int c = Integer.compare(a.precedence, b.precedence);
                if (c != 0) return c;
                c = a.type.compareTo(b.type);
                if (c != 0) return c;
                return a.name.compareTo(b.name);
            }
        });
        return defs;
    }

    /** The prefix operator visible for {@code name}, or null. */
    public Def prefix(String name) { return def(global.getPrefixOperator(name)); }

    /** The infix operator visible for {@code name}, or null. */
    public Def infix(String name) { return def(global.getInfixOperator(name)); }

    /** The postfix operator visible for {@code name}, or null. */
    public Def postfix(String name) { return def(global.getPostfixOperator(name)); }

    /** Infix first, then prefix, then postfix — the historical {@code getOperator} order. */
    public Def any(String name) {
        Def d = infix(name);
        if (d != null) return d;
        d = prefix(name);
        if (d != null) return d;
        return postfix(name);
    }

    /** True when {@code name} is an operator of any class. */
    public boolean isDefined(String name) { return any(name) != null; }

    /** Every visible operator keyed by {@code name:class} (the historical map shape). */
    public Map<String, Def> all() {
        Map<String, Def> out = new LinkedHashMap<String, Def>();
        for (Def d : visible()) out.put(key(d.name, d.type), d);
        return out;
    }

    private Def def(Operator op) {
        if (op == null) return null;
        String t = op.getType().name().toLowerCase();
        String o = owner.get(key(op.getName(), t));
        return new Def(op.getPrecedence(), t, op.getName(), o == null ? "user" : o);
    }

    // ------------------------------------------------------------------
    // helpers
    // ------------------------------------------------------------------

    /** The composite key {@code name:class} — one definition per (name, prefix/infix/postfix). */
    public static String key(String name, String specifier) { return name + ":" + typeClass(specifier); }

    /** prefix / infix / postfix for an operator specifier. */
    public static String typeClass(String specifier) {
        if (specifier == null) return "infix";
        String s = specifier.toLowerCase();
        if ("fx".equals(s) || "fy".equals(s)) return "prefix";
        if ("xf".equals(s) || "yf".equals(s)) return "postfix";
        return "infix";
    }

    private static List<Operator> sameClass(OperatorTable t, String type, String name) {
        List<Operator> out = new ArrayList<Operator>();
        String cls = typeClass(type);
        for (Operator op : t.getOperators(name)) {
            if (cls.equals(classOf(op))) out.add(op);
        }
        return out;
    }

    private static String classOf(Operator op) {
        if (op.isPrefix()) return "prefix";
        if (op.isPostfix()) return "postfix";
        return "infix";
    }

    /**
     * Drop every definition of {@code name} in the same class as {@code type}. ISO allows one
     * definition per (name, class); {@code OperatorTable.defineOperator} would otherwise keep both
     * the old and the new precedence in its set and {@code current_op/3} would report two answers.
     */
    private static void replaceClass(OperatorTable t, String type, String name) {
        for (Operator op : sameClass(t, type, name)) {
            t.removeOperator(op.getPrecedence(), op.getType(), name);
        }
    }
}
// END_CHANGE: ISS-2025-0474
