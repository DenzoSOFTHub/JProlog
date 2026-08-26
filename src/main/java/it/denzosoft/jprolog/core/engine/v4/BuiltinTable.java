package it.denzosoft.jprolog.core.engine.v4;

import java.util.HashMap;
import java.util.Map;

// START_CHANGE: ISS-2025-0443 - engine v4, design B.5 (registration by name AND arity).
/**
 * The v4 built-in table: {@code (name, arity)} -> {@link Builtin}.
 *
 * <p>The legacy {@link it.denzosoft.jprolog.core.engine.BuiltInRegistry} maps a bare name to one
 * implementation and answers {@code isBuiltIn(name, arity)} from a separate hand-maintained arity
 * table, with an "any arity" wildcard for anything not listed. v4 keys the entry itself by arity,
 * so {@code foo/2} can be native while {@code foo/3} is a user predicate, and a missing entry is
 * unambiguous.
 *
 * <p>Anything not registered here falls through to {@link LegacyBuiltinAdapter}, which runs the
 * existing ~400 {@code BuiltIn} classes unchanged.
 */
public final class BuiltinTable {

    private final Map<String, Builtin> entries = new HashMap<String, Builtin>();

    private static String key(String name, int arity) { return name + "/" + arity; }

    /** Register a native v4 built-in. */
    public void register(String name, int arity, Builtin b) { entries.put(key(name, arity), b); }

    /** Remove one (used by the sandbox). */
    public void unregister(String name, int arity) { entries.remove(key(name, arity)); }

    /** The native implementation of {@code name/arity}, or null. */
    public Builtin lookup(String name, int arity) { return entries.get(key(name, arity)); }

    public boolean isNative(String name, int arity) { return entries.containsKey(key(name, arity)); }

    // START_CHANGE: ISS-2025-0501 - the same question asked with a key the caller already built
    // (Machine.isProtectedProcedure asks the native table AND the prelude index, and both are
    // indexed by "name/arity"; building the string twice showed up on the assert/retract loop).
    /** Is {@code key} ({@code "name/arity"}) a native? */
    boolean isNativeKey(String key) { return entries.containsKey(key); }
    // END_CHANGE: ISS-2025-0501

    public int size() { return entries.size(); }
}
// END_CHANGE: ISS-2025-0443
