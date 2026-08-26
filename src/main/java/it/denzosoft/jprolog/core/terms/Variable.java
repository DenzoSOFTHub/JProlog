package it.denzosoft.jprolog.core.terms;

import java.util.Map;


public class Variable extends Term {

    // START_CHANGE: ISS-2025-0438 - engine v4 (design B.2): a Variable is a MUTABLE REFERENCE CELL.
    //
    // Until v3.8.0 a variable was a name and nothing else: every engine bound it through an external
    // {@code Map<String,Term>} keyed by that name, which is what made bindings unreclaimable
    // (LIM-033), variable identity string-typed and clause activation a string-allocating rename.
    // The v4 engine (core.engine.v4) binds IN the object: {@link #ref} holds the value (null when
    // unbound) and the machine's trail records the cells to reset on backtracking, so the JVM's own
    // GC reclaims every binding of a finished deterministic call.
    //
    // Two consequences for everything outside core.engine.v4:
    //   * equals/hashCode are IDENTITY. Two distinct cells are different variables even when they
    //     print the same. {@link #getName()} is still unique per cell (lazily "_G<serial>"), so the
    //     legacy name-keyed model keeps working: name equality still implies cell equality.
    //   * {@link #unify(Term, java.util.Map)} — the legacy binding API used by the ~400 registry
    //     built-ins and by the v2 engine — deliberately keeps comparing variables BY NAME,
    //     because on that path two objects sharing a name ARE the same logical variable (JpcReader
    //     and the legacy parser both build one object per occurrence).
    private static final java.util.concurrent.atomic.AtomicLong SERIAL =
        new java.util.concurrent.atomic.AtomicLong(0);

    /** Creation order, unique for the JVM. Drives conditional trailing in the v4 machine and the
     *  lazily generated {@code _G<serial>} print name. */
    public final long serial = SERIAL.incrementAndGet();

    /**
     * v4 binding cell: {@code null} = unbound, otherwise the bound value (a chain is possible).
     * Written ONLY by the v4 machine (through its trail); the legacy and v2 engines never look at
     * it, they bind through their own substitution maps.
     */
    public Term ref;

    /** The serial counter's current value: the watermark a v4 choice point records so
     *  conditional trailing can tell "older than this choice point" from "created after it". */
    public static long currentSerial() { return SERIAL.get(); }

    /** @return the bound value, or null when this cell is unbound (v4 engine). */
    public Term getRef() { return ref; }

    /** Bind this cell (v4 engine only — the caller MUST trail it). */
    public void setRef(Term value) { this.ref = value; }
    // END_CHANGE: ISS-2025-0438

    private String name;
    private final boolean isAnonymous;

    // START_CHANGE: LIM-002 - Attributed variables support
    /**
     * Lazily initialized attribute map for attributed variables.
     * Null by default for zero overhead on normal (non-attributed) variables.
     * Keys are module names, values are attribute terms.
     */
    private Map<String, Term> attributes;

    /**
     * Set an attribute on this variable for the given module.
     * @param module the module key
     * @param value the attribute value term
     */
    public void putAttribute(String module, Term value) {
        if (attributes == null) {
            // START_CHANGE: ISS-2025-0457 - insertion-ordered: the v4 wake queue runs one goal per
            // attribute module, and the order in which they run must be reproducible.
            attributes = new java.util.LinkedHashMap<>();
            // END_CHANGE: ISS-2025-0457
        }
        attributes.put(module, value);
    }

    /**
     * Get the attribute for the given module, or null if not set.
     * @param module the module key
     * @return the attribute term, or null
     */
    public Term getAttribute(String module) {
        if (attributes == null) return null;
        return attributes.get(module);
    }

    /**
     * Check if this variable has any attributes.
     * @return true if at least one attribute is set
     */
    public boolean hasAttributes() {
        return attributes != null && !attributes.isEmpty();
    }

    /**
     * Remove the attribute for the given module.
     * @param module the module key
     */
    public void removeAttribute(String module) {
        if (attributes != null) {
            attributes.remove(module);
            if (attributes.isEmpty()) {
                attributes = null;
            }
        }
    }

    /**
     * Get all attributes as an unmodifiable map. Returns empty map if no attributes.
     * @return the attributes map
     */
    public Map<String, Term> getAttributes() {
        if (attributes == null) return java.util.Collections.emptyMap();
        return java.util.Collections.unmodifiableMap(attributes);
    }
    /**
     * Callback interface for attributed variable unification hooks.
     * Called when an attributed variable is unified with a non-variable term.
     */
    public interface AttributeUnifyHook {
        /**
         * Called when an attributed variable is unified with a value.
         * @param variable the attributed variable being bound
         * @param value the term it is being unified with
         * @param substitution the current substitution map
         * @return true if the hook goals succeeded, false to fail unification
         */
        boolean onAttributeUnify(Variable variable, Term value, Map<String, Term> substitution);
    }

    /**
     * Hook installed by the engine context to intercept attributed variable unifications.
     * Null when no solver is active (zero overhead).
     */
    private static final ThreadLocal<AttributeUnifyHook> attributeUnifyHook = new ThreadLocal<>();

    /**
     * Set the attribute unification hook for the current thread.
     * @param hook the hook, or null to clear
     */
    public static void setAttributeUnifyHook(AttributeUnifyHook hook) {
        // START_CHANGE: Round5 minor - null clears the ThreadLocal entry for proper cleanup
        if (hook == null) {
            attributeUnifyHook.remove();
        } else {
            attributeUnifyHook.set(hook);
        }
        // END_CHANGE: Round5 minor
    }

    /**
     * Get the current attribute unification hook.
     * @return the hook, or null if not set
     */
    public static AttributeUnifyHook getAttributeUnifyHook() {
        return attributeUnifyHook.get();
    }
    // END_CHANGE: LIM-002

    // START_CHANGE: ISS-2025-0168 - Occurs check flag (default: false for performance)
    /**
     * Global flag controlling whether the occurs check is performed during
     * standard unification. When false (default), the occurs check is skipped
     * for performance, matching most Prolog implementations. When true, the
     * occurs check is always performed. The unify_with_occurs_check/2 built-in
     * always performs the check regardless of this flag.
     */
    // START_CHANGE: ISS-2025-0437 - ENG-06: occurs_check is PER ENGINE. It used to be a static
    // AtomicBoolean here, so set_prolog_flag(occurs_check, true) in one Prolog instance changed
    // unification for every instance in the JVM. It now lives in the engine's PrologFlags store;
    // these two methods delegate to the store current on this thread, keeping every existing call
    // site working. PrologFlags.isOccursCheckEnabled() short-circuits on a static volatile that is
    // only ever set when SOME engine turns the check on, so the (universal) off case costs one
    // volatile read and never touches the ThreadLocal — this is the unification hot path.
    /**
     * Set the occurs_check flag of the engine current on this thread.
     *
     * @param enabled true to enable occurs check in standard unification
     */
    public static void setOccursCheckEnabled(boolean enabled) {
        it.denzosoft.jprolog.core.system.PrologFlags.setOccursCheckEnabled(enabled);
    }

    /**
     * Get the occurs_check flag of the engine current on this thread.
     *
     * @return true if occurs check is enabled
     */
    public static boolean isOccursCheckEnabled() {
        return it.denzosoft.jprolog.core.system.PrologFlags.isOccursCheckEnabled();
    }
    // END_CHANGE: ISS-2025-0437
    // END_CHANGE: ISS-2025-0168

    public Variable(String name) {
        if ("_".equals(name)) {
            // Anonymous variable - each instance gets a unique name
            // ISS-2025-0438: from the SAME counter as the lazily named v4 cells, so "_G<n>" names
            // never collide between the two (a collision would alias two distinct variables on the
            // legacy name-keyed path).
            this.name = "_G" + serial;
            this.isAnonymous = true;
        } else {
            this.name = name;
            this.isAnonymous = false;
        }
    }

    // START_CHANGE: ISS-2025-0438 - a FRESH v4 cell: no name until one is asked for. Clause
    // activation allocates one of these per clause variable, which is why v4 no longer builds the
    // "_R<id>_<name>" strings (and the HashMap that held them) on every call.
    public Variable() {
        this.name = null;
        this.isAnonymous = false;
    }
    // END_CHANGE: ISS-2025-0438

    public String getName() {
        // ISS-2025-0438: materialise the print/legacy-map name on first use, and keep it, so the
        // name is stable and unique for the cell's whole life.
        if (name == null) name = "_G" + serial;
        return name;
    }

    // ISS-2025-0189: Removed setName() to enforce immutability contract
    
    public boolean isAnonymous() {
        return isAnonymous;
    }
    
    public String getDisplayName() {
        return isAnonymous ? "_" : getName();
    }

   // START_CHANGE: ISS-2025-0012 - Complete redesign with iterative dereferencing
   @Override
   public boolean unify(Term term, Map<String, Term> substitution) {
        // Dereference this variable iteratively to avoid recursion
        Term derefThis = dereferenceIterative(this, substitution);
        Term derefTerm = dereferenceIterative(term, substitution);

        // If both sides are the same after dereferencing, they unify.
        // START_CHANGE: ISS-2025-0438 - Variable.equals is IDENTITY now, but on this legacy
        // name-keyed path two Variable objects sharing a name are the SAME logical variable
        // (JpcReader and the legacy parser allocate one object per occurrence). Comparing them by
        // name here preserves the pre-v4 behaviour exactly; without it the two objects would bind
        // to each other and create a name self-loop in the substitution map.
        if (derefThis == derefTerm) {
            return true;
        }
        if (derefThis instanceof Variable && derefTerm instanceof Variable) {
            if (((Variable) derefThis).getName().equals(((Variable) derefTerm).getName())) return true;
        } else if (!(derefThis instanceof Variable) && !(derefTerm instanceof Variable)
                   && derefThis.equals(derefTerm)) {
            return true;
        }
        // END_CHANGE: ISS-2025-0438

        // If the dereferenced term is still a variable, handle variable-to-term binding
        if (derefThis instanceof Variable) {
            Variable var = (Variable) derefThis;

            // START_CHANGE: ISS-2025-0168 - Conditional occurs check based on global flag
            // Occurs check: prevent circular references (only when flag is enabled)
            if (isOccursCheckEnabled() && occursCheckIterative(var, derefTerm, substitution)) {
                return false; // Unification fails if variable occurs in the term
            }
            // END_CHANGE: ISS-2025-0168

            // Bind the variable to the term
            substitution.put(var.getName(), derefTerm);

            if (var.hasAttributes() && !(derefTerm instanceof Variable)) {
                AttributeUnifyHook hook = attributeUnifyHook.get();
                if (hook != null) {
                    if (!hook.onAttributeUnify(var, derefTerm, substitution)) {
                        substitution.remove(var.getName());
                        return false;
                    }
                }
            }
            return true;
        }

        // If dereferenced term is also a variable, bind to the non-variable side
        if (derefTerm instanceof Variable) {
            Variable var = (Variable) derefTerm;

            // START_CHANGE: ISS-2025-0168 - Conditional occurs check based on global flag
            // Occurs check: prevent circular references (only when flag is enabled)
            if (isOccursCheckEnabled() && occursCheckIterative(var, derefThis, substitution)) {
                return false; // Unification fails if variable occurs in the term
            }
            // END_CHANGE: ISS-2025-0168

            // Bind the variable to the term
            substitution.put(var.getName(), derefThis);

            // START_CHANGE: LIM-002 - Trigger attribute unification hooks
            if (var.hasAttributes() && !(derefThis instanceof Variable)) {
                AttributeUnifyHook hook = attributeUnifyHook.get();
                if (hook != null) {
                    if (!hook.onAttributeUnify(var, derefThis, substitution)) {
                        substitution.remove(var.getName());
                        return false;
                    }
                }
            }
            // END_CHANGE: LIM-002
            return true;
        }

        // Both sides are non-variables, delegate to standard unification
        return derefThis.unify(derefTerm, substitution);
   }

   // START_CHANGE: ISS-2025-0091 - Fast-path dereferencing without HashSet allocation
   /**
    * Iteratively dereference a term following substitution chains.
    * Uses a fast path for short chains (typical case: 1-3 levels) that avoids
    * HashSet allocation. Falls back to HashSet-based cycle detection only for
    * deep chains (>16 levels), which are extremely rare.
    */
   private Term dereferenceIterative(Term term, Map<String, Term> substitution) {
        Term current = term;
        int depth = 0;

        // Fast path: no allocation for typical short variable chains
        while (current instanceof Variable) {
            String varName = ((Variable) current).getName();
            Term bound = substitution.get(varName);
            if (bound == null) {
                break;
            }
            current = bound;
            if (++depth > 16) {
                // Deep chain: fall back to HashSet-based cycle detection
                return dereferenceWithCycleDetection(current, substitution);
            }
        }

        return current;
   }

   /**
    * Fallback dereference with HashSet cycle detection for deep chains.
    */
   private Term dereferenceWithCycleDetection(Term term, Map<String, Term> substitution) {
        java.util.Set<String> visited = new java.util.HashSet<>();
        Term current = term;

        while (current instanceof Variable) {
            String varName = ((Variable) current).getName();
            if (!visited.add(varName)) {
                break; // Cycle detected
            }
            Term bound = substitution.get(varName);
            if (bound == null) {
                break;
            }
            current = bound;
        }

        return current;
   }
   // END_CHANGE: ISS-2025-0091
   // END_CHANGE: ISS-2025-0012

    // START_CHANGE: ISS-2025-0012 - Implement iterative occurs check
    /**
     * Iterative occurs check to prevent infinite recursion.
     * Checks if a variable occurs within a term after dereferencing.
     */
    private boolean occursCheckIterative(Variable variable, Term term, Map<String, Term> substitution) {
        // Dereference the term first
        Term derefTerm = dereferenceIterative(term, substitution);
        
        // Now check if the variable occurs in the dereferenced term
        return occursInTerm(variable, derefTerm, substitution, new java.util.HashSet<>());
    }
    
    /**
     * Helper method to check if a variable occurs within a term structure.
     * Uses visited set to prevent infinite recursion on circular structures.
     */
    // START_CHANGE: ISS-2025-0428 - ENG-09: iterative with an explicit work stack. The recursive
    // version needed one Java frame per list cell, so the occurs check (occurs_check = true, and
    // unify_with_occurs_check/2) overflowed on lists of a few tens of thousands of elements.
    // {@code visited} is now a MARK-ON-VISIT set: a variable chain is expanded at most once, which
    // both terminates on circular chains and keeps the walk linear on shared sub-terms.
    private boolean occursInTerm(Variable variable, Term term, Map<String, Term> substitution, java.util.Set<String> visited) {
        String target = variable.getName();
        java.util.ArrayDeque<Term> work = new java.util.ArrayDeque<>();
        work.push(term);
        while (!work.isEmpty()) {
            Term t = work.pop();
            if (t instanceof Variable) {
                String n = ((Variable) t).getName();
                if (target.equals(n)) return true;
                if (!visited.add(n)) continue;                     // already expanded (or a cycle)
                Term bound = substitution.get(n);
                if (bound != null) work.push(bound);
            } else if (t instanceof CompoundTerm) {
                for (Term arg : ((CompoundTerm) t).getArguments()) work.push(arg);
            }
            // Atoms, Numbers and Strings cannot contain variables
        }
        return false;
    }
    // END_CHANGE: ISS-2025-0428
    
    // START_CHANGE: ISS-2025-0178 - Remove dead legacy occurs() stub
    // Removed unused backwards-compatibility occurs() method that just delegated to occursCheckIterative()
    // END_CHANGE: ISS-2025-0178
    // END_CHANGE: ISS-2025-0012


    @Override
    public boolean isGround() {
        return false; // Variables are never ground until they are substituted
    }

     @Override
    public String toString() {
        return getDisplayName();
    }

    // START_CHANGE: ISS-2025-0438 - B.14 identity audit: a named variable copies to ITSELF.
    //
    // copy() used to return {@code new Variable(name)} — a different object with the same name.
    // Under the name-keyed model those two objects WERE the same logical variable, which is why
    // ~10 built-ins (arg/3, member/2, nth0/nth1, select/3, aggregate_all, CollectionUtils) call
    // {@code x.copy()} and then unify with it, relying on the alias (CollectionUtils even documents
    // it). With identity variables that alias silently disappeared: {@code arg(1, f(X), A), X = a}
    // left A bound to a dead twin of X. Returning {@code this} restores the alias for BOTH models —
    // it is a no-op change for the name-keyed engines and correct for the cell model. Callers that
    // want genuinely fresh variables use copy_term/2 (TermCopier / Unify.copy), never copy().
    //
    // An ANONYMOUS variable still copies to a fresh one: that was already the behaviour (each
    // {@code _} gets its own generated name) and several built-ins depend on it.
    @Override
    public Term copy() {
        return isAnonymous ? new Variable("_") : this;
    }
    // END_CHANGE: ISS-2025-0438
    
    // START_CHANGE: ISS-2025-0091 - Fully iterative resolveBindings without HashSet
    @Override
    public Term resolveBindings(Map<String, Term> bindings) {
        // Iterative variable chain resolution - no allocation for typical chains
        Term current = bindings.get(getName());
        if (current == null) {
            return this;
        }

        int depth = 0;
        while (current instanceof Variable) {
            Term next = bindings.get(((Variable) current).getName());
            if (next == null) {
                return current; // Unbound variable at end of chain
            }
            current = next;
            if (++depth > 16) {
                // Extremely deep chain - fall back to cycle detection
                return resolveBindingsWithCycleDetection(bindings, new java.util.HashSet<>());
            }
        }

        // current is now a non-variable term - resolve its bindings too
        return current.resolveBindings(bindings);
    }
    // END_CHANGE: ISS-2025-0091

    /**
     * Resolve bindings with cycle detection to prevent infinite recursion.
     */
    private Term resolveBindingsWithCycleDetection(Map<String, Term> bindings, java.util.Set<String> visited) {
        if (!visited.add(getName())) {
            return this; // Circular reference
        }

        Term bound = bindings.get(getName());
        if (bound == null) {
            return this;
        }
        if (bound instanceof Variable) {
            return ((Variable) bound).resolveBindingsWithCycleDetection(bindings, visited);
        }
        return bound.resolveBindings(bindings);
    }
    
    // START_CHANGE: ISS-2025-0438 - IDENTITY equality (design B.2/B.14). A variable is a cell:
    // two cells are the same variable only when they are the same object. Name equality is still
    // sufficient for the legacy path because getName() is unique per cell.
    @Override
    public boolean equals(Object obj) {
        return this == obj;
    }

    @Override
    public int hashCode() {
        return System.identityHashCode(this);
    }
    // END_CHANGE: ISS-2025-0438
}
