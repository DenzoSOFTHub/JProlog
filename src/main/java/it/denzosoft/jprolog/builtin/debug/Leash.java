// START_CHANGE: ISS-2025-0172 - Leash built-in predicate for debug port filtering
package it.denzosoft.jprolog.builtin.debug;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Term;

import java.util.*;

/**
 * Implementation of leash/1 predicate.
 *
 * leash(+Ports)
 *
 * Controls which ports of the 4-port debug model (call, exit, fail, redo)
 * trigger a pause during debugging.
 *
 * Ports can be:
 * - A list of port atoms: [call, exit, fail, redo]
 * - The atom 'full' for all ports
 * - The atom 'none' for no ports
 * - The atom 'half' for [call, redo] (common SWI-Prolog convention)
 * - The atom 'loose' for [call] only
 * - The atom 'tight' for [call, redo, fail, exit] (same as full)
 *
 * Examples:
 * ?- leash(full).
 * true.
 *
 * ?- leash([call, fail]).
 * true.
 *
 * ?- leash(none).
 * true.
 */
public class Leash implements BuiltIn {

    // Global leash state (mirrors DebugController's leashedPorts for standalone use)
    private static final Set<String> leashedPorts = new HashSet<>(Arrays.asList("CALL", "EXIT", "FAIL", "REDO"));

    private static final Set<String> ALL_PORTS = new HashSet<>(Arrays.asList("CALL", "EXIT", "FAIL", "REDO"));

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments() == null || query.getArguments().size() != 1) {
            throw new PrologEvaluationException("leash/1 requires exactly 1 argument");
        }

        Term arg = query.getArguments().get(0).resolveBindings(bindings);

        Set<String> newPorts = new HashSet<>();

        if (arg instanceof Atom) {
            String atomName = ((Atom) arg).getName();
            switch (atomName) {
                case "full":
                case "tight":
                    newPorts.addAll(ALL_PORTS);
                    break;
                case "none":
                    // empty set
                    break;
                case "half":
                    newPorts.add("CALL");
                    newPorts.add("REDO");
                    break;
                case "loose":
                    newPorts.add("CALL");
                    break;
                case "call":
                    newPorts.add("CALL");
                    break;
                case "exit":
                    newPorts.add("EXIT");
                    break;
                case "fail":
                    newPorts.add("FAIL");
                    break;
                case "redo":
                    newPorts.add("REDO");
                    break;
                default:
                    throw new PrologEvaluationException(
                        "leash/1: unknown port or mode '" + atomName + "'. Use full, none, half, loose, or a list of [call, exit, fail, redo].");
            }
        } else if (arg instanceof CompoundTerm) {
            // Parse as a Prolog list of port atoms
            Term current = arg;
            while (current instanceof CompoundTerm) {
                CompoundTerm ct = (CompoundTerm) current;
                if (".".equals(ct.getName()) && ct.getArguments() != null && ct.getArguments().size() == 2) {
                    Term head = ct.getArguments().get(0).resolveBindings(bindings);
                    if (head instanceof Atom) {
                        String portName = ((Atom) head).getName().toUpperCase();
                        if (ALL_PORTS.contains(portName)) {
                            newPorts.add(portName);
                        } else {
                            throw new PrologEvaluationException(
                                "leash/1: invalid port '" + ((Atom) head).getName() + "'. Valid ports: call, exit, fail, redo.");
                        }
                    } else {
                        throw new PrologEvaluationException("leash/1: port list elements must be atoms");
                    }
                    current = ct.getArguments().get(1).resolveBindings(bindings);
                } else {
                    throw new PrologEvaluationException("leash/1: argument must be a port mode atom or a list of port atoms");
                }
            }
            // current should be [] (empty list / atom)
        } else {
            throw new PrologEvaluationException("leash/1: argument must be an atom or list of port atoms");
        }

        // Update global leash state
        leashedPorts.clear();
        leashedPorts.addAll(newPorts);

        // Print feedback
        if (newPorts.isEmpty()) {
            System.out.println("% Leashing set to none");
        } else {
            System.out.println("% Leashing set to " + newPorts);
        }

        solutions.add(bindings);
        return true;
    }

    /**
     * Get the current set of leashed ports.
     */
    public static Set<String> getLeashedPorts() {
        return new HashSet<>(leashedPorts);
    }

    /**
     * Check if a specific port is leashed.
     */
    public static boolean isPortLeashed(String portName) {
        return leashedPorts.contains(portName.toUpperCase());
    }

    /**
     * Set leash programmatically (used by DebugController integration).
     */
    public static void setLeashedPorts(Set<String> ports) {
        leashedPorts.clear();
        for (String p : ports) {
            leashedPorts.add(p.toUpperCase());
        }
    }

    /**
     * Reset to default (all ports leashed).
     */
    public static void resetToDefault() {
        leashedPorts.clear();
        leashedPorts.addAll(ALL_PORTS);
    }
}
// END_CHANGE: ISS-2025-0172
