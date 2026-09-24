package it.denzosoft.jprolog.builtin.os;

// START_CHANGE: ISS-2025-0116 - OS/System built-in predicates
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.engine.v4.Errors;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * OS/System predicates:
 *   shell/1          - shell(+Command)                 execute shell command
 *   shell/2          - shell(+Command, -ExitCode)      execute and get exit code
 *   shell_output/3   - shell_output(+Command, -Output, -ExitCode)
 *   getenv/2         - getenv(+VarName, -Value)
 *   hostname/1       - hostname(-Name)
 *   pid/1            - pid(-ProcessId)
 *   sleep/1          - sleep(+Seconds)
 *   system_time/1    - system_time(-MillisEpoch)
 *   os_name/1        - os_name(-Name)
 *   cpu_count/1      - cpu_count(-N)
 *   free_memory/1    - free_memory(-Bytes)
 *   total_memory/1   - total_memory(-Bytes)
 */
public class OsPredicates implements BuiltIn {

    public enum Mode {
        SHELL, SHELL2, SHELL_OUTPUT,
        SETENV, UNSETENV,                                   // ISS-2025-0718
        GETENV, HOSTNAME, PID, SLEEP, SYSTEM_TIME,
        OS_NAME, CPU_COUNT, FREE_MEMORY, TOTAL_MEMORY
    }

    private final Mode mode;

    public OsPredicates(Mode mode) {
        this.mode = mode;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        try {
            switch (mode) {
                // ISS-2025-0718: shell/2 (documented, SWI) is the same name as shell/1
                case SHELL:        return doShell(query, bindings, solutions, arityOf(query) == 2);
                case SETENV:       return doSetenv(query, bindings, solutions, true);
                case UNSETENV:     return doSetenv(query, bindings, solutions, false);
                case SHELL2:       return doShell(query, bindings, solutions, true);
                case SHELL_OUTPUT: return doShellOutput(query, bindings, solutions);
                case GETENV:       return doGetenv(query, bindings, solutions);
                case HOSTNAME:     return doHostname(query, bindings, solutions);
                case PID:          return doPid(query, bindings, solutions);
                case SLEEP:        return doSleep(query, bindings, solutions);
                case SYSTEM_TIME:  return doSystemTime(query, bindings, solutions);
                case OS_NAME:      return doOsName(query, bindings, solutions);
                case CPU_COUNT:    return doCpuCount(query, bindings, solutions);
                case FREE_MEMORY:  return doFreeMemory(query, bindings, solutions);
                case TOTAL_MEMORY: return doTotalMemory(query, bindings, solutions);
                default: return false;
            }
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            throw Errors.host(e, "execute", "process", null, modeName(), arityOf(query));   // ISS-2025-0689
        }
    }

    private boolean doShell(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions, boolean returnCode) throws Exception {
        int expectedArity = returnCode ? 2 : 1;
        checkArity(query, expectedArity);
        String cmd = resolveAtom(query, 0, bindings);
        Process p = Runtime.getRuntime().exec(new String[]{"/bin/sh", "-c", cmd}, childEnv());   // ISS-2025-0718
        int exitCode = p.waitFor();
        if (returnCode) {
            return unify(query.getArguments().get(1), new Number(exitCode), bindings, solutions);
        }
        if (exitCode == 0) { solutions.add(bindings); return true; }
        return false;
    }

    private boolean doShellOutput(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws Exception {
        checkArity(query, 3);
        String cmd = resolveAtom(query, 0, bindings);
        Process p = Runtime.getRuntime().exec(new String[]{"/bin/sh", "-c", cmd}, childEnv());   // ISS-2025-0718
        String output = readStream(p.getInputStream());
        int exitCode = p.waitFor();
        Map<String, Term> nb = new HashMap<>(bindings);
        if (query.getArguments().get(1).resolveBindings(bindings).unify(new Atom(output), nb) &&
            query.getArguments().get(2).resolveBindings(nb).unify(new Number(exitCode), nb)) {
            solutions.add(nb);
            return true;
        }
        return false;
    }

    private boolean doGetenv(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 2);
        String varName = resolveAtom(query, 0, bindings);
        // START_CHANGE: ISS-2025-0718 - setenv/2 / unsetenv/1 overrides come first
        String value;
        if (ENV.containsKey(varName)) {
            value = ENV.get(varName);
            if (UNSET.equals(value)) return false;
        } else {
            value = System.getenv(varName);
        }
        // END_CHANGE: ISS-2025-0718
        if (value == null) return false;
        return unify(query.getArguments().get(1), new Atom(value), bindings, solutions);
    }

    // START_CHANGE: ISS-2025-0718 - wave Q2.8: setenv/2 and unsetenv/1 (documented, SWI). A JVM
    // cannot change its own process environment, so the change is a process-wide OVERLAY that
    // getenv/2 reads and that shell/1,2 and shell_output/3 pass to their child processes — which
    // is what the documented use ("configure the environment for child processes") needs.
    private static final java.util.concurrent.ConcurrentHashMap<String, String> ENV =
        new java.util.concurrent.ConcurrentHashMap<String, String>();
    private static final String UNSET = new String("\u0000unset");   // identity marker

    private boolean doSetenv(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions, boolean set) {
        checkArity(query, set ? 2 : 1);
        String name = resolveAtom(query, 0, bindings);
        if (set) {
            Term v = query.getArguments().get(1).resolveBindings(bindings);
            if (v instanceof it.denzosoft.jprolog.core.terms.Variable) {
                throw Errors.instantiation(modeName(), 2, "the value must be bound");
            }
            String text = (v instanceof Atom) ? ((Atom) v).getName()
                : (v instanceof it.denzosoft.jprolog.core.terms.PrologString)
                    ? ((it.denzosoft.jprolog.core.terms.PrologString) v).getStringValue()
                : (v instanceof Number) ? v.toString() : null;
            if (text == null) throw Errors.type("text", v, modeName(), 2, "the value must be text or a number");
            ENV.put(name, text);
        } else {
            ENV.put(name, UNSET);
        }
        solutions.add(bindings);
        return true;
    }

    /** The environment of a child process: the JVM's plus the overlay; null when unchanged. */
    private static String[] childEnv() {
        if (ENV.isEmpty()) return null;
        Map<String, String> env = new HashMap<String, String>(System.getenv());
        for (Map.Entry<String, String> e : ENV.entrySet()) {
            if (UNSET.equals(e.getValue())) env.remove(e.getKey()); else env.put(e.getKey(), e.getValue());
        }
        String[] out = new String[env.size()];
        int i = 0;
        for (Map.Entry<String, String> e : env.entrySet()) out[i++] = e.getKey() + "=" + e.getValue();
        return out;
    }
    // END_CHANGE: ISS-2025-0718

    private boolean doHostname(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws Exception {
        checkArity(query, 1);
        String hostname = java.net.InetAddress.getLocalHost().getHostName();
        return unify(query.getArguments().get(0), new Atom(hostname), bindings, solutions);
    }

    private boolean doPid(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 1);
        long pid = ProcessHandle.current().pid();
        return unify(query.getArguments().get(0), new Number(pid), bindings, solutions);
    }

    private boolean doSleep(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws InterruptedException {
        checkArity(query, 1);
        Term secTerm = query.getArguments().get(0).resolveBindings(bindings);
        // START_CHANGE: ISS-2025-0689
        if (secTerm instanceof it.denzosoft.jprolog.core.terms.Variable) throw Errors.instantiation("sleep", 1, "the time must be bound");
        if (!(secTerm instanceof Number)) throw Errors.type("number", secTerm, "sleep", 1, "the time must be a number");
        // END_CHANGE: ISS-2025-0689
        long millis = (long) (((Number) secTerm).getValue() * 1000);
        Thread.sleep(millis);
        solutions.add(bindings);
        return true;
    }

    private boolean doSystemTime(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 1);
        return unify(query.getArguments().get(0), new Number(System.currentTimeMillis()), bindings, solutions);
    }

    private boolean doOsName(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 1);
        return unify(query.getArguments().get(0), new Atom(System.getProperty("os.name")), bindings, solutions);
    }

    private boolean doCpuCount(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 1);
        return unify(query.getArguments().get(0),
            new Number(Runtime.getRuntime().availableProcessors()), bindings, solutions);
    }

    private boolean doFreeMemory(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 1);
        return unify(query.getArguments().get(0),
            new Number(Runtime.getRuntime().freeMemory()), bindings, solutions);
    }

    private boolean doTotalMemory(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 1);
        return unify(query.getArguments().get(0),
            new Number(Runtime.getRuntime().totalMemory()), bindings, solutions);
    }

    private String readStream(InputStream is) throws IOException {
        StringBuilder sb = new StringBuilder();
        try (BufferedReader br = new BufferedReader(new InputStreamReader(is, StandardCharsets.UTF_8))) {
            String line;
            while ((line = br.readLine()) != null) {
                if (sb.length() > 0) sb.append('\n');
                sb.append(line);
            }
        }
        return sb.toString();
    }


    // START_CHANGE: ISS-2025-0689 - wave Q1.1: ISO error terms error(Formal, context(Name/Arity, Msg)),
    // not message atoms (LIM-038)
    private static int arityOf(Term query) {
        return query.getArguments() == null ? 0 : query.getArguments().size();
    }

    /** Unreachable through the registry since ISS-2025-0685 (exact arities); kept for direct calls. */
    private void checkArity(Term query, int expected) {
        int n = arityOf(query);
        if (n != expected) throw Errors.existence("procedure", Errors.pi(modeName(), n), modeName(), n, null);
    }

    /** Argument {@code i} as text: an atom (or a string); unbound is an instantiation error. */
    private String resolveAtom(Term query, int i, Map<String, Term> bindings) {
        int n = arityOf(query);
        Term resolved = query.getArguments().get(i).resolveBindings(bindings);
        if (resolved instanceof it.denzosoft.jprolog.core.terms.Variable) {
            throw Errors.instantiation(modeName(), n, "argument " + (i + 1) + " must be bound");
        }
        if (resolved instanceof it.denzosoft.jprolog.core.terms.PrologString) {
            return ((it.denzosoft.jprolog.core.terms.PrologString) resolved).getStringValue();
        }
        if (!(resolved instanceof Atom)) {
            throw Errors.type("atom", resolved, modeName(), n, "argument " + (i + 1) + " must be an atom");
        }
        return ((Atom) resolved).getName();
    }
    // END_CHANGE: ISS-2025-0689

    private boolean unify(Term target, Term value, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        Map<String, Term> nb = new HashMap<>(bindings);
        if (target.resolveBindings(bindings).unify(value, nb)) { solutions.add(nb); return true; }
        return false;
    }

    private String modeName() { return mode.name().toLowerCase(); }
}
// END_CHANGE: ISS-2025-0116
