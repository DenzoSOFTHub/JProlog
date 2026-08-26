package it.denzosoft.jprolog.builtin.os;

// START_CHANGE: ISS-2025-0116 - OS/System built-in predicates
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

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
                case SHELL:        return doShell(query, bindings, solutions, false);
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
            throw new PrologEvaluationException(modeName() + ": " + e.getMessage());
        }
    }

    private boolean doShell(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions, boolean returnCode) throws Exception {
        int expectedArity = returnCode ? 2 : 1;
        checkArity(query, expectedArity);
        String cmd = resolveAtom(query.getArguments().get(0), bindings);
        Process p = Runtime.getRuntime().exec(new String[]{"/bin/sh", "-c", cmd});
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
        String cmd = resolveAtom(query.getArguments().get(0), bindings);
        Process p = Runtime.getRuntime().exec(new String[]{"/bin/sh", "-c", cmd});
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
        String varName = resolveAtom(query.getArguments().get(0), bindings);
        String value = System.getenv(varName);
        if (value == null) return false;
        return unify(query.getArguments().get(1), new Atom(value), bindings, solutions);
    }

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
        if (!(secTerm instanceof Number)) throw new PrologEvaluationException("sleep/1: argument must be a number.");
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

    private void checkArity(Term query, int expected) {
        if (query.getArguments().size() != expected)
            throw new PrologEvaluationException(modeName() + " requires " + expected + " arguments.");
    }

    private String resolveAtom(Term term, Map<String, Term> bindings) {
        Term resolved = term.resolveBindings(bindings);
        if (!(resolved instanceof Atom)) throw new PrologEvaluationException(modeName() + ": argument must be an atom.");
        return ((Atom) resolved).getName();
    }

    private boolean unify(Term target, Term value, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        Map<String, Term> nb = new HashMap<>(bindings);
        if (target.resolveBindings(bindings).unify(value, nb)) { solutions.add(nb); return true; }
        return false;
    }

    private String modeName() { return mode.name().toLowerCase(); }
}
// END_CHANGE: ISS-2025-0116
