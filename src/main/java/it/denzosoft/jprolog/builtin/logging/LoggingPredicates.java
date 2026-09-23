package it.denzosoft.jprolog.builtin.logging;

// START_CHANGE: ISS-2025-0121 - Logging built-in predicates
import it.denzosoft.jprolog.builtin.io.StreamManager;
import it.denzosoft.jprolog.core.engine.BuiltInWithContext;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.engine.SolverContext;
import it.denzosoft.jprolog.core.engine.v4.Errors;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.PrologString;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;

/**
 * Logging predicates:
 *   log_info/1      - log_info(+Message)
 *   log_warning/1   - log_warning(+Message)
 *   log_error/1     - log_error(+Message)
 *   log_debug/1     - log_debug(+Message)
 *   log_level/1     - log_level(+Level)  set level: debug, info, warning, error, off, all
 *   log_to_file/1   - log_to_file(+FilePath)  append the log to a file (host access: denied in
 *                     safe mode)
 *
 * <p>START_CHANGE: ISS-2025-0626 - 4.5 wave P6.1: the log is PER ENGINE. It used to be the static
 * JVM-wide {@code java.util.logging} logger "JProlog": {@code log_to_file/1} in one engine
 * redirected the logging of every engine in the JVM (a sandboxed one included), and
 * {@code log_level/1} changed everybody's level. Each {@link Prolog} now has its own level and
 * sink — the engine's {@code user_error} stream, or the file named by {@code log_to_file/1} — kept
 * in a weak map keyed by the engine. The argument errors are ISO terms. END_CHANGE: ISS-2025-0626
 */
public class LoggingPredicates implements BuiltInWithContext {

    public enum Mode { LOG_INFO, LOG_WARNING, LOG_ERROR, LOG_DEBUG, LOG_LEVEL, LOG_TO_FILE }

    private static final int DEBUG = 0, INFO = 1, WARNING = 2, ERROR = 3, OFF = 4;
    private static final String[] NAMES = { "DEBUG", "INFO", "WARNING", "ERROR" };

    /** One engine's log: its level and its file sink (null = the engine's user_error). */
    private static final class LogState {
        int level = INFO;
        String file;
    }

    private static final Map<Prolog, LogState> STATES = new WeakHashMap<>();
    private static final LogState NO_ENGINE = new LogState();
    private static final DateTimeFormatter TS = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

    private final Mode mode;

    public LoggingPredicates(Mode mode) {
        this.mode = mode;
    }

    private static LogState state(SolverContext solver) {
        Prolog p = (solver == null) ? null : solver.getPrologContext();
        if (p == null) return NO_ENGINE;
        synchronized (STATES) {
            LogState s = STATES.get(p);
            if (s == null) { s = new LogState(); STATES.put(p, s); }
            return s;
        }
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        return executeWithContext(null, query, bindings, solutions);
    }

    @Override
    public boolean executeWithContext(SolverContext solver, Term query, Map<String, Term> bindings,
                                      List<Map<String, Term>> solutions) {
        LogState st = state(solver);
        Term arg = query.getArguments().get(0).resolveBindings(bindings);
        String ctx = modeName() + "/1";
        switch (mode) {
            case LOG_DEBUG:   log(st, DEBUG, text(arg, ctx)); break;
            case LOG_INFO:    log(st, INFO, text(arg, ctx)); break;
            case LOG_WARNING: log(st, WARNING, text(arg, ctx)); break;
            case LOG_ERROR:   log(st, ERROR, text(arg, ctx)); break;
            case LOG_LEVEL:   setLevel(st, arg, ctx); break;
            case LOG_TO_FILE: setFile(st, arg, ctx); break;
            default: return false;
        }
        solutions.add(new HashMap<>(bindings));
        return true;
    }

    private static void log(LogState st, int level, String message) {
        int threshold;
        String file;
        synchronized (st) { threshold = st.level; file = st.file; }
        if (level < threshold) return;
        String line = LocalDateTime.now().format(TS) + " " + NAMES[level] + ": " + message;
        if (file != null) {
            synchronized (st) {
                try (Writer w = new OutputStreamWriter(new FileOutputStream(file, true), StandardCharsets.UTF_8)) {
                    w.write(line);
                    w.write(System.lineSeparator());
                } catch (IOException e) {
                    throw Errors.permission("open", "source_sink", new Atom(file), "log");
                }
            }
        } else {
            PrintStream err = StreamManager.resolveOutput("user_error");
            if (err == null) err = StreamManager.out();
            err.println(line);
            err.flush();
        }
    }

    private static void setLevel(LogState st, Term arg, String ctx) {
        String s = atom(arg, ctx);
        int level;
        switch (s.toLowerCase()) {
            case "debug":   level = DEBUG; break;
            case "all":     level = DEBUG; break;
            case "info":    level = INFO; break;
            case "warning": level = WARNING; break;
            case "error":   level = ERROR; break;
            case "off":     level = OFF; break;
            default: throw Errors.domain("log_level", arg, ctx);
        }
        synchronized (st) { st.level = level; }
    }

    private static void setFile(LogState st, Term arg, String ctx) {
        String path = (arg instanceof PrologString) ? ((PrologString) arg).getStringValue() : atom(arg, ctx);
        try (OutputStream probe = new FileOutputStream(path, true)) {
            // opened (and created) now, so a bad path is reported here and not at the first message
        } catch (IOException e) {
            throw Errors.permission("open", "source_sink", arg, ctx);
        }
        synchronized (st) { st.file = path; }
    }

    private static String atom(Term t, String ctx) {
        if (t instanceof Variable) throw Errors.instantiation(ctx);
        if (!(t instanceof Atom)) throw Errors.type("atom", t, ctx);
        return ((Atom) t).getName();
    }

    private static String text(Term t, String ctx) {
        if (t instanceof Variable) throw Errors.instantiation(ctx);
        if (t instanceof PrologString) return ((PrologString) t).getStringValue();
        if (t instanceof Atom) return ((Atom) t).getName();
        return it.denzosoft.jprolog.core.util.TermFormatter.format(t, false, false, true, 1200);
    }

    private String modeName() { return mode.name().toLowerCase(); }
}
// END_CHANGE: ISS-2025-0121
