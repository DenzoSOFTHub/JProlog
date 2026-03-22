package it.denzosoft.jprolog.builtin.logging;

// START_CHANGE: ISS-2025-0121 - Logging built-in predicates
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.*;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.logging.*;

/**
 * Logging predicates:
 *   log_info/1      - log_info(+Message)
 *   log_warning/1   - log_warning(+Message)
 *   log_error/1     - log_error(+Message)
 *   log_debug/1     - log_debug(+Message)
 *   log_level/1     - log_level(+Level)  set level: debug, info, warning, error, off
 *   log_to_file/1   - log_to_file(+FilePath)  redirect logging to file
 */
public class LoggingPredicates implements BuiltIn {

    public enum Mode { LOG_INFO, LOG_WARNING, LOG_ERROR, LOG_DEBUG, LOG_LEVEL, LOG_TO_FILE }

    private final Mode mode;

    private static final Logger LOGGER = Logger.getLogger("JProlog");
    private static FileHandler fileHandler = null;

    static {
        LOGGER.setUseParentHandlers(true);
        LOGGER.setLevel(Level.INFO);
    }

    public LoggingPredicates(Mode mode) {
        this.mode = mode;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        try {
            switch (mode) {
                case LOG_INFO:    return doLog(query, bindings, solutions, Level.INFO);
                case LOG_WARNING: return doLog(query, bindings, solutions, Level.WARNING);
                case LOG_ERROR:   return doLog(query, bindings, solutions, Level.SEVERE);
                case LOG_DEBUG:   return doLog(query, bindings, solutions, Level.FINE);
                case LOG_LEVEL:   return doLogLevel(query, bindings, solutions);
                case LOG_TO_FILE: return doLogToFile(query, bindings, solutions);
                default: return false;
            }
        } catch (Exception e) {
            throw new PrologEvaluationException(modeName() + ": " + e.getMessage());
        }
    }

    private boolean doLog(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions, Level level) {
        checkArity(query, 1);
        String message = resolveAtom(query.getArguments().get(0), bindings);
        LOGGER.log(level, message);
        solutions.add(bindings);
        return true;
    }

    private boolean doLogLevel(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 1);
        String levelStr = resolveAtom(query.getArguments().get(0), bindings);
        Level level;
        switch (levelStr.toLowerCase()) {
            case "debug":   level = Level.FINE; break;
            case "info":    level = Level.INFO; break;
            case "warning": level = Level.WARNING; break;
            case "error":   level = Level.SEVERE; break;
            case "off":     level = Level.OFF; break;
            case "all":     level = Level.ALL; break;
            default: throw new PrologEvaluationException("log_level/1: unknown level: " + levelStr);
        }
        LOGGER.setLevel(level);
        solutions.add(bindings);
        return true;
    }

    private boolean doLogToFile(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws IOException {
        checkArity(query, 1);
        String path = resolveAtom(query.getArguments().get(0), bindings);
        synchronized (LOGGER) {
            if (fileHandler != null) {
                LOGGER.removeHandler(fileHandler);
                fileHandler.close();
            }
            fileHandler = new FileHandler(path, true);
            fileHandler.setFormatter(new SimpleFormatter());
            LOGGER.addHandler(fileHandler);
        }
        solutions.add(bindings);
        return true;
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

    private String modeName() { return mode.name().toLowerCase(); }
}
// END_CHANGE: ISS-2025-0121
