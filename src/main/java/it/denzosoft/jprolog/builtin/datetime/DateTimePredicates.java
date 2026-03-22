package it.denzosoft.jprolog.builtin.datetime;

// START_CHANGE: ISS-2025-0114 - Date/time built-in predicates
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.time.*;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.*;

/**
 * Date/time predicates:
 *   get_time/1        - get_time(-Timestamp)            current epoch millis
 *   now/1             - now(-DateTimeAtom)               current datetime as ISO string
 *   today/1           - today(-DateAtom)                 current date YYYY-MM-DD
 *   format_time/3     - format_time(+Format, +Timestamp, -Formatted)
 *   parse_time/3      - parse_time(+Format, +String, -Timestamp)
 *   date_add/4        - date_add(+Date, +Amount, +Unit, -NewDate)
 *   date_diff/4       - date_diff(+Date1, +Date2, +Unit, -Diff)
 *   day_of_week/2     - day_of_week(+Date, -DayName)
 *   date_parts/4      - date_parts(+Date, -Year, -Month, -Day)
 *   time_parts/4      - time_parts(+DateTime, -Hour, -Minute, -Second)
 */
public class DateTimePredicates implements BuiltIn {

    public enum Mode {
        GET_TIME, NOW, TODAY, FORMAT_TIME, PARSE_TIME,
        DATE_ADD, DATE_DIFF, DAY_OF_WEEK, DATE_PARTS, TIME_PARTS
    }

    private final Mode mode;

    public DateTimePredicates(Mode mode) {
        this.mode = mode;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        switch (mode) {
            case GET_TIME:    return doGetTime(query, bindings, solutions);
            case NOW:         return doNow(query, bindings, solutions);
            case TODAY:       return doToday(query, bindings, solutions);
            case FORMAT_TIME: return doFormatTime(query, bindings, solutions);
            case PARSE_TIME:  return doParseTime(query, bindings, solutions);
            case DATE_ADD:    return doDateAdd(query, bindings, solutions);
            case DATE_DIFF:   return doDateDiff(query, bindings, solutions);
            case DAY_OF_WEEK: return doDayOfWeek(query, bindings, solutions);
            case DATE_PARTS:  return doDateParts(query, bindings, solutions);
            case TIME_PARTS:  return doTimeParts(query, bindings, solutions);
            default: return false;
        }
    }

    private boolean doGetTime(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 1, "get_time/1");
        return unify(query.getArguments().get(0), new Number(System.currentTimeMillis()), bindings, solutions);
    }

    private boolean doNow(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 1, "now/1");
        String now = LocalDateTime.now().format(DateTimeFormatter.ISO_LOCAL_DATE_TIME);
        return unify(query.getArguments().get(0), new Atom(now), bindings, solutions);
    }

    private boolean doToday(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 1, "today/1");
        String today = LocalDate.now().toString();
        return unify(query.getArguments().get(0), new Atom(today), bindings, solutions);
    }

    private boolean doFormatTime(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 3, "format_time/3");
        String format = resolveAtom(query.getArguments().get(0), bindings);
        Term tsTerm = query.getArguments().get(1).resolveBindings(bindings);

        String formatted;
        if (tsTerm instanceof Number) {
            long millis = ((Number) tsTerm).getValue().longValue();
            LocalDateTime dt = LocalDateTime.ofInstant(Instant.ofEpochMilli(millis), ZoneId.systemDefault());
            formatted = dt.format(DateTimeFormatter.ofPattern(format));
        } else if (tsTerm instanceof Atom) {
            LocalDateTime dt = parseDateTime(((Atom) tsTerm).getName());
            formatted = dt.format(DateTimeFormatter.ofPattern(format));
        } else {
            throw new PrologEvaluationException("format_time/3: Timestamp must be a number or date atom.");
        }
        return unify(query.getArguments().get(2), new Atom(formatted), bindings, solutions);
    }

    private boolean doParseTime(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 3, "parse_time/3");
        String format = resolveAtom(query.getArguments().get(0), bindings);
        String dateStr = resolveAtom(query.getArguments().get(1), bindings);

        LocalDateTime dt = LocalDateTime.parse(dateStr, DateTimeFormatter.ofPattern(format));
        long millis = dt.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
        return unify(query.getArguments().get(2), new Number(millis), bindings, solutions);
    }

    private boolean doDateAdd(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 4, "date_add/4");
        String dateStr = resolveAtom(query.getArguments().get(0), bindings);
        Term amtTerm = query.getArguments().get(1).resolveBindings(bindings);
        String unit = resolveAtom(query.getArguments().get(2), bindings);

        if (!(amtTerm instanceof Number)) throw new PrologEvaluationException("date_add/4: Amount must be a number.");
        long amount = ((Number) amtTerm).getValue().longValue();

        LocalDate date = LocalDate.parse(dateStr);
        LocalDate result;
        switch (unit.toLowerCase()) {
            case "days": result = date.plusDays(amount); break;
            case "weeks": result = date.plusWeeks(amount); break;
            case "months": result = date.plusMonths(amount); break;
            case "years": result = date.plusYears(amount); break;
            default: throw new PrologEvaluationException("date_add/4: Unknown unit: " + unit);
        }
        return unify(query.getArguments().get(3), new Atom(result.toString()), bindings, solutions);
    }

    private boolean doDateDiff(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 4, "date_diff/4");
        String d1 = resolveAtom(query.getArguments().get(0), bindings);
        String d2 = resolveAtom(query.getArguments().get(1), bindings);
        String unit = resolveAtom(query.getArguments().get(2), bindings);

        LocalDate date1 = LocalDate.parse(d1);
        LocalDate date2 = LocalDate.parse(d2);
        long diff;
        switch (unit.toLowerCase()) {
            case "days": diff = ChronoUnit.DAYS.between(date1, date2); break;
            case "weeks": diff = ChronoUnit.WEEKS.between(date1, date2); break;
            case "months": diff = ChronoUnit.MONTHS.between(date1, date2); break;
            case "years": diff = ChronoUnit.YEARS.between(date1, date2); break;
            default: throw new PrologEvaluationException("date_diff/4: Unknown unit: " + unit);
        }
        return unify(query.getArguments().get(3), new Number(diff), bindings, solutions);
    }

    private boolean doDayOfWeek(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 2, "day_of_week/2");
        String dateStr = resolveAtom(query.getArguments().get(0), bindings);
        LocalDate date = LocalDate.parse(dateStr);
        String dayName = date.getDayOfWeek().name().toLowerCase();
        return unify(query.getArguments().get(1), new Atom(dayName), bindings, solutions);
    }

    private boolean doDateParts(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 4, "date_parts/4");
        String dateStr = resolveAtom(query.getArguments().get(0), bindings);
        LocalDate date = LocalDate.parse(dateStr);
        Map<String, Term> nb = new HashMap<>(bindings);
        if (query.getArguments().get(1).resolveBindings(bindings).unify(new Number(date.getYear()), nb) &&
            query.getArguments().get(2).resolveBindings(nb).unify(new Number(date.getMonthValue()), nb) &&
            query.getArguments().get(3).resolveBindings(nb).unify(new Number(date.getDayOfMonth()), nb)) {
            solutions.add(nb);
            return true;
        }
        return false;
    }

    private boolean doTimeParts(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 4, "time_parts/4");
        String dtStr = resolveAtom(query.getArguments().get(0), bindings);
        LocalDateTime dt = parseDateTime(dtStr);
        Map<String, Term> nb = new HashMap<>(bindings);
        if (query.getArguments().get(1).resolveBindings(bindings).unify(new Number(dt.getHour()), nb) &&
            query.getArguments().get(2).resolveBindings(nb).unify(new Number(dt.getMinute()), nb) &&
            query.getArguments().get(3).resolveBindings(nb).unify(new Number(dt.getSecond()), nb)) {
            solutions.add(nb);
            return true;
        }
        return false;
    }

    private LocalDateTime parseDateTime(String s) {
        try { return LocalDateTime.parse(s); }
        catch (Exception e1) {
            try { return LocalDate.parse(s).atStartOfDay(); }
            catch (Exception e2) {
                throw new PrologEvaluationException("Cannot parse datetime: " + s);
            }
        }
    }

    private void checkArity(Term query, int expected, String name) {
        if (query.getArguments().size() != expected)
            throw new PrologEvaluationException(name + " requires " + expected + " arguments.");
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
// END_CHANGE: ISS-2025-0114
