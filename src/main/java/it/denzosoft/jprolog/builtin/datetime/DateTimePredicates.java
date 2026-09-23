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
        DATE_ADD, DATE_DIFF, DAY_OF_WEEK, DATE_PARTS, TIME_PARTS,
        STAMP_DATE_TIME, DATE_TIME_STAMP                         // ISS-2025-0609
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
            case STAMP_DATE_TIME: return doStampDateTime(query, bindings, solutions);   // ISS-2025-0609
            case DATE_TIME_STAMP: return doDateTimeStamp(query, bindings, solutions);   // ISS-2025-0609
            default: return false;
        }
    }

    private boolean doGetTime(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 1, "get_time/1");
        // START_CHANGE: ISS-2025-0609 - SWI: a FLOAT number of seconds since the epoch (it was an
        // integer number of milliseconds); format_time/3, parse_time/3 and stamp_date_time/3 agree.
        return unify(query.getArguments().get(0), new Number(System.currentTimeMillis() / 1000.0), bindings, solutions);
        // END_CHANGE: ISS-2025-0609
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
        // START_CHANGE: ISS-2025-0609 - SWI's argument order format_time(+Out, +Format, +Stamp)
        // (Out = atom(A) / string(S) / codes(C) / chars(C) / a stream) is recognised next to the
        // historical format_time(+Format, +Stamp, -Atom); a numeric stamp is SECONDS since the
        // epoch (get_time/1); a Format containing '%' uses the strftime directives.
        Term a0 = query.getArguments().get(0).resolveBindings(bindings);
        boolean swiOrder = isOutSpec(a0);
        String format = resolveText(query.getArguments().get(swiOrder ? 1 : 0), bindings);
        Term tsTerm = query.getArguments().get(swiOrder ? 2 : 1).resolveBindings(bindings);

        ZonedDateTime zdt;
        if (tsTerm instanceof Number) {
            zdt = zonedOf(((Number) tsTerm).doubleValue(), ZoneId.systemDefault());
        } else if (tsTerm instanceof Atom) {
            zdt = parseDateTime(((Atom) tsTerm).getName()).atZone(ZoneId.systemDefault());
        } else if (tsTerm instanceof CompoundTerm && "date".equals(tsTerm.getName())) {
            zdt = zonedOf(stampOfDate((CompoundTerm) tsTerm), ZoneId.systemDefault());
        } else {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("number", tsTerm, "format_time/3"));
        }
        String formatted = format.indexOf('%') >= 0 ? strftime(format, zdt)
                         : zdt.format(DateTimeFormatter.ofPattern(format));
        if (!swiOrder) return unify(query.getArguments().get(2), new Atom(formatted), bindings, solutions);
        CompoundTerm out = (a0 instanceof CompoundTerm) ? (CompoundTerm) a0 : null;
        if (out != null && out.getArguments().size() == 1) {
            Term target = out.getArguments().get(0);
            switch (out.getName()) {
                case "atom":   return unify(target, new Atom(formatted), bindings, solutions);
                case "string": return unify(target, new it.denzosoft.jprolog.core.terms.PrologString(formatted), bindings, solutions);
                case "codes": case "chars": {
                    List<Term> es = new ArrayList<>();
                    formatted.codePoints().forEach(cp -> es.add("codes".equals(out.getName())
                        ? (Term) Number.valueOf((long) cp) : new Atom(new String(Character.toChars(cp)))));
                    Term list = new Atom("[]");
                    for (int i = es.size() - 1; i >= 0; i--) list = new CompoundTerm(new Atom("."), Arrays.asList(es.get(i), list));
                    return unify(target, list, bindings, solutions);
                }
                default: break;
            }
        }
        java.io.PrintStream ps = it.denzosoft.jprolog.builtin.io.IOStreamUtils.resolveOutputStream(a0, bindings, "format_time/3");
        ps.print(formatted);
        ps.flush();
        solutions.add(bindings);
        return true;
        // END_CHANGE: ISS-2025-0609
    }

    private boolean doParseTime(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 3, "parse_time/3");
        String format = resolveAtom(query.getArguments().get(0), bindings);
        String dateStr = resolveAtom(query.getArguments().get(1), bindings);

        LocalDateTime dt = LocalDateTime.parse(dateStr, DateTimeFormatter.ofPattern(format));
        long millis = dt.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
        // ISS-2025-0609: seconds since the epoch, as get_time/1
        return unify(query.getArguments().get(2), new Number(millis / 1000.0), bindings, solutions);
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

    // START_CHANGE: ISS-2025-0609 - SWI's stamp_date_time/3, date_time_stamp/2 and strftime
    private static boolean isOutSpec(Term t) {
        if (t instanceof CompoundTerm && t.getArguments().size() == 1) {
            String n = t.getName();
            return "atom".equals(n) || "string".equals(n) || "codes".equals(n) || "chars".equals(n)
                || "$stream".equals(n) || "stream".equals(n);
        }
        if (t instanceof Atom) {
            String n = ((Atom) t).getName();
            return "user_output".equals(n) || "user_error".equals(n) || "current_output".equals(n);
        }
        return false;
    }

    private String resolveText(Term term, Map<String, Term> bindings) {
        Term r = term.resolveBindings(bindings);
        if (r instanceof it.denzosoft.jprolog.core.terms.PrologString) {
            return ((it.denzosoft.jprolog.core.terms.PrologString) r).getStringValue();
        }
        return resolveAtom(term, bindings);
    }

    private static ZonedDateTime zonedOf(double seconds, ZoneId zone) {
        long whole = (long) Math.floor(seconds);
        long nanos = Math.round((seconds - whole) * 1e9);
        if (nanos >= 1_000_000_000L) { whole++; nanos -= 1_000_000_000L; }
        return Instant.ofEpochSecond(whole, nanos).atZone(zone);
    }

    /** date(Y,M,D,H,Mn,S,Off,TZ,DST) (or date(Y,M,D)) to seconds since the epoch. */
    private static double stampOfDate(CompoundTerm d) {
        List<Term> a = d.getArguments();
        int y = intOf(a.get(0)), mo = intOf(a.get(1)), da = intOf(a.get(2));
        int h = 0, mi = 0; double sec = 0; Integer offWest = null;
        if (a.size() >= 6) {
            h = intOf(a.get(3)); mi = intOf(a.get(4));
            sec = (a.get(5) instanceof Number) ? ((Number) a.get(5)).doubleValue() : 0;
        }
        if (a.size() >= 7 && a.get(6) instanceof Number) offWest = Integer.valueOf(intOf(a.get(6)));
        LocalDateTime ldt = LocalDateTime.of(y, 1, 1, 0, 0).plusMonths(mo - 1L).plusDays(da - 1L)
            .plusHours(h).plusMinutes(mi);
        long epoch = (offWest != null)
            ? ldt.toEpochSecond(ZoneOffset.ofTotalSeconds(-offWest.intValue()))
            : ldt.atZone(ZoneId.systemDefault()).toEpochSecond();
        return epoch + sec;
    }

    private static int intOf(Term t) {
        if (t instanceof Number && ((Number) t).isInteger()) return (int) ((Number) t).longValue();
        if (t instanceof it.denzosoft.jprolog.core.terms.Variable) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.instantiationError("date_time_stamp/2"));
        }
        throw new it.denzosoft.jprolog.core.exceptions.PrologException(
            it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("integer", t, "date_time_stamp/2"));
    }

    /** stamp_date_time(+Stamp, -date(Y,M,D,H,Mn,S,Off,TZ,DST), +TimeZone) — SWI semantics:
     *  TimeZone is local, 'UTC' or an offset in seconds WEST of Greenwich. */
    private boolean doStampDateTime(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        checkArity(query, 3, "stamp_date_time/3");
        Term st = query.getArguments().get(0).resolveBindings(bindings);
        Term tz = query.getArguments().get(2).resolveBindings(bindings);
        if (st instanceof it.denzosoft.jprolog.core.terms.Variable) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.instantiationError("stamp_date_time/3"));
        }
        if (!(st instanceof Number)) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("number", st, "stamp_date_time/3"));
        }
        double stamp = ((Number) st).doubleValue();
        ZoneId zone;
        Term tzName;
        Term dst;
        if (tz instanceof it.denzosoft.jprolog.core.terms.Variable || (tz instanceof Atom && "local".equals(((Atom) tz).getName()))) {
            zone = ZoneId.systemDefault();
            tzName = null; dst = null;
        } else if (tz instanceof Atom && "UTC".equals(((Atom) tz).getName())) {
            zone = ZoneOffset.UTC;
            tzName = new Atom("UTC"); dst = new Atom("-");
        } else if (tz instanceof Number && ((Number) tz).isInteger()) {
            zone = ZoneOffset.ofTotalSeconds(-(int) ((Number) tz).longValue());
            tzName = new Atom("-"); dst = new Atom("-");
        } else {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.domainError("timezone", tz, "stamp_date_time/3"));
        }
        ZonedDateTime z = zonedOf(stamp, zone);
        int offWest = -z.getOffset().getTotalSeconds();
        if (tzName == null) {
            boolean isDst = zone.getRules().isDaylightSavings(z.toInstant());
            String abbr = java.util.TimeZone.getTimeZone(zone).getDisplayName(isDst, java.util.TimeZone.SHORT, Locale.ROOT);
            tzName = new Atom(abbr);
            dst = new Atom(isDst ? "true" : "false");
        }
        double sec = z.getSecond() + z.getNano() / 1e9;
        Term date = new CompoundTerm(new Atom("date"), Arrays.asList(
            Number.valueOf((long) z.getYear()), Number.valueOf((long) z.getMonthValue()),
            Number.valueOf((long) z.getDayOfMonth()), Number.valueOf((long) z.getHour()),
            Number.valueOf((long) z.getMinute()), new Number(sec), Number.valueOf((long) offWest),
            tzName, dst));
        Map<String, Term> nb = new HashMap<>(bindings);
        if (tz instanceof it.denzosoft.jprolog.core.terms.Variable && !tz.unify(new Atom("local"), nb)) return false;
        if (!query.getArguments().get(1).resolveBindings(nb).unify(date, nb)) return false;
        solutions.add(nb);
        return true;
    }

    /** date_time_stamp(+date(Y,M,D,H,Mn,S,Off,TZ,DST), -Stamp) — seconds since the epoch. */
    private boolean doDateTimeStamp(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        checkArity(query, 2, "date_time_stamp/2");
        Term d = query.getArguments().get(0).resolveBindings(bindings);
        if (!(d instanceof CompoundTerm) || !"date".equals(d.getName())
                || (d.getArguments().size() != 9 && d.getArguments().size() != 3)) {
            throw new it.denzosoft.jprolog.core.exceptions.PrologException(
                it.denzosoft.jprolog.builtin.exception.ISOErrorTerms.typeError("date", d, "date_time_stamp/2"));
        }
        return unify(query.getArguments().get(1), new Number(stampOfDate((CompoundTerm) d)), bindings, solutions);
    }

    /** The strftime directives SWI's format_time/3 documents (the common subset). */
    static String strftime(String fmt, ZonedDateTime z) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < fmt.length(); i++) {
            char c = fmt.charAt(i);
            if (c != '%' || i + 1 >= fmt.length()) { sb.append(c); continue; }
            char d = fmt.charAt(++i);
            switch (d) {
                case 'Y': sb.append(z.getYear()); break;
                case 'y': sb.append(String.format("%02d", z.getYear() % 100)); break;
                case 'm': sb.append(String.format("%02d", z.getMonthValue())); break;
                case 'd': sb.append(String.format("%02d", z.getDayOfMonth())); break;
                case 'e': sb.append(String.format("%2d", z.getDayOfMonth())); break;
                case 'H': sb.append(String.format("%02d", z.getHour())); break;
                case 'I': sb.append(String.format("%02d", (z.getHour() + 11) % 12 + 1)); break;
                case 'M': sb.append(String.format("%02d", z.getMinute())); break;
                case 'S': sb.append(String.format("%02d", z.getSecond())); break;
                case 'f': sb.append(String.format("%06d", z.getNano() / 1000)); break;
                case 'j': sb.append(String.format("%03d", z.getDayOfYear())); break;
                case 'p': sb.append(z.getHour() < 12 ? "AM" : "PM"); break;
                case 'P': sb.append(z.getHour() < 12 ? "am" : "pm"); break;
                case 'a': sb.append(z.getDayOfWeek().getDisplayName(java.time.format.TextStyle.SHORT, Locale.ENGLISH)); break;
                case 'A': sb.append(z.getDayOfWeek().getDisplayName(java.time.format.TextStyle.FULL, Locale.ENGLISH)); break;
                case 'b': case 'h': sb.append(z.getMonth().getDisplayName(java.time.format.TextStyle.SHORT, Locale.ENGLISH)); break;
                case 'B': sb.append(z.getMonth().getDisplayName(java.time.format.TextStyle.FULL, Locale.ENGLISH)); break;
                case 'u': sb.append(z.getDayOfWeek().getValue()); break;
                case 'w': sb.append(z.getDayOfWeek().getValue() % 7); break;
                case 's': sb.append(z.toEpochSecond()); break;
                case 'z': {
                    int o = z.getOffset().getTotalSeconds();
                    sb.append(o < 0 ? '-' : '+').append(String.format("%02d%02d", Math.abs(o) / 3600, (Math.abs(o) % 3600) / 60));
                    break;
                }
                case 'Z': sb.append(z.getZone().getDisplayName(java.time.format.TextStyle.SHORT, Locale.ROOT)); break;
                case 'F': sb.append(strftime("%Y-%m-%d", z)); break;
                case 'D': sb.append(strftime("%m/%d/%y", z)); break;
                case 'T': sb.append(strftime("%H:%M:%S", z)); break;
                case 'R': sb.append(strftime("%H:%M", z)); break;
                case 'c': sb.append(strftime("%a %b %e %H:%M:%S %Y", z)); break;
                case 'n': sb.append('\n'); break;
                case 't': sb.append('\t'); break;
                case '%': sb.append('%'); break;
                default: sb.append('%').append(d); break;
            }
        }
        return sb.toString();
    }
    // END_CHANGE: ISS-2025-0609

    private LocalDateTime parseDateTime(String s) {
        try { return LocalDateTime.parse(s); }
        catch (Exception e1) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e1);   // ISS-2025-0431
            try { return LocalDate.parse(s).atStartOfDay(); }
            catch (Exception e2) {
                it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e2);   // ISS-2025-0431
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
