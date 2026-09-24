package it.denzosoft.jprolog.builtin.regex;

// START_CHANGE: ISS-2025-0117 - Regex built-in predicates
// START_CHANGE: ISS-2025-0174 - Regex injection / escaping security hardening
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.engine.v4.Errors;
import it.denzosoft.jprolog.core.utils.CollectionUtils;

import java.util.*;
import java.util.regex.*;

/**
 * Regex predicates:
 *   re_match/2       - re_match(+Pattern, +String)              test match
 *   re_matchsub/3    - re_matchsub(+Pattern, +String, -Groups)  extract groups as list
 *   re_replace/4     - re_replace(+Pattern, +Replacement, +String, -Result)
 *   re_split/3       - re_split(+Pattern, +String, -Parts)
 *   re_findall/3     - re_findall(+Pattern, +String, -Matches)  all occurrences
 *   re_escape/2      - re_escape(+Atom, -Escaped)               escape regex special chars
 */
public class RegexPredicates implements BuiltIn {

    public enum Mode { RE_MATCH, RE_MATCHSUB, RE_REPLACE, RE_SPLIT, RE_FINDALL, RE_ESCAPE }

    private final Mode mode;

    public RegexPredicates(Mode mode) {
        this.mode = mode;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        switch (mode) {
            case RE_MATCH:    return doMatch(query, bindings, solutions);
            case RE_MATCHSUB: return doMatchSub(query, bindings, solutions);
            case RE_REPLACE:  return doReplace(query, bindings, solutions);
            case RE_SPLIT:    return doSplit(query, bindings, solutions);
            case RE_FINDALL:  return doFindAll(query, bindings, solutions);
            case RE_ESCAPE:   return doEscape(query, bindings, solutions);
            default: return false;
        }
    }

    /**
     * Safely compile a regex pattern, throwing a proper Prolog syntax_error
     * instead of letting PatternSyntaxException propagate as a Java exception.
     */
    // START_CHANGE: ISS-2025-0786 - 4.6 wave Q6 (extra 7): the matcher reads its input through
    // a CharSequence that charges the query's inference budget (and polls a Stop), so a
    // catastrophically backtracking pattern is bounded instead of invisible to the budget.
    private static CharSequence metered(String input) {
        return it.denzosoft.jprolog.core.engine.ResourceGuard.guarded(input);
    }
    // END_CHANGE: ISS-2025-0786

    private Pattern safeCompile(String pattern, String predicateName) {
        try {
            return Pattern.compile(pattern);
        } catch (PatternSyntaxException e) {
            // START_CHANGE: ISS-2025-0683 - a real error(syntax_error(invalid_regex), _) term; the
            // old code built that term as a Java STRING, so the ball was a bare atom
            int slash = predicateName.lastIndexOf('/');
            String nm = slash > 0 ? predicateName.substring(0, slash) : predicateName;
            int ar = slash > 0 ? Integer.parseInt(predicateName.substring(slash + 1)) : 0;
            throw Errors.syntax("invalid_regex", nm, ar, e.getDescription());
            // END_CHANGE: ISS-2025-0683
        }
    }

    private boolean doMatch(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 2);
        String pattern = resolveAtom(query, 0, bindings);
        String input = resolveAtom(query, 1, bindings);
        if (safeCompile(pattern, "re_match/2").matcher(metered(input)).find()) {
            solutions.add(bindings);
            return true;
        }
        return false;
    }

    private boolean doMatchSub(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 3);
        String pattern = resolveAtom(query, 0, bindings);
        String input = resolveAtom(query, 1, bindings);
        Matcher m = safeCompile(pattern, "re_matchsub/3").matcher(metered(input));
        if (m.find()) {
            List<Term> groups = new ArrayList<>();
            for (int i = 0; i <= m.groupCount(); i++) {
                String g = m.group(i);
                groups.add(new Atom(g != null ? g : ""));
            }
            return unify(query.getArguments().get(2), CollectionUtils.createListTerm(groups), bindings, solutions);
        }
        return false;
    }

    private boolean doReplace(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 4);
        String pattern = resolveAtom(query, 0, bindings);
        String replacement = resolveAtom(query, 1, bindings);
        String input = resolveAtom(query, 2, bindings);
        // START_CHANGE: ISS-2025-0174 - Use Matcher.quoteReplacement to prevent unintended group substitution
        Pattern compiled = safeCompile(pattern, "re_replace/4");
        String safeReplacement = Matcher.quoteReplacement(replacement);
        String result = compiled.matcher(metered(input)).replaceAll(safeReplacement);
        // END_CHANGE: ISS-2025-0174
        return unify(query.getArguments().get(3), new Atom(result), bindings, solutions);
    }

    private boolean doSplit(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 3);
        String pattern = resolveAtom(query, 0, bindings);
        String input = resolveAtom(query, 1, bindings);
        // Validate pattern before use (ISS-2025-0786: and split over the metered input — the
        // same result as String.split, which compiles the same pattern)
        String[] parts = safeCompile(pattern, "re_split/3").split(metered(input));
        List<Term> list = new ArrayList<>();
        for (String p : parts) list.add(new Atom(p));
        return unify(query.getArguments().get(2), CollectionUtils.createListTerm(list), bindings, solutions);
    }

    private boolean doFindAll(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 3);
        String pattern = resolveAtom(query, 0, bindings);
        String input = resolveAtom(query, 1, bindings);
        Matcher m = safeCompile(pattern, "re_findall/3").matcher(metered(input));
        List<Term> matches = new ArrayList<>();
        while (m.find()) {
            matches.add(new Atom(m.group()));
            it.denzosoft.jprolog.core.engine.ResourceGuard.chargeBridged(1);   // ISS-2025-0786: per match
        }
        return unify(query.getArguments().get(2), CollectionUtils.createListTerm(matches), bindings, solutions);
    }

    // START_CHANGE: ISS-2025-0174 - re_escape/2 predicate using Pattern.quote()
    private boolean doEscape(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 2);
        String input = resolveAtom(query, 0, bindings);
        String escaped = Pattern.quote(input);
        return unify(query.getArguments().get(1), new Atom(escaped), bindings, solutions);
    }
    // END_CHANGE: ISS-2025-0174


    // START_CHANGE: ISS-2025-0683 - wave Q1.1: ISO error terms error(Formal, context(Name/Arity, Msg)),
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
    // END_CHANGE: ISS-2025-0683

    private boolean unify(Term target, Term value, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        Map<String, Term> nb = new HashMap<>(bindings);
        if (target.resolveBindings(bindings).unify(value, nb)) { solutions.add(nb); return true; }
        return false;
    }

    private String modeName() { return mode.name().toLowerCase(); }
}
// END_CHANGE: ISS-2025-0174
// END_CHANGE: ISS-2025-0117
