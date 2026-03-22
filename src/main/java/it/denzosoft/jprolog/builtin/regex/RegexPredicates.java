package it.denzosoft.jprolog.builtin.regex;

// START_CHANGE: ISS-2025-0117 - Regex built-in predicates
// START_CHANGE: ISS-2025-0174 - Regex injection / escaping security hardening
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
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
    private Pattern safeCompile(String pattern, String predicateName) {
        try {
            return Pattern.compile(pattern);
        } catch (PatternSyntaxException e) {
            throw new PrologEvaluationException(
                "error(syntax_error(invalid_regex), " + predicateName + "): " + e.getMessage());
        }
    }

    private boolean doMatch(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 2);
        String pattern = resolveAtom(query.getArguments().get(0), bindings);
        String input = resolveAtom(query.getArguments().get(1), bindings);
        if (safeCompile(pattern, "re_match/2").matcher(input).find()) {
            solutions.add(bindings);
            return true;
        }
        return false;
    }

    private boolean doMatchSub(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 3);
        String pattern = resolveAtom(query.getArguments().get(0), bindings);
        String input = resolveAtom(query.getArguments().get(1), bindings);
        Matcher m = safeCompile(pattern, "re_matchsub/3").matcher(input);
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
        String pattern = resolveAtom(query.getArguments().get(0), bindings);
        String replacement = resolveAtom(query.getArguments().get(1), bindings);
        String input = resolveAtom(query.getArguments().get(2), bindings);
        // START_CHANGE: ISS-2025-0174 - Use Matcher.quoteReplacement to prevent unintended group substitution
        Pattern compiled = safeCompile(pattern, "re_replace/4");
        String safeReplacement = Matcher.quoteReplacement(replacement);
        String result = compiled.matcher(input).replaceAll(safeReplacement);
        // END_CHANGE: ISS-2025-0174
        return unify(query.getArguments().get(3), new Atom(result), bindings, solutions);
    }

    private boolean doSplit(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 3);
        String pattern = resolveAtom(query.getArguments().get(0), bindings);
        String input = resolveAtom(query.getArguments().get(1), bindings);
        // Validate pattern before use
        safeCompile(pattern, "re_split/3");
        String[] parts = input.split(pattern);
        List<Term> list = new ArrayList<>();
        for (String p : parts) list.add(new Atom(p));
        return unify(query.getArguments().get(2), CollectionUtils.createListTerm(list), bindings, solutions);
    }

    private boolean doFindAll(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 3);
        String pattern = resolveAtom(query.getArguments().get(0), bindings);
        String input = resolveAtom(query.getArguments().get(1), bindings);
        Matcher m = safeCompile(pattern, "re_findall/3").matcher(input);
        List<Term> matches = new ArrayList<>();
        while (m.find()) {
            matches.add(new Atom(m.group()));
        }
        return unify(query.getArguments().get(2), CollectionUtils.createListTerm(matches), bindings, solutions);
    }

    // START_CHANGE: ISS-2025-0174 - re_escape/2 predicate using Pattern.quote()
    private boolean doEscape(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 2);
        String input = resolveAtom(query.getArguments().get(0), bindings);
        String escaped = Pattern.quote(input);
        return unify(query.getArguments().get(1), new Atom(escaped), bindings, solutions);
    }
    // END_CHANGE: ISS-2025-0174

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
// END_CHANGE: ISS-2025-0174
// END_CHANGE: ISS-2025-0117
