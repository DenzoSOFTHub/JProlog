package it.denzosoft.jprolog.core.dcg.v2;

// START_CHANGE: ISS-2025-0393 - ISO error terms for invalid terminal sequences
import it.denzosoft.jprolog.builtin.exception.ISOErrorTerms;
import it.denzosoft.jprolog.core.exceptions.PrologException;
// END_CHANGE: ISS-2025-0393
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.PrologString;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Clean-room DCG translator: rewrites a {@code Head --> Body} grammar rule into an ordinary clause
 * with two extra difference-list arguments (ISO 13211-1 §7.14 / standard SWI semantics). It is a
 * single recursive pass over the body and supports the full standard set the legacy transformer
 * grew piecemeal:
 *
 * <ul>
 *   <li>terminal lists {@code [a,b,c]} and {@code []} (and strings as code lists);</li>
 *   <li>{@code {Goal}} (zero-width Prolog escape), {@code !} (cut), {@code \+ B} (negation);</li>
 *   <li>{@code (A,B)}, {@code (A;B)}, {@code (A|B)}, {@code (A->B)} control;</li>
 *   <li>{@code call(G)}/{@code call//N} non-terminal application and variable bodies (→ phrase);</li>
 *   <li>ISO head <b>push-back</b>: {@code Head, PushBackList --> Body}.</li>
 * </ul>
 *
 * <p>{@link #translate(CompoundTerm)} returns a clause term ({@code (Head' :- Body')}); the engine
 * turns it into a {@code Rule} the same way it does for ordinary clauses.
 */
public final class DCGTranslator {

    private int counter = 0;

    // START_CHANGE: ISS-2025-0391 - configurable fresh-variable prefix so phrase/2,3 can expand
    // grammar bodies at runtime without the generated names colliding with caller variables.
    private final String varPrefix;

    public DCGTranslator() { this("_S"); }

    public DCGTranslator(String varPrefix) { this.varPrefix = varPrefix; }

    private Variable fresh() { return new Variable(varPrefix + (counter++)); }

    /**
     * Translate a grammar BODY threading the difference list {@code s0 -> s}. Public entry point
     * for phrase/2,3 (and call_dcg/3 style callers), which must apply the FULL body translation
     * (control constructs, terminal lists, strings, {}/1, !, \+) rather than blindly appending
     * the two list arguments to any compound.
     */
    public Term body(Term b, Term s0, Term s) { return translateBody(b, s0, s); }
    // END_CHANGE: ISS-2025-0391

    /** Is {@code t} a {@code -->/2} grammar rule? */
    public static boolean isDCGRule(Term t) {
        return t instanceof CompoundTerm
            && "-->".equals(((CompoundTerm) t).getName())
            && ((CompoundTerm) t).getArguments().size() == 2;
    }

    /** Translate a {@code Head --> Body} rule into a clause term {@code (Head' :- Body')}. */
    public Term translate(CompoundTerm dcgRule) {
        Term head = dcgRule.getArguments().get(0);
        Term body = dcgRule.getArguments().get(1);
        Variable s0 = fresh(), s = fresh();

        // Head push-back:  (NonTerminal, PushBack) --> Body
        if (head instanceof CompoundTerm && isComma((CompoundTerm) head)) {
            CompoundTerm h = (CompoundTerm) head;
            Term nt = h.getArguments().get(0);
            Term pushback = h.getArguments().get(1);
            // START_CHANGE: ISS-2025-0410 - validate the push-back head's non-terminal
            checkCallableHead(nt);
            // END_CHANGE: ISS-2025-0410
            // START_CHANGE: ISS-2025-0392 - the push-back must be a terminal sequence: convert a
            // string to its code list (mirroring the body path); a variable or non-list push-back
            // is rejected inside terminal() instead of silently discarding the body's rest var.
            if (pushback instanceof PrologString) {
                pushback = stringToCodes((PrologString) pushback);
            }
            // END_CHANGE: ISS-2025-0392
            Variable s1 = fresh();
            Term newHead = addArgs(nt, s0, s);
            Term goal = translateBody(body, s0, s1);
            Term pb = terminal(pushback, s, s1);          // S0..S1 parsed by Body, then push PushBack back: S = PB ++ S1
            return clause(newHead, conj(goal, pb));
        }

        // START_CHANGE: ISS-2025-0410 - validate the grammar-rule head at translation time
        checkCallableHead(head);
        // END_CHANGE: ISS-2025-0410
        Term newHead = addArgs(head, s0, s);
        return clause(newHead, translateBody(body, s0, s));
    }

    // START_CHANGE: ISS-2025-0410 - a grammar-rule head must be a callable non-terminal.
    // A number/string head (e.g. 7 --> [a]) used to be wrapped as call(Head, S0, S), which
    // consult then rejected with the misleading "Cannot redefine built-in predicate call/3";
    // raise instantiation_error / type_error(callable, Head) here instead (ISO 13211-3).
    private static void checkCallableHead(Term head) {
        if (head instanceof Variable) {
            throw new PrologException(ISOErrorTerms.instantiationError("dcg_head"));
        }
        if (!(head instanceof Atom) && !(head instanceof CompoundTerm)) {
            throw new PrologException(ISOErrorTerms.typeError("callable", head, "dcg_head"));
        }
    }
    // END_CHANGE: ISS-2025-0410

    // ------------------------------------------------------------------- body translation
    private Term translateBody(Term b, Term s0, Term s) {
        if (b instanceof Variable) {                       // variable body -> phrase(V, S0, S)
            return new CompoundTerm(new Atom("phrase"), Arrays.asList(b, s0, s));
        }
        if (b instanceof PrologString) {                   // "abc" -> terminal code list
            return terminal(stringToCodes((PrologString) b), s0, s);
        }
        if (b instanceof Atom) {
            String n = ((Atom) b).getName();
            if ("[]".equals(n)) return unify(s0, s);       // empty terminal
            if ("!".equals(n)) return conj(new Atom("!"), unify(s0, s));
            return addArgs(b, s0, s);                       // 0-arg non-terminal
        }
        if (b instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) b;
            String f = c.getName();
            List<Term> a = c.getArguments();
            int n = a.size();
            if (".".equals(f) && n == 2) return terminal(b, s0, s);          // terminal list
            if ("{}".equals(f) && n == 1) return conj(a.get(0), unify(s0, s)); // {Goal}
            if (",".equals(f) && n == 2) {                                    // (A, B)
                Variable s1 = fresh();
                return conj(translateBody(a.get(0), s0, s1), translateBody(a.get(1), s1, s));
            }
            if ((";".equals(f) || "|".equals(f)) && n == 2) {                 // (A ; B) / (A | B)
                return disj(translateBody(a.get(0), s0, s), translateBody(a.get(1), s0, s));
            }
            if ("->".equals(f) && n == 2) {                                   // (A -> B)
                Variable s1 = fresh();
                return new CompoundTerm(new Atom("->"),
                    Arrays.asList(translateBody(a.get(0), s0, s1), translateBody(a.get(1), s1, s)));
            }
            if ("\\+".equals(f) && n == 1) {                                  // \+ A  (zero width)
                Term neg = new CompoundTerm(new Atom("\\+"), Arrays.asList(translateBody(a.get(0), s0, fresh())));
                return conj(neg, unify(s0, s));
            }
            if ("call".equals(f)) {                                          // call//N -> call(G, ..., S0, S)
                List<Term> args = new ArrayList<>(a);
                args.add(s0);
                args.add(s);
                return new CompoundTerm(new Atom("call"), args);
            }
            return addArgs(b, s0, s);                                         // non-terminal nt(Args)
        }
        // numbers etc. as a non-terminal are ill-formed; emit a failing call
        return addArgs(b, s0, s);
    }

    // ------------------------------------------------------------------- helpers
    /** Append the two difference-list args to a non-terminal: {@code nt} → {@code nt(S0,S)}. */
    private Term addArgs(Term nt, Term s0, Term s) {
        if (nt instanceof Atom) {
            return new CompoundTerm((Atom) nt, Arrays.asList(s0, s));
        }
        if (nt instanceof CompoundTerm) {
            CompoundTerm c = (CompoundTerm) nt;
            List<Term> args = new ArrayList<>(c.getArguments());
            args.add(s0);
            args.add(s);
            return new CompoundTerm(new Atom(c.getName()), args);
        }
        // variable/other: route through call/3
        return new CompoundTerm(new Atom("call"), Arrays.asList(nt, s0, s));
    }

    /** A terminal list: unify S0 with the list whose tail is S (i.e. S0 = elems ++ S). */
    private Term terminal(Term listTerm, Term s0, Term s) {
        List<Term> elems = new ArrayList<>();
        Term cur = listTerm;
        while (cur instanceof CompoundTerm && ".".equals(((CompoundTerm) cur).getName())
                && ((CompoundTerm) cur).getArguments().size() == 2) {
            elems.add(((CompoundTerm) cur).getArguments().get(0));
            cur = ((CompoundTerm) cur).getArguments().get(1);
        }
        // cur is the tail; for a proper list it is [].
        boolean proper = cur instanceof Atom && "[]".equals(((Atom) cur).getName());
        // START_CHANGE: ISS-2025-0393 - a terminal sequence must be a PROPER list (ISO 13211-3):
        // a partial list like [a|T] used to silently drop the tail (the rule then meant [a]) and
        // a variable/non-list push-back (ISS-2025-0392) discarded the body's rest variable.
        // Raise instantiation_error / type_error(list, T) at translation time instead.
        if (!proper) {
            if (cur instanceof Variable) {
                throw new PrologException(ISOErrorTerms.instantiationError("dcg_terminal_sequence"));
            }
            throw new PrologException(ISOErrorTerms.typeError("list", listTerm, "dcg_terminal_sequence"));
        }
        // END_CHANGE: ISS-2025-0393
        if (elems.isEmpty()) {
            return unify(s0, s);
        }
        Term listWithTail = s;                            // build elems ++ S right-to-left
        for (int i = elems.size() - 1; i >= 0; i--) {
            listWithTail = new CompoundTerm(new Atom("."), Arrays.asList(elems.get(i), listWithTail));
        }
        return unify(s0, listWithTail);
    }

    private Term stringToCodes(PrologString str) {
        String v = str.getStringValue();
        Term list = new Atom("[]");
        for (int i = v.length() - 1; i >= 0; i--) {
            list = new CompoundTerm(new Atom("."), Arrays.asList(new Number((long) v.charAt(i)), list));
        }
        return list;
    }

    private static boolean isComma(CompoundTerm c) { return ",".equals(c.getName()) && c.getArguments().size() == 2; }
    private Term clause(Term head, Term body) { return new CompoundTerm(new Atom(":-"), Arrays.asList(head, body)); }
    private Term conj(Term a, Term b) { return new CompoundTerm(new Atom(","), Arrays.asList(a, b)); }
    private Term disj(Term a, Term b) { return new CompoundTerm(new Atom(";"), Arrays.asList(a, b)); }
    private Term unify(Term a, Term b) { return new CompoundTerm(new Atom("="), Arrays.asList(a, b)); }
}
