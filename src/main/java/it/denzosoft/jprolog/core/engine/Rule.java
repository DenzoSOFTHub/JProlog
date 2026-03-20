package it.denzosoft.jprolog.core.engine;

import it.denzosoft.jprolog.core.terms.Term;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class Rule {
    private final Term head;
    private final List<Term> body;
    // START_CHANGE: ISS-2025-0092 - Cache ground status for ground fact optimization
    private final boolean groundFact;
    // END_CHANGE: ISS-2025-0092

    /**
     * Create a new rule.
     *
     * @param head The rule head (required)
     * @param body The rule body (can be empty for facts)
     */
    public Rule(Term head, List<Term> body) {
        this.head = Objects.requireNonNull(head, "Rule head cannot be null");
        this.body = body != null ? Collections.unmodifiableList(body) : Collections.emptyList();
        this.groundFact = this.body.isEmpty() && this.head.isGround();
    }

    /**
     * Get the rule head.
     * 
     * @return The head term
     */
    public Term getHead() {
        return head;
    }

    /**
     * Get the rule body.
     * 
     * @return An immutable list of body terms
     */
    public List<Term> getBody() {
        return body;
    }

    // START_CHANGE: ISS-2025-0092 - Ground fact detection for TermCopier optimization
    /**
     * Check if this rule is a ground fact (fact with no variables in the head).
     * Ground facts don't need TermCopier variable renaming.
     */
    public boolean isGroundFact() {
        return groundFact;
    }
    // END_CHANGE: ISS-2025-0092

    @Override
    public String toString() {
        if (body.isEmpty()) {
            return head.toString() + ".";
        }
        return head.toString() + " :- " + body.stream().map(Term::toString).collect(Collectors.joining(", ")) + ".";
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        Rule rule = (Rule) obj;
        return Objects.equals(head, rule.head) && Objects.equals(body, rule.body);
    }

    @Override
    public int hashCode() {
        return Objects.hash(head, body);
    }
}
