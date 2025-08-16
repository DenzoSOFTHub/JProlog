package it.denzosoft.jprolog;

import it.denzosoft.jprolog.terms.Term;
import java.util.Objects;

public class Clause {
    private final Term head;
    private final Term body;

    /**
     * Create a new clause.
     * 
     * @param head The clause head (required)
     * @param body The clause body (optional, null for facts)
     */
    public Clause(Term head, Term body) {
        if (head == null) {
            throw new IllegalArgumentException("Clause head cannot be null.");
        }
        this.head = head;
        this.body = body;
    }

    /**
     * Get the clause head.
     * 
     * @return The head term
     */
    public Term getHead() {
        return head;
    }

    /**
     * Get the clause body.
     * 
     * @return The body term or null for facts
     */
    public Term getBody() {
        return body;
    }

    @Override
    public String toString() {
        if (body == null) {
            return head.toString() + ".";
        }
        return head.toString() + " :- " + body.toString() + ".";
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        Clause clause = (Clause) obj;
        return Objects.equals(head, clause.head) && Objects.equals(body, clause.body);
    }

    @Override
    public int hashCode() {
        return Objects.hash(head, body);
    }
}
