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
    // START_CHANGE: ISS-2025-0322 - source line of this clause's head (1-based; -1 if unknown).
    // Powers line-accurate breakpoints in the IDE. Not part of equality (clause identity is head+body).
    private int sourceLine = -1;
    // END_CHANGE: ISS-2025-0322

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

    // START_CHANGE: ISS-2025-0439 - engine v4 (design B.2): cache of this clause's compiled
    // skeleton (core.engine.v4.Clause), so re-syncing the v4 ClauseStore after an external write is
    // a pointer copy instead of a recompilation. Typed as Object to keep core.engine independent of
    // core.engine.v4; not part of clause identity, never serialised.
    private transient Object compiled;

    /** The v4 compiled skeleton of this clause, or null when it has not been compiled yet. */
    public Object getCompiled() { return compiled; }

    /** Cache the v4 compiled skeleton of this clause. */
    public void setCompiled(Object skeleton) { this.compiled = skeleton; }
    // END_CHANGE: ISS-2025-0439

    /** Source line of this clause's head (1-based), or -1 if unknown. (ISS-2025-0322) */
    // START_CHANGE: ISS-2025-0544 - wave P2.5/P2.7/P2.8: the KnowledgeBase keeps each predicate's
    // clauses in its own gap buffer (no global list). kbSeq is this clause's position in the old
    // global order (assertz counts up, asserta counts down) so getRules() can still return it;
    // kbSlot is a HINT to the clause's slot, which makes an identity retract O(1). Neither is part
    // of clause identity; both are maintained by KnowledgeBase only.
    transient long kbSeq;
    transient int kbSlot = -1;
    // END_CHANGE: ISS-2025-0544

    // START_CHANGE: ISS-2025-0730 - 4.6 wave Q3.1: the file a consulted clause came from (the
    // canonical path of the LOAD's file; an included file's clauses belong to the including load),
    // null for asserted clauses and text consulted from Java. A reconsult removes only the
    // clauses its file owns.
    private String sourceFile;
    public String getSourceFile() { return sourceFile; }
    public void setSourceFile(String file) { this.sourceFile = file; }
    // END_CHANGE: ISS-2025-0730

    public int getSourceLine() { return sourceLine; }
    public void setSourceLine(int line) { this.sourceLine = line; }

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
