package it.denzosoft.jprolog.builtin.list;

import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;


public class Length implements BuiltIn {

    // START_CHANGE: ISS-2025-0215 - global counter for fresh vars to prevent collisions across calls
    private static final AtomicLong FRESH_COUNTER = new AtomicLong(0);
    // END_CHANGE: ISS-2025-0215

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("length/2 requires exactly 2 arguments.");
        }

        // START_CHANGE: ISS-2025-0080 - Resolve bindings before type/ground checks
        Term list = query.getArguments().get(0).resolveBindings(bindings);
        Term lengthTerm = query.getArguments().get(1).resolveBindings(bindings);
        // END_CHANGE: ISS-2025-0080

        if (list.isGround()) {
            // Case: length(GroundList, Length)
            int count = countElements(list);
            // START_CHANGE: ISS-2025-0182 - Built-in predicate bug fixes
            // Fail gracefully for malformed lists instead of producing wrong results
            if (count == -1) {
                return false;
            }
            // END_CHANGE: ISS-2025-0182
            Term length = new Number(count);
            if (lengthTerm.unify(length, bindings)) {
                solutions.add(new HashMap<>(bindings));
                return true;
            }
            return false;
        } else if (lengthTerm.isGround() && lengthTerm instanceof Number) {
            // Case: length(List, GroundInteger)
            int expectedLength = (int) Math.round(((Number) lengthTerm).getValue());
            if (expectedLength < 0) {
                return false; // Can't have negative length lists
            }
            
            Term generatedList = generateList(expectedLength);
            if (list.unify(generatedList, bindings)) {
                solutions.add(new HashMap<>(bindings));
                return true;
            }
            return false;
        } else {
            // START_CHANGE: ISS-2025-0079 - Return false instead of throwing for normal failure
            return false;
            // END_CHANGE: ISS-2025-0079
        }
    }

    private int countElements(Term list) {
        // START_CHANGE: ISS-2025-0216 - cycle detection prevents infinite loop on X = [a|X]
        java.util.IdentityHashMap<Term, Boolean> visited = new java.util.IdentityHashMap<>();
        int count = 0;
        Term current = list;

        while (current instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) current;
            if (compound.getName().equals(".") && compound.getArguments().size() == 2) {
                if (visited.put(compound, Boolean.TRUE) != null) return -1; // cycle
                count++;
                current = compound.getArguments().get(1);
            } else {
                break;
            }
        }

        if (current instanceof Atom && ((Atom) current).getName().equals("[]")) {
            return count;
        }
        return -1;
        // END_CHANGE: ISS-2025-0216
    }

    private Term generateList(int length) {
        if (length < 0) {
            throw new IllegalArgumentException("List length must be non-negative");
        }
        
        Term current = new Atom("[]");
        // START_CHANGE: ISS-2025-0215 - global counter prevents cross-call collisions
        for (int i = 0; i < length; i++) {
            List<Term> args = new ArrayList<>();
            args.add(new Variable("_Glen" + FRESH_COUNTER.incrementAndGet()));
            args.add(current);
            current = new CompoundTerm(new Atom("."), args);
        }
        // END_CHANGE: ISS-2025-0215
        return current;
    }
}
