package it.denzosoft.jprolog.builtin.network;

// START_CHANGE: ISS-2025-0109 - Network communication built-in predicates
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * hostname_address/2 - hostname_address(+Hostname, -IPAddress)
 * Resolves a hostname to its IP address.
 */
public class HostLookup implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException(
                "hostname_address/2 requires 2 arguments: hostname_address(+Host, -IP).");
        }

        Term hostTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term ipTerm = query.getArguments().get(1);

        if (!(hostTerm instanceof Atom)) {
            throw new PrologEvaluationException("hostname_address/2: Hostname must be an atom.");
        }

        try {
            InetAddress addr = InetAddress.getByName(((Atom) hostTerm).getName());
            String ip = addr.getHostAddress();

            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (ipTerm.resolveBindings(bindings).unify(new Atom(ip), newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            return false;
        } catch (UnknownHostException e) {
            throw new PrologEvaluationException("hostname_address: Unknown host: " + ((Atom) hostTerm).getName());
        }
    }
}
// END_CHANGE: ISS-2025-0109
