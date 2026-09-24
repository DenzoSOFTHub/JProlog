package it.denzosoft.jprolog.builtin.network;

// START_CHANGE: ISS-2025-0109 - Network communication built-in predicates
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.builtin.LibArgs;
import it.denzosoft.jprolog.core.engine.v4.Errors;

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
            throw LibArgs.unknownArity(query);   // ISS-2025-0693
        }

        Term hostTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term ipTerm = query.getArguments().get(1);

        if (!(hostTerm instanceof Atom)) {
            throw LibArgs.notA("atom", hostTerm, "hostname_address", 2, "Hostname must be an atom");   // ISS-2025-0693
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
            throw Errors.existence("host", hostTerm, "hostname_address", 2, "unknown host");   // ISS-2025-0693
        }
    }
}
// END_CHANGE: ISS-2025-0109
