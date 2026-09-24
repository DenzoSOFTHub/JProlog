package it.denzosoft.jprolog.builtin.network;

// START_CHANGE: ISS-2025-0109 - Network communication built-in predicates
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.builtin.LibArgs;
import it.denzosoft.jprolog.core.engine.v4.Errors;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * tcp_connect/3 - tcp_connect(+Host, +Port, -Socket)
 * Creates a TCP client connection.
 */
public class TcpConnect implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) {
            throw LibArgs.unknownArity(query);   // ISS-2025-0693
        }

        Term hostTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term portTerm = query.getArguments().get(1).resolveBindings(bindings);
        Term socketTerm = query.getArguments().get(2);

        if (!(hostTerm instanceof Atom)) {
            throw LibArgs.notA("atom", hostTerm, "tcp_connect", 3, "Host must be an atom");   // ISS-2025-0693
        }
        if (!(portTerm instanceof Number)) {
            throw LibArgs.notA("number", portTerm, "tcp_connect", 3, "Port must be a number");   // ISS-2025-0693
        }

        String host = ((Atom) hostTerm).getName();
        int port = ((Number) portTerm).getValue().intValue();

        try {
            String handle = SocketManager.getInstance().createClientSocket(host, port);
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (socketTerm.resolveBindings(bindings).unify(new Atom(handle), newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            SocketManager.getInstance().closeSocket(handle);
            return false;
        } catch (IOException e) {
            throw Errors.host(e, "read", "socket", null, "tcp_connect", LibArgs.arity(query));   // ISS-2025-0693
        }
    }
}
// END_CHANGE: ISS-2025-0109
