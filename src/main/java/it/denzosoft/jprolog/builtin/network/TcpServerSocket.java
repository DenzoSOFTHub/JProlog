package it.denzosoft.jprolog.builtin.network;

// START_CHANGE: ISS-2025-0109 - Network communication built-in predicates
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * tcp_server_socket/2 - tcp_server_socket(+Port, -ServerSocket)
 * Creates a TCP server socket bound to the given port.
 */
public class TcpServerSocket implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException(
                "tcp_server_socket/2 requires 2 arguments: tcp_server_socket(+Port, -ServerSocket).");
        }

        Term portTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term ssTerm = query.getArguments().get(1);

        if (!(portTerm instanceof Number)) {
            throw new PrologEvaluationException("tcp_server_socket/2: Port must be a number.");
        }

        int port = ((Number) portTerm).getValue().intValue();

        try {
            String handle = SocketManager.getInstance().createServerSocket(port);
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (ssTerm.resolveBindings(bindings).unify(new Atom(handle), newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            SocketManager.getInstance().closeServerSocket(handle);
            return false;
        } catch (IOException e) {
            throw new PrologEvaluationException("tcp_server_socket: " + e.getMessage());
        }
    }
}
// END_CHANGE: ISS-2025-0109
