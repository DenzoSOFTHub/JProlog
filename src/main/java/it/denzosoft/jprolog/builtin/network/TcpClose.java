package it.denzosoft.jprolog.builtin.network;

// START_CHANGE: ISS-2025-0109 - Network communication built-in predicates
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.builtin.LibArgs;
import it.denzosoft.jprolog.core.engine.v4.Errors;

import java.io.IOException;
import java.util.List;
import java.util.Map;

/**
 * tcp_close/1 - tcp_close(+SocketOrServerSocket)
 * Closes a client socket or server socket.
 */
public class TcpClose implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 1) {
            throw LibArgs.unknownArity(query);   // ISS-2025-0693
        }

        Term handleTerm = query.getArguments().get(0).resolveBindings(bindings);
        if (!(handleTerm instanceof Atom)) {
            throw LibArgs.notA("atom", handleTerm, "tcp_close", 1, "Handle must be an atom");   // ISS-2025-0693
        }

        String handle = ((Atom) handleTerm).getName();
        try {
            if (handle.startsWith("$server_socket_")) {
                SocketManager.getInstance().closeServerSocket(handle);
            } else {
                SocketManager.getInstance().closeSocket(handle);
            }
            solutions.add(bindings);
            return true;
        } catch (IOException e) {
            throw Errors.host(e, "read", "socket", null, "tcp_close", LibArgs.arity(query));   // ISS-2025-0693
        }
    }
}
// END_CHANGE: ISS-2025-0109
