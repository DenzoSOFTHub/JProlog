package it.denzosoft.jprolog.builtin.network;

// START_CHANGE: ISS-2025-0109 - Network communication built-in predicates
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * tcp_accept/2 - tcp_accept(+ServerSocket, -ClientSocket)
 * Accepts an incoming connection on a server socket.
 */
public class TcpAccept implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException(
                "tcp_accept/2 requires 2 arguments: tcp_accept(+ServerSocket, -ClientSocket).");
        }

        Term ssTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term clientTerm = query.getArguments().get(1);

        if (!(ssTerm instanceof Atom)) {
            throw new PrologEvaluationException("tcp_accept/2: ServerSocket must be an atom handle.");
        }

        try {
            ServerSocket ss = SocketManager.getInstance().getServerSocket(((Atom) ssTerm).getName());
            Socket client = ss.accept();
            String handle = SocketManager.getInstance().registerSocket(client);

            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (clientTerm.resolveBindings(bindings).unify(new Atom(handle), newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            SocketManager.getInstance().closeSocket(handle);
            return false;
        } catch (IOException e) {
            throw new PrologEvaluationException("tcp_accept: " + e.getMessage());
        }
    }
}
// END_CHANGE: ISS-2025-0109
