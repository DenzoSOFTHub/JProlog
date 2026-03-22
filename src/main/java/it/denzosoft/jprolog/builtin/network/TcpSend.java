package it.denzosoft.jprolog.builtin.network;

// START_CHANGE: ISS-2025-0109 - Network communication built-in predicates
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.IOException;
import java.io.OutputStream;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;

/**
 * tcp_send/2 - tcp_send(+Socket, +Data)
 * Sends a string (atom) over a TCP socket.
 */
public class TcpSend implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 2) {
            throw new PrologEvaluationException("tcp_send/2 requires 2 arguments: tcp_send(+Socket, +Data).");
        }

        Term sockTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term dataTerm = query.getArguments().get(1).resolveBindings(bindings);

        if (!(sockTerm instanceof Atom)) {
            throw new PrologEvaluationException("tcp_send/2: Socket must be an atom handle.");
        }
        if (!(dataTerm instanceof Atom)) {
            throw new PrologEvaluationException("tcp_send/2: Data must be an atom.");
        }

        try {
            Socket socket = SocketManager.getInstance().getSocket(((Atom) sockTerm).getName());
            OutputStream out = socket.getOutputStream();
            out.write(((Atom) dataTerm).getName().getBytes(StandardCharsets.UTF_8));
            out.flush();
            solutions.add(bindings);
            return true;
        } catch (IOException e) {
            throw new PrologEvaluationException("tcp_send: " + e.getMessage());
        }
    }
}
// END_CHANGE: ISS-2025-0109
