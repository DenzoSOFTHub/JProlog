package it.denzosoft.jprolog.builtin.network;

// START_CHANGE: ISS-2025-0109 - Network communication built-in predicates
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.IOException;
import java.io.InputStream;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * tcp_receive/3 - tcp_receive(+Socket, -Data, +MaxBytes)
 * Receives data from a TCP socket up to MaxBytes.
 */
public class TcpReceive implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) {
            throw new PrologEvaluationException(
                "tcp_receive/3 requires 3 arguments: tcp_receive(+Socket, -Data, +MaxBytes).");
        }

        Term sockTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term dataTerm = query.getArguments().get(1);
        Term maxTerm = query.getArguments().get(2).resolveBindings(bindings);

        if (!(sockTerm instanceof Atom)) {
            throw new PrologEvaluationException("tcp_receive/3: Socket must be an atom handle.");
        }
        if (!(maxTerm instanceof Number)) {
            throw new PrologEvaluationException("tcp_receive/3: MaxBytes must be a number.");
        }

        int maxBytes = ((Number) maxTerm).getValue().intValue();
        if (maxBytes <= 0 || maxBytes > 1048576) {
            throw new PrologEvaluationException("tcp_receive/3: MaxBytes must be between 1 and 1048576.");
        }

        try {
            Socket socket = SocketManager.getInstance().getSocket(((Atom) sockTerm).getName());
            InputStream in = socket.getInputStream();
            byte[] buffer = new byte[maxBytes];
            int bytesRead = in.read(buffer);

            if (bytesRead == -1) {
                // End of stream
                Map<String, Term> newBindings = new HashMap<>(bindings);
                if (dataTerm.resolveBindings(bindings).unify(new Atom("end_of_stream"), newBindings)) {
                    solutions.add(newBindings);
                    return true;
                }
                return false;
            }

            String data = new String(buffer, 0, bytesRead, StandardCharsets.UTF_8);
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (dataTerm.resolveBindings(bindings).unify(new Atom(data), newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            return false;
        } catch (IOException e) {
            throw new PrologEvaluationException("tcp_receive: " + e.getMessage());
        }
    }
}
// END_CHANGE: ISS-2025-0109
