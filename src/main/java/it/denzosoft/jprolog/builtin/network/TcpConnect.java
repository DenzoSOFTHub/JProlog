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
 * tcp_connect/3 - tcp_connect(+Host, +Port, -Socket)
 * Creates a TCP client connection.
 */
public class TcpConnect implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        if (query.getArguments().size() != 3) {
            throw new PrologEvaluationException(
                "tcp_connect/3 requires 3 arguments: tcp_connect(+Host, +Port, -Socket).");
        }

        Term hostTerm = query.getArguments().get(0).resolveBindings(bindings);
        Term portTerm = query.getArguments().get(1).resolveBindings(bindings);
        Term socketTerm = query.getArguments().get(2);

        if (!(hostTerm instanceof Atom)) {
            throw new PrologEvaluationException("tcp_connect/3: Host must be an atom.");
        }
        if (!(portTerm instanceof Number)) {
            throw new PrologEvaluationException("tcp_connect/3: Port must be a number.");
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
            throw new PrologEvaluationException("tcp_connect: " + e.getMessage());
        }
    }
}
// END_CHANGE: ISS-2025-0109
