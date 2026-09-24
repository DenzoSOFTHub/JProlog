package it.denzosoft.jprolog.builtin.network;

// START_CHANGE: ISS-2025-0109 - Network communication built-in predicates
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.builtin.LibArgs;
import it.denzosoft.jprolog.core.engine.v4.Errors;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * UDP socket predicates:
 *   udp_socket/2    - udp_socket(+Port, -Socket)           bind to port
 *   udp_send/4      - udp_send(+Socket, +Host, +Port, +Data)  send datagram
 *   udp_receive/4   - udp_receive(+Socket, -Data, -From, +MaxBytes) receive datagram
 *   udp_close/1     - udp_close(+Socket)                   close
 */
public class UdpSocket implements BuiltIn {

    public enum Mode { CREATE, SEND, RECEIVE, CLOSE }

    private final Mode mode;

    private static final Map<String, DatagramSocket> sockets = new ConcurrentHashMap<>();
    private static final AtomicInteger counter = new AtomicInteger(0);

    public UdpSocket(Mode mode) {
        this.mode = mode;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        try {
            switch (mode) {
                case CREATE:  return doCreate(query, bindings, solutions);
                case SEND:    return doSend(query, bindings, solutions);
                case RECEIVE: return doReceive(query, bindings, solutions);
                case CLOSE:   return doClose(query, bindings, solutions);
                default: return false;
            }
        } catch (IOException e) {
            throw Errors.host(e, "read", "socket", null, modeName(), LibArgs.arity(query));   // ISS-2025-0693
        }
    }

    private boolean doCreate(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws IOException {
        List<Term> args = query.getArguments();
        if (args.size() != 2) {
            throw LibArgs.unknownArity(query);   // ISS-2025-0693
        }
        Term portTerm = args.get(0).resolveBindings(bindings);
        if (!(portTerm instanceof Number)) {
            throw LibArgs.notA("number", portTerm, "udp_socket", 2, "Port must be a number");   // ISS-2025-0693
        }
        int port = ((Number) portTerm).getValue().intValue();
        DatagramSocket ds = new DatagramSocket(port);
        String handle = "$udp_" + counter.incrementAndGet();
        sockets.put(handle, ds);

        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (args.get(1).resolveBindings(bindings).unify(new Atom(handle), newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        ds.close();
        return false;
    }

    private boolean doSend(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws IOException {
        List<Term> args = query.getArguments();
        if (args.size() != 4) {
            throw LibArgs.unknownArity(query);   // ISS-2025-0693
        }
        String handle = resolveAtom(args.get(0), bindings, "Socket");
        String host = resolveAtom(args.get(1), bindings, "Host");
        int port = resolveInt(args.get(2), bindings, "Port");
        String data = resolveAtom(args.get(3), bindings, "Data");

        DatagramSocket ds = getSocket(handle);
        byte[] bytes = data.getBytes(StandardCharsets.UTF_8);
        InetAddress addr = InetAddress.getByName(host);
        ds.send(new DatagramPacket(bytes, bytes.length, addr, port));

        solutions.add(bindings);
        return true;
    }

    private boolean doReceive(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws IOException {
        List<Term> args = query.getArguments();
        if (args.size() != 4) {
            throw LibArgs.unknownArity(query);   // ISS-2025-0693
        }
        String handle = resolveAtom(args.get(0), bindings, "Socket");
        int maxBytes = resolveInt(args.get(3), bindings, "MaxBytes");

        DatagramSocket ds = getSocket(handle);
        byte[] buffer = new byte[maxBytes];
        DatagramPacket packet = new DatagramPacket(buffer, buffer.length);
        ds.receive(packet);

        String data = new String(packet.getData(), 0, packet.getLength(), StandardCharsets.UTF_8);
        Term fromTerm = new CompoundTerm(new Atom("from"), Arrays.asList(
            new Atom(packet.getAddress().getHostAddress()),
            new Number(packet.getPort())
        ));

        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (args.get(1).resolveBindings(bindings).unify(new Atom(data), newBindings) &&
            args.get(2).resolveBindings(bindings).unify(fromTerm, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }

    private boolean doClose(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        List<Term> args = query.getArguments();
        if (args.size() != 1) {
            throw LibArgs.unknownArity(query);   // ISS-2025-0693
        }
        String handle = resolveAtom(args.get(0), bindings, "Socket");
        DatagramSocket ds = sockets.remove(handle);
        if (ds != null && !ds.isClosed()) {
            ds.close();
        }
        solutions.add(bindings);
        return true;
    }

    private DatagramSocket getSocket(String handle) {
        DatagramSocket ds = sockets.get(handle);
        if (ds == null) {
            throw Errors.existence("udp_socket", new Atom(handle), modeName(), LibArgs.nameArity(modeName()),
                                   "unknown UDP socket handle");   // ISS-2025-0693
        }
        return ds;
    }

    private String resolveAtom(Term term, Map<String, Term> bindings, String argName) {
        Term resolved = term.resolveBindings(bindings);
        if (!(resolved instanceof Atom)) {
            throw LibArgs.notA("atom", resolved, modeName(), LibArgs.nameArity(modeName()), argName);   // ISS-2025-0693
        }
        return ((Atom) resolved).getName();
    }

    private int resolveInt(Term term, Map<String, Term> bindings, String argName) {
        Term resolved = term.resolveBindings(bindings);
        if (!(resolved instanceof Number)) {
            throw LibArgs.notA("integer", resolved, modeName(), LibArgs.nameArity(modeName()), argName);   // ISS-2025-0693
        }
        return ((Number) resolved).getValue().intValue();
    }

    private String modeName() {
        switch (mode) {
            case CREATE: return "udp_socket";
            case SEND: return "udp_send";
            case RECEIVE: return "udp_receive";
            case CLOSE: return "udp_close";
            default: return "udp";
        }
    }
}
// END_CHANGE: ISS-2025-0109
