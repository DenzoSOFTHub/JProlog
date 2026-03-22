package it.denzosoft.jprolog.builtin.network;

// START_CHANGE: ISS-2025-0109 - Network communication built-in predicates
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Manages TCP socket handles for Prolog predicates.
 */
public final class SocketManager {

    private static final SocketManager INSTANCE = new SocketManager();

    private final Map<String, Socket> clientSockets = new ConcurrentHashMap<>();
    private final Map<String, ServerSocket> serverSockets = new ConcurrentHashMap<>();
    private final AtomicInteger counter = new AtomicInteger(0);

    private SocketManager() {}

    public static SocketManager getInstance() {
        return INSTANCE;
    }

    // ---- Client sockets ----

    public String createClientSocket(String host, int port) throws IOException {
        Socket socket = new Socket(host, port);
        String handle = "$socket_" + counter.incrementAndGet();
        clientSockets.put(handle, socket);
        return handle;
    }

    public String registerSocket(Socket socket) {
        String handle = "$socket_" + counter.incrementAndGet();
        clientSockets.put(handle, socket);
        return handle;
    }

    public Socket getSocket(String handle) {
        Socket s = clientSockets.get(handle);
        if (s == null) {
            throw new IllegalArgumentException("Unknown socket handle: " + handle);
        }
        return s;
    }

    public void closeSocket(String handle) throws IOException {
        Socket s = clientSockets.remove(handle);
        if (s != null && !s.isClosed()) {
            s.close();
        }
    }

    // ---- Server sockets ----

    public String createServerSocket(int port) throws IOException {
        ServerSocket ss = new ServerSocket(port);
        String handle = "$server_socket_" + counter.incrementAndGet();
        serverSockets.put(handle, ss);
        return handle;
    }

    public ServerSocket getServerSocket(String handle) {
        ServerSocket ss = serverSockets.get(handle);
        if (ss == null) {
            throw new IllegalArgumentException("Unknown server socket handle: " + handle);
        }
        return ss;
    }

    public void closeServerSocket(String handle) throws IOException {
        ServerSocket ss = serverSockets.remove(handle);
        if (ss != null && !ss.isClosed()) {
            ss.close();
        }
    }

    /** Close all open sockets. */
    public void closeAll() {
        for (String h : clientSockets.keySet()) {
            try { closeSocket(h); } catch (IOException ignored) {}
        }
        for (String h : serverSockets.keySet()) {
            try { closeServerSocket(h); } catch (IOException ignored) {}
        }
    }
}
// END_CHANGE: ISS-2025-0109
