package it.denzosoft.jprolog.builtin.http;

// START_CHANGE: ISS-2025-0125 - HTTP server/client built-in predicates
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.utils.CollectionUtils;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;

import java.io.*;
import java.net.*;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Logger;

/**
 * HTTP server and client predicates:
 *   http_server/2       - http_server(+Port, -ServerHandle)
 *   http_stop/1         - http_stop(+ServerHandle)
 *   http_handler/3      - http_handler(+ServerHandle, +Path, +HandlerAtom)
 *   http_get_request/2  - http_get_request(+ServerHandle, -Request)
 *   http_reply/4        - http_reply(+ServerHandle, +RequestId, +StatusCode, +Body)
 *   http_reply_json/3   - http_reply_json(+ServerHandle, +RequestId, +JsonTerm)
 *   http_client_get/2   - http_client_get(+Url, -Response)
 *   http_client_post/3  - http_client_post(+Url, +PostBody, -Response)
 *   http_open/3         - http_open(+Url, +Options, -Response)
 *   url_encode/2        - url_encode(+Text, -Encoded)
 *   url_decode/2        - url_decode(+Encoded, -Text)
 */
public class HttpServerPredicates implements BuiltIn {

    public enum Mode {
        HTTP_SERVER, HTTP_STOP, HTTP_HANDLER, HTTP_GET_REQUEST,
        HTTP_REPLY, HTTP_REPLY_JSON,
        HTTP_CLIENT_GET, HTTP_CLIENT_POST, HTTP_OPEN,
        URL_ENCODE, URL_DECODE
    }

    private final Mode mode;

    // START_CHANGE: ISS-2025-0173 - Add logger and max queue size to prevent unbounded queue growth
    private static final Logger LOGGER = Logger.getLogger(HttpServerPredicates.class.getName());
    private static final int MAX_REQUEST_QUEUE_SIZE = 1000;
    // END_CHANGE: ISS-2025-0173

    /** Active servers keyed by handle name. */
    private static final ConcurrentHashMap<String, HttpServer> activeServers = new ConcurrentHashMap<>();

    /** Incoming request queues per server handle. */
    private static final ConcurrentHashMap<String, LinkedBlockingQueue<RequestContext>> requestQueues =
            new ConcurrentHashMap<>();

    /** Pending exchanges waiting for a reply, keyed by request ID. */
    private static final ConcurrentHashMap<String, HttpExchange> pendingExchanges = new ConcurrentHashMap<>();

    /** Counter for unique server handles. */
    private static final AtomicInteger serverCounter = new AtomicInteger(0);

    /** Counter for unique request IDs. */
    private static final AtomicInteger requestCounter = new AtomicInteger(0);

    /** Shared HTTP client for outgoing requests. */
    private static final HttpClient httpClient = HttpClient.newBuilder()
            .connectTimeout(Duration.ofSeconds(30))
            .build();

    public HttpServerPredicates(Mode mode) {
        this.mode = mode;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        try {
            switch (mode) {
                case HTTP_SERVER:      return doHttpServer(query, bindings, solutions);
                case HTTP_STOP:        return doHttpStop(query, bindings, solutions);
                case HTTP_HANDLER:     return doHttpHandler(query, bindings, solutions);
                case HTTP_GET_REQUEST: return doHttpGetRequest(query, bindings, solutions);
                case HTTP_REPLY:       return doHttpReply(query, bindings, solutions);
                case HTTP_REPLY_JSON:  return doHttpReplyJson(query, bindings, solutions);
                case HTTP_CLIENT_GET:  return doHttpClientGet(query, bindings, solutions);
                case HTTP_CLIENT_POST: return doHttpClientPost(query, bindings, solutions);
                case HTTP_OPEN:        return doHttpOpen(query, bindings, solutions);
                case URL_ENCODE:       return doUrlEncode(query, bindings, solutions);
                case URL_DECODE:       return doUrlDecode(query, bindings, solutions);
                default: return false;
            }
        } catch (PrologEvaluationException e) {
            throw e;
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            throw new PrologEvaluationException(modeName() + ": " + e.getMessage());
        }
    }

    // ---- Server predicates ----

    /**
     * http_server(+Port, -ServerHandle)
     * Start an HTTP server on the given port.
     */
    private boolean doHttpServer(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws Exception {
        checkArity(query, 2);
        Term portTerm = query.getArguments().get(0).resolveBindings(bindings);
        if (!(portTerm instanceof Number)) {
            throw new PrologEvaluationException("http_server/2: first argument must be a port number.");
        }
        int port = ((Number) portTerm).getValue().intValue();

        String handle = "http_server_" + serverCounter.incrementAndGet();
        HttpServer server = HttpServer.create(new InetSocketAddress(port), 0);
        LinkedBlockingQueue<RequestContext> queue = new LinkedBlockingQueue<>();

        // Default root handler that queues all requests
        server.createContext("/", new QueueingHandler(handle, queue));
        server.setExecutor(Executors.newCachedThreadPool(r -> {
            Thread t = new Thread(r, "jprolog-http-" + handle);
            t.setDaemon(true);
            return t;
        }));
        server.start();

        activeServers.put(handle, server);
        requestQueues.put(handle, queue);

        return unify(query.getArguments().get(1), new Atom(handle), bindings, solutions);
    }

    /**
     * http_stop(+ServerHandle)
     * Stop a running server.
     */
    private boolean doHttpStop(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 1);
        String handle = resolveAtom(query.getArguments().get(0), bindings);
        HttpServer server = activeServers.remove(handle);
        if (server == null) {
            throw new PrologEvaluationException("http_stop/1: no server with handle '" + handle + "'.");
        }
        server.stop(1);
        // START_CHANGE: ISS-2025-0173 - Clean up request queue and pending exchanges on server stop
        LinkedBlockingQueue<RequestContext> queue = requestQueues.remove(handle);
        if (queue != null) {
            queue.clear();
        }
        // END_CHANGE: ISS-2025-0173
        // Drain pending exchanges for this server
        pendingExchanges.entrySet().removeIf(e -> e.getKey().startsWith(handle + "_"));
        solutions.add(bindings);
        return true;
    }

    /**
     * http_handler(+ServerHandle, +Path, +HandlerAtom)
     * Register a path handler. Creates a new context on the server that queues requests.
     */
    private boolean doHttpHandler(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        checkArity(query, 3);
        String handle = resolveAtom(query.getArguments().get(0), bindings);
        String path = resolveAtom(query.getArguments().get(1), bindings);
        // HandlerAtom is stored for identification but all requests go to the queue
        resolveAtom(query.getArguments().get(2), bindings);

        HttpServer server = activeServers.get(handle);
        if (server == null) {
            throw new PrologEvaluationException("http_handler/3: no server with handle '" + handle + "'.");
        }
        LinkedBlockingQueue<RequestContext> queue = requestQueues.get(handle);
        server.createContext(path, new QueueingHandler(handle, queue));
        solutions.add(bindings);
        return true;
    }

    /**
     * http_get_request(+ServerHandle, -Request)
     * Block and wait for the next incoming request (up to 30s).
     * Returns request(Method, Path, Headers, Body, RequestId).
     */
    private boolean doHttpGetRequest(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws Exception {
        checkArity(query, 2);
        String handle = resolveAtom(query.getArguments().get(0), bindings);
        LinkedBlockingQueue<RequestContext> queue = requestQueues.get(handle);
        if (queue == null) {
            throw new PrologEvaluationException("http_get_request/2: no server with handle '" + handle + "'.");
        }

        RequestContext ctx = queue.poll(30, TimeUnit.SECONDS);
        if (ctx == null) {
            return false; // timeout, no request received
        }

        // Store exchange for later reply
        pendingExchanges.put(ctx.id, ctx.exchange);

        // Build header list: [header(Name, Value), ...]
        List<Term> headerTerms = new ArrayList<>();
        for (Map.Entry<String, String> h : ctx.headers.entrySet()) {
            List<Term> headerArgs = Arrays.asList(new Atom(h.getKey()), new Atom(h.getValue()));
            headerTerms.add(new CompoundTerm(new Atom("header"), headerArgs));
        }
        Term headerList = CollectionUtils.createListTerm(headerTerms);

        // Build request(Method, Path, Headers, Body, RequestId)
        List<Term> reqArgs = Arrays.asList(
                new Atom(ctx.method.toLowerCase()),
                new Atom(ctx.path),
                headerList,
                new Atom(ctx.body),
                new Atom(ctx.id)
        );
        Term requestTerm = new CompoundTerm(new Atom("request"), reqArgs);

        return unify(query.getArguments().get(1), requestTerm, bindings, solutions);
    }

    /**
     * http_reply(+ServerHandle, +RequestId, +StatusCode, +Body)
     * Send a response to a pending request.
     */
    private boolean doHttpReply(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws Exception {
        checkArity(query, 4);
        // ServerHandle is accepted but we key on RequestId
        resolveAtom(query.getArguments().get(0), bindings);
        String requestId = resolveAtom(query.getArguments().get(1), bindings);
        Term statusTerm = query.getArguments().get(2).resolveBindings(bindings);
        if (!(statusTerm instanceof Number)) {
            throw new PrologEvaluationException("http_reply/4: status code must be a number.");
        }
        int statusCode = ((Number) statusTerm).getValue().intValue();
        String body = resolveAtom(query.getArguments().get(3), bindings);

        HttpExchange exchange = pendingExchanges.remove(requestId);
        if (exchange == null) {
            throw new PrologEvaluationException("http_reply/4: no pending request with id '" + requestId + "'.");
        }

        sendResponse(exchange, statusCode, "text/plain; charset=utf-8", body);
        solutions.add(bindings);
        return true;
    }

    /**
     * http_reply_json(+ServerHandle, +RequestId, +JsonTerm)
     * Send a JSON response. JsonTerm is an atom containing JSON text.
     */
    private boolean doHttpReplyJson(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws Exception {
        checkArity(query, 3);
        resolveAtom(query.getArguments().get(0), bindings);
        String requestId = resolveAtom(query.getArguments().get(1), bindings);
        String jsonBody = resolveAtom(query.getArguments().get(2), bindings);

        HttpExchange exchange = pendingExchanges.remove(requestId);
        if (exchange == null) {
            throw new PrologEvaluationException("http_reply_json/3: no pending request with id '" + requestId + "'.");
        }

        sendResponse(exchange, 200, "application/json; charset=utf-8", jsonBody);
        solutions.add(bindings);
        return true;
    }

    // ---- Client predicates ----

    /**
     * http_client_get(+Url, -Response)
     * Make an HTTP GET request. Returns response(StatusCode, Headers, Body).
     */
    private boolean doHttpClientGet(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws Exception {
        checkArity(query, 2);
        String url = resolveAtom(query.getArguments().get(0), bindings);

        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(url))
                .GET()
                .timeout(Duration.ofSeconds(30))
                .build();

        HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());
        Term responseTerm = buildResponseTerm(response);
        return unify(query.getArguments().get(1), responseTerm, bindings, solutions);
    }

    /**
     * http_client_post(+Url, +PostBody, -Response)
     * Make an HTTP POST request.
     */
    private boolean doHttpClientPost(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws Exception {
        checkArity(query, 3);
        String url = resolveAtom(query.getArguments().get(0), bindings);
        String postBody = resolveAtom(query.getArguments().get(1), bindings);

        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(url))
                .POST(HttpRequest.BodyPublishers.ofString(postBody))
                .header("Content-Type", "text/plain; charset=utf-8")
                .timeout(Duration.ofSeconds(30))
                .build();

        HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());
        Term responseTerm = buildResponseTerm(response);
        return unify(query.getArguments().get(2), responseTerm, bindings, solutions);
    }

    /**
     * http_open(+Url, +Options, -Response)
     * General HTTP request with options list.
     * Options: [method(get|post|put|delete), body(Data), header(Name, Value), ...]
     */
    private boolean doHttpOpen(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws Exception {
        checkArity(query, 3);
        String url = resolveAtom(query.getArguments().get(0), bindings);
        Term optionsTerm = query.getArguments().get(1).resolveBindings(bindings);

        List<Term> options = CollectionUtils.termToList(optionsTerm);
        if (options == null) {
            throw new PrologEvaluationException("http_open/3: second argument must be a list of options.");
        }

        String method = "GET";
        String body = "";
        Map<String, String> headers = new LinkedHashMap<>();

        for (Term opt : options) {
            Term resolved = opt.resolveBindings(bindings);
            if (resolved instanceof CompoundTerm) {
                CompoundTerm ct = (CompoundTerm) resolved;
                String functor = ct.getFunctor().getName();
                if ("method".equals(functor) && ct.getArguments().size() == 1) {
                    method = resolveTermToString(ct.getArguments().get(0), bindings).toUpperCase();
                } else if ("body".equals(functor) && ct.getArguments().size() == 1) {
                    body = resolveTermToString(ct.getArguments().get(0), bindings);
                } else if ("header".equals(functor) && ct.getArguments().size() == 2) {
                    String hName = resolveTermToString(ct.getArguments().get(0), bindings);
                    String hValue = resolveTermToString(ct.getArguments().get(1), bindings);
                    headers.put(hName, hValue);
                }
            }
        }

        HttpRequest.Builder builder = HttpRequest.newBuilder()
                .uri(URI.create(url))
                .timeout(Duration.ofSeconds(30));

        for (Map.Entry<String, String> h : headers.entrySet()) {
            builder.header(h.getKey(), h.getValue());
        }

        switch (method) {
            case "POST":
                builder.POST(HttpRequest.BodyPublishers.ofString(body));
                break;
            case "PUT":
                builder.PUT(HttpRequest.BodyPublishers.ofString(body));
                break;
            case "DELETE":
                builder.DELETE();
                break;
            default:
                builder.GET();
                break;
        }

        HttpResponse<String> response = httpClient.send(builder.build(), HttpResponse.BodyHandlers.ofString());
        Term responseTerm = buildResponseTerm(response);
        return unify(query.getArguments().get(2), responseTerm, bindings, solutions);
    }

    // ---- URL encoding predicates ----

    /**
     * url_encode(+Text, -Encoded)
     */
    private boolean doUrlEncode(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws Exception {
        checkArity(query, 2);
        String text = resolveAtom(query.getArguments().get(0), bindings);
        String encoded = URLEncoder.encode(text, StandardCharsets.UTF_8.name());
        return unify(query.getArguments().get(1), new Atom(encoded), bindings, solutions);
    }

    /**
     * url_decode(+Encoded, -Text)
     */
    private boolean doUrlDecode(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws Exception {
        checkArity(query, 2);
        String encoded = resolveAtom(query.getArguments().get(0), bindings);
        String decoded = URLDecoder.decode(encoded, StandardCharsets.UTF_8.name());
        return unify(query.getArguments().get(1), new Atom(decoded), bindings, solutions);
    }

    // ---- Internal helpers ----

    /**
     * Queuing handler that stores incoming HTTP requests for Prolog to pick up.
     */
    private static class QueueingHandler implements HttpHandler {
        private final String serverHandle;
        private final LinkedBlockingQueue<RequestContext> queue;

        QueueingHandler(String serverHandle, LinkedBlockingQueue<RequestContext> queue) {
            this.serverHandle = serverHandle;
            this.queue = queue;
        }

        @Override
        public void handle(HttpExchange exchange) throws IOException {
            String id = serverHandle + "_req_" + requestCounter.incrementAndGet();
            String method = exchange.getRequestMethod();
            String path = exchange.getRequestURI().getPath();
            String queryString = exchange.getRequestURI().getQuery();
            if (queryString != null) {
                path = path + "?" + queryString;
            }

            // Read headers (first value per header name)
            Map<String, String> headers = new LinkedHashMap<>();
            for (Map.Entry<String, List<String>> entry : exchange.getRequestHeaders().entrySet()) {
                if (entry.getValue() != null && !entry.getValue().isEmpty()) {
                    headers.put(entry.getKey().toLowerCase(), entry.getValue().get(0));
                }
            }

            // Read body
            String body = "";
            try (InputStream is = exchange.getRequestBody()) {
                byte[] bytes = readAllBytes(is);
                body = new String(bytes, StandardCharsets.UTF_8);
            }

            RequestContext ctx = new RequestContext();
            ctx.id = id;
            ctx.method = method;
            ctx.path = path;
            ctx.headers = headers;
            ctx.body = body;
            ctx.exchange = exchange;

            // START_CHANGE: ISS-2025-0173 - Drop oldest entry when queue exceeds max size
            while (queue.size() >= MAX_REQUEST_QUEUE_SIZE) {
                RequestContext dropped = queue.poll();
                if (dropped != null && dropped.exchange != null) {
                    try {
                        sendErrorResponse(dropped.exchange, 503, "Server queue full");
                    } catch (IOException ignored) {
                        // Best effort to respond to dropped request
                    }
                    LOGGER.warning("HTTP request queue for " + serverHandle
                        + " exceeded " + MAX_REQUEST_QUEUE_SIZE + " entries. Dropped oldest request: " + dropped.id);
                }
            }
            // END_CHANGE: ISS-2025-0173
            queue.offer(ctx);
        }

        // START_CHANGE: ISS-2025-0173 - Helper to send error response for dropped requests
        private static void sendErrorResponse(HttpExchange exchange, int statusCode, String body) throws IOException {
            byte[] responseBytes = body.getBytes(StandardCharsets.UTF_8);
            exchange.getResponseHeaders().set("Content-Type", "text/plain; charset=utf-8");
            exchange.sendResponseHeaders(statusCode, responseBytes.length);
            try (OutputStream os = exchange.getResponseBody()) {
                os.write(responseBytes);
            }
        }
        // END_CHANGE: ISS-2025-0173
    }

    /** Read all bytes from an InputStream (Java 8 compatible). */
    private static byte[] readAllBytes(InputStream is) throws IOException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        byte[] buf = new byte[4096];
        int n;
        while ((n = is.read(buf)) != -1) {
            baos.write(buf, 0, n);
        }
        return baos.toByteArray();
    }

    /** Incoming request context. */
    private static class RequestContext {
        String id;
        String method;
        String path;
        Map<String, String> headers;
        String body;
        HttpExchange exchange;
    }

    /** Send an HTTP response and close the exchange. */
    private void sendResponse(HttpExchange exchange, int statusCode, String contentType, String body)
            throws IOException {
        byte[] responseBytes = body.getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().set("Content-Type", contentType);
        exchange.sendResponseHeaders(statusCode, responseBytes.length);
        try (OutputStream os = exchange.getResponseBody()) {
            os.write(responseBytes);
        }
    }

    /** Build a response(StatusCode, Headers, Body) term from an HttpResponse. */
    private Term buildResponseTerm(HttpResponse<String> response) {
        List<Term> headerTerms = new ArrayList<>();
        for (Map.Entry<String, List<String>> entry : response.headers().map().entrySet()) {
            String name = entry.getKey();
            for (String value : entry.getValue()) {
                List<Term> headerArgs = Arrays.asList(new Atom(name), new Atom(value));
                headerTerms.add(new CompoundTerm(new Atom("header"), headerArgs));
            }
        }
        Term headerList = CollectionUtils.createListTerm(headerTerms);
        String bodyStr = response.body() != null ? response.body() : "";

        List<Term> respArgs = Arrays.asList(
                new Number(response.statusCode()),
                headerList,
                new Atom(bodyStr)
        );
        return new CompoundTerm(new Atom("response"), respArgs);
    }

    /** Resolve a term to its string representation (atom name). */
    private String resolveTermToString(Term term, Map<String, Term> bindings) {
        Term resolved = term.resolveBindings(bindings);
        if (resolved instanceof Atom) {
            return ((Atom) resolved).getName();
        }
        return resolved.toString();
    }

    private void checkArity(Term query, int expected) {
        if (query.getArguments().size() != expected) {
            throw new PrologEvaluationException(modeName() + " requires " + expected + " arguments.");
        }
    }

    private String resolveAtom(Term term, Map<String, Term> bindings) {
        Term resolved = term.resolveBindings(bindings);
        if (!(resolved instanceof Atom)) {
            throw new PrologEvaluationException(modeName() + ": argument must be an atom, got: " + resolved);
        }
        return ((Atom) resolved).getName();
    }

    private boolean unify(Term target, Term value, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        Map<String, Term> nb = new HashMap<>(bindings);
        if (target.resolveBindings(bindings).unify(value, nb)) {
            solutions.add(nb);
            return true;
        }
        return false;
    }

    private String modeName() {
        return mode.name().toLowerCase();
    }
}
// END_CHANGE: ISS-2025-0125
