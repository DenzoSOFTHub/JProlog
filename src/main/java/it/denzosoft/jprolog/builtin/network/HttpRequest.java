package it.denzosoft.jprolog.builtin.network;

// START_CHANGE: ISS-2025-0109 - Network communication built-in predicates
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.builtin.LibArgs;
import it.denzosoft.jprolog.core.engine.v4.Errors;

import java.io.*;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * http_request/4 - http_request(+Method, +URL, -StatusCode, -Body)
 * Performs an HTTP request (GET, POST, PUT, DELETE).
 *
 * http_post/4 - http_post(+URL, +RequestBody, -StatusCode, -ResponseBody)
 * Convenience for POST with a body.
 */
public class HttpRequest implements BuiltIn {

    public enum Mode { REQUEST, POST }

    private final Mode mode;

    public HttpRequest(Mode mode) {
        this.mode = mode;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        List<Term> args = query.getArguments();

        try {
            switch (mode) {
                case REQUEST: return executeRequest(args, bindings, solutions);
                case POST:    return executePost(args, bindings, solutions);
                default: return false;
            }
        } catch (IOException e) {
            throw Errors.host(e, "read", "socket", null, modeName(), LibArgs.arity(query));   // ISS-2025-0693
        }
    }

    private boolean executeRequest(List<Term> args, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws IOException {
        if (args.size() != 4) {
            throw LibArgs.unknownArity("http_request", args.size());   // ISS-2025-0693
        }

        Term methodTerm = args.get(0).resolveBindings(bindings);
        Term urlTerm = args.get(1).resolveBindings(bindings);

        if (!(methodTerm instanceof Atom) || !(urlTerm instanceof Atom)) {   // ISS-2025-0693
            Term bad = !(methodTerm instanceof Atom) ? methodTerm : urlTerm;
            throw LibArgs.notA("atom", bad, "http_request", 4, "Method and URL");
        }

        String method = ((Atom) methodTerm).getName().toUpperCase();
        String urlStr = ((Atom) urlTerm).getName();

        HttpURLConnection conn = (HttpURLConnection) new URL(urlStr).openConnection();
        // START_CHANGE: ISS-2025-0257 - disconnect in finally so the connection/socket is not
        // leaked when getResponseCode()/readResponseBody() throws (timeout, reset, bad response).
        try {
            conn.setRequestMethod(method);
            conn.setConnectTimeout(30000);
            conn.setReadTimeout(30000);

            int statusCode = conn.getResponseCode();
            String body = readResponseBody(conn);
            return unifyResult(args.get(2), args.get(3), statusCode, body, bindings, solutions);
        } finally {
            conn.disconnect();
        }
        // END_CHANGE: ISS-2025-0257
    }

    private boolean executePost(List<Term> args, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws IOException {
        if (args.size() != 4) {
            throw LibArgs.unknownArity("http_post", args.size());   // ISS-2025-0693
        }

        Term urlTerm = args.get(0).resolveBindings(bindings);
        Term reqBodyTerm = args.get(1).resolveBindings(bindings);

        if (!(urlTerm instanceof Atom) || !(reqBodyTerm instanceof Atom)) {   // ISS-2025-0693
            Term bad = !(urlTerm instanceof Atom) ? urlTerm : reqBodyTerm;
            throw LibArgs.notA("atom", bad, "http_post", 4, "URL and RequestBody");
        }

        String urlStr = ((Atom) urlTerm).getName();
        String reqBody = ((Atom) reqBodyTerm).getName();

        HttpURLConnection conn = (HttpURLConnection) new URL(urlStr).openConnection();
        // START_CHANGE: ISS-2025-0257 - disconnect in finally (see executeRequest).
        try {
            conn.setRequestMethod("POST");
            conn.setDoOutput(true);
            conn.setConnectTimeout(30000);
            conn.setReadTimeout(30000);
            conn.setRequestProperty("Content-Type", "application/x-www-form-urlencoded");

            try (OutputStream os = conn.getOutputStream()) {
                os.write(reqBody.getBytes(StandardCharsets.UTF_8));
            }

            int statusCode = conn.getResponseCode();
            String body = readResponseBody(conn);
            return unifyResult(args.get(2), args.get(3), statusCode, body, bindings, solutions);
        } finally {
            conn.disconnect();
        }
        // END_CHANGE: ISS-2025-0257
    }

    private boolean unifyResult(Term statusTerm, Term bodyTerm, int statusCode, String body,
            Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        Map<String, Term> newBindings = new HashMap<>(bindings);
        Term statusAtom = new Number(statusCode);
        Term bodyAtom = new Atom(body);

        if (statusTerm.resolveBindings(bindings).unify(statusAtom, newBindings) &&
            bodyTerm.resolveBindings(bindings).unify(bodyAtom, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }

    private String readResponseBody(HttpURLConnection conn) throws IOException {
        InputStream is;
        try {
            is = conn.getInputStream();
        } catch (IOException e) {
            is = conn.getErrorStream();
            if (is == null) return "";
        }

        StringBuilder sb = new StringBuilder();
        try (BufferedReader br = new BufferedReader(new InputStreamReader(is, StandardCharsets.UTF_8))) {
            String line;
            while ((line = br.readLine()) != null) {
                if (sb.length() > 0) sb.append('\n');
                sb.append(line);
            }
        }
        return sb.toString();
    }

    private String modeName() {
        return mode == Mode.REQUEST ? "http_request" : "http_post";
    }
}
// END_CHANGE: ISS-2025-0109
