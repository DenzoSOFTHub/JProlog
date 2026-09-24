package it.denzosoft.jprolog.builtin.json;

// START_CHANGE: ISS-2025-0113 - JSON built-in predicates
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.v4.Errors;
import it.denzosoft.jprolog.builtin.LibArgs;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.engine.v4.Errors;
import it.denzosoft.jprolog.core.utils.CollectionUtils;

import java.util.*;

/**
 * JSON predicates:
 *   json_parse/2    - json_parse(+JsonString, -Term)     parse JSON to Prolog term
 *   json_serialize/2 - json_serialize(+Term, -JsonString) serialize Prolog term to JSON
 *   json_get/3      - json_get(+JsonTerm, +Path, -Value) extract value by path
 *   json_set/4      - json_set(+JsonTerm, +Path, +Value, -NewJsonTerm) set value
 *   json_keys/2     - json_keys(+JsonObject, -Keys)      get object keys
 *   json_member/3   - json_member(+JsonObject, ?Key, ?Value) member access
 *
 * JSON mapping:
 *   JSON object  -> json([key1=val1, key2=val2, ...])
 *   JSON array   -> [elem1, elem2, ...]
 *   JSON string  -> atom
 *   JSON number  -> number
 *   JSON true    -> @(true)
 *   JSON false   -> @(false)
 *   JSON null    -> @(null)
 */
public class JsonPredicates implements BuiltIn {

    public enum Mode { PARSE, SERIALIZE, GET, SET, KEYS, MEMBER }

    private final Mode mode;

    public JsonPredicates(Mode mode) {
        this.mode = mode;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        switch (mode) {
            case PARSE:     return doParse(query, bindings, solutions);
            case SERIALIZE: return doSerialize(query, bindings, solutions);
            case GET:       return doGet(query, bindings, solutions);
            case SET:       return doSet(query, bindings, solutions);
            case KEYS:      return doKeys(query, bindings, solutions);
            case MEMBER:    return doMember(query, bindings, solutions);
            default: return false;
        }
    }

    // ---- PARSE ----
    private boolean doParse(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        List<Term> args = query.getArguments();
        if (args.size() != 2) throw LibArgs.unknownArity(query);   // ISS-2025-0688
        String json = resolveAtom(args.get(0), bindings);
        Term result = parseJson(json.trim(), new int[]{0});
        return unify(args.get(1), result, bindings, solutions);
    }

    // ---- SERIALIZE ----
    private boolean doSerialize(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        List<Term> args = query.getArguments();
        if (args.size() != 2) throw LibArgs.unknownArity(query);   // ISS-2025-0688
        Term term = LibArgs.bound(query, 0, bindings, "json_serialize", "the term");   // ISS-2025-0688
        String json = serializeTerm(term);
        return unify(args.get(1), new Atom(json), bindings, solutions);
    }

    // ---- GET ----
    private boolean doGet(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        List<Term> args = query.getArguments();
        if (args.size() != 3) throw LibArgs.unknownArity(query);   // ISS-2025-0688
        Term jsonTerm = args.get(0).resolveBindings(bindings);
        Term pathTerm = args.get(1).resolveBindings(bindings);
        // START_CHANGE: ISS-2025-0797 - 4.6 wave Q7: an unbound document or path raises (it failed)
        if (jsonTerm instanceof it.denzosoft.jprolog.core.terms.Variable) {
            throw LibArgs.notA("json", jsonTerm, "json_get", 3, "the JSON term");
        }
        if (pathTerm instanceof it.denzosoft.jprolog.core.terms.Variable) {
            throw LibArgs.notA("atom", pathTerm, "json_get", 3, "the path");
        }
        // END_CHANGE: ISS-2025-0797

        Term value = navigatePath(jsonTerm, pathTerm);
        if (value == null) return false;
        return unify(args.get(2), value, bindings, solutions);
    }

    // ---- SET ----
    private boolean doSet(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        List<Term> args = query.getArguments();
        if (args.size() != 4) throw LibArgs.unknownArity(query);   // ISS-2025-0688
        Term jsonTerm = args.get(0).resolveBindings(bindings);
        Term keyTerm = args.get(1).resolveBindings(bindings);
        Term valueTerm = args.get(2).resolveBindings(bindings);

        String key = LibArgs.atom(query, 1, bindings, "json_set", "the key");   // ISS-2025-0688

        if (jsonTerm instanceof CompoundTerm && "json".equals(((CompoundTerm) jsonTerm).getName())) {
            List<Term> pairs = termToList(((CompoundTerm) jsonTerm).getArguments().get(0));
            List<Term> newPairs = new ArrayList<>();
            boolean found = false;
            for (Term pair : pairs) {
                if (pair instanceof CompoundTerm && "=".equals(((CompoundTerm) pair).getName())) {
                    Term k = ((CompoundTerm) pair).getArguments().get(0);
                    if (k instanceof Atom && key.equals(((Atom) k).getName())) {
                        newPairs.add(new CompoundTerm(new Atom("="), Arrays.asList(k, valueTerm)));
                        found = true;
                    } else {
                        newPairs.add(pair);
                    }
                } else {
                    newPairs.add(pair);
                }
            }
            if (!found) {
                newPairs.add(new CompoundTerm(new Atom("="), Arrays.asList(new Atom(key), valueTerm)));
            }
            Term newJson = new CompoundTerm(new Atom("json"),
                Collections.singletonList(CollectionUtils.createListTerm(newPairs)));
            return unify(args.get(3), newJson, bindings, solutions);
        }
        throw notJson(jsonTerm, "json_set", 4);                        // ISS-2025-0688
    }

    // ---- KEYS ----
    private boolean doKeys(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        List<Term> args = query.getArguments();
        if (args.size() != 2) throw LibArgs.unknownArity(query);   // ISS-2025-0688
        Term jsonTerm = args.get(0).resolveBindings(bindings);

        if (jsonTerm instanceof CompoundTerm && "json".equals(((CompoundTerm) jsonTerm).getName())) {
            List<Term> pairs = termToList(((CompoundTerm) jsonTerm).getArguments().get(0));
            List<Term> keys = new ArrayList<>();
            for (Term pair : pairs) {
                if (pair instanceof CompoundTerm && "=".equals(((CompoundTerm) pair).getName())) {
                    keys.add(((CompoundTerm) pair).getArguments().get(0));
                }
            }
            return unify(args.get(1), CollectionUtils.createListTerm(keys), bindings, solutions);
        }
        throw notJson(jsonTerm, "json_keys", 2);                       // ISS-2025-0688
    }

    // ---- MEMBER ----
    private boolean doMember(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        List<Term> args = query.getArguments();
        if (args.size() != 3) throw LibArgs.unknownArity(query);   // ISS-2025-0688
        Term jsonTerm = args.get(0).resolveBindings(bindings);

        if (jsonTerm instanceof CompoundTerm && "json".equals(((CompoundTerm) jsonTerm).getName())) {
            List<Term> pairs = termToList(((CompoundTerm) jsonTerm).getArguments().get(0));
            boolean anyMatch = false;
            for (Term pair : pairs) {
                if (pair instanceof CompoundTerm && "=".equals(((CompoundTerm) pair).getName())) {
                    Term k = ((CompoundTerm) pair).getArguments().get(0);
                    Term v = ((CompoundTerm) pair).getArguments().get(1);
                    Map<String, Term> newBindings = new HashMap<>(bindings);
                    if (args.get(1).resolveBindings(bindings).unify(k, newBindings) &&
                        args.get(2).resolveBindings(newBindings).unify(v, newBindings)) {
                        solutions.add(newBindings);
                        anyMatch = true;
                    }
                }
            }
            return anyMatch;
        }
        throw notJson(jsonTerm, "json_member", 3);                     // ISS-2025-0688
    }

    // ---- JSON Parser ----
    private Term parseJson(String json, int[] pos) {
        skipWhitespace(json, pos);
        if (pos[0] >= json.length()) throw err("json_parse: Unexpected end of input.");
        char c = json.charAt(pos[0]);
        if (c == '{') return parseObject(json, pos);
        if (c == '[') return parseArray(json, pos);
        if (c == '"') return parseString(json, pos);
        if (c == 't' || c == 'f') return parseBool(json, pos);
        if (c == 'n') return parseNull(json, pos);
        if (c == '-' || Character.isDigit(c)) return parseNumber(json, pos);
        throw err("json_parse: Unexpected character '" + c + "' at position " + pos[0]);
    }

    private Term parseObject(String json, int[] pos) {
        pos[0]++; // skip {
        skipWhitespace(json, pos);
        List<Term> pairs = new ArrayList<>();
        if (json.charAt(pos[0]) != '}') {
            while (true) {
                skipWhitespace(json, pos);
                Term key = parseString(json, pos);
                skipWhitespace(json, pos);
                expect(json, pos, ':');
                skipWhitespace(json, pos);
                Term value = parseJson(json, pos);
                pairs.add(new CompoundTerm(new Atom("="), Arrays.asList(key, value)));
                skipWhitespace(json, pos);
                if (json.charAt(pos[0]) == ',') { pos[0]++; } else break;
            }
        }
        expect(json, pos, '}');
        return new CompoundTerm(new Atom("json"),
            Collections.singletonList(CollectionUtils.createListTerm(pairs)));
    }

    private Term parseArray(String json, int[] pos) {
        pos[0]++; // skip [
        skipWhitespace(json, pos);
        List<Term> elements = new ArrayList<>();
        if (json.charAt(pos[0]) != ']') {
            while (true) {
                skipWhitespace(json, pos);
                elements.add(parseJson(json, pos));
                skipWhitespace(json, pos);
                if (json.charAt(pos[0]) == ',') { pos[0]++; } else break;
            }
        }
        expect(json, pos, ']');
        return CollectionUtils.createListTerm(elements);
    }

    private Term parseString(String json, int[] pos) {
        expect(json, pos, '"');
        StringBuilder sb = new StringBuilder();
        while (pos[0] < json.length() && json.charAt(pos[0]) != '"') {
            char c = json.charAt(pos[0]);
            if (c == '\\') {
                pos[0]++;
                char esc = json.charAt(pos[0]);
                switch (esc) {
                    case '"': sb.append('"'); break;
                    case '\\': sb.append('\\'); break;
                    case '/': sb.append('/'); break;
                    case 'n': sb.append('\n'); break;
                    case 't': sb.append('\t'); break;
                    case 'r': sb.append('\r'); break;
                    default: sb.append(esc);
                }
            } else {
                sb.append(c);
            }
            pos[0]++;
        }
        expect(json, pos, '"');
        return new Atom(sb.toString());
    }

    private Term parseNumber(String json, int[] pos) {
        int start = pos[0];
        if (json.charAt(pos[0]) == '-') pos[0]++;
        while (pos[0] < json.length() && Character.isDigit(json.charAt(pos[0]))) pos[0]++;
        boolean isFloat = false;
        if (pos[0] < json.length() && json.charAt(pos[0]) == '.') {
            isFloat = true;
            pos[0]++;
            while (pos[0] < json.length() && Character.isDigit(json.charAt(pos[0]))) pos[0]++;
        }
        if (pos[0] < json.length() && (json.charAt(pos[0]) == 'e' || json.charAt(pos[0]) == 'E')) {
            isFloat = true;
            pos[0]++;
            if (pos[0] < json.length() && (json.charAt(pos[0]) == '+' || json.charAt(pos[0]) == '-')) pos[0]++;
            while (pos[0] < json.length() && Character.isDigit(json.charAt(pos[0]))) pos[0]++;
        }
        // START_CHANGE: ISS-2025-0424 - ENG-02: JSON 1.0 is a FLOAT, JSON 1 an INTEGER. The
        // isFloat flag was already computed by the scanner above but discarded, because
        // Number(double) auto-classified integral doubles as integers.
        String numText = json.substring(start, pos[0]);
        if (isFloat) return new Number(Double.parseDouble(numText));
        try {
            return new Number(new java.math.BigInteger(numText));
        } catch (NumberFormatException nfe) {
            return new Number(Double.parseDouble(numText));
        }
        // END_CHANGE: ISS-2025-0424
    }

    private Term parseBool(String json, int[] pos) {
        if (json.startsWith("true", pos[0])) {
            pos[0] += 4;
            return new CompoundTerm(new Atom("@"), Collections.singletonList(new Atom("true")));
        }
        if (json.startsWith("false", pos[0])) {
            pos[0] += 5;
            return new CompoundTerm(new Atom("@"), Collections.singletonList(new Atom("false")));
        }
        throw err("json_parse: Expected boolean at position " + pos[0]);
    }

    private Term parseNull(String json, int[] pos) {
        if (json.startsWith("null", pos[0])) {
            pos[0] += 4;
            return new CompoundTerm(new Atom("@"), Collections.singletonList(new Atom("null")));
        }
        throw err("json_parse: Expected null at position " + pos[0]);
    }

    // ---- JSON Serializer ----
    private String serializeTerm(Term term) {
        if (term instanceof Number) {
            double v = ((Number) term).getValue();
            if (v == Math.floor(v) && !Double.isInfinite(v)) return String.valueOf((long) v);
            return String.valueOf(v);
        }
        if (term instanceof Atom) {
            return "\"" + escapeJson(((Atom) term).getName()) + "\"";
        }
        if (term instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) term;
            String name = ct.getName();
            if ("json".equals(name) && ct.getArguments().size() == 1) {
                List<Term> pairs = termToList(ct.getArguments().get(0));
                StringBuilder sb = new StringBuilder("{");
                for (int i = 0; i < pairs.size(); i++) {
                    if (i > 0) sb.append(",");
                    CompoundTerm pair = (CompoundTerm) pairs.get(i);
                    sb.append("\"").append(escapeJson(((Atom) pair.getArguments().get(0)).getName())).append("\":");
                    sb.append(serializeTerm(pair.getArguments().get(1)));
                }
                sb.append("}");
                return sb.toString();
            }
            if ("@".equals(name) && ct.getArguments().size() == 1) {
                return ((Atom) ct.getArguments().get(0)).getName();
            }
            if (".".equals(name) && ct.getArguments().size() == 2) {
                List<Term> elems = termToList(term);
                StringBuilder sb = new StringBuilder("[");
                for (int i = 0; i < elems.size(); i++) {
                    if (i > 0) sb.append(",");
                    sb.append(serializeTerm(elems.get(i)));
                }
                sb.append("]");
                return sb.toString();
            }
        }
        if (term instanceof Atom && "[]".equals(((Atom) term).getName())) {
            return "[]";
        }
        return "\"" + escapeJson(term.toString()) + "\"";
    }

    private String escapeJson(String s) {
        return s.replace("\\", "\\\\").replace("\"", "\\\"")
                .replace("\n", "\\n").replace("\r", "\\r").replace("\t", "\\t");
    }

    // ---- Path navigation ----
    private Term navigatePath(Term json, Term path) {
        if (path instanceof Atom) {
            return getFromObject(json, ((Atom) path).getName());
        }
        if (path instanceof Number) {
            return getFromArray(json, ((Number) path).getValue().intValue());
        }
        if (path instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) path;
            if ("/".equals(ct.getName()) && ct.getArguments().size() == 2) {
                Term intermediate = navigatePath(json, ct.getArguments().get(0));
                if (intermediate == null) return null;
                return navigatePath(intermediate, ct.getArguments().get(1));
            }
        }
        return null;
    }

    private Term getFromObject(Term json, String key) {
        if (json instanceof CompoundTerm && "json".equals(((CompoundTerm) json).getName())) {
            List<Term> pairs = termToList(((CompoundTerm) json).getArguments().get(0));
            for (Term pair : pairs) {
                if (pair instanceof CompoundTerm && "=".equals(((CompoundTerm) pair).getName())) {
                    Term k = ((CompoundTerm) pair).getArguments().get(0);
                    if (k instanceof Atom && key.equals(((Atom) k).getName())) {
                        return ((CompoundTerm) pair).getArguments().get(1);
                    }
                }
            }
        }
        return null;
    }

    private Term getFromArray(Term json, int index) {
        List<Term> elems = termToList(json);
        if (index >= 0 && index < elems.size()) return elems.get(index);
        return null;
    }

    // ---- Utility ----
    private List<Term> termToList(Term term) {
        List<Term> result = new ArrayList<>();
        Term current = term;
        while (current instanceof CompoundTerm) {
            CompoundTerm ct = (CompoundTerm) current;
            if (".".equals(ct.getName()) && ct.getArguments().size() == 2) {
                result.add(ct.getArguments().get(0));
                current = ct.getArguments().get(1);
            } else break;
        }
        return result;
    }

    private void skipWhitespace(String s, int[] pos) {
        while (pos[0] < s.length() && Character.isWhitespace(s.charAt(pos[0]))) pos[0]++;
    }

    private void expect(String s, int[] pos, char c) {
        if (pos[0] >= s.length() || s.charAt(pos[0]) != c) {
            throw err("json_parse: Expected '" + c + "' at position " + pos[0]);
        }
        pos[0]++;
    }

    private boolean unify(Term target, Term value, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (target.resolveBindings(bindings).unify(value, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }

    // START_CHANGE: ISS-2025-0688 - wave Q1.1: ISO error terms (LIM-038)
    private String resolveAtom(Term term, Map<String, Term> bindings) {
        return LibArgs.text(term.resolveBindings(bindings), modeName(), LibArgs.nameArity(modeName()), "the JSON text");
    }

    /** A malformed JSON text: {@code error(syntax_error(json), context(json_parse/2, Msg))}. */
    private it.denzosoft.jprolog.core.exceptions.PrologException err(String msg) {
        String m = msg.startsWith("json_parse: ") ? msg.substring("json_parse: ".length()) : msg;
        return Errors.syntax("json", "json_parse", 2, m);
    }

    private static it.denzosoft.jprolog.core.exceptions.PrologException notJson(Term t, String name, int arity) {
        if (t instanceof it.denzosoft.jprolog.core.terms.Variable) {
            return Errors.instantiation(name, arity, "the JSON object must be bound");
        }
        return Errors.type("json", t, name, arity, "a json(Pairs) term expected");
    }
    // END_CHANGE: ISS-2025-0688

    private String modeName() {
        switch (mode) {
            case PARSE: return "json_parse";
            case SERIALIZE: return "json_serialize";
            case GET: return "json_get";
            case SET: return "json_set";
            case KEYS: return "json_keys";
            case MEMBER: return "json_member";
            default: return "json";
        }
    }
}
// END_CHANGE: ISS-2025-0113
