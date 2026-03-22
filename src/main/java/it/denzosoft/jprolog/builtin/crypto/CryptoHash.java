package it.denzosoft.jprolog.builtin.crypto;

// START_CHANGE: ISS-2025-0112 - Cryptographic built-in predicates
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Cryptographic hash predicates:
 *   md5_hash/2      - md5_hash(+Text, -Hash)
 *   sha256_hash/2   - sha256_hash(+Text, -Hash)
 *   sha512_hash/2   - sha512_hash(+Text, -Hash)
 *   crypto_hash/3   - crypto_hash(+Algorithm, +Text, -Hash)
 */
public class CryptoHash implements BuiltIn {

    public enum Mode { MD5, SHA256, SHA512, GENERIC }

    private final Mode mode;

    public CryptoHash(Mode mode) {
        this.mode = mode;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        List<Term> args = query.getArguments();

        String algorithm;
        String text;
        Term hashTerm;

        if (mode == Mode.GENERIC) {
            if (args.size() != 3) {
                throw new PrologEvaluationException("crypto_hash/3 requires 3 arguments.");
            }
            Term algoTerm = args.get(0).resolveBindings(bindings);
            if (!(algoTerm instanceof Atom)) {
                throw new PrologEvaluationException("crypto_hash/3: Algorithm must be an atom.");
            }
            algorithm = ((Atom) algoTerm).getName();
            text = resolveAtom(args.get(1), bindings);
            hashTerm = args.get(2);
        } else {
            if (args.size() != 2) {
                throw new PrologEvaluationException(modeName() + " requires 2 arguments.");
            }
            algorithm = modeAlgorithm();
            text = resolveAtom(args.get(0), bindings);
            hashTerm = args.get(1);
        }

        try {
            MessageDigest md = MessageDigest.getInstance(algorithm);
            byte[] digest = md.digest(text.getBytes(StandardCharsets.UTF_8));
            String hex = bytesToHex(digest);

            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (hashTerm.resolveBindings(bindings).unify(new Atom(hex), newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            return false;
        } catch (NoSuchAlgorithmException e) {
            throw new PrologEvaluationException(modeName() + ": Unknown algorithm: " + algorithm);
        }
    }

    private static String bytesToHex(byte[] bytes) {
        StringBuilder sb = new StringBuilder(bytes.length * 2);
        for (byte b : bytes) {
            sb.append(String.format("%02x", b & 0xFF));
        }
        return sb.toString();
    }

    private String resolveAtom(Term term, Map<String, Term> bindings) {
        Term resolved = term.resolveBindings(bindings);
        if (!(resolved instanceof Atom)) {
            throw new PrologEvaluationException(modeName() + ": Text must be an atom.");
        }
        return ((Atom) resolved).getName();
    }

    private String modeAlgorithm() {
        switch (mode) {
            case MD5: return "MD5";
            case SHA256: return "SHA-256";
            case SHA512: return "SHA-512";
            default: return "SHA-256";
        }
    }

    private String modeName() {
        switch (mode) {
            case MD5: return "md5_hash";
            case SHA256: return "sha256_hash";
            case SHA512: return "sha512_hash";
            case GENERIC: return "crypto_hash";
            default: return "crypto";
        }
    }
}
// END_CHANGE: ISS-2025-0112
