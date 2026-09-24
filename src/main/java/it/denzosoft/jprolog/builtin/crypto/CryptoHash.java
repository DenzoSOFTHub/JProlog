package it.denzosoft.jprolog.builtin.crypto;

// START_CHANGE: ISS-2025-0112 - Cryptographic built-in predicates
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.engine.v4.Errors;
import it.denzosoft.jprolog.builtin.LibArgs;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.engine.v4.Errors;

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
            // START_CHANGE: ISS-2025-0684 - wave Q1.1: ISO error terms (LIM-038)
            if (args.size() != 3) {
                throw Errors.existence("procedure", Errors.pi("crypto_hash", args.size()), "crypto_hash", args.size(), null);
            }
            algorithm = LibArgs.atom(query, 0, bindings, "crypto_hash", "the algorithm");
            text = LibArgs.text(query, 1, bindings, "crypto_hash", "the text");
            // END_CHANGE: ISS-2025-0684
            hashTerm = args.get(2);
        } else {
            if (args.size() != 2) {                                     // ISS-2025-0684
                throw Errors.existence("procedure", Errors.pi(modeName(), args.size()), modeName(), args.size(), null);
            }
            algorithm = modeAlgorithm();
            text = LibArgs.text(query, 0, bindings, modeName(), "the text");   // ISS-2025-0684
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
            throw Errors.domain("hash_algorithm", new Atom(algorithm), modeName(), args.size(),
                                "unknown algorithm");                      // ISS-2025-0684
        }
    }

    private static String bytesToHex(byte[] bytes) {
        StringBuilder sb = new StringBuilder(bytes.length * 2);
        for (byte b : bytes) {
            sb.append(String.format("%02x", b & 0xFF));
        }
        return sb.toString();
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
