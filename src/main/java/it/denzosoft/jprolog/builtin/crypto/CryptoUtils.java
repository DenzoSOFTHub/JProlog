package it.denzosoft.jprolog.builtin.crypto;

// START_CHANGE: ISS-2025-0112 - Cryptographic built-in predicates
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.builtin.LibArgs;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.engine.v4.Errors;

import javax.crypto.Cipher;
import javax.crypto.Mac;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;
import java.security.SecureRandom;
import java.security.spec.KeySpec;
import java.util.*;

/**
 * Crypto utility predicates:
 *   hmac/4           - hmac(+Algorithm, +Key, +Data, -MAC)
 *   base64_encode/2  - base64_encode(+Text, -Encoded)
 *   base64_decode/2  - base64_decode(+Encoded, -Text)
 *   uuid/1           - uuid(-UUID)
 *   random_token/2   - random_token(+Length, -Token)
 *   crypto_random_int/3 - crypto_random_int(+Low, +High, -N)
 */
public class CryptoUtils implements BuiltIn {

    public enum Mode {
        HMAC, BASE64_ENCODE, BASE64_DECODE, UUID, RANDOM_TOKEN, RANDOM_INT,
        // START_CHANGE: ISS-2025-0176 - AES encryption and password hashing
        AES_ENCRYPT, AES_DECRYPT, HASH_PASSWORD, VERIFY_PASSWORD
        // END_CHANGE: ISS-2025-0176
    }

    private final Mode mode;
    private static final SecureRandom SECURE_RANDOM = new SecureRandom();

    public CryptoUtils(Mode mode) {
        this.mode = mode;
    }

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, List<Map<String, Term>> solutions) {
        try {
            switch (mode) {
                case HMAC:           return doHmac(query, bindings, solutions);
                case BASE64_ENCODE:  return doBase64Encode(query, bindings, solutions);
                case BASE64_DECODE:  return doBase64Decode(query, bindings, solutions);
                case UUID:           return doUuid(query, bindings, solutions);
                case RANDOM_TOKEN:   return doRandomToken(query, bindings, solutions);
                case RANDOM_INT:     return doRandomInt(query, bindings, solutions);
                // START_CHANGE: ISS-2025-0176 - AES encryption and password hashing dispatch
                case AES_ENCRYPT:    return doAesEncrypt(query, bindings, solutions);
                case AES_DECRYPT:    return doAesDecrypt(query, bindings, solutions);
                case HASH_PASSWORD:  return doHashPassword(query, bindings, solutions);
                case VERIFY_PASSWORD: return doVerifyPassword(query, bindings, solutions);
                // END_CHANGE: ISS-2025-0176
                default: return false;
            }
        } catch (Exception e) {
            it.denzosoft.jprolog.core.engine.ControlFlow.rethrowIfControl(e);   // ISS-2025-0431
            throw Errors.host(e, "read", "crypto", null, modeName(), arityOf(query));   // ISS-2025-0684
        }
    }

    private boolean doHmac(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws Exception {
        List<Term> args = query.getArguments();
        if (args.size() != 4) {
            throw LibArgs.unknownArity(query);   // ISS-2025-0684
        }
        String algo = resolveAtom(args.get(0), bindings);
        String key = resolveAtom(args.get(1), bindings);
        String data = resolveAtom(args.get(2), bindings);

        String javaAlgo = "Hmac" + algo.replace("-", "");
        Mac mac = Mac.getInstance(javaAlgo);
        mac.init(new SecretKeySpec(key.getBytes(StandardCharsets.UTF_8), javaAlgo));
        byte[] result = mac.doFinal(data.getBytes(StandardCharsets.UTF_8));

        StringBuilder hex = new StringBuilder();
        for (byte b : result) hex.append(String.format("%02x", b & 0xFF));

        return unifyResult(args.get(3), new Atom(hex.toString()), bindings, solutions);
    }

    private boolean doBase64Encode(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        List<Term> args = query.getArguments();
        if (args.size() != 2) {
            throw LibArgs.unknownArity(query);   // ISS-2025-0684
        }
        String text = resolveAtom(args.get(0), bindings);
        String encoded = Base64.getEncoder().encodeToString(text.getBytes(StandardCharsets.UTF_8));
        return unifyResult(args.get(1), new Atom(encoded), bindings, solutions);
    }

    private boolean doBase64Decode(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        List<Term> args = query.getArguments();
        if (args.size() != 2) {
            throw LibArgs.unknownArity(query);   // ISS-2025-0684
        }
        String encoded = resolveAtom(args.get(0), bindings);
        String decoded = new String(Base64.getDecoder().decode(encoded), StandardCharsets.UTF_8);
        return unifyResult(args.get(1), new Atom(decoded), bindings, solutions);
    }

    private boolean doUuid(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        List<Term> args = query.getArguments();
        if (args.size() != 1) {
            throw LibArgs.unknownArity(query);   // ISS-2025-0684
        }
        String uuid = java.util.UUID.randomUUID().toString();
        return unifyResult(args.get(0), new Atom(uuid), bindings, solutions);
    }

    private boolean doRandomToken(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        List<Term> args = query.getArguments();
        if (args.size() != 2) {
            throw LibArgs.unknownArity(query);   // ISS-2025-0684
        }
        // START_CHANGE: ISS-2025-0684
        int len = (int) LibArgs.integer(query, 0, bindings, "random_token", "the length");
        if (len < 1 || len > 1024) {
            throw Errors.domain("token_length", args.get(0).resolveBindings(bindings), "random_token", 2,
                                "the length must be 1-1024");
        }
        // END_CHANGE: ISS-2025-0684
        byte[] bytes = new byte[len];
        SECURE_RANDOM.nextBytes(bytes);
        StringBuilder hex = new StringBuilder(len * 2);
        for (byte b : bytes) hex.append(String.format("%02x", b & 0xFF));
        return unifyResult(args.get(1), new Atom(hex.toString()), bindings, solutions);
    }

    private boolean doRandomInt(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        List<Term> args = query.getArguments();
        if (args.size() != 3) {
            throw LibArgs.unknownArity(query);   // ISS-2025-0684
        }
        // START_CHANGE: ISS-2025-0684
        int low = (int) LibArgs.integer(query, 0, bindings, "crypto_random_int", "Low");
        int high = (int) LibArgs.integer(query, 1, bindings, "crypto_random_int", "High");
        if (high <= low) {
            throw Errors.domain("empty_range", args.get(1).resolveBindings(bindings), "crypto_random_int", 3,
                                "High must be greater than Low");
        }
        // END_CHANGE: ISS-2025-0684
        int n = low + SECURE_RANDOM.nextInt(high - low);
        return unifyResult(args.get(2), new Number(n), bindings, solutions);
    }

    private boolean unifyResult(Term target, Term value, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        Map<String, Term> newBindings = new HashMap<>(bindings);
        if (target.resolveBindings(bindings).unify(value, newBindings)) {
            solutions.add(newBindings);
            return true;
        }
        return false;
    }

    // START_CHANGE: ISS-2025-0684 - wave Q1.1: ISO error terms (LIM-038)
    private String resolveAtom(Term term, Map<String, Term> bindings) {
        return LibArgs.text(term.resolveBindings(bindings), modeName(), modeArity(), "the argument");
    }

    private int modeArity() {
        switch (mode) {
            case HMAC: case AES_ENCRYPT: case AES_DECRYPT: return 4;
            case RANDOM_INT: return 3;
            case UUID: return 1;
            default: return 2;
        }
    }

    private static int arityOf(Term query) { return LibArgs.arity(query); }
    // END_CHANGE: ISS-2025-0684

    private String modeName() {
        switch (mode) {
            case HMAC: return "hmac";
            case BASE64_ENCODE: return "base64_encode";
            case BASE64_DECODE: return "base64_decode";
            case UUID: return "uuid";
            case RANDOM_TOKEN: return "random_token";
            case RANDOM_INT: return "crypto_random_int";
            // START_CHANGE: ISS-2025-0176 - AES and password hashing mode names
            case AES_ENCRYPT: return "crypto_aes_encrypt";
            case AES_DECRYPT: return "crypto_aes_decrypt";
            case HASH_PASSWORD: return "crypto_hash_password";
            case VERIFY_PASSWORD: return "crypto_verify_password";
            // END_CHANGE: ISS-2025-0176
            default: return "crypto";
        }
    }
    // START_CHANGE: ISS-2025-0176 - AES encryption/decryption and password hashing

    private static final String AES_ALGORITHM = "AES/CBC/PKCS5Padding";
    private static final int PBKDF2_ITERATIONS = 65536;
    private static final int PBKDF2_KEY_LENGTH = 256;

    /**
     * crypto_aes_encrypt(+PlainText, +Key, +IV, -CipherText)
     * Key must be 16, 24, or 32 hex-encoded bytes (32, 48, or 64 hex chars).
     * IV must be 16 hex-encoded bytes (32 hex chars).
     * Returns CipherText as hex string.
     */
    private boolean doAesEncrypt(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws Exception {
        List<Term> args = query.getArguments();
        if (args.size() != 4) {
            throw LibArgs.unknownArity(query);   // ISS-2025-0684
        }
        String plainText = resolveAtom(args.get(0), bindings);
        byte[] keyBytes = hexToBytes(resolveAtom(args.get(1), bindings));
        byte[] ivBytes = hexToBytes(resolveAtom(args.get(2), bindings));

        validateAesKeyLength(keyBytes.length);
        if (ivBytes.length != 16) {
            throw Errors.domain("aes_iv", args.get(2).resolveBindings(bindings), "crypto_aes_encrypt", 4,
                                "the IV must be 16 bytes (32 hex chars)");   // ISS-2025-0684
        }

        Cipher cipher = Cipher.getInstance(AES_ALGORITHM);
        cipher.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(keyBytes, "AES"), new IvParameterSpec(ivBytes));
        byte[] encrypted = cipher.doFinal(plainText.getBytes(StandardCharsets.UTF_8));

        return unifyResult(args.get(3), new Atom(bytesToHex(encrypted)), bindings, solutions);
    }

    /**
     * crypto_aes_decrypt(+CipherText, +Key, +IV, -PlainText)
     */
    private boolean doAesDecrypt(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws Exception {
        List<Term> args = query.getArguments();
        if (args.size() != 4) {
            throw LibArgs.unknownArity(query);   // ISS-2025-0684
        }
        byte[] cipherBytes = hexToBytes(resolveAtom(args.get(0), bindings));
        byte[] keyBytes = hexToBytes(resolveAtom(args.get(1), bindings));
        byte[] ivBytes = hexToBytes(resolveAtom(args.get(2), bindings));

        validateAesKeyLength(keyBytes.length);
        if (ivBytes.length != 16) {
            throw Errors.domain("aes_iv", args.get(2).resolveBindings(bindings), "crypto_aes_decrypt", 4,
                                "the IV must be 16 bytes (32 hex chars)");   // ISS-2025-0684
        }

        Cipher cipher = Cipher.getInstance(AES_ALGORITHM);
        cipher.init(Cipher.DECRYPT_MODE, new SecretKeySpec(keyBytes, "AES"), new IvParameterSpec(ivBytes));
        byte[] decrypted = cipher.doFinal(cipherBytes);

        return unifyResult(args.get(3), new Atom(new String(decrypted, StandardCharsets.UTF_8)), bindings, solutions);
    }

    /**
     * crypto_hash_password(+Password, -Hash)
     * Uses PBKDF2WithHmacSHA256 with random salt.
     * Returns hash in format: pbkdf2$iterations$salt_hex$hash_hex
     */
    private boolean doHashPassword(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws Exception {
        List<Term> args = query.getArguments();
        if (args.size() != 2) {
            throw LibArgs.unknownArity(query);   // ISS-2025-0684
        }
        String password = resolveAtom(args.get(0), bindings);

        byte[] salt = new byte[16];
        SECURE_RANDOM.nextBytes(salt);

        SecretKeyFactory factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256");
        KeySpec spec = new PBEKeySpec(password.toCharArray(), salt, PBKDF2_ITERATIONS, PBKDF2_KEY_LENGTH);
        byte[] hash = factory.generateSecret(spec).getEncoded();

        String result = "pbkdf2$" + PBKDF2_ITERATIONS + "$" + bytesToHex(salt) + "$" + bytesToHex(hash);
        return unifyResult(args.get(1), new Atom(result), bindings, solutions);
    }

    /**
     * crypto_verify_password(+Password, +Hash)
     * Verifies a password against a hash in pbkdf2$iterations$salt$hash format.
     * Succeeds if password matches, fails otherwise.
     */
    private boolean doVerifyPassword(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws Exception {
        List<Term> args = query.getArguments();
        if (args.size() != 2) {
            throw LibArgs.unknownArity(query);   // ISS-2025-0684
        }
        String password = resolveAtom(args.get(0), bindings);
        String storedHash = resolveAtom(args.get(1), bindings);

        String[] parts = storedHash.split("\\$");
        if (parts.length != 4 || !"pbkdf2".equals(parts[0])) {
            throw Errors.domain("password_hash", args.get(1).resolveBindings(bindings), "crypto_verify_password", 2,
                                "the hash must be in format pbkdf2$iterations$salt$hash");   // ISS-2025-0684
        }

        int iterations = Integer.parseInt(parts[1]);
        byte[] salt = hexToBytes(parts[2]);
        byte[] expectedHash = hexToBytes(parts[3]);

        SecretKeyFactory factory = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256");
        KeySpec spec = new PBEKeySpec(password.toCharArray(), salt, iterations, expectedHash.length * 8);
        byte[] computedHash = factory.generateSecret(spec).getEncoded();

        // Constant-time comparison to prevent timing attacks
        if (constantTimeEquals(expectedHash, computedHash)) {
            solutions.add(new HashMap<>(bindings));
            return true;
        }
        return false;
    }

    private void validateAesKeyLength(int length) {
        if (length != 16 && length != 24 && length != 32) {
            throw Errors.domain("aes_key_length", it.denzosoft.jprolog.core.terms.Number.valueOf(length), modeName(), 4,
                                "the AES key must be 16, 24, or 32 bytes");   // ISS-2025-0684
        }
    }

    private byte[] hexToBytes(String hex) {
        // START_CHANGE: ISS-2025-0684 - a malformed hex argument is a domain error of the predicate
        if (hex.length() % 2 != 0 || !hex.matches("[0-9a-fA-F]*")) {
            throw Errors.domain("hex_encoding", new Atom(hex), modeName(), modeArity(),
                                "expected an even-length hexadecimal string");
        }
        // END_CHANGE: ISS-2025-0684
        byte[] bytes = new byte[hex.length() / 2];
        for (int i = 0; i < bytes.length; i++) {
            bytes[i] = (byte) Integer.parseInt(hex.substring(i * 2, i * 2 + 2), 16);
        }
        return bytes;
    }

    private static String bytesToHex(byte[] bytes) {
        StringBuilder hex = new StringBuilder(bytes.length * 2);
        for (byte b : bytes) {
            hex.append(String.format("%02x", b & 0xFF));
        }
        return hex.toString();
    }

    private static boolean constantTimeEquals(byte[] a, byte[] b) {
        if (a.length != b.length) return false;
        int result = 0;
        for (int i = 0; i < a.length; i++) {
            result |= a[i] ^ b[i];
        }
        return result == 0;
    }
    // END_CHANGE: ISS-2025-0176
}
// END_CHANGE: ISS-2025-0112
