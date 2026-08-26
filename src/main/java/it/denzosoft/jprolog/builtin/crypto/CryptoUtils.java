package it.denzosoft.jprolog.builtin.crypto;

// START_CHANGE: ISS-2025-0112 - Cryptographic built-in predicates
import it.denzosoft.jprolog.core.engine.BuiltIn;
import it.denzosoft.jprolog.core.exceptions.PrologEvaluationException;
import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Number;
import it.denzosoft.jprolog.core.terms.Term;

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
            throw new PrologEvaluationException(modeName() + ": " + e.getMessage());
        }
    }

    private boolean doHmac(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) throws Exception {
        List<Term> args = query.getArguments();
        if (args.size() != 4) {
            throw new PrologEvaluationException("hmac/4 requires 4 arguments: hmac(+Algo, +Key, +Data, -MAC).");
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
            throw new PrologEvaluationException("base64_encode/2 requires 2 arguments.");
        }
        String text = resolveAtom(args.get(0), bindings);
        String encoded = Base64.getEncoder().encodeToString(text.getBytes(StandardCharsets.UTF_8));
        return unifyResult(args.get(1), new Atom(encoded), bindings, solutions);
    }

    private boolean doBase64Decode(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        List<Term> args = query.getArguments();
        if (args.size() != 2) {
            throw new PrologEvaluationException("base64_decode/2 requires 2 arguments.");
        }
        String encoded = resolveAtom(args.get(0), bindings);
        String decoded = new String(Base64.getDecoder().decode(encoded), StandardCharsets.UTF_8);
        return unifyResult(args.get(1), new Atom(decoded), bindings, solutions);
    }

    private boolean doUuid(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        List<Term> args = query.getArguments();
        if (args.size() != 1) {
            throw new PrologEvaluationException("uuid/1 requires 1 argument.");
        }
        String uuid = java.util.UUID.randomUUID().toString();
        return unifyResult(args.get(0), new Atom(uuid), bindings, solutions);
    }

    private boolean doRandomToken(Term query, Map<String, Term> bindings,
            List<Map<String, Term>> solutions) {
        List<Term> args = query.getArguments();
        if (args.size() != 2) {
            throw new PrologEvaluationException("random_token/2 requires 2 arguments.");
        }
        Term lenTerm = args.get(0).resolveBindings(bindings);
        if (!(lenTerm instanceof Number)) {
            throw new PrologEvaluationException("random_token/2: Length must be a number.");
        }
        int len = ((Number) lenTerm).getValue().intValue();
        if (len < 1 || len > 1024) {
            throw new PrologEvaluationException("random_token/2: Length must be 1-1024.");
        }
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
            throw new PrologEvaluationException("crypto_random_int/3 requires 3 arguments.");
        }
        Term lowTerm = args.get(0).resolveBindings(bindings);
        Term highTerm = args.get(1).resolveBindings(bindings);
        if (!(lowTerm instanceof Number) || !(highTerm instanceof Number)) {
            throw new PrologEvaluationException("crypto_random_int/3: Low and High must be numbers.");
        }
        int low = ((Number) lowTerm).getValue().intValue();
        int high = ((Number) highTerm).getValue().intValue();
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

    private String resolveAtom(Term term, Map<String, Term> bindings) {
        Term resolved = term.resolveBindings(bindings);
        if (!(resolved instanceof Atom)) {
            throw new PrologEvaluationException(modeName() + ": argument must be an atom.");
        }
        return ((Atom) resolved).getName();
    }

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
            throw new PrologEvaluationException(
                "crypto_aes_encrypt/4 requires 4 arguments: crypto_aes_encrypt(+PlainText, +Key, +IV, -CipherText).");
        }
        String plainText = resolveAtom(args.get(0), bindings);
        byte[] keyBytes = hexToBytes(resolveAtom(args.get(1), bindings));
        byte[] ivBytes = hexToBytes(resolveAtom(args.get(2), bindings));

        validateAesKeyLength(keyBytes.length);
        if (ivBytes.length != 16) {
            throw new PrologEvaluationException("crypto_aes_encrypt/4: IV must be 16 bytes (32 hex chars).");
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
            throw new PrologEvaluationException(
                "crypto_aes_decrypt/4 requires 4 arguments: crypto_aes_decrypt(+CipherText, +Key, +IV, -PlainText).");
        }
        byte[] cipherBytes = hexToBytes(resolveAtom(args.get(0), bindings));
        byte[] keyBytes = hexToBytes(resolveAtom(args.get(1), bindings));
        byte[] ivBytes = hexToBytes(resolveAtom(args.get(2), bindings));

        validateAesKeyLength(keyBytes.length);
        if (ivBytes.length != 16) {
            throw new PrologEvaluationException("crypto_aes_decrypt/4: IV must be 16 bytes (32 hex chars).");
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
            throw new PrologEvaluationException(
                "crypto_hash_password/2 requires 2 arguments: crypto_hash_password(+Password, -Hash).");
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
            throw new PrologEvaluationException(
                "crypto_verify_password/2 requires 2 arguments: crypto_verify_password(+Password, +Hash).");
        }
        String password = resolveAtom(args.get(0), bindings);
        String storedHash = resolveAtom(args.get(1), bindings);

        String[] parts = storedHash.split("\\$");
        if (parts.length != 4 || !"pbkdf2".equals(parts[0])) {
            throw new PrologEvaluationException(
                "crypto_verify_password/2: Hash must be in format pbkdf2$iterations$salt$hash.");
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
            throw new PrologEvaluationException(
                "AES key must be 16, 24, or 32 bytes (128, 192, or 256 bit). Got: " + length + " bytes.");
        }
    }

    private static byte[] hexToBytes(String hex) {
        if (hex.length() % 2 != 0) {
            throw new PrologEvaluationException("Hex string must have even length.");
        }
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
