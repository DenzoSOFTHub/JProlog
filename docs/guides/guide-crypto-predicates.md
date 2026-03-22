# Cryptographic Predicates Guide

## Package Overview

The `it.denzosoft.jprolog.builtin.crypto` package provides cryptographic primitives for JProlog programs. It includes hash functions (MD5, SHA-256, SHA-512), HMAC-based message authentication, Base64 encoding/decoding, UUID generation, and cryptographically secure random number generation.

All hash outputs are lowercase hexadecimal strings. Random generation uses Java's `SecureRandom` for cryptographic strength. These predicates enable JProlog programs to implement authentication schemes, data integrity checks, secure token management, and data encoding workflows.

**Source files:**
- `CryptoHash.java` -- Hash predicates (md5_hash/2, sha256_hash/2, sha512_hash/2, crypto_hash/3)
- `CryptoUtils.java` -- Utility predicates (hmac/4, base64_encode/2, base64_decode/2, uuid/1, random_token/2, crypto_random_int/3)

---

## Predicate Reference

### md5_hash/2

```prolog
md5_hash(+Text, -Hash)
```

Computes the MD5 hash of Text and unifies Hash with the lowercase hexadecimal digest string (32 characters).

| Argument | Type   | Mode | Description                         |
|----------|--------|------|-------------------------------------|
| Text     | atom   | +    | The input text to hash              |
| Hash     | atom   | -    | The resulting 128-bit MD5 hex digest|

**Errors:**
- `evaluation_error` if Text is not an atom.

**Note:** MD5 is considered cryptographically broken. Use sha256_hash/2 or sha512_hash/2 for security-sensitive applications. MD5 remains useful for checksums and non-security fingerprinting.

---

### sha256_hash/2

```prolog
sha256_hash(+Text, -Hash)
```

Computes the SHA-256 hash of Text and unifies Hash with the lowercase hexadecimal digest string (64 characters).

| Argument | Type   | Mode | Description                             |
|----------|--------|------|-----------------------------------------|
| Text     | atom   | +    | The input text to hash                  |
| Hash     | atom   | -    | The resulting 256-bit SHA-256 hex digest|

**Errors:**
- `evaluation_error` if Text is not an atom.

---

### sha512_hash/2

```prolog
sha512_hash(+Text, -Hash)
```

Computes the SHA-512 hash of Text and unifies Hash with the lowercase hexadecimal digest string (128 characters).

| Argument | Type   | Mode | Description                             |
|----------|--------|------|-----------------------------------------|
| Text     | atom   | +    | The input text to hash                  |
| Hash     | atom   | -    | The resulting 512-bit SHA-512 hex digest|

**Errors:**
- `evaluation_error` if Text is not an atom.

---

### crypto_hash/3

```prolog
crypto_hash(+Algorithm, +Text, -Hash)
```

Computes a hash using the specified algorithm. The algorithm name must match a Java `MessageDigest` algorithm identifier.

| Argument  | Type   | Mode | Description                                        |
|-----------|--------|------|----------------------------------------------------|
| Algorithm | atom   | +    | Algorithm name (e.g., 'MD5', 'SHA-256', 'SHA-512') |
| Text      | atom   | +    | The input text to hash                              |
| Hash      | atom   | -    | The resulting hex digest                            |

**Errors:**
- `evaluation_error` if Algorithm is not an atom, Text is not an atom, or the algorithm is unknown.

**Supported algorithms (common):** `'MD5'`, `'SHA-1'`, `'SHA-256'`, `'SHA-384'`, `'SHA-512'`

---

### hmac/4

```prolog
hmac(+Algorithm, +Key, +Data, -MAC)
```

Computes an HMAC (Hash-based Message Authentication Code) of Data using the given Key and Algorithm. The Algorithm name is transformed internally by prepending "Hmac" and removing hyphens (e.g., `'SHA256'` becomes `HmacSHA256`).

| Argument  | Type   | Mode | Description                                                |
|-----------|--------|------|------------------------------------------------------------|
| Algorithm | atom   | +    | HMAC algorithm (e.g., 'SHA256', 'SHA512', 'SHA1')         |
| Key       | atom   | +    | The secret key                                             |
| Data      | atom   | +    | The data to authenticate                                   |
| MAC       | atom   | -    | The resulting HMAC as a lowercase hex string               |

**Errors:**
- `evaluation_error` if any of Algorithm, Key, or Data is not an atom, or if the algorithm is unsupported.

**Note:** The algorithm atom is used to construct a Java HMAC algorithm name. For example, `'SHA256'` maps to `HmacSHA256`, and `'SHA-256'` also maps to `HmacSHA256` (hyphens are stripped).

---

### base64_encode/2

```prolog
base64_encode(+Text, -Encoded)
```

Encodes Text as a Base64 string using standard Base64 encoding (RFC 4648).

| Argument | Type   | Mode | Description                         |
|----------|--------|------|-------------------------------------|
| Text     | atom   | +    | The plaintext input                 |
| Encoded  | atom   | -    | The Base64-encoded result           |

**Errors:**
- `evaluation_error` if Text is not an atom.

---

### base64_decode/2

```prolog
base64_decode(+Encoded, -Text)
```

Decodes a Base64-encoded string back to plaintext.

| Argument | Type   | Mode | Description                         |
|----------|--------|------|-------------------------------------|
| Encoded  | atom   | +    | A Base64-encoded string             |
| Text     | atom   | -    | The decoded plaintext               |

**Errors:**
- `evaluation_error` if Encoded is not an atom or contains invalid Base64 characters.

---

### uuid/1

```prolog
uuid(-UUID)
```

Generates a random UUID (version 4) and unifies it with UUID as an atom in standard format (e.g., `'550e8400-e29b-41d4-a716-446655440000'`).

| Argument | Type   | Mode | Description                                |
|----------|--------|------|--------------------------------------------|
| UUID     | atom   | -    | A freshly generated version-4 UUID string  |

Each call produces a new unique identifier.

---

### random_token/2

```prolog
random_token(+Length, -Token)
```

Generates a cryptographically secure random token of the specified byte length. The Token is a lowercase hexadecimal string with `Length * 2` characters.

| Argument | Type   | Mode | Description                                    |
|----------|--------|------|------------------------------------------------|
| Length   | number | +    | Number of random bytes (1-1024)                |
| Token    | atom   | -    | Hex-encoded random token (2 * Length chars)    |

**Errors:**
- `evaluation_error` if Length is not a number or is outside the range 1-1024.

---

### crypto_random_int/3

```prolog
crypto_random_int(+Low, +High, -N)
```

Generates a cryptographically secure random integer N such that `Low =< N < High`.

| Argument | Type   | Mode | Description                                 |
|----------|--------|------|---------------------------------------------|
| Low      | number | +    | Lower bound (inclusive)                     |
| High     | number | +    | Upper bound (exclusive)                     |
| N        | number | -    | The generated random integer                |

**Errors:**
- `evaluation_error` if Low or High is not a number, or if Low >= High.

---

## Esempi Reali (Real Examples)

### Example 1: Password Hashing and Verification System

A complete password management system that hashes passwords with SHA-256, stores them in a user database, and verifies credentials on login.

```prolog
% ============================================================
% Password hashing and verification system
% Uses SHA-256 to securely store and verify user passwords.
% ============================================================

% --- User database: user(Username, PasswordHash) ---
:- dynamic user/2.

% Register a new user by hashing their password before storage.
% Fails if the username already exists.
register_user(Username, Password) :-
    \+ user(Username, _),
    sha256_hash(Password, Hash),
    assert(user(Username, Hash)),
    write('User registered: '), write(Username), nl.

register_user(Username, _) :-
    user(Username, _),
    write('Error: username already taken: '), write(Username), nl,
    fail.

% Verify a login attempt. Hash the candidate password and
% compare it against the stored hash.
verify_login(Username, Password) :-
    user(Username, StoredHash),
    sha256_hash(Password, CandidateHash),
    StoredHash == CandidateHash.

% High-level login with user feedback.
login(Username, Password) :-
    (   verify_login(Username, Password)
    ->  write('Login successful for '), write(Username), nl
    ;   write('Login failed: invalid username or password'), nl,
        fail
    ).

% Change a user's password. Requires the old password for safety.
change_password(Username, OldPassword, NewPassword) :-
    verify_login(Username, OldPassword),
    sha256_hash(NewPassword, NewHash),
    retract(user(Username, _)),
    assert(user(Username, NewHash)),
    write('Password changed for '), write(Username), nl.

% --- Usage ---
% ?- register_user(alice, 'S3cretP@ss!').
%    User registered: alice
%
% ?- login(alice, 'S3cretP@ss!').
%    Login successful for alice
%
% ?- login(alice, 'wrong_password').
%    Login failed: invalid username or password
%
% ?- change_password(alice, 'S3cretP@ss!', 'N3wP@ss#2025').
%    Password changed for alice
%
% ?- login(alice, 'N3wP@ss#2025').
%    Login successful for alice
```

---

### Example 2: HMAC-Based API Authentication

Compute an HMAC signature for an HTTP-style API request, mimicking the pattern used by AWS Signature V4 and similar schemes.

```prolog
% ============================================================
% HMAC-based API request signing
% Creates a canonical request string, signs it with HMAC-SHA256,
% and constructs an Authorization header.
% ============================================================

% Build a canonical request string from method, path, and body.
% The canonical form is: METHOD|PATH|BODY_HASH
canonical_request(Method, Path, Body, Canonical) :-
    sha256_hash(Body, BodyHash),
    atom_concat(Method, '|', T1),
    atom_concat(T1, Path, T2),
    atom_concat(T2, '|', T3),
    atom_concat(T3, BodyHash, Canonical).

% Sign a canonical request with the API secret key.
sign_request(Method, Path, Body, SecretKey, Signature) :-
    canonical_request(Method, Path, Body, Canonical),
    hmac('SHA256', SecretKey, Canonical, Signature).

% Build the full authorization header value.
build_auth_header(ApiKeyId, Signature, AuthHeader) :-
    atom_concat('HMAC-SHA256 Credential=', ApiKeyId, T1),
    atom_concat(T1, ', Signature=', T2),
    atom_concat(T2, Signature, AuthHeader).

% Complete workflow: sign a request and produce an auth header.
authenticate_request(Method, Path, Body, ApiKeyId, SecretKey, AuthHeader) :-
    sign_request(Method, Path, Body, SecretKey, Signature),
    build_auth_header(ApiKeyId, Signature, AuthHeader).

% Verify an incoming request by recomputing the signature and comparing.
verify_request(Method, Path, Body, SecretKey, ClaimedSignature) :-
    sign_request(Method, Path, Body, SecretKey, ExpectedSignature),
    ClaimedSignature == ExpectedSignature.

% --- Usage ---
% ?- authenticate_request('POST', '/api/v1/orders', '{"item":"widget","qty":5}',
%                          'key-12345', 'my-secret-api-key', AuthHeader).
%    AuthHeader = 'HMAC-SHA256 Credential=key-12345, Signature=<hex...>'
%
% ?- sign_request('POST', '/api/v1/orders', '{"item":"widget","qty":5}',
%                  'my-secret-api-key', Sig),
%    verify_request('POST', '/api/v1/orders', '{"item":"widget","qty":5}',
%                   'my-secret-api-key', Sig).
%    true.
%
% Tampered body will fail verification:
% ?- sign_request('POST', '/api/v1/orders', '{"item":"widget","qty":5}',
%                  'my-secret-api-key', Sig),
%    verify_request('POST', '/api/v1/orders', '{"item":"widget","qty":999}',
%                   'my-secret-api-key', Sig).
%    false.
```

---

### Example 3: Generating Secure Session Tokens with Expiration

A session management system that creates tokens, tracks their expiration, and validates them.

```prolog
% ============================================================
% Secure session token management
% Generates random tokens with expiration timestamps,
% validates them, and supports cleanup of expired sessions.
% ============================================================

:- dynamic session/3.  % session(Token, UserId, ExpiresAt)

% Session lifetime in milliseconds (30 minutes = 1800000 ms).
session_lifetime(1800000).

% Create a new session for a user. Generates a 32-byte random token
% and records the expiration time.
create_session(UserId, Token) :-
    random_token(32, Token),
    get_time(Now),
    session_lifetime(Lifetime),
    ExpiresAt is Now + Lifetime,
    assert(session(Token, UserId, ExpiresAt)),
    write('Session created for '), write(UserId),
    write(' (expires in 30 min)'), nl.

% Validate a session token: it must exist and not be expired.
validate_session(Token, UserId) :-
    session(Token, UserId, ExpiresAt),
    get_time(Now),
    Now < ExpiresAt.

% High-level session check with error reporting.
check_session(Token) :-
    (   validate_session(Token, UserId)
    ->  write('Valid session for user: '), write(UserId), nl
    ;   write('Invalid or expired session'), nl,
        fail
    ).

% Extend a valid session by resetting its expiration.
refresh_session(Token) :-
    validate_session(Token, UserId),
    retract(session(Token, UserId, _)),
    get_time(Now),
    session_lifetime(Lifetime),
    NewExpiry is Now + Lifetime,
    assert(session(Token, UserId, NewExpiry)),
    write('Session refreshed for '), write(UserId), nl.

% Destroy a session (logout).
destroy_session(Token) :-
    retract(session(Token, _, _)),
    write('Session destroyed'), nl.

% Clean up all expired sessions from the database.
cleanup_expired_sessions :-
    get_time(Now),
    findall(Token,
        (session(Token, _, ExpiresAt), ExpiresAt =< Now),
        ExpiredTokens),
    length(ExpiredTokens, Count),
    forall(member(T, ExpiredTokens), retract(session(T, _, _))),
    write('Cleaned up '), write(Count), write(' expired sessions'), nl.

% --- Usage ---
% ?- create_session(alice, Token).
%    Session created for alice (expires in 30 min)
%    Token = 'a3f8c1...(64 hex chars)'
%
% ?- check_session('a3f8c1...').
%    Valid session for user: alice
%
% ?- refresh_session('a3f8c1...').
%    Session refreshed for alice
%
% ?- destroy_session('a3f8c1...').
%    Session destroyed
```

---

### Example 4: File Integrity Verification

Compute hashes for file content, store reference hashes, and detect tampering.

```prolog
% ============================================================
% File integrity verification system
% Stores reference hashes and detects content changes.
% ============================================================

:- dynamic file_hash/3.  % file_hash(Filename, Algorithm, Hash)

% Compute and store the reference hash for a file's content.
% In a real system, Content would come from read_file/2; here we
% pass the content directly as an atom for demonstration.
register_file(Filename, Content) :-
    sha256_hash(Content, Hash256),
    md5_hash(Content, HashMD5),
    (   retract(file_hash(Filename, sha256, _)) ; true ),
    (   retract(file_hash(Filename, md5, _)) ; true ),
    assert(file_hash(Filename, sha256, Hash256)),
    assert(file_hash(Filename, md5, HashMD5)),
    write('Registered hashes for '), write(Filename), nl,
    write('  SHA-256: '), write(Hash256), nl,
    write('  MD5:     '), write(HashMD5), nl.

% Verify a file's current content against the stored reference hash.
verify_file(Filename, Content) :-
    file_hash(Filename, sha256, ExpectedHash),
    sha256_hash(Content, ActualHash),
    (   ExpectedHash == ActualHash
    ->  write(Filename), write(': INTEGRITY OK'), nl
    ;   write(Filename), write(': INTEGRITY FAILURE'), nl,
        write('  Expected: '), write(ExpectedHash), nl,
        write('  Got:      '), write(ActualHash), nl,
        fail
    ).

% Verify a batch of files. Reports status for each one.
verify_batch([]).
verify_batch([file(Name, Content) | Rest]) :-
    (   verify_file(Name, Content)
    ->  true
    ;   true   % Continue checking remaining files even on failure
    ),
    verify_batch(Rest).

% Compare two versions of content to see if they differ.
content_changed(Content1, Content2) :-
    sha256_hash(Content1, H1),
    sha256_hash(Content2, H2),
    H1 \== H2.

% Generate a manifest of all tracked files and their hashes.
generate_manifest(Manifest) :-
    findall(entry(F, Algo, H), file_hash(F, Algo, H), Manifest).

% --- Usage ---
% ?- register_file('config.xml', '<config><db host="localhost"/></config>').
%    Registered hashes for config.xml
%      SHA-256: 7a3b...
%      MD5:     e2f1...
%
% ?- verify_file('config.xml', '<config><db host="localhost"/></config>').
%    config.xml: INTEGRITY OK
%
% ?- verify_file('config.xml', '<config><db host="attacker.com"/></config>').
%    config.xml: INTEGRITY FAILURE
%
% ?- verify_batch([
%        file('config.xml', '<config><db host="localhost"/></config>'),
%        file('data.csv', 'id,name\n1,Alice\n2,Bob')
%    ]).
```

---

### Example 5: Base64 Encoding for URL-Safe Data Embedding

Encode structured data in Base64 for safe inclusion in URLs and query strings.

```prolog
% ============================================================
% Base64 encoding for URL-safe data embedding
% Encodes key-value pairs into a compact, URL-safe token,
% and decodes them back for processing.
% ============================================================

% Encode a list of key=value pairs into a single Base64 token
% suitable for embedding in a URL.
encode_params(Pairs, Token) :-
    pairs_to_string(Pairs, ParamString),
    base64_encode(ParamString, Token).

% Decode a Base64 token back to the original parameter string.
decode_params(Token, ParamString) :-
    base64_decode(Token, ParamString).

% Convert a list of Key=Value atoms to a delimited string.
% Format: key1:value1;key2:value2;...
pairs_to_string([], '').
pairs_to_string([Key=Value], Result) :-
    atom_concat(Key, ':', T1),
    atom_concat(T1, Value, Result).
pairs_to_string([Key=Value | Rest], Result) :-
    Rest \= [],
    atom_concat(Key, ':', T1),
    atom_concat(T1, Value, T2),
    atom_concat(T2, ';', T3),
    pairs_to_string(Rest, RestStr),
    atom_concat(T3, RestStr, Result).

% Build a complete URL with an encoded parameter token.
build_url(BaseUrl, Pairs, FullUrl) :-
    encode_params(Pairs, Token),
    atom_concat(BaseUrl, '?data=', T1),
    atom_concat(T1, Token, FullUrl).

% Encode user profile data for an email verification link.
verification_link(UserId, Email, Link) :-
    get_time(Timestamp),
    random_token(8, Nonce),
    number_codes(Timestamp, TsCodes),
    atom_codes(TsAtom, TsCodes),
    encode_params([user=UserId, email=Email, ts=TsAtom, nonce=Nonce], Token),
    atom_concat('https://example.com/verify?token=', Token, Link).

% Round-trip test: encode and decode, then verify match.
roundtrip_test(Original) :-
    base64_encode(Original, Encoded),
    base64_decode(Encoded, Decoded),
    (   Original == Decoded
    ->  write('Round-trip OK: '), write(Original), nl
    ;   write('Round-trip FAILED'), nl, fail
    ).

% --- Usage ---
% ?- build_url('https://api.example.com/callback',
%              [user='alice', role='admin', ref='dashboard'],
%              Url).
%    Url = 'https://api.example.com/callback?data=dXNlcjphbGljZTtyb2xlOmFkbWluO3JlZjpkYXNoYm9hcmQ='
%
% ?- encode_params([user='alice', role='admin'], Token),
%    decode_params(Token, Back).
%    Token = 'dXNlcjphbGljZTtyb2xlOmFkbWlu'
%    Back  = 'user:alice;role:admin'
%
% ?- verification_link(alice, 'alice@example.com', Link).
%    Link = 'https://example.com/verify?token=dXNlcjphbGl...'
%
% ?- roundtrip_test('Hello, World! Special chars: @#$%').
%    Round-trip OK: Hello, World! Special chars: @#$%
```
