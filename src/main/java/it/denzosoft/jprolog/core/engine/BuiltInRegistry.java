package it.denzosoft.jprolog.core.engine;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

public class BuiltInRegistry {
    private static final Logger LOGGER = Logger.getLogger(BuiltInRegistry.class.getName());
    private final Map<String, BuiltIn> builtIns = new HashMap<>();

    // START_CHANGE: ISS-2025-0091 - Replace 140-line switch with HashMap for O(1) arity lookup
    /** Map from functor name to set of valid arities. */
    private static final Map<String, Set<Integer>> BUILTIN_ARITIES = new HashMap<>();
    /** Predicates that accept a range of arities (e.g., call/1..8). */
    private static final Map<String, int[]> BUILTIN_ARITY_RANGES = new HashMap<>();

    static {
        // Type checking (arity 1)
        for (String p : new String[]{"simple", "number", "atom", "var", "nonvar", "compound",
                "integer", "float", "atomic", "callable", "ground", "is_list", "partial_list"}) {
            putArity(p, 1);
        }
        // Binary operators and predicates (arity 2)
        for (String p : new String[]{"is", "=", "\\=", "==", "\\==", ">", "<", ">=", "=<",
                "=:=", "=\\=", "=..", "@<", "@=<", "@>", "@>=",
                "atom_codes", "number_codes", "string_codes", "to_codes",
                "atom_length", "atom_chars", "member", "length", "copy_term",
                "reverse", "msort", "sort", "number_chars", "atom_number",
                "atom_string", "number_string", "keysort", "succ",
                "term_variables", "subsumes_term", "clause", "forall",
                "->", ";"}) {
            putArity(p, 2);
        }
        // Ternary predicates (arity 3)
        for (String p : new String[]{"functor", "arg", "atom_concat", "append", "findall",
                "bagof", "setof", "between", "select", "nth0", "nth1", "op",
                "current_op", "catch", "plus", "compare"}) {
            putArity(p, 3);
        }
        // Unary predicates
        for (String p : new String[]{"assert", "asserta", "assertz", "retract", "retractall",
                "current_predicate", "once", "ignore", "\\+", "writeln",
                "put_char", "put_code", "write",
                "throw"}) {
            putArity(p, 1);
        }
        // START_CHANGE: R3 - get_char/1,2  get_code/1,2 with stream argument
        putArity("get_char", 1, 2);
        putArity("get_code", 1, 2);
        // END_CHANGE: R3
        // START_CHANGE: ISS-2025-0373 - stream-argument arities: write/2, writeln/2, nl/1, put_char/2
        putArity("write", 1, 2);
        putArity("writeln", 1, 2);
        putArity("nl", 0, 1);
        putArity("put_char", 1, 2);
        // END_CHANGE: ISS-2025-0373
        // START_CHANGE: ISS-2025-0376 - peek_char/1,2 peek_code/1,2 with stream argument
        putArity("peek_char", 1, 2);
        putArity("peek_code", 1, 2);
        // END_CHANGE: ISS-2025-0376
        // START_CHANGE: ISS-2025-0378 - print/1,2
        putArity("print", 1, 2);
        // END_CHANGE: ISS-2025-0378
        // START_CHANGE: ISS-2025-0253 - close/1,2
        putArity("close", 1, 2);
        // END_CHANGE: ISS-2025-0253
        // START_CHANGE: ISS-2025-0203 - read/1 and read/2
        putArity("read", 1, 2);
        // ISS-2025-0670: the placeholders registered for these names must not accept arities the
        // natives do not implement (a placeholder is never meant to execute)
        putArity("read_term", 2, 3);
        putArity("term_to_atom", 2);
        putArity("dcg_translate_rule", 2);
        // END_CHANGE: ISS-2025-0203
        // START_CHANGE: ISS-2025-0574/0576 - the loader built-ins
        putArity("consult", 1);
        putArity("ensure_loaded", 1);
        putArity("load_files", 1, 2);
        putArity(".", 2);
        putArity("make", 0);
        putArity("use_module", 1, 2);                                          // ISS-2025-0735
        putArity("source_file", 1, 2);
        putArity("prolog_load_context", 2);
        // END_CHANGE: ISS-2025-0574/0576
        // START_CHANGE: ISS-2025-0630 - wave P6.5: the thread / queue / mutex family at its real
        // arities, so thread_join/5 is an unknown procedure instead of reaching the built-in.
        putArity("thread_create", 2, 3);
        putArity("thread_join", 1, 2);
        putArity("thread_detach", 1);
        putArity("thread_self", 1);
        putArity("thread_sleep", 1);
        putArity("thread_is_alive", 1);
        putArity("thread_property", 2);
        putArity("thread_exit", 1);
        putArity("message_queue_create", 1, 2);
        putArity("message_queue_destroy", 1);
        putArity("thread_send_message", 2, 3);                        // ISS-2025-0750: /3
        putArity("thread_get_message", 1, 2, 3);
        putArity("thread_peek_message", 1, 2);
        putArity("mutex_create", 1, 2);
        putArity("mutex_destroy", 1);
        putArity("mutex_lock", 1);
        putArity("mutex_trylock", 1);
        putArity("mutex_unlock", 1);
        putArity("mutex_unlock_all", 0);
        putArity("with_mutex", 2);
        // START_CHANGE: ISS-2025-0749/0750 - 4.6 wave Q4.1
        putArity("thread_signal", 2);
        putArity("thread_statistics", 3);
        putArity("message_queue_property", 2);
        putArity("mutex_property", 2);
        putArity("thread_pool_create", 3);
        putArity("thread_pool_destroy", 1);
        putArity("thread_create_in_pool", 4);
        putArity("thread_pool_property", 2);
        putArity("current_thread_pool", 1);
        // END_CHANGE: ISS-2025-0749/0750
        putArity("concurrent", 3);
        putArity("concurrent_maplist", 2, 3, 4);
        putArity("concurrent_forall", 2, 3);
        putArity("first_solution", 3);
        putArity("concurrent_and", 2);
        putArity("concurrent_or", 2);
        // END_CHANGE: ISS-2025-0630
        // Zero-arity predicates
        putArity("!", 0);
        putArity("repeat", 0);
        putArity("nl", 0);
        // Multi-arity predicates
        putArity("abolish", 1, 2);
        putArity("listing", 0, 1);
        putArity("halt", 0, 1);
        putArity("phrase", 2, 3);
        putArity("open", 3, 4);
        putArity("sub_atom", 5);
        // call/1..8
        BUILTIN_ARITY_RANGES.put("call", new int[]{1, 8});
        // START_CHANGE: ISS-2025-0222 - maplist/2..5, sort/4
        BUILTIN_ARITY_RANGES.put("maplist", new int[]{2, 5});
        putArity("sort", 2, 4);
        // partition/4 not registered: would shadow user-defined partition (e.g. quicksort)
        // END_CHANGE: ISS-2025-0222
        // START_CHANGE: ISS-2025-0237 - atomic_list_concat/2,3
        putArity("atomic_list_concat", 2, 3);
        // END_CHANGE: ISS-2025-0237
        // START_CHANGE: ISS-2025-0238 - atom_to_term/3
        putArity("atom_to_term", 3);
        // END_CHANGE: ISS-2025-0238
        // START_CHANGE: R1 - setarg/3
        putArity("setarg", 3);
        // END_CHANGE: R1
        // START_CHANGE: CR-2025-0005 - seek/4
        putArity("seek", 4);
        // END_CHANGE: CR-2025-0005
        // START_CHANGE: CR-2025-0009 - debug + profile predicates
        putArity("debugging", 0);
        putArity("spying", 1);
        putArity("profile", 0);
        putArity("noprofile", 0);
        putArity("profile_data", 1);
        putArity("reset_profile", 0);
        // END_CHANGE: CR-2025-0009
        // START_CHANGE: v2.9.7 - pairs_* + must_be SWI utilities
        putArity("pairs_keys", 2);
        putArity("pairs_values", 2);
        putArity("pairs_keys_values", 3);
        putArity("must_be", 2);
        // END_CHANGE: v2.9.7
        // START_CHANGE: ISS-2025-0092 - Tabling predicates
        putArity("table", 1);
        putArity("abolish_all_tables", 0);
        // END_CHANGE: ISS-2025-0092
        // START_CHANGE: LIM-005 - predicate_property/2
        putArity("predicate_property", 2);
        // END_CHANGE: LIM-005
        // START_CHANGE: LIM-006 - code_type/2
        putArity("code_type", 2);
        // END_CHANGE: LIM-006

        // START_CHANGE: LIM-002 - Attributed variable predicates
        // START_CHANGE: ISS-2025-0491 - 4.1 wave A: put_attr/3, get_attr/3, del_attr/2, attvar/1,
        // freeze/2, when/2 and dif/2 have no arity entry any more. isBuiltIn() needs BOTH a
        // registration and an arity entry, and their legacy registrations went with the v2-era
        // classes: put_attr & co. are v4 natives, freeze/when/dif are prelude clauses (which a
        // user module is allowed to override — that is the documented library rule).
        // END_CHANGE: ISS-2025-0491
        // END_CHANGE: LIM-001

        // START_CHANGE: ISS-2025-0369 - dynamic/1 is a goal-callable built-in at arity 1 ONLY
        // (without this entry, registering it would claim every arity of 'dynamic')
        putArity("dynamic", 1);
        // END_CHANGE: ISS-2025-0369

        // START_CHANGE: ISS-2025-0398 - V^Goal callable as an ordinary goal (call(Goal)) at arity 2
        putArity("^", 2);
        // END_CHANGE: ISS-2025-0398

        // START_CHANGE: ISS-2025-0685 - wave Q1.2: EVERY registered name declares its exact arity
        // set. Until 4.5.0 a name with no entry here was a built-in at EVERY arity, so
        // char_code(X) reached builtin.character.CharCode and answered the message atom
        // 'char_code/2 requires exactly 2 arguments' instead of
        // existence_error(procedure, char_code/1), and consult refused a user char_code/1
        // (1 089 of 3 780 probed goals, ExtendedLibraryErrorsTest). The sets below were taken
        // from the v4 native table (a name that is native at some arity), the prelude module
        // exports (a library name), or the built-in's own arity guard; ExtendedLibraryErrorsTest
        // fails if a name registered by new Prolog() has no entry.
        // character
        putArity("char_code", 2); putArity("char_type", 2); putArity("downcase_atom", 2);
        putArity("upcase_atom", 2);
        // clpfd
        putArity("#<", 2); putArity("#=", 2); putArity("#=<", 2); putArity("#>", 2);
        putArity("#>=", 2); putArity("#\\=", 2); putArity("all_different", 1);
        putArity("all_distinct", 1); putArity("fd_dom", 2); putArity("fd_size", 2);
        putArity("in", 2); putArity("indomain", 1); putArity("label", 1); putArity("labeling", 2);
        // control
        putArity("cut", 0); putArity("unify_with_occurs_check", 2);
        // conversion
        putArity("atom_to_number", 2); putArity("number_to_atom", 2); putArity("string_code", 3);
        putArity("string_to_atom", 2);
        // crypto
        putArity("base64_decode", 2); putArity("base64_encode", 2);
        putArity("crypto_aes_decrypt", 4); putArity("crypto_aes_encrypt", 4);
        putArity("crypto_hash", 3); putArity("crypto_hash_password", 2);
        putArity("crypto_random_int", 3); putArity("crypto_verify_password", 2);
        putArity("hmac", 4); putArity("md5_hash", 2); putArity("random_token", 2);
        putArity("sha256_hash", 2); putArity("sha512_hash", 2); putArity("uuid", 1);
        // csv
        putArity("csv_parse", 2); putArity("csv_read_file", 2); putArity("csv_serialize", 2);
        putArity("csv_write_file", 2);
        // datetime
        putArity("date_add", 4); putArity("date_diff", 4); putArity("date_parts", 4);
        putArity("date_time_stamp", 2); putArity("day_of_week", 2); putArity("format_time", 3);
        putArity("get_time", 1); putArity("now", 1); putArity("parse_time", 3);
        putArity("stamp_date_time", 3); putArity("time_parts", 4); putArity("today", 1);
        // dcg
        putArity("call_dcg", 3); putArity("dcg_body", 4); putArity("enhanced_phrase", 2, 3);
        putArity("phrase_with_options", 4);
        // debug
        putArity("leash", 1); putArity("nospy", 1); putArity("notrace", 0); putArity("spy", 1);
        putArity("trace", 0);
        // engine
        putArity("atom_gc", 0); putArity("atom_table_size", 1);   // ISS-2025-0712: rational/1 is native
        // ffi
        putArity("java_array_get", 3); putArity("java_array_length", 2);
        putArity("java_array_new", 3); putArity("java_array_set", 3); putArity("java_call", 4);
        putArity("java_class", 2); putArity("java_from_term", 2); putArity("java_gc", 0);
        putArity("java_get_field", 3); putArity("java_instanceof", 2); putArity("java_new", 3);
        putArity("java_release_ref", 1); putArity("java_set_field", 3); putArity("java_to_term", 2);
        // filesystem
        putArity("absolute_file_name", 2, 3); putArity("copy_file", 2);   // ISS-2025-0737: + /3
        putArity("delete_directory", 1); putArity("delete_file", 1);
        putArity("directory_exists", 1); putArity("directory_files", 2); putArity("file_exists", 1);
        putArity("file_modified", 2); putArity("file_size", 2); putArity("make_directory", 1);
        putArity("make_directory_path", 1); putArity("read_file_to_atom", 2);
        putArity("rename_file", 2); putArity("working_directory", 2);
        putArity("exists_file", 1); putArity("exists_directory", 1);   // ISS-2025-0745 (SWI names)
        putArity("write_atom_to_file", 2);
        // graph
        putArity("graph_components", 2); putArity("graph_connected", 2);
        putArity("graph_degree", 3); putArity("graph_edges", 2); putArity("graph_has_cycle", 1);
        putArity("graph_neighbors", 3); putArity("graph_path", 4); putArity("graph_reachable", 3);
        putArity("graph_scc", 2); putArity("graph_vertices", 2);
        putArity("minimum_spanning_tree", 2); putArity("shortest_path", 4);
        putArity("topological_sort", 2);
        // http
        putArity("http_client_get", 2); putArity("http_client_post", 3);
        putArity("http_get_request", 2); putArity("http_handler", 3); putArity("http_open", 3);
        putArity("http_reply", 4); putArity("http_reply_json", 3); putArity("http_server", 2);
        putArity("http_stop", 1); putArity("url_decode", 2); putArity("url_encode", 2);
        // io
        putArity("at_end_of_stream", 0, 1); putArity("character_count", 2);
        putArity("current_input", 1); putArity("current_output", 1); putArity("current_stream", 3);
        putArity("flush_output", 0, 1); putArity("format", 1, 2, 3); putArity("get_byte", 1, 2);
        putArity("line_count", 2); putArity("line_position", 2); putArity("peek_byte", 1, 2);
        putArity("portray_clause", 1, 2); putArity("print_message", 2); putArity("put_byte", 1, 2);
        putArity("set_input", 1); putArity("set_output", 1); putArity("set_stream", 2);
        putArity("set_stream_position", 2); putArity("stream_position", 2);
        putArity("stream_position_data", 3); putArity("stream_property", 2); putArity("tab", 1, 2);
        putArity("with_output_to", 2); putArity("write_canonical", 1, 2);
        putArity("write_term", 2, 3); putArity("writeq", 1, 2);
        // jdbc
        putArity("jdbc_call_execute", 1); putArity("jdbc_call_get_result", 3);
        putArity("jdbc_call_get_resultset", 2); putArity("jdbc_call_register_out", 3);
        putArity("jdbc_call_set_param", 3); putArity("jdbc_close_statement", 1);
        putArity("jdbc_columns", 3); putArity("jdbc_commit", 1); putArity("jdbc_connect", 2, 4);
        putArity("jdbc_disconnect", 1); putArity("jdbc_driver_load", 1);
        putArity("jdbc_execute_prepared_query", 2); putArity("jdbc_execute_prepared_update", 2);
        putArity("jdbc_execute_update", 3); putArity("jdbc_get_blob_bytes", 3);
        putArity("jdbc_get_blob_to_file", 3); putArity("jdbc_get_clob", 3);
        putArity("jdbc_prepare", 3); putArity("jdbc_prepare_call", 3); putArity("jdbc_query", 3);
        putArity("jdbc_rollback", 1); putArity("jdbc_set_autocommit", 2);
        putArity("jdbc_set_blob", 3); putArity("jdbc_set_blob_bytes", 3);
        putArity("jdbc_set_clob", 3); putArity("jdbc_set_param", 3); putArity("jdbc_set_params", 2);
        putArity("jdbc_tables", 2);
        // json
        putArity("json_get", 3); putArity("json_keys", 2); putArity("json_member", 3);
        putArity("json_parse", 2); putArity("json_serialize", 2); putArity("json_set", 4);
        // list
        putArity("delete", 3); putArity("exclude", 3); putArity("flatten", 2);
        putArity("foldl", 4, 5, 6, 7); putArity("include", 3); putArity("intersection", 3);
        putArity("last", 2); putArity("max_list", 2); putArity("min_list", 2);
        putArity("numlist", 3); putArity("permutation", 2); putArity("predsort", 3);
        putArity("subtract", 3); putArity("sum_list", 2); putArity("sumlist", 2);
        putArity("union", 3);
        // logging
        putArity("log_debug", 1); putArity("log_error", 1); putArity("log_info", 1);
        putArity("log_level", 1); putArity("log_to_file", 1); putArity("log_warning", 1);
        // meta
        putArity("abolish_table", 1); putArity("aggregate_all", 3); putArity("call_cleanup", 2);
        putArity("setup_call_cleanup", 3);
        // network
        putArity("hostname_address", 2); putArity("http_post", 4); putArity("http_request", 4);
        putArity("tcp_accept", 2); putArity("tcp_close", 1); putArity("tcp_connect", 3);
        putArity("tcp_receive", 3); putArity("tcp_send", 2); putArity("tcp_server_socket", 2);
        putArity("udp_close", 1); putArity("udp_receive", 4); putArity("udp_send", 4);
        putArity("udp_socket", 2);
        // os
        putArity("cpu_count", 1); putArity("free_memory", 1); putArity("getenv", 2);
        putArity("hostname", 1); putArity("os_name", 1); putArity("pid", 1); putArity("shell", 1, 2);   // ISS-2025-0718: shell/2
        putArity("setenv", 2); putArity("unsetenv", 1);   // ISS-2025-0718
        putArity("shell2", 2); putArity("shell_output", 3); putArity("sleep", 1);
        putArity("system_time", 1); putArity("total_memory", 1);
        // persistence
        putArity("db_batch_assert", 1); putArity("db_clear", 0); putArity("db_export_json", 1);
        putArity("db_import_json", 1); putArity("db_load", 1); putArity("db_restore", 1);
        putArity("db_save", 1); putArity("db_save_predicate", 2); putArity("db_snapshot", 1);
        putArity("db_sync", 0); putArity("db_transaction", 1); putArity("persist", 1);
        putArity("unpersist", 1);
        // regex
        putArity("re_escape", 2); putArity("re_findall", 3); putArity("re_match", 2);
        putArity("re_matchsub", 3); putArity("re_replace", 4); putArity("re_split", 3);
        // string
        putArity("split_string", 4); putArity("string_chars", 2); putArity("string_concat", 3);
        putArity("string_length", 2); putArity("sub_string", 5);
        // system
        putArity("b_getval", 2); putArity("b_setval", 2); putArity("char_conversion", 2);
        putArity("current_char_conversion", 2); putArity("current_prolog_flag", 2);
        putArity("nb_current", 2); putArity("nb_delete", 1); putArity("nb_getval", 2);
        putArity("nb_setval", 2); putArity("set_prolog_flag", 2); putArity("statistics", 0, 2);
        // term
        putArity("number_vars", 3); putArity("numbervars", 3);
        // type
        putArity("acyclic_term", 1); putArity("proper_list", 1); putArity("string", 1);
        // xml
        putArity("xml_parse", 2); putArity("xml_serialize", 2); putArity("xpath", 3);
        // the natives that implement an arity the entries above did not list
        putArity("copy_term", 3);
        putArity("findall", 4);
        putArity("put_code", 2);
        // END_CHANGE: ISS-2025-0685
    }

    private static void putArity(String name, int... arities) {
        Set<Integer> set = BUILTIN_ARITIES.computeIfAbsent(name, k -> new HashSet<>());
        for (int a : arities) {
            set.add(a);
        }
    }
    // END_CHANGE: ISS-2025-0091

    /**
     * Register a built-in predicate.
     */
    public void registerBuiltIn(String name, BuiltIn builtIn) {
        // START_CHANGE: ISS-2025-0674 - FINE, not WARNING: every `new Prolog()` re-registers the 13
        // CLP(FD) names deliberately (the v2 solver replaces the legacy entries), so the warning
        // was 26 lines of stderr noise on every CLI start (`java -jar jprolog.jar`) and in every
        // embedder's log, and said nothing actionable.
        if (builtIns.containsKey(name) && LOGGER.isLoggable(java.util.logging.Level.FINE)) {
            LOGGER.fine("Overriding existing built-in predicate: " + name);
        }
        // END_CHANGE: ISS-2025-0674
        builtIns.put(name, builtIn);
        modCount++;                                                    // ISS-2025-0542
    }

    // START_CHANGE: ISS-2025-0542 - wave P2.3: lets the v4 call-site cache notice a (re)registration.
    private volatile int modCount;

    /** Bumped by every register/unregister. */
    public int modCount() { return modCount; }
    // END_CHANGE: ISS-2025-0542

    // START_CHANGE: ISS-2025-0338 - support a sandbox/safe-mode that removes unsafe built-ins
    /** Snapshot of the registered predicate names (this instance only). */
    public java.util.Set<String> getBuiltInNames() {
        return new java.util.HashSet<>(builtIns.keySet());
    }

    /** Remove a built-in from THIS instance. isBuiltIn() checks {@code builtIns} first, so an
     *  unregistered predicate becomes unavailable (it falls through to user clauses / existence_error). */
    public void unregisterBuiltIn(String name) {
        builtIns.remove(name);
        modCount++;                                                    // ISS-2025-0542
    }
    // END_CHANGE: ISS-2025-0338

    /**
     * Get a built-in predicate by name.
     */
    public BuiltIn getBuiltIn(String name) {
        return builtIns.get(name);
    }

    /**
     * Check if a built-in predicate exists.
     */
    public boolean hasBuiltIn(String name) {
        return builtIns.containsKey(name);
    }

    /**
     * Check if a built-in predicate exists with specific arity.
     */
    public boolean isBuiltIn(String functor, int arity) {
        if (!builtIns.containsKey(functor)) {
            return false;
        }

        // Check exact arity match
        Set<Integer> arities = BUILTIN_ARITIES.get(functor);
        if (arities != null) {
            return arities.contains(arity);
        }

        // Check arity range (e.g., call/1..8)
        int[] range = BUILTIN_ARITY_RANGES.get(functor);
        if (range != null) {
            return arity >= range[0] && arity <= range[1];
        }

        // START_CHANGE: ISS-2025-0685 - a name registered at run time with its arities
        Set<Integer> dyn = dynamicArities.get(functor);
        if (dyn != null) {
            return dyn.contains(arity);
        }
        // Registered but no arity declaration: only an EMBEDDER's registerBuiltIn(name, b) can
        // produce this (every name new Prolog() registers is declared above, and
        // ExtendedLibraryErrorsTest pins it), so the historical any-arity answer is kept for it.
        return true;
        // END_CHANGE: ISS-2025-0685
    }

    // START_CHANGE: ISS-2025-0685 - wave Q1.2: exact arities for built-ins registered at run time
    private final Map<String, Set<Integer>> dynamicArities = new java.util.concurrent.ConcurrentHashMap<>();

    /**
     * Register a built-in predicate that exists at exactly the given arities: any other arity of
     * {@code name} is an unknown procedure ({@code existence_error(procedure, Name/Arity)}).
     */
    public void registerBuiltIn(String name, BuiltIn builtIn, int... arities) {
        registerBuiltIn(name, builtIn);
        Set<Integer> set = new HashSet<>();
        for (int a : arities) set.add(a);
        dynamicArities.put(name, set);
    }

    /** The arities the built-in NAME table declares for {@code name} (empty when none). */
    public static Set<Integer> staticArities(String name) {
        Set<Integer> r = new java.util.TreeSet<>();
        Set<Integer> st = BUILTIN_ARITIES.get(name);
        if (st != null) r.addAll(st);
        int[] range = BUILTIN_ARITY_RANGES.get(name);
        if (range != null) for (int a = range[0]; a <= range[1]; a++) r.add(a);
        return r;
    }

    /**
     * The arities {@code name} is declared at, or {@code null} when it has no declaration (an
     * embedder's registration without arities, which is a built-in at every arity).
     */
    public Set<Integer> declaredArities(String name) {
        Set<Integer> dyn = dynamicArities.get(name);
        if (dyn != null) return java.util.Collections.unmodifiableSet(dyn);
        Set<Integer> st = BUILTIN_ARITIES.get(name);
        int[] range = BUILTIN_ARITY_RANGES.get(name);
        if (st == null && range == null) return null;
        Set<Integer> r = new java.util.TreeSet<>();
        if (st != null) r.addAll(st);
        if (range != null) for (int a = range[0]; a <= range[1]; a++) r.add(a);
        return r;
    }
    // END_CHANGE: ISS-2025-0685
}
