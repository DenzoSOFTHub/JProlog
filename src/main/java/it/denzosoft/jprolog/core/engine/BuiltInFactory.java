package it.denzosoft.jprolog.core.engine;

import it.denzosoft.jprolog.builtin.*;
import it.denzosoft.jprolog.builtin.control.ControlConstruct;
import it.denzosoft.jprolog.builtin.control.Unify;
import it.denzosoft.jprolog.builtin.control.NotUnify;
import it.denzosoft.jprolog.builtin.control.UnifyWithOccursCheck;
import it.denzosoft.jprolog.builtin.control.Is;
import it.denzosoft.jprolog.builtin.control.Cut;
import it.denzosoft.jprolog.builtin.control.Repeat;
import it.denzosoft.jprolog.builtin.control.Findall;
import it.denzosoft.jprolog.builtin.control.Bagof;
import it.denzosoft.jprolog.builtin.control.Setof;
import it.denzosoft.jprolog.builtin.arithmetic.ArithmeticComparison;
import it.denzosoft.jprolog.builtin.term.TermComparison;
import it.denzosoft.jprolog.builtin.term.TermConstruction;
import it.denzosoft.jprolog.builtin.type.*;
import it.denzosoft.jprolog.builtin.atom.*;
import it.denzosoft.jprolog.builtin.database.*;
import it.denzosoft.jprolog.builtin.io.*;
import it.denzosoft.jprolog.builtin.list.*;
import it.denzosoft.jprolog.builtin.conversion.*;
import it.denzosoft.jprolog.builtin.exception.*;
import it.denzosoft.jprolog.builtin.meta.*;
import it.denzosoft.jprolog.builtin.debug.*;
import it.denzosoft.jprolog.builtin.system.*;
import it.denzosoft.jprolog.builtin.string.*;
import it.denzosoft.jprolog.builtin.character.*;
import it.denzosoft.jprolog.builtin.dcg.*;
import it.denzosoft.jprolog.builtin.unification.*;
import it.denzosoft.jprolog.builtin.jdbc.*;
import it.denzosoft.jprolog.builtin.network.*;
import it.denzosoft.jprolog.builtin.crypto.*;
import it.denzosoft.jprolog.builtin.json.*;
import it.denzosoft.jprolog.builtin.datetime.*;
import it.denzosoft.jprolog.builtin.filesystem.*;
import it.denzosoft.jprolog.builtin.os.*;
import it.denzosoft.jprolog.builtin.regex.*;
import it.denzosoft.jprolog.builtin.xml.*;
import it.denzosoft.jprolog.builtin.threading.*;
import it.denzosoft.jprolog.builtin.csv.*;
import it.denzosoft.jprolog.builtin.logging.*;
import it.denzosoft.jprolog.builtin.http.*;
import it.denzosoft.jprolog.builtin.clpfd.ClpfdPredicates;
import it.denzosoft.jprolog.builtin.graph.GraphPredicates;
// START_CHANGE: ISS-2025-0126 - Persistence built-in predicates
import it.denzosoft.jprolog.builtin.persistence.PersistencePredicates;
// END_CHANGE: ISS-2025-0126
// START_CHANGE: ISS-2025-0160 - Java FFI built-in predicates
import it.denzosoft.jprolog.builtin.ffi.JavaFFI;
// END_CHANGE: ISS-2025-0160

import java.util.HashMap;
import java.util.Map;
import java.util.function.Supplier;


public class BuiltInFactory {
    static final Map<String, Supplier<BuiltIn>> FACTORY_MAP = new HashMap<>(); // Changed to package-private
    
    static {
        // START_CHANGE: ISS-2025-0007 - Register missing inequality operator \=
        // Unification
        registerFactory("=", Unify::new);
        registerFactory("\\=", NotUnify::new);
        registerFactory("unify_with_occurs_check", UnifyWithOccursCheck::new);
        // END_CHANGE: ISS-2025-0007
        
        // Type checking
        registerFactory("var", VarCheck::new);
        registerFactory("nonvar", NonVarCheck::new);
        registerFactory("atom", AtomCheck::new);
        registerFactory("integer", IntegerCheck::new);
        registerFactory("float", FloatCheck::new);
        registerFactory("atomic", AtomicCheck::new);
        registerFactory("compound", CompoundCheck::new);
        registerFactory("number", NumberCheck::new);
        registerFactory("callable", CallableCheck::new);
        registerFactory("is_list", IsListCheck::new);
        registerFactory("ground", GroundCheck::new);
        registerFactory("simple", SimpleCheck::new);
        registerFactory("partial_list", PartialListCheck::new);
        // START_CHANGE: ISS-2025-0404 - string/1 type check (true for PrologString only)
        registerFactory("string", StringCheck::new);
        // END_CHANGE: ISS-2025-0404
        // START_CHANGE: ISS-2025-0170 - Add missing ISO predicates acyclic_term/1 and proper_list/1
        registerFactory("acyclic_term", AcyclicTermCheck::new);
        registerFactory("proper_list", ProperListCheck::new);
        // END_CHANGE: ISS-2025-0170
        
        // Term comparison
        registerFactory("@=<", () -> new TermComparison(TermComparison.ComparisonType.AT_LESS_EQUAL));
        registerFactory("@<", () -> new TermComparison(TermComparison.ComparisonType.AT_LESS));
        registerFactory("@>", () -> new TermComparison(TermComparison.ComparisonType.AT_GREATER));
        registerFactory("@>=", () -> new TermComparison(TermComparison.ComparisonType.AT_GREATER_EQUAL));
        registerFactory("==", () -> new TermComparison(TermComparison.ComparisonType.TERM_EQUAL));
        registerFactory("\\==", () -> new TermComparison(TermComparison.ComparisonType.TERM_NOT_EQUAL));
        
        // Term construction
        registerFactory("functor", () -> new TermConstruction(TermConstruction.OperationType.FUNCTOR));
        registerFactory("arg", () -> new TermConstruction(TermConstruction.OperationType.ARG));
        registerFactory("=..", () -> new TermConstruction(TermConstruction.OperationType.UNIV));
        registerFactory("copy_term", () -> new TermConstruction(TermConstruction.OperationType.COPY_TERM));
        
        // Phase 5: Term Operations & Meta-programming
        registerFactory("compare", it.denzosoft.jprolog.builtin.term.Compare::new);
        registerFactory("term_variables", it.denzosoft.jprolog.builtin.term.TermVariables::new);
        registerFactory("subsumes_term", it.denzosoft.jprolog.builtin.term.SubsumesTerm::new);
        // START_CHANGE: ISS-2025-0670 - term_to_atom/2, atom_to_term/3, read/1,2, read_term/2,3,
        // between/3 and dcg_translate_rule/2 are native or inline in core.engine.v4 at every
        // registered arity; their legacy classes (TermToAtom, AtomToTerm, Read, ReadTerm, Between,
        // DCGUtils.DCGTranslateRule) were unreachable and are deleted. The registry keeps a
        // placeholder under each name so the registry-side answers stay as they were (built-in
        // name listings, safe-mode snapshots, predicate_property). END_CHANGE: ISS-2025-0670
        registerFactory("term_to_atom", () -> new it.denzosoft.jprolog.builtin.control.ControlConstruct("term_to_atom/2"));
        // START_CHANGE: R1 - setarg/3
        registerFactory("setarg", it.denzosoft.jprolog.builtin.term.SetArg::new);
        // END_CHANGE: R1
        // START_CHANGE: ISS-2025-0238 - atom_to_term/3
        registerFactory("atom_to_term", () -> new it.denzosoft.jprolog.builtin.control.ControlConstruct("atom_to_term/3"));   // ISS-2025-0670
        // END_CHANGE: ISS-2025-0238
        registerFactory("numbervars", it.denzosoft.jprolog.builtin.term.NumberVars::new);
        registerFactory("number_vars", it.denzosoft.jprolog.builtin.term.NumberVars::new);
        
        // Arithmetic evaluation
        registerFactory("is", Is::new);
        
        // Arithmetic comparison
        registerFactory("=:=", () -> new ArithmeticComparison(ArithmeticComparison.ComparisonType.EQUAL));
        registerFactory("=\\=", () -> new ArithmeticComparison(ArithmeticComparison.ComparisonType.NOT_EQUAL));
        registerFactory("<", () -> new ArithmeticComparison(ArithmeticComparison.ComparisonType.LESS));
        registerFactory("=<", () -> new ArithmeticComparison(ArithmeticComparison.ComparisonType.LESS_EQUAL));
        registerFactory(">", () -> new ArithmeticComparison(ArithmeticComparison.ComparisonType.GREATER));
        registerFactory(">=", () -> new ArithmeticComparison(ArithmeticComparison.ComparisonType.GREATER_EQUAL));
        
        // Advanced arithmetic (ISO Prolog)
        registerFactory("between", () -> new it.denzosoft.jprolog.builtin.control.ControlConstruct("between/3"));   // ISS-2025-0670
        registerFactory("succ", it.denzosoft.jprolog.builtin.arithmetic.Succ::new);
        registerFactory("plus", it.denzosoft.jprolog.builtin.arithmetic.Plus::new);
        
        // List operations
        registerFactory("append", it.denzosoft.jprolog.builtin.list.Append::new); // Specify full package
        registerFactory("length", Length::new);
        registerFactory("member", Member::new);
        registerFactory("nth0", Nth0::new);
        registerFactory("nth1", Nth1::new);
        registerFactory("msort", Msort::new);
        registerFactory("reverse", Reverse::new);
        registerFactory("select", Select::new);
        registerFactory("sort", Sort::new);
        // START_CHANGE: ISS-2025-0061 - Add keysort/2
        registerFactory("keysort", it.denzosoft.jprolog.builtin.list.KeySort::new);
        // START_CHANGE: CR-2025-0008 - List operations extension
        registerFactory("permutation", it.denzosoft.jprolog.builtin.list.Permutation::new);
        registerFactory("predsort", () -> new it.denzosoft.jprolog.builtin.list.PredSort(null));
        registerFactory("last", it.denzosoft.jprolog.builtin.list.Last::new);
        registerFactory("flatten", it.denzosoft.jprolog.builtin.list.Flatten::new);
        registerFactory("numlist", it.denzosoft.jprolog.builtin.list.Numlist::new);
        registerFactory("sum_list", it.denzosoft.jprolog.builtin.list.SumList::new);
        registerFactory("sumlist", it.denzosoft.jprolog.builtin.list.SumList::new);
        registerFactory("max_list", it.denzosoft.jprolog.builtin.list.MaxList::new);
        registerFactory("min_list", it.denzosoft.jprolog.builtin.list.MinList::new);
        registerFactory("delete", it.denzosoft.jprolog.builtin.list.Delete::new);
        registerFactory("subtract", it.denzosoft.jprolog.builtin.list.Subtract::new);
        registerFactory("intersection", it.denzosoft.jprolog.builtin.list.Intersection::new);
        registerFactory("union", it.denzosoft.jprolog.builtin.list.Union::new);
        // START_CHANGE: v2.9.7 - pairs_* + must_be SWI library predicates
        registerFactory("pairs_keys", () -> new it.denzosoft.jprolog.builtin.list.Pairs(
            it.denzosoft.jprolog.builtin.list.Pairs.Mode.KEYS));
        registerFactory("pairs_values", () -> new it.denzosoft.jprolog.builtin.list.Pairs(
            it.denzosoft.jprolog.builtin.list.Pairs.Mode.VALUES));
        registerFactory("pairs_keys_values", () -> new it.denzosoft.jprolog.builtin.list.Pairs(
            it.denzosoft.jprolog.builtin.list.Pairs.Mode.KEYS_VALUES));
        registerFactory("must_be", () -> new it.denzosoft.jprolog.builtin.type.MustBe());
        // END_CHANGE: v2.9.7

        // Higher-order list predicates (context-dependent)
        registerFactory("maplist", () -> new it.denzosoft.jprolog.builtin.list.MapList(null));
        registerFactory("include", () -> new it.denzosoft.jprolog.builtin.list.Include(null));
        registerFactory("exclude", () -> new it.denzosoft.jprolog.builtin.list.Exclude(null));
        registerFactory("foldl", () -> new it.denzosoft.jprolog.builtin.list.Foldl(null));
        // START_CHANGE: ISS-2025-0221 - partition/4 NOT registered as builtin to avoid shadowing user-defined partition/N
        // (e.g. quicksort uses partition(Pivot,List,Less,Greater)). Class exists for explicit invocation.
        // END_CHANGE: ISS-2025-0221
        // END_CHANGE: CR-2025-0008
        // END_CHANGE: ISS-2025-0061
        
        // Control
        registerFactory("!", Cut::new);
        registerFactory("cut", Cut::new);
        registerFactory("repeat", Repeat::new);
        
        // START_CHANGE: ISS-2025-0485 - wave W9: the ISO control constructs are native in BOTH
        // surviving engines and were only ever dispatched from the registry by the (now deleted)
        // recursive solver. They stay REGISTERED as placeholders so isBuiltIn/2 keeps answering
        // true, which is what makes assert/retract/clause raise permission_error on them.
        registerFactory("\\+", () -> new ControlConstruct("\\+/1"));
        registerFactory("->", () -> new ControlConstruct("->/2"));
        registerFactory(";", () -> new ControlConstruct(";/2"));
        // ','/2 has never been registered: it is intercepted before the registry by every engine.
        // END_CHANGE: ISS-2025-0485

        // I/O
        registerFactory("write", Write::new);
        registerFactory("writeln", Writeln::new);
        // START_CHANGE: ISS-2025-0378 - print/1,2 (write semantics + numbervars(true))
        registerFactory("print", () -> new it.denzosoft.jprolog.builtin.io.Print(null));
        // START_CHANGE: ISS-2025-0475 - engine v4 wave W7 (design B.12): the writer-backed family
        registerFactory("portray_clause", it.denzosoft.jprolog.builtin.io.PortrayClause::new);
        registerFactory("print_message", it.denzosoft.jprolog.builtin.io.PrintMessage::new);
        // END_CHANGE: ISS-2025-0475
        // END_CHANGE: ISS-2025-0378
        registerFactory("nl", Nl::new);
        registerFactory("read", () -> new it.denzosoft.jprolog.builtin.control.ControlConstruct("read/1,2"));   // ISS-2025-0670
        registerFactory("tab", it.denzosoft.jprolog.builtin.io.Tab::new);
        registerFactory("with_output_to", () -> new it.denzosoft.jprolog.builtin.io.WithOutputTo(null));
        
        // Character I/O (ISO Prolog)
        registerFactory("get_char", GetChar::new);
        registerFactory("put_char", PutChar::new);
        registerFactory("get_code", GetCode::new);
        registerFactory("put_code", PutCode::new);
        
        // Stream I/O (ISO Prolog)
        registerFactory("open", Open::new);
        // START_CHANGE: CR-2025-0005 - seek/4
        registerFactory("seek", () -> new it.denzosoft.jprolog.builtin.io.Seek());
        // END_CHANGE: CR-2025-0005
        registerFactory("close", Close::new);
        registerFactory("current_input", CurrentInput::new);
        registerFactory("current_output", CurrentOutput::new);
        registerFactory("set_input", SetInput::new);
        registerFactory("set_output", SetOutput::new);
        
        // Phase 4 I/O predicates (ISO Prolog)
        registerFactory("flush_output", FlushOutput::new);
        registerFactory("peek_char", PeekChar::new);
        registerFactory("peek_code", PeekCode::new);
        registerFactory("peek_byte", PeekByte::new);
        registerFactory("get_byte", GetByte::new);
        registerFactory("put_byte", PutByte::new);
        registerFactory("at_end_of_stream", AtEndOfStream::new);
        registerFactory("stream_property", StreamProperty::new);
        // START_CHANGE: ISS-2025-0473 - engine v4 wave W7 (design B.11): stream introspection
        registerFactory("set_stream", () -> new it.denzosoft.jprolog.builtin.io.StreamInfo(
            it.denzosoft.jprolog.builtin.io.StreamInfo.Kind.SET_STREAM));
        registerFactory("stream_position_data", () -> new it.denzosoft.jprolog.builtin.io.StreamInfo(
            it.denzosoft.jprolog.builtin.io.StreamInfo.Kind.POSITION_DATA));
        registerFactory("character_count", () -> new it.denzosoft.jprolog.builtin.io.StreamInfo(
            it.denzosoft.jprolog.builtin.io.StreamInfo.Kind.CHARACTER_COUNT));
        registerFactory("line_count", () -> new it.denzosoft.jprolog.builtin.io.StreamInfo(
            it.denzosoft.jprolog.builtin.io.StreamInfo.Kind.LINE_COUNT));
        registerFactory("line_position", () -> new it.denzosoft.jprolog.builtin.io.StreamInfo(
            it.denzosoft.jprolog.builtin.io.StreamInfo.Kind.LINE_POSITION));
        registerFactory("current_stream", () -> new it.denzosoft.jprolog.builtin.io.StreamInfo(
            it.denzosoft.jprolog.builtin.io.StreamInfo.Kind.CURRENT_STREAM));
        // END_CHANGE: ISS-2025-0473
        registerFactory("writeq", WriteQ::new);
        registerFactory("write_canonical", WriteCanonical::new);
        
        // Database
        registerFactory("listing", Listing0::new); // listing/0 - context-dependent
        
        // Atom operations
        registerFactory("atom_length", AtomLength::new);
        registerFactory("atom_concat", AtomConcat::new);
        registerFactory("sub_atom", SubAtom::new);
        
        // Type conversion
        registerFactory("atom_number", AtomNumber::new);
        registerFactory("atom_chars", AtomChars::new);
        registerFactory("atom_codes", AtomCodes::new);
        registerFactory("number_chars", NumberChars::new);
        registerFactory("number_codes", NumberCodes::new);
        // START_CHANGE: ISS-2025-0009 - Register missing to_codes/2 built-in (use simple version)
        registerFactory("to_codes", ToCodesSimple::new);
        registerFactory("string_to_atom", it.denzosoft.jprolog.builtin.conversion.StringToAtom::new);
        registerFactory("number_to_atom", it.denzosoft.jprolog.builtin.conversion.NumberToAtom::new);
        registerFactory("atom_to_number", it.denzosoft.jprolog.builtin.conversion.NumberToAtom::new);
        registerFactory("string_code", it.denzosoft.jprolog.builtin.conversion.StringCode::new);
        // END_CHANGE: ISS-2025-0009
        
        // String operations
        registerFactory("string_length", StringLength::new);
        registerFactory("string_concat", StringConcat::new);
        registerFactory("sub_string", SubString::new);
        registerFactory("string_chars", StringChars::new);
        registerFactory("string_codes", StringCodes::new);
        registerFactory("atom_string", AtomString::new);
        registerFactory("number_string", NumberString::new);
        
        // Collection predicates (context-dependent)
        registerFactory("findall", () -> new Findall(null)); // the context is passed at call time
        registerFactory("bagof", () -> new Bagof(null)); // the context is passed at call time
        registerFactory("setof", () -> new Setof(null)); // the context is passed at call time
        // START_CHANGE: ISS-2025-0122 - aggregate_all/3
        registerFactory("aggregate_all", () -> new it.denzosoft.jprolog.builtin.meta.AggregateAll(null)); // the context is passed at call time
        // END_CHANGE: ISS-2025-0122
        
        // Exception handling (ISO Prolog)
        registerFactory("catch", () -> new ControlConstruct("catch/3"));   // ISS-2025-0485
        registerFactory("throw", Throw::new);
        registerFactory("halt", Halt::new);
        
        // Meta-predicates (ISO Prolog)
        registerFactory("call", () -> new ControlConstruct("call/N"));     // ISS-2025-0485
        registerFactory("once", () -> new Once(null)); // the context is passed at call time
        registerFactory("ignore", () -> new Ignore(null)); // the context is passed at call time
        registerFactory("forall", () -> new ForAll(null)); // the context is passed at call time
        // START_CHANGE: ISS-2025-0273 - setup_call_cleanup/3 and call_cleanup/2
        registerFactory("setup_call_cleanup", () -> new it.denzosoft.jprolog.builtin.meta.SetupCallCleanup(null));
        registerFactory("call_cleanup", () -> new it.denzosoft.jprolog.builtin.meta.SetupCallCleanup(null));
        // END_CHANGE: ISS-2025-0273
        // START_CHANGE: ISS-2025-0398 - V^Goal as an ordinary goal behaves as call(Goal)
        registerFactory("^", () -> new ControlConstruct("^/2"));           // ISS-2025-0485
        // END_CHANGE: ISS-2025-0398
        // START_CHANGE: LIM-005 - predicate_property/2 meta-predicate
        registerFactory("predicate_property", () -> new PredicateProperty(null)); // the context is passed at call time
        // END_CHANGE: LIM-005

        // Dynamic database operations (ISO Prolog)
        // START_CHANGE: ISS-2025-0023 - Add assert/1 as alias for assertz/1
        registerFactory("assert", () -> new Assertz(null)); // assert/1 is alias for assertz/1
        // END_CHANGE: ISS-2025-0023
        registerFactory("asserta", () -> new Asserta(null)); // the context is passed at call time
        registerFactory("assertz", () -> new Assertz(null)); // the context is passed at call time
        registerFactory("retract", () -> new Retract(null)); // the context is passed at call time
        registerFactory("retractall", () -> new Retractall(null)); // the context is passed at call time
        registerFactory("abolish", () -> new Abolish(null)); // the context is passed at call time
        registerFactory("current_predicate", () -> new CurrentPredicate(null)); // the context is passed at call time
        registerFactory("clause", () -> new it.denzosoft.jprolog.builtin.database.Clause(null)); // the context is passed at call time
        // START_CHANGE: ISS-2025-0369 - dynamic/1 callable as a runtime goal (was silently failing)
        registerFactory("dynamic", () -> new it.denzosoft.jprolog.builtin.database.Dynamic(null)); // the context is passed at call time
        // END_CHANGE: ISS-2025-0369

        // Debugging predicates (ISO Prolog)
        registerFactory("trace", Trace::new);
        registerFactory("notrace", NoTrace::new);
        registerFactory("spy", Spy::new);
        registerFactory("nospy", NoSpy::new);
        // START_CHANGE: CR-2025-0009 - debugging/0, spying/1, profile/0, noprofile/0, profile_data/1, reset_profile/0
        registerFactory("debugging", () -> new it.denzosoft.jprolog.builtin.debug.Debugging());
        registerFactory("spying", () -> new it.denzosoft.jprolog.builtin.debug.Spying());
        registerFactory("profile", () -> new it.denzosoft.jprolog.builtin.debug.Profile(
            it.denzosoft.jprolog.builtin.debug.Profile.Mode.PROFILE));
        registerFactory("noprofile", () -> new it.denzosoft.jprolog.builtin.debug.Profile(
            it.denzosoft.jprolog.builtin.debug.Profile.Mode.NOPROFILE));
        registerFactory("profile_data", () -> new it.denzosoft.jprolog.builtin.debug.Profile(
            it.denzosoft.jprolog.builtin.debug.Profile.Mode.PROFILE_DATA));
        registerFactory("reset_profile", () -> new it.denzosoft.jprolog.builtin.debug.Profile(
            it.denzosoft.jprolog.builtin.debug.Profile.Mode.RESET_PROFILE));
        // END_CHANGE: CR-2025-0009
        // START_CHANGE: ISS-2025-0172 - Leash built-in for debug port filtering
        registerFactory("leash", Leash::new);
        // END_CHANGE: ISS-2025-0172
        
        // System predicates (ISO Prolog)
        registerFactory("current_prolog_flag", CurrentPrologFlag::new);
        registerFactory("set_prolog_flag", SetPrologFlag::new);
        // START_CHANGE: ISS-2025-0049 - Implement comprehensive op/3 system
        registerFactory("op", () -> new OperatorDefinition(OperatorDefinition.OperatorType.OP));
        registerFactory("current_op", () -> new OperatorDefinition(OperatorDefinition.OperatorType.CURRENT_OP));
        // END_CHANGE: ISS-2025-0049
        // START_CHANGE: ISS-2025-0048 - Character conversion predicates
        registerFactory("char_conversion", () -> new it.denzosoft.jprolog.builtin.system.CharConversion(
            it.denzosoft.jprolog.builtin.system.CharConversion.Mode.DEFINE));
        registerFactory("current_char_conversion", () -> new it.denzosoft.jprolog.builtin.system.CharConversion(
            it.denzosoft.jprolog.builtin.system.CharConversion.Mode.QUERY));
        // END_CHANGE: ISS-2025-0048
        
        // Advanced I/O predicates (ISO Prolog)
        registerFactory("read_term", () -> new it.denzosoft.jprolog.builtin.control.ControlConstruct("read_term/2,3"));   // ISS-2025-0670
        registerFactory("write_term", () -> new WriteTerm(null)); // the context is passed at call time
        registerFactory("format", () -> new Format(null)); // the context is passed at call time
        // START_CHANGE: LIM-007 - Stream Repositioning predicates
        registerFactory("set_stream_position", SetStreamPosition::new);
        registerFactory("stream_position", StreamPosition::new);
        // END_CHANGE: LIM-007

        // Character predicates (ISO Prolog)
        registerFactory("char_type", CharType::new);
        registerFactory("char_code", CharCode::new);
        // START_CHANGE: LIM-006 - code_type/2 character code classification
        registerFactory("code_type", CodeType::new);
        // END_CHANGE: LIM-006

        // Phase 6: Character & String Processing
        registerFactory("upcase_atom", it.denzosoft.jprolog.builtin.character.UpCase::new);
        registerFactory("downcase_atom", it.denzosoft.jprolog.builtin.character.DownCase::new);
        registerFactory("split_string", it.denzosoft.jprolog.builtin.string.SplitString::new);
        registerFactory("atomic_list_concat", it.denzosoft.jprolog.builtin.string.JoinString::new);
        
        // DCG predicates (ISO Prolog and DTS 13211-3)
        registerFactory("phrase", () -> new Phrase(null)); // the context is passed at call time (legacy)
        
        // Phase 8: Enhanced DCG predicates per ISO/IEC DTS 13211-3
        registerFactory("enhanced_phrase", it.denzosoft.jprolog.builtin.dcg.EnhancedPhrase::new);
        registerFactory("phrase_with_options", it.denzosoft.jprolog.builtin.dcg.PhraseWithOptions::new);
        registerFactory("call_dcg", it.denzosoft.jprolog.builtin.dcg.DCGUtils.CallDCG::new);
        registerFactory("dcg_translate_rule", () -> new it.denzosoft.jprolog.builtin.control.ControlConstruct("dcg_translate_rule/2"));   // ISS-2025-0670
        registerFactory("dcg_body", it.denzosoft.jprolog.builtin.dcg.DCGUtils.DCGBody::new);
        
        // Additional system predicates
        registerFactory("statistics", () -> new Statistics(null)); // the context is passed at call time

        // START_CHANGE: ISS-2025-0092 - Tabling (memoization) predicates
        registerFactory("table", TableDirective::new);
        registerFactory("abolish_all_tables", AbolishAllTables::new);
        // START_CHANGE: ISS-2025-0124 - abolish_table/1 predicate
        registerFactory("abolish_table", AbolishTable::new);
        // END_CHANGE: ISS-2025-0124
        // END_CHANGE: ISS-2025-0092
        // START_CHANGE: ISS-2025-0064 - Remove duplicate current_op registration (already at line 218)
        // END_CHANGE: ISS-2025-0064

        // START_CHANGE: ISS-2025-0108 - JDBC built-in predicates
        registerFactory("jdbc_driver_load", JdbcDriverLoad::new);
        registerFactory("jdbc_connect", JdbcConnect::new);
        registerFactory("jdbc_disconnect", JdbcDisconnect::new);
        registerFactory("jdbc_query", JdbcQuery::new);
        registerFactory("jdbc_execute_update", JdbcExecuteUpdate::new);
        registerFactory("jdbc_set_autocommit", () -> new JdbcTransaction(JdbcTransaction.Mode.SET_AUTOCOMMIT));
        registerFactory("jdbc_commit", () -> new JdbcTransaction(JdbcTransaction.Mode.COMMIT));
        registerFactory("jdbc_rollback", () -> new JdbcTransaction(JdbcTransaction.Mode.ROLLBACK));
        registerFactory("jdbc_tables", () -> new JdbcMetadata(JdbcMetadata.Mode.TABLES));
        registerFactory("jdbc_columns", () -> new JdbcMetadata(JdbcMetadata.Mode.COLUMNS));
        // END_CHANGE: ISS-2025-0108

        // START_CHANGE: ISS-2025-0110 - Prepared statements and stored procedures
        registerFactory("jdbc_prepare", JdbcPrepare::new);
        registerFactory("jdbc_set_param", JdbcSetParam::new);
        registerFactory("jdbc_set_params", JdbcSetParams::new);
        registerFactory("jdbc_execute_prepared_query", () -> new JdbcExecutePrepared(JdbcExecutePrepared.Mode.QUERY));
        registerFactory("jdbc_execute_prepared_update", () -> new JdbcExecutePrepared(JdbcExecutePrepared.Mode.UPDATE));
        registerFactory("jdbc_close_statement", () -> new JdbcExecutePrepared(JdbcExecutePrepared.Mode.CLOSE));
        registerFactory("jdbc_prepare_call", () -> new JdbcCallProcedure(JdbcCallProcedure.Mode.PREPARE_CALL));
        registerFactory("jdbc_call_set_param", () -> new JdbcCallProcedure(JdbcCallProcedure.Mode.SET_PARAM));
        registerFactory("jdbc_call_register_out", () -> new JdbcCallProcedure(JdbcCallProcedure.Mode.REGISTER_OUT));
        registerFactory("jdbc_call_execute", () -> new JdbcCallProcedure(JdbcCallProcedure.Mode.EXECUTE));
        registerFactory("jdbc_call_get_result", () -> new JdbcCallProcedure(JdbcCallProcedure.Mode.GET_RESULT));
        registerFactory("jdbc_call_get_resultset", () -> new JdbcCallProcedure(JdbcCallProcedure.Mode.GET_RESULTSET));
        // END_CHANGE: ISS-2025-0110

        // START_CHANGE: ISS-2025-0111 - CLOB and BLOB support
        registerFactory("jdbc_set_clob", () -> new JdbcLob(JdbcLob.Mode.SET_CLOB));
        registerFactory("jdbc_set_blob", () -> new JdbcLob(JdbcLob.Mode.SET_BLOB_FILE));
        registerFactory("jdbc_set_blob_bytes", () -> new JdbcLob(JdbcLob.Mode.SET_BLOB_BYTES));
        registerFactory("jdbc_get_clob", () -> new JdbcLob(JdbcLob.Mode.GET_CLOB));
        registerFactory("jdbc_get_blob_to_file", () -> new JdbcLob(JdbcLob.Mode.GET_BLOB_FILE));
        registerFactory("jdbc_get_blob_bytes", () -> new JdbcLob(JdbcLob.Mode.GET_BLOB_BYTES));
        // END_CHANGE: ISS-2025-0111

        // START_CHANGE: ISS-2025-0109 - Network communication built-in predicates
        registerFactory("tcp_connect", TcpConnect::new);
        registerFactory("tcp_server_socket", TcpServerSocket::new);
        registerFactory("tcp_accept", TcpAccept::new);
        registerFactory("tcp_send", TcpSend::new);
        registerFactory("tcp_receive", TcpReceive::new);
        registerFactory("tcp_close", TcpClose::new);
        registerFactory("http_request", () -> new HttpRequest(HttpRequest.Mode.REQUEST));
        registerFactory("http_post", () -> new HttpRequest(HttpRequest.Mode.POST));
        registerFactory("udp_socket", () -> new UdpSocket(UdpSocket.Mode.CREATE));
        registerFactory("udp_send", () -> new UdpSocket(UdpSocket.Mode.SEND));
        registerFactory("udp_receive", () -> new UdpSocket(UdpSocket.Mode.RECEIVE));
        registerFactory("udp_close", () -> new UdpSocket(UdpSocket.Mode.CLOSE));
        registerFactory("hostname_address", HostLookup::new);
        // END_CHANGE: ISS-2025-0109

        // START_CHANGE: ISS-2025-0112 - Cryptographic built-in predicates
        registerFactory("md5_hash", () -> new CryptoHash(CryptoHash.Mode.MD5));
        registerFactory("sha256_hash", () -> new CryptoHash(CryptoHash.Mode.SHA256));
        registerFactory("sha512_hash", () -> new CryptoHash(CryptoHash.Mode.SHA512));
        registerFactory("crypto_hash", () -> new CryptoHash(CryptoHash.Mode.GENERIC));
        registerFactory("hmac", () -> new CryptoUtils(CryptoUtils.Mode.HMAC));
        registerFactory("base64_encode", () -> new CryptoUtils(CryptoUtils.Mode.BASE64_ENCODE));
        registerFactory("base64_decode", () -> new CryptoUtils(CryptoUtils.Mode.BASE64_DECODE));
        registerFactory("uuid", () -> new CryptoUtils(CryptoUtils.Mode.UUID));
        registerFactory("random_token", () -> new CryptoUtils(CryptoUtils.Mode.RANDOM_TOKEN));
        registerFactory("crypto_random_int", () -> new CryptoUtils(CryptoUtils.Mode.RANDOM_INT));
        // START_CHANGE: ISS-2025-0176 - AES encryption and password hashing
        registerFactory("crypto_aes_encrypt", () -> new CryptoUtils(CryptoUtils.Mode.AES_ENCRYPT));
        registerFactory("crypto_aes_decrypt", () -> new CryptoUtils(CryptoUtils.Mode.AES_DECRYPT));
        registerFactory("crypto_hash_password", () -> new CryptoUtils(CryptoUtils.Mode.HASH_PASSWORD));
        registerFactory("crypto_verify_password", () -> new CryptoUtils(CryptoUtils.Mode.VERIFY_PASSWORD));
        // END_CHANGE: ISS-2025-0176
        // END_CHANGE: ISS-2025-0112

        // START_CHANGE: ISS-2025-0113 - JSON built-in predicates
        registerFactory("json_parse", () -> new JsonPredicates(JsonPredicates.Mode.PARSE));
        registerFactory("json_serialize", () -> new JsonPredicates(JsonPredicates.Mode.SERIALIZE));
        registerFactory("json_get", () -> new JsonPredicates(JsonPredicates.Mode.GET));
        registerFactory("json_set", () -> new JsonPredicates(JsonPredicates.Mode.SET));
        registerFactory("json_keys", () -> new JsonPredicates(JsonPredicates.Mode.KEYS));
        registerFactory("json_member", () -> new JsonPredicates(JsonPredicates.Mode.MEMBER));
        // END_CHANGE: ISS-2025-0113

        // START_CHANGE: ISS-2025-0114 - Date/time built-in predicates
        registerFactory("get_time", () -> new DateTimePredicates(DateTimePredicates.Mode.GET_TIME));
        registerFactory("now", () -> new DateTimePredicates(DateTimePredicates.Mode.NOW));
        registerFactory("today", () -> new DateTimePredicates(DateTimePredicates.Mode.TODAY));
        registerFactory("format_time", () -> new DateTimePredicates(DateTimePredicates.Mode.FORMAT_TIME));
        registerFactory("parse_time", () -> new DateTimePredicates(DateTimePredicates.Mode.PARSE_TIME));
        registerFactory("date_add", () -> new DateTimePredicates(DateTimePredicates.Mode.DATE_ADD));
        registerFactory("date_diff", () -> new DateTimePredicates(DateTimePredicates.Mode.DATE_DIFF));
        registerFactory("day_of_week", () -> new DateTimePredicates(DateTimePredicates.Mode.DAY_OF_WEEK));
        registerFactory("date_parts", () -> new DateTimePredicates(DateTimePredicates.Mode.DATE_PARTS));
        registerFactory("time_parts", () -> new DateTimePredicates(DateTimePredicates.Mode.TIME_PARTS));
        // START_CHANGE: ISS-2025-0609 - SWI's stamp_date_time/3 and date_time_stamp/2
        registerFactory("stamp_date_time", () -> new DateTimePredicates(DateTimePredicates.Mode.STAMP_DATE_TIME));
        registerFactory("date_time_stamp", () -> new DateTimePredicates(DateTimePredicates.Mode.DATE_TIME_STAMP));
        // END_CHANGE: ISS-2025-0609
        // END_CHANGE: ISS-2025-0114

        // START_CHANGE: ISS-2025-0115 - File system built-in predicates
        // START_CHANGE: ISS-2025-0574 - 4.5 wave P3.1: loading files (safe-mode denied package)
        registerFactory("consult", () -> new LoadFiles(LoadFiles.Mode.CONSULT));
        registerFactory("ensure_loaded", () -> new LoadFiles(LoadFiles.Mode.ENSURE_LOADED));
        registerFactory("load_files", () -> new LoadFiles(LoadFiles.Mode.LOAD_FILES));
        registerFactory(".", () -> new LoadFiles(LoadFiles.Mode.LIST));
        registerFactory("make", () -> new LoadFiles(LoadFiles.Mode.MAKE));
        // END_CHANGE: ISS-2025-0574
        // START_CHANGE: ISS-2025-0576
        registerFactory("source_file", () -> new it.denzosoft.jprolog.builtin.system.SourceFiles(
            it.denzosoft.jprolog.builtin.system.SourceFiles.Mode.SOURCE_FILE));
        registerFactory("prolog_load_context", () -> new it.denzosoft.jprolog.builtin.system.SourceFiles(
            it.denzosoft.jprolog.builtin.system.SourceFiles.Mode.LOAD_CONTEXT));
        // END_CHANGE: ISS-2025-0576
        registerFactory("file_exists", () -> new FileSystemPredicates(FileSystemPredicates.Mode.FILE_EXISTS));
        registerFactory("directory_exists", () -> new FileSystemPredicates(FileSystemPredicates.Mode.DIR_EXISTS));
        registerFactory("make_directory", () -> new FileSystemPredicates(FileSystemPredicates.Mode.MAKE_DIR));
        registerFactory("make_directory_path", () -> new FileSystemPredicates(FileSystemPredicates.Mode.MAKE_DIR_PATH));
        registerFactory("delete_file", () -> new FileSystemPredicates(FileSystemPredicates.Mode.DELETE_FILE));
        registerFactory("delete_directory", () -> new FileSystemPredicates(FileSystemPredicates.Mode.DELETE_DIR));
        registerFactory("rename_file", () -> new FileSystemPredicates(FileSystemPredicates.Mode.RENAME));
        registerFactory("copy_file", () -> new FileSystemPredicates(FileSystemPredicates.Mode.COPY));
        registerFactory("file_size", () -> new FileSystemPredicates(FileSystemPredicates.Mode.FILE_SIZE));
        registerFactory("file_modified", () -> new FileSystemPredicates(FileSystemPredicates.Mode.FILE_MODIFIED));
        registerFactory("directory_files", () -> new FileSystemPredicates(FileSystemPredicates.Mode.DIR_FILES));
        registerFactory("working_directory", () -> new FileSystemPredicates(FileSystemPredicates.Mode.WORKING_DIR));
        registerFactory("absolute_file_name", () -> new FileSystemPredicates(FileSystemPredicates.Mode.ABS_FILE_NAME));
        registerFactory("read_file_to_atom", () -> new FileSystemPredicates(FileSystemPredicates.Mode.READ_FILE));
        registerFactory("write_atom_to_file", () -> new FileSystemPredicates(FileSystemPredicates.Mode.WRITE_FILE));
        // END_CHANGE: ISS-2025-0115

        // START_CHANGE: ISS-2025-0116 - OS/System built-in predicates
        registerFactory("shell", () -> new OsPredicates(OsPredicates.Mode.SHELL));
        registerFactory("shell2", () -> new OsPredicates(OsPredicates.Mode.SHELL2));
        registerFactory("shell_output", () -> new OsPredicates(OsPredicates.Mode.SHELL_OUTPUT));
        registerFactory("getenv", () -> new OsPredicates(OsPredicates.Mode.GETENV));
        registerFactory("hostname", () -> new OsPredicates(OsPredicates.Mode.HOSTNAME));
        registerFactory("pid", () -> new OsPredicates(OsPredicates.Mode.PID));
        registerFactory("sleep", () -> new OsPredicates(OsPredicates.Mode.SLEEP));
        registerFactory("system_time", () -> new OsPredicates(OsPredicates.Mode.SYSTEM_TIME));
        registerFactory("os_name", () -> new OsPredicates(OsPredicates.Mode.OS_NAME));
        registerFactory("cpu_count", () -> new OsPredicates(OsPredicates.Mode.CPU_COUNT));
        registerFactory("free_memory", () -> new OsPredicates(OsPredicates.Mode.FREE_MEMORY));
        registerFactory("total_memory", () -> new OsPredicates(OsPredicates.Mode.TOTAL_MEMORY));
        // END_CHANGE: ISS-2025-0116

        // START_CHANGE: ISS-2025-0117 - Regex built-in predicates
        registerFactory("re_match", () -> new RegexPredicates(RegexPredicates.Mode.RE_MATCH));
        registerFactory("re_matchsub", () -> new RegexPredicates(RegexPredicates.Mode.RE_MATCHSUB));
        registerFactory("re_replace", () -> new RegexPredicates(RegexPredicates.Mode.RE_REPLACE));
        registerFactory("re_split", () -> new RegexPredicates(RegexPredicates.Mode.RE_SPLIT));
        registerFactory("re_findall", () -> new RegexPredicates(RegexPredicates.Mode.RE_FINDALL));
        // START_CHANGE: ISS-2025-0174 - Register re_escape/2 predicate
        registerFactory("re_escape", () -> new RegexPredicates(RegexPredicates.Mode.RE_ESCAPE));
        // END_CHANGE: ISS-2025-0174
        // END_CHANGE: ISS-2025-0117

        // START_CHANGE: ISS-2025-0118 - XML built-in predicates
        registerFactory("xml_parse", () -> new XmlPredicates(XmlPredicates.Mode.XML_PARSE));
        registerFactory("xml_serialize", () -> new XmlPredicates(XmlPredicates.Mode.XML_SERIALIZE));
        registerFactory("xpath", () -> new XmlPredicates(XmlPredicates.Mode.XPATH));
        // END_CHANGE: ISS-2025-0118

        // START_CHANGE: ISS-2025-0119 - Threading built-in predicates
        registerFactory("thread_create", () -> new ThreadPredicates(ThreadPredicates.Mode.THREAD_CREATE));
        registerFactory("thread_join", () -> new ThreadPredicates(ThreadPredicates.Mode.THREAD_JOIN));
        registerFactory("thread_detach", () -> new ThreadPredicates(ThreadPredicates.Mode.THREAD_DETACH));
        registerFactory("thread_self", () -> new ThreadPredicates(ThreadPredicates.Mode.THREAD_SELF));
        registerFactory("thread_sleep", () -> new ThreadPredicates(ThreadPredicates.Mode.THREAD_SLEEP));
        registerFactory("thread_is_alive", () -> new ThreadPredicates(ThreadPredicates.Mode.THREAD_IS_ALIVE));
        registerFactory("message_queue_create", () -> new ThreadPredicates(ThreadPredicates.Mode.MQ_CREATE));
        registerFactory("thread_send_message", () -> new ThreadPredicates(ThreadPredicates.Mode.MQ_SEND));
        registerFactory("thread_get_message", () -> new ThreadPredicates(ThreadPredicates.Mode.MQ_GET));
        registerFactory("thread_peek_message", () -> new ThreadPredicates(ThreadPredicates.Mode.MQ_PEEK));
        // START_CHANGE: ISS-2025-0630..0632 - wave P6.5: the rest of SWI's thread / queue / mutex API
        registerFactory("thread_property", () -> new ThreadPredicates(ThreadPredicates.Mode.THREAD_PROPERTY));
        registerFactory("thread_exit", () -> new ThreadPredicates(ThreadPredicates.Mode.THREAD_EXIT));
        registerFactory("message_queue_destroy", () -> new ThreadPredicates(ThreadPredicates.Mode.MQ_DESTROY));
        registerFactory("mutex_create", () -> new ThreadPredicates(ThreadPredicates.Mode.MUTEX_CREATE));
        registerFactory("mutex_destroy", () -> new ThreadPredicates(ThreadPredicates.Mode.MUTEX_DESTROY));
        registerFactory("mutex_lock", () -> new ThreadPredicates(ThreadPredicates.Mode.MUTEX_LOCK));
        registerFactory("mutex_trylock", () -> new ThreadPredicates(ThreadPredicates.Mode.MUTEX_TRYLOCK));
        registerFactory("mutex_unlock", () -> new ThreadPredicates(ThreadPredicates.Mode.MUTEX_UNLOCK));
        registerFactory("mutex_unlock_all", () -> new ThreadPredicates(ThreadPredicates.Mode.MUTEX_UNLOCK_ALL));
        registerFactory("with_mutex", () -> new ThreadPredicates(ThreadPredicates.Mode.WITH_MUTEX));
        registerFactory("concurrent_forall", () -> new ConcurrentPredicates(ConcurrentPredicates.OperationType.CONCURRENT_FORALL));
        // END_CHANGE: ISS-2025-0630..0632
        // END_CHANGE: ISS-2025-0119

        // START_CHANGE: ISS-2025-0139 - SWI-Prolog compatible concurrent execution predicates
        registerFactory("concurrent", () -> new ConcurrentPredicates(ConcurrentPredicates.OperationType.CONCURRENT));
        // START_CHANGE: ISS-2025-0480 - the registry is keyed by NAME, so concurrent_maplist/2,3,4
        // must be ONE entry that dispatches on the goal's arity. The former `concurrent_maplist3` /
        // `concurrent_maplist4` names were not callable from Prolog at all.
        registerFactory("concurrent_maplist", () -> new ConcurrentPredicates(ConcurrentPredicates.OperationType.CONCURRENT_MAPLIST_2));
        // END_CHANGE: ISS-2025-0480
        registerFactory("first_solution", () -> new ConcurrentPredicates(ConcurrentPredicates.OperationType.FIRST_SOLUTION));
        registerFactory("concurrent_and", () -> new ConcurrentPredicates(ConcurrentPredicates.OperationType.CONCURRENT_AND));
        registerFactory("concurrent_or", () -> new ConcurrentPredicates(ConcurrentPredicates.OperationType.CONCURRENT_OR));
        // END_CHANGE: ISS-2025-0139

        // Removed ISS-2025-0140 through ISS-2025-0159 (31 toy/academic packages)

        // START_CHANGE: ISS-2025-0120 - CSV built-in predicates
        registerFactory("csv_read_file", () -> new CsvPredicates(CsvPredicates.Mode.CSV_READ_FILE));
        registerFactory("csv_write_file", () -> new CsvPredicates(CsvPredicates.Mode.CSV_WRITE_FILE));
        registerFactory("csv_parse", () -> new CsvPredicates(CsvPredicates.Mode.CSV_PARSE));
        registerFactory("csv_serialize", () -> new CsvPredicates(CsvPredicates.Mode.CSV_SERIALIZE));
        // END_CHANGE: ISS-2025-0120

        // START_CHANGE: ISS-2025-0121 - Logging built-in predicates
        registerFactory("log_info", () -> new LoggingPredicates(LoggingPredicates.Mode.LOG_INFO));
        registerFactory("log_warning", () -> new LoggingPredicates(LoggingPredicates.Mode.LOG_WARNING));
        registerFactory("log_error", () -> new LoggingPredicates(LoggingPredicates.Mode.LOG_ERROR));
        registerFactory("log_debug", () -> new LoggingPredicates(LoggingPredicates.Mode.LOG_DEBUG));
        registerFactory("log_level", () -> new LoggingPredicates(LoggingPredicates.Mode.LOG_LEVEL));
        registerFactory("log_to_file", () -> new LoggingPredicates(LoggingPredicates.Mode.LOG_TO_FILE));
        // END_CHANGE: ISS-2025-0121

        // START_CHANGE: ISS-2025-0125 - HTTP server/client built-in predicates
        registerFactory("http_server", () -> new HttpServerPredicates(HttpServerPredicates.Mode.HTTP_SERVER));
        registerFactory("http_stop", () -> new HttpServerPredicates(HttpServerPredicates.Mode.HTTP_STOP));
        registerFactory("http_handler", () -> new HttpServerPredicates(HttpServerPredicates.Mode.HTTP_HANDLER));
        registerFactory("http_get_request", () -> new HttpServerPredicates(HttpServerPredicates.Mode.HTTP_GET_REQUEST));
        registerFactory("http_reply", () -> new HttpServerPredicates(HttpServerPredicates.Mode.HTTP_REPLY));
        registerFactory("http_reply_json", () -> new HttpServerPredicates(HttpServerPredicates.Mode.HTTP_REPLY_JSON));
        registerFactory("http_client_get", () -> new HttpServerPredicates(HttpServerPredicates.Mode.HTTP_CLIENT_GET));
        registerFactory("http_client_post", () -> new HttpServerPredicates(HttpServerPredicates.Mode.HTTP_CLIENT_POST));
        registerFactory("http_open", () -> new HttpServerPredicates(HttpServerPredicates.Mode.HTTP_OPEN));
        registerFactory("url_encode", () -> new HttpServerPredicates(HttpServerPredicates.Mode.URL_ENCODE));
        registerFactory("url_decode", () -> new HttpServerPredicates(HttpServerPredicates.Mode.URL_DECODE));
        // END_CHANGE: ISS-2025-0125

        // START_CHANGE: ISS-2025-0123 - CLP(FD) Constraint Logic Programming over Finite Domains
        registerFactory("in", () -> new ClpfdPredicates(null, ClpfdPredicates.OperationType.IN));
        registerFactory("#=", () -> new ClpfdPredicates(null, ClpfdPredicates.OperationType.HASH_EQ));
        registerFactory("#\\=", () -> new ClpfdPredicates(null, ClpfdPredicates.OperationType.HASH_NEQ));
        registerFactory("#<", () -> new ClpfdPredicates(null, ClpfdPredicates.OperationType.HASH_LT));
        registerFactory("#>", () -> new ClpfdPredicates(null, ClpfdPredicates.OperationType.HASH_GT));
        registerFactory("#=<", () -> new ClpfdPredicates(null, ClpfdPredicates.OperationType.HASH_LEQ));
        registerFactory("#>=", () -> new ClpfdPredicates(null, ClpfdPredicates.OperationType.HASH_GEQ));
        registerFactory("all_different", () -> new ClpfdPredicates(null, ClpfdPredicates.OperationType.ALL_DIFFERENT));
        registerFactory("label", () -> new ClpfdPredicates(null, ClpfdPredicates.OperationType.LABEL));
        registerFactory("labeling", () -> new ClpfdPredicates(null, ClpfdPredicates.OperationType.LABELING));
        registerFactory("indomain", () -> new ClpfdPredicates(null, ClpfdPredicates.OperationType.INDOMAIN));
        registerFactory("fd_dom", () -> new ClpfdPredicates(null, ClpfdPredicates.OperationType.FD_DOM));
        registerFactory("fd_size", () -> new ClpfdPredicates(null, ClpfdPredicates.OperationType.FD_SIZE));
        // END_CHANGE: ISS-2025-0123

        // START_CHANGE: ISS-2025-0127 - Graph algorithm predicates
        registerFactory("graph_path", () -> new GraphPredicates(GraphPredicates.OperationType.GRAPH_PATH));
        registerFactory("shortest_path", () -> new GraphPredicates(GraphPredicates.OperationType.SHORTEST_PATH));
        registerFactory("graph_connected", () -> new GraphPredicates(GraphPredicates.OperationType.GRAPH_CONNECTED));
        registerFactory("graph_vertices", () -> new GraphPredicates(GraphPredicates.OperationType.GRAPH_VERTICES));
        registerFactory("graph_edges", () -> new GraphPredicates(GraphPredicates.OperationType.GRAPH_EDGES));
        registerFactory("graph_neighbors", () -> new GraphPredicates(GraphPredicates.OperationType.GRAPH_NEIGHBORS));
        registerFactory("topological_sort", () -> new GraphPredicates(GraphPredicates.OperationType.TOPOLOGICAL_SORT));
        registerFactory("graph_components", () -> new GraphPredicates(GraphPredicates.OperationType.GRAPH_COMPONENTS));
        registerFactory("minimum_spanning_tree", () -> new GraphPredicates(GraphPredicates.OperationType.MINIMUM_SPANNING_TREE));
        registerFactory("graph_degree", () -> new GraphPredicates(GraphPredicates.OperationType.GRAPH_DEGREE));
        registerFactory("graph_has_cycle", () -> new GraphPredicates(GraphPredicates.OperationType.GRAPH_HAS_CYCLE));
        registerFactory("graph_reachable", () -> new GraphPredicates(GraphPredicates.OperationType.GRAPH_REACHABLE));
        // START_CHANGE: ISS-2025-0176 - Graph SCC (Tarjan's algorithm)
        registerFactory("graph_scc", () -> new GraphPredicates(GraphPredicates.OperationType.GRAPH_SCC));
        // END_CHANGE: ISS-2025-0176
        // END_CHANGE: ISS-2025-0127

        // START_CHANGE: ISS-2025-0126 - Persistence built-in predicates
        registerFactory("db_save", () -> new PersistencePredicates(PersistencePredicates.OperationType.DB_SAVE));
        registerFactory("db_load", () -> new PersistencePredicates(PersistencePredicates.OperationType.DB_LOAD));
        registerFactory("db_save_predicate", () -> new PersistencePredicates(PersistencePredicates.OperationType.DB_SAVE_PREDICATE));
        registerFactory("persist", () -> new PersistencePredicates(PersistencePredicates.OperationType.PERSIST));
        registerFactory("unpersist", () -> new PersistencePredicates(PersistencePredicates.OperationType.UNPERSIST));
        registerFactory("db_export_json", () -> new PersistencePredicates(PersistencePredicates.OperationType.DB_EXPORT_JSON));
        registerFactory("db_import_json", () -> new PersistencePredicates(PersistencePredicates.OperationType.DB_IMPORT_JSON));
        registerFactory("db_snapshot", () -> new PersistencePredicates(PersistencePredicates.OperationType.DB_SNAPSHOT));
        registerFactory("db_restore", () -> new PersistencePredicates(PersistencePredicates.OperationType.DB_RESTORE));
        registerFactory("db_clear", () -> new PersistencePredicates(PersistencePredicates.OperationType.DB_CLEAR));
        // END_CHANGE: ISS-2025-0126
        // START_CHANGE: ISS-2025-0175 - Transaction and batch persistence predicates
        registerFactory("db_transaction", () -> new PersistencePredicates(PersistencePredicates.OperationType.DB_TRANSACTION));
        registerFactory("db_sync", () -> new PersistencePredicates(PersistencePredicates.OperationType.DB_SYNC));
        registerFactory("db_batch_assert", () -> new PersistencePredicates(PersistencePredicates.OperationType.DB_BATCH_ASSERT));
        // END_CHANGE: ISS-2025-0175

        // START_CHANGE: ISS-2025-0160 - Java FFI built-in predicates
        registerFactory("java_new", () -> new JavaFFI(JavaFFI.OperationType.JAVA_NEW));
        registerFactory("java_call", () -> new JavaFFI(JavaFFI.OperationType.JAVA_CALL));
        registerFactory("java_get_field", () -> new JavaFFI(JavaFFI.OperationType.JAVA_GET_FIELD));
        registerFactory("java_set_field", () -> new JavaFFI(JavaFFI.OperationType.JAVA_SET_FIELD));
        registerFactory("java_instanceof", () -> new JavaFFI(JavaFFI.OperationType.JAVA_INSTANCEOF));
        registerFactory("java_class", () -> new JavaFFI(JavaFFI.OperationType.JAVA_CLASS));
        registerFactory("java_array_new", () -> new JavaFFI(JavaFFI.OperationType.JAVA_ARRAY_NEW));
        registerFactory("java_array_get", () -> new JavaFFI(JavaFFI.OperationType.JAVA_ARRAY_GET));
        registerFactory("java_array_set", () -> new JavaFFI(JavaFFI.OperationType.JAVA_ARRAY_SET));
        registerFactory("java_array_length", () -> new JavaFFI(JavaFFI.OperationType.JAVA_ARRAY_LENGTH));
        registerFactory("java_to_term", () -> new JavaFFI(JavaFFI.OperationType.JAVA_TO_TERM));
        registerFactory("java_from_term", () -> new JavaFFI(JavaFFI.OperationType.JAVA_FROM_TERM));
        // END_CHANGE: ISS-2025-0160
        // START_CHANGE: ISS-2025-0173 - Register java_release_ref/1 and java_gc/0 for FFI memory management
        registerFactory("java_release_ref", () -> new JavaFFI(JavaFFI.OperationType.JAVA_RELEASE_REF));
        registerFactory("java_gc", () -> new JavaFFI(JavaFFI.OperationType.JAVA_GC));
        // END_CHANGE: ISS-2025-0173

        // START_CHANGE: LIM-003 - Global variable predicates
        registerFactory("nb_setval", () -> new it.denzosoft.jprolog.builtin.system.GlobalVariables(
            it.denzosoft.jprolog.builtin.system.GlobalVariables.Mode.NB_SETVAL));
        registerFactory("nb_getval", () -> new it.denzosoft.jprolog.builtin.system.GlobalVariables(
            it.denzosoft.jprolog.builtin.system.GlobalVariables.Mode.NB_GETVAL));
        registerFactory("nb_current", () -> new it.denzosoft.jprolog.builtin.system.GlobalVariables(
            it.denzosoft.jprolog.builtin.system.GlobalVariables.Mode.NB_CURRENT));
        registerFactory("nb_delete", () -> new it.denzosoft.jprolog.builtin.system.GlobalVariables(
            it.denzosoft.jprolog.builtin.system.GlobalVariables.Mode.NB_DELETE));
        registerFactory("b_setval", () -> new it.denzosoft.jprolog.builtin.system.GlobalVariables(
            it.denzosoft.jprolog.builtin.system.GlobalVariables.Mode.B_SETVAL));
        registerFactory("b_getval", () -> new it.denzosoft.jprolog.builtin.system.GlobalVariables(
            it.denzosoft.jprolog.builtin.system.GlobalVariables.Mode.B_GETVAL));
        // END_CHANGE: LIM-003

        // START_CHANGE: ISS-2025-0491 - 4.1 wave A: the LEGACY coroutining and attributed-variable
        // built-ins are DELETED (builtin.control.Freeze/When/Dif, builtin.term.AttributedVariables).
        // They existed for the v2 engine, which fired them from Variable.AttributeUnifyHook inside
        // Term.unify(Term, Map). On v4 `put_attr/3`, `get_attr/3`, `del_attr/2`, `attvar/1` and
        // `term_attvars/2` are natives (core.engine.v4.Coroutining) and `freeze/2`, `frozen/2`,
        // `when/2`, `dif/2` and `?=/2` are prelude clauses (prelude/coroutining.pl) over the
        // attr_unify_hook protocol — neither the registry entry nor the Java class was reachable.
        // END_CHANGE: ISS-2025-0491

        // START_CHANGE: LIM-012 - Rational number predicate
        registerFactory("rational", () -> (it.denzosoft.jprolog.core.engine.BuiltIn) (query, bindings, solutions) -> {
            it.denzosoft.jprolog.core.terms.Term arg = query.getArguments().get(0).resolveBindings(bindings);
            if (arg instanceof it.denzosoft.jprolog.core.terms.Rational) {
                solutions.add(new java.util.HashMap<>(bindings));
                return true;
            }
            return false;
        });
        // END_CHANGE: LIM-012

        // START_CHANGE: LIM-016 - Atom garbage collection predicates
        registerFactory("atom_gc", () -> (it.denzosoft.jprolog.core.engine.BuiltIn) (query, bindings, solutions) -> {
            it.denzosoft.jprolog.core.terms.AtomTable.gc();
            solutions.add(new java.util.HashMap<>(bindings));
            return true;
        });
        registerFactory("atom_table_size", () -> (it.denzosoft.jprolog.core.engine.BuiltIn) (query, bindings, solutions) -> {
            it.denzosoft.jprolog.core.terms.Term arg = query.getArguments().get(0).resolveBindings(bindings);
            it.denzosoft.jprolog.core.terms.Number size =
                new it.denzosoft.jprolog.core.terms.Number((long) it.denzosoft.jprolog.core.terms.AtomTable.size());
            if (arg instanceof it.denzosoft.jprolog.core.terms.Variable) {
                java.util.Map<String, it.denzosoft.jprolog.core.terms.Term> newBindings = new java.util.HashMap<>(bindings);
                newBindings.put(((it.denzosoft.jprolog.core.terms.Variable) arg).getName(), size);
                solutions.add(newBindings);
                return true;
            }
            return arg.unify(size, bindings);
        });
        // END_CHANGE: LIM-016

    }
    
    private static void registerFactory(String name, Supplier<BuiltIn> factory) {
        FACTORY_MAP.put(name, factory);
    }
    
    /**
     * Create a built-in predicate by name.
     * 
     * @param name The predicate name
     * @return The built-in implementation or null if not found
     */
    public static BuiltIn createBuiltIn(String name) {
        Supplier<BuiltIn> factory = FACTORY_MAP.get(name);
        return factory != null ? factory.get() : null;
    }
    
    /**
     * Check if a built-in predicate exists.
     * 
     * @param name The predicate name
     * @return true if the built-in exists
     */
    public static boolean hasBuiltIn(String name) {
        return FACTORY_MAP.containsKey(name);
    }
    
    // START_CHANGE: ISS-2025-0091 - Return unmodifiable view instead of defensive copy
    static Map<String, Supplier<BuiltIn>> getFactoryMap() {
        return java.util.Collections.unmodifiableMap(FACTORY_MAP);
    }
    // END_CHANGE: ISS-2025-0091
}
