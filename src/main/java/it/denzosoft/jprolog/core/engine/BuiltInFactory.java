package it.denzosoft.jprolog.core.engine;

import it.denzosoft.jprolog.builtin.*;
import it.denzosoft.jprolog.builtin.control.NegationAsFailure;
import it.denzosoft.jprolog.builtin.control.Unify;
import it.denzosoft.jprolog.builtin.control.NotUnify;
import it.denzosoft.jprolog.builtin.control.UnifyWithOccursCheck;
import it.denzosoft.jprolog.builtin.control.Is;
import it.denzosoft.jprolog.builtin.control.Cut;
import it.denzosoft.jprolog.builtin.control.Repeat;
import it.denzosoft.jprolog.builtin.control.IfThen;
import it.denzosoft.jprolog.builtin.control.IfThenElse;
import it.denzosoft.jprolog.builtin.control.Conjunction;
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
        registerFactory("between", it.denzosoft.jprolog.builtin.arithmetic.Between::new);
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
        
        // Control
        registerFactory("!", Cut::new);
        registerFactory("cut", Cut::new);
        registerFactory("repeat", Repeat::new);
        
        // Negation as failure
        registerFactory("\\+", () -> new NegationAsFailure(null)); // QuerySolver will be injected
        
        // Logical operators (context-dependent, need QuerySolver injection)
        registerFactory("->", () -> new IfThen(null));
        registerFactory(";", () -> new IfThenElse(null));
        registerFactory(",", () -> new Conjunction(null));
        
        // I/O
        registerFactory("write", Write::new);
        registerFactory("writeln", Writeln::new);
        registerFactory("nl", Nl::new);
        registerFactory("read", Read::new);
        
        // Character I/O (ISO Prolog)
        registerFactory("get_char", GetChar::new);
        registerFactory("put_char", PutChar::new);
        registerFactory("get_code", GetCode::new);
        registerFactory("put_code", PutCode::new);
        
        // Stream I/O (ISO Prolog)
        registerFactory("open", Open::new);
        registerFactory("close", Close::new);
        registerFactory("current_input", CurrentInput::new);
        registerFactory("current_output", CurrentOutput::new);
        registerFactory("set_input", SetInput::new);
        registerFactory("set_output", SetOutput::new);
        
        // Database
        registerFactory("listing", Listing0::new); // listing/0 - works with QuerySolver context
        
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
        // END_CHANGE: ISS-2025-0009
        
        // String operations
        registerFactory("string_length", StringLength::new);
        registerFactory("string_concat", StringConcat::new);
        registerFactory("sub_string", SubString::new);
        registerFactory("string_chars", StringChars::new);
        registerFactory("atom_string", AtomString::new);
        registerFactory("number_string", NumberString::new);
        
        // Collection predicates (context-dependent)
        registerFactory("findall", () -> new Findall(null)); // QuerySolver will be injected
        registerFactory("bagof", () -> new Bagof(null)); // QuerySolver will be injected
        registerFactory("setof", () -> new Setof(null)); // QuerySolver will be injected
        
        // Exception handling (ISO Prolog)
        registerFactory("catch", () -> new Catch(null)); // QuerySolver will be injected
        registerFactory("throw", Throw::new);
        registerFactory("halt", Halt::new);
        
        // Meta-predicates (ISO Prolog)
        registerFactory("call", () -> new Call(null)); // QuerySolver will be injected
        registerFactory("once", () -> new Once(null)); // QuerySolver will be injected
        registerFactory("ignore", () -> new Ignore(null)); // QuerySolver will be injected
        registerFactory("forall", () -> new ForAll(null)); // QuerySolver will be injected
        
        // Dynamic database operations (ISO Prolog)
        // START_CHANGE: ISS-2025-0023 - Add assert/1 as alias for assertz/1
        registerFactory("assert", () -> new Assertz(null)); // assert/1 is alias for assertz/1
        // END_CHANGE: ISS-2025-0023
        registerFactory("asserta", () -> new Asserta(null)); // QuerySolver will be injected
        registerFactory("assertz", () -> new Assertz(null)); // QuerySolver will be injected
        registerFactory("retract", () -> new Retract(null)); // QuerySolver will be injected
        registerFactory("retractall", () -> new Retractall(null)); // QuerySolver will be injected
        registerFactory("abolish", () -> new Abolish(null)); // QuerySolver will be injected
        registerFactory("current_predicate", () -> new CurrentPredicate(null)); // QuerySolver will be injected
        
        // Debugging predicates (ISO Prolog)
        registerFactory("trace", Trace::new);
        registerFactory("notrace", NoTrace::new);
        registerFactory("spy", Spy::new);
        registerFactory("nospy", NoSpy::new);
        
        // System predicates (ISO Prolog)
        registerFactory("current_prolog_flag", CurrentPrologFlag::new);
        registerFactory("set_prolog_flag", SetPrologFlag::new);
        registerFactory("op", () -> new Op(null)); // QuerySolver will be injected
        
        // Advanced I/O predicates (ISO Prolog)
        registerFactory("read_term", () -> new ReadTerm(null)); // QuerySolver will be injected
        registerFactory("write_term", () -> new WriteTerm(null)); // QuerySolver will be injected
        registerFactory("format", () -> new Format(null)); // QuerySolver will be injected
        
        // Character predicates (ISO Prolog)
        registerFactory("char_type", () -> new CharType(null)); // QuerySolver will be injected
        registerFactory("char_code", () -> new CharCode(null)); // QuerySolver will be injected
        
        // DCG predicates (ISO Prolog)
        registerFactory("phrase", () -> new Phrase(null)); // QuerySolver will be injected
        
        // Additional system predicates
        registerFactory("statistics", () -> new Statistics(null)); // QuerySolver will be injected
        registerFactory("current_op", () -> new Op(null)); // Uses same implementation as op/3
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
    
    // Add a getter method for package access
    static Map<String, Supplier<BuiltIn>> getFactoryMap() {
        return new HashMap<>(FACTORY_MAP);
    }
}
