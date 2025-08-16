package it.denzosoft.jprolog;

import it.denzosoft.jprolog.builtin.*;
import it.denzosoft.jprolog.builtin.arithmetic.ArithmeticComparison;
import it.denzosoft.jprolog.builtin.term.TermComparison;
import it.denzosoft.jprolog.builtin.term.TermConstruction;
import it.denzosoft.jprolog.builtin.type.*;
import it.denzosoft.jprolog.builtin.atom.*;
import it.denzosoft.jprolog.builtin.database.*;
import it.denzosoft.jprolog.builtin.io.*;
import it.denzosoft.jprolog.terms.Term;
import it.denzosoft.jprolog.terms.Atom;
import it.denzosoft.jprolog.terms.Variable;
import it.denzosoft.jprolog.terms.CompoundTerm;

import java.util.*;
import java.util.logging.Logger;


public class Prolog {
    private static final Logger LOGGER = Logger.getLogger(Prolog.class.getName());
    
    private final KnowledgeBase knowledgeBase;
    private final BuiltInRegistry builtInRegistry;
    private final QuerySolver querySolver;
    private final Parser parser;
    private boolean traceEnabled = false;

    /**
     * Create a new Prolog engine with default components.
     */
    public Prolog() {
        this.knowledgeBase = new KnowledgeBase();
        this.builtInRegistry = new BuiltInRegistry();
        this.querySolver = new QuerySolver(knowledgeBase, builtInRegistry);
        this.querySolver.setPrologContext(this);
        this.parser = new Parser();
        registerBuiltInPredicates();
    }

    private void registerBuiltInPredicates() {
        // Unification
        builtInRegistry.registerBuiltIn("=", new Unify());
        builtInRegistry.registerBuiltIn("unify_with_occurs_check", new UnifyWithOccursCheck());
        
        // Type checking
        builtInRegistry.registerBuiltIn("var", new VarCheck());
        builtInRegistry.registerBuiltIn("nonvar", new NonVarCheck());
        builtInRegistry.registerBuiltIn("atom", new AtomCheck());
        builtInRegistry.registerBuiltIn("integer", new IntegerCheck());
        builtInRegistry.registerBuiltIn("float", new FloatCheck());
        builtInRegistry.registerBuiltIn("atomic", new AtomicCheck());
        builtInRegistry.registerBuiltIn("compound", new CompoundCheck());
        builtInRegistry.registerBuiltIn("number", new NumberCheck());
        
        // Term comparison
        builtInRegistry.registerBuiltIn("@=<", new TermComparison(TermComparison.ComparisonType.AT_LESS_EQUAL));
        builtInRegistry.registerBuiltIn("@<", new TermComparison(TermComparison.ComparisonType.AT_LESS));
        builtInRegistry.registerBuiltIn("@>", new TermComparison(TermComparison.ComparisonType.AT_GREATER));
        builtInRegistry.registerBuiltIn("@>=", new TermComparison(TermComparison.ComparisonType.AT_GREATER_EQUAL));
        builtInRegistry.registerBuiltIn("==", new TermComparison(TermComparison.ComparisonType.TERM_EQUAL));
        builtInRegistry.registerBuiltIn("\\==", new TermComparison(TermComparison.ComparisonType.TERM_NOT_EQUAL));
        
        // Term construction
        builtInRegistry.registerBuiltIn("functor", new TermConstruction(TermConstruction.OperationType.FUNCTOR));
        builtInRegistry.registerBuiltIn("arg", new TermConstruction(TermConstruction.OperationType.ARG));
        builtInRegistry.registerBuiltIn("=..", new TermConstruction(TermConstruction.OperationType.UNIV));
        builtInRegistry.registerBuiltIn("copy_term", new TermConstruction(TermConstruction.OperationType.COPY_TERM));
        
        // Arithmetic evaluation
        builtInRegistry.registerBuiltIn("is", new Is());
        
        // Arithmetic comparison
        builtInRegistry.registerBuiltIn("=:=", new ArithmeticComparison(ArithmeticComparison.ComparisonType.EQUAL));
        builtInRegistry.registerBuiltIn("=\\=", new ArithmeticComparison(ArithmeticComparison.ComparisonType.NOT_EQUAL));
        builtInRegistry.registerBuiltIn("<", new ArithmeticComparison(ArithmeticComparison.ComparisonType.LESS));
        builtInRegistry.registerBuiltIn("=<", new ArithmeticComparison(ArithmeticComparison.ComparisonType.LESS_EQUAL));
        builtInRegistry.registerBuiltIn(">", new ArithmeticComparison(ArithmeticComparison.ComparisonType.GREATER));
        builtInRegistry.registerBuiltIn(">=", new ArithmeticComparison(ArithmeticComparison.ComparisonType.GREATER_EQUAL));
        
        // List operations
        builtInRegistry.registerBuiltIn("append", new Append());
        builtInRegistry.registerBuiltIn("length", new Length());
        builtInRegistry.registerBuiltIn("member", new Member());
        builtInRegistry.registerBuiltIn("nth0", new Nth0());
        builtInRegistry.registerBuiltIn("nth1", new Nth1());
        builtInRegistry.registerBuiltIn("msort", new Msort());
        builtInRegistry.registerBuiltIn("reverse", new Reverse());
        builtInRegistry.registerBuiltIn("select", new Select());
        builtInRegistry.registerBuiltIn("sort", new Sort());
        
        // Collection predicates with context
        builtInRegistry.registerBuiltIn("findall", new Findall(querySolver));
        builtInRegistry.registerBuiltIn("bagof", new Bagof(querySolver));
        builtInRegistry.registerBuiltIn("setof", new Setof(querySolver));
        
        // Control
        builtInRegistry.registerBuiltIn("!", new Cut());
        builtInRegistry.registerBuiltIn("cut", new Cut());
        builtInRegistry.registerBuiltIn("repeat", new Repeat());
        
        // I/O
        builtInRegistry.registerBuiltIn("write", new Write());
        builtInRegistry.registerBuiltIn("writeln", new Writeln());
        builtInRegistry.registerBuiltIn("nl", new Nl());
        builtInRegistry.registerBuiltIn("read", new Read());
        
        // Database
        builtInRegistry.registerBuiltIn("listing", new Listing0());
        builtInRegistry.registerBuiltIn("listing", new Listing1());
        
        // Atom operations
        builtInRegistry.registerBuiltIn("atom_length", new AtomLength());
        builtInRegistry.registerBuiltIn("atom_concat", new AtomConcat());
    }

    /**
     * Load a Prolog program into the knowledge base.
     * 
     * @param programString The program to load
     */
    public void consult(String programString) {
        try {
            List<Rule> rules = parser.parse(programString);
            knowledgeBase.addRules(rules);
        } catch (PrologParserException e) {
            throw new PrologException("Error parsing program: " + e.getMessage(), e);
        }
    }

    /**
     * Add a rule at the beginning of the knowledge base.
     * 
     * @param clauseString The clause to add
     */
    public void asserta(String clauseString) {
        try {
            List<Rule> rules = parser.parse(clauseString);
            if (!rules.isEmpty()) {
                knowledgeBase.asserta(rules.get(0));
            }
        } catch (PrologParserException e) {
            throw new PrologException("Error parsing clause: " + e.getMessage(), e);
        }
    }

    /**
     * Remove a rule from the knowledge base.
     * 
     * @param clauseString The clause to remove
     */
    public void retract(String clauseString) {
        try {
            List<Rule> rules = parser.parse(clauseString);
            if (!rules.isEmpty()) {
                knowledgeBase.retract(rules.get(0));
            }
        } catch (PrologParserException e) {
            throw new PrologException("Error parsing clause: " + e.getMessage(), e);
        }
    }

    /**
     * Register a custom built-in predicate.
     * 
     * @param name The predicate name
     * @param predicate The implementation
     */
    public void registerBuiltInPredicate(String name, BuiltIn predicate) {
        builtInRegistry.registerBuiltIn(name, predicate);
    }

    /**
     * Solve a query.
     * 
     * @param query The query term
     * @return List of solutions
     */
    public List<Map<String, Term>> solve(Term query) {
        return querySolver.solve(query);
    }

    /**
     * Solve a query string.
     * 
     * @param queryString The query string
     * @return List of solutions
     */
    public List<Map<String, Term>> solve(String queryString) {
        try {
            // Remove the trailing period from the query string
            if (queryString != null && queryString.endsWith(".")) {
                queryString = queryString.substring(0, queryString.length() - 1);
            }
            Term query = parser.parseTerm(queryString);
            return querySolver.solve(query);
        } catch (PrologParserException e) {
            throw new PrologException("Error parsing query: " + e.getMessage(), e);
        }
    }

    /**
     * Enable or disable tracing.
     * 
     * @param traceEnabled true to enable tracing
     */
    public void setTraceEnabled(boolean traceEnabled) {
        this.traceEnabled = traceEnabled;
        querySolver.setTraceEnabled(traceEnabled);
    }

    /**
     * Get all rules in the knowledge base.
     * 
     * @return List of rules
     */
    public List<Rule> getRules() {
        return knowledgeBase.getRules();
    }

    /**
     * List all rules in the knowledge base.
     */
    public void listing() {
        System.out.println(knowledgeBase.toString());
    }

    /**
     * List rules for a specific predicate.
     * 
     * @param predicateIndicator The predicate indicator
     */
    public void listing(String predicateIndicator) {
        System.out.println("Listing for: " + predicateIndicator);
        for (Rule rule : knowledgeBase.getRules()) {
            if (rule.getHead().getName().equals(predicateIndicator.split("/")[0])) {
                System.out.println("  " + rule);
            }
        }
    }

    /**
     * Get the term parser.
     * 
     * @return The term parser
     */
    public TermParser getTermParser() {
        return new TermParser(); // Return a new instance to avoid state sharing issues
    }
}
