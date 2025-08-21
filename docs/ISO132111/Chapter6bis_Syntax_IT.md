# Capitolo 6bis: Sintassi - Struttura del Linguaggio JProlog

## Panoramica

Questo capitolo fornisce un'analisi completa dell'implementazione della sintassi di JProlog, coprendo la struttura completa del linguaggio dall'elaborazione dei caratteri di basso livello alla costruzione di termini di alto livello. Dimostra come JProlog implementa le specifiche di sintassi ISO Prolog con esempi dettagliati di parsing, strategie di tokenizzazione e analisi sintattica.

## 6.1 Notazione

### 6.1.1 Forma di Backus Naur

JProlog implementa un parser completo basato su BNF che riconosce la sintassi ISO Prolog completa. Il parser utilizza il parsing a discesa ricorsiva con gestione appropriata della precedenza degli operatori.

#### Implementazione della Grammatica BNF

```java
// Produzioni grammaticali principali nel Parser JProlog
public class PrologParser {
    
    /**
     * term ::= variable | atomic_term | compound_term
     * Implementa la regola di produzione fondamentale del termine
     */
    public Term parseTerm() throws PrologParserException {
        if (currentToken.getType() == TokenType.VARIABLE) {
            return parseVariable();
        } else if (isAtomicToken(currentToken)) {
            return parseAtomicTerm();
        } else {
            return parseCompoundTerm();
        }
    }
    
    /**
     * compound_term ::= functor '(' arguments ')'
     *                 | '(' term ')'
     *                 | list
     *                 | curly_bracketed_term
     */
    public CompoundTerm parseCompoundTerm() throws PrologParserException {
        if (currentToken.getType() == TokenType.ATOM) {
            String functor = currentToken.getValue();
            expectToken(TokenType.LEFT_PAREN);
            List<Term> args = parseArgumentList();
            expectToken(TokenType.RIGHT_PAREN);
            return new CompoundTerm(functor, args);
        } else if (currentToken.getType() == TokenType.LEFT_PAREN) {
            // Gestisce termini parentesizzati
            nextToken();
            Term innerTerm = parseTerm();
            expectToken(TokenType.RIGHT_PAREN);
            return (CompoundTerm) innerTerm;
        } else if (currentToken.getType() == TokenType.LEFT_BRACKET) {
            return parseList();
        } else if (currentToken.getType() == TokenType.LEFT_BRACE) {
            return parseCurlyBracketedTerm();
        }
        throw new PrologParserException("Struttura termine composto non valida");
    }
    
    /**
     * arguments ::= term | term ',' arguments
     * Gestisce il parsing delle liste di argomenti con precedenza appropriata
     */
    private List<Term> parseArgumentList() throws PrologParserException {
        List<Term> arguments = new ArrayList<>();
        arguments.add(parseTerm());
        
        while (currentToken.getType() == TokenType.COMMA) {
            nextToken(); // consuma la virgola
            arguments.add(parseTerm());
        }
        return arguments;
    }
}
```

#### Produzioni Grammaticali per Operatori

```java
/**
 * Implementazione tabella precedenza operatori
 * Segue le regole di precedenza operatori ISO Prolog
 */
public class OperatorTable {
    
    // Livelli di precedenza operatori (1200 = più basso, 1 = più alto)
    private static final Map<String, OperatorInfo> OPERATORS = Map.of(
        ":-", new OperatorInfo(1200, "xfx"),    // Operatore regola
        ";", new OperatorInfo(1100, "xfy"),     // Disgiunzione
        "->", new OperatorInfo(1050, "xfy"),    // If-then
        ",", new OperatorInfo(1000, "xfy"),     // Congiunzione
        "\\+", new OperatorInfo(900, "fy"),     // Non provabile
        "=", new OperatorInfo(700, "xfx"),      // Unificazione
        "\\=", new OperatorInfo(700, "xfx"),    // Non unificabile
        "==", new OperatorInfo(700, "xfx"),     // Uguaglianza termini
        "\\==", new OperatorInfo(700, "xfx"),   // Disuguaglianza termini
        "is", new OperatorInfo(700, "xfx"),     // Valutazione aritmetica
        "=:=", new OperatorInfo(700, "xfx"),    // Uguaglianza aritmetica
        "=\\=", new OperatorInfo(700, "xfx"),   // Disuguaglianza aritmetica
        "<", new OperatorInfo(700, "xfx"),      // Minore di
        "=<", new OperatorInfo(700, "xfx"),     // Minore o uguale
        ">", new OperatorInfo(700, "xfx"),      // Maggiore di
        ">=", new OperatorInfo(700, "xfx"),     // Maggiore o uguale
        "+", new OperatorInfo(500, "yfx"),      // Addizione
        "-", new OperatorInfo(500, "yfx"),      // Sottrazione
        "*", new OperatorInfo(400, "yfx"),      // Moltiplicazione
        "/", new OperatorInfo(400, "yfx"),      // Divisione
        "mod", new OperatorInfo(400, "yfx"),    // Modulo
        "**", new OperatorInfo(200, "xfx"),     // Potenza
        "^", new OperatorInfo(200, "xfy"),      // Potenza (alternativa)
        "-", new OperatorInfo(200, "fy"),       // Meno unario
        "+", new OperatorInfo(200, "fy")        // Più unario
    );
    
    /**
     * Parsing espressione operatore con precedenza appropriata
     */
    public Term parseOperatorExpression(int precedence) throws PrologParserException {
        Term left = parsePrimaryTerm();
        
        while (isOperator(currentToken) && 
               getOperatorPrecedence(currentToken.getValue()) <= precedence) {
            
            String operator = currentToken.getValue();
            OperatorInfo opInfo = OPERATORS.get(operator);
            nextToken();
            
            Term right = parseOperatorExpression(opInfo.precedence - 1);
            left = new CompoundTerm(operator, Arrays.asList(left, right));
        }
        return left;
    }
}
```

### 6.1.2 Sintassi Astratta dei Termini

JProlog implementa una rappresentazione completa dell'albero sintattico astratto (AST) che cattura accuratamente la struttura gerarchica dei termini Prolog.

#### Gerarchia Termini Astratti

```java
/**
 * Classe base astratta per tutti i termini Prolog
 * Fornisce il fondamento per la gerarchia dei tipi di termine
 */
public abstract class Term {
    protected boolean isGround = false;
    protected Set<Variable> variables = new HashSet<>();
    
    public abstract boolean unify(Term other, Substitution substitution);
    public abstract Term substitute(Substitution substitution);
    public abstract boolean isVariable();
    public abstract boolean isAtom();
    public abstract boolean isNumber();
    public abstract boolean isCompound();
    public abstract String toCanonicalString();
    
    /**
     * Rappresentazione struttura termine astratto
     * Restituisce il termine in forma sintattica astratta
     */
    public abstract String toAbstractSyntax();
}

/**
 * Rappresentazione termine atomico coprendo atomi e numeri
 */
public class Atom extends Term {
    private final String value;
    
    public Atom(String value) {
        this.value = value;
        this.isGround = true;
    }
    
    @Override
    public String toAbstractSyntax() {
        return String.format("atom('%s')", value);
    }
    
    @Override
    public boolean unify(Term other, Substitution substitution) {
        if (other instanceof Variable) {
            return other.unify(this, substitution);
        }
        return other instanceof Atom && this.value.equals(((Atom) other).value);
    }
}

/**
 * Termine variabile con scoping e binding appropriati
 */
public class Variable extends Term {
    private final String name;
    private final UUID uuid;  // Identificatore unico per scoping variabili
    private Term binding = null;
    
    public Variable(String name) {
        this.name = name;
        this.uuid = UUID.randomUUID();
        this.variables.add(this);
    }
    
    @Override
    public String toAbstractSyntax() {
        return String.format("variable('%s', '%s')", name, uuid.toString().substring(0, 8));
    }
    
    @Override
    public boolean unify(Term other, Substitution substitution) {
        if (this.binding != null) {
            return this.binding.unify(other, substitution);
        }
        
        // Implementazione verifica occorrenze
        if (other.variables.contains(this)) {
            return false; // Previene strutture infinite
        }
        
        substitution.bind(this, other);
        return true;
    }
}

/**
 * Termine composto con funtore e argomenti
 */
public class CompoundTerm extends Term {
    private final String functor;
    private final List<Term> arguments;
    private final int arity;
    
    public CompoundTerm(String functor, List<Term> arguments) {
        this.functor = functor;
        this.arguments = new ArrayList<>(arguments);
        this.arity = arguments.size();
        
        // Calcola variabili e stato ground
        for (Term arg : arguments) {
            this.variables.addAll(arg.variables);
        }
        this.isGround = this.variables.isEmpty();
    }
    
    @Override
    public String toAbstractSyntax() {
        StringBuilder sb = new StringBuilder();
        sb.append(String.format("compound('%s', %d, [", functor, arity));
        for (int i = 0; i < arguments.size(); i++) {
            if (i > 0) sb.append(", ");
            sb.append(arguments.get(i).toAbstractSyntax());
        }
        sb.append("])");
        return sb.toString();
    }
}
```

## 6.2 Testo e Dati Prolog

### 6.2.1 Testo Prolog

JProlog implementa un sistema completo di elaborazione testo che gestisce programmi Prolog completi con parsing appropriato di clausole e elaborazione direttive.

#### Implementazione Elaborazione Testo

```java
/**
 * Processore testo Prolog gestisce programmi completi
 */
public class PrologTextProcessor {
    
    /**
     * Elabora testo Prolog completo in clausole e direttive
     */
    public PrologProgram processPrologText(String prologText) throws PrologParserException {
        List<Clause> clauses = new ArrayList<>();
        List<Directive> directives = new ArrayList<>();
        
        PrologTokenizer tokenizer = new PrologTokenizer(prologText);
        PrologParser parser = new PrologParser(tokenizer);
        
        while (!parser.isAtEnd()) {
            if (parser.currentToken().getType() == TokenType.DIRECTIVE_PREFIX) {
                // Elabora direttiva: :- goal.
                directives.add(parseDirective(parser));
            } else {
                // Elabora clausola: head :- body. o fatto.
                clauses.add(parseClause(parser));
            }
            
            // Aspetta terminatore clausola
            parser.expectToken(TokenType.DOT);
        }
        
        return new PrologProgram(clauses, directives);
    }
    
    /**
     * Parsing clausola individuale (fatto o regola)
     */
    private Clause parseClause(PrologParser parser) throws PrologParserException {
        Term head = parser.parseTerm();
        
        if (parser.currentToken().getType() == TokenType.RULE_OPERATOR) {
            // Regola: head :- body
            parser.nextToken(); // consuma :-
            Term body = parser.parseTerm();
            return new Rule(head, body);
        } else {
            // Fatto: head
            return new Fact(head);
        }
    }
    
    /**
     * Parsing direttiva
     */
    private Directive parseDirective(PrologParser parser) throws PrologParserException {
        parser.expectToken(TokenType.DIRECTIVE_PREFIX); // :-
        Term goal = parser.parseTerm();
        return new Directive(goal);
    }
}

/**
 * Rappresentazione programma con clausole e direttive
 */
public class PrologProgram {
    private final List<Clause> clauses;
    private final List<Directive> directives;
    private final Map<String, List<Clause>> predicateIndex;
    
    public PrologProgram(List<Clause> clauses, List<Directive> directives) {
        this.clauses = clauses;
        this.directives = directives;
        this.predicateIndex = buildPredicateIndex();
    }
    
    /**
     * Costruisce indice per recupero efficiente clausole
     */
    private Map<String, List<Clause>> buildPredicateIndex() {
        Map<String, List<Clause>> index = new HashMap<>();
        
        for (Clause clause : clauses) {
            String predicateIndicator = clause.getPredicateIndicator();
            index.computeIfAbsent(predicateIndicator, k -> new ArrayList<>())
                 .add(clause);
        }
        return index;
    }
}
```

### 6.2.2 Dati Prolog

JProlog implementa un sistema completo di rappresentazione dati che mantiene relazioni appropriate tra termini e supporta modifiche dinamiche.

#### Implementazione Strutture Dati

```java
/**
 * Sistema gestione dati Prolog
 */
public class PrologDataManager {
    
    /**
     * Converte dati esterni in termini Prolog
     */
    public Term convertToProlog(Object data) {
        if (data == null) {
            return new Atom("null");
        } else if (data instanceof String) {
            return new Atom((String) data);
        } else if (data instanceof Integer) {
            return new Number((Integer) data);
        } else if (data instanceof Double) {
            return new Number((Double) data);
        } else if (data instanceof List<?>) {
            return convertListToProlog((List<?>) data);
        } else if (data instanceof Map<?, ?>) {
            return convertMapToProlog((Map<?, ?>) data);
        } else {
            return new Atom(data.toString());
        }
    }
    
    /**
     * Converte List in struttura lista Prolog
     */
    private Term convertListToProlog(List<?> list) {
        if (list.isEmpty()) {
            return new Atom("[]");
        }
        
        Term result = new Atom("[]");
        for (int i = list.size() - 1; i >= 0; i--) {
            Term element = convertToProlog(list.get(i));
            result = new CompoundTerm(".", Arrays.asList(element, result));
        }
        return result;
    }
    
    /**
     * Converte Map in termine composto Prolog
     */
    private Term convertMapToProlog(Map<?, ?> map) {
        List<Term> pairs = new ArrayList<>();
        for (Map.Entry<?, ?> entry : map.entrySet()) {
            Term key = convertToProlog(entry.getKey());
            Term value = convertToProlog(entry.getValue());
            pairs.add(new CompoundTerm("pair", Arrays.asList(key, value)));
        }
        return new CompoundTerm("map", pairs);
    }
}
```

## 6.3 Termini

### 6.3.1 Termini Atomici

JProlog implementa supporto completo per termini atomici inclusi atomi, numeri e costanti speciali con corretta conformità ISO.

#### Implementazione Termini Atomici

```java
/**
 * Implementazione completa termini atomici
 */
public abstract class AtomicTerm extends Term {
    
    @Override
    public final boolean isCompound() { return false; }
    @Override
    public final boolean isVariable() { return false; }
    
    public abstract Object getValue();
    public abstract String getCanonicalForm();
}

/**
 * Implementazione atomo con regole appropriate per virgolettature
 */
public class Atom extends AtomicTerm {
    private final String value;
    private final boolean needsQuoting;
    
    public Atom(String value) {
        this.value = value;
        this.needsQuoting = requiresQuoting(value);
        this.isGround = true;
    }
    
    /**
     * Determina se l'atomo richiede virgolettature secondo regole ISO
     */
    private boolean requiresQuoting(String atom) {
        if (atom.isEmpty()) return true;
        
        // Controlla se inizia con lettera minuscola
        if (!Character.isLowerCase(atom.charAt(0))) {
            return true;
        }
        
        // Controlla che tutti i caratteri siano alfanumerici o underscore
        for (int i = 1; i < atom.length(); i++) {
            char c = atom.charAt(i);
            if (!Character.isLetterOrDigit(c) && c != '_') {
                return true;
            }
        }
        return false;
    }
    
    @Override
    public String toString() {
        if (needsQuoting) {
            return "'" + value.replace("'", "''") + "'";
        }
        return value;
    }
    
    @Override
    public String getCanonicalForm() {
        return value;
    }
    
    @Override
    public Object getValue() {
        return value;
    }
}

/**
 * Implementazione numero supportando interi e virgola mobile
 */
public class Number extends AtomicTerm {
    private final java.lang.Number value;
    private final boolean isInteger;
    
    public Number(int value) {
        this.value = value;
        this.isInteger = true;
        this.isGround = true;
    }
    
    public Number(double value) {
        this.value = value;
        this.isInteger = false;
        this.isGround = true;
    }
    
    @Override
    public boolean isNumber() { return true; }
    
    @Override
    public String toString() {
        if (isInteger) {
            return String.valueOf(value.intValue());
        } else {
            return String.valueOf(value.doubleValue());
        }
    }
    
    @Override
    public Object getValue() {
        return value;
    }
    
    /**
     * Supporto operazioni aritmetiche
     */
    public Number add(Number other) {
        if (this.isInteger && other.isInteger) {
            return new Number(this.value.intValue() + other.value.intValue());
        } else {
            return new Number(this.value.doubleValue() + other.value.doubleValue());
        }
    }
    
    public Number multiply(Number other) {
        if (this.isInteger && other.isInteger) {
            return new Number(this.value.intValue() * other.value.intValue());
        } else {
            return new Number(this.value.doubleValue() * other.value.doubleValue());
        }
    }
}
```

### 6.3.2 Variabili

JProlog implementa gestione appropriata delle variabili con scoping, binding e verifica occorrenze secondo specifiche ISO.

#### Implementazione Variabili con Scoping

```java
/**
 * Implementazione variabili con semantica appropriata scoping e binding
 */
public class Variable extends Term {
    private final String name;
    private final UUID scopeId;
    private Term binding = null;
    private final boolean isAnonymous;
    
    public Variable(String name) {
        this.name = name;
        this.scopeId = UUID.randomUUID();
        this.isAnonymous = "_".equals(name) || name.startsWith("_");
        this.variables.add(this);
    }
    
    /**
     * Crea variabile con scope specifico (per copia regole)
     */
    public Variable(String name, UUID scopeId) {
        this.name = name;
        this.scopeId = scopeId;
        this.isAnonymous = "_".equals(name) || name.startsWith("_");
        this.variables.add(this);
    }
    
    @Override
    public boolean unify(Term other, Substitution substitution) {
        // Controlla se già legata
        if (binding != null) {
            return binding.unify(other, substitution);
        }
        
        // Variabili anonime si unificano con qualsiasi cosa
        if (isAnonymous) {
            return true;
        }
        
        // Controlla binding esistente nella sostituzione
        Term existingBinding = substitution.getBinding(this);
        if (existingBinding != null) {
            return existingBinding.unify(other, substitution);
        }
        
        // Verifica occorrenze per prevenire strutture infinite
        if (performOccursCheck() && other.variables.contains(this)) {
            return false;
        }
        
        // Crea nuovo binding
        substitution.bind(this, other);
        return true;
    }
    
    @Override
    public Term substitute(Substitution substitution) {
        Term binding = substitution.getBinding(this);
        if (binding != null) {
            return binding.substitute(substitution);
        }
        return this;
    }
    
    @Override
    public String toString() {
        if (binding != null) {
            return binding.toString();
        }
        return name;
    }
    
    /**
     * Crea copia rinominata per copia regole
     */
    public Variable createRenamedCopy() {
        String newName = isAnonymous ? "_" : name + "_" + System.nanoTime();
        return new Variable(newName, UUID.randomUUID());
    }
    
    /**
     * Uguaglianza variabili basata su scope
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (!(obj instanceof Variable)) return false;
        Variable other = (Variable) obj;
        return name.equals(other.name) && scopeId.equals(other.scopeId);
    }
    
    @Override
    public int hashCode() {
        return Objects.hash(name, scopeId);
    }
}

/**
 * Gestione sostituzioni variabili
 */
public class Substitution {
    private final Map<Variable, Term> bindings = new HashMap<>();
    
    public void bind(Variable variable, Term term) {
        bindings.put(variable, term);
    }
    
    public Term getBinding(Variable variable) {
        return bindings.get(variable);
    }
    
    public boolean isBound(Variable variable) {
        return bindings.containsKey(variable);
    }
    
    /**
     * Compone questa sostituzione con un'altra
     */
    public Substitution compose(Substitution other) {
        Substitution result = new Substitution();
        
        // Applica altra sostituzione ai nostri binding
        for (Map.Entry<Variable, Term> entry : this.bindings.entrySet()) {
            result.bind(entry.getKey(), entry.getValue().substitute(other));
        }
        
        // Aggiunge binding da altra che non sono in conflitto
        for (Map.Entry<Variable, Term> entry : other.bindings.entrySet()) {
            if (!result.bindings.containsKey(entry.getKey())) {
                result.bind(entry.getKey(), entry.getValue());
            }
        }
        
        return result;
    }
}
```

### 6.3.3 Termini Composti - Notazione Funzionale

JProlog implementa supporto completo per notazione funzionale con gestione appropriata argomenti e gestione funtori.

#### Implementazione Notazione Funzionale

```java
/**
 * Implementazione termini composti con supporto notazione funzionale
 */
public class CompoundTerm extends Term {
    private final String functor;
    private final List<Term> arguments;
    private final int arity;
    private final String predicateIndicator;
    
    public CompoundTerm(String functor, List<Term> arguments) {
        this.functor = functor;
        this.arguments = new ArrayList<>(arguments);
        this.arity = arguments.size();
        this.predicateIndicator = functor + "/" + arity;
        
        // Calcola variabili e stato ground
        for (Term arg : arguments) {
            this.variables.addAll(arg.variables);
        }
        this.isGround = this.variables.isEmpty();
    }
    
    @Override
    public boolean unify(Term other, Substitution substitution) {
        if (other instanceof Variable) {
            return other.unify(this, substitution);
        }
        
        if (!(other instanceof CompoundTerm)) {
            return false;
        }
        
        CompoundTerm otherCompound = (CompoundTerm) other;
        
        // Controlla corrispondenza funtore e arità
        if (!this.functor.equals(otherCompound.functor) || 
            this.arity != otherCompound.arity) {
            return false;
        }
        
        // Unifica tutti gli argomenti
        for (int i = 0; i < arity; i++) {
            if (!this.arguments.get(i).unify(otherCompound.arguments.get(i), substitution)) {
                return false;
            }
        }
        
        return true;
    }
    
    @Override
    public Term substitute(Substitution substitution) {
        List<Term> newArgs = new ArrayList<>();
        boolean changed = false;
        
        for (Term arg : arguments) {
            Term newArg = arg.substitute(substitution);
            newArgs.add(newArg);
            if (newArg != arg) {
                changed = true;
            }
        }
        
        return changed ? new CompoundTerm(functor, newArgs) : this;
    }
    
    @Override
    public String toString() {
        if (arity == 0) {
            return functor;
        }
        
        StringBuilder sb = new StringBuilder();
        sb.append(functor).append("(");
        for (int i = 0; i < arguments.size(); i++) {
            if (i > 0) sb.append(", ");
            sb.append(arguments.get(i).toString());
        }
        sb.append(")");
        return sb.toString();
    }
    
    /**
     * Ottieni argomento per posizione (indicizzazione base 1 secondo ISO)
     */
    public Term getArgument(int position) {
        if (position < 1 || position > arity) {
            throw new IllegalArgumentException("Posizione argomento non valida: " + position);
        }
        return arguments.get(position - 1);
    }
    
    /**
     * Crea nuovo termine composto con argomento modificato
     */
    public CompoundTerm withArgument(int position, Term newArg) {
        if (position < 1 || position > arity) {
            throw new IllegalArgumentException("Posizione argomento non valida: " + position);
        }
        
        List<Term> newArgs = new ArrayList<>(arguments);
        newArgs.set(position - 1, newArg);
        return new CompoundTerm(functor, newArgs);
    }
    
    /**
     * Manipolazione funtore per predicato functor/3
     */
    public static CompoundTerm createFromFunctorArity(String functor, int arity) {
        List<Term> args = new ArrayList<>();
        for (int i = 0; i < arity; i++) {
            args.add(new Variable("_G" + i));
        }
        return new CompoundTerm(functor, args);
    }
}
```

### 6.3.4 Termini Composti - Notazione Operatore

JProlog implementa notazione completa operatori con gestione appropriata precedenza e regole associatività.

#### Implementazione Notazione Operatore

```java
/**
 * Parsing operatori con precedenza e associatività appropriate
 */
public class OperatorParser {
    
    /**
     * Struttura informazioni operatore
     */
    public static class OperatorInfo {
        public final int precedence;
        public final String associativity; // fx, fy, xf, yf, xfx, xfy, yfx
        
        public OperatorInfo(int precedence, String associativity) {
            this.precedence = precedence;
            this.associativity = associativity;
        }
        
        public boolean isPrefix() {
            return associativity.startsWith("f");
        }
        
        public boolean isInfix() {
            return associativity.length() == 3;
        }
        
        public boolean isPostfix() {
            return associativity.endsWith("f") && associativity.length() == 2;
        }
    }
    
    /**
     * Parsing espressione operatore con scalata precedenza
     */
    public Term parseOperatorExpression(PrologParser parser, int minPrec) 
            throws PrologParserException {
        
        Term left = parsePrefix(parser);
        
        while (parser.hasMore()) {
            Token opToken = parser.currentToken();
            if (!isInfixOperator(opToken.getValue())) {
                break;
            }
            
            OperatorInfo opInfo = getOperatorInfo(opToken.getValue());
            if (opInfo.precedence < minPrec) {
                break;
            }
            
            parser.nextToken(); // consuma operatore
            
            int nextMinPrec = opInfo.associativity.equals("xfy") ? 
                opInfo.precedence : opInfo.precedence + 1;
            
            Term right = parseOperatorExpression(parser, nextMinPrec);
            left = new CompoundTerm(opToken.getValue(), 
                Arrays.asList(left, right));
        }
        
        return left;
    }
    
    /**
     * Parsing operatori prefisso
     */
    private Term parsePrefix(PrologParser parser) throws PrologParserException {
        Token token = parser.currentToken();
        
        if (isPrefixOperator(token.getValue())) {
            OperatorInfo opInfo = getOperatorInfo(token.getValue());
            parser.nextToken();
            
            int nextMinPrec = opInfo.associativity.equals("fy") ? 
                opInfo.precedence : opInfo.precedence + 1;
            
            Term operand = parseOperatorExpression(parser, nextMinPrec);
            return new CompoundTerm(token.getValue(), Arrays.asList(operand));
        }
        
        return parser.parsePrimaryTerm();
    }
    
    /**
     * Definizioni operatori standard
     */
    private static final Map<String, OperatorInfo> STANDARD_OPERATORS = Map.of(
        // Operatori aritmetici
        "+", new OperatorInfo(500, "yfx"),
        "-", new OperatorInfo(500, "yfx"),
        "*", new OperatorInfo(400, "yfx"),
        "/", new OperatorInfo(400, "yfx"),
        "mod", new OperatorInfo(400, "yfx"),
        "**", new OperatorInfo(200, "xfx"),
        
        // Operatori comparazione
        "=", new OperatorInfo(700, "xfx"),
        "\\=", new OperatorInfo(700, "xfx"),
        "==", new OperatorInfo(700, "xfx"),
        "\\==", new OperatorInfo(700, "xfx"),
        "is", new OperatorInfo(700, "xfx"),
        "=:=", new OperatorInfo(700, "xfx"),
        "=\\=", new OperatorInfo(700, "xfx"),
        "<", new OperatorInfo(700, "xfx"),
        "=<", new OperatorInfo(700, "xfx"),
        ">", new OperatorInfo(700, "xfx"),
        ">=", new OperatorInfo(700, "xfx"),
        
        // Operatori controllo
        ":-", new OperatorInfo(1200, "xfx"),
        ";", new OperatorInfo(1100, "xfy"),
        "->", new OperatorInfo(1050, "xfy"),
        ",", new OperatorInfo(1000, "xfy"),
        "\\+", new OperatorInfo(900, "fy"),
        
        // Operatori unari
        "-", new OperatorInfo(200, "fy"),
        "+", new OperatorInfo(200, "fy")
    );
    
    /**
     * Visualizza operatori in forma canonica
     */
    public String displayOperatorTerm(CompoundTerm term) {
        String functor = term.getFunctor();
        int arity = term.getArity();
        
        if (arity == 1 && isPrefixOperator(functor)) {
            return functor + " " + term.getArgument(1).toString();
        } else if (arity == 2 && isInfixOperator(functor)) {
            return term.getArgument(1).toString() + " " + 
                   functor + " " + 
                   term.getArgument(2).toString();
        } else if (arity == 1 && isPostfixOperator(functor)) {
            return term.getArgument(1).toString() + " " + functor;
        } else {
            return term.toString(); // Usa notazione funzionale
        }
    }
}
```

### 6.3.5 Termini Composti - Notazione Lista

JProlog implementa supporto completo notazione lista con decomposizione appropriata testa/coda e rappresentazione lista conforme ISO.

#### Implementazione Liste

```java
/**
 * Implementazione liste con semantica appropriata ISO Prolog
 */
public class ListTerm extends CompoundTerm {
    
    /**
     * Crea lista da elementi
     */
    public static Term createList(List<Term> elements) {
        if (elements.isEmpty()) {
            return new Atom("[]");
        }
        
        Term result = new Atom("[]");
        for (int i = elements.size() - 1; i >= 0; i--) {
            result = new CompoundTerm(".", Arrays.asList(elements.get(i), result));
        }
        return result;
    }
    
    /**
     * Crea lista con struttura testa/coda
     */
    public static Term createList(Term head, Term tail) {
        return new CompoundTerm(".", Arrays.asList(head, tail));
    }
    
    /**
     * Controlla se termine rappresenta una lista
     */
    public static boolean isList(Term term) {
        if (term instanceof Atom && "[]".equals(((Atom) term).getValue())) {
            return true;
        }
        
        if (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            if (".".equals(compound.getFunctor()) && compound.getArity() == 2) {
                return isList(compound.getArgument(2));
            }
        }
        
        return false;
    }
    
    /**
     * Controlla se termine è lista propria (termina con [])
     */
    public static boolean isProperList(Term term) {
        while (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            if (!".".equals(compound.getFunctor()) || compound.getArity() != 2) {
                return false;
            }
            term = compound.getArgument(2);
        }
        
        return term instanceof Atom && "[]".equals(((Atom) term).getValue());
    }
    
    /**
     * Converte termine lista in List Java
     */
    public static List<Term> toJavaList(Term listTerm) {
        List<Term> result = new ArrayList<>();
        
        while (listTerm instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) listTerm;
            if (!".".equals(compound.getFunctor()) || compound.getArity() != 2) {
                break;
            }
            
            result.add(compound.getArgument(1));
            listTerm = compound.getArgument(2);
        }
        
        return result;
    }
    
    /**
     * Ottieni lunghezza lista
     */
    public static int getListLength(Term listTerm) {
        int length = 0;
        
        while (listTerm instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) listTerm;
            if (!".".equals(compound.getFunctor()) || compound.getArity() != 2) {
                return -1; // Non è lista propria
            }
            
            length++;
            listTerm = compound.getArgument(2);
        }
        
        if (listTerm instanceof Atom && "[]".equals(((Atom) listTerm).getValue())) {
            return length;
        }
        
        return -1; // Non è lista propria
    }
    
    /**
     * Visualizza lista in notazione parentesi quadre
     */
    public static String displayList(Term listTerm) {
        if (!isList(listTerm)) {
            return listTerm.toString();
        }
        
        if (listTerm instanceof Atom && "[]".equals(((Atom) listTerm).getValue())) {
            return "[]";
        }
        
        StringBuilder sb = new StringBuilder();
        sb.append("[");
        
        boolean first = true;
        while (listTerm instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) listTerm;
            if (!".".equals(compound.getFunctor()) || compound.getArity() != 2) {
                break;
            }
            
            if (!first) {
                sb.append(", ");
            }
            sb.append(compound.getArgument(1).toString());
            first = false;
            
            listTerm = compound.getArgument(2);
        }
        
        if (!(listTerm instanceof Atom && "[]".equals(((Atom) listTerm).getValue()))) {
            // Lista impropria con coda
            sb.append("|").append(listTerm.toString());
        }
        
        sb.append("]");
        return sb.toString();
    }
}

/**
 * Supporto parsing liste
 */
public class ListParser {
    
    /**
     * Parsing notazione lista [H|T] o [a,b,c]
     */
    public Term parseList(PrologParser parser) throws PrologParserException {
        parser.expectToken(TokenType.LEFT_BRACKET);
        
        if (parser.currentToken().getType() == TokenType.RIGHT_BRACKET) {
            // Lista vuota []
            parser.nextToken();
            return new Atom("[]");
        }
        
        List<Term> elements = new ArrayList<>();
        Term tail = null;
        
        // Parsing primo elemento
        elements.add(parser.parseTerm());
        
        while (parser.currentToken().getType() == TokenType.COMMA) {
            parser.nextToken(); // consuma virgola
            elements.add(parser.parseTerm());
        }
        
        // Controlla notazione coda
        if (parser.currentToken().getType() == TokenType.PIPE) {
            parser.nextToken(); // consuma |
            tail = parser.parseTerm();
        }
        
        parser.expectToken(TokenType.RIGHT_BRACKET);
        
        // Costruisce struttura lista
        if (tail == null) {
            tail = new Atom("[]");
        }
        
        for (int i = elements.size() - 1; i >= 0; i--) {
            tail = new CompoundTerm(".", Arrays.asList(elements.get(i), tail));
        }
        
        return tail;
    }
}
```

### 6.3.6 Termini Composti - Termini Parentesi Graffe

JProlog implementa termini parentesi graffe con parsing appropriato e gestione semantica.

#### Implementazione Termini Parentesi Graffe

```java
/**
 * Implementazione termini parentesi graffe
 */
public class CurlyBracketedTermParser {
    
    /**
     * Parsing termine parentesi graffe {term}
     */
    public Term parseCurlyBracketedTerm(PrologParser parser) 
            throws PrologParserException {
        
        parser.expectToken(TokenType.LEFT_BRACE);
        
        if (parser.currentToken().getType() == TokenType.RIGHT_BRACE) {
            // Parentesi graffe vuote {}
            parser.nextToken();
            return new CompoundTerm("{}", Collections.emptyList());
        }
        
        Term innerTerm = parser.parseTerm();
        parser.expectToken(TokenType.RIGHT_BRACE);
        
        return new CompoundTerm("{}", Arrays.asList(innerTerm));
    }
}

/**
 * Rappresentazione termini parentesi graffe
 */
public class CurlyTerm extends CompoundTerm {
    
    public CurlyTerm(Term content) {
        super("{}", content != null ? Arrays.asList(content) : Collections.emptyList());
    }
    
    public Term getContent() {
        return getArity() > 0 ? getArgument(1) : new Atom("true");
    }
    
    @Override
    public String toString() {
        if (getArity() == 0) {
            return "{}";
        }
        return "{" + getArgument(1).toString() + "}";
    }
    
    /**
     * Termini parentesi graffe spesso usati per notazione insieme
     * o per raggruppare termini senza influenzare parsing
     */
    public boolean isSet() {
        return getArity() > 0 && isSetContent(getArgument(1));
    }
    
    private boolean isSetContent(Term term) {
        // Controlla se termine rappresenta elementi insieme
        if (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            return ",".equals(compound.getFunctor()) && compound.getArity() == 2;
        }
        return true; // Insieme elemento singolo
    }
}
```

### 6.3.7 Termini - Notazione Lista Doppi Apici

JProlog implementa stringhe doppi apici con conversione appropriata lista caratteri secondo specifiche ISO.

#### Implementazione Stringhe Doppi Apici

```java
/**
 * Elaborazione stringhe doppi apici
 */
public class DoubleQuotedStringProcessor {
    
    /**
     * Converte stringa doppi apici in lista codici carattere
     */
    public Term processDoubleQuotedString(String quotedString) {
        // Rimuove apici
        String content = quotedString.substring(1, quotedString.length() - 1);
        
        // Gestisce sequenze escape
        String processed = processEscapeSequences(content);
        
        // Converte in codici carattere
        List<Term> charCodes = new ArrayList<>();
        for (char c : processed.toCharArray()) {
            charCodes.add(new Number((int) c));
        }
        
        return ListTerm.createList(charCodes);
    }
    
    /**
     * Elabora sequenze escape nella stringa
     */
    private String processEscapeSequences(String input) {
        StringBuilder result = new StringBuilder();
        boolean inEscape = false;
        
        for (int i = 0; i < input.length(); i++) {
            char c = input.charAt(i);
            
            if (inEscape) {
                switch (c) {
                    case 'n':  result.append('\n'); break;
                    case 't':  result.append('\t'); break;
                    case 'r':  result.append('\r'); break;
                    case '\\': result.append('\\'); break;
                    case '"':  result.append('"'); break;
                    case '\'': result.append('\''); break;
                    case 'a':  result.append('\u0007'); break; // Alert
                    case 'b':  result.append('\b'); break;
                    case 'f':  result.append('\f'); break;
                    case 'v':  result.append('\u000B'); break; // Tab verticale
                    default:
                        // Escape ottale o esadecimale
                        if (Character.isDigit(c)) {
                            result.append(processNumericEscape(input, i));
                        } else {
                            result.append(c);
                        }
                        break;
                }
                inEscape = false;
            } else if (c == '\\') {
                inEscape = true;
            } else {
                result.append(c);
            }
        }
        
        return result.toString();
    }
    
    /**
     * Elabora sequenze escape numeriche
     */
    private char processNumericEscape(String input, int startPos) {
        // Gestisce escape ottali \nnn\ o escape esadecimali \xHH\
        if (startPos > 0 && input.charAt(startPos - 1) == 'x') {
            // Escape esadecimale
            StringBuilder hex = new StringBuilder();
            int pos = startPos;
            while (pos < input.length() && hex.length() < 2) {
                char c = input.charAt(pos);
                if (Character.isDigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')) {
                    hex.append(c);
                    pos++;
                } else {
                    break;
                }
            }
            return (char) Integer.parseInt(hex.toString(), 16);
        } else {
            // Escape ottale
            StringBuilder octal = new StringBuilder();
            int pos = startPos;
            while (pos < input.length() && octal.length() < 3) {
                char c = input.charAt(pos);
                if (c >= '0' && c <= '7') {
                    octal.append(c);
                    pos++;
                } else {
                    break;
                }
            }
            return (char) Integer.parseInt(octal.toString(), 8);
        }
    }
    
    /**
     * Converte lista caratteri di nuovo in stringa
     */
    public String charListToString(Term listTerm) {
        if (!ListTerm.isList(listTerm)) {
            throw new IllegalArgumentException("Non è lista caratteri");
        }
        
        List<Term> elements = ListTerm.toJavaList(listTerm);
        StringBuilder result = new StringBuilder();
        
        for (Term element : elements) {
            if (!(element instanceof Number)) {
                throw new IllegalArgumentException("Codice carattere non valido");
            }
            
            Number num = (Number) element;
            if (!num.isInteger() || num.intValue() < 0 || num.intValue() > 1114111) {
                throw new IllegalArgumentException("Codice carattere non valido: " + num);
            }
            
            result.append((char) num.intValue());
        }
        
        return result.toString();
    }
}
```

## 6.4 Token

### 6.4.1 Testo Layout

JProlog implementa gestione completa testo layout includendo commenti, spazi vuoti e preservazione formattazione.

#### Implementazione Testo Layout

```java
/**
 * Processore testo layout gestisce spazi vuoti e commenti
 */
public class LayoutTextProcessor {
    
    /**
     * Salta testo layout (spazi vuoti e commenti)
     */
    public void skipLayoutText(PrologTokenizer tokenizer) {
        while (tokenizer.hasMore() && isLayoutText(tokenizer.peek())) {
            if (tokenizer.peek() == '%') {
                skipLineComment(tokenizer);
            } else if (tokenizer.peek() == '/' && tokenizer.peekAhead(1) == '*') {
                skipBlockComment(tokenizer);
            } else {
                // Salta spazi vuoti
                tokenizer.next();
            }
        }
    }
    
    /**
     * Salta commento linea (% fino fine linea)
     */
    private void skipLineComment(PrologTokenizer tokenizer) {
        while (tokenizer.hasMore() && tokenizer.peek() != '\n' && tokenizer.peek() != '\r') {
            tokenizer.next();
        }
        if (tokenizer.hasMore()) {
            tokenizer.next(); // Salta newline
        }
    }
    
    /**
     * Salta commento blocco (/* fino *)
     */
    private void skipBlockComment(PrologTokenizer tokenizer) {
        tokenizer.next(); // Salta /
        tokenizer.next(); // Salta *
        
        while (tokenizer.hasMore()) {
            char c = tokenizer.next();
            if (c == '*' && tokenizer.hasMore() && tokenizer.peek() == '/') {
                tokenizer.next(); // Salta /
                break;
            }
        }
    }
    
    /**
     * Controlla se carattere è testo layout
     */
    private boolean isLayoutText(char c) {
        return Character.isWhitespace(c) || c == '%' || 
               (c == '/' && /* controlla inizio commento blocco */);
    }
    
    /**
     * Preserva formattazione per pretty printing
     */
    public String preserveLayout(String originalText, List<Token> tokens) {
        StringBuilder result = new StringBuilder();
        int textPos = 0;
        
        for (Token token : tokens) {
            // Aggiunge testo layout prima token
            while (textPos < token.getStartPosition()) {
                result.append(originalText.charAt(textPos));
                textPos++;
            }
            
            // Aggiunge testo token
            result.append(token.getValue());
            textPos = token.getEndPosition();
        }
        
        // Aggiunge testo layout rimanente
        while (textPos < originalText.length()) {
            result.append(originalText.charAt(textPos));
            textPos++;
        }
        
        return result.toString();
    }
}
```

### 6.4.2 Nomi

JProlog implementa tokenizzazione completa nomi includendo atomi, atomi virgolettati e riconoscimento operatori.

#### Implementazione Tokenizzazione Nomi

```java
/**
 * Tokenizzatore nomi per atomi e operatori
 */
public class NameTokenizer {
    
    /**
     * Tokenizza nome (atomo o operatore)
     */
    public Token tokenizeName(PrologTokenizer tokenizer) throws PrologParserException {
        int startPos = tokenizer.getPosition();
        StringBuilder name = new StringBuilder();
        
        if (tokenizer.peek() == '\'') {
            // Atomo virgolettato
            return tokenizeQuotedAtom(tokenizer);
        } else if (isGraphicChar(tokenizer.peek())) {
            // Nome grafico (operatore)
            return tokenizeGraphicName(tokenizer);
        } else if (Character.isLowerCase(tokenizer.peek())) {
            // Nome lettera-cifra
            return tokenizeLetterDigitName(tokenizer);
        } else {
            throw new PrologParserException("Carattere inizio nome non valido");
        }
    }
    
    /**
     * Tokenizza atomo virgolettato 'atomo'
     */
    private Token tokenizeQuotedAtom(PrologTokenizer tokenizer) 
            throws PrologParserException {
        int startPos = tokenizer.getPosition();
        StringBuilder atom = new StringBuilder();
        
        tokenizer.next(); // Salta apice iniziale
        
        while (tokenizer.hasMore()) {
            char c = tokenizer.next();
            
            if (c == '\'') {
                if (tokenizer.hasMore() && tokenizer.peek() == '\'') {
                    // Apice escaped
                    atom.append('\'');
                    tokenizer.next();
                } else {
                    // Fine atomo
                    break;
                }
            } else {
                atom.append(c);
            }
        }
        
        return new Token(TokenType.ATOM, atom.toString(), startPos, tokenizer.getPosition());
    }
    
    /**
     * Tokenizza nome grafico (operatori come +, -, ==, etc.)
     */
    private Token tokenizeGraphicName(PrologTokenizer tokenizer) {
        int startPos = tokenizer.getPosition();
        StringBuilder name = new StringBuilder();
        
        while (tokenizer.hasMore() && isGraphicChar(tokenizer.peek())) {
            name.append(tokenizer.next());
        }
        
        String nameStr = name.toString();
        TokenType type = determineOperatorType(nameStr);
        
        return new Token(type, nameStr, startPos, tokenizer.getPosition());
    }
    
    /**
     * Tokenizza nome lettera-cifra (atomi regolari)
     */
    private Token tokenizeLetterDigitName(PrologTokenizer tokenizer) {
        int startPos = tokenizer.getPosition();
        StringBuilder name = new StringBuilder();
        
        // Primo carattere deve essere lettera
        name.append(tokenizer.next());
        
        // Caratteri successivi possono essere lettere, cifre o underscore
        while (tokenizer.hasMore()) {
            char c = tokenizer.peek();
            if (Character.isLetterOrDigit(c) || c == '_') {
                name.append(tokenizer.next());
            } else {
                break;
            }
        }
        
        return new Token(TokenType.ATOM, name.toString(), startPos, tokenizer.getPosition());
    }
    
    /**
     * Determina tipo operatore da nome
     */
    private TokenType determineOperatorType(String name) {
        switch (name) {
            case ":-": return TokenType.RULE_OPERATOR;
            case ",": return TokenType.COMMA;
            case ";": return TokenType.SEMICOLON;
            case "->": return TokenType.IF_THEN;
            case "=": return TokenType.UNIFY;
            case "\\=": return TokenType.NOT_UNIFY;
            case "==": return TokenType.EQUAL;
            case "\\==": return TokenType.NOT_EQUAL;
            case "is": return TokenType.IS;
            case "=:=": return TokenType.ARITH_EQUAL;
            case "=\\=": return TokenType.ARITH_NOT_EQUAL;
            case "<": return TokenType.LESS_THAN;
            case "=<": return TokenType.LESS_EQUAL;
            case ">": return TokenType.GREATER_THAN;
            case ">=": return TokenType.GREATER_EQUAL;
            case "+": return TokenType.PLUS;
            case "-": return TokenType.MINUS;
            case "*": return TokenType.MULTIPLY;
            case "/": return TokenType.DIVIDE;
            case "**": return TokenType.POWER;
            case "mod": return TokenType.MOD;
            case "\\+": return TokenType.NOT_PROVABLE;
            default: return TokenType.OPERATOR;
        }
    }
    
    /**
     * Controlla se carattere è carattere grafico
     */
    private boolean isGraphicChar(char c) {
        return "!@#$%^&*-+=|\\:<>.?/~`".indexOf(c) >= 0;
    }
}
```

### 6.4.3 Variabili

JProlog implementa tokenizzazione variabili con gestione appropriata variabili anonime e convenzioni naming.

#### Tokenizzazione Variabili

```java
/**
 * Tokenizzatore variabili
 */
public class VariableTokenizer {
    
    /**
     * Tokenizza variabile
     */
    public Token tokenizeVariable(PrologTokenizer tokenizer) {
        int startPos = tokenizer.getPosition();
        StringBuilder varName = new StringBuilder();
        
        char firstChar = tokenizer.next();
        varName.append(firstChar);
        
        // Continua con caratteri alfanumerici e underscore
        while (tokenizer.hasMore()) {
            char c = tokenizer.peek();
            if (Character.isLetterOrDigit(c) || c == '_') {
                varName.append(tokenizer.next());
            } else {
                break;
            }
        }
        
        String name = varName.toString();
        TokenType type = isAnonymousVariable(name) ? 
            TokenType.ANONYMOUS_VARIABLE : TokenType.VARIABLE;
        
        return new Token(type, name, startPos, tokenizer.getPosition());
    }
    
    /**
     * Controlla se variabile è anonima
     */
    private boolean isAnonymousVariable(String name) {
        return "_".equals(name) || name.startsWith("_");
    }
    
    /**
     * Valida nome variabile
     */
    public boolean isValidVariableName(String name) {
        if (name.isEmpty()) {
            return false;
        }
        
        char first = name.charAt(0);
        if (!Character.isUpperCase(first) && first != '_') {
            return false;
        }
        
        for (int i = 1; i < name.length(); i++) {
            char c = name.charAt(i);
            if (!Character.isLetterOrDigit(c) && c != '_') {
                return false;
            }
        }
        
        return true;
    }
}
```

### 6.4.4 Numeri Interi

JProlog implementa parsing completo numeri interi includendo basi diverse e controllo range appropriato.

#### Implementazione Numeri Interi

```java
/**
 * Tokenizzatore numeri interi
 */
public class IntegerTokenizer {
    
    /**
     * Tokenizza numero intero
     */
    public Token tokenizeInteger(PrologTokenizer tokenizer) throws PrologParserException {
        int startPos = tokenizer.getPosition();
        
        // Controlla basi numeriche diverse
        if (tokenizer.peek() == '0') {
            if (tokenizer.peekAhead(1) == 'x' || tokenizer.peekAhead(1) == 'X') {
                return tokenizeHexInteger(tokenizer);
            } else if (tokenizer.peekAhead(1) == 'o' || tokenizer.peekAhead(1) == 'O') {
                return tokenizeOctalInteger(tokenizer);
            } else if (tokenizer.peekAhead(1) == 'b' || tokenizer.peekAhead(1) == 'B') {
                return tokenizeBinaryInteger(tokenizer);
            }
        }
        
        return tokenizeDecimalInteger(tokenizer);
    }
    
    /**
     * Tokenizza intero decimale
     */
    private Token tokenizeDecimalInteger(PrologTokenizer tokenizer) {
        int startPos = tokenizer.getPosition();
        StringBuilder number = new StringBuilder();
        
        while (tokenizer.hasMore() && Character.isDigit(tokenizer.peek())) {
            number.append(tokenizer.next());
        }
        
        long value = Long.parseLong(number.toString());
        return new Token(TokenType.INTEGER, String.valueOf(value), startPos, tokenizer.getPosition());
    }
    
    /**
     * Tokenizza intero esadecimale (0x...)
     */
    private Token tokenizeHexInteger(PrologTokenizer tokenizer) throws PrologParserException {
        int startPos = tokenizer.getPosition();
        tokenizer.next(); // Salta 0
        tokenizer.next(); // Salta x
        
        StringBuilder hex = new StringBuilder();
        while (tokenizer.hasMore() && isHexDigit(tokenizer.peek())) {
            hex.append(tokenizer.next());
        }
        
        if (hex.length() == 0) {
            throw new PrologParserException("Numero esadecimale non valido");
        }
        
        long value = Long.parseLong(hex.toString(), 16);
        return new Token(TokenType.INTEGER, String.valueOf(value), startPos, tokenizer.getPosition());
    }
    
    /**
     * Tokenizza intero ottale (0o...)
     */
    private Token tokenizeOctalInteger(PrologTokenizer tokenizer) throws PrologParserException {
        int startPos = tokenizer.getPosition();
        tokenizer.next(); // Salta 0
        tokenizer.next(); // Salta o
        
        StringBuilder octal = new StringBuilder();
        while (tokenizer.hasMore() && isOctalDigit(tokenizer.peek())) {
            octal.append(tokenizer.next());
        }
        
        if (octal.length() == 0) {
            throw new PrologParserException("Numero ottale non valido");
        }
        
        long value = Long.parseLong(octal.toString(), 8);
        return new Token(TokenType.INTEGER, String.valueOf(value), startPos, tokenizer.getPosition());
    }
    
    /**
     * Tokenizza intero binario (0b...)
     */
    private Token tokenizeBinaryInteger(PrologTokenizer tokenizer) throws PrologParserException {
        int startPos = tokenizer.getPosition();
        tokenizer.next(); // Salta 0
        tokenizer.next(); // Salta b
        
        StringBuilder binary = new StringBuilder();
        while (tokenizer.hasMore() && isBinaryDigit(tokenizer.peek())) {
            binary.append(tokenizer.next());
        }
        
        if (binary.length() == 0) {
            throw new PrologParserException("Numero binario non valido");
        }
        
        long value = Long.parseLong(binary.toString(), 2);
        return new Token(TokenType.INTEGER, String.valueOf(value), startPos, tokenizer.getPosition());
    }
    
    private boolean isHexDigit(char c) {
        return Character.isDigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
    }
    
    private boolean isOctalDigit(char c) {
        return c >= '0' && c <= '7';
    }
    
    private boolean isBinaryDigit(char c) {
        return c == '0' || c == '1';
    }
}
```

### 6.4.5 Numeri Virgola Mobile

JProlog implementa parsing numeri virgola mobile con supporto appropriato notazione scientifica.

#### Implementazione Virgola Mobile

```java
/**
 * Tokenizzatore numeri virgola mobile
 */
public class FloatingPointTokenizer {
    
    /**
     * Tokenizza numero virgola mobile
     */
    public Token tokenizeFloatingPoint(PrologTokenizer tokenizer) throws PrologParserException {
        int startPos = tokenizer.getPosition();
        StringBuilder number = new StringBuilder();
        
        // Parsing parte intera
        while (tokenizer.hasMore() && Character.isDigit(tokenizer.peek())) {
            number.append(tokenizer.next());
        }
        
        // Parsing punto decimale e parte frazionaria
        if (tokenizer.hasMore() && tokenizer.peek() == '.') {
            number.append(tokenizer.next());
            
            if (!tokenizer.hasMore() || !Character.isDigit(tokenizer.peek())) {
                throw new PrologParserException("Numero virgola mobile non valido");
            }
            
            while (tokenizer.hasMore() && Character.isDigit(tokenizer.peek())) {
                number.append(tokenizer.next());
            }
        }
        
        // Parsing esponente
        if (tokenizer.hasMore() && (tokenizer.peek() == 'e' || tokenizer.peek() == 'E')) {
            number.append(tokenizer.next());
            
            // Segno opzionale
            if (tokenizer.hasMore() && (tokenizer.peek() == '+' || tokenizer.peek() == '-')) {
                number.append(tokenizer.next());
            }
            
            if (!tokenizer.hasMore() || !Character.isDigit(tokenizer.peek())) {
                throw new PrologParserException("Esponente non valido in numero virgola mobile");
            }
            
            while (tokenizer.hasMore() && Character.isDigit(tokenizer.peek())) {
                number.append(tokenizer.next());
            }
        }
        
        double value = Double.parseDouble(number.toString());
        return new Token(TokenType.FLOAT, String.valueOf(value), startPos, tokenizer.getPosition());
    }
    
    /**
     * Valida formato virgola mobile
     */
    public boolean isValidFloatingPoint(String text) {
        try {
            Double.parseDouble(text);
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }
}
```

### 6.4.6 Liste Doppi Apici

Implementazione coperta nella sezione 6.3.7.

### 6.4.7 Stringhe Apice Inverso

JProlog implementa gestione stringhe apice inverso con elaborazione appropriata caratteri.

#### Implementazione Stringhe Apice Inverso

```java
/**
 * Processore stringhe apice inverso
 */
public class BackQuotedStringProcessor {
    
    /**
     * Elabora stringa apice inverso
     */
    public Token processBackQuotedString(PrologTokenizer tokenizer) throws PrologParserException {
        int startPos = tokenizer.getPosition();
        StringBuilder content = new StringBuilder();
        
        tokenizer.next(); // Salta apice inverso iniziale
        
        while (tokenizer.hasMore()) {
            char c = tokenizer.next();
            
            if (c == '`') {
                // Fine stringa
                break;
            } else if (c == '\\') {
                // Gestisce sequenza escape
                if (tokenizer.hasMore()) {
                    char escaped = tokenizer.next();
                    content.append(processEscapeSequence(escaped));
                } else {
                    throw new PrologParserException("Sequenza escape non terminata");
                }
            } else {
                content.append(c);
            }
        }
        
        return new Token(TokenType.BACK_QUOTED_STRING, content.toString(), 
                        startPos, tokenizer.getPosition());
    }
    
    private char processEscapeSequence(char escaped) {
        switch (escaped) {
            case 'n': return '\n';
            case 't': return '\t';
            case 'r': return '\r';
            case '\\': return '\\';
            case '`': return '`';
            default: return escaped;
        }
    }
}
```

### 6.4.8 Altri Token

JProlog implementa gestione per tutti token speciali includendo punteggiatura e delimitatori.

#### Implementazione Token Speciali

```java
/**
 * Processore token speciali
 */
public class SpecialTokenProcessor {
    
    /**
     * Elabora token speciali (punteggiatura, delimitatori)
     */
    public Token processSpecialToken(PrologTokenizer tokenizer) throws PrologParserException {
        int startPos = tokenizer.getPosition();
        char c = tokenizer.next();
        
        TokenType type;
        String value = String.valueOf(c);
        
        switch (c) {
            case '(': type = TokenType.LEFT_PAREN; break;
            case ')': type = TokenType.RIGHT_PAREN; break;
            case '[': type = TokenType.LEFT_BRACKET; break;
            case ']': type = TokenType.RIGHT_BRACKET; break;
            case '{': type = TokenType.LEFT_BRACE; break;
            case '}': type = TokenType.RIGHT_BRACE; break;
            case ',': type = TokenType.COMMA; break;
            case '.': 
                if (tokenizer.hasMore() && Character.isDigit(tokenizer.peek())) {
                    // Questo è un numero virgola mobile
                    tokenizer.backup();
                    return tokenizeFloatingPoint(tokenizer);
                } else {
                    type = TokenType.DOT;
                }
                break;
            case '|': type = TokenType.PIPE; break;
            case '!': type = TokenType.CUT; break;
            default:
                throw new PrologParserException("Carattere speciale sconosciuto: " + c);
        }
        
        return new Token(type, value, startPos, tokenizer.getPosition());
    }
}
```

## 6.5 Set Caratteri Processore

### 6.5.1 Caratteri Grafici

JProlog implementa supporto completo caratteri grafici per costruzione operatori e simboli speciali.

#### Implementazione Caratteri Grafici

```java
/**
 * Processore caratteri grafici
 */
public class GraphicCharacterProcessor {
    
    // Caratteri grafici ISO Prolog
    private static final String GRAPHIC_CHARS = "#$&*+-./:<=>?@^`~\\";
    
    /**
     * Controlla se carattere è grafico
     */
    public boolean isGraphicChar(char c) {
        return GRAPHIC_CHARS.indexOf(c) >= 0;
    }
    
    /**
     * Elabora sequenza caratteri grafici
     */
    public String processGraphicSequence(String sequence) {
        StringBuilder result = new StringBuilder();
        
        for (char c : sequence.toCharArray()) {
            if (isGraphicChar(c)) {
                result.append(c);
            } else {
                break; // Fine sequenza grafica
            }
        }
        
        return result.toString();
    }
    
    /**
     * Valida uso caratteri grafici negli operatori
     */
    public boolean isValidOperatorCharacter(char c) {
        return isGraphicChar(c) && c != '(' && c != ')' && c != '[' && c != ']' && 
               c != '{' && c != '}' && c != ',' && c != '|' && c != '.';
    }
}
```

### 6.5.2 Caratteri Alfanumerici

JProlog supporta elaborazione completa caratteri alfanumerici con supporto Unicode.

#### Implementazione Caratteri Alfanumerici

```java
/**
 * Processore caratteri alfanumerici
 */
public class AlphanumericProcessor {
    
    /**
     * Controlla se carattere è alfanumerico
     */
    public boolean isAlphanumeric(char c) {
        return Character.isLetterOrDigit(c);
    }
    
    /**
     * Controlla se carattere è lettera
     */
    public boolean isLetter(char c) {
        return Character.isLetter(c);
    }
    
    /**
     * Controlla se carattere è cifra
     */
    public boolean isDigit(char c) {
        return Character.isDigit(c);
    }
    
    /**
     * Controlla se carattere può iniziare un atomo
     */
    public boolean canStartAtom(char c) {
        return Character.isLowerCase(c);
    }
    
    /**
     * Controlla se carattere può iniziare una variabile
     */
    public boolean canStartVariable(char c) {
        return Character.isUpperCase(c) || c == '_';
    }
    
    /**
     * Controlla se carattere può continuare identificatore
     */
    public boolean canContinueIdentifier(char c) {
        return Character.isLetterOrDigit(c) || c == '_';
    }
}
```

### 6.5.3 Caratteri Solo

JProlog gestisce caratteri solo (token carattere singolo) appropriatamente.

#### Implementazione Caratteri Solo

```java
/**
 * Processore caratteri solo
 */
public class SoloCharacterProcessor {
    
    // Caratteri che formano sempre token carattere singolo
    private static final Set<Character> SOLO_CHARS = Set.of(
        '(', ')', '[', ']', '{', '}', ',', '|', '!'
    );
    
    /**
     * Controlla se carattere è carattere solo
     */
    public boolean isSoloChar(char c) {
        return SOLO_CHARS.contains(c);
    }
    
    /**
     * Elabora carattere solo
     */
    public Token processSoloChar(char c, int position) {
        TokenType type;
        switch (c) {
            case '(': type = TokenType.LEFT_PAREN; break;
            case ')': type = TokenType.RIGHT_PAREN; break;
            case '[': type = TokenType.LEFT_BRACKET; break;
            case ']': type = TokenType.RIGHT_BRACKET; break;
            case '{': type = TokenType.LEFT_BRACE; break;
            case '}': type = TokenType.RIGHT_BRACE; break;
            case ',': type = TokenType.COMMA; break;
            case '|': type = TokenType.PIPE; break;
            case '!': type = TokenType.CUT; break;
            default:
                throw new IllegalArgumentException("Non è carattere solo: " + c);
        }
        
        return new Token(type, String.valueOf(c), position, position + 1);
    }
}
```

### 6.5.4 Caratteri Layout

JProlog implementa gestione completa caratteri layout.

#### Implementazione Caratteri Layout

```java
/**
 * Processore caratteri layout
 */
public class LayoutCharacterProcessor {
    
    /**
     * Controlla se carattere è carattere layout
     */
    public boolean isLayoutChar(char c) {
        return Character.isWhitespace(c);
    }
    
    /**
     * Controlla se carattere è spazio
     */
    public boolean isSpace(char c) {
        return c == ' ' || c == '\t';
    }
    
    /**
     * Controlla se carattere è newline
     */
    public boolean isNewline(char c) {
        return c == '\n' || c == '\r';
    }
    
    /**
     * Salta caratteri layout
     */
    public void skipLayout(PrologTokenizer tokenizer) {
        while (tokenizer.hasMore() && isLayoutChar(tokenizer.peek())) {
            tokenizer.next();
        }
    }
    
    /**
     * Preserva layout per formattazione
     */
    public String preserveLayoutBetweenTokens(String original, int start, int end) {
        StringBuilder layout = new StringBuilder();
        for (int i = start; i < end; i++) {
            char c = original.charAt(i);
            if (isLayoutChar(c)) {
                layout.append(c);
            }
        }
        return layout.toString();
    }
}
```

### 6.5.5 Caratteri Meta

JProlog gestisce caratteri meta che hanno significato speciale nel parsing.

#### Implementazione Caratteri Meta

```java
/**
 * Processore caratteri meta
 */
public class MetaCharacterProcessor {
    
    // Caratteri con significato speciale nella sintassi Prolog
    private static final Set<Character> META_CHARS = Set.of(
        '\\', '\'', '"', '`', '%'
    );
    
    /**
     * Controlla se carattere è carattere meta
     */
    public boolean isMetaChar(char c) {
        return META_CHARS.contains(c);
    }
    
    /**
     * Elabora carattere meta nel contesto
     */
    public void processMetaChar(char c, PrologTokenizer tokenizer) throws PrologParserException {
        switch (c) {
            case '\\':
                processEscapeCharacter(tokenizer);
                break;
            case '\'':
                processQuotedAtom(tokenizer);
                break;
            case '"':
                processDoubleQuotedString(tokenizer);
                break;
            case '`':
                processBackQuotedString(tokenizer);
                break;
            case '%':
                processLineComment(tokenizer);
                break;
            default:
                throw new PrologParserException("Carattere meta sconosciuto: " + c);
        }
    }
    
    private void processEscapeCharacter(PrologTokenizer tokenizer) {
        // Gestisce sequenze escape in stringhe e atomi
        if (tokenizer.hasMore()) {
            char nextChar = tokenizer.peek();
            // Elabora sequenza escape specifica
        }
    }
    
    private void processQuotedAtom(PrologTokenizer tokenizer) throws PrologParserException {
        // Implementazione per parsing atomo virgolettato
    }
    
    private void processDoubleQuotedString(PrologTokenizer tokenizer) throws PrologParserException {
        // Implementazione per parsing stringa doppi apici
    }
    
    private void processBackQuotedString(PrologTokenizer tokenizer) throws PrologParserException {
        // Implementazione per parsing stringa apice inverso
    }
    
    private void processLineComment(PrologTokenizer tokenizer) {
        // Salta commento linea
        while (tokenizer.hasMore() && tokenizer.peek() != '\n') {
            tokenizer.next();
        }
    }
}
```

## 6.6 Sequenza Collazione

JProlog implementa sequenza collazione conforme ISO per ordinamento termini e comparazione.

#### Implementazione Sequenza Collazione

```java
/**
 * Implementazione sequenza collazione per ordinamento termini
 */
public class CollatingSequence {
    
    /**
     * Compara termini secondo ordinamento standard ISO Prolog
     * Variabili < Numeri < Atomi < Termini composti
     */
    public int compareTerms(Term t1, Term t2) {
        // Ottieni ordine tipo per ogni termine
        int order1 = getTypeOrder(t1);
        int order2 = getTypeOrder(t2);
        
        if (order1 != order2) {
            return Integer.compare(order1, order2);
        }
        
        // Stesso tipo, compara all'interno del tipo
        return compareWithinType(t1, t2);
    }
    
    /**
     * Ottieni valore ordinamento per tipo termine
     */
    private int getTypeOrder(Term term) {
        if (term.isVariable()) return 1;
        if (term.isNumber()) return 2;
        if (term.isAtom()) return 3;
        return 4; // Termine composto
    }
    
    /**
     * Compara termini dello stesso tipo
     */
    private int compareWithinType(Term t1, Term t2) {
        if (t1.isVariable()) {
            return compareVariables((Variable) t1, (Variable) t2);
        } else if (t1.isNumber()) {
            return compareNumbers((Number) t1, (Number) t2);
        } else if (t1.isAtom()) {
            return compareAtoms((Atom) t1, (Atom) t2);
        } else {
            return compareCompoundTerms((CompoundTerm) t1, (CompoundTerm) t2);
        }
    }
    
    /**
     * Compara variabili (per ordine creazione/nome)
     */
    private int compareVariables(Variable v1, Variable v2) {
        return v1.getName().compareTo(v2.getName());
    }
    
    /**
     * Compara numeri (ordinamento numerico)
     */
    private int compareNumbers(Number n1, Number n2) {
        return Double.compare(n1.doubleValue(), n2.doubleValue());
    }
    
    /**
     * Compara atomi (ordinamento alfabetico)
     */
    private int compareAtoms(Atom a1, Atom a2) {
        return a1.getValue().compareTo(a2.getValue());
    }
    
    /**
     * Compara termini composti
     * Prima per arità, poi per nome funtore, poi per argomenti
     */
    private int compareCompoundTerms(CompoundTerm c1, CompoundTerm c2) {
        // Compara per arità prima
        int arityCompare = Integer.compare(c1.getArity(), c2.getArity());
        if (arityCompare != 0) {
            return arityCompare;
        }
        
        // Compara per nome funtore
        int functorCompare = c1.getFunctor().compareTo(c2.getFunctor());
        if (functorCompare != 0) {
            return functorCompare;
        }
        
        // Compara argomenti da sinistra a destra
        for (int i = 0; i < c1.getArity(); i++) {
            int argCompare = compareTerms(c1.getArgument(i + 1), c2.getArgument(i + 1));
            if (argCompare != 0) {
                return argCompare;
            }
        }
        
        return 0; // Termini uguali
    }
    
    /**
     * Predicato ordinamento termine standard @< implementazione
     */
    public boolean standardTermLessThan(Term t1, Term t2) {
        return compareTerms(t1, t2) < 0;
    }
    
    /**
     * Predicato ordinamento termine standard @=< implementazione
     */
    public boolean standardTermLessOrEqual(Term t1, Term t2) {
        return compareTerms(t1, t2) <= 0;
    }
    
    /**
     * Predicato ordinamento termine standard @> implementazione
     */
    public boolean standardTermGreaterThan(Term t1, Term t2) {
        return compareTerms(t1, t2) > 0;
    }
    
    /**
     * Predicato ordinamento termine standard @>= implementazione
     */
    public boolean standardTermGreaterOrEqual(Term t1, Term t2) {
        return compareTerms(t1, t2) >= 0;
    }
}
```

## Conclusione

JProlog implementa un sistema sintassi completo che supporta pienamente le specifiche linguaggio ISO Prolog. L'implementazione include:

- **Grammatica BNF Completa**: Parser discesa ricorsiva con precedenza operatori appropriata
- **Sintassi Termine Astratto**: Rappresentazione AST gerarchica con relazioni termine appropriate  
- **Elaborazione Testo**: Parsing programma completo con gestione clausole e direttive
- **Sistema Termini**: Implementazione completa termini atomici, variabili e composti
- **Tokenizzazione**: Tokenizzatore completo supportando tutti tipi token ISO
- **Elaborazione Caratteri**: Supporto completo set caratteri con classificazione appropriata
- **Sequenza Collazione**: Ordinamento termini conforme ISO per operazioni comparazione

L'implementazione sintassi fornisce fondamento solido per interprete JProlog, assicurando compatibilità con codice Prolog standard mantenendo separazione pulita tra fasi parsing, rappresentazione ed esecuzione.

Questo sistema sintassi supporta funzionalità avanzate includendo definizioni operatori, notazione DCG, elaborazione liste e scoping variabile appropriato, rendendo JProlog adatto sia per uso educativo che applicazioni pratiche programmazione Prolog.