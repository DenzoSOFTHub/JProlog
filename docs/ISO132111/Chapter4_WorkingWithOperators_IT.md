# Capitolo 4: Lavorare con gli Operatori

## Panoramica

Questo capitolo fornisce documentazione completa dell'implementazione di JProlog del sistema degli operatori Prolog. Gli operatori in Prolog permettono espressioni matematiche e logiche più naturali fornendo alternative di notazione infissa, prefissa e postfissa alla notazione tradizionale con funtori. JProlog implementa il sistema degli operatori ISO Prolog con supporto completo per la precedenza degli operatori, l'associatività e la definizione dinamica degli operatori.

## 4.1 Precedenza e Associatività

### 4.1.1 Sistema di Precedenza degli Operatori

JProlog implementa un sistema di parsing basato sulla precedenza dove gli operatori con valori di precedenza più bassi si legano più strettamente di quelli con valori più alti. Il range di precedenza è 1-1200, seguendo gli standard ISO Prolog.

#### Implementazione Centrale della Precedenza

```java
// OperatorTable.java - Precedenze operatori standard
private void initializeStandardOperators() {
    // Precedenza più alta (1200) - struttura clausole e goal
    defineOperator(1200, Operator.Type.XFX, ":-");      // Definizione regole
    defineOperator(1200, Operator.Type.XFX, "-->");     // Regole DCG
    defineOperator(1200, Operator.Type.FX, ":-");       // Direttive
    defineOperator(1200, Operator.Type.FX, "?-");       // Query
    
    // Strutture di controllo
    defineOperator(1100, Operator.Type.XFY, ";");       // Disgiunzione
    defineOperator(1050, Operator.Type.XFY, "->");      // Se-allora
    defineOperator(1000, Operator.Type.XFY, ",");       // Congiunzione
    
    // Negazione
    defineOperator(900, Operator.Type.FY, "\\+");       // Negazione come fallimento
    
    // Confronto e unificazione
    defineOperator(700, Operator.Type.XFX, "=");        // Unificazione
    defineOperator(700, Operator.Type.XFX, "\\=");      // Non unificabile
    defineOperator(700, Operator.Type.XFX, "==");       // Identità termini
    defineOperator(700, Operator.Type.XFX, "\\==");     // Non-identità termini
    defineOperator(700, Operator.Type.XFX, "is");       // Valutazione aritmetica
    defineOperator(700, Operator.Type.XFX, "=:=");      // Uguaglianza aritmetica
    defineOperator(700, Operator.Type.XFX, "=\\=");     // Disuguaglianza aritmetica
    
    // Operazioni aritmetiche
    defineOperator(500, Operator.Type.YFX, "+");        // Addizione
    defineOperator(500, Operator.Type.YFX, "-");        // Sottrazione
    defineOperator(400, Operator.Type.YFX, "*");        // Moltiplicazione
    defineOperator(400, Operator.Type.YFX, "/");        // Divisione
    defineOperator(200, Operator.Type.XFX, "**");       // Elevamento a potenza
}
```

#### Parsing Guidato dalla Precedenza

Il parser usa la precedenza per risolvere espressioni ambigue:

```prolog
% Esempio: Parsing espressioni con precedenza
X is 2 + 3 * 4.
% Analizzato come: X is 2 + (3 * 4)
% Perché * ha precedenza 400 (legame più forte) di + precedenza 500

X is (2 + 3) * 4.
% Esplicitamente raggruppato per sovrascrivere la precedenza

% Esempio precedenza complessa:
Goal = (P ; Q), (R -> S ; T).
% Analizzato come: Goal = ((P ; Q), ((R -> S) ; T))
% Precedenze: , (1000), ; (1100), -> (1050)
```

#### Validazione Precedenza nel Parsing

```java
// TermParser.java - Parsing espressioni basato su precedenza
private static final Map<String, Integer> OPERATOR_PRECEDENCE = new HashMap<>();

static {
    // Precedenze ISO Prolog standard
    OPERATOR_PRECEDENCE.put("is", 700);
    OPERATOR_PRECEDENCE.put("=", 700);
    OPERATOR_PRECEDENCE.put("=:=", 700);
    OPERATOR_PRECEDENCE.put("+", 500);
    OPERATOR_PRECEDENCE.put("-", 500);
    OPERATOR_PRECEDENCE.put("*", 400);
    OPERATOR_PRECEDENCE.put("/", 400);
    OPERATOR_PRECEDENCE.put("**", 200);
    OPERATOR_PRECEDENCE.put("->", 1050);
    OPERATOR_PRECEDENCE.put(";", 1100);
    OPERATOR_PRECEDENCE.put(",", 1000);
}

public Term parseExpression(int minPrecedence) {
    Term left = parseAtom();
    
    while (!isAtEnd() && isOperator(current()) && 
           getPrecedence(current()) >= minPrecedence) {
        
        String operator = consume();
        int precedence = getPrecedence(operator);
        
        // Operatori associativi a destra necessitano gestione speciale
        int nextMinPrec = isRightAssociative(operator) ? 
                          precedence : precedence + 1;
        
        Term right = parseExpression(nextMinPrec);
        left = new CompoundTerm(new Atom(operator), Arrays.asList(left, right));
    }
    
    return left;
}
```

### 4.1.2 Regole di Associatività

JProlog supporta tre tipi di associatività seguendo gli standard ISO Prolog:

#### Associatività Sinistra (YFX, YF)

Operatori associativi a sinistra si raggruppano da sinistra a destra:

```prolog
% Esempio: Operatori associativi a sinistra
A + B + C + D
% Analizzato come: ((A + B) + C) + D
% Perché + è definito come YFX (infisso associativo a sinistra)

A - B - C
% Analizzato come: (A - B) - C
% Mantiene la convenzione matematica
```

#### Associatività Destra (XFY, FY)

Operatori associativi a destra si raggruppano da destra a sinistra:

```prolog
% Esempio: Operatori associativi a destra
P , Q , R
% Analizzato come: P , (Q , R)  
% Perché , è definito come XFY (associativo a destra)

\+ \+ P
% Analizzato come: \+ (\+ P)
% Perché \+ è FY (prefisso associativo a destra)
```

#### Non-Associatività (XFX, FX, XF)

Operatori non-associativi non possono essere concatenati senza parentesi:

```prolog
% Esempio: Operatori non-associativi
X = Y = Z          % ERRORE SINTASSI
X = (Y = Z)        % Legale - esplicitamente parentesizzato
(X = Y) = Z        % Legale - esplicitamente parentesizzato

% Perché = è XFX (infisso non-associativo)
```

#### Implementazione dell'Associatività

```java
// Operator.java - Implementazione associatività
public enum Type {
    FX,   // Prefisso, non-associativo:     fx(arg)
    FY,   // Prefisso, associativo a destra: fy(fy(arg))
    XF,   // Postfisso, non-associativo:    (arg)xf  
    YF,   // Postfisso, associativo a sinistra: ((arg)yf)yf
    XFX,  // Infisso, non-associativo:      arg1 xfx arg2
    XFY,  // Infisso, associativo a destra: arg1 xfy (arg2 xfy arg3)
    YFX   // Infisso, associativo a sinistra: (arg1 yfx arg2) yfx arg3
}

public boolean isLeftAssociative() {
    return type == Type.YFX || type == Type.YF;
}

public boolean isRightAssociative() {
    return type == Type.XFY || type == Type.FY;
}

public boolean isNonAssociative() {
    return type == Type.XFX || type == Type.FX || type == Type.XF;
}

// Calcolo precedenza per posizioni argomento
public int getLeftPrecedence() {
    switch (type) {
        case YFX:
        case YF:
            return precedence;        // Può accettare stessa precedenza
        case XFX:
        case XFY:
            return precedence - 1;    // Deve avere precedenza strettamente maggiore
        default:
            return -1;                // Nessun argomento sinistro
    }
}

public int getRightPrecedence() {
    switch (type) {
        case XFY:
        case FY:
            return precedence;        // Può accettare stessa precedenza
        case XFX:
        case YFX:
        case FX:
            return precedence - 1;    // Deve avere precedenza strettamente maggiore
        default:
            return -1;                // Nessun argomento destro
    }
}
```

### 4.1.3 Conflitti di Precedenza e Risoluzione

Quando operatori multipli hanno la stessa precedenza, l'associatività determina il parsing:

```prolog
% Esempio: Stessa precedenza, associatività diversa
% + e - entrambi hanno precedenza 500, entrambi YFX (associativo a sinistra)
A + B - C + D
% Analizzato come: ((A + B) - C) + D

% Operatori misti con stessa precedenza
A + B * C
% Analizzato come: A + (B * C)  
% Perché * ha precedenza 400 (legame più forte) di + precedenza 500
```

#### Organizzazione Tabella Precedenze

```java
// OperatorTable.java - Livelli precedenza organizzati
public class OperatorTable {
    
    private void initializeStandardOperators() {
        // Livello 1200: Struttura clausole
        defineOperator(1200, Operator.Type.XFX, ":-");
        defineOperator(1200, Operator.Type.XFX, "-->");
        
        // Livello 1100: Disgiunzione
        defineOperator(1100, Operator.Type.XFY, ";");
        
        // Livello 1050: Se-allora
        defineOperator(1050, Operator.Type.XFY, "->");
        
        // Livello 1000: Congiunzione  
        defineOperator(1000, Operator.Type.XFY, ",");
        
        // Livello 900: Negazione
        defineOperator(900, Operator.Type.FY, "\\+");
        
        // Livello 700: Confronti e unificazione
        defineOperator(700, Operator.Type.XFX, "=");
        defineOperator(700, Operator.Type.XFX, "is");
        defineOperator(700, Operator.Type.XFX, "=:=");
        
        // Livello 500: Addizione/sottrazione
        defineOperator(500, Operator.Type.YFX, "+");
        defineOperator(500, Operator.Type.YFX, "-");
        
        // Livello 400: Moltiplicazione/divisione
        defineOperator(400, Operator.Type.YFX, "*");
        defineOperator(400, Operator.Type.YFX, "/");
        
        // Livello 200: Potenza/elevamento a potenza
        defineOperator(200, Operator.Type.XFX, "**");
        defineOperator(200, Operator.Type.XFY, "^");
    }
}
```

## 4.2 Dichiarare Operatori con op/3

### 4.2.1 Il Predicato op/3

Il predicato `op/3` permette definizione e modifica dinamica degli operatori durante l'esecuzione del programma:

```prolog
% Sintassi: op(+Precedenza, +Tipo, +Nome)
% Definisci un nuovo operatore o modifica esistente

% Esempi:
op(500, yfx, piace).               % Definisci operatore infisso 'piace'
op(300, fx, molto).                % Definisci operatore prefisso 'molto'
op(250, xf, davvero).              % Definisci operatore postfisso 'davvero'

% Utilizzo dopo definizione:
giovanni piace maria.              % Equivalente a: piace(giovanni, maria)
molto importante.                  % Equivalente a: molto(importante)  
vero davvero.                      % Equivalente a: davvero(vero)
```

#### Implementazione op/3

```java
// Op.java - Implementazione predicato op/3
public class Op extends AbstractBuiltInWithContext {
    
    private final OperatorTable operatorTable;
    
    @Override
    public boolean solve(QuerySolver solver, Map<String, Term> bindings) {
        Term[] args = getArguments();
        if (args.length != 3) {
            return false;
        }
        
        try {
            // Estrai precedenza
            if (!(args[0] instanceof it.denzosoft.jprolog.core.terms.Number)) {
                return false;
            }
            int precedence = ((it.denzosoft.jprolog.core.terms.Number) args[0])
                           .getValue().intValue();
            
            // Estrai tipo
            if (!(args[1] instanceof Atom)) {
                return false;
            }
            String typeStr = ((Atom) args[1]).getName();
            Operator.Type type = Operator.parseType(typeStr);
            
            // Estrai nome/nomi
            if (args[2] instanceof Atom) {
                String name = ((Atom) args[2]).getName();
                return defineOrRemoveOperator(precedence, type, name);
            } else if (args[2] instanceof CompoundTerm && 
                      ".".equals(((CompoundTerm) args[2]).getFunctor().getName())) {
                // Lista di nomi operatori
                return defineOrRemoveOperatorList(precedence, type, args[2]);
            }
            
            return false;
            
        } catch (Exception e) {
            return false;
        }
    }
    
    private boolean defineOrRemoveOperator(int precedence, Operator.Type type, String name) {
        try {
            if (precedence == 0) {
                // Rimuovi operatore
                return operatorTable.removeOperator(0, type, name) || 
                       removeAllOperators(name, type);
            } else {
                // Definisci operatore
                if (precedence < 1 || precedence > 1200) {
                    return false; // Range precedenza invalido
                }
                
                // Controlla conflitti con operatori esistenti
                if (!isValidOperatorDefinition(precedence, type, name)) {
                    return false;
                }
                
                operatorTable.defineOperator(precedence, type, name);
                return true;
            }
        } catch (Exception e) {
            return false;
        }
    }
}
```

### 4.2.2 Tipi di Operatori e Sintassi

JProlog supporta tutti e sette i tipi di operatori ISO Prolog:

#### Operatori Prefissi (FX, FY)

```prolog
% FX - Prefisso non-associativo
op(900, fx, non).
non vero.                          % Legale: non(vero)
non non vero.                      % ILLEGALE: Non può concatenare operatori FX

% FY - Prefisso associativo a destra  
op(900, fy, veramente).
veramente molto importante.        % Legale: veramente(molto(importante))
veramente veramente importante.    % Legale: veramente(veramente(importante))
```

#### Operatori Postfissi (XF, YF)

```prolog
% XF - Postfisso non-associativo
op(100, xf, '!').                  % Operatore fattoriale
5 !.                               % Legale: !(5) 
5 ! !.                             % ILLEGALE: Non può concatenare operatori XF

% YF - Postfisso associativo a sinistra
op(150, yf, percento).
tassa 15 percento percento.        % Legale: percento(percento(tassa(15)))
```

#### Operatori Infissi (XFX, XFY, YFX)

```prolog
% XFX - Infisso non-associativo
op(700, xfx, uguale).
A uguale B uguale C.               % ILLEGALE: Non può concatenare operatori XFX
A uguale (B uguale C).             % Legale: esplicitamente parentesizzato

% XFY - Infisso associativo a destra
op(400, xfy, poi).
A poi B poi C.                     % Legale: A poi (B poi C)

% YFX - Infisso associativo a sinistra
op(400, yfx, più).
A più B più C.                     % Legale: (A più B) più C
```

### 4.2.3 Validazione Definizione Operatori

JProlog esegue validazione completa quando definisce operatori:

#### Validazione Range Precedenza

```java
// Op.java - Validazione precedenza
private boolean isValidOperatorDefinition(int precedence, Operator.Type type, String name) {
    // Controlla limiti precedenza (standard ISO)
    if (precedence < 1 || precedence > 1200) {
        return false;
    }
    
    // Controlla operatori riservati
    if (isReservedOperator(name)) {
        return false;
    }
    
    // Valida formato nome operatore
    if (!isValidOperatorName(name)) {
        return false;
    }
    
    // Controlla conflitti di parsing
    return !hasConflictingDefinition(precedence, type, name);
}

private boolean isReservedOperator(String name) {
    // Alcuni operatori fondamentali non possono essere ridefiniti
    switch (name) {
        case ",":  // Congiunzione è fondamentale per sintassi Prolog
        case ";":  // Struttura disgiunzione
        case "!":  // Cut non può essere ridefinito
            return true;
        default:
            return false;
    }
}
```

#### Validazione Nome

```java
// Op.java - Validazione nome operatore
private boolean isValidOperatorName(String name) {
    if (name == null || name.isEmpty()) {
        return false;
    }
    
    // Nomi operatori devono essere atomi validi
    // Non possono iniziare con maiuscola (sarebbe variabile)
    if (Character.isUpperCase(name.charAt(0)) || name.charAt(0) == '_') {
        return false;
    }
    
    // Regole validazione ISO aggiuntive potrebbero essere aggiunte qui
    return true;
}
```

### 4.2.4 Definizioni Multiple di Operatori

Lo stesso nome può avere definizioni multiple di operatori con tipi diversi:

```prolog
% Definizioni multiple per stesso nome
op(500, yfx, +).                   % Addizione infissa  
op(200, fy, +).                    % Plus prefisso (unario)

% Esempi di utilizzo:
X is + 5.                          % Plus unario: +(5)
X is 3 + 5.                        % Plus binario: +(3,5)
X is + 3 + 5.                      % Misto: +(+(3),5)
```

#### Implementazione Definizioni Multiple

```java
// OperatorTable.java - Supporto operatori multipli
public class OperatorTable {
    
    private final Map<String, Set<Operator>> operators;
    private final Map<String, Operator> prefixOperators;
    private final Map<String, Operator> postfixOperators;
    private final Map<String, Operator> infixOperators;
    
    public void defineOperator(int precedence, Operator.Type type, String name) {
        Operator operator = new Operator(precedence, type, name);
        
        // Aggiungi a mappa operatori principale (supporta definizioni multiple)
        operators.computeIfAbsent(name, k -> new HashSet<>()).add(operator);
        
        // Aggiorna mappe specializzate per efficienza parsing
        if (operator.isPrefix()) {
            prefixOperators.put(name, operator);
        }
        if (operator.isPostfix()) {
            postfixOperators.put(name, operator);
        }
        if (operator.isInfix()) {
            infixOperators.put(name, operator);
        }
    }
    
    public Set<Operator> getOperators(String name) {
        return new HashSet<>(operators.getOrDefault(name, Collections.emptySet()));
    }
}
```

### 4.2.5 Rimozione Operatori

Gli operatori possono essere rimossi definendoli con precedenza 0:

```prolog
% Rimuovi definizione operatore specifica
op(0, yfx, +).                     % Rimuovi operatore infisso +
op(0, fy, +).                      % Rimuovi operatore prefisso +

% Rimuovi tutte le definizioni di un operatore
op(0, fx, mio_op).                 % Rimuovi tutti gli operatori mio_op
```

#### Implementazione Rimozione Operatori

```java
// Op.java - Rimozione operatori
private boolean defineOrRemoveOperator(int precedence, Operator.Type type, String name) {
    if (precedence == 0) {
        // Rimuovi operatore con tipo specificato, o tutti se tipo corrisponde
        return operatorTable.removeOperator(0, type, name) || 
               removeAllOperators(name, type);
    } else {
        // Definisci nuovo operatore
        if (!isValidOperatorDefinition(precedence, type, name)) {
            return false;
        }
        
        operatorTable.defineOperator(precedence, type, name);
        return true;
    }
}

private boolean removeAllOperators(String name, Operator.Type type) {
    boolean removed = false;
    
    // Trova e rimuovi tutti gli operatori con nome e tipo corrispondenti
    for (Operator op : operatorTable.getOperators(name)) {
        if (op.getType() == type) {
            operatorTable.removeOperator(op.getPrecedence(), op.getType(), op.getName());
            removed = true;
        }
    }
    
    return removed;
}
```

## 4.3 Caratteristiche Avanzate degli Operatori

### 4.3.1 Introspezione Operatori Correnti

JProlog fornisce predicati built-in per esaminare le definizioni degli operatori correnti:

```prolog
% current_op(?Precedenza, ?Tipo, ?Nome)
% Trova definizioni operatori correnti

?- current_op(P, T, +).            % Trova tutti gli operatori +
P = 500, T = yfx ;                 % Addizione infissa
P = 200, T = fy.                   % Plus prefisso

?- current_op(700, T, N).          % Trova tutti gli operatori precedenza 700
T = xfx, N = '=' ;
T = xfx, N = 'is' ;
T = xfx, N = '=:=' ;
% ... ecc
```

#### Implementazione current_op/3

```java
// CurrentOp.java - Introspezione operatori
public class CurrentOp implements BuiltIn {
    
    private final OperatorTable operatorTable;
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, 
                          List<Map<String, Term>> solutions) {
        
        if (query.getArguments().size() != 3) {
            return false;
        }
        
        Term precTerm = query.getArguments().get(0);
        Term typeTerm = query.getArguments().get(1);
        Term nameTerm = query.getArguments().get(2);
        
        // Ottieni tutti gli operatori correnti
        List<Operator> currentOps = operatorTable.getCurrentOperators();
        
        for (Operator op : currentOps) {
            Map<String, Term> newBindings = new HashMap<>(bindings);
            
            // Prova ad unificare con precedenza
            Term precedence = new it.denzosoft.jprolog.core.terms.Number(op.getPrecedence());
            if (!precTerm.unify(precedence, newBindings)) {
                continue;
            }
            
            // Prova ad unificare con tipo
            Term type = new Atom(op.getType().name().toLowerCase());
            if (!typeTerm.unify(type, newBindings)) {
                continue;
            }
            
            // Prova ad unificare con nome
            Term name = new Atom(op.getName());
            if (!nameTerm.unify(name, newBindings)) {
                continue;
            }
            
            solutions.add(newBindings);
        }
        
        return !solutions.isEmpty();
    }
}
```

### 4.3.2 Valutazione Espressioni Basate su Operatori

Gli operatori si integrano senza problemi con la valutazione delle espressioni di JProlog:

```prolog
% Operatori personalizzati per linguaggi specifici del dominio
op(500, xfx, contiene).
op(400, yfx, e).
op(300, fx, non).

% Espressioni simili al linguaggio naturale
lista([1,2,3]) contiene 2.               % Vero
non lista([1,2,3]) contiene 4.           % Vero  
X contiene 1 e X contiene 2.             % X = lista contenente sia 1 che 2
```

#### Integrazione con Valutatore Aritmetico

```java
// ArithmeticEvaluator.java - Integrazione operatori
public class ArithmeticEvaluator {
    
    private final OperatorTable operatorTable;
    
    public Number evaluateExpression(Term expression, Map<String, Term> bindings) {
        if (expression instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) expression;
            String functor = compound.getFunctor().getName();
            
            // Controlla se il funtore è un operatore aritmetico definito
            if (operatorTable.isOperator(functor)) {
                return evaluateOperatorExpression(compound, bindings);
            }
        }
        
        return evaluateStandardExpression(expression, bindings);
    }
    
    private Number evaluateOperatorExpression(CompoundTerm compound, 
                                             Map<String, Term> bindings) {
        String operator = compound.getFunctor().getName();
        List<Term> args = compound.getArguments();
        
        // Gestisci operatori aritmetici personalizzati
        switch (operator) {
            case "+":
                return evaluateAddition(args, bindings);
            case "*":
                return evaluateMultiplication(args, bindings);
            case "**":
                return evaluateExponentiation(args, bindings);
            default:
                throw new ArithmeticException("Operatore sconosciuto: " + operator);
        }
    }
}
```

### 4.3.3 Parsing con Operatori Personalizzati

Il parser si adatta dinamicamente agli operatori appena definiti:

```prolog
% Definisci operatori specifici del dominio
op(400, xfx, ama).
op(300, fx, decisamente).
op(200, xf, '!!!').

% Scrivi espressioni naturali
giovanni ama maria.                         % ama(giovanni, maria)
decisamente giovanni ama maria.             % decisamente(ama(giovanni, maria))
giovanni ama maria !!!.                     % !!!(ama(giovanni, maria))
```

#### Adattamento Parser Dinamico

```java
// TermParser.java - Parsing operatori dinamico
public class TermParser {
    
    private final OperatorTable operatorTable;
    
    public Term parseExpression(int minPrecedence) {
        Term left = parsePrimary();
        
        while (!isAtEnd() && isCurrentAnOperator()) {
            String operatorName = getCurrentToken();
            Operator op = operatorTable.getInfixOperator(operatorName);
            
            if (op == null || op.getPrecedence() < minPrecedence) {
                break;
            }
            
            consume(); // consuma operatore
            
            int nextMinPrec = op.isRightAssociative() ? 
                             op.getPrecedence() : op.getPrecedence() + 1;
            
            Term right = parseExpression(nextMinPrec);
            left = new CompoundTerm(new Atom(operatorName), 
                                   Arrays.asList(left, right));
        }
        
        return left;
    }
    
    private boolean isCurrentAnOperator() {
        String token = getCurrentToken();
        return operatorTable.isOperator(token);
    }
}
```

## 4.4 Esempi e Applicazioni

### 4.4.1 Linguaggio Specifico del Dominio Matematico

```prolog
% Definisci operatori notazione matematica
op(400, xfx, volte).
op(350, xfx, diviso_per).
op(300, xfx, più).
op(300, xfx, meno).

% Espressioni matematiche naturali
calcolo_area(Lunghezza, Larghezza, Area) :-
    Area is Lunghezza volte Larghezza.

calcolo_tasse(Lordo, Aliquota, Netto) :-
    Tassa is Lordo volte Aliquota diviso_per 100,
    Netto is Lordo meno Tassa.

% Utilizzo:
?- calcolo_area(5, 3, A).              % A = 15
?- calcolo_tasse(1000, 20, N).         % N = 800
```

### 4.4.2 DSL Programmazione Logica

```prolog
% Definisci operatori logici
op(300, xfx, implica).
op(250, xfx, see).                     % Se e solo se
op(200, fx, necessariamente).
op(150, fx, possibilmente).

% Espressioni logica modale
regola_logica(P, Q) :-
    P implica Q.

logica_modale(Affermazione) :-
    necessariamente Affermazione.

biconditional(P, Q) :-
    P see Q.

% Utilizzo:
?- regola_logica(socrate_è_uomo, socrate_è_mortale).
?- logica_modale(possibilmente piove).
```

### 4.4.3 Operatori Confronto Personalizzati

```prolog
% Definisci operatori confronto personalizzati
op(700, xfx, approssimativamente).
op(700, xfx, molto_maggiore_di).
op(700, xfx, vicino_a).

% Implementazione confronti personalizzati
X approssimativamente Y :-
    Diff is abs(X - Y),
    Diff < 0.1.

X molto_maggiore_di Y :-
    X > Y * 2.

X vicino_a Y :-
    X approssimativamente Y.

% Utilizzo nelle applicazioni:
controllo_temperatura(Effettiva, Attesa) :-
    Effettiva vicino_a Attesa.

confronto_prestazioni(Nuovo, Vecchio) :-
    Nuovo molto_maggiore_di Vecchio.
```

### 4.4.4 Operatori Processamento Stringhe e Liste

```prolog
% Definisci operatori stringhe/liste
op(500, xfx, concat).
op(450, xfx, contiene).
op(400, xfx, inizia_con).
op(400, xfx, finisce_con).

% Operazioni stringhe (assumendo esistano predicati stringa)
Stringa1 concat Stringa2 :-
    atom_concat(Stringa1, Stringa2, Risultato).

Lista contiene Elemento :-
    member(Elemento, Lista).

Stringa inizia_con Prefisso :-
    atom_codes(Stringa, Codici),
    atom_codes(Prefisso, CodiciPrefisso),
    append(CodiciPrefisso, _, Codici).

% Utilizzo:
?- "ciao" concat " mondo".                % Concatenazione stringhe
?- [1,2,3] contiene 2.                    % Appartenenza lista
?- "nomefile.txt" finisce_con ".txt".     % Controllo suffisso stringa
```

## 4.5 Considerazioni sulle Prestazioni

### 4.5.1 Efficienza Tabella Operatori

JProlog ottimizza la ricerca operatori attraverso strutture dati specializzate:

```java
// OperatorTable.java - Ottimizzazioni prestazioni
public class OperatorTable {
    
    // Mappe separate per tipi operatori diversi per ricerca O(1)
    private final Map<String, Operator> prefixOperators;
    private final Map<String, Operator> postfixOperators;
    private final Map<String, Operator> infixOperators;
    
    // Strutture dati concorrenti per thread safety
    private final Map<String, Set<Operator>> operators = new ConcurrentHashMap<>();
    
    public Operator getInfixOperator(String name) {
        // Ricerca diretta O(1) invece di cercare attraverso tutti gli operatori
        return infixOperators.get(name);
    }
    
    public boolean isOperator(String name) {
        // Test appartenenza veloce
        return operators.containsKey(name);
    }
}
```

### 4.5.2 Prestazioni Parsing

Il parser mette in cache informazioni operatori per evitare ricerche ripetute:

```java
// TermParser.java - Ottimizzazioni parsing
public class TermParser {
    
    // Cache precedenze operatori per operatori usati frequentemente
    private static final Map<String, Integer> PRECEDENCE_CACHE = new HashMap<>();
    
    private int getOperatorPrecedence(String operator) {
        return PRECEDENCE_CACHE.computeIfAbsent(operator, op -> {
            Operator opDef = operatorTable.getInfixOperator(op);
            return opDef != null ? opDef.getPrecedence() : Integer.MAX_VALUE;
        });
    }
}
```

## Conclusione

Questo capitolo ha coperto il sistema completo degli operatori di JProlog:

1. **Precedenza e Associatività**: Implementazione completa delle regole di precedenza ISO Prolog con gestione corretta dell'associatività per tutti i tipi di operatori
2. **Definizione Dinamica Operatori**: Supporto completo per predicato `op/3` con validazione, definizioni multiple e rimozione operatori
3. **Tipi Operatori**: Supporto per tutti e sette i tipi operatori ISO (FX, FY, XF, YF, XFX, XFY, YFX) con comportamento parsing corretto
4. **Integrazione**: Integrazione senza problemi con valutazione aritmetica, parsing e gestione espressioni
5. **Prestazioni**: Strutture dati ottimizzate e caching per processamento operatori efficiente

Il sistema degli operatori abilita notazione matematica naturale, linguaggi specifici del dominio e costrutti di programmazione espressivi mantenendo completa compatibilità ISO Prolog.

---

*Questa documentazione riflette l'implementazione di JProlog del sistema operatori ISO Prolog con ottimizzazioni specifiche per Java e considerazioni thread-safety.*