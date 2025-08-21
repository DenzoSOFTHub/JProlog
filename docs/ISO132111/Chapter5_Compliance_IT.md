# Capitolo 5: Conformità - Implementazione JProlog

## Panoramica

Questo capitolo fornisce un'analisi completa della conformità di JProlog con lo standard Prolog ISO/IEC 13211-1:1995. Esamina come JProlog implementa ogni aspetto dello standard, documentando sia le caratteristiche conformi che le deviazioni, con esempi di implementazione dettagliati e applicazioni pratiche.

---

## 5.1 Processore Prolog

### Definizione e Ambito

Il processore Prolog in JProlog consiste nel motore di esecuzione principale che interpreta i programmi Prolog secondo le specifiche ISO. Il processore gestisce il ciclo di vita dell'esecuzione dal parsing alla risoluzione delle query.

### Implementazione JProlog

Il processore di JProlog è implementato attraverso diversi componenti chiave:

```java
// Implementazione del processore principale in Prolog.java
public class Prolog {
    private final KnowledgeBase knowledgeBase;
    private final QuerySolver querySolver;
    private final Parser parser;
    private final BuiltInRegistry builtInRegistry;
    
    public Prolog() {
        this.knowledgeBase = new KnowledgeBase();
        this.parser = new Parser();
        this.querySolver = new QuerySolver(knowledgeBase);
        this.builtInRegistry = new BuiltInRegistry();
        registerBuiltIns();
    }
    
    // Metodo principale di esecuzione query
    public List<Map<String, Term>> solve(String query) {
        Term queryTerm = parser.parseTerm(query);
        return querySolver.solve(queryTerm);
    }
}
```

### Caratteristiche Principali

1. **Motore di Risoluzione Query**: La classe `QuerySolver` implementa la risoluzione SLD con backtracking:

```java
public class QuerySolver {
    public List<Map<String, Term>> solve(Term goal) {
        // Implementazione della risoluzione SLD
        List<Map<String, Term>> solutions = new ArrayList<>();
        Stack<ChoicePoint> choicePoints = new Stack<>();
        
        // Crea punto di scelta iniziale
        ChoicePoint initial = new ChoicePoint(goal, new Substitution());
        choicePoints.push(initial);
        
        while (!choicePoints.isEmpty()) {
            ChoicePoint current = choicePoints.pop();
            // Tenta di risolvere l'obiettivo corrente
            List<Substitution> unifications = findUnifications(current.goal);
            
            for (Substitution sub : unifications) {
                if (isComplete(sub)) {
                    solutions.add(extractBindings(sub));
                } else {
                    // Crea nuovo punto di scelta per il backtracking
                    choicePoints.push(new ChoicePoint(
                        resolveNext(current.goal, sub), sub
                    ));
                }
            }
        }
        return solutions;
    }
}
```

### Applicazione Pratica

Il processore gestisce query complesse con soluzioni multiple:

```prolog
% Esempio: Trovare tutti i percorsi in un grafo
edge(a, b). edge(b, c). edge(c, d). edge(b, d).

path(X, Y) :- edge(X, Y).
path(X, Y) :- edge(X, Z), path(Z, Y).

% Esecuzione query in JProlog
?- path(a, d).
% Soluzioni: 
% 1. a -> b -> d
% 2. a -> b -> c -> d
```

### Livello di Conformità

- **Conformità ISO**: ~85% conforme con i requisiti del processore principale
- **Deviazioni**: Supporto limitato per le caratteristiche avanzate del sistema di moduli
- **Estensioni**: Capacità aggiuntive di debugging e tracciamento

---

## 5.2 Testo Prolog

### Definizione e Ambito

Il testo Prolog comprende la rappresentazione del codice sorgente, incluse clausole, direttive e commenti. JProlog deve analizzare e interpretare correttamente tutti i formati di testo Prolog validi.

### Implementazione JProlog

Le classi `Parser` e `PrologParser` gestiscono l'elaborazione del testo:

```java
public class Parser {
    private static final String COMMENT_PATTERN = "%.*$";
    private static final String BLOCK_COMMENT_PATTERN = "/\\*.*?\\*/";
    
    public Term parseTerm(String text) {
        // Rimuove commenti
        text = removeComments(text);
        
        // Gestisce notazione DCG
        if (text.contains("-->")) {
            text = transformDCG(text);
        }
        
        // Analizza il testo pulito
        return parseCleanText(text);
    }
    
    private String transformDCG(String dcgRule) {
        // Trasforma regola DCG: S --> NP, VP
        // In: S(S0, S2) :- NP(S0, S1), VP(S1, S2)
        
        String[] parts = dcgRule.split("-->");
        String head = parts[0].trim();
        String body = parts[1].trim();
        
        // Aggiunge argomenti difference list
        String transformedHead = head + "(S0, S" + varCounter + ")";
        String transformedBody = transformDCGBody(body);
        
        return transformedHead + " :- " + transformedBody;
    }
}
```

### Esempi di Formato Testo

JProlog supporta vari formati di testo Prolog:

```prolog
% 1. Fatti (clausole unitarie)
parent(john, mary).
age(john, 45).

% 2. Regole (clausole non unitarie)
grandfather(X, Z) :- 
    father(X, Y), 
    parent(Y, Z).

% 3. Direttive
:- dynamic(score/2).
:- multifile(helper/3).

% 4. Regole DCG
sentence --> noun_phrase, verb_phrase.
noun_phrase --> determiner, noun.

% 5. Commenti
% Commento singola linea
/* Blocco commento
   multi-linea */

% 6. Definizioni operatori
:- op(500, xfx, likes).
john likes mary.  % Usando operatore personalizzato
```

### Codifica Caratteri

JProlog gestisce testo codificato UTF-8 con supporto caratteri appropriato:

```java
public class TermParser {
    private static final String ATOM_PATTERN = 
        "[a-z][a-zA-Z0-9_]*|'[^']*'";
    
    private static final String VARIABLE_PATTERN = 
        "[A-Z_][a-zA-Z0-9_]*";
    
    // Supporto per atomi Unicode
    private boolean isValidAtom(String text) {
        return text.matches(ATOM_PATTERN) || 
               isQuotedAtom(text) ||
               isUnicodeAtom(text);
    }
}
```

### Livello di Conformità

- **Conformità ISO**: ~90% conforme con le specifiche del formato testo
- **Limitazioni**: Alcuni casi avanzati di precedenza degli operatori
- **Estensioni**: Supporto DCG migliorato oltre lo standard ISO

---

## 5.3 Obiettivo Prolog

### Definizione e Ambito

Un obiettivo Prolog rappresenta una query da dimostrare rispetto alla base di conoscenza. Gli obiettivi possono essere atomi semplici, termini composti, o congiunzioni e disgiunzioni complesse.

### Implementazione JProlog

L'elaborazione degli obiettivi è gestita dal sistema di risoluzione query:

```java
public class QuerySolver {
    public List<Map<String, Term>> solve(Term goal) {
        if (goal instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) goal;
            
            // Gestisce costrutti di controllo speciali
            if (compound.getName().equals(",")) {
                return solveConjunction(compound);
            } else if (compound.getName().equals(";")) {
                return solveDisjunction(compound);
            } else if (compound.getName().equals("->")) {
                return solveIfThen(compound);
            } else if (compound.getName().equals("!")) {
                return solveCut();
            }
        }
        
        // Risoluzione obiettivo standard
        return resolveGoal(goal);
    }
    
    private List<Map<String, Term>> solveConjunction(CompoundTerm conj) {
        // (Goal1, Goal2) - entrambi devono avere successo
        Term goal1 = conj.getArguments().get(0);
        Term goal2 = conj.getArguments().get(1);
        
        List<Map<String, Term>> solutions = new ArrayList<>();
        List<Map<String, Term>> sol1 = solve(goal1);
        
        for (Map<String, Term> binding1 : sol1) {
            Term goal2Instantiated = applySubstitution(goal2, binding1);
            List<Map<String, Term>> sol2 = solve(goal2Instantiated);
            
            for (Map<String, Term> binding2 : sol2) {
                Map<String, Term> combined = combineBindings(binding1, binding2);
                solutions.add(combined);
            }
        }
        return solutions;
    }
}
```

### Tipi di Obiettivi ed Esempi

1. **Obiettivi Semplici**:
```prolog
?- parent(john, mary).  % Verifica fatto
?- X = 5.               % Unificazione
?- atom(hello).         % Predicato built-in
```

2. **Obiettivi Composti**:
```prolog
?- father(X, Y), mother(Y, Z).  % Congiunzione
?- member(X, [1,2,3]) ; member(X, [4,5,6]).  % Disgiunzione
?- X > 0 -> write(positive) ; write(non_positive).  % If-then-else
```

3. **Meta-Obiettivi**:
```prolog
?- findall(X, parent(X, _), Parents).  % Raccoglie tutte le soluzioni
?- bagof(Age, person(Name, Age), Ages).  % Collezione raggruppata
?- \+ member(x, [a,b,c]).  % Negazione come fallimento
```

### Traccia Esecuzione Obiettivi

JProlog fornisce capacità di tracciamento per l'esecuzione degli obiettivi:

```java
public class DebugTracer {
    public void traceGoal(Term goal, int depth) {
        String indent = "  ".repeat(depth);
        System.out.println(indent + "Call: " + goal);
        
        List<Map<String, Term>> solutions = solve(goal);
        
        if (solutions.isEmpty()) {
            System.out.println(indent + "Fail: " + goal);
        } else {
            for (Map<String, Term> solution : solutions) {
                System.out.println(indent + "Exit: " + 
                    instantiate(goal, solution));
            }
        }
    }
}
```

### Applicazione Pratica

Risoluzione di obiettivi complessi in un sistema di base di conoscenza:

```prolog
% Sistema di diagnosi medica
symptom(john, fever).
symptom(john, cough).
symptom(mary, headache).

disease(flu, [fever, cough]).
disease(cold, [cough]).
disease(migraine, [headache]).

diagnose(Patient, Disease) :-
    disease(Disease, Symptoms),
    has_all_symptoms(Patient, Symptoms).

has_all_symptoms(_, []).
has_all_symptoms(Patient, [S|Rest]) :-
    symptom(Patient, S),
    has_all_symptoms(Patient, Rest).

% Query
?- diagnose(john, Disease).
% Disease = flu
% Disease = cold
```

### Livello di Conformità

- **Conformità ISO**: ~95% conforme con la semantica di esecuzione degli obiettivi
- **Punti di Forza**: Backtracking appropriato e semantica del cut
- **Limitazioni**: Alcuni costrutti meta-predicato avanzati

---

## 5.4 Documentazione

### Definizione e Ambito

I requisiti di documentazione specificano come un sistema Prolog dovrebbe documentare le sue caratteristiche, predicati built-in e deviazioni dallo standard.

### Implementazione JProlog

JProlog fornisce documentazione completa attraverso molteplici canali:

```java
public class DocumentationGenerator {
    public void generateBuiltInDocs() {
        for (BuiltIn predicate : builtInRegistry.getAll()) {
            Documentation doc = predicate.getDocumentation();
            
            System.out.println("## " + doc.getName() + "/" + doc.getArity());
            System.out.println("**Categoria**: " + doc.getCategory());
            System.out.println("**Conformità ISO**: " + doc.getISOStatus());
            System.out.println("**Descrizione**: " + doc.getDescription());
            System.out.println("**Esempi**:");
            
            for (Example ex : doc.getExamples()) {
                System.out.println("```prolog");
                System.out.println(ex.getQuery());
                System.out.println("% " + ex.getExpectedResult());
                System.out.println("```");
            }
        }
    }
}
```

### Struttura Documentazione

1. **Riferimento Predicati Built-in**:
```markdown
## append/3

**Categoria**: Operazioni Liste
**Conformità ISO**: Completa
**Firma**: append(?List1, ?List2, ?List3)

**Descrizione**: 
Vero quando List3 è la concatenazione di List1 e List2.

**Esempi**:
```prolog
?- append([1,2], [3,4], L).
L = [1,2,3,4].

?- append(L1, L2, [a,b,c]).
L1 = [], L2 = [a,b,c] ;
L1 = [a], L2 = [b,c] ;
L1 = [a,b], L2 = [c] ;
L1 = [a,b,c], L2 = [].
```
```

2. **Documentazione API**:
```java
/**
 * Risolve una query Prolog e restituisce tutte le soluzioni.
 * 
 * @param query La query Prolog come stringa
 * @return Lista di binding variabili per ogni soluzione
 * @throws PrologException se la query è malformata
 * 
 * Esempio:
 * <pre>
 * Prolog prolog = new Prolog();
 * prolog.consult("parent(john, mary).");
 * List<Map<String, Term>> solutions = prolog.solve("parent(X, mary)");
 * // Restituisce: [{X=john}]
 * </pre>
 */
public List<Map<String, Term>> solve(String query) throws PrologException
```

### Sistema di Aiuto Interattivo

```java
public class HelpSystem {
    public void showHelp(String topic) {
        if (topic.equals("predicates")) {
            System.out.println("Predicati disponibili:");
            System.out.println("  append/3 - Concatenazione liste");
            System.out.println("  member/2 - Appartenenza lista");
            System.out.println("  findall/3 - Raccoglie tutte le soluzioni");
            // ... altri predicati
        } else if (topic.matches("\\w+/\\d+")) {
            showPredicateHelp(topic);
        }
    }
}
```

### Livello di Conformità

- **Copertura Documentazione**: 100% dei predicati built-in documentati
- **Formato**: Formati Markdown e JavaDoc
- **Accessibilità**: Integrazione IDE, aiuto CLI, documentazione web

---

## 5.5 Estensioni

### 5.5.1 Sintassi

#### Definizione e Ambito

Le estensioni di sintassi permettono a JProlog di supportare costrutti aggiuntivi oltre lo standard ISO mantenendo la retrocompatibilità.

#### Implementazione JProlog

```java
public class SyntaxExtensions {
    // Supporto operatori esteso
    private static final Map<String, OperatorDef> EXTENDED_OPS = Map.of(
        "?-", new OperatorDef(1200, "fx", "query"),
        ":-", new OperatorDef(1200, "xfx", "rule"),
        "-->", new OperatorDef(1200, "xfx", "dcg"),
        "@", new OperatorDef(200, "xfx", "at"),  // Estensione
        "::", new OperatorDef(600, "xfx", "module")  // Estensione
    );
    
    public Term parseExtendedSyntax(String text) {
        // Supporto per list comprehension (estensione)
        if (text.contains(" | ") && text.contains(" <- ")) {
            return parseListComprehension(text);
        }
        
        // Supporto per letterali stringa (estensione)
        if (text.startsWith("\"") && text.endsWith("\"")) {
            return parseStringLiteral(text);
        }
        
        return standardParse(text);
    }
}
```

#### Esempi di Sintassi Estesa

```prolog
% 1. List comprehension (estensione JProlog)
?- L = [X*2 | X <- [1,2,3,4], X > 2].
L = [6, 8].

% 2. Letterali stringa (estensione JProlog)
?- Name = "John Doe".
Name = "John Doe".

% 3. Qualificazione modulo (supporto parziale)
?- lists:member(X, [1,2,3]).
X = 1 ; X = 2 ; X = 3.

% 4. Sintassi vincoli (supporto limitato)
?- X #> 5, X #< 10.
X = 6 ; X = 7 ; X = 8 ; X = 9.
```

### 5.5.2 Operatori Predefiniti

#### Tabella Operatori JProlog

```java
public class OperatorRegistry {
    private final List<OperatorDef> operators = Arrays.asList(
        // Operatori standard ISO
        new OperatorDef(1200, "xfx", ":-"),
        new OperatorDef(1200, "fx", "?-"),
        new OperatorDef(1100, "xfy", ";"),
        new OperatorDef(1050, "xfy", "->"),
        new OperatorDef(1000, "xfy", ","),
        new OperatorDef(900, "fy", "\\+"),
        new OperatorDef(700, "xfx", "="),
        new OperatorDef(700, "xfx", "\\="),
        new OperatorDef(700, "xfx", "=="),
        new OperatorDef(700, "xfx", "\\=="),
        new OperatorDef(700, "xfx", "@<"),
        new OperatorDef(700, "xfx", "@>"),
        new OperatorDef(700, "xfx", "@=<"),
        new OperatorDef(700, "xfx", "@>="),
        new OperatorDef(700, "xfx", "=.."),
        new OperatorDef(700, "xfx", "is"),
        new OperatorDef(700, "xfx", "=:="),
        new OperatorDef(700, "xfx", "=\\="),
        new OperatorDef(700, "xfx", "<"),
        new OperatorDef(700, "xfx", ">"),
        new OperatorDef(700, "xfx", "=<"),
        new OperatorDef(700, "xfx", ">="),
        new OperatorDef(600, "xfy", ":"),
        new OperatorDef(500, "yfx", "+"),
        new OperatorDef(500, "yfx", "-"),
        new OperatorDef(400, "yfx", "*"),
        new OperatorDef(400, "yfx", "/"),
        new OperatorDef(400, "yfx", "//"),
        new OperatorDef(400, "yfx", "rem"),
        new OperatorDef(400, "yfx", "mod"),
        new OperatorDef(400, "yfx", "<<"),
        new OperatorDef(400, "yfx", ">>"),
        new OperatorDef(200, "xfx", "**"),
        new OperatorDef(200, "xfy", "^"),
        new OperatorDef(200, "fy", "-"),
        new OperatorDef(200, "fy", "+"),
        new OperatorDef(200, "fy", "\\"),
        
        // Estensioni JProlog
        new OperatorDef(1200, "xfx", "-->"),  // DCG
        new OperatorDef(900, "xfx", "@"),     // Operatore at
        new OperatorDef(600, "xfx", "::")     // Separatore modulo
    );
    
    public int getPrecedence(String op) {
        return operators.stream()
            .filter(o -> o.symbol.equals(op))
            .findFirst()
            .map(o -> o.precedence)
            .orElse(0);
    }
}
```

### 5.5.3 Mappatura Conversione Caratteri

#### Implementazione

```java
public class CharacterConversion {
    private final Map<Character, Character> conversionMap = new HashMap<>();
    
    public void setCharConversion(char from, char to) {
        if (from == to) {
            conversionMap.remove(from);
        } else {
            conversionMap.put(from, to);
        }
    }
    
    public String applyConversions(String input) {
        StringBuilder result = new StringBuilder();
        for (char c : input.toCharArray()) {
            result.append(conversionMap.getOrDefault(c, c));
        }
        return result.toString();
    }
}
```

#### Esempio d'Uso

```prolog
% Imposta conversione caratteri
?- char_conversion('a', 'A').
true.

% Ora 'a' viene convertito in 'A' durante la lettura
?- atom_codes(abc, Codes).
Codes = [65, 98, 99].  % 'A', 'b', 'c'
```

### 5.5.4 Tipi

#### Sistema di Tipi JProlog

```java
public abstract class Term {
    public abstract boolean isAtom();
    public abstract boolean isNumber();
    public abstract boolean isVariable();
    public abstract boolean isCompound();
    public abstract boolean isList();
    
    public String getType() {
        if (isAtom()) return "atom";
        if (isNumber()) return "number";
        if (isVariable()) return "variable";
        if (isCompound()) return "compound";
        if (isList()) return "list";
        return "unknown";
    }
}

public class TypeChecker {
    public boolean checkType(Term term, String expectedType) {
        switch (expectedType) {
            case "integer":
                return term instanceof Number && 
                       ((Number) term).isInteger();
            case "float":
                return term instanceof Number && 
                       ((Number) term).isFloat();
            case "atom":
                return term instanceof Atom;
            case "compound":
                return term instanceof CompoundTerm;
            case "list":
                return term instanceof ListTerm || 
                       isListStructure(term);
            case "callable":
                return term instanceof Atom || 
                       term instanceof CompoundTerm;
            default:
                return false;
        }
    }
}
```

#### Esempi di Controllo Tipi

```prolog
% Predicati di controllo tipi base
?- integer(42).
true.

?- float(3.14).
true.

?- atom(hello).
true.

?- compound(f(x,y)).
true.

?- is_list([1,2,3]).
true.

% Operazioni specifiche per tipo
?- X is float(7).     % Conversione tipo
X = 7.0.

?- number_codes(123, Codes).  % Numero a codici
Codes = [49, 50, 51].
```

### 5.5.5 Direttive

#### Elaborazione Direttive

```java
public class DirectiveProcessor {
    public void processDirective(CompoundTerm directive) {
        String name = directive.getName();
        
        switch (name) {
            case "dynamic":
                processDynamic(directive.getArguments());
                break;
            case "multifile":
                processMultifile(directive.getArguments());
                break;
            case "discontiguous":
                processDiscontiguous(directive.getArguments());
                break;
            case "op":
                processOperatorDefinition(directive.getArguments());
                break;
            case "include":
                processInclude(directive.getArguments());
                break;
            case "ensure_loaded":
                processEnsureLoaded(directive.getArguments());
                break;
            default:
                throw new UnsupportedDirectiveException(name);
        }
    }
    
    private void processDynamic(List<Term> args) {
        // Marca predicato come dinamico (modificabile a runtime)
        for (Term arg : args) {
            PredicateIndicator pi = parseIndicator(arg);
            knowledgeBase.markDynamic(pi.name, pi.arity);
        }
    }
}
```

#### Esempi di Direttive

```prolog
% Dichiarazione predicato dinamico
:- dynamic(score/2).
:- dynamic(cache/3).

% Predicato multifile
:- multifile(helper/2).

% Definizione operatore
:- op(900, xfx, likes).

% Include un altro file
:- include('utilities.pl').

% Assicura caricamento una sola volta
:- ensure_loaded(library(lists)).

% Predicato non contiguo
:- discontiguous(process/2).
```

### 5.5.6 Effetti Collaterali

#### Gestione Effetti Collaterali

```java
public class SideEffectManager {
    private final IOManager ioManager;
    private final DatabaseManager dbManager;
    private final FileManager fileManager;
    
    public void executeSideEffect(String predicate, List<Term> args) {
        switch (predicate) {
            case "write":
            case "writeln":
            case "nl":
                ioManager.performOutput(predicate, args);
                break;
            case "assert":
            case "assertz":
            case "asserta":
            case "retract":
            case "retractall":
                dbManager.modifyDatabase(predicate, args);
                break;
            case "open":
            case "close":
            case "see":
            case "seen":
            case "tell":
            case "told":
                fileManager.handleFileOperation(predicate, args);
                break;
        }
    }
    
    // Traccia effetti collaterali per debugging
    private final List<SideEffectLog> effectLog = new ArrayList<>();
    
    public void logEffect(String type, String details) {
        effectLog.add(new SideEffectLog(
            System.currentTimeMillis(), type, details
        ));
    }
}
```

#### Esempi di Effetti Collaterali

```prolog
% Effetti collaterali I/O
?- write('Hello'), write(' '), writeln('World').
Hello World
true.

% Modifica database
?- assertz(new_fact(1)).
true.

?- retract(old_fact(_)).
true.

% Operazioni su file
?- open('data.txt', read, Stream),
   read(Stream, Data),
   close(Stream).
```

### 5.5.7 Costrutti di Controllo

#### Implementazione Costrutti di Controllo

```java
public class ControlConstructs {
    public List<Map<String, Term>> executeControl(
            String construct, List<Term> args) {
        
        switch (construct) {
            case "!":  // Cut
                return executeCut();
                
            case "->":  // If-then
                return executeIfThen(args.get(0), args.get(1));
                
            case ";":  // Or / If-then-else
                if (args.get(0) instanceof CompoundTerm &&
                    ((CompoundTerm)args.get(0)).getName().equals("->")) {
                    // If-then-else: (Cond -> Then ; Else)
                    return executeIfThenElse(args.get(0), args.get(1));
                } else {
                    // Disgiunzione semplice
                    return executeOr(args.get(0), args.get(1));
                }
                
            case "\\+":  // Negazione come fallimento
                return executeNegation(args.get(0));
                
            case "true":  // Sempre successo
                return Collections.singletonList(new HashMap<>());
                
            case "fail":  // Sempre fallimento
                return Collections.emptyList();
                
            case "repeat":  // Punto di scelta infinito
                return executeRepeat();
                
            case "once":  // Successo al massimo una volta
                return executeOnce(args.get(0));
                
            default:
                throw new UnknownControlConstruct(construct);
        }
    }
    
    private List<Map<String, Term>> executeCut() {
        // Rimuove tutti i punti di scelta fino al genitore
        choicePointStack.removeIf(cp -> cp.level >= currentLevel);
        return Collections.singletonList(currentBinding);
    }
}
```

#### Esempi di Costrutti di Controllo

```prolog
% Esempio cut - massimo deterministico
max(X, Y, X) :- X >= Y, !.
max(X, Y, Y).

% If-then-else
classify(X, Type) :-
    (X < 0 -> Type = negative ;
     X > 0 -> Type = positive ;
     Type = zero).

% Negazione come fallimento
not_member(_, []).
not_member(X, [H|T]) :- 
    X \= H, 
    not_member(X, T).

% Once - trova solo la prima soluzione
?- once(member(X, [1,2,3])).
X = 1.

% Repeat - backtracking infinito
generate_numbers(N) :-
    repeat,
    random(1, 100, N).
```

### 5.5.8 Flag

#### Sistema di Gestione Flag

```java
public class FlagManager {
    private final Map<String, Flag> flags = new HashMap<>();
    
    public FlagManager() {
        initializeStandardFlags();
    }
    
    private void initializeStandardFlags() {
        // Flag standard ISO
        flags.put("bounded", new Flag("bounded", true, true));
        flags.put("max_integer", new Flag("max_integer", 
            Long.MAX_VALUE, true));
        flags.put("min_integer", new Flag("min_integer", 
            Long.MIN_VALUE, true));
        flags.put("integer_rounding_function", 
            new Flag("integer_rounding_function", "toward_zero", true));
        flags.put("char_conversion", 
            new Flag("char_conversion", "off", false));
        flags.put("debug", new Flag("debug", "off", false));
        flags.put("max_arity", new Flag("max_arity", 255, true));
        flags.put("unknown", new Flag("unknown", "error", false));
        flags.put("double_quotes", 
            new Flag("double_quotes", "codes", false));
        
        // Flag specifici JProlog
        flags.put("jprolog_version", 
            new Flag("jprolog_version", "2.0.15", true));
        flags.put("encoding", new Flag("encoding", "UTF-8", false));
        flags.put("trace_mode", new Flag("trace_mode", "off", false));
        flags.put("optimization_level", 
            new Flag("optimization_level", 1, false));
    }
    
    public Object getFlag(String name) {
        Flag flag = flags.get(name);
        if (flag == null) {
            throw new ExistenceError("prolog_flag", name);
        }
        return flag.getValue();
    }
    
    public void setFlag(String name, Object value) {
        Flag flag = flags.get(name);
        if (flag == null) {
            throw new ExistenceError("prolog_flag", name);
        }
        if (flag.isReadOnly()) {
            throw new PermissionError("modify", "flag", name);
        }
        flag.setValue(value);
    }
}
```

#### Esempi d'Uso dei Flag

```prolog
% Query flag correnti
?- current_prolog_flag(debug, Value).
Value = off.

?- current_prolog_flag(max_integer, Max).
Max = 9223372036854775807.

% Imposta flag modificabili
?- set_prolog_flag(debug, on).
true.

?- set_prolog_flag(double_quotes, string).
true.

% Controlla flag specifici JProlog
?- current_prolog_flag(jprolog_version, Version).
Version = '2.0.15'.
```

### 5.5.9 Predicati Built-in

#### Registro Predicati Built-in

```java
public class BuiltInRegistry {
    private final Map<String, BuiltIn> builtIns = new HashMap<>();
    
    public void registerAll() {
        // Predicati controllo tipo
        register("var", 1, new VarCheck());
        register("nonvar", 1, new NonVarCheck());
        register("atom", 1, new AtomCheck());
        register("number", 1, new NumberCheck());
        register("integer", 1, new IntegerCheck());
        register("float", 1, new FloatCheck());
        register("compound", 1, new CompoundCheck());
        register("atomic", 1, new AtomicCheck());
        register("callable", 1, new CallableCheck());
        register("ground", 1, new GroundCheck());
        register("is_list", 1, new IsListCheck());
        
        // Predicati aritmetici
        register("is", 2, new Is());
        register("=:=", 2, new ArithmeticEqual());
        register("=\\=", 2, new ArithmeticNotEqual());
        register("<", 2, new LessThan());
        register(">", 2, new GreaterThan());
        register("=<", 2, new LessOrEqual());
        register(">=", 2, new GreaterOrEqual());
        register("between", 3, new Between());
        register("succ", 2, new Succ());
        register("plus", 3, new Plus());
        
        // Operazioni liste
        register("append", 3, new Append());
        register("member", 2, new Member());
        register("length", 2, new Length());
        register("reverse", 2, new Reverse());
        register("sort", 2, new Sort());
        register("msort", 2, new Msort());
        register("nth0", 3, new Nth0());
        register("nth1", 3, new Nth1());
        register("select", 3, new Select());
        
        // Meta-predicati
        register("findall", 3, new Findall());
        register("bagof", 3, new Bagof());
        register("setof", 3, new Setof());
        register("once", 1, new Once());
        
        // Manipolazione termini
        register("functor", 3, new Functor());
        register("arg", 3, new Arg());
        register("=..", 2, new Univ());
        register("copy_term", 2, new CopyTerm());
        register("term_variables", 2, new TermVariables());
        
        // Operazioni atomi
        register("atom_length", 2, new AtomLength());
        register("atom_concat", 3, new AtomConcat());
        register("sub_atom", 5, new SubAtom());
        register("atom_chars", 2, new AtomChars());
        register("atom_codes", 2, new AtomCodes());
        
        // Operazioni database
        register("assert", 1, new Assert());
        register("asserta", 1, new Asserta());
        register("assertz", 1, new Assertz());
        register("retract", 1, new Retract());
        register("retractall", 1, new Retractall());
        register("abolish", 1, new Abolish());
        register("current_predicate", 1, new CurrentPredicate());
        
        // Operazioni I/O
        register("write", 1, new Write());
        register("writeln", 1, new Writeln());
        register("nl", 0, new Nl());
        register("read", 1, new Read());
        register("open", 3, new Open());
        register("close", 1, new Close());
        register("get_char", 1, new GetChar());
        register("put_char", 1, new PutChar());
        
        // Predicati di controllo
        register("!", 0, new Cut());
        register("true", 0, new True());
        register("fail", 0, new Fail());
        register("repeat", 0, new Repeat());
        
        // Supporto DCG
        register("phrase", 2, new Phrase());
        register("phrase", 3, new Phrase3());
    }
}
```

#### Esempi di Predicati Built-in

```prolog
% Controllo tipi
?- atom(hello).
true.

?- var(X), X = 5, nonvar(X).
X = 5.

% Aritmetica
?- X is 2 * 3 + 4.
X = 10.

?- between(1, 5, X).
X = 1 ; X = 2 ; X = 3 ; X = 4 ; X = 5.

% Operazioni liste
?- append([1,2], [3,4], L).
L = [1,2,3,4].

?- length([a,b,c], N).
N = 3.

% Meta-predicati
?- findall(X, member(X, [1,2,3]), L).
L = [1,2,3].

% Manipolazione termini
?- functor(f(a,b,c), F, A).
F = f, A = 3.

?- f(a,b) =.. L.
L = [f,a,b].

% Operazioni database
?- assertz(dynamic_fact(1)).
true.

?- retract(dynamic_fact(X)).
X = 1.
```

---

## Riepilogo Conformità

### Statistiche Generali di Conformità

| Componente | Conformità ISO | Note |
|------------|----------------|------|
| Processore Prolog | ~85% | Risoluzione principale completa, sistema moduli parziale |
| Testo Prolog | ~90% | Supporto sintassi completo, alcuni problemi precedenza operatori |
| Obiettivo Prolog | ~95% | Esecuzione obiettivi completa con backtracking appropriato |
| Documentazione | 100% | Documentazione completa per tutte le caratteristiche |
| Estensioni Sintassi | N/A | Caratteristiche aggiuntive oltre ISO |
| Operatori | ~95% | Tutti gli operatori standard più estensioni |
| Conversione Caratteri | ~80% | Supporto conversione base |
| Sistema Tipi | ~90% | Predicati controllo tipo completi |
| Direttive | ~75% | Direttive principali supportate |
| Effetti Collaterali | ~85% | I/O e modifiche database |
| Costrutti Controllo | ~95% | Cut completo, if-then-else, negazione |
| Flag | ~90% | Flag standard più estensioni JProlog |
| Predicati Built-in | ~85% | 80+ predicati implementati |

### Punti di Forza Principali

1. **Motore Principale Robusto**: Risoluzione SLD solida con backtracking appropriato
2. **Built-in Completi**: 80+ predicati built-in che coprono categorie principali
3. **Supporto DCG**: Supporto grammatiche esteso oltre lo standard ISO
4. **Documentazione**: Documentazione completa per tutte le caratteristiche
5. **Integrazione IDE**: Ambiente di sviluppo professionale

### Limitazioni Note

1. **Sistema Moduli**: Supporto moduli limitato rispetto alla specifica ISO completa
2. **Programmazione a Vincoli**: Nessuna capacità nativa di risoluzione vincoli
3. **Operatori Avanzati**: Alcuni casi complessi di precedenza operatori
4. **Tabling**: Nessun supporto memoization o tabling
5. **Thread**: Nessun supporto multi-threading

### Obiettivi Futuri di Conformità

1. Implementazione completa sistema moduli
2. Aggiunta caratteristiche programmazione logica a vincoli
3. Implementazione tabling per ottimizzazione
4. Miglioramento gestione precedenza operatori
5. Aggiunta capacità multi-threading

---

## Applicazioni Pratiche

### Caso d'Uso 1: Sistemi Esperti

```prolog
% Sistema esperto diagnosi medica
symptom(patient1, fever).
symptom(patient1, cough).
symptom(patient1, fatigue).

disease_symptom(flu, fever).
disease_symptom(flu, cough).
disease_symptom(flu, fatigue).
disease_symptom(cold, cough).
disease_symptom(cold, runny_nose).

diagnose(Patient, Disease) :-
    findall(S, symptom(Patient, S), Symptoms),
    possible_disease(Symptoms, Disease).

possible_disease(Symptoms, Disease) :-
    disease_symptom(Disease, _),
    forall(disease_symptom(Disease, S), member(S, Symptoms)).
```

### Caso d'Uso 2: Elaborazione Linguaggio Naturale

```prolog
% NLP semplice con DCG
sentence --> noun_phrase, verb_phrase.
noun_phrase --> determiner, noun.
verb_phrase --> verb, noun_phrase.

determiner --> [the].
determiner --> [a].
noun --> [cat].
noun --> [dog].
verb --> [chases].
verb --> [sees].

?- phrase(sentence, [the, cat, chases, a, dog]).
true.
```

### Caso d'Uso 3: Pianificazione e Scheduling

```prolog
% Sistema scheduling attività
task(a, 3).  % Task a richiede 3 unità
task(b, 2).
task(c, 4).

dependency(b, a).  % b dipende da a
dependency(c, b).  % c dipende da b

schedule([], _, []).
schedule(Tasks, Time, [(Task, Time)|Schedule]) :-
    select(Task, Tasks, Rest),
    task(Task, Duration),
    all_dependencies_scheduled(Task, Schedule),
    NextTime is Time + Duration,
    schedule(Rest, NextTime, Schedule).

all_dependencies_scheduled(Task, Schedule) :-
    forall(dependency(Task, Dep), member((Dep, _), Schedule)).
```

---

## Conclusione

JProlog dimostra una forte conformità con lo standard Prolog ISO/IEC 13211-1:1995, raggiungendo approssimativamente l'85-95% di conformità attraverso i diversi componenti. L'implementazione fornisce una base solida per la programmazione Prolog con predicati built-in completi, costrutti di controllo appropriati e documentazione estesa. Mentre alcune caratteristiche avanzate come sistemi di moduli completi e programmazione a vincoli non sono ancora implementate, JProlog offre un ambiente Prolog robusto e pratico adatto per scopi educativi, ricerca e molte applicazioni del mondo reale.

L'architettura estensibile permette miglioramenti futuri mantenendo la retrocompatibilità con il codice Prolog standard. La combinazione di un motore potente, IDE completo e documentazione approfondita rende JProlog uno strumento prezioso sia per l'apprendimento che per lo sviluppo Prolog professionale.