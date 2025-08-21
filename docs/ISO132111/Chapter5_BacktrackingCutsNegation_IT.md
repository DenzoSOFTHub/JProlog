# Capitolo 5: Backtracking, Tagli e Negazione

## Panoramica

Questo capitolo fornisce documentazione completa dell'implementazione di JProlog dei meccanismi di backtracking, operatori di taglio e negazione-come-fallimento. Questi costrutti di controllo formano le fondamenta del modello di esecuzione di Prolog e permettono sofisticato controllo del flusso del programma.

## 5.1 Backtracking e Tagli

### 5.1.1 Backtracking Rivisitato

#### Meccanismo Centrale di Backtracking

JProlog implementa il backtracking attraverso un meccanismo di ricerca sistematico che esplora soluzioni alternative quando il percorso corrente fallisce. Il meccanismo di backtracking è centralizzato nella classe `QuerySolver`.

```java
// QuerySolver.java - Implementazione centrale del backtracking
public class QuerySolver {
    
    /**
     * Risolve un goal con binding correnti, esplorando tutti i choice point.
     * 
     * @param goal Il goal da risolvere
     * @param bindings Binding delle variabili correnti
     * @param solutions Lista per aggiungere soluzioni riuscite
     * @param cutStatus Stato del controllo cut per il controllo del backtracking
     * @return true se sono state trovate soluzioni
     */
    public boolean solve(Term goal, Map<String, Term> bindings, 
                        List<Map<String, Term>> solutions, CutStatus cutStatus) {
        
        // Protezione ricorsione con limitazione di profondità
        Integer depth = recursionDepth.get();
        if (depth == null) depth = 0;
        
        if (depth > MAX_RECURSION_DEPTH) {
            System.err.println("ATTENZIONE: Profondità ricorsione massima " + 
                             MAX_RECURSION_DEPTH + " raggiunta per goal: " + goal);
            return false;
        }
        
        // Processa choice points sistematicamente
        return solveInternalProtected(goal, bindings, solutions, cutStatus);
    }
}
```

#### Gestione dei Choice Point

I choice point vengono creati quando multiple regole o fatti possono corrispondere a un goal. JProlog gestisce i choice point attraverso esplorazione sistematica:

```java
// Esempio: Fatti multipli creano choice point
// Database:
// colore(rosso).
// colore(blu). 
// colore(verde).

// Query: ?- colore(X).
// Crea choice point per X = rosso, X = blu, X = verde

public boolean solveGoal(Term goal, Map<String, Term> bindings, 
                        List<Map<String, Term>> solutions) {
    boolean foundSolution = false;
    
    // Prova tutte le clausole corrispondenti (crea choice point)
    for (Clause clause : knowledgeBase.getClauses(goal.getFunctor())) {
        // Crea nuovo contesto di binding per questo choice point
        Map<String, Term> newBindings = new HashMap<>(bindings);
        
        if (unifyAndSolve(goal, clause, newBindings, solutions)) {
            foundSolution = true;
            // Continua ad esplorare altri choice point a meno che non si incontra cut
        }
    }
    
    return foundSolution;
}
```

#### Binding e Unbinding delle Variabili

Durante il backtracking, i binding delle variabili devono essere gestiti correttamente:

```java
// Esempio: Backtracking con gestione binding variabili
// Goal: append([1,2], [3,4], X), member(Y, X).

public boolean solveConjunction(List<Term> goals, Map<String, Term> bindings,
                               List<Map<String, Term>> solutions) {
    if (goals.isEmpty()) {
        solutions.add(new HashMap<>(bindings));
        return true;
    }
    
    Term firstGoal = goals.get(0);
    List<Term> remainingGoals = goals.subList(1, goals.size());
    
    boolean success = false;
    List<Map<String, Term>> firstGoalSolutions = new ArrayList<>();
    
    // Risolve primo goal - può creare choice point multipli
    if (solve(firstGoal, bindings, firstGoalSolutions, CutStatus.notOccurred())) {
        
        // Per ogni soluzione del primo goal, prova i goal rimanenti
        for (Map<String, Term> solution : firstGoalSolutions) {
            // Chiamata ricorsiva con nuovi binding
            if (solveConjunction(remainingGoals, solution, solutions)) {
                success = true;
            }
            // Unbinding automatico al backtrack - nuova iterazione crea contesto fresco
        }
    }
    
    return success;
}
```

### 5.1.2 Problemi con il Backtracking

#### Cicli Infiniti

Il backtracking non vincolato può portare a cicli infiniti, specialmente con regole ricorsive a sinistra:

```prolog
% Regola ricorsiva a sinistra problematica
percorso(X, Y) :- percorso(X, Z), arco(Z, Y).
percorso(X, Y) :- arco(X, Y).

% Database
arco(a, b).
arco(b, c).

% Query: ?- percorso(a, c).
% Crea ricorsione infinita: percorso(a,c) -> percorso(a,Z1) -> percorso(a,Z2) -> ...
```

JProlog affronta questo con limitazione della profondità ricorsiva:

```java
// QuerySolver.java - Protezione ricorsione
private static final ThreadLocal<Integer> recursionDepth = new ThreadLocal<>();
private static final int MAX_RECURSION_DEPTH = 100;

private boolean solveInternal(Term goal, Map<String, Term> bindings, 
                             List<Map<String, Term>> solutions, CutStatus cutStatus) {
    Integer depth = recursionDepth.get();
    if (depth == null) depth = 0;
    
    if (depth > MAX_RECURSION_DEPTH) {
        System.err.println("ATTENZIONE: Profondità ricorsione massima " + 
                         MAX_RECURSION_DEPTH + " raggiunta per goal: " + goal);
        return false; // Previene stack overflow
    }
    
    try {
        recursionDepth.set(depth + 1);
        return solveInternalProtected(goal, bindings, solutions, cutStatus);
    } finally {
        // Pulizia corretta del tracking ricorsione
        if (depth == 0) {
            recursionDepth.remove();
        } else {
            recursionDepth.set(depth);
        }
    }
}
```

#### Problemi di Prestazioni

Il backtracking eccessivo può portare a complessità temporale esponenziale:

```prolog
% Esempio: Fibonacci con ricorsione naive
fib(0, 1).
fib(1, 1).
fib(N, F) :- 
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fib(N1, F1),
    fib(N2, F2),
    F is F1 + F2.

% Query: ?- fib(30, X).
% Crea numero esponenziale di choice point
```

#### Soluzioni Ridondanti

Percorsi multipli possono portare alla stessa soluzione:

```prolog
% Esempio: Percorsi multipli alla stessa soluzione
connesso(X, Y) :- arco(X, Y).
connesso(X, Y) :- arco(Y, X).
connesso(X, Z) :- connesso(X, Y), connesso(Y, Z).

% Database
arco(a, b).
arco(b, c).

% Query: ?- connesso(a, c).
% Può trovare: connesso(a,c) via arco(a,b), arco(b,c)
%              connesso(a,c) via arco(b,a), arco(b,c) [se interpretazione simmetrica]
```

### 5.1.3 Introduzione dei Tagli

#### Implementazione dell'Operatore Cut

L'operatore cut (!) previene il backtracking ai choice point creati prima di esso nella regola corrente:

```java
// Cut.java - Implementazione operatore cut
public class Cut implements BuiltIn {

    @Override
    public boolean execute(Term query, Map<String, Term> bindings, 
                          List<Map<String, Term>> solutions) {
        if (query.getArguments() != null && query.getArguments().size() != 0) {
            throw new PrologEvaluationException("cut/0 non accetta argomenti.");
        }
        
        // Cut ha sempre successo e aggiunge binding correnti
        solutions.add(new HashMap<>(bindings));
        
        // Il comportamento del cut è implementato da:
        // 1. Restituendo solo una soluzione (primo successo)  
        // 2. QuerySolver gestisce rilevamento cut per choice point delle regole
        
        return true;
    }
}
```

#### Semantica del Cut nel Processamento delle Regole

Il cut influenza il processamento di clausole alternative:

```java
// QuerySolver.java - Gestione cut nel processamento regole
private boolean solveGoal(Term goal, Map<String, Term> bindings,
                         List<Map<String, Term>> solutions, CutStatus cutStatus) {
    
    boolean foundSolution = false;
    List<Clause> matchingClauses = knowledgeBase.getClauses(goal.getFunctor());
    
    for (int i = 0; i < matchingClauses.size(); i++) {
        Clause clause = matchingClauses.get(i);
        Map<String, Term> clauseBindings = new HashMap<>(bindings);
        
        // Prova ad unificare e risolvere questa clausola
        if (unifyGoalWithClause(goal, clause, clauseBindings)) {
            
            // Crea stato cut mutabile per questa clausola
            MutableCutStatus mutableCutStatus = new MutableCutStatus();
            
            if (solveRuleBody(clause, clauseBindings, solutions, mutableCutStatus)) {
                foundSolution = true;
                
                // Controlla se è stato incontrato cut
                if (mutableCutStatus.wasCutEncountered()) {
                    // Cut previene backtracking alle clausole rimanenti
                    break;
                }
            }
        }
    }
    
    return foundSolution;
}
```

#### Cut Verde vs Cut Rosso

**Cut Verde** - Rimuove choice point ridondanti senza influenzare la correttezza:

```prolog
% Esempio cut verde - rimuove soluzioni ridondanti
max(X, Y, X) :- X >= Y, !.
max(X, Y, Y).

% Senza cut: ?- max(5, 3, Z). produce Z = 5 due volte
% Con cut:   ?- max(5, 3, Z). produce Z = 5 una volta
```

**Cut Rosso** - Cambia la semantica del programma e il significato logico:

```prolog
% Esempio cut rosso - cambia significato logico  
p(X) :- q(X), !, r(X).
p(X) :- s(X).

% Database
q(a). r(a).
q(b). 
s(b).

% Senza cut: ?- p(X). produrrebbe X = a, X = b
% Con cut:   ?- p(X). produce solo X = a
% Il cut previene di provare s(b) anche quando r(b) fallisce
```

#### Implementazione Cut con CutStatus

JProlog usa un sistema `CutStatus` per tracciare occorrenze di cut:

```java
// CutStatus.java - Interfaccia tracking cut
public interface CutStatus {
    boolean wasCutEncountered();
    
    static CutStatus occurred() {
        return new SimpleCutStatus(true);
    }
    
    static CutStatus notOccurred() {
        return new SimpleCutStatus(false);
    }
}

// MutableCutStatus.java - Tracking cut mutabile
public class MutableCutStatus implements CutStatus {
    private boolean cutEncountered = false;
    
    public void setCutEncountered() {
        this.cutEncountered = true;
    }
    
    @Override
    public boolean wasCutEncountered() {
        return cutEncountered;
    }
}
```

### 5.1.4 Problemi con i Tagli

#### Perdita di Purezza Logica

I tagli possono rendere i programmi procedurali piuttosto che dichiarativi:

```prolog
% Senza cut - relazione logica pura
membro(X, [X|_]).
membro(X, [_|T]) :- membro(X, T).

% Con cut - interpretazione procedurale
membro(X, [X|_]) :- !.
membro(X, [_|T]) :- membro(X, T).

% La versione con cut fallisce a generare tutte le soluzioni al backtracking
```

#### Dipendenza dall'Ordine

Il cut introduce dipendenza dall'ordinamento delle clausole:

```prolog
% Versione 1 - cut rende l'ordine clausole critico
classifica(X, negativo) :- X < 0, !.
classifica(X, zero) :- X =:= 0, !.
classifica(X, positivo).

% Versione 2 - clausole riordinate danno comportamento diverso
classifica(X, positivo).
classifica(X, negativo) :- X < 0, !.  % Questo cut ora irraggiungibile per X positivo
classifica(X, zero) :- X =:= 0, !.
```

#### Difficoltà di Debug

I tagli rendono il comportamento del programma più difficile da tracciare e debuggare:

```java
// Implementazione debug-friendly senza tagli
public boolean solveWithTrace(Term goal, Map<String, Term> bindings,
                             List<Map<String, Term>> solutions) {
    if (traceEnabled) {
        System.out.println("Provando goal: " + goal + " con binding: " + bindings);
    }
    
    // Tutti i choice point sono esplorati e visibili nel trace
    boolean success = solveAllChoicePoints(goal, bindings, solutions);
    
    if (traceEnabled) {
        System.out.println("Goal " + goal + " " + (success ? "riuscito" : "fallito") + 
                          " con " + solutions.size() + " soluzioni");
    }
    
    return success;
}
```

## 5.2 Negazione come Fallimento

### 5.2.1 L'Assunzione del Mondo Chiuso

JProlog implementa la negazione basata sull'Assunzione del Mondo Chiuso (CWA): se un fatto non può essere provato dalla knowledge base corrente, si assume che sia falso.

```java
// Esempio di ragionamento mondo chiuso
// Database:
// uccello(pettirosso).
// uccello(passero).

// Query: ?- \+ uccello(pinguino).
// Ha successo perché pinguino non può essere provato essere un uccello
// Questo assume che la knowledge base sia completa per il predicato uccello
```

#### Implementazione dell'Assunzione del Mondo Chiuso

```java
// NegationAsFailure.java - Implementazione CWA
public class NegationAsFailure implements BuiltInWithContext {
    
    @Override
    public boolean executeWithContext(QuerySolver solver, Term query, Map<String, Term> bindings, 
                                     List<Map<String, Term>> solutions) {
        
        Term goal = query.getArguments().get(0).resolveBindings(bindings);
        
        // Prova a risolvere il goal sotto CWA
        List<Map<String, Term>> goalSolutions = new ArrayList<>();
        boolean goalSucceeds = solver.solve(goal, new HashMap<>(bindings), 
                                          goalSolutions, CutStatus.notOccurred());
        
        // CWA: ha successo se il goal non può essere provato (fallisce)
        if (!goalSucceeds || goalSolutions.isEmpty()) {
            solutions.add(new HashMap<>(bindings));
            return true; // Goal non dimostrabile, quindi negazione ha successo
        } else {
            return false; // Goal dimostrabile, quindi negazione fallisce
        }
    }
}
```

#### Limitazioni dell'Assunzione del Mondo Chiuso

```prolog
% Esempio che mostra limitazioni CWA
mortale(X) :- umano(X).
umano(socrate).

% Query: ?- \+ mortale(aristotele).
% Ha successo sotto CWA, ma può essere incorretto se la conoscenza è incompleta
% Non possiamo provare che aristotele è mortale, ma potrebbe essere umano (sconosciuto)
```

### 5.2.2 L'Operatore \+

#### Sintassi e Semantica

L'operatore `\+` implementa la negazione come fallimento:

```prolog
% Sintassi: \+ Goal
% Semantica: Ha successo se Goal fallisce, fallisce se Goal ha successo

% Esempi:
?- \+ fail.        % Ha successo (fail sempre fallisce)
?- \+ true.        % Fallisce (true sempre ha successo)
?- \+ (1 = 2).     % Ha successo (unificazione fallisce)
?- \+ (1 = 1).     % Fallisce (unificazione ha successo)
```

#### Dettagli Implementativi

```java
// NegationAsFailure.java - Implementazione completa
public class NegationAsFailure implements BuiltInWithContext {
    
    private QuerySolver solver;
    
    public NegationAsFailure(QuerySolver solver) {
        this.solver = solver;
    }
    
    @Override
    public boolean executeWithContext(QuerySolver solver, Term query, 
                                     Map<String, Term> bindings, 
                                     List<Map<String, Term>> solutions) {
        
        // Valida struttura argomenti
        if (query.getArguments() == null || query.getArguments().size() != 1) {
            throw new IllegalArgumentException("\\+ richiede esattamente un argomento");
        }
        
        Term goal = query.getArguments().get(0).resolveBindings(bindings);
        
        // Crea contesto binding fresco per evitare inquinamento variabili
        List<Map<String, Term>> goalSolutions = new ArrayList<>();
        boolean goalSucceeds = solver.solve(goal, new HashMap<>(bindings), 
                                          goalSolutions, CutStatus.notOccurred());
        
        // Logica negazione come fallimento
        if (!goalSucceeds || goalSolutions.isEmpty()) {
            // Goal fallito - negazione ha successo con binding originali
            solutions.add(new HashMap<>(bindings));
            return true;
        } else {
            // Goal riuscito - negazione fallisce
            return false;
        }
    }
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, 
                          List<Map<String, Term>> solutions) {
        throw new UnsupportedOperationException("\\+ richiede contesto");
    }
}
```

#### Scoping delle Variabili nella Negazione

Le variabili in goal negati hanno regole di scoping speciali:

```prolog
% Esempio: Scoping variabili nella negazione
?- \+ (X = 1), X = 2.
% Ha successo con X = 2
% Il binding X = 1 dentro \+ non esce

% L'implementazione assicura corretta isolazione variabili:
```

```java
// Isolazione variabili nella negazione
public boolean executeNegation(Term goal, Map<String, Term> bindings,
                              List<Map<String, Term>> solutions) {
    // Crea contesto binding isolato
    Map<String, Term> isolatedBindings = new HashMap<>(bindings);
    List<Map<String, Term>> goalSolutions = new ArrayList<>();
    
    boolean goalSucceeds = solver.solve(goal, isolatedBindings, goalSolutions, 
                                       CutStatus.notOccurred());
    
    if (!goalSucceeds) {
        // Restituisce binding originali (non quelli isolati)
        solutions.add(new HashMap<>(bindings));
        return true;
    }
    
    return false;
}
```

## 5.3 Disgiunzione

### 5.3.1 Operatore Punto e Virgola (;)

L'operatore punto e virgola implementa la disgiunzione (logica OR):

```prolog
% Sintassi: Goal1 ; Goal2
% Semantica: Ha successo se Goal1 OR Goal2 ha successo
% Entrambi i goal sono provati, raccogliendo tutte le soluzioni

% Esempi:
?- (X = 1 ; X = 2).     % X = 1, X = 2
?- (fail ; true).       % Ha successo
?- (true ; fail).       % Ha successo
```

#### Implementazione di Disgiunzione Semplice

```java
// IfThenElse.java - Implementazione disgiunzione
private boolean executeDisjunction(Term leftTerm, Term rightTerm, 
                                 Map<String, Term> bindings, 
                                 List<Map<String, Term>> solutions, 
                                 QuerySolver solver) {
    boolean success = false;
    
    // Prova prima alternativa sinistra
    List<Map<String, Term>> leftSolutions = new ArrayList<>();
    boolean leftSuccess = solver.solve(leftTerm, new HashMap<>(bindings), 
                                      leftSolutions, CutStatus.notOccurred());
    if (leftSuccess) {
        solutions.addAll(leftSolutions);
        success = true;
    }
    
    // Prova alternativa destra
    List<Map<String, Term>> rightSolutions = new ArrayList<>();
    boolean rightSuccess = solver.solve(rightTerm, new HashMap<>(bindings), 
                                       rightSolutions, CutStatus.notOccurred());
    if (rightSuccess) {
        solutions.addAll(rightSolutions);
        success = true;
    }
    
    return success;
}
```

### 5.3.2 Costrutto If-Then-Else

Il costrutto if-then-else combina esecuzione condizionale con disgiunzione:

```prolog
% Sintassi: (Condizione -> Allora ; Altrimenti)
% Semantica: Se Condizione ha successo, esegui Allora; altrimenti esegui Altrimenti

% Esempi:
?- (1 = 1 -> write(vero) ; write(falso)).    % Output: vero
?- (1 = 2 -> write(vero) ; write(falso)).    % Output: falso
```

#### Implementazione di If-Then-Else

```java
// IfThenElse.java - Implementazione completa if-then-else
public class IfThenElse implements BuiltInWithContext {
    
    private boolean executeIfThenElse(Term condition, Term thenTerm, Term elseTerm, 
                                    Map<String, Term> bindings, 
                                    List<Map<String, Term>> solutions, 
                                    QuerySolver solver) {
        
        // Prova a risolvere la condizione
        List<Map<String, Term>> conditionSolutions = new ArrayList<>();
        boolean conditionSuccess = solver.solve(condition, new HashMap<>(bindings), 
                                               conditionSolutions, CutStatus.notOccurred());
        
        if (conditionSuccess && !conditionSolutions.isEmpty()) {
            // Condizione riuscita - esegui parte Allora per ogni soluzione
            boolean success = false;
            for (Map<String, Term> conditionBinding : conditionSolutions) {
                List<Map<String, Term>> thenSolutions = new ArrayList<>();
                boolean thenSuccess = solver.solve(thenTerm, new HashMap<>(conditionBinding), 
                                                 thenSolutions, CutStatus.notOccurred());
                if (thenSuccess) {
                    solutions.addAll(thenSolutions);
                    success = true;
                }
            }
            return success;
        } else {
            // Condizione fallita - esegui parte Altrimenti
            List<Map<String, Term>> elseSolutions = new ArrayList<>();
            boolean elseSuccess = solver.solve(elseTerm, new HashMap<>(bindings), 
                                             elseSolutions, CutStatus.notOccurred());
            if (elseSuccess) {
                solutions.addAll(elseSolutions);
            }
            return elseSuccess;
        }
    }
}
```

#### If-Then-Else Deterministico vs Non-Deterministico

```prolog
% Deterministico: Condizione si impegna alla prima soluzione
?- (member(X, [1,2,3]) -> Y = X ; Y = nessuno).
% Risultati: Y = 1 (si impegna alla prima soluzione di member/2)

% Versione non-deterministica richiederebbe:
?- member(X, [1,2,3]), Y = X.
% Risultati: Y = 1, Y = 2, Y = 3 (tutte le soluzioni)
```

## 5.4 Esempio: Valutazione di Formule Logiche

### 5.4.1 Rappresentazione di Formule Booleane

```prolog
% Rappresenta formule booleane come termini Prolog
% and(X, Y)     - AND logico
% or(X, Y)      - OR logico  
% not(X)        - NOT logico
% true, false   - Costanti booleane
% Variabili: p, q, r, etc.

% Formule di esempio:
% F1 = and(p, or(q, not(r)))
% F2 = or(and(p, q), and(not(p), r))
```

### 5.4.2 Implementazione Valutazione Formule

```prolog
% evaluate(Formula, Assegnamento, Risultato)
% Valuta Formula sotto Assegnamento variabili, producendo Risultato

% Casi base - costanti
evaluate(true, _, true).
evaluate(false, _, false).

% Ricerca variabile
evaluate(Var, Assignment, Value) :-
    atom(Var),
    member(Var = Value, Assignment).

% Operatori logici
evaluate(and(X, Y), Assignment, Result) :-
    evaluate(X, Assignment, XVal),
    evaluate(Y, Assignment, YVal),
    and_truth(XVal, YVal, Result).

evaluate(or(X, Y), Assignment, Result) :-
    evaluate(X, Assignment, XVal),
    evaluate(Y, Assignment, YVal), 
    or_truth(XVal, YVal, Result).

evaluate(not(X), Assignment, Result) :-
    evaluate(X, Assignment, XVal),
    not_truth(XVal, Result).

% Tabelle verità
and_truth(true, true, true).
and_truth(true, false, false).
and_truth(false, true, false).
and_truth(false, false, false).

or_truth(true, true, true).
or_truth(true, false, true).
or_truth(false, true, true).
or_truth(false, false, false).

not_truth(true, false).
not_truth(false, true).
```

### 5.4.3 Implementazione Java

```java
// LogicFormulaEvaluator.java - Implementazione Java per JProlog
public class LogicFormulaEvaluator implements BuiltIn {
    
    @Override
    public boolean execute(Term query, Map<String, Term> bindings, 
                          List<Map<String, Term>> solutions) {
        
        // evaluate(Formula, Assegnamento, Risultato)
        if (query.getArguments().size() != 3) {
            throw new PrologEvaluationException("evaluate/3 richiede 3 argomenti");
        }
        
        Term formula = query.getArguments().get(0).resolveBindings(bindings);
        Term assignment = query.getArguments().get(1).resolveBindings(bindings);
        Term result = query.getArguments().get(2);
        
        try {
            Term evaluatedResult = evaluateFormula(formula, assignment);
            
            // Prova ad unificare con risultato
            Map<String, Term> newBindings = new HashMap<>(bindings);
            if (result.unify(evaluatedResult, newBindings)) {
                solutions.add(newBindings);
                return true;
            }
            
        } catch (EvaluationException e) {
            // Valutazione formula fallita
            return false;
        }
        
        return false;
    }
    
    private Term evaluateFormula(Term formula, Term assignment) throws EvaluationException {
        
        // Gestisce costanti
        if (formula instanceof Atom) {
            String name = ((Atom) formula).getName();
            if ("true".equals(name)) return new Atom("true");
            if ("false".equals(name)) return new Atom("false");
            
            // Ricerca variabile in assegnamento
            return lookupVariable(formula, assignment);
        }
        
        // Gestisce formule composte
        if (formula instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) formula;
            String functor = compound.getFunctor().getName();
            
            switch (functor) {
                case "and":
                    return evaluateAnd(compound.getArguments(), assignment);
                case "or":
                    return evaluateOr(compound.getArguments(), assignment);
                case "not":
                    return evaluateNot(compound.getArguments(), assignment);
                default:
                    throw new EvaluationException("Operatore sconosciuto: " + functor);
            }
        }
        
        throw new EvaluationException("Formula invalida: " + formula);
    }
    
    private Term evaluateAnd(List<Term> args, Term assignment) throws EvaluationException {
        if (args.size() != 2) {
            throw new EvaluationException("and/2 richiede esattamente 2 argomenti");
        }
        
        Term left = evaluateFormula(args.get(0), assignment);
        Term right = evaluateFormula(args.get(1), assignment);
        
        boolean leftTrue = isTrue(left);
        boolean rightTrue = isTrue(right);
        
        return new Atom(leftTrue && rightTrue ? "true" : "false");
    }
    
    private Term evaluateOr(List<Term> args, Term assignment) throws EvaluationException {
        if (args.size() != 2) {
            throw new EvaluationException("or/2 richiede esattamente 2 argomenti");
        }
        
        Term left = evaluateFormula(args.get(0), assignment);
        Term right = evaluateFormula(args.get(1), assignment);
        
        boolean leftTrue = isTrue(left);
        boolean rightTrue = isTrue(right);
        
        return new Atom(leftTrue || rightTrue ? "true" : "false");
    }
    
    private Term evaluateNot(List<Term> args, Term assignment) throws EvaluationException {
        if (args.size() != 1) {
            throw new EvaluationException("not/1 richiede esattamente 1 argomento");
        }
        
        Term operand = evaluateFormula(args.get(0), assignment);
        boolean operandTrue = isTrue(operand);
        
        return new Atom(!operandTrue ? "true" : "false");
    }
    
    private boolean isTrue(Term term) {
        return term instanceof Atom && "true".equals(((Atom) term).getName());
    }
}
```

### 5.4.4 Esempio di Utilizzo

```prolog
% Esempio 1: Valutazione AND semplice
?- evaluate(and(true, false), [], Result).
Result = false.

% Esempio 2: Assegnamento variabili
?- evaluate(and(p, q), [p = true, q = false], Result).
Result = false.

% Esempio 3: Formula complessa
?- evaluate(or(and(p, q), not(p)), [p = true, q = false], Result).
Result = false.  % (true AND false) OR (NOT true) = false OR false = false

% Esempio 4: Controllo tautologia
?- evaluate(or(p, not(p)), [p = true], Result).
Result = true.

?- evaluate(or(p, not(p)), [p = false], Result).  
Result = true.
```

### 5.4.5 Applicazioni nell'Analisi di Programmi

Il valutatore di formule logiche può essere usato per analisi di programmi:

```prolog
% Controlla se formula è soddisfacibile
soddisfacibile(Formula) :-
    assegnamento_variabili(Assegnamento),
    evaluate(Formula, Assegnamento, true).

% Genera tutti gli assegnamenti variabili
assegnamento_variabili([]).
assegnamento_variabili([Var = true | Rest]) :-
    assegnamento_variabili(Rest).
assegnamento_variabili([Var = false | Rest]) :-
    assegnamento_variabili(Rest).

% Controlla se formula è tautologia
tautologia(Formula) :-
    \+ (assegnamento_variabili(Assegnamento),
        evaluate(Formula, Assegnamento, false)).

% Esempio: Controlla tautologia
?- tautologia(or(p, not(p))).     % Si - sempre vero
?- tautologia(and(p, not(p))).    % No - contraddizione
```

## 5.5 Costrutti di Controllo Avanzati

### 5.5.1 Predicato Once

Il predicato `once/1` esegue il suo argomento esattamente una volta, tagliando i choice point:

```java
// Once.java - Implementazione di once/1
public class Once implements BuiltInWithContext {
    
    @Override
    public boolean executeWithContext(QuerySolver solver, Term query, 
                                     Map<String, Term> bindings,
                                     List<Map<String, Term>> solutions) {
        
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("once/1 richiede esattamente un argomento");
        }
        
        Term goal = query.getArguments().get(0).resolveBindings(bindings);
        
        // Risolve goal ma prende solo prima soluzione
        List<Map<String, Term>> goalSolutions = new ArrayList<>();
        boolean success = solver.solve(goal, new HashMap<>(bindings), 
                                     goalSolutions, CutStatus.notOccurred());
        
        if (success && !goalSolutions.isEmpty()) {
            // Prende solo prima soluzione - implementa semantica "once"
            solutions.add(goalSolutions.get(0));
            return true;
        }
        
        return false;
    }
}
```

### 5.5.2 Predicato Ignore

Il predicato `ignore/1` ha sempre successo, sia che il suo argomento abbia successo o fallisca:

```java
// Ignore.java - Implementazione di ignore/1
public class Ignore implements BuiltInWithContext {
    
    @Override
    public boolean executeWithContext(QuerySolver solver, Term query,
                                     Map<String, Term> bindings,
                                     List<Map<String, Term>> solutions) {
        
        if (query.getArguments().size() != 1) {
            throw new PrologEvaluationException("ignore/1 richiede esattamente un argomento");
        }
        
        Term goal = query.getArguments().get(0).resolveBindings(bindings);
        
        // Prova a risolvere goal
        List<Map<String, Term>> goalSolutions = new ArrayList<>();
        boolean success = solver.solve(goal, new HashMap<>(bindings),
                                     goalSolutions, CutStatus.notOccurred());
        
        if (success && !goalSolutions.isEmpty()) {
            // Goal riuscito - usa le sue soluzioni
            solutions.addAll(goalSolutions);
        } else {
            // Goal fallito - ignora fallimento e ha successo con binding originali
            solutions.add(new HashMap<>(bindings));
        }
        
        return true; // ignore/1 ha sempre successo
    }
}
```

## Conclusione

Questo capitolo ha coperto i costrutti di controllo essenziali in JProlog:

1. **Backtracking**: Esplorazione sistematica di spazi soluzioni con gestione appropriata dei choice point e protezione ricorsione
2. **Operatore Cut**: Prevenzione del backtracking con distinzione attenta tra tagli verdi e rossi
3. **Negazione come Fallimento**: Implementazione di ragionamento mondo-chiuso con scoping appropriato delle variabili
4. **Disgiunzione**: Supporto per percorsi di esecuzione alternativi attraverso operatore punto e virgola e costrutti if-then-else
5. **Costrutti Avanzati**: Meta-predicati come `once/1` e `ignore/1` per controllo di flusso specializzato

Questi costrutti forniscono le fondamenta per programmazione Prolog sofisticata mantenendo la natura dichiarativa del linguaggio dove possibile. L'implementazione enfatizza correttezza, prestazioni e supporto al debugging.

---

*Questa documentazione riflette l'implementazione di JProlog dei costrutti di controllo ISO Prolog con ottimizzazioni ed estensioni specifiche per Java.*