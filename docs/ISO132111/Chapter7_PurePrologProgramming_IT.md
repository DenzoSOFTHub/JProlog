# Capitolo 7: Programmazione in Prolog Puro - Principi di Programmazione JProlog

## Panoramica

Questo capitolo esplora i principi fondamentali della programmazione in Prolog puro utilizzando JProlog, concentrandosi sugli aspetti dichiarativi della programmazione logica senza effetti collaterali o predicati extra-logici. La programmazione Prolog pura enfatizza le relazioni logiche, l'uso appropriato dell'unificazione e la risoluzione dichiarativa dei problemi. Questo capitolo copre l'ordine delle regole, le condizioni di terminazione, le strategie di ordinamento obiettivi, la gestione di soluzioni ridondanti, le tecniche di programmazione ricorsiva e i fondamenti teorici che rendono Prolog un potente linguaggio di programmazione logica.

Il Prolog puro rappresenta l'essenza della programmazione dichiarativa dove i programmi descrivono quali relazioni valgono piuttosto che come calcolarle. Comprendere questi principi è cruciale per scrivere programmi Prolog efficienti, manutenibili e logicamente corretti.

## 7.1 Ordine delle Regole

L'ordine delle regole in Prolog influenza significativamente l'esecuzione del programma, la strategia di ricerca e la scoperta delle soluzioni. In JProlog, come nel Prolog standard, le clausole vengono provate nell'ordine in cui appaiono nel programma, rendendo l'ordinamento delle regole una considerazione di design cruciale.

### 7.1.1 Strategia di Selezione Clausole

JProlog implementa la strategia standard di selezione clausole Prolog: le regole vengono provate dall'alto verso il basso, e gli obiettivi da sinistra a destra.

#### Implementazione dell'Ordinamento Regole in JProlog

```java
/**
 * Ordinamento regole e selezione clausole nel QuerySolver JProlog
 */
public class QuerySolver {
    
    /**
     * Risolve query usando risoluzione SLD con ordinamento clausole appropriato
     */
    public Iterator<Substitution> solve(Term goal, ExecutionContext context) {
        String predicateIndicator = getPredicateIndicator(goal);
        List<Clause> clauses = context.getKnowledgeBase().getClauses(predicateIndicator);
        
        return new ClauseIterator(clauses, goal, context);
    }
    
    /**
     * Iterator che mantiene ordinamento clausole appropriato
     */
    private class ClauseIterator implements Iterator<Substitution> {
        private final List<Clause> clauses;
        private final Term goal;
        private final ExecutionContext context;
        private int currentClauseIndex;
        private Iterator<Substitution> currentSolutionIterator;
        
        public ClauseIterator(List<Clause> clauses, Term goal, ExecutionContext context) {
            this.clauses = clauses;
            this.goal = goal;
            this.context = context;
            this.currentClauseIndex = 0;
            this.currentSolutionIterator = Collections.emptyIterator();
        }
        
        @Override
        public boolean hasNext() {
            // Prova prima soluzioni clausola corrente
            if (currentSolutionIterator.hasNext()) {
                return true;
            }
            
            // Passa alla clausola successiva
            while (currentClauseIndex < clauses.size()) {
                Clause clause = clauses.get(currentClauseIndex++);
                
                // Crea copia fresca con variabili rinominate
                Map<Variable, Variable> variableMap = new HashMap<>();
                Clause freshClause = clause.createFreshCopy(variableMap);
                
                // Prova a unificare obiettivo con testa clausola
                Substitution unificationSubst = new Substitution(context.getSubstitution());
                if (goal.unify(freshClause.getHead(), unificationSubst)) {
                    
                    if (freshClause.getBody() == null) {
                        // Fatto - soluzione diretta
                        currentSolutionIterator = Collections.singletonList(unificationSubst).iterator();
                    } else {
                        // Regola - risolvi corpo con nuova sostituzione
                        ExecutionContext newContext = context.withSubstitution(unificationSubst);
                        currentSolutionIterator = solve(freshClause.getBody(), newContext);
                    }
                    
                    if (currentSolutionIterator.hasNext()) {
                        return true;
                    }
                }
            }
            
            return false;
        }
        
        @Override
        public Substitution next() {
            if (!hasNext()) {
                throw new NoSuchElementException();
            }
            return currentSolutionIterator.next();
        }
    }
}
```

### 7.1.2 Impatto dell'Ordine Regole sul Comportamento Programma

L'ordine delle regole influenza fondamentalmente il comportamento del programma, l'efficienza e le proprietà di terminazione.

#### Esempio: Ordini Regole Diversi

```prolog
% Versione 1: Caso base primo (raccomandato)
factorial(0, 1).
factorial(N, F) :- 
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.

% Versione 2: Caso ricorsivo primo (problematico)
factorial(N, F) :- 
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.
factorial(0, 1).
```

#### Analisi JProlog Impatto Ordine Regole

```java
/**
 * Strumento di analisi per l'impatto dell'ordine regole in JProlog
 */
public class RuleOrderAnalyzer {
    
    /**
     * Analizza l'impatto dell'ordinamento regole sull'esecuzione
     */
    public void analyzeRuleOrder(String predicateIndicator, KnowledgeBase kb) {
        List<Clause> clauses = kb.getClauses(predicateIndicator);
        
        System.out.println("Analisi ordine regole per " + predicateIndicator + ":");
        System.out.println("Clausole totali: " + clauses.size());
        
        for (int i = 0; i < clauses.size(); i++) {
            Clause clause = clauses.get(i);
            System.out.println("Regola " + (i+1) + ": " + formatClause(clause));
            
            // Analizza proprietà terminazione
            if (isBaseCase(clause)) {
                System.out.println("  -> Caso base (termina immediatamente)");
            } else if (isRecursiveCase(clause)) {
                System.out.println("  -> Caso ricorsivo (può non terminare senza caso base)");
            }
            
            // Analizza ordinamento obiettivi dentro corpo regola
            if (clause.getBody() != null) {
                analyzeGoalOrder(clause.getBody());
            }
        }
        
        // Controlla ordinamento ottimale
        checkOptimalOrdering(clauses);
    }
    
    /**
     * Determina se clausola è caso base
     */
    private boolean isBaseCase(Clause clause) {
        if (clause.getBody() == null) {
            return true; // Il fatto è sempre caso base
        }
        
        // Controlla se corpo non contiene chiamate ricorsive
        return !containsRecursiveCall(clause.getBody(), clause.getHead());
    }
    
    /**
     * Controlla se corpo contiene chiamate ricorsive
     */
    private boolean containsRecursiveCall(Term body, Term head) {
        if (body.isCompound()) {
            CompoundTerm compound = (CompoundTerm) body;
            String headFunctor = getMainFunctor(head);
            
            if (",".equals(compound.getFunctor())) {
                // Congiunzione - controlla entrambi i lati
                return containsRecursiveCall(compound.getArgument(1), head) ||
                       containsRecursiveCall(compound.getArgument(2), head);
            } else {
                // Controlla se questo obiettivo è ricorsivo
                return headFunctor.equals(getMainFunctor(body));
            }
        }
        return false;
    }
    
    /**
     * Controlla ordinamento regole ottimale
     */
    private void checkOptimalOrdering(List<Clause> clauses) {
        boolean foundBase = false;
        boolean foundRecursive = false;
        
        for (Clause clause : clauses) {
            if (isBaseCase(clause)) {
                if (foundRecursive) {
                    System.out.println("AVVISO: Caso base dopo caso ricorsivo - può influenzare efficienza");
                }
                foundBase = true;
            } else {
                foundRecursive = true;
            }
        }
        
        if (!foundBase) {
            System.out.println("AVVISO: Nessun caso base trovato - ricorsione infinita probabile");
        }
        
        if (foundBase && foundRecursive) {
            System.out.println("INFO: Presenti sia casi base che ricorsivi - struttura buona");
        }
    }
}
```

### 7.1.3 Migliori Pratiche per Ordinamento Regole

#### Strategia Ordinamento Regole Ottimale

1. **Casi base primi**: Metti condizioni di terminazione prima dei casi ricorsivi
2. **Più specifici primi**: Metti regole più specifiche prima di quelle generali
3. **Più probabili primi**: Metti regole usate frequentemente prima di casi rari
4. **Deterministici primi**: Metti regole deterministiche prima di quelle non-deterministiche

#### Esempio: Elaborazione Liste con Ordine Ottimale

```prolog
% Ordinamento ottimale: caso base primo
list_length([], 0).
list_length([_|T], N) :-
    list_length(T, N1),
    N is N1 + 1.

% Appartenenza lista con ordinamento ottimale
member(X, [X|_]).           % Caso base: elemento trovato
member(X, [_|T]) :-         % Caso ricorsivo: cerca nella coda
    member(X, T).

% Append con ordinamento ottimale  
append([], L, L).           % Caso base: prima lista vuota
append([H|T], L, [H|R]) :-  % Caso ricorsivo: sposta testa
    append(T, L, R).
```

#### Ottimizzatore Ordine Regole JProlog

```java
/**
 * Suggerimenti ottimizzazione ordine regole per JProlog
 */
public class RuleOrderOptimizer {
    
    /**
     * Suggerisci ordinamento regole ottimale
     */
    public List<Clause> optimizeRuleOrder(List<Clause> originalClauses) {
        List<Clause> optimized = new ArrayList<>();
        List<Clause> baseCases = new ArrayList<>();
        List<Clause> recursiveCases = new ArrayList<>();
        
        // Separa casi base da casi ricorsivi
        for (Clause clause : originalClauses) {
            if (isBaseCase(clause)) {
                baseCases.add(clause);
            } else {
                recursiveCases.add(clause);
            }
        }
        
        // Ordina casi base per specificità (più specifici primi)
        baseCases.sort(this::compareSpecificity);
        
        // Ordina casi ricorsivi per complessità (più semplici primi)
        recursiveCases.sort(this::compareComplexity);
        
        // Combina: casi base primi, poi casi ricorsivi
        optimized.addAll(baseCases);
        optimized.addAll(recursiveCases);
        
        return optimized;
    }
    
    /**
     * Compara specificità clausole (più specifico = valore minore)
     */
    private int compareSpecificity(Clause c1, Clause c2) {
        // Conta termini ground nella testa (più ground = più specifico)
        int groundCount1 = countGroundTerms(c1.getHead());
        int groundCount2 = countGroundTerms(c2.getHead());
        
        return Integer.compare(groundCount2, groundCount1); // Ordine discendente
    }
    
    /**
     * Compara complessità clausole (più semplice = valore minore)
     */
    private int compareComplexity(Clause c1, Clause c2) {
        int complexity1 = calculateComplexity(c1);
        int complexity2 = calculateComplexity(c2);
        
        return Integer.compare(complexity1, complexity2); // Ordine ascendente
    }
    
    private int calculateComplexity(Clause clause) {
        if (clause.getBody() == null) return 0;
        return countGoals(clause.getBody());
    }
}
```

## 7.2 Terminazione

La terminazione è un aspetto critico della programmazione Prolog puro. A differenza dei linguaggi imperativi, i programmi Prolog potrebbero non terminare a causa di ricorsioni infinite o spazi di ricerca infiniti.

### 7.2.1 Condizioni di Terminazione

Un programma Prolog termina quando tutte le possibili derivazioni o riescono finitamente o falliscono finitamente, senza entrare in loop infiniti.

#### Analisi Terminazione in JProlog

```java
/**
 * Analisi terminazione per programmi JProlog
 */
public class TerminationAnalyzer {
    
    private final Set<String> analyzedPredicates = new HashSet<>();
    private final Map<String, TerminationResult> results = new HashMap<>();
    
    /**
     * Analizza proprietà terminazione di un predicato
     */
    public TerminationResult analyzeTermination(String predicateIndicator, KnowledgeBase kb) {
        if (analyzedPredicates.contains(predicateIndicator)) {
            return results.get(predicateIndicator);
        }
        
        analyzedPredicates.add(predicateIndicator);
        
        List<Clause> clauses = kb.getClauses(predicateIndicator);
        TerminationResult result = new TerminationResult(predicateIndicator);
        
        // Analizza ogni clausola
        for (Clause clause : clauses) {
            ClauseTerminationInfo info = analyzeClause(clause, kb);
            result.addClauseInfo(info);
        }
        
        // Determina proprietà terminazione generale
        result.determineOverallTermination();
        results.put(predicateIndicator, result);
        
        return result;
    }
    
    /**
     * Analizza terminazione clausola individuale
     */
    private ClauseTerminationInfo analyzeClause(Clause clause, KnowledgeBase kb) {
        ClauseTerminationInfo info = new ClauseTerminationInfo(clause);
        
        if (clause.getBody() == null) {
            // Il fatto termina sempre
            info.setTerminationStatus(TerminationStatus.ALWAYS_TERMINATES);
            return info;
        }
        
        // Analizza struttura ricorsiva
        String headPredicateIndicator = getPredicateIndicator(clause.getHead());
        
        if (containsRecursiveCall(clause.getBody(), headPredicateIndicator)) {
            info.setRecursive(true);
            
            // Controlla argomenti decrescenti (condizione terminazione)
            if (hasDecreasingArguments(clause)) {
                info.setTerminationStatus(TerminationStatus.LIKELY_TERMINATES);
            } else {
                info.setTerminationStatus(TerminationStatus.MAY_NOT_TERMINATE);
            }
        } else {
            // Clausola non ricorsiva
            info.setTerminationStatus(TerminationStatus.ALWAYS_TERMINATES);
        }
        
        return info;
    }
    
    /**
     * Controlla se clausola ha argomenti decrescenti che assicurano terminazione
     */
    private boolean hasDecreasingArguments(Clause clause) {
        // Euristica semplice: controlla decrementazione aritmetica
        if (clause.getBody() instanceof CompoundTerm) {
            CompoundTerm body = (CompoundTerm) clause.getBody();
            
            // Cerca pattern come: N1 is N - 1
            if (containsDecrementPattern(body)) {
                return true;
            }
            
            // Cerca ricorsione strutturale su liste [H|T] -> T
            if (containsStructuralRecursion(clause)) {
                return true;
            }
        }
        
        return false;
    }
    
    /**
     * Controlla pattern ricorsione strutturale
     */
    private boolean containsStructuralRecursion(Clause clause) {
        Term head = clause.getHead();
        if (head instanceof CompoundTerm) {
            CompoundTerm headCompound = (CompoundTerm) head;
            
            // Controlla ogni argomento per pattern lista [H|T]
            for (int i = 1; i <= headCompound.getArity(); i++) {
                Term arg = headCompound.getArgument(i);
                if (isListPattern(arg)) {
                    // Controlla se chiamata ricorsiva usa coda
                    if (recursiveCallUsesTail(clause.getBody(), arg)) {
                        return true;
                    }
                }
            }
        }
        
        return false;
    }
    
    private boolean isListPattern(Term term) {
        if (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            return ".".equals(compound.getFunctor()) && compound.getArity() == 2;
        }
        return false;
    }
}

/**
 * Risultato terminazione con informazioni analisi
 */
public class TerminationResult {
    public enum TerminationStatus {
        ALWAYS_TERMINATES,
        LIKELY_TERMINATES,
        MAY_NOT_TERMINATE,
        NEVER_TERMINATES
    }
    
    private final String predicateIndicator;
    private final List<ClauseTerminationInfo> clauseInfos;
    private TerminationStatus overallStatus;
    
    public TerminationResult(String predicateIndicator) {
        this.predicateIndicator = predicateIndicator;
        this.clauseInfos = new ArrayList<>();
    }
    
    public void determineOverallTermination() {
        boolean hasTerminatingClause = clauseInfos.stream()
            .anyMatch(info -> info.getTerminationStatus() == TerminationStatus.ALWAYS_TERMINATES);
        
        boolean hasNonTerminatingClause = clauseInfos.stream()
            .anyMatch(info -> info.getTerminationStatus() == TerminationStatus.MAY_NOT_TERMINATE);
        
        if (hasTerminatingClause && !hasNonTerminatingClause) {
            overallStatus = TerminationStatus.ALWAYS_TERMINATES;
        } else if (hasTerminatingClause && hasNonTerminatingClause) {
            overallStatus = TerminationStatus.LIKELY_TERMINATES;
        } else if (hasNonTerminatingClause) {
            overallStatus = TerminationStatus.MAY_NOT_TERMINATE;
        } else {
            overallStatus = TerminationStatus.ALWAYS_TERMINATES;
        }
    }
}
```

### 7.2.2 Problemi Terminazione Comuni

#### Esempi Ricorsione Infinita

```prolog
% PROBLEMA: Nessun caso base - ricorsione infinita
factorial(N, F) :- 
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.

% SOLUZIONE: Aggiungere caso base
factorial(0, 1).
factorial(N, F) :- 
    N > 0,
    N1 is N - 1,
    factorial(N1, F1),
    F is N * F1.

% PROBLEMA: Nessun argomento decrescente
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- ancestor(X, Z), ancestor(Z, Y).  % Può loopare infinitamente

% SOLUZIONE: Usare approccio diverso o aggiungere cut/memoizzazione
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).  % Meglio: assicurare Z sia intermedio
```

#### Controllo Terminazione JProlog

```java
/**
 * Rilevamento terminazione runtime per JProlog
 */
public class RuntimeTerminationChecker {
    
    private final Map<String, Integer> recursionDepth = new HashMap<>();
    private final int maxRecursionDepth;
    
    public RuntimeTerminationChecker(int maxDepth) {
        this.maxRecursionDepth = maxDepth;
    }
    
    /**
     * Controlla potenziale ricorsione infinita durante esecuzione
     */
    public void checkRecursionDepth(String predicateIndicator, ExecutionContext context) 
            throws InfiniteRecursionException {
        
        String key = predicateIndicator + "@" + context.getCallStack().size();
        int depth = recursionDepth.getOrDefault(key, 0) + 1;
        recursionDepth.put(key, depth);
        
        if (depth > maxRecursionDepth) {
            throw new InfiniteRecursionException(
                "Possibile ricorsione infinita rilevata in " + predicateIndicator +
                " (profondità: " + depth + ")"
            );
        }
        
        // Pulisci quando ritorni dalla ricorsione
        context.addCleanupAction(() -> {
            int newDepth = recursionDepth.get(key) - 1;
            if (newDepth <= 0) {
                recursionDepth.remove(key);
            } else {
                recursionDepth.put(key, newDepth);
            }
        });
    }
}
```

### 7.2.3 Assicurare Terminazione

#### Tecniche per Terminazione Garantita

1. **Ricorsione Strutturale**: Ricorri su argomenti strutturalmente più piccoli
2. **Relazioni Ben Fondate**: Usa relazioni con catene decrescenti finite
3. **Restrizioni Modi**: Limita chiamate ricorsive ad argomenti decrescenti
4. **Rilevamento Loop**: Rileva e previeni loop infiniti

#### Esempio: Operazioni Liste Terminanti

```prolog
% Termina sempre - ricorsione strutturale su lista
list_sum([], 0).
list_sum([H|T], Sum) :-
    list_sum(T, TailSum),
    Sum is H + TailSum.

% Termina sempre - argomento numerico decrescente
countdown(0).
countdown(N) :-
    N > 0,
    N1 is N - 1,
    countdown(N1).

% Potenzialmente non terminante - argomento numerico crescente
count_up(N) :-
    write(N), nl,
    N1 is N + 1,
    count_up(N1).  % Nessun limite superiore - ricorsione infinita!
```

## 7.3 Ordine Obiettivi

L'ordinamento obiettivi dentro corpi delle regole influenza significativamente l'efficienza di esecuzione, le proprietà di terminazione e la generazione soluzioni nei programmi Prolog.

### 7.3.1 Esecuzione Obiettivi Sinistra-Destra

JProlog segue l'esecuzione obiettivi Prolog standard: gli obiettivi nei corpi delle regole vengono eseguiti da sinistra a destra, creando una strategia di ricerca depth-first.

#### Analisi Impatto Ordine Obiettivi

```java
/**
 * Analisi ordine obiettivi per esecuzione JProlog
 */
public class GoalOrderAnalyzer {
    
    /**
     * Analizza impatto ordinamento obiettivi nel corpo regola
     */
    public GoalOrderAnalysis analyzeGoalOrder(Clause clause) {
        GoalOrderAnalysis analysis = new GoalOrderAnalysis(clause);
        
        if (clause.getBody() == null) {
            analysis.setStatus("Fatto - nessun problema ordinamento obiettivi");
            return analysis;
        }
        
        List<Term> goals = extractGoals(clause.getBody());
        analysis.setGoals(goals);
        
        // Analizza proprietà di ogni obiettivo
        for (int i = 0; i < goals.size(); i++) {
            Term goal = goals.get(i);
            GoalInfo info = analyzeGoal(goal, i, goals);
            analysis.addGoalInfo(info);
        }
        
        // Controlla ordinamento ottimale
        checkOptimalGoalOrder(analysis);
        
        return analysis;
    }
    
    /**
     * Estrai obiettivi individuali da corpo composto
     */
    private List<Term> extractGoals(Term body) {
        List<Term> goals = new ArrayList<>();
        extractGoalsRecursive(body, goals);
        return goals;
    }
    
    private void extractGoalsRecursive(Term term, List<Term> goals) {
        if (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            
            if (",".equals(compound.getFunctor()) && compound.getArity() == 2) {
                // Congiunzione - estrai entrambi i lati
                extractGoalsRecursive(compound.getArgument(1), goals);
                extractGoalsRecursive(compound.getArgument(2), goals);
            } else {
                // Obiettivo regolare
                goals.add(term);
            }
        } else {
            goals.add(term);
        }
    }
    
    /**
     * Analizza proprietà obiettivo individuale
     */
    private GoalInfo analyzeGoal(Term goal, int position, List<Term> allGoals) {
        GoalInfo info = new GoalInfo(goal, position);
        
        // Analizza tipo obiettivo
        if (isArithmeticTest(goal)) {
            info.setType(GoalType.ARITHMETIC_TEST);
            info.setDeterministic(true);
        } else if (isArithmeticComputation(goal)) {
            info.setType(GoalType.ARITHMETIC_COMPUTATION);
            info.setDeterministic(true);
        } else if (isUnification(goal)) {
            info.setType(GoalType.UNIFICATION);
            info.setDeterministic(true);
        } else if (isTypeTest(goal)) {
            info.setType(GoalType.TYPE_TEST);
            info.setDeterministic(true);
        } else {
            info.setType(GoalType.USER_DEFINED);
            info.setDeterministic(false); // Può avere soluzioni multiple
        }
        
        // Analizza dipendenze variabili
        Set<Variable> goalVars = goal.getVariables();
        Set<Variable> previouslyBound = getPreviouslyBoundVariables(allGoals, position);
        
        info.setUnboundVariables(Sets.difference(goalVars, previouslyBound));
        info.setBoundVariables(Sets.intersection(goalVars, previouslyBound));
        
        return info;
    }
    
    /**
     * Controlla pattern ordinamento obiettivi ottimale
     */
    private void checkOptimalGoalOrder(GoalOrderAnalysis analysis) {
        List<GoalInfo> goals = analysis.getGoalInfos();
        
        // Controlla se guardie (test) vengono prima dei generatori
        boolean foundGenerator = false;
        for (GoalInfo goal : goals) {
            if (goal.getType() == GoalType.USER_DEFINED && !goal.isDeterministic()) {
                foundGenerator = true;
            } else if (foundGenerator && isGuard(goal)) {
                analysis.addWarning("Condizione guardia dopo generatore - considera riordinamento");
            }
        }
        
        // Controlla se computazioni aritmetiche hanno tutte variabili legate
        for (GoalInfo goal : goals) {
            if (goal.getType() == GoalType.ARITHMETIC_COMPUTATION) {
                if (!goal.getUnboundVariables().isEmpty()) {
                    analysis.addWarning("Computazione aritmetica con variabili non legate");
                }
            }
        }
        
        // Suggerisci miglioramenti
        suggestImprovements(analysis);
    }
    
    /**
     * Suggerisci miglioramenti ordine obiettivi
     */
    private void suggestImprovements(GoalOrderAnalysis analysis) {
        List<GoalInfo> goals = analysis.getGoalInfos();
        List<String> suggestions = new ArrayList<>();
        
        // Identifica guardie che dovrebbero venire prima
        for (int i = 0; i < goals.size(); i++) {
            GoalInfo goal = goals.get(i);
            
            if (isGuard(goal) && goal.getBoundVariables().size() > 0) {
                // Questa guardia potrebbe essere spostata prima
                for (int j = 0; j < i; j++) {
                    GoalInfo earlierGoal = goals.get(j);
                    if (earlierGoal.getType() == GoalType.USER_DEFINED) {
                        suggestions.add("Considera spostare " + goal.getGoal() + 
                                      " prima di " + earlierGoal.getGoal());
                        break;
                    }
                }
            }
        }
        
        analysis.setSuggestions(suggestions);
    }
    
    private boolean isGuard(GoalInfo goal) {
        return goal.getType() == GoalType.ARITHMETIC_TEST ||
               goal.getType() == GoalType.TYPE_TEST;
    }
}
```

### 7.3.2 Strategie Ordinamento Obiettivi Ottimali

#### Il Principio "Guardie Prima dei Generatori"

Metti test deterministici (guardie) prima di generatori non-deterministici per ridurre lo spazio di ricerca.

```prolog
% INEFFICIENTE: Generatore prima della guardia
member_positive(X, List) :-
    member(X, List),        % Generatore: produce molte soluzioni
    X > 0.                  % Guardia: testa ogni soluzione

% EFFICIENTE: Guardia dopo legatura parziale
member_positive(X, List) :-
    member(X, List),        % Serve ancora generatore per X sconosciuta
    X > 0.                  % Guardia: testa immediatamente

% MEGLIO: Quando X è conosciuta, testa prima
check_positive_member(X, List) :-
    X > 0,                  % Guardia: testa presto se X è legata
    member(X, List).        % Generatore: solo se guardia ha successo

% OTTIMALE: Clausole diverse per pattern uso diversi
member_positive(X, List) :-
    var(X),                 % X è non legata - genera poi testa
    !,
    member(X, List),
    X > 0.
member_positive(X, List) :-
    nonvar(X),              % X è legata - testa poi controlla appartenenza
    X > 0,
    member(X, List).
```

#### Ottimizzatore Ordine Obiettivi JProlog

```java
/**
 * Ottimizzazione ordine obiettivi per regole JProlog
 */
public class GoalOrderOptimizer {
    
    /**
     * Ottimizza ordine obiettivi nel corpo regola
     */
    public Term optimizeGoalOrder(Term body) {
        if (body == null) return null;
        
        List<Term> goals = extractGoals(body);
        List<GoalInfo> goalInfos = analyzeGoals(goals);
        
        // Ordina obiettivi per criteri ottimizzazione
        goalInfos.sort(this::compareGoalPriority);
        
        // Ricostruisci corpo con ordine ottimizzato
        List<Term> optimizedGoals = goalInfos.stream()
            .map(GoalInfo::getGoal)
            .collect(Collectors.toList());
        
        return buildConjunction(optimizedGoals);
    }
    
    /**
     * Compara priorità obiettivo per ordinamento ottimale
     */
    private int compareGoalPriority(GoalInfo g1, GoalInfo g2) {
        // Ordine priorità:
        // 1. Test tipi (più veloce, più restrittivo)
        // 2. Test aritmetici con tutte variabili legate
        // 3. Unificazioni
        // 4. Computazioni aritmetiche
        // 5. Predicati definiti dall'utente (generatori)
        
        int priority1 = getGoalPriority(g1);
        int priority2 = getGoalPriority(g2);
        
        if (priority1 != priority2) {
            return Integer.compare(priority1, priority2);
        }
        
        // Criteri secondari: meno variabili non legate prima
        return Integer.compare(g1.getUnboundVariables().size(), 
                             g2.getUnboundVariables().size());
    }
    
    private int getGoalPriority(GoalInfo goal) {
        switch (goal.getType()) {
            case TYPE_TEST:
                return 1;
            case ARITHMETIC_TEST:
                return goal.getUnboundVariables().isEmpty() ? 2 : 4;
            case UNIFICATION:
                return 3;
            case ARITHMETIC_COMPUTATION:
                return 5;
            case USER_DEFINED:
                return goal.isDeterministic() ? 6 : 7;
            default:
                return 8;
        }
    }
    
    /**
     * Costruisci congiunzione da lista obiettivi
     */
    private Term buildConjunction(List<Term> goals) {
        if (goals.isEmpty()) {
            return new Atom("true");
        }
        
        if (goals.size() == 1) {
            return goals.get(0);
        }
        
        Term result = goals.get(goals.size() - 1);
        for (int i = goals.size() - 2; i >= 0; i--) {
            result = new CompoundTerm(",", Arrays.asList(goals.get(i), result));
        }
        
        return result;
    }
}
```

### 7.3.3 Analisi Legatura Variabili

Comprendere il flusso variabili attraverso obiettivi è cruciale per ordinamento obiettivi ottimale.

#### Tracciatore Legatura Variabili

```java
/**
 * Traccia legature variabili attraverso esecuzione obiettivi
 */
public class VariableBindingTracker {
    
    /**
     * Analizza flusso legatura variabili attraverso obiettivi
     */
    public BindingFlowAnalysis analyzeBindingFlow(List<Term> goals) {
        BindingFlowAnalysis analysis = new BindingFlowAnalysis();
        Set<Variable> currentlyBound = new HashSet<>();
        
        for (int i = 0; i < goals.size(); i++) {
            Term goal = goals.get(i);
            Set<Variable> goalVars = goal.getVariables();
            
            GoalBindingInfo bindingInfo = new GoalBindingInfo(goal, i);
            
            // Variabili che sono input a questo obiettivo (già legate)
            Set<Variable> inputVars = Sets.intersection(goalVars, currentlyBound);
            bindingInfo.setInputVariables(inputVars);
            
            // Variabili che questo obiettivo legherà (output)
            Set<Variable> outputVars = determineOutputVariables(goal, inputVars);
            bindingInfo.setOutputVariables(outputVars);
            
            // Variabili che rimangono non legate dopo questo obiettivo
            Set<Variable> unboundVars = Sets.difference(goalVars, 
                Sets.union(inputVars, outputVars));
            bindingInfo.setUnboundVariables(unboundVars);
            
            analysis.addGoalBindingInfo(bindingInfo);
            
            // Aggiorna variabili attualmente legate
            currentlyBound.addAll(outputVars);
        }
        
        return analysis;
    }
    
    /**
     * Determina quali variabili un obiettivo legherà
     */
    private Set<Variable> determineOutputVariables(Term goal, Set<Variable> inputVars) {
        Set<Variable> outputVars = new HashSet<>();
        
        if (goal instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) goal;
            String functor = compound.getFunctor();
            
            switch (functor) {
                case "is":
                    // X is Expr - X diventa legata se Expr è valutabile
                    if (compound.getArity() == 2) {
                        Term leftArg = compound.getArgument(1);
                        Term rightArg = compound.getArgument(2);
                        
                        if (leftArg.isVariable() && isEvaluableExpression(rightArg, inputVars)) {
                            outputVars.add((Variable) leftArg);
                        }
                    }
                    break;
                    
                case "=":
                    // X = Y - unificazione può legare variabili
                    if (compound.getArity() == 2) {
                        Term arg1 = compound.getArgument(1);
                        Term arg2 = compound.getArgument(2);
                        
                        outputVars.addAll(determineUnificationOutputs(arg1, arg2, inputVars));
                    }
                    break;
                    
                default:
                    // Predicato definito dall'utente - assume possa legare qualsiasi variabile non legata
                    // Questo è conservativo; analisi più sofisticata potrebbe essere più precisa
                    Set<Variable> goalVars = goal.getVariables();
                    for (Variable var : goalVars) {
                        if (!inputVars.contains(var)) {
                            outputVars.add(var);
                        }
                    }
                    break;
            }
        }
        
        return outputVars;
    }
    
    /**
     * Controlla se espressione è valutabile date variabili legate
     */
    private boolean isEvaluableExpression(Term expr, Set<Variable> boundVars) {
        if (expr.isNumber()) {
            return true;
        }
        
        if (expr.isVariable()) {
            return boundVars.contains((Variable) expr);
        }
        
        if (expr instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) expr;
            String functor = compound.getFunctor();
            
            // Controlla se tutti argomenti sono valutabili
            if (isArithmeticOperator(functor)) {
                for (int i = 1; i <= compound.getArity(); i++) {
                    if (!isEvaluableExpression(compound.getArgument(i), boundVars)) {
                        return false;
                    }
                }
                return true;
            }
        }
        
        return false;
    }
}
```

### 7.3.4 Esempi Ordine Obiettivi

#### Pattern Ordinamento Obiettivi Efficaci

```prolog
% Pattern 1: Controllo tipo prima delle operazioni
process_number(X, Result) :-
    number(X),              % Guardia: controlla tipo prima
    X > 0,                  % Guardia: controlla vincolo
    Result is X * 2.        % Computazione: sicura procedere

% Pattern 2: Legatura strutturale prima della ricorsione
process_list([H|T], [H2|T2]) :-
    process_element(H, H2),     % Elabora testa
    process_list(T, T2).        % Ricorri sulla coda

process_list([], []).

% Pattern 3: Condizioni fallimento precoce
safe_divide(X, Y, Result) :-
    number(X),                  % Controlli tipo prima
    number(Y),
    Y =\= 0,                   % Controllo divisione per zero
    Result is X / Y.            % Computazione sicura

% Pattern 4: Genera e testa con ottimizzazione
find_factors(N, Factor) :-
    integer(N),                 % Guardia: assicura input valido
    N > 1,                      % Guardia: input significativo
    between(2, N, Factor),      % Generatore: prova fattori
    0 is N mod Factor.          % Test: controlla se effettivamente fattore
```

## 7.4 Soluzioni Ridondanti

Le soluzioni ridondanti si verificano quando un programma Prolog genera risposte identiche multiple attraverso percorsi di derivazione diversi. Gestire la ridondanza è cruciale per efficienza e correttezza.

### 7.4.1 Cause Soluzioni Ridondanti

#### Problema Percorsi Multipli

```prolog
% Questo predicato genera soluzioni ridondanti
path(X, Y) :- edge(X, Y).
path(X, Y) :- edge(X, Z), path(Z, Y).

% Con archi: edge(a,b). edge(b,c). edge(a,c).
% Query: path(a, c) produce:
% 1. path(a,c) via edge(a,c)           - percorso diretto
% 2. path(a,c) via edge(a,b), path(b,c) - percorso indiretto
```

#### Analizzatore Ridondanza JProlog

```java
/**
 * Analizza e rileva soluzioni ridondanti in JProlog
 */
public class RedundancyAnalyzer {
    
    private final Set<Term> seenSolutions = new HashSet<>();
    private final Map<String, Integer> solutionCounts = new HashMap<>();
    
    /**
     * Rileva soluzioni ridondanti durante esecuzione query
     */
    public Iterator<Substitution> filterRedundantSolutions(
            Iterator<Substitution> originalSolutions, Term query) {
        
        return new Iterator<Substitution>() {
            private Substitution nextSolution = null;
            private boolean hasComputedNext = false;
            
            @Override
            public boolean hasNext() {
                if (!hasComputedNext) {
                    computeNext();
                }
                return nextSolution != null;
            }
            
            @Override
            public Substitution next() {
                if (!hasNext()) {
                    throw new NoSuchElementException();
                }
                Substitution result = nextSolution;
                hasComputedNext = false;
                return result;
            }
            
            private void computeNext() {
                nextSolution = null;
                hasComputedNext = true;
                
                while (originalSolutions.hasNext()) {
                    Substitution solution = originalSolutions.next();
                    Term instantiatedQuery = query.substitute(solution);
                    
                    if (!seenSolutions.contains(instantiatedQuery)) {
                        seenSolutions.add(instantiatedQuery);
                        nextSolution = solution;
                        
                        // Traccia statistiche soluzioni
                        String solutionKey = instantiatedQuery.toString();
                        solutionCounts.put(solutionKey, 
                            solutionCounts.getOrDefault(solutionKey, 0) + 1);
                        
                        break;
                    } else {
                        // Registra soluzione ridondante
                        recordRedundantSolution(instantiatedQuery);
                    }
                }
            }
        };
    }
    
    /**
     * Registra soluzione ridondante per analisi
     */
    private void recordRedundantSolution(Term redundantSolution) {
        String solutionKey = redundantSolution.toString();
        solutionCounts.put(solutionKey, solutionCounts.getOrDefault(solutionKey, 0) + 1);
    }
    
    /**
     * Genera report ridondanza
     */
    public RedundancyReport generateReport() {
        RedundancyReport report = new RedundancyReport();
        
        int totalSolutions = solutionCounts.values().stream()
            .mapToInt(Integer::intValue)
            .sum();
        int uniqueSolutions = solutionCounts.size();
        int redundantSolutions = totalSolutions - uniqueSolutions;
        
        report.setTotalSolutions(totalSolutions);
        report.setUniqueSolutions(uniqueSolutions);
        report.setRedundantSolutions(redundantSolutions);
        report.setRedundancyRatio((double) redundantSolutions / totalSolutions);
        
        // Trova soluzioni più ridondanti
        solutionCounts.entrySet().stream()
            .filter(entry -> entry.getValue() > 1)
            .sorted(Map.Entry.<String, Integer>comparingByValue().reversed())
            .limit(10)
            .forEach(entry -> 
                report.addRedundantSolution(entry.getKey(), entry.getValue()));
        
        return report;
    }
}
```

### 7.4.2 Eliminazione Ridondanza

#### Uso Cut per Eliminare Ridondanza

```prolog
% Senza cut - soluzioni multiple per max
max(X, Y, X) :- X >= Y.
max(X, Y, Y) :- Y > X.

% Con cut - max deterministico
max(X, Y, X) :- X >= Y, !.
max(X, Y, Y).

% Path con cut per prevenire ridondanza
path_unique(X, Y) :- edge(X, Y), !.
path_unique(X, Y) :- edge(X, Z), path_unique(Z, Y).
```

#### Implementazione Cut JProlog

```java
/**
 * Implementazione cut nel QuerySolver JProlog
 */
public class CutImplementation {
    
    /**
     * Gestisce cut durante esecuzione query
     */
    public Iterator<Substitution> handleCut(ExecutionContext context) {
        // Cut rimuove tutti punti scelta fino alla regola corrente
        int currentRuleLevel = context.getCurrentRuleLevel();
        context.removeChoicePointsAfter(currentRuleLevel);
        
        // Continua con sostituzione corrente
        return Collections.singletonList(context.getSubstitution()).iterator();
    }
    
    /**
     * Esecuzione obiettivo consapevole cut
     */
    public Iterator<Substitution> solveWithCutAwareness(Term goal, ExecutionContext context) {
        if (isCut(goal)) {
            return handleCut(context);
        }
        
        return new CutAwareIterator(solveGoal(goal, context), context);
    }
    
    private boolean isCut(Term goal) {
        return goal.isAtom() && "!".equals(((Atom) goal).getValue());
    }
    
    /**
     * Iterator che rispetta semantica cut
     */
    private class CutAwareIterator implements Iterator<Substitution> {
        private final Iterator<Substitution> originalIterator;
        private final ExecutionContext context;
        private boolean cutEncountered = false;
        
        public CutAwareIterator(Iterator<Substitution> originalIterator, 
                               ExecutionContext context) {
            this.originalIterator = originalIterator;
            this.context = context;
        }
        
        @Override
        public boolean hasNext() {
            return !cutEncountered && originalIterator.hasNext();
        }
        
        @Override
        public Substitution next() {
            if (cutEncountered || !originalIterator.hasNext()) {
                throw new NoSuchElementException();
            }
            
            Substitution solution = originalIterator.next();
            
            // Controlla se cut è stato eseguito in questa derivazione
            if (context.isCutExecuted()) {
                cutEncountered = true;
                context.resetCutFlag();
            }
            
            return solution;
        }
    }
}
```

#### Tecniche Alternative Eliminazione Ridondanza

```prolog
% Tecnica 1: Usa setof/3 per eliminare duplicati
unique_paths(Start, End, Paths) :-
    setof(Path, path_sequence(Start, End, Path), Paths).

% Tecnica 2: Usa once/1 per soluzione singola
first_path(Start, End, Path) :-
    once(path_sequence(Start, End, Path)).

% Tecnica 3: Pattern memoizzazione
:- dynamic path_memo/3.

path_with_memo(X, Y, Path) :-
    path_memo(X, Y, Path), !.
path_with_memo(X, Y, Path) :-
    path_sequence(X, Y, Path),
    assertz(path_memo(X, Y, Path)).

% Tecnica 4: Accumulatore per tracciare nodi visitati
path_no_cycles(Start, End, Path) :-
    path_no_cycles(Start, End, [Start], Path).

path_no_cycles(Node, Node, Visited, [Node]) :-
    member(Node, Visited).
path_no_cycles(Start, End, Visited, [Start|Path]) :-
    edge(Start, Next),
    \+ member(Next, Visited),
    path_no_cycles(Next, End, [Next|Visited], Path).
```

### 7.4.3 Rilevamento e Ottimizzazione Ridondanza

#### Ottimizzatore Ridondanza JProlog

```java
/**
 * Ottimizza predicati per ridurre soluzioni ridondanti
 */
public class RedundancyOptimizer {
    
    /**
     * Analizza predicato per pattern ridondanza
     */
    public OptimizationSuggestions analyzeForRedundancy(
            String predicateIndicator, List<Clause> clauses) {
        
        OptimizationSuggestions suggestions = new OptimizationSuggestions();
        
        // Controlla pattern sovrapposti
        checkOverlappingPatterns(clauses, suggestions);
        
        // Controlla predicati non deterministici che potrebbero essere deterministici
        checkUnnecessaryNonDeterminism(clauses, suggestions);
        
        // Controlla cut mancanti
        checkMissingCuts(clauses, suggestions);
        
        return suggestions;
    }
    
    /**
     * Controlla pattern clausole sovrapposti
     */
    private void checkOverlappingPatterns(List<Clause> clauses, 
                                         OptimizationSuggestions suggestions) {
        
        for (int i = 0; i < clauses.size(); i++) {
            for (int j = i + 1; j < clauses.size(); j++) {
                Clause clause1 = clauses.get(i);
                Clause clause2 = clauses.get(j);
                
                if (clausesOverlap(clause1, clause2)) {
                    suggestions.addSuggestion(
                        "Clausole " + (i+1) + " e " + (j+1) + " hanno pattern sovrapposti - " +
                        "considera riordinamento o aggiunta cut per eliminare ridondanza"
                    );
                }
            }
        }
    }
    
    /**
     * Controlla se due clausole possono produrre soluzioni sovrapposte
     */
    private boolean clausesOverlap(Clause c1, Clause c2) {
        // Controllo semplificato - in pratica servirebbe analisi unificazione completa
        Term head1 = c1.getHead();
        Term head2 = c2.getHead();
        
        // Crea variabili fresche per test unificazione
        Map<Variable, Variable> varMap1 = new HashMap<>();
        Map<Variable, Variable> varMap2 = new HashMap<>();
        
        Term freshHead1 = head1.createFreshCopy(varMap1);
        Term freshHead2 = head2.createFreshCopy(varMap2);
        
        // Controlla se teste possono unificarsi
        Substitution testSubst = new Substitution();
        return freshHead1.unify(freshHead2, testSubst);
    }
    
    /**
     * Suggerisci posizionamento cut per eliminazione ridondanza
     */
    public List<CutSuggestion> suggestCutPlacements(List<Clause> clauses) {
        List<CutSuggestion> suggestions = new ArrayList<>();
        
        for (int i = 0; i < clauses.size(); i++) {
            Clause clause = clauses.get(i);
            
            if (shouldHaveCut(clause, clauses, i)) {
                CutSuggestion suggestion = new CutSuggestion(
                    i,
                    "Aggiungi cut dopo condizioni deterministiche",
                    identifyOptimalCutPosition(clause)
                );
                suggestions.add(suggestion);
            }
        }
        
        return suggestions;
    }
    
    private boolean shouldHaveCut(Clause clause, List<Clause> allClauses, int index) {
        // Controlla se questa clausola è deterministica ma seguita da alternative
        if (index < allClauses.size() - 1 && isDeterministicCondition(clause)) {
            return true;
        }
        
        return false;
    }
    
    private boolean isDeterministicCondition(Clause clause) {
        // Controlla se clausola ha condizioni che la rendono mutuamente esclusiva
        if (clause.getBody() != null) {
            return containsArithmeticTests(clause.getBody()) ||
                   containsTypeTests(clause.getBody());
        }
        return false;
    }
}
```

## 7.5 Programmazione Ricorsiva in Prolog Puro

La ricorsione è la struttura di controllo primaria nel Prolog puro, abilitando soluzioni eleganti a problemi complessi attraverso definizioni auto-referenziali.

### 7.5.1 Tipi di Ricorsione

#### Ricorsione Lineare

La ricorsione lineare elabora un elemento alla volta, facendo una singola chiamata ricorsiva per iterazione.

```prolog
% Ricorsione lineare su liste
list_length([], 0).
list_length([_|T], N) :-
    list_length(T, N1),
    N is N1 + 1.

% Ricorsione lineare con accumulatore (ricorsione di coda)
list_length_acc(List, Length) :-
    list_length_acc(List, 0, Length).

list_length_acc([], Acc, Acc).
list_length_acc([_|T], Acc, Length) :-
    Acc1 is Acc + 1,
    list_length_acc(T, Acc1, Length).
```

#### Ricorsione Binaria

La ricorsione binaria fa due chiamate ricorsive, spesso usata per strutture ad albero o algoritmi divide-et-impera.

```prolog
% Operazioni alberi binari
tree_size(nil, 0).
tree_size(node(_, L, R), Size) :-
    tree_size(L, SizeL),
    tree_size(R, SizeR),
    Size is 1 + SizeL + SizeR.

% Calcolo altezza albero
tree_height(nil, 0).
tree_height(node(_, L, R), Height) :-
    tree_height(L, HL),
    tree_height(R, HR),
    Height is 1 + max(HL, HR).
```

#### Analisi Ricorsione JProlog

```java
/**
 * Analizza pattern ricorsivi nei predicati JProlog
 */
public class RecursionAnalyzer {
    
    /**
     * Analizza tipo e proprietà ricorsione
     */
    public RecursionAnalysis analyzeRecursion(String predicateIndicator, 
                                             List<Clause> clauses) {
        RecursionAnalysis analysis = new RecursionAnalysis(predicateIndicator);
        
        // Classifica ogni clausola
        for (Clause clause : clauses) {
            ClauseRecursionInfo info = classifyClause(clause, predicateIndicator);
            analysis.addClauseInfo(info);
        }
        
        // Determina pattern ricorsione generale
        analysis.determineRecursionPattern();
        
        return analysis;
    }
    
    /**
     * Classifica clausola come caso base, ricorsiva lineare, o ricorsiva complessa
     */
    private ClauseRecursionInfo classifyClause(Clause clause, String predicateIndicator) {
        ClauseRecursionInfo info = new ClauseRecursionInfo(clause);
        
        if (clause.getBody() == null) {
            info.setType(RecursionType.BASE_CASE);
            return info;
        }
        
        int recursiveCalls = countRecursiveCalls(clause.getBody(), predicateIndicator);
        
        if (recursiveCalls == 0) {
            info.setType(RecursionType.NON_RECURSIVE);
        } else if (recursiveCalls == 1) {
            if (isTailRecursive(clause, predicateIndicator)) {
                info.setType(RecursionType.TAIL_RECURSIVE);
            } else {
                info.setType(RecursionType.LINEAR_RECURSIVE);
            }
        } else {
            info.setType(RecursionType.BINARY_RECURSIVE);
        }
        
        info.setRecursiveCallCount(recursiveCalls);
        
        return info;
    }
    
    /**
     * Controlla se clausola è ricorsiva di coda
     */
    private boolean isTailRecursive(Clause clause, String predicateIndicator) {
        Term body = clause.getBody();
        
        // Trova ultimo obiettivo nella congiunzione
        Term lastGoal = findLastGoal(body);
        
        // Controlla se ultimo obiettivo è chiamata ricorsiva
        return isRecursiveCall(lastGoal, predicateIndicator);
    }
    
    /**
     * Trova obiettivo più a destra in congiunzione
     */
    private Term findLastGoal(Term body) {
        if (body instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) body;
            
            if (",".equals(compound.getFunctor()) && compound.getArity() == 2) {
                // Congiunzione - ritorna obiettivo più a destra
                return findLastGoal(compound.getArgument(2));
            }
        }
        
        return body;
    }
    
    /**
     * Conta chiamate ricorsive nel termine
     */
    private int countRecursiveCalls(Term term, String predicateIndicator) {
        if (term == null) return 0;
        
        if (isRecursiveCall(term, predicateIndicator)) {
            return 1;
        }
        
        if (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            
            if (",".equals(compound.getFunctor())) {
                // Congiunzione - conta chiamate in entrambi argomenti
                return countRecursiveCalls(compound.getArgument(1), predicateIndicator) +
                       countRecursiveCalls(compound.getArgument(2), predicateIndicator);
            }
        }
        
        return 0;
    }
    
    /**
     * Controlla se termine è chiamata ricorsiva
     */
    private boolean isRecursiveCall(Term term, String predicateIndicator) {
        if (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            String termPredicateIndicator = compound.getFunctor() + "/" + compound.getArity();
            return predicateIndicator.equals(termPredicateIndicator);
        } else if (term instanceof Atom) {
            // Predicato arità zero
            String termPredicateIndicator = ((Atom) term).getValue() + "/0";
            return predicateIndicator.equals(termPredicateIndicator);
        }
        
        return false;
    }
}
```

### 7.5.2 Ottimizzazione Ricorsione Coda

La ricorsione di coda può essere ottimizzata per usare spazio stack costante, rendendola equivalente all'iterazione.

#### Pattern Ricorsivi di Coda

```prolog
% Reverse ricorsivo standard (non ricorsione coda)
reverse([], []).
reverse([H|T], Rev) :-
    reverse(T, RevT),
    append(RevT, [H], Rev).

% Reverse ricorsivo coda con accumulatore
reverse_tr(List, Rev) :-
    reverse_tr(List, [], Rev).

reverse_tr([], Acc, Acc).
reverse_tr([H|T], Acc, Rev) :-
    reverse_tr(T, [H|Acc], Rev).

% Fattoriale ricorsivo coda
factorial_tr(N, F) :-
    factorial_tr(N, 1, F).

factorial_tr(0, Acc, Acc).
factorial_tr(N, Acc, F) :-
    N > 0,
    N1 is N - 1,
    Acc1 is N * Acc,
    factorial_tr(N1, Acc1, F).
```

#### Ottimizzatore Ricorsione Coda JProlog

```java
/**
 * Ottimizzazione ricorsione coda in JProlog
 */
public class TailRecursionOptimizer {
    
    /**
     * Rileva predicati ricorsivi coda per ottimizzazione
     */
    public boolean isTailRecursiveOptimizable(String predicateIndicator, List<Clause> clauses) {
        // Controlla se tutte clausole ricorsive sono ricorsive di coda
        for (Clause clause : clauses) {
            if (isRecursiveClause(clause, predicateIndicator)) {
                if (!isTailRecursive(clause, predicateIndicator)) {
                    return false;
                }
            }
        }
        return true;
    }
    
    /**
     * Trasforma predicato ricorsivo coda in forma iterativa
     */
    public OptimizedPredicate optimizeTailRecursion(String predicateIndicator, 
                                                   List<Clause> clauses) {
        
        if (!isTailRecursiveOptimizable(predicateIndicator, clauses)) {
            return null;
        }
        
        // Genera implementazione iterativa ottimizzata
        return new OptimizedTailRecursivePredicate(predicateIndicator, clauses);
    }
    
    /**
     * Implementazione predicato ricorsivo coda ottimizzato
     */
    private class OptimizedTailRecursivePredicate extends BuiltIn {
        private final String predicateIndicator;
        private final List<Clause> originalClauses;
        private final Clause baseCase;
        private final Clause recursiveCase;
        
        public OptimizedTailRecursivePredicate(String predicateIndicator, 
                                             List<Clause> clauses) {
            this.predicateIndicator = predicateIndicator;
            this.originalClauses = clauses;
            
            // Identifica caso base e caso ricorsivo
            this.baseCase = findBaseCase(clauses);
            this.recursiveCase = findRecursiveCase(clauses, predicateIndicator);
        }
        
        @Override
        public Iterator<Substitution> execute(List<Term> arguments, 
                                            ExecutionContext context) 
                throws PrologException {
            
            // Implementa versione iterativa di ricorsione coda
            return executeIteratively(arguments, context);
        }
        
        /**
         * Esegue predicato ricorsivo coda iterativamente
         */
        private Iterator<Substitution> executeIteratively(List<Term> arguments, 
                                                         ExecutionContext context) {
            
            List<Term> currentArgs = new ArrayList<>(arguments);
            Substitution currentSubst = context.getSubstitution();
            
            while (true) {
                // Prova caso base
                ExecutionContext baseContext = context.withSubstitution(currentSubst);
                Iterator<Substitution> baseSolutions = tryClause(baseCase, currentArgs, baseContext);
                
                if (baseSolutions.hasNext()) {
                    // Caso base riuscito
                    return baseSolutions;
                }
                
                // Prova caso ricorsivo
                ExecutionContext recContext = context.withSubstitution(currentSubst);
                RecursiveStepResult stepResult = executeRecursiveStep(recursiveCase, currentArgs, recContext);
                
                if (stepResult == null) {
                    // Caso ricorsivo fallito
                    return Collections.emptyIterator();
                }
                
                // Aggiorna argomenti e sostituzione per iterazione successiva
                currentArgs = stepResult.getNextArguments();
                currentSubst = stepResult.getNextSubstitution();
            }
        }
        
        @Override
        public String getSignature() {
            return predicateIndicator;
        }
    }
}
```

### 7.5.3 Strutture Dati Ricorsive

Il Prolog puro eccelle nell'elaborare strutture dati ricorsive come liste, alberi e grafi.

#### Pattern Elaborazione Liste

```prolog
% Template elaborazione liste base
process_list([], []).
process_list([H|T], [H2|T2]) :-
    process_element(H, H2),
    process_list(T, T2).

% Pattern filtro
filter_list([], _, []).
filter_list([H|T], Condition, [H|FilteredT]) :-
    call(Condition, H),
    !,
    filter_list(T, Condition, FilteredT).
filter_list([_|T], Condition, FilteredT) :-
    filter_list(T, Condition, FilteredT).

% Pattern folding (riduci)
fold_list([], Base, Base).
fold_list([H|T], Base, Result) :-
    fold_list(T, Base, Intermediate),
    combine(H, Intermediate, Result).

% Esempio: somma lista usando fold
sum_list(List, Sum) :-
    fold_list(List, 0, Sum).

combine(X, Y, Z) :- Z is X + Y.
```

#### Pattern Elaborazione Alberi

```prolog
% Definizione albero binario: tree(Value, LeftSubtree, RightSubtree) o nil

% Pattern attraversamento alberi
inorder(nil, []).
inorder(tree(V, L, R), Traversal) :-
    inorder(L, LeftTraversal),
    inorder(R, RightTraversal),
    append(LeftTraversal, [V|RightTraversal], Traversal).

preorder(nil, []).
preorder(tree(V, L, R), [V|Traversal]) :-
    preorder(L, LeftTraversal),
    preorder(R, RightTraversal),
    append(LeftTraversal, RightTraversal, Traversal).

postorder(nil, []).
postorder(tree(V, L, R), Traversal) :-
    postorder(L, LeftTraversal),
    postorder(R, RightTraversal),
    append(LeftTraversal, RightTraversal, PartialTraversal),
    append(PartialTraversal, [V], Traversal).

% Ricerca albero
tree_member(X, tree(X, _, _)).
tree_member(X, tree(_, L, _)) :-
    tree_member(X, L).
tree_member(X, tree(_, _, R)) :-
    tree_member(X, R).

% Operazioni albero binario ricerca
bst_insert(X, nil, tree(X, nil, nil)).
bst_insert(X, tree(V, L, R), tree(V, L2, R)) :-
    X =< V,
    bst_insert(X, L, L2).
bst_insert(X, tree(V, L, R), tree(V, L, R2)) :-
    X > V,
    bst_insert(X, R, R2).
```

### 7.5.4 Ricorsione Mutua

La ricorsione mutua coinvolge predicati multipli che si chiamano a vicenda, utile per parsing e macchine a stati.

```prolog
% Esempio: parsing espressioni con ricorsione mutua
% Grammatica: expr -> term ('+' term)*
%         term -> factor ('*' factor)*
%         factor -> number | '(' expr ')'

parse_expr([N|Rest], N, Rest) :-
    number(N).
parse_expr(['('|Tokens], Expr, Rest) :-
    parse_expr(Tokens, Expr, [')'|Rest]).
parse_expr(Tokens, Expr, Rest) :-
    parse_term(Tokens, Term, Rest1),
    parse_expr_rest(Rest1, Term, Expr, Rest).

parse_expr_rest(['+' | Tokens], Left, Expr, Rest) :-
    parse_term(Tokens, Right, Rest1),
    Combined is Left + Right,
    parse_expr_rest(Rest1, Combined, Expr, Rest).
parse_expr_rest(Tokens, Expr, Expr, Tokens).

parse_term(Tokens, Term, Rest) :-
    parse_factor(Tokens, Factor, Rest1),
    parse_term_rest(Rest1, Factor, Term, Rest).

parse_term_rest(['*' | Tokens], Left, Term, Rest) :-
    parse_factor(Tokens, Right, Rest1),
    Combined is Left * Right,
    parse_term_rest(Rest1, Combined, Term, Rest).
parse_term_rest(Tokens, Term, Term, Tokens).

parse_factor([N|Rest], N, Rest) :-
    number(N).
parse_factor(['('|Tokens], Expr, Rest) :-
    parse_expr(Tokens, Expr, [')'|Rest2]),
    Rest2 = Rest.
```

### 7.5.5 Migliori Pratiche Programmazione Ricorsiva

#### Principi Design

1. **Casi Base Chiari**: Definisci sempre condizioni terminazione chiare
2. **Progresso Verso Caso Base**: Assicura che chiamate ricorsive facciano progresso
3. **Responsabilità Singola**: Ogni predicato ricorsivo dovrebbe avere uno scopo chiaro
4. **Pattern Accumulatore**: Usa accumulatori per ricorsione coda quando possibile

#### Pattern Ricorsivi Comuni in Prolog Puro

```prolog
% Pattern 1: Ricorsione strutturale su liste
list_pattern([], BaseCase).
list_pattern([H|T], Result) :-
    process_head(H, ProcessedH),
    list_pattern(T, TailResult),
    combine(ProcessedH, TailResult, Result).

% Pattern 2: Pattern accumulatore per efficienza
efficient_pattern(Input, Output) :-
    efficient_pattern(Input, InitialAccumulator, Output).

efficient_pattern([], Acc, Acc).
efficient_pattern([H|T], Acc, Output) :-
    update_accumulator(H, Acc, NewAcc),
    efficient_pattern(T, NewAcc, Output).

% Pattern 3: Divide et impera
divide_conquer([], BaseResult).
divide_conquer([Single], SingleResult) :-
    process_single(Single, SingleResult).
divide_conquer(List, Result) :-
    length(List, Len),
    Len > 1,
    split_list(List, Left, Right),
    divide_conquer(Left, LeftResult),
    divide_conquer(Right, RightResult),
    combine_results(LeftResult, RightResult, Result).

% Pattern 4: Ricorsione basata stati
state_recursion(InitialState, FinalState) :-
    transition_possible(InitialState),
    next_state(InitialState, NextState),
    state_recursion(NextState, FinalState).
state_recursion(FinalState, FinalState) :-
    is_final_state(FinalState).
```

## 7.6 Background

Comprendere i fondamenti teorici della programmazione Prolog puro fornisce intuizioni sulla sua potenza e limitazioni, aiutando sviluppatori a scrivere programmi logici più efficaci.

### 7.6.1 Fondamenti Programmazione Logica

Il Prolog puro è basato sulla logica del primo ordine con clausole di Horn, fornendo un paradigma programmazione dichiarativa dove i programmi descrivono relazioni piuttosto che computazioni.

#### Logica Clausole Horn

```java
/**
 * Rappresentazione logica clausole Horn in JProlog
 */
public class HornClauseLogic {
    
    /**
     * Una clausola Horn è o:
     * 1. Un fatto: head.
     * 2. Una regola: head :- body1, body2, ..., bodyN.
     * Dove head è un atomo e bodies sono letterali
     */
    
    /**
     * Converte clausola JProlog in rappresentazione logica
     */
    public LogicalClause toLogicalForm(Clause prologClause) {
        Term head = prologClause.getHead();
        Term body = prologClause.getBody();
        
        if (body == null) {
            // Fatto: head.
            return new LogicalClause(head, Collections.emptyList());
        } else {
            // Regola: head :- body.
            List<Term> bodyLiterals = extractLiterals(body);
            return new LogicalClause(head, bodyLiterals);
        }
    }
    
    /**
     * Estrai letterali da corpo composto
     */
    private List<Term> extractLiterals(Term body) {
        List<Term> literals = new ArrayList<>();
        extractLiteralsRecursive(body, literals);
        return literals;
    }
    
    private void extractLiteralsRecursive(Term term, List<Term> literals) {
        if (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            
            if (",".equals(compound.getFunctor()) && compound.getArity() == 2) {
                // Congiunzione - estrai entrambi operandi
                extractLiteralsRecursive(compound.getArgument(1), literals);
                extractLiteralsRecursive(compound.getArgument(2), literals);
            } else {
                // Letterale regolare
                literals.add(term);
            }
        } else {
            literals.add(term);
        }
    }
    
    /**
     * Rappresentazione logica clausola Horn
     */
    public static class LogicalClause {
        private final Term head;
        private final List<Term> body;
        
        public LogicalClause(Term head, List<Term> body) {
            this.head = head;
            this.body = new ArrayList<>(body);
        }
        
        /**
         * Converte in implicazione logica: body1 ∧ body2 ∧ ... ∧ bodyN → head
         */
        public String toLogicalImplication() {
            if (body.isEmpty()) {
                return head.toString(); // Fatto
            }
            
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < body.size(); i++) {
                if (i > 0) sb.append(" ∧ ");
                sb.append(body.get(i).toString());
            }
            sb.append(" → ").append(head.toString());
            
            return sb.toString();
        }
        
        /**
         * Converte in forma disgiuntiva: ¬body1 ∨ ¬body2 ∨ ... ∨ ¬bodyN ∨ head
         */
        public String toDisjunctiveForm() {
            StringBuilder sb = new StringBuilder();
            
            for (Term bodyTerm : body) {
                sb.append("¬").append(bodyTerm.toString()).append(" ∨ ");
            }
            sb.append(head.toString());
            
            return sb.toString();
        }
    }
}
```

### 7.6.2 Risoluzione SLD

La risoluzione SLD (Selective Linear Definite clause) è la procedura dimostrazione usata da Prolog, combinando risoluzione con unificazione e strategia ricerca specifica.

#### Implementazione Risoluzione SLD

```java
/**
 * Implementazione risoluzione SLD in JProlog
 */
public class SLDResolution {
    
    /**
     * Esegui passo risoluzione SLD
     */
    public ResolutionStep performResolutionStep(Goal goal, Clause clause, 
                                               ExecutionContext context) {
        
        // Passo 1: Crea copia fresca clausola (rinominamento variabili)
        Map<Variable, Variable> variableMap = new HashMap<>();
        Clause freshClause = clause.createFreshCopy(variableMap);
        
        // Passo 2: Seleziona obiettivo più a sinistra da congiunzione obiettivi
        Term selectedGoal = selectLeftmostGoal(goal.getTerm());
        
        // Passo 3: Tenta unificazione con testa clausola
        Substitution mgu = computeMGU(selectedGoal, freshClause.getHead());
        
        if (mgu == null) {
            return ResolutionStep.failure("Unificazione fallita");
        }
        
        // Passo 4: Applica sostituzione per creare risolvent
        Goal resolvent = createResolvent(goal, selectedGoal, freshClause, mgu);
        
        return ResolutionStep.success(resolvent, mgu);
    }
    
    /**
     * Calcola Most General Unifier (MGU)
     */
    private Substitution computeMGU(Term term1, Term term2) {
        Substitution mgu = new Substitution();
        
        if (term1.unify(term2, mgu)) {
            return mgu;
        } else {
            return null;
        }
    }
    
    /**
     * Crea risolvent dopo passo risoluzione riuscito
     */
    private Goal createResolvent(Goal originalGoal, Term selectedGoal, 
                                Clause clause, Substitution mgu) {
        
        // Rimuovi obiettivo selezionato da congiunzione obiettivi
        Term remainingGoals = removeGoalFromConjunction(originalGoal.getTerm(), selectedGoal);
        
        // Aggiungi corpo clausola (se presente) ad obiettivi rimanenti
        Term clauseBody = clause.getBody();
        Term newGoalTerm;
        
        if (clauseBody == null) {
            // Fatto - solo obiettivi rimanenti
            newGoalTerm = remainingGoals;
        } else if (remainingGoals == null) {
            // Solo corpo clausola
            newGoalTerm = clauseBody;
        } else {
            // Congiunzione corpo clausola e obiettivi rimanenti
            newGoalTerm = new CompoundTerm(",", Arrays.asList(clauseBody, remainingGoals));
        }
        
        // Applica sostituzione al nuovo obiettivo
        if (newGoalTerm != null) {
            newGoalTerm = newGoalTerm.substitute(mgu);
        }
        
        return new Goal(newGoalTerm, originalGoal.getVariables());
    }
    
    /**
     * Costruzione albero derivazione SLD
     */
    public SLDDerivationTree constructDerivationTree(Goal initialGoal, KnowledgeBase kb) {
        SLDDerivationTree tree = new SLDDerivationTree(initialGoal);
        
        Queue<DerivationNode> nodeQueue = new LinkedList<>();
        nodeQueue.offer(tree.getRoot());
        
        while (!nodeQueue.isEmpty()) {
            DerivationNode currentNode = nodeQueue.poll();
            
            if (currentNode.getGoal().isEmpty()) {
                // Nodo successo - obiettivo vuoto
                currentNode.setSuccess(true);
                continue;
            }
            
            // Prova risoluzione con ogni clausola applicabile
            Term leftmostGoal = selectLeftmostGoal(currentNode.getGoal().getTerm());
            String predicateIndicator = getPredicateIndicator(leftmostGoal);
            
            List<Clause> applicableClauses = kb.getClauses(predicateIndicator);
            
            for (Clause clause : applicableClauses) {
                ResolutionStep step = performResolutionStep(
                    currentNode.getGoal(), clause, new ExecutionContext());
                
                if (step.isSuccess()) {
                    DerivationNode childNode = new DerivationNode(
                        step.getResolvent(), currentNode, clause, step.getSubstitution());
                    
                    currentNode.addChild(childNode);
                    nodeQueue.offer(childNode);
                }
            }
            
            if (currentNode.getChildren().isEmpty()) {
                // Nodo fallimento - nessuna clausola applicabile
                currentNode.setFailure(true);
            }
        }
        
        return tree;
    }
}
```

### 7.6.3 Completezza e Correttezza

Il Prolog puro con risoluzione SLD è corretto e completo per la logica clausole Horn entro certi vincoli.

#### Analisi Correttezza e Completezza

```java
/**
 * Analisi proprietà correttezza e completezza
 */
public class CompletenessAnalyzer {
    
    /**
     * Controlla correttezza: tutte risposte calcolate sono corrette
     */
    public boolean checkSoundness(String predicateIndicator, List<Clause> clauses,
                                 List<Term> computedAnswers, KnowledgeBase kb) {
        
        // Per ogni risposta calcolata, verifica che sia conseguenza logica
        for (Term answer : computedAnswers) {
            if (!isLogicalConsequence(answer, clauses, kb)) {
                return false; // Non corretto - calcolata risposta incorretta
            }
        }
        
        return true; // Tutte risposte calcolate sono corrette
    }
    
    /**
     * Controlla completezza: tutte risposte corrette sono calcolate
     */
    public CompletenessResult checkCompleteness(String predicateIndicator, 
                                               List<Clause> clauses,
                                               List<Term> computedAnswers, 
                                               KnowledgeBase kb) {
        
        CompletenessResult result = new CompletenessResult();
        
        // Genera tutte possibili risposte corrette (teorico)
        Set<Term> theoreticalAnswers = generateTheoreticalAnswers(predicateIndicator, clauses, kb);
        
        Set<Term> computedSet = new HashSet<>(computedAnswers);
        
        // Controlla se tutte risposte teoriche sono state calcolate
        for (Term theoreticalAnswer : theoreticalAnswers) {
            if (!computedSet.contains(theoreticalAnswer)) {
                result.addMissedAnswer(theoreticalAnswer);
            }
        }
        
        result.setComplete(result.getMissedAnswers().isEmpty());
        result.setComputedAnswers(computedAnswers.size());
        result.setTheoreticalAnswers(theoreticalAnswers.size());
        
        return result;
    }
    
    /**
     * Controlla se termine è conseguenza logica delle clausole
     */
    private boolean isLogicalConsequence(Term answer, List<Clause> clauses, KnowledgeBase kb) {
        // Controllo semplificato - in pratica servirebbe ragionamento logico completo
        
        // Crea negazione risposta e prova a derivare contraddizione
        Term negatedAnswer = negate(answer);
        
        // Prova a dimostrare risposta negata porta a contraddizione
        return !canDerive(negatedAnswer, clauses, kb);
    }
    
    /**
     * Genera tutte risposte teoriche (può essere infinito)
     */
    private Set<Term> generateTheoreticalAnswers(String predicateIndicator, 
                                                List<Clause> clauses, 
                                                KnowledgeBase kb) {
        Set<Term> answers = new HashSet<>();
        
        // Usa ricerca limitata per evitare generazione infinita
        int maxDepth = 10; // Limite configurabile
        
        generateAnswersWithBound(predicateIndicator, clauses, kb, answers, maxDepth);
        
        return answers;
    }
    
    private void generateAnswersWithBound(String predicateIndicator, 
                                         List<Clause> clauses, 
                                         KnowledgeBase kb,
                                         Set<Term> answers, 
                                         int remainingDepth) {
        if (remainingDepth <= 0) return;
        
        // Genera risposte fino a profondità limitata
        // Implementazione userebbe esplorazione sistematica alberi dimostrazione
    }
}
```

### 7.6.4 Complessità Computazionale

Comprendere la complessità computazionale dei programmi Prolog puro aiuta nel progettare algoritmi efficienti.

#### Framework Analisi Complessità

```java
/**
 * Analizza complessità computazionale predicati Prolog
 */
public class ComplexityAnalyzer {
    
    /**
     * Analizza complessità temporale del predicato
     */
    public ComplexityResult analyzeTimeComplexity(String predicateIndicator, 
                                                 List<Clause> clauses) {
        
        ComplexityResult result = new ComplexityResult(predicateIndicator);
        
        // Analizza ogni clausola
        for (Clause clause : clauses) {
            ClauseComplexity clauseComplexity = analyzeClauseComplexity(clause);
            result.addClauseComplexity(clauseComplexity);
        }
        
        // Determina complessità generale
        result.computeOverallComplexity();
        
        return result;
    }
    
    /**
     * Analizza complessità clausola individuale
     */
    private ClauseComplexity analyzeClauseComplexity(Clause clause) {
        ClauseComplexity complexity = new ClauseComplexity(clause);
        
        if (clause.getBody() == null) {
            // Fatto - tempo costante
            complexity.setTimeComplexity(ComplexityClass.CONSTANT);
            return complexity;
        }
        
        // Analizza complessità corpo
        int recursiveCallsCount = countRecursiveCalls(clause);
        int nonRecursiveGoalsCount = countNonRecursiveGoals(clause);
        
        if (recursiveCallsCount == 0) {
            // Non ricorsivo - dipende dalle complessità obiettivi
            complexity.setTimeComplexity(ComplexityClass.POLYNOMIAL);
        } else if (recursiveCallsCount == 1) {
            // Ricorsione lineare
            if (hasStructuralRecursion(clause)) {
                complexity.setTimeComplexity(ComplexityClass.LINEAR);
            } else {
                complexity.setTimeComplexity(ComplexityClass.EXPONENTIAL);
            }
        } else {
            // Chiamate ricorsive multiple
            complexity.setTimeComplexity(ComplexityClass.EXPONENTIAL);
        }
        
        return complexity;
    }
    
    /**
     * Controlla pattern ricorsione strutturale
     */
    private boolean hasStructuralRecursion(Clause clause) {
        // Controlla se chiamate ricorsive operano su argomenti strutturalmente più piccoli
        
        Term head = clause.getHead();
        Term body = clause.getBody();
        
        // Cerca pattern come elaborazione liste: [H|T] -> T
        if (head instanceof CompoundTerm && body instanceof CompoundTerm) {
            CompoundTerm headCompound = (CompoundTerm) head;
            
            // Controlla ogni posizione argomento per decremento strutturale
            for (int i = 1; i <= headCompound.getArity(); i++) {
                Term arg = headCompound.getArgument(i);
                if (isListStructure(arg) && recursiveCallUsesStructurallySmaller(body, arg)) {
                    return true;
                }
            }
        }
        
        return false;
    }
    
    private boolean isListStructure(Term term) {
        if (term instanceof CompoundTerm) {
            CompoundTerm compound = (CompoundTerm) term;
            return ".".equals(compound.getFunctor()) && compound.getArity() == 2;
        }
        return false;
    }
    
    /**
     * Classi complessità per predicati Prolog
     */
    public enum ComplexityClass {
        CONSTANT("O(1)"),
        LOGARITHMIC("O(log n)"),
        LINEAR("O(n)"),
        POLYNOMIAL("O(n^k)"),
        EXPONENTIAL("O(2^n)"),
        FACTORIAL("O(n!)"),
        UNKNOWN("Sconosciuto");
        
        private final String notation;
        
        ComplexityClass(String notation) {
            this.notation = notation;
        }
        
        public String getNotation() {
            return notation;
        }
    }
}
```

### 7.6.5 Lettura Dichiarativa vs. Procedurale

I programmi Prolog puro possono essere letti sia dichiarativamente (quali relazioni valgono) che proceduralmente (come procedono le computazioni).

#### Esempio: Interpretazione Dichiarativa vs. Procedurale

```prolog
% Predicato esempio: append/3
append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).

% Lettura dichiarativa:
% "La relazione append vale tra tre liste X, Y, Z quando:
%  1. Se X è vuota, allora Y e Z sono la stessa lista, OPPURE
%  2. Se X ha testa H e coda T, allora Z ha testa H e coda R,
%     dove la relazione append vale tra T, Y, e R."

% Lettura procedurale:
% "Per appendere lista X a lista Y dando risultato Z:
%  1. Se X è vuota, allora Z è Y
%  2. Se X ha testa H e coda T, allora:
%     - Rendi H la testa di Z
%     - Ricorsivamente appendi T a Y per ottenere coda R di Z"
```

#### Analisi Dichiarativa-Procedurale

```java
/**
 * Analizza aspetti dichiarativi vs procedurali dei predicati Prolog
 */
public class DeclarativeProceduralAnalyzer {
    
    /**
     * Genera interpretazioni sia dichiarative che procedurali
     */
    public PredicateInterpretation analyzeInterpretations(String predicateIndicator, 
                                                         List<Clause> clauses) {
        
        PredicateInterpretation interpretation = new PredicateInterpretation(predicateIndicator);
        
        // Genera interpretazione dichiarativa
        String declarativeReading = generateDeclarativeReading(clauses);
        interpretation.setDeclarativeReading(declarativeReading);
        
        // Genera interpretazione procedurale
        String proceduralReading = generateProceduralReading(clauses);
        interpretation.setProceduralReading(proceduralReading);
        
        // Analizza direzionalità
        DirectionalityAnalysis directionality = analyzeDirectionality(clauses);
        interpretation.setDirectionalityAnalysis(directionality);
        
        return interpretation;
    }
    
    /**
     * Genera interpretazione dichiarativa
     */
    private String generateDeclarativeReading(List<Clause> clauses) {
        StringBuilder reading = new StringBuilder();
        reading.append("La relazione vale quando:\n");
        
        for (int i = 0; i < clauses.size(); i++) {
            Clause clause = clauses.get(i);
            reading.append("  ").append(i + 1).append(". ");
            
            if (clause.getBody() == null) {
                reading.append("La relazione ").append(clause.getHead().toString())
                       .append(" è vera.\n");
            } else {
                reading.append("Se ").append(formatBodyAsCondition(clause.getBody()))
                       .append(", allora ").append(clause.getHead().toString())
                       .append(" vale.\n");
            }
        }
        
        return reading.toString();
    }
    
    /**
     * Genera interpretazione procedurale
     */
    private String generateProceduralReading(List<Clause> clauses) {
        StringBuilder reading = new StringBuilder();
        reading.append("Per risolvere questo obiettivo:\n");
        
        for (int i = 0; i < clauses.size(); i++) {
            Clause clause = clauses.get(i);
            reading.append("  Opzione ").append(i + 1).append(": ");
            
            if (clause.getBody() == null) {
                reading.append("Riuscire immediatamente se pattern ")
                       .append(clause.getHead().toString()).append(" fa match.\n");
            } else {
                reading.append("Se pattern ").append(clause.getHead().toString())
                       .append(" fa match, allora eseguire: ")
                       .append(formatBodyAsProcedure(clause.getBody())).append("\n");
            }
        }
        
        return reading.toString();
    }
    
    /**
     * Analizza come predicato può essere usato direzionalmente
     */
    private DirectionalityAnalysis analyzeDirectionality(List<Clause> clauses) {
        DirectionalityAnalysis analysis = new DirectionalityAnalysis();
        
        // Per ogni clausola, determina quali argomenti possono essere input vs output
        for (Clause clause : clauses) {
            ClauseDirectionality directionality = analyzeClauseDirectionality(clause);
            analysis.addClauseDirectionality(directionality);
        }
        
        // Determina modi uso generale
        analysis.computeUsageModes();
        
        return analysis;
    }
    
    private ClauseDirectionality analyzeClauseDirectionality(Clause clause) {
        ClauseDirectionality directionality = new ClauseDirectionality(clause);
        
        Term head = clause.getHead();
        if (head instanceof CompoundTerm) {
            CompoundTerm headCompound = (CompoundTerm) head;
            
            // Analizza ogni posizione argomento
            for (int i = 1; i <= headCompound.getArity(); i++) {
                Term arg = headCompound.getArgument(i);
                ArgumentMode mode = determineArgumentMode(arg, clause.getBody());
                directionality.setArgumentMode(i, mode);
            }
        }
        
        return directionality;
    }
    
    private ArgumentMode determineArgumentMode(Term arg, Term body) {
        if (arg.isVariable()) {
            // Argomento variabile - controlla se è vincolato dal corpo
            if (body != null && isVariableConstrained((Variable) arg, body)) {
                return ArgumentMode.INPUT_OUTPUT;
            } else {
                return ArgumentMode.OUTPUT;
            }
        } else {
            // Termine ground - deve essere input
            return ArgumentMode.INPUT;
        }
    }
    
    public enum ArgumentMode {
        INPUT,          // Deve essere legato (ground) quando chiamato
        OUTPUT,         // Verrà legato dal predicato
        INPUT_OUTPUT,   // Può essere legato o non legato
        BIDIRECTIONAL   // Funziona in entrambe direzioni
    }
}
```

Questo capitolo completo sulla programmazione Prolog puro fornisce i fondamenti teorici e la guida pratica necessari per programmazione logica efficace in JProlog. Comprendere questi concetti abilita sviluppatori a scrivere programmi Prolog efficienti, manutenibili e logicamente corretti che sfruttano la piena potenza della programmazione dichiarativa.

## Conclusione

La programmazione in Prolog puro richiede comprendere l'interazione tra relazioni logiche ed esecuzione computazionale. I principi chiave coperti in questo capitolo - ordine regole, terminazione, ordine obiettivi, gestione ridondanza, pattern ricorsivi e fondamenti teorici - forniscono il framework per scrivere programmi Prolog efficaci.

L'implementazione JProlog di questi concetti dimostra l'applicazione pratica della teoria programmazione logica, abilitando sviluppatori a creare soluzioni dichiarative robuste ed efficienti a problemi complessi. L'equilibrio tra chiarezza dichiarativa ed efficienza procedurale è al cuore della programmazione Prolog di successo.