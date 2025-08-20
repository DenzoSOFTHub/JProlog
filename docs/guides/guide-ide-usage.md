# JProlog IDE - Interactive Prolog Environment

## Descrizione

L'IDE JProlog è un ambiente di sviluppo interattivo per programmi Prolog che permette di:

- Caricare programmi Prolog da file
- Eseguire query interattive sulla knowledge base
- Esplorare tutte le soluzioni tramite backtracking usando il carattere ";"
- Visualizzare i risultati con formattazione colorata

## Come Avviare l'IDE

### Opzione 1: Script di Avvio
```bash
./run_ide.sh
```

### Opzione 2: Script di Avvio Alternativo
```bash
./start-ide.sh
```

### Opzione 3: Comando Maven
```bash
mvn compile
java -cp target/classes it.denzosoft.jprolog.editor.PrologIDE
```

## Come Usare l'IDE

### 1. Caricamento di un Programma Prolog
- Clicca sul pulsante "Load Program" nell'area "Prolog Program"
- Seleziona un file .pl o .pro
- Il programma verrà caricato nell'area di testo e consultato da JProlog

### 2. Esecuzione di Query
- Digita una query nel campo "Interactive Query" 
- Premi Enter o clicca "Execute Query"
- Le soluzioni appariranno nell'area "Query Results"

### 3. Backtracking con ";"
- Dopo che viene mostrata una soluzione, se ce ne sono altre disponibili, apparirà il prompt ";"
- Premi il tasto ";" (punto e virgola) per vedere la prossima soluzione
- Continua a premere ";" per esplorare tutte le soluzioni disponibili
- Quando non ci sono più soluzioni, apparirà "false."

## File di Esempio

È incluso un file `example_program.pl` con esempi di:
- Fatti e regole sui rapporti familiari
- Predicati aritmetici
- Liste e membership
- Logica condizionale

### Query di Esempio da Provare

```prolog
% Trovare tutti i genitori
parent(X, Y).

% Trovare tutti i padri
father(X, Y).

% Trovare i nonni
grandparent(X, Z).

% Trovare tutti i colori
color(X).

% Test aritmetici
even(X).

% Classificazione numeri
classify_number(5, Type).
classify_number(-3, Type).
classify_number(0, Type).
```

## Funzionalità dell'IDE

### Area Programma
- Editor di testo per visualizzare e modificare programmi Prolog
- Pulsante "Load Program" per caricare file esterni

### Area Output
- Visualizzazione colorata dei risultati
- Blu: query eseguite
- Nero: risultati delle query
- Rosso: messaggi di errore
- Grigio scuro: prompt e messaggi di sistema
- Pulsante "Clear Output" per pulire l'area

### Area Query Interattiva
- Campo di input per le query
- Supporto per Enter (esegui query) e ";" (prossima soluzione)
- Pulsante "Execute Query" alternativo

### Barra di Stato
- Mostra lo stato corrente del sistema
- Indica il numero di soluzioni disponibili
- Segnala errori e completamento delle query

## Note Tecniche

- L'IDE supporta tutte le funzionalità del motore JProlog v2.0.5
- Include supporto completo per operatori condizionali (-> e ;)
- Gestisce correttamente il backtracking e le soluzioni multiple
- Supporta term manipulation predicates (functor/3, arg/3, =../2, copy_term/2)
- Meta-predicates completamente funzionali (findall/3, bagof/3, setof/3)
- Liste in formato ISO-compliant [a,b,c]
- Sistema DCG (Definite Clause Grammar) completamente operativo
- Interface utente responsive con aggiornamenti in tempo reale
- 95% di compatibilità con lo standard ISO Prolog

## Esempi di Backtracking

```prolog
% Carica il programma example_program.pl e prova:

?- parent(X, Y).
% Risultato: X = tom, Y = bob ;
% Premi ';' per vedere: X = tom, Y = liz ;
% Premi ';' per vedere: X = bob, Y = ann ;
% E così via...

?- color(X).
% Risultato: X = red ;
% Premi ';' per vedere: X = blue ;
% Premi ';' per vedere: X = green ;
% Premi ';' per vedere: false.
```