# JProlog IDE — Analisi UX/Feature vs IDE moderni (2026-06-09)

Analisi del codice (`src/main/java/it/denzosoft/jprolog/editor/`, ~8.5k righe) su scrittura
codice, esecuzione, compilazione, debug, breakpoint e ispezione variabili, valutata rispetto agli
standard degli IDE moderni (VS Code, IntelliJ, Eclipse). Eseguita con review multi-agente (6 dimensioni).
La GUI non è stata avviata (analisi statica del codice).

**Bilancio**: 5 critical · 14 high · 21 medium · 12 low.

## Aggiornamento — correzioni applicate (2026-06-09)

Risolti i critici contenuti + 2 richieste aggiuntive (685/685 test, BUILD SUCCESS):
- ✅ **Undo/redo** (`UndoManager` su Document, Ctrl+Z / Ctrl+Y / Ctrl+Shift+Z, voci Edit menu, reset su load; salta gli edit style-only dell'highlighting).
- ✅ **Stop query reale** (ISS-0320): il loop `drive` del motore v2 polla `Thread.isInterrupted()` e lancia `QueryCancelledException` (non catchabile da `catch/3`); l'IDE già interrompe il thread → ora una query infinita si ferma. *(test: `testISS0320`)*
- ✅ **Soluzioni incrementali/cap** (ISS-0321): nuova API `Prolog.solveStream(query, sink)` (lazy via v2, cancellabile); `RunPanel` raccoglie con cap (5000) e indica "stopped after N". Niente più OOM/hang su query a molte soluzioni. *(test: `testISS0321`)*
- ✅ **Formattatore sorgente** (richiesta): `core.write.v2.PrologFormatter` (parser+writer v2) — fatti su una riga, corpo regola con un goal per riga indentato, riga vuota tra clausole, commenti inter-clausola preservati; Ctrl+Alt+L + voce Edit menu. *(8 test)*
- ✅ **Syntax highlighting responsivo** (richiesta + finding HIGH): aggiunto **debounce ~150ms** (re-style una volta a fine digitazione invece che a ogni tasto → niente lag). L'highlighter copre già commenti/stringhe/atomi/**variabili**/keyword/operatori/numeri/funtori/direttive.

### Breakpoint — RISOLTO (ISS-0322)

- `Rule.sourceLine` impostato al consult (`consultWithDiagnosticsV2`).
- `Prolog.getPredicateIndicatorAtLine(line)` — mapping **accurato** linea→predicato dalle vere righe
  delle clausole (non più regex sul testo). *(test `testISS0322`)*
- Il gutter click di `FileEditor` usa questo lookup (fallback regex solo se file non ancora consultato).
- **Persistenza breakpoint**: file sidecar `.<nome>.bps` salvato al toggle, ripristinato all'apertura.
- **Shortcut di stepping reali** in `DebugPanel`: F7/F8/Shift+F8/F9 (InputMap window-wide, attivi solo in pausa).

### HIGH risolti (11/14)

- ✅ Highlighting lag (debounce, sopra) · ✅ **Autocomplete** Ctrl+Space (predicati/builtin/variabili, ISS-0324)
- ✅ Bug `replaceAll` div-by-zero → conteggio via `Matcher` · ✅ **Highlight ricerca** rotto (`getTextArea` null) → riscritto su `JTextPane` (ISS-0323)
- ✅ **Ricerca console** Ctrl+F (RunPanel + OutputConsole) · ✅ **Problems view** cliccabile con jump-to-source (BuildPanel)
- ✅ **Shortcut stepping** reali · ✅ **Breakpoint model** (mapping accurato) · ✅ **Persistenza breakpoint**
- ✅ **Persistenza preferenze** (bounds/divider/font/recent) · ✅ **Session restore / Recent Projects** (PrologIDE)

### Ultimi 3 HIGH — CONCLUSI (14/14)

- ✅ **#1 `System.out` globale** (ISS-0327): `StreamManager` ha ora un **output thread-local** + `out()`;
  gli 8 builtin di output (write/writeln/nl/tab/put_char/put_code/write_term/format) scrivono lì.
  L'IDE cattura per-thread (niente `System.setOut` globale). `out()` legge `System.out` live → rispetta
  gli swap dei test e `with_output_to`. *(687/687)*
- ✅ **#2 squiggle inline + gutter** (ISS-0326): `highlightErrorLine` ora disegna un **underline rosso
  ondulato** persistente sul token, un **marker "!" nel gutter** e un **tooltip per-riga**; niente più
  hijack di caret/selezione.
- ✅ **#3 box-model debug** (ISS-0328): **corretta una regressione** — sotto v2-default `engine.solve`
  instradava su v2 (senza hook di debug) → il debugger non scattava. Nuovo `Prolog.solveLegacy()` forza
  il motore legacy (con gli hook a 4 porte); `DebugPanel` lo usa. Lo stepping port-by-port è
  incrementale (modello a due thread). *(Resta minore: consegna lazy della lista soluzioni finale.)*

### MEDIUM — 15/21 risolti (workflow multi-agente, un file per agente)

- ✅ **M01** bracket matching + auto-close ( ) [ ] { } ' " (highlight coppia + auto-chiusura, inerte in commenti/stringhe)
- ✅ **M03** salvataggio **UTF-8** (coerente col load) · ✅ **M04/M20** find **all'indietro** + highlight-all + contatore
- ✅ **M05** vista **tabella risultati** (colonna per variabile, riga per soluzione, copy/export CSV) · ✅ **M06** status/progress (timer elapsed + contatore live)
- ✅ **M08** diagnostica stale ripulita all'edit della riga · ✅ **M09** **Compile to .jpc** (Shift+F9)
- ✅ **M11** **Run to Cursor** + **Restart** debug · ✅ **M12** estrazione predicato accurata (già via `getPredicateIndicatorAtLine`)
- ✅ **M14** **Variables tree** espandibile (CompoundTerm→figli, liste→elementi) · ✅ **M15** **Watch expressions** (valutate ad ogni pausa)
- ✅ **M17** toolbar context-sensitive (wired) · ✅ **M19** **Go to Line** (Ctrl+G) + **Quick Open** (Ctrl+P) · ✅ **M21** toolbar Debug button ricablato

**MEDIUM non fatti (6) — con motivazione:**
- **M18** dark theme → richiede FlatLaf (dipendenza esterna, vietata dal vincolo no-deps del progetto).
- **M02** highlighting lexer-based → riscrittura grande; il **debounce** già mitiga il lag (HIGH #1 risolto).
- **M07** modello input terminale → riscrittura grande, basso valore vs REPL attuale.
- **M13** breakpoint condizionali/hit-count → modifica cross-file del modello `DebugController`.
- **M10/M16** current-line via source-mapping → parziale (`Rule.sourceLine` esiste); resta solo l'highlight euristico della riga in debug.

## Verdetto sintetico

Lo **scheletro** di un IDe reale c'è ed è sorprendentemente completo per un'app Swing scritta a mano:
layout a 3 pannelli, project tree con operazioni file, search-in-files, outline dei predicati,
diagnostica di build line-accurate, e — punto di forza maggiore — un **debugger a due thread ben
architettato** con box-model a 4 porte, binding live e call stack navigabile. Ma la **profondità** è
sottile: mancano fondamentali "table-stakes" (undo/redo, autocomplete, stop di una query, breakpoint
di linea reali), e diverse feature pubblicizzate sono di fatto non funzionanti.

---

## Per area

### 1. Scrittura codice — *funzionale ma di prima generazione*
**Forze**: gutter numeri-linea con clipping al viewport + marker breakpoint/debug; highlighter
comment/string-aware; UI find/replace con regex/case/whole-word; tab con dirty-marker e prompt salva.
**Gap critici/alti**:
- **[CRITICAL] Nessun undo/redo** — nessun `UndoManager`; Ctrl+Z/Y non fanno nulla. Da soli rendono
  l'editing serio impraticabile.
- **[HIGH] Highlighting full-document a ogni tasto** — `highlightSyntax()` ri-tokenizza tutto il
  documento (9 pass regex) su ogni keystroke, senza incrementale né debounce → lag su file grandi.
- **[HIGH] Nessun autocomplete** — niente completamento predicati/builtin/variabili (eppure
  registry e PredicatePanel esistono già come sorgenti).
- **[HIGH] Bug `replaceAll`** — divisione per zero quando search e replace hanno uguale lunghezza.
- **[MED] No bracket matching/auto-close**; current-line highlight disabilitato; find solo in avanti;
  mismatch encoding load(UTF-8)/save(default).

### 2. Esecuzione query — *REPL funzionale ma non cancellabile né incrementale*
**Forze**: query su worker thread (una query finita non blocca l'EDT); REPL con history Up/Down e
input multi-linea; cattura output side-effect distinta dai binding; bind mostrati `Name = Value`.
**Gap critici/alti**:
- **[CRITICAL] Stop non funziona** — Interrupt/Ctrl+C chiama `queryThread.interrupt()` ma né
  `Prolog.solve` né `QuerySolver.solve` controllano `isInterrupted()` → una query infinita non si
  ferma.
- **[CRITICAL] Soluzioni eager** — l'engine bufferizza TUTTE le soluzioni prima di mostrare qualcosa;
  niente `;` interattivo "prossima soluzione", nessun cap → query con molte/infinite soluzioni
  appendono o esauriscono memoria.
- **[HIGH] Nessuna ricerca nella console** (no Ctrl+F sull'output).
- **[HIGH] Redirect globale `System.out`** dal worker (non thread-confinato).

### 3. Compilazione/build — *l'area più solida*
**Forze**: `consultWithDiagnosticsV2` dà diagnostica per-clausola line-accurate con recovery (riporta
TUTTI gli errori); modello `CompilationError` strutturato; styling error/warning/success; marcatura
riga rossa + tooltip puliti prima di ogni rebuild; transcript con summary.
**Gap alti**:
- **[HIGH] Nessun "Problems view" cliccabile** — gli errori sono testo in un JTextPane read-only; niente
  lista navigabile né jump-to-source (il pattern clickable esiste ma non è collegato).
- **[HIGH] Niente squiggle inline** — solo sfondo riga rosso piatto, no underline a livello token, no
  marker nel gutter, nessuna colonna nell'errore.

### 4. Debugger (control flow) — *fondamenta solide, profondità scarsa*
**Forze (notevoli)**: architettura a due thread corretta (wait/notify, guardie spurious-wakeup, campi
volatile, callback marshallate su EDT); **Stop sicuro** via unwind cooperativo (no `Thread.stop`);
trace 4-porte color-coded e indentato; Step Over/Out con depth-target corretto.
**Gap critici/alti**:
- **[CRITICAL] Breakpoint di linea = illusione** — cliccare il gutter salva un numero di LINEA, ma il
  breakpoint reale è una stringa predicato/arità ricavata via regex da quella riga. L'engine non
  conosce le righe.
- **[HIGH] Shortcut di stepping non funzionano** — i bottoni hanno tooltip F7/F8/Shift+F8/F9 ma solo
  F7 è registrato (e mostra solo il pannello).
- **[HIGH] Box-model non incrementale** — il debug gira `engine.solve(query)` eager; non si può
  mettere in pausa per soluzione.

### 5. Breakpoint & ispezione variabili — *core Prolog-aware credibile, fondamentali deboli*
**Forze**: pausa sul box-model a 4 porte; **binding variabili live deep-resolved**; **call stack
navigabile** dove selezionare un frame ricarica le Variabili dallo snapshot di quel frame (immutabile
per-frame — evita il bug classico); gutter con dot e highlight riga corrente.
**Gap alti**:
- **[HIGH] Breakpoint predicate-based, non per-linea** (marker visuale e breakpoint funzionale
  disallineati).
- **[HIGH] Breakpoint non persistiti** — lista in memoria, persi alla chiusura; niente condizioni/hit-count.
- **[MED] Niente watch expressions**; le variabili non espandono i termini composti come albero.

### 6. UX generale — *superficie ampia, feature di punta non funzionanti*
**Forze**: layout 3-pannelli con splitter; project tree con create/rename/delete/refresh; search-in-files
con risultati ad albero navigabili; status bar ricca; outline predicati con go-to-definition; accelerator
standard sulle azioni core.
**Gap critici/alti**:
- **[CRITICAL] Niente undo/redo** (ribadito a livello editor/menu).
- **[HIGH] Preferenze non persistite** — `~/.jprolog-ide.properties` è caricato/salvato ma nessuna chiave
  viene mai scritta/letta (config "morta").
- **[HIGH] Nessun session restore / recent projects** — all'avvio nessun progetto/file/storia.
- **[HIGH] Highlight risultati di ricerca rotto** — `FileEditor.getTextArea()` ritorna sempre null → NPE
  inghiottito; l'evidenziazione colonna non funziona mai.

---

## Connessione con il nuovo motore v2 (importante)

Tre dei gap critical/high dell'esecuzione/debug derivano dal fatto che l'IDE è cablato sul **motore
legacy eager**. Il **motore v2** (ora default) ha già:
- **enumerazione lazy** + API a callback `m.solve(query, sol -> ...)` → abilita il `;` interattivo,
  il cap sulle soluzioni e il **box-model incrementale** (pausa per soluzione);
- iterazione su goal-stack → si presta a un **flag di cancellazione** pollato → Stop reale.

Ricablare RunPanel/DebugPanel sul callback del v2 risolverebbe in un colpo: soluzioni incrementali,
cancellazione, e debug per-soluzione.

---

## Raccomandazioni prioritizzate

**P0 (table-stakes mancanti)**
1. `UndoManager` su Document + Ctrl+Z/Y + voci Edit menu (fix sia "Code Editing" sia "Overall UX").
2. Stop query reale: flag `volatile interrupted` pollato nel loop SLD → `QueryInterruptedException`.
3. Esecuzione incrementale via callback del motore v2 (`;` prossima soluzione + cap).
4. Breakpoint di linea reali: taggare ogni `Rule`/clausola con (file, line) al consult, propagare al
   `DebugStackEntry`, confrontare (file,line) in `isBreakpointHit()`.

**P1 (qualità d'uso)**
5. Highlighting incrementale (re-style solo le righe modificate + debounce ~150ms).
6. Autocomplete (Ctrl+Space) da registry builtin + outline + variabili della clausola.
7. Problems view cliccabile (JTable: severità/msg/file/riga → openFile+goToLine) + squiggle inline.
8. Shortcut di stepping reali (InputMap/ActionMap F7/F8/Shift+F8/F9, abilitati in pausa).
9. Persistenza: preferenze (bounds, divider, font, recent), breakpoint, session restore.
10. Fix bug puntuali: `replaceAll` div-by-zero, `getTextArea()` null/NPE, encoding load/save UTF-8.

**P2 (rifiniture)**
- Bracket matching/auto-close · current-line highlight (leggero) · find bidirezionale · ricerca nella
  console · watch expressions · variabili composte espandibili ad albero · dark theme.
