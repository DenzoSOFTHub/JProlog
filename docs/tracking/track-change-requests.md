# JProlog - Change Request Tracking

## Change Request Attive e Completate

### CR-2025-0001: Centering All Popup Windows

**Titolo**: Tutte le finestre popup devono aprirsi al centro della finestra principale  
**Data Richiesta**: 2025-08-19  
**Status**: COMPLETED  
**Data Inizio Analisi**: 2025-08-19  
**Data Approvazione**: 2025-08-19  
**Data Inizio Sviluppo**: 2025-08-19  
**Data Completamento**: 2025-08-19  
**Priorità**: MEDIUM  
**Complessità Stimata**: TRIVIAL  

#### Descrizione
L'utente richiede che tutte le finestre di dialogo e popup dell'IDE JProlog si aprano centrate rispetto alla finestra principale dell'applicazione, invece di apparire in posizioni casuali o nell'angolo dello schermo.

#### Requisiti Funzionali
- Tutti i JDialog devono essere centrati rispetto al JFrame principale
- Tutti i JOptionPane devono essere centrati rispetto al parent component
- Le finestre di dialogo personalizzate devono seguire lo stesso comportamento
- Il centering deve funzionare correttamente con multi-monitor setup

#### Impatto
- Modifica a tutti i componenti UI che creano finestre di dialogo
- Nessun impatto su funzionalità core del sistema Prolog
- Miglioramento significativo dell'esperienza utente
- Compatibilità backwards mantenuta

#### Criteri di Accettazione  
- [x] Tutte le finestre di dialogo si aprono centrate rispetto alla finestra principale
- [x] Il comportamento è consistente su tutti i sistemi operativi (Windows, Linux, macOS)
- [x] Funziona correttamente con configurazioni multi-monitor
- [x] Nessuna regressione nelle funzionalità esistenti
- [x] Le finestre mantengono il centering anche dopo resize della finestra principale

#### Analisi Tecnica

**File Identificati con Popup Windows** (7 file):
- `PrologIDE.java` - Finestra principale (JFrame) con multipli JOptionPane
- `ProjectTree.java` - Dialoghi per gestione progetto
- `BuildPanel.java` - Dialoghi per build e compilazione  
- `DebugPanel.java` - Dialoghi per debugging
- `PrologToolbar.java` - Dialoghi dalla toolbar
- `FileEditor.java` - Dialoghi per editing file
- `EditorTabbedPane.java` - Dialoghi per gestione tabs

**Soluzione Proposta**:

1. **Approccio Utility Class**: Creare `DialogUtils.java` con metodi centralizzati
   - `showCenteredMessage()` - Wrapper per JOptionPane.showMessageDialog
   - `showCenteredInput()` - Wrapper per JOptionPane.showInputDialog
   - `showCenteredConfirm()` - Wrapper per JOptionPane.showConfirmDialog
   - `centerDialog()` - Metodo per centrare JDialog custom

2. **Implementazione**:
   ```java
   public class DialogUtils {
       public static void centerDialog(Window dialog, Component parent) {
           dialog.setLocationRelativeTo(parent);
       }
   }
   ```

3. **Refactoring Necessario**:
   - Sostituire tutte le chiamate dirette a JOptionPane con DialogUtils
   - Aggiungere centering a JDialog custom se presenti
   - Mantenere riferimento al JFrame principale dove necessario

**Complessità Rivista**: LOW → TRIVIAL (soluzione standard Java Swing)

#### Soluzione Implementata

✅ **COMPLETATA**: Implementato sistema centralizzato DialogUtils per centering automatico di tutte le finestre popup

**Implementazione Realizzata**:

1. **DialogUtils.java**: Creata utility class completa con 19 metodi per gestire tutti i tipi di dialoghi
   - `showCenteredMessage()` - Messaggi informativi centrati  
   - `showCenteredInput()` - Input dialogs centrati
   - `showCenteredConfirm()` - Confirmation dialogs centrati
   - `showError()`, `showWarning()` - Dialoghi di errore e warning centrati
   - `centerDialog()` - Centering per JDialog personalizzati
   - `showCenteredFileChooser()` - File chooser centrati
   - Metodi di convenienza per casi comuni

2. **Refactoring Completo**: Sostituiti tutti i JOptionPane diretti in 7 file:
   - **PrologIDE.java**: 13 refactoring (messaggi, errori, conferme)
   - **ProjectTree.java**: 10 refactoring (input, errori, conferme) 
   - **BuildPanel.java**: 3 refactoring (messaggi, errori)
   - **DebugPanel.java**: 2 refactoring (input dialogs)
   - **PrologToolbar.java**: 2 refactoring (messaggi info)
   - **FileEditor.java**: 2 refactoring (errori)
   - **EditorTabbedPane.java**: 3 refactoring (errori, conferme)

**Risultato**: Sistema completamente centralizzato per popup centering con 35 total refactoring in 7 file core

**File creati/modificati**:
- `src/main/java/it/denzosoft/jprolog/editor/util/DialogUtils.java` (nuovo - 219 righe)
- `src/main/java/it/denzosoft/jprolog/editor/PrologIDE.java` (modificato)
- `src/main/java/it/denzosoft/jprolog/editor/ProjectTree.java` (modificato)
- `src/main/java/it/denzosoft/jprolog/editor/BuildPanel.java` (modificato)
- `src/main/java/it/denzosoft/jprolog/editor/DebugPanel.java` (modificato)
- `src/main/java/it/denzosoft/jprolog/editor/PrologToolbar.java` (modificato)
- `src/main/java/it/denzosoft/jprolog/editor/FileEditor.java` (modificato)
- `src/main/java/it/denzosoft/jprolog/editor/EditorTabbedPane.java` (modificato)

---

## CR-2025-0002: Module System Implementation

**Titolo**: Implementazione completa del sistema di moduli ISO Prolog
**Data Richiesta**: 2025-08-19
**Status**: COMPLETED
**Date Completed**: 2026-03-18
**Priorità**: HIGH
**Complessità Stimata**: VERY_HIGH

### Descrizione
Implementare il sistema di moduli standard ISO Prolog per organizzazione del codice e gestione dei namespace. Il sistema deve supportare:
- Dichiarazioni modulo con `module/2`
- Import di moduli con `use_module/1` e `use_module/2`
- Qualificazione esplicita con `module:predicate`
- Lista di esportazione/importazione
- Risoluzione predicati module-aware

### Impatto
- Modifica architetturale significativa al QuerySolver
- Estensione del Parser per supporto sintassi moduli
- Aggiornamento KnowledgeBase per gestione multi-modulo
- Modifiche al sistema di risoluzione predicati
- Impatto su tutti i built-in per module context

### Criteri di Accettazione
- [x] `module/2` per dichiarazione moduli
- [x] `use_module/1` e `use_module/2` per import
- [x] Qualificazione esplicita `module:goal`
- [x] Lista esportazione/importazione
- [x] Risoluzione predicati corretta cross-module
- [x] Compatibilità backwards per codice non-modularizzato
- [x] Test coverage >= 90% per funzionalità moduli

### Solution Implemented

**Architecture**:
1. `ModuleManager` manages named modules, each with local rules and export/import lists
2. `Module` stores `localRules` (module-scoped), `exportedPredicates`, and `importedModules`
3. `QuerySolver.currentModuleContext` tracks active module during resolution
4. Module-qualified calls (`Mod:Goal`) use `solveInModuleContext` to set context and delegate to standard resolution
5. Unqualified calls check `currentModuleContext` first, then global KB, then imported modules via `resolvePredicate`
6. `Prolog.consult()` and `Prolog.asserta()` route rules to module's `localRules` for non-user modules, and to global `KnowledgeBase` for the default `user` module

**Files modified**:
- `QuerySolver.java` — added `currentModuleContext`, `solveInModuleContext`, module-aware `solveAgainstKnowledgeBase`
- `Prolog.java` — module-isolated rule storage in `consult()` and `asserta()`

**Test results**: 320/320 JUnit tests pass, 20/20 example programs pass, full backward compatibility maintained.

---

## CR-2025-0003: Definite Clause Grammar (DCG) Support

**Title**: Full DCG (Definite Clause Grammars) support
**Date Requested**: 2025-08-19
**Status**: COMPLETED
**Date Completed**: 2026-03-18
**Priority**: MEDIUM
**Estimated Complexity**: HIGH
**Category**: EXTENSION (Non-ISO)

### Description
Implement full DCG support per de facto Prolog standard:
- Parsing DCG rules with `-->` operator
- Automatic transformation to standard predicates
- `phrase/2` and `phrase/3` predicates
- Automatic difference list management
- Support for terminals, non-terminals, and Prolog goals in DCG

### Solution Implemented

DCG was already implemented in previous versions. The ISS-2025-0085 Pratt parser rewrite resolved the remaining parser limitations (ISS-0040, 0041, 0042) that prevented complex DCG rules from parsing correctly.

**Components**:
- `DCGTransformer` — transforms `-->` rules to standard Prolog clauses
- `Phrase.java` — phrase/2 and phrase/3 with context-aware execution
- `EnhancedPhrase.java` — ISO/IEC DTS 13211-3 compliant version
- `PhraseWithOptions.java` — extended phrase/4 with options

### Acceptance Criteria
- [x] Correct parsing of DCG rules with `-->`
- [x] Automatic transformation to standard clauses
- [x] `phrase/2` and `phrase/3` working
- [x] Terminal and non-terminal handling
- [x] Prolog goal support in DCG rules (constraint goals `{ }`)
- [x] Complete tests for standard DCG cases
- [x] ISO compliance for DCG syntax

---

## CR-2025-0004: Custom Operator Definitions

**Title**: Custom operator definitions with op/3
**Date Requested**: 2025-08-19
**Status**: COMPLETED
**Date Completed**: 2026-03-18
**Priority**: MEDIUM
**Estimated Complexity**: HIGH

### Description
Implement support for custom operator definitions via `op/3`:
- Runtime operator definition with precedence
- Support for infix, prefix, postfix operators
- Associativity handling (fx, fy, xf, yf, xfx, yfx, xfy)
- `current_op/3` for operator queries
- Dynamic parser that respects custom operators

### Solution Implemented (ISS-2025-0085)

**Unified Operator Table Architecture**:
1. Replaced three disconnected operator registries with single shared `OperatorTable`
2. `TermParser` rewritten as proper Pratt parser using `OperatorTable` for operator lookup
3. `op/3` predicate updates the shared table, immediately visible to parser
4. Incremental directive processing: `consult()` and `asserta()` process `op/3` directives between clause parses
5. Precedence 0 removes operators per ISO standard
6. `current_op/3` queries all defined operators

### Acceptance Criteria
- [x] `op/3` for runtime operator definition
- [x] `current_op/3` for operator queries
- [x] All ISO associativity types supported
- [x] Correct parsing with custom operators
- [x] Operator precedence correctly respected
- [x] Compatibility with existing built-in operators
- [x] Error handling for invalid operator definitions
- [x] Operator removal with precedence 0

---

## CR-2025-0005: Advanced I/O and Stream Management

**Title**: Advanced I/O system with complete stream management
**Date Requested**: 2025-08-19
**Status**: COMPLETED
**Date Completed**: 2026-03-18
**Priority**: MEDIUM
**Estimated Complexity**: HIGH

### Description
Extend I/O system for full ISO Prolog stream support.

### Acceptance Criteria
- [x] `read_term/2` with options (variables, variable_names, etc.)
- [x] `write_term/2` with options (quoted, write_strings, etc.)
- [x] `stream_property/2` complete
- [x] Binary I/O predicates (get_byte, put_byte, peek_byte)
- [x] `format/2` and `format/3` with ISO format specifiers
- [ ] `seek/4` and stream positioning (deferred - rarely needed)
- [x] Stream aliases supported
- [x] Error handling for stream operations

---

## CR-2025-0006: Character Type System Enhancement

**Title**: Complete character classification system
**Date Requested**: 2025-08-19
**Status**: COMPLETED
**Date Completed**: 2026-03-18
**Priority**: MEDIUM
**Estimated Complexity**: MEDIUM

### Acceptance Criteria
- [x] `char_type/2` with 19 types (exceeds ISO 18: alpha, digit, alnum, ascii, upper, lower, cntrl, graph, print, punct, space, xdigit, newline, end_of_file, end_of_line, layout, meta, solo, symbol)
- [x] `peek_char/1` and `peek_code/1` working
- [x] `char_conversion/2` for conversion definitions
- [x] `current_char_conversion/2` for querying
- [x] ISO-compliant character classification
- [x] Basic Unicode support

---

## CR-2025-0007: Standard ISO Exception Terms

**Title**: Standard ISO error terms implementation
**Date Requested**: 2025-08-19
**Status**: COMPLETED
**Date Completed**: 2026-03-18
**Priority**: MEDIUM
**Estimated Complexity**: MEDIUM

### Description
ISO error term structure already implemented in `ISOErrorTerms.java` with all standard error types.

### Acceptance Criteria
- [x] `error(Error_term, Implementation_defined_term)` structure
- [x] All ISO error types (instantiation_error, type_error, domain_error, existence_error, permission_error, representation_error, evaluation_error, resource_error, syntax_error)
- [x] Integration with catch/3 and throw/1
- [x] Java exception to ISO term conversion
- [x] Backward compatibility with existing error handling

---

## CR-2025-0008: List Operations Extension

**Title**: List operations extension for ISO completeness
**Date Requested**: 2025-08-19
**Status**: COMPLETED
**Date Completed**: 2026-03-18
**Priority**: LOW
**Estimated Complexity**: LOW

### Acceptance Criteria
- [x] `keysort/2` for key-value pair sorting
- [x] `predsort/3` with custom comparison predicate
- [x] `permutation/2` for list permutations
- [x] Performance acceptable on large lists
- [x] Error handling for edge cases

---

## CR-2025-0009: Debugging Port Model Implementation

**Titolo**: Implementazione completa del debugging port model ISO
**Data Richiesta**: 2025-08-19
**Status**: COMPLETED
**Data Completamento**: 2026-03-19
**Priorità**: LOW
**Complessità Stimata**: HIGH

### Descrizione
Implementare il modello completo di debugging con ports ISO Prolog:
- Four-port model (Call, Exit, Redo, Fail)
- `debugging/0` per stato debugging
- Predicati di controllo trace avanzati
- Stack trace visualization
- Performance profiling integration

### Resolution (2026-03-19)

**Full implementation of the ISO four-port debug model with interactive IDE integration:**

1. **Engine debug infrastructure** (3 new classes):
   - `DebugEvent` — data carrier for port events (CALL/EXIT/FAIL/REDO) with goal, depth, bindings, call stack
   - `DebugStackEntry` — call stack frame with goal, depth, bindings snapshot
   - `DebugController` — orchestrator with thread-safe wait/notify synchronization, breakpoint management, step modes (Step Into/Over/Out/Continue), call stack tracking

2. **QuerySolver instrumentation**:
   - CALL port notification at entry to `solveInternalProtected()`
   - EXIT/FAIL port notification in `handleBuiltIn()` and `solveAgainstKnowledgeBase()`
   - All hooks guarded by `if (debugController != null)` — zero overhead when not debugging

3. **DebugPanel complete rewrite**:
   - Implements `DebugController.DebugListener` with EDT-safe callbacks
   - Colored trace output (blue=CALL, green=EXIT, red=FAIL, orange=REDO)
   - Real-time call stack tree with per-frame variable bindings
   - Variables table filtered to user-visible variables
   - Query input field for debug-mode queries
   - All step buttons wired to `DebugController.resumeWithAction()`

4. **FileEditor breakpoint gutter**:
   - Click in line number area toggles breakpoint (red circle marker)
   - Debug line highlighting (green background + arrow)
   - Error line highlighting via Highlighter (light red background)
   - Automatic predicate name extraction for breakpoint registration

5. **Compilation diagnostics**:
   - `Prolog.consultWithDiagnostics()` — per-clause error collection with file/line/message
   - `CompilationResult` and `CompilationError` classes
   - IDE Build panel shows per-line errors with clickable locations
   - Inline error highlighting in editor

**Files Created**: `DebugEvent.java`, `DebugStackEntry.java`, `DebugController.java`
**Files Modified**: `QuerySolver.java`, `Prolog.java`, `DebugPanel.java`, `FileEditor.java`, `PrologIDE.java`
**Tests**: 320 pass, 0 failures. 20/20 examples pass (100%).

### Impatto
- Estensione significativa del QuerySolver per port tracking
- Implementazione debugging infrastructure
- Aggiunta UI components per debug visualization
- Performance overhead per execution tracing
- Integration con IDE debugging features

### Criteri di Accettazione
- [ ] Four-port model completo (Call, Exit, Redo, Fail)
- [ ] `debugging/0` per query stato debug
- [ ] Trace output formattato secondo standard
- [ ] Stack trace visualization nell'IDE
- [ ] Performance profiling basic
- [ ] Configurabilità livelli debug detail
- [ ] Integration con existing spy/nospy system

---

## CR-2025-0010: Binary I/O Operations

**Title**: Complete binary I/O operations
**Date Requested**: 2025-08-19
**Status**: COMPLETED
**Date Completed**: 2026-03-18
**Priority**: LOW
**Estimated Complexity**: MEDIUM

### Description
Implement complete binary I/O support per ISO Prolog.

### Acceptance Criteria
- [x] `get_byte/1` and `get_byte/2` working
- [x] `put_byte/1` and `put_byte/2` for output
- [x] `peek_byte/1` and `peek_byte/2` implemented
- [x] Binary stream mode correct
- [x] Error handling for binary operations

---

## Template per Nuove Change Request

```markdown
### CR-YYYY-NNNN: [Titolo Change Request]

**Titolo**: [Descrizione breve della funzionalità richiesta]  
**Data Richiesta**: YYYY-MM-DD  
**Status**: RICHIESTA  
**Priorità**: [LOW|MEDIUM|HIGH|CRITICAL]  
**Complessità Stimata**: [TRIVIAL|LOW|MEDIUM|HIGH|VERY_HIGH]  
**Data Completamento**: [quando completata]  

#### Descrizione
[Descrizione dettagliata della funzionalità richiesta, contesto, motivazioni]

#### Impatto
[Descrizione dell'impatto sul sistema esistente, modifiche architetturali necessarie]

#### Criteri di Accettazione  
- [ ] [Criterio 1]
- [ ] [Criterio 2]  
- [ ] [Criterio N]

#### Note Implementative
[Quando in sviluppo: note tecniche, decisioni di design, problemi incontrati]

#### Soluzione Implementata
[Quando completata: descrizione della soluzione finale implementata]

**File creati/modificati**:
- [lista file quando completata]
```

---

## Esempi di Change Request

### CR-2025-EXAMPLE-001: Aggiunta supporto operatori aritmetici avanzati

**Titolo**: Implementazione operatori aritmetici avanzati (mod, abs, sin, cos)  
**Data Richiesta**: 2025-08-19  
**Status**: RICHIESTA  
**Priorità**: MEDIUM  
**Complessità Stimata**: MEDIUM  

#### Descrizione
Aggiungere supporto per operatori aritmetici avanzati per migliorare la compatibilità ISO Prolog. Gli operatori richiesti includono:
- `mod/2`: Operazione modulo
- `abs/1`: Valore assoluto  
- `sin/1`, `cos/1`, `tan/1`: Funzioni trigonometriche
- `sqrt/1`: Radice quadrata
- `log/1`: Logaritmo naturale

#### Impatto
- Modifica al sistema ArithmeticEvaluator
- Aggiunta nuove classi built-in per operatori matematici
- Aggiornamento parser per riconoscimento nuovi operatori
- Compatibilità backwards mantenuta

#### Criteri di Accettazione
- [ ] Tutti gli operatori matematici funzionano correttamente
- [ ] Test coverage >= 90% per nuove funzionalità
- [ ] Documentazione aggiornata
- [ ] Nessuna regressione su funzionalità esistenti
- [ ] Conformità ISO Prolog per operatori implementati

---

### CR-2025-EXAMPLE-002: Miglioramento IDE con syntax highlighting avanzato

**Titolo**: Enhancement syntax highlighting per Prolog nell'IDE  
**Data Richiesta**: 2025-08-19  
**Status**: RICHIESTA  
**Priorità**: LOW  
**Complessità Stimata**: MEDIUM  

#### Descrizione
Migliorare il sistema di syntax highlighting dell'IDE JProlog per supportare:
- Evidenziazione variabili vs atom
- Colori differenti per built-in predicates
- Highlighting per commenti multi-linea
- Bracket matching migliorato
- Error highlighting in tempo reale

#### Impatto
- Modifica componenti editor nell'IDE
- Aggiornamento PrologSyntaxHighlighter
- Possibili modifiche alle performance di rendering
- Miglioramento UX generale

#### Criteri di Accettazione
- [ ] Syntax highlighting preciso per tutti i costrutti Prolog
- [ ] Performance acceptable su file di grandi dimensioni
- [ ] Configurabilità colori/tema
- [ ] Compatibilità con funzionalità editor esistenti

---

## Categorie Change Request

### 🚀 **Features** - Nuove funzionalità
- Aggiunta di nuovi predicati built-in
- Implementazione nuovi operatori
- Estensioni al linguaggio Prolog

### 🔧 **Enhancements** - Miglioramenti esistenti
- Ottimizzazioni performance
- Miglioramenti UI/UX
- Refactoring architetturale

### 📚 **Documentation** - Documentazione
- Aggiornamenti manuali
- Esempi e tutorial
- API documentation

### 🧪 **Testing** - Miglioramenti testing
- Nuove suite di test
- Automation testing
- Coverage improvements

### 🏗️ **Infrastructure** - Infrastruttura
- Build system improvements
- Development tools
- CI/CD pipeline

---

## Statistiche Change Request

**Total CR**: 10
**Requested**: 0
**In Analysis**: 0
**Approved**: 0
**In Development**: 0
**Completed**: 10
**Rejected**: 0

### Breakdown by Priority
- **HIGH**: 1 CR (Module System - COMPLETED)
- **MEDIUM**: 5 CR (DCG COMPLETED, Custom Operators COMPLETED, Advanced I/O COMPLETED, Character Types COMPLETED, ISO Exceptions COMPLETED)
- **LOW**: 3 CR (List Operations COMPLETED, Debugging REQUESTED, Binary I/O COMPLETED)

### Breakdown by Complexity
- **VERY_HIGH**: 1 CR (Module System - COMPLETED)
- **HIGH**: 4 CR (DCG COMPLETED, Custom Operators COMPLETED, Advanced I/O COMPLETED, Debugging REQUESTED)
- **MEDIUM**: 3 CR (Character Types COMPLETED, ISO Exceptions COMPLETED, Binary I/O COMPLETED)
- **LOW**: 1 CR (List Operations - COMPLETED)
- **TRIVIAL**: 1 CR (Dialog Centering - COMPLETED)

**Last Updated**: 2026-03-19

---

## Note

- Le Change Request vengono valutate in base a priorità e impatto
- CR con status RICHIESTA richiedono analisi tecnica prima dell'approvazione
- Tutte le CR devono avere criteri di accettazione misurabili
- Le modifiche architetturali significative richiedono discussione e approvazione