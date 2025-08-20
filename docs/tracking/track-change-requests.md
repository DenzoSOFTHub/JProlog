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
**Status**: RICHIESTA  
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
- [ ] `module/2` per dichiarazione moduli
- [ ] `use_module/1` e `use_module/2` per import
- [ ] Qualificazione esplicita `module:goal`
- [ ] Lista esportazione/importazione
- [ ] Risoluzione predicati corretta cross-module
- [ ] Compatibilità backwards per codice non-modularizzato
- [ ] Test coverage >= 90% per funzionalità moduli

---

## CR-2025-0003: Definite Clause Grammar (DCG) Support

**Titolo**: Supporto completo per DCG (Definite Clause Grammars)  
**Data Richiesta**: 2025-08-19  
**Status**: RICHIESTA  
**Priorità**: MEDIUM  
**Complessità Stimata**: HIGH  
**Categoria**: EXTENSION (Non-ISO)

### Descrizione
Implementare supporto completo per DCG secondo de facto standard Prolog (NON ISO standard):
- Parsing regole DCG con operatore `-->`
- Trasformazione automatica in predicati standard
- Predicati `phrase/2` e `phrase/3`
- Gestione difference lists automatica
- Supporto per terminali, non-terminali e goal Prolog in DCG

### Impatto
- Estensione significativa del Parser per sintassi DCG
- Implementazione DCGTransformer per conversione regole
- Aggiunta predicati phrase/2 e phrase/3
- Modifica sistema parsing per riconoscimento `-->`
- Aggiornamento tokenizer per gestione DCG syntax

### Criteri di Accettazione
- [ ] Parsing corretto regole DCG con `-->`
- [ ] Trasformazione automatica in clausole standard
- [ ] `phrase/2` e `phrase/3` funzionanti
- [ ] Gestione terminali e non-terminali
- [ ] Supporto goal Prolog in DCG rules
- [ ] Test completi per casi DCG standard
- [ ] Conformità ISO per DCG syntax

---

## CR-2025-0004: Custom Operator Definitions

**Titolo**: Definizione operatori personalizzati con op/3  
**Data Richiesta**: 2025-08-19  
**Status**: RICHIESTA  
**Priorità**: MEDIUM  
**Complessità Stimata**: HIGH  

### Descrizione
Implementare supporto per definizione di operatori personalizzati tramite `op/3`:
- Definizione operatori runtime con precedenza
- Supporto per operatori infix, prefix, postfix
- Gestione associatività (fx, fy, xf, yf, xfx, yfx, xfy)
- `current_op/3` per interrogazione operatori
- Parser dinamico che rispetta operatori custom

### Impatto
- Modifica significativa al Parser per gestione operatori dinamici
- Implementazione operatore table runtime-modifiable
- Aggiornamento precedence handling nel TermParser
- Aggiunta predicati op/3 e current_op/3
- Testing estensivo per casi edge di precedenza

### Criteri di Accettazione
- [ ] `op/3` per definizione operatori runtime
- [ ] `current_op/3` per query operatori
- [ ] Supporto tutti i tipi associatività ISO
- [ ] Parsing corretto con operatori custom
- [ ] Precedenza operatori rispettata correttamente
- [ ] Compatibilità con operatori built-in esistenti
- [ ] Error handling per definizioni operatori invalide

---

## CR-2025-0005: Advanced I/O and Stream Management

**Titolo**: Sistema I/O avanzato con gestione stream completa  
**Data Richiesta**: 2025-08-19  
**Status**: RICHIESTA  
**Priorità**: MEDIUM  
**Complessità Stimata**: HIGH  

### Descrizione
Estendere il sistema I/O per supporto completo stream secondo ISO Prolog:
- `read_term/2` e `write_term/2` con opzioni
- `stream_property/2` per interrogazione stream
- Binary I/O con `get_byte/1`, `put_byte/1`
- Stream positioning con `seek/4`
- Format predicates `format/2`, `format/3`
- Gestione stream aliases

### Impatto
- Estensione significativa del StreamManager
- Implementazione parsing opzioni per read_term/2
- Aggiunta supporto binary streams
- Implementazione format string processor
- Modifica gestione stream per proprietà e positioning

### Criteri di Accettazione
- [ ] `read_term/2` con opzioni (variables, variable_names, etc.)
- [ ] `write_term/2` con opzioni (quoted, write_strings, etc.)
- [ ] `stream_property/2` completo
- [ ] Binary I/O predicates funzionanti
- [ ] `format/2` e `format/3` con format specifiers ISO
- [ ] `seek/4` e stream positioning
- [ ] Stream aliases supportati
- [ ] Error handling corretto per stream operations

---

## CR-2025-0006: Character Type System Enhancement

**Titolo**: Sistema completo di classificazione caratteri  
**Data Richiesta**: 2025-08-19  
**Status**: RICHIESTA  
**Priorità**: MEDIUM  
**Complessità Stimata**: MEDIUM  

### Descrizione
Implementare il sistema completo di classificazione caratteri ISO Prolog:
- `char_type/2` con tutti i 18 tipi ISO standard
- `peek_char/1`, `peek_code/1` per lettura non-distruttiva
- `char_conversion/2` per tabelle di conversione caratteri
- `current_char_conversion/2` per query conversioni
- Gestione completa character properties

### Impatto
- Estensione CharType per tutti i tipi ISO standard
- Implementazione peek operations sui streams
- Aggiunta character conversion table system
- Modifica I/O system per supporto peek operations
- Testing per compliance character classification

### Criteri di Accettazione
- [ ] `char_type/2` con 18 tipi ISO (alpha, digit, alnum, etc.)
- [ ] `peek_char/1` e `peek_code/1` funzionanti
- [ ] `char_conversion/2` per definizione conversioni
- [ ] `current_char_conversion/2` per query
- [ ] Character classification conforme ISO
- [ ] Gestione Unicode base per caratteri
- [ ] Test completi per tutti i tipi carattere

---

## CR-2025-0007: Standard ISO Exception Terms

**Titolo**: Implementazione termini di errore standard ISO  
**Data Richiesta**: 2025-08-19  
**Status**: RICHIESTA  
**Priorità**: MEDIUM  
**Complessità Stimata**: MEDIUM  

### Descrizione
Implementare la struttura standard dei termini di errore ISO Prolog:
- Formato `error(Error_term, Implementation_defined_term)`
- Tutti i tipi di errore ISO standard
- Integrazione con sistema `catch/3` e `throw/1` esistente
- Conversione delle eccezioni Java in termini ISO
- Predicato `abort/0` per terminazione controllata

### Impatto
- Creazione ISOErrorTerms utility class
- Modifica di tutti i built-in per generare errori ISO
- Aggiornamento exception handling per formato standard
- Standardizzazione error reporting attraverso il sistema
- Documentazione completa error types

### Criteri di Accettazione
- [ ] Struttura `error(Error_term, Implementation_defined_term)`
- [ ] Tutti i tipi errore ISO implementati (instantiation_error, type_error, etc.)
- [ ] `abort/0` per terminazione programma
- [ ] Conversione automatica eccezioni Java → ISO terms
- [ ] Integrazione completa con catch/3 e throw/1
- [ ] Documentazione error handling standard
- [ ] Backward compatibility con error handling esistente

---

## CR-2025-0008: List Operations Extension

**Titolo**: Estensione operazioni liste per completezza ISO  
**Data Richiesta**: 2025-08-19  
**Status**: RICHIESTA  
**Priorità**: LOW  
**Complessità Stimata**: LOW  

### Descrizione
Aggiungere le operazioni liste mancanti per completezza ISO Prolog:
- `keysort/2` per sorting su key-value pairs
- `predsort/3` per sorting con predicato custom
- `permutation/2` per generazione permutazioni
- Ottimizzazione existing list predicates
- Enhanced error handling per edge cases

### Impatto
- Aggiunta 3 nuovi predicati nel builtin/list package
- Ottimizzazioni performance per list operations
- Testing estensivo per correttezza algoritmi
- Minimal impact su architettura esistente

### Criteri di Accettazione
- [ ] `keysort/2` per sorting key-value pairs
- [ ] `predsort/3` con predicato comparazione custom
- [ ] `permutation/2` per permutazioni liste
- [ ] Performance acceptable su liste grandi
- [ ] Error handling robusto per casi edge
- [ ] Test coverage completo per nuovi predicates
- [ ] Documentazione per usage patterns

---

## CR-2025-0009: Debugging Port Model Implementation

**Titolo**: Implementazione completa del debugging port model ISO  
**Data Richiesta**: 2025-08-19  
**Status**: RICHIESTA  
**Priorità**: LOW  
**Complessità Stimata**: HIGH  

### Descrizione
Implementare il modello completo di debugging con ports ISO Prolog:
- Four-port model (Call, Exit, Redo, Fail)
- `debugging/0` per stato debugging
- Predicati di controllo trace avanzati
- Stack trace visualization
- Performance profiling integration

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

**Titolo**: Operazioni I/O binarie complete  
**Data Richiesta**: 2025-08-19  
**Status**: RICHIESTA  
**Priorità**: LOW  
**Complessità Stimata**: MEDIUM  

### Descrizione
Implementare supporto completo per I/O binario secondo ISO Prolog:
- `get_byte/1`, `get_byte/2` per lettura bytes
- `put_byte/1`, `put_byte/2` per scrittura bytes
- `peek_byte/1`, `peek_byte/2` per peek operations
- Binary stream mode support
- Efficient binary data handling

### Impatto
- Estensione StreamManager per binary modes
- Implementazione binary I/O predicates
- Testing per performance su binary data
- Documentation per binary I/O usage patterns

### Criteri di Accettazione
- [ ] `get_byte/1` e `get_byte/2` funzionanti
- [ ] `put_byte/1` e `put_byte/2` per output
- [ ] `peek_byte/1` e `peek_byte/2` implemented
- [ ] Binary stream mode corretto
- [ ] Performance acceptable per binary data
- [ ] Error handling per binary operations
- [ ] Integration con text/binary stream switching

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

**Totale CR**: 10  
**Richieste**: 9  
**In Analisi**: 0  
**Approvate**: 0  
**In Sviluppo**: 0  
**Completate**: 1  
**Rigettate**: 0  

### Breakdown per Priorità
- **HIGH**: 1 CR (Module System - ISO compliance critical)
- **MEDIUM**: 5 CR (DCG Support [Extension], Custom Operators, Advanced I/O, Character Types, ISO Exceptions)  
- **LOW**: 3 CR (List Operations, Debugging, Binary I/O)

### Breakdown per Complessità
- **VERY_HIGH**: 1 CR (Module System)
- **HIGH**: 4 CR (DCG, Custom Operators, Advanced I/O, Debugging)
- **MEDIUM**: 3 CR (Character Types, ISO Exceptions, Binary I/O)
- **LOW**: 1 CR (List Operations)
- **TRIVIAL**: 1 CR (Dialog Centering - COMPLETED)

**Ultimo Aggiornamento**: 2025-08-19

---

## Note

- Le Change Request vengono valutate in base a priorità e impatto
- CR con status RICHIESTA richiedono analisi tecnica prima dell'approvazione
- Tutte le CR devono avere criteri di accettazione misurabili
- Le modifiche architetturali significative richiedono discussione e approvazione