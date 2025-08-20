# Esempio di Workflow Bug: DCG Arithmetic Parsing

Questo documento mostra come il nuovo workflow di gestione bug sarebbe stato applicato alla issue ISS-2025-0005.

## 📝 **Fase 1: Segnalazione Bug (Status: TO_ANALYZE)**

**User Report**: "parse_expr("1 + 2*3 - 4", AST). restituisce false"

### Registrazione Iniziale in issues.md:

```markdown
### ISS-2025-0005: DCG Arithmetic Expression Parsing Failure

**Titolo**: Il parsing delle espressioni aritmetiche DCG fallisce  
**Data Rilevamento**: 2025-08-19  
**Status**: TO_ANALYZE  
**Data Apertura**: 2025-08-19  

#### Descrizione Iniziale
L'utente riporta che il parsing dell'espressione "1 + 2*3 - 4" usando DCG fallisce.

Codice fornito dall'utente:
- Programma DCG complesso per parsing espressioni aritmetiche
- Query: `parse_expr("1 + 2*3 - 4", AST)`
- Risultato atteso: AST con struttura parsed
- Risultato attuale: `false`

#### Causa Root
[DA DETERMINARE durante l'analisi]

#### Casi di Test
[DA DEFINIRE durante l'analisi]
```

## 🔍 **Fase 2: Analisi Tecnica (Status: IN_ANALYSIS)**

### Update in issues.md:

```markdown
**Status**: IN_ANALYSIS  
**Data Inizio Analisi**: 2025-08-19  

#### Causa Root
🔄 **IN ANALISI**: Creazione test specifici per identificare il problema

#### Analisi Tecnica
**Test Creati**:
1. `TestDCGArithmetic.java` - Test caricamento programma DCG
2. `TestNumberCodes.java` - Test predicato number_codes/2
3. `TestDCGDirect.java` - Test componenti individuali

**Primi Risultati**:
- DCG Transformer funziona (regole `-->` trasformate correttamente)
- Il problema non è nella sintassi DCG
```

## ✅ **Fase 3: Identificazione Causa (Status: IN_ANALYSIS continuato)**

### Update dopo debugging sistematico:

```markdown
#### Causa Root
✅ **IDENTIFICATA**: Mancano 3 predicati built-in ISO Prolog standard:
1. `number_codes/2` - Conversione numero ↔ codici ASCII
2. `phrase/2` / `phrase/3` - Esecuzione regole DCG  
3. `atom_number/2` - Conversione atom ↔ numero

**Test di Conferma**:
- ✅ `number_codes(123, [49,50,51])` → `false` (dovrebbe essere `true`)
- ✅ `phrase(simple_rule, [a])` → `false` (anche per regole semplici)
- ✅ `atom_number('123', N)` → `false`

#### Scoperte Aggiuntive
Durante l'analisi sono emersi questi problemi correlati:
- Built-in `phrase/2` completamente non funzionale
- Sistema DCG genera regole corrette ma non può eseguirle
- Parser numeri dipende da `number_codes/2` assente
```

## 🛠️ **Fase 4: Pianificazione Fix (Status: IN_PROGRESS)**

```markdown
**Status**: IN_PROGRESS  
**Data Inizio Implementazione**: 2025-08-19  

#### Soluzione Pianificata
Implementare 3 predicati built-in mancanti:

1. **`number_codes/2`** in `src/main/java/it/denzosoft/jprolog/builtin/conversion/`
2. **`phrase/2` e `phrase/3`** in `src/main/java/it/denzosoft/jprolog/builtin/dcg/`
3. **`atom_number/2`** in `src/main/java/it/denzosoft/jprolog/builtin/conversion/`

#### Casi di Test per Validazione
- [ ] `number_codes(123, [49,50,51])` deve essere `true`
- [ ] `number_codes(N, [49,50,51])` deve unificare `N=123`
- [ ] `phrase/2` deve funzionare per regole DCG semplici
- [ ] `parse_expr("1", AST)` deve funzionare per numeri singoli
- [ ] `parse_expr("1 + 2*3 - 4", AST)` deve produrre AST corretto
```

## ✅ **Fase 5: Risoluzione (Status: RESOLVED)**

```markdown
**Status**: RESOLVED  
**Data Risoluzione**: 2025-08-19  

#### Soluzione Implementata
Implementati tutti e 3 i predicati built-in mancanti:

1. **NumberCodes.java**: Implementazione completa `number_codes/2`
   - Supporta conversione bidirezionale numero ↔ lista codici
   - Gestisce casi unify in entrambe le direzioni
   
2. **PhraseBuiltin.java**: Implementazione `phrase/2` e `phrase/3`
   - `phrase(Goal, List)` → `phrase(Goal, List, [])`
   - `phrase(Goal, List, Rest)` → `call(Goal, List, Rest)`
   
3. **AtomNumber.java**: Implementazione `atom_number/2`
   - Conversione bidirezionale atom ↔ numero

**File modificati**:
- `src/main/java/it/denzosoft/jprolog/builtin/conversion/NumberCodes.java` (nuovo)
- `src/main/java/it/denzosoft/jprolog/builtin/dcg/PhraseBuiltin.java` (nuovo)  
- `src/main/java/it/denzosoft/jprolog/builtin/conversion/AtomNumber.java` (nuovo)
- `src/main/java/it/denzosoft/jprolog/core/engine/BuiltInFactory.java` (aggiornato)

**Test di Validazione**:
- ✅ `number_codes(123, [49,50,51])` → `true`
- ✅ `number_codes(N, [49,50,51])` → `N=123`
- ✅ `phrase(simple_rule, [a])` → `true`  
- ✅ `parse_expr("1", AST)` → `AST=1`
- ✅ `parse_expr("1 + 2*3 - 4", AST)` → `AST=minus(plus(1, times(2, 3)), 4)`
```

## 🔒 **Fase 6: Chiusura (Status: CLOSED)**

```markdown
**Status**: CLOSED  
**Data Chiusura**: 2025-08-19  

#### Validazione Finale
- ✅ Tutti i casi di test passano
- ✅ Nessuna regressione rilevata su funzionalità esistenti
- ✅ Parsing DCG aritmetico completamente funzionale
- ✅ Conformità ISO Prolog per built-in implementati

#### Impatto
La risoluzione di questa issue ha sbloccato:
- Parsing completo espressioni aritmetiche con DCG
- Funzionalità `phrase/2` per tutti i casi d'uso DCG
- Conversioni numero-stringa standard ISO Prolog
- Base per sviluppo parser più complessi
```

---

## 📚 **Lezioni Apprese**

### ✅ **Workflow Corretto Applicato**
1. **Registrazione immediata** del bug con status `TO_ANALYZE`
2. **Analisi sistematica** con test specifici prima di implementare
3. **Identificazione precisa** della causa root
4. **Documentazione completa** di ogni fase
5. **Validazione rigorosa** prima della chiusura

### ⚠️ **Errori Evitati**
- **NON** tentare fix immediati senza capire la causa
- **NON** assumere che il problema sia dove sembra  
- **NON** risolvere solo il sintomo senza indagare a fondo
- **NON** dimenticare di testare regressioni

### 🎯 **Benefici del Processo**
- **Comprensione completa** del problema
- **Soluzione robusta** che affronta la causa root
- **Documentazione utile** per problemi futuri simili
- **Tracciabilità completa** delle decisioni tecniche