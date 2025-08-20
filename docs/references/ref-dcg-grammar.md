# DCG (Definite Clause Grammars) - Chiarimento Standard

## Importante: DCG non sono ISO Prolog Standard

### Stato Ufficiale delle DCG

**ISO/IEC 13211-1:1995 (Standard ISO Prolog)**:
- ❌ **Non include** sintassi DCG (`-->`)
- ❌ **Non definisce** predicati `phrase/2` o `phrase/3`
- ❌ **Non specifica** trasformazione difference lists
- ❌ **Non menziona** grammar rules

### Stato delle DCG nella Pratica

**De Facto Standard** (Ampiamente Adottato):
- ✅ **SWI-Prolog**: Supporto DCG completo
- ✅ **GNU Prolog**: Implementazione DCG standard  
- ✅ **YAP Prolog**: Sintassi DCG compatibile
- ✅ **Sicstus Prolog**: DCG con estensioni
- ✅ **B-Prolog**: Supporto DCG base

### Perché le DCG sono Importanti

#### Vantaggi Pratici
1. **Grammar Processing**: Ideali per parsing linguaggi formali
2. **Syntax Analysis**: Parsing espressioni, linguaggi programmazione
3. **Natural Language Processing**: Analisi linguistica
4. **Difference Lists**: Gestione efficiente liste

#### Esempi di Uso Comune
```prolog
% Parsing espressioni aritmetiche
expr --> term, [+], expr.
expr --> term.
term --> factor, [*], term.
term --> factor.
factor --> [X], { number(X) }.
factor --> ['('], expr, [')'].

% Query: phrase(expr, [2, +, 3, *, 4], [])
```

### Raccomandazioni per JProlog

#### Classificazione Corretta
- **CR-2025-0003**: Riclassificato da "ISO compliance" a "Popular Extension"
- **Priorità**: MEDIUM (non HIGH per ISO compliance)
- **Categoria**: EXTENSION (chiaramente marcato come non-ISO)

#### Valore per JProlog
1. **Completezza Funzionale**: Aumenta utilità pratica del sistema
2. **Compatibilità**: Permette esecuzione codice da altri sistemi Prolog
3. **Educational Value**: Importante per insegnamento parsing
4. **Community Expectation**: Gli utenti Prolog si aspettano DCG

#### Roadmap Aggiornato
```
Phase 1: ISO Compliance Puro (92% ISO)
- Exception terms standard
- List operations complete
- Character type system

Phase 2: Core ISO + Critical Extensions (95% ISO + DCG)
- Module system (ISO)
- Custom operators (ISO)
- DCG support (Extension)

Phase 3: Advanced Features (100% ISO + Extensions)
- Advanced I/O (ISO)
- Binary I/O (ISO)
- Debugging port model (ISO)
```

### Conclusione

Le DCG sono una **estensione estremamente utile e largamente adottata**, ma **non fanno parte dello standard ISO Prolog**. 

**Raccomandazione**: Implementare le DCG come **high-value extension** dopo aver completato la compliance ISO pura per i moduli e gli operatori personalizzati che sono effettivamente parte dello standard ISO.

**Stato Change Request**: 
- **CR-2025-0003** correttamente riclassificato come EXTENSION
- Priorità aggiornata a MEDIUM (non più HIGH per ISO compliance)
- Documentazione aggiornata per chiarire lo status non-ISO

---

*Aggiornato: 2025-08-19*
*Riferimento: ISO/IEC 13211-1:1995 - Programming languages - Prolog*