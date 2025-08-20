# JProlog Documentation Naming Convention

## Regole di Nomenclatura File di Documentazione

### Prefissi per Categoria

**File di Sistema (uppercase)**:
- `README.md` - Panoramica principale del progetto
- `CHANGELOG.md` - Log delle modifiche per versione
- `CLAUDE.md` - Istruzioni per l'AI assistant

**Guide Utente (guide-)**:
- `guide-quick-start.md` - Guida rapida per iniziare
- `guide-user-manual.md` - Manuale completo dell'utente  
- `guide-cli-usage.md` - Guida all'uso della CLI
- `guide-ide-usage.md` - Guida all'uso dell'IDE
- `guide-prolog-intro.md` - Introduzione al linguaggio Prolog
- `guide-java-integration.md` - Integrazione con Java
- `guide-debugging.md` - Guida al debugging
- `guide-extension.md` - Guida per estensioni

**Riferimenti Tecnici (ref-)**:
- `ref-builtins.md` - Riferimento predicati built-in
- `ref-iso-compliance.md` - Analisi compatibilità ISO
- `ref-dcg-grammar.md` - Riferimento DCG
- `ref-limitations.md` - Limitazioni note del sistema

**Report e Analisi (report-)**:
- `report-test-results.md` - Risultati test completi
- `report-package-reorg.md` - Report riorganizzazione package
- `report-resolution-summary.md` - Sommario risoluzioni

**Tracking e Gestione (track-)**:
- `track-issues.md` - Tracking delle issue
- `track-change-requests.md` - Tracking change request
- `track-release-notes.md` - Note di rilascio

**Esempi e Tutorial (example-)**:
- `example-bug-workflow.md` - Esempio workflow per bug
- `example-nqueens-compilation.md` - Esempio compilazione N-Queens

### Regole Generali

1. **Formato**: `[categoria-]nome-descrittivo.md`
2. **Caratteri**: Solo minuscole, numeri, trattini (no underscore, no spazi)
3. **Lunghezza**: Massimo 30 caratteri per il nome del file
4. **Descrizione**: Nome descrittivo del contenuto, non generico
5. **Versioning**: Non includere versioni nel nome file (gestite nel contenuto)

### Struttura Directory

```
/workspace/JProlog/
├── README.md                    # Overview principale
├── CHANGELOG.md                 # Change log
├── CLAUDE.md                    # AI instructions
├── docs/                        # Directory documentazione
│   ├── guides/
│   │   ├── guide-quick-start.md
│   │   ├── guide-user-manual.md
│   │   ├── guide-cli-usage.md
│   │   └── guide-ide-usage.md
│   ├── references/
│   │   ├── ref-builtins.md
│   │   ├── ref-iso-compliance.md
│   │   └── ref-limitations.md
│   ├── reports/
│   │   └── report-test-results.md
│   └── tracking/
│       ├── track-issues.md
│       ├── track-change-requests.md
│       └── track-release-notes.md
└── examples/
    └── demo-interactive.md
```

### Migrazione Esistente

I file esistenti verranno rinominati secondo questa convenzione mantenendo l'integrità dei riferimenti interni e la cronologia git.