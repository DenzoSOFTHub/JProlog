# JProlog IDE

Un Integrated Development Environment completo per lo sviluppo di progetti Prolog.

## Caratteristiche Principali

### 🗂️ **Gestione Progetti**
- Creazione e apertura progetti
- Albero di navigazione delle directory
- Supporto per file `.pl` con icone specifiche
- Menu contestuali per operazioni sui file

### 📝 **Editor Avanzato**
- Editor con tab multipli
- Numerazione righe automatica
- Syntax highlighting per Prolog
- Auto-indentazione intelligente
- Indicazione file modificati
- Pulsanti di chiusura su ogni tab

### 🔧 **Funzionalità Prolog**
- **Compilazione**: File singoli o intero progetto
- **Esecuzione Query**: Dialog per query interattive
- **Debug e Trace**: Attivazione/disattivazione trace
- **Gestione Errori**: Evidenziazione errori di sintassi

### 🔍 **Sistema di Ricerca**
- Ricerca nel file corrente
- Ricerca in tutto il progetto
- Sostituzioni semplici e multiple
- Opzioni: case sensitive, parola intera, regex
- Lista risultati con navigazione

### 📊 **Console e Output**
- Console di output con syntax highlighting
- Messaggi di compilazione ed errori
- Risultati query con formattazione
- Timestamps opzionali
- Menu contestuale con funzioni utili

### 📋 **Interfaccia Utente**
- Toolbar con funzioni principali
- Barra di stato informativa
- Shortcuts da tastiera
- Menu completi
- Layout ridimensionabile

## Struttura del Progetto

```
src/main/java/it/denzosoft/jprolog/editor/
├── PrologIDE.java           # Classe principale dell'IDE
├── ProjectTree.java         # Albero di navigazione progetto
├── EditorTabbedPane.java    # Gestione tab degli editor
├── FileEditor.java          # Editor di file singolo
├── PrologToolbar.java       # Toolbar con pulsanti
├── SearchPanel.java         # Pannello di ricerca
├── OutputConsole.java       # Console di output
└── StatusBar.java           # Barra di stato
```

## Come Avviare l'IDE

### Opzione 1: Script di Avvio
```bash
./start-ide.sh
```

### Opzione 2: Maven Diretto
```bash
# Compila il progetto
mvn compile

# Avvia l'IDE
mvn exec:java -Dexec.mainClass="it.denzosoft.jprolog.editor.PrologIDE"
```

### Opzione 3: Java Diretto (dopo compilazione)
```bash
java -cp target/classes it.denzosoft.jprolog.editor.PrologIDE
```

## Utilizzo dell'IDE

### 1. **Creazione Progetto**
1. Menu **File** → **Nuovo Progetto...**
2. Seleziona directory di destinazione
3. Inserisci nome progetto
4. L'IDE creerà la struttura e un file `main.pl` di esempio

### 2. **Apertura Progetto Esistente**
1. Menu **File** → **Apri Progetto...**
2. Seleziona directory del progetto
3. L'albero mostrerà la struttura dei file

### 3. **Editing File**
- **Doppio-click** su file nell'albero per aprire
- **Tab** per navigare tra file aperti
- **Ctrl+S** per salvare
- **Ctrl+Shift+S** per salvare tutto

### 4. **Compilazione**
- **F9**: Compila file corrente
- **Ctrl+F9**: Compila intero progetto
- Errori mostrati nella console con evidenziazione

### 5. **Esecuzione Query**
- **F5**: Apre dialog per query
- Inserisci query Prolog (es: `hello_world.`)
- Risultati mostrati nella console

### 6. **Ricerca**
- **Ctrl+F**: Ricerca nel file corrente
- **Ctrl+Shift+F**: Ricerca nell'intero progetto
- **Escape**: Chiude pannello ricerca

### 7. **Debug**
- **F8**: Attiva/disattiva trace
- Indicatore nella toolbar mostra stato

## Shortcuts da Tastiera

| Shortcut | Funzione |
|----------|----------|
| **Ctrl+N** | Nuovo progetto |
| **Ctrl+O** | Apri progetto |
| **Ctrl+Shift+N** | Nuovo file |
| **Ctrl+S** | Salva file |
| **Ctrl+Shift+S** | Salva tutto |
| **F9** | Compila file |
| **Ctrl+F9** | Compila progetto |
| **F5** | Esegui query |
| **F8** | Toggle trace |
| **Ctrl+F** | Trova |
| **Ctrl+Shift+F** | Trova in progetto |
| **Escape** | Chiudi ricerca |

## Menu Contestuali

### **Albero Progetto**
- **Su file**: Apri, Compila, Elimina, Rinomina
- **Su directory**: Nuovo file, Nuova cartella, Aggiorna
- **Su radice**: Aggiorna progetto

### **Editor**
- Operazioni standard di editing
- Copia, Taglia, Incolla
- Seleziona tutto

### **Console**
- Pulisci console
- Copia output
- Salva output su file
- Toggle timestamps

## Funzionalità Avanzate

### **Auto-indentazione**
L'editor riconosce le strutture Prolog e indenta automaticamente:
```prolog
predicato(X) :-
    clausola1(X),    % Indentazione automatica
    clausola2(X).
```

### **Evidenziazione Errori**
Errori di sintassi evidenziati direttamente nell'editor con:
- Riga evidenziata in rosso
- Tooltip con messaggio d'errore
- Riferimento nella console

### **Gestione Progetti Intelligente**
- Rilevamento automatico file `.pl`
- Ordinamento: directory prima, poi file alfabeticamente
- Icone specifiche per tipo di elemento
- Aggiornamento dinamico modifiche

### **Console Avanzata**
- Colori diversi per: normali, errori, successi, warning
- Timestamps opzionali
- Limitazione automatica numero righe
- Salvataggio/caricamento output

## Estensioni Future

L'IDE è progettato per essere estendibile:

### **In Sviluppo**
- [ ] Debugger completo con breakpoint
- [ ] IntelliSense e autocompletamento
- [ ] Integrazione con Git
- [ ] Plugin system
- [ ] Themes personalizzabili

### **Possibili Aggiunte**
- [ ] Profiling delle performance
- [ ] Unit testing framework
- [ ] Refactoring tools
- [ ] Documentation generator
- [ ] Export progetti

## Configurazione

L'IDE salva la configurazione in `~/.jprolog-ide.properties`:
- Posizione e dimensioni finestra
- File recenti
- Preferenze editor
- Impostazioni UI

## Risoluzione Problemi

### **L'IDE non si avvia**
- Verifica Java versione 8+ installato
- Controlla che Maven sia configurato correttamente
- Controlla path e permessi

### **Errori di compilazione**
- Verifica sintassi Prolog nei file
- Controlla che tutti i file siano salvati
- Reset del motore: ricompila progetto

### **Ricerca non funziona**
- Verifica che ci sia un progetto aperto
- Per ricerca progetto: assicurati ci siano file `.pl`
- Controlla opzioni ricerca (case sensitive, regex, etc.)

## Contributi

Per contribuire al progetto:
1. Fai fork del repository
2. Crea branch feature (`git checkout -b feature/nuova-funzione`)
3. Commit le modifiche (`git commit -m 'Aggiunge nuova funzione'`)
4. Push al branch (`git push origin feature/nuova-funzione`)
5. Apri Pull Request

## Licenza

Progetto open source sotto licenza MIT.

---

**JProlog IDE v1.0** - Un IDE moderno per lo sviluppo Prolog efficiente e produttivo.