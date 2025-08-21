# Capitolo 7: Modello di Esecuzione - Sistema Runtime JProlog

## Panoramica

Questo capitolo fornisce un'analisi completa del modello di esecuzione di JProlog, coprendo l'intero ciclo di vita dal caricamento del programma all'esecuzione degli obiettivi, incluse operazioni database, costrutti di controllo, gestione I/O e gestione errori. Dimostra come JProlog implementa la semantica di esecuzione ISO Prolog con esempi pratici e approfondimenti dettagliati sull'implementazione.

---

## 7.4.2 Direttive

### Definizione e Scopo

Le direttive sono istruzioni elaborate al momento del caricamento che configurano il sistema Prolog, dichiarano proprietà dei predicati o eseguono task di inizializzazione. JProlog implementa un sistema di direttive completo che supporta sia le direttive standard ISO che le estensioni.

### Architettura di Implementazione

```java
public class ProcessoreDirettive {
    private final Map<String, GestoreDirettiva> gestori;
    private final ContestoDirettiva contesto;
    private final List<DirettivaInizializzazione> inizializzazioni;
    
    public ProcessoreDirettive(MotoreProlog motore) {
        this.gestori = new HashMap<>();
        this.contesto = new ContestoDirettiva(motore);
        this.inizializzazioni = new ArrayList<>();
        registraDirettiveStandard();
    }
    
    private void registraDirettiveStandard() {
        // Direttive principali ISO
        registraGestore("dynamic", new GestoreDirettivaDynamic());
        registraGestore("multifile", new GestoreDirettivaMultifile());
        registraGestore("discontiguous", new GestoreDirettivaDiscontiguous());
        registraGestore("op", new GestoreDirettivaOperatore());
        registraGestore("char_conversion", new GestoreConversioneCaratteri());
        registraGestore("set_prolog_flag", new GestoreDirettivaFlag());
        registraGestore("initialization", new GestoreInizializzazione());
        
        // Estensioni JProlog
        registraGestore("module", new GestoreDirettivaModulo());
        registraGestore("use_module", new GestoreUseModule());
        registraGestore("encoding", new GestoreDirettivaEncoding());
        registraGestore("meta_predicate", new GestoreMetaPredicato());
    }
    
    public void processaDirettiva(Direttiva direttiva) {
        String nome = direttiva.getNome();
        GestoreDirettiva gestore = gestori.get(nome);
        
        if (gestore == null) {
            // Controlla se è una direttiva obiettivo (:- Goal.)
            if (direttiva.isDirettivaObiettivo()) {
                processaDirettivaObiettivo(direttiva);
                return;
            }
            throw new EccezioneDirettivaSconosciuta(
                "Direttiva sconosciuta: " + nome);
        }
        
        // Valida contesto direttiva
        validaContestoDirettiva(direttiva, gestore);
        
        // Processa la direttiva
        try {
            gestore.processa(direttiva, contesto);
            loggaElaborazioneDirettiva(direttiva, true);
        } catch (EccezioneDirettiva e) {
            loggaElaborazioneDirettiva(direttiva, false);
            throw e;
        }
    }
}
```

### Gestore Direttiva Dynamic

```java
public class GestoreDirettivaDynamic implements GestoreDirettiva {
    @Override
    public void processa(Direttiva direttiva, ContestoDirettiva contesto) {
        for (Term indicatore : direttiva.getArgomenti()) {
            IndicatorePredicato ip = parseIndicatorePredicato(indicatore);
            
            // Marca predicato come dinamico
            contesto.getBaseConoscenza().marcaDinamico(ip);
            
            // Crea infrastruttura predicato dinamico
            impostaPredicatoDinamico(ip, contesto);
            
            // Registra con gestore database
            contesto.getGestoreDatabase().registraDinamico(ip);
        }
    }
    
    private void impostaPredicatoDinamico(IndicatorePredicato ip, 
            ContestoDirettiva contesto) {
        // Crea strutture indicizzazione
        GestoreIndici gestoreIndici = contesto.getGestoreIndici();
        gestoreIndici.creaIndice(ip, TipoIndice.PRIMO_ARGOMENTO);
        gestoreIndici.creaIndice(ip, TipoIndice.HASH_TABLE);
        
        // Imposta tracciamento asserzioni
        TracciatorAsserzioni tracciatore = new TracciatorAsserzioni(ip);
        contesto.getGestoreAsserzioni().registraTracciatore(tracciatore);
        
        // Crea metodi assert/retract specializzati
        generaMetodiDinamici(ip, contesto);
    }
    
    private void generaMetodiDinamici(IndicatorePredicato ip,
            ContestoDirettiva contesto) {
        // Genera metodo assert ottimizzato
        String metodoAssert = String.format(
            "public void assert_%s_%d(Term[] args) {\n" +
            "    Clausola clausola = new Clausola(new TermineComposto(\"%s\", args));\n" +
            "    inserimentoIndicizzato(clausola);\n" +
            "    notificaAsserzione(clausola);\n" +
            "}",
            ip.getNome(), ip.getArita(), ip.getNome()
        );
        
        // Compila e registra metodo
        CompilatoreMedotodiDinamici compilatore = contesto.getCompilatoreMetodi();
        compilatore.compilaERegistra(ip, metodoAssert);
    }
}
```

### Gestore Direttiva Operatore

```java
public class GestoreDirettivaOperatore implements GestoreDirettiva {
    @Override
    public void processa(Direttiva direttiva, ContestoDirettiva contesto) {
        List<Term> args = direttiva.getArgomenti();
        if (args.size() != 3) {
            throw new EccezioneArgomentiDirettiva(
                "La direttiva op/3 richiede esattamente 3 argomenti");
        }
        
        // Analizza definizione operatore
        int precedenza = parseIntero(args.get(0));
        String associativita = parseAtomo(args.get(1));
        String simbolo = parseAtomo(args.get(2));
        
        // Valida definizione operatore
        validaOperatore(precedenza, associativita, simbolo);
        
        // Registra operatore
        RegistroOperatori registro = contesto.getRegistroOperatori();
        registro.definiOperatore(precedenza, associativita, simbolo);
        
        // Aggiorna tabelle parser
        contesto.getParser().aggiornaTabelleOperatori();
        
        // Logga definizione operatore
        contesto.log(String.format("Operatore definito %s(%d, %s, %s)",
            simbolo, precedenza, associativita, simbolo));
    }
    
    private void validaOperatore(int precedenza, String associativita,
            String simbolo) {
        // Controlla range precedenza
        if (precedenza < 1 || precedenza > 1200) {
            throw new EccezioneDefinizioneOperatore(
                "La precedenza operatore deve essere tra 1 e 1200");
        }
        
        // Valida associatività
        Set<String> assocValida = Set.of(
            "fx", "fy", "xf", "yf", "xfx", "xfy", "yfx", "yfy"
        );
        if (!assocValida.contains(associativita)) {
            throw new EccezioneDefinizioneOperatore(
                "Associatività non valida: " + associativita);
        }
        
        // Controlla validità simbolo
        if (simbolo.isEmpty() || simbolo.contains(" ")) {
            throw new EccezioneDefinizioneOperatore(
                "Simbolo operatore non valido: " + simbolo);
        }
    }
}
```

### Gestore Inizializzazione

```java
public class GestoreInizializzazione implements GestoreDirettiva {
    @Override
    public void processa(Direttiva direttiva, ContestoDirettiva contesto) {
        List<Term> args = direttiva.getArgomenti();
        if (args.size() != 1) {
            throw new EccezioneArgomentiDirettiva(
                "La direttiva initialization/1 richiede esattamente 1 argomento");
        }
        
        Term obiettivo = args.get(0);
        
        // Crea task di inizializzazione
        TaskInizializzazione task = new TaskInizializzazione(obiettivo, contesto);
        
        // Aggiungi alla coda inizializzazione
        contesto.getCodaInizializzazione().aggiungi(task);
        
        // Marca per esecuzione dopo caricamento
        contesto.marcaPerInizializzazione(task);
    }
}

public class TaskInizializzazione {
    private final Term obiettivo;
    private final ContestoDirettiva contesto;
    private final long priorita;
    
    public TaskInizializzazione(Term obiettivo, ContestoDirettiva contesto) {
        this.obiettivo = obiettivo;
        this.contesto = contesto;
        this.priorita = System.currentTimeMillis();
    }
    
    public void esegui() {
        try {
            RisolutoreQuery solver = contesto.getRisolutoreQuery();
            List<Substitution> soluzioni = solver.risolvi(obiettivo);
            
            if (soluzioni.isEmpty()) {
                contesto.log("AVVISO: Obiettivo inizializzazione fallito: " + obiettivo);
            } else {
                contesto.log("Obiettivo inizializzazione riuscito: " + obiettivo);
            }
        } catch (Exception e) {
            contesto.log("ERRORE: Obiettivo inizializzazione ha lanciato eccezione: " + 
                e.getMessage());
            throw new EccezioneInizializzazione(
                "Inizializzazione fallita per obiettivo: " + obiettivo, e);
        }
    }
}
```

---

## 7.4.3 Clausole

### Struttura e Tipi di Clausola

JProlog implementa un sofisticato sistema di gestione clausole che gestisce fatti, regole e le loro rappresentazioni interne in modo efficiente.

### Implementazione Clausole

```java
public abstract class Clausola {
    protected final Term testa;
    protected final UUID id;
    protected final long timestamp;
    protected volatile boolean attiva;
    
    public Clausola(Term testa) {
        this.testa = testa;
        this.id = UUID.randomUUID();
        this.timestamp = System.currentTimeMillis();
        this.attiva = true;
    }
    
    public abstract boolean isFatto();
    public abstract boolean isRegola();
    public abstract Term getCorpo();
    public abstract List<Variable> getVariabili();
    public abstract Clausola rinomina(RinominatoreVariabili rinominatore);
    
    // Supporto indicizzazione clausole
    public abstract ChiaveIndice getChiaveIndice();
    public abstract Set<ChiaveIndice> getTutteChiaviIndice();
}

public class Fatto extends Clausola {
    public Fatto(Term testa) {
        super(testa);
        if (testa.contieneVariabili()) {
            throw new IllegalArgumentException(
                "I fatti non possono contenere variabili: " + testa);
        }
    }
    
    @Override
    public boolean isFatto() { return true; }
    
    @Override
    public boolean isRegola() { return false; }
    
    @Override
    public Term getCorpo() { return Atomo.TRUE; }
    
    @Override
    public List<Variable> getVariabili() { 
        return Collections.emptyList(); 
    }
    
    @Override
    public Clausola rinomina(RinominatoreVariabili rinominatore) {
        // I fatti non necessitano rinominazione
        return this;
    }
    
    @Override
    public ChiaveIndice getChiaveIndice() {
        if (testa instanceof TermineComposto) {
            TermineComposto composto = (TermineComposto) testa;
            return new ChiaveIndice(composto.getNome(), 
                composto.getPrimoArgomento());
        }
        return new ChiaveIndice(testa.toString(), null);
    }
}

public class Regola extends Clausola {
    private final Term corpo;
    private final List<Variable> variabili;
    
    public Regola(Term testa, Term corpo) {
        super(testa);
        this.corpo = corpo;
        this.variabili = estraiVariabili(testa, corpo);
    }
    
    @Override
    public boolean isFatto() { return false; }
    
    @Override
    public boolean isRegola() { return true; }
    
    @Override
    public Term getCorpo() { return corpo; }
    
    @Override
    public List<Variable> getVariabili() { 
        return Collections.unmodifiableList(variabili); 
    }
    
    @Override
    public Clausola rinomina(RinominatoreVariabili rinominatore) {
        Term testaRinominata = rinominatore.rinomina(testa);
        Term corpoRinominato = rinominatore.rinomina(corpo);
        return new Regola(testaRinominata, corpoRinominato);
    }
    
    private List<Variable> estraiVariabili(Term testa, Term corpo) {
        Set<Variable> setVar = new LinkedHashSet<>();
        raccogli'Variabili(testa, setVar);
        raccogli'Variabili(corpo, setVar);
        return new ArrayList<>(setVar);
    }
}
```

### Archiviazione e Recupero Clausole

```java
public class DatabaseClausole {
    private final Map<IndicatorePredicato, CollezioneClausole> clausole;
    private final GestoreIndici gestoreIndici;
    private final ValidatoreClausole validatore;
    
    public DatabaseClausole() {
        this.clausole = new ConcurrentHashMap<>();
        this.gestoreIndici = new GestoreIndici();
        this.validatore = new ValidatoreClausole();
    }
    
    public void aggiungiClausola(Clausola clausola) {
        // Valida clausola
        validatore.valida(clausola);
        
        // Ottieni o crea collezione predicato
        IndicatorePredicato ip = getIndicatorePredicato(clausola.getTesta());
        CollezioneClausole collezione = clausole.computeIfAbsent(ip,
            k -> new CollezioneClausole(k, gestoreIndici));
        
        // Aggiungi clausola alla collezione
        collezione.aggiungiClausola(clausola);
        
        // Aggiorna indici
        gestoreIndici.indicizzaClausola(clausola, ip);
        
        // Notifica ascoltatori
        notificaClausolaAggiunta(clausola, ip);
    }
    
    public List<Clausola> getClausole(Term obiettivo) {
        IndicatorePredicato ip = getIndicatorePredicato(obiettivo);
        CollezioneClausole collezione = clausole.get(ip);
        
        if (collezione == null) {
            return Collections.emptyList();
        }
        
        // Usa indicizzazione per recupero efficiente
        if (gestoreIndici.haIndice(ip, TipoIndice.PRIMO_ARGOMENTO)) {
            return collezione.getClausoleIndicizzate(obiettivo);
        } else {
            return collezione.getTutteClausole();
        }
    }
    
    public boolean rimuoviClausola(Clausola clausola) {
        IndicatorePredicato ip = getIndicatorePredicato(clausola.getTesta());
        CollezioneClausole collezione = clausole.get(ip);
        
        if (collezione != null) {
            boolean rimossa = collezione.rimuoviClausola(clausola);
            if (rimossa) {
                gestoreIndici.rimuoviDaIndice(clausola, ip);
                notificaClausolaRimossa(clausola, ip);
                return true;
            }
        }
        return false;
    }
}

public class CollezioneClausole {
    private final IndicatorePredicato predicato;
    private final List<Clausola> tutteClausole;
    private final Map<ChiaveIndice, List<Clausola>> clausoleIndicizzate;
    private final GestoreIndici gestoreIndici;
    private final ReadWriteLock lock;
    
    public CollezioneClausole(IndicatorePredicato predicato, 
            GestoreIndici gestoreIndici) {
        this.predicato = predicato;
        this.tutteClausole = new ArrayList<>();
        this.clausoleIndicizzate = new HashMap<>();
        this.gestoreIndici = gestoreIndici;
        this.lock = new ReentrantReadWriteLock();
    }
    
    public void aggiungiClausola(Clausola clausola) {
        lock.writeLock().lock();
        try {
            tutteClausole.add(clausola);
            
            // Aggiungi agli indici
            for (ChiaveIndice chiave : clausola.getTutteChiaviIndice()) {
                clausoleIndicizzate.computeIfAbsent(chiave, 
                    k -> new ArrayList<>()).add(clausola);
            }
        } finally {
            lock.writeLock().unlock();
        }
    }
    
    public List<Clausola> getClausoleIndicizzate(Term obiettivo) {
        lock.readLock().lock();
        try {
            ChiaveIndice chiave = creaChiaveIndice(obiettivo);
            List<Clausola> risultato = clausoleIndicizzate.get(chiave);
            return risultato != null ? new ArrayList<>(risultato) : 
                Collections.emptyList();
        } finally {
            lock.readLock().unlock();
        }
    }
}
```

---

## 7.5 Database

### 7.5.1 Preparazione di un Testo Prolog per l'Esecuzione

JProlog implementa una pipeline di compilazione completa che trasforma il testo Prolog in forma eseguibile con ottimizzazioni e validazione.

### Pipeline di Compilazione

```java
public class CompilatoreProlog {
    private final AnalizzatoreLessicale lexer;
    private final AnalizzatoreSintattico parser;
    private final AnalizzatoreSemantico analizzatoreSemantico;
    private final Ottimizzatore ottimizzatore;
    private final GeneratoreCodice generatoreCodice;
    
    public RisultatoCompilazione compila(String testoProlog, 
            OpzioniCompilazione opzioni) {
        ContestoCompilazione contesto = new ContestoCompilazione(opzioni);
        
        try {
            // Fase 1: Analisi Lessicale
            List<Token> tokens = lexer.tokenizza(testoProlog);
            contesto.setTokens(tokens);
            
            // Fase 2: Analisi Sintattica
            AST ast = parser.analizza(tokens);
            contesto.setAST(ast);
            
            // Fase 3: Analisi Semantica
            InfoSemantica infoSemantica = analizzatoreSemantico.analizza(ast);
            contesto.setInfoSemantica(infoSemantica);
            
            // Fase 4: Ottimizzazione
            ASTOttimizzato astOttimizzato = ottimizzatore.ottimizza(ast, infoSemantica);
            contesto.setASTOttimizzato(astOttimizzato);
            
            // Fase 5: Generazione Codice
            CodiceEseguibile codice = generatoreCodice.genera(astOttimizzato);
            contesto.setCodiceEseguibile(codice);
            
            // Fase 6: Caricamento Database
            caricaInDatabase(codice, contesto);
            
            return new RisultatoCompilazione(true, contesto);
            
        } catch (EccezioneCompilazione e) {
            return new RisultatoCompilazione(false, e, contesto);
        }
    }
    
    private void caricaInDatabase(CodiceEseguibile codice, 
            ContestoCompilazione contesto) {
        CaricatoreDatabase caricatore = new CaricatoreDatabase(contesto.getDatabase());
        
        // Carica clausole
        for (ClausolaCompilata clausola : codice.getClausole()) {
            caricatore.caricaClausola(clausola);
        }
        
        // Processa direttive
        for (DirettivaCompilata direttiva : codice.getDirettive()) {
            caricatore.processaDirettiva(direttiva);
        }
        
        // Esegui obiettivi di inizializzazione
        for (ObiettivoInizializzazione obiettivo : codice.getInizializzazioni()) {
            caricatore.eseguiInizializzazione(obiettivo);
        }
    }
}
```

### Analisi Pre-compilazione

```java
public class AnalizzatorePrecompilazione {
    public InfoPrecompilazione analizza(AST ast) {
        InfoPrecompilazione info = new InfoPrecompilazione();
        
        // Analizza dipendenze predicati
        analizzaDipendenze(ast, info);
        
        // Rileva predicati ricorsivi
        analizzaRicorsione(ast, info);
        
        // Analizza pattern clausole per ottimizzazione
        analizzaPatternClausole(ast, info);
        
        // Controlla predicati dinamici
        analizzaPredicatiDinamici(ast, info);
        
        // Analizza uso cut
        analizzaUsoCut(ast, info);
        
        return info;
    }
    
    private void analizzaDipendenze(AST ast, InfoPrecompilazione info) {
        GrafoDipendenze grafo = new GrafoDipendenze();
        
        for (Clausola clausola : ast.getClausole()) {
            IndicatorePredicato testa = getIndicatorePredicato(clausola.getTesta());
            
            if (clausola.isRegola()) {
                Set<IndicatorePredicato> predicatiCorpo = 
                    estraiPredicati(clausola.getCorpo());
                
                for (IndicatorePredicato predCorpo : predicatiCorpo) {
                    grafo.aggiungiDipendenza(testa, predCorpo);
                }
            }
        }
        
        info.setGrafoDipendenze(grafo);
        
        // Rileva componenti fortemente connessi (ricorsione mutua)
        List<Set<IndicatorePredicato>> sccs = grafo.getComponentiFortementeConnessi();
        info.setGruppiMutuamenteRicorsivi(sccs);
    }
    
    private void analizzaPatternClausole(AST ast, InfoPrecompilazione info) {
        Map<IndicatorePredicato, PatternClausola> pattern = new HashMap<>();
        
        for (Clausola clausola : ast.getClausole()) {
            IndicatorePredicato ip = getIndicatorePredicato(clausola.getTesta());
            PatternClausola pattern = patterns.computeIfAbsent(ip, 
                k -> new PatternClausola(k));
            
            pattern.aggiungiClausola(clausola);
        }
        
        // Analizza pattern per opportunità di ottimizzazione
        for (PatternClausola pattern : patterns.values()) {
            analizzaPattern(pattern, info);
        }
        
        info.setPatternClausole(patterns);
    }
    
    private void analizzaPattern(PatternClausola pattern, InfoPrecompilazione info) {
        // Controlla predicati deterministici
        if (pattern.isDeterministico()) {
            info.aggiungiPredicatoDeterministico(pattern.getPredicato());
        }
        
        // Controlla predicati indicizzabili
        if (pattern.isIndicizzabile()) {
            info.aggiungiPredicatoIndicizzabile(pattern.getPredicato(), 
                pattern.getArgomentiIndicizzabili());
        }
        
        // Controlla predicati tail-recursivi
        if (pattern.isTailRicorsivo()) {
            info.aggiungiPredicatoTailRicorsivo(pattern.getPredicato());
        }
    }
}
```

### 7.5.2 Procedure Statiche e Dinamiche

JProlog distingue tra procedure statiche e dinamiche con meccanismi diversi di archiviazione e accesso.

### Gestione Procedure Dinamiche

```java
public class GestoreProcedureDinamiche {
    private final Map<IndicatorePredicato, PredicatoDinamico> predicatiDinamici;
    private final Map<IndicatorePredicato, PredicatoStatico> predicatiStatici;
    private final TracciatoreModifiche tracciatore;
    
    public GestoreProcedureDinamiche() {
        this.predicatiDinamici = new ConcurrentHashMap<>();
        this.predicatiStatici = new HashMap<>();
        this.tracciatore = new TracciatoreModifiche();
    }
    
    public void dichiaraDinamico(IndicatorePredicato ip) {
        if (predicatiStatici.containsKey(ip)) {
            throw new EccezioneProcedura(
                "Non posso ridichiarare predicato statico come dinamico: " + ip);
        }
        
        PredicatoDinamico predDin = new PredicatoDinamico(ip);
        predicatiDinamici.put(ip, predDin);
        
        // Imposta indicizzazione per predicato dinamico
        impostaIndicizzazioneDinamica(predDin);
        
        // Abilita tracciamento modifiche
        tracciatore.tracciaPredicato(ip);
    }
    
    public void asserisciClausola(Clausola clausola, PosizioneAsserzione posizione) {
        IndicatorePredicato ip = getIndicatorePredicato(clausola.getTesta());
        PredicatoDinamico predDin = predicatiDinamici.get(ip);
        
        if (predDin == null) {
            throw new EccezioneProcedura(
                "Non posso asserire su predicato non-dinamico: " + ip);
        }
        
        // Registra modifica per backtracking
        Modifica modifica = new ModificaAsserzione(clausola, posizione);
        tracciatore.registraModifica(ip, modifica);
        
        // Aggiungi clausola al predicato
        predDin.aggiungiClausola(clausola, posizione);
        
        // Aggiorna indici
        predDin.aggiornaIndici(clausola, OperazioneIndice.AGGIUNGI);
        
        // Notifica ascoltatori
        notificaAsserzione(ip, clausola);
    }
    
    public List<Clausola> ritraiClausole(Term pattern) {
        IndicatorePredicato ip = getIndicatorePredicato(pattern);
        PredicatoDinamico predDin = predicatiDinamici.get(ip);
        
        if (predDin == null) {
            throw new EccezioneProcedura(
                "Non posso ritrarre da predicato non-dinamico: " + ip);
        }
        
        List<Clausola> ritratte = predDin.ritraiClausole(pattern);
        
        // Registra modifiche
        for (Clausola clausola : ritratte) {
            Modifica modifica = new ModificaRitrazione(clausola);
            tracciatore.registraModifica(ip, modifica);
        }
        
        // Aggiorna indici
        for (Clausola clausola : ritratte) {
            predDin.aggiornaIndici(clausola, OperazioneIndice.RIMUOVI);
        }
        
        // Notifica ascoltatori
        for (Clausola clausola : ritratte) {
            notificaRitrazione(ip, clausola);
        }
        
        return ritratte;
    }
}

public class PredicatoDinamico {
    private final IndicatorePredicato indicatore;
    private final List<Clausola> clausole;
    private final Map<TipoIndice, IndicePredicato> indici;
    private final ReadWriteLock lock;
    
    public PredicatoDinamico(IndicatorePredicato indicatore) {
        this.indicatore = indicatore;
        this.clausole = new ArrayList<>();
        this.indici = new HashMap<>();
        this.lock = new ReentrantReadWriteLock();
    }
    
    public void aggiungiClausola(Clausola clausola, PosizioneAsserzione posizione) {
        lock.writeLock().lock();
        try {
            switch (posizione) {
                case PRIMA:
                    clausole.add(0, clausola);
                    break;
                case ULTIMA:
                    clausole.add(clausola);
                    break;
                default:
                    throw new IllegalArgumentException("Posizione non valida: " + posizione);
            }
        } finally {
            lock.writeLock().unlock();
        }
    }
    
    public List<Clausola> ritraiClausole(Term pattern) {
        lock.writeLock().lock();
        try {
            List<Clausola> ritratte = new ArrayList<>();
            Iterator<Clausola> iteratore = clausole.iterator();
            
            while (iteratore.hasNext()) {
                Clausola clausola = iteratore.next();
                if (unifica(clausola.getTesta(), pattern)) {
                    iteratore.remove();
                    ritratte.add(clausola);
                }
            }
            
            return ritratte;
        } finally {
            lock.writeLock().unlock();
        }
    }
    
    public List<Clausola> getClausole(Term obiettivo) {
        lock.readLock().lock();
        try {
            // Usa indicizzazione se disponibile
            if (haIndice(TipoIndice.PRIMO_ARGOMENTO)) {
                return getClausoleIndicizzate(obiettivo);
            } else {
                return new ArrayList<>(clausole);
            }
        } finally {
            lock.readLock().unlock();
        }
    }
}
```

### 7.5.3 Procedure Private e Pubbliche

JProlog implementa controllo di visibilità basato sui moduli per le procedure.

### Sistema Moduli

```java
public class GestoreModuli {
    private final Map<String, Modulo> moduli;
    private final Modulo moduloUtente;
    private String moduloCorrente;
    
    public GestoreModuli() {
        this.moduli = new HashMap<>();
        this.moduloUtente = new Modulo("user");
        this.moduloCorrente = "user";
        
        moduli.put("user", moduloUtente);
        moduli.put("system", creaModuloSistema());
    }
    
    public void dichiaraModulo(String nome, List<IndicatorePredicato> esportazioni) {
        if (moduli.containsKey(nome)) {
            throw new EccezioneModulo("Il modulo esiste già: " + nome);
        }
        
        Modulo modulo = new Modulo(nome);
        modulo.setEsportazioni(esportazioni);
        moduli.put(nome, modulo);
    }
    
    public void setModuloCorrente(String nome) {
        if (!moduli.containsKey(nome)) {
            throw new EccezioneModulo("Modulo sconosciuto: " + nome);
        }
        this.moduloCorrente = nome;
    }
    
    public boolean isVisibile(IndicatorePredicato ip, String daModulo) {
        Modulo moduloTarget = trovaModuloPerPredicato(ip);
        if (moduloTarget == null) return false;
        
        // Sempre visibile nello stesso modulo
        if (moduloTarget.getNome().equals(daModulo)) {
            return true;
        }
        
        // Controlla se predicato è esportato
        return moduloTarget.isEsportato(ip);
    }
    
    public void esportaPredicato(IndicatorePredicato ip, String modulo) {
        Modulo mod = moduli.get(modulo);
        if (mod == null) {
            throw new EccezioneModulo("Modulo sconosciuto: " + modulo);
        }
        
        mod.aggiungiEsportazione(ip);
    }
    
    public void importaPredicato(IndicatorePredicato ip, 
            String daModulo, String aModulo) {
        Modulo sorgente = moduli.get(daModulo);
        Modulo destinazione = moduli.get(aModulo);
        
        if (sorgente == null || destinazione == null) {
            throw new EccezioneModulo("Modulo sconosciuto nell'importazione");
        }
        
        if (!sorgente.isEsportato(ip)) {
            throw new EccezioneModulo(
                "Predicato non esportato: " + ip + " da " + daModulo);
        }
        
        destinazione.aggiungiImportazione(ip, daModulo);
    }
}

public class Modulo {
    private final String nome;
    private final Set<IndicatorePredicato> esportazioni;
    private final Map<IndicatorePredicato, String> importazioni;
    private final Set<IndicatorePredicato> predicatiLocali;
    
    public Modulo(String nome) {
        this.nome = nome;
        this.esportazioni = new HashSet<>();
        this.importazioni = new HashMap<>();
        this.predicatiLocali = new HashSet<>();
    }
    
    public boolean isEsportato(IndicatorePredicato ip) {
        return esportazioni.contains(ip);
    }
    
    public boolean isLocale(IndicatorePredicato ip) {
        return predicatiLocali.contains(ip);
    }
    
    public String getModuloImportazione(IndicatorePredicato ip) {
        return importazioni.get(ip);
    }
    
    public void aggiungiPredicatoLocale(IndicatorePredicato ip) {
        predicatiLocali.add(ip);
    }
    
    public void aggiungiEsportazione(IndicatorePredicato ip) {
        esportazioni.add(ip);
        predicatiLocali.add(ip); // Predicati esportati sono anche locali
    }
    
    public void aggiungiImportazione(IndicatorePredicato ip, String daModulo) {
        importazioni.put(ip, daModulo);
    }
}
```

### 7.5.4 Un Aggiornamento Logico del Database

JProlog implementa aggiornamenti logici del database con semantica transazionale e garanzie di consistenza.

### Gestione Transazioni

```java
public class TransazioneDatabase {
    private final UUID idTransazione;
    private final long tempoInizio;
    private final List<ModificaDatabase> modifiche;
    private final LivelloIsolamento livelloIsolamento;
    private volatile StatoTransazione stato;
    
    public TransazioneDatabase(LivelloIsolamento livelloIsolamento) {
        this.idTransazione = UUID.randomUUID();
        this.tempoInizio = System.currentTimeMillis();
        this.modifiche = new ArrayList<>();
        this.livelloIsolamento = livelloIsolamento;
        this.stato = StatoTransazione.ATTIVA;
    }
    
    public void aggiungiModifica(ModificaDatabase modifica) {
        if (stato != StatoTransazione.ATTIVA) {
            throw new EccezioneTransazione(
                "Non posso modificare transazione non attiva");
        }
        modifiche.add(modifica);
    }
    
    public void commit() {
        if (stato != StatoTransazione.ATTIVA) {
            throw new EccezioneTransazione(
                "Non posso fare commit di transazione non attiva");
        }
        
        try {
            // Valida tutte le modifiche
            validaModifiche();
            
            // Applica modifiche atomicamente
            applicaModifiche();
            
            stato = StatoTransazione.COMMITTATA;
            
        } catch (Exception e) {
            stato = StatoTransazione.FALLITA;
            throw new EccezioneTransazione("Commit fallito", e);
        }
    }
    
    public void rollback() {
        if (stato == StatoTransazione.COMMITTATA) {
            throw new EccezioneTransazione(
                "Non posso fare rollback di transazione committata");
        }
        
        // Annulla modifiche in ordine inverso
        Collections.reverse(modifiche);
        for (ModificaDatabase modifica : modifiche) {
            modifica.annulla();
        }
        
        stato = StatoTransazione.ROLLBACK_ESEGUITO;
    }
    
    private void validaModifiche() {
        ValidatoreModifiche validatore = new ValidatoreModifiche();
        for (ModificaDatabase modifica : modifiche) {
            validatore.valida(modifica);
        }
    }
    
    private void applicaModifiche() {
        for (ModificaDatabase modifica : modifiche) {
            modifica.applica();
        }
    }
}

public abstract class ModificaDatabase {
    protected final long timestamp;
    protected final IndicatorePredicato predicato;
    protected boolean applicata = false;
    
    public ModificaDatabase(IndicatorePredicato predicato) {
        this.timestamp = System.currentTimeMillis();
        this.predicato = predicato;
    }
    
    public abstract void applica();
    public abstract void annulla();
    public abstract TipoModifica getTipo();
    
    protected void marcaApplicata() {
        this.applicata = true;
    }
    
    protected void controllaGiaApplicata() {
        if (applicata) {
            throw new IllegalStateException("Modifica già applicata");
        }
    }
}

public class ModificaAsserzione extends ModificaDatabase {
    private final Clausola clausola;
    private final PosizioneAsserzione posizione;
    
    public ModificaAsserzione(IndicatorePredicato predicato, Clausola clausola,
            PosizioneAsserzione posizione) {
        super(predicato);
        this.clausola = clausola;
        this.posizione = posizione;
    }
    
    @Override
    public void applica() {
        controllaGiaApplicata();
        GestoreDatabase.getInstance().asserisciClausola(clausola, posizione);
        marcaApplicata();
    }
    
    @Override
    public void annulla() {
        if (!applicata) {
            throw new IllegalStateException("Modifica non applicata");
        }
        GestoreDatabase.getInstance().ritraiClausolaSpecifica(clausola);
    }
    
    @Override
    public TipoModifica getTipo() {
        return TipoModifica.ASSERZIONE;
    }
}
```

---

## 7.7 Esecuzione di un Obiettivo Prolog

### 7.7.1 Esecuzione

JProlog implementa un sofisticato motore di esecuzione obiettivi che gestisce il processo completo di risoluzione SLD con ottimizzazioni e supporto debugging.

### Motore di Esecuzione

```java
public class MotoreEsecuzioneObiettivi {
    private final DatabaseClausole database;
    private final MotoreUnificazione unificatore;
    private final StackPuntiScelta puntiScelta;
    private final GestoreCut gestoreCut;
    private final RegistroBuiltIn builtIns;
    private final TracciatoreEsecuzione tracciatore;
    
    public MotoreEsecuzioneObiettivi(DatabaseClausole database) {
        this.database = database;
        this.unificatore = new MotoreUnificazione();
        this.puntiScelta = new StackPuntiScelta();
        this.gestoreCut = new GestoreCut();
        this.builtIns = new RegistroBuiltIn();
        this.tracciatore = new TracciatoreEsecuzione();
    }
    
    public RisultatoEsecuzione esegui(Term obiettivo) {
        ContestoEsecuzione contesto = creaContestoEsecuzione(obiettivo);
        
        try {
            tracciatore.tracciaIngressoObiettivo(obiettivo, contesto);
            
            List<Substitution> soluzioni = risolviObiettivo(obiettivo, contesto);
            
            if (soluzioni.isEmpty()) {
                tracciatore.tracciaFallimentoObiettivo(obiettivo, contesto);
                return RisultatoEsecuzione.fallimento(obiettivo);
            } else {
                tracciatore.tracciaSuccessoObiettivo(obiettivo, soluzioni, contesto);
                return RisultatoEsecuzione.successo(obiettivo, soluzioni);
            }
            
        } catch (EccezioneEsecuzione e) {
            tracciatore.tracciaErroreObiettivo(obiettivo, e, contesto);
            return RisultatoEsecuzione.errore(obiettivo, e);
        }
    }
    
    private List<Substitution> risolviObiettivo(Term obiettivo, ContestoEsecuzione contesto) {
        // Controlla predicato built-in
        if (builtIns.isBuiltIn(obiettivo)) {
            return eseguiBuiltIn(obiettivo, contesto);
        }
        
        // Predicato definito dall'utente
        return eseguiDefinitoUtente(obiettivo, contesto);
    }
    
    private List<Substitution> eseguiDefinitoUtente(Term obiettivo, 
            ContestoEsecuzione contesto) {
        List<Substitution> soluzioni = new ArrayList<>();
        
        // Ottieni clausole corrispondenti
        List<Clausola> clausole = database.getClausole(obiettivo);
        if (clausole.isEmpty()) {
            return soluzioni; // Nessuna clausola = fallimento
        }
        
        // Prova ogni clausola
        for (int i = 0; i < clausole.size(); i++) {
            Clausola clausola = clausole.get(i);
            
            // Crea punto di scelta se rimangono più clausole
            if (i < clausole.size() - 1) {
                PuntoScelta ps = new PuntoScelta(obiettivo, clausole.subList(i + 1, clausole.size()), contesto);
                puntiScelta.push(ps);
            }
            
            // Prova questa clausola
            List<Substitution> soluzioniClausola = provaClausola(obiettivo, clausola, contesto);
            soluzioni.addAll(soluzioniClausola);
            
            // Se questa clausola ha avuto successo e c'è un cut, rimuovi punti di scelta
            if (!soluzioniClausola.isEmpty() && contieneCut(clausola)) {
                gestoreCut.eseguiCut(contesto);
                break; // Il cut impedisce di provare più clausole
            }
        }
        
        return soluzioni;
    }
    
    private List<Substitution> provaClausola(Term obiettivo, Clausola clausola, 
            ContestoEsecuzione contesto) {
        // Rinomina variabili nella clausola per evitare conflitti
        RinominatoreVariabili rinominatore = new RinominatoreVariabili();
        Clausola clausolaRinominata = clausola.rinomina(rinominatore);
        
        // Prova a unificare obiettivo con testa clausola
        Substitution unificazione = unificatore.unifica(obiettivo, clausolaRinominata.getTesta());
        if (unificazione == null) {
            return Collections.emptyList(); // Unificazione fallita
        }
        
        // Crea nuovo contesto con unificazione
        ContestoEsecuzione nuovoContesto = contesto.estendi(unificazione);
        
        // Esegui corpo clausola
        if (clausolaRinominata instanceof Fatto) {
            // Fatto - successo immediato
            return Collections.singletonList(unificazione);
        } else {
            // Regola - esegui corpo
            Regola regola = (Regola) clausolaRinominata;
            Term corpo = regola.getCorpo().applicaSostituzione(unificazione);
            
            return risolviObiettivo(corpo, nuovoContesto);
        }
    }
}
```

### 7.7.2 Tipi di Dati per il Modello di Esecuzione

```java
public class ContestoEsecuzione {
    private final Substitution sostituzioneCorrente;
    private final AmbienteEsecuzione ambiente;
    private final int profondita;
    private final long tempoInizio;
    private final Map<String, Object> proprieta;
    
    public ContestoEsecuzione() {
        this(new Substitution(), new AmbienteEsecuzione(), 0);
    }
    
    private ContestoEsecuzione(Substitution sostituzione,
            AmbienteEsecuzione ambiente, int profondita) {
        this.sostituzioneCorrente = sostituzione;
        this.ambiente = ambiente;
        this.profondita = profondita;
        this.tempoInizio = System.nanoTime();
        this.proprieta = new HashMap<>();
    }
    
    public ContestoEsecuzione estendi(Substitution nuovaSostituzione) {
        Substitution combinata = sostituzioneCorrente.componi(nuovaSostituzione);
        return new ContestoEsecuzione(combinata, ambiente, profondita + 1);
    }
    
    public ContestoEsecuzione entraScopo(String nomeScopo) {
        AmbienteEsecuzione nuovoAmb = ambiente.creaFiglio(nomeScopo);
        return new ContestoEsecuzione(sostituzioneCorrente, nuovoAmb, profondita);
    }
    
    public Term istanzia(Term termine) {
        return termine.applicaSostituzione(sostituzioneCorrente);
    }
    
    public boolean isVariabile(Term termine) {
        Term istanziato = istanzia(termine);
        return istanziato instanceof Variable;
    }
    
    public long getTempoTrascorso() {
        return System.nanoTime() - tempoInizio;
    }
}

public class AmbienteEsecuzione {
    private final String nome;
    private final AmbienteEsecuzione genitore;
    private final Map<String, Object> binding;
    private final Set<IndicatorePredicato> predicatiLocali;
    
    public AmbienteEsecuzione(String nome) {
        this(nome, null);
    }
    
    private AmbienteEsecuzione(String nome, AmbienteEsecuzione genitore) {
        this.nome = nome;
        this.genitore = genitore;
        this.binding = new HashMap<>();
        this.predicatiLocali = new HashSet<>();
    }
    
    public AmbienteEsecuzione creaFiglio(String nomeFiglio) {
        return new AmbienteEsecuzione(nomeFiglio, this);
    }
    
    public void legaVariabile(String nome, Object valore) {
        binding.put(nome, valore);
    }
    
    public Object cercaBinding(String nome) {
        Object valore = binding.get(nome);
        if (valore != null || genitore == null) {
            return valore;
        }
        return genitore.cercaBinding(nome);
    }
    
    public void dichiaraLocale(IndicatorePredicato ip) {
        predicatiLocali.add(ip);
    }
    
    public boolean isLocale(IndicatorePredicato ip) {
        return predicatiLocali.contains(ip) ||
               (genitore != null && genitore.isLocale(ip));
    }
}

public class PuntoScelta {
    private final Term obiettivo;
    private final List<Clausola> clausoleRimanenti;
    private final ContestoEsecuzione contesto;
    private final int livello;
    private final long timestamp;
    
    public PuntoScelta(Term obiettivo, List<Clausola> clausoleRimanenti,
            ContestoEsecuzione contesto) {
        this.obiettivo = obiettivo;
        this.clausoleRimanenti = new ArrayList<>(clausoleRimanenti);
        this.contesto = contesto;
        this.livello = contesto.getProfondita();
        this.timestamp = System.currentTimeMillis();
    }
    
    public boolean haPiuScelte() {
        return !clausoleRimanenti.isEmpty();
    }
    
    public Clausola prossimaScelta() {
        if (clausoleRimanenti.isEmpty()) {
            throw new IllegalStateException("Nessuna altra scelta");
        }
        return clausoleRimanenti.remove(0);
    }
    
    public ContestoEsecuzione ripristina() {
        return contesto;
    }
}
```

---

## 7.8 Costrutti di Controllo

### 7.8.1 true/0

```java
public class TrueBuiltIn implements BuiltIn {
    @Override
    public String getNome() { return "true"; }
    
    @Override
    public int getArita() { return 0; }
    
    @Override
    public RisultatoEsecuzione esegui(List<Term> args, ContestoEsecuzione contesto) {
        if (!args.isEmpty()) {
            throw new TypeError("true/0 chiamato con argomenti");
        }
        
        // true ha sempre successo con sostituzione vuota
        return RisultatoEsecuzione.successo(Collections.singletonList(
            new Substitution()));
    }
    
    @Override
    public boolean isDeterministico() { return true; }
    
    @Override
    public Set<ModalitaChiamata> getModalitaSupportate() {
        return Set.of(ModalitaChiamata.SEMIDET);
    }
}
```

### 7.8.2 fail/0

```java
public class FailBuiltIn implements BuiltIn {
    @Override
    public String getNome() { return "fail"; }
    
    @Override
    public int getArita() { return 0; }
    
    @Override
    public RisultatoEsecuzione esegui(List<Term> args, ContestoEsecuzione contesto) {
        if (!args.isEmpty()) {
            throw new TypeError("fail/0 chiamato con argomenti");
        }
        
        // fail fallisce sempre
        return RisultatoEsecuzione.fallimento();
    }
    
    @Override
    public boolean isDeterministico() { return true; }
    
    @Override
    public Set<ModalitaChiamata> getModalitaSupportate() {
        return Set.of(ModalitaChiamata.FAILURE);
    }
}
```

### 7.8.3 call/1

```java
public class CallBuiltIn implements BuiltIn {
    private final MotoreEsecuzioneObiettivi motore;
    
    public CallBuiltIn(MotoreEsecuzioneObiettivi motore) {
        this.motore = motore;
    }
    
    @Override
    public String getNome() { return "call"; }
    
    @Override
    public int getArita() { return 1; }
    
    @Override
    public RisultatoEsecuzione esegui(List<Term> args, ContestoEsecuzione contesto) {
        if (args.size() != 1) {
            throw new TypeError("call/1 si aspetta esattamente 1 argomento");
        }
        
        Term obiettivo = args.get(0).applicaSostituzione(contesto.getSostituzione());
        
        // Valida che l'obiettivo sia chiamabile
        if (!isChiamabile(obiettivo)) {
            throw new TypeError("L'argomento di call/1 deve essere chiamabile: " + obiettivo);
        }
        
        // Esegui l'obiettivo
        try {
            return motore.esegui(obiettivo, contesto);
        } catch (EccezioneEsecuzione e) {
            // Propaga errori di esecuzione
            throw e;
        }
    }
    
    private boolean isChiamabile(Term termine) {
        if (termine instanceof Atom) {
            return true;
        }
        if (termine instanceof TermineComposto) {
            TermineComposto composto = (TermineComposto) termine;
            // Controlla che il funtore non sia una variabile
            return !composto.getNome().isEmpty();
        }
        return false;
    }
    
    @Override
    public boolean isDeterministico() { return false; }
    
    @Override
    public Set<ModalitaChiamata> getModalitaSupportate() {
        return Set.of(ModalitaChiamata.NONDET);
    }
}
```

### 7.8.4 !/0 - Cut

```java
public class CutBuiltIn implements BuiltIn {
    private final GestoreCut gestoreCut;
    
    public CutBuiltIn(GestoreCut gestoreCut) {
        this.gestoreCut = gestoreCut;
    }
    
    @Override
    public String getNome() { return "!"; }
    
    @Override
    public int getArita() { return 0; }
    
    @Override
    public RisultatoEsecuzione esegui(List<Term> args, ContestoEsecuzione contesto) {
        if (!args.isEmpty()) {
            throw new TypeError("!/0 chiamato con argomenti");
        }
        
        // Esegui cut: rimuovi punti di scelta
        gestoreCut.eseguiCut(contesto);
        
        // Il cut ha sempre successo
        return RisultatoEsecuzione.successo(Collections.singletonList(
            new Substitution()));
    }
    
    @Override
    public boolean isDeterministico() { return true; }
    
    @Override
    public boolean haEffettiCollaterali() { return true; }
    
    @Override
    public Set<ModalitaChiamata> getModalitaSupportate() {
        return Set.of(ModalitaChiamata.SEMIDET);
    }
}

public class GestoreCut {
    public void eseguiCut(ContestoEsecuzione contesto) {
        StackPuntiScelta puntiScelta = contesto.getPuntiScelta();
        int livelloCorrente = contesto.getLivello();
        
        // Rimuovi tutti i punti di scelta creati al livello corrente o più profondo
        puntiScelta.removeIf(ps -> ps.getLivello() >= livelloCorrente);
        
        // Marca cut come eseguito nello scopo corrente
        contesto.marcaCutEseguito();
        
        // Logga esecuzione cut per debugging
        if (contesto.isTraceAbilitato()) {
            contesto.getTracciatore().registraCut(livelloCorrente);
        }
    }
    
    public boolean isCutEseguito(ContestoEsecuzione contesto) {
        return contesto.isCutEseguito();
    }
}
```

### 7.8.5 (,)/2 - Congiunzione

```java
public class CongiunzioneBuiltIn implements BuiltIn {
    private final MotoreEsecuzioneObiettivi motore;
    
    @Override
    public String getNome() { return ","; }
    
    @Override
    public int getArita() { return 2; }
    
    @Override
    public RisultatoEsecuzione esegui(List<Term> args, ContestoEsecuzione contesto) {
        if (args.size() != 2) {
            throw new TypeError("(,)/2 si aspetta esattamente 2 argomenti");
        }
        
        Term primoObiettivo = args.get(0);
        Term secondoObiettivo = args.get(1);
        
        List<Substitution> soluzioni = new ArrayList<>();
        
        // Esegui primo obiettivo
        RisultatoEsecuzione primoRisultato = motore.esegui(primoObiettivo, contesto);
        
        if (!primoRisultato.isSuccesso()) {
            return RisultatoEsecuzione.fallimento(); // Primo obiettivo fallito
        }
        
        // Per ogni soluzione del primo obiettivo, esegui il secondo obiettivo
        for (Substitution primaSoluzione : primoRisultato.getSoluzioni()) {
            ContestoEsecuzione nuovoContesto = contesto.estendi(primaSoluzione);
            
            // Applica prima soluzione al secondo obiettivo
            Term secondoObiettivoIstanziato = 
                secondoObiettivo.applicaSostituzione(primaSoluzione);
            
            // Esegui secondo obiettivo
            RisultatoEsecuzione secondoRisultato = 
                motore.esegui(secondoObiettivoIstanziato, nuovoContesto);
            
            if (secondoRisultato.isSuccesso()) {
                // Combina soluzioni
                for (Substitution secondaSoluzione : secondoRisultato.getSoluzioni()) {
                    Substitution combinata = primaSoluzione.componi(secondaSoluzione);
                    soluzioni.add(combinata);
                }
            }
        }
        
        if (soluzioni.isEmpty()) {
            return RisultatoEsecuzione.fallimento();
        } else {
            return RisultatoEsecuzione.successo(soluzioni);
        }
    }
    
    @Override
    public boolean isDeterministico() { return false; }
}
```

### 7.8.6 (;)/2 - Disgiunzione

```java
public class DisgiUnzioneBuiltIn implements BuiltIn {
    private final MotoreEsecuzioneObiettivi motore;
    
    @Override
    public String getNome() { return ";"; }
    
    @Override
    public int getArita() { return 2; }
    
    @Override
    public RisultatoEsecuzione esegui(List<Term> args, ContestoEsecuzione contesto) {
        if (args.size() != 2) {
            throw new TypeError("(;)/2 si aspetta esattamente 2 argomenti");
        }
        
        Term obiettivoSinistro = args.get(0);
        Term obiettivoDestro = args.get(1);
        
        // Controlla se questo è if-then-else: (Cond -> Then ; Else)
        if (obiettivoSinistro instanceof TermineComposto) {
            TermineComposto composto = (TermineComposto) obiettivoSinistro;
            if (composto.getNome().equals("->") && composto.getArita() == 2) {
                return eseguiIfThenElse(composto, obiettivoDestro, contesto);
            }
        }
        
        // Disgiunzione normale: prova prima obiettivo sinistro
        List<Substitution> soluzioni = new ArrayList<>();
        
        RisultatoEsecuzione risultatoSinistro = motore.esegui(obiettivoSinistro, contesto);
        if (risultatoSinistro.isSuccesso()) {
            soluzioni.addAll(risultatoSinistro.getSoluzioni());
        }
        
        // Poi prova obiettivo destro
        RisultatoEsecuzione risultatoDestro = motore.esegui(obiettivoDestro, contesto);
        if (risultatoDestro.isSuccesso()) {
            soluzioni.addAll(risultatoDestro.getSoluzioni());
        }
        
        if (soluzioni.isEmpty()) {
            return RisultatoEsecuzione.fallimento();
        } else {
            return RisultatoEsecuzione.successo(soluzioni);
        }
    }
    
    private RisultatoEsecuzione eseguiIfThenElse(TermineComposto ifThen,
            Term obiettivoElse, ContestoEsecuzione contesto) {
        Term condizione = ifThen.getArgomento(0);
        Term obiettivoThen = ifThen.getArgomento(1);
        
        // Prova condizione
        RisultatoEsecuzione risultatoCondizione = motore.esegui(condizione, contesto);
        
        if (risultatoCondizione.isSuccesso()) {
            // Condizione riuscita - esegui parte then per ogni soluzione
            List<Substitution> soluzioni = new ArrayList<>();
            
            for (Substitution soluzioneCondizione : risultatoCondizione.getSoluzioni()) {
                ContestoEsecuzione nuovoContesto = contesto.estendi(soluzioneCondizione);
                Term thenIstanziato = obiettivoThen.applicaSostituzione(soluzioneCondizione);
                
                RisultatoEsecuzione risultatoThen = motore.esegui(thenIstanziato, nuovoContesto);
                if (risultatoThen.isSuccesso()) {
                    for (Substitution soluzioneThen : risultatoThen.getSoluzioni()) {
                        soluzioni.add(soluzioneCondizione.componi(soluzioneThen));
                    }
                }
            }
            
            return soluzioni.isEmpty() ? RisultatoEsecuzione.fallimento() : 
                RisultatoEsecuzione.successo(soluzioni);
        } else {
            // Condizione fallita - esegui parte else
            return motore.esegui(obiettivoElse, contesto);
        }
    }
}
```

---

## Conclusione

Questa analisi completa del modello di esecuzione di JProlog dimostra un'implementazione sofisticata che segue da vicino la semantica ISO Prolog fornendo estensioni pratiche e ottimizzazioni. Le principali conquiste architetturali includono:

1. **Pipeline di Esecuzione Completa**: Dal processamento delle direttive attraverso l'esecuzione degli obiettivi con appropriata gestione errori
2. **Gestione Database Dinamica**: Supporto completo per operazioni assert/retract con semantica transazionale
3. **Costrutti di Controllo Avanzati**: Implementazione appropriata di cut, congiunzione, disgiunzione e if-then-else
4. **Sistema I/O Completo**: I/O completo basato su stream con supporto per modalità testo e binario
5. **Gestione Errori Conforme ISO**: Tassonomia errori completa con appropriata propagazione eccezioni
6. **Ottimizzazioni Prestazioni**: Gestione punti di scelta, indicizzazione e monitoraggio esecuzione

L'implementazione fornisce una base solida sia per uso educativo che applicazioni di produzione, con attenzione particolare alla correttezza, prestazioni e manutenibilità.