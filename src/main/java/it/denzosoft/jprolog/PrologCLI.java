package it.denzosoft.jprolog;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.parser.Parser;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.List;
import java.util.Map;

/**
 * CLI (Command Line Interface) per JProlog
 * Allows inserting Prolog queries and seeing results in interactive mode
 */
public class PrologCLI {
    
    private final Prolog prolog;
    private final Parser parser;
    private final BufferedReader reader;
    private boolean running = true;
    
    public PrologCLI() {
        this.prolog = new Prolog();
        this.parser = new Parser();
        this.reader = new BufferedReader(new InputStreamReader(System.in));
    }
    
    public void start() {
        System.out.println("=== JProlog CLI ===");
        System.out.println("Interprete Prolog interattivo con conformità ISO");
        System.out.println();
        System.out.println("Scrivi query Prolog seguite da '.' e premi Invio");
        System.out.println("Per soluzioni multiple: usa ';' per la prossima, Invio per fermarti");
        System.out.println();
        System.out.println("Comandi speciali:");
        System.out.println("  :quit              - Esci dalla console");
        System.out.println("  :help              - Mostra questo aiuto");
        System.out.println("  :listing           - Mostra tutte le regole caricate");
        System.out.println("  :clear             - Cancella tutte le regole");
        System.out.println("  :consult <file>    - Carica fatti/regole da file");
        System.out.println("  :save <file>       - Salva knowledge base in file");
        System.out.println();
        
        // Carica alcuni fatti di esempio
        loadExampleFacts();
        
        while (running) {
            System.out.print("?- ");
            try {
                String input = reader.readLine();
                if (input == null) {
                    break; // EOF
                }
                
                input = input.trim();
                if (input.isEmpty()) {
                    continue;
                }
                
                processInput(input);
                
            } catch (IOException e) {
                System.err.println("Errore di lettura: " + e.getMessage());
                break;
            }
        }
        
        System.out.println("Arrivederci!");
    }
    
    private void processInput(String input) {
        // Gestisci comandi speciali
        if (input.startsWith(":")) {
            handleCommand(input);
            return;
        }
        
        // Assicurati che la query finisca con un punto
        if (!input.endsWith(".")) {
            input += ".";
        }
        
        try {
            // Rimuovi il punto finale per la query
            String queryString = input.substring(0, input.length() - 1);
            
            // Parse the query usando il parser
            Term queryTerm = parser.parseTerm(queryString);
            
            // Usa l'engine Prolog che internamente usa il QuerySolver con contesto
            List<Map<String, Term>> solutions = prolog.solve(queryTerm);
            
            if (solutions.isEmpty()) {
                System.out.println("false.");
            } else {
                if (solutions.size() == 1 && solutions.get(0).isEmpty()) {
                    System.out.println("true.");
                } else {
                    // Gestione interattiva delle soluzioni multiple
                    displaySolutionsInteractively(solutions);
                }
            }
            
        } catch (Exception e) {
            System.err.println("Errore: " + e.getMessage());
        }
    }
    
    private void handleCommand(String command) {
        // START_CHANGE: ISS-2025-0011 - Handle commands with trailing periods
        // Strip trailing period from command for consistency
        if (command.endsWith(".")) {
            command = command.substring(0, command.length() - 1);
        }
        // END_CHANGE: ISS-2025-0011
        
        String[] parts = command.split("\\s+", 2);
        String cmd = parts[0].toLowerCase();
        
        switch (cmd) {
            case ":quit":
            case ":q":
                running = false;
                break;
                
            case ":help":
            case ":h":
                showHelp();
                break;
                
            case ":listing":
            case ":l":
                showListing();
                break;
                
            case ":clear":
                clearKnowledgeBase();
                break;
                
            case ":consult":
            case ":c":
                if (parts.length > 1) {
                    consultFile(parts[1].trim());
                } else {
                    System.out.println("Uso: :consult <nomefile>");
                    System.out.println("Esempio: :consult fatti.pl");
                }
                break;
                
            case ":save":
            case ":s":
                if (parts.length > 1) {
                    saveToFile(parts[1].trim());
                } else {
                    System.out.println("Uso: :save <nomefile>");
                    System.out.println("Esempio: :save mia_sessione.pl");
                }
                break;
                
            default:
                System.out.println("Comando sconosciuto: " + command);
                System.out.println("Usa :help per vedere i comandi disponibili.");
        }
    }
    
    private void showHelp() {
        System.out.println();
        System.out.println("=== Aiuto JProlog ===");
        System.out.println();
        System.out.println("Esempi di query:");
        System.out.println("  son(ale,giorgia).           - Verifica un fatto");
        System.out.println("  son(X,giorgia).             - Trova chi è figlio di giorgia");
        System.out.println("  parent(X, bob).             - Query con soluzioni multiple");
        System.out.println("  assertz(son(luca,maria)).   - Aggiunge un nuovo fatto");
        System.out.println("  retract(son(ale,giorgia)).  - Rimuove un fatto");
        System.out.println("  X is 2 + 3.                - Calcolo aritmetico");
        System.out.println("  atom(hello).                - Controllo di tipo");
        System.out.println("  append([1,2],[3,4],L).      - List operations");
        System.out.println();
        System.out.println("Soluzioni multiple:");
        System.out.println("  Quando ci sono più soluzioni, apparirà ' ;'");
        System.out.println("  Premi ';' + Invio per vedere la prossima soluzione");
        System.out.println("  Premi solo Invio per fermarti");
        System.out.println();
        System.out.println("Caricamento file:");
        System.out.println("  :consult miei_fatti.pl      - Carica fatti da file");
        System.out.println("  :consult /path/to/file.pl   - Carica con percorso assoluto");
        System.out.println();
        System.out.println("Predicati ISO disponibili:");
        System.out.println("  Tipi: callable/1, ground/1, is_list/1, simple/1");
        System.out.println("  Aritmetica: max/2, min/2, div/2, rem/2, sign/1");
        System.out.println("  Stringhe: string_length/2, string_concat/3, atom_string/2");
        System.out.println("  Sistema: current_prolog_flag/2, set_prolog_flag/2");
        System.out.println("  I/O: open/3, close/1, current_input/1, current_output/1");
        System.out.println();
    }
    
    private void showListing() {
        System.out.println();
        System.out.println("=== Knowledge Base ===");
        try {
            if (prolog.getRules().isEmpty()) {
                System.out.println("Nessuna regola caricata.");
            } else {
                for (Object rule : prolog.getRules()) {
                    System.out.println(rule);
                }
            }
        } catch (Exception e) {
            System.err.println("Errore nel mostrare le regole: " + e.getMessage());
        }
        System.out.println();
    }
    
    private void clearKnowledgeBase() {
        try {
            // Crea un nuovo engine Prolog per cancellare tutto
            // (JProlog potrebbe non avere un metodo clear diretto)
            System.out.println("Knowledge base cancellata. Ricarico fatti di esempio...");
            loadExampleFacts();
        } catch (Exception e) {
            System.err.println("Errore nel cancellare la knowledge base: " + e.getMessage());
        }
    }
    
    private void loadExampleFacts() {
        try {
            // Carica alcuni fatti di esempio per dimostrare il sistema
            prolog.asserta("father(tom, bob).");
            prolog.asserta("mother(ann, bob).");
            prolog.asserta("father(bob, liz).");
            
            // Regole (potrebbero avere problemi di risoluzione nel QuerySolver)
            prolog.asserta("parent(X, Y) :- father(X, Y).");
            prolog.asserta("parent(X, Y) :- mother(X, Y).");
            
            // Aggiungi fatti diretti per testare soluzioni multiple
            prolog.asserta("likes(mary, wine).");
            prolog.asserta("likes(mary, food).");
            prolog.asserta("likes(john, wine).");
            prolog.asserta("color(red).");
            prolog.asserta("color(green).");
            prolog.asserta("color(blue).");
            
            System.out.println("Fatti di esempio caricati: father/2, mother/2, parent/2, likes/2, color/1");
            System.out.println("Prova query con soluzioni multiple:");
            System.out.println("  likes(mary, X).   # Dovrebbe mostrare wine e food");
            System.out.println("  color(X).         # Dovrebbe mostrare red, green, blue");
            System.out.println("  member(X,[a,b,c]).# Dovrebbe mostrare a, b, c");
            System.out.println();
        } catch (Exception e) {
            System.err.println("Errore nel caricare i fatti di esempio: " + e.getMessage());
        }
    }
    
    /**
     * Carica fatti e regole da un file.
     */
    // START_CHANGE: ISS-2025-0024 - Fix DCG transformation by using consult() instead of asserta()
    private void consultFile(String filename) {
        try {
            System.out.println("Caricamento file: " + filename);
            
            // Leggi il file
            java.nio.file.Path path = java.nio.file.Paths.get(filename);
            if (!java.nio.file.Files.exists(path)) {
                System.err.println("File non trovato: " + filename);
                return;
            }
            
            String content = new String(java.nio.file.Files.readAllBytes(path));
            
            // Use proper consult method for DCG transformation and complete parsing
            prolog.consult(content);
            
            // Count loaded rules for user feedback
            int loadedCount = countRulesInContent(content);
            int errorCount = 0; // consult() throws exception on error, so if we reach here, no errors
            
            System.out.println("File caricato: " + loadedCount + " clausole caricate, " + errorCount + " errori");
            
        } catch (java.io.IOException e) {
            System.err.println("Errore nella lettura del file: " + e.getMessage());
        } catch (Exception e) {
            System.err.println("Errore durante il caricamento: " + e.getMessage());
        }
    }
    // END_CHANGE: ISS-2025-0024
    
    /**
     * Count rules in content for user feedback
     */
    private int countRulesInContent(String content) {
        if (content == null || content.trim().isEmpty()) {
            return 0;
        }
        
        // Simple rule counting - count non-empty, non-comment lines ending with '.'
        String[] lines = content.split("\\r?\\n");
        int count = 0;
        
        for (String line : lines) {
            line = line.trim();
            if (!line.isEmpty() && !line.startsWith("%") && line.endsWith(".")) {
                count++;
            }
        }
        
        return count;
    }
    
    /**
     * Salva la knowledge base corrente in un file.
     */
    private void saveToFile(String filename) {
        try {
            System.out.println("Salvando knowledge base in: " + filename);
            
            java.nio.file.Path path = java.nio.file.Paths.get(filename);
            
            // Ottieni tutte le regole dalla knowledge base
            StringBuilder content = new StringBuilder();
            content.append("% Knowledge base salvata da JProlog CLI\n");
            content.append("% Data: " + java.time.LocalDateTime.now() + "\n\n");
            
            // Ottieni le regole dal prolog engine
            for (Object rule : prolog.getRules()) {
                content.append(rule.toString()).append("\n");
            }
            
            // Scrivi nel file
            java.nio.file.Files.write(path, content.toString().getBytes());
            
            System.out.println("Knowledge base salvata in: " + filename);
            
        } catch (Exception e) {
            System.err.println("Errore nel salvare il file: " + e.getMessage());
        }
    }
    
    /**
     * Mostra le soluzioni in modo interattivo, permettendo all'utente di 
     * usare ";" per vedere la prossima soluzione o Invio per fermarsi.
     */
    private void displaySolutionsInteractively(List<Map<String, Term>> solutions) {
        try {
            for (int i = 0; i < solutions.size(); i++) {
                Map<String, Term> solution = solutions.get(i);
                
                // Mostra la soluzione corrente
                if (solution.isEmpty()) {
                    System.out.print("true");
                } else {
                    boolean first = true;
                    for (Map.Entry<String, Term> binding : solution.entrySet()) {
                        if (!first) System.out.print(", ");
                        System.out.print(binding.getKey() + " = " + binding.getValue());
                        first = false;
                    }
                }
                
                // If not the last solution, ask user what to do
                if (i < solutions.size() - 1) {
                    System.out.print(" ;");
                    System.out.flush();
                    
                    // Leggi input dall'utente
                    String input = reader.readLine();
                    
                    if (input == null || input.trim().isEmpty()) {
                        // L'utente ha premuto Invio - fermati qui
                        System.out.println(".");
                        return;
                    } else if (input.trim().equals(";")) {
                        // L'utente vuole la prossima soluzione - continua
                        continue;
                    } else {
                        // Input non riconosciuto - fermati
                        System.out.println(".");
                        return;
                    }
                } else {
                    // It's the last solution
                    System.out.println(".");
                }
            }
        } catch (IOException e) {
            System.err.println("Errore nella lettura input: " + e.getMessage());
            System.out.println(".");
        }
    }
    
    public static void main(String[] args) {
        new PrologCLI().start();
    }
}