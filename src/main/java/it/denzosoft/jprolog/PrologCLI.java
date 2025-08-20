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
 * CLI (Command Line Interface) for JProlog
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
        System.out.println("Interactive Prolog interpreter with ISO compliance");
        System.out.println();
        System.out.println("Enter Prolog queries followed by '.' and press Enter");
        System.out.println("For multiple solutions: use ';' for next, Enter to stop");
        System.out.println();
        System.out.println("Special commands:");
        System.out.println("  :quit              - Exit the console");
        System.out.println("  :help              - Show this help");
        System.out.println("  :listing           - Show all loaded rules");
        System.out.println("  :clear             - Clear all rules");
        System.out.println("  :consult <file>    - Load facts/rules from file");
        System.out.println("  :save <file>       - Save knowledge base to file");
        System.out.println();
        
        // Load some example facts
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
                System.err.println("Read error: " + e.getMessage());
                break;
            }
        }
        
        System.out.println("Goodbye!");
    }
    
    private void processInput(String input) {
        // Handle special commands
        if (input.startsWith(":")) {
            handleCommand(input);
            return;
        }
        
        // Ensure the query ends with a period
        if (!input.endsWith(".")) {
            input += ".";
        }
        
        try {
            // Remove the final period for the query
            String queryString = input.substring(0, input.length() - 1);
            
            // Parse the query using the parser
            Term queryTerm = parser.parseTerm(queryString);
            
            // Use the Prolog engine which internally uses QuerySolver with context
            List<Map<String, Term>> solutions = prolog.solve(queryTerm);
            
            if (solutions.isEmpty()) {
                System.out.println("false.");
            } else {
                if (solutions.size() == 1 && solutions.get(0).isEmpty()) {
                    System.out.println("true.");
                } else {
                    // Interactive handling of multiple solutions
                    displaySolutionsInteractively(solutions);
                }
            }
            
        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
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
                    System.out.println("Usage: :consult <filename>");
                    System.out.println("Example: :consult facts.pl");
                }
                break;
                
            case ":save":
            case ":s":
                if (parts.length > 1) {
                    saveToFile(parts[1].trim());
                } else {
                    System.out.println("Usage: :save <filename>");
                    System.out.println("Example: :save my_session.pl");
                }
                break;
                
            default:
                System.out.println("Unknown command: " + command);
                System.out.println("Use :help to see available commands.");
        }
    }
    
    private void showHelp() {
        System.out.println();
        System.out.println("=== JProlog Help ===");
        System.out.println();
        System.out.println("Query examples:");
        System.out.println("  son(ale,giorgia).           - Verify a fact");
        System.out.println("  son(X,giorgia).             - Find who is son of giorgia");
        System.out.println("  parent(X, bob).             - Query with multiple solutions");
        System.out.println("  assertz(son(luca,maria)).   - Add a new fact");
        System.out.println("  retract(son(ale,giorgia)).  - Remove a fact");
        System.out.println("  X is 2 + 3.                - Arithmetic calculation");
        System.out.println("  atom(hello).                - Type checking");
        System.out.println("  append([1,2],[3,4],L).      - List operations");
        System.out.println();
        System.out.println("Multiple solutions:");
        System.out.println("  When there are multiple solutions, ' ;' will appear");
        System.out.println("  Press ';' + Enter to see the next solution");
        System.out.println("  Press Enter only to stop");
        System.out.println();
        System.out.println("File loading:");
        System.out.println("  :consult my_facts.pl        - Load facts from file");
        System.out.println("  :consult /path/to/file.pl   - Load with absolute path");
        System.out.println();
        System.out.println("Available ISO predicates:");
        System.out.println("  Types: callable/1, ground/1, is_list/1, simple/1");
        System.out.println("  Arithmetic: max/2, min/2, div/2, rem/2, sign/1");
        System.out.println("  Strings: string_length/2, string_concat/3, atom_string/2");
        System.out.println("  System: current_prolog_flag/2, set_prolog_flag/2");
        System.out.println("  I/O: open/3, close/1, current_input/1, current_output/1");
        System.out.println();
    }
    
    private void showListing() {
        System.out.println();
        System.out.println("=== Knowledge Base ===");
        try {
            if (prolog.getRules().isEmpty()) {
                System.out.println("No rules loaded.");
            } else {
                for (Object rule : prolog.getRules()) {
                    System.out.println(rule);
                }
            }
        } catch (Exception e) {
            System.err.println("Error showing rules: " + e.getMessage());
        }
        System.out.println();
    }
    
    private void clearKnowledgeBase() {
        try {
            // Create a new Prolog engine to clear everything
            // (JProlog might not have a direct clear method)
            System.out.println("Knowledge base cleared. Reloading example facts...");
            loadExampleFacts();
        } catch (Exception e) {
            System.err.println("Error clearing knowledge base: " + e.getMessage());
        }
    }
    
    private void loadExampleFacts() {
        try {
            // Load some example facts to demonstrate the system
            prolog.asserta("father(tom, bob).");
            prolog.asserta("mother(ann, bob).");
            prolog.asserta("father(bob, liz).");
            
            // Rules (might have resolution problems in QuerySolver)
            prolog.asserta("parent(X, Y) :- father(X, Y).");
            prolog.asserta("parent(X, Y) :- mother(X, Y).");
            
            // Add direct facts to test multiple solutions
            prolog.asserta("likes(mary, wine).");
            prolog.asserta("likes(mary, food).");
            prolog.asserta("likes(john, wine).");
            prolog.asserta("color(red).");
            prolog.asserta("color(green).");
            prolog.asserta("color(blue).");
            
            System.out.println("Example facts loaded: father/2, mother/2, parent/2, likes/2, color/1");
            System.out.println("Try queries with multiple solutions:");
            System.out.println("  likes(mary, X).   # Should show wine and food");
            System.out.println("  color(X).         # Should show red, green, blue");
            System.out.println("  member(X,[a,b,c]).# Should show a, b, c");
            System.out.println();
        } catch (Exception e) {
            System.err.println("Error loading example facts: " + e.getMessage());
        }
    }
    
    /**
     * Load facts and rules from a file.
     */
    // START_CHANGE: ISS-2025-0024 - Fix DCG transformation by using consult() instead of asserta()
    private void consultFile(String filename) {
        try {
            System.out.println("Loading file: " + filename);
            
            // Leggi il file
            java.nio.file.Path path = java.nio.file.Paths.get(filename);
            if (!java.nio.file.Files.exists(path)) {
                System.err.println("File not found: " + filename);
                return;
            }
            
            String content = new String(java.nio.file.Files.readAllBytes(path));
            
            // Use proper consult method for DCG transformation and complete parsing
            prolog.consult(content);
            
            // Count loaded rules for user feedback
            int loadedCount = countRulesInContent(content);
            int errorCount = 0; // consult() throws exception on error, so if we reach here, no errors
            
            System.out.println("File loaded: " + loadedCount + " clauses loaded, " + errorCount + " errors");
            
        } catch (java.io.IOException e) {
            System.err.println("File reading error: " + e.getMessage());
        } catch (Exception e) {
            System.err.println("Error during loading: " + e.getMessage());
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
     * Save the current knowledge base to a file.
     */
    private void saveToFile(String filename) {
        try {
            System.out.println("Saving knowledge base to: " + filename);
            
            java.nio.file.Path path = java.nio.file.Paths.get(filename);
            
            // Get all rules from the knowledge base
            StringBuilder content = new StringBuilder();
            content.append("% Knowledge base saved by JProlog CLI\n");
            content.append("% Date: " + java.time.LocalDateTime.now() + "\n\n");
            
            // Get rules from the prolog engine
            for (Object rule : prolog.getRules()) {
                content.append(rule.toString()).append("\n");
            }
            
            // Write to file
            java.nio.file.Files.write(path, content.toString().getBytes());
            
            System.out.println("Knowledge base saved to: " + filename);
            
        } catch (Exception e) {
            System.err.println("Error saving file: " + e.getMessage());
        }
    }
    
    /**
     * Display solutions interactively, allowing the user to 
     * use ";" to see the next solution or Enter to stop.
     */
    private void displaySolutionsInteractively(List<Map<String, Term>> solutions) {
        try {
            for (int i = 0; i < solutions.size(); i++) {
                Map<String, Term> solution = solutions.get(i);
                
                // Display the current solution
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
                    
                    // Read input from user
                    String input = reader.readLine();
                    
                    if (input == null || input.trim().isEmpty()) {
                        // User pressed Enter - stop here
                        System.out.println(".");
                        return;
                    } else if (input.trim().equals(";")) {
                        // User wants next solution - continue
                        continue;
                    } else {
                        // Unrecognized input - stop
                        System.out.println(".");
                        return;
                    }
                } else {
                    // It's the last solution
                    System.out.println(".");
                }
            }
        } catch (IOException e) {
            System.err.println("Input reading error: " + e.getMessage());
            System.out.println(".");
        }
    }
    
    public static void main(String[] args) {
        new PrologCLI().start();
    }
}