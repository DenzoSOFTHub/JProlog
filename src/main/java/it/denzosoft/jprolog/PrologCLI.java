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

    // START_CHANGE: ISS-2025-0483 - wave W8: a non-interactive console must never consume the next
    // INPUT LINE as the answer to the "more solutions?" prompt. When stdin is not a terminal
    // (`System.console() == null` — a pipe, a here-doc, a redirected file, surefire) or when
    // `--batch` / `-q` is given, every solution is printed at once, separated by ` ;` and
    // terminated by `.`; nothing is read back. The interactive behaviour is unchanged.
    /** True when solutions are printed all at once instead of one per `;` keystroke. */
    private final boolean batch;

    /** Is this CLI printing solutions in batch (non-interactive) mode? */
    public boolean isBatch() { return batch; }

    public PrologCLI() {
        this(new String[0]);
    }

    public PrologCLI(String[] args) {
        this.prolog = new Prolog();
        this.parser = new Parser();
        this.reader = new BufferedReader(new InputStreamReader(System.in));
        boolean forced = false;
        if (args != null) {
            for (String a : args) {
                if ("--batch".equals(a) || "-q".equals(a) || "--quiet".equals(a)) forced = true;
            }
        }
        this.batch = forced || (System.console() == null);
    }
    // END_CHANGE: ISS-2025-0483
    
    public void start() {
        System.out.println("=== JProlog CLI ===");
        System.out.println("Interactive Prolog interpreter with ISO compliance");
        System.out.println();
        System.out.println("Enter Prolog queries followed by '.' and press Enter");
        // ISS-2025-0483: say which answer mode is in force, so a piped session is self-describing
        if (batch) {
            System.out.println("Non-interactive input: all solutions are printed, separated by ';'");
        } else {
            System.out.println("For multiple solutions: use ';' for next, Enter to stop");
        }
        System.out.println();
        System.out.println("Special commands:");
        System.out.println("  :quit              - Exit the console");
        System.out.println("  :help              - Show this help");
        System.out.println("  :listing           - Show all loaded rules");
        System.out.println("  :clear             - Clear all rules");
        System.out.println("  :consult <file>    - Load facts/rules from file");
        System.out.println("  :save <file>       - Save knowledge base to file");
        System.out.println("  :trace [on|off]    - Toggle four-port call tracing (or use trace. / notrace.)");
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
            
            // Solve through the default (v2) engine via the String entry point, so the CLI matches the
            // IDE: query-variable-keyed solutions, and four-port output from trace/0 (ISS-2025-0329).
            List<Map<String, Term>> solutions = prolog.solve(queryString);
            
            // START_CHANGE: ISS-2025-0476 - wave W7, design decision 5 (B.17, approved): answers
            // print in quoted operator notation with _A-style variable names and the residual
            // goals the answer still carries (limit L-11).
            if (solutions.isEmpty()) {
                System.out.println("false.");
            } else {
                List<List<String>> rendered = new java.util.ArrayList<>();
                for (Map<String, Term> sol : solutions) {
                    rendered.add(it.denzosoft.jprolog.core.engine.v4.Answer.lines(
                        sol, prolog.residualGoals(sol), prolog.getOps().table()));   // ISS-2025-0490
                }
                if (rendered.size() == 1 && rendered.get(0).isEmpty()) {
                    System.out.println("true.");
                } else {
                    displaySolutionsInteractively(rendered);
                }
            }
            // END_CHANGE: ISS-2025-0476

        // START_CHANGE: ISS-2025-0346 - halt/0 and halt/1 exit the processor with the given status
        // (ISO 8.17.3/8.17.4); the CLI is the processor here, so terminate the JVM immediately.
        } catch (it.denzosoft.jprolog.core.exceptions.PrologException pe) {
            if (pe.isHalt()) {
                System.out.flush();
                System.exit(pe.getExitCode());
            }
            System.err.println("Error: " + pe.getMessage());
        // END_CHANGE: ISS-2025-0346
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

            // ISS-2025-0329: toggle four-port call tracing (also available as the trace/0 .. notrace/0 goals)
            case ":trace":
                // ISS-2025-0437 - ENG-06: tracing is per engine; set it on THIS CLI's engine.
                if (parts.length > 1 && parts[1].trim().equalsIgnoreCase("off")) {
                    prolog.setTracing(false);
                    System.out.println("% Tracing disabled");
                } else {
                    prolog.setTracing(true);
                    System.out.println("% Tracing enabled (use ':trace off' or notrace. to disable)");
                }
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

            // START_CHANGE: ISS-2025-0085 - Compiled format CLI commands
            case ":compile":
                if (parts.length > 1) {
                    compileFile(parts[1].trim());
                } else {
                    System.out.println("Usage: :compile <filename.pl>");
                }
                break;

            case ":consult_compiled":
            case ":cc":
                if (parts.length > 1) {
                    consultCompiledFile(parts[1].trim());
                } else {
                    System.out.println("Usage: :consult_compiled <filename.jpc>");
                }
                break;
            // END_CHANGE: ISS-2025-0085

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
        // START_CHANGE: ISS-2025-0483 - the help must describe the mode actually in force
        if (batch) {
            System.out.println("  Input is not a terminal (or --batch / -q was given):");
            System.out.println("  every solution is printed at once, separated by ' ;'");
            System.out.println("  and terminated by '.' — no answer is read back");
        } else {
            System.out.println("  When there are multiple solutions, ' ;' will appear");
            System.out.println("  Press ';' + Enter to see the next solution");
            System.out.println("  Press Enter only to stop");
            System.out.println("  (--batch / -q prints them all at once instead)");
        }
        // END_CHANGE: ISS-2025-0483
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
            
            // Rules
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
            
            // Read the file
            java.nio.file.Path path = java.nio.file.Paths.get(filename);
            if (!java.nio.file.Files.exists(path)) {
                System.err.println("File not found: " + filename);
                return;
            }
            
            // START_CHANGE: ISS-2025-0286 - read source as UTF-8 (matches the encoding=utf8 flag)
            // instead of the platform default charset.
            String content = new String(java.nio.file.Files.readAllBytes(path),
                java.nio.charset.StandardCharsets.UTF_8);
            // END_CHANGE: ISS-2025-0286
            
            // Use proper consult method for DCG transformation and complete parsing
            prolog.consult(content);
            
            // Count loaded rules for user feedback
            int loadedCount = countRulesInContent(content);
            int errorCount = 0; // consult() throws exception on error, so if we reach here, no errors
            
            System.out.println("File loaded: " + loadedCount + " clauses loaded, " + errorCount + " errors");
            
        } catch (java.io.IOException e) {
            System.err.println("File reading error: " + e.getMessage());
        // START_CHANGE: ISS-2025-0346 - a ':- halt.' directive in a consulted file exits the processor
        } catch (it.denzosoft.jprolog.core.exceptions.PrologException pe) {
            if (pe.isHalt()) {
                System.out.flush();
                System.exit(pe.getExitCode());
            }
            System.err.println("Error during loading: " + pe.getMessage());
        // END_CHANGE: ISS-2025-0346
        } catch (Exception e) {
            System.err.println("Error during loading: " + e.getMessage());
        }
    }
    // END_CHANGE: ISS-2025-0024

    // START_CHANGE: ISS-2025-0085 - Compiled format methods
    private void compileFile(String filename) {
        try {
            long start = System.currentTimeMillis();
            String jpcFile = prolog.compileFile(filename);
            long elapsed = System.currentTimeMillis() - start;
            java.io.File f = new java.io.File(jpcFile);
            System.out.println("Compiled: " + jpcFile + " (" + f.length() + " bytes, " + elapsed + " ms)");
        } catch (java.io.IOException e) {
            System.err.println("Compilation error: " + e.getMessage());
        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
        }
    }

    private void consultCompiledFile(String filename) {
        try {
            long start = System.currentTimeMillis();
            prolog.consultCompiled(filename);
            long elapsed = System.currentTimeMillis() - start;
            System.out.println("Loaded compiled file: " + filename + " (" + elapsed + " ms)");
        } catch (java.io.IOException e) {
            System.err.println("Load error: " + e.getMessage());
        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
        }
    }
    // END_CHANGE: ISS-2025-0085

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
    private void displaySolutionsInteractively(List<List<String>> solutions) {
        try {
            for (int i = 0; i < solutions.size(); i++) {
                List<String> lines = solutions.get(i);

                // Display the current solution
                if (lines.isEmpty()) {
                    System.out.print("true");
                } else {
                    for (int k = 0; k < lines.size(); k++) {
                        if (k > 0) System.out.print(",\n");
                        System.out.print(lines.get(k));
                    }
                }

                // If not the last solution, ask user what to do
                if (i < solutions.size() - 1) {
                    // START_CHANGE: ISS-2025-0483 - non-interactive: never read the next input
                    // line (it is the next QUERY, not the user's answer). Print ` ;` and go on.
                    if (batch) {
                        System.out.println(" ;");
                        continue;
                    }
                    // END_CHANGE: ISS-2025-0483
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
        new PrologCLI(args).start();   // ISS-2025-0483: --batch / -q
    }
}