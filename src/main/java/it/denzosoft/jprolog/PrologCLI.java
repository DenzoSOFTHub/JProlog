package it.denzosoft.jprolog;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.exceptions.PrologException;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * CLI (Command Line Interface) for JProlog.
 *
 * <pre>
 *   jprolog [options] [file.pl ...]
 *     file.pl ...          consult the files, in order
 *     -g Goal              run Goal (once) after loading; failure or an uncaught error exits 1
 *     -t Goal              run Goal as the toplevel instead of the interactive loop; exit 0/1
 *     --safe               Prolog.enableSafeMode(allowHalt) before loading anything
 *     --budget N           inference budget per query (0 = unlimited)
 *     --max-solutions N    print at most N answers per query (default: unlimited, streamed)
 *     --demo               load the demo facts (father/2, likes/2, color/1, ...)
 *     --batch              non-interactive answers (the default when stdin is not a terminal)
 *     --interactive        prompt for ';' even when stdin is not a terminal
 *     -q, --quiet          --batch without the banner
 *     -h, --help           this text
 * </pre>
 *
 * <p>START_CHANGE: ISS-2025-0627 - 4.5 wave P6.4: answers are STREAMED. The CLI used to call
 * {@code prolog.solve(query)} — every solution computed and kept before the first was printed — so
 * {@code between(1, inf, X).} ran out of memory, {@code last([a|T], b).} (an infinite enumeration
 * since P4) ate the heap in batch mode, and the side effects of every solution ran before the first
 * prompt. Both modes now go through {@link Prolog#solveStream(String, Prolog.AnswerSink)}: one
 * answer is computed, printed, and (interactively) the user decides whether the next one is
 * computed at all; memory is constant per answer. Like SWI's toplevel the answer ends in {@code .}
 * when the machine has no alternative left, and a {@code ;} that finds no further answer prints
 * {@code false.}. The demo facts are loaded only with {@code --demo}; command-line files, goals,
 * safe mode, the budget and exit statuses are new (halt(N) exits N). The legacy {@code Parser}
 * field that nothing used is gone (ISS-2025-0637). END_CHANGE: ISS-2025-0627
 */
public class PrologCLI {

    private Prolog prolog;
    private final BufferedReader reader;
    private final PrintStream out;
    private final PrintStream err;
    private boolean running = true;

    // START_CHANGE: ISS-2025-0483 - wave W8: a non-interactive console must never consume the next
    // INPUT LINE as the answer to the "more solutions?" prompt. When stdin is not a terminal
    // (`System.console() == null` — a pipe, a here-doc, a redirected file, surefire) or when
    // `--batch` / `-q` is given, every solution is printed as it comes, separated by ` ;` and
    // terminated by `.`; nothing is read back. The interactive behaviour is unchanged.
    /** True when solutions are printed all at once instead of one per `;` keystroke. */
    private final boolean batch;

    /** Is this CLI printing solutions in batch (non-interactive) mode? */
    public boolean isBatch() { return batch; }
    // END_CHANGE: ISS-2025-0483

    // START_CHANGE: ISS-2025-0627 - the command line
    private final List<String> files = new ArrayList<String>();
    private final List<String> goals = new ArrayList<String>();
    private final List<String> argErrors = new ArrayList<String>();
    private String toplevelGoal;
    private boolean quiet, demo, safe, help;
    private long budget;
    private long maxSolutions;
    /** Set when the process must end now with {@link #exitStatus} (halt/1, -g, -t, main). */
    private boolean exitRequested;
    private int exitStatus;
    // END_CHANGE: ISS-2025-0627

    public PrologCLI() {
        this(new String[0]);
    }

    public PrologCLI(String[] args) {
        this(args, System.in, System.out, System.err);
    }

    /**
     * A CLI over explicit streams (tests, embedding); {@link #run()} never calls System.exit. The
     * answers, prompts and messages go to {@code out}/{@code err}; what the PROGRAM writes goes to
     * the engine's {@code user_output} as always.
     */
    public PrologCLI(String[] args, InputStream in, PrintStream out, PrintStream err) {
        this.reader = new BufferedReader(new InputStreamReader(in));
        this.out = out;
        this.err = err;
        boolean forced = false;
        boolean interactive = false;
        if (args != null) {
            for (int i = 0; i < args.length; i++) {
                String a = args[i];
                if ("--batch".equals(a)) forced = true;
                else if ("-q".equals(a) || "--quiet".equals(a)) { forced = true; quiet = true; }
                else if ("--interactive".equals(a)) interactive = true;
                else if ("--demo".equals(a)) demo = true;
                else if ("--safe".equals(a)) safe = true;
                else if ("-h".equals(a) || "--help".equals(a)) help = true;
                else if ("-g".equals(a) || "-t".equals(a) || "--budget".equals(a) || "--max-solutions".equals(a)) {
                    if (i + 1 >= args.length) { argErrors.add(a + " needs an argument"); continue; }
                    String v = args[++i];
                    if ("-g".equals(a)) goals.add(v);
                    else if ("-t".equals(a)) toplevelGoal = v;
                    else {
                        try {
                            long n = Long.parseLong(v);
                            if (n < 0) throw new NumberFormatException();
                            if ("--budget".equals(a)) budget = n; else maxSolutions = n;
                        } catch (NumberFormatException e) {
                            argErrors.add(a + " needs a non-negative integer, not " + v);
                        }
                    }
                } else if (a.startsWith("-") && a.length() > 1) {
                    argErrors.add("unknown option " + a);
                } else {
                    files.add(a);
                }
            }
        }
        this.batch = !interactive && (forced || System.console() == null);   // --interactive wins
        this.prolog = newEngine();
    }

    private Prolog newEngine() {
        Prolog p = new Prolog();
        if (safe) p.enableSafeMode(new it.denzosoft.jprolog.core.engine.SafeModeOptions().allowHalt());   // ISS-2025-0672: the CLI's process is the user's
        if (budget > 0) p.setInferenceBudget(budget);
        return p;
    }

    /** The engine this CLI drives. */
    public Prolog getProlog() { return prolog; }

    /** Backward-compatible entry point: run and ignore the status. */
    public void start() {
        run();
    }

    // START_CHANGE: ISS-2025-0627 - the program: files, -g goals, initialization(main), -t, the loop
    /** Run the CLI and return the process exit status. Never calls System.exit. */
    public int run() {
        if (!argErrors.isEmpty()) {
            for (String e : argErrors) err.println("jprolog: " + e);
            usage(err);
            return 2;
        }
        if (help) {
            usage(out);
            return 0;
        }
        try {
            // command-line files: initialization(G, main) is the program's main goal (ISS-2025-0636)
            prolog.setDeferInitializationMain(true);
            for (String f : files) {
                loadFile(f, false);
                if (exitRequested) return exitStatus;
            }
            prolog.setDeferInitializationMain(false);
            for (String g : goals) {
                int st = runGoal(g, null);
                if (exitRequested) return exitStatus;
                if (st != 0) return st;
            }
            List<Term> mains = prolog.takeInitializationMain();
            if (!mains.isEmpty()) {
                int st = 0;
                for (Term g : mains) {
                    st = runGoal(null, g);
                    if (exitRequested) return exitStatus;
                    if (st != 0) break;
                }
                return st;                          // SWI: main halts after the goal
            }
            if (toplevelGoal != null) {
                int st = runGoal(toplevelGoal, null);
                return exitRequested ? exitStatus : st;
            }
            loop();
            return exitRequested ? exitStatus : 0;
        } finally {
            out.flush();
            err.flush();
        }
    }

    /** Run one goal once (a -g/-t goal from text, or an initialization(main) goal). 0 / 1. */
    private int runGoal(String text, Term term) {
        String shown = (text != null) ? text : writeq(term);
        try {
            boolean ok;
            if (term != null) {
                ok = prolog.runOnce(term);
            } else {
                final boolean[] found = {false};
                prolog.solveStream(text, (java.util.function.Predicate<Map<String, Term>>) sol -> {
                    found[0] = true;
                    return false;
                });
                ok = found[0];
            }
            if (!ok) {
                err.println("Warning: goal (" + shown + ") failed");
                return 1;
            }
            return 0;
        } catch (PrologException pe) {
            if (pe.isHalt()) {
                requestExit(pe.getExitCode());
                return pe.getExitCode();
            }
            err.println("Error: " + errorText(pe));
            return 1;
        } catch (RuntimeException e) {
            // the budget, a Stop: not PrologExceptions (the trust model) — report and fail
            err.println("Error: " + e.getMessage());
            return 1;
        }
    }

    private void requestExit(int status) {
        exitRequested = true;
        exitStatus = status;
        running = false;
    }

    private void usage(PrintStream ps) {
        ps.println("Usage: jprolog [options] [file.pl ...]");
        ps.println("  file.pl ...          consult the files, in order");
        ps.println("  -g Goal              run Goal once after loading (failure or error: exit 1)");
        ps.println("  -t Goal              run Goal as the toplevel instead of the interactive loop");
        ps.println("  --safe               safe mode (no host access: files, processes, network, threads;");
        ps.println("                       halt/0,1 still end the CLI)");
        ps.println("  --budget N           inference budget per query (0 = unlimited)");
        ps.println("  --max-solutions N    print at most N answers per query");
        ps.println("  --demo               load the demo facts (father/2, likes/2, color/1, ...)");
        ps.println("  --batch              print answers without prompting (default without a terminal)");
        ps.println("  --interactive        prompt for ';' even when stdin is not a terminal");
        ps.println("  -q, --quiet          --batch without the banner");
        ps.println("  -h, --help           this text");
        ps.println("halt/0,1 and initialization(Goal, main) end the process with the goal's status.");
    }
    // END_CHANGE: ISS-2025-0627

    private void loop() {
        if (!quiet) banner();
        if (demo) loadExampleFacts();                  // ISS-2025-0627: only with --demo
        while (running) {
            out.print("?- ");
            out.flush();
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
                err.println("Read error: " + e.getMessage());
                break;
            }
        }
        if (!quiet) out.println("Goodbye!");
    }

    private void banner() {
        out.println("=== JProlog CLI ===");
        out.println("Interactive Prolog interpreter with ISO compliance");
        out.println();
        out.println("Enter Prolog queries followed by '.' and press Enter");
        // ISS-2025-0483: say which answer mode is in force, so a piped session is self-describing
        if (batch) {
            out.println("Non-interactive input: all solutions are printed, separated by ';'");
        } else {
            out.println("For multiple solutions: use ';' for next, Enter to stop");
        }
        out.println();
        out.println("Special commands:");
        out.println("  :quit              - Exit the console");
        out.println("  :help              - Show this help");
        out.println("  :listing           - Show all loaded rules");
        out.println("  :clear             - Clear all rules");
        out.println("  :consult <file>    - Load facts/rules from file");
        out.println("  :save <file>       - Save knowledge base to file");
        out.println("  :trace [on|off]    - Toggle four-port call tracing (or use trace. / notrace.)");
        out.println();
    }

    private void processInput(String input) {
        // Handle special commands
        if (input.startsWith(":")) {
            handleCommand(input);
            return;
        }
        if (!input.endsWith(".")) {
            input += ".";
        }
        String queryString = input.substring(0, input.length() - 1);
        // START_CHANGE: ISS-2025-0627 - stream the answers (see the class comment)
        final boolean[] pending = {false};   // an answer was printed with " ;" and more was asked for
        final long[] count = {0};
        try {
            prolog.solveStream(queryString, new Prolog.AnswerSink() {
                @Override public boolean onAnswer(Map<String, Term> sol, boolean more) {
                    count[0]++;
                    // ISS-2025-0476: quoted operator notation, _A-style names, residual goals
                    List<String> lines = it.denzosoft.jprolog.core.engine.v4.Answer.lines(
                        sol, prolog.residualGoals(sol), prolog.getOps().table());   // ISS-2025-0490
                    if (lines.isEmpty()) {
                        out.print("true");
                    } else {
                        for (int k = 0; k < lines.size(); k++) {
                            if (k > 0) out.print(",\n");
                            out.print(lines.get(k));
                        }
                    }
                    boolean capped = maxSolutions > 0 && count[0] >= maxSolutions;
                    if (!more || capped) {
                        out.println(".");
                        pending[0] = false;
                        return false;
                    }
                    if (batch) {
                        // ISS-2025-0483: never read an answer line; go on to the next solution
                        out.println(" ;");
                        pending[0] = true;
                        return true;
                    }
                    out.print(" ;");
                    out.flush();
                    String answer;
                    try {
                        answer = reader.readLine();
                    } catch (IOException e) {
                        answer = null;
                    }
                    if (answer != null && answer.trim().equals(";")) {
                        pending[0] = true;
                        return true;                  // compute the next answer
                    }
                    out.println(".");
                    pending[0] = false;
                    return false;
                }
            });
            if (count[0] == 0 || pending[0]) out.println("false.");
        // END_CHANGE: ISS-2025-0627
        // START_CHANGE: ISS-2025-0346 - halt/0 and halt/1 end the processor with the given status
        } catch (PrologException pe) {
            if (pe.isHalt()) {
                requestExit(pe.getExitCode());
                return;
            }
            err.println("Error: " + errorText(pe));   // ISS-2025-0565
        // END_CHANGE: ISS-2025-0346
        } catch (Exception e) {
            err.println("Error: " + e.getMessage());
        }
        out.flush();
    }

    // START_CHANGE: ISS-2025-0565 - the uncaught error is written like writeq/1 (operators,
    // quoting) through the engine's writer: existence_error(procedure, foo/0), not /(foo, 0).
    private String errorText(PrologException pe) {
        Term t = pe.getErrorTerm();
        if (t == null) return pe.getMessage();
        return writeq(t);
    }
    // END_CHANGE: ISS-2025-0565

    private String writeq(Term t) {
        it.denzosoft.jprolog.core.engine.v4.Writer.Options o = it.denzosoft.jprolog.core.engine.v4.Writer.Options.writeq();
        o.ops = prolog.getOps().table();
        o.spacingNextArgument = true;
        return it.denzosoft.jprolog.core.engine.v4.Writer.format(t, o);
    }

    private void handleCommand(String command) {
        // ISS-2025-0011 - strip a trailing period
        if (command.endsWith(".")) {
            command = command.substring(0, command.length() - 1);
        }
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
            // ISS-2025-0329: toggle four-port call tracing (also available as trace/0 .. notrace/0)
            case ":trace":
                // ISS-2025-0437 - ENG-06: tracing is per engine; set it on THIS CLI's engine.
                if (parts.length > 1 && parts[1].trim().equalsIgnoreCase("off")) {
                    prolog.setTracing(false);
                    out.println("% Tracing disabled");
                } else {
                    prolog.setTracing(true);
                    out.println("% Tracing enabled (use ':trace off' or notrace. to disable)");
                }
                break;
            case ":consult":
            case ":c":
                if (parts.length > 1) {
                    loadFile(parts[1].trim(), true);
                } else {
                    out.println("Usage: :consult <filename>");
                    out.println("Example: :consult facts.pl");
                }
                break;
            case ":save":
            case ":s":
                if (parts.length > 1) {
                    saveToFile(parts[1].trim());
                } else {
                    out.println("Usage: :save <filename>");
                    out.println("Example: :save my_session.pl");
                }
                break;
            // ISS-2025-0085 - Compiled format CLI commands
            case ":compile":
                if (parts.length > 1) {
                    compileFile(parts[1].trim());
                } else {
                    out.println("Usage: :compile <filename.pl>");
                }
                break;
            case ":consult_compiled":
            case ":cc":
                if (parts.length > 1) {
                    consultCompiledFile(parts[1].trim());
                } else {
                    out.println("Usage: :consult_compiled <filename.jpc>");
                }
                break;
            default:
                out.println("Unknown command: " + command);
                out.println("Use :help to see available commands.");
        }
    }

    private void showHelp() {
        out.println();
        out.println("=== JProlog Help ===");
        out.println();
        out.println("Query examples:");
        out.println("  assertz(son(luca,maria)).   - Add a new fact");
        out.println("  son(X,maria).               - Query it");
        out.println("  retract(son(luca,maria)).   - Remove a fact");
        out.println("  X is 2 + 3.                 - Arithmetic calculation");
        out.println("  atom(hello).                - Type checking");
        out.println("  append([1,2],[3,4],L).      - List operations");
        out.println();
        out.println("Multiple solutions:");
        // ISS-2025-0483 - the help must describe the mode actually in force
        if (batch) {
            out.println("  Input is not a terminal (or --batch / -q was given):");
            out.println("  every solution is printed as it is found, separated by ' ;'");
            out.println("  and terminated by '.' — no answer is read back");
        } else {
            out.println("  When more solutions may follow, ' ;' will appear");
            out.println("  Press ';' + Enter to compute the next solution");
            out.println("  Press Enter only to stop");
            out.println("  (--batch / -q prints them all without prompting instead)");
        }
        out.println();
        out.println("File loading:");
        out.println("  :consult my_facts.pl        - Load facts from file");
        out.println("  :consult /path/to/file.pl   - Load with absolute path");
        out.println();
        out.println("Command line:");
        usage(out);
        out.println();
    }

    private void showListing() {
        out.println();
        out.println("=== Knowledge Base ===");
        try {
            String listing = prolog.getListingOutput();
            out.println(listing.isEmpty() ? "No rules loaded." : listing);
        } catch (Exception e) {
            err.println("Error showing rules: " + e.getMessage());
        }
        out.println();
    }

    // ISS-2025-0627: `:clear` really clears now (it used to only reload the demo facts)
    private void clearKnowledgeBase() {
        prolog = newEngine();
        out.println("Knowledge base cleared.");
        if (demo) loadExampleFacts();
    }

    private void loadExampleFacts() {
        try {
            prolog.asserta("father(tom, bob).");
            prolog.asserta("mother(ann, bob).");
            prolog.asserta("father(bob, liz).");
            prolog.asserta("parent(X, Y) :- father(X, Y).");
            prolog.asserta("parent(X, Y) :- mother(X, Y).");
            prolog.asserta("likes(mary, wine).");
            prolog.asserta("likes(mary, food).");
            prolog.asserta("likes(john, wine).");
            prolog.asserta("color(red).");
            prolog.asserta("color(green).");
            prolog.asserta("color(blue).");
            out.println("Example facts loaded: father/2, mother/2, parent/2, likes/2, color/1");
            out.println("Try queries with multiple solutions:");
            out.println("  likes(mary, X).   # Should show wine and food");
            out.println("  color(X).         # Should show red, green, blue");
            out.println("  member(X,[a,b,c]).# Should show a, b, c");
            out.println();
        } catch (Exception e) {
            err.println("Error loading example facts: " + e.getMessage());
        }
    }

    /**
     * Load facts and rules from a file ({@code :consult}, or a command-line file when
     * {@code verbose} is false: only errors are printed then).
     */
    // START_CHANGE: ISS-2025-0024 - Fix DCG transformation by using consult() instead of asserta()
    private void loadFile(String filename, boolean verbose) {
        try {
            if (verbose) out.println("Loading file: " + filename);
            // ISS-2025-0578 - the loader resolves the name (".pl" added)
            java.nio.file.Path path = java.nio.file.Paths.get(filename);
            if (!java.nio.file.Files.exists(path) && !java.nio.file.Files.exists(java.nio.file.Paths.get(filename + ".pl"))) {
                err.println("File not found: " + filename);
                if (!verbose) requestExit(1);
                return;
            }
            // ISS-2025-0578 - load through the engine's loader and report the clauses it added
            Prolog.LoadResult r = prolog.loadFile(filename);
            for (Prolog.LoadError e : r.errors) {
                err.println("Error: " + e);
            }
            if (verbose) out.println("File loaded: " + r.clauses + " clauses loaded, " + r.errors.size() + " errors");
        // ISS-2025-0346 - a ':- halt.' directive in a consulted file ends the processor
        } catch (PrologException pe) {
            if (pe.isHalt()) {
                requestExit(pe.getExitCode());
                return;
            }
            err.println("Error during loading: " + pe.getMessage());
        } catch (Exception e) {
            err.println("Error during loading: " + e.getMessage());
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
            out.println("Compiled: " + jpcFile + " (" + f.length() + " bytes, " + elapsed + " ms)");
        } catch (IOException e) {
            err.println("Compilation error: " + e.getMessage());
        } catch (Exception e) {
            err.println("Error: " + e.getMessage());
        }
    }

    private void consultCompiledFile(String filename) {
        try {
            long start = System.currentTimeMillis();
            prolog.consultCompiled(filename);
            long elapsed = System.currentTimeMillis() - start;
            out.println("Loaded compiled file: " + filename + " (" + elapsed + " ms)");
        } catch (IOException e) {
            err.println("Load error: " + e.getMessage());
        } catch (Exception e) {
            err.println("Error: " + e.getMessage());
        }
    }
    // END_CHANGE: ISS-2025-0085

    /**
     * Save the current knowledge base to a file.
     */
    private void saveToFile(String filename) {
        try {
            out.println("Saving knowledge base to: " + filename);
            java.nio.file.Path path = java.nio.file.Paths.get(filename);
            StringBuilder content = new StringBuilder();
            content.append("% Knowledge base saved by JProlog CLI\n");
            content.append("% Date: " + java.time.LocalDateTime.now() + "\n\n");
            // ISS-2025-0570 - the saved file is the re-readable listing, in UTF-8
            content.append(prolog.getListingOutput());
            java.nio.file.Files.write(path, content.toString().getBytes(java.nio.charset.StandardCharsets.UTF_8));
            out.println("Knowledge base saved to: " + filename);
        } catch (Exception e) {
            err.println("Error saving file: " + e.getMessage());
        }
    }

    public static void main(String[] args) {
        // ISS-2025-0627: the exit status is the program's (halt/1, -g, -t, initialization main);
        // a plain end of input returns normally, as before
        PrologCLI cli = new PrologCLI(args);
        int status = cli.run();
        if (cli.exitRequested || status != 0) {
            System.out.flush();
            System.exit(status);
        }
    }
}
