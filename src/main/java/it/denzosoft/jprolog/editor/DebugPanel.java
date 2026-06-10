package it.denzosoft.jprolog.editor;

import it.denzosoft.jprolog.core.engine.DebugController;
import it.denzosoft.jprolog.core.engine.DebugEvent;
import it.denzosoft.jprolog.core.engine.DebugStackEntry;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import it.denzosoft.jprolog.editor.util.DialogUtils;

import javax.swing.*;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.text.*;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.*;
import java.util.List;

/**
 * Panel for debugging Prolog programs with full step execution.
 * Integrates with DebugController for real breakpoints, stepping,
 * call stack inspection, and variable monitoring.
 */
public class DebugPanel extends JPanel implements DebugController.DebugListener {

    private PrologIDE ide;
    private boolean debugMode = false;

    // Debug infrastructure
    // START_CHANGE: ISS-2025-0190 - Thread safety: volatile for cross-thread fields
    private volatile DebugController debugController;
    private volatile Thread debugThread;
    private volatile boolean queryRunning = false;
    // END_CHANGE: ISS-2025-0190

    // UI Components - Toolbar
    private JButton startDebugButton;
    private JButton stopDebugButton;
    private JButton stepIntoButton;
    private JButton stepOverButton;
    private JButton stepOutButton;
    private JButton continueButton;
    private JToggleButton traceToggleButton;
    // M11 CONTROL FLOW: extra flow controls
    private JButton runToCursorButton;
    private JButton restartButton;

    // UI Components - Query input
    private JTextField queryField;
    private JButton runQueryButton;

    // Debug information panels
    private JTree stackTraceTree;
    private DefaultTreeModel stackTraceModel;
    // M14 VARIABLES TREE: structure-aware variables view (replaces the flat JTable).
    private JTree variablesTree;
    private DefaultTreeModel variablesTreeModel;
    // Retained model instance for backward compatibility (public API preserved).
    private VariablesTableModel variablesModel;
    // M15 WATCH EXPRESSIONS: watch input + results list.
    private JTextField watchField;
    private DefaultListModel<String> watchesModel;
    private JList<String> watchesList;
    // Watch goals (expression text only, without results).
    private final List<String> watchExpressions = new ArrayList<>();
    private JList<String> breakpointsList;
    private DefaultListModel<String> breakpointsModel;
    private JTextPane traceOutputArea;
    private StyledDocument traceDocument;
    private Style traceCallStyle;
    private Style traceExitStyle;
    private Style traceFailStyle;
    private Style traceRedoStyle;
    private Style traceInfoStyle;
    private Style traceNormalStyle;
    private Style tracePauseStyle;

    // Breakpoints
    private List<String> breakpoints;
    /** Breakpoint list label ("p/2 [if ..]") -> the bare predicate indicator (ISS-2025-0333). */
    private final Map<String, String> breakpointSpecOf = new HashMap<>();

    // Query history
    private List<String> queryHistory = new ArrayList<>();
    private int historyIndex = -1;

    // M11 CONTROL FLOW: last debug query (for Restart) and a one-shot run-to-cursor breakpoint.
    private volatile String lastDebugQuery;
    private volatile String runToCursorBreakpoint;

    // Last paused event (for stack frame click -> variable update)
    // START_CHANGE: ISS-2025-0190 - Thread safety: volatile for cross-thread access
    private volatile DebugEvent lastPausedEvent;
    // END_CHANGE: ISS-2025-0190
    // Call stack entries from last pause (indexed parallel to tree nodes)
    // START_CHANGE: ISS-2025-0188 - Thread safety: volatile for cross-thread access
    private volatile List<DebugStackEntry> lastCallStack;
    // END_CHANGE: ISS-2025-0188

    // Trace output limit
    private static final int MAX_TRACE_LINES = 5000;
    private int traceLineCount = 0;

    // Colors
    private static final Color CALL_COLOR = new Color(0, 0, 180);    // Blue
    private static final Color EXIT_COLOR = new Color(0, 128, 0);    // Green
    private static final Color FAIL_COLOR = new Color(180, 0, 0);    // Red
    private static final Color REDO_COLOR = new Color(180, 128, 0);  // Orange
    private static final Color PAUSE_COLOR = new Color(0, 100, 0);   // Dark green for paused line
    private static final Color INFO_COLOR = new Color(128, 128, 128);// Gray

    public DebugPanel(PrologIDE ide) {
        this.ide = ide;
        this.breakpoints = new ArrayList<>();

        initializeComponents();
        layoutComponents();
        setupEventHandlers();
        setupTraceStyles();
    }

    // ===================== INITIALIZATION =====================

    private void initializeComponents() {
        // Toolbar buttons
        startDebugButton = createButton("Start Debug", "Start debugging session (F5)");
        stopDebugButton = createButton("Stop", "Stop debugging session");
        stopDebugButton.setEnabled(false);

        stepIntoButton = createButton("Step Into", "Step into next call (F7)");
        stepIntoButton.setEnabled(false);

        stepOverButton = createButton("Step Over", "Step over next call (F8)");
        stepOverButton.setEnabled(false);

        stepOutButton = createButton("Step Out", "Step out of current call (Shift+F8)");
        stepOutButton.setEnabled(false);

        continueButton = createButton("Continue", "Continue to next breakpoint (F9)");
        continueButton.setEnabled(false);

        // M11 CONTROL FLOW
        runToCursorButton = createButton("Run to Cursor",
            "Run until the predicate at the editor caret line is called (F4)");
        runToCursorButton.setEnabled(false);
        restartButton = createButton("Restart", "Re-run the last debug query with a fresh controller");
        restartButton.setEnabled(false);

        traceToggleButton = new JToggleButton("Trace");
        traceToggleButton.setToolTipText("Toggle trace output for all events");
        traceToggleButton.setSelected(true);
        traceToggleButton.setFocusPainted(false);

        // Query input
        queryField = new JTextField(30);
        queryField.setFont(new Font("Consolas", Font.PLAIN, 13));
        queryField.setToolTipText("Enter query to debug (e.g., parent(tom, X).)");

        runQueryButton = createButton("Debug Query", "Run query in debug mode");

        // Stack trace tree
        DefaultMutableTreeNode root = new DefaultMutableTreeNode("Call Stack");
        stackTraceModel = new DefaultTreeModel(root);
        stackTraceTree = new JTree(stackTraceModel);
        stackTraceTree.setRootVisible(true);
        stackTraceTree.setShowsRootHandles(true);
        stackTraceTree.setFont(new Font("Consolas", Font.PLAIN, 12));

        // M14 VARIABLES TREE: structure-aware variables view backed by the real Term.
        variablesModel = new VariablesTableModel(); // retained for backward compatibility
        DefaultMutableTreeNode varsRoot = new DefaultMutableTreeNode("Variables");
        variablesTreeModel = new DefaultTreeModel(varsRoot);
        variablesTree = new JTree(variablesTreeModel);
        variablesTree.setRootVisible(false);
        variablesTree.setShowsRootHandles(true);
        variablesTree.setFont(new Font("Consolas", Font.PLAIN, 12));

        // M15 WATCH EXPRESSIONS
        watchField = new JTextField(20);
        watchField.setFont(new Font("Consolas", Font.PLAIN, 12));
        watchField.setToolTipText("Enter a goal to watch (evaluated against the paused bindings)");
        watchesModel = new DefaultListModel<>();
        watchesList = new JList<>(watchesModel);
        watchesList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        watchesList.setFont(new Font("Consolas", Font.PLAIN, 12));

        // Breakpoints list
        breakpointsModel = new DefaultListModel<>();
        breakpointsList = new JList<>(breakpointsModel);
        breakpointsList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        breakpointsList.setFont(new Font("Consolas", Font.PLAIN, 12));

        // Trace output area (styled for colored output)
        traceOutputArea = new JTextPane();
        traceOutputArea.setEditable(false);
        traceOutputArea.setFont(new Font("Consolas", Font.PLAIN, 12));
        traceOutputArea.setBackground(Color.WHITE);
        traceDocument = traceOutputArea.getStyledDocument();
    }

    private JButton createButton(String text, String tooltip) {
        JButton button = new JButton(text);
        button.setToolTipText(tooltip);
        button.setFocusPainted(false);
        button.setMargin(new Insets(2, 8, 2, 8));
        return button;
    }

    private void setupTraceStyles() {
        traceCallStyle = traceOutputArea.addStyle("call", null);
        StyleConstants.setForeground(traceCallStyle, CALL_COLOR);
        StyleConstants.setFontFamily(traceCallStyle, "Consolas");
        StyleConstants.setFontSize(traceCallStyle, 12);

        traceExitStyle = traceOutputArea.addStyle("exit", null);
        StyleConstants.setForeground(traceExitStyle, EXIT_COLOR);
        StyleConstants.setFontFamily(traceExitStyle, "Consolas");
        StyleConstants.setFontSize(traceExitStyle, 12);

        traceFailStyle = traceOutputArea.addStyle("fail", null);
        StyleConstants.setForeground(traceFailStyle, FAIL_COLOR);
        StyleConstants.setFontFamily(traceFailStyle, "Consolas");
        StyleConstants.setFontSize(traceFailStyle, 12);

        traceRedoStyle = traceOutputArea.addStyle("redo", null);
        StyleConstants.setForeground(traceRedoStyle, REDO_COLOR);
        StyleConstants.setFontFamily(traceRedoStyle, "Consolas");
        StyleConstants.setFontSize(traceRedoStyle, 12);

        traceInfoStyle = traceOutputArea.addStyle("info", null);
        StyleConstants.setForeground(traceInfoStyle, INFO_COLOR);
        StyleConstants.setItalic(traceInfoStyle, true);
        StyleConstants.setFontFamily(traceInfoStyle, "Consolas");
        StyleConstants.setFontSize(traceInfoStyle, 12);

        traceNormalStyle = traceOutputArea.addStyle("normal", null);
        StyleConstants.setForeground(traceNormalStyle, Color.BLACK);
        StyleConstants.setFontFamily(traceNormalStyle, "Consolas");
        StyleConstants.setFontSize(traceNormalStyle, 12);

        tracePauseStyle = traceOutputArea.addStyle("pause", null);
        StyleConstants.setForeground(tracePauseStyle, PAUSE_COLOR);
        StyleConstants.setBold(tracePauseStyle, true);
        StyleConstants.setFontFamily(tracePauseStyle, "Consolas");
        StyleConstants.setFontSize(tracePauseStyle, 12);
    }

    private void layoutComponents() {
        setLayout(new BorderLayout(0, 2));

        // Top toolbar
        JPanel toolbarPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 4, 2));
        toolbarPanel.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, Color.LIGHT_GRAY));
        toolbarPanel.add(startDebugButton);
        toolbarPanel.add(stopDebugButton);
        toolbarPanel.add(createSeparator());
        toolbarPanel.add(stepIntoButton);
        toolbarPanel.add(stepOverButton);
        toolbarPanel.add(stepOutButton);
        toolbarPanel.add(continueButton);
        toolbarPanel.add(createSeparator());
        // M11 CONTROL FLOW
        toolbarPanel.add(runToCursorButton);
        toolbarPanel.add(restartButton);
        toolbarPanel.add(createSeparator());
        toolbarPanel.add(traceToggleButton);

        // Query input bar
        JPanel queryPanel = new JPanel(new BorderLayout(4, 0));
        queryPanel.setBorder(BorderFactory.createEmptyBorder(2, 4, 2, 4));
        JLabel queryLabel = new JLabel("Query: ");
        queryLabel.setFont(new Font("SansSerif", Font.BOLD, 12));
        queryPanel.add(queryLabel, BorderLayout.WEST);
        queryPanel.add(queryField, BorderLayout.CENTER);
        queryPanel.add(runQueryButton, BorderLayout.EAST);

        JPanel topPanel = new JPanel(new BorderLayout());
        topPanel.add(toolbarPanel, BorderLayout.NORTH);
        topPanel.add(queryPanel, BorderLayout.SOUTH);
        add(topPanel, BorderLayout.NORTH);

        // Main content: left (stack + vars) | right (breakpoints + trace)
        JSplitPane mainSplit = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
        mainSplit.setDividerLocation(400);

        // Left: stack trace above, variables tree in the middle, watches below
        JSplitPane leftSplit = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
        leftSplit.setDividerLocation(180);

        JPanel stackPanel = new JPanel(new BorderLayout());
        stackPanel.setBorder(BorderFactory.createTitledBorder("Call Stack"));
        stackPanel.add(new JScrollPane(stackTraceTree), BorderLayout.CENTER);

        // M14 VARIABLES TREE
        JPanel varsPanel = new JPanel(new BorderLayout());
        varsPanel.setBorder(BorderFactory.createTitledBorder("Variables"));
        varsPanel.add(new JScrollPane(variablesTree), BorderLayout.CENTER);

        // M15 WATCH EXPRESSIONS
        JPanel watchPanel = new JPanel(new BorderLayout());
        watchPanel.setBorder(BorderFactory.createTitledBorder("Watches"));
        watchPanel.add(new JScrollPane(watchesList), BorderLayout.CENTER);
        JPanel watchInput = new JPanel(new BorderLayout(4, 0));
        watchInput.setBorder(BorderFactory.createEmptyBorder(2, 2, 2, 2));
        JButton addWatch = createButton("+", "Add watch expression");
        JButton removeWatch = createButton("-", "Remove selected watch");
        JPanel watchInputButtons = new JPanel(new FlowLayout(FlowLayout.LEFT, 2, 0));
        watchInputButtons.add(addWatch);
        watchInputButtons.add(removeWatch);
        watchInput.add(watchField, BorderLayout.CENTER);
        watchInput.add(watchInputButtons, BorderLayout.EAST);
        watchPanel.add(watchInput, BorderLayout.SOUTH);
        addWatch.addActionListener(e -> addWatch());
        removeWatch.addActionListener(e -> removeSelectedWatch());
        watchField.addActionListener(e -> addWatch());

        // Variables tree above, watches below (nested split inside the left column).
        JSplitPane varsWatchSplit = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
        varsWatchSplit.setDividerLocation(180);
        varsWatchSplit.setTopComponent(varsPanel);
        varsWatchSplit.setBottomComponent(watchPanel);

        leftSplit.setTopComponent(stackPanel);
        leftSplit.setBottomComponent(varsWatchSplit);

        // Right: breakpoints above, trace output below
        JSplitPane rightSplit = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
        rightSplit.setDividerLocation(130);

        JPanel bpPanel = new JPanel(new BorderLayout());
        bpPanel.setBorder(BorderFactory.createTitledBorder("Breakpoints"));
        bpPanel.add(new JScrollPane(breakpointsList), BorderLayout.CENTER);

        JPanel bpButtons = new JPanel(new FlowLayout(FlowLayout.LEFT, 4, 2));
        JButton addBp = createButton("Add", "Add breakpoint (predicate/arity)");
        JButton removeBp = createButton("Remove", "Remove selected breakpoint");
        JButton clearBp = createButton("Clear All", "Remove all breakpoints");
        bpButtons.add(addBp);
        bpButtons.add(removeBp);
        bpButtons.add(clearBp);
        bpPanel.add(bpButtons, BorderLayout.SOUTH);

        addBp.addActionListener(e -> addBreakpoint());
        removeBp.addActionListener(e -> removeSelectedBreakpoint());
        clearBp.addActionListener(e -> clearAllBreakpoints());

        JPanel tracePanel = new JPanel(new BorderLayout());
        tracePanel.setBorder(BorderFactory.createTitledBorder("Trace Output"));
        JScrollPane traceScroll = new JScrollPane(traceOutputArea);
        tracePanel.add(traceScroll, BorderLayout.CENTER);

        JPanel traceButtons = new JPanel(new FlowLayout(FlowLayout.LEFT, 4, 2));
        JButton clearTrace = createButton("Clear", "Clear trace output");
        clearTrace.addActionListener(e -> clearTraceOutput());
        traceButtons.add(clearTrace);
        tracePanel.add(traceButtons, BorderLayout.SOUTH);

        rightSplit.setTopComponent(bpPanel);
        rightSplit.setBottomComponent(tracePanel);

        mainSplit.setLeftComponent(leftSplit);
        mainSplit.setRightComponent(rightSplit);

        add(mainSplit, BorderLayout.CENTER);
    }

    private JSeparator createSeparator() {
        JSeparator sep = new JSeparator(SwingConstants.VERTICAL);
        sep.setPreferredSize(new Dimension(2, 24));
        return sep;
    }

    private void setupEventHandlers() {
        startDebugButton.addActionListener(e -> startDebugging());
        stopDebugButton.addActionListener(e -> stopDebugging());
        stepIntoButton.addActionListener(e -> stepInto());
        stepOverButton.addActionListener(e -> stepOver());
        stepOutButton.addActionListener(e -> stepOut());
        continueButton.addActionListener(e -> continueExecution());
        // M11 CONTROL FLOW
        runToCursorButton.addActionListener(e -> runToCursor());
        restartButton.addActionListener(e -> restartDebug());
        // (stepping keyboard shortcuts are registered in setupSteppingShortcuts())
        runQueryButton.addActionListener(e -> debugQuery());
        queryField.addActionListener(e -> debugQuery());

        // ISS-2025-0322: real stepping shortcuts (the buttons were tooltipped F7/F8/Shift+F8/F9 but
        // only one key was wired). Bind them window-wide; each fires only when its button is enabled.
        setupSteppingShortcuts();

        // Query field history navigation
        queryField.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_UP && !queryHistory.isEmpty()) {
                    e.consume();
                    if (historyIndex < 0) historyIndex = queryHistory.size();
                    historyIndex = Math.max(0, historyIndex - 1);
                    queryField.setText(queryHistory.get(historyIndex));
                } else if (e.getKeyCode() == KeyEvent.VK_DOWN && !queryHistory.isEmpty()) {
                    e.consume();
                    historyIndex = Math.min(queryHistory.size() - 1, historyIndex + 1);
                    queryField.setText(queryHistory.get(historyIndex));
                }
            }
        });

        traceToggleButton.addActionListener(e -> {
            if (debugController != null) {
                debugController.setTraceEnabled(traceToggleButton.isSelected());
            }
        });

        // Stack tree selection: update variables for selected frame
        stackTraceTree.addTreeSelectionListener((TreeSelectionEvent e) -> {
            TreePath path = e.getNewLeadSelectionPath();
            if (path != null && lastCallStack != null) {
                DefaultMutableTreeNode node = (DefaultMutableTreeNode) path.getLastPathComponent();
                DefaultMutableTreeNode root = (DefaultMutableTreeNode) stackTraceModel.getRoot();
                int childIndex = root.getIndex(node);
                if (childIndex >= 0 && childIndex < lastCallStack.size()) {
                    // Reverse index: tree shows deepest first
                    int stackIndex = lastCallStack.size() - 1 - childIndex;
                    if (stackIndex >= 0 && stackIndex < lastCallStack.size()) {
                        updateVariablesDisplay(lastCallStack.get(stackIndex).getBindingsSnapshot());
                    }
                }
            }
        });
    }

    // ===================== DEBUG SESSION MANAGEMENT =====================

    /**
     * Starts a debug session - creates DebugController and wires it up.
     */
    public void startDebugging() {
        debugMode = true;

        // Create fresh debug controller
        debugController = new DebugController();
        debugController.setListener(this);
        debugController.setTraceEnabled(traceToggleButton.isSelected());
        // ISS-2025-0333: evaluate breakpoint conditions via a clean sub-solve (debugger detached, so no
        // re-entrant pausing). Runs on the solver thread during the pause decision.
        debugController.setConditionEvaluator((cond, bindings) -> {
            Prolog engine = ide.getPrologEngine();
            if (engine == null || engine.getQuerySolver() == null) return false;
            String goal = buildWatchGoal(cond, bindings);
            DebugController saved = engine.getQuerySolver().getDebugController();
            try {
                engine.getQuerySolver().setDebugController(null);
                List<Map<String, Term>> sols = engine.solveLegacy(goal);
                return sols != null && !sols.isEmpty();
            } catch (RuntimeException e) {
                return false;
            } finally {
                engine.getQuerySolver().setDebugController(saved);
            }
        });

        // Sync breakpoints
        for (String bp : breakpoints) {
            debugController.addBreakpoint(bp);
        }

        // Wire to query solver
        Prolog engine = ide.getPrologEngine();
        if (engine != null) {
            engine.getQuerySolver().setDebugController(debugController);
        }

        updateButtonStates(true, false);
        appendInfo("Debug session started. Enter a query and click 'Debug Query'.\n");
        updateStatusBar("Debug: Active");

        if (ide != null) {
            ide.setDebugMode(true);
        }
    }

    /**
     * Stop the debug session.
     */
    private void stopDebugging() {
        if (debugController != null) {
            debugController.stop();
        }

        // Wait for debug thread to finish
        if (debugThread != null && debugThread.isAlive()) {
            debugThread.interrupt();
            try {
                debugThread.join(2000);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }

        // Unwire from solver
        Prolog engine = ide.getPrologEngine();
        if (engine != null) {
            engine.getQuerySolver().setDebugController(null);
        }

        debugMode = false;
        debugController = null;
        debugThread = null;
        queryRunning = false;
        lastPausedEvent = null;
        lastCallStack = null;

        clearCallStack();
        clearVariables();
        clearEditorDebugHighlight();
        updateButtonStates(false, false);
        updateStatusBar("Ready");

        appendInfo("Debug session stopped.\n");

        if (ide != null) {
            ide.setDebugMode(false);
        }
    }

    /**
     * Run a query in debug mode on a background thread.
     */
    private void debugQuery() {
        String queryText = queryField.getText().trim();
        if (queryText.isEmpty()) return;

        // Don't start a new query while one is running and paused
        if (queryRunning) {
            appendInfo("A query is already running. Stop it first.\n");
            return;
        }

        if (!debugMode) {
            startDebugging();
        }

        // Remove trailing period if present
        if (queryText.endsWith(".")) {
            queryText = queryText.substring(0, queryText.length() - 1).trim();
        }
        if (queryText.isEmpty()) return;

        // Save to history
        if (queryHistory.isEmpty() || !queryHistory.get(queryHistory.size() - 1).equals(queryText)) {
            queryHistory.add(queryText);
        }
        historyIndex = queryHistory.size();

        // Reset controller for new query
        debugController.reset();
        debugController.setTraceEnabled(traceToggleButton.isSelected());
        for (String bp : breakpoints) {
            debugController.addBreakpoint(bp);
        }

        final String query = queryText;
        // M11 CONTROL FLOW: remember the last query so Restart can re-issue it.
        lastDebugQuery = query;
        appendInfo("\n--- Debug query: " + query + " ---\n");
        traceLineCount = 0;

        updateButtonStates(true, false);
        clearEditorDebugHighlight();
        queryRunning = true;

        debugThread = new Thread(() -> {
            Prolog engine = ide.getPrologEngine();
            if (engine == null) {
                SwingUtilities.invokeLater(() -> appendError("No Prolog engine available.\n"));
                return;
            }

            try {
                // Capture side-effect output for this debug thread (thread-local, not global)
                ByteArrayOutputStream baos = new ByteArrayOutputStream();
                PrintStream captureOut = new PrintStream(baos);

                List<Map<String, Term>> solutions;
                String capturedOutput;

                try {
                    // ISS-2025-0331: the default v2 engine now carries the four-port DebugController
                    // hooks, so debug on it (consistent with normal execution). Capture output per-thread.
                    it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(captureOut);
                    solutions = engine.solve(query);   // String -> v2 engine (with debug hooks)
                    captureOut.flush();
                    capturedOutput = baos.toString();
                } finally {
                    it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(null);
                }

                final String output = capturedOutput;
                final List<Map<String, Term>> finalSolutions = solutions;

                SwingUtilities.invokeLater(() -> {
                    if (!output.isEmpty()) {
                        appendInfo("Output: " + output);
                    }
                    displaySolutions(finalSolutions);
                    appendInfo("--- Query finished ---\n");
                });

            } catch (DebugController.DebugStopException e) {
                SwingUtilities.invokeLater(() ->
                    appendInfo("Query execution stopped by user.\n"));
            // START_CHANGE: ISS-2025-0346 - halt ends the debug session gracefully (no JVM exit)
            } catch (it.denzosoft.jprolog.core.exceptions.PrologException pe) {
                if (pe.isHalt()) {
                    final int exitCode = pe.getExitCode();
                    SwingUtilities.invokeLater(() ->
                        appendInfo("halt: debug session ended (exit code " + exitCode + ").\n"));
                } else {
                    SwingUtilities.invokeLater(() ->
                        appendError("Error: " + pe.getMessage() + "\n"));
                }
            // END_CHANGE: ISS-2025-0346
            } catch (Exception e) {
                SwingUtilities.invokeLater(() ->
                    appendError("Error: " + e.getMessage() + "\n"));
            } finally {
                SwingUtilities.invokeLater(() -> {
                    queryRunning = false;
                    lastPausedEvent = null;
                    lastCallStack = null;
                    clearCallStack();
                    clearVariables();
                    clearEditorDebugHighlight();
                    updateButtonStates(debugMode, false);
                    updateStatusBar(debugMode ? "Debug: Ready" : "Ready");
                });
            }
        }, "JProlog-Debug");

        debugThread.setDaemon(true);
        debugThread.start();
    }

    /**
     * Display query solutions in the trace output.
     */
    private void displaySolutions(List<Map<String, Term>> solutions) {
        if (solutions.isEmpty()) {
            appendError("false.\n");
        } else if (solutions.size() == 1 && solutions.get(0).isEmpty()) {
            appendTrace("true.\n", traceExitStyle);
        } else {
            for (int i = 0; i < solutions.size(); i++) {
                Map<String, Term> sol = solutions.get(i);
                StringBuilder sb = new StringBuilder();
                boolean first = true;
                for (Map.Entry<String, Term> entry : sol.entrySet()) {
                    if (entry.getKey().startsWith("_R") || entry.getKey().startsWith("_")) continue;
                    if (!first) sb.append(",\n  ");
                    sb.append(entry.getKey()).append(" = ").append(entry.getValue());
                    first = false;
                }
                if (sb.length() > 0) {
                    appendTrace(sb.toString(), traceExitStyle);
                    appendTrace(i < solutions.size() - 1 ? " ;\n" : ".\n", traceNormalStyle);
                }
            }
        }
    }

    // ===================== STEP CONTROLS =====================

    /** Bind F7 / F8 / Shift+F8 / F9 (and Esc) window-wide to the debug step actions (ISS-2025-0322).
     *  Each action delegates to the button handler, which already no-ops unless paused. */
    private void setupSteppingShortcuts() {
        InputMap im = getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        ActionMap am = getActionMap();
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_F7, 0), "dbg-step-into");
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_F8, 0), "dbg-step-over");
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_F8, InputEvent.SHIFT_DOWN_MASK), "dbg-step-out");
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_F9, 0), "dbg-continue");
        // M11 CONTROL FLOW: F4 = run to cursor
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_F4, 0), "dbg-run-to-cursor");
        am.put("dbg-run-to-cursor", new AbstractAction() {
            @Override public void actionPerformed(ActionEvent e) { if (runToCursorButton.isEnabled()) runToCursor(); }
        });
        am.put("dbg-step-into", new AbstractAction() {
            @Override public void actionPerformed(ActionEvent e) { if (stepIntoButton.isEnabled()) stepInto(); }
        });
        am.put("dbg-step-over", new AbstractAction() {
            @Override public void actionPerformed(ActionEvent e) { if (stepOverButton.isEnabled()) stepOver(); }
        });
        am.put("dbg-step-out", new AbstractAction() {
            @Override public void actionPerformed(ActionEvent e) { if (stepOutButton.isEnabled()) stepOut(); }
        });
        am.put("dbg-continue", new AbstractAction() {
            @Override public void actionPerformed(ActionEvent e) { if (continueButton.isEnabled()) continueExecution(); }
        });
    }

    private void stepInto() {
        if (debugController != null && debugController.isPaused()) {
            clearEditorDebugHighlight();
            updateButtonStates(true, false);
            updateStatusBar("Debug: Stepping...");
            debugController.resumeWithAction(DebugEvent.Action.STEP_INTO);
        }
    }

    private void stepOver() {
        if (debugController != null && debugController.isPaused()) {
            clearEditorDebugHighlight();
            updateButtonStates(true, false);
            updateStatusBar("Debug: Stepping over...");
            debugController.resumeWithAction(DebugEvent.Action.STEP_OVER);
        }
    }

    private void stepOut() {
        if (debugController != null && debugController.isPaused()) {
            clearEditorDebugHighlight();
            updateButtonStates(true, false);
            updateStatusBar("Debug: Stepping out...");
            debugController.resumeWithAction(DebugEvent.Action.STEP_OUT);
        }
    }

    private void continueExecution() {
        if (debugController != null && debugController.isPaused()) {
            clearEditorDebugHighlight();
            updateButtonStates(true, false);
            updateStatusBar("Debug: Running...");
            debugController.resumeWithAction(DebugEvent.Action.CONTINUE);
        }
    }

    // ===================== M11 CONTROL FLOW =====================

    /**
     * Run to Cursor: set a one-shot breakpoint on the predicate at the editor caret
     * line, then continue. When that breakpoint is next hit the one-shot is cleared
     * again so it does not persist.
     */
    private void runToCursor() {
        if (debugController == null || !debugController.isPaused()) {
            return;
        }
        if (ide == null) {
            appendInfo("Run to Cursor: no IDE context.\n");
            return;
        }
        FileEditor editor = ide.getEditorTabs().getCurrentEditor();
        Prolog engine = ide.getPrologEngine();
        if (editor == null || engine == null) {
            appendInfo("Run to Cursor: no active editor or engine.\n");
            return;
        }
        int line = caretLine(editor);
        String indicator = engine.getPredicateIndicatorAtLine(line);
        if (indicator == null) {
            appendInfo("Run to Cursor: no predicate found at line " + line + ".\n");
            return;
        }
        // Install a one-shot breakpoint (remembered so we can remove it once hit).
        runToCursorBreakpoint = indicator;
        debugController.addBreakpoint(indicator);
        appendInfo("Run to Cursor: " + indicator + " (line " + line + ")\n");
        // Now continue execution until that breakpoint (or an existing one) is hit.
        continueExecution();
    }

    /**
     * Compute the 1-based caret line of the given editor from its text pane.
     */
    private int caretLine(FileEditor editor) {
        JTextPane pane = editor.getTextPane();
        if (pane == null) return 1;
        int caret = pane.getCaretPosition();
        javax.swing.text.Element root = pane.getDocument().getDefaultRootElement();
        return root.getElementIndex(caret) + 1; // element index is 0-based
    }

    /**
     * If a one-shot Run-to-Cursor breakpoint is set and this pause is at that
     * predicate, remove the breakpoint so it does not persist beyond the single hit.
     * Breakpoints the user added explicitly (also present in {@code breakpoints}) are
     * left untouched.
     */
    private void clearRunToCursorIfHit(DebugEvent event) {
        String oneShot = runToCursorBreakpoint;
        if (oneShot == null || debugController == null) {
            return;
        }
        Term goal = event.getGoal();
        if (goal == null) {
            return;
        }
        String name = goal.getName();
        int arity = (goal.getArguments() != null) ? goal.getArguments().size() : 0;
        String indicator = (name != null) ? (name + "/" + arity) : null;
        if (oneShot.equals(indicator)) {
            runToCursorBreakpoint = null;
            // Only remove if it was not also a user breakpoint.
            if (!breakpoints.contains(oneShot)) {
                debugController.removeBreakpoint(oneShot);
            }
        }
    }

    /**
     * Restart: re-issue the last debug query with a fresh controller. Stops the
     * current run (if any), recreates the DebugController via startDebugging(),
     * and replays the remembered query text.
     */
    private void restartDebug() {
        final String query = lastDebugQuery;
        if (query == null || query.isEmpty()) {
            appendInfo("Restart: no previous query to re-run.\n");
            return;
        }
        // Tear down the current session (also resumes any paused thread to let it exit).
        if (debugController != null) {
            debugController.stop();
        }
        if (debugThread != null && debugThread.isAlive()) {
            debugThread.interrupt();
            try {
                debugThread.join(2000);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }
        debugThread = null;
        queryRunning = false;
        runToCursorBreakpoint = null;

        // Fresh controller wired by startDebugging(); clear residual UI state first.
        debugMode = false;
        debugController = null;
        clearCallStack();
        clearVariables();
        clearEditorDebugHighlight();
        appendInfo("\n--- Restarting debug query ---\n");

        startDebugging();
        queryField.setText(query);
        debugQuery();
    }

    // ===================== M15 WATCH EXPRESSIONS =====================

    /** Add the current watch-field expression to the watch list. */
    private void addWatch() {
        String expr = watchField.getText().trim();
        if (expr.endsWith(".")) {
            expr = expr.substring(0, expr.length() - 1).trim();
        }
        if (expr.isEmpty() || watchExpressions.contains(expr)) {
            return;
        }
        watchExpressions.add(expr);
        watchesModel.addElement(expr + "  =  ?");
        watchField.setText("");
        // Evaluate immediately if we are currently paused.
        if (lastPausedEvent != null) {
            evaluateWatches(lastPausedEvent.getBindings());
        }
    }

    /** Remove the selected watch expression. */
    private void removeSelectedWatch() {
        int idx = watchesList.getSelectedIndex();
        if (idx >= 0 && idx < watchExpressions.size()) {
            watchExpressions.remove(idx);
            watchesModel.removeElementAt(idx);
        }
    }

    /**
     * Evaluate each watch goal against the supplied paused bindings and display
     * the result next to it. The goal is solved through the legacy engine
     * (debug-safe); when the goal shares variables with the current bindings those
     * bindings are substituted into the goal text before solving.
     */
    private void evaluateWatches(Map<String, Term> bindings) {
        if (watchExpressions.isEmpty()) {
            return;
        }
        Prolog engine = (ide != null) ? ide.getPrologEngine() : null;
        for (int i = 0; i < watchExpressions.size(); i++) {
            String expr = watchExpressions.get(i);
            String result;
            if (engine == null) {
                result = "<no engine>";
            } else {
                result = evaluateWatch(engine, expr, bindings);
            }
            if (i < watchesModel.size()) {
                watchesModel.set(i, expr + "  =  " + result);
            } else {
                watchesModel.addElement(expr + "  =  " + result);
            }
        }
    }

    /**
     * Evaluate a single watch goal with the current paused bindings substituted in.
     * Returns "true", "false", a binding string for the first solution's fresh
     * variables, or an error marker.
     */
    private String evaluateWatch(Prolog engine, String expr, Map<String, Term> bindings) {
        // Substitute known bindings as a prefix of (Var = Value, ...) conjunctions so
        // the watch goal sees the paused frame's values.
        String goal = buildWatchGoal(expr, bindings);
        // Detach the debug controller for this nested solve: we are on the EDT while the
        // solver thread is blocked at a pause; re-entering the debugger here would attempt
        // to pause again and deadlock. Restore it afterwards.
        DebugController saved = (engine.getQuerySolver() != null)
            ? engine.getQuerySolver().getDebugController() : null;
        try {
            if (engine.getQuerySolver() != null) {
                engine.getQuerySolver().setDebugController(null);
            }
            List<Map<String, Term>> sols = engine.solveLegacy(goal);
            if (sols == null || sols.isEmpty()) {
                return "false";
            }
            // Report bindings of variables that appear in the watch expression itself.
            Map<String, Term> first = sols.get(0);
            StringBuilder sb = new StringBuilder();
            for (Map.Entry<String, Term> e : first.entrySet()) {
                String key = e.getKey();
                if (key.startsWith("_")) continue;
                // Only show variables the user actually wrote in the watch expression.
                if (!mentionsVariable(expr, key)) continue;
                if (sb.length() > 0) sb.append(", ");
                sb.append(key).append(" = ").append(e.getValue());
            }
            if (sb.length() == 0) {
                return "true" + (sols.size() > 1 ? " (" + sols.size() + " solutions)" : "");
            }
            return sb.toString() + (sols.size() > 1 ? "  (+" + (sols.size() - 1) + " more)" : "");
        } catch (Exception ex) {
            return "<error: " + ex.getMessage() + ">";
        } finally {
            // Re-attach the debug controller so stepping/continue still work.
            if (engine.getQuerySolver() != null) {
                engine.getQuerySolver().setDebugController(saved);
            }
        }
    }

    /**
     * Prefix the watch goal with the paused frame's user bindings so the goal is
     * evaluated in that context, e.g. {@code X = 3, Y = foo, <expr>}.
     */
    private String buildWatchGoal(String expr, Map<String, Term> bindings) {
        if (bindings == null || bindings.isEmpty()) {
            return expr;
        }
        StringBuilder prefix = new StringBuilder();
        for (Map.Entry<String, Term> b : bindings.entrySet()) {
            String name = b.getKey();
            if (name.startsWith("_")) continue;
            // Only substitute bindings whose variable the watch expression references.
            if (!mentionsVariable(expr, name)) continue;
            Term resolved = deepResolve(b.getValue(), bindings);
            // Skip still-unbound variables — leaving them free lets the goal bind them.
            if (resolved instanceof Variable) continue;
            if (prefix.length() > 0) prefix.append(", ");
            prefix.append(name).append(" = ").append(termToSource(resolved));
        }
        if (prefix.length() == 0) {
            return expr;
        }
        return prefix.toString() + ", " + expr;
    }

    /**
     * Render a term as re-parseable Prolog source. The default toString of terms in
     * this engine is ISO-compatible for the structures we care about (atoms, numbers,
     * lists, compounds), so it round-trips through the parser.
     */
    private String termToSource(Term t) {
        return (t == null) ? "_" : t.toString();
    }

    /**
     * Heuristic check: does the watch expression mention variable {@code name} as a
     * whole token (so we don't substitute on accidental substring matches)?
     */
    private boolean mentionsVariable(String expr, String name) {
        int idx = expr.indexOf(name);
        while (idx >= 0) {
            boolean leftOk = (idx == 0) || !isIdentChar(expr.charAt(idx - 1));
            int after = idx + name.length();
            boolean rightOk = (after >= expr.length()) || !isIdentChar(expr.charAt(after));
            if (leftOk && rightOk) return true;
            idx = expr.indexOf(name, idx + 1);
        }
        return false;
    }

    private boolean isIdentChar(char c) {
        return Character.isLetterOrDigit(c) || c == '_';
    }

    // ===================== DebugController.DebugListener =====================

    @Override
    public void onDebugPaused(DebugEvent event) {
        // Called on solver thread — post everything to EDT
        SwingUtilities.invokeLater(() -> {
            lastPausedEvent = event;
            lastCallStack = event.getCallStack();

            // M11 CONTROL FLOW: clear a one-shot Run-to-Cursor breakpoint once its
            // predicate is reached (so it does not linger as a permanent breakpoint).
            clearRunToCursorIfHit(event);

            // Update call stack display
            updateCallStackDisplay(event.getCallStack());

            // Update variables display with current frame bindings
            updateVariablesDisplay(event.getBindings());

            // M15 WATCH EXPRESSIONS: re-evaluate each watch against the paused bindings.
            evaluateWatches(event.getBindings());

            // Enable step buttons
            updateButtonStates(true, true);

            // Show paused trace line (highlighted)
            appendTraceLine(event, true);

            // Update status bar with current goal
            String goalStr = event.getGoal().toString();
            if (goalStr.length() > 60) goalStr = goalStr.substring(0, 57) + "...";
            updateStatusBar("Debug: PAUSED at [" + event.getDepth() + "] "
                + event.getPort().toString().toUpperCase() + " " + goalStr);

            // Highlight current execution line in editor (if applicable)
            highlightEditorDebugLine(event);
        });
    }

    @Override
    public void onTraceEvent(DebugEvent event) {
        // Called on solver thread for every port — post to EDT
        SwingUtilities.invokeLater(() -> appendTraceLine(event, false));
    }

    @Override
    public void onDebugFinished() {
        SwingUtilities.invokeLater(() -> {
            appendInfo("Debug execution finished.\n");
            queryRunning = false;
            lastPausedEvent = null;
            lastCallStack = null;
            clearCallStack();
            clearVariables();
            clearEditorDebugHighlight();
            updateButtonStates(debugMode, false);
            updateStatusBar(debugMode ? "Debug: Ready" : "Ready");
        });
    }

    // ===================== EDITOR INTEGRATION =====================

    /**
     * Try to highlight the current debug line in the active editor.
     * Uses the goal name to search for the predicate definition.
     */
    private void highlightEditorDebugLine(DebugEvent event) {
        if (ide == null) return;
        FileEditor editor = ide.getEditorTabs().getCurrentEditor();
        if (editor == null) return;

        Term goal = event.getGoal();
        if (goal == null || goal.getName() == null) return;

        // Try to find the predicate definition line in the editor
        String text = editor.getText();
        if (text == null || text.isEmpty()) return;

        String goalName = goal.getName();
        int arity = (goal.getArguments() != null) ? goal.getArguments().size() : 0;

        // Search for predicate head matching goalName/arity
        String[] lines = text.split("\n");
        for (int i = 0; i < lines.length; i++) {
            String line = lines[i].trim();
            if (line.isEmpty() || line.startsWith("%")) continue;

            // Check if line starts with the predicate name
            if (arity == 0) {
                // Fact with no args: "name." or "name :-"
                if (line.equals(goalName + ".") || line.startsWith(goalName + " :-")) {
                    editor.setDebugHighlightLine(i + 1);
                    return;
                }
            } else {
                // Predicate with args: "name("
                if (line.startsWith(goalName + "(")) {
                    // Verify arity by counting top-level commas
                    int parenDepth = 0;
                    int commas = 0;
                    boolean inHead = true;
                    for (int j = goalName.length(); j < line.length() && inHead; j++) {
                        char c = line.charAt(j);
                        if (c == '(') parenDepth++;
                        else if (c == ')') {
                            parenDepth--;
                            if (parenDepth == 0) inHead = false;
                        }
                        else if (c == ',' && parenDepth == 1) commas++;
                    }
                    if (commas + 1 == arity) {
                        editor.setDebugHighlightLine(i + 1);
                        return;
                    }
                }
            }
        }
    }

    /**
     * Clear editor debug highlighting.
     */
    private void clearEditorDebugHighlight() {
        if (ide == null) return;
        FileEditor editor = ide.getEditorTabs().getCurrentEditor();
        if (editor != null) {
            editor.clearDebugHighlighting();
        }
    }

    // ===================== BREAKPOINT MANAGEMENT =====================

    private void addBreakpoint() {
        // ISS-2025-0333: predicate + optional condition goal + optional ignore (hit) count.
        JTextField predField = new JTextField();
        JTextField condField = new JTextField();
        JTextField ignoreField = new JTextField("0");
        JPanel panel = new JPanel(new GridLayout(0, 1, 2, 2));
        panel.add(new JLabel("Predicate (name/arity, e.g. parent/2):"));
        panel.add(predField);
        panel.add(new JLabel("Condition goal (optional; pauses only if it succeeds):"));
        panel.add(condField);
        panel.add(new JLabel("Ignore count (skip the first N hits):"));
        panel.add(ignoreField);
        int res = JOptionPane.showConfirmDialog(this, panel, "Add Breakpoint",
            JOptionPane.OK_CANCEL_OPTION, JOptionPane.PLAIN_MESSAGE);
        if (res != JOptionPane.OK_OPTION) return;

        String bp = predField.getText().trim();
        if (bp.isEmpty() || breakpoints.contains(bp)) return;
        String cond = condField.getText().trim();
        int ignore = 0;
        try { ignore = Integer.parseInt(ignoreField.getText().trim()); } catch (NumberFormatException ignored) {}

        breakpoints.add(bp);
        String label = bp
            + (cond.isEmpty() ? "" : "  [if " + cond + "]")
            + (ignore > 0 ? "  [skip " + ignore + "]" : "");
        breakpointsModel.addElement(label);
        breakpointSpecOf.put(label, bp);
        if (debugController != null) {
            debugController.addBreakpoint(bp, null, cond.isEmpty() ? null : cond, ignore);
        }
        appendInfo("Breakpoint added: " + label + "\n");
    }

    private void removeSelectedBreakpoint() {
        int idx = breakpointsList.getSelectedIndex();
        if (idx != -1) {
            String label = breakpointsModel.getElementAt(idx);
            String bp = breakpointSpecOf.getOrDefault(label, label);   // label may carry [if ..]/[skip ..]
            breakpoints.remove(bp);
            breakpointSpecOf.remove(label);
            breakpointsModel.removeElementAt(idx);
            if (debugController != null) {
                debugController.removeBreakpoint(bp);
            }
            appendInfo("Breakpoint removed: " + bp + "\n");
        }
    }

    private void clearAllBreakpoints() {
        breakpoints.clear();
        breakpointsModel.clear();
        if (debugController != null) {
            debugController.clearBreakpoints();
        }
        appendInfo("All breakpoints cleared.\n");
    }

    // ===================== DISPLAY UPDATES =====================

    private void updateCallStackDisplay(List<DebugStackEntry> stack) {
        DefaultMutableTreeNode root = new DefaultMutableTreeNode(
            "Call Stack (" + stack.size() + " frames)");

        // Display deepest frame first (top of stack)
        for (int i = stack.size() - 1; i >= 0; i--) {
            DebugStackEntry entry = stack.get(i);
            DefaultMutableTreeNode node = new DefaultMutableTreeNode(entry.toString());

            // Add user-visible variable bindings as children
            Map<String, Term> bindings = entry.getBindingsSnapshot();
            for (Map.Entry<String, Term> binding : bindings.entrySet()) {
                if (!binding.getKey().startsWith("_R") && !binding.getKey().startsWith("_")) {
                    Term resolved = deepResolve(binding.getValue(), bindings);
                    node.add(new DefaultMutableTreeNode(
                        binding.getKey() + " = " + resolved));
                }
            }
            root.add(node);
        }

        stackTraceModel.setRoot(root);
        stackTraceTree.expandRow(0);
        if (stackTraceTree.getRowCount() > 1) {
            stackTraceTree.expandRow(1);
        }
    }

    // M14 VARIABLES TREE: repopulate the structure-aware tree from a frame's bindings.
    private void updateVariablesDisplay(Map<String, Term> bindings) {
        variablesModel.clear();
        DefaultMutableTreeNode root = new DefaultMutableTreeNode("Variables");

        if (bindings != null) {
            // Sort variable names for consistent display
            List<String> sortedNames = new ArrayList<>();
            for (String name : bindings.keySet()) {
                if (!name.startsWith("_R") && !name.startsWith("_")) {
                    sortedNames.add(name);
                }
            }
            Collections.sort(sortedNames);

            for (String name : sortedNames) {
                Term value = bindings.get(name);
                Term resolved = deepResolve(value, bindings);
                // Keep the flat model in sync (backward-compatible public API).
                variablesModel.addVariable(name, resolved.toString());
                // Build the expandable tree node backed by the real term.
                root.add(VariablesTableModel.buildTreeNode(name, resolved));
            }
        }

        variablesTreeModel.setRoot(root);
        // Expand the top-level bindings so they are visible at a glance.
        for (int i = 0; i < variablesTree.getRowCount(); i++) {
            variablesTree.expandRow(i);
        }
    }

    /**
     * Deep-resolve a term through the binding map, handling Variables and CompoundTerms.
     */
    private Term deepResolve(Term value, Map<String, Term> bindings) {
        if (value == null) return value;
        // Use the built-in resolveBindings which handles Variable chains and CompoundTerms
        Term resolved = value.resolveBindings(bindings);
        // Double resolve to catch transitive chains
        if (resolved instanceof Variable) {
            Term second = resolved.resolveBindings(bindings);
            if (second != resolved) return second;
        }
        return resolved;
    }

    private void clearCallStack() {
        DefaultMutableTreeNode root = new DefaultMutableTreeNode("Call Stack");
        stackTraceModel.setRoot(root);
    }

    private void clearVariables() {
        variablesModel.clear();
        // M14 VARIABLES TREE: clear the tree as well.
        if (variablesTreeModel != null) {
            variablesTreeModel.setRoot(new DefaultMutableTreeNode("Variables"));
        }
    }

    private void appendTraceLine(DebugEvent event, boolean isPause) {
        // Enforce trace output limit
        if (traceLineCount >= MAX_TRACE_LINES) {
            if (traceLineCount == MAX_TRACE_LINES) {
                appendTrace("... trace output limit reached (" + MAX_TRACE_LINES + " lines). Clear to continue.\n", traceInfoStyle);
                traceLineCount++;
            }
            return;
        }
        traceLineCount++;

        Style style;
        switch (event.getPort()) {
            case CALL: style = traceCallStyle; break;
            case EXIT: style = traceExitStyle; break;
            case FAIL: style = traceFailStyle; break;
            case REDO: style = traceRedoStyle; break;
            default: style = traceNormalStyle; break;
        }

        if (isPause) {
            // Paused line: show with bold green prefix
            appendTrace(" >> ", tracePauseStyle);
            appendTrace(event.formatTraceLine() + "\n", style);
        } else {
            appendTrace("    " + event.formatTraceLine() + "\n", style);
        }
    }

    // ===================== TEXT OUTPUT HELPERS =====================

    private void appendTrace(String text, Style style) {
        try {
            traceDocument.insertString(traceDocument.getLength(), text, style);
            traceOutputArea.setCaretPosition(traceDocument.getLength());
        } catch (BadLocationException e) {
            // ignore
        }
    }

    private void appendInfo(String text) {
        appendTrace(text, traceInfoStyle);
    }

    private void appendError(String text) {
        appendTrace(text, traceFailStyle);
    }

    private void clearTraceOutput() {
        try {
            traceDocument.remove(0, traceDocument.getLength());
            traceLineCount = 0;
        } catch (BadLocationException e) {
            // ignore
        }
    }

    // ===================== STATUS BAR =====================

    private void updateStatusBar(String message) {
        if (ide != null && ide.getStatusBar() != null) {
            ide.getStatusBar().setMessage(message);
        }
    }

    // ===================== BUTTON STATE MANAGEMENT =====================

    private void updateButtonStates(boolean debugging, boolean paused) {
        startDebugButton.setEnabled(!debugging);
        stopDebugButton.setEnabled(debugging);
        stepIntoButton.setEnabled(paused);
        stepOverButton.setEnabled(paused);
        stepOutButton.setEnabled(paused);
        continueButton.setEnabled(paused);
        // M11 CONTROL FLOW: run-to-cursor needs an active pause; restart needs a prior query.
        runToCursorButton.setEnabled(paused);
        restartButton.setEnabled(debugging && !queryRunning && lastDebugQuery != null);
        runQueryButton.setEnabled(debugging && !queryRunning);
        queryField.setEnabled(debugging && !queryRunning);
    }

    // ===================== PUBLIC API =====================

    public boolean isDebugMode() {
        return debugMode;
    }

    public boolean isTraceMode() {
        return traceToggleButton.isSelected();
    }

    public List<String> getBreakpoints() {
        return new ArrayList<>(breakpoints);
    }

    /**
     * Add a breakpoint programmatically (e.g., from editor gutter click).
     */
    public void addBreakpointProgrammatic(String predicateIndicator) {
        if (!breakpoints.contains(predicateIndicator)) {
            breakpoints.add(predicateIndicator);
            breakpointsModel.addElement(predicateIndicator);
            if (debugController != null) {
                debugController.addBreakpoint(predicateIndicator);
            }
        }
    }

    /**
     * Execute a query in debug mode from an external source (e.g., RunPanel).
     * Sets the query text and triggers debug execution.
     */
    public void debugQueryExternal(String query) {
        queryField.setText(query);
        debugQuery();
    }

    /**
     * Remove a breakpoint programmatically (e.g., from editor gutter click).
     */
    public void removeBreakpointProgrammatic(String predicateIndicator) {
        if (breakpoints.remove(predicateIndicator)) {
            breakpointsModel.removeElement(predicateIndicator);
            if (debugController != null) {
                debugController.removeBreakpoint(predicateIndicator);
            }
        }
    }

    /**
     * Push a stack frame (compatibility method for external callers).
     */
    public void pushStackFrame(String predicate, Map<String, Object> variables) {
        // Maintained for compatibility; real stack is managed by DebugController
    }

    /**
     * Pop a stack frame (compatibility method).
     */
    public void popStackFrame() {
        // Maintained for compatibility
    }
}
