package it.denzosoft.jprolog.editor;

import it.denzosoft.jprolog.core.engine.DebugController;
import it.denzosoft.jprolog.core.engine.DebugEvent;
import it.denzosoft.jprolog.core.engine.DebugStackEntry;
import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.terms.Term;
import it.denzosoft.jprolog.core.terms.Variable;
import it.denzosoft.jprolog.core.terms.CompoundTerm;
import it.denzosoft.jprolog.editor.util.DialogUtils;

import javax.swing.*;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.text.*;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;
import java.awt.*;
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

    // UI Components - Query input
    private JTextField queryField;
    private JButton runQueryButton;

    // Debug information panels
    private JTree stackTraceTree;
    private DefaultTreeModel stackTraceModel;
    private JTable variablesTable;
    private VariablesTableModel variablesModel;
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

    // Query history
    private List<String> queryHistory = new ArrayList<>();
    private int historyIndex = -1;

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

        // Variables table
        variablesModel = new VariablesTableModel();
        variablesTable = new JTable(variablesModel);
        variablesTable.setFillsViewportHeight(true);
        variablesTable.setFont(new Font("Consolas", Font.PLAIN, 12));
        variablesTable.getTableHeader().setFont(new Font("SansSerif", Font.BOLD, 12));

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

        // Left: stack trace above, variables below
        JSplitPane leftSplit = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
        leftSplit.setDividerLocation(180);

        JPanel stackPanel = new JPanel(new BorderLayout());
        stackPanel.setBorder(BorderFactory.createTitledBorder("Call Stack"));
        stackPanel.add(new JScrollPane(stackTraceTree), BorderLayout.CENTER);

        JPanel varsPanel = new JPanel(new BorderLayout());
        varsPanel.setBorder(BorderFactory.createTitledBorder("Variables"));
        varsPanel.add(new JScrollPane(variablesTable), BorderLayout.CENTER);

        leftSplit.setTopComponent(stackPanel);
        leftSplit.setBottomComponent(varsPanel);

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
        runQueryButton.addActionListener(e -> debugQuery());
        queryField.addActionListener(e -> debugQuery());

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
                // Capture System.out for side effects
                PrintStream originalOut = System.out;
                ByteArrayOutputStream baos = new ByteArrayOutputStream();
                PrintStream captureOut = new PrintStream(baos);

                List<Map<String, Term>> solutions;
                String capturedOutput;

                try {
                    System.setOut(captureOut);
                    solutions = engine.solve(query);
                    capturedOutput = baos.toString();
                } finally {
                    System.setOut(originalOut);
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

    // ===================== DebugController.DebugListener =====================

    @Override
    public void onDebugPaused(DebugEvent event) {
        // Called on solver thread — post everything to EDT
        SwingUtilities.invokeLater(() -> {
            lastPausedEvent = event;
            lastCallStack = event.getCallStack();

            // Update call stack display
            updateCallStackDisplay(event.getCallStack());

            // Update variables display with current frame bindings
            updateVariablesDisplay(event.getBindings());

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
        String input = DialogUtils.showCenteredInput(this,
            "Enter breakpoint (predicate/arity, e.g., parent/2):",
            "Add Breakpoint", JOptionPane.PLAIN_MESSAGE);

        if (input != null && !input.trim().isEmpty()) {
            String bp = input.trim();
            if (!breakpoints.contains(bp)) {
                breakpoints.add(bp);
                breakpointsModel.addElement(bp);
                if (debugController != null) {
                    debugController.addBreakpoint(bp);
                }
                appendInfo("Breakpoint added: " + bp + "\n");
            }
        }
    }

    private void removeSelectedBreakpoint() {
        int idx = breakpointsList.getSelectedIndex();
        if (idx != -1) {
            String bp = breakpointsModel.getElementAt(idx);
            breakpoints.remove(bp);
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

    private void updateVariablesDisplay(Map<String, Term> bindings) {
        variablesModel.clear();
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
                variablesModel.addVariable(name, resolved.toString());
            }
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
