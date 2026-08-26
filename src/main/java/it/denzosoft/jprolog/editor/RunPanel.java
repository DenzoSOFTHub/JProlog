package it.denzosoft.jprolog.editor;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.terms.Term;

import javax.swing.*;
import javax.swing.table.DefaultTableModel;
import javax.swing.text.*;
import java.awt.*;
import java.awt.datatransfer.StringSelection;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Panel that acts as a complete Prolog CLI terminal.
 * The text area serves as both input and output, like a real terminal.
 */
public class RunPanel extends JPanel {
    
    private JTextPane terminalArea;
    private StyledDocument document;
    private Style normalStyle;
    private Style queryStyle;
    private Style resultStyle;
    private Style errorStyle;
    private Style sideEffectStyle;
    private Style promptStyle;
    private Style commentStyle;
    private PrologIDE ide;
    
    // Terminal state
    private int promptPosition = 0;
    private boolean waitingForInput = false;
    private StringBuilder currentInput = new StringBuilder();
    private List<String> queryHistory = new ArrayList<>();
    private int historyIndex = -1;
    
    // For handling multi-line input
    private boolean inMultilineQuery = false;
    private StringBuilder multilineBuffer = new StringBuilder();
    
    // Colors for styles - Black text on white background for better readability
    private static final Color BACKGROUND_COLOR = Color.WHITE;
    private static final Color TEXT_COLOR = Color.BLACK;
    private static final Color QUERY_COLOR = new Color(0, 0, 139);  // Dark blue
    private static final Color RESULT_COLOR = new Color(0, 100, 0);  // Dark green
    private static final Color ERROR_COLOR = new Color(180, 0, 0);   // Dark red
    private static final Color SIDE_EFFECT_COLOR = new Color(128, 0, 128); // Purple
    private static final Color PROMPT_COLOR = new Color(0, 128, 0);  // Green
    private static final Color COMMENT_COLOR = Color.GRAY;
    
    // Console state
    private boolean isProcessingQuery = false;
    private Thread queryThread = null;
    /** Safety cap on the number of solutions collected for a single query (prevents OOM/hangs). */
    private static final int MAX_SOLUTIONS = 5000;

    // START_CHANGE: CONSOLE_SEARCH - Incremental find bar over the output pane
    /** Container holding the output scroll pane plus the (hidden) find bar. */
    private JPanel centerContainer;
    /** Find bar shown over the output (toggled with Ctrl+F, hidden by default). */
    private JPanel findBar;
    /** Text field where the user types the search term. */
    private JTextField findField;
    /** Label showing the current/total match count (e.g. "3 / 12"). */
    private JLabel findCountLabel;
    /** Highlight painter used for all matches (light yellow). */
    private final Highlighter.HighlightPainter findPainter =
            new DefaultHighlighter.DefaultHighlightPainter(new Color(255, 255, 0, 120));
    /** Highlight painter used for the currently-selected match (orange). */
    private final Highlighter.HighlightPainter currentPainter =
            new DefaultHighlighter.DefaultHighlightPainter(new Color(255, 150, 0, 180));
    /** Start offsets of all matches for the current search term, in document order. */
    private final List<Integer> findMatches = new ArrayList<>();
    /** Index into {@link #findMatches} of the currently-selected match, or -1 if none. */
    private int findCurrentIndex = -1;
    /** Length of the search term backing the offsets in {@link #findMatches}. */
    private int findTermLength = 0;
    // END_CHANGE: CONSOLE_SEARCH

    // START_CHANGE: M06 - Status label promoted to a field so the running timer/counter can update it.
    /** Status label in the top control panel; shows Ready / Running… / done summaries. */
    private JLabel statusLabel;
    /** Swing timer that refreshes the "Running…" elapsed-time display while a query runs. */
    private Timer runningTimer;
    /** Wall-clock start time (ms) of the in-flight query, used for elapsed-time reporting. */
    private long queryStartMillis = 0L;
    /** Live count of solutions found by the in-flight query (updated from the solver thread). */
    private volatile int liveSolutionCount = 0;
    // END_CHANGE: M06

    // START_CHANGE: M05 - Optional structured (table) view of the last query's solutions.
    /** Split pane hosting the text console (top/left) and the results table (bottom/right). */
    private JSplitPane consoleSplit;
    /** Container wrapping the results table + its toolbar; hidden by default. */
    private JPanel tablePanel;
    /** Table showing one column per query variable and one row per solution. */
    private JTable resultsTable;
    /** Backing model for {@link #resultsTable}. */
    private DefaultTableModel resultsModel;
    /** Toggle button (in the top control panel) that shows/hides the results table. */
    private JToggleButton tableToggle;
    /** Trace on/off toggle — four-port call tracing in the console output (ISS-2025-0329). */
    private JToggleButton traceToggle;
    /** Caption above the table summarising the last query and its solution count. */
    private JLabel tableCaption;
    // END_CHANGE: M05

    public RunPanel(PrologIDE ide) {
        this.ide = ide;
        initializeComponents();
        setupStyles();
        setupEventHandlers();
        setupFindBar();
        initializeTerminal();
    }
    
    /**
     * Initializes panel components.
     */
    private void initializeComponents() {
        setLayout(new BorderLayout());
        
        // Terminal area - acts as both input and output
        terminalArea = new JTextPane();
        terminalArea.setFont(new Font("Consolas", Font.PLAIN, 13));
        terminalArea.setBackground(BACKGROUND_COLOR);
        terminalArea.setForeground(TEXT_COLOR);
        terminalArea.setCaretColor(Color.BLACK);
        
        document = terminalArea.getStyledDocument();
        
        JScrollPane scrollPane = new JScrollPane(terminalArea);
        scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollPane.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        // START_CHANGE: M05 - Build the (initially hidden) structured results table and host it,
        // together with the text console, inside a vertical split pane. The text console remains the
        // default view (the table panel is collapsed/hidden until the user toggles it on).
        JPanel tableHost = buildResultsTablePanel();

        consoleSplit = new JSplitPane(JSplitPane.VERTICAL_SPLIT, scrollPane, tableHost);
        consoleSplit.setBorder(null);
        consoleSplit.setResizeWeight(0.65);
        consoleSplit.setOneTouchExpandable(true);
        // END_CHANGE: M05

        // START_CHANGE: CONSOLE_SEARCH - Host the output and a (hidden) find bar in one CENTER region.
        // The find bar is created in setupFindBar() and shown on demand (Ctrl+F); it sits below the
        // output so it does not disturb the existing top control panel / status layout.
        JPanel centerPanel = new JPanel(new BorderLayout());
        // M05: the split pane (console + table) replaces the bare scroll pane as the CENTER content.
        centerPanel.add(consoleSplit, BorderLayout.CENTER);
        add(centerPanel, BorderLayout.CENTER);
        this.centerContainer = centerPanel;
        // END_CHANGE: CONSOLE_SEARCH

        // M05: start with the table hidden so the text console is the sole, default view.
        setTableVisible(false);
        
        // Control panel at the top
        JPanel controlPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        controlPanel.setBackground(new Color(240, 240, 240));
        controlPanel.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, Color.LIGHT_GRAY));
        
        JButton clearButton = new JButton("Clear");
        clearButton.setToolTipText("Clear the terminal screen");
        clearButton.addActionListener(e -> clearTerminal());
        
        JButton restartButton = new JButton("Restart");
        restartButton.setToolTipText("Restart Prolog engine and reload knowledge base");
        restartButton.addActionListener(e -> restartProlog());
        
        JButton interruptButton = new JButton("Interrupt");
        interruptButton.setToolTipText("Interrupt current query (Ctrl+C)");
        interruptButton.addActionListener(e -> interruptQuery());
        
        controlPanel.add(clearButton);
        controlPanel.add(restartButton);
        controlPanel.add(interruptButton);

        // START_CHANGE: M05 - Toggle button to show/hide the structured results table.
        tableToggle = new JToggleButton("Table");
        tableToggle.setToolTipText("Show/hide the structured results table (one row per solution)");
        tableToggle.addActionListener(e -> setTableVisible(tableToggle.isSelected()));
        controlPanel.add(tableToggle);
        // END_CHANGE: M05

        // START_CHANGE: ISS-2025-0329 - Trace on/off toggle (four-port call tracing in the console output)
        traceToggle = new JToggleButton("Trace");
        traceToggle.setToolTipText("Enable/disable four-port call tracing (trace/0 .. notrace/0)");
        // ISS-2025-0437 - ENG-06: tracing is per ENGINE now, and this runs on the EDT (no engine is
        // "current" on that thread), so toggle it on the IDE's shared Prolog instance directly.
        traceToggle.setSelected(ide.getPrologEngine() != null && ide.getPrologEngine().isTracing());
        traceToggle.addActionListener(e -> {
            boolean on = traceToggle.isSelected();
            if (ide.getPrologEngine() != null) ide.getPrologEngine().setTracing(on);
            appendText("% Tracing " + (on ? "enabled" : "disabled") + "\n", commentStyle);
        });
        controlPanel.add(traceToggle);
        // END_CHANGE: ISS-2025-0329

        // START_CHANGE: M06 - Promote the status label to a field so the running timer/counter can
        // update it live (Running… + elapsed + solution count) and reset it to a done summary.
        statusLabel = new JLabel("  Ready");
        statusLabel.setForeground(Color.GRAY);
        controlPanel.add(statusLabel);
        // END_CHANGE: M06

        add(controlPanel, BorderLayout.NORTH);
    }

    // START_CHANGE: M05 - Structured results table (one column per variable, one row per solution).
    /**
     * Build the panel that hosts the results {@link JTable} plus a small caption/toolbar.
     * The table is read-only; a right-click popup offers Copy and Export CSV. The panel is created
     * once and toggled visible via {@link #setTableVisible(boolean)} — the text console stays default.
     *
     * @return the table host panel (ready to drop into the split pane)
     */
    private JPanel buildResultsTablePanel() {
        tablePanel = new JPanel(new BorderLayout());

        resultsModel = new DefaultTableModel() {
            @Override
            public boolean isCellEditable(int row, int column) {
                return false; // results are read-only
            }
        };
        resultsTable = new JTable(resultsModel);
        resultsTable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
        resultsTable.setFont(new Font("Consolas", Font.PLAIN, 12));
        resultsTable.setFillsViewportHeight(true);
        resultsTable.setAutoCreateRowSorter(true);

        // Right-click popup: Copy selection (or whole table) and Export CSV.
        final JPopupMenu popup = new JPopupMenu();
        JMenuItem copyItem = new JMenuItem("Copy");
        copyItem.addActionListener(e -> copyResultsToClipboard());
        JMenuItem exportItem = new JMenuItem("Export CSV...");
        exportItem.addActionListener(e -> exportResultsToCsv());
        popup.add(copyItem);
        popup.add(exportItem);
        resultsTable.setComponentPopupMenu(popup);

        tableCaption = new JLabel(" No results");
        tableCaption.setForeground(Color.GRAY);
        tableCaption.setBorder(BorderFactory.createEmptyBorder(2, 4, 2, 4));

        JPanel topBar = new JPanel(new BorderLayout());
        topBar.setBackground(new Color(248, 248, 248));
        topBar.setBorder(BorderFactory.createMatteBorder(1, 0, 1, 0, Color.LIGHT_GRAY));
        topBar.add(tableCaption, BorderLayout.WEST);

        JPanel buttons = new JPanel(new FlowLayout(FlowLayout.RIGHT, 4, 1));
        buttons.setOpaque(false);
        JButton copyBtn = new JButton("Copy");
        copyBtn.setFocusable(false);
        copyBtn.setToolTipText("Copy table contents to the clipboard (TSV)");
        copyBtn.addActionListener(e -> copyResultsToClipboard());
        JButton csvBtn = new JButton("Export CSV");
        csvBtn.setFocusable(false);
        csvBtn.setToolTipText("Export the results table to a CSV file");
        csvBtn.addActionListener(e -> exportResultsToCsv());
        buttons.add(copyBtn);
        buttons.add(csvBtn);
        topBar.add(buttons, BorderLayout.EAST);

        tablePanel.add(topBar, BorderLayout.NORTH);
        tablePanel.add(new JScrollPane(resultsTable), BorderLayout.CENTER);
        return tablePanel;
    }

    /**
     * Show or hide the structured results table, keeping the toggle button in sync.
     * When hidden the split divider collapses so the text console reclaims the full height.
     *
     * @param visible true to reveal the table, false to hide it
     */
    private void setTableVisible(boolean visible) {
        if (tablePanel == null || consoleSplit == null) {
            return;
        }
        tablePanel.setVisible(visible);
        if (tableToggle != null && tableToggle.isSelected() != visible) {
            tableToggle.setSelected(visible);
        }
        if (visible) {
            // Give the table a reasonable share of the height when first shown.
            SwingUtilities.invokeLater(() -> consoleSplit.setDividerLocation(0.65));
        }
        consoleSplit.revalidate();
        consoleSplit.repaint();
    }

    /**
     * Populate the results table from a query's solution list: one column per distinct variable
     * (preserving first-seen order) and one row per solution. Empty bindings (a bare "true") yield
     * a single "(true)" column so the table is never blank for a succeeding ground query.
     *
     * @param query     the query text (for the caption)
     * @param solutions the solutions to render (each a variable->Term binding map)
     */
    private void populateResultsTable(String query, List<Map<String, Term>> solutions) {
        if (resultsModel == null) {
            return;
        }

        // Collect column names in first-seen order across all solutions.
        java.util.LinkedHashSet<String> columns = new java.util.LinkedHashSet<>();
        for (Map<String, Term> sol : solutions) {
            columns.addAll(sol.keySet());
        }

        resultsModel.setRowCount(0);
        resultsModel.setColumnCount(0);

        if (columns.isEmpty()) {
            // Ground query that succeeded with no bindings: show a simple truth column.
            resultsModel.addColumn("(result)");
            for (int i = 0; i < solutions.size(); i++) {
                resultsModel.addRow(new Object[]{"true"});
            }
        } else {
            for (String col : columns) {
                resultsModel.addColumn(col);
            }
            for (Map<String, Term> sol : solutions) {
                Object[] row = new Object[columns.size()];
                int c = 0;
                for (String col : columns) {
                    Term value = sol.get(col);
                    row[c++] = (value == null) ? "" : value.toString();
                }
                resultsModel.addRow(row);
            }
        }

        if (tableCaption != null) {
            String q = (query == null) ? "" : query.trim();
            if (q.length() > 80) {
                q = q.substring(0, 77) + "...";
            }
            int n = solutions.size();
            tableCaption.setText(" " + q + "  —  " + n + (n == 1 ? " solution" : " solutions"));
        }
    }

    /**
     * Copy the results table (header + currently-selected rows, or all rows if none selected) to the
     * system clipboard as tab-separated values.
     */
    private void copyResultsToClipboard() {
        if (resultsModel == null || resultsModel.getColumnCount() == 0) {
            return;
        }
        StringBuilder sb = new StringBuilder();
        int cols = resultsModel.getColumnCount();
        for (int c = 0; c < cols; c++) {
            if (c > 0) sb.append('\t');
            sb.append(resultsModel.getColumnName(c));
        }
        sb.append('\n');

        int[] selected = resultsTable.getSelectedRows();
        int rows = resultsModel.getRowCount();
        if (selected == null || selected.length == 0) {
            for (int r = 0; r < rows; r++) {
                appendTableRow(sb, r, cols, '\t', false);
            }
        } else {
            for (int viewRow : selected) {
                int modelRow = resultsTable.convertRowIndexToModel(viewRow);
                appendTableRow(sb, modelRow, cols, '\t', false);
            }
        }

        try {
            Toolkit.getDefaultToolkit().getSystemClipboard()
                    .setContents(new StringSelection(sb.toString()), null);
            if (statusLabel != null) {
                statusLabel.setText("  Copied table to clipboard");
            }
        } catch (Exception ex) {
            JOptionPane.showMessageDialog(this, "Copy failed: " + ex.getMessage(),
                    "Copy", JOptionPane.WARNING_MESSAGE);
        }
    }

    /**
     * Export the full results table to a user-chosen CSV file (header + all rows, RFC-4180 quoting).
     */
    private void exportResultsToCsv() {
        if (resultsModel == null || resultsModel.getColumnCount() == 0) {
            JOptionPane.showMessageDialog(this, "There are no results to export.",
                    "Export CSV", JOptionPane.INFORMATION_MESSAGE);
            return;
        }
        JFileChooser chooser = new JFileChooser();
        chooser.setDialogTitle("Export results to CSV");
        chooser.setSelectedFile(new java.io.File("results.csv"));
        if (chooser.showSaveDialog(this) != JFileChooser.APPROVE_OPTION) {
            return;
        }
        java.io.File file = chooser.getSelectedFile();
        if (!file.getName().toLowerCase().endsWith(".csv")) {
            file = new java.io.File(file.getParentFile(), file.getName() + ".csv");
        }

        int cols = resultsModel.getColumnCount();
        StringBuilder sb = new StringBuilder();
        for (int c = 0; c < cols; c++) {
            if (c > 0) sb.append(',');
            sb.append(csvQuote(resultsModel.getColumnName(c)));
        }
        sb.append('\n');
        for (int r = 0; r < resultsModel.getRowCount(); r++) {
            appendTableRow(sb, r, cols, ',', true);
        }

        try (java.io.FileWriter w = new java.io.FileWriter(file)) {
            w.write(sb.toString());
            if (statusLabel != null) {
                statusLabel.setText("  Exported " + resultsModel.getRowCount() + " rows to CSV");
            }
        } catch (java.io.IOException ex) {
            JOptionPane.showMessageDialog(this, "Export failed: " + ex.getMessage(),
                    "Export CSV", JOptionPane.WARNING_MESSAGE);
        }
    }

    /**
     * Append a single table row to {@code sb} using the given separator, optionally CSV-quoting cells.
     */
    private void appendTableRow(StringBuilder sb, int modelRow, int cols, char sep, boolean csv) {
        for (int c = 0; c < cols; c++) {
            if (c > 0) sb.append(sep);
            Object v = resultsModel.getValueAt(modelRow, c);
            String s = (v == null) ? "" : v.toString();
            sb.append(csv ? csvQuote(s) : s);
        }
        sb.append('\n');
    }

    /**
     * Quote a value for CSV output per RFC 4180 (wrap in quotes if it contains a comma, quote or
     * newline; double any embedded quotes).
     */
    private String csvQuote(String s) {
        if (s == null) {
            return "";
        }
        if (s.indexOf(',') >= 0 || s.indexOf('"') >= 0 || s.indexOf('\n') >= 0 || s.indexOf('\r') >= 0) {
            return "\"" + s.replace("\"", "\"\"") + "\"";
        }
        return s;
    }
    // END_CHANGE: M05

    // START_CHANGE: M06 - Running status: elapsed-time timer + live solution counter in the status label.
    /**
     * Switch the status label into "Running…" mode and start the elapsed-time timer.
     * Called on the EDT just before a query begins executing.
     */
    private void startRunningStatus() {
        queryStartMillis = System.currentTimeMillis();
        liveSolutionCount = 0;
        updateRunningStatus();
        if (runningTimer == null) {
            // Refresh roughly 4x/second so the elapsed time and counter feel live without flicker.
            runningTimer = new Timer(250, e -> updateRunningStatus());
            runningTimer.setRepeats(true);
        }
        runningTimer.start();
    }

    /**
     * Refresh the "Running…" label with the current elapsed time and live solution count.
     */
    private void updateRunningStatus() {
        if (statusLabel == null) {
            return;
        }
        long elapsed = System.currentTimeMillis() - queryStartMillis;
        statusLabel.setForeground(new Color(0, 100, 0));
        statusLabel.setText("  Running…  " + formatElapsed(elapsed)
                + "  (" + liveSolutionCount + (liveSolutionCount == 1 ? " solution)" : " solutions)"));
    }

    /**
     * Stop the running timer and reset the status label to a done summary (or plain "Ready").
     *
     * @param totalSolutions number of solutions the query produced (-1 to show a bare "Ready")
     */
    private void finishRunningStatus(int totalSolutions) {
        if (runningTimer != null) {
            runningTimer.stop();
        }
        if (statusLabel == null) {
            return;
        }
        statusLabel.setForeground(Color.GRAY);
        if (totalSolutions < 0) {
            statusLabel.setText("  Ready");
        } else {
            long elapsed = System.currentTimeMillis() - queryStartMillis;
            statusLabel.setText("  Ready  —  " + totalSolutions
                    + (totalSolutions == 1 ? " solution" : " solutions")
                    + " in " + formatElapsed(elapsed));
        }
    }

    /**
     * Format an elapsed duration (ms) compactly as e.g. "0.42s" or "1m 03.5s".
     */
    private String formatElapsed(long millis) {
        if (millis < 0) {
            millis = 0;
        }
        if (millis < 60000) {
            return String.format("%.2fs", millis / 1000.0);
        }
        long minutes = millis / 60000;
        double seconds = (millis % 60000) / 1000.0;
        return String.format("%dm %04.1fs", minutes, seconds);
    }
    // END_CHANGE: M06

    /**
     * Setup text styles.
     */
    private void setupStyles() {
        normalStyle = terminalArea.addStyle("Normal", null);
        StyleConstants.setForeground(normalStyle, TEXT_COLOR);
        StyleConstants.setFontFamily(normalStyle, "Consolas");
        StyleConstants.setFontSize(normalStyle, 13);
        
        promptStyle = terminalArea.addStyle("Prompt", null);
        StyleConstants.setForeground(promptStyle, PROMPT_COLOR);
        StyleConstants.setBold(promptStyle, true);
        StyleConstants.setFontFamily(promptStyle, "Consolas");
        StyleConstants.setFontSize(promptStyle, 13);
        
        queryStyle = terminalArea.addStyle("Query", null);
        StyleConstants.setForeground(queryStyle, QUERY_COLOR);
        StyleConstants.setBold(queryStyle, false);
        StyleConstants.setFontFamily(queryStyle, "Consolas");
        StyleConstants.setFontSize(queryStyle, 13);
        
        resultStyle = terminalArea.addStyle("Result", null);
        StyleConstants.setForeground(resultStyle, RESULT_COLOR);
        StyleConstants.setFontFamily(resultStyle, "Consolas");
        StyleConstants.setFontSize(resultStyle, 13);
        
        errorStyle = terminalArea.addStyle("Error", null);
        StyleConstants.setForeground(errorStyle, ERROR_COLOR);
        StyleConstants.setFontFamily(errorStyle, "Consolas");
        StyleConstants.setFontSize(errorStyle, 13);
        
        sideEffectStyle = terminalArea.addStyle("SideEffect", null);
        StyleConstants.setForeground(sideEffectStyle, SIDE_EFFECT_COLOR);
        StyleConstants.setItalic(sideEffectStyle, true);
        StyleConstants.setFontFamily(sideEffectStyle, "Consolas");
        StyleConstants.setFontSize(sideEffectStyle, 13);
        
        commentStyle = terminalArea.addStyle("Comment", null);
        StyleConstants.setForeground(commentStyle, COMMENT_COLOR);
        StyleConstants.setItalic(commentStyle, true);
        StyleConstants.setFontFamily(commentStyle, "Consolas");
        StyleConstants.setFontSize(commentStyle, 13);
    }
    
    /**
     * Setup event handlers for terminal interaction.
     */
    private void setupEventHandlers() {
        terminalArea.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                handleKeyPress(e);
            }

            @Override
            public void keyTyped(KeyEvent e) {
                handleKeyTyped(e);
            }
        });

        // START_CHANGE: CONSOLE_SEARCH - Ctrl+F opens the find bar when the output pane is focused.
        // Use the InputMap/ActionMap (rather than the KeyListener above) so Ctrl+F is intercepted
        // cleanly and never reaches the terminal's typed-character handling.
        int menuMask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();
        terminalArea.getInputMap(JComponent.WHEN_FOCUSED).put(
                KeyStroke.getKeyStroke(KeyEvent.VK_F, menuMask), "console-find");
        terminalArea.getActionMap().put("console-find", new AbstractAction() {
            @Override
            public void actionPerformed(java.awt.event.ActionEvent e) {
                showFindBar();
            }
        });
        // END_CHANGE: CONSOLE_SEARCH
    }
    
    // START_CHANGE: CONSOLE_SEARCH - Build the incremental find bar (hidden until Ctrl+F / Esc toggles it).
    /**
     * Build the find bar used for incremental, case-insensitive search over the output pane.
     * The bar is created once, kept hidden, and added to the bottom of the output container.
     * It is fully self-contained: it reads the document text and paints matches through the
     * pane's own {@link Highlighter}, so it never touches query-execution logic.
     */
    private void setupFindBar() {
        findBar = new JPanel(new FlowLayout(FlowLayout.LEFT, 4, 2));
        findBar.setBackground(new Color(245, 245, 245));
        findBar.setBorder(BorderFactory.createMatteBorder(1, 0, 0, 0, Color.LIGHT_GRAY));

        JLabel findLabel = new JLabel("Find:");
        findField = new JTextField(20);
        findField.setToolTipText("Find in console output (case-insensitive). Enter = next, Shift+Enter = previous, Esc = close.");

        findCountLabel = new JLabel("");
        findCountLabel.setForeground(Color.GRAY);

        JButton nextButton = new JButton("Next");
        nextButton.setToolTipText("Next match (Enter)");
        nextButton.setFocusable(false);
        nextButton.addActionListener(e -> findNext(true));

        JButton prevButton = new JButton("Prev");
        prevButton.setToolTipText("Previous match (Shift+Enter)");
        prevButton.setFocusable(false);
        prevButton.addActionListener(e -> findNext(false));

        JButton closeButton = new JButton("×");
        closeButton.setToolTipText("Close (Esc)");
        closeButton.setFocusable(false);
        closeButton.addActionListener(e -> hideFindBar());

        findBar.add(findLabel);
        findBar.add(findField);
        findBar.add(prevButton);
        findBar.add(nextButton);
        findBar.add(findCountLabel);
        findBar.add(closeButton);
        findBar.setVisible(false);

        // Recompute matches incrementally as the user types.
        findField.getDocument().addDocumentListener(new javax.swing.event.DocumentListener() {
            @Override public void insertUpdate(javax.swing.event.DocumentEvent e) { updateSearch(true); }
            @Override public void removeUpdate(javax.swing.event.DocumentEvent e) { updateSearch(true); }
            @Override public void changedUpdate(javax.swing.event.DocumentEvent e) { updateSearch(true); }
        });

        // Enter = next, Shift+Enter = previous, Esc = close.
        findField.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_ENTER) {
                    e.consume();
                    findNext(!e.isShiftDown());
                } else if (e.getKeyCode() == KeyEvent.VK_ESCAPE) {
                    e.consume();
                    hideFindBar();
                }
            }
        });

        if (centerContainer != null) {
            centerContainer.add(findBar, BorderLayout.SOUTH);
        }
    }

    /**
     * Show the find bar, pre-filling it with any current selection, and focus the field.
     */
    private void showFindBar() {
        if (findBar == null) {
            return;
        }
        String selected = terminalArea.getSelectedText();
        if (selected != null && !selected.isEmpty() && selected.indexOf('\n') < 0) {
            findField.setText(selected);
        }
        findBar.setVisible(true);
        if (centerContainer != null) {
            centerContainer.revalidate();
            centerContainer.repaint();
        }
        SwingUtilities.invokeLater(() -> {
            findField.requestFocusInWindow();
            findField.selectAll();
            updateSearch(true);
        });
    }

    /**
     * Hide the find bar, clear all highlights, and return focus to the terminal.
     */
    private void hideFindBar() {
        if (findBar == null) {
            return;
        }
        clearFindHighlights();
        findBar.setVisible(false);
        if (centerContainer != null) {
            centerContainer.revalidate();
            centerContainer.repaint();
        }
        focusPrompt();
    }

    /**
     * Recompute the set of matches for the current search term and repaint highlights.
     *
     * @param moveToFirst when true, select the first match at or after the caret; otherwise keep
     *                    the current selection index if it is still valid.
     */
    private void updateSearch(boolean moveToFirst) {
        clearFindHighlights();
        findMatches.clear();
        findCurrentIndex = -1;

        String term = (findField == null) ? "" : findField.getText();
        findTermLength = term.length();
        if (term.isEmpty()) {
            updateCountLabel();
            return;
        }

        String text;
        try {
            text = document.getText(0, document.getLength());
        } catch (BadLocationException e) {
            return;
        }

        String haystack = text.toLowerCase();
        String needle = term.toLowerCase();
        int from = 0;
        int idx;
        while ((idx = haystack.indexOf(needle, from)) >= 0) {
            findMatches.add(idx);
            from = idx + needle.length();
        }

        if (findMatches.isEmpty()) {
            updateCountLabel();
            return;
        }

        // Pick the first match at or after the caret so Find feels anchored to the view.
        if (moveToFirst) {
            int caret = terminalArea.getCaretPosition();
            findCurrentIndex = 0;
            for (int i = 0; i < findMatches.size(); i++) {
                if (findMatches.get(i) >= caret) {
                    findCurrentIndex = i;
                    break;
                }
            }
        } else if (findCurrentIndex < 0 || findCurrentIndex >= findMatches.size()) {
            findCurrentIndex = 0;
        }

        repaintHighlights();
        scrollToCurrentMatch();
        updateCountLabel();
    }

    /**
     * Advance to the next (or previous) match with wrap-around.
     *
     * @param forward true for next, false for previous
     */
    private void findNext(boolean forward) {
        if (findMatches.isEmpty()) {
            return;
        }
        if (findCurrentIndex < 0) {
            findCurrentIndex = forward ? 0 : findMatches.size() - 1;
        } else {
            findCurrentIndex += forward ? 1 : -1;
            if (findCurrentIndex >= findMatches.size()) {
                findCurrentIndex = 0;           // wrap to start
            } else if (findCurrentIndex < 0) {
                findCurrentIndex = findMatches.size() - 1; // wrap to end
            }
        }
        repaintHighlights();
        scrollToCurrentMatch();
        updateCountLabel();
    }

    /**
     * Repaint all match highlights, drawing the current match in a distinct colour.
     */
    private void repaintHighlights() {
        Highlighter hl = terminalArea.getHighlighter();
        hl.removeAllHighlights();
        if (findTermLength <= 0) {
            return;
        }
        for (int i = 0; i < findMatches.size(); i++) {
            int start = findMatches.get(i);
            int end = start + findTermLength;
            try {
                Highlighter.HighlightPainter painter =
                        (i == findCurrentIndex) ? currentPainter : findPainter;
                hl.addHighlight(start, end, painter);
            } catch (BadLocationException e) {
                // Offsets are always valid relative to the current document; ignore defensively.
            }
        }
    }

    /**
     * Scroll the output so the current match is visible and select it in the pane.
     */
    private void scrollToCurrentMatch() {
        if (findCurrentIndex < 0 || findCurrentIndex >= findMatches.size()) {
            return;
        }
        int start = findMatches.get(findCurrentIndex);
        int end = start + findTermLength;
        try {
            Rectangle r = terminalArea.modelToView(start);
            if (r != null) {
                terminalArea.scrollRectToVisible(r);
            }
            // Reflect the match as the caret/selection too (kept consistent with anchoring).
            terminalArea.select(start, end);
        } catch (BadLocationException e) {
            // Ignore defensively.
        }
    }

    /**
     * Remove all search highlights from the output pane.
     */
    private void clearFindHighlights() {
        terminalArea.getHighlighter().removeAllHighlights();
    }

    /**
     * Update the "N / M" match-count label (or "No matches" / blank).
     */
    private void updateCountLabel() {
        if (findCountLabel == null) {
            return;
        }
        if (findField == null || findField.getText().isEmpty()) {
            findCountLabel.setText("");
        } else if (findMatches.isEmpty()) {
            findCountLabel.setText("No matches");
        } else {
            findCountLabel.setText((findCurrentIndex + 1) + " / " + findMatches.size());
        }
    }

    /**
     * Refresh search results if the find bar is open while new output is appended.
     * Called from {@link #appendText} so the match set stays in sync with live output.
     */
    private void refreshFindIfVisible() {
        if (findBar != null && findBar.isVisible()
                && findField != null && !findField.getText().isEmpty()) {
            // Keep the current selection index where possible (do not jump to caret).
            updateSearch(false);
        }
    }
    // END_CHANGE: CONSOLE_SEARCH

    /**
     * Initialize terminal with welcome message and first prompt.
     */
    private void initializeTerminal() {
        appendText("% JProlog Interactive Console\n", commentStyle);
        appendText("% Version 1.0 - JProlog Editor from DenzoSOFT\n", commentStyle);
        appendText("% Type 'help.' for help, 'halt.' to exit session.\n", commentStyle);
        appendText("% Knowledge base initialized from project compilation.\n\n", commentStyle);
        
        // Show initial KB status if engine is available
        showKnowledgeBaseStatus();
        
        showPrompt();
        
        // Ensure the terminal has focus
        SwingUtilities.invokeLater(() -> {
            terminalArea.requestFocusInWindow();
            terminalArea.setCaretPosition(document.getLength());
        });
    }
    
    /**
     * Show knowledge base status.
     */
    private void showKnowledgeBaseStatus() {
        Prolog engine = ide.getPrologEngine();
        if (engine != null) {
            String kbStatus = engine.getListingOutput();
            if (kbStatus != null && !kbStatus.trim().isEmpty() && 
                !kbStatus.contains("Knowledge base is empty")) {
                appendText("% Current knowledge base:\n", commentStyle);
                appendText(kbStatus, sideEffectStyle);
                appendText("\n", normalStyle);
            }
        }
    }
    
    /**
     * Show the prompt.
     */
    private void showPrompt() {
        if (inMultilineQuery) {
            appendText("|    ", promptStyle);
        } else {
            appendText("?- ", promptStyle);
        }
        promptPosition = document.getLength();
        waitingForInput = true;
        
        // Ensure caret is at the right position and terminal has focus
        SwingUtilities.invokeLater(() -> {
            terminalArea.setCaretPosition(promptPosition);
            terminalArea.requestFocusInWindow();
        });
    }
    
    /**
     * Handle key press events.
     */
    private void handleKeyPress(KeyEvent e) {
        int caretPos = terminalArea.getCaretPosition();
        
        // Prevent editing before prompt
        if (caretPos < promptPosition && e.getKeyCode() != KeyEvent.VK_C && !e.isControlDown()) {
            e.consume();
            terminalArea.setCaretPosition(document.getLength());
            return;
        }
        
        switch (e.getKeyCode()) {
            case KeyEvent.VK_ENTER:
                if (!isProcessingQuery) {
                    e.consume();
                    processInput();
                }
                break;
                
            case KeyEvent.VK_UP:
                if (waitingForInput && !queryHistory.isEmpty()) {
                    e.consume();
                    navigateHistory(-1);
                }
                break;
                
            case KeyEvent.VK_DOWN:
                if (waitingForInput && !queryHistory.isEmpty()) {
                    e.consume();
                    navigateHistory(1);
                }
                break;
                
            case KeyEvent.VK_BACK_SPACE:
            case KeyEvent.VK_DELETE:
                if (caretPos <= promptPosition) {
                    e.consume();
                }
                break;
                
            case KeyEvent.VK_LEFT:
                if (caretPos <= promptPosition) {
                    e.consume();
                }
                break;
                
            case KeyEvent.VK_HOME:
                e.consume();
                terminalArea.setCaretPosition(promptPosition);
                break;
                
            case KeyEvent.VK_C:
                if (e.isControlDown()) {
                    interruptQuery();
                }
                break;
        }
    }
    
    /**
     * Handle key typed events.
     */
    private void handleKeyTyped(KeyEvent e) {
        if (!waitingForInput || terminalArea.getCaretPosition() < promptPosition) {
            e.consume();
            if (waitingForInput) {
                terminalArea.setCaretPosition(document.getLength());
            }
        }
    }
    
    /**
     * Process the input when Enter is pressed.
     */
    private void processInput() {
        try {
            String input = document.getText(promptPosition, document.getLength() - promptPosition);
            appendText("\n", normalStyle);
            
            // Check for multi-line continuation
            if (!input.trim().isEmpty() && !input.trim().endsWith(".")) {
                inMultilineQuery = true;
                multilineBuffer.append(input).append(" ");
                showPrompt();
                return;
            }
            
            // Complete multi-line query
            if (inMultilineQuery) {
                multilineBuffer.append(input);
                input = multilineBuffer.toString();
                multilineBuffer.setLength(0);
                inMultilineQuery = false;
            }
            
            // Process the complete query
            if (!input.trim().isEmpty()) {
                String query = input.trim();
                if (query.endsWith(".")) {
                    query = query.substring(0, query.length() - 1).trim();
                }
                
                if (!query.isEmpty()) {
                    queryHistory.add(query);
                    historyIndex = queryHistory.size();
                    
                    waitingForInput = false;
                    executeQuery(query);
                } else {
                    showPrompt();
                }
            } else {
                showPrompt();
            }
            
        } catch (BadLocationException ex) {
            appendText("Error reading input\n", errorStyle);
            showPrompt();
        }
    }
    
    /**
     * Navigate through query history.
     */
    private void navigateHistory(int direction) {
        if (queryHistory.isEmpty()) return;
        
        historyIndex += direction;
        
        if (historyIndex < 0) {
            historyIndex = 0;
        } else if (historyIndex >= queryHistory.size()) {
            historyIndex = queryHistory.size() - 1;
        }
        
        if (historyIndex >= 0 && historyIndex < queryHistory.size()) {
            try {
                // Clear current input
                document.remove(promptPosition, document.getLength() - promptPosition);
                // Insert history item
                document.insertString(promptPosition, queryHistory.get(historyIndex), queryStyle);
                terminalArea.setCaretPosition(document.getLength());
            } catch (BadLocationException ex) {
                // Ignore
            }
        }
    }
    
    /**
     * Execute a Prolog query.
     */
    private void executeQuery(String query) {
        isProcessingQuery = true;
        
        // Handle special commands
        if (handleSpecialCommands(query)) {
            isProcessingQuery = false;
            showPrompt();
            return;
        }
        
        // START_CHANGE: ISS-2025-0090 - Route queries through debugger when debug mode active
        if (ide.isDebugMode() && ide.getDebugPanel() != null) {
            appendText("% Routing query to Debug panel (debug mode active).\n", commentStyle);
            SwingUtilities.invokeLater(() -> {
                ide.getBottomTabbedPane().showDebugTab();
                ide.getDebugPanel().debugQueryExternal(query);
                isProcessingQuery = false;
                showPrompt();
            });
            return;
        }
        // END_CHANGE: ISS-2025-0090

        // START_CHANGE: M06 - Begin the running-status timer/counter for this (real) query.
        startRunningStatus();
        // END_CHANGE: M06

        // Execute in separate thread to keep UI responsive
        queryThread = new Thread(() -> {
            executeNormalQuery(query);
            SwingUtilities.invokeLater(() -> {
                isProcessingQuery = false;
                showPrompt();
            });
        });
        queryThread.start();
    }
    
    /**
     * Handle special commands.
     */
    private boolean handleSpecialCommands(String query) {
        // Handle help
        if (query.equals("help") || query.equals("help()")) {
            showHelp();
            return true;
        }
        
        // Handle halt
        if (query.equals("halt") || query.equals("halt()")) {
            appendText("% Session terminated.\n", commentStyle);
            waitingForInput = false;
            return true;
        }
        
        // Handle listing
        if (query.equals("listing") || query.equals("listing()")) {
            executeListing();
            return true;
        }
        
        if (query.startsWith("listing(") && query.endsWith(")")) {
            String arg = query.substring(8, query.length() - 1).trim();
            if (arg.startsWith("'") && arg.endsWith("'")) {
                arg = arg.substring(1, arg.length() - 1);
            }
            executeListing(arg);
            return true;
        }
        
        // Handle consult
        if (query.startsWith("consult(") && query.endsWith(")")) {
            String filename = query.substring(8, query.length() - 1).trim();
            if (filename.startsWith("'") && filename.endsWith("'")) {
                filename = filename.substring(1, filename.length() - 1);
            }
            executeConsult(filename);
            return true;
        }
        
        // Handle clear
        if (query.equals("clear") || query.equals("cls")) {
            clearTerminal();
            return true;
        }
        
        return false;
    }
    
    /**
     * Show help information.
     */
    private void showHelp() {
        appendText("\n% Available commands:\n", commentStyle);
        appendText("%   help.              - Show this help\n", commentStyle);
        appendText("%   listing.           - List all predicates\n", commentStyle);
        appendText("%   listing('p/n').    - List specific predicate\n", commentStyle);
        appendText("%   consult('file').   - Load Prolog file\n", commentStyle);
        appendText("%   clear.             - Clear screen\n", commentStyle);
        appendText("%   halt.              - Exit session\n", commentStyle);
        appendText("%   Ctrl+C             - Interrupt query\n", commentStyle);
        appendText("\n", normalStyle);
    }
    
    /**
     * Execute listing command.
     */
    private void executeListing() {
        Prolog engine = ide.getPrologEngine();
        if (engine == null) {
            appendText("ERROR: No Prolog engine available.\n", errorStyle);
            return;
        }
        
        String output = engine.getListingOutput();
        if (output != null && !output.trim().isEmpty()) {
            appendText(output, sideEffectStyle);
        } else {
            appendText("% Knowledge base is empty.\n", commentStyle);
        }
        appendText("true.\n", resultStyle);
    }
    
    /**
     * Execute listing for specific predicate.
     */
    private void executeListing(String predicate) {
        Prolog engine = ide.getPrologEngine();
        if (engine == null) {
            appendText("ERROR: No Prolog engine available.\n", errorStyle);
            return;
        }
        
        String output = engine.getListingOutput(predicate);
        if (output != null && !output.trim().isEmpty()) {
            appendText(output, sideEffectStyle);
        }
        appendText("true.\n", resultStyle);
    }
    
    /**
     * Execute consult command.
     */
    private void executeConsult(String filename) {
        Prolog engine = ide.getPrologEngine();
        if (engine == null) {
            appendText("ERROR: No Prolog engine available.\n", errorStyle);
            return;
        }
        
        try {
            java.io.File file = new java.io.File(filename);
            if (!file.exists() && ide.getCurrentProjectRoot() != null) {
                file = new java.io.File(ide.getCurrentProjectRoot(), filename);
            }
            
            if (!file.exists()) {
                appendText("ERROR: File not found: " + filename + "\n", errorStyle);
                appendText("false.\n", errorStyle);
                return;
            }
            
            String content = new String(java.nio.file.Files.readAllBytes(file.toPath()));
            engine.consult(content);
            
            appendText("% " + filename + " consulted.\n", commentStyle);
            appendText("true.\n", resultStyle);
            
        } catch (Exception e) {
            appendText("ERROR: " + e.getMessage() + "\n", errorStyle);
            appendText("false.\n", errorStyle);
        }
    }
    
    /**
     * Execute normal Prolog query.
     */
    private void executeNormalQuery(String query) {
        Prolog engine = ide.getPrologEngine();
        
        if (engine == null) {
            SwingUtilities.invokeLater(() -> {
                appendText("ERROR: No Prolog engine available.\n", errorStyle);
                appendText("false.\n", errorStyle);
            });
            return;
        }
        
        try {
            // ISS-2025-0327: capture write/1 output for THIS query thread only (no process-wide
            // System.setOut, which would garble unrelated output from the EDT/other threads).
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            PrintStream captureOut = new PrintStream(baos);

            // Stream solutions lazily with a safety cap, so a query with many/infinite solutions
            // neither buffers everything (OOM) nor hangs — and the Stop button can interrupt it.
            final int CAP = MAX_SOLUTIONS;
            final List<Map<String, Term>> solutions = new ArrayList<>();
            final boolean[] capped = {false};
            String capturedOutput = "";
            try {
                it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(captureOut);
                engine.solveStream(query, sol -> {
                    solutions.add(sol);
                    // M06: publish the live count so the running-status timer can display progress.
                    liveSolutionCount = solutions.size();
                    if (solutions.size() >= CAP) { capped[0] = true; return false; }
                    return true;
                });
                captureOut.flush();
                capturedOutput = baos.toString();
            } finally {
                it.denzosoft.jprolog.builtin.io.StreamManager.setThreadLocalOutput(null);
            }

            final String output = capturedOutput;
            final List<Map<String, Term>> finalSolutions = solutions;
            final boolean wasCapped = capped[0];
            // START_CHANGE: ISS-2025-0476 - wave W7, design decision 5: render the answers with the
            // engine's operator table, quoted, with _A-style variable names and residual goals.
            // Done on the SOLVER thread, right after the solve, because the CLP(FD) part of
            // Prolog.residualGoals reads the per-query constraint store.
            final List<List<String>> renderedSolutions = new ArrayList<>();
            for (Map<String, Term> sol : finalSolutions) {
                renderedSolutions.add(it.denzosoft.jprolog.core.engine.v4.Answer.lines(
                    sol, engine.residualGoals(sol), engine.getOps().table()));   // ISS-2025-0490
            }
            // END_CHANGE: ISS-2025-0476

            SwingUtilities.invokeLater(() -> {
                // Display captured output (from write/1, nl/0, etc.)
                if (!output.isEmpty()) {
                    appendText(output, sideEffectStyle);
                }

                // Display solutions
                if (finalSolutions.isEmpty()) {
                    appendText("false.\n", errorStyle);
                } else if (renderedSolutions.size() == 1 && renderedSolutions.get(0).isEmpty()) {
                    appendText("true.\n", resultStyle);
                } else {
                    displaySolutions(renderedSolutions);
                    if (wasCapped) {
                        appendText("% (stopped after " + CAP + " solutions — more may exist)\n", commentStyle);
                    }
                }

                // START_CHANGE: M05 - Mirror the solutions into the structured results table.
                populateResultsTable(query, finalSolutions);
                // END_CHANGE: M05
                // START_CHANGE: M06 - Reset status to a done summary (solutions + elapsed time).
                finishRunningStatus(finalSolutions.size());
                // END_CHANGE: M06
            });

        } catch (it.denzosoft.jprolog.core.engine.QueryCancelledException ce) {
            // Stop was pressed; interruptQuery() already printed "% Query interrupted." — stay quiet.
            // START_CHANGE: M06 - Clear running status on cancellation.
            SwingUtilities.invokeLater(() -> finishRunningStatus(-1));
            // END_CHANGE: M06
        // START_CHANGE: ISS-2025-0346 - halt/0-halt/1 ends the run session gracefully (the IDE is
        // the processor's host: report the exit code instead of killing the whole IDE JVM).
        } catch (it.denzosoft.jprolog.core.exceptions.PrologException pe) {
            if (pe.isHalt()) {
                final int exitCode = pe.getExitCode();
                SwingUtilities.invokeLater(() -> {
                    appendText("% halt: query session ended (exit code " + exitCode + ").\n", commentStyle);
                    finishRunningStatus(-1);
                });
                return;
            }
            SwingUtilities.invokeLater(() -> {
                appendText("ERROR: " + pe.getMessage() + "\n", errorStyle);
                appendText("false.\n", errorStyle);
                finishRunningStatus(-1);
            });
        // END_CHANGE: ISS-2025-0346
        } catch (Exception e) {
            if (e.getCause() instanceof it.denzosoft.jprolog.core.engine.QueryCancelledException) {
                // START_CHANGE: M06 - Clear running status on (wrapped) cancellation.
                SwingUtilities.invokeLater(() -> finishRunningStatus(-1));
                // END_CHANGE: M06
                return;
            }
            SwingUtilities.invokeLater(() -> {
                appendText("ERROR: " + e.getMessage() + "\n", errorStyle);
                appendText("false.\n", errorStyle);
                // START_CHANGE: M06 - Clear running status on error.
                finishRunningStatus(-1);
                // END_CHANGE: M06
            });
        }
    }
    
    /**
     * Display query solutions interactively.
     */
    private void displaySolutions(List<List<String>> solutions) {
        for (int i = 0; i < solutions.size(); i++) {
            List<String> lines = solutions.get(i);

            if (lines.isEmpty()) {
                appendText("true", resultStyle);
            } else {
                for (int k = 0; k < lines.size(); k++) {
                    if (k > 0) appendText(",\n", resultStyle);
                    appendText(lines.get(k), resultStyle);
                }
            }

            if (i < solutions.size() - 1) {
                appendText(" ;\n", resultStyle);
            } else {
                appendText(".\n", resultStyle);
            }
        }
    }
    
    /**
     * Clear the terminal.
     */
    private void clearTerminal() {
        terminalArea.setText("");
        initializeTerminal();
    }
    
    /**
     * Restart Prolog engine.
     */
    private void restartProlog() {
        appendText("\n% Restarting Prolog engine...\n", commentStyle);
        
        // Trigger recompilation
        ide.compileProject();
        
        clearTerminal();
    }
    
    /**
     * Interrupt current query.
     */
    private void interruptQuery() {
        if (isProcessingQuery && queryThread != null) {
            queryThread.interrupt();
            isProcessingQuery = false;
            appendText("\n% Query interrupted.\n", errorStyle);
            // START_CHANGE: M06 - Stop the running timer/status when the user interrupts.
            finishRunningStatus(-1);
            // END_CHANGE: M06
            showPrompt();
        }
    }
    
    /**
     * Append text with specific style.
     */
    private void appendText(String text, Style style) {
        try {
            document.insertString(document.getLength(), text, style);
            terminalArea.setCaretPosition(document.getLength());
        } catch (BadLocationException e) {
            // Ignore
        }
        // START_CHANGE: CONSOLE_SEARCH - Keep search results in sync when output grows while find is open.
        refreshFindIfVisible();
        // END_CHANGE: CONSOLE_SEARCH
    }
    
    /**
     * Called when knowledge base is reloaded.
     */
    public void onKnowledgeBaseReloaded() {
        if (!waitingForInput) {
            appendText("\n", normalStyle);
        }
        appendText("% Knowledge base reloaded from project compilation.\n", commentStyle);
        showKnowledgeBaseStatus();
        if (!waitingForInput) {
            showPrompt();
        }
        
        // Ensure terminal has focus and prompt is ready
        SwingUtilities.invokeLater(() -> {
            terminalArea.requestFocusInWindow();
            if (waitingForInput) {
                terminalArea.setCaretPosition(document.getLength());
            }
        });
    }
    
    /**
     * Reset the console after compilation.
     */
    public void reset() {
        clearTerminal();
    }
    
    /**
     * Focus the terminal.
     */
    public void focusPrompt() {
        SwingUtilities.invokeLater(() -> {
            terminalArea.requestFocusInWindow();
            if (waitingForInput) {
                terminalArea.setCaretPosition(document.getLength());
            }
        });
    }
    
    /**
     * Get the output text (for BottomTabbedPane compatibility).
     */
    public String getOutputText() {
        try {
            return document.getText(0, document.getLength());
        } catch (BadLocationException e) {
            return "";
        }
    }
}