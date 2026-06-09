package it.denzosoft.jprolog.editor;

import it.denzosoft.jprolog.core.engine.Prolog.CompilationError;
import it.denzosoft.jprolog.core.engine.Prolog.CompilationResult;
import it.denzosoft.jprolog.editor.util.DialogUtils;

import javax.swing.*;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableColumn;
import javax.swing.text.*;
import java.awt.*;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

/**
 * Panel dedicated to build/compilation results.
 * Clears itself on each new build and shows detailed outcome.
 *
 * <p>In addition to the free-text transcript (preserved unchanged), the panel
 * exposes a structured, clickable "Problems" table (severity, message, file,
 * line) populated from {@link CompilationError} rows. Double-clicking a row
 * (or pressing Enter on it) opens the corresponding source file in the editor
 * and jumps to the offending line.</p>
 */
// START_CHANGE: HIGH - Clickable Problems view (jump-to-source from build diagnostics)
public class BuildPanel extends JPanel {

    private JTextPane textPane;
    private StyledDocument document;
    private Style normalStyle;
    private Style errorStyle;
    private Style successStyle;
    private Style headerStyle;
    private Style warningStyle;

    // START_CHANGE: HIGH - structured problems list + jump-to-source
    /** Backing model for the structured problems table. */
    private ProblemsTableModel problemsModel;
    /** Table that renders the clickable build diagnostics. */
    private JTable problemsTable;
    /** Scroll pane wrapping the problems table (so we can show/hide it). */
    private JScrollPane problemsScroll;
    /** Split pane keeping the transcript above and the problems table below. */
    private JSplitPane splitPane;
    /**
     * Reference to the IDE, used to open files and jump to lines.
     * Optional: when {@code null} the problems table is still shown but
     * double-click/Enter only reports that no editor is available. The
     * integrator should call {@link #setIde(PrologIDE)} after construction.
     */
    private PrologIDE ide;
    // END_CHANGE: HIGH

    // Colors for styles
    private static final Color ERROR_COLOR = new Color(220, 20, 60);
    private static final Color SUCCESS_COLOR = new Color(0, 128, 0);
    private static final Color WARNING_COLOR = new Color(255, 140, 0);
    private static final Color HEADER_COLOR = new Color(0, 0, 139);

    public BuildPanel() {
        initializeComponents();
        setupStyles();
    }

    // START_CHANGE: HIGH - allow the IDE to wire itself for jump-to-source
    /**
     * Convenience constructor that also wires the owning IDE so the problems
     * table can open files / jump to lines. The existing no-arg constructor is
     * preserved for backward compatibility.
     */
    public BuildPanel(PrologIDE ide) {
        this();
        this.ide = ide;
    }

    /**
     * Sets the owning IDE used by the problems table to open files and jump to
     * the offending line. Safe to call at any time (may be {@code null}).
     */
    public void setIde(PrologIDE ide) {
        this.ide = ide;
    }
    // END_CHANGE: HIGH

    /**
     * Initializes panel components.
     */
    private void initializeComponents() {
        setLayout(new BorderLayout());

        // Text pane with styles
        textPane = new JTextPane();
        textPane.setEditable(false);
        textPane.setFont(new Font("Consolas", Font.PLAIN, 12));
        textPane.setBackground(Color.WHITE);

        document = textPane.getStyledDocument();

        // Scroll pane
        JScrollPane scrollPane = new JScrollPane(textPane);
        scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        // START_CHANGE: HIGH - structured, clickable problems table below the transcript
        initializeProblemsTable();

        // Keep the transcript on top and the problems list on the bottom.
        splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, scrollPane, problemsScroll);
        splitPane.setResizeWeight(0.6); // transcript gets more room by default
        splitPane.setOneTouchExpandable(true);
        splitPane.setContinuousLayout(true);
        splitPane.setBorder(null);

        add(splitPane, BorderLayout.CENTER);
        // END_CHANGE: HIGH

        // Context menu
        setupContextMenu();
    }

    // START_CHANGE: HIGH - build the clickable problems table
    /**
     * Builds the structured problems table (severity, message, file, line) and
     * installs the double-click / Enter handlers that jump to source.
     */
    private void initializeProblemsTable() {
        problemsModel = new ProblemsTableModel();
        problemsTable = new JTable(problemsModel);
        problemsTable.setFont(new Font("Consolas", Font.PLAIN, 12));
        problemsTable.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        problemsTable.setRowHeight(20);
        problemsTable.setFillsViewportHeight(true);
        problemsTable.setAutoResizeMode(JTable.AUTO_RESIZE_LAST_COLUMN);
        problemsTable.getTableHeader().setReorderingAllowed(false);

        // Severity column: compact + colour coded.
        DefaultTableCellRenderer severityRenderer = new DefaultTableCellRenderer() {
            @Override
            public Component getTableCellRendererComponent(JTable table, Object value,
                    boolean isSelected, boolean hasFocus, int row, int column) {
                Component c = super.getTableCellRendererComponent(table, value, isSelected,
                        hasFocus, row, column);
                if (!isSelected) {
                    String sev = value == null ? "" : value.toString();
                    if ("warning".equalsIgnoreCase(sev)) {
                        c.setForeground(WARNING_COLOR);
                    } else {
                        c.setForeground(ERROR_COLOR);
                    }
                }
                return c;
            }
        };

        TableColumn severityCol = problemsTable.getColumnModel().getColumn(ProblemsTableModel.COL_SEVERITY);
        severityCol.setCellRenderer(severityRenderer);
        severityCol.setPreferredWidth(70);
        severityCol.setMaxWidth(110);

        TableColumn fileCol = problemsTable.getColumnModel().getColumn(ProblemsTableModel.COL_FILE);
        fileCol.setPreferredWidth(160);

        TableColumn lineCol = problemsTable.getColumnModel().getColumn(ProblemsTableModel.COL_LINE);
        lineCol.setPreferredWidth(50);
        lineCol.setMaxWidth(80);

        TableColumn messageCol = problemsTable.getColumnModel().getColumn(ProblemsTableModel.COL_MESSAGE);
        messageCol.setPreferredWidth(400);

        // Double-click jumps to source.
        problemsTable.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                if (e.getClickCount() == 2 && SwingUtilities.isLeftMouseButton(e)) {
                    int viewRow = problemsTable.rowAtPoint(e.getPoint());
                    if (viewRow >= 0) {
                        openProblem(viewRow);
                    }
                }
            }
        });

        // Enter key on the selected row jumps to source.
        problemsTable.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_ENTER) {
                    int viewRow = problemsTable.getSelectedRow();
                    if (viewRow >= 0) {
                        openProblem(viewRow);
                        e.consume();
                    }
                }
            }
        });

        problemsScroll = new JScrollPane(problemsTable);
        problemsScroll.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        problemsScroll.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        JTableHeader header = problemsTable.getTableHeader();
        if (header != null) {
            problemsScroll.setColumnHeaderView(header);
        }
    }

    /**
     * Opens the source file for the problem at the given (view) row and jumps
     * to its line. Uses the IDE's editor tabs; no-op (with status note) when no
     * IDE / no file / no valid line is available.
     */
    private void openProblem(int viewRow) {
        if (viewRow < 0 || viewRow >= problemsModel.getRowCount()) {
            return;
        }
        CompilationError error = problemsModel.getError(viewRow);
        if (error == null) {
            return;
        }
        if (ide == null) {
            // No editor wired in: still give the user feedback rather than
            // silently doing nothing.
            DialogUtils.showCenteredMessage(this,
                    "Cannot jump to source: no editor available.\n"
                        + error.file + ":" + error.lineNumber,
                    "Open Problem", JOptionPane.INFORMATION_MESSAGE);
            return;
        }
        if (error.file == null || error.file.trim().isEmpty()) {
            return;
        }
        try {
            File file = new File(error.file);
            EditorTabbedPane tabs = ide.getEditorTabs();
            if (tabs == null) {
                return;
            }
            // openFile selects the (existing or newly created) tab synchronously,
            // so getCurrentEditor() afterwards refers to that file's editor.
            tabs.openFile(file);
            FileEditor editor = tabs.getCurrentEditor();
            if (editor != null && error.lineNumber > 0) {
                editor.goToLine(error.lineNumber);
            }
        } catch (Exception ex) {
            DialogUtils.showError(this,
                    "Error opening problem: " + ex.getMessage(),
                    "Error");
        }
    }
    // END_CHANGE: HIGH

    /**
     * Configures text styles.
     */
    private void setupStyles() {
        // Normal style
        normalStyle = textPane.addStyle("normal", null);
        StyleConstants.setForeground(normalStyle, Color.BLACK);
        StyleConstants.setFontFamily(normalStyle, "Consolas");
        StyleConstants.setFontSize(normalStyle, 12);

        // Error style
        errorStyle = textPane.addStyle("error", normalStyle);
        StyleConstants.setForeground(errorStyle, ERROR_COLOR);
        StyleConstants.setBold(errorStyle, true);

        // Success style
        successStyle = textPane.addStyle("success", normalStyle);
        StyleConstants.setForeground(successStyle, SUCCESS_COLOR);
        StyleConstants.setBold(successStyle, true);

        // Header style
        headerStyle = textPane.addStyle("header", normalStyle);
        StyleConstants.setForeground(headerStyle, HEADER_COLOR);
        StyleConstants.setBold(headerStyle, true);
        StyleConstants.setFontSize(headerStyle, 14);

        // Warning style
        warningStyle = textPane.addStyle("warning", normalStyle);
        StyleConstants.setForeground(warningStyle, WARNING_COLOR);
        StyleConstants.setBold(warningStyle, true);
    }

    /**
     * Configures the context menu.
     */
    private void setupContextMenu() {
        JPopupMenu contextMenu = new JPopupMenu();

        JMenuItem clearItem = new JMenuItem("Clear Build");
        clearItem.addActionListener(e -> clearBuild());

        JMenuItem copyItem = new JMenuItem("Copy All");
        copyItem.addActionListener(e -> copyAll());

        JMenuItem saveItem = new JMenuItem("Save Build Log...");
        saveItem.addActionListener(e -> saveBuildLog());

        contextMenu.add(clearItem);
        contextMenu.addSeparator();
        contextMenu.add(copyItem);
        contextMenu.add(saveItem);

        textPane.setComponentPopupMenu(contextMenu);
    }

    /**
     * Starts a new build (clears the panel).
     */
    public void startBuild(String buildType) {
        textPane.setText("");
        // START_CHANGE: HIGH - also reset the structured problems list per build
        clearProblems();
        // END_CHANGE: HIGH
        String timestamp = LocalDateTime.now().format(DateTimeFormatter.ofPattern("HH:mm:ss"));
        appendText("=== " + buildType + " - " + timestamp + " ===\n", headerStyle);
    }

    /**
     * Adds normal text.
     */
    public void appendText(String text) {
        appendText(text, normalStyle);
    }

    /**
     * Adds error text.
     */
    public void appendError(String text) {
        appendText(text, errorStyle);
    }

    /**
     * Adds success text.
     */
    public void appendSuccess(String text) {
        appendText(text, successStyle);
    }

    /**
     * Adds warning text.
     */
    public void appendWarning(String text) {
        appendText(text, warningStyle);
    }

    /**
     * Adds text with header.
     */
    public void appendHeader(String text) {
        appendText(text, headerStyle);
    }

    /**
     * Finishes the build with the result.
     */
    public void finishBuild(boolean success) {
        String timestamp = LocalDateTime.now().format(DateTimeFormatter.ofPattern("HH:mm:ss"));
        if (success) {
            appendSuccess("Build completed successfully - " + timestamp + "\n");
        } else {
            appendError("Build completed with errors - " + timestamp + "\n");
        }
        appendText("=== Build Complete ===\n\n", headerStyle);

        // Scroll to the end
        scrollToBottom();
    }

    // START_CHANGE: HIGH - structured problems API (populated from CompilationError/Result)
    /**
     * Replaces the structured problems list with the given compilation errors.
     * The text transcript is left untouched. Each entry becomes a clickable row
     * (severity, message, file, line). Safe to call off the EDT.
     */
    public void setProblems(List<CompilationError> errors) {
        Runnable task = () -> {
            problemsModel.setErrors(errors);
            refreshProblemsVisibility();
        };
        if (SwingUtilities.isEventDispatchThread()) {
            task.run();
        } else {
            SwingUtilities.invokeLater(task);
        }
    }

    /**
     * Appends a single problem row to the structured list.
     */
    public void addProblem(CompilationError error) {
        if (error == null) {
            return;
        }
        Runnable task = () -> {
            problemsModel.addError(error);
            refreshProblemsVisibility();
        };
        if (SwingUtilities.isEventDispatchThread()) {
            task.run();
        } else {
            SwingUtilities.invokeLater(task);
        }
    }

    /**
     * Clears all structured problems (the transcript is untouched).
     */
    public void clearProblems() {
        Runnable task = () -> {
            problemsModel.clear();
            refreshProblemsVisibility();
        };
        if (SwingUtilities.isEventDispatchThread()) {
            task.run();
        } else {
            SwingUtilities.invokeLater(task);
        }
    }

    /**
     * Convenience helper: populates the structured problems table directly from
     * a {@link CompilationResult}. Does not alter the text transcript, so it can
     * be called alongside the existing append* calls.
     */
    public void setCompilationResult(CompilationResult result) {
        if (result == null) {
            clearProblems();
        } else {
            setProblems(result.errors);
        }
    }

    /**
     * Number of structured problems currently displayed.
     */
    public int getProblemCount() {
        return problemsModel.getRowCount();
    }

    /**
     * Shows the problems table only when there is at least one problem, so the
     * transcript keeps the full panel when there is nothing to jump to.
     */
    private void refreshProblemsVisibility() {
        boolean hasProblems = problemsModel.getRowCount() > 0;
        problemsScroll.setVisible(hasProblems);
        if (hasProblems) {
            // Give the problems table a sensible share of the space.
            splitPane.setDividerSize(8);
            int h = splitPane.getHeight();
            if (h > 0) {
                splitPane.setDividerLocation(Math.max(0, (int) (h * 0.6)));
            }
        } else {
            splitPane.setDividerSize(0);
        }
        splitPane.revalidate();
        splitPane.repaint();
    }
    // END_CHANGE: HIGH

    /**
     * Adds text with a specific style.
     */
    private void appendText(String text, Style style) {
        try {
            document.insertString(document.getLength(), text, style);
        } catch (BadLocationException e) {
            // Ignore
        }
    }

    /**
     * Scroll down.
     */
    private void scrollToBottom() {
        SwingUtilities.invokeLater(() -> {
            textPane.setCaretPosition(document.getLength());
        });
    }

    /**
     * Clears the build panel.
     */
    private void clearBuild() {
        textPane.setText("");
        // START_CHANGE: HIGH - clearing the build also clears the problems list
        clearProblems();
        // END_CHANGE: HIGH
    }

    /**
     * Copies all content.
     */
    private void copyAll() {
        textPane.selectAll();
        textPane.copy();
        textPane.setCaretPosition(document.getLength());
    }

    /**
     * Saves the build log.
     */
    private void saveBuildLog() {
        JFileChooser chooser = new JFileChooser();
        chooser.setDialogTitle("Save Build Log");
        chooser.setSelectedFile(new java.io.File("build.log"));

        if (chooser.showSaveDialog(this) == JFileChooser.APPROVE_OPTION) {
            try (java.io.FileWriter writer = new java.io.FileWriter(chooser.getSelectedFile())) {
                writer.write(textPane.getText());
                DialogUtils.showCenteredMessage(this,
                    "Build log saved: " + chooser.getSelectedFile().getName(),
                    "Save", JOptionPane.INFORMATION_MESSAGE);
            } catch (Exception ex) {
                DialogUtils.showError(this,
                    "Error saving: " + ex.getMessage(),
                    "Error");
            }
        }
    }

    /**
     * Gets the panel content.
     */
    public String getText() {
        return textPane.getText();
    }

    /**
     * Checks if the panel is empty.
     */
    public boolean isEmpty() {
        return textPane.getText().trim().isEmpty();
    }

    // START_CHANGE: HIGH - table model backing the structured problems list
    /**
     * Table model for the structured (clickable) problems list. Columns:
     * severity, message, file (base name), line. The full {@link CompilationError}
     * is retained per row so the jump-to-source handler has the absolute path.
     */
    private static final class ProblemsTableModel extends AbstractTableModel {

        static final int COL_SEVERITY = 0;
        static final int COL_MESSAGE = 1;
        static final int COL_FILE = 2;
        static final int COL_LINE = 3;

        private final String[] columnNames = { "Severity", "Message", "File", "Line" };
        private final List<CompilationError> rows = new ArrayList<>();

        void setErrors(List<CompilationError> errors) {
            rows.clear();
            if (errors != null) {
                for (CompilationError e : errors) {
                    if (e != null) {
                        rows.add(e);
                    }
                }
            }
            fireTableDataChanged();
        }

        void addError(CompilationError error) {
            if (error != null) {
                rows.add(error);
                int idx = rows.size() - 1;
                fireTableRowsInserted(idx, idx);
            }
        }

        void clear() {
            if (!rows.isEmpty()) {
                rows.clear();
                fireTableDataChanged();
            }
        }

        CompilationError getError(int row) {
            if (row < 0 || row >= rows.size()) {
                return null;
            }
            return rows.get(row);
        }

        @Override
        public int getRowCount() {
            return rows.size();
        }

        @Override
        public int getColumnCount() {
            return columnNames.length;
        }

        @Override
        public String getColumnName(int column) {
            return columnNames[column];
        }

        @Override
        public boolean isCellEditable(int rowIndex, int columnIndex) {
            return false;
        }

        @Override
        public Object getValueAt(int rowIndex, int columnIndex) {
            CompilationError e = rows.get(rowIndex);
            switch (columnIndex) {
                case COL_SEVERITY:
                    return e.severity == null ? "error" : e.severity;
                case COL_MESSAGE:
                    return e.message == null ? "" : e.message;
                case COL_FILE:
                    return baseName(e.file);
                case COL_LINE:
                    return e.lineNumber > 0 ? Integer.valueOf(e.lineNumber) : "";
                default:
                    return "";
            }
        }

        private static String baseName(String path) {
            if (path == null || path.isEmpty()) {
                return "";
            }
            return new File(path).getName();
        }
    }
    // END_CHANGE: HIGH
}
// END_CHANGE: HIGH
