package it.denzosoft.jprolog.editor;

import it.denzosoft.jprolog.editor.util.DialogUtils;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * Main IDE toolbar with buttons for primary functions.
 * Includes buttons for file operations, compilation, query execution,
 * debug and other common features.
 */
public class PrologToolbar extends JToolBar {
    
    private PrologIDE ide;
    
    // Toolbar buttons
    private JButton newProjectBtn;
    private JButton openProjectBtn;
    private JButton newFileBtn;
    private JButton saveFileBtn;
    private JButton saveAllBtn;
    
    private JButton compileFileBtn;
    private JButton compileProjectBtn;
    private JButton runQueryBtn;
    private JButton traceBtn;
    private JButton debugBtn;
    
    private JButton findBtn;
    private JButton findInProjectBtn;
    
    private JButton aboutBtn;
    
    // Status indicators
    private JLabel statusIndicator;
    private JLabel traceIndicator;
    
    public PrologToolbar(PrologIDE ide) {
        this.ide = ide;
        
        setupToolbar();
        createButtons();
        layoutButtons();
        setupEventHandlers();
    }
    
    /**
     * Configures basic toolbar properties.
     */
    private void setupToolbar() {
        setFloatable(false);
        setRollover(true);
        setBorderPainted(true);
        setPreferredSize(new Dimension(0, 40));
    }
    
    /**
     * Creates all toolbar buttons.
     */
    private void createButtons() {
        // File group
        newProjectBtn = createButton("New Project", "Create a new Prolog project", null);
        openProjectBtn = createButton("Open Project", "Open an existing project", null);
        newFileBtn = createButton("New File", "Create a new Prolog file", null);
        saveFileBtn = createButton("Save", "Save current file", KeyStroke.getKeyStroke("ctrl S"));
        saveAllBtn = createButton("Save All", "Save all modified files", null);
        
        // Prolog group
        compileFileBtn = createButton("Compile File", "Compile current file", KeyStroke.getKeyStroke("F9"));
        compileProjectBtn = createButton("Compile Project", "Compile all project files", KeyStroke.getKeyStroke("ctrl F9"));
        runQueryBtn = createButton("Run Query", "Open query execution window", KeyStroke.getKeyStroke("F5"));
        traceBtn = createButton("Trace", "Toggle trace mode", KeyStroke.getKeyStroke("F8"));
        debugBtn = createButton("Debug", "Start debugger", null);
        
        // Search group
        findBtn = createButton("Find", "Find text in current file", KeyStroke.getKeyStroke("ctrl F"));
        findInProjectBtn = createButton("Find in Project", "Search in all project files", KeyStroke.getKeyStroke("ctrl shift F"));
        
        // Help group
        aboutBtn = createButton("?", "About the IDE", null);
        
        // Status indicators
        statusIndicator = new JLabel("●");
        statusIndicator.setForeground(Color.GREEN);
        statusIndicator.setFont(statusIndicator.getFont().deriveFont(Font.BOLD, 16f));
        statusIndicator.setToolTipText("Prolog engine active");
        
        traceIndicator = new JLabel("Trace: OFF");
        traceIndicator.setFont(traceIndicator.getFont().deriveFont(Font.PLAIN, 11f));
        traceIndicator.setForeground(Color.GRAY);
    }
    
    /**
     * Creates a button with specified properties.
     */
    private JButton createButton(String text, String tooltip, KeyStroke shortcut) {
        JButton button = new JButton(text);
        button.setToolTipText(tooltip + (shortcut != null ? " (" + shortcut.toString().replace("pressed ", "") + ")" : ""));
        button.setFocusable(false);
        button.setMargin(new Insets(2, 8, 2, 8));
        
        // Button style
        button.setBorderPainted(true);
        button.setContentAreaFilled(true);
        
        return button;
    }
    
    /**
     * Organizes buttons in the toolbar.
     */
    private void layoutButtons() {
        // File group
        add(newProjectBtn);
        add(openProjectBtn);
        addSeparator();
        add(newFileBtn);
        add(saveFileBtn);
        add(saveAllBtn);
        
        addSeparator();
        
        // Prolog group
        add(compileFileBtn);
        add(compileProjectBtn);
        addSeparator();
        add(runQueryBtn);
        add(traceBtn);
        add(debugBtn);
        
        addSeparator();
        
        // Search group
        add(findBtn);
        add(findInProjectBtn);
        
        // Spacer to push indicators to the right
        add(Box.createHorizontalGlue());
        
        // Status indicators
        add(new JLabel("Status: "));
        add(statusIndicator);
        add(Box.createHorizontalStrut(15));
        add(traceIndicator);
        add(Box.createHorizontalStrut(15));
        add(aboutBtn);
    }
    
    /**
     * Configure event handlers for all buttons.
     */
    private void setupEventHandlers() {
        // File group
        newProjectBtn.addActionListener(e -> ide.createNewProject());
        openProjectBtn.addActionListener(e -> ide.openProject());
        newFileBtn.addActionListener(e -> ide.createNewFile());
        saveFileBtn.addActionListener(e -> ide.saveCurrentFile());
        saveAllBtn.addActionListener(e -> ide.saveAllFiles());
        
        // Prolog group
        compileFileBtn.addActionListener(e -> ide.compileCurrentFile());
        compileProjectBtn.addActionListener(e -> ide.compileProject());
        runQueryBtn.addActionListener(e -> ide.showQueryDialog());
        traceBtn.addActionListener(e -> toggleTrace());
        debugBtn.addActionListener(e -> startDebugger());
        
        // Search group
        findBtn.addActionListener(e -> ide.showSearchPanel());
        findInProjectBtn.addActionListener(e -> ide.showProjectSearch());
        
        // Help
        aboutBtn.addActionListener(e -> ide.showAboutDialog());
    }
    
    /**
     * Toggles trace on/off.
     */
    private void toggleTrace() {
        // For now only visual indication
        boolean traceEnabled = "Trace: OFF".equals(traceIndicator.getText());
        
        if (traceEnabled) {
            traceIndicator.setText("Trace: ON");
            traceIndicator.setForeground(new Color(255, 140, 0)); // Orange
            traceBtn.setBackground(new Color(255, 240, 200));
            ide.getBottomTabbedPane().appendToOutput("Trace enabled.\n");
        } else {
            traceIndicator.setText("Trace: OFF");
            traceIndicator.setForeground(Color.GRAY);
            traceBtn.setBackground(null);
            ide.getBottomTabbedPane().appendToOutput("Trace disabled.\n");
        }
        
        traceBtn.setContentAreaFilled(traceEnabled);
    }
    
    /**
     * Starts the debugger.
     */
    private void startDebugger() {
        // Future debugger implementation
        DialogUtils.showCenteredMessage(ide, 
            "Debugger functionality under development.\n\n" +
            "Planned features:\n" +
            "• Breakpoints\n" +
            "• Step-by-step execution\n" +
            "• Variable watching\n" +
            "• Call stack visualization",
            "Debugger", JOptionPane.INFORMATION_MESSAGE);
    }
    
    /**
     * Updates the toolbar state based on context.
     */
    public void updateToolbarState() {
        boolean hasProject = ide.getCurrentProjectRoot() != null;
        boolean hasCurrentFile = ide.getEditorTabs().getCurrentEditor() != null;
        boolean hasOpenFiles = ide.getEditorTabs().getOpenFileCount() > 0;
        boolean hasUnsavedFiles = ide.getEditorTabs().hasUnsavedFiles();
        
        // Enable/disable buttons based on context
        newFileBtn.setEnabled(hasProject);
        saveFileBtn.setEnabled(hasCurrentFile && hasUnsavedFiles);
        saveAllBtn.setEnabled(hasUnsavedFiles);
        
        compileFileBtn.setEnabled(hasCurrentFile);
        compileProjectBtn.setEnabled(hasProject);
        runQueryBtn.setEnabled(hasProject);
        traceBtn.setEnabled(hasProject);
        debugBtn.setEnabled(hasProject);
        
        findBtn.setEnabled(hasCurrentFile);
        findInProjectBtn.setEnabled(hasProject);
        
        // Update Prolog engine status indicator
        if (ide.getPrologEngine() != null) {
            statusIndicator.setForeground(Color.GREEN);
            statusIndicator.setToolTipText("Prolog engine active and ready");
        } else {
            statusIndicator.setForeground(Color.RED);
            statusIndicator.setToolTipText("Prolog engine not available");
        }
    }
    
    /**
     * Sets the status message in the indicators area.
     */
    public void setStatusMessage(String message) {
        statusIndicator.setToolTipText(message);
    }
    
    /**
     * Gets the trace state.
     */
    public boolean isTraceEnabled() {
        return "Trace: ON".equals(traceIndicator.getText());
    }
    
    /**
     * Sets the trace state programmatically.
     */
    public void setTraceEnabled(boolean enabled) {
        if (enabled != isTraceEnabled()) {
            toggleTrace();
        }
    }
    
    /**
     * Highlights a button to attract attention.
     */
    public void highlightButton(JButton button, boolean highlight) {
        if (highlight) {
            button.setBackground(new Color(255, 255, 150));
            button.setContentAreaFilled(true);
        } else {
            button.setBackground(null);
            button.setContentAreaFilled(false);
        }
    }
    
    /**
     * Show a progress indicator on the toolbar.
     */
    public void showProgress(String operation) {
        // Create a temporary progress panel
        JPanel progressPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 0));
        JProgressBar progressBar = new JProgressBar();
        progressBar.setIndeterminate(true);
        progressBar.setPreferredSize(new Dimension(100, 20));
        
        JLabel progressLabel = new JLabel(operation + "...");
        progressLabel.setFont(progressLabel.getFont().deriveFont(Font.PLAIN, 11f));
        
        progressPanel.add(progressLabel);
        progressPanel.add(progressBar);
        
        // Add temporarily to toolbar
        add(progressPanel);
        revalidate();
        repaint();
        
        // Remove after a while (in real implementation would be controlled by operation)
        Timer timer = new Timer(3000, e -> {
            remove(progressPanel);
            revalidate();
            repaint();
        });
        timer.setRepeats(false);
        timer.start();
    }
    
    // ===================== GETTERS =====================
    
    public JButton getNewProjectButton() { return newProjectBtn; }
    public JButton getOpenProjectButton() { return openProjectBtn; }
    public JButton getNewFileButton() { return newFileBtn; }
    public JButton getSaveFileButton() { return saveFileBtn; }
    public JButton getSaveAllButton() { return saveAllBtn; }
    public JButton getCompileFileButton() { return compileFileBtn; }
    public JButton getCompileProjectButton() { return compileProjectBtn; }
    public JButton getRunQueryButton() { return runQueryBtn; }
    public JButton getTraceButton() { return traceBtn; }
    public JButton getDebugButton() { return debugBtn; }
    public JButton getFindButton() { return findBtn; }
    public JButton getFindInProjectButton() { return findInProjectBtn; }
}