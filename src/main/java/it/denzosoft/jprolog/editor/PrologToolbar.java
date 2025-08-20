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
    
    // Pulsanti della toolbar
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
    
    // Indicatori di stato
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
     * Crea tutti i pulsanti della toolbar.
     */
    private void createButtons() {
        // Gruppo File
        newProjectBtn = createButton("New Project", "Create a new Prolog project", null);
        openProjectBtn = createButton("Open Project", "Open an existing project", null);
        newFileBtn = createButton("New File", "Create a new Prolog file", null);
        saveFileBtn = createButton("Save", "Save current file", KeyStroke.getKeyStroke("ctrl S"));
        saveAllBtn = createButton("Save All", "Save all modified files", null);
        
        // Gruppo Prolog
        compileFileBtn = createButton("Compile File", "Compile current file", KeyStroke.getKeyStroke("F9"));
        compileProjectBtn = createButton("Compile Project", "Compile all project files", KeyStroke.getKeyStroke("ctrl F9"));
        runQueryBtn = createButton("Run Query", "Open query execution window", KeyStroke.getKeyStroke("F5"));
        traceBtn = createButton("Trace", "Toggle trace mode", KeyStroke.getKeyStroke("F8"));
        debugBtn = createButton("Debug", "Start debugger", null);
        
        // Gruppo Ricerca
        findBtn = createButton("Find", "Find text in current file", KeyStroke.getKeyStroke("ctrl F"));
        findInProjectBtn = createButton("Find in Project", "Search in all project files", KeyStroke.getKeyStroke("ctrl shift F"));
        
        // Gruppo Help
        aboutBtn = createButton("?", "About the IDE", null);
        
        // Indicatori di stato
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
        
        // Stile del pulsante
        button.setBorderPainted(true);
        button.setContentAreaFilled(true);
        
        return button;
    }
    
    /**
     * Organizza i pulsanti nella toolbar.
     */
    private void layoutButtons() {
        // Gruppo File
        add(newProjectBtn);
        add(openProjectBtn);
        addSeparator();
        add(newFileBtn);
        add(saveFileBtn);
        add(saveAllBtn);
        
        addSeparator();
        
        // Gruppo Prolog
        add(compileFileBtn);
        add(compileProjectBtn);
        addSeparator();
        add(runQueryBtn);
        add(traceBtn);
        add(debugBtn);
        
        addSeparator();
        
        // Gruppo Ricerca
        add(findBtn);
        add(findInProjectBtn);
        
        // Spaziatore per spingere gli indicatori a destra
        add(Box.createHorizontalGlue());
        
        // Indicatori di stato
        add(new JLabel("Status: "));
        add(statusIndicator);
        add(Box.createHorizontalStrut(15));
        add(traceIndicator);
        add(Box.createHorizontalStrut(15));
        add(aboutBtn);
    }
    
    /**
     * Configura gli event handlers per tutti i pulsanti.
     */
    private void setupEventHandlers() {
        // Gruppo File
        newProjectBtn.addActionListener(e -> ide.createNewProject());
        openProjectBtn.addActionListener(e -> ide.openProject());
        newFileBtn.addActionListener(e -> ide.createNewFile());
        saveFileBtn.addActionListener(e -> ide.saveCurrentFile());
        saveAllBtn.addActionListener(e -> ide.saveAllFiles());
        
        // Gruppo Prolog
        compileFileBtn.addActionListener(e -> ide.compileCurrentFile());
        compileProjectBtn.addActionListener(e -> ide.compileProject());
        runQueryBtn.addActionListener(e -> ide.showQueryDialog());
        traceBtn.addActionListener(e -> toggleTrace());
        debugBtn.addActionListener(e -> startDebugger());
        
        // Gruppo Ricerca
        findBtn.addActionListener(e -> ide.showSearchPanel());
        findInProjectBtn.addActionListener(e -> ide.showProjectSearch());
        
        // Help
        aboutBtn.addActionListener(e -> ide.showAboutDialog());
    }
    
    /**
     * Attiva/disattiva il trace.
     */
    private void toggleTrace() {
        // Per ora solo indicazione visiva
        boolean traceEnabled = "Trace: OFF".equals(traceIndicator.getText());
        
        if (traceEnabled) {
            traceIndicator.setText("Trace: ON");
            traceIndicator.setForeground(new Color(255, 140, 0)); // Arancione
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
     * Avvia il debugger.
     */
    private void startDebugger() {
        // Implementazione futura del debugger
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
     * Aggiorna lo stato della toolbar in base al contesto.
     */
    public void updateToolbarState() {
        boolean hasProject = ide.getCurrentProjectRoot() != null;
        boolean hasCurrentFile = ide.getEditorTabs().getCurrentEditor() != null;
        boolean hasOpenFiles = ide.getEditorTabs().getOpenFileCount() > 0;
        boolean hasUnsavedFiles = ide.getEditorTabs().hasUnsavedFiles();
        
        // Abilita/disabilita pulsanti in base al contesto
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
        
        // Aggiorna indicatore di stato del motore Prolog
        if (ide.getPrologEngine() != null) {
            statusIndicator.setForeground(Color.GREEN);
            statusIndicator.setToolTipText("Prolog engine active and ready");
        } else {
            statusIndicator.setForeground(Color.RED);
            statusIndicator.setToolTipText("Prolog engine not available");
        }
    }
    
    /**
     * Imposta il messaggio di stato nell'area degli indicatori.
     */
    public void setStatusMessage(String message) {
        statusIndicator.setToolTipText(message);
    }
    
    /**
     * Ottiene lo stato del trace.
     */
    public boolean isTraceEnabled() {
        return "Trace: ON".equals(traceIndicator.getText());
    }
    
    /**
     * Imposta lo stato del trace programmaticamente.
     */
    public void setTraceEnabled(boolean enabled) {
        if (enabled != isTraceEnabled()) {
            toggleTrace();
        }
    }
    
    /**
     * Evidenzia un pulsante per attirare l'attenzione.
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
     * Mostra un indicatore di progresso sulla toolbar.
     */
    public void showProgress(String operation) {
        // Crea un pannello di progresso temporaneo
        JPanel progressPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 0));
        JProgressBar progressBar = new JProgressBar();
        progressBar.setIndeterminate(true);
        progressBar.setPreferredSize(new Dimension(100, 20));
        
        JLabel progressLabel = new JLabel(operation + "...");
        progressLabel.setFont(progressLabel.getFont().deriveFont(Font.PLAIN, 11f));
        
        progressPanel.add(progressLabel);
        progressPanel.add(progressBar);
        
        // Aggiungi temporaneamente alla toolbar
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