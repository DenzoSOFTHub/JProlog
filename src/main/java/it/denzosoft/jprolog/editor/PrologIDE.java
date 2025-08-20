package it.denzosoft.jprolog.editor;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.editor.util.DialogUtils;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.*;
import java.nio.file.*;
import java.util.*;

/**
 * Complete IDE for Prolog project development.
 * 
 * Features:
 * - Project management with directory tree
 * - Multi-tab editor with line numbering
 * - Compilation with error highlighting
 * - Query execution and debugging
 * - Advanced search system
 */
public class PrologIDE extends JFrame {
    
    // Main components
    private ProjectTree projectTree;
    private EditorTabbedPane editorTabs;
    private PrologToolbar toolbar;
    private SearchPanel searchPanel;
    private BottomTabbedPane bottomTabbedPane;
    private StatusBar statusBar;
    private PredicatePanel predicatePanel;
    
    // Prolog engine
    private Prolog prologEngine;
    
    // Current project
    private File currentProjectRoot;
    private String currentProjectName;
    
    // Debug state
    private boolean debugMode = false;
    
    // Configuration
    private Properties config;
    private File configFile;
    
    public PrologIDE() {
        super("JProlog Editor from DenzoSOFT");
        initializeComponents();
        setupLayout();
        setupMenuBar();
        setupEventHandlers();
        loadConfiguration();
        
        setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
        setExtendedState(JFrame.MAXIMIZED_BOTH);
        setLocationRelativeTo(null);
        
        // Initialize Prolog engine
        prologEngine = new Prolog();
        
        statusBar.setMessage("IDE started - Open or create a project to begin");
    }
    
    /**
     * Initializes all IDE components.
     */
    private void initializeComponents() {
        projectTree = new ProjectTree(this);
        editorTabs = new EditorTabbedPane(this);
        toolbar = new PrologToolbar(this);
        searchPanel = new SearchPanel(this);
        bottomTabbedPane = new BottomTabbedPane(this);
        statusBar = new StatusBar();
        predicatePanel = new PredicatePanel(this);
        
        searchPanel.setVisible(false); // Inizialmente nascosto
    }
    
    /**
     * Configura il layout principale dell'IDE.
     */
    private void setupLayout() {
        setLayout(new BorderLayout());
        
        // Toolbar in alto
        add(toolbar, BorderLayout.NORTH);
        
        // Pannello principale con split pane
        JSplitPane mainSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
        mainSplitPane.setDividerLocation(250);
        mainSplitPane.setResizeWeight(0.2);
        
        // Pannello sinistro: albero progetto + pannello predicati
        JSplitPane leftSplitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
        leftSplitPane.setResizeWeight(0.6); // 60% per albero progetto, 40% per predicati
        
        // Pannello albero progetto
        JPanel projectPanel = new JPanel(new BorderLayout());
        projectPanel.add(new JLabel("Project", SwingConstants.CENTER), BorderLayout.NORTH);
        projectPanel.add(new JScrollPane(projectTree), BorderLayout.CENTER);
        projectPanel.setMinimumSize(new Dimension(200, 150));
        leftSplitPane.setTopComponent(projectPanel);
        
        // Pannello predicati
        predicatePanel.setMinimumSize(new Dimension(200, 100));
        leftSplitPane.setBottomComponent(predicatePanel);
        
        leftSplitPane.setMinimumSize(new Dimension(200, 0));
        mainSplitPane.setLeftComponent(leftSplitPane);
        
        // Pannello destro: editor e console
        JSplitPane rightSplitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
        rightSplitPane.setResizeWeight(0.7);
        
        // Pannello editor con ricerca
        JPanel editorPanel = new JPanel(new BorderLayout());
        editorPanel.add(searchPanel, BorderLayout.NORTH);
        editorPanel.add(editorTabs, BorderLayout.CENTER);
        rightSplitPane.setTopComponent(editorPanel);
        
        // Pannello a tab in basso (Output, Build, Run, Search)
        rightSplitPane.setBottomComponent(bottomTabbedPane);
        
        mainSplitPane.setRightComponent(rightSplitPane);
        add(mainSplitPane, BorderLayout.CENTER);
        
        // Status bar in basso
        add(statusBar, BorderLayout.SOUTH);
    }
    
    /**
     * Configura la barra dei menu.
     */
    private void setupMenuBar() {
        JMenuBar menuBar = new JMenuBar();
        
        // Menu File
        JMenu fileMenu = new JMenu("File");
        fileMenu.setMnemonic(KeyEvent.VK_F);
        
        JMenuItem newProject = new JMenuItem("New Project...", KeyEvent.VK_N);
        newProject.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N, ActionEvent.CTRL_MASK));
        newProject.addActionListener(e -> createNewProject());
        
        JMenuItem openProject = new JMenuItem("Open Project...", KeyEvent.VK_O);
        openProject.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_O, ActionEvent.CTRL_MASK));
        openProject.addActionListener(e -> openProject());
        
        JMenuItem newFile = new JMenuItem("New File...", KeyEvent.VK_F);
        newFile.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N, ActionEvent.CTRL_MASK | ActionEvent.SHIFT_MASK));
        newFile.addActionListener(e -> createNewFile());
        
        JMenuItem saveFile = new JMenuItem("Save", KeyEvent.VK_S);
        saveFile.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S, ActionEvent.CTRL_MASK));
        saveFile.addActionListener(e -> saveCurrentFile());
        
        JMenuItem saveAllFiles = new JMenuItem("Save All", KeyEvent.VK_A);
        saveAllFiles.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_S, ActionEvent.CTRL_MASK | ActionEvent.SHIFT_MASK));
        saveAllFiles.addActionListener(e -> saveAllFiles());
        
        fileMenu.add(newProject);
        fileMenu.add(openProject);
        fileMenu.addSeparator();
        fileMenu.add(newFile);
        fileMenu.addSeparator();
        fileMenu.add(saveFile);
        fileMenu.add(saveAllFiles);
        fileMenu.addSeparator();
        
        JMenuItem exit = new JMenuItem("Exit", KeyEvent.VK_X);
        exit.addActionListener(e -> exitApplication());
        fileMenu.add(exit);
        
        // Menu Edit
        JMenu editMenu = new JMenu("Edit");
        editMenu.setMnemonic(KeyEvent.VK_E);
        
        JMenuItem find = new JMenuItem("Find...", KeyEvent.VK_F);
        find.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F, ActionEvent.CTRL_MASK));
        find.addActionListener(e -> showSearchPanel());
        
        JMenuItem findInProject = new JMenuItem("Find in Project...", KeyEvent.VK_P);
        findInProject.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F, ActionEvent.CTRL_MASK | ActionEvent.SHIFT_MASK));
        findInProject.addActionListener(e -> showProjectSearch());
        
        editMenu.add(find);
        editMenu.add(findInProject);
        
        // Menu Prolog
        JMenu prologMenu = new JMenu("Prolog");
        prologMenu.setMnemonic(KeyEvent.VK_P);
        
        JMenuItem compileFile = new JMenuItem("Compile File", KeyEvent.VK_C);
        compileFile.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F9, 0));
        compileFile.addActionListener(e -> compileCurrentFile());
        
        JMenuItem compileProject = new JMenuItem("Compile Project", KeyEvent.VK_P);
        compileProject.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F9, ActionEvent.CTRL_MASK));
        compileProject.addActionListener(e -> compileProject());
        
        JMenuItem runQuery = new JMenuItem("Run Query...", KeyEvent.VK_Q);
        runQuery.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F5, 0));
        runQuery.addActionListener(e -> showQueryDialog());
        
        JMenuItem toggleTrace = new JMenuItem("Toggle Trace", KeyEvent.VK_T);
        toggleTrace.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F8, 0));
        toggleTrace.addActionListener(e -> toggleTrace());
        
        prologMenu.add(compileFile);
        prologMenu.add(compileProject);
        prologMenu.addSeparator();
        prologMenu.add(runQuery);
        prologMenu.add(toggleTrace);
        prologMenu.addSeparator();
        
        JMenuItem clearKB = new JMenuItem("Clear Knowledge Base", KeyEvent.VK_K);
        clearKB.addActionListener(e -> clearKnowledgeBaseAction());
        prologMenu.add(clearKB);
        
        JMenuItem showKBStatus = new JMenuItem("Show KB Status", KeyEvent.VK_S);
        showKBStatus.addActionListener(e -> showKnowledgeBaseStatus());
        prologMenu.add(showKBStatus);
        
        // Menu Help
        JMenu helpMenu = new JMenu("Help");
        helpMenu.setMnemonic(KeyEvent.VK_H);
        
        JMenuItem about = new JMenuItem("About", KeyEvent.VK_I);
        about.addActionListener(e -> showAboutDialog());
        helpMenu.add(about);
        
        menuBar.add(fileMenu);
        menuBar.add(editMenu);
        menuBar.add(prologMenu);
        menuBar.add(helpMenu);
        
        setJMenuBar(menuBar);
    }
    
    /**
     * Configura gli event handlers.
     */
    private void setupEventHandlers() {
        // Gestione chiusura finestra
        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                exitApplication();
            }
        });
        
        // Shortcuts globali
        setupGlobalShortcuts();
    }
    
    /**
     * Configura le scorciatoie da tastiera globali.
     */
    private void setupGlobalShortcuts() {
        InputMap inputMap = getRootPane().getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        ActionMap actionMap = getRootPane().getActionMap();
        
        // Escape per chiudere pannello ricerca
        inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), "hideSearch");
        actionMap.put("hideSearch", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                hideSearchPanel();
            }
        });
    }
    
    // ===================== GESTIONE PROGETTI =====================
    
    /**
     * Crea un nuovo progetto.
     */
    public void createNewProject() {
        JFileChooser chooser = new JFileChooser();
        chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        chooser.setDialogTitle("Select directory for new project");
        
        if (chooser.showDialog(this, "Create Project") == JFileChooser.APPROVE_OPTION) {
            File selectedDir = chooser.getSelectedFile();
            String projectName = selectedDir.getName();
            
            try {
                // The selected directory becomes the project root
                if (!selectedDir.exists()) {
                    throw new IOException("Selected directory does not exist");
                }
                
                // Create example file if directory is empty
                File[] existingFiles = selectedDir.listFiles(file -> file.getName().endsWith(".pl"));
                if (existingFiles == null || existingFiles.length == 0) {
                    File exampleFile = new File(selectedDir, "main.pl");
                    try (FileWriter writer = new FileWriter(exampleFile)) {
                        writer.write("% Main file of project " + projectName + "\n\n");
                        writer.write("% Define your predicates here\n");
                        writer.write("hello_world :- write('Hello, World!').\n\n");
                        writer.write("% Example predicates\n");
                        writer.write("fact(prolog_is_great).\n");
                        writer.write("fact(ide_is_useful).\n");
                        writer.write("fact(programming_is_fun).\n\n");
                        writer.write("% Show all facts (robust version)\n");
                        writer.write("show_facts :- forall(fact(X), (write(X), write(' | '))).\n\n");
                        writer.write("% Alternative version with separators\n");
                        writer.write("list_facts :- \n");
                        writer.write("    write('Facts: '),\n");
                        writer.write("    fact(F1), write(F1), write(', '),\n");
                        writer.write("    fact(F2), F2 \\= F1, write(F2), write(', '),\n");
                        writer.write("    fact(F3), F3 \\= F1, F3 \\= F2, write(F3), write('.').\n");
                    }
                }
                
                openProjectDirectory(selectedDir, projectName);
                bottomTabbedPane.appendToOutput("Project created/opened: " + selectedDir.getAbsolutePath() + "\n");
                
            } catch (IOException ex) {
                DialogUtils.showCenteredMessage(this, 
                    "Error creating project: " + ex.getMessage(),
                    "Error", JOptionPane.ERROR_MESSAGE);
            }
        }
    }
    
    /**
     * Apre un progetto esistente.
     */
    public void openProject() {
        JFileChooser chooser = new JFileChooser();
        chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        chooser.setDialogTitle("Select project directory");
        
        if (chooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
            File selectedDir = chooser.getSelectedFile();
            openProjectDirectory(selectedDir, selectedDir.getName());
        }
    }
    
    /**
     * Apre una directory come progetto.
     */
    private void openProjectDirectory(File projectDir, String projectName) {
        if (!projectDir.exists() || !projectDir.isDirectory()) {
            DialogUtils.showCenteredMessage(this, 
                "Invalid directory: " + projectDir.getAbsolutePath(),
                "Error", JOptionPane.ERROR_MESSAGE);
            return;
        }
        
        currentProjectRoot = projectDir;
        currentProjectName = projectName;
        
        // Aggiorna il titolo della finestra
        setTitle("JProlog IDE - " + projectName + " [" + projectDir.getAbsolutePath() + "]");
        
        // Carica l'albero del progetto
        projectTree.loadProject(projectDir);
        
        // Reset del motore Prolog per il nuovo progetto
        prologEngine = new Prolog();
        
        statusBar.setMessage("Project loaded: " + projectName);
        bottomTabbedPane.appendToOutput("Project opened: " + projectDir.getAbsolutePath() + "\n");
    }
    
    // ===================== GESTIONE FILE =====================
    
    /**
     * Crea un nuovo file nel progetto.
     */
    public void createNewFile() {
        if (currentProjectRoot == null) {
            DialogUtils.showCenteredMessage(this, 
                "Please open a project first", "No Project", JOptionPane.WARNING_MESSAGE);
            return;
        }
        
        String fileName = DialogUtils.showCenteredInput(this, 
            "File name (without extension):", "New File", JOptionPane.QUESTION_MESSAGE);
        
        if (fileName != null && !fileName.trim().isEmpty()) {
            File newFile = new File(currentProjectRoot, fileName.trim() + ".pl");
            try {
                if (newFile.createNewFile()) {
                    // Apri il file nell'editor
                    editorTabs.openFile(newFile);
                    projectTree.refreshProject();
                    bottomTabbedPane.appendToOutput("New file created: " + newFile.getName() + "\n");
                } else {
                    DialogUtils.showCenteredMessage(this, 
                        "File already exists: " + newFile.getName(),
                        "Error", JOptionPane.ERROR_MESSAGE);
                }
            } catch (IOException ex) {
                DialogUtils.showCenteredMessage(this, 
                    "Error creating file: " + ex.getMessage(),
                    "Error", JOptionPane.ERROR_MESSAGE);
            }
        }
    }
    
    /**
     * Salva il file corrente.
     */
    public void saveCurrentFile() {
        editorTabs.saveCurrentFile();
    }
    
    /**
     * Salva tutti i file aperti.
     */
    public void saveAllFiles() {
        editorTabs.saveAllFiles();
    }
    
    // ===================== PROLOG FUNCTIONALITY =====================
    
    /**
     * Compila il file corrente.
     */
    public void compileCurrentFile() {
        FileEditor currentEditor = editorTabs.getCurrentEditor();
        if (currentEditor == null) {
            bottomTabbedPane.appendToOutput("No file open for compilation.\n");
            return;
        }
        
        // Salva il file prima di compilare
        if (!currentEditor.save()) {
            bottomTabbedPane.appendToOutput("Unable to save file before compilation.\n");
            return;
        }
        
        File file = currentEditor.getFile();
        bottomTabbedPane.startBuild("Compiling File: " + file.getName());
        
        // Pulisci la knowledge base prima di compilare il singolo file
        clearKnowledgeBase();
        
        boolean success = compileFile(file);
        bottomTabbedPane.finishBuild(success);
    }
    
    /**
     * Compila tutti i file del progetto.
     */
    public void compileProject() {
        if (currentProjectRoot == null) {
            bottomTabbedPane.appendToOutput("No project open.\n");
            return;
        }
        
        bottomTabbedPane.startBuild("Compiling Project: " + currentProjectName);
        
        // Salva tutti i file aperti
        saveAllFiles();
        
        // Pulisci completamente la knowledge base prima di ricostruire
        clearKnowledgeBase();
        
        // Reinitialize Prolog engine with empty knowledge base
        initializePrologEngine();
        
        // Trova tutti i file .pl
        java.util.List<File> prologFiles = findPrologFiles(currentProjectRoot);
        
        if (prologFiles.isEmpty()) {
            bottomTabbedPane.appendToBuild("No .pl files found in project.\n");
            bottomTabbedPane.finishBuild(true);
            return;
        }
        
        bottomTabbedPane.appendToBuild("Found " + prologFiles.size() + " Prolog files to compile.\n");
        
        boolean hasErrors = false;
        int compiledFiles = 0;
        
        for (File file : prologFiles) {
            if (compileFile(file)) {
                compiledFiles++;
            } else {
                hasErrors = true;
            }
        }
        
        bottomTabbedPane.appendToBuild("\nCompilation Summary:\n");
        bottomTabbedPane.appendToBuild("  Files processed: " + prologFiles.size() + "\n");
        bottomTabbedPane.appendToBuild("  Files compiled successfully: " + compiledFiles + "\n");
        
        if (hasErrors) {
            bottomTabbedPane.appendToBuild("  Files with errors: " + (prologFiles.size() - compiledFiles) + "\n");
        }
        
        bottomTabbedPane.finishBuild(!hasErrors);
        
        // Notify RunPanel that knowledge base has been reloaded
        if (!hasErrors) {
            bottomTabbedPane.getRunPanel().onKnowledgeBaseReloaded();
        }
    }
    
    /**
     * Compila un singolo file: parsing ed caricamento nella knowledge base.
     */
    private boolean compileFile(File file) {
        try {
            bottomTabbedPane.appendToBuild("Parsing and loading: " + file.getName() + " ... ");
            
            // Leggi il contenuto del file
            String content = readFileContent(file);
            
            // Complete content parsing through JProlog parser (which now handles comments)
            try {
                prologEngine.consult(content);
                bottomTabbedPane.appendBuildSuccess("OK\n");
                return true;
            } catch (Exception e) {
                bottomTabbedPane.appendBuildError("ERROR\n");
                bottomTabbedPane.appendBuildError("  " + e.getMessage() + "\n");
                
                // Highlight error in editor if file is open
                FileEditor editor = editorTabs.getEditor(file);
                if (editor != null) {
                    editor.highlightError(1, e.getMessage());
                }
                return false;
            }
            
        } catch (Exception e) {
            bottomTabbedPane.appendBuildError("ERROR: " + e.getMessage() + "\n");
            return false;
        }
    }
    
    /**
     * Mostra il pannello Run per eseguire query interattive.
     */
    public void showQueryDialog() {
        if (prologEngine == null) {
            bottomTabbedPane.appendToOutput("Please compile the project first.\n");
            return;
        }
        
        // Mostra il tab Run e da focus al prompt
        bottomTabbedPane.showRunTab();
    }
    
    
    /**
     * Attiva/disattiva il trace.
     */
    private void toggleTrace() {
        bottomTabbedPane.showDebugTab();
        
        DebugPanel debugPanel = bottomTabbedPane.getDebugPanel();
        if (!debugPanel.isDebugMode()) {
            // If not in debug mode, start it automatically
            debugPanel.startDebugging();
        }
        
        // Toggle trace mode nel debug panel
        JToggleButton traceButton = findTraceButton(debugPanel);
        if (traceButton != null) {
            traceButton.doClick();
        }
    }
    
    /**
     * Trova il pulsante trace nel debug panel (helper method).
     */
    private JToggleButton findTraceButton(JComponent component) {
        if (component instanceof JToggleButton) {
            JToggleButton button = (JToggleButton) component;
            if (button.getText().contains("Trace")) {
                return button;
            }
        }
        
        for (Component child : component.getComponents()) {
            if (child instanceof JComponent) {
                JToggleButton found = findTraceButton((JComponent) child);
                if (found != null) {
                    return found;
                }
            }
        }
        
        return null;
    }
    
    // ===================== RICERCA =====================
    
    /**
     * Mostra il pannello di ricerca.
     */
    public void showSearchPanel() {
        searchPanel.setVisible(true);
        searchPanel.focusSearchField();
    }
    
    /**
     * Nasconde il pannello di ricerca.
     */
    private void hideSearchPanel() {
        searchPanel.setVisible(false);
        
        // Restituisce il focus all'editor corrente
        FileEditor currentEditor = editorTabs.getCurrentEditor();
        if (currentEditor != null) {
            currentEditor.requestFocus();
        }
    }
    
    /**
     * Mostra la ricerca nel progetto.
     */
    public void showProjectSearch() {
        searchPanel.setVisible(true);
        searchPanel.setProjectSearchMode(true);
        searchPanel.focusSearchField();
    }
    
    // ===================== KNOWLEDGE BASE MANAGEMENT =====================
    
    /**
     * Pulisce completamente la knowledge base.
     */
    private void clearKnowledgeBase() {
        if (prologEngine != null) {
            try {
                // Pulisce tutte le clausole dalla knowledge base
                bottomTabbedPane.appendToBuild("Clearing knowledge base... ");
                prologEngine = new Prolog(); // Reset completo del motore
                bottomTabbedPane.appendToBuild("OK\n");
            } catch (Exception e) {
                bottomTabbedPane.appendBuildError("ERROR: " + e.getMessage() + "\n");
            }
        }
    }
    
    /**
     * Initializes Prolog engine with empty knowledge base.
     */
    private void initializePrologEngine() {
        try {
            bottomTabbedPane.appendToBuild("Initializing Prolog engine... ");
            prologEngine = new Prolog();
            // Carica built-in predicates se necessario
            // prologEngine.loadBuiltins(); // Se disponibile
            bottomTabbedPane.appendToBuild("OK\n");
        } catch (Exception e) {
            bottomTabbedPane.appendBuildError("ERROR initializing Prolog engine: " + e.getMessage() + "\n");
        }
    }
    
    /**
     * Mostra lo stato della knowledge base.
     */
    public void showKnowledgeBaseStatus() {
        if (prologEngine == null) {
            bottomTabbedPane.appendToOutput("Prolog engine not initialized.\n");
            return;
        }
        
        bottomTabbedPane.appendToOutput("Knowledge Base Status:\n");
        bottomTabbedPane.appendToOutput("  Engine initialized: Yes\n");
        
        // Se il motore ha metodi per ottenere statistiche, usarli
        try {
            // Esempio: prologEngine.getClauseCount()
            bottomTabbedPane.appendToOutput("  Ready for queries.\n");
        } catch (Exception e) {
            bottomTabbedPane.appendToOutput("  Error checking status: " + e.getMessage() + "\n");
        }
    }
    
    /**
     * Azione per pulire manualmente la knowledge base.
     */
    private void clearKnowledgeBaseAction() {
        int result = DialogUtils.showCenteredConfirm(
            this,
            "This will clear all loaded predicates from the knowledge base.\n" +
            "You will need to recompile your project to reload them.\n\n" +
            "Are you sure you want to continue?",
            "Clear Knowledge Base",
            JOptionPane.YES_NO_OPTION,
            JOptionPane.WARNING_MESSAGE
        );
        
        if (result == JOptionPane.YES_OPTION) {
            bottomTabbedPane.switchToOutputTab();
            bottomTabbedPane.appendToOutput("=== Manual Knowledge Base Clear ===\n");
            clearKnowledgeBase();
            initializePrologEngine();
            bottomTabbedPane.appendToOutput("Knowledge base cleared successfully.\n");
            bottomTabbedPane.appendToOutput("Tip: Use 'Compile Project' to reload all predicates.\n\n");
        }
    }
    
    // ===================== UTILITY =====================
    
    /**
     * Trova tutti i file .pl in una directory (ricorsivo).
     */
    private java.util.List<File> findPrologFiles(File directory) {
        java.util.List<File> prologFiles = new ArrayList<>();
        findPrologFilesRecursive(directory, prologFiles);
        return prologFiles;
    }
    
    /**
     * Trova i file .pl ricorsivamente.
     */
    private void findPrologFilesRecursive(File directory, java.util.List<File> result) {
        File[] files = directory.listFiles();
        if (files != null) {
            for (File file : files) {
                if (file.isDirectory()) {
                    findPrologFilesRecursive(file, result);
                } else if (file.getName().toLowerCase().endsWith(".pl")) {
                    result.add(file);
                }
            }
        }
    }
    
    /**
     * Legge il contenuto di un file.
     */
    private String readFileContent(File file) throws IOException {
        return new String(Files.readAllBytes(file.toPath()), "UTF-8");
    }
    
    /**
     * Loads configuration.
     */
    private void loadConfiguration() {
        config = new Properties();
        configFile = new File(System.getProperty("user.home"), ".jprolog-ide.properties");
        
        if (configFile.exists()) {
            try (FileInputStream fis = new FileInputStream(configFile)) {
                config.load(fis);
            } catch (IOException e) {
                // Use default configuration
            }
        }
    }
    
    /**
     * Saves configuration.
     */
    private void saveConfiguration() {
        try (FileOutputStream fos = new FileOutputStream(configFile)) {
            config.store(fos, "JProlog Editor from DenzoSOFT Configuration");
        } catch (IOException e) {
            // Ignore configuration save errors
        }
    }
    
    /**
     * Mostra il dialogo informazioni.
     */
    public void showAboutDialog() {
        String message = 
            "JProlog Editor from DenzoSOFT\n" +
            "Integrated Development Environment for Prolog\n\n" +
            "Version: 1.0\n" +
            "Author: DenzoSOFT\n" +
            "Website: https://denzosoft.it\n\n" +
            "Features:\n" +
            "• Project management\n" +
            "• Editor with syntax highlighting\n" +
            "• Compilation and debugging\n" +
            "• Query execution\n" +
            "• Advanced search system\n" +
            "• Complete debug support\n" +
            "• Multi-line comments\n" +
            "• Cut operator support";
        
        DialogUtils.showCenteredMessage(this, message, 
            "About JProlog Editor from DenzoSOFT", JOptionPane.INFORMATION_MESSAGE);
    }
    
    /**
     * Chiude l'applicazione.
     */
    private void exitApplication() {
        // Controlla se ci sono file non salvati
        if (editorTabs.hasUnsavedFiles()) {
            int choice = DialogUtils.showCenteredConfirm(this,
                "There are unsaved files. Do you want to save before exiting?",
                "Unsaved Files", JOptionPane.YES_NO_CANCEL_OPTION);
            
            if (choice == JOptionPane.YES_OPTION) {
                saveAllFiles();
            } else if (choice == JOptionPane.CANCEL_OPTION) {
                return;
            }
        }
        
        saveConfiguration();
        dispose();
        System.exit(0);
    }
    
    // ===================== GETTERS =====================
    
    public Prolog getPrologEngine() {
        return prologEngine;
    }
    
    public File getCurrentProjectRoot() {
        return currentProjectRoot;
    }
    
    public BottomTabbedPane getBottomTabbedPane() {
        return bottomTabbedPane;
    }
    
    public OutputConsole getOutputConsole() {
        return bottomTabbedPane.getOutputPanel();
    }
    
    public StatusBar getStatusBar() {
        return statusBar;
    }
    
    public EditorTabbedPane getEditorTabs() {
        return editorTabs;
    }
    
    public PredicatePanel getPredicatePanel() {
        return predicatePanel;
    }
    
    // ===================== DEBUG SUPPORT =====================
    
    /**
     * Sets debug mode.
     */
    public void setDebugMode(boolean debugMode) {
        this.debugMode = debugMode;
        if (debugMode) {
            bottomTabbedPane.startDebugSession();
        } else {
            bottomTabbedPane.stopDebugSession();
        }
    }
    
    /**
     * Checks if we are in debug mode.
     */
    public boolean isDebugMode() {
        return debugMode;
    }
    
    /**
     * Ottieni il debug panel.
     */
    public DebugPanel getDebugPanel() {
        return bottomTabbedPane.getDebugPanel();
    }
    
    // ===================== MAIN =====================
    
    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            try {
                UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
            } catch (Exception e) {
                // Usa look and feel di default
            }
            
            new PrologIDE().setVisible(true);
        });
    }
}