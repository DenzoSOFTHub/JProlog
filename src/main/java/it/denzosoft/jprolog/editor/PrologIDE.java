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

    // START_CHANGE: ISS-IDE-SETTINGS - Persisted UI components & MRU
    // Split panes promoted to fields so divider locations can be persisted/restored.
    private JSplitPane mainSplitPane;
    private JSplitPane leftSplitPane;
    private JSplitPane rightSplitPane;
    // Recent Projects (MRU) menu, populated from config on startup.
    private JMenu recentProjectsMenu;
    // Most-recently-used project paths (absolute), newest first, max 10 entries.
    private final java.util.List<String> recentProjects = new ArrayList<>();
    // Maximum number of entries kept in the MRU list.
    private static final int MAX_RECENT_PROJECTS = 10;
    // Property keys used in ~/.jprolog-ide.properties
    private static final String KEY_WIN_X = "window.x";
    private static final String KEY_WIN_Y = "window.y";
    private static final String KEY_WIN_W = "window.width";
    private static final String KEY_WIN_H = "window.height";
    private static final String KEY_WIN_MAXIMIZED = "window.maximized";
    private static final String KEY_DIVIDER_MAIN = "divider.main";
    private static final String KEY_DIVIDER_LEFT = "divider.left";
    private static final String KEY_DIVIDER_RIGHT = "divider.right";
    private static final String KEY_FONT_SIZE = "editor.fontSize";
    private static final String KEY_LAST_PROJECT = "project.last";
    private static final String KEY_RECENT_PREFIX = "project.recent.";
    private static final String KEY_RECENT_COUNT = "project.recent.count";
    // END_CHANGE: ISS-IDE-SETTINGS

    public PrologIDE() {
        super("JProlog Editor from DenzoSOFT");
        // START_CHANGE: ISS-IDE-SETTINGS - load config before building UI so MRU menu can be populated
        loadConfiguration();
        loadRecentProjects();
        // END_CHANGE: ISS-IDE-SETTINGS
        initializeComponents();
        setupLayout();
        setupMenuBar();
        setupEventHandlers();

        setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);

        // START_CHANGE: ISS-IDE-SETTINGS - apply persisted window geometry / dividers / font / last project
        applyWindowSettings();
        applyEditorSettings();
        // END_CHANGE: ISS-IDE-SETTINGS

        // Initialize Prolog engine
        prologEngine = new Prolog();

        statusBar.setMessage("IDE started - Open or create a project to begin");

        // START_CHANGE: ISS-IDE-SETTINGS - reopen last project (deferred so frame is realized first)
        SwingUtilities.invokeLater(this::reopenLastProject);
        // END_CHANGE: ISS-IDE-SETTINGS
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
        
        searchPanel.setVisible(false); // Initially hidden
    }
    
    /**
     * Configure the main IDE layout.
     */
    private void setupLayout() {
        setLayout(new BorderLayout());
        
        // Toolbar at top
        add(toolbar, BorderLayout.NORTH);
        
        // Main panel with split pane
        // START_CHANGE: ISS-IDE-SETTINGS - assign split panes to fields for divider persistence
        mainSplitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
        mainSplitPane.setDividerLocation(250);
        mainSplitPane.setResizeWeight(0.2);

        // Left panel: project tree + predicates panel
        leftSplitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
        leftSplitPane.setResizeWeight(0.6); // 60% for project tree, 40% for predicates
        
        // Project tree panel
        JPanel projectPanel = new JPanel(new BorderLayout());
        projectPanel.add(new JLabel("Project", SwingConstants.CENTER), BorderLayout.NORTH);
        projectPanel.add(new JScrollPane(projectTree), BorderLayout.CENTER);
        projectPanel.setMinimumSize(new Dimension(200, 150));
        leftSplitPane.setTopComponent(projectPanel);
        
        // Predicates panel
        predicatePanel.setMinimumSize(new Dimension(200, 100));
        leftSplitPane.setBottomComponent(predicatePanel);
        
        leftSplitPane.setMinimumSize(new Dimension(200, 0));
        mainSplitPane.setLeftComponent(leftSplitPane);
        
        // Pannello destro: editor e console
        rightSplitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
        rightSplitPane.setResizeWeight(0.7);
        // END_CHANGE: ISS-IDE-SETTINGS
        
        // Pannello editor con ricerca
        JPanel editorPanel = new JPanel(new BorderLayout());
        editorPanel.add(searchPanel, BorderLayout.NORTH);
        editorPanel.add(editorTabs, BorderLayout.CENTER);
        rightSplitPane.setTopComponent(editorPanel);
        
        // Bottom tabbed panel (Output, Build, Run, Search)
        rightSplitPane.setBottomComponent(bottomTabbedPane);
        
        mainSplitPane.setRightComponent(rightSplitPane);
        add(mainSplitPane, BorderLayout.CENTER);
        
        // Status bar at bottom
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

        // START_CHANGE: ISS-IDE-SETTINGS - Recent Projects (MRU) submenu
        recentProjectsMenu = new JMenu("Recent Projects");
        recentProjectsMenu.setMnemonic(KeyEvent.VK_R);
        rebuildRecentProjectsMenu();
        fileMenu.add(recentProjectsMenu);
        // END_CHANGE: ISS-IDE-SETTINGS

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

        JMenuItem undoItem = new JMenuItem("Undo", KeyEvent.VK_U);
        undoItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Z, ActionEvent.CTRL_MASK));
        undoItem.addActionListener(e -> { FileEditor ed = editorTabs.getCurrentEditor(); if (ed != null) ed.undo(); });

        JMenuItem redoItem = new JMenuItem("Redo", KeyEvent.VK_R);
        redoItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_Y, ActionEvent.CTRL_MASK));
        redoItem.addActionListener(e -> { FileEditor ed = editorTabs.getCurrentEditor(); if (ed != null) ed.redo(); });

        JMenuItem formatItem = new JMenuItem("Format Source", KeyEvent.VK_L);
        formatItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_L, ActionEvent.CTRL_MASK | ActionEvent.ALT_MASK));
        formatItem.addActionListener(e -> { FileEditor ed = editorTabs.getCurrentEditor(); if (ed != null) ed.formatSource(); });

        // START_CHANGE: ISS-IDE-M19 - Go to Line + Quick Open navigation
        JMenuItem goToLineItem = new JMenuItem("Go to Line...", KeyEvent.VK_G);
        goToLineItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_G, ActionEvent.CTRL_MASK));
        goToLineItem.addActionListener(e -> showGoToLineDialog());

        JMenuItem quickOpenItem = new JMenuItem("Quick Open...", KeyEvent.VK_P);
        quickOpenItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_P, ActionEvent.CTRL_MASK));
        quickOpenItem.addActionListener(e -> showQuickOpenDialog());
        // END_CHANGE: ISS-IDE-M19

        editMenu.add(undoItem);
        editMenu.add(redoItem);
        editMenu.addSeparator();
        editMenu.add(formatItem);
        editMenu.addSeparator();
        // START_CHANGE: ISS-IDE-M19 - navigation menu entries
        editMenu.add(goToLineItem);
        editMenu.add(quickOpenItem);
        editMenu.addSeparator();
        // END_CHANGE: ISS-IDE-M19
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

        // START_CHANGE: ISS-IDE-M09 - Compile current file to a .jpc binary
        JMenuItem compileJpc = new JMenuItem("Compile to .jpc", KeyEvent.VK_J);
        compileJpc.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F9, ActionEvent.SHIFT_MASK));
        compileJpc.addActionListener(e -> compileCurrentFileToJpc());
        // END_CHANGE: ISS-IDE-M09
        
        JMenuItem runQuery = new JMenuItem("Run Query...", KeyEvent.VK_Q);
        runQuery.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F5, 0));
        runQuery.addActionListener(e -> showQueryDialog());
        
        JMenuItem toggleTrace = new JMenuItem("Toggle Trace", KeyEvent.VK_T);
        toggleTrace.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F8, 0));
        toggleTrace.addActionListener(e -> toggleTrace());

        JMenuItem debugQuery = new JMenuItem("Debug Query...", KeyEvent.VK_D);
        debugQuery.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F5, ActionEvent.SHIFT_MASK));
        debugQuery.addActionListener(e -> showDebugPanel());

        prologMenu.add(compileFile);
        prologMenu.add(compileProject);
        // START_CHANGE: ISS-IDE-M09 - Compile to .jpc menu entry
        prologMenu.add(compileJpc);
        // END_CHANGE: ISS-IDE-M09
        prologMenu.addSeparator();
        prologMenu.add(runQuery);
        prologMenu.add(debugQuery);
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

        // START_CHANGE: ISS-IDE-M17 - keep toolbar enable/disable state in sync with the
        // active editor tab. updateToolbarState() exists on PrologToolbar but was never
        // invoked; hook it to tab changes (in addition to project open/close and compile).
        editorTabs.addChangeListener(e -> updateToolbar());
        // END_CHANGE: ISS-IDE-M17

        // START_CHANGE: ISS-IDE-M21 - the toolbar Debug button was a placeholder ("under
        // development" dialog). Re-route it through the same path as the Debug menu item.
        if (toolbar != null && toolbar.getDebugButton() != null) {
            JButton dbgBtn = toolbar.getDebugButton();
            for (java.awt.event.ActionListener al : dbgBtn.getActionListeners()) {
                dbgBtn.removeActionListener(al);
            }
            dbgBtn.addActionListener(e -> showDebugPanel());
        }
        // END_CHANGE: ISS-IDE-M21

        // START_CHANGE: ISS-IDE-M17 - initialise toolbar state once the UI is built.
        updateToolbar();
        // END_CHANGE: ISS-IDE-M17
    }
    
    /**
     * Configura le scorciatoie da tastiera globali.
     */
    private void setupGlobalShortcuts() {
        InputMap inputMap = getRootPane().getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
        ActionMap actionMap = getRootPane().getActionMap();

        // Escape to close search panel
        inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), "hideSearch");
        actionMap.put("hideSearch", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                hideSearchPanel();
            }
        });

        // F7 - Step Into (when debug is active)
        inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_F7, 0), "debugStepInto");
        actionMap.put("debugStepInto", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                DebugPanel dp = getDebugPanel();
                if (dp != null && dp.isDebugMode()) {
                    bottomTabbedPane.showDebugTab();
                }
            }
        });
    }
    
    // ===================== GESTIONE PROGETTI =====================
    
    /**
     * Creates a new project.
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
     * Opens a directory as project.
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
        
        // Update window title
        setTitle("JProlog IDE - " + projectName + " [" + projectDir.getAbsolutePath() + "]");
        
        // Carica l'albero del progetto
        projectTree.loadProject(projectDir);
        
        // Reset Prolog engine for the new project
        prologEngine = new Prolog();
        
        statusBar.setMessage("Project loaded: " + projectName);
        bottomTabbedPane.appendToOutput("Project opened: " + projectDir.getAbsolutePath() + "\n");

        // START_CHANGE: ISS-IDE-SETTINGS - record opened project in MRU and as last project
        addToRecentProjects(projectDir);
        // END_CHANGE: ISS-IDE-SETTINGS

        // START_CHANGE: ISS-IDE-M17 - refresh toolbar state on project open
        updateToolbar();
        // END_CHANGE: ISS-IDE-M17
    }
    
    // ===================== GESTIONE FILE =====================
    
    /**
     * Creates a new file in the project.
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
     * Save all open files.
     */
    public void saveAllFiles() {
        editorTabs.saveAllFiles();
    }
    
    // ===================== PROLOG FUNCTIONALITY =====================
    
    /**
     * Compile current file.
     */
    public void compileCurrentFile() {
        FileEditor currentEditor = editorTabs.getCurrentEditor();
        if (currentEditor == null) {
            bottomTabbedPane.appendToOutput("No file open for compilation.\n");
            return;
        }
        
        // Save file before compiling
        if (!currentEditor.save()) {
            bottomTabbedPane.appendToOutput("Unable to save file before compilation.\n");
            return;
        }
        
        File file = currentEditor.getFile();
        bottomTabbedPane.startBuild("Compiling File: " + file.getName());
        
        // Clear knowledge base before compiling single file
        clearKnowledgeBase();
        
        boolean success = compileFile(file);
        bottomTabbedPane.finishBuild(success);

        // START_CHANGE: ISS-IDE-M17 - refresh toolbar state after compile
        updateToolbar();
        // END_CHANGE: ISS-IDE-M17
    }

    // START_CHANGE: ISS-IDE-M09 - Compile current file to a .jpc binary
    /**
     * Compiles the file in the active editor tab to a binary {@code .jpc} file using the
     * engine's compiler. The source is saved first; the generated path is reported to the
     * Build output and status bar.
     */
    public void compileCurrentFileToJpc() {
        FileEditor currentEditor = editorTabs.getCurrentEditor();
        if (currentEditor == null) {
            bottomTabbedPane.appendToOutput("No file open to compile to .jpc.\n");
            return;
        }

        // Save file before compiling so the .jpc reflects the on-disk source.
        if (!currentEditor.save()) {
            bottomTabbedPane.appendToOutput("Unable to save file before .jpc compilation.\n");
            return;
        }

        File file = currentEditor.getFile();
        bottomTabbedPane.startBuild("Compiling to .jpc: " + file.getName());

        try {
            // Prefer the engine's compileFile(path) which derives the .jpc name and writes it.
            String jpcPath = prologEngine.compileFile(file.getAbsolutePath());
            bottomTabbedPane.appendBuildSuccess("Compiled to: " + jpcPath + "\n");
            statusBar.setMessage("Compiled to .jpc: " + jpcPath);
            bottomTabbedPane.finishBuild(true);
        } catch (Exception ex) {
            bottomTabbedPane.appendBuildError("ERROR compiling to .jpc: " + ex.getMessage() + "\n");
            statusBar.setMessage("Failed to compile " + file.getName() + " to .jpc");
            bottomTabbedPane.finishBuild(false);
        }

        // Refresh toolbar/project view so the new artifact is reflected.
        updateToolbar();
        if (projectTree != null) {
            projectTree.refreshProject();
        }
    }
    // END_CHANGE: ISS-IDE-M09
    
    /**
     * Compile all project files.
     */
    public void compileProject() {
        if (currentProjectRoot == null) {
            bottomTabbedPane.appendToOutput("No project open.\n");
            return;
        }
        
        bottomTabbedPane.startBuild("Compiling Project: " + currentProjectName);
        
        // Save all open files
        saveAllFiles();
        
        // Completely clear knowledge base before rebuilding
        clearKnowledgeBase();
        
        // Reinitialize Prolog engine with empty knowledge base
        initializePrologEngine();
        
        // Find all .pl files
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

        // START_CHANGE: ISS-IDE-M17 - refresh toolbar state after project compile
        updateToolbar();
        // END_CHANGE: ISS-IDE-M17
    }
    
    /**
     * Compile single file with diagnostic error reporting.
     * Uses consultWithDiagnostics for per-clause error collection.
     */
    private boolean compileFile(File file) {
        try {
            bottomTabbedPane.appendToBuild("Parsing and loading: " + file.getName() + " ... ");

            String content = readFileContent(file);
            FileEditor editor = editorTabs.getEditor(file);

            // Clear previous error highlights
            if (editor != null) {
                editor.clearAllHighlights();
                editor.clearErrorHighlighting();
            }

            // Use diagnostics-based compilation
            Prolog.CompilationResult result = prologEngine.consultWithDiagnostics(content, file.getName());

            if (result.success) {
                bottomTabbedPane.appendBuildSuccess("OK (" + result.totalClauses + " clauses)\n");
                return true;
            } else {
                bottomTabbedPane.appendBuildError("ERRORS\n");
                for (Prolog.CompilationError error : result.errors) {
                    String errorLine = "  Line " + error.lineNumber + ": " + error.message + "\n";
                    bottomTabbedPane.appendBuildError(errorLine);

                    // Highlight error in editor
                    if (editor != null) {
                        editor.highlightErrorLine(error.lineNumber, error.message);
                    }
                }
                if (result.totalClauses > 0) {
                    bottomTabbedPane.appendBuildWarning(
                        "  (" + result.totalClauses + " clauses loaded successfully before errors)\n");
                }
                return false;
            }

        } catch (Exception e) {
            bottomTabbedPane.appendBuildError("ERROR: " + e.getMessage() + "\n");
            return false;
        }
    }
    
    /**
     * Show Run panel to execute interactive queries.
     */
    public void showQueryDialog() {
        if (prologEngine == null) {
            bottomTabbedPane.appendToOutput("Please compile the project first.\n");
            return;
        }
        
        // Show Run tab and give focus to prompt
        bottomTabbedPane.showRunTab();
    }
    
    
    /**
     * Enable/disable trace.
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
     * Find trace button in debug panel (helper method).
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
    
    // ===================== SEARCH =====================
    
    /**
     * Show search panel.
     */
    public void showSearchPanel() {
        searchPanel.setVisible(true);
        searchPanel.focusSearchField();
    }
    
    /**
     * Hide search panel.
     */
    private void hideSearchPanel() {
        searchPanel.setVisible(false);
        
        // Return focus to current editor
        FileEditor currentEditor = editorTabs.getCurrentEditor();
        if (currentEditor != null) {
            currentEditor.requestFocus();
        }
    }
    
    /**
     * Show project search.
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
                // Clear all clauses from knowledge base
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
            // Load built-in predicates if necessary
            // prologEngine.loadBuiltins(); // Se disponibile
            bottomTabbedPane.appendToBuild("OK\n");
        } catch (Exception e) {
            bottomTabbedPane.appendBuildError("ERROR initializing Prolog engine: " + e.getMessage() + "\n");
        }
    }
    
    /**
     * Shows the knowledge base status.
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
     * Finds all .pl files in a directory (recursive).
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
        // START_CHANGE: ISS-IDE-SETTINGS - capture live UI state before persisting
        captureSettings();
        // END_CHANGE: ISS-IDE-SETTINGS
        try (FileOutputStream fos = new FileOutputStream(configFile)) {
            config.store(fos, "JProlog Editor from DenzoSOFT Configuration");
        } catch (IOException e) {
            // Ignore configuration save errors
        }
    }

    // START_CHANGE: ISS-IDE-SETTINGS - settings persistence helpers
    /**
     * Captures the current window geometry, split-pane dividers, editor font size,
     * last project and the recent-projects MRU list into the {@link #config} object.
     * Called right before the configuration is written to disk on exit.
     */
    private void captureSettings() {
        if (config == null) {
            return;
        }

        // Window state: persist whether maximized, plus the "normal" bounds.
        boolean maximized = (getExtendedState() & JFrame.MAXIMIZED_BOTH) == JFrame.MAXIMIZED_BOTH;
        config.setProperty(KEY_WIN_MAXIMIZED, Boolean.toString(maximized));
        // When maximized, getBounds() returns the maximized bounds; keep the previously
        // stored normal bounds so the window can be restored to a sensible size.
        if (!maximized) {
            Rectangle b = getBounds();
            config.setProperty(KEY_WIN_X, Integer.toString(b.x));
            config.setProperty(KEY_WIN_Y, Integer.toString(b.y));
            config.setProperty(KEY_WIN_W, Integer.toString(b.width));
            config.setProperty(KEY_WIN_H, Integer.toString(b.height));
        }

        // Split-pane divider locations.
        if (mainSplitPane != null) {
            config.setProperty(KEY_DIVIDER_MAIN, Integer.toString(mainSplitPane.getDividerLocation()));
        }
        if (leftSplitPane != null) {
            config.setProperty(KEY_DIVIDER_LEFT, Integer.toString(leftSplitPane.getDividerLocation()));
        }
        if (rightSplitPane != null) {
            config.setProperty(KEY_DIVIDER_RIGHT, Integer.toString(rightSplitPane.getDividerLocation()));
        }

        // Current editor font size (if an editor is open and exposes a text pane).
        int fontSize = getCurrentEditorFontSize();
        if (fontSize > 0) {
            config.setProperty(KEY_FONT_SIZE, Integer.toString(fontSize));
        }

        // Last opened project.
        if (currentProjectRoot != null) {
            config.setProperty(KEY_LAST_PROJECT, currentProjectRoot.getAbsolutePath());
        }

        // Recent projects MRU list.
        storeRecentProjects();
    }

    /**
     * Returns the font size of the current editor's text pane, or -1 if unavailable.
     */
    private int getCurrentEditorFontSize() {
        try {
            FileEditor ed = editorTabs != null ? editorTabs.getCurrentEditor() : null;
            if (ed != null) {
                JTextPane tp = ed.getTextPane();
                if (tp != null && tp.getFont() != null) {
                    return tp.getFont().getSize();
                }
            }
        } catch (Exception ignore) {
            // best-effort only
        }
        return -1;
    }

    /**
     * Applies the persisted window geometry (bounds + maximized) at startup.
     * Falls back to centered MAXIMIZED_BOTH when no settings are present.
     */
    private void applyWindowSettings() {
        boolean applied = false;
        try {
            String sw = config.getProperty(KEY_WIN_W);
            String sh = config.getProperty(KEY_WIN_H);
            if (sw != null && sh != null) {
                int w = Integer.parseInt(sw);
                int h = Integer.parseInt(sh);
                int x = parseIntOr(config.getProperty(KEY_WIN_X), 0);
                int y = parseIntOr(config.getProperty(KEY_WIN_Y), 0);
                if (w > 100 && h > 100) {
                    // Clamp to the available screen so the window cannot open off-screen.
                    Dimension screen = Toolkit.getDefaultToolkit().getScreenSize();
                    w = Math.min(w, screen.width);
                    h = Math.min(h, screen.height);
                    x = Math.max(0, Math.min(x, screen.width - w));
                    y = Math.max(0, Math.min(y, screen.height - h));
                    setBounds(x, y, w, h);
                    applied = true;
                }
            }
        } catch (Exception ignore) {
            // fall through to default sizing
        }

        if (!applied) {
            // Default behaviour preserved: maximized & centered.
            setExtendedState(JFrame.MAXIMIZED_BOTH);
            setLocationRelativeTo(null);
        } else if (Boolean.parseBoolean(config.getProperty(KEY_WIN_MAXIMIZED, "false"))) {
            // Restore maximized state on top of the stored normal bounds.
            setExtendedState(JFrame.MAXIMIZED_BOTH);
        }

        // Restore divider locations once the frame is realized (sizes are known then).
        SwingUtilities.invokeLater(this::applyDividerSettings);
    }

    /**
     * Restores the split-pane divider locations from config.
     */
    private void applyDividerSettings() {
        applyDivider(mainSplitPane, KEY_DIVIDER_MAIN);
        applyDivider(leftSplitPane, KEY_DIVIDER_LEFT);
        applyDivider(rightSplitPane, KEY_DIVIDER_RIGHT);
    }

    private void applyDivider(JSplitPane pane, String key) {
        if (pane == null) {
            return;
        }
        String v = config.getProperty(key);
        if (v != null) {
            try {
                int loc = Integer.parseInt(v);
                if (loc > 0) {
                    pane.setDividerLocation(loc);
                }
            } catch (NumberFormatException ignore) {
                // keep default divider location
            }
        }
    }

    /**
     * Applies the persisted editor font size to all currently open editors (and any
     * opened later inherit the default; this best-effort sync covers the common case).
     */
    private void applyEditorSettings() {
        String v = config.getProperty(KEY_FONT_SIZE);
        if (v == null) {
            return;
        }
        try {
            int size = Integer.parseInt(v);
            if (size <= 0 || editorTabs == null) {
                return;
            }
            for (FileEditor ed : editorTabs.getEditors()) {
                JTextPane tp = ed.getTextPane();
                if (tp != null && tp.getFont() != null) {
                    tp.setFont(tp.getFont().deriveFont((float) size));
                }
            }
        } catch (Exception ignore) {
            // best-effort only
        }
    }

    /**
     * Reopens the last project recorded in config, if its directory still exists.
     */
    private void reopenLastProject() {
        String last = config.getProperty(KEY_LAST_PROJECT);
        if (last == null || last.trim().isEmpty()) {
            return;
        }
        File dir = new File(last);
        if (dir.exists() && dir.isDirectory()) {
            openProjectDirectory(dir, dir.getName());
        }
    }

    /**
     * Loads the recent-projects MRU list from config into {@link #recentProjects}.
     */
    private void loadRecentProjects() {
        recentProjects.clear();
        if (config == null) {
            return;
        }
        int count = parseIntOr(config.getProperty(KEY_RECENT_COUNT), 0);
        for (int i = 0; i < count && recentProjects.size() < MAX_RECENT_PROJECTS; i++) {
            String path = config.getProperty(KEY_RECENT_PREFIX + i);
            if (path != null && !path.trim().isEmpty() && !recentProjects.contains(path)) {
                recentProjects.add(path);
            }
        }
    }

    /**
     * Stores {@link #recentProjects} back into config. Stale keys beyond the current
     * size are removed so the file does not accumulate orphaned entries.
     */
    private void storeRecentProjects() {
        if (config == null) {
            return;
        }
        // Remove any previously stored recent keys.
        int previous = parseIntOr(config.getProperty(KEY_RECENT_COUNT), 0);
        for (int i = 0; i < Math.max(previous, MAX_RECENT_PROJECTS); i++) {
            config.remove(KEY_RECENT_PREFIX + i);
        }
        int n = Math.min(recentProjects.size(), MAX_RECENT_PROJECTS);
        for (int i = 0; i < n; i++) {
            config.setProperty(KEY_RECENT_PREFIX + i, recentProjects.get(i));
        }
        config.setProperty(KEY_RECENT_COUNT, Integer.toString(n));
    }

    /**
     * Adds (or promotes) a project directory to the front of the MRU list and refreshes
     * the Recent Projects submenu. Capped at {@link #MAX_RECENT_PROJECTS} entries.
     */
    private void addToRecentProjects(File projectDir) {
        if (projectDir == null) {
            return;
        }
        String path = projectDir.getAbsolutePath();
        recentProjects.remove(path);          // de-duplicate
        recentProjects.add(0, path);          // newest first
        while (recentProjects.size() > MAX_RECENT_PROJECTS) {
            recentProjects.remove(recentProjects.size() - 1);
        }
        rebuildRecentProjectsMenu();
    }

    /**
     * Rebuilds the Recent Projects submenu from the current MRU list. Entries whose
     * directory no longer exists are still shown but open will warn the user.
     */
    private void rebuildRecentProjectsMenu() {
        if (recentProjectsMenu == null) {
            return;
        }
        recentProjectsMenu.removeAll();
        if (recentProjects.isEmpty()) {
            JMenuItem empty = new JMenuItem("(none)");
            empty.setEnabled(false);
            recentProjectsMenu.add(empty);
            recentProjectsMenu.setEnabled(false);
            return;
        }
        recentProjectsMenu.setEnabled(true);
        int index = 1;
        for (final String path : recentProjects) {
            JMenuItem item = new JMenuItem(index + "  " + path);
            item.addActionListener(e -> openRecentProject(path));
            recentProjectsMenu.add(item);
            index++;
        }
        recentProjectsMenu.addSeparator();
        JMenuItem clear = new JMenuItem("Clear Recent Projects");
        clear.addActionListener(e -> {
            recentProjects.clear();
            rebuildRecentProjectsMenu();
        });
        recentProjectsMenu.add(clear);
    }

    /**
     * Opens a project chosen from the Recent Projects submenu. If the directory no
     * longer exists it is removed from the MRU list and the user is informed.
     */
    private void openRecentProject(String path) {
        File dir = new File(path);
        if (dir.exists() && dir.isDirectory()) {
            openProjectDirectory(dir, dir.getName());
        } else {
            recentProjects.remove(path);
            rebuildRecentProjectsMenu();
            DialogUtils.showCenteredMessage(this,
                "Project no longer exists:\n" + path,
                "Recent Project", JOptionPane.WARNING_MESSAGE);
        }
    }

    /**
     * Parses an int, returning a default value on null/parse error.
     */
    private int parseIntOr(String value, int defaultValue) {
        if (value == null) {
            return defaultValue;
        }
        try {
            return Integer.parseInt(value.trim());
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }
    // END_CHANGE: ISS-IDE-SETTINGS
    
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
        // Check if there are unsaved files
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
    
    /**
     * Show the debug panel and start a debug session.
     */
    private void showDebugPanel() {
        bottomTabbedPane.showDebugTab();
        DebugPanel dp = bottomTabbedPane.getDebugPanel();
        if (!dp.isDebugMode()) {
            dp.startDebugging();
        }
    }

    // ===================== NAVIGATION (M19) =====================

    // START_CHANGE: ISS-IDE-M17 - centralised toolbar state refresh
    /**
     * Refreshes the toolbar's context-sensitive enable/disable state. Invoked on editor
     * tab changes, project open/close and after compile so buttons reflect the current
     * context. {@link PrologToolbar#updateToolbarState()} previously existed but was never
     * called.
     */
    private void updateToolbar() {
        if (toolbar != null) {
            toolbar.updateToolbarState();
        }
    }
    // END_CHANGE: ISS-IDE-M17

    // START_CHANGE: ISS-IDE-M19 - Go to Line dialog
    /**
     * Prompts for a 1-based line number and moves the active editor's caret there via
     * {@link FileEditor#goToLine(int)}.
     */
    private void showGoToLineDialog() {
        FileEditor editor = editorTabs.getCurrentEditor();
        if (editor == null) {
            DialogUtils.showCenteredMessage(this,
                "No file is open.", "Go to Line", JOptionPane.INFORMATION_MESSAGE);
            return;
        }
        String input = DialogUtils.showCenteredInput(this,
            "Line number:", "Go to Line", JOptionPane.QUESTION_MESSAGE);
        if (input == null || input.trim().isEmpty()) {
            return;
        }
        try {
            int line = Integer.parseInt(input.trim());
            if (line < 1) {
                throw new NumberFormatException("line must be >= 1");
            }
            editor.goToLine(line);
            editor.requestFocusInWindow();
            statusBar.setMessage("Moved to line " + line);
        } catch (NumberFormatException ex) {
            DialogUtils.showCenteredMessage(this,
                "Invalid line number: " + input,
                "Go to Line", JOptionPane.ERROR_MESSAGE);
        }
    }
    // END_CHANGE: ISS-IDE-M19

    // START_CHANGE: ISS-IDE-M19 - Quick Open dialog
    /**
     * Shows a modal Quick Open dialog listing the project's {@code .pl} files with a
     * type-to-filter field. The chosen file is opened via {@link EditorTabbedPane#openFile(File)}.
     */
    private void showQuickOpenDialog() {
        if (currentProjectRoot == null) {
            DialogUtils.showCenteredMessage(this,
                "Please open a project first.", "Quick Open", JOptionPane.WARNING_MESSAGE);
            return;
        }

        final java.util.List<File> allFiles = findPrologFiles(currentProjectRoot);
        if (allFiles.isEmpty()) {
            DialogUtils.showCenteredMessage(this,
                "No .pl files found in the project.", "Quick Open", JOptionPane.INFORMATION_MESSAGE);
            return;
        }
        // Stable, human-friendly ordering by relative path.
        Collections.sort(allFiles, new Comparator<File>() {
            @Override
            public int compare(File a, File b) {
                return quickOpenLabel(a).compareToIgnoreCase(quickOpenLabel(b));
            }
        });

        final JDialog dialog = new JDialog(this, "Quick Open", true);
        dialog.setLayout(new BorderLayout(4, 4));

        final JTextField filterField = new JTextField();
        final DefaultListModel<File> listModel = new DefaultListModel<>();
        final JList<File> fileList = new JList<>(listModel);
        fileList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        fileList.setCellRenderer(new DefaultListCellRenderer() {
            @Override
            public Component getListCellRendererComponent(JList<?> list, Object value,
                    int index, boolean isSelected, boolean cellHasFocus) {
                Component c = super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
                if (value instanceof File) {
                    setText(quickOpenLabel((File) value));
                }
                return c;
            }
        });

        // Populate the list, keeping only entries that contain the (case-insensitive) filter.
        final Runnable refilter = new Runnable() {
            @Override
            public void run() {
                String needle = filterField.getText().trim().toLowerCase();
                listModel.clear();
                for (File f : allFiles) {
                    if (needle.isEmpty() || quickOpenLabel(f).toLowerCase().contains(needle)) {
                        listModel.addElement(f);
                    }
                }
                if (!listModel.isEmpty()) {
                    fileList.setSelectedIndex(0);
                }
            }
        };
        refilter.run();

        filterField.getDocument().addDocumentListener(new javax.swing.event.DocumentListener() {
            @Override public void insertUpdate(javax.swing.event.DocumentEvent e) { refilter.run(); }
            @Override public void removeUpdate(javax.swing.event.DocumentEvent e) { refilter.run(); }
            @Override public void changedUpdate(javax.swing.event.DocumentEvent e) { refilter.run(); }
        });

        // Action that opens the currently selected file and closes the dialog.
        final Runnable openSelected = new Runnable() {
            @Override
            public void run() {
                File selected = fileList.getSelectedValue();
                if (selected != null) {
                    dialog.dispose();
                    editorTabs.openFile(selected);
                }
            }
        };

        // Keyboard handling on the filter field: Enter opens, Down moves into the list,
        // Escape cancels.
        filterField.addKeyListener(new java.awt.event.KeyAdapter() {
            @Override
            public void keyPressed(java.awt.event.KeyEvent e) {
                int code = e.getKeyCode();
                if (code == KeyEvent.VK_ENTER) {
                    openSelected.run();
                } else if (code == KeyEvent.VK_ESCAPE) {
                    dialog.dispose();
                } else if (code == KeyEvent.VK_DOWN && !listModel.isEmpty()) {
                    int idx = Math.min(fileList.getSelectedIndex() + 1, listModel.getSize() - 1);
                    fileList.setSelectedIndex(Math.max(idx, 0));
                    fileList.ensureIndexIsVisible(fileList.getSelectedIndex());
                } else if (code == KeyEvent.VK_UP && !listModel.isEmpty()) {
                    int idx = Math.max(fileList.getSelectedIndex() - 1, 0);
                    fileList.setSelectedIndex(idx);
                    fileList.ensureIndexIsVisible(idx);
                }
            }
        });
        fileList.addMouseListener(new java.awt.event.MouseAdapter() {
            @Override
            public void mouseClicked(java.awt.event.MouseEvent e) {
                if (e.getClickCount() == 2) {
                    openSelected.run();
                }
            }
        });
        fileList.addKeyListener(new java.awt.event.KeyAdapter() {
            @Override
            public void keyPressed(java.awt.event.KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_ENTER) {
                    openSelected.run();
                } else if (e.getKeyCode() == KeyEvent.VK_ESCAPE) {
                    dialog.dispose();
                }
            }
        });

        JPanel content = new JPanel(new BorderLayout(4, 4));
        content.setBorder(BorderFactory.createEmptyBorder(8, 8, 8, 8));
        content.add(filterField, BorderLayout.NORTH);
        content.add(new JScrollPane(fileList), BorderLayout.CENTER);
        dialog.add(content, BorderLayout.CENTER);

        dialog.setSize(500, 360);
        dialog.setLocationRelativeTo(this);
        // Focus the filter field so the user can type-to-filter immediately.
        SwingUtilities.invokeLater(filterField::requestFocusInWindow);
        dialog.setVisible(true);
    }

    /**
     * Returns the project-relative path of a file for display in Quick Open, falling back
     * to the file name when it is not under the project root.
     */
    private String quickOpenLabel(File f) {
        if (currentProjectRoot != null) {
            String root = currentProjectRoot.getAbsolutePath();
            String path = f.getAbsolutePath();
            if (path.startsWith(root)) {
                String rel = path.substring(root.length());
                if (rel.startsWith(File.separator)) {
                    rel = rel.substring(1);
                }
                return rel.isEmpty() ? f.getName() : rel;
            }
        }
        return f.getName();
    }
    // END_CHANGE: ISS-IDE-M19

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

    // START_CHANGE: ISS-IDE-M17 - expose toolbar for context-sensitive state updates
    public PrologToolbar getToolbar() {
        return toolbar;
    }
    // END_CHANGE: ISS-IDE-M17
    
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