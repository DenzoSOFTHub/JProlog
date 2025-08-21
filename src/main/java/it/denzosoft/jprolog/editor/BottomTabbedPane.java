package it.denzosoft.jprolog.editor;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;
import java.util.Map;

/**
 * Tabbed panel that replaces the single console.
 * Contains: Output, Build, Run, Search Results.
 */
public class BottomTabbedPane extends JTabbedPane {
    
    private OutputConsole outputPanel;
    private BuildPanel buildPanel;
    private RunPanel runPanel;
    private SearchResultsPanel searchResultsPanel;
    private DebugPanel debugPanel;
    private PrologIDE ide;
    
    // Tab indices
    public static final int OUTPUT_TAB = 0;
    public static final int BUILD_TAB = 1;
    public static final int RUN_TAB = 2;
    public static final int SEARCH_TAB = 3;
    public static final int DEBUG_TAB = 4;
    
    public BottomTabbedPane(PrologIDE ide) {
        this.ide = ide;
        initializeTabs();
        setupEventHandlers();
    }
    
    /**
     * Initializes all tabs.
     */
    private void initializeTabs() {
        // Tab Output (console generale)
        outputPanel = new OutputConsole();
        addTab("Output", createTabIcon("📄"), outputPanel, "Console di output generale");
        
        // Tab Build (risultati compilazione)
        buildPanel = new BuildPanel();
        addTab("Build", createTabIcon("🔨"), buildPanel, "Risultati di compilazione e build");
        
        // Tab Run (query interattive)
        runPanel = new RunPanel(ide);
        addTab("Run", createTabIcon("▶️"), runPanel, "Console per query Prolog interattive");
        
        // Tab Search Results (risultati ricerca)
        searchResultsPanel = new SearchResultsPanel(ide);
        addTab("Search", createTabIcon("🔍"), searchResultsPanel, "Risultati della ricerca");
        
        // Tab Debug (debug e trace)
        debugPanel = new DebugPanel(ide);
        addTab("Debug", createTabIcon("🐛"), debugPanel, "Debug e trace di programmi Prolog");
        
        // Seleziona il tab Output di default
        setSelectedIndex(OUTPUT_TAB);
        
        // Dimensione preferita
        setPreferredSize(new Dimension(0, 250));
    }
    
    /**
     * Creates an icon for tabs (placeholder for now).
     */
    private Icon createTabIcon(String emoji) {
        // Per ora ritorna null, in futuro si possono aggiungere icone vere
        return null;
    }
    
    /**
     * Configura gli event handlers.
     */
    private void setupEventHandlers() {
        // Listener per cambi di tab
        addChangeListener(e -> {
            int selectedIndex = getSelectedIndex();
            if (selectedIndex == RUN_TAB) {
                // Quando si seleziona Run, dai focus al prompt
                SwingUtilities.invokeLater(() -> runPanel.focusPrompt());
            }
        });
    }
    
    // ===================== METODI PER OUTPUT TAB =====================
    
    /**
     * Aggiunge testo al tab Output.
     */
    public void appendToOutput(String text) {
        outputPanel.appendText(text);
        // Non cambiare tab automaticamente per l'output generale
    }
    
    /**
     * Pulisce il tab Output.
     */
    public void clearOutput() {
        outputPanel.clear();
    }
    
    // ===================== METODI PER BUILD TAB =====================
    
    /**
     * Inizia una nuova build.
     */
    public void startBuild(String buildType) {
        buildPanel.startBuild(buildType);
        // Automatically switch to Build tab
        setSelectedIndex(BUILD_TAB);
        
        // Add notification badge if necessary
        updateTabTitle(BUILD_TAB, "Build", true);
    }
    
    /**
     * Add text to build.
     */
    public void appendToBuild(String text) {
        buildPanel.appendText(text);
    }
    
    /**
     * Add error to build.
     */
    public void appendBuildError(String text) {
        buildPanel.appendError(text);
    }
    
    /**
     * Add success to build.
     */
    public void appendBuildSuccess(String text) {
        buildPanel.appendSuccess(text);
    }
    
    /**
     * Aggiunge warning al build.
     */
    public void appendBuildWarning(String text) {
        buildPanel.appendWarning(text);
    }
    
    /**
     * Termina la build.
     */
    public void finishBuild(boolean success) {
        buildPanel.finishBuild(success);
        updateTabTitle(BUILD_TAB, "Build", false);
    }
    
    // ===================== METODI PER RUN TAB =====================
    
    /**
     * Mostra il tab Run e da focus al prompt.
     */
    public void showRunTab() {
        setSelectedIndex(RUN_TAB);
        runPanel.focusPrompt();
    }
    
    /**
     * Ottiene il contenuto dell'output Run.
     */
    public String getRunOutput() {
        return runPanel.getOutputText();
    }
    
    // ===================== METODI PER SEARCH TAB =====================
    
    /**
     * Imposta i risultati di ricerca.
     */
    public void setSearchResults(Map<java.io.File, List<SearchResultsPanel.SearchMatch>> results, String searchTerm) {
        searchResultsPanel.setSearchResults(results, searchTerm);
        
        if (searchResultsPanel.hasResults()) {
            // Automatically switch to Search tab if there are results
            setSelectedIndex(SEARCH_TAB);
            updateTabTitle(SEARCH_TAB, "Search (" + searchResultsPanel.getTotalMatches() + ")", true);
        } else {
            updateTabTitle(SEARCH_TAB, "Search", false);
        }
    }
    
    /**
     * Pulisce i risultati di ricerca.
     */
    public void clearSearchResults() {
        searchResultsPanel.clearResults();
        updateTabTitle(SEARCH_TAB, "Search", false);
    }
    
    // ===================== METODI PER DEBUG TAB =====================
    
    /**
     * Mostra il tab Debug.
     */
    public void showDebugTab() {
        setSelectedIndex(DEBUG_TAB);
    }
    
    /**
     * Ottieni il debug panel.
     */
    public DebugPanel getDebugPanel() {
        return debugPanel;
    }
    
    /**
     * Notifies that a debug session has started.
     */
    public void startDebugSession() {
        updateTabTitle(DEBUG_TAB, "Debug (Active)", true);
        setSelectedIndex(DEBUG_TAB);
    }
    
    /**
     * Notifies that a debug session has ended.
     */
    public void stopDebugSession() {
        updateTabTitle(DEBUG_TAB, "Debug", false);
    }
    
    /**
     * Passa al tab Output.
     */
    public void switchToOutputTab() {
        setSelectedIndex(OUTPUT_TAB);
    }
    
    // ===================== GETTERS =====================
    
    /**
     * Get the run panel.
     */
    public RunPanel getRunPanel() {
        return runPanel;
    }
    
    // ===================== UTILITY =====================
    
    /**
     * Updates a tab title with optional notification.
     */
    private void updateTabTitle(int tabIndex, String baseTitle, boolean hasNotification) {
        if (hasNotification) {
            setTitleAt(tabIndex, baseTitle + " 🔴");
        } else {
            setTitleAt(tabIndex, baseTitle);
        }
    }
    
    /**
     * Shows a temporary message in the IDE status bar.
     */
    private void showStatusMessage(String message) {
        if (ide != null && ide.getStatusBar() != null) {
            ide.getStatusBar().setMessage(message);
            
            // Remove message after 3 seconds
            Timer timer = new Timer(3000, new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    ide.getStatusBar().setMessage("Ready");
                }
            });
            timer.setRepeats(false);
            timer.start();
        }
    }
    
    // ===================== GETTERS =====================
    
    public OutputConsole getOutputPanel() {
        return outputPanel;
    }
    
    public BuildPanel getBuildPanel() {
        return buildPanel;
    }
    
    public SearchResultsPanel getSearchResultsPanel() {
        return searchResultsPanel;
    }
}