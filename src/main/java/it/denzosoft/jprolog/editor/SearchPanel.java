package it.denzosoft.jprolog.editor;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.File;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Search panel for finding and replacing text in files.
 * Supports search in current file and entire project,
 * with options for case sensitivity, whole words, regex, etc.
 */
public class SearchPanel extends JPanel {
    
    private PrologIDE ide;
    
    // Interface components
    private JTextField searchField;
    private JTextField replaceField;
    private JCheckBox caseSensitiveBox;
    private JCheckBox wholeWordBox;
    private JCheckBox regexBox;
    private JButton findNextBtn;
    private JButton findPreviousBtn;
    private JButton replaceBtn;
    private JButton replaceAllBtn;
    private JButton findInProjectBtn;
    private JButton closeBtn;
    
    // Search mode
    private boolean projectSearchMode = false;
    
    // Project search results
    private List<SearchResult> searchResults;
    private JList<SearchResult> resultsList;
    private JScrollPane resultsScrollPane;
    
    public SearchPanel(PrologIDE ide) {
        this.ide = ide;
        this.searchResults = new ArrayList<>();
        
        initializeComponents();
        setupLayout();
        setupEventHandlers();
    }
    
    /**
     * Initializes panel components.
     */
    private void initializeComponents() {
        // Search field
        searchField = new JTextField(20);
        searchField.setFont(searchField.getFont().deriveFont(Font.PLAIN, 12f));
        
        // Replace field
        replaceField = new JTextField(20);
        replaceField.setFont(replaceField.getFont().deriveFont(Font.PLAIN, 12f));
        
        // Checkbox for options
        caseSensitiveBox = new JCheckBox("Case sensitive");
        caseSensitiveBox.setFont(caseSensitiveBox.getFont().deriveFont(Font.PLAIN, 11f));
        
        wholeWordBox = new JCheckBox("Whole word");
        wholeWordBox.setFont(wholeWordBox.getFont().deriveFont(Font.PLAIN, 11f));
        
        regexBox = new JCheckBox("Regex");
        regexBox.setFont(regexBox.getFont().deriveFont(Font.PLAIN, 11f));
        
        // Buttons
        findNextBtn = new JButton("Find");
        findPreviousBtn = new JButton("Previous");
        replaceBtn = new JButton("Replace");
        replaceAllBtn = new JButton("Replace All");
        findInProjectBtn = new JButton("Find in Project");
        closeBtn = new JButton("✕");
        
        // Button style
        Dimension btnSize = new Dimension(100, 25);
        findNextBtn.setPreferredSize(btnSize);
        findPreviousBtn.setPreferredSize(btnSize);
        replaceBtn.setPreferredSize(btnSize);
        replaceAllBtn.setPreferredSize(new Dimension(120, 25));
        findInProjectBtn.setPreferredSize(new Dimension(130, 25));
        
        closeBtn.setPreferredSize(new Dimension(25, 25));
        closeBtn.setMargin(new Insets(2, 2, 2, 2));
        closeBtn.setFont(closeBtn.getFont().deriveFont(Font.BOLD, 12f));
        
        // Results list for project search
        resultsList = new JList<>();
        resultsList.setCellRenderer(new SearchResultCellRenderer());
        resultsList.setFont(resultsList.getFont().deriveFont(Font.PLAIN, 11f));
        
        resultsScrollPane = new JScrollPane(resultsList);
        resultsScrollPane.setPreferredSize(new Dimension(0, 150));
        resultsScrollPane.setVisible(false);
    }
    
    /**
     * Configures the panel layout.
     */
    private void setupLayout() {
        setLayout(new BorderLayout());
        setBorder(new EmptyBorder(5, 5, 5, 5));
        setBackground(new Color(245, 245, 245));
        
        // Main panel
        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setOpaque(false);
        GridBagConstraints gbc = new GridBagConstraints();
        
        // First row: search field and buttons
        gbc.gridx = 0; gbc.gridy = 0; gbc.insets = new Insets(2, 2, 2, 2);
        mainPanel.add(new JLabel("Find:"), gbc);
        
        gbc.gridx = 1; gbc.fill = GridBagConstraints.HORIZONTAL; gbc.weightx = 1.0;
        mainPanel.add(searchField, gbc);
        
        gbc.gridx = 2; gbc.fill = GridBagConstraints.NONE; gbc.weightx = 0;
        mainPanel.add(findNextBtn, gbc);
        
        gbc.gridx = 3;
        mainPanel.add(findPreviousBtn, gbc);
        
        gbc.gridx = 4;
        mainPanel.add(findInProjectBtn, gbc);
        
        gbc.gridx = 5;
        mainPanel.add(closeBtn, gbc);
        
        // Second row: replace field and buttons
        gbc.gridx = 0; gbc.gridy = 1;
        mainPanel.add(new JLabel("Replace:"), gbc);
        
        gbc.gridx = 1; gbc.fill = GridBagConstraints.HORIZONTAL; gbc.weightx = 1.0;
        mainPanel.add(replaceField, gbc);
        
        gbc.gridx = 2; gbc.fill = GridBagConstraints.NONE; gbc.weightx = 0;
        mainPanel.add(replaceBtn, gbc);
        
        gbc.gridx = 3;
        mainPanel.add(replaceAllBtn, gbc);
        
        // Third row: options
        JPanel optionsPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 0));
        optionsPanel.setOpaque(false);
        optionsPanel.add(caseSensitiveBox);
        optionsPanel.add(wholeWordBox);
        optionsPanel.add(regexBox);
        
        gbc.gridx = 1; gbc.gridy = 2; gbc.gridwidth = 4;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(optionsPanel, gbc);
        
        add(mainPanel, BorderLayout.NORTH);
        add(resultsScrollPane, BorderLayout.CENTER);
    }
    
    /**
     * Configures event handlers.
     */
    private void setupEventHandlers() {
        // Enter in search field executes search
        searchField.addKeyListener(new KeyListener() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_ENTER) {
                    findNext();
                } else if (e.getKeyCode() == KeyEvent.VK_ESCAPE) {
                    hidePanel();
                }
            }
            
            @Override
            public void keyTyped(KeyEvent e) {}
            @Override
            public void keyReleased(KeyEvent e) {}
        });
        
        // Enter in replace field executes replacement
        replaceField.addKeyListener(new KeyListener() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_ENTER) {
                    replace();
                } else if (e.getKeyCode() == KeyEvent.VK_ESCAPE) {
                    hidePanel();
                }
            }
            
            @Override
            public void keyTyped(KeyEvent e) {}
            @Override
            public void keyReleased(KeyEvent e) {}
        });
        
        // Buttons
        findNextBtn.addActionListener(e -> findNext());
        findPreviousBtn.addActionListener(e -> findPrevious());
        replaceBtn.addActionListener(e -> replace());
        replaceAllBtn.addActionListener(e -> replaceAll());
        findInProjectBtn.addActionListener(e -> findInProject());
        closeBtn.addActionListener(e -> hidePanel());
        
        // Click on results list
        resultsList.addListSelectionListener(e -> {
            if (!e.getValueIsAdjusting()) {
                SearchResult selected = resultsList.getSelectedValue();
                if (selected != null) {
                    openSearchResult(selected);
                }
            }
        });
    }
    
    /**
     * Finds the next occurrence.
     */
    private void findNext() {
        String searchText = searchField.getText();
        if (searchText.isEmpty()) {
            return;
        }
        
        FileEditor currentEditor = ide.getEditorTabs().getCurrentEditor();
        if (currentEditor == null) {
            showMessage("No file open");
            return;
        }
        
        boolean found = currentEditor.findText(searchText, caseSensitiveBox.isSelected(), wholeWordBox.isSelected());
        if (!found) {
            showMessage("Text not found: " + searchText);
        }
    }
    
    /**
     * Finds the previous occurrence.
     */
    private void findPrevious() {
        // Simplified implementation - in a complete implementation
        // should search backwards
        findNext();
    }
    
    /**
     * Replaces the current occurrence.
     */
    private void replace() {
        String searchText = searchField.getText();
        String replaceText = replaceField.getText();
        
        if (searchText.isEmpty()) {
            return;
        }
        
        FileEditor currentEditor = ide.getEditorTabs().getCurrentEditor();
        if (currentEditor == null) {
            showMessage("No file open");
            return;
        }
        
        boolean replaced = currentEditor.replaceText(searchText, replaceText, caseSensitiveBox.isSelected());
        if (replaced) {
            showMessage("Text replaced");
        } else {
            showMessage("No selected text to replace");
        }
    }
    
    /**
     * Replaces all occurrences.
     */
    private void replaceAll() {
        String searchText = searchField.getText();
        String replaceText = replaceField.getText();
        
        if (searchText.isEmpty()) {
            return;
        }
        
        FileEditor currentEditor = ide.getEditorTabs().getCurrentEditor();
        if (currentEditor == null) {
            showMessage("No file open");
            return;
        }
        
        int count = currentEditor.replaceAllText(searchText, replaceText, caseSensitiveBox.isSelected());
        showMessage("Replaced " + count + " occurrences");
    }
    
    /**
     * Searches all project files using the new SearchResultsPanel.
     */
    private void findInProject() {
        String searchText = searchField.getText();
        if (searchText.isEmpty()) {
            return;
        }
        
        File projectRoot = ide.getCurrentProjectRoot();
        if (projectRoot == null) {
            showMessage("No project open");
            return;
        }
        
        // Use new search system with SearchResultsPanel
        java.util.Map<File, java.util.List<SearchResultsPanel.SearchMatch>> results = 
            new java.util.HashMap<>();
        findInDirectoryNew(projectRoot, searchText, results);
        
        // Show results in Search tab
        ide.getBottomTabbedPane().setSearchResults(results, searchText);
        
        // Calculate statistics
        int totalMatches = results.values().stream().mapToInt(java.util.List::size).sum();
        int fileCount = results.size();
        showMessage("Found " + totalMatches + " occurrences in " + fileCount + " files");
    }
    
    /**
     * Recursive search in a directory (new format for SearchResultsPanel).
     */
    private void findInDirectoryNew(File directory, String searchText, 
            java.util.Map<File, java.util.List<SearchResultsPanel.SearchMatch>> results) {
        File[] files = directory.listFiles();
        if (files == null) return;
        
        for (File file : files) {
            if (file.isDirectory() && !file.getName().startsWith(".")) {
                findInDirectoryNew(file, searchText, results);
            } else if (file.getName().toLowerCase().endsWith(".pl")) {
                java.util.List<SearchResultsPanel.SearchMatch> matches = findInFileNew(file, searchText);
                if (!matches.isEmpty()) {
                    results.put(file, matches);
                }
            }
        }
    }
    
    /**
     * Recursive search in a directory (old implementation).
     */
    private void findInDirectory(File directory, String searchText) {
        File[] files = directory.listFiles();
        if (files == null) return;
        
        for (File file : files) {
            if (file.isDirectory() && !file.getName().startsWith(".")) {
                findInDirectory(file, searchText);
            } else if (file.getName().toLowerCase().endsWith(".pl")) {
                findInFile(file, searchText);
            }
        }
    }
    
    /**
     * Search in a single file (new format for SearchResultsPanel).
     */
    private java.util.List<SearchResultsPanel.SearchMatch> findInFileNew(File file, String searchText) {
        java.util.List<SearchResultsPanel.SearchMatch> matches = new ArrayList<>();
        
        try {
            List<String> lines = Files.readAllLines(file.toPath());
            
            Pattern pattern;
            if (regexBox.isSelected()) {
                int flags = caseSensitiveBox.isSelected() ? 0 : Pattern.CASE_INSENSITIVE;
                pattern = Pattern.compile(searchText, flags);
            } else {
                String quotedText = Pattern.quote(searchText);
                if (wholeWordBox.isSelected()) {
                    quotedText = "\\b" + quotedText + "\\b";
                }
                int flags = caseSensitiveBox.isSelected() ? 0 : Pattern.CASE_INSENSITIVE;
                pattern = Pattern.compile(quotedText, flags);
            }
            
            for (int i = 0; i < lines.size(); i++) {
                String line = lines.get(i);
                Matcher matcher = pattern.matcher(line);
                
                while (matcher.find()) {
                    SearchResultsPanel.SearchMatch match = new SearchResultsPanel.SearchMatch(
                        i + 1, line, matcher.group(), matcher.start(), matcher.end());
                    matches.add(match);
                }
            }
        } catch (Exception e) {
            // Ignore file reading errors
        }
        
        return matches;
    }
    
    /**
     * Search in a single file.
     */
    private void findInFile(File file, String searchText) {
        try {
            List<String> lines = Files.readAllLines(file.toPath());
            
            Pattern pattern;
            if (regexBox.isSelected()) {
                int flags = caseSensitiveBox.isSelected() ? 0 : Pattern.CASE_INSENSITIVE;
                pattern = Pattern.compile(searchText, flags);
            } else {
                String quotedText = Pattern.quote(searchText);
                if (wholeWordBox.isSelected()) {
                    quotedText = "\\b" + quotedText + "\\b";
                }
                int flags = caseSensitiveBox.isSelected() ? 0 : Pattern.CASE_INSENSITIVE;
                pattern = Pattern.compile(quotedText, flags);
            }
            
            for (int i = 0; i < lines.size(); i++) {
                String line = lines.get(i);
                Matcher matcher = pattern.matcher(line);
                
                while (matcher.find()) {
                    SearchResult result = new SearchResult(
                        file, i + 1, matcher.start(), matcher.end(), 
                        line.trim(), searchText);
                    searchResults.add(result);
                }
            }
            
        } catch (Exception e) {
            // Ignore file reading errors
        }
    }
    
    /**
     * Counts unique files in results.
     */
    private int countUniqueFiles() {
        return (int) searchResults.stream()
                .map(r -> r.getFile())
                .distinct()
                .count();
    }
    
    /**
     * Opens a search result in the editor.
     */
    private void openSearchResult(SearchResult result) {
        ide.getEditorTabs().openFile(result.getFile());
        FileEditor editor = ide.getEditorTabs().getEditor(result.getFile());
        
        if (editor != null) {
            editor.goToLine(result.getLineNumber());
            
            // Highlight found text
            JTextArea textArea = editor.getTextArea();
            try {
                int lineStart = textArea.getLineStartOffset(result.getLineNumber() - 1);
                int highlightStart = lineStart + result.getColumnStart();
                int highlightEnd = lineStart + result.getColumnEnd();
                
                textArea.select(highlightStart, highlightEnd);
            } catch (Exception e) {
                // Ignore positioning errors
            }
        }
    }
    
    /**
     * Shows a status message.
     */
    private void showMessage(String message) {
        ide.getStatusBar().setMessage(message);
    }
    
    /**
     * Hides the search panel.
     */
    private void hidePanel() {
        setVisible(false);
        resultsScrollPane.setVisible(false);
        
        // Return focus to editor
        FileEditor currentEditor = ide.getEditorTabs().getCurrentEditor();
        if (currentEditor != null) {
            currentEditor.requestFocus();
        }
    }
    
    /**
     * Sets focus on the search field.
     */
    public void focusSearchField() {
        searchField.requestFocus();
        searchField.selectAll();
    }
    
    /**
     * Sets project search mode.
     */
    public void setProjectSearchMode(boolean projectMode) {
        this.projectSearchMode = projectMode;
        if (projectMode) {
            findInProjectBtn.setEnabled(true);
        }
    }
    
    /**
     * Class to represent a search result.
     */
    private static class SearchResult {
        private File file;
        private int lineNumber;
        private int columnStart;
        private int columnEnd;
        private String lineText;
        private String searchText;
        
        public SearchResult(File file, int lineNumber, int columnStart, int columnEnd, 
                           String lineText, String searchText) {
            this.file = file;
            this.lineNumber = lineNumber;
            this.columnStart = columnStart;
            this.columnEnd = columnEnd;
            this.lineText = lineText;
            this.searchText = searchText;
        }
        
        public File getFile() { return file; }
        public int getLineNumber() { return lineNumber; }
        public int getColumnStart() { return columnStart; }
        public int getColumnEnd() { return columnEnd; }
        public String getLineText() { return lineText; }
        public String getSearchText() { return searchText; }
        
        @Override
        public String toString() {
            return file.getName() + ":" + lineNumber + " - " + lineText;
        }
    }
    
    /**
     * Custom renderer for search results.
     */
    private static class SearchResultCellRenderer extends DefaultListCellRenderer {
        @Override
        public Component getListCellRendererComponent(JList<?> list, Object value,
                int index, boolean isSelected, boolean cellHasFocus) {
            
            super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
            
            if (value instanceof SearchResult) {
                SearchResult result = (SearchResult) value;
                
                String fileName = result.getFile().getName();
                String lineNum = String.valueOf(result.getLineNumber());
                String lineText = result.getLineText();
                
                // Limit line text length
                if (lineText.length() > 80) {
                    lineText = lineText.substring(0, 77) + "...";
                }
                
                setText(fileName + ":" + lineNum + " - " + lineText);
                setFont(getFont().deriveFont(Font.PLAIN, 11f));
            }
            
            return this;
        }
    }
}