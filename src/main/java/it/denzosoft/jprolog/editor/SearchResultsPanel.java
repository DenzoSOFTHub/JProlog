package it.denzosoft.jprolog.editor;

import javax.swing.*;
import javax.swing.tree.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Panel for search results represented as a tree.
 * Shows all files with matches and code lines containing the match.
 */
public class SearchResultsPanel extends JPanel {
    
    private JTree resultsTree;
    private DefaultMutableTreeNode rootNode;
    private DefaultTreeModel treeModel;
    private PrologIDE ide;
    private JLabel statusLabel;
    private Map<File, List<SearchMatch>> searchResults;
    
    /**
     * Class to represent a search match.
     */
    public static class SearchMatch {
        public final int lineNumber;
        public final String lineContent;
        public final String matchedText;
        public final int columnStart;
        public final int columnEnd;
        
        public SearchMatch(int lineNumber, String lineContent, String matchedText, int columnStart, int columnEnd) {
            this.lineNumber = lineNumber;
            this.lineContent = lineContent.trim();
            this.matchedText = matchedText;
            this.columnStart = columnStart;
            this.columnEnd = columnEnd;
        }
        
        @Override
        public String toString() {
            return "Line " + lineNumber + ": " + lineContent;
        }
    }
    
    /**
     * Custom node for the results tree.
     */
    private static class SearchResultNode extends DefaultMutableTreeNode {
        private File file;
        private SearchMatch match;
        private NodeType type;
        
        enum NodeType {
            ROOT, FILE, MATCH
        }
        
        public SearchResultNode(String text, NodeType type) {
            super(text);
            this.type = type;
        }
        
        public SearchResultNode(File file) {
            super(file.getName() + " (" + file.getParent() + ")");
            this.file = file;
            this.type = NodeType.FILE;
        }
        
        public SearchResultNode(SearchMatch match) {
            super(match.toString());
            this.match = match;
            this.type = NodeType.MATCH;
        }
        
        public NodeType getType() { return type; }
        public File getFile() { return file; }
        public SearchMatch getMatch() { return match; }
    }
    
    public SearchResultsPanel(PrologIDE ide) {
        this.ide = ide;
        this.searchResults = new HashMap<>();
        initializeComponents();
        setupEventHandlers();
    }
    
    /**
     * Initializes panel components.
     */
    private void initializeComponents() {
        setLayout(new BorderLayout());
        
        // Status label
        statusLabel = new JLabel("No search performed");
        statusLabel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        statusLabel.setFont(new Font("Dialog", Font.PLAIN, 11));
        statusLabel.setForeground(Color.GRAY);
        add(statusLabel, BorderLayout.NORTH);
        
        // Results tree
        rootNode = new SearchResultNode("Search Results", SearchResultNode.NodeType.ROOT);
        treeModel = new DefaultTreeModel(rootNode);
        resultsTree = new JTree(treeModel);
        
        // Tree configuration
        resultsTree.setRootVisible(false);
        resultsTree.setShowsRootHandles(true);
        resultsTree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        
        // Custom renderer for different icons
        resultsTree.setCellRenderer(new SearchResultCellRenderer());
        
        // Scroll pane
        JScrollPane scrollPane = new JScrollPane(resultsTree);
        scrollPane.setPreferredSize(new Dimension(300, 0));
        add(scrollPane, BorderLayout.CENTER);
        
        // Control panel
        JPanel controlPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        
        JButton expandAllButton = new JButton("Expand All");
        expandAllButton.addActionListener(e -> expandAll());
        
        JButton collapseAllButton = new JButton("Collapse All");
        collapseAllButton.addActionListener(e -> collapseAll());
        
        JButton clearButton = new JButton("Clear");
        clearButton.addActionListener(e -> clearResults());
        
        controlPanel.add(expandAllButton);
        controlPanel.add(collapseAllButton);
        controlPanel.add(clearButton);
        
        add(controlPanel, BorderLayout.SOUTH);
    }
    
    /**
     * Configure event handlers.
     */
    private void setupEventHandlers() {
        // Double click to open file/go to line
        resultsTree.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                if (e.getClickCount() == 2) {
                    TreePath path = resultsTree.getPathForLocation(e.getX(), e.getY());
                    if (path != null) {
                        SearchResultNode node = (SearchResultNode) path.getLastPathComponent();
                        handleNodeDoubleClick(node);
                    }
                }
            }
        });
    }
    
    /**
     * Handles double click on a node.
     */
    private void handleNodeDoubleClick(SearchResultNode node) {
        switch (node.getType()) {
            case FILE:
                // Open the file
                if (node.getFile() != null) {
                    ide.getEditorTabs().openFile(node.getFile());
                }
                break;
                
            case MATCH:
                // Open the file and go to the match line
                SearchResultNode fileNode = (SearchResultNode) node.getParent();
                if (fileNode != null && fileNode.getFile() != null) {
                    ide.getEditorTabs().openFile(fileNode.getFile());
                    FileEditor editor = ide.getEditorTabs().getEditor(fileNode.getFile());
                    if (editor != null) {
                        editor.goToLine(node.getMatch().lineNumber);
                        // Highlight the match
                        highlightMatch(editor, node.getMatch());
                    }
                }
                break;
        }
    }
    
    /**
     * Highlights the match in the editor.
     */
    private void highlightMatch(FileEditor editor, SearchMatch match) {
        JTextArea textArea = editor.getTextArea();
        try {
            int lineStart = textArea.getLineStartOffset(match.lineNumber - 1);
            int matchStart = lineStart + match.columnStart;
            int matchEnd = lineStart + match.columnEnd;
            
            textArea.select(matchStart, matchEnd);
            textArea.requestFocus();
        } catch (Exception e) {
            // Fallback: select the entire line
            editor.goToLine(match.lineNumber);
        }
    }
    
    /**
     * Sets the search results.
     */
    public void setSearchResults(Map<File, List<SearchMatch>> results, String searchTerm) {
        this.searchResults = results;
        
        // Clear the tree
        rootNode.removeAllChildren();
        
        int totalMatches = 0;
        int fileCount = 0;
        
        // Populate the tree
        for (Map.Entry<File, List<SearchMatch>> entry : results.entrySet()) {
            File file = entry.getKey();
            List<SearchMatch> matches = entry.getValue();
            
            if (!matches.isEmpty()) {
                fileCount++;
                totalMatches += matches.size();
                
                SearchResultNode fileNode = new SearchResultNode(file);
                rootNode.add(fileNode);
                
                for (SearchMatch match : matches) {
                    SearchResultNode matchNode = new SearchResultNode(match);
                    fileNode.add(matchNode);
                }
            }
        }
        
        // Update the model
        treeModel.reload();
        
        // Expand all files with few matches
        expandNodesWithFewMatches();
        
        // Update the status
        if (totalMatches > 0) {
            statusLabel.setText(String.format("'%s': %d matches in %d files", 
                searchTerm, totalMatches, fileCount));
        } else {
            statusLabel.setText("No results for: " + searchTerm);
        }
    }
    
    /**
     * Clears the results.
     */
    public void clearResults() {
        rootNode.removeAllChildren();
        treeModel.reload();
        searchResults.clear();
        statusLabel.setText("No search performed");
    }
    
    /**
     * Expands all nodes.
     */
    private void expandAll() {
        for (int i = 0; i < resultsTree.getRowCount(); i++) {
            resultsTree.expandRow(i);
        }
    }
    
    /**
     * Collapses all nodes.
     */
    private void collapseAll() {
        for (int i = resultsTree.getRowCount() - 1; i >= 0; i--) {
            resultsTree.collapseRow(i);
        }
    }
    
    /**
     * Automatically expands nodes with few matches.
     */
    private void expandNodesWithFewMatches() {
        for (int i = 0; i < rootNode.getChildCount(); i++) {
            SearchResultNode fileNode = (SearchResultNode) rootNode.getChildAt(i);
            if (fileNode.getChildCount() <= 5) { // Expand if there are max 5 matches
                resultsTree.expandPath(new TreePath(new Object[]{rootNode, fileNode}));
            }
        }
    }
    
    /**
     * Custom renderer for the tree.
     */
    private static class SearchResultCellRenderer extends DefaultTreeCellRenderer {
        private Icon fileIcon = UIManager.getIcon("FileView.fileIcon");
        private Icon matchIcon = UIManager.getIcon("FileView.computerIcon");
        
        @Override
        public Component getTreeCellRendererComponent(JTree tree, Object value,
                boolean sel, boolean expanded, boolean leaf, int row, boolean hasFocus) {
            
            super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus);
            
            if (value instanceof SearchResultNode) {
                SearchResultNode node = (SearchResultNode) value;
                switch (node.getType()) {
                    case FILE:
                        setIcon(fileIcon);
                        break;
                    case MATCH:
                        setIcon(matchIcon);
                        break;
                }
            }
            
            return this;
        }
    }
    
    /**
     * Checks if there are results.
     */
    public boolean hasResults() {
        return !searchResults.isEmpty();
    }
    
    /**
     * Gets the total number of matches.
     */
    public int getTotalMatches() {
        return searchResults.values().stream()
                .mapToInt(List::size)
                .sum();
    }
}