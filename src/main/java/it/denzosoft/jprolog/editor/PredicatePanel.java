package it.denzosoft.jprolog.editor;

import javax.swing.*;
import javax.swing.tree.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.util.*;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Panel that shows predicates and facts defined in the current file.
 * Allows quick navigation with double-click to go to definition.
 */
public class PredicatePanel extends JPanel {
    
    private JTree predicateTree;
    private DefaultMutableTreeNode rootNode;
    private DefaultTreeModel treeModel;
    private PrologIDE ide;
    private File currentFile;
    private JLabel fileLabel;
    
    // Pattern per riconoscere predicati e fatti
    private static final Pattern FACT_PATTERN = Pattern.compile(
        "^\\s*([a-z][a-zA-Z0-9_]*)(\\([^)]*\\))?\\s*\\.$"
    );
    private static final Pattern RULE_PATTERN = Pattern.compile(
        "^\\s*([a-z][a-zA-Z0-9_]*)(\\([^)]*\\))?\\s*:-"
    );
    private static final Pattern DIRECTIVE_PATTERN = Pattern.compile(
        "^\\s*:-\\s*(.+)\\.$"
    );
    
    /**
     * Class to represent a found predicate/fact.
     */
    public static class PredicateInfo {
        public final String name;
        public final String signature;
        public final int lineNumber;
        public final String type; // "fact", "rule", "directive"
        public final String fullText;
        
        public PredicateInfo(String name, String signature, int lineNumber, String type, String fullText) {
            this.name = name;
            this.signature = signature;
            this.lineNumber = lineNumber;
            this.type = type;
            this.fullText = fullText.trim();
        }
        
        @Override
        public String toString() {
            return signature + " (" + type + ")";
        }
    }
    
    /**
     * Custom node for the predicates tree.
     */
    private static class PredicateNode extends DefaultMutableTreeNode {
        private PredicateInfo predicateInfo;
        private NodeType type;
        
        enum NodeType {
            ROOT, CATEGORY, PREDICATE
        }
        
        public PredicateNode(String text, NodeType type) {
            super(text);
            this.type = type;
        }
        
        public PredicateNode(PredicateInfo info) {
            super(info.toString());
            this.predicateInfo = info;
            this.type = NodeType.PREDICATE;
        }
        
        public NodeType getNodeType() { return type; }
        public PredicateInfo getPredicateInfo() { return predicateInfo; }
    }
    
    public PredicatePanel(PrologIDE ide) {
        this.ide = ide;
        initializeComponents();
        setupEventHandlers();
    }
    
    /**
     * Initializes panel components.
     */
    private void initializeComponents() {
        setLayout(new BorderLayout());
        setBorder(BorderFactory.createTitledBorder("Predicates"));
        
        // Label per il file corrente
        fileLabel = new JLabel("No file selected");
        fileLabel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        fileLabel.setFont(new Font("Dialog", Font.ITALIC, 11));
        fileLabel.setForeground(Color.GRAY);
        add(fileLabel, BorderLayout.NORTH);
        
        // Albero dei predicati
        rootNode = new PredicateNode("Predicates", PredicateNode.NodeType.ROOT);
        treeModel = new DefaultTreeModel(rootNode);
        predicateTree = new JTree(treeModel);
        
        // Tree configuration
        predicateTree.setRootVisible(false);
        predicateTree.setShowsRootHandles(true);
        predicateTree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        
        // Renderer personalizzato per icone diverse
        predicateTree.setCellRenderer(new PredicateCellRenderer());
        
        // Scroll pane
        JScrollPane scrollPane = new JScrollPane(predicateTree);
        scrollPane.setPreferredSize(new Dimension(250, 200));
        add(scrollPane, BorderLayout.CENTER);
        
        // Pannello dei controlli
        JPanel controlPanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 2, 2));
        
        JButton refreshButton = new JButton("↻");
        refreshButton.setPreferredSize(new Dimension(25, 25));
        refreshButton.setToolTipText("Refresh predicates");
        refreshButton.addActionListener(e -> refreshPredicates());
        
        JButton expandAllButton = new JButton("▼");
        expandAllButton.setPreferredSize(new Dimension(25, 25));
        expandAllButton.setToolTipText("Expand all");
        expandAllButton.addActionListener(e -> expandAll());
        
        JButton collapseAllButton = new JButton("▶");
        collapseAllButton.setPreferredSize(new Dimension(25, 25));
        collapseAllButton.setToolTipText("Collapse all");
        collapseAllButton.addActionListener(e -> collapseAll());
        
        controlPanel.add(refreshButton);
        controlPanel.add(expandAllButton);
        controlPanel.add(collapseAllButton);
        
        add(controlPanel, BorderLayout.SOUTH);
    }
    
    /**
     * Configura gli event handlers.
     */
    private void setupEventHandlers() {
        // Doppio click per navigare al predicato
        predicateTree.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                if (e.getClickCount() == 2) {
                    TreePath path = predicateTree.getPathForLocation(e.getX(), e.getY());
                    if (path != null) {
                        PredicateNode node = (PredicateNode) path.getLastPathComponent();
                        if (node.getNodeType() == PredicateNode.NodeType.PREDICATE) {
                            navigateToPredicateDefinition(node.getPredicateInfo());
                        }
                    }
                }
            }
        });
    }
    
    /**
     * Updates predicates for the current file.
     */
    public void updatePredicatesForFile(File file) {
        this.currentFile = file;
        
        if (file != null) {
            fileLabel.setText(file.getName());
            refreshPredicates();
        } else {
            fileLabel.setText("No file selected");
            clearPredicates();
        }
    }
    
    /**
     * Aggiorna l'elenco dei predicati.
     */
    private void refreshPredicates() {
        if (currentFile == null || !currentFile.exists()) {
            clearPredicates();
            return;
        }
        
        try {
            List<String> lines = java.nio.file.Files.readAllLines(currentFile.toPath());
            List<PredicateInfo> predicates = parsePredicates(lines);
            populateTree(predicates);
        } catch (Exception e) {
            clearPredicates();
            // Show error in status bar
            if (ide != null && ide.getStatusBar() != null) {
                ide.getStatusBar().setMessage("Error parsing predicates: " + e.getMessage());
            }
        }
    }
    
    /**
     * Analizza il codice Prolog per trovare predicati e fatti.
     */
    private List<PredicateInfo> parsePredicates(List<String> lines) {
        List<PredicateInfo> predicates = new ArrayList<>();
        
        for (int i = 0; i < lines.size(); i++) {
            String line = lines.get(i).trim();
            int lineNumber = i + 1;
            
            // Salta righe vuote e commenti
            if (line.isEmpty() || line.startsWith("%")) {
                continue;
            }
            
            // Riconosce direttive
            Matcher directiveMatcher = DIRECTIVE_PATTERN.matcher(line);
            if (directiveMatcher.matches()) {
                String directive = directiveMatcher.group(1);
                predicates.add(new PredicateInfo("directive", ":-" + directive, lineNumber, "directive", line));
                continue;
            }
            
            // Riconosce regole
            Matcher ruleMatcher = RULE_PATTERN.matcher(line);
            if (ruleMatcher.matches()) {
                String name = ruleMatcher.group(1);
                String args = ruleMatcher.group(2);
                String signature = args != null ? name + args : name;
                
                // Per regole multi-linea, cerca la fine
                String fullRule = line;
                int j = i;
                while (j < lines.size() - 1 && !fullRule.trim().endsWith(".")) {
                    j++;
                    fullRule += " " + lines.get(j).trim();
                }
                
                predicates.add(new PredicateInfo(name, signature, lineNumber, "rule", fullRule));
                continue;
            }
            
            // Riconosce fatti
            Matcher factMatcher = FACT_PATTERN.matcher(line);
            if (factMatcher.matches()) {
                String name = factMatcher.group(1);
                String args = factMatcher.group(2);
                String signature = args != null ? name + args : name;
                
                predicates.add(new PredicateInfo(name, signature, lineNumber, "fact", line));
            }
        }
        
        return predicates;
    }
    
    /**
     * Popola l'albero con i predicati trovati.
     */
    private void populateTree(List<PredicateInfo> predicates) {
        rootNode.removeAllChildren();
        
        if (predicates.isEmpty()) {
            PredicateNode emptyNode = new PredicateNode("No predicates found", PredicateNode.NodeType.CATEGORY);
            rootNode.add(emptyNode);
            treeModel.reload();
            return;
        }
        
        // Raggruppa per tipo
        Map<String, List<PredicateInfo>> grouped = new LinkedHashMap<>();
        grouped.put("Facts", new ArrayList<>());
        grouped.put("Rules", new ArrayList<>());
        grouped.put("Directives", new ArrayList<>());
        
        for (PredicateInfo predicate : predicates) {
            switch (predicate.type) {
                case "fact":
                    grouped.get("Facts").add(predicate);
                    break;
                case "rule":
                    grouped.get("Rules").add(predicate);
                    break;
                case "directive":
                    grouped.get("Directives").add(predicate);
                    break;
            }
        }
        
        // Create nodes for categories
        for (Map.Entry<String, List<PredicateInfo>> entry : grouped.entrySet()) {
            List<PredicateInfo> categoryPredicates = entry.getValue();
            if (!categoryPredicates.isEmpty()) {
                PredicateNode categoryNode = new PredicateNode(
                    entry.getKey() + " (" + categoryPredicates.size() + ")", 
                    PredicateNode.NodeType.CATEGORY
                );
                rootNode.add(categoryNode);
                
                // Sort by name and add
                categoryPredicates.sort((a, b) -> a.name.compareTo(b.name));
                for (PredicateInfo predicate : categoryPredicates) {
                    PredicateNode predicateNode = new PredicateNode(predicate);
                    categoryNode.add(predicateNode);
                }
            }
        }
        
        treeModel.reload();
        
        // Expand automatically if there are few predicates
        if (predicates.size() <= 10) {
            expandAll();
        }
    }
    
    /**
     * Navigate to predicate definition in editor.
     */
    private void navigateToPredicateDefinition(PredicateInfo predicate) {
        if (currentFile == null || ide == null) {
            return;
        }
        
        // Open file if not already open
        ide.getEditorTabs().openFile(currentFile);
        
        // Get the editor for this file
        FileEditor editor = ide.getEditorTabs().getEditor(currentFile);
        if (editor != null) {
            // Go to predicate line
            editor.goToLine(predicate.lineNumber);
            
            // Aggiorna la status bar
            ide.getStatusBar().setMessage("Navigated to: " + predicate.signature + " (line " + predicate.lineNumber + ")");
        }
    }
    
    /**
     * Pulisce l'albero dei predicati.
     */
    private void clearPredicates() {
        rootNode.removeAllChildren();
        PredicateNode emptyNode = new PredicateNode("No file open", PredicateNode.NodeType.CATEGORY);
        rootNode.add(emptyNode);
        treeModel.reload();
    }
    
    /**
     * Expands all nodes.
     */
    private void expandAll() {
        for (int i = 0; i < predicateTree.getRowCount(); i++) {
            predicateTree.expandRow(i);
        }
    }
    
    /**
     * Collapses all nodes.
     */
    private void collapseAll() {
        for (int i = predicateTree.getRowCount() - 1; i >= 0; i--) {
            predicateTree.collapseRow(i);
        }
    }
    
    /**
     * Renderer personalizzato per l'albero.
     */
    private static class PredicateCellRenderer extends DefaultTreeCellRenderer {
        private Icon factIcon = new ColorIcon(new Color(0, 128, 0), 12); // Verde per fatti
        private Icon ruleIcon = new ColorIcon(new Color(0, 0, 255), 12);  // Blu per regole
        private Icon directiveIcon = new ColorIcon(new Color(255, 140, 0), 12); // Arancione per direttive
        private Icon categoryIcon = new ColorIcon(new Color(128, 128, 128), 12); // Grigio per categorie
        
        @Override
        public Component getTreeCellRendererComponent(JTree tree, Object value,
                boolean sel, boolean expanded, boolean leaf, int row, boolean hasFocus) {
            
            super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus);
            
            if (value instanceof PredicateNode) {
                PredicateNode node = (PredicateNode) value;
                
                switch (node.getNodeType()) {
                    case CATEGORY:
                        setIcon(categoryIcon);
                        break;
                    case PREDICATE:
                        PredicateInfo info = node.getPredicateInfo();
                        if (info != null) {
                            switch (info.type) {
                                case "fact":
                                    setIcon(factIcon);
                                    break;
                                case "rule":
                                    setIcon(ruleIcon);
                                    break;
                                case "directive":
                                    setIcon(directiveIcon);
                                    break;
                            }
                        }
                        break;
                }
            }
            
            return this;
        }
    }
    
    /**
     * Icona colorata semplice.
     */
    private static class ColorIcon implements Icon {
        private Color color;
        private int size;
        
        public ColorIcon(Color color, int size) {
            this.color = color;
            this.size = size;
        }
        
        @Override
        public void paintIcon(Component c, Graphics g, int x, int y) {
            Graphics2D g2 = (Graphics2D) g.create();
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g2.setColor(color);
            g2.fillOval(x + 2, y + 2, size - 4, size - 4);
            g2.setColor(color.darker());
            g2.drawOval(x + 2, y + 2, size - 4, size - 4);
            g2.dispose();
        }
        
        @Override
        public int getIconWidth() { return size; }
        
        @Override
        public int getIconHeight() { return size; }
    }
}