package it.denzosoft.jprolog.editor;

import it.denzosoft.jprolog.editor.util.DialogUtils;

import javax.swing.*;
import javax.swing.tree.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.util.*;
import java.util.regex.Pattern;

/**
 * Tree component for displaying project structure.
 * Shows files and directories with appropriate icons and supports operations
 * such as opening files, creating/deleting, etc.
 */
public class ProjectTree extends JTree {
    
    private PrologIDE ide;
    private DefaultTreeModel treeModel;
    private DefaultMutableTreeNode rootNode;
    private File projectRoot;
    
    // To maintain node expansion state
    private Set<String> expandedPaths = new HashSet<>();
    
    // Icons for different file/directory types
    private static final Icon FOLDER_ICON = UIManager.getIcon("FileView.directoryIcon");
    private static final Icon FOLDER_OPEN_ICON = UIManager.getIcon("Tree.openIcon");
    private static final Icon PROLOG_FILE_ICON = UIManager.getIcon("FileView.fileIcon");
    
    public ProjectTree(PrologIDE ide) {
        this.ide = ide;
        
        // Basic tree configuration
        setupTree();
        setupRenderer();
        setupEventHandlers();
    }
    
    /**
     * Configures basic tree properties.
     */
    private void setupTree() {
        rootNode = new DefaultMutableTreeNode("No project");
        treeModel = new DefaultTreeModel(rootNode);
        setModel(treeModel);
        
        setRootVisible(true);
        setShowsRootHandles(true);
        getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        
        // Automatically expand root
        expandRow(0);
    }
    
    /**
     * Configure custom renderer for icons.
     */
    private void setupRenderer() {
        setCellRenderer(new DefaultTreeCellRenderer() {
            @Override
            public java.awt.Component getTreeCellRendererComponent(
                    JTree tree, Object value, boolean sel, boolean expanded,
                    boolean leaf, int row, boolean hasFocus) {
                
                super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus);
                
                if (value instanceof FileTreeNode) {
                    FileTreeNode node = (FileTreeNode) value;
                    File file = node.getFile();
                    
                    if (file.isDirectory()) {
                        setIcon(expanded ? FOLDER_OPEN_ICON : FOLDER_ICON);
                    } else if (file.getName().toLowerCase().endsWith(".pl")) {
                        setIcon(PROLOG_FILE_ICON);
                    } else {
                        setIcon(PROLOG_FILE_ICON);
                    }
                    
                    setText(file.getName());
                } else {
                    setIcon(FOLDER_ICON);
                }
                
                return this;
            }
        });
    }
    
    /**
     * Configura gli event handlers.
     */
    private void setupEventHandlers() {
        // Double-click to open file
        addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                if (e.getClickCount() == 2) {
                    TreePath path = getPathForLocation(e.getX(), e.getY());
                    if (path != null) {
                        Object node = path.getLastPathComponent();
                        if (node instanceof FileTreeNode) {
                            FileTreeNode fileNode = (FileTreeNode) node;
                            File file = fileNode.getFile();
                            
                            if (file.isFile() && file.getName().toLowerCase().endsWith(".pl")) {
                                ide.getEditorTabs().openFile(file);
                            }
                        }
                    }
                }
            }
            
            @Override
            public void mousePressed(MouseEvent e) {
                if (e.isPopupTrigger()) {
                    showContextMenu(e);
                }
            }
            
            @Override
            public void mouseReleased(MouseEvent e) {
                if (e.isPopupTrigger()) {
                    showContextMenu(e);
                }
            }
        });
    }
    
    /**
     * Mostra il menu contestuale.
     */
    private void showContextMenu(MouseEvent e) {
        TreePath path = getPathForLocation(e.getX(), e.getY());
        if (path != null) {
            setSelectionPath(path);
            Object node = path.getLastPathComponent();
            
            JPopupMenu popup = new JPopupMenu();
            
            if (node instanceof FileTreeNode) {
                FileTreeNode fileNode = (FileTreeNode) node;
                File file = fileNode.getFile();
                
                if (file.isDirectory()) {
                    // Menu for directory
                    JMenuItem newFile = new JMenuItem("New Prolog File...");
                    newFile.addActionListener(ev -> createNewFileInDirectory(file));
                    popup.add(newFile);
                    
                    JMenuItem newFolder = new JMenuItem("New Folder...");
                    newFolder.addActionListener(ev -> createNewFolderInDirectory(file));
                    popup.add(newFolder);
                    
                    popup.addSeparator();
                    
                    JMenuItem refresh = new JMenuItem("Refresh");
                    refresh.addActionListener(ev -> refreshNode(fileNode));
                    popup.add(refresh);
                    
                } else {
                    // Menu for file
                    JMenuItem open = new JMenuItem("Open");
                    open.addActionListener(ev -> ide.getEditorTabs().openFile(file));
                    popup.add(open);
                    
                    if (file.getName().toLowerCase().endsWith(".pl")) {
                        JMenuItem compile = new JMenuItem("Compile");
                        compile.addActionListener(ev -> compileFile(file));
                        popup.add(compile);
                    }
                }
                
                popup.addSeparator();
                
                JMenuItem delete = new JMenuItem("Delete...");
                delete.addActionListener(ev -> deleteFile(file));
                popup.add(delete);
                
                JMenuItem rename = new JMenuItem("Rename...");
                rename.addActionListener(ev -> renameFile(fileNode));
                popup.add(rename);
                
            } else {
                // Menu for project root
                JMenuItem newFile = new JMenuItem("New Prolog File...");
                newFile.addActionListener(ev -> {
                    if (projectRoot != null) {
                        createNewFileInDirectory(projectRoot);
                    }
                });
                popup.add(newFile);
                
                JMenuItem newFolder = new JMenuItem("New Folder...");
                newFolder.addActionListener(ev -> {
                    if (projectRoot != null) {
                        createNewFolderInDirectory(projectRoot);
                    }
                });
                popup.add(newFolder);
                
                popup.addSeparator();
                
                JMenuItem refresh = new JMenuItem("Refresh Project");
                refresh.addActionListener(ev -> refreshProject());
                popup.add(refresh);
            }
            
            popup.show(this, e.getX(), e.getY());
        } else {
            // Right-click on empty space - menu for project root
            if (projectRoot != null) {
                JPopupMenu popup = new JPopupMenu();
                
                JMenuItem newFile = new JMenuItem("New Prolog File...");
                newFile.addActionListener(ev -> createNewFileInDirectory(projectRoot));
                popup.add(newFile);
                
                JMenuItem newFolder = new JMenuItem("New Folder...");
                newFolder.addActionListener(ev -> createNewFolderInDirectory(projectRoot));
                popup.add(newFolder);
                
                popup.addSeparator();
                
                JMenuItem refresh = new JMenuItem("Refresh Project");
                refresh.addActionListener(ev -> refreshProject());
                popup.add(refresh);
                
                popup.show(this, e.getX(), e.getY());
            }
        }
    }
    
    /**
     * Load a project into the tree.
     */
    public void loadProject(File projectRoot) {
        this.projectRoot = projectRoot;
        
        rootNode.removeAllChildren();
        rootNode.setUserObject(projectRoot.getName());
        
        loadDirectoryContents(projectRoot, rootNode);
        treeModel.reload();
        
        // Espandi la radice
        expandRow(0);
    }
    
    /**
     * Carica ricorsivamente il contenuto di una directory.
     */
    private void loadDirectoryContents(File directory, DefaultMutableTreeNode parentNode) {
        File[] files = directory.listFiles();
        if (files == null) return;
        
        // Sort: directories first, then files, both alphabetically
        Arrays.sort(files, new Comparator<File>() {
            @Override
            public int compare(File f1, File f2) {
                if (f1.isDirectory() && !f2.isDirectory()) {
                    return -1;
                } else if (!f1.isDirectory() && f2.isDirectory()) {
                    return 1;
                } else {
                    return f1.getName().compareToIgnoreCase(f2.getName());
                }
            }
        });
        
        for (File file : files) {
            // Skip hidden files
            if (file.getName().startsWith(".")) {
                continue;
            }
            
            FileTreeNode fileNode = new FileTreeNode(file);
            parentNode.add(fileNode);
            
            // Recursively load subdirectories
            if (file.isDirectory()) {
                loadDirectoryContents(file, fileNode);
            }
        }
    }
    
    /**
     * Refresh entire project.
     */
    public void refreshProject() {
        if (projectRoot != null) {
            loadProject(projectRoot);
        }
    }
    
    /**
     * Aggiorna un nodo specifico.
     */
    private void refreshNode(FileTreeNode node) {
        File directory = node.getFile();
        if (directory.isDirectory()) {
            node.removeAllChildren();
            loadDirectoryContents(directory, node);
            treeModel.reload(node);
        }
    }
    
    /**
     * Creates a new Prolog file in a directory.
     */
    private void createNewFileInDirectory(File directory) {
        String fileName = DialogUtils.showCenteredInput(this, 
            "File name (without extension):", "New File", JOptionPane.QUESTION_MESSAGE);
        
        if (fileName != null && !fileName.trim().isEmpty()) {
            File newFile = new File(directory, fileName.trim() + ".pl");
            try {
                if (newFile.createNewFile()) {
                    refreshProject();
                    ide.getEditorTabs().openFile(newFile);
                    ide.getBottomTabbedPane().appendToOutput("New file created: " + newFile.getAbsolutePath() + "\n");
                } else {
                    DialogUtils.showError(this, 
                        "File already exists: " + newFile.getName(),
                        "Error");
                }
            } catch (Exception ex) {
                DialogUtils.showError(this, 
                    "Error creating file: " + ex.getMessage(),
                    "Error");
            }
        }
    }
    
    /**
     * Crea una nuova cartella in una directory.
     */
    private void createNewFolderInDirectory(File directory) {
        String folderName = DialogUtils.showCenteredInput(this, 
            "Folder name:", "New Folder", JOptionPane.QUESTION_MESSAGE);
        
        if (folderName != null && !folderName.trim().isEmpty()) {
            File newFolder = new File(directory, folderName.trim());
            if (newFolder.mkdirs()) {
                refreshProject();
                ide.getBottomTabbedPane().appendToOutput("New folder created: " + newFolder.getAbsolutePath() + "\n");
            } else {
                DialogUtils.showError(this, 
                    "Unable to create folder",
                    "Error");
            }
        }
    }
    
    /**
     * Elimina un file o directory.
     */
    private void deleteFile(File file) {
        String message = file.isDirectory() 
            ? "Delete folder '" + file.getName() + "' and all its contents?"
            : "Delete file '" + file.getName() + "'?";
            
        int choice = DialogUtils.showCenteredConfirm(this, message, 
            "Confirm Deletion", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);
        
        if (choice == JOptionPane.YES_OPTION) {
            try {
                deleteRecursive(file);
                refreshProject();
                
                // Close file in editor if it was open
                if (file.isFile()) {
                    ide.getEditorTabs().closeFile(file);
                }
                
                ide.getBottomTabbedPane().appendToOutput("Deleted: " + file.getAbsolutePath() + "\n");
            } catch (Exception ex) {
                DialogUtils.showError(this, 
                    "Error deleting: " + ex.getMessage(),
                    "Error");
            }
        }
    }
    
    /**
     * Recursively delete a file or directory.
     */
    private void deleteRecursive(File file) {
        if (file.isDirectory()) {
            File[] children = file.listFiles();
            if (children != null) {
                for (File child : children) {
                    deleteRecursive(child);
                }
            }
        }
        if (!file.delete()) {
            throw new RuntimeException("Unable to delete: " + file.getAbsolutePath());
        }
    }
    
    /**
     * Rename a file.
     */
    private void renameFile(FileTreeNode node) {
        File file = node.getFile();
        String newName = DialogUtils.showCenteredInput(this, 
            "New name:", file.getName());
        
        if (newName != null && !newName.trim().isEmpty()) {
            File newFile = new File(file.getParent(), newName.trim());
            if (file.renameTo(newFile)) {
                refreshProject();
                
                // Update editor if file was open
                ide.getEditorTabs().updateFileReference(file, newFile);
                
                ide.getBottomTabbedPane().appendToOutput("File renamed: " + file.getName() + " -> " + newName + "\n");
            } else {
                DialogUtils.showError(this, 
                    "Unable to rename file",
                    "Error");
            }
        }
    }
    
    /**
     * Compile a Prolog file.
     */
    private void compileFile(File file) {
        ide.getBottomTabbedPane().appendToOutput("Compiling " + file.getName() + "...\n");
        // Actual compilation is handled by the main IDE class
        // Here we can add specific behaviors for the tree
    }
    
    /**
     * Save current node expansion state.
     */
    private void saveExpandedState() {
        expandedPaths.clear();
        saveExpandedStateRecursive(rootNode, "");
    }
    
    /**
     * Recursively save expanded paths.
     */
    private void saveExpandedStateRecursive(DefaultMutableTreeNode node, String basePath) {
        TreePath path = new TreePath(node.getPath());
        
        if (isExpanded(path)) {
            String currentPath;
            if (node == rootNode) {
                currentPath = "";
            } else if (node instanceof FileTreeNode) {
                FileTreeNode fileNode = (FileTreeNode) node;
                currentPath = basePath.isEmpty() ? fileNode.getFile().getName() 
                    : basePath + File.separator + fileNode.getFile().getName();
            } else {
                return;
            }
            
            if (!currentPath.isEmpty()) {
                expandedPaths.add(currentPath);
            }
            
            // Continue recursively for children
            for (int i = 0; i < node.getChildCount(); i++) {
                DefaultMutableTreeNode child = (DefaultMutableTreeNode) node.getChildAt(i);
                saveExpandedStateRecursive(child, currentPath);
            }
        }
    }
    
    /**
     * Restore node expansion state.
     */
    private void restoreExpandedState() {
        // Always expand root
        expandPath(new TreePath(rootNode));
        
        // Restore other expanded paths
        for (String expandedPath : expandedPaths) {
            FileTreeNode node = findNodeByPath(expandedPath);
            if (node != null) {
                TreePath treePath = new TreePath(node.getPath());
                expandPath(treePath);
            }
        }
    }
    
    /**
     * Find a node based on relative path.
     */
    private FileTreeNode findNodeByPath(String relativePath) {
        if (relativePath.isEmpty()) {
            return null;
        }
        
        String[] pathComponents = relativePath.split(Pattern.quote(File.separator));
        DefaultMutableTreeNode currentNode = rootNode;
        
        for (String component : pathComponents) {
            FileTreeNode found = null;
            for (int i = 0; i < currentNode.getChildCount(); i++) {
                DefaultMutableTreeNode child = (DefaultMutableTreeNode) currentNode.getChildAt(i);
                if (child instanceof FileTreeNode) {
                    FileTreeNode fileChild = (FileTreeNode) child;
                    if (fileChild.getFile().getName().equals(component)) {
                        found = fileChild;
                        break;
                    }
                }
            }
            
            if (found == null) {
                return null;
            }
            currentNode = found;
        }
        
        return (FileTreeNode) currentNode;
    }
    
    /**
     * Seleziona ed espande il percorso fino al file specificato.
     */
    private void selectAndExpandPath(File file) {
        FileTreeNode node = findNodeByFile(file);
        if (node != null) {
            TreePath treePath = new TreePath(node.getPath());
            expandPath(treePath);
            setSelectionPath(treePath);
            scrollPathToVisible(treePath);
        }
    }
    
    /**
     * Trova un nodo in base al file.
     */
    private FileTreeNode findNodeByFile(File targetFile) {
        return findNodeByFileRecursive(rootNode, targetFile);
    }
    
    /**
     * Recursively finds a node for a file.
     */
    private FileTreeNode findNodeByFileRecursive(DefaultMutableTreeNode node, File targetFile) {
        if (node instanceof FileTreeNode) {
            FileTreeNode fileNode = (FileTreeNode) node;
            if (fileNode.getFile().equals(targetFile)) {
                return fileNode;
            }
        }
        
        for (int i = 0; i < node.getChildCount(); i++) {
            DefaultMutableTreeNode child = (DefaultMutableTreeNode) node.getChildAt(i);
            FileTreeNode result = findNodeByFileRecursive(child, targetFile);
            if (result != null) {
                return result;
            }
        }
        
        return null;
    }
    
    /**
     * Rimuove i percorsi eliminati dallo stato di espansione.
     */
    private void removeDeletedPathsFromExpandedState(File deletedFile) {
        String deletedPath = getRelativePathFromRoot(deletedFile);
        if (deletedPath != null) {
            Set<String> pathsToRemove = new HashSet<>();
            for (String path : expandedPaths) {
                if (path.startsWith(deletedPath)) {
                    pathsToRemove.add(path);
                }
            }
            expandedPaths.removeAll(pathsToRemove);
        }
    }
    
    /**
     * Updates expanded paths when a file is renamed.
     */
    private void updateExpandedPathsForRename(File oldFile, File newFile) {
        String oldPath = getRelativePathFromRoot(oldFile);
        String newPath = getRelativePathFromRoot(newFile);
        
        if (oldPath != null && newPath != null) {
            Set<String> updatedPaths = new HashSet<>();
            for (String path : expandedPaths) {
                if (path.equals(oldPath)) {
                    updatedPaths.add(newPath);
                } else if (path.startsWith(oldPath + File.separator)) {
                    String relativePart = path.substring(oldPath.length());
                    updatedPaths.add(newPath + relativePart);
                } else {
                    updatedPaths.add(path);
                }
            }
            expandedPaths = updatedPaths;
        }
    }
    
    /**
     * Gets the relative path from the project root.
     */
    private String getRelativePathFromRoot(File file) {
        if (projectRoot == null || !file.getAbsolutePath().startsWith(projectRoot.getAbsolutePath())) {
            return null;
        }
        
        String absolutePath = file.getAbsolutePath();
        String rootPath = projectRoot.getAbsolutePath();
        
        if (absolutePath.equals(rootPath)) {
            return "";
        }
        
        return absolutePath.substring(rootPath.length() + 1);
    }
    
    /**
     * Nodo personalizzato che contiene riferimento al file.
     */
    private static class FileTreeNode extends DefaultMutableTreeNode {
        private File file;
        
        public FileTreeNode(File file) {
            super(file.getName());
            this.file = file;
        }
        
        public File getFile() {
            return file;
        }
        
        @Override
        public String toString() {
            return file.getName();
        }
    }
}