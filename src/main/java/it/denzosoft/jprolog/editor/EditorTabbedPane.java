package it.denzosoft.jprolog.editor;

import it.denzosoft.jprolog.editor.util.DialogUtils;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.*;
import java.util.List;

/**
 * Tabbed panel for managing multiple file editors.
 * Each tab represents an open file and includes functionality
 * such as saving, closing, indication of modified files.
 */
public class EditorTabbedPane extends JTabbedPane {
    
    private PrologIDE ide;
    private Map<File, FileEditor> openFiles;
    private Map<Component, FileEditor> tabToEditor;
    
    public EditorTabbedPane(PrologIDE ide) {
        this.ide = ide;
        this.openFiles = new HashMap<>();
        this.tabToEditor = new HashMap<>();
        
        setupTabbedPane();
    }
    
    /**
     * Configure the tabbed panel.
     */
    private void setupTabbedPane() {
        setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);
        
        // Listener for tab change
        addChangeListener(e -> {
            FileEditor currentEditor = getCurrentEditor();
            if (currentEditor != null) {
                ide.getStatusBar().setMessage("File: " + currentEditor.getFile().getAbsolutePath());
                currentEditor.requestFocusInWindow();
                
                // Notify PredicatePanel of file change
                if (ide.getPredicatePanel() != null) {
                    ide.getPredicatePanel().updatePredicatesForFile(currentEditor.getFile());
                }
            } else {
                ide.getStatusBar().setMessage("No file open");
                
                // Clear PredicatePanel if no file is open
                if (ide.getPredicatePanel() != null) {
                    ide.getPredicatePanel().updatePredicatesForFile(null);
                }
            }
        });
    }
    
    /**
     * Opens a file in the editor.
     */
    public void openFile(File file) {
        if (file == null || !file.exists() || !file.isFile()) {
            return;
        }
        
        // If file is already open, select the tab
        if (openFiles.containsKey(file)) {
            FileEditor editor = openFiles.get(file);
            int index = indexOfComponent(editor.getScrollPane());
            if (index >= 0) {
                setSelectedIndex(index);
            }
            return;
        }
        
        try {
            // Create new editor
            FileEditor editor = new FileEditor(file, ide);
            
            // Create tab panel with close button
            JPanel tabPanel = createTabPanel(editor);
            
            // Add the tab
            addTab(file.getName(), editor.getScrollPane());
            int index = getTabCount() - 1;
            setTabComponentAt(index, tabPanel);
            setSelectedIndex(index);
            
            // Register the editor
            openFiles.put(file, editor);
            tabToEditor.put(editor.getScrollPane(), editor);
            
            ide.getStatusBar().setMessage("File opened: " + file.getAbsolutePath());
            
        } catch (Exception ex) {
            DialogUtils.showError(ide, 
                "Error opening file: " + ex.getMessage(),
                "Error");
        }
    }
    
    /**
     * Create the tab panel with title and close button.
     */
    private JPanel createTabPanel(FileEditor editor) {
        JPanel tabPanel = new JPanel(new BorderLayout(5, 0));
        tabPanel.setOpaque(false);
        
        // Title label
        JLabel titleLabel = new JLabel(editor.getFile().getName());
        titleLabel.setFont(titleLabel.getFont().deriveFont(Font.PLAIN));
        tabPanel.add(titleLabel, BorderLayout.CENTER);
        
        // Close button
        JButton closeButton = new JButton("×");
        closeButton.setFont(closeButton.getFont().deriveFont(Font.BOLD, 16f));
        closeButton.setMargin(new Insets(0, 3, 0, 3));
        closeButton.setPreferredSize(new Dimension(20, 20));
        closeButton.setContentAreaFilled(false);
        closeButton.setBorderPainted(false);
        closeButton.setFocusable(false);
        
        closeButton.addActionListener(e -> closeFile(editor.getFile()));
        
        // Hover effect
        closeButton.addMouseListener(new java.awt.event.MouseAdapter() {
            @Override
            public void mouseEntered(java.awt.event.MouseEvent e) {
                closeButton.setContentAreaFilled(true);
                closeButton.setBackground(Color.RED);
                closeButton.setForeground(Color.WHITE);
            }
            
            @Override
            public void mouseExited(java.awt.event.MouseEvent e) {
                closeButton.setContentAreaFilled(false);
                closeButton.setForeground(Color.BLACK);
            }
        });
        
        tabPanel.add(closeButton, BorderLayout.EAST);
        
        // Listener for title update when file is modified
        editor.addModifiedListener(() -> {
            String title = editor.getFile().getName();
            if (editor.isModified()) {
                title += " *";
                titleLabel.setFont(titleLabel.getFont().deriveFont(Font.BOLD));
            } else {
                titleLabel.setFont(titleLabel.getFont().deriveFont(Font.PLAIN));
            }
            titleLabel.setText(title);
        });
        
        return tabPanel;
    }
    
    /**
     * Closes a file.
     */
    public void closeFile(File file) {
        FileEditor editor = openFiles.get(file);
        if (editor == null) {
            return;
        }
        
        // Check if file is modified
        if (editor.isModified()) {
            int choice = DialogUtils.showCenteredConfirm(ide,
                "The file '" + file.getName() + "' has been modified.\n" +
                "Do you want to save changes before closing?",
                "Modified File", 
                JOptionPane.YES_NO_CANCEL_OPTION,
                JOptionPane.WARNING_MESSAGE);
            
            if (choice == JOptionPane.YES_OPTION) {
                if (!editor.save()) {
                    return; // Don't close if save fails
                }
            } else if (choice == JOptionPane.CANCEL_OPTION) {
                return; // Cancel closing
            }
        }
        
        // Remove the tab
        Component scrollPane = editor.getScrollPane();
        int index = indexOfComponent(scrollPane);
        if (index >= 0) {
            removeTabAt(index);
        }
        
        // Remove from maps
        openFiles.remove(file);
        tabToEditor.remove(scrollPane);
        
        ide.getStatusBar().setMessage("File closed: " + file.getName());
    }
    
    /**
     * Close all files.
     */
    public void closeAllFiles() {
        // Create a copy of the list to avoid ConcurrentModificationException
        List<File> filesToClose = new ArrayList<>(openFiles.keySet());
        for (File file : filesToClose) {
            closeFile(file);
        }
    }
    
    /**
     * Save current file.
     */
    public void saveCurrentFile() {
        FileEditor currentEditor = getCurrentEditor();
        if (currentEditor != null) {
            currentEditor.save();
        }
    }
    
    /**
     * Save all open files.
     */
    public void saveAllFiles() {
        for (FileEditor editor : openFiles.values()) {
            if (editor.isModified()) {
                editor.save();
            }
        }
    }
    
    /**
     * Checks if there are unsaved files.
     */
    public boolean hasUnsavedFiles() {
        for (FileEditor editor : openFiles.values()) {
            if (editor.isModified()) {
                return true;
            }
        }
        return false;
    }
    
    /**
     * Ottiene l'editor correntemente selezionato.
     */
    public FileEditor getCurrentEditor() {
        Component selected = getSelectedComponent();
        if (selected != null) {
            return tabToEditor.get(selected);
        }
        return null;
    }
    
    /**
     * Ottiene l'editor per un file specifico.
     */
    public FileEditor getEditor(File file) {
        return openFiles.get(file);
    }
    
    /**
     * Updates a file reference after renaming.
     */
    public void updateFileReference(File oldFile, File newFile) {
        FileEditor editor = openFiles.remove(oldFile);
        if (editor != null) {
            editor.updateFile(newFile);
            openFiles.put(newFile, editor);
            
            // Aggiorna il titolo del tab
            Component scrollPane = editor.getScrollPane();
            int index = indexOfComponent(scrollPane);
            if (index >= 0) {
                setTitleAt(index, newFile.getName());
            }
        }
    }
    
    /**
     * Gets all open files.
     */
    public Set<File> getOpenFiles() {
        return new HashSet<>(openFiles.keySet());
    }
    
    /**
     * Gets all open editors.
     */
    public Collection<FileEditor> getEditors() {
        return openFiles.values();
    }
    
    /**
     * Seleziona il tab di un file specifico.
     */
    public void selectFile(File file) {
        FileEditor editor = openFiles.get(file);
        if (editor != null) {
            int index = indexOfComponent(editor.getScrollPane());
            if (index >= 0) {
                setSelectedIndex(index);
            }
        }
    }
    
    /**
     * Chiude il tab corrente.
     */
    public void closeCurrentFile() {
        FileEditor currentEditor = getCurrentEditor();
        if (currentEditor != null) {
            closeFile(currentEditor.getFile());
        }
    }
    
    /**
     * Naviga al tab precedente.
     */
    public void selectPreviousTab() {
        if (getTabCount() > 0) {
            int current = getSelectedIndex();
            int previous = (current - 1 + getTabCount()) % getTabCount();
            setSelectedIndex(previous);
        }
    }
    
    /**
     * Naviga al tab successivo.
     */
    public void selectNextTab() {
        if (getTabCount() > 0) {
            int current = getSelectedIndex();
            int next = (current + 1) % getTabCount();
            setSelectedIndex(next);
        }
    }
    
    /**
     * Checks if a file is open.
     */
    public boolean isFileOpen(File file) {
        return openFiles.containsKey(file);
    }
    
    /**
     * Ottiene il numero di file aperti.
     */
    public int getOpenFileCount() {
        return openFiles.size();
    }
}