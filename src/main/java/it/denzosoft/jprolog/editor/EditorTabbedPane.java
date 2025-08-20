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
     * Configura il pannello con tab.
     */
    private void setupTabbedPane() {
        setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);
        
        // Listener per cambiamento tab
        addChangeListener(e -> {
            FileEditor currentEditor = getCurrentEditor();
            if (currentEditor != null) {
                ide.getStatusBar().setMessage("File: " + currentEditor.getFile().getAbsolutePath());
                currentEditor.requestFocusInWindow();
                
                // Notifica il PredicatePanel del cambio file
                if (ide.getPredicatePanel() != null) {
                    ide.getPredicatePanel().updatePredicatesForFile(currentEditor.getFile());
                }
            } else {
                ide.getStatusBar().setMessage("Nessun file aperto");
                
                // Clear PredicatePanel if no file is open
                if (ide.getPredicatePanel() != null) {
                    ide.getPredicatePanel().updatePredicatesForFile(null);
                }
            }
        });
    }
    
    /**
     * Apre un file nell'editor.
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
            // Crea nuovo editor
            FileEditor editor = new FileEditor(file, ide);
            
            // Crea il pannello tab con pulsante chiusura
            JPanel tabPanel = createTabPanel(editor);
            
            // Aggiunge il tab
            addTab(file.getName(), editor.getScrollPane());
            int index = getTabCount() - 1;
            setTabComponentAt(index, tabPanel);
            setSelectedIndex(index);
            
            // Registra l'editor
            openFiles.put(file, editor);
            tabToEditor.put(editor.getScrollPane(), editor);
            
            ide.getStatusBar().setMessage("File aperto: " + file.getAbsolutePath());
            
        } catch (Exception ex) {
            DialogUtils.showError(ide, 
                "Errore nell'apertura del file: " + ex.getMessage(),
                "Errore");
        }
    }
    
    /**
     * Crea il pannello del tab con titolo e pulsante chiusura.
     */
    private JPanel createTabPanel(FileEditor editor) {
        JPanel tabPanel = new JPanel(new BorderLayout(5, 0));
        tabPanel.setOpaque(false);
        
        // Label del titolo
        JLabel titleLabel = new JLabel(editor.getFile().getName());
        titleLabel.setFont(titleLabel.getFont().deriveFont(Font.PLAIN));
        tabPanel.add(titleLabel, BorderLayout.CENTER);
        
        // Pulsante chiusura
        JButton closeButton = new JButton("×");
        closeButton.setFont(closeButton.getFont().deriveFont(Font.BOLD, 16f));
        closeButton.setMargin(new Insets(0, 3, 0, 3));
        closeButton.setPreferredSize(new Dimension(20, 20));
        closeButton.setContentAreaFilled(false);
        closeButton.setBorderPainted(false);
        closeButton.setFocusable(false);
        
        closeButton.addActionListener(e -> closeFile(editor.getFile()));
        
        // Effetto hover
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
        
        // Listener per aggiornamento titolo quando il file viene modificato
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
     * Chiude un file.
     */
    public void closeFile(File file) {
        FileEditor editor = openFiles.get(file);
        if (editor == null) {
            return;
        }
        
        // Check if file is modified
        if (editor.isModified()) {
            int choice = DialogUtils.showCenteredConfirm(ide,
                "Il file '" + file.getName() + "' è stato modificato.\n" +
                "Vuoi salvare le modifiche prima di chiudere?",
                "File Modificato", 
                JOptionPane.YES_NO_CANCEL_OPTION,
                JOptionPane.WARNING_MESSAGE);
            
            if (choice == JOptionPane.YES_OPTION) {
                if (!editor.save()) {
                    return; // Non chiudere se il salvataggio fallisce
                }
            } else if (choice == JOptionPane.CANCEL_OPTION) {
                return; // Annulla chiusura
            }
        }
        
        // Rimuovi il tab
        Component scrollPane = editor.getScrollPane();
        int index = indexOfComponent(scrollPane);
        if (index >= 0) {
            removeTabAt(index);
        }
        
        // Rimuovi dalle mappe
        openFiles.remove(file);
        tabToEditor.remove(scrollPane);
        
        ide.getStatusBar().setMessage("File chiuso: " + file.getName());
    }
    
    /**
     * Chiude tutti i file.
     */
    public void closeAllFiles() {
        // Crea una copia della lista per evitare ConcurrentModificationException
        List<File> filesToClose = new ArrayList<>(openFiles.keySet());
        for (File file : filesToClose) {
            closeFile(file);
        }
    }
    
    /**
     * Salva il file corrente.
     */
    public void saveCurrentFile() {
        FileEditor currentEditor = getCurrentEditor();
        if (currentEditor != null) {
            currentEditor.save();
        }
    }
    
    /**
     * Salva tutti i file aperti.
     */
    public void saveAllFiles() {
        for (FileEditor editor : openFiles.values()) {
            if (editor.isModified()) {
                editor.save();
            }
        }
    }
    
    /**
     * Controlla se ci sono file non salvati.
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
     * Aggiorna il riferimento di un file dopo rinomin.
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
     * Ottiene tutti i file aperti.
     */
    public Set<File> getOpenFiles() {
        return new HashSet<>(openFiles.keySet());
    }
    
    /**
     * Ottiene tutti gli editor aperti.
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