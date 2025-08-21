package it.denzosoft.jprolog.editor;

import it.denzosoft.jprolog.editor.util.DialogUtils;

import javax.swing.*;
import javax.swing.text.*;
import java.awt.*;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

/**
 * Panel dedicated to build/compilation results.
 * Clears itself on each new build and shows detailed outcome.
 */
public class BuildPanel extends JPanel {
    
    private JTextPane textPane;
    private StyledDocument document;
    private Style normalStyle;
    private Style errorStyle;
    private Style successStyle;
    private Style headerStyle;
    private Style warningStyle;
    
    // Colors for styles
    private static final Color ERROR_COLOR = new Color(220, 20, 60);
    private static final Color SUCCESS_COLOR = new Color(0, 128, 0);
    private static final Color WARNING_COLOR = new Color(255, 140, 0);
    private static final Color HEADER_COLOR = new Color(0, 0, 139);
    
    public BuildPanel() {
        initializeComponents();
        setupStyles();
    }
    
    /**
     * Initializes panel components.
     */
    private void initializeComponents() {
        setLayout(new BorderLayout());
        
        // Text pane with styles
        textPane = new JTextPane();
        textPane.setEditable(false);
        textPane.setFont(new Font("Consolas", Font.PLAIN, 12));
        textPane.setBackground(Color.WHITE);
        
        document = textPane.getStyledDocument();
        
        // Scroll pane
        JScrollPane scrollPane = new JScrollPane(textPane);
        scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        
        add(scrollPane, BorderLayout.CENTER);
        
        // Context menu
        setupContextMenu();
    }
    
    /**
     * Configures text styles.
     */
    private void setupStyles() {
        // Normal style
        normalStyle = textPane.addStyle("normal", null);
        StyleConstants.setForeground(normalStyle, Color.BLACK);
        StyleConstants.setFontFamily(normalStyle, "Consolas");
        StyleConstants.setFontSize(normalStyle, 12);
        
        // Error style
        errorStyle = textPane.addStyle("error", normalStyle);
        StyleConstants.setForeground(errorStyle, ERROR_COLOR);
        StyleConstants.setBold(errorStyle, true);
        
        // Success style
        successStyle = textPane.addStyle("success", normalStyle);
        StyleConstants.setForeground(successStyle, SUCCESS_COLOR);
        StyleConstants.setBold(successStyle, true);
        
        // Header style
        headerStyle = textPane.addStyle("header", normalStyle);
        StyleConstants.setForeground(headerStyle, HEADER_COLOR);
        StyleConstants.setBold(headerStyle, true);
        StyleConstants.setFontSize(headerStyle, 14);
        
        // Warning style
        warningStyle = textPane.addStyle("warning", normalStyle);
        StyleConstants.setForeground(warningStyle, WARNING_COLOR);
        StyleConstants.setBold(warningStyle, true);
    }
    
    /**
     * Configures the context menu.
     */
    private void setupContextMenu() {
        JPopupMenu contextMenu = new JPopupMenu();
        
        JMenuItem clearItem = new JMenuItem("Clear Build");
        clearItem.addActionListener(e -> clearBuild());
        
        JMenuItem copyItem = new JMenuItem("Copy All");
        copyItem.addActionListener(e -> copyAll());
        
        JMenuItem saveItem = new JMenuItem("Save Build Log...");
        saveItem.addActionListener(e -> saveBuildLog());
        
        contextMenu.add(clearItem);
        contextMenu.addSeparator();
        contextMenu.add(copyItem);
        contextMenu.add(saveItem);
        
        textPane.setComponentPopupMenu(contextMenu);
    }
    
    /**
     * Starts a new build (clears the panel).
     */
    public void startBuild(String buildType) {
        textPane.setText("");
        String timestamp = LocalDateTime.now().format(DateTimeFormatter.ofPattern("HH:mm:ss"));
        appendText("=== " + buildType + " - " + timestamp + " ===\n", headerStyle);
    }
    
    /**
     * Adds normal text.
     */
    public void appendText(String text) {
        appendText(text, normalStyle);
    }
    
    /**
     * Adds error text.
     */
    public void appendError(String text) {
        appendText(text, errorStyle);
    }
    
    /**
     * Adds success text.
     */
    public void appendSuccess(String text) {
        appendText(text, successStyle);
    }
    
    /**
     * Adds warning text.
     */
    public void appendWarning(String text) {
        appendText(text, warningStyle);
    }
    
    /**
     * Adds text with header.
     */
    public void appendHeader(String text) {
        appendText(text, headerStyle);
    }
    
    /**
     * Finishes the build with the result.
     */
    public void finishBuild(boolean success) {
        String timestamp = LocalDateTime.now().format(DateTimeFormatter.ofPattern("HH:mm:ss"));
        if (success) {
            appendSuccess("Build completed successfully - " + timestamp + "\n");
        } else {
            appendError("Build completed with errors - " + timestamp + "\n");
        }
        appendText("=== Build Complete ===\n\n", headerStyle);
        
        // Scroll to the end
        scrollToBottom();
    }
    
    /**
     * Adds text with a specific style.
     */
    private void appendText(String text, Style style) {
        try {
            document.insertString(document.getLength(), text, style);
        } catch (BadLocationException e) {
            // Ignore
        }
    }
    
    /**
     * Scroll down.
     */
    private void scrollToBottom() {
        SwingUtilities.invokeLater(() -> {
            textPane.setCaretPosition(document.getLength());
        });
    }
    
    /**
     * Clears the build panel.
     */
    private void clearBuild() {
        textPane.setText("");
    }
    
    /**
     * Copies all content.
     */
    private void copyAll() {
        textPane.selectAll();
        textPane.copy();
        textPane.setCaretPosition(document.getLength());
    }
    
    /**
     * Saves the build log.
     */
    private void saveBuildLog() {
        JFileChooser chooser = new JFileChooser();
        chooser.setDialogTitle("Save Build Log");
        chooser.setSelectedFile(new java.io.File("build.log"));
        
        if (chooser.showSaveDialog(this) == JFileChooser.APPROVE_OPTION) {
            try (java.io.FileWriter writer = new java.io.FileWriter(chooser.getSelectedFile())) {
                writer.write(textPane.getText());
                DialogUtils.showCenteredMessage(this, 
                    "Build log saved: " + chooser.getSelectedFile().getName(),
                    "Save", JOptionPane.INFORMATION_MESSAGE);
            } catch (Exception ex) {
                DialogUtils.showError(this,
                    "Error saving: " + ex.getMessage(),
                    "Error");
            }
        }
    }
    
    /**
     * Gets the panel content.
     */
    public String getText() {
        return textPane.getText();
    }
    
    /**
     * Checks if the panel is empty.
     */
    public boolean isEmpty() {
        return textPane.getText().trim().isEmpty();
    }
}