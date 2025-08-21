package it.denzosoft.jprolog.editor;

import javax.swing.*;
import javax.swing.text.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * Output console for displaying compilation results,
 * query execution, debug messages and other system output.
 * Includes input functionality for interactive queries.
 */
public class OutputConsole extends JTextPane {
    
    // Styles for different types of output
    private StyledDocument document;
    private Style normalStyle;
    private Style errorStyle;
    private Style successStyle;
    private Style warningStyle;
    private Style timestampStyle;
    private Style promptStyle;
    private Style resultStyle;
    
    // Gestione input
    private StringBuilder currentInput;
    private int promptPosition;
    private boolean inputMode;
    
    // Configurations
    private boolean showTimestamps = true;
    private int maxLines = 1000;
    private SimpleDateFormat timeFormat = new SimpleDateFormat("HH:mm:ss");
    
    public OutputConsole() {
        super();
        
        initializeStyles();
        setupConsole();
        setupEventHandlers();
        
        showWelcomeMessage();
    }
    
    /**
     * Initializes text styles.
     */
    private void initializeStyles() {
        document = getStyledDocument();
        
        // Stile normale
        normalStyle = document.addStyle("normal", null);
        StyleConstants.setFontFamily(normalStyle, "Consolas");
        StyleConstants.setFontSize(normalStyle, 12);
        StyleConstants.setForeground(normalStyle, Color.BLACK);
        
        // Stile errore
        errorStyle = document.addStyle("error", normalStyle);
        StyleConstants.setForeground(errorStyle, new Color(200, 0, 0));
        StyleConstants.setBold(errorStyle, true);
        
        // Stile successo
        successStyle = document.addStyle("success", normalStyle);
        StyleConstants.setForeground(successStyle, new Color(0, 150, 0));
        StyleConstants.setBold(successStyle, true);
        
        // Stile warning
        warningStyle = document.addStyle("warning", normalStyle);
        StyleConstants.setForeground(warningStyle, new Color(200, 100, 0));
        StyleConstants.setBold(warningStyle, true);
        
        // Stile timestamp
        timestampStyle = document.addStyle("timestamp", normalStyle);
        StyleConstants.setForeground(timestampStyle, new Color(128, 128, 128));
        StyleConstants.setFontSize(timestampStyle, 10);
        
        // Stile prompt
        promptStyle = document.addStyle("prompt", normalStyle);
        StyleConstants.setForeground(promptStyle, new Color(0, 0, 200));
        StyleConstants.setBold(promptStyle, true);
        
        // Stile risultati
        resultStyle = document.addStyle("result", normalStyle);
        StyleConstants.setForeground(resultStyle, new Color(0, 100, 0));
        StyleConstants.setItalic(resultStyle, true);
    }
    
    /**
     * Configura la console.
     */
    private void setupConsole() {
        setEditable(false);
        setBackground(Color.WHITE);
        setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        
        // Configure font
        Font consoleFont = new Font("Consolas", Font.PLAIN, 12);
        setFont(consoleFont);
        
        currentInput = new StringBuilder();
        inputMode = false;
        promptPosition = 0;
    }
    
    /**
     * Configure event handlers.
     */
    private void setupEventHandlers() {
        addKeyListener(new KeyListener() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (inputMode) {
                    handleInputKey(e);
                }
            }
            
            @Override
            public void keyTyped(KeyEvent e) {
                if (inputMode && e.getKeyChar() != KeyEvent.CHAR_UNDEFINED) {
                    handleInputChar(e);
                }
            }
            
            @Override
            public void keyReleased(KeyEvent e) {}
        });
        
        // Context menu
        setupContextMenu();
    }
    
    /**
     * Configure context menu.
     */
    private void setupContextMenu() {
        JPopupMenu contextMenu = new JPopupMenu();
        
        JMenuItem clearItem = new JMenuItem("Clear Console");
        clearItem.addActionListener(e -> clear());
        contextMenu.add(clearItem);
        
        JMenuItem copyItem = new JMenuItem("Copy");
        copyItem.addActionListener(e -> copy());
        contextMenu.add(copyItem);
        
        JMenuItem selectAllItem = new JMenuItem("Seleziona Tutto");
        selectAllItem.addActionListener(e -> selectAll());
        contextMenu.add(selectAllItem);
        
        contextMenu.addSeparator();
        
        JCheckBoxMenuItem timestampsItem = new JCheckBoxMenuItem("Mostra Timestamp", showTimestamps);
        timestampsItem.addActionListener(e -> {
            showTimestamps = timestampsItem.isSelected();
        });
        contextMenu.add(timestampsItem);
        
        setComponentPopupMenu(contextMenu);
    }
    
    /**
     * Mostra il messaggio di benvenuto.
     */
    private void showWelcomeMessage() {
        appendText("=== JProlog IDE Console ===\n", successStyle);
        appendText("Console pronta per output di compilazione, query e debug.\n\n", normalStyle);
    }
    
    /**
     * Adds normal text to the console.
     */
    public void appendText(String text) {
        appendText(text, normalStyle);
    }
    
    /**
     * Aggiunge testo con uno stile specifico.
     */
    public void appendText(String text, Style style) {
        SwingUtilities.invokeLater(() -> {
            try {
                // Add timestamp if enabled
                if (showTimestamps && !text.equals("\n")) {
                    String timestamp = "[" + timeFormat.format(new Date()) + "] ";
                    document.insertString(document.getLength(), timestamp, timestampStyle);
                }
                
                document.insertString(document.getLength(), text, style);
                
                // Maintain maximum number of lines
                limitLines();
                
                // Automatically scroll to bottom
                setCaretPosition(document.getLength());
                
            } catch (BadLocationException e) {
                // Ignore insertion errors
            }
        });
    }
    
    /**
     * Aggiunge testo di errore.
     */
    public void appendError(String text) {
        appendText("ERRORE: " + text + "\n", errorStyle);
    }
    
    /**
     * Aggiunge testo di successo.
     */
    public void appendSuccess(String text) {
        appendText(text + "\n", successStyle);
    }
    
    /**
     * Aggiunge testo di warning.
     */
    public void appendWarning(String text) {
        appendText("ATTENZIONE: " + text + "\n", warningStyle);
    }
    
    /**
     * Aggiunge una query Prolog formattata.
     */
    public void appendQuery(String query) {
        appendText("?- " + query + "\n", promptStyle);
    }
    
    /**
     * Aggiunge un risultato di query formattato.
     */
    public void appendResult(String result) {
        appendText(result + "\n", resultStyle);
    }
    
    /**
     * Aggiunge un'eccezione formattata.
     */
    public void appendException(Exception e) {
        appendError("Eccezione: " + e.getMessage());
        
        // Detailed stack trace in debug mode
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        e.printStackTrace(pw);
        
        appendText("Stack trace:\n" + sw.toString() + "\n", normalStyle);
    }
    
    /**
     * Aggiunge un separatore visivo.
     */
    public void appendSeparator() {
        appendText("" + "=".repeat(60) + "\n", normalStyle);
    }
    
    /**
     * Pulisce la console.
     */
    public void clear() {
        SwingUtilities.invokeLater(() -> {
            try {
                document.remove(0, document.getLength());
                showWelcomeMessage();
            } catch (BadLocationException e) {
                // Ignora errori di rimozione
            }
        });
    }
    
    /**
     * Limits the number of lines in the console.
     */
    private void limitLines() {
        try {
            String text = document.getText(0, document.getLength());
            String[] lines = text.split("\n");
            
            if (lines.length > maxLines) {
                // Remove older lines
                int linesToRemove = lines.length - maxLines + 100; // Remove a bit more for efficiency
                int removeLength = 0;
                
                for (int i = 0; i < linesToRemove && i < lines.length; i++) {
                    removeLength += lines[i].length() + 1; // +1 per \n
                }
                
                document.remove(0, removeLength);
            }
        } catch (BadLocationException e) {
            // Ignora errori
        }
    }
    
    /**
     * Starts input mode for interactive queries.
     */
    public void startInputMode(String prompt) {
        SwingUtilities.invokeLater(() -> {
            appendText(prompt, promptStyle);
            promptPosition = document.getLength();
            inputMode = true;
            currentInput.setLength(0);
            setEditable(true);
            setCaretPosition(document.getLength());
        });
    }
    
    /**
     * Ends input mode.
     */
    public void endInputMode() {
        SwingUtilities.invokeLater(() -> {
            inputMode = false;
            setEditable(false);
        });
    }
    
    /**
     * Handles input characters.
     */
    private void handleInputChar(KeyEvent e) {
        char ch = e.getKeyChar();
        
        if (ch == '\n' || ch == '\r') {
            // Enter: elabora input
            String input = currentInput.toString().trim();
            appendText("\n", normalStyle);
            
            if (!input.isEmpty()) {
                processInput(input);
            }
            
            endInputMode();
            e.consume();
            
        } else if (ch == '\b' || ch == KeyEvent.VK_BACK_SPACE) {
            // Backspace
            if (currentInput.length() > 0 && getCaretPosition() > promptPosition) {
                currentInput.setLength(currentInput.length() - 1);
                try {
                    document.remove(getCaretPosition() - 1, 1);
                } catch (BadLocationException ex) {
                    // Ignora
                }
            }
            e.consume();
            
        } else if (ch >= 32) { // Caratteri stampabili
            currentInput.append(ch);
            try {
                document.insertString(getCaretPosition(), String.valueOf(ch), normalStyle);
            } catch (BadLocationException ex) {
                // Ignora
            }
            e.consume();
        }
    }
    
    /**
     * Handles special keys in input mode.
     */
    private void handleInputKey(KeyEvent e) {
        switch (e.getKeyCode()) {
            case KeyEvent.VK_HOME:
                setCaretPosition(promptPosition);
                e.consume();
                break;
                
            case KeyEvent.VK_LEFT:
                if (getCaretPosition() <= promptPosition) {
                    e.consume();
                }
                break;
                
            case KeyEvent.VK_UP:
            case KeyEvent.VK_DOWN:
                // TODO: Implementare cronologia comandi
                e.consume();
                break;
        }
    }
    
    /**
     * Elabora l'input dell'utente.
     */
    private void processInput(String input) {
        // This method would be connected to the Prolog engine for interactive queries
        appendResult("Input ricevuto: " + input);
        appendText("(Elaborazione query interattive in sviluppo)\n", normalStyle);
    }
    
    /**
     * Imposta il numero massimo di righe.
     */
    public void setMaxLines(int maxLines) {
        this.maxLines = maxLines;
    }
    
    /**
     * Abilita/disabilita i timestamp.
     */
    public void setShowTimestamps(boolean show) {
        this.showTimestamps = show;
    }
    
    /**
     * Gets all console text.
     */
    public String getAllText() {
        try {
            return document.getText(0, document.getLength());
        } catch (BadLocationException e) {
            return "";
        }
    }
    
    /**
     * Saves console content to a file.
     */
    public void saveToFile(java.io.File file) throws java.io.IOException {
        try (java.io.FileWriter writer = new java.io.FileWriter(file)) {
            writer.write(getAllText());
        }
    }
    
    /**
     * Loads content from file into console.
     */
    public void loadFromFile(java.io.File file) throws java.io.IOException {
        clear();
        String content = new String(java.nio.file.Files.readAllBytes(file.toPath()));
        appendText(content, normalStyle);
    }
}