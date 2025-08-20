package it.denzosoft.jprolog.editor;

import it.denzosoft.jprolog.core.engine.Prolog;
import it.denzosoft.jprolog.core.terms.Term;

import javax.swing.*;
import javax.swing.text.*;
import java.awt.*;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * Panel that acts as a complete Prolog CLI terminal.
 * The text area serves as both input and output, like a real terminal.
 */
public class RunPanel extends JPanel {
    
    private JTextPane terminalArea;
    private StyledDocument document;
    private Style normalStyle;
    private Style queryStyle;
    private Style resultStyle;
    private Style errorStyle;
    private Style sideEffectStyle;
    private Style promptStyle;
    private Style commentStyle;
    private PrologIDE ide;
    
    // Terminal state
    private int promptPosition = 0;
    private boolean waitingForInput = false;
    private StringBuilder currentInput = new StringBuilder();
    private List<String> queryHistory = new ArrayList<>();
    private int historyIndex = -1;
    
    // For handling multi-line input
    private boolean inMultilineQuery = false;
    private StringBuilder multilineBuffer = new StringBuilder();
    
    // Colors for styles - Black text on white background for better readability
    private static final Color BACKGROUND_COLOR = Color.WHITE;
    private static final Color TEXT_COLOR = Color.BLACK;
    private static final Color QUERY_COLOR = new Color(0, 0, 139);  // Dark blue
    private static final Color RESULT_COLOR = new Color(0, 100, 0);  // Dark green
    private static final Color ERROR_COLOR = new Color(180, 0, 0);   // Dark red
    private static final Color SIDE_EFFECT_COLOR = new Color(128, 0, 128); // Purple
    private static final Color PROMPT_COLOR = new Color(0, 128, 0);  // Green
    private static final Color COMMENT_COLOR = Color.GRAY;
    
    // Console state
    private boolean isProcessingQuery = false;
    private Thread queryThread = null;
    
    public RunPanel(PrologIDE ide) {
        this.ide = ide;
        initializeComponents();
        setupStyles();
        setupEventHandlers();
        initializeTerminal();
    }
    
    /**
     * Initializes panel components.
     */
    private void initializeComponents() {
        setLayout(new BorderLayout());
        
        // Terminal area - acts as both input and output
        terminalArea = new JTextPane();
        terminalArea.setFont(new Font("Consolas", Font.PLAIN, 13));
        terminalArea.setBackground(BACKGROUND_COLOR);
        terminalArea.setForeground(TEXT_COLOR);
        terminalArea.setCaretColor(Color.BLACK);
        
        document = terminalArea.getStyledDocument();
        
        JScrollPane scrollPane = new JScrollPane(terminalArea);
        scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollPane.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        add(scrollPane, BorderLayout.CENTER);
        
        // Control panel at the top
        JPanel controlPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        controlPanel.setBackground(new Color(240, 240, 240));
        controlPanel.setBorder(BorderFactory.createMatteBorder(0, 0, 1, 0, Color.LIGHT_GRAY));
        
        JButton clearButton = new JButton("Clear");
        clearButton.setToolTipText("Clear the terminal screen");
        clearButton.addActionListener(e -> clearTerminal());
        
        JButton restartButton = new JButton("Restart");
        restartButton.setToolTipText("Restart Prolog engine and reload knowledge base");
        restartButton.addActionListener(e -> restartProlog());
        
        JButton interruptButton = new JButton("Interrupt");
        interruptButton.setToolTipText("Interrupt current query (Ctrl+C)");
        interruptButton.addActionListener(e -> interruptQuery());
        
        controlPanel.add(clearButton);
        controlPanel.add(restartButton);
        controlPanel.add(interruptButton);
        
        // Add a label to show status
        JLabel statusLabel = new JLabel("  Ready");
        statusLabel.setForeground(Color.GRAY);
        controlPanel.add(statusLabel);
        
        add(controlPanel, BorderLayout.NORTH);
    }
    
    /**
     * Setup text styles.
     */
    private void setupStyles() {
        normalStyle = terminalArea.addStyle("Normal", null);
        StyleConstants.setForeground(normalStyle, TEXT_COLOR);
        StyleConstants.setFontFamily(normalStyle, "Consolas");
        StyleConstants.setFontSize(normalStyle, 13);
        
        promptStyle = terminalArea.addStyle("Prompt", null);
        StyleConstants.setForeground(promptStyle, PROMPT_COLOR);
        StyleConstants.setBold(promptStyle, true);
        StyleConstants.setFontFamily(promptStyle, "Consolas");
        StyleConstants.setFontSize(promptStyle, 13);
        
        queryStyle = terminalArea.addStyle("Query", null);
        StyleConstants.setForeground(queryStyle, QUERY_COLOR);
        StyleConstants.setBold(queryStyle, false);
        StyleConstants.setFontFamily(queryStyle, "Consolas");
        StyleConstants.setFontSize(queryStyle, 13);
        
        resultStyle = terminalArea.addStyle("Result", null);
        StyleConstants.setForeground(resultStyle, RESULT_COLOR);
        StyleConstants.setFontFamily(resultStyle, "Consolas");
        StyleConstants.setFontSize(resultStyle, 13);
        
        errorStyle = terminalArea.addStyle("Error", null);
        StyleConstants.setForeground(errorStyle, ERROR_COLOR);
        StyleConstants.setFontFamily(errorStyle, "Consolas");
        StyleConstants.setFontSize(errorStyle, 13);
        
        sideEffectStyle = terminalArea.addStyle("SideEffect", null);
        StyleConstants.setForeground(sideEffectStyle, SIDE_EFFECT_COLOR);
        StyleConstants.setItalic(sideEffectStyle, true);
        StyleConstants.setFontFamily(sideEffectStyle, "Consolas");
        StyleConstants.setFontSize(sideEffectStyle, 13);
        
        commentStyle = terminalArea.addStyle("Comment", null);
        StyleConstants.setForeground(commentStyle, COMMENT_COLOR);
        StyleConstants.setItalic(commentStyle, true);
        StyleConstants.setFontFamily(commentStyle, "Consolas");
        StyleConstants.setFontSize(commentStyle, 13);
    }
    
    /**
     * Setup event handlers for terminal interaction.
     */
    private void setupEventHandlers() {
        terminalArea.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                handleKeyPress(e);
            }
            
            @Override
            public void keyTyped(KeyEvent e) {
                handleKeyTyped(e);
            }
        });
    }
    
    /**
     * Initialize terminal with welcome message and first prompt.
     */
    private void initializeTerminal() {
        appendText("% JProlog Interactive Console\n", commentStyle);
        appendText("% Version 1.0 - JProlog Editor from DenzoSOFT\n", commentStyle);
        appendText("% Type 'help.' for help, 'halt.' to exit session.\n", commentStyle);
        appendText("% Knowledge base initialized from project compilation.\n\n", commentStyle);
        
        // Show initial KB status if engine is available
        showKnowledgeBaseStatus();
        
        showPrompt();
        
        // Ensure the terminal has focus
        SwingUtilities.invokeLater(() -> {
            terminalArea.requestFocusInWindow();
            terminalArea.setCaretPosition(document.getLength());
        });
    }
    
    /**
     * Show knowledge base status.
     */
    private void showKnowledgeBaseStatus() {
        Prolog engine = ide.getPrologEngine();
        if (engine != null) {
            String kbStatus = engine.getListingOutput();
            if (kbStatus != null && !kbStatus.trim().isEmpty() && 
                !kbStatus.contains("Knowledge base is empty")) {
                appendText("% Current knowledge base:\n", commentStyle);
                appendText(kbStatus, sideEffectStyle);
                appendText("\n", normalStyle);
            }
        }
    }
    
    /**
     * Show the prompt.
     */
    private void showPrompt() {
        if (inMultilineQuery) {
            appendText("|    ", promptStyle);
        } else {
            appendText("?- ", promptStyle);
        }
        promptPosition = document.getLength();
        waitingForInput = true;
        
        // Ensure caret is at the right position and terminal has focus
        SwingUtilities.invokeLater(() -> {
            terminalArea.setCaretPosition(promptPosition);
            terminalArea.requestFocusInWindow();
        });
    }
    
    /**
     * Handle key press events.
     */
    private void handleKeyPress(KeyEvent e) {
        int caretPos = terminalArea.getCaretPosition();
        
        // Prevent editing before prompt
        if (caretPos < promptPosition && e.getKeyCode() != KeyEvent.VK_C && !e.isControlDown()) {
            e.consume();
            terminalArea.setCaretPosition(document.getLength());
            return;
        }
        
        switch (e.getKeyCode()) {
            case KeyEvent.VK_ENTER:
                if (!isProcessingQuery) {
                    e.consume();
                    processInput();
                }
                break;
                
            case KeyEvent.VK_UP:
                if (waitingForInput && !queryHistory.isEmpty()) {
                    e.consume();
                    navigateHistory(-1);
                }
                break;
                
            case KeyEvent.VK_DOWN:
                if (waitingForInput && !queryHistory.isEmpty()) {
                    e.consume();
                    navigateHistory(1);
                }
                break;
                
            case KeyEvent.VK_BACK_SPACE:
            case KeyEvent.VK_DELETE:
                if (caretPos <= promptPosition) {
                    e.consume();
                }
                break;
                
            case KeyEvent.VK_LEFT:
                if (caretPos <= promptPosition) {
                    e.consume();
                }
                break;
                
            case KeyEvent.VK_HOME:
                e.consume();
                terminalArea.setCaretPosition(promptPosition);
                break;
                
            case KeyEvent.VK_C:
                if (e.isControlDown()) {
                    interruptQuery();
                }
                break;
        }
    }
    
    /**
     * Handle key typed events.
     */
    private void handleKeyTyped(KeyEvent e) {
        if (!waitingForInput || terminalArea.getCaretPosition() < promptPosition) {
            e.consume();
            if (waitingForInput) {
                terminalArea.setCaretPosition(document.getLength());
            }
        }
    }
    
    /**
     * Process the input when Enter is pressed.
     */
    private void processInput() {
        try {
            String input = document.getText(promptPosition, document.getLength() - promptPosition);
            appendText("\n", normalStyle);
            
            // Check for multi-line continuation
            if (!input.trim().isEmpty() && !input.trim().endsWith(".")) {
                inMultilineQuery = true;
                multilineBuffer.append(input).append(" ");
                showPrompt();
                return;
            }
            
            // Complete multi-line query
            if (inMultilineQuery) {
                multilineBuffer.append(input);
                input = multilineBuffer.toString();
                multilineBuffer.setLength(0);
                inMultilineQuery = false;
            }
            
            // Process the complete query
            if (!input.trim().isEmpty()) {
                String query = input.trim();
                if (query.endsWith(".")) {
                    query = query.substring(0, query.length() - 1).trim();
                }
                
                if (!query.isEmpty()) {
                    queryHistory.add(query);
                    historyIndex = queryHistory.size();
                    
                    waitingForInput = false;
                    executeQuery(query);
                } else {
                    showPrompt();
                }
            } else {
                showPrompt();
            }
            
        } catch (BadLocationException ex) {
            appendText("Error reading input\n", errorStyle);
            showPrompt();
        }
    }
    
    /**
     * Navigate through query history.
     */
    private void navigateHistory(int direction) {
        if (queryHistory.isEmpty()) return;
        
        historyIndex += direction;
        
        if (historyIndex < 0) {
            historyIndex = 0;
        } else if (historyIndex >= queryHistory.size()) {
            historyIndex = queryHistory.size() - 1;
        }
        
        if (historyIndex >= 0 && historyIndex < queryHistory.size()) {
            try {
                // Clear current input
                document.remove(promptPosition, document.getLength() - promptPosition);
                // Insert history item
                document.insertString(promptPosition, queryHistory.get(historyIndex), queryStyle);
                terminalArea.setCaretPosition(document.getLength());
            } catch (BadLocationException ex) {
                // Ignore
            }
        }
    }
    
    /**
     * Execute a Prolog query.
     */
    private void executeQuery(String query) {
        isProcessingQuery = true;
        
        // Handle special commands
        if (handleSpecialCommands(query)) {
            isProcessingQuery = false;
            showPrompt();
            return;
        }
        
        // Execute in separate thread to keep UI responsive
        queryThread = new Thread(() -> {
            executeNormalQuery(query);
            SwingUtilities.invokeLater(() -> {
                isProcessingQuery = false;
                showPrompt();
            });
        });
        queryThread.start();
    }
    
    /**
     * Handle special commands.
     */
    private boolean handleSpecialCommands(String query) {
        // Handle help
        if (query.equals("help") || query.equals("help()")) {
            showHelp();
            return true;
        }
        
        // Handle halt
        if (query.equals("halt") || query.equals("halt()")) {
            appendText("% Session terminated.\n", commentStyle);
            waitingForInput = false;
            return true;
        }
        
        // Handle listing
        if (query.equals("listing") || query.equals("listing()")) {
            executeListing();
            return true;
        }
        
        if (query.startsWith("listing(") && query.endsWith(")")) {
            String arg = query.substring(8, query.length() - 1).trim();
            if (arg.startsWith("'") && arg.endsWith("'")) {
                arg = arg.substring(1, arg.length() - 1);
            }
            executeListing(arg);
            return true;
        }
        
        // Handle consult
        if (query.startsWith("consult(") && query.endsWith(")")) {
            String filename = query.substring(8, query.length() - 1).trim();
            if (filename.startsWith("'") && filename.endsWith("'")) {
                filename = filename.substring(1, filename.length() - 1);
            }
            executeConsult(filename);
            return true;
        }
        
        // Handle clear
        if (query.equals("clear") || query.equals("cls")) {
            clearTerminal();
            return true;
        }
        
        return false;
    }
    
    /**
     * Show help information.
     */
    private void showHelp() {
        appendText("\n% Available commands:\n", commentStyle);
        appendText("%   help.              - Show this help\n", commentStyle);
        appendText("%   listing.           - List all predicates\n", commentStyle);
        appendText("%   listing('p/n').    - List specific predicate\n", commentStyle);
        appendText("%   consult('file').   - Load Prolog file\n", commentStyle);
        appendText("%   clear.             - Clear screen\n", commentStyle);
        appendText("%   halt.              - Exit session\n", commentStyle);
        appendText("%   Ctrl+C             - Interrupt query\n", commentStyle);
        appendText("\n", normalStyle);
    }
    
    /**
     * Execute listing command.
     */
    private void executeListing() {
        Prolog engine = ide.getPrologEngine();
        if (engine == null) {
            appendText("ERROR: No Prolog engine available.\n", errorStyle);
            return;
        }
        
        String output = engine.getListingOutput();
        if (output != null && !output.trim().isEmpty()) {
            appendText(output, sideEffectStyle);
        } else {
            appendText("% Knowledge base is empty.\n", commentStyle);
        }
        appendText("true.\n", resultStyle);
    }
    
    /**
     * Execute listing for specific predicate.
     */
    private void executeListing(String predicate) {
        Prolog engine = ide.getPrologEngine();
        if (engine == null) {
            appendText("ERROR: No Prolog engine available.\n", errorStyle);
            return;
        }
        
        String output = engine.getListingOutput(predicate);
        if (output != null && !output.trim().isEmpty()) {
            appendText(output, sideEffectStyle);
        }
        appendText("true.\n", resultStyle);
    }
    
    /**
     * Execute consult command.
     */
    private void executeConsult(String filename) {
        Prolog engine = ide.getPrologEngine();
        if (engine == null) {
            appendText("ERROR: No Prolog engine available.\n", errorStyle);
            return;
        }
        
        try {
            java.io.File file = new java.io.File(filename);
            if (!file.exists() && ide.getCurrentProjectRoot() != null) {
                file = new java.io.File(ide.getCurrentProjectRoot(), filename);
            }
            
            if (!file.exists()) {
                appendText("ERROR: File not found: " + filename + "\n", errorStyle);
                appendText("false.\n", errorStyle);
                return;
            }
            
            String content = new String(java.nio.file.Files.readAllBytes(file.toPath()));
            engine.consult(content);
            
            appendText("% " + filename + " consulted.\n", commentStyle);
            appendText("true.\n", resultStyle);
            
        } catch (Exception e) {
            appendText("ERROR: " + e.getMessage() + "\n", errorStyle);
            appendText("false.\n", errorStyle);
        }
    }
    
    /**
     * Execute normal Prolog query.
     */
    private void executeNormalQuery(String query) {
        Prolog engine = ide.getPrologEngine();
        
        if (engine == null) {
            SwingUtilities.invokeLater(() -> {
                appendText("ERROR: No Prolog engine available.\n", errorStyle);
                appendText("false.\n", errorStyle);
            });
            return;
        }
        
        try {
            // Capture System.out
            PrintStream originalOut = System.out;
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            PrintStream captureOut = new PrintStream(baos);
            
            List<Map<String, Term>> solutions;
            String capturedOutput = "";
            
            try {
                System.setOut(captureOut);
                solutions = engine.solve(query);
                capturedOutput = baos.toString();
            } finally {
                System.setOut(originalOut);
            }
            
            final String output = capturedOutput;
            final List<Map<String, Term>> finalSolutions = solutions;
            
            SwingUtilities.invokeLater(() -> {
                // Display captured output (from write/1, nl/0, etc.)
                if (!output.isEmpty()) {
                    appendText(output, sideEffectStyle);
                }
                
                // Display solutions
                if (finalSolutions.isEmpty()) {
                    appendText("false.\n", errorStyle);
                } else if (finalSolutions.size() == 1 && finalSolutions.get(0).isEmpty()) {
                    appendText("true.\n", resultStyle);
                } else {
                    displaySolutions(finalSolutions);
                }
            });
            
        } catch (Exception e) {
            SwingUtilities.invokeLater(() -> {
                appendText("ERROR: " + e.getMessage() + "\n", errorStyle);
                appendText("false.\n", errorStyle);
            });
        }
    }
    
    /**
     * Display query solutions interactively.
     */
    private void displaySolutions(List<Map<String, Term>> solutions) {
        for (int i = 0; i < solutions.size(); i++) {
            Map<String, Term> solution = solutions.get(i);
            
            if (solution.isEmpty()) {
                appendText("true", resultStyle);
            } else {
                boolean first = true;
                for (Map.Entry<String, Term> entry : solution.entrySet()) {
                    if (!first) {
                        appendText(",\n", resultStyle);
                    }
                    appendText(entry.getKey() + " = " + entry.getValue(), resultStyle);
                    first = false;
                }
            }
            
            if (i < solutions.size() - 1) {
                appendText(" ;\n", resultStyle);
            } else {
                appendText(".\n", resultStyle);
            }
        }
    }
    
    /**
     * Clear the terminal.
     */
    private void clearTerminal() {
        terminalArea.setText("");
        initializeTerminal();
    }
    
    /**
     * Restart Prolog engine.
     */
    private void restartProlog() {
        appendText("\n% Restarting Prolog engine...\n", commentStyle);
        
        // Trigger recompilation
        ide.compileProject();
        
        clearTerminal();
    }
    
    /**
     * Interrupt current query.
     */
    private void interruptQuery() {
        if (isProcessingQuery && queryThread != null) {
            queryThread.interrupt();
            isProcessingQuery = false;
            appendText("\n% Query interrupted.\n", errorStyle);
            showPrompt();
        }
    }
    
    /**
     * Append text with specific style.
     */
    private void appendText(String text, Style style) {
        try {
            document.insertString(document.getLength(), text, style);
            terminalArea.setCaretPosition(document.getLength());
        } catch (BadLocationException e) {
            // Ignore
        }
    }
    
    /**
     * Called when knowledge base is reloaded.
     */
    public void onKnowledgeBaseReloaded() {
        if (!waitingForInput) {
            appendText("\n", normalStyle);
        }
        appendText("% Knowledge base reloaded from project compilation.\n", commentStyle);
        showKnowledgeBaseStatus();
        if (!waitingForInput) {
            showPrompt();
        }
        
        // Ensure terminal has focus and prompt is ready
        SwingUtilities.invokeLater(() -> {
            terminalArea.requestFocusInWindow();
            if (waitingForInput) {
                terminalArea.setCaretPosition(document.getLength());
            }
        });
    }
    
    /**
     * Reset the console after compilation.
     */
    public void reset() {
        clearTerminal();
    }
    
    /**
     * Focus the terminal.
     */
    public void focusPrompt() {
        SwingUtilities.invokeLater(() -> {
            terminalArea.requestFocusInWindow();
            if (waitingForInput) {
                terminalArea.setCaretPosition(document.getLength());
            }
        });
    }
    
    /**
     * Get the output text (for BottomTabbedPane compatibility).
     */
    public String getOutputText() {
        try {
            return document.getText(0, document.getLength());
        } catch (BadLocationException e) {
            return "";
        }
    }
}