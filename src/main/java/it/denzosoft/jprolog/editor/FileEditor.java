package it.denzosoft.jprolog.editor;

import it.denzosoft.jprolog.editor.util.DialogUtils;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.*;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.*;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Prolog file editor with syntax highlighting, line numbering
 * and advanced editing features.
 */
public class FileEditor extends JPanel {
    
    private File file;
    private PrologIDE ide;
    private JTextPane textPane;
    private LineNumberArea lineNumberArea;
    private JScrollPane scrollPane;
    private boolean isModified;
    private List<Runnable> modifiedListeners;
    private PrologSyntaxHighlighter syntaxHighlighter;
    
    // Breakpoints and debug state
    private java.util.Set<Integer> breakpointLines = new java.util.HashSet<>();
    private int debugHighlightLine = -1; // Currently executing line during debug (-1 = none)

    // Colors for syntax highlighting
    private static final Color COMMENT_COLOR = new Color(0, 128, 0);
    private static final Color STRING_COLOR = new Color(0, 0, 255);
    private static final Color KEYWORD_COLOR = new Color(128, 0, 128);
    private static final Color OPERATOR_COLOR = new Color(128, 128, 0);
    private static final Color ERROR_COLOR = new Color(255, 0, 0);
    private static final Color LINE_NUMBER_COLOR = new Color(128, 128, 128);
    private static final Color CURRENT_LINE_COLOR = new Color(255, 255, 220);
    private static final Color BREAKPOINT_COLOR = new Color(200, 50, 50);
    private static final Color BREAKPOINT_GUTTER_COLOR = new Color(255, 200, 200);
    private static final Color DEBUG_LINE_COLOR = new Color(198, 219, 174); // Green highlight for current debug line
    private static final Color ERROR_LINE_COLOR = new Color(255, 220, 220); // Light red for error lines
    
    // Patterns for syntax highlighting
    private static final Pattern COMMENT_PATTERN = Pattern.compile("%.*");
    private static final Pattern STRING_PATTERN = Pattern.compile("\"[^\"]*\"|'[^']*'");
    private static final Pattern ATOM_PATTERN = Pattern.compile("\\b[a-z][a-zA-Z0-9_]*\\b");
    private static final Pattern VARIABLE_PATTERN = Pattern.compile("\\b[A-Z_][a-zA-Z0-9_]*\\b");
    private static final Pattern OPERATOR_PATTERN = Pattern.compile(":-|-->|\\\\\\+|=\\.\\.|\\\\\\/|/\\\\|@<|@=<|@>|@>=|=:=|=\\\\=|==|\\\\==|=<|>=|\\\\=|is|\\+|-|\\*|/|\\^|mod|rem|abs|sin|cos|tan|exp|log|sqrt");
    
    // Prolog keywords
    private static final String[] PROLOG_KEYWORDS = {
        "true", "false", "fail", "cut", "call", "findall", "bagof", "setof",
        "assert", "asserta", "assertz", "retract", "retractall", "abolish",
        "consult", "reconsult", "listing", "trace", "notrace", "spy", "nospy",
        "halt", "abort", "break", "statistics", "current_predicate", "functor",
        "arg", "univ", "var", "nonvar", "atom", "number", "integer", "float",
        "atomic", "compound", "is_list", "length", "member", "append", "reverse",
        "sort", "keysort", "write", "writeln", "writeq", "write_canonical",
        "read", "get", "put", "get_char", "put_char", "open", "close", "see",
        "tell", "seen", "told", "nl", "tab", "format", "catch", "throw"
    };
    
    public FileEditor(File file, PrologIDE ide) throws IOException {
        this.file = file;
        this.ide = ide;
        this.isModified = false;
        this.modifiedListeners = new ArrayList<>();
        
        initializeComponents();
        loadFile();
        setupEventHandlers();
    }
    
    /**
     * Initializes editor components.
     */
    private void initializeComponents() {
        setLayout(new BorderLayout());
        
        // Main text area with syntax highlighting
        textPane = new JTextPane();
        textPane.setFont(new Font("Consolas", Font.PLAIN, 14));
        
        // JTextPane configuration
        textPane.setBackground(Color.WHITE);
        textPane.setCaretColor(Color.BLACK);
        textPane.setSelectionColor(new Color(173, 214, 255));
        textPane.setSelectedTextColor(Color.BLACK);
        
        // Line numbering area (adapted for JTextPane)
        lineNumberArea = new LineNumberArea(textPane);
        
        // ScrollPane
        scrollPane = new JScrollPane(textPane);
        scrollPane.setRowHeaderView(lineNumberArea);
        scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        
        // Add listener to synchronize line numbers during scroll
        scrollPane.getViewport().addChangeListener(e -> {
            if (lineNumberArea != null) {
                lineNumberArea.repaint();
            }
        });
        
        add(scrollPane, BorderLayout.CENTER);
        
        // Initialize syntax highlighter
        syntaxHighlighter = new PrologSyntaxHighlighter(textPane);
        
        // Setup syntax highlighting
        setupSyntaxHighlighting();
    }
    
    /**
     * Configure syntax highlighting.
     */
    private void setupSyntaxHighlighting() {
        // Add listener for syntax highlighting updates
        textPane.getDocument().addDocumentListener(new DocumentListener() {
            @Override
            public void insertUpdate(DocumentEvent e) {
                SwingUtilities.invokeLater(() -> {
                    if (syntaxHighlighter != null) {
                        syntaxHighlighter.highlightSyntax();
                    }
                });
            }
            
            @Override
            public void removeUpdate(DocumentEvent e) {
                SwingUtilities.invokeLater(() -> {
                    if (syntaxHighlighter != null) {
                        syntaxHighlighter.highlightSyntax();
                    }
                });
            }
            
            @Override
            public void changedUpdate(DocumentEvent e) {
                // Ignored for document listener
            }
        });
        
        // Add listener for current line highlighting
        textPane.addCaretListener(e -> highlightCurrentLine());
    }
    
    /**
     * Highlight current line.
     */
    private void highlightCurrentLine() {
        // Disabled to avoid performance issues
        // Current line highlighting can be implemented
        // with a different approach using a custom Highlighter
    }
    
    /**
     * Load file content.
     */
    private void loadFile() throws IOException {
        try {
            byte[] bytes = Files.readAllBytes(file.toPath());
            String content = new String(bytes, "UTF-8");
            textPane.setText(content);
            textPane.setCaretPosition(0);
            
            // Apply syntax highlighting after loading
            if (syntaxHighlighter != null) {
                SwingUtilities.invokeLater(() -> syntaxHighlighter.highlightSyntax());
            }
            
            isModified = false;
            updateTitle();
        } catch (IOException e) {
            throw new IOException("Error loading file: " + e.getMessage());
        }
    }
    
    /**
     * Configure event handlers.
     */
    private void setupEventHandlers() {
        // Listener for document changes (added to the syntax highlighting one)
        textPane.getDocument().addDocumentListener(new DocumentListener() {
            @Override
            public void insertUpdate(DocumentEvent e) {
                markAsModified();
            }
            
            @Override
            public void removeUpdate(DocumentEvent e) {
                markAsModified();
            }
            
            @Override
            public void changedUpdate(DocumentEvent e) {
                markAsModified();
            }
        });
        
        // Keyboard shortcuts
        textPane.addKeyListener(new KeyListener() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.isControlDown()) {
                    switch (e.getKeyCode()) {
                        case KeyEvent.VK_S:
                            save();
                            e.consume();
                            break;
                        case KeyEvent.VK_F:
                            ide.getEditorTabs();
                            e.consume();
                            break;
                    }
                }
                
                // Auto-indentation
                if (e.getKeyCode() == KeyEvent.VK_ENTER) {
                    autoIndent();
                }
            }
            
            @Override
            public void keyTyped(KeyEvent e) {}
            
            @Override
            public void keyReleased(KeyEvent e) {}
        });
    }
    
    /**
     * Auto-indentation when Enter is pressed.
     */
    private void autoIndent() {
        try {
            int caretPos = textPane.getCaretPosition();
            Document doc = textPane.getDocument();
            Element root = doc.getDefaultRootElement();
            int lineNum = root.getElementIndex(caretPos);
            Element lineElement = root.getElement(lineNum);
            int lineStart = lineElement.getStartOffset();
            int lineEnd = lineElement.getEndOffset();
            
            String line = doc.getText(lineStart, lineEnd - lineStart);
            
            // Count spaces/tabs at the beginning of the line
            int indent = 0;
            for (char c : line.toCharArray()) {
                if (c == ' ') indent++;
                else if (c == '\t') indent += 4;
                else break;
            }
            
            // Add extra indentation for certain constructions
            if (line.trim().endsWith(":-") || line.trim().endsWith("(")) {
                indent += 4;
            }
            
            // Insert indentation
            final int finalIndent = indent;
            SwingUtilities.invokeLater(() -> {
                StringBuilder indentStr = new StringBuilder("\n");
                for (int i = 0; i < finalIndent; i++) {
                    indentStr.append(" ");
                }
                try {
                    doc.insertString(textPane.getCaretPosition(), indentStr.toString(), null);
                } catch (BadLocationException ex) {
                    // Ignore
                }
            });
            
        } catch (BadLocationException e) {
            // Ignora
        }
    }
    
    /**
     * Marks the file as modified.
     */
    private void markAsModified() {
        if (!isModified) {
            isModified = true;
            updateTitle();
            notifyModifiedListeners();
        }
    }
    
    /**
     * Updates the tab title.
     */
    private void updateTitle() {
        notifyModifiedListeners();
    }
    
    /**
     * Salva il file.
     */
    public boolean save() {
        try {
            String content = textPane.getText();
            try (FileWriter writer = new FileWriter(file, false)) {
                writer.write(content);
            }
            
            isModified = false;
            updateTitle();
            ide.getStatusBar().setMessage("File saved: " + file.getName());
            ide.getBottomTabbedPane().appendToOutput("Saved: " + file.getAbsolutePath() + "\n");
            
            // Update predicates after saving
            if (ide.getPredicatePanel() != null) {
                ide.getPredicatePanel().updatePredicatesForFile(file);
            }
            
            return true;
            
        } catch (IOException e) {
            DialogUtils.showError(ide, 
                "Error saving file: " + e.getMessage(),
                "Error");
            return false;
        }
    }
    
    /**
     * Evidenzia un errore su una riga specifica.
     */
    public void highlightError(int lineNumber, String errorMessage) {
        try {
            Document doc = textPane.getDocument();
            Element root = doc.getDefaultRootElement();
            if (lineNumber <= root.getElementCount()) {
                Element lineElement = root.getElement(lineNumber - 1);
                int lineStart = lineElement.getStartOffset();
                int lineEnd = lineElement.getEndOffset();
                
                // Highlight the error line
                textPane.select(lineStart, lineEnd - 1);
                textPane.setSelectionColor(ERROR_COLOR);
                
                // Mostra tooltip con messaggio errore
                textPane.setCaretPosition(lineStart);
                textPane.setToolTipText("Error line " + lineNumber + ": " + errorMessage);
            }
        } catch (Exception e) {
            // Ignora
        }
    }
    
    /**
     * Rimuove l'evidenziazione degli errori.
     */
    public void clearErrorHighlighting() {
        textPane.setSelectionColor(new Color(173, 214, 255));
        textPane.setToolTipText(null);
    }
    
    /**
     * Trova testo nell'editor.
     */
    public boolean findText(String searchText, boolean caseSensitive, boolean wholeWord) {
        if (searchText == null || searchText.isEmpty()) {
            return false;
        }
        
        String content = textPane.getText();
        String search = caseSensitive ? searchText : searchText.toLowerCase();
        String text = caseSensitive ? content : content.toLowerCase();
        
        int startPos = textPane.getCaretPosition();
        int foundPos = text.indexOf(search, startPos);
        
        // If not found from current position, search from beginning
        if (foundPos == -1) {
            foundPos = text.indexOf(search, 0);
        }
        
        if (foundPos != -1) {
            textPane.select(foundPos, foundPos + searchText.length());
            textPane.setCaretPosition(foundPos);
            return true;
        }
        
        return false;
    }
    
    /**
     * Sostituisce testo nell'editor.
     */
    public boolean replaceText(String searchText, String replaceText, boolean caseSensitive) {
        String selectedText = textPane.getSelectedText();
        if (selectedText != null) {
            boolean matches = caseSensitive ? 
                selectedText.equals(searchText) : 
                selectedText.equalsIgnoreCase(searchText);
            
            if (matches) {
                textPane.replaceSelection(replaceText);
                return true;
            }
        }
        
        return findText(searchText, caseSensitive, false);
    }
    
    /**
     * Replaces all text in the editor.
     */
    public int replaceAllText(String searchText, String replaceText, boolean caseSensitive) {
        String content = textPane.getText();
        String result;
        int count = 0;
        
        if (caseSensitive) {
            result = content.replaceAll(Pattern.quote(searchText), replaceText);
            count = content.length() - result.length();
            count = count / (searchText.length() - replaceText.length());
        } else {
            result = content.replaceAll("(?i)" + Pattern.quote(searchText), replaceText);
            count = (content.length() - result.length()) / (searchText.length() - replaceText.length());
        }
        
        if (!content.equals(result)) {
            textPane.setText(result);
            if (syntaxHighlighter != null) {
                SwingUtilities.invokeLater(() -> syntaxHighlighter.highlightSyntax());
            }
        }
        
        return count;
    }
    
    /**
     * Va a una riga specifica.
     */
    public void goToLine(int lineNumber) {
        try {
            Document doc = textPane.getDocument();
            Element root = doc.getDefaultRootElement();
            if (lineNumber > 0 && lineNumber <= root.getElementCount()) {
                Element lineElement = root.getElement(lineNumber - 1);
                int lineStart = lineElement.getStartOffset();
                textPane.setCaretPosition(lineStart);
                textPane.requestFocus();
            }
        } catch (Exception e) {
            // Riga non valida
        }
    }
    
    // ===================== GETTERS E SETTERS =====================
    
    public File getFile() {
        return file;
    }
    
    public void updateFile(File newFile) {
        this.file = newFile;
    }
    
    public boolean isModified() {
        return isModified;
    }
    
    public JScrollPane getScrollPane() {
        return scrollPane;
    }
    
    public JTextPane getTextPane() {
        return textPane;
    }
    
    public JTextArea getTextArea() {
        // Compatibility method - returns null since we now use JTextPane
        return null;
    }
    
    public String getText() {
        return textPane.getText();
    }
    
    public void setText(String text) {
        textPane.setText(text);
        if (syntaxHighlighter != null) {
            SwingUtilities.invokeLater(() -> syntaxHighlighter.highlightSyntax());
        }
        isModified = false;
        updateTitle();
    }
    
    public void addModifiedListener(Runnable listener) {
        modifiedListeners.add(listener);
    }
    
    private void notifyModifiedListeners() {
        for (Runnable listener : modifiedListeners) {
            listener.run();
        }
    }
    
    @Override
    public void requestFocus() {
        textPane.requestFocus();
    }
    
    @Override
    public boolean requestFocusInWindow() {
        return textPane.requestFocusInWindow();
    }
    
    // ===================== BREAKPOINT SUPPORT =====================

    /**
     * Toggle a breakpoint on the given line.
     * @return true if breakpoint was added, false if removed
     */
    public boolean toggleBreakpoint(int lineNumber) {
        if (breakpointLines.contains(lineNumber)) {
            breakpointLines.remove(lineNumber);
            lineNumberArea.repaint();
            return false;
        } else {
            breakpointLines.add(lineNumber);
            lineNumberArea.repaint();
            return true;
        }
    }

    public java.util.Set<Integer> getBreakpointLines() {
        return java.util.Collections.unmodifiableSet(breakpointLines);
    }

    public void clearBreakpoints() {
        breakpointLines.clear();
        lineNumberArea.repaint();
    }

    // ===================== DEBUG LINE HIGHLIGHTING =====================

    /**
     * Highlight a specific line during debug (shows green background).
     * @param lineNumber 1-based line number, or -1 to clear
     */
    public void setDebugHighlightLine(int lineNumber) {
        this.debugHighlightLine = lineNumber;
        if (lineNumber > 0) {
            goToLine(lineNumber);
        }
        lineNumberArea.repaint();
        textPane.repaint();
    }

    public void clearDebugHighlighting() {
        this.debugHighlightLine = -1;
        lineNumberArea.repaint();
        textPane.repaint();
    }

    /**
     * Highlight an error line with red underline/background.
     * Enhanced version that uses a Highlighter.
     */
    public void highlightErrorLine(int lineNumber, String errorMessage) {
        try {
            Document doc = textPane.getDocument();
            Element root = doc.getDefaultRootElement();
            if (lineNumber > 0 && lineNumber <= root.getElementCount()) {
                Element lineElement = root.getElement(lineNumber - 1);
                int lineStart = lineElement.getStartOffset();
                int lineEnd = lineElement.getEndOffset();

                // Use Highlighter for persistent error marking
                textPane.getHighlighter().addHighlight(
                    lineStart, Math.min(lineEnd, doc.getLength()),
                    new javax.swing.text.DefaultHighlighter.DefaultHighlightPainter(ERROR_LINE_COLOR));

                textPane.setCaretPosition(lineStart);
                textPane.setToolTipText("Line " + lineNumber + ": " + errorMessage);
            }
        } catch (Exception e) {
            // ignore
        }
    }

    /**
     * Clear all error highlights.
     */
    public void clearAllHighlights() {
        textPane.getHighlighter().removeAllHighlights();
        textPane.setToolTipText(null);
    }

    /**
     * Componente per la numerazione delle righe.
     */
    private class LineNumberArea extends JComponent {
        private static final int MARGIN = 5;
        private JTextPane textPane;
        
        public LineNumberArea(JTextPane textPane) {
            this.textPane = textPane;
            setPreferredSize(new Dimension(55, 0));
            setBackground(new Color(240, 240, 240));
            setBorder(new EmptyBorder(0, MARGIN, 0, MARGIN));
            setFont(new Font("Consolas", Font.PLAIN, 12));

            // Click in gutter toggles breakpoint
            addMouseListener(new java.awt.event.MouseAdapter() {
                @Override
                public void mouseClicked(java.awt.event.MouseEvent e) {
                    int lineNum = getLineAtPoint(e.getY());
                    if (lineNum > 0) {
                        boolean added = toggleBreakpoint(lineNum);
                        // Notify IDE debug panel if available
                        if (ide != null && ide.getDebugPanel() != null) {
                            // Extract predicate on this line for the breakpoint
                            String predicateName = extractPredicateAtLine(lineNum);
                            if (predicateName != null) {
                                if (added) {
                                    ide.getDebugPanel().addBreakpointProgrammatic(predicateName);
                                } else {
                                    ide.getDebugPanel().removeBreakpointProgrammatic(predicateName);
                                }
                            }
                        }
                    }
                }
            });

            // Propagate scroll events
            addMouseWheelListener(ev -> {
                Component par = getParent();
                while (par != null && !(par instanceof JScrollPane)) {
                    par = par.getParent();
                }
                if (par instanceof JScrollPane) {
                    ((JScrollPane) par).dispatchEvent(ev);
                }
            });
        }

        /**
         * Get the 1-based line number at a Y coordinate.
         */
        private int getLineAtPoint(int y) {
            try {
                Document doc = textPane.getDocument();
                Element root = doc.getDefaultRootElement();
                // Find the line element at this y position
                int pos = textPane.viewToModel(new Point(0, y));
                return root.getElementIndex(pos) + 1;
            } catch (Exception e) {
                return -1;
            }
        }

        /**
         * Extract predicate name/arity from a line (for breakpoint registration).
         * Looks for patterns like "name(" or "name :-"
         */
        private String extractPredicateAtLine(int lineNum) {
            try {
                Document doc = textPane.getDocument();
                Element root = doc.getDefaultRootElement();
                if (lineNum > 0 && lineNum <= root.getElementCount()) {
                    Element lineElem = root.getElement(lineNum - 1);
                    int start = lineElem.getStartOffset();
                    int end = lineElem.getEndOffset();
                    String line = doc.getText(start, end - start).trim();

                    // Skip comments and empty lines
                    if (line.isEmpty() || line.startsWith("%")) return null;

                    // Match predicate head: name(arg1, arg2, ...)
                    java.util.regex.Matcher m = java.util.regex.Pattern
                        .compile("^([a-z_][a-zA-Z0-9_]*)\\(([^)]*)\\)").matcher(line);
                    if (m.find()) {
                        String name = m.group(1);
                        String args = m.group(2).trim();
                        int arity = args.isEmpty() ? 0 :
                            args.split(",").length;
                        return name + "/" + arity;
                    }

                    // Match fact without args: name.  or  name :-
                    m = java.util.regex.Pattern
                        .compile("^([a-z_][a-zA-Z0-9_]*)\\s*[.:]").matcher(line);
                    if (m.find()) {
                        return m.group(1) + "/0";
                    }
                }
            } catch (Exception e) {
                // ignore
            }
            return null;
        }
        
        @Override
        protected void paintComponent(Graphics g) {
            super.paintComponent(g);

            Graphics2D g2 = (Graphics2D) g.create();
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

            // Background
            g2.setColor(getBackground());
            g2.fillRect(0, 0, getWidth(), getHeight());

            g2.setFont(getFont());
            FontMetrics fm = g2.getFontMetrics();
            int fontHeight = fm.getHeight();

            try {
                Document doc = textPane.getDocument();
                Element root = doc.getDefaultRootElement();
                int lineCount = root.getElementCount();

                Point viewStart = new Point(0, 0);
                Point viewEnd = new Point(0, getHeight());
                int start = textPane.viewToModel(viewStart);
                int end = textPane.viewToModel(viewEnd);

                int startLine = root.getElementIndex(start);
                int endLine = root.getElementIndex(end);

                for (int line = startLine; line <= Math.min(endLine, lineCount - 1); line++) {
                    Element lineElement = root.getElement(line);
                    int lineStart = lineElement.getStartOffset();
                    Rectangle rect = textPane.modelToView(lineStart);

                    if (rect != null) {
                        int lineNum1Based = line + 1;
                        int lineY = rect.y;
                        int lineHeight = fontHeight;

                        // Draw debug highlight line (green background)
                        if (debugHighlightLine == lineNum1Based) {
                            g2.setColor(DEBUG_LINE_COLOR);
                            g2.fillRect(0, lineY, getWidth(), lineHeight);
                        }

                        // Draw breakpoint background
                        if (breakpointLines.contains(lineNum1Based)) {
                            g2.setColor(BREAKPOINT_GUTTER_COLOR);
                            g2.fillRect(0, lineY, getWidth(), lineHeight);

                            // Draw red circle for breakpoint
                            int circleSize = 10;
                            int circleX = 3;
                            int circleY = lineY + (lineHeight - circleSize) / 2;
                            g2.setColor(BREAKPOINT_COLOR);
                            g2.fillOval(circleX, circleY, circleSize, circleSize);
                            g2.setColor(BREAKPOINT_COLOR.darker());
                            g2.drawOval(circleX, circleY, circleSize, circleSize);
                        }

                        // Draw debug arrow (current execution point)
                        if (debugHighlightLine == lineNum1Based) {
                            int arrowX = 2;
                            int arrowY = lineY + lineHeight / 2;
                            g2.setColor(new Color(0, 160, 0));
                            int[] xPoints = {arrowX, arrowX + 8, arrowX};
                            int[] yPoints = {arrowY - 4, arrowY, arrowY + 4};
                            g2.fillPolygon(xPoints, yPoints, 3);
                        }

                        // Draw line number
                        g2.setColor(LINE_NUMBER_COLOR);
                        String lineNum = String.valueOf(lineNum1Based);
                        int x = getWidth() - fm.stringWidth(lineNum) - MARGIN;
                        int y = rect.y + fontHeight - fm.getDescent();
                        g2.drawString(lineNum, x, y);
                    }
                }
            } catch (BadLocationException e) {
                // ignore
            }

            g2.dispose();
        }
        
        @Override
        public Dimension getPreferredSize() {
            FontMetrics fm = getFontMetrics(getFont());
            Document doc = textPane.getDocument();
            Element root = doc.getDefaultRootElement();
            int lineCount = root.getElementCount();
            int width = fm.stringWidth(String.valueOf(lineCount)) + 2 * MARGIN;
            return new Dimension(Math.max(width, 50), 0);
        }
    }
}