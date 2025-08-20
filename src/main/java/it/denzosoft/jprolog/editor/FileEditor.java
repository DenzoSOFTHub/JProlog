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
    
    // Colori per syntax highlighting
    private static final Color COMMENT_COLOR = new Color(0, 128, 0);
    private static final Color STRING_COLOR = new Color(0, 0, 255);
    private static final Color KEYWORD_COLOR = new Color(128, 0, 128);
    private static final Color OPERATOR_COLOR = new Color(128, 128, 0);
    private static final Color ERROR_COLOR = new Color(255, 0, 0);
    private static final Color LINE_NUMBER_COLOR = new Color(128, 128, 128);
    private static final Color CURRENT_LINE_COLOR = new Color(255, 255, 220);
    
    // Pattern per syntax highlighting
    private static final Pattern COMMENT_PATTERN = Pattern.compile("%.*");
    private static final Pattern STRING_PATTERN = Pattern.compile("\"[^\"]*\"|'[^']*'");
    private static final Pattern ATOM_PATTERN = Pattern.compile("\\b[a-z][a-zA-Z0-9_]*\\b");
    private static final Pattern VARIABLE_PATTERN = Pattern.compile("\\b[A-Z_][a-zA-Z0-9_]*\\b");
    private static final Pattern OPERATOR_PATTERN = Pattern.compile(":-|-->|\\\\\\+|=\\.\\.|\\\\\\/|/\\\\|@<|@=<|@>|@>=|=:=|=\\\\=|==|\\\\==|=<|>=|\\\\=|is|\\+|-|\\*|/|\\^|mod|rem|abs|sin|cos|tan|exp|log|sqrt");
    
    // Parole chiave Prolog
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
        
        // Area di testo principale con syntax highlighting
        textPane = new JTextPane();
        textPane.setFont(new Font("Consolas", Font.PLAIN, 14));
        
        // JTextPane configuration
        textPane.setBackground(Color.WHITE);
        textPane.setCaretColor(Color.BLACK);
        textPane.setSelectionColor(new Color(173, 214, 255));
        textPane.setSelectedTextColor(Color.BLACK);
        
        // Area numerazione righe (adattata per JTextPane)
        lineNumberArea = new LineNumberArea(textPane);
        
        // ScrollPane
        scrollPane = new JScrollPane(textPane);
        scrollPane.setRowHeaderView(lineNumberArea);
        scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
        scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        
        // Aggiungi listener per sincronizzare i numeri di riga durante lo scroll
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
     * Configura il syntax highlighting.
     */
    private void setupSyntaxHighlighting() {
        // Aggiungi listener per aggiornamenti del syntax highlighting
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
                // Ignorato per document listener
            }
        });
        
        // Aggiungi listener per highlight della riga corrente
        textPane.addCaretListener(e -> highlightCurrentLine());
    }
    
    /**
     * Evidenzia la riga corrente.
     */
    private void highlightCurrentLine() {
        // Disabilitato per evitare problemi di performance
        // Current line highlighting can be implemented
        // con un approccio diverso usando un Highlighter personalizzato
    }
    
    /**
     * Carica il contenuto del file.
     */
    private void loadFile() throws IOException {
        try {
            byte[] bytes = Files.readAllBytes(file.toPath());
            String content = new String(bytes, "UTF-8");
            textPane.setText(content);
            textPane.setCaretPosition(0);
            
            // Applica syntax highlighting dopo il caricamento
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
     * Configura gli event handlers.
     */
    private void setupEventHandlers() {
        // Listener per modifiche al documento (aggiunto a quello del syntax highlighting)
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
        
        // Shortcuts da tastiera
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
                
                // Auto-indentazione
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
     * Auto-indentazione quando si preme Enter.
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
            
            // Conta gli spazi/tab all'inizio della riga
            int indent = 0;
            for (char c : line.toCharArray()) {
                if (c == ' ') indent++;
                else if (c == '\t') indent += 4;
                else break;
            }
            
            // Aggiungi indentazione extra per certe costruzioni
            if (line.trim().endsWith(":-") || line.trim().endsWith("(")) {
                indent += 4;
            }
            
            // Inserisci l'indentazione
            final int finalIndent = indent;
            SwingUtilities.invokeLater(() -> {
                StringBuilder indentStr = new StringBuilder("\n");
                for (int i = 0; i < finalIndent; i++) {
                    indentStr.append(" ");
                }
                try {
                    doc.insertString(textPane.getCaretPosition(), indentStr.toString(), null);
                } catch (BadLocationException ex) {
                    // Ignora
                }
            });
            
        } catch (BadLocationException e) {
            // Ignora
        }
    }
    
    /**
     * Segna il file come modificato.
     */
    private void markAsModified() {
        if (!isModified) {
            isModified = true;
            updateTitle();
            notifyModifiedListeners();
        }
    }
    
    /**
     * Aggiorna il titolo del tab.
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
            
            // Aggiorna i predicati dopo il salvataggio
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
                
                // Evidenzia la riga con errore
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
        
        // Se non trovato dalla posizione corrente, cerca dall'inizio
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
     * Sostituisce tutto il testo nell'editor.
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
    
    /**
     * Componente per la numerazione delle righe.
     */
    private static class LineNumberArea extends JComponent {
        private static final int MARGIN = 5;
        private JTextPane textPane;
        
        public LineNumberArea(JTextPane textPane) {
            this.textPane = textPane;
            setPreferredSize(new Dimension(50, 0));
            setBackground(new Color(240, 240, 240));
            setBorder(new EmptyBorder(0, MARGIN, 0, MARGIN));
            setFont(new Font("Consolas", Font.PLAIN, 12));
            
            // Aggiungi mouse wheel listener per propagare eventi di scroll al textPane
            addMouseWheelListener(e -> {
                // Propaga l'evento di scroll wheel al JScrollPane parent
                Component parent = getParent();
                while (parent != null && !(parent instanceof JScrollPane)) {
                    parent = parent.getParent();
                }
                if (parent instanceof JScrollPane) {
                    JScrollPane scrollPane = (JScrollPane) parent;
                    scrollPane.dispatchEvent(e);
                }
            });
        }
        
        @Override
        protected void paintComponent(Graphics g) {
            super.paintComponent(g);
            
            Graphics2D g2 = (Graphics2D) g.create();
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            
            // Sfondo
            g2.setColor(getBackground());
            g2.fillRect(0, 0, getWidth(), getHeight());
            
            // Numeri di riga
            g2.setColor(LINE_NUMBER_COLOR);
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
                        String lineNum = String.valueOf(line + 1);
                        int x = getWidth() - fm.stringWidth(lineNum) - MARGIN;
                        int y = rect.y + fontHeight - fm.getDescent();
                        g2.drawString(lineNum, x, y);
                    }
                }
            } catch (BadLocationException e) {
                // Ignora
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