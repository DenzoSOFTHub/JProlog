package it.denzosoft.jprolog.editor;

import it.denzosoft.jprolog.editor.util.DialogUtils;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.UndoableEditEvent;
import javax.swing.event.UndoableEditListener;
import javax.swing.text.*;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.UndoManager;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.*;
import java.nio.charset.StandardCharsets;
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
    private final UndoManager undoManager = new UndoManager();
    private LineNumberArea lineNumberArea;
    private JScrollPane scrollPane;
    private boolean isModified;
    private List<Runnable> modifiedListeners;
    private PrologSyntaxHighlighter syntaxHighlighter;
    private javax.swing.Timer highlightTimer;
    
    // Breakpoints and debug state
    private java.util.Set<Integer> breakpointLines = new java.util.HashSet<>();
    private int debugHighlightLine = -1; // Currently executing line during debug (-1 = none)
    // Compile-error markers (ISS-2025-0326): wavy underline in the text + a gutter marker + tooltip.
    private final java.util.Set<Integer> errorLines = new java.util.TreeSet<>();
    private final java.util.Map<Integer, String> errorMessages = new java.util.HashMap<>();
    private final java.util.List<Object> errorHighlightTags = new java.util.ArrayList<>();
    private static final WavyUnderlinePainter WAVY_UNDERLINE = new WavyUnderlinePainter();

    // M04/M20 find: transient highlight of every match + a match counter the Find dialog can read.
    private final java.util.List<Object> findHighlightTags = new java.util.ArrayList<>();
    private int lastFindMatchCount = 0;
    private static final javax.swing.text.Highlighter.HighlightPainter FIND_ALL_PAINTER =
        new DefaultHighlighter.DefaultHighlightPainter(new Color(255, 235, 120));

    // M01 bracket matching: transient highlight of the bracket pair around the caret.
    private final java.util.List<Object> bracketHighlightTags = new java.util.ArrayList<>();
    private static final javax.swing.text.Highlighter.HighlightPainter BRACKET_MATCH_PAINTER =
        new DefaultHighlighter.DefaultHighlightPainter(new Color(160, 220, 160));

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

        // Undo/redo: track document edits and bind Ctrl+Z / Ctrl+Y / Ctrl+Shift+Z (IDE-P0 fix).
        undoManager.setLimit(1000);
        textPane.getDocument().addUndoableEditListener(new UndoableEditListener() {
            @Override public void undoableEditHappened(UndoableEditEvent e) {
                // Skip the bulk attribute changes from syntax highlighting (style-only, not content).
                if (e.getEdit().getPresentationName().toLowerCase().contains("style")) return;
                undoManager.addEdit(e.getEdit());
            }
        });
        InputMap im = textPane.getInputMap();
        ActionMap am = textPane.getActionMap();
        int menuMask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_Z, menuMask), "jprolog-undo");
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_Y, menuMask), "jprolog-redo");
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_Z, menuMask | InputEvent.SHIFT_DOWN_MASK), "jprolog-redo");
        am.put("jprolog-undo", new AbstractAction() {
            @Override public void actionPerformed(ActionEvent e) { undo(); }
        });
        am.put("jprolog-redo", new AbstractAction() {
            @Override public void actionPerformed(ActionEvent e) { redo(); }
        });
        // Format source (Ctrl+Alt+L) — pretty-print via the v2 parser/writer.
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_L, menuMask | InputEvent.ALT_DOWN_MASK), "jprolog-format");
        am.put("jprolog-format", new AbstractAction() {
            @Override public void actionPerformed(ActionEvent e) { formatSource(); }
        });
        // Code completion (Ctrl+Space) — predicates/builtins/variables. (ISS-2025-0324)
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_SPACE, menuMask), "jprolog-complete");
        am.put("jprolog-complete", new AbstractAction() {
            @Override public void actionPerformed(ActionEvent e) { showCompletion(); }
        });

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

        // Restore persisted breakpoints for this file (ISS-2025-0322)
        loadBreakpoints();
    }
    
    /**
     * Configure syntax highlighting.
     */
    private void setupSyntaxHighlighting() {
        // Debounce re-highlighting so it runs once after a typing pause instead of on every keystroke
        // (the full-document re-style is otherwise O(file) per character — laggy on large files).
        highlightTimer = new javax.swing.Timer(150, e -> {
            if (syntaxHighlighter != null) syntaxHighlighter.highlightSyntax();
        });
        highlightTimer.setRepeats(false);
        textPane.getDocument().addDocumentListener(new DocumentListener() {
            @Override public void insertUpdate(DocumentEvent e) { highlightTimer.restart(); }
            @Override public void removeUpdate(DocumentEvent e) { highlightTimer.restart(); }
            @Override public void changedUpdate(DocumentEvent e) { /* attribute-only change */ }
        });
        // Initial highlight of the loaded content.
        if (syntaxHighlighter != null) SwingUtilities.invokeLater(() -> syntaxHighlighter.highlightSyntax());

        // Add listener for current line highlighting
        textPane.addCaretListener(e -> highlightCurrentLine());

        // M01: bracket matching (caret listener) + auto-close/skip-over (document filter).
        setupBracketMatchingAndAutoClose();
    }

    // ===================== M01 BRACKET MATCHING + AUTO-CLOSE =====================

    private static final String OPEN_BRACKETS = "([{";
    private static final String CLOSE_BRACKETS = ")]}";

    /**
     * M01: install caret-driven bracket-pair highlighting plus a DocumentFilter that auto-inserts the
     * matching closer for ( [ { ' " (keeping the caret between the pair), skips over a just-typed closer
     * that already sits at the caret, and stays inert inside line comments and string/quote literals.
     * Wired separately from the syntax-highlight debounce and the undo manager so neither is disturbed.
     */
    private void setupBracketMatchingAndAutoClose() {
        // Bracket-pair highlight follows the caret.
        textPane.addCaretListener(e -> SwingUtilities.invokeLater(this::highlightMatchingBracket));

        Document doc = textPane.getDocument();
        if (doc instanceof AbstractDocument) {
            ((AbstractDocument) doc).setDocumentFilter(new BracketAutoCloseFilter());
        }
    }

    /** Map an opening bracket/quote to the character that closes it. */
    private static char closerFor(char open) {
        switch (open) {
            case '(': return ')';
            case '[': return ']';
            case '{': return '}';
            case '\'': return '\'';
            case '"': return '"';
            default: return 0;
        }
    }

    /** True when {@code offset} falls inside a % line comment or an unterminated quote on its line. */
    private boolean isInCommentOrString(String text, int offset) {
        // Walk from the start of the current line to the offset, tracking quote state and % comments.
        int lineStart = text.lastIndexOf('\n', Math.max(0, offset - 1)) + 1;
        char quote = 0;
        for (int i = lineStart; i < offset && i < text.length(); i++) {
            char c = text.charAt(i);
            if (quote != 0) {
                if (c == quote) quote = 0;            // closing quote (escapes are rare in Prolog source)
            } else if (c == '\'' || c == '"') {
                quote = c;
            } else if (c == '%') {
                return true;                          // rest of the line is a comment
            }
        }
        return quote != 0;
    }

    /** Clear and (if a matching pair exists around the caret) re-highlight the bracket pair. */
    private void highlightMatchingBracket() {
        Highlighter hl = textPane.getHighlighter();
        for (Object tag : bracketHighlightTags) {
            try { hl.removeHighlight(tag); } catch (Exception ignored) {}
        }
        bracketHighlightTags.clear();

        try {
            String text = textPane.getText();
            int caret = textPane.getCaretPosition();
            int len = text.length();

            // Consider the char just before the caret first, then the char at the caret.
            int bracketPos = -1;
            if (caret > 0 && isBracket(text.charAt(caret - 1))) {
                bracketPos = caret - 1;
            } else if (caret < len && isBracket(text.charAt(caret))) {
                bracketPos = caret;
            }
            if (bracketPos < 0) return;

            int matchPos = findMatchingBracket(text, bracketPos);
            if (matchPos < 0) return;

            int a = Math.min(bracketPos, matchPos);
            int b = Math.max(bracketPos, matchPos);
            bracketHighlightTags.add(hl.addHighlight(a, a + 1, BRACKET_MATCH_PAINTER));
            bracketHighlightTags.add(hl.addHighlight(b, b + 1, BRACKET_MATCH_PAINTER));
        } catch (Exception ignored) { /* best-effort */ }
    }

    private static boolean isBracket(char c) {
        return OPEN_BRACKETS.indexOf(c) >= 0 || CLOSE_BRACKETS.indexOf(c) >= 0;
    }

    /** Scan for the bracket matching the one at {@code pos}; -1 if unbalanced. */
    private static int findMatchingBracket(String text, int pos) {
        char c = text.charAt(pos);
        int openIdx = OPEN_BRACKETS.indexOf(c);
        int closeIdx = CLOSE_BRACKETS.indexOf(c);
        if (openIdx >= 0) {
            char open = c, close = CLOSE_BRACKETS.charAt(openIdx);
            int depth = 0;
            for (int i = pos; i < text.length(); i++) {
                char ch = text.charAt(i);
                if (ch == open) depth++;
                else if (ch == close && --depth == 0) return i;
            }
        } else if (closeIdx >= 0) {
            char close = c, open = OPEN_BRACKETS.charAt(closeIdx);
            int depth = 0;
            for (int i = pos; i >= 0; i--) {
                char ch = text.charAt(i);
                if (ch == close) depth++;
                else if (ch == open && --depth == 0) return i;
            }
        }
        return -1;
    }

    /**
     * DocumentFilter that performs bracket/quote auto-close and skip-over. All mutations still go
     * through the document model, so the existing UndoableEditListener and DocumentListeners observe
     * them normally (no special-casing of undo or the syntax-highlight debounce required).
     */
    private class BracketAutoCloseFilter extends DocumentFilter {
        @Override
        public void insertString(FilterBypass fb, int offset, String string, AttributeSet attr)
                throws BadLocationException {
            if (!handleTypedChar(fb, offset, 0, string, attr)) {
                super.insertString(fb, offset, string, attr);
            }
        }

        @Override
        public void replace(FilterBypass fb, int offset, int length, String string, AttributeSet attr)
                throws BadLocationException {
            if (!handleTypedChar(fb, offset, length, string, attr)) {
                super.replace(fb, offset, length, string, attr);
            }
        }

        /**
         * Handle a single typed character. Returns true if the edit was fully handled here.
         * Falls through (returns false) for pastes, multi-char inserts, and selection replacements.
         */
        private boolean handleTypedChar(FilterBypass fb, int offset, int length, String string,
                                        AttributeSet attr) throws BadLocationException {
            if (string == null || string.length() != 1 || length != 0) return false;
            char c = string.charAt(0);
            Document d = fb.getDocument();
            int docLen = d.getLength();
            String full = docLen > 0 ? d.getText(0, docLen) : "";

            // Skip-over: typing a closer (or a quote) that already sits right at the caret just advances.
            char next = (offset < docLen) ? full.charAt(offset) : 0;
            boolean isCloser = CLOSE_BRACKETS.indexOf(c) >= 0;
            boolean isQuote = (c == '\'' || c == '"');
            if ((isCloser || isQuote) && next == c) {
                final int target = offset + 1;
                SwingUtilities.invokeLater(() -> {
                    if (target <= textPane.getDocument().getLength()) textPane.setCaretPosition(target);
                });
                return true;
            }

            // Auto-close only for openers/quotes, and not inside comments/strings.
            char close = closerFor(c);
            boolean isOpener = OPEN_BRACKETS.indexOf(c) >= 0;
            if (close == 0 || (!isOpener && !isQuote)) return false;
            if (isInCommentOrString(full, offset)) return false;

            // Don't auto-close a quote when the caret already abuts an identifier char (likely a
            // word-internal apostrophe / closing an existing literal) — keeps typing natural.
            if (isQuote) {
                char prev = (offset > 0) ? full.charAt(offset - 1) : 0;
                if (isIdentChar(prev) || isIdentChar(next)) return false;
            }

            fb.insertString(offset, String.valueOf(c) + close, attr);
            final int target = offset + 1;           // park the caret between the pair
            SwingUtilities.invokeLater(() -> {
                if (target <= textPane.getDocument().getLength()) textPane.setCaretPosition(target);
            });
            return true;
        }
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
            undoManager.discardAllEdits();   // loading the file is not an undoable edit
            
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
                clearStaleErrorMarkers(e);   // M08
            }

            @Override
            public void removeUpdate(DocumentEvent e) {
                markAsModified();
                clearStaleErrorMarkers(e);   // M08
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
            // M03: write UTF-8 explicitly so the on-disk encoding matches the UTF-8 load in loadFile()
            // (FileWriter used the platform default charset, corrupting non-ASCII atoms/strings).
            Files.write(file.toPath(), content.getBytes(StandardCharsets.UTF_8));

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
            Element root = textPane.getDocument().getDefaultRootElement();
            if (lineNumber < 1 || lineNumber > root.getElementCount()) return;
            Element lineElement = root.getElement(lineNumber - 1);
            int lineStart = lineElement.getStartOffset();
            int lineEnd = Math.max(lineStart, lineElement.getEndOffset() - 1);
            // Persistent wavy red underline (doesn't move the caret or hijack the selection).
            errorHighlightTags.add(textPane.getHighlighter().addHighlight(lineStart, lineEnd, WAVY_UNDERLINE));
            errorLines.add(lineNumber);
            errorMessages.put(lineNumber, errorMessage);
            if (lineNumberArea != null) lineNumberArea.repaint();
        } catch (Exception e) {
            // Ignora
        }
    }

    /**
     * Rimuove l'evidenziazione degli errori.
     */
    public void clearErrorHighlighting() {
        for (Object tag : errorHighlightTags) {
            try { textPane.getHighlighter().removeHighlight(tag); } catch (Exception ignored) {}
        }
        errorHighlightTags.clear();
        errorLines.clear();
        errorMessages.clear();
        textPane.setToolTipText(null);
        if (lineNumberArea != null) lineNumberArea.repaint();
    }

    /**
     * M08: drop stale compile-error markers for the line(s) touched by an edit so the wavy underline,
     * gutter '!' and hover tooltip disappear as soon as the user starts fixing the offending line.
     * Cheap no-op when there are no active error markers. Runs inside the document mutation, so it
     * only touches the highlighter (which is edit-safe) and rebuilds the line/message bookkeeping.
     */
    private void clearStaleErrorMarkers(DocumentEvent e) {
        if (errorLines.isEmpty()) return;
        try {
            Element root = textPane.getDocument().getDefaultRootElement();
            int offset = e.getOffset();
            int len = e.getLength();
            int firstLine = root.getElementIndex(offset) + 1;                       // 1-based
            int lastLine = root.getElementIndex(Math.min(offset + Math.max(len, 0),
                    Math.max(0, textPane.getDocument().getLength()))) + 1;
            boolean affected = false;
            for (int ln = firstLine; ln <= lastLine; ln++) {
                if (errorLines.contains(ln)) { affected = true; break; }
            }
            if (!affected) return;
            // Snapshot the surviving error lines (those outside the edited range), then fully rebuild the
            // highlighter tags — their spans can no longer be trusted once the affected lines changed.
            java.util.Map<Integer, String> survivors = new java.util.HashMap<>();
            for (Integer ln : errorLines) {
                if (ln < firstLine || ln > lastLine) {
                    String msg = errorMessages.get(ln);
                    survivors.put(ln, msg != null ? msg : "");
                }
            }
            clearErrorHighlighting();
            // Re-apply markers for the untouched error lines after the document settles.
            if (!survivors.isEmpty()) {
                SwingUtilities.invokeLater(() -> {
                    for (java.util.Map.Entry<Integer, String> en : survivors.entrySet()) {
                        highlightError(en.getKey(), en.getValue());
                    }
                });
            }
        } catch (Exception ignored) { /* best-effort marker cleanup */ }
    }

    /** Wavy red underline painter for compile-error spans (ISS-2025-0326). */
    private static class WavyUnderlinePainter implements javax.swing.text.Highlighter.HighlightPainter {
        @Override
        public void paint(Graphics g, int p0, int p1, Shape bounds, javax.swing.text.JTextComponent c) {
            try {
                Rectangle r0 = c.modelToView(p0);
                Rectangle r1 = c.modelToView(p1);
                if (r0 == null || r1 == null) return;
                int y = r0.y + r0.height - 2;
                int xEnd = (r1.y == r0.y) ? r1.x : c.getWidth() - 2;
                g.setColor(Color.RED);
                for (int x = r0.x; x < xEnd - 1; x += 4) {
                    g.drawLine(x, y, x + 2, y - 2);
                    g.drawLine(x + 2, y - 2, Math.min(x + 4, xEnd), y);
                }
            } catch (Exception ignored) {}
        }
    }
    
    /**
     * Trova testo nell'editor (forward search, wrap-around). Kept for existing callers.
     */
    public boolean findText(String searchText, boolean caseSensitive, boolean wholeWord) {
        return findText(searchText, caseSensitive, wholeWord, true);
    }

    /**
     * M04/M20: directional incremental find with wrap-around.
     * @param forward true to search forward from the caret, false to search backward (uses
     *                lastIndexOf before the selection start); both directions wrap around the buffer.
     * @return true if a match was found and selected.
     */
    public boolean findText(String searchText, boolean caseSensitive, boolean wholeWord, boolean forward) {
        if (searchText == null || searchText.isEmpty()) {
            return false;
        }

        String content = textPane.getText();
        String search = caseSensitive ? searchText : searchText.toLowerCase();
        String text = caseSensitive ? content : content.toLowerCase();
        int len = searchText.length();

        int foundPos;
        if (forward) {
            // Start just after the current selection so repeated Find advances past the active match.
            int from = Math.max(textPane.getSelectionStart() + 1, textPane.getCaretPosition());
            if (from > text.length()) from = 0;
            foundPos = text.indexOf(search, from);
            if (foundPos == -1) foundPos = text.indexOf(search, 0);   // wrap to top
        } else {
            // Search before the current selection; wrap to the end of the buffer if nothing precedes it.
            int before = textPane.getSelectionStart() - 1;
            foundPos = (before >= 0) ? text.lastIndexOf(search, before) : -1;
            if (foundPos == -1) foundPos = text.lastIndexOf(search);  // wrap to bottom
        }

        if (foundPos != -1) {
            textPane.select(foundPos, foundPos + len);
            textPane.setCaretPosition(foundPos);
            return true;
        }
        return false;
    }

    /**
     * M04/M20: highlight every occurrence of {@code searchText} with a transient (non-persistent)
     * highlighter and record the count. Replaces any previous find highlights. Returns the number of
     * matches, also retrievable via {@link #getLastFindMatchCount()}.
     */
    public int highlightAllMatches(String searchText, boolean caseSensitive) {
        clearFindHighlights();
        if (searchText == null || searchText.isEmpty()) {
            lastFindMatchCount = 0;
            return 0;
        }
        String content = textPane.getText();
        String search = caseSensitive ? searchText : searchText.toLowerCase();
        String text = caseSensitive ? content : content.toLowerCase();
        int len = searchText.length();
        int count = 0;
        Highlighter hl = textPane.getHighlighter();
        int idx = text.indexOf(search);
        while (idx >= 0) {
            try {
                findHighlightTags.add(hl.addHighlight(idx, idx + len, FIND_ALL_PAINTER));
            } catch (BadLocationException ignored) {}
            count++;
            idx = text.indexOf(search, idx + Math.max(len, 1));
        }
        lastFindMatchCount = count;
        if (lastFindMatchCount == 0) textPane.repaint();
        return count;
    }

    /** M04/M20: number of matches from the last {@link #highlightAllMatches} call. */
    public int getLastFindMatchCount() {
        return lastFindMatchCount;
    }

    /** M04/M20: remove the transient find-all highlights (leaves error/debug highlights intact). */
    public void clearFindHighlights() {
        Highlighter hl = textPane.getHighlighter();
        for (Object tag : findHighlightTags) {
            try { hl.removeHighlight(tag); } catch (Exception ignored) {}
        }
        findHighlightTags.clear();
        lastFindMatchCount = 0;
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
        if (searchText == null || searchText.isEmpty()) return 0;
        String content = textPane.getText();
        // Count matches with a Matcher rather than deriving the count from a length delta — the old
        // formula divided by (search.len - replace.len), an ArithmeticException when they are equal.
        int flags = caseSensitive ? 0 : (Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE);
        Matcher m = Pattern.compile(Pattern.quote(searchText), flags).matcher(content);
        StringBuffer sb = new StringBuffer();
        int count = 0;
        while (m.find()) {
            count++;
            m.appendReplacement(sb, Matcher.quoteReplacement(replaceText));
        }
        m.appendTail(sb);
        String result = sb.toString();
        if (!content.equals(result)) {
            textPane.setText(result);
            markAsModified();
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
    
    // ===================== CODE COMPLETION (Ctrl+Space) =====================
    private static final String[] PROLOG_BUILTINS = {
        "atom_codes", "atom_chars", "atom_length", "atom_concat", "atom_string", "atom_number",
        "number_codes", "char_code", "sub_atom", "findall", "bagof", "setof", "forall", "aggregate_all",
        "member", "append", "length", "reverse", "nth0", "nth1", "last", "msort", "sort", "permutation",
        "maplist", "foldl", "include", "exclude", "between", "succ_or_zero", "assert", "asserta", "assertz",
        "retract", "retractall", "functor", "arg", "copy_term", "write", "writeln", "print", "nl", "read",
        "format", "call", "catch", "throw", "ground", "is_list", "succ", "tab", "true", "fail", "halt"
    };

    /** Show a completion popup for the identifier before the caret (ISS-2025-0324). Candidates: the
     *  program's own predicates, common built-ins/keywords, and the variables in the current clause. */
    private void showCompletion() {
        try {
            int caret = textPane.getCaretPosition();
            String text = textPane.getText();
            int start = caret;
            while (start > 0 && isIdentChar(text.charAt(start - 1))) start--;
            String prefix = text.substring(start, caret);
            if (prefix.isEmpty()) return;
            boolean wantVar = Character.isUpperCase(prefix.charAt(0)) || prefix.charAt(0) == '_';

            java.util.TreeSet<String> cands = new java.util.TreeSet<>();
            if (wantVar) {
                // variables in the current clause (back to the previous '.')
                int cs = text.lastIndexOf('.', start - 1) + 1;
                Matcher vm = VARIABLE_PATTERN.matcher(text.substring(cs, Math.min(text.length(), caret + 200)));
                while (vm.find()) cands.add(vm.group());
            } else {
                for (String k : PROLOG_KEYWORDS) cands.add(k);
                for (String b : PROLOG_BUILTINS) cands.add(b);
                if (ide != null && ide.getPrologEngine() != null) {
                    for (String pi : ide.getPrologEngine().getCurrentPredicates()) {
                        int slash = pi.lastIndexOf('/');
                        cands.add(slash > 0 ? pi.substring(0, slash) : pi);
                    }
                }
                // functors already used in this file
                Matcher fm = Pattern.compile("\\b([a-z][a-zA-Z0-9_]*)\\s*\\(").matcher(text);
                while (fm.find()) cands.add(fm.group(1));
            }
            java.util.List<String> matches = new java.util.ArrayList<>();
            for (String c : cands) if (c.startsWith(prefix) && !c.equals(prefix)) matches.add(c);
            if (matches.isEmpty()) { java.awt.Toolkit.getDefaultToolkit().beep(); return; }

            final int fStart = start;
            if (matches.size() == 1) { applyCompletion(fStart, caret, matches.get(0)); return; }

            final JList<String> list = new JList<>(matches.toArray(new String[0]));
            list.setSelectedIndex(0);
            list.setVisibleRowCount(Math.min(10, matches.size()));
            list.setFont(textPane.getFont());
            final JPopupMenu popup = new JPopupMenu();
            popup.setLayout(new BorderLayout());
            popup.add(new JScrollPane(list), BorderLayout.CENTER);
            final int fCaret = caret;
            list.addMouseListener(new java.awt.event.MouseAdapter() {
                @Override public void mouseClicked(java.awt.event.MouseEvent e) {
                    if (e.getClickCount() == 2) { applyCompletion(fStart, fCaret, list.getSelectedValue()); popup.setVisible(false); }
                }
            });
            list.addKeyListener(new KeyListener() {
                @Override public void keyPressed(KeyEvent e) {
                    if (e.getKeyCode() == KeyEvent.VK_ENTER) {
                        applyCompletion(fStart, fCaret, list.getSelectedValue()); popup.setVisible(false); e.consume();
                    } else if (e.getKeyCode() == KeyEvent.VK_ESCAPE) { popup.setVisible(false); e.consume(); }
                }
                @Override public void keyReleased(KeyEvent e) {}
                @Override public void keyTyped(KeyEvent e) {}
            });
            java.awt.Rectangle r = textPane.modelToView(caret);
            popup.show(textPane, r.x, r.y + r.height);
            list.requestFocusInWindow();
        } catch (Exception ignored) { /* completion is best-effort */ }
    }

    private void applyCompletion(int start, int caret, String value) {
        if (value == null) return;
        try {
            textPane.getDocument().remove(start, caret - start);
            textPane.getDocument().insertString(start, value, null);
        } catch (BadLocationException ignored) {}
    }

    private static boolean isIdentChar(char c) { return Character.isLetterOrDigit(c) || c == '_'; }

    /** Pretty-print the whole document (one goal per body line, blank line between clauses), preserving
     *  inter-clause comments. Unparseable source is left untouched. (IDE source-formatter feature.) */
    public void formatSource() {
        String src = textPane.getText();
        String formatted;
        try {
            formatted = it.denzosoft.jprolog.core.write.v2.PrologFormatter.format(
                src, it.denzosoft.jprolog.core.operator.OperatorTable.getDefault());
        } catch (RuntimeException ex) {
            return;   // never corrupt the buffer
        }
        if (formatted == null || formatted.equals(src)) return;
        int caret = textPane.getCaretPosition();
        textPane.setText(formatted);
        textPane.setCaretPosition(Math.min(caret, textPane.getDocument().getLength()));
        markAsModified();
        if (syntaxHighlighter != null) SwingUtilities.invokeLater(() -> syntaxHighlighter.highlightSyntax());
    }

    /** Undo the last content edit (IDE-P0 fix). */
    public void undo() {
        try { if (undoManager.canUndo()) undoManager.undo(); } catch (CannotUndoException ignored) {}
    }

    /** Redo the last undone edit. */
    public void redo() {
        try { if (undoManager.canRedo()) undoManager.redo(); } catch (CannotRedoException ignored) {}
    }

    public boolean canUndo() { return undoManager.canUndo(); }
    public boolean canRedo() { return undoManager.canRedo(); }

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
        boolean added;
        if (breakpointLines.contains(lineNumber)) {
            breakpointLines.remove(lineNumber);
            added = false;
        } else {
            breakpointLines.add(lineNumber);
            added = true;
        }
        lineNumberArea.repaint();
        saveBreakpoints();                 // ISS-2025-0322: persist across sessions
        return added;
    }

    /** Sidecar file holding this source file's breakpoint lines. */
    private File breakpointStore() {
        return new File(file.getParentFile(), "." + file.getName() + ".bps");
    }

    /** Persist breakpoint lines next to the source so they survive IDE/file close (ISS-2025-0322). */
    private void saveBreakpoints() {
        if (file == null) return;
        File store = breakpointStore();
        try {
            if (breakpointLines.isEmpty()) { store.delete(); return; }
            StringBuilder sb = new StringBuilder();
            for (Integer l : new java.util.TreeSet<>(breakpointLines)) sb.append(l).append('\n');
            Files.write(store.toPath(), sb.toString().getBytes("UTF-8"));
        } catch (IOException ignored) { /* persistence is best-effort */ }
    }

    /** Restore breakpoint lines (and re-register them with the debugger) when the file is opened. */
    private void loadBreakpoints() {
        if (file == null) return;
        File store = breakpointStore();
        if (!store.exists()) return;
        try {
            breakpointLines.clear();
            for (String line : new String(Files.readAllBytes(store.toPath()), "UTF-8").split("\n")) {
                line = line.trim();
                if (line.isEmpty()) continue;
                try {
                    int ln = Integer.parseInt(line);
                    breakpointLines.add(ln);
                    // Best-effort: register the predicate now if the engine already knows this file;
                    // otherwise the visual marker is restored and re-registers when toggled/consulted.
                    if (ide != null && ide.getDebugPanel() != null && ide.getPrologEngine() != null) {
                        String pred = ide.getPrologEngine().getPredicateIndicatorAtLine(ln);
                        if (pred != null) ide.getDebugPanel().addBreakpointProgrammatic(pred);
                    }
                } catch (NumberFormatException ignored) {}
            }
            if (lineNumberArea != null) lineNumberArea.repaint();
        } catch (IOException ignored) {}
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
        // ISS-2025-0326: rich marking (wavy underline + gutter marker + per-line tooltip), no caret hijack.
        highlightError(lineNumber, errorMessage);
    }

    /**
     * Clear all error highlights.
     */
    public void clearAllHighlights() {
        textPane.getHighlighter().removeAllHighlights();
        errorHighlightTags.clear();
        errorLines.clear();
        errorMessages.clear();
        findHighlightTags.clear();        // M04/M20: keep find-tag bookkeeping consistent
        lastFindMatchCount = 0;
        textPane.setToolTipText(null);
        if (lineNumberArea != null) lineNumberArea.repaint();
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
            javax.swing.ToolTipManager.sharedInstance().registerComponent(this);   // ISS-2025-0326

            // Click in gutter toggles breakpoint
            addMouseListener(new java.awt.event.MouseAdapter() {
                @Override
                public void mouseClicked(java.awt.event.MouseEvent e) {
                    int lineNum = getLineAtPoint(e.getY());
                    if (lineNum > 0) {
                        boolean added = toggleBreakpoint(lineNum);
                        // Notify IDE debug panel if available
                        if (ide != null && ide.getDebugPanel() != null) {
                            // ISS-2025-0322: resolve the clause at this line accurately from the engine
                            // (real Rule source lines) instead of a fragile regex over the text; fall
                            // back to the regex only when the file has not been consulted yet.
                            String predicateName = null;
                            if (ide.getPrologEngine() != null) {
                                predicateName = ide.getPrologEngine().getPredicateIndicatorAtLine(lineNum);
                            }
                            if (predicateName == null) predicateName = extractPredicateAtLine(lineNum);
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

                        // Draw error marker (compile error on this line) - ISS-2025-0326
                        if (errorLines.contains(lineNum1Based)) {
                            int sz = 9;
                            int ex = 3;
                            int ey = lineY + (lineHeight - sz) / 2;
                            g2.setColor(new Color(200, 0, 0));
                            g2.fillOval(ex, ey, sz, sz);
                            g2.setColor(Color.WHITE);
                            g2.setFont(g2.getFont().deriveFont(Font.BOLD, 9f));
                            g2.drawString("!", ex + 3, ey + sz - 1);
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

        @Override
        public String getToolTipText(java.awt.event.MouseEvent e) {
            int line = getLineAtPoint(e.getY());                  // show the compile error on hover
            String msg = errorMessages.get(line);
            return msg != null ? "Line " + line + ": " + msg : null;
        }
    }
}