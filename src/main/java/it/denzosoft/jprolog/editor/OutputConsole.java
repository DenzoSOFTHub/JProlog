package it.denzosoft.jprolog.editor;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

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
    
    // START_CHANGE: ISS-2025-0176 - CLI command history
    // Command history for UP/DOWN arrow navigation
    private List<String> commandHistory = new ArrayList<>();
    private int historyIndex = -1;
    private static final int MAX_HISTORY_SIZE = 100;
    // END_CHANGE: ISS-2025-0176

    // Configurations
    private boolean showTimestamps = true;
    private int maxLines = 1000;
    private SimpleDateFormat timeFormat = new SimpleDateFormat("HH:mm:ss");

    // START_CHANGE: ISS-2025-0260 - Console incremental find (Ctrl+F)
    // Self-contained incremental search bar for the output console.
    // Implemented as a lightweight undecorated JWindow anchored to the top of
    // this text pane so that OutputConsole can remain a JTextPane (it is added
    // directly as a tab component, so it cannot host sibling components itself).
    private JWindow findBar;                 // floating find bar window
    private JTextField findField;            // search text input
    private JLabel findMatchLabel;           // "n/total" match counter
    private final List<int[]> findMatches = new ArrayList<>(); // [start,end] offsets
    private int findCurrentIndex = -1;       // index into findMatches of active match
    private final List<Object> findHighlightTags = new ArrayList<>(); // highlighter tags
    private final Highlighter.HighlightPainter findAllPainter =
            new DefaultHighlighter.DefaultHighlightPainter(new Color(255, 235, 120)); // all matches
    private final Highlighter.HighlightPainter findCurrentPainter =
            new DefaultHighlighter.DefaultHighlightPainter(new Color(255, 160, 60));  // active match
    private ComponentAdapter findReposListener; // keeps the bar aligned with the pane
    // END_CHANGE: ISS-2025-0260

    public OutputConsole() {
        super();

        initializeStyles();
        setupConsole();
        setupEventHandlers();
        // START_CHANGE: ISS-2025-0260 - install Ctrl+F / Esc key bindings
        setupFindKeyBindings();
        // END_CHANGE: ISS-2025-0260

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
            // START_CHANGE: ISS-2025-0176 - Add command to history on Enter
            String input = currentInput.toString().trim();
            appendText("\n", normalStyle);

            if (!input.isEmpty()) {
                // Add to command history, avoiding consecutive duplicates
                if (commandHistory.isEmpty() || !commandHistory.get(commandHistory.size() - 1).equals(input)) {
                    commandHistory.add(input);
                    if (commandHistory.size() > MAX_HISTORY_SIZE) {
                        commandHistory.remove(0);
                    }
                }
                historyIndex = commandHistory.size(); // Reset to end
                processInput(input);
            }
            // END_CHANGE: ISS-2025-0176

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
                
            // START_CHANGE: ISS-2025-0176 - Navigate command history with UP/DOWN arrows
            case KeyEvent.VK_UP:
                if (!commandHistory.isEmpty()) {
                    if (historyIndex > 0) {
                        historyIndex--;
                    }
                    replaceCurrentInput(commandHistory.get(historyIndex));
                }
                e.consume();
                break;

            case KeyEvent.VK_DOWN:
                if (!commandHistory.isEmpty()) {
                    if (historyIndex < commandHistory.size() - 1) {
                        historyIndex++;
                        replaceCurrentInput(commandHistory.get(historyIndex));
                    } else {
                        historyIndex = commandHistory.size();
                        replaceCurrentInput("");
                    }
                }
                e.consume();
                break;
            // END_CHANGE: ISS-2025-0176
        }
    }
    
    // START_CHANGE: ISS-2025-0176 - Replace current input line with history entry
    /**
     * Replaces the current input text with the given string.
     * Used for command history navigation.
     */
    private void replaceCurrentInput(String text) {
        try {
            // Remove current input from document
            int inputLength = document.getLength() - promptPosition;
            if (inputLength > 0) {
                document.remove(promptPosition, inputLength);
            }
            // Insert new text
            document.insertString(promptPosition, text, normalStyle);
            currentInput.setLength(0);
            currentInput.append(text);
            setCaretPosition(document.getLength());
        } catch (BadLocationException ex) {
            // Ignore replacement errors
        }
    }
    // END_CHANGE: ISS-2025-0176

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

    // START_CHANGE: ISS-2025-0260 - Console incremental find (Ctrl+F)
    // ------------------------------------------------------------------
    //  Output console search bar (Ctrl+F)
    //  - Case-insensitive incremental search over the console text.
    //  - Next / Prev navigation with wrap-around.
    //  - Live match count "current/total".
    //  - All matches highlighted via the Highlighter; the active match uses
    //    a stronger colour.
    //  - Esc closes the bar and clears highlights.
    // ------------------------------------------------------------------

    /**
     * Installs the Ctrl+F (open search) and Escape (close search) key bindings
     * on this text pane. These are registered on the WHEN_FOCUSED input map so
     * they work whenever the console has focus.
     */
    private void setupFindKeyBindings() {
        InputMap im = getInputMap(JComponent.WHEN_FOCUSED);
        ActionMap am = getActionMap();

        int menuMask = Toolkit.getDefaultToolkit().getMenuShortcutKeyMask();
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_F, menuMask), "console-find");
        am.put("console-find", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                showFindBar();
            }
        });

        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), "console-find-close");
        am.put("console-find-close", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                hideFindBar();
            }
        });
    }

    /**
     * Shows (creating it lazily) the floating find bar and gives it focus.
     * If the console has a current selection, it is used as the initial query.
     */
    public void showFindBar() {
        if (findBar == null) {
            buildFindBar();
        }

        // Seed the field with any current selection for convenience.
        String selection = getSelectedText();
        if (selection != null && !selection.isEmpty() && selection.indexOf('\n') < 0) {
            findField.setText(selection);
        }

        positionFindBar();
        findBar.setVisible(true);

        // Track parent window movement/resize to keep the bar aligned.
        installFindRepositionListener();

        SwingUtilities.invokeLater(() -> {
            findField.requestFocusInWindow();
            findField.selectAll();
            updateSearch(true); // refresh highlights for any seeded text
        });
    }

    /**
     * Hides the find bar, removes all search highlights and returns focus to
     * the console. Safe to call when the bar was never shown.
     */
    public void hideFindBar() {
        clearFindHighlights();
        findMatches.clear();
        findCurrentIndex = -1;
        if (findBar != null) {
            findBar.setVisible(false);
        }
        uninstallFindRepositionListener();
        requestFocusInWindow();
    }

    /**
     * Builds the find bar UI (text field, prev/next buttons, match counter,
     * close button) inside an undecorated JWindow owned by this pane's window.
     */
    private void buildFindBar() {
        Window owner = SwingUtilities.getWindowAncestor(this);
        findBar = (owner != null) ? new JWindow(owner) : new JWindow();

        JPanel bar = new JPanel(new FlowLayout(FlowLayout.LEFT, 4, 3));
        bar.setBorder(BorderFactory.createCompoundBorder(
                BorderFactory.createLineBorder(new Color(120, 120, 120)),
                BorderFactory.createEmptyBorder(2, 4, 2, 4)));
        bar.setBackground(new Color(245, 245, 245));

        JLabel title = new JLabel("Find:");

        findField = new JTextField(20);
        findField.setToolTipText("Search the console (case-insensitive)");

        findMatchLabel = new JLabel("0 matches");
        findMatchLabel.setForeground(new Color(90, 90, 90));

        JButton prevBtn = new JButton("▲"); // up triangle
        prevBtn.setToolTipText("Previous match (Shift+Enter)");
        prevBtn.setMargin(new Insets(1, 6, 1, 6));
        prevBtn.setFocusable(false);
        prevBtn.addActionListener(e -> findNext(false));

        JButton nextBtn = new JButton("▼"); // down triangle
        nextBtn.setToolTipText("Next match (Enter)");
        nextBtn.setMargin(new Insets(1, 6, 1, 6));
        nextBtn.setFocusable(false);
        nextBtn.addActionListener(e -> findNext(true));

        JButton closeBtn = new JButton("✕"); // x
        closeBtn.setToolTipText("Close (Esc)");
        closeBtn.setMargin(new Insets(1, 6, 1, 6));
        closeBtn.setFocusable(false);
        closeBtn.addActionListener(e -> hideFindBar());

        bar.add(title);
        bar.add(findField);
        bar.add(prevBtn);
        bar.add(nextBtn);
        bar.add(findMatchLabel);
        bar.add(closeBtn);

        findBar.setContentPane(bar);

        // Incremental search: re-run on every text change.
        findField.getDocument().addDocumentListener(new DocumentListener() {
            @Override public void insertUpdate(DocumentEvent e) { updateSearch(true); }
            @Override public void removeUpdate(DocumentEvent e) { updateSearch(true); }
            @Override public void changedUpdate(DocumentEvent e) { updateSearch(true); }
        });

        // Enter = next, Shift+Enter = previous, Esc = close.
        InputMap fim = findField.getInputMap(JComponent.WHEN_FOCUSED);
        ActionMap fam = findField.getActionMap();
        fim.put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0), "find-next");
        fim.put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, KeyEvent.SHIFT_DOWN_MASK), "find-prev");
        fim.put(KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0), "find-close");
        fam.put("find-next", new AbstractAction() {
            @Override public void actionPerformed(ActionEvent e) { findNext(true); }
        });
        fam.put("find-prev", new AbstractAction() {
            @Override public void actionPerformed(ActionEvent e) { findNext(false); }
        });
        fam.put("find-close", new AbstractAction() {
            @Override public void actionPerformed(ActionEvent e) { hideFindBar(); }
        });
    }

    /**
     * Positions the find bar at the top-right of the visible console area.
     */
    private void positionFindBar() {
        if (findBar == null || !isShowing()) {
            return;
        }
        findBar.pack();
        Point paneLoc = getLocationOnScreen();
        int paneWidth = getVisibleRect().width;
        int barWidth = findBar.getWidth();
        // Anchor near the top-right, but never off the left edge.
        int x = paneLoc.x + Math.max(0, paneWidth - barWidth - 18);
        int y = paneLoc.y + 2;
        findBar.setLocation(x, y);
    }

    /**
     * Adds a listener to the owning window so the find bar follows the IDE
     * when it is moved or resized.
     */
    private void installFindRepositionListener() {
        if (findReposListener != null) {
            return;
        }
        final Window owner = SwingUtilities.getWindowAncestor(this);
        if (owner == null) {
            return;
        }
        findReposListener = new ComponentAdapter() {
            @Override public void componentMoved(ComponentEvent e) { positionFindBar(); }
            @Override public void componentResized(ComponentEvent e) { positionFindBar(); }
        };
        owner.addComponentListener(findReposListener);
    }

    /**
     * Removes the reposition listener installed by
     * {@link #installFindRepositionListener()}.
     */
    private void uninstallFindRepositionListener() {
        if (findReposListener == null) {
            return;
        }
        Window owner = SwingUtilities.getWindowAncestor(this);
        if (owner != null) {
            owner.removeComponentListener(findReposListener);
        }
        findReposListener = null;
    }

    /**
     * Recomputes all matches for the current query and refreshes highlights.
     *
     * @param moveToFirst when true, selects the first match at/after the caret
     *                    (used on incremental typing); when false, keeps the
     *                    current active index if still valid.
     */
    private void updateSearch(boolean moveToFirst) {
        if (findField == null) {
            return;
        }
        clearFindHighlights();
        findMatches.clear();

        String query = findField.getText();
        if (query == null || query.isEmpty()) {
            findCurrentIndex = -1;
            findField.setForeground(Color.BLACK);
            updateMatchLabel();
            return;
        }

        String content;
        try {
            content = document.getText(0, document.getLength());
        } catch (BadLocationException e) {
            content = "";
        }

        String lcContent = content.toLowerCase();
        String lcQuery = query.toLowerCase();
        int from = 0;
        int idx;
        while ((idx = lcContent.indexOf(lcQuery, from)) >= 0) {
            findMatches.add(new int[]{idx, idx + lcQuery.length()});
            from = idx + lcQuery.length();
        }

        if (findMatches.isEmpty()) {
            findCurrentIndex = -1;
            // Visual "not found" feedback on the field.
            findField.setForeground(new Color(200, 0, 0));
            updateMatchLabel();
            return;
        }

        findField.setForeground(Color.BLACK);

        if (moveToFirst || findCurrentIndex < 0 || findCurrentIndex >= findMatches.size()) {
            // Pick the first match at/after the current caret position.
            int caret = getCaretPosition();
            findCurrentIndex = 0;
            for (int i = 0; i < findMatches.size(); i++) {
                if (findMatches.get(i)[0] >= caret) {
                    findCurrentIndex = i;
                    break;
                }
            }
        }

        applyFindHighlights();
        scrollToCurrentMatch();
        updateMatchLabel();
    }

    /**
     * Advances to the next (forward=true) or previous (forward=false) match,
     * wrapping around the ends of the list.
     */
    private void findNext(boolean forward) {
        if (findMatches.isEmpty()) {
            updateSearch(true);
            return;
        }
        if (forward) {
            findCurrentIndex = (findCurrentIndex + 1) % findMatches.size();
        } else {
            findCurrentIndex = (findCurrentIndex - 1 + findMatches.size()) % findMatches.size();
        }
        applyFindHighlights();
        scrollToCurrentMatch();
        updateMatchLabel();
    }

    /**
     * Highlights every match, drawing the active one with a stronger colour.
     */
    private void applyFindHighlights() {
        clearFindHighlights();
        Highlighter hl = getHighlighter();
        for (int i = 0; i < findMatches.size(); i++) {
            int[] m = findMatches.get(i);
            try {
                Highlighter.HighlightPainter painter =
                        (i == findCurrentIndex) ? findCurrentPainter : findAllPainter;
                Object tag = hl.addHighlight(m[0], m[1], painter);
                findHighlightTags.add(tag);
            } catch (BadLocationException e) {
                // Ignore stale offsets (content may have changed concurrently).
            }
        }
    }

    /**
     * Removes all highlights previously added by the search.
     */
    private void clearFindHighlights() {
        Highlighter hl = getHighlighter();
        for (Object tag : findHighlightTags) {
            hl.removeHighlight(tag);
        }
        findHighlightTags.clear();
    }

    /**
     * Scrolls the console so the active match is visible and moves the caret to
     * it (without stealing focus from the find field).
     */
    private void scrollToCurrentMatch() {
        if (findCurrentIndex < 0 || findCurrentIndex >= findMatches.size()) {
            return;
        }
        int[] m = findMatches.get(findCurrentIndex);
        try {
            // Move the caret so subsequent incremental searches anchor here,
            // and ensure the match rectangle is scrolled into view.
            setCaretPosition(m[0]);
            Rectangle r = modelToView(m[0]);
            Rectangle r2 = modelToView(m[1]);
            if (r != null) {
                if (r2 != null) {
                    r = r.union(r2);
                }
                scrollRectToVisible(r);
            }
        } catch (BadLocationException e) {
            // Ignore.
        }
    }

    /**
     * Updates the "current/total" match counter label.
     */
    private void updateMatchLabel() {
        if (findMatchLabel == null) {
            return;
        }
        int total = findMatches.size();
        if (total == 0) {
            String q = (findField != null) ? findField.getText() : "";
            findMatchLabel.setText((q == null || q.isEmpty()) ? "0 matches" : "No matches");
        } else {
            findMatchLabel.setText((findCurrentIndex + 1) + "/" + total);
        }
    }
    // END_CHANGE: ISS-2025-0260
}