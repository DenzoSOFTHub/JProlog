package it.denzosoft.jprolog.editor;

import javax.swing.*;
import javax.swing.text.*;
import java.awt.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Syntax highlighting manager for Prolog code.
 * Applies different colors and styles for comments, strings, variables, atoms, operators, etc.
 */
public class PrologSyntaxHighlighter {
    
    // Colors for syntax highlighting
    public static final Color COMMENT_COLOR = new Color(128, 128, 128);      // Gray for comments
    public static final Color STRING_COLOR = new Color(0, 128, 0);           // Green for strings
    public static final Color ATOM_COLOR = new Color(0, 0, 128);             // Dark blue for atoms
    public static final Color VARIABLE_COLOR = new Color(128, 0, 128);       // Purple for variables
    public static final Color KEYWORD_COLOR = new Color(0, 0, 255);          // Blue for keywords
    public static final Color OPERATOR_COLOR = new Color(255, 140, 0);       // Orange for operators
    public static final Color NUMBER_COLOR = new Color(255, 0, 0);           // Red for numbers
    public static final Color FUNCTOR_COLOR = new Color(0, 128, 128);        // Teal for functors
    public static final Color DIRECTIVE_COLOR = new Color(128, 64, 0);       // Brown for directives
    
    // Patterns for recognizing Prolog elements
    private static final Pattern COMMENT_PATTERN = Pattern.compile("%.*");
    private static final Pattern MULTILINE_COMMENT_PATTERN = Pattern.compile("/\\*.*?\\*/", Pattern.DOTALL);
    
    // Additional patterns for Prolog-style multiline comments
    // Some Prolog dialects also support /* comments without mandatory closing */
    private static final Pattern STRING_PATTERN = Pattern.compile("('[^']*'|\"[^\"]*\")");
    private static final Pattern ATOM_PATTERN = Pattern.compile("\\b[a-z][a-zA-Z0-9_]*\\b");
    private static final Pattern VARIABLE_PATTERN = Pattern.compile("\\b[A-Z_][a-zA-Z0-9_]*\\b");
    private static final Pattern NUMBER_PATTERN = Pattern.compile("\\b\\d+(\\.\\d+)?\\b");
    private static final Pattern FUNCTOR_PATTERN = Pattern.compile("\\b[a-z][a-zA-Z0-9_]*(?=\\s*\\()");
    private static final Pattern DIRECTIVE_PATTERN = Pattern.compile(":-");
    
    // Pattern for Prolog operators
    private static final Pattern OPERATOR_PATTERN = Pattern.compile(
        "(:-|-->|\\\\\\+|=\\.\\.|\\\\=|/\\\\|@<|@=<|@>|@>=|=:=|=\\\\=|==|\\\\==|=<|>=|\\\\=|" +
        "is|\\+|-|\\*|/|\\^|mod|rem|abs|sin|cos|tan|exp|log|sqrt|=|\\.|,|;|\\||!)"
    );
    
    // Common Prolog keywords
    private static final Pattern KEYWORD_PATTERN = Pattern.compile(
        "\\b(true|false|fail|cut|call|findall|bagof|setof|forall|once|repeat|" +
        "assert|asserta|assertz|retract|retractall|abolish|consult|reconsult|" +
        "listing|trace|notrace|spy|nospy|halt|abort|break|statistics|" +
        "current_predicate|functor|arg|univ|var|nonvar|atom|number|integer|" +
        "float|atomic|compound|is_list|length|member|append|reverse|sort|" +
        "keysort|write|writeln|writeq|write_canonical|read|get|put|get_char|" +
        "put_char|open|close|see|tell|seen|told|nl|tab|format|catch|throw)\\b"
    );
    
    private JTextPane textPane;
    private StyledDocument document;
    
    // Per tenere traccia delle aree commentate
    private java.util.List<CommentRange> commentRanges = new java.util.ArrayList<>();
    
    // Classe per rappresentare i range dei commenti
    private static class CommentRange {
        final int start;
        final int end;
        
        CommentRange(int start, int end) {
            this.start = start;
            this.end = end;
        }
        
        boolean contains(int position) {
            return position >= start && position < end;
        }
        
        boolean overlaps(int rangeStart, int rangeEnd) {
            return rangeStart < end && rangeEnd > start;
        }
    }
    
    // Stili per il syntax highlighting
    private Style defaultStyle;
    private Style commentStyle;
    private Style stringStyle;
    private Style atomStyle;
    private Style variableStyle;
    private Style keywordStyle;
    private Style operatorStyle;
    private Style numberStyle;
    private Style functorStyle;
    private Style directiveStyle;
    
    public PrologSyntaxHighlighter(JTextPane textPane) {
        this.textPane = textPane;
        this.document = textPane.getStyledDocument();
        initializeStyles();
    }
    
    /**
     * Initializes all styles for syntax highlighting.
     */
    private void initializeStyles() {
        // Stile di default
        defaultStyle = document.addStyle("default", null);
        StyleConstants.setForeground(defaultStyle, Color.BLACK);
        StyleConstants.setFontFamily(defaultStyle, "Consolas");
        StyleConstants.setFontSize(defaultStyle, 14);
        
        // Stile commenti
        commentStyle = document.addStyle("comment", defaultStyle);
        StyleConstants.setForeground(commentStyle, COMMENT_COLOR);
        StyleConstants.setItalic(commentStyle, true);
        
        // Stile stringhe
        stringStyle = document.addStyle("string", defaultStyle);
        StyleConstants.setForeground(stringStyle, STRING_COLOR);
        
        // Stile atomi
        atomStyle = document.addStyle("atom", defaultStyle);
        StyleConstants.setForeground(atomStyle, ATOM_COLOR);
        
        // Stile variabili
        variableStyle = document.addStyle("variable", defaultStyle);
        StyleConstants.setForeground(variableStyle, VARIABLE_COLOR);
        StyleConstants.setBold(variableStyle, true);
        
        // Stile keywords
        keywordStyle = document.addStyle("keyword", defaultStyle);
        StyleConstants.setForeground(keywordStyle, KEYWORD_COLOR);
        StyleConstants.setBold(keywordStyle, true);
        
        // Stile operatori
        operatorStyle = document.addStyle("operator", defaultStyle);
        StyleConstants.setForeground(operatorStyle, OPERATOR_COLOR);
        StyleConstants.setBold(operatorStyle, true);
        
        // Stile numeri
        numberStyle = document.addStyle("number", defaultStyle);
        StyleConstants.setForeground(numberStyle, NUMBER_COLOR);
        
        // Stile funtori
        functorStyle = document.addStyle("functor", defaultStyle);
        StyleConstants.setForeground(functorStyle, FUNCTOR_COLOR);
        StyleConstants.setBold(functorStyle, true);
        
        // Stile direttive
        directiveStyle = document.addStyle("directive", defaultStyle);
        StyleConstants.setForeground(directiveStyle, DIRECTIVE_COLOR);
        StyleConstants.setBold(directiveStyle, true);
    }
    
    /**
     * Applica il syntax highlighting all'intero documento.
     */
    public void highlightSyntax() {
        try {
            String text = document.getText(0, document.getLength());
            
            // Reset all styles
            document.setCharacterAttributes(0, document.getLength(), defaultStyle, true);
            
            // First identify and store all commented areas
            identifyCommentRanges(text);
            
            // Apply highlighting by category (order matters!)
            // I commenti vengono applicati per primi e proteggono le aree interne
            highlightComments(text);
            highlightStrings(text);
            highlightDirectives(text);
            highlightKeywords(text);
            highlightFunctors(text);
            highlightOperators(text);
            highlightNumbers(text);
            highlightVariables(text);
            highlightAtoms(text);
            
        } catch (BadLocationException e) {
            // Ignora errori di highlighting
        }
    }
    
    /**
     * Applica il syntax highlighting a una riga specifica.
     */
    public void highlightLine(int lineNumber) {
        try {
            Element root = document.getDefaultRootElement();
            if (lineNumber >= root.getElementCount()) {
                return;
            }
            
            Element line = root.getElement(lineNumber);
            int start = line.getStartOffset();
            int end = line.getEndOffset();
            int length = end - start;
            
            String lineText = document.getText(start, length);
            
            // Reset line style
            document.setCharacterAttributes(start, length, defaultStyle, true);
            
            // Re-identify comments for this line
            String fullText = document.getText(0, document.getLength());
            identifyCommentRanges(fullText);
            
            // Apply highlighting to the line
            highlightCommentsInRange(lineText, start);
            highlightStringsInRange(lineText, start);
            highlightDirectivesInRange(lineText, start);
            highlightKeywordsInRange(lineText, start);
            highlightFunctorsInRange(lineText, start);
            highlightOperatorsInRange(lineText, start);
            highlightNumbersInRange(lineText, start);
            highlightVariablesInRange(lineText, start);
            highlightAtomsInRange(lineText, start);
            
        } catch (BadLocationException e) {
            // Ignora errori di highlighting
        }
    }
    
    /**
     * Identifies and stores all commented areas in the text.
     */
    private void identifyCommentRanges(String text) {
        commentRanges.clear();
        
        // Commenti single-line (% fino a fine riga)
        Matcher matcher = COMMENT_PATTERN.matcher(text);
        while (matcher.find()) {
            commentRanges.add(new CommentRange(matcher.start(), matcher.end()));
        }
        
        // Commenti multi-line (/* ... */)
        matcher = MULTILINE_COMMENT_PATTERN.matcher(text);
        while (matcher.find()) {
            commentRanges.add(new CommentRange(matcher.start(), matcher.end()));
        }
        
        // Handle unclosed multiline comments (/* without */)
        // Look for /* patterns that don't have corresponding */ in the same line
        identifyUnclosedMultilineComments(text);
    }
    
    /**
     * Identifica commenti multilinea non chiusi o malformati.
     */
    private void identifyUnclosedMultilineComments(String text) {
        int pos = 0;
        while ((pos = text.indexOf("/*", pos)) != -1) {
            // Check if this /* is already covered by a complete comment
            boolean alreadyCovered = false;
            for (CommentRange range : commentRanges) {
                if (range.contains(pos)) {
                    alreadyCovered = true;
                    break;
                }
            }
            
            if (!alreadyCovered) {
                // Look for */ closure after this position
                int closePos = text.indexOf("*/", pos + 2);
                if (closePos == -1) {
                    // Unclosed comment - extends to end of file
                    commentRanges.add(new CommentRange(pos, text.length()));
                } else {
                    // This case should already be handled by the normal pattern,
                    // ma aggiungiamo per sicurezza
                    commentRanges.add(new CommentRange(pos, closePos + 2));
                }
            }
            pos += 2;
        }
    }
    
    /**
     * Checks if a position is inside a comment.
     */
    private boolean isInsideComment(int position) {
        for (CommentRange range : commentRanges) {
            if (range.contains(position)) {
                return true;
            }
        }
        return false;
    }
    
    /**
     * Checks if a range is completely inside a comment.
     */
    private boolean isRangeInsideComment(int start, int end) {
        for (CommentRange range : commentRanges) {
            if (range.overlaps(start, end)) {
                return true;
            }
        }
        return false;
    }
    
    // Metodi per highlighting di categorie specifiche
    
    private void highlightComments(String text) {
        // Highlight single-line comments (% until end of line)
        Matcher matcher = COMMENT_PATTERN.matcher(text);
        while (matcher.find()) {
            document.setCharacterAttributes(matcher.start(), matcher.end() - matcher.start(), commentStyle, false);
        }
        
        // Highlight multiline comments (/* ... */)
        matcher = MULTILINE_COMMENT_PATTERN.matcher(text);
        while (matcher.find()) {
            document.setCharacterAttributes(matcher.start(), matcher.end() - matcher.start(), commentStyle, false);
        }
    }
    
    private void highlightStrings(String text) {
        Matcher matcher = STRING_PATTERN.matcher(text);
        while (matcher.find()) {
            if (!isRangeInsideComment(matcher.start(), matcher.end())) {
                document.setCharacterAttributes(matcher.start(), matcher.end() - matcher.start(), stringStyle, false);
            }
        }
    }
    
    private void highlightKeywords(String text) {
        Matcher matcher = KEYWORD_PATTERN.matcher(text);
        while (matcher.find()) {
            if (!isRangeInsideComment(matcher.start(), matcher.end())) {
                document.setCharacterAttributes(matcher.start(), matcher.end() - matcher.start(), keywordStyle, false);
            }
        }
    }
    
    private void highlightFunctors(String text) {
        Matcher matcher = FUNCTOR_PATTERN.matcher(text);
        while (matcher.find()) {
            if (!isRangeInsideComment(matcher.start(), matcher.end())) {
                document.setCharacterAttributes(matcher.start(), matcher.end() - matcher.start(), functorStyle, false);
            }
        }
    }
    
    private void highlightOperators(String text) {
        Matcher matcher = OPERATOR_PATTERN.matcher(text);
        while (matcher.find()) {
            if (!isRangeInsideComment(matcher.start(), matcher.end())) {
                document.setCharacterAttributes(matcher.start(), matcher.end() - matcher.start(), operatorStyle, false);
            }
        }
    }
    
    private void highlightNumbers(String text) {
        Matcher matcher = NUMBER_PATTERN.matcher(text);
        while (matcher.find()) {
            if (!isRangeInsideComment(matcher.start(), matcher.end())) {
                document.setCharacterAttributes(matcher.start(), matcher.end() - matcher.start(), numberStyle, false);
            }
        }
    }
    
    private void highlightVariables(String text) {
        Matcher matcher = VARIABLE_PATTERN.matcher(text);
        while (matcher.find()) {
            if (!isRangeInsideComment(matcher.start(), matcher.end())) {
                // Check that it's not already highlighted (e.g. inside strings)
                AttributeSet attrs = document.getCharacterElement(matcher.start()).getAttributes();
                if (attrs.containsAttribute(StyleConstants.Foreground, Color.BLACK) ||
                    attrs.containsAttribute(StyleConstants.Foreground, ATOM_COLOR)) {
                    document.setCharacterAttributes(matcher.start(), matcher.end() - matcher.start(), variableStyle, false);
                }
            }
        }
    }
    
    private void highlightAtoms(String text) {
        Matcher matcher = ATOM_PATTERN.matcher(text);
        while (matcher.find()) {
            if (!isRangeInsideComment(matcher.start(), matcher.end())) {
                // Check that it's not already highlighted (e.g. keyword, functor)
                AttributeSet attrs = document.getCharacterElement(matcher.start()).getAttributes();
                if (attrs.containsAttribute(StyleConstants.Foreground, Color.BLACK)) {
                    document.setCharacterAttributes(matcher.start(), matcher.end() - matcher.start(), atomStyle, false);
                }
            }
        }
    }
    
    private void highlightDirectives(String text) {
        Matcher matcher = DIRECTIVE_PATTERN.matcher(text);
        while (matcher.find()) {
            if (!isRangeInsideComment(matcher.start(), matcher.end())) {
                document.setCharacterAttributes(matcher.start(), matcher.end() - matcher.start(), directiveStyle, false);
            }
        }
    }
    
    // Metodi per highlighting di singole righe
    
    private void highlightCommentsInRange(String text, int offset) {
        // Highlight single-line comments in range
        Matcher matcher = COMMENT_PATTERN.matcher(text);
        while (matcher.find()) {
            document.setCharacterAttributes(offset + matcher.start(), matcher.end() - matcher.start(), commentStyle, false);
        }
        
        // Highlight multiline comments in range
        matcher = MULTILINE_COMMENT_PATTERN.matcher(text);
        while (matcher.find()) {
            document.setCharacterAttributes(offset + matcher.start(), matcher.end() - matcher.start(), commentStyle, false);
        }
    }
    
    private void highlightStringsInRange(String text, int offset) {
        Matcher matcher = STRING_PATTERN.matcher(text);
        while (matcher.find()) {
            int globalStart = offset + matcher.start();
            int globalEnd = offset + matcher.end();
            if (!isRangeInsideComment(globalStart, globalEnd)) {
                document.setCharacterAttributes(globalStart, matcher.end() - matcher.start(), stringStyle, false);
            }
        }
    }
    
    private void highlightKeywordsInRange(String text, int offset) {
        Matcher matcher = KEYWORD_PATTERN.matcher(text);
        while (matcher.find()) {
            int globalStart = offset + matcher.start();
            int globalEnd = offset + matcher.end();
            if (!isRangeInsideComment(globalStart, globalEnd)) {
                document.setCharacterAttributes(globalStart, matcher.end() - matcher.start(), keywordStyle, false);
            }
        }
    }
    
    private void highlightFunctorsInRange(String text, int offset) {
        Matcher matcher = FUNCTOR_PATTERN.matcher(text);
        while (matcher.find()) {
            int globalStart = offset + matcher.start();
            int globalEnd = offset + matcher.end();
            if (!isRangeInsideComment(globalStart, globalEnd)) {
                document.setCharacterAttributes(globalStart, matcher.end() - matcher.start(), functorStyle, false);
            }
        }
    }
    
    private void highlightOperatorsInRange(String text, int offset) {
        Matcher matcher = OPERATOR_PATTERN.matcher(text);
        while (matcher.find()) {
            int globalStart = offset + matcher.start();
            int globalEnd = offset + matcher.end();
            if (!isRangeInsideComment(globalStart, globalEnd)) {
                document.setCharacterAttributes(globalStart, matcher.end() - matcher.start(), operatorStyle, false);
            }
        }
    }
    
    private void highlightNumbersInRange(String text, int offset) {
        Matcher matcher = NUMBER_PATTERN.matcher(text);
        while (matcher.find()) {
            int globalStart = offset + matcher.start();
            int globalEnd = offset + matcher.end();
            if (!isRangeInsideComment(globalStart, globalEnd)) {
                document.setCharacterAttributes(globalStart, matcher.end() - matcher.start(), numberStyle, false);
            }
        }
    }
    
    private void highlightVariablesInRange(String text, int offset) {
        Matcher matcher = VARIABLE_PATTERN.matcher(text);
        while (matcher.find()) {
            int globalStart = offset + matcher.start();
            int globalEnd = offset + matcher.end();
            if (!isRangeInsideComment(globalStart, globalEnd)) {
                AttributeSet attrs = document.getCharacterElement(globalStart).getAttributes();
                if (attrs.containsAttribute(StyleConstants.Foreground, Color.BLACK) ||
                    attrs.containsAttribute(StyleConstants.Foreground, ATOM_COLOR)) {
                    document.setCharacterAttributes(globalStart, matcher.end() - matcher.start(), variableStyle, false);
                }
            }
        }
    }
    
    private void highlightAtomsInRange(String text, int offset) {
        Matcher matcher = ATOM_PATTERN.matcher(text);
        while (matcher.find()) {
            int globalStart = offset + matcher.start();
            int globalEnd = offset + matcher.end();
            if (!isRangeInsideComment(globalStart, globalEnd)) {
                AttributeSet attrs = document.getCharacterElement(globalStart).getAttributes();
                if (attrs.containsAttribute(StyleConstants.Foreground, Color.BLACK)) {
                    document.setCharacterAttributes(globalStart, matcher.end() - matcher.start(), atomStyle, false);
                }
            }
        }
    }
    
    private void highlightDirectivesInRange(String text, int offset) {
        Matcher matcher = DIRECTIVE_PATTERN.matcher(text);
        while (matcher.find()) {
            int globalStart = offset + matcher.start();
            int globalEnd = offset + matcher.end();
            if (!isRangeInsideComment(globalStart, globalEnd)) {
                document.setCharacterAttributes(globalStart, matcher.end() - matcher.start(), directiveStyle, false);
            }
        }
    }
    
    /**
     * Attiva/disattiva il syntax highlighting.
     */
    public void setEnabled(boolean enabled) {
        if (!enabled) {
            // Remove all highlighting
            try {
                document.setCharacterAttributes(0, document.getLength(), defaultStyle, true);
            } catch (Exception e) {
                // Ignora
            }
        } else {
            highlightSyntax();
        }
    }
    
    /**
     * Ottiene il colore per un tipo di token.
     */
    public static Color getColorForTokenType(String tokenType) {
        switch (tokenType) {
            case "comment": return COMMENT_COLOR;
            case "string": return STRING_COLOR;
            case "atom": return ATOM_COLOR;
            case "variable": return VARIABLE_COLOR;
            case "keyword": return KEYWORD_COLOR;
            case "operator": return OPERATOR_COLOR;
            case "number": return NUMBER_COLOR;
            case "functor": return FUNCTOR_COLOR;
            case "directive": return DIRECTIVE_COLOR;
            default: return Color.BLACK;
        }
    }
}