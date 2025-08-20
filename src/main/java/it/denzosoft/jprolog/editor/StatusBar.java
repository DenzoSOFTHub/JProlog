package it.denzosoft.jprolog.editor;

import javax.swing.*;
import javax.swing.border.BevelBorder;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * IDE status bar that shows current information
 * such as file status, cursor position, temporary messages, etc.
 */
public class StatusBar extends JPanel {
    
    // Componenti della barra di stato
    private JLabel messageLabel;
    private JLabel fileStatusLabel;
    private JLabel positionLabel;
    private JLabel encodingLabel;
    private JLabel timeLabel;
    private JProgressBar progressBar;
    
    // Timer per aggiornamento orario
    private Timer clockTimer;
    
    // Configuration
    private SimpleDateFormat timeFormat = new SimpleDateFormat("HH:mm:ss");
    
    public StatusBar() {
        initializeComponents();
        setupLayout();
        startClockTimer();
    }
    
    /**
     * Initializes status bar components.
     */
    private void initializeComponents() {
        setBorder(new BevelBorder(BevelBorder.LOWERED));
        
        // Label per messaggi generali
        messageLabel = new JLabel("Ready");
        messageLabel.setFont(messageLabel.getFont().deriveFont(Font.PLAIN, 11f));
        
        // Label per stato del file
        fileStatusLabel = new JLabel("No file");
        fileStatusLabel.setFont(fileStatusLabel.getFont().deriveFont(Font.PLAIN, 11f));
        fileStatusLabel.setHorizontalAlignment(SwingConstants.CENTER);
        
        // Label per posizione cursore
        positionLabel = new JLabel("Line: -, Col: -");
        positionLabel.setFont(positionLabel.getFont().deriveFont(Font.PLAIN, 11f));
        positionLabel.setHorizontalAlignment(SwingConstants.CENTER);
        
        // Label per encoding
        encodingLabel = new JLabel("UTF-8");
        encodingLabel.setFont(encodingLabel.getFont().deriveFont(Font.PLAIN, 11f));
        encodingLabel.setHorizontalAlignment(SwingConstants.CENTER);
        
        // Label per orario
        timeLabel = new JLabel();
        timeLabel.setFont(timeLabel.getFont().deriveFont(Font.PLAIN, 11f));
        timeLabel.setHorizontalAlignment(SwingConstants.CENTER);
        updateTime();
        
        // Progress bar (inizialmente nascosta)
        progressBar = new JProgressBar();
        progressBar.setVisible(false);
        progressBar.setPreferredSize(new Dimension(100, 16));
    }
    
    /**
     * Configura il layout della barra di stato.
     */
    private void setupLayout() {
        setLayout(new BorderLayout());
        setPreferredSize(new Dimension(0, 25));
        
        // Pannello principale con BoxLayout
        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.X_AXIS));
        mainPanel.setBorder(BorderFactory.createEmptyBorder(2, 5, 2, 5));
        
        // Messaggio principale (espandibile)
        mainPanel.add(messageLabel);
        mainPanel.add(Box.createHorizontalGlue());
        
        // Progress bar
        mainPanel.add(progressBar);
        mainPanel.add(Box.createHorizontalStrut(5));
        
        // Sezioni fisse
        addStatusSection(mainPanel, fileStatusLabel, 150);
        addSeparator(mainPanel);
        
        addStatusSection(mainPanel, positionLabel, 100);
        addSeparator(mainPanel);
        
        addStatusSection(mainPanel, encodingLabel, 50);
        addSeparator(mainPanel);
        
        addStatusSection(mainPanel, timeLabel, 70);
        
        add(mainPanel, BorderLayout.CENTER);
    }
    
    /**
     * Aggiunge una sezione con larghezza fissa.
     */
    private void addStatusSection(JPanel parent, JLabel label, int width) {
        label.setPreferredSize(new Dimension(width, 20));
        label.setMaximumSize(new Dimension(width, 20));
        label.setMinimumSize(new Dimension(width, 20));
        label.setBorder(BorderFactory.createLoweredBevelBorder());
        parent.add(label);
    }
    
    /**
     * Aggiunge un separatore verticale.
     */
    private void addSeparator(JPanel parent) {
        parent.add(Box.createHorizontalStrut(2));
        JSeparator separator = new JSeparator(SwingConstants.VERTICAL);
        separator.setPreferredSize(new Dimension(2, 20));
        separator.setMaximumSize(new Dimension(2, 20));
        parent.add(separator);
        parent.add(Box.createHorizontalStrut(2));
    }
    
    /**
     * Avvia il timer per l'aggiornamento dell'orario.
     */
    private void startClockTimer() {
        clockTimer = new Timer(1000, new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                updateTime();
            }
        });
        clockTimer.start();
    }
    
    /**
     * Aggiorna l'orario visualizzato.
     */
    private void updateTime() {
        timeLabel.setText(timeFormat.format(new Date()));
    }
    
    /**
     * Imposta il messaggio principale.
     */
    public void setMessage(String message) {
        SwingUtilities.invokeLater(() -> {
            messageLabel.setText(message);
            messageLabel.setToolTipText(message); // Tooltip per messaggi lunghi
        });
    }
    
    /**
     * Imposta un messaggio temporaneo che scompare dopo un timeout.
     */
    public void setTemporaryMessage(String message, int milliseconds) {
        setMessage(message);
        
        Timer timer = new Timer(milliseconds, e -> setMessage("Ready"));
        timer.setRepeats(false);
        timer.start();
    }
    
    /**
     * Imposta lo stato del file corrente.
     */
    public void setFileStatus(String filename, boolean modified, boolean readOnly) {
        SwingUtilities.invokeLater(() -> {
            StringBuilder status = new StringBuilder();
            
            if (filename != null) {
                // Mostra solo il nome del file, non il path completo
                String name = filename;
                if (name.length() > 20) {
                    name = "..." + name.substring(name.length() - 17);
                }
                status.append(name);
                
                if (modified) {
                    status.append(" *");
                }
                
                if (readOnly) {
                    status.append(" [RO]");
                }
                
                fileStatusLabel.setText(status.toString());
                fileStatusLabel.setToolTipText(filename + (modified ? " (modified)" : "") + 
                                               (readOnly ? " (read only)" : ""));
            } else {
                fileStatusLabel.setText("No file");
                fileStatusLabel.setToolTipText(null);
            }
        });
    }
    
    /**
     * Aggiorna la posizione del cursore.
     */
    public void setCaretPosition(int line, int column) {
        SwingUtilities.invokeLater(() -> {
            positionLabel.setText(String.format("Line: %d, Col: %d", line, column));
        });
    }
    
    /**
     * Imposta l'encoding del file.
     */
    public void setEncoding(String encoding) {
        SwingUtilities.invokeLater(() -> {
            encodingLabel.setText(encoding);
            encodingLabel.setToolTipText("Encoding: " + encoding);
        });
    }
    
    /**
     * Mostra/nasconde la progress bar.
     */
    public void setProgressVisible(boolean visible) {
        SwingUtilities.invokeLater(() -> {
            progressBar.setVisible(visible);
            revalidate();
            repaint();
        });
    }
    
    /**
     * Imposta il valore della progress bar.
     */
    public void setProgress(int value) {
        SwingUtilities.invokeLater(() -> {
            progressBar.setValue(value);
            if (!progressBar.isVisible()) {
                setProgressVisible(true);
            }
        });
    }
    
    /**
     * Imposta il range della progress bar.
     */
    public void setProgressRange(int min, int max) {
        SwingUtilities.invokeLater(() -> {
            progressBar.setMinimum(min);
            progressBar.setMaximum(max);
        });
    }
    
    /**
     * Sets the progress bar in indeterminate mode.
     */
    public void setProgressIndeterminate(boolean indeterminate) {
        SwingUtilities.invokeLater(() -> {
            progressBar.setIndeterminate(indeterminate);
            if (indeterminate && !progressBar.isVisible()) {
                setProgressVisible(true);
            }
        });
    }
    
    /**
     * Nasconde la progress bar e reimposta il messaggio.
     */
    public void clearProgress() {
        SwingUtilities.invokeLater(() -> {
            setProgressVisible(false);
            progressBar.setIndeterminate(false);
            progressBar.setValue(0);
        });
    }
    
    /**
     * Mostra un messaggio di errore con colore rosso.
     */
    public void setErrorMessage(String message) {
        SwingUtilities.invokeLater(() -> {
            messageLabel.setText("ERROR: " + message);
            messageLabel.setForeground(Color.RED);
            messageLabel.setToolTipText(message);
            
            // Ripristina il colore normale dopo 5 secondi
            Timer timer = new Timer(5000, e -> {
                messageLabel.setForeground(Color.BLACK);
                setMessage("Ready");
            });
            timer.setRepeats(false);
            timer.start();
        });
    }
    
    /**
     * Mostra un messaggio di successo con colore verde.
     */
    public void setSuccessMessage(String message) {
        SwingUtilities.invokeLater(() -> {
            messageLabel.setText(message);
            messageLabel.setForeground(new Color(0, 150, 0));
            messageLabel.setToolTipText(message);
            
            // Ripristina il colore normale dopo 3 secondi
            Timer timer = new Timer(3000, e -> {
                messageLabel.setForeground(Color.BLACK);
                setMessage("Ready");
            });
            timer.setRepeats(false);
            timer.start();
        });
    }
    
    /**
     * Mostra un messaggio di warning con colore arancione.
     */
    public void setWarningMessage(String message) {
        SwingUtilities.invokeLater(() -> {
            messageLabel.setText("WARNING: " + message);
            messageLabel.setForeground(new Color(255, 140, 0));
            messageLabel.setToolTipText(message);
            
            // Ripristina il colore normale dopo 4 secondi
            Timer timer = new Timer(4000, e -> {
                messageLabel.setForeground(Color.BLACK);
                setMessage("Ready");
            });
            timer.setRepeats(false);
            timer.start();
        });
    }
    
    /**
     * Aggiunge informazioni aggiuntive alla barra di stato.
     */
    public void addStatusInfo(String key, String value) {
        // Per estensioni future: potrebbe essere usato per mostrare
        // informazioni specifiche del progetto o del file
        setMessage(key + ": " + value);
    }
    
    /**
     * Ferma il timer dell'orologio quando il componente viene rimosso.
     */
    public void dispose() {
        if (clockTimer != null && clockTimer.isRunning()) {
            clockTimer.stop();
        }
    }
    
    /**
     * Ottiene il messaggio corrente.
     */
    public String getCurrentMessage() {
        return messageLabel.getText();
    }
    
    /**
     * Checks if the progress bar is visible.
     */
    public boolean isProgressVisible() {
        return progressBar.isVisible();
    }
}