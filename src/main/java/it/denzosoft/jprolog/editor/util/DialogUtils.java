package it.denzosoft.jprolog.editor.util;

import javax.swing.*;
import java.awt.*;

/**
 * Utility class for managing dialog windows in JProlog IDE.
 * Ensures all dialogs are properly centered relative to the main window.
 * 
 * This class addresses CR-2025-0001: All popup windows must open centered
 * relative to the main application window.
 * 
 * @author JProlog Team
 * @since 2025-08-19
 */
public class DialogUtils {
    
    /**
     * Shows a centered message dialog.
     * 
     * @param parent The parent component (typically the main JFrame)
     * @param message The message to display
     * @param title The dialog title
     * @param messageType The type of message (JOptionPane constants)
     */
    public static void showCenteredMessage(Component parent, Object message, 
                                          String title, int messageType) {
        JOptionPane.showMessageDialog(parent, message, title, messageType);
    }
    
    /**
     * Shows a centered message dialog with default INFO type.
     * 
     * @param parent The parent component
     * @param message The message to display
     * @param title The dialog title
     */
    public static void showCenteredMessage(Component parent, Object message, String title) {
        showCenteredMessage(parent, message, title, JOptionPane.INFORMATION_MESSAGE);
    }
    
    /**
     * Shows a centered error message dialog.
     * 
     * @param parent The parent component
     * @param message The error message to display
     * @param title The dialog title
     */
    public static void showError(Component parent, String message, String title) {
        showCenteredMessage(parent, message, title, JOptionPane.ERROR_MESSAGE);
    }
    
    /**
     * Shows a centered warning message dialog.
     * 
     * @param parent The parent component
     * @param message The warning message to display
     * @param title The dialog title
     */
    public static void showWarning(Component parent, String message, String title) {
        showCenteredMessage(parent, message, title, JOptionPane.WARNING_MESSAGE);
    }
    
    /**
     * Shows a centered input dialog.
     * 
     * @param parent The parent component
     * @param message The message to display
     * @param title The dialog title
     * @param messageType The type of message
     * @return The user input string, or null if cancelled
     */
    public static String showCenteredInput(Component parent, Object message, 
                                          String title, int messageType) {
        return JOptionPane.showInputDialog(parent, message, title, messageType);
    }
    
    /**
     * Shows a centered input dialog with default QUESTION type.
     * 
     * @param parent The parent component
     * @param message The message to display
     * @param title The dialog title
     * @return The user input string, or null if cancelled
     */
    public static String showCenteredInput(Component parent, Object message, String title) {
        return showCenteredInput(parent, message, title, JOptionPane.QUESTION_MESSAGE);
    }
    
    /**
     * Shows a centered confirmation dialog.
     * 
     * @param parent The parent component
     * @param message The message to display
     * @param title The dialog title
     * @param optionType The options to display (YES_NO, YES_NO_CANCEL, etc.)
     * @param messageType The type of message
     * @return The user's choice (JOptionPane.YES_OPTION, NO_OPTION, etc.)
     */
    public static int showCenteredConfirm(Component parent, Object message, 
                                         String title, int optionType, int messageType) {
        return JOptionPane.showConfirmDialog(parent, message, title, optionType, messageType);
    }
    
    /**
     * Shows a centered confirmation dialog with YES_NO_CANCEL options.
     * 
     * @param parent The parent component
     * @param message The message to display
     * @param title The dialog title
     * @return The user's choice
     */
    public static int showCenteredConfirm(Component parent, Object message, String title) {
        return showCenteredConfirm(parent, message, title, 
                                  JOptionPane.YES_NO_CANCEL_OPTION, 
                                  JOptionPane.QUESTION_MESSAGE);
    }
    
    /**
     * Shows a centered confirmation dialog with specified option type.
     * 
     * @param parent The parent component
     * @param message The message to display
     * @param title The dialog title
     * @param optionType The options to display
     * @return The user's choice
     */
    public static int showCenteredConfirm(Component parent, Object message, 
                                         String title, int optionType) {
        return showCenteredConfirm(parent, message, title, 
                                  optionType, JOptionPane.QUESTION_MESSAGE);
    }
    
    /**
     * Shows a centered YES/NO confirmation dialog.
     * 
     * @param parent The parent component
     * @param message The message to display
     * @param title The dialog title
     * @return true if YES was selected, false otherwise
     */
    public static boolean showYesNoConfirm(Component parent, Object message, String title) {
        int result = showCenteredConfirm(parent, message, title, 
                                        JOptionPane.YES_NO_OPTION, 
                                        JOptionPane.QUESTION_MESSAGE);
        return result == JOptionPane.YES_OPTION;
    }
    
    /**
     * Centers a dialog window relative to its parent.
     * This method can be used for custom JDialog instances.
     * 
     * @param dialog The dialog to center
     * @param parent The parent component (null for screen center)
     */
    public static void centerDialog(Window dialog, Component parent) {
        if (parent != null) {
            dialog.setLocationRelativeTo(parent);
        } else {
            // Center on screen if no parent
            Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
            Dimension dialogSize = dialog.getSize();
            int x = (screenSize.width - dialogSize.width) / 2;
            int y = (screenSize.height - dialogSize.height) / 2;
            dialog.setLocation(x, y);
        }
    }
    
    /**
     * Creates and shows a centered custom dialog.
     * 
     * @param parent The parent frame
     * @param title The dialog title
     * @param modal Whether the dialog should be modal
     * @return The created JDialog (already centered and visible)
     */
    public static JDialog createCenteredDialog(Frame parent, String title, boolean modal) {
        JDialog dialog = new JDialog(parent, title, modal);
        centerDialog(dialog, parent);
        return dialog;
    }
    
    /**
     * Shows a file chooser dialog centered on the parent.
     * 
     * @param parent The parent component
     * @param title The chooser title
     * @param approveButtonText The text for the approve button
     * @param fileSelectionMode JFileChooser mode (FILES_ONLY, DIRECTORIES_ONLY, etc.)
     * @return The selected file, or null if cancelled
     */
    public static java.io.File showCenteredFileChooser(Component parent, String title, 
                                                       String approveButtonText, 
                                                       int fileSelectionMode) {
        JFileChooser chooser = new JFileChooser();
        chooser.setDialogTitle(title);
        chooser.setFileSelectionMode(fileSelectionMode);
        
        // The JFileChooser will automatically center when parent is provided
        int result = chooser.showDialog(parent, approveButtonText);
        
        if (result == JFileChooser.APPROVE_OPTION) {
            return chooser.getSelectedFile();
        }
        return null;
    }
    
    /**
     * Shows a directory chooser dialog centered on the parent.
     * 
     * @param parent The parent component
     * @param title The chooser title
     * @return The selected directory, or null if cancelled
     */
    public static java.io.File showDirectoryChooser(Component parent, String title) {
        return showCenteredFileChooser(parent, title, "Select", 
                                      JFileChooser.DIRECTORIES_ONLY);
    }
}