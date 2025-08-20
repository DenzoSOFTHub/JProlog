package it.denzosoft.jprolog.builtin.io;

import it.denzosoft.jprolog.core.terms.Atom;
import it.denzosoft.jprolog.core.terms.Term;

import java.io.*;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Manages I/O streams for ISO Prolog compliance.
 * Handles standard streams and file-based streams.
 */
public class StreamManager {
    private static final Map<String, InputStream> INPUT_STREAMS = new HashMap<>();
    private static final Map<String, OutputStream> OUTPUT_STREAMS = new HashMap<>();
    private static final AtomicInteger STREAM_COUNTER = new AtomicInteger(1000);
    
    private static String currentInputStream = "user_input";
    private static String currentOutputStream = "user_output";
    
    static {
        // Initialize standard streams
        INPUT_STREAMS.put("user_input", System.in);
        OUTPUT_STREAMS.put("user_output", System.out);
        OUTPUT_STREAMS.put("user_error", System.err);
    }
    
    /**
     * Open a file stream for reading or writing.
     */
    public static String openStream(String filename, String mode) throws IOException {
        String streamAlias = "stream_" + STREAM_COUNTER.incrementAndGet();
        
        switch (mode.toLowerCase()) {
            case "read":
                FileInputStream fis = new FileInputStream(filename);
                INPUT_STREAMS.put(streamAlias, fis);
                break;
            case "write":
                FileOutputStream fos = new FileOutputStream(filename);
                OUTPUT_STREAMS.put(streamAlias, fos);
                break;
            case "append":
                FileOutputStream aos = new FileOutputStream(filename, true);
                OUTPUT_STREAMS.put(streamAlias, aos);
                break;
            default:
                throw new IllegalArgumentException("Invalid stream mode: " + mode);
        }
        
        return streamAlias;
    }
    
    /**
     * Close a stream by alias.
     */
    public static boolean closeStream(String streamAlias) {
        try {
            InputStream is = INPUT_STREAMS.remove(streamAlias);
            if (is != null && is != System.in) {
                is.close();
                return true;
            }
            
            OutputStream os = OUTPUT_STREAMS.remove(streamAlias);
            if (os != null && os != System.out && os != System.err) {
                os.close();
                return true;
            }
            
            return false;
        } catch (IOException e) {
            return false;
        }
    }
    
    /**
     * Get input stream by alias.
     */
    public static InputStream getInputStream(String streamAlias) {
        return INPUT_STREAMS.get(streamAlias);
    }
    
    /**
     * Get output stream by alias.
     */
    public static OutputStream getOutputStream(String streamAlias) {
        return OUTPUT_STREAMS.get(streamAlias);
    }
    
    /**
     * Set current input stream.
     */
    public static void setCurrentInput(String streamAlias) {
        if (INPUT_STREAMS.containsKey(streamAlias)) {
            currentInputStream = streamAlias;
        }
    }
    
    /**
     * Set current output stream.
     */
    public static void setCurrentOutput(String streamAlias) {
        if (OUTPUT_STREAMS.containsKey(streamAlias)) {
            currentOutputStream = streamAlias;
        }
    }
    
    /**
     * Get current input stream alias.
     */
    public static String getCurrentInput() {
        return currentInputStream;
    }
    
    /**
     * Get current output stream alias.
     */
    public static String getCurrentOutput() {
        return currentOutputStream;
    }
    
    /**
     * Check if stream exists.
     */
    public static boolean hasStream(String streamAlias) {
        return INPUT_STREAMS.containsKey(streamAlias) || OUTPUT_STREAMS.containsKey(streamAlias);
    }
}