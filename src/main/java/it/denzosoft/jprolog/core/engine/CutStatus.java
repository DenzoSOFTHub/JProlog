package it.denzosoft.jprolog.core.engine;

// START_CHANGE: ISS-2025-0054 - Make CutStatus mutable for proper cut propagation
public final class CutStatus {
    private boolean cutOccurred;

    private CutStatus(boolean cutOccurred) {
        this.cutOccurred = cutOccurred;
    }

    /**
     * Mark that cut has occurred. This allows cut in body goals
     * to propagate back to the clause selection loop.
     */
    public void setCutOccurred() {
        this.cutOccurred = true;
    }
    // END_CHANGE: ISS-2025-0054

    /**
     * Check if cut has occurred.
     * 
     * @return true if cut occurred
     */
    public boolean isCutOccurred() {
        return cutOccurred;
    }

    /**
     * Create a CutStatus indicating no cut has occurred.
     * 
     * @return A CutStatus with cutOccurred = false
     */
    public static CutStatus notOccurred() {
        return new CutStatus(false);
    }

    /**
     * Create a CutStatus indicating cut has occurred.
     * 
     * @return A CutStatus with cutOccurred = true
     */
    public static CutStatus occurred() {
        return new CutStatus(true);
    }
}
