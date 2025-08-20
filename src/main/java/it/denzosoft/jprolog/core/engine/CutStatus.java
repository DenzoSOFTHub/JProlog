package it.denzosoft.jprolog.core.engine;

public final class CutStatus {
    private final boolean cutOccurred;

    private CutStatus(boolean cutOccurred) {
        this.cutOccurred = cutOccurred;
    }

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
