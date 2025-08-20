package it.denzosoft.jprolog.core.engine;

/**
 * Mutable wrapper for CutStatus to allow propagating cut occurrences up the call stack.
 * This is needed because cut semantics require modifying state during backtracking.
 */
public class MutableCutStatus {
    private boolean cutOccurred = false;
    
    /**
     * Check if cut has occurred.
     * 
     * @return true if cut occurred
     */
    public boolean isCutOccurred() {
        return cutOccurred;
    }
    
    /**
     * Mark that cut has occurred.
     */
    public void setCutOccurred() {
        this.cutOccurred = true;
    }
    
    /**
     * Reset the cut status.
     */
    public void reset() {
        this.cutOccurred = false;
    }
    
    /**
     * Create a new MutableCutStatus with the same state.
     * 
     * @return A copy of this cut status
     */
    public MutableCutStatus copy() {
        MutableCutStatus copy = new MutableCutStatus();
        copy.cutOccurred = this.cutOccurred;
        return copy;
    }
}