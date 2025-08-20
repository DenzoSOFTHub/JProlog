package it.denzosoft.jprolog.builtin.arithmetic;

public interface ArithmeticOperation {
    /**
     * Perform the arithmetic operation.
     * 
     * @param left The left operand
     * @param right The right operand
     * @return The result of the operation
     */
    double apply(double left, double right);
    
    /**
     * Get the symbol representing this operation.
     * 
     * @return The operation symbol
     */
    String getSymbol();
}
