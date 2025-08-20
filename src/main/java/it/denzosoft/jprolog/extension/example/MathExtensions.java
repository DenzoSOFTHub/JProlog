package it.denzosoft.jprolog.extension.example;

import it.denzosoft.jprolog.core.engine.ArithmeticEvaluator;


public class MathExtensions {
    
    /**
     * Registers additional custom mathematical functions.
     * This method can be called at application startup
     * to extend Prolog interpreter capabilities.
     */
    public static void registerCustomFunctions() {
        // Funzione potenza (es. pow(2, 3) = 8)
        ArithmeticEvaluator.registerBinaryOperation("pow", Math::pow);
        
        // Funzione logaritmo in base 10
        ArithmeticEvaluator.registerUnaryFunction("log10", Math::log10);
        
        // Funzione valore massimo tra due numeri
        ArithmeticEvaluator.registerBinaryOperation("max", Math::max);
        
        // Funzione valore minimo tra due numeri
        ArithmeticEvaluator.registerBinaryOperation("min", Math::min);
        
        // Funzione fattoriale (implementazione ricorsiva)
        ArithmeticEvaluator.registerUnaryFunction("fact", x -> {
            if (x < 0) return Double.NaN;
            int n = (int) Math.round(x); // Fixed: properly convert double to int
            double result = 1;
            for (int i = 2; i <= n; i++) {
                result *= i;
            }
            return result;
        });
        
        // Funzione di conversione da gradi a radianti
        ArithmeticEvaluator.registerUnaryFunction("degToRad", Math::toRadians);
        
        // Funzione di conversione da radianti a gradi
        ArithmeticEvaluator.registerUnaryFunction("radToDeg", Math::toDegrees);
    }
    
    /**
     * Esempio d'uso dell'estensione matematica.
     * Questo metodo mostra come utilizzare le funzioni registrate.
     */
    public static void demonstrateUsage() {
        System.out.println("Esempio di utilizzo delle estensioni matematiche:");
        System.out.println("================================================");
        
        // Nota: Per testare realmente queste funzioni,
        // sarebbe necessario integrarle con il parser e il motore Prolog
        System.out.println("Funzioni registrate:");
        System.out.println("- pow(X, Y): Eleva X alla potenza Y");
        System.out.println("- log10(X): Logaritmo in base 10 di X");
        System.out.println("- max(X, Y): Valore massimo tra X e Y");
        System.out.println("- min(X, Y): Valore minimo tra X e Y");
        System.out.println("- fact(X): Fattoriale di X (per interi)");
        System.out.println("- degToRad(X): Converte X da gradi a radianti");
        System.out.println("- radToDeg(X): Converte X da radianti a gradi");
        
        System.out.println("\nEsempi di query Prolog che potrebbero essere eseguite:");
        System.out.println("?- X is pow(2, 3).              % Risultato: X = 8.0");
        System.out.println("?- X is log10(100).             % Risultato: X = 2.0");
        System.out.println("?- X is max(5, 12).             % Risultato: X = 12.0");
        System.out.println("?- X is fact(5).                % Risultato: X = 120.0");
        System.out.println("?- X is degToRad(180).          % Risultato: X = 3.14159...");
    }
}
