# Demo CLI Interattiva con Soluzioni Multiple

## Come testare manualmente:

1. **Avvia la CLI:**
   ```bash
   java -cp target/classes it.denzosoft.jprolog.PrologCLI
   ```

2. **Test 1 - member/2 (3 soluzioni):**
   ```prolog
   ?- member(X, [a,b,c]).
   X = a ;     # Premi ';' + Invio per vedere la prossima
   X = b ;     # Premi ';' + Invio per vedere la prossima  
   X = c.      # Ultima soluzione
   ```

3. **Test 2 - Query con variabili (già caricati fatti parent):**
   ```prolog
   ?- parent(Who, bob).
   Who = tom ;    # Premi ';' + Invio
   Who = ann.     # Ultima soluzione
   ```

4. **Test 3 - Aggiungi fatti e testa:**
   ```prolog
   ?- assertz(likes(mary, food)).
   true.
   ?- assertz(likes(mary, wine)).
   true.
   ?- assertz(likes(john, wine)).
   true.
   ?- likes(mary, What).
   What = food ;  # Premi ';' + Invio
   What = wine.   # Ultima soluzione
   ```

5. **Test 4 - Fermarsi prima della fine:**
   ```prolog
   ?- member(X, [1,2,3,4,5]).
   X = 1 ;        # Premi solo Invio (non ';')
   .              # Si ferma qui senza mostrare le altre
   ```

## Comportamento atteso:
- ✅ Quando ci sono multiple soluzioni, mostra la prima seguita da " ;"
- ✅ Aspetta input dell'utente  
- ✅ Se utente preme ";", mostra la prossima soluzione
- ✅ Se utente preme Invio, termina con "."
- ✅ Alla fine di tutte le soluzioni, termina automaticamente con "."