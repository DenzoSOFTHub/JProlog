#!/bin/bash

echo "=== DEMO GESTIONE ';' PER SOLUZIONI MULTIPLE ==="
echo

echo "Avviando PrologCLI per demo interattiva..."
echo "Query da provare (una alla volta):"
echo
echo "1. likes(mary, X).      # Dovrebbe mostrare X = wine ; poi X = food"
echo "2. color(X).           # Dovrebbe mostrare X = red ; poi X = green ; poi X = blue" 
echo "3. member(X,[a,b,c]).  # Dovrebbe mostrare X = a ; poi X = b ; poi X = c"
echo "4. likes(Who, wine).   # Dovrebbe mostrare Who = mary ; poi Who = john"
echo
echo "Per ogni query:"
echo "  - Vedrai la prima soluzione seguita da ' ;'"
echo "  - Premi ';' + Invio per vedere la prossima"
echo "  - Premi solo Invio per fermarti prima della fine"
echo
echo "Digita :quit per uscire dalla CLI"
echo

java -cp target/classes it.denzosoft.jprolog.PrologCLI