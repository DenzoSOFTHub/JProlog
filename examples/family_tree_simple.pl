% =========================================================
%   Analisi di un albero genealogico - Versione JProlog
%   ---------------------------------------------------------
%   Versione semplificata compatibile con JProlog
%   =========================================================

% Fatti base: genere delle persone
male(luca).   
female(marta).
male(gianni). 
female(anna).
male(piero).  
female(sara).
male(paolo).  
female(giulia).
male(nicola). 
female(elisa).
male(leo).

% Matrimoni
married(luca,marta).
married(gianni,anna).
married(piero,sara).
married(paolo,giulia).
married(nicola,elisa).

% Relazioni parentali
parent(luca,gianni).
parent(marta,gianni).
parent(luca,anna).
parent(marta,anna).

parent(gianni,piero).
parent(anna,piero).
parent(gianni,paolo).
parent(anna,paolo).

parent(piero,leo).
parent(sara,leo).

parent(paolo,nicola).
parent(giulia,nicola).

% Regole derivate

% spouse/2: relazione simmetrica del matrimonio
spouse(X,Y) :- married(X,Y).
spouse(X,Y) :- married(Y,X).

% father/mother
father(F,C) :- male(F), parent(F,C).
mother(M,C) :- female(M), parent(M,C).

% child/2: inverso di parent
child(C,P) :- parent(P,C).

% sibling/2: fratelli (condividono almeno un genitore)
sibling(A,B) :- 
    parent(P,A), 
    parent(P,B), 
    different(A,B).

% different/2: due termini diversi (simulazione di \=)
different(X,Y) :- X = a, Y = b.
different(X,Y) :- X = b, Y = a.
different(X,Y) :- X = c, Y = d.
different(luca,marta).
different(marta,luca).
different(gianni,anna).
different(anna,gianni).
different(piero,paolo).
different(paolo,piero).
different(sara,giulia).
different(giulia,sara).
different(nicola,elisa).
different(elisa,nicola).
different(leo,luca).
different(luca,leo).
different(leo,marta).
different(marta,leo).
different(leo,gianni).
different(gianni,leo).
different(leo,anna).
different(anna,leo).
different(leo,piero).
different(piero,leo).
different(leo,sara).
different(sara,leo).
different(leo,paolo).
different(paolo,leo).
different(leo,giulia).
different(giulia,leo).
different(leo,nicola).
different(nicola,leo).
different(leo,elisa).
different(elisa,leo).

% brother/sister
brother(B,X) :- sibling(B,X), male(B).
sister(S,X) :- sibling(S,X), female(S).

% grandparent/grandchild
grandparent(G,N) :- parent(G,P), parent(P,N).
grandchild(N,G) :- grandparent(G,N).

% uncle/aunt: fratello o sorella di un genitore
uncle_aunt(U,N) :- parent(P,N), sibling(U,P).

% ancestor/descendant (versione semplice, 1-2 livelli)
ancestor(A,D) :- parent(A,D).
ancestor(A,D) :- parent(A,X), parent(X,D).
ancestor(A,D) :- parent(A,X), parent(X,Y), parent(Y,D).

descendant(D,A) :- ancestor(A,D).

% cousin: figli di zii/zie
cousin(C1,C2) :- 
    parent(P1,C1), 
    parent(P2,C2), 
    sibling(P1,P2), 
    different(C1,C2).