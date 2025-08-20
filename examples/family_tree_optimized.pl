% =========================================================
%   Albero Genealogico - Versione Ottimizzata per JProlog
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

% Regole derivate ottimizzate

% Padri e madri definiti direttamente dai fatti
father(luca,gianni).
father(luca,anna).
father(gianni,piero).
father(gianni,paolo).
father(piero,leo).
father(paolo,nicola).

mother(marta,gianni).
mother(marta,anna).
mother(anna,piero).
mother(anna,paolo).
mother(sara,leo).
mother(giulia,nicola).

% spouse/2: relazione simmetrica del matrimonio
spouse(luca,marta).
spouse(marta,luca).
spouse(gianni,anna).
spouse(anna,gianni).
spouse(piero,sara).
spouse(sara,piero).
spouse(paolo,giulia).
spouse(giulia,paolo).
spouse(nicola,elisa).
spouse(elisa,nicola).

% child/2: inverso di parent
child(gianni,luca).
child(gianni,marta).
child(anna,luca).
child(anna,marta).
child(piero,gianni).
child(piero,anna).
child(paolo,gianni).
child(paolo,anna).
child(leo,piero).
child(leo,sara).
child(nicola,paolo).
child(nicola,giulia).

% sibling/2: fratelli (definiti esplicitamente)
sibling(gianni,anna).
sibling(anna,gianni).
sibling(piero,paolo).
sibling(paolo,piero).

% brother/sister
brother(gianni,anna).
brother(piero,paolo).
sister(anna,gianni).

% grandparent/grandchild
grandparent(luca,piero).
grandparent(luca,paolo).
grandparent(marta,piero).
grandparent(marta,paolo).
grandparent(gianni,leo).
grandparent(anna,leo).
grandparent(gianni,nicola).
grandparent(anna,nicola).

grandchild(piero,luca).
grandchild(piero,marta).
grandchild(paolo,luca).
grandchild(paolo,marta).
grandchild(leo,gianni).
grandchild(leo,anna).
grandchild(nicola,gianni).
grandchild(nicola,anna).

% uncle/aunt
uncle_aunt(gianni,nicola).
uncle_aunt(anna,nicola).

% ancestor/descendant (definiti esplicitamente per evitare loop)
ancestor(luca,gianni).
ancestor(luca,anna).
ancestor(luca,piero).
ancestor(luca,paolo).
ancestor(luca,leo).
ancestor(luca,nicola).

ancestor(marta,gianni).
ancestor(marta,anna).
ancestor(marta,piero).
ancestor(marta,paolo).
ancestor(marta,leo).
ancestor(marta,nicola).

ancestor(gianni,piero).
ancestor(gianni,paolo).
ancestor(gianni,leo).
ancestor(gianni,nicola).

ancestor(anna,piero).
ancestor(anna,paolo).
ancestor(anna,leo).
ancestor(anna,nicola).

ancestor(piero,leo).
ancestor(paolo,nicola).

descendant(gianni,luca).
descendant(anna,luca).
descendant(piero,luca).
descendant(paolo,luca).
descendant(leo,luca).
descendant(nicola,luca).

descendant(gianni,marta).
descendant(anna,marta).
descendant(piero,marta).
descendant(paolo,marta).
descendant(leo,marta).
descendant(nicola,marta).

descendant(piero,gianni).
descendant(paolo,gianni).
descendant(leo,gianni).
descendant(nicola,gianni).

descendant(piero,anna).
descendant(paolo,anna).
descendant(leo,anna).
descendant(nicola,anna).

descendant(leo,piero).
descendant(nicola,paolo).

% cousin: piero e paolo sono cugini di nicola (figli di fratelli)
cousin(piero,nicola).
cousin(nicola,piero).
cousin(paolo,nicola).
cousin(nicola,paolo).