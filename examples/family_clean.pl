% =========================================================
%   Albero Genealogico Famiglia Rossi - Versione Pulita
%   =========================================================

% Genere delle persone
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

% Relazioni parentali base
parent_of(luca,gianni).
parent_of(marta,gianni).
parent_of(luca,anna).
parent_of(marta,anna).
parent_of(gianni,piero).
parent_of(anna,piero).
parent_of(gianni,paolo).
parent_of(anna,paolo).
parent_of(piero,leo).
parent_of(sara,leo).
parent_of(paolo,nicola).
parent_of(giulia,nicola).

% Padri
father_of(luca,gianni).
father_of(luca,anna).
father_of(gianni,piero).
father_of(gianni,paolo).
father_of(piero,leo).
father_of(paolo,nicola).

% Madri
mother_of(marta,gianni).
mother_of(marta,anna).
mother_of(anna,piero).
mother_of(anna,paolo).
mother_of(sara,leo).
mother_of(giulia,nicola).

% Coniugi
spouse_of(luca,marta).
spouse_of(marta,luca).
spouse_of(gianni,anna).
spouse_of(anna,gianni).
spouse_of(piero,sara).
spouse_of(sara,piero).
spouse_of(paolo,giulia).
spouse_of(giulia,paolo).
spouse_of(nicola,elisa).
spouse_of(elisa,nicola).

% Figli
child_of(gianni,luca).
child_of(gianni,marta).
child_of(anna,luca).
child_of(anna,marta).
child_of(piero,gianni).
child_of(piero,anna).
child_of(paolo,gianni).
child_of(paolo,anna).
child_of(leo,piero).
child_of(leo,sara).
child_of(nicola,paolo).
child_of(nicola,giulia).

% Fratelli/Sorelle
sibling_of(gianni,anna).
sibling_of(anna,gianni).
sibling_of(piero,paolo).
sibling_of(paolo,piero).

brother_of(gianni,anna).
brother_of(piero,paolo).
sister_of(anna,gianni).

% Nonni/Nipoti
grandparent_of(luca,piero).
grandparent_of(luca,paolo).
grandparent_of(marta,piero).
grandparent_of(marta,paolo).
grandparent_of(gianni,leo).
grandparent_of(anna,leo).
grandparent_of(gianni,nicola).
grandparent_of(anna,nicola).

grandchild_of(piero,luca).
grandchild_of(piero,marta).
grandchild_of(paolo,luca).
grandchild_of(paolo,marta).
grandchild_of(leo,gianni).
grandchild_of(leo,anna).
grandchild_of(nicola,gianni).
grandchild_of(nicola,anna).

% Zii/Zie
uncle_aunt_of(gianni,nicola).
uncle_aunt_of(anna,nicola).

% Antenati (da generazione più vecchia a più giovane)
ancestor_of(luca,gianni).
ancestor_of(luca,anna).
ancestor_of(luca,piero).
ancestor_of(luca,paolo).
ancestor_of(luca,leo).
ancestor_of(luca,nicola).

ancestor_of(marta,gianni).
ancestor_of(marta,anna).
ancestor_of(marta,piero).
ancestor_of(marta,paolo).
ancestor_of(marta,leo).
ancestor_of(marta,nicola).

ancestor_of(gianni,piero).
ancestor_of(gianni,paolo).
ancestor_of(gianni,leo).
ancestor_of(gianni,nicola).

ancestor_of(anna,piero).
ancestor_of(anna,paolo).
ancestor_of(anna,leo).
ancestor_of(anna,nicola).

ancestor_of(piero,leo).
ancestor_of(paolo,nicola).

% Discendenti (da generazione più giovane a più vecchia)
descendant_of(gianni,luca).
descendant_of(anna,luca).
descendant_of(piero,luca).
descendant_of(paolo,luca).
descendant_of(leo,luca).
descendant_of(nicola,luca).

descendant_of(gianni,marta).
descendant_of(anna,marta).
descendant_of(piero,marta).
descendant_of(paolo,marta).
descendant_of(leo,marta).
descendant_of(nicola,marta).

descendant_of(piero,gianni).
descendant_of(paolo,gianni).
descendant_of(leo,gianni).
descendant_of(nicola,gianni).

descendant_of(piero,anna).
descendant_of(paolo,anna).
descendant_of(leo,anna).
descendant_of(nicola,anna).

descendant_of(leo,piero).
descendant_of(nicola,paolo).

% Cugini
cousin_of(piero,nicola).
cousin_of(nicola,piero).
cousin_of(paolo,nicola).
cousin_of(nicola,paolo).