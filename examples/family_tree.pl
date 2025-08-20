/* =========================================================
   Analisi di un albero genealogico  SWI-Prolog
   ---------------------------------------------------------
   - Definizione di persone, genere, relazioni parentali e matrimoniali
   - Predicati derivati per analizzare parentele comuni
   - Esempio di base (Famiglia Rossi) in fondo
   ========================================================= */

:- module(albero, [
    % Fatti da fornire/estendere:
    male/1, female/1, parent/2, married/2,

    % Predicati derivati principali:
    spouse/2, father/2, mother/2, child/2, sibling/2, brother/2, sister/2,
    half_sibling/2, grandparent/2, grandchild/2, uncle_aunt/2, cousin/2,
    ancestor/2, descendant/2, in_law/2, common_ancestor/3,
    generation_gap/3, consanguinity_degree/3, relationship_path/3
]).

:- use_module(library(lists)).
:- use_module(library(ugraphs)).   % per il path di relazione generale

/* ----------------------------
   Regole di coerenza/sintassi
   ---------------------------- */

% spouse/2: chiusura simmetrica del matrimonio
spouse(X,Y) :- married(X,Y).
spouse(X,Y) :- married(Y,X).

% father/mother
father(F,C) :- male(F), parent(F,C).
mother(M,C) :- female(M), parent(M,C).

% child/2: inverso di parent
child(C,P) :- parent(P,C).

/* ----------------------------
   Fratelli / Sorelle
   ---------------------------- */

% sibling/2: condividono almeno un genitore, persone distinte
sibling(A,B) :-
    A \= B,
    parent(P,A),
    parent(P,B).

% Escludi falsi positivi considerando due genitori uguali per "fratelli pieni"
full_siblings(A,B) :-
    A \= B,
    parent(P1,A), parent(P1,B),
    parent(P2,A), parent(P2,B),
    P1 \= P2.

% half_sibling/2: condividono esattamente un genitore
half_sibling(A,B) :-
    sibling(A,B),
    \+ full_siblings(A,B).

brother(B,X) :- sibling(B,X), male(B).
sister(S,X)  :- sibling(S,X), female(S).

/* ----------------------------
   Nonni / Nipoti, Zii / Cugini
   ---------------------------- */

grandparent(G,N) :- parent(G,P), parent(P,N).
grandchild(N,G)  :- grandparent(G,N).

% zio/zia: fratello/sorella di un genitore (anche acquisito tramite spouse)
uncle_aunt(U,N) :-
    parent(P,N),
    sibling(U,P).
uncle_aunt(U,N) :-
    parent(P,N),
    spouse(P,S), sibling(U,S).  % zio acquisito

% cugini: figli di zii/zie (escludendo fratelli)
cousin(C1,C2) :-
    parent(P1,C1),
    parent(P2,C2),
    sibling(P1,P2),
    C1 \= C2,
    \+ sibling(C1,C2).

/* ----------------------------
   Antenati / Discendenti
   ---------------------------- */

ancestor(A,D) :- parent(A,D).
ancestor(A,D) :- parent(A,X), ancestor(A,X).

descendant(D,A) :- ancestor(A,D).

/* ----------------------------
   Parenti acquisiti (in-law)
   Esempi: suocero/suocera, cognato/cognata, genero/nuora
   ---------------------------- */

% in_law/2 vero se X è parente acquisito di Y in uno dei modi comuni
in_law(X,Y) :-
    spouse(Y,SY), parent(X,SY) ;      % suocero/a di Y
    spouse(X,SX), parent(SX,Y) ;      % Y è figlio del coniuge di X (patrigno/matrigna)
    spouse(Y,SY), sibling(X,SY) ;     % cognato/a di Y
    spouse(X,SX), sibling(SX,Y) ;     % cognato/a di X
    spouse(X,SX), child(SY,X), spouse(SY,Y). % genero/nuora rispetto a X

/* ----------------------------
   Antenato comune e gap generazionale
   ---------------------------- */

% common_ancestor(A,B,CA) - antenato comune (può produrre più soluzioni)
common_ancestor(A,B,CA) :-
    ancestor(CA,A),
    ancestor(CA,B).

% livello_antenato(CA,X,Depth): profondità (0 se X=CA)
livello_antenato(X,X,0).
livello_antenato(CA,X,D) :-
    parent(P,X),
    livello_antenato(CA,P,D1),
    D is D1 + 1.

% generation_gap(A,B,Gap): numero di "salti" di generazione:
% positivo se A è ascendente di B, negativo se discendente, zero se stessa persona
generation_gap(A,B,0) :- A == B, !.
generation_gap(A,B,Gap) :-
    ancestor(A,B), !, livello_antenato(A,B,Gap).
generation_gap(A,B,GapNeg) :-
    ancestor(B,A), !, livello_antenato(B,A,Gap), GapNeg is -Gap.
generation_gap(_,_,undefined).  % non direttamente in linea retta

/* ----------------------------
   Grado di consanguineità
   (numero minimo di archi padre/madre tra due persone)
   Esclude archi di matrimonio.
   ---------------------------- */

% tutti gli antenati con distanza
antenato_con_distanza(Anc,Per,Dist) :-
    livello_antenato(Anc,Per,Dist).

consanguinity_degree(A,B,Degree) :-
    setof(D1-D2-CA,
          ( common_ancestor(A,B,CA),
            antenato_con_distanza(CA,A,D1),
            antenato_con_distanza(CA,B,D2)
          ),
          Lista),
    % prendi la somma minima D1+D2
    findall(Sum, (member(D1-D2-_, Lista), Sum is D1 + D2), Sums),
    min_list(Sums, Degree).

/* ----------------------------
   Percorso di relazione (spiegazione)
   Usa grafo non orientato con archi parentali e matrimoniali
   ---------------------------- */

% arco non orientato tra due persone per relazione "generica"
edge(X,Y) :- parent(X,Y).
edge(X,Y) :- parent(Y,X).
edge(X,Y) :- spouse(X,Y).

% costruisci grafo sugli individui presenti nei fatti
node(Persona) :-
    ( male(Persona) ; female(Persona) ; parent(Persona,_) ; parent(_,Persona) ; married(Persona,_) ).

all_nodes(Nodes) :- setof(N, node(N), Nodes).

all_edges(Edges) :-
    setof(X-Y, (edge(X,Y), X @< Y), Edges).

ugraph_from_facts(Graph) :-
    all_nodes(Nodes),
    all_edges(Edges),
    vertices_edges_to_ugraph(Nodes, Edges, Graph).

% relationship_path(A,B,Path): lista di persone dal primo all'ultimo
relationship_path(A,B,Path) :-
    ugraph_from_facts(G),
    vertices_edges_to_ugraph(_,_,G), % assicurati che G sia un ugraph
    vertices(G, _),                  % forza costruzione
    shortest_path(G, A, B, Path).

/* =========================================================
   ESEMPIO: Famiglia Rossi (puoi modificare/estendere)
   ========================================================= */

% Genere
male(luca).   female(marta).
male(gianni). female(anna).
male(piero).  female(sara).
male(paolo).  female(giulia).
male(nicola). female(elisa).
male(leo).

% Matrimoni
married(luca,marta).
married(gianni,anna).
married(piero,sara).
married(paolo,giulia).
married(nicola,elisa).

% Genitori
parent(luca,gianni).
parent(marta,gianni).
parent(luca,anna).
parent(marta,anna).

parent(gianni,piero).
parent(gianni,paolo).
parent(anna,paolo).       % fratellastri: paolo condivide solo gianni o anna?
                          % (qui: paolo ha genitori gianni+anna -> fratelli pieni con piero se anche anna è madre di piero)
parent(anna,piero).

parent(piero,leo).
parent(sara,leo).

parent(paolo,nicola).
parent(giulia,nicola).

parent(nicola,elisa).   % NOTA: questi due ultimi rendono elisa figlia di nicola,
parent(elisa,leo).      % e madre di leo: è solo a scopo dimostrativo (ciclo)  rimuovi in dati reali!

/* =========================================================
   NOTE:
   - Evita cicli genealogici (una persona non dovrebbe essere
     antenata di se stessa). Le due ultime righe mostrano come
     il sistema si comporta con dati incoerenti.
   - Per un dataset reale, togli gli esempi non plausibili.
   ========================================================= */