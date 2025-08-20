% Dimostrazione del backtracking in JProlog IDE
% Carica questo file e prova le query seguenti con ";"

% Database di studenti e voti
student(alice).
student(bob).
student(charlie).
student(diana).

grade(alice, math, 95).
grade(alice, physics, 87).
grade(alice, chemistry, 92).
grade(bob, math, 78).
grade(bob, physics, 85).
grade(charlie, math, 88).
grade(charlie, chemistry, 90).
grade(diana, physics, 92).
grade(diana, chemistry, 89).

% Regole per le prestazioni
excellent(Student, Subject) :- grade(Student, Subject, Grade), Grade >= 90.
good(Student, Subject) :- grade(Student, Subject, Grade), Grade >= 80, Grade < 90.
needs_improvement(Student, Subject) :- grade(Student, Subject, Grade), Grade < 80.

% Database di animali
animal(dog).
animal(cat).
animal(bird).
animal(fish).

% Proprietà degli animali
has_fur(dog).
has_fur(cat).
has_wings(bird).
lives_in_water(fish).

% Database di cibi e categorie
food(apple).
food(banana).
food(carrot).
food(broccoli).
food(chicken).
food(beef).

category(apple, fruit).
category(banana, fruit).
category(carrot, vegetable).
category(broccoli, vegetable).
category(chicken, meat).
category(beef, meat).

% Regole nutrizionali
healthy(Food) :- category(Food, fruit).
healthy(Food) :- category(Food, vegetable).
protein_source(Food) :- category(Food, meat).

% Database di colori e oggetti
object_color(apple, red).
object_color(banana, yellow).
object_color(carrot, orange).
object_color(broccoli, green).
object_color(sky, blue).
object_color(sun, yellow).
object_color(grass, green).

% Query suggerite per testare il backtracking:
% 1. student(X).          % Mostra tutti gli studenti uno per volta
% 2. grade(alice, X, Y).  % Mostra tutti i voti di Alice
% 3. excellent(X, Y).     % Trova tutti gli eccellenti risultati
% 4. animal(X).           % Mostra tutti gli animali
% 5. healthy(X).          % Trova tutti i cibi sani
% 6. object_color(X, yellow). % Trova tutti gli oggetti gialli
% 7. grade(X, math, Y).   % Trova tutti i voti in matematica