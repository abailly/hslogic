female(leia).
male(vader).
male(luke).
male(kylo).

child(luke, vader).
child(leia, vader).
child(kylo, leia).

son(X,Y) :- male(X), child(X,Y).
daughter(X,Y) :- female(X), child(X,Y).

grandchild(X,Z) :- child(X,Y), child(Y,Z).
