father(john, mary).
father(john, tom).
father(kevin, john).
mother(eva, mary).
mother(eva, tom).
mother(cristina, john).
mother(cristina, kelly).
mother(kelly, alloy).
parent(X,Y) :- father(X,Y).
parent(X,Y) :- mother(X,Y).
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(X,Z),ancestor(Z,Y).

append([], Y, Y).
append([X|Xs], Y, [X|Rs]) :- append(Xs, Y, Rs).

reverse([], []).
reverse([H|T], R) :- reverse(T, R1), append(R1, [H], R).