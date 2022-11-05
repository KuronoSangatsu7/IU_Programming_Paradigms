% student(Name, Group)
student(alisa, 2).
student(bob, 1).
student(chloe, 2).
student(denise, 1).
student(edward, 2).

% friend(Name, Name)
friend(alisa, bob).
friend(alisa, denise).
friend(bob, chloe).
friend(bob, edward).
friend(chloe, denise).
friend(denise, edward).

% parent(Parent, Child)
parent(marjorie, bart).
parent(marjorie, lisa).
parent(marjorie, maggie).
parent(homer, bart).
parent(homer, lisa).
parent(homer, maggie).
parent(abraham, homer).
parent(mona, homer).
parent(jacqueline, marjorie).
parent(jacqueline, patty).
parent(jacqueline, selma).
parent(clancy, marjorie).
parent(clancy, patty).
parent(clancy, selma).
parent(selma, ling).

% unary(Number)
unary(z).
unary(s(X)) :- unary(X).

% q2
groupmates(X, Y) :- student(X, Z), student(Y, Z).

% q3
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).

relative(X, Y) :- ancestor(Z, X), ancestor(Z, Y).

% q4.a
double(z, z).
double(s(X), s(s(Y))) :- double(X, Y).

% q4.b
leq(z, z).
leq(z, s(Y)) :- leq(z, Y).
leq(s(X), s(Y)) :- leq(X, Y).

% q4.c
add(z, X, X).
add(s(Y), X, s(Z)) :- add(Y, X, Z).

mult(z, X, z).
mult(s(z), X, X).
mult(s(Y), X, Z) :- add(X, S, Z), mult(Y, X, S).

% q4.d
powerOf2(z, s(z)).
powerOf2(s(X), Y) :- leq(Y, s(s(s(s(s(s(s(s(z))))))))), mult(s(s(z)), S, Y), powerOf2(X, S).