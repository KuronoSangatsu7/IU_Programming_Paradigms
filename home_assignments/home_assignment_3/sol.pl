% Exercise 1

% 1.1
tree(empty).
tree(node(_, Left, Right)) :- tree(Left), tree(Right).

% 1.2
containedTree(empty, _).
containedTree(node(Val1, Left1, Right1), node(Val1, Left2, Right2)) :- 
    containedTree(Left1, Left2), 
    containedTree(Right1, Right2).

% 1.3
from(_, []).
from(X, [X]).
from(Start, [Start, Head|Rest]) :- 
    Head is Start + 1,
    from(Head, [Head|Rest]).

% 1.4
preorder(empty, []).
preorder(node(Value, Left, Right), [Value | List]) :-
    append(L1, L2, List),
    preorder(Left, L1),
    preorder(Right, L2).

% 1.5
leq(-infinity, _).
leq(_, +infinity).
leq(X, Y) :-
    X =< Y.

less(_, +infinity).
less(-infinity, _).
less(X, Y) :-
    X < Y.

% 1.6
bst(empty).
bst(node(_, empty, empty)).
bst(node(Value, empty, node(RightValue, Subtree1, Subtree2))) :-
    leq(Value, RightValue),
    bst(Subtree1),
    bst(Subtree2).
bst(node(Value, node(LeftValue, Subtree1, Subtree2), empty)) :-
    less(LeftValue, Value),
    bst(Subtree1),
    bst(Subtree2).
bst(node(Value, node(LeftValue, Subtree1, Subtree2), node(RightValue, Subtree3, Subtree4))) :-
    less(LeftValue, Value),
    leq(Value, RightValue),
    bst(node(LeftValue, Subtree1, Subtree2)),
    bst(node(RightValue, Subtree3, Subtree4)).

% 1.7
bstInsert(Value, empty, node(Value, empty, empty)).
bstInsert(InsertValue, node(Value, Left, Right), node(Value, Left, Next)) :-
    less(Value, InsertValue),
    bstInsert(InsertValue, Right, Next).
bstInsert(InsertValue, node(Value, Left, Right), node(Value, Next, Right)) :-
    leq(InsertValue, Value),
    bstInsert(InsertValue, Left, Next).
          

% 1.8
bstMin(empty, -infinity).
bstMin(node(Min, empty, _), Min).
bstMin(node(_, Left, _), Min) :-
    bstMin(Left, Min).

bstMax(empty, +infinity).
bstMax(node(Max, _, empty), Max).
bstMax(node(_, _, Right), Max) :-
    bstMax(Right, Max).

% ----------------------------------------------------------------------

% Exercise 2

% 2.1
expr(X + Y) :- !, nonvar(X), expr(X), nonvar(Y), expr(Y).
expr(X * Y) :- !, nonvar(X), expr(X), nonvar(Y), expr(Y).
expr(X) :- nonvar(X), number(X).

% 2.2
expr(X, [X]).
expr(X + Y, List) :- append(L1, L2, List), L1 \= [], L2\= [], expr(X, L1), expr(Y, L2).
expr(X * Y, List) :- append(L1, L2, List), L1 \= [], L2\= [], expr(X, L1), expr(Y, L2).

% 2.3
equation(List, Equation) :- expr(Expression, List), Result is Expression, Equation = (Result = Expression).

%2.4
% Reference: https://stackoverflow.com/questions/4191038/getting-list-of-solutions-in-prolog
equations(Values, Equations) :-
    equations(Values, [], Equations).

equations(Values, Acc, Equations) :-
    equation(Values, Equation),
    \+ member(Equation, Acc), !,
    equations(Values, [Equation|Acc], Equations). 
equations(_, Equations, Equations).




