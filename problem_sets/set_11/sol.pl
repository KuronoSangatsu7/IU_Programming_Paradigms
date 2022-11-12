% 1.a
subseq([], []).
subseq(X, [_|Tail]) :- subseq(X, Tail).
subseq([Head|XTail], [Head|YTail]) :- subseq(XTail, YTail).

% 1.b TODO

% 1.c TODO
append([], Y, Y).
append([H|X], Y, [H|Z]) :- append(X, Y, Z).

% 1.d
suffix(X, X).
suffix(X, [_|Tail]) :- suffix(X, Tail).

% 1.e
repeat(X, [X]).
repeat(X, [X|Rest]) :- repeat(X, Rest).

% 2.a
allLEQ(_, []).
allLEQ(X, [H|Tail]) :- allLEQ(X, Tail), X =< H.

% 2.b
minimum(_, []).
minimum(X, Y) :- subseq([X], Y), allLEQ(X, Y).

% 2.c
partition(_, _, [], []).
partition(Pivot, [X|List], [X|Less], Greater) :- Pivot > X, partition(Pivot, List, Less, Greater).
partition(Pivot, [X|List], Less, [X|Greater]) :- X > Pivot, partition(Pivot, List, Less, Greater).
partition(Pivot, [X|List], Less, Greater) :- X = Pivot, partition(Pivot, List, Less, Greater).

% 2.d TODO

% 3.a TODO

% 3.b
countLeadingZeros([], 0).
countLeadingZeros([1|_], 0).
countLeadingZeros([0|Rest], N) :- countLeadingZeros(Rest, M), N is M + 1.

countTrailingZeros(X, N) :- countLeadingZeros(Z, N), reverse(X, Z).

% 4
fib2(0, 1).
fib2(Y, Z) :- fib2(X, Y), Z is Y + X.

fib(X) :- fib2(X, _).