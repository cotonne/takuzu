/* test framework */
assertEquals(M, X, X) :- write('[OK] '), write(M),nl.
assertEquals(M, X, Y) :- X\==Y,
	write('[KO] '), write(M),nl,
	write("Expecting "),write(X),
	write(", got "),write(Y),nl,fail.

/* utility functions */
getElementAt(X, Y, M, V):-
        nth0(Y, M, Row),
	nth0(X, Row, V).

nbOf(_, [], 0).
nbOf(X, [X|T], N):- 
	nbOf(X, T, Nx),
	N is Nx + 1.
nbOf(X, [Y|T], N):-
	X\==Y, 
	nbOf(X, T, N).

getColumn(_, [], []).
getColumn(Y, [H|T], [E|C]) :- 
	nth0(Y, H, E), 
	getColumn(Y, T, C).

getColumns(Y, M, []) :- length(M, L), L =< Y.
getColumns(Y, M, [C|R]) :-
	getColumn(Y, M, C),
	Yx is Y + 1,
	getColumns(Yx, M, R).
transpose(M, R) :-
	getColumns(0, M, R).

allElementsFrom(Xb, Xe, _, []) :- Xe < Xb.
allElementsFrom(Xb, Xe, M, [E|R]) :-
	nth0(Xb, M, E),
        Xb =< Xe,
	Xbx is Xb + 1,
	allElementsFrom(Xbx, Xe, M, R).

replaceWith(V, X, Y, M, Result):-
	length(M, S), Smin is S - 1,
	Yup is Y-1, allElementsFrom(0, Yup, M, Lup),
	allElementsFrom(Y, Y, M, Rtmp),
	nth0(0, Rtmp, R),
	Yun is Y + 1, allElementsFrom(Yun, Smin, M, Lun),
	Xbe is X - 1, allElementsFrom(0, Xbe, R, Rbe),
	Xaf is X + 1, allElementsFrom(Xaf, Smin, R, Raf),
	append(Rbe, [V|Raf], NewRow),
	append(Lup, [NewRow|Lun], Result).


/* Takuzu solver */
matrixDoesntContainLine(_, []).
matrixDoesntContainLine(L, [H|T]) :-
	L \= H,
	matrixDoesntContainLine(L, T).

matrixDoesntContainDuplicateLines([]).
matrixDoesntContainDuplicateLines([H|T]) :-
	nbOf('.', H, N),
	1 =< N,
        matrixDoesntContainDuplicateLines(T).

matrixDoesntContainDuplicateLines([H|T]) :- 	
        nbOf('.', H, N),
        N = 0,
	matrixDoesntContainLine(H, T),
	matrixDoesntContainDuplicateLines(T).

matrixDoesntContainDuplicateLinesOrColumns(M) :- 
	matrixDoesntContainDuplicateLines(M),
	transpose(M, T),
	matrixDoesntContainDuplicateLines(T).

isRowAsFairNumberOf0And1(L) :- 
	nbOf('0', L, N0),
	nbOf('1', L, N1),
	nbOf('.', L, N),
	abs(N0 - N1) =< N.


matrixDoestContainsUnfairNumberOf0or1ForLines([]).
matrixDoestContainsUnfairNumberOf0or1ForLines([H|T]) :-
	isRowAsFairNumberOf0And1(H),
 	matrixDoestContainsUnfairNumberOf0or1ForLines(T).	

matrixDoesntContainUnfairNumberOf0or1ForLinesOrColumns(M) :- 
	matrixDoestContainsUnfairNumberOf0or1ForLines(M),
	transpose(M, T),
	matrixDoestContainsUnfairNumberOf0or1ForLines(T).

matrixIsValid(M):- 
	matrixDoesntContainDuplicateLinesOrColumns(M),
	matrixDoesntContainUnfairNumberOf0or1ForLinesOrColumns(M).

replaceCurrent(_, Y, M, M) :- 
	length(M, S),
	S =< Y.
replaceCurrent(X, Y, M, R) :-
        length(M, S),
	X = S,
	Yx is Y + 1,
        replaceCurrent(0, Yx, M, R).

replaceCurrent(X, Y, M, R) :- 
	getElementAt(X, Y, M, E),
	E \= '.', 
	Xx is X + 1,
	replaceCurrent(Xx, Y, M, R).

replaceCurrent(X, Y, M, R) :-
        getElementAt(X, Y, M, E),
        E = '.', 
	replaceWith('0', X, Y, M, Mx),
        matrixIsValid(Mx),
	Xx is X + 1,
        replaceCurrent(Xx, Y, Mx, R).

replaceCurrent(X, Y, M, R) :-
        getElementAt(X, Y, M, E),
        E = '.', 
        replaceWith('1', X, Y, M, Mx),
 	matrixIsValid(Mx),
	Xx is X + 1,
        replaceCurrent(Xx, Y, Mx, R).

replaceDotBy0or1(M, R) :- replaceCurrent(0, 0, M, R).

solveTakuzu(M, R) :- 
	replaceDotBy0or1(M, R),
	matrixIsValid(R).

/* TESTS */
testAllElementsFrom0To0 :-
	allElementsFrom(0, 0, [0, 1, 2], X),
	assertEquals('Should take the first element', [0], X).

testAllElementsInTheMiddle :-
        allElementsFrom(1, 3, [0, 1, 2, 3, 4, 5], X),
        assertEquals('Should take the first element', [1, 2, 3], X).

testAllElementsInTheEnd :-
        allElementsFrom(4, 5, [0, 1, 2, 3, 4, 5], X),
        assertEquals('Should take the first element', [4, 5], X).

testReplaceWith :-
	replaceWith(0, 1, 1, [[0, 1, 2], [3, 4, 5], [6, 7, 8]], X),
	assertEquals('Should replace the center with 0', [[0, 1, 2], [3, 0, 5], [6, 7, 8]], X).

testNoSolutionForTakuzu :-
	not(solveTakuzu([['1', '0'], ['1', '0']], _)).

testSimpleSolutionForTakuzu :-
	solveTakuzu([['.', '1'], ['1', '0']], R),
	assertEquals("should find a simple anwser for takuzu", [['0', '1'], ['1', '0']], R).
	
testAnotherSimpleSolutionForTakuzu :-
        solveTakuzu([['.', '0'], ['0', '1']], R),
        assertEquals("should find a simple anwser for takuzu", [['1', '0'], ['0', '1']], R).


testMatrixIsNotValidBecauseOfUnfairNumberOf1 :-
	not(matrixIsValid([['1', '1'], ['1', '.']])).

testMatrixIsValid :-
	matrixIsValid([['1', '0'], ['0', '.']]).

testFinal :- 
	getElementAt(2, 1, [[0, 1, 2], [3, 4, 5], [6, 7, 8]], X),
	assertEquals('should get the good terms from matrix', 5, X).

testDoestNotContainDuplicateLines :- 
	matrixDoesntContainDuplicateLines([[0, 1, 2], [3, 4, 5], [6, 7, 8]]).

testDoestNotContainDuplicateLinesIfThereIsJoker :-
        matrixDoesntContainDuplicateLines([['0', '0', '1', '.'], ['0', '0', '1', '.']]).


testContainsDuplicateLines :-
        not(matrixDoesntContainDuplicateLines([[1, 0], [1, 0]])).

testMatrixDoesntContainDuplicateLinesOrColumns :-
	matrixDoesntContainDuplicateLinesOrColumns([[0, 1, 2], [3, 4, 5], [6, 7, 8]]).

testMatrixContainsDuplicateLinesOrColumns :-
	not(matrixDoesntContainDuplicateLinesOrColumns([[1, 1, 0, 0], [1, 1, 0, 1], [1, 1, 1, 0], [1, 1, 1, 1]])).

testNbOf:-
	nbOf('1', ['1', '0', '.', '1'], X),
	assertEquals("should count correclty element 1 in list", 2, X).

testRowIsValid:-
	isRowAsFairNumberOf0And1(['1', '0', '0', '1']).

testRowIsNotValid:-
	not(isRowAsFairNumberOf0And1(['1', '1', '1', '0'])).

testRowIsValidWithDot:-
	isRowAsFairNumberOf0And1(['1', '0', '.', '1']).

testRowIsNotValidWithDot:-
        not(isRowAsFairNumberOf0And1(['1', '1', '1', '.'])).


testGetColumn :- 
	getColumn(2, [[0, 1, 2], [3, 4, 5], [6, 7, 8]], C),
	assertEquals("should get the second column", [2, 5, 8], C).
	
testTranspose :- 
	transpose([[0, 1, 2], [3, 4, 5], [6, 7, 8]], X),
        assertEquals("should get the transposed matrix", [[0, 3, 6], [1, 4, 7], [2, 5, 8]], X).

testAll :- 
	testAllElementsFrom0To0,
	testAllElementsInTheMiddle,
	testAllElementsInTheEnd,
	testReplaceWith,
	testNoSolutionForTakuzu,
	testSimpleSolutionForTakuzu,
	testAnotherSimpleSolutionForTakuzu,
	testMatrixIsNotValidBecauseOfUnfairNumberOf1,
	testMatrixIsValid,
	testFinal, 
	testDoestNotContainDuplicateLines, 
	testDoestNotContainDuplicateLinesIfThereIsJoker,
	testContainsDuplicateLines,
	testMatrixDoesntContainDuplicateLinesOrColumns,
	testMatrixContainsDuplicateLinesOrColumns,
	testNbOf,
	testRowIsValid,
	testRowIsNotValid,
	testRowIsValidWithDot,
	testRowIsNotValidWithDot,
	testGetColumn, 
	testTranspose.
