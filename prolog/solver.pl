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
:- begin_tests(takuzu).
test(should_take_the_first_element) :-
	allElementsFrom(0, 0, [0, 1, 2], X),
        X =:= [0], !.

test(should_take_the_first_element_in_the_middle) :-
        allElementsFrom(1, 3, [0, 1, 2, 3, 4, 5], X),
        X =@= [1, 2, 3], !.

test(should_take_the_first_element_in_the_end) :-
        allElementsFrom(4, 5, [0, 1, 2, 3, 4, 5], X),
        X =@= [4, 5], !.

test(should_replace_the_center_with_0) :-
	replaceWith(0, 1, 1, [[0, 1, 2], [3, 4, 5], [6, 7, 8]], X),
        X =@= [[0, 1, 2], [3, 0, 5], [6, 7, 8]], !.

test(should_not_find_solution_for_takuzu, [fail]) :-
	solveTakuzu([['1', '0'], ['1', '0']], _).

test(should_find_a_simple_solution_for_takuzu) :-
	solveTakuzu([['.', '1'], ['1', '0']], R),
	R =@= [['0', '1'], ['1', '0']], !.
	
test(should_find_another_simple_solution_for_takuzu) :-
        solveTakuzu([['.', '0'], ['0', '1']], R),
        R =@= [['1', '0'], ['0', '1']], !.


test(should_not_validate_matrix_because_of_unfair_number_of_1, [fail]) :-
	matrixIsValid([['1', '1'], ['1', '.']]).

test(should_validate_this_matrix) :-
	matrixIsValid([['1', '0'], ['0', '.']]), !.

test(should_get_the_good_terms_from_matrix) :- 
	getElementAt(2, 1, [[0, 1, 2], [3, 4, 5], [6, 7, 8]], X),
        X =:= 5, !.

test(should_not_indicate_that_matrix_contains_duplicate_lines) :- 
	matrixDoesntContainDuplicateLines([[0, 1, 2], [3, 4, 5], [6, 7, 8]]), !.

test(should_not_indicate_that_matrix_contains_duplicate_lines_when_there_is_joker) :-
        matrixDoesntContainDuplicateLines([['0', '0', '1', '.'], ['0', '0', '1', '.']]), !.


test(should_indicate_that_matrix_contains_duplicate_lines, [fail]) :-
        matrixDoesntContainDuplicateLines([[1, 0], [1, 0]]).

test(should_say_that_matrix_doesnt_contain_duplicate_lines_or_columns) :-
	matrixDoesntContainDuplicateLinesOrColumns([[0, 1, 2], [3, 4, 5], [6, 7, 8]]), !.

test(should_say_that_matrix_contains_duplicate_lines_or_columns, [fail]) :-
	matrixDoesntContainDuplicateLinesOrColumns([[1, 1, 0, 0], [1, 1, 0, 1], [1, 1, 1, 0], [1, 1, 1, 1]]).

test(should_count_the_occurence_of_1) :-
	nbOf('1', ['1', '0', '.', '1'], X),
        X =:= 2, !.

test(should_say_that_row_has_a_fair_number_of_1_and_0):-
	isRowAsFairNumberOf0And1(['1', '0', '0', '1']), !.

test(should_say_that_row_has_not_a_fair_number_of_1_and_0, [fail]):-
	isRowAsFairNumberOf0And1(['1', '1', '1', '0']).

test(should_say_that_row_has_a_fair_number_of_1_and_0_when_there_is_a_joker):-
	isRowAsFairNumberOf0And1(['1', '0', '.', '1']), !.

test(should_say_that_row_has_not_a_fair_number_of_1_and_0_with_a_joker, [fail]):-
        isRowAsFairNumberOf0And1(['1', '1', '1', '.']).


test(should_return_the_good_column) :- 
	getColumn(2, [[0, 1, 2], [3, 4, 5], [6, 7, 8]], C),
	C =@= [2, 5, 8], !.
	
test(should_transpose_a_matrix) :- 
	transpose([[0, 1, 2], [3, 4, 5], [6, 7, 8]], X),
        X =@=[[0, 3, 6], [1, 4, 7], [2, 5, 8]], !.


:- end_tests(takuzu).
