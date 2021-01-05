:- use_module(library(lists)).
:- use_module(library(samsort)).

% Finds the duplicate elements Dups in a list L
find_dups(L, Dups) :-
    samsort(L, SortedL),
    find_dups_aux(SortedL, DupsWithDups),
    remove_dups(DupsWithDups, Dups).

% Finds the duplicates given a samsorted list
find_dups_aux([], []).
find_dups_aux([_], []).
find_dups_aux([_A, B | L], Dups) :-
    _A \= B,
    find_dups_aux([B | L], Dups).
find_dups_aux([A, A | L], [A | Dups]) :-
    find_dups_aux(L, Dups).


% Creates matrix of given size
create_matrix(0, _, []).
create_matrix(Rows, Cols, [H | T]) :-
    Rows > 0,
    length(H, Cols),
    NextRows is Rows - 1,
    create_matrix(NextRows, Cols, T).

% Returns a matrix row
get_row(Mat, RowNum, Row) :-
    ActualRowNum is RowNum + 1,
    element(ActualRowNum, Mat, Row).

% Returns a matrix column
get_col(Mat, ColNum, Col) :-
    ActualColNum is ColNum + 1,
    get_col_aux(Mat, ActualColNum, Col).
get_col_aux([], _, []).
get_col_aux([H | T], ColNum, [H1 | T1]) :-
    element(ColNum, H, H1),
    get_col_aux(T, ColNum, T1).


% Places Fulcrums
place_fulcrums(_, []).
place_fulcrums(Mat, [[Row, Col] | T]) :-
    nth0(Row, Mat, Line),
    nth0(Col, Line, -1),
    place_fulcrums(Mat, T).

% Zeroes out rows and cols whenever Fulcrums are on the border
nullify_cells_from_border_fulcrums(_, _, _, []).
nullify_cells_from_border_fulcrums(Mat, Rows, Cols, [[Row, Col] | T]) :-
    zero_col_row(Mat, Rows, Cols, [Row, Col]),
    nullify_cells_from_border_fulcrums(Mat, Rows, Cols, T).

% Zeroes out whenever a Fulcrum is on the border
zero_col_row(Mat, _, _, [Row, 0]) :-
    zero_row(Mat, Row), !.
zero_col_row(Mat, _, _, [0, Col]) :-
    zero_col(Mat, Col), !.
zero_col_row(Mat, _, Cols, [Row, Col]) :-
    Col is Cols - 1,
    zero_row(Mat, Row), !.
zero_col_row(Mat, Rows, _, [Row, Col]) :-
    Row is Rows - 1,
    zero_col(Mat, Col), !.
zero_col_row(_, _, _, _).

% Zeroes all unassigned vars of row
zero_row(Mat, RowNum) :-
    nth0(RowNum, Mat, Row),
    set_val(Row, 0).
% Zeroes all unassigned vars of col
zero_col(Mat, ColNum) :-
    get_col(Mat, ColNum, Col),
    set_val(Col, 0).

% Sets all unassigned vars of List to Val
set_val([], _).
set_val([H|T], Val) :-
    var(H),
    H is Val,
    set_val(T, Val).
set_val([H | T], Val) :-
    \+ var(H),
    set_val(T, Val).

% Creates two lists: The first contains the indexes of the rows that contain fulcrums and 
% the second conatins the indexes of the columns that contain fulcrums (contains duplicates which is important)
get_fulcrum_lines_cols([], [], []).
get_fulcrum_lines_cols([[Row, Col] | T], [Row | Lines], [Col | Cols]) :-
    get_fulcrum_lines_cols(T, Lines, Cols).

% Nullifies (puts a 0) cells that don't have fulcrums in its row or column
nullify_cells(Mat, Lines, Cols) :-
    % Cut because we know there is only 1 solution possible
    nullify_cells_aux(Mat, 0, Lines, Cols).

% We want to nullify cells that respect the following condition:
% 0 if NOT col OR NOT Row
% AKA: 0 if NOT (col AND row)
nullify_cells_aux([], _, _, _).
nullify_cells_aux([H | T], I, Lines, Cols) :-
    NextI is I + 1,
    nullify_cells_aux_lines(H, I, 0, Lines, Cols),
    nullify_cells_aux(T, NextI, Lines, Cols).

% Called only if line doesn't have a Fulcrum
nullify_cells_aux_lines([], _, _, _, _).
nullify_cells_aux_lines([_ | T], I, J, Lines, Cols) :-
    member(I, Lines),
    member(J, Cols),
    NextJ is J + 1,
    nullify_cells_aux_lines(T, I, NextJ, Lines, Cols).
nullify_cells_aux_lines([0 | T], I, J, Lines, Cols) :-
    \+ (
        member(I, Lines),
        member(J, Cols)
    ),
    NextJ is J + 1,
    nullify_cells_aux_lines(T, I, NextJ, Lines, Cols).

% Nullifies cells from rows/cols where there are multiple Fulcrums
nullify_cells_from_multiple_fulcrums(Mat, FLines, FCols) :-
    find_dups(FLines, DupLines),
    find_dups(FCols, DupCols),
    nullify_cells_from_rows(Mat, DupLines),
    nullify_cells_from_cols(Mat, DupCols).

nullify_cells_from_rows(_, []).
nullify_cells_from_rows(Mat, [H | T]) :-
    nth0(H, Mat, Row),
    set_val(Row, 0),
    nullify_cells_from_rows(Mat, T).

nullify_cells_from_cols(_, []).
nullify_cells_from_cols(Mat, [H | T]) :-
    get_col(Mat, H, Col),
    set_val(Col, 0),
    nullify_cells_from_cols(Mat, T).

% flatten a list (or multiple nested lists, such as a matrix)
flatten([], []).
flatten([A|B],L) :- 
    is_list(A),
    flatten(B,B1),
    append(A,B1,L).
flatten([A|B], [A|B1]) :-
    \+ is_list(A),
    flatten(B, B1).

% Returns Vars that contains all the variables on the matrix
get_vars_mat(Mat, Vars) :-
    flatten(Mat, Tmp),
    get_vars(Tmp, Vars).

% Returns a list with all the uninstantiated variables from an input list 
get_vars([], []).
get_vars([A|B], [A|B1]) :- 
    var(A),
    get_vars(B, B1).
get_vars([A|B], B1) :-
    \+ var(A),
    get_vars(B, B1).

