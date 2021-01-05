:- use_module(library(clpfd)).
:- use_module(library(lists)).


% Checks if a position is on the border of the matrix
is_in_border(_, Cols, [_, Col]) :-
    is_in_horizontal_border(Cols, Col).
is_in_border(Rows, Cols, [Row, Col]) :-
    \+ is_in_horizontal_border(Cols, Col),
    is_in_vertical_border(Rows, Row).

is_in_horizontal_border(_, 0).
is_in_horizontal_border(Cols, Col) :-
    Col #\= 0,
    Col is Cols - 1.

is_in_vertical_border(_, 0).
is_in_vertical_border(Rows, Row) :-
    Row #\= 0,
    Row is Rows - 1.


% Predicate responsible for applying every restriction to every Fulcrum
restrict_fulcrums(_, _, _, []).
restrict_fulcrums(Mat, Rows, Cols, [[RowNum, ColNum] | Fulcrums]) :-
    get_horizontal_lists(Mat, Rows, Cols, [RowNum, ColNum], Left, Right, LeftCoeff, RightCoeff),
    get_vertical_lists(Mat, Rows, Cols, [RowNum, ColNum], Up, Down, UpCoeff, DownCoeff),
    scalar_product(LeftCoeff, Left, #=, H),
    scalar_product(RightCoeff, Right, #=, H),
    scalar_product(UpCoeff, Up, #=, V),
    scalar_product(DownCoeff, Down, #=, V),

    ((V #> 0) #/\ (H #= 0)) #\/ ((V #= 0) #/\ (H #> 0)),

    restrict_fulcrums(Mat, Rows, Cols, Fulcrums).

% Creates a list of [N, ..., 3, 2, 1]
reverse_filled_length([], 0).
reverse_filled_length([Size | T], Size) :-
    Size > 0,
    NextSize is Size - 1,
    reverse_filled_length(T, NextSize).

% Creates a list of [1, 2, 3, ..., N]
filled_length([], 0).
filled_length(L, Size) :-
    filled_length_aux(L, 1, Size).
filled_length_aux([Size], Size, Size).
filled_length_aux([CurSize | T], CurSize, Size) :-
    CurSize < Size,
    NextSize is CurSize + 1,
    filled_length_aux(T, NextSize, Size).

% This predicate doesn't screw stuff up, as the domain is 0..N and the Fulcrums (-1) are already instantiated
is_fulcrum(Val, Output) :-
    (Val #= -1) #<=> Output.

% Checks if a list contains a Fulcrum
contains_fulcrum([H | _]) :-
    is_fulcrum(H, 1).
contains_fulcrum([H | T]) :-
    is_fulcrum(H, 0),
    contains_fulcrum(T).    

% Checks for Fulcrums in each pair of lists (left/right or up/down)
% If either contain a Fulcrum, they return empty lists to reduce the number of redundant restrictions
% Thanks to the preprocessing, we know that no cases are being discarded with this predicate
check_lists_fulcrums(L1, _, _, _, [], [], [], []) :-
    contains_fulcrum(L1).
check_lists_fulcrums(L1, L2, _, _, [], [], [], []) :-
    \+ contains_fulcrum(L1),
    contains_fulcrum(L2).
check_lists_fulcrums(L1, L2, Len1, Len2, L1, L2, Coeff1, Coeff2) :-
    \+ contains_fulcrum(L1),
    \+ contains_fulcrum(L2),
    reverse_filled_length(Coeff1, Len1),
    filled_length(Coeff2, Len2).

% Gets the sublists and coefficients at the left and right of a given position
get_horizontal_lists(_, _, Cols, [_, ColNum], [], [], [], []) :-
    is_in_horizontal_border(Cols, ColNum).
get_horizontal_lists(Mat, _, Cols, [RowNum, ColNum], Left, Right, LeftCoeff, RightCoeff) :-
    \+ is_in_horizontal_border(Cols, ColNum),
    nth0(RowNum, Mat, Row),
    RowPrefix is ColNum,
    prefix_length(Row, LeftL, RowPrefix),
    RowSuffix is Cols - ColNum - 1,
    suffix_length(Row, RightL, RowSuffix),
    check_lists_fulcrums(LeftL, RightL, RowPrefix, RowSuffix, Left, Right, LeftCoeff, RightCoeff).

% Gets the sublists and coefficients above and below a given position
get_vertical_lists(_, Rows, _, [RowNum, _], [], [], [], []) :-
    is_in_vertical_border(Rows, RowNum).
get_vertical_lists(Mat, Rows, _, [RowNum, ColNum], Up, Down, UpCoeff, DownCoeff) :-
    \+ is_in_vertical_border(Rows, RowNum),
    get_col(Mat, ColNum, Col),
    ColPrefix is RowNum,
    prefix_length(Col, UpL, ColPrefix),
    ColSuffix is Rows - RowNum - 1,
    suffix_length(Col, DownL, ColSuffix),
    check_lists_fulcrums(UpL, DownL, ColPrefix, ColSuffix, Up, Down, UpCoeff, DownCoeff).

