:- use_module(library(clpfd)).
:- use_module(library(lists)).


% Checks if a position is on the border of the matrix
is_in_border(_, Cols, [_, Col]) :-
    is_in_horizontal_border(Cols, Col).
is_in_border(Rows, _, [Row, _]) :-
    is_in_vertical_border(Rows, Row).

is_in_horizontal_border(_, 0).
is_in_horizontal_border(Cols, Col) :-
    Col is Cols - 1.

is_in_vertical_border(_, 0).
is_in_vertical_border(Rows, Row) :-
    Row is Rows - 1.


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


reverse_filled_length([], 0).
reverse_filled_length([Size | T], Size) :-
    NextSize is Size - 1,
    reverse_filled_length(T, NextSize).

filled_length([], 0).
filled_length(L, Size) :-
    filled_length_aux(L, 1, Size).

filled_length_aux([Size], Size, Size).
filled_length_aux([CurSize | T], CurSize, Size) :-
    NextSize is CurSize + 1,
    filled_length_aux(T, NextSize, Size).


get_horizontal_lists(_, _, Cols, [_, ColNum], [], [], [], []) :-
    is_in_horizontal_border(Cols, ColNum).
get_horizontal_lists(Mat, _, Cols, [RowNum, ColNum], Left, Right, LeftCoeff, RightCoeff) :-
    nth0(RowNum, Mat, Row),
    RowPrefix is ColNum,
    prefix_length(Row, Left, RowPrefix),
    reverse_filled_length(LeftCoeff, RowPrefix),
    RowSuffix is Cols - ColNum - 1,
    suffix_length(Row, Right, RowSuffix),
    filled_length(RightCoeff, RowSuffix).

get_vertical_lists(_, Rows, _, [RowNum, _], [], [], [], []) :-
    is_in_vertical_border(Rows, RowNum).
get_vertical_lists(Mat, Rows, _, [RowNum, ColNum], Up, Down, UpCoeff, DownCoeff) :-
    get_col(Mat, ColNum, Col),
    ColPrefix is RowNum,
    prefix_length(Col, Up, ColPrefix),
    reverse_filled_length(UpCoeff, ColPrefix),
    ColSuffix is Rows - RowNum - 1,
    suffix_length(Col, Down, ColSuffix),
    filled_length(DownCoeff, ColSuffix).
