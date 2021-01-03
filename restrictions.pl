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
    Size > 0,
    NextSize is Size - 1,
    reverse_filled_length(T, NextSize).

filled_length([], 0).
filled_length(L, Size) :-
    filled_length_aux(L, 1, Size).

filled_length_aux([Size], Size, Size).
filled_length_aux([CurSize | T], CurSize, Size) :-
    CurSize < Size,
    NextSize is CurSize + 1,
    filled_length_aux(T, NextSize, Size).

is_fulcrum(Val, Output) :-
    (Val #= -1) #<=> Output.

contains_fulcrum([H | _]) :-
    is_fulcrum(H, 1).
contains_fulcrum([H | T]) :-
    is_fulcrum(H, 0),
    contains_fulcrum(T).

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



find_fulcrums(Mat, Fulcrums) :-
    find_fulcrums_aux(Mat, 0, [], Fulcrums).


find_fulcrums_aux([], _, Fulcrums, Fulcrums).
find_fulcrums_aux([Row | T], RowNum, Fulcrums, Res) :-
    find_fulcrums_cols(Row, RowNum, 0, RowFulcrums),
    NextRow is RowNum + 1,
    append(Fulcrums, RowFulcrums, NextFulcrums),
    find_fulcrums_aux(T, NextRow, NextFulcrums, Res).


find_fulcrums_cols([], _, _, []).
find_fulcrums_cols([H | T], RowNum, ColNum, [[RowNum, ColNum] | Fulcrums]) :-
    is_fulcrum(H, 1),
    NextCol is ColNum + 1,
    find_fulcrums_cols(T, RowNum, NextCol, Fulcrums).
find_fulcrums_cols([H | T], RowNum, ColNum, Fulcrums) :-
    is_fulcrum(H, 0),
    NextCol is ColNum + 1,
    find_fulcrums_cols(T, RowNum, NextCol, Fulcrums).



count_line(Line, Domain, Fulcrums, Blanks, Digits) :-
    get_count_line_cardinality(Domain, CL, Fulcrums, Blanks, 0, Digits),
    global_cardinality(Line, CL).

get_count_line_cardinality(0, [0-A, -1-B], B, A, Digits, Digits) :-
    A #>= 0,
    B #>= 0.
get_count_line_cardinality(Domain, [Domain-A | T], Fulcrums, Blanks, DigitsAcc, Digits) :-
    Domain > 0,
    NextDomain is Domain - 1,
    NextDigitsAcc #= DigitsAcc + A,
    get_count_line_cardinality(NextDomain, T, Fulcrums, Blanks, NextDigitsAcc, Digits).



restrict_digit_count(Mat, Domain, _, Cols, RowCount, ColCount) :-
    restrict_digit_count_rows(Mat, Domain, RowCount),
    restrict_digit_count_cols(Mat, Domain, Cols, 0, ColCount).

restrict_digit_count_rows([], _, []).
restrict_digit_count_rows([Row | Mat], Domain, [Fulcrums-Blanks-Digits | T]) :-
    restrict_digit_count_list(Row, Domain, Fulcrums-Blanks-Digits),
    restrict_digit_count_rows(Mat, Domain, T).
    

restrict_digit_count_cols(_, _, Cols, Cols, []).
restrict_digit_count_cols(Mat, Domain, Cols, ColNum, [Fulcrums-Blanks-Digits | T]) :-
    ColNum < Cols,
    get_col(Mat, ColNum, Col),
    restrict_digit_count_list(Col, Domain, Fulcrums-Blanks-Digits),
    NextCol is ColNum + 1,
    restrict_digit_count_cols(Mat, Domain, Cols, NextCol, T).


restrict_digit_count_list(List, Domain, Fulcrums-Blanks-Digits) :-
    count_line(List, Domain, Fulcrums, Blanks, Digits),
    Digits #\= 1,
    (Digits #= 0) #\/ (Fulcrums #= 1).



restrict_cells_with_empty_col_and_row([], _, _).
restrict_cells_with_empty_col_and_row([Row | Mat], [R | RowCount], ColCount) :-
    restr_cells_empty_col(Row, R, ColCount),
    restrict_cells_with_empty_col_and_row(Mat, RowCount, ColCount).

restr_cells_empty_col([], _, _).
restr_cells_empty_col([Cell | Cols], _-_-RDigits, [_-_-CDigits | ColCount]) :-
    ((RDigits #= 0) #/\ (CDigits #= 0)) #<=> 1,
    Cell #\= -1,
    restr_cells_empty_col(Cols, _-_-RDigits, ColCount).
restr_cells_empty_col([_ | Cols], _-_-RDigits, [_-_-CDigits | ColCount]) :-
    ((RDigits #= 0) #/\ (CDigits #= 0)) #<=> 0,
    restr_cells_empty_col(Cols, _-_-RDigits, ColCount).

