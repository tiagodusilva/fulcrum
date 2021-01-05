% Generates the cardinality list ignoring Fulcrums
get_cardinality_list(0, [0-A]) :-
    A #>= 0.
get_cardinality_list(Domain, [Domain-1 | T]) :-
    Domain > 0,
    NextDomain is Domain - 1,
    get_cardinality_list(NextDomain, T).

% Generates the cardinality list with Fulcrums
get_cardinality_list_with_fulcrums(0, NoFulcrums, [0-A, -1-NoFulcrums]) :-
    A #>= 0.
get_cardinality_list_with_fulcrums(Domain, NoFulcrums, [Domain-1 | T]) :-
    Domain > 0,
    NextDomain is Domain - 1,
    get_cardinality_list_with_fulcrums(NextDomain, NoFulcrums, T).


% Safely counts the number of each type of variable in a list
% Thanks to global cardinality no values are instantiated
count_line(Line, Domain, Fulcrums, Blanks, Digits) :-
    get_count_line_cardinality(Domain, CL, Fulcrums, Blanks, 0, Digits),
    global_cardinality(Line, CL).

% Returns the list to be for count line, alongside the accumulator used for number of digits
get_count_line_cardinality(0, [0-A, -1-B], B, A, Digits, Digits) :-
    A #>= 0,
    B #>= 0.
get_count_line_cardinality(Domain, [Domain-A | T], Fulcrums, Blanks, DigitsAcc, Digits) :-
    Domain > 0,
    NextDomain is Domain - 1,
    NextDigitsAcc #= DigitsAcc + A,
    get_count_line_cardinality(NextDomain, T, Fulcrums, Blanks, NextDigitsAcc, Digits).


% For every row and columns counts the types of variables in them and returns a list with said counts for rows and collumns
% For each cell it also applies the restriction no line with 1 digit, every line with 2+ digits must contain exactly 1 fulcrum
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

% For each member of list counts the number of elements and applies the restriction no line with 1 digit, every line with 2+ digits must contain exactly 1 fulcrum
restrict_digit_count_list(List, Domain, Fulcrums-Blanks-Digits) :-
    count_line(List, Domain, Fulcrums, Blanks, Digits),
    Digits #\= 1,
    (Digits #= 0) #\/ (Fulcrums #= 1).

% Forces the generated solutions to occupy the whole board (to avoid 5x6 solutions inside a 8x8 grid for example)
force_full_sized_puzzle(RowCount, ColCount) :-
    RowCount = [FRFulcrums-_-FRDigits | _],
    ColCount = [FCFulcrums-_-FCDigits | _],
    last(RowCount, LRFulcrums-_-LRDigits),
    last(ColCount, LCFulcrums-_-LCDigits),
    (FRFulcrums #> 0) #\/ (FRDigits #>= 2),
    (FCFulcrums #> 0) #\/ (FCDigits #>= 2),
    (LRFulcrums #> 0) #\/ (LRDigits #>= 2),
    (LCFulcrums #> 0) #\/ (LCDigits #>= 2).

% Removes most symmetry from the solutions by forcing the scalar product of the first row to be >= to the last row
% Same applied for the cols
% Unfortunately, there are better methods to enforce this
remove_some_symmetry(Mat, Rows, Cols) :-
    Mat = [FRow | _],
    last(Mat, LRow),
    get_col(Mat, 0 , FCol),
    LastColNum is Cols - 1,
    get_col(Mat, LastColNum, LCol),
    filled_length(RCoeffs, Cols),
    filled_length(CCoeffs, Rows),
    scalar_product(RCoeffs, FRow, #=, FR),
    scalar_product(RCoeffs, LRow, #>=, FR),
    scalar_product(CCoeffs, FCol, #=, FC),
    scalar_product(CCoeffs, LCol, #>=, FC).

% Restricts cells with no fulcrum in either their row or column to be Blanks
% Restricts cells with no digits in both their row or column to not be Fulcrums
restrict_cells_with_empty_col_and_row([], _, _).
restrict_cells_with_empty_col_and_row([Row | Mat], [R | RowCount], ColCount) :-
    restr_cells_empty_col(Row, R, ColCount),
    restrict_cells_with_empty_col_and_row(Mat, RowCount, ColCount).
restr_cells_empty_col([], _, _).
restr_cells_empty_col([Cell | Cols], RFulcrums-_-RDigits, [CFulcrums-_-CDigits | ColCount]) :-
    ((RFulcrums #= 0) #\/ (CFulcrums #= 0) #/\ (Cell #\= -1)) #=> (Cell #= 0),
    ((RDigits #= 0) #/\ (CDigits #= 0)) #=> (Cell #\= -1),
    restr_cells_empty_col(Cols, _-_-RDigits, ColCount).

% Equivalent of get_horizontal_lists but handles non-instantiated matrix
restr_get_horizontal_lists(_, _, Cols, [_, ColNum], [], [], [], []) :-
    is_in_horizontal_border(Cols, ColNum).
restr_get_horizontal_lists(Mat, _, Cols, [RowNum, ColNum], Left, Right, LeftCoeff, RightCoeff) :-
    \+ is_in_horizontal_border(Cols, ColNum),
    nth0(RowNum, Mat, Row),
    RowPrefix is ColNum,
    prefix_length(Row, Left, RowPrefix),
    RowSuffix is Cols - ColNum - 1,
    suffix_length(Row, Right, RowSuffix),
    reverse_filled_length(LeftCoeff, RowPrefix),
    filled_length(RightCoeff, RowSuffix).

% Equivalent of get_vertical_lists but handles non-instantiated matrix
restr_get_vertical_lists(_, Rows, _, [RowNum, _], [], [], [], []) :-
    is_in_vertical_border(Rows, RowNum).
restr_get_vertical_lists(Mat, Rows, _, [RowNum, ColNum], Up, Down, UpCoeff, DownCoeff) :-
    \+ is_in_vertical_border(Rows, RowNum),
    get_col(Mat, ColNum, Col),
    ColPrefix is RowNum,
    prefix_length(Col, Up, ColPrefix),
    ColSuffix is Rows - RowNum - 1,
    suffix_length(Col, Down, ColSuffix),
    reverse_filled_length(UpCoeff, ColPrefix),
    filled_length(DownCoeff, ColSuffix).

% Checks if a given list contains a Fulcrum
% Handles non-instantiated list
restr_contains_fulcrum(L, Domain, Output) :-
    count_line(L, Domain, Fulcrums, _, _),
    Fulcrums #> 0 #<=> Output.

% Applies the Fulcrum restriction to every cell using materialized restrictions
apply_fulcrum(_, _, _, [], _, _).
apply_fulcrum(Mat, Rows, Cols, [Row | RemainingMat], RowNum, Domain) :-
    apply_fulcrum_l(Mat, Rows, Cols, Row, RowNum, 0, Domain),
    NextRowNum is RowNum + 1,
    apply_fulcrum(Mat, Rows, Cols, RemainingMat, NextRowNum, Domain).

apply_fulcrum_l(_, _, _, [], _, _, _).
apply_fulcrum_l(Mat, Rows, Cols, [Cell | RemainingRow], RowNum, ColNum, Domain) :-
    Cell #= -1 #<=> IsFulcrum,
    
    restr_get_horizontal_lists(Mat, Rows, Cols, [RowNum, ColNum], Left, Right, LeftCoeff, RightCoeff),
    restr_get_vertical_lists(Mat, Rows, Cols, [RowNum, ColNum], Up, Down, UpCoeff, DownCoeff),
    scalar_product(LeftCoeff, Left, #=, L),
    scalar_product(RightCoeff, Right, #=, R),
    scalar_product(UpCoeff, Up, #=, U),
    scalar_product(DownCoeff, Down, #=, D),

    % Check if any sublist contains Fulcums
    restr_contains_fulcrum(Left, Domain, LeftHasFulcrum),
    restr_contains_fulcrum(Right, Domain, RightHasFulcrum),
    restr_contains_fulcrum(Up, Domain, UpHasFulcrum),
    restr_contains_fulcrum(Down, Domain, DownHasFulcrum),

    % Check if the cell is forced to be in a certain direction
    ForcedVertical #= (LeftHasFulcrum #\/ RightHasFulcrum),
    ForcedHorizontal #= (UpHasFulcrum #\/ DownHasFulcrum),
    NotForced #= ((#\ForcedHorizontal) #/\ (#\ForcedVertical)),

    % If it is a Fulcrum then it cannot be forced in both directions
    IsFulcrum #=> (#\(ForcedHorizontal #/\ ForcedVertical)),

    % Handles the cases where the fulcrum's direction is known
    (IsFulcrum #/\ ForcedVertical) #=> ((U #= D) #/\ (U #> 0)),
    (IsFulcrum #/\ ForcedHorizontal) #=> ((L #= R) #/\ (L #> 0)),

    % Whenever the direction is not forced, it must be used exclusively horizontally or vertically
    % We can check for 0 here, because we know there are no fulcrums in any of the sublists
    (IsFulcrum #/\ NotForced) #=> (
        ((U #= D) #/\ (U #> 0) #/\ (L #= 0) #/\ (R #= 0)) #\/
        ((L #= R) #/\ (L #> 0) #/\ (U #= 0) #/\ (D #= 0))
    ),

    NextColNum is ColNum + 1,
    apply_fulcrum_l(Mat, Rows, Cols, RemainingRow, RowNum, NextColNum, Domain).

