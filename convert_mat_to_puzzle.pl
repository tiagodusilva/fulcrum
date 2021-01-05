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


convert_mat_to_puzzle(Mat, Rows, Cols, Domain, [Rows, Cols, Domain, Fulcrums]) :-
    find_fulcrums(Mat, Fulcrums).
