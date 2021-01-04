:- use_module(library(clpfd)).


:- ensure_loaded('display.pl').
:- ensure_loaded('puzzles.pl').
:- ensure_loaded('preprocessing.pl').
:- ensure_loaded('restrictions.pl').
:- ensure_loaded('generate.pl').


reset_timer :- statistics(walltime,_).    
print_time :-
    statistics(walltime,[_,T]),
    format('Time ~3d sec.~n', [T]).
    % TS is ((T//10)*10)/1000,
    % nl, write('Time: '), write(T), write('s'), nl, nl.


solve(Mat) :-
    % Setup
    puzzle(Rows, Cols, Domain, Fulcrums),
    create_matrix(Rows, Cols, Mat),

    % Preprocessing
    get_fulcrum_lines_cols(Fulcrums, FLines, FCols),
    sort(FLines, UniqueFLines),
    sort(FCols, UniqueFCols),
    % trace,
    nullify_cells(Mat, UniqueFLines, UniqueFCols),
    place_fulcrums(Mat, Fulcrums),
    nullify_cells_from_border_fulcrums(Mat, Rows, Cols, Fulcrums),
    nullify_cells_from_multiple_fulcrums(Mat, FLines, FCols),

    % Apply domain
    get_vars_mat(Mat, Vars),
    domain(Vars, 0, Domain),

    % Restrictions
    get_cardinality_list(Domain, CL),
    global_cardinality(Vars, CL),
    restrict_fulcrums(Mat, Rows, Cols, Fulcrums),

    % Labeling
    labeling([], Vars),

    % Solution
    show_solution(Mat).



generate(Mat) :-
    Rows is 6,
    Cols is 8,
    Domain is 6,
    NoFulcrums is 5,
    % Setup
    create_matrix(Rows, Cols, Mat),
    flatten(Mat, Vars),

    reset_timer,

    % Restrictions
    domain(Vars, -1, Domain),
    get_cardinality_list_with_fulcrums(Domain, NoFulcrums, CL),
    global_cardinality(Vars, CL),
    

    restrict_digit_count(Mat, Domain, Rows, Cols, RowCount, ColCount),
    force_full_sized_puzzle(RowCount, ColCount),
    restrict_cells_with_empty_col_and_row(Mat, RowCount, ColCount),

    % trace,
    apply_fulcrum(Mat, Rows, Cols, Mat, 0, Domain),

    % Labeling
    labeling([occurrence], Vars),

    print_time,

    % Solution
    show_solution(Mat).

