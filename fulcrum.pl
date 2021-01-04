:- use_module(library(clpfd)).


:- ensure_loaded('display.pl').
:- ensure_loaded('puzzles.pl').
:- ensure_loaded('preprocessing.pl').
:- ensure_loaded('restrictions.pl').
:- ensure_loaded('generate.pl').


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
    write('Labeling'), nl,
    labeling([], Vars),

    % Solution
    show_solution(Mat).

generate(Mat) :-
    Rows is 6,
    Cols is 8,
    Domain is 6,
    % Setup
    create_matrix(Rows, Cols, Mat),
    flatten(Mat, Vars),

    % Restrictions
    domain(Vars, -1, Domain),
    get_cardinality_list_with_fulcrums(Domain, CL),
    global_cardinality(Vars, CL),
    
    restrict_digit_count(Mat, Domain, Rows, Cols, RowCount, ColCount),
    restrict_cells_with_empty_col_and_row(Mat, RowCount, ColCount),

    write('Gonna apply'), nl,

    % trace,
    apply_fulcrum(Mat, Rows, Cols, Mat, 0, Domain),

    write('Done'), nl,

    % Labeling
    labeling([], Vars),

    % Solution
    show_solution(Mat).


