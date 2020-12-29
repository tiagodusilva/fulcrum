:- use_module(library(clpfd)).


:- ensure_loaded('display.pl').
:- ensure_loaded('puzzles.pl').
:- ensure_loaded('preprocessing.pl').
:- ensure_loaded('restrictions.pl').


solve :-
    % Setup
    puzzle(Rows, Cols, Domain, Fulcrums),
    create_matrix(Rows, Cols, Mat),

    % Preprocessing
    get_fulcrum_lines_cols(Fulcrums, FLines, FCols),
    nullify_cells(Mat, FLines, FCols),
    place_fulcrums(Mat, Fulcrums),
    nullify_cells_from_border_fulcrums(Mat, Rows, Cols, Fulcrums),
    nullify_cells_from_multiple_fulcrums(Mat, FLines, FCols),

    % Apply domain
    get_vars_mat(Mat, Vars),
    domain(Vars, 0, Domain),

    % Restrictions
    all_distinct_except_0(Vars),
    nvalue(N, Vars),
    N #= Domain #\/ N #= (Domain + 1),
    restrict_fulcrums(Mat, Rows, Cols, Fulcrums),

    % Labeling
    write('Labeling'), nl,
    labeling([], Vars),

    % Solution
    show_solution(Mat).

