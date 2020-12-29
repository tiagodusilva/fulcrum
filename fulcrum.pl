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
    domain(Vars, 1, Domain),

    % Restrictions
    

    % Solution
    show_solution(Mat).

