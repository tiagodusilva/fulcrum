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
    get_cardinality_list(Domain, CL),
    global_cardinality(Vars, CL),
    restrict_fulcrums(Mat, Rows, Cols, Fulcrums),

    % Labeling
    write('Labeling'), nl,
    labeling([], Vars),

    % Solution
    show_solution(Mat).


get_cardinality_list(0, [0-A]) :-
    A #>= 0.
get_cardinality_list(Domain, [Domain-1 | T]) :-
    Domain > 0,
    NextDomain is Domain - 1,
    get_cardinality_list(NextDomain, T).


get_cardinality_list_with_fulcrums(0, [0-A, -1-B]) :-
    A #>= 0,
    B #> 0.
get_cardinality_list_with_fulcrums(Domain, [Domain-1 | T]) :-
    Domain > 0,
    NextDomain is Domain - 1,
    get_cardinality_list_with_fulcrums(NextDomain, T).


generate :-
    % Rows is 6,
    % Cols is 8,
    % Domain is 6,
    Rows is 4,
    Cols is 4,
    Domain is 4,
    % Setup
    create_matrix(Rows, Cols, Mat),
    get_vars_mat(Mat, Vars),

    % Restrictions
    domain(Vars, -1, Domain),
    get_cardinality_list_with_fulcrums(Domain, CL),
    % trace,
    global_cardinality(Vars, CL),

    restrict_digit_count(Mat, Domain, Rows, Cols),

    find_fulcrums(Mat, Fulcrums),
    restrict_fulcrums(Mat, Rows, Cols, Fulcrums),

    % Labeling
    labeling([], Vars),
    

    % Solution
    show_solution(Mat).
