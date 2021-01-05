:- use_module(library(clpfd)).


:- ensure_loaded('display.pl').
:- ensure_loaded('puzzles.pl').
:- ensure_loaded('preprocessing.pl').
:- ensure_loaded('restrictions.pl').
:- ensure_loaded('generate.pl').
:- ensure_loaded('convert_mat_to_puzzle.pl').


reset_timer :- statistics(walltime,_).    
print_time :-
    statistics(walltime,[_,T]),
    format('Time ~3d sec.~n', [T]).
    % TS is ((T//10)*10)/1000,
    % nl, write('Time: '), write(T), write('s'), nl, nl.

puzzle(Sol) :-
    puzzle_73(Puzzle),
    puzzle(Puzzle, Sol).

puzzle(Puzzle, Mat) :-
    % Setup
    Puzzle = [Rows, Cols, Domain, Fulcrums],
    create_matrix(Rows, Cols, Mat),

    % Preprocessing
    get_fulcrum_lines_cols(Fulcrums, FLines, FCols),    % Get list of rows and cols that have fulcrums
    sort(FLines, UniqueFLines),
    sort(FCols, UniqueFCols),
    % trace,
    nullify_cells(Mat, UniqueFLines, UniqueFCols),  % Nullify cells with zero fulcrums in a row or in a column
    place_fulcrums(Mat, Fulcrums),  % Place fulcrums in the matrix
    nullify_cells_from_border_fulcrums(Mat, Rows, Cols, Fulcrums),  % Nullifies rows or columns when the fulcrum is in a border
    nullify_cells_from_multiple_fulcrums(Mat, FLines, FCols),   % Nullify cells on rows or columns that contain multiple fulcrums

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


generate(Puzzle) :-
    generate(6, 8, 6, 5, Puzzle).


generate(Rows, Cols, Domain, NoFulcrums, Puzzle) :-

    % Setup
    create_matrix(Rows, Cols, Mat),
    flatten(Mat, Vars),

    reset_timer,

    % Restrictions
    domain(Vars, -1, Domain),
    get_cardinality_list_with_fulcrums(Domain, NoFulcrums, CL),
    global_cardinality(Vars, CL),
    
    remove_some_symmetry(Mat, Rows, Cols),

    restrict_digit_count(Mat, Domain, Rows, Cols, RowCount, ColCount),
    force_full_sized_puzzle(RowCount, ColCount),
    restrict_cells_with_empty_col_and_row(Mat, RowCount, ColCount),

    % trace,
    apply_fulcrum(Mat, Rows, Cols, Mat, 0, Domain),

    % Labeling
    labeling([occurrence], Vars),

    print_time,

    % Solution
    show_solution(Mat),
    convert_mat_to_puzzle(Mat, Rows, Cols, Domain, Puzzle).
