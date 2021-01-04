% Condensed Display
show_condensed_solution([]).
show_condensed_solution([H | T]) :-
    show_solution_line(H), nl,
    show_condensed_solution(T).

show_solution_line([]) :- write('|').
show_solution_line([H | T]) :-
    var(H),
    write('? '),
    show_solution_line(T).
show_solution_line([-1 | T]) :-
    put_code(9632), write(' '),
    show_solution_line(T).
show_solution_line([0 | T]) :-
    write('  '),
    show_solution_line(T).
show_solution_line([H | T]) :-
    \+ var(H),
    H #\= 0,
    H #\= -1,    
    write(H), write(' '),
    show_solution_line(T).


% Full Display
write_cell(H) :-
    var(H),
    write(' ?').
write_cell(0) :-
    write('  ').
write_cell(-1) :-
    put_code(9632),
    put_char(' ').
write_cell(H) :-
    \+ var(H),
    H #\= 0,
    H #\= -1,    
    print_number_with_padding(H, 32).

put_code_n_times(_, 0).
put_code_n_times(Code, Times) :-
    put_code(Code),
    NextTimes is Times - 1,
    put_code_n_times(Code, NextTimes).

put_char_n_times(_, 0).
put_char_n_times(Char, Times) :-
    put_char(Char),
    NextTimes is Times - 1,
    put_char_n_times(Char, NextTimes).

print_number_with_padding(Number, _) :-
    Number > 10,
    X is Number // 10,
    Y is Number rem 10,
    digit_code(X, Code1),
    digit_code(Y, Code2),
    put_code(Code1),
    put_code(Code2).
print_number_with_padding(Number, StuffCharCode) :-
    digit_code(Number, Code1),
    put_code(Code1),
    put_code(StuffCharCode).

digit_code(0, 48).
digit_code(1, 49).
digit_code(2, 50).
digit_code(3, 51).
digit_code(4, 52).
digit_code(5, 53).
digit_code(6, 54).
digit_code(7, 55).
digit_code(8, 56).
digit_code(9, 57).


show_solution([FirstRow | Board]) :-
    put_code(9484),
    print_line_top(FirstRow), nl,
    print_board([FirstRow | Board]),
    put_code(9492), print_line_bot(FirstRow).

print_board([Row]) :-
    put_code(9474),
    print_line(Row), nl.
print_board([Row | Board]) :-
    put_code(9474),
    print_line(Row), nl,
    put_code(9500),
    print_line_sep(Row), nl,
    print_board(Board).

print_line_sep([_]) :- put_code_n_times(9472, 3), put_code(9508).
print_line_sep([_ | Line]) :-
    put_code_n_times(9472, 3),
    put_code(9532),
    print_line_sep(Line).

print_line_top([_]) :- put_code_n_times(9472, 3), put_code(9488).
print_line_top([_ | Line]) :-
    put_code_n_times(9472, 3),
    put_code(9516),
    print_line_top(Line).

print_line_bot([_]) :- put_code_n_times(9472, 3), put_code(9496).
print_line_bot([_ | Line]) :-
    put_code_n_times(9472, 3),
    put_code(9524),
    print_line_bot(Line).

print_line([]).
print_line([Cell | Line]) :-
    put_char(' '),
    write_cell(Cell),
    put_code(9474),
    print_line(Line).


% boundary(vert, 9474).
% boundary(hor, 9472).
% boundary(cross, 9532).
% boundary(vert_right, 9500).
% boundary(vert_left, 9508).
% boundary(top_right, 9488).
% boundary(top_left, 9484).
% boundary(bot_right, 9496).
% boundary(bot_left, 9492).
