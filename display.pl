show_solution([]).
show_solution([H | T]) :-
    show_solution_line(H), nl,
    show_solution(T).

show_solution_line([]) :- write('|').
show_solution_line([H | T]) :-
    var(H),
    write('? '),
    show_solution_line(T).
show_solution_line([-1 | T]) :-
    put_code(9650), write(' '),
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