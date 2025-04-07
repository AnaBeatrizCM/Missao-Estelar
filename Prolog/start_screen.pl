:- use_module(library(random)).
:- use_module(library(system)).

start_screen :-
    cls,
    show_intro(20),
    show_system_check,
    show_title,
    put_code(7), flush_output,
    write('                            Pressione [ENTER] para iniciar o jogo...'), nl,
    wait_for_enter,
    cls,
    start_game.

show_intro(0) :- !.
show_intro(N) :-
    random_bit_line(Line),
    write(Line), nl,
    sleep(0.05),
    N1 is N - 1,
    show_intro(N1).

random_bit_line(Line) :-
    random_bit_line(60, Line).

random_bit_line(0, "") :- !.
random_bit_line(N, Line) :-
    N > 0,
    random_member(Bit, [0'0, 0'1]),
    char_code(Char, Bit),
    N1 is N - 1,
    random_bit_line(N1, Rest),
    atom_concat(Char, Rest, Line).

show_system_check :-
    nl,
    write('       [OK] Power System ............ online'), nl,
    sleep(0.2),
    write('       [OK] Weapon System ........... armed'), nl,
    sleep(0.2),
    write('       [OK] Navigation System ....... locked'), nl,
    sleep(0.2),
    write('       [OK] Communication Link ...... active'), nl,
    sleep(0.3),
        nl.


show_title :-
    write('            ██████╗  █████╗  ██╗      █████╗ ████████╗ ██╗ ██████╗  ██████╗ ███████╗'), nl,
    write('            ██╔════╝ ██╔══██╗██║     ██╔══██╗╚══██╔══╝██║██╔════╝ ██╔═══██╗██╔════╝'), nl,
    write('            ██║  ███╗███████║██║     ███████║   ██║   ██║██║      ██║   ██╗███████╗'), nl,
    write('            ██║   ██║██╔══██║██║     ██╔══██║   ██║   ██║██║   ║  ██║   ██╔══╝  ██ '), nl,
    write('            ╚██████╔╝██║  ██║███████╗██║  ██║   ██║   ██║╚██████╔╝╚██████╔╝███████╗╗'), nl,
    write('             ╚═════╝ ╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝   ╚═╝   ╚═╝ ╚═════╝  ╚═════╝ ╚══════╝╚'), nl,
    write('                                          SPACE VANGERS'), nl,
    nl.

wait_for_enter :-
    get_char('\n').

cls :-
    (   current_prolog_flag(unix, true)
    ->  shell('clear')
    ;   shell('cls')
    ).

% Substitua esta por seu jogo real
default_game :- write('Iniciando o jogo...
').

start_game :-
    default_game.