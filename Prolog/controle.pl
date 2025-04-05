% controle.pl
:- consult('logica_jogador.pl').
:- dynamic nave/4.

tecla_pressionada('a') :-
    move_esquerda,
    desenhar_nave.

tecla_pressionada('d') :-
    move_direita,
    desenhar_nave.

tecla_pressionada('w') :-
    atirar(_),
    mostrar_tiros,
    desenhar_nave.

tecla_pressionada('SPC') :-
    write('espaco pressionado'), nl,  % DEBUG
    nave(V, Vel, X, Y),
    atirar(nave(V, Vel, X, Y)),
    mostrar_tiros,
    desenhar_nave.


tecla_pressionada(K) :-
    format('Tecla pressionada: ~w~n', [K]),
    true.


move_esquerda :-
    nave(V, Vel, X, Y),
    Direcao = -3,
    mover_nave_esq(nave(V, Vel, X, Y), Direcao, Nova),
    retract(nave(V, Vel, X, Y)),
    assert(Nova).

move_direita :-
    nave(V, Vel, X, Y),
    Direcao = 3,
    mover_nave_dir(nave(V, Vel, X, Y), Direcao, Nova),
    retract(nave(V, Vel, X, Y)),
    assert(Nova).

