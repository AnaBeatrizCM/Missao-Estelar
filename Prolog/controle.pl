% controle.pl
:- consult('logica_jogador.pl').
:- dynamic nave/4.

tecla_pressionada(_) :-
    jogo_acabado, !.

tecla_pressionada('a') :-
    \+ jogo_acabado,
    move_esquerda,
    desenhar_nave.

tecla_pressionada('d') :-
    \+ jogo_acabado,
    move_direita,
    desenhar_nave.


tecla_pressionada('SPC') :-

    \+ jogo_acabado,
    nave(V, Vel, X, Y),
    atirar(nave(V, Vel, X, Y)),
    desenhar_nave.





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

