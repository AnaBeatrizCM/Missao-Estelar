% inimigos.pl
:- dynamic invasores_ativos/1.
:- dynamic tiros_inimigos_ativos/1.

invasores_ativos([
    invasor(50, 100, 2), invasor(100, 100, 2), invasor(150, 100, 2), invasor(200, 100, 2), invasor(250, 100, 2),
    invasor(50, 140, 2), invasor(100, 140, 2), invasor(150, 140, 2), invasor(200, 140, 2), invasor(250, 140, 2),
    invasor(50, 180, 2), invasor(100, 180, 2), invasor(150, 180, 2), invasor(200, 180, 2), invasor(250, 180, 2)
]).

tiros_inimigos_ativos([]).

desenhar_invasores :-
    janela(Window),
    invasores_ativos(Invasores),
    forall(
        member(invasor(X, Y, _), Invasores),
        (
            new(Shape, box(20, 20)),
            send(Shape, fill_pattern, colour(green)),
            send(Window, display, Shape, point(X, Y))
        )
    ).

mover_invasores :-
    invasores_ativos(Invasores),
    (   precisa_mudar_direcao(Invasores)
    ->  mudar_direcao_e_descer
    ;   mover_invasores_horizontal
    ).

precisa_mudar_direcao(Invasores) :-
    (   member(invasor(X, _, Dir), Invasores),
        (X =< 0, Dir =:= -2 ; X >= 330, Dir =:= 2)
    ), !.

mover_invasores_horizontal :-
    invasores_ativos(Antigos),
    findall(invasor(NX, Y, Dir), (
        member(invasor(X, Y, Dir), Antigos),
        NX is X + Dir
    ), Novos),
    retractall(invasores_ativos(_)),
    assertz(invasores_ativos(Novos)).

mudar_direcao_e_descer :-
    invasores_ativos(Antigos),
    findall(invasor(X, NY, NDir), (
        member(invasor(X, Y, Dir), Antigos),
        NY is Y + 20,
        NDir is -Dir
    ), Novos),
    retractall(invasores_ativos(_)),
    assertz(invasores_ativos(Novos)).

% move os tiros inimigos para baixo e remove os que saíram da tela
atualizar_tiros_inimigos :-
    tiros_inimigos_ativos(Tiros),
    findall(tiro_inimigo(X, NY, V),
        (
            member(tiro_inimigo(X, Y, V), Tiros),
            NY is Y + V,
            NY =< 600
        ),
        Atualizados
    ),
    retractall(tiros_inimigos_ativos(_)),
    assertz(tiros_inimigos_ativos(Atualizados)).



% 10% de chance de disparo por frame
disparar_inimigo :-
    maybe(0.1),
    invasores_ativos(Invasores),
    random_member(invasor(X, Y, _), Invasores),
    adicionar_tiro_inimigo(X, Y).

% chance aleatória de disparo inimigo
talvez_disparar :-
    random(0.0, 1.0, R),
    R < 0.05,
    invasores_ativos(Invasores),
    random_member(invasor(X, Y, _), Invasores),
    tiros_inimigos_ativos(Tiros),
    append(Tiros, [tiro_inimigo(X, Y, 5)], Novos),
    retractall(tiros_inimigos_ativos(_)),
    assertz(tiros_inimigos_ativos(Novos)).
talvez_disparar.


adicionar_tiro_inimigo(X, Y) :-
    tiros_inimigos_ativos(Atual),
    append(Atual, [tiro_inimigo(X, Y, 8)], Novo),
    retractall(tiros_inimigos_ativos(_)),
    assertz(tiros_inimigos_ativos(Novo)).

maybe(P) :- random(R), R < P.

desenhar_tiros_inimigos :-
    janela(Window),
    tiros_inimigos_ativos(Tiros),
    forall(
        member(tiro_inimigo(X, Y, _), Tiros),
        (
            new(Bala, circle(5)),
            send(Bala, fill_pattern, colour(blue)),
            send(Bala, move, point(X, Y)),
            send(Window, display, Bala)
        )
    ).
verificar_colisoes_tiros :-
    tiros_ativos(Tiros),
    invasores_ativos(Invasores),
    findall(tiro(XT, YT, V),
        (
            member(tiro(XT, YT, V), Tiros),
            \+ (
                member(invasor(XI, YI, _), Invasores),
                XT >= XI, XT =< XI + 20,
                600 - YT >= YI, 600 - YT =< YI + 20
            )
        ),
        TirosFiltrados),
    findall(invasor(XI, YI, D),
        (
            member(invasor(XI, YI, D), Invasores),
            \+ (
                member(tiro(XT, YT, _), Tiros),
                XT >= XI, XT =< XI + 20,
                600 - YT >= YI, 600 - YT =< YI + 20
            )
        ),
        InvasoresFiltrados),
    retractall(tiros_ativos(_)),
    assertz(tiros_ativos(TirosFiltrados)),
    retractall(invasores_ativos(_)),
    assertz(invasores_ativos(InvasoresFiltrados)).

