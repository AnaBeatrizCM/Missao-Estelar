:- module(logicaInvasores, [invasor/2, estado_inicial_invasores/1, atualizar_invasores/2]).

:- dynamic invasor/3.  % invasor(X, Y, Velocidade)
:- dynamic tiro/3.     % tiro(X, Y, Velocidade)
:- dynamic estado_invasores/4. 

% Constantes
largura_janela(400).
altura_janela(600).

invasor(Window, invasor(X, Y, _)) :-
    new(Invasor, box(40, 30)),
    send(Invasor, fill_pattern, colour(green)),
    send(Window, display, Invasor, point(X, Y)).

%Estado inicial
estado_inicial_invasores(Estado) :-
    findall(invasor(X, Y, 2), 
           (member(X, [-330, -255, -180, -105, 30, 105, 180, 255, 330]), 
            member(Y, [200, 150, 100])),
           Invasores),
    Estado = estado_invasores(Invasores, 1, 2, []).

atualizar_invasores(Estado, Estado).

%mover Invasores
mover_invasores(DT, Estado, NovoEstado) :-
    Estado = estado_invasores(Invs, Dir, Tempo, Tiros),
    maplist(ajustar_posicao(DT, Dir), Invs, NovosInvasores),
    ajustar_movimento(DT, NovosInvasores, Dir, InvasoresAjustados, NovaDir),
    NovoEstado = estado_invasores(InvasoresAjustados, NovaDir, Tempo, Tiros).

ajustar_posicao(DT, Dir, invasor(X, Y, V), invasor(NovoX, Y, V)) :-
    NovoX is X + V * Dir * DT * 50.

ajustar_movimento(DT, Invs, Dir, InvasoresAjustados, NovaDir) :-
    algum_invasor_fora_da_tela(Invs) ->
        maplist(descer_invasor(DT), Invs, InvsDescidos),
        (algum_invasor_muito_baixo(InvsDescidos) ->
            InvasoresAjustados = Invs,
            NovaDir is -Dir
        ;
            InvasoresAjustados = InvsDescidos,
            NovaDir is -Dir
        )
    ;
        InvasoresAjustados = Invs,
        NovaDir = Dir.

algum_invasor_fora_da_tela(Invs) :-
    largura_janela(Largura),
    MeiaLargura is Largura / 2,
    member(invasor(X, _, _), Invs),
    (X =< -MeiaLargura ; X >= MeiaLargura).

algum_invasor_muito_baixo(Invs) :-
    altura_janela(Altura),
    MeiaAltura is Altura / 2,
    member(invasor(_, Y, _), Invs),
    Y =< (-MeiaAltura + 50).

descer_invasor(DT, invasor(X, Y, V), invasor(X, NovoY, V)) :-
    NovoY is Y - 10 * DT.

%Disparar Invasores
disparar_invasores(Estado, Tiros, NovoEstado) :-
    Estado = estado_invasores(Invs, Dir, Tempo, TirosAtuais),
    (Tempo =< 0 ->
        escolher_invasor_aleatorio(Invs, InvasorAleatorio),
        InvasorAleatorio = invasor(X, Y, _),
        NovoTiro = tiro(X, Y, 5),
        Tiros = [NovoTiro],
        NovoEstado = estado_invasores(Invs, Dir, 2, [NovoTiro | TirosAtuais])
    ;
        Tiros = [],
        NovoTempo is Tempo - 0.1,
        NovoEstado = estado_invasores(Invs, Dir, NovoTempo, TirosAtuais)
    ).

escolher_invasor_aleatorio(Invs, InvasorAleatorio) :-
    length(Invs, N),
    N > 0,
    random(0, N, Indice),
    nth0(Indice, Invs, InvasorAleatorio).

%Verificar colisões
verificar_colisoes_tiros_invasores(Tiros, Invasores, TirosRestantes, InvasoresRestantes) :-
    filtrar_tiros_sem_colisao(Tiros, Invasores, TirosRestantes),
    filtrar_invasores_sem_colisao(Invasores, Tiros, InvasoresRestantes).

filtrar_tiros_sem_colisao([], _, []).
filtrar_tiros_sem_colisao([Tiro|Resto], Invasores, Filtrados) :-
    colide_com_invasor(Tiro, Invasores) ->
        filtrar_tiros_sem_colisao(Resto, Invasores, Filtrados)
    ;
        Filtrados = [Tiro|Outros],
        filtrar_tiros_sem_colisao(Resto, Invasores, Outros).

filtrar_invasores_sem_colisao([], _, []).
filtrar_invasores_sem_colisao([Inv|Resto], Tiros, Filtrados) :-
    colide_com_tiro(Inv, Tiros) ->
        filtrar_invasores_sem_colisao(Resto, Tiros, Filtrados)
    ;
        Filtrados = [Inv|Outros],
        filtrar_invasores_sem_colisao(Resto, Tiros, Outros).

colide_com_invasor(tiro(TX, TY, _), Invasores) :-
    member(invasor(IX, IY, _), Invasores),
    abs(TX - IX) < 20,
    abs(TY - IY) < 20.

colide_com_tiro(invasor(IX, IY, _), Tiros) :-
    member(tiro(TX, TY, _), Tiros),
    abs(TX - IX) < 20,
    abs(TY - IY) < 20.

%Atualizar Invasores
atualizar_invasores(DT, Estado, NovoEstado, TirosInimigos) :-
    mover_invasores(DT, Estado, EstadoMovido),
    disparar_invasores(EstadoMovido, TirosInimigos, NovoEstado).       