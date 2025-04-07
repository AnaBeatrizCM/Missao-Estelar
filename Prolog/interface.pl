% interface.pl
:- use_module(library(pce)).
:- consult('logica_jogador.pl').
:- consult('controle.pl').
:- consult('inimigos.pl').
:- consult('start_screen.pl').
:- initialization(start_screen).

:- dynamic animacao/1.
:- dynamic janela/1.

start_game :-
    Width = 350,
    Height = 600,
    new(Window, picture('Missao Estelar')),
    send(Window, scrollbars, none),
    send(Window, size, size(Width, Height)),
    send(Window, open),
    assertz(janela(Window)),
    desenhar_nave,
    send(Window, recogniser,
        handler(keyboard, message(@prolog, tecla_pressionada, @event?key))),
        send(Window, focus, Window),
        iniciar_animacao.
% Desenha a nave na posição atual
desenhar_nave :-
    janela(Window),
    nave(_, _, X, _),
    get(Window, size, size(_, Height)),
    new(NaveImg, bitmap('th.jpg')),
    get(NaveImg, width, IW),
    get(NaveImg, height, IH),
    XDraw is X - IW // 2,
    YDraw is Height - IH,
    send(Window, display, NaveImg, point(XDraw, YDraw)).


% Desenha todos os tiros na tela
desenhar_tiros :-
    janela(Window),
    tiros_ativos(Tiros),
    forall(
        member(tiro(X, Y, _), Tiros),
        (
            YTop is 600 - Y,
            YBottom is YTop - 15,
            new(Laser, line(X, YTop, X, YBottom)),
            send(Laser, colour, red),
            send(Laser, pen, 3),  % ← espessura da linha (padrão é 1)
            send(Window, display, Laser)
        )
    ).


iniciar_animacao :-
    new(Timer, timer(0.05, message(@prolog, atualizar_tela))),
    assertz(animacao(Timer)),
    send(Timer, start).


atualizar_tela :-
    atualizar_tiros,
    atualizar_tiros_inimigos,
    verificar_colisoes_tiros,
    verificar_colisoes_tiros_inimigos,
    talvez_disparar,
    mover_invasores,
    redesenhar_tiros.


redesenhar_tiros :-
    janela(Janela),
    send(Janela, clear),            % Limpa a tela
    desenhar_nave,
    desenhar_invasores,
    desenhar_tiros,                 % desenha os tiros da nave com forma de linha
    desenhar_tiros_inimigos.       % mantém os tiros inimigos como estão

criar_janela :-
    new(J, window('Missao Estelar', size(400, 600))),
    assertz(janela(J)),
    send(J, open).


