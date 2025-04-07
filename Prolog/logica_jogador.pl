% logica_jogador.pl
:- dynamic nave/4.
:- dynamic tiros_ativos/1.
:- dynamic jogo_acabado/0.

% Estado inicial da nave: vida, velocidade, X, Y
nave(3, 4, 175, 0).
tiros_ativos([]).

% Predicado para tomar dano // MEXI

tomar_dano(nave(Vida, Vel, PosX, PosY), nave(NovaVida, Vel, PosX, PosY)) :-
    verifica_vida(Vida),
    (Vida - 1 =< 0 ->
        NovaVida is 0,
        assertz(jogo_acabado),
        game_over();
        NovaVida is Vida - 1).

% Mensagem de game over     MEXI AQUI


game_over :-

    janela(J),
    send(J, clear),

    new(Window, dialog('Game Over')),

    new(Msg, text('Game Over!', center)),
    send(Msg, font, font(times, bold, 40)),
    send(Msg, colour, red),
    send(Window, append, Msg),
    send(Window, open).

% Criando predicado para ganho

you_win :-
    janela(J),
    animacao(Timer),
    send(Timer, stop),
    retractall(animacao(_)),
    send(J, clear),

    new(Window, dialog('You Win!')),

    new(Msg, text('You Win!', center)),
    send(Msg, font, font(times, bold, 40)),
    send(Msg, colour, green),
    send(Window, append, Msg),
    send(Window, open), !.


% Verifica se a vida é maior que zero
verifica_vida(Vida) :-
    Vida > 0.

% Verifica se a nave não saiu pela esquerda
verifica_extremo_esq_nave(PosX, Vel, Direcao) :-
    (PosX + (Vel * Direcao)) >= 30.

% Verifica se a nave não saiu pela direita
verifica_extremo_dir_nave(PosX, Vel, Direcao) :-
    (PosX + (Vel * Direcao)) =< 320. % Largura da tela

% Move para a esquerda
mover_nave_esq(nave(Vida, Vel, PosX, PosY), Direcao, nave(Vida, Vel, NovaPosX, PosY)) :-
    (verifica_extremo_esq_nave(PosX, Vel, Direcao) ->
        NovaPosX is PosX + (Vel * Direcao);
        NovaPosX is 30).

% Move para a direita
mover_nave_dir(nave(Vida, Vel, PosX, PosY), Direcao, nave(Vida, Vel, NovaPosX, PosY)) :-
    (verifica_extremo_dir_nave(PosX, Vel, Direcao) ->
        NovaPosX is PosX + (Vel * Direcao);
        NovaPosX is 320).

% Atira
atirar(nave(Vida, _, PosX, _)) :-
    verifica_vida(Vida),
    tiros_ativos(TirosAtuais),
    NovoTiro = tiro(PosX, 115, 8),
    retractall(tiros_ativos(_)),
    assertz(tiros_ativos([NovoTiro|TirosAtuais])).

% Atualiza posição dos tiros
atualizar_tiros :-
    tiros_ativos(TirosAtuais),
    findall(
        tiro(PosX, NovaPosY, Vel),
        (member(tiro(PosX, PosY, Vel), TirosAtuais),
         NovaPosY is PosY + Vel,
         NovaPosY =< 600),
        NovosTiros),
    retractall(tiros_ativos(_)),
    assertz(tiros_ativos(NovosTiros)).


