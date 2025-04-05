% logica_jogador.pl
:- dynamic nave/4.
:- dynamic tiros_ativos/1.

% Estado inicial da nave: vida, velocidade, X, Y
nave(3, 4, 175, 0).
tiros_ativos([]).

% Predicado para tomar dano
tomar_dano(nave(Vida, Vel, PosX, PosY), nave(NovaVida, Vel, PosX, PosY)) :-
    verifica_vida(Vida),
    (Vida - 1 =< 0 ->
        NovaVida is 0,
        game_over();
        NovaVida is Vida - 1).

% Mensagem de game over
game_over() :-
    write("Você perdeu! 💔"), nl.

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

% Mostra tiros ativos
mostrar_tiros :-
    tiros_ativos(Tiros),
    write("Tiros ativos: "), write(Tiros), nl.
