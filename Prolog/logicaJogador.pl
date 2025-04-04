:- module(logicaJogador, [nave/4, naveInicial/1, nave/3, mover_nave_dir/3, 
    mover_nave_esq/3, atirar/1, atualizar_nave/2]).
:- consult('controll').

/* Definição do estado inicial da nave
(vida, velocidade, PosX, posY) */
nave(3, 4, 200, 600).

/* Definição do tiro que sai da nave
(PosX, PosY, velocidade). */

:- dynamic nave/4.

:- dynamic tiros_ativos/1.
tiros_ativos([]).

:- dynamic jogo_ativo/1.
jogo_ativo(sim).

nave(Window, X, Y) :-
    new(Nave, box(80, 30)),
    send(Nave, fill_pattern, colour(blue)),
    send(Window, display, Nave, point(X, Y)).

naveInicial(nave(3, 4, 200, 600)).

atualizar_nave(Nave, Nave).
% Predicado para tomar dano
tomar_dano(nave(Vida, Vel, PosX, PosY), nave(NovaVida, Vel, PosX, PosY)) :-
    verifica_vida(Vida),
    (Vida - 1 =< 0 -> 
        NovaVida is 0, 
        game_over(), nl; 
        NovaVida is Vida - 1).

% Predicado para finalizar o jogo quando a vida chega a zero
game_over() :-
    write("Você perdeu! 💔"), nl,
    retractall(jogo_ativo(_)), 
    assertz(jogo_ativo(nao)).

% Verifica se a vida é maior que zero
verifica_vida(Vida) :-
    Vida > 0.

/*Verifica se a nave não ultrapassou o limite esquerdo
verifica_extremo_esq_nave(PosX, Vel, Direcao) :-
    (PosX + (Vel * Direcao)) >= 0.

% Verifica se a nave não ultrapassou o limite direito
verifica_extremo_dir_nave(PosX, Vel, Direcao) :-
    (PosX + (Vel * Direcao)) =< 600. % Largura da tela 800
*/
% Movimenta a nave para a esquerda
mover_nave_esq(nave(Vida, Vel, PosX, PosY), nave(Vida, Vel, NovaPosX, PosY)) :-
    NovaPosX is max(0, PosX - Vel).

% Movimenta a nave para a direita
mover_nave_dir(nave(Vida, Vel, PosX, PosY), Direcao, nave(Vida, Vel, NovaPosX, PosY)) :-
    NovaPosX is min(600, PosX + Vel).

atirar(nave(Vida, _, PosX, _)):-
    verifica_vida(Vida),
    tiros_ativos(TirosAtuais),
    NovoTiro = tiro(PosX, 30, 8), % PosX, PosY, vel.
    retractall(tiros_ativos(_)),
    assertz(tiros_ativos([NovoTiro|TirosAtuais])).

atualizar_tiros:-
    tiros_ativos(TirosAtuais),
    findall(
        tiro(PosX, NovaPosY, Vel),
        (member(tiro(PosX, PosY, Vel), TirosAtuais),
            NovaPosY is PosY + Vel,
            NovaPosY =< 800
        ),
        NovosTiros),
    retractall(tiros_ativos(_)),
    assertz(tiros_ativos(NovosTiros)).

            
