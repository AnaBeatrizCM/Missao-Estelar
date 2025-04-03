:- use_module(library(pce)).
:- consult('logicaJogador.pl').
:- consult('logicaInvasores.pl').
:- consult('controll.pl').

main :-
    new(Window, picture('Missao Estelar')),
    send(Window, size, size(400, 600)), 
    send(Window, scrollbars, none),
    send(Window, open),
    naveInicial(NaveInicial),
    estado_inicial_invasores(EstadoInvasores),
    renderizar_nave(Window, NaveInicial),
    renderizar_invasores(Window, EstadoInvasores),
    atirar(NaveInicial).

/*Criar um loop
Fazer nave andar
Fazer nave atirar
Adicionar 2 colunas de invasores
Fazer invasores andar
Fazer invasores atirarem
*/

renderizar_nave(Window, nave(_, _, X, Y)) :-
    nave(Window, X, Y).

renderizar_invasores(Window, estado_invasores(Invasores, _, _, _)) :-
    maplist(invasor(Window), Invasores).

renderizar_jogo(Window, Estado) :-
    limpar_tela(Window),
    renderizar_nave(Window, Estado),
    renderizar_invasores(Window, EstadoInvasores).

limpar_tela(Window) :-
    send(Window, clear).


