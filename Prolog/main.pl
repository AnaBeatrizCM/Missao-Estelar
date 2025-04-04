:- use_module(library(pce)).
:- consult('logicaJogador.pl').
:- consult('logicaInvasores.pl').
:- consult('controll.pl').

:- dynamic jogo_estado/3.

main :-
    new(Window, picture('Missao Estelar')),
    send(Window, size, size(400, 600)), 
    send(Window, scrollbars, none),
    send(Window, open),

    naveInicial(NaveInicial),
    estado_inicial_invasores(EstadoInvasores),
    assertz(jogo_estado(Window, NaveInicial, EstadoInvasores)),

    renderizar_nave(Window, NaveInicial),
    renderizar_invasores(Window, EstadoInvasores),

    new(Binding, key_binding(my_keys)),
    send(Binding, function, 'a', message(@prolog, tecla_pressionada, 'a')),
    send(Binding, function, 'd', message(@prolog, tecla_pressionada, 'd')),
    send(Binding, function, 'w', message(@prolog, tecla_pressionada, 'w')),
    send(Binding, function, ' ', message(@prolog, tecla_pressionada, ' ')),
    send(Window, recogniser, Binding),

    atirar(NaveInicial),
    loop.

/*Criar um loop
Fazer nave andar
Fazer nave atirar
Fazer invasores andar
Fazer invasores atirarem
*/

loop :-
    new(Timer, timer(0.05, message(@prolog, atualizar_jogo))),
    send(Timer, start).

renderizar_nave(Window, nave(_, _, X, Y)) :-
    desenho_nave(Window, X, Y).

renderizar_invasores(Window, estado_invasores(Invasores, _, _, _)) :-
    maplist(invasor(Window), Invasores).

renderizar_jogo(Window, Estado) :-
    limpar_tela(Window),
    renderizar_nave(Window, Nave),
    renderizar_invasores(Window, EstadoInvasores).

atualizar_jogo :-
    jogo_estado(Window, Nave, EstadoInvasores),
    object(Window), !,

    atualizar_nave(Nave, NovaNave),
    atualizar_invasores(EstadoInvasores, NovoEstadoInvasores),
    
    retract(jogo_estado(_, _, _)),
    assertz(jogo_estado(Window, NovaNave, NovoEstadoInvasores)),
    
    limpar_tela(Window),
    renderizar_nave(Window, NovaNave),
    renderizar_invasores(Window, NovoEstadoInvasores).

atualizar_jogo :- 
    retractall(jogo_estado(_, _, _)).

tecla_pressionada(Tecla) :-
    cap(Tecla).

limpar_tela(Window) :-
    send(Window, clear).


