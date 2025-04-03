:- module(controll, [cap/1]).
:- use_module('logicaJogador').

cap('a') :- 
    nave(Vida, Vel, PosX, PosY),  
    Direcao is -3, 
    mover_nave_esq(nave(Vida, Vel, PosX, PosY), Direcao, NovaNave),
    retractall(nave(_, _, _, _)), 
    assertz(NovaNave).
    /*,write('Movendo para a esquerda'), nl.*/

cap('d') :- 
    nave(Vida, Vel, PosX, PosY),  
    Direcao is 3,  
    mover_nave_dir(nave(Vida, Vel, PosX, PosY), Direcao, NovaNave),
    retractall(nave(_, _, _, _)), 
    assertz(NovaNave).
    /*write('direita'), nl.*/

cap('w') :- 
    nave(Vida, Vel, PosX, PosY), 
    atirar(nave(Vida, Vel, PosX, PosY)).

cap(' ') :- 
    nave(Vida, Vel, PosX, PosY), 
    atirar(nave(Vida, Vel, PosX, PosY)).  