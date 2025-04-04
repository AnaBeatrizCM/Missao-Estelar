:- module(controll, [cap/1]).
:- use_module(logicaJogador).
:- use_module(main, [jogo_estado/3, renderizar_jogo/3]).


cap('a') :- 
    jogo_estado(Window, Nave, Invasores),
    mover_nave_esq(Nave, NovaNave),
    atualizar_estado(Window, NovaNave, Invasores).

cap('d') :- 
    jogo_estado(Window, Nave, Invasores),
    mover_nave_dir(Nave, NovaNave),
    atualizar_estado(Window, NovaNave, Invasores).

cap('w') :- 
    jogo_estado(Window, Nave, Invasores),
    atirar(Nave),
    atualizar_estado(Window, Nave, Invasores).
  
cap(' ') :- 
    nave(Vida, Vel, PosX, PosY), 
    atirar(nave(Vida, Vel, PosX, PosY)).  

tecla_pressionada(Key) :-
    atom_string(AtomKey, Key),
    cap(AtomKey).
