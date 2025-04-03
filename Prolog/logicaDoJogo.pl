% Definição do estado do jogo
estado_jogo(jogador(EstadoJogador), invasores(EstadoInvasores), pontuacao(0), nivel(1), jogo_ativo(true)).

% Perder vida e verificar fim de jogo
perder_vida(EstadoJogo, NovoEstadoJogo) :-
    EstadoJogo = estado_jogo(jogador(EstadoJogador), invasores(Invasores), pontuacao(Pontos), nivel(Nivel), jogo_ativo(Ativo)),
    EstadoJogador = jogador(Vidas, OutrosDadosJogador),
    (   Vidas > 1
    ->  NovaVida is Vidas - 1,
        NovoEstadoJogo = estado_jogo(jogador(NovaVida, OutrosDadosJogador), invasores(Invasores), pontuacao(Pontos), nivel(Nivel), jogo_ativo(Ativo))
    ;   NovoEstadoJogo = estado_jogo(jogador(0, OutrosDadosJogador), invasores(Invasores), pontuacao(Pontos), nivel(Nivel), jogo_ativo(false))
    ).

% Verificar se o jogo ainda está ativo
verificar_fim_jogo(estado_jogo(_, _, _, _, jogo_ativo(Ativo)), Ativo).