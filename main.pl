
:-dynamic posicao/3.
:-dynamic memory/3.
:-dynamic visitado/2.
:-dynamic certeza/2.
:-dynamic energia/1.
:-dynamic pontuacao/1.
:-dynamic ourosColetados/1.
:-dynamic safe_positions/1.
:-dynamic energy_positions/1.

:-consult('mapa.pl').

delete([], _, []).
delete([Elem|Tail], Del, Result) :-
    (   \+ Elem \= Del
    ->  delete(Tail, Del, Result)
    ;   Result = [Elem|Rest],
        delete(Tail, Del, Rest)
    ).

imprime_lista([]).
imprime_lista([H|T]) :-
    writeln(H),
    imprime_lista(T).

reset_game :- retractall(memory(_,_,_)), 
			retractall(visitado(_,_)), 
			retractall(certeza(_,_)),
			retractall(energia(_)),
			retractall(pontuacao(_)),
            retractall(safe_positions(_)),
            retractall(energy_positions(_)),
			retractall(posicao(_,_,_)),
			assert(energia(100)),
            assert(ourosColetados(0)),
			assert(pontuacao(0)),
			assert(posicao(1,1, norte)),
            assert(energy_positions([])),
            assert(safe_positions([(1,1)])).




:-reset_game.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

vizinho((X,Y), (X1,Y)) :-
    map_size(MAX_X, _),
    X < MAX_X,
    X1 is X + 1.

vizinho((X,Y), (X1,Y)) :-
    X > 1,
    X1 is X - 1.

vizinho((X,Y), (X,Y1)) :-
    map_size(_, MAX_Y),
    Y < MAX_Y,
    Y1 is Y + 1.

vizinho((X,Y), (X,Y1)) :-
    Y > 1,
    Y1 is Y - 1.

% Heurística: Manhattan distance
heuristica((X1, Y1), (X2, Y2), H) :-
    H is abs(X1 - X2) + abs(Y1 - Y2).

% A* busca: Fila é lista de tuplas (F, G, Pos, Caminho)
a_estrela([(F, G, Objetivo, Caminho) | _], Objetivo, Caminho) :- !.

a_estrela([(F, G, PosAtual, CaminhoAc) | RestoFila], Objetivo, CaminhoFinal) :-
    findall((F2, G2, Prox, [Prox | CaminhoAc]),
        (
            vizinho(PosAtual, Prox),
            is_safe_pos(Prox),
            \+ member(Prox, CaminhoAc),
            G2 is G + 1,
            heuristica(Prox, Objetivo, H2),
            F2 is G2 + H2
        ),
        Novos),
    append(RestoFila, Novos, FilaTemp),
    sort(FilaTemp, FilaOrdenada),  % mantém prioridade
    a_estrela(FilaOrdenada, Objetivo, CaminhoFinal).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Caminho até energia
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_energy_position(X, Y) :-
    energy_positions(L),
    \+ member((X,Y), L),                      % se ainda não está na lista
    retract(energy_positions(L)),
    assert(energy_positions([(X,Y)|L])).       % adiciona na cabeça da lista
add_energy_position(_, _).                     % se já está, não faz nada

% Calcular energia mais próxima
energia_mais_proxima(PosAtual, EnergiaMaisProxima) :-
    energy_positions(L),
    L \= [],
    findall((D, Pos),
        (member(Pos, L),
        heuristica(PosAtual, Pos, D)),
        Distancias),
    sort(Distancias, [( _ , EnergiaMaisProxima) | _]).

% Wrapper principal
caminho_ate_energia(Caminho) :-
    posicao(X, Y, _),
    PosAtual = (X,Y),
    energia_mais_proxima(PosAtual, Destino),
    heuristica(PosAtual, Destino, H),
    writeln(H),
    a_estrela([(H, 0, PosAtual, [PosAtual])], Destino, CaminhoReverso),
    writeln("Está faltando o QUE?"),
    reverse(CaminhoReverso, Caminho).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Caminho inicial
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%adiciona uma posicao como segura
add_safe_position(X, Y) :-
    safe_positions(L),
    \+ member((X,Y), L),                      % se ainda não está na lista
    retract(safe_positions(L)),
    assert(safe_positions([(X,Y)|L])).       % adiciona na cabeça da lista
add_safe_position(_, _).                     % se já está, não faz nada

%verifica se uma posicao é segura
is_safe(X,Y) :- safe_positions(L), member((X,Y), L).

% verifica se a posição é segura
is_safe_pos((X,Y)) :- safe_positions(L), member((X,Y), L).

% caminho_ate_inicio(-Caminho)
caminho_ate_inicio(Caminho) :-
    posicao(X, Y, _),
    PosAtual = (X,Y),
    heuristica(PosAtual, (1,1), H),
    a_estrela([(H, 0, PosAtual, [PosAtual])], (1,1), CaminhoReverso),   
    reverse(CaminhoReverso, Caminho).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Controle de Status
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%atualiza pontuacao
atualiza_pontuacao(X):- pontuacao(P), retract(pontuacao(P)), NP is P + X, assert(pontuacao(NP)),!.

%atualiza ouros_coletados
atualiza_ouros:- ourosColetados(P), retract(ourosColetados(P)), NP is P + 1, assert(ourosColetados(NP)),!.

%atualiza energia
atualiza_energia(N):- energia(E), retract(energia(E)), NE is E + N, 
					(
					 (NE =<0, assert(energia(0)),posicao(X,Y,_),retract(posicao(_,_,_)), assert(posicao(X,Y,morto)),!);
					 (NE >100, assert(energia(100)),!);
					  (NE >0,assert(energia(NE)),!)
					 ).

%verifica situacao da nova posicao e atualiza energia e pontos
verifica_player :- posicao(X,Y,_), tile(X,Y,'P'), atualiza_energia(-100), atualiza_pontuacao(-1000),!.
verifica_player :- posicao(X,Y,_), tile(X,Y,'D'), random_between(-80,-50,D), atualiza_energia(D),!.
verifica_player :- posicao(X,Y,_), tile(X,Y,'d'), random_between(-50,-25,D), atualiza_energia(D),!.
verifica_player :- posicao(X,Y,Z), tile(X,Y,'T'), 
					map_size(SX,SY), random_between(1,SX,NX), random_between(1,SY,NY),
				retract(posicao(X,Y,Z)), assert(posicao(NX,NY,Z)), atualiza_obs, verifica_player,!.
verifica_player :- true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Comandos
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%virar direita
virar_direita :- posicao(X,Y, norte), retract(posicao(_,_,_)), assert(posicao(X, Y, leste)),atualiza_pontuacao(-1),!.
virar_direita :- posicao(X,Y, oeste), retract(posicao(_,_,_)), assert(posicao(X, Y, norte)),atualiza_pontuacao(-1),!.
virar_direita :- posicao(X,Y, sul), retract(posicao(_,_,_)), assert(posicao(X, Y, oeste)),atualiza_pontuacao(-1),!.
virar_direita :- posicao(X,Y, leste), retract(posicao(_,_,_)), assert(posicao(X, Y, sul)),atualiza_pontuacao(-1),!.

%virar esquerda
virar_esquerda :- posicao(X,Y, norte), retract(posicao(_,_,_)), assert(posicao(X, Y, oeste)),atualiza_pontuacao(-1),!.
virar_esquerda :- posicao(X,Y, oeste), retract(posicao(_,_,_)), assert(posicao(X, Y, sul)),atualiza_pontuacao(-1),!.
virar_esquerda :- posicao(X,Y, sul), retract(posicao(_,_,_)), assert(posicao(X, Y, leste)),atualiza_pontuacao(-1),!.
virar_esquerda :- posicao(X,Y, leste), retract(posicao(_,_,_)), assert(posicao(X, Y, norte)),atualiza_pontuacao(-1),!.

%andar
andar :- posicao(X,Y,P), P = norte, map_size(_,MAX_Y), Y < MAX_Y, YY is Y + 1, 
         retract(posicao(X,Y,_)), assert(posicao(X, YY, P)), 
		 %((retract(certeza(X,YY)), assert(certeza(X,YY))); assert(certeza(X,YY))),
		 set_real(X,YY),
		 ((retract(visitado(X,Y)), assert(visitado(X,Y))); assert(visitado(X,Y))),atualiza_pontuacao(-1),!.
		 
andar :- posicao(X,Y,P), P = sul,  Y > 1, YY is Y - 1, 
         retract(posicao(X,Y,_)), assert(posicao(X, YY, P)), 
		 %((retract(certeza(X,YY)), assert(certeza(X,YY))); assert(certeza(X,YY))),
		 set_real(X,YY),
		 ((retract(visitado(X,Y)), assert(visitado(X,Y))); assert(visitado(X,Y))),atualiza_pontuacao(-1),!.

andar :- posicao(X,Y,P), P = leste, map_size(MAX_X,_), X < MAX_X, XX is X + 1, 
         retract(posicao(X,Y,_)), assert(posicao(XX, Y, P)), 
		 %((retract(certeza(XX,Y)), assert(certeza(XX,Y))); assert(certeza(XX,Y))),
		 set_real(XX,Y),
		 ((retract(visitado(X,Y)), assert(visitado(X,Y))); assert(visitado(X,Y))),atualiza_pontuacao(-1),!.

andar :- posicao(X,Y,P), P = oeste,  X > 1, XX is X - 1, 
         retract(posicao(X,Y,_)), assert(posicao(XX, Y, P)), 
		 %((retract(certeza(XX,Y)), assert(certeza(XX,Y))); assert(certeza(XX,Y))),
		 set_real(XX,Y),
		 ((retract(visitado(X,Y)), assert(visitado(X,Y))); assert(visitado(X,Y))),atualiza_pontuacao(-1),!.
		 
%pegar	
pegar :- posicao(X,Y,_), tile(X,Y,'O'), retract(tile(X,Y,'O')), assert(tile(X,Y,'')), atualiza_pontuacao(-5), atualiza_pontuacao(500),set_real(X,Y),!. 
pegar :- posicao(X,Y,_), tile(X,Y,'U'), retract(tile(X,Y,'U')), assert(tile(X,Y,'')), atualiza_pontuacao(-5), atualiza_energia(50),set_real(X,Y),!. 
pegar :- atualiza_pontuacao(-5),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Funcoes Auxiliares de navegação e observação
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		 
%Define as 4 adjacencias		 
adjacente(X, Y) :- posicao(PX, Y, _), map_size(MAX_X,_),PX < MAX_X, X is PX + 1.  
adjacente(X, Y) :- posicao(PX, Y, _), PX > 1, X is PX - 1.  
adjacente(X, Y) :- posicao(X, PY, _), map_size(_,MAX_Y),PY < MAX_Y, Y is PY + 1.  
adjacente(X, Y) :- posicao(X, PY, _), PY > 1, Y is PY - 1.  

%cria lista com a adjacencias
adjacentes(L) :- findall(Z,(adjacente(X,Y),tile(X,Y,Z)),L).

%define observacoes locais
observacao_loc(brilho,L) :- member('O',L).
observacao_loc(reflexo,L) :- member('U',L).

%define observacoes adjacentes
observacao_adj(brisa,L) :- member('P',L).
observacao_adj(palmas,L) :- member('T',L).
observacao_adj(passos,L) :- member('D',L).
observacao_adj(passos,L) :- member('d',L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tratamento de KB e observações
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%consulta e processa observações
atualiza_obs:-adj_cand_obs(LP), observacoes(LO), iter_pos_list(LP,LO), observacao_certeza, observacao_vazia.

%adjacencias candidatas p/ a observacao (aquelas não visitadas)
adj_cand_obs(L) :- findall((X,Y), (adjacente(X, Y), \+visitado(X,Y)), L).

%cria lista de observacoes
observacoes(X) :- adjacentes(L), findall(Y, observacao_adj(Y,L), X).

%itera posicoes da lista para adicionar observacoes
iter_pos_list([], _) :- !.
iter_pos_list([H|T], LO) :- H=(X,Y), 
							((corrige_observacoes_antigas(X, Y, LO),!);
							adiciona_observacoes(X, Y, LO)),
							iter_pos_list(T, LO).							 

%Corrige observacoes antigas na memoria que ficaram com apenas uma adjacencia
corrige_observacoes_antigas(X, Y, []):- \+certeza(X,Y), memory(X,Y,[]).
corrige_observacoes_antigas(X, Y, LO):-
	\+certeza(X,Y), \+ memory(X,Y,[]), memory(X, Y, LM), intersection(LO, LM, L), 
	retract(memory(X, Y, LM)), assert(memory(X, Y, L)).

%Adiciona observacoes na memoria
adiciona_observacoes(X, Y, _) :- certeza(X,Y),!.
adiciona_observacoes(X, Y, LO) :- \+certeza(X,Y), \+ memory(X,Y,_), assert(memory(X, Y, LO)).

%Quando há apenas uma observação e uma unica posição incerta, deduz que a observação está na casa incerta
%e marca como certeza
%observacao_certeza:- findall((X,Y), (adjacente(X, Y), 
%						((\+visitado(X,Y), \+certeza(X,Y));(certeza(X,Y),memory(X,Y,ZZ),ZZ\=[])),
%						memory(X,Y,Z), Z\=[]), L), ((length(L,1),L=[(XX,YY)], assert(certeza(XX,YY)),!);true).
						
observacao_certeza:- observacao_certeza('brisa'),
						observacao_certeza('palmas'),
						observacao_certeza('passos').
						
observacao_certeza(Z):- findall((X,Y), (adjacente(X, Y), 
						((\+visitado(X,Y), \+certeza(X,Y));(certeza(X,Y),memory(X,Y,[Z]))),
						memory(X,Y,[Z])), L), ((length(L,1),L=[(XX,YY)], assert(certeza(XX,YY)),!);true).						

%Quando posição não tem observações
observacao_vazia:- adj_cand_obs(LP), observacao_vazia(LP).
observacao_vazia([]) :- !.
observacao_vazia([H|T]) :- H=(X,Y), ((memory(X,Y,[]), \+certeza(X,Y),assert(certeza(X,Y)),!);true), observacao_vazia(T).

%Quando posicao é visitada, atualiza memoria de posicao com a informação real do mapa 
set_real(X,Y):- ((retract(certeza(X,Y)), assert(certeza(X,Y)),!); assert(certeza(X,Y))), set_real2(X,Y),!.
set_real2(X,Y):- tile(X,Y,'P'), ((retract(memory(X,Y,_)),assert(memory(X,Y,[brisa])),!);assert(memory(X,Y,[brisa]))),!.
set_real2(X,Y):- tile(X,Y,'O'), ((retract(memory(X,Y,_)),assert(memory(X,Y,[brilho])),!);assert(memory(X,Y,[brilho]))),!.
set_real2(X,Y):- tile(X,Y,'T'), ((retract(memory(X,Y,_)),assert(memory(X,Y,[palmas])),!);assert(memory(X,Y,[palmas]))),!.
set_real2(X,Y):- ((tile(X,Y,'D'),!); tile(X,Y,'d')), ((retract(memory(X,Y,_)),assert(memory(X,Y,[passos])),!);assert(memory(X,Y,[passos]))),!.
set_real2(X,Y):- tile(X,Y,'U'), add_energy_position(X,Y), add_safe_position(X,Y), ((retract(memory(X,Y,_)),assert(memory(X,Y,[reflexo])),!);assert(memory(X,Y,[reflexo]))),!.
set_real2(X,Y):- tile(X,Y,''), add_safe_position(X,Y), ((retract(memory(X,Y,_)),assert(memory(X,Y,[])),!);assert(memory(X,Y,[]))),!.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Mostra mapa real
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
show_player(X,Y) :- posicao(X,Y, norte), write('^'),!.
show_player(X,Y) :- posicao(X,Y, oeste), write('<'),!.
show_player(X,Y) :- posicao(X,Y, leste), write('>'),!.
show_player(X,Y) :- posicao(X,Y, sul), write('v'),!.
show_player(X,Y) :- posicao(X,Y, morto), write('+'),!.

%show_position(X,Y) :- show_player(X,Y),!.
show_position(X,Y) :- (show_player(X,Y); write(' ')), tile(X,Y,Z), ((Z='', write(' '));write(Z)),!.

show_map :- map_size(_,MAX_Y), show_map(1,MAX_Y),!.
show_map(X,Y) :- Y >= 1, map_size(MAX_X,_), X =< MAX_X, show_position(X,Y), write(' | '), XX is X + 1, show_map(XX, Y),!.
show_map(X,Y) :- Y >= 1, map_size(X,_),YY is Y - 1, write(Y), nl, show_map(1, YY),!.
show_map(_,0) :- energia(E), pontuacao(P), write('E: '), write(E), write('   P: '), write(P),!.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Mostra mapa conhecido
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

show_mem_info(X,Y) :- memory(X,Y,Z), 
		((visitado(X,Y), write('.'),!); (\+certeza(X,Y), write('?'),!); (certeza(X,Y), write('!'))),
		((member(brisa, Z), write('P'));write(' ')),
		((member(palmas, Z), write('T'));write(' ')),
		((member(brilho, Z), write('O'));write(' ')),
		((member(passos, Z), write('D'));write(' ')),
		((member(reflexo, Z), write('U'));write(' ')),!.

show_mem_info(X,Y) :- \+memory(X,Y,[]), 
			((visitado(X,Y), write('.'),!); (\+certeza(X,Y), write('?'),!); (certeza(X,Y), write('!'))),
			write('     '),!.		
		
		

show_mem_position(X,Y) :- posicao(X,Y,_), 
		((visitado(X,Y), write('.'),!); (certeza(X,Y), write('!'),!); write(' ')),
		write(' '), show_player(X,Y),
		((memory(X,Y,Z),
		((member(brilho, Z), write('O'));write(' ')),
		((member(passos, Z), write('D'));write(' ')),
		((member(reflexo, Z), write('U'));write(' ')),!);
		(write('   '),!)).

		
show_mem_position(X,Y) :- show_mem_info(X,Y),!.


show_mem :- map_size(_,MAX_Y), show_mem(1,MAX_Y),!.
show_mem(X,Y) :- Y >= 1, map_size(MAX_X,_), X =< MAX_X, show_mem_position(X,Y), write('|'), XX is X + 1, show_mem(XX, Y),!.
show_mem(X,Y) :- Y >= 1, map_size(X,_),YY is Y - 1, write(Y), nl, show_mem(1, YY),!.
show_mem(_,0) :- energia(E), pontuacao(P), write('E: '), write(E), write('   P: '), write(P),!.

mostrar_ouros :-
    ourosColetados(Qtd),
    format("Ouros coletados: ~w~n", [Qtd]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

tem_inimigo(X, Y) :-
    certeza((X, Y), inimigo).

posicao_segura(X, Y) :-
    \+ tem_inimigo(X, Y),
    (memory(X, Y, Obs) ->
        \+ member(brisa, Obs),
        \+ member(passos, Obs),
        \+ member(palmas, Obs)
    ; true).

movimentos_seguros_nao_visitados(L) :-
    findall((X, Y),
        (adjacente(X, Y),
         \+ visitado(X, Y),
         posicao_segura(X, Y)),
    L).

movimentos_inseguros_nao_visitados(L) :-
    findall((X, Y),
        (adjacente(X, Y),
         \+ visitado(X, Y),
         \+ member(brisa, Obs)),
    L).

movimentos_seguros_visitados(L) :-
    findall((X, Y),
        (adjacente(X, Y),
         visitado(X, Y),
         posicao_segura(X, Y)),
    L).


melhor_movimento(Pos) :-
    movimentos_seguros_nao_visitados(L1),
    L1 \= [], !,
    L1 = [Pos|_].

melhor_movimento(Pos) :-
    movimentos_inseguros_nao_visitados(L2),
    L2 \= [], !,
    L2 = [Pos|_].

melhor_movimento(Pos) :-
    movimentos_seguros_visitados(L3),
    L3 \= [], !,
    L3 = [Pos|_].


% Direção desejada
direcao_desejada((X, Y), (X2, Y), leste)  :- X2 is X + 1.
direcao_desejada((X, Y), (X2, Y), oeste)  :- X2 is X - 1.
direcao_desejada((X, Y), (X, Y2), norte)  :- Y2 is Y + 1.
direcao_desejada((X, Y), (X, Y2), sul)    :- Y2 is Y - 1.

% Próxima direção ao girar
proxima_direcao(norte, virar_direita, leste).
proxima_direcao(leste, virar_direita, sul).
proxima_direcao(sul, virar_direita, oeste).
proxima_direcao(oeste, virar_direita, norte).
proxima_direcao(norte, virar_esquerda, oeste).
proxima_direcao(oeste, virar_esquerda, sul).
proxima_direcao(sul, virar_esquerda, leste).
proxima_direcao(leste, virar_esquerda, norte).

% Código numérico das direções
direcao_num(norte, 0).
direcao_num(leste, 1).
direcao_num(sul, 2).
direcao_num(oeste, 3).

% Distância de rotação entre direções
distancia_direcao(D1, D2, Dist) :-
    direcao_num(D1, N1),
    direcao_num(D2, N2),
    Diff is abs(N1 - N2),
    Dist is min(Diff, 4 - Diff).

todos_ouros_coletados :-
    % lógica para verificar se todos os ouros foram coletados
    writeln("Todos os ouros foram coletados.").

energia_baixa :-
    writeln("Caminho para energia encontrado.").

% Ações

% executa_acao(energia_baixa) :-
%     energia(E),
%     E < 50,
%     writeln("Energia baixa! Buscando energia mais próxima..."),
%     caminho_ate_energia(Caminho),
%     writeln("Demorando muito"),
%     imprime_lista(Caminho),
%     writeln("Caminho até energia mais próxima!"), !.

% executa_acao(todos_ouros_coletados) :-
%     ourosColetados(Qtd),
%     Qtd =:= 3,
%     safe_positions(IL),
%     imprime_lista(IL),
%     writeln("Primeira lista"),
%     caminho_ate_inicio(C),
%     imprime_lista(C),
%     writeln("CAMINHO PRA VOLTA"),!.

executa_acao(pegar) :- 
    posicao(X, Y, _),
    memory(X, Y, L),
    member(brilho, L),
    atualiza_ouros,!.
    
executa_acao(pegar) :- 
    posicao(X, Y, _),
    memory(X, Y, L),
    member(reflexo, L),
    energia(E),
    E < 50, !.

executa_acao(andar) :-
    posicao(X, Y, Dir),
    melhor_movimento((NX, NY)),
    direcao_desejada((X, Y), (NX, NY), Dir), !.


executa_acao(virar_direita) :-
    posicao(X, Y, Dir),
    melhor_movimento((NX, NY)),
    direcao_desejada((X, Y), (NX, NY), Desejada),
    Dir \= Desejada,
    proxima_direcao(Dir, virar_direita, NovoDir),
    distancia_direcao(NovoDir, Desejada, DistR),
    proxima_direcao(Dir, virar_esquerda, OutroDir),
    distancia_direcao(OutroDir, Desejada, DistL),
    DistR =< DistL, !.

executa_acao(virar_esquerda) :-
    posicao(X, Y, Dir),
    melhor_movimento((NX, NY)),
    direcao_desejada((X, Y), (NX, NY), Desejada),
    Dir \= Desejada, !.

% Último recurso: ação aleatória
executa_acao(Acao) :-
    Acoes = [virar_esquerda, virar_direita, andar],
    random_member(Acao, Acoes), !.






