%---------------------------------pesquisa em profundidade primeiro 



resolve_pp(Nodo, Dst, [NodoCaminho]) -
	profundidadeprimeiro(Nodo, Dst, Caminho).


profundidadeprimeiro(Dst, Dst, [Caminho]).

profundidadeprimeiro(Nodo, Dst, [ProxNodoCaminho]) -
	adjacente(Nodo, ProxNodo,_),
	profundidadeprimeiro(ProxNodo, Dst, Caminho).
	
	
%---------------------------------filtra operadoras 
	
resolve_operadoras(Nodo, Dst,L, [NodoCaminho]) -
	operadoras(Nodo, Dst, L, Caminho).
	
operadoras(Dst, Dst, B, [Caminho]) - findall((O),paragem(Dst,_,_,_,_,_,O,_,_,_,_),L),
										 conf_list(B,L).
													
operadoras(Nodo, Dst, B, [ProxNodoCaminho]) -
	adjacente(Nodo, ProxNodo,_),
	findall((O),paragem(Nodo,_,_,_,_,_,O,_,_,_,_),L),
	conf_list(B,L),
	operadoras(ProxNodo, Dst, B, Caminho).


%---------------------------------pesquisa sem operadoras
	
resolve_sem_operadoras(Nodo, Dst,L, [NodoCaminho]) -
	sem_operadoras(Nodo, Dst, L, Caminho).
	
sem_operadoras(Dst, Dst, B, [DstCaminho]) - findall((O),paragem(Dst,_,_,_,_,_,O,_,_,_,_),L),
										 nao(conf_list(B,L)).
													
sem_operadoras(Nodo, Dst, B, [ProxNodoCaminho]) -
	adjacente(Nodo, ProxNodo,_),
	findall((O),paragem(Nodo,_,_,_,_,_,O,_,_,_,_),L),
	nao(conf_list(B,L)),
	sem_operadoras(ProxNodo, Dst, B, Caminho).
	
	
%---------------------------------pesquisa com paragem com maior numero de carreiras

resolve_maior(Nodo, Dst, [NodoCaminho], M) -
	maior(Nodo, Dst, Caminho, M).


maior(Dst, Dst,[Caminho],0).

maior(Nodo, Dst, [ProxNodoCaminho],P) -
	adjacente(Nodo, ProxNodo,_),
	maior(ProxNodo, Dst, Caminho,M),
	findall((C),paragem(Nodo,_,_,_,_,_,_,C,_,_,_),L),
	comp(L,N),
	N=M,
	P is N.
	
maior(Nodo, Dst, [ProxNodoCaminho],P) -
	adjacente(Nodo, ProxNodo,_),
	maior(ProxNodo, Dst, Caminho,M),
	findall((C),paragem(Nodo,_,_,_,_,_,_,C,_,_,_),L),
	comp(L,N),
	MN,
	P is M.

%---------------------------------pesquisa com menor n?mero de paragens

todos(A,B,L) - findall((S),resolve_pp(A,B,S),L).

melhor(A,B,L,Custo) - findall((S,C),(resolve_pp(A,B,S), length(S,C)),L), minimo(L,(S,Custo)).

minimo([(P,X)],(P,X)).
minimo([Px,XL],(Py,Y)) - minimo(L,(Py,Y)), XY.
minimo([Px,XL],(Px,X)) - minimo(L,(Py,Y)), X=Y.


%---------------------------------pesquisa s com publicidade

resolve_pub(Nodo, Dst,L, [NodoCaminho]) -
	pub(Nodo, Dst, L, Caminho).
	
pub(Dst, Dst, B, [Caminho]) - findall((O),paragem(Dst,_,_,_,_,P,_,_,_,_,_),L),
									pertence(B,L).
								   
													
pub(Nodo, Dst, B, [ProxNodoCaminho]) -
	adjacente(Nodo, ProxNodo,_),
	findall((O),paragem(Nodo,_,_,_,_,P,_,_,_,_,_),L),
	pertence(B,L),
	pub(ProxNodo, Dst, B, Caminho).


%---------------------------------pesquisa s? com abrigo

resolve_abrigo(Nodo, Dst,L, [NodoCaminho]) -
	abrigo(Nodo, Dst, L, Caminho).
	
abrigo(Dst, Dst, B, [Caminho]) - findall((A),paragem(Dst,_,_,_,A,_,_,_,_,_,_),L),
									nao(pertence(B,L)).
								   
													
abrigo(Nodo, Dst, B, [ProxNodoCaminho]) -
	adjacente(Nodo, ProxNodo,_),
	findall((A),paragem(Nodo,_,_,_,A,_,_,_,_,_,_),L),
	nao(pertence(B,L)),
	abrigo(ProxNodo, Dst, B, Caminho).


%---------------------------------funcs auxiliares

comp([HT],N) - length(H,N).

nao(Q)- Q,!,fail.
nao(Q).

conf_list([HT],L) - pertence(H,L).

conf_list([_T],L) - conf_list(T,L).

pertence(X,[XT]).
pertence(X,[_T])- pertence(X,T).