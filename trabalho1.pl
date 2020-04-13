%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Base de Conhecimento para contratos publicos para prestacao de servicos

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).
:- op( 900,xfy,'e'  ).
:- op( 900,xfy,'ou' ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Meta predicados.

:- dynamic adjudicante/4.
:- dynamic adjudicataria/4.
:- dynamic contrato/10.
%:- dynamic data/4.
:- dynamic '-'/1.
:- dynamic excecao/1.
:- dynamic clausImperfeito/2.
:- dynamic registaConhecimentoIncerto/2.
:- dynamic nuloInterdito/1.
:- dynamic (::)/2.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado insere: Termo -> {V,F}

insere(P) :- assert(P).
insere(P) :- retract(P), !, fail.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado remove: Termo -> {V,F}

remove(P) :- retract(P).
remove(P) :- assert(P), !, fail.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado testa: Lista -> {V,F}

testa([]).
testa([X|R]) :- X, testa(R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado solucoes: Termo, Questão, Resultado -> {V,F}

solucoes(X,Y,Z) :- findall(X,Y,Z).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado solucoesSRep: Termo, Questão, Resultado -> {V,F}

solucoesSRep(X,Y,Z) :- setof(X,Y,Z). 

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado evolucao: Termo -> {V,F}

evolucao(T) :-
	solucoes(I, +T :: I, S),
	insere(T),
	testa(S).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado involucao: Termo -> {V,F}

involucao(T) :-
	solucoes(I, -T :: I, S),
	remove(T),
	testa(S).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado comprimento: Lista, Resultado -> {V,F}

comprimento(S,N) :- length(S,N).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do meta-predicado atualizar: Termo -> {V,F}

% Conhecimento positivo -> Conhecimento positivo (adjudicante)
atualizar(adjudicante(IdA,N,Nif,M)):-
    nao(adjudicante(IdA,N,Nif,M)),
    nao(excecao(adjudicante(IdA,N,Nif,M))),
    solucoes((adjudicante(IdA,_,_,_)),
             (adjudicante(IdA,_,_,_)),
             R),
    elimina(R),
    insere(adjudicante(IdA,N,Nif,M)).

% Conhecimento positivo -> Conhecimento positivo (adjudicataria)
atualizar(adjudicataria(IdAda,N,Nif,M)):-
    nao(adjudicataria(IdAda,N,Nif,M)),
    nao(excecao(adjudicataria(IdAda,N,Nif,M))),
    solucoes((adjudicataria(IdAda,_,_,_)),
             (adjudicataria(IdAda,_,_,_)),
             R),
    elimina(R),
    insere(adjudicataria(IdAda,N,Nif,M)).

% Conhecimento positivo -> Conhecimento positivo (contrato) 
atualizar(contrato(Id,IdA,IdAda,Tipo,Proc,Desc,Custo,Prazo,Local,Data)):-
    nao(contrato(Id,IdA,IdAda,Tipo,Proc,Desc,Custo,Prazo,Local,Data)),
    nao(excecao(contrato(Id,IdA,IdAda,Tipo,Proc,Desc,Custo,Prazo,Local,Data)),
    solucoes((contrato(Id,_,_,_,_,_,_,_,_,_)),
             (contrato(Id,_,_,_,_,_,_,_,_,_)),
             R),
    elimina(R),
    insere(contrato(Id,IdA,IdAda,Tipo,Proc,Desc,Custo,Prazo,Local,Data)).

% Conhecimento incerto/impreciso -> Conhecimento positivo
atualizar(Q):-
    demo(Q, desconhecido),
    solucoes(C,
             (clausImperfeito(Q, C)),
             R),
    elimina(R),
    insere(Q).

% Conhecimento impreciso sem intervalo -> Conhecimento negativo
atualizar(-Q):-
    clause(excecao(Q), true),
    remove(excecao(Q)),
    insere(-Q).

% Conhecimento negativo -> Conhecimento positivo
atualizar(Q):-
    clause(-Q,true),
    remove(-Q),
    insere(Q).

% Conhecimento positivo -> Conhecimento negativo
atualizar(-Q):-
    solucoes(Q, excecao(Q), S),
    comprimento(S,N), 
    N == 0,
    clause(Q,true),
    remove(Q),
    insere(-Q).

% Conhecimento positivo/negativo novo
atualizar(Q):-
    insere(Q).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado demo: Questao,Resposta -> {V,F}

demo( Questao,verdadeiro ) :-
    Questao.
demo( Questao,falso ) :-
    -Questao.
demo( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado demoComp: CompQuestao,Resposta -> {V,D,F}

demoComp(Q1 e Q2, R) :-
	demo(Q1,R1),
	demoComp(Q2,R2),
	conjuncao(R1,R2,R).
demoComp(Q1 ou Q2, R) :-
	demo(Q1,R1),
	demoComp(Q2,R2),
	disjuncao(R1,R2,R).
demoComp(Q, R) :-
	demo(Q,R).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado conjuncao: Resposta1, Resposta2, Resposta -> {V,D,F}

conjuncao(verdadeiro,verdadeiro,verdadeiro).
conjuncao(verdadeiro,falso,falso).
conjuncao(falso,verdadeiro,falso).
conjuncao(falso,falso,falso).
conjuncao(desconhecido,desconhecido,desconhecido).
conjuncao(desconhecido,verdadeiro,desconhecido).
conjuncao(verdadeiro,desconhecido,desconhecido).
conjuncao(desconhecido,falso,falso).
conjuncao(falso,desconhecido,falso).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado disjuncao: Resposta1, Resposta2, Resposta -> {V,D,F}

disjuncao(verdadeiro,verdadeiro,verdadeiro).
disjuncao(verdadeiro,falso,verdadeiro).
disjuncao(falso,verdadeiro,verdadeiro).
disjuncao(falso,falso,falso).
disjuncao(desconhecido,desconhecido,desconhecido).
disjuncao(desconhecido,verdadeiro,verdadeiro).
disjuncao(verdadeiro,desconhecido,verdadeiro).
disjuncao(desconhecido,falso,desconhecido).
disjuncao(falso,desconhecido,desconhecido).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%Extensão do predicado adjudicante: #IdA, Nome, Nif, Morada -> {V,F,D}

adjudicante(1, cmb, 123456789, braga).
%(...)
%FAZER EXATAMENTE 15 EXEMPLOS

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão da negação forte do predicado adjudicante

-adjudicante(IdA,N,Nif,M) :- nao(adjudicante(IdA,N,Nif,M)), nao(excecao(adjudicante(IdA,N,Nif,M))).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%Extensão do predicado adjudicataria: #IdAda, Nome, Nif, Morada -> {V,F,D}

adjudicataria(1, aaum, 420123954, braga).
%(...)
%FAZER EXATAMENTE 15 EXEMPLOS

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão da negação forte do predicado adjudicataria

-adjudicataria(IdAda,N,Nif,M) :- nao(adjudicataria(IdAda,N,Nif,M)), nao(excecao(adjudicataria(IdAda,N,Nif,M))).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%Extensão do predicado contrato: Id, IdA, IdAda, Tipo, Procedimento, Descricao, Valor, Prazo, Local, Data -> {V,F,D}

contrato(1, 1, 1, aquisicao_servico, consulta_previa, assessoria, 13599, 547, braga, 11-02-2020).
%(...)
%FAZER EXATAMENTE 10 EXEMPLOS

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão da negação forte do predicado contrato

-contrato(Id,IdA,IdAda,Tipo,Proc,Desc,Custo,Prazo,Local,Data) :- 
    nao(contrato(Id,IdA,IdAda,Tipo,Proc,Desc,Custo,Prazo,Local,Data)), 
    nao(excecao(contrato(Id,IdA,IdAda,Tipo,Proc,Desc,Custo,Prazo,Local,Data))).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Conhecimento imperfeito incerto, para o predicado adjudicante

adjudicante(16, cml , sem_nif1 , lisboa).

excecao(adjudicante(ID,N,Nif,M)) :-
    adjudicante(ID,N,sem_nif1,M).

clausImperfeito(adjudicante(ID,N,Nif,M), R) :-
    adjudicante(ID,N,sem_nif1,M),
    R = (adjudicante(ID,N,sem_nif1,M)).

clausImperfeito(adjudicante(ID,N,Nif,M), R) :-
    adjudicante(ID,N,sem_nif1,M),
    R = (excecao(adjudicante(Id,Nome,Numero,Morada)) :- adjudicante(Id,Nome,sem_nif1,Morada)).

adjudicante(17, cmpl , 854894729 , sem_morada1).

excecao(adjudicante(ID,N,Nif,M)) :-
    adjudicante(ID,N,Nif,sem_morada1).

clausImperfeito(adjudicante(ID,N,Nif,M), R) :-
    adjudicante(ID,N,Nif,sem_morada1),
    R = (adjudicante(ID,N,Nif,sem_morada1)).

clausImperfeito(adjudicante(ID,N,Nif,M), R) :-
    adjudicante(ID,N,Nif,sem_morada1),
    R = (excecao(adjudicante(Id,Nome,Numero,Morada)) :- adjudicante(Id,Nome,Numero,sem_morada1)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Conhecimento imperfeito incerto, para o predicado adjudicataria

adjudicataria(16, ipdj, sem_nif2, lisboa). 

excecao(adjudicataria(ID,N,Nif,M)) :-
    adjudicataria(ID,N,sem_nif2,M).

clausImperfeito(adjudicataria(ID,N,Nif,M), R) :-
    adjudicataria(ID,N,sem_nif2,M),
    R = (adjudicataria(ID,N,sem_nif2,M)).

clausImperfeito(adjudicataria(ID,N,Nif,M), R) :-
    adjudicataria(ID,N,sem_nif2,M),
    R = (excecao(adjudicataria(Id,Nome,Numero,Morada)) :- adjudicataria(Id,Nome,sem_nif2,Morada)).

adjudicataria(17, gdpeoes , 274917053 , sem_morada2).

excecao(adjudicataria(ID,N,Nif,M)) :-
    adjudicataria(ID,N,Nif,sem_morada2).

clausImperfeito(adjudicataria(ID,N,Nif,M), R) :-
    adjudicataria(ID,N,Nif,sem_morada2),
    R = (adjudicataria(ID,N,Nif,sem_morada2)).

clausImperfeito(adjudicataria(ID,N,Nif,M), R) :-
    adjudicataria(ID,N,Nif,sem_morada2),
    R = (excecao(adjudicataria(Id,Nome,Numero,Morada)) :- adjudicataria(Id,Nome,Numero,sem_morada2)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Conhecimento imperfeito interdito, para o predicado contrato

% ---------------------------
% -------  FAZER XD  --------    TUDO PUTAS CARALHO FODASSE PUTA QUE PARIU LÁ O CARALHO DESTA PUTA DESTA MERDA TODA FODASSE :)
% ---------------------------





















































%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariantes
% Invariante Estrutural:  nao permitir a insercao de conhecimento
%                         repetido para o adjudicante

+adjudicante(IdA,Nome,Nif,Morada)::((solucoes(IdA, adjudicante(IdA,Nome,Nif,Morada), A), comprimento(A,N), N==1)).

% Invariante Estrutural:  nao permitir a insercao de conhecimento
%                         repetido para a adjudicataria

+adjudicataria(IdAda,Nome,Nif,Morada)::((solucoes(IdAda, adjudicataria(IdAda,Nome,Nif,Morada), A), comprimento(A,N), N==1)).

% Invariante Estrutural:  nao permitir a insercao de conhecimento
%                         repetido para o contrato

+cuidado(ID, D, IDu, IDp, Des, C) :: (solucoes((ID),(cuidado(ID,A,B,X,Y,Z)),S),
                  comprimento(S,N), 
				  N =< 1).
