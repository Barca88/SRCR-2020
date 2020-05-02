%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Base de Conhecimento para contratos publicos para prestacao de servicos

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
%:- set_prolog_flag( unknown,fail ).

:-style_check(-discontiguous).
:- discontiguous (::)/2.
:- discontiguous excecao/1.

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
:- dynamic '-'/1.
:- dynamic excecao/1.
:- dynamic clausImperfeito/2.
:- dynamic registaConhecimentoIncerto/2.
:- dynamic nuloInterdito/1.
:- dynamic (::)/2.
:- dynamic soma/2.
:- dynamic data/4.

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
atualizar(contrato(Id,IdA,IdAda,Tipo,Proc,Desc,Custo,Prazo,Local,IdData)):-
    nao(contrato(Id,IdA,IdAda,Tipo,Proc,Desc,Custo,Prazo,Local,IdData)),
    nao(excecao(contrato(Id,IdA,IdAda,Tipo,Proc,Desc,Custo,Prazo,Local,IdData))),
    solucoes((contrato(Id,_,_,_,_,_,_,_,_,_)),
             (contrato(Id,_,_,_,_,_,_,_,_,_)),
             R),
    elimina(R),
    insere(contrato(Id,IdA,IdAda,Tipo,Proc,Desc,Custo,Prazo,Local,IdData)).

% Conhecimento positivo -> Conhecimento positivo (data)
atualizar(data(Id,A,M,D)):-
    nao(data(Id,A,M,D)),
    nao(excecao(data(Id,A,M,D))),
    solucoes((data(Id,_,_,_)),
             (data(Id,_,_,_)),
             R),
    elimina(R),
    insere(data(Id,A,M,D)).

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
adjudicante(2, cmvm, 325824562, vieira_minho).
adjudicante(3, cmvl, 858617833, vila_real).
adjudicante(4, cmpv, 389646360, povoa_varzim).
adjudicante(5, cmpl, 809032071, povoa_lanhoso).
adjudicante(6, cmf, 928803358, fafe).
adjudicante(7, cmg, 261109115, guimaraes).
adjudicante(8, cmp, 974841598, porto).
adjudicante(9, cmtb, 919338755, terras_bouro).
adjudicante(10, cmvc, 369641730, vila_conde).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão da negação forte do predicado adjudicante

-adjudicante(IdA,N,Nif,M) :- nao(adjudicante(IdA,N,Nif,M)), nao(excecao(adjudicante(IdA,N,Nif,M))).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%Extensão do predicado adjudicataria: #IdAda, Nome, Nif, Morada -> {V,F,D}

adjudicataria(1, aaum, 420123954, braga).
adjudicataria(2, bvvm, 420123954, vieira_minho).
adjudicataria(3, hpb, 420123954, braga).
adjudicataria(4, jeor, 420123954, povoa_varzim).
adjudicataria(5, vsc, 420123954, guimaraes).
adjudicataria(6, scb, 420123954, braga).
adjudicataria(7, aevl, 420123954, vila_real).
adjudicataria(8, pcne, 420123954, braga).
adjudicataria(9, drpus, 420123954, braga).
adjudicataria(10, mfc, 420123954, melgaco).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão da negação forte do predicado adjudicataria

-adjudicataria(IdAda,N,Nif,M) :- nao(adjudicataria(IdAda,N,Nif,M)), nao(excecao(adjudicataria(IdAda,N,Nif,M))).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%Extensão do predicado contrato: Id, IdA, IdAda, Tipo, Procedimento, Descricao, Valor, Prazo, Local, IdData -> {V,F,D}

contrato(1, 1, 1, aquisicao_servico, consulta_previa, assessoria, 13599, 547, braga, 1).
contrato(2, 5, 2, aquisicao_bens, ajuste_direto, assessoria, 1982, 53, viera_minho, 2).
contrato(3, 8, 6, locacao_bens, consulta_previa, assessoria, 13599, 547, povoa_varzim, 3).
contrato(4, 3, 4, aquisicao_servico, concurso_publico, assessoria, 13599, 547, vila_real, 4).
contrato(5, 9, 8, aquisicao_bens, consulta_previa, assessoria, 13599, 133, braga, 5).
contrato(6, 10,1, locacao_bens, concurso_publico, assessoria, 13599, 547, braga, 6).
contrato(7, 2, 2, aquisicao_bens, consulta_previa, assessoria, 13599, 547, lisboa, 7).
contrato(8, 6, 9, aquisicao_servico, concurso_publico, assessoria, 13599, 547, braga, 8).
contrato(9, 8, 3, aquisicao_bens, ajuste_direto, assessoria, 1359, 105, coimbra, 9).
contrato(10, 1, 1, locacao_bens, concurso_publico, assessoria, 13599, 547, braga, 10).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão da negação forte do predicado contrato

-contrato(Id,IdA,IdAda,Tipo,Proc,Desc,Custo,Prazo,Local,IdData) :- 
    nao(contrato(Id,IdA,IdAda,Tipo,Proc,Desc,Custo,Prazo,Local,IdData)), 
    nao(excecao(contrato(Id,IdA,IdAda,Tipo,Proc,Desc,Custo,Prazo,Local,IdData))).

% -------------------------------------------------------------------------------------------
%Extensão do predicado data: IdData, Ano, Mês, Dia -> {V,F}
data(1,2020,1,1).
data(2,2020,1,1).
data(3,2020,1,1).
data(4,2020,1,2).
data(5,2020,1,2).
data(6,2020,1,3).
data(7,2020,1,3).
data(8,2020,1,3).
data(9,2020,1,4).
data(10,2020,1,4).
data(11,2020,1,5).
data(12,2020,2,5).
data(13,2020,2,6).
data(14,2020,2,6).
data(15,2020,2,7).
data(16,2020,1,2).
data(17,2020,3,5).

-data(IdD,Ano,Mes,Dia) :- nao(data(IdD,Ano,Mes,Dia)), nao(excecao(data(IdD,Ano,Mes,Dia))).
    
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


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% conhecimento imperfeito incerto, para o predicado contrato

contrato(11,3,5,aquisicao_servico,consulta_previa,desc1,4932,360,braga,10).

excecao(contrato(Id,IdA,IdAda,Tipo,Proc,Desc,Custo,Prazo,Local,IdData)) :-
    contrato(Id,IdA,IdAda,Tipo,Proc,desc1,Custo,Prazo,Local,IdData).

clausImperfeito(contrato(Id,IdA,IdAda,Tipo,Proc,Desc,Custo,Prazo,Local,IdData)) :-
    contrato(Id,IdA,IdAda,Tipo,Proc,desc1,Custo,Prazo,Local,IdData),
    R = (contrato(Id,IdA,IdAda,Tipo,Proc,desc1,Custo,Prazo,Local,IdData)).

clausImperfeito(contrato(Id,IdA,IdAda,Tipo,Proc,Desc,Custo,Prazo,Local,IdData), R) :-
    contrato(Id,IdA,IdAda,Tipo,Proc,desc1,Custo,Prazo,Local,IdData),
    R = (excecao(contrato(Id,IdA,IdAda,Tipo,Proc,Desc,Custo,Prazo,Local,IdData)) :-
    contrato(Id,IdA,IdAda,Tipo,Proc,desc1,Custo,Prazo,Local,IdData)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Invariantes

% Não permite que haja conhecimento negativo repetido
+(-Q) :: (solucoes(Q, clause(-Q, true), S),
        comprimento(S,N), 
        N =< 1).

% Não pode existir o mesmo conhecimento positivo e negativo em simultâneo
+Q :: nao(-Q).

% Não pode existir o mesmo conhecimento positivo e negativo em simultâneo
+(-Q) :: nao(Q).

% Não pode existir exatamente o mesmo conhecimento negativo e desconhecido
+(-Q) :: (solucoes(Q,clause(excecao(Q), true),S),
                  comprimento(S,N), 
                  N == 0).

% Não permitir a insercao de conhecimento repetido para o adjudicante
+adjudicante(IdA,Nome,Nif,Morada)::((solucoes(IdA, adjudicante(IdA,A,B,C), Z), comprimento(Z,N), N=<1)).

% Não permitir a insercao de conhecimento repetido para a adjudicataria
+adjudicataria(IdAda,Nome,Nif,Morada)::((solucoes(IdAda, adjudicataria(IdAda,A,B,C), Z), comprimento(Z,N), N=<1)).

% Não permitir a insercao de conhecimento repetido para o contrato
+contrato(Id,IdA,IdAda,Tipo,Proc,Desc,Custo,Prazo,Local,Data) :: (solucoes(Id,(contrato(Id,A,B,C,D,E,F,G,H,I)),Z),
                  comprimento(Z,N), 
				  N =< 1).

% Não permite que remova o adjudicante caso este tenha contratos
-adjudicante(IdA,Nome,Nif,Morada) :: (solucoes((IdA),(contrato(Id,IdA,IdAda,Tipo,Proc,Desc,Custo,Prazo,Local,Data)),S),
                comprimento(S,N),
                N == 0).

% Não permite que remova a adjudicataria caso este tenha contratos
-adjudicataria(IdAda,Nome,Nif,Morada) :: (solucoes((IdAda),(contrato(Id,IdA,IdAda,Tipo,Proc,Desc,Custo,Prazo,Local,Data)),S),
                comprimento(S,N),
                N == 0).     

% Não permite inserir um contrato caso o adjudicante correspondente não esteja na base de conhecimento
+contrato(Id,IdA,IdAda,Tipo,Proc,Desc,Custo,Prazo,Local,Data) :: (solucoes(IdA,(adjudicante(IdA,Nome,Nif,Morada)),S),
                  comprimento(S,N), 
                  N == 1).

% Não permite inserir um cuidado caso a adjudicataria correspondente não esteja na base de conhecimento
+contrato(Id,IdA,IdAda,Tipo,Proc,Desc,Custo,Prazo,Local,Data) :: (solucoes(IdAda,(adjudicataria(IdAda,Nome,Nif,Morada)),S),
                  comprimento(S,N), 
                  N == 1).    

% Não permite inserção de conhecimento repetido para a data. 
+data(IdD,_,_,_) :: (solucoes(data(IdD,Ano,Mes,Dia), data(IdD,_,_,_), R), comprimento(R,N), N ==1).    

% Garante que duas datas com Id diferentes não têm os mesmo dados
+data(_,Ano,Mes,Dia) :: (solucoes((Ano,Mes,Dia), data(_,Ano,Mes,Dia), R), comprimento(R,N), N==1).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

+contrato(Id,IdA,IdAda,Tipo,ajuste_direto,Desc,Custo,Prazo,Local,IdData) :- 
    Custo > 5000,
    Prazo =< 365,
    Tipo = aquisicao_bens; 
    Tipo = locacao_bens;
    Tipo = aquisicao_servico.
    
+contrato(Id,IdA,IdAda,Tipo,Proc,Desc,Custo,Prazo,Local,IdData) :: (solucoes((IdA,IdAda),(contrato(Id,IdA,IdAda,Tipo,Proc,Desc,Custo,Prazo,Local,IdData)), S)),
    X = soma(S,0),
    X =< 75000.
    
soma([], R) :- R.
soma([contrato(Id,IdA,IdAda,Tipo,Proc,Desc,Custo,Prazo,Local,IdData)|T], R) :-
    R = R + Custo,
    soma(T, R).





%---------------------------------
%---------  TODO  ----------------
%---------------------------------

% Meter a só dar 3 tipos de porcedimento : ajuste_direto, consulta_previa, concurso_publico
% Meter o ajuste direto com as seguintes cenas: 
%     - Valor tem de ser menor que 5k
%     - O tipo de contrato é : aquisicao_bens, locacao_bens, aquisicao_servicos
%     - O prazo máximo é de 365 dias.
% Um adjudicante não pode fazer um contrato com o um adjucatario se:
%     - Nos ultimos 3 anos a soma do valor dos contratos for >= 75k


