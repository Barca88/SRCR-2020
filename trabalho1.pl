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

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Base de conhecimento com informação dos adjudicantes, adjudicatarios e de contratos.

:- dynamic adjudicante/4.
:- dynamic adjudicataria/4.
:- dynamic contrato/10.
:- dynamic data/4.
:- dynamic (-)/1.
:- dynamic excecao/1.

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
%Extensão do predicado adjudicante: IdA, Nome, Nif, Morada -> {V,F}

adjudicante(1, cmb, 123456789, braga).
%(...)

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%Extensão do predicado adjudicataria: IdAda, Nome, Nif, Morada -> {V,F}

adjudicataria(1, aaum, 420123954, braga).
%(...)

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%Extensão do predicado contrato: Id, IdA, IdAda, Tipo, Procedimento, Descricao, Valor, Prazo, Local, Data -> {V,F}

contrato(1, 1, 1, aquisicao_servico, ).
%(...)

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