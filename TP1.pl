--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Programacao em logica estendida TP1

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

:- op( 900,xfy,'::' ).
:- dynamic '-'/1.
:- dynamic adjudicante/4.
:- dynamic adjudicataria/4.
:- dynamic contrato/9.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%% extensão do predicado adjudicante: Questao -> { V, F, D }

adjudicante(1,antonio,245555555,rua da fonte 25 braga 0000).
adjudicante(2,bartolomeu,245555554,avenida 77 186 lisboa 0000).
adjudicante(3,carlos,245554554,travessa 77 186 porto 0000).
adjudicante(4,deolinda,243554554,rua da avenida 88 6 leiria 0000).
adjudicante(5,ermelinda,253554554,avenida dos carvalhos 16 viseu 0000).
adjudicante(6,fernanda,253654554,rua das benditas 116 faro 0000).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%% extensão do predicado adjudicataria: Questao -> { V, F, D }

adjudicataria(1,adj,16565655,rua da fonte 35 braga 0000).
adjudicataria(2,cataria,54545455,avenida 17 16 lisboa 0000).
adjudicataria(3,adju,54545455645,travessa 7 18 porto 0000).
adjudicataria(4,judi,54545444445,rua da avenida 8 6 leiria 0000).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%% extensão do predicado contrato: Questao -> { V, F, D }

contrato(1,1,a,a,adjudicar o adjudicante,777.88,12-12-2020,no sitio a,01-01-2019).
contrato(2,2,b,b,adjudicar o adjudicante,666.88,12-12-2021,no sitio b,02-02-2020).
contrato(3,3,c,c,adjudicar o adjudicante,785.88,01-10-2022,no sitio c,02-02-2019).
contrato(4,4,a,a,adjudicar o adjudicante,786.44,01-11-2021,no sitio d,02-12-2019).
contrato(5,1,a,a,adjudicar o adjudicante em termos de x,686.44,01-09-2022,no sitio a,05-12-2019).
contrato(6,1,b,b,adjudicar o adjudicante em termos de z,1044.44,01-11-2023,no sitio a,01-01-2020).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%% representação de conehcimento negativo

-adjudicante(7,gabriel,45646545,rua do cruzamento 77 porto 0000).

-adjudicataria(5,sfc,456465451,rua do cruzamento 88 lisboa 0000).

-adjudicante(A,B,C,D) :-
        nao(adjudicante(A,B,C,D)),
        nao(excepcao(adjudicante(A,B,C,D))).

-adjudicataria(A,B,C,D) :-
        nao(adjudicataria(A,B,C,D)),
        nao(excepcao(adjudicataria(A,B,C,D))).

-contrato(A,B,C,D,E,F,G,H,F) :-
        nao(contrato(A,B,C,D,E,F,G,H,F)),
        nao(excepcao(contrato(A,B,C,D,E,F,G,H,F))).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%% impossibilidade de inserção de conhecimento repetido

+adjudicante(A,_,_,_) :: (solucoes((A), (adjudicante(A,_,_,_)),S),
                         comprimento(S,N),
                        N=0).

+adjudicataria(A,_,_,_) :: (solucoes((A), (adjudicataria(A,_,_,_)),S),
                         comprimento(S,N),
                        N=0).

+contrato(A,B,C,D,E,F,G,H,F) :: (solucoes((A,B,C,D,E,F,G,H,F), (contrato(A,B,C,D,E,F,G,H,F)),S),
                         comprimento(S,N),
                        N=0).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%% impossibilidade de remover adjudicante dentro de contrato
%% ver como se manipula datas.

-adjudicante(solucoes((A,D), (contrato(A,_,_,_,_,_,D,_,_)),S),
                             percorrer S  e ver D>hoje).

-adjudicataria(solucoes((A,D), (contrato(_,A,_,_,_,_,D,_,_)),S),
                            percorrer S  e ver D>hoje).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%% representação de conhecimento desconhecido tipo 1

adjudicante(7,hercules,xpto,rua das beneditas 116 faro 0000).

excepcao(adjudicante(A,B,C,D)) :-
        adjudicante(A,B,xpto,D).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%% representação de conhecimento desconhecido tipo 2

excepcao(adjudicante(8,isabela,4564654654,rua das camelias 5 setubal 0000)).
excepcao(adjudicante(8,isabela,4564654654,rua das camelias 10 setubal 0000)).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
%% representação de conhecimento desconhecido tipo 3
%% ver nulo e adicionar invariante
nulo(xpp).

adjudicante(10,xpp,465456456,rua dos destinos 78 areosa 0000).

excepcao(adjudicante(A,B,C,D)) :-
        adjudicante(A,xpp,C,D).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a evolucao do conhecimento

evolucao( Termo ) :-
    solucoes( Invariante,+Termo::Invariante,Lista ),
    insercao( Termo ),
    teste( Lista ).

insercao( Termo ) :-
    assert( Termo ).
insercao( Termo ) :-
    retract( Termo ), !,fail.
	
teste( [] ).
teste( [R|LR] ) :-
    R,
    teste( LR ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a involucao do conhecimento

involucao( Termo ) :-
    solucoes( Invariante,-Termo::Invariante,Lista ),
    remocao( Termo ),
    teste( Lista ).

remocao( Termo ) :-
    retract( Termo ).
remocao( Termo ) :-
    assert( Termo ),!,fail.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

solucoes( X,Y,Z ) :-
    findall( X,Y,Z ).

comprimento( S,N ) :-
    length( S,N ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado demo: Questao,Resposta -> {V,F}
%                            Resposta = { verdadeiro,falso,desconhecido }

demo( Questao,verdadeiro ) :-
    Questao.
	
demo( Questao,falso ) :-
    -Questao.
	
demo( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).