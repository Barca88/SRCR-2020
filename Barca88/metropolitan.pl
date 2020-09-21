% SICStus PROLOG: Declaracoes iniciais
:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).
:- use_module(library(lists)).
:- set_prolog_flag(toplevel_print_options,
    [quoted(true), portrayed(true), max_depth(0)]).
% Meta predicados.

:-dynamic estacao/7. % ID GIS_ID Nome LINHA Morada X Y
:-dynamic ligacao/4. % ID1 ID2 Distancia

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Base de conhecimento

:-include(estacoes).
:-include(arcos). 

not(X) :-
        X, !, fail ; true.

/* 1 - Calcular um trajeto entre duas estações
I - Ínicio
F - Fim
R - Rota
R1 - Rota auxiliar

mapa(1,2).
mapaBF(52,71,Trajeto).
*/
mapa(I,F):- 
        mapaAux(I,[F],R), nl,
        escreverLista(R).

mapaAux(I,[I|R1],[I|R1]).
mapaAux(I,[F|R1],R):- 
        arco(X,F,_),    
        \+ memberchk(X,[F|R1]),   
        mapaAux(I,[X,F|R1],R).

% É utilizada na 5, 6 e 8
mapa2(I,F,R):- 
        mapaAux(I,[F],R). 
% ----------------------- BF ---------------------------

mapaBF(I, F, Rota) :-
        mapaBFAux([I],[],RevVisitados,F),
        removeNaoLigados(RevVisitados, Rota).
    
mapaBFAux([F|_], Historico, [F|Historico], F).
mapaBFAux([Node|RestQ], Historico, RevVisitados, F) :-
        findall(ProxNode, (arco(Node,ProxNode,_), 
                                \+ member(ProxNode, Historico), 
                                \+ member(ProxNode, RestQ)), Successors), 
        append(RestQ, Successors, Queue), 
        mapaBFAux(Queue, [Node|Historico], RevVisitados, F).    
    
% --- Função auxiliar
auxiliar([], Acc, Acc).
auxiliar([X], Acc, Resultado) :- auxiliar([], [X|Acc], Resultado).
auxiliar([Node1,Node2|Resto], Acc, Resultado) :- 
        \+arco(Node1,Node2,_),
        auxiliar([Node1|Resto], Acc, Resultado).
auxiliar([Node1,Node2|Resto], Acc, Resultado) :-
        arco(Node1,Node2,_),
        auxiliar([Node2|Resto], [Node1|Acc], Resultado).

removeNaoLigados(List, Resultado) :- auxiliar(List, [], Resultado).

% 2 - Selecionar  apenas estações com  uma  determinada caraterística, para um  determinado percurso


% 3 - Excluir uma ou mais características de estações para o percurso


/* 4 - Identificar quais as linhas com o maior número de possibilidades de saída num determinado percurso.
maiorSaidas([52,53,54,51,27,25,24,23,8,6,88,89,91,9,86,59,87,29,28,67,71]).
*/
maiorSaidas(Percurso) :-
        findall(Cor,(member(X,Percurso),
                        estacao(X,_,_,Linhas,_,_,_),
                        member(Cor,Linhas)),Cores),
        escreveCores(Cores).

escreveCores(Lista) :- nl,
        write('vermelho = '),
        count(Lista,vermelho,Vermelho),
        write(Vermelho), nl,
        write('verde = '),
        count(Lista,verde,Verde),
        write(Verde), nl,
        write('amarelo = '),
        count(Lista,amarelo,Amarelo),
        write(Amarelo), nl,
        write('azul = '),
        count(Lista,azul,Azul),
        write(Azul), nl,
        write('laranja = '),
        count(Lista,laranja,Laranja),
        write(Laranja), nl,
        write('prateado = '),
        count(Lista,prateado,Prateado),
        write(Prateado).

% count(L, X, R) Conta as as ocorrencias de X em L e guarda em R
count([],X,0).
count([X|T],X,Y):- count(T,X,Z), Y is 1+Z.
count([X1|T],X,Z):- X1\=X, count(T,X,Z).

countall(List,X,C) :-
    sort(List,List1),
    member(X,List1),
    count(List,X,C).                                
 
/* 5 - Escolher o menor percurso (usando o critério do menor número de estações intermédias)
menorPercurso(60,10).
*/
menorPercurso(I, F) :-
        menorPercursoAux(I, F, R),
        cabeca(R, X),
        par(X,Caminho), nl,
        escreverLista(Caminho), nl,
        escreverTrajeto(Caminho).

menorPercursoAux(I, F, R) :-
        setof((NEstacoes,Caminho),
                (mapa2(I,F,Caminho),
                length(Caminho,NEstacoes)), R).

/* 6 - Escolher o percurso mais rápido (usando o critério da distância) 
maisRapido(60,63).
*/ 
maisRapido(I, F) :-
        maisRapidoAux(I, F, R),
        cabeca(R, X),
        par(X,Caminho), nl,
        escreverLista(Caminho), nl,
        escreverTrajeto(Caminho).

maisRapidoAux(I,F,R) :-
        setof((Distancia,Caminho),
                (mapa2(I,F,Caminho),
                distanciaCaminho(Caminho,Distancia)), R).

distanciaCaminho([X|T], Resultado) :-
        distanciaCaminho(T, X, 0, Resultado).
distanciaCaminho([], _, D, D).
distanciaCaminho([Y|T], X, Distancia, Resultado) :-
        distancia(X, Y, D),
        D2 is Distancia + D,
        distanciaCaminho(T, Y, D2, Resultado).

distancia(Id1,Id2,D) :-
        estacao(Id1,_,_,_,_,X1,Y1),
        estacao(Id2,_,_,_,_,X2,Y2),
        D is sqrt((X2-X1)^2 + (Y2-Y1)^2).
/* 7 - Escolher o percurso que passe apenas por uma determinada linha
 
mapaLinhasBF(52,72,[vermelho,laranja]).
mapaLinhasBF(88,60,[azul]).
mapaLinhasBF(88,60,[amarelo]).

*/
% ----------------------- BF ---------------------------
mapaLinhasBF(I, F, Linhas) :-
        mapaLinhasBFAux([I], [], RevVisitados, F, Linhas),
        removeNaoLigados(RevVisitados, Rota), nl,
        escreverLista(Rota), nl,
        escreverTrajeto(Rota).
            
mapaLinhasBFAux([F|_], Historico, [F|Historico], F, Linhas).
mapaLinhasBFAux([Node|RestQ], Historico, RevVisitados, F,Linhas) :-
        findall(ProxNode, (arco(Node, ProxNode, Cor),
                                member(Cor, Linhas), 
                                \+ member(ProxNode, Historico), 
                                \+ member(ProxNode, RestQ)), Successors), 
        append(RestQ, Successors, Queue), 
        mapaLinhasBFAux(Queue, [Node|Historico], RevVisitados, F, Linhas).



/* 8 - Escolher uma ou mais linhas por onde o percurso deverá passar, dadas duas estações.
escolherPercursos(1,88).
escolherPercursos(52,71).
*/

escolherPercursos(I,F) :-
        setof(Percurso,mapa2(I,F,Percurso),Lista),
        escolherPercursosAux(Lista).

escolherPercursosAux([]).
escolherPercursosAux([H|T]) :- nl,
        escreverLista(H), nl,
        coresCaminho(H,R), 
        escreveCores(R), nl,
        escolherPercursosAux(T).

coresCaminho([X|T], Resultado) :-
        coresCaminho(T, X, [], Resultado).
coresCaminho([], _, Linhas, Linhas).
coresCaminho([Y|T], X, Linhas, Resultado) :-
        setof(Cor,arco(X, Y, Cor),Cores),
        append(Linhas, Cores, NovaLinhas),
        coresCaminho(T, Y, NovaLinhas, Resultado).


% Funções auxiliares

% Adiciona elementos sem repetidos
cabeca([R|T],R).
cabeca([R],R).
cabeca([],vazio).

par(vazio,vazio).
par((X,Y),Y).


escreverTrajeto(vazio) :-
        write('Erro!').
escreverTrajeto([Id]):-
        estacao(Id,_,Nome,_,_,_,_),
        write(Nome).
escreverTrajeto([H|T]):-
        estacao(H,_,Nome,_,_,_,_),
        write(Nome),
        write(' -> '),
        escreverTrajeto(T).


escreverLista(vazio) :-
        write('Erro!').
escreverLista([X]) :-
        write(X).
escreverLista([H|T]) :- 
        write(H),
        write(' -> '),
        escreverLista(T).

escreverListaDeListas([H]) :- nl,
        escreverLista(H).
escreverListaDeListas([H|T]) :- nl,
        escreverLista(H),
        write(' =>> '),
        escreverListaDeListas(T).