% ==========================================================
%  Problema de Misioneros y Caníbales
%  Búsqueda por Amplitud Limitada (2 hijos por turno)
%  Versión didáctica con nombres en español
% ==========================================================

:- use_module(library(apply)).   
:- dynamic visitado/1.            % conjunto de estados ya visitados

% ----------------- Parámetros del problema -----------------

total_misioneros(3).     % Total de misioneros
total_canibales(3).      % Total de caníbales

% Movimientos posibles (Misioneros, Caníbales) que el bote puede transportar
movimientos([
    (2,0),
    (0,2),
    (1,1),
    (1,0),
    (0,1)
]).

% Estado inicial y estado meta
estado_inicial(estado(3,3,izquierda)).
estado_meta(estado(0,0,derecha)).

% ----------------- Punto de entrada -----------------

resolver(Camino) :-
    retractall(visitado(_)),                                
    estado_inicial(EstadoInicial),                         
    generar_hijos(EstadoInicial, HijosIniciales),           
    assertz(visitado(EstadoInicial)),                       
    amplitud_limitada([nodo(EstadoInicial, [EstadoInicial], HijosIniciales)], 
                      CaminoReves),                         
    reverse(CaminoReves, Camino),                          
    mostrar_camino(Camino).                                 

% ----------------- Búsqueda por Amplitud Limitada -----------------

amplitud_limitada([nodo(Estado, Camino, _Pendientes)|_], Camino) :-
    estado_meta(Estado), !.

amplitud_limitada([nodo(Estado, Camino, HijosPendientes)|ColaResto], Solucion) :-
    tomar_dos_hijos(HijosPendientes, HijosAhora, HijosRestantes),          
    encolar_hijos(HijosAhora, Camino, NuevosNodos),                        
    ( HijosRestantes = [] ->
        Cola1 = ColaResto                                   
    ;   agregar_final(ColaResto, [nodo(Estado, Camino, HijosRestantes)], Cola1)
    ),
    agregar_final(Cola1, NuevosNodos, Cola2),                               
    amplitud_limitada(Cola2, Solucion).                                     

% ----------------- Utilidades de cola -----------------

agregar_final(Cola, Elementos, ColaNueva) :- append(Cola, Elementos, ColaNueva).

tomar_dos_hijos([], [], []).
tomar_dos_hijos([A], [A], []).
tomar_dos_hijos([A,B|Resto], [A,B], Resto).

% ----------------- Generación y encolado de hijos -----------------

encolar_hijos([], _Camino, []).
encolar_hijos([Hijo|Mas], CaminoPadre, NodosSalida) :-
    ( visitado(Hijo) ->
        encolar_hijos(Mas, CaminoPadre, NodosSalida)
    ;   assertz(visitado(Hijo)),                    
        generar_hijos(Hijo, Nietos),                
        Nodo = nodo(Hijo, [Hijo|CaminoPadre], Nietos),
        encolar_hijos(Mas, CaminoPadre, ColaNuevos),
        NodosSalida = [Nodo|ColaNuevos]
    ).

% ----------------- Sucesores legales -----------------

generar_hijos(Estado, Hijos) :-
    findall(Siguiente,
            ( sucesor(Estado, Siguiente),
              estado_seguro(Siguiente)
            ),
            Todos),
    sort(Todos, Unicos),
    exclude(visitado, Unicos, Hijos).

sucesor(estado(MisionerosIzq,CanibalesIzq,izquierda),
        estado(NuevosM, NuevosC, derecha)) :-
    movimientos(Movs),
    member((M,C), Movs),
    NuevosM is MisionerosIzq - M, 
    NuevosC is CanibalesIzq - C,
    dentro_limites(NuevosM, NuevosC).

sucesor(estado(MisionerosIzq,CanibalesIzq,derecha),
        estado(NuevosM, NuevosC, izquierda)) :-
    movimientos(Movs),
    member((M,C), Movs),
    NuevosM is MisionerosIzq + M, 
    NuevosC is CanibalesIzq + C,
    dentro_limites(NuevosM, NuevosC).

dentro_limites(MisionerosIzq, CanibalesIzq) :-
    total_misioneros(Tm), total_canibales(Tc),
    MisionerosIzq >= 0, CanibalesIzq >= 0, 
    MisionerosIzq =< Tm, CanibalesIzq =< Tc.

estado_seguro(estado(MisionerosIzq, CanibalesIzq, _)) :-
    total_misioneros(Tm), total_canibales(Tc),
    MisionerosDer is Tm - MisionerosIzq, 
    CanibalesDer is Tc - CanibalesIzq,
    (MisionerosIzq = 0 ; MisionerosIzq >= CanibalesIzq),
    (MisionerosDer = 0 ; MisionerosDer >= CanibalesDer).

% ----------------- Impresión de la solución -----------------

mostrar_camino(Camino) :-
    nl, write('Solucion en '), length(Camino,N), write(N), write(' estados:'), nl,
    maplist(imprimir_estado, Camino), nl.

imprimir_estado(estado(MisionerosIzq,CanibalesIzq,Barca)) :-
    total_misioneros(Tm), total_canibales(Tc),
    MisionerosDer is Tm - MisionerosIzq, 
    CanibalesDer is Tc - CanibalesIzq,
    format('Izq: M=~w C=~w  |  Der: M=~w C=~w  | Barca=~w~n', 
          [MisionerosIzq,CanibalesIzq,MisionerosDer,CanibalesDer,Barca]).
