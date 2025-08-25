% ==========================================================
%  Problema de Misioneros y Caníbales
%  Búsqueda por Amplitud Limitada (2 hijos por turno)
%  Ejecutar: resolver(Camino).
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
                      CaminoInvertido),
    reverse(CaminoInvertido, Camino),
    mostrar_camino(Camino).

% ----------------- Búsqueda por Amplitud Limitada -----------------

amplitud_limitada([nodo(Estado, Camino, _HijosNoUsados)|_], Camino) :-
    estado_meta(Estado), !.

amplitud_limitada([nodo(Estado, Camino, HijosPorExpandir)|ColaResto], Solucion) :-
    tomar_dos_hijos(HijosPorExpandir, HijosSeleccionados, HijosPendientesDespues),
    encolar_hijos(HijosSeleccionados, Camino, NuevosNodos),
    ( HijosPendientesDespues = [] ->
        ColaIntermedia = ColaResto
    ;   agregar_final(ColaResto,
                      [nodo(Estado, Camino, HijosPendientesDespues)],
                      ColaIntermedia)
    ),
    agregar_final(ColaIntermedia, NuevosNodos, ColaActualizada),
    amplitud_limitada(ColaActualizada, Solucion).

% ----------------- Utilidades de cola -----------------

agregar_final(Cola, Elementos, ColaNueva) :- append(Cola, Elementos, ColaNueva).

tomar_dos_hijos([], [], []).
tomar_dos_hijos([Hijo1], [Hijo1], []).
tomar_dos_hijos([Hijo1,Hijo2|Restantes], [Hijo1,Hijo2], Restantes).

% ----------------- Generación y encolado de hijos -----------------

encolar_hijos([], _CaminoHastaPadre, []).
encolar_hijos([Hijo|OtrosHijos], CaminoHastaPadre, NodosSalida) :-
    ( visitado(Hijo) ->
        encolar_hijos(OtrosHijos, CaminoHastaPadre, NodosSalida)
    ;   assertz(visitado(Hijo)),
        generar_hijos(Hijo, Nietos),
        Nodo = nodo(Hijo, [Hijo|CaminoHastaPadre], Nietos),
        encolar_hijos(OtrosHijos, CaminoHastaPadre, NuevosDelResto),
        NodosSalida = [Nodo|NuevosDelResto]
    ).

% ----------------- Sucesores -----------------

generar_hijos(Estado, Hijos) :-
    findall(Siguiente,
            ( sucesor(Estado, Siguiente),
              estado_seguro(Siguiente)
            ),
            TodosLosSucesores),
    sort(TodosLosSucesores, SucesoresSinDuplicados),
    exclude(visitado, SucesoresSinDuplicados, Hijos).

% Barca en la izquierda: restar lo que se mueve desde la izquierda
sucesor(estado(MisionerosIzq, CanibalesIzq, izquierda),
        estado(NuevosMisionerosIzq, NuevosCanibalesIzq, derecha)) :-
    movimientos(ListaMovimientos),
    member((MisionerosMover, CanibalesMover), ListaMovimientos),
    NuevosMisionerosIzq is MisionerosIzq - MisionerosMover,
    NuevosCanibalesIzq is CanibalesIzq - CanibalesMover,
    dentro_limites(NuevosMisionerosIzq, NuevosCanibalesIzq).

% Barca en la derecha: sumar lo que regresa a la izquierda
sucesor(estado(MisionerosIzq, CanibalesIzq, derecha),
        estado(NuevosMisionerosIzq, NuevosCanibalesIzq, izquierda)) :-
    movimientos(ListaMovimientos),
    member((MisionerosMover, CanibalesMover), ListaMovimientos),
    NuevosMisionerosIzq is MisionerosIzq + MisionerosMover,
    NuevosCanibalesIzq is CanibalesIzq + CanibalesMover,
    dentro_limites(NuevosMisionerosIzq, NuevosCanibalesIzq).

dentro_limites(MisionerosIzq, CanibalesIzq) :-
    total_misioneros(TotalMisioneros),
    total_canibales(TotalCanibales),
    MisionerosIzq >= 0, CanibalesIzq >= 0,
    MisionerosIzq =< TotalMisioneros, CanibalesIzq =< TotalCanibales.

estado_seguro(estado(MisionerosIzq, CanibalesIzq, _)) :-
    total_misioneros(TotalMisioneros),
    total_canibales(TotalCanibales),
    MisionerosDer is TotalMisioneros - MisionerosIzq,
    CanibalesDer is TotalCanibales - CanibalesIzq,
    (MisionerosIzq = 0 ; MisionerosIzq >= CanibalesIzq),
    (MisionerosDer = 0 ; MisionerosDer >= CanibalesDer).

% ----------------- Impresión de la solución -----------------

mostrar_camino(Camino) :-
    nl, write('Solucion en '), length(Camino, NumeroEstados),
    write(NumeroEstados), write(' estados:'), nl,
    maplist(imprimir_estado, Camino), nl.

imprimir_estado(estado(MisionerosIzq, CanibalesIzq, Barca)) :-
    total_misioneros(TotalMisioneros),
    total_canibales(TotalCanibales),
    MisionerosDer is TotalMisioneros - MisionerosIzq,
    CanibalesDer is TotalCanibales - CanibalesIzq,
    format('Izq: M=~w C=~w  |  Der: M=~w C=~w  | Barca=~w~n',
          [MisionerosIzq, CanibalesIzq, MisionerosDer, CanibalesDer, Barca]).
