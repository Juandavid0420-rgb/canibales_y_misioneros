% ==========================================================
%  Misioneros y Caníbales - BFS "Amplitud Limitada" (2 hijos)
%  Versión didáctica para SWI-Prolog / SWISH
% ==========================================================
%
%  Idea:
%   - Estrategia de amplitud por niveles (como BFS),
%     pero cada vez que un nodo es elegido, SOLO encola
%     hasta 2 de sus hijos y re-encola el mismo nodo con
%     sus hijos restantes (nodo "parcialmente expandido").
%
%  Estado:
%     state(M_izq, C_izq, Barca)
%     - M_izq: # misioneros en la orilla izquierda
%     - C_izq: # caníbales  en la orilla izquierda
%     - Barca:  left  |  right   (lado donde está la barca)
%
%  Meta (para 3 y 3):
%     state(0, 0, right)
%
%  Ejecución:
%     ?- solve(Path).
%     Muestra los estados de la solución y unifica Path.
%
% ==========================================================

:- use_module(library(apply)).   % para exclude/3
:- dynamic visited/1.            % conjunto de estados ya visitados

% ----------------- Parámetros del problema -----------------

total_m(3).     % Total de misioneros
total_c(3).     % Total de caníbales

% Movimientos posibles (M,C) que el bote puede transportar
% Capacidad 2: dos misioneros, dos caníbales, uno y uno, uno solo, o un caníbal solo.
moves([
    (2,0),
    (0,2),
    (1,1),
    (1,0),
    (0,1)
]).

% Estado inicial y estado meta
initial_state(state(3,3,left)).
goal_state(state(0,0,right)).

% ----------------- Punto de entrada -----------------

% solve(-Path)
%  Limpia visitados, arma la cola inicial con el nodo raíz,
%  corre el BFS limitado a 2 hijos por turno, invierte el camino
%  (porque lo vamos guardando al revés) y lo imprime bonito.
solve(Path) :-
    retractall(visited(_)),                        % 1) Limpiar memoria de visitados
    initial_state(S0),                             % 2) Tomar estado inicial
    children_of(S0, ChildStates0),                 % 3) Generar sus hijos legales
    assertz(visited(S0)),                          % 4) Marcar S0 como visitado
    bfs_limit2([node(S0, [S0], ChildStates0)],     % 5) Cola inicial con 1 nodo: (Estado, Camino, HijosPend)
               PathRev),                           %    PathRev lo devuelve al revés
    reverse(PathRev, Path),                        % 6) Dar Path en orden de inicio→meta
    pretty_path(Path).                             % 7) Imprimir estados

% ----------------- BFS "Amplitud Limitada (2)" -----------------

% bfs_limit2(+OpenQueue, -SolutionPathRev)
%  Regla 1: Si el primero de la cola ya es meta, terminamos y devolvemos su Path (reversa).
bfs_limit2([node(S, Path, _Pending)|_], Path) :-
    goal_state(S), !.

%  Regla 2: Tomamos el primer nodo, encolamos (máx.) 2 hijos suyos y re-encolamos el mismo nodo si le quedan hijos.
bfs_limit2([node(S, Path, Pending)|RestQueue], SolutionPath) :-
    take_up_to_two(Pending, ToEnqueue, Remaining),          % A) Sacar hasta 2 hijos
    enqueue_children(ToEnqueue, Path, NewNodes),             % B) Crear nodos hijos (con sus propios hijos)
    ( Remaining = [] ->
        Queue1 = RestQueue                                   % C1) Si no quedan hijos: no re-encolamos el mismo
    ;   enqueue_end(RestQueue,                               % C2) Si quedan: re-encolamos el MISMO nodo al final
                     [node(S, Path, Remaining)], Queue1)
    ),
    enqueue_end(Queue1, NewNodes, Queue2),                   % D) Encolar hijos nuevos al final de la cola
    bfs_limit2(Queue2, SolutionPath).                        % E) Continuar

% ----------------- Utilidades de cola -----------------

% enqueue_end(+Queue, +Items, -QueueOut)
%  Agrega (concatena) una lista de Items al final de la cola.
enqueue_end(Q, Items, Qout) :- append(Q, Items, Qout).

% take_up_to_two(+List, -Taken, -Rest)
%  Toma hasta 2 elementos de la lista: los que van a encolarse ahora (Taken)
%  y deja el resto (Rest).
take_up_to_two([], [], []).
take_up_to_two([A], [A], []).
take_up_to_two([A,B|Rest], [A,B], Rest).

% ----------------- Generación y encolado de hijos -----------------

% enqueue_children(+States, +ParentPath, -NodesOut)
%  Para cada estado hijo:
%    - Si no está visitado, lo marca visitado,
%    - calcula sus propios hijos legales,
%    - y construye el "node(ChildState, NewPath, ChildPending)".
enqueue_children([], _Path, []).
enqueue_children([Schild|More], Path, NodesOut) :-
    ( visited(Schild) ->
        % Ya estaba visitado: lo descartamos
        enqueue_children(More, Path, NodesOut)
    ;   assertz(visited(Schild)),                    % Marcar visitado
        children_of(Schild, GrandChildren),          % Hijos del hijo
        Node = node(Schild, [Schild|Path], GrandChildren),
        enqueue_children(More, Path, Tail),
        NodesOut = [Node|Tail]
    ).

% ----------------- Sucesores legales -----------------

% children_of(+State, -Children)
%  Encuentra todos los Next que sean sucesores legales de State
%  (que respeten límites y "seguridad"), elimina duplicados exactos
%  y excluye los ya visitados.
children_of(State, Children) :-
    findall(Next,
            ( successor(State, Next),
              safe(Next)
            ),
            All),
    sort(All, Unique),
    exclude(visited, Unique, Children).

% successor(+State, -Next)
%  Genera un sucesor "aplicando un movimiento" según el lado de la barca.
%  Si la barca está a la izquierda, restamos (M,C) de la orilla izq;
%  si está a la derecha, sumamos (M,C) a la orilla izq.
successor(state(Ml,Cl,left), state(Ml2,Cl2,right)) :-
    moves(Ms),
    member((Dm,Dc), Ms),
    Ml2 is Ml - Dm, Cl2 is Cl - Dc,
    bounds_ok(Ml2, Cl2).

successor(state(Ml,Cl,right), state(Ml2,Cl2,left)) :-
    moves(Ms),
    member((Dm,Dc), Ms),
    Ml2 is Ml + Dm, Cl2 is Cl + Dc,
    bounds_ok(Ml2, Cl2).

% bounds_ok(+M_left, +C_left)
%  Verifica que el estado esté dentro de [0..total] para M y C.
bounds_ok(Ml, Cl) :-
    total_m(Tm), total_c(Tc),
    Ml >= 0, Cl >= 0, Ml =< Tm, Cl =< Tc.

% safe(+State)
%  Verifica la "seguridad" en ambas orillas:
%  - En cada lado, si hay misioneros (>0), no pueden ser menos que caníbales.
safe(state(Ml, Cl, _)) :-
    total_m(Tm), total_c(Tc),
    Mr is Tm - Ml, Cr is Tc - Cl,   % M y C en la orilla derecha
    (Ml = 0 ; Ml >= Cl),
    (Mr = 0 ; Mr >= Cr).

% ----------------- Impresión de la solución -----------------

% pretty_path(+Path)
%  Muestra longitud y todos los estados de la solución.
pretty_path(Path) :-
    nl, write('Solucion en '), length(Path,N), write(N), write(' estados:'), nl,
    maplist(print_state, Path), nl.

% print_state(+State)
%  Muestra un estado como:
%    Izq: M=x C=y | Der: M=x C=y | Barca=left/right
print_state(state(Ml,Cl,Side)) :-
    total_m(Tm), total_c(Tc),
    Mr is Tm - Ml, Cr is Tc - Cl,
    format('Izq: M=~w C=~w  |  Der: M=~w C=~w  | Barca=~w~n', [Ml,Cl,Mr,Cr,Side]).
