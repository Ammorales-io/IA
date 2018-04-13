%-----------------------------------------------
% 
%     Lab assignment 3: Prolog
%     LAB GROUP: 2363
%     Couple: 10
%     Author 1: Celia San Gregorio Moreno 
%     Author 2: Álvaro Martínez Morales
%
%-----------------------------------------------

%----------------%
%   Ejercicio 1  %
%----------------%
pertenece_m(X, [X|_]) :- X \= [_|_].
pertenece_m(X, [L|Rs]) :- pertenece_m(X, L); pertenece_m(X, Rs).

%----------------%
%   Ejercicio 2  %
%----------------%
concatena([], L, L).
concatena([X|L1], L2, [X|L3]) :- concatena(L1, L2, L3).

invierte([], []).
invierte([X|R], L) :- invierte(R, L1), concatena(L1, [X], L).

%----------------%
%   Ejercicio 3  %
%----------------%
insert([X-P], [], [X-P]).
insert([X-P], [A-Q|Ls], R) :- P=<Q, concatena([X-P], [A-Q|Ls], R).
insert([X-P], [A-Q|Ls], [A-Q|Rs]) :- P>Q, insert([X-P], Ls, Rs).

%------------------%
%   Ejercicio 4.1  %
%------------------%
elem_count(_, [], 0).
elem_count(X, [X|Ls], C1) :- elem_count(X, Ls, C), C1 is C + 1.
elem_count(X, [Y|Ls], C1) :- X\=Y, elem_count(X, Ls, C1).

%------------------%
%   Ejercicio 4.2  %
%------------------%
list_count([], [_|_], []).
list_count([X|Ls], L2, [X-C|Rs]) :- elem_count(X, L2, C), list_count(Ls, L2, Rs).

%----------------%
%   Ejercicio 5  %
%----------------%
sort_list([],[]).
sort_list([X-P|L1], L3) :- sort_list(L1, L2), insert([X-P], L2, L3).

%----------------%
%   Ejercicio 6  %
%----------------%
build_tree([], tree(nil, nil, nil)).
build_tree([X], tree(X, nil, nil).
build_tree([X|RL], RE) :- build_tree(X, N) 

