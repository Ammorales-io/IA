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
pertenece_m(X, [Y|_]) :- Y \= [_|_], X=Y.
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
insert([X-P], [A-Q|L1], R) :- P<Q, concatena([X-P], [A-Q|L1], R).
insert([X-P], [A-Q|L1], [A-Q|L2]) :- insert([X-P], L1, L2).

%------------------%
%   Ejercicio 4.1  %
%------------------%
elem_count(X, [], 0).
elem_count(X, [X|Ls], C1) :- elem_count(X, Ls, C), C1 is C + 1.
elem_count(X, [_|Ls], C) :- elem_count(X, Ls, C).

%------------------%
%   Ejercicio 4.2  %
%------------------%
