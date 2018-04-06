invertlist([], []).
invertlist([X|R], L) :- invertlist(R, L1), append(L1, [X], L).