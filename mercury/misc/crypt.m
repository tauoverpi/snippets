:- module crypt.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is cc_multi.
:- implementation.
:- import_module int, list, string.

main(!IO) :-
  io.format("DOG + ANT = CAT\n", [], !IO),
  ( if
      Ds0 = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9], C0 = 0,
      pick(Ds0, G, Ds1),
      pick(Ds1, T, Ds2), S1 = G+T+C0, T = S1 mod 10, C1 = S1/10,
      pick(Ds2, O, Ds3),
      pick(Ds3, N, Ds4), S2 = O+N+C1, A = S2 mod 10, C2 = S2/10, A \= 0,
      pick(Ds4, D, Ds5),
      pick(Ds5, A, Ds6), S3 = D+A+C2, C = S3 mod 10, C3 = S3/10, C \= 0,
      pick(Ds6, C, _),
      C3 = 0
    then
      DOG = 100 * D + 10 * O + G,
      ANT = 100 * A + 10 * N + T,
      CAT = 100 * C + 10 * A + T,
      io.format("%d + %d = %d\n", [i(DOG), i(ANT), i(CAT)], !IO)
    else
      io.format("no solutions\n", [], !IO)
  ).

:- pred pick(list(int)::in, int::out, list(int)::out) is nondet.
pick([X | Xs], X, Xs).
pick([X | Xs], Y, [X | Zs]) :- pick(Xs, Y, Zs).
