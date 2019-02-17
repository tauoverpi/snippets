:- module append.
:- interface.
:- import_module list.
:- pred append0(list(T), list(T), list(T)).
:- mode append0(in     , in    , out) is det.
:- mode append0(out    , out   , in) is multi.
:- implementation.

append0([], Ys, Ys).
append0([X | Xs], Ys, Zs) :-
  append0(Xs, Ys, Zs0), Zs = [X | Zs0].
