:- module food.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module int, string, list.

main --> write_string("hi").

:- type quantity --->
  grams(int);
  kilograms(int);
  liters(int);
  milliliters(int);
  deciliters(int);
  tablespoons(int);
  teaspoons(int);
  numberof(int);
  arbitrary(string, int).

:- type measurement --->
  centimeters(int);
  millimeters(int).

:- type ingredient ---> ingredient(string, quantity).
:- type method     ---> method(list(string)).
:- type recepie    ---> recepie(string, list(ingredient), method).

:- pred r_tea(list(ingredient), recepie).
:- mode r_tea(in, out) is cc_nondet.

r_tea([], _) :- false.
r_tea([I | _], R) :-
  I = ingredient("teabag", numberof(N)),
  N >= 1,
  M = method([ "boil water"
             , "place teabag in the cup"
             , "pour the hot water in the cup"
             , "stir until satisfied"
             ]),
  R = recepie("tea", [I], M).
r_tea([_ | Xs], R) :- r_tea(Xs, R).
