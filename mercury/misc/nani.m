:- module nani.
:- interface.
:- import_module io.
:- pred main(state::di, state::uo) is det.
:- implementation.
:- import_module solutions, list.

main(!IO) :- write_string("hi", !IO).

% prolog is dunamically typed so we encode the universe of possible types
% and leave it to runtime unification to solve as in the original tutorial.
% A type safe version where this is solved statically should be written
% shortly.
:- type universe --->
  kitchen; office; hall; dining_room; cellar;
  apple; flashlight; nani; broccoli; crackers; computer;
  washing_machine; desk.

:- pred room(universe::in) is semidet.
room(kitchen).
room(office).
room(hall).
room(dining_room).
room(cellar).

:- pred location(universe::in, universe::out) is semidet.
location(desk, office).
location(apple, kitchen).
location(flashlight, desk).
location(washing_machine, cellar).
location(nani, washing_machine).
location(broccoli, kitchen).
location(crackers, kitchen).
location(computer, office).

:- pred door(universe, universe).
:- mode door(in, out) is nondet.
:- mode door(out, in) is semidet.
door(office, hall).
door(kitchen, office).
door(hall, dining_room).
door(kitchen, cellar).
door(dining_room, kitchen).

:- pred edible(universe::in) is semidet.
edible(apple).
edible(crackers).

:- pred yucky(universe::in) is semidet.
yucky(broccoli).

:- pred turned_off(universe::in) is semidet.
turned_off(flashlight).

:- pred here(universe::in) is semidet.
here(kitchen).

