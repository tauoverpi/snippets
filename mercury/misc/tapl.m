%----------------------------------------------------------------------------%
% FILENAME : tapl.m
% AUTHOR   : Simon Nielsen Knights
% COPYRIGHT: Copyright © 2019 Simon Nielsen Knights <tauoverpi@yandex.com>
% LICENSE  : MIT
% CREATED  : Fri Feb 15, 2019  03:45AM
% MODIFIED : Fri Feb 15, 2019  04:15AM
%----------------------------------------------------------------------------%
% TBD: write documentation
%----------------------------------------------------------------------------%

:- module tapl.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%----------------------------------------------------------------------------%

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module solutions.

:- type term --->
  tm_if(term, term, term);
  tm_is_zero(term);
  tm_pred(term);
  tm_succ(term);
  tm_zero;
  tm_true;
  tm_false.

:- pred is_numerical(tapl.term).
:- mode is_numerical(in) is semidet.

is_numerical(tm_succ(N)) :- is_numerical(N).
is_numerical(tm_pred(N)) :- is_numerical(N).
is_numerical(tm_zero).

:- pred step(term, term).
:- mode step(in, out) is nondet.

step(Tm, Nm) :-
  (
    Tm = tm_if(T, C, A),
    step(T, T0),
    step(C, C0),
    step(A, A0),
    (
      T0 = tm_true,
      Nm = C0
    ;
      T0 = tm_false,
      Nm = A0
    )
  ;
    Tm = tm_is_zero(Z),
    (
      Z = tm_zero,
      Nm = tm_true
    ;
      Nm = tm_false
    )
  ;
    Tm = tm_pred(N),
    is_numerical(N),
    Nm = Tm
  ;
    Tm = tm_pred(tm_succ(N)),
    is_numerical(N),
    Nm = N
  ;
    Tm = tm_succ(N),
    is_numerical(N),
    Nm = Tm
  ;
    Tm = tm_zero,
    Nm = Tm
  ;
    Tm = tm_true,
    Nm = Tm
  ;
    Tm = tm_false,
    Nm = Tm
  ).

%----------------------------------------------------------------------------%

main(!IO) :- solutions(step(example), R), print(R, !IO).

:- func example = tapl.term.
example = tm_if(tm_is_zero(tm_succ(tm_zero)), tm_true, tm_false).

%----------------------------------------------------------------------------%
:- end_module tapl.
%----------------------------------------------------------------------------%
