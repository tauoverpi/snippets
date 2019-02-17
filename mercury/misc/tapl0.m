:- module tapl0.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module solutions.

:- type info ---> info(string).

:- type term --->
  tm_true(info);
  tm_false(info);
  tm_zero(info);
  tm_succ(info, term);
  tm_pred(info, term);
  tm_is_zero(info, term);
  tm_if(info, term, term, term).

:- pred evaluate0(term, term).
:- mode evaluate0(in, out) is nondet.

:- pred is_number(term::in) is semidet.
is_number(tm_zero(_)).
is_number(tm_succ(_, N)) :- is_number(N).
is_number(tm_pred(_, N)) :- is_number(N).

evaluate0(tm_true(_), tm_true(dummy)).
evaluate0(tm_false(_), tm_false(dummy)).
evaluate0(tm_zero(_), tm_zero(dummy)).
evaluate0(tm_succ(_, N), tm_succ(dummy, N)) :- is_number(N).
evaluate0(tm_pred(_, tm_succ(dummy, N)), N) :- is_number(N).
evaluate0(tm_pred(_, tm_zero(_)), tm_zero(dummy)).
evaluate0(tm_is_zero(_, tm_zero(_)), tm_true(dummy)).
evaluate0(tm_is_zero(_, N), R) :- is_number(N0), evaluate0(N, N0), R = tm_true(dummy).
evaluate0(tm_if(_, tm_true(_), C, _), C).
evaluate0(tm_if(_, tm_false(_), _, A), A).
evaluate0(tm_if(I, Tm, A, C), R) :- evaluate0(Tm, T0), R = tm_if(I, T0, A, C).

:- func dummy = info.
dummy = info("").

:- func program = term.
program = tm_if( dummy
               , tm_is_zero(dummy, tm_pred(dummy, tm_succ(dummy, tm_zero(dummy))))
               , tm_false(dummy)
               , tm_true(dummy)
               ).

main(!IO) :- solutions(evaluate0(program), R), print(R, !IO), nl(!IO).
