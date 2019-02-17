%----------------------------------------------------------------------------%
% FILENAME : calc.m
% AUTHOR   : Levy Teklemarim
% COPYRIGHT: Copyright © 2019 Levy Teklemarim
% LICENSE  : MIT
% CREATED  : Sun Feb 17, 2019  09:11AM
% MODIFIED : Sun Feb 17, 2019  09:45AM
%----------------------------------------------------------------------------%
% TBD: write documentation
%----------------------------------------------------------------------------%

:- module calc.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%----------------------------------------------------------------------------%

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module string.

:- pred compute(result(string), result(string), result(string), string).
:- mode compute(in, in, in, out) is semidet.
compute(ok(AS), ok(BS), ok(O), Out) :-
  to_int(AS, A),
  to_int(BS, B),
  (
    O = "+",
    R = A + B,
    Out = int_to_string(R)
  ;
    O = "*",
    R = A * B,
    Out = int_to_string(R)
  ;
    O = "/",
    R = A / B,
    Out = int_to_string(R)
  ;
    O = "-",
    R = A - B,
    Out = int_to_string(R)
  ).

main(!IO) :-
  print("Enter a number: ", !IO),
  read_line_as_string(S0, !IO),

  print("Enter another: ", !IO),
  read_line_as_string(S1, !IO),

  print("Enter the operation: ", !IO),
  read_line_as_string(OP, !IO),

  (if compute(S0, S1, OP, R)
      then print("Result: " ++ R, !IO)
      else print("nothing", !IO)).

%----------------------------------------------------------------------------%
:- end_module calc.
%----------------------------------------------------------------------------%
