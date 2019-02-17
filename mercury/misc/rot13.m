:- module rot13.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module char, string.

main(!IO) :-
  io.read_char(Result, !IO),
  (
    Result = ok(Char),
    io.write_char(rot13(Char), !IO),
    main(!IO)
  ;
    Result = eof
  ;
    Result = error(ErrorCode),
    io.format("%s\n", [s(io.error_message(ErrorCode))], !IO)
  ).

:- func rot13(char) = char is det.
rot13(X) = (if rot13(X,R) then R else X).

:- pred rot13(char::in, char::out) is semidet.
rot13('a', 'n').
rot13('b', 'o').
rot13('c', 'p').
rot13('d', 'q').
rot13('e', 'r').
rot13('f', 's').
rot13('g', 't').
rot13('h', 'u').
rot13('i', 'v').
rot13('j', 'w').
rot13('k', 'x').
rot13('l', 'y').
rot13('m', 'z').
rot13('n', 'a').
rot13('o', 'b').
rot13('p', 'c').
rot13('q', 'd').
rot13('r', 'e').
rot13('s', 'f').
rot13('t', 'g').
rot13('u', 'h').
rot13('v', 'i').
rot13('w', 'j').
rot13('x', 'k').
rot13('y', 'l').
rot13('z', 'm').
