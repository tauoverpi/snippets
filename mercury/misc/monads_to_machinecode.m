%----------------------------------------------------------------------------%
% FILENAME : monads_to_machinecode.m
% AUTHOR   : Simon Nielsen Knights
% COPYRIGHT: Copyright © 2019 Simon Nielsen Knights <tauoverpi@yandex.com>
% LICENSE  : MIT
% CREATED  : Fri Feb 15, 2019  01:27PM
% MODIFIED : Fri Feb 15, 2019  09:48PM
%----------------------------------------------------------------------------%
% TBD: write documentation
%----------------------------------------------------------------------------%

:- module monads_to_machinecode.

:- interface.

% TBD: list modules
:- import_module uint32, uint8, list, maybe, int.

%----------------------------------------------------------------------------%

:- type jitmem ---> jitmem(
  instrs :: list(instr),
  mach   :: list(uint8),
  icount :: uint32,
  memptr :: uint32,
  memoff :: uint32
  ).

:- type val ---> int(int64); r_reg(r_reg); addr(uint32).
:- type r_reg --->
  r_rax; % accumulator
  r_rcx; % counter
  r_rdx; % data
  r_rbx; % general purpose
  r_rsp; % current stack pointer
  r_rbp; % previous stack frame link
  r_rsi; % source index pointer
  r_rdi. % destination index pointer

:- type x86(T) ---> x86(jitmem, maybe(string), T).

:- type instr --->
  ret;
  mov(val, val);
  add(val, val);
  sub(val, val);
  mul(val);
  imul(val, val);
  xor(val,val);
  inc(val);
  dec(val, val);
  push(val);
  pop(val);
  call(val);
  loop(val);
  nop;
  syscall.

:- pred index(r_reg, int).
:- mode index(in, out) is det.
:- mode index(out, in) is semidet.

:- pred emit_prim(list(uint8), jitmem, jitmem).
:- mode emit_prim(in, in, out) is semidet.
:- pred emit(list(int), jitmem, jitmem).
:- mode emit(in, in, out) is semidet.

:- pred ret(jitmem, jitmem).
:- mode ret(in, out) is semidet.

:- pred push(val, jitmem, jitmem).
:- mode push(in, in, out) is semidet.

:- func rax = val.
:- func rcx = val.
:- func rdx = val.
:- func rbx = val.
:- func rsp = val.
:- func rbp = val.
:- func rsi = val.
:- func rdi = val.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- func prot_none = uint32.
:- func prot_read = uint32.
:- func prot_write = uint32.
:- func prot_exec = uint32.
:- func map_file = uint32.
:- func map_anon = uint32.
:- func map_type = uint32.

index(r_rax, 0).
index(r_rcx, 1).
index(r_rdx, 2).
index(r_rbx, 3).
index(r_rsp, 4).
index(r_rbp, 5).
index(r_rsi, 6).
index(r_rdi, 7).

index(r_rax) = 0.
index(r_rcx) = 1.
index(r_rdx) = 2.
index(r_rbx) = 3.
index(r_rsp) = 4.
index(r_rbp) = 5.
index(r_rsi) = 6.
index(r_rdi) = 7.

rax = r_reg(r_rax).
rcx = r_reg(r_rcx).
rdx = r_reg(r_rdx).
rbx = r_reg(r_rbx).
rsp = r_reg(r_rsp).
rbp = r_reg(r_rbp).
rsi = r_reg(r_rsi).
rdi = r_reg(r_rdi).

prot_none  = cast_from_int(0x00).
prot_read  = cast_from_int(0x04).
prot_write = cast_from_int(0x02).
prot_exec  = cast_from_int(0x01).
map_file   = cast_from_int(0x0001).
map_anon   = cast_from_int(0x0002).
map_type   = cast_from_int(0x000f).

emit_prim(U8s, !Rec) :-
  R = !.Rec ^ memoff,
  length(U8s, L),
  from_int(L, O),
  !Rec ^ memoff := R + O,
  !Rec ^ mach   := !.Rec ^ mach ++ U8s.

emit(Is, !Rec) :- map(uint8.from_int, Is, Rs), emit_prim(Rs, !Rec).

label(L, !Rec) :- L = addr(!.Rec ^ memoff).

ret --> emit([0xc3]).

% add(r_reg(_), int(R)) --> emit([0x48]), emit([0x05]), imm(R).

push(r_reg(L)) --> emit([0x50 + index(L)]).

pop(r_reg(L)) --> emit([0x58 + index(L)]).

%call(addr(D), !R) :-
%  emit([0xe8], !R),
%  SRC = !.R ^ memoff,
%  imm(D - (SRC + cast_from_int(5))).

%----------------------------------------------------------------------------%
:- end_module monads_to_machinecode.
%----------------------------------------------------------------------------%
