%----------------------------------------------------------------------------%
% FILENAME : solve.m
% AUTHOR   : Simon Nielsen Knights
% COPYRIGHT: Copyright © 2019 Simon Nielsen Knights <tauoverpi@yandex.com>
% LICENSE  : MIT
% CREATED  : Mon Feb 18, 2019  01:39AM
% MODIFIED : Mon Feb 18, 2019  02:05AM
%----------------------------------------------------------------------------%
% TBD: write documentation
%----------------------------------------------------------------------------%

:- module solve.

:- interface.

% TBD: list modules

%----------------------------------------------------------------------------%

% TDB: declare predicates and functions

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- import_module list.
:- import_module int.
:- import_module set.

:- solver type po_vertex.

:- pred init(po_vertex::in) is semidet.
:- pred eq(po_vertex::in, po_vertex::in) is semidet.
:- pred '<'(po_vertex::in, po_vertex::in) is semidet.
:- pred '<='(po_vertex::in, po_vertex::in) is semidet.
:- impure pred order(list(po_vertex)::in, list(po_vertex)::out) is semidet.

:- type vertex == int.
:- type po_solver_state ---> pss(int, set(constraint)).
:- type constraint      ---> lt(vertex, vertex) ; le(vertex, vertex).

:- solver type po_vertex where
  representation is vertex,
  constraint_store is [mutable(a, po_solver_state, pss(0, init), ground)],
  equality is eq.

eq(A, B) :- A <= B, B <= A.

%----------------------------------------------------------------------------%
:- end_module solve.
%----------------------------------------------------------------------------%
