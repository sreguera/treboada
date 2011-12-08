/*
  Copyright 2011 Jose Sebastian Reguera Candal

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/
:- module(comp, [compile/2]).

depth(constant(_), 1).
depth(variable(_), 1).
depth(funcall(_), 1000).
depth(op(E1, E2), D) :-
        depth(E1, D1),
        depth(E2, D2),
        (  D1 > D2
        -> D1 = D
        ;  D1 < D2
        -> D2 = D
        ;  D is D1 + 1
        ).

compile(AST, Asm) :-
        phrase(compile(AST), Asm).

compile(constant(C)) -->
        [ldc(C)].
compile(variable(V)) -->
        [ldl(V)].
compile(op(E1, E2)) -->
        { depth(E1, D1) },
        { depth(E2, D2) },
        (  { D2 > D1 }
        -> (  { D1 > 2 }
           -> compile(E2),
              [stl(temp)],
              compile(E1),
              [ldl(temp)]
           ;  { conmutes('+') }
           -> compile(E2),
              compile(E1)
           ;  compile(E2),
              compile(E1),
              [rev]
           )
        ;  { D2 < 3 }
        -> compile(E1),
           compile(E2)
        ;  compile(E2),
           [stl(temp)],
           compile(E1),
           [ldl(temp)]
        ),
        [sum].

conmutes('+').


:- begin_tests(comp).

test(depth) :-
        depth(op(constant(a),
                 op(op(constant(b), constant(c)),
                    constant(d))),
              2).

:- end_tests(comp).