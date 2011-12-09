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
:- module(gen, [generate/2]).

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

generate(AST, Asm) :-
        phrase(generate(AST), Asm).

generate(constant(C)) -->
        [ldc(C)].
generate(variable(V)) -->
        [ldl(V)].
generate(op(E1, E2)) -->
        { depth(E1, D1) },
        { depth(E2, D2) },
        (  { D2 > D1 }
        -> (  { D1 > 2 }
           -> generate(E2),
              [stl(temp)],
              generate(E1),
              [ldl(temp)]
           ;  { conmutes('+') }
           -> generate(E2),
              generate(E1)
           ;  generate(E2),
              generate(E1),
              [rev]
           )
        ;  { D2 < 3 }
        -> generate(E1),
           generate(E2)
        ;  generate(E2),
           [stl(temp)],
           generate(E1),
           [ldl(temp)]
        ),
        [sum].

conmutes('+').


:- begin_tests(gen).

test(depth) :-
        depth(op(constant(a),
                 op(op(constant(b), constant(c)),
                    constant(d))),
              2).

:- end_tests(gen).