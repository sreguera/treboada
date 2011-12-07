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
:- module(parser, []).

parse(AST, Tokens) :-
        parse(AST, Tokens, []).

parse(AST) -->
        expression(AST).

expression(E) -->
        relation(E).

relation(E) -->
        simple_expression(E).

simple_expression(E) -->
        term(T),
        simple_expression_aux(T, E).

simple_expression_aux(E1, E) -->
        binary_adding_operator(_),
        !,
        term(E2),
        simple_expression_aux(op(E1, E2), E).
simple_expression_aux(E, E) -->
        [].
        
binary_adding_operator('+') -->
        ['+'].

term(E) -->
        factor(E).

factor(E) -->
        primary(E).

primary(E) -->
        numeric_literal(E).

numeric_literal(constant(N)) -->
        [num(N)].


:- begin_tests(parser).

test(add) :-
        parse(op(op(constant(1), constant(2)), constant(3)),
              [num(1), '+', num(2), '+', num(3)]).

:- end_tests(parser).
        