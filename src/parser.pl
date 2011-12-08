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


%% expression

expression(E) -->
        relation(E).


%% relation

relation(E) -->
        simple_expression(E).


%% simple_expression ::= [unary_adding_operator] term {binary_adding_operator term}

simple_expression(E) -->
        unary_adding_operator(_),
        !,
        term(T),
        simple_expression_aux(T, E0),
        { E = op(E0) }.
simple_expression(E) -->
        term(T),
        simple_expression_aux(T, E).

simple_expression_aux(E0, E) -->
        binary_adding_operator(_),
        !,
        term(T),
        simple_expression_aux(op(E0, T), E).
simple_expression_aux(E, E) -->
        [].


%% term ::= factor {multiplying_operator factor}

term(T) -->
        factor(F),
        term_aux(F, T).

term_aux(T0, T) -->
        multiplying_operator(_),
        !,
        factor(F),
        term_aux(op(T0, F), T).
term_aux(T, T) -->
        [].


%% factor

factor(E) -->
        primary(E).


%% primary

primary(E) -->
        numeric_literal(E).


%% numeric_literal

numeric_literal(constant(N)) -->
        [num(N)].


binary_adding_operator('+') --> ['+'].
binary_adding_operator('-') --> ['-'].

unary_adding_operator('+') --> ['+'].
unary_adding_operator('-') --> ['-'].

multiplying_operator('*') --> ['*'].
multiplying_operator('/') --> ['/'].


:- begin_tests(parser).

test(add) :-
        parse(op(op(constant(1), constant(2)), constant(3)),
              [num(1), '+', num(2), '+', num(3)]).

test(add_mul) :-
        parse(op(constant(1), op(constant(2), constant(3))),
              [num(1), '+', num(2), '*', num(3)]).

:- end_tests(parser).
        