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
:- module(treboada, []).

:- use_module(lexer).
:- use_module(parser).
:- use_module(sem).
:- use_module(gen).
:- use_module(asm).
:- use_module(util).


compile_source(Input, Output) :-
        lexer:scan(Input, Tokens),
        parser:parse(Tokens, AST),
        sem:analyze(AST, AAST),
        gen:generate(AAST, Asm),
        asm:assemble(Asm, Output).


compile_file(_Opts, Input_File, Output_File) :-
        read_file_codes(Input_File, Source),
        compile_source(Source, Output),
        write_file_bytes(Output_File, Output).


compile(Opts, PosArgs) :-
        PosArgs = [Input_File],
        compile_file(Opts, Input_File, 'a.out').


run :-
        current_prolog_flag(argv, Args),
        append(_SysArgs, ['--'|AppArgs], Args),
        !,
        parse_args(AppArgs, Opts, PosArgs),
        compile(Opts, PosArgs).


%% parse_args(+Args, -Options, -PositionalArgs)

parse_args([], [], []).
parse_args([Arg|Args], Opts, [Arg|PosArgs]) :-
        parse_args(Args, Opts, PosArgs).

        