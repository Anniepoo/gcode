:- module(gcode_parse, [gcode_parse/2,
                       gcode//1]).
/** <module> Parser for G-code
 *
 */

:- use_module(library(chr)).

%!  gcode_parse(+Text:text, -GCode:list) is det
%
gcode_parse(Text, GCode) :-
    text_to_string(Text, String),
    string_codes(String, Codes),
    once(gcode_tokenize(Codes, Tokens)),
    phrase(gcode_blocks(GCode), Tokens),
    !.

test_gcode_parse(L) :-
    gcode_parse(" (hello, 3 world) \nN001 G00 X4.5Y7.0Z1.0 (first line)\nN002 G01Z0.1\nG00 Z-0.5\n", L). % TODO


gcode_blocks([]) --> [].
gcode_blocks([Block|Blocks]) -->
    gcode_block(Block),
    gcode_blocks(Blocks).
gcode_blocks(Blocks) -->
    non_generating_block,
    gcode_blocks(Blocks).

gcode_block(Block) -->
    block_body(Block),
    eob.

eob -->
    empty.
eob -->
    [end_of_line].

empty([], []).

non_generating_block -->
    any_but_block_end,
    [tape_end],
    any_but_block_end,
    eob.

any_but_block_end -->
    [X],
    {\+ memberchk(X,  [end_of_line, tape_end])},
    any_but_block_end.
any_but_block_end --> [].

block_body(Block) -->
    block_skip(BS),
    label(L),
    block_stuff(B),
    { append([BS, L, B], BList),
      % TODO deal with multiple occurances of the same word
      dict_create(Block, block, BList)
    }.

block_skip([block_skip-[N|Rest]]) -->
    [block_skip, num(N)],
    { integer(N) },
    block_skip_num(Rest),!.
block_skip(block_skip-[]) -->
    [block_skip],
    !.
block_skip([]) --> [].

block_skip_num([]) --> [].
block_skip_num([N|Rest]) -->
    [block_skip, num(N)],
    {integer(N) },
    block_skip_num(Rest).

label([label-N]) -->
    [word(0'N), num(N)].
label([]) --> [].

block_stuff([]) --> [].
block_stuff([comment-C| Rest]) -->
    [comment(C)],
    {gtrace},
    block_stuff(Rest).
block_stuff([WordAtom-Addr|Rest]) -->
    [word(Word), num(Addr)],
    { code_type(Word, to_upper(LwrCode)),
      atom_codes(WordAtom, [LwrCode]) },
    block_stuff(Rest).


%!  gcode_tokenize(+Codes:list, -Tokens:list) is nondet
%
%  @arg Codes is a codes string to parst
%  @arg Tokens is a list of tokens
%
%  Tokens are one of
%
%  * =block_skip=
%    The block skip operator (forward slash / )
%  * =end_of_line=
%    sequence of one or two chars from code_type/2 =end_of_line=
%  * =|word(W)|=
%    upper cased ascii code for a word (an alphabetic symbol in G-Code)
%  * =|num(N)|=
%    the number is either an int or a float depending on if the input
%  had a decimal point
%  * =|comment(S)|=
%    a program comment, S is the contents of the comment
%  * =tape_end=
%    the tape end mark %
%  * =|error(E)|=
%    An unexpected token - we trap everything to EOL as a codes string
%
%  [this page is useful for basic
%  format](https://www.cnccookbook.com/g-code-basics-program-format-structure-blocks/)
%
gcode_tokenize([], []).
gcode_tokenize([0'/ |T], [block_skip|Tokens]) :-  % block skip operator
    gcode_tokenize(T, Tokens).
gcode_tokenize([0'% |T], [tape_end|Tokens]) :-  % tape end operator
    gcode_tokenize(T, Tokens).
gcode_tokenize([H|T], [word(HU)|Tokens]) :-  % word
    code_type(H, alpha),
    code_type(HU, to_upper(H)),
    gcode_tokenize(T, Tokens).
% TODO negativ numbers!
gcode_tokenize([0'-|T], Tokens) :-
    more_num(0, -1, T, Tokens).
gcode_tokenize([H|T], Tokens) :-   % num
    code_type(H, digit(W)),
    more_num(W, 1, T, Tokens).
gcode_tokenize([H|T], Tokens) :-  % whitespace
    code_type(H, white),
    gcode_tokenize(T, Tokens).
gcode_tokenize([H,H2|T], [end_of_line|Tokens]) :-   % newline
    code_type(H,end_of_line),
    code_type(H2,end_of_line),
    gcode_tokenize(T, Tokens).
gcode_tokenize([H|T], [end_of_line|Tokens]) :-
    code_type(H,end_of_line),
    gcode_tokenize(T, Tokens).
gcode_tokenize([0'( | T], [comment(S) | Tokens]) :-  % comment
    body_of_comment(T, Body, Tokens),
    string_codes(S, Body).
gcode_tokenize([H|T], [error([H|Rem])|Tokens]) :- % error
    rest_of_block(T, Rem, Tokens).

rest_of_block([], [], []).
rest_of_block([H|T], [], Tokens) :-
    code_type(H, end_of_line),
    !,
    gcode_tokenize(T, Tokens).
rest_of_block([H|T], [H|Rem], Tokens) :-
    rest_of_block(T, Rem, Tokens).

body_of_comment([], [], []).
body_of_comment([0') | T], [], Tokens) :-
    gcode_tokenize(T, Tokens).
body_of_comment([H | T], [H|Rem], Tokens) :-
    body_of_comment(T, Rem, Tokens).

more_num(N, Sgn, [], [num(SN)]) :-
    SN is Sgn * N.
more_num(N, Sgn, [H|T],Tokens) :-
    code_type(H, digit(W)),
    !,
    K is N * 10 + W,
    more_num(K, Sgn, T, Tokens).
more_num(N, Sgn, [0'. | T], Tokens) :-
    K is float(N),
    frac(K, 10, Sgn, T, Tokens).
more_num(N, Sgn, [H|T],[num(SN) |Tokens]) :-
    SN is N * Sgn,
    gcode_tokenize([H|T], Tokens).

frac(K, _, Sgn, [], [num(N)]) :-
    N is K * Sgn.
frac(N, Pwr, Sgn, [H|T], Tokens) :-
    code_type(H, digit(W)),
    !,
    K is N  + W / float(Pwr),
    NPwr is 10 * Pwr,
    frac(K, NPwr, Sgn, T, Tokens).
frac(K, _, Sgn, [H|T], [num(N) | Tokens]) :-
    N is K * Sgn,
    gcode_tokenize([H|T], Tokens).

test_gcode_tokenize :-
    once(gcode_tokenize(` N1 G -5.1 G\n00001 X-3Y7.5Z1\nF5`,
                   [word(78),num(1),word(71),num(-5.1),word(71),end_of_line,
                    num(1),word(88),num(-3),word(89),num(7.5),word(90),num(1),end_of_line,
                    word(70),num(5)] )).



/*
gcode([]) --> [].
gcode([label(N)|Rest]) -->
    [word(0'
*/



















