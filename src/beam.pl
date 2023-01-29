% This module contains evaluation predicates for
% ProErl – Simple Erlang Interpreter in Prolog
%
% Named after Erlang's Virtual Machine: the BEAM :-)

:- module( beam, [eval/2] ).
:- use_module( syntax, [is_expr/1, is_function/1, is_atom/1] ).

:- set_prolog_flag(toplevel_print_options,
    [quoted(true),numbervars(true),portrayed(true),
                                   max_depth(1000)]).



%%% IO
eval_arith(N, N) :- number(N).
eval_arith('<EXPR>'(L, O, R), Result) :-
    eval_arith(L, LRes),
    eval_arith(R, RRes),
    do_math(LRes, O, RRes, Result).

%%% INTERNAL
do_math(L, '+', R, Result) :- Result is L+R.
do_math(L, '-', R, Result) :- Result is L-R.
do_math(L, '/', R, Result) :- Result is L/R.
do_math(L, '*', R, Result) :- Result is L*R.



eval(Node, Result) :- is_expr(Node), eval_arith(Node, Result).
eval(Node, Result) :- is_function(Node), eval_function(Node, Result).
eval('<ATOM>'(Atom), Atom) :- is_atom(Atom).
eval(Node, _) :- write('Unable to evaluate: '), write(Node), fail.


% TODO: differenciálni kellene a funciton és a function call között
eval_function('<FUN>'(_, _, FunBody), Result) :- % TODO: az argumentumok nem igazán vannak így bindolva
    eval_funbody(FunBody, Result).

eval_funbody([H], Result) :-
    eval(H, Result).
eval_funbody([H|T], Result) :-
    eval(H, _), % TODO: really? do not drop result :D
    eval_funbody(T, Result).
