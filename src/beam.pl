% This module contains evaluation predicates for
% ProErl – Simple Erlang Interpreter in Prolog
%
% Named after Erlang's Virtual Machine: the BEAM :-)

:- module( beam, [eval/2] ).
:- use_module( syntax, [is_expr/1, is_function/1, is_atom/1] ).

:- set_prolog_flag(toplevel_print_options,
    [quoted(true),numbervars(true),portrayed(true),
                                   max_depth(1000)]).


:- dynamic dict/2. % like process dictionary TODO: docs

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
eval('<BINDING>'(L, R), Result) :- eval_binding(L,R, Result).
eval('<VAR>'(V), Result) :- eval_var(V, Result).
eval(Node, _) :- write('Unable to evaluate: '), write(Node), fail.


% TODO: differenciálni kellene a funciton és a function call között
eval_function('<FUN>'(_, _, FunBody), Result) :- % TODO: az argumentumok nem igazán vannak így bindolva
    eval_funbody(FunBody, Result).

eval_funbody([H], Result) :-
    eval(H, Result).
eval_funbody([H|T], Result) :-
    eval(H, _), % TODO: really? do not drop result :D
    eval_funbody(T, Result).

eval_var(Var, Res) :- get_value(Var, ok(Res)).
eval_var(Var, Res) :-
    get_value(Var, error(not_bound)), 
    write('Variable is not bound: '), write(Ver), write('\n'), fail.

get_value(Variable, Result) :-
    (
        dict(Variable, Value) ->
            Result = ok(Value);
        Result = error(not_bound)
    ).

bind_value(Variable, Value, Success) :-
    get_value(Variable, Result),
    (
        ok(V) = Result, V = Value -> 
            Success = already_bound, % var is bound, but with the same value
        ok(V) = Result -> 
            Success = cannot_redifine,
            write('Cannot redifine variable.\n'),
            fail;
        error(not_bound) = Result ->
            asserta(dict(Variable,Value)),
            Success = ok;

        % default
        write('Unknown error happened while binding: '), 
        write(Variable), 
        write(' = '), write(Value), write(Result), write('\n'),
        Success = failed
    ).

accpet_bind(already_bound). accpet_bind(ok).
eval_binding(L, R, Result) :-
    write('Trying to bind'), write(R),
    eval(R, EvalR), % TODO: UNABLE TO EVALUATE BECAUSE RIGHT HAND SIDE IS NOT A SYNTACTIC NODE
    write(EvalR),
    bind_value(L, EvalR, Success),
    accpet_bind(Success),
    Result = R.  %TODO: eval right hand side  


