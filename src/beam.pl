% This module contains evaluation predicates for
% ProErl – Simple Erlang Interpreter in Prolog
%
% Named after Erlang's Virtual Machine: the BEAM :-)

:- module( beam, [eval/2, set_mod/1, reset_beam/1] ).
:- use_module( syntax, [is_expr/1, is_function_node/1, is_atom_name/1] ).

:- set_prolog_flag(toplevel_print_options,
    [quoted(true),numbervars(true),portrayed(true),
                                   max_depth(1000)]).


:- dynamic dict/2. % like process dictionary
:- dynamic module/1.

%%% IO 


set_mod(Module) :- 
    retractall(module(_)),
    asserta(module(Module)).

reset_beam(_) :-
    retractall(dict(_,_)).

%%% INTERNAL

eval_arith(N, N) :- number(N).
eval_arith('<EXPR>'(L, O, R), Result) :-
    eval_arith(L, LRes),
    eval_arith(R, RRes),
    do_math(LRes, O, RRes, Result).


do_math(L, '+', R, Result) :- Result is L+R.
do_math(L, '-', R, Result) :- Result is L-R.
do_math(L, '/', R, Result) :- Result is L/R.
do_math(L, '*', R, Result) :- Result is L*R.



eval(Node, Result) :- is_expr(Node), eval_arith(Node, Result).
eval(Node, Result) :- is_function_node(Node), eval_function(Node, Result).
eval('<ATOM>'(Atom), Atom) :- is_atom_name(Atom).
eval('<BINDING>'(L, R), Result) :- eval_binding(L,R, Result).
eval('<VAR>'(V), Result) :- eval_var(V, Result).
eval('<FUNCALL>'(Name, Arity, Args), Result) :- eval_funcall(Name, Arity, Args, Result).
eval(Node, _) :- write('Unable to evaluate: '), write(Node), fail.

eval_function('<FUN>'(Name, _, FunBody), Result) :- 
    eval_funbody(FunBody, Result).

eval_funbody([H], Result) :-
    eval(H, Result).
eval_funbody([H|T], Result) :-
    eval(H, _), % the result of the function is the result of the last statement
    eval_funbody(T, Result).

eval_var(Var, Res) :- get_value(Var, ok(Res)).
eval_var(Var, _) :-
    get_value(Var, error(not_bound)), 
    write('Variable is not bound: '), write(Var), write('\n'), fail.

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
            Success = already_bound; % var is bound, but with the same value
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
        write(' = '), write(Value), write('\n'),
        Success = failed
    ).

accpet_bind(already_bound). accpet_bind(ok).
eval_binding(L, R, Result) :-
    eval(R, EvalR),
    bind_value(L, EvalR, Success),
    accpet_bind(Success),
    Result = EvalR. 


find_fun([H|_], Name, Arity, H) :-
    '<FUN>'(Name, Args, _) = H,
    length(Args, Arity).

find_fun([], N, Arity, error) :- 
    write('No function definition found for '), write(N), write('/'), write(Arity),
    write('\n'), fail. % TODO: check for this error
find_fun([_|T], N, Arity, Result) :- find_fun(T, N, Arity, Result).

bind_funheader([], []).
bind_funheader([FunArg|FA], [FunCallArg|FCA]) :-
    eval_binding(FunArg, FunCallArg, _),
    bind_funheader(FA, FCA).

gc_funheader([]).
gc_funheader([FunArg|FA]) :-
    retractall(dict(FunArg, _)),
    gc_funheader(FA).

eval_funcall(Name, Arity, Args, Result) :-
    module('$MODULE'(_, _, Funs)),
    find_fun(Funs, Name, Arity, FunNode),

    % the Args should contain either values or variables, anyhow the evaulation
    % of them should provide the binding results
    '<FUN>'(_, Arglist, _) = FunNode,
    bind_funheader(Arglist, Args),

    % bind its variable
    eval(FunNode, Result),
    gc_funheader(Arglist).
    


% TODO: double check for function eval