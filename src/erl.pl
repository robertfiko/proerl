% Program to scan a file and assemble identifiers,
% sequences of special characters, numbers and delimiters.
%:- module( erl, [run/1, run/2, p] ).

:- use_module( scan, [scan/2, reap_tokens/2] ).
:- use_module(library(lists)).
:- set_prolog_flag(toplevel_print_options,
    [quoted(true),numbervars(true),portrayed(true),
                                   max_depth(1000)]).

run(Result) :- run('examples/arithmetics.erl', Result).

run(Path, TermList) :- 
    scan_whole_file(Path, Result),
    init(Result, TermList).

init([_], []).
init([X|Xs], [X|WithoutLast]) :- 
    init(Xs, WithoutLast).

scan_whole_file(Path, Result) :-
    open(Path, read, Stream), 
    set_input(Stream),
    start_scan(Result),
    close(Stream).
    %init(RawResult, Result).


start_scan([List|OtherResults]) :- 
    catch(
        (   get_code(C1), 
            reap_tokens(C1,List),
            start_scan(OtherResults)
        ),
        Error,
        (print_message(error, Error), OtherResults = [])
    ).
    
    
% parsing terms: either ATTRIBUTE or FUNCTION
% parse_term([-,module,'(',simple,')','.'], A).
% attribute(['-',module,'(',simple,')','.'], A).
% attribute([-,alma,'(',simple,')','.'], A).
% is_attribute([alma,module,'(',simple,')','.']).
parse_term(['-'|Rest], Node) :- attribute(['-'|Rest], Node).

split_on(List, Element, Left, Right) :-
    append(Left, [Element|Right], List).


attribute([-,module,'(',ModuleName,')','.'], '<MOD>'(ModuleName)).
attribute([-,export, '(', ExportList, ')', '.'], '<EXP>'(ExportList)).

% [[-,module,'(',simple,')','.'],[-,export,'(','[',foo,/,1,']',')','.'],[],[foo,'(','_',')',->,ok,'.']]
% [foo,'(','_',')',->,ok,'.']
% function([foo,'(','_',')',->,ok,'.'], Term).

% function([bar,'(','Apple',',','Pear',')',->,london,',',foo,'(','Pear',')','.'], Term).

% split_on([foo,'(','_',')',->,ok,'.'], '->', FunHeader, FunBody).

%split_on(MaybeFun, '->', FunHeader, FunBody),
function(MaybeFun, '<FUN>'(FunName, Arglist, FunBody)) :-
    split_on(MaybeFun, '->', FunHeader, FunBody),
    split_on(FunHeader, '(', [FunName], Args),
    split_on(Args, ')', Arglist0, []),
    exclude(=(','), Arglist0, Arglist). % TODO: explain this

% Function Body Item
% PASS will contain the leftover stuff 
%fbi([Id,'('|Rest], '<FUNCALL>'(Id, Params), PASS) :-
%    split_on(Rest, '(', Params0, Rem),
%    exclude(=(','), Params0, Params1),
%    split_on(Rem, ')', [], [','| PASS]).
% TODO: need to check for following comma and whether we can move on or not
% TODO: type of params
% TODO: type of fun Id
% TODO: finish function call
% TODO: cross-module function call

%fbi(Expr, '<EXPR>'(Left, Operator, Right)) :-


is_atom(Term) :- 
    atom(Term), 
    atom_chars(Term, [A|_]),
    char_type(A, lower).

is_var_name(['_',A|_]) :- char_type(A, upper).
is_var_name([A|_]) :- char_type(A, upper).
is_variable(Term) :-
    atom(Term),
    atom_chars(Term, Chars),
    is_var_name(Chars).


is_op('+'). is_op('-'). is_op('*'). is_op('/').
is_operand(O) :- number(O).
is_operand(O) :- is_expr(O).
is_expr('<EXPR>'(Left, Op, Right)) :- 
    is_operand(Left),
    is_op(Op),
    is_operand(Right).

% to_expr([5,'+',6], '<EXPR>'(5, '+', 6)).
%to_expr([Left, Op, Right], Expr) :- 
%    Expr = '<EXPR>'(Left, Op, Right),
%    is_expr(Expr).



% group_expression([5, '+', '(', 4, '-', 3, ')'], R).
% group_expression([5, '+', '(', '(', 4, '-', 1, ')', '/', 2, ')'], R).
% group_expression(['(', 5, '+', '(', 4, '-', 1, ')', ')', '/', 2], R).
group_expression(Chars, Result) :-
    group_expression(Chars, Result, []).
group_expression([H|Tail], Result, Collector) :-
    ( H = '(' ->
        group_expression(Tail, Result, Collector)
    ; H = ')' ->
        [A,B,C|Rest] = Collector,
        Term = '<EXPR>'(C, B, A),
        group_expression(Tail, Result, [Term|Rest])
    ; group_expression(Tail, Result, [H|Collector])
    ).

group_expression([], Result, Collector) :-
    [A,B,C] = Collector,
    Result = '<EXPR>'(C, B, A).






%%%%%%% EVAL %%%%%%


do_math(L, '+', R, Result) :- Result is L+R.
do_math(L, '-', R, Result) :- Result is L-R.
do_math(L, '/', R, Result) :- Result is L/R.
do_math(L, '*', R, Result) :- Result is L*R.

% eval_arith('<EXPR>'(5,+,'<EXPR>'('<EXPR>'(4,-,1),/,2)), R).
% eval_arith('<EXPR>'('<EXPR>'(5,+,'<EXPR>'(4,-,1)),/,2), R).
eval_arith(N, N) :- number(N).
eval_arith('<EXPR>'(L, O, R), Result) :-
    eval_arith(L, LRes),
    eval_arith(R, RRes),
    do_math(LRes, O, RRes, Result).
    
eval_expression('<EXPR>'(L, O, R), Result) :-
    