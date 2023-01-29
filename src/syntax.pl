% The syntax checkers and term parsers for ProErl
%                                        – Simple Erlang Interpreter in Prolog

:- module( syntax, [parse_terms/2, is_expr/1, is_function/1] ).

:- use_module( utils, [raise/1, split_on/4, init/2] ).

:- use_module(library(lists), [last/2, exclude/3, reverse/2]).

:- set_prolog_flag(toplevel_print_options,
    [quoted(true),numbervars(true),portrayed(true),
                                   max_depth(1000)]).




% parsing terms: either ATTRIBUTE or FUNCTION
parse_terms([],[]).

% In case of attributes
parse_terms([Term|TL], [Node|NL]) :-
    to_attribute(Term, Node),
    parse_terms(TL, NL).

% In case of functions
parse_terms([Term|TL], [Node|NL]) :-
    function(Term, TL, Node, Rem),
    parse_terms(Rem, NL).

% In case of unrecognised terms
parse_terms([Term|_TL], ['<???>'(Term)|_NL]) :- % TODO: TL NL singleton volt
    write('Unknow term: '), write(Term),
    raise('ERROR Unknow term').
    %parse_terms(TL, NL).


% Turn raw termlist to Attribute NODE
to_attribute([-,module,'(',ModuleName,')','.'], '<MOD>'(ModuleName)).
to_attribute([-,export|Rest], '<EXPORT>'(ExportList)) :- % TODO: nem működik
    split_on(Rest, '[', ['('], ExpList0), % left side of split_on is to ensure syntax
    split_on(ExpList0, ']', ExpList1, [')','.']), % right side of split_on is to ensure syntax
    format_export_list(ExpList1, ExportList).


% Turn raw termlist to Function NODE
function(MaybeFun, TermList, '<FUN>'(FunName, Arglist, FunBody), Rem) :-
    split_on(MaybeFun, '->', FunHeader, FunBody0),
    split_on(FunHeader, '(', [FunName], Args),
    split_on(Args, ')', Arglist0, []),
    exclude(=(','), Arglist0, Arglist), % filters element where goal failed
    take_until_funbody([FunBody0|TermList], FunBody1, Rem),
    exclude(=([]), FunBody1, FunBody2),
    function_body(FunBody2, FunBody).


take_until_funbody(Terms, Funbody, Rem) :-
    take_until_funbody(Terms, [], Funbody, Rem).

take_until_funbody([], Funbody, Funbody, []).

take_until_funbody([H|T], Acc, Funbody, Rem) :-
    (   last(H, '.') ->  
            FunbodyRev = [H|Acc],
            reverse(Funbody, FunbodyRev),
            Rem = T
        ;   
            take_until_funbody(T, [H|Acc], Funbody, Rem)
    ).


format_export_list([], []).
format_export_list([Fun,/,Arity,','|Rest], ['<FUNREF>'(Fun, Arity)|FunRefRest]) :-
    format_export_list(Rest, FunRefRest).
format_export_list([Fun,/,Arity], ['<FUNREF>'(Fun, Arity)]).


% TODO: FUN with arity

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

function_body([],[]).
function_body([Term|FBody], [Node|Nodes]) :-
    fbody_item(Term, Node),
    function_body(FBody, Nodes).

% TODO: LEXICAL, SYNTACTICAL, SEMANTICAL LAYERS are not so well defined

% In Function body:
% TODO: Function call, 
% Artih Expression, 
% TODO: atoms, 
% TODO: variables
% TODO: variable wrapped funciton call
fbody_item(Term, Node) :-
    init(Term, SmallTerm), % TODO: emiatt fixen nem lehet soronként több statementet írni
    to_expression(SmallTerm, Node).

% function call
fbody_item(Term, Node) :-
    init(Term, SmallTerm), % TODO: emiatt fixen nem lehet soronként több statementet írni
    to_function_call(SmallTerm, Node).


fbody_item(Term, '<???>'(Term)). % TODO: should raise error

is_function('<FUN>'(_, _, _)).


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

% TEST erl:to_expression([683,*,2], Node).
to_expression(Chars, Result) :-
    to_expression(Chars, Result, []).
to_expression([H|Tail], Result, Collector) :-
    ( H = '(' ->
        to_expression(Tail, Result, Collector)
    ; H = ')' ->
        [A,B,C|Rest] = Collector,
        Term = '<EXPR>'(C, B, A),
        to_expression(Tail, Result, [Term|Rest])
    ; to_expression(Tail, Result, [H|Collector])
    ).

to_expression([], Result, Collector) :-
    [A,B,C] = Collector,
    Result = '<EXPR>'(C, B, A).


% [foo,'(',whatever,')']
% []
to_function_call([FunName|Term], '<FUNCALL>'(FunName, Arity, Args)) :-
    split_on(Term, '(', _, PartialArglist), % TODO: ensure syntax
    split_on(PartialArglist, ')', _, RawArglist), % TODO: ensure syntax
    Arity = -1,
    Args = RawArglist.


