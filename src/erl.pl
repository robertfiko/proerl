% Program to scan a file and assemble identifiers,
% sequences of special characters, numbers and delimiters.
 :- module( erl, [run/1, run/2] ).

:- use_module( scan, [scan/2, reap_tokens/2] ).
%:- use_module(library(lists), ).
:- set_prolog_flag(toplevel_print_options,
    [quoted(true),numbervars(true),portrayed(true),
                                   max_depth(1000)]).

%%%%% ?? ONLY FOR TESTING PURPOSES ?? %%%%%
run(Result) :- run('examples/arithmetics.erl', Result).


%%%% MAIN IO %%%%
run(Path, Final) :- 
    scan_whole_file(Path, Result),
    init(Result, TermList),
    parse_terms(TermList, NodeList),

    Final = NodeList.


%%%%% INTERNAL %%%%%%



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
% parse_tl_term([-,module,'(',simple,')','.'], A).
% attribute(['-',module,'(',simple,')','.'], A).
% attribute([-,alma,'(',simple,')','.'], A).
% is_attribute([alma,module,'(',simple,')','.']).


parse_terms([],[]).

% In case of attributes
parse_terms([Term|TL], [Node|NL]) :-
    attribute(Term, Node),
    parse_terms(TL, NL).

% In case of functions
parse_terms([Term|TL], [Node|NL]) :-
    function(Term, TL, Node, Rem)
    parse_terms(Rem, NL).




find_end_function([H|TermList], FunTermlist, Remaining, Collector) :-
    %last(H, '.'),
    Remaining = TermList,
    FunTermlist = [H|Collector].

find_end_function([H|TermList], FunTermlist, Remaining, Collector) :-
    find_end_function(TermList, FunTermlist, Remaining, [H|Collector]).



parse_term(['-'|Rest], Node) :- attribute(['-'|Rest], Node).
parse_term(Term, Node) :- function(Term, Node). % TODO: NAGYON NAGYON DE NAGYON
parse_term([], '<E>').

parse_term(Node, '<???>'(Node)). % TODO: this shold raise an error


%TODO: to_ prefix
attribute([-,module,'(',ModuleName,')','.'], '<MOD>'(ModuleName)).
attribute([-,export|Rest], '<EXPORT>'(ExportList)) :- % TODO: nem működik
    split_on(Rest, '[', ['('], ExpList0), % left side of split_on is to ensure syntax
    split_on(ExpList0, ']', ExportList, [')','.']). % right side of split_on is to ensure syntax
    %exclude(=(','), Arglist0, Arglist). % TODO: explain this

% [[-,module,'(',simple,')','.'],[-,export,'(','[',foo,/,1,']',')','.'],[],[foo,'(','_',')',->,ok,'.']]
% [foo,'(','_',')',->,ok,'.']
% function([foo,'(','_',')',->,ok,'.'], Term).

% function([bar,'(','Apple',',','Pear',')',->,london,',',foo,'(','Pear',')','.'], Term).

% split_on([foo,'(','_',')',->,ok,'.'], '->', FunHeader, FunBody).

%split_on(MaybeFun, '->', FunHeader, FunBody),
function(MaybeFun, TermList, '<FUN>'(FunName, Arglist, FunBody), Rem) :-
    split_on(MaybeFun, '->', FunHeader, FunBody0),
    split_on(FunHeader, '(', [FunName], Args),
    split_on(Args, ')', Arglist0, []),
    exclude(=(','), Arglist0, Arglist), %TODO: explain this
    take_until_funbody([FunBody0|TermList], fun_construct(FunBody1, Rem)),

    FunBody = FunBody1.


% take_until_funbody([[korte, ','], [5, '+', 6, '.'], [szilva]], [[korte,','],[5,+,6,'.']], [[szilva]]).
% take_until_funbody([[korte, ','], [5, '+', 6, '.'], [szilva]], A).

%take_until_funbody([], [], []).

take_until_funbody([H|T], fun_construct([H|Result], T)) :-
    last('.', H), !.
    %take_until_funbody(T, Result).

take_until_funbody([H|T], [H|Result]) :-
    take_until_funbody(T, Result).

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
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    UTILS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


split_on(List, Element, Left, Right) :-
    append(Left, [Element|Right], List).

init([_], []).
init([X|Xs], [X|WithoutLast]) :- 
    init(Xs, WithoutLast).

last(X,[X]).
last(X,[_|Z]) :- last(X,Z).