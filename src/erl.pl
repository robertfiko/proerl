% Program to scan a file and assemble identifiers,
% sequences of special characters, numbers and delimiters.
 :- module( erl, [run/1, run/2] ).

:- use_module( scan, [scan/2, reap_tokens/2] ).
:- use_module(library(lists), [exclude/3, reverse/2]).
:- set_prolog_flag(toplevel_print_options,
    [quoted(true),numbervars(true),portrayed(true),
                                   max_depth(1000)]).


% TODO: module doc
% TODO: attribute doc


%%%%%%%%%%%%%%% ?? ONLY FOR TESTING PURPOSES ?? %%%%%%%%%%%%%%%
run(Result) :- run('examples/arithmetics.erl', Result).
%%%%%%%%%%%%%%% ?? ONLY FOR TESTING PURPOSES ?? %%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   MAIN IO 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run(Path, Final) :- 
    scan_whole_file(Path, Result),
    init(Result, TermList0),
    exclude(=([]), TermList0, TermList),
    parse_terms(TermList, NodeList),
    

    Final = NodeList.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   INTERNAL Functions 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Scan the whole file until EOF
scan_whole_file(Path, Result) :-
    open(Path, read, Stream), 
    set_input(Stream),
    start_scan(Result),
    close(Stream).
    %init(RawResult, Result). %TODO: probably to get rid of last []

% Initation scanning with error handling
start_scan([List|OtherResults]) :- 
    catch(
        (   get_code(C1), 
            reap_tokens(C1,List),
            start_scan(OtherResults)
        ),
        _Error, % error is not raised to the user, as it is handled
        (OtherResults = [])
    ).
    
    
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
parse_terms([Term|TL], ['<???>'(Term)|NL]) :- % TODO: this shold raise an error
    parse_terms(TL, NL).


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
    (   last('.', H) ->  
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



%%%%%%% EVAL %%%%%%
do_math(L, '+', R, Result) :- Result is L+R.
do_math(L, '-', R, Result) :- Result is L-R.
do_math(L, '/', R, Result) :- Result is L/R.
do_math(L, '*', R, Result) :- Result is L*R.

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



% TODO: after module construction and simple evaluation is done split to modules
% TODO: scan.pl-t visszaállítani kié??
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tcm(A) :-
    Nodes = ['<MOD>'(arithmetics_onemain),'<EXPORT>'(['<FUNREF>'(main,0)]),'<FUN>'(main,[],['<EXPR>'(16,+,43)])],
    construct_module(Nodes, A).

%'$MODULE'(ModuleDefinition, Exportlist, Funs)
construct_module(Nodes, Module) :-
    construct_module(Nodes, '!none', '!none', [], '!MOD'(Mod, Exp, Funs)),

    % checks
    Mod = '$MOD'(ModDef), atom(ModDef),

    '$EXPORT'(ExportList) = Exp, 
    ['<FUNREF>'(_,_)|_] = ExportList,

    ['<FUN>'(_, _, _)|_] = Funs,

    Module = '$MODULE'(Mod, Exp, Funs).

construct_module([], Mod, Exp, Funs, '!MOD'(Mod, Exp, Funs)).  % TODO: doc: ! needs attention <> lexical/syn node $ smenatic another node
construct_module([Node|Nodes], ModDef, Exportlist, Funs, MOD) :-
    write('\n---\n'), write([Node|Nodes]), %write('\n'), write(ModDef), write('\n'), write(ExportList), write('\n'), write(Funs), write('\n'),
    (
        '<MOD>'(_) = Node -> 
            write('<<<M>>>'),
            declare_module(ModDef, Node, NewModDef),
            construct_module(Nodes, NewModDef, Exportlist, Funs, MOD);
        '<EXPORT>'(ExpList) = Node ->
            write('<<<E>>>'),
            declare_explist(ExpList, Node, NewExportList),
            construct_module(Nodes, ModDef, NewExportList, Funs, MOD);
        '<FUN>'(_, _, _) = Node ->
            write('<<<F>>>'),
            construct_module(Nodes, ModDef, Exportlist, [Node|Funs], MOD);
        write('\nERROR: Unrecognised node type'), write(Node), write('\n') % todo: universal halting
        %construct_module(Nodes, NewModDef, Exportlist, Funs, MOD);
    ).

declare_module(OldDef, Def, NewDef) :-
    (   
        % module is not defined already
        '!none' = OldDef,
        '<MOD>'(ModName) = Def ->
            NewDef = '$MOD'(ModName);

        % module is already defined
        '<$MOD>'(ModName) = OldDef ->
            NewDef = OldDef,
            write('\nERROR: Module already defined'); % todo: universal halting
        
        % not a module definition
        '!none' = OldDef ->
        % if 'Def' would have a correct syntax, an earlier clause would have run
            NewDef = OldDef,
            write('\nERROR: Not a module definition'); % todo: universal halting
        
        % default
        write('ERROR: while module def, unknow.') % todo: universal halting
    
    ).

%declare_module('!none', '<MOD>'(ModName), '<MOD>'(ModDef)) :- ModDef = ModName, write('\nMODDEF.').
%declare_module(M, '<MOD>'(ModName), M) :- write('\nERROR: Module already defined'), write(M), write(ModName). % todo: universal halting
%declare_module(M, _, M) :- write('\nERROR: Not a module definition'). % todo: universal halting


declare_explist(OldDef, Def, NewDef) :-
    (   
        % module is not defined already
        '!none' = OldDef,
        '<EXPORT>'(ExpList) = Def ->
            NewDef = '$EXPORT'(ExpList);

        % module is already defined
        '$EXPORT'(ExpList) = OldDef ->
            NewDef = OldDef,
            write('\nERROR: Explist is already defined'); % todo: universal halting
        
        % not a module definition
        '!none' = OldDef ->
        % if 'Def' would have a correct syntax, an earlier clause would have run
            NewDef = OldDef,
            write('\nERROR: Not a explist definition'); % todo: universal halting
        
        % default
        write('ERROR: while module def, unknow.') % todo: universal halting
    
    ).

%declare_explist('!none', '<EXPORT>'(ExpList), ExpList) :- write('\n EXPLDEF.').
%declare_explist(E, '<EXPORT>'(E), E) :- write('\nERROR: Export list already defined'). % todo: universal halting
%declare_explist(E, X, E) :- write('\nERROR: Not an export list definition'), write(X), write(E). % todo: universal halting


% erl:tcm(A).

%TODO: tests for errors
% TODO: test for multiple functions in module
% TODO: clean this section






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

