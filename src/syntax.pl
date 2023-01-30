% The syntax checkers and term parsers for 
% ProErl – Simple Erlang Interpreter in Prolog                               

:- module( syntax, [parse_terms/2, is_expr/1, is_function/1] ).

:- use_module( utils, [split_on/4, init/2] ).
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

% In case of comments – JUST SKIPPED
parse_terms([Term|TL], NL) :-
    ['%'|_] = Term,
    parse_terms(TL, NL).


% In case of unrecognised terms
parse_terms([Term|_TL], ['<???>'(Term)|_NL]) :-
    write('Unknow term: '), write(Term),
    write('ERROR Unknow term'), fail.


% Turn raw termlist to Attribute NODE
to_attribute([-,module,'(',ModuleName,')','.'], '<MOD>'(ModuleName)).
to_attribute([-,export|Rest], '<EXPORT>'(ExportList)) :-
    split_on(Rest, '[', ['('], ExpList0), % left side of split_on is to ensure syntax
    split_on(ExpList0, ']', ExpList1, [')','.']), % right side of split_on is to ensure syntax
    format_export_list(ExpList1, ExportList).


% Turn raw termlist to Function NODE
function(MaybeFun, TermList, '<FUN>'(FunName, Arglist, FunBody), Rem) :-
    split_on(MaybeFun, '->', FunHeader, FunBody0),
    split_on(FunHeader, '(', [FunName], Args),
    is_atom(FunName),
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


% TODO: need to check for following comma and whether we can move on or not
% TODO: type of params
% TODO: type of fun Id
% TODO: finish function call



function_body([],[]).
function_body([Term|FBody], [Node|Nodes]) :-
    fbody_item(Term, Node),
    function_body(FBody, Nodes).


% In Function body: Artih Expression | Atoms |
% TODO: Function call, 
% TODO: variable bindings

% For Arithmetic Expressions
fbody_item(Term, Node) :-
    init(Term, SmallTerm), % this probably can an obstacle to multi-statement lines
    to_expression(SmallTerm, Node),
    is_expr(Node).

% For Function Calls
fbody_item(Term, Node) :-
    init(Term, SmallTerm), % this probably can an obstacle to multi-statement lines
    to_function_call(SmallTerm, Node).

% For Atoms
fbody_item(Term, Node) :-
    init(Term, [Atom]),
    is_atom(Atom),
    Node = '<ATOM>'(Atom).

% For Bindings
fbody_item(Term, Node) :-
    to_binding(Term, Node).

% For Variables
fbody_item(Term, Node) :-
    to_variable(Term, Node).

% FALLBACK ~ DEFAULT
fbody_item(Term, '<???>'(Term)) :-
    write('Unknow function body term: '), write(Term), write('\n'), fail.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CHECKERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


is_function('<FUN>'(_, _, _)).

% syntax:starts_with_lowercase('Atom').
% 
starts_with_lowercase(Atom) :-
    atom_chars(Atom, [FirstChar|_]),
    char_code(FirstChar, Code),
    Code >= 97,
    Code =< 122.

is_atom(Term) :- 
    atom(Term), 
    starts_with_lowercase(Term).


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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CONVERTERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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



to_function_call([FunName|Term], '<FUNCALL>'(FunName, Arity, Args)) :-
    is_atom(FunName),
    split_on(Term, '(', A, PartialArglist), % TODO: ensure syntax
    split_on(PartialArglist, ')', B, RawArglist), % TODO: ensure syntax
    Arity = -1, %TODO:
    write(A), write('  ----  '), write(B), write('\n'),
    Args = RawArglist.


to_binding(Term, Node) :-
    split_on(Term, '=', [LeftSide], RightSide), % ensure that on the left side there is only one thing
    init(RightSide, SmallRightSide),  % TODO: small side should be rather evaluated but maybe in beam 

    % on the right hand side similar items can occour than in the function body
    fbody_item(RightSide, RighNode),
    % TODO: check for correctnes on all types
    Node = '<BINDING>'(LeftSide, RighNode).


accept_var([V,'.'], V). accept_var([V,','], V).
to_variable(Term, Node) :-
    accept_var(Term, V),
    Node = '<VAR>'(V). 



    %TODO: doc what kind of nodes we have
% TODO: check if has correct naming syntax on variables
    