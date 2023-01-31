% The syntax checkers and term parsers for 
% ProErl – Simple Erlang Interpreter in Prolog                               

:- module( syntax, [parse_terms/2, is_expr/1, is_function_node/1] ).

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
    is_atom_name(FunName),
    split_on(Args, ')', Arglist0, []),
    exclude(=(','), Arglist0, Arglist), % filters element where goal failed
    take_until_funbody([FunBody0|TermList], FunBody1, Rem),
    function_body(FunBody1, FunBody2),
    exclude(=([]), FunBody2, FunBody).



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


function_body([],[]).
function_body([Term|FBody], [Node|Nodes]) :-
    fbody_item(Term, Node),
    function_body(FBody, Nodes).


% In Function body: Artih Expression | Atoms | Function call | variable bindings
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
    is_atom_name(Atom),
    Node = '<ATOM>'(Atom).

% For Bindings
fbody_item(Term, Node) :-
    to_binding(Term, Node).

% For Variables
fbody_item(Term, Node) :-
    to_variable(Term, Node).

% For Comments
fbody_item(['%',_], []).

% Empty lines
fbody_item([], []).

% FALLBACK ~ DEFAULT
fbody_item(Term, '<???>'(Term)) :-
    write('Unknow function body term: '), write(Term), write('\n'), fail.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CHECKERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

starts_with_lowercase(Atom) :-
    atom_chars(Atom, [FirstChar|_]),
    char_code(FirstChar, Code),
    Code >= 97,
    Code =< 122.

starts_with_uppercase(Atom) :-
    atom_chars(Atom, [FirstChar|_]),
    char_code(FirstChar, Code),
    Code >= 65,
    Code =< 90.



is_function_node('<FUN>'(_, _, _)).
is_atom_node('<ATOM>'(A)) :- is_atom_name(A).
is_variable_node('<VAR>'(V)) :- is_variable_name(V).
is_funcall_node('<FUNCALL>'(FunName, Arity, Args)) :- is_atom_name(FunName).


is_atom_name(Term) :- 
    atom(Term), 
    starts_with_lowercase(Term).


is_variable_name(Term) :- 
    atom(Term),
    starts_with_uppercase(Term).


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

% For Atoms
arg_item(Term, Node) :-
    is_atom_name(Term),
    Node = '<ATOM>'(Term).


% For Variables
arg_item(Term, Node) :-
    is_variable_name(Term),
    Node = '<VAR>'(Term). 

% FALLBACK ~ DEFAULT
arg_item(Term, '<???>'(Term)) :-
    write('Unknow arg term: '), write(Term), write('\n'), fail.


% In FCall args can be: Atom, Expression, Var
% We can start by using the to_funbody predicate, than introduce some safe cheks
% Accepted types: EXPR | ATOM | VAR
accept_as_arg_item(N) :- is_expr(N).
accept_as_arg_item(N) :- is_atom_node(N).
accept_as_arg_item(N) :- is_variable_node(N).
accept_as_arg_item(X) :- write('Cannot bind to arg: '), write(X), write('\n'), fail.
to_arglist_nodes([], []).
to_arglist_nodes([A|Args], [N|Nodes]) :- % arith atom var
    arg_item(A, N), 
    accept_as_arg_item(N),
    to_arglist_nodes(Args, Nodes).

to_function_call([FunName|Term], '<FUNCALL>'(FunName, Arity, Args)) :-
    is_atom_name(FunName),
    split_on(Term, '(', [], PartialArglist), 
    split_on(PartialArglist, ')', RawArglist, []),
    exclude(=(','), RawArglist, Arglist),
    length(Arglist, Arity),
    to_arglist_nodes(Arglist, Args).

accepted_binding_right(Node) :- is_expr(Node).
accepted_binding_right(Node) :- is_atom_node(Node).
accepted_binding_right(Node) :- is_variable_node(Node).
accepted_binding_right(Node) :- is_funcall_node(Node).
to_binding(Term, Node) :-
    split_on(Term, '=', [LeftSide], RightSide), % ensure that on the left side there is only one thing

    % on the right hand side similar items can occour than in the function body
    fbody_item(RightSide, RighNode),
    accepted_binding_right(RighNode),
    
    Node = '<BINDING>'(LeftSide, RighNode).


unwrap_var([V,'.'], V). unwrap_var([V,','], V). unwrap_var(V, V) :- is_atom_name(V).
to_variable(Term, Node) :-
    unwrap_var(Term, V),
    is_variable_name(V),
    Node = '<VAR>'(V). 

    