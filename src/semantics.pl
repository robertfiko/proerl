% This module contains semantic transformers and checkers for
% ProErl – Simple Erlang Interpreter in Prolog
% 
% At this point the syntactical nodes are present, so we can start the 
% semantical analysis

:- module( semantics, [construct_module/2] ).

:- use_module( utils, [split_on/4, init/2] ).

:- set_prolog_flag(toplevel_print_options,
    [quoted(true),numbervars(true),portrayed(true),
                                   max_depth(1000)]).


construct_module(Nodes, Module) :-
    construct_module(Nodes, '!none', '!none', [], '!MOD'(Mod, Exp, Funs)),

    % checks
    Mod = '$MOD'(ModDef), atom(ModDef),

    '$EXPORT'(ExportList) = Exp, 
    ['<FUNREF>'(_,_)|_] = ExportList,

    ['<FUN>'(_, _, _)|_] = Funs,

    Module = '$MODULE'(Mod, Exp, Funs).

construct_module([], Mod, Exp, Funs, '!MOD'(Mod, Exp, Funs)).
construct_module([Node|Nodes], ModDef, Exportlist, Funs, MOD) :-
    (
        '<MOD>'(_) = Node -> 
            declare_module(ModDef, Node, NewModDef),
            construct_module(Nodes, NewModDef, '!none', Funs, MOD);
        '<EXPORT>'(_) = Node ->
            declare_explist(Exportlist, Node, NewExportList),
            construct_module(Nodes, ModDef, NewExportList, Funs, MOD); 
        '<FUN>'(_, _, _) = Node ->
            construct_module(Nodes, ModDef, Exportlist, [Node|Funs], MOD);
        write('\nERROR: Unrecognised node type: '), write(Node), write('\n'), fail
        %construct_module(Nodes, NewModDef, Exportlist, Funs, MOD);
    ).

declare_module(OldDef, Def, NewDef) :-
    (   
        % module is not defined already
        '!none' = OldDef,
        '<MOD>'(ModName) = Def ->
            NewDef = '$MOD'(ModName);

        % module is already defined
        '<MOD>'(ModName) = OldDef ->
            NewDef = OldDef,
            write('\nERROR: Module already defined'), fail; 
        
        % not a module definition
        '!none' = OldDef ->
        % if 'Def' would have a correct syntax, an earlier clause would have run
            NewDef = OldDef,
            write('\nERROR: Not a module definition'), fail; 
        
        % default
        write('ERROR: while module def, unknow.'), fail
    
    ).



declare_explist(OldDef, Def, NewDef) :-
    (   
        % module is not defined already
        '!none' = OldDef,
        '<EXPORT>'(ExpList) = Def ->
            NewDef = '$EXPORT'(ExpList);

        % module is already defined
        '$EXPORT'(ExpList) = OldDef ->
            NewDef = OldDef,
            write('\nERROR: Explist is already defined'), fail; 
        
        % not a module definition
        '!none' = OldDef ->
        % if 'Def' would have a correct syntax, an earlier clause would have run
            NewDef = OldDef,
            write('\nERROR: Not a explist definition'), fail; 
        
        % default
        write('ERROR: while explist def, unknow. \n NewDef: '), 
        write(Def),  write('\n OldDef: '), write(OldDef), fail
    
    ).

find_main([Fun|_], Main) :-
    '<FUN>'(main, _, _) = Fun,
    Main = Fun.

find_main([_|Funlist], Main) :- find_main(Funlist, Main).
find_main([], error) :- fail.
