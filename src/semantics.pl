:- module( semantics, [construct_module/2] ).

:- use_module( utils, [raise/1, split_on/4, init/2] ).

:- set_prolog_flag(toplevel_print_options,
    [quoted(true),numbervars(true),portrayed(true),
                                   max_depth(1000)]).

%
% 
% 
% 
% ez már  szemantikus része, mivel itt már a szintaktikus egységek rendelkezésre állnak, és itt egy szemantikailag
% is értelmes kódot kapunk
% 
% 
% 






%tcm(A) :-
%    Nodes = ['<MOD>'(arithmetics_onemain),'<EXPORT>'(['<FUNREF>'(main,0)]),'<FUN>'(main,[],['<EXPR>'(16,+,43)])],
%    construct_module(Nodes, A).

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
construct_module([Node|Nodes], ModDef, Exportlist, Funs, MOD) :- % TODO: azt mondja Export
    (
        '<MOD>'(_) = Node -> 
            declare_module(ModDef, Node, NewModDef),
            construct_module(Nodes, NewModDef, '!none', Funs, MOD);
        '<EXPORT>'(_) = Node ->
            declare_explist(Exportlist, Node, NewExportList), % TODO Export_L_ist volt nagy L-el. így is jó?
            construct_module(Nodes, ModDef, NewExportList, Funs, MOD); 
        '<FUN>'(_, _, _) = Node ->
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
        '<MOD>'(ModName) = OldDef ->
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
        write('ERROR: while explist def, unknow. \n NewDef: '), 
        write(Def),  write('\n OldDef: '), write(OldDef) % todo: universal halting
    
    ).

%declare_explist('!none', '<EXPORT>'(ExpList), ExpList) :- write('\n EXPLDEF.').
%declare_explist(E, '<EXPORT>'(E), E) :- write('\nERROR: Export list already defined'). % todo: universal halting
%declare_explist(E, X, E) :- write('\nERROR: Not an export list definition'), write(X), write(E). % todo: universal halting


% erl:tcm(A).

%TODO: tests for errors
% TODO: test for multiple functions in module
% TODO: clean this section
