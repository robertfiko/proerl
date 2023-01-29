:- module( runner, [run_tests/1] ).

:- use_module( '../src/erl', [] ).
:- use_module( '../src/syntax', [] ).
:- use_module( '../src/utils', [] ).
%:- use_module( '../src/semantics', [] ).
:- use_module( '../src/beam', [] ).


:- set_prolog_flag(toplevel_print_options,
    [quoted(true),numbervars(true),portrayed(true),
                                   max_depth(1000)]).

not(P) :- (P -> fail;true).

pass(Term) :- 
    Term -> 
        true
    ;
        (
            write('P-FAILED: '),
            write(Term),
            write('\n'),
            halt
        ).

neg(Term) :- 
    not(Term) -> 
        true
    ;
        (
            write('F-FAILED: '),
            write(Term),
            write('\n'),
            halt
        ).

run_tests(_) :-
    % UTILS
    pass(utils:init([1,2,3,4,5], [1,2,3,4])),
    pass(utils:init([1,2,3], [1,2])),
    pass(utils:init([1], [])),
    neg(utils:init([], [])), % implementation is partial

    pass(utils:split_on([a,b,c,x,c,b,a], x, [a,b,c], [c,b,a])),
    pass(utils:split_on([a,b,c,x], x, [a,b,c], [])),
    pass(utils:split_on([x,a,b,c], x, [], [a,b,c])),
    pass(utils:split_on([x], x, [], [])),

    % cannot split on elem, not in list
    neg(utils:split_on([a,b,c], x, [], [a,b,c])), 
    neg(utils:split_on([], x, [], [])),


    neg(utils:find_main([], _)),
    pass(utils:find_main(['<FUN>'(main,[],['<EXPR>'(5, '+', 4)])], '<FUN>'(main,[],['<EXPR>'(5, '+', 4)]))),
    pass(utils:find_main(
        [
            '<FUN>'(kortefa,[],['<EXPR>'(2, '+', 4)]),
            '<FUN>'(main,[],['<EXPR>'(5, '+', 4)]),
            '<FUN>'(almafa,[],['<EXPR>'(4, '+', 4)])
        ], 
        '<FUN>'(main,[],['<EXPR>'(5, '+', 4)]))),

    pass(utils:find_main(
        [
            '<FUN>'(almafa,[],['<EXPR>'(4, '+', 4)]),
            '<FUN>'(kortefa,[],['<EXPR>'(2, '+', 4)]),
            '<FUN>'(main,[],['<EXPR>'(5, '+', 4)])
        ], 
        '<FUN>'(main,[],['<EXPR>'(5, '+', 4)]))),

    pass(utils:find_main(
        [   
            '<FUN>'(main,[],['<EXPR>'(5, '+', 4)]),
            '<FUN>'(almafa,[],['<EXPR>'(4, '+', 4)]),
            '<FUN>'(kortefa,[],['<EXPR>'(2, '+', 4)])
        ], 
        '<FUN>'(main,[],['<EXPR>'(5, '+', 4)]))),
    



    % ATTRIBUTE
    pass(syntax:to_attribute([-,module,'(',simple,')','.'], '<MOD>'(simple))),
    neg(syntax:to_attribute([-,module,'(',simple,')','.'], '<MOD>'(simple_random))),
    pass(syntax:to_attribute([-,export,'(','[',foo,/,1,']',')','.'], '<EXPORT>'(['<FUNREF>'(foo,1)]))),
    % TODO: empty export list
    % TODO: longer export list
    neg(syntax:to_attribute([-,cica,'(',simple,')','.'], _)), % not an attribute
    neg(syntax:to_attribute([cica,'(',simple,')','.'], _)), % not an attribute syntax


    pass(syntax:format_export_list([foo,/,1,',',main,/,0], ['<FUNREF>'(foo,1),'<FUNREF>'(main,0)])),
    pass(syntax:format_export_list([foo,/,1], ['<FUNREF>'(foo,1)])),
    pass(syntax:format_export_list([], [])),

    neg(syntax:format_export_list([foo,/,1,main,/,0], ['<FUNREF>'(foo,1),'<FUNREF>'(main,0)])),
    neg(syntax:format_export_list([foo,/,main,/,0], ['<FUNREF>'(foo,1),'<FUNREF>'(main,0)])),
    neg(syntax:format_export_list([foo,main], ['<FUNREF>'(foo,1),'<FUNREF>'(main,0)])),


    % FUNCTION
    pass(syntax:take_until_funbody([[korte, ','], [5, '+', 6, '.'], [szilva]], [[korte,','],[5,+,6,'.']], [[szilva]])),
    pass(syntax:take_until_funbody([[korte, ','], [5, '+', 6, '.'], [bar, '(', ')', ->], [ok, '.']], [[korte,','],[5,+,6,'.']], [[bar, '(', ')', ->], [ok, '.']])),



    % ARITHMETICS
    pass(syntax:to_expression([5, '+', '(', 4, '-', 3, ')'], '<EXPR>'(5,+,'<EXPR>'(4,-,3)))),
    pass(syntax:to_expression([5, '+', '(', '(', 4, '-', 1, ')', '/', 2, ')'], '<EXPR>'(5,+,'<EXPR>'('<EXPR>'(4,-,1),/,2)))),
    pass(syntax:to_expression(['(', 5, '+', '(', 4, '-', 1, ')', ')', '/', 2], '<EXPR>'('<EXPR>'(5,+,'<EXPR>'(4,-,1)),/,2))),

    pass(beam:eval_arith('<EXPR>'(5,+,'<EXPR>'('<EXPR>'(4,-,1),/,2)), 6.5)),
    %pass(erl:eval_arith('<EXPR>'('<EXPR>'(5,+,'<EXPR>'(4,-,1)),/,2), 4 )),
    %TODO: failing on Sicstus
    

    ( 
        current_prolog_flag(version, Version), sub_atom(Version, 0, 13, _, Ver) ->
        (
            Ver = 'SICStus 4.7.1' -> 
                run_only_on(Ver)
            ; 
                debug('__WARNING__: UNKNOWN PROLOG VERSION, SOME TEST MAY BE SKIPPED') 
        )
        ; debug('__WARNING__: CANNOT GET PROLOG VERSION, SOME TEST MAY BE SKIPPED')
    ),
    


    write('ALL TESTS PASSED').




run_only_on('SICStus 4.7.1') :-
    % TODO:
    erl:run('examples/arithmetics_onemain.erl', 59),
    write('SICStus ONLY TESTS PASSED.\n').



% TODO: tests
% [[-,module,'(',simple,')','.'],[-,export,'(','[',foo,/,1,']',')','.'],[],[foo,'(','_',')',->,ok,'.']]
% [foo,'(','_',')',->,ok,'.']
% function([foo,'(','_',')',->,ok,'.'], Term).

% ['<MOD>'(arithmetics_onemain),'<EXPORT>'(['<FUNREF>'(main,0)]),'<FUN>'(main,[],['<EXPR>'(16,+,43)])]

% function([bar,'(','Apple',',','Pear',')',->,london,',',foo,'(','Pear',')','.'], Term).
% split_on([foo,'(','_',')',->,ok,'.'], '->', FunHeader, FunBody).


%split_on(MaybeFun, '->', FunHeader, FunBody),
% TEST erl:take_until_funbody([[],[5,+,4,,],[683,*,2,.],[main,(,),->],[foo,(,whatever,),.]], R).



% syntax:parse_terms([[-,module,'(',simple,')','.']], ['<MOD>'(simple)]).
% syntax:parse_terms([[foo,'(','_',')',->,ok,'.']], A).
% syntax:parse_terms([[-,random,'(',simple,')','.']], A)