:- module( runner, [run_tests/1] ).
:- use_module( '../src/erl', [] ).

%:- import(erl:attribute/1).

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
    % ATTRIBUTE
    pass(erl:to_attribute([-,module,'(',simple,')','.'], '<MOD>'(simple))),
    neg(erl:to_attribute([-,module,'(',simple,')','.'], '<MOD>'(simple_random))),
    pass(erl:to_attribute([-,export,'(','[',foo,/,1,']',')','.'], '<EXPORT>'(['<FUNREF>'(foo,1)]))),
    % TODO: empty export list
    % TODO: longer export list
    neg(erl:to_attribute([-,cica,'(',simple,')','.'], _)), % not an attribute
    neg(erl:to_attribute([cica,'(',simple,')','.'], _)), % not an attribute syntax


    pass(erl:format_export_list([foo,/,1,',',main,/,0], ['<FUNREF>'(foo,1),'<FUNREF>'(main,0)])),
    pass(erl:format_export_list([foo,/,1], ['<FUNREF>'(foo,1)])),
    pass(erl:format_export_list([], [])),

    neg(erl:format_export_list([foo,/,1,main,/,0], ['<FUNREF>'(foo,1),'<FUNREF>'(main,0)])),
    neg(erl:format_export_list([foo,/,main,/,0], ['<FUNREF>'(foo,1),'<FUNREF>'(main,0)])),
    neg(erl:format_export_list([foo,main], ['<FUNREF>'(foo,1),'<FUNREF>'(main,0)])),


    % FUNCTION
    pass(erl:take_until_funbody([[korte, ','], [5, '+', 6, '.'], [szilva]], [[korte,','],[5,+,6,'.']], [[szilva]])),
    pass(erl:take_until_funbody([[korte, ','], [5, '+', 6, '.'], [bar, '(', ')', ->], [ok, '.']], [[korte,','],[5,+,6,'.']], [[bar, '(', ')', ->], [ok, '.']])),



    % ARITHMETICS
    pass(erl:to_expression([5, '+', '(', 4, '-', 3, ')'], '<EXPR>'(5,+,'<EXPR>'(4,-,3)))),
    pass(erl:to_expression([5, '+', '(', '(', 4, '-', 1, ')', '/', 2, ')'], '<EXPR>'(5,+,'<EXPR>'('<EXPR>'(4,-,1),/,2)))),
    pass(erl:to_expression(['(', 5, '+', '(', 4, '-', 1, ')', ')', '/', 2], '<EXPR>'('<EXPR>'(5,+,'<EXPR>'(4,-,1)),/,2))),

    pass(erl:eval_arith('<EXPR>'(5,+,'<EXPR>'('<EXPR>'(4,-,1),/,2)), 6.5)),
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
    erl:run('examples/arithmetics_onemain.erl', ['<MOD>'(arithmetics_onemain),'<EXPORT>'(['<FUNREF>'(main,0)]),'<FUN>'(main,[],['<EXPR>'(16,+,43)])]),
    write('SICStus ONLY TESTS PASSED.\n').



% TODO: tests
% [[-,module,'(',simple,')','.'],[-,export,'(','[',foo,/,1,']',')','.'],[],[foo,'(','_',')',->,ok,'.']]
% [foo,'(','_',')',->,ok,'.']
% function([foo,'(','_',')',->,ok,'.'], Term).

% function([bar,'(','Apple',',','Pear',')',->,london,',',foo,'(','Pear',')','.'], Term).
% split_on([foo,'(','_',')',->,ok,'.'], '->', FunHeader, FunBody).


%split_on(MaybeFun, '->', FunHeader, FunBody),
% TEST erl:take_until_funbody([[],[5,+,4,,],[683,*,2,.],[main,(,),->],[foo,(,whatever,),.]], R).
