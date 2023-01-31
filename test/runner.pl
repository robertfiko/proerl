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



    pass(syntax:parse_terms([[-,module,'(',simple,')','.']], ['<MOD>'(simple)])),
    pass(syntax:parse_terms([[foo,'(','_',')',->,ok,'.']], ['<FUN>'(foo,['_'],['<ATOM>'(ok)])])),
    neg(syntax:parse_terms([[-,random,'(',simple,')','.']], A)),


    neg(semantics:find_main([], _)),
    pass(semantics:find_main(['<FUN>'(main,[],['<EXPR>'(5, '+', 4)])], '<FUN>'(main,[],['<EXPR>'(5, '+', 4)]))),
    pass(semantics:find_main(
        [
            '<FUN>'(kortefa,[],['<EXPR>'(2, '+', 4)]),
            '<FUN>'(main,[],['<EXPR>'(5, '+', 4)]),
            '<FUN>'(almafa,[],['<EXPR>'(4, '+', 4)])
        ], 
        '<FUN>'(main,[],['<EXPR>'(5, '+', 4)]))),

    pass(semantics:find_main(
        [
            '<FUN>'(almafa,[],['<EXPR>'(4, '+', 4)]),
            '<FUN>'(kortefa,[],['<EXPR>'(2, '+', 4)]),
            '<FUN>'(main,[],['<EXPR>'(5, '+', 4)])
        ], 
        '<FUN>'(main,[],['<EXPR>'(5, '+', 4)]))),

    pass(semantics:find_main(
        [   
            '<FUN>'(main,[],['<EXPR>'(5, '+', 4)]),
            '<FUN>'(almafa,[],['<EXPR>'(4, '+', 4)]),
            '<FUN>'(kortefa,[],['<EXPR>'(2, '+', 4)])
        ], 
        '<FUN>'(main,[],['<EXPR>'(5, '+', 4)]))),

    neg(syntax:starts_with_lowercase('Atom')),
    pass(syntax:starts_with_lowercase(apple)),
    



    % ATTRIBUTE
    pass(syntax:to_attribute([-,module,'(',simple,')','.'], '<MOD>'(simple))),
    neg(syntax:to_attribute([-,module,'(',simple,')','.'], '<MOD>'(simple_random))),
    pass(syntax:to_attribute([-,export,'(','[',foo,/,1,']',')','.'], '<EXPORT>'(['<FUNREF>'(foo,1)]))),
    pass(syntax:to_attribute([-,export,'(','[',']',')','.'], '<EXPORT>'([]))),
    pass(syntax:to_attribute([-,export,'(','[',foo,/,1,',',bar,/,2,']',')','.'], '<EXPORT>'(['<FUNREF>'(foo,1), '<FUNREF>'(bar,2)]))),
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

    write('\nTHIS IS OK: '), neg(beam:find_fun(['<FUN>'(goo,['Ya','Xa'],['<VAR>'('Xa')])], foo, 2, error)),
    pass(beam:find_fun(['<FUN>'(goo,['Ya','Xa'],['<VAR>'('Xa')])], goo, 2, '<FUN>'(goo,['Ya','Xa'],['<VAR>'('Xa')]) )),
    pass(beam:find_fun(['<FUN>'(goo,['Ya','Xa'],['<VAR>'('Xa')]), '<FUN>'(goo2,['Ya','Xa'],['<VAR>'('Xa')])], goo, 2, '<FUN>'(goo,['Ya','Xa'],['<VAR>'('Xa')]) )),


    % ARITHMETICS
    pass(syntax:to_expression([683,*,2], '<EXPR>'(683,*,2))),
    pass(syntax:to_expression([5, '+', '(', 4, '-', 3, ')'], '<EXPR>'(5,+,'<EXPR>'(4,-,3)))),
    pass(syntax:to_expression([5, '+', '(', '(', 4, '-', 1, ')', '/', 2, ')'], '<EXPR>'(5,+,'<EXPR>'('<EXPR>'(4,-,1),/,2)))),
    pass(syntax:to_expression(['(', 5, '+', '(', 4, '-', 1, ')', ')', '/', 2], '<EXPR>'('<EXPR>'(5,+,'<EXPR>'(4,-,1)),/,2))),

    pass(beam:eval_arith('<EXPR>'(5,+,'<EXPR>'('<EXPR>'(4,-,1),/,2)), 6.5)),
    pass(beam:eval_arith('<EXPR>'('<EXPR>'(6,+,'<EXPR>'(4,-,1)),/,2), 4.5 )),

    % BEAM UNITS
    pass(beam:get_value('$Alma', error(not_bound))),
    pass(beam:bind_value('$Alma', 'Apple', ok)),
    pass(beam:get_value('$Alma', ok('Apple'))),
    write('\nTHIS IS OK: '), pass(beam:bind_value('$Alma', 'Apple', already_bound)),
    neg(beam:bind_value('$Alma', 'Apfel', cannot_redifine)),
    pass(beam:get_value('$Alma', ok('Apple'))),

    

    ( 
        current_prolog_flag(version, Version), sub_atom(Version, 0, 13, _, Ver) ->
        (
            Ver = 'SICStus 4.7.1' -> 
                run_only_on(Ver)
            ; 
                write('__WARNING__: UNKNOWN PROLOG VERSION, SOME TEST MAY BE SKIPPED') 
        )
        ; write('__WARNING__: CANNOT GET PROLOG VERSION, SOME TEST MAY BE SKIPPED')
    ),
    


    write('\nALL TESTS PASSED'),
    halt(0).




run_only_on('SICStus 4.7.1') :-
    % arithmetics_onemain.erl
    beam:reset_beam(_),
    pass(erl:run('examples/arithmetics_oneliner.erl', 9)),

    write('\n --- THIS IS OK <BEGIN> ---'),
    beam:reset_beam(_),
    neg(erl:run('examples/arithmetics_oneliner.erl', 10)),
    write('\n --- THIS IS OK <END> ---'),



    % arithmetics_onemain.erl
    beam:reset_beam(_),
    pass(erl:run('examples/arithmetics_onemain.erl', 59)),

    write('\n --- THIS IS OK <BEGIN> ---'),
    beam:reset_beam(_),
    neg(erl:run('examples/arithmetics_onemain.erl', 10)),




    % arithmetics.erl
    beam:reset_beam(_),
    pass(erl:run('examples/arithmetics.erl', 1366)),

    write('\n --- THIS IS OK <BEGIN> ---'),
    beam:reset_beam(_),
    neg(erl:run('examples/arithmetics.erl', 10)),



    % atoms.erl
    beam:reset_beam(_),
    pass(erl:run('examples/atoms.erl', szilva)),

    write('\n --- THIS IS OK <BEGIN> ---'),
    beam:reset_beam(_),
    neg(erl:run('examples/atoms.erl', korte)),

    
    % % TODO: chained binding not works
    % binding.erl
    beam:reset_beam(_),
    pass(erl:run('examples/binding.erl', c)),

    write('\n --- THIS IS OK <BEGIN> ---'),
    beam:reset_beam(_),
    neg(erl:run('examples/binding.erl', 16)),
    
    

    % catchy_comments.erl
    beam:reset_beam(_),
    pass(erl:run('examples/catchy_comments.erl', london)),
 
    
    %%%%%%%%%%%% simple.erl %%%%%%%%%%%%
    pass(erl:run('examples/simple.erl', pear_tree)),
    

    write('\n --- THIS IS OK <BEGIN> ---'),
    neg(erl:run('examples/simple.erl', error)),
    write('\n --- THIS IS OK <END> ---'),

    % FAIL IF NO MAIN
    neg(erl:run('examples/moddef.erl', error)),

    write('\nSICStus ONLY TESTS PASSED.\n').



% Usefull structures for testing
% [[-,module,'(',simple,')','.'],[-,export,'(','[',foo,/,1,']',')','.'],[],[foo,'(','_',')',->,ok,'.']]
% [foo,'(','_',')',->,ok,'.']
% ['<MOD>'(arithmetics_onemain),'<EXPORT>'(['<FUNREF>'(main,0)]),'<FUN>'(main,[],['<EXPR>'(16,+,43)])]

