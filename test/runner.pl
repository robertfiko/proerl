:- module( runner, [run_tests/1] ).
:- use_module( '../src/erl', [] ).

%:- import(erl:attribute/1).

:- set_prolog_flag(toplevel_print_options,
    [quoted(true),numbervars(true),portrayed(true),
                                   max_depth(1000)]).

not(P) :- (P -> fail;true).

pass(Term) :- Term -> true;(write('FAILED'), halt).
neg(Term) :- not(Term) -> true; (write('FAILED'), halt).

run_tests(_) :-
    % ATTRIBUTE
    pass(erl:attribute([-,module,'(',simple,')','.'], '<MOD>'(simple))),
    neg(erl:attribute([-,module,'(',simple,')','.'], '<MOD>'(simple_random))),
    pass(erl:attribute([-,export,'(','[',foo,/,1,']',')','.'], '<EXPORT>'([foo,/,1]))),
    neg(erl:attribute([-,cica,'(',simple,')','.'], _)),

    % FUNCTION
    pass(erl:take_until_funbody([[korte, ','], [5, '+', 6, '.'], [szilva]], [[korte,','],[5,+,6,'.']])),


    % ARITHMETICS
    pass(erl:group_expression([5, '+', '(', 4, '-', 3, ')'], '<EXPR>'(5,+,'<EXPR>'(4,-,3)))),
    pass(erl:group_expression([5, '+', '(', '(', 4, '-', 1, ')', '/', 2, ')'], '<EXPR>'(5,+,'<EXPR>'('<EXPR>'(4,-,1),/,2)))),
    pass(erl:group_expression(['(', 5, '+', '(', 4, '-', 1, ')', ')', '/', 2], '<EXPR>'('<EXPR>'(5,+,'<EXPR>'(4,-,1)),/,2))),





    write('ALL TESTS PASSED').