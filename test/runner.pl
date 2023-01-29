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
            write('FAILED: '),
            write(Term),
            write('\n'),
            halt
        ).

neg(Term) :- not(Term) -> true; (write('FAILED'), halt).

run_tests(_) :-
    % ATTRIBUTE
    pass(erl:to_attribute([-,module,'(',simple,')','.'], '<MOD>'(simple))),
    neg(erl:to_attribute([-,module,'(',simple,')','.'], '<MOD>'(simple_random))),
    pass(erl:to_attribute([-,export,'(','[',foo,/,1,']',')','.'], '<EXPORT>'([foo,/,1]))),
    neg(erl:to_attribute([-,cica,'(',simple,')','.'], _)), % not an attribute syntax

    % FUNCTION
    pass(erl:take_until_funbody([[korte, ','], [5, '+', 6, '.'], [szilva]], [[korte,','],[5,+,6,'.']], [[szilva]])),
    pass(erl:take_until_funbody([[korte, ','], [5, '+', 6, '.'], [bar, '(', ')', ->], [ok, '.']], [[korte,','],[5,+,6,'.']], [[bar, '(', ')', ->], [ok, '.']])),



    % ARITHMETICS
    pass(erl:group_expression([5, '+', '(', 4, '-', 3, ')'], '<EXPR>'(5,+,'<EXPR>'(4,-,3)))),
    pass(erl:group_expression([5, '+', '(', '(', 4, '-', 1, ')', '/', 2, ')'], '<EXPR>'(5,+,'<EXPR>'('<EXPR>'(4,-,1),/,2)))),
    pass(erl:group_expression(['(', 5, '+', '(', 4, '-', 1, ')', ')', '/', 2], '<EXPR>'('<EXPR>'(5,+,'<EXPR>'(4,-,1)),/,2))),

    pass(erl:eval_arith('<EXPR>'(5,+,'<EXPR>'('<EXPR>'(4,-,1),/,2)), 6.5)),
    pass(erl:eval_arith('<EXPR>'('<EXPR>'(5,+,'<EXPR>'(4,-,1)),/,2), 4 )),



    write('ALL TESTS PASSED').