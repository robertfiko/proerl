:- use_module( '../src/erl', [is_attribute/1] ).

:- set_prolog_flag(toplevel_print_options,
    [quoted(true),numbervars(true),portrayed(true),
                                   max_depth(1000)]).

not(P) :- (P -> fail;true).

pass(Term) :- Term -> true;(write('FAILED'), halt).
neg(Term) :- not(Term) -> true; (write('FAILED'), halt).

run_tests(_) :-
    pass(is_attribute([-,module,'(',simple,')','.'])),
    neg(is_attribute([-,cica,'(',simple,')','.'])),
    write('ALL TESTS PASSED').