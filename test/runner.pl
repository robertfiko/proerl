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
    pass(erl:attribute([-,module,'(',simple,')','.'], '<MOD>'(simple))),
    %pass(erl:attribute([-,export,'(','[',foo,/,1,']',')','.'], <EXP>'(ExportList))),
    %neg(erl:attribute([-,cica,'(',simple,')','.'])),
    write('ALL TESTS PASSED').