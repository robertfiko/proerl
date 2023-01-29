% The interface module for ProErl – Simple Erlang Interpreter in Prolog
% Use run(Path, Result) to evaluate an Erlan file, witha  main/0 function
% TODO: rewrite this doc

:- module( utils, [raise/1, split_on/4, init/2, find_main/2] ).

:- set_prolog_flag(toplevel_print_options,
    [quoted(true),numbervars(true),portrayed(true),
                                   max_depth(1000)]).
                                


%TODO: reduce custom utils


raise(Text) :- write(Text), halt.


split_on(List, Element, Left, Right) :-
    append(Left, [Element|Right], List).

init([_], []).
init([X|Xs], [X|WithoutLast]) :- 
    init(Xs, WithoutLast).







% SEMANTIC REPRESENTATION

find_main([Fun|_], Main) :-
    '<FUN>'(main, _, _) = Fun,
    Main = Fun.

find_main([_|Funlist], Main) :- find_main(Funlist, Main).
fund_main([], []).

% TODO: test for yes and no presence of main