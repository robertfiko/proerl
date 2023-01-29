% The utility module for ProErl – Simple Erlang Interpreter in Prolog
:- module( utils, [raise/1, split_on/4, init/2, find_main/2] ).

:- set_prolog_flag(toplevel_print_options,
    [quoted(true),numbervars(true),portrayed(true),
                                   max_depth(1000)]).
                                
% Raise an error. Write to STDIO, than exit.
raise(Text) :- write(Text), halt.

% Split 'List' on 'Element'
split_on(List, Element, Left, Right) :-
    append(Left, [Element|Right], List).

% Get the list without the last item 
init([_], []).
init([X|Xs], [X|WithoutLast]) :- 
    init(Xs, WithoutLast).



%%%%%%%%%%%%%%%%%%%%%%%% SEMANTIC REPRESENTATION UTILS %%%%%%%%%%%%%%%%%%%%%%%%%
find_main([Fun|_], Main) :-
    '<FUN>'(main, _, _) = Fun,
    Main = Fun.

find_main([_|Funlist], Main) :- find_main(Funlist, Main).
fund_main([], []).