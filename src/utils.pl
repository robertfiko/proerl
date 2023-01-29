% The utility module for ProErl – Simple Erlang Interpreter in Prolog
:- module( utils, [split_on/4, init/2, find_main/2] ).

:- set_prolog_flag(toplevel_print_options,
    [quoted(true),numbervars(true),portrayed(true),
                                   max_depth(1000)]).
                                

% Split 'List' on 'Element'
split_on(List, Element, Left, Right) :-
    append(Left, [Element|Right], List).

% Get the list without the last item 
init([_], []).
init([X|Xs], [X|WithoutLast]) :- 
    init(Xs, WithoutLast).

