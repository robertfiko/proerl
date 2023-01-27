% Program to scan a file and assemble identifiers,
% sequences of special characters, numbers and delimiters.
:- module( erl, [run/1, run/2] ).

:- use_module( scan, [scan/2, reap_tokens/2] ).

:- set_prolog_flag(toplevel_print_options,
    [quoted(true),numbervars(true),portrayed(true),
                                   max_depth(1000)]).

run(Result) :- run('examples/simple.erl', Result).

run(Path, TermList) :- 
    scan_whole_file(Path, Result),
    init(Result, TermList).

init([_], []).
init([X|Xs], [X|WithoutLast]) :- 
    init(Xs, WithoutLast).

scan_whole_file(Path, Result) :-
    open(Path, read, Stream), 
    set_input(Stream),
    start_scan(Result),
    close(Stream).
    %init(RawResult, Result).


start_scan([List|OtherResults]) :- 
    catch(
        (   get_code(C1), 
            reap_tokens(C1,List),
            start_scan(OtherResults)
        ),
        Error,
        (print_message(error, Error), OtherResults = [])
    ).
    
    
    