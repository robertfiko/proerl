% The interface module for ProErl – Simple Erlang Interpreter in Prolog
% Use run(Path, Result) to evaluate an Erlan file, witha  main/0 function
% TODO: shell

:- module( erl, [run/1, run/2] ).

:- use_module( scan, [scan/2, reap_tokens/2] ).
:- use_module( syntax, [parse_terms/2] ).
:- use_module( semantics, [construct_module/2, find_main/2] ).
:- use_module( beam, [eval/2, set_mod/1, reset_beam/1 ] ).
:- use_module( utils, [init/2] ).

:- set_prolog_flag(toplevel_print_options,
    [quoted(true),numbervars(true),portrayed(true),
                                   max_depth(1000)]).

:- use_module(library(lists), [exclude/3, reverse/2]).





%%%%%%%%%%%%%%% ?? ONLY FOR TESTING PURPOSES ?? %%%%%%%%%%%%%%%
run(Result) :- run('examples/simple.erl', Result).
%%%%%%%%%%%%%%% ?? ONLY FOR TESTING PURPOSES ?? %%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   MAIN IO 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run(Path, Final) :- 
    scan_whole_file(Path, Result),
    init(Result, TermList0),
    exclude(=([]), TermList0, TermList),
    parse_terms(TermList, NodeList),
    construct_module(NodeList, Module),
    write(NodeList), write('\n'),
    '$MODULE'(_, _, FunList) = Module,

    reset_beam(_),
    set_mod(Module),
    find_main(FunList, Main),
    write('Main found, running it!\n'), 
    eval(Main, Value),

    Final = Value.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   INTERNAL Functions 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Scan the whole file until EOF
scan_whole_file(Path, Result) :-
    open(Path, read, Stream), 
    set_input(Stream),
    start_scan(Result),
    close(Stream).

% Initation scanning with error handling
start_scan([List|OtherResults]) :- 
    catch(
        (   get_code(C1), 
            reap_tokens(C1,List),
            start_scan(OtherResults)
        ),
        _Error, % error is not raised to the user, as it is handled
        (OtherResults = [])
    ).



