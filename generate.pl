#!/usr/bin/swipl
file_search_path(library, './lib').
file_search_path(template, './templates').

:- use_module(library(loader)).
:- use_module(interface).

:- initialization(main, main).

main([Config | Files]) :-
    consult(Config),
    forall(member(File, Files),
           load_source(File, _)),
    qsave_program('query.cgi', [stand_alone(true), goal(handle_request)]).