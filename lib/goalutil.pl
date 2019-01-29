:- module(goalutil, [enumerate/2, strictly_once/1]).
:- use_module(library(error)).

:- meta_predicate enumerate(0, ?).
:- meta_predicate strictly_once(0).

enumerate(Goal, Idx) :-
    Counter = count(1),
    call(Goal),
    arg(1, Counter, Idx),
    Idx1 is Idx + 1,
    nb_setarg(1, Counter, Idx1).

strictly_once(Goal) :-
    Keep = keep(_),
    (call(Goal),
     arg(1, Keep, Goal0),
     (var(Goal0) ->
      nb_setarg(1, Keep, Goal),
      fail;
      domain_error(deterministic, Goal));
     arg(1, Keep, Goal0), nonvar(Goal0), Goal = Goal0).
