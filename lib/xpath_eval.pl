:- module(xpath_eval, [xpath_eval/3, xpath_evaluate/3,
                       xpath_instance_of/2, xpath_subtype_of/2]).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(aggregate)).
:- use_module(library(error)).
:- use_module(library(dom)).
:- use_module(library(goalutil)).
:- use_module(library(xpath_syntax)).
:- use_module(library(ebnf)).

:- multifile xpath_function/4.
:- multifile xpath_atomic_instance_of/2.
:- multifile xpath_atomic_subtype_of/2.
:- multifile xpath_atomic_cast/3.

make_binding(X:_, Y, X = Y).

make_partial_application(Bindings, Function, Arguments, curry(Vars, apply(literal(Function), PartialArgs))) :-
    make_partial_args(Bindings, Arguments, Vars, PartialArgs).

make_partial_args(_, [], [], []).
make_partial_args(Bindings, ['?' | Rest], [V | NextVars], [literal(V) | NextArgs]) :- !,
    make_partial_args(Bindings, Rest, NextVars, NextArgs).
make_partial_args(Bindings, [Arg | Rest], Vars, [literal(Result) | NextArgs]) :-
    xpath_eval_seq(Bindings, Arg, Result), !,
    make_partial_args(Bindings, Rest, Vars, NextArgs).

xpath_apply(Bindings, Function, Arguments, Result) :- 
    memberchk('?', Arguments), !,
    make_partial_application(Bindings, Function,  Arguments, Result).

xpath_apply(Bindings, function(Name), Arguments, Result) :- !,
    maplist(xpath_eval_seq(Bindings), Arguments, EArguments),
    xpath_function(Name, Bindings, EArguments, Result).

xpath_apply(Bindings, function(Params, _, Bindings, Body), Arguments, Result) :-
    maplist(xpath_eval_seq(Bindings), Arguments, EArguments), 
    maplist(make_binding, Params, EArguments, ArgBindings),
    append(ArgBindings, Bindings, NewBindings),
    xpath_eval(NewBindings, Body, Result).

xpath_apply(Bindings, curry(Vars, Body), Arguments, Result) :-
    copy_term(Vars:Body, Vars0:Body0),
    maplist(xpath_eval_seq(Bindings), Arguments, EArguments),
    Vars0 = EArguments,
    xpath_eval(Bindings, Body0, Result).

xpath_apply(Bindings, map(Map), [KE], Result) :-
    xpath_eval_atomic(Bindings, KE, Key),
    memberchk(Key = VL, Map),
    xpath_const(VL, Result).

xpath_apply(Bindings, array(L), [IE], Result) :-
    xpath_eval_number(Bindings, IE, Num),
    Idx is integer(Num),
    nth1(Idx, L, Item),
    xpath_const(Item, Result).

new_focus(Bindings, Item, Pos, Size,
          [focus(item) = Item, focus(pos) = Pos, focus(size) = Size | Bindings]).

process_sequence(Bindings, V, Generator, NBindings) :-
    freeze(SizeL, (aggregate_all(count, Generator, Size), SizeL = [Size])),
    enumerate(Generator, I),
    new_focus(Bindings, [V], [I], SizeL, NBindings).

xpath_filter_boolean(Bindings, N) :-
    number(N), !, memberchk(focus(pos) = Pos, Bindings), N =:= Pos.
xpath_filter_boolean(_, N) :- xpath_boolean(N, true).

xpath_boolean(bool(X), X) :- !.
xpath_boolean(S, B) :- atom(S), !, (S == '' -> B = false; B = true).
xpath_boolean(N, B) :- number(N), !, ((N =:= 0; N =\= N) -> B = false; B = true).
xpath_boolean(X, true) :- xml_is_node(X).

xpath_atomize(bool(X), bool(X)) :- !.
xpath_atomize(X, X) :- atom(X), !.
xpath_atomize(X, X) :- number(X), !.
xpath_atomize(array(L), Y) :- !, member(X, L), xpath_const(X, X0), xpath_atomize(X0, Y).
xpath_atomize(X, Y) :- xml_is_node(X), typed_value(X, Y).

typed_value(X, Y) :- xml_node_schema_type(X, T), xml_node_string_value(X, S), do_cast(T, S, Y), !.
typed_value(X, Y) :- xml_node_string_value(X, Y).

xpath_number(X, X) :- number(X), !.
xpath_number(X, Y) :- atom(X), (atom_number(X, Y); Y is nan), !.
xpath_number(bool(true), 1).
xpath_number(bool(false), 0).

xpath_string(X, X) :- atom(X), !.
xpath_string(X, Y) :- number(X), atom_number(Y, X), !.
xpath_string(bool(X), X).

xpath_eval_atomic(Context, Expr, Y) :-
    strictly_once((xpath_eval(Context, Expr, X),
                   xpath_atomize(X, Y))).
    
xpath_eval_number(Context, Expr, Num) :-
    xpath_eval_atomic(Context, Expr, X), xpath_number(X, Num).

xpath_eval_string(Context, Expr, Str) :-
    xpath_eval_scalar(Context, Expr, X),
    xpath_string(X, Str).

xpath_comparison(eq, bool(X), bool(Y)) :- !, X == Y.
xpath_comparison(eq, N1, N2) :- number(N1), number(N2), !, N1 =:= N2.
xpath_comparison(eq, S1, S2) :- atom(S1), atom(S2), !, S1 == S2.
xpath_comparison(ne, bool(X), bool(Y)) :- !, X \== Y.
xpath_comparison(ne, N1, N2) :- number(N1), number(N2), !, N1 =\= N2.
xpath_comparison(ne, S1, S2) :- atom(S1), atom(S2), !, S1 == S2.
xpath_comparison(lt, bool(false), bool(true)) :- !.
xpath_comparison(lt, N1, N2) :- number(N1), number(N2), !, N1 < N2.
xpath_comparison(lt, S1, S2) :- atom(S1), atom(S2), !, S1 @< S2.
xpath_comparison(le, bool(false), bool(_)) :- !.
xpath_comparison(le, N1, N2) :- number(N1), number(N2), !, N1 =< N2.
xpath_comparison(le, S1, S2) :- atom(S1), atom(S2), !, S1 @=< S2.
xpath_comparison(gt, bool(true), bool(false)) :- !.
xpath_comparison(gt, N1, N2) :- number(N1), number(N2), !, N1 > N2.
xpath_comparison(gt, S1, S2) :- atom(S1), atom(S2), !, S1 @> S2.
xpath_comparison(ge, bool(true), bool(_)) :- !.
xpath_comparison(ge, N1, N2) :- number(N1), number(N2), !, N1 >= N2.
xpath_comparison(ge, S1, S2) :- atom(S1), atom(S2), !, S1 @>= S2.

xpath_instance_of(null, []).
xpath_instance_of(single(T), [X]) :- item_instance_of(T, X).
xpath_instance_of(optional(_), []).
xpath_instance_of(optional(T), [X]) :- item_instance_of(T, X).
xpath_instance_of(star(T), L) :- maplist(item_instance_of(T), L).
xpath_instance_of(plus(T), L) :- L \== [], maplist(item_instance_of(T), L).

item_instance_of(true, _).
item_instance_of(atomic(AT), X) :- xpath_atomic_instance_of(AT, X).
item_instance_of(node(NT), X) :- xml_is_node(X), test_node(child, NT, X).
item_instance_of(map(KT, VT), map(L)) :- maplist(check_map_type(KT, VT), L).
item_instance_of(array(T), array(L)) :- maplist(xpath_instance_of(T), L).
item_instance_of(function(ATS, RT), function(PS, RT, _, _)) :-
    maplist(check_arg_type, ATS, PS).
item_instance_of(X, Y) :- item_direct_subtype_of(T, X), item_instance_of(T, Y), !.

check_map_type(KT, VT, K = V) :- xpath_atomic_instance_of(KT, K), xpath_instance_of(VT, V).

check_arg_type(T, _:T).

xpath_subtype_of(null, null).
xpath_subtype_of(null, optional(_)).
xpath_subtype_of(null, star(_)).
xpath_subtype_of(optional(T1), optional(T2)) :- item_subtype_of(T1, T2).
xpath_subtype_of(optional(T1), star(T2)) :- item_subtype_of(T1, T2).
xpath_subtype_of(star(T1), star(T2)) :- item_subtype_of(T1, T2).
xpath_subtype_of(single(T1), optional(T2)) :- item_subtype_of(T1, T2).
xpath_subtype_of(single(T1), star(T2)) :- item_subtype_of(T1, T2).
xpath_subtype_of(single(T1), plus(T2)) :- item_subtype_of(T1, T2).
xpath_subtype_of(single(T1), single(T2)) :- item_subtype_of(T1, T2).
xpath_subtype_of(plus(T1), star(T2)) :- item_subtype_of(T1, T2).
xpath_subtype_of(plus(T1), plus(T2)) :- item_subtype_of(T1, T2).

xpath_strict_subtype(X, Y) :- xpath_subtype_of(X, Y), X \== Y.

item_subtype_of(X, X).
item_subtype_of(X, Y) :- item_direct_subtype_of(X, Z), item_subtype_of(Z, Y).

item_direct_subtype_of(T, true) :- T \== true.
item_direct_subtype_of(atomic(AT1), atomic(AT2)) :- atomic_subtype_of(AT1, AT2).
item_direct_subtype_of(node(NT1), node(NT2)) :- node_subtype_of(NT1, NT2).
item_direct_subtype_of(array, function([atomic('http://www.w3.org/2001/XMLSchema':integer)], star(true))).
item_direct_subtype_of(array(_), array).
item_direct_subtype_of(array(T1), array(T2)) :- xpath_strict_subtype_of(T1, T2).
item_direct_subtype_of(array(T1), function([atomic('http://www.w3.org/2001/XMLSchema':integer)], T1)).
item_direct_subtype_of(map, function([atomic('http://www.w3.org/2001/XMLSchema':anyAtomicType)], star(true))).
item_direct_subtype_of(map(_, _), map).
item_direct_subtype_of(map(KT1, VT1), map(KT2, VT2)) :- atomic_subtype_of(KT1, KT2), xpath_strict_subtype_of(VT1, VT2).
item_direct_subtype_of(map(KT, VT), function([atomic(KT)], VT)).
item_direct_subtype_of(function(_, _), function).
item_direct_subtype_of(function(ATS1, RT1), function(ATS2, RT2)) :- maplist(xpath_strict_subtype_of, ATS2, ATS1), xpath_strict_subtype_of(RT1, RT2).

atomic_subtype_of(AT1, AT2) :- (xpath_atomic_subtype_of(AT1, AT2) *-> true; AT2 = 'http://www.w3.org/2001/XMLSchema':anyAtomicType), AT1 \== AT2.

node_subtype_of(T, true) :- T \== true.
node_subtype_of(pi(T1), pi(T2)) :- node_subtype_of(T1, T2).
node_subtype_of(document(T1), document(T2)) :- node_subtype_of(T1, T2).
node_subtype_of(element(N), element(*)) :- N \== '*'.
node_subtype_of(attribute(N), attribute(*)) :- N \== '*'.
node_subtype_of(element(N, T1, Nillable), element(N, T2, Nillable)) :- atomic_subtype_of(T1, T2).
node_subtype_of(element(N, T, false), element(N, T, true)).
node_subtype_of(element(N, _, _), element(N)).
node_subtype_of(element(N, T, Nillable), element(*, T, Nillable)) :- N \== '*'.
node_subtype_of(attribute(N), attribute(*)) :- N \== '*'.
node_subtype_of(attribute(N, _), attribute(N)).
node_subtype_of(attribute(N, T), attribute(*, T)) :- N \== '*'.
node_subtype_of(attribute(N, T1), attribute(N, T2)) :- atomic_subtype_of(T1, T2).

xpath_atomic_instance_of('http://www.w3.org/2001/XMLSchema':boolean, bool(_)).
xpath_atomic_instance_of('http://www.w3.org/2001/XMLSchema':string, X) :- atom(X).
xpath_atomic_instance_of('http://www.w3.org/2001/XMLSchema':numeric, X) :- number(X).
xpath_atomic_instance_of('http://www.w3.org/2001/XMLSchema':decimal, X) :- number(X), is_finite(X).
xpath_atomic_instance_of('http://www.w3.org/2001/XMLSchema':integer, X) :- integer(X).
xpath_atomic_instance_of('http://www.w3.org/2001/XMLSchema':float, X) :- float(X), float_range(X, 3.402823466E38, 1.175494351E-38).
xpath_atomic_instance_of('http://www.w3.org/2001/XMLSchema':double, X) :- float(X), float_range(X, 1.7976931348623157E08, 2.2250738585072014E-308).
xpath_atomic_instance_of('http://www.w3.org/2001/XMLSchema':'QName', NS:NCName) :- atom(NS), atom(NCName).
xpath_atomic_instance_of('http://www.w3.org/2001/XMLSchema':'QName', NCName) :- atom(NCName), atom_codes(NCName, CL), phrase(ncname(_), CL).
xpath_atomic_instance_of('http://www.w3.org/2001/XMLSchema':anyURI, URI) :- atom(URI).
xpath_atomic_instance_of('http://www.w3.org/2001/XMLSchema':hexBinary, URI) :- atom(URI).
xpath_atomic_instance_of('http://www.w3.org/2001/XMLSchema':base64Binary, URI) :- atom(URI).

xpath_atomic_subtype_of('http://www.w3.org/2001/XMLSchema':decimal, 'http://www.w3.org/2001/XMLSchema':numeric).
xpath_atomic_subtype_of('http://www.w3.org/2001/XMLSchema':integer, 'http://www.w3.org/2001/XMLSchema':decimal).
xpath_atomic_subtype_of('http://www.w3.org/2001/XMLSchema':float, 'http://www.w3.org/2001/XMLSchema':numeric).
xpath_atomic_subtype_of('http://www.w3.org/2001/XMLSchema':double, 'http://www.w3.org/2001/XMLSchema':numeric).

xpath_atomic_cast('http://www.w3.org/2001/XMLSchema':untypedAtomic, X, Y) :- xpath_atomic_cast('http://www.w3.org/2001/XMLSchema':string, X, Y), !.

xpath_atomic_cast('http://www.w3.org/2001/XMLSchema':string, X, S) :- xpath_string(X, S).

xpath_atomic_cast('http://www.w3.org/2001/XMLSchema':numeric, X, N) :- xpath_number(X, N).
xpath_atomic_cast('http://www.w3.org/2001/XMLSchema':integer, X, I) :- xpath_number(X, N), I is integer(N).
xpath_atomic_cast('http://www.w3.org/2001/XMLSchema':float, X, F) :- xpath_number(X, N), F is float(N).
xpath_atomic_cast('http://www.w3.org/2001/XMLSchema':double, X, F) :- xpath_number(X, N), !, F is float(N).

xpath_atomic_cast('http://www.w3.org/2001/XMLSchema':boolean, N, bool(B)) :- number(N), xpath_boolean(N, B).
xpath_atomic_cast('http://www.w3.org/2001/XMLSchema':boolean, true, bool(true)).
xpath_atomic_cast('http://www.w3.org/2001/XMLSchema':boolean, false, bool(false)).

is_finite(X) :- X =:= X, X =\= inf, X =\= -inf.

float_range(X, _, _) :- \+ is_finite(X), !.
float_range(X, Max, Min) :- XA is abs(X), XA =< Max, XA >= Min.

xpath_function('http://www.w3.org/2001/XMLSchema':T, _, [[X]], Y) :- do_cast('http://www.w3.org/2001/XMLSchema':T, X, Y).
xpath_function('http://www.w3.org/2001/XMLSchema':_, _, [[]], _) :- !, fail.

xpath_function('http://www.w3.org/2005/xpath-functions':'node-name', Context, [], X) :-
    memberchk(focus(item) = [Item], Context), xml_node_qname(Item, X).
xpath_function('http://www.w3.org/2005/xpath-functions':'node-name', _, [[N]], X) :- xml_node_qname(N, X).

xpath_function('http://www.w3.org/2005/xpath-functions':nilled, Context, [], bool(X)) :-
    memberchk(focus(item) = [Item], Context),
    (xml_node_schema_nilled(Item) -> X = true; X = false).
xpath_function('http://www.w3.org/2005/xpath-functions':nilled, _, [[N]], bool(X)) :- 
    xml_node_schema_nilled(N) -> X = true; X = false.    

xpath_function('http://www.w3.org/2005/xpath-functions':string, Context, [], X) :-
    memberchk(focus(item) = [Item], Context),
    xpath_function('http://www.w3.org/2005/xpath-functions':string, Context, [[Item]], X).

xpath_function('http://www.w3.org/2005/xpath-functions':string, _, [[N]], X) :- xml_is_node(N), !, xml_node_string_value(N, X).
xpath_function('http://www.w3.org/2005/xpath-functions':string, _, [[V]], X) :- xpath_string(V, X).

xpath_function('http://www.w3.org/2005/xpath-functions':data, _, [], X) :-
    memberchk(focus(item) = [Item], Context),
    xpath_function('http://www.w3.org/2005/xpath-functions':data, Context, [[Item]], X).

xpath_function('http://www.w3.org/2005/xpath-functions':data, _, [L], X) :- get_data(L, X).

get_data([H|_], X) :- get_one_data(H, X).
get_data([_|T], X) :- get_data(T, X).

get_one_data(H, X) :- xml_is_node(H), !, typed_value(H, X).
get_one_data(array(L), X) :- !, get_data(L, X).
get_one_data(X, X).

traverse_axis(child, Node, NewNode) :- xml_node_child(Node, NewNode).  traverse_axis(parent, Node, NewNode) :-
    xml_node_parent(Node, NewNode).
traverse_axis(ancestor, Node, NewNode) :- xml_node_ancestor(Node, NewNode).
traverse_axis(ancestor_or_self, Node, NewNode) :- xml_node_ancestor_or_self(Node, NewNode).
traverse_axis(descendant, Node, NewNode) :- xml_node_descendant(Node, NewNode).
traverse_axis(descendant_or_self, Node, NewNode) :- xml_node_descendant_or_self(Node, NewNode).
traverse_axis(following_sibling, Node, NewNode) :- xml_node_following_sibling(Node, NewNode).
traverse_axis(following, Node, NewNode) :- xml_node_following_sibling(Node, NewNode).
traverse_axis(preceding_sibling, Node, NewNode) :- xml_node_preceding_sibling(Node, NewNode).
traverse_axis(preceding, Node, NewNode) :- xml_node_preceding(Node, NewNode).
traverse_axis(attribute, Node, NewNode) :- xml_node_attr(Node, NewNode).
traverse_axis(namespace, Node, NewNode) :- xml_node_nsdecl(Node, NewNode).
traverse_axis(self, Node, Node).

is_principal_node(attribute, N) :- xml_node_type(N, attribute), !.
is_principal_node(namespace, N) :- xml_node_type(N, namespace), !.
is_principal_node(_, N) :- xml_node_type(N, element).

test_node(_, true, _) :- !.
test_node(_, node, N) :- xml_is_node(N).
test_node(Axis, name(QName), N) :-
    is_principal_node(Axis, N),
    xml_node_qname(N, QName).
test_node(Axis, namespace(NS), N) :-
    is_principal_node(Axis, N),
    xpath_static_nsdecl(NS, NSURI),
    xml_node_namespace(N, NSURI).
test_node(Axis, namespace_uri(NSURI), N) :-
    is_principal_node(Axis, N),
    xml_node_namespace(N, NSURI).
test_node(Axis, ncname(NCName), N) :-
    is_principal_node(Axis, N),
    xml_node_name(N, NCName).
test_node(Axis, *, N) :- is_principal_node(Axis, N).
test_node(_, text, N) :- xml_node_type(N, text).
test_node(_, comment, N) :- xml_node_type(N, comment).
test_node(_, document(T), N) :- xml_node_type(N, document),
    xml_node_child(N, Top), test_node(child, Top, T), !.
test_node(_, namespace, N) :- xml_node_type(N, namespace).
test_node(_, element(*), N) :- !, xml_node_type(N, element).
test_node(_, element(QName), N) :-
    xml_node_type(N, element), xml_node_qname(N, QName).
test_node(_, element(*, Type, Nillable), N) :- !,
    xml_node_type(N, element), is_nillable_type(N, Type, Nillable).
test_node(_, element(QName, Type, Nillable), N) :-
    xml_node_type(N, element), xml_node_qname(N, QName), is_nillable_type(N, Type, Nillable).
test_node(_, attribute(*), N) :- !, xml_node_type(N, attribute).
test_node(_, attribute(QName), N) :-
    xml_node_type(N, attribute), xml_node_qname(N, QName).
test_node(_, attribute(*, Type), N) :-
    xml_node_type(N, attribute), xml_node_schema_type(N, Type).
test_node(_, attribute(QName, Type), N) :-
    xml_node_type(N, attribute), xml_node_qname(N, QName), xml_node_schema_type(N, Type).
test_node(_, pi(T), N) :- xml_node_type(N, pi), test_node(child, T, N).

is_nillable_type(N, _, true) :- !, xml_node_schema_nilled(N).
is_nillable_type(N, T, true) :- !, xml_node_schema_type(N, T).
is_nillable_type(N, T, false) :- xml_node_schema_type(N, T), \+ xmk_node_schema_nilled(N).

xpath_const([], _) :- !, fail.
xpath_const(X, Y) :- X = [_|_], !, member(Y, X).
xpath_const(X, X).

xpath_eval_scalar(Bindings, Expr, Result) :- strictly_once(xpath_eval(Bindings, Expr, Result)).

xpath_eval_seq(Bindings, Expr, Seq) :- findall(X, xpath_eval(Bindings, Expr, X), Seq).

xpath_eval(_, literal(X), Y) :- xpath_const(X, Y).
xpath_eval(Bindings, var(Name), Y) :- memberchk(Name = Value, Bindings), xpath_const(Value, Y).
xpath_eval(Bindings, '.', Item) :- memberchk(focus(item) = [Item], Bindings).
xpath_eval(Bindings, call(Name, Arguments), Result) :-
    xpath_apply(Bindings, function(Name), Arguments, Result).

xpath_eval(_, #(Name, _), function(Name)).

xpath_eval(Bindings, function(L, T, B), function(L, T, Bindings, B)).

xpath_eval(Bindings, filter(Expr, Pred), Result) :-
    process_sequence(Bindings, Result, xpath_eval(Bindings, Expr, Result), NBindings),
    xpath_eval_scalar(NBindings, Pred, B),
    xpath_filter_boolean(NBindings, B).

xpath_eval(Bindings, apply(FuncExpr, Arguments), Result) :-
    xpath_eval_scalar(Bindings, FuncExpr, Function),
    xpath_apply(Bindings, Function, Arguments, Result).

xpath_eval(Bindings, sort(Expr), Result) :-
    setof(X, xpath_eval(Bindings, Expr, X), L),
    member(Result, L).

xpath_eval(Bindings, /(Base, Step), Result) :-
    process_sequence(Bindings, X, xpath_eval(Bindings, Base, X), NBindings),
    xpath_eval(NBindings, Step, Result).

xpath_eval(Bindings, /, Result) :-
    memberchk(focus(item) = [Item], Bindings),
    xml_node_root(Item, Result).

xpath_eval(Bindings, /(Step), Result) :-
    memberchk(focus(item) = [Item], Bindings),
    xml_node_root(Item, Root),
    new_focus(Bindings, [Root], [1], [1], NBindings),
    xpath_eval(NBindings, Step, Result).

xpath_eval(Bindings, step(Axis, NodeTest), X) :-
    memberchk(focus(item) = [Item], Bindings),
    traverse_axis(Axis, Item, X),
    test_node(Axis, NodeTest, X).

xpath_eval(Bindings, (X1, _), Result) :- xpath_eval(Bindings, X1, Result).
xpath_eval(Bindings, (_, X2), Result) :- xpath_eval(Bindings, X2, Result).

xpath_eval(Bindings, range(X1, X2), Result) :-
    xpath_eval_number(Bindings, X1, N1),
    xpath_eval_number(Bindings, X2, N2),
    I1 is integer(N1), I2 is integer(N2),
    between(I1, I2, Result).

xpath_eval(Bindings, union(X1, X2), Result) :-
    xpath_eval_seq(Bindings, X1, R1), xpath_eval_seq(Bindings, X2, R2),
    ord_union(R1, R2, RU), member(Result, RU).

xpath_eval(Context, intersect(X1, X2), Result) :-
    xpath_eval_seq(Context, X1, R1), xpath_eval(Context, X2, R2),
    ord_intresect(R1, R2, RI), member(Result, RI).

xpath_eval(Context, except(X1, X2), Result) :-
    xpath_eval_seq(Context, X1, R1), xpath_eval(Context, X2, R2),
    ord_subtract(R1, R2, RI), member(Result, RI).

xpath_eval(Context, Expr1 + Expr2, Result) :-
    xpath_eval_number(Context, Expr1, R1),
    xpath_eval_number(Context, Expr2, R2),
    Result is R1 + R2.

xpath_eval(Context, Expr1 - Expr2, Result) :-
    xpath_eval_number(Context, Expr1, R1),
    xpath_eval_number(Context, Expr2, R2),
    Result is R1 - R2.

xpath_eval(Context, Expr1 * Expr2, Result) :-
    xpath_eval_number(Context, Expr1, R1),
    xpath_eval_number(Context, Expr2, R2),
    Result is R1 * R2.

xpath_eval(Context, Expr1 div Expr2, Result) :-
    xpath_eval_number(Context, Expr1, R1),
    xpath_eval_number(Context, Expr2, R2),
    Result is R1 / R2.

xpath_eval(Context, idiv(Expr1, Expr2), Result) :-
    xpath_eval_number(Context, Expr1, R1),
    xpath_eval_number(Context, Expr2, R2),
    Result is R1 div R2.

xpath_eval(Context, Expr1 mod Expr2, Result) :-
    xpath_eval_number(Context, Expr1, R1),
    xpath_eval_number(Context, Expr2, R2),
    Result is R1 mod R2.

xpath_eval(Context, +Expr, Result) :-
    xpath_eval_number(Context, Expr, Result).

xpath_eval(Context, -Expr, Result) :-
    xpath_eval_number(Context, Expr, R),
    Result is -R.

xpath_eval(Context, '||'(Expr1, Expr2), Result) :-
    xpath_eval_string(Context, Expr1, R1),
    xpath_eval_string(Context, Expr2, R2),
    atom_concat(R1, R2, Result).

xpath_eval(Context, compare(Op, Expr1, Expr2), bool(Result)) :- memberchk(Op, [eq, ne, lt, gt, le, ge]), !,
    xpath_eval_atomic(Context, Expr1, R1),
    xpath_eval_atomic(Context, Expr2, R2),
    (xpath_comparison(Op, R1, R2) -> Result = true; Result = false).

xpath_eval(Context, compare(Op, Expr1, Expr2), bool(Result)) :-
    memberchk(Op = AOp, [(=) = eq, ('!=') = ne, (<) = lt, (>) = gt, (<=) = le, (>=) = ge]), !,
    (xpath_eval(Context, Expr1, R1), xpath_eval(Context, Expr2, R2),
     xpath_atomize(R1, A1), xpath_atomize(R2, A2),
     xpath_comparison(AOp, A1, A2) -> Result = true; Result = false).

xpath_eval(Context, compare(is, Expr1, Expr2), bool(Result)) :- 
    xpath_eval_scalar(Context, Expr1, R1),
    xpath_eval_scalar(Context, Expr2, R2),
    (R1 == R2 -> Result = true; Result = false).

xpath_eval(Context, compare(<<, Expr1, Expr2), bool(Result)) :-
    xpath_eval_scalar(Context, Expr1, R1),
    xpath_eval_scalar(Context, Expr2, R2),
    (R1 @< R2 -> Result = true; Result = false).

xpath_eval(Context, compare(>>, Expr1, Expr2), bool(Result)) :-
    xpath_eval_scalar(Context, Expr1, R1),
    xpath_eval_scalar(Context, Expr2, R2),
    (R1 @> R2 -> Result = true; Result = false).

xpath_eval(Context, and(Expr1, Expr2), bool(Result)) :-
    xpath_eval_scalar(Context, Expr1, R1),
    xpath_boolean(R1, B1),
    (B1 = false -> Result = false;
     xpath_eval_scalar(Context, Expr2, R2),
     xpath_boolean(R2, Result)).

xpath_eval(Context, or(Expr1, Expr2), bool(Result)) :-
    xpath_eval_scalar(Context, Expr1, R1),
    xpath_boolean(R1, B1),
    (B1 = true -> Result = true;
     xpath_eval_scalar(Context, Expr2, R2),
     xpath_boolean(R2, Result)).

xpath_eval(Context, for(NBindings, X), Result) :-
    eval_for(Context, NBindings, X, Result).

xpath_eval(Context, let([], X), Result) :-
    xpath_eval(Context, X, Result).

xpath_eval(Context, let([Var = H | T], X), Result) :-
    xpath_eval_seq(Context, H, V),
    xpath_eval([Var = V | Context], let(T, X), Result).

xpath_eval(Context, map(Bindings), map(Result)) :-
    maplist(eval_map(Context), Bindings, Result).

xpath_eval(Context, array({Expr}), array(L)) :- !,
    findall([X], xpath_eval(Context, Expr, X), L).

xpath_eval(Context, array(EL), array(L)) :-
    findall(X, (member(E, EL), xpath_eval_seq(Context, E, X)), L).

xpath_eval(Context, lookup(E, *), Result) :- !,
    xpath_eval_scalar(Context, E, Base),
    all_values(Base, Result).

xpath_eval(Context, lookup(E1, E2), Result) :-
    xpath_eval_scalar(Context, E1, Base),
    xpath_eval(Context, E2, Idx),
    do_lookup(Base, Idx, Result).

xpath_eval(Context, if(Cond, Then, Else), Result) :-
    xpath_eval_scalar(Context, Cond, X),
    xpath_boolean(X, true) ->
    xpath_eval(Context, Then, Result);
    xpath_eval(Context, Else, Result).

xpath_eval(Context, quantified(some, L, X), bool(Result)) :-
    eval_some(Context, L, X) -> Result = true; Result = false.

xpath_eval(Context, quantified(every, L, X), bool(Result)) :-
    eval_every(Context, L, X) -> Result = true; Result = false.

xpath_eval(Context, instanceof(Expr, Type), bool(Result)) :-
    xpath_eval_seq(Context, Expr, L),
    (xpath_instance_of(Type, L) -> Result = true; Result = false).

xpath_eval(Context, cast(Expr, Type, Empty), Result) :-
    xpath_eval_atomic(Context, Expr, V) ->
    do_cast(Type, V, Result);
    (Empty = true -> fail; type_error(nonempty, Expr)).

xpath_eval(Context, castable(Expr, Type, Empty), bool(Result)) :-
    xpath_eval_atomic(Context, Expr, V) ->
    (do_cast0(Type, V, _) -> Result = true; Result = false);
    Result = Empty.

xpath_eval(Context, treat(Expr, Type), Result) :-
    xpath_eval_seq(Context, Expr, L),
    (xpath_instance_of(Type, L) -> member(Result, L);
     type_error(instanceof(Type), L)).

all_values(map(L), Result) :- member(_ = V, L), xpath_const(V, Result).
all_values(array(L), Result) :- member(V, L), xpath_const(V, Result).

do_lookup(map(L), Idx, Result) :- member(Idx = V, L), xpath_const(V, Result).
do_lookup(array(L), Idx, Result) :- I is integer(Idx), nth1(I, L, V), xpath_const(V, Result).

eval_for(Context, [], X, Result) :-
    xpath_eval(Context, X, Result).

eval_for(Context, [Var = H|T], X, Result) :-
    xpath_eval(Context, H, V),
    eval_for([Var = [V] | Context], T, X, Result).

eval_map(Context, NE = VE, Name = Value) :-
    xpath_eval_atomic(Context, NE, Name),
    xpath_eval_seq(Context, VE, Value).

eval_some(Context, [], X) :- xpath_eval_scalar(Context, X, Y), xpath_boolean(Y, true).
eval_some(Context, [Var = H | T], X) :-
    xpath_eval(Context, H, V),
    eval_some([Var = [V] | Context], T, X).

eval_every(Context, [], X) :- xpath_eval_scalar(Context, X, Y), xpath_boolean(Y, true).
eval_every(Context, [Var = H | T], X) :-
    forall(xpath_eval(Context, H, V), eval_every([Var = [V] | Context], T, X)).

do_cast0(T, X, X) :- xpath_atomic_instance_of(T, X), !.
do_cast0(T, X, Y) :- xpath_atomic_cast(T, X, Y), !.
do_cast0(T, X, Y) :- atomic_subtype_of(T, ST), do_cast0(ST, X, Y).

do_cast(T, X, Y) :- do_cast0(T, X, Y), !.
do_cast(T, X, _) :- type_error(castable(T), X).

xpath_evaluate(CtxItem, Expr, Result) :-
    (var(CtxItem) -> Bindings = []; new_focus([], [CtxItem], [1], [1], Bindings)),
    atom_codes(Expr, ExprL),
    phrase(xpath(XPath), ExprL),
    xpath_eval(Bindings, XPath, Result).