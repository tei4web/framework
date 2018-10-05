:- module(dom, []).
:- use_module(library(sgml)).

:- dynamic dom_node/4.
:- initializiation(nb_setval(next_node, 1)).

is_ns_attr(xmlns).
is_ns_attr(xmlns:_).

dom_node_type(ref(X), Type) :- dom_node(X, Type, _,  _).
dom_node_type(element(_, _, _), element).
dom_node_type(pi(_), pi).
dom_node_type(comment(_), comment).
dom_node_type(X, text) :- atom(X).
dom_node_type(Name = _, Type) :-
    is_ns_attr(Name) -> Type = ns; Type = attribute.
dom_node_type(xml(_), document).

dom_node_name(ref(X), Name) :- dom_node(X, element, Name, _).
dom_node_name(ref(X), Name) :- dom_node(X, Type, Name = _, _),
    (Type = attribute; Type = ns).
dom_node_name(element(Name, _, _), Name).
dom_node_name(AttrName = _, Name) :-
    AttrName = xmlns:Name -> true;
    AttrName = xmlns -> Name = '';
    Name = AttrName.

dom_node_value(ref(X), Value) :-
    dom_node(X, Type, Value, _),
    (Type = text; Type = pi; Type = comment).
dom_node_value(ref(X), Value) :-
    dom_node(X, Type, _ = Value, _),
    (Type = attribute; Type = ns).
dom_node_value(X, X) :- atom(X).
dom_node_value(pi(X), X).
dom_node_value(comment(X), X).
dom_node_value(_ = Value, Value).

dom_node_children(ref(X), [First|Rest]) :-
    dom_node(X, element, _, Nav), !, First = ref(Nav.child),
    dom_node_next_siblings(First, Rest).
dom_node_children(element(_, _, Children), _) :- !.
dom_node_children(_, []).

dom_node_next_siblings(X, [First|Rest]) :-
    dom_node_next(X, First), !,
    dom_node_next_siblings(First, Rest).
dom_node_next_siblings(_, []).

dom_node_next(ref(X), Next) :-
    dom_node(X, _, _, Nav), Next = ref(Nav.next).

dom_node_parent(ref(X), Parent) :-
    dom_node(X, _, _, Nav), Parent = ref(Nav.parent).

is_ns_decl(Name = _) :- is_ns_attr(Name).

dom_node_attributes(ref(X), [First|Rest]) :-
    dom_node(X, element, _, Nav), !, First = ref(Nav.attrs),
    dom_node_next_siblings(First, Rest).
dom_node_attributes(element(_, Attrs, _), Attrs0) :-
    exclude(is_ns_decl, Attrs, Attrs0).


dom_retrieve_node(X, Node) :-
    dom_node(X, Type, Value, _), !,
    (Type = text -> Node = Value;
     Type = element -> Node = element(Value, Attrs, Children),

    dom_retrieve_node(X, X).







