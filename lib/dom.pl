:- module(dom, [xml_is_node/1, xml_node_asserted/1, xml_node_type/2, xml_node_name/2, xml_node_ns/2, xml_node_qname/2,
                xml_node_child/2, xml_node_nth_child/3, xml_node_attr/2,
                xml_node_value/2, xml_node_string_value/2, xml_node_nsdecl/2,
                xml_node_parent/2, xml_node_root/2,
                xml_node_descendant/2, xml_node_descendant_or_self/2,
                xml_node_ancestor/2, xml_node_ancestor_or_self/2,
                xml_node_following_sibling/2, xml_node_preceding_sibling/2,
                xml_node_following/2, xml_node_preceding/2, xml_get_node/2,
                xml_deep_get_node/2, xml_assert_node/2, xml_node_byid/3, xml_node_schema_type/2, xml_node_schema_nilled/1,
                xml_expand_qname/4, xml_node_base/2, xml_node_lang/2, xml_node_document_uri/2,
                xml_load_document/3]).
:- use_module(library(sgml)).
:- use_module(library(lists)).
:- use_module(library(uri)).
:- use_module(library(ebnf)).

:- dynamic dom_node/4.
:- dynamic dom_child/3.
:- dynamic dom_attrib/2.
:- dynamic dom_nsdecl/2.
:- dynamic dom_rightmost/2.
:- dynamic dom_loaded/3.

:- initialization nb_setval(dom_next_node_id, 1).

rbetween(X, Y, Z) :-
    NX is -X,
    NY is -Y,
    between(NY, NX, NZ),
    Z is -NZ.

xml_node_asserted(ref(_)).

xml_is_node(ref(N)) :- dom_node(N, _, _, _), !.
xml_is_node(element(_, _, _)).
xml_is_node(xml(_)).
xml_is_node(pi(_)).
xml_is_node(comment(_)).
% inline text is intentionally not a node
xml_is_node(_ = _).

xml_node_type(V, X) :- var(V), !, V = ref(Id), dom_node(Id, X, _, _).
xml_node_type(ref(Id), Type) :- dom_node(Id, Type, _, _).
xml_node_type(element(_, _, _), element).
xml_node_type(Text, text) :- atom(Text).
xml_node_type(pi(_), pi).
xml_node_type(comment(_), comment).
xml_node_type(Name = _, Type) :- (Name = xmlns; Name = xmlns:_) -> Type = namespace; Type = attribute.
xml_node_type(xml(_), document).

xml_node_qname(V, X) :- var(V), !, V = ref(Id), dom_node(Id, _, X, _).
xml_node_qname(ref(Id), Name) :- dom_node(Id, _, Name, _).
xml_node_qname(element(Name, _, _), Name).
xml_node_qname(xmlns:Name = _, Name) :- !.
xml_node_qname(xmlns = _, '') :- !.
xml_node_qname(pi(PI), Name) :- sub_atom(PI, Prefix, 1, _, C), char_type(C, space), sub_atom(PI, 0, Prefix, _, Name).
xml_node_qname(Name = _, Name).

xml_node_name(X, Y) :- xml_node_qname(X, QName), (QName = _:Y -> true; Y = QName).

xml_node_ns(X, Y) :- xml_node_qname(X, Y:_).

xml_node_child(V, ref(X)) :- var(V), !, V = ref(Id), dom_child(Id, _, X).
xml_node_child(ref(Id), ref(Child)) :- dom_child(Id, _, Child).
xml_node_child(element(_, _, Children), Child) :- member(Child, Children).
xml_node_child(xml(Children), Child) :- member(Child, Children).

xml_node_nth_child(V, N, ref(X)) :- var(V), !, V = ref(Id), dom_child(Id, N, X).
xml_node_nth_child(ref(Id), N, ref(Child)) :- dom_child(Id, N, Child).
xml_node_nth_child(element(_, _, Children), N, Child) :- nth0(N, Children, Child).
xml_node_nth_child(xml(Children), N, Child) :- nth0(N, Children, Child).

xml_node_attr(V, ref(X)) :- var(V), !, V = ref(Id), dom_attrib(Id, X).
xml_node_attr(ref(Id), ref(Attr)) :- dom_attrib(Id, Attr).
xml_node_attr(element(_, Attrs, _), Attr) :- member(Attr, Attrs), Attr \= (xmlns = _), Attr \= (xmlns:_ = _).

xml_node_value(V, X) :- var(V), !, V = ref(Id), dom_node(Id, _, _, X), X \== [].
xml_node_value(ref(Id), Value) :- dom_node(Id, _, _, Value), Value \== [].
xml_node_value(Text, Text) :- atom(Text).
xml_node_value(_ = Value, Value).
xml_node_value(pi(PI), Value) :- sub_atom(PI, Prefix, 1, _, C), char_type(C, space), sub_atom(PI, Prefix, _, 0, Rest),
    normalize_space(atom(Value), Rest).

xml_node_string_value(Node, Text) :- xml_node_value(Node, Text), !.
xml_node_string_value(Node, Text) :- findall(ChText, (xml_node_child(Node, Child), xml_node_string_value(Child, ChText)), L), atomic_list_concat(L, Text), !.
xml_node_string_value(_, '').

xml_node_nsdecl(V, ref(X)) :- var(V), !, V = ref(Id), dom_nsdecl(Id, X).
xml_node_nsdecl(ref(Id), ref(Attr)) :- dom_nsdecl(Id, Attr).
xml_node_nsdecl(element(_, Attrs, _), Attr) :- member(Attr, Attrs), (Attr = (xmlns = _); Attr = (xmlns:_ = _)).

xml_node_parent(X, Y) :- xml_node_child(Y, X).
xml_node_parent(X, Y) :- xml_node_attr(Y, X).
xml_node_parent(X, Y) :- xml_node_nsdecl(Y, X).

xml_node_descendant(ref(X), ref(Y)) :- !, dom_rightmost(X, Last), X1 is X + 1, between(X1, Last, Y), dom_child(_, _, Y).
xml_node_descendant(X, Y) :- xml_node_child(X, C), xml_node_descendant_or_self(C, Y).

xml_node_descendant_or_self(X, X).
xml_node_descendant_or_self(X, Y) :- xml_node_descendant(X, Y).

xml_node_ancestor(X, Y) :- xml_node_parent(X, C), xml_node_ancestor_or_self(C, Y).

xml_node_ancestor_or_self(X, X).
xml_node_ancestor_or_self(X, Y) :- xml_node_ancestor(X, Y).

xml_node_following_sibling(X, Y) :- xml_node_nth_child(P, N, X), xml_node_nth_child(P, N1, Y), N1 > N.

xml_node_preceding_sibling(X, Y) :- xml_node_nth_child(P, N, X), N0 is N - 1, rbetween(0, N0, N1), xml_node_nth_child(P, N1, Y).

xml_node_root(X, Y) :- xml_node_ancestor_or_self(X, Y), xml_node_type(Y, document).

xml_node_following(ref(X), ref(Y)) :- xml_node_root(ref(X), ref(R)), dom_rightmost(R, Last),
    X1 is X + 1, between(X1, Last, Y), dom_child(_, _, Y).

xml_node_preceding(ref(X), ref(Y)) :-  xml_node_root(ref(X), ref(R)), X1 is X - 1, rbetween(R, X1, Y), dom_child(_, _, Y).

xml_mk_node(dom_node(Id, element, Name, _), element(Name, Attrs, ChildRef)) :-
    findall(Y, ((dom_attrib(Id, X); dom_nsdecl(Id, X)), xml_mk_node(X, Y)), Attrs),
    findall(ref(Y), dom_child(Id, _, Y), ChildRef).
xml_mk_node(dom_node(_, text, _, Value), Value).
xml_mk_node(dom_node(_, pi, Name, Value), pi(PI)) :- format(atom(PI), '~a ~a', [Name, Value]).
xml_mk_node(dom_node(_, comment, _, Value), comment(Value)).
xml_mk_node(dom_node(_, attribute, Name, Value), Name = Value).
xml_mk_node(dom_node(_, namespace, '', Value), xmlns = Value) :- !.
xml_mk_node(dom_node(_, namespace, Name, Value), xmlns:Name = Value).
xml_mk_node(dom_node(Id, document, _, _), xml(Children)) :-
    findall(ref(Y), dom_child(Id, _, Y), Children).

xml_get_node(ref(Id), E) :- !, dom_node(Id, Type, Name, Value),
    xml_mk_node(dom_node(Id, Type, Name, Value), E).
xml_get_node(X, X).

xml_deep_get_node(X, E1) :- xml_get_node(X, E0), xml_deepen(E0, E1).

xml_deepen(element(Name, Attrs, Children), element(Name, Attrs, ChildrenCopy)) :- !, maplist(xml_deep_get_node, Children, ChildrenCopy).
xml_deepen(xml(Children), xml(ChildrenCopy)) :- !, maplist(xml_deep_get_node, Children, ChildrenCopy).
xml_deepen(X, X).

:- multifile xml_index_hook/2.

xml_assert_node(Id, Node, Next) :-
    xml_add_node(Id, Node, Next),
    forall(xml_index_hook(Id, Node), true).

xml_add_node(Id, element(Name, Attrs, Children), Next) :-
    assertz(dom_node(Id, element, Name, [])),
    Id1 is Id + 1,
    xml_add_attrs(Id, Id1, Attrs, Id2),
    xml_add_children(Id, 0, Id2, Children, Next),
    Last is Next - 1,
    assertz(dom_rightmost(Id, Last)).

xml_add_node(Id, xml(Children), Next) :-
    assertz(dom_node(Id, document, '', [])),
    Id1 is Id + 1,
    xml_add_children(Id, 0, Id1, Children, Next),
    Last is Next - 1,
    assertz(dom_rightmost(Id, Last)).

xml_add_node(Id, Text, Next) :-
    atom(Text), !,
    assertz(dom_node(Id, text, '', Text)),
    Next is Id + 1.

xml_add_node(Id, pi(PI), Next) :-
    xml_node_name(pi(PI), Name),
    xml_node_value(pi(PI), Value),
    assertz(dom_node(Id, pi, Name, Value)),
    Next is Id + 1.

xml_add_node(Id, comment(Text), Next) :-
    assertz(dom_node(Id, comment, '', Text)),
    Next is Id + 1.

xml_add_node(Id, xmlns = URI, Next) :- !,
    assertz(dom_node(Id, namespace, '', URI)),
    Next is Id + 1.

xml_add_node(Id, xmlns:NS = URI, Next) :- !,
    assertz(dom_node(Id, namespace, NS, URI)),
    Next is Id + 1.

xml_add_node(Id, Name = Value, Next) :- !,
    assertz(dom_node(Id, attribute, Name, Value)),
    Next is Id + 1.

xml_add_children(_, _, Id, [], Id).
xml_add_children(Parent, N, Id, [H|T], Next) :-
    assertz(dom_child(Parent, N, Id)),
    xml_assert_node(Id, H, Id1),
    N1 is N + 1,
    xml_add_children(Parent, N1, Id1, T, Next).

xml_add_attrs(_, Id, [], Id).
xml_add_attrs(Parent, Id, [H|T], Next) :-
    ((H = (xmlns = _); H = (xmlns:_ = _)) ->
     assertz(dom_nsdecl(Parent, Id));
     assertz(dom_attrib(Parent, Id))),
    xml_assert_node(Id, H, Id0),
    xml_add_attrs(Parent, Id0, T, Next).

xml_assert_node(ref(Id), Id) :- !.
xml_assert_node(Node, Id) :-
    nb_getval(dom_next_node_id, Id),
    xml_assert_node(Id, Node, Next),
    nb_setval(dom_next_node_id, Next).

:- dynamic dom_id/2.
:- dynamic dom_schema_type/2.

xml_index_hook(Id, xml:id = XMLID) :-
    normalize_space(atom(Norm), XMLID),
    dom_attrib(Parent, Id),
    assertz(dom_id(Norm, Parent)).

xml_index_hook(Id, 'http://www.w3.org/2001/XMLSchema-instance':type = QName) :-
    xml_expand_qname(false, ref(Id), QName, QN),
    dom_attrib(Parent, Id),
    assertz(dom_schema_type(Parent, QN)).

xml_node_byid(Base, XMLID, ref(Id)) :-
    xml_node_root(Base, Root),
    dom_id(XMLID, Id),
    xml_node_root(ref(Id), Root).

xml_node_schema_type(ref(Id), Type) :-
    dom_schema_type(Id, Type), !.
xml_node_schema_type(Node, 'http://www.w3.org/2001/XMLSchema':untypedAtomic) :-
    xml_node_type(Node, element),
    \+ (xml_node_child(Node, Child), xml_node_type(Child, element)).

xml_node_schema_nilled(Node) :-
    xml_node_attr(Node, Attr), xml_node_qname(Attr,
                                              'http://www.w3.org/2001/XMLSchema-instance':nil),
    xml_node_value(Attr, true), !.

xml_expand_qname(Default, Context, QName, Expanded) :-
    atom_codes(QName, QNL),
    phrase(qname(QN), QNL),
    xml_expand_qname0(Default, Context, QN, Expanded).

xml_expand_qname0(_, Context, NS:Local, URI:Local) :-
    xml_node_ancestor_or_self(Context, X),
    xml_node_nsdecl(X, NSDecl),
    xml_node_name(NSDecl, NS), !,
    xml_node_value(NSDecl, URI).
xml_expand_qname0(true, Context, Local, URI:Local) :- atom(Local),
    xml_node_ancestor_or_self(Context, X),
    xml_node_nsdecl(X, NSDecl),
    xml_node_qname(X, NSDecl, ''), !,
    xml_node_value(NSDecl, URI).
xml_expand_qname0(_, _, Local, Local) :- atom(Local).

xml_node_lang(Node, Lang) :-
    xml_node_ancestor_or_self(Node, X),
    xml_node_attr(X, Attr), xml_node_qname(Attr, xml:lang), !,
    xml_node_value(Attr, Lang).

xml_node_base(Node, Base) :-
    xml_node_ancestor_or_self(Node, X),
    xml_node_attr(X, Attr), xml_node_qname(Attr, xml:base), !,
    xml_node_value(Attr, Base0),
    (uri_is_global(Base0) -> Base = Base0;
     xml_node_parent(X, XP), xml_node_base(XP, PBase), uri_resolve(Base0, PBase, Base)).
xml_node_base(Node, Base) :- xml_node_document_uri(Node, Base).

xml_node_document_uri(Node, URI) :-
    xml_node_root(Node, ref(Id)),
    dom_loaded(URI, Id, _).

xml_load_document(URI, ref(Id), Options) :-
    dom_loaded(URI, Id, Options), !.
xml_load_document(URI, ref(Id), Options) :-
    uri_file_name(URI, FileName),
    load_structure(FileName, XML, [dialect(xmlns) | Options]),
    xml_assert_node(xml(XML), Id),
    assertz(dom_loaded(URI, Id, Options)).