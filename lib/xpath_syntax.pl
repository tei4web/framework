:- module(xpath_syntax, [xpath//1, xpath_expand_eqname/3, xpath_with_ns/2]).
:- use_module(library(apply)).
:- use_module(library(ebnf)).

:- multifile xpath_static_nsdecl/2.
:- dynamic xpath_static_nsdecl/2.

xpath_expand_eqname(true, Name, URI:Name) :- atom(Name), xpath_static_nsdecl('', URI), !.
xpath_expand_eqname(_, Name, Name) :- atom(Name), !.
xpath_expand_eqname(_, uri(URI):Name, URI:Name).
xpath_expand_eqname(_, NS:Name, URI:Name) :- xpath_static_nsdecl(NS, URI), !.

xlexeme(G) --> lexeme(xpath_comment, G).

xpath_comment --> `(:`, xpath_comment_contents, `:)`.

xpath_comment_contents --> xpath_comment, !, xpath_comment_contents.
xpath_comment_contents, `:)` --> `:)`, !.
xpath_comment_contents --> [_], xpath_comment_contents.

xkeyword(K) --> xlexeme(keyword(K)).

xncname(Name) --> xlexeme(ncname(Name)).

xpath(X) --> whitespace(xpath_comment), expr(X).

param_list(L) --> listof(xlexeme(`,`), X:T, param(X, T), L).

param(Name, T) --> xlexeme(`$`), eqname(false, Name), optional(T = star(true), type_declaration(T)). 	

function_body(X) --> enclosed_expr(X).

enclosed_expr(X) --> xlexeme(`{`), optional(X = null, expr(X)), xlexeme(`}`).
expr(Seq) --> chain([xlexeme(`,`) = (,)], X, expr_single(X), Seq).

expr_single(X) --> for_expr(X).
expr_single(X) --> let_expr(X).
expr_single(X) --> quantified_expr(X).
expr_single(X) --> if_expr(X).
expr_single(X) --> or_expr(X).

for_expr(for(Bindings, X)) --> simple_for_clause(Bindings), xkeyword(return), expr_single(X).

simple_for_clause(L) --> xkeyword(for), listof(xlexeme(`,`), X, simple_for_binding(X), L).

simple_for_binding(Name = X) --> xlexeme(`$`), var_name(Name), xkeyword(in), expr_single(X). 	

let_expr(let(Bindings, X)) --> simple_let_clause(Bindings), xkeyword(return), expr_single(X). 	

simple_let_clause(L) --> xkeyword(let), listof(xlexeme(`,`), X, simple_let_binding(X), L).

simple_let_binding(Name = X) --> xlexeme(`$`), var_name(Name), xlexeme(`:=`), expr_single(X).

quantified_expr(quantified(Quant, L, X1)) -->
    anyof([xkeyword(some) = some, xkeyword(every) = every], Quant),
    listof(xlexeme(`,`), X, quantified_bind(X), L),
    xkeyword(satisfies), expr_single(X1). 	

quantified_bind(N = X) --> xlexeme(`$`), var_name(N), xkeyword(in), expr_single(X).

if_expr(if(Cond, Then, Else)) --> xkeyword(if), xlexeme(`(`), expr(Cond), xlexeme(`)`),
        xkeyword(then), expr_single(Then), xkeyword(else), expr_single(Else). 	

or_expr(Expr) --> chain([xkeyword(or) = or], X, and_expr(X), Expr).

and_expr(Expr) --> chain([xkeyword(and) = and], X, comparison_expr(X), Expr).

comparison_expr(X) --> string_concat_expr(E), comparison_suffix(E, X).

comparison_suffix(X1, compare(Op, X1, X2)) --> comparison_op(Op), !, string_concat_expr(X2).
comparison_suffix(X, X) --> [].

string_concat_expr(Expr) --> chain([xlexeme(`||`) = '||'], X, range_expr(X), Expr).

range_expr(X) --> additive_expr(E), range_suffix(E, X).

range_suffix(X1, range(X1, X2)) --> xkeyword(to), !, additive_expr(X2).
range_suffix(X, X) --> [].

additive_expr(Expr) --> chain([xlexeme(`+`) = (+), xlexeme(`-`) = (-)], X, multiplicative_expr(X), Expr).

multiplicative_expr(Expr) --> chain([xlexeme(`*`) = (*), xkeyword(div) = div, xkeyword(idiv) = idiv, xkeyword(mod) = mod],
                                    X, union_expr(X), Expr).

make_combining(Label, X, Y, Z) :-
    maybe_sort(X, XS),
    maybe_sort(Y, YS),
    Z =.. [Label, XS, YS].

union_expr(Expr) --> chaingen(_, [xkeyword(union) = union, xlexeme((`|`, \+ `|`)) = union],
                              make_combining, X, intersect_except_expr(X), Expr).

intersect_except_expr(Expr) --> chaingen(_, [xkeyword(intersect) = intersect, xkeyword(except) = except],
                                         make_combining, X, instanceof_expr(X), Expr).

instanceof_expr(X) --> treat_expr(E), instanceof_suffix(E, X).

instanceof_suffix(E, instanceof(E, Type)) --> xkeyword(instance), xkeyword(of), !, sequence_type(Type).
instanceof_suffix(E, E) --> [].

treat_expr(X) --> castable_expr(E), treat_suffix(E, X).

treat_suffix(E, treat(E, Type)) --> xkeyword(treat), xkeyword(as), !, sequence_type(Type).
treat_suffix(E, E) --> [].

castable_expr(X) --> cast_expr(E), castable_suffix(E, X).

castable_suffix(E, castable(E, Type, Empty)) --> xkeyword(castable), xkeyword(as), !, single_type(Type, Empty).
castable_suffix(E, E) --> [].

cast_expr(X) --> arrow_expr(E), cast_suffix(E, X).
    
cast_suffix(E, cast(E, Type, Empty)) --> xkeyword(cast), xkeyword(as), !, single_type(Type, Empty).
cast_suffix(E, E) --> [].

arrow_expr(Expr) --> unary_expr(H), postfix_arrow(H, Expr).

postfix_arrow(X, R) --> xlexeme(`=>`), !, arrow_function_specifier(F), argument_list(Args),
    { make_arrow(F, [X|Args], F1) }, postfix_arrow(F1, R).
postfix_arrow(X, X) --> [].

make_arrow(name(F), Args, call(F, Args)).
make_arrow(expr(E), Args, apply(E, Args)).

unary_expr(-(Expr)) --> xlexeme(`-`), unary_expr(Expr).
unary_expr(+(Expr)) --> xlexeme(`+`), unary_expr(Expr).
unary_expr(Expr) --> value_expr(Expr).

value_expr(X) --> simple_map_expr(X).

comparison_op(Op) --> anyof([xlexeme(`=`) = (=),
                             xlexeme(`!=`) = ('!='),
                             xlexeme(`<=`) = (<=),
                             xlexeme(`>=`) = (>=),                             
                             xlexeme(`<<`) = (<<),
                             xlexeme(`>>`) = (>>),
                             xlexeme(`<`) = (<),
                             xlexeme(`>`) = (>),
                             xkeyword(eq) = (eq),
                             xkeyword(ne) = (ne),
                             xkeyword(lt) = (lt),
                             xkeyword(le) = (le),
                             xkeyword(gt) = (gt),
                             xkeyword(ge) = (ge),
                             xkeyword(is) = (is)], Op).

simple_map_expr(Expr) --> chain([xlexeme(`!`) = /], X, path_expr(X), Expr).

path_expr(/(X)) --> xlexeme(`/`), relative_path_expr(_, X), !.
path_expr(/) --> xlexeme(`/`).
path_expr(/(X)) --> xlexeme(`//`), relative_path_expr(step(descendant_or_self, node), X).
path_expr(X) --> relative_path_expr(_, X).

make_path_step([], X, Y, Z) :- !, make_path_step(/, X, Y, Z).
make_path_step(//, X, Y, Z) :- !,
    make_path_step(/, X, step(descendant_or_self, node, []), Y0),
    make_path_step(/, Y0, Y, Z).
make_path_step(/, X, Y, Z) :- !,
    maybe_sort(X / Y, Z).

maybe_sort(X, X) :- is_sorted(X), !.
maybe_sort(X, sort(X)).

is_sorted('.').
is_sorted(filter(X, _)) :- is_sorted(X).
is_sorted(step(child, _)).
is_sorted(step(descendant, _)).
is_sorted(step(self, _)).
is_sorted(step(descendant_or_self, _)).
is_sorted(step(attribute, _)).
is_sorted(step(namespace, _)).
is_sorted(step(following, _)).
is_sorted(step(following_sibling, _)).
is_sorted(step(parent, _)).
is_sorted(X / Y) :- is_sorted(X), is_sorted(Y), no_intermix(X, Y), !.
is_sorted(sort(_)).
is_sorted(union(_, _)).
is_sorted(intersect(_, _)).
is_sorted(except(_, _)).

no_intermix('.', _).
no_intermix(_, '.').
no_intermix(step(self, _), _).
no_intermix(_, step(self, _)).
no_intermix(_ / X, Y) :- no_intermix(X, Y).
no_intermix(X, Y / Z) :- no_intermix(X, Y), no_intermix(Y, Z).
no_intermix(step(child, _), _).
no_intermix(step(following, _), step(child, _)).
no_intermix(step(following_sibling, _), step(child, _)).
no_intermix(step(following, _), step(descendant, _)).
no_intermix(step(following_sibling, _), step(descendant, _)).
no_intermix(step(following, _), step(descendant_or_self, _)).
no_intermix(step(following_sibling, _), step(descendant_or_self, _)).
no_intermix(_, step(attribute, _)).
no_intermix(_, step(namespace, _)).
no_intermix(filter(X), Y) :- no_intermix(X, Y).
no_intermix(X, filter(Y)) :- no_intermix(X, Y).
no_intermix(sort(X), Y) :- no_intermix(X, Y).
no_intermix(X, sort(Y)) :- no_intermix(X, Y).
no_intermix(union(X, Y), Z) :- no_intermix(X, Z), no_intermix(Y, Z).
no_intermix(intersect(X, _), Y) :- no_intermix(X, Y).
no_intermix(except(X, _), Y) :- no_intermix(X, Y).

relative_path_expr(Seed, Expr) --> chaingen(Seed,
                                            [xlexeme(`//`) = //, xlexeme(`/`) = /],
                                            make_path_step,
                                            X, step_expr(X), Expr).

step_expr(X) --> axis_step(X).
step_expr(X) --> postfix_expr(X).

axis_step(X) --> step(Axis, Test), predicate_list(step(Axis, Test), X). 	

step(Axis, Test) --> reverse_step(Axis, Test).
step(Axis, Test) --> forward_step(Axis, Test).

forward_step(Axis, Test) --> forward_axis(Axis), xlexeme(`::`), node_test(Axis, Test).
forward_step(Axis, Test) --> abbrev_forward_step(Axis, Test).

forward_axis(child) --> xkeyword(child).
forward_axis(descendant) --> xkeyword(descendant).
forward_axis(attribute) --> xkeyword(attribute).
forward_axis(self) --> xkeyword(self).
forward_axis(descendant_or_self) --> xkeyword('descendant-or-self').
forward_axis(following_sibling) --> xkeyword('following-sibling').
forward_axis(following) --> xkeyword('following').
forward_axis(namespace) --> xkeyword('namespace').

detect_default_axis(attribute(_), attribute) :- !.
detect_default_axis(attribute(_, _), attribute) :- !.
detect_default_axis(schema_attr(_), attribute) :- !.
detect_default_axis(namespace, namespace) :- !.
detect_default_axis(_, child).

abbrev_forward_step(attribute, X) --> xlexeme(`@`), node_test(attribute, X).
abbrev_forward_step(DefaultAxis, X) --> node_test(default, X), { detect_default_axis(X, DefaultAxis) }.

reverse_step(Axis, Test) --> reverse_axis(Axis), xlexeme(`::`), node_test(Axis, Test).
reverse_step(Axis, Test) --> abbrev_reverse_step(Axis, Test).

reverse_axis(parent) --> xkeyword(parent).
reverse_axis(ancestor) --> xkeyword(ancestor).
reverse_axis(preceding_sibling) --> xkeyword('preceding-sibling').
reverse_axis(preceding) --> xkeyword(preceding).
reverse_axis(ancestor_or_self) --> xkeyword('ancestor-or-self').

abbrev_reverse_step(parent, node)  --> xlexeme(`..`).

node_test(_, X) --> kind_test(X).
node_test(Axis, X) --> name_test(Axis, X).

use_default_ns(attribute, false) :- !.
use_default_ns(namespace, false) :- !.
use_default_ns(_, true).

name_test(_, X) --> wildcard(X).
name_test(Axis, name(X)) --> { use_default_ns(Axis, UseDflt) }, eqname(UseDflt, X).

wildcard(ncname(X)) --> xlexeme(`*:`), xncname(X).
wildcard(namespace(X)) --> xncname(X), xlexeme(`:*`).
wildcard(namespace_uri(X)) --> braced_uri_literal(X), xlexeme(`*`).
wildcard(*) --> xlexeme(`*`).

postfix_expr(Expr) --> primary_expr(X0), postfix_postfix(X0, Expr).

postfix_postfix(X, R) --> predicate(P), !, postfix_postfix(filter(X, P), R).
postfix_postfix(X, R) --> argument_list(L), !, postfix_postfix(apply(X, L), R).
postfix_postfix(X, R) --> lookup(K), !, postfix_postfix(lookup(X, K), R).
postfix_postfix(X, X) --> [].

argument_list(L) --> xlexeme(`(`), opt_listof(xlexeme(`,`), X, argument(X), L), xlexeme(`)`).
        
predicate_list(X, Y) --> predicate(P), !, predicate_list(filter(X, P), Y).
predicate_list(X, X) --> [].

predicate(X) --> xlexeme(`[`), expr(X), xlexeme(`]`).

lookup(X) --> xlexeme(`?`), key_specifier(X).

key_specifier(literal(X)) --> xncname(X).
key_specifier(literal(X)) --> integer_literal(X).
key_specifier(X) --> parenthesized_expr(X).
key_specifier(*) -->  xlexeme(`*`).

arrow_function_specifier(name(X)) --> eqname(false, X).
arrow_function_specifier(expr(var(X))) --> var_ref(X).
arrow_function_specifier(expr(X)) --> parenthesized_expr(X).

primary_expr(literal(X)) --> literal(X), !.
primary_expr(var(X)) --> var_ref(X), !.
primary_expr(X) --> parenthesized_expr(X), !.
primary_expr(X) --> context_item_expr(X), !.
primary_expr(X) --> function_call(X).
primary_expr(X) --> function_item_expr(X), !.
primary_expr(X) --> map_constructor(X), !.
primary_expr(X) --> array_constructor(X), !.
primary_expr(X) --> unary_lookup(X).

literal(X) --> numeric_literal(X); string_literal(X).

numeric_literal(X) --> double_literal(X); decimal_literal(X); integer_literal(X).

var_ref(X) --> xlexeme(`$`), var_name(X). 	

var_name(X) --> eqname(false, X). 	

parenthesized_expr(X) --> xlexeme(`(`), expr(X), xlexeme(`)`), !.
parenthesized_expr(literal([])) --> xlexeme(`(`), xlexeme(`)`).

context_item_expr(.) --> xlexeme(`.`).

function_call(call(X, Args)) --> eqname(false, X), { \+ reserved_keyword(X) }, argument_list(Args).

reserved_keyword('array').
reserved_keyword('attribute').
reserved_keyword('comment').
reserved_keyword('document-node').
reserved_keyword('element').
reserved_keyword('empty-sequence').
reserved_keyword('function').
reserved_keyword('if').
reserved_keyword('item').
reserved_keyword('map').
reserved_keyword('namespace-node').
reserved_keyword('node').
reserved_keyword('processing-instruction').
reserved_keyword('schema-attribute').
reserved_keyword('schema-element').
reserved_keyword('switch').
reserved_keyword('text').
reserved_keyword('typeswitch').

argument(Arg) --> expr_single(Arg); argument_placeholder(Arg).

argument_placeholder(?) --> xlexeme(`?`).

function_item_expr(X) --> named_function_ref(X); inline_function_expr(X).

named_function_ref(#(X, Y)) --> eqname(false, X), xlexeme(`#`), integer_literal(Y).
inline_function_expr(function(L, T, B)) --> xkeyword(function), xlexeme(`(`), optional(L = [], param_list(L)), xlexeme(`)`),
    optional(T = star(true), type_declaration(T)), function_body(B).

map_constructor(map(L)) --> xkeyword(map), xlexeme(`{`), listof(xlexeme(`,`), X, map_constructor_entry(X), L), xlexeme(`}`).
               
map_constructor_entry(K = V) --> map_key_expr(K), xlexeme(`:`), map_value_expr(V).

map_key_expr(X) --> expr_single(X).

map_value_expr(X) --> expr_single(X).

array_constructor(X) --> square_array_constructor(X); curly_array_constructor(X).

square_array_constructor(array(L)) --> xlexeme(`[`), opt_listof(xlexeme(`,`), X, expr_single(X), L), xlexeme(`]`).
                        
curly_array_constructor(array({X})) --> xkeyword(array), enclosed_expr(X).

unary_lookup(lookup('.', X)) --> xlexeme(`?`), key_specifier(X).

single_type(T, true) --> simple_type_name(T), xlexeme(`?`), !.
single_type(T, false) --> simple_type_name(T).

type_declaration(T) --> xkeyword(as), sequence_type(T).

sequence_type(null) --> xkeyword('empty-sequence'), xlexeme(`(`), xlexeme(`)`).
sequence_type(optional(Type)) --> item_type(Type),  xlexeme(`?`), !.
sequence_type(star(Type)) --> item_type(Type),  xlexeme(`*`), !.
sequence_type(plus(Type)) --> item_type(Type),  xlexeme(`+`), !.
sequence_type(single(Type)) --> item_type(Type).

item_type(node(Type)) --> kind_test(Type).
item_type(true) --> xkeyword(item), xlexeme(`(`), xlexeme(`)`).
item_type(Type) --> function_test(Type).
item_type(Type) --> map_test(Type).
item_type(Type) --> array_test(Type).
item_type(atomic(Type)) --> atomic_or_union_type(Type).
item_type(Type) --> parenthesized_item_type(Type). 	

atomic_or_union_type(Name) --> eqname(true, Name).

kind_test(T) --> document_test(T).
kind_test(T) --> element_test(T).
kind_test(T) --> attribute_test(T).
kind_test(T) --> schema_element_test(T).
kind_test(T) --> schema_attribute_test(T).
kind_test(T) --> pitest(T). 
kind_test(T) --> comment_test(T).
kind_test(T) --> text_test(T).
kind_test(T) --> namespace_node_test(T).
kind_test(T) --> any_kind_test(T).

any_kind_test(node) --> xkeyword(node), xlexeme(`(`),  xlexeme(`)`).

document_test(document(T)) --> xkeyword('document-node'), xlexeme(`(`), optional(T = true, (element_test(T); schema_element_test(T))), xlexeme(`)`).
             
text_test(text) --> xkeyword(text), xlexeme(`(`), xlexeme(`)`).
              
comment_test(comment) --> xkeyword(comment), xlexeme(`(`), xlexeme(`)`).

namespace_node_test(namespace) --> xkeyword('namespace-node'), xlexeme(`(`), xlexeme(`)`).

pitest(pi(T)) --> xkeyword('processing-instruction'), xlexeme(`(`), pitest_inner(T), xlexeme(`)`).

pitest_inner(name(T)) --> xncname(T).
pitest_inner(name(T)) --> string_literal(T).
pitest_inner(true) --> [].

attribute_test(attribute(*)) --> xkeyword(attribute), xlexeme(`(`), xlexeme(`)`).
attribute_test(attribute(Name)) --> xkeyword(attribute), xlexeme(`(`), attribute_name_or_wildcard(Name), xlexeme(`)`).
attribute_test(attribute(Name, Type)) --> xkeyword(attribute), xlexeme(`(`), attribute_name_or_wildcard(Name), xlexeme(','), type_name(Type), xlexeme(`)`).

attrib_name_or_wildcard(Name) --> attribute_name(Name).
attrib_name_or_wildcard(*) --> xlexeme(`*`).

schema_attribute_test(schema_attr(X)) --> xkeyword('schema-attribute'), xlexeme(`(`), attribute_declaration(X), xlexeme(`)`).

attribute_declaration(X) --> attribute_name(X).

element_test(element(*)) --> xkeyword(element), xlexeme(`(`), xlexeme(`)`).
element_test(element(Name)) --> xkeyword(element), xlexeme(`(`), element_name_or_wildcard(Name), xlexeme(`)`).
element_test(element(Name, Type, Nillable)) --> xkeyword(element), xlexeme(`(`), element_name_or_wildcard(Name), xlexeme(`,`),
                                                                           type_name(Type), nillable_spec(Nillable),                                                                         
                                                                           xlexeme(`)`).

element_name_or_wildcard(Name) --> element_name(Name).
element_name_or_wildcard(*) --> xlexeme(`*`).

nillable_spec(true) --> xlexeme(`?`), !.
nillable_spec(false) --> [].

schema_element_test(schema_elt(X)) --> xkeyword('schema-element'), xlexeme(`(`), element_declaration(X), xlexeme(`)`).

element_declaration(X) --> element_name(X).

attribute_name(X) --> eqname(false, X).

element_name(X) --> eqname(true, X).

simple_type_name(X) --> type_name(X).

type_name(X) --> eqname(true, X).

function_test(X) --> any_function_test(X).
function_test(X) --> typed_function_test(X).
any_function_test(function) --> xkeyword(function), xlexeme(`(`), xlexeme(`*`), xlexeme(`)`).
typed_function_test(function(L, R)) --> xkeyword(function), xlexeme(`(`),
                                                                    opt_listof(xlexeme(`,`), X, sequence_type(X), L),
                                                                    xlexeme(`)`), xkeyword(as), sequence_type(R).
map_test(X) --> any_map_test(X).
map_test(X) --> typed_map_test(X).

any_map_test(map) --> xkeyword(map), xlexeme(`(`), xlexeme(`*`), xlexeme(`)`).
typed_map_test(map(X, Y)) --> xkeyword(map), xlexeme(`(`), atomic_or_union_type(X), xlexeme(`,`), sequence_type(Y), xlexeme(`)`).
array_test(X) --> any_array_test(X); typed_array_test(X).
any_array_test(array) --> xkeyword(array), xlexeme(`(`), xlexeme(`*`), xlexeme(`)`). 	
typed_array_test(array(T)) --> xkeyword(array), xlexeme(`(`), sequence_type(T), xlexeme(`)`).
parenthesized_item_type(X) --> xlexeme(`(`), item_type(X), xlexeme(`)`). 	
eqname(UseDflt, Exp) --> xlexeme((qname(X); uri_qualified_name(X))),
    { xpath_expand_eqname(UseDflt, X, Exp) }.

integer_literal(N) --> xlexeme(digits(D)), { number_codes(N, D) }.

decimal_literal(N) --> xlexeme(decimal_digits(D)), {number_codes(N, D) }.

decimal_digits([0'. | D]) --> `.`, digits(D).
decimal_digits(D) --> digits(D1), `.`, (digits(D2), !; {D2 = []}), { append(D1, [0'. | D2], D) }.

double_literal(N) --> decimal_digits(D0), (`e`; `E`), opt_sign(Sign), digits(Exp), { append([D0, `E`, Sign, Exp], D), number_codes(N, D) }.

opt_sign([0'+]) --> `+`.
opt_sign([0'-]) --> `-`.
opt_sign([]) --> [].

string_literal(N) --> `"`, star(X, (escape_quot, { X = 0'" }; [X], { X \== 0'" }), L), xlexeme(`"`), { atom_codes(N, L) }.
string_literal(N) --> `'`, star(X, (escape_apos, { X = 0'' }; [X], { X \== 0''}), L), xlexeme(`'`), { atom_codes(N, L) }.

uri_qualified_name(uri(NS):Name) --> braced_uri_literal(NS), xncname(Name).

braced_uri_literal(N) --> `Q`, `{`, star(X, ([X], {X \== 0'{, X \== 0'}}), L), `}`, { atom_codes(N, L) }.
                                
escape_quot --> `""`.
escape_apos --> `''`. 	

digits([H|T]) --> [H], { code_type(H, digit) }, !, digits0(T).
digits0(L) --> digits(L).
digits0([]) --> [].

add_nsdecl(NS = URI, Ref) :- asserta(xpath_static_nsdecl(NS, URI), Ref).

xpath_with_ns(NSList, Goal) :-
    setup_call_cleanup(maplist(add_nsdecl, NSList, Refs), Goal, maplist(erase, Refs)).