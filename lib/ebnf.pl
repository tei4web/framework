:- module(ebnf, [whitespace//0, whitespace//1,
                 lexeme//1, lexeme//2, keyword//1, anyof//2,
                 star//1, star//3, star1//1, star1//3,
                 listof//4, opt_listof//4, chain//4, chaingen//6,
                 ncname//1, qname//1, optional//2]).

:- meta_predicate whitespace(//, ?, ?).
:- meta_predicate lexeme(//, ?, ?).
:- meta_predicate lexeme(//, //, ?, ?).
:- meta_predicate star(//, ?, ?).
:- meta_predicate star(-, //, ?, ?, ?).
:- meta_predicate star1(//, ?, ?).
:- meta_predicate star1(-, //, ?, ?, ?).

:- meta_predicate optional(0, //, ?, ?).

:- meta_predicate anyof(:, ?, ?, ?).

:- meta_predicate listof(//, -, //, ?, ?, ?).
:- meta_predicate opt_listof(//, -, //, ?, ?, ?).

:- meta_predicate chain(:, -, //, ?, ?, ?).
:- meta_predicate chaingen(?, :, 3, -, //, ?, ?, ?).

whitespace --> [W], { code_type(W, space) }, !, whitespace.
whitespace --> [].

whitespace(Comment) --> Comment, !, whitespace(Comment).
whitespace(Comment) --> [W], { code_type(W, space) }, !, whitespace(Comment).
whitespace(_) --> [].

lexeme(G) --> G, whitespace.

lexeme(Comment, G) --> G, whitespace(Comment).

keyword(Atom) --> { atom_codes(Atom, Codes) }, Codes, keyword_trail.
keyword_trail, [X] --> [X], !, { \+ (xml_name_range(A, B), between(A, B, X)) }.
keyword_trail --> [].

star(G) --> { copy_term(G, GC) }, GC, !, star(G).
star(_) --> [].

star(V, R, [H|T]) --> { copy_term(V:R, H:RC) }, RC, !, star(V, R, T).
star(_, _, []) --> [].

star1(G) --> { copy_term(G, GC) }, GC, !, star(G).

star1(V, G, [H|T]) --> { copy_term(V:G, H:RC) }, RC, !, star(V, G, T).

optional(_, G) --> G.
optional(Def, _) --> [], { call(Def) }.

anyof(M:Mapping, Label) --> { member(X = Label, Mapping) }, M:X.

listof(Sep, V, Body, [V0|T]) --> { copy_term(V:Body, V0:Body0) }, Body0, list_next(Sep, V, Body, T).

list_next(Sep, V, Body, T) --> Sep, !, listof(Sep, V, Body, T).
list_next(_, _, _, []) --> [].


opt_listof(Sep, V, Body, Result) --> listof(Sep, V, Body, Result), !.
opt_listof(_, _, _, []) --> [].

chain(Seps, V, Body, Result) --> { copy_term(V:Body, V0:Body0) }, Body0, chain_next(Seps, V0, V, Body, Result).

chain_next(Seps, X, V, Body, Result) --> anyof(Seps, Label), !,
    { copy_term(V:Body, V0:Body0) }, Body0, { Y =.. [Label, X, V0] },
    chain_next(Seps, Y, V, Body, Result).
chain_next(_, X, _, _, X) --> [].

chaingen(Seed, Seps, Fold, V, Body, Result) -->
    { copy_term(V:Body, V0:Body0) }, Body0,
    { var(Seed) -> V1 = V0; call(Fold, [], Seed, V0, V1) },
    chaingen_next(Seps, Fold, V1, V, Body, Result).

chaingen_next(Seps, Fold, X, V, Body, Result) --> anyof(Seps, Label), !,
    { copy_term(V:Body, V0:Body0) }, Body0, { call(Fold, Label, X, V0, Y) },
    chaingen_next(Seps, Fold, Y, V, Body, Result).
chaingen_next(_, _, X, _, _, X) --> [].

xml_namestart_range(0'A, 0'Z).
xml_namestart_range(0'_, 0'_).
xml_namestart_range(0'a, 0'z).
xml_namestart_range(0xC0, 0xD6).
xml_namestart_range(0xD8, 0xF6). 
xml_namestart_range(0xF8, 0x2FF).
xml_namestart_range(0x370, 0x37D). 
xml_namestart_range(0x37F, 0x1FFF). 
xml_namestart_range(0x200C, 0x200D). 
xml_namestart_range(0x2070, 0x218F). 
xml_namestart_range(0x2C00, 0x2FEF). 
xml_namestart_range(0x3001, 0xD7FF). 
xml_namestart_range(0xF900, 0xFDCF). 
xml_namestart_range(0xFDF0, 0xFFFD). 
xml_namestart_range(0x10000, 0xEFFFF).

xml_name_range(X, Y) :- xml_namestart_range(X, Y).
xml_name_range(0'-, 0'.).
xml_name_range(0'0, 0'9).
xml_name_range(0xB7, 0xB7).
xml_name_range(0x0300, 0x036F).
xml_name_range(0x203F, 0x2040).

ncname(Name) --> [N], { xml_namestart_range(A, B), between(A, B, N) }, ncname_cont(T), { atom_chars(Name, [N|T]) }.

ncname_cont([N | T]) --> [N], { xml_name_range(A, B), between(A, B, N) }, !, ncname_cont(T).
ncname_cont([]), [X] --> [X], !, { \+ (xml_name_range(A, B), between(A, B, X)) }.
ncname_cont([]) --> [].

qname(NS:NC) --> ncname(NS), `:`, ncname(NC), !.
qname(NC) --> ncname(NC).

