:- module(ebnf, [whitespace//0, lexeme//1, star//1, star//3, ncname//1, qname//1]).

:- meta_predicate lexeme(//, ?, ?).
:- meta_predicate star(:, ?, ?, ?).

whitespace --> [W], { code_type(W, space) }, !, whitespace.
whitespace --> [].

lexeme(G) --> G, whitespace.

star(G) --> { copy_term(G, GC) }, GC, !, star(G).
star(_) --> [].

star(V, R, [H|T]) --> { copy_term(V:R, H:RC) }, RC, !, star(V, R, T).
star(_, _, []) --> [].

xml_namestart_range(0'A, 0'Z).
xml_namestart_range(0'_, 0'_).
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

ncname([N | T]) --> [N], { xml_namestart_range(A, B), between(A, B, N) }, ncname_cont(T).

ncname_cont([N | T]) --> [N], { xml_name_range(A, B), between(A, B, N) }, !, ncname_cont(T).
ncname_cont([]) --> [].

qname(NS:NC) --> ncname(NS), `:`, ncname(NC), !.
qname(NC) --> ncname(NC).