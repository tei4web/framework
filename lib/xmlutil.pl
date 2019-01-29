:- module(xmlutil, [xml_resource/3, pwp_resource/4, xhtml_to_html/2, xhtml_html_write/2,
                    make_html_elt/4, make_html_span/3, xml_pretty_print/2]).
:- use_module(library(apply)).
:- use_module(library(sgml)).
:- use_module(library(sgml_write)).
:- use_module(library(pwp)).
:- use_module(library(error)).
:- meta_predicate xml_resource(:, ?, ?).
:- meta_predicate pwp_resource(:, ?, +, ?).

open_resource_exc(Rsrc, Class, Stream) :-
    open_resource(Rsrc, Class, Stream), !.
open_resource_exc(Rsrc, Class, _) :-
    existence_error(resource, Rsrc:Class).

xml_resource(Rsrc, Class, XML) :-
    open_resource_exc(Rsrc, Class, Stream),
    set_stream(Stream, type(text)), set_stream(Stream, encoding('utf8')),
    load_structure(stream(Stream), XML, [dialect(xmlns), space(sgml)]),
    close(Stream).

pwp_resource(M:Rsrc, Class, Context, XML) :-
    xml_resource(M:Rsrc, Class, PWP),
    pwp_xml(M:PWP, XML, Context).

xhtml_to_html(element('http://www.w3.org/1999/xhtml':Name, Attrs, Children),
              element(Name, CvtAttrs, CvtChildren)) :-
    convlist(xhtml_to_html, Attrs, CvtAttrs),
    convlist(xhtml_to_html, Children, CvtChildren).
xhtml_to_html(Name = Value, Name = Value) :- atom(Name), Name \== xmlns.
xhtml_to_html(comment(X), comment(X)).
xhtml_to_html(X, X) :- atom(X).
xhtml_to_html(L, CvtL) :- is_list(L), convlist(xhtml_to_html, L, CvtL).

xhtml_html_write(Stream, XHTML) :-
    xhtml_to_html(XHTML, HTML),
    html_write(Stream, HTML, [doctype(html), system('http://www.w3.org/TR/html4/strict.dtd'),
                              public('-//W3C//DTD HTML 4.01//EN'), layout(false)]).

make_html_elt(Name, Attrs, Children, element('http://www.w3.org/1999/xhtml':Name,
                                             Attrs, Children)).

make_html_span(Classes, Children, Result) :-
    (Classes == [] -> ClsList = [];
     atomic_list_concat(Classes, ' ', ClassesStr), ClsList = [class = ClassesStr]),
    make_html_elt(span, ClsList, Children, Result).

xml_pretty_print(L, L1) :- is_list(L), maplist(xml_pretty_print, L, L1).
xml_pretty_print(xml(Doc), Text) :- xml_pretty_print(Doc, Text).
xml_pretty_print(Text, Text) :- atom(Text).
xml_pretty_print(element(Name, Attrs, Children), Div) :-
    xml_pp_name(Name, PPN),
    make_html_span(['pp-starttag'], [PPN], Start),
    maplist(xml_pretty_print, Attrs, PPAttrs),
    make_html_span(['pp-attributes'], PPAttrs, PPAttr),
    maplist(xml_pretty_print, Children, PPChildren),
    make_html_elt(div, [class = 'pp-children'], PPChildren, PPChild),
    make_html_span(['pp-starttag'], [PPN], End),
    make_html_elt(div, [class = 'pp-element'], [Start, PPAttr, PPChild, End], Div).
    
xml_pretty_print(comment(Text), Div) :- make_html_elt(comment, [class = 'pp-comment'], [Text], Div).
xml_pretty_print(pi(Text), Div) :- make_html_elt(div, [class = 'pp-pi'], [Text], Div).
xml_pretty_print(X = Y, Span) :- xml_pp_name(X, PPN), make_html_span(['pp-attribute'], [PPN, ' = ', Y], Span).

xml_pp_name(X, PPN) :- atom(X), make_html_span(['pp-name'], [X], PPN).
xml_pp_name(NS:Name, PPN) :- make_html_span(['pp-name'], [NS, ':', Name], PPN).