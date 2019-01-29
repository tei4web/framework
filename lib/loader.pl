:- module(loader, [load_source/2]).
:- use_module(library(sgml)).
:- use_module(library(ebnf)).
:- use_module(library(http/mimetype)).

:- multifile mime_type_loader/4.
:- multifile mime:mime_extension/2.

mime:mime_extension(tei, application/'tei+xml').

mime_type_loader(application/'tei+xml', Source, _, xml(XML)) :-
    load_structure(Source, XML, [dialect(xmlns), space(default)]).

attach_header(Source, Header) :-
    file_name_extension(Source, teih, HeaderSource),
    exists_file(HeaderSource) ->
    load_structure(HeaderSource, Header, [dialect(xmlns), space(default)]);
    Header = [].

load_source(Source, Document) :-
    file_mime_type(Source, MimeType),
    attach_header(Source, Parameters),
    mime_type_loader(MimeType, Source, Parameters, Document).



              
    