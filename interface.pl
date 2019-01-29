:- module(interface, [handle_request/0]).
:- use_module(library(sgml)).
:- use_module(library(cgi)).
:- use_module(library(http/mimetype)).
:- use_module(library(prolog_stack)).
:- use_module(library(xmlutil)).
:- use_module(library(dom)).

:- multifile handle_request/3.

resource(notfound, template, template('notfound.pwp')).
resource(error, template, template('error.pwp')).
resource(pprint, template, template('pprint.pwp')).
resource(nodelist, template, template('nodelist.pwp')).


find_matching_node(Arguments, N) :-
    member(tag(Name), Arguments),
    xml_node_name(N, Name), 
    xml_node_type(N, element),
    (member(text(Text), Arguments), Text \== '' *->
     xml_node_descendant_or_self(N, N1), xml_node_word(N1, Text); true),
    (member(attr(AName), Arguments), member(attrval(AVal), Arguments), AName \== '' *->
     xml_node_attr(N, AN), xml_node_name(AN, AName), xml_node_word(AN, AVal); true).

traverse_node(Arguments, N, NL) :-
    (member(ref(IdAttr), Arguments), IdAttr \== '' *->
     xml_node_attr(N, NA), xml_node_name(NA, IdAttr), xml_node_word(NA, RefVal),
     xml_node_value(Ref, RefVal), xml_node_name(Ref, id), xml_node_attrib(N1, Ref);
     N1 = N),
    (member(up(UpTag), Arguments), UpTag \== '' *->
     xml_node_ancestor_or_self(N1, N2), xml_node_name(N2, UpTag); N2 = N1),
    (member(from(FromTag), Arguments), member(to(ToTag), Arguments), FromTag \== '', ToTag \== '' *->
     xml_node_preceding_sibling(N2, Start), xml_node_name(Start, FromTag),
     xml_node_following_sibling(N2, End), xml_node_name(End, ToTag),
     findall(X, (xml_node_following_sibling(Start, X), xml_node_following_sibling(X, End)), L),
     NL = [Start | L]; NL = [N2]).

handle_request(X, 'GET', _) :-
    atom_concat('/static/', Rsrc, X),
    file_mime_type(Rsrc, Major / Minor),
    open_resource(Rsrc, static, Stream),
    set_stream(Stream, type(binary)),
    format('Content-Type: ~a/~a~n~n', [Major, Minor]),
    set_stream(current_output, type(binary)),
    copy_stream_data(Stream, current_output),
    close(Stream).

handle_request('/query.html', 'GET', Arguments) :- !,
    findall(NodeList, (find_matching_node(Arguments, N),
                      traverse_node(Arguments, N, NodeList)), L),
    pwp_resource(nodelist, template, ['NODES' = L], XHTML),
    writeln('Content-Type: text/html'), nl,
    xhtml_html_write(current_output, XHTML).

handle_request(X, 'GET', _) :-
    atom_concat('/', Name, X),
    file_name_extension(Rsrc, tei, Name),
    atom_number(Rsrc, Id),
    xml_deep_get_node(ref(Id), Doc),
    (Doc = xml(Content); Doc = Content),
    writeln('Content-Type: application/tei+xml'), nl,
    xml_write(current_output, Content, [layout(false)]).

handle_request(X, 'GET', _) :-
    atom_concat('/', Name, X),
    file_name_extension(Rsrc, html, Name),
    atom_number(Rsrc, Id),
    xml_deep_get_node(ref(Id), Doc),
    xml_pretty_print(Doc, PrettyPrint),
    pwp_resource(pprint, template, ['PPRINT' = PrettyPrint], XHTML),
    writeln('Content-Type: text/html'), nl,
    xhtml_html_write(current_output, XHTML).

handle_request(X, 'GET', Arguments) :-
    file_name_extension(Rsrc, html, X),
    pwp_resource(Rsrc, template, ['ARGUMENTS' = Arguments], XML),
    writeln('Content-Type: text/html'), nl,
    xhtml_html_write(current_output, XML).

handle_not_found(Handler, Method, Arguments) :-
    pwp_resource(notfound, template, ['HANDLER' = Handler,
                                      'METHOD' = Method,
                                      'ARGUMENTS' = Arguments], XML),
    writeln('Status: 404 Not Found'),
    writeln('Content-Type: text/html'), nl,
    xhtml_html_write(current_output, XML), !.

handle_exception(Handler, Method, Arguments, Exception) :-
    writeln('Status: 500 Internal Server Error'),
    writeln('Content-Type: text/html'), nl,
    print_message(error, Exception),
    message_to_string(Exception, Message),
    atom_string(MessageAtom, Message),
    pwp_resource(error, template, ['HANDLER' = Handler,
                                   'METHOD' = Method,
                                   'ARGUMENTS' = Arguments,
                                   'EXCEPTION' = Exception,
                                   'MESSAGE' = MessageAtom], XML),
    xhtml_html_write(current_output, XML).
    
handle_request :-
    set_stream(current_output, encoding(utf8)),
    cgi_get_form(Arguments),
    getenv('PATH_INFO', Handler),
    getenv('REQUEST_METHOD', Method),
    catch((handle_request(Handler, Method, Arguments) -> true;
           handle_not_found(Handler, Method, Arguments)),
          Exception,
          handle_exception(Handler, Method, Arguments, Exception)),
    halt.
