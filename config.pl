file_search_path(template, '../adaptors/morphy').
resource('pprint.css', static, './static/pprint.css').
resource('/searchform', template, template('searchform.pwp')).
:- use_module('../adaptors/morphy/morphy2tei').
:- consult('../adaptors/morphy/config').

