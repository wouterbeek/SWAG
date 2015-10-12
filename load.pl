:- use_module(library(html/html_resource)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_server)).
:- use_module(library(http/http_session)).
:- use_module(library(rdf/rdf_save)).
:- use_module(library(sb/sb_init)).
:- use_module(library(sb/sb_settings)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(settings)).

:- rdf_register_prefix(swag, 'http://sacknerarchives.org/').

:- start_server.

:- load_settings('swag.db').

:- dynamic(user:file_search_path/2).
:- multifile(user:file_search_path/2).

user:file_search_path(css, resource/css).
user:file_search_path(img, resource/img).

:- html_resource(css(swag), [requires([css('swag.css')]),virtual(true)]).

:- use_module(sa_scrape).
:- use_module(swag_home).

:- initialization(thread_create(init, _, [])).

init:-
  init(swag).

init(G):-
  rdf_graph(G), !.
init(G):-
  absolute_file_name(
    G,
    File,
    [access(read),extensions([rdf]),file_errors(fail)]
  ), !,
  rdf_load(File, [graph(G)]).
init(G):-
  sa_scrape(G),
  absolute_file_name(G, File, [access(write),extensions([nt])]),
  rdf_save_any(File, [format(ctriples),graph(G)]).
