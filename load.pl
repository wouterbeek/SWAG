% The load file for the SWAG project.

:- dynamic(user:project/3).
:- multifile(user:project/3).
user:project('SWAG', 'The Social Web of the Avant-Garde.', swag).

:- use_module(load_project).
:- load_project([
  mt-'ModelTheory',
  plc-'Prolog-Library-Collection',
  plGraph,
  plGraphDraw,
  plGraphViz,
  plHtml,
  plHttp,
  plLangTag,
  plLattice,
  plLatticeDraw,
  plRdf,
  plSet,
  plSvg,
  plTabular,
  plTms,
  plTree,
  plTreeDraw,
  plUri,
  plXml,
  plXsd
]).


:- use_module(library(http/html_head)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plRdf(management/rdf_save_any)).

:- rdf_register_prefix(swag, 'http://sacknerarchives.org/').

:- use_module(swag(sa_scrape)).
:- use_module(swag(swag_home)).

:- dynamic(user:file_search_path/2).
:- multifile(user:file_search_path/2).

user:file_search_path(css, swag(resource/css)).
user:file_search_path(img, swag(resource/img)).
user:file_search_path(js, swag(resource/js)).

:- html_resource(css(swag), [requires([css('swag.css')]),virtual(true)]).

:- initialization(thread_create(init, _, [])).

init:-
  rdf_graph(swag), !.
init:-
  absolute_file_name(
    swag,
    File,
    [access(read),extensions([rdf]),file_errors(fail)]
  ), !,
  rdf_load(File, [graph(swag)]).
init:-
  sa_scrape(swag),
  absolute_file_name(swag, File, [access(write),file_type(turtle)]),
  rdf_save_any(File, [format(ntriples),graph(swag)]).
