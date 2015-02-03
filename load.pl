% The load file for the SWAG project.

:- use_module(library(ansi_term)).

:- dynamic(user:project/3).
:- multifile(user:project/3).
   user:project('SWAG', 'The Social Web of the Avant-Garde.', swag).

:- use_module(load_project).
:- load_project([
     plc-'Prolog-Library-Collection',
     plDcg,
     plGraph,
     plHtml,
     plHttp,
     plLangTag,
     plRdf,
     plSet,
     plServer,
     plTms,
     plTree,
     plUri,
     plXml,
     plXsd
   ]).



:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- rdf_register_prefix(swag, 'http://sacknerarchives.org/').



:- use_module(plRdf(management/rdf_load_any)).
:- use_module(plRdf(management/rdf_save_any)).

:- use_module(swag(sa_scrape)).

:- initialization(thread_create(init, _, [])).

init:-
  rdf_graph(swag), !.
init:-
  absolute_file_name(
    data(swag),
    File,
    [access(read),file_errors(fail),file_type(ntriples)]
  ), !,
  rdf_load_any(File, [format(ntriples),graph(swag)]).
init:-
  sa_scrape(swag),
  absolute_file_name(data(swag), File, [access(write),file_type(turtle)]),
  rdf_save_any(File, [format(ntriples),graph(swag)]).
