:- module(sa, []).

/** <module> Sackner Archive

@author Wouter Beek
@version 2013/04
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(void(void_file)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(cp, 'http://www.wouterbeek.com/concrete_poetry#').

:- initialization(load_concrete).



load_concrete:-
  rdf_graph('CP'), !.
load_concrete:-
  absolute_file_name(sa_data('CP'), File, [access(read),file_type(turtle)]),
  % Make sure the VoID library is saved in graph 'CP'.
  void_load_library(File, 'CP').

