% The standalone startup file for the SWAG project.

:- if(current_prolog_flag(argv, ['--debug'|_])).
  :- ensure_loaded(debug).
:- else.
  :- ensure_loaded(load).
:- endif.


% Start a server that runs the Web interface.
:- use_module(load_project).
:- load_subproject(dh, plServer).

:- use_module(plServer(app_server)).
:- use_module(plServer(web_modules)). % Web module registration.

:- start_app_server_clas.


:- multifile(user:file_search_path/2).

:- dynamic(user:web_module/2).
:- multifile(user:web_module/2).

http:location(swag, /, []).

% SWAG: Main
:- use_module(swag(web/swag_main_web)).
user:web_module('SWAG', swag_main_web).
