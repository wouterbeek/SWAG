% The standalone startup file for the SWAG project.

:- if(current_prolog_flag(argv, ['--debug'|_])).
  :- ensure_loaded(debug).
:- else.
  :- ensure_loaded(load).
:- endif.

:- use_module(plc(server/app_server)).
:- start_app_server_clas.

:- use_module(plHtml(template/easy_peasy)).
:- dynamic(user:current_html_style/1).
:- multifile(user:current_html_style/1).
user:current_html_style(easy_peasy).
