% The load file for the SWAG project.

:- multifile(user:project_name/1).
user:project_name('SWAG').

:- initialization(load_swag).

load_swag:-
  % Entry point.
  source_file(load_swag, ThisFile),
  file_directory_name(ThisFile, ThisDir),

  % SWAG
  assert(user:file_search_path(swag, ThisDir)),
  use_module(swag(swag)).

