% The load file for the Sackner Archive project.

:- multifile(user:project_name/1).
user:project_name('SacknerArchive').

:- initialization(load_sa).

load_sa:-
  % Entry point.
  source_file(load_sa, ThisFile),
  file_directory_name(ThisFile, ThisDir),

  % SA
  assert(user:file_search_path(sa, ThisDir)),
  assert(user:file_search_path(sa_data, sa('Data'))),
  use_module(sa(sa_web)).
