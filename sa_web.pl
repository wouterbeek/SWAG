:- module(sa_web, []).

/** <module> Sackner Archives Web

@author Wouter Beek
@version 2013/04, 2014/01
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(db_ext)).
:- use_module(html(html)).
:- use_module(html(html_image)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_server_files)).
:- use_module(library(lists)).
:- use_module(math(random_ext)).
:- use_module(rdf(rdf_datatype)).
:- use_module(sa(sa)).
:- use_module(server(web_modules)).

:- initialization(web_module_add('SacknerArchive', sa_web)).

:- http_handler(root(sa), sa, [prefix,priority(0),spawn(show_some_images)]).

% /css
http:location(css, root(css), []).
:- db_add_novel(user:file_search_path(css, server(css))).
:- db_add_novel(user:file_search_path(css, sa(css))).
:- http_handler(css(.), serve_files_in_directory(css), [prefix,priority(10)]).
:- html_resource(css('sackner_archive.css'), []).

% /img
http:location(img, root(img), []).
:- db_add_novel(user:file_search_path(img, sa('Images'))).
:- http_handler(img(.), serve_files_in_directory(img), [prefix,priority(10)]).

sa(_Request):-
  reply_html_page(app_style, \swag_head('Grid'), \image_grid(5, 5)).

image_grid(Cols, Rows) -->
  {
     rdf_global_id(prasem:image, Datatype),
    findall(
      Author-Base,
      (
        rdf(Entry, sa:image, literal(type(Datatype, File)), 'CP_Entry'),
        once(rdf_datatype(Entry, sa:author, string, Author, 'CP_Entry')),
        file_base_name(File, Base)
      ),
      Pairs
    ),
    length(Pairs, Length)
  },
  html(div(class=image_group, \image_rows(Length-Pairs, Cols, Rows))).

image_rows(_-_, _, 0) --> !.
image_rows(Length-Pairs, Cols, Rows1) -->
  {Rows2 is Rows1 - 1},
  html([
    div(class=image_row, \image_row(Length-Pairs, Cols)),
    \image_rows(Length-Pairs, Cols, Rows2)
  ]).

image_row(_-_, 0) --> !.
image_row(Length-Pairs, Cols1) -->
  {
    random_betwixt(1, Length, Index),
    nth1(Index, Pairs, Author-Base),
    Cols2 is Cols1 - 1
  },
  html([
    \html_image([], Author, [], Base),
    \image_row(Length-Pairs, Cols2)
  ]).

swag_head(String) -->
  html([
    title('The Social Web of the Avant-Garde (0.0.1) -- ',String),
    \html_requires(css('sackner_archive.css'))
  ]).
