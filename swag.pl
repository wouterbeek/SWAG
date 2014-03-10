:- module(swag, []).

/** <module> SWAG

The Social Web of the Avant-Garde.
Web front-end for the Social Web of the Avant-Garde.

@author Wouter Beek
@version 2013/04, 2014/01, 2014/03
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
:- use_module(library(semweb/rdf_db)).
:- use_module(math(random_ext)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdf(rdf_serial)).
:- use_module(swag(sa_scrape)).
:- use_module(server(web_modules)).
:- use_module(void(void_file)).
:- use_module(xml(xml_namespace)).

:- http_handler(root(swag), swag, [prefix,priority(0),spawn(show_some_images)]).

:- xml_register_namespace(swag, 'http://www.wouterbeek.com/SWAG/').

% /css
http:location(css, root(css), []).
:- db_add_novel(user:file_search_path(css, server(css))).
:- db_add_novel(user:file_search_path(css, swag(css))).
:- http_handler(css(.), serve_files_in_directory(css), [prefix,priority(10)]).
:- html_resource(css('sackner_archive.css'), []).

% /img
http:location(img, root(img), []).
:- db_add_novel(user:file_search_path(img, swag('Images'))).
:- http_handler(img(.), serve_files_in_directory(img), [prefix,priority(10)]).

:- initialization(init_swag).



swag(_Request):-
  reply_html_page(app_style, \swag_head('Grid'), \image_grid(5, 5)).

image_grid(Cols, Rows) -->
  {
     rdf_global_id(prasem:image, Datatype),
    findall(
      Author-Base,
      (
        rdf(Entry, swag:image, literal(type(Datatype, File)), 'CP_Entry'),
        once(rdf_datatype(Entry, swag:author, string, Author, 'CP_Entry')),
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


init_swag:-
  load_swag,
  web_module_add('SWAG', swag).

load_swag:-
  rdf_graph(swag), !.
load_swag:-
  absolute_file_name(
    data(swag),
    File,
    [access(read),file_errors(fail),file_type(turtle)]
  ),
  rdf_load([format(turtle)], swag, File).
load_swag:-
  sa_scrape(swag),
  absolute_file_name(data(swag), File, [access(write),file_type(turtle)]),
  rdf_save([format(turtle)], swag, File).

