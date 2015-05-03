:- module(swag_main_web, []).

/** <module> SWAG Website

The Social Web of the Avant-Garde.
Web front-end for the Social Web of the Avant-Garde.

@author Wouter Beek
@version 2013/04, 2014/01, 2014/03-2014/04, 2015/02, 2015/04
*/

:- use_module(library(aggregate)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(generics(db_ext)).

:- use_module(plUri(uri_ext)).

:- use_module(plHtml(html_image)).
:- use_module(plHtml(page/http_error_page)).
:- use_module(plHtml(page_component/page_footer)).
:- use_module(plHtml(page_component/page_head)).
:- use_module(plHtml(page_component/page_header)).

:- use_module(plRdf(api/rdf_read)).

:- dynamic(user:file_search_path/2).
:- multifile(user:file_search_path/2).

user:file_search_path(img, data(.)).

:- http_handler(/, swag_home, [id(swag_home),prefix]).
:- http_handler(/, swag_404, [id(swag_404),prefix]).





swag_home(_):-
  reply_styled_html_page(
    html(\swag_head(["Home"])),
    html(\swag_body("Home", \swag_picture_grid, false)
  ).



swag_body(Selection, Content, _) -->
  html(
    div(id=canvas, [
      \page_centered_header(
        "The Semantic Web of the Avant-Garde",
        'logo.svg',
        ["Home"],
        Selection
      ),
      Content,
      footer([])
    ])
  ).



swag_head(Subtitles) -->
  page_head(
    "The Social Web of the Avant-Garde",
    Subtitles,
    []
  ).



swag_404(Request):-
  http_error_page(404, Request, nl, swag_head, swag_body).



swag_picture_grid -->
  {
    site_name(Site),
    once(first_pairs(25, Pairs))
    %%%%random_pairs(25, Pairs)
  },
  html([
    div(id=page_title, Site),
    \html_image_thumbnail_box_grid(5, 5, 350, 350, Pairs)
  ]).

first_pairs(N, Pairs):-
  findnsols(
    N,
    Caption-File,
    (
      rdf(Entry, swag:image, Uri),
      rdf_typed_literal(Entry, swag:author, Caption, xsd:string),
      uri_nested_file(data(.), Uri, File)
    ),
    Pairs
  ).

random_pairs(N, Pairs):-
  aggregate_all(
    count,
    rdfs_individual_of(_, swag:'Entry'),
    Max
  ),
  random_pairs(N, Max, Pairs).

random_pairs(0, _, []):- !.
random_pairs(N1, Max, Pairs1):-
  random_between(1, Max, I),
  rdf_typed_literal(Entry, swag:original_id, I, xsd:integer),
  (   rdf(Entry, swag:image, Uri),
      rdf_typed_literal(Entry, swag:author, Caption, xsd:string)
  ->  uri_nested_file(data(.), Uri, File),
      N2 is N1 - 1,
      Pairs1 = [Caption-File|Pairs2],
      random_pairs(N2, Max, Pairs2)
  ;   random_pairs(N1, Max, Pairs1)
  ).
