:- module(swag_home, []).

/** <module> SWAG homepage

The Social Web of the Avant-Garde.
Web front-end for the Social Web of the Avant-Garde.

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(aggregate)).
:- use_module(library(html/component/html_page_head)).
:- use_module(library(html/component/html_page_header)).
:- use_module(library(html/element/html_image)).
:- use_module(library(html/page/html_error_page)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(rdf/rdf_read)).
:- use_module(library(sb/html_sb)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- http_handler(/, swag_home, [priority(1)]).
:- http_handler(/, swag_404, [prefix,priority(-1)]).





swag_404(_):-
  html_error_page(404, nl, swag_head, swag_body).

swag_home(_):-
  reply_html_page(semblog,
    \swag_head(["Home"]),
    html(\swag_body("Home", \swag_picture_grid))
  ).

swag_body(Selection, Content) -->
  swag_body(Selection, Content, options{}).

swag_body(Selection, Content, _) -->
  html(
    div(id=canvas, [
      \page_horizontal_header(
        "SWAG",
        'header.jpg',
        ["Home"],
        Selection
      ),
      Content,
      \swag_footer
    ])
  ).

swag_footer -->
  html(
    div([
      "Developed by ",
      a(href='http://www.wouterbeek.com', "Wouter Beek"),
      " in May 2015 using ",
      a(href='http://www.swi-prolog.org', "SWI-Prolog"),
      "."
    ])
  ).

swag_head(Subtitles) -->
  html(
    \html_page_head(
      "The Social Web of the Avant-Garde",
      Subtitles,
      []
    )
  ).




swag_picture_grid -->
  {once(first_pairs(25, Pairs))},
  html(\html_image_thumbnail_box_grid(5, 5, Pairs, [])).

first_pairs(N, Pairs):-
  findnsols(
    N,
    image(Img,Caption),
    (
      rdf_literal(Entry, swag:image, xsd:string, Img),
      rdf_literal(Entry, swag:author, xsd:string, Caption)
    ),
    Pairs
  ).

random_pairs(N, Pairs):-
  aggregate_all(count, rdfs_individual_of(_, swag:'Entry'), Max),
  random_pairs(N, Max, Pairs).

random_pairs(0, _, []):- !.
random_pairs(N1, Max, Pairs1):-
  random_between(1, Max, I),
  rdf_literal(Entry, swag:original_id, xsd:integer, I),
  (   rdf_literal(Entry, swag:image, xsd:string, Img),
      rdf_literal(Entry, swag:author, xsd:string, Caption)
  ->  N2 is N1 - 1,
      Pairs1 = [Caption-Img|Pairs2],
      random_pairs(N2, Max, Pairs2)
  ;   random_pairs(N1, Max, Pairs1)
  ).
