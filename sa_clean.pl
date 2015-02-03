:- module(
  sa_clean,
  [
    sa_clean/1 % +Graph:atom
  ]
).

/** <module> Sackner Archives cleaning

Automated cleaning of the Sackner Archives dataset.

@author Wouter Beek
@version 2014/03-2014/04, 2015/02
*/

:- use_module(library(aggregate)).
:- use_module(library(http/html_write)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- use_module(plXsd(xsd)).

:- use_module(plRdf(rdf_name)).
:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdf_read)).
:- use_module(plRdf(api/rdfs_read)).

:- rdf_meta(sa_clean(r,r,r,+)).
:- rdf_meta(sa_clean_preview(r,r,r,+)).





sa_clean(Graph):-
  forall(
    (
      % @tbd Use rdfs_domain/4 when its done.
      rdf(P, rdfs:range, Range, Graph),
      xsd_datatype(Range),
      \+ rdf_equal(Range, xsd:string)
    ),
    sa_clean_preview(P, xsd:string, Range, Graph)
  ).


sa_clean(P, FromDatatype, ToDatatype, Graph):-
  forall(
    rdf_typed_literal(S, P, FromLexicalForm, FromDatatype, Graph),
    (
      xsd_lexical_map(FromDatatype, FromLexicalForm, Value),
      rdf_assert_typed_literal(S, P, Value, ToDatatype, Graph),
      rdf_retractall_literal(S, P, FromLexicalForm, FromDatatype, Graph)
    )
  ).


sa_clean_preview(P, FromDatatype, ToDatatype, Graph):-
  findall(
    [S,P,FromLexicalForm,ToLexicalForm,ToDatatype,Graph],
    (
      rdf_typed_literal(S, P, FromLexicalForm, FromDatatype, Graph),
      xsd_lexical_map(FromDatatype, FromLexicalForm, Value),
      xsd_canonical_map(ToDatatype, Value, ToLexicalForm)
    ),
    Rows
  ),
  rdf_store_rows(
    html([
      'Type literal transformations for RDF property ',
      \rdf_term_html(rdf_tabular, P),
      '.'
    ]),
    ['Subject','Predicate','Old LEX','New LEX','Datatype','Graph'],
    Rows
  ).



/*
% Not all years are compliant with `xsd:gYear`.
sa_assert_value(Entry, Predicate, year, Value1, Graph):-
  once(dcg_phrase(year(Value2), Value1)), !,
  sa_assert_value(Entry, Predicate, gYear, Value2, Graph).
sa_assert_value(Entry, Predicate, dimensions, Value, Graph):- !,
  once(dcg_phrase(dimensions(Height, Width, Depth), Value)),
  rdf_bnode(BNode),
  rdf_assert(Entry, Predicate, BNode, Graph),
  rdf_assert_typed_literal(BNode, swag:height, Height, xsd:decimal, Graph),
  rdf_assert_typed_literal(BNode, swag:width, Width, xsd:decimal, Graph),
  (   var(Depth)
  ->  true
  ;   rdf_assert_typed_literal(BNode, swag:depth, Depth, xsd:decimal, Graph)
  ).
sa_assert_value(Entry, Predicate, DatatypeName, Value1, Graph):-
  xsd_datatype(DatatypeName, Datatype),
  xsd_value(DatatypeName, Value1, Value2),
  rdf_assert_typed_literal(Entry, Predicate, Value2, Datatype, Graph), !.
sa_assert_value(Entry, Predicate, Datatype, Value, Graph):-
  gtrace, %DEB
  format(user_output, '<~w,~w,~w^^~w>', [Entry,Predicate,Value,Datatype]),
  sa_assert_value(Entry, Predicate, Datatype, Value, Graph).

year(Year) -->
  (   "c.",
      blanks
  ;   ""
  ),
  xsd_gYear_lexical_map(dateTime(Year, _, _, _, _, _, _)).

dimensions(Height, Width, Depth) -->
  xsd_decimal_lexical_map(Height),
  dimensions_separator,
  xsd_decimal_lexical_map(Width),
  (   dimensions_separator,
      xsd_decimal_lexical_map(Depth)
  ;   ""
  ).

dimensions_separator -->
  blanks,
  "x",
  blanks.


year(Year) -->
  ("c.", blanks ; []),
  gYearLexicalRep(dateTime(Year, _, _, _, _, _, _)).
*/

