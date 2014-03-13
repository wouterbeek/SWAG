:- module(
  sa_clean,
  [
    sa_clean/1 % +Graph:atom
  ]
).

/** <module> Sackner Archives cleaning

Automated cleaning of the Sackner Archives dataset.

@author Wouter Beek
@version 2014/03
*/

:- use_module(dcg(dcg_content)).
:- use_module(dcg(dcg_generic)).
:- use_module(xsd(xsd)).
:- use_module(xsd(xsd_clean)).
:- use_module(xsd(xsd_decimal)).
:- use_module(xsd(xsd_gYear)).



sa_clean(Graph):-
  forall(
    sa_predicate_type(Predicate, _),
    sa_clean(Graph, Predicate)
  ).

sa_clean(Graph, Predicate):-
  sa_predicate_type(Predicate, Type),
  


% Not all years are compliant with `xsd:gYear`.
sa_assert_value(Entry, Predicate, year, Value1, Graph):-
  once(dcg_phrase(year(Value2), Value1)), !,
  sa_assert_value(Entry, Predicate, gYear, Value2, Graph).
sa_assert_value(Entry, Predicate, dimensions, Value, Graph):- !,
  once(dcg_phrase(dimensions(Height, Width, Depth), Value)),
  rdf_bnode(BNode),
  rdf_assert(Entry, Predicate, BNode, Graph),
  rdf_assert_datatype(BNode, swag:height, xsd:decimal, Height, Graph),
  rdf_assert_datatype(BNode, swag:width, xsd:decimal, Width, Graph),
  (
    var(Depth), !
  ;
    rdf_assert_datatype(BNode, swag:depth, xsd:decimal, Depth, Graph)
  ).
sa_assert_value(Entry, Predicate, DatatypeName, Value1, Graph):-
  rdf_datatype(DatatypeName, Datatype),
  xsd_value(DatatypeName, Value1, Value2),
  rdf_assert_datatype(Entry, Predicate, Datatype, Value2, Graph), !.
sa_assert_value(Entry, Predicate, Datatype, Value, Graph):-
  gtrace, %DEB
  format(user_output, '<~w,~w,~w^^~w>', [Entry,Predicate,Value,Datatype]),
  sa_assert_value(Entry, Predicate, Datatype, Value, Graph).

year(Year) -->
  (`c.`, blanks ; []),
  xsd_gYear_lexical_map(dateTime(Year, _, _, _, _, _, _)).

dimensions(Height, Width, Depth) -->
  xsd_decimal_lexical_map(Height),
  dimensions_separator,
  xsd_decimal_lexical_map(Width),
  (
    dimensions_separator,
    xsd_decimal_lexical_map(Depth)
  ;
    []
  ).

dimensions_separator -->
  blanks,
  `x`,
  blanks.


year(Year) -->
  (`c.`, blanks ; []),
  gYearLexicalRep(dateTime(Year, _, _, _, _, _, _)).


sa_predicate_type(number_of_artist_proofs, integer   ).
sa_predicate_type(number_of_images,        integer   ).
sa_predicate_type(number_of_art_proofs,    integer   ).
sa_predicate_type(number_of_letter_copies, integer   ).
sa_predicate_type(announcement,            string    ).
sa_predicate_type(annotation,              string    ).
sa_predicate_type(author,                  string    ).
sa_predicate_type(catalog,                 string    ).
sa_predicate_type(city_country,            string    ).
sa_predicate_type(classification,          string    ).
sa_predicate_type(container,               string    ).
sa_predicate_type(contributor,             string    ).
sa_predicate_type(exhibition_announcement, string    ).
sa_predicate_type(exhibition_catalog,      string    ).
sa_predicate_type(dimensions,              dimensions).
sa_predicate_type(illustration_bwc,        string    ).
sa_predicate_type(inscribed,               string    ).
sa_predicate_type(language,                string    ).
sa_predicate_type(media,                   string    ).
sa_predicate_type(nationality,             string    ).
sa_predicate_type(number_of_dups,          decimal   ).
sa_predicate_type(number_series_month,     string    ).
sa_predicate_type(number_of_pages,         integer   ).
sa_predicate_type(periodical,              string    ).
sa_predicate_type(publisher,               string    ).
sa_predicate_type(purchase_year,           year      ).
sa_predicate_type(series,                  string    ).
sa_predicate_type(signature,               string    ).
sa_predicate_type(subtitle_author,         string    ).
sa_predicate_type(subtitle,                string    ).
sa_predicate_type(title,                   string    ).
sa_predicate_type(number_of_copies,        integer   ).
sa_predicate_type(translator,              string    ).
sa_predicate_type(volume,                  string    ).
sa_predicate_type(year,                    year      ).
