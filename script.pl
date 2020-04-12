:- encoding(utf8).
:- module(script, [run/0, run/1]).

/** <module> Transformation script

@author Wouter Beek
@version 2020
*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(xpath)).
:- use_module(library(yall)).

:- use_module(library(atom_ext)).
:- use_module(library(debug_ext)).
:- use_module(library(file_ext)).
:- use_module(library(html/html_ext)).
:- use_module(library(http/http_client2)).
:- use_module(library(rdf/rdf_api)).
:- use_module(library(rdf/rdf_mem)).
:- use_module(library(rdf/rdf_prefix)).
:- use_module(library(rdf/rdf_term)).
:- use_module(library(triply/triply_api)).

:- discontiguous
    property_name_/2,
    property_name_/3.

:- maplist(rdf_register_prefix, [
     dct,
     graph-'https://triplydb.com/wouter/sa/graphs/',
     sdo,
     vocab-'https://triplydb.com/wouter/sa/vocab/',
     work-'https://triplydb.com/wouter/sa/id/work/'
   ]).

:- rdf_meta
   property_name(+, r, r),
   property_name_(+, r),
   property_name_(+, r, r),
   scrape_entries(+, r).


%! run is det.
%! run(+Offset:nonneg) is det.
%
% The Offset argument allows for easy continuing of crawls that broke
% before being completed.
%
% The last entry has identifier 50,122, as of 2013/03/14.

run :-
  run(0).


run(Offset) :-
  rdf_prefix_iri(graph, data, Name),
  create_dataset([graph(memory(Name))], [], Dataset),
  between(Offset, inf, Index),
  (run_item(Dataset, Index) -> fail ; !).


run_item(Dataset, Index) :-
  % The entry identifier must be padded with zeros.
  format(atom(Query), "310201890825~~~|~`0t~d~5+", [Index]),
  html_download(
    uri(http,'ww3.rediscov.com',[sacknerarchives,'showitem.aspx'],Query,_),
    Dom,
    [html_dialect(html4)]
  ),
  xpath_chk(Dom, //table, Table),
  findall(Row, table_row(Table, Row), Rows),
  run_rows(Dataset, Index, Table, Rows).


% Do not create a subject term for empty records.
run_rows(_, Index, _, []) :-
  format(user_output, "~D empty\n", [Index]).
run_rows(Dataset, Index, Table, Rows) :-
  atom_number(Id, Index),
  rdf_prefix_iri(work, Id, Work),
  assert_instance(Work, sdo:'CreativeWork', Dataset),
  assert_triple(Work, sdo:id, string(Id), Dataset),
  % Add the properties.
  forall(
    member(row(P,D,Lex), Rows),
    (
      rdf_typed_literal(D, Lex, Lit),
      assert_triple(Work, P, Lit, Dataset)
    )
  ),
  % Add the images, if any.
  xpath_chk(Table, //tr(2)/td(2)/p, P),
  forall(
    (
      xpath(P, input(@src(lower)), ImageSubpath),
      atom_concat('thumb\\', ImageName, ImageSubpath)
    ),
    download_image(Dataset, Index, Work, ImageName)
  ),
  format_interval(Index, 100).


table_row(Table, row(P,D,Lex)) :-
  xpath(Table, //tr, Row),
  xpath_chk(Row, //td(1)/span(normalize_space), Name),
  % Skip empty keys.
  \+ memberchk(Name, ['Label']),
  property_name(Name, P, D),
  xpath_chk(Row, //td(2)/p, Paragraph),
  (   xpath_chk(Paragraph, //input(@value), Lexs0)
  ->  true
  ;   xpath_chk(Paragraph, //textarea(normalize_space), Lexs0)
  ),
  % Some values are enumarations separated by dashes.
  atomic_list_concat(Lexs, ' --', Lexs0),
  member(Lex0, Lexs),
  % Some values have superfluous spaces pre- and/or postfixed.
  atom_strip(Lex0, Lex).


%! download_image(+Dataset:compound, +Index:nonneg, +Work:rdf_subject, +Name:atom) is det.
%
% Downloads one image for the given entry in the Sackner Archive, if
% the image is non-blank.
%
% The image is retrieved from the server of the Sackner Archive and is
% then stored locally.
%
% If an entry has more than one image, then subsequent runs of this
% predicate will return those additional images.
%
% If the entry has no more images, then this method succeeds without
% instantiating `ImageName'.

% Skip the blank image.
download_image(_, _, _, 'blank.jpg') :- !.
% Skip erroneous image.
download_image(_, 14 424, _, _) :- !.
download_image(Dataset, _, Work, Name) :-
  directory_file_path(img, Name, File),
  % @note We have to explicitly request a JPEG image, because the
  %       server cannot process ‘Accept’ header value ‘*’.
  http_download(
    uri(http,'ww3.rediscov.com',[sacknerarchives,'FULL',Name],_,_),
    File,
    [accept(jpg)]
  ),
  asset_uri(triplydb, wouter, sa, Name, Asset),
  assert_triple(Work, foaf:depiction, uri(Asset), Dataset).


%! property_name(+Name:atom, -P:iri, -D:iri) is det.

property_name(Name, P, D) :-
  property_name_(Name, P, D), !.
property_name(Name, P, xsd:string) :-
  property_name_(Name, P), !.
property_name(Name, _, _) :-
  (var(Name) -> instantiation_error(Name) ; syntax_error(Name)).

property_name_('# Artist Proofs:',         vocab:number_of_artist_proofs, xsd:nonNegativeInteger).
property_name_('# Images:',                vocab:number_of_images, xsd:nonNegativeInteger).
property_name_('# Letter Art Proofs:',     vocab:number_of_art_proofs, xsd:nonNegativeInteger).
property_name_('# Letter Copies:',         vocab:number_of_letter_copies, xsd:nonNegativeInteger).
property_name_('Announcement:',            vocab:announcement).
property_name_('Annotation:',              vocab:annotation).
property_name_('Author:',                  dct:creator).
property_name_('Catalog:',                 vocab:catalog).
property_name_('City County:',             vocab:city_country).
property_name_('Classification:',          vocab:classification).
property_name_('Container:',               vocab:container).
property_name_('Contributors:',            vocab:contributor).
property_name_('Exhibition Announcement:', vocab:exhibition_announcement).
property_name_('Exhibition Catalog:',      vocab:exhibition_catalog).
property_name_('Ht Wdt Dpth:',             vocab:dimensions).
property_name_('Illus BWC:',               vocab:illustration_bwc).
property_name_('Inscribed:',               vocab:inscribed).
property_name_('Language:',                vocab:language).
property_name_('Media:',                   vocab:media).
property_name_('Nationality:',             vocab:nationality).
property_name_('Number of Dups:',          vocab:number_of_dups, xsd:decimal).
property_name_('Nbr Ser Mn:',              vocab:number_series_month).
property_name_('Pages:',                   vocab:number_of_pages, xsd:nonNegativeInteger).
property_name_('Periodical:',              vocab:periodical).
property_name_('Publisher:',               dct:publisher).
property_name_('Purchase Year:',           vocab:purchase_year, xsd:gYear).
property_name_('Series:',                  vocab:series).
property_name_('Signature:',               vocab:signature).
property_name_('Sub Tit Au:',              vocab:subtitle_author).
property_name_('Subtitle:',                vocab:subtitle).
property_name_('Title:',                   dct:title).
property_name_('Total Copies:',            vocab:number_of_copies, xsd:nonNegativeInteger).
property_name_('Translator:',              vocab:translator).
property_name_('Volume:',                  vocab:volume).
property_name_('Year:',                    dct:created, xsd:gYear).
