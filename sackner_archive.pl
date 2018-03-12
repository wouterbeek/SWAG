:- module(sackner_archive, [run/1,upload/0]).

/** <module> Sackner Archive

Constructs a Semantic Web database based on the Sackner Archive data,
by scraping an online Web site.

# Number of entries clawled.

47,374 entries, with codes between 0 and 50,122.

@author Wouter Beek
@version 2013, 2015, 2018
*/

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(http/http_open), []).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db), [
     rdf/4,
     rdf_assert/4,
     rdf_load/2,
     rdf_unload_graph/1
   ]).
:- use_module(library(semweb/turtle), []).
:- use_module(library(sgml)).
:- use_module(library(xpath)).

:- use_module(library(atom_ext)).
:- use_module(library(http/http_client2)).
:- use_module(library(sw/rdf_export)).
:- use_module(library(sw/rdf_prefix)).
:- use_module(library(sw/rdf_term)).
:- use_module(library(tapir)).
:- use_module(library(uri_ext)).

:- debug(sackner_archive).

:- discontiguous
    property_name_/2,
    property_name_/3.

:- maplist(rdf_assert_prefix, [
     dct-'http://purl.org/dc/terms/',
     def-'https://sackner-archive.org/def/',
     graph-'https://sackner-archive.org/graph/',
     schema-'http://schema.org/',
     work-'https://sackner-archive.org/id/work/'
   ]).

:- rdf_meta
   property_name(+, r, r),
   property_name_(+, r),
   property_name_(+, r, r),
   scrape_entries(+, r).





%! run(+N:nonneg) is det.
%
% Scrapes the Sackner Archive.
%
% Argument `FirstEntry` allows for easy continuing of crawls
% that broke before being completed.
%
% The last entry has identifier 50,122, as of 2013/03/14.

run(N) :-
  must_be(nonneg, N),
  debug(sackner_archive, "Start scrape.", []),
  scrape_entries(N, graph:data),
  upload.

upload :-
  setup_call_cleanup(
    gzopen('data.nq.gz', write, Out),
    forall(
      rdf(S, P, O, G),
      rdf_write_quad(Out, S, P, O, G)
    ),
    close(Out)
  ),
  expand_file_name('img/*.jpg', Files),
  Properties = _{
    accessLevel: public,
    avatar: 'avatar.gif',
    binary_files: Files,
    description: "Ruth and Marvin Sackner founded the Archive in Miami Beach, Florida in 1979, later moving it to Miami, Florida in 2005.  Its initial mission was to establish a collection of books, critical texts, periodicals, ephemera, prints, drawings, collages, paintings, sculptures, objects, manuscripts, and correspondence dealing with precedent and contemporary, internationally produced, concrete and visual poetry.  The antecedent material had at its starting point, Stephane Mallarme’s poem, “Un Coup de Des” (Cosmopolis, 1897).  The historic examples included works with concrete/visual poetic sensibilities from such twentieth century art movements as Italian Futurism, Russian and Eastern European Avant Garde, Dada, Surrealism, Bauhaus, De Stijl, Ultra, Tabu-Dada, Lettrisme, and Ultra-Lettrisme.",
    files: ['data.nq.gz','vocab.trig'],
    prefixes: [dct,def,graph,schema,work]
  },
  dataset_upload('sackner-archive', Properties),
  % Clean up temporary files.
  concurrent_maplist(delete_file, ['data.nq.gz'|Files]).

scrape_entries(N1, G) :-
  (   scrape_entry(N1, G)
  ->  N2 is N1 + 1,
      scrape_entries(N2, G)
  ;   print_message(success, "Done!")
  ).

scrape_entry(N, G) :-
  atom_number(Id, N),
  % The entry identifier has to be padded with zeros.
  format(atom(Query), "310201890825~~~|~`0t~d~5+", [N]),
  uri_comps(
    Uri,
    uri(http,'ww3.rediscov.com',[sacknerarchives,'showitem.aspx'],Query,_)
  ),
  http_open2(Uri, In, [accept(html)]),
  call_cleanup(
    load_html(In, Dom, [html_dialect(html4)]),
    close(In)
  ),
  xpath_chk(Dom, //table, Table),
  findall(Triple, table_row(Table, Triple), Triples),
  % Do not create a subject term for empty records.
  (   Triples == []
  ->  debug(sackner_archive, "Skipping empty entry ~D.", [N])
  ;   rdf_prefix_iri(work, Id, S),
      rdf_assert(S, rdf:type, schema:'CreativeWork', G),
      rdf_assert(S, def:source_id, literal(type(xsd:nonNegativeInteger,Id)), G),
      % Add the properties.
      maplist({S,G}/[Triple]>>assert_row(S, Triple, G), Triples),
      % Add the images, if any.
      xpath_chk(Table, //tr(2)/td(2)/p, P),
      forall(
        (
          xpath(P, input(@src(lower)), ImageSubpath),
          atom_concat('thumb\\', ImageName, ImageSubpath),
          % Exclude empty images.
          ImageName \== 'blank.jpg'
        ),
        download_image(S, ImageName, G)
      ),
      debug(sackner_archive, "Scraped entry ~D.", [N])
  ).

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

assert_row(S, row(P,D,Lex), G) :-
  writeln(P),
  rdf_assert(S, P, literal(type(D,Lex)), G).


%! download_image(+S:iri, +ImageName:atom, +Graph:rdf_graph) is det.
%
% Returns an image for the given entry in the Sackner Archive.
%
% The image is retrieved from the server of the Sackner Archive and is
% then stored locally.
%
% If an entry has more than one image, then subsequent runs of this
% predicate will return those additional images.
%
% If the entry has no more images, then this method succeeds without
% instantiating `ImageName'.

download_image(S, Lex0, G) :-
  uri_comps(
    Uri,
    uri(http,'ww3.rediscov.com',[sacknerarchives,'FULL',Lex0],_,_)
  ),
  directory_file_path(img, Lex0, File),
  atom_concat('https://krr.triply.cc/wouter/sackner-archive/files/', Lex0, Lex),
  file_download_buggy(Uri, File),
  rdf_assert(S, foaf:depiction, literal(type(xsd:anyURI,Lex)), G).



%! property_name(+Name:atom, -P:iri, -D:iri) is det.

property_name(Name, P, D) :-
  property_name_(Name, P, D), !.
property_name(Name, P, xsd:string) :-
  property_name_(Name, P), !.
property_name(Name, _, _) :-
  (var(Name) -> instantiation_error(Name) ; syntax_error(Name)).

property_name_('# Artist Proofs:',         def:number_of_artist_proofs, xsd:nonNegativeInteger).
property_name_('# Images:',                def:number_of_images, xsd:nonNegativeInteger).
property_name_('# Letter Art Proofs:',     def:number_of_art_proofs, xsd:nonNegativeInteger).
property_name_('# Letter Copies:',         def:number_of_letter_copies, xsd:nonNegativeInteger).
property_name_('Announcement:',            def:announcement).
property_name_('Annotation:',              def:annotation).
property_name_('Author:',                  dct:creator).
property_name_('Catalog:',                 def:catalog).
property_name_('City County:',             def:city_country).
property_name_('Classification:',          def:classification).
property_name_('Container:',               def:container).
property_name_('Contributors:',            def:contributor).
property_name_('Exhibition Announcement:', def:exhibition_announcement).
property_name_('Exhibition Catalog:',      def:exhibition_catalog).
property_name_('Ht Wdt Dpth:',             def:dimensions).
property_name_('Illus BWC:',               def:illustration_bwc).
property_name_('Inscribed:',               def:inscribed).
property_name_('Language:',                def:language).
property_name_('Media:',                   def:media).
property_name_('Nationality:',             def:nationality).
property_name_('Number of Dups:',          def:number_of_dups, xsd:decimal).
property_name_('Nbr Ser Mn:',              def:number_series_month).
property_name_('Pages:',                   def:number_of_pages, xsd:nonNegativeInteger).
property_name_('Periodical:',              def:periodical).
property_name_('Publisher:',               dct:publisher).
property_name_('Purchase Year:',           def:purchase_year, xsd:gYear).
property_name_('Series:',                  def:series).
property_name_('Signature:',               def:signature).
property_name_('Sub Tit Au:',              def:subtitle_author).
property_name_('Subtitle:',                def:subtitle).
property_name_('Title:',                   def:title).
property_name_('Total Copies:',            def:number_of_copies, xsd:nonNegativeInteger).
property_name_('Translator:',              def:translator).
property_name_('Volume:',                  def:volume).
property_name_('Year:',                    dct:created, xsd:gYear).





% GENERICS %

% @bug The Sackner Archive cannot handle the HTTP `Accept' header for
%      image downloads.
file_download_buggy(Uri, File) :-
  http_open:http_open(Uri, In, [status_code(Status)]),
  (   Status == 200
  ->  true
  ;   print_message(warning, could_not_download_image(Uri,Status))
  ),
  call_cleanup(
    setup_call_cleanup(
      open(File, write, Out, [type(binary)]),
      copy_stream_data(In, Out),
      close(Out)
    ),
    close(In)
  ).

html_download(Uri, Dom, Options) :-
  http_open2(Uri, In, Options),
  call_cleanup(
    load_html(In, Dom, Options),
    close(In)
  ).
