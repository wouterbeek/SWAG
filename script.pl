:- encoding(utf8).
/* etl-sackner-archive

Constructs a Semantic Web database based on the Sackner Archive data,
by scraping an online Web site.

---

@author Wouter Beek
@version 2013-2018
*/

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(sgml)).
:- use_module(library(xpath)).
:- use_module(library(yall)).

:- use_module(library(atom_ext)).
:- use_module(library(file_ext)).
:- use_module(library(http/http_client2)).
:- use_module(library(semweb/rdf_export)).
:- use_module(library(semweb/rdf_mem)).
:- use_module(library(semweb/rdf_prefix)).
:- use_module(library(semweb/rdf_term)).
:- use_module(library(tapir/tapir_api)).
:- use_module(library(uri_ext)).

%:- curl.

:- debug(known_issue).

:- discontiguous
    property_name_/2,
    property_name_/3.

:- maplist(rdf_register_prefix, [
     dcterm,
     graph-'https://demo.triply.cc/wouter/sackner-archive/graph/',
     sa-'https://demo.triply.cc/wouter/sackner-archive/def/',
     schema,
     work-'https://demo.triply.cc/wouter/sackner-archive/id/work/'
   ]).

:- rdf_meta
   property_name(+, r, r),
   property_name_(+, r),
   property_name_(+, r, r),
   scrape_entries(+, r).





etl :-
  rdf_equal(G, graph:data),
  scrape_site(0, G),
  upload(G).



%! scrape_site(+Offset:nonneg, +G:rdf_graph) is det.
%
% The Offset argument allows for easy continuing of crawls that broke
% before being completed.
%
% The last entry has identifier 50,122, as of 2013/03/14.

scrape_site(Offset, G) :-
  must_be(nonneg, Offset),
  between(Offset, inf, N),
  (scrape_entry(N, G) -> fail ; !).

scrape_entry(N, G) :-
  atom_number(Id, N),
  % The entry identifier must be padded with zeros.
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
  findall(Row, table_row(Table, Row), Rows),
  % Do not create a subject term for empty records.
  (   Rows == []
  ->  debug(known_issue, "Skipping empty entry № ~D.", [N])
  ;   rdf_prefix_iri(work, Id, S),
      rdf_assert_triple(S, rdf:type, schema:'CreativeWork', G),
      rdf_assert_triple(S, sa:source_id, nonneg(N), G),
      % Add the properties.
      forall(
        member(row(P,D,Lex), Rows),
        rdf_assert_triple(S, P, literal(type(D,Lex)), G)
      ),
      % Add the images, if any.
      xpath_chk(Table, //tr(2)/td(2)/p, P),
      forall(
        (
          xpath(P, input(@src(lower)), ImageSubpath),
          atom_concat('thumb\\', ImageName, ImageSubpath)
        ),
        download_nonblank_image(N, S, ImageName, G)
      )
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
  rdf_assert_triple(S, P, literal(type(D,Lex)), G).



%! download_image(+S:iri, +Local:atom, +Graph:rdf_graph) is det.
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

download_image(S, Local, G) :-
  uri_comps(
    FromUri,
    uri(http,'ww3.rediscov.com',[sacknerarchives,'FULL',Local],_,_)
  ),
  directory_file_path(img, Local, File),
  uri_comps(
    ToUri,
    uri(https,'demo.triply.cc', [wouter,'sackner-archives',assets,Local], _, _)
  ),
  http_download(FromUri, File),
  rdf_assert_triple(S, foaf:depiction, uri(ToUri), G).



%! download_nonblank_image(+N:nonneg, +S:rdf_subject, +ImageName:atom, +G:rdf_graph) is det.

download_nonblank_image(N, _, 'blank.jpg', _) :- !,
  debug(known_issue, "Skipping blank image for record № ~D.", [N]).
download_nonblank_image(_, S, ImageName, G) :-
  download_image(S, ImageName, G).



%! property_name(+Name:atom, -P:iri, -D:iri) is det.

property_name(Name, P, D) :-
  property_name_(Name, P, D), !.
property_name(Name, P, xsd:string) :-
  property_name_(Name, P), !.
property_name(Name, _, _) :-
  (var(Name) -> instantiation_error(Name) ; syntax_error(Name)).

property_name_('# Artist Proofs:',         sa:number_of_artist_proofs, xsd:nonNegativeInteger).
property_name_('# Images:',                sa:number_of_images, xsd:nonNegativeInteger).
property_name_('# Letter Art Proofs:',     sa:number_of_art_proofs, xsd:nonNegativeInteger).
property_name_('# Letter Copies:',         sa:number_of_letter_copies, xsd:nonNegativeInteger).
property_name_('Announcement:',            sa:announcement).
property_name_('Annotation:',              sa:annotation).
property_name_('Author:',                  dcterm:creator).
property_name_('Catalog:',                 sa:catalog).
property_name_('City County:',             sa:city_country).
property_name_('Classification:',          sa:classification).
property_name_('Container:',               sa:container).
property_name_('Contributors:',            sa:contributor).
property_name_('Exhibition Announcement:', sa:exhibition_announcement).
property_name_('Exhibition Catalog:',      sa:exhibition_catalog).
property_name_('Ht Wdt Dpth:',             sa:dimensions).
property_name_('Illus BWC:',               sa:illustration_bwc).
property_name_('Inscribed:',               sa:inscribed).
property_name_('Language:',                sa:language).
property_name_('Media:',                   sa:media).
property_name_('Nationality:',             sa:nationality).
property_name_('Number of Dups:',          sa:number_of_dups, xsd:decimal).
property_name_('Nbr Ser Mn:',              sa:number_series_month).
property_name_('Pages:',                   sa:number_of_pages, xsd:nonNegativeInteger).
property_name_('Periodical:',              sa:periodical).
property_name_('Publisher:',               dcterm:publisher).
property_name_('Purchase Year:',           sa:purchase_year, xsd:gYear).
property_name_('Series:',                  sa:series).
property_name_('Signature:',               sa:signature).
property_name_('Sub Tit Au:',              sa:subtitle_author).
property_name_('Subtitle:',                sa:subtitle).
property_name_('Title:',                   sa:title).
property_name_('Total Copies:',            sa:number_of_copies, xsd:nonNegativeInteger).
property_name_('Translator:',              sa:translator).
property_name_('Volume:',                  sa:volume).
property_name_('Year:',                    dcterm:created, xsd:gYear).



%! upload(+G:rdf_graph) is det.

upload(G) :-
  DataFile = 'data.nq.gz',
  write_to_file(DataFile, rdf_write_quads(G)),
  expand_file_name('img/*.jpg', ImageFiles),
  Properties = _{
    accessLevel: public,
    assets: ImageFiles,
    avatar: 'avatar.gif',
    description: "Ruth and Marvin Sackner founded the Archive in Miami Beach, Florida in 1979, later moving it to Miami, Florida in 2005.  Its initial mission was to establish a collection of books, critical texts, periodicals, ephemera, prints, drawings, collages, paintings, sculptures, objects, manuscripts, and correspondence dealing with precedent and contemporary, internationally produced, concrete and visual poetry.  The antecedent material had at its starting point, Stephane Mallarme’s poem, “Un Coup de Des” (Cosmopolis, 1897).  The historic examples included works with concrete/visual poetic sensibilities from such twentieth century art movements as Italian Futurism, Russian and Eastern European Avant Garde, Dada, Surrealism, Bauhaus, De Stijl, Ultra, Tabu-Dada, Lettrisme, and Ultra-Lettrisme.",
    files: [DataFile,'vocab.trig'],
    prefixes: [dcterm,graph,sa,schema,work]
  },
  dataset_upload(demo, wouter, 'sackner-archive', Properties),
  delete_file(DataFile).





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
