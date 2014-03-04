:- module(
  sa_scrape,
  [
    sa_scrape/1 % +Graph:atom
  ]
).

/** <module> Sackner Archive Scraper

Constructs a Semantic Web database based on the Sackner Archive data,
 by scraping an online Web site.

# Number of entries clawled.

47.374 entries, with codes between 00.000 and 50.122.

# Errors during the entiry clawl process.

ERROR: Socket error: Connection timed out

ERROR: url `http://ww2.rediscov.com/sacknerarchive/FULL/14424s.jpg#'
does not exist (status(404,Not Found))

ERROR: copy_stream_data/2: I/O error in read on stream
<stream>(0000000004FB0150) (Socket operation on non-socket)

@author Wouter Beek
@version 2013/04, 2014/03
*/

:- use_module(generics(atom_ext)).
:- use_module(html(html)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(uri)).
:- use_module(library(xpath)).
:- use_module(math(math_ext)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdfs(rdfs_label_build)).
:- use_module(vocabularies('VoID')).



%! sa_scrape(+Graph:atom) is det.
% Crawls the Sackner Archive for all the entries that it contains.

sa_scrape(Graph):-
  assert_schema(Graph),
  sa_scrape(Graph, 0).

%! sa_scrape(+Graph:atom, +FirstEntry:nonneg) is det.
% Crawls the Sackner Archive as of the given entry number.
% This allows for easy continuing of crawls
%  that broke before being completed.

sa_scrape(Graph, FirstNumber):-
  rdfs_assert_subclass(sa:'Entry', rdfs:'Resource', Graph),
  rdfs_assert_label(sa:'Entry', 'Entry in the Sackner Archive', Graph),
  
  rdfs_assert_subclass(sa:'Property', rdf:'Property', Graph),
  rdfs_assert_label(sa:'Property', 'Property used in the Sackner Archive', Graph),
  
  % The last entry has identifier 50,122, as of 2013/03/14.
  forall(
    between(FirstEntry, 100000, Entry),
    sa_scrape_entry(Graph, Entry)
  ).


%! assert_schema(+Graph:atom) is det.
% Asserts the RDFS schema for the Sackner Archive dataset.

assert_schema(Graph):-
  % Assert descriptive labels for the properties.
  forall(
    sa_predicate_term(_, PropertyName1, RdfsLabel),
    (
      atomic_list_concat(['Property',PropertyName1], '/', PropertyName2),
      rdf_global_id(sa:PropertyName2, Property),
      rdfs_assert_label(Property, en, RdfsLabel, Graph)
    )
  ).


%% sa_scrape_entry(+Graph:atom, +Entry:nonneg) is det.
% Scrapes the Sackner Archive for the specific entry
%  with the given identifier.

sa_scrape_entry(Graph, EntryId1):-
  % The entry identifier has to be padded with zeros.
  format_integer(EntryId1, 5, EntryId2),
  
  rdf_create_next_resource(cp, 'Entry', Entry, Graph),
  rdf_assert_datatype(Entry, sa:original_id, xsd:integer, EntryId, Graph),
  
  atomic_concat('413201333230~', Entry2, TemporaryNumber),
  uri_components(
    DescriptionURI,
    uri_components(
      http,
      'ww2.rediscov.com',
      '/sacknerarchive/ShowItem.aspx',
      TemporaryNumber,
      ''
    )
  ),
  url_to_html(DescriptionURI, HTML),
  xpath_chk(HTML, //table, Table),
  xpath_chk(Table, //tbody, TBody),

  findall(
    PredicateName-Value,
    (
      xpath(TBody, //tr, Row),
      xpath_chk(Row, //td(1, normalize_space), TemporaryName),
      xpath_chk(Row, //td(2)/p, P),
      (
        xpath_chk(P, //input(@value), Value)
      ;
        xpath_chk(P, //textarea(normalize_space), TemporaryValue),
        % Some values are enumarations separated by dashes.
        split_atom_exclusive(TemporaryValue, [' --'], Values),
        member(UnstrippedValue, Values),
        % Some values have superfluous spaces pre- and/or postfixed.
        strip_atom([' '], UnstrippedValue, Value)
      ),
      once(sa_predicate_term(TemporaryName, PredicateName))
    ),
    Pairs
  ),
  forall(
    member(PredicateName-Value, Pairs),
    (
      rdf_global_id(sa:PredicateName, Predicate),
      rdf_assert_datatype(Entry, Predicate, xsd:string, Value, Graph)
    )
  ),
  
  xpath_chk(TBody, //tr(2)/td(2)/p, P),
  findall(
    ImageName,
    (
      xpath(P, input(@src), ImageSubpath1),
      downcase_atom(ImageSubpath1, ImageSubpath2),
      atom_concat('thumb\\', ImageName, ImageSubpath2)
      %ImageName \== 'blank.jpg'
    ),
    ImageNames
  ),
  maplist(crawl_image(Entry), ImageNames).


%% crawl_image(+Entry:integer, -ImageName:atom) is det.
% Returns an image for the given entry in the Sackner Archive.
%
% The image is retrieved from the server of the Sackner Archive and is
%  then stored locally.
%
% If an entry has more than one image, then subsequent runs of this
%  predicate will return those additional images.
%
% If the entry has no more images, then this method succeeds without
%  instantiating =|ImageName|=.

crawl_image(Entry, ImageName):-
  atomic_concat('/sacknerarchive/FULL/', ImageName, ImagePath),
  uri_components(
    ImageURI,
    uri_components(
      http,
      'ww2.rediscov.com',
      ImagePath,
      '',
      ''
    )
  ),
  absolute_file_name(
    project(ImageName),
    ImageFile,
    [access(write), file_type(jpg)]
  ),
  open(ImageFile, write, Out, [type(binary)]),
  catch(
    http_open(ImageURI, In, []),
    _ExistenceError,
    fail
  ),
  !,
  copy_stream_data(In, Out),
  close(In),
  close(Out),
  rdf_assert_datatype(Entry, cp:image, string, ImageFile, cp).
crawl_image(_Entry, _ImageName).

save_concrete:-
  void_save('CP').

%% sa_predicate_term(?Legacy:atom, ?Property:atom, ?Label:atom) is nondet.

sa_predicate_term('# Artist Proofs:',         number_of_artist_proofs, 'Number of artist proofs'     ).
sa_predicate_term('# Images:',                number_of_images,        'Number of images'            ).
sa_predicate_term('# Letter Art Proofs:',     number_of_art_proofs,    'Number of art proofs'        ).
sa_predicate_term('# Letter Copies:',         number_of_letter_copies, 'Number of letter copies'     ).
sa_predicate_term('Announcement:',            announcement,            'Announcement'                ).
sa_predicate_term('Annotation:',              annotation,              'Annotation'                  ).
sa_predicate_term('Author:',                  author,                  'Author'                      ).
sa_predicate_term('Catalog:',                 catalog,                 'Catalog'                     ).
sa_predicate_term('City County:',             city_country,            'City and/or country'         ).
sa_predicate_term('Classification:',          classification,          'Classification'              ).
sa_predicate_term('Container:',               container,               'Container'                   ).
sa_predicate_term('Contributors:',            contributor,             'Contributor'                 ).
sa_predicate_term('Exhibition Announcement:', exhibition_announcement, 'Exhibition announcement'     ).
sa_predicate_term('Exhibition Catalog:',      exhibition_catalog,      'Exhibition catalog'          ).
sa_predicate_term('Ht Wdt Dpth:',             dimensions,              'Dimensions'                  ).
sa_predicate_term('Illus BWC:',               illustration_bwc,        'Illustration BWC'            ).
sa_predicate_term('Inscribed:',               inscribed,               'Inscribed'                   ).
sa_predicate_term('Language:',                language,                'Language'                    ).
sa_predicate_term('Media:',                   media,                   'Media'                       ).
sa_predicate_term('Nationality:',             nationality,             'Nationality'                 ).
sa_predicate_term('Number of Dups:',          number_of_dups,          'Number of duplicates'        ).
sa_predicate_term('Nbr Ser Mn:',              number_series_month,     'Magazine number/series/month').
sa_predicate_term('Pages:',                   number_of_pages,         'Number of pages'             ).
sa_predicate_term('Periodical:',              periodical,              'Periodical'                  ).
sa_predicate_term('Publisher:',               publisher,               'Publisher'                   ).
sa_predicate_term('Purchase Year:',           purchase_year,           'Year of purchase'            ).
sa_predicate_term('Series:',                  series,                  'Series'                      ).
sa_predicate_term('Signature:',               signature,               'Signature'                   ).
sa_predicate_term('Sub Tit Au:',              subtitle_author,         'Subtitle author'             ).
sa_predicate_term('Subtitle:',                subtitle,                'Subtitle'                    ).
sa_predicate_term('Title:',                   title,                   'Title'                       ).
sa_predicate_term('Total Copies:',            number_of_copies,        'Number of copies'            ).
sa_predicate_term('Translator:',              translator,              'Translator'                  ).
sa_predicate_term('Volume:',                  volume,                  'Volume'                      ).
sa_predicate_term('Year:',                    year,                    'Year'                        ).
sa_predicate_term(Name, _):-
  gtrace, %DEB
  write(Name).

