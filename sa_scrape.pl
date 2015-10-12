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

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(apply)).
:- use_module(library(atom_ext)).
:- use_module(library(debug)).
:- use_module(library(filesex)).
:- use_module(library(http/http_download)).
:- use_module(library(lists)).
:- use_module(library(rdf/rdf_build)).
:- use_module(library(rdf/rdf_image)).
:- use_module(library(rdfs/rdfs_build)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).
:- use_module(library(xpath)).





%! sa_scrape(+Graph:atom) is det.
% Crawls the Sackner Archive for all the entries that it contains.

sa_scrape(G):-
  sa_assert_schema(G),
  sa_scrape(G, 0).

%! sa_scrape(+Graph:atom, +FirstEntry:nonneg) is det.
% Crawls the Sackner Archive as of the given entry number.
%
% Argument `FirstEntry` allows for easy continuing of crawls
% that broke before being completed.

sa_scrape(G, N):-
  % swag:Entry
  rdfs_assert_subclass(swag:'Entry', G),
  rdfs_assert_label(swag:'Entry', 'Entry in the Sackner Archive', G),

  % swag:Property
  rdfs_assert_subclass(swag:'Property', rdf:'Property', G),
  rdfs_assert_label(
    swag:'Property',
    'Property used in the Sackner Archive',
    G
  ),

  % The last entry has identifier 50,122, as of 2013/03/14.
  sa_scrape_entries(G, N).

sa_scrape_entries(G, N):-
  (   sa_scrape_entry(G, N)
  ->  NewN is N + 1,
      sa_scrape_entries(G, NewN)
  ;   debug(sa(scrape), "SA scrape is done.", [])
  ).

%! sa_scrape_entry(+Graph:atom, +Entry:nonneg) is det.
% Scrapes the Sackner Archive for the specific entry
% with the given identifier.

sa_scrape_entry(G, N1):-
  % The entry identifier has to be padded with zeros.
  format_integer(N1, 5, N2),
  atomic_concat('413201333230~', N2, TmpN),
  uri_components(
    Iri,
    uri_components(
      http,
      'ww3.rediscov.com',
      '/sacknerarchives/ShowItem.aspx',
      TmpN,
      ''
    )
  ),
  html_download(Iri, Dom, [html_dialect(html4)]),

  xpath_chk(Dom, //table, Table),

  findall(PName-Val, sa_nvpair(Table, PName, Val), Pairs),
  
  % Only create an entry if there are some properties.
  length(Pairs, NumberOfProperties),
  (   NumberOfProperties =:= 0
  ->  debug(sa(scrape), "SKIPPING entry ~D.", [N1])
  ;   % Create a SWAG entry resource.
      fresh_iri(swag, ['Entry'], Entry),
      rdf_assert_instance(Entry, swag:'Entry', G),
      rdf_assert_literal(Entry, swag:original_id, xsd:integer, N1, G),
      
      % Add the properties.
      maplist(sa_assert_triple(G, Entry), Pairs),
      
      % Add the images, if any.
      xpath_chk(Table, //tr(2)/td(2)/p, P),
      findall(
        ImgName,
        (
          xpath(P, input(@src), ImgSubpath1),
          downcase_atom(ImgSubpath1, ImgSubpath2),
          atom_concat('thumb\\', ImgName, ImgSubpath2),
          ImgName \== 'blank.jpg'
        ),
        ImgNames
      ),
      maplist(crawl_image(G, Entry), ImgNames),
      debug(sa(scrape), "Scraped entry ~D.", [N1])
  ).


sa_nvpair(Table, PName2, Val2):-
  xpath(Table, //tr, Row),
  xpath_chk(Row, //td(1)/span(normalize_space), PName1),
  PName1 \== '',
  xpath_chk(Row, //td(2)/p, P),
  (   xpath_chk(P, //input(@value), Vals1)
  ;   xpath_chk(P, //textarea(normalize_space), Vals1)
  ),

  % Some values are enumarations separated by dashes.
  atomic_list_concat(Vals2, ' --', Vals1),
  member(Val1, Vals2),

  % Some values have superfluous spaces pre- and/or postfixed.
  strip_atom([' '], Val1, Val2),
  once(sa_predicate_term(PName1, PName2, _)).


sa_assert_schema(G):-
  % Assert descriptive labels for the properties.
  forall(
    sa_predicate_term(_, PName1, Label),
    (
      atomic_list_concat(['Property',PName1], /, PName2),
      rdf_global_id(swag:PName2, P),
      rdfs_assert_label(P, Label-'en-US', G)
    )
  ),

  % Assert the domain and range restrictions of the properties.
  rdfs_assert_domain(swag:number_of_artist_proofs, swag:'Entry', G),
  rdfs_assert_range( swag:number_of_artist_proofs, xsd:integer,  G),
  rdfs_assert_domain(swag:number_of_images,        swag:'Entry', G),
  rdfs_assert_range( swag:number_of_images,        xsd:integer,  G),
  rdfs_assert_domain(swag:number_of_art_proofs,    swag:'Entry', G),
  rdfs_assert_range( swag:number_of_art_proofs,    xsd:integer,  G),
  rdfs_assert_domain(swag:number_of_letter_copies, swag:'Entry', G),
  rdfs_assert_range( swag:number_of_letter_copies, xsd:integer,  G),
  rdfs_assert_domain(swag:announcement,            swag:'Entry', G),
  rdfs_assert_range( swag:announcement,            xsd:string,   G),
  rdfs_assert_domain(swag:annotation,              swag:'Entry', G),
  rdfs_assert_range( swag:annotation,              xsd:string,   G),
  rdfs_assert_domain(swag:author,                  swag:'Entry', G),
  rdfs_assert_range( swag:author,                  xsd:string,   G),
  rdfs_assert_domain(swag:catalog,                 swag:'Entry', G),
  rdfs_assert_range( swag:catalog,                 xsd:string,   G),
  rdfs_assert_domain(swag:city_country,            swag:'Entry', G),
  rdfs_assert_range( swag:city_country,            xsd:string,   G),
  rdfs_assert_domain(swag:classification,          swag:'Entry', G),
  rdfs_assert_range( swag:classification,          xsd:string,   G),
  rdfs_assert_domain(swag:container,               swag:'Entry', G),
  rdfs_assert_range( swag:container,               xsd:string,   G),
  rdfs_assert_domain(swag:contributor,             swag:'Entry', G),
  rdfs_assert_range( swag:contributor,             xsd:string,   G),
  rdfs_assert_domain(swag:exhibition_announcement, swag:'Entry', G),
  rdfs_assert_range( swag:exhibition_announcement, xsd:string,   G),
  rdfs_assert_domain(swag:exhibition_catalog,      swag:'Entry', G),
  rdfs_assert_range( swag:exhibition_catalog,      xsd:string,   G),
  rdfs_assert_domain(swag:dimensions,              swag:'Entry', G),
  rdfs_assert_range( swag:dimensions,              xsd:string,   G),
  rdfs_assert_domain(swag:illustration_bwc,        swag:'Entry', G),
  rdfs_assert_range( swag:illustration_bwc,        xsd:string,   G),
  rdfs_assert_domain(swag:inscribed,               swag:'Entry', G),
  rdfs_assert_range( swag:inscribed,               xsd:string,   G),
  rdfs_assert_domain(swag:language,                swag:'Entry', G),
  rdfs_assert_range( swag:language,                xsd:string,   G),
  rdfs_assert_domain(swag:media,                   swag:'Entry', G),
  rdfs_assert_range( swag:media,                   xsd:string,   G),
  rdfs_assert_domain(swag:nationality,             swag:'Entry', G),
  rdfs_assert_range( swag:nationality,             xsd:string,   G),
  rdfs_assert_domain(swag:number_of_dups,          swag:'Entry', G),
  rdfs_assert_range( swag:number_of_dups,          xsd:decimal,  G),
  rdfs_assert_domain(swag:number_series_month,     swag:'Entry', G),
  rdfs_assert_range( swag:number_series_month,     xsd:string,   G),
  rdfs_assert_domain(swag:number_of_pages,         swag:'Entry', G),
  rdfs_assert_range( swag:number_of_pages,         xsd:integer,  G),
  rdfs_assert_domain(swag:periodical,              swag:'Entry', G),
  rdfs_assert_range( swag:periodical,              xsd:string,   G),
  rdfs_assert_domain(swag:publisher,               swag:'Entry', G),
  rdfs_assert_range( swag:publisher,               xsd:string,   G),
  rdfs_assert_domain(swag:purchase_year,           swag:'Entry', G),
  rdfs_assert_range( swag:purchase_year,           xsd:gYear,    G),
  rdfs_assert_domain(swag:series,                  swag:'Entry', G),
  rdfs_assert_range( swag:series,                  xsd:string,   G),
  rdfs_assert_domain(swag:signature,               swag:'Entry', G),
  rdfs_assert_range( swag:signature,               xsd:string,   G),
  rdfs_assert_domain(swag:subtitle_author,         swag:'Entry', G),
  rdfs_assert_range( swag:subtitle_author,         xsd:string,   G),
  rdfs_assert_domain(swag:subtitle,                swag:'Entry', G),
  rdfs_assert_range( swag:subtitle,                xsd:string,   G),
  rdfs_assert_domain(swag:title,                   swag:'Entry', G),
  rdfs_assert_range( swag:title,                   xsd:string,   G),
  rdfs_assert_domain(swag:number_of_copies,        swag:'Entry', G),
  rdfs_assert_range( swag:number_of_copies,        xsd:integer,  G),
  rdfs_assert_domain(swag:translator,              swag:'Entry', G),
  rdfs_assert_range( swag:translator,              xsd:string,   G),
  rdfs_assert_domain(swag:volume,                  swag:'Entry', G),
  rdfs_assert_range( swag:volume,                  xsd:string,   G),
  rdfs_assert_domain(swag:year,                    swag:'Entry', G),
  rdfs_assert_range( swag:year,                    xsd:gYear,    G).


sa_assert_triple(G, Entry, PName-Val):-
  rdf_global_id(swag:PName, P),
  rdf_assert_literal(Entry, P, xsd:string, Val, G).


%! crawl_image(+Graph:atom, +Entry:nonneg, -ImageName:atom) is det.
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

crawl_image(G, Entry, Name):-
  atomic_concat('/sacknerarchives/FULL/', Name, Path),
  uri_components(Iri, uri_components(http,'ww3.rediscov.com',Path,_,_)),
  absolute_file_name(resource/img, Dir, [access(write),file_type(directory)]),
  directory_file_path(Dir, Name, File),
  file_download(Iri, File),
  rdf_assert_literal(Entry, swag:image, xsd:string, Name, G), !.
crawl_image(G, Entry, Name):-
  gtrace, %DEB
  crawl_image(G, Entry, Name).


%! sa_predicate_term(?Legacy:atom, ?Property:atom, ?Label:atom) is nondet.

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
sa_predicate_term(Name, _, _):-
  nonvar(Name),
  gtrace, %DEB
  format(user_output, '~a', [Name]).
