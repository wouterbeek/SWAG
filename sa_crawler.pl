:- module(
  sa_crawler,
  [
    crawl_concrete/0,
    crawl_concrete/1, % +FirstNumber:integer
    save_concrete/0
  ]
).

/** <module> SACKNER ARCHIVE CRAWLER

Constructs a Semantic Web database based on the Sackner Archive data.

---+ Number of entries clawled.

47.374 entries, with codes between 00.000 and 50.122.

---+ Errors during the entiry clawl process.

ERROR: Socket error: Connection timed out

ERROR: url `http://ww2.rediscov.com/sacknerarchive/FULL/14424s.jpg#'
does not exist (status(404,Not Found))

ERROR: copy_stream_data/2: I/O error in read on stream
<stream>(0000000004FB0150) (Socket operation on non-socket)

@author Wouter Beek
@version 2013/04
*/

:- use_module(html(html)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(uri)).
:- use_module(library(xpath)).
:- use_module(math(math_ext)).
:- use_module(rdf(rdf_build)).
:- use_module(vocabularies('VoID')).



%% assert_schema is det.
% Asserts the RDF schema for the Sackner Archive.

assert_schema:-
  % Assert descriptive labels for the properties.
  forall(
    translate_name(_Legacy, Property1, Label),
    (
      rdf_global_id(cp:Property1, Property2),
      rdfs_assert_label(Property2, en, Label, 'CP_Entry')
    )
  ).

%% crawl_concrete is det.
% Crawls the Sackner Archive for all the entries that it contains.

crawl_concrete:-
  crawl_concrete(0).

%% crawl_concrete(+FirstNumber:integer) is det.
% Crawls the Sackner Archive as of the given entry number.
% This allows for easy continuing of crawls that break before being
% completed.

crawl_concrete(FirstNumber):-
  % 50122 is the last entry as of 2013/03/14.
  forall(
    between(FirstNumber, 50122, Number1),
    (
      format_integer(Number1, 5, Number2),
      crawl_concrete0(Number2)
    )
  ).

%% crawl_concrete0(+Number:atom) is det.
% Crawls the Sackner Archive for the specific entry with the given identifier.

crawl_concrete0(Number):-
  rdf_global_id(cp:Number, Entry),
  rdfs_assert_individual(Entry, cp:'Entry', cp),

  atomic_concat('413201333230~', Number, TemporaryNumber),
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
  uri_to_html(DescriptionURI, HTML),
  xpath_chk(HTML, //table, Table),
  xpath_chk(Table, //tbody, TBody),

  findall(
    Name/Value,
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
        strip([' '], UnstrippedValue, Value)
      ),
      once(translate_name(TemporaryName, Name))
    ),
    Pairs
  ),
  forall(
    member(Name/Value, Pairs),
    (
      rdf_global_id(cp:Name, Predicate),
      rdf_assert_datatype(Entry, Predicate, string, Value, cp)
    )
  ),

  xpath_chk(TBody, //tr(2)/td(2)/p, P),
  findall(
    ImageName,
    (
      xpath(P, input(@src), ImageSubpath1),
      downcase_atom(ImageSubpath1, ImageSubpath2),
      atom_concat('thumb\\', ImageName, ImageSubpath2),
      ImageName \== 'blank.jpg'
    ),
    ImageNames
  ),
  maplist(crawl_image(Entry), ImageNames).

%% crawl_image(+Entry:integer, -ImageName:atom) is det.
% Returns an image for the given entry in the Sackner Archive.
%
% The image is retrieved from the server of the Sackner Archive and is
% then stored locally.
%
% If an entry has more than one image, then subsequent runs of this
% predicate will return those additional images.
%
% If the entry has no more images, then this method succeeds without
% instantiating =|ImageName|=.

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
  void_save_library('CP').

%% translate_name(?Legacy:atom, ?Property:atom, ?Label:atom) is nondet.

translate_name('# Artist Proofs:',         number_of_artist_proofs, 'Number of artist proofs'     ).
translate_name('# Images:',                number_of_images,        'Number of images'            ).
translate_name('# Letter Art Proofs:',     number_of_art_proofs,    'Number of art proofs'        ).
translate_name('# Letter Copies:',         number_of_letter_copies, 'Number of letter copies'     ).
translate_name('Announcement:',            announcement,            'Announcement'                ).
translate_name('Annotation:',              annotation,              'Annotation'                  ).
translate_name('Author:',                  author,                  'Author'                      ).
translate_name('Catalog:',                 catalog,                 'Catalog'                     ).
translate_name('City County:',             city_country,            'City and/or country'         ).
translate_name('Classification:',          classification,          'Classification'              ).
translate_name('Container:',               container,               'Container'                   ).
translate_name('Contributors:',            contributor,             'Contributor'                 ).
translate_name('Exhibition Announcement:', exhibition_announcement, 'Exhibition announcement'     ).
translate_name('Exhibition Catalog:',      exhibition_catalog,      'Exhibition catalog'          ).
translate_name('Ht Wdt Dpth:',             dimensions,              'Dimensions'                  ).
translate_name('Illus BWC:',               illustration_bwc,        'Illustration BWC'            ).
translate_name('Inscribed:',               inscribed,               'Inscribed'                   ).
translate_name('Language:',                language,                'Language'                    ).
translate_name('Media:',                   media,                   'Media'                       ).
translate_name('Nationality:',             nationality,             'Nationality'                 ).
translate_name('Number of Dups:',          number_of_dups,          'Number of duplicates'        ).
translate_name('Nbr Ser Mn:',              number_series_month,     'Magazine number/series/month').
translate_name('Pages:',                   number_of_pages,         'Number of pages'             ).
translate_name('Periodical:',              periodical,              'Periodical'                  ).
translate_name('Publisher:',               publisher,               'Publisher'                   ).
translate_name('Purchase Year:',           purchase_year,           'Year of purchase'            ).
translate_name('Series:',                  series,                  'Series'                      ).
translate_name('Signature:',               signature,               'Signature'                   ).
translate_name('Sub Tit Au:',              subtitle_author,         'Subtitle author'             ).
translate_name('Subtitle:',                subtitle,                'Subtitle'                    ).
translate_name('Title:',                   title,                   'Title'                       ).
translate_name('Total Copies:',            number_of_copies,        'Number of copies'            ).
translate_name('Translator:',              translator,              'Translator'                  ).
translate_name('Volume:',                  volume,                  'Volume'                      ).
translate_name('Year:',                    year,                    'Year'                        ).
translate_name(Name, _):-
  gtrace, %DEB
  write(Name).

