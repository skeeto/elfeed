# Changes

## 3.4.1 (2021-02-25)

* Fix elfeed-search-last-entry and elfeed-search-first-entry

## 3.4.0 (2021-01-30)

* New search filter: ~ excludes matching feeds

* Support fetching feeds via file://

* Support fetching feeds via the Gopher protocol

* "End of entries" no longer appears in elfeed-search

* Support bookmarks in entries

* New customizations: elfeed-show-entry-switch, elfeed-show-entry-delete

* More extensive information from elfeed-curl-get-capabilities

* New elfeed-show command: elfeed-kill-link-url-at-point (c)

* Various other minor behavior improvements

## 3.3.0 (2019-11-23)

* New option: `elfeed-search-remain-on-entry`.

* More graceful handling of feed filters (=).

* Fix minor time handling issue with Emacs >= 26.1

* Load bookmarks before trying to use them.

## 3.2.0 (2019-08-24)

* Support for absolute date/time expressions in filters. See README.md
  for documentation and examples.

* curl's `--disable` is now default. To load your .curlrc file, use
  `--config` explicitly in `elfeed-curl-extra-arguments`.

* Re-enable curl's HTTP/2 support.

* Function `elfeed-next-link` was renamed to `elfeed-show-next-link`.

* New search buffer bindings: <, >, h, c

* Multiple authors are now parsed from entries. Reflecting this, the
  meta key for authors is now `:authors` instead of `:author`. The
  value is always a list of zero or more authors.

* New variable: `elfeed-show-unique-buffers`. Allows for displaying
  multiple show buffers at the same time.

* Various minor fixes and improvements.

## 3.1.0 (2018-08-29)

* Add `elfeed-show-enclosure-filename-function` for controlling
  enclosure filenames.

* Dynamically enable/disable --compressed curl option. On some systems
  curl is built without this option, so it causes errors when it is
  used.

* Minor documentation fixes.

## 3.0.0 (2018-05-13)

* Under Emacs 26, there is a new database index format (version 4).

Emacs 26 introduces a new "record" type, and cl-defstruct objects used
by Elfeed as its database format are now based on this type. This
unfortunately changes (and breaks) Elfeed's index format. Prior to this
release, Emacs 26 could not open an Emacs 24–25 index and vice versa.

As of Elfeed 2.4.0, Elfeed running under Emacs 26 will automatically and
quietly upgrade an Emacs 25 database index for Emacs 26. **THIS UPGRADE
IS IRREVERSIBLE** and the database can no longer be used with Emacs 25.
A one-time backup copy ("index.backup") of the original Emacs 25 index
is created before performing the upgrade. If Emacs 25, whether running
this or any prior Elfeed release, loads the new database format, it will
see an empty database as if starting from scratch.

## 2.3.1 (2018-05-13)

* The index is now saved when quitting the elfeed-search window ("q").
* `elfeed-link` is now autoloaded.

## 2.3.0 (2018-01-21)

* New `=<feed-matcher>` syntax in search filters
* Support for protocol-relative entry links (i.e. //example.com/foo/)
* New `elfeed-add-feed` `:save` key argument
* New plist-based parsed search filter format (breaking change)
* New hook: `elfeed-search-update-hook`
* New hook: `elfeed-db-unload-hook`
* New variable: `elfeed-search-sort-function`
* Connect curl with a pipe instead of a pty—a performance boost
* Minor bug fixes

## 2.2.0 (2017-07-09)

* Support for org links (`elfeed-link.el`)
* Added `elfeed-db-unload`
* New `elfeed-curl-retrieve` interface (breaking changes)
* New hooks `elfeed-tag-hooks` and `elfeed-untag-hooks`

## 2.1.1 (2017-04-02)

* Added `elfeed-show-entry-author` customization variable.
* Added `elfeed-search-unparse-filter`

## 2.1.0 (2017-01-25)

* New entry ID based only on domain, not whole feed
* Byte-compiled search filters (`elfeed-search-compile-filter`)
* Improved metadata persistence on entry updates
* Gather `:author` from entries
* Gather `:categories` from entries
* New `elfeed-add-feed` interface (thanks Mark Oteiza)
* New xml-query macros for faster feed parsing

## 2.0.1 (2016-10-30)

* Added `elfeed-curl-extra-arguments` customization
* Use `x-get-selection` instead of `x-get-selection-value`
* More flexible date handling (including Atom 0.3 support)
* Various elfeed-web fixes

## 2.0.0 (2016-08-26)

* Elfeed now uses cURL when available (`elfeed-use-curl`)
* Windows OS now supported when using cURL
* Conditional GET (ETag, `If-Modified-Since`) when using cURL
* Support for xml:base in Atom feeds
* New options: `elfeed-set-max-connections`, `elfeed-set-timeout`
* New feed metadata: :canonical-url, :etag, :last-modified
* New variable: `elfeed-log-level`
* New database export option: `elfeed-csv-export`
* Additional validation for `elfeed-feeds`

## 1.4.1 (2016-05-25)

* Major bug fix: disable local variables when loading the index
* New command `elfeed-show-play-enclosure` (requires emms)
* Yank now works on regions in the search buffer
* Feed structs now have author field filled out
* New command `elfeed-search-set-feed-title`
* New command `elfeed-search-set-entry-title`
* Smarter handling of invalid timestamps
* Following links in show mode (`elfeed-show-visit`) takes a prefix arg

## 1.4.0 (2015-12-22)

* New header built on Emacs' built-in buffer headers
* New hook: `elfeed-new-entry-parse-hook`
* Emacs' bookmark support (`bookmark-set`, `bookmark-jump`)
* Emacs' desktop support (save/restore windows)
* Custom faces in search listing via `elfeed-search-face-alist`
* Dedicated log buffer, *elfeed-log*
* Scoped updates with prefix argument to `elfeed-search-fetch`
* Various bug fixes
* Fixes to feed Unicode decoding

## 1.3.0 (2015-11-20)

* `elfeed-search-face-alist` for custom entry faces
* `display-local-help` (C-h .) support in search
* Fixes to #n count filter

## 1.2.0 (2015-10-05)

* Switched to url-queue (see `url-queue-timeout`)
* New #n filter for limiting results to first n entries
* Faster live filtering
* `elfeed-version`
* Enclosure downloading
* Database size optimizations
* Search listing is more responsive to updates
* `elfeed-http-error-hooks`, `elfeed-parse-error-hooks`
* Various bug fixes

## 1.1.2 (2014-11-04)

* Fixed support for non-HTTP protocols
* Add ! search syntax
* Add elfeed-unjam
* Combine regexp search terms by AND instead of OR
* Link navigation keybindings (tab)
* Add elfeed-show-truncate-long-urls
* Add elfeed-search-filter customization
* Various bug fixes

## 1.1.1 (2014-06-14)

* Fix database corruption issue
* Properly handle URLs from XML
* Slightly better RSS date guessing
* User interface tweaks
* Add `elfeed-sort-order`
* Use tab and backtab to move between links

## 1.1.0 (2014-01-27)

* Autotagging support
* Better database performance
* Database packing
* Arbitrary struct metadata
* Added `elfeed-search-clipboard-type`
* Update to cl-lib from cl
* Lots of bug fixes

## 1.0.1 (2013-09-08)

* Live filter editing
* Support for RSS 1.0
* OPML import/export
* Fix multibyte support (thanks cellscape)
* Fix date-change database corruption
* Add n and p bindings to elfeed-search, like notmuch
* Friendlier intro header
* Automated builds
* Lots of small bug fixes

## 1.0.0 (2013-09-02)

* Initial release
