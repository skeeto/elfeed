# Elfeed Emacs Web Feed Reader

Elfeed is a web feed reader for Emacs, inspired by
[notmuch](http://notmuchmail.org/). It supports both Atom and RSS and
is currently in early development.

Requires Emacs 24 (lexical closures).

## Getting Started

Add your feeds to the `elfeed-feeds` list and populate the database
with entries using `M-x elfeed-update` and view the entries list with
`M-x elfeed`.

In the elfeed buffer use `b` to launch the item under the point in the
browser. If the region is active, all entries in the region will be
acted upon. Currently there is no way to view an entry within Emacs,
but this is planned for the near future.

## Status and Roadmap

I've already got it to a point where it can serve about 90% of my web
feed needs. Of course, I'd like to flesh out a lot more so that it's
more generally useful. Except for a single RDF feed, it can properly
parse my entire 136-entry feed list.

Some things I want to add:

 * Reading entries within Emacs (like e-mail)
 * Track entry read/unread state
 * Persist the database between Emacs instances
 * More extensible API
 * Enclosure support
 * Searching (maybe)
 * Optional external database back-end (Xapian?, maybe)

## Motivation

As far as I know, outside of Elfeed there does not currently exist an
extensible, text-file configured, power use web feed client that can
handle large numbers of feeds. The existing clients I've tried are
missing some important capability that limits its usefulness to me.
