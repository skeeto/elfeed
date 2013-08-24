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
