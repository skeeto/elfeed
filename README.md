# Elfeed Emacs Web Feed Reader

Elfeed is a web feed reader for Emacs, inspired by
[notmuch](http://notmuchmail.org/). It supports both Atom and RSS and
is currently in beta. Elfeed requires Emacs 24 (lexical closures).

The database is not yet persisted between Emacs instances, so don't
spend too much time manually tagging entries.

[![](http://i.imgur.com/8eTghK8.png)](http://i.imgur.com/l5aVPDc.png)

## Getting Started

It is recommended that you make a global binding for `elfeed`.

```el
(global-set-key (kbd "C-x w") 'elfeed)
```

Running the interactive function `elfeed` will pop up the
`*elfeed-search*` buffer, which will display feed items.

 * <kbd>g</kbd>: refresh view of the feed listing
 * <kbd>G</kbd>: fetch feed updates from the servers
 * <kbd>s</kbd>: update the search filter (see tags)

This buffer will be empty until you add your feeds to the
`elfeed-feeds` list and initiate an update with `M-x elfeed-update`
(or <kbd>G</kbd> in the Elfeed buffer). This will populate the Elfeed
database with entries.

```el
;; Somewhere in your .emacs file
(setq elfeed-feeds
      '("http://nullprogram.com/feeds/"
        "http://www.terminally-incoherent.com/blog/feed/"))
```

From this buffer there are a number of ways to interact with entries.
Entries are selected by placing the point over an entry. Multiple
entries are selected at once by using an active region.

 * <kbd>b</kbd>: open selected entries in your browser (`browse-url`)
 * <kbd>y</kbd>: copy selected entries URL to the clipboard
 * <kbd>r</kbd>: mark selected entries as read
 * <kbd>u</kbd>: mark selected entries as unread
 * <kbd>+</kbd>: add a specific tag to selected entries
 * <kbd>-</kbd>: remove a specific tag from selected entries

Currently there is no way to view an entry within Emacs, but this is
planned for the near future.

## Tags

Elfeed maintains a list of arbitrary tags -- symbols attached to an
entry. The tag `unread` is treated specially by default, with unread
entries appearing in bold.

To make tags useful, the Elfeed entry listing buffer can be filtered
by tags. Use `elfeed-search-filter-read` (or <kbd>s</kbd>) to update
the filter. Tags beginning with a `+` are required and tags beginning
with a `-` must not be present. Here are some examples:

 * `+unread`: only show unread entries
 * `-unread +youtube`: only show previously-read YouTube entries

The latter assumes you've tagged posts with `youtube`. You probably
want to do this sort of thing automatically, which can be done with
the `elfeed-new-entry-hook`. Functions in this hook are called with
new entries, allowing them to be manipulated, such as adding tags.

```el
;; Mark all YouTube entries
(add-hook 'elfeed-new-entry-hook
          (elfeed-regexp-tagger "youtube\\.com" 'youtube))
```

Or avoiding tagging old entries as `unread`:

```el
;; Entries older than 2 weeks are marked as read
(add-hook 'elfeed-new-entry-hook
          (elfeed-time-untagger "2 weeks ago" 'unread))
```

Further enhancement of the search filter is planned for the future.

## Status and Roadmap

I've already got it to a point where it can serve about 90% of my web
feed needs. Of course, I'd like to flesh out a lot more so that it's
more generally useful. Except for a single RDF feed, it can properly
parse my entire 136-entry feed list.

Some things I want to add:

 * Reading entries within Emacs (like e-mail)
 * Persist the database between Emacs instances
 * Enclosure support
 * Database synchronization between computers (maybe)
 * Optional external database back-end (Xapian?, maybe)

## Motivation

As far as I know, outside of Elfeed there does not currently exist an
extensible, text-file configured, power use web feed client that can
handle large numbers of feeds. The existing clients I've tried are
missing some important capability that limits its usefulness to me.
