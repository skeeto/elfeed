# Elfeed Emacs Web Feed Reader

Elfeed is a web feed reader for Emacs, inspired by
[notmuch](http://notmuchmail.org/). It supports both Atom and RSS and
is currently in beta. Requires Emacs 24 (lexical closures).

[![](http://i.imgur.com/8eTghK8.png)](http://i.imgur.com/l5aVPDc.png)

[![](http://i.imgur.com/3yHGITn.png)](http://i.imgur.com/EfdBKif.png)

## Getting Started

Elfeed is broken into a multiple source files, so if you manually
install it you will need to add the Elfeed package directory to your
`load-path`. If installed via ELPA, this will be done automatically.

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

If there are a lot of feeds, the initial update will take noticeably
longer than normal operation because of the large amount of
information being written the database. Future updates will only need
to write new or changed data.

From the search buffer there are a number of ways to interact with
entries. Entries are selected by placing the point over an entry.
Multiple entries are selected at once by using an active region.

 * <kbd>RET</kbd>: view selected entry in a buffer
 * <kbd>b</kbd>: open selected entries in your browser (`browse-url`)
 * <kbd>y</kbd>: copy selected entries URL to the clipboard
 * <kbd>r</kbd>: mark selected entries as read
 * <kbd>u</kbd>: mark selected entries as unread
 * <kbd>+</kbd>: add a specific tag to selected entries
 * <kbd>-</kbd>: remove a specific tag from selected entries

## Tags

Elfeed maintains a list of arbitrary tags -- symbols attached to an
entry. The tag `unread` is treated specially by default, with unread
entries appearing in bold.

### Filter Syntax

To make tags useful, the Elfeed entry listing buffer can be filtered
by tags. Use `elfeed-search-set-filter` (or <kbd>s</kbd>) to update
the filter.

Any component of the search string beginning with a `+` or
a `-` is treated like a tag. `+` means the tag is required, `-` means
the tag must not be present.

A component beginning with a `@` indicates an age. Entries older than
this age are filtered out. The age description accepts plain English,
but cannot have spaces, so use dashes. For example, `"@2-years-old"`
or `"@3-days-ago"`.

All other components are treated as a regular expression.

Here are some example filters.

 * `+unread`

Only show unread entries. This is the default filter.

 * `linu[xs] @1-year-old`

Only show entries about Linux or Linus from the last year.

 * `-unread +youtube`

Only show previously-read entries tagged as `youtube`.

### Tag Hooks

The last example assumes you've tagged posts with `youtube`. You
probably want to do this sort of thing automatically, which can be
done with the `elfeed-new-entry-hook`. Functions in this hook are
called with new entries, allowing them to be manipulated, such as
adding tags.

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

## Status and Roadmap

I've already got it to a point where it can serve about 90% of my web
feed needs. Of course, I'd like to flesh out a lot more so that it's
more generally useful. Except for a single RDF feed, it can properly
parse my entire 136-entry feed list.

Some things I want to add:

 * Database synchronization between computers (maybe)

## Motivation

As far as I know, outside of Elfeed there does not currently exist an
extensible, text-file configured, power-user web feed client that can
handle large numbers of feeds. The existing clients I've tried are
missing some important capability that limits its usefulness to me.
