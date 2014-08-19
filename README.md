# Elfeed Emacs Web Feed Reader

Elfeed is an extensible web feed reader for Emacs, supporting both
Atom and RSS. It requires Emacs 24 and is available for download from
[MELPA](http://melpa.milkbox.net/) or
[el-get](https://github.com/dimitri/el-get). Elfeed was inspired by
[notmuch](http://notmuchmail.org/).

For a longer overview,

 * [Introducing Elfeed, an Emacs Web Feed Reader](http://nullprogram.com/blog/2013/09/04/).
 * [Tips and Tricks](http://nullprogram.com/blog/2013/11/26/)
 * [... and more ...](http://nullprogram.com/tags/elfeed/)

[![](http://i.imgur.com/8eTghK8.png)](http://i.imgur.com/l5aVPDc.png)

[![](http://i.imgur.com/3yHGITn.png)](http://i.imgur.com/EfdBKif.png)

The database format is now stable, but there *may* still be an update
someday that requires you to initialize a new database from scratch.

## Getting Started

Elfeed is broken into a multiple source files, so if you manually
install it you will need to add the Elfeed package directory to your
`load-path`. If installed via package.el or el-get, this will be done
automatically.

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
      '("http://nullprogram.com/feed/"
        "http://www.terminally-incoherent.com/blog/feed/"))
```

Another option for providing a feel list is with an OPML file. Running
`M-x elfeed-load-opml` will fill `elfeed-feeds` with feeds listed in
an OPML file. When `elfeed-load-opml` is called interactively, it will
automatically save the feedlist to your customization file, so you
will only need to do this once.

If there are a lot of feeds, the initial update will take noticeably
longer than normal operation because of the large amount of
information being written the database. Future updates will only need
to write new or changed data. If updating feeds slows down Emacs too
much on your computer, reduce the value of `elfeed-max-connections`
(the number of feeds to process at one time).

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

### Autotagging

Tags can automatically be applied to entries discovered in specific
feeds through extra syntax in `elfeed-feeds`. Normally this is a list
of strings, but an item can also be a list, providing set of
"autotags" for a feed's entries.

```el
(setq elfeed-feeds
      '(("http://nullprogram.com/feed/" blog emacs)
        "http://www.50ply.com/atom.xml"  ; no autotagging
        ("http://nedroid.com/feed/" webcomic)))
```

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
or `"@3-days-ago"`. The database is date-oriented, so **filters that
include an age restriction are significantly more efficient.**

A component beginning with a `!` is treated as an "inverse" regular
expression.  This means that any entry matching this regular
expression will be filtered out (the regular expression begins *after*
the `!` character).  You can read this as "entry not matching `foo`".

All other components are treated as a regular expression, which means
only entries matching this will be shown.

Here are some example filters.

 * `@6-months-ago +unread`

Only show unread entries of the last six months. This is the default filter.

 * `linu[xs] @1-year-old`

Only show entries about Linux or Linus from the last year.

 * `-unread +youtube`

Only show previously-read entries tagged as `youtube`.

 * `+unread !x?emacs`

Only show unread entries not having `emacs` or `xemacs` in the title
or link.

#### Default Search Filter

You can set your default search filter by changing the default value
of `elfeed-search-filter`. It only changes buffer-locally when you're
adjusting the filter within Elfeed. For example, some users prefer to
have a space on the end for easier quick searching.

    (setq-default elfeed-search-filter "@1-week-ago +unread ")

### Tag Hooks

The last example assumes you've tagged posts with `youtube`. You
probably want to do this sort of thing automatically, which can be
done with the `elfeed-new-entry-hook`. Functions in this hook are
called with new entries, allowing them to be manipulated, such as
adding tags.

```el
;; Mark all YouTube entries
(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :feed-url "youtube\\.com"
                              :add '(video youtube)))
```

Avoiding tagging old entries as `unread`:

```el
;; Entries older than 2 weeks are marked as read
(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :before "2 weeks ago"
                              :remove 'unread))
```

Or building your own subset feeds:

```el
(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :feed-url "example\\.com"
                              :entry-title '(not "something interesting")
                              :add 'junk
                              :remove 'unread))
```

## Web Interface

Elfeed includes a web interface for remote network access. It's a
single-page web application that follows the database live as new
entries arrive. It's packaged separately on MELPA as `elfeed-web`. To
fire it up, run `M-x elfeed-web-start` and visit
http://localhost:8080/elfeed/ (check your `httpd-port`) with a
browser. See the `elfeed-web.el` header for endpoint documentation if
you'd like to access the Elfeed database through the web API.

It's a bit rough at the moment -- no keyboard shortcuts, read-only, no
authentication, and a narrow entry viewer -- but I'm looking to
improve it in the future. This will be Elfeed's "mobile" interface.

## Platform Support

I only use Elfeed on Linux right now, but I've also tested it on
Windows. Unfortunately the Windows port of Emacs is a bit too unstable
for parallel feed downloads, not to mention the
[tiny, hard-coded, 512 open descriptor limitation][files], so it
limits itself to one feed at a time on this platform. However, even on
Linux Elfeed occasionally triggers segmentation faults in Emacs. If I
am able, I intend to isolate these segfaults and report them to the
Emacs developers.

[files]: http://msdn.microsoft.com/en-us/library/kdfaxaay%28vs.71%29.aspx

The GNU-provided W32 build of Emacs doesn't include any of the
libraries needed to actually view entries within Emacs, but you can
still see the entry listing and visit entries in your browser. So on
Windows you'll either have to track down and install the missing DLLs,
or use [ntemacs](http://ntemacs.sourceforge.net/), which includes
these libraries.

## Database Management

The database should keep itself under control without any manual
intervention, but steps can be taken to minimize the database size if
desired. The simplest option is to run the `elfeed-db-compact`
command, which will pack the loose-file content database into a single
compressed file. This function can be added to `kill-emacs-hook`.

Going further, a function could be added to `elfeed-new-entry-hook` to
strip unwanted/unneeded content from select entries before being
stored in the database. For example, for YouTube videos only the entry
link is of interest and the regularly-changing entry content could be
tossed to save time and storage.

## Status and Roadmap

Elfeed is to the point where it can serve 100% of my own web feed
needs. My personal selection of about 140 feeds has been acting as my
test case as I optimize and add features.

Some things I still might want to add:

 * Database synchronization between computers
 * Support for [RDF content](http://web.resource.org/rss/1.0/modules/content/)

## Motivation

As far as I know, outside of Elfeed there does not exist an
extensible, text-file configured, power-user web feed client that can
handle a reasonable number of feeds. The existing clients I've tried
are missing some important capability that limits its usefulness to
me.
