# XML Parser Benchmark Results

Benchmark comparing `xml-parse-region` (Elisp) against
`libxml-parse-xml-region` (C/libxml2) for elfeed's feed parsing.

## Setup

- **Emacs**: 30.2, compiled with libxml2
- **Feeds tested**: 183 real-world feeds (Atom, RSS 2.0, RSS 1.0)
  covering blogs, substacks, YouTube, Reddit, Google Alerts, and more
- **Feed sizes**: 388 bytes to 16.9 MB
- **Parse errors**: 2 feeds had malformed XML that neither parser could handle
- **Iterations**: 5 per feed per parser

## Performance

| Metric | xml-parse-region | libxml-parse-xml-region |
|---|---|---|
| Total parse time (183 feeds × 5) | 9.960s | 1.724s |
| **Overall speedup** | | **5.8×** |

Per-feed speedup ranged from 2× on tiny feeds (< 1 KB) to **20× on
larger feeds**. The largest feed (16.9 MB, Austin Rochford) went from
1096ms to 75ms per parse — a 14.6× improvement.

Selected per-feed results:

| Feed | Size | xml-parse | libxml | Speedup |
|---|---|---|---|---|
| Signal Blog | 197 KB | 23.8ms | 1.1ms | 20.7× |
| Here Dragons Abound (Blogspot) | 1.5 MB | 160.1ms | 7.7ms | 20.7× |
| Dan Luu | 6.3 MB | 414.5ms | 25.8ms | 16.1× |
| Macro Man (Blogspot) | 2.5 MB | 195.3ms | 13.9ms | 14.1× |
| Contrary Brin (Blogspot) | 1.8 MB | 139.7ms | 10.8ms | 13.0× |
| Aswath Damodaran (Blogspot) | 1.2 MB | 68.5ms | 6.5ms | 10.5× |
| Sebastian Raschka (Substack) | 2.5 MB | 128.1ms | 13.8ms | 9.3× |
| Don't Worry About the Vase (Substack) | 2.5 MB | 124.1ms | 14.9ms | 8.3× |
| SCMP | 83 KB | 4.3ms | 0.5ms | 8.0× |
| xkcd | 2.5 KB | 0.2ms | 0.1ms | 2.2× |

## Correctness

Tested every `xml-query` pattern elfeed uses (feed type detection,
titles, links, IDs, dates, authors, categories, content, enclosures)
on all 181 parseable feeds.

**After applying the two required transformations** (see below), all
queries produce functionally equivalent results. The remaining
differences are:

| Difference | Feeds affected | Impact on elfeed |
|---|---|---|
| Inter-element whitespace nodes stripped | 46 | None — `xml-query--stringp` already filters these |
| Trailing whitespace trimmed in content strings | ~8 | None — `elfeed-cleanup` trims extracted text |
| `\r\n` normalized to `\n` in CDATA | ~8 | None — more correct per XML spec |
| Text node split at CDATA boundaries | 1 | None — elfeed concatenates with `apply #'concat` |

## Required Transformations

### 1. Unwrap synthetic `top` node

When comments or processing instructions appear before the root
element, `libxml-parse-xml-region` wraps everything in a
`(top nil ...)` node. `xml-parse-region` skips these and returns only
the document element.

Affected 3 of 181 feeds (feeds with XML/HTML comments before the root
tag, e.g. blackarbs, Remains of the Day, Pluralistic).

Fix: extract the first non-comment element child from the `top` node.

### 2. Wrap result in a list

`xml-parse-region` returns a list of top-level nodes:
`((feed ...))`. `libxml-parse-xml-region` returns a single node:
`(feed ...)`.

Fix: `(list result)` — applied inside the unwrap helper.
