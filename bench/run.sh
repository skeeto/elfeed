#!/bin/bash
# Orchestrate the full elfeed XML parser comparison test.
#
# Usage: run.sh <url-file> [results-dir]
#
# The url-file should contain one feed URL per line. You can generate
# one from an elfeed-org file with extract-urls.el:
#
#   emacs --batch -Q -l bench/extract-urls.el /path/to/feeds.org \
#     | grep -v localhost > urls.txt
#
# Results and artefacts are stored under results-dir.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

if [ $# -lt 1 ]; then
    echo "Usage: $0 <url-file> [results-dir]" >&2
    exit 1
fi

SRC_URL_FILE="$1"
RESULTS_DIR="${2:-$SCRIPT_DIR/results-$(date +%Y%m%d-%H%M%S)}"
FEED_DIR="$RESULTS_DIR/feeds"
URL_FILE="$RESULTS_DIR/urls.txt"
REPORT="$RESULTS_DIR/report.txt"

mkdir -p "$RESULTS_DIR"

echo "=== Step 1: Copy URL list ==="
cp "$SRC_URL_FILE" "$URL_FILE"
echo "  $(wc -l < "$URL_FILE" | tr -d ' ') feed URLs"

echo ""
echo "=== Step 2: Download feeds ==="
bash "$SCRIPT_DIR/download-feeds.sh" "$FEED_DIR" "$URL_FILE"

echo ""
echo "=== Step 3: Run parser comparison ==="
EMACS_LOG="$RESULTS_DIR/emacs.log"
emacs --batch -Q \
    -l "$SCRIPT_DIR/test-parsers.el" \
    "$FEED_DIR" "$REPORT" \
    2>"$EMACS_LOG"

echo ""
echo "=== Done ==="
echo "Results directory: $RESULTS_DIR"
echo "Report:           $REPORT"
echo "Downloaded feeds: $FEED_DIR/"
