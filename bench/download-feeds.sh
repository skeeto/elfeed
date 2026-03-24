#!/bin/bash
# Download all feeds into individual files under feeds/
set -euo pipefail

FEED_DIR="$1"
URL_FILE="$2"
TIMEOUT=30
PARALLEL=16

mkdir -p "$FEED_DIR"

download_one() {
    local url="$1"
    local feed_dir="$2"
    local timeout="$3"
    # Use SHA1 of URL as filename to avoid path issues
    local hash
    hash=$(printf '%s' "$url" | (sha1sum 2>/dev/null || shasum -a 1) | cut -d' ' -f1)
    local outfile="${feed_dir}/${hash}.xml"
    local metafile="${feed_dir}/${hash}.meta"

    if curl -sS -L --compressed -m "$timeout" \
         -H "User-Agent: Emacs Elfeed 3.4.2" \
         -o "$outfile" \
         -w '%{http_code}' \
         "$url" > "${feed_dir}/${hash}.status" 2>"${feed_dir}/${hash}.err"; then
        printf '%s\n' "$url" > "$metafile"
        local status
        status=$(cat "${feed_dir}/${hash}.status")
        if [ "$status" -ge 400 ] 2>/dev/null; then
            printf 'FAIL [HTTP %s] %s\n' "$status" "$url" >&2
            rm -f "$outfile"
        else
            local size
            size=$(wc -c < "$outfile" | tr -d ' ')
            printf 'OK   [%s] %6s bytes  %s\n' "$status" "$size" "$url" >&2
        fi
    else
        printf '%s\n' "$url" > "$metafile"
        printf 'FAIL [curl error] %s\n' "$url" >&2
        rm -f "$outfile"
    fi
    rm -f "${feed_dir}/${hash}.status" "${feed_dir}/${hash}.err"
}

export -f download_one

printf 'Downloading %d feeds (max %d parallel, %ds timeout)...\n' \
    "$(wc -l < "$URL_FILE")" "$PARALLEL" "$TIMEOUT" >&2

xargs -P "$PARALLEL" -I{} bash -c 'download_one "$@"' _ {} "$FEED_DIR" "$TIMEOUT" < "$URL_FILE"

downloaded=$(find "$FEED_DIR" -name '*.xml' | wc -l | tr -d ' ')
printf '\nDownloaded %s feeds successfully.\n' "$downloaded" >&2
