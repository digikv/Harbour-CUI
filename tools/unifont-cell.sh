#!/bin/sh
# Builds and installs the "Unifont Cell" X font (Linux, gtxwc): GNU Unifont
# with one-cell advances, so Harbour-CUI shows Chinese, Japanese and Korean
# text two cells wide in gtxwc. See unifont-cell.py for what changes.
#
# Run as root (sudo sh tools/unifont-cell.sh). Needs the packages
# xfonts-unifont, pcf2bdf, xfonts-utils and python3; on Debian/Ubuntu:
#   sudo apt install xfonts-unifont pcf2bdf xfonts-utils python3
#
# ht_cuiSetupDisplay() picks the font up by itself when it is installed.
set -e

SRC=/usr/share/fonts/X11/misc/unifont.pcf.gz
DEST=/usr/share/fonts/X11/misc
HERE=$(cd "$(dirname "$0")" && pwd)
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

[ -f "$SRC" ] || { echo "missing $SRC - install xfonts-unifont"; exit 1; }
for t in pcf2bdf bdftopcf mkfontdir python3; do
    command -v $t >/dev/null || { echo "missing $t - install pcf2bdf xfonts-utils python3"; exit 1; }
done

gzip -dc "$SRC" > "$TMP/unifont.pcf"
pcf2bdf -o "$TMP/unifont.bdf" "$TMP/unifont.pcf"
python3 "$HERE/unifont-cell.py" "$TMP/unifont.bdf" "$TMP/unifont-cell.bdf"
bdftopcf -t -o "$TMP/unifont-cell.pcf" "$TMP/unifont-cell.bdf"
gzip -9 -c "$TMP/unifont-cell.pcf" > "$DEST/unifont-cell.pcf.gz"
mkfontdir "$DEST"

# a running X server reads its font path again (no effect without one)
[ -n "$DISPLAY" ] && command -v xset >/dev/null && xset fp rehash || true
echo "installed $DEST/unifont-cell.pcf.gz"
