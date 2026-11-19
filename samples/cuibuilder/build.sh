#!/bin/sh
# CuiBuilder build (Linux) - README.md, "Building CuiBuilder"
#   ./build.sh        the Harbour-CUI library (when it is not built yet), then cuibuilder and cuibc
#   ./build.sh all    the library again, then cuibuilder and cuibc
# HBMK2  hbmk2 to use (default: hbmk2 in the PATH, else /opt/harbour-core/bin/linux/gcc/hbmk2)
cd "$(dirname "$0")" || exit 1

if [ -z "$HBMK2" ]; then
    if command -v hbmk2 >/dev/null 2>&1; then
        HBMK2=hbmk2
    else
        HBMK2=/opt/harbour-core/bin/linux/gcc/hbmk2
    fi
fi
if ! command -v "$HBMK2" >/dev/null 2>&1; then
    echo "Harbour was not found ($HBMK2)."
    echo "Build Harbour first, or tell where it is:"
    echo "    HBMK2=/path/to/hbmk2 ./build.sh"
    exit 1
fi
if [ ! -f /usr/include/X11/Xlib.h ]; then
    echo "The X11 development files are missing: sudo apt install libx11-dev"
    exit 1
fi

ALL=no
if [ "$1" = "all" ]; then
    ALL=yes
    shift
fi
if [ $ALL = yes ] || ! ls ../../lib/linux/*/libhbtui.a >/dev/null 2>&1; then
    echo "=== Harbour-CUI library"
    ( cd ../.. && "$HBMK2" hbtui.hbp ) || { echo "The library was not built - see the messages above."; exit 1; }
fi

echo "=== CuiBuilder"
"$HBMK2" cuibuilder.hbp "$@" || { echo "CuiBuilder was not built - see the messages above."; exit 1; }
echo "=== cuibc"
"$HBMK2" cuibc.hbp "$@" || { echo "cuibc was not built - see the messages above."; exit 1; }
echo "Done: ./cuibuilder and ./cuibc   (run: ./cuibuilder project.hwprj)"
