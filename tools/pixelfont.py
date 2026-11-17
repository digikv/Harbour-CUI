#!/usr/bin/env python3
"""pixelfont.py - builds a square-cell 8x8 pixel TrueType font from a text file.

Usage:  python pixelfont.py glyphs.txt out.ttf "Family Name" [x11dir]
Needs:  fontTools  (pip install fonttools)

With x11dir it also writes X11 bitmap fonts for gtxwc (Linux): BDF files at
16, 24, 32, 40 and 48 pixels per cell (each font pixel a whole square of
screen pixels: crisp) and a fonts.dir; ht_fontLoad( x11dir ) adds the folder
to the X font path. XLFD: -hbtui-<family>-medium-r-normal--<px>-*-*-*-c-*-iso10646-1

The glyph file describes every character as 8 rows of '#' (pixel) and '.'
(empty); wider sprites are 16, 24, ... columns and become consecutive
characters, 8 columns each:

    # comment
    char A                 # the character itself ("char space" = U+0020)
    ..####..
    .##..##.
    ...                    (8 rows)

    glyph E000 ship        # hex code point (Private Use Area: E000..F8FF)
    .......##.......       # 16 columns -> U+E000 and U+E001
    ...                    (8 rows)

Every character advances one square cell: 8 pixels wide, 8 pixels high,
no descent - a console font in the ZX Spectrum manner. Rows of pixels become
rectangles (one contour per horizontal run), so the glyphs stay crisp at any
size that is a multiple of 8 pixels.
"""

import sys

from fontTools.fontBuilder import FontBuilder
from fontTools.pens.ttGlyphPen import TTGlyphPen
from fontTools.ttLib.tables.O_S_2f_2 import Panose

PIXEL = 128            # font units per pixel
CELL = 8 * PIXEL       # 1024 = units per em = cell width = cell height


def parse(path):
    """-> list of (codepoint, [8 strings of 8 '#'/'.'])"""
    glyphs = []
    with open(path, encoding="utf-8") as f:
        lines = [ln.rstrip("\n") for ln in f]
    i = 0
    while i < len(lines):
        line = lines[i].strip()
        i += 1
        # blank lines and comments ("# ..." - pixel rows never hold a space)
        if not line or line == "#" or line.startswith("# "):
            continue
        words = line.split()
        if words[0] == "char" and len(line) >= 6:
            code = 0x20 if words[1:2] == ["space"] else ord(line[5])
        elif words[0] == "glyph" and len(words) >= 2:
            code = int(words[1], 16)
        else:
            raise SystemExit("%s:%d: expected 'char' or 'glyph', got %r" % (path, i, line))
        rows = lines[i:i + 8]
        i += 8
        if len(rows) != 8:
            raise SystemExit("%s: %s needs 8 rows" % (path, line))
        width = len(rows[0].strip())
        if width % 8 or any(len(r.strip()) != width or set(r.strip()) - set("#.") for r in rows):
            raise SystemExit("%s: %s: rows must be equal, a multiple of 8 long, '#' and '.' only" % (path, line))
        for cell in range(width // 8):
            glyphs.append((code + cell, [r.strip()[cell * 8:cell * 8 + 8] for r in rows]))
    return glyphs


def draw(rows):
    """8 rows -> TrueType glyph: one clockwise rectangle per horizontal run."""
    pen = TTGlyphPen(None)
    for r, row in enumerate(rows):
        top = (8 - r) * PIXEL
        bottom = top - PIXEL
        c = 0
        while c < 8:
            if row[c] == "#":
                start = c
                while c < 8 and row[c] == "#":
                    c += 1
                left, right = start * PIXEL, c * PIXEL
                pen.moveTo((left, bottom))
                pen.lineTo((left, top))
                pen.lineTo((right, top))
                pen.lineTo((right, bottom))
                pen.closePath()
            else:
                c += 1
    return pen.glyph()


def build(glyph_file, out_file, family):
    glyphs = parse(glyph_file)
    if not any(code == 0x20 for code, _ in glyphs):
        glyphs.append((0x20, ["........"] * 8))

    order = [".notdef"] + ["uni%04X" % code for code, _ in glyphs]
    cmap = {code: "uni%04X" % code for code, _ in glyphs}
    outlines = {".notdef": draw(["########", "#......#", "#......#", "#......#",
                                 "#......#", "#......#", "#......#", "########"])}
    for code, rows in glyphs:
        outlines["uni%04X" % code] = draw(rows)

    fb = FontBuilder(CELL, isTTF=True)
    fb.setupGlyphOrder(order)
    fb.setupCharacterMap(cmap)
    fb.setupGlyf(outlines)
    fb.setupHorizontalMetrics({name: (CELL, 0) for name in order})
    fb.setupHorizontalHeader(ascent=CELL, descent=0)
    # GDI (AddFontResourceEx, gtwvt) rejects a font without the full set of
    # names; GDI+ and browsers do not mind
    ps_name = "".join(ch for ch in family if ch.isalnum()) + "-Regular"
    fb.setupNameTable({"familyName": family, "styleName": "Regular",
                       "uniqueFontIdentifier": ps_name + ";1.000",
                       "fullName": family + " Regular", "psName": ps_name,
                       "version": "Version 1.000"})
    panose = Panose()
    panose.bFamilyType = 2                       # Latin text
    panose.bProportion = 9                       # monospaced
    fb.setupOS2(sTypoAscender=CELL, sTypoDescender=0, sTypoLineGap=0,
                usWinAscent=CELL, usWinDescent=0,
                ulCodePageRange1=1,                      # Latin 1
                panose=panose)
    fb.setupPost(isFixedPitch=1)
    import io
    import os
    # fixed timestamps: the same glyphs give the same file
    fb.font.recalcTimestamp = False
    fb.font["head"].created = fb.font["head"].modified = 3846528000   # 2025-11-23
    data = io.BytesIO()
    fb.save(data)
    data = data.getvalue()
    # a running program that loaded the font keeps the file locked (Windows):
    # leave an unchanged font alone
    if os.path.exists(out_file) and open(out_file, "rb").read() == data:
        print("%s: unchanged (%d glyphs)" % (out_file, len(glyphs)))
        return
    try:
        with open(out_file, "wb") as f:
            f.write(data)
    except PermissionError:
        print("%s: in use (a running program loaded it) - not written" % out_file)
        return
    print("%s: %d glyphs, family %r" % (out_file, len(glyphs), family))


X11_SIZES = (16, 24, 32, 40, 48)
LF = chr(10)                                          # X servers read Unix line ends


def write_x11(glyph_file, out_dir, family):
    """BDF fonts (one per size in X11_SIZES) + fonts.dir for the X server."""
    import os
    glyphs = parse(glyph_file)
    if not any(code == 0x20 for code, _ in glyphs):
        glyphs.append((0x20, ["........"] * 8))
    glyphs.sort()
    name = "".join(ch for ch in family if ch.isalnum()).lower()
    os.makedirs(out_dir, exist_ok=True)
    entries = []
    for px in X11_SIZES:
        k = px // 8                                   # screen pixels per font pixel
        xlfd = "-hbtui-%s-medium-r-normal--%d-%d-75-75-c-%d-iso10646-1" % (name, px, px * 10, px * 10)
        file_name = "%s-%d.bdf" % (name, px)
        nbytes = (px + 7) // 8
        out = ["STARTFONT 2.1", "FONT " + xlfd, "SIZE %d 75 75" % px,
               "FONTBOUNDINGBOX %d %d 0 0" % (px, px),
               "STARTPROPERTIES 6", "FONT_ASCENT %d" % px, "FONT_DESCENT 0",
               "PIXEL_SIZE %d" % px, 'SPACING "C"', "AVERAGE_WIDTH %d" % (px * 10),
               "DEFAULT_CHAR 32", "ENDPROPERTIES", "CHARS %d" % len(glyphs)]
        for code, rows in glyphs:
            out += ["STARTCHAR uni%04X" % code, "ENCODING %d" % code,
                    "SWIDTH 1000 0", "DWIDTH %d 0" % px, "BBX %d %d 0 0" % (px, px), "BITMAP"]
            for row in rows:
                bits = "".join(("1" if ch == "#" else "0") * k for ch in row).ljust(nbytes * 8, "0")
                line = "%0*X" % (nbytes * 2, int(bits, 2))
                out += [line] * k
            out.append("ENDCHAR")
        out.append("ENDFONT")
        with open(os.path.join(out_dir, file_name), "w", newline=LF) as f:
            f.write(LF.join(out) + LF)
        entries.append("%s %s" % (file_name, xlfd))
    with open(os.path.join(out_dir, "fonts.dir"), "w", newline=LF) as f:
        f.write(str(len(entries)) + LF + LF.join(entries) + LF)
    print("%s: %d X11 fonts (%s px)" % (out_dir, len(entries), ", ".join(map(str, X11_SIZES))))


if __name__ == "__main__":
    if len(sys.argv) not in (4, 5):
        raise SystemExit(__doc__)
    build(sys.argv[1], sys.argv[2], sys.argv[3])
    if len(sys.argv) == 5:
        write_x11(sys.argv[1], sys.argv[4], sys.argv[3])
