#!/usr/bin/env python3
"""Make "Unifont Cell" from GNU Unifont (BDF in, BDF out) for gtxwc.

gtxwc (Harbour's X11 GT) sizes its character cell by the widest glyph of
the font and draws a row of text as one string, each glyph advancing by its
own width. Unifont's Chinese, Japanese and Korean glyphs are 16 pixels wide,
twice the 8-pixel Latin ones: gtxwc would make every cell 16 pixels wide.

Harbour-CUI gives a wide character two screen cells - the character and a
filler cell after it - and needs the glyph to spill over the filler. This
font keeps every glyph's bitmap (BBX) but makes every glyph advance by one
8-pixel cell (DWIDTH 8): a wide glyph advances one cell, the filler the
next one, and its ink covers both.

It also adds 4096 blank glyphs at U+E100..U+F0FF (Private Use Area, empty
in Unifont), the filler cells: Harbour-CUI writes U+E100 + (character &
0xFFF) after a wide character. gtxwc repaints only the cells that changed;
a filler that changes with its character makes gtxwc repaint both, so no
half of a previous glyph is left behind. (U+E040..U+E076 stay free: the
Harbour GTs draw their own box and arrow symbols there.)

Usage: unifont-cell.py unifont.bdf unifont-cell.bdf
(tools/unifont-cell.sh builds and installs the X font)
"""
import sys

FAMILY = "unifontcell"
FOUNDRY = "hbtui"
FILLER_FIRST = 0xE100
FILLER_COUNT = 0x1000


def blank_glyph(code):
    return ("STARTCHAR uni%04X\nENCODING %d\nSWIDTH 500 0\nDWIDTH 8 0\n"
            "BBX 8 16 0 -2\nBITMAP\n%sENDCHAR\n" % (code, code, "00\n" * 16))


def main(src, dst):
    out = []
    wide = 0
    with open(src, encoding="latin-1") as f:
        for line in f:
            if line.startswith("FONT "):
                # -foundry-family-weight-slant-setwidth-addstyle-pixel-point-resx-resy-spacing-avg-registry-encoding
                xlfd = line[5:].strip().split("-")
                xlfd[1], xlfd[2] = FOUNDRY, FAMILY
                line = "FONT " + "-".join(xlfd) + "\n"
            elif line.startswith("FAMILY_NAME "):
                line = 'FAMILY_NAME "%s"\n' % FAMILY
            elif line.startswith("FOUNDRY "):
                line = 'FOUNDRY "%s"\n' % FOUNDRY
            elif line.startswith("CHARS "):
                line = "CHARS %d\n" % (int(line.split()[1]) + FILLER_COUNT)
            elif line.startswith("ENCODING "):
                code = int(line.split()[1])
                if FILLER_FIRST <= code < FILLER_FIRST + FILLER_COUNT:
                    sys.exit("U+%04X is already in the font" % code)
            elif line.startswith("DWIDTH "):
                if int(line.split()[1]) > 8:
                    wide += 1
                    line = "DWIDTH 8 0\n"
            elif line.startswith("SWIDTH "):
                if int(line.split()[1]) > 500:
                    line = "SWIDTH 500 0\n"
            elif line.startswith("ENDFONT"):
                out.extend(blank_glyph(c) for c in range(FILLER_FIRST, FILLER_FIRST + FILLER_COUNT))
            out.append(line)
    with open(dst, "w", encoding="latin-1") as f:
        f.writelines(out)
    print("%d wide glyphs advance one cell, %d filler glyphs added" % (wide, FILLER_COUNT))


if __name__ == "__main__":
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    main(sys.argv[1], sys.argv[2])
