# What Harbour-CUI uses

What the library, the samples and CBro need besides Harbour-CUI itself: libraries, other
repositories, fonts and system packages. The library alone needs only Harbour.

## The library (`hbtui.hbp`)

| What | Where it comes from | Notes |
|---|---|---|
| Harbour compiler and runtime | https://github.com/harbour/core | the newest build; on Windows 32-bit MinGW (`D:\tools\harbour-core\bin\win\mingw\hbmk2.exe`). Core only: no contribs, no `hbct` |
| GT driver | part of Harbour | `gtwvt` on Windows, `gtxwc` on Linux (`hbtui.hbc` selects it); tests use `gtnul` / `gttrm` |
| Windows system DLLs | Windows | user32, gdi32 (linked by Harbour anyway); `imm32.dll` (IME placement) is loaded at run time |
| Xlib | Linux: `libx11-dev` | only for programs that call `ht_fontLoad()` / `ht_keyDown()` (`{unix}-lX11` in their `hbmk.hbm`, e.g. `samples/game`) |

Nothing else is linked: no external C libraries, no other Harbour projects.

## Fonts

| Font | Used for | Where it comes from |
|---|---|---|
| Consolas | the gtwvt screen font (Windows) | Windows |
| System CJK fonts (Microsoft YaHei, MS Gothic, Malgun Gothic, …) | Chinese, Japanese, Korean on Windows (GDI falls back to them) | Windows |
| Nirmala UI | Indic scripts on Windows: Gujarati, Hindi (Devanagari), Bengali, Gurmukhi, Oriya, Tamil, Telugu, Kannada, Malayalam (`ht_winmouse.c` draws their syllables with it) | Windows 8 and later |
| `fixed` and the other X core fonts | the gtxwc screen font (Linux) | `sudo apt install xfonts-base` |
| "Unifont Cell" | Chinese, Japanese, Korean on Linux (gtxwc) | built once by `tools/unifont-cell.sh` from GNU Unifont: `sudo apt install xfonts-unifont pcf2bdf xfonts-utils python3` |
| Own pixel fonts (`.glyphs`) | `samples/game`, `samples/fonteditor` | built by `src/HTPixelFont.prg`, no external tools |

Indic scripts on Linux (gtxwc) are not shaped yet: gtxwc draws every code point in its own cell,
so vowel signs and conjuncts are not joined.

## Samples

Every sample in `samples/` builds with `hbmk2 <name>.prg hbmk.hbm` and needs only the library, except:

| Sample | Needs in addition |
|---|---|
| `game` | Linux: Xlib (`libx11-dev`); sound with `aplay` (alsa-utils) |
| `cbro` | see below (own project `cbro.hbp`) |

## CBro (`samples/cbro`)

CBro is an application, not a part of the library; its build is described in
[samples/cbro/README.md](../samples/cbro/README.md) and its components (with licenses) in
[samples/cbro/THIRD-PARTY.md](../samples/cbro/THIRD-PARTY.md). In short:

| What | Where it comes from | Used for |
|---|---|---|
| XLSX class (`XLSXClass.prg`) | https://github.com/digikv/xlsx - next to this repository (`../xlsx`); `build.bat` clones it when it is missing, or `set CBRO_XLSXDIR=<folder>`. Optional: without it CBro is built without Excel (see [BUILD.md](BUILD.md)) | Excel: open a sheet as a table, Copy To / Export `.xlsx`, Append From a sheet |
| LetoDBf client (default) or LetoDB client | the LetoDBf / LetoDB sources, built with `rddleto.hbp` into `samples/cbro/lib/<platform>/` | `leto://` tables on a LetoDB server |
| Harbour contribs: hbmzip, hbziparc, hbmemio, rddsql, sddodbc, sddsqlt3 (SQLite), hbwin (Windows) | part of Harbour | zip/xlsx files, SQL (SQLite, ODBC), printing |
| unixODBC | Linux: `unixodbc-dev` (build), `libodbc2` (run) | ODBC data sources |
| Harbour compiler library (`hbcplr`) | part of Harbour | `.prg` scripts compiled in memory (left out with `-env:CBRO_NOCOMPILER=yes`) |
