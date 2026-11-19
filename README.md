# Harbour-CUI

Character User Interface (CUI) library for the [Harbour](https://harbour.github.io/) compiler.

**CBro 1.1.3**, an xBase database browser built on Harbour-CUI (a successor to BroPlus), is ready to run on
Windows and Linux: [download the release](https://github.com/digikv/Harbour-CUI/releases/tag/cbro-1.1.3)
([samples/cbro](samples/cbro/README.md)).

## Origin

**Osnova ovog projekta preuzeta je od [tfonrouge/HBTui](https://github.com/tfonrouge/HBTui).**

The foundation of this project is taken from [tfonrouge/HBTui](https://github.com/tfonrouge/HBTui)
(Teo. USA, S.A. / Francisco Garcia Rodriguez and contributors). This repository continues that
work as **Harbour-CUI**, with a SaveScreen-based window layer (no `hbct` dependency), CUI-style
chrome, and further widget/event improvements.

Older standalone CUI helpers (`win.prg`, `cui.ch`) are kept under [`legacy/`](legacy/) for reference.

## Features

**30 widgets** covering forms, data display, navigation, charts and feedback:

| Category | Widgets |
|----------|---------|
| Windows | HTMainWindow, HTDesktop, HTDialog (modal + button bar + draggable), HTMessageBox |
| Input | HTLineEdit (selection + clipboard), HTGet (full PICTURE/VALID/WHEN, Shift+arrow selection), HTTextEdit (multi-line, selection + clipboard), HTSpinner, HTCheckList |
| Buttons | HTPushButton, HTCheckBox, HTRadioButton |
| Lists | HTListBox, HTComboBox (type-to-filter), HTBrowse (TBrowse wrapper, inline cell editing: F2 / typing / double-click, typed values via the column PICTURE) |
| Menus | HTMenuBar (`&` hot keys, Alt+letter, action shortcuts), HTMenu, HTContextMenu, HTAction |
| Layout | HTVBoxLayout, HTHBoxLayout, HTGridLayout |
| Display | HTLabel, HTFrame, HTProgressBar, HTStatusBar, HTSeparator, HTScrollBar, HTTabWidget, HTToolBar |
| Charts | HTSparkline (live values, any height), HTBarChart (labels, compact values) - block characters, eight levels per row |
| Feedback | HTToast (non-blocking notifications, auto-dismiss, stacking) |

**Event system** — `HTEventLoop` with priority queue, mouse tracking, task scheduling. On Windows/gtwvt, mouse is driven by a Win32 WndProc subclass with coalesced `WM_MOUSEMOVE`. A widget that returns `.T.` from `mouseDragWanted()` (drawing, dragging) gets every mouse move and the button up while a button is held, also outside it. See [docs/EVENT_SYSTEM.md](docs/EVENT_SYSTEM.md).

**Viewport system** — `wFormat()` / viewport remapping in `src/HTWindow/` so child widgets paint in local coordinates.

**Window stack** — overlapping SaveScreen windows composed in z-order over the desktop (`src/HTWindow/HTWinApi.prg`):

- Title-bar chrome: drag the title to move, `┘` resizes (content repaints live while the mouse button is held), `□` maximizes (`▫` restores), `─` minimizes, `×` closes. The buttons are three cells wide from a window width of 24; on a codepage without these symbols they fall back to `/ # _ x`.
- A click anywhere in a window brings it to the front (`widget:raise()`). The fullscreen `HTMainWindow` stays underneath as the application shell, and closing it ends the application.
- Ctrl+F6 / Ctrl+Shift+F6 activate the next / previous window from the keyboard (Alt+Tab is taken by Windows itself).
- Drop shadows follow every move and resize. A window covered by others paints into its own layer and never over the windows above it.
- The text cursor returns to the focused editor (`HTGet`, `HTLineEdit`, `HTTextEdit`) after any repaint, also when switching between windows. A click past the typed text in an `HTGet` lands right after it.
- In `HTGet` and `HTLineEdit`, Enter moves to the next field like Tab (Clipper READ style); a failing VALID keeps the focus.
- Text selection: Shift + arrows / Home / End selects in `HTGet`, `HTLineEdit` and `HTTextEdit`; Del and Backspace delete the selection, typing replaces it (picture literals of an `HTGet` stay). Clipboard: Ctrl+C / Ctrl+X / Ctrl+V and Ctrl+Ins / Shift+Ins; a plain Ins toggles insert mode.
- Window repaints are buffered (`DispBegin`/`DispEnd`) and a focus change repaints only the two affected controls, so clicks don't make the screen flicker.

**Unicode and East Asian text** — Chinese (traditional and simplified), Japanese and Korean can be typed with the Windows IME and are shown in every widget, as are Cyrillic, Greek and other scripts.

- `HTApplication():new()` switches the VM to the `UTF8EX` codepage when the application kept Harbour's default (`EN`). An application that selects its own codepage before that call keeps it; choose `UTF8EX` for CJK text.
- Data in an older codepage needs no conversion in your code: tell the RDD the table's codepage (`USE customers CODEPAGE "HR852"`, or `Set( _SET_DBCODEPAGE, "HR852" )` for all tables) and it translates on every read and write. The file stays in that codepage, so older programs can keep using it.
- A Chinese, Japanese or Korean character takes two screen cells, as in a terminal. Editors, GETs, labels, menus, window titles and HTBrowse columns measure text in cells, so the cursor, mouse clicks and columns stay aligned (`ht_strWidth()`, `ht_cellPadR()`, … in `src/HTCells.prg`).
- On Windows, gtwvt draws CJK characters through a fallback font. Windows lays out Korean Hangul by its own glyph width, so the cell after a Hangul character holds a zero width space instead of a space: Chinese, Japanese and Korean all line up on the same grid.
- The IME composition window and candidate list open at the text cursor. `ht_fullScreen( .F. )` before `ht_cuiSetupDisplay()` / `exec()` opens a maximized window instead of fullscreen, so the taskbar with the IME mode indicator stays visible (Alt+Enter still switches). Or keep fullscreen and show the indicator in the program: `ht_inputLanguage()` returns the input language and IME mode (`"EN"`, `"GU"`, `"ZH 中"`, `"JA あ"`, `"KO A"`, …), the `unicode` sample and CBro show it in their status bar.
- Multi-byte codepages such as `CP950` (Big5) keep their text through window moves and restores. Harbour's CP950 counts bytes, not characters, so for editing keep the VM in `UTF8EX` and open Big5 tables with `CODEPAGE "CP950"`.
- Indic scripts (Gujarati, Hindi and the other scripts of India, U+0900–U+0D7F): a syllable — a consonant with its vowel signs, or a conjunct joined by a virama — takes one cell per code point, so `Len()`, the cursor and the columns stay as they are. On Windows, gtwvt would draw a whole row with the glyphs' own widths and the rest of the row drifted off the grid; the window bridge draws these rows again, each syllable shaped (Nirmala UI font) and centered on its cells. On Linux (gtxwc) they are not shaped yet.
- `samples/unicode` shows labels, GETs, editors and a browse in Chinese, Japanese, Korean, Gujarati, Hindi, Cyrillic and Greek.
- On Linux, `gtxwc` draws with a single X font, and its default `fixed` has no Chinese, Japanese or Korean glyphs: they show as empty boxes (Cyrillic, Greek and accented Latin are fine). Install the "Unifont Cell" font once and every application picks it up by itself:

  ```bash
  sudo apt install xfonts-unifont pcf2bdf xfonts-utils python3
  sudo sh tools/unifont-cell.sh
  ```

  The script builds it from the installed GNU Unifont: the same glyphs, each advancing one cell, so the two-cell CJK characters line up on the grid.

**Themes** — every widget takes its colors from the `HTTheme` singleton when it paints. A theme is one color per category (`HT_CLR_WINDOW`, `HT_CLR_BROWSE_FOCUSED`, …) plus its palette: the 16 RGB colors that the color indexes 0–15 show on screen (gtwvt and gtxwc). So the same `"0/9"` is gray in one theme and dark slate in another.

| Theme | Look |
|-------|------|
| `Classic` (default) | gray windows, pastel blue title and highlight, white fields |
| `Blue` | the classic blue DOS look: white on blue, green browse with a red cursor |
| `Light` | Classic with a lighter palette |
| `Dark` | dark gray, blue highlight |
| `Amber` | for the night: warm amber on near black, little blue light |
| `HighContrast` | white and yellow on black |
| `Mono` | grays only |

The first five are the color schemes of Digi knjigovodstvo, palette included; `Blue` is its original look, still a favorite of many users.

```harbour
HTTheme():loadTheme( "Blue" )          // or loadBlue(), loadDark(), ...
HTApplication():repaintAll()           // show it on every window

// an own theme: start from a built-in one, change colors and palette, keep it
HTTheme():loadTheme( "Classic" )
HTTheme():setColor( HT_CLR_TITLEBAR, "15/1" )
HTTheme():setPaletteColor( 14, ht_rgb( 255, 220, 120 ) )
HTTheme():saveTheme( "My theme" )      // built-in themes stay as they are
```

`addTheme()`, `removeTheme()`, `themeNames()`, `themeColors()` and `themePalette()` manage own themes. `samples/theme_editor` edits the colors and the palette interactively, saves own themes and keeps them in `themes.json` for the next start.

**Pixel fonts and sprites** — text mode with an own character set, the way 8-bit home computers did it: every character an 8×8 square cell, sprites at U+E000…, all drawn as text.

- A `.glyphs` file holds the drawings (`#` = pixel, `.` = empty; wider sprites become several characters). `src/HTPixelFont.prg` reads and writes it and builds the fonts in Harbour, no external tools: a TrueType font for Windows (`ht_pixelFontTTF()`) and X11 bitmap fonts at 16–48 px for Linux (`ht_pixelFontX11()`).
- `ht_fontLoad()` makes the font usable for this program only (Windows: no installation; Linux: added to the X font path). `ht_keyDown( nVirtualKey )` reads keys held down — several at once, the same key codes on Windows and Linux — only while the program's window has the focus.
- `samples/fonteditor` draws the glyphs (mouse with drag, keyboard, preview, transforms, undo) and builds the fonts with F9; `tools/pixelfont.prg` does the build from the command line.
- `samples/game` is NEBULA RAID, a fixed-screen shooter on a 32×24 grid in its own font (Windows and Linux).

## Quick Start

```bash
# Build the library
hbmk2 hbtui.hbp

# Build and run Hello World
cd samples/hello && hbmk2 hello.prg hbmk.hbm && ./hello
```

## Samples

| Sample | Description |
|--------|-------------|
| `hello` | Minimal HTMainWindow + HTLabel |
| `dialog` | HTDialog + HTMessageBox + toast results |
| `window` | Overlapping windows: a form with two HTGets and two editable HTBrowse grids (move, resize, raise, maximize, minimize, cell editing) |
| `browse` | HTBrowse data grid with inline editing |
| `controls` | Buttons, checkboxes, spinner, combo, list |
| `get` | HTGet with PICTURE / VALID / WHEN |
| `layout` / `hbox_layout` / `grid_layout` | Layout managers |
| `editor` | HTLineEdit + HTTextEdit |
| `menu` | HTMenuBar + HTMenu |
| `progress` | Animated HTProgressBar |
| `charts` | HTSparkline (live, 4 rows) and HTBarChart, following the themes (F1–F7) |
| `theme` | Switching among the seven built-in themes (F1–F7) |
| `theme_editor` | Change theme colors and the palette, add own themes, save them to `themes.json` |
| `unicode` | Chinese, Japanese, Korean, Gujarati, Hindi, Cyrillic and Greek text: IME input, two-cell characters, Indic syllables |
| `toast` | HTToast notifications |
| `fonteditor` | Pixel font editor for `.glyphs` files: draw with the mouse, preview, build TTF + X11 fonts (F9) |
| `game` | NEBULA RAID: a shooter drawn entirely with its own 8×8 font, held keys for moving and firing ([release nebula-raid-1.0](https://github.com/digikv/Harbour-CUI/releases/tag/nebula-raid-1.0)) |
| `showcase` | Full controls demo |
| `cbro` | CBro: xBase database browser and file manager, a successor to BroPlus - DBF, LetoDB/LetoDBf, SQL, Excel, scripts (own build: `samples/cbro/README.md`; programs: [release cbro-1.1.3](https://github.com/digikv/Harbour-CUI/releases/tag/cbro-1.1.3); dual license GPL-3+ / commercial) |
| `cedit` | CEdit: text editor with the keys of HbEdit, tabs, find / replace, syntax highlighting (`samples/cedit/README.md`; programs: [release cedit-1.0.2](https://github.com/digikv/Harbour-CUI/releases/tag/cedit-1.0.2); dual license GPL-3+ / commercial) |
| `cnc` | CNC: two-panel file manager in the style of Norton Commander with options of Total Commander - Name / Ext / Size / Date columns sorted by a click, the Net menu (FTP in a panel, network drives); F3 / F4 with the editor of CEdit (`samples/cnc/README.md`; programs: [release cnc-1.0.2](https://github.com/digikv/Harbour-CUI/releases/tag/cnc-1.0.2); dual license GPL-3+ / commercial) |
| `cuibuilder` | CuiBuilder: builds Harbour programs and libraries like HwBuilder (Kresin) - runs Harbour, the C compiler and the linker itself from the templates of `cuibuild.ini`, only what changed; `.hwprj` projects (read / written) and `.hbp` (read); errors open in the editor of CEdit; a dialog designer (Harbour-CUI widgets, generates the dialog function); a report designer (Clipper .frm, tried in Word / Excel, generates the report function for the program); Git (status, stage, diff, commit, log, pull, push); `cuibc` on the command line (`samples/cuibuilder/README.md`; dual license GPL-3+ / commercial) |

```bash
cd samples/<name> && hbmk2 <name>.prg hbmk.hbm && ./<name>

# the window sample's source is window_01.prg
cd samples/window && hbmk2 window_01.prg hbmk.hbm && ./window_01

# the game's source is nebula.prg
cd samples/game && hbmk2 nebula.prg hbmk.hbm && ./nebula

# CBro has its own project (see samples/cbro/README.md)
cd samples/cbro && hbmk2 cbro.hbp && ./cbro
```

## Requirements

The library needs only Harbour (core, no contribs, no `hbct`). Everything else the samples and CBro
use — the [XLSX class](https://github.com/digikv/xlsx) and the LetoDB/LetoDBf clients for CBro,
fonts, Linux packages — is listed in [docs/DEPENDENCIES.md](docs/DEPENDENCIES.md). How to build the library, the samples and CBro: [docs/BUILD.md](docs/BUILD.md).

- Harbour compiler
- GT driver: `gtxwc` on Linux, `gtwvt` on Windows (tests: `gttrm` / `-gtnul`)
  - Windows: borderless fullscreen with the Consolas font scaled to fill the screen (or a maximized window: `ht_fullScreen( .F. )`); mouse via a Win32 WndProc bridge. Pixel-font programs should be DPI aware (manifest, see `samples/game/nebula.rc`), or Windows stretches them at 125% / 150%.
  - Linux: an X11 window that starts with the requested grid (default 40×131) in the GT's own font; mouse via `Inkey()`. `gtxwc` cannot scale its font, so resizing or maximizing the window changes the grid instead: the `HTMainWindow` shell and its bottom status bar follow, floating windows keep their place. Platform code is selected with `#ifdef __PLATFORM__WINDOWS`.
  - `gtxwc` draws with X core fonts and stops with "Cannot load 'fixed' font" when they are missing — install them first (Debian/Ubuntu: `sudo apt install xfonts-base`). Building Harbour itself with `gtxwc` needs `libx11-dev`.
- Programs that call `ht_fontLoad()` / `ht_keyDown()` on Linux link Xlib (`{unix}-lX11` in their `hbmk.hbm`).
- Compiler flags: `-w3 -es2`

## License / credits

- Harbour-CUI is released under the [MIT License](LICENSE) — Copyright (c) 2021-2026 Srđan Dragojlović.
- Widget framework foundation: [tfonrouge/HBTui](https://github.com/tfonrouge/HBTui)
- Harbour-CUI maintenance and CUI window evolution: [digikv/Harbour-CUI](https://github.com/digikv/Harbour-CUI)
