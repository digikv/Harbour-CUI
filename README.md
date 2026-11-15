# Harbour-CUI

Character User Interface (CUI) library for the [Harbour](https://harbour.github.io/) compiler.

## Origin

**Osnova ovog projekta preuzeta je od [tfonrouge/HBTui](https://github.com/tfonrouge/HBTui).**

The foundation of this project is taken from [tfonrouge/HBTui](https://github.com/tfonrouge/HBTui)
(Teo. USA, S.A. / Francisco Garcia Rodriguez and contributors). This repository continues that
work as **Harbour-CUI**, with a SaveScreen-based window layer (no `hbct` dependency), CUI-style
chrome, and further widget/event improvements.

Older standalone CUI helpers (`win.prg`, `cui.ch`) are kept under [`legacy/`](legacy/) for reference.

## Features

**25+ widgets** covering forms, data display, navigation, and feedback:

| Category | Widgets |
|----------|---------|
| Windows | HTMainWindow, HTDesktop, HTDialog (modal + button bar + draggable), HTMessageBox |
| Input | HTLineEdit (selection + clipboard), HTGet (full PICTURE/VALID/WHEN), HTTextEdit, HTSpinner, HTCheckList |
| Buttons | HTPushButton, HTCheckBox, HTRadioButton |
| Lists | HTListBox, HTComboBox (type-to-filter), HTBrowse (TBrowse wrapper, inline cell editing: F2 / typing / double-click, typed values via the column PICTURE) |
| Menus | HTMenuBar, HTMenu, HTContextMenu, HTAction |
| Layout | HTVBoxLayout, HTHBoxLayout, HTGridLayout |
| Display | HTLabel, HTFrame, HTProgressBar, HTStatusBar, HTSeparator, HTScrollBar, HTTabWidget, HTToolBar |
| Feedback | HTToast (non-blocking notifications, auto-dismiss, stacking) |

**Event system** — `HTEventLoop` with priority queue, mouse tracking, task scheduling. On Windows/gtwvt, mouse is driven by a Win32 WndProc subclass with coalesced `WM_MOUSEMOVE`. See [docs/EVENT_SYSTEM.md](docs/EVENT_SYSTEM.md).

**Viewport system** — `wFormat()` / viewport remapping in `src/HTWindow/` so child widgets paint in local coordinates.

**Window stack** — overlapping SaveScreen windows composed in z-order over the desktop (`src/HTWindow/HTWinApi.prg`):

- Title-bar chrome: drag the title to move, `┘` resizes (content repaints live while the mouse button is held), `□` maximizes (`▫` restores), `─` minimizes, `×` closes. The buttons are three cells wide from a window width of 24; on a codepage without these symbols they fall back to `/ # _ x`.
- A click anywhere in a window brings it to the front (`widget:raise()`). The fullscreen `HTMainWindow` stays underneath as the application shell, and closing it ends the application.
- Ctrl+F6 / Ctrl+Shift+F6 activate the next / previous window from the keyboard (Alt+Tab is taken by Windows itself).
- Drop shadows follow every move and resize. A window covered by others paints into its own layer and never over the windows above it.
- The text cursor returns to the focused editor (`HTGet`, `HTLineEdit`, `HTTextEdit`) after any repaint, also when switching between windows. A click past the typed text in an `HTGet` lands right after it.
- In `HTGet` and `HTLineEdit`, Enter moves to the next field like Tab (Clipper READ style); a failing VALID keeps the focus.
- Window repaints are buffered (`DispBegin`/`DispEnd`) and a focus change repaints only the two affected controls, so clicks don't make the screen flicker.

**Unicode and East Asian text** — Chinese (traditional and simplified), Japanese and Korean can be typed with the Windows IME and are shown in every widget, as are Cyrillic, Greek and other scripts.

- `HTApplication():new()` switches the VM to the `UTF8EX` codepage when the application kept Harbour's default (`EN`). An application that selects its own codepage before that call keeps it; choose `UTF8EX` for CJK text.
- Data in an older codepage needs no conversion in your code: tell the RDD the table's codepage (`USE customers CODEPAGE "HR852"`, or `Set( _SET_DBCODEPAGE, "HR852" )` for all tables) and it translates on every read and write. The file stays in that codepage, so older programs can keep using it.
- A Chinese, Japanese or Korean character takes two screen cells, as in a terminal. Editors, GETs, labels, menus, window titles and HTBrowse columns measure text in cells, so the cursor, mouse clicks and columns stay aligned (`ht_strWidth()`, `ht_cellPadR()`, … in `src/HTCells.prg`).
- On Windows, gtwvt draws CJK characters through a fallback font. Windows lays out Korean Hangul by its own glyph width, so the cell after a Hangul character holds a zero width space instead of a space: Chinese, Japanese and Korean all line up on the same grid.
- `samples/unicode` shows labels, GETs, editors and a browse in Chinese, Japanese, Korean, Cyrillic and Greek.
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
| `theme` | Switching among the seven built-in themes (F1–F7) |
| `theme_editor` | Change theme colors and the palette, add own themes, save them to `themes.json` |
| `unicode` | Chinese, Japanese, Korean, Cyrillic and Greek text: IME input, two-cell characters |
| `toast` | HTToast notifications |
| `showcase` | Full controls demo |

```bash
cd samples/<name> && hbmk2 <name>.prg hbmk.hbm && ./<name>

# the window sample's source is window_01.prg
cd samples/window && hbmk2 window_01.prg hbmk.hbm && ./window_01
```

## Requirements

- Harbour compiler
- GT driver: `gtxwc` on Linux, `gtwvt` on Windows (tests: `gttrm` / `-gtnul`)
  - Windows: borderless fullscreen with the Consolas font scaled to fill the screen; mouse via a Win32 WndProc bridge.
  - Linux: an X11 window that starts with the requested grid (default 40×131) in the GT's own font; mouse via `Inkey()`. `gtxwc` cannot scale its font, so resizing or maximizing the window changes the grid instead: the `HTMainWindow` shell and its bottom status bar follow, floating windows keep their place. Platform code is selected with `#ifdef __PLATFORM__WINDOWS`.
  - `gtxwc` draws with X core fonts and stops with "Cannot load 'fixed' font" when they are missing — install them first (Debian/Ubuntu: `sudo apt install xfonts-base`). Building Harbour itself with `gtxwc` needs `libx11-dev`.
- Compiler flags: `-w3 -es2`

## License / credits

- Harbour-CUI is released under the [MIT License](LICENSE) — Copyright (c) 2021-2026 Srđan Dragojlović.
- Widget framework foundation: [tfonrouge/HBTui](https://github.com/tfonrouge/HBTui)
- Harbour-CUI maintenance and CUI window evolution: [digikv/Harbour-CUI](https://github.com/digikv/Harbour-CUI)
