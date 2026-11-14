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

- Title-bar chrome: drag the title to move, `/` resizes (content repaints live while the mouse button is held), `#` maximizes, `_` minimizes, `x` closes.
- A click anywhere in a window brings it to the front (`widget:raise()`). The fullscreen `HTMainWindow` stays underneath as the application shell, and closing it ends the application.
- Ctrl+F6 / Ctrl+Shift+F6 activate the next / previous window from the keyboard (Alt+Tab is taken by Windows itself).
- Drop shadows follow every move and resize. A window covered by others paints into its own layer and never over the windows above it.
- The text cursor returns to the focused editor (`HTGet`, `HTLineEdit`, `HTTextEdit`) after any repaint, also when switching between windows. A click past the typed text in an `HTGet` lands right after it.
- In `HTGet` and `HTLineEdit`, Enter moves to the next field like Tab (Clipper READ style); a failing VALID keeps the focus.
- Window repaints are buffered (`DispBegin`/`DispEnd`) and a focus change repaints only the two affected controls, so clicks don't make the screen flicker.

**Theme system** — `HTTheme` singleton with multiple built-in themes (Default, Dark, High Contrast, Mono).

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
| `theme` | Theme switching |
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

- Widget framework foundation: [tfonrouge/HBTui](https://github.com/tfonrouge/HBTui)
- Harbour-CUI maintenance and CUI window evolution: [digikv/Harbour-CUI](https://github.com/digikv/Harbour-CUI)
