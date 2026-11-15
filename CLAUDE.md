# Harbour-CUI (HBTui-based)

Character UI library for the Harbour compiler. **Foundation taken from
[tfonrouge/HBTui](https://github.com/tfonrouge/HBTui).** SaveScreen-based
window system in `src/HTWindow/` (no `hbct` dependency), with `wFormat()`
viewport-based child widget coordinate isolation.

## Build

```bash
# Build the library
hbmk2 hbtui.hbp

# Build a sample
cd samples/showcase && hbmk2 showcase.prg hbmk.hbm
cd samples/window && hbmk2 window_01.prg hbmk.hbm   # source is window_01.prg

# Run tests (all headless, exit 0 = pass)
cd tests && for t in test_basic test_eventloop test_layout test_theme test_widgets test_browse test_get test_integration; do hbmk2 $t.prg hbmk.hbm && ./$t; done
```

Requires: Harbour compiler at `~/git/core`.

GT driver: `gtxwc` on Linux (X11 window), `gtwvt` on Windows. Tests use `gttrm` / `-gtnul`.

Platform code is split with `#ifdef __PLATFORM__WINDOWS` (note: Harbour defines only the
upper-case form; `__PLATFORM__Windows` is never defined):
- `HTEventLoop:poll()` — Win32 WndProc mouse queue (Windows) vs portable `Inkey()` path
- `HTApplication:exec()` — installs the Win32 mouse bridge (`ht_winmouse.c`, stubs elsewhere).
  It subclasses the gtwvt window with the **W** API (`SetWindowLongPtrW`/`CallWindowProcW`):
  hbmk2 compiles C as ANSI by default, and an ANSI subclass turns the window into an ANSI
  window — IME/Unicode `WM_CHAR` (Chinese, Cyrillic on a Western system, …) then reaches
  gtwvt as `?`. Its `WM_PAINT` hook also widens the repaint to whole rows once wide (CJK)
  text was written (see Unicode below)
- `ht_cuiSetupDisplay()` — gtwvt fullscreen + Consolas font fit (Windows) vs a resizable
  `SetMode( nRows, nCols )` window in the GT's own font (Linux)
- `HTApplication:exec()` event mask — non-Windows adds `HB_INKEY_GTEVENT`, so gtxwc's
  `HB_K_RESIZE` (grid follows the window size) reaches `HTEventLoop:handleResize()`,
  which repaints the desktop, refits `HTMainWindow` (`screenResized()`: bottom status
  bar re-docked) and repaints every window. gtxwc needs X core fonts (`xfonts-base`).

The portable path can be checked on Windows by building a copy of the library with
`__PLATFORM__WINDOWS` renamed in those three files.

Compiler flags: `-w3 -es2` (warnings level 3, warnings as errors).

## Architecture

### Class Hierarchy

```
HTBase
  HTObject (parent/child tree, menubar, destroy/removeChild)
    HTWidget (rendering, events, focus, viewport, context menu, size policies)
      HTMainWindow, HTDesktop (singleton), HTDialog (modal + button bar)
      HTLabel, HTLineEdit (selection + clipboard), HTListBox, HTComboBox (type-to-filter)
      HTBrowse (TBrowse wrapper, inline editing), HTEditor (legacy, deprecated)
      HTTextEdit (modern multi-line editor, clipboard)
      HTGet (TGet-backed: full PICTURE, numeric/date/logical, undo, validation rejection)
      HTAbstractButton → HTPushButton, HTCheckBox, HTRadioButton
      HTFrame, HTProgressBar, HTStatusBar, HTSeparator
      HTMenuBar, HTMenu, HTContextMenu (right-click popup)
      HTMessageBox (singleton), HTScrollBar, HTSpinner, HTCheckList
      HTTabWidget, HTToolBar
      HTTheme (singleton: named themes with palettes; Classic/Blue/Light/Dark/Amber from
               Digi knjigovodstvo, HighContrast, Mono; own themes)
      HTToast (singleton, non-blocking notifications with auto-dismiss)
    HTAction (menu item with onTriggered callback)
    HTLayout → HTBoxLayout → HTHBoxLayout, HTVBoxLayout
               HTGridLayout
  HTLayoutItem (alignment mixin)

HTEventLoop (singleton, unified event polling + task scheduling)
HTGetBackend (TGet subclass, suppresses display() for viewport rendering)
HTWinApi (CT-compatible window shims: wOpen/wSelect/wFormat/… via SaveScreen)

HTEvent (standalone, HB_CLS_NOTOBJECT)
  HTFocusEvent, HTPaintEvent, HTCloseEvent, HTShowEvent, HTHideEvent
  HTMoveEvent, HTResizeEvent, HTMaximizeEvent
  HTInputEvent → HTKeyEvent, HTMouseEvent

HTPoint, HTSize (value types, HB_CLS_NOTOBJECT)
HTContextMenu (right-click popup menu, inherits HTObject)
```

### Key Design Patterns

**Viewport painting**: `paintChild()` uses `wFormat(margins)` to create a coordinate viewport per child widget. The child sees `(0,0)` to `(MaxRow(), MaxCol())` as its world. `hbtui.ch` remaps `DispOutAt`/`MaxRow`/`MaxCol`/`SetPos`/`DispBox`/`mRow`/`mCol` through `ht_*` helpers in `src/HTWindow/`. TBrowse is configured with absolute screen coordinates each paint.

**wFormat() is additive**: calling `wFormat(t,l,b,r)` adds to current margins. `wFormat()` with no args resets to (0,0,0,0). After `paintTopLevelWindow()`, margins are always 0. Each `paintChild()` sets margins, paints, resets.

**Event flow**: `HTApplication:exec()` → `getEvent()` (Inkey) → queue by priority (HIGH/NORMAL/LOW) → `widget:event()` → type-based dispatch → handler method. Keyboard events go to `activeWindow()`, mouse events to window under cursor. Right-click dispatches to `contextMenu:popup()`.

**Focus chain**: `HTWidget:FfocusWidget` tracks the focused child. Tab/Shift+Tab cycles via `focusNextChild()`/`focusPrevChild()`. Mouse click focuses via `childAt()` hit-testing. Focus policy: NONE/TAB/CLICK/STRONG.
`show()` only queues paint/focus/show events (LOW priority): a top-level window opens — and
becomes the keyboard target — when `app:exec()` handles them, so the last window shown wins.
`win:activate()` queues a focus-in behind them (the HTMainWindow shell is selected but stays
beneath); a `wSelect()` before `exec()` is overridden.

**Signal/callbacks**: Harbour code blocks on READWRITE properties. `btn:onClicked := {|| Save() }`, `chk:onToggled := {|l| ... }`, `cmb:onChanged := {|n| ... }`, `brw:onActivated := {|r,c| ... }`, `action:onTriggered := {|| ... }`.

**Widget lifecycle**: `widget:destroy()` recursively destroys children, closes windows, removes from parent's child list, clears focus references. `HTComboBox` overrides to close dropdown first.

**Targeted repaint**: `parent:repaintChild(self)` repaints a single child without clearing the entire window. Used by all controls for state changes.

**Geometry**: `move()` and `resize()` take effect at once (they also queue the move/resize event), so a
widget resized inside a running `HTDialog:exec()` paints with its new size. `HTDialog:move()` before
`exec()` keeps that position (otherwise the dialog is centered). `setTitleButtons( lHide, lMaximize,
lResize )` hides title bar buttons besides close (a message box: `.F., .F., .F.`).

**HTBrowse**: wraps a TBrowse (`browse()`), or shows an existing one: `setBrowse( oTBrowse )` (its
columns/blocks stay). `setColorSpec( c )` keeps own colors (NIL = theme). A paint redraws from the
rows TBrowse already read (`invalidate()`); position and colors are assigned only when they change
(each assignment makes TBrowse re-read every row) — after data changes call `refreshAll()` /
`refreshCurrent()`, as with a plain TBrowse. `stabilize()` leaves the screen cursor on the current
cell (a GET at `Row(), Col()` edits it in place).

**Theme system**: `HTTheme()` singleton, `HT_CLR_MAX` (57) color categories. All widgets use `HTTheme():getColor(HT_CLR_xxx)` instead of hardcoded colors, at paint time.
- A theme = one color string per category + optionally a **palette** (16 COLORREF values, `ht_rgb()`:
  what indexes 0..15 look like). `loadTheme( cName )` sets both; `applyPalette()` pushes the palette
  to the GT only when the GT reports one (gtwvt/gtxwc; not gtnul/most terminals).
  `ht_cuiApplyPalette()` = `HTTheme():applyPalette()`.
- Built-in, in this order: `Classic` (default), `Blue`, `Light`, `Dark`, `Amber` — ported from the
  Digi knjigovodstvo color schemes (`boje.prg`: palette + screen attributes; Blue = the old blue DOS
  look, white on blue with a green browse) — then `HighContrast`, `Mono` (DOS palette).
  `loadDefault()/loadClassic()/loadBlue()/loadLight()/loadDark()/loadAmber()/loadHighContrast()/loadMono()`.
  The Digi themes are built from a few roles (`ht_themeFromRoles()`: window, field, focus, list,
  select, browse, menu, status, title, …); built-in themes cannot be replaced or removed.
- Own themes: `setColor()` / `setPaletteColor()` change the current colors, `saveTheme( cName )` keeps
  them; `addTheme( cName, aColors, aPalette )`, `removeTheme()`, `themeNames()`, `themeColors()`,
  `themePalette()`, `isBuiltIn()`, `categoryName( nCat )`. Names are case-insensitive.
- After a change call `HTApplication():repaintAll()` (desktop + every top-level window in place).
  A palette change alone repaints by itself (the GT exposes the whole screen).
- `ht_usePalette( .F. )` (callable before `HTApplication():new()`): themes change the color strings
  only and never touch the GT palette — for apps that manage their own palette.

**Debug logging**: `ht_debugLog(cMessage)` writes to stderr when enabled via `HTApplication():setDebug(.T.)`. Capture with `./myapp 2>debug.log`.

### HTGet / TGet Integration

HTGet wraps Harbour's core `Get` class for full Clipper-compatible input editing:
- All PICTURE @ functions: `@! @A @B @C @D @E @K @R @S @T @X @Z @( @)`
- All template chars: `9 # A N X L Y ! $ *`
- Type-aware: numeric decimal alignment, date CToD validation, logical T/F
- Insert/Overwrite toggle (Ins key), Undo (Ctrl+U), word navigation (Ctrl+Left/Right)
- VALID block returning `.F.` rejects focus change (keeps editing)
- WHEN block returning `.F.` prevents focus entry
- Existing `Transform()` used for display; `oGet:buffer` for edit state

### Layout System

- `HTBoxLayout` / `HTVBoxLayout` / `HTHBoxLayout`: linear stacking with spacing, margins
- `addStretch(nFactor)`: flexible space that expands proportionally
- `addSpacing(nChars)`: fixed-size gap
- `addLayout(oLayout)`: nested layouts via internal container widget
- `HTGridLayout`: 2D grid with row/column spans, spacing, margins
- Size policies: `minimumWidth/Height`, `maximumWidth/Height` on HTWidget

### Unicode / CJK (Chinese, Japanese, Korean)

- **Codepage**: `HTApplication:new()` switches the VM to `UTF8EX` when the app left Harbour's
  default `"EN"` (an app that called `hb_cdpSelect()` first keeps its codepage). UTF8EX keeps
  `Len()/SubStr()/Left()` character-indexed — the editors' cursor arithmetic relies on that
  (plain `"UTF8"` is byte-indexed). Box frames still go through the GT box codepage (CP437).
- **SaveScreen cells**: under a UTF-8 codepage `HB_GTI_COMPATBUFFER` must be `.F.` (set in
  `HTApplication:new()` and `ht_cuiSetupDisplay()`): Clipper 2-byte cells store one byte per
  character, so every window restore of the SaveScreen window stack turned Unicode into `?`.
- **Typing**: `ht_keyText( nKey )` (HTKeyEvent.prg) is the text a key types, `""` for
  non-printing keys. One character but possibly several bytes — insert the whole string and
  advance the cursor by `Len()`; never test `Len( hb_keyChar( n ) ) = 1`.
- **Wide cells** (`ht_viewport.c`): a CJK character takes two screen cells. The `DispOutAt`
  remap writes it followed by a filler cell (the glyph spills over it) and clips at the
  viewport's right edge. The filler is a space, except after Hangul on Windows: there it is
  a zero width space (U+200B), because GDI ignores gtwvt's per-cell widths for Hangul and
  advances by the glyph's own two cells (a space made it three, and the run drifted right).
  A zero width space after Chinese/Japanese would pull the next glyph onto it — keep the space.
  Auto on for graphic GTs (gtwvt/gtxwc), off for text GTs (gttrm,
  gtnul); `ht_wideCells( lOn )` overrides. TBrowse paints through the core RTL, so
  `HTBrowse:paintEvent()` calls `ht_widenRegion()` right after `forceStable()` (fields end at
  a color change or a `DispOutAtBox` column separator; must only run on a freshly redrawn area).
- **Measure on-screen text in cells** (`src/HTCells.prg`): `ht_strWidth()`, `ht_cellLeft()`,
  `ht_cellPadR/L/C()` instead of `PadR/PadL/PadC()`, `ht_cellCol()` (cursor column),
  `ht_cellPos()` (mouse click → character), `ht_scrollToCursor()` (horizontal scroll offset).
  Character positions in the text stay `Len()`-based.
- **gtwvt font**: `ht_cuiSetupDisplay()` sets `HB_GTI_CODEPAGE` to `DEFAULT_CHARSET` (1) under
  UTF-8 — with the default OEM charset Consolas draws CJK as empty boxes; DEFAULT lets GDI fall
  back to a system CJK font. gtwvt repaints only changed cells; a repaint starting on a filler
  cell would erase the right half of the glyph, hence the whole-row `WM_PAINT` in `ht_winmouse.c`.
- **Symbols**: `ht_glyph( nUnicode, cFallback )` — Unicode symbol when the codepage has it, else
  ASCII. Title bar buttons `─ □/▫ ×` (3 cells each from width 24), resize grip `┘`, `√`, `─│`.
  Never write CP437 bytes (`e"\xC4"`, `Chr(251)`) through `DispOutAt` — under UTF8EX they are
  invalid UTF-8 (`DispOutAtBox`/`DispBox`/TBrowse separators are fine: box codepage).
- **gtxwc (Linux)** draws with one X core font; its default `fixed` has no CJK glyphs (empty
  boxes). `tools/unifont-cell.sh` (root; needs `xfonts-unifont pcf2bdf xfonts-utils python3`)
  builds "Unifont Cell" from the installed GNU Unifont: every glyph advances one 8-px cell (wide
  ones spill over their filler; gtxwc sizes cells by the widest advance and draws runs with each
  glyph's own advance) plus 4096 blank filler glyphs U+E100..U+F0FF. `ht_cuiSetupDisplay()` selects
  it with `HB_GTI_FONTSEL` when installed (UTF-8 only; FONTSEL leaves `fixed` for a missing font)
  and calls `ht_wideFillerBase( 0xE100 )`: filler = U+E100 + (char & 0xFFF). gtxwc repaints only
  changed cells, so a filler that changes with its character also repaints the glyph's right half
  (a plain space left the old glyph's half behind). Never put anything drawn at U+E040..U+E076:
  the Harbour GTs draw their own box/arrow symbols there. Unifont has no zero-width glyph (ZWSP and
  the other format characters are visible 16-px boxes).

### Coordinate Convention

`move(x, y)` where **x = column, y = row**. `HTPoint(x, y)` same. `wOpen(top, left, bottom, right)` takes row-first. In `paintTopLevelWindow()`, `wOpen(::Fx, ::Fy, ...)` passes x(col) as top(row) — this works because `move()` stores x=col in Fx, and the window position happens to be correct, but the naming is misleading.

### Event System Coordinates

Four coordinate spaces used in the event system:

| Space | Functions | Range | When to use |
|-------|-----------|-------|-------------|
| Screen absolute | `mRow(.T.)`, `mCol(.T.)`, `wRow()`, `wCol()` | 0..MaxRow/MaxCol of desktop | Positioning new windows (wOpen, wMove) |
| Window-relative | `mRow()`, `mCol()` (with window selected via wSelect) | 0..window height/width | Hit-testing within a window, mouse event coords |
| Content-area | `child:x`, `child:y` | 0..content width/height (inside border) | Child widget positioning in parent |
| Viewport | `MaxRow()`, `MaxCol()` inside active `wFormat()` | Varies by current margins | Widget painting inside paintChild viewport |

**HTMouseEvent** captures both absolute and window-relative coords at creation:
- `mouseRow`, `mouseCol` — window-relative (relative to wSelect'd window at creation time)
- Used by event handlers for hit-testing and delta calculations

**Key rules:**
- `wFormat()` is additive — always reset with `wFormat()` (no args) after use
- `wMove(nRow, nCol)` takes screen-absolute position (not delta)
- `mRow()` returns coords relative to currently selected window + current margins
- After `wBox()` + `wFormat()` reset, margins are (0,0,0,0)

### Property System

`include/property.ch` defines PROPERTY macros. Key patterns:
- `PROPERTY name INIT value` → read-only with DATA Fname PROTECTED
- `PROPERTY name WRITE setter INIT value` → WRITE keyword BEFORE INIT
- `PROPERTY name READ getter WRITE setter` → custom accessors
- `PROPERTY name READWRITE` → auto-generates setter (use for callbacks: `PROPERTY onClicked READWRITE`)

### Harbour Gotchas

- `LOCAL` declarations MUST come before any executable statements
- `wFormat()` without args resets margins (gets current, negates, adds back)
- `wFormat()` with args is additive to current margins
- `DATA Ftype INIT value` in subclass overrides parent's INIT value
- `-es2` flag treats warnings as errors — unused variables/params will fail build
- `K_HOME = K_CTRL_A = 1`, `K_PGDN = K_CTRL_C = 3`, `K_DOWN = K_CTRL_X = 24` — same key codes, can't have both in SWITCH. Use `hb_gtInfo(HB_GTI_KBDSHIFTS)` + `HB_GTI_KBD_CTRL` to detect Ctrl modifier.
- Callback properties MUST use `READWRITE` or `WRITE setter` — plain `PROPERTY onFoo` is read-only
- `PROPERTY readOnly ...` is broken: the preprocessor parses `FreadOnly` as `F` + scope keyword `READONLY`, naming the DATA `"F"` instead of `"FreadOnly"`. Workaround: declare `DATA FreadOnly INIT .F.` explicitly in `PROTECTED:` and use `METHOD readOnly() INLINE ::FreadOnly` + `METHOD _readOnly(b) INLINE ::FreadOnly := b` in `PUBLIC:`. Same issue would affect any property whose name case-insensitively matches a scope keyword: `EXPORTED`, `EXPORT`, `VISIBLE`, `PUBLIC`, `PROTECTED`, `HIDDEN`, `PRIVATE`, `READONLY`, `RO`, `PUBLISHED`.
- The compiler folds `Len()`, `Chr()`, `PadR()` … of **literals** at compile time, in bytes:
  `Len( "王小明" )` is 9 even under UTF8EX and `Chr( 9474 )` is `Chr( 2 )`. Measure strings at
  runtime (variables) and build symbols with `hb_UChar()` / `ht_glyph()`.
- Binary strings (SaveScreen buffers, file data) under UTF8EX: use `hb_BLen()/hb_BSubStr()/
  hb_BCode()` — `Len()/SubStr()` count UTF-8 characters.

## Source Layout

```
include/          hbtui.ch (constants, color categories, alignment), property.ch (macros)
src/              Core classes (HTBase, HTObject, HTApplication, HTEventLoop), layouts, C API,
                  HTCells.prg (screen-cell text helpers for wide CJK characters, ht_glyph)
src/HTWindow/     SaveScreen window API (HTWinApi) + viewport C helpers (ht_viewport.c)
src/HTEvent/      Event classes (HTEvent, HTKeyEvent, HTMouseEvent, etc.)
src/HTWidget/     Widget classes (25+ widgets, HTGetBackend, HTToast)
src/types/        HTPoint, HTSize, HT
legacy/         Prior win.prg / cui.ch kept for reference
tools/            unifont-cell.sh/.py: builds the "Unifont Cell" X font (CJK in gtxwc)
docs/             Documentation
  EVENT_SYSTEM.md HTEventLoop guide, event types, routing, focus, tasks, coordinates
samples/          Demo applications (human-facing, require a display)
  hello/          Minimal Hello World (HTMainWindow + HTLabel)
  dialog/         Modal HTDialog + HTMessageBox (info/question/custom)
  browse/         HTBrowse data grid with inline editing and context menu
  controls/       PushButton, CheckBox, RadioButton, Spinner, ComboBox, ListBox
  get/            HTGet demo (string, numeric, date PICTURE, validation)
  grid_layout/    HTGridLayout 2D form arrangement
  hbox_layout/    HTHBoxLayout horizontal button bar
  editor/         HTLineEdit + HTTextEdit with clipboard support
  helpline/       Per-widget help text in status bar
  layout/         HTVBoxLayout demo (spacing, stretch, nesting)
  menu/           HTMenuBar + HTMenu + HTAction with keyboard shortcuts
  progress/       Animated HTProgressBar with scheduleRepeat()
  theme/          Theme switching demo (7 built-in themes, F1-F7, theme list + buttons)
  theme_editor/   Theme editor: change category colors (Text/Back spinners or a color string) and
                  the 16-color palette (R G B), save as own themes, themes.json persistence,
                  preview window (GET, combo, buttons, progress, browse), toasts, message box
  toast/          HTToast non-blocking notifications (4 styles, stacking)
  window/         window_01.prg: overlapping floating windows over an HTMainWindow
                  shell — "Dialog" (2 HTGets + OK/Cancel), "Products" and
                  "Customers" (HTBrowse grids). Exercises the window stack:
                  drag/resize/maximize/minimize, click-to-raise, shadows,
                  Ctrl+F6 window cycling, Enter-to-next-field, cursor restore
  unicode/        Chinese/Japanese/Korean/Cyrillic text: IME input in HTGet, HTLineEdit,
                  HTTextEdit and the HTBrowse editor, wide (2-cell) characters
  showcase/       Full controls demo with all features
tests/            Automated test suite (headless, -gtnul, exit code 0/1)
  test_basic      HTPoint, HTSize, all HTEvent subclasses
  test_eventloop  HTEventLoop schedule, scheduleRepeat, cancelTask, runPendingTasks
  test_layout     HTBoxLayout, HTGridLayout item management
  test_theme      HTTheme singleton, all built-in themes and palettes, own themes (save/add/
                  replace/remove, built-ins protected), category names, invalid index fallback
  test_widgets    HTLabel, HTLineEdit, HTCheckBox, HTPushButton, HTListBox,
                  HTComboBox, HTCheckList, HTSpinner, HTProgressBar, HTScrollBar,
                  HTTextEdit, HTContextMenu, HTMenu:items() (submenu order),
                  Unicode: UTF8EX default, CJK typing (ht_keyText, editors, HTGet),
                  wide-cell helpers, SaveScreen keeping Unicode, ht_glyph
  test_browse     HTBrowse column management, navigation blocks, callbacks,
                  inline cell editing (F2/typing, Enter/Esc, typed numeric save),
                  setBrowse (external TBrowse), own colors, repaint without re-reading rows
  test_get        HTGet setup/getValue/setValue, label/picture/width, readOnly,
                  Enter moves to the next field (VALID rejection keeps focus)
  test_integration Focus cycle (flat + nested layouts), layout geometry,
                  childAt recursion, repaintChild delegation
```
Posle svake završene izmene radim commit i push na main. 