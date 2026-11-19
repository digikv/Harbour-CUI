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

Requires: Harbour compiler at `~/git/core` (Windows: always the newest one,
`D:\tools\harbour-core\bin\win\mingw\hbmk2.exe`, 32-bit MinGW).

GT driver: `gtxwc` on Linux (X11 window), `gtwvt` on Windows. Tests use `gttrm` / `-gtnul`.

Platform code is split with `#ifdef __PLATFORM__WINDOWS` (note: Harbour defines only the
upper-case form; `__PLATFORM__Windows` is never defined):
- `HTEventLoop:poll()` — Win32 WndProc mouse queue (Windows) vs portable `Inkey()` path
- `HTApplication:exec()` — installs the Win32 mouse bridge (`ht_winmouse.c`, stubs elsewhere).
  It subclasses the gtwvt window with the **W** API (`SetWindowLongPtrW`/`CallWindowProcW`):
  hbmk2 compiles C as ANSI by default, and an ANSI subclass turns the window into an ANSI
  window — IME/Unicode `WM_CHAR` (Chinese, Cyrillic on a Western system, …) then reaches
  gtwvt as `?`. Its `WM_PAINT` hook also widens the repaint to whole rows once wide (CJK)
  text was written, and draws rows holding Indic text again on the cell grid (see Unicode below)
- `ht_cuiSetupDisplay()` — gtwvt fullscreen + Consolas font fit (Windows) vs a resizable
  `SetMode( nRows, nCols )` window in the GT's own font (Linux)
- `HTApplication:exec()` event mask — non-Windows adds `HB_INKEY_GTEVENT`, so gtxwc's
  `HB_K_RESIZE` (grid follows the window size) reaches `HTEventLoop:handleResize()`,
  which repaints the desktop, refits `HTMainWindow` (`screenResized()`: bottom status
  bar re-docked) and repaints every window. gtxwc needs X core fonts (`xfonts-base`).

The portable path can be checked on Windows by building a copy of the library with
`__PLATFORM__WINDOWS` renamed in those three files.

Compiler flags: `-w3 -es2` (warnings level 3, warnings as errors) in `hbtui.hbp` and in every
`samples/*/hbmk.hbm` and `tests/hbmk.hbm` — newer Harbour builds report more warnings (e.g.
W0032 for an assignment never read), so keep the code free of them everywhere.

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
      HTSparkline, HTBarChart (block-character charts, eighths of a row; idea after Ratatui)
      HTMenuBar, HTMenu, HTContextMenu (right-click popup)
      HTMessageBox (singleton: information/warning/error/question, choose( cTitle, cMsg,
                    aButtons, nKind ) like Alert(); wrapped multi-line text, kind badge),
      HTScrollBar, HTSpinner, HTCheckList
      HTTabWidget, HTToolBar
      HTTheme (singleton: named themes with palettes; Classic/Blue/Light/Dark/Amber from
               Digi knjigovodstvo, HighContrast, Mono; own themes)
      HTToast (singleton, non-blocking notifications with auto-dismiss)
    HTAction (menu item with onTriggered callback)
    HTLayout → HTBoxLayout → HTHBoxLayout, HTVBoxLayout
               HTGridLayout
  HTLayoutItem (alignment mixin)

HTEventLoop (singleton, unified event polling + task scheduling; a task is removed /
             rescheduled before it runs, so one that opens a modal dialog is not re-entered)
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

**Mouse grab**: a widget whose `mouseDragWanted()` returns .T. (drawing, dragging) gets, after a left
or right button down on it, every `K_MOUSEMOVE` and the button up from its window
(`HTWidget:FmouseGrab`, `forwardToGrab()`), in its own coordinates even outside it. Other children
get clicks only; right-click otherwise goes to a context menu.

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
`exec()` keeps that position (otherwise the dialog is centered). `addButtonBar( "OK", "Cancel" )` lays the
buttons out right-aligned in the given order (Tab order too); the first accepts, the last rejects.
A dialog has a drop shadow. Its modal loop (`HTEventLoop:run()`) turns the Win32 queue's screen-absolute
mouse cells into the dialog window's coordinates, as `HTApplication:exec()` does for top-level windows. `setTitleButtons( lHide, lMaximize,
lResize )` hides title bar buttons besides close (a message box: `.F., .F., .F.`).

**HTMenuBar**: `"&File"` / `"Save &as"` mark hot keys (shown without the `&`, the letter in
`HT_CLR_MENU_HOTKEY(_SEL)`): Alt + a title's hot key (else its first letter) opens it; on the active
bar a title's letter opens it; in an open dropdown / submenu an entry's letter runs it.
`action:setShortcut( K_CTRL_B )` runs an action from anywhere (bar inactive or open). `activate()`,
`isActive()`, `activeMenu()`, `popup( n )` (opens menu n, first entry highlighted: F-key menus); `setBarRow( 0 )` draws the bar on its window's title row (a one-row
window as a screen menu bar on row 0; the window routes clicks: `handleClick( 0, nCol )` /
`handleMouseClick( nAbsRow, nAbsCol )`; a click on that title row opens a menu instead of
dragging the window). Inkey Alt codes follow the keyboard rows
(K_ALT_Q = 272, K_ALT_A = 286, K_ALT_Z = 300) — `hb_keyCode( "Alt+F" )` is the code of `"A"`.

**HTBrowse**: wraps a TBrowse (`browse()`), or shows an existing one: `setBrowse( oTBrowse )` (its
columns/blocks stay). `setColorSpec( c )` keeps own colors (NIL = theme). A paint redraws from the
rows TBrowse already read (`invalidate()`); position and colors are assigned only when they change
(each assignment makes TBrowse re-read every row) — after data changes call `refreshAll()` /
`refreshCurrent()`, as with a plain TBrowse. `stabilize()` leaves the screen cursor on the current
cell (a GET at `Row(), Col()` edits it in place). `alignLeft := .T.`: TBrowse centers columns that
fit (half the free width on the left) - with it the TBrowse ends with its last column
(`fitWidth()`) and the rest of the row is blanked. `extraColors`: color pairs appended to the
theme's six, for column `colorBlock`s (pair 7, 8, ...).

**Theme system**: `HTTheme()` singleton, `HT_CLR_MAX` (59) color categories. All widgets use `HTTheme():getColor(HT_CLR_xxx)` instead of hardcoded colors, at paint time.
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
- Shift + Left/Right/Home/End/Ctrl+Left/Right selects (`setSelection( nFrom, nTo )`, `selectedText()`,
  `hasSelection()`, `editBuffer()`); Del/Backspace delete it, typing replaces it. Only editable
  positions are deleted, right to left (`HTGetBackend:editable()` = TGet's PROTECTED `IsEditable()`),
  so picture literals stay. TGet's cursor cannot pass the last character: `FlSelLast` covers it.

### Pixel fonts (`src/HTPixelFont.prg`, Harbour only)

- `.glyphs` file: `char A` / `glyph E000 name` + 8 rows of `#`/`.` (wider = more cells), `# ` comments,
  `# font: <file.ttf> <Family> [<x11dir>]`. `ht_glyphsLoad()` / `ht_glyphsSave()` keep comments and
  order; `ht_glyphsCells()` splits sprites into 8x8 cells; `ht_glyphsBuild()` makes what `# font:` says.
- `ht_pixelFontTTF()` writes the TrueType tables itself (head hhea maxp OS/2 hmtx cmap-4 loca glyf name
  post, one clockwise rectangle per pixel run, checksums, fixed timestamps). Pitfalls: GDI rejects a
  font without name IDs 1-6; `hmtx` lsb must equal the glyph's xMin (head flag bit 1) or renderers
  shift the glyph to the left edge; maxp 1.0 is exactly 32 bytes; table tags in binary order.
- `ht_pixelFontX11()` writes BDF + fonts.dir (identical to what fontTools-based tools produced).

### Own fonts and game input (`ht_winmouse.c`)

- `ht_fontLoad( cFile )`: `AddFontResourceEx( FR_PRIVATE )` - the font is usable by this process
  only, no installation; then `hb_gtInfo( HB_GTI_FONTNAME, cFamily )`. Set `HB_GTI_CODEPAGE` 1
  (DEFAULT_CHARSET) so GDI takes the font as it is. Sprites live at U+E000.. (Private Use Area).
- `ht_keyDown( nVK )`: `GetAsyncKeyState`, only while the GT window is in the foreground - held keys,
  several at once (move + fire). `ht_keyStateAvailable()` is .F. where it does not work: read Inkey().
- A process that is not DPI aware is stretched by Windows at 125%/150%: pixel fonts blur. A
  manifest with `dpiAware true/pm` (samples/game/nebula.rc) keeps them crisp.
- **Linux / gtxwc** (`src/HTWindow/ht_x11.c`, a file of its own so only programs that call these
  link Xlib - `{unix}-lX11` in their hbmk.hbm): `ht_fontLoad( cDir )` adds a folder holding
  `fonts.dir` + BDF fonts to the X server's font path (`XSetFontPath`, like `xset +fp`); select with
  `HB_GTI_FONTSEL` and an XLFD. `ht_pixelFontX11()` writes BDFs at 16..48 px (font
  pixels as whole squares). `ht_keyDown()` = `XQueryKeymap`, same Windows VK codes, only while the
  window titled `HB_GTI_WINTITLE` has the focus (focus window, a parent or a child; with
  PointerRoot the window under the pointer). gtxwc stores WM_NAME as UTF8_STRING: read it with
  `XGetWMName`, `XFetchName` returns nothing. A resized gtxwc window changes the grid: a game sets
  `HB_GTI_RESIZABLE .F.`. BDF / fonts.dir must keep LF (`.gitattributes`).

### Charts (HTSparkline, HTBarChart)

- Values scale to eighths of a row (`ht_chartScale()`); `ht_blockGlyph( nEighths )` = ▁..█.
  A **full cell is drawn as a space in the bar color** (`ht_chartFullColor()`): █ leaves gaps
  between rows. Consolas (gtwvt) has only ▄ █, not U+2581..U+2587 - GDI shows a box and does not
  fall back to another font - so `ht_fontHasChar( cFont, nUnicode )` (`ht_winmouse.c`; .T. off
  Windows) switches to halves. Glyphs are cached per codepage + font.
- Colors: default = the theme's progress fill color on the window background (`ht_chartColor()`);
  an own color may be a foreground only ("G" -> "G/<window bg>"). Named colors follow the theme
  palette (in Classic "R+" is gray): take colors from the theme (e.g. a toast background).
- `HTBarChart`: bars `{ cLabel, nValue [, cColor ] }`, values row (`valueText()`: 12k, 2.5M,
  or `valueBlock`) and labels row under the bars; `barWidth`, `barGap`, `maximum`.
- `HTSparkline`: last width() values, `addValue()` (keeps `maxCount`), `minimum`/`maximum`.

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
- **SaveScreen cells**: under a multi-byte codepage (`ht_cdpIsMultiByte()`: UTF-8, CP950/Big5, …)
  `HB_GTI_COMPATBUFFER` must be `.F.` (set in `HTApplication:new()` and `ht_cuiSetupDisplay()`):
  Clipper 2-byte cells store one byte per character, so every window restore of the SaveScreen
  window stack turned Chinese into `?`.
- **CP950 (Big5)**: Harbour's CP950 is multi-byte but byte-indexed (`Len()` counts bytes), so the
  editors' cursor would split a character. Keep the VM in UTF8EX and open Big5 tables with
  `USE … CODEPAGE "CP950"` (the RDD translates fields both ways).
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
- **Indic scripts** (U+0900..U+0D7F: Devanagari, Bengali, Gurmukhi, Gujarati, Oriya, Tamil, Telugu,
  Kannada, Malayalam): one cell per code point (vowel signs and viramas too), so `Len()` positions
  stay. gtwvt draws a same-colored run with one `ExtTextOut()`: Windows shapes it but advances by
  the glyphs' own widths, so the rest of the run drifted off the grid. `ht_winmouse.c`
  (`ht_indic_paint()`, after gtwvt's `WM_PAINT`) draws every run holding Indic text again: each
  syllable (base + marks, consonants joined by a virama / ZWJ) shaped in Nirmala UI, centered on its
  cells (narrowed by a world transform when wider), other characters one per cell in a copy of the
  GT font. Not done on gtxwc (Linux): there each code point stays in its own cell, unshaped.
- **Measure on-screen text in cells** (`src/HTCells.prg`): `ht_strWidth()`, `ht_cellLeft()`,
  `ht_cellPadR/L/C()` instead of `PadR/PadL/PadC()`, `ht_cellCol()` (cursor column),
  `ht_cellPos()` (mouse click → character), `ht_scrollToCursor()` (horizontal scroll offset).
  Character positions in the text stay `Len()`-based.
- **gtwvt font**: `ht_cuiSetupDisplay()` sets `HB_GTI_CODEPAGE` to `DEFAULT_CHARSET` (1) under
  UTF-8 — with the default OEM charset Consolas draws CJK as empty boxes; DEFAULT lets GDI fall
  back to a system CJK font. gtwvt repaints only changed cells; a repaint starting on a filler
  cell would erase the right half of the glyph, hence the whole-row repaint in `ht_winmouse.c`:
  every partial-width rectangle of the update region (`GetUpdateRgn`, not the bounding
  `GetUpdateRect`) is collected and invalidated as whole rows by a posted message, after
  `WM_PAINT` returned. Never call `InvalidateRect()` inside `WM_PAINT`: Windows then repaints the
  whole window with background erase (every caret blink flashed the screen, a loop at ~20 Hz).
- **IME** (`ht_winmouse.c`): on `WM_IME_STARTCOMPOSITION` / `WM_IME_COMPOSITION` /
  `IMN_OPENCANDIDATE` the composition window goes to the caret (gtwvt keeps the system caret
  on the GT cursor) and the candidate list below the cursor row (imm32 loaded at run time).
  `ht_fullScreen( .F. )` before `ht_cuiSetupDisplay()`/`exec()`: a maximized window instead
  of fullscreen, the taskbar (IME mode indicator) stays visible (the unicode sample uses it).
  `ht_inputLanguage()` (`ht_winmouse.c`): the thread's keyboard layout as an ISO 639 code
  ("HR", "GU") + for Chinese/Japanese/Korean the IME mode ("ZH 中" / "ZH A", "JA あ" / "JA カ",
  "KO 가"); "" off Windows. The unicode sample and CBro poll it (300 ms) into status bar section 1.
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
- `K_HOME = K_CTRL_A = 1`, `K_PGDN = K_CTRL_C = 3`, `K_DOWN = K_CTRL_X = 24`, `K_INS = K_CTRL_V = 22` — same key codes, can't have both in SWITCH. Use `hb_gtInfo(HB_GTI_KBDSHIFTS)` + `HB_GTI_KBD_CTRL` to detect Ctrl modifier.
- Callback properties MUST use `READWRITE` or `WRITE setter` — plain `PROPERTY onFoo` is read-only
- `PROPERTY readOnly ...` is broken: the preprocessor parses `FreadOnly` as `F` + scope keyword `READONLY`, naming the DATA `"F"` instead of `"FreadOnly"`. Workaround: declare `DATA FreadOnly INIT .F.` explicitly in `PROTECTED:` and use `METHOD readOnly() INLINE ::FreadOnly` + `METHOD _readOnly(b) INLINE ::FreadOnly := b` in `PUBLIC:`. Same issue would affect any property whose name case-insensitively matches a scope keyword: `EXPORTED`, `EXPORT`, `VISIBLE`, `PUBLIC`, `PROTECTED`, `HIDDEN`, `PRIVATE`, `READONLY`, `RO`, `PUBLISHED`.
- The compiler folds `Len()`, `Chr()`, `PadR()` … of **literals** at compile time, in bytes:
  `Len( "王小明" )` is 9 even under UTF8EX and `Chr( 9474 )` is `Chr( 2 )`. Measure strings at
  runtime (variables) and build symbols with `hb_UChar()` / `ht_glyph()`.
- `AScan( aArray, xValue )` compares with `=`: with SET EXACT OFF `".ch" = ".c"` and `"no" = ""` are
  .T. Compare exactly with `hb_AScan( aArray, xValue,,, .T. )`. Likewise `".c" $ ".frm|.ch"` is .T.
  (a substring): test a value against a list with an array, not with `$`.
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
                  pixelfont.prg: builds the fonts of a .glyphs file (src/HTPixelFont.prg, no external tools)
docs/             Documentation
  EVENT_SYSTEM.md HTEventLoop guide, event types, routing, focus, tasks, coordinates
  DEPENDENCIES.md What the library, samples and CBro use (Harbour, fonts, XLSX class,
                  LetoDB/LetoDBf, contribs, Linux packages)
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
  fonteditor/     Pixel font editor for .glyphs files: list, pixel grid (mouse draw/erase with drag,
                  keys), 1:1 + 2:1 preview, transforms, undo, copy/paste, new character / sprite
                  dialog, Build = ht_glyphsBuild() (TTF + X11 fonts, no external tools)
  game/           NEBULA RAID: fixed-screen shooter drawn only with its own 8x8 font (nebula8.ttf
                  from nebula.glyphs via ht_glyphsBuild()): ZX Spectrum style 32x24 cells,
 16-color palette, ht_fontLoad() + ht_keyDown(); DPI-aware manifest (nebula.rc);
 Linux: X11 bitmap fonts in x11/ (gtxwc). Sound: 8-bit WAVs synthesized at start,
 played async (PlaySound from memory / aplay; Tone() blocks the frame); high
 scores in nebula.hi (git-ignored); diving mantas from wave 3
  charts/         HTSparkline (live, 4 rows; 1 row in its own color) + HTBarChart, F1-F7 themes
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
  unicode/        Chinese/Japanese/Korean/Gujarati/Hindi/Cyrillic text: IME input in HTGet,
                  HTLineEdit, HTTextEdit and the HTBrowse editor, wide (2-cell) characters,
                  Indic syllables
  showcase/       Full controls demo with all features
  cbro/           CBro: xBase browser, BroPlus successor (own cbro.hbp + build.bat; dual license
                  GPL-3+ / commercial, see samples/cbro/LICENSE): DBF, LetoDB/LetoDBf, SQL (SQLMIX),
                  .xlsx via the user's XLSX class (../../../xlsx), scripts, dynamic indexes, memo arrays/JSON
  cedit/          CEdit: text editor with HbEdit's keys (F2 save, F3 block, F4 open, F9 menu,
                  F10/Esc close, F12 files), written from scratch (own cedit.hbp + build.bat; dual
                  license like cbro). htcodeedit.prg = HTCodeEdit widget (tabs, wide cells, stream +
                  vertical block selection, clipboard, mouse drag), cebuffer.prg = CEBuffer (lines,
                  cursor, grouped undo/redo, UTF-8 / CP852 / 35 codepages, Big5 + GBK auto on a Chinese system, CRLF/LF, BOM). Ctrl-Z vs Ctrl-Left (both
                  Inkey 26): ht_keyDown( 'Z' ). 1.0: search / replace (F7 ...), go to, Ctrl-B, Alt-B,
                  cesyntax.prg = highlighting (8 languages, states cached per line in CEBuffer,
                  colors picked against the theme's palette; more languages from cedit.lang in the
                  format of HbEdit's hbedit.ini [LANG_xxx], ce_SyntaxLoad), Shift-F8 codepage, cedit.ini (settings,
                  recent files with the cursor). "Turbo Pascal" editor colors = an own theme with the
                  VGA palette (ce_TurboTheme). cetabs.prg = CETabBar (a tab per open file, x closes).
                  Ctrl-F / Ctrl-H (codes of End / Backspace) reach CEdit through HTCodeEdit:bCtrlKey.
                  build.bat / build.sh build the library first when it is missing. Linux tests: WSL
                  Ubuntu-24.04 with Xvfb + xdotool (hidden display: no keys reach the user); UI tests
                  on Windows that send keys steal the user's keyboard: only when the user is away.
                  On main since 1.0; releases cedit-1.0, cedit-1.0.1 (-win32.zip, -linux-x86_64.tar.gz).
  cnc/            CNC: two-panel file manager like Norton Commander, keys of HbCommander (Kresin) + NC,
                  written from scratch. cncpanel.prg = CNCPanel (draws the NC frame, full / brief NC / brief TC (columns), marks,
                  quick search, sort), cnc.prg = menus, F-keys, copy/move/delete (recursive), command line
                  (hb_processRun, output Ctrl-W), "Norton" theme (VGA palette), cnc.ini. F3/F4 = CEdit's
                  HTCodeEdit + CEBuffer in a full-screen HTDialog (cnc.hbp links ../cedit/*.prg).
                  cncnet.prg = Net menu: CNCFtp (hbtip TIPClientFTP) behind CNCPanel:oNet, net use /
                  net view (Windows). cncwin.c CNC_RUNHIDDEN: commands with CREATE_NO_WINDOW (hb_processRun
                  opened a black console window). Test FTP: WSL python3-pyftpdlib on port 2121.
  cuibuilder/     CuiBuilder: builds Harbour programs / libraries like HwBuilder (Kresin), written from scratch
                  (own cuibuilder.hbp + cuibc.hbp + build.bat/.sh; dual license like cbro). cbengine.prg = the
                  engine (no UI): cuibuild.ini (sections of hwbuild.ini; %APPDATA%\CuiBuilder or
                  ~/.config/cuibuilder, auto-detected), command templates per family (mingw gcc clang msvc
                  bcc; cmd_comp/res/lib/exe), .hwprj read/written (:project parts, {filters}, @lists, sub
                  projects; read with lEdit=.T. keeps filters/macros raw for saving), .hbp/.hbm/.hbc read
                  (${hb_name} in an .hbc = its own name), _cbstub.c sets the default GT (as hbmk2), make
                  mode by obj time vs source + local #includes, relink when an object / library is newer.
                  cuibuilder.prg = screen (files, options, messages, CEdit editor at the error line),
                  cuibc.prg = command line (gtcgi). cbwin.c CB_RUNHIDDEN / CB_STARTPROGRAM. Focus a child
                  with setFocusChild() (setFocus() does not move it). cuibuilder.hwprj builds itself.
                  cbrepdes.prg = report designer: Clipper .frm (CBR_Load / CBR_Save), columns from a
                  DBF (F5), try in Word / Excel (F6/F7, CB_OPENDOC / xdg-open), F9 generates
                  rpt_<name>.prg (Rpt_<Name>( cFile, bFor, cTitle, @cErr, cCdp )) and adds it, the shared
                  module, DOCXClass / XLSXClass ([REPORTS] of cuibuild.ini) and hbziparc.hbc to the project.
  shared/         cbreport.prg: Report Form to Word (.docx) / Excel (.xlsx), used by CBro and CuiBuilder:
                  CBR_Load / CBR_Save (writes the 1990-byte .frm) / CBR_Rows / CBR_Docx / CBR_Xlsx, a report
                  in code: CBR_New / CBR_Title / CBR_Group / CBR_Column / CBR_Make (.docx or .xlsx by the
                  extension). -DCBR_NO_DOCX / -DCBR_NO_XLSX (CBro: CBRO_NO_*); the classes need hbziparc.hbc.
tests/            Automated test suite (headless, -gtnul, exit code 0/1)
  test_basic      HTPoint, HTSize, all HTEvent subclasses
  test_eventloop  HTEventLoop schedule, scheduleRepeat, cancelTask, runPendingTasks
  test_layout     HTBoxLayout, HTGridLayout item management
  test_theme      HTTheme singleton, all built-in themes and palettes, own themes (save/add/
                  replace/remove, built-ins protected), category names, invalid index fallback
  test_widgets    HTLabel, HTLineEdit, HTCheckBox, HTPushButton, HTListBox, HTSparkline, HTBarChart, HTPixelFont (.glyphs, TTF checksum, BDF),
                  HTComboBox, HTCheckList, HTSpinner, HTProgressBar, HTScrollBar,
 HTTextEdit, HTContextMenu, HTMenu:items() (submenu order),
 HTMenuBar hot keys / Alt / shortcuts / popup(), setTitleButtons,
                  Unicode: UTF8EX default, CJK typing (ht_keyText, editors, HTGet),
                  wide-cell helpers, SaveScreen keeping Unicode, ht_glyph
  test_browse     HTBrowse column management, navigation blocks, callbacks,
                  inline cell editing (F2/typing, Enter/Esc, typed numeric save),
                  setBrowse (external TBrowse), own colors, repaint without re-reading rows
  test_get        HTGet setup/getValue/setValue, label/picture/width, readOnly, Shift+arrow selection,
                  Enter moves to the next field (VALID rejection keeps focus)
  test_integration Focus cycle (flat + nested layouts), layout geometry,
                  childAt recursion, repaintChild delegation, mouse grab (mouseDragWanted)
```
Posle svake završene izmene radim commit i push na main. 