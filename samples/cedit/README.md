# CEdit

Text editor in character mode, built on Harbour-CUI.
Copyright (c) 2026 Srđan Dragojlović. Dual licensed: GNU GPL version 3 or later (`COPYING`), or a
commercial license from the author — see `LICENSE`.

Made to work like HbEdit by Alexander S. Kresin — the same F-keys, so those who are used to it
feel at home — but written from scratch on Harbour-CUI: no HbEdit source code is used.

## Download

Ready-to-run programs, no build needed: **[CEdit 1.0.3 release](https://github.com/digikv/Harbour-CUI/releases/tag/cedit-1.0.3)**

| File | Platform |
|---|---|
| `cedit-1.0.3-win32.zip` | Windows 7 and newer (32-bit, runs on 64-bit Windows too) |
| `cedit-1.0.3-linux-x86_64.tar.gz` | Linux x86_64 (X11); needs `libx11-6` and `libgpm2` |

Unpack and run `cedit` or `cedit file.txt`. To build CEdit yourself, see
[Building CEdit, step by step](#building-cedit-step-by-step).

New in 1.0.5: large files use a **disk line index**, an LRU **page cache** and a
**piece table** with an append-only `.add` swap — editable, without holding the
whole file in RAM. See [Large files](#large-files).

New in 1.0.4: first large-file support (paged view).

New in 1.0.3: Chinese files in Big5 (Traditional) and GBK (Simplified); the codepage AUTO
(the default): Big5 / GBK on a Chinese Windows, else CP852; all the codepages of Harbour's
character tables (Latin 1/2/9, Cyrillic, Greek, Turkish, Baltic, Hebrew ...), YUSCII last.

New in 1.0.2: the program icon; no Harbour debugger inside (Alt-D opened it); the text cursor
is hidden in lists.

New in 1.0.1: the mouse wheel and two-finger scrolling on a touchpad scroll the text and the lists.

## Keys

F9 opens the menu (also Alt + the underlined letter, or the mouse); the menu shows the key of every
command. F1 shows the keys in the editor.

| Key | Command |
|---|---|
| F2 / Shift-F2 | save / save as |
| F4 / Ctrl-O | open (recent files) / open file (Windows dialog) |
| Shift-F4 | new file |
| F12, Ctrl-Tab, Shift-Tab | list of open files, next, previous (also the tabs under the menu) |
| F10, Esc | close the file (the last one ends CEdit); Esc first cancels a selection |
| Alt-X | exit |
| F3 / Ctrl-F3 | block / vertical block: the cursor keys select, F3 again ends it |
| Shift + cursor keys, mouse | select (drag; double click: a word; Shift + click extends) |
| Ctrl-A | select all |
| Ctrl-C, Ctrl-Ins / Ctrl-X, Shift-Del / Ctrl-V, Shift-Ins | copy / cut / paste (a vertical block is pasted as a block) |
| Ctrl-Y | delete line |
| Ctrl-Z, Alt-Backspace / Ctrl-R | undo / redo |
| Ins | insert / overwrite |
| Tab | spaces to the next tab stop; with several lines selected: indent |
| F7, Ctrl-F / Ctrl-F7, Ctrl-H | find / replace (case, whole words; replace asks: Yes / No / All / Cancel) |
| Shift-F7 / Alt-F7 | next / previous |
| Alt-8 / Alt-3 | the word at the cursor, forwards / backwards |
| Alt-F8 | go to line[,column] |
| Ctrl-B | matching bracket |
| Alt-B | back to where the cursor was before a jump |
| F8 | syntax highlighting on / off (View - Language: choose the language) |
| Shift-F8 | codepage: an unchanged file is read again in it, a changed one is saved in it |
| Home | first text of the line, again: line start |
| Ctrl-Left / Ctrl-Right | word left / right |
| Ctrl-Home / Ctrl-End, Ctrl-PgUp / Ctrl-PgDn | start / end of the text, top / bottom of the screen |
| Ctrl-Up / Ctrl-Down | scroll a line |

The menu View also has Theme, Editor colors (theme colors, Turbo Pascal - yellow on blue with the
VGA palette, white on blue, ...) and Options (tab size, Tab with spaces, highlighting, the codepage
of files that are not UTF-8).

## Tabs

Every open file has a tab under the menu, as in Notepad++: a click on a tab shows that file,
a click on `×` (on the active tab) closes it, `*` after a name means the file is changed.
When the tabs do not fit, `<` and `>` at the ends show the others; the active tab is always
visible. View - Tabs hides / shows them (kept in `cedit.ini`).

## Files

- Text is edited in UTF-8. A file that is not valid UTF-8 is read and written in CP852 (DOS /
  Harbour DBF Latin), on a Chinese system (Windows ANSI codepage 950 / 936) in Big5 / GBK;
  Options: another one - Latin 1 / 2 / 9, Cyrillic (CP1251, CP866, KOI8-R/U, ...), Greek, Turkish,
  Baltic, Hebrew, Big5, GBK, YUSCII. Shift-F8 reads a file again in another codepage. The status
  line shows the codepage.
- Line ends (CRLF or LF), the final line end and a UTF-8 BOM are kept as they were.
- Tabs are kept and shown up to the next tab stop; wide (CJK) characters take two cells.
- A file given on the command line that does not exist is made by the first save.

## Large files

A 32-bit Harbour process cannot hold a multi-gigabyte string. Above about **32 MB**
(`CE_MEM_LIMIT`), CEdit keeps the text on disk instead of in `aLines`.

Architecture (ideas from [klogg](https://github.com/variar/klogg) and CudaText’s
open-then-index habit — not their code):

1. **Line index on disk** (`.idx` in the temp folder): every line start is an 8-byte
   offset, written in buffered chunks. Growing a Harbour string for millions of lines
   was the main reason the first paged attempt felt slow.
2. **LRU page cache** (~16 MB of `CE_PAGE_SIZE` pages) so scrolling does not `FSeek` for
   every single line from cold storage.
3. **Piece table** over the original file plus an append-only **`.add` swap**: inserts go
   to `.add`; deletes shrink pieces. The file is **editable** (undo/redo/save). Save
   streams the pieces in order to the destination.
4. Only a small first chunk (`CE_FIRST_STEP`, default 256 KB) is indexed before the editor
   appears; the rest and any reindex after an edit continue in the idle loop in small steps
   (`CE_IDLE_STEP`) so the UI stays responsive. Paint never runs a full `indexAll()`.
5. Status: `Idx n%` while indexing, then `Edit` / `Swap` when the buffer has unsaved
   piece-table changes. A `+` after the line count means the index is still growing.

| Limit / knob | Meaning |
|---|---|
| `CE_MEM_LIMIT` | above this size → disk-backed buffer (default 32 MB) |
| `CE_FIRST_STEP` | bytes indexed before the editor is shown |
| `CE_IDLE_STEP` | bytes indexed per idle tick |
| `CE_PAGE_SIZE` / `CE_CACHE_PAGES` | page cache size |
| `CE_IDX_BUF` | index write buffer |
| `CE_LINE_CAP` | max characters kept for one very long line |

Temp files (`cedit-*.idx`, `cedit-*.add`) are removed when the buffer is closed.

## Syntax highlighting

Harbour / Clipper / xBase; C, C++, C#, Java, JavaScript, PHP, Go; SQL; Python; shell scripts;
Windows batch; INI and hbmk2 files (.hbp, .hbc); HTML and XML — chosen by the file extension.
The colors are picked for the theme's palette (they stay readable in every theme), the
Turbo Pascal editor colors have their own.

More languages: `cedit.lang` next to `cedit.ini`, in the format of HbEdit's `hbedit.ini` -
its `[LANG_xxx]` sections (`ext=`, `keywords1=` .. , `scomm=`, `mcomm=`, `startline=`, `case=`,
`quotes=`) can be copied there, or `hbedit.ini` itself can be copied as `cedit.lang`.
View - Language definitions opens the file (a new one with an example: Lisp, Pascal); saving it
in CEdit reads the languages again. A language for the extensions of a built-in one (`.prg`,
`.c`, ...) adds its keywords and comments to it. All keyword groups get the keyword color
(HbEdit has a color for each). CEdit's own keys: `title=`, `first=` (comment lines, `*`),
`dotwords=` (`.AND.`), `escape=on|off`, `base=<built-in language>|none`.

## Settings

`cedit.ini`: next to the program when it is there (a portable copy), else in `%APPDATA%\CEdit`
(Windows) or `~/.config/cedit` (Linux). It keeps the options, the theme and the editor colors, and
the last 20 files with the line and column of the cursor — a recent file opens where it was left.
`cedit.lang` (languages, see above) is in the same folder.

## Building CEdit, step by step

CEdit is built from its source code with Harbour (a compiler for the Clipper language) and the
Harbour-CUI library. It takes three things, each done once: a C compiler, Harbour, and this
folder. If you already have Harbour (`hbmk2` works), go to step 3.

Everything below is typed in a command window: on Windows **cmd** (Start, type `cmd`, Enter),
on Linux a **terminal**.

### Windows

**Step 1 - the C compiler (MinGW, through MSYS2).** Harbour turns `.prg` into C, and a C
compiler makes the `.exe` from it.

1. Download the installer from https://www.msys2.org and install it into `C:\msys64` (the
   default folder - keep it).
2. At the end the installer opens an "MSYS2" window. Type there:

   ```
   pacman -S --needed mingw-w64-i686-gcc git
   ```

   and answer `Y`. This installs the 32-bit MinGW compiler into `C:\msys64\mingw32\bin`
   and git. Leave the window open for step 2.

**Step 2 - Harbour.** Harbour is built from its sources once (about 10 - 20 minutes).
First the sources, in the **MSYS2** window:

```
git clone https://github.com/harbour/core /d/tools/harbour-core
```

(or without git: on https://github.com/harbour/core the green "Code" button - "Download ZIP",
unpack it so that `D:\tools\harbour-core\win-make.exe` exists). Then the build, in **cmd**:

```
set PATH=C:\msys64\mingw32\bin;%PATH%
cd /d D:\tools\harbour-core
win-make
```

Only `mingw32\bin` goes into the PATH, not `C:\msys64\usr\bin`: the tools there disturb the
Harbour build.

At the end `D:\tools\harbour-core\bin\win\mingw\hbmk2.exe` must exist. Check:

```
D:\tools\harbour-core\bin\win\mingw\hbmk2.exe --version
```

Another folder than `D:\tools\harbour-core` works too: then set `HBMK2` before step 3, e.g.
`set HBMK2=C:\harbour\bin\win\mingw\hbmk2.exe`.

**Step 3 - CEdit.** In **cmd**, in the `samples\cedit` folder of Harbour-CUI:

```
cd /d D:\projekti\Harbour-CUI\samples\cedit
build
```

`build.bat` finds MinGW in `C:\msys64\mingw32\bin` by itself, builds the Harbour-CUI library
the first time (`..\..\lib\win\mingw\libhbtui.a`) and then CEdit. At the end it says
`Done: cedit.exe`. Run it:

```
cedit
cedit D:\work\program.prg
```

`build all` builds the library again too (after an update of Harbour-CUI or of Harbour).
`cedit.exe` needs no other files: it can be copied anywhere (e.g. into a folder in the PATH).

### Linux (Ubuntu, Debian, Mint ...)

CEdit runs in its own window (X11), not inside the terminal: it needs a desktop
(also WSLg on Windows 11, or `ssh -X`).

**Step 1 - the C compiler and the libraries:**

```
sudo apt update
sudo apt install build-essential git libx11-dev xfonts-base
```

`libx11-dev` must be there **before** Harbour is built: without it Harbour has no window
driver (gtxwc) and CEdit cannot start.

**Step 2 - Harbour** (about 10 minutes):

```
sudo git clone https://github.com/harbour/core /opt/harbour-core
cd /opt/harbour-core
sudo make
export PATH=/opt/harbour-core/bin/linux/gcc:$PATH
hbmk2 --version
```

Put the `export PATH=...` line at the end of `~/.bashrc`, so that `hbmk2` is found in every new
terminal.

**Step 3 - CEdit:**

```
cd ~/Harbour-CUI/samples/cedit
chmod +x build.sh
./build.sh
./cedit file.txt
```

`build.sh` builds the Harbour-CUI library the first time and then CEdit; `./build.sh all` builds
the library again too. Copy `cedit` to `~/.local/bin` (or `/usr/local/bin`) to run it from
anywhere.

### When something goes wrong

| Message | What to do |
|---|---|
| `Harbour was not found` | Harbour is not built, or is in another folder: step 2, or `set HBMK2=...` (Linux: `HBMK2=... ./build.sh`) |
| `The C compiler gcc.exe was not found` | MSYS2 / MinGW is missing: step 1 (Windows) |
| `git: command not found` | step 1 (`pacman -S git` in the MSYS2 window), or "Download ZIP" |
| `The X11 development files are missing` | `sudo apt install libx11-dev`, then build Harbour again (`sudo make` in `/opt/harbour-core`) |
| `cannot find -lhbtui` | the library is not built: `build all` |
| CEdit opens no window (Linux) | no desktop: CEdit needs X11 (a desktop session, WSLg, `ssh -X`); `echo $DISPLAY` must not be empty |
| CEdit starts, but the letters are boxes (Linux) | `sudo apt install xfonts-base` |
| `Error ... Message not found` when CEdit starts | the library is older than CEdit: `build all` |

Usage: `cedit [file ...]` - several files open in tabs.

## Notes

- Ctrl-Z and Ctrl-Left (also Ctrl-B and Ctrl-Right) give the same Inkey code; CEdit tells them apart
  by the state of the Z / B key (`ht_keyDown()`).
- Ctrl-F and Ctrl-H have the codes of End and Backspace: the editor widget gives them to CEdit
  (`bCtrlKey`) only while Ctrl is held, so they are not menu shortcuts.
- `htcodeedit.prg` (the editor widget `HTCodeEdit`), `cebuffer.prg` (`CEBuffer`: text, cursor,
  selection, undo / redo, search of one file) and `cesyntax.prg` (languages, colorizer) do not
  depend on the rest of CEdit; `cetabs.prg` is the row of tabs (`CETabBar`).
