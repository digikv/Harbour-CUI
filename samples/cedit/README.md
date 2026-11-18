# CEdit

Text editor in character mode, built on Harbour-CUI.
Copyright (c) 2026 Srđan Dragojlović. Dual licensed: GNU GPL version 3 or later (`COPYING`), or a
commercial license from the author — see `LICENSE`.

Made to work like HbEdit by Alexander S. Kresin — the same F-keys, so those who are used to it
feel at home — but written from scratch on Harbour-CUI: no HbEdit source code is used.

Status: **version 0.1, part 1 of version 1** — files, editing, selection, clipboard, undo, menu.
Coming next: search / replace / go to line, syntax highlighting and codepage switching, settings
and recent files kept between sessions.

## Keys

F9 opens the menu (also Alt + the underlined letter, or the mouse); the menu shows the key of every
command. F1 shows the keys in the editor.

| Key | Command |
|---|---|
| F2 / Shift-F2 | save / save as |
| F4 / Ctrl-F4 | open (recent files) / open file |
| Shift-F4 | new file |
| F12, Ctrl-Tab, Shift-Tab | list of open files, next, previous |
| F10, Esc | close the file (the last one ends CEdit); Esc first cancels a selection |
| Alt-X | exit |
| F3 / Ctrl-F3 | block / vertical block: the cursor keys select, F3 again ends it |
| Shift + cursor keys, mouse | select (drag; double click: a word; Shift + click extends) |
| Ctrl-A | select all |
| Ctrl-C, Ctrl-Ins / Ctrl-X, Shift-Del / Ctrl-V, Shift-Ins | copy / cut / paste (a vertical block is pasted as a block) |
| Ctrl-Y | delete line |
| Ctrl-Z, Alt-Backspace / Ctrl-R | undo / redo |
| Ins | insert / overwrite |
| Home | first text of the line, again: line start |
| Ctrl-Left / Ctrl-Right | word left / right |
| Ctrl-Home / Ctrl-End, Ctrl-PgUp / Ctrl-PgDn | start / end of the text, top / bottom of the screen |
| Ctrl-Up / Ctrl-Down | scroll a line |
| Tab | spaces to the next tab stop; with several lines selected: indent |

## Files

- Text is edited in UTF-8. A file that is not valid UTF-8 is read and written in CP852 (DOS /
  Harbour DBF Latin); the status line shows the codepage.
- Line ends (CRLF or LF), the final line end and a UTF-8 BOM are kept as they were.
- Tabs are kept and shown up to the next tab stop (4); wide (CJK) characters take two cells.
- A file given on the command line that does not exist is made by the first save.

## Build

```
build.bat                 Windows, with the newest Harbour in D:\tools\harbour-core
hbmk2 cedit.hbp           any platform (Linux: X11, -lX11, for the Ctrl-Z / Ctrl-Left test)
```

Usage: `cedit [file ...]`

## Notes

- Ctrl-Z and Ctrl-Left (also Ctrl-B and Ctrl-Right) give the same Inkey code; CEdit tells them apart
  by the state of the Z / B key (`ht_keyDown()`).
- `htcodeedit.prg` (the editor widget `HTCodeEdit`) and `cebuffer.prg` (`CEBuffer`: text, cursor,
  selection, undo / redo of one file) do not depend on the rest of CEdit.
