# CEdit

Text editor in character mode, built on Harbour-CUI.
Copyright (c) 2026 Srđan Dragojlović. Dual licensed: GNU GPL version 3 or later (`COPYING`), or a
commercial license from the author — see `LICENSE`.

Made to work like HbEdit by Alexander S. Kresin — the same F-keys, so those who are used to it
feel at home — but written from scratch on Harbour-CUI: no HbEdit source code is used.

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
| Tab | spaces to the next tab stop; with several lines selected: indent |
| F7 / Ctrl-F7 | find / replace (case, whole words; replace asks: Yes / No / All / Cancel) |
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

## Files

- Text is edited in UTF-8. A file that is not valid UTF-8 is read and written in CP852 (DOS /
  Harbour DBF Latin; Options: another codepage); the status line shows the codepage.
- Line ends (CRLF or LF), the final line end and a UTF-8 BOM are kept as they were.
- Tabs are kept and shown up to the next tab stop; wide (CJK) characters take two cells.
- A file given on the command line that does not exist is made by the first save.

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

## Build

```
build.bat                 Windows, with the newest Harbour in D:\tools\harbour-core
hbmk2 cedit.hbp           any platform (Linux: X11, -lX11, for the Ctrl-Z / Ctrl-Left test)
```

Usage: `cedit [file ...]`

## Notes

- Ctrl-Z and Ctrl-Left (also Ctrl-B and Ctrl-Right) give the same Inkey code; CEdit tells them apart
  by the state of the Z / B key (`ht_keyDown()`).
- `htcodeedit.prg` (the editor widget `HTCodeEdit`), `cebuffer.prg` (`CEBuffer`: text, cursor,
  selection, undo / redo, search of one file) and `cesyntax.prg` (languages, colorizer) do not
  depend on the rest of CEdit.
