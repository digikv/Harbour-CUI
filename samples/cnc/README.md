# CNC

A two-panel file manager in character mode, in the style of Norton Commander, built on
Harbour-CUI. Copyright (c) 2026 Srđan Dragojlović. Dual licensed: GNU GPL version 3 or later
(`COPYING`), or a commercial license from the author — see `LICENSE`.

The keys follow HbCommander, the file manager of HbEdit by Alexander S. Kresin, and the classic
Norton Commander (F3 view, F4 edit, F5 copy, F6 move, F7 folder, F8 delete, F9 menu, F10 quit).
It is written from scratch on Harbour-CUI: no HbEdit code is used. F3 and F4 open the editor of
[CEdit](../cedit/README.md) (`htcodeedit.prg`, `cebuffer.prg`, `cesyntax.prg`), with syntax
highlighting.

```
╔══════ C:\work\ ═══════╗╔══════ D:\backup\ ══════╗
║   Name   │  Size  │Date║║   Name   │  Size  │Date║
║ ..       │►UP--DIR◄│   ║║ ..       │►UP--DIR◄│   ║
║ src      │►SUB-DIR◄│   ║║ old      │►SUB-DIR◄│   ║
║ main.prg │    1234│...║║ main.prg │    1100│...║
╟──────────┴────────┴───╢╟──────────┴────────┴───╢
║ main.prg   1234 19.11.26║║ ..        ►UP--DIR◄   ║
╚═ 1,234 bytes in 1 file ═╝╚═══════════════════════╝
C:\work\>
1Help 2Menu 3View 4Edit 5Copy 6RenMov 7Mkdir 8Delete 9PullDn 10Quit
```

## Keys

F9 opens the menu (Left, Files, Commands, Options, Right; also the mouse); the menu shows the key
of every command. F1 shows all the keys.

| Key | Command |
|---|---|
| Tab | the other panel |
| Enter, double click | into a folder; a program runs, a document opens in its program |
| Backspace, Ctrl-PgUp | the parent folder |
| Ins, right click | mark a file; F5 / F6 / F8 work on the marked files, else on the current one |
| + / - / * | mark / unmark by a mask (`*.prg`), invert the marks |
| Ctrl-S, Alt + letter | quick search (Alt-L, F, C, O, R open the menus) |
| F1 / F2 | help / the commands for the current file |
| F3 | view (on a folder: its size, files and folders) |
| F4 / Shift-F4 | edit / edit a new file (F2 saves, F7 finds, Esc closes) |
| F5 / F6 | copy / move to the other panel, or to the path typed (folders with everything in them) |
| Shift-F6 | rename |
| F7 / F8 | make a folder / delete |
| F10 | quit |
| Ctrl-U | swap the panels |
| Ctrl-P | brief (two columns of names) / full (name, size, date, time) |
| Ctrl-F3 .. Ctrl-F6, a click on Name / Size / Date | sort by name, extension, time, size; the same again turns the order round (the arrow in the title shows it) |
| Ctrl-R | read the folders again |
| Ctrl-F7 | find files by a mask in the folder and below |
| Ctrl-Ins | the full name of the current file to the clipboard |
| D, Ctrl-F1 / Ctrl-F2 | type a folder for the active / left / right panel |
| Alt-F1 / Alt-F2 | drive of the left / right panel (Linux: /, home, /mnt, /media) |
| H | the folders visited |
| M / ' | set a bookmark (a, s, d) on the folder / go to a bookmark |
| Ctrl-O | the command line; Enter runs the command in the folder of the active panel, Esc back |
| Ctrl-Enter | the current name to the command line |
| Ctrl-W | the output of the last command |
| = | a Harbour expression: `=1024*16`, `=Date()` (also `=...` in the command line) |

Commands - Compare directories marks the files that are only in one panel or newer there.

## Settings

`cnc.ini`: next to the program when it is there, else in `%APPDATA%\CNC` (Windows) or
`~/.config/cnc` (Linux). It keeps the folders of the panels, the sort order and view, the theme,
the bookmarks and the folders visited.

## Building

As CEdit - see [Building CEdit, step by step](../cedit/README.md#building-cedit-step-by-step) for
Harbour and the C compiler. Then:

```
build.bat          Windows
./build.sh         Linux
```

Both build the Harbour-CUI library first when it is not built yet (`build all`: again). CNC needs the
`cedit` folder next to it (`../cedit`). Usage: `cnc [left folder [right folder]]`.
