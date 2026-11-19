# CNC

A two-panel file manager in character mode, in the style of Norton Commander with options of
Total Commander, built on Harbour-CUI. Copyright (c) 2026 Srđan Dragojlović. Dual licensed: GNU GPL version 3 or later
(`COPYING`), or a commercial license from the author — see `LICENSE`.

The keys follow HbCommander, the file manager of HbEdit by Alexander S. Kresin, and the classic
Norton Commander (F3 view, F4 edit, F5 copy, F6 move, F7 folder, F8 delete, F9 menu, F10 quit).
It is written from scratch on Harbour-CUI: no HbEdit code is used. F3 and F4 open the editor of
[CEdit](../cedit/README.md) (`htcodeedit.prg`, `cebuffer.prg`, `cesyntax.prg`), with syntax
highlighting.

From Total Commander:

- the columns **Name │ Ext │ Size │ Date** (date and time together); a click on a column title
  sorts by it, a second click turns the order round, an arrow in the title shows it
- the **Net** menu: an **FTP server in a panel** (Ctrl-F connect, Ctrl-N new connection,
  Ctrl-Shift-F disconnect; view, edit, copy and move both ways, make folders, delete, rename on
  the server; saved connections), and on Windows network drives (map / disconnect) and the
  computers of the network with their shares

```
╔═════════════════ C:\work\ ═════════════════╗
║    Name↑    │ Ext │   Size   │     Date     ║
║..           │     │►UP--DIR◄│              ║
║src          │     │►SUB-DIR◄│19.11.26 05:24║
║main         │prg  │      1234│19.11.26 05:24║
║readme       │md   │       512│18.11.26 22:10║
╟─────────────┴─────┴──────────┴──────────────╢
║main.prg            1234 19.11.26 05:24      ║
╚═════════ 1,234 bytes in 1 selected file ════╝
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
| Ctrl-P | brief (two columns of names) / full (Name, Ext, Size, Date - as Total Commander) |
| Ctrl-F3 .. Ctrl-F6, a click on Name / Ext / Size / Date | sort by name, extension, time, size; the same again turns the order round (the arrow in the title shows it) |
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

## Net (as in Total Commander)

| Key | Command |
|---|---|
| Ctrl-F | FTP connect: the saved connections, or a new one |
| Ctrl-N | FTP new connection: host (or `ftp://user@host:port/folder`), port, user, password, folder; "Save as" keeps it in `cnc.ini` (without the password) |
| Ctrl-Shift-F | FTP disconnect: the panel goes back to its local folder |
| Net - Map network drive | `net use X: \\server\share` (Windows) |
| Net - Disconnect network drive | `net use X: /delete` (Windows) |
| Net - Network computers | `net view`: the computers, then their shares; a share opens in the panel as `\\pc\share\` (Windows) |

On an FTP panel Enter goes into folders, Enter / F3 view a file, F4 edits it (saved back to the
server when changed), F5 / F6 copy / move to the local panel and from it (folders too), F7 makes
a folder, F8 deletes (folders with everything in them), Shift-F6 renames. FTP is the client of
Harbour's hbtip (passive mode).

Commands typed in the command line run without a console window (Windows); their output is shown
when they end (Ctrl-W shows it again; Options - Show command output off: only the exit code). A program with its own window started there keeps CNC
waiting until it is closed: `start notepad` does not.

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
`cedit` folder next to it (`../cedit`) and the hbtip contrib of Harbour (FTP; on Linux also hbunix:
`HB_BUILD_CONTRIBS="hbtip hbunix" make` when Harbour is built, or `hbmk2 hbtip.hbp` and
`hbmk2 hbunix.hbp` in `contrib/hbtip` and `contrib/hbunix`). Usage: `cnc [left folder [right folder]]`.
