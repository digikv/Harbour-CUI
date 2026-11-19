# CuiBuilder

Builds Harbour programs and libraries, in character mode, built on Harbour-CUI.
Copyright (c) 2026 Srđan Dragojlović. Dual licensed: GNU GPL version 3 or later (`COPYING`), or a
commercial license from the author — see `LICENSE`.

Made to work like **HwBuilder** by Alexander S. Kresin (HwGUI): CuiBuilder runs the Harbour
compiler, the C compiler, the resource compiler and the linker itself — no hbmk2 — from the
command templates of the compiler family in `cuibuild.ini`, and builds only the files that
changed. It reads and writes HwBuilder's `.hwprj` projects and reads hbmk2's `.hbp` projects.
Written from scratch on Harbour-CUI: no HwBuilder source code is used.

It has a **dialog designer** - dialogs of the Harbour-CUI widgets, from which it generates the
Harbour code (see [Dialogs](#dialogs-of-harbour-cui)) - and a **report designer**: reports for Word (`.docx`) and Excel (`.xlsx`), saved as
Clipper `.frm` files, from which it generates the Harbour code of the report for your program
(see [Reports](#reports-word-and-excel)).

And **Git**: status, stage, diff, commit, log, pull and push of the project's folder (see
[Git](#git)).

## Download

Ready-to-run programs: **[CuiBuilder 1.0.1 release](https://github.com/digikv/Harbour-CUI/releases/tag/cuibuilder-1.0.1)**

| File | Platform |
|---|---|
| `cuibuilder-1.0.1-win32.zip` | Windows 7 and newer (32-bit, runs on 64-bit Windows too) |
| `cuibuilder-1.0.1-linux-x86_64.tar.gz` | Linux x86_64 (X11); needs `libx11-6` and `libgpm2` |

New in 1.0.1: no error at the exit and in Options - Theme (1.0 stopped with "Message not
found: HTTHEME:CURRENTNAME"); filters at the end of a word and with a value in .hbp / .hbc
(`-lfoo{LETO='letodbf'}`, `libs=hbwin.hbc{allwin}`), as hbmk2 reads them; a project without a GT
keeps the GT the program chooses itself (`REQUEST HB_GT_xxx_DEFAULT`), as with hbmk2.

CuiBuilder builds with Harbour and a C compiler, so they must be on the computer (MinGW on
Windows, gcc on Linux). For Harbour-CUI programs it also needs the Harbour-CUI library: a clone of
this repository with the library built (`hbmk2 hbtui.hbp`); its folder goes into `Options -
Compilers and paths` (found by itself when CuiBuilder runs from `samples/cuibuilder`). The report
designer uses `samples/shared/cbreport.prg` and the DOCX / XLSX classes (github.com/digikv/docx,
/xlsx), the Git menu uses git.

Two programs:

| Program | What it is |
|---|---|
| `cuibuilder` | the screen: source files, options, messages of the build, the editor of CEdit for the sources and the errors |
| `cuibc` | the same build on the command line (like HwBuilder's `hwbc`), for scripts and other editors |

Tested with MinGW (Windows, 32-bit, MSYS2) and gcc (Linux). The templates of MSVC and Borland C
are there (`family=msvc` / `bcc`), not tried yet.

## The screen

```
 Project  Files  Build  Options
 D:\projekti\app\app.hwprj  *                                   Part: 1 (main)
┌ Source files (5) ──────────┐┌ Options ─────────────────────────────────────────┐
│app.prg                     ││ Output name    app                               │
│customers.prg               ││ Output path                                      │
│reports.prg                 ││ Objects path                                     │
│util.c                      ││ Source path                                      │
│app.rc                      ││ Target         Program                         ▼ │
│                            ││ C compiler     (default: mingw)                ▼ │
│                            ││ GT             (default: gtwvt)                ▼ │
│                            ││ Libraries      hbct hbwin                        │
│                            ││ ...                                              │
│                            ││ [√] Harbour-CUI library   [√] Only changed files │
└────────────────────────────┘└──────────────────────────────────────────────────┘
┌ Messages ───────────────────────────────────────────────────────────────────────┐
│Harbour: reports.prg                                                             │
│D:\projekti\app\reports.prg(112) Error E0030  Syntax error "syntax error at ')'" │
│Failed: 1 errors, 0 warnings, 0.4 s                                              │
└─────────────────────────────────────────────────────────────────────────────────┘
 F2 Save  F3 Open  F4 Edit  Ins Add  F9 Build  Ctrl-F9 Run  Shift-F9 Rebuild  ...
```

| Key | Command |
|---|---|
| F9 | build (only what changed) |
| Ctrl-F9 | build, then run the program |
| Shift-F9 | rebuild all |
| Alt-F9 | clean: delete the objects and the output |
| Esc while building | stop after the current step |
| F8 / Shift-F8 | next / previous error: the file in the editor, the cursor on the line |
| Enter on a message | the file of an error at its line; a long message whole |
| F2 / F3 | save / open a project (`Project - New`, `Recent`, `Save as` in the menu) |
| Ins / Del | add files (Space or Ins marks several) / remove a file from the project |
| Enter, F4 | the file in the editor (F2 saves, F7 finds, Esc closes) |
| Ctrl-Up / Ctrl-Down | move a file up / down |
| Tab | next field |
| F10 | the menu (also Alt + the underlined letter, the mouse) |
| Alt-X | exit |

A changed project is saved before it is built. An `.hbp` is shown read only: `Project - Edit
project file` opens it in the editor, `Save as` makes a `.hwprj` of it.

`Options - Compilers and paths` sets where Harbour, the C compiler and Harbour-CUI are;
`Default compiler` chooses between several `[C_COMPILER]` sections; `Edit cuibuild.ini` changes
the rest (command templates, environment). `Build - Show command lines` puts the full commands
into the messages.

## Projects: `.hwprj`

The format of HwBuilder: options `key=value`, then the files, one per line. `#` starts a comment.

```
# app - a CuiBuilder / HwBuilder project
outname=app
prgflags=-w3 -es2
libs=hbct
{win}libs=hbwin
app.prg
customers.prg
util.c
{win}app.rc
```

| Key | Meaning |
|---|---|
| `outname` | the name of the program / library (without the extension), may have a folder |
| `outpath` | the folder of the output |
| `objpath` | the folder of the objects (default: `obj/<compiler id>[/<part>]`) |
| `srcpath` | the folder of the source files listed below |
| `target=lib` | a library instead of a program |
| `makemode=off` | always build everything (default: only what changed) |
| `c_compiler` | the id of a `[C_COMPILER]` of `cuibuild.ini` |
| `guilib=` | empty: without the Harbour-CUI library (default: with it) |
| `gtlib` | the GT: `gtwvt`, `gtwin`, `gtcgi`, `gtstd`, `gttrm`, `gtxwc` ... (default: the one of `[GUILIB]`, gtwvt / gtxwc) |
| `gui=yes/no` | a window program (`-mwindows`) or a console one (default: by the GT) |
| `libs`, `libspath` | libraries (`hbct`, `mylib`, or a file `lib/x.a`) and their folders |
| `incpath` | folders of the `#include` files |
| `prgflags`, `cflags`, `lflags` | Harbour, C compiler and linker flags |
| `def_cflags`, `def_lflags`, `def_libflags` | instead of the defaults of the compiler |
| `sub.hwprj`, `sub.hbp` | another project, built first |
| `name.hbc` | the libraries, paths and flags of an `.hbc` (`hbziparc.hbc`, `hbct.hbc` ...; searched next to the project, then in Harbour's `contrib` / `addons`) |
| `report.frm` | a report: Enter opens it in the report designer, the build skips it |
| `dialog.cuiform` | a dialog: Enter opens it in the dialog designer, the build skips it |
| `@files.txt` | the files listed in a text file |
| `:project name` | a new part: several programs / libraries in one file, built in order |

A line may start with a filter: `{win}`, `{unix}`, `{linux}`, `{mingw}`, `{msvc}`, `{!win}`,
`{win&mingw}`, `{win|linux}`, `{hb_ispath='../x/y.prg'}` (the file or folder exists), or the
name of an environment variable that is set. `${NAME}` /
`$(NAME)` is an environment variable; `${hb_plat}`, `${hb_comp}`, `${hb_root}` are known too.
Filters, `@lists` and macros stay as they are when the screen saves the project.

A library and the program that uses it, in one project:

```
:project mylib
target=lib
outname=lib/mylib
srcpath=src
libf.prg

:project app
outname=app
libs=mylib
libspath=lib
app.prg
```

The program is linked again when the library changed.

## Projects: `.hbp`

hbmk2 projects are read as they are: the sources (wildcards too), `-o`, `-hblib`, `-l`, `-L`,
`-i`, `-gt...`, `-gui` / `-std`, `-inc`, `-workdir=`, `-main=`, `-request=`, `-prgflag=`,
`-cflag=`, `-ldflag=`, the Harbour flags (`-w3 -es2 -b -d...`), `.hbm` and `@` files, `.hbc`
files (`libs= libpaths= incpaths= hbcs= gt= prgflags= cflags= ldflags= sources=`; searched next
to the project, then in Harbour's `contrib` / `addons`), `{filters}` and `${macros}`. Other
options are reported and skipped. So `cuibc cedit.hbp` builds CEdit, `cuibc cnc.hbp` builds CNC.

## Settings: `cuibuild.ini`

Next to the program when it is there, else `%APPDATA%\CuiBuilder\cuibuild.ini` (Windows) or
`~/.config/cuibuilder/cuibuild.ini` (Linux). Made at the first start: Harbour is looked for in
`HB_ROOT`, `HB_PATH`, the `PATH`, `D:\tools\harbour-core`, `C:\harbour`, `/opt/harbour-core`,
`~/git/core`; MinGW in the `PATH` and `C:\msys64\mingw32\bin`; Harbour-CUI two folders above
the program. The sections are those of HwBuilder's `hwbuild.ini`:

```
[HARBOUR]
harbour_path=D:\tools\harbour-core
harbour_bin_path=D:\tools\harbour-core\bin\win\mingw
harbour_include_path=D:\tools\harbour-core\include
def_flags=-n -q0 -w
libs=hbvm hbrtl hblang hbcpage gtcgi gtpca gtstd gtwin gtwvt gtgui hbrdd ...

[GUILIB]
id=hbtui
path=D:\projekti\Harbour-CUI
inc_path=D:\projekti\Harbour-CUI\include
lib_path=D:\projekti\Harbour-CUI\lib\win\mingw
libs=hbtui
gt=gtwvt

[C_COMPILER]
id=mingw
family=mingw
bin_path=
harbour_lib_path=D:\tools\harbour-core\lib\win\mingw
def_cflags=-c -O2 -W -Wall -pipe
def_linkflags=-mwindows
def_linkflagscons=-mconsole
def_libflags=
def_syslibs=-lwinmm -lkernel32 -luser32 -lgdi32 ...
cmd_comp=
cmd_res=
cmd_lib=
cmd_exe=
env_INCLUDE=...

[C_COMPILER_2]
id=msvc
family=msvc
...

[VIEW]
theme=
compiler=
```

`family` is `mingw`, `gcc`, `clang`, `msvc` or `bcc`; it gives the command templates. `cmd_comp`,
`cmd_res`, `cmd_lib`, `cmd_exe` replace them, with these names:

| Name | Is replaced by |
|---|---|
| `{path}` | `bin_path` with a separator at the end ("" = from the `PATH`) |
| `{f}` | the flags |
| `{src}`, `{obj}`, `{out}` | the source, the object, the output |
| `{objs}`, `{res}` | the objects, the compiled resources |
| `{incs}`, `{libpaths}`, `{libs}` | `-I...`, `-L...`, `-l...` (or `/I`, `/LIBPATH:`, `x.lib`) |
| `{hi}`, `{gi}`, `{hL}`, `{gL}`, `{dL}` | as in HwBuilder: Harbour / GUI library include and library folders |

The MinGW templates:

```
cmd_comp={path}gcc.exe {f} {incs} -o{obj} {src}
cmd_res={path}windres.exe {incs} {src} -O coff -o {out}
cmd_lib={path}ar.exe rcs {f} {out} {objs}
cmd_exe={path}gcc.exe {f} -o{out} {objs} {res} {libpaths} -Wl,--allow-multiple-definition -Wl,--start-group {libs} -Wl,--end-group
```

`env_NAME=value` sets an environment variable before the build (MSVC: `INCLUDE`, `LIB`); the
`bin_path` is put in front of the `PATH`.

What a program gets: its objects, a small C file (`_cbstub.c`, as hbmk2 makes) that chooses the
default GT, then the project's libraries, the Harbour-CUI library, the Harbour libraries of
`[HARBOUR] libs=` (`hbmainwin` / `hbmainstd` with MinGW and MSVC) and the system libraries.

What is "changed": an object is older than its source or than a local header that the source
includes (`#include "x.ch"`, in its folder or `incpath`); a program is linked again when an
object or a library is newer than it. When the flags of the compilers change (`-d...`, `-gc3`,
include folders, C flags - kept in `_cbflags.txt` of the objects folder), everything is compiled
again, as the objects were made with the old ones.

## Dialogs of Harbour-CUI

`Design - New dialog` (or Enter on a `.cuiform` in the list of files) opens the dialog designer:
the dialog is drawn as it will look, the selected control in the color of a selection. On the
right: the dialog or the selected control, and the keys.

| Key | Command |
|---|---|
| Ins | add a control: label, text field (HTLineEdit), GET with PICTURE (HTGet), button, check box, radio button, combo box, list, text of several lines (HTTextEdit), number with arrows (HTSpinner), progress bar, frame |
| arrows / Ctrl + arrows | move / size the selected control (no selection: Ctrl + arrows size the dialog) |
| Tab / Shift-Tab | the next / previous control |
| Enter, double click | the properties: position, size, name (the variable), text, PICTURE, type and length of a GET, VALID / WHEN, items, start value, the action of a button (`ok`, `cancel` or a function) |
| Del | delete the control |
| Ctrl-PgUp / Ctrl-PgDn | earlier / later in the Tab order |
| mouse | click selects, drag moves |
| Esc | no selection (the dialog); again: close |
| F3 | the dialog: name, title, size, the buttons at the bottom (`OK,Cancel`) |
| F5 | the dialog itself, made of the widgets, and the values it returns |
| F2 / F9 | save the `.cuiform` (JSON) / generate the code |

F9 writes `dlg_<name>.prg` and adds it to the project. It has the function
`Dlg_<Name>( [hValues] ) -> hValues / NIL`: it builds the dialog with HTDialog, HTLabel, HTGet,
HTLineEdit, HTCheckBox ... (the code you would write by hand), takes the start values of the
fields from `hValues` by their names, shows the dialog and returns the values of the fields
(`NIL` = cancelled). A button with a function as its action calls `Func( dlg, hCtl )`
(`hCtl`: the widgets by their names).

```
hVal := Dlg_Kupac( { "cNaziv" => KUPCI->NAZIV, "nRok" => KUPCI->ROK } )
IF hVal != NIL
   KUPCI->NAZIV := hVal[ "cNaziv" ]
   KUPCI->ROK   := hVal[ "nRok" ]
ENDIF
```

The program creates `HTApplication():new()` first, as every Harbour-CUI program does.

## Git

The `Git` menu works in the folder of the project (git from the `PATH`, or `cuibuild.ini [VIEW]
git=`; no console window, and git does not wait for a password in a terminal).

| Command | What it does |
|---|---|
| Status, commit (F11) | the changed files: `√` = staged. Space / Ins stage or unstage one, F3 all, Shift-F3 none, Enter its diff, F2 commit (a summary line, more lines, "Push after the commit"; nothing staged: all changes, when you say so), F6 push, F7 pull, F5 refresh |
| Diff | all the changes, staged and not |
| Log | the last 200 commits; Enter shows one (`git show --stat -p`) |
| Pull / Push | `git pull --ff-only` / `git push`; the output goes into the messages |
| Init repository | `git init -b main` and a `.gitignore` (obj/, the programs the project makes, the tries of reports) |

Diffs are shown in color (added lines green, removed red); Left / Right scroll long lines.

## Screen size

40 rows x 132 columns by default; `Options - Screen size` chooses 25x80 up to 75x240 or any other
(kept in `cuibuild.ini [VIEW] rows= cols=`), `cuibuilder -size=60x200` for one start. On Windows
the font is made to fit the whole screen (on 1920x1080: 9.6 x 18 pixels at 60x200, 7.7 x 13.5 at
80x250); gtxwc (Linux) has at most 256 x 256 cells, its font keeps its size.

## Reports: Word and Excel

`Reports - New report` (or Enter on a `.frm` in the list of files) opens the designer. A report
is what Clipper's REPORT FORM knows: up to 4 title lines, a group and a subgroup (an expression
and a heading; subtotals), up to 24 columns (an expression, a heading - `;` starts a new line -,
the width, the decimals, a total), "Summary" (only the totals). It is saved as a Clipper `.frm`,
so REPORT FORM, RL and CBro (Report Form to Word / Excel) read it too.

| Key | Command |
|---|---|
| Ins / Enter / Del | add / change / delete a column |
| Ctrl-Up / Ctrl-Down | move a column |
| F5 | columns from the fields of the table (Space marks several) |
| F6 / F7 | try the report on the table: a `.docx` / `.xlsx` made and opened at once |
| F2 | save the `.frm` |
| F9 | generate the code |
| Esc | close |

`Table` is a `.dbf` to try the report on (and to check the expressions); `Codepage` the one it is
written in (`SL852`, `UTF8` ...).

F9 writes `rpt_<name>.prg` next to the `.frm` and adds to the project what it needs: the generated
file, the shared report module `samples/shared/cbreport.prg`, the DOCX and XLSX classes of
github.com/digikv/docx and /xlsx (`DOCXClass.prg`, `XLSXClass.prg`, next to the Harbour-CUI
folder; `cuibuild.ini [REPORTS]` says where they are) and `hbziparc.hbc`. In the program:

```
USE KUPCI
IF ! Rpt_Kupci( "kupci.docx",,, @cErr )          // or "kupci.xlsx"
   Alert( cErr )
ENDIF
```

`Rpt_<Name>( cFile, [bFor], [cTitle], [@cErr], [cCdp] )` writes the report of the current work
area: `bFor` the records to take, `cTitle` instead of the first title line, `cCdp` the codepage of
text that is not UTF-8. The generated code builds the report with `CBR_New()`, `CBR_Title()`,
`CBR_Group()`, `CBR_Column()` and `CBR_Make()` of `cbreport.prg` - no `.frm` is needed at run
time. Word gets a table with the heading row repeated on every page (landscape for wide reports),
Excel a sheet with numbers as numbers.

## `cuibc`

```
cuibc [project.hwprj | project.hbp] [-clean] [-rebuild] [-cleanonly] [-comp=<id>] [-ini=<file>] [-v] [-q] [-run]
```

Without a project: the only `.hwprj` (else `.hbp`) of the current folder. `-v` shows the command
lines, `-q` only errors, warnings and the result. Exit code 0 = built, 1 = errors, 2 = wrong
parameters or settings.

## Building CuiBuilder

With hbmk2 (the Harbour-CUI library is built first when it is missing):

```
build.bat            (Windows)
./build.sh           (Linux; needs libx11-dev)
```

`build all` builds the library again. Or by hand: `hbmk2 cuibuilder.hbp`, `hbmk2 cuibc.hbp`.
Then CuiBuilder builds itself: `cuibuilder cuibuilder.hwprj` (F9), or `cuibc cuibuilder.hwprj`.

The editor is the one of CEdit (`../cedit/htcodeedit.prg`, `cebuffer.prg`, `cesyntax.prg`).
