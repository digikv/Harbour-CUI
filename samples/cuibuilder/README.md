# CuiBuilder

Builds Harbour programs and libraries, in character mode, built on Harbour-CUI.
Copyright (c) 2026 Srđan Dragojlović. Dual licensed: GNU GPL version 3 or later (`COPYING`), or a
commercial license from the author — see `LICENSE`.

Made to work like **HwBuilder** by Alexander S. Kresin (HwGUI): CuiBuilder runs the Harbour
compiler, the C compiler, the resource compiler and the linker itself — no hbmk2 — from the
command templates of the compiler family in `cuibuild.ini`, and builds only the files that
changed. It reads and writes HwBuilder's `.hwprj` projects and reads hbmk2's `.hbp` projects.
Written from scratch on Harbour-CUI: no HwBuilder source code is used.

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
| `@files.txt` | the files listed in a text file |
| `:project name` | a new part: several programs / libraries in one file, built in order |

A line may start with a filter: `{win}`, `{unix}`, `{linux}`, `{mingw}`, `{msvc}`, `{!win}`,
`{win&mingw}`, `{win|linux}`, or the name of an environment variable that is set. `${NAME}` /
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
object or a library is newer than it.

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
