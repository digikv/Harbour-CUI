# CBro

xBase database browser and file manager in character mode, built on Harbour-CUI.
Copyright (c) 2026 Srđan Dragojlović. Dual licensed: GNU GPL version 3 or later (`COPYING`), or a
commercial license from the author — see `LICENSE`; components: `THIRD-PARTY.md`.

Made as a successor to BroPlus by Peter Volz (SmartCode Programming): the same F1..F9 menus,
hot keys and status lines, written from scratch — no BroPlus source code is used.
Its GUI sibling is GBro (HWGUI).

## Download

Ready-to-run programs, no build needed: **[CBro 1.0 release](https://github.com/digikv/Harbour-CUI/releases/tag/cbro-1.0)**

| File | Platform |
|---|---|
| `cbro-1.0-win32.zip` | Windows (32-bit, runs on 64-bit Windows too) |
| `cbro-1.0-linux-x86_64.tar.gz` | Linux x86_64 (X11); needs `libx11-6`, `libodbc2` (unixODBC), `libgpm2` |

Unpack and run `cbro` (`cbro /DEMO` opens demo tables, `cbro /?` lists the parameters). Both are
the GPL build with the LetoDBf client. To build CBro yourself, see [Build](#build).

## What it does

- DBF tables (DBFCDX / DBFNTX) in floating windows, one per table; any data codepage
  (GlobalSet > Data Codepage, `/C:CP852`)
- LetoDB / LetoDBf servers (`leto://host:port/path/file.dbf`, Ctrl-L lists the server's files)
- SQL: SQLite files and ODBC data sources (Ctrl-Q); SELECT opens a window, other statements run
- Excel: open a sheet as a table, Copy To / Export writes `.xlsx`, Append From reads a sheet
  (through the XLSX class, https://github.com/digikv/xlsx)
- Scripts: `.prg` compiled in memory or `.hrb` (Alt-K, Script Editor, `/P:script.prg`)
- Dynamic indexes: unique keys (Shift-F6), descending order (Alt-F6), custom indexes with keys
  added / removed by hand (Ctrl-F8 / Ctrl-F9)
- Memos holding JSON, arrays (nested, edited as Harbour literals), dates, numbers, logicals
- BroPlus colors: the original BroPlus color set and DOS palette is the default theme
- Indexes, scopes, filters, relations (master-detail), color conditions, views (`.cbv`),
  structure editor, Copy To / Append From / Replace / Count / Sum / Pack / Zap / Reindex,
  Label and Report Form, List to file / printer, calculator, expression evaluator, help

## Build

### Tools used

CBro 1.0 was built and tested with:

| | Windows | Linux |
|---|---|---|
| OS | Windows 11 Pro (10.0.26300) | Ubuntu 24.04.5 LTS (WSL 2), X11 / Xvfb |
| Harbour | 3.2.1dev r2610242111 — github.com/harbour/core, commit `694428e` (2026-10-24), built 2026-11-17 in `D:\tools\harbour-core` | the same commit, built in `/opt/harbour-core` |
| C compiler | MinGW-w64 GCC 16.2.0, 32-bit (`i686-w64-mingw32`, MSYS2 `mingw32`, "Rev6"), GNU windres 2.47 | GCC 13.3.0 (Ubuntu 13.3.0-6ubuntu2~24.04.1), 64-bit |
| GT driver | gtwvt (window, Consolas) | gtxwc (X11 window) |
| Packages | — | `libx11-dev`, `unixodbc-dev` 2.3.12 (sddodbc), `xfonts-base`; `xfonts-unifont` for CJK |

Harbour comes from https://github.com/harbour/core; build it as its README describes
(`win-make.exe` with MinGW on Windows, `make` on Linux).

Every part is compiled with that same Harbour: the Harbour-CUI library, the LETO client and CBro.
Libraries built with another Harbour version must be built again.

### What CBro links

- Harbour-CUI: `hbtui.hbp` in the repository root (`../../hbtui.hbc`)
- Harbour contribs: hbmzip, hbziparc, hbmemio, rddsql, sddodbc, sddsqlt3 (with SQLite), on Windows also hbwin
- the XLSX class: `../../../xlsx/XLSXClass.prg` (clone https://github.com/digikv/xlsx next to the
  repository); in another folder: `set CBRO_XLSXDIR=<folder>` before `build.bat`, or
  `hbmk2 cbro.hbp -env:CBRO_XLSXOWN=yes <folder>/XLSXClass.prg`
- the LETO client library `librddleto.a`: LetoDBf (default) from `lib/<platform>/letodbf`, or
  LetoDB from `lib/<platform>/letodb` (`<platform>` = `win` | `linux`)

Build switches (hbmk2 `-env:`):

| Switch | Effect |
|---|---|
| `-env:CBRO_LETODB=yes` | the LetoDB client instead of LetoDBf (both are the LETO driver: only one per program) |
| `-env:CBRO_NOCOMPILER=yes` | without the Harbour compiler (GPL, no linking exception) — required for the commercial license; `.prg` scripts are then compiled by `harbour` next to CBro or in the PATH, `.hrb` scripts run as before |

### Windows

1. Harbour in `D:\tools\harbour-core` (32-bit MinGW, `bin\win\mingw\hbmk2.exe`); the MinGW
   compiler (`C:\msys64\mingw32\bin`) in the PATH. The contribs CBro needs are part of the Harbour
   build.
2. Harbour-CUI library, in the repository root:

   ```
   D:\tools\harbour-core\bin\win\mingw\hbmk2.exe hbtui.hbp
   ```

3. LETO client libraries (once; from the LetoDBf sources in `D:\letodbf`, LetoDB in `C:\letodb`),
   written into CBro's `lib` folder - the source folders are not changed:

   ```
   cd /d D:\letodbf
   mkdir D:\Harbour-CUI\samples\cbro\lib\win\letodbf\.hbmk
   D:\tools\harbour-core\bin\win\mingw\hbmk2.exe rddleto.hbp -oD:/Harbour-CUI/samples/cbro/lib/win/letodbf/rddleto -workdir=D:/Harbour-CUI/samples/cbro/lib/win/letodbf/.hbmk

   cd /d C:\letodb
   mkdir D:\Harbour-CUI\samples\cbro\lib\win\letodb\.hbmk
   D:\tools\harbour-core\bin\win\mingw\hbmk2.exe rddleto.hbp -oD:/Harbour-CUI/samples/cbro/lib/win/letodb/rddleto -workdir=D:/Harbour-CUI/samples/cbro/lib/win/letodb/.hbmk
   ```

4. CBro, in `samples\cbro`:

   ```
   build.bat                    LetoDBf client (GPL build, scripts compiled in memory)
   build.bat letodb             LetoDB client
   build.bat commercial         without the Harbour compiler (commercial license)
   build.bat commercial letodb
   ```

   `build.bat` calls `D:\tools\harbour-core\bin\win\mingw\hbmk2.exe cbro.hbp` with the switches;
   the result is `cbro.exe` with the icon (`cbro.rc` / `cbro.ico`).

### Linux

Tested in WSL (Ubuntu 24.04) as root, Harbour in `/opt/harbour-core`.

1. Packages and Harbour with the contribs CBro needs:

   ```
   apt-get install -y build-essential git libx11-dev unixodbc-dev xfonts-base
   git clone https://github.com/harbour/core /opt/harbour-core
   cd /opt/harbour-core
   HB_BUILD_CONTRIBS="hbmzip hbziparc hbmemio rddsql sddsqlt3 sddodbc" make
   export PATH=/opt/harbour-core/bin/linux/gcc:$PATH
   ```

2. Repository, XLSX class and library:

   ```
   git clone <Harbour-CUI repository> ~/Harbour-CUI
   git clone https://github.com/digikv/xlsx ~/xlsx          (or copy XLSXClass.prg there)
   cd ~/Harbour-CUI && hbmk2 hbtui.hbp
   ```

   `samples/cbro` is not in the repository yet (git-ignored): copy it to `~/Harbour-CUI/samples/cbro`.

3. LETO client libraries (copy the LetoDBf / LetoDB sources first, so their folders stay as they are):

   ```
   L=~/Harbour-CUI/samples/cbro/lib/linux
   mkdir -p $L/letodbf/.hbmk $L/letodb/.hbmk
   cd ~/src/letodbf && hbmk2 rddleto.hbp -o$L/letodbf/rddleto -workdir=$L/letodbf/.hbmk
   cd ~/src/letodb  && hbmk2 rddleto.hbp -o$L/letodb/rddleto  -workdir=$L/letodb/.hbmk
   ```

4. CBro:

   ```
   cd ~/Harbour-CUI/samples/cbro
   hbmk2 cbro.hbp                                   LetoDBf client
   hbmk2 cbro.hbp -env:CBRO_LETODB=yes              LetoDB client
   hbmk2 cbro.hbp -env:CBRO_NOCOMPILER=yes          commercial build
   ./cbro
   ```

   The program needs an X display (gtxwc). ODBC needs unixODBC at run time; printing uses `lp`,
   OS commands `sh -c`. For CJK text install `xfonts-unifont` and build the "Unifont Cell" font
   (`tools/unifont-cell.sh` in the repository).

## Run

```
cbro                                   the Database Selection screen
cbro /DEMO                             demo tables CUSTOMER + ORDERS (made next to the program
                                       when missing; read-only folder: in TEMP)
cbro customer.dbf custid.cdx /X        a table with an index, EXCLUSIVE
cbro PopisDA.xlsx                      a sheet of a workbook
cbro leto://192.168.1.10:2812/data/x.dbf
cbro /?                                all parameters
```

Settings: GlobalSet > Save Global Settings writes `cbro.ini` next to the program.
