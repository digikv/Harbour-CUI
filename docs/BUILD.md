# Building Harbour-CUI, the samples and CBro

What each part needs is listed in [DEPENDENCIES.md](DEPENDENCIES.md); this page says how to build it.
Everything must be built with the same Harbour: the library, the LETO client libraries and the
programs. A library built with another Harbour version must be built again.

## 1. Harbour

The newest Harbour from https://github.com/harbour/core, built as its README describes.

- **Windows**: 32-bit MinGW (MSYS2 `mingw32`), e.g. in `D:\tools\harbour-core`;
  `hbmk2` is `D:\tools\harbour-core\bin\win\mingw\hbmk2.exe`, the MinGW compiler
  (`C:\msys64\mingw32\bin`) must be in the PATH.
- **Linux**: `make` in the Harbour folder; CBro also needs some contribs:

  ```
  apt-get install -y build-essential git libx11-dev unixodbc-dev xfonts-base
  git clone https://github.com/harbour/core /opt/harbour-core
  cd /opt/harbour-core
  HB_BUILD_CONTRIBS="hbmzip hbziparc hbmemio rddsql sddsqlt3 sddodbc" make
  export PATH=/opt/harbour-core/bin/linux/gcc:$PATH
  ```

## 2. The library

In the repository root:

```
hbmk2 hbtui.hbp
```

It writes `lib/<platform>/<compiler>/libhbtui.a`. The library needs only Harbour.

## 3. A sample

```
cd samples/hello
hbmk2 hello.prg hbmk.hbm
```

All samples build like this (`hbmk.hbm` links the library with `../../hbtui.hbc`), except CBro,
which has its own project.

## 4. CBro (`samples/cbro`)

### The folders

CBro uses the XLSX class (https://github.com/digikv/xlsx) for Excel files. It is a separate
repository and is expected **next to** the Harbour-CUI folder:

```
<projects>/
    Harbour-CUI/          this repository
        samples/cbro/
    xlsx/                 https://github.com/digikv/xlsx  (XLSXClass.prg)
```

### Windows: `build.bat`

```
cd samples\cbro
build                     LetoDBf client (GPL build, scripts compiled in memory)
build letodb              LetoDB client
build commercial          without the Harbour compiler (for the commercial license)
build update              first git pull the XLSX class, then build
```

- When `..\..\..\xlsx\XLSXClass.prg` is missing, `build.bat` clones it:
  `git clone https://github.com/digikv/xlsx ..\..\..\xlsx` (git must be in the PATH).
- Without git (and without the class) CBro is built **without Excel**: everything works, the
  Excel commands say that the XLSX class was not there.
- `build update ...` runs `git pull --ff-only` in the `xlsx` folder first (an existing folder is
  never changed otherwise).
- The class in another folder: `set CBRO_XLSXDIR=<folder>` before `build`.
- `set HBMK2=<path to hbmk2.exe>` when Harbour is not in `D:\tools\harbour-core`.

### Any platform: `hbmk2 cbro.hbp`

```
cd samples/cbro
git clone https://github.com/digikv/xlsx ../../../xlsx      (once, for Excel)
hbmk2 cbro.hbp
```

| Switch | Effect |
|---|---|
| (none) | with the XLSX class from `../../../xlsx` if it is there, else without Excel |
| `-env:CBRO_NOXLSX=yes` | without Excel, also when the class is there |
| `-env:CBRO_XLSXOWN=yes <folder>/XLSXClass.prg` | the class from another folder |
| `-env:CBRO_LETODB=yes` | the LetoDB client instead of LetoDBf |
| `-env:CBRO_NOCOMPILER=yes` | without the Harbour compiler (commercial license) |

### LETO client libraries

CBro links `samples/cbro/lib/<platform>/letodbf/librddleto.a` (or `letodb`). They are built once
from the LetoDBf / LetoDB sources with their `rddleto.hbp` - see
[samples/cbro/README.md](../samples/cbro/README.md).

## Problems

| Message | Cause, what to do |
|---|---|
| `Cannot open ..\..\..\xlsx\XLSXClass.prg, assumed external` | an older `cbro.hbp`: the XLSX class is not next to the repository. Update Harbour-CUI (`git pull`) - CBro is then built without Excel - or clone https://github.com/digikv/xlsx next to the Harbour-CUI folder |
| `undefined reference to HB_FUN_WORKBOOK` (link) | the same: the XLSX class was not compiled in |
| `cbro.exe` asks for `libsqlite3-0.dll` | the SQLite of MSYS2 was linked (as a DLL): Harbour's own SQLite was not built. Build it once - `hbmk2 sqlite3.hbp` in `<harbour>\contrib\3rd\sqlite3`, copy `libsqlite3.a` to `<harbour>\lib\win\mingw` - and build CBro again. `objdump -p cbro.exe \| findstr DLL` must list only Windows DLLs |
| `cannot find -lrddleto` | the LETO client library is not in `samples/cbro/lib/<platform>/letodbf` (or `letodb`): build it as above |
| `cannot find -lhbtui` | the library was not built (`hbmk2 hbtui.hbp` in the repository root), or with another compiler |
| boxes instead of Chinese / Japanese / Korean on Linux | the "Unifont Cell" X font: `tools/unifont-cell.sh` (see DEPENDENCIES.md) |
