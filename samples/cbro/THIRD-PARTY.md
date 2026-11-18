# Third-party components in CBro

CBro (Copyright (c) 2026 Srđan Dragojlović, dual licensed: GPL-3.0-or-later or commercial, see
`LICENSE`) is built with the components below. Their licenses apply to them, also in a CBro
program.

| Component | Used for | License |
|---|---|---|
| Harbour runtime, RDDs, contribs (hbmzip, hbziparc, hbmemio, rddsql, sddodbc, sddsqlt3, hbwin) — https://harbour.github.io | the language runtime, DBF, zip, SQL, Windows printing | GPL-2.0-or-later **with the Harbour exception** (linking with other code is allowed under any terms) |
| Harbour compiler (`hbcplr`, `hbpp`) | `.prg` scripts compiled in memory | GPL-2.0-or-later, **no exception**; not in a commercial build (`CBRO_NOCOMPILER`) |
| Harbour-CUI (hbtui) — https://github.com/ (Srđan Dragojlović) | the character UI | MIT |
| XLSX class (`XLSXClass.prg`) — https://github.com/digikv/xlsx (Srđan Dragojlović) | reading and writing `.xlsx` | dual: GPL-3.0-or-later or commercial (same author as CBro) |
| LetoDBf client — Alexander S. Kresin, Pavel Tsarenko and others; the "elch" fork of LetoDB | the LETO driver (default build) | GPL-2.0-or-later with the Harbour exception |
| LetoDB client — Alexander S. Kresin | the LETO driver (`build.bat letodb`) | GPL-2.0-or-later with the Harbour exception |
| SQLite (bundled with sddsqlt3) — https://sqlite.org | SQLite databases | public domain |
| zlib, minizip (bundled with Harbour) | zip / xlsx files | zlib license |
| unixODBC (Linux, at run time) | ODBC | LGPL-2.1 (shared library) |

BroPlus (Peter Volz, SmartCode Programming) is not a component: CBro only follows its menus and
keys and contains none of its code.
