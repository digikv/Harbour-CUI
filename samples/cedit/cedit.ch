/*
 * CEdit - text editor in character mode (Harbour-CUI)
 * Copyright (c) 2026 Srđan Dragojlović - dual licensed, see LICENSE
 */

#ifndef CEDIT_CH_
#define CEDIT_CH_

#define CE_VERSION      "1.0.8"

/* a file bigger than this is not loaded into memory (a 32-bit process cannot
   hold a 2 GB string). It is kept on disk: line index + optional .add swap. */
#ifndef CE_MEM_LIMIT
#define CE_MEM_LIMIT    ( 32 * 1024 * 1024 )
#endif
#ifndef CE_BLOCK
#define CE_BLOCK        ( 1024 * 1024 )  /* save / bulk I/O */
#endif
#ifndef CE_INDEX_BLOCK
#define CE_INDEX_BLOCK  ( 64 * 1024 )    /* one read while indexing (keep UI alive) */
#endif
#ifndef CE_LINE_CAP
#define CE_LINE_CAP     262144
#endif
/* line-start index on disk (8 bytes each). load() indexes CE_FIRST_STEP bytes
   first; the rest continues while the file is shown. */
#ifndef CE_LINE_STEP
#define CE_LINE_STEP    1
#endif
#ifndef CE_FIRST_STEP
#define CE_FIRST_STEP   ( 128 * 1024 )   /* first screen; rest in idle */
#endif
#ifndef CE_IDLE_STEP
#define CE_IDLE_STEP    ( 64 * 1024 )    /* per idle tick; many ticks via time budget */
#endif
#ifndef CE_IDLE_MS
#define CE_IDLE_MS      40              /* spend up to this many ms indexing per tick */
#endif
#ifndef CE_IDX_BUF
#define CE_IDX_BUF      ( 32 * 1024 )   /* flush .idx every 4k line starts */
#endif
#ifndef CE_IDX_MAX_BREAKS
#define CE_IDX_MAX_BREAKS  8000         /* cap lines scanned per indexStep */
#endif
#ifndef CE_PAGE_SIZE
#define CE_PAGE_SIZE    65536
#endif
#ifndef CE_CACHE_PAGES
#define CE_CACHE_PAGES  256
#endif

/* piece table: bytes from the original file or from the append-only .add swap */
#define CE_SRC_ORIG     0
#define CE_SRC_ADD      1

/* selection: none, stream (Shift + arrows, F3), vertical block (Ctrl-F3) */
#define CE_SEL_NONE     0
#define CE_SEL_STREAM   1
#define CE_SEL_BLOCK    2

/* undo record */
#define UR_GROUP        1
#define UR_TYPE         2     /* "I" insert, "D" delete */
#define UR_ROW1         3
#define UR_COL1         4
#define UR_ROW2         5     /* insert: end of the inserted text */
#define UR_COL2         6
#define UR_TEXT         7
#define UR_CURROW       8     /* cursor before the change */
#define UR_CURCOL       9

/* Windows virtual keys, for ht_keyDown(): Ctrl-Z and Ctrl-Left have the same
   Inkey code (26), Ctrl-B and Ctrl-Right too (2) */
#define VK_KEY_B        0x42
#define VK_KEY_Z        0x5A

#endif
