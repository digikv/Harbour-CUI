/*
 * CEdit - text editor in character mode (Harbour-CUI)
 * Copyright (c) 2026 Srđan Dragojlović - dual licensed, see LICENSE
 */

#ifndef CEDIT_CH_
#define CEDIT_CH_

#define CE_VERSION      "1.0.4"

/* a file bigger than this is opened read-only without reading it into memory
   (CELargeBuffer, celarge.prg) */
#define CE_LARGE_FILE   ( 32 * 1024 * 1024 )
#define CE_INDEX_STEP   ( 4 * 1024 * 1024 )    /* bytes indexed in one step */
#define CE_INDEX_MS     80                     /* steps per event loop turn: at most this long */

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
