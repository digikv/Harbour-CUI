/*
 * CNC - definitions shared by cnc.prg and cncpanel.prg
 *
 * Copyright (c) 2026 Srđan Dragojlović - dual licensed, see LICENSE
 */

#define CNC_VERSION     "1.0.3"

/* a panel entry: { cName, nSize, dDate, cTime, cAttr, lDir, lMarked } */
#define F_NAME          1
#define F_SIZE          2
#define F_DATE          3
#define F_TIME          4
#define F_ATTR          5
#define F_DIR           6
#define F_MARK          7

#define CNC_SORT_NAME   1
#define CNC_SORT_EXT    2
#define CNC_SORT_TIME   3
#define CNC_SORT_SIZE   4
#define CNC_SORT_NONE   5

#define CNC_VIEW_FULL   1               /* Name, Ext, Size, Date */
#define CNC_VIEW_BRIEF  2               /* two columns of names (NC) */
#define CNC_VIEW_TC     3               /* columns as wide as the longest name (Total Commander) */

/* the classic colors (VGA palette of the "Norton" theme) */
#define CNC_CLR_PANEL   "11/1"          /* files, frame: bright cyan on blue */
#define CNC_CLR_DIR     "15/1"          /* folders: white */
#define CNC_CLR_MARK    "14/1"          /* marked: yellow */
#define CNC_CLR_HEAD    "14/1"          /* column titles */
#define CNC_CLR_CURSOR  "0/3"           /* the cursor bar: black on cyan */
#define CNC_CLR_CURMARK "14/3"          /* the cursor on a marked file */
#define CNC_CLR_PATH    "0/3"           /* the path of the active panel */
#define CNC_CLR_KEYNUM  "7/0"           /* key bar: the numbers */
#define CNC_CLR_KEYTXT  "0/3"           /* key bar: the texts */
