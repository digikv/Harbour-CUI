/*
 * CuiBuilder - shared definitions
 *
 * Copyright (c) 2026 Srđan Dragojlović - dual licensed, see LICENSE
 */

#ifndef CUIBUILD_CH_
#define CUIBUILD_CH_

#define CB_VERSION      "1.0.3"

/* the kinds of build messages: bOut( cText, nType, cFile, nLine ) */
#define CB_MSG_INFO     0
#define CB_MSG_TITLE    1       /* === a project */
#define CB_MSG_STEP     2       /* Harbour: a.prg */
#define CB_MSG_CMD      3       /* the command line */
#define CB_MSG_OUT      4       /* what a tool wrote */
#define CB_MSG_WARN     5
#define CB_MSG_ERROR    6
#define CB_MSG_OK       7       /* built */

#endif
