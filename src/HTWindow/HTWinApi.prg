/*
 * Harbour-CUI CT-compatible window API shims (SaveScreen/RestScreen).
 *
 * Framework foundation: https://github.com/tfonrouge/HBTui
 *
 * Replaces hbct ctwin GT layer — no hbct.hbc dependency.
 */

#include "hbgtinfo.ch"

#define _HT_BOX_SINGLE  e"\xDA\xC4\xBF\xB3\xD9\xC4\xC0\xB3"

STATIC s_aWindows   := { => }
STATIC s_aZOrder    := {}
STATIC s_nCurrent   := 0
STATIC s_nNextId    := 1
STATIC s_nShadow    := -1   /* off until a secondary window enables HT_SHADOW_ATTR */
STATIC s_aBoard     := { 0, 0, 0, 0 }
STATIC s_lInit      := .F.

STATIC FUNCTION ht_winNew( nId, nTop, nLeft, nBottom, nRight )
    LOCAL hWin

    hWin := { ;
        "id"         => nId, ;
        "top"        => nTop, ;
        "left"       => nLeft, ;
        "bottom"     => nBottom, ;
        "right"      => nRight, ;
        "backImage"  => NIL, ;
        "winImage"   => NIL, ;
        "shadBottom" => nBottom, ;
        "shadRight"  => nRight, ;
        "fmtTop"     => 0, ;
        "fmtLeft"    => 0, ;
        "fmtBottom"  => 0, ;
        "fmtRight"   => 0, ;
        "color"      => NIL ;
        }

    s_aWindows[ nId ] := hWin

RETURN hWin

STATIC FUNCTION ht_winGet( nId )
RETURN IIF( nId != NIL .AND. hb_hHasKey( s_aWindows, nId ), s_aWindows[ nId ], NIL )

STATIC FUNCTION ht_winShadowExtents( hWin )
    IF s_nShadow >= 0
        hWin[ "shadBottom" ] := hWin[ "bottom" ] + 1
        hWin[ "shadRight"  ] := hWin[ "right"  ] + 2
    ELSE
        hWin[ "shadBottom" ] := hWin[ "bottom" ]
        hWin[ "shadRight"  ] := hWin[ "right"  ]
    ENDIF
RETURN NIL

STATIC FUNCTION ht_winArea( hWin )
RETURN { hWin[ "top" ], hWin[ "left" ], hWin[ "shadBottom" ], hWin[ "shadRight" ] }

STATIC FUNCTION ht_defaultBoxFrame()
    LOCAL cFill := Chr( GetClearB() )
RETURN _HT_BOX_SINGLE + cFill

STATIC FUNCTION ht_colorIndex( xColor, nDefault )
    LOCAL nColor := nDefault

    IF HB_ISNUMERIC( xColor )
        nColor := xColor
    ELSEIF HB_ISCHAR( xColor ) .AND. Len( xColor ) > 0
        nColor := hb_ColorToN( xColor )
        IF nColor < 0
            nColor := nDefault
        ENDIF
    ENDIF

RETURN nColor

STATIC FUNCTION ht_drawBoxAbs( nTop, nLeft, nBottom, nRight, xBox, xColor )
    LOCAL cBox := ht_defaultBoxFrame()
    LOCAL nColor := ht_colorIndex( xColor, -1 )

    IF HB_ISCHAR( xBox ) .AND. Len( xBox ) >= 8
        cBox := xBox
        IF Len( cBox ) < 9
            cBox += Chr( GetClearB() )
        ENDIF
    ENDIF

    HT_RAWDISPBOX( nTop, nLeft, nBottom, nRight, cBox, nColor )

RETURN NIL

STATIC FUNCTION ht_updateDesktopBounds()
    LOCAL hDesk := ht_winGet( 0 )

    IF hDesk != NIL
        hDesk[ "bottom"     ] := ht_ScreenMaxRow()
        hDesk[ "right"      ] := ht_ScreenMaxCol()
        hDesk[ "shadBottom" ] := hDesk[ "bottom" ]
        hDesk[ "shadRight"  ] := hDesk[ "right"  ]
        s_aBoard[ 3 ] := hDesk[ "bottom" ]
        s_aBoard[ 4 ] := hDesk[ "right"  ]
    ENDIF

RETURN NIL

/** Pushes current window position + wFormat margins to the C viewport layer. */
FUNCTION ht_SyncViewport()
    LOCAL hWin
    LOCAL nOffRow, nOffCol, nMaxRow, nMaxCol

    hWin := ht_winGet( s_nCurrent )

    IF hWin == NIL
        HT_SETVIEWPORT( 0, 0, -1, 0 )
        RETURN NIL
    ENDIF

    nOffRow := hWin[ "top"  ] + hWin[ "fmtTop"  ]
    nOffCol := hWin[ "left" ] + hWin[ "fmtLeft" ]

    nMaxRow := hWin[ "bottom" ] - hWin[ "top" ] - hWin[ "fmtTop" ] - hWin[ "fmtBottom" ]
    nMaxCol := hWin[ "right"  ] - hWin[ "left" ] - hWin[ "fmtLeft" ] - hWin[ "fmtRight" ]

    IF nMaxRow < 0
        nMaxRow := 0
    ENDIF
    IF nMaxCol < 0
        nMaxCol := 0
    ENDIF

    HT_SETWINORIGIN( hWin[ "top" ], hWin[ "left" ] )
    HT_SETVIEWPORT( nOffRow, nOffCol, nMaxRow, nMaxCol )

RETURN NIL

/** Applies Windows/CUI RGB palette slots 9-15 (harbour-cui Application:New).
 * Values are Windows COLORREF (0x00BBGGRR), not RRGGBB.
 */
FUNCTION ht_cuiApplyPalette()
    LOCAL aPal, nI

    /* keep defaults for 0-8, override 9-15 like harbour-cui */
    aPal := hb_gtInfo( HB_GTI_PALETTE )
    IF ! HB_ISARRAY( aPal ) .OR. Len( aPal ) != 16
        aPal := Array( 16 )
        FOR nI := 1 TO 16
            aPal[ nI ] := 0
        NEXT
    ENDIF

    aPal[ 10 ] := ht_rgb( 160, 160, 160 )  /* 9  window gray */
    aPal[ 11 ] := ht_rgb( 240, 240, 240 )  /* 10 menu bar */
    aPal[ 12 ] := ht_rgb( 153, 180, 209 )  /* 11 active title (Windows blue) */
    aPal[ 13 ] := ht_rgb( 171, 171, 171 )  /* 12 app workspace */
    aPal[ 14 ] := ht_rgb( 128, 128, 128 )  /* 13 dark gray */
    aPal[ 15 ] := ht_rgb( 166, 202, 240 )  /* 14 pastel blue */
    aPal[ 16 ] := ht_rgb( 169, 169, 169 )  /* 15 gray */

    hb_gtInfo( HB_GTI_PALETTE, aPal )
RETURN NIL

/** Windows COLORREF from R,G,B (0..255). */
FUNCTION ht_rgb( nR, nG, nB )
RETURN nR + nG * 256 + nB * 65536

/** Initializes the window system and virtual desktop (window 0). */
FUNCTION ctWInit()
    IF ! s_lInit
        s_lInit := .T.
        ht_cuiApplyPalette()
        s_aWindows := { => }
        s_aZOrder  := { 0 }
        s_nCurrent := 0
        s_nNextId  := 1
        ht_winNew( 0, 0, 0, ht_ScreenMaxRow(), ht_ScreenMaxCol() )
        ht_updateDesktopBounds()
        ht_SyncViewport()
    ENDIF
RETURN .T.

/** Opens a new window at screen coordinates; returns numeric window id. */
FUNCTION wOpen( nTop, nLeft, nBottom, nRight, lClear )
    LOCAL nId
    LOCAL hWin
    LOCAL aArea

    ctWInit()

    nId  := s_nNextId++
    hWin := ht_winNew( nId, nTop, nLeft, nBottom, nRight )
    ht_winShadowExtents( hWin )

    HT_SETVIEWPORT( 0, 0, -1, 0 )

    aArea := ht_winArea( hWin )
    hWin[ "backImage" ] := SaveScreen( aArea[ 1 ], aArea[ 2 ], aArea[ 3 ], aArea[ 4 ] )

    IF s_nShadow >= 0
        hb_Shadow( hWin[ "top" ], hWin[ "left" ], hWin[ "bottom" ], hWin[ "right" ] )
    ENDIF

    IF lClear
        Scroll( hWin[ "top" ] + 1, hWin[ "left" ] + 1, ;
                hWin[ "bottom" ] - 1, hWin[ "right" ] - 1 )
    ENDIF

    AAdd( s_aZOrder, nId )
    s_nCurrent := nId
    hWin[ "fmtTop" ] := 0
    hWin[ "fmtLeft" ] := 0
    hWin[ "fmtBottom" ] := 0
    hWin[ "fmtRight" ] := 0
    ht_SyncViewport()

RETURN nId

/** Closes a window, restoring the saved background. */
FUNCTION wClose( nId )
    LOCAL hWin
    LOCAL aArea
    LOCAL nPrev
    LOCAL i

    IF nId == NIL
        nId := s_nCurrent
    ENDIF

    IF nId == 0 .OR. ht_winGet( nId ) == NIL
        RETURN 0
    ENDIF

    hWin  := ht_winGet( nId )
    aArea := ht_winArea( hWin )

    HT_SETVIEWPORT( 0, 0, -1, 0 )
    RestScreen( aArea[ 1 ], aArea[ 2 ], aArea[ 3 ], aArea[ 4 ], hWin[ "backImage" ] )

    hb_hDel( s_aWindows, nId )

    i := AScan( s_aZOrder, nId )
    IF i > 0
        hb_ADel( s_aZOrder, i, .T. )
    ENDIF

    nPrev := 0
    IF s_nCurrent == nId
        IF Len( s_aZOrder ) > 0
            nPrev := s_aZOrder[ Len( s_aZOrder ) ]
        ENDIF
        wSelect( nPrev, .F. )
    ENDIF

RETURN nPrev

/** Selects the active window; no args returns current id. */
FUNCTION wSelect( nId, lBringToFront )
    LOCAL i

    ctWInit()

    IF nId == NIL
        RETURN s_nCurrent
    ENDIF

    IF ! hb_hHasKey( s_aWindows, nId )
        RETURN -1
    ENDIF

    IF lBringToFront == NIL
        lBringToFront := .T.
    ENDIF

    IF lBringToFront
        i := AScan( s_aZOrder, nId )
        IF i > 0
            hb_ADel( s_aZOrder, i, .T. )
            AAdd( s_aZOrder, nId )
        ENDIF
    ENDIF

    s_nCurrent := nId

    IF nId == 0
        ht_updateDesktopBounds()
    ENDIF

    ht_SyncViewport()

RETURN nId

/** Sets additive viewport margins; no args resets to 0,0,0,0. */
FUNCTION wFormat( nTop, nLeft, nBottom, nRight )
    LOCAL hWin

    hWin := ht_winGet( s_nCurrent )

    IF hWin == NIL
        RETURN -1
    ENDIF

    IF nTop == NIL
        hWin[ "fmtTop"    ] := 0
        hWin[ "fmtLeft"   ] := 0
        hWin[ "fmtBottom" ] := 0
        hWin[ "fmtRight"  ] := 0
    ELSE
        hWin[ "fmtTop"    ] := Max( hWin[ "fmtTop"    ] + nTop,    0 )
        hWin[ "fmtLeft"   ] := Max( hWin[ "fmtLeft"   ] + nLeft,   0 )
        hWin[ "fmtBottom" ] := Max( hWin[ "fmtBottom" ] + nBottom, 0 )
        hWin[ "fmtRight"  ] := Max( hWin[ "fmtRight"  ] + nRight,  0 )
    ENDIF

    ht_SyncViewport()

RETURN 0

/** Draws a border on the current window and sets 1,1,1,1 content margins. */
FUNCTION wBox( xBox, cColor )
    LOCAL hWin
    LOCAL nTop, nLeft, nBottom, nRight

    hWin := ht_winGet( s_nCurrent )

    IF hWin == NIL
        RETURN -1
    ENDIF

    IF cColor != NIL
        hWin[ "color" ] := cColor
    ENDIF

    HT_SETVIEWPORT( 0, 0, -1, 0 )

    nTop    := hWin[ "top"    ]
    nLeft   := hWin[ "left"   ]
    nBottom := hWin[ "bottom" ]
    nRight  := hWin[ "right"  ]

    ht_drawBoxAbs( nTop, nLeft, nBottom, nRight, xBox, cColor )

    IF s_nCurrent > 0
        hWin[ "fmtTop"    ] := 1
        hWin[ "fmtLeft"   ] := 1
        hWin[ "fmtBottom" ] := 1
        hWin[ "fmtRight"  ] := 1
    ENDIF

    ht_SyncViewport()

RETURN 0

/** Moves the current window to an absolute screen position. */
FUNCTION wMove( nRow, nCol )
    LOCAL hWin
    LOCAL aArea
    LOCAL nDeltaRow, nDeltaCol
    LOCAL nHeight, nWidth

    hWin := ht_winGet( s_nCurrent )

    IF hWin == NIL .OR. s_nCurrent == 0
        RETURN -1
    ENDIF

    nDeltaRow := nRow - hWin[ "top"  ]
    nDeltaCol := nCol - hWin[ "left" ]

    IF nDeltaRow == 0 .AND. nDeltaCol == 0
        RETURN 0
    ENDIF

    HT_SETVIEWPORT( 0, 0, -1, 0 )

    DispBegin()

    aArea := ht_winArea( hWin )
    RestScreen( aArea[ 1 ], aArea[ 2 ], aArea[ 3 ], aArea[ 4 ], hWin[ "backImage" ] )

    nHeight := hWin[ "bottom" ] - hWin[ "top" ] + 1
    nWidth  := hWin[ "right"  ] - hWin[ "left" ] + 1

    hWin[ "top"    ] += nDeltaRow
    hWin[ "left"   ] += nDeltaCol
    hWin[ "bottom" ] := hWin[ "top" ] + nHeight - 1
    hWin[ "right"  ] := hWin[ "left" ] + nWidth - 1

    ht_winShadowExtents( hWin )

    aArea := ht_winArea( hWin )
    hWin[ "backImage" ] := SaveScreen( aArea[ 1 ], aArea[ 2 ], aArea[ 3 ], aArea[ 4 ] )

    IF hWin[ "winImage" ] != NIL
        RestScreen( hWin[ "top" ], hWin[ "left" ], hWin[ "bottom" ], hWin[ "right" ], hWin[ "winImage" ] )
        IF s_nShadow >= 0
            hb_Shadow( hWin[ "top" ], hWin[ "left" ], hWin[ "bottom" ], hWin[ "right" ] )
        ENDIF
    ELSE
        ht_drawBoxAbs( hWin[ "top" ], hWin[ "left" ], hWin[ "bottom" ], hWin[ "right" ], ;
            NIL, hWin[ "color" ] )
        IF s_nShadow >= 0
            hb_Shadow( hWin[ "top" ], hWin[ "left" ], hWin[ "bottom" ], hWin[ "right" ] )
        ENDIF
    ENDIF

    DispEnd()

    ht_SyncViewport()

RETURN 0

/** Returns the top row of the current window (screen-absolute). */
FUNCTION wRow()
    LOCAL hWin := ht_winGet( s_nCurrent )
RETURN IIF( hWin != NIL, hWin[ "top" ], 0 )

/** Returns the left column of the current window (screen-absolute). */
FUNCTION wCol()
    LOCAL hWin := ht_winGet( s_nCurrent )
RETURN IIF( hWin != NIL, hWin[ "left" ], 0 )

/** Stores or returns the window board (movable area). */
FUNCTION wBoard( nTop, nLeft, nBottom, nRight )
    IF nTop == NIL .AND. nLeft == NIL .AND. nBottom == NIL .AND. nRight == NIL
        s_aBoard[ 1 ] := 0
        s_aBoard[ 2 ] := 0
        s_aBoard[ 3 ] := ht_ScreenMaxRow()
        s_aBoard[ 4 ] := ht_ScreenMaxCol()
    ELSEIF nTop != NIL .OR. nLeft != NIL .OR. nBottom != NIL .OR. nRight != NIL
        IF nTop != NIL
            s_aBoard[ 1 ] := nTop
        ENDIF
        IF nLeft != NIL
            s_aBoard[ 2 ] := nLeft
        ENDIF
        IF nBottom != NIL
            s_aBoard[ 3 ] := nBottom
        ELSE
            s_aBoard[ 3 ] := ht_ScreenMaxRow()
        ENDIF
        IF nRight != NIL
            s_aBoard[ 4 ] := nRight
        ELSE
            s_aBoard[ 4 ] := ht_ScreenMaxCol()
        ENDIF
    ENDIF
RETURN 0

/** Border mode — no-op compatibility stub. */
FUNCTION wMode( nTop, nLeft, nBottom, nRight )
    HB_SYMBOL_UNUSED( nTop )
    HB_SYMBOL_UNUSED( nLeft )
    HB_SYMBOL_UNUSED( nBottom )
    HB_SYMBOL_UNUSED( nRight )
RETURN 0

/** Sets the default shadow color attribute; returns previous value. */
FUNCTION wSetShadow( nAttr )
    LOCAL nOld := s_nShadow
    IF nAttr != NIL
        s_nShadow := nAttr
    ENDIF
RETURN nOld

/** Sets the screen clear color (attribute A). */
FUNCTION SetClearA( xColor )
    HT_SETCLEARA( xColor )
RETURN NIL

/** Sets the screen clear character (attribute B). */
FUNCTION SetClearB( x )
    HT_SETCLEARB( x )
RETURN NIL

/** Returns the screen clear character. */
FUNCTION GetClearB()
RETURN HT_GETCLEARB()

/** Captures current window content for wMove drag (optional). */
FUNCTION ht_winCapture( nId )
    LOCAL hWin

    IF nId == NIL
        nId := s_nCurrent
    ENDIF

    hWin := ht_winGet( nId )

    IF hWin != NIL .AND. nId > 0
        HT_SETVIEWPORT( 0, 0, -1, 0 )
        hWin[ "winImage" ] := SaveScreen( hWin[ "top" ], hWin[ "left" ], ;
            hWin[ "bottom" ], hWin[ "right" ] )
        ht_SyncViewport()
    ENDIF

RETURN NIL

/** Hit-test: returns window id at screen-absolute row/col (topmost wins). */
FUNCTION ht_winAtPos( nRow, nCol )
    LOCAL i, nId, hWin

    FOR i := Len( s_aZOrder ) TO 1 STEP -1
        nId  := s_aZOrder[ i ]
        hWin := ht_winGet( nId )
        IF hWin != NIL
            IF nRow >= hWin[ "top" ] .AND. nRow <= hWin[ "bottom" ] .AND. ;
               nCol >= hWin[ "left" ] .AND. nCol <= hWin[ "right" ]
                RETURN nId
            ENDIF
        ENDIF
    NEXT

RETURN 0

/** Returns the topmost window id at the current mouse position. */
FUNCTION ht_windowAtMousePos()
    LOCAL nRow := ht_mRow( .T. )
    LOCAL nCol := ht_mCol( .T. )
RETURN ht_winAtPos( nRow, nCol )
