/*
 * Harbour-CUI CT-compatible window API shims (SaveScreen/RestScreen).
 *
 * Framework foundation: https://github.com/tfonrouge/HBTui
 *
 * Replaces hbct ctwin GT layer — no hbct.hbc dependency.
 */

#include "hbgtinfo.ch"
#include "inkey.ch"

#define _HT_BOX_SINGLE  e"\xDA\xC4\xBF\xB3\xD9\xC4\xC0\xB3"

STATIC s_aWindows   := { => }
STATIC s_aZOrder    := {}
STATIC s_nCurrent   := 0
STATIC s_nNextId    := 1
STATIC s_nShadow    := -1   /* off until a secondary window enables HT_SHADOW_ATTR */
STATIC s_aBoard     := { 0, 0, 0, 0 }
STATIC s_lInit      := .F.
STATIC s_cDesktop   := NIL  /* full-screen base layer (stack bottom) */

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
        "color"      => NIL, ;
        "shadow"     => s_nShadow, ;
        "moveImage"  => NIL, ;
        "layerImage" => NIL, ;
        "dragging"   => .F. ;
        }

    s_aWindows[ nId ] := hWin

RETURN hWin

STATIC FUNCTION ht_winGet( nId )
RETURN IIF( nId != NIL .AND. hb_hHasKey( s_aWindows, nId ), s_aWindows[ nId ], NIL )

STATIC FUNCTION ht_winShadowExtents( hWin )
    LOCAL nShadow := IIF( hb_hHasKey( hWin, "shadow" ), hWin[ "shadow" ], s_nShadow )

    IF nShadow >= 0
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

/** Configures GT display like digikv awin.prg:
 * fullscreen (no OS chrome), SetMode, then scale Consolas to the desktop.
 * A second SetMode to DESKTOPCOLS/ROWS removes the large black side margins
 * that gtwvt leaves when Consolas metrics are narrower than the requested
 * FONTWIDTH (FitSize centers the leftover as MarginLeft/Right).
 * @param nRows  Preferred rows (default 40); final size may grow to fill screen
 * @param nCols  Preferred cols (default 131); final size may grow to fill screen
 */
FUNCTION ht_cuiSetupDisplay( nRows, nCols )
    LOCAL nWidth, nHeight, nFontW, nFontH
    LOCAL nDeskCols, nDeskRows

    IF nRows == NIL
        nRows := 40
    ENDIF
    IF nCols == NIL
        nCols := 131
    ENDIF

#ifdef __PLATFORM__WINDOWS
    hb_gtInfo( HB_GTI_FONTNAME, "Consolas" )
    hb_gtInfo( HB_GTI_SELECTCOPY, "Select and Copy" )
#else
    hb_gtInfo( HB_GTI_FONTNAME, "Consolas" )
#endif
    hb_gtInfo( HB_GTI_RESIZEMODE, HB_GTI_RESIZEMODE_FONT )
    hb_gtInfo( HB_GTI_FONTQUALITY, HB_GTI_FONTQ_HIGH )
    hb_gtInfo( HB_GTI_FONTWEIGHT, HB_GTI_FONTW_NORMAL )
    hb_gtInfo( HB_GTI_CLOSABLE, .T. )
    hb_gtInfo( HB_GTI_MAXIMIZED, .T. )
    hb_gtInfo( HB_GTI_RESIZABLE, .T. )
    /* Digi/awin: borderless client = full desktop (no OS title-bar frame) */
    hb_gtInfo( HB_GTI_ISFULLSCREEN, .T. )
    hb_gtInfo( HB_GTI_ALTENTER, .T. )
    hb_gtInfo( HB_GTI_COMPATBUFFER, .T. )

    SetMode( nRows, nCols )

    nWidth  := hb_gtInfo( HB_GTI_DESKTOPWIDTH )
    nHeight := hb_gtInfo( HB_GTI_DESKTOPHEIGHT )
    IF HB_ISNUMERIC( nWidth ) .AND. nWidth > 0 .AND. ;
       HB_ISNUMERIC( nHeight ) .AND. nHeight > 0 .AND. ;
       MaxCol() >= 0 .AND. MaxRow() >= 0
        nFontW := Int( nWidth / ( MaxCol() + 1 ) )
        nFontH := Int( nHeight / ( MaxRow() + 1 ) )
        IF nFontW < 4
            nFontW := 4
        ENDIF
        IF nFontH < 8
            nFontH := 8
        ENDIF
        hb_gtInfo( HB_GTI_FONTWIDTH, nFontW )
        hb_gtInfo( HB_GTI_FONTSIZE, nFontH )

        /* Grow the character grid to what actually fits at the real cell size.
         * Without this, Consolas often renders narrower than FONTWIDTH and
         * gtwvt paints large black margins on both sides. */
        nDeskCols := hb_gtInfo( HB_GTI_DESKTOPCOLS )
        nDeskRows := hb_gtInfo( HB_GTI_DESKTOPROWS )
        IF HB_ISNUMERIC( nDeskCols ) .AND. nDeskCols >= 40 .AND. ;
           HB_ISNUMERIC( nDeskRows ) .AND. nDeskRows >= 20
            IF nDeskCols != MaxCol() + 1 .OR. nDeskRows != MaxRow() + 1
                SetMode( nDeskRows, nDeskCols )
                nFontW := Int( nWidth / ( MaxCol() + 1 ) )
                nFontH := Int( nHeight / ( MaxRow() + 1 ) )
                IF nFontW < 4
                    nFontW := 4
                ENDIF
                IF nFontH < 8
                    nFontH := 8
                ENDIF
                hb_gtInfo( HB_GTI_FONTWIDTH, nFontW )
                hb_gtInfo( HB_GTI_FONTSIZE, nFontH )
            ENDIF
        ENDIF
    ENDIF

    ht_cuiApplyPalette()

RETURN NIL

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
        /* full window clear — CUI chrome has no 1-char CT border ring */
        Scroll( hWin[ "top" ], hWin[ "left" ], ;
                hWin[ "bottom" ], hWin[ "right" ] )
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

    /* recomposite remaining stack so holes under the closed window are filled */
    ht_winCompose( 0 )

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

/** Snapshot the full screen as the desktop base layer (bottom of the stack). */
FUNCTION ht_winSaveDesktop()
    HT_SETVIEWPORT( 0, 0, -1, 0 )
    s_cDesktop := SaveScreen( 0, 0, ht_ScreenMaxRow(), ht_ScreenMaxCol() )
    ht_SyncViewport()
RETURN NIL

/** Blit one window layer (chrome) onto the physical screen; shadow applied after. */
STATIC PROCEDURE ht_winBlitLayer( hWin )
    LOCAL cImg

    IF hWin == NIL
        RETURN
    ENDIF

    cImg := hWin[ "winImage" ]
    IF cImg == NIL
        cImg := hWin[ "layerImage" ]
    ENDIF
    IF cImg == NIL
        RETURN
    ENDIF

    RestScreen( hWin[ "top" ], hWin[ "left" ], ;
        hWin[ "bottom" ], hWin[ "right" ], cImg )
    IF hb_hHasKey( hWin, "shadow" ) .AND. hWin[ "shadow" ] >= 0
        hb_Shadow( hWin[ "top" ], hWin[ "left" ], hWin[ "bottom" ], hWin[ "right" ] )
    ENDIF
RETURN

/** Recompose screen from desktop stack base + windows in z-order.
 * @param nSkipId  Window id to omit (0 = omit none); use while dragging that window
 */
FUNCTION ht_winCompose( nSkipId )
    LOCAL i, nId

    IF nSkipId == NIL
        nSkipId := 0
    ENDIF

    HT_SETVIEWPORT( 0, 0, -1, 0 )

    IF s_cDesktop != NIL
        RestScreen( 0, 0, ht_ScreenMaxRow(), ht_ScreenMaxCol(), s_cDesktop )
    ENDIF

    FOR i := 1 TO Len( s_aZOrder )
        nId := s_aZOrder[ i ]
        IF nId == 0 .OR. nId == nSkipId
            LOOP
        ENDIF
        ht_winBlitLayer( ht_winGet( nId ) )
    NEXT

RETURN NIL

/** Digi/win.prg blocking drag with screen stack compose.
 * Desktop + other windows are the stack under the dragged layer.
 */
FUNCTION ht_winDrag( nId, nGrabAbsRow, nGrabAbsCol )
    LOCAL hWin
    LOCAL cImage
    LOCAL nOffR, nOffC
    LOCAL nH, nW, nShadH, nShadW
    LOCAL nMaxR, nMaxC
    LOCAL nNewR, nNewC
    LOCAL nKey
    LOCAL aArea
    LOCAL lShadow

    hWin := ht_winGet( nId )
    IF hWin == NIL .OR. nId == 0
        RETURN { 0, 0 }
    ENDIF

    /* ensure we have a desktop base */
    IF s_cDesktop == NIL
        ht_winSaveDesktop()
    ENDIF

    wSelect( nId, .T. )
    HT_SETVIEWPORT( 0, 0, -1, 0 )

    nOffR := nGrabAbsRow - hWin[ "top" ]
    nOffC := nGrabAbsCol - hWin[ "left" ]
    nH := hWin[ "bottom" ] - hWin[ "top" ] + 1
    nW := hWin[ "right"  ] - hWin[ "left" ] + 1

    lShadow := hb_hHasKey( hWin, "shadow" ) .AND. hWin[ "shadow" ] >= 0
    IF lShadow
        nShadH := 1
        nShadW := 2
    ELSE
        nShadH := 0
        nShadW := 0
    ENDIF

    nMaxR := ht_ScreenMaxRow()
    nMaxC := ht_ScreenMaxCol()

    /* freeze window chrome only; shadow is recomputed each frame over the stack */
    IF hWin[ "winImage" ] != NIL
        cImage := hWin[ "winImage" ]
    ELSE
        cImage := SaveScreen( hWin[ "top" ], hWin[ "left" ], ;
            hWin[ "bottom" ], hWin[ "right" ] )
        hWin[ "winImage" ] := cImage
    ENDIF
    hWin[ "layerImage" ] := cImage
    hWin[ "dragging" ] := .T.

    DO WHILE MLeftDown()
        nKey := Inkey( 0.01 )
        HB_SYMBOL_UNUSED( nKey )

        nNewR := MRow( .T. ) - nOffR
        nNewC := MCol( .T. ) - nOffC

        IF nNewR < 0 .OR. nNewC < 0
            LOOP
        ENDIF
        IF nNewR + nH - 1 + nShadH > nMaxR
            LOOP
        ENDIF
        IF nNewC + nW - 1 + nShadW > nMaxC
            LOOP
        ENDIF
        IF nNewR == hWin[ "top" ] .AND. nNewC == hWin[ "left" ]
            LOOP
        ENDIF

        DispBegin()
        /* stack: desktop + all other windows */
        ht_winCompose( nId )
        hWin[ "top"    ] := nNewR
        hWin[ "left"   ] := nNewC
        hWin[ "bottom" ] := nNewR + nH - 1
        hWin[ "right"  ] := nNewC + nW - 1
        hWin[ "shadBottom" ] := hWin[ "bottom" ] + nShadH
        hWin[ "shadRight"  ] := hWin[ "right"  ] + nShadW
        /* top of stack: dragged window + live shadow */
        RestScreen( hWin[ "top" ], hWin[ "left" ], ;
            hWin[ "bottom" ], hWin[ "right" ], cImage )
        IF lShadow
            hb_Shadow( hWin[ "top" ], hWin[ "left" ], hWin[ "bottom" ], hWin[ "right" ] )
        ENDIF
        DispEnd()
    ENDDO

    hWin[ "dragging" ] := .F.
    /* refresh under-image for wClose + keep layer in stack */
    aArea := ht_winArea( hWin )
    ht_winCompose( nId )
    hWin[ "backImage" ] := SaveScreen( aArea[ 1 ], aArea[ 2 ], aArea[ 3 ], aArea[ 4 ] )
    RestScreen( hWin[ "top" ], hWin[ "left" ], ;
        hWin[ "bottom" ], hWin[ "right" ], cImage )
    IF lShadow
        hb_Shadow( hWin[ "top" ], hWin[ "left" ], hWin[ "bottom" ], hWin[ "right" ] )
    ENDIF
    hWin[ "layerImage" ] := cImage
    hWin[ "winImage" ] := cImage
    hWin[ "moveImage" ] := NIL
    ht_SyncViewport()

RETURN { hWin[ "left" ], hWin[ "top" ] }

/** Compatibility stubs — drag uses ht_winDrag(). */
FUNCTION ht_winDragBegin( nId )
    HB_SYMBOL_UNUSED( nId )
RETURN .T.

FUNCTION ht_winDragEnd( nId )
    HB_SYMBOL_UNUSED( nId )
RETURN NIL

/** Programmatic move via compose stack + DispBegin. */
FUNCTION wMove( nRow, nCol )
    LOCAL hWin
    LOCAL nHeight, nWidth
    LOCAL nShadH, nShadW
    LOCAL nMaxRow, nMaxCol
    LOCAL cImage
    LOCAL aArea
    LOCAL nId

    hWin := ht_winGet( s_nCurrent )
    IF hWin == NIL .OR. s_nCurrent == 0
        RETURN -1
    ENDIF
    IF nRow == hWin[ "top" ] .AND. nCol == hWin[ "left" ]
        RETURN 0
    ENDIF

    nId := s_nCurrent
    nMaxRow := ht_ScreenMaxRow()
    nMaxCol := ht_ScreenMaxCol()
    nHeight := hWin[ "bottom" ] - hWin[ "top" ] + 1
    nWidth  := hWin[ "right"  ] - hWin[ "left" ] + 1

    IF hb_hHasKey( hWin, "shadow" ) .AND. hWin[ "shadow" ] >= 0
        nShadH := 1
        nShadW := 2
    ELSE
        nShadH := 0
        nShadW := 0
    ENDIF

    IF nRow < 0 .OR. nCol < 0
        RETURN 0
    ENDIF
    IF nRow + nHeight - 1 + nShadH > nMaxRow .OR. nCol + nWidth - 1 + nShadW > nMaxCol
        RETURN 0
    ENDIF

    IF s_cDesktop == NIL
        ht_winSaveDesktop()
    ENDIF

    HT_SETVIEWPORT( 0, 0, -1, 0 )
    IF hWin[ "winImage" ] != NIL
        cImage := hWin[ "winImage" ]
    ELSEIF hWin[ "layerImage" ] != NIL
        cImage := hWin[ "layerImage" ]
    ELSE
        cImage := SaveScreen( hWin[ "top" ], hWin[ "left" ], ;
            hWin[ "bottom" ], hWin[ "right" ] )
    ENDIF
    hWin[ "winImage" ] := cImage
    hWin[ "layerImage" ] := cImage

    DispBegin()
    ht_winCompose( nId )
    hWin[ "top"    ] := nRow
    hWin[ "left"   ] := nCol
    hWin[ "bottom" ] := nRow + nHeight - 1
    hWin[ "right"  ] := nCol + nWidth - 1
    hWin[ "shadBottom" ] := hWin[ "bottom" ] + nShadH
    hWin[ "shadRight"  ] := hWin[ "right"  ] + nShadW
    RestScreen( hWin[ "top" ], hWin[ "left" ], ;
        hWin[ "bottom" ], hWin[ "right" ], cImage )
    IF nShadH > 0
        hb_Shadow( hWin[ "top" ], hWin[ "left" ], hWin[ "bottom" ], hWin[ "right" ] )
    ENDIF
    DispEnd()

    aArea := ht_winArea( hWin )
    ht_winCompose( nId )
    hWin[ "backImage" ] := SaveScreen( aArea[ 1 ], aArea[ 2 ], aArea[ 3 ], aArea[ 4 ] )
    RestScreen( hWin[ "top" ], hWin[ "left" ], ;
        hWin[ "bottom" ], hWin[ "right" ], cImage )
    IF nShadH > 0
        hb_Shadow( hWin[ "top" ], hWin[ "left" ], hWin[ "bottom" ], hWin[ "right" ] )
    ENDIF

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

/** Captures window chrome for the compose stack (winImage only; shadow is live). */
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
        hWin[ "layerImage" ] := hWin[ "winImage" ]
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
