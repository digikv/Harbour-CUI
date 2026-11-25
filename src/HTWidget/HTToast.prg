/** @class HTToast
 * Non-blocking toast notification widget (singleton).
 *
 * Shows a floating message that auto-dismisses after a configurable duration.
 * Multiple toasts stack vertically from the chosen corner or edge.
 * Does not steal focus or block user interaction.
 *
 * Where it sits: the fourth argument of show(), or setPlace() for every
 * toast after that. HT_TOAST_TOP_LEFT, HT_TOAST_TOP, HT_TOAST_TOP_RIGHT,
 * HT_TOAST_LEFT, HT_TOAST_CENTER, HT_TOAST_RIGHT, HT_TOAST_BOTTOM_LEFT,
 * HT_TOAST_BOTTOM, HT_TOAST_BOTTOM_RIGHT (the default).
 *
 * @example
 *   HTToast():show( "Record saved!" )
 *   HTToast():show( "Error occurred", 5000, HT_CLR_TOAST_ERROR )
 *   HTToast():show( "Please wait", 120000, HT_CLR_TOAST_INFO, HT_TOAST_CENTER )
 */

#include "hbtui.ch"

SINGLETON CLASS HTToast

PROTECTED:

    DATA FaToasts INIT {}   /* { { nWinId, nCreatedSec, nDurationSec, nPlace }, ... } */
    DATA nPlace INIT HT_TOAST_BOTTOM_RIGHT

PUBLIC:

    CONSTRUCTOR new()

    METHOD show( cMessage, nDurationMs, nColorCategory, nPlace )
    METHOD setPlace( nPlace )
    METHOD close( nWinId )
    METHOD checkExpired()
    METHOD dismissAll()

ENDCLASS

/** Creates the singleton instance. */
METHOD new() CLASS HTToast
RETURN self

/** Shows a toast. nPlace is one HT_TOAST_* constant; NIL keeps setPlace().
 * Flat panel (no DispBox border), without a shadow.
 * @param cMessage Text to display
 * @param nDurationMs Duration in milliseconds (default 3000)
 * @param nColorCategory HT_CLR_TOAST_* constant (default HT_CLR_TOAST_INFO)
 * @param nPlace HT_TOAST_* constant (default the place from setPlace())
 */
METHOD FUNCTION show( cMessage, nDurationMs, nColorCategory, nPlace ) CLASS HTToast

    LOCAL nWinId, nPrevWin
    LOCAL nToastWidth, nToastHeight
    LOCAL nTop, nLeft, nBottom, nRight
    LOCAL nScreenRows, nScreenCols
    LOCAL cColor
    LOCAL nOldShadow
    LOCAL n, nStack, aAt

    DEFAULT nDurationMs    := 3000
    DEFAULT nColorCategory := HT_CLR_TOAST_INFO
    IF ! HB_ISNUMERIC( nPlace )
        nPlace := ::nPlace
    ENDIF
    IF nPlace < HT_TOAST_TOP_LEFT .OR. nPlace > HT_TOAST_BOTTOM_RIGHT
        nPlace := HT_TOAST_BOTTOM_RIGHT
    ENDIF

    cColor := HTTheme():getColor( nColorCategory )

    /* get screen dimensions from desktop window */
    nPrevWin := wSelect()
    wSelect( 0 )
    nScreenRows := ht_ScreenMaxRow()
    nScreenCols := ht_ScreenMaxCol()

    /* flat toast: padding row + text row + padding row, no box border */
    nToastHeight := 3
    nToastWidth  := Min( ht_strWidth( cMessage ) + 4, Max( 20, nScreenCols - 4 ) )

    /* same place stacks; a toast in another place does not push this one */
    nStack := 0
    FOR n := 1 TO Len( ::FaToasts )
        IF Len( ::FaToasts[ n ] ) >= 4 .AND. ::FaToasts[ n ][ 4 ] == nPlace
            nStack++
        ENDIF
    NEXT
    aAt := ht_toastAt( nPlace, nScreenRows, nScreenCols, nToastWidth, nToastHeight, nStack )
    nTop  := aAt[ 1 ]
    nLeft := aAt[ 2 ]
    nBottom := nTop + nToastHeight - 1
    nRight  := nLeft + nToastWidth - 1

    nOldShadow := wSetShadow( -1 )
    nWinId := wOpen( nTop, nLeft, nBottom, nRight, .F. )
    wSetShadow( nOldShadow )

    FOR n := 0 TO nToastHeight - 1
        DispOutAt( n, 0, Space( nToastWidth ), cColor )
    NEXT
    DispOutAt( 1, 1, ht_cellPadR( cMessage, nToastWidth - 2 ), cColor )

    /* store toast info */
    AAdd( ::FaToasts, { nWinId, Seconds(), nDurationMs / 1000, nPlace } )

    /* restore previous window selection */
    IF nPrevWin > 0
        wSelect( nPrevWin, .F. )
    ENDIF

RETURN nWinId

/** Where every following show() sits, when that call does not pass a place. */
METHOD FUNCTION setPlace( nPlace ) CLASS HTToast

    IF HB_ISNUMERIC( nPlace ) .AND. nPlace >= HT_TOAST_TOP_LEFT .AND. nPlace <= HT_TOAST_BOTTOM_RIGHT
        ::nPlace := nPlace
    ENDIF

RETURN self

/** Closes the toast show() returned. A wait message stays until this. */
METHOD PROCEDURE close( nWinId ) CLASS HTToast

    LOCAL i

    IF ! HB_ISNUMERIC( nWinId )
        RETURN
    ENDIF
    FOR i := Len( ::FaToasts ) TO 1 STEP -1
        IF ::FaToasts[ i ][ 1 ] == nWinId
            wClose( nWinId )
            hb_ADel( ::FaToasts, i, .T. )
            RETURN
        ENDIF
    NEXT

RETURN

/** Checks for expired toasts and closes their CT windows.
 * Called from HTApplication:exec() on each event loop iteration.
 */
METHOD PROCEDURE checkExpired() CLASS HTToast

    LOCAL i, nNow

    IF Len( ::FaToasts ) = 0
        RETURN
    ENDIF

    nNow := Seconds()

    FOR i := Len( ::FaToasts ) TO 1 STEP -1
        IF nNow - ::FaToasts[ i ][ 2 ] >= ::FaToasts[ i ][ 3 ]
            wClose( ::FaToasts[ i ][ 1 ] )
            ADel( ::FaToasts, i )
            ASize( ::FaToasts, Len( ::FaToasts ) - 1 )
        ENDIF
    NEXT

RETURN

/** Immediately closes all active toast windows. */
METHOD PROCEDURE dismissAll() CLASS HTToast

    LOCAL i

    FOR i := 1 TO Len( ::FaToasts )
        wClose( ::FaToasts[ i ][ 1 ] )
    NEXT
    ::FaToasts := {}

RETURN

/* Top-left of the toast. A bottom place grows upward, the others downward.
   One row and one column stay clear of the screen edge, as the old
   bottom-right toast did. */
STATIC FUNCTION ht_toastAt( nPlace, nRows, nCols, nW, nH, nStack )

    LOCAL nTop, nLeft, nFromBottom := .F.

    DO CASE
    CASE nPlace == HT_TOAST_TOP_LEFT
        nTop := 1
        nLeft := 1
    CASE nPlace == HT_TOAST_TOP
        nTop := 1
        nLeft := Int( ( nCols + 1 - nW ) / 2 )
    CASE nPlace == HT_TOAST_TOP_RIGHT
        nTop := 1
        nLeft := nCols - nW
    CASE nPlace == HT_TOAST_LEFT
        nTop := Int( ( nRows + 1 - nH ) / 2 )
        nLeft := 1
    CASE nPlace == HT_TOAST_CENTER
        nTop := Int( ( nRows + 1 - nH ) / 2 )
        nLeft := Int( ( nCols + 1 - nW ) / 2 )
    CASE nPlace == HT_TOAST_RIGHT
        nTop := Int( ( nRows + 1 - nH ) / 2 )
        nLeft := nCols - nW
    CASE nPlace == HT_TOAST_BOTTOM_LEFT
        nFromBottom := .T.
        nTop := nRows - nH
        nLeft := 1
    CASE nPlace == HT_TOAST_BOTTOM
        nFromBottom := .T.
        nTop := nRows - nH
        nLeft := Int( ( nCols + 1 - nW ) / 2 )
    OTHERWISE
        nFromBottom := .T.
        nTop := nRows - nH
        nLeft := nCols - nW
    ENDCASE

    IF nFromBottom
        nTop -= nStack * ( nH + 1 )
    ELSE
        nTop += nStack * ( nH + 1 )
    ENDIF
    IF nTop < 0
        nTop := 0
    ENDIF
    IF nTop + nH - 1 > nRows
        nTop := Max( 0, nRows - nH + 1 )
    ENDIF
    IF nLeft < 0
        nLeft := 0
    ENDIF
    IF nLeft + nW - 1 > nCols
        nLeft := Max( 0, nCols - nW + 1 )
    ENDIF

RETURN { nTop, nLeft }
