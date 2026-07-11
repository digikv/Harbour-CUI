/** @class HTToast
 * Non-blocking toast notification widget (singleton).
 *
 * Shows a floating message that auto-dismisses after a configurable duration.
 * Multiple toasts stack vertically from the bottom-right corner.
 * Does not steal focus or block user interaction.
 *
 * @example
 *   HTToast():show( "Record saved!" )
 *   HTToast():show( "Error occurred", 5000, HT_CLR_TOAST_ERROR )
 */

#include "hbtui.ch"

SINGLETON CLASS HTToast

PROTECTED:

    DATA FaToasts INIT {}   /* { { nWinId, nCreatedSec, nDurationSec }, ... } */

PUBLIC:

    CONSTRUCTOR new()

    METHOD show( cMessage, nDurationMs, nColorCategory )
    METHOD checkExpired()
    METHOD dismissAll()

ENDCLASS

/** Creates the singleton instance. */
METHOD new() CLASS HTToast
RETURN self

/** Shows a toast notification at the bottom-right of the screen.
 * Flat panel (no DispBox border) with drop shadow.
 * @param cMessage Text to display
 * @param nDurationMs Duration in milliseconds (default 3000)
 * @param nColorCategory HT_CLR_TOAST_* constant (default HT_CLR_TOAST_INFO)
 */
METHOD PROCEDURE show( cMessage, nDurationMs, nColorCategory ) CLASS HTToast

    LOCAL nWinId, nPrevWin
    LOCAL nToastWidth, nToastHeight
    LOCAL nTop, nLeft, nBottom, nRight
    LOCAL nScreenRows, nScreenCols
    LOCAL cColor
    LOCAL nOldShadow
    LOCAL n

    DEFAULT nDurationMs    := 3000
    DEFAULT nColorCategory := HT_CLR_TOAST_INFO

    cColor := HTTheme():getColor( nColorCategory )

    /* flat toast: padding row + text row + padding row, no box border */
    nToastHeight := 3
    nToastWidth  := Min( Len( cMessage ) + 4, 60 )

    /* get screen dimensions from desktop window */
    nPrevWin := wSelect()
    wSelect( 0 )
    nScreenRows := ht_ScreenMaxRow()
    nScreenCols := ht_ScreenMaxCol()

    /* position: bottom-right, stacked upward for multiple toasts */
    nBottom := nScreenRows - 1 - ( Len( ::FaToasts ) * ( nToastHeight + 1 ) )
    nRight  := nScreenCols - 1
    nTop    := nBottom - nToastHeight + 1
    nLeft   := nRight - nToastWidth + 1

    /* clamp to screen */
    IF nTop < 1
        nTop := 1
        nBottom := nTop + nToastHeight - 1
    ENDIF

    nOldShadow := wSetShadow( HT_SHADOW_ATTR )
    nWinId := wOpen( nTop, nLeft, nBottom, nRight, .F. )
    wSetShadow( nOldShadow )

    FOR n := 0 TO nToastHeight - 1
        DispOutAt( n, 0, Space( nToastWidth ), cColor )
    NEXT
    DispOutAt( 1, 1, PadR( cMessage, nToastWidth - 2 ), cColor )

    /* store toast info */
    AAdd( ::FaToasts, { nWinId, Seconds(), nDurationMs / 1000 } )

    /* restore previous window selection */
    IF nPrevWin > 0
        wSelect( nPrevWin, .F. )
    ENDIF

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
