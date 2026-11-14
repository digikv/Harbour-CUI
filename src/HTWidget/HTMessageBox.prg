/** @class HTMessageBox
 * Singleton factory for simple modal message dialogs (information, warning, question).
 * All methods block until the user dismisses the dialog.
 */

#include "hbtui.ch"
#include "inkey.ch"

#define HT_DIALOG_ACCEPTED  1
#define HT_DIALOG_REJECTED  0


SINGLETON CLASS HTMessageBox

PUBLIC:

    CONSTRUCTOR new()

    METHOD information( cTitle, cMessage )
    METHOD warning( cTitle, cMessage )
    METHOD question( cTitle, cMessage )
    METHOD showDialog( cTitle, cMessage, cColor, lQuestion )

ENDCLASS

/** Creates a new instance. */
METHOD new() CLASS HTMessageBox
RETURN self

/** Shows an informational message dialog with an OK button.
 * @param cTitle Dialog title
 * @param cMessage Message text
 * @return HT_DIALOG_ACCEPTED
 */
METHOD FUNCTION information( cTitle, cMessage ) CLASS HTMessageBox
RETURN ::showDialog( cTitle, cMessage, HTTheme():getColor( HT_CLR_MSGBOX_INFO ), .F. )

/** Shows a warning message dialog with an OK button.
 * @param cTitle Dialog title
 * @param cMessage Message text
 * @return HT_DIALOG_ACCEPTED
 */
METHOD FUNCTION warning( cTitle, cMessage ) CLASS HTMessageBox
RETURN ::showDialog( cTitle, cMessage, HTTheme():getColor( HT_CLR_MSGBOX_WARN ), .F. )

/** Shows a question dialog with OK and Cancel buttons.
 * @param cTitle Dialog title
 * @param cMessage Message text
 * @return HT_DIALOG_ACCEPTED or HT_DIALOG_REJECTED
 */
METHOD FUNCTION question( cTitle, cMessage ) CLASS HTMessageBox
RETURN ::showDialog( cTitle, cMessage, HTTheme():getColor( HT_CLR_MSGBOX_QUEST ), .T. )

/** Internal: creates and runs a modal message dialog.
 * CUI chrome (title bar + fill, no DispBox border) with drop shadow.
 * @param cTitle Dialog title
 * @param cMessage Message text
 * @param cColor Color string for the dialog body
 * @param lQuestion .T. to show OK+Cancel, .F. for OK only
 * @return HT_DIALOG_ACCEPTED or HT_DIALOG_REJECTED
 */
METHOD FUNCTION showDialog( cTitle, cMessage, cColor, lQuestion ) CLASS HTMessageBox

    LOCAL nMsgWidth, nWinWidth, nWinHeight
    LOCAL nTop, nLeft
    LOCAL nWinId
    LOCAL event, nResult
    LOCAL nBtnCol
    LOCAL nOldShadow
    LOCAL n
    LOCAL cTitleColor := HTTheme():getColor( HT_CLR_TITLEBAR )

    /* dismiss any active toasts so they don't intercept clicks */
    HTToast():dismissAll()

    IF cTitle = NIL
        cTitle := ""
    ENDIF
    IF cMessage = NIL
        cMessage := ""
    ENDIF

    nMsgWidth  := Max( Len( cMessage ), Len( cTitle ) + 4 )
    nMsgWidth  := Max( nMsgWidth, IIF( lQuestion, 20, 10 ) )
    nWinWidth  := nMsgWidth + 4
    nWinHeight := 5   /* title + pad + message + pad + buttons */

    /* center on screen */
    nTop  := Int( ( ht_ScreenMaxRow() + 1 - nWinHeight ) / 2 )
    nLeft := Int( ( ht_ScreenMaxCol() + 1 - nWinWidth ) / 2 )

    nOldShadow := wSetShadow( HT_SHADOW_ATTR )
    nWinId := wOpen( nTop, nLeft, nTop + nWinHeight - 1, nLeft + nWinWidth - 1, .F. )
    wSetShadow( nOldShadow )

    /* CUI chrome: title bar + client fill (no box border) */
    DispOutAt( 0, 0, Space( nWinWidth ), cTitleColor )
    DispOutAt( 0, 0, " " + PadR( cTitle, nWinWidth - 2 ), cTitleColor )
    FOR n := 1 TO nWinHeight - 1
        DispOutAt( n, 0, Space( nWinWidth ), cColor )
    NEXT

    DispOutAt( 2, 1, PadR( cMessage, nWinWidth - 2 ), cColor )

    /* flat padded buttons (same as HTPushButton) */
    IF lQuestion
        nBtnCol := Int( ( nWinWidth - 20 ) / 2 )
        ht_paintDigiButton( 4, nBtnCol, "OK", .T. )
        ht_paintDigiButton( 4, nBtnCol + 10, "Cancel", .F. )
    ELSE
        nBtnCol := Int( ( nWinWidth - 6 ) / 2 )
        ht_paintDigiButton( 4, nBtnCol, "OK", .T. )
    ENDIF

    nResult := HT_DIALOG_ACCEPTED

    /* modal loop using shared event polling */
    DO WHILE .T.
        event := HTEventLoop():poll( 0.05 )

        IF event = NIL
            LOOP
        ENDIF

        IF event:className() == "HTKEYEVENT"
            IF event:key = K_ENTER .OR. event:key = K_SPACE
                nResult := HT_DIALOG_ACCEPTED
                EXIT
            ELSEIF event:key = K_ESC
                nResult := HT_DIALOG_REJECTED
                EXIT
            ENDIF
        ELSEIF event:className() == "HTMOUSEEVENT" .AND. event:nKey = K_LBUTTONDOWN
            wSelect( nWinId, .T. )
            IF mRow() = 4
                IF lQuestion
                    IF mCol() >= nBtnCol .AND. mCol() < nBtnCol + 6
                        nResult := HT_DIALOG_ACCEPTED
                        EXIT
                    ELSEIF mCol() >= nBtnCol + 10 .AND. mCol() < nBtnCol + 20
                        nResult := HT_DIALOG_REJECTED
                        EXIT
                    ENDIF
                ELSE
                    IF mCol() >= nBtnCol .AND. mCol() < nBtnCol + 6
                        nResult := HT_DIALOG_ACCEPTED
                        EXIT
                    ENDIF
                ENDIF
            ENDIF
        ENDIF
    ENDDO

    wClose( nWinId )

RETURN nResult
