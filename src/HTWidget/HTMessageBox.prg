/** @class HTMessageBox
 * Singleton factory for modal message dialogs (information, warning, error,
 * question, or any set of buttons). All methods block until the user
 * dismisses the dialog.
 *
 * Layout: the title bar, then the message on the window color with a
 * colored badge for the kind ( i  !  ?  x ), the buttons centered under it.
 * The message may have several lines (Chr(10) / CRLF); long lines are
 * wrapped at word boundaries to fit the screen.
 *
 * Keys: Left/Right/Tab/Shift+Tab move between the buttons, a button's first
 * letter (or its &-marked letter) presses it, Enter presses the focused one,
 * Esc cancels. A mouse click presses a button.
 *
 * @example
 *   HTMessageBox():information( "Saved", "The file was saved." )
 *   IF HTMessageBox():question( "Delete", "Delete 3 records?" ) == HT_DIALOG_ACCEPTED
 *   n := HTMessageBox():choose( "Exit", "Save the changes?", { "&Save", "&Discard", "Cancel" } )
 */

#include "hbtui.ch"
#include "inkey.ch"

#define _BTN_GAP   2

SINGLETON CLASS HTMessageBox

PUBLIC:

    CONSTRUCTOR new()

    METHOD information( cTitle, cMessage )
    METHOD warning( cTitle, cMessage )
    METHOD error( cTitle, cMessage )
    METHOD question( cTitle, cMessage )
    METHOD choose( cTitle, cMessage, aButtons, nKind, nDefault )
    METHOD showDialog( cTitle, cMessage, cColor, lQuestion )

ENDCLASS

/** Creates a new instance. */
METHOD new() CLASS HTMessageBox
RETURN self

/** Shows an informational message with an OK button.
 * @return HT_DIALOG_ACCEPTED
 */
METHOD FUNCTION information( cTitle, cMessage ) CLASS HTMessageBox
    ::choose( cTitle, cMessage, { "OK" }, HT_MSG_INFO )
RETURN HT_DIALOG_ACCEPTED

/** Shows a warning with an OK button.
 * @return HT_DIALOG_ACCEPTED
 */
METHOD FUNCTION warning( cTitle, cMessage ) CLASS HTMessageBox
    ::choose( cTitle, cMessage, { "OK" }, HT_MSG_WARNING )
RETURN HT_DIALOG_ACCEPTED

/** Shows an error with an OK button.
 * @return HT_DIALOG_ACCEPTED
 */
METHOD FUNCTION error( cTitle, cMessage ) CLASS HTMessageBox
    ::choose( cTitle, cMessage, { "OK" }, HT_MSG_ERROR )
RETURN HT_DIALOG_ACCEPTED

/** Shows a question with OK and Cancel buttons.
 * @return HT_DIALOG_ACCEPTED or HT_DIALOG_REJECTED
 */
METHOD FUNCTION question( cTitle, cMessage ) CLASS HTMessageBox
RETURN IIF( ::choose( cTitle, cMessage, { "OK", "Cancel" }, HT_MSG_QUESTION ) == 1, ;
            HT_DIALOG_ACCEPTED, HT_DIALOG_REJECTED )

/** Compatibility entry of the first HTMessageBox: lQuestion = OK + Cancel.
 * The color is ignored (the kind follows lQuestion).
 * @return HT_DIALOG_ACCEPTED or HT_DIALOG_REJECTED
 */
METHOD FUNCTION showDialog( cTitle, cMessage, cColor, lQuestion ) CLASS HTMessageBox
    HB_SYMBOL_UNUSED( cColor )
    IF hb_defaultValue( lQuestion, .F. )
        RETURN ::question( cTitle, cMessage )
    ENDIF
RETURN ::information( cTitle, cMessage )

/** Shows a message with any buttons (like Alert()).
 * @param cTitle   Title bar text
 * @param cMessage Text; Chr(10) starts a new line, long lines are wrapped
 * @param aButtons Button labels, default { "OK" }; "&" marks the hot key
 * @param nKind    HT_MSG_INFO (default), _WARNING, _QUESTION, _ERROR, _NONE
 * @param nDefault Button focused at the start (default 1)
 * @return Number of the pressed button, 0 for Esc
 */
METHOD FUNCTION choose( cTitle, cMessage, aButtons, nKind, nDefault ) CLASS HTMessageBox

    LOCAL aLines, aBtn := {}, aHot := {}, aPos := {}
    LOCAL nTextW, nBtnW, nWinW, nWinH, nTop, nLeft, nWinId, nOldShadow
    LOCAL nBtnRow, nCol, nFocus, nResult, n, c, cLabel, nAmp
    LOCAL cWin, cTitleClr, cBadgeClr, cBadge, nTextCol
    LOCAL event, nKey, nOldCursor := SetCursor( 0 ), nOldWin := wSelect()
    LOCAL nMaxText := Max( 20, Min( 72, ht_ScreenMaxCol() + 1 - 16 ) )

    /* toasts would take the clicks */
    HTToast():dismissAll()

    cTitle   := hb_defaultValue( cTitle, "" )
    cMessage := hb_defaultValue( cMessage, "" )
    nKind    := hb_defaultValue( nKind, HT_MSG_INFO )
    IF ! HB_ISARRAY( aButtons ) .OR. Len( aButtons ) == 0
        aButtons := { "OK" }
    ENDIF

    /* labels without "&", hot keys: the marked letter, else the first one */
    FOR EACH cLabel IN aButtons
        nAmp := At( "&", cLabel )
        IF nAmp > 0 .AND. nAmp < Len( cLabel )
            AAdd( aHot, Upper( SubStr( cLabel, nAmp + 1, 1 ) ) )
            cLabel := Left( cLabel, nAmp - 1 ) + SubStr( cLabel, nAmp + 1 )
        ELSE
            AAdd( aHot, Upper( Left( LTrim( cLabel ), 1 ) ) )
        ENDIF
        AAdd( aBtn, cLabel )
    NEXT

    aLines := ht_msgWrap( cMessage, nMaxText )

    /* sizes: 2 cells margin, badge (3) + 2, text; buttons as wide as
       HTPushButton faces ("  OK  ", at least 8) */
    nTextW := 0
    AEval( aLines, {| x | nTextW := Max( nTextW, ht_strWidth( x ) ) } )
    nBtnW := 0
    FOR EACH cLabel IN aBtn
        nBtnW += Max( ht_strWidth( cLabel ) + 4, 8 ) + IIF( cLabel:__enumIndex() > 1, _BTN_GAP, 0 )
    NEXT
    nTextCol := IIF( nKind == HT_MSG_NONE, 3, 8 )
    nWinW := Max( nTextCol + nTextW + 3, nBtnW + 6 )
    nWinW := Max( nWinW, ht_strWidth( cTitle ) + 8 )
    nWinW := Min( nWinW, ht_ScreenMaxCol() + 1 )
    nWinH := Len( aLines ) + 5          /* title, gap, text, gap, buttons, gap */
    nWinH := Min( nWinH, ht_ScreenMaxRow() + 1 )
    nBtnRow := nWinH - 2

    nTop  := Int( ( ht_ScreenMaxRow() + 1 - nWinH ) / 2 )
    nLeft := Int( ( ht_ScreenMaxCol() + 1 - nWinW ) / 2 )

    cWin      := HTTheme():getColor( HT_CLR_WINDOW )
    cTitleClr := HTTheme():getColor( HT_CLR_TITLEBAR )
    SWITCH nKind
    CASE HT_MSG_WARNING
        cBadgeClr := HTTheme():getColor( HT_CLR_MSGBOX_WARN )
        cBadge := "!"
        EXIT
    CASE HT_MSG_QUESTION
        cBadgeClr := HTTheme():getColor( HT_CLR_MSGBOX_QUEST )
        cBadge := "?"
        EXIT
    CASE HT_MSG_ERROR
        cBadgeClr := HTTheme():getColor( HT_CLR_TOAST_ERROR )
        cBadge := ht_glyph( 0x00D7, "x" )
        EXIT
    OTHERWISE
        cBadgeClr := HTTheme():getColor( HT_CLR_MSGBOX_INFO )
        cBadge := "i"
    ENDSWITCH

    nOldShadow := wSetShadow( HT_SHADOW_ATTR )
    nWinId := wOpen( nTop, nLeft, nTop + nWinH - 1, nLeft + nWinW - 1, .F. )
    wSetShadow( nOldShadow )

    /* title bar and body */
    DispOutAt( 0, 0, " " + ht_cellPadR( cTitle, nWinW - 1 ), cTitleClr )
    FOR n := 1 TO nWinH - 1
        DispOutAt( n, 0, Space( nWinW ), cWin )
    NEXT
    IF nKind != HT_MSG_NONE
        DispOutAt( 2, 2, "  " + cBadge + "  ", cBadgeClr )
        IF Len( aLines ) > 1
            DispOutAt( 3, 2, Space( 5 ), cBadgeClr )
        ENDIF
    ENDIF
    FOR n := 1 TO Len( aLines )
        IF 1 + n < nBtnRow - 1
            DispOutAt( 1 + n, nTextCol, ht_cellLeft( aLines[ n ], nWinW - nTextCol - 1 ), cWin )
        ENDIF
    NEXT

    /* centered buttons */
    nCol := Max( 1, Int( ( nWinW - nBtnW ) / 2 ) )
    FOR EACH cLabel IN aBtn
        n := Max( ht_strWidth( cLabel ) + 4, 8 )
        AAdd( aPos, { nCol, n } )
        nCol += n + _BTN_GAP
    NEXT

    nFocus := Max( 1, Min( hb_defaultValue( nDefault, 1 ), Len( aBtn ) ) )
    ht_msgButtons( nBtnRow, aBtn, aPos, nFocus )

    nResult := -1
    DO WHILE nResult < 0
        event := HTEventLoop():poll( 0.05 )
        IF event == NIL
            LOOP
        ENDIF
        IF event:className() == "HTKEYEVENT"
            nKey := event:key
            DO CASE
            CASE nKey == K_ENTER .OR. nKey == K_SPACE
                nResult := nFocus
            CASE nKey == K_ESC
                nResult := 0
            CASE nKey == K_RIGHT .OR. nKey == K_TAB .OR. nKey == K_DOWN
                nFocus := IIF( nFocus == Len( aBtn ), 1, nFocus + 1 )
            CASE nKey == K_LEFT .OR. nKey == K_SH_TAB .OR. nKey == K_UP
                nFocus := IIF( nFocus == 1, Len( aBtn ), nFocus - 1 )
            CASE nKey == K_HOME
                nFocus := 1
            CASE nKey == K_END
                nFocus := Len( aBtn )
            OTHERWISE
                c := Upper( ht_keyText( nKey ) )
                IF ! Empty( c ) .AND. ( n := AScan( aHot, c ) ) > 0
                    nResult := n
                ENDIF
            ENDCASE
            IF nResult < 0
                wSelect( nWinId, .F. )
                ht_msgButtons( nBtnRow, aBtn, aPos, nFocus )
            ENDIF
        ELSEIF event:className() == "HTMOUSEEVENT" .AND. event:nKey == K_LBUTTONDOWN
            wSelect( nWinId, .F. )
            IF mRow() == nBtnRow
                FOR n := 1 TO Len( aPos )
                    IF mCol() >= aPos[ n ][ 1 ] .AND. mCol() < aPos[ n ][ 1 ] + aPos[ n ][ 2 ]
                        nResult := n
                        EXIT
                    ENDIF
                NEXT
            ENDIF
        ENDIF
    ENDDO

    wClose( nWinId )
    IF nOldWin > 0
        wSelect( nOldWin, .F. )
    ENDIF
    SetCursor( nOldCursor )
    HTApplication():syncCursor()

RETURN nResult

/* buttons as HTPushButton paints them, the focused one highlighted */
STATIC PROCEDURE ht_msgButtons( nRow, aBtn, aPos, nFocus )

    LOCAL n, cFace

    FOR n := 1 TO Len( aBtn )
        cFace := ht_cellPadC( aBtn[ n ], aPos[ n ][ 2 ] )
        DispOutAt( nRow, aPos[ n ][ 1 ], cFace, HTTheme():getColor( ;
            IIF( n == nFocus, HT_CLR_BUTTON_FOCUSED, HT_CLR_BUTTON_NORMAL ) ) )
    NEXT

RETURN

/* message lines: Chr(10) / CRLF breaks, words wrapped at nWidth cells */
FUNCTION ht_msgWrap( cText, nWidth )

    LOCAL aOut := {}, cPara, cLine, cWord, aWords

    cText := StrTran( StrTran( cText, Chr( 13 ) + Chr( 10 ), Chr( 10 ) ), Chr( 13 ), Chr( 10 ) )
    FOR EACH cPara IN hb_ATokens( cText, Chr( 10 ) )
        IF ht_strWidth( cPara ) <= nWidth
            AAdd( aOut, cPara )
            LOOP
        ENDIF
        cLine := ""
        aWords := hb_ATokens( cPara, " " )
        FOR EACH cWord IN aWords
            DO WHILE ht_strWidth( cWord ) > nWidth    /* a word longer than a line */
                IF ! cLine == ""
                    AAdd( aOut, cLine )
                    cLine := ""
                ENDIF
                AAdd( aOut, ht_cellLeft( cWord, nWidth ) )
                cWord := SubStr( cWord, Len( ht_cellLeft( cWord, nWidth ) ) + 1 )
            ENDDO
            IF cLine == ""
                cLine := cWord
            ELSEIF ht_strWidth( cLine ) + 1 + ht_strWidth( cWord ) <= nWidth
                cLine += " " + cWord
            ELSE
                AAdd( aOut, cLine )
                cLine := cWord
            ENDIF
        NEXT
        AAdd( aOut, cLine )
    NEXT

RETURN aOut
