/** @class HTPushButton
 * Standard push button activated by Enter or Space key.
 * Digi/knjigelast buttons.prg look: padded face, hot letter, ▀/▄ shadow.
 * @extends HTAbstractButton
 */

#include "hbtui.ch"
#include "inkey.ch"

#define _BTN_UPPER  Chr( 223 )   /* ▀ bottom shadow */
#define _BTN_LOWER  Chr( 220 )   /* ▄ right shadow  */

CLASS HTPushButton FROM HTAbstractButton

PROTECTED:
    METHOD faceText()
    METHOD letterCol()
    METHOD shadowColor( cFaceColor )
    METHOD pressVisual()
PUBLIC:

    METHOD new( ... )
    METHOD paintEvent( paintEvent )
    METHOD keyEvent( keyEvent )
    METHOD mouseEvent( eventMouse )

    PROPERTY autoDefault
    PROPERTY default

ENDCLASS

/** Creates a new push button. Accepts optional text and/or parent widget. */
METHOD new( ... ) CLASS HTPushButton

    LOCAL p

    IF pCount() > 0
        p := hb_pValue( 1 )
        IF hb_isObject( p )
            ::super:new( p )
        ELSE
            ::setText( p )
            ::super:new( hb_pValue( 2 ) )
        ENDIF
    ELSE
        ::super:new()
    ENDIF

    ::setFocusPolicy( HT_FOCUS_STRONG )
    ::FisVisible := .T.
    /* face "  text  " + 1 col for right ▄ ; 2 rows for ▀ shadow */
    ::Fheight := 2
    ::Fwidth  := Len( ::Ftext ) + 5

RETURN self

/** Digi ButtonNew padding: two spaces on each side. */
METHOD FUNCTION faceText() CLASS HTPushButton
RETURN "  " + ::Ftext + "  "

/** Column of the hot letter inside the face (first character of label). */
METHOD FUNCTION letterCol() CLASS HTPushButton
RETURN 2

/** Black foreground on the same background as the face (Digi shadow). */
METHOD FUNCTION shadowColor( cFaceColor ) CLASS HTPushButton

    LOCAL nAt

    IF Empty( cFaceColor )
        RETURN "0/7"
    ENDIF
    nAt := At( "/", cFaceColor )
    IF nAt > 0
        RETURN "0" + SubStr( cFaceColor, nAt )
    ENDIF

RETURN "0/7"

/** Digi Press(): briefly shift the face into the shadow slot. */
METHOD PROCEDURE pressVisual() CLASS HTPushButton

    LOCAL cFace := ::faceText()
    LOCAL nLen := Len( cFace )
    LOCAL cFaceClr := IIF( ::hasFocus(), ;
        HTTheme():getColor( HT_CLR_BUTTON_FOCUSED ), ;
        HTTheme():getColor( HT_CLR_BUTTON_NORMAL ) )
    LOCAL cLetClr

    cLetClr := IIF( ::hasFocus(), "15" + SubStr( cFaceClr, At( "/", cFaceClr ) ), ;
        "12" + SubStr( cFaceClr, At( "/", cFaceClr ) ) )

    DispBegin()
    DispOutAt( 0, 0, Space( nLen + 1 ) )
    DispOutAt( 1, 1, Space( nLen ) )
    DispOutAt( 1, 1, cFace, cFaceClr )
    IF ! Empty( ::Ftext )
        DispOutAt( 1, 1 + ::letterCol(), Left( ::Ftext, 1 ), cLetClr )
    ENDIF
    DispEnd()
    Inkey( 0.12 )
    ::paintEvent()
    Inkey( 0.06 )

RETURN

/** Renders Digi-style button: padded face, hot letter, bottom/right shadow.
 * @param paintEvent HTPaintEvent (unused)
 */
METHOD PROCEDURE paintEvent( paintEvent ) CLASS HTPushButton

    LOCAL cFace
    LOCAL nLen
    LOCAL cFaceClr
    LOCAL cLetClr
    LOCAL cShadClr
    LOCAL nAt
    LOCAL cBg

    HB_SYMBOL_UNUSED( paintEvent )

    cFace := ::faceText()
    nLen  := Len( cFace )
    cFaceClr := IIF( ::hasFocus(), ;
        HTTheme():getColor( HT_CLR_BUTTON_FOCUSED ), ;
        HTTheme():getColor( HT_CLR_BUTTON_NORMAL ) )

    /* Digi: letter R+/BG normal, W/BG when selected */
    nAt := At( "/", cFaceClr )
    cBg := IIF( nAt > 0, SubStr( cFaceClr, nAt ), "/7" )
    cLetClr := IIF( ::hasFocus(), "15" + cBg, "12" + cBg )
    cShadClr := ::shadowColor( cFaceClr )

    DispOutAt( 0, 0, cFace, cFaceClr )
    IF ! Empty( ::Ftext )
        DispOutAt( 0, ::letterCol(), Left( ::Ftext, 1 ), cLetClr )
    ENDIF

    /* right ▄ and bottom ▀▀▀ (offset +1 col like buttons.prg) */
    DispOutAt( 0, nLen, _BTN_LOWER, cShadClr )
    IF ::Fheight > 1
        DispOutAt( 1, 1, Replicate( _BTN_UPPER, nLen ), cShadClr )
    ENDIF

RETURN

/** Fires the onClicked callback on mouse click.
 * @param eventMouse HTMouseEvent
 */
METHOD PROCEDURE mouseEvent( eventMouse ) CLASS HTPushButton

    IF eventMouse:nKey = K_LBUTTONDOWN
        ::pressVisual()
        IF ::FonClicked != NIL
            Eval( ::FonClicked )
        ENDIF
    ENDIF

RETURN

/** Fires the onClicked callback on Enter or Space.
 * @param keyEvent HTKeyEvent
 */
METHOD PROCEDURE keyEvent( keyEvent ) CLASS HTPushButton

    IF keyEvent:key = K_ENTER .OR. keyEvent:key = K_SPACE
        ::pressVisual()
        IF ::FonClicked != NIL
            Eval( ::FonClicked )
        ENDIF
        keyEvent:accept()
    ENDIF

RETURN

/** Digi/knjigelast ButtonShow for ad-hoc painting (e.g. HTMessageBox).
 * @param nRow  Absolute/viewport row of the face
 * @param nCol  Absolute/viewport column of the face
 * @param cText Button label (no padding)
 * @param lFocus .T. = selected/focused colors
 */
FUNCTION ht_paintDigiButton( nRow, nCol, cText, lFocus )

    LOCAL cFace
    LOCAL nLen
    LOCAL cFaceClr
    LOCAL cLetClr
    LOCAL cShadClr
    LOCAL nAt
    LOCAL cBg

    IF cText == NIL
        cText := ""
    ENDIF
    IF lFocus == NIL
        lFocus := .F.
    ENDIF

    cFace := "  " + cText + "  "
    nLen  := Len( cFace )
    cFaceClr := IIF( lFocus, ;
        HTTheme():getColor( HT_CLR_BUTTON_FOCUSED ), ;
        HTTheme():getColor( HT_CLR_BUTTON_NORMAL ) )
    nAt := At( "/", cFaceClr )
    cBg := IIF( nAt > 0, SubStr( cFaceClr, nAt ), "/3" )
    cLetClr  := IIF( lFocus, "15" + cBg, "12" + cBg )
    cShadClr := "0" + cBg

    DispOutAt( nRow, nCol, cFace, cFaceClr )
    IF ! Empty( cText )
        DispOutAt( nRow, nCol + 2, Left( cText, 1 ), cLetClr )
    ENDIF
    DispOutAt( nRow, nCol + nLen, Chr( 220 ), cShadClr )
    DispOutAt( nRow + 1, nCol + 1, Replicate( Chr( 223 ), nLen ), cShadClr )

RETURN NIL
