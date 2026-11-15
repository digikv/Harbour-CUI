/** @class HTPushButton
 * Standard push button activated by Enter or Space key.
 * Flat padded face (no Digi ▀/▄ shadow — those render as solid black blocks).
 * @extends HTAbstractButton
 */

#include "hbtui.ch"
#include "inkey.ch"

CLASS HTPushButton FROM HTAbstractButton

PROTECTED:
    METHOD faceText()
PUBLIC:

    METHOD new( ... )
    METHOD paintEvent( paintEvent )
    METHOD keyEvent( keyEvent )
    METHOD mouseEvent( eventMouse )
    METHOD setText( cText )

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
    ::Fheight := 1
    ::Fwidth  := ht_strWidth( ::Ftext ) + 4

RETURN self

/** Keep width in sync when label changes. */
METHOD PROCEDURE setText( cText ) CLASS HTPushButton
    ::super:setText( cText )
    ::Fwidth := ht_strWidth( ::Ftext ) + 4
RETURN

METHOD FUNCTION faceText() CLASS HTPushButton
RETURN "  " + ::Ftext + "  "

/** Flat padded button face; focused uses BUTTON_FOCUSED color.
 * @param paintEvent HTPaintEvent (unused)
 */
METHOD PROCEDURE paintEvent( paintEvent ) CLASS HTPushButton

    LOCAL cFace
    LOCAL cColor

    HB_SYMBOL_UNUSED( paintEvent )

    cFace  := ::faceText()
    cColor := IIF( ::hasFocus(), ;
        HTTheme():getColor( HT_CLR_BUTTON_FOCUSED ), ;
        HTTheme():getColor( HT_CLR_BUTTON_NORMAL ) )

    IF Len( cFace ) > MaxCol() + 1
        cFace := Left( cFace, MaxCol() + 1 )
    ENDIF

    DispOutAt( 0, 0, cFace, cColor )

RETURN

/** Fires onClicked on mouse click.
 * @param eventMouse HTMouseEvent
 */
METHOD PROCEDURE mouseEvent( eventMouse ) CLASS HTPushButton

    IF eventMouse:nKey = K_LBUTTONDOWN
        IF ::FonClicked != NIL
            Eval( ::FonClicked )
        ENDIF
    ENDIF

RETURN

/** Fires onClicked on Enter or Space.
 * @param keyEvent HTKeyEvent
 */
METHOD PROCEDURE keyEvent( keyEvent ) CLASS HTPushButton

    IF keyEvent:key = K_ENTER .OR. keyEvent:key = K_SPACE
        IF ::FonClicked != NIL
            Eval( ::FonClicked )
        ENDIF
        keyEvent:accept()
    ENDIF

RETURN

/** Flat button paint helper for HTMessageBox (no shadow).
 * @param nRow Viewport row
 * @param nCol Viewport column
 * @param cText Label
 * @param lFocus Focused colors
 */
FUNCTION ht_paintDigiButton( nRow, nCol, cText, lFocus )

    LOCAL cFace
    LOCAL cFaceClr

    IF cText == NIL
        cText := ""
    ENDIF
    IF lFocus == NIL
        lFocus := .F.
    ENDIF

    cFace := "  " + cText + "  "
    cFaceClr := IIF( lFocus, ;
        HTTheme():getColor( HT_CLR_BUTTON_FOCUSED ), ;
        HTTheme():getColor( HT_CLR_BUTTON_NORMAL ) )

    DispOutAt( nRow, nCol, cFace, cFaceClr )

RETURN NIL
