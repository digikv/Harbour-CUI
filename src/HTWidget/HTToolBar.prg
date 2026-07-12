/** @class HTToolBar
 * Horizontal toolbar with padded text buttons (no Digi ▀/▄ shadow).
 * @extends HTWidget
 */

#include "hbtui.ch"
#include "inkey.ch"

CLASS HTToolBar FROM HTWidget

PROTECTED:

    DATA FaButtons      INIT {}     /* array of { cText, bAction, nKey } */
    DATA FnActiveButton INIT 0

PUBLIC:

    CONSTRUCTOR new( ... )

    METHOD paintEvent( event )
    METHOD keyEvent( keyEvent )
    METHOD mouseEvent( eventMouse )

    METHOD addButton( cText, bAction, nShortcutKey )
    METHOD buttonCount() INLINE Len( ::FaButtons )
    METHOD buttonWidth( nIndex )
    METHOD buttonCol( nIndex )

HIDDEN:

    METHOD recalcWidth()

ENDCLASS

/** Creates a new toolbar with optional parent widget. */
METHOD new( ... ) CLASS HTToolBar

    LOCAL p

    IF pCount() > 0
        p := hb_pValue( 1 )
        IF hb_isObject( p )
            ::super:new( p )
        ELSE
            ::super:new()
        ENDIF
    ELSE
        ::super:new()
    ENDIF

    ::setFocusPolicy( HT_FOCUS_TAB )
    ::FisVisible := .T.
    ::Fheight := 1
    ::Fwidth := 1

RETURN self

/** Face width: "  text  " (no right-shadow column). */
METHOD FUNCTION buttonWidth( nIndex ) CLASS HTToolBar
    IF nIndex < 1 .OR. nIndex > Len( ::FaButtons )
        RETURN 0
    ENDIF
RETURN Len( ::FaButtons[ nIndex ][ 1 ] ) + 4

/** Left column of button nIndex within the toolbar. */
METHOD FUNCTION buttonCol( nIndex ) CLASS HTToolBar

    LOCAL i, nCol := 0

    FOR i := 1 TO nIndex - 1
        nCol += ::buttonWidth( i ) + 1
    NEXT

RETURN nCol

/** Adds a button to the toolbar.
 * @param cText Button label text
 * @param bAction Code block invoked on activation
 * @param nShortcutKey Optional Inkey code for keyboard shortcut
 * @return 1-based button index
 */
METHOD addButton( cText, bAction, nShortcutKey ) CLASS HTToolBar

    LOCAL nIndex

    AAdd( ::FaButtons, { cText, bAction, nShortcutKey } )
    nIndex := Len( ::FaButtons )

    IF ::FnActiveButton = 0
        ::FnActiveButton := 1
    ENDIF

    ::recalcWidth()

RETURN nIndex

/** Recalculates total widget width from button faces + gaps. */
METHOD PROCEDURE recalcWidth() CLASS HTToolBar

    LOCAL i, nTotal := 0

    FOR i := 1 TO Len( ::FaButtons )
        nTotal += ::buttonWidth( i )
        IF i < Len( ::FaButtons )
            nTotal += 1
        ENDIF
    NEXT

    ::Fwidth := Max( 1, nTotal )
    ::Fheight := 1

RETURN

/** Renders flat padded toolbar buttons (no shadow).
 * @param event HTPaintEvent (unused)
 */
METHOD PROCEDURE paintEvent( event ) CLASS HTToolBar

    LOCAL i, nCol, nMaxCol, cFace, cFaceClr, cLetClr, cBg, nAt
    LOCAL lFocused := ::hasFocus()
    LOCAL cBarClr := IIF( lFocused, ;
        HTTheme():getColor( HT_CLR_TOOLBAR_FOCUSED ), ;
        HTTheme():getColor( HT_CLR_TOOLBAR_NORMAL ) )

    HB_SYMBOL_UNUSED( event )

    nMaxCol := MaxCol()
    DispOutAt( 0, 0, Space( nMaxCol + 1 ), cBarClr )

    nCol := 0
    FOR i := 1 TO Len( ::FaButtons )
        cFace := "  " + ::FaButtons[ i ][ 1 ] + "  "
        IF lFocused .AND. i = ::FnActiveButton
            cFaceClr := HTTheme():getColor( HT_CLR_BUTTON_FOCUSED )
        ELSE
            cFaceClr := HTTheme():getColor( HT_CLR_BUTTON_NORMAL )
        ENDIF
        nAt := At( "/", cFaceClr )
        cBg := IIF( nAt > 0, SubStr( cFaceClr, nAt ), "/8" )
        cLetClr := IIF( lFocused .AND. i = ::FnActiveButton, "15" + cBg, "14" + cBg )

        DispOutAt( 0, nCol, cFace, cFaceClr )
        IF ! Empty( ::FaButtons[ i ][ 1 ] )
            DispOutAt( 0, nCol + 2, Left( ::FaButtons[ i ][ 1 ], 1 ), cLetClr )
        ENDIF
        nCol += ::buttonWidth( i ) + 1
    NEXT

RETURN

/** Handles Left/Right navigation, Enter/Space activation, and shortcut keys.
 * @param keyEvent HTKeyEvent
 */
METHOD PROCEDURE keyEvent( keyEvent ) CLASS HTToolBar

    LOCAL parent
    LOCAL nOldActive := ::FnActiveButton
    LOCAL i

    SWITCH keyEvent:key
    CASE K_LEFT
        IF ::FnActiveButton > 1
            ::FnActiveButton--
        ENDIF
        EXIT
    CASE K_RIGHT
        IF ::FnActiveButton < Len( ::FaButtons )
            ::FnActiveButton++
        ENDIF
        EXIT
    CASE K_ENTER
    CASE K_SPACE
        IF ::FnActiveButton > 0 .AND. ::FnActiveButton <= Len( ::FaButtons )
            IF ::FaButtons[ ::FnActiveButton ][ 2 ] != NIL
                Eval( ::FaButtons[ ::FnActiveButton ][ 2 ] )
            ENDIF
        ENDIF
        keyEvent:accept()
        RETURN
    OTHERWISE
        FOR i := 1 TO Len( ::FaButtons )
            IF ::FaButtons[ i ][ 3 ] != NIL .AND. ::FaButtons[ i ][ 3 ] = keyEvent:key
                ::FnActiveButton := i
                IF ::FaButtons[ i ][ 2 ] != NIL
                    Eval( ::FaButtons[ i ][ 2 ] )
                ENDIF
                keyEvent:accept()
                RETURN
            ENDIF
        NEXT
        RETURN
    ENDSWITCH

    keyEvent:accept()

    IF ::FnActiveButton != nOldActive
        parent := ::parent()
        IF parent != NIL .AND. parent:isDerivedFrom( "HTWidget" )
            parent:repaintChild( self )
        ENDIF
    ENDIF

RETURN

/** Handles mouse click to activate the clicked button.
 * @param eventMouse HTMouseEvent
 */
METHOD PROCEDURE mouseEvent( eventMouse ) CLASS HTToolBar

    LOCAL nClickCol, i, nStart, nEnd
    LOCAL parent

    IF eventMouse:nKey = K_LBUTTONDOWN
        nClickCol := eventMouse:mouseCol
        IF nClickCol >= 0
            FOR i := 1 TO Len( ::FaButtons )
                nStart := ::buttonCol( i )
                nEnd   := nStart + ::buttonWidth( i ) - 1
                IF nClickCol >= nStart .AND. nClickCol <= nEnd
                    ::FnActiveButton := i
                    IF ::FaButtons[ i ][ 2 ] != NIL
                        Eval( ::FaButtons[ i ][ 2 ] )
                    ENDIF
                    parent := ::parent()
                    IF parent != NIL .AND. parent:isDerivedFrom( "HTWidget" )
                        parent:repaintChild( self )
                    ENDIF
                    RETURN
                ENDIF
            NEXT
        ENDIF
    ENDIF

RETURN
