/** @class HTDesktop
 * Singleton desktop background widget that fills the entire screen.
 * Creates the CUI/HBTui window system and renders the desktop pattern.
 * @extends HTWidget
 */

#include "hbtui.ch"

SINGLETON CLASS HTDesktop FROM HTWidget

PROTECTED:
PUBLIC:

    CONSTRUCTOR new()

    METHOD paintEvent( paintEvent )

ENDCLASS

/** Creates the desktop without a system glyph menu (the old Chr(254) ■
 * showed as a black square in the top-left corner).
 */
METHOD new() CLASS HTDesktop

    ::super:new( NIL, HT_DESKTOP )
    ::Fshadow := -1

RETURN self

/** Initializes the CUI/HBTui window system, paints the desktop background,
 * and registers as window 0.
 * @param paintEvent HTPaintEvent instance
 */
METHOD PROCEDURE paintEvent( paintEvent ) CLASS HTDesktop

    LOCAL nRow

    ctWInit()

    wSelect( 0 )

    paintEvent:accept()
    wBoard()
    wMode( .f., .f., .f., .f. )
    wSetShadow( ::Fshadow )
    SetClearA( ::FclearA )
    SetClearB( " " )

    /* fill with real screen size — never viewport MaxRow/MaxCol */
    ::Fwidth  := ht_ScreenMaxCol() + 1
    ::Fheight := ht_ScreenMaxRow() + 1
    FOR nRow := 0 TO ::Fheight - 1
        DispOutAt( nRow, 0, Space( ::Fwidth ), ::color )
    NEXT

    SetPos( 0, 0 )
    ::FisVisible := .T.
    ::setWindowId( 0 )

    wBoard( NIL, NIL, NIL, NIL )

    ::paintWidget()

RETURN
