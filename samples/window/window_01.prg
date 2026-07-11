/*
 *
 */

#include "hbtui.ch"

CLASS MainWindow FROM HTMainWindow
PROTECTED:
PUBLIC:
ENDCLASS

PROCEDURE Main()
    LOCAL app
    LOCAL win
    LOCAL i

    SetMode( 40, 100 )

    app := HTApplication():new()

    FOR i := 1 TO 3
        win := MakeWindow()
        /* cascade so SaveScreen/RestScreen overlap is intentional and visible */
        win:move( ( i - 1 ) * 5, ( i - 1 ) * 3 )  /* x=col, y=row */
        win:resize( 40, 12 )
        win:setWindowTitle( " Window " + hb_ntos( i ) + " " )
        win:show()
    NEXT

    app:exec()

RETURN

STATIC FUNCTION MakeWindow()
    LOCAL w

    w := MainWindow():new()

RETURN w
