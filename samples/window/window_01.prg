/*
 * Floating top-level windows on the desktop — drag by the title bar.
 * Demonstrates SaveScreen/RestScreen move and the window/desktop edge.
 */

#include "hbtui.ch"

PROCEDURE Main()

    LOCAL app
    LOCAL win
    LOCAL lbl
    LOCAL i

    app := HTApplication():new()
    ht_cuiSetupDisplay( app:rows(), app:cols() )

    FOR i := 1 TO 3
        /* plain HTWidget = floating window (HTMainWindow is fullscreen app shell) */
        win := HTWidget():new()
        win:shadow := HT_SHADOW_ATTR
        win:move( 4 + ( i - 1 ) * 8, 2 + ( i - 1 ) * 4 )  /* x=col, y=row */
        win:resize( 42, 12 )
        win:setWindowTitle( "Window " + hb_ntos( i ) )

        lbl := HTLabel():new( "Drag the title bar to move.", win )
        lbl:move( 2, 2 )
        lbl := HTLabel():new( "Watch the edge vs desktop.", win )
        lbl:move( 2, 4 )
        lbl := HTLabel():new( "Click [x] to close.", win )
        lbl:move( 2, 6 )

        win:show()
    NEXT

    app:exec()

RETURN
