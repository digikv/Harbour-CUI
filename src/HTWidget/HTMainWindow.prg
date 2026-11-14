/** @class HTMainWindow
 * Top-level application window with an optional menu bar.
 * Fills the entire GT screen (Digi/awin style) — not a floating dialog on the desktop.
 * @extends HTWidget
 */

#include "hbtui.ch"
#include "hbgtinfo.ch"

CLASS HTMainWindow FROM HTWidget

PROTECTED:

PUBLIC:

    CONSTRUCTOR new( ... )
    METHOD closeEvent( closeEvent )
    METHOD menuBar()
    /* fullscreen app shell: stays beneath floating windows, never raised */
    METHOD raise() INLINE NIL
    METHOD show()
    METHOD fitToScreen()

ENDCLASS

/** Creates a main window; no drop shadow; will fill the screen on show(). */
METHOD new( ... ) CLASS HTMainWindow
    ::super:new( ... )
    ::Fshadow := -1
    ::FlPosSet := .T.
    ::Fx := 0
    ::Fy := 0
    /* fullscreen app shell — no floating chrome controls */
    ::FcharWidgetResize   := NIL
    ::FcharWidgetMaximize := NIL
    ::FcharWidgetHide     := NIL
RETURN self

/** Closing the main window ends the application event loop.
 * @param closeEvent HTCloseEvent instance
 */
METHOD PROCEDURE closeEvent( closeEvent ) CLASS HTMainWindow
    ::super:closeEvent( closeEvent )
    HTApplication():quit()
RETURN

/** Sizes this window to the full character grid (0,0)..(MaxCol,MaxRow). */
METHOD PROCEDURE fitToScreen() CLASS HTMainWindow
    ::FlPosSet := .T.
    ::Fx := 0
    ::Fy := 0
    ::Fwidth  := ht_ScreenMaxCol() + 1
    ::Fheight := ht_ScreenMaxRow() + 1
    IF ::Fwidth < 40
        ::Fwidth := 40
    ENDIF
    IF ::Fheight < 20
        ::Fheight := 20
    ENDIF
RETURN

/** Returns the menu bar, creating one if it does not exist.
 * @return HTMenuBar instance
 */
METHOD FUNCTION menuBar() CLASS HTMainWindow
    IF ::FmenuBar = NIL
        HTMenuBar():new( self )
    ENDIF
RETURN ht_objectFromId( ::FmenuBar )

/** Ensures display grid is set, fits to full screen, then shows. */
METHOD PROCEDURE show() CLASS HTMainWindow
    LOCAL app := HTApplication()

    IF app != NIL
        ht_cuiSetupDisplay( app:rows(), app:cols() )
    ELSE
        ht_cuiSetupDisplay()
    ENDIF

    ::fitToScreen()
    IF ! Empty( ::FwindowTitle )
        hb_gtInfo( HB_GTI_WINTITLE, AllTrim( ::FwindowTitle ) )
    ENDIF
    ::super:show()
RETURN
