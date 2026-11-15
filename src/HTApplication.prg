/** @class HTApplication
 * Singleton event loop controller. Manages the desktop, top-level windows,
 * event queue with priority levels, and the main Inkey-based event dispatch loop.
 * @extends HTObject
 */

#include "hbtui.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

/* Unicode text by default (see HTApplication:new()) */
REQUEST HB_CODEPAGE_UTF8EX

STATIC s_lDebug := .F.
STATIC s_aFormatStack := {}

SINGLETON CLASS HTApplication FROM HTObject

PROTECTED:

    DATA Fexecute INIT .F.
    DATA FeventStack        INIT { {}, {}, {} }
    DATA FeventStackLen     INIT { 0, 0, 0 }
    DATA Fdesktop           INIT NIL  /* explicit: avoids READONLY keyword clash in PROPERTY macro */
    DATA FnRows             INIT 40
    DATA FnCols             INIT 131  /* Digi/awin default — fits maximized Consolas */

PUBLIC:

    CONSTRUCTOR new()

    METHOD activateNextWindow( nStep )
    METHOD activeWindow()
    METHOD addTopLevelWindow( windowId, widget )
    METHOD removeTopLevelWindow( windowId )
    METHOD exec()
    METHOD getTopLevelWindowFromWindowId( windowId )
    METHOD queueEvent( event, priority )
    METHOD quit() INLINE ::Fexecute := .F.
    METHOD repaintAll()
    METHOD setDebug( lEnable )
    METHOD setDisplaySize( nRows, nCols )
    METHOD syncCursor()

    METHOD desktop()   INLINE ::Fdesktop  /* explicit INLINE: avoids BLOCK getter scope issue */
    METHOD rows()      INLINE ::FnRows
    METHOD cols()      INLINE ::FnCols
    PROPERTY topLevelWindows INIT { => }
    
ENDCLASS

/** Creates the application singleton and initializes the desktop.
 *
 * Codepage: when the application has not selected one (Harbour's default
 * "EN" is still active), switches the VM to "UTF8EX" so editors can hold
 * any Unicode text - Chinese/Japanese/Korean from an IME, Cyrillic, etc.
 * "EN" (CP437) has no room for them: such keys would type nothing. UTF8EX
 * keeps Len()/SubStr()/Left() character-indexed, which the editors' cursor
 * arithmetic relies on (plain "UTF8" is byte-indexed). Box frames are not
 * affected: the GT draws them through its own box codepage (CP437).
 * An application that selected a codepage (hb_cdpSelect( "HR852" ), ...)
 * before this call keeps it; to stay on "EN", select it after this call.
 *
 * Under a UTF-8 codepage the GT also switches SaveScreen() buffers to the
 * extended cell format (HB_GTI_COMPATBUFFER .F.): the Clipper format keeps
 * one byte per character, so every window restore (raise, move, close)
 * would turn Unicode text into "?".
 */
METHOD new() CLASS HTApplication

    LOCAL desktop

    IF ::Fdesktop = NIL
        IF hb_cdpSelect() == "EN"
            hb_cdpSelect( "UTF8EX" )
        ENDIF
        IF hb_cdpIsUTF8()
            hb_gtInfo( HB_GTI_COMPATBUFFER, .F. )
        ENDIF
        desktop := HTDesktop():new()
        desktop:setAsDesktopWidget()
        ::Fdesktop := desktop
    ENDIF

RETURN self

/** Sets character grid size used by ht_cuiSetupDisplay (call before :new()
 * on a fresh process, or call ht_cuiSetupDisplay() directly).
 * Digi default is 40 x 131.
 */
METHOD PROCEDURE setDisplaySize( nRows, nCols ) CLASS HTApplication
    IF nRows != NIL
        ::FnRows := nRows
    ENDIF
    IF nCols != NIL
        ::FnCols := nCols
    ENDIF
RETURN

/** Returns the currently active (focused) top-level window.
 * @return HTWidget or NIL
 */
METHOD FUNCTION activeWindow() CLASS HTApplication
RETURN ::getTopLevelWindowFromWindowId( wSelect() )

/** Activates the next (nStep = 1) or previous (nStep = -1) floating window,
 * in creation order, wrapping around: brings it to the front and makes it
 * the keyboard target. The fullscreen HTMainWindow shell is not cycled.
 * @param nStep 1 = next, -1 = previous
 * @return .T. if a window was activated
 */
METHOD FUNCTION activateNextWindow( nStep ) CLASS HTApplication

    LOCAL aIds := {}
    LOCAL nId, nPos
    LOCAL window

    FOR EACH nId IN hb_hKeys( ::FtopLevelWindows )
        window := ::FtopLevelWindows[ nId ]
        IF nId > 0 .AND. window != NIL .AND. window:isDerivedFrom( "HTWidget" ) .AND. ;
           ! window:isDerivedFrom( "HTMainWindow" ) .AND. ht_winIsShown( nId )
            AAdd( aIds, nId )
        ENDIF
    NEXT

    IF Len( aIds ) = 0
        RETURN .F.
    ENDIF

    ASort( aIds )

    nPos := AScan( aIds, wSelect() )
    IF nPos = 0
        /* active window is the shell (or none): start at either end */
        nPos := IIF( nStep > 0, 1, Len( aIds ) )
    ELSE
        nPos += nStep
        IF nPos > Len( aIds )
            nPos := 1
        ELSEIF nPos < 1
            nPos := Len( aIds )
        ENDIF
    ENDIF

    window := ::FtopLevelWindows[ aIds[ nPos ] ]
    window:raise()
    wSelect( aIds[ nPos ], .F. )

RETURN .T.

/** Registers a top-level window by its CT window handle.
 * @param windowId Numeric CT window handle
 * @param widget HTWidget instance
 */
METHOD PROCEDURE addTopLevelWindow( windowId, widget ) CLASS HTApplication

    ::FtopLevelWindows[ windowId ] := widget

RETURN

/** Unregisters a top-level window by its CT window handle.
 * @param windowId Numeric CT window handle
 */
METHOD PROCEDURE removeTopLevelWindow( windowId ) CLASS HTApplication
    IF hb_hHasKey( ::FtopLevelWindows, windowId )
        hb_hDel( ::FtopLevelWindows, windowId )
    ENDIF
RETURN

/** Starts the main event loop. Paints the desktop, then continuously polls
 * for input events and dispatches queued events by priority (HIGH > NORMAL > LOW).
 * @return 0 on normal exit
 */
METHOD FUNCTION exec() CLASS HTApplication

    LOCAL result
    LOCAL event
    LOCAL widget
    LOCAL window
    LOCAL priority

    IF !::Fexecute

        /* awin-style grid + font fit — before any window paint */
        ht_cuiSetupDisplay( ::FnRows, ::FnCols )

        //set( _SET_EVENTMASK, INKEY_ALL + HB_INKEY_RAW + HB_INKEY_EXT + HB_INKEY_GTEVENT )
#ifdef __PLATFORM__WINDOWS
        Set( _SET_EVENTMASK, INKEY_ALL )
#else
        /* gtxwc changes the grid when its window is resized/maximized and
           posts HB_K_RESIZE; GT event keys only pass Inkey() when
           HB_INKEY_GTEVENT is in the mask (HTEventLoop:poll handles them) */
        Set( _SET_EVENTMASK, INKEY_ALL + HB_INKEY_GTEVENT )
#endif
        SetBlink( .F. )

        result := 0

        /* paint desktop synchronously BEFORE processing queued events,
         * so that ctWInit() and the desktop rendering happen first,
         * before any user windows are created */
        ::Fdesktop:paintEvent( HTPaintEvent():new() )

#ifdef __PLATFORM__WINDOWS
        /* Win32/gtwvt: subclass console HWND for coalesced mouse events.
           Elsewhere (gtxwc/gttrm) HTEventLoop reads the mouse via Inkey. */
        ht_WinMouseInstall()
        RegisterHtOnEventMouseMove()
        RegisterHtOnEventMouseDown()
        RegisterHtOnEventLBDblClick()
#endif

        ::Fexecute := .T.

        WHILE ::Fexecute

            /* poll for input via the unified event loop */
            event := HTEventLoop():poll( 0.1 )

            IF event != NIL
                IF event:className() == "HTMOUSEEVENT"
                    IF event:nKey != K_MOUSEMOVE
                        /* mouse click: route to window under cursor;
                           fall back to active window if clicked window is
                           unregistered (e.g., menu dropdown CT window) */
                        window := ::getTopLevelWindowFromWindowId( ht_windowAtMousePos() )
                        IF Empty( window )
                            window := ::activeWindow()
                        ENDIF
                    ELSE
                        /* mouse move: route to active window */
                        window := ::activeWindow()
                    ENDIF
                    IF ! Empty( window )
                        /* recapture coords relative to the target window using
                           screen-absolute positions (immune to wFormat margin state) */
                        wSelect( window:windowId, .F. )
                        event:mouseRow := event:mouseAbsRow - wRow()
                        event:mouseCol := event:mouseAbsCol - wCol()
                        window:addEvent( event )
                    ENDIF
                ELSEIF event:key == K_CTRL_F6
                    /* Ctrl+F6 / Ctrl+Shift+F6: next / previous window (MDI
                       style). Alt+Tab never reaches the app — Windows uses it. */
                    ::activateNextWindow( IIF( hb_bitAnd( hb_gtInfo( HB_GTI_KBDSHIFTS ), HB_GTI_KBD_SHIFT ) != 0, -1, 1 ) )
                ELSE
                    /* keyboard events go to active window */
                    ::queueEvent( event )
                ENDIF
            ENDIF

            /* process queued events by priority
             * NOTE: aDel() + shift is O(n) per dequeue, but acceptable here —
             * HBTUI_UI_STACK_EVENT_SIZE is 128, and TUI event rates are low. */
            FOR priority := HT_EVENT_PRIORITY_HIGH TO HT_EVENT_PRIORITY_LOW
                WHILE ::FeventStackLen[ priority ] > 0

                    event := ::FeventStack[ priority, 1 ]
                    aDel( ::FeventStack[ priority ], 1 )
                    --::FeventStackLen[ priority ]

                    widget := IIF( event:widget = NIL, ::activeWindow(), event:widget )

                    IF widget != NIL
                        widget:event( event )
                    ENDIF

                ENDDO
            NEXT

            /* any paint (e.g. a status bar in another window) moves the
               hardware cursor — put it back on the focused editor */
            ::syncCursor()

        ENDDO

    ELSE

        ht_debugLog( "HTApplication:exec() called while already running — ignored" )

    ENDIF

RETURN result

/** Places the hardware cursor on the text cursor of the focused widget in the
 * active window (HTGet, HTLineEdit, HTTextEdit). No-op when the focused
 * widget has no cursor or that cell is covered by another window.
 */
METHOD PROCEDURE syncCursor() CLASS HTApplication

    LOCAL window, oFocus, aCur, aOff

    window := ::activeWindow()
    IF window == NIL
        RETURN
    ENDIF

    oFocus := window:focusWidget()
    IF oFocus == NIL
        RETURN
    ENDIF

    aCur := oFocus:cursorPos()
    IF aCur == NIL
        RETURN
    ENDIF

    aOff := oFocus:windowOffset()
    IF aOff == NIL
        RETURN
    ENDIF

    ht_winSetCursor( window:windowId, aOff[ 1 ] + aCur[ 1 ], aOff[ 2 ] + aCur[ 2 ] )

RETURN

/** Looks up a top-level widget by its CT window handle.
 * @param windowId Numeric CT window handle
 * @return HTWidget or NIL if not found
 */
METHOD FUNCTION getTopLevelWindowFromWindowId( windowId ) CLASS HTApplication

    LOCAL nPos

    nPos := hb_hPos( ::FtopLevelWindows, windowId )

    IF nPos > 0
        RETURN hb_hValueAt( ::FtopLevelWindows, nPos )
    ENDIF

RETURN NIL

/** Repaints the desktop and every top-level window in place, e.g. after
 * HTTheme():loadTheme() or setColor(): widgets take the theme colors at
 * paint time. Window positions and the stacking order stay as they are. */
METHOD PROCEDURE repaintAll() CLASS HTApplication

    LOCAL nOldWin := wSelect()
    LOCAL window, key

    DispBegin()

    IF ::Fdesktop != NIL
        wSelect( 0 )
        ::Fdesktop:paintEvent( HTPaintEvent():new() )
        ht_winCompose( 0 )   /* lay the windows back over the fresh desktop */
    ENDIF

    FOR EACH key IN hb_hKeys( ::FtopLevelWindows )
        window := ::FtopLevelWindows[ key ]
        IF key != 0 .AND. window != NIL .AND. window:isDerivedFrom( "HTWidget" )
            window:repaint()
        ENDIF
    NEXT

    DispEnd()
    wSelect( nOldWin, .F. )

RETURN

/** Enqueues an event at the given priority level. Auto-assigns priority
 * based on widget visibility if not specified.
 * @param event HTEvent instance
 * @param priority Optional HT_EVENT_PRIORITY_HIGH/NORMAL/LOW
 */
METHOD PROCEDURE queueEvent( event, priority ) CLASS HTApplication

    IF priority = NIL
        IF event:widget != NIL .AND. event:widget:isVisible
            priority := HT_EVENT_PRIORITY_NORMAL    /* default priority if not given */
        ELSE
            priority := HT_EVENT_PRIORITY_LOW    /* LOW priority if widget not visible yet */
        ENDIF
    ENDIF

    IF ::FeventStackLen[ priority ] < HBTUI_UI_STACK_EVENT_SIZE
        ++::FeventStackLen[ priority ]
        IF Len( ::FeventStack[ priority ] ) < ::FeventStackLen[ priority ]
            ASize( ::FeventStack[ priority ], ::FeventStackLen[ priority ] )
        ENDIF
        ::FeventStack[ priority, ::FeventStackLen[ priority ] ] := event
    ELSE
        ht_debugLog( "queueEvent: event stack overflow at priority " + hb_ntos( priority ) )
    ENDIF

RETURN

/** Enables or disables debug logging to stderr.
 * @param lEnable .T. to enable, .F. to disable
 */
METHOD PROCEDURE setDebug( lEnable ) CLASS HTApplication
    s_lDebug := lEnable
RETURN

/** Global debug log function. Writes to stderr when debug is enabled.
 * Enable with HTApplication():setDebug( .T. ), capture with: ./myapp 2>debug.log
 * @param cMessage Log message string
 * @return NIL
 */
FUNCTION ht_debugLog( cMessage )
    IF s_lDebug
        OutErr( "[HBTui " + Time() + "] " + cMessage + hb_eol() )
    ENDIF
RETURN NIL

/*
 * wFormat viewport stack — push/pop pattern for nested viewports.
 *
 * CT's wFormat() is additive but wFormat() no-args resets ALL margins to (0,0,0,0),
 * destroying any parent viewport. These functions provide proper push/pop semantics:
 *   ht_wFormatPush(t,l,b,r) — adds margins and saves them on a stack
 *   ht_wFormatPop()         — undoes the last push, restoring parent margins
 *
 * Used by paintChild() for nested widget viewports.
 */

/** Pushes viewport margins onto the stack and applies them (additive).
 * @param nTop Top margin to add
 * @param nLeft Left margin to add
 * @param nBottom Bottom margin to add
 * @param nRight Right margin to add
 */
FUNCTION ht_wFormatPush( nTop, nLeft, nBottom, nRight )
    AAdd( s_aFormatStack, { nTop, nLeft, nBottom, nRight } )
    wFormat( nTop, nLeft, nBottom, nRight )
RETURN NIL

/** Pops the last pushed margins, restoring the parent viewport.
 * Logs a warning on stack underflow (mismatched push/pop).
 */
FUNCTION ht_wFormatPop()

    LOCAL aMargins

    IF Len( s_aFormatStack ) > 0
        aMargins := ATail( s_aFormatStack )
        ASize( s_aFormatStack, Len( s_aFormatStack ) - 1 )
        wFormat( -aMargins[ 1 ], -aMargins[ 2 ], -aMargins[ 3 ], -aMargins[ 4 ] )
    ELSE
        ht_debugLog( "ht_wFormatPop() underflow — mismatched push/pop" )
    ENDIF

RETURN NIL
