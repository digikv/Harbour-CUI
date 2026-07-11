/** @class HTWidget
 * Core base class for all visible UI controls.
 * Provides rendering via SaveScreen windows and viewports, event dispatch, focus management,
 * keyboard/mouse handling, layout support, and window system button interactions.
 * @extends HTObject
 */

#include "hbtui.ch"
#include "inkey.ch"

#define _DESKTOP_CHAR   e"\xB1"
#define _WIDGET_CHAR    e"\x20"
#define _WIDGET_SHADOW  -1   /* popups enable HT_SHADOW_ATTR explicitly */

CLASS HTWidget FROM HTObject

PROTECTED:

    DATA Factions
    DATA FAsDesktopWidget
    DATA FbtnClosePos
    DATA FBtnHidePos
    DATA FbtnMaximizePos
    DATA FbtnResizePos
    DATA FclearA    INIT "07/15"
    DATA FclearB    INIT _DESKTOP_CHAR
    DATA Fcolor
    DATA FposDown
    DATA FposUp
    DATA Fshadow    INIT _WIDGET_SHADOW
    DATA FwindowId  /* window handle from HTWinApi */
    DATA FlPosSet   INIT .F.  /* .T. after move() — otherwise top-level windows auto-center */
    DATA FfocusWidget                       /* currently focused child (object ref or NIL) */
    DATA FhelpLineWidget                    /* HTStatusBar ref for help line display */
    DATA FonKeyBindings                     /* hash: nKey => bAction for per-window key handlers */
    DATA FwinSysBtnMove     INIT .F.
    DATA FwinSysBtnClose    INIT .F.
    DATA FwinSysBtnHide     INIT .F.
    DATA FwinSysBtnMaximize INIT .F.
    DATA FwinSysBtnResize   INIT .F.

    METHOD childAt( nRow, nCol )
    METHOD displayLayout()
    METHOD focusableChildren()
    METHOD getClearA INLINE ::FclearA
    METHOD getClearB INLINE ::FclearB
    METHOD paintChild( child )
    METHOD getColor
    METHOD getPos() INLINE HTPoint():new( ::x, ::y )
    METHOD getShadow INLINE ::Fshadow
    METHOD getWindowId()
    METHOD drawCuiChrome()
    METHOD paintTopLevelWindow()
    METHOD paintWidget()
    METHOD setClearA( clearA ) INLINE ::FclearA := clearA
    METHOD setClearB( clearB ) INLINE ::FclearB := clearB
    METHOD SetColor( color ) INLINE ::Fcolor := color
    METHOD setShadow( shadow ) INLINE ::Fshadow := shadow
    METHOD setWindowId( windowId )

PUBLIC:

    CONSTRUCTOR new( ... )

    METHOD actions()
    METHOD addAction( action )
    METHOD addEvent( event, priority )
    METHOD destroy()
    METHOD repaintChild( child )

    METHOD closeEvent( closeEvent )
    METHOD event( event )
    METHOD focusInEvent( eventFocus )
    METHOD focusOutEvent( eventFocus )
    METHOD keyEvent( keyEvent )
    METHOD mouseEvent( eventMouse )
    METHOD move( ... )
    METHOD moveEvent( moveEvent )
    METHOD paintEvent( event )
    METHOD repaint()
    METHOD resize( ... )
    METHOD resizeEvent( event )

    METHOD focusNextChild()
    METHOD focusPrevChild()
    METHOD focusWidget()
    METHOD hasFocus()
    METHOD setAsDesktopWidget
    METHOD setBackgroundColor( color )
    METHOD setFocus()
    METHOD setFocusChild( oChild ) INLINE ::FfocusWidget := oChild
    METHOD setFocusPolicy( policy )
    METHOD setGeometry( nX, nY, nW, nH ) /* synchronous position+size for layouts */
    METHOD setForegroundColor( color )
    METHOD setLayout( layout )
    METHOD setWindowFlags( type ) INLINE ::FwindowFlags := type
    METHOD setWindowTitle( title )

    METHOD show()
    METHOD showEvent( showEvent )

    METHOD windowType() INLINE hb_bitAnd( ::windowFlags, 0x000000ff )

    PROPERTY backgroundColor WRITE setBackgroundColor
    PROPERTY charWidgetClose    INIT "x"    /* CUI close */
    PROPERTY charWidgetHide     INIT NIL    /* off — set "_" if needed */
    PROPERTY charWidgetMaximize INIT NIL    /* off — set Chr(254) if needed */
    PROPERTY charWidgetResize   INIT NIL
    PROPERTY clearA READ getClearA WRITE setClearA
    PROPERTY clearB READ getClearB WRITE setClearB
    PROPERTY color READ getColor WRITE SetColor
    PROPERTY focusPolicy WRITE setFocusPolicy INIT HT_FOCUS_NONE
    PROPERTY foregroundColor WRITE setForegroundColor
    PROPERTY height INIT 0
    PROPERTY isVisible INIT .F.
    PROPERTY layout WRITE setLayout
    PROPERTY pos READ getPos()
    PROPERTY shadow READ getShadow WRITE setShadow
    PROPERTY size
    PROPERTY width INIT 0
    PROPERTY windowFlags INIT 0
    PROPERTY windowId READ getWindowId WRITE setWindowId /* only main windows have it */
    PROPERTY helpLine READWRITE                         /* context help text for this control */
    PROPERTY contextMenu READWRITE                      /* HTContextMenu instance for right-click */
    PROPERTY windowTitle INIT ""
    PROPERTY x INIT 0
    PROPERTY y INIT 0
    PROPERTY minimumWidth INIT 0
    PROPERTY minimumHeight INIT 0
    PROPERTY maximumWidth INIT 0              /* 0 = no maximum */
    PROPERTY maximumHeight INIT 0             /* 0 = no maximum */

    METHOD onKey( nKey, bAction )
    METHOD setHelpLineWidget( oStatusBar )

ENDCLASS

/** Creates a new widget.
 * @param parent Optional parent widget (HTWidget or NIL)
 * @param f Optional window flags bitmask
 */
METHOD new( ... ) CLASS HTWidget

    LOCAL version := 0
    LOCAL parent
    LOCAL f

    ::Factions := {}

    IF pCount() <= 2
        parent := hb_pValue( 1 )
        f := hb_pValue( 2 )
        IF parent == NIL .OR. hb_isObject( parent )
            version := 1
        ENDIF
    ENDIF

    SWITCH version
    CASE 1
        ::super:new( parent )
        ::setWindowFlags( f )
        EXIT
    OTHERWISE
        ::PARAM_ERROR()
    ENDSWITCH

RETURN self

/** Returns the array of HTAction objects attached to this widget.
 * @return Array of HTAction
 */
METHOD FUNCTION actions() CLASS HTWidget

RETURN ::Factions

/** Adds an action to this widget if not already present.
 * @param action HTAction instance to add
 */
METHOD PROCEDURE addAction( action ) CLASS HTWidget
    IF AScan( ::Factions, action ) = 0
        AAdd( ::Factions, action )
    ENDIF
RETURN

/** Queues an event for this widget in the application event loop.
 * @param event HTEvent instance to enqueue
 * @param priority Optional priority (HT_EVENT_PRIORITY_HIGH/NORMAL/LOW)
 */
METHOD PROCEDURE addEvent( event, priority ) CLASS HTWidget
    event:setWidget( self )
    HTApplication():queueEvent( event, priority )
RETURN

/** Destroys this widget: recursively destroys children, closes windows,
 * removes from parent, and clears focus references.
 */
METHOD PROCEDURE destroy() CLASS HTWidget

    LOCAL i
    LOCAL parent

    ht_debugLog( "destroy: " + ::className() + " windowId=" + hb_ntos( hb_defaultValue( ::FwindowId, 0 ) ) )

    /* recursively destroy children (iterate backwards since array shrinks) */
    FOR i := Len( ::Fchildren ) TO 1 STEP -1
        IF ::Fchildren[ i ]:isDerivedFrom( "HTWidget" )
            ::Fchildren[ i ]:destroy()
        ENDIF
    NEXT

    /* close window if this is a top-level window */
    IF ::FwindowId != NIL
        HTApplication():removeTopLevelWindow( ::FwindowId )
        WClose( ::FwindowId )
        ::FwindowId := NIL
    ENDIF

    /* clear parent's focus reference if it points to us */
    parent := ::parent()
    IF parent != NIL .AND. parent:isDerivedFrom( "HTWidget" )
        IF parent:focusWidget() == self
            parent:FfocusWidget := NIL
        ENDIF
        parent:removeChild( self )
    ENDIF

    ::FisVisible := .F.

RETURN

/** Handles a close event by accepting it and destroying this widget.
 * @param closeEvent HTCloseEvent instance
 */
METHOD PROCEDURE closeEvent( closeEvent ) CLASS HTWidget
    closeEvent:accept()
    ::destroy()
RETURN

/** Finds the child widget at content-area coordinates.
 * Coordinates are relative to the content area (inside border).
 * @param nRow Row position within content area
 * @param nCol Column position within content area
 * @return Child widget at position or NIL
 */
METHOD FUNCTION childAt( nRow, nCol ) CLASS HTWidget

    LOCAL child, oNested
    LOCAL nI

    /* iterate in reverse: last-added (topmost) widgets are checked first,
       so interactive controls painted over decorative frames win hit-testing */
    FOR nI := Len( ::Fchildren ) TO 1 STEP -1
        child := ::Fchildren[ nI ]
        IF child:isDerivedFrom( "HTWidget" ) .AND. child:isVisible .AND. ;
           child:width > 0 .AND. child:height > 0 .AND. ;
           nRow >= child:y .AND. nRow < child:y + child:height .AND. ;
           nCol >= child:x .AND. nCol < child:x + child:width

            /* recurse into layout containers (no CT window) to find nested widgets */
            IF child:FwindowId = NIL .AND. Len( child:Fchildren ) > 0
                oNested := child:childAt( nRow - child:y, nCol - child:x )
                IF oNested != NIL
                    RETURN oNested
                ENDIF
            ENDIF
            RETURN child
        ENDIF
    NEXT

RETURN NIL

/** Calculates layout positions and paints child widgets managed by the layout. */
METHOD PROCEDURE displayLayout() CLASS HTWidget

    LOCAL child
    LOCAL nContentWidth, nContentHeight

    IF ::Flayout = NIL
        RETURN
    ENDIF

    /* content area: top-level windows skip title row only; containers use full size */
    IF ::FwindowId != NIL
        nContentWidth  := ::Fwidth
        nContentHeight := ::Fheight - 1
    ELSE
        nContentWidth  := ::Fwidth
        nContentHeight := ::Fheight
    ENDIF

    IF nContentWidth <= 0 .OR. nContentHeight <= 0
        RETURN
    ENDIF

    /* calculate positions */
    ::Flayout:doLayout( nContentWidth, nContentHeight )

    /* paint widgets from the layout */
    FOR EACH child IN ::Flayout:widgetList()
        IF child:isDerivedFrom( "HTWidget" )
            ::paintChild( child )
        ENDIF
    NEXT

RETURN

/** Dispatches an event to the appropriate type-specific handler method.
 * Unaccepted events propagate to the parent widget.
 * @param event HTEvent instance
 * @return .T. if the event was accepted
 */
METHOD FUNCTION event( event ) CLASS HTWidget
    LOCAL parent

    SWITCH event:type
    CASE HT_EVENT_TYPE_CLOSE
        ::closeEvent( event )
        EXIT
    CASE HT_EVENT_TYPE_FOCUSIN
        ::focusInEvent( event )
        EXIT
    CASE HT_EVENT_TYPE_FOCUSOUT
        ::focusOutEvent( event )
        EXIT
    CASE HT_EVENT_TYPE_KEYBOARD
        ::keyEvent( event )
        EXIT
    CASE HT_EVENT_TYPE_MOUSE
        ::mouseEvent( event )
        EXIT
    CASE HT_EVENT_TYPE_MOVE
        ::moveEvent( event )
        EXIT
    CASE HT_EVENT_TYPE_PAINT
        ::paintEvent( event )
        EXIT
    CASE HT_EVENT_TYPE_RESIZE
        ::resizeEvent( event )
        EXIT
    CASE HT_EVENT_TYPE_SHOW
        ::showEvent( event )
        EXIT
    OTHERWISE
        RETURN ::super:event( event )
    ENDSWITCH

    parent := ::parent()

    IF ! event:isAccepted() .AND. parent != NIL
        RETURN parent:event( event )
    ENDIF

RETURN event:accept()

/** Handles focus-in: selects CT window, auto-focuses first child,
 * repaints for focus highlight, and updates help line.
 * @param eventFocus HTFocusEvent instance or NIL
 */
METHOD PROCEDURE focusInEvent( eventFocus ) CLASS HTWidget

    LOCAL aFocusable
    LOCAL parent

    IF eventFocus != NIL
        eventFocus:accept()
    ENDIF

    IF ::FwindowId != NIL
        /* select and bring window to top (Z-order) */
        wSelect( ::windowId, .T. )
    ENDIF

    /* auto-focus first focusable child if none focused */
    IF ::FfocusWidget = NIL
        aFocusable := ::focusableChildren()
        IF Len( aFocusable ) > 0
            ::FfocusWidget := aFocusable[ 1 ]
            ::FfocusWidget:focusInEvent( HTFocusEvent():new( HT_EVENT_TYPE_FOCUSIN ) )
        ENDIF
    ENDIF

    /* repaint this widget to show focus highlight */
    parent := ::parent()
    IF parent != NIL .AND. parent:isDerivedFrom( "HTWidget" )
        parent:repaintChild( self )
        /* update help line if parent has a helpLineWidget */
        IF parent:FhelpLineWidget != NIL .AND. ::FhelpLine != NIL
            parent:FhelpLineWidget:setSection( 1, ::FhelpLine )
            parent:repaintChild( parent:FhelpLineWidget )
        ENDIF
    ENDIF

RETURN

/** Handles focus-out by repainting to remove focus highlight.
 * @param eventFocus HTFocusEvent instance (unused)
 */
METHOD PROCEDURE focusOutEvent( eventFocus ) CLASS HTWidget

    LOCAL parent

    HB_SYMBOL_UNUSED( eventFocus )

    /* repaint this widget so it loses focus highlight */
    parent := ::parent()
    IF parent != NIL .AND. parent:isDerivedFrom( "HTWidget" )
        parent:repaintChild( self )
    ENDIF

RETURN

/** Returns the effective color string, inheriting from parent or theme if not set.
 * @return Color string (e.g. "07/15")
 */
METHOD FUNCTION getColor CLASS HTWidget
    LOCAL parent

    IF ::Fcolor != NIL
        RETURN ::Fcolor
    ELSE
        parent := ::parent()
        IF parent != NIL
            RETURN parent:Color
        ENDIF
    ENDIF
RETURN IIF( ::FAsDesktopWidget = .T., HTTheme():getColor( HT_CLR_DESKTOP ), HTTheme():getColor( HT_CLR_WINDOW ) )

/** Returns the CT window handle, walking up to parent if this widget has none.
 * @return Numeric CT window handle or NIL
 */
METHOD FUNCTION getWindowId() CLASS HTWidget
    LOCAL parent := ::parent()
    IF ::FwindowId = NIL .AND. parent != NIL
        RETURN parent:windowId
    ENDIF
RETURN ::FwindowId

/** Returns array of visible children that can receive focus.
 * @return Array of HTWidget with focusPolicy != HT_FOCUS_NONE
 */
METHOD FUNCTION focusableChildren() CLASS HTWidget

    LOCAL aResult := {}
    LOCAL child, nested

    FOR EACH child IN ::Fchildren
        IF child:isDerivedFrom( "HTWidget" ) .AND. child:isVisible
            IF child:focusPolicy != HT_FOCUS_NONE
                AAdd( aResult, child )
            ENDIF
            /* recurse into layout containers to find nested focusable widgets */
            IF child:FwindowId = NIL .AND. Len( child:Fchildren ) > 0
                FOR EACH nested IN child:focusableChildren()
                    AAdd( aResult, nested )
                NEXT
            ENDIF
        ENDIF
    NEXT

RETURN aResult

/** Moves focus to the next focusable child, wrapping around at the end.
 * @return .T. if focus was moved, .F. if no focusable children
 */
METHOD FUNCTION focusNextChild() CLASS HTWidget

    LOCAL aFocusable := ::focusableChildren()
    LOCAL nPos
    LOCAL nLen := Len( aFocusable )
    LOCAL oFocusOut
    LOCAL oOldWidget

    IF nLen = 0
        RETURN .F.
    ENDIF

    IF ::FfocusWidget = NIL
        /* no current focus: focus first child */
        ::FfocusWidget := aFocusable[ 1 ]
        ::FfocusWidget:focusInEvent( HTFocusEvent():new( HT_EVENT_TYPE_FOCUSIN ) )
        RETURN .T.
    ENDIF

    nPos := AScan( aFocusable, {|w| w == ::FfocusWidget } )

    IF nPos = 0
        /* current focus widget not in list: focus first */
        oOldWidget := ::FfocusWidget
        ::FfocusWidget := NIL
        oOldWidget:focusOutEvent( HTFocusEvent():new( HT_EVENT_TYPE_FOCUSOUT ) )
        ::FfocusWidget := aFocusable[ 1 ]
        ::FfocusWidget:focusInEvent( HTFocusEvent():new( HT_EVENT_TYPE_FOCUSIN ) )
        RETURN .T.
    ENDIF

    /* move to next, wrap around — clear FfocusWidget first so
       hasFocus() returns .F. during the old widget's focusOutEvent repaint */
    oFocusOut := HTFocusEvent():new( HT_EVENT_TYPE_FOCUSOUT )
    oOldWidget := ::FfocusWidget
    ::FfocusWidget := NIL
    oOldWidget:focusOutEvent( oFocusOut )
    IF ! oFocusOut:isAccepted
        /* VALID rejected — re-activate current widget and stay */
        ::FfocusWidget := oOldWidget
        oOldWidget:focusInEvent( HTFocusEvent():new( HT_EVENT_TYPE_FOCUSIN ) )
        RETURN .F.
    ENDIF
    nPos := IIF( nPos >= nLen, 1, nPos + 1 )
    ::FfocusWidget := aFocusable[ nPos ]
    ::FfocusWidget:focusInEvent( HTFocusEvent():new( HT_EVENT_TYPE_FOCUSIN ) )

RETURN .T.

/** Moves focus to the previous focusable child, wrapping around at the start.
 * @return .T. if focus was moved, .F. if no focusable children
 */
METHOD FUNCTION focusPrevChild() CLASS HTWidget

    LOCAL aFocusable := ::focusableChildren()
    LOCAL nPos
    LOCAL nLen := Len( aFocusable )
    LOCAL oFocusOut
    LOCAL oOldWidget

    IF nLen = 0
        RETURN .F.
    ENDIF

    IF ::FfocusWidget = NIL
        /* no current focus: focus last child */
        ::FfocusWidget := aFocusable[ nLen ]
        ::FfocusWidget:focusInEvent( HTFocusEvent():new( HT_EVENT_TYPE_FOCUSIN ) )
        RETURN .T.
    ENDIF

    nPos := AScan( aFocusable, {|w| w == ::FfocusWidget } )

    IF nPos = 0
        oOldWidget := ::FfocusWidget
        ::FfocusWidget := NIL
        oOldWidget:focusOutEvent( HTFocusEvent():new( HT_EVENT_TYPE_FOCUSOUT ) )
        ::FfocusWidget := aFocusable[ nLen ]
        ::FfocusWidget:focusInEvent( HTFocusEvent():new( HT_EVENT_TYPE_FOCUSIN ) )
        RETURN .T.
    ENDIF

    /* move to previous, wrap around — clear FfocusWidget first so
       hasFocus() returns .F. during the old widget's focusOutEvent repaint */
    oFocusOut := HTFocusEvent():new( HT_EVENT_TYPE_FOCUSOUT )
    oOldWidget := ::FfocusWidget
    ::FfocusWidget := NIL
    oOldWidget:focusOutEvent( oFocusOut )
    IF ! oFocusOut:isAccepted
        /* VALID rejected — re-activate current widget and stay */
        ::FfocusWidget := oOldWidget
        oOldWidget:focusInEvent( HTFocusEvent():new( HT_EVENT_TYPE_FOCUSIN ) )
        RETURN .F.
    ENDIF
    nPos := IIF( nPos <= 1, nLen, nPos - 1 )
    ::FfocusWidget := aFocusable[ nPos ]
    ::FfocusWidget:focusInEvent( HTFocusEvent():new( HT_EVENT_TYPE_FOCUSIN ) )

RETURN .T.

/** Returns .T. if this widget is the currently focused child of its parent.
 * @return Logical
 */
/** Returns the focused child widget reference (debug logging for containers). */
METHOD FUNCTION focusWidget() CLASS HTWidget

    IF ::FfocusWidget != NIL .AND. ::className() == "HTWIDGET"
        ht_debugLog( "container:focusWidget()=" + ;
            IIF( hb_isString( ::FfocusWidget:Ftext ), ::FfocusWidget:Ftext, "?" ) + ;
            " caller=" + ProcName( 1 ) + "/" + ProcName( 2 ) + "/" + ProcName( 3 ) )
    ENDIF

RETURN ::FfocusWidget

METHOD FUNCTION hasFocus() CLASS HTWidget

    LOCAL p := ::parent()

    /* find the top-level window (first ancestor with a CT window) and check
       if it has this widget as its focused child. Containers (no windowId)
       are skipped — they don't participate in the focus system. */
    DO WHILE p != NIL .AND. p:isDerivedFrom( "HTWidget" )
        IF p:FwindowId != NIL
            RETURN p:FfocusWidget == self
        ENDIF
        p := p:parent()
    ENDDO

RETURN .F.

/** Dispatches key events: tries menu bar first, then key bindings,
 * then Tab/Shift+Tab focus cycling, then the focused child widget.
 * @param keyEvent HTKeyEvent instance
 */
METHOD PROCEDURE keyEvent( keyEvent ) CLASS HTWidget

    LOCAL menuBar

    /* let the menu bar handle F10 and Alt+letter before anything else */
    menuBar := ht_objectFromId( ::FmenuBar )
    IF menuBar != NIL .AND. menuBar:handleKey( keyEvent:key )
        keyEvent:accept()
        RETURN
    ENDIF

    /* check per-window key bindings (ON KEY system) */
    IF ::FonKeyBindings != NIL .AND. hb_hHasKey( ::FonKeyBindings, keyEvent:key )
        Eval( ::FonKeyBindings[ keyEvent:key ] )
        keyEvent:accept()
        RETURN
    ENDIF

    /* Tab / Shift+Tab: cycle focus among children */
    IF keyEvent:key = K_TAB
        IF ::focusNextChild()
            keyEvent:accept()
        ENDIF
        RETURN
    ENDIF

    IF keyEvent:key = K_SH_TAB
        IF ::focusPrevChild()
            keyEvent:accept()
        ENDIF
        RETURN
    ENDIF

    /* dispatch to focused child widget */
    IF ::FfocusWidget != NIL
        ::FfocusWidget:keyEvent( keyEvent )
        RETURN
    ENDIF

RETURN

/** Handles mouse events: window system buttons (close/hide/maximize/resize/move),
 * child hit-testing with focus transfer, and right-click context menus.
 * @param eventMouse HTMouseEvent instance
 */
METHOD PROCEDURE mouseEvent( eventMouse ) CLASS HTWidget

    LOCAL oHitChild
    LOCAL oOldWidget
    LOCAL nContentRow, nContentCol
    LOCAL aPos

    SWITCH eventMouse:nKey
    CASE K_LBUTTONDOWN

        ::FposUp := NIL
        ::FposDown := HTPoint():new( eventMouse:mouseCol, eventMouse:mouseRow )

        ::FwinSysBtnClose := ::FbtnClosePos != NIL .AND. ::FposDown:y = 0 .AND. ::FposDown:x >= ::FbtnClosePos[ 1 ] .AND. ::FposDown:x <= ::FbtnClosePos[ 2 ]

        ::FwinSysBtnHide := ::FBtnHidePos != NIL .AND. ::FposDown:y = 0 .AND. ::FposDown:x >= ::FBtnHidePos[ 1 ] .AND. ::FposDown:x <= ::FBtnHidePos[ 2 ]

        ::FwinSysBtnMaximize := ::FbtnMaximizePos != NIL .AND. ::FposDown:y = 0 .AND. ::FposDown:x >= ::FbtnMaximizePos[ 1 ] .AND. ::FposDown:x <= ::FbtnMaximizePos[ 2 ]

        ::FwinSysBtnResize := ::FbtnResizePos != NIL .AND. ::FposDown:y = ( ::Fheight - 1 ) .AND. ::FposDown:x >= ::FbtnResizePos[ 1 ] .AND. ::FposDown:x <= ::FbtnResizePos[ 2 ]

        IF ::FposDown:y = 0 .AND. ! ::FwinSysBtnClose .AND. ! ::FwinSysBtnHide .AND. ! ::FwinSysBtnMaximize .AND. ! ::FwinSysBtnResize
            /* blocking Digi drag with screen-stack compose */
            IF ::FwindowId != NIL
                aPos := ht_winDrag( ::FwindowId, eventMouse:mouseAbsRow, eventMouse:mouseAbsCol )
                ::Fx := aPos[ 1 ]
                ::Fy := aPos[ 2 ]
            ENDIF
            EXIT
        ENDIF

        IF HTApplication():activeWindow() = NIL .OR. HTApplication():activeWindow():windowId != ::windowId
            ::addEvent( HTFocusEvent():new( HT_EVENT_TYPE_FOCUSIN ) )
        ENDIF

        /*
         * Child hit-testing: check if click landed on a child widget.
         * Mouse coords are relative to the window (margins=0).
         * Content area starts at row 1 (below title bar), col 0.
         */
        nContentRow := ::FposDown:y - 1
        nContentCol := ::FposDown:x

        /* menu bar: if a dropdown is open, route ALL clicks through the menu */
        IF ht_objectFromId( ::FmenuBar ) != NIL
            IF ht_objectFromId( ::FmenuBar ):isMenuOpen()
                ht_objectFromId( ::FmenuBar ):handleMouseClick( eventMouse:mouseAbsRow, eventMouse:mouseAbsCol )
                EXIT
            ENDIF
            /* check menu bar click (row 0 of content = menu bar row) */
            IF nContentRow = 0
                IF ht_objectFromId( ::FmenuBar ):handleClick( 0, nContentCol )
                    EXIT
                ENDIF
            ENDIF
        ENDIF

        IF nContentRow >= 0 .AND. nContentCol >= 0
            oHitChild := ::childAt( nContentRow, nContentCol )

            IF oHitChild != NIL
                /* focus the clicked child if it accepts click focus */
                IF oHitChild:focusPolicy = HT_FOCUS_CLICK .OR. ;
                   oHitChild:focusPolicy = HT_FOCUS_STRONG
                    IF ! oHitChild == ::FfocusWidget
                        IF ::FfocusWidget != NIL
                            /* clear FfocusWidget so hasFocus() returns .F. during repaint */
                            oOldWidget := ::FfocusWidget
                            ::FfocusWidget := NIL
                            oOldWidget:focusOutEvent( HTFocusEvent():new( HT_EVENT_TYPE_FOCUSOUT ) )
                        ENDIF
                        ::FfocusWidget := oHitChild
                        ::FfocusWidget:focusInEvent( HTFocusEvent():new( HT_EVENT_TYPE_FOCUSIN ) )
                        ::repaint()
                    ENDIF
                ENDIF

                /* dispatch mouse event to child with translated coordinates */
                eventMouse:mouseRow := nContentRow - oHitChild:y
                eventMouse:mouseCol := nContentCol - oHitChild:x
                oHitChild:mouseEvent( eventMouse )
            ENDIF
        ENDIF

        EXIT

    CASE K_LBUTTONUP

        ::FposDown := NIL
        ::FposUp := HTPoint():new( eventMouse:mouseCol, eventMouse:mouseRow )

        /* Close Event */
        IF ::FposUp:y = 0 .AND. ::FwinSysBtnClose .AND. ::FposUp:x >= ::FbtnClosePos[ 1 ] .AND. ::FposUp:x <= ::FbtnClosePos[ 2 ]
            ::addEvent( HTCloseEvent():new() )
        ENDIF

        /* Hide Event */
        IF ::FposUp:y = 0 .AND. ::FwinSysBtnHide .AND. ::FposUp:x >= ::FBtnHidePos[ 1 ] .AND. ::FposUp:x <= ::FBtnHidePos[ 2 ]
            ::addEvent( HTHideEvent():new() )
        ENDIF

        /* Maximize Event */
        IF ::FposUp:y = 0 .AND. ::FwinSysBtnMaximize .AND. ::FposUp:x >= ::FbtnMaximizePos[ 1 ] .AND. ::FposUp:x <= ::FbtnMaximizePos[ 2 ]
            ::addEvent( HTMaximizeEvent():new() )
        ENDIF

        ::FwinSysBtnMove     := .F.
        ::FwinSysBtnClose    := .F.
        ::FwinSysBtnHide     := .F.
        ::FwinSysBtnMaximize := .F.

        EXIT

    CASE K_MOUSEMOVE

        IF ::FposDown != NIL .AND. ::FwinSysBtnResize
            ::addEvent( HTResizeEvent():new() )
        ENDIF

        EXIT

    CASE K_RBUTTONDOWN

        /* right-click: show context menu on the child under cursor, or on self */
        nContentRow := eventMouse:mouseRow - 1
        nContentCol := eventMouse:mouseCol

        IF nContentRow >= 0 .AND. nContentCol >= 0
            oHitChild := ::childAt( nContentRow, nContentCol )
            IF oHitChild != NIL .AND. oHitChild:contextMenu != NIL
                oHitChild:contextMenu:popup( mRow( .T. ), mCol( .T. ) )
                ::repaint()
            ELSEIF ::FcontextMenu != NIL
                ::FcontextMenu:popup( mRow( .T. ), mCol( .T. ) )
                ::repaint()
            ENDIF
        ELSEIF ::FcontextMenu != NIL
            ::FcontextMenu:popup( mRow( .T. ), mCol( .T. ) )
            ::repaint()
        ENDIF

    ENDSWITCH

RETURN

/** Moves the widget to a new position. Accepts HTPoint or (x, y) numeric pair.
 * @param ... HTPoint or two numeric coordinates (x, y)
 */
METHOD PROCEDURE move( ... ) CLASS HTWidget
    LOCAL pos

    SWITCH PCount()
    CASE 1
        pos := HB_PValue( 1 )
        IF HB_IsObject( pos ) .AND. pos:classH = HTPoint():classH
            ::FlPosSet := .T.
            ::addEvent( HTMoveEvent():new( pos, ::pos ) )
            RETURN
        ENDIF
        EXIT
    CASE 2
        IF hb_isNumeric( HB_PValue( 1 ) ) .AND. hb_isNumeric( HB_PValue( 2 ) )
            ::FlPosSet := .T.
            ::Fx := HB_PValue( 1 )
            ::Fy := HB_PValue( 2 )
            ::addEvent( HTMoveEvent():new( HTPoint():new( ::Fx, ::Fy ), ::pos ) )
            RETURN
        ENDIF
        EXIT
    ENDSWITCH

    ::PARAM_ERROR()

RETURN

/** Handles a move event: physically moves CT window for top-level widgets,
 * or stores new position for child widgets.
 * @param moveEvent HTMoveEvent with the new position
 */
METHOD PROCEDURE moveEvent( moveEvent ) CLASS HTWidget
    LOCAL pos

    pos := moveEvent:pos

    IF ::FwindowId != NIL
        /* top-level window: physically move the window (wMove is row, col) */
        wSelect( ::FwindowId, .F. )
        wMove( pos:y, pos:x )
        ::Fy := wRow()
        ::Fx := wCol()
    ELSE
        /* child widget: just store position (x=col, y=row). */
        ::Fx := pos:x
        ::Fy := pos:y
    ENDIF

RETURN

/** Handles a paint event: creates/repaints the top-level window,
 * then paints child widgets. Captures window image for drag moves.
 * @param event HTPaintEvent instance
 */
METHOD PROCEDURE paintEvent( event ) CLASS HTWidget

    HB_SYMBOL_UNUSED( event )

    /* no parent, widget has/is a top level window */
    IF ::FwindowId != NIL .OR. ( ::parent = NIL .AND. ::FwindowId = NIL )
        ::paintTopLevelWindow()
    ENDIF

    ::paintWidget()

    IF ::FwindowId != NIL .AND. ::FwindowId > 0
        ht_winCapture( ::FwindowId )
    ENDIF

RETURN

/** Draws CUI title bar and fills the client area (no DispBox border). */
METHOD PROCEDURE drawCuiChrome() CLASS HTWidget

    LOCAL n, nRight, nTitleWidth
    LOCAL cTitle
    LOCAL cTitleColor := HTTheme():getColor( HT_CLR_TITLEBAR )
    LOCAL cBtnColor   := HTTheme():getColor( HT_CLR_TITLEBTN )

    ::FbtnClosePos    := NIL
    ::FBtnHidePos     := NIL
    ::FbtnMaximizePos := NIL
    ::FbtnResizePos   := NIL

    DispOutAt( 0, 0, Space( ::Fwidth ), cTitleColor )

    /* system buttons on the right: hide, maximize, close (Windows order) */
    nRight := ::Fwidth - 1

    IF ::charWidgetClose != NIL
        ::FbtnClosePos := { nRight, nRight }
        DispOutAt( 0, nRight, ::charWidgetClose, cBtnColor )
        nRight--
    ENDIF

    IF ::charWidgetMaximize != NIL
        ::FbtnMaximizePos := { nRight, nRight }
        DispOutAt( 0, nRight, ::charWidgetMaximize, cBtnColor )
        nRight--
    ENDIF

    IF ::charWidgetHide != NIL
        ::FBtnHidePos := { nRight, nRight }
        DispOutAt( 0, nRight, ::charWidgetHide, cBtnColor )
        nRight--
    ENDIF

    nTitleWidth := nRight + 1
    cTitle := ' ' + PadR( ::FwindowTitle, Max( 0, nTitleWidth - 1 ) )
    IF Len( cTitle ) > nTitleWidth
        cTitle := Left( cTitle, nTitleWidth )
    ENDIF
    DispOutAt( 0, 0, cTitle, cTitleColor )

    IF ::Fheight > 1
        FOR n := 1 TO ::Fheight - 1
            DispOutAt( n, 0, Space( ::Fwidth ), ::color )
        NEXT
    ENDIF

    IF ::charWidgetResize != NIL
        nRight := Len( ::charWidgetResize )
        ::FbtnResizePos := { ::Fwidth - nRight, ::Fwidth - 1 }
        DispOutAt( ::Fheight - 1, ::FbtnResizePos[ 1 ], ::charWidgetResize, ::color )
    ENDIF

RETURN

/** Creates or repaints the top-level window for a widget.
 * On first paint, creates the window with CUI chrome and system buttons.
 * On repaint, redraws title bar and clears the client area.
 */
METHOD PROCEDURE paintTopLevelWindow() CLASS HTWidget

    IF ::FwindowId = NIL
        /* first paint: create the window */

        /* use user-specified dimensions, default to 10x40 if not set */
        IF ::Fheight <= 0
            ::Fheight := 10
        ENDIF
        IF ::Fwidth <= 0
            ::Fwidth := 40
        ENDIF

        /* center on SCREEN if position was never set via move()
         * Fx = column (x), Fy = row (y). MaxRow/MaxCol are viewport-relative. */
        IF ! ::FlPosSet
            ::Fy := Int( ( ht_ScreenMaxRow() + 1 - ::Fheight ) / 2 )
            ::Fx := Int( ( ht_ScreenMaxCol() + 1 - ::Fwidth ) / 2 )
            IF ::Fy < 0
                ::Fy := 0
            ENDIF
            IF ::Fx < 0
                ::Fx := 0
            ENDIF
        ENDIF

        wSetShadow( ::Fshadow )
        /* deactivate viewport so SaveScreen in wOpen is truly absolute */
        HT_SETVIEWPORT( 0, 0, -1, 0 )
        SetClearA( ::color )
        SetClearB( _WIDGET_CHAR )
        ::setWindowId( wOpen( ::Fy, ::Fx, ::Fy + ::Fheight - 1, ::Fx + ::Fwidth - 1, .T. ) )

        ::drawCuiChrome()
        wFormat()

    ELSE
        /* repaint: select the existing window and redraw chrome */
        wSelect( ::FwindowId, .F. )
        wFormat()
        ::drawCuiChrome()
        wFormat()
    ENDIF

RETURN

/** Paints a child widget inside a wFormat viewport.
 * Sets margins so the child sees (0,0) to (MaxRow(),MaxCol()) as its world.
 * @param child HTWidget child to paint
 */
METHOD PROCEDURE paintChild( child ) CLASS HTWidget

    LOCAL nTopMargin, nLeftMargin, nBottomMargin, nRightMargin

    IF ! child:isVisible
        RETURN
    ENDIF

    IF child:width <= 0 .OR. child:height <= 0
        RETURN
    ENDIF

    /* ensure we are in the right CT window */
    IF ::FwindowId != NIL
        wSelect( ::FwindowId, .F. )
    ENDIF

    /*
     * Calculate margins for the child viewport.
     * Top-level windows (with CT window) have a 1-char border on each side,
     * so child coords are offset by +1. Container widgets (no CT window,
     * created by addLayout) have no border — coords are zero-based.
     */
    IF ::FwindowId != NIL
        nTopMargin    := 1 + child:y
        nLeftMargin   := child:x
        nBottomMargin := ::Fheight - 1 - child:y - child:height
        nRightMargin  := ::Fwidth - child:x - child:width
    ELSE
        nTopMargin    := child:y
        nLeftMargin   := child:x
        nBottomMargin := ::Fheight - child:y - child:height
        nRightMargin  := ::Fwidth  - child:x - child:width
    ENDIF

    /* clamp to valid range */
    nTopMargin    := Max( nTopMargin, 0 )
    nLeftMargin   := Max( nLeftMargin, 0 )
    nBottomMargin := Max( nBottomMargin, 0 )
    nRightMargin  := Max( nRightMargin, 0 )

    /* validate: margins must leave room for content */
    IF nTopMargin + nBottomMargin >= ::Fheight .OR. nLeftMargin + nRightMargin >= ::Fwidth
        RETURN
    ENDIF

    /* push child viewport (additive, preserves parent margins on stack) */
    ht_wFormatPush( nTopMargin, nLeftMargin, nBottomMargin, nRightMargin )

    /* child sees (0,0) to (MaxRow(), MaxCol()) as its world */
    child:paintEvent( HTPaintEvent():new() )

    /* pop child viewport, restoring parent margins */
    ht_wFormatPop()

RETURN

/** Repaints a single child without clearing the whole window.
 * Used for targeted repaints on focus change or state change.
 * @param child HTWidget child to repaint
 */
METHOD PROCEDURE repaintChild( child ) CLASS HTWidget

    IF ::FwindowId != NIL
        /* top-level window: select CT window, reset margins, paint child */
        wSelect( ::FwindowId, .F. )
        wFormat()
        ::paintChild( child )
    ELSE
        /* container widget (no CT window): delegate to parent so the full
           viewport chain is rebuilt (container → child positions are correct) */
        IF ::parent() != NIL .AND. ::parent():isDerivedFrom( "HTWidget" )
            ::parent():repaintChild( self )
        ENDIF
    ENDIF

RETURN

/** Paints all child widgets: menu bar first, then layout or direct children. */
METHOD PROCEDURE paintWidget() CLASS HTWidget

    LOCAL menuBar := ht_objectFromId( ::FmenuBar )
    LOCAL child

    IF menuBar != NIL
        menuBar:repaint()
    ENDIF

    IF ::Flayout != NIL
        ::displayLayout()
    ELSE
        FOR EACH child IN ::Fchildren
            IF ! menuBar == child .AND. child:isDerivedFrom( "HTWidget" )
                ::paintChild( child )
            ENDIF
        NEXT
    ENDIF

RETURN

/** Triggers an immediate repaint by calling paintEvent directly. */
METHOD PROCEDURE repaint() CLASS HTWidget
    ::paintEvent()
RETURN

/** Resizes the widget. Accepts HTSize or (width, height) numeric pair.
 * @param ... HTSize or two numeric values (width, height)
 */
METHOD PROCEDURE resize( ... ) CLASS HTWidget

    LOCAL eventResize

    SWITCH pCount()
    CASE 2
        eventResize := HTResizeEvent():new( HTSize():new( hb_pValue( 1 ), hb_pValue( 2 ) ), ::size )
        EXIT
    CASE 1
        eventResize := HTResizeEvent():new( hb_pValue( 1 ), ::size )
        EXIT
    ENDSWITCH
    IF eventResize != NIL
        ::addEvent( eventResize )
    ENDIF
RETURN

/** Handles a resize event by storing the new width and height.
 * @param event HTResizeEvent with the new size
 */
/** Sets position and size synchronously (no event queuing).
 * Used by layout managers that need immediate geometry changes.
 * @param nX Column position
 * @param nY Row position
 * @param nW Width
 * @param nH Height
 */
METHOD PROCEDURE setGeometry( nX, nY, nW, nH ) CLASS HTWidget
    ::Fx      := nX
    ::Fy      := nY
    ::Fwidth  := nW
    ::Fheight := nH
RETURN

METHOD PROCEDURE resizeEvent( event ) CLASS HTWidget

    LOCAL oSize

    IF event != NIL
        oSize := event:size
        IF oSize != NIL
            ::Fwidth  := oSize:width
            ::Fheight := oSize:height
        ENDIF
    ENDIF

RETURN

/** Marks this widget as the desktop background widget (singleton). */
METHOD PROCEDURE setAsDesktopWidget CLASS HTWidget

    /* just one widget can be the desktop widget.
     * NOTE: do NOT call HTApplication() here — this method is invoked from
     * HTApplication:new() while the singleton obj is still NIL (mid-construction),
     * so HTApplication() would return NIL and any message send on it would fail.
     * The HTApplication:new() caller already guards with IF ::Fdesktop = NIL. */
    IF ::FAsDesktopWidget = NIL
        ::FAsDesktopWidget := .T.
    ENDIF

RETURN

/** Sets the background color. @param color Color value */
METHOD FUNCTION setBackgroundColor( color ) CLASS HTWidget
    ::FbackgroundColor := color
RETURN ::FbackgroundColor

/** Programmatically gives focus to this top-level widget,
 * removing focus from the previously active window.
 */
METHOD PROCEDURE setFocus() CLASS HTWidget

    LOCAL activeWindow := HTApplication():activeWindow()

    IF ! activeWindow == self
        IF activeWindow != NIL
            activeWindow:focusOutEvent( NIL )
        ENDIF
        ::focusInEvent( NIL )
    ENDIF

RETURN

/** Sets the focus policy. @param policy HT_FOCUS_NONE/TAB/CLICK/STRONG */
METHOD PROCEDURE setFocusPolicy( policy ) CLASS HTWidget
    ::FfocusPolicy := policy
RETURN

/** Sets the foreground color. @param color Color value */
METHOD FUNCTION setForegroundColor( color ) CLASS HTWidget
    ::FforegroundColor := color
RETURN ::FforegroundColor

/** Sets the layout manager for this widget (only if none is set).
 * @param layout HTLayout instance
 */
METHOD PROCEDURE setLayout( layout ) CLASS HTWidget
    IF ::Flayout = NIL
        ::Flayout := layout
    ENDIF
RETURN

/** Sets the CT window handle and registers as a top-level window.
 * @param windowId Numeric CT window handle
 */
METHOD PROCEDURE setWindowId( windowId ) CLASS HTWidget
    IF ::FwindowId = NIL
        ::FwindowId := windowId
        HTApplication():addTopLevelWindow( windowId, self )
    ENDIF
RETURN

/** Sets the window title. @param title Title string */
METHOD PROCEDURE setWindowTitle( title ) CLASS HTWidget
    ::FwindowTitle := title
RETURN

/** Registers a per-window key binding.
 * @param nKey Inkey code to bind
 * @param bAction Code block to execute when the key is pressed
 */
METHOD PROCEDURE onKey( nKey, bAction ) CLASS HTWidget

    IF ::FonKeyBindings = NIL
        ::FonKeyBindings := { => }
    ENDIF

    ::FonKeyBindings[ nKey ] := bAction

RETURN

/** Sets the status bar widget used for displaying context help lines.
 * @param oStatusBar HTStatusBar instance
 */
METHOD PROCEDURE setHelpLineWidget( oStatusBar ) CLASS HTWidget
    ::FhelpLineWidget := oStatusBar
RETURN

/** Makes the widget visible and queues paint, focus, and show events
 * at LOW priority so any pending move/resize events are processed first.
 */
METHOD PROCEDURE show() CLASS HTWidget

    ::FisVisible := .T.

    /* use LOW priority so that any move/resize events queued
     * before show() are processed first */
    ::addEvent( HTPaintEvent():new(), HT_EVENT_PRIORITY_LOW )
    ::addEvent( HTFocusEvent():new( HT_EVENT_TYPE_FOCUSIN ), HT_EVENT_PRIORITY_LOW )
    ::addEvent( HTShowEvent():new(), HT_EVENT_PRIORITY_LOW )

RETURN

/** Handles a show event by accepting it. Override for custom show behavior.
 * @param showEvent HTShowEvent instance
 */
METHOD PROCEDURE showEvent( showEvent ) CLASS HTWidget
    showEvent:accept()
RETURN

/* EndClass */
