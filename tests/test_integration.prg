/**
 * test_integration.prg - Integration tests for focus cycle, layout reparenting,
 * childAt recursion, and repaintChild delegation.
 *
 * Tests the full focus chain across flat and nested-layout widget trees
 * without needing a display (no CT window functions called).
 *
 * Runs headless (-gtnul).
 *
 * Build: hbmk2 test_integration.prg hbmk.hbm
 */

#include "hbtui.ch"
#include "inkey.ch"

STATIC nPass := 0
STATIC nFail := 0

PROCEDURE Main()

    LOCAL nTotal

    TestFlatFocus()
    TestNestedLayoutFocus()
    TestLayoutGeometry()
    TestNestedFocusCycleCoversAll()
    TestRepaintChildDelegation()
    TestMouseGrab()

    nTotal := nPass + nFail
    QOut( "" )
    QOut( "Results: " + hb_ntos( nPass ) + " passed, " + hb_ntos( nFail ) + " failed out of " + hb_ntos( nTotal ) )
    ErrorLevel( IIF( nFail > 0, 1, 0 ) )

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE Assert( cName, lCond )

    IF lCond
        nPass++
    ELSE
        nFail++
        OutErr( "FAIL: " + cName + hb_eol() )
    ENDIF

RETURN

/*----------------------------------------------------------------*/

/** Tests Tab focus cycling across 3 buttons directly parented to a window.
 *  No paintEvent called (avoids CT wOpen). focusInEvent is safe because
 *  FwindowId is NIL (no CT window), so repaintChild is a harmless no-op.
 */
STATIC PROCEDURE TestFlatFocus()

    LOCAL win, btn1, btn2, btn3

    win  := HTMainWindow():new()
    win:resize( 60, 20 )

    btn1 := HTPushButton():new( "One",   win )
    btn2 := HTPushButton():new( "Two",   win )
    btn3 := HTPushButton():new( "Three", win )

    /* auto-focus first child */
    win:focusInEvent( HTFocusEvent():new( HT_EVENT_TYPE_FOCUSIN ) )

    Assert( "Flat: focusWidget == btn1",        win:focusWidget() == btn1 )
    Assert( "Flat: btn1 is focused child",      win:focusWidget() == btn1 )

    /* Tab to btn2 */
    win:keyEvent( HTKeyEvent():new( K_TAB ) )
    Assert( "Flat: focusWidget == btn2",        win:focusWidget() == btn2 )
    Assert( "Flat: btn1 lost focus",            ! btn1:hasFocus() )

    /* Tab to btn3 */
    win:keyEvent( HTKeyEvent():new( K_TAB ) )
    Assert( "Flat: focusWidget == btn3",        win:focusWidget() == btn3 )

    /* Tab wraps to btn1 */
    win:keyEvent( HTKeyEvent():new( K_TAB ) )
    Assert( "Flat: Tab wraps to btn1",          win:focusWidget() == btn1 )
    Assert( "Flat: btn1 focused after wrap",    win:focusWidget() == btn1 )

RETURN

/*----------------------------------------------------------------*/

/** Tests Tab focus cycling when buttons live inside a nested layout
 *  (VBox -> HBox addLayout -> container reparent).
 */
STATIC PROCEDURE TestNestedLayoutFocus()

    LOCAL win, vbox, hbox, btn1, btn2, btn3
    LOCAL cClass1, cClass2, cClass3

    win  := HTMainWindow():new()
    win:resize( 60, 20 )

    vbox := HTVBoxLayout():new( win )
    hbox := HTHBoxLayout():new()

    /* create buttons with win as parent (matches real-world pattern) */
    btn1 := HTPushButton():new( "A", win )
    btn2 := HTPushButton():new( "B", win )
    btn3 := HTPushButton():new( "C", win )

    hbox:addWidget( btn1 )
    hbox:addWidget( btn2 )
    hbox:addWidget( btn3 )

    /* addLayout reparents buttons from win to an internal container */
    vbox:addLayout( hbox )

    /* after reparent, button parent should be the container (an HTWidget) */
    cClass1 := btn1:parent():className()
    cClass2 := btn2:parent():className()
    cClass3 := btn3:parent():className()
    Assert( "Nested: btn1 reparented to container", cClass1 == "HTWIDGET" )
    Assert( "Nested: btn2 reparented to container", cClass2 == "HTWIDGET" )
    Assert( "Nested: btn3 reparented to container", cClass3 == "HTWIDGET" )

    /* auto-focus first focusable child (found via recursion into container) */
    win:focusInEvent( HTFocusEvent():new( HT_EVENT_TYPE_FOCUSIN ) )

    Assert( "Nested: focusWidget == btn1", win:focusWidget() == btn1 )
    Assert( "Nested: btn1 is focused child", win:focusWidget() == btn1 )

    /* Tab to btn2 */
    win:keyEvent( HTKeyEvent():new( K_TAB ) )
    Assert( "Nested: focusWidget == btn2", win:focusWidget() == btn2 )
    Assert( "Nested: btn1 lost focus",     ! btn1:hasFocus() )

    /* Tab to btn3 */
    win:keyEvent( HTKeyEvent():new( K_TAB ) )
    Assert( "Nested: focusWidget == btn3", win:focusWidget() == btn3 )

    /* Tab wraps to btn1 */
    win:keyEvent( HTKeyEvent():new( K_TAB ) )
    Assert( "Nested: Tab wraps to btn1",   win:focusWidget() == btn1 )

RETURN

/*----------------------------------------------------------------*/

/** Tests widget positions after doLayout and nested layout geometry.
 *  Verifies vbox sets container size, then hbox sets button positions.
 */
STATIC PROCEDURE TestLayoutGeometry()

    LOCAL win, vbox, hbox, btn1, btn2
    LOCAL oContainer

    win  := HTMainWindow():new()
    win:resize( 60, 20 )

    vbox := HTVBoxLayout():new( win )
    hbox := HTHBoxLayout():new()

    btn1 := HTPushButton():new( "X", win )
    btn2 := HTPushButton():new( "Y", win )

    hbox:addWidget( btn1 )
    hbox:addWidget( btn2 )
    vbox:addLayout( hbox )

    /* vbox doLayout sets container geometry */
    vbox:doLayout( 60, 20 )

    oContainer := btn1:parent()

    /* container should have valid geometry after vbox doLayout */
    Assert( "layout: container width > 0",  oContainer:width > 0 )
    Assert( "layout: container height > 0", oContainer:height > 0 )

    /* now run hbox doLayout on the container's size to position buttons */
    hbox:doLayout( oContainer:width, oContainer:height )

    /* buttons should have distinct x positions in horizontal layout */
    Assert( "layout: btn1 width > 0",       btn1:width > 0 )
    Assert( "layout: btn2 x > btn1 x",      btn2:x > btn1:x )

RETURN

/*----------------------------------------------------------------*/

/** Tests that focus cycling via Tab finds all 3 buttons in nested layout.
 *  This indirectly verifies focusableChildren recurses into containers.
 */
STATIC PROCEDURE TestNestedFocusCycleCoversAll()

    LOCAL win, vbox, hbox, btn1, btn2, btn3

    win  := HTMainWindow():new()
    win:resize( 60, 20 )

    vbox := HTVBoxLayout():new( win )
    hbox := HTHBoxLayout():new()

    btn1 := HTPushButton():new( "P", win )
    btn2 := HTPushButton():new( "Q", win )
    btn3 := HTPushButton():new( "R", win )

    hbox:addWidget( btn1 )
    hbox:addWidget( btn2 )
    hbox:addWidget( btn3 )
    vbox:addLayout( hbox )

    /* focusInEvent auto-focuses btn1 (first focusable found recursively) */
    win:focusInEvent( HTFocusEvent():new( HT_EVENT_TYPE_FOCUSIN ) )
    Assert( "cycleAll: starts at btn1", win:focusWidget() == btn1 )

    /* Tab through all 3 and verify each is reachable */
    win:keyEvent( HTKeyEvent():new( K_TAB ) )
    Assert( "cycleAll: Tab reaches btn2", win:focusWidget() == btn2 )

    win:keyEvent( HTKeyEvent():new( K_TAB ) )
    Assert( "cycleAll: Tab reaches btn3", win:focusWidget() == btn3 )

RETURN

/*----------------------------------------------------------------*/

/** Tests that layout containers have no CT window, so repaintChild
 *  will delegate upward rather than selecting a non-existent window.
 */
STATIC PROCEDURE TestRepaintChildDelegation()

    LOCAL win, vbox, hbox, btn1
    LOCAL oContainer
    LOCAL xContainerWinId, xWinWinId

    win  := HTMainWindow():new()
    win:resize( 60, 20 )

    vbox := HTVBoxLayout():new( win )
    hbox := HTHBoxLayout():new()

    btn1 := HTPushButton():new( "Z", win )
    hbox:addWidget( btn1 )
    vbox:addLayout( hbox )

    oContainer := btn1:parent()

    /* container must have no CT window ID - it is a layout wrapper.
       windowId property walks up to parent, so for container whose parent
       is the window (also no CT window), both return NIL. */
    xContainerWinId := oContainer:windowId
    Assert( "repaintChild: container has no windowId", xContainerWinId == NIL )

    /* the window itself also has no windowId (never painted) */
    xWinWinId := win:windowId
    Assert( "repaintChild: win has no windowId (no paint)", xWinWinId == NIL )

RETURN

/*----------------------------------------------------------------*/

/* A child that wants mouse drags (HTWidget:mouseDragWanted) gets the button
   down, every move and the button up from its window - also outside it -
   in its own coordinates; the right button too. Other children get no moves. */
STATIC PROCEDURE TestMouseGrab()

    LOCAL win, oDraw, oPlain

    win := HTWidget():new()
    win:resize( 40, 12 )
    oDraw := TGrabChild():new( win )
    oDraw:move( 5, 2 )
    oDraw:resize( 4, 2 )
    oDraw:lWants := .T.
    oPlain := TGrabChild():new( win )
    oPlain:move( 20, 2 )
    oPlain:resize( 4, 2 )

    /* window coordinates: content row = mouse row - 1 (title bar) */
    WinMouse( win, K_LBUTTONDOWN, 3, 6 )
    WinMouse( win, K_MOUSEMOVE, 8, 30 )
    WinMouse( win, K_LBUTTONUP, 8, 30 )
    Assert( "mouse grab: down, move, up", Len( oDraw:aLog ) == 3 )
    Assert( "mouse grab: down in child coords", Len( oDraw:aLog ) >= 1 .AND. hb_ValToExp( oDraw:aLog[ 1 ] ) == hb_ValToExp( { K_LBUTTONDOWN, 0, 1 } ) )
    Assert( "mouse grab: move outside the child", Len( oDraw:aLog ) >= 2 .AND. hb_ValToExp( oDraw:aLog[ 2 ] ) == hb_ValToExp( { K_MOUSEMOVE, 5, 25 } ) )
    Assert( "mouse grab: up ends it", Len( oDraw:aLog ) >= 3 .AND. oDraw:aLog[ 3 ][ 1 ] == K_LBUTTONUP )

    WinMouse( win, K_MOUSEMOVE, 4, 7 )
    Assert( "mouse grab: no moves after the up", Len( oDraw:aLog ) == 3 )

    WinMouse( win, K_RBUTTONDOWN, 3, 7 )
    WinMouse( win, K_MOUSEMOVE, 4, 8 )
    WinMouse( win, K_RBUTTONUP, 4, 8 )
    Assert( "mouse grab: right button", Len( oDraw:aLog ) == 6 .AND. oDraw:aLog[ 4 ][ 1 ] == K_RBUTTONDOWN .AND. oDraw:aLog[ 6 ][ 1 ] == K_RBUTTONUP )

    oPlain:aLog := {}
    WinMouse( win, K_LBUTTONDOWN, 3, 21 )
    WinMouse( win, K_MOUSEMOVE, 3, 22 )
    WinMouse( win, K_LBUTTONUP, 3, 22 )
    Assert( "mouse grab: a plain child gets the click only", Len( oPlain:aLog ) == 1 .AND. oPlain:aLog[ 1 ][ 1 ] == K_LBUTTONDOWN )

RETURN

STATIC PROCEDURE WinMouse( win, nKey, nRow, nCol )

    LOCAL e := HTMouseEvent():new( nKey, nRow, nCol )

    e:mouseRow := nRow
    e:mouseCol := nCol
    win:mouseEvent( e )

RETURN

CLASS TGrabChild FROM HTWidget

    DATA aLog INIT {}
    DATA lWants INIT .F.

    METHOD new( parent )
    METHOD mouseDragWanted() INLINE ::lWants
    METHOD mouseEvent( e )

ENDCLASS

METHOD new( parent ) CLASS TGrabChild

    ::super:new( parent )
    ::aLog := {}
    ::setFocusPolicy( HT_FOCUS_CLICK )
    ::FisVisible := .T.

RETURN self

METHOD PROCEDURE mouseEvent( e ) CLASS TGrabChild
    AAdd( ::aLog, { e:nKey, e:mouseRow, e:mouseCol } )
RETURN
