/*
 * Floating top-level windows on the desktop.
 * Title bar: drag to move, ─ = hide, □ = maximize (▫ = restore), × = close.
 * Bottom-right ┘ = resize. (ASCII _ # x / on a codepage without them.)
 *
 *   Window 1 "Dialog"    - form with two GETs (name, amount) + OK / Cancel
 *   Window 2 "Products"  - HTBrowse over an array
 *   Window 3 "Customers" - HTBrowse over an array
 *
 * Browses sit in an HTVBoxLayout, so they follow the window size on resize.
 * Geometry (top/left/height/width) is shown on the main window status bar.
 */

#include "hbtui.ch"

STATIC s_oMainStatus

/* GET variables for the dialog window */
STATIC s_cName
STATIC s_nAmount

CLASS DemoWindow FROM HTWidget

PROTECTED:
    DATA FaLastGeom INIT NIL   /* { x, y, w, h } last shown in the status bar */

PUBLIC:
    METHOD new()
    METHOD mouseEvent( eventMouse )
    METHOD hideEvent( hideEvent )
    METHOD maximizeEvent( maximizeEvent )
    METHOD reportGeom()

ENDCLASS

METHOD new() CLASS DemoWindow
    ::super:new()
    ::shadow := HT_SHADOW_ATTR
RETURN self

METHOD PROCEDURE mouseEvent( eventMouse ) CLASS DemoWindow
    /* baseline before the first event, so a plain click reports nothing */
    IF ::FaLastGeom == NIL
        ::FaLastGeom := { ::Fx, ::Fy, ::Fwidth, ::Fheight }
    ENDIF
    ::super:mouseEvent( eventMouse )
    ::reportGeom()
RETURN

METHOD PROCEDURE hideEvent( hideEvent ) CLASS DemoWindow
    ::super:hideEvent( hideEvent )
    ::reportGeom()
RETURN

METHOD PROCEDURE maximizeEvent( maximizeEvent ) CLASS DemoWindow
    ::super:maximizeEvent( maximizeEvent )
    ::reportGeom()
RETURN

/** Push this window's geometry to the main-window status bar, only when it
 * changed (move, resize, maximize, minimize). Reporting on every mouse
 * event would overwrite messages such as the OK button's right away. */
METHOD PROCEDURE reportGeom() CLASS DemoWindow

    LOCAL aGeom := { ::Fx, ::Fy, ::Fwidth, ::Fheight }

    IF ::FaLastGeom != NIL .AND. ;
       aGeom[ 1 ] == ::FaLastGeom[ 1 ] .AND. aGeom[ 2 ] == ::FaLastGeom[ 2 ] .AND. ;
       aGeom[ 3 ] == ::FaLastGeom[ 3 ] .AND. aGeom[ 4 ] == ::FaLastGeom[ 4 ]
        RETURN
    ENDIF
    ::FaLastGeom := aGeom

    ShowStatus( AllTrim( ::FwindowTitle ) + ;
        "  top=" + hb_ntos( ::Fy ) + ;
        "  left=" + hb_ntos( ::Fx ) + ;
        "  height=" + hb_ntos( ::Fheight ) + ;
        "  width=" + hb_ntos( ::Fwidth ) )

RETURN

PROCEDURE Main()

    LOCAL app
    LOCAL main
    LOCAL win

    s_cName   := Space( 25 )
    s_nAmount := 0

    app := HTApplication():new()
    ht_cuiSetupDisplay( app:rows(), app:cols() )

    main := HTMainWindow():new()
    main:setWindowTitle( "Window Demo" )
    main:show()

    /* same symbols as HTWidget's title bar (ASCII on a codepage without them) */
    HTLabel():new( "Drag title = move   " + ;
        ht_glyph( 0x2518, "/" ) + " = resize   " + ;
        ht_glyph( 0x25A1, "#" ) + " = maximize   " + ;
        ht_glyph( 0x2500, "_" ) + " = minimize   " + ;
        ht_glyph( 0x00D7, "x" ) + " = close" + ;
        "   Ctrl+F6 = next window   F2 = edit cell", main ):move( 2, 1 )

    s_oMainStatus := HTStatusBar():new( main )
    s_oMainStatus:setGeometry( 0, Max( 0, main:height - 2 ), main:width, 1 )
    s_oMainStatus:addSection( "Move or resize a floating window" )
    main:repaint()

    /* --- Window 1: dialog with two GETs --- */
    win := DemoWindow():new()
    win:move( 4, 4 )   /* x=col, y=row */
    win:resize( 46, 11 )
    win:setWindowTitle( "Dialog" )
    BuildGetDialog( win )
    win:show()

    /* --- Window 2: products browse --- */
    win := DemoWindow():new()
    win:move( 56, 4 )
    win:resize( 52, 14 )
    win:setWindowTitle( "Products" )
    BuildBrowse( win, "Products", ;
        { { "P-01", "Keyboard",      "Input",     49.90 }, ;
          { "P-02", "Mouse",         "Input",     19.50 }, ;
          { "P-03", "Monitor 27",    "Display",  289.00 }, ;
          { "P-04", "Laptop Stand",  "Desk",      35.00 }, ;
          { "P-05", "USB-C Hub",     "Adapter",   42.90 }, ;
          { "P-06", "Webcam HD",     "Video",     64.00 }, ;
          { "P-07", "Headset",       "Audio",     79.99 }, ;
          { "P-08", "SSD 1TB",       "Storage",   99.00 }, ;
          { "P-09", "Printer",       "Office",   159.00 }, ;
          { "P-10", "Desk Lamp",     "Desk",      27.50 }, ;
          { "P-11", "Speakers",      "Audio",     54.00 }, ;
          { "P-12", "Router",        "Network",   89.00 } }, ;
        { { "Code", 1, 6, NIL }, ;
          { "Name", 2, 14, NIL }, ;
          { "Group", 3, 9, NIL }, ;
          { "Price", 4, 9, "99,999.99" } } )
    win:show()

    /* --- Window 3: customers browse --- */
    win := DemoWindow():new()
    win:move( 14, 19 )
    win:resize( 60, 14 )
    win:setWindowTitle( "Customers" )
    BuildBrowse( win, "Customers", ;
        { { 1001, "Petar Petrovic",    "Beograd",     1250.00 }, ;
          { 1002, "Marko Markovic",    "Novi Sad",     980.50 }, ;
          { 1003, "Jelena Jovanovic",  "Nis",         3100.00 }, ;
          { 1004, "Milan Nikolic",     "Kragujevac",   450.00 }, ;
          { 1005, "Ana Ilic",          "Subotica",    2200.75 }, ;
          { 1006, "Nikola Djordjevic", "Zrenjanin",    760.00 }, ;
          { 1007, "Ivana Stojanovic",  "Pancevo",     1890.00 }, ;
          { 1008, "Stefan Pavlovic",   "Cacak",        320.40 }, ;
          { 1009, "Milica Lazic",      "Kraljevo",    1475.00 }, ;
          { 1010, "Dragan Simic",      "Smederevo",    605.20 } }, ;
        { { "ID", 1, 5, "9999" }, ;
          { "Name", 2, 18, NIL }, ;
          { "City", 3, 11, NIL }, ;
          { "Balance", 4, 10, "999,999.99" } } )
    win:show()

    app:exec()

RETURN

/** Dialog-style form: two GETs and OK / Cancel buttons. */
STATIC PROCEDURE BuildGetDialog( win )

    LOCAL oGetName, oGetAmount
    LOCAL sep, btn

    oGetName := HTGet():new( win )
    oGetName:setup( {|x| IIF( x == NIL, s_cName, s_cName := x ) }, ;
        "Name:", "@!", NIL, NIL, "Customer name (uppercase)", NIL, .F., .F., 9 + 25 )
    oGetName:setLabelWidth( 9 )
    oGetName:move( 2, 1 )

    oGetAmount := HTGet():new( win )
    oGetAmount:setup( {|x| IIF( x == NIL, s_nAmount, s_nAmount := x ) }, ;
        "Amount:", "999,999.99", ;
        {|v| IIF( v >= 0, .T., ( ShowStatus( "Amount must be >= 0" ), .F. ) ) }, ;
        NIL, "Amount (>= 0)", NIL, .F., .F., 9 + 10 )
    oGetAmount:setLabelWidth( 9 )
    oGetAmount:move( 2, 3 )

    sep := HTSeparator():new( win )
    sep:move( 1, 5 )
    sep:resize( 44, 1 )

    btn := HTPushButton():new( "OK", win )
    btn:move( 2, 7 )
    btn:onClicked := {|| ShowStatus( "Dialog OK:  name=" + AllTrim( s_cName ) + ;
        "  amount=" + LTrim( Transform( s_nAmount, "999,999.99" ) ) ) }

    btn := HTPushButton():new( "Cancel", win )
    btn:move( 10, 7 )
    btn:onClicked := {|| oGetName:setValue( Space( 25 ) ), ;
        oGetAmount:setValue( 0 ), ;
        win:repaint(), ;
        ShowStatus( "Dialog cancelled - fields cleared" ) }

RETURN

/** Fills a window with an HTBrowse over aData.
 * @param win    Parent window
 * @param cTitle Prefix for status messages
 * @param aData  Array of rows
 * @param aCols  { { cHeading, nIndex, nWidth, cPicture|NIL }, ... }
 */
STATIC PROCEDURE BuildBrowse( win, cTitle, aData, aCols )

    LOCAL aRec := { 1 }   /* current row, shared by the browse blocks */
    LOCAL layout, brw, aCol

    /* browse fills the window; bottom row stays free for the "/" grip */
    layout := HTVBoxLayout():new( win )
    layout:setContentsMargins( 0, 1, 1, 1 )   /* top, left, bottom, right */

    brw := HTBrowse():new( win )
    brw:setGoTopBlock( {|| aRec[ 1 ] := 1 } )
    brw:setGoBottomBlock( {|| aRec[ 1 ] := Len( aData ) } )
    brw:setSkipBlock( {|n| ArraySkip( aRec, Len( aData ), n ) } )

    FOR EACH aCol IN aCols
        IF aCol[ 4 ] == NIL
            brw:addColumn( aCol[ 1 ], ColumnBlock( aData, aRec, aCol[ 2 ] ), aCol[ 3 ] )
        ELSE
            brw:addColumn( aCol[ 1 ], ColumnBlock( aData, aRec, aCol[ 2 ] ), aCol[ 3 ], aCol[ 4 ] )
        ENDIF
    NEXT

    brw:setHeadSep( e"\xC4" )
    brw:setColSep( e"\xB3" )
    brw:onActivated := {|| ShowStatus( cTitle + ": " + hb_CStr( aData[ aRec[ 1 ], 2 ] ) ) }

    /* cells are editable: F2 / typing / double-click, Enter saves, Esc cancels */
    brw:editable := .T.
    brw:onEndEdit := {| nRow, nCol, xOld, xNew | HB_SYMBOL_UNUSED( nRow ), ;
        ShowStatus( cTitle + ": " + aCols[ nCol ][ 1 ] + "  " + ;
            AllTrim( hb_CStr( xOld ) ) + " -> " + AllTrim( hb_CStr( xNew ) ) ), .T. }

    layout:addWidget( brw )

RETURN

/** Read/write column block bound to its own column index: {|x| ... }
 * returns the cell, or stores x into it (used by inline editing). */
STATIC FUNCTION ColumnBlock( aData, aRec, nCol )
RETURN {| x | IIF( x == NIL, aData[ aRec[ 1 ], nCol ], aData[ aRec[ 1 ], nCol ] := x ) }

/** TBrowse skip over an array; aRec[ 1 ] is the current row. */
STATIC FUNCTION ArraySkip( aRec, nLen, nRequest )

    LOCAL nActual

    nActual := IIF( nRequest > 0, ;
        Min( nRequest, nLen - aRec[ 1 ] ), ;
        Max( nRequest, 1 - aRec[ 1 ] ) )
    aRec[ 1 ] += nActual

RETURN nActual

/** Shows a message in the main-window status bar. */
STATIC PROCEDURE ShowStatus( cText )

    IF s_oMainStatus != NIL
        s_oMainStatus:setSection( 1, cText )
    ENDIF

RETURN
