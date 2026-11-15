/**
 * test_browse.prg - Headless tests for HTBrowse.
 *
 * Tests column management and callback configuration; does not exercise
 * TBrowse rendering or keyboard navigation (those require a real GT).
 *
 * Build: hbmk2 test_browse.prg hbmk.hbm
 */

#include "hbtui.ch"
#include "inkey.ch"

STATIC nPass := 0
STATIC nFail := 0

PROCEDURE Main()

    LOCAL nTotal

    TestBrowseInit()
    TestBrowseColumns()
    TestBrowseCallbacks()
    TestBrowseInlineEdit()
    TestBrowseExternal()

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

STATIC PROCEDURE TestBrowseInit()

    LOCAL o

    o := HTBrowse():new()
    Assert( "HTBrowse:columnCount 0 on new",  o:columnCount() == 0 )
    Assert( "HTBrowse:editable default .F.",   ! o:editable )
    Assert( "HTBrowse:onActivated default NIL", o:onActivated == NIL )
    Assert( "HTBrowse:onBeginEdit default NIL", o:onBeginEdit == NIL )
    Assert( "HTBrowse:onEndEdit default NIL",   o:onEndEdit == NIL )

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE TestBrowseColumns()

    LOCAL o
    LOCAL oCol

    o := HTBrowse():new()

    /* addColumn: convenience form (heading + block) */
    o:addColumn( "Name", {|| "Alice"} )
    Assert( "HTBrowse:columnCount 1 after addColumn",  o:columnCount() == 1 )

    o:addColumn( "Age", {|| 30}, 5 )
    Assert( "HTBrowse:columnCount 2 after second add", o:columnCount() == 2 )

    /* addColumn: TBColumn object form */
    oCol := TBColumnNew( "City", {|| "Paris"} )
    oCol:width := 12
    o:addColumn( oCol )
    Assert( "HTBrowse:columnCount 3 after TBColumn add", o:columnCount() == 3 )

    /* browse() accessor returns the underlying TBrowse */
    Assert( "HTBrowse:browse() not NIL",               o:browse() != NIL )
    Assert( "HTBrowse:browse():colCount == 3",          o:browse():colCount == 3 )

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE TestBrowseCallbacks()

    LOCAL o
    LOCAL lSkipFired  := .F.
    LOCAL lTopFired   := .F.
    LOCAL lBotFired   := .F.

    o := HTBrowse():new()

    /* set navigation blocks — verify they are stored and callable */
    o:setSkipBlock(    {|n| lSkipFired := ( n != NIL ) } )
    o:setGoTopBlock(   {|| lTopFired   := .T. } )
    o:setGoBottomBlock({|| lBotFired   := .T. } )

    Eval( o:browse():skipBlock, 1 )
    Assert( "HTBrowse:setSkipBlock stored and callable", lSkipFired )

    Eval( o:browse():goTopBlock )
    Assert( "HTBrowse:setGoTopBlock stored and callable", lTopFired )

    Eval( o:browse():goBottomBlock )
    Assert( "HTBrowse:setGoBottomBlock stored and callable", lBotFired )

    /* callback properties writable */
    o:onActivated := {|r, c| lBotFired := ( r >= 0 .AND. c >= 0 ) }
    Assert( "HTBrowse:onActivated set",  o:onActivated != NIL )

    Assert( "HTBrowse:editable still .F.", ! o:editable )

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE TestBrowseInlineEdit()

    LOCAL o
    LOCAL aData := { { "A", 10.50 } }
    LOCAL aEnd := {}

    o := HTBrowse():new()
    o:setGoTopBlock( {|| NIL } )
    o:setGoBottomBlock( {|| NIL } )
    o:setSkipBlock( {|| 0 } )
    o:addColumn( "Name",   {|x| IIF( x == NIL, aData[ 1, 1 ], aData[ 1, 1 ] := x ) }, 10 )
    o:addColumn( "Amount", {|x| IIF( x == NIL, aData[ 1, 2 ], aData[ 1, 2 ] := x ) }, 8, "9999.99" )
    o:editable := .T.
    o:onEndEdit := {| r, c, xOld, xNew | HB_SYMBOL_UNUSED( r ), AAdd( aEnd, { c, xOld, xNew } ), .T. }

    /* F2 on text: cursor after the text, typed keys extend it, Enter saves */
    o:keyEvent( HTKeyEvent():new( K_F2 ) )
    Assert( "Edit: F2 starts editing",              o:isEditing() )
    o:keyEvent( HTKeyEvent():new( Asc( "B" ) ) )
    o:keyEvent( HTKeyEvent():new( Asc( "C" ) ) )
    o:keyEvent( HTKeyEvent():new( K_ENTER ) )
    Assert( "Edit: Enter ends editing",             ! o:isEditing() )
    Assert( "Edit: text saved (RTrim)",             aData[ 1, 1 ] == "ABC" )
    Assert( "Edit: onEndEdit old/new",              Len( aEnd ) == 1 .AND. aEnd[ 1, 2 ] == "A" .AND. aEnd[ 1, 3 ] == "ABC" )

    /* typing on a numeric cell replaces it and saves a number */
    o:browse():colPos := 2
    o:keyEvent( HTKeyEvent():new( Asc( "4" ) ) )
    o:keyEvent( HTKeyEvent():new( Asc( "2" ) ) )
    o:keyEvent( HTKeyEvent():new( K_ENTER ) )
    Assert( "Edit: numeric saved as number",        HB_ISNUMERIC( aData[ 1, 2 ] ) .AND. aData[ 1, 2 ] == 42 )
    Assert( "Edit: onEndEdit gets typed value",     Len( aEnd ) == 2 .AND. aEnd[ 2, 2 ] == 10.50 .AND. aEnd[ 2, 3 ] == 42 )

    /* Esc discards */
    o:keyEvent( HTKeyEvent():new( Asc( "9" ) ) )
    o:keyEvent( HTKeyEvent():new( K_ESC ) )
    Assert( "Edit: Esc keeps the old value",        aData[ 1, 2 ] == 42 .AND. Len( aEnd ) == 2 )

    /* not editable: typing does not start an edit */
    o:editable := .F.
    o:keyEvent( HTKeyEvent():new( Asc( "7" ) ) )
    Assert( "Edit: disabled when not editable",     ! o:isEditing() )

RETURN

/* an existing TBrowse shown by HTBrowse (setBrowse); own colors; a repaint
   draws from the rows TBrowse read, refreshAll() reads them again */
STATIC PROCEDURE TestBrowseExternal()

    LOCAL o, oTB, n
    LOCAL nReads := 0
    LOCAL aRow := { "X" }

    oTB := TBrowseNew()
    oTB:goTopBlock    := {|| NIL }
    oTB:goBottomBlock := {|| NIL }
    oTB:skipBlock     := {|| 0 }
    oTB:addColumn( TBColumnNew( "Col", {|| ++nReads, aRow[ 1 ] } ) )

    o := HTBrowse():new()
    o:setBrowse( oTB )
    o:resize( 20, 5 )
    Assert( "setBrowse: shows that TBrowse",       o:browse() == oTB .AND. o:columnCount() == 1 )
    Assert( "resize: size holds at once",          o:width == 20 .AND. o:height == 5 )

    o:setColorSpec( "W/B,N/W" )
    o:paintEvent( HTPaintEvent():new() )
    Assert( "setColorSpec: kept over the theme",   oTB:colorSpec == "W/B,N/W" )

    n := nReads
    o:paintEvent( HTPaintEvent():new() )
    Assert( "repaint: no row read again",          nReads == n )

    o:refreshAll()
    o:paintEvent( HTPaintEvent():new() )
    Assert( "refreshAll: rows read again",         nReads > n )

    /* the cell: first data row; TBrowse centers a narrow column */
    SetPos( 0, 0 )
    o:stabilize()
    Assert( "stabilize: cursor on the current cell", Row() == oTB:nTop + 1 .AND. ;
        Col() >= oTB:nLeft .AND. Col() <= oTB:nRight - 2 )

    o:setColorSpec( NIL )
    o:paintEvent( HTPaintEvent():new() )
    Assert( "setColorSpec( NIL ): theme colors",   !( oTB:colorSpec == "W/B,N/W" ) )

RETURN
