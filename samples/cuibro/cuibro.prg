/** @file cuibro.prg
 * CUIBro - xBase browse utility and file manager in character mode.
 *
 * Modeled after BroPlus 2.50 (SmartCode Programming, Peter Volz) and its
 * HWGUI port GBro: the same function key menus (F1 Help .. F9 Exit), the
 * same hot keys and status lines - written from scratch on HBTui widgets.
 *
 * Every open database has its own floating window:
 *   title       alias, position among the open databases, index order
 *               (BroPlus Bp_ShowStat title)
 *   status bar  Rec. n/count, <Lock: n>, <Shared>, <Softseek>, <Filter>,
 *               <Rel: ...>, <Deleted> (BroPlus Bp_IdleStat)
 *   browse      the records; Enter edits a field, Ins appends, Del deletes
 *   key line    Key: index key expression (BroPlus Bp_ShowStat key line)
 * The main window holds the F-key row, the menu bar and the bottom line
 * (BroPlus Bp_StatLine: directory, edit mode, incremental seek string).
 *
 * Relations (F5 AreaSet / Alt-R): the active window becomes the parent of
 * another open database. Moving in the parent repositions the child, and
 * with "related records only" the child window shows just the records with
 * the parent's key (master-detail). Without parameters CUIBro creates
 * CUSTOMER.DBF and ORDERS.DBF next to the program and opens them related
 * by customer ID.
 *
 * Usage: cuibro [table[.dbf] [index]]
 * Keys:  F1..F9 menus, Ctrl+F6 next window, F1 > Browse Window Help
 */

#include "hbtui.ch"
#include "inkey.ch"
#include "set.ch"
#include "dbinfo.ch"
#include "directry.ch"

REQUEST DBFCDX, DBFNTX

/* one open database */
#define AR_ALIAS     1
#define AR_FILE      2
#define AR_DRIVER    3
#define AR_BAGS      4   /* index files opened by hand */
#define AR_WIN       5   /* TableWindow */
#define AR_BRW       6   /* CBBrowse */
#define AR_IDLE      7   /* HTStatusBar: Bp_IdleStat indicators */
#define AR_KEY       8   /* HTLabel: Key: line */
#define AR_RELS      9   /* { { cChildAlias, cKeyExpr, bKey, lRelatedOnly }, ... } */
#define AR_IDLETXT   10  /* texts shown in the indicator sections */
#define AR_LASTREC   11  /* record the children were last synchronized to */
#define AR_LEN       11

#define REL_CHILD    1
#define REL_EXPR     2
#define REL_BLOCK    3
#define REL_ONLY     4

#define IDLE_SECTIONS 5

STATIC s_aAreas := {}
STATIC s_nArea  := 0
STATIC s_oMain, s_oBar, s_oMsg
STATIC s_aMsgTxt := { "", "", "" }
STATIC s_hAct := { => }
STATIC s_hFKeys := { => }

/* GlobalSet */
STATIC s_cDriver    := "DBFCDX"
STATIC s_lExclusive := .F.
STATIC s_lModify    := .T.
STATIC s_lImmediate := .F.
STATIC s_lSoftSeek  := .F.
STATIC s_cTheme     := "Classic"

/* AreaSet / Record */
STATIC s_lIncSeek := .F.
STATIC s_cSeek    := ""
STATIC s_cLastSeek := ""
STATIC s_cLocate  := ""
STATIC s_xClipCell
STATIC s_aClipRec
STATIC s_oCutCol

/*---------------------------------------------------------------- windows */

/* main window: keys reach it while a menu is open, or after a click on
   the menu bar or the desktop - give them back to the database window */
CLASS MainShell FROM HTMainWindow
    METHOD keyEvent( keyEvent )
ENDCLASS

METHOD PROCEDURE keyEvent( keyEvent ) CLASS MainShell

    ::super:keyEvent( keyEvent )

    IF ! s_oBar:isActive()
        ActivateCurrent()
        IF ! keyEvent:isAccepted .AND. s_nArea > 0
            s_aAreas[ s_nArea ][ AR_WIN ]:keyEvent( keyEvent )
        ENDIF
    ENDIF

RETURN

/* floating window of one database */
CLASS TableWindow FROM HTWidget

PUBLIC:
    DATA oIdle
    DATA oBrw
    DATA oKey

    METHOD new()
    METHOD arrange()
    METHOD place( nX, nY, nW, nH )
    METHOD title() INLINE ::FwindowTitle
    METHOD paintEvent( event )
    METHOD keyEvent( keyEvent )
    METHOD mouseEvent( eventMouse )
    METHOD focusInEvent( eventFocus )
    METHOD closeEvent( closeEvent )

    PROPERTY onWindowKey READWRITE      /* {|oWin, nKey| lUsed } before the browse */
    PROPERTY onActivity READWRITE       /* {|oWin| ... } focus, click */
    PROPERTY onClose READWRITE          /* {|oWin| ... } before the window goes */

ENDCLASS

METHOD new() CLASS TableWindow
    ::super:new()
    ::shadow := HT_SHADOW_ATTR
RETURN self

/* status bar on top, browse, key line on the last row (left of the grip) */
METHOD PROCEDURE arrange() CLASS TableWindow
    IF ::oBrw != NIL
        ::oIdle:setGeometry( 0, 0, ::Fwidth, 1 )
        ::oBrw:setGeometry( 0, 1, ::Fwidth, Max( 1, ::Fheight - 3 ) )
        ::oKey:setGeometry( 0, ::Fheight - 2, Max( 1, ::Fwidth - 1 ), 1 )
    ENDIF
RETURN

METHOD PROCEDURE place( nX, nY, nW, nH ) CLASS TableWindow
    ::Fx := nX
    ::Fy := nY
    ::Fwidth := nW
    ::Fheight := nH
    ::FlPosSet := .T.
    ::FlMaximized := .F.
    ::FlMinimized := .F.
    IF ::FwindowId != NIL
        ht_winSetRect( ::FwindowId, nY, nX, nY + nH - 1, nX + nW - 1 )
    ENDIF
RETURN

/* any size change (resize grip, maximize, tile) is painted here */
METHOD PROCEDURE paintEvent( event ) CLASS TableWindow
    ::arrange()
    ::super:paintEvent( event )
RETURN

METHOD PROCEDURE keyEvent( keyEvent ) CLASS TableWindow
    IF ::FonWindowKey != NIL .AND. Eval( ::FonWindowKey, self, keyEvent:key )
        keyEvent:accept()
        RETURN
    ENDIF
    ::super:keyEvent( keyEvent )
RETURN

METHOD PROCEDURE mouseEvent( eventMouse ) CLASS TableWindow
    IF ::FonActivity != NIL
        Eval( ::FonActivity, self )
    ENDIF
    ::super:mouseEvent( eventMouse )
RETURN

METHOD PROCEDURE focusInEvent( eventFocus ) CLASS TableWindow
    ::super:focusInEvent( eventFocus )
    IF ::FonActivity != NIL
        Eval( ::FonActivity, self )
    ENDIF
RETURN

METHOD PROCEDURE closeEvent( closeEvent ) CLASS TableWindow
    IF ::FonClose != NIL
        Eval( ::FonClose, self )
    ENDIF
    ::super:closeEvent( closeEvent )
RETURN

/* browse that lets CUIBro see each key first and report every move */
CLASS CBBrowse FROM HTBrowse
    METHOD keyEvent( keyEvent )
    METHOD mouseEvent( eventMouse )
    PROPERTY onKeyPress READWRITE       /* {|nKey| lUsed } when not editing */
    PROPERTY onMoved READWRITE          /* {|| ... } after a key or click */
ENDCLASS

METHOD PROCEDURE keyEvent( keyEvent ) CLASS CBBrowse
    IF ! ::isEditing() .AND. ::FonKeyPress != NIL .AND. Eval( ::FonKeyPress, keyEvent:key )
        keyEvent:accept()
    ELSE
        ::super:keyEvent( keyEvent )
    ENDIF
    IF ::FonMoved != NIL
        Eval( ::FonMoved )
    ENDIF
RETURN

METHOD PROCEDURE mouseEvent( eventMouse ) CLASS CBBrowse
    ::super:mouseEvent( eventMouse )
    IF ::FonMoved != NIL
        Eval( ::FonMoved )
    ENDIF
RETURN

/*---------------------------------------------------------------- main */

PROCEDURE Main( cDbf, cIndex )

    LOCAL app, aDemo

    Set( _SET_DATEFORMAT, "dd.mm.yyyy" )
    Set( _SET_EPOCH, 1950 )
    LoadSettings()
    rddSetDefault( s_cDriver )

    app := HTApplication():new()
    ht_cuiSetupDisplay( app:rows(), app:cols() )
    IF ! s_cTheme == "Classic"
        HTTheme():loadTheme( s_cTheme )
    ENDIF

    s_oMain := MainShell():new()
    s_oMain:setWindowTitle( "CUIBro" )
    s_oMain:show()

    s_oMsg := HTStatusBar():new( s_oMain )
    s_oMsg:setGeometry( 0, s_oMain:height - 2, s_oMain:width, 1 )
    AEval( s_aMsgTxt, {|| s_oMsg:addSection( "" ) } )

    BuildMenu()
    s_oMain:setWindowTitle( FKeyRow() )

    IF Empty( cDbf )
        aDemo := DemoTables()
        IF OpenTable( aDemo[ 1 ] ) .AND. OpenTable( aDemo[ 2 ] )
            ordSetFocus( "CUSTID" )
            SetRelation( 1, "ORDERS", "ID", .T. )
        ENDIF
        TileWindows()
        IF Len( s_aAreas ) > 0
            SelectArea( 1, .F. )
            s_aAreas[ 1 ][ AR_WIN ]:activate()
        ENDIF
    ELSEIF OpenTable( cDbf ) .AND. ! Empty( cIndex )
        AddIndexFile( cIndex )
    ENDIF

    ShowStatLine()
    app:exec()

    dbCloseAll()

RETURN

/* the F-key row of BroPlus, over the menu titles (window title row) */
STATIC FUNCTION FKeyRow()

    LOCAL aTitles := { "Help", "Database", "Index", "Fields", "AreaSet", ;
                       "GlobalSet", "Record", "Command", "Exit" }
    LOCAL cRow := "", nCol := 2, n

    FOR n := 1 TO Len( aTitles )
        cRow := PadR( cRow, nCol - 1 ) + "F" + hb_ntos( n )
        nCol += Len( aTitles[ n ] ) + 3
    NEXT

RETURN PadR( cRow, nCol + 2 ) + "CUIBro"

STATIC PROCEDURE BuildMenu()

    LOCAL oMenu, n
    LOCAL aFKeys := { K_F1, K_F2, K_F3, K_F4, K_F5, K_F6, K_F7, K_F8, K_F9 }

    s_oBar := HTMenuBar():new( s_oMain )

    oMenu := s_oBar:addMenu( "Help" )
    Item( oMenu, "&Browse Window Help           ", NIL, {|| BrowseHelp() }, .F. )
    Item( oMenu, "&Expression Evaluator <Alt-F1>", K_ALT_F1, {|| Evaluator() }, .F. )
    Item( oMenu, "c&Opyright                    ", NIL, {|| Copyright() }, .F. )

    oMenu := s_oBar:addMenu( "Database" )
    Item( oMenu, "&Database Selection Screen  <Alt-F2>", K_ALT_F2, {|| SelectDatabase() }, .F. )
    Item( oMenu, "&Next Database              <Alt-N> ", K_ALT_N, {|| NextArea( 1 ) } )
    Item( oMenu, "&Previous Database       <Alt-Minus>", K_ALT_MINUS, {|| NextArea( -1 ) } )
    Item( oMenu, "c&Lose Database                     ", NIL, {|| CloseDatabase() } )
    oMenu:addSeparator()
    Item( oMenu, "", K_ALT_X, {|| ToggleOpenMode() }, .T., "openmode" )
    Item( oMenu, "Create/&Modify Structure    <Alt-M> ", K_ALT_M, {|| CreateModify() }, .F. )
    oMenu:addSeparator()
    Item( oMenu, "&Change Directory           <Alt-Y> ", K_ALT_Y, {|| ChangeDirectory() }, .F. )

    oMenu := s_oBar:addMenu( "Index" )
    Item( oMenu, "&Index File Selection Screen <Alt-F3>", K_ALT_F3, {|| OpenIndexFile() } )
    Item( oMenu, "&Next Order                  <Alt-O> ", K_ALT_O, {|| NextOrder( 1 ) } )
    Item( oMenu, "&Previous Order                      ", NIL, {|| NextOrder( -1 ) } )
    Item( oMenu, "&Tag Selection/Information  <Ctrl-F3>", K_CTRL_F3, {|| ChooseOrder() } )
    Item( oMenu, "Index &ScopeTop/Bottom      <Shft-F3>", K_SH_F3, {|| ScopeDlg() } )
    oMenu:addSeparator()
    Item( oMenu, "c&Reate Index File                   ", NIL, {|| CreateIndexDlg() } )

    oMenu := s_oBar:addMenu( "Fields" )
    Item( oMenu, "&Field Selection Screen           ", NIL, {|| SelectFields() } )
    Item( oMenu, "&Cut/Delete Column     <Shift-F5>", K_SH_F5, {|| CutColumn() } )
    Item( oMenu, "&Paste Column           <Ctrl-F5>", K_CTRL_F5, {|| PasteColumn() } )
    Item( oMenu, "&Insert Column           <Alt-F5>", K_ALT_F5, {|| InsertColumn() } )
    Item( oMenu, "", K_ALT_L, {|| LockFields() }, .T., "lock" )
    Item( oMenu, "Column &Options...       <Alt-H> ", K_ALT_H, {|| ColumnOptions() } )
    Item( oMenu, "Create/&Modify Structure         ", NIL, {|| CreateModify() }, .F. )

    oMenu := s_oBar:addMenu( "AreaSet" )
    Item( oMenu, "Range & &Filter          <Alt-F>", K_ALT_F, {|| FilterDlg() } )
    Item( oMenu, "&Relation                <Alt-R>", K_ALT_R, {|| RelationDlg() } )
    Item( oMenu, "", K_ALT_I, {|| ToggleIncSeek() }, .T., "incseek" )
    oMenu:addSeparator()
    Item( oMenu, "&Next Select Area        <Alt-N>", NIL, {|| NextArea( 1 ) } )
    Item( oMenu, "&Prev. Select Area   <Alt-Minus>", NIL, {|| NextArea( -1 ) } )
    Item( oMenu, "Next Index &Order        <Alt-O>", NIL, {|| NextOrder( 1 ) } )
    oMenu:addSeparator()
    Item( oMenu, "Ne&xt Window           <Ctrl-F6>", NIL, {|| HTApplication():activateNextWindow( 1 ) } )
    Item( oMenu, "&Tile Windows                   ", NIL, {|| TileWindows(), HTApplication():repaintAll() } )
    Item( oMenu, "&Cascade Windows                ", NIL, {|| CascadeWindows(), HTApplication():repaintAll() } )

    oMenu := s_oBar:addMenu( "GlobalSet" )
    Item( oMenu, "", NIL, {|| ChooseDriver() }, .F., "driver" )
    Item( oMenu, "", NIL, {|| s_lExclusive := ! s_lExclusive }, .F., "defmode" )
    Item( oMenu, "", NIL, {|| s_lModify := ! s_lModify }, .F., "modify" )
    Item( oMenu, "", NIL, {|| s_lImmediate := ! s_lImmediate }, .F., "edit" )
    Item( oMenu, "", K_ALT_D, {|| ToggleDeleted() }, .F., "deleted" )
    Item( oMenu, "", K_ALT_Q, {|| s_lSoftSeek := ! s_lSoftSeek }, .F., "softseek" )
    Item( oMenu, "", NIL, {|| SetEpoch() }, .F., "epoch" )
    Item( oMenu, "", NIL, {|| SetDateFormat() }, .F., "datefmt" )
    oMenu:addSeparator()
    Item( oMenu, "", NIL, {|| ChooseTheme() }, .F., "theme" )
    Item( oMenu, "Sa&ve Global Settings           ", NIL, {|| SaveSettings() }, .F. )

    oMenu := s_oBar:addMenu( "Record" )
    Item( oMenu, "&Seek          <Alt-S>", K_ALT_S, {|| SeekDlg() } )
    Item( oMenu, "&Goto          <Alt-G>", K_ALT_G, {|| GotoDlg() } )
    Item( oMenu, "&Locate        <Alt-U>", K_ALT_U, {|| LocateDlg() } )
    Item( oMenu, "sk&Ip          <Alt-J>", K_ALT_J, {|| SkipDlg() } )
    Item( oMenu, "Window &Top    <Alt-T>", K_ALT_T, {|| WindowEdge( .T. ) } )
    Item( oMenu, "Window &Bottom <Alt-B>", K_ALT_B, {|| WindowEdge( .F. ) } )
    oMenu:addSeparator()
    Item( oMenu, "&All Fields    <Alt-A>", K_ALT_A, {|| EditRecord( .F. ) } )
    Item( oMenu, "s&Elected Flds <Alt-E>", K_ALT_E, {|| EditRecord( .T. ) } )
    Item( oMenu, "Ap&pend Record  <Ins> ", NIL, {|| AppendRecord() } )
    Item( oMenu, "&Delete/Recall  <Del> ", NIL, {|| DeleteRecall() } )
    Item( oMenu, "Erase &Memo <Alt-BSpc>", NIL, {|| EraseMemo() } )
    oMenu:addSeparator()
    Item( oMenu, "&Copy Field  <Shft-F4>", K_SH_F4, {|| CopyField() } )
    Item( oMenu, "Paste &Field <Ctrl-F4>", K_CTRL_F4, {|| PasteField() } )
    Item( oMenu, "c&Opy Record <Shft-F7>", K_SH_F7, {|| CopyRecord() } )
    Item( oMenu, "Paste &Recrd <Ctrl-F7>", K_CTRL_F7, {|| PasteRecord() } )
    Item( oMenu, "&View Clipbrd <Alt-F7>", K_ALT_F7, {|| ViewClipboard() }, .F. )

    oMenu := s_oBar:addMenu( "Command" )
    Item( oMenu, "&Append From   ", NIL, {|| AppendFrom() } )
    Item( oMenu, "&Copy Structure", NIL, {|| CopyStructure( .F. ) } )
    Item( oMenu, "Copy Stru E&xte", NIL, {|| CopyStructure( .T. ) } )
    Item( oMenu, "C&opy To       ", NIL, {|| CopyTo() } )
    Item( oMenu, "Coun&t         ", NIL, {|| CountDlg() } )
    Item( oMenu, "&Delete        ", NIL, {|| DeleteFor( .T. ) } )
    Item( oMenu, "&Pack          ", NIL, {|| PackZap( .T. ) } )
    Item( oMenu, "&Recall        ", NIL, {|| DeleteFor( .F. ) } )
    Item( oMenu, "Rei&ndex       ", NIL, {|| Reindex() } )
    Item( oMenu, "R&eplace       ", NIL, {|| ReplaceDlg() } )
    Item( oMenu, "&Sum/Average   ", NIL, {|| SumDlg() } )
    Item( oMenu, "&Zap           ", NIL, {|| PackZap( .F. ) } )

    oMenu := s_oBar:addMenu( "Exit" )
    Item( oMenu, "e&Xit", NIL, {|| Quit() }, .F. )

    /* F1..F9 open the menus, from a database window too (WindowKey) */
    FOR n := 1 TO Len( aFKeys )
        s_hFKeys[ aFKeys[ n ] ] := MenuOpener( n )
        s_oMain:onKey( aFKeys[ n ], s_hFKeys[ aFKeys[ n ] ] )
    NEXT

    MenuTexts()

RETURN

STATIC FUNCTION MenuOpener( n )
RETURN {|| s_oBar:popup( n ) }

/* menu entry; lTable: needs an open database; cId: text kept up to date */
STATIC FUNCTION Item( oMenu, cText, nKey, bAction, lTable, cId )

    LOCAL oAct := oMenu:addAction( cText )

    IF nKey != NIL
        oAct:setShortcut( nKey )
    ENDIF
    IF hb_defaultValue( lTable, .T. )
        oAct:onTriggered := {|| IIF( NeedTable(), Eval( bAction ), NIL ), AfterAction() }
    ELSE
        oAct:onTriggered := {|| Eval( bAction ), AfterAction() }
    ENDIF
    IF cId != NIL
        s_hAct[ cId ] := oAct
    ENDIF

RETURN oAct

STATIC PROCEDURE AfterAction()
    MenuTexts()
    ShowStatLine()
    IF s_nArea > 0
        ShowAreaStatus( s_nArea )
    ENDIF
RETURN

STATIC PROCEDURE MenuTexts()

    LOCAL lShared := .T.

    IF s_nArea > 0
        lShared := ( s_aAreas[ s_nArea ][ AR_ALIAS ] )->( dbInfo( DBI_SHARED ) )
    ENDIF

    SetActText( "openmode", "&Open Mode:     " + IIF( lShared, "SHARED   ", "EXCLUSIVE" ) + "   <Alt-X> " )
    SetActText( "lock", "&Lock # of Fields: " + PadR( hb_ntos( IIF( s_nArea > 0, CurTB():freeze, 0 ) ), 2 ) + "    <Alt-L> " )
    SetActText( "incseek", "&Incremental Seek is " + OnOff( s_lIncSeek ) + " <Alt-I>" )
    SetActText( "driver", "Database d&River:   " + PadR( s_cDriver, 13 ) )
    SetActText( "defmode", "Default &Open Mode: " + PadR( IIF( s_lExclusive, "EXCLUSIVE", "SHARED" ), 13 ) )
    SetActText( "modify", "&Modify:            " + PadR( IIF( s_lModify, "ENABLED", "DISABLED" ), 13 ) )
    SetActText( "edit", PadR( IIF( s_lImmediate, "Imm&ediate", "Prot&ected" ) + " Edit", 32 ) )
    SetActText( "deleted", "H&ide Deleted Recs is " + OnOff( Set( _SET_DELETED ) ) + " <Alt-D>" )
    SetActText( "softseek", "&Softseek          is " + OnOff( s_lSoftSeek ) + " <Alt-Q>" )
    SetActText( "epoch", "E&poch:             " + PadR( hb_ntos( Set( _SET_EPOCH ) ), 13 ) )
    SetActText( "datefmt", "Date &Format:       " + PadR( Set( _SET_DATEFORMAT ), 13 ) )
    SetActText( "theme", "&Theme:             " + PadR( s_cTheme, 13 ) )

RETURN

STATIC PROCEDURE SetActText( cId, cText )
    IF hb_HHasKey( s_hAct, cId )
        s_hAct[ cId ]:setText( cText )
    ENDIF
RETURN

STATIC FUNCTION OnOff( l )
RETURN IIF( l, "ON ", "OFF" )

STATIC FUNCTION NeedTable()
    IF s_nArea == 0
        Toast( "No Database Selected - F2 or Alt-F2" )
        RETURN .F.
    ENDIF
RETURN .T.

/*---------------------------------------------------------------- databases */

/* opens a database in a new window; .T. when it is open (or already was) */
STATIC FUNCTION OpenTable( cFile )

    LOCAL cAlias, cErr, nPos, oWin, aArea

    cFile := FullPath( DbfName( AllTrim( cFile ) ) )
    IF ! hb_FileExists( cFile )
        Warn( cFile + ": File cannot be found" )
        RETURN .F.
    ENDIF
    IF ( nPos := AScan( s_aAreas, {| a | Upper( a[ AR_FILE ] ) == Upper( cFile ) } ) ) > 0
        SelectArea( nPos, .T. )
        RETURN .T.
    ENDIF

    cAlias := NewAlias( cFile )
    cErr := Try( {|| dbUseArea( .T., s_cDriver, cFile, cAlias, ! s_lExclusive ) } )
    IF Select( cAlias ) == 0
        Warn( hb_FNameNameExt( cFile ) + " is currently " + IIF( s_lExclusive, "opened", "locked" ) + ;
              " by another user" + IIF( Empty( cErr ), "", " - " + cErr ) )
        RETURN .F.
    ENDIF

    oWin := TableWindow():new()
    oWin:oIdle := HTStatusBar():new( oWin )
    oWin:oBrw  := CBBrowse():new( oWin )
    oWin:oKey  := HTLabel():new( "", oWin )
    oWin:onWindowKey := {| o, nKey | WindowKey( o, nKey ) }
    oWin:onActivity  := {| o | SyncArea( o ) }
    oWin:onClose     := {| o | CloseArea( AreaOfWin( o ) ) }
    AEval( Array( IDLE_SECTIONS ), {|| oWin:oIdle:addSection( "" ) } )

    oWin:oBrw:editable := .T.
    oWin:oBrw:onBeginEdit := {| nRow, nCol | CanEdit( nRow, nCol ) }
    oWin:oBrw:onKeyPress  := {| nKey | BrowseKey( nKey ) }
    oWin:oBrw:onMoved     := {|| AfterMove() }
    oWin:oBrw:contextMenu := ContextMenu()
    BuildBrowse( oWin:oBrw:browse(), cAlias )

    aArea := Array( AR_LEN )
    aArea[ AR_ALIAS ]   := cAlias
    aArea[ AR_FILE ]    := cFile
    aArea[ AR_DRIVER ]  := s_cDriver
    aArea[ AR_BAGS ]    := {}
    aArea[ AR_WIN ]     := oWin
    aArea[ AR_BRW ]     := oWin:oBrw
    aArea[ AR_IDLE ]    := oWin:oIdle
    aArea[ AR_KEY ]     := oWin:oKey
    aArea[ AR_RELS ]    := {}
    aArea[ AR_IDLETXT ] := AFill( Array( IDLE_SECTIONS ), "" )
    aArea[ AR_LASTREC ] := 0
    AAdd( s_aAreas, aArea )

    CascadePlace( oWin, Len( s_aAreas ) )
    oWin:setWindowTitle( cAlias )
    s_nArea := Len( s_aAreas )
    ShowAreaStatus( s_nArea )
    oWin:show()

RETURN .T.

STATIC FUNCTION DbfName( cFile )
    IF Empty( hb_FNameExt( cFile ) )
        cFile += ".dbf"
    ENDIF
RETURN cFile

STATIC FUNCTION FullPath( cFile )
    IF ! ( ":" $ cFile .OR. Left( cFile, 1 ) $ "\/" )
        cFile := hb_cwd() + cFile
    ENDIF
RETURN cFile

/* alias from the file name (BroPlus Auto-Alias), unique */
STATIC FUNCTION NewAlias( cFile )

    LOCAL cBase := Upper( hb_FNameName( cFile ) ), cAlias := "", cTry, c, n := 1

    FOR EACH c IN cBase
        cAlias += IIF( IsAlpha( c ) .OR. IsDigit( c ) .OR. c == "_", c, "_" )
    NEXT
    IF Empty( cAlias ) .OR. IsDigit( Left( cAlias, 1 ) )
        cAlias := "T_" + cAlias
    ENDIF
    cAlias := Left( cAlias, 20 )
    cTry := cAlias
    DO WHILE Select( cTry ) > 0
        cTry := cAlias + "_" + hb_ntos( ++n )
    ENDDO

RETURN cTry

/* all fields as columns of oTB */
STATIC PROCEDURE BuildBrowse( oTB, cAlias )

    LOCAL aField

    oTB:skipBlock     := {| n | ( cAlias )->( Skipper( n ) ) }
    oTB:goTopBlock    := {|| ( cAlias )->( dbGoTop() ) }
    oTB:goBottomBlock := {|| ( cAlias )->( dbGoBottom() ) }
    oTB:headSep := e"\xC4"
    oTB:colSep  := e"\xB3"
    DO WHILE oTB:colCount > 0
        oTB:delColumn( 1 )
    ENDDO
    FOR EACH aField IN ( cAlias )->( dbStruct() )
        oTB:addColumn( FieldColumn( cAlias, aField ) )
    NEXT
    oTB:freeze := 0

RETURN

/* column cargo: { cField or cExpression, cType ("E" = expression), cAlias } */
STATIC FUNCTION FieldColumn( cAlias, aField )

    LOCAL nPos := ( cAlias )->( FieldPos( aField[ 1 ] ) )
    LOCAL cType := aField[ 2 ], oCol

    DO CASE
    CASE cType == "M"
        oCol := TBColumnNew( aField[ 1 ], {|| IIF( Empty( ( cAlias )->( FieldGet( nPos ) ) ), Space( 6 ), "<Memo>" ) } )
    CASE IsEditable( cType )
        oCol := TBColumnNew( aField[ 1 ], FieldRW( cAlias, nPos ) )
        IF cType == "N"
            oCol:picture := NumPicture( aField[ 3 ], aField[ 4 ] )
        ELSEIF cType == "C" .AND. aField[ 3 ] > 50
            oCol:width := 50
        ENDIF
    CASE cType $ "WGP"
        oCol := TBColumnNew( aField[ 1 ], {|| "<Binary>" } )
    OTHERWISE
        oCol := TBColumnNew( aField[ 1 ], {|| ( cAlias )->( FieldGet( nPos ) ) } )
    ENDCASE
    oCol:cargo := { aField[ 1 ], cType, cAlias }

RETURN oCol

/* read/write block: inline editing writes through it */
STATIC FUNCTION FieldRW( cAlias, nPos )
RETURN {| x | IIF( x == NIL, ( cAlias )->( FieldGet( nPos ) ), PutField( cAlias, nPos, x ) ) }

STATIC FUNCTION PutField( cAlias, nPos, xValue )
    IF ( cAlias )->( RLock() )
        ( cAlias )->( FieldPut( nPos, xValue ) )
        ( cAlias )->( dbCommit() )
        ( cAlias )->( dbUnlock() )
    ELSE
        Warn( "Record is currently locked by another user" )
    ENDIF
RETURN xValue

STATIC FUNCTION IsEditable( cType )
RETURN cType $ "CNDLIBYFTQ@"

STATIC FUNCTION NumPicture( nLen, nDec )
    IF nDec > 0
        RETURN Replicate( "9", Max( 1, nLen - nDec - 1 ) ) + "." + Replicate( "9", nDec )
    ENDIF
RETURN Replicate( "9", nLen )

STATIC FUNCTION Skipper( nRequest )

    LOCAL nDone := 0

    IF LastRec() == 0
        RETURN 0
    ENDIF
    IF nRequest == 0
        dbSkip( 0 )
    ELSEIF nRequest > 0
        DO WHILE nDone < nRequest
            dbSkip( 1 )
            IF Eof()
                dbSkip( -1 )
                EXIT
            ENDIF
            nDone++
        ENDDO
    ELSE
        DO WHILE nDone > nRequest
            dbSkip( -1 )
            IF Bof()
                EXIT
            ENDIF
            nDone--
        ENDDO
    ENDIF

RETURN nDone

STATIC FUNCTION CurTB()
RETURN s_aAreas[ s_nArea ][ AR_BRW ]:browse()

STATIC FUNCTION CurColumn()

    LOCAL oTB

    IF s_nArea == 0
        RETURN NIL
    ENDIF
    oTB := CurTB()
    IF oTB:colCount == 0
        RETURN NIL
    ENDIF

RETURN oTB:getColumn( oTB:colPos )

STATIC FUNCTION AreaOfWin( oWin )
RETURN AScan( s_aAreas, {| a | a[ AR_WIN ] == oWin } )

STATIC FUNCTION AreaByAlias( cAlias )
RETURN AScan( s_aAreas, {| a | a[ AR_ALIAS ] == cAlias } )

/* makes area n the current one; lActivate also brings its window up */
STATIC PROCEDURE SelectArea( n, lActivate )

    s_nArea := n
    s_cSeek := ""
    IF n > 0
        dbSelectArea( s_aAreas[ n ][ AR_ALIAS ] )
        IF lActivate
            ActivateCurrent()
        ENDIF
        ShowAreaStatus( n )
    ENDIF
    MenuTexts()
    ShowStatLine()

RETURN

/* a window got the focus, a click or a key: its database becomes current */
STATIC PROCEDURE SyncArea( oWin )

    LOCAL n := AreaOfWin( oWin )

    IF n > 0 .AND. n != s_nArea
        SelectArea( n, .F. )
    ENDIF

RETURN

/* keyboard back to the window of the current database */
STATIC PROCEDURE ActivateCurrent()

    LOCAL oWin

    IF s_nArea > 0
        oWin := s_aAreas[ s_nArea ][ AR_WIN ]
        IF oWin:windowId != NIL
            oWin:raise()
            wSelect( oWin:windowId, .F. )
        ENDIF
    ENDIF

RETURN

/* keys of a database window: menu shortcuts and F-keys first (the menu bar
   lives in the main window), then the browse */
STATIC FUNCTION WindowKey( oWin, nKey )

    LOCAL lUsed

    SyncArea( oWin )
    wSelect( s_oMain:windowId, .F. )
    lUsed := s_oBar:handleKey( nKey )
    IF ! lUsed .AND. hb_HHasKey( s_hFKeys, nKey )
        Eval( s_hFKeys[ nKey ] )
        lUsed := .T.
    ENDIF
    IF ! s_oBar:isActive()
        IF AreaOfWin( oWin ) > 0 .AND. ! lUsed
            oWin:raise()
            wSelect( oWin:windowId, .F. )
        ELSE
            ActivateCurrent()
        ENDIF
    ENDIF

RETURN lUsed

STATIC PROCEDURE NextArea( nStep )

    LOCAL n

    IF Len( s_aAreas ) < 2
        Toast( "Only one database is open" )
        RETURN
    ENDIF
    n := s_nArea + nStep
    IF n > Len( s_aAreas )
        n := 1
    ELSEIF n < 1
        n := Len( s_aAreas )
    ENDIF
    SelectArea( n, .T. )

RETURN

STATIC PROCEDURE CloseDatabase()

    LOCAL oWin := s_aAreas[ s_nArea ][ AR_WIN ]

    CloseArea( s_nArea )
    oWin:destroy()
    s_oMain:repaint()
    HTApplication():repaintAll()

RETURN

/* closes the database of area n; the window is destroyed by the caller */
STATIC PROCEDURE CloseArea( n )

    LOCAL cAlias, aArea, i

    IF n == 0
        RETURN
    ENDIF
    cAlias := s_aAreas[ n ][ AR_ALIAS ]

    /* relations into this database go with it */
    FOR i := 1 TO Len( s_aAreas )
        aArea := s_aAreas[ i ]
        IF AScan( aArea[ AR_RELS ], {| r | r[ REL_CHILD ] == cAlias } ) > 0
            aArea[ AR_RELS ] := ADelWhere( aArea[ AR_RELS ], {| r | r[ REL_CHILD ] == cAlias } )
            ApplyRelations( i )
        ENDIF
    NEXT
    FOR EACH aArea IN s_aAreas[ n ][ AR_RELS ]
        ClearChildScope( aArea[ REL_CHILD ] )
    NEXT

    ( cAlias )->( dbCloseArea() )
    hb_ADel( s_aAreas, n, .T. )
    IF Len( s_aAreas ) == 0
        s_nArea := 0
        SelectArea( 0, .F. )
    ELSE
        SelectArea( Min( n, Len( s_aAreas ) ), .F. )
        s_aAreas[ s_nArea ][ AR_WIN ]:activate()
    ENDIF

RETURN

STATIC FUNCTION ADelWhere( aArray, bWhere )

    LOCAL aOut := {}, x

    FOR EACH x IN aArray
        IF ! Eval( bWhere, x )
            AAdd( aOut, x )
        ENDIF
    NEXT

RETURN aOut

/*---------------------------------------------------------------- window layout */

/* screen rows 2 .. MaxRow()-1: under the menu bar, over the bottom line;
   the row under each window takes its shadow */
STATIC PROCEDURE TileWindows()

    LOCAL nTop := 2, nRows := ht_ScreenMaxRow() - 2, nCount := Len( s_aAreas )
    LOCAL nH, n, nY

    IF nCount == 0
        RETURN
    ENDIF
    IF nCount > 3
        CascadeWindows()
        RETURN
    ENDIF
    nH := Int( nRows / nCount )
    nY := nTop
    FOR n := 1 TO nCount
        s_aAreas[ n ][ AR_WIN ]:place( 0, nY, ht_ScreenMaxCol() + 1, IIF( n == nCount, nTop + nRows - nY, nH ) - 1 )
        nY += nH
    NEXT

RETURN

STATIC PROCEDURE CascadeWindows()

    LOCAL n

    FOR n := 1 TO Len( s_aAreas )
        CascadePlace( s_aAreas[ n ][ AR_WIN ], n )
    NEXT

RETURN

STATIC PROCEDURE CascadePlace( oWin, n )

    LOCAL nStep := ( n - 1 ) % 6
    LOCAL nW := Min( ht_ScreenMaxCol() + 1 - 12, 110 )
    LOCAL nH := Max( 10, ht_ScreenMaxRow() - 2 - 12 )

    oWin:place( 1 + nStep * 2, 2 + nStep * 2, nW, nH )

RETURN

STATIC PROCEDURE RepaintWindow( oWin )

    LOCAL nWin := wSelect()

    IF oWin:windowId != NIL
        oWin:repaint()
        wSelect( nWin, .F. )
    ENDIF

RETURN

/*---------------------------------------------------------------- status lines */

/* after every key / click in a browse */
STATIC PROCEDURE AfterMove()

    IF s_nArea > 0
        ShowAreaStatus( s_nArea )
        IF s_aAreas[ s_nArea ][ AR_LASTREC ] != RecNo()
            s_aAreas[ s_nArea ][ AR_LASTREC ] := RecNo()
            RefreshRelated( s_nArea )
        ENDIF
    ENDIF
    ShowStatLine()

RETURN

/* BroPlus Bp_ShowStat + Bp_IdleStat for the window of area n */
STATIC PROCEDURE ShowAreaStatus( n )

    LOCAL aArea := s_aAreas[ n ], nOld := Select(), aText, cKey, cRel := "", aRel, i
    LOCAL oTB := aArea[ AR_BRW ]:browse(), oWin := aArea[ AR_WIN ], cTitle

    dbSelectArea( aArea[ AR_ALIAS ] )

    FOR EACH aRel IN aArea[ AR_RELS ]
        cRel += IIF( cRel == "", "", "," ) + aRel[ REL_CHILD ]
    NEXT
    aText := { ;
        " Rec. " + hb_ntos( RecNo() ) + "/" + hb_ntos( LastRec() ) + " ", ;
        " <Lock: " + hb_ntos( oTB:freeze ) + "> ", ;
        " <" + IIF( dbInfo( DBI_SHARED ), "Shared", "Exclusive" ) + "> ", ;
        " <Softseek " + IIF( s_lSoftSeek, "On", "Off" ) + "> ", ;
        IIF( Empty( dbFilter() ), "", " <Filter>" ) + IIF( HasScope(), " <Scope>", "" ) + ;
            IIF( cRel == "", "", " <Rel: " + cRel + ">" ) + ;
            IIF( Set( _SET_DELETED ), " <Deleted On>", IIF( Deleted(), " <Deleted>", "" ) ) + " " }
    FOR i := 1 TO IDLE_SECTIONS
        IF ! aText[ i ] == aArea[ AR_IDLETXT ][ i ]
            aArea[ AR_IDLETXT ][ i ] := aText[ i ]
            aArea[ AR_IDLE ]:setSection( i, aText[ i ] )
        ENDIF
    NEXT

    cKey := ""
    IF IndexOrd() > 0
        cKey := " Key: " + OrdKey() + IIF( Empty( OrdFor() ), "", "  For: " + OrdFor() )
    ENDIF
    IF ! Empty( dbFilter() )
        cKey += "  Filter: " + dbFilter()
    ENDIF
    IF Eof() .AND. LastRec() == 0
        cKey += "  (empty - Ins appends a record)"
    ENDIF
    SetLabel( aArea[ AR_KEY ], cKey, Max( 1, oWin:width - 1 ) )

    cTitle := AreaTitle( n )
    IF ! cTitle == oWin:title()
        oWin:setWindowTitle( cTitle )
        RepaintWindow( oWin )
    ENDIF

    dbSelectArea( nOld )

RETURN

/* title of a database window (current area selected) */
STATIC FUNCTION AreaTitle( n )

    LOCAL cTitle := s_aAreas[ n ][ AR_ALIAS ] + "  (" + hb_ntos( n ) + "/" + hb_ntos( Len( s_aAreas ) ) + ")"
    LOCAL aArea, aRel

    IF IndexOrd() > 0
        cTitle += "  Order " + OrdName()
    ENDIF
    FOR EACH aArea IN s_aAreas
        FOR EACH aRel IN aArea[ AR_RELS ]
            IF aRel[ REL_CHILD ] == s_aAreas[ n ][ AR_ALIAS ]
                cTitle += "  " + ht_glyph( 0x2190, "<-" ) + " " + aArea[ AR_ALIAS ] + "->(" + aRel[ REL_EXPR ] + ")"
            ENDIF
        NEXT
    NEXT

RETURN cTitle

/* BroPlus Bp_StatLine: directory and edit mode, or the incremental seek */
STATIC PROCEDURE ShowStatLine()

    LOCAL aText, i

    IF s_lIncSeek .AND. s_nArea > 0
        aText := { " KEY: " + ht_glyph( 0x25BA, ">" ) + OrdKey() + ht_glyph( 0x25C4, "<" ) + "   SEEK: " + s_cSeek + " " }
    ELSE
        aText := { " Directory: " + hb_cwd() + " " }
    ENDIF
    AAdd( aText, IIF( s_lImmediate, " Immediate Edit ", " Protected Edit " ) + IIF( s_lModify, "", "- Modify DISABLED " ) )
    AAdd( aText, " F1-F9 menus  Ctrl+F6 next window  Alt-R relation " )

    FOR i := 1 TO Len( aText )
        IF ! aText[ i ] == s_aMsgTxt[ i ]
            s_aMsgTxt[ i ] := aText[ i ]
            s_oMsg:setSection( i, aText[ i ] )
        ENDIF
    NEXT

RETURN

/* HTLabel:setText() sizes the label to the text: keep the full width so a
   shorter text clears the old one */
STATIC PROCEDURE SetLabel( oLabel, cText, nWidth )

    LOCAL parent

    IF ! oLabel:text == cText
        oLabel:setText( cText )
        oLabel:setGeometry( oLabel:x, oLabel:y, nWidth, 1 )
        parent := oLabel:parent()
        IF parent != NIL .AND. parent:windowId != NIL
            parent:repaintChild( oLabel )
        ENDIF
    ENDIF

RETURN

STATIC FUNCTION HasScope()
RETURN IndexOrd() > 0 .AND. ( ordScope( 0 ) != NIL .OR. ordScope( 1 ) != NIL )

/* the current database's data changed or moved: browse, status, children */
STATIC PROCEDURE RefreshArea( n )

    IF n > 0 .AND. n <= Len( s_aAreas )
        s_aAreas[ n ][ AR_BRW ]:refreshAll()
        ShowAreaStatus( n )
        s_aAreas[ n ][ AR_LASTREC ] := ( s_aAreas[ n ][ AR_ALIAS ] )->( RecNo() )
        RefreshRelated( n )
    ENDIF

RETURN

STATIC PROCEDURE RefreshCur()
    RefreshArea( s_nArea )
RETURN

/*---------------------------------------------------------------- relations */

/* AreaSet > Relation: the current database becomes the parent */
STATIC PROCEDURE RelationDlg()

    LOCAL aNames := {}, aChild := {}, aArea, n, cChild, aAsk, nChild, aRel, lOnly

    FOR EACH aArea IN s_aAreas
        IF aArea:__enumIndex() != s_nArea
            AAdd( aNames, PadR( aArea[ AR_ALIAS ], 12 ) + ;
                  IIF( ( aArea[ AR_ALIAS ] )->( IndexOrd() ) > 0, "Order " + ( aArea[ AR_ALIAS ] )->( OrdName() ), "(no index)" ) )
            AAdd( aChild, aArea[ AR_ALIAS ] )
        ENDIF
    NEXT
    FOR EACH aRel IN s_aAreas[ s_nArea ][ AR_RELS ]
        AAdd( aNames, "Clear relation into " + aRel[ REL_CHILD ] )
        AAdd( aChild, "-" + aRel[ REL_CHILD ] )
    NEXT
    IF Empty( aNames )
        Info( "Open another database first (F2), then relate it to " + Alias() )
        RETURN
    ENDIF

    IF ( n := Choose( "Relation: " + Alias() + " into", aNames, 1 ) ) == 0
        RETURN
    ENDIF
    cChild := aChild[ n ]
    IF Left( cChild, 1 ) == "-"
        cChild := SubStr( cChild, 2 )
        s_aAreas[ s_nArea ][ AR_RELS ] := ADelWhere( s_aAreas[ s_nArea ][ AR_RELS ], {| r | r[ REL_CHILD ] == cChild } )
        ApplyRelations( s_nArea )
        ClearChildScope( cChild )
        RefreshArea( AreaByAlias( cChild ) )
        ShowAreaStatus( AreaByAlias( cChild ) )
        RefreshCur()
        RETURN
    ENDIF

    nChild := AreaByAlias( cChild )
    IF ( cChild )->( IndexOrd() ) == 0
        Warn( cChild + " needs an index order: the relation seeks the key in it (F3)" )
        RETURN
    ENDIF
    aAsk := { { "Key expression (in " + Alias() + "):", PadR( GuessRelKey( cChild ), 200 ), "@S40" }, ;
              { "Related records only (master-detail):", .T., "Y" } }
    IF ! Ask( "Relation " + Alias() + " -> " + cChild + "  (" + cChild + " key: " + ( cChild )->( OrdKey() ) + ")", aAsk )
        RETURN
    ENDIF
    lOnly := aAsk[ 2 ][ 2 ]
    IF ExprBlock( aAsk[ 1 ][ 2 ], ValType( ( cChild )->( KeySample() ) ) ) == NIL
        RETURN
    ENDIF
    SetRelation( s_nArea, cChild, AllTrim( aAsk[ 1 ][ 2 ] ), lOnly )
    ShowAreaStatus( nChild )
    RefreshCur()

RETURN

/* parent field with the name of the child's key, else the key itself */
STATIC FUNCTION GuessRelKey( cChild )

    LOCAL cKey := Upper( AllTrim( ( cChild )->( OrdKey() ) ) )

    IF FieldPos( cKey ) > 0
        RETURN cKey
    ENDIF
    IF FieldPos( "ID" ) > 0 .AND. "ID" $ cKey
        RETURN "ID"
    ENDIF

RETURN cKey

/* the child's current key value: gives the type the relation key needs */
STATIC FUNCTION KeySample()

    LOCAL xKey := ""

    Try( {|| xKey := Eval( hb_macroBlock( OrdKey() ) ) } )

RETURN xKey

STATIC PROCEDURE SetRelation( nParent, cChild, cExpr, lOnly )

    LOCAL aArea := s_aAreas[ nParent ]

    aArea[ AR_RELS ] := ADelWhere( aArea[ AR_RELS ], {| r | r[ REL_CHILD ] == cChild } )
    AAdd( aArea[ AR_RELS ], { cChild, cExpr, hb_macroBlock( cExpr ), lOnly } )
    ApplyRelations( nParent )
    aArea[ AR_LASTREC ] := 0
    RefreshRelated( nParent )

RETURN

/* SET RELATION of area n from its list (after a change or a reopen) */
STATIC PROCEDURE ApplyRelations( n )

    LOCAL aArea := s_aAreas[ n ], aRel

    ( aArea[ AR_ALIAS ] )->( dbClearRelation() )
    FOR EACH aRel IN aArea[ AR_RELS ]
        ( aArea[ AR_ALIAS ] )->( dbSetRelation( aRel[ REL_CHILD ], aRel[ REL_BLOCK ], aRel[ REL_EXPR ] ) )
    NEXT

RETURN

STATIC PROCEDURE ApplyAllRelations()

    LOCAL n

    FOR n := 1 TO Len( s_aAreas )
        ApplyRelations( n )
    NEXT

RETURN

/* the parent moved: scope the children to its key, repaint their windows */
STATIC PROCEDURE RefreshRelated( n )

    LOCAL aArea := s_aAreas[ n ], aRel, nChild, xKey

    FOR EACH aRel IN aArea[ AR_RELS ]
        nChild := AreaByAlias( aRel[ REL_CHILD ] )
        IF nChild == 0
            LOOP
        ENDIF
        IF aRel[ REL_ONLY ]
            xKey := ( aArea[ AR_ALIAS ] )->( Eval( aRel[ REL_BLOCK ] ) )
            ( aRel[ REL_CHILD ] )->( ordScope( 0, xKey ) )
            ( aRel[ REL_CHILD ] )->( ordScope( 1, xKey ) )
            ( aRel[ REL_CHILD ] )->( dbGoTop() )
        ENDIF
        s_aAreas[ nChild ][ AR_BRW ]:browse():rowPos := 1
        s_aAreas[ nChild ][ AR_BRW ]:refreshAll()
        ShowAreaStatus( nChild )
        s_aAreas[ nChild ][ AR_LASTREC ] := ( aRel[ REL_CHILD ] )->( RecNo() )
        RefreshRelated( nChild )
    NEXT

RETURN

STATIC PROCEDURE ClearChildScope( cChild )
    IF Select( cChild ) > 0
        ( cChild )->( ordScope( 0, NIL ) )
        ( cChild )->( ordScope( 1, NIL ) )
    ENDIF
RETURN

/*---------------------------------------------------------------- browse keys */

/* keys the browse does not do itself (BroPlus browse window keys) */
STATIC FUNCTION BrowseKey( nKey )

    LOCAL cText

    IF s_nArea == 0
        RETURN .F.
    ENDIF

    IF s_lIncSeek
        IF nKey == K_BS
            IncSeek( NIL )
            RETURN .T.
        ENDIF
        cText := ht_keyText( nKey )
        IF ! cText == ""
            IncSeek( cText )
            RETURN .T.
        ENDIF
        s_cSeek := ""
    ENDIF

    SWITCH nKey
    CASE K_ENTER
        EditCell()
        RETURN .T.
    CASE K_INS
        AppendRecord()
        RETURN .T.
    CASE K_DEL
        DeleteRecall()
        RETURN .T.
    CASE K_ALT_BS
        EraseMemo()
        RETURN .T.
    ENDSWITCH

    /* Protected Edit: typing does not start editing (Enter does) */
RETURN ! s_lImmediate .AND. ! ht_keyText( nKey ) == ""

/* AreaSet > Incremental Seek: typed characters seek in the index order */
STATIC PROCEDURE IncSeek( cChar )

    LOCAL nRec := RecNo(), cTry

    IF IndexOrd() == 0 .OR. ! ValType( KeySample() ) == "C"
        Toast( "Incremental Seek needs a character index order (F3)" )
        RETURN
    ENDIF
    cTry := IIF( cChar == NIL, Left( s_cSeek, Len( s_cSeek ) - 1 ), s_cSeek + cChar )
    IF "UPPER(" $ Upper( StrTran( OrdKey(), " ", "" ) )
        cTry := Upper( cTry )
    ENDIF
    IF cTry == ""
        s_cSeek := ""
        dbGoTop()
    ELSEIF dbSeek( cTry, .F. )
        s_cSeek := cTry
    ELSE
        dbGoto( nRec )
        Tone( 87.3, 1 )
    ENDIF
    RefreshCur()

RETURN

STATIC PROCEDURE ToggleIncSeek()
    s_lIncSeek := ! s_lIncSeek
    s_cSeek := ""
RETURN

/* Enter: edit the field (a memo in the memo window) */
STATIC PROCEDURE EditCell()

    LOCAL oCol := CurColumn()

    IF oCol != NIL
        IF oCol:cargo[ 2 ] == "M"
            EditMemo( oCol:cargo[ 1 ] )
        ELSE
            s_aAreas[ s_nArea ][ AR_BRW ]:beginEdit( 0 )
        ENDIF
    ENDIF

RETURN

/* onBeginEdit: may this cell be edited inline? */
STATIC FUNCTION CanEdit( nRow, nCol )

    LOCAL oCol := CurTB():getColumn( nCol )

    HB_SYMBOL_UNUSED( nRow )

    IF oCol:cargo[ 2 ] == "M"
        EditMemo( oCol:cargo[ 1 ] )
        RETURN .F.
    ENDIF
    IF ! s_lModify
        Toast( "Modify is DISABLED (F6 GlobalSet)" )
        RETURN .F.
    ENDIF
    IF ! IsEditable( oCol:cargo[ 2 ] ) .OR. ! oCol:cargo[ 3 ] == Alias()
        Toast( "This column cannot be edited" )
        RETURN .F.
    ENDIF
    IF Eof()
        Toast( "No record - Ins appends one" )
        RETURN .F.
    ENDIF

RETURN .T.

STATIC PROCEDURE EditMemo( cField )

    LOCAL nPos := FieldPos( cField ), dlg, oEdit, nW, nH, cText

    IF nPos == 0 .OR. Eof()
        RETURN
    ENDIF
    nW := Min( 84, ht_ScreenMaxCol() - 6 )
    nH := Min( 26, ht_ScreenMaxRow() - 4 )
    dlg := HTDialog():new( NIL, Alias() + "->" + cField + "  Record " + hb_ntos( RecNo() ) )
    dlg:resize( nW, nH )
    oEdit := HTTextEdit():new( dlg )
    oEdit:setGeometry( 1, 1, nW - 4, nH - 7 )
    oEdit:text := FieldGet( nPos )
    IF s_lModify
        dlg:addButtonBar( "Save", "Cancel" )
    ELSE
        oEdit:_readOnly( .T. )
        dlg:addButtonBar( "Close" )
    ENDIF
    IF dlg:exec() == HT_DIALOG_ACCEPTED .AND. s_lModify
        cText := oEdit:text
        PutField( Alias(), nPos, cText )
        RefreshCur()
    ENDIF

RETURN

STATIC PROCEDURE EraseMemo()

    LOCAL oCol := CurColumn()

    IF oCol == NIL .OR. ! oCol:cargo[ 2 ] == "M" .OR. Eof()
        Toast( "Move to a memo column first" )
    ELSEIF ! s_lModify
        Toast( "Modify is DISABLED (F6 GlobalSet)" )
    ELSEIF Confirm( "Erase memo " + oCol:cargo[ 1 ] + " of record " + hb_ntos( RecNo() ) + "?" )
        PutField( Alias(), FieldPos( oCol:cargo[ 1 ] ), "" )
        RefreshCur()
    ENDIF

RETURN

/*---------------------------------------------------------------- record */

/* Alt-A all fields / Alt-E the fields of the browse */
STATIC PROCEDURE EditRecord( lSelected )

    LOCAL aStruct := dbStruct(), aEdit := {}, aItem, oTB := CurTB(), oCol, n, nPos
    LOCAL nLabel := 1, nInput := 1, nPerCol, nCols, nColW, nShown, dlg, oGet, lChanged := .F.

    IF Eof()
        Info( "No current record - Ins appends one" )
        RETURN
    ENDIF
    IF lSelected
        FOR n := 1 TO oTB:colCount
            oCol := oTB:getColumn( n )
            IF ! oCol:cargo[ 2 ] == "E" .AND. oCol:cargo[ 3 ] == Alias() .AND. ( nPos := FieldPos( oCol:cargo[ 1 ] ) ) > 0
                AAdd( aEdit, nPos )
            ENDIF
        NEXT
    ELSE
        FOR n := 1 TO Len( aStruct )
            AAdd( aEdit, n )
        NEXT
    ENDIF
    /* { name, value, picture, field number, lEditable } - no memos here */
    FOR n := Len( aEdit ) TO 1 STEP -1
        nPos := aEdit[ n ]
        IF aStruct[ nPos ][ 2 ] $ "MWGP"
            hb_ADel( aEdit, n, .T. )
        ELSE
            aEdit[ n ] := { aStruct[ nPos ][ 1 ], FieldGet( nPos ), FieldPicture( aStruct[ nPos ] ), nPos, ;
                            s_lModify .AND. IsEditable( aStruct[ nPos ][ 2 ] ) }
            nLabel := Max( nLabel, Len( aStruct[ nPos ][ 1 ] ) + 1 )
            nInput := Max( nInput, FieldWidth( aStruct[ nPos ] ) )
        ENDIF
    NEXT
    IF Empty( aEdit )
        Info( "No fields to show (memo fields: Enter on the memo column)" )
        RETURN
    ENDIF

    nPerCol := Max( 1, Min( Len( aEdit ), ht_ScreenMaxRow() - 9 ) )
    nColW   := nLabel + nInput + 2
    nCols   := Min( Int( ( Len( aEdit ) - 1 ) / nPerCol ) + 1, Max( 1, Int( ( ht_ScreenMaxCol() - 6 ) / nColW ) ) )
    nShown  := Min( Len( aEdit ), nCols * nPerCol )

    dlg := HTDialog():new( NIL, Alias() + "  Record " + hb_ntos( RecNo() ) + "/" + hb_ntos( LastRec() ) + ;
                           IIF( Deleted(), "  <Deleted>", "" ) )
    dlg:resize( Max( 40, nCols * nColW + 4 ), Min( nPerCol, nShown ) + 6 )
    FOR n := 1 TO nShown
        aItem := aEdit[ n ]
        oGet := HTGet():new( dlg )
        oGet:setup( ElementBlock( aItem ), aItem[ 1 ], aItem[ 3 ], NIL, NIL, "", NIL, ! aItem[ 5 ], .F., nLabel + nInput )
        oGet:labelWidth := nLabel
        oGet:move( 2 + Int( ( n - 1 ) / nPerCol ) * nColW, 1 + ( n - 1 ) % nPerCol )
    NEXT
    IF s_lModify
        dlg:addButtonBar( "Save", "Cancel" )
    ELSE
        dlg:addButtonBar( "Close" )
    ENDIF
    IF nShown < Len( aEdit )
        Toast( hb_ntos( Len( aEdit ) - nShown ) + " fields do not fit - Alt-E edits the browse fields" )
    ENDIF

    IF dlg:exec() == HT_DIALOG_ACCEPTED .AND. s_lModify
        FOR n := 1 TO nShown
            IF aEdit[ n ][ 5 ] .AND. ! aEdit[ n ][ 2 ] == FieldGet( aEdit[ n ][ 4 ] )
                lChanged := .T.
            ENDIF
        NEXT
        IF lChanged
            IF RLock()
                FOR n := 1 TO nShown
                    IF aEdit[ n ][ 5 ]
                        FieldPut( aEdit[ n ][ 4 ], aEdit[ n ][ 2 ] )
                    ENDIF
                NEXT
                dbCommit()
                dbUnlock()
            ELSE
                Warn( "Record is currently locked by another user" )
            ENDIF
        ENDIF
        RefreshCur()
    ENDIF

RETURN

STATIC FUNCTION FieldPicture( aField )

    SWITCH aField[ 2 ]
    CASE "C"
        RETURN IIF( aField[ 3 ] > 40, "@S40", "" )
    CASE "N"
        RETURN NumPicture( aField[ 3 ], aField[ 4 ] )
    CASE "D"
        RETURN "@D"
    CASE "L"
        RETURN "Y"
    ENDSWITCH

RETURN ""

STATIC FUNCTION FieldWidth( aField )

    SWITCH aField[ 2 ]
    CASE "C"
        RETURN Min( aField[ 3 ], 40 )
    CASE "N"
        RETURN aField[ 3 ]
    CASE "D"
        RETURN Len( DToC( Date() ) )
    CASE "L"
        RETURN 1
    ENDSWITCH

RETURN 24

STATIC PROCEDURE AppendRecord()

    IF ! s_lModify
        Toast( "Modify is DISABLED (F6 GlobalSet)" )
        RETURN
    ENDIF
    dbAppend()
    IF NetErr()
        Warn( "Append failed: the file is locked by another user" )
        RETURN
    ENDIF
    dbCommit()
    dbUnlock()
    RefreshCur()
    EditRecord( .F. )

RETURN

STATIC PROCEDURE DeleteRecall()

    IF Eof()
        RETURN
    ENDIF
    IF ! s_lModify
        Toast( "Modify is DISABLED (F6 GlobalSet)" )
        RETURN
    ENDIF
    IF RLock()
        IF Deleted()
            dbRecall()
        ELSE
            dbDelete()
        ENDIF
        dbCommit()
        dbUnlock()
        s_aAreas[ s_nArea ][ AR_BRW ]:refreshCurrent()
        ShowAreaStatus( s_nArea )
    ELSE
        Warn( "Record is currently locked by another user" )
    ENDIF

RETURN

STATIC PROCEDURE SeekDlg()

    LOCAL aAsk, nRec := RecNo()

    IF IndexOrd() == 0
        Warn( "Seek needs an index order (F3 Index)" )
        RETURN
    ENDIF
    aAsk := { { "Seek " + Left( OrdKey(), 30 ) + ":", PadR( s_cLastSeek, 60 ), "@S40" }, ;
              { "Softseek:", s_lSoftSeek, "Y" } }
    IF Ask( "Seek - Order " + OrdName(), aAsk )
        s_cLastSeek := AllTrim( aAsk[ 1 ][ 2 ] )
        IF ! dbSeek( KeyValue( aAsk[ 1 ][ 2 ] ), aAsk[ 2 ][ 2 ] ) .AND. Eof()
            dbGoto( nRec )
            Info( "Not found: " + s_cLastSeek )
        ENDIF
        RefreshCur()
    ENDIF

RETURN

STATIC PROCEDURE GotoDlg()

    LOCAL aAsk := { { "Record number:", RecNo(), "9999999999" } }

    IF Ask( "Goto - " + hb_ntos( LastRec() ) + " records", aAsk )
        IF aAsk[ 1 ][ 2 ] < 1 .OR. aAsk[ 1 ][ 2 ] > LastRec()
            Warn( "Record number out of range" )
        ELSE
            dbGoto( aAsk[ 1 ][ 2 ] )
            RefreshCur()
        ENDIF
    ENDIF

RETURN

STATIC PROCEDURE LocateDlg()

    LOCAL aAsk := { { "Locate FOR:", PadR( s_cLocate, 200 ), "@S50" }, ;
                    { "Continue from the current record:", ! Empty( s_cLocate ), "Y" } }
    LOCAL bFor, nRec := RecNo()

    IF ! Ask( "Locate", aAsk ) .OR. ( bFor := ExprBlock( aAsk[ 1 ][ 2 ], "L" ) ) == NIL
        RETURN
    ENDIF
    s_cLocate := AllTrim( aAsk[ 1 ][ 2 ] )
    IF aAsk[ 2 ][ 2 ]
        dbSkip()
    ELSE
        dbGoTop()
    ENDIF
    DO WHILE ! Eof() .AND. ! Eval( bFor )
        dbSkip()
    ENDDO
    IF Eof()
        dbGoto( nRec )
        Info( "End of file reached - no record for " + s_cLocate )
    ENDIF
    RefreshCur()

RETURN

STATIC PROCEDURE SkipDlg()

    LOCAL aAsk := { { "Skip records (+/-):", 1, "9999999" } }

    IF Ask( "Skip", aAsk )
        dbSkip( aAsk[ 1 ][ 2 ] )
        IF Eof()
            dbGoBottom()
        ELSEIF Bof()
            dbGoTop()
        ENDIF
        RefreshCur()
    ENDIF

RETURN

/* Alt-T / Alt-B: first / last row of the window */
STATIC PROCEDURE WindowEdge( lTop )

    LOCAL oTB := CurTB(), n

    IF lTop
        FOR n := 2 TO oTB:rowPos
            oTB:up()
        NEXT
    ELSE
        FOR n := oTB:rowPos + 1 TO oTB:rowCount
            oTB:down()
        NEXT
    ENDIF
    s_aAreas[ s_nArea ][ AR_WIN ]:repaintChild( s_aAreas[ s_nArea ][ AR_BRW ] )
    AfterMove()

RETURN

STATIC PROCEDURE CopyField()

    LOCAL oCol := CurColumn()

    IF oCol != NIL .AND. ! Eof()
        s_xClipCell := Eval( oCol:block )
        Toast( "Copied: " + Left( AllTrim( hb_CStr( s_xClipCell ) ), 40 ) )
    ENDIF

RETURN

STATIC PROCEDURE PasteField()

    LOCAL oCol := CurColumn()

    IF s_xClipCell == NIL .OR. oCol == NIL
        Toast( "Clipboard is empty - Shift-F4 copies a field" )
    ELSEIF ! s_lModify .OR. ! IsEditable( oCol:cargo[ 2 ] ) .OR. Eof()
        Toast( "This field cannot be changed" )
    ELSEIF ! ValType( Eval( oCol:block ) ) == ValType( s_xClipCell )
        Warn( "Type mismatch: the clipboard holds " + ValType( s_xClipCell ) + ", the field is " + ValType( Eval( oCol:block ) ) )
    ELSE
        Eval( oCol:block, s_xClipCell )
        RefreshCur()
    ENDIF

RETURN

STATIC PROCEDURE CopyRecord()

    LOCAL n

    IF Eof()
        RETURN
    ENDIF
    s_aClipRec := {}
    FOR n := 1 TO FCount()
        AAdd( s_aClipRec, { FieldName( n ), FieldGet( n ) } )
    NEXT
    Toast( "Record " + hb_ntos( RecNo() ) + " copied" )

RETURN

/* fields with the same name and type get the copied values */
STATIC PROCEDURE PasteRecord()

    LOCAL aField, nPos, nDone := 0

    IF s_aClipRec == NIL
        Toast( "Clipboard is empty - Shift-F7 copies a record" )
        RETURN
    ENDIF
    IF ! s_lModify .OR. Eof()
        Toast( "Modify is DISABLED or no current record" )
        RETURN
    ENDIF
    IF ! Confirm( "Paste the copied record over record " + hb_ntos( RecNo() ) + "?" )
        RETURN
    ENDIF
    IF ! RLock()
        Warn( "Record is currently locked by another user" )
        RETURN
    ENDIF
    FOR EACH aField IN s_aClipRec
        nPos := FieldPos( aField[ 1 ] )
        IF nPos > 0 .AND. ValType( FieldGet( nPos ) ) == ValType( aField[ 2 ] ) .AND. ;
           ( IsEditable( dbStruct()[ nPos ][ 2 ] ) .OR. dbStruct()[ nPos ][ 2 ] == "M" )
            FieldPut( nPos, aField[ 2 ] )
            nDone++
        ENDIF
    NEXT
    dbCommit()
    dbUnlock()
    RefreshCur()
    Toast( hb_ntos( nDone ) + " fields pasted" )

RETURN

STATIC PROCEDURE ViewClipboard()

    LOCAL aLines := { "Field:  " + IIF( s_xClipCell == NIL, "(empty)", hb_ValToExp( s_xClipCell ) ), "", "Record:" }
    LOCAL aField

    IF s_aClipRec == NIL
        AAdd( aLines, "  (empty)" )
    ELSE
        FOR EACH aField IN s_aClipRec
            AAdd( aLines, "  " + PadR( aField[ 1 ], 11 ) + Left( hb_ValToExp( aField[ 2 ] ), 60 ) )
        NEXT
    ENDIF
    ShowText( "Clipboard", aLines )

RETURN

/*---------------------------------------------------------------- index */

STATIC FUNCTION OrderCount()

    LOCAL n := 0

    DO WHILE ! Empty( OrdName( n + 1 ) )
        n++
    ENDDO

RETURN n

STATIC PROCEDURE NextOrder( nStep )

    LOCAL nCount := OrderCount(), n

    IF nCount == 0
        Toast( "No index open - Alt-F3 opens one, Index > Create makes one" )
        RETURN
    ENDIF
    n := IndexOrd() + nStep
    IF n > nCount
        n := 0
    ELSEIF n < 0
        n := nCount
    ENDIF
    ordSetFocus( n )
    RefreshCur()

RETURN

/* Ctrl-F3: tags with key, FOR and index file */
STATIC PROCEDURE ChooseOrder()

    LOCAL aItems := { "Natural Order" }, n

    FOR n := 1 TO OrderCount()
        AAdd( aItems, PadR( OrdName( n ), 11 ) + PadR( OrdKey( n ), 30 ) + " " + ;
              hb_FNameNameExt( ordBagName( n ) ) + IIF( Empty( OrdFor( n ) ), "", "  For " + OrdFor( n ) ) )
    NEXT
    IF ( n := Choose( "Tag Selection - " + Alias(), aItems, IndexOrd() + 1 ) ) > 0
        ordSetFocus( n - 1 )
        RefreshCur()
    ENDIF

RETURN

STATIC PROCEDURE ScopeDlg()

    LOCAL aAsk, xTop, xBottom

    IF IndexOrd() == 0
        Warn( "Index Scope needs an index order (F3 Index)" )
        RETURN
    ENDIF
    xTop := ordScope( 0 )
    xBottom := ordScope( 1 )
    aAsk := { { "Scope top:", PadR( IIF( xTop == NIL, "", hb_CStr( xTop ) ), 60 ), "@S30" }, ;
              { "Scope bottom:", PadR( IIF( xBottom == NIL, "", hb_CStr( xBottom ) ), 60 ), "@S30" } }
    IF Ask( "Index Scope - " + OrdKey() + "  (empty = none)", aAsk )
        ordScope( 0, IIF( Empty( aAsk[ 1 ][ 2 ] ), NIL, KeyValue( aAsk[ 1 ][ 2 ] ) ) )
        ordScope( 1, IIF( Empty( aAsk[ 2 ][ 2 ] ), NIL, KeyValue( aAsk[ 2 ][ 2 ] ) ) )
        dbGoTop()
        RefreshCur()
    ENDIF

RETURN

/* typed text as a value of the index key's type */
STATIC FUNCTION KeyValue( cText )

    SWITCH ValType( KeySample() )
    CASE "N"
        RETURN Val( cText )
    CASE "D"
        RETURN CToD( AllTrim( cText ) )
    CASE "L"
        RETURN Upper( Left( AllTrim( cText ), 1 ) ) $ "TY"
    ENDSWITCH
    cText := RTrim( cText )
    IF "UPPER(" $ Upper( StrTran( OrdKey(), " ", "" ) )
        cText := Upper( cText )
    ENDIF

RETURN cText

STATIC PROCEDURE OpenIndexFile()

    LOCAL cFile := FileDialog( "Index File Selection", IIF( s_aAreas[ s_nArea ][ AR_DRIVER ] == "DBFNTX", "*.ntx", "*.cdx" ) )

    IF ! Empty( cFile ) .AND. AddIndexFile( cFile )
        RefreshCur()
    ENDIF

RETURN

STATIC FUNCTION AddIndexFile( cFile )

    LOCAL cErr, n

    cFile := FullPath( cFile )
    IF Empty( hb_FNameExt( cFile ) )
        cFile += IIF( s_aAreas[ s_nArea ][ AR_DRIVER ] == "DBFNTX", ".ntx", ".cdx" )
    ENDIF
    cErr := Try( {|| ordListAdd( cFile ) } )
    IF ! Empty( cErr )
        Warn( hb_FNameNameExt( cFile ) + ": " + cErr )
        RETURN .F.
    ENDIF
    AAdd( s_aAreas[ s_nArea ][ AR_BAGS ], cFile )
    FOR n := 1 TO OrderCount()
        IF Upper( hb_FNameName( ordBagName( n ) ) ) == Upper( hb_FNameName( cFile ) )
            ordSetFocus( n )
            EXIT
        ENDIF
    NEXT
    ShowAreaStatus( s_nArea )

RETURN .T.

STATIC PROCEDURE CreateIndexDlg()

    LOCAL aAsk := { { "Tag name:", Space( 10 ), "@!" }, ;
                    { "Key expression:", Space( 200 ), "@S45" }, ;
                    { "FOR condition:", Space( 200 ), "@S45" }, ;
                    { "Index file (empty = production):", Space( 120 ), "@S30" }, ;
                    { "Unique:", .F., "Y" } }
    LOCAL cTag, cKey, cFor, cBag, aBags := s_aAreas[ s_nArea ][ AR_BAGS ]

    IF ! Ask( "Create Index - " + Alias(), aAsk )
        RETURN
    ENDIF
    cTag := AllTrim( aAsk[ 1 ][ 2 ] )
    cKey := AllTrim( aAsk[ 2 ][ 2 ] )
    cFor := AllTrim( aAsk[ 3 ][ 2 ] )
    cBag := AllTrim( aAsk[ 4 ][ 2 ] )
    IF Empty( cTag ) .OR. Empty( cKey )
        Warn( "Tag name and key expression are required" )
        RETURN
    ENDIF
    IF ExprBlock( cKey ) == NIL .OR. ( ! Empty( cFor ) .AND. ExprBlock( cFor, "L" ) == NIL )
        RETURN
    ENDIF
    IF ! Empty( cBag )
        cBag := FullPath( cBag )
        AAdd( aBags, cBag )
    ENDIF
    IF WithExclusive( {|| CreateTag( cTag, cKey, cFor, aAsk[ 5 ][ 2 ], IIF( Empty( cBag ), NIL, cBag ) ) } )
        ordSetFocus( cTag )
        Toast( "Index " + cTag + " created" )
    ELSEIF ! Empty( cBag )
        ASize( aBags, Len( aBags ) - 1 )
    ENDIF
    RefreshCur()

RETURN

STATIC PROCEDURE CreateTag( cTag, cKey, cFor, lUnique, cBag )
    IF ! Empty( cFor )
        ordCondSet( cFor, hb_macroBlock( cFor ) )
    ENDIF
    ordCreate( cBag, cTag, cKey, hb_macroBlock( cKey ), lUnique )
RETURN

/*---------------------------------------------------------------- fields */

STATIC PROCEDURE SelectFields()

    LOCAL oTB := CurTB(), aStruct := dbStruct(), aShown := {}, aKeep := {}, n, dlg, oList, oCol, aField, nRows

    FOR n := 1 TO oTB:colCount
        AAdd( aShown, oTB:getColumn( n ) )
    NEXT
    nRows := Max( 3, Min( Len( aStruct ), ht_ScreenMaxRow() - 10 ) )
    dlg := HTDialog():new( NIL, "Field Selection - " + Alias() )
    dlg:resize( 44, nRows + 6 )
    oList := HTCheckList():new( dlg )
    oList:setGeometry( 1, 1, 40, nRows )
    FOR EACH aField IN aStruct
        oList:addItem( PadR( aField[ 1 ], 12 ) + aField[ 2 ] + Str( aField[ 3 ], 5 ) + ;
                       IIF( aField[ 4 ] > 0, "," + hb_ntos( aField[ 4 ] ), "" ), ;
                       AScan( aShown, {| o | ! o:cargo[ 2 ] == "E" .AND. o:cargo[ 1 ] == aField[ 1 ] } ) > 0 )
    NEXT
    dlg:addButtonBar( "OK", "Cancel" )
    IF dlg:exec() != HT_DIALOG_ACCEPTED
        RETURN
    ENDIF
    IF oList:checkedCount() == 0
        Warn( "Select at least one field" )
        RETURN
    ENDIF

    /* shown columns keep their order and options; new fields go last */
    FOR EACH oCol IN aShown
        n := AScan( aStruct, {| a | a[ 1 ] == oCol:cargo[ 1 ] } )
        IF oCol:cargo[ 2 ] == "E" .OR. ( n > 0 .AND. oList:isChecked( n ) )
            AAdd( aKeep, oCol )
        ENDIF
    NEXT
    FOR n := 1 TO Len( aStruct )
        IF oList:isChecked( n ) .AND. AScan( aKeep, {| o | ! o:cargo[ 2 ] == "E" .AND. o:cargo[ 1 ] == aStruct[ n ][ 1 ] } ) == 0
            AAdd( aKeep, FieldColumn( Alias(), aStruct[ n ] ) )
        ENDIF
    NEXT
    DO WHILE oTB:colCount > 0
        oTB:delColumn( 1 )
    ENDDO
    AEval( aKeep, {| o | oTB:addColumn( o ) } )
    oTB:freeze := Min( oTB:freeze, oTB:colCount - 1 )
    oTB:colPos := 1
    RefreshCur()

RETURN

STATIC PROCEDURE CutColumn()

    LOCAL oTB := CurTB()

    IF oTB:colCount < 2
        Toast( "The last column stays" )
        RETURN
    ENDIF
    s_oCutCol := oTB:delColumn( oTB:colPos )
    oTB:freeze := Min( oTB:freeze, oTB:colCount - 1 )
    RefreshCur()

RETURN

STATIC PROCEDURE PasteColumn()

    LOCAL oTB := CurTB()

    IF s_oCutCol == NIL
        Toast( "No column cut - Shift-F5 cuts one" )
    ELSEIF ! s_oCutCol:cargo[ 3 ] == Alias()
        Warn( "The cut column belongs to " + s_oCutCol:cargo[ 3 ] )
    ELSE
        oTB:insColumn( Max( 1, oTB:colPos ), s_oCutCol )
        s_oCutCol := NIL
        RefreshCur()
    ENDIF

RETURN

/* Alt-F5: a field or any expression as a new column */
STATIC PROCEDURE InsertColumn()

    LOCAL aStruct := dbStruct(), aNames := {}, aField, n, aAsk, bExpr, oCol, oTB := CurTB(), cExpr

    FOR EACH aField IN aStruct
        AAdd( aNames, PadR( aField[ 1 ], 12 ) + aField[ 2 ] + Str( aField[ 3 ], 5 ) )
    NEXT
    AAdd( aNames, "<Expression...>" )
    IF ( n := Choose( "Insert Column", aNames, 1 ) ) == 0
        RETURN
    ENDIF
    IF n <= Len( aStruct )
        oCol := FieldColumn( Alias(), aStruct[ n ] )
    ELSE
        aAsk := { { "Expression:", Space( 200 ), "@S45" }, { "Heading:", Space( 20 ), "" } }
        IF ! Ask( "Insert Column", aAsk ) .OR. ( bExpr := ExprBlock( aAsk[ 1 ][ 2 ] ) ) == NIL
            RETURN
        ENDIF
        cExpr := AllTrim( aAsk[ 1 ][ 2 ] )
        oCol := TBColumnNew( IIF( Empty( aAsk[ 2 ][ 2 ] ), cExpr, AllTrim( aAsk[ 2 ][ 2 ] ) ), bExpr )
        oCol:cargo := { cExpr, "E", Alias() }
    ENDIF
    oTB:insColumn( Max( 1, oTB:colPos ), oCol )
    RefreshCur()

RETURN

STATIC PROCEDURE LockFields()

    LOCAL oTB := CurTB(), aAsk := { { "Lock # of Fields:", oTB:freeze, "99" } }

    IF Ask( "Lock Fields", aAsk )
        oTB:freeze := Max( 0, Min( aAsk[ 1 ][ 2 ], oTB:colCount - 1 ) )
        RefreshCur()
    ENDIF

RETURN

/* Alt-H: heading, width and picture of the current column */
STATIC PROCEDURE ColumnOptions()

    LOCAL oCol := CurColumn(), aAsk

    IF oCol == NIL
        RETURN
    ENDIF
    aAsk := { { "Heading:", PadR( oCol:heading, 40 ), "@S30" }, ;
              { "Width:", IIF( HB_ISNUMERIC( oCol:width ), oCol:width, 0 ), "999" }, ;
              { "Picture:", PadR( hb_defaultValue( oCol:picture, "" ), 40 ), "@S30" } }
    IF Ask( "Column Options - " + oCol:cargo[ 1 ] + "  (width 0 = automatic)", aAsk )
        oCol:heading := AllTrim( aAsk[ 1 ][ 2 ] )
        oCol:width := IIF( aAsk[ 2 ][ 2 ] > 0, aAsk[ 2 ][ 2 ], NIL )
        oCol:picture := IIF( Empty( aAsk[ 3 ][ 2 ] ), NIL, AllTrim( aAsk[ 3 ][ 2 ] ) )
        CurTB():configure()
        RefreshCur()
    ENDIF

RETURN

/*---------------------------------------------------------------- areaset */

STATIC PROCEDURE FilterDlg()

    LOCAL aAsk := { { "Filter:", PadR( dbFilter(), 200 ), "@S50" } }, bFor

    IF ! Ask( "Range & Filter - " + Alias() + "  (empty = none)", aAsk )
        RETURN
    ENDIF
    IF Empty( aAsk[ 1 ][ 2 ] )
        dbClearFilter()
    ELSEIF ( bFor := ExprBlock( aAsk[ 1 ][ 2 ], "L" ) ) == NIL
        RETURN
    ELSE
        dbSetFilter( bFor, AllTrim( aAsk[ 1 ][ 2 ] ) )
    ENDIF
    dbGoTop()
    RefreshCur()

RETURN

/*---------------------------------------------------------------- database */

STATIC PROCEDURE SelectDatabase()

    LOCAL cFile := FileDialog( "Database Selection", "*.dbf" )

    IF ! Empty( cFile )
        OpenTable( cFile )
    ENDIF

RETURN

STATIC PROCEDURE ChangeDirectory()

    LOCAL aAsk := { { "Directory:", PadR( hb_cwd(), 200 ), "@S50" } }, cDir

    IF Ask( "Change Directory", aAsk )
        cDir := AllTrim( aAsk[ 1 ][ 2 ] )
        IF hb_DirExists( cDir )
            hb_cwd( cDir )
        ELSE
            Warn( cDir + ": directory not found" )
        ENDIF
    ENDIF

RETURN

/* Alt-X: reopen the current database SHARED / EXCLUSIVE */
STATIC PROCEDURE ToggleOpenMode()

    LOCAL lShared := dbInfo( DBI_SHARED )

    IF ! Reopen( lShared ) .AND. lShared
        Warn( Alias() + " is in use - cannot open it EXCLUSIVE" )
    ENDIF
    RefreshCur()

RETURN

/* closes and opens the current database again, keeping index files, order,
   filter, record and relations; .T. when opened in the wanted mode */
STATIC FUNCTION Reopen( lExclusive )

    LOCAL aArea := s_aAreas[ s_nArea ], cAlias := aArea[ AR_ALIAS ]
    LOCAL nRec := RecNo(), cOrder := OrdName(), cFilter := dbFilter(), cBag, lOk
    LOCAL xTop := ordScope( 0 ), xBottom := ordScope( 1 )

    dbCloseArea()
    Try( {|| dbUseArea( .F., aArea[ AR_DRIVER ], aArea[ AR_FILE ], cAlias, ! lExclusive ) } )
    lOk := Used()
    IF ! lOk
        Try( {|| dbUseArea( .F., aArea[ AR_DRIVER ], aArea[ AR_FILE ], cAlias, .T. ) } )
        IF ! Used()
            Warn( aArea[ AR_FILE ] + " cannot be opened again" )
            aArea[ AR_WIN ]:destroy()
            CloseArea( s_nArea )
            RETURN .F.
        ENDIF
    ENDIF
    FOR EACH cBag IN aArea[ AR_BAGS ]
        Try( {|| ordListAdd( cBag ) } )
    NEXT
    IF ! Empty( cOrder )
        ordSetFocus( cOrder )
        ordScope( 0, xTop )
        ordScope( 1, xBottom )
    ENDIF
    IF ! Empty( cFilter )
        dbSetFilter( hb_macroBlock( cFilter ), cFilter )
    ENDIF
    dbGoto( nRec )
    ApplyAllRelations()

RETURN lOk

/* runs bAction with the current database EXCLUSIVE (PACK, ZAP, INDEX ON) */
STATIC FUNCTION WithExclusive( bAction )

    LOCAL lShared := dbInfo( DBI_SHARED ), cErr, nArea := s_nArea

    IF lShared .AND. ! Reopen( .T. )
        IF s_nArea == nArea
            Warn( Alias() + " is in use by another program - it cannot be opened EXCLUSIVE" )
        ENDIF
        RETURN .F.
    ENDIF
    cErr := Try( bAction )
    IF lShared
        Reopen( .F. )
    ENDIF
    IF ! Empty( cErr )
        Warn( cErr )
    ENDIF

RETURN Empty( cErr )

/* Alt-M: structure editor; the current file is modified (old data kept in
   <name>_bak), any other name creates a new database */
STATIC PROCEDURE CreateModify()

    LOCAL aStru := {}, aField, aRes, aNew, cFile, cErr, nOpen

    IF s_nArea > 0
        cFile := s_aAreas[ s_nArea ][ AR_FILE ]
        FOR EACH aField IN dbStruct()
            AAdd( aStru, { PadR( aField[ 1 ], 10 ), aField[ 2 ], aField[ 3 ], aField[ 4 ] } )
        NEXT
    ELSE
        cFile := hb_cwd() + "new.dbf"
        AAdd( aStru, { PadR( "NAME", 10 ), "C", 30, 0 } )
    ENDIF

    IF ( aRes := StructEditor( cFile, aStru ) ) == NIL .OR. ( aNew := ValidStruct( aRes[ 2 ] ) ) == NIL
        RETURN
    ENDIF
    cFile := FullPath( DbfName( aRes[ 1 ] ) )
    nOpen := AScan( s_aAreas, {| a | Upper( a[ AR_FILE ] ) == Upper( cFile ) } )
    IF nOpen > 0 .AND. nOpen == s_nArea
        ModifyStructure( aNew )
    ELSEIF nOpen > 0
        Warn( hb_FNameNameExt( cFile ) + " is open in another window - select it first (Alt-N)" )
    ELSEIF ! hb_FileExists( cFile ) .OR. Confirm( hb_FNameNameExt( cFile ) + " exists - overwrite it?" )
        cErr := Try( {|| dbCreate( cFile, aNew, s_cDriver ) } )
        IF ! Empty( cErr )
            Warn( "Create failed: " + cErr )
        ELSEIF Confirm( hb_FNameNameExt( cFile ) + " created - open it?" )
            OpenTable( cFile )
        ENDIF
    ENDIF

RETURN

STATIC FUNCTION StructEditor( cFile, aStru )

    LOCAL aRec := { 1 }, nH, dlg, oGet, brw, btn, nBtn

    cFile := PadR( cFile, 200 )
    nH := Max( 20, Min( 28, ht_ScreenMaxRow() - 4 ) )
    dlg := HTDialog():new( NIL, "Create/Modify Structure" )
    dlg:resize( 56, nH )

    oGet := HTGet():new( dlg )
    oGet:setup( {| x | IIF( x == NIL, cFile, cFile := x ) }, "File:", "@S44", NIL, NIL, "", NIL, .F., .F., 50 )
    oGet:move( 2, 1 )

    brw := HTBrowse():new( dlg )
    brw:setGeometry( 2, 3, 32, nH - 6 )
    brw:setGoTopBlock( {|| aRec[ 1 ] := 1 } )
    brw:setGoBottomBlock( {|| aRec[ 1 ] := Len( aStru ) } )
    brw:setSkipBlock( {| n | ArraySkip( aRec, Len( aStru ), n ) } )
    brw:addColumn( "Name", StruBlock( aStru, aRec, 1 ), 10, "@!" )
    brw:addColumn( "Type", StruBlock( aStru, aRec, 2 ), 4, "!" )
    brw:addColumn( "Len",  StruBlock( aStru, aRec, 3 ), 5, "99999" )
    brw:addColumn( "Dec",  StruBlock( aStru, aRec, 4 ), 3, "99" )
    brw:setHeadSep( e"\xC4" )
    brw:setColSep( e"\xB3" )
    brw:editable := .T.
    brw:onActivated := {|| brw:beginEdit( 0 ) }

    nBtn := 37
    btn := HTPushButton():new( "Add", dlg )
    btn:setGeometry( nBtn, 3, 14, 1 )
    btn:onClicked := {|| AAdd( aStru, { Space( 10 ), "C", 10, 0 } ), brw:browse():goBottom(), brw:refreshAll() }
    btn := HTPushButton():new( "Insert", dlg )
    btn:setGeometry( nBtn, 5, 14, 1 )
    btn:onClicked := {|| hb_AIns( aStru, aRec[ 1 ], { Space( 10 ), "C", 10, 0 }, .T. ), brw:refreshAll() }
    btn := HTPushButton():new( "Delete", dlg )
    btn:setGeometry( nBtn, 7, 14, 1 )
    btn:onClicked := {|| IIF( Len( aStru ) > 1, ( hb_ADel( aStru, aRec[ 1 ], .T. ), ;
        aRec[ 1 ] := Min( aRec[ 1 ], Len( aStru ) ), brw:refreshAll() ), NIL ) }
    HTLabel():new( "Types: C N D L M", dlg ):move( nBtn, 9 )
    HTLabel():new( "Enter edits a cell", dlg ):move( nBtn, 10 )

    btn := HTPushButton():new( "Save", dlg )
    btn:setGeometry( nBtn, nH - 6, 14, 1 )
    btn:onClicked := {|| dlg:accept() }
    btn := HTPushButton():new( "Cancel", dlg )
    btn:setGeometry( nBtn, nH - 4, 14, 1 )
    btn:onClicked := {|| dlg:reject() }

    IF dlg:exec() == HT_DIALOG_ACCEPTED
        RETURN { AllTrim( cFile ), aStru }
    ENDIF

RETURN NIL

STATIC FUNCTION StruBlock( aStru, aRec, nCol )
RETURN {| x | IIF( x == NIL, aStru[ aRec[ 1 ] ][ nCol ], aStru[ aRec[ 1 ] ][ nCol ] := x ) }

STATIC FUNCTION ValidStruct( aStru )

    LOCAL aNew := {}, aField, cName, cType, nLen, nDec, c

    FOR EACH aField IN aStru
        cName := Upper( AllTrim( aField[ 1 ] ) )
        IF Empty( cName )
            LOOP
        ENDIF
        cType := Upper( Left( AllTrim( aField[ 2 ] ), 1 ) )
        nLen := aField[ 3 ]
        nDec := aField[ 4 ]
        IF ! IsAlpha( Left( cName, 1 ) ) .OR. Len( cName ) > 10
            Warn( "Invalid field name: " + cName )
            RETURN NIL
        ENDIF
        FOR EACH c IN cName
            IF ! ( IsAlpha( c ) .OR. IsDigit( c ) .OR. c == "_" )
                Warn( "Invalid field name: " + cName )
                RETURN NIL
            ENDIF
        NEXT
        IF AScan( aNew, {| a | a[ 1 ] == cName } ) > 0
            Warn( "Duplicate field name: " + cName )
            RETURN NIL
        ENDIF
        DO CASE
        CASE cType == "D"
            nLen := 8
            nDec := 0
        CASE cType == "L"
            nLen := 1
            nDec := 0
        CASE cType == "M"
            nLen := 10
            nDec := 0
        CASE cType == "C"
            nDec := 0
        CASE cType == "N"
            IF nDec > 0 .AND. nDec > nLen - 2
                Warn( cName + ": too many decimals for the length" )
                RETURN NIL
            ENDIF
        CASE Empty( cType )
            Warn( cName + ": field type missing" )
            RETURN NIL
        ENDCASE
        IF nLen < 1
            Warn( cName + ": length must be at least 1" )
            RETURN NIL
        ENDIF
        AAdd( aNew, { cName, cType, nLen, nDec } )
    NEXT
    IF Empty( aNew )
        Warn( "The structure has no fields" )
        RETURN NIL
    ENDIF

RETURN aNew

/* new structure for the current file: data appended by field name from the
   renamed old file, production index tags built again */
STATIC PROCEDURE ModifyStructure( aNew )

    LOCAL aArea := s_aAreas[ s_nArea ], cFile := aArea[ AR_FILE ], cAlias := aArea[ AR_ALIAS ]
    LOCAL cBase := hb_FNameDir( cFile ) + hb_FNameName( cFile ), cBak, aTags := {}, aTag
    LOCAL aExt := { ".dbf", ".fpt", ".dbt", ".cdx" }, cExt, cErr, aFailed := {}, n

    cBak := cBase + "_bak"
    IF ! Confirm( "Modify structure of " + hb_FNameNameExt( cFile ) + "? Old data stays in " + hb_FNameName( cBak ) + ".dbf" )
        RETURN
    ENDIF
    IF dbInfo( DBI_SHARED ) .AND. ! Reopen( .T. )
        Warn( Alias() + " is in use - the structure can be changed only EXCLUSIVE" )
        RETURN
    ENDIF
    FOR n := 1 TO OrderCount()
        IF Upper( hb_FNameName( ordBagName( n ) ) ) == Upper( hb_FNameName( cFile ) )
            AAdd( aTags, { OrdName( n ), OrdKey( n ), OrdFor( n ), ordIsUnique( n ) } )
        ENDIF
    NEXT
    dbCloseArea()

    FOR EACH cExt IN aExt
        IF hb_FileExists( cBase + cExt )
            FErase( cBak + cExt )
            FRename( cBase + cExt, cBak + cExt )
        ENDIF
    NEXT

    cErr := Try( {|| dbCreate( cFile, aNew, aArea[ AR_DRIVER ] ), ;
                     dbUseArea( .F., aArea[ AR_DRIVER ], cFile, cAlias, .F. ), ;
                     AppendFile( cBak + ".dbf" ) } )
    IF Empty( cErr )
        FOR EACH aTag IN aTags
            IF ! Empty( Try( {|| CreateTag( aTag[ 1 ], aTag[ 2 ], aTag[ 3 ], aTag[ 4 ], NIL ) } ) )
                AAdd( aFailed, aTag[ 1 ] )
            ENDIF
        NEXT
    ENDIF
    IF Used()
        dbCloseArea()
    ENDIF
    IF ! Empty( cErr )
        FOR EACH cExt IN aExt
            IF hb_FileExists( cBak + cExt )
                FErase( cBase + cExt )
                FRename( cBak + cExt, cBase + cExt )
            ENDIF
        NEXT
    ENDIF

    Try( {|| dbUseArea( .F., aArea[ AR_DRIVER ], cFile, cAlias, ! s_lExclusive ) } )
    IF ! Used()
        Warn( hb_FNameNameExt( cFile ) + " cannot be opened again" )
        aArea[ AR_WIN ]:destroy()
        CloseArea( s_nArea )
        RETURN
    ENDIF
    aArea[ AR_BAGS ] := {}
    BuildBrowse( aArea[ AR_BRW ]:browse(), cAlias )
    ApplyAllRelations()
    dbGoTop()
    RefreshCur()
    RepaintWindow( aArea[ AR_WIN ] )
    IF ! Empty( cErr )
        Warn( "Modify failed, the old file is back: " + cErr )
    ELSEIF ! Empty( aFailed )
        Warn( "Structure changed; tags not rebuilt: " + ArrJoin( aFailed ) )
    ELSE
        Toast( "Structure changed, " + hb_ntos( LastRec() ) + " records" )
    ENDIF

RETURN

STATIC PROCEDURE AppendFile( cFile )
    APPEND FROM ( cFile )
RETURN

STATIC FUNCTION ArrJoin( aItems )

    LOCAL cOut := "", c

    FOR EACH c IN aItems
        cOut += IIF( cOut == "", "", ", " ) + c
    NEXT

RETURN cOut

/*---------------------------------------------------------------- globalset */

STATIC PROCEDURE ChooseDriver()

    LOCAL aDrivers := { "DBFCDX", "DBFNTX" }, n

    IF ( n := Choose( "Database Driver (for the next database opened)", aDrivers, Max( 1, AScan( aDrivers, s_cDriver ) ) ) ) > 0
        s_cDriver := aDrivers[ n ]
        rddSetDefault( s_cDriver )
    ENDIF

RETURN

STATIC PROCEDURE ToggleDeleted()

    LOCAL n

    Set( _SET_DELETED, ! Set( _SET_DELETED ) )
    FOR n := 1 TO Len( s_aAreas )
        RefreshArea( n )
    NEXT

RETURN

STATIC PROCEDURE SetEpoch()

    LOCAL aAsk := { { "Epoch:", Set( _SET_EPOCH ), "9999" } }

    IF Ask( "Epoch", aAsk )
        Set( _SET_EPOCH, aAsk[ 1 ][ 2 ] )
    ENDIF

RETURN

STATIC PROCEDURE SetDateFormat()

    LOCAL aFormats := { "dd.mm.yyyy", "mm/dd/yyyy", "dd/mm/yyyy", "yyyy-mm-dd", "yyyy.mm.dd", "dd-mm-yyyy" }, n

    IF ( n := Choose( "Date Format", aFormats, Max( 1, AScan( aFormats, Set( _SET_DATEFORMAT ) ) ) ) ) > 0
        Set( _SET_DATEFORMAT, aFormats[ n ] )
        FOR n := 1 TO Len( s_aAreas )
            s_aAreas[ n ][ AR_BRW ]:browse():configure()
            RefreshArea( n )
        NEXT
    ENDIF

RETURN

STATIC PROCEDURE ChooseTheme()

    LOCAL aNames := HTTheme():themeNames(), n

    IF ( n := Choose( "Theme", aNames, Max( 1, AScan( aNames, s_cTheme ) ) ) ) > 0
        s_cTheme := aNames[ n ]
        HTTheme():loadTheme( s_cTheme )
        HTApplication():repaintAll()
    ENDIF

RETURN

STATIC FUNCTION IniFile()
RETURN hb_DirBase() + "cuibro.ini"

STATIC PROCEDURE LoadSettings()

    LOCAL hIni := hb_iniRead( IniFile(), .T., "=", .F. ), h

    IF HB_ISHASH( hIni ) .AND. hb_HHasKey( hIni, "CUIBro" )
        h := hIni[ "CUIBro" ]
        s_cDriver    := hb_HGetDef( h, "Driver", s_cDriver )
        s_lExclusive := hb_HGetDef( h, "OpenMode", "SHARED" ) == "EXCLUSIVE"
        s_lModify    := hb_HGetDef( h, "Modify", "ENABLED" ) == "ENABLED"
        s_lImmediate := hb_HGetDef( h, "Edit", "PROTECTED" ) == "IMMEDIATE"
        s_lSoftSeek  := hb_HGetDef( h, "Softseek", "OFF" ) == "ON"
        s_cTheme     := hb_HGetDef( h, "Theme", s_cTheme )
        Set( _SET_DELETED, hb_HGetDef( h, "HideDeleted", "OFF" ) == "ON" )
        Set( _SET_EPOCH, Val( hb_HGetDef( h, "Epoch", "1950" ) ) )
        Set( _SET_DATEFORMAT, hb_HGetDef( h, "DateFormat", Set( _SET_DATEFORMAT ) ) )
    ENDIF

RETURN

STATIC PROCEDURE SaveSettings()

    LOCAL h := { => }

    h[ "Driver" ]      := s_cDriver
    h[ "OpenMode" ]    := IIF( s_lExclusive, "EXCLUSIVE", "SHARED" )
    h[ "Modify" ]      := IIF( s_lModify, "ENABLED", "DISABLED" )
    h[ "Edit" ]        := IIF( s_lImmediate, "IMMEDIATE", "PROTECTED" )
    h[ "Softseek" ]    := IIF( s_lSoftSeek, "ON", "OFF" )
    h[ "HideDeleted" ] := IIF( Set( _SET_DELETED ), "ON", "OFF" )
    h[ "Epoch" ]       := hb_ntos( Set( _SET_EPOCH ) )
    h[ "DateFormat" ]  := Set( _SET_DATEFORMAT )
    h[ "Theme" ]       := s_cTheme

    IF hb_iniWrite( IniFile(), { "CUIBro" => h }, "; CUIBro global settings", NIL, .F. )
        Toast( "Global settings saved: " + IniFile() )
    ELSE
        Warn( "Cannot write " + IniFile() )
    ENDIF

RETURN

/*---------------------------------------------------------------- command */

STATIC FUNCTION AskFor( cTitle )

    LOCAL aAsk := { { "FOR:", PadR( ".T.", 200 ), "@S50" } }

    IF Ask( cTitle + " - " + Alias(), aAsk )
        RETURN ExprBlock( aAsk[ 1 ][ 2 ], "L" )
    ENDIF

RETURN NIL

STATIC PROCEDURE CountDlg()

    LOCAL bFor, nCount := 0, nRec := RecNo()

    IF ( bFor := AskFor( "Count" ) ) != NIL
        dbEval( {|| nCount++ }, bFor )
        dbGoto( nRec )
        Info( hb_ntos( nCount ) + " records" )
    ENDIF

RETURN

STATIC PROCEDURE SumDlg()

    LOCAL aStruct := dbStruct(), aNames := {}, aNums := {}, aField, n, bFor
    LOCAL nSum := 0, nCount := 0, nRec := RecNo()

    FOR EACH aField IN aStruct
        IF aField[ 2 ] $ "NIBYF"
            AAdd( aNames, aField[ 1 ] )
            AAdd( aNums, aField:__enumIndex() )
        ENDIF
    NEXT
    IF Empty( aNames )
        Info( Alias() + " has no numeric fields" )
        RETURN
    ENDIF
    IF ( n := Choose( "Sum/Average", aNames, 1 ) ) == 0 .OR. ( bFor := AskFor( "Sum " + aNames[ n ] ) ) == NIL
        RETURN
    ENDIF
    n := aNums[ n ]
    dbEval( {|| nSum += FieldGet( n ), nCount++ }, bFor )
    dbGoto( nRec )
    Info( FieldName( n ) + "  Sum: " + hb_ntos( nSum ) + "   Average: " + ;
          IIF( nCount > 0, hb_ntos( Round( nSum / nCount, 4 ) ), "-" ) + "   (" + hb_ntos( nCount ) + " records)" )

RETURN

STATIC PROCEDURE DeleteFor( lDelete )

    LOCAL bFor, nDone := 0, nLocked := 0, nRec := RecNo()

    IF ! s_lModify
        Toast( "Modify is DISABLED (F6 GlobalSet)" )
        RETURN
    ENDIF
    IF ( bFor := AskFor( IIF( lDelete, "Delete", "Recall" ) ) ) == NIL
        RETURN
    ENDIF
    IF ! Confirm( IIF( lDelete, "Delete", "Recall" ) + " all records FOR the condition?" )
        RETURN
    ENDIF
    dbEval( {|| IIF( RLock(), ( IIF( lDelete, dbDelete(), dbRecall() ), dbUnlock(), nDone++ ), nLocked++ ) }, bFor )
    dbCommit()
    dbGoto( nRec )
    RefreshCur()
    Info( hb_ntos( nDone ) + " records " + IIF( lDelete, "deleted", "recalled" ) + ;
          IIF( nLocked > 0, ", " + hb_ntos( nLocked ) + " locked by other users", "" ) )

RETURN

STATIC PROCEDURE ReplaceDlg()

    LOCAL aStruct := dbStruct(), aNames := {}, aField, n, aAsk, bWith, bFor
    LOCAL nDone := 0, nLocked := 0, nRec := RecNo()

    IF ! s_lModify
        Toast( "Modify is DISABLED (F6 GlobalSet)" )
        RETURN
    ENDIF
    FOR EACH aField IN aStruct
        AAdd( aNames, PadR( aField[ 1 ], 12 ) + aField[ 2 ] + Str( aField[ 3 ], 5 ) )
    NEXT
    IF ( n := Choose( "Replace field", aNames, 1 ) ) == 0
        RETURN
    ENDIF
    IF ! ( IsEditable( aStruct[ n ][ 2 ] ) .OR. aStruct[ n ][ 2 ] == "M" )
        Warn( aStruct[ n ][ 1 ] + " cannot be replaced" )
        RETURN
    ENDIF
    aAsk := { { "WITH:", Space( 200 ), "@S50" }, { "FOR:", PadR( ".T.", 200 ), "@S50" } }
    IF ! Ask( "Replace " + aStruct[ n ][ 1 ], aAsk ) .OR. ;
       ( bWith := ExprBlock( aAsk[ 1 ][ 2 ], ValueType( aStruct[ n ][ 2 ] ) ) ) == NIL .OR. ;
       ( bFor := ExprBlock( aAsk[ 2 ][ 2 ], "L" ) ) == NIL
        RETURN
    ENDIF
    IF ! Confirm( "Replace " + aStruct[ n ][ 1 ] + " WITH " + AllTrim( aAsk[ 1 ][ 2 ] ) + " in all records FOR the condition?" )
        RETURN
    ENDIF
    dbEval( {|| IIF( RLock(), ( FieldPut( n, Eval( bWith ) ), dbUnlock(), nDone++ ), nLocked++ ) }, bFor )
    dbCommit()
    dbGoto( nRec )
    RefreshCur()
    Info( hb_ntos( nDone ) + " records replaced" + IIF( nLocked > 0, ", " + hb_ntos( nLocked ) + " locked by other users", "" ) )

RETURN

/* expression type a field of type cType takes */
STATIC FUNCTION ValueType( cType )

    DO CASE
    CASE cType $ "CMQ"
        RETURN "C"
    CASE cType $ "NIBYF"
        RETURN "N"
    CASE cType $ "DLT"
        RETURN cType
    CASE cType == "@"
        RETURN "T"
    ENDCASE

RETURN NIL

STATIC PROCEDURE PackZap( lPack )

    LOCAL nBefore := LastRec()

    IF ! s_lModify
        Toast( "Modify is DISABLED (F6 GlobalSet)" )
        RETURN
    ENDIF
    IF lPack
        IF Confirm( "Pack " + Alias() + ": remove the deleted records for good?" ) .AND. WithExclusive( {|| __dbPack() } )
            dbGoTop()
            Info( hb_ntos( nBefore - LastRec() ) + " deleted records removed" )
        ENDIF
    ELSEIF Confirm( "Zap " + Alias() + ": remove ALL " + hb_ntos( nBefore ) + " records for good?" ) .AND. ;
           Confirm( "Really remove all records of " + Alias() + "?" ) .AND. WithExclusive( {|| __dbZap() } )
        Info( Alias() + " is empty" )
    ENDIF
    RefreshCur()

RETURN

STATIC PROCEDURE Reindex()
    IF WithExclusive( {|| ordListRebuild() } )
        Toast( "Indexes of " + Alias() + " rebuilt" )
    ENDIF
    RefreshCur()
RETURN

STATIC PROCEDURE AppendFrom()

    LOCAL cFile, nBefore := LastRec(), cErr

    IF ! s_lModify
        Toast( "Modify is DISABLED (F6 GlobalSet)" )
        RETURN
    ENDIF
    cFile := FileDialog( "Append From (into " + Alias() + ")", "*.dbf" )
    IF Empty( cFile ) .OR. Upper( cFile ) == Upper( s_aAreas[ s_nArea ][ AR_FILE ] )
        RETURN
    ENDIF
    IF Confirm( "Append the records of " + hb_FNameNameExt( cFile ) + " to " + Alias() + "?" )
        cErr := Try( {|| AppendFile( cFile ) } )
        RefreshCur()
        IF Empty( cErr )
            Info( hb_ntos( LastRec() - nBefore ) + " records appended" )
        ELSE
            Warn( cErr )
        ENDIF
    ENDIF

RETURN

STATIC PROCEDURE CopyStructure( lExtended )

    LOCAL aAsk := { { "To file:", PadR( IIF( lExtended, "struct.dbf", "empty.dbf" ), 120 ), "@S40" } }
    LOCAL cFile, cErr

    IF ! Ask( IIF( lExtended, "Copy Structure Extended", "Copy Structure" ) + " - " + Alias(), aAsk )
        RETURN
    ENDIF
    cFile := FullPath( DbfName( AllTrim( aAsk[ 1 ][ 2 ] ) ) )
    IF hb_FileExists( cFile ) .AND. ! Confirm( hb_FNameNameExt( cFile ) + " exists - overwrite it?" )
        RETURN
    ENDIF
    cErr := Try( {|| CopyStruFile( cFile, lExtended ) } )
    IF Empty( cErr )
        Toast( hb_FNameNameExt( cFile ) + " created" )
    ELSE
        Warn( cErr )
    ENDIF

RETURN

STATIC PROCEDURE CopyStruFile( cFile, lExtended )
    IF lExtended
        COPY STRUCTURE EXTENDED TO ( cFile )
    ELSE
        COPY STRUCTURE TO ( cFile )
    ENDIF
RETURN

STATIC PROCEDURE CopyTo()

    LOCAL aAsk := { { "To file:", PadR( "copy", 120 ), "@S40" }, ;
                    { "Type (D=DBF  S=SDF text  C=CSV):", "D", "!" }, ;
                    { "FOR:", PadR( ".T.", 200 ), "@S40" } }
    LOCAL cFile, cType, bFor, cErr, nRec := RecNo()

    IF ! Ask( "Copy To - " + Alias(), aAsk ) .OR. ( bFor := ExprBlock( aAsk[ 3 ][ 2 ], "L" ) ) == NIL
        RETURN
    ENDIF
    cType := IIF( aAsk[ 2 ][ 2 ] $ "SC", aAsk[ 2 ][ 2 ], "D" )
    cFile := AllTrim( aAsk[ 1 ][ 2 ] )
    IF Empty( hb_FNameExt( cFile ) )
        cFile += IIF( cType == "S", ".txt", IIF( cType == "C", ".csv", ".dbf" ) )
    ENDIF
    cFile := FullPath( cFile )
    IF AScan( s_aAreas, {| a | Upper( a[ AR_FILE ] ) == Upper( cFile ) } ) > 0
        Warn( hb_FNameNameExt( cFile ) + " is open - choose another name" )
        RETURN
    ENDIF
    IF hb_FileExists( cFile ) .AND. ! Confirm( hb_FNameNameExt( cFile ) + " exists - overwrite it?" )
        RETURN
    ENDIF
    cErr := Try( {|| CopyFile( cFile, cType, bFor ) } )
    dbGoto( nRec )
    IF Empty( cErr )
        Toast( "Copied to " + hb_FNameNameExt( cFile ) )
    ELSE
        Warn( cErr )
    ENDIF

RETURN

STATIC PROCEDURE CopyFile( cFile, cType, bFor )
    DO CASE
    CASE cType == "S"
        COPY TO ( cFile ) SDF FOR Eval( bFor )
    CASE cType == "C"
        COPY TO ( cFile ) DELIMITED FOR Eval( bFor )
    OTHERWISE
        COPY TO ( cFile ) FOR Eval( bFor )
    ENDCASE
RETURN

/*---------------------------------------------------------------- help, exit */

STATIC PROCEDURE BrowseHelp()

    ShowText( "Browse Window Help", { ;
        "Arrows PgUp PgDn Home End    move in the browse", ;
        "Ctrl+Home / Ctrl+End         first / last record", ;
        "Ctrl+PgUp / Ctrl+PgDn        first / last column", ;
        "Enter                        edit the field (memo: memo window)", ;
        "Ins / Del                    append record / delete-recall", ;
        "Alt+BackSpace                erase the memo", ;
        "Alt-A / Alt-E                edit record: all / browse fields", ;
        "Alt-S Alt-G Alt-U Alt-J      seek, goto, locate, skip", ;
        "Alt-T / Alt-B                window top / bottom", ;
        "Alt-F / Alt-R                range & filter / relation", ;
        "Alt-N / Alt-Minus            next / previous database", ;
        "Ctrl+F6                      next window", ;
        "Alt-O  Ctrl-F3  Shift-F3     next order, tags, index scope", ;
        "Alt-L / Alt-H                lock columns / column options", ;
        "Shift-F5 Ctrl-F5 Alt-F5      cut, paste, insert column", ;
        "Shift-F4 / Ctrl-F4           copy / paste field", ;
        "Shift-F7 Ctrl-F7 Alt-F7      copy, paste record, clipboard", ;
        "Alt-I Alt-D Alt-Q Alt-X      inc. seek, hide deleted, softseek, open mode", ;
        "Alt-M  Alt-Y  Alt-F1         structure, directory, expression", ;
        "", ;
        "F1 Help  F2 Database  F3 Index  F4 Fields  F5 AreaSet", ;
        "F6 GlobalSet  F7 Record  F8 Command  F9 Exit  F10 menu bar", ;
        "Right mouse button           context menu", ;
        "", ;
        "Relation: in the parent window Alt-R, pick the child database", ;
        "(it needs an index order on the key) and the key expression.", ;
        "Related records only = the child window shows the parent's", ;
        "records only (master-detail)." } )

RETURN

STATIC PROCEDURE Evaluator()

    LOCAL aAsk := { { "Expression:", Space( 200 ), "@S50" } }, bExpr, xValue, cErr

    DO WHILE Ask( "Expression Evaluator" + IIF( s_nArea > 0, " - " + Alias(), "" ), aAsk )
        IF ( bExpr := ExprBlock( aAsk[ 1 ][ 2 ] ) ) == NIL
            LOOP
        ENDIF
        cErr := Try( {|| xValue := Eval( bExpr ) } )
        IF Empty( cErr )
            Info( "= " + Left( hb_ValToExp( xValue ), 70 ) + "   (" + ValType( xValue ) + ")" )
        ELSE
            Warn( cErr )
        ENDIF
    ENDDO

RETURN

STATIC PROCEDURE Copyright()
    ShowText( "Copyright", { ;
        "CUIBro - xBase browse utility & file manager for HBTui", ;
        "", ;
        "Modeled after BroPlus 2.50, (c) 1989-1994 SmartCode Programming", ;
        "(Peter Volz), and its HWGUI port GBro by Srdjan Dragojlovic.", ;
        "Written from scratch on HBTui widgets, no BroPlus source code.", ;
        "", ;
        hb_Version() } )
RETURN

STATIC PROCEDURE Quit()
    IF Confirm( "Exit CUIBro?" )
        HTApplication():quit()
    ENDIF
RETURN

STATIC FUNCTION ContextMenu()

    LOCAL oMenu := HTContextMenu():new()

    oMenu:addAction( "Edit Field           Enter" ):onTriggered := {|| EditCell() }
    oMenu:addAction( "All Fields           Alt-A" ):onTriggered := {|| EditRecord( .F. ) }
    oMenu:addAction( "Append Record          Ins" ):onTriggered := {|| AppendRecord() }
    oMenu:addAction( "Delete/Recall          Del" ):onTriggered := {|| DeleteRecall() }
    oMenu:addSeparator()
    oMenu:addAction( "Copy Field        Shift-F4" ):onTriggered := {|| CopyField() }
    oMenu:addAction( "Paste Field        Ctrl-F4" ):onTriggered := {|| PasteField() }
    oMenu:addSeparator()
    oMenu:addAction( "Seek                 Alt-S" ):onTriggered := {|| SeekDlg() }
    oMenu:addAction( "Range & Filter       Alt-F" ):onTriggered := {|| FilterDlg() }
    oMenu:addAction( "Relation             Alt-R" ):onTriggered := {|| RelationDlg() }

RETURN oMenu

/*---------------------------------------------------------------- demo data */

/* CUSTOMER.DBF + ORDERS.DBF next to the program (made once) */
STATIC FUNCTION DemoTables()

    LOCAL cCust := hb_DirBase() + "customer.dbf", cOrders := hb_DirBase() + "orders.dbf"
    LOCAL aFirst := { "Petar", "Marko", "Jelena", "Milan", "Ana", "Nikola", "Ivana", "Stefan", "Milica", "Dragan", ;
                      "Jovana", "Luka", "Sara", "Nemanja", "Tijana", "Vuk", "Maja", "Bojan", "Katarina", "Aleksandar" }
    LOCAL aLast := { "Petrovic", "Markovic", "Jovanovic", "Nikolic", "Ilic", "Djordjevic", "Stojanovic", "Pavlovic", ;
                     "Lazic", "Simic", "Popovic", "Kostic", "Savic", "Lukic", "Mitrovic", "Zivkovic" }
    LOCAL aCity := { "Beograd", "Novi Sad", "Nis", "Kragujevac", "Subotica", "Zrenjanin", "Pancevo", "Cacak", ;
                     "Kraljevo", "Smederevo", "Valjevo", "Sabac" }
    LOCAL aItem := { "Keyboard", "Mouse", "Monitor 27", "Laptop stand", "USB-C hub", "Webcam HD", "Headset", ;
                     "SSD 1TB", "Printer", "Desk lamp", "Speakers", "Router", "Paper A4", "Toner" }
    LOCAL aPrice := { 49.90, 19.50, 289.00, 35.00, 42.90, 64.00, 79.99, 99.00, 159.00, 27.50, 54.00, 89.00, 6.20, 74.00 }
    LOCAL nCust := 300, n, i, nOrder := 0, nItem

    IF ! hb_FileExists( cCust )
        dbCreate( cCust, { { "ID", "N", 6, 0 }, { "NAME", "C", 30, 0 }, { "CITY", "C", 15, 0 }, ;
                           { "BORN", "D", 8, 0 }, { "BALANCE", "N", 12, 2 }, { "ACTIVE", "L", 1, 0 }, ;
                           { "NOTES", "M", 10, 0 } }, "DBFCDX" )
        dbUseArea( .T., "DBFCDX", cCust, "DEMOCUST", .F. )
        FOR n := 1 TO nCust
            dbAppend()
            FieldPut( 1, n )
            FieldPut( 2, aFirst[ ( n * 7 ) % Len( aFirst ) + 1 ] + " " + aLast[ ( n * 11 ) % Len( aLast ) + 1 ] )
            FieldPut( 3, aCity[ ( n * 5 ) % Len( aCity ) + 1 ] )
            FieldPut( 4, 0d19500101 + ( n * 389 ) % 20000 )
            FieldPut( 5, ( ( n * 7919 ) % 200000 ) / 100 - 300 )
            FieldPut( 6, n % 4 != 0 )
            IF n % 6 == 0
                FieldPut( 7, "Customer since " + hb_ntos( 1990 + n % 30 ) + "." + hb_eol() + "Preferred contact: phone." )
            ENDIF
            IF n % 23 == 0
                dbDelete()
            ENDIF
        NEXT
        ordCreate( NIL, "ID", "ID", {|| FIELD->ID } )
        ordCreate( NIL, "NAME", "UPPER(NAME)", {|| Upper( FIELD->NAME ) } )
        ordCreate( NIL, "CITY", "CITY+NAME", {|| FIELD->CITY + FIELD->NAME } )
        dbCloseArea()
    ENDIF

    IF ! hb_FileExists( cOrders )
        dbCreate( cOrders, { { "ORDERNO", "N", 6, 0 }, { "CUSTID", "N", 6, 0 }, { "ODATE", "D", 8, 0 }, ;
                             { "ITEM", "C", 20, 0 }, { "QTY", "N", 4, 0 }, { "PRICE", "N", 10, 2 } }, "DBFCDX" )
        dbUseArea( .T., "DBFCDX", cOrders, "DEMOORD", .F. )
        FOR n := 1 TO nCust
            FOR i := 1 TO ( n * 13 ) % 7
                nItem := ( n * 3 + i * 5 ) % Len( aItem ) + 1
                dbAppend()
                FieldPut( 1, ++nOrder )
                FieldPut( 2, n )
                FieldPut( 3, 0d20250101 + ( n * 17 + i * 29 ) % 600 )
                FieldPut( 4, aItem[ nItem ] )
                FieldPut( 5, ( n + i ) % 9 + 1 )
                FieldPut( 6, aPrice[ nItem ] )
            NEXT
        NEXT
        ordCreate( NIL, "ORDERNO", "ORDERNO", {|| FIELD->ORDERNO } )
        ordCreate( NIL, "CUSTID", "CUSTID", {|| FIELD->CUSTID } )
        ordCreate( NIL, "ITEM", "UPPER(ITEM)", {|| Upper( FIELD->ITEM ) } )
        dbCloseArea()
    ENDIF

RETURN { cCust, cOrders }

/*---------------------------------------------------------------- dialogs */

/* form of GETs: aFields = { { cLabel, xValue, cPicture }, ... }, values
   are written back into aFields; .T. = OK */
STATIC FUNCTION Ask( cTitle, aFields )

    LOCAL dlg, oGet, aField, nLabel := 1, nInput := 1, nRow := 1

    FOR EACH aField IN aFields
        nLabel := Max( nLabel, Len( aField[ 1 ] ) + 1 )
        nInput := Max( nInput, InputWidth( aField ) )
    NEXT
    dlg := HTDialog():new( NIL, cTitle )
    dlg:resize( Min( ht_ScreenMaxCol() - 4, Max( Max( Len( cTitle ) + 8, nLabel + nInput + 6 ), 40 ) ), Len( aFields ) * 2 + 5 )
    FOR EACH aField IN aFields
        oGet := HTGet():new( dlg )
        oGet:setup( ElementBlock( aField ), aField[ 1 ], aField[ 3 ], NIL, NIL, "", NIL, .F., .F., nLabel + nInput )
        oGet:labelWidth := nLabel
        oGet:move( 2, nRow )
        nRow += 2
    NEXT
    dlg:addButtonBar( "OK", "Cancel" )

RETURN dlg:exec() == HT_DIALOG_ACCEPTED

STATIC FUNCTION ElementBlock( aItem )
RETURN {| x | IIF( x == NIL, aItem[ 2 ], aItem[ 2 ] := x ) }

STATIC FUNCTION InputWidth( aField )

    LOCAL cPicture := hb_defaultValue( aField[ 3 ], "" )

    IF Left( cPicture, 2 ) == "@S"
        RETURN Val( SubStr( cPicture, 3 ) )
    ENDIF

RETURN Min( 50, Len( Transform( aField[ 2 ], cPicture ) ) )

/* list to pick from; returns the index, 0 = cancelled */
STATIC FUNCTION Choose( cTitle, aItems, nStart )

    LOCAL dlg, oList, nW := Len( cTitle ) + 8, nRows, c

    FOR EACH c IN aItems
        nW := Max( nW, Len( c ) + 2 )
    NEXT
    nW := Min( nW, ht_ScreenMaxCol() - 8 )
    nRows := Max( 1, Min( Len( aItems ), ht_ScreenMaxRow() - 10 ) )
    dlg := HTDialog():new( NIL, cTitle )
    dlg:resize( Max( 30, nW + 4 ), nRows + 6 )
    oList := HTListBox():new( dlg )
    oList:setGeometry( 1, 1, Max( 26, nW ), nRows )
    AEval( aItems, {| x | oList:addItem( x ) } )
    oList:setCurrentIndex( Max( 1, Min( hb_defaultValue( nStart, 1 ), Len( aItems ) ) ) )
    oList:onActivated := {|| dlg:accept() }
    dlg:addButtonBar( "OK", "Cancel" )
    IF dlg:exec() == HT_DIALOG_ACCEPTED
        RETURN oList:currentIndex()
    ENDIF

RETURN 0

STATIC PROCEDURE ShowText( cTitle, aLines )

    LOCAL dlg, oList, nW := Len( cTitle ) + 8, nRows, c

    FOR EACH c IN aLines
        nW := Max( nW, Len( c ) + 2 )
    NEXT
    nW := Min( nW, ht_ScreenMaxCol() - 8 )
    nRows := Max( 1, Min( Len( aLines ), ht_ScreenMaxRow() - 10 ) )
    dlg := HTDialog():new( NIL, cTitle )
    dlg:resize( nW + 4, nRows + 6 )
    oList := HTListBox():new( dlg )
    oList:setGeometry( 1, 1, nW, nRows )
    AEval( aLines, {| x | oList:addItem( x ) } )
    dlg:addButtonBar( "OK" )
    dlg:exec()

RETURN

/* BroPlus selection screen: the files of a directory, Enter on a
   directory opens it; returns the full path or "" */
STATIC FUNCTION FileDialog( cTitle, cSpec )

    LOCAL cDir := hb_cwd(), cFile := Space( 200 ), cPick
    LOCAL dlg, oList, oGet, oPath, nRows

    nRows := Max( 5, Min( 18, ht_ScreenMaxRow() - 14 ) )
    dlg := HTDialog():new( NIL, cTitle + "  " + cSpec )
    dlg:resize( 62, nRows + 10 )
    oPath := HTLabel():new( "", dlg )
    oPath:setGeometry( 1, 1, 58, 1 )
    oList := HTListBox():new( dlg )
    oList:setGeometry( 1, 2, 58, nRows )
    oGet := HTGet():new( dlg )
    oGet:setup( {| x | IIF( x == NIL, cFile, cFile := x ) }, "File:", "@S50", NIL, NIL, "", NIL, .F., .F., 56 )
    oGet:move( 1, nRows + 3 )
    FillFiles( oList, oPath, cDir, cSpec )

    oList:onChanged := {| n | cPick := oList:itemText( n ), ;
        IIF( IsDirItem( cPick ), NIL, ( cFile := PadR( cPick, 200 ), dlg:repaintChild( oGet ) ) ) }
    oList:onActivated := {| n | cPick := oList:itemText( n ), ;
        IIF( IsDirItem( cPick ), ;
             ( cDir := SubDir( cDir, cPick ), FillFiles( oList, oPath, cDir, cSpec ), ;
               dlg:repaintChild( oPath ), dlg:repaintChild( oList ) ), ;
             ( cFile := PadR( cPick, 200 ), dlg:accept() ) ) }
    dlg:addButtonBar( "OK", "Cancel" )

    IF dlg:exec() == HT_DIALOG_ACCEPTED .AND. ! Empty( cFile )
        cFile := AllTrim( cFile )
        RETURN IIF( ":" $ cFile .OR. Left( cFile, 1 ) $ "\/", cFile, cDir + cFile )
    ENDIF

RETURN ""

STATIC PROCEDURE FillFiles( oList, oPath, cDir, cSpec )

    LOCAL aDirs := {}, aFiles := {}, aEntry, c

    oList:clear()
    IF ! ( Len( cDir ) <= 1 .OR. ( Len( cDir ) == 3 .AND. SubStr( cDir, 2, 1 ) == ":" ) )
        oList:addItem( ".." + hb_ps() )
    ENDIF
    FOR EACH aEntry IN Directory( cDir + hb_osFileMask(), "D" )
        IF "D" $ aEntry[ F_ATTR ] .AND. ! aEntry[ F_NAME ] == "." .AND. ! aEntry[ F_NAME ] == ".."
            AAdd( aDirs, aEntry[ F_NAME ] + hb_ps() )
        ENDIF
    NEXT
    FOR EACH aEntry IN Directory( cDir + cSpec )
        AAdd( aFiles, aEntry[ F_NAME ] )
    NEXT
    ASort( aDirs,,, {| a, b | Upper( a ) < Upper( b ) } )
    ASort( aFiles,,, {| a, b | Upper( a ) < Upper( b ) } )
    FOR EACH c IN aFiles
        oList:addItem( c )
    NEXT
    FOR EACH c IN aDirs
        oList:addItem( c )
    NEXT
    IF oList:count() > 0
        oList:setCurrentIndex( 1 )
    ENDIF
    oPath:setText( "Directory: " + cDir )
    oPath:setGeometry( 1, 1, 58, 1 )

RETURN

STATIC FUNCTION IsDirItem( c )
RETURN Right( c, 1 ) == hb_ps()

STATIC FUNCTION SubDir( cDir, cItem )
    IF cItem == ".." + hb_ps()
        RETURN hb_FNameDir( Left( cDir, Len( cDir ) - 1 ) )
    ENDIF
RETURN cDir + cItem

STATIC FUNCTION ArraySkip( aRec, nLen, nRequest )

    LOCAL nActual := IIF( nRequest > 0, Min( nRequest, nLen - aRec[ 1 ] ), Max( nRequest, 1 - aRec[ 1 ] ) )

    aRec[ 1 ] += nActual

RETURN nActual

STATIC PROCEDURE Info( cText )
    HTMessageBox():information( "CUIBro", Left( cText, ht_ScreenMaxCol() - 8 ) )
RETURN

STATIC PROCEDURE Warn( cText )
    Tone( 87.3, 1 )
    HTMessageBox():warning( "CUIBro", Left( cText, ht_ScreenMaxCol() - 8 ) )
RETURN

STATIC FUNCTION Confirm( cText )
RETURN HTMessageBox():question( "CUIBro", Left( cText, ht_ScreenMaxCol() - 8 ) ) == HT_DIALOG_ACCEPTED

STATIC PROCEDURE Toast( cText )
    HTToast():show( cText, 2500, HT_CLR_TOAST_INFO )
RETURN

/* runs bBlock; "" or the error text */
STATIC FUNCTION Try( bBlock )

    LOCAL oErr, cErr := ""

    BEGIN SEQUENCE WITH {| e | Break( e ) }
        Eval( bBlock )
    RECOVER USING oErr
        cErr := ErrText( oErr )
    END SEQUENCE

RETURN cErr

STATIC FUNCTION ErrText( oErr )

    LOCAL cText

    IF ! HB_ISOBJECT( oErr )
        RETURN hb_CStr( oErr )
    ENDIF
    cText := oErr:subSystem + "/" + hb_ntos( oErr:subCode ) + " " + oErr:description
    IF ! Empty( oErr:operation )
        cText += ": " + oErr:operation
    ENDIF
    IF ! Empty( oErr:filename )
        cText += " " + oErr:filename
    ENDIF

RETURN cText

/* compiled expression, checked on the current record; NIL when invalid
   (cType: the value type it must have, NIL = any) */
STATIC FUNCTION ExprBlock( cExpr, cType )

    LOCAL bExpr, xValue, cErr

    cExpr := AllTrim( cExpr )
    IF Empty( cExpr )
        Warn( "Expression missing" )
        RETURN NIL
    ENDIF
    cErr := Try( {|| bExpr := hb_macroBlock( cExpr ), xValue := Eval( bExpr ) } )
    IF Empty( cErr ) .AND. cType != NIL .AND. ! ValType( xValue ) == cType
        cErr := "the value is of type " + ValType( xValue ) + ", " + cType + " expected"
    ENDIF
    IF ! Empty( cErr )
        Warn( "Invalid Expression " + cExpr + ": " + cErr )
        RETURN NIL
    ENDIF

RETURN bExpr
