/** @file cbro.prg
 * CBro - xBase database browser and file manager in character mode (CUI).
 *
 * Copyright (c) 2026 Srđan Dragojlović
 * Dual licensed: GNU GPL version 3 or later (COPYING), or a commercial
 * license from the author - see LICENSE.
 * Made as a successor to BroPlus by Peter Volz (SmartCode Programming):
 * the same function key menus (F1 Help .. F9 Exit), hot keys and status
 * lines, written from scratch on the Harbour-CUI widgets - no BroPlus code.
 * Its GUI sibling is GBro (HWGUI).
 *
 * Every open table has its own floating window:
 *   title       alias, position among the open tables, index order, relation
 *   browse      the records, down to the status bar; Enter edits a field,
 *               Ins appends, Del deletes, a click / Space toggles a logical
 *   status bar  Rec. n/count, <Lock: n>, <Shared>, <Softseek>, <Filter>,
 *               <Rel: ...>, <Deleted>, Key: the index key
 * The main window holds the F-key row, the menu bar and the bottom line
 * (directory, edit mode, incremental seek string).
 *
 * Tables: DBF (DBFCDX / DBFNTX, any data codepage), LetoDB / LetoDBf servers
 * (driver LETO), SQL queries (ODBC or SQLite through Harbour's SQLMIX) and
 * .xlsx sheets (read into a memory table; Copy To writes .xlsx).
 * Scripts: Command > Run Script compiles a .prg (or loads a .hrb) and runs it.
 *
 * Usage: cbro [table[.dbf|.xlsx] | leto://host:port/path/table.dbf [index]] [/options]
 *        cbro /? lists the parameters
 * Keys:  F1..F9 menus, Ctrl+F6 next window, F1 > Browse Window Help
 */

#include "hbtui.ch"
#include "inkey.ch"
#include "set.ch"
#include "dbinfo.ch"
#include "directry.ch"
#include "fileio.ch"
#include "hbextcdp.ch"
#include "hbhrb.ch"
#include "hbgtinfo.ch"

REQUEST DBFCDX, DBFNTX, DBFFPT, HB_MEMIO
REQUEST SQLMIX, SDDODBC, SDDSQLITE3
REQUEST LETO

#define CB_VERSION   "1.1.1"

/* one open table */
#define AR_ALIAS     1
#define AR_FILE      2   /* file, server path, xlsx file or SQL query */
#define AR_DRIVER    3
#define AR_BAGS      4   /* index files opened by hand */
#define AR_WIN       5   /* TableWindow */
#define AR_BRW       6   /* CBBrowse */
#define AR_IDLE      7   /* HTStatusBar: Bp_IdleStat indicators */
#define AR_KIND      8   /* "DBF", "LETO", "SQL", "XLSX" */
#define AR_RELS      9   /* { { cChildAlias, cKeyExpr, bKey, lRelatedOnly }, ... } */
#define AR_IDLETXT   10  /* texts shown in the indicator sections */
#define AR_LASTREC   11  /* record the children were last synchronized to */
#define AR_CDP       12  /* data codepage the table was opened with ("" = none) */
#define AR_COLOR     13  /* color condition: { cExpr, bExpr, cColor } or NIL */
#define AR_MEMOW     14  /* memo column width (0 = <Memo>) */
#define AR_CONN      15  /* SQL connection handle */
#define AR_CUSTOM    16  /* custom index tag Ctrl-F8 / Ctrl-F9 add to / remove from */
#define AR_LEN       16

#define REL_CHILD    1
#define REL_EXPR     2
#define REL_BLOCK    3
#define REL_ONLY     4

/* column cargo */
#define CC_NAME      1   /* field name or expression */
#define CC_TYPE      2   /* field type, "E" = expression */
#define CC_ALIAS     3
#define CC_RAW       4   /* the field's read/write block (the shown block may format it) */

#define IDLE_SECTIONS 5

STATIC s_aAreas := {}
STATIC s_nArea  := 0
STATIC s_oMain, s_oBar, s_oMsg
STATIC s_aMsgTxt := { "", "", "" }
STATIC s_nMsgBase := 0              /* 1: section 1 shows the input language */
STATIC s_cInputLang := ""
STATIC s_hAct := { => }
STATIC s_hFKeys := { => }

/* GlobalSet */
STATIC s_cDriver    := "DBFCDX"
STATIC s_lExclusive := .F.
STATIC s_lModify    := .T.
STATIC s_lImmediate := .F.
STATIC s_lSoftSeek  := .F.
STATIC s_cTheme     := "BroPlus"     /* BroPlus colors until GlobalSet > Theme */
STATIC s_cCodepage  := ""          /* data codepage of the tables opened ("" = as stored) */
STATIC s_cLetoAddr  := "//127.0.0.1:2812/"
STATIC s_cDbfSpec   := "*.dbf"
STATIC s_cNdxSpec   := ""          /* "" = by driver: *.cdx / *.ntx */
STATIC s_lPreselect := .T.         /* open the production / same-name index */
STATIC s_lSound     := .T.
STATIC s_nMemoWidth := 0           /* default memo column width */
STATIC s_lAutoAlias := .T.
STATIC s_cSqlConn   := ""          /* last SQL connection string */
STATIC s_cSqlType   := "SQLITE3"
STATIC s_cScript    := ""          /* last script file */

/* AreaSet / Record */
STATIC s_lIncSeek := .F.
STATIC s_cSeek    := ""
STATIC s_cLastSeek := ""
STATIC s_cLocate  := ""
STATIC s_xClipCell
STATIC s_aClipRec
STATIC s_oCutCol
STATIC s_aSqlConns := {}           /* { { cType, cConn, nHandle }, ... } */
STATIC s_lLetoConn := .F.

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

/* browse from under the title down to the status bar on the last row
   (left of the grip) */
METHOD PROCEDURE arrange() CLASS TableWindow
    IF ::oBrw != NIL
        ::oBrw:setGeometry( 0, 0, ::Fwidth, Max( 1, ::Fheight - 2 ) )
        ::oIdle:setGeometry( 0, ::Fheight - 2, Max( 1, ::Fwidth - 1 ), 1 )
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
    PROPERTY onCellClick READWRITE      /* {|nRow, nCol, nOldRow, nOldCol| ... } left click */
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
    LOCAL nCol := ::FoBrowse:colPos, nRow := ::FoBrowse:rowPos
    ::super:mouseEvent( eventMouse )
    /* a click on a logical cell toggles it (a click that only moved there too) */
    IF eventMouse:nKey == K_LBUTTONDOWN .AND. ::FonCellClick != NIL .AND. ! ::isEditing()
        Eval( ::FonCellClick, ::FoBrowse:rowPos, ::FoBrowse:colPos, nRow, nCol )
    ENDIF
    IF ::FonMoved != NIL
        Eval( ::FonMoved )
    ENDIF
RETURN

/*---------------------------------------------------------------- main */

PROCEDURE Main( ... )

    LOCAL app, aDemo, aArgs := hb_AParams(), cArg, cFile := "", cIndex := "", cScript := ""
    LOCAL lReadOnly := .F., cOpt, lDemo := .F.

    Set( _SET_DATEFORMAT, "dd.mm.yyyy" )
    Set( _SET_EPOCH, 1950 )
    LoadSettings()

    /* command line: files and /options (Help > Command Line Parameters) */
    FOR EACH cArg IN aArgs
        IF Left( cArg, 1 ) $ "/-" .AND. Len( cArg ) > 1 .AND. ! ( Left( cArg, 2 ) == "//" )
            cOpt := Upper( SubStr( cArg, 2 ) )
            DO CASE
            CASE cOpt == "?" .OR. cOpt == "H" .OR. cOpt == "HELP"
                CmdLineHelp()
                RETURN
            CASE cOpt == "DEMO"
                lDemo := .T.
            CASE cOpt == "X"
                s_lExclusive := .T.
            CASE cOpt == "S"
                s_lExclusive := .F.
            CASE cOpt == "R"
                lReadOnly := .T.
            CASE cOpt == "M"
                s_lModify := .F.
            CASE Left( cOpt, 2 ) == "D:"
                s_cDriver := SubStr( cOpt, 3 )
            CASE Left( cOpt, 2 ) == "C:"
                s_cCodepage := SubStr( cOpt, 3 )
            CASE Left( cOpt, 2 ) == "T:"
                s_cTheme := SubStr( cArg, 4 )
            CASE Left( cOpt, 2 ) == "L:"
                s_cLetoAddr := LetoAddr( SubStr( cArg, 4 ) )
            CASE Left( cOpt, 2 ) == "P:"
                cScript := SubStr( cArg, 4 )
            ENDCASE
        ELSEIF Empty( cFile )
            cFile := cArg
        ELSE
            cIndex := cArg
        ENDIF
    NEXT
    IF AScan( { "DBFCDX", "DBFNTX", "LETO" }, s_cDriver ) == 0
        s_cDriver := "DBFCDX"
    ENDIF
    rddSetDefault( IIF( s_cDriver == "LETO", "DBFCDX", s_cDriver ) )
    Set( _SET_AUTOPEN, s_lPreselect )

    app := HTApplication():new()
    ht_cuiSetupDisplay( app:rows(), app:cols() )
    hb_gtInfo( HB_GTI_WINTITLE, "CBro" )
    hb_gtInfo( HB_GTI_ICONRES, 1 )
    RegisterBroPlusTheme()
    IF ! s_cTheme == "Classic" .AND. HTTheme():hasTheme( s_cTheme )
        HTTheme():loadTheme( s_cTheme )
    ENDIF

    s_oMain := MainShell():new()
    s_oMain:setWindowTitle( "CBro" )
    s_oMain:show()

    s_oMsg := HTStatusBar():new( s_oMain )
    s_oMsg:setGeometry( 0, s_oMain:height - 2, s_oMain:width, 1 )
    /* the input language / IME mode: the taskbar showing it is hidden under
       the fullscreen window (Windows) */
    s_cInputLang := ht_inputLanguage()
    IF ! Empty( s_cInputLang )
        s_nMsgBase := 1
        s_oMsg:addSection( " " + s_cInputLang + " " )
        HTEventLoop():scheduleRepeat( {|| ShowInputLang() }, 300 )
    ENDIF
    AEval( s_aMsgTxt, {|| s_oMsg:addSection( "" ) } )

    BuildMenu()
    s_oMain:setWindowTitle( FKeyRow() )

    IF Empty( cFile ) .AND. ! lDemo
        /* BroPlus starts with the database selection screen */
        HTEventLoop():schedule( {|| SelectDatabase(), AfterAction() }, 400 )
    ELSEIF Empty( cFile )
        /* /DEMO: the demo tables next to the program, made when missing; a read-only
           folder (Program Files): in the TEMP folder; else no tables */
        aDemo := NIL
        IF ! Empty( Try( {|| aDemo := DemoTables( hb_DirBase() ) } ) )
            Try( {|| dbCloseAll(), aDemo := DemoTables( hb_DirTemp() ) } )
        ENDIF
        IF aDemo == NIL
            HTEventLoop():schedule( {|| Toast( "F2 or Alt-F2 opens a table" ) }, 400 )
        ELSEIF OpenTable( aDemo[ 1 ] ) .AND. OpenTable( aDemo[ 2 ] )
            ordSetFocus( "CUSTID" )
            SetRelation( 1, "ORDERS", "ID", .T. )
        ENDIF
        TileWindows()
        IF Len( s_aAreas ) > 0
            SelectArea( 1, .F. )
            s_aAreas[ 1 ][ AR_WIN ]:activate()
        ENDIF
    ELSE
        /* after the main window is on the screen (questions, messages) */
        HTEventLoop():schedule( {|| IIF( OpenAny( cFile, lReadOnly ) .AND. ! Empty( cIndex ), AddIndexFile( cIndex ), NIL ), AfterAction() }, 400 )
    ENDIF

    ShowStatLine()
    IF ! Empty( cScript )
        HTEventLoop():schedule( {|| RunScriptFile( cScript ) }, 300 )
    ENDIF
    app:exec()

    dbCloseAll()
    SqlDisconnectAll()

RETURN

/* a file of any kind CBro opens: .xlsx sheet, server path (//host:port/),
   else a DBF */
STATIC FUNCTION OpenAny( cFile, lReadOnly )

    cFile := AllTrim( cFile )
    IF Lower( Left( cFile, 7 ) ) == "leto://"     /* command line: Harbour takes "//..." for its own switches */
        cFile := SubStr( cFile, 6 )
    ENDIF
    DO CASE
    CASE Lower( hb_FNameExt( cFile ) ) $ ".xlsx.xlsm"
        RETURN OpenXlsx( cFile )
    CASE Left( cFile, 2 ) == "//"
        RETURN OpenLeto( cFile, lReadOnly )
    ENDCASE

RETURN OpenTable( cFile, NIL, NIL, lReadOnly )

/* the F-key row of BroPlus, over the menu titles (window title row) */
STATIC FUNCTION FKeyRow()

    LOCAL aTitles := { "Help", "Database", "Index", "Fields", "AreaSet", ;
                       "GlobalSet", "Record", "Command", "Exit" }
    LOCAL cRow := "", nCol := 2, n

    FOR n := 1 TO Len( aTitles )
        cRow := PadR( cRow, nCol - 1 ) + "F" + hb_ntos( n )
        nCol += Len( aTitles[ n ] ) + 3
    NEXT

RETURN PadR( cRow, nCol + 2 ) + "CBro"

STATIC PROCEDURE BuildMenu()

    LOCAL oMenu, n
    LOCAL aFKeys := { K_F1, K_F2, K_F3, K_F4, K_F5, K_F6, K_F7, K_F8, K_F9 }

    s_oBar := HTMenuBar():new( s_oMain )

    oMenu := s_oBar:addMenu( "Help" )
    Item( oMenu, "&Browse Window Help            ", NIL, {|| CBH_Show( "browse" ) }, .F. )
    Item( oMenu, "&Help on Help                  ", NIL, {|| CBH_Show( "help" ) }, .F. )
    Item( oMenu, "Help &Index                    ", NIL, {|| CBH_Show( "" ) }, .F. )
    Item( oMenu, "&Command Line Parameters       ", NIL, {|| CBH_Show( "cmdline" ) }, .F. )
    oMenu:addSeparator()
    Item( oMenu, "&Expression Evaluator  <Alt-F1>", K_ALT_F1, {|| Evaluator() }, .F. )
    Item( oMenu, "c&Alculator           <Ctrl-F1>", K_CTRL_F1, {|| Calculator() }, .F. )
    oMenu:addSeparator()
    Item( oMenu, "c&Opyright                     ", NIL, {|| CBH_About() }, .F. )

    oMenu := s_oBar:addMenu( "Database" )
    Item( oMenu, "&Database Selection Screen  <Alt-F2>", K_ALT_F2, {|| SelectDatabase() }, .F. )
    Item( oMenu, "Open from &LetoDB Server   <Ctrl-L> ", K_CTRL_L, {|| LetoDialog() }, .F. )
    Item( oMenu, "S&QL Query                 <Ctrl-Q> ", K_CTRL_Q, {|| SqlDialog() }, .F. )
    Item( oMenu, "Open E&xcel Sheet (.xlsx)           ", NIL, {|| SelectXlsx() }, .F. )
    oMenu:addSeparator()
    Item( oMenu, "&Next Database              <Alt-N> ", K_ALT_N, {|| NextArea( 1 ) } )
    Item( oMenu, "&Previous Database       <Alt-Minus>", K_ALT_MINUS, {|| NextArea( -1 ) } )
    Item( oMenu, "c&Lose Database                     ", NIL, {|| CloseDatabase() } )
    oMenu:addSeparator()
    Item( oMenu, "", K_ALT_X, {|| ToggleOpenMode() }, .T., "openmode" )
    Item( oMenu, "", NIL, {|| AliasDlg() }, .T., "alias" )
    Item( oMenu, "Create/&Modify Structure    <Alt-M> ", K_ALT_M, {|| CreateModify() }, .F. )
    oMenu:addSeparator()
    Item( oMenu, "", NIL, {|| SpecDlg( .T. ) }, .F., "dbfspec" )
    Item( oMenu, "&Change Directory           <Alt-Y> ", K_ALT_Y, {|| ChangeDirectory() }, .F. )
    Item( oMenu, "&Run OS Command             <Alt-Z> ", K_ALT_Z, {|| RunCommand() }, .F. )

    oMenu := s_oBar:addMenu( "Index" )
    Item( oMenu, "&Index File Selection Screen <Alt-F3>", K_ALT_F3, {|| OpenIndexFile() } )
    Item( oMenu, "&Next Order                  <Alt-O> ", K_ALT_O, {|| NextOrder( 1 ) } )
    Item( oMenu, "&Previous Order                      ", NIL, {|| NextOrder( -1 ) } )
    Item( oMenu, "&Tag Selection/Information  <Ctrl-F3>", K_CTRL_F3, {|| ChooseOrder() } )
    Item( oMenu, "Index &ScopeTop/Bottom      <Shft-F3>", K_SH_F3, {|| ScopeDlg() } )
    oMenu:addSeparator()
    oMenu:addSeparator()
    Item( oMenu, "", K_SH_F6, {|| ToggleUnique() }, .T., "unique" )
    Item( oMenu, "", K_ALT_F6, {|| ToggleDescend() }, .T., "descend" )
    Item( oMenu, "Make &Custom Index...                ", NIL, {|| CustomIndexDlg() } )
    Item( oMenu, "Custom: &Add Record         <Ctrl-F8>", K_CTRL_F8, {|| CustomKey( .T. ) } )
    Item( oMenu, "Custom: Re&move Record      <Ctrl-F9>", K_CTRL_F9, {|| CustomKey( .F. ) } )
    oMenu:addSeparator()
    Item( oMenu, "c&Reate Index File                   ", NIL, {|| CreateIndexDlg() } )
    Item( oMenu, "&Verify Index Integrity              ", NIL, {|| VerifyIndex() } )
    Item( oMenu, "", K_ALT_P, {|| s_lPreselect := ! s_lPreselect, Set( _SET_AUTOPEN, s_lPreselect ) }, .F., "presel" )
    Item( oMenu, "", NIL, {|| SpecDlg( .F. ) }, .F., "ndxspec" )

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
    Item( oMenu, "&Color Condition         <Alt-C>", K_ALT_C, {|| ColorCondDlg() } )
    Item( oMenu, "", K_ALT_I, {|| ToggleIncSeek() }, .T., "incseek" )
    Item( oMenu, "", NIL, {|| MemoWidthDlg() }, .T., "memow" )
    oMenu:addSeparator()
    Item( oMenu, "&Next Select Area        <Alt-N>", NIL, {|| NextArea( 1 ) } )
    Item( oMenu, "&Prev. Select Area   <Alt-Minus>", NIL, {|| NextArea( -1 ) } )
    Item( oMenu, "Next Index &Order        <Alt-O>", NIL, {|| NextOrder( 1 ) } )
    oMenu:addSeparator()
    Item( oMenu, "Ne&xt Window           <Ctrl-F6>", NIL, {|| HTApplication():activateNextWindow( 1 ) } )
    Item( oMenu, "Pr&evious Window               ", NIL, {|| HTApplication():activateNextWindow( -1 ) } )
    Item( oMenu, "&Tile Windows                   ", NIL, {|| TileWindows(), HTApplication():repaintAll() } )
    Item( oMenu, "Cascade &Windows                ", NIL, {|| CascadeWindows(), HTApplication():repaintAll() } )
    oMenu:addSeparator()
    Item( oMenu, "Restore &View            <Alt-V>", K_ALT_V, {|| RestoreView() }, .F. )
    Item( oMenu, "&Save Current View       <Alt-W>", K_ALT_W, {|| SaveView() } )
    Item( oMenu, "Curren&t View Info              ", NIL, {|| ViewInfo() } )

    oMenu := s_oBar:addMenu( "GlobalSet" )
    Item( oMenu, "", NIL, {|| ChooseDriver() }, .F., "driver" )
    Item( oMenu, "", NIL, {|| ChooseCodepage() }, .F., "cdp" )
    Item( oMenu, "", NIL, {|| LetoServerDlg() }, .F., "leto" )
    Item( oMenu, "", NIL, {|| s_lExclusive := ! s_lExclusive }, .F., "defmode" )
    Item( oMenu, "", NIL, {|| s_lModify := ! s_lModify }, .F., "modify" )
    Item( oMenu, "", NIL, {|| s_lImmediate := ! s_lImmediate }, .F., "edit" )
    Item( oMenu, "", K_ALT_D, {|| ToggleDeleted() }, .F., "deleted" )
    Item( oMenu, "", K_ALT_Q, {|| s_lSoftSeek := ! s_lSoftSeek }, .F., "softseek" )
    Item( oMenu, "", NIL, {|| s_lSound := ! s_lSound }, .F., "sound" )
    oMenu:addSeparator()
    Item( oMenu, "", NIL, {|| SetEpoch() }, .F., "epoch" )
    Item( oMenu, "", NIL, {|| SetDateFormat() }, .F., "datefmt" )
    Item( oMenu, "", NIL, {|| DefMemoWidth() }, .F., "defmemo" )
    Item( oMenu, "", NIL, {|| s_lAutoAlias := ! s_lAutoAlias }, .F., "autoalias" )
    Item( oMenu, "", NIL, {|| ChooseTheme() }, .F., "theme" )
    oMenu:addSeparator()
    Item( oMenu, "Sa&ve Global Settings             ", NIL, {|| SaveSettings() }, .F. )

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
    Item( oMenu, "I&mport File to Memo  ", NIL, {|| MemoFile( .T. ) } )
    Item( oMenu, "e&Xport Memo to File  ", NIL, {|| MemoFile( .F. ) } )
    oMenu:addSeparator()
    Item( oMenu, "&Copy Field  <Shft-F4>", K_SH_F4, {|| CopyField() } )
    Item( oMenu, "Paste &Field <Ctrl-F4>", K_CTRL_F4, {|| PasteField() } )
    Item( oMenu, "c&Opy Record <Shft-F7>", K_SH_F7, {|| CopyRecord() } )
    Item( oMenu, "Paste &Recrd <Ctrl-F7>", K_CTRL_F7, {|| PasteRecord() } )
    Item( oMenu, "&View Clipbrd <Alt-F7>", K_ALT_F7, {|| ViewClipboard() }, .F. )

    oMenu := s_oBar:addMenu( "Command" )
    Item( oMenu, "&Append From           ", NIL, {|| AppendFrom() } )
    Item( oMenu, "&Copy Structure        ", NIL, {|| CopyStructure( .F. ) } )
    Item( oMenu, "Copy Stru E&xtended    ", NIL, {|| CopyStructure( .T. ) } )
    Item( oMenu, "C&opy To / Export      ", NIL, {|| CopyTo() } )
    Item( oMenu, "Coun&t                 ", NIL, {|| CountDlg() } )
    Item( oMenu, "&Delete                ", NIL, {|| DeleteFor( .T. ) } )
    Item( oMenu, "&Label Form            ", NIL, {|| LabelReport( .T. ) } )
    Item( oMenu, "L&ist to File/Print    ", NIL, {|| ListTo() } )
    Item( oMenu, "&Pack                  ", NIL, {|| PackZap( .T. ) } )
    Item( oMenu, "&Recall                ", NIL, {|| DeleteFor( .F. ) } )
    Item( oMenu, "Rei&ndex               ", NIL, {|| Reindex() } )
    Item( oMenu, "R&eplace               ", NIL, {|| ReplaceDlg() } )
    Item( oMenu, "Report &Form           ", NIL, {|| LabelReport( .F. ) } )
    Item( oMenu, "&Sum/Average           ", NIL, {|| SumDlg() } )
    Item( oMenu, "&Zap                   ", NIL, {|| PackZap( .F. ) } )
    oMenu:addSeparator()
    Item( oMenu, "R&un Script     <Alt-K>", K_ALT_K, {|| RunScript() }, .F. )
    Item( oMenu, "Script &Editor         ", NIL, {|| ScriptEditor() }, .F. )
    Item( oMenu, "SQL &Query     <Ctrl-Q>", NIL, {|| SqlDialog() }, .F. )

    oMenu := s_oBar:addMenu( "Exit" )
    Item( oMenu, "e&Xit  <Alt-F9>", K_ALT_F9, {|| Quit() }, .F. )

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

    LOCAL lShared := .T., cAlias := "", nMemo := s_nMemoWidth

    IF s_nArea > 0
        lShared := ( s_aAreas[ s_nArea ][ AR_ALIAS ] )->( dbInfo( DBI_SHARED ) )
        cAlias := s_aAreas[ s_nArea ][ AR_ALIAS ]
        nMemo := s_aAreas[ s_nArea ][ AR_MEMOW ]
    ENDIF

    SetActText( "openmode", "&Open Mode:     " + IIF( lShared, "SHARED   ", "EXCLUSIVE" ) + "   <Alt-X> " )
    SetActText( "alias", "&Alias Name:    " + PadR( cAlias, 21 ) )
    SetActText( "dbfspec", "Database &FileSpec:   " + PadR( s_cDbfSpec, 14 ) )
    SetActText( "ndxspec", "Index &FileSpec:      " + PadR( NdxSpec(), 15 ) )
    SetActText( "unique", "Show &Unique Keys  is " + OnOff( IndexState( 1 ) ) + "  <Shift-F6>" )
    SetActText( "descend", PadR( IIF( IndexState( 2 ), "De", "A" ) + "scending &Order", 18 ) + "         <Alt-F6>" )
    SetActText( "presel", "In&dex PreSelection is " + OnOff( s_lPreselect ) + "   <Alt-P> " )
    SetActText( "lock", "&Lock # of Fields: " + PadR( hb_ntos( IIF( s_nArea > 0, CurTB():freeze, 0 ) ), 2 ) + "    <Alt-L> " )
    SetActText( "incseek", "&Incremental Seek is " + OnOff( s_lIncSeek ) + " <Alt-I>" )
    SetActText( "memow", "&Memo Width:" + Str( nMemo, 9 ) + Space( 10 ) )
    SetActText( "driver", "Database d&River:   " + PadR( s_cDriver, 15 ) )
    SetActText( "cdp", "Data &Codepage:     " + PadR( IIF( Empty( s_cCodepage ), "(as stored)", s_cCodepage ), 15 ) )
    SetActText( "leto", "&LetoDB Server:     " + PadR( s_cLetoAddr, 15 ) )
    SetActText( "defmode", "Default &Open Mode: " + PadR( IIF( s_lExclusive, "EXCLUSIVE", "SHARED" ), 15 ) )
    SetActText( "modify", "&Modify:            " + PadR( IIF( s_lModify, "ENABLED", "DISABLED" ), 15 ) )
    SetActText( "edit", PadR( IIF( s_lImmediate, "Imm&ediate", "Prot&ected" ) + " Edit", 34 ) )
    SetActText( "deleted", "H&ide Deleted Recs is " + OnOff( Set( _SET_DELETED ) ) + "   <Alt-D>" )
    SetActText( "softseek", "&Softseek          is " + OnOff( s_lSoftSeek ) + "   <Alt-Q>" )
    SetActText( "sound", "So&und             is " + OnOff( s_lSound ) + "          " )
    SetActText( "epoch", "E&poch:             " + PadR( hb_ntos( Set( _SET_EPOCH ) ), 15 ) )
    SetActText( "datefmt", "Date &Format:       " + PadR( Set( _SET_DATEFORMAT ), 15 ) )
    SetActText( "defmemo", "Default Memo &Width: " + PadR( hb_ntos( s_nMemoWidth ), 14 ) )
    SetActText( "autoalias", "&Auto-Alias        is " + OnOff( s_lAutoAlias ) + "          " )
    SetActText( "theme", "&Theme:             " + PadR( s_cTheme, 15 ) )

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

/* opens a DBF (local, or on a LetoDB server: "//host:port/path/file.dbf") in
   a new window; .T. when it is open (or already was) */
STATIC FUNCTION OpenTable( cFile, cDriver, cAlias, lReadOnly, cCdp )

    LOCAL cErr, nPos, lLeto, cKind

    cFile := AllTrim( cFile )
    lLeto := Left( cFile, 2 ) == "//"
    cDriver := hb_defaultValue( cDriver, IIF( lLeto, "LETO", IIF( s_cDriver == "LETO", "DBFCDX", s_cDriver ) ) )
    cCdp := hb_defaultValue( cCdp, s_cCodepage )
    cKind := IIF( lLeto, "LETO", "DBF" )
    IF lLeto
        cFile := DbfName( cFile )
    ELSE
        cFile := FullPath( DbfName( cFile ) )
        IF ! hb_FileExists( cFile )
            Warn( cFile + ": File cannot be found" )
            RETURN .F.
        ENDIF
    ENDIF
    IF ( nPos := AScan( s_aAreas, {| a | Upper( a[ AR_FILE ] ) == Upper( cFile ) } ) ) > 0
        SelectArea( nPos, .T. )
        RETURN .T.
    ENDIF

    IF cAlias == NIL
        cAlias := NewAlias( cFile )
        IF cAlias == NIL
            RETURN .F.
        ENDIF
    ENDIF
    cErr := Try( {|| dbUseArea( .T., cDriver, cFile, cAlias, ! s_lExclusive, hb_defaultValue( lReadOnly, .F. ), ;
                                IIF( Empty( cCdp ), NIL, cCdp ) ) } )
    IF Select( cAlias ) == 0
        Warn( hb_FNameNameExt( cFile ) + " cannot be opened" + IIF( s_lExclusive, " EXCLUSIVE", "" ) + ;
              IIF( Empty( cErr ), " - it is in use by another user", ": " + cErr ) )
        RETURN .F.
    ENDIF

RETURN AddArea( cAlias, cFile, cDriver, cKind, cCdp )

/* window, browse and area record for the open work area cAlias */
STATIC FUNCTION AddArea( cAlias, cFile, cDriver, cKind, cCdp, nConn )

    LOCAL oWin, aArea

    oWin := TableWindow():new()
    oWin:oIdle := HTStatusBar():new( oWin )
    oWin:oBrw  := CBBrowse():new( oWin )
    oWin:onWindowKey := {| o, nKey | WindowKey( o, nKey ) }
    oWin:onActivity  := {| o | SyncArea( o ) }
    oWin:onClose     := {| o | CloseArea( AreaOfWin( o ) ) }
    AEval( Array( IDLE_SECTIONS ), {|| oWin:oIdle:addSection( "" ) } )

    oWin:oBrw:editable := .T.
    oWin:oBrw:alignLeft := .T.
    oWin:oBrw:onBeginEdit := {| nRow, nCol | CanEdit( nRow, nCol ) }
    oWin:oBrw:onKeyPress  := {| nKey | BrowseKey( nKey ) }
    oWin:oBrw:onMoved     := {|| AfterMove() }
    oWin:oBrw:onCellClick := {| nRow, nCol | CellClick( nRow, nCol ) }
    oWin:oBrw:contextMenu := ContextMenu()

    aArea := Array( AR_LEN )
    aArea[ AR_ALIAS ]   := cAlias
    aArea[ AR_FILE ]    := cFile
    aArea[ AR_DRIVER ]  := cDriver
    aArea[ AR_BAGS ]    := {}
    aArea[ AR_WIN ]     := oWin
    aArea[ AR_BRW ]     := oWin:oBrw
    aArea[ AR_IDLE ]    := oWin:oIdle
    aArea[ AR_KIND ]    := cKind
    aArea[ AR_RELS ]    := {}
    aArea[ AR_IDLETXT ] := AFill( Array( IDLE_SECTIONS ), "" )
    aArea[ AR_LASTREC ] := 0
    aArea[ AR_CDP ]     := hb_defaultValue( cCdp, "" )
    aArea[ AR_COLOR ]   := NIL
    aArea[ AR_MEMOW ]   := s_nMemoWidth
    aArea[ AR_CONN ]    := nConn
    AAdd( s_aAreas, aArea )
    s_nArea := Len( s_aAreas )
    dbSelectArea( cAlias )

    BuildBrowse( oWin:oBrw:browse(), cAlias )
    CascadePlace( oWin, Len( s_aAreas ) )
    oWin:setWindowTitle( cAlias )
    ShowAreaStatus( s_nArea )
    oWin:show()

RETURN .T.

/*---------------------------------------------------------------- LetoDB / LetoDBf */

/* "host:port" / "//host:port" / "//host:port/dir" -> "//host:port/dir/" */
STATIC FUNCTION LetoAddr( cAddr )

    cAddr := StrTran( AllTrim( cAddr ), "\", "/" )
    IF Empty( cAddr )
        RETURN ""
    ENDIF
    IF ! Left( cAddr, 2 ) == "//"
        cAddr := "//" + cAddr
    ENDIF
    IF ! ":" $ SubStr( cAddr, 3 )
        cAddr := Left( cAddr, At( "/", SubStr( cAddr, 3 ) + "/" ) + 1 ) + ":2812" + SubStr( cAddr, At( "/", SubStr( cAddr, 3 ) + "/" ) + 2 )
    ENDIF
    IF ! Right( cAddr, 1 ) == "/"
        cAddr += "/"
    ENDIF

RETURN cAddr

/* "//host:port/" of a server path */
STATIC FUNCTION LetoServer( cPath )

    LOCAL n := At( "/", SubStr( cPath, 3 ) )

RETURN IIF( n == 0, cPath + "/", Left( cPath, n + 2 ) )

/* connects to the server of cPath once; .T. when connected */
STATIC FUNCTION LetoConnect( cPath )

    LOCAL cServer := LetoServer( cPath ), nConn := -1, cErr

    cErr := Try( {|| nConn := leto_Connect( cServer ) } )
    IF nConn < 0
        Warn( "No connection to the LetoDB server " + cServer + IIF( Empty( cErr ), "", ": " + cErr ) + ;
              hb_eol() + "Is the server running? GlobalSet > LetoDB Server sets the address." )
        RETURN .F.
    ENDIF
    s_lLetoConn := .T.

RETURN .T.

STATIC FUNCTION OpenLeto( cPath, lReadOnly )
    IF ! LetoConnect( cPath )
        RETURN .F.
    ENDIF
RETURN OpenTable( cPath, "LETO", NIL, lReadOnly )

/* Database > Open from LetoDB Server: the server's files, then open one */
STATIC PROCEDURE LetoDialog()

    LOCAL aAsk := { { "Server (//host:port/):", PadR( s_cLetoAddr, 80 ), "@S40" } }, cServer, cFile

    IF ! Ask( "Open from LetoDB Server", aAsk )
        RETURN
    ENDIF
    cServer := LetoAddr( aAsk[ 1 ][ 2 ] )
    IF Empty( cServer ) .OR. ! LetoConnect( cServer )
        RETURN
    ENDIF
    s_cLetoAddr := cServer
    cFile := FileDialog( "LetoDB " + cServer, "*.dbf", cServer )
    IF ! Empty( cFile )
        OpenTable( cFile, "LETO" )
    ENDIF

RETURN

STATIC PROCEDURE LetoServerDlg()

    LOCAL aAsk := { { "LetoDB server (//host:port/):", PadR( s_cLetoAddr, 80 ), "@S40" } }

    IF Ask( "LetoDB / LetoDBf Server", aAsk )
        s_cLetoAddr := LetoAddr( aAsk[ 1 ][ 2 ] )
    ENDIF

RETURN

/* files of a server directory: { { cName, nSize, dDate, cTime, cAttr }, ... } */
STATIC FUNCTION LetoFiles( cServerDir, cSpec )

    LOCAL aDir := {}

    Try( {|| aDir := leto_Directory( cServerDir + cSpec, "D" ) } )

RETURN IIF( HB_ISARRAY( aDir ), aDir, {} )

/*---------------------------------------------------------------- xlsx */

STATIC PROCEDURE SelectXlsx()

    LOCAL cFile := FileDialog( "Open Excel Sheet", "*.xlsx" )

    IF ! Empty( cFile )
        OpenXlsx( cFile )
    ENDIF

RETURN

/* a sheet of an .xlsx workbook as a memory table (Copy To saves it) */
STATIC FUNCTION OpenXlsx( cFile )

    LOCAL oBook, cErr := "", aNames, nSheet := 1, aRows, aRes, aStruct, nRow, aRow, n, x
    LOCAL cAlias, cMem, aAsk

    cFile := FullPath( AllTrim( cFile ) )
    Busy( "Reading " + hb_FNameNameExt( cFile ) + " ..." )
    oBook := CBX_Open( cFile, @cErr )
    IF oBook == NIL
        Warn( cErr )
        RETURN .F.
    ENDIF
    aNames := oBook:SheetNames()
    IF Len( aNames ) > 1 .AND. ( nSheet := Choose( "Sheet of " + hb_FNameNameExt( cFile ), aNames, 1 ) ) == 0
        RETURN .F.
    ENDIF
    aRows := CBX_SheetRows( oBook, nSheet )
    IF Empty( aRows )
        Info( aNames[ nSheet ] + " is empty" )
        RETURN .F.
    ENDIF
    aAsk := { { "First row holds the column names:", .T., "Y" } }
    IF ! Ask( hb_FNameNameExt( cFile ) + " - " + aNames[ nSheet ], aAsk )
        RETURN .F.
    ENDIF
    aRes := CBX_ToStruct( aRows, aAsk[ 1 ][ 2 ] )
    aStruct := aRes[ 1 ]
    IF Empty( aStruct )
        Info( aNames[ nSheet ] + " has no columns" )
        RETURN .F.
    ENDIF

    cAlias := NewAlias( hb_FNameName( cFile ) + "_" + aNames[ nSheet ] )
    IF cAlias == NIL
        RETURN .F.
    ENDIF
    cMem := "mem:" + cAlias + ".dbf"
    cErr := Try( {|| dbCreate( cMem, aStruct, "DBFCDX" ), dbUseArea( .T., "DBFCDX", cMem, cAlias, .F. ) } )
    IF ! Empty( cErr ) .OR. Select( cAlias ) == 0
        Warn( "Cannot make the memory table: " + cErr )
        RETURN .F.
    ENDIF
    FOR nRow := aRes[ 2 ] TO Len( aRows )
        aRow := aRows[ nRow ]
        ( cAlias )->( dbAppend() )
        FOR n := 1 TO Min( Len( aRow ), Len( aStruct ) )
            IF ( x := aRow[ n ] ) != NIL
                ( cAlias )->( FieldPut( n, CellAs( x, aStruct[ n ][ 2 ] ) ) )
            ENDIF
        NEXT
    NEXT
    ( cAlias )->( dbGoTop() )

RETURN AddArea( cAlias, cFile, "DBFCDX", "XLSX", "" )

/* a sheet value as the type of its field */
STATIC FUNCTION CellAs( x, cType )

    DO CASE
    CASE cType == ValType( x )
        RETURN x
    CASE cType $ "CM"
        RETURN CBX_AsText( x )
    CASE cType == "N"
        RETURN IIF( HB_ISSTRING( x ), Val( x ), 0 )
    CASE cType == "D"
        RETURN IIF( HB_ISSTRING( x ), CToD( x ), CToD( "" ) )
    CASE cType == "L"
        RETURN HB_ISSTRING( x ) .AND. Upper( Left( AllTrim( x ), 1 ) ) $ "TYD"
    ENDCASE

RETURN x

/*---------------------------------------------------------------- SQL (SQLMIX: ODBC, SQLite) */

/* connection handle for cType ("ODBC" / "SQLITE3") + cConn, made once */
STATIC FUNCTION SqlConnect( cType, cConn )

    LOCAL n, nHandle := 0, cErr

    FOR n := 1 TO Len( s_aSqlConns )
        IF s_aSqlConns[ n ][ 1 ] == cType .AND. s_aSqlConns[ n ][ 2 ] == cConn
            RETURN s_aSqlConns[ n ][ 3 ]
        ENDIF
    NEXT
    cErr := Try( {|| nHandle := rddInfo( RDDI_CONNECT, { cType, cConn }, "SQLMIX" ) } )
    IF Empty( nHandle )
        Warn( "SQL connection failed: " + cType + " " + cConn + hb_eol() + ;
              IIF( Empty( cErr ), SqlError(), cErr ) )
        RETURN 0
    ENDIF
    AAdd( s_aSqlConns, { cType, cConn, nHandle } )

RETURN nHandle

STATIC FUNCTION SqlError( nConn )

    LOCAL cText := "", xNo := NIL

    Try( {|| xNo := rddInfo( RDDI_ERRORNO, NIL, "SQLMIX", nConn ), cText := rddInfo( RDDI_ERROR, NIL, "SQLMIX", nConn ) } )

RETURN hb_CStr( cText ) + IIF( Empty( xNo ), "", " (" + hb_CStr( xNo ) + ")" )

STATIC PROCEDURE SqlDisconnectAll()

    LOCAL aConn

    FOR EACH aConn IN s_aSqlConns
        Try( {|| rddInfo( RDDI_DISCONNECT, NIL, "SQLMIX", aConn[ 3 ] ) } )
    NEXT
    s_aSqlConns := {}

RETURN

/* SQL Query: connection, statement; SELECT opens a window (the rows are a
   copy in memory: changes stay there - Copy To saves them), other statements
   run on the server */
STATIC PROCEDURE SqlDialog()

    LOCAL dlg, oType, oConn, oEdit, oBtn, nW, nH, cConn := s_cSqlConn, nResult := 0, oHint
    LOCAL aTypes := { "SQLITE3", "ODBC" }

    nW := Min( 92, ht_ScreenMaxCol() - 4 )
    nH := Min( 24, ht_ScreenMaxRow() - 4 )
    dlg := HTDialog():new( NIL, "SQL Query  (SQLite file or ODBC connection string)" )
    dlg:resize( nW, nH )

    HTLabel():new( "Driver:", dlg ):move( 2, 1 )
    oType := HTComboBox():new( dlg )
    AEval( aTypes, {| c | oType:addItem( c ) } )
    oType:setCurrentIndex( Max( 1, AScan( aTypes, s_cSqlType ) ) )
    oType:setGeometry( 14, 1, 12, 1 )

    HTLabel():new( "Connection:", dlg ):move( 2, 3 )
    oConn := HTLineEdit():new( dlg )
    oConn:setGeometry( 14, 3, nW - 30, 1 )
    oConn:text := cConn
    oBtn := HTPushButton():new( "File...", dlg )
    oBtn:setGeometry( nW - 14, 3, 10, 1 )
    oBtn:onClicked := {|| cConn := FileDialog( "SQLite database", "*.*" ), ;
                          IIF( Empty( cConn ), NIL, ( oConn:text := cConn, dlg:repaintChild( oConn ) ) ) }
    oHint := HTLabel():new( "ODBC: DSN=name;UID=user;PWD=... or Driver={...};Server=...;Database=...", dlg )
    oHint:move( 14, 4 )

    HTLabel():new( "SQL statement (SELECT opens a window, others run on the server):", dlg ):move( 2, 6 )
    oEdit := HTTextEdit():new( dlg )
    oEdit:setGeometry( 2, 7, nW - 6, nH - 13 )
    oEdit:text := IIF( Empty( ScriptVar( "sql" ) ), "SELECT * FROM ", ScriptVar( "sql" ) )

    oBtn := HTPushButton():new( "Execute", dlg )
    oBtn:setGeometry( nW - 30, nH - 5, 12, 1 )
    oBtn:onClicked := {|| nResult := 1, dlg:accept() }
    oBtn := HTPushButton():new( "Cancel", dlg )
    oBtn:setGeometry( nW - 16, nH - 5, 12, 1 )
    oBtn:onClicked := {|| dlg:reject() }

    IF dlg:exec() != HT_DIALOG_ACCEPTED .OR. nResult == 0
        RETURN
    ENDIF
    s_cSqlType := aTypes[ Max( 1, oType:currentIndex() ) ]
    s_cSqlConn := AllTrim( oConn:text )
    ScriptVar( "sql", oEdit:text )
    SqlRun( s_cSqlType, s_cSqlConn, AllTrim( oEdit:text ) )

RETURN

STATIC PROCEDURE SqlRun( cType, cConn, cSql )

    LOCAL nConn, cAlias, cErr, cHead, nAffected := 0

    IF Empty( cConn ) .OR. Empty( cSql )
        Warn( "Connection and SQL statement are needed" )
        RETURN
    ENDIF
    IF cType == "SQLITE3"
        cConn := FullPath( cConn )
    ENDIF
    IF ( nConn := SqlConnect( cType, cConn ) ) == 0
        RETURN
    ENDIF
    DO WHILE Right( cSql, 1 ) $ "; " + Chr( 13 ) + Chr( 10 )
        cSql := Left( cSql, Len( cSql ) - 1 )
    ENDDO
    cHead := Upper( Left( LTrim( StrTran( StrTran( cSql, Chr( 13 ), " " ), Chr( 10 ), " " ) ), 7 ) )
    IF Left( cHead, 6 ) == "SELECT" .OR. Left( cHead, 4 ) == "WITH" .OR. Left( cHead, 6 ) == "PRAGMA" .OR. ;
       Left( cHead, 4 ) == "SHOW" .OR. Left( cHead, 7 ) == "EXPLAIN" .OR. Left( cHead, 4 ) == "DESC"
        cAlias := NewAlias( "SQL", .T. )
        cErr := Try( {|| dbUseArea( .T., "SQLMIX", cSql, cAlias, NIL, NIL, NIL, nConn ) } )
        IF Select( cAlias ) == 0
            Warn( "Query failed: " + IIF( Empty( cErr ), "", cErr + hb_eol() ) + SqlError( nConn ) )
            RETURN
        ENDIF
        ( cAlias )->( dbGoBottom() )          /* SQLMIX counts the rows read so far */
        ( cAlias )->( dbGoTop() )
        AddArea( cAlias, cSql, "SQLMIX", "SQL", "", nConn )
        Toast( hb_ntos( ( cAlias )->( LastRec() ) ) + " rows" )
    ELSE
        cErr := Try( {|| rddInfo( RDDI_EXECUTE, cSql, "SQLMIX", nConn ) } )
        IF ! Empty( cErr ) .OR. ! Empty( SqlErrorNo( nConn ) )
            Warn( "Statement failed: " + IIF( Empty( cErr ), "", cErr + hb_eol() ) + SqlError( nConn ) )
            RETURN
        ENDIF
        Try( {|| nAffected := rddInfo( RDDI_AFFECTEDROWS, NIL, "SQLMIX", nConn ) } )
        Info( "Done" + IIF( HB_ISNUMERIC( nAffected ), " - " + hb_ntos( nAffected ) + " rows affected", "" ) )
    ENDIF

RETURN

STATIC FUNCTION SqlErrorNo( nConn )

    LOCAL xNo := 0

    Try( {|| xNo := rddInfo( RDDI_ERRORNO, NIL, "SQLMIX", nConn ) } )

RETURN IIF( HB_ISNUMERIC( xNo ), xNo, 0 )

/*---------------------------------------------------------------- file names, aliases */

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

/* alias from the file name (BroPlus Auto-Alias), unique; with Auto-Alias
   OFF the user names it (NIL = cancelled) */
STATIC FUNCTION NewAlias( cFile, lAuto )

    LOCAL cBase := Upper( hb_FNameName( StrTran( cFile, "/", hb_ps() ) ) ), cAlias := "", cTry, c, n := 1, aAsk

    FOR EACH c IN cBase
        cAlias += IIF( ( c >= "A" .AND. c <= "Z" ) .OR. IsDigit( c ) .OR. c == "_", c, "_" )
    NEXT
    IF Empty( cAlias ) .OR. IsDigit( Left( cAlias, 1 ) )
        cAlias := "T_" + cAlias
    ENDIF
    cAlias := Left( cAlias, 20 )
    cTry := cAlias
    DO WHILE Select( cTry ) > 0
        cTry := cAlias + "_" + hb_ntos( ++n )
    ENDDO
    IF ! s_lAutoAlias .AND. ! hb_defaultValue( lAuto, .F. )
        aAsk := { { "Alias:", PadR( cTry, 20 ), "@!" } }
        IF ! Ask( "Alias for " + hb_FNameNameExt( cFile ), aAsk )
            RETURN NIL
        ENDIF
        cTry := AllTrim( aAsk[ 1 ][ 2 ] )
        IF Empty( cTry ) .OR. Select( cTry ) > 0
            Warn( "Alias " + cTry + " is empty or in use" )
            RETURN NIL
        ENDIF
    ENDIF

RETURN cTry

/* Database > Alias Name: renames the alias (closes and opens the table) */
STATIC PROCEDURE AliasDlg()

    LOCAL aArea := s_aAreas[ s_nArea ], aAsk := { { "New alias:", PadR( aArea[ AR_ALIAS ], 20 ), "@!" } }, cNew

    IF ! aArea[ AR_KIND ] $ "DBF LETO"
        Info( "The alias of a " + aArea[ AR_KIND ] + " table stays " + aArea[ AR_ALIAS ] )
        RETURN
    ENDIF
    IF ! Ask( "Alias Name - " + Alias(), aAsk )
        RETURN
    ENDIF
    cNew := AllTrim( aAsk[ 1 ][ 2 ] )
    IF Empty( cNew ) .OR. cNew == aArea[ AR_ALIAS ]
        RETURN
    ENDIF
    IF Select( cNew ) > 0 .OR. ! IsAlpha( cNew )
        Warn( "Alias " + cNew + " is in use or invalid" )
        RETURN
    ENDIF
    Reopen( ! dbInfo( DBI_SHARED ), cNew )
    RefreshCur()

RETURN

/*---------------------------------------------------------------- columns */

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
    ApplyColorCond( AreaByAlias( cAlias ) )

RETURN

/* column of a field; numbers with a right-aligned heading, logical values
   as a check box, memos as <Memo> or their first characters */
STATIC FUNCTION FieldColumn( cAlias, aField )

    LOCAL nPos := ( cAlias )->( FieldPos( aField[ 1 ] ) )
    LOCAL cType := aField[ 2 ], oCol, bRaw := FieldRW( cAlias, nPos ), nW, cPict

    DO CASE
    CASE cType == "M"
        oCol := TBColumnNew( aField[ 1 ], {|| MemoCell( cAlias, nPos ) } )
    CASE cType == "L"
        nW := Max( 3, Len( aField[ 1 ] ) )
        oCol := TBColumnNew( PadC( aField[ 1 ], nW ), {|| PadC( CheckBox( ( cAlias )->( FieldGet( nPos ) ) ), nW ) } )
    CASE cType $ "NIBYF"
        cPict := NumPicture( aField[ 3 ], aField[ 4 ] )
        nW := Max( Len( Transform( 0, cPict ) ), Len( aField[ 1 ] ) )
        oCol := TBColumnNew( PadL( aField[ 1 ], nW ), bRaw )
        oCol:picture := cPict
    CASE IsEditable( cType )
        oCol := TBColumnNew( aField[ 1 ], bRaw )
        /* the field length, not the first value: TBrowse counts its characters,
           so a first value in Chinese / UTF-8 made the column too narrow */
        IF cType == "C"
            oCol:width := Min( 50, Max( Len( aField[ 1 ] ), aField[ 3 ] ) )
        ENDIF
    CASE cType $ "WGP"
        oCol := TBColumnNew( aField[ 1 ], {|| "<Binary>" } )
    OTHERWISE
        oCol := TBColumnNew( aField[ 1 ], {|| ( cAlias )->( FieldGet( nPos ) ) } )
    ENDCASE
    oCol:cargo := { aField[ 1 ], cType, cAlias, bRaw }

RETURN oCol

STATIC FUNCTION CheckBox( l )
RETURN IIF( HB_ISLOGICAL( l ) .AND. l, "[" + ht_glyph( 0x221A, "X" ) + "]", "[ ]" )

/* memo cell: <Memo> / blank, or the first characters on one line */
STATIC FUNCTION MemoCell( cAlias, nPos )

    LOCAL n := AreaByAlias( cAlias )

RETURN CBM_Cell( ( cAlias )->( FieldGet( nPos ) ), IIF( n > 0, s_aAreas[ n ][ AR_MEMOW ], 0 ) )

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

/* the field value of a column (its shown text may be a check box) */
STATIC FUNCTION ColValue( oCol )
    IF Len( oCol:cargo ) >= CC_RAW .AND. oCol:cargo[ CC_RAW ] != NIL
        RETURN Eval( oCol:cargo[ CC_RAW ] )
    ENDIF
RETURN Eval( oCol:block )

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
    IF s_aAreas[ n ][ AR_KIND ] == "XLSX"
        Try( {|| dbDrop( "mem:" + cAlias + ".dbf", NIL, "DBFCDX" ) } )
    ENDIF
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
        s_aAreas[ n ][ AR_WIN ]:place( 0, nY, WindowWidth( n, ht_ScreenMaxCol() + 1 ), IIF( n == nCount, nTop + nRows - nY, nH ) - 1 )
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
    LOCAL nW := WindowWidth( n, ht_ScreenMaxCol() + 1 - 1 - nStep * 2 )
    LOCAL nH := Max( 10, ht_ScreenMaxRow() - 2 - 12 )

    oWin:place( 1 + nStep * 2, 2 + nStep * 2, nW, nH )

RETURN

/* width of the window of table n: its columns (+ separators), at least
   what the status bar needs, at most nMax */
STATIC FUNCTION WindowWidth( n, nMax )

    LOCAL oTB, oCol, nW := 0, i

    IF n < 1 .OR. n > Len( s_aAreas )
        RETURN nMax
    ENDIF
    oTB := s_aAreas[ n ][ AR_BRW ]:browse()
    FOR i := 1 TO oTB:colCount
        oCol := oTB:getColumn( i )
        nW += ColumnWidth( oCol, s_aAreas[ n ][ AR_ALIAS ] ) + IIF( i > 1, 1, 0 )
        IF nW >= nMax
            RETURN nMax
        ENDIF
    NEXT

RETURN Max( Min( nW + 2, nMax ), Min( 66, nMax ) )

/* the width TBrowse gives a column: its width, else heading or value */
STATIC FUNCTION ColumnWidth( oCol, cAlias )

    LOCAL nW := 0, x, cHead

    IF HB_ISNUMERIC( oCol:width ) .AND. oCol:width > 0
        RETURN oCol:width
    ENDIF
    FOR EACH cHead IN hb_ATokens( hb_defaultValue( oCol:heading, "" ), ";" )
        nW := Max( nW, ht_strWidth( cHead ) )
    NEXT
    BEGIN SEQUENCE WITH {| e | Break( e ) }
        x := ( cAlias )->( Eval( oCol:block ) )
        nW := Max( nW, ht_strWidth( IIF( Empty( oCol:picture ), hb_CStr( x ), Transform( x, oCol:picture ) ) ) )
        IF oCol:cargo[ CC_TYPE ] == "C"
            nW := Max( nW, Min( 50, ( cAlias )->( FieldLen( FieldPos( oCol:cargo[ CC_NAME ] ) ) ) ) )
        ELSEIF oCol:cargo[ CC_TYPE ] == "D"
            nW := Max( nW, Len( DToC( Date() ) ) )
        ENDIF
    END SEQUENCE

RETURN nW

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
    LOCAL oTB := aArea[ AR_BRW ]:browse(), oWin := aArea[ AR_WIN ], cTitle, cMode

    dbSelectArea( aArea[ AR_ALIAS ] )

    FOR EACH aRel IN aArea[ AR_RELS ]
        cRel += IIF( cRel == "", "", "," ) + aRel[ REL_CHILD ]
    NEXT
    DO CASE
    CASE aArea[ AR_KIND ] == "SQL"
        cMode := "SQL copy"
    CASE aArea[ AR_KIND ] == "XLSX"
        cMode := "xlsx in memory"
    OTHERWISE
        cMode := IIF( dbInfo( DBI_SHARED ), "Shared", "Exclusive" ) + IIF( dbInfo( DBI_ISREADONLY ), " R/O", "" ) + ;
                 IIF( aArea[ AR_KIND ] == "LETO", " Leto", "" )
    ENDCASE
    cKey := ""
    IF IndexOrd() > 0
        cKey := " Key: " + OrdKey() + IIF( Empty( OrdFor() ), "", "  For: " + OrdFor() )
    ENDIF
    IF Eof() .AND. LastRec() == 0
        cKey += "  (empty - Ins appends a record)"
    ENDIF
    aText := { ;
        " Rec. " + hb_ntos( RecNo() ) + "/" + hb_ntos( LastRec() ) + " ", ;
        " <Lock: " + hb_ntos( oTB:freeze ) + "> ", ;
        " <" + cMode + "> ", ;
        " <Softseek " + IIF( s_lSoftSeek, "On", "Off" ) + "> ", ;
        IIF( Empty( dbFilter() ), "", " <Filter>" ) + IIF( HasScope(), " <Scope>", "" ) + ;
            IIF( cRel == "", "", " <Rel: " + cRel + ">" ) + ;
            IIF( Set( _SET_DELETED ), " <Deleted On>", IIF( Deleted(), " <Deleted>", "" ) ) + ;
            IIF( Empty( aArea[ AR_CDP ] ), "", " <" + aArea[ AR_CDP ] + ">" ) + IndexFlags() + cKey + " " }
    FOR i := 1 TO IDLE_SECTIONS
        IF ! aText[ i ] == aArea[ AR_IDLETXT ][ i ]
            aArea[ AR_IDLETXT ][ i ] := aText[ i ]
            aArea[ AR_IDLE ]:setSection( i, aText[ i ] )
        ENDIF
    NEXT

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
    LOCAL cKind := s_aAreas[ n ][ AR_KIND ]
    LOCAL aArea, aRel

    IF cKind == "XLSX"
        cTitle += "  " + hb_FNameNameExt( s_aAreas[ n ][ AR_FILE ] )
    ELSEIF cKind == "LETO"
        cTitle += "  " + LetoServer( s_aAreas[ n ][ AR_FILE ] )
    ELSEIF cKind == "SQL"
        cTitle += "  " + Left( StrTran( StrTran( s_aAreas[ n ][ AR_FILE ], Chr( 13 ), " " ), Chr( 10 ), " " ), 40 )
    ENDIF
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
            s_oMsg:setSection( s_nMsgBase + i, aText[ i ] )
        ENDIF
    NEXT

RETURN

/* section 1 of the bottom line follows the input language (Win+Space) */
STATIC PROCEDURE ShowInputLang()

    LOCAL cLang := ht_inputLanguage()

    IF ! cLang == s_cInputLang
        s_cInputLang := cLang
        s_oMsg:setSection( 1, " " + cLang + " " )
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
    CASE K_SPACE
        IF CurColumn() != NIL .AND. CurColumn():cargo[ CC_TYPE ] == "L"
            ToggleLogical()
            RETURN .T.
        ENDIF
        EXIT
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
        Beep()
    ENDIF
    RefreshCur()

RETURN

STATIC PROCEDURE ToggleIncSeek()
    s_lIncSeek := ! s_lIncSeek
    s_cSeek := ""
RETURN

/* a click on a logical cell toggles it */
STATIC PROCEDURE CellClick( nRow, nCol )

    LOCAL oTB, oCol

    HB_SYMBOL_UNUSED( nRow )
    IF s_nArea == 0
        RETURN
    ENDIF
    oTB := CurTB()
    IF nCol >= 1 .AND. nCol <= oTB:colCount
        oCol := oTB:getColumn( nCol )
        IF oCol:cargo[ CC_TYPE ] == "L"
            ToggleLogical()
        ENDIF
    ENDIF

RETURN

/* Space, Enter or a click on a logical field: .T. <-> .F. */
STATIC PROCEDURE ToggleLogical()

    LOCAL oCol := CurColumn()

    IF oCol == NIL .OR. ! oCol:cargo[ CC_TYPE ] == "L" .OR. Eof()
        RETURN
    ENDIF
    IF ! s_lModify
        Toast( "Modify is DISABLED (F6 GlobalSet)" )
        RETURN
    ENDIF
    IF ! oCol:cargo[ CC_ALIAS ] == Alias() .OR. dbInfo( DBI_ISREADONLY )
        Toast( "This field cannot be changed" )
        RETURN
    ENDIF
    Eval( oCol:cargo[ CC_RAW ], ! ColValue( oCol ) )
    s_aAreas[ s_nArea ][ AR_BRW ]:refreshCurrent()
    ShowAreaStatus( s_nArea )

RETURN

/* Enter: edit the field (a memo in the memo window) */
STATIC PROCEDURE EditCell()

    LOCAL oCol := CurColumn()

    IF oCol != NIL
        IF oCol:cargo[ 2 ] == "M"
            EditMemo( oCol:cargo[ 1 ] )
        ELSEIF oCol:cargo[ 2 ] == "L"
            ToggleLogical()
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
    IF oCol:cargo[ 2 ] == "L"
        ToggleLogical()
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

/* memo window: text, JSON (checked, Format pretty prints it), an array (as
   a Harbour literal), a date, number or logical value; Store As changes
   the kind of value the memo holds */
STATIC PROCEDURE EditMemo( cField )

    LOCAL nPos := FieldPos( cField ), dlg, oEdit, nW, nH, x, cKind, cText, nAction, xNew, cErr, n
    LOCAL aKinds := { "TEXT", "JSON", "ARRAY", "DATE", "NUMBER", "LOGICAL" }
    LOCAL aNames := { "Text", "JSON text", "Array", "Date", "Number", "Logical" }

    IF nPos == 0 .OR. Eof()
        RETURN
    ENDIF
    x := FieldGet( nPos )
    cKind := CBM_Kind( x )
    IF cKind == "EMPTY"
        cKind := "TEXT"
    ENDIF
    cText := CBM_ToText( x, cKind )
    nW := Min( 96, ht_ScreenMaxCol() - 4 )
    nH := Min( 30, ht_ScreenMaxRow() - 4 )

    DO WHILE .T.
        nAction := 0
        dlg := HTDialog():new( NIL, Alias() + "->" + cField + "  Record " + hb_ntos( RecNo() ) + ;
                               "  [" + aNames[ AScan( aKinds, cKind ) ] + "]" )
        dlg:resize( nW, nH )
        oEdit := HTTextEdit():new( dlg )
        oEdit:setGeometry( 1, 1, nW - 4, nH - 6 )
        oEdit:text := cText
        IF s_lModify
            MemoButton( dlg, "Save", 2, nH - 4, {|| nAction := 1, dlg:accept() } )
            MemoButton( dlg, "Format", 14, nH - 4, {|| nAction := 2, dlg:accept() } )
            MemoButton( dlg, "Store As", 28, nH - 4, {|| nAction := 3, dlg:accept() } )
            MemoButton( dlg, "Cancel", nW - 14, nH - 4, {|| nAction := 0, dlg:reject() } )
        ELSE
            oEdit:_readOnly( .T. )
            MemoButton( dlg, "Close", nW - 14, nH - 4, {|| dlg:reject() } )
        ENDIF
        dlg:exec()
        cText := oEdit:text
        DO CASE
        CASE nAction == 1                        /* save as its kind */
            xNew := CBM_FromText( cText, cKind, @cErr )
            IF ! Empty( cErr )
                Warn( cErr )
                LOOP
            ENDIF
            PutField( Alias(), nPos, xNew )
            RefreshCur()
            EXIT
        CASE nAction == 2                        /* pretty print */
            xNew := CBM_FromText( cText, cKind, @cErr )
            IF ! Empty( cErr )
                Warn( cErr )
            ELSEIF cKind $ "JSON ARRAY"
                cText := CBM_ToText( xNew, cKind )
            ENDIF
        CASE nAction == 3                        /* another kind of value */
            IF ( n := Choose( "Store memo " + cField + " as", aNames, AScan( aKinds, cKind ) ) ) == 0
                LOOP
            ENDIF
            xNew := CBM_FromText( cText, cKind, @cErr )
            IF ! Empty( cErr )
                Warn( cErr )
                LOOP
            ENDIF
            IF cKind == "JSON" .AND. aKinds[ n ] == "ARRAY"
                hb_jsonDecode( xNew, @xNew )
                IF ! HB_ISARRAY( xNew )
                    Warn( "This JSON is an object { }, not an array [ ] - it stays JSON text" )
                    LOOP
                ENDIF
            ENDIF
            cKind := aKinds[ n ]
            cText := CBM_ToText( xNew, cKind )
        OTHERWISE
            EXIT
        ENDCASE
    ENDDO

RETURN

STATIC PROCEDURE MemoButton( dlg, cText, nX, nY, bAction )

    LOCAL oBtn := HTPushButton():new( cText, dlg )

    oBtn:setGeometry( nX, nY, Len( cText ) + 4, 1 )
    oBtn:onClicked := bAction

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
        s_xClipCell := ColValue( oCol )
        Toast( "Copied: " + Left( AllTrim( hb_CStr( s_xClipCell ) ), 40 ) )
    ENDIF

RETURN

STATIC PROCEDURE PasteField()

    LOCAL oCol := CurColumn()

    IF s_xClipCell == NIL .OR. oCol == NIL
        Toast( "Clipboard is empty - Shift-F4 copies a field" )
    ELSEIF ! s_lModify .OR. ! IsEditable( oCol:cargo[ 2 ] ) .OR. Eof()
        Toast( "This field cannot be changed" )
    ELSEIF ! ValType( ColValue( oCol ) ) == ValType( s_xClipCell )
        Warn( "Type mismatch: the clipboard holds " + ValType( s_xClipCell ) + ", the field is " + ValType( ColValue( oCol ) ) )
    ELSE
        Eval( oCol:cargo[ CC_RAW ], s_xClipCell )
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
        oCol:cargo := { cExpr, "E", Alias(), NIL }
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

    IF ! s_aAreas[ s_nArea ][ AR_KIND ] $ "DBF LETO"
        Info( "A " + s_aAreas[ s_nArea ][ AR_KIND ] + " table is in memory: it has no open mode" )
        RETURN
    ENDIF
    IF ! Reopen( lShared ) .AND. lShared
        Warn( Alias() + " is in use - cannot open it EXCLUSIVE" )
    ENDIF
    RefreshCur()

RETURN

/* closes and opens the current database again, keeping index files, order,
   filter, record and relations; .T. when opened in the wanted mode */
STATIC FUNCTION Reopen( lExclusive, cNewAlias )

    LOCAL aArea := s_aAreas[ s_nArea ], cAlias := aArea[ AR_ALIAS ], cOld := aArea[ AR_ALIAS ]
    LOCAL nRec := RecNo(), cOrder := OrdName(), cFilter := dbFilter(), cBag, lOk, aOther, aRel
    LOCAL xTop := ordScope( 0 ), xBottom := ordScope( 1 ), lReadOnly := dbInfo( DBI_ISREADONLY )
    LOCAL cCdp := IIF( Empty( aArea[ AR_CDP ] ), NIL, aArea[ AR_CDP ] )

    IF cNewAlias != NIL
        cAlias := cNewAlias
    ENDIF
    dbCloseArea()
    Try( {|| dbUseArea( .F., aArea[ AR_DRIVER ], aArea[ AR_FILE ], cAlias, ! lExclusive, lReadOnly, cCdp ) } )
    lOk := Used()
    IF ! lOk
        Try( {|| dbUseArea( .F., aArea[ AR_DRIVER ], aArea[ AR_FILE ], cAlias, .T., lReadOnly, cCdp ) } )
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
    IF ! cAlias == cOld          /* renamed: blocks and relations name the alias */
        aArea[ AR_ALIAS ] := cAlias
        BuildBrowse( aArea[ AR_BRW ]:browse(), cAlias )
        FOR EACH aOther IN s_aAreas
            FOR EACH aRel IN aOther[ AR_RELS ]
                IF aRel[ REL_CHILD ] == cOld
                    aRel[ REL_CHILD ] := cAlias
                ENDIF
            NEXT
        NEXT
        RepaintWindow( aArea[ AR_WIN ] )
    ENDIF
    ApplyAllRelations()

RETURN lOk

/* runs bAction with the current database EXCLUSIVE (PACK, ZAP, INDEX ON) */
STATIC FUNCTION WithExclusive( bAction )

    LOCAL lShared := dbInfo( DBI_SHARED ), cErr, nArea := s_nArea

    IF ! s_aAreas[ s_nArea ][ AR_KIND ] $ "DBF LETO"
        lShared := .F.            /* memory tables are exclusive */
    ENDIF
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

STATIC PROCEDURE ChooseTheme()

    LOCAL aNames := HTTheme():themeNames(), n

    IF ( n := Choose( "Theme", aNames, Max( 1, AScan( aNames, s_cTheme ) ) ) ) > 0
        s_cTheme := aNames[ n ]
        HTTheme():loadTheme( s_cTheme )
        HTApplication():repaintAll()
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

/*---------------------------------------------------------------- demo data */

/* CUSTOMER.DBF + ORDERS.DBF next to the program (made once) */
STATIC FUNCTION DemoTables( cDir )

    LOCAL cCust := cDir + "customer.dbf", cOrders := cDir + "orders.dbf"
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
            /* memos of every kind: JSON text, an array (FPT stores it as it is), text */
            DO CASE
            CASE n % 15 == 0
                FieldPut( 7, '{"email":"' + Lower( StrTran( FieldGet( 2 ), " ", "." ) ) + '@example.com",' + ;
                             '"tags":["vip","' + hb_ntos( 2000 + n % 26 ) + '"],"limit":' + hb_ntos( 1000 + n * 10 ) + '}' )
            CASE n % 10 == 0
                FieldPut( 7, { "Contact", { "phone", "036/" + StrZero( n * 37 % 1000, 3 ) + "-" + StrZero( n * 91 % 1000, 3 ) }, ;
                               { "since", 0d20000101 + n * 17 }, { "discount", ( n % 20 ) / 2 }, { "newsletter", n % 20 == 0 } } )
            CASE n % 6 == 0
                FieldPut( 7, "Customer since " + hb_ntos( 1990 + n % 30 ) + "." + hb_eol() + "Preferred contact: phone." )
            ENDCASE
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
    ShowLines( cTitle, aLines )
RETURN

STATIC FUNCTION IsDirItem( c )
RETURN HB_ISSTRING( c ) .AND. Right( c, 1 ) $ "\/"

STATIC FUNCTION SubDir( cDir, cItem )
    IF cItem == ".." + hb_ps()
        RETURN hb_FNameDir( Left( cDir, Len( cDir ) - 1 ) )
    ENDIF
RETURN cDir + cItem

STATIC FUNCTION ArraySkip( aRec, nLen, nRequest )

    LOCAL nActual := IIF( nRequest > 0, Min( nRequest, nLen - aRec[ 1 ] ), Max( nRequest, 1 - aRec[ 1 ] ) )

    aRec[ 1 ] += nActual

RETURN nActual

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

/*================================================================ CBro additions */

/*---------------------------------------------------------------- database selection */

STATIC PROCEDURE SelectDatabase()

    LOCAL cFile := FileDialog( "Database Selection", s_cDbfSpec + ";*.xlsx" )

    IF ! Empty( cFile )
        OpenAny( cFile, .F. )
    ENDIF

RETURN

/* Database / Index FileSpec: the masks the selection screens show */
STATIC PROCEDURE SpecDlg( lDbf )

    LOCAL aAsk := { { IIF( lDbf, "Database", "Index" ) + " file masks (; between):", ;
                      PadR( IIF( lDbf, s_cDbfSpec, NdxSpec() ), 60 ), "@S30" } }

    IF Ask( IIF( lDbf, "Database", "Index" ) + " FileSpec", aAsk )
        IF lDbf
            s_cDbfSpec := IIF( Empty( aAsk[ 1 ][ 2 ] ), "*.dbf", AllTrim( aAsk[ 1 ][ 2 ] ) )
        ELSE
            s_cNdxSpec := AllTrim( aAsk[ 1 ][ 2 ] )
        ENDIF
    ENDIF

RETURN

/* index masks: the FileSpec, else by the current table's driver */
STATIC FUNCTION NdxSpec()

    IF ! Empty( s_cNdxSpec )
        RETURN s_cNdxSpec
    ENDIF
    IF s_nArea > 0 .AND. s_aAreas[ s_nArea ][ AR_DRIVER ] == "DBFNTX"
        RETURN "*.ntx"
    ENDIF

RETURN "*.cdx"

STATIC PROCEDURE OpenIndexFile()

    LOCAL aArea := s_aAreas[ s_nArea ], cFile

    IF aArea[ AR_KIND ] == "SQL"
        Info( "A SQL result is indexed with Index > Create Index (in memory)" )
        RETURN
    ENDIF
    IF aArea[ AR_KIND ] == "LETO"
        cFile := FileDialog( "Index File Selection", NdxSpec(), hb_FNameDir( aArea[ AR_FILE ] ) )
    ELSE
        cFile := FileDialog( "Index File Selection", NdxSpec() )
    ENDIF
    IF ! Empty( cFile ) .AND. AddIndexFile( cFile )
        RefreshCur()
    ENDIF

RETURN

/* every order: keys in order and as many as the records it covers */
STATIC PROCEDURE VerifyIndex()

    LOCAL nOrder := IndexOrd(), nRec := RecNo(), n, aOut := {}, xKey, xPrev, nKeys, bKey, lOk, lDesc, cErr

    IF OrderCount() == 0
        Info( Alias() + " has no index open" )
        RETURN
    ENDIF
    FOR n := 1 TO OrderCount()
        ordSetFocus( n )
        lOk := .T.
        nKeys := 0
        xPrev := NIL
        lDesc := ordDescend()
        cErr := Try( {|| bKey := hb_macroBlock( OrdKey() ) } )
        IF Empty( cErr )
            dbGoTop()
            DO WHILE ! Eof()
                xKey := Eval( bKey )
                IF xPrev != NIL .AND. IIF( lDesc, xKey > xPrev, xKey < xPrev )
                    lOk := .F.
                ENDIF
                xPrev := xKey
                nKeys++
                dbSkip()
            ENDDO
            IF nKeys != ordKeyCount() .AND. Empty( dbFilter() ) .AND. ! Set( _SET_DELETED )
                lOk := .F.
            ENDIF
        ELSE
            lOk := .F.
        ENDIF
        AAdd( aOut, PadR( OrdName( n ), 11 ) + PadR( OrdKey( n ), 30 ) + " " + ;
              IIF( lOk, "OK", "DAMAGED - Command > Reindex" ) + "  (" + hb_ntos( nKeys ) + " keys)" )
    NEXT
    ordSetFocus( nOrder )
    dbGoto( nRec )
    ShowText( "Verify Index Integrity - " + Alias(), aOut )

RETURN

/* Alt-Z: an operating system command, its output in a window */
STATIC PROCEDURE RunCommand()

    LOCAL aAsk := { { "Command:", PadR( ScriptVar( "cmd" ), 200 ), "@S50" } }, cCmd, cOut := "", cErr := "", nRet

    IF ! Ask( "Run OS Command  (output is shown, no input)", aAsk )
        RETURN
    ENDIF
    cCmd := AllTrim( aAsk[ 1 ][ 2 ] )
    IF Empty( cCmd )
        RETURN
    ENDIF
    ScriptVar( "cmd", cCmd )
#ifdef __PLATFORM__WINDOWS
    nRet := hb_processRun( "cmd /c " + cCmd, NIL, @cOut, @cErr )
#else
    nRet := hb_processRun( "sh -c '" + StrTran( cCmd, "'", "'\''" ) + "'", NIL, @cOut, @cErr )
#endif
    ShowLines( cCmd + "  (exit " + hb_ntos( nRet ) + ")", hb_ATokens( StrTran( cOut + cErr, Chr( 13 ), "" ), Chr( 10 ) ) )

RETURN

/*---------------------------------------------------------------- color condition, memo width */

/* AreaSet > Color Condition: records FOR an expression in another color */
STATIC PROCEDURE ColorCondDlg()

    LOCAL aArea := s_aAreas[ s_nArea ], aCond := aArea[ AR_COLOR ], aAsk, bExpr, n
    LOCAL aNames := { "Red", "Green", "Blue", "Magenta", "Brown / dark yellow", "Cyan", "White", "Black", "Highlight (selection colors)" }
    LOCAL aFg := { "4", "2", "1", "5", "6", "3", "15", "0", "" }

    aAsk := { { "Color FOR (empty = none):", PadR( IIF( aCond == NIL, "", aCond[ 1 ] ), 200 ), "@S45" } }
    IF ! Ask( "Color Condition - " + Alias(), aAsk )
        RETURN
    ENDIF
    IF Empty( aAsk[ 1 ][ 2 ] )
        aArea[ AR_COLOR ] := NIL
    ELSE
        IF ( bExpr := ExprBlock( aAsk[ 1 ][ 2 ], "L" ) ) == NIL
            RETURN
        ENDIF
        IF ( n := Choose( "Color of the records FOR " + AllTrim( aAsk[ 1 ][ 2 ] ), aNames, 1 ) ) == 0
            RETURN
        ENDIF
        aArea[ AR_COLOR ] := { AllTrim( aAsk[ 1 ][ 2 ] ), bExpr, aFg[ n ] }
    ENDIF
    ApplyColorCond( s_nArea )
    RepaintWindow( aArea[ AR_WIN ] )

RETURN

/* colorBlock of every column: pair 7 (extraColors) for the records FOR the
   condition - the color on the browse background */
STATIC PROCEDURE ApplyColorCond( n )

    LOCAL aArea, oTB, oCol, cAlias, bCond, cBg, cColor, i

    IF n == 0 .OR. n > Len( s_aAreas )
        RETURN
    ENDIF
    aArea := s_aAreas[ n ]
    oTB := aArea[ AR_BRW ]:browse()
    cAlias := aArea[ AR_ALIAS ]
    IF aArea[ AR_COLOR ] == NIL
        aArea[ AR_BRW ]:extraColors := ""
        FOR i := 1 TO oTB:colCount
            oTB:getColumn( i ):colorBlock := {|| NIL }
        NEXT
    ELSE
        bCond := aArea[ AR_COLOR ][ 2 ]
        cBg := hb_tokenGet( HTTheme():getColor( HT_CLR_BROWSE_FOCUSED ), 1, "," )
        cBg := IIF( "/" $ cBg, SubStr( cBg, At( "/", cBg ) + 1 ), "0" )
        cColor := IIF( Empty( aArea[ AR_COLOR ][ 3 ] ), HTTheme():getColor( HT_CLR_BROWSE_SELECTED ), aArea[ AR_COLOR ][ 3 ] + "/" + cBg )
        aArea[ AR_BRW ]:extraColors := cColor
        FOR i := 1 TO oTB:colCount
            oCol := oTB:getColumn( i )
            oCol:colorBlock := {|| IIF( CondTrue( cAlias, bCond ), { 7, 2 }, { 1, 2 } ) }
        NEXT
    ENDIF
    oTB:configure()
    aArea[ AR_BRW ]:refreshAll()

RETURN

STATIC FUNCTION CondTrue( cAlias, bCond )

    LOCAL lTrue := .F.

    BEGIN SEQUENCE WITH {| e | Break( e ) }
        lTrue := ( cAlias )->( Eval( bCond ) )
    END SEQUENCE

RETURN HB_ISLOGICAL( lTrue ) .AND. lTrue

STATIC PROCEDURE MemoWidthDlg()

    LOCAL aArea := s_aAreas[ s_nArea ], aAsk := { { "Memo column width (0 = <Memo>):", aArea[ AR_MEMOW ], "999" } }

    IF Ask( "Memo Width - " + Alias(), aAsk )
        aArea[ AR_MEMOW ] := Max( 0, Min( aAsk[ 1 ][ 2 ], 200 ) )
        CurTB():configure()
        RefreshCur()
        RepaintWindow( aArea[ AR_WIN ] )
    ENDIF

RETURN

STATIC PROCEDURE DefMemoWidth()

    LOCAL aAsk := { { "Memo width of the next tables (0 = <Memo>):", s_nMemoWidth, "999" } }

    IF Ask( "Default Memo Width", aAsk )
        s_nMemoWidth := Max( 0, Min( aAsk[ 1 ][ 2 ], 200 ) )
    ENDIF

RETURN

/*---------------------------------------------------------------- views */

/* Alt-W: every open table with its index files, order, scope, filter,
   columns, color condition and relations, in a .cbv file (JSON) */
STATIC PROCEDURE SaveView()

    LOCAL aAsk := { { "View file:", PadR( IIF( Empty( ScriptVar( "view" ) ), "view.cbv", ScriptVar( "view" ) ), 120 ), "@S40" } }
    LOCAL aView := {}, aArea, cFile, nOld := Select()

    IF ! Ask( "Save Current View", aAsk )
        RETURN
    ENDIF
    cFile := FullPath( AllTrim( aAsk[ 1 ][ 2 ] ) )
    IF Empty( hb_FNameExt( cFile ) )
        cFile += ".cbv"
    ENDIF
    FOR EACH aArea IN s_aAreas
        dbSelectArea( aArea[ AR_ALIAS ] )
        AAdd( aView, AreaView( aArea ) )
    NEXT
    dbSelectArea( nOld )
    IF hb_MemoWrit( cFile, hb_jsonEncode( { "cbro" => CB_VERSION, "current" => s_nArea, "tables" => aView }, .T. ) )
        ScriptVar( "view", cFile )
        Toast( "View saved: " + hb_FNameNameExt( cFile ) )
    ELSE
        Warn( "Cannot write " + cFile )
    ENDIF

RETURN

/* the view of one table (its area selected) */
STATIC FUNCTION AreaView( aArea )

    LOCAL h := { => }, aCols := {}, oTB := aArea[ AR_BRW ]:browse(), oCol, n, aRels := {}, aRel

    h[ "kind" ]   := aArea[ AR_KIND ]
    h[ "file" ]   := aArea[ AR_FILE ]
    h[ "driver" ] := aArea[ AR_DRIVER ]
    h[ "alias" ]  := aArea[ AR_ALIAS ]
    h[ "cdp" ]    := aArea[ AR_CDP ]
    h[ "shared" ] := dbInfo( DBI_SHARED )
    h[ "bags" ]   := aArea[ AR_BAGS ]
    h[ "order" ]  := OrdName()
    h[ "filter" ] := dbFilter()
    h[ "recno" ]  := RecNo()
    h[ "freeze" ] := oTB:freeze
    h[ "memow" ]  := aArea[ AR_MEMOW ]
    h[ "color" ]  := IIF( aArea[ AR_COLOR ] == NIL, NIL, { aArea[ AR_COLOR ][ 1 ], aArea[ AR_COLOR ][ 3 ] } )
    IF aArea[ AR_KIND ] == "SQL"
        h[ "sqltype" ] := ""
        FOR EACH aRel IN s_aSqlConns
            IF aRel[ 3 ] == aArea[ AR_CONN ]
                h[ "sqltype" ] := aRel[ 1 ]
                h[ "sqlconn" ] := aRel[ 2 ]
            ENDIF
        NEXT
    ENDIF
    FOR n := 1 TO oTB:colCount
        oCol := oTB:getColumn( n )
        AAdd( aCols, { "name" => oCol:cargo[ CC_NAME ], "type" => oCol:cargo[ CC_TYPE ], ;
                       "heading" => oCol:heading, "width" => oCol:width, "picture" => oCol:picture } )
    NEXT
    h[ "columns" ] := aCols
    FOR EACH aRel IN aArea[ AR_RELS ]
        AAdd( aRels, { aRel[ REL_CHILD ], aRel[ REL_EXPR ], aRel[ REL_ONLY ] } )
    NEXT
    h[ "relations" ] := aRels

RETURN h

/* Alt-V: closes the open tables and opens the ones of a .cbv view */
STATIC PROCEDURE RestoreView()

    LOCAL cFile := FileDialog( "Restore View", "*.cbv" ), hView, h, aRel, n, nFirst, lOk, lShared, lOldExcl

    IF Empty( cFile )
        RETURN
    ENDIF
    hView := hb_jsonDecode( hb_MemoRead( cFile ) )
    IF ! HB_ISHASH( hView ) .OR. ! hb_HHasKey( hView, "tables" )
        Warn( hb_FNameNameExt( cFile ) + " is not a CBro view" )
        RETURN
    ENDIF
    IF Len( s_aAreas ) > 0 .AND. ! Confirm( "Close the open tables and restore " + hb_FNameNameExt( cFile ) + "?" )
        RETURN
    ENDIF
    DO WHILE Len( s_aAreas ) > 0
        SelectArea( Len( s_aAreas ), .F. )
        CloseDatabase()
    ENDDO
    ScriptVar( "view", cFile )
    nFirst := Len( s_aAreas ) + 1
    FOR EACH h IN hView[ "tables" ]
        lShared := hb_HGetDef( h, "shared", .T. )
        DO CASE
        CASE h[ "kind" ] == "XLSX"
            IF ! OpenXlsx( h[ "file" ] )
                LOOP
            ENDIF
        CASE h[ "kind" ] == "SQL"
            n := Len( s_aAreas )
            SqlRun( hb_HGetDef( h, "sqltype", "SQLITE3" ), hb_HGetDef( h, "sqlconn", "" ), h[ "file" ] )
            IF Len( s_aAreas ) == n
                LOOP
            ENDIF
        OTHERWISE
            IF h[ "kind" ] == "LETO" .AND. ! LetoConnect( h[ "file" ] )
                LOOP
            ENDIF
            lOldExcl := s_lExclusive
            s_lExclusive := ! lShared
            lOk := OpenTable( h[ "file" ], h[ "driver" ], IIF( Select( h[ "alias" ] ) == 0, h[ "alias" ], NIL ), .F., h[ "cdp" ] )
            s_lExclusive := lOldExcl
            IF ! lOk
                LOOP
            ENDIF
            AEval( h[ "bags" ], {| c | AddIndexFile( c ) } )
        ENDCASE
        ApplyAreaView( s_nArea, h )
    NEXT
    /* relations last: every child is open now */
    FOR EACH h IN hView[ "tables" ]
        IF ( n := AreaByAlias( h[ "alias" ] ) ) > 0
            FOR EACH aRel IN hb_HGetDef( h, "relations", {} )
                IF AreaByAlias( aRel[ 1 ] ) > 0
                    SetRelation( n, aRel[ 1 ], aRel[ 2 ], aRel[ 3 ] )
                ENDIF
            NEXT
        ENDIF
    NEXT
    IF Len( s_aAreas ) >= nFirst
        n := Max( nFirst, Min( hb_HGetDef( hView, "current", nFirst ), Len( s_aAreas ) ) )
        SelectArea( n, .T. )
        s_aAreas[ n ][ AR_WIN ]:activate()
    ENDIF
    Toast( "View restored: " + hb_FNameNameExt( cFile ) )

RETURN

STATIC PROCEDURE ApplyAreaView( n, h )

    LOCAL aArea := s_aAreas[ n ], oTB := aArea[ AR_BRW ]:browse(), aStruct, hCol, oCol, nPos, bExpr, xRec

    dbSelectArea( aArea[ AR_ALIAS ] )
    IF ! Empty( hb_HGetDef( h, "order", "" ) )
        Try( {|| ordSetFocus( h[ "order" ] ) } )
    ENDIF
    IF ! Empty( hb_HGetDef( h, "filter", "" ) ) .AND. ( bExpr := ExprBlock( h[ "filter" ], "L" ) ) != NIL
        dbSetFilter( bExpr, h[ "filter" ] )
    ENDIF
    aArea[ AR_MEMOW ] := hb_HGetDef( h, "memow", 0 )
    IF HB_ISARRAY( hb_HGetDef( h, "columns", NIL ) ) .AND. ! Empty( h[ "columns" ] )
        aStruct := dbStruct()
        DO WHILE oTB:colCount > 0
            oTB:delColumn( 1 )
        ENDDO
        FOR EACH hCol IN h[ "columns" ]
            oCol := NIL
            IF hCol[ "type" ] == "E"
                IF ( bExpr := ExprBlock( hCol[ "name" ] ) ) != NIL
                    oCol := TBColumnNew( hCol[ "name" ], bExpr )
                    oCol:cargo := { hCol[ "name" ], "E", Alias(), NIL }
                ENDIF
            ELSEIF ( nPos := AScan( aStruct, {| a | a[ 1 ] == hCol[ "name" ] } ) ) > 0
                oCol := FieldColumn( Alias(), aStruct[ nPos ] )
            ENDIF
            IF oCol != NIL
                oCol:heading := hb_HGetDef( hCol, "heading", oCol:heading )
                oCol:width := hb_HGetDef( hCol, "width", NIL )
                IF ! Empty( hb_HGetDef( hCol, "picture", NIL ) )
                    oCol:picture := hCol[ "picture" ]
                ENDIF
                oTB:addColumn( oCol )
            ENDIF
        NEXT
        IF oTB:colCount == 0
            BuildBrowse( oTB, Alias() )
        ENDIF
        oTB:freeze := Min( hb_HGetDef( h, "freeze", 0 ), oTB:colCount - 1 )
    ENDIF
    IF HB_ISARRAY( hb_HGetDef( h, "color", NIL ) ) .AND. ( bExpr := ExprBlock( h[ "color" ][ 1 ], "L" ) ) != NIL
        aArea[ AR_COLOR ] := { h[ "color" ][ 1 ], bExpr, h[ "color" ][ 2 ] }
    ENDIF
    ApplyColorCond( n )
    xRec := hb_HGetDef( h, "recno", 0 )
    IF HB_ISNUMERIC( xRec ) .AND. xRec > 0 .AND. xRec <= LastRec()
        dbGoto( xRec )
    ENDIF
    RefreshArea( n )

RETURN

STATIC PROCEDURE ViewInfo()

    LOCAL aOut := {}, aArea, h, hCol, aRel

    FOR EACH aArea IN s_aAreas
        dbSelectArea( aArea[ AR_ALIAS ] )
        h := AreaView( aArea )
        AAdd( aOut, h[ "alias" ] + "  (" + h[ "kind" ] + IIF( Empty( h[ "cdp" ] ), "", ", " + h[ "cdp" ] ) + ")  " + ;
              Left( StrTran( StrTran( h[ "file" ], Chr( 13 ), " " ), Chr( 10 ), " " ), 60 ) )
        AAdd( aOut, "   Order: " + IIF( Empty( h[ "order" ] ), "natural", h[ "order" ] + "  Key: " + OrdKey() ) + ;
              "   Filter: " + IIF( Empty( h[ "filter" ] ), "-", h[ "filter" ] ) )
        AAdd( aOut, "   Columns: " + hb_ntos( Len( h[ "columns" ] ) ) + "  Locked: " + hb_ntos( h[ "freeze" ] ) + ;
              "   Color: " + IIF( h[ "color" ] == NIL, "-", h[ "color" ][ 1 ] ) + "   Memo width: " + hb_ntos( h[ "memow" ] ) )
        FOR EACH hCol IN h[ "columns" ]
            AAdd( aOut, "      " + PadR( hCol[ "name" ], 24 ) + " " + hCol[ "type" ] )
        NEXT
        FOR EACH aRel IN h[ "relations" ]
            AAdd( aOut, "   Relation into " + aRel[ 1 ] + " ON " + aRel[ 2 ] + IIF( aRel[ 3 ], "  (related records only)", "" ) )
        NEXT
        AAdd( aOut, "" )
    NEXT
    dbSelectArea( s_aAreas[ s_nArea ][ AR_ALIAS ] )
    ShowLines( "Current View Info", aOut )

RETURN

/*---------------------------------------------------------------- globalset */

STATIC PROCEDURE ChooseDriver()

    LOCAL aDrivers := { "DBFCDX", "DBFNTX", "LETO" }, n

    IF ( n := Choose( "Database Driver (for the next database opened)", ;
                      { "DBFCDX  - FoxPro / Clipper 5.3 compound indexes (.cdx, .fpt)", ;
                        "DBFNTX  - Clipper indexes (.ntx, .dbt)", ;
                        "LETO    - LetoDB / LetoDBf server (GlobalSet > LetoDB Server)" }, ;
                      Max( 1, AScan( aDrivers, s_cDriver ) ) ) ) > 0
        s_cDriver := aDrivers[ n ]
        rddSetDefault( IIF( s_cDriver == "LETO", "DBFCDX", s_cDriver ) )
    ENDIF

RETURN

/* GlobalSet > Data Codepage: the codepage of the data in the tables; the
   RDD translates it for the screen (UTF-8) and back when writing */
STATIC PROCEDURE ChooseCodepage()

    LOCAL aCdp := hb_cdpList(), aItems := { "(as stored - no translation)" }, aIds := { "" }, c, n, nStart := 1
    LOCAL aArea

    ASort( aCdp,,, {| a, b | Upper( a ) < Upper( b ) } )
    FOR EACH c IN aCdp
        IF Left( c, 4 ) == "UTF8" .OR. Left( c, 5 ) == "UTF16"
            LOOP
        ENDIF
        AAdd( aItems, PadR( c, 9 ) + " " + hb_cdpInfo( c ) )
        AAdd( aIds, c )
        IF Upper( c ) == Upper( s_cCodepage )
            nStart := Len( aIds )
        ENDIF
    NEXT
    IF ( n := Choose( "Data Codepage (tables opened from now on)", aItems, nStart ) ) == 0
        RETURN
    ENDIF
    s_cCodepage := aIds[ n ]
    IF s_nArea > 0
        aArea := s_aAreas[ s_nArea ]
        IF aArea[ AR_KIND ] $ "DBF LETO" .AND. ! aArea[ AR_CDP ] == s_cCodepage .AND. ;
           Confirm( "Open " + aArea[ AR_ALIAS ] + " again with " + IIF( Empty( s_cCodepage ), "no translation", s_cCodepage ) + "?" )
            aArea[ AR_CDP ] := s_cCodepage
            Reopen( ! dbInfo( DBI_SHARED ) )
            RefreshCur()
            RepaintWindow( aArea[ AR_WIN ] )
        ENDIF
    ENDIF

RETURN

STATIC PROCEDURE SetDateFormat()

    LOCAL aFormats := { "dd.mm.yyyy", "mm/dd/yyyy", "dd/mm/yyyy", "yyyy-mm-dd", "yyyy.mm.dd", "dd-mm-yyyy", ;
                        "dd.mm.yy", "mm/dd/yy", "Other..." }, n, aAsk, cFmt

    n := AScan( aFormats, Set( _SET_DATEFORMAT ) )
    IF ( n := Choose( "Date Format", aFormats, IIF( n == 0, Len( aFormats ), n ) ) ) == 0
        RETURN
    ENDIF
    cFmt := aFormats[ n ]
    IF n == Len( aFormats )
        aAsk := { { "Date format (d m y, e.g. dd.mm.yyyy):", PadR( Set( _SET_DATEFORMAT ), 20 ), "" } }
        IF ! Ask( "Date Format", aAsk )
            RETURN
        ENDIF
        cFmt := Lower( AllTrim( aAsk[ 1 ][ 2 ] ) )
        IF ! "dd" $ cFmt .OR. ! "mm" $ cFmt .OR. ! "yy" $ cFmt
            Warn( "A date format needs dd, mm and yy or yyyy" )
            RETURN
        ENDIF
    ENDIF
    Set( _SET_DATEFORMAT, cFmt )
    FOR n := 1 TO Len( s_aAreas )
        s_aAreas[ n ][ AR_BRW ]:browse():configure()
        RefreshArea( n )
    NEXT

RETURN

STATIC FUNCTION IniFile()
RETURN hb_DirBase() + "cbro.ini"

STATIC PROCEDURE LoadSettings()

    LOCAL hIni := hb_iniRead( IniFile(), .T., "=", .F. ), h

    IF HB_ISHASH( hIni ) .AND. hb_HHasKey( hIni, "CBro" )
        h := hIni[ "CBro" ]
        s_cDriver    := hb_HGetDef( h, "Driver", s_cDriver )
        s_lExclusive := hb_HGetDef( h, "OpenMode", "SHARED" ) == "EXCLUSIVE"
        s_lModify    := hb_HGetDef( h, "Modify", "ENABLED" ) == "ENABLED"
        s_lImmediate := hb_HGetDef( h, "Edit", "PROTECTED" ) == "IMMEDIATE"
        s_lSoftSeek  := hb_HGetDef( h, "Softseek", "OFF" ) == "ON"
        s_cTheme     := hb_HGetDef( h, "Theme", s_cTheme )
        s_cCodepage  := hb_HGetDef( h, "Codepage", s_cCodepage )
        s_cLetoAddr  := hb_HGetDef( h, "LetoServer", s_cLetoAddr )
        s_cDbfSpec   := hb_HGetDef( h, "DbfSpec", s_cDbfSpec )
        s_cNdxSpec   := hb_HGetDef( h, "IndexSpec", s_cNdxSpec )
        s_lPreselect := hb_HGetDef( h, "PreselectIndexes", "ON" ) == "ON"
        s_lSound     := hb_HGetDef( h, "Sound", "ON" ) == "ON"
        s_lAutoAlias := hb_HGetDef( h, "AutoAlias", "ON" ) == "ON"
        s_nMemoWidth := Val( hb_HGetDef( h, "MemoWidth", "0" ) )
        s_cSqlType   := hb_HGetDef( h, "SqlDriver", s_cSqlType )
        s_cSqlConn   := hb_HGetDef( h, "SqlConnection", s_cSqlConn )
        Set( _SET_DELETED, hb_HGetDef( h, "HideDeleted", "OFF" ) == "ON" )
        Set( _SET_EPOCH, Val( hb_HGetDef( h, "Epoch", "1950" ) ) )
        Set( _SET_DATEFORMAT, hb_HGetDef( h, "DateFormat", Set( _SET_DATEFORMAT ) ) )
        IF ! Empty( s_cCodepage ) .AND. AScan( hb_cdpList(), {| c | Upper( c ) == Upper( s_cCodepage ) } ) == 0
            s_cCodepage := ""
        ENDIF
    ENDIF

RETURN

STATIC PROCEDURE SaveSettings()

    LOCAL h := { => }

    h[ "Driver" ]           := s_cDriver
    h[ "Codepage" ]         := s_cCodepage
    h[ "LetoServer" ]       := s_cLetoAddr
    h[ "OpenMode" ]         := IIF( s_lExclusive, "EXCLUSIVE", "SHARED" )
    h[ "Modify" ]           := IIF( s_lModify, "ENABLED", "DISABLED" )
    h[ "Edit" ]             := IIF( s_lImmediate, "IMMEDIATE", "PROTECTED" )
    h[ "Softseek" ]         := IIF( s_lSoftSeek, "ON", "OFF" )
    h[ "HideDeleted" ]      := IIF( Set( _SET_DELETED ), "ON", "OFF" )
    h[ "PreselectIndexes" ] := IIF( s_lPreselect, "ON", "OFF" )
    h[ "Sound" ]            := IIF( s_lSound, "ON", "OFF" )
    h[ "AutoAlias" ]        := IIF( s_lAutoAlias, "ON", "OFF" )
    h[ "MemoWidth" ]        := hb_ntos( s_nMemoWidth )
    h[ "DbfSpec" ]          := s_cDbfSpec
    h[ "IndexSpec" ]        := s_cNdxSpec
    h[ "Epoch" ]            := hb_ntos( Set( _SET_EPOCH ) )
    h[ "DateFormat" ]       := Set( _SET_DATEFORMAT )
    h[ "Theme" ]            := s_cTheme
    h[ "SqlDriver" ]        := s_cSqlType
    h[ "SqlConnection" ]    := s_cSqlConn

    IF hb_iniWrite( IniFile(), { "CBro" => h }, "; CBro global settings", NIL, .F. )
        Toast( "Global settings saved: " + IniFile() )
    ELSE
        Warn( "Cannot write " + IniFile() )
    ENDIF

RETURN

/*---------------------------------------------------------------- memo files */

/* Record > Import File to Memo / Export Memo to File */
STATIC PROCEDURE MemoFile( lImport )

    LOCAL oCol := CurColumn(), cFile, cText, aAsk

    IF oCol == NIL .OR. ! oCol:cargo[ CC_TYPE ] == "M" .OR. Eof()
        Toast( "Move to a memo column first" )
        RETURN
    ENDIF
    IF lImport
        IF ! s_lModify
            Toast( "Modify is DISABLED (F6 GlobalSet)" )
            RETURN
        ENDIF
        cFile := FileDialog( "Import File to Memo " + oCol:cargo[ CC_NAME ], "*.*" )
        IF Empty( cFile )
            RETURN
        ENDIF
        cText := hb_MemoRead( cFile )
        IF Confirm( "Replace memo " + oCol:cargo[ CC_NAME ] + " of record " + hb_ntos( RecNo() ) + ;
                    " with " + hb_FNameNameExt( cFile ) + " (" + hb_ntos( hb_BLen( cText ) ) + " bytes)?" )
            PutField( Alias(), FieldPos( oCol:cargo[ CC_NAME ] ), cText )
            RefreshCur()
        ENDIF
    ELSE
        aAsk := { { "To file:", PadR( Lower( oCol:cargo[ CC_NAME ] ) + "_" + hb_ntos( RecNo() ) + ".txt", 120 ), "@S40" } }
        IF Ask( "Export Memo " + oCol:cargo[ CC_NAME ], aAsk )
            cFile := FullPath( AllTrim( aAsk[ 1 ][ 2 ] ) )
            IF ( ! hb_FileExists( cFile ) .OR. Confirm( hb_FNameNameExt( cFile ) + " exists - overwrite it?" ) ) .AND. ;
               ! hb_MemoWrit( cFile, CBM_ToText( FieldGet( FieldPos( oCol:cargo[ CC_NAME ] ) ), CBM_Kind( FieldGet( FieldPos( oCol:cargo[ CC_NAME ] ) ) ) ) )
                Warn( "Cannot write " + cFile )
            ENDIF
        ENDIF
    ENDIF

RETURN

/*---------------------------------------------------------------- append, copy to */

/* Command > Append From: a DBF, an .xlsx sheet, a CSV / delimited or SDF
   text file - into the current table */
STATIC PROCEDURE AppendFrom()

    LOCAL cFile, nBefore := LastRec(), cErr, cExt, n, cDelim

    IF ! s_lModify
        Toast( "Modify is DISABLED (F6 GlobalSet)" )
        RETURN
    ENDIF
    cFile := FileDialog( "Append From (into " + Alias() + ")", "*.dbf;*.xlsx;*.csv;*.txt" )
    IF Empty( cFile ) .OR. Upper( cFile ) == Upper( s_aAreas[ s_nArea ][ AR_FILE ] )
        RETURN
    ENDIF
    cExt := Lower( hb_FNameExt( cFile ) )
    IF ! Confirm( "Append the records of " + hb_FNameNameExt( cFile ) + " to " + Alias() + "?" )
        RETURN
    ENDIF
    DO CASE
    CASE cExt == ".xlsx"
        cErr := AppendXlsx( cFile )
    CASE cExt == ".csv" .OR. cExt == ".txt"
        n := Choose( "Format of " + hb_FNameNameExt( cFile ), ;
                     { "Delimited - comma (,)", "Delimited - semicolon (;)", "Delimited - tab", "SDF - fixed width" }, ;
                     IIF( cExt == ".csv", 1, 4 ) )
        IF n == 0
            RETURN
        ENDIF
        cDelim := { ",", ";", Chr( 9 ), "" }[ n ]
        cErr := Try( {|| IIF( n == 4, AppendSdf( cFile ), AppendDelim( cFile, cDelim ) ) } )
    OTHERWISE
        cErr := Try( {|| AppendFile( cFile ) } )
    ENDCASE
    RefreshCur()
    IF Empty( cErr )
        Info( hb_ntos( LastRec() - nBefore ) + " records appended" )
    ELSE
        Warn( cErr )
    ENDIF

RETURN

STATIC PROCEDURE AppendSdf( cFile )
    APPEND FROM ( cFile ) SDF
RETURN

STATIC PROCEDURE AppendDelim( cFile, cDelim )
    IF cDelim == ","
        APPEND FROM ( cFile ) DELIMITED
    ELSE
        APPEND FROM ( cFile ) DELIMITED WITH ( { '"', cDelim } )
    ENDIF
RETURN

/* the rows of a sheet into the current table: columns by the heading of the
   first row when it names fields, else by position */
STATIC FUNCTION AppendXlsx( cFile )

    LOCAL cErr := "", oBook, aNames, nSheet := 1, aRows, aMap := {}, aRow, n, nFirst := 1, nPos, x, aStruct := dbStruct()

    oBook := CBX_Open( cFile, @cErr )
    IF oBook == NIL
        RETURN cErr
    ENDIF
    aNames := oBook:SheetNames()
    IF Len( aNames ) > 1 .AND. ( nSheet := Choose( "Sheet of " + hb_FNameNameExt( cFile ), aNames, 1 ) ) == 0
        RETURN ""
    ENDIF
    aRows := CBX_SheetRows( oBook, nSheet )
    IF Empty( aRows )
        RETURN ""
    ENDIF
    FOR n := 1 TO Len( aRows[ 1 ] )
        x := aRows[ 1 ][ n ]
        AAdd( aMap, IIF( HB_ISSTRING( x ), FieldPos( AllTrim( x ) ), 0 ) )
    NEXT
    IF AScan( aMap, {| p | p > 0 } ) > 0
        nFirst := 2
    ELSE
        aMap := {}
        FOR n := 1 TO FCount()
            AAdd( aMap, n )
        NEXT
    ENDIF
    FOR n := nFirst TO Len( aRows )
        aRow := aRows[ n ]
        IF Empty( aRow )
            LOOP
        ENDIF
        dbAppend()
        IF NetErr()
            RETURN "Append failed: the file is locked"
        ENDIF
        FOR nPos := 1 TO Min( Len( aRow ), Len( aMap ) )
            IF aMap[ nPos ] > 0 .AND. aRow[ nPos ] != NIL
                Try( {|| FieldPut( aMap[ nPos ], CellAs( aRow[ nPos ], aStruct[ aMap[ nPos ] ][ 2 ] ) ) } )
            ENDIF
        NEXT
    NEXT
    dbCommit()
    dbUnlock()

RETURN cErr

/* Command > Copy To / Export: the CUI form - file, type, fields, scope,
   FOR / WHILE, delimiter */
STATIC PROCEDURE CopyTo()

    LOCAL dlg, oFile, oBtn, oTypes, aType := {}, oFields, oScope, aScope := {}, oNext, oFor, oWhile, oDelim, oDel
    LOCAL aStruct := dbStruct(), aField, nW := 74, nH, aType1 := { 1 }, aScope1 := { 1 }, nType, nScope, n, nFld
    LOCAL cFile, cFor, cWhile, cDelim, aFields, bFor, bWhile, nNext, cErr, nRec := RecNo(), nDone
    LOCAL aTypes := { "DBF table", "Excel .xlsx", "CSV delimited", "SDF text (fixed)" }
    LOCAL aExt := { ".dbf", ".xlsx", ".csv", ".txt" }

    nFld := Max( 8, Min( Len( aStruct ), ht_ScreenMaxRow() - 20 ) )
    nH := nFld + 16
    dlg := HTDialog():new( NIL, "Copy To / Export - " + Alias() )
    dlg:resize( nW, nH )

    HTLabel():new( "File:", dlg ):move( 2, 1 )
    oFile := HTLineEdit():new( dlg )
    oFile:setGeometry( 9, 1, nW - 26, 1 )
    oFile:text := Lower( Alias() ) + "_copy"
    oBtn := HTPushButton():new( "Browse", dlg )
    oBtn:setGeometry( nW - 15, 1, 10, 1 )
    oBtn:onClicked := {|| cFile := FileDialog( "Copy To - file", "*.*" ), ;
                          IIF( Empty( cFile ), NIL, ( oFile:text := cFile, dlg:repaintChild( oFile ) ) ) }

    /* type: radio buttons over a frame (the frame only draws the box) */
    oTypes := HTFrame():new( " Type ", dlg )
    oTypes:setGeometry( 2, 3, 24, Len( aTypes ) + 2 )
    FOR n := 1 TO Len( aTypes )
        AAdd( aType, HTRadioButton():new( aTypes[ n ], dlg ) )
        ATail( aType ):move( 4, 3 + n )
        ATail( aType ):onToggled := RadioPick( aType1, n )
    NEXT
    aType[ 1 ]:checked := .T.

    /* scope */
    oScope := HTFrame():new( " Scope ", dlg )
    oScope:setGeometry( 2, Len( aTypes ) + 6, 24, 4 )
    AAdd( aScope, HTComboBox():new( dlg ) )
    AEval( { "All records", "Rest (from current)", "Next n records" }, {| c | aScope[ 1 ]:addItem( c ) } )
    aScope[ 1 ]:setCurrentIndex( 1 )
    aScope[ 1 ]:setGeometry( 4, Len( aTypes ) + 7, 20, 1 )
    aScope[ 1 ]:onChanged := {| n | aScope1[ 1 ] := n }
    HTLabel():new( "n:", dlg ):move( 4, Len( aTypes ) + 8 )
    oNext := HTSpinner():new( dlg )
    oNext:setup( "", 1, 9999999, 1, 100 )
    oNext:setGeometry( 8, Len( aTypes ) + 8, 14, 1 )

    /* fields */
    HTLabel():new( "Fields (Space toggles):", dlg ):move( 29, 3 )
    oFields := HTCheckList():new( dlg )
    oFields:setGeometry( 29, 4, nW - 33, nFld )
    FOR EACH aField IN aStruct
        oFields:addItem( PadR( aField[ 1 ], 12 ) + aField[ 2 ] + Str( aField[ 3 ], 5 ) + ;
                         IIF( aField[ 4 ] > 0, "," + hb_ntos( aField[ 4 ] ), "" ), .T. )
    NEXT

    n := Max( Len( aTypes ) + 11, nFld + 5 )
    HTLabel():new( "FOR:", dlg ):move( 2, n )
    oFor := HTLineEdit():new( dlg )
    oFor:setGeometry( 10, n, nW - 14, 1 )
    oFor:text := ".T."
    HTLabel():new( "WHILE:", dlg ):move( 2, n + 2 )
    oWhile := HTLineEdit():new( dlg )
    oWhile:setGeometry( 10, n + 2, nW - 14, 1 )
    oWhile:text := ".T."
    HTLabel():new( "CSV separator:", dlg ):move( 2, n + 4 )
    oDelim := HTComboBox():new( dlg )
    AEval( { "; (semicolon)", ", (comma)", "Tab" }, {| c | oDelim:addItem( c ) } )
    oDelim:setCurrentIndex( 1 )
    oDelim:setGeometry( 17, n + 4, 16, 1 )
    oDel := HTCheckBox():new( "Skip deleted records", dlg )
    oDel:move( 37, n + 4 )
    oDel:checked := Set( _SET_DELETED )

    dlg:resize( nW, n + 10 )
    dlg:addButtonBar( "Copy", "Cancel" )
    IF dlg:exec() != HT_DIALOG_ACCEPTED
        RETURN
    ENDIF
    nType := aType1[ 1 ]
    nScope := aScope1[ 1 ]

    cFile := AllTrim( oFile:text )
    IF Empty( cFile )
        Warn( "File name is missing" )
        RETURN
    ENDIF
    IF Empty( hb_FNameExt( cFile ) )
        cFile += aExt[ nType ]
    ENDIF
    cFile := FullPath( cFile )
    cFor := AllTrim( oFor:text )
    cWhile := AllTrim( oWhile:text )
    IF ( bFor := ExprBlock( IIF( Empty( cFor ), ".T.", cFor ), "L" ) ) == NIL .OR. ;
       ( bWhile := ExprBlock( IIF( Empty( cWhile ), ".T.", cWhile ), "L" ) ) == NIL
        RETURN
    ENDIF
    aFields := {}
    FOR n := 1 TO Len( aStruct )
        IF oFields:isChecked( n )
            AAdd( aFields, aStruct[ n ][ 1 ] )
        ENDIF
    NEXT
    IF Empty( aFields )
        Warn( "Select at least one field" )
        RETURN
    ENDIF
    IF AScan( s_aAreas, {| a | Upper( a[ AR_FILE ] ) == Upper( cFile ) } ) > 0
        Warn( hb_FNameNameExt( cFile ) + " is open - choose another name" )
        RETURN
    ENDIF
    IF hb_FileExists( cFile ) .AND. ! Confirm( hb_FNameNameExt( cFile ) + " exists - overwrite it?" )
        RETURN
    ENDIF
    nNext := IIF( nScope == 3, oNext:value, NIL )
    cDelim := { ";", ",", Chr( 9 ) }[ Max( 1, oDelim:currentIndex() ) ]
    IF oDel:checked
        bFor := {|| ! Deleted() .AND. Eval( bFor ) }
    ENDIF

    nDone := 0
    IF nScope == 1
        dbGoTop()
    ENDIF
    cErr := Try( {|| nDone := CopyData( cFile, nType, aFields, bFor, bWhile, nNext, cDelim ) } )
    dbGoto( nRec )
    RefreshCur()
    IF Empty( cErr )
        Info( hb_ntos( nDone ) + " records copied to " + hb_FNameNameExt( cFile ) )
    ELSE
        Warn( cErr )
    ENDIF

RETURN

/* onToggled of a radio button: its number into aSel[ 1 ] */
STATIC FUNCTION RadioPick( aSel, n )
RETURN {| l | IIF( l, aSel[ 1 ] := n, NIL ) }

/* copies from the current record on: nNext records (NIL = to the end),
   WHILE / FOR; returns the number of records written */
STATIC FUNCTION CopyData( cFile, nType, aFields, bFor, bWhile, nNext, cDelim )

    LOCAL aStruct := dbStruct(), aCols := {}, aPos := {}, cField, nPos, nCount := 0, nSeen := 0
    LOCAL nOut, cLine, x, n, cErr := "", lEnd := .F., cCdp := s_aAreas[ s_nArea ][ AR_CDP ]

    FOR EACH cField IN aFields
        nPos := AScan( aStruct, {| a | a[ 1 ] == cField } )
        AAdd( aCols, aStruct[ nPos ] )
        AAdd( aPos, nPos )
    NEXT

    SWITCH nType
    CASE 1   /* DBF: same structure for the chosen fields */
        dbCreate( cFile, aCols, IIF( s_aAreas[ s_nArea ][ AR_DRIVER ] $ "DBFNTX", "DBFNTX", "DBFCDX" ) )
        dbUseArea( .T., IIF( s_aAreas[ s_nArea ][ AR_DRIVER ] $ "DBFNTX", "DBFNTX", "DBFCDX" ), cFile, "CB_COPYTO", .F., .F., ;
                   IIF( Empty( cCdp ), NIL, cCdp ) )
        dbSelectArea( s_aAreas[ s_nArea ][ AR_ALIAS ] )
        DO WHILE NextCopy( bFor, bWhile, nNext, @nSeen, @lEnd )
            CB_COPYTO->( dbAppend() )
            FOR n := 1 TO Len( aPos )
                CB_COPYTO->( FieldPut( n, ( s_aAreas[ s_nArea ][ AR_ALIAS ] )->( FieldGet( aPos[ n ] ) ) ) )
            NEXT
            nCount++
        ENDDO
        CB_COPYTO->( dbCloseArea() )
        dbSelectArea( s_aAreas[ s_nArea ][ AR_ALIAS ] )
        EXIT
    CASE 2   /* xlsx */
        IF ! CBX_Write( cFile, Alias(), aCols, ;
                        {|| IIF( NextCopy( bFor, bWhile, nNext, @nSeen, @lEnd ), ( nCount++, RowValues( aPos ) ), NIL ) }, ;
                        @cErr, IIF( Empty( cCdp ), "CP852", cCdp ) )
            Break( cErr )
        ENDIF
        EXIT
    OTHERWISE   /* CSV with a heading row, or SDF */
        nOut := FCreate( cFile )
        IF nOut == F_ERROR
            Break( "Cannot create " + cFile )
        ENDIF
        IF nType == 3
            cLine := ""
            FOR EACH x IN aCols
                cLine += IIF( x:__enumIndex() > 1, cDelim, "" ) + x[ 1 ]
            NEXT
            FWrite( nOut, cLine + hb_eol() )
        ENDIF
        DO WHILE NextCopy( bFor, bWhile, nNext, @nSeen, @lEnd )
            cLine := ""
            FOR n := 1 TO Len( aPos )
                x := FieldGet( aPos[ n ] )
                IF nType == 3
                    cLine += IIF( n > 1, cDelim, "" ) + CsvValue( x, cDelim )
                ELSE
                    cLine += SdfValue( x, aCols[ n ] )
                ENDIF
            NEXT
            FWrite( nOut, cLine + hb_eol() )
            nCount++
        ENDDO
        FClose( nOut )
    ENDSWITCH

RETURN nCount

/* moves to the next record to copy (the first call stays on the current
   one); .F. at the end, WHILE false or NEXT reached */
STATIC FUNCTION NextCopy( bFor, bWhile, nNext, nSeen, lEnd )

    IF lEnd
        RETURN .F.
    ENDIF
    DO WHILE .T.
        IF nSeen > 0
            dbSkip()
        ENDIF
        nSeen++
        IF Eof() .OR. ( nNext != NIL .AND. nSeen > nNext ) .OR. ! Eval( bWhile )
            lEnd := .T.
            RETURN .F.
        ENDIF
        IF Eval( bFor )
            RETURN .T.
        ENDIF
    ENDDO

RETURN .F.

STATIC FUNCTION RowValues( aPos )

    LOCAL aRow := Array( Len( aPos ) ), n

    FOR n := 1 TO Len( aPos )
        aRow[ n ] := FieldGet( aPos[ n ] )
        IF HB_ISARRAY( aRow[ n ] )
            aRow[ n ] := CBM_Plain( aRow[ n ] )
        ENDIF
    NEXT

RETURN aRow

STATIC FUNCTION CsvValue( x, cDelim )

    IF HB_ISARRAY( x )
        x := CBM_Plain( x )
    ENDIF
    SWITCH ValType( x )
    CASE "C"
    CASE "M"
        x := RTrim( x )
        IF cDelim $ x .OR. '"' $ x .OR. Chr( 10 ) $ x
            RETURN '"' + StrTran( x, '"', '""' ) + '"'
        ENDIF
        RETURN x
    CASE "N"
        RETURN LTrim( Str( x ) )
    CASE "D"
        RETURN DToC( x )
    CASE "L"
        RETURN IIF( x, "T", "F" )
    ENDSWITCH

RETURN hb_CStr( x )

STATIC FUNCTION SdfValue( x, aField )

    SWITCH aField[ 2 ]
    CASE "C"
        RETURN PadR( x, aField[ 3 ] )
    CASE "N"
        RETURN Str( x, aField[ 3 ], aField[ 4 ] )
    CASE "D"
        RETURN DToS( x )
    CASE "L"
        RETURN IIF( x, "T", "F" )
    CASE "M"
        RETURN RTrim( CBM_Plain( x ) )
    ENDSWITCH

RETURN hb_CStr( x )

/*---------------------------------------------------------------- list, label, report */

/* Command > List to File/Print: the chosen fields as text columns */
STATIC PROCEDURE ListTo()

    LOCAL aStruct := dbStruct(), aNames := {}, aField, dlg, oList, oFor, oOut, n, nRec := RecNo()
    LOCAL aPos := {}, cFor, bFor, cLine, aLines := {}, cHead := "", nOut, cFile

    dlg := HTDialog():new( NIL, "List to File / Print - " + Alias() )
    dlg:resize( 60, Min( Len( aStruct ), ht_ScreenMaxRow() - 14 ) + 11 )
    HTLabel():new( "Fields:", dlg ):move( 2, 1 )
    oList := HTCheckList():new( dlg )
    oList:setGeometry( 2, 2, 54, Min( Len( aStruct ), ht_ScreenMaxRow() - 14 ) )
    FOR EACH aField IN aStruct
        oList:addItem( PadR( aField[ 1 ], 12 ) + aField[ 2 ] + Str( aField[ 3 ], 5 ), ! aField[ 2 ] $ "MWGP" )
        AAdd( aNames, aField[ 1 ] )
    NEXT
    n := Min( Len( aStruct ), ht_ScreenMaxRow() - 14 ) + 3
    HTLabel():new( "FOR:", dlg ):move( 2, n )
    oFor := HTLineEdit():new( dlg )
    oFor:setGeometry( 8, n, 48, 1 )
    oFor:text := ".T."
    HTLabel():new( "To:", dlg ):move( 2, n + 2 )
    oOut := HTComboBox():new( dlg )
    AEval( { "Screen (text window)", "File (.txt)", "Printer (default)" }, {| c | oOut:addItem( c ) } )
    oOut:setCurrentIndex( 1 )
    oOut:setGeometry( 8, n + 2, 26, 1 )
    dlg:addButtonBar( "List", "Cancel" )
    IF dlg:exec() != HT_DIALOG_ACCEPTED
        RETURN
    ENDIF
    cFor := AllTrim( oFor:text )
    IF ( bFor := ExprBlock( IIF( Empty( cFor ), ".T.", cFor ), "L" ) ) == NIL
        RETURN
    ENDIF
    FOR n := 1 TO Len( aStruct )
        IF oList:isChecked( n )
            AAdd( aPos, n )
            cHead += IIF( Len( aPos ) > 1, " ", "" ) + PadR( aStruct[ n ][ 1 ], ListWidth( aStruct[ n ] ) )
        ENDIF
    NEXT
    IF Empty( aPos )
        RETURN
    ENDIF
    AAdd( aLines, "Record  " + cHead )
    dbGoTop()
    DO WHILE ! Eof()
        IF Eval( bFor )
            cLine := Str( RecNo(), 7 ) + " "
            FOR n := 1 TO Len( aPos )
                cLine += IIF( n > 1, " ", "" ) + ListValue( FieldGet( aPos[ n ] ), aStruct[ aPos[ n ] ] )
            NEXT
            AAdd( aLines, cLine )
        ENDIF
        dbSkip()
    ENDDO
    dbGoto( nRec )

    SWITCH oOut:currentIndex()
    CASE 2
        cFile := FullPath( Lower( Alias() ) + "_list.txt" )
        nOut := FCreate( cFile )
        AEval( aLines, {| c | FWrite( nOut, c + hb_eol() ) } )
        FClose( nOut )
        Info( hb_ntos( Len( aLines ) - 1 ) + " records listed to " + cFile )
        EXIT
    CASE 3
        PrintLines( aLines )
        EXIT
    OTHERWISE
        ShowLines( "List - " + Alias() + "  (" + hb_ntos( Len( aLines ) - 1 ) + " records)", aLines )
    ENDSWITCH

RETURN

STATIC FUNCTION ListWidth( aField )

    SWITCH aField[ 2 ]
    CASE "D"
        RETURN Max( Len( DToC( Date() ) ), Len( aField[ 1 ] ) )
    CASE "L"
        RETURN Max( 1, Len( aField[ 1 ] ) )
    CASE "M"
        RETURN 30
    ENDSWITCH

RETURN Max( Min( aField[ 3 ], 60 ), Len( aField[ 1 ] ) )

STATIC FUNCTION ListValue( x, aField )

    LOCAL nW := ListWidth( aField )

    SWITCH aField[ 2 ]
    CASE "N"
        RETURN PadL( Str( x, aField[ 3 ], aField[ 4 ] ), nW )
    CASE "D"
        RETURN PadR( DToC( x ), nW )
    CASE "L"
        RETURN PadR( IIF( x, "T", "F" ), nW )
    CASE "M"
        RETURN ht_cellPadR( CBM_Plain( x ), nW )
    ENDSWITCH

RETURN ht_cellPadR( hb_CStr( x ), nW )

/* raw text to the default printer (Windows), else to a file */
STATIC PROCEDURE PrintLines( aLines )

    LOCAL cFile := hb_DirTemp() + "cbro_print.txt", nOut := FCreate( cFile ), nRet

    AEval( aLines, {| c | FWrite( nOut, hb_StrToUTF8( c ) + Chr( 13 ) + Chr( 10 ) ) } )
    FWrite( nOut, Chr( 12 ) )
    FClose( nOut )
#ifdef __PLATFORM__WINDOWS
    nRet := win_PrintFileRaw( win_PrinterGetDefault(), cFile, "CBro - " + Alias() )
    IF nRet < 0
        Warn( "Printing failed (" + hb_ntos( nRet ) + "): " + win_PrinterGetDefault() )
    ELSE
        Toast( "Sent to " + win_PrinterGetDefault() )
    ENDIF
#else
    nRet := hb_processRun( "lp " + cFile )
    IF nRet != 0
        Warn( "lp failed (" + hb_ntos( nRet ) + ") - the list is in " + cFile )
    ENDIF
#endif

RETURN

/* Command > Label Form / Report Form: a Clipper .lbl / .frm file, FOR,
   output to the screen, a file or the printer */
STATIC PROCEDURE LabelReport( lLabel )

    LOCAL cForm := FileDialog( IIF( lLabel, "Label Form", "Report Form" ), IIF( lLabel, "*.lbl", "*.frm" ) )
    LOCAL aAsk, bFor, cOut, nRec := RecNo(), cErr, aLines, n

    IF Empty( cForm )
        RETURN
    ENDIF
    aAsk := { { "FOR:", PadR( ".T.", 200 ), "@S40" }, ;
              { "To (S=screen F=file P=printer):", "S", "!" } }
    IF ! Ask( IIF( lLabel, "Label Form ", "Report Form " ) + hb_FNameNameExt( cForm ), aAsk ) .OR. ;
       ( bFor := ExprBlock( aAsk[ 1 ][ 2 ], "L" ) ) == NIL
        RETURN
    ENDIF
    cOut := FullPath( Lower( Alias() ) + IIF( lLabel, "_labels.txt", "_report.txt" ) )
    FErase( cOut )
    dbGoTop()
    IF lLabel
        cErr := Try( {|| __LabelForm( cForm, .F., cOut, .T., bFor ) } )
    ELSE
        cErr := Try( {|| __ReportForm( cForm, .F., cOut, .T., bFor ) } )
    ENDIF
    dbGoto( nRec )
    IF ! Empty( cErr )
        Warn( cErr )
        RETURN
    ENDIF
    aLines := hb_ATokens( StrTran( StrTran( hb_MemoRead( cOut ), Chr( 13 ), "" ), Chr( 12 ), "" ), Chr( 10 ) )
    n := aAsk[ 2 ][ 2 ]
    DO CASE
    CASE n == "P"
        PrintLines( aLines )
        FErase( cOut )
    CASE n == "F"
        Info( "Output written to " + cOut )
    OTHERWISE
        ShowLines( hb_FNameNameExt( cForm ) + " - " + Alias(), aLines )
        FErase( cOut )
    ENDCASE

RETURN

/*---------------------------------------------------------------- scripts */

/* Command > Run Script: a .prg (compiled in memory) or a compiled .hrb */
STATIC PROCEDURE RunScript()

    LOCAL cFile := FileDialog( "Run Script", "*.prg;*.hrb" )

    IF ! Empty( cFile )
        RunScriptFile( cFile )
    ENDIF

RETURN

STATIC PROCEDURE RunScriptFile( cFile )

    cFile := FullPath( cFile )
    IF ! hb_FileExists( cFile )
        Warn( cFile + ": File cannot be found" )
        RETURN
    ENDIF
    s_cScript := cFile
    IF Lower( hb_FNameExt( cFile ) ) == ".hrb"
        RunHrb( hb_MemoRead( cFile ), hb_FNameNameExt( cFile ) )
    ELSE
        RunPrg( hb_MemoRead( cFile ), hb_FNameNameExt( cFile ), hb_FNameDir( cFile ) )
    ENDIF

RETURN

/* compiles Harbour source in memory and runs its first procedure */
STATIC FUNCTION RunPrg( cSource, cName, cDir )

    LOCAL cHrb, cErr

#ifdef CBRO_NO_COMPILER
    cErr := ExternalCompile( cSource, cDir, @cHrb )
#else
    cErr := Try( {|| cHrb := hb_compileFromBuf( cSource, .T., "harbour", "-n2", "-q2", "-w0", "-es0", ;
                                                 "-I" + hb_DirBase(), IIF( Empty( cDir ), NIL, "-I" + cDir ) ) } )
#endif
    IF ! Empty( cErr ) .OR. Empty( cHrb )
        Warn( "Script " + cName + " does not compile" + IIF( Empty( cErr ), "", ":" + hb_eol() + cErr ) )
        RETURN .F.
    ENDIF

RETURN RunHrb( cHrb, cName )

#ifdef CBRO_NO_COMPILER
/* built without the Harbour compiler (its license is GPL without the linking
   exception): harbour.exe next to CBro or in the PATH compiles the script */
STATIC FUNCTION ExternalCompile( cSource, cDir, cHrb )

    LOCAL cTmp := hb_DirTemp() + "cbro_script_" + hb_ntos( hb_RandomInt( 1, 999999 ) )
    LOCAL cExe := hb_DirBase() + "harbour" + IIF( hb_ps() == Chr( 92 ), ".exe", "" ), cOut := "", nRet

    IF ! hb_FileExists( cExe )
        cExe := "harbour"
    ENDIF
    hb_MemoWrit( cTmp + ".prg", cSource )
    nRet := hb_processRun( cExe + " " + cTmp + ".prg -gh -n2 -q0 -w0 -o" + cTmp + ".hrb" + ;
                           " -I" + hb_DirBase() + IIF( Empty( cDir ), "", " -I" + cDir ), NIL, @cOut, @cOut )
    cHrb := IIF( hb_FileExists( cTmp + ".hrb" ), hb_MemoRead( cTmp + ".hrb" ), NIL )
    FErase( cTmp + ".prg" )
    FErase( cTmp + ".hrb" )
    IF nRet == -1 .OR. ( nRet != 0 .AND. Empty( cOut ) )
        RETURN "harbour.exe not found - copy it next to CBro, or run compiled .hrb scripts"
    ENDIF
    IF nRet != 0 .OR. cHrb == NIL
        RETURN AllTrim( cOut )
    ENDIF

RETURN ""
#endif

STATIC FUNCTION RunHrb( cHrb, cName )

    LOCAL pHrb, cErr

    cErr := Try( {|| pHrb := hb_hrbLoad( HB_HRB_BIND_FORCELOCAL, cHrb ), hb_hrbDo( pHrb ) } )
    IF pHrb != NIL
        hb_hrbUnload( pHrb )
    ENDIF
    /* the script may have moved, changed or opened anything */
    AEval( s_aAreas, {| a, n | HB_SYMBOL_UNUSED( a ), RefreshArea( n ) } )
    IF s_nArea > 0
        dbSelectArea( s_aAreas[ s_nArea ][ AR_ALIAS ] )
    ENDIF
    HTApplication():repaintAll()
    IF ! Empty( cErr )
        Warn( "Script " + cName + ": " + cErr )
        RETURN .F.
    ENDIF

RETURN .T.

/* Command > Script Editor: write, open, save and run a script */
STATIC PROCEDURE ScriptEditor()

    LOCAL dlg, oEdit, oBtn, nW := Min( 100, ht_ScreenMaxCol() - 2 ), nH := Min( 34, ht_ScreenMaxRow() - 2 )
    LOCAL nAction := 0, cFile := s_cScript, cText

    cText := IIF( ! Empty( cFile ) .AND. hb_FileExists( cFile ), hb_MemoRead( cFile ), ScriptSample() )
    DO WHILE .T.
        nAction := 0
        dlg := HTDialog():new( NIL, "Script Editor  " + IIF( Empty( cFile ), "(new)", cFile ) )
        dlg:resize( nW, nH )
        oEdit := HTTextEdit():new( dlg )
        oEdit:setGeometry( 1, 1, nW - 4, nH - 6 )
        oEdit:text := cText
        oBtn := HTPushButton():new( "Run", dlg )
        oBtn:setGeometry( 2, nH - 4, 9, 1 )
        oBtn:onClicked := {|| nAction := 1, dlg:accept() }
        oBtn := HTPushButton():new( "Open", dlg )
        oBtn:setGeometry( 13, nH - 4, 10, 1 )
        oBtn:onClicked := {|| nAction := 2, dlg:accept() }
        oBtn := HTPushButton():new( "Save", dlg )
        oBtn:setGeometry( 25, nH - 4, 10, 1 )
        oBtn:onClicked := {|| nAction := 3, dlg:accept() }
        oBtn := HTPushButton():new( "Save as", dlg )
        oBtn:setGeometry( 37, nH - 4, 11, 1 )
        oBtn:onClicked := {|| nAction := 4, dlg:accept() }
        oBtn := HTPushButton():new( "Close", dlg )
        oBtn:setGeometry( nW - 14, nH - 4, 10, 1 )
        oBtn:onClicked := {|| nAction := 0, dlg:reject() }
        dlg:exec()
        cText := oEdit:text
        DO CASE
        CASE nAction == 1
            RunPrg( cText, IIF( Empty( cFile ), "(editor)", hb_FNameNameExt( cFile ) ), hb_FNameDir( cFile ) )
        CASE nAction == 2
            cFile := FileDialog( "Open Script", "*.prg" )
            IF ! Empty( cFile )
                cText := hb_MemoRead( cFile )
                s_cScript := cFile
            ENDIF
        CASE nAction == 3 .OR. nAction == 4
            IF nAction == 4 .OR. Empty( cFile )
                cFile := AskFile( "Save Script As", IIF( Empty( cFile ), "script.prg", cFile ), ".prg" )
            ENDIF
            IF ! Empty( cFile )
                IF hb_MemoWrit( cFile, cText )
                    s_cScript := cFile
                    Toast( "Saved " + hb_FNameNameExt( cFile ) )
                ELSE
                    Warn( "Cannot write " + cFile )
                ENDIF
            ENDIF
        OTHERWISE
            EXIT
        ENDCASE
    ENDDO

RETURN

STATIC FUNCTION ScriptSample()
RETURN ;
    "/* CBro script: the first procedure runs. The current table is selected;" + hb_eol() + ;
    "   CBro_*() functions talk to CBro (Help > Help Index > Scripts). */" + hb_eol() + ;
    hb_eol() + ;
    "PROCEDURE Main()" + hb_eol() + ;
    hb_eol() + ;
    "   LOCAL nCount := 0" + hb_eol() + ;
    hb_eol() + ;
    "   IF Empty( Alias() )" + hb_eol() + ;
    '      CBro_Message( "Open a table first" )' + hb_eol() + ;
    "      RETURN" + hb_eol() + ;
    "   ENDIF" + hb_eol() + ;
    "   dbEval( {|| nCount++ }, {|| ! Deleted() } )" + hb_eol() + ;
    '   CBro_Message( Alias() + ": " + hb_ntos( nCount ) + " records not deleted" )' + hb_eol() + ;
    hb_eol() + ;
    "   RETURN" + hb_eol()

/* a file name to write (in the current directory unless a path is given) */
STATIC FUNCTION AskFile( cTitle, cDefault, cExt )

    LOCAL aAsk := { { "File:", PadR( cDefault, 120 ), "@S40" } }, cFile

    IF ! Ask( cTitle, aAsk )
        RETURN ""
    ENDIF
    cFile := AllTrim( aAsk[ 1 ][ 2 ] )
    IF Empty( cFile )
        RETURN ""
    ENDIF
    IF Empty( hb_FNameExt( cFile ) )
        cFile += cExt
    ENDIF
    cFile := FullPath( cFile )
    IF hb_FileExists( cFile ) .AND. ! Confirm( hb_FNameNameExt( cFile ) + " exists - overwrite it?" )
        RETURN ""
    ENDIF

RETURN cFile

/* values kept for this session (last command, SQL text, view file) */
STATIC FUNCTION ScriptVar( cKey, xValue )

    STATIC s_hVars := { => }

    IF PCount() > 1
        s_hVars[ cKey ] := xValue
    ENDIF

RETURN hb_HGetDef( s_hVars, cKey, "" )

/* functions a script can call */
FUNCTION CBro_Message( cText )
    Info( hb_CStr( cText ) )
RETURN NIL

FUNCTION CBro_Confirm( cText )
RETURN Confirm( hb_CStr( cText ) )

FUNCTION CBro_Open( cFile )
RETURN OpenAny( cFile, .F. )

FUNCTION CBro_Refresh()
    AEval( s_aAreas, {| a, n | HB_SYMBOL_UNUSED( a ), RefreshArea( n ) } )
RETURN NIL

FUNCTION CBro_Alias( n )
    IF n == NIL
        n := s_nArea
    ENDIF
RETURN IIF( n > 0 .AND. n <= Len( s_aAreas ), s_aAreas[ n ][ AR_ALIAS ], "" )

FUNCTION CBro_Tables()
RETURN Len( s_aAreas )

/* runs SQL: a SELECT opens a window, other statements run on the server */
FUNCTION CBro_Sql( cDriver, cConn, cSql )
    SqlRun( Upper( hb_defaultValue( cDriver, "SQLITE3" ) ), hb_defaultValue( cConn, "" ), hb_defaultValue( cSql, "" ) )
RETURN NIL

FUNCTION CBro_Ask( cLabel, xValue )

    LOCAL aAsk := { { cLabel, xValue, IIF( HB_ISSTRING( xValue ), "@S40", NIL ) } }

    IF HB_ISSTRING( xValue )
        aAsk[ 1 ][ 2 ] := PadR( xValue, 200 )
    ENDIF
    IF Ask( "Script", aAsk )
        RETURN IIF( HB_ISSTRING( xValue ), AllTrim( aAsk[ 1 ][ 2 ] ), aAsk[ 1 ][ 2 ] )
    ENDIF

RETURN NIL

FUNCTION CBro_ShowText( cTitle, xText )
    ShowLines( cTitle, IIF( HB_ISARRAY( xText ), xText, hb_ATokens( StrTran( hb_CStr( xText ), Chr( 13 ), "" ), Chr( 10 ) ) ) )
RETURN NIL

/*---------------------------------------------------------------- help, exit */

/* cbro /? : the parameters in the help window, then quit */
STATIC PROCEDURE CmdLineHelp()

    LOCAL app := HTApplication():new()

    ht_cuiSetupDisplay( app:rows(), app:cols() )
    hb_gtInfo( HB_GTI_WINTITLE, "CBro" )
    CBH_Show( "cmdline" )

RETURN

STATIC PROCEDURE Quit()
    IF Confirm( "Exit CBro?" )
        HTApplication():quit()
    ENDIF
RETURN

STATIC PROCEDURE Calculator()

    LOCAL xValue := CBC_Calculator( s_xClipCell )

    IF xValue != NIL
        s_xClipCell := xValue
        Toast( "Result copied - Ctrl-F4 pastes it into a field" )
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
    oMenu:addAction( "Color Condition      Alt-C" ):onTriggered := {|| ColorCondDlg() }
    oMenu:addAction( "Relation             Alt-R" ):onTriggered := {|| RelationDlg() }
    oMenu:addSeparator()
    oMenu:addAction( "Copy To / Export..." ):onTriggered := {|| CopyTo(), AfterAction() }

RETURN oMenu

/*---------------------------------------------------------------- file dialog */

/* BroPlus selection screen: the files of a directory (cSpec may hold several
   masks: "*.dbf;*.xlsx"), Enter on a directory opens it; cServer = a LetoDB
   directory "//host:port/dir/" (the server's files); returns the full path
   or "" */
STATIC FUNCTION FileDialog( cTitle, cSpec, cServer )

    LOCAL cDir := IIF( Empty( cServer ), hb_cwd(), cServer ), cFile := Space( 200 ), cPick
    LOCAL dlg, oList, oGet, oPath, nRows, nW

    nRows := Max( 5, Min( 20, ht_ScreenMaxRow() - 14 ) )
    nW := Min( 76, ht_ScreenMaxCol() - 6 )
    dlg := HTDialog():new( NIL, cTitle + "  " + cSpec )
    dlg:resize( nW, nRows + 10 )
    oPath := HTLabel():new( "", dlg )
    oPath:setGeometry( 1, 1, nW - 4, 1 )
    oList := HTListBox():new( dlg )
    oList:setGeometry( 1, 2, nW - 4, nRows )
    oGet := HTGet():new( dlg )
    oGet:setup( {| x | IIF( x == NIL, cFile, cFile := x ) }, "File:", "@S" + hb_ntos( nW - 12 ), NIL, NIL, "", NIL, .F., .F., nW - 6 )
    oGet:move( 1, nRows + 3 )
    FillFiles( oList, oPath, cDir, cSpec, ! Empty( cServer ) )
    IF oList:count() > 0 .AND. ! IsDirItem( oList:currentData() )
        cFile := PadR( oList:currentData(), 200 )
    ENDIF

    oList:onChanged := {| n | HB_SYMBOL_UNUSED( n ), cPick := oList:currentData(), ;
        IIF( IsDirItem( cPick ), NIL, ( cFile := PadR( cPick, 200 ), dlg:repaintChild( oGet ) ) ) }
    oList:onActivated := {| n | HB_SYMBOL_UNUSED( n ), cPick := oList:currentData(), ;
        IIF( IsDirItem( cPick ), ;
             ( cDir := SubDir( cDir, cPick ), FillFiles( oList, oPath, cDir, cSpec, ! Empty( cServer ) ), ;
               dlg:repaintChild( oPath ), dlg:repaintChild( oList ) ), ;
             ( cFile := PadR( cPick, 200 ), dlg:accept() ) ) }
    dlg:addButtonBar( "OK", "Cancel" )

    IF dlg:exec() == HT_DIALOG_ACCEPTED .AND. ! Empty( cFile )
        cFile := AllTrim( cFile )
        RETURN IIF( ":" $ cFile .OR. Left( cFile, 1 ) $ "\/", cFile, cDir + cFile )
    ENDIF

RETURN ""

STATIC PROCEDURE FillFiles( oList, oPath, cDir, cSpec, lLeto )

    LOCAL aDirs := {}, aFiles := {}, aEntry, c, cMask, aDir, nW := oList:width - 2, cSep := IIF( lLeto, "/", hb_ps() )

    oList:clear()
    IF lLeto
        IF Len( SubStr( cDir, At( "/", SubStr( cDir, 3 ) ) + 3 ) ) > 0
            oList:addItem( ".." + cSep, ".." + cSep )
        ENDIF
    ELSEIF ! ( Len( cDir ) <= 1 .OR. ( Len( cDir ) == 3 .AND. SubStr( cDir, 2, 1 ) == ":" ) )
        oList:addItem( ".." + cSep, ".." + cSep )
    ENDIF
    aDir := IIF( lLeto, LetoFiles( cDir, "*.*" ), Directory( cDir + hb_osFileMask(), "D" ) )
    FOR EACH aEntry IN aDir
        IF "D" $ aEntry[ F_ATTR ] .AND. ! aEntry[ F_NAME ] == "." .AND. ! aEntry[ F_NAME ] == ".."
            AAdd( aDirs, aEntry[ F_NAME ] + cSep )
        ENDIF
    NEXT
    FOR EACH cMask IN hb_ATokens( cSpec, ";" )
        aDir := IIF( lLeto, LetoFiles( cDir, AllTrim( cMask ) ), Directory( cDir + AllTrim( cMask ) ) )
        FOR EACH aEntry IN aDir
            IF ! "D" $ aEntry[ F_ATTR ] .AND. AScan( aFiles, {| a | a[ 1 ] == aEntry[ F_NAME ] } ) == 0
                AAdd( aFiles, { aEntry[ F_NAME ], aEntry[ F_SIZE ], aEntry[ F_DATE ] } )
            ENDIF
        NEXT
    NEXT
    ASort( aDirs,,, {| a, b | Upper( a ) < Upper( b ) } )
    ASort( aFiles,,, {| a, b | Upper( a[ 1 ] ) < Upper( b[ 1 ] ) } )
    FOR EACH aEntry IN aFiles
        c := ht_cellPadR( aEntry[ 1 ], Max( 12, nW - 25 ) ) + ;
             Str( aEntry[ 2 ], 12 ) + "  " + DToC( aEntry[ 3 ] )
        oList:addItem( c, aEntry[ 1 ] )
    NEXT
    FOR EACH c IN aDirs
        oList:addItem( c, c )
    NEXT
    IF oList:count() > 0
        oList:setCurrentIndex( 1 )
    ENDIF
    oPath:setText( "Directory: " + cDir )
    oPath:setGeometry( 1, 1, nW + 2, 1 )

RETURN

/* a text window (list output, command output) */
STATIC PROCEDURE ShowLines( cTitle, aLines )

    LOCAL dlg, oEdit, nW := Min( ht_ScreenMaxCol() - 2, 120 ), nH := Min( ht_ScreenMaxRow() - 2, Max( 12, Len( aLines ) + 6 ) ), c, cText := ""

    FOR EACH c IN aLines
        cText += c + IIF( c:__enumIsLast(), "", Chr( 10 ) )
    NEXT
    dlg := HTDialog():new( NIL, cTitle )
    dlg:resize( nW, nH )
    oEdit := HTTextEdit():new( dlg )
    oEdit:setGeometry( 1, 1, nW - 4, nH - 6 )
    oEdit:text := cText
    oEdit:_readOnly( .T. )
    dlg:addButtonBar( "Close" )
    dlg:exec()

RETURN

/*---------------------------------------------------------------- messages */

STATIC PROCEDURE Info( cText )
    HTMessageBox():information( "CBro", cText )
RETURN

STATIC PROCEDURE Warn( cText )
    Beep()
    HTMessageBox():warning( "CBro", cText )
RETURN

STATIC FUNCTION Confirm( cText )
RETURN HTMessageBox():question( "CBro", cText ) == HT_DIALOG_ACCEPTED

/* a message in the bottom line while something takes time */
STATIC PROCEDURE Busy( cText )
    s_aMsgTxt[ 1 ] := " " + cText + " "
    s_oMsg:setSection( s_nMsgBase + 1, s_aMsgTxt[ 1 ] )
    s_oMain:repaintChild( s_oMsg )
RETURN

STATIC PROCEDURE Beep()
    IF s_lSound
        Tone( 87.3, 1 )
    ENDIF
RETURN

/*---------------------------------------------------------------- dynamic indexes */

/* state of the current order of the current table: 1 = unique keys shown,
   2 = descending */
STATIC FUNCTION IndexState( nWhat )

    LOCAL lOn := .F., cAlias

    IF s_nArea > 0
        cAlias := s_aAreas[ s_nArea ][ AR_ALIAS ]
        IF ( cAlias )->( IndexOrd() ) > 0
            Try( {|| lOn := IIF( nWhat == 1, ( cAlias )->( dbOrderInfo( DBOI_UNIQUE ) ), ( cAlias )->( ordDescend() ) ) } )
        ENDIF
    ENDIF

RETURN HB_ISLOGICAL( lOn ) .AND. lOn

/* status bar flags of the current order (current area selected) */
STATIC FUNCTION IndexFlags()

    LOCAL cOut := "", lUnique := .F., lDesc := .F., lCustom := .F.

    IF IndexOrd() > 0
        Try( {|| lUnique := dbOrderInfo( DBOI_UNIQUE ), lDesc := ordDescend(), lCustom := ordCustom() } )
        cOut := IIF( HB_ISLOGICAL( lUnique ) .AND. lUnique, " <Unique>", "" ) + ;
                IIF( HB_ISLOGICAL( lDesc ) .AND. lDesc, " <Desc>", "" ) + ;
                IIF( HB_ISLOGICAL( lCustom ) .AND. lCustom, " <Custom>", "" )
    ENDIF

RETURN cOut

/* Shift-F6: only the first record of every key value of the order */
STATIC PROCEDURE ToggleUnique()

    LOCAL lOld, lNew

    IF IndexOrd() == 0
        Toast( "Show Unique Keys needs an index order (F3)" )
        RETURN
    ENDIF
    lOld := dbOrderInfo( DBOI_UNIQUE )
    dbOrderInfo( DBOI_UNIQUE, NIL, NIL, ! lOld )
    lNew := dbOrderInfo( DBOI_UNIQUE )
    IF lNew == lOld
        Info( "Order " + OrdName() + " was created UNIQUE - it always shows unique keys" + ;
              IIF( s_aAreas[ s_nArea ][ AR_KIND ] == "LETO", " (or the server does not support it)", "" ) )
        RETURN
    ENDIF
    dbGoTop()
    RefreshCur()

RETURN

/* Alt-F6: the order read backwards, without rebuilding it */
STATIC PROCEDURE ToggleDescend()

    IF IndexOrd() == 0
        Toast( "Descending Order needs an index order (F3)" )
        RETURN
    ENDIF
    ordDescend( NIL, NIL, ! ordDescend() )
    dbGoTop()
    RefreshCur()

RETURN

/* Index > Make Custom Index: keys are added / removed by hand (Ctrl-F8 /
   Ctrl-F9), not by changes to the records - a chosen set of records */
STATIC PROCEDURE CustomIndexDlg()

    LOCAL aArea := s_aAreas[ s_nArea ], aItems := {}, aIds := {}, n, aAsk, cTag, cKey, cOrder := OrdName()

    IF ! aArea[ AR_DRIVER ] $ "DBFCDX LETO"
        Info( "Custom indexes need the DBFCDX driver" )
        RETURN
    ENDIF
    IF IndexOrd() > 0 .AND. ! ordCustom()
        AAdd( aItems, "Turn order " + cOrder + " into a custom index (its keys stay)" )
        AAdd( aIds, 1 )
    ENDIF
    IF IndexOrd() > 0 .AND. ordCustom()
        AAdd( aItems, "Make order " + cOrder + " a normal index again (Reindex adds every record)" )
        AAdd( aIds, 2 )
    ENDIF
    AAdd( aItems, "New empty custom tag ..." )
    AAdd( aIds, 3 )
    IF ( n := Choose( "Custom Index - " + Alias(), aItems, 1 ) ) == 0
        RETURN
    ENDIF
    SWITCH aIds[ n ]
    CASE 1
        ordCustom( NIL, NIL, .T. )
        aArea[ AR_CUSTOM ] := cOrder
        Info( cOrder + " is a custom index: Ctrl-F8 adds the current record, Ctrl-F9 removes it" )
        EXIT
    CASE 2
        ordCustom( NIL, NIL, .F. )
        aArea[ AR_CUSTOM ] := NIL
        Toast( cOrder + " is a normal index again" )
        EXIT
    CASE 3
        aAsk := { { "Tag name:", PadR( "MARKED", 10 ), "@!" }, ;
                  { "Key expression:", PadR( IIF( IndexOrd() > 0, OrdKey(), FieldName( 1 ) ), 200 ), "@S45" } }
        IF ! Ask( "New Custom Tag - " + Alias(), aAsk )
            RETURN
        ENDIF
        cTag := AllTrim( aAsk[ 1 ][ 2 ] )
        cKey := AllTrim( aAsk[ 2 ][ 2 ] )
        IF Empty( cTag ) .OR. ExprBlock( cKey ) == NIL
            RETURN
        ENDIF
        IF WithExclusive( {|| ordCondSet( NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, .T. ), ;
                              ordCreate( NIL, cTag, cKey, hb_macroBlock( cKey ) ) } )
            aArea[ AR_CUSTOM ] := cTag
            ordSetFocus( IIF( Empty( cOrder ), 0, cOrder ) )
            Info( "Custom tag " + cTag + " is empty: Ctrl-F8 adds the current record, Ctrl-F9 removes it;" + hb_eol() + ;
                  "Ctrl-F3 / Alt-O shows the chosen records" )
        ENDIF
        EXIT
    ENDSWITCH
    RefreshCur()

RETURN

/* Ctrl-F8 / Ctrl-F9: the current record into / out of the custom tag */
STATIC PROCEDURE CustomKey( lAdd )

    LOCAL aArea := s_aAreas[ s_nArea ], cTag := aArea[ AR_CUSTOM ], lOk := .F., cErr

    IF Empty( cTag ) .AND. IndexOrd() > 0 .AND. ordCustom()
        cTag := OrdName()
    ENDIF
    IF Empty( cTag ) .OR. ordNumber( cTag ) == 0
        Toast( "No custom index - Index > Make Custom Index" )
        RETURN
    ENDIF
    IF Eof()
        RETURN
    ENDIF
    cErr := Try( {|| lOk := IIF( lAdd, ordKeyAdd( cTag ), ordKeyDel( cTag ) ) } )
    IF ! Empty( cErr )
        Warn( cErr )
    ELSEIF ! lOk
        Toast( "Record " + hb_ntos( RecNo() ) + IIF( lAdd, " is already in ", " is not in " ) + cTag )
    ELSE
        Toast( "Record " + hb_ntos( RecNo() ) + IIF( lAdd, " added to ", " removed from " ) + cTag )
        IF OrdName() == cTag
            RefreshCur()
        ENDIF
    ENDIF

RETURN

/*---------------------------------------------------------------- BroPlus colors */

/* the BroPlus 2.50 color set (Bp_Colors) as a theme: gray screen and
   browse, yellow browse cursor, cyan pull-down menus with a brown bar and
   yellow hot keys, the white-on-dark-gray function key row, green fields,
   red inactive fields; the standard VGA palette (DOS colors) */
STATIC PROCEDURE RegisterBroPlusTheme()

    LOCAL a := Array( HT_CLR_MAX )
    LOCAL aPal := { ht_rgb( 0x00, 0x00, 0x00 ), ht_rgb( 0x00, 0x00, 0xAA ), ht_rgb( 0x00, 0xAA, 0x00 ), ht_rgb( 0x00, 0xAA, 0xAA ), ;
                    ht_rgb( 0xAA, 0x00, 0x00 ), ht_rgb( 0xAA, 0x00, 0xAA ), ht_rgb( 0xAA, 0x55, 0x00 ), ht_rgb( 0xAA, 0xAA, 0xAA ), ;
                    ht_rgb( 0x55, 0x55, 0x55 ), ht_rgb( 0x55, 0x55, 0xFF ), ht_rgb( 0x55, 0xFF, 0x55 ), ht_rgb( 0x55, 0xFF, 0xFF ), ;
                    ht_rgb( 0xFF, 0x55, 0x55 ), ht_rgb( 0xFF, 0x55, 0xFF ), ht_rgb( 0xFF, 0xFF, 0x55 ), ht_rgb( 0xFF, 0xFF, 0xFF ) }

    /* COLOR1 standard N/W */
    a[ HT_CLR_WINDOW ]            := "0/7"
    a[ HT_CLR_DESKTOP ]           := "0/7"
    a[ HT_CLR_LABEL ]             := "0/7"
    a[ HT_CLR_FRAME ]             := "0/7"
    a[ HT_CLR_GET_LABEL ]         := "0/7"
    a[ HT_CLR_CHECK_NORMAL ]      := "0/7"
    /* COLOR4 parameter entry: active GET N/G*, other GETs W+/R */
    a[ HT_CLR_GET_NORMAL ]        := "15/4"
    a[ HT_CLR_LINEEDIT_NORMAL ]   := "15/4"
    a[ HT_CLR_COMBO_NORMAL ]      := "15/4"
    a[ HT_CLR_SPINNER_NORMAL ]    := "15/4"
    a[ HT_CLR_GET_FOCUSED ]       := "0/10"
    a[ HT_CLR_LINEEDIT_FOCUSED ]  := "0/10"
    a[ HT_CLR_COMBO_FOCUSED ]     := "0/10"
    a[ HT_CLR_SPINNER_FOCUSED ]   := "0/10"
    a[ HT_CLR_CHECK_FOCUSED ]     := "0/10"
    a[ HT_CLR_GET_READONLY ]      := "8/7"
    a[ HT_CLR_LINEEDIT_SELECTED ] := "0/14"
    /* COLOR17 alert box: N/G*, choice N/R* */
    a[ HT_CLR_BUTTON_NORMAL ]     := "0/10"
    a[ HT_CLR_BUTTON_FOCUSED ]    := "0/12"
    /* COLOR18B picklists: W+/N*, selection N/GR* */
    a[ HT_CLR_LIST_NORMAL ]       := "15/8"
    a[ HT_CLR_LIST_SELECTED ]     := "0/14"
    a[ HT_CLR_LIST_SEL_UNFOC ]    := "0/7"
    a[ HT_CLR_COMBO_DROP_N ]      := "15/8"
    a[ HT_CLR_COMBO_DROP_S ]      := "0/14"
    /* COLOR18F field selection: W+/N*, selection N/RB* */
    a[ HT_CLR_CHECKLIST_NORMAL ]  := "15/8"
    a[ HT_CLR_CHECKLIST_SELECTED ] := "0/13"
    a[ HT_CLR_CHECKLIST_SELUNFOC ] := "0/7"
    /* COLOR14 browse: N/W, cursor N/GR* */
    a[ HT_CLR_BROWSE_FOCUSED ]    := "0/7,0/14,0/7,0/7,0/14"
    a[ HT_CLR_BROWSE_NORMAL ]     := "0/7,0/15,0/7,0/7,0/14"
    a[ HT_CLR_BROWSE_SELECTED ]   := "0/14"
    /* COLOR5 pull-down menus: N/BG, bar W+/GR, speed letters GR+/BG, GR+/GR */
    a[ HT_CLR_MENU_BAR ]          := "0/3"
    a[ HT_CLR_MENU_BAR_SEL ]      := "15/6"
    a[ HT_CLR_MENU_ITEM ]         := "0/3"
    a[ HT_CLR_MENU_ITEM_SEL ]     := "15/6"
    a[ HT_CLR_MENU_SEP ]          := "0/3"
    a[ HT_CLR_MENU_HOTKEY ]       := "14/3"
    a[ HT_CLR_MENU_HOTKEY_SEL ]   := "14/6"
    a[ HT_CLR_CTXMENU_NORMAL ]    := "0/3"
    a[ HT_CLR_CTXMENU_SELECTED ]  := "15/6"
    a[ HT_CLR_CTXMENU_SEP ]       := "0/3"
    a[ HT_CLR_TOOLBAR_NORMAL ]    := "0/3"
    a[ HT_CLR_TOOLBAR_FOCUSED ]   := "0/14"
    a[ HT_CLR_TOOLBAR_ACTIVE ]    := "15/6"
    /* COLOR7 function key row and COLOR6A message line: W+/N* */
    a[ HT_CLR_TITLEBAR ]          := "15/8"
    a[ HT_CLR_TITLEBTN ]          := "15/8"
    a[ HT_CLR_STATUSBAR ]         := "15/8"
    a[ HT_CLR_SCROLLBAR ]         := "8/7"
    a[ HT_CLR_PROGRESS_FILL ]     := "15/4"
    a[ HT_CLR_PROGRESS_EMPTY ]    := "0/7"
    a[ HT_CLR_TAB_ACTIVE ]        := "0/15"
    a[ HT_CLR_TAB_INACTIVE ]      := "0/7"
    a[ HT_CLR_TAB_BAR ]           := "0/7"
    /* message boxes, toasts: alert N/G*, help W+/RB, warning W+/R */
    a[ HT_CLR_MSGBOX_INFO ]       := "0/10"
    a[ HT_CLR_MSGBOX_WARN ]       := "15/4"
    a[ HT_CLR_MSGBOX_QUEST ]      := "15/5"
    a[ HT_CLR_TOAST_INFO ]        := "0/10"
    a[ HT_CLR_TOAST_SUCCESS ]     := "0/10"
    a[ HT_CLR_TOAST_WARNING ]     := "0/14"
    a[ HT_CLR_TOAST_ERROR ]       := "15/4"

    HTTheme():addTheme( "BroPlus", a, aPal )

RETURN
