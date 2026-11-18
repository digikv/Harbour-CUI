/*
 * CEdit - text editor in character mode, built on Harbour-CUI
 *
 * Copyright (c) 2026 Srđan Dragojlović - dual licensed, see LICENSE
 *
 * Works like HbEdit (Alexander S. Kresin): the same F-keys, so those used to
 * it feel at home - F2 save, F3 block, F4 open, F9 menu, F10 / Esc close,
 * F12 open files ... - written from scratch on Harbour-CUI (no HbEdit code).
 *
 *   row 0         menu bar (F9, Alt + letter, mouse)
 *   rows 1 ..     the text (HTCodeEdit, htcodeedit.prg)
 *   last row      status: file, line / column, codepage, Ins / Ovr, mode
 *
 * Usage: cedit [file ...]
 */

#include "hbtui.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"
#include "cedit.ch"

REQUEST HB_CODEPAGE_UTF8EX
REQUEST HB_CODEPAGE_SL852
REQUEST HB_CODEPAGE_SLWIN
REQUEST HB_CODEPAGE_SLISO
REQUEST HB_CODEPAGE_SRWIN
REQUEST HB_CODEPAGE_SR646

STATIC s_oApp, s_oMain, s_oBar, s_oEdit, s_oStat
STATIC s_aBufs := {}
STATIC s_nBuf := 0
STATIC s_aRecent := {}
STATIC s_cDefCP := "SL852"          /* codepage of files that are not UTF-8 */

PROCEDURE Main( ... )

    LOCAL cArg

    hb_cdpSelect( "UTF8EX" )

    s_oApp := HTApplication():new()
    hb_gtInfo( HB_GTI_WINTITLE, "CEdit" )

    s_oMain := CEMain():new()
    s_oMain:setWindowTitle( "CEdit" )
    s_oMain:show()

    s_oEdit := HTCodeEdit():new( s_oMain )
    s_oEdit:onCursor := {|| ce_Status() }
    s_oStat := HTStatusBar():new( s_oMain )
    s_oStat:addSection( "" )
    ce_Arrange()
    ce_BuildMenu()

    FOR EACH cArg IN hb_AParams()
        IF ! Left( cArg, 1 ) $ "/-"
            ce_OpenFile( cArg )
        ENDIF
    NEXT
    IF Empty( s_aBufs )
        ce_NewBuffer()
    ENDIF

    s_oEdit:setFocus()
    ce_Status()
    s_oApp:exec()

RETURN

/*----------------------------------------------------------- main window */

/* F9 opens the menu, F10 / Esc close the file (HbEdit), Tab goes to the
   text (not to the next control); the rest: menu shortcuts, then the text */
CLASS CEMain FROM HTMainWindow
    METHOD keyEvent( keyEvent )
    METHOD closeEvent( closeEvent )
    METHOD screenResized()
ENDCLASS

METHOD PROCEDURE keyEvent( keyEvent ) CLASS CEMain

    LOCAL nKey := keyEvent:key

    IF s_oBar != NIL .AND. ! s_oBar:isActive()
        DO CASE
        CASE nKey == K_F9
            keyEvent:accept()
            s_oBar:popup( 1 )
            RETURN
        CASE nKey == K_F10
            keyEvent:accept()
            ce_CmdClose()
            RETURN
        CASE nKey == K_ESC
            keyEvent:accept()
            IF ! s_oEdit:cancelMarking()
                ce_CmdClose()
            ENDIF
            RETURN
        CASE nKey == K_TAB
            s_oEdit:keyEvent( keyEvent )
            RETURN
        ENDCASE
    ENDIF

    ::super:keyEvent( keyEvent )

RETURN

/* the close button of the window: ask for every changed file */
METHOD PROCEDURE closeEvent( closeEvent ) CLASS CEMain
    IF ce_AskAll()
        ::super:closeEvent( closeEvent )
    ENDIF
RETURN

METHOD PROCEDURE screenResized() CLASS CEMain
    ::super:screenResized()
    ce_Arrange()
RETURN

/* text from under the menu bar down to the status line on the last row */
STATIC PROCEDURE ce_Arrange()
    s_oEdit:setGeometry( 0, 0, s_oMain:width, Max( 1, s_oMain:height - 2 ) )
    s_oStat:setGeometry( 0, s_oMain:height - 2, s_oMain:width, 1 )
RETURN

/*----------------------------------------------------------- menu */

STATIC PROCEDURE ce_BuildMenu()

    LOCAL oMenu

    s_oBar := s_oMain:menuBar()
    s_oBar:setBarRow( 0 )

    oMenu := s_oBar:addMenu( "&File" )
    ce_Item( oMenu, "&New", "Shift-F4", K_SH_F4, {|| ce_NewBuffer() } )
    ce_Item( oMenu, "&Open...", "F4", K_F4, {|| ce_CmdOpenRecent() } )
    ce_Item( oMenu, "Open &file...", "Ctrl-F4", K_CTRL_F4, {|| ce_CmdOpen() } )
    oMenu:addSeparator()
    ce_Item( oMenu, "&Save", "F2", K_F2, {|| ce_SaveBuffer( ce_Cur(), .F. ) } )
    ce_Item( oMenu, "Save &as...", "Shift-F2", K_SH_F2, {|| ce_SaveBuffer( ce_Cur(), .T. ) } )
    ce_Item( oMenu, "Save a&ll", "", NIL, {|| ce_SaveAll() } )
    oMenu:addSeparator()
    ce_Item( oMenu, "&Close", "F10, Esc", NIL, {|| ce_CmdClose() } )
    ce_Item( oMenu, "E&xit", "Alt-X", K_ALT_X, {|| ce_CmdExit() } )

    oMenu := s_oBar:addMenu( "&Edit" )
    ce_Item( oMenu, "&Undo", "Ctrl-Z", NIL, {|| s_oEdit:undo() } )
    ce_Item( oMenu, "&Redo", "Ctrl-R", NIL, {|| s_oEdit:redo() } )
    oMenu:addSeparator()
    ce_Item( oMenu, "&Block", "F3", K_F3, {|| s_oEdit:startMarking( CE_SEL_STREAM ) } )
    ce_Item( oMenu, "&Vertical block", "Ctrl-F3", K_CTRL_F3, {|| s_oEdit:startMarking( CE_SEL_BLOCK ) } )
    ce_Item( oMenu, "Select &all", "Ctrl-A", NIL, {|| s_oEdit:selectAll() } )
    oMenu:addSeparator()
    ce_Item( oMenu, "&Copy", "Ctrl-C", NIL, {|| s_oEdit:copy() } )
    ce_Item( oMenu, "Cu&t", "Ctrl-X", NIL, {|| s_oEdit:cut() } )
    ce_Item( oMenu, "&Paste", "Ctrl-V", NIL, {|| s_oEdit:paste() } )
    ce_Item( oMenu, "&Delete line", "Ctrl-Y", NIL, {|| s_oEdit:deleteLine() } )
    oMenu:addSeparator()
    ce_Item( oMenu, "&Insert / overwrite", "Ins", NIL, {|| s_oEdit:lInsert := ! s_oEdit:lInsert } )

    oMenu := s_oBar:addMenu( "&Windows" )
    ce_Item( oMenu, "&List of files", "F12", K_F12, {|| ce_CmdBuffers() } )
    ce_Item( oMenu, "&Next file", "Ctrl-Tab", K_CTRL_TAB, {|| ce_SwitchTo( IIF( s_nBuf >= Len( s_aBufs ), 1, s_nBuf + 1 ) ) } )
    ce_Item( oMenu, "&Previous file", "Shift-Tab", K_SH_TAB, {|| ce_SwitchTo( IIF( s_nBuf <= 1, Len( s_aBufs ), s_nBuf - 1 ) ) } )

    oMenu := s_oBar:addMenu( "&Help" )
    ce_Item( oMenu, "&Help", "F1", K_F1, {|| ce_CmdHelp() } )
    ce_Item( oMenu, "&About", "", NIL, {|| ce_CmdAbout() } )

RETURN

/* menu entry "Text    Key"; nKey: the key runs it from anywhere (keys
   that the text itself uses, Ctrl-C ..., are only shown) */
STATIC PROCEDURE ce_Item( oMenu, cText, cKey, nKey, bAction )

    /* "&" is not shown: one more character for the column of the keys */
    LOCAL oAct := oMenu:addAction( PadR( cText, IIF( "&" $ cText, 21, 20 ) ) + PadL( cKey, 10 ) )

    IF nKey != NIL
        oAct:setShortcut( nKey )
    ENDIF
    oAct:onTriggered := {|| Eval( bAction ), ce_After() }

RETURN

STATIC PROCEDURE ce_After()
    IF ! Empty( s_aBufs )
        s_oEdit:refresh()
    ENDIF
RETURN

/*----------------------------------------------------------- status */

STATIC PROCEDURE ce_Status()

    LOCAL oB := ce_Cur(), cLeft, cRight, cMode

    IF oB == NIL
        RETURN
    ENDIF
    DO CASE
    CASE oB:lReadOnly
        cMode := "View"
    CASE oB:lMarking
        cMode := IIF( oB:nSelMode == CE_SEL_BLOCK, "Vert", "Sele" )
    OTHERWISE
        cMode := "Edit"
    ENDCASE
    cLeft := " " + oB:title() + IIF( oB:isModified(), " *", "" ) + ;
             IIF( Len( s_aBufs ) > 1, "  [" + hb_ntos( s_nBuf ) + "/" + hb_ntos( Len( s_aBufs ) ) + "]", "" )
    cRight := "Ln " + hb_ntos( oB:nRow ) + "/" + hb_ntos( oB:lineCount() ) + ;
              "  Col " + hb_ntos( s_oEdit:cursorCell() + 1 ) + "  " + ;
              ce_CPName( oB:cCP ) + IIF( oB:cEol == Chr( 10 ), " LF", "" ) + "  " + ;
              IIF( s_oEdit:lInsert, "Ins", "Ovr" ) + "  " + cMode + " "
    s_oStat:setSection( 1, ht_cellPadR( cLeft, Max( 1, s_oMain:width - Len( cRight ) ) ) + cRight )
    s_oMain:repaintChild( s_oStat )

RETURN

STATIC FUNCTION ce_CPName( cCP )
    SWITCH cCP
    CASE "UTF8"  ; RETURN "UTF-8"
    CASE "SL852" ; RETURN "CP852"
    CASE "SLWIN" ; RETURN "CP1250"
    CASE "SLISO" ; RETURN "ISO-8859-2"
    CASE "SRWIN" ; RETURN "CP1251"
    CASE "SR646" ; RETURN "YUSCII"
    CASE "EN"    ; RETURN "CP437"
    ENDSWITCH
RETURN cCP

/*----------------------------------------------------------- buffers */

STATIC FUNCTION ce_Cur()
RETURN IIF( s_nBuf >= 1 .AND. s_nBuf <= Len( s_aBufs ), s_aBufs[ s_nBuf ], NIL )

STATIC PROCEDURE ce_SwitchTo( n )
    IF n >= 1 .AND. n <= Len( s_aBufs )
        s_nBuf := n
        s_oEdit:setBuffer( s_aBufs[ n ] )
        hb_gtInfo( HB_GTI_WINTITLE, "CEdit - " + s_aBufs[ n ]:title() )
        s_oEdit:refresh()
    ENDIF
RETURN

STATIC PROCEDURE ce_NewBuffer()
    AAdd( s_aBufs, CEBuffer():new() )
    ce_SwitchTo( Len( s_aBufs ) )
RETURN

/* opens a file (or switches to it when it is already open); a missing file
   is a new file of that name */
STATIC FUNCTION ce_OpenFile( cFile )

    LOCAL n, oB, oCur := ce_Cur()

    cFile := ce_FullPath( cFile )
    n := AScan( s_aBufs, {| o | ce_SameFile( o:cFile, cFile ) } )
    IF n > 0
        ce_SwitchTo( n )
        RETURN .T.
    ENDIF
    oB := CEBuffer():new()
    IF ! oB:load( cFile, s_cDefCP )
        HTMessageBox():error( "CEdit", oB:cError )
        RETURN .F.
    ENDIF
    /* an untouched empty "Untitled" gives its place */
    IF oCur != NIL .AND. Empty( oCur:cFile ) .AND. Empty( oCur:cName ) .AND. ! oCur:isModified() .AND. ;
       oCur:lineCount() == 1 .AND. oCur:aLines[ 1 ] == ""
        s_aBufs[ s_nBuf ] := oB
    ELSE
        AAdd( s_aBufs, oB )
        s_nBuf := Len( s_aBufs )
    ENDIF
    ce_AddRecent( cFile )
    ce_SwitchTo( s_nBuf )

RETURN .T.

/* .T.: the buffer may go (saved, not changed, or the user does not want it) */
STATIC FUNCTION ce_AskSave( oB )

    LOCAL nChoice

    IF ! oB:isModified() .OR. oB:lReadOnly
        RETURN .T.
    ENDIF
    nChoice := HTMessageBox():choose( "CEdit", oB:title() + " has been changed. Save it?", ;
                                      { "&Save", "&Don't save", "&Cancel" }, HT_MSG_QUESTION )
    DO CASE
    CASE nChoice == 1
        RETURN ce_SaveBuffer( oB, .F. )
    CASE nChoice == 2
        RETURN .T.
    ENDCASE

RETURN .F.

STATIC FUNCTION ce_AskAll()

    LOCAL n

    FOR n := 1 TO Len( s_aBufs )
        IF s_aBufs[ n ]:isModified() .AND. ! s_aBufs[ n ]:lReadOnly
            ce_SwitchTo( n )
            IF ! ce_AskSave( s_aBufs[ n ] )
                RETURN .F.
            ENDIF
        ENDIF
    NEXT

RETURN .T.

/* F2 / Shift-F2 */
STATIC FUNCTION ce_SaveBuffer( oB, lAs )

    LOCAL cFile

    IF oB == NIL .OR. ( oB:lReadOnly .AND. ! lAs )
        RETURN .F.
    ENDIF
    cFile := oB:cFile
    IF lAs .OR. Empty( cFile )
        cFile := ce_FileDialog( "Save as", IIF( Empty( cFile ), "", cFile ) )
        IF Empty( cFile )
            RETURN .F.
        ENDIF
        IF hb_FileExists( cFile ) .AND. ! ce_SameFile( cFile, oB:cFile ) .AND. ;
           HTMessageBox():choose( "CEdit", hb_FNameNameExt( cFile ) + " exists. Replace it?", ;
                                  { "&Replace", "&Cancel" }, HT_MSG_WARNING ) != 1
            RETURN .F.
        ENDIF
    ENDIF
    IF ! oB:save( cFile )
        HTMessageBox():error( "CEdit", oB:cError )
        RETURN .F.
    ENDIF
    oB:lReadOnly := .F.
    oB:cName := ""
    ce_AddRecent( cFile )
    hb_gtInfo( HB_GTI_WINTITLE, "CEdit - " + oB:title() )
    HTToast():show( "Saved: " + oB:title(), 1500, HT_CLR_TOAST_SUCCESS )

RETURN .T.

STATIC PROCEDURE ce_SaveAll()
    LOCAL oB
    FOR EACH oB IN s_aBufs
        IF oB:isModified() .AND. ! oB:lReadOnly
            ce_SaveBuffer( oB, .F. )
        ENDIF
    NEXT
RETURN

/* F10 / Esc: closes the file; the last one ends the program (HbEdit) */
STATIC PROCEDURE ce_CmdClose()

    LOCAL oB := ce_Cur()

    IF oB == NIL .OR. ! ce_AskSave( oB )
        RETURN
    ENDIF
    hb_ADel( s_aBufs, s_nBuf, .T. )
    IF Empty( s_aBufs )
        s_nBuf := 0
        s_oEdit:setBuffer( NIL )
        HTApplication():quit()
        RETURN
    ENDIF
    ce_SwitchTo( Min( s_nBuf, Len( s_aBufs ) ) )

RETURN

STATIC PROCEDURE ce_CmdExit()
    IF ce_AskAll()
        HTApplication():quit()
    ENDIF
RETURN

/* Ctrl-F4 */
STATIC PROCEDURE ce_CmdOpen()

    LOCAL oB := ce_Cur(), cFile

    cFile := ce_FileDialog( "Open file", IIF( oB != NIL .AND. ! Empty( oB:cFile ), hb_FNameDir( oB:cFile ), "" ) )
    IF ! Empty( cFile )
        ce_OpenFile( cFile )
    ENDIF

RETURN

/* F4: the files opened lately, and "Open file..." */
STATIC PROCEDURE ce_CmdOpenRecent()

    LOCAL aItems := { "Open file...                Ctrl-F4" }, n

    IF Empty( s_aRecent )
        ce_CmdOpen()
        RETURN
    ENDIF
    AEval( s_aRecent, {| c | AAdd( aItems, c ) } )
    n := ce_ListDialog( "Open", aItems, 1 )
    DO CASE
    CASE n == 1
        ce_CmdOpen()
    CASE n > 1
        ce_OpenFile( s_aRecent[ n - 1 ] )
    ENDCASE

RETURN

STATIC PROCEDURE ce_AddRecent( cFile )

    LOCAL n := AScan( s_aRecent, {| c | ce_SameFile( c, cFile ) } )

    IF n > 0
        hb_ADel( s_aRecent, n, .T. )
    ENDIF
    hb_AIns( s_aRecent, 1, cFile, .T. )
    IF Len( s_aRecent ) > 20
        ASize( s_aRecent, 20 )
    ENDIF

RETURN

/* F12 */
STATIC PROCEDURE ce_CmdBuffers()

    LOCAL aItems := {}, oB, n

    FOR EACH oB IN s_aBufs
        AAdd( aItems, PadR( IIF( oB:isModified(), "* ", "  " ) + oB:title(), 28 ) + " " + ;
                      IIF( Empty( oB:cFile ), "", hb_FNameDir( oB:cFile ) ) )
    NEXT
    n := ce_ListDialog( "Open files", aItems, s_nBuf )
    IF n > 0
        ce_SwitchTo( n )
    ENDIF

RETURN

/* F1: the help as a read-only buffer */
STATIC PROCEDURE ce_CmdHelp()

    LOCAL n := AScan( s_aBufs, {| o | o:cName == "$Help" } ), oB

    IF n > 0
        ce_SwitchTo( n )
        RETURN
    ENDIF
    oB := CEBuffer():new()
    oB:setText( ce_HelpText() )
    oB:cName := "$Help"
    oB:lReadOnly := .T.
    AAdd( s_aBufs, oB )
    ce_SwitchTo( Len( s_aBufs ) )

RETURN

STATIC PROCEDURE ce_CmdAbout()
    HTMessageBox():information( "CEdit", "CEdit " + CE_VERSION + " - text editor in character mode" + Chr( 10 ) + ;
        "built on Harbour-CUI" + Chr( 10 ) + Chr( 10 ) + ;
        "Copyright (c) 2026 Srđan Dragojlović" + Chr( 10 ) + ;
        "GPL-3.0-or-later or a commercial license" + Chr( 10 ) + Chr( 10 ) + ;
        "Keys as in HbEdit by Alexander S. Kresin" )
RETURN

STATIC FUNCTION ce_HelpText()
RETURN ce_Join( { ;
    "CEdit " + CE_VERSION + " - text editor in character mode (Harbour-CUI)", ;
    "", ;
    "F9 opens the menu (also Alt + the underlined letter, or the mouse); the menu", ;
    "shows the keys of its commands. Esc closes this help.", ;
    "", ;
    "Files", ;
    "  F2           save                  Shift-F2   save as", ;
    "  F4           open (recent files)   Ctrl-F4    open file", ;
    "  Shift-F4     new file              F12        list of open files", ;
    "  Ctrl-Tab     next file             Shift-Tab  previous file", ;
    "  F10, Esc     close the file (the last one ends CEdit)", ;
    "  Alt-X        exit", ;
    "", ;
    "Editing", ;
    "  Ins          insert / overwrite    Enter      new line, keeps the indent", ;
    "  Tab          spaces to the next tab stop; with lines selected: indent", ;
    "  Ctrl-Y       delete line           Ctrl-Z, Alt-BS  undo   Ctrl-R  redo", ;
    "", ;
    "Selection and clipboard", ;
    "  Shift + cursor keys, the mouse (drag; double click: a word), Ctrl-A all", ;
    "  F3           block: the cursor keys select, F3 again ends it", ;
    "  Ctrl-F3      vertical block", ;
    "  Esc          cancel the selection", ;
    "  Ctrl-C, Ctrl-Ins   copy        Ctrl-X, Shift-Del   cut", ;
    "  Ctrl-V, Shift-Ins  paste (a vertical block goes in as a block)", ;
    "", ;
    "Cursor", ;
    "  Home         first text of the line, again: line start", ;
    "  Ctrl-Left/Right   word        Ctrl-Home / Ctrl-End  start / end of the text", ;
    "  Ctrl-PgUp/PgDn    top / bottom of the screen", ;
    "  Ctrl-Up/Down      scroll a line", ;
    "", ;
    "Files that are not UTF-8 are read and written in CP852 (the status line shows", ;
    "the codepage). Line ends (CRLF / LF) and a UTF-8 BOM are kept." }, Chr( 10 ) )

/*----------------------------------------------------------- dialogs */

/* list of items, Enter / double click chooses; returns the number, 0 = Esc */
STATIC FUNCTION ce_ListDialog( cTitle, aItems, nCurrent )

    LOCAL dlg, oList, nW, nH, nResult := 0, c

    nW := 20
    AEval( aItems, {| x | nW := Max( nW, ht_strWidth( x ) + 6 ) } )
    nW := Min( nW, ht_ScreenMaxCol() - 3 )
    nH := Min( Len( aItems ) + 4, ht_ScreenMaxRow() - 3 )

    dlg := HTDialog():new()
    dlg:setWindowTitle( cTitle )
    dlg:resize( nW, nH )
    dlg:move( Int( ( ht_ScreenMaxCol() + 1 - nW ) / 2 ), Int( ( ht_ScreenMaxRow() + 1 - nH ) / 2 ) )

    oList := HTListBox():new( dlg )
    oList:setGeometry( 1, 1, nW - 4, nH - 3 )
    FOR EACH c IN aItems
        oList:addItem( c )
    NEXT
    oList:setCurrentIndex( Max( 1, Min( nCurrent, Len( aItems ) ) ) )
    oList:onActivated := {| n | nResult := n, dlg:accept() }

    dlg:exec()

RETURN nResult

/* file name: a list of the folder (Enter on a folder goes into it, on a file
   takes it) and an input line (a name, a path, or a folder + Enter);
   returns the full path, "" = cancelled */
STATIC FUNCTION ce_FileDialog( cTitle, cStart )

    LOCAL dlg, oLbl, oName, oList, nW, nH, cResult := ""
    LOCAL cDir := hb_FNameDir( cStart ), cName := hb_FNameNameExt( cStart )
    LOCAL bTake

    IF Empty( cDir ) .OR. ! hb_DirExists( cDir )
        cDir := hb_cwd()
    ENDIF
    cDir := hb_DirSepAdd( cDir )

    nW := Min( 76, ht_ScreenMaxCol() - 3 )
    nH := Min( 22, ht_ScreenMaxRow() - 2 )

    dlg := HTDialog():new()
    dlg:setWindowTitle( cTitle )
    dlg:resize( nW, nH )
    dlg:move( Int( ( ht_ScreenMaxCol() + 1 - nW ) / 2 ), Int( ( ht_ScreenMaxRow() + 1 - nH ) / 2 ) )

    oLbl := HTLabel():new( "", dlg )
    oLbl:setGeometry( 1, 0, nW - 4, 1 )
    oName := CEPathEdit():new( dlg )
    oName:setGeometry( 1, 1, nW - 4, 1 )
    oName:setText( cName )
    oList := HTListBox():new( dlg )
    oList:setGeometry( 1, 3, nW - 4, nH - 7 )

    /* a name: a folder -> into it; else the file -> done */
    bTake := {| cTxt, cPath |
        cTxt := AllTrim( cTxt )
        IF Empty( cTxt )
            RETURN NIL
        ENDIF
        cPath := IIF( ce_IsAbsolute( cTxt ), cTxt, cDir + cTxt )
        IF hb_DirExists( cPath )
            cDir := IIF( cTxt == "..", ce_ParentDir( cDir ), hb_DirSepAdd( hb_DirSepToOS( cPath ) ) )
            ce_FillDir( oList, oLbl, cDir )
            oName:setText( "" )
            dlg:repaint()
        ELSE
            cResult := cPath
            dlg:accept()
        ENDIF
        RETURN NIL
    }
    oName:bEnter := {|| Eval( bTake, oName:text ) }
    oList:onActivated := {|| Eval( bTake, oList:currentData() ) }
    oList:onChanged := {|| IIF( HB_ISSTRING( oList:currentData() ) .AND. ! hb_DirExists( cDir + oList:currentData() ), ;
                                ( oName:setText( oList:currentData() ), dlg:repaintChild( oName ) ), NIL ) }

    ce_FillDir( oList, oLbl, cDir )
    dlg:addButtonBar( "OK", "Cancel" )

    IF dlg:exec() == HT_DIALOG_ACCEPTED .AND. Empty( cResult ) .AND. ! Empty( oName:text )
        cResult := IIF( ce_IsAbsolute( AllTrim( oName:text ) ), AllTrim( oName:text ), cDir + AllTrim( oName:text ) )
        IF hb_DirExists( cResult )
            cResult := ""
        ENDIF
    ENDIF

RETURN cResult

STATIC PROCEDURE ce_FillDir( oList, oLbl, cDir )

    LOCAL aDirs := {}, aFiles := {}, a

    FOR EACH a IN Directory( cDir + hb_osFileMask(), "D" )
        IF a[ 1 ] == "." .OR. a[ 1 ] == ".."
            LOOP
        ENDIF
        IF "D" $ a[ 5 ]
            AAdd( aDirs, a[ 1 ] )
        ELSE
            AAdd( aFiles, { a[ 1 ], a[ 2 ] } )
        ENDIF
    NEXT
    ASort( aDirs, , , {| x, y | Upper( x ) < Upper( y ) } )
    ASort( aFiles, , , {| x, y | Upper( x[ 1 ] ) < Upper( y[ 1 ] ) } )

    oList:clear()
    IF ! ce_IsRoot( cDir )
        oList:addItem( "[..]", ".." )
    ENDIF
    FOR EACH a IN aDirs
        oList:addItem( "[" + a + "]", a )
    NEXT
    FOR EACH a IN aFiles
        oList:addItem( ht_cellPadR( a[ 1 ], Max( 10, oList:width - 14 ) ) + Str( a[ 2 ], 12 ), a[ 1 ] )
    NEXT
    oList:setCurrentIndex( 1 )
    oLbl:setText( cDir )

RETURN

/* input line of the file dialog: Enter takes the name (not the next field) */
CLASS CEPathEdit FROM HTLineEdit
    DATA bEnter
    METHOD enterMovesFocus() INLINE .F.
    METHOD keyEvent( keyEvent )
ENDCLASS

METHOD PROCEDURE keyEvent( keyEvent ) CLASS CEPathEdit
    IF keyEvent:key == K_ENTER .AND. ::bEnter != NIL
        keyEvent:accept()
        Eval( ::bEnter )
        RETURN
    ENDIF
    ::super:keyEvent( keyEvent )
RETURN

/*----------------------------------------------------------- paths */

STATIC FUNCTION ce_IsAbsolute( c )
RETURN Left( c, 1 ) $ "\/" .OR. SubStr( c, 2, 1 ) == ":"

STATIC FUNCTION ce_FullPath( cFile )
    cFile := hb_DirSepToOS( AllTrim( cFile ) )
    IF ! ce_IsAbsolute( cFile )
        cFile := hb_cwd() + cFile
    ENDIF
RETURN cFile

/* "C:\a\b\" -> "C:\a\" */
STATIC FUNCTION ce_ParentDir( cDir )

    LOCAL cUp := hb_FNameDir( Left( cDir, Len( cDir ) - 1 ) )

RETURN IIF( Empty( cUp ), cDir, cUp )

STATIC FUNCTION ce_IsRoot( cDir )
RETURN Len( cDir ) <= 3 .AND. ( SubStr( cDir, 2, 1 ) == ":" .OR. cDir == hb_ps() )

STATIC FUNCTION ce_SameFile( c1, c2 )
#ifdef __PLATFORM__WINDOWS
RETURN ! Empty( c1 ) .AND. Lower( c1 ) == Lower( c2 )
#else
RETURN ! Empty( c1 ) .AND. c1 == c2
#endif
