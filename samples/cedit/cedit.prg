/*
 * CEdit - text editor in character mode, built on Harbour-CUI
 *
 * Copyright (c) 2026 Srđan Dragojlović - dual licensed, see LICENSE
 *
 * Works like HbEdit (Alexander S. Kresin): the same F-keys, so those used to
 * it feel at home - F2 save, F3 block, F4 open, F7 search, F8 syntax,
 * F9 menu, F10 / Esc close, F12 open files ... - written from scratch on
 * Harbour-CUI (no HbEdit code).
 *
 *   row 0         menu bar (F9, Alt + letter, mouse)
 *   rows 1 ..     the text (HTCodeEdit, htcodeedit.prg)
 *   last row      status: file, line / column, codepage, Ins / Ovr, mode
 *
 * Settings and the recent files (with the cursor position) are kept in
 * cedit.ini: next to the program if it is there, else in %APPDATA%\CEdit
 * (Windows) or ~/.config/cedit (Linux).
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
STATIC s_aRecent := {}               /* { { cFile, nRow, nCol }, ... } the newest first */
STATIC s_hSet                        /* settings (cedit.ini [OPTIONS]) */
STATIC s_cIni := ""

/* search */
STATIC s_cFind := ""
STATIC s_cRepl := ""
STATIC s_lCase := .F.
STATIC s_lWord := .F.
STATIC s_lAsk  := .T.

PROCEDURE Main( ... )

    LOCAL cArg

    hb_cdpSelect( "UTF8EX" )
    ce_LoadSettings()

    s_oApp := HTApplication():new()
    hb_gtInfo( HB_GTI_WINTITLE, "CEdit" )
    IF ! s_hSet[ "theme" ] == "Classic" .AND. HTTheme():hasTheme( s_hSet[ "theme" ] )
        HTTheme():loadTheme( s_hSet[ "theme" ] )
    ENDIF

    s_oMain := CEMain():new()
    s_oMain:setWindowTitle( "CEdit" )
    s_oMain:show()

    s_oEdit := HTCodeEdit():new( s_oMain )
    s_oEdit:onCursor := {|| ce_Status() }
    s_oEdit:lInsert := s_hSet[ "insert" ]
    s_oEdit:lTabSpaces := s_hSet[ "tabspaces" ]
    ce_SetEditorColors( s_hSet[ "colors" ] )
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

    ce_SaveSettings()

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
    ce_Item( oMenu, "Upper case", "", NIL, {|| ce_CaseSel( .T. ) } )
    ce_Item( oMenu, "Lower case", "", NIL, {|| ce_CaseSel( .F. ) } )
    ce_Item( oMenu, "&Insert / overwrite", "Ins", NIL, {|| s_oEdit:lInsert := ! s_oEdit:lInsert } )

    oMenu := s_oBar:addMenu( "&Search" )
    ce_Item( oMenu, "&Find...", "F7", K_F7, {|| ce_CmdFind() } )
    ce_Item( oMenu, "Find &next", "Shift-F7", K_SH_F7, {|| ce_FindNext( .F. ) } )
    ce_Item( oMenu, "Find &previous", "Alt-F7", K_ALT_F7, {|| ce_FindNext( .T. ) } )
    ce_Item( oMenu, "&Replace...", "Ctrl-F7", K_CTRL_F7, {|| ce_CmdReplace() } )
    oMenu:addSeparator()
    ce_Item( oMenu, "&Word at cursor", "Alt-8", K_ALT_8, {|| ce_FindWord( .F. ) } )
    ce_Item( oMenu, "Word, b&ackwards", "Alt-3", K_ALT_3, {|| ce_FindWord( .T. ) } )
    oMenu:addSeparator()
    ce_Item( oMenu, "&Go to line...", "Alt-F8", K_ALT_F8, {|| ce_CmdGoto() } )
    ce_Item( oMenu, "&Matching bracket", "Ctrl-B", NIL, {|| s_oEdit:matchBracket() } )
    ce_Item( oMenu, "&Back", "Alt-B", K_ALT_B, {|| ce_CmdBack() } )

    oMenu := s_oBar:addMenu( "&View" )
    ce_Item( oMenu, "&Syntax highlighting", "F8", K_F8, {|| ce_CmdHilite() } )
    ce_Item( oMenu, "&Language...", "", NIL, {|| ce_CmdSyntax() } )
    ce_Item( oMenu, "&Codepage...", "Shift-F8", K_SH_F8, {|| ce_CmdCodepage() } )
    oMenu:addSeparator()
    ce_Item( oMenu, "&Theme...", "", NIL, {|| ce_CmdTheme() } )
    ce_Item( oMenu, "&Editor colors...", "", NIL, {|| ce_CmdColors() } )
    ce_Item( oMenu, "&Options...", "", NIL, {|| ce_CmdOptions() } )

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
              IIF( Empty( oB:cSyntax ) .OR. ! oB:lHilite, "", oB:cSyntax + "  " ) + ;
              ce_CPName( oB:cCP ) + IIF( oB:cEol == Chr( 10 ), " LF", "" ) + "  " + ;
              IIF( s_oEdit:lInsert, "Ins", "Ovr" ) + "  " + cMode + " "
    s_oStat:setSection( 1, ht_cellPadR( cLeft, Max( 1, s_oMain:width - Len( cRight ) ) ) + cRight )
    s_oMain:repaintChild( s_oStat )

RETURN

/* Harbour codepage -> its usual name */
STATIC FUNCTION ce_CPName( cCP )
    LOCAL n := AScan( ce_CPList(), {| a | a[ 1 ] == cCP } )
RETURN IIF( n > 0, ce_CPList()[ n ][ 2 ], cCP )

STATIC FUNCTION ce_CPList()
RETURN { { "UTF8",  "UTF-8",      "UTF-8 (Unicode)" }, ;
         { "SL852", "CP852",      "CP852 - DOS Latin 2 (Harbour DBF)" }, ;
         { "SLWIN", "CP1250",     "CP1250 - Windows Latin 2" }, ;
         { "SLISO", "ISO-8859-2", "ISO-8859-2 - Latin 2" }, ;
         { "SRWIN", "CP1251",     "CP1251 - Windows Cyrillic" }, ;
         { "SR646", "YUSCII",     "YUSCII - ISO-646 (7 bit)" } }

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

/* a buffer with the settings */
STATIC FUNCTION ce_Buffer()
    LOCAL oB := CEBuffer():new()
    oB:nTab := s_hSet[ "tab" ]
    oB:lHilite := s_hSet[ "hilite" ]
RETURN oB

STATIC PROCEDURE ce_NewBuffer()
    AAdd( s_aBufs, ce_Buffer() )
    ce_SwitchTo( Len( s_aBufs ) )
RETURN

/* opens a file (or switches to it when it is already open); a missing file
   is a new file of that name; a recent file gets its cursor back */
STATIC FUNCTION ce_OpenFile( cFile )

    LOCAL n, oB, oCur := ce_Cur()

    cFile := ce_FullPath( cFile )
    n := AScan( s_aBufs, {| o | ce_SameFile( o:cFile, cFile ) } )
    IF n > 0
        ce_SwitchTo( n )
        RETURN .T.
    ENDIF
    oB := ce_Buffer()
    IF ! oB:load( cFile, s_hSet[ "codepage" ] )
        HTMessageBox():error( "CEdit", oB:cError )
        RETURN .F.
    ENDIF
    oB:cSyntax := ce_SyntaxFor( cFile )
    n := AScan( s_aRecent, {| a | ce_SameFile( a[ 1 ], cFile ) } )
    IF n > 0
        oB:nRow := Max( 1, Min( s_aRecent[ n ][ 2 ], oB:lineCount() ) )
        oB:nCol := Max( 1, Min( s_aRecent[ n ][ 3 ], Len( oB:line( oB:nRow ) ) + 1 ) )
        oB:nTop := Max( 1, oB:nRow - 5 )
    ENDIF
    /* an untouched empty "Untitled" gives its place */
    IF oCur != NIL .AND. Empty( oCur:cFile ) .AND. Empty( oCur:cName ) .AND. ! oCur:isModified() .AND. ;
       oCur:lineCount() == 1 .AND. oCur:aLines[ 1 ] == ""
        s_aBufs[ s_nBuf ] := oB
    ELSE
        AAdd( s_aBufs, oB )
        s_nBuf := Len( s_aBufs )
    ENDIF
    ce_AddRecent( oB )
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
    AEval( s_aBufs, {| o | ce_AddRecent( o ) } )

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
    IF oB:lReadOnly .OR. Empty( oB:cSyntax )      /* the help saved as a file, an Untitled */
        oB:cSyntax := ce_SyntaxFor( cFile )
        oB:nStateValid := 1
    ENDIF
    oB:lReadOnly := .F.
    oB:cName := ""
    ce_AddRecent( oB )
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
    ce_AddRecent( oB )
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
    AEval( s_aRecent, {| a | AAdd( aItems, a[ 1 ] ) } )
    n := ce_ListDialog( "Open", aItems, 1 )
    DO CASE
    CASE n == 1
        ce_CmdOpen()
    CASE n > 1
        ce_OpenFile( s_aRecent[ n - 1 ][ 1 ] )
    ENDCASE

RETURN

/* the file of oB to the top of the recent files, with its cursor */
STATIC PROCEDURE ce_AddRecent( oB )

    LOCAL n

    IF Empty( oB:cFile )
        RETURN
    ENDIF
    n := AScan( s_aRecent, {| a | ce_SameFile( a[ 1 ], oB:cFile ) } )
    IF n > 0
        hb_ADel( s_aRecent, n, .T. )
    ENDIF
    hb_AIns( s_aRecent, 1, { oB:cFile, oB:nRow, oB:nCol }, .T. )
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
    oB := ce_Buffer()
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
        "Keys as in HbEdit by Alexander S. Kresin" + Chr( 10 ) + ;
        "Settings: " + s_cIni )
RETURN

/*----------------------------------------------------------- editing */

/* upper / lower case of the selection */
STATIC PROCEDURE ce_CaseSel( lUpper )

    LOCAL oB := ce_Cur(), a, cText, aEnd

    IF oB == NIL .OR. oB:lReadOnly .OR. ! oB:hasSel() .OR. oB:nSelMode != CE_SEL_STREAM
        RETURN
    ENDIF
    a := oB:streamRange()
    cText := oB:selText()
    oB:startAction( "edit" )
    oB:deleteRange( a[ 1 ], a[ 2 ], a[ 3 ], a[ 4 ] )
    aEnd := oB:insertText( a[ 1 ], a[ 2 ], IIF( lUpper, Upper( cText ), Lower( cText ) ) )
    oB:endAction( "edit" )
    oB:nSelRow := a[ 1 ]
    oB:nSelCol := a[ 2 ]
    oB:nRow := aEnd[ 1 ]
    oB:nCol := aEnd[ 2 ]

RETURN

/*----------------------------------------------------------- search */

/* F7 */
STATIC PROCEDURE ce_CmdFind()
    IF ce_SearchDialog( .F. )
        ce_FindNext( .F. )
    ENDIF
RETURN

/* Shift-F7 / Alt-F7: the next / previous match; it is selected */
STATIC PROCEDURE ce_FindNext( lBack )

    LOCAL oB := ce_Cur(), a, nRow, nCol

    IF oB == NIL
        RETURN
    ENDIF
    IF Len( s_cFind ) == 0
        ce_CmdFind()
        RETURN
    ENDIF
    nRow := oB:nRow
    nCol := oB:nCol
    IF lBack .AND. oB:hasSel() .AND. oB:nSelMode == CE_SEL_STREAM
        a := oB:streamRange()
        nRow := a[ 1 ]
        nCol := a[ 2 ]
    ENDIF
    a := oB:find( s_cFind, nRow, nCol, s_lCase, s_lWord, lBack, .T. )
    IF a == NIL
        HTToast():show( "Not found: " + s_cFind, 2000, HT_CLR_TOAST_WARNING )
        RETURN
    ENDIF
    IF a[ 3 ]
        HTToast():show( IIF( lBack, "Searched on from the end", "Searched on from the start" ), 1500, HT_CLR_TOAST_INFO )
    ENDIF
    oB:pushBack()
    ce_SelectAt( oB, a[ 1 ], a[ 2 ], Len( s_cFind ) )

RETURN

/* selects nLen characters at nRow, nCol, the line in the middle of the screen */
STATIC PROCEDURE ce_SelectAt( oB, nRow, nCol, nLen )
    oB:clearSel()
    oB:nSelMode := CE_SEL_STREAM
    oB:nSelRow := nRow
    oB:nSelCol := nCol
    oB:nRow := nRow
    oB:nCol := nCol + nLen
    oB:nWantCell := -1
    oB:cLastKind := ""
    ce_Center( oB )
RETURN

STATIC PROCEDURE ce_Center( oB )
    IF oB:nRow < oB:nTop .OR. oB:nRow >= oB:nTop + s_oEdit:visRows()
        oB:nTop := Max( 1, oB:nRow - Int( s_oEdit:visRows() / 2 ) )
    ENDIF
RETURN

/* Alt-8 / Alt-3: the word at the cursor, as a whole word */
STATIC PROCEDURE ce_FindWord( lBack )

    LOCAL oB := ce_Cur(), cWord

    IF oB == NIL .OR. Empty( cWord := s_oEdit:wordAtCursor() )
        RETURN
    ENDIF
    s_cFind := cWord
    s_lWord := .T.
    s_oEdit:selectWord( oB:nRow, IIF( oB:nCol > Len( oB:line( oB:nRow ) ) .OR. ! ce_IsWordChar( SubStr( oB:line( oB:nRow ), oB:nCol, 1 ) ), oB:nCol - 1, oB:nCol ) )
    ce_FindNext( lBack )

RETURN

/* Ctrl-F7: replace, asking for every match (Yes / No / All / Cancel) */
STATIC PROCEDURE ce_CmdReplace()

    LOCAL oB := ce_Cur(), a, aEnd, nRow, nCol, nCount := 0, lAll, nChoice

    IF oB == NIL .OR. oB:lReadOnly .OR. ! ce_SearchDialog( .T. )
        RETURN
    ENDIF
    lAll := ! s_lAsk
    nRow := oB:nRow
    nCol := oB:nCol
    IF oB:hasSel() .AND. oB:nSelMode == CE_SEL_STREAM
        a := oB:streamRange()
        nRow := a[ 1 ]
        nCol := a[ 2 ]
    ENDIF
    oB:pushBack()
    IF lAll
        oB:startAction( "edit" )                  /* all of them: one undo */
    ENDIF
    DO WHILE ( a := oB:find( s_cFind, nRow, nCol, s_lCase, s_lWord, .F., .F. ) ) != NIL
        IF ! lAll
            ce_SelectAt( oB, a[ 1 ], a[ 2 ], Len( s_cFind ) )
            s_oEdit:refresh()
            nChoice := HTMessageBox():choose( "Replace", "Replace this one?", ;
                                              { "&Yes", "&No", "&All", "&Cancel" }, HT_MSG_QUESTION )
            DO CASE
            CASE nChoice == 2
                nRow := a[ 1 ]
                nCol := a[ 2 ] + Len( s_cFind )
                LOOP
            CASE nChoice == 3
                lAll := .T.
                oB:startAction( "edit" )
            CASE nChoice != 1
                EXIT
            OTHERWISE
                oB:startAction( "edit" )
            ENDCASE
        ENDIF
        oB:deleteRange( a[ 1 ], a[ 2 ], a[ 1 ], a[ 2 ] + Len( s_cFind ) )
        aEnd := oB:insertText( a[ 1 ], a[ 2 ], s_cRepl )
        oB:endAction( "edit" )
        nCount++
        nRow := aEnd[ 1 ]
        nCol := aEnd[ 2 ]
        oB:nRow := nRow
        oB:nCol := nCol
    ENDDO
    oB:clearSel()
    oB:nRow := nRow
    oB:nCol := nCol
    ce_Center( oB )
    HTToast():show( "Replaced: " + hb_ntos( nCount ), 2000, IIF( nCount > 0, HT_CLR_TOAST_SUCCESS, HT_CLR_TOAST_WARNING ) )

RETURN

/* the Find / Replace dialog; .T. = OK with a text to find */
STATIC FUNCTION ce_SearchDialog( lReplace )

    LOCAL dlg, oFind, oRepl, oCase, oWord, oAsk, nW := 64, nH := IIF( lReplace, 11, 8 ), nRow
    LOCAL oB := ce_Cur(), cInit := s_cFind, a

    /* the selection (one line) or the word at the cursor */
    IF oB != NIL
        a := oB:streamRange()
        IF a != NIL .AND. oB:nSelMode == CE_SEL_STREAM .AND. a[ 1 ] == a[ 3 ]
            cInit := oB:selText()
        ELSEIF ! Empty( s_oEdit:wordAtCursor() )
            cInit := s_oEdit:wordAtCursor()
        ENDIF
    ENDIF

    dlg := HTDialog():new()
    dlg:setWindowTitle( IIF( lReplace, "Replace", "Find" ) )
    dlg:resize( nW, nH )
    dlg:move( Int( ( ht_ScreenMaxCol() + 1 - nW ) / 2 ), Int( ( ht_ScreenMaxRow() + 1 - nH ) / 2 ) )

    HTLabel():new( "Find:", dlg ):setGeometry( 1, 1, 14, 1 )
    oFind := CEPathEdit():new( dlg )
    oFind:setGeometry( 15, 1, nW - 18, 1 )
    oFind:setText( cInit )
    oFind:bEnter := {|| dlg:accept() }
    nRow := 3
    IF lReplace
        HTLabel():new( "Replace with:", dlg ):setGeometry( 1, 2, 14, 1 )
        oRepl := CEPathEdit():new( dlg )
        oRepl:setGeometry( 15, 2, nW - 18, 1 )
        oRepl:setText( s_cRepl )
        oRepl:bEnter := {|| dlg:accept() }
        nRow := 4
    ENDIF
    oCase := HTCheckBox():new( "Case sensitive", dlg )
    oCase:setGeometry( 15, nRow, 30, 1 )
    oCase:checked := s_lCase
    oWord := HTCheckBox():new( "Whole words", dlg )
    oWord:setGeometry( 15, nRow + 1, 30, 1 )
    oWord:checked := s_lWord
    IF lReplace
        oAsk := HTCheckBox():new( "Ask before each one", dlg )
        oAsk:setGeometry( 15, nRow + 2, 30, 1 )
        oAsk:checked := s_lAsk
    ENDIF
    dlg:addButtonBar( "OK", "Cancel" )

    IF dlg:exec() != HT_DIALOG_ACCEPTED .OR. Len( oFind:text ) == 0
        RETURN .F.
    ENDIF
    s_cFind := oFind:text
    s_lCase := oCase:checked
    s_lWord := oWord:checked
    IF lReplace
        s_cRepl := oRepl:text
        s_lAsk := oAsk:checked
    ENDIF

RETURN .T.

/* Alt-F8: "line" or "line,column" */
STATIC PROCEDURE ce_CmdGoto()

    LOCAL oB := ce_Cur(), cText, a

    IF oB == NIL
        RETURN
    ENDIF
    cText := ce_InputDialog( "Go to", "Line[,column]:", hb_ntos( oB:nRow ) )
    IF Empty( cText )
        RETURN
    ENDIF
    a := hb_ATokens( StrTran( cText, " ", "" ), "," )
    oB:pushBack()
    s_oEdit:gotoPos( Val( a[ 1 ] ), IIF( Len( a ) > 1, ce_CellToCol( oB:line( Max( 1, Min( Val( a[ 1 ] ), oB:lineCount() ) ) ), Val( a[ 2 ] ) - 1, oB:nTab ), 1 ) )
    ce_Center( oB )

RETURN

/* Alt-B: back to where the cursor was before a jump */
STATIC PROCEDURE ce_CmdBack()

    LOCAL oB := ce_Cur(), a

    IF oB == NIL .OR. Empty( oB:aBack )
        RETURN
    ENDIF
    a := ATail( oB:aBack )
    ASize( oB:aBack, Len( oB:aBack ) - 1 )
    s_oEdit:gotoPos( a[ 1 ], a[ 2 ] )
    ce_Center( oB )

RETURN

/*----------------------------------------------------------- view */

/* F8 */
STATIC PROCEDURE ce_CmdHilite()

    LOCAL oB := ce_Cur()

    IF oB != NIL
        IF Empty( oB:cSyntax )
            ce_CmdSyntax()
        ELSE
            oB:lHilite := ! oB:lHilite
        ENDIF
    ENDIF

RETURN

STATIC PROCEDURE ce_CmdSyntax()

    LOCAL oB := ce_Cur(), aNames := ce_SyntaxNames(), aItems := { "Plain text" }, n

    IF oB == NIL
        RETURN
    ENDIF
    AEval( aNames, {| c | AAdd( aItems, ce_SyntaxTitle( c ) ) } )
    n := ce_ListDialog( "Language", aItems, AScan( aNames, {| c | c == oB:cSyntax } ) + 1 )
    IF n > 0
        oB:cSyntax := IIF( n == 1, "", aNames[ n - 1 ] )
        oB:lHilite := .T.
        oB:nStateValid := 1
    ENDIF

RETURN

/* Shift-F8: an unchanged file is read again in the codepage; a changed one
   (or a new one) is saved in it */
STATIC PROCEDURE ce_CmdCodepage()

    LOCAL oB := ce_Cur(), aCP := ce_CPList(), aItems := {}, n, nRow, nCol

    IF oB == NIL
        RETURN
    ENDIF
    AEval( aCP, {| a | AAdd( aItems, a[ 3 ] ) } )
    n := ce_ListDialog( "Codepage of " + oB:title(), aItems, AScan( aCP, {| a | a[ 1 ] == oB:cCP } ) )
    IF n == 0 .OR. aCP[ n ][ 1 ] == oB:cCP
        RETURN
    ENDIF
    IF ! Empty( oB:cFile ) .AND. hb_FileExists( oB:cFile ) .AND. ! oB:isModified()
        nRow := oB:nRow
        nCol := oB:nCol
        IF oB:load( oB:cFile, aCP[ n ][ 1 ], .T. )
            oB:nRow := Min( nRow, oB:lineCount() )
            oB:nCol := Min( nCol, Len( oB:line( oB:nRow ) ) + 1 )
            HTToast():show( "Read in " + aCP[ n ][ 2 ], 1500, HT_CLR_TOAST_INFO )
        ELSE
            HTMessageBox():error( "CEdit", oB:cError )
        ENDIF
    ELSE
        oB:cCP := aCP[ n ][ 1 ]
        HTToast():show( "It will be saved in " + aCP[ n ][ 2 ], 2000, HT_CLR_TOAST_INFO )
    ENDIF

RETURN

STATIC PROCEDURE ce_CmdTheme()

    LOCAL aNames := HTTheme():themeNames(), n

    n := ce_ListDialog( "Theme", aNames, AScan( aNames, {| c | Lower( c ) == Lower( HTTheme():themeName() ) } ) )
    IF n > 0
        HTTheme():loadTheme( aNames[ n ] )
        s_hSet[ "theme" ] := aNames[ n ]
        HTApplication():repaintAll()
        ce_SaveSettings()
    ENDIF

RETURN

/* the colors of the text: the theme's, or a classic editor look */
STATIC FUNCTION ce_ColorSets()
RETURN { { "",      "",     "Theme colors" }, ;
         { "W+/B",  "N/BG", "White on blue (HbEdit, Norton)" }, ;
         { "GR+/B", "B/W",  "Yellow on blue" }, ;
         { "W/N",   "N/W",  "Gray on black" }, ;
         { "N/W*",  "W+/B", "Black on white" } }

STATIC PROCEDURE ce_SetEditorColors( cColor )
    LOCAL a := ce_ColorSets(), n := AScan( a, {| x | x[ 1 ] == cColor } )
    n := Max( 1, n )
    s_oEdit:cColor := a[ n ][ 1 ]
    s_oEdit:cSelColor := a[ n ][ 2 ]
RETURN

STATIC PROCEDURE ce_CmdColors()

    LOCAL a := ce_ColorSets(), aItems := {}, n

    AEval( a, {| x | AAdd( aItems, x[ 3 ] ) } )
    n := ce_ListDialog( "Editor colors", aItems, Max( 1, AScan( a, {| x | x[ 1 ] == s_oEdit:cColor } ) ) )
    IF n > 0
        s_hSet[ "colors" ] := a[ n ][ 1 ]
        ce_SetEditorColors( a[ n ][ 1 ] )
        ce_SaveSettings()
    ENDIF

RETURN

/* tab size, Tab with spaces, highlighting, codepage of non-UTF-8 files */
STATIC PROCEDURE ce_CmdOptions()

    LOCAL dlg, oTab, oSpaces, oHilite, oCP, aCP := ce_CPList(), a, nW := 60, nH := 11, oB

    dlg := HTDialog():new()
    dlg:setWindowTitle( "Options" )
    dlg:resize( nW, nH )
    dlg:move( Int( ( ht_ScreenMaxCol() + 1 - nW ) / 2 ), Int( ( ht_ScreenMaxRow() + 1 - nH ) / 2 ) )

    HTLabel():new( "Tab size:", dlg ):setGeometry( 1, 1, 24, 1 )
    oTab := HTLineEdit():new( dlg )
    oTab:setGeometry( 26, 1, 4, 1 )
    oTab:setText( hb_ntos( s_hSet[ "tab" ] ) )
    oSpaces := HTCheckBox():new( "Tab inserts spaces", dlg )
    oSpaces:setGeometry( 26, 2, 30, 1 )
    oSpaces:checked := s_hSet[ "tabspaces" ]
    oHilite := HTCheckBox():new( "Syntax highlighting", dlg )
    oHilite:setGeometry( 26, 3, 30, 1 )
    oHilite:checked := s_hSet[ "hilite" ]
    HTLabel():new( "Files not in UTF-8:", dlg ):setGeometry( 1, 5, 24, 1 )
    oCP := HTComboBox():new( dlg )
    oCP:setGeometry( 26, 5, 30, 1 )
    FOR EACH a IN aCP
        IF !( a[ 1 ] == "UTF8" )
            oCP:addItem( a[ 3 ], a[ 1 ] )
        ENDIF
    NEXT
    oCP:setCurrentIndex( Max( 1, AScan( aCP, {| x | x[ 1 ] == s_hSet[ "codepage" ] } ) - 1 ) )
    dlg:addButtonBar( "OK", "Cancel" )

    IF dlg:exec() == HT_DIALOG_ACCEPTED
        s_hSet[ "tab" ] := Max( 1, Min( 16, Val( oTab:text ) ) )
        s_hSet[ "tabspaces" ] := oSpaces:checked
        s_hSet[ "hilite" ] := oHilite:checked
        IF HB_ISSTRING( oCP:currentData() )
            s_hSet[ "codepage" ] := oCP:currentData()
        ENDIF
        s_oEdit:lTabSpaces := s_hSet[ "tabspaces" ]
        FOR EACH oB IN s_aBufs
            oB:nTab := s_hSet[ "tab" ]
            oB:lHilite := s_hSet[ "hilite" ]
        NEXT
        ce_SaveSettings()
    ENDIF

RETURN

/*----------------------------------------------------------- settings */

/* cedit.ini next to the program (portable), else in the user's folder */
STATIC FUNCTION ce_IniPath()

    LOCAL cIni := hb_DirBase() + "cedit.ini", cDir

    IF hb_FileExists( cIni )
        RETURN cIni
    ENDIF
#ifdef __PLATFORM__WINDOWS
    cDir := GetEnv( "APPDATA" )
    cDir := IIF( Empty( cDir ), hb_DirBase(), hb_DirSepAdd( cDir ) + "CEdit" + hb_ps() )
#else
    cDir := hb_DirSepAdd( GetEnv( "HOME" ) ) + ".config/cedit/"
#endif

RETURN cDir + "cedit.ini"

STATIC PROCEDURE ce_LoadSettings()

    LOCAL hIni, h, cKey, a

    s_hSet := { "theme" => "Classic", "colors" => "", "tab" => 4, "tabspaces" => .T., ;
                "codepage" => "SL852", "hilite" => .T., "insert" => .T. }
    s_cIni := ce_IniPath()
    IF ! hb_FileExists( s_cIni )
        RETURN
    ENDIF
    hIni := hb_iniRead( s_cIni, .F., "=", .F. )
    IF ! HB_ISHASH( hIni )
        RETURN
    ENDIF
    h := hb_HGetDef( hIni, "OPTIONS", { => } )
    s_hSet[ "theme" ]     := hb_HGetDef( h, "theme", s_hSet[ "theme" ] )
    s_hSet[ "colors" ]    := hb_HGetDef( h, "colors", "" )
    s_hSet[ "tab" ]       := Max( 1, Min( 16, Val( hb_HGetDef( h, "tab", "4" ) ) ) )
    s_hSet[ "tabspaces" ] := !( hb_HGetDef( h, "tabspaces", "1" ) == "0" )
    s_hSet[ "codepage" ]  := hb_HGetDef( h, "codepage", "SL852" )
    s_hSet[ "hilite" ]    := !( hb_HGetDef( h, "hilite", "1" ) == "0" )
    s_hSet[ "insert" ]    := !( hb_HGetDef( h, "insert", "1" ) == "0" )
    IF AScan( ce_CPList(), {| x | x[ 1 ] == s_hSet[ "codepage" ] } ) < 2
        s_hSet[ "codepage" ] := "SL852"
    ENDIF

    h := hb_HGetDef( hIni, "RECENT", { => } )
    FOR EACH cKey IN hb_HKeys( h )
        a := hb_ATokens( h[ cKey ], "|" )
        IF ! Empty( a[ 1 ] )
            AAdd( s_aRecent, { Val( cKey ), a[ 1 ], IIF( Len( a ) > 1, Val( a[ 2 ] ), 1 ), IIF( Len( a ) > 2, Val( a[ 3 ] ), 1 ) } )
        ENDIF
    NEXT
    ASort( s_aRecent, , , {| x, y | x[ 1 ] < y[ 1 ] } )
    AEval( s_aRecent, {| x, i | s_aRecent[ i ] := { x[ 2 ], x[ 3 ], x[ 4 ] } } )

RETURN

STATIC PROCEDURE ce_SaveSettings()

    LOCAL hIni := { => }, h := { => }, a

    IF s_oEdit != NIL
        s_hSet[ "insert" ] := s_oEdit:lInsert
    ENDIF
    h[ "theme" ]     := s_hSet[ "theme" ]
    h[ "colors" ]    := s_hSet[ "colors" ]
    h[ "tab" ]       := hb_ntos( s_hSet[ "tab" ] )
    h[ "tabspaces" ] := IIF( s_hSet[ "tabspaces" ], "1", "0" )
    h[ "codepage" ]  := s_hSet[ "codepage" ]
    h[ "hilite" ]    := IIF( s_hSet[ "hilite" ], "1", "0" )
    h[ "insert" ]    := IIF( s_hSet[ "insert" ], "1", "0" )
    hIni[ "OPTIONS" ] := h
    h := { => }
    FOR EACH a IN s_aRecent
        h[ hb_ntos( a:__enumIndex() ) ] := a[ 1 ] + "|" + hb_ntos( a[ 2 ] ) + "|" + hb_ntos( a[ 3 ] )
    NEXT
    hIni[ "RECENT" ] := h
    hb_DirBuild( hb_FNameDir( s_cIni ) )
    hb_iniWrite( s_cIni, hIni, "# CEdit settings", NIL, .F. )

RETURN

/*----------------------------------------------------------- help */

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
    "Search", ;
    "  F7           find                  Ctrl-F7    replace", ;
    "  Shift-F7     next                  Alt-F7     previous", ;
    "  Alt-8        the word at the cursor, Alt-3 backwards", ;
    "  Alt-F8       go to line[,column]   Ctrl-B     matching bracket", ;
    "  Alt-B        back to where the cursor was before a jump", ;
    "", ;
    "View", ;
    "  F8           syntax highlighting on / off (View - Language: choose)", ;
    "  Shift-F8     codepage: an unchanged file is read again in it,", ;
    "               a changed one is saved in it", ;
    "  View - Theme, Editor colors, Options (tab size, spaces, codepage)", ;
    "", ;
    "Cursor", ;
    "  Home         first text of the line, again: line start", ;
    "  Ctrl-Left/Right   word        Ctrl-Home / Ctrl-End  start / end of the text", ;
    "  Ctrl-PgUp/PgDn    top / bottom of the screen", ;
    "  Ctrl-Up/Down      scroll a line", ;
    "", ;
    "Files that are not UTF-8 are read and written in CP852 (Options: another", ;
    "codepage). Line ends (CRLF / LF) and a UTF-8 BOM are kept. Recent files", ;
    "reopen at the line where they were closed." }, Chr( 10 ) )

/*----------------------------------------------------------- dialogs */

/* list of items, Enter / double click chooses; returns the number, 0 = Esc */
STATIC FUNCTION ce_ListDialog( cTitle, aItems, nCurrent )

    LOCAL dlg, oList, nW, nH, nResult := 0, c

    nW := Max( 24, ht_strWidth( cTitle ) + 12 )
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

/* one line of input; returns the text, "" = cancelled */
STATIC FUNCTION ce_InputDialog( cTitle, cLabel, cInit )

    LOCAL dlg, oEdit, nW := 50, nH := 6

    dlg := HTDialog():new()
    dlg:setWindowTitle( cTitle )
    dlg:resize( nW, nH )
    dlg:move( Int( ( ht_ScreenMaxCol() + 1 - nW ) / 2 ), Int( ( ht_ScreenMaxRow() + 1 - nH ) / 2 ) )
    HTLabel():new( cLabel, dlg ):setGeometry( 1, 1, 16, 1 )
    oEdit := CEPathEdit():new( dlg )
    oEdit:setGeometry( 18, 1, nW - 21, 1 )
    oEdit:setText( cInit )
    oEdit:bEnter := {|| dlg:accept() }
    dlg:addButtonBar( "OK", "Cancel" )

RETURN IIF( dlg:exec() == HT_DIALOG_ACCEPTED, AllTrim( oEdit:text ), "" )

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

/* input line of a dialog: Enter runs bEnter (not the next field) */
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
