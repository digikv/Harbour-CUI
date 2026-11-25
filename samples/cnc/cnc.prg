/*
 * CNC - a two-panel file manager in the style of Norton Commander,
 * built on Harbour-CUI
 *
 * Copyright (c) 2026 Srđan Dragojlović - dual licensed, see LICENSE
 *
 * The keys follow HbCommander, the file manager of HbEdit (Alexander S.
 * Kresin), and Norton Commander: F3 view, F4 edit, F5 copy, F6 move,
 * F7 folder, F8 delete, F9 menu, F10 quit ... Written from scratch on
 * Harbour-CUI (no HbEdit code); F3 / F4 use the editor of CEdit
 * (../cedit/htcodeedit.prg, cebuffer.prg, cesyntax.prg).
 *
 *   row 0         menu bar (F9, the mouse)
 *   rows 1 ..     the left and the right panel (cncpanel.prg)
 *   next row      the command line (Ctrl-O; Enter runs it in the panel's folder)
 *   last row      1Help 2Menu 3View 4Edit 5Copy 6RenMov 7Mkdir 8Delete 9PullDn 10Quit
 *
 * Settings (folders, sort order, bookmarks, history) are kept in cnc.ini:
 * next to the program if it is there, else in %APPDATA%\CNC (Windows) or
 * ~/.config/cnc (Linux).
 *
 * Net menu (Total Commander): an FTP server in a panel (cncnet.prg, hbtip),
 * network drives and the computers of the network (Windows).
 *
 * Usage: cnc [left folder [right folder]]
 */

#include "hbtui.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"
#include "fileio.ch"
#include "cedit.ch"
#include "cnc.ch"

REQUEST HB_CODEPAGE_UTF8EX
REQUEST HB_CODEPAGE_SL852
REQUEST HB_CODEPAGE_SLWIN

STATIC s_oApp, s_oMain, s_oBar, s_oCmd, s_oPrompt, s_oKeys
STATIC s_aPanel := { NIL, NIL }
STATIC s_nAct := 1                      /* the active panel */
STATIC s_hSet                           /* settings (cnc.ini [OPTIONS]) */
STATIC s_cIni := ""
STATIC s_aHist := {}                    /* folders visited, the newest first */
STATIC s_aHot := {}                     /* the directory hotlist (Ctrl-D) */
STATIC s_cOutput := ""                  /* the output of the last command (Ctrl-W) */
STATIC s_cOutCmd := ""
STATIC s_hFtp := { => }                 /* the FTP connections (cnc.ini [FTP]) */
STATIC s_aCheck := {}                   /* menu entries with a check: { oAction, cText, cKey, bOn } */
STATIC s_aMenuHot := {}                   /* the hot letters taken: { oMenu, cLetters } */

PROCEDURE Main( ... )

    LOCAL aArgs := {}, cArg, i

    hb_cdpSelect( "UTF8EX" )
    cnc_LoadSettings()

    s_oApp := HTApplication():new()
    ht_cuiSetupDisplay( s_oApp:rows(), s_oApp:cols() )
    hb_gtInfo( HB_GTI_WINTITLE, "CNC" )
    hb_gtInfo( HB_GTI_ICONRES, 1 )                 /* cnc.ico (cnc.rc), Windows */
    cnc_NortonTheme()
    IF HTTheme():hasTheme( s_hSet[ "theme" ] )
        HTTheme():loadTheme( s_hSet[ "theme" ] )
    ENDIF

    s_oMain := CNCMain():new()
    s_oMain:setWindowTitle( "CNC" )
    s_oMain:show()

    FOR EACH cArg IN hb_AParams()
        IF !( Left( cArg, 1 ) == "-" )
            AAdd( aArgs, cArg )
        ENDIF
    NEXT

    FOR i := 1 TO 2
        s_aPanel[ i ] := CNCPanel():new( s_oMain )
        s_aPanel[ i ]:nSort := s_hSet[ "sort" + hb_ntos( i ) ]
        s_aPanel[ i ]:lDesc := s_hSet[ "desc" + hb_ntos( i ) ]
        s_aPanel[ i ]:nView := s_hSet[ "view" + hb_ntos( i ) ]
        s_aPanel[ i ]:lHidden := s_hSet[ "hidden" ]
        s_aPanel[ i ]:bEnter := {| o, a | cnc_OpenFile( o, a ) }
        s_aPanel[ i ]:bActivate := {| o | cnc_Activated( o ) }
        s_aPanel[ i ]:bChanged := {| o | cnc_Changed( o ) }
        s_aPanel[ i ]:bCursor := {| o | cnc_CmdCursor( o ) }
    NEXT
    s_oPrompt := HTLabel():new( "", s_oMain )
    s_oCmd := CNCLine():new( s_oMain )
    s_oCmd:bEnter := {|| cnc_RunLine() }
    s_oCmd:bEsc := {|| s_oCmd:setText( "" ), cnc_Activate( s_nAct ) }
    s_oKeys := CNCKeyBar():new( s_oMain )
    s_oKeys:bClick := {| n | cnc_FKey( n ) }

    FOR i := 1 TO 2
        cArg := IIF( Len( aArgs ) >= i, aArgs[ i ], s_hSet[ "dir" + hb_ntos( i ) ] )
        IF Empty( cArg ) .OR. ! s_aPanel[ i ]:read( cnc_FullPath( cArg ) )
            s_aPanel[ i ]:read( hb_cwd() )
        ENDIF
    NEXT
    AEval( s_aPanel, {| o | IIF( o:isTree(), cnc_Follow( o ), NIL ) } )

    cnc_Arrange()
    cnc_BuildMenu()
    cnc_Activate( IIF( s_hSet[ "active" ] == 2, 2, 1 ) )
    s_oApp:exec()

    cnc_SaveSettings()

RETURN

/*----------------------------------------------------------- main window */

CLASS CNCMain FROM HTMainWindow
    METHOD keyEvent( keyEvent )
    METHOD screenResized()
ENDCLASS

/* the keys of the program; the panel gets the rest (cursor, Ins, Enter ...) */
METHOD PROCEDURE keyEvent( keyEvent ) CLASS CNCMain

    LOCAL nKey := keyEvent:key, lCtrl, lPanel, c

    IF s_oBar != NIL .AND. s_oBar:isActive()
        ::super:keyEvent( keyEvent )
        RETURN
    ENDIF
    lPanel := ! s_oCmd:hasFocus()
    lCtrl := hb_bitAnd( hb_gtInfo( HB_GTI_KBDSHIFTS ), HB_GTI_KBD_CTRL ) != 0

    /* Ctrl + a letter: these codes are also cursor keys (Ctrl-R = PgUp ...) */
    IF lCtrl .AND. nKey >= 1 .AND. nKey <= 26
        c := Chr( nKey + 64 )
        IF !( c $ "WBZ" ) .OR. ! ht_keyStateAvailable() .OR. ht_keyDown( Asc( c ) )
            IF cnc_CtrlKey( c )
                keyEvent:accept()
                RETURN
            ENDIF
        ENDIF
    ENDIF

    DO CASE
    CASE nKey == K_F9
        keyEvent:accept()
        s_oBar:popup( IIF( s_nAct == 2, 5, 1 ) )
        RETURN
    CASE nKey == K_F10
        keyEvent:accept()
        cnc_Quit()
        RETURN
    CASE nKey == K_TAB .AND. lPanel
        keyEvent:accept()
        cnc_Activate( 3 - s_nAct )
        RETURN
    CASE nKey == K_CTRL_F3 .OR. nKey == K_CTRL_F4 .OR. nKey == K_CTRL_F5 .OR. nKey == K_CTRL_F6
        keyEvent:accept()
        cnc_Sort( s_nAct, AScan( { K_CTRL_F3, K_CTRL_F4, K_CTRL_F5, K_CTRL_F6 }, nKey ) )
        RETURN
    CASE nKey == K_CTRL_ENTER
        keyEvent:accept()
        cnc_NameToCmd()
        RETURN
    CASE nKey == K_ALT_DOWN
        keyEvent:accept()
        cnc_History()
        RETURN
    CASE lPanel .AND. s_aPanel[ s_nAct ]:cQuick == NIL .AND. cnc_TypeKey( nKey )
        keyEvent:accept()
        RETURN
    CASE lPanel .AND. ! Empty( c := cnc_AltLetter( nKey ) ) .AND. !( c $ "LFCNOR" )
        keyEvent:accept()                       /* Alt + letter: quick search (NC) */
        s_aPanel[ s_nAct ]:quickSearch( Lower( c ) )
        RETURN
    ENDCASE

    ::super:keyEvent( keyEvent )

RETURN

METHOD PROCEDURE screenResized() CLASS CNCMain
    ::super:screenResized()
    cnc_Arrange()
RETURN

/* as in Norton Commander, while a panel has the keys: what is typed goes
   to the command line (the cursor stays there); + - * mark files and =
   opens the calculator while it is empty; Enter runs it, Backspace takes a character back, Esc clears it.
   With an empty command line Enter and Backspace are the panel's. */
STATIC FUNCTION cnc_TypeKey( nKey )

    LOCAL cCmd := s_oCmd:text, c

    DO CASE
    CASE nKey == K_ENTER .AND. ! Empty( cCmd )
        cnc_RunLine()
    CASE nKey == K_BS .AND. ! Empty( cCmd )
        cnc_SetCmd( hb_StrShrink( cCmd, 1 ) )
    CASE nKey == K_ESC .AND. ! Empty( cCmd )
        cnc_SetCmd( "" )
    CASE Empty( cCmd ) .AND. ( nKey == 43 .OR. nKey == 45 )       /* + - */
        cnc_SelectMask( nKey == 43 )
    CASE Empty( cCmd ) .AND. nKey == 42                           /* * */
        s_aPanel[ s_nAct ]:invertMarks()
    CASE Empty( cCmd ) .AND. nKey == 61                           /* = the calculator (HbCommander) */
        cnc_Calc( "" )
    CASE !( ( c := ht_keyText( nKey ) ) == "" )         /* also a space */
        cnc_SetCmd( cCmd + c )
    OTHERWISE
        RETURN .F.
    ENDCASE

RETURN .T.

STATIC PROCEDURE cnc_SetCmd( cText )
    s_oCmd:setText( cText )
    s_oMain:repaintChild( s_oCmd )
    HTApplication():syncCursor()
RETURN

/* the text cursor of an active panel: at the end of the command line */
STATIC FUNCTION cnc_CmdCursor( oPanel )
RETURN { s_oCmd:y - oPanel:y, s_oCmd:x - oPanel:x + Min( ht_strWidth( s_oCmd:text ), Max( 0, s_oCmd:width - 1 ) ) }

STATIC FUNCTION cnc_CtrlKey( c )

    SWITCH c
    CASE "R"
        cnc_Reread()
        EXIT
    CASE "U"
        cnc_Swap()
        EXIT
    CASE "O"
        cnc_Console()
        EXIT
    CASE "W"
        cnc_ShowOutput()
        EXIT
    CASE "S"
        s_aPanel[ s_nAct ]:quickSearch( "" )
        EXIT
    CASE "P"
        cnc_SetMode( s_nAct, s_aPanel[ s_nAct ]:nView % 4 + 1 )    /* full, brief, brief TC, tree */
        EXIT
    CASE "F"                                     /* Ctrl-F FTP connect, Ctrl-Shift-F disconnect */
        IF hb_bitAnd( hb_gtInfo( HB_GTI_KBDSHIFTS ), HB_GTI_KBD_SHIFT ) != 0
            cnc_FtpDisconnect()
        ELSE
            cnc_FtpConnect( .F. )
        ENDIF
        EXIT
    CASE "N"
        cnc_FtpConnect( .T. )
        EXIT
    CASE "D"                                     /* the directory hotlist (Total Commander) */
        cnc_Hotlist()
        EXIT
    OTHERWISE
        RETURN .F.
    ENDSWITCH

RETURN .T.

/* K_ALT_A .. K_ALT_Z -> "A" .. "Z", else "" */
STATIC FUNCTION cnc_AltLetter( nKey )

    LOCAL aAlt := { K_ALT_A, K_ALT_B, K_ALT_C, K_ALT_D, K_ALT_E, K_ALT_F, K_ALT_G, K_ALT_H, K_ALT_I, ;
                    K_ALT_J, K_ALT_K, K_ALT_L, K_ALT_M, K_ALT_N, K_ALT_O, K_ALT_P, K_ALT_Q, K_ALT_R, ;
                    K_ALT_S, K_ALT_T, K_ALT_U, K_ALT_V, K_ALT_W, K_ALT_X, K_ALT_Y, K_ALT_Z }
    LOCAL n := AScan( aAlt, nKey )

RETURN IIF( n > 0, Chr( 64 + n ), "" )

/* the key bar: 1 .. 10 */
STATIC PROCEDURE cnc_FKey( n )
    SWITCH n
    CASE 1 ; cnc_Help() ; EXIT
    CASE 2 ; cnc_UserMenu() ; EXIT
    CASE 3 ; cnc_View() ; EXIT
    CASE 4 ; cnc_Edit( .F. ) ; EXIT
    CASE 5 ; cnc_CopyMove( .F. ) ; EXIT
    CASE 6 ; cnc_CopyMove( .T. ) ; EXIT
    CASE 7 ; cnc_Mkdir() ; EXIT
    CASE 8 ; cnc_Delete() ; EXIT
    CASE 9 ; s_oBar:popup( IIF( s_nAct == 2, 5, 1 ) ) ; EXIT
    CASE 10 ; cnc_Quit() ; EXIT
    ENDSWITCH
RETURN

STATIC PROCEDURE cnc_Arrange()

    LOCAL nW := s_oMain:width, nH := s_oMain:height, nL := Int( nW / 2 ), nP

    s_aPanel[ 1 ]:setGeometry( 0, 0, nL, Max( 7, nH - 3 ) )
    s_aPanel[ 2 ]:setGeometry( nL, 0, nW - nL, Max( 7, nH - 3 ) )
    nP := Min( Int( nW / 2 ), Len( cnc_PromptText() ) )
    s_oPrompt:setGeometry( 0, nH - 3, nP, 1 )
    s_oCmd:setGeometry( nP, nH - 3, nW - nP, 1 )
    s_oKeys:setGeometry( 0, nH - 2, nW, 1 )

RETURN

STATIC FUNCTION cnc_PromptText()

    LOCAL o := s_aPanel[ s_nAct ], c := IIF( o:isNet(), o:oNet:url( o:cDir ), o:cDir ), nMax := Max( 10, Int( s_oMain:width / 2 ) - 1 )

    IF Len( c ) > nMax
        c := "…" + Right( c, nMax - 1 )
    ENDIF

RETURN c + ">"

/*----------------------------------------------------------- menu */

STATIC PROCEDURE cnc_BuildMenu()

    LOCAL oMenu, i

    s_oBar := s_oMain:menuBar()
    s_oBar:setBarRow( 0 )

    FOR i := 1 TO 2
        oMenu := s_oBar:addMenu( IIF( i == 1, "Left", "Right" ) )
        cnc_PanelMenu( oMenu, i )
        IF i == 1
            oMenu := s_oBar:addMenu( "Files" )
            cnc_Item( oMenu, "Help", "F1", K_F1, {|| cnc_Help() } )
            cnc_Item( oMenu, "User menu", "F2", K_F2, {|| cnc_UserMenu() } )
            cnc_Item( oMenu, "View", "F3", K_F3, {|| cnc_View() } )
            cnc_Item( oMenu, "Edit", "F4", K_F4, {|| cnc_Edit( .F. ) } )
            cnc_Item( oMenu, "Edit new file", "Shift-F4", K_SH_F4, {|| cnc_Edit( .T. ) } )
            cnc_Item( oMenu, "Copy", "F5", K_F5, {|| cnc_CopyMove( .F. ) } )
            cnc_Item( oMenu, "Rename or move", "F6", K_F6, {|| cnc_CopyMove( .T. ) } )
            cnc_Item( oMenu, "Rename", "Shift-F6", K_SH_F6, {|| cnc_Rename() } )
            cnc_Item( oMenu, "Make directory", "F7", K_F7, {|| cnc_Mkdir() } )
            cnc_Item( oMenu, "Delete", "F8", K_F8, {|| cnc_Delete() } )
            oMenu:addSeparator()
            cnc_Item( oMenu, "Select group", "+", NIL, {|| cnc_SelectMask( .T. ) } )
            cnc_Item( oMenu, "Unselect group", "-", NIL, {|| cnc_SelectMask( .F. ) } )
            cnc_Item( oMenu, "Invert selection", "*", NIL, {|| s_aPanel[ s_nAct ]:invertMarks() } )
            oMenu:addSeparator()
            cnc_Item( oMenu, "Copy name", "Ctrl-Ins", K_CTRL_INS, {|| cnc_CopyName() } )
            cnc_Item( oMenu, "Name to command line", "Ctrl-Enter", NIL, {|| cnc_NameToCmd() } )
            cnc_Item( oMenu, "Quit", "F10", NIL, {|| cnc_Quit() } )

            oMenu := s_oBar:addMenu( "Commands" )
            cnc_Item( oMenu, "Find file", "Ctrl-F7", K_CTRL_F7, {|| cnc_Find() } )
            cnc_Item( oMenu, "Directory history", "Alt-Down", NIL, {|| cnc_History() } )
            cnc_Item( oMenu, "Directory hotlist", "Ctrl-D", NIL, {|| cnc_Hotlist() } )
            cnc_Item( oMenu, "Change directory", "cd ...", NIL, {|| cnc_ChDir( s_nAct ) } )
            oMenu:addSeparator()
            cnc_Item( oMenu, "Swap panels", "Ctrl-U", NIL, {|| cnc_Swap() } )
            cnc_Item( oMenu, "Compare directories", "", NIL, {|| cnc_Compare() } )
            cnc_Item( oMenu, "Equal panels", "", NIL, {|| cnc_Equal() } )
            oMenu:addSeparator()
            cnc_Item( oMenu, "Command line", "Ctrl-O", NIL, {|| cnc_Console() } )
            cnc_Item( oMenu, "Last output", "Ctrl-W", NIL, {|| cnc_ShowOutput() } )
            cnc_Item( oMenu, "Calculator", "=", NIL, {|| cnc_Calc( "" ) } )

            oMenu := s_oBar:addMenu( "Net" )
            cnc_Item( oMenu, "FTP connect...", "Ctrl-F", NIL, {|| cnc_FtpConnect( .F. ) } )
            cnc_Item( oMenu, "FTP new connection...", "Ctrl-N", NIL, {|| cnc_FtpConnect( .T. ) } )
            cnc_Item( oMenu, "FTP disconnect", "Ctrl-Shift-F", NIL, {|| cnc_FtpDisconnect() } )
            oMenu:addSeparator()
            cnc_Item( oMenu, "Map network drive...", "", NIL, {|| cnc_NetCmd( 1 ) } )
            cnc_Item( oMenu, "Disconnect network drive...", "", NIL, {|| cnc_NetCmd( 2 ) } )
            cnc_Item( oMenu, "Network computers...", "", NIL, {|| cnc_NetCmd( 3 ) } )

            oMenu := s_oBar:addMenu( "Options" )
            cnc_Item( oMenu, "Show hidden files", "", NIL, {|| cnc_Hidden() }, {|| s_hSet[ "hidden" ] } )
            cnc_Item( oMenu, "Show command output", "", NIL, {|| cnc_OutputOpt() }, {|| s_hSet[ "output" ] } )
            cnc_Item( oMenu, "Theme...", "", NIL, {|| cnc_Theme() } )
            cnc_Item( oMenu, "About", "", NIL, {|| cnc_About() } )
        ENDIF
    NEXT

RETURN

STATIC PROCEDURE cnc_PanelMenu( oMenu, n )

    LOCAL cDrv := IIF( n == 1, "Alt-F1", "Alt-F2" ), nDrv := IIF( n == 1, K_ALT_F1, K_ALT_F2 )
    LOCAL nCD := IIF( n == 1, K_CTRL_F1, K_CTRL_F2 )

    cnc_Item( oMenu, "Brief", "", NIL, {|| cnc_SetMode( n, CNC_VIEW_BRIEF ) }, {|| s_aPanel[ n ]:nView == CNC_VIEW_BRIEF } )
    cnc_Item( oMenu, "Brief (Total Commander)", "", NIL, {|| cnc_SetMode( n, CNC_VIEW_TC ) }, {|| s_aPanel[ n ]:nView == CNC_VIEW_TC } )
    cnc_Item( oMenu, "Full", "", NIL, {|| cnc_SetMode( n, CNC_VIEW_FULL ) }, {|| s_aPanel[ n ]:nView == CNC_VIEW_FULL } )
    cnc_Item( oMenu, "Tree", "", NIL, {|| cnc_SetMode( n, CNC_VIEW_TREE ) }, {|| s_aPanel[ n ]:nView == CNC_VIEW_TREE } )
    oMenu:addSeparator()
    cnc_Item( oMenu, "Name", "Ctrl-F3", NIL, {|| cnc_Sort( n, CNC_SORT_NAME ) } )
    cnc_Item( oMenu, "Extension", "Ctrl-F4", NIL, {|| cnc_Sort( n, CNC_SORT_EXT ) } )
    cnc_Item( oMenu, "Time", "Ctrl-F5", NIL, {|| cnc_Sort( n, CNC_SORT_TIME ) } )
    cnc_Item( oMenu, "Size", "Ctrl-F6", NIL, {|| cnc_Sort( n, CNC_SORT_SIZE ) } )
    oMenu:addSeparator()
    cnc_Item( oMenu, "Re-read", "Ctrl-R", NIL, {|| s_aPanel[ n ]:reread(), s_aPanel[ n ]:refresh() } )
    cnc_Item( oMenu, "Change directory", IIF( n == 1, "Ctrl-F1", "Ctrl-F2" ), nCD, {|| cnc_ChDir( n ) } )
    cnc_Item( oMenu, "Drive", cDrv, nDrv, {|| cnc_Drive( n ) } )

RETURN

/* menu entry "√ Text    Key"; nKey: the key runs it from anywhere; bOn: the
   entry shows √ while it returns .T. (cnc_Checks()) */
STATIC PROCEDURE cnc_Item( oMenu, cText, cKey, nKey, bAction, bOn )

    LOCAL oAct

    cText := cnc_Hot( oMenu, cText )
    oAct := oMenu:addAction( cnc_ItemText( cText, cKey, bOn ) )

    IF nKey != NIL
        oAct:setShortcut( nKey )
    ENDIF
    oAct:onTriggered := bAction
    IF bOn != NIL
        AAdd( s_aCheck, { oAct, cText, cKey, bOn } )
    ENDIF

RETURN

STATIC FUNCTION cnc_ItemText( cText, cKey, bOn )
RETURN IIF( bOn != NIL .AND. Eval( bOn ), ht_glyph( 0x221A, "*" ), " " ) + " " + ;
       PadR( cText, IIF( "&" $ cText, 24, 23 ) ) + PadL( cKey, 11 )      /* "&" is not shown */

/* a hot letter for a menu entry that is not taken in its menu yet: the first
   letter of a word, else any letter ("Copy" -> "&Copy", then "Change dir" ->
   "C&hange dir"); HTMenu runs an entry by the letter after its "&" */
STATIC FUNCTION cnc_Hot( oMenu, cText )

    LOCAL n, i, c, nPass

    IF "&" $ cText
        RETURN cText
    ENDIF
    IF ( n := AScan( s_aMenuHot, {| a | a[ 1 ] == oMenu } ) ) == 0
        AAdd( s_aMenuHot, { oMenu, "" } )
        n := Len( s_aMenuHot )
    ENDIF
    FOR nPass := 1 TO 2
        FOR i := 1 TO Len( cText )
            c := Upper( SubStr( cText, i, 1 ) )
            IF ( ( c >= "A" .AND. c <= "Z" ) .OR. IsDigit( c ) ) .AND. ! c $ s_aMenuHot[ n ][ 2 ] .AND. ;
               ( nPass == 2 .OR. i == 1 .OR. SubStr( cText, i - 1, 1 ) == " " )
                s_aMenuHot[ n ][ 2 ] += c
                RETURN Left( cText, i - 1 ) + "&" + SubStr( cText, i )
            ENDIF
        NEXT
    NEXT

RETURN cText

/* the √ of the menu entries again, after a setting changed */
STATIC PROCEDURE cnc_Checks()

    LOCAL a

    FOR EACH a IN s_aCheck
        a[ 1 ]:setText( cnc_ItemText( a[ 2 ], a[ 3 ], a[ 4 ] ) )
    NEXT

RETURN

/*----------------------------------------------------------- panels */

STATIC FUNCTION cnc_Act()
RETURN s_aPanel[ s_nAct ]

STATIC PROCEDURE cnc_Activate( n )

    LOCAL oOld := s_aPanel[ s_nAct ], lCmd := s_oCmd:hasFocus()

    s_nAct := n
    s_oMain:setFocusChild( s_aPanel[ n ] )
    IF lCmd
        s_oCmd:focusOutEvent( NIL )
    ENDIF
    IF ! oOld == s_aPanel[ n ]
        oOld:refresh()
    ENDIF
    s_aPanel[ n ]:refresh()
    cnc_Changed( s_aPanel[ n ] )

RETURN

/* a panel took the focus (a mouse click) */
STATIC PROCEDURE cnc_Activated( oPanel )

    LOCAL n := IIF( oPanel == s_aPanel[ 2 ], 2, 1 )

    IF n != s_nAct
        s_nAct := n
        s_aPanel[ 3 - n ]:refresh()
        cnc_Changed( oPanel )
    ENDIF

RETURN

/* another folder: history, the prompt of the command line, the process' folder */
STATIC PROCEDURE cnc_Changed( oPanel )

    LOCAL n

    IF oPanel:isTree()
        cnc_Follow( oPanel )
    ENDIF
    IF oPanel == s_aPanel[ s_nAct ] .AND. ! Empty( oPanel:cDir )
        IF ! oPanel:isNet()
            hb_cwd( oPanel:cDir )
            n := AScan( s_aHist, {| c | cnc_SamePath( c, oPanel:cDir ) } )
            IF n > 0
                hb_ADel( s_aHist, n, .T. )
            ENDIF
            hb_AIns( s_aHist, 1, oPanel:cDir, .T. )
            ASize( s_aHist, Min( Len( s_aHist ), 30 ) )
        ENDIF
        IF s_oPrompt != NIL .AND. s_oMain != NIL .AND. s_oMain:width > 0
            s_oPrompt:setText( cnc_PromptText() )
            cnc_Arrange()
            s_oMain:repaintChild( s_oPrompt )
            s_oMain:repaintChild( s_oCmd )
        ENDIF
    ENDIF

RETURN

STATIC PROCEDURE cnc_Reread()
    LOCAL o
    FOR EACH o IN s_aPanel
        o:reread()
        o:refresh()
    NEXT
RETURN

STATIC PROCEDURE cnc_Swap()

    LOCAL o1 := s_aPanel[ 1 ], o2 := s_aPanel[ 2 ], c, x

    FOR EACH c IN { "oNet", "cLocal", "cDir", "aFiles", "nCur", "nTop", "nSort", "lDesc", "nView", "nLongest", "aRoot", "aTree" }
        x := __objSendMsg( o1, c )
        __objSendMsg( o1, "_" + c, __objSendMsg( o2, c ) )
        __objSendMsg( o2, "_" + c, x )
    NEXT
    o1:ensureVisible()
    o2:ensureVisible()
    AEval( s_aPanel, {| o | o:refresh() } )
    cnc_Checks()
    cnc_Changed( cnc_Act() )

RETURN

/* Commands - Equal panels: the other panel shows the folder of this one */
STATIC PROCEDURE cnc_Equal()

    LOCAL o := cnc_Act(), oOther := s_aPanel[ 3 - s_nAct ]

    IF o:isNet()
        HTToast():show( "Not for an FTP panel", 1500, HT_CLR_TOAST_INFO )
        RETURN
    ENDIF
    cnc_FtpClose( 3 - s_nAct )
    oOther:read( o:cDir )
    oOther:refresh()

RETURN

/* Ctrl-F3 .. Ctrl-F6, the menu, a click on a column title: the same
   order again turns it round */
STATIC PROCEDURE cnc_Sort( n, nSort )
    s_aPanel[ n ]:sortBy( nSort )
RETURN

STATIC PROCEDURE cnc_SetMode( n, nView )

    LOCAL o := s_aPanel[ n ], lTree := o:isTree()

    IF nView == CNC_VIEW_TREE .AND. o:isNet()
        IF o:nView == CNC_VIEW_TC                 /* Ctrl-P goes round without the tree */
            nView := CNC_VIEW_FULL
        ELSE
            HTToast():show( "No tree for an FTP panel", 1500, HT_CLR_TOAST_INFO )
            RETURN
        ENDIF
    ENDIF
    o:nView := nView
    IF o:isTree() != lTree                       /* into the tree or out of it: read again */
        o:read( o:cDir )
        IF o:isTree()
            cnc_Follow( o )
        ENDIF
    ENDIF
    o:ensureVisible()
    o:refresh()
    cnc_Checks()

RETURN

/* the other panel shows the folder on the cursor of a tree panel (Norton Commander) */
STATIC PROCEDURE cnc_Follow( oTree )

    LOCAL n := AScan( s_aPanel, {| o | o == oTree } ), oOther

    IF n == 0 .OR. Empty( oTree:cDir )
        RETURN
    ENDIF
    oOther := s_aPanel[ 3 - n ]
    IF oOther == NIL .OR. oOther:isTree() .OR. oOther:isNet() .OR. cnc_SamePath( oOther:cDir, oTree:cDir )
        RETURN
    ENDIF
    oOther:read( oTree:cDir )
    oOther:refresh()

RETURN

/* Options - Show command output: the output screen after a command, or only
   a short message (Ctrl-W shows the output) */
STATIC PROCEDURE cnc_OutputOpt()
    s_hSet[ "output" ] := ! s_hSet[ "output" ]
    cnc_Checks()
    HTToast():show( IIF( s_hSet[ "output" ], "The output is shown after a command", ;
                         "After a command only its exit code; Ctrl-W shows the output" ), 2500, HT_CLR_TOAST_INFO )
RETURN

STATIC PROCEDURE cnc_Hidden()
    s_hSet[ "hidden" ] := ! s_hSet[ "hidden" ]
    cnc_Checks()
    AEval( s_aPanel, {| o | o:lHidden := s_hSet[ "hidden" ], o:reread(), o:refresh() } )
    HTToast():show( IIF( s_hSet[ "hidden" ], "Hidden files are shown", "Hidden files are not shown" ), 1500, HT_CLR_TOAST_INFO )
RETURN

/* D, Ctrl-F1 / Ctrl-F2: type a folder */
STATIC PROCEDURE cnc_ChDir( n )

    LOCAL o := s_aPanel[ n ], cDir := cnc_Input( "Change directory", IIF( o:isNet(), "FTP folder:", "Directory:" ), o:cDir )

    IF Empty( cDir )
        RETURN
    ENDIF
    IF o:isNet()
        IF ! o:read( IIF( Left( cDir, 1 ) == "/", cDir, o:cDir + cDir ) )
            HTMessageBox():error( "FTP", o:oNet:cErr )
        ENDIF
        o:refresh()
        RETURN
    ENDIF
    cnc_GoDir( n, cnc_FullPath( cDir ) )

RETURN

/* a local folder; an FTP panel is disconnected first */
STATIC PROCEDURE cnc_GoDir( n, cDir )
    cnc_FtpClose( n )
    IF s_aPanel[ n ]:read( cDir )
        s_aPanel[ n ]:refresh()
        IF n != s_nAct
            cnc_Activate( n )
        ENDIF
    ELSE
        HTMessageBox():error( "CNC", "The folder does not exist:" + Chr( 10 ) + cDir )
    ENDIF
RETURN

/* Alt-F1 / Alt-F2: the drives (Windows) or the usual places (Linux) */
STATIC PROCEDURE cnc_Drive( n )

    LOCAL aDirs := {}, c, nCur
#ifndef __PLATFORM__WINDOWS
    LOCAL a
#endif

#ifdef __PLATFORM__WINDOWS
    FOR EACH c IN "CDEFGHIJKLMNOPQRSTUVWXYZ"
        IF hb_DirExists( c + ":\" )
            AAdd( aDirs, c + ":\" )
        ENDIF
    NEXT
#else
    AAdd( aDirs, "/" )
    IF ! Empty( GetEnv( "HOME" ) )
        AAdd( aDirs, hb_DirSepAdd( GetEnv( "HOME" ) ) )
    ENDIF
    FOR EACH c IN { "/mnt/", "/media/" + GetEnv( "USER" ) + "/", "/media/" }
        FOR EACH a IN Directory( c + "*", "D" )
            IF "D" $ a[ 5 ] .AND. !( a[ 1 ] == "." .OR. a[ 1 ] == ".." )
                AAdd( aDirs, c + a[ 1 ] + "/" )
            ENDIF
        NEXT
    NEXT
#endif
    nCur := Max( 1, AScan( aDirs, {| x | Lower( Left( s_aPanel[ n ]:cDir, Len( x ) ) ) == Lower( x ) } ) )
    nCur := cnc_List( IIF( n == 1, "Left", "Right" ) + " panel: drive", aDirs, nCur )
    IF nCur > 0
        cnc_GoDir( n, aDirs[ nCur ] )
    ENDIF

RETURN

/* H: the folders visited */
STATIC PROCEDURE cnc_History()
    LOCAL n
    IF Empty( s_aHist )
        RETURN
    ENDIF
    n := cnc_List( "Directory history", s_aHist, 1 )
    IF n > 0
        cnc_GoDir( s_nAct, s_aHist[ n ] )
    ENDIF
RETURN

/* Ctrl-D: the directory hotlist (as in Total Commander): a folder of the
   list, add the folder of the active panel, or remove one */
STATIC PROCEDURE cnc_Hotlist()

    LOCAL aItems := AClone( s_aHot ), n, cDir := cnc_Act():cDir

    AAdd( aItems, "<Add " + cDir + ">" )
    IF ! Empty( s_aHot )
        AAdd( aItems, "<Remove a folder...>" )
    ENDIF
    n := cnc_List( "Directory hotlist", aItems, 1 )
    DO CASE
    CASE n == 0
    CASE n <= Len( s_aHot )
        cnc_GoDir( s_nAct, s_aHot[ n ] )
    CASE n == Len( s_aHot ) + 1
        IF cnc_Act():isNet()
            HTToast():show( "Not for an FTP folder (Ctrl-F keeps the connections)", 2000, HT_CLR_TOAST_INFO )
        ELSEIF AScan( s_aHot, {| c | cnc_SamePath( c, cDir ) } ) == 0
            AAdd( s_aHot, cDir )
        ENDIF
    OTHERWISE
        n := cnc_List( "Remove from the hotlist", s_aHot, 1 )
        IF n > 0
            hb_ADel( s_aHot, n, .T. )
        ENDIF
    ENDCASE

RETURN

/* + / -: mark or unmark by a mask */
STATIC PROCEDURE cnc_SelectMask( lOn )

    LOCAL cMask := cnc_Input( IIF( lOn, "Select", "Unselect" ), "Files:", "*" )

    IF ! Empty( cMask )
        cnc_Act():markMask( cMask, lOn )
    ENDIF

RETURN

/* Commands - Compare directories: marks the files that are only in one
   panel, or newer than in the other one */
STATIC PROCEDURE cnc_Compare()

    LOCAL o1 := s_aPanel[ 1 ], o2 := s_aPanel[ 2 ], a, n, nMarked := 0

    AEval( o1:aFiles, {| x | x[ F_MARK ] := .F. } )
    AEval( o2:aFiles, {| x | x[ F_MARK ] := .F. } )
    FOR EACH a IN o1:aFiles
        IF ! a[ F_DIR ]
            n := AScan( o2:aFiles, {| x | ! x[ F_DIR ] .AND. Lower( x[ F_NAME ] ) == Lower( a[ F_NAME ] ) } )
            IF n == 0 .OR. cnc_Newer( a, o2:aFiles[ n ] ) .OR. ( a[ F_SIZE ] != o2:aFiles[ n ][ F_SIZE ] .AND. ! cnc_Newer( o2:aFiles[ n ], a ) )
                a[ F_MARK ] := .T.
                nMarked++
            ENDIF
            IF n > 0 .AND. cnc_Newer( o2:aFiles[ n ], a )
                o2:aFiles[ n ][ F_MARK ] := .T.
                nMarked++
            ENDIF
        ENDIF
    NEXT
    FOR EACH a IN o2:aFiles
        IF ! a[ F_DIR ] .AND. AScan( o1:aFiles, {| x | ! x[ F_DIR ] .AND. Lower( x[ F_NAME ] ) == Lower( a[ F_NAME ] ) } ) == 0
            a[ F_MARK ] := .T.
            nMarked++
        ENDIF
    NEXT
    o1:refresh()
    o2:refresh()
    HTToast():show( IIF( nMarked == 0, "The folders have the same files", hb_ntos( nMarked ) + " different file(s) marked" ), 2000, HT_CLR_TOAST_INFO )

RETURN

STATIC FUNCTION cnc_Newer( a, b )
RETURN DToS( a[ F_DATE ] ) + a[ F_TIME ] > DToS( b[ F_DATE ] ) + b[ F_TIME ]

/*----------------------------------------------------------- files */

STATIC FUNCTION cnc_Path( oPanel, a )
RETURN oPanel:cDir + a[ F_NAME ]

/* Enter on a file: a program runs, a document opens in its program */
STATIC PROCEDURE cnc_OpenFile( oPanel, a )

    LOCAL cFile := cnc_Path( oPanel, a ), cExt := Lower( hb_FNameExt( a[ F_NAME ] ) ), cBro
#ifndef __PLATFORM__WINDOWS
    LOCAL nAttr := 0
#endif

    IF oPanel:isNet()
        cnc_View()
        RETURN
    ENDIF
    IF cExt == ".dbf" .AND. ! Empty( cBro := cnc_CBroPath() )     /* a table: CBro, in its own window */
#ifdef __PLATFORM__WINDOWS
        wapi_ShellExecute( NIL, "open", cBro, '"' + cFile + '"', oPanel:cDir, 1 )
#else
        hb_processOpen( "'" + cBro + "' '" + cFile + "'", , , , .T. )
#endif
        RETURN
    ENDIF
#ifdef __PLATFORM__WINDOWS
    IF cExt $ ".bat|.cmd"
        cnc_Run( '"' + cFile + '"' )
    ELSE
        wapi_ShellExecute( NIL, "open", cFile, NIL, oPanel:cDir, 1 )
    ENDIF
#else
    IF ( hb_vfAttrGet( cFile, @nAttr ) .AND. hb_bitAnd( nAttr, HB_FA_XUSR ) != 0 ) .OR. cExt == ".sh"
        cnc_Run( "'" + cFile + "'" )
    ELSE
        hb_processOpen( "xdg-open '" + cFile + "'", , , , .T. )
    ENDIF
#endif

RETURN

/* CBro (the table browser of Harbour-CUI, samples/cbro): cbro= in cnc.ini,
   else next to CNC, else ../cbro/ of the samples; "" = not there (Enter on a
   .dbf then opens it in the program the system has for it) */
STATIC FUNCTION cnc_CBroPath()

    LOCAL cExe := "cbro" + IIF( hb_ps() == "\", ".exe", "" ), c

    FOR EACH c IN { s_hSet[ "cbro" ], hb_DirBase() + cExe, hb_DirBase() + ".." + hb_ps() + "cbro" + hb_ps() + cExe }
        IF ! Empty( c ) .AND. hb_FileExists( c )
            RETURN c
        ENDIF
    NEXT

RETURN ""

/* F3: view a file (the editor of CEdit, read only); on a folder: its size */
STATIC PROCEDURE cnc_View()

    LOCAL o := cnc_Act(), a := o:current(), aSize

    IF a == NIL .OR. a[ F_NAME ] == ".."
        RETURN
    ENDIF
    IF o:isNet()
        cnc_NetEdit( o, a, .T. )
        RETURN
    ENDIF
    IF a[ F_DIR ]
        aSize := cnc_DirSize( cnc_Path( o, a ) + hb_ps() )
        HTMessageBox():information( "CNC", a[ F_NAME ] + Chr( 10 ) + Chr( 10 ) + ;
            cnc_Thousands( aSize[ 1 ] ) + " bytes in " + hb_ntos( aSize[ 2 ] ) + " files, " + ;
            hb_ntos( aSize[ 3 ] ) + " folders" )
        RETURN
    ENDIF
    cnc_EditFile( cnc_Path( o, a ), .T. )

RETURN

/* { nBytes, nFiles, nFolders } of a folder and its subfolders */
STATIC FUNCTION cnc_DirSize( cDir )

    LOCAL aRes := { 0, 0, 0 }, a, aSub

    FOR EACH a IN Directory( cDir + hb_osFileMask(), "DHS" )
        IF a[ 1 ] == "." .OR. a[ 1 ] == ".."
            LOOP
        ENDIF
        IF "D" $ a[ 5 ]
            aSub := cnc_DirSize( cDir + a[ 1 ] + hb_ps() )
            aRes[ 1 ] += aSub[ 1 ]
            aRes[ 2 ] += aSub[ 2 ]
            aRes[ 3 ] += aSub[ 3 ] + 1
        ELSE
            aRes[ 1 ] += a[ 2 ]
            aRes[ 2 ]++
        ENDIF
    NEXT

RETURN aRes

/* F4: edit the current file; Shift-F4: a new one */
STATIC PROCEDURE cnc_Edit( lNew )

    LOCAL o := cnc_Act(), a := o:current(), cName

    IF o:isNet()
        IF lNew
            cName := cnc_Input( "Edit new file", "File name:", "" )
            IF ! Empty( cName )
                cnc_NetEdit( o, { cName, 0, CToD( "" ), "", "", .F., .F. }, .F. )
            ENDIF
        ELSEIF a != NIL .AND. ! a[ F_DIR ]
            cnc_NetEdit( o, a, .F. )
        ENDIF
        RETURN
    ENDIF
    IF lNew
        cName := cnc_Input( "Edit new file", "File name:", "" )
        IF Empty( cName )
            RETURN
        ENDIF
        cnc_EditFile( cnc_FullPath( cName ), .F. )
        o:read( o:cDir, hb_FNameNameExt( cName ) )
        o:refresh()
        RETURN
    ENDIF
    IF a == NIL .OR. a[ F_DIR ]
        RETURN
    ENDIF
    cnc_EditFile( cnc_Path( o, a ), .F. )
    o:reread()
    o:refresh()

RETURN

/* F5 copy / F6 move: the marked files (or the current one) to the other
   panel's folder, or to the path typed */
STATIC PROCEDURE cnc_CopyMove( lMove )

    LOCAL oSrc := cnc_Act(), oDst := s_aPanel[ 3 - s_nAct ], aList := oSrc:marked(), cTo, cWhat, a
    LOCAL nMode := 0, nDone := 0, cDst, lToDir

    IF Empty( aList )
        RETURN
    ENDIF
    IF oSrc:isNet() .OR. oDst:isNet()
        cnc_NetCopy( oSrc, oDst, aList, lMove )
        RETURN
    ENDIF
    cWhat := IIF( Len( aList ) == 1, '"' + aList[ 1 ][ F_NAME ] + '"', hb_ntos( Len( aList ) ) + " files" )
    cTo := cnc_Input( IIF( lMove, "Rename / move", "Copy" ), IIF( lMove, "Move ", "Copy " ) + cWhat + " to:", oDst:cDir )
    IF Empty( cTo )
        RETURN
    ENDIF
    cTo := cnc_FullPath( cTo )
    /* one file to a name that is not a folder: copy / rename to that name */
    lToDir := hb_DirExists( cTo ) .OR. Right( cTo, 1 ) $ "\/" .OR. Len( aList ) > 1
    IF lToDir
        cTo := hb_DirSepAdd( cTo )
        hb_DirBuild( cTo )
    ENDIF
    FOR EACH a IN aList
        cDst := IIF( lToDir, cTo + a[ F_NAME ], cTo )
        cnc_Progress( IIF( lMove, "Moving ", "Copying " ) + a[ F_NAME ] )
        IF ! cnc_CopyItem( cnc_Path( oSrc, a ), cDst, a[ F_DIR ], lMove, @nMode )
            EXIT
        ENDIF
        a[ F_MARK ] := .F.
        nDone++
    NEXT
    cnc_Progress( "" )
    oSrc:reread()
    oDst:reread()
    AEval( s_aPanel, {| o | o:refresh() } )
    IF nDone > 0
        HTToast():show( hb_ntos( nDone ) + IIF( lMove, " moved", " copied" ), 1500, HT_CLR_TOAST_SUCCESS )
    ENDIF

RETURN

/* copies / moves one file or folder; nMode: 0 ask, 1 overwrite all, 2 skip
   all. .F. = cancelled */
STATIC FUNCTION cnc_CopyItem( cSrc, cDst, lDir, lMove, nMode )

    LOCAL a, n

    IF cnc_SamePath( cSrc, cDst )
        RETURN .T.
    ENDIF
    IF lDir
        IF cnc_SamePath( Left( hb_DirSepAdd( cDst ), Len( hb_DirSepAdd( cSrc ) ) ), hb_DirSepAdd( cSrc ) )
            HTMessageBox():error( "CNC", "Cannot copy a folder into itself:" + Chr( 10 ) + cSrc )
            RETURN .F.
        ENDIF
        IF lMove .AND. ! hb_DirExists( cDst ) .AND. FRename( cSrc, cDst ) == 0
            RETURN .T.                           /* the same disk: a rename */
        ENDIF
        hb_DirBuild( cDst )
        FOR EACH a IN Directory( hb_DirSepAdd( cSrc ) + hb_osFileMask(), "DHS" )
            IF !( a[ 1 ] == "." .OR. a[ 1 ] == ".." )
                IF ! cnc_CopyItem( hb_DirSepAdd( cSrc ) + a[ 1 ], hb_DirSepAdd( cDst ) + a[ 1 ], "D" $ a[ 5 ], lMove, @nMode )
                    RETURN .F.
                ENDIF
            ENDIF
        NEXT
        IF lMove
            hb_DirDelete( cSrc )
        ENDIF
        RETURN .T.
    ENDIF

    IF hb_FileExists( cDst )
        IF nMode == 2
            RETURN .T.
        ELSEIF nMode == 0
            n := HTMessageBox():choose( "CNC", hb_FNameNameExt( cDst ) + " already exists. Overwrite it?", ;
                                        { "&Overwrite", "&All", "&Skip", "Skip a&ll", "&Cancel" }, HT_MSG_WARNING )
            DO CASE
            CASE n == 2
                nMode := 1
            CASE n == 3
                RETURN .T.
            CASE n == 4
                nMode := 2
                RETURN .T.
            CASE n != 1
                RETURN .F.
            ENDCASE
        ENDIF
        FErase( cDst )
    ENDIF
    IF lMove .AND. FRename( cSrc, cDst ) == 0
        RETURN .T.
    ENDIF
    IF hb_vfCopyFileEx( cSrc, cDst ) != 0
        n := HTMessageBox():choose( "CNC", "Cannot copy " + hb_FNameNameExt( cSrc ) + " (error " + hb_ntos( FError() ) + ")", ;
                                    { "&Skip", "&Cancel" }, HT_MSG_WARNING )
        RETURN n == 1
    ENDIF
    IF lMove
        FErase( cSrc )
    ENDIF

RETURN .T.

/* Shift-F6: rename the current file */
STATIC PROCEDURE cnc_Rename()

    LOCAL o := cnc_Act(), a := o:current(), cNew

    IF a == NIL .OR. a[ F_NAME ] == ".."
        RETURN
    ENDIF
    cNew := cnc_Input( "Rename", "New name:", a[ F_NAME ] )
    IF Empty( cNew ) .OR. cNew == a[ F_NAME ]
        RETURN
    ENDIF
    IF o:isNet()
        IF ! o:oNet:rename( o:cDir + a[ F_NAME ], o:cDir + cNew )
            HTMessageBox():error( "FTP", o:oNet:cErr )
        ENDIF
        o:read( o:cDir, cNew )
        o:refresh()
        RETURN
    ENDIF
    IF FRename( cnc_Path( o, a ), o:cDir + cNew ) != 0
        HTMessageBox():error( "CNC", "Cannot rename " + a[ F_NAME ] + " (error " + hb_ntos( FError() ) + ")" )
        RETURN
    ENDIF
    o:read( o:cDir, cNew )
    o:refresh()

RETURN

/* F7: a new folder */
STATIC PROCEDURE cnc_Mkdir()

    LOCAL o := cnc_Act(), cName := cnc_Input( "Make directory", "Name:", "" )

    IF Empty( cName )
        RETURN
    ENDIF
    IF o:isNet()
        IF ! o:oNet:mkdir( o:cDir + cName )
            HTMessageBox():error( "FTP", o:oNet:cErr )
        ENDIF
        o:read( o:cDir, cName )
        o:refresh()
        RETURN
    ENDIF
    IF ! hb_DirBuild( hb_DirSepToOS( cnc_FullPath( cName ) ) )
        HTMessageBox():error( "CNC", "Cannot make the folder " + cName )
        RETURN
    ENDIF
    o:read( o:cDir, hb_FNameNameExt( hb_DirSepDel( hb_DirSepToOS( cName ) ) ) )
    o:refresh()

RETURN

/* F8: delete the marked files (or the current one) */
STATIC PROCEDURE cnc_Delete()

    LOCAL o := cnc_Act(), aList := o:marked(), a, nMode := 0, nDone := 0, cWhat

    IF Empty( aList )
        RETURN
    ENDIF
    cWhat := IIF( Len( aList ) == 1, '"' + aList[ 1 ][ F_NAME ] + '"', hb_ntos( Len( aList ) ) + " files" )
    IF HTMessageBox():choose( "Delete", "Do you wish to delete " + cWhat + "?", { "&Delete", "&Cancel" }, HT_MSG_WARNING ) != 1
        RETURN
    ENDIF
    FOR EACH a IN aList
        cnc_Progress( "Deleting " + a[ F_NAME ] )
        IF o:isNet()
            IF ! cnc_NetDelete( o:oNet, o:cDir + a[ F_NAME ], a[ F_DIR ] )
                EXIT
            ENDIF
        ELSEIF ! cnc_DeleteItem( cnc_Path( o, a ), a[ F_DIR ], @nMode )
            EXIT
        ENDIF
        nDone++
    NEXT
    cnc_Progress( "" )
    o:reread()
    o:refresh()
    s_aPanel[ 3 - s_nAct ]:reread()
    s_aPanel[ 3 - s_nAct ]:refresh()
    IF nDone > 0
        HTToast():show( hb_ntos( nDone ) + " deleted", 1500, HT_CLR_TOAST_SUCCESS )
    ENDIF

RETURN

/* nMode: 0 ask for a folder that is not empty, 1 delete all. .F. = cancelled */
STATIC FUNCTION cnc_DeleteItem( cPath, lDir, nMode )

    LOCAL a, aDir, n

    IF lDir
        aDir := Directory( hb_DirSepAdd( cPath ) + hb_osFileMask(), "DHS" )
        n := 0
        AEval( aDir, {| x | IIF( x[ 1 ] == "." .OR. x[ 1 ] == "..", NIL, n++ ) } )
        IF n > 0 .AND. nMode == 0
            n := HTMessageBox():choose( "Delete", "The folder " + hb_FNameNameExt( cPath ) + " is not empty." + Chr( 10 ) + ;
                                        "Delete it with everything in it?", { "&Delete", "&All", "&Skip", "&Cancel" }, HT_MSG_WARNING )
            DO CASE
            CASE n == 2
                nMode := 1
            CASE n == 3
                RETURN .T.
            CASE n != 1
                RETURN .F.
            ENDCASE
        ENDIF
        FOR EACH a IN aDir
            IF !( a[ 1 ] == "." .OR. a[ 1 ] == ".." )
                IF ! cnc_DeleteItem( hb_DirSepAdd( cPath ) + a[ 1 ], "D" $ a[ 5 ], @nMode )
                    RETURN .F.
                ENDIF
            ENDIF
        NEXT
        IF hb_DirDelete( cPath ) != 0
            RETURN HTMessageBox():choose( "CNC", "Cannot delete the folder " + hb_FNameNameExt( cPath ), { "&Skip", "&Cancel" }, HT_MSG_WARNING ) == 1
        ENDIF
        RETURN .T.
    ENDIF
    IF FErase( cPath ) != 0
        RETURN HTMessageBox():choose( "CNC", "Cannot delete " + hb_FNameNameExt( cPath ) + " (error " + hb_ntos( FError() ) + ")", ;
                                      { "&Skip", "&Cancel" }, HT_MSG_WARNING ) == 1
    ENDIF

RETURN .T.

/* Ctrl-F7: files by a mask in the folder and its subfolders */
STATIC PROCEDURE cnc_Find()

    LOCAL cMask, aFound := {}, aItems := {}, n, o := cnc_Act()

    IF o:isNet()
        HTToast():show( "Find file: not on an FTP server", 1500, HT_CLR_TOAST_INFO )
        RETURN
    ENDIF
    cMask := cnc_Input( "Find file", "File mask:", "*.prg" )
    IF Empty( cMask )
        RETURN
    ENDIF
    cnc_Progress( "Searching " + o:cDir )
    cnc_FindIn( o:cDir, cMask, aFound )
    cnc_Progress( "" )
    IF Empty( aFound )
        HTToast():show( "Not found: " + cMask, 1500, HT_CLR_TOAST_INFO )
        RETURN
    ENDIF
    AEval( aFound, {| c | AAdd( aItems, SubStr( c, Len( o:cDir ) + 1 ) ) } )
    n := cnc_List( hb_ntos( Len( aFound ) ) + " found", aItems, 1 )
    IF n > 0
        o:read( hb_FNameDir( aFound[ n ] ), hb_FNameNameExt( aFound[ n ] ) )
        o:refresh()
    ENDIF

RETURN

STATIC PROCEDURE cnc_FindIn( cDir, cMask, aFound )

    LOCAL a

    FOR EACH a IN Directory( cDir + hb_osFileMask(), "DHS" )
        IF Len( aFound ) >= 2000
            RETURN
        ENDIF
        IF a[ 1 ] == "." .OR. a[ 1 ] == ".."
            LOOP
        ENDIF
        IF hb_FileMatch( a[ 1 ], cMask )
            AAdd( aFound, cDir + a[ 1 ] )
        ENDIF
        IF "D" $ a[ 5 ]
            cnc_FindIn( cDir + a[ 1 ] + hb_ps(), cMask, aFound )
        ENDIF
    NEXT

RETURN

/* F2: the commands for the current file */
STATIC PROCEDURE cnc_UserMenu()

    LOCAL aItems := { "View          F3", "Edit          F4", "Copy          F5", "Rename/move   F6", ;
                      "Delete        F8", "Copy name     Ctrl-Ins", "Name to command line", "Directory size" }
    LOCAL n := cnc_List( "Menu", aItems, 1 )

    SWITCH n
    CASE 1 ; cnc_View() ; EXIT
    CASE 2 ; cnc_Edit( .F. ) ; EXIT
    CASE 3 ; cnc_CopyMove( .F. ) ; EXIT
    CASE 4 ; cnc_CopyMove( .T. ) ; EXIT
    CASE 5 ; cnc_Delete() ; EXIT
    CASE 6 ; cnc_CopyName() ; EXIT
    CASE 7 ; cnc_NameToCmd() ; EXIT
    CASE 8 ; cnc_View() ; EXIT
    ENDSWITCH

RETURN

STATIC PROCEDURE cnc_CopyName()
    LOCAL o := cnc_Act(), a := o:current()
    IF a != NIL
        hb_gtInfo( HB_GTI_CLIPBOARDDATA, cnc_Path( o, a ) )
        HTToast():show( "Copied: " + cnc_Path( o, a ), 1500, HT_CLR_TOAST_INFO )
    ENDIF
RETURN

/* Ctrl-Enter: the current name to the command line */
STATIC PROCEDURE cnc_NameToCmd()

    LOCAL a := cnc_Act():current(), c

    IF a == NIL
        RETURN
    ENDIF
    c := IIF( " " $ a[ F_NAME ], '"' + a[ F_NAME ] + '"', a[ F_NAME ] )
    s_oCmd:setText( s_oCmd:text + IIF( Empty( s_oCmd:text ) .OR. Right( s_oCmd:text, 1 ) == " ", "", " " ) + c )
    s_oMain:repaintChild( s_oCmd )

RETURN

STATIC PROCEDURE cnc_Quit()
    IF HTMessageBox():choose( "CNC", "Do you want to quit CNC?", { "&Yes", "&No" }, HT_MSG_QUESTION ) == 1
        AEval( s_aPanel, {| o | IIF( o:isNet(), o:oNet:close(), NIL ) } )
        s_oApp:quit()
    ENDIF
RETURN

/*----------------------------------------------------------- command line */

/* Ctrl-O: the command line takes the keys (Esc gives them back) */
STATIC PROCEDURE cnc_Console()
    s_oMain:setFocusChild( s_oCmd )
    s_oCmd:focusInEvent( NIL )
    s_aPanel[ s_nAct ]:refresh()
RETURN

/* Enter in the command line: cd, =expression, or a command in the folder
   of the active panel (its output: Ctrl-W) */
STATIC PROCEDURE cnc_RunLine()

    LOCAL cLine := AllTrim( s_oCmd:text ), cDir

    s_oCmd:setText( "" )
    s_oMain:repaintChild( s_oCmd )
    DO CASE
    CASE Empty( cLine )
        cnc_Activate( s_nAct )
    CASE Left( cLine, 1 ) == "="
        cnc_Calc( SubStr( cLine, 2 ) )
    CASE cnc_Act():isNet()
        HTToast():show( "The command line works in a local folder (Ctrl-Shift-F: disconnect)", 2500, HT_CLR_TOAST_INFO )
    CASE Lower( cLine ) == "cd" .OR. Lower( Left( cLine, 3 ) ) == "cd "
        cDir := AllTrim( SubStr( cLine, 3 ) )
        IF Empty( cDir )
            cDir := IIF( Empty( GetEnv( "HOME" ) ), GetEnv( "USERPROFILE" ), GetEnv( "HOME" ) )
        ELSEIF cDir == ".."
            cDir := cnc_ParentDir( cnc_Act():cDir )
        ENDIF
        cnc_GoDir( s_nAct, cnc_FullPath( cDir ) )
    OTHERWISE
        cnc_Run( cLine )
    ENDCASE

RETURN

/* runs a command and shows what it wrote */
STATIC PROCEDURE cnc_Run( cCmd )

    LOCAL cOut := "", cErr := "", nRes

    cnc_Progress( "Running: " + cCmd )
    hb_cwd( cnc_Act():cDir )
#ifdef __PLATFORM__WINDOWS
    HB_SYMBOL_UNUSED( cErr )
    nRes := cnc_RunHidden( "cmd /c " + cCmd, @cOut )      /* no console window (cncwin.c) */
#else
    nRes := hb_processRun( "sh -c " + cnc_ShQuote( cCmd ), , @cOut, @cErr )
#endif
    cnc_Progress( "" )
    cOut := cnc_ToUTF8( cOut ) + IIF( Empty( cErr ), "", hb_eol() + cnc_ToUTF8( cErr ) )
    s_cOutCmd := cCmd
    s_cOutput := cnc_Act():cDir + "> " + cCmd + hb_eol() + cOut + hb_eol() + "[exit code " + hb_ntos( nRes ) + "]"
    cnc_Reread()
    IF s_hSet[ "output" ]
        cnc_ShowOutput()
    ELSE
        HTToast():show( "Exit code " + hb_ntos( nRes ) + " - Ctrl-W: the output", 2000, ;
                        IIF( nRes == 0, HT_CLR_TOAST_SUCCESS, HT_CLR_TOAST_INFO ) )
    ENDIF

RETURN

#ifndef __PLATFORM__WINDOWS
STATIC FUNCTION cnc_ShQuote( c )
RETURN "'" + StrTran( c, "'", "'\''" ) + "'"
#endif

/* the output of a Windows console program is in the OEM codepage */
STATIC FUNCTION cnc_ToUTF8( c )
RETURN IIF( hb_StrIsUTF8( c ), c, hb_StrToUTF8( c, "SL852" ) )

/* Ctrl-W: the output of the last command */
STATIC PROCEDURE cnc_ShowOutput()
    IF Empty( s_cOutput )
        HTToast():show( "No command has run yet (Ctrl-O: the command line)", 2000, HT_CLR_TOAST_INFO )
        RETURN
    ENDIF
    cnc_ViewText( "Output: " + s_cOutCmd, s_cOutput )
RETURN

/* =: a Harbour expression (=1024*16, =Date(), =hb_ntos(2^10)) */
STATIC PROCEDURE cnc_Calc( cExpr )

    LOCAL xRes, oErr, cRes

    IF Empty( cExpr )
        cExpr := cnc_Input( "Calculator", "Expression:", "" )
        IF Empty( cExpr )
            RETURN
        ENDIF
    ENDIF
    BEGIN SEQUENCE WITH {| e | Break( e ) }
        xRes := &( cExpr )
        cRes := hb_ValToExp( xRes )
    RECOVER USING oErr
        cRes := "Error: " + oErr:description + IIF( Empty( oErr:operation ), "", " (" + oErr:operation + ")" )
    END SEQUENCE
    HTMessageBox():information( "Calculator", cExpr + Chr( 10 ) + "= " + cRes )

RETURN

/*----------------------------------------------------------- Net */

/* Ctrl-F: the saved connections (or a new one); Ctrl-N: a new one */
STATIC PROCEDURE cnc_FtpConnect( lNew )

    LOCAL aList := cnc_FtpList( s_hFtp ), aItems := {}, aConn, n, h, aNames := {}

    IF ! lNew .AND. ! Empty( aList )
        AEval( aList, {| x | AAdd( aItems, PadR( x[ 1 ], 16 ) + " " + x[ 4 ] + "@" + x[ 2 ] + x[ 5 ] ) } )
        AAdd( aItems, "<New connection...>" )
        AAdd( aItems, "<Delete a connection...>" )
        n := cnc_List( "FTP connect", aItems, 1 )
        IF n == 0
            RETURN
        ELSEIF n == Len( aItems )
            AEval( aList, {| x | AAdd( aNames, x[ 1 ] ) } )
            n := cnc_List( "Delete the connection", aNames, 1 )
            IF n > 0
                hb_HDel( s_hFtp, aNames[ n ] )
            ENDIF
            RETURN
        ELSEIF n <= Len( aList )
            aConn := aList[ n ]
        ENDIF
    ENDIF
    h := cnc_FtpDialog( IIF( aConn == NIL, "FTP new connection", "FTP connect: " + aConn[ 1 ] ), aConn )
    IF h == NIL
        RETURN
    ENDIF
    IF ! Empty( h[ "name" ] )
        s_hFtp[ h[ "name" ] ] := h[ "host" ] + "|" + hb_ntos( h[ "port" ] ) + "|" + h[ "user" ] + "|" + h[ "dir" ]
    ENDIF
    cnc_FtpOpen( s_nAct, h )

RETURN

STATIC PROCEDURE cnc_FtpOpen( n, h )

    LOCAL oNet := CNCFtp():new(), o := s_aPanel[ n ]

    cnc_Progress( "Connecting to " + h[ "host" ] + " ..." )
    IF ! oNet:connect( h[ "host" ], h[ "port" ], h[ "user" ], h[ "pass" ] )
        cnc_Progress( "" )
        HTMessageBox():error( "FTP", oNet:cErr )
        RETURN
    ENDIF
    cnc_Progress( "" )
    cnc_FtpClose( n )
    o:cLocal := o:cDir
    IF o:nView == CNC_VIEW_TREE                  /* no tree of a server: the files */
        o:nView := CNC_VIEW_FULL
    ENDIF
    o:oNet := oNet
    IF ! o:read( h[ "dir" ] ) .AND. ! o:read( "/" )
        HTMessageBox():error( "FTP", oNet:cErr )
        cnc_FtpClose( n )
        RETURN
    ENDIF
    o:refresh()
    cnc_Changed( o )

RETURN

/* Ctrl-Shift-F: the FTP panel (the active one, else the other) back to its folder */
STATIC PROCEDURE cnc_FtpDisconnect()
    IF cnc_Act():isNet()
        cnc_FtpClose( s_nAct )
    ELSEIF s_aPanel[ 3 - s_nAct ]:isNet()
        cnc_FtpClose( 3 - s_nAct )
    ELSE
        HTToast():show( "No FTP connection", 1500, HT_CLR_TOAST_INFO )
    ENDIF
RETURN

STATIC PROCEDURE cnc_FtpClose( n )

    LOCAL o := s_aPanel[ n ]

    IF ! o:isNet()
        RETURN
    ENDIF
    o:oNet:close()
    o:oNet := NIL
    o:cDir := ""
    IF Empty( o:cLocal ) .OR. ! o:read( o:cLocal )
        o:read( hb_DirBase() )
    ENDIF
    o:refresh()
    cnc_Changed( o )

RETURN

/* F3 / F4 on an FTP panel: the file is downloaded, viewed or edited, and
   uploaded again when it was changed */
STATIC PROCEDURE cnc_NetEdit( o, a, lView )

    LOCAL cRemote := o:cDir + a[ F_NAME ], cTmp := cnc_TempName( a[ F_NAME ] ), cBefore := NIL, lNew := a[ F_SIZE ] == 0 .AND. Empty( a[ F_ATTR ] )

    IF a[ F_DIR ]
        HTToast():show( "A folder on the FTP server", 1500, HT_CLR_TOAST_INFO )
        RETURN
    ENDIF
    IF ! lNew
        cnc_Progress( "Downloading " + a[ F_NAME ] )
        IF ! o:oNet:get( cRemote, cTmp )
            cnc_Progress( "" )
            HTMessageBox():error( "FTP", o:oNet:cErr )
            FErase( cTmp )
            RETURN
        ENDIF
        cnc_Progress( "" )
        cBefore := hb_MemoRead( cTmp )
    ENDIF
    cnc_EditFile( cTmp, lView, o:oNet:url( cRemote ) )
    IF ! lView .AND. hb_FileExists( cTmp ) .AND. ( cBefore == NIL .OR. !( hb_MemoRead( cTmp ) == cBefore ) )
        cnc_Progress( "Uploading " + a[ F_NAME ] )
        IF o:oNet:put( cTmp, cRemote )
            HTToast():show( "Uploaded: " + a[ F_NAME ], 1500, HT_CLR_TOAST_SUCCESS )
        ELSE
            HTMessageBox():error( "FTP", o:oNet:cErr )
        ENDIF
        cnc_Progress( "" )
        o:read( o:cDir, a[ F_NAME ] )
        o:refresh()
    ENDIF
    FErase( cTmp )

RETURN

STATIC FUNCTION cnc_TempName( cName )
RETURN hb_DirSepAdd( hb_DirTemp() ) + "cnc" + hb_ntos( hb_RandomInt( 100000, 999999 ) ) + "_" + cName

/* F5 / F6 between an FTP panel and a local one */
STATIC PROCEDURE cnc_NetCopy( oSrc, oDst, aList, lMove )

    LOCAL a, nMode := 0, nDone := 0, lOk, cWhat, nDel := 1

    IF oSrc:isNet() .AND. oDst:isNet()
        HTMessageBox():error( "FTP", "From one FTP panel to another: copy to a local folder first" )
        RETURN
    ENDIF
    cWhat := IIF( Len( aList ) == 1, '"' + aList[ 1 ][ F_NAME ] + '"', hb_ntos( Len( aList ) ) + " files" )
    IF HTMessageBox():choose( IIF( lMove, "Move", "Copy" ), IIF( lMove, "Move ", "Copy " ) + cWhat + " to" + Chr( 10 ) + ;
                              IIF( oDst:isNet(), oDst:oNet:url( oDst:cDir ), oDst:cDir ) + " ?", ;
                              { IIF( lMove, "&Move", "&Copy" ), "&Cancel" }, HT_MSG_QUESTION ) != 1
        RETURN
    ENDIF
    FOR EACH a IN aList
        IF oSrc:isNet()
            lOk := cnc_NetGet( oSrc:oNet, oSrc:cDir + a[ F_NAME ], oDst:cDir + a[ F_NAME ], a[ F_DIR ], @nMode )
        ELSE
            lOk := .T.
            IF AScan( oDst:aFiles, {| x | x[ F_NAME ] == a[ F_NAME ] } ) > 0 .AND. ! a[ F_DIR ]
                lOk := cnc_AskOverwrite( a[ F_NAME ], @nMode )
            ENDIF
            IF lOk == NIL
                EXIT
            ENDIF
            lOk := lOk .AND. cnc_NetPut( oDst:oNet, cnc_Path( oSrc, a ), oDst:cDir + a[ F_NAME ], a[ F_DIR ] )
        ENDIF
        IF lOk == NIL
            EXIT
        ENDIF
        IF lOk
            IF lMove
                IF oSrc:isNet()
                    cnc_NetDelete( oSrc:oNet, oSrc:cDir + a[ F_NAME ], a[ F_DIR ] )
                ELSE
                    cnc_DeleteItem( cnc_Path( oSrc, a ), a[ F_DIR ], @nDel )
                ENDIF
            ENDIF
            a[ F_MARK ] := .F.
            nDone++
        ENDIF
    NEXT
    cnc_Progress( "" )
    oSrc:reread()
    oDst:reread()
    AEval( s_aPanel, {| o | o:refresh() } )
    IF nDone > 0
        HTToast():show( hb_ntos( nDone ) + IIF( lMove, " moved", " copied" ), 1500, HT_CLR_TOAST_SUCCESS )
    ENDIF

RETURN

/* a file exists: .T. overwrite, .F. skip, NIL cancel; nMode 1 / 2 = all */
STATIC FUNCTION cnc_AskOverwrite( cName, nMode )

    LOCAL n

    IF nMode == 1
        RETURN .T.
    ELSEIF nMode == 2
        RETURN .F.
    ENDIF
    n := HTMessageBox():choose( "CNC", cName + " already exists. Overwrite it?", ;
                                { "&Overwrite", "&All", "&Skip", "Skip a&ll", "&Cancel" }, HT_MSG_WARNING )
    DO CASE
    CASE n == 1
        RETURN .T.
    CASE n == 2
        nMode := 1
        RETURN .T.
    CASE n == 3
        RETURN .F.
    CASE n == 4
        nMode := 2
        RETURN .F.
    ENDCASE

RETURN NIL

/* download a file or a folder: .T. done, .F. skipped, NIL cancelled */
STATIC FUNCTION cnc_NetGet( oNet, cRemote, cLocal, lDir, nMode )

    LOCAL aList, a, x

    IF lDir
        hb_DirBuild( cLocal )
        IF ( aList := oNet:dir( cnc_FtpDir( cRemote ) ) ) == NIL
            RETURN cnc_NetError( oNet )
        ENDIF
        FOR EACH a IN aList
            x := cnc_NetGet( oNet, cnc_FtpDir( cRemote ) + a[ F_NAME ], hb_DirSepAdd( cLocal ) + a[ F_NAME ], a[ F_DIR ], @nMode )
            IF x == NIL
                RETURN NIL
            ENDIF
        NEXT
        RETURN .T.
    ENDIF
    IF hb_FileExists( cLocal ) .AND. ( x := cnc_AskOverwrite( hb_FNameNameExt( cLocal ), @nMode ) ) != .T.
        RETURN x
    ENDIF
    cnc_Progress( "Downloading " + cRemote )
    IF ! oNet:get( cRemote, cLocal )
        RETURN cnc_NetError( oNet )
    ENDIF

RETURN .T.

/* upload a file or a folder: .T. done, .F. skipped, NIL cancelled */
STATIC FUNCTION cnc_NetPut( oNet, cLocal, cRemote, lDir )

    LOCAL a, x

    IF lDir
        oNet:mkdir( cnc_FtpDir( cRemote ) )          /* it may be there already */
        FOR EACH a IN Directory( hb_DirSepAdd( cLocal ) + hb_osFileMask(), "DHS" )
            IF !( a[ 1 ] == "." .OR. a[ 1 ] == ".." )
                x := cnc_NetPut( oNet, hb_DirSepAdd( cLocal ) + a[ 1 ], cnc_FtpDir( cRemote ) + a[ 1 ], "D" $ a[ 5 ] )
                IF x == NIL
                    RETURN NIL
                ENDIF
            ENDIF
        NEXT
        RETURN .T.
    ENDIF
    cnc_Progress( "Uploading " + cLocal )
    IF ! oNet:put( cLocal, cRemote )
        RETURN cnc_NetError( oNet )
    ENDIF

RETURN .T.

/* delete on the server, a folder with everything in it */
STATIC FUNCTION cnc_NetDelete( oNet, cRemote, lDir )

    LOCAL aList, a

    IF lDir
        IF ( aList := oNet:dir( cnc_FtpDir( cRemote ) ) ) == NIL
            RETURN cnc_NetError( oNet ) == .T.
        ENDIF
        FOR EACH a IN aList
            IF ! cnc_NetDelete( oNet, cnc_FtpDir( cRemote ) + a[ F_NAME ], a[ F_DIR ] )
                RETURN .F.
            ENDIF
        NEXT
        oNet:oFtp:Cwd( cnc_FtpParent( cnc_FtpDir( cRemote ) ) )
        IF ! oNet:rmdir( cRemote )
            RETURN cnc_NetError( oNet ) == .T.
        ENDIF
        RETURN .T.
    ENDIF
    IF ! oNet:del( cRemote )
        RETURN cnc_NetError( oNet ) == .T.
    ENDIF

RETURN .T.

/* an FTP error: .F. skip, NIL cancel */
STATIC FUNCTION cnc_NetError( oNet )
RETURN IIF( HTMessageBox():choose( "FTP", oNet:cErr, { "&Skip", "&Cancel" }, HT_MSG_WARNING ) == 1, .F., NIL )

/* Net - Map / Disconnect network drive, Network computers (Windows) */
STATIC PROCEDURE cnc_NetCmd( nCmd )

#ifdef __PLATFORM__WINDOWS
    LOCAL c

    SWITCH nCmd
    CASE 1
        IF ! Empty( c := cnc_NetMap() )
            cnc_GoDir( s_nAct, c )
        ENDIF
        EXIT
    CASE 2
        IF ! Empty( c := cnc_NetUnmap() )
            AEval( s_aPanel, {| o, i | IIF( ! o:isNet() .AND. Upper( Left( o:cDir, 2 ) ) == Upper( c ), cnc_GoDir( i, hb_DirBase() ), NIL ) } )
            HTToast():show( "Disconnected: " + c, 1500, HT_CLR_TOAST_SUCCESS )
        ENDIF
        EXIT
    CASE 3
        IF ! Empty( c := cnc_NetBrowse() )
            cnc_GoDir( s_nAct, c )
        ENDIF
        EXIT
    ENDSWITCH
#else
    HB_SYMBOL_UNUSED( nCmd )
    HTMessageBox():information( "Network", "Network drives and computers: Windows only." + Chr( 10 ) + ;
        "On Linux mount the share first (smb:// in the file manager," + Chr( 10 ) + "or mount -t cifs), then open its folder." )
#endif

RETURN

/*----------------------------------------------------------- viewer / editor */

/* F3 / F4: the editor of CEdit in a window over the panels */
STATIC PROCEDURE cnc_EditFile( cFile, lView, cTitle )

    LOCAL oB := CEBuffer():new(), dlg

    IF hb_FileExists( cFile )
        IF hb_FSize( cFile ) > 32 * 1024 * 1024
            HTMessageBox():error( "CNC", hb_FNameNameExt( cFile ) + " is bigger than 32 MB" )
            RETURN
        ENDIF
        IF ! oB:load( cFile, "SL852" )
            HTMessageBox():error( "CNC", oB:cError )
            RETURN
        ENDIF
    ELSE
        oB:cFile := cFile
    ENDIF
    oB:cSyntax := ce_SyntaxFor( cFile )
    oB:lHilite := .T.
    oB:lReadOnly := lView
    dlg := CNCEditDlg():create( oB, IIF( lView, "View: ", "Edit: " ) + hb_defaultValue( cTitle, cFile ), lView )
    dlg:exec()

RETURN

STATIC PROCEDURE cnc_ViewText( cTitle, cText )

    LOCAL oB := CEBuffer():new(), dlg

    oB:setText( StrTran( cText, Chr( 13 ), "" ) )
    oB:lReadOnly := .T.
    dlg := CNCEditDlg():create( oB, cTitle, .T. )
    dlg:exec()

RETURN

CLASS CNCEditDlg FROM HTDialog
    DATA oEdit
    DATA oStat
    DATA lView       INIT .T.
    DATA cFind       INIT ""
    METHOD create( oBuf, cTitle, lView )
    METHOD keyEvent( keyEvent )
    METHOD status()
    METHOD save()
    METHOD find( lAgain )
ENDCLASS

METHOD create( oBuf, cTitle, lView ) CLASS CNCEditDlg

    LOCAL nW := ht_ScreenMaxCol() + 1, nH := ht_ScreenMaxRow() + 1

    ::super:new()
    ::lView := lView
    ::setWindowTitle( cTitle )
    ::resize( nW, nH )
    ::move( 0, 0 )
    ::oEdit := HTCodeEdit():new( self )
    ::oEdit:setGeometry( 0, 0, nW - 2, nH - 3 )
    ::oEdit:setBuffer( oBuf )
    ::oEdit:cColor := CNC_CLR_PANEL                /* as in the NC viewer: cyan on blue */
    ::oEdit:cSelColor := CNC_CLR_CURSOR
    ::oEdit:onCursor := {|| ::status() }
    ::oEdit:bCtrlKey := {| c | IIF( c == "F", ( ::find( .F. ), .T. ), .F. ) }
    ::oStat := HTLabel():new( "", self )
    ::oStat:setGeometry( 0, nH - 3, nW - 2, 1 )
    ::status()

RETURN self

METHOD status() CLASS CNCEditDlg

    LOCAL oB := ::oEdit:oBuf, c

    c := " Ln " + hb_ntos( oB:nRow ) + "/" + hb_ntos( oB:lineCount() ) + "  Col " + hb_ntos( oB:nCol ) + ;
         IIF( ! ::lView .AND. oB:isModified(), "  *", "" ) + ;
         IIF( ::lView, "   F7 find  Shift-F7 next  Esc / F3 close", "   F2 save  F7 find  Shift-F7 next  Esc close" )
    ::oStat:setText( c )
    ::repaintChild( ::oStat )

RETURN NIL

METHOD save() CLASS CNCEditDlg

    LOCAL oB := ::oEdit:oBuf

    IF ! oB:save( oB:cFile )
        HTMessageBox():error( "CNC", oB:cError )
        RETURN .F.
    ENDIF
    HTToast():show( "Saved: " + hb_FNameNameExt( oB:cFile ), 1500, HT_CLR_TOAST_SUCCESS )
    ::status()

RETURN .T.

/* F7 / Ctrl-F: find; Shift-F7: the next one */
METHOD find( lAgain ) CLASS CNCEditDlg

    LOCAL oB := ::oEdit:oBuf, a

    IF ! lAgain .OR. Empty( ::cFind )
        ::cFind := cnc_Input( "Find", "Text:", ::cFind )
        IF Empty( ::cFind )
            RETURN NIL
        ENDIF
    ENDIF
    a := oB:find( ::cFind, oB:nRow, oB:nCol + IIF( lAgain, 1, 0 ), .F., .F., .F., .T. )
    IF a == NIL
        HTToast():show( "Not found: " + ::cFind, 1500, HT_CLR_TOAST_INFO )
        RETURN NIL
    ENDIF
    ::oEdit:gotoPos( a[ 1 ], a[ 2 ] )
    ::oEdit:refresh()

RETURN NIL

METHOD PROCEDURE keyEvent( keyEvent ) CLASS CNCEditDlg

    LOCAL nKey := keyEvent:key, oB := ::oEdit:oBuf, n

    DO CASE
    CASE nKey == K_ESC .OR. nKey == K_F10 .OR. ( ::lView .AND. nKey == K_F3 )
        keyEvent:accept()
        IF nKey == K_ESC .AND. ::oEdit:cancelMarking()
            RETURN
        ENDIF
        IF ! ::lView .AND. oB:isModified()
            n := HTMessageBox():choose( "CNC", hb_FNameNameExt( oB:cFile ) + " has been changed. Save it?", ;
                                        { "&Save", "&Don't save", "&Cancel" }, HT_MSG_QUESTION )
            IF n == 3 .OR. n == 0 .OR. ( n == 1 .AND. ! ::save() )
                RETURN
            ENDIF
        ENDIF
        ::accept()
        RETURN
    CASE nKey == K_F2 .AND. ! ::lView
        keyEvent:accept()
        ::save()
        RETURN
    CASE nKey == K_F7
        keyEvent:accept()
        ::find( .F. )
        RETURN
    CASE nKey == K_SH_F7
        keyEvent:accept()
        ::find( .T. )
        RETURN
    CASE nKey == K_TAB
        ::oEdit:keyEvent( keyEvent )
        RETURN
    ENDCASE

    ::super:keyEvent( keyEvent )

RETURN

/*----------------------------------------------------------- widgets */

/* the command line: Enter runs it, Esc gives the keys back to the panel */
CLASS CNCLine FROM HTLineEdit
    DATA bEnter
    DATA bEsc
    METHOD enterMovesFocus() INLINE .F.
    METHOD keyEvent( keyEvent )
ENDCLASS

METHOD PROCEDURE keyEvent( keyEvent ) CLASS CNCLine
    IF keyEvent:key == K_ENTER .AND. ::bEnter != NIL
        keyEvent:accept()
        Eval( ::bEnter )
        RETURN
    ELSEIF keyEvent:key == K_ESC .AND. ::bEsc != NIL
        keyEvent:accept()
        Eval( ::bEsc )
        RETURN
    ENDIF
    ::super:keyEvent( keyEvent )
RETURN

/* 1Help 2Menu 3View ... 10Quit; a click runs the key; a text in cMsg
   (copying, searching ...) takes the row while it works */
CLASS CNCKeyBar FROM HTWidget
    DATA bClick
    DATA cMsg       INIT ""
    METHOD new( parent )
    METHOD paintEvent( event )
    METHOD mouseEvent( eventMouse )
ENDCLASS

METHOD new( parent ) CLASS CNCKeyBar
    ::super:new( parent )
    ::setFocusPolicy( HT_FOCUS_NONE )
    ::FisVisible := .T.
RETURN self

STATIC FUNCTION cnc_KeyNames()
RETURN { "Help", "Menu", "View", "Edit", "Copy", "RenMov", "Mkdir", "Delete", "PullDn", "Quit" }

METHOD PROCEDURE paintEvent( event ) CLASS CNCKeyBar

    LOCAL nW := ::Fwidth, nCell := Int( ::Fwidth / 10 ), i, nCol := 0, cNum

    HB_SYMBOL_UNUSED( event )
    IF ! Empty( ::cMsg )
        DispOutAt( 0, 0, cnc_Fit( " " + ::cMsg, nW ), CNC_CLR_KEYTXT )
        RETURN
    ENDIF
    FOR i := 1 TO 10
        cNum := hb_ntos( i )
        DispOutAt( 0, nCol, cNum, CNC_CLR_KEYNUM )
        DispOutAt( 0, nCol + Len( cNum ), PadR( cnc_KeyNames()[ i ], IIF( i == 10, nW - nCol - Len( cNum ), nCell - Len( cNum ) ) ), CNC_CLR_KEYTXT )
        nCol += nCell
    NEXT

RETURN

METHOD PROCEDURE mouseEvent( eventMouse ) CLASS CNCKeyBar
    IF eventMouse:nKey == K_LBUTTONDOWN .AND. HB_ISEVALITEM( ::bClick )
        Eval( ::bClick, Min( 10, Int( eventMouse:mouseCol / Max( 1, Int( ::Fwidth / 10 ) ) ) + 1 ) )
    ENDIF
RETURN

/* a text in the key bar while copying, deleting, searching */
STATIC PROCEDURE cnc_Progress( cText )
    s_oKeys:cMsg := cText
    s_oMain:repaintChild( s_oKeys )
RETURN

/*----------------------------------------------------------- dialogs */

/* list of items, Enter / double click chooses; returns the number, 0 = Esc */
FUNCTION cnc_List( cTitle, aItems, nCurrent )

    LOCAL dlg, oList, nW, nH, nResult := 0, c

    nW := Max( 30, ht_strWidth( cTitle ) + 12 )
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
FUNCTION cnc_Input( cTitle, cLabel, cInit )

    LOCAL dlg, oEdit, nW := Min( 72, ht_ScreenMaxCol() - 3 ), nH := 6, nL := Min( 30, Len( cLabel ) + 1 )

    dlg := HTDialog():new()
    dlg:setWindowTitle( cTitle )
    dlg:resize( nW, nH )
    dlg:move( Int( ( ht_ScreenMaxCol() + 1 - nW ) / 2 ), Int( ( ht_ScreenMaxRow() + 1 - nH ) / 2 ) )
    HTLabel():new( cLabel, dlg ):setGeometry( 1, 1, nL, 1 )
    oEdit := CNCLine():new( dlg )
    oEdit:setGeometry( nL + 2, 1, nW - nL - 5, 1 )
    oEdit:setText( cInit )
    oEdit:bEnter := {|| dlg:accept() }
    dlg:addButtonBar( "OK", "Cancel" )

RETURN IIF( dlg:exec() == HT_DIALOG_ACCEPTED, AllTrim( oEdit:text ), "" )

STATIC PROCEDURE cnc_Theme()

    LOCAL aNames := HTTheme():themeNames(), n

    n := cnc_List( "Theme", aNames, Max( 1, AScan( aNames, {| c | c == s_hSet[ "theme" ] } ) ) )
    IF n > 0
        s_hSet[ "theme" ] := aNames[ n ]
        HTTheme():loadTheme( aNames[ n ] )
        HTApplication():repaintAll()
    ENDIF

RETURN

STATIC PROCEDURE cnc_About()
    HTMessageBox():information( "CNC", "CNC " + CNC_VERSION + " - a file manager in character mode" + Chr( 10 ) + ;
        "built on Harbour-CUI" + Chr( 10 ) + Chr( 10 ) + ;
        "Copyright (c) 2026 Srđan Dragojlović" + Chr( 10 ) + ;
        "GPL-3.0-or-later or a commercial license" + Chr( 10 ) + Chr( 10 ) + ;
        "Keys as in HbCommander (Alexander S. Kresin)" + Chr( 10 ) + ;
        "and Norton Commander" + Chr( 10 ) + ;
        "Settings: " + s_cIni )
RETURN

/* the dialogs, menus and the key bar of Norton Commander, on the VGA palette */
STATIC PROCEDURE cnc_NortonTheme()

    LOCAL a, aPal

    IF HTTheme():hasTheme( "Norton" ) .OR. ! HTTheme():hasTheme( "HighContrast" )
        RETURN
    ENDIF
    a := AClone( HTTheme():themeColors( "HighContrast" ) )
    aPal := { 0, 8388608, 32768, 8421376, 128, 8388736, 32896, 12632256, ;
              8421504, 16711680, 65280, 16776960, 255, 16711935, 65535, 16777215 }
    AEval( { HT_CLR_MENU_BAR, HT_CLR_MENU_ITEM, HT_CLR_MENU_SEP, HT_CLR_CTXMENU_NORMAL, HT_CLR_CTXMENU_SEP, ;
             HT_CLR_STATUSBAR }, {| n | a[ n ] := "0/3" } )
    AEval( { HT_CLR_MENU_BAR_SEL, HT_CLR_MENU_ITEM_SEL, HT_CLR_CTXMENU_SELECTED }, {| n | a[ n ] := "15/0" } )
    a[ HT_CLR_MENU_HOTKEY ]     := "14/3"
    a[ HT_CLR_MENU_HOTKEY_SEL ] := "14/0"
    AEval( { HT_CLR_WINDOW, HT_CLR_LABEL, HT_CLR_FRAME, HT_CLR_GET_LABEL, HT_CLR_CHECK_NORMAL, ;
             HT_CLR_TAB_INACTIVE, HT_CLR_TAB_BAR, HT_CLR_TITLEBAR, HT_CLR_TITLEBTN }, {| n | a[ n ] := "0/7" } )
    AEval( { HT_CLR_BUTTON_NORMAL, HT_CLR_TAB_ACTIVE }, {| n | a[ n ] := "0/3" } )
    a[ HT_CLR_BUTTON_FOCUSED ]  := "15/3"
    a[ HT_CLR_CHECK_FOCUSED ]   := "15/7"
    AEval( { HT_CLR_GET_NORMAL, HT_CLR_LINEEDIT_NORMAL, HT_CLR_COMBO_NORMAL, HT_CLR_SPINNER_NORMAL }, {| n | a[ n ] := "0/3" } )
    AEval( { HT_CLR_GET_FOCUSED, HT_CLR_LINEEDIT_FOCUSED, HT_CLR_COMBO_FOCUSED, HT_CLR_SPINNER_FOCUSED }, {| n | a[ n ] := "15/3" } )
    a[ HT_CLR_LINEEDIT_SELECTED ] := "15/1"
    AEval( { HT_CLR_LIST_NORMAL, HT_CLR_CHECKLIST_NORMAL, HT_CLR_COMBO_DROP_N }, {| n | a[ n ] := "11/1" } )
    AEval( { HT_CLR_LIST_SELECTED, HT_CLR_CHECKLIST_SELECTED, HT_CLR_COMBO_DROP_S }, {| n | a[ n ] := "0/3" } )
    a[ HT_CLR_DESKTOP ] := "7/0"
    HTTheme():addTheme( "Norton", a, aPal )

RETURN

/*----------------------------------------------------------- help */

STATIC PROCEDURE cnc_Help()
    cnc_ViewText( "CNC help", cnc_HelpText() )
RETURN

STATIC FUNCTION cnc_HelpText()
RETURN ce_Join( { ;
    "CNC " + CNC_VERSION + " - a two-panel file manager (Harbour-CUI)", ;
    "Esc closes this help. F7 searches in it.", ;
    "", ;
    "PANELS", ;
    "  Tab          the other panel           Ctrl-U     swap the panels", ;
    "  Cursor keys, PgUp / PgDn, Home / End, the mouse wheel", ;
    "  Enter        into a folder; a program runs, a document opens in", ;
    "               its program, a table (.dbf) in CBro; double click the same", ;
    "  Backspace, Ctrl-PgUp   the parent folder", ;
    "  Ins, right click       mark a file (the marked ones are copied,", ;
    "                         moved, deleted together)", ;
    "  +  -  *      mark / unmark by a mask (*.prg), invert (while the", ;
    "               command line is empty)", ;
    "  Ctrl-S, Alt + letter   quick search (Alt-L F C O R open the menus)", ;
    "  Ctrl-P       full view (Name, Ext, Size, Date), brief (2 columns),", ;
    "               brief as Total Commander (as many columns as fit), tree", ;
    "  Tree (Left / Right menu, Ctrl-P): the folders of the drive; the other", ;
    "               panel shows the folder on the cursor. Right / Enter open a", ;
    "               folder, Left closes it (closed: up), Backspace up; F7 makes", ;
    "               a folder in it, F5 / F6 from the other panel copy into it", ;
    "  Ctrl-R       read the folders again", ;
    "  Ctrl-F3 .. Ctrl-F6     sort by name, extension, time, size; a click", ;
    "               on Name / Ext / Size / Date sorts too; again: the other way", ;
    "", ;
    "FILES", ;
    "  F1  help                    F2  menu of the current file", ;
    "  F3  view (on a folder: its size)", ;
    "  F4  edit                    Shift-F4  edit a new file", ;
    "  F5  copy to the other panel (or to the path typed)", ;
    "  F6  move / rename           Shift-F6  rename", ;
    "  F7  make a folder           F8  delete", ;
    "  F9  the menu                F10 quit", ;
    "  Ctrl-Ins     the full name of the file to the clipboard", ;
    "  Ctrl-F7      find files by a mask in the folder and below", ;
    "", ;
    "FOLDERS", ;
    "  cd <folder>  (typed), Ctrl-F1 / Ctrl-F2: left / right panel", ;
    "  Alt-F1 / Alt-F2        drive of the left / right panel", ;
    "  Alt-Down     the folders visited", ;
    "  Ctrl-D       the directory hotlist: go, add this folder, remove", ;
    "  Commands - Compare directories: marks the files that differ", ;
    "", ;
    "COMMAND LINE", ;
    "  Type         what is typed goes to the command line (as in NC);", ;
    "               Enter runs it in the folder of the active panel,", ;
    "               Backspace / Esc correct it; with an empty command line", ;
    "               Enter and Backspace are the panel's", ;
    "  Ctrl-O       the command line takes all the keys (Esc: back)", ;
    "  Ctrl-Enter   the current name to the command line", ;
    "  Ctrl-W       the output of the last command (Options - Show command", ;
    "               output: off = only the exit code after a command)", ;
    "  cd <folder>  changes the folder of the active panel", ;
    "  =            the calculator (command line empty): a Harbour", ;
    "               expression, 1024*16, Date(); or =1024*16 typed after", ;
    "               a command", ;
    "", ;
    "NET (as Total Commander)", ;
    "  Ctrl-F       FTP connect: the saved connections, or a new one", ;
    "  Ctrl-N       FTP new connection (host, or ftp://user@host:port/dir)", ;
    "  Ctrl-Shift-F FTP disconnect: the panel goes back to its folder", ;
    "  On an FTP panel: Enter / F3 view, F4 edit (saved back), F5 / F6", ;
    "  copy / move to the other panel and back, F7 folder, F8 delete,", ;
    "  Shift-F6 rename. Passwords are not saved.", ;
    "  Net - Map / Disconnect network drive, Network computers (Windows)", ;
    "", ;
    "VIEWER AND EDITOR (F3 / F4: the editor of CEdit)", ;
    "  F2 save, F7 / Ctrl-F find, Shift-F7 next, Esc close; the keys of", ;
    "  CEdit: F3 block, Ctrl-C / Ctrl-V, Ctrl-Z undo, Ctrl-Y delete line", ;
    "", ;
    "Settings, bookmarks and history:", ;
    "  " + s_cIni }, Chr( 10 ) )

/*----------------------------------------------------------- settings */

STATIC FUNCTION cnc_FullPath( cFile )
    cFile := hb_DirSepToOS( AllTrim( cFile ) )
    IF !( Left( cFile, 1 ) $ "\/" .OR. SubStr( cFile, 2, 1 ) == ":" )
        cFile := cnc_Act():cDir + cFile
    ENDIF
RETURN cFile

STATIC FUNCTION cnc_IniPath()

    LOCAL cExe := hb_DirBase() + "cnc.ini"

    IF hb_FileExists( cExe )
        RETURN cExe
    ENDIF
#ifdef __PLATFORM__WINDOWS
    RETURN hb_DirSepAdd( GetEnv( "APPDATA" ) ) + "CNC\cnc.ini"
#else
    RETURN hb_DirSepAdd( GetEnv( "HOME" ) ) + ".config/cnc/cnc.ini"
#endif

STATIC PROCEDURE cnc_LoadSettings()

    LOCAL hIni, h, c, cKey

    s_hSet := { "theme" => "Norton", "dir1" => "", "dir2" => "", "active" => 1, ;
                "sort1" => CNC_SORT_NAME, "sort2" => CNC_SORT_NAME, "view1" => CNC_VIEW_FULL, "view2" => CNC_VIEW_FULL, "hidden" => .F., ;
                "desc1" => .F., "desc2" => .F., "output" => .T., "cbro" => "" }
    s_cIni := cnc_IniPath()
    IF ! hb_FileExists( s_cIni )
        RETURN
    ENDIF
    hIni := hb_iniRead( s_cIni, .F., "=", .F. )
    IF ! HB_ISHASH( hIni )
        RETURN
    ENDIF
    h := hb_HGetDef( hIni, "OPTIONS", { => } )
    hb_HCaseMatch( h, .F. )                 /* hb_iniRead() gives the keys in upper case */
    s_hSet[ "theme" ]  := hb_HGetDef( h, "theme", "Norton" )
    s_hSet[ "dir1" ]   := hb_HGetDef( h, "dir1", "" )
    s_hSet[ "dir2" ]   := hb_HGetDef( h, "dir2", "" )
    s_hSet[ "active" ] := Val( hb_HGetDef( h, "active", "1" ) )
    s_hSet[ "sort1" ]  := Max( 1, Min( 4, Val( hb_HGetDef( h, "sort1", "1" ) ) ) )
    s_hSet[ "sort2" ]  := Max( 1, Min( 4, Val( hb_HGetDef( h, "sort2", "1" ) ) ) )
    FOR EACH c IN { "1", "2" }                /* full1=0 of older versions: brief */
        s_hSet[ "view" + c ] := Max( 1, Min( CNC_VIEW_TREE, Val( hb_HGetDef( h, "view" + c, IIF( hb_HGetDef( h, "full" + c, "1" ) == "0", "2", "1" ) ) ) ) )
    NEXT
    s_hSet[ "desc1" ]  := hb_HGetDef( h, "desc1", "0" ) == "1"
    s_hSet[ "desc2" ]  := hb_HGetDef( h, "desc2", "0" ) == "1"
    s_hSet[ "hidden" ] := hb_HGetDef( h, "hidden", "0" ) == "1"
    s_hSet[ "output" ] := !( hb_HGetDef( h, "output", "1" ) == "0" )
    s_hSet[ "cbro" ]   := hb_HGetDef( h, "cbro", "" )

    h := hb_HGetDef( hIni, "HOTLIST", hb_HGetDef( hIni, "BOOKMARKS", { => } ) )
    FOR EACH cKey IN h
        IF ! Empty( cKey )
            AAdd( s_aHot, cKey )
        ENDIF
    NEXT
    s_hFtp := hb_HGetDef( hIni, "FTP", { => } )
    h := hb_HGetDef( hIni, "HISTORY", { => } )
    FOR EACH cKey IN ASort( hb_HKeys( h ), , , {| x, y | Val( x ) < Val( y ) } )
        c := h[ cKey ]
        IF ! Empty( c )
            AAdd( s_aHist, c )
        ENDIF
    NEXT

RETURN

STATIC PROCEDURE cnc_SaveSettings()

    LOCAL hIni := { => }, h := { => }, i, c

    h[ "theme" ]  := s_hSet[ "theme" ]
    FOR i := 1 TO 2
        h[ "dir" + hb_ntos( i ) ]  := IIF( s_aPanel[ i ]:isNet(), s_aPanel[ i ]:cLocal, s_aPanel[ i ]:cDir )
        h[ "sort" + hb_ntos( i ) ] := hb_ntos( s_aPanel[ i ]:nSort )
        h[ "view" + hb_ntos( i ) ] := hb_ntos( s_aPanel[ i ]:nView )
        h[ "desc" + hb_ntos( i ) ] := IIF( s_aPanel[ i ]:lDesc, "1", "0" )
    NEXT
    h[ "active" ] := hb_ntos( s_nAct )
    h[ "hidden" ] := IIF( s_hSet[ "hidden" ], "1", "0" )
    h[ "output" ] := IIF( s_hSet[ "output" ], "1", "0" )
    IF ! Empty( s_hSet[ "cbro" ] )
        h[ "cbro" ] := s_hSet[ "cbro" ]
    ENDIF
    hIni[ "OPTIONS" ] := h
    h := { => }
    FOR EACH c IN s_aHot
        h[ hb_ntos( c:__enumIndex() ) ] := c
    NEXT
    hIni[ "HOTLIST" ] := h
    h := { => }
    FOR EACH c IN s_aHist
        h[ hb_ntos( c:__enumIndex() ) ] := c
    NEXT
    hIni[ "HISTORY" ] := h
    hIni[ "FTP" ] := s_hFtp
    hb_DirBuild( hb_FNameDir( s_cIni ) )
    hb_iniWrite( s_cIni, hIni, "# CNC settings", NIL, .F. )

RETURN
