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
#ifdef __PLATFORM__WINDOWS
#include "hbwin.ch"
#endif

REQUEST HB_CODEPAGE_UTF8EX
REQUEST HB_CODEPAGE_SL852
REQUEST HB_CODEPAGE_SLWIN
REQUEST HB_CODEPAGE_SLISO
REQUEST HB_CODEPAGE_SRWIN
REQUEST HB_CODEPAGE_RU866
REQUEST HB_CODEPAGE_RUKOI8
REQUEST HB_CODEPAGE_UAKOI8
REQUEST HB_CODEPAGE_UA1125
REQUEST HB_CODEPAGE_RUISO
REQUEST HB_CODEPAGE_BGMIK
REQUEST HB_CODEPAGE_EN
REQUEST HB_CODEPAGE_DE850
REQUEST HB_CODEPAGE_DE858
REQUEST HB_CODEPAGE_DEWIN
REQUEST HB_CODEPAGE_DEISO
REQUEST HB_CODEPAGE_SVISO
REQUEST HB_CODEPAGE_PT860
REQUEST HB_CODEPAGE_IS861
REQUEST HB_CODEPAGE_DK865
REQUEST HB_CODEPAGE_EL737
REQUEST HB_CODEPAGE_ELWIN
REQUEST HB_CODEPAGE_ELISO
REQUEST HB_CODEPAGE_TR857
REQUEST HB_CODEPAGE_TRWIN
REQUEST HB_CODEPAGE_TRISO
REQUEST HB_CODEPAGE_EE775
REQUEST HB_CODEPAGE_LTWIN
REQUEST HB_CODEPAGE_HE862
REQUEST HB_CODEPAGE_HEWIN
REQUEST HB_CODEPAGE_CSKAMC
REQUEST HB_CODEPAGE_PLMAZ
REQUEST HB_CODEPAGE_CP950
REQUEST HB_CODEPAGE_GBK
REQUEST HB_CODEPAGE_SR646
REQUEST HB_CODEPAGE_SR646C

STATIC s_oApp, s_oMain, s_oBar, s_oEdit, s_oStat
STATIC s_aBufs := {}
STATIC s_oTabs                        /* CETabBar: a tab for every open file */
STATIC s_nBuf := 0
STATIC s_aRecent := {}               /* { { cFile, nRow, nCol }, ... } the newest first */
STATIC s_hSet                        /* settings (cedit.ini [OPTIONS]) */
STATIC s_cIni := ""
STATIC s_cLang := ""                  /* keyboard language / IME mode (ht_inputLanguage) */
STATIC s_nIdxTask := 0                /* HTEventLoop id: keep indexing large files */

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
    ce_SyntaxLoad( ce_LangPath() )

    s_oApp := HTApplication():new()
    /* full screen; the grid is set up before the first window takes its size.
       The input language / IME mode is shown in the status line (no taskbar). */
    ht_cuiSetupDisplay( s_oApp:rows(), s_oApp:cols() )
    hb_gtInfo( HB_GTI_WINTITLE, "CEdit" )
    hb_gtInfo( HB_GTI_ICONRES, 1 )                 /* cedit.ico (cedit.rc), Windows */
    ce_TurboTheme()
    IF ! s_hSet[ "theme" ] == "Classic" .AND. HTTheme():hasTheme( s_hSet[ "theme" ] )
        HTTheme():loadTheme( s_hSet[ "theme" ] )
    ENDIF

    s_oMain := CEMain():new()
    s_oMain:setWindowTitle( "CEdit" )
    s_oMain:show()

    s_oEdit := HTCodeEdit():new( s_oMain )
    s_oEdit:onCursor := {|| ce_Status() }
    s_oEdit:bCtrlKey := {| c | ce_CtrlKey( c ) }
    s_oEdit:lInsert := s_hSet[ "insert" ]
    s_oEdit:lTabSpaces := s_hSet[ "tabspaces" ]
    ce_SetEditorColors( s_hSet[ "colors" ] )
    s_oTabs := CETabBar():new( s_oMain )
    s_oTabs:bTabs := {|| ce_TabList() }
    s_oTabs:bActive := {|| s_nBuf }
    s_oTabs:bSelect := {| n | ce_SwitchTo( n ), s_oEdit:setFocus(), ce_Status() }
    s_oTabs:bClose := {| n | ce_SwitchTo( n ), ce_CmdClose(), ce_After(), ce_Status() }
    s_oStat := HTStatusBar():new( s_oMain )
    s_oStat:addSection( "" )
    ce_Arrange()
    ce_BuildMenu()

    FOR EACH cArg IN hb_AParams()
        /* options: -x (and /x on Windows; on Linux "/" starts a path) */
        IF !( Left( cArg, 1 ) == "-" .OR. ( Left( cArg, 1 ) == "/" .AND. hb_ps() == "\" ) )
            ce_OpenFile( cArg )
        ENDIF
    NEXT
    IF Empty( s_aBufs )
        ce_NewBuffer()
    ENDIF

    s_oEdit:setFocus()
    s_cLang := ht_inputLanguage()
    IF ! Empty( s_cLang )
        HTEventLoop():scheduleRepeat( {|| ce_InputLang() }, 300 )
    ENDIF
    /* large files: first chunk is already indexed in load(); the rest here */
    s_nIdxTask := HTEventLoop():scheduleRepeat( {|| ce_IndexIdle() }, 50 )
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

    LOCAL nTop := IIF( s_hSet[ "tabs" ], 1, 0 )

    s_oTabs:setGeometry( 0, 0, IIF( nTop == 1, s_oMain:width, 0 ), nTop )
    s_oEdit:setGeometry( 0, nTop, s_oMain:width, Max( 1, s_oMain:height - 2 - nTop ) )
    s_oStat:setGeometry( 0, s_oMain:height - 2, s_oMain:width, 1 )

RETURN

/* the tabs: { { cTitle, lModified }, ... } */
STATIC FUNCTION ce_TabList()

    LOCAL aTabs := {}, oB

    FOR EACH oB IN s_aBufs
        AAdd( aTabs, { oB:title(), oB:isModified() .AND. ! oB:lReadOnly } )
    NEXT

RETURN aTabs

/* View - Tabs */
STATIC PROCEDURE ce_CmdTabs()
    s_hSet[ "tabs" ] := ! s_hSet[ "tabs" ]
    ce_Arrange()
    HTApplication():repaintAll()
RETURN

/*----------------------------------------------------------- menu */

STATIC PROCEDURE ce_BuildMenu()

    LOCAL oMenu

    s_oBar := s_oMain:menuBar()
    s_oBar:setBarRow( 0 )

    oMenu := s_oBar:addMenu( "&File" )
    ce_Item( oMenu, "&New", "Shift-F4", K_SH_F4, {|| ce_NewBuffer() } )
    ce_Item( oMenu, "&Open...", "F4", K_F4, {|| ce_CmdOpenRecent() } )
    ce_Item( oMenu, "Open &file...", "Ctrl-O", K_CTRL_O, {|| ce_CmdOpen() } )
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
    ce_Item( oMenu, "&Find...", "F7 / Ctrl-F", K_F7, {|| ce_CmdFind() } )
    ce_Item( oMenu, "Find &next", "Shift-F7", K_SH_F7, {|| ce_FindNext( .F. ) } )
    ce_Item( oMenu, "Find &previous", "Alt-F7", K_ALT_F7, {|| ce_FindNext( .T. ) } )
    ce_Item( oMenu, "&Replace...", "Ctrl-F7/Ctrl-H", K_CTRL_F7, {|| ce_CmdReplace() } )
    oMenu:addSeparator()
    ce_Item( oMenu, "&Word at cursor", "Alt-8", K_ALT_8, {|| ce_FindWord( .F. ) } )
    ce_Item( oMenu, "Word, b&ackwards", "Alt-3", K_ALT_3, {|| ce_FindWord( .T. ) } )
    oMenu:addSeparator()
    ce_Item( oMenu, "&Go to line...", "Alt-F8", K_ALT_F8, {|| ce_CmdGoto() } )
    ce_Item( oMenu, "&Matching bracket", "Ctrl-B", NIL, {|| s_oEdit:matchBracket() } )
    ce_Item( oMenu, "&Back", "Alt-B", K_ALT_B, {|| ce_CmdBack() } )

    oMenu := s_oBar:addMenu( "&View" )
    ce_Item( oMenu, "Ta&bs (open files)", "", NIL, {|| ce_CmdTabs() } )
    ce_Item( oMenu, "&Syntax highlighting", "F8", K_F8, {|| ce_CmdHilite() } )
    ce_Item( oMenu, "&Language...", "", NIL, {|| ce_CmdSyntax() } )
    ce_Item( oMenu, "Language &definitions", "", NIL, {|| ce_CmdLangFile() } )
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
    LOCAL oAct := oMenu:addAction( PadR( cText, IIF( "&" $ cText, 21, 20 ) ) + PadL( cKey, 14 ) )

    IF nKey != NIL
        oAct:setShortcut( nKey )
    ENDIF
    oAct:onTriggered := {|| Eval( bAction ), ce_After() }

RETURN

/* Ctrl-letters that are also cursor keys (Ctrl-F = End, Ctrl-H = Backspace):
   the editor gives them here, they cannot be menu shortcuts */
STATIC FUNCTION ce_CtrlKey( c )

    SWITCH c
    CASE "F"
        ce_CmdFind()
        ce_After()
        RETURN .T.
    CASE "H"
        ce_CmdReplace()
        ce_After()
        RETURN .T.
    ENDSWITCH

RETURN .F.

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
    IF s_oTabs != NIL                    /* "*" of a changed file */
        s_oTabs:refresh()
    ENDIF
    DO CASE
    CASE oB:lPaged .AND. ! oB:lIndexed
        cMode := "Idx " + hb_ntos( Int( 100 * oB:nIdxPos / Max( 1, oB:nDocLen ) ) ) + "%"
    CASE oB:lPaged
        cMode := IIF( oB:isModified(), "Swap", "Edit" )
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
              IIF( oB:lPaged .AND. ! oB:lIndexed, "+", "" ) + ;
              "  Col " + hb_ntos( s_oEdit:cursorCell() + 1 ) + "  " + ;
              IIF( Empty( oB:cSyntax ) .OR. ! oB:lHilite, "", oB:cSyntax + "  " ) + ;
              ce_CPName( oB:cCP ) + IIF( oB:cEol == Chr( 10 ), " LF", "" ) + "  " + ;
              IIF( s_oEdit:lInsert, "Ins", "Ovr" ) + "  " + cMode + " " + ;
              IIF( Empty( s_cLang ), "", " " + s_cLang + " " )
    s_oStat:setSection( 1, ht_cellPadR( cLeft, Max( 1, s_oMain:width - ht_strWidth( cRight ) ) ) + cRight )
    s_oMain:repaintChild( s_oStat )

RETURN

/* keep indexing every open paged buffer; also finishes a reindex after edits */
STATIC PROCEDURE ce_IndexIdle()

    LOCAL oB, lPaint := .F., lDone := .F., nPos0, nEnd, oCur := ce_Cur()

    nEnd := hb_MilliSeconds() + CE_IDLE_MS
    FOR EACH oB IN s_aBufs
        IF ! oB:lPaged
            LOOP
        ENDIF
        IF oB:lIndexed
            IF oB:follow()
                oB:indexStep( CE_IDLE_STEP )
                lPaint := .T.
            ENDIF
        ELSE
            nPos0 := oB:nIdxPos
            DO WHILE ! oB:lIndexed .AND. hb_MilliSeconds() < nEnd
                oB:indexStep( CE_IDLE_STEP )
                IF ! Empty( oB:cError )
                    HTMessageBox():error( "CEdit", oB:cError )
                    EXIT
                ENDIF
            ENDDO
            IF oB:nIdxPos != nPos0 .OR. oB:lIndexed
                lPaint := .T.
            ENDIF
            IF oB:lIndexed
                lDone := .T.
                oCur := oB
            ENDIF
        ENDIF
    NEXT
    IF lPaint
        IF lDone
            ce_RevealPaged( oCur )
        ELSE
            ce_Status()
            IF s_oEdit != NIL .AND. oCur != NIL .AND. oCur:lPaged .AND. ! oCur:lIndexed
                s_oEdit:refresh( .T. )
            ENDIF
        ENDIF
    ENDIF

RETURN

/* after Idx 100%: put the caret on line 1 and force a real paint of the text */
STATIC FUNCTION ce_FmtSize( n )
    IF n >= 1024 * 1024 * 1024
        RETURN hb_ntos( Int( n / ( 1024 * 1024 * 1024 ) ) ) + "GB"
    ELSEIF n >= 1024 * 1024
        RETURN hb_ntos( Int( n / ( 1024 * 1024 ) ) ) + "MB"
    ELSEIF n >= 1024
        RETURN hb_ntos( Int( n / 1024 ) ) + "KB"
    ENDIF
RETURN hb_ntos( Int( n ) ) + "B"

STATIC PROCEDURE ce_RevealPaged( oB )

    LOCAL c1, nH, nW

    IF oB == NIL .OR. ! oB:lPaged
        RETURN
    ENDIF
    oB:hCache := { => }
    oB:aCache := {}
    oB:nRow := 1
    oB:nCol := 1
    oB:nTop := 1
    oB:nLeft := 0
    IF s_oApp != NIL
        ht_cuiSetupDisplay( s_oApp:rows(), s_oApp:cols() )
    ENDIF
    IF s_oMain != NIL
        s_oMain:fitToScreen()
        s_oMain:show()
    ENDIF
    ce_Arrange()
    IF s_oEdit != NIL
        s_oEdit:setBuffer( oB )
        s_oEdit:refresh( .F. )
        s_oEdit:setFocus()
    ENDIF
    IF s_oMain != NIL
        s_oMain:repaint()
    ENDIF
    ce_Status()
    c1 := oB:line( 1 )
    /* NULs / controls are invisible in the GT — show a readable preview */
    c1 := StrTran( StrTran( c1, Chr( 0 ), "." ), Chr( 9 ), " " )
    nH := IIF( s_oEdit != NIL, s_oEdit:height, 0 )
    nW := IIF( s_oEdit != NIL, s_oEdit:width, 0 )
    HTToast():show( ;
       "Idx OK Ln " + hb_ntos( oB:lineCount() ) + ;
       " br " + hb_ntos( oB:nBreaks ) + ;
       " sz " + ce_FmtSize( oB:nDocLen ) + ;
       " " + hb_ntos( nW ) + "x" + hb_ntos( nH ) + ;
       " [" + Left( c1, 40 ) + "]", ;
       6000, HT_CLR_TOAST_INFO )

RETURN

STATIC PROCEDURE ce_InputLang()

    LOCAL cLang := ht_inputLanguage()

    IF ! cLang == s_cLang
        s_cLang := cLang
        ce_Status()
    ENDIF

RETURN

/* Harbour codepage -> its usual name */
STATIC FUNCTION ce_CPName( cCP )
    LOCAL n := AScan( ce_CPList(), {| a | a[ 1 ] == cCP } )
RETURN IIF( n > 0, ce_CPList()[ n ][ 2 ], cCP )

/* the codepages of files: { Harbour codepage, name in the status line, name in the lists };
   one per character table (the language of a Harbour codepage does not change the text) */
STATIC FUNCTION ce_CPList()
RETURN { { "UTF8",   "UTF-8",       "UTF-8 (Unicode)" }, ;
         { "SL852",  "CP852",       "CP852 - DOS Latin 2 (Harbour DBF)" }, ;
         { "SLWIN",  "CP1250",      "CP1250 - Windows Latin 2" }, ;
         { "SLISO",  "ISO-8859-2",  "ISO-8859-2 - Latin 2" }, ;
         { "SRWIN",  "CP1251",      "CP1251 - Windows Cyrillic" }, ;
         { "RU866",  "CP866",       "CP866 - DOS Cyrillic" }, ;
         { "RUKOI8", "KOI8-R",      "KOI8-R - Russian" }, ;
         { "UAKOI8", "KOI8-U",      "KOI8-U - Ukrainian" }, ;
         { "UA1125", "CP1125",      "CP1125 - DOS Ukrainian" }, ;
         { "RUISO",  "ISO-8859-5",  "ISO-8859-5 - Cyrillic" }, ;
         { "BGMIK",  "MIK",         "MIK - DOS Bulgarian" }, ;
         { "EN",     "CP437",       "CP437 - DOS US" }, ;
         { "DE850",  "CP850",       "CP850 - DOS Latin 1" }, ;
         { "DE858",  "CP858",       "CP858 - DOS Latin 1 + euro" }, ;
         { "DEWIN",  "CP1252",      "CP1252 - Windows Latin 1" }, ;
         { "DEISO",  "ISO-8859-1",  "ISO-8859-1 - Latin 1" }, ;
         { "SVISO",  "ISO-8859-15", "ISO-8859-15 - Latin 9 (euro)" }, ;
         { "PT860",  "CP860",       "CP860 - DOS Portuguese" }, ;
         { "IS861",  "CP861",       "CP861 - DOS Icelandic" }, ;
         { "DK865",  "CP865",       "CP865 - DOS Nordic" }, ;
         { "EL737",  "CP737",       "CP737 - DOS Greek" }, ;
         { "ELWIN",  "CP1253",      "CP1253 - Windows Greek" }, ;
         { "ELISO",  "ISO-8859-7",  "ISO-8859-7 - Greek" }, ;
         { "TR857",  "CP857",       "CP857 - DOS Turkish" }, ;
         { "TRWIN",  "CP1254",      "CP1254 - Windows Turkish" }, ;
         { "TRISO",  "ISO-8859-9",  "ISO-8859-9 - Turkish" }, ;
         { "EE775",  "CP775",       "CP775 - DOS Baltic" }, ;
         { "LTWIN",  "CP1257",      "CP1257 - Windows Baltic" }, ;
         { "HE862",  "CP862",       "CP862 - DOS Hebrew" }, ;
         { "HEWIN",  "CP1255",      "CP1255 - Windows Hebrew" }, ;
         { "CSKAMC", "Kamenicky",   "Kamenicky - DOS Czech / Slovak" }, ;
         { "PLMAZ",  "Mazovia",     "Mazovia - DOS Polish" }, ;
         { "CP950",  "Big5",        "CP950 / Big5 - Traditional Chinese" }, ;
         { "GBK",    "GBK",         "CP936 / GBK - Simplified Chinese" }, ;
         { "SR646",  "YUSCII",      "YUSCII - ISO-646 (7 bit)" }, ;
         { "SR646C", "YUSCII-C",    "YUSCII - ISO-646 Cyrillic (7 bit)" } }

/* the codepage of files that are not UTF-8: the setting, or "AUTO" */
STATIC FUNCTION ce_DefCP()
RETURN IIF( s_hSet[ "codepage" ] == "AUTO", ce_AutoCP(), s_hSet[ "codepage" ] )

/* "AUTO": the Chinese codepage of the system (Windows ANSI codepage 950 / 936,
   Linux LANG zh_TW.Big5 / zh_CN.GBK), else CP852 (DOS / Clipper DBF) */
STATIC FUNCTION ce_AutoCP()

    STATIC s_cAuto
    LOCAL cOS

    IF s_cAuto == NIL
        cOS := hb_cdpOS()
        DO CASE
        CASE HB_ISSTRING( cOS ) .AND. ( Upper( cOS ) == "CP950" .OR. Upper( cOS ) == "BIG5" )
            s_cAuto := "CP950"
        CASE HB_ISSTRING( cOS ) .AND. Upper( cOS ) == "GBK"
            s_cAuto := "GBK"
        OTHERWISE
            s_cAuto := "SL852"
        ENDCASE
    ENDIF

RETURN s_cAuto

/*----------------------------------------------------------- buffers */

STATIC FUNCTION ce_Cur()
RETURN IIF( s_nBuf >= 1 .AND. s_nBuf <= Len( s_aBufs ), s_aBufs[ s_nBuf ], NIL )

STATIC PROCEDURE ce_SwitchTo( n )
    IF n >= 1 .AND. n <= Len( s_aBufs )
        s_nBuf := n
        s_oEdit:setBuffer( s_aBufs[ n ] )
        hb_gtInfo( HB_GTI_WINTITLE, "CEdit - " + s_aBufs[ n ]:title() )
        s_oEdit:refresh()
        s_oTabs:refresh()
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
    IF hb_FileExists( cFile ) .AND. hb_FSize( cFile ) > CE_MEM_LIMIT
        HTToast():show( "Opening large file...", 2000, HT_CLR_TOAST_INFO )
    ENDIF
    IF ! oB:load( cFile, ce_DefCP() )
        HTMessageBox():error( "CEdit", IIF( Empty( oB:cError ), "Cannot open " + cFile, oB:cError ) )
        RETURN .F.
    ENDIF
    IF oB:lPaged
        oB:lHilite := .F.
        oB:cSyntax := ""
    ELSE
        oB:cSyntax := ce_SyntaxFor( cFile )
    ENDIF
    n := AScan( s_aRecent, {| a | ce_SameFile( a[ 1 ], cFile ) } )
    IF n > 0
        oB:nRow := Max( 1, Min( s_aRecent[ n ][ 2 ], oB:lineCount() ) )
        oB:nCol := Max( 1, Min( s_aRecent[ n ][ 3 ], Len( oB:line( oB:nRow ) ) + 1 ) )
        oB:nTop := Max( 1, oB:nRow - 5 )
    ENDIF
    /* empty Untitled (not a paged buffer) gives its place */
    IF oCur != NIL .AND. Empty( oCur:cFile ) .AND. Empty( oCur:cName ) .AND. ;
       ! oCur:isModified() .AND. ! oCur:lPaged .AND. ;
       oCur:lineCount() == 1 .AND. Len( oCur:aLines ) >= 1 .AND. oCur:aLines[ 1 ] == ""
        s_aBufs[ s_nBuf ]:closeFile()
        s_aBufs[ s_nBuf ] := oB
    ELSE
        AAdd( s_aBufs, oB )
        s_nBuf := Len( s_aBufs )
    ENDIF
    ce_AddRecent( oB )
    ce_SwitchTo( s_nBuf )
    ce_Status()

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

    LOCAL n, nCur := s_nBuf

    FOR n := 1 TO Len( s_aBufs )
        IF s_aBufs[ n ]:isModified() .AND. ! s_aBufs[ n ]:lReadOnly
            ce_SwitchTo( n )
            IF ! ce_AskSave( s_aBufs[ n ] )
                RETURN .F.
            ENDIF
        ENDIF
    NEXT
    /* all open files to the recent ones, the file worked on last at the top */
    FOR n := 1 TO Len( s_aBufs )
        IF n != nCur
            ce_AddRecent( s_aBufs[ n ] )
        ENDIF
    NEXT
    IF nCur >= 1 .AND. nCur <= Len( s_aBufs )
        ce_AddRecent( s_aBufs[ nCur ] )
    ENDIF

RETURN .T.

/* F2 / Shift-F2 */
STATIC FUNCTION ce_SaveBuffer( oB, lAs )

    LOCAL cFile

    IF oB == NIL .OR. ( oB:lReadOnly .AND. ! lAs )
        RETURN .F.
    ENDIF
    cFile := oB:cFile
    IF lAs .OR. Empty( cFile )
        cFile := ce_FileDialog( "Save as", IIF( Empty( cFile ), "", cFile ), .T. )
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
    IF ce_SameFile( cFile, ce_LangPath() )         /* cedit.lang: the languages again */
        ce_LangReload()
    ENDIF
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

/* Ctrl-O / Ctrl-F4 */
STATIC PROCEDURE ce_CmdOpen()

    LOCAL oB := ce_Cur(), cFile, cStart

    cStart := IIF( oB != NIL .AND. ! Empty( oB:cFile ), hb_FNameDir( oB:cFile ), hb_cwd() )
    cFile := ce_FileDialog( "Open file", cStart, .F. )
    IF Empty( cFile )
        RETURN
    ENDIF
    /* native GetOpenFileName can leave the GT viewport wrong */
    IF s_oMain != NIL
        s_oMain:fitToScreen()
    ENDIF
    ce_Arrange()
    IF ! ce_OpenFile( cFile )
        RETURN
    ENDIF
    oB := ce_Cur()
    IF oB != NIL .AND. oB:lPaged .AND. oB:lIndexed
        ce_RevealPaged( oB )
    ELSE
        ce_Status()
        s_oEdit:refresh( .F. )
        s_oEdit:setFocus()
    ENDIF
    HTToast():show( "Opened " + hb_FNameNameExt( cFile ), 1500, HT_CLR_TOAST_INFO )

RETURN

/* F4: the files opened lately, and "Open file..." */
STATIC PROCEDURE ce_CmdOpenRecent()

    LOCAL aItems := { "Open file...                  Ctrl-O" }, n

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

    LOCAL dlg, oFind, oRepl, oCase, oWord, oAsk, nW := 64, nH := IIF( lReplace, 12, 9 ), nRow
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
    cText := ce_InputDialog( "Go to", "Line[,column]:", "" )
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

/* cedit.lang: languages in HbEdit's format (hbedit.ini [LANG_xxx]), next to cedit.ini */
STATIC FUNCTION ce_LangPath()
RETURN hb_FNameDir( ce_IniPath() ) + "cedit.lang"

/* View - Language definitions: opens cedit.lang (a new one with an example);
   saving it reads the languages again */
STATIC PROCEDURE ce_CmdLangFile()

    LOCAL cFile := ce_LangPath()

    IF ! hb_FileExists( cFile )
        hb_DirBuild( hb_FNameDir( cFile ) )
        hb_MemoWrit( cFile, ce_LangSample() )
    ENDIF
    ce_OpenFile( cFile )

RETURN

STATIC PROCEDURE ce_LangReload()

    LOCAL oB, n := ce_SyntaxLoad( ce_LangPath() )

    FOR EACH oB IN s_aBufs
        IF ! Empty( oB:cFile ) .AND. ! oB:lReadOnly
            oB:cSyntax := ce_SyntaxFor( oB:cFile )
            oB:nStateValid := 1
        ENDIF
    NEXT
    HTToast():show( "Languages from cedit.lang: " + hb_ntos( n ), 2000, HT_CLR_TOAST_SUCCESS )

RETURN

STATIC FUNCTION ce_LangSample()

    LOCAL cEol := hb_eol()

RETURN ;
    "; CEdit languages - the format of HbEdit's hbedit.ini (Alexander S. Kresin):" + cEol + ;
    "; its [LANG_xxx] sections can be copied here, or this file can be hbedit.ini." + cEol + ;
    ";" + cEol + ;
    ";   ext=         extensions (.prg.ch)" + cEol + ;
    ";   keywords1=   .. keywords9=  words in the keyword color" + cEol + ;
    ";   scomm=       comment to the end of the line (//); a word (rem) = a comment line" + cEol + ;
    ";   mcomm=       block comment (/* */); < > = HTML / XML tags; [ ] = INI sections" + cEol + ;
    ";   startline=   preprocessor / label mark at the line start (#)" + cEol + ;
    ";   case=        on | off (keywords in any case)" + cEol + ;
    ";   quotes=      string quotes (default: double and single quote)" + cEol + ;
    ";   CEdit only:  title=  first= (comment lines: *)  dotwords= (.AND. .T.)" + cEol + ;
    ";                escape= on | off   base= harbour | c | sql | python | shell | bat | ini | html | none" + cEol + ;
    ";" + cEol + ;
    "; A language for the extensions of a built-in one (.prg, .c, ...) adds to it." + cEol + ;
    "; Saving this file in CEdit reads it again." + cEol + ;
    cEol + ;
    "[LANG_LISP]" + cEol + ;
    "ext=.lisp.cl.el" + cEol + ;
    "keywords1=defun defmacro defvar defparameter defconstant lambda let let* flet labels progn block" + ;
    " return-from if when unless cond case and or not loop do dolist dotimes setq setf push pop" + cEol + ;
    "keywords2=car cdr cons list append mapcar apply funcall format print eq eql equal null nil t" + cEol + ;
    "scomm=;" + cEol + ;
    "case=off" + cEol + ;
    "quotes=" + Chr( 34 ) + cEol + ;
    cEol + ;
    "[LANG_PAS]" + cEol + ;
    "title=Pascal, Delphi" + cEol + ;
    "ext=.pas.pp.dpr.inc" + cEol + ;
    "keywords1=and array begin case const div do downto else end file for function goto if implementation" + ;
    " in interface label mod nil not of or packed procedure program record repeat set then to type unit" + ;
    " until uses var while with class object private public property constructor destructor" + cEol + ;
    "keywords2=boolean byte char integer longint real string word true false" + cEol + ;
    "scomm=//" + cEol + ;
    "mcomm={ }" + cEol + ;
    "case=off" + cEol + ;
    "quotes='" + cEol + ;
    "escape=off" + cEol

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

/* The colors of the text: the theme's, or a classic editor look:
   { text, selection, name, syntax colors (NIL = chosen for the theme's
   palette), theme to load with them ("" = keep the theme) } */
STATIC FUNCTION ce_ColorSets()
RETURN { { "",      "",     "Theme colors", NIL, "" }, ;
         { "14/1",  "1/7",  "Turbo Pascal (yellow on blue)", ;
           { "14/1", "15/1", "11/1", "7/1", "11/1", "10/1" }, "Turbo Pascal" }, ;
         { "W+/B",  "N/BG", "White on blue (HbEdit, Norton)", NIL, "" }, ;
         { "GR+/B", "B/W",  "Yellow on blue", NIL, "" }, ;
         { "W/N",   "N/W",  "Gray on black", NIL, "" }, ;
         { "N/W*",  "W+/B", "Black on white", NIL, "" } }

STATIC PROCEDURE ce_SetEditorColors( cColor )
    LOCAL a := ce_ColorSets(), n := AScan( a, {| x | x[ 1 ] == cColor } )
    n := Max( 1, n )
    s_oEdit:cColor := a[ n ][ 1 ]
    s_oEdit:cSelColor := a[ n ][ 2 ]
    s_oEdit:aSyntaxColors := a[ n ][ 4 ]
RETURN

STATIC PROCEDURE ce_CmdColors()

    LOCAL a := ce_ColorSets(), aItems := {}, n

    AEval( a, {| x | AAdd( aItems, x[ 3 ] ) } )
    n := ce_ListDialog( "Editor colors", aItems, Max( 1, AScan( a, {| x | x[ 1 ] == s_oEdit:cColor } ) ) )
    IF n > 0
        s_hSet[ "colors" ] := a[ n ][ 1 ]
        ce_SetEditorColors( a[ n ][ 1 ] )
        /* Turbo Pascal: its yellow, white and light cyan need the VGA palette */
        IF ! Empty( a[ n ][ 5 ] ) .AND. HTTheme():hasTheme( a[ n ][ 5 ] )
            HTTheme():loadTheme( a[ n ][ 5 ] )
            s_hSet[ "theme" ] := a[ n ][ 5 ]
            HTApplication():repaintAll()
        ENDIF
        ce_SaveSettings()
    ENDIF

RETURN

/* the Turbo Pascal 7 look: the VGA palette, menus black on light gray with
   red hot keys and the chosen entry on green, light gray dialogs, green
   buttons, a blue desktop (made from HighContrast, which has the VGA palette) */
STATIC PROCEDURE ce_TurboTheme()

    LOCAL a, aPal

    IF HTTheme():hasTheme( "Turbo Pascal" ) .OR. ! HTTheme():hasTheme( "HighContrast" )
        RETURN
    ENDIF
    a := AClone( HTTheme():themeColors( "HighContrast" ) )
    aPal := { 0, 8388608, 32768, 8421376, 128, 8388736, 32896, 12632256, ;
              8421504, 16711680, 65280, 16776960, 255, 16711935, 65535, 16777215 }
    AEval( { HT_CLR_MENU_BAR, HT_CLR_MENU_ITEM, HT_CLR_MENU_SEP, HT_CLR_CTXMENU_NORMAL, HT_CLR_CTXMENU_SEP, ;
             HT_CLR_STATUSBAR, HT_CLR_WINDOW, HT_CLR_LABEL, HT_CLR_FRAME, HT_CLR_GET_LABEL, HT_CLR_CHECK_NORMAL, ;
             HT_CLR_TAB_INACTIVE, HT_CLR_TAB_BAR }, {| n | a[ n ] := "0/7" } )
    AEval( { HT_CLR_MENU_BAR_SEL, HT_CLR_MENU_ITEM_SEL, HT_CLR_CTXMENU_SELECTED, HT_CLR_BUTTON_NORMAL, ;
             HT_CLR_TAB_ACTIVE }, {| n | a[ n ] := "0/2" } )
    a[ HT_CLR_MENU_HOTKEY ]     := "4/7"
    a[ HT_CLR_MENU_HOTKEY_SEL ] := "4/2"
    a[ HT_CLR_BUTTON_FOCUSED ]  := "15/2"
    a[ HT_CLR_CHECK_FOCUSED ]   := "15/7"
    a[ HT_CLR_TITLEBAR ]        := "15/7"
    a[ HT_CLR_TITLEBTN ]        := "15/7"
    AEval( { HT_CLR_GET_NORMAL, HT_CLR_LINEEDIT_NORMAL, HT_CLR_COMBO_NORMAL, HT_CLR_SPINNER_NORMAL }, {| n | a[ n ] := "15/1" } )
    AEval( { HT_CLR_GET_FOCUSED, HT_CLR_LINEEDIT_FOCUSED, HT_CLR_COMBO_FOCUSED, HT_CLR_SPINNER_FOCUSED }, {| n | a[ n ] := "14/1" } )
    a[ HT_CLR_LINEEDIT_SELECTED ] := "1/7"
    AEval( { HT_CLR_LIST_NORMAL, HT_CLR_CHECKLIST_NORMAL, HT_CLR_COMBO_DROP_N }, {| n | a[ n ] := "0/3" } )
    AEval( { HT_CLR_LIST_SELECTED, HT_CLR_CHECKLIST_SELECTED, HT_CLR_COMBO_DROP_S }, {| n | a[ n ] := "15/1" } )
    a[ HT_CLR_DESKTOP ] := "7/1"
    HTTheme():addTheme( "Turbo Pascal", a, aPal )

RETURN

/* tab size, Tab with spaces, highlighting, codepage of non-UTF-8 files */
STATIC PROCEDURE ce_CmdOptions()

    LOCAL dlg, oTab, oSpaces, oHilite, oCP, aCP := ce_CPList(), a, nW := 64, nH := 11, oB

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
    oCP:setGeometry( 26, 5, 36, 1 )
    oCP:addItem( "Automatic (" + ce_CPName( ce_AutoCP() ) + ")", "AUTO" )
    FOR EACH a IN aCP
        IF !( a[ 1 ] == "UTF8" )
            oCP:addItem( a[ 3 ], a[ 1 ] )
        ENDIF
    NEXT
    oCP:setCurrentIndex( IIF( s_hSet[ "codepage" ] == "AUTO", 1, Max( 1, AScan( aCP, {| x | x[ 1 ] == s_hSet[ "codepage" ] } ) ) ) )
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
                "codepage" => "AUTO", "hilite" => .T., "insert" => .T., "tabs" => .T. }
    s_cIni := ce_IniPath()
    IF ! hb_FileExists( s_cIni )
        RETURN
    ENDIF
    hIni := hb_iniRead( s_cIni, .F., "=", .F. )
    IF ! HB_ISHASH( hIni )
        RETURN
    ENDIF
    h := hb_HGetDef( hIni, "OPTIONS", { => } )
    hb_HCaseMatch( h, .F. )          /* hb_iniRead() gives the keys in upper case */
    s_hSet[ "theme" ]     := hb_HGetDef( h, "theme", s_hSet[ "theme" ] )
    s_hSet[ "colors" ]    := hb_HGetDef( h, "colors", "" )
    s_hSet[ "tab" ]       := Max( 1, Min( 16, Val( hb_HGetDef( h, "tab", "4" ) ) ) )
    s_hSet[ "tabspaces" ] := !( hb_HGetDef( h, "tabspaces", "1" ) == "0" )
    s_hSet[ "codepage" ]  := hb_HGetDef( h, "codepage", "AUTO" )
    s_hSet[ "hilite" ]    := !( hb_HGetDef( h, "hilite", "1" ) == "0" )
    s_hSet[ "insert" ]    := !( hb_HGetDef( h, "insert", "1" ) == "0" )
    s_hSet[ "tabs" ]      := !( hb_HGetDef( h, "tabs", "1" ) == "0" )
    /* CP852 was the fixed default before "AUTO": on a Chinese system it was
       never chosen, so it becomes "AUTO" (Big5 / GBK) */
    IF AScan( ce_CPList(), {| x | x[ 1 ] == s_hSet[ "codepage" ] } ) < 2 .OR. ;
       ( s_hSet[ "codepage" ] == "SL852" .AND. !( ce_AutoCP() == "SL852" ) )
        s_hSet[ "codepage" ] := "AUTO"
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
    h[ "tabs" ]      := IIF( s_hSet[ "tabs" ], "1", "0" )
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
    "Esc or F10 closes this help. F7 / Ctrl-F searches in it.", ;
    "", ;
    "GETTING STARTED", ;
    "  cedit file.prg        opens a file (several files: cedit a.prg b.ch)", ;
    [  cedit                 an empty "Untitled"; F2 asks for the name], ;
    "  F9           the menu: cursor keys and Enter, or Alt + the red letter", ;
    "               (Alt-F = File ...), or the mouse. Every command shows its", ;
    "               key on the right: the menu is the reference.", ;
    "  Esc                   closes a menu or a dialog, cancels a selection,", ;
    "                        then closes the file (the last one ends CEdit)", ;
    "  A changed file has * after its name in the status line; before it is", ;
    "  closed CEdit asks: Save / Don't save / Cancel.", ;
    "", ;
    "FILES", ;
    "  F2           save                    Shift-F2       save as", ;
    "  F4           open: recent files      Ctrl-O         open: choose a file", ;
    "  Shift-F4     new file                F12            list of open files", ;
    "  Ctrl-Tab     next open file          Shift-Tab      previous open file", ;
    "  F10, Esc     close the file          Alt-X          exit (asks for changes)", ;
    "  A recent file opens at the line and column where it was closed.", ;
    "", ;
    "TABS", ;
    "  Every open file has a tab under the menu (View - Tabs: on / off).", ;
    "  Click on a tab: that file; click on x (on the active tab): close it", ;
    "  (F10 / Esc close the file too). Ctrl-Tab / Shift-Tab: next / previous.", ;
    "  * after a name: the file is changed. < > at the ends: more tabs.", ;
    "", ;
    "MOVING", ;
    "  Cursor keys, PgUp / PgDn, the mouse wheel", ;
    "  Home         first text of the line; again: the line start", ;
    "  End          end of the line", ;
    "  Ctrl-Left / Ctrl-Right    word left / right", ;
    "  Ctrl-Home / Ctrl-End      start / end of the text", ;
    "  Ctrl-PgUp / Ctrl-PgDn     top / bottom of the screen", ;
    "  Ctrl-Up / Ctrl-Down       scroll one line, the cursor stays", ;
    [  Alt-F8       go to a line: "120", or line and column: "120,15"], ;
    "  Ctrl-B       the matching bracket of ( [ { at the cursor", ;
    "  Alt-B        back to where the cursor was before a jump (go to, search)", ;
    "", ;
    "EDITING", ;
    "  Ins          insert / overwrite (status line: Ins / Ovr)", ;
    "  Enter        new line, it keeps the indent of the line above", ;
    "  Tab          spaces up to the next tab stop (View - Options: tab size,", ;
    "               or a real Tab character); with several lines selected:", ;
    "               indents them", ;
    "  Backspace, Del    delete a character, or the selection", ;
    "  Ctrl-Y       delete the line", ;
    "  Ctrl-Z, Alt-Backspace     undo", ;
    "  Ctrl-R       redo", ;
    "  Edit - Upper case / Lower case: the selected text", ;
    "", ;
    "SELECTION AND CLIPBOARD", ;
    "  Shift + cursor keys       select", ;
    "  Mouse        drag to select; double click: a word; Shift + click: extend", ;
    "  F3           block: then the cursor keys select, F3 again ends it", ;
    "  Ctrl-F3      vertical block (columns), the same way", ;
    "  Ctrl-A       select all                Esc        cancel the selection", ;
    "  Ctrl-C, Ctrl-Ins          copy", ;
    "  Ctrl-X, Shift-Del         cut", ;
    "  Ctrl-V, Shift-Ins         paste (a vertical block goes in as a block)", ;
    "  The clipboard is the one of the system: copy in CEdit, paste anywhere.", ;
    "", ;
    "SEARCH AND REPLACE", ;
    [  F7, Ctrl-F   find: the text, "Case sensitive", "Whole words".], ;
    "               The match is selected.", ;
    "  Shift-F7     next match              Alt-F7     previous match", ;
    "  Alt-8        the word at the cursor, next; Alt-3: previous", ;
    [  Ctrl-F7, Ctrl-H   replace. With "Ask before each one" CEdit asks for], ;
    "               every match: Yes (replace it) / No (skip it) / All (the", ;
    "               rest without asking) / Cancel; without it all are replaced.", ;
    "               Ctrl-Z undoes the replacements.", ;
    [  At the end of the text the search goes on from the start ("Searched on], ;
    [  from the start" is shown).], ;
    "", ;
    "VIEW", ;
    "  F8           syntax highlighting on / off", ;
    "  View - Language        the language of the file (by the extension:", ;
    "                         .prg .c .sql .py .sh .bat .ini .html ...)", ;
    "  View - Language definitions   more languages, file cedit.lang", ;
    "  Shift-F8     codepage: an unchanged file is read again in it,", ;
    "               a changed one is saved in it (e.g. CP852 -> UTF-8)", ;
    "  View - Theme           the colors of menus and dialogs", ;
    "  View - Editor colors   the colors of the text: the theme's, Turbo", ;
    "                         Pascal (yellow on blue), white on blue, ...", ;
    "  View - Options         tab size, Tab as spaces, highlighting, the", ;
    "                         codepage of files that are not UTF-8", ;
    "", ;
    "FILES AND CODEPAGES", ;
    "  Text is edited in UTF-8 (all languages, also Chinese, Hindi ...).", ;
    "  A file that is not valid UTF-8 is read and saved in CP852 (DOS / Clipper", ;
    "  DBF Latin 2), on a Chinese Windows in Big5 (CP950) or GBK (CP936);", ;
    "  View - Options chooses another one (CP1250, CP1251, Big5, ...).", ;
    [  The status line shows it: "UTF-8", "CP852", "Big5"; "LF" = Linux line ends.], ;
    "  Line ends (CRLF Windows / LF Linux), the last line end and a UTF-8 BOM", ;
    "  stay as they were.", ;
    "", ;
    "SETTINGS", ;
    "  Options, theme, colors and the recent files are saved when CEdit ends:", ;
    "    " + s_cIni, ;
    "  Languages: " + ce_LangPath(), ;
    "", ;
    "FOR USERS OF HBEDIT", ;
    "  The F-keys are those of HbEdit (Alexander S. Kresin): F2, F3, F4, F7,", ;
    "  F8, F9, F10, F12, Alt-F7, Alt-F8, Ctrl-Y ... CEdit is written anew on", ;
    "  Harbour-CUI; the [LANG_xxx] sections of hbedit.ini work in cedit.lang." }, Chr( 10 ) )

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
    dlg:addButtonBar( "OK", "Cancel" )
    IF dlg:exec() == HT_DIALOG_ACCEPTED .AND. nResult == 0
        nResult := oList:currentIndex()
    ENDIF

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

/* returns full path, "" = cancelled; lSave=.T. for Save as */
STATIC FUNCTION ce_FileDialog( cTitle, cStart, lSave )

    LOCAL cDir := hb_FNameDir( cStart ), cName := hb_FNameNameExt( cStart ), cFile
#ifdef __PLATFORM__WINDOWS
    LOCAL nFlags, aFilter
#endif

    lSave := hb_defaultValue( lSave, .F. )
    IF Empty( cDir ) .OR. ! hb_DirExists( cDir )
        cDir := hb_cwd()
    ENDIF
    cDir := hb_DirSepAdd( hb_DirSepToOS( cDir ) )

#ifdef __PLATFORM__WINDOWS
    /* native Windows dialog — the CUI list dialog was returning "" for users */
    nFlags := hb_bitOr( WIN_OFN_EXPLORER, WIN_OFN_HIDEREADONLY, WIN_OFN_NOCHANGEDIR )
    aFilter := { { "All files", "*.*" }, { "Text", "*.txt;*.prg;*.ch;*.c;*.h;*.md;*.log" } }
    IF lSave
        nFlags := hb_bitOr( nFlags, WIN_OFN_OVERWRITEPROMPT, WIN_OFN_PATHMUSTEXIST )
        cFile := win_GetSaveFileName( @nFlags, cTitle, cDir, NIL, aFilter, NIL, NIL, cName )
    ELSE
        nFlags := hb_bitOr( nFlags, WIN_OFN_FILEMUSTEXIST, WIN_OFN_PATHMUSTEXIST )
        cFile := win_GetOpenFileName( @nFlags, cTitle, cDir, NIL, aFilter, NIL, NIL, cName )
    ENDIF
    IF ! Empty( cFile )
        RETURN hb_DirSepToOS( cFile )
    ENDIF
    RETURN ""
#else
    RETURN ce_FileDialogCui( cTitle, cDir, cName, lSave )
#endif

#ifndef __PLATFORM__WINDOWS
/* CUI fallback (Linux) */
STATIC FUNCTION ce_FileDialogCui( cTitle, cDir, cName, lSave )

    LOCAL dlg, oLbl, oName, oList, aBtn, nW, nH, cResult := ""
    LOCAL aState, bTake, bOk

    HB_SYMBOL_UNUSED( lSave )
    cDir := hb_DirSepAdd( hb_DirSepToOS( cDir ) )
    aState := { cDir, "", "" }   /* dir, pick, result — avoid codeblock capture quirks */

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

    bTake := {| cTxt, cPath |
        cTxt := AllTrim( IIF( HB_ISSTRING( cTxt ), cTxt, "" ) )
        IF Empty( cTxt )
            RETURN NIL
        ENDIF
        cPath := IIF( ce_IsAbsolute( cTxt ), hb_DirSepToOS( cTxt ), aState[ 1 ] + cTxt )
        IF cTxt == ".." .OR. hb_DirExists( cPath )
            aState[ 1 ] := IIF( cTxt == "..", ce_ParentDir( aState[ 1 ] ), hb_DirSepAdd( hb_DirSepToOS( cPath ) ) )
            aState[ 2 ] := ce_FillDir( oList, oLbl, oName, aState[ 1 ] )
            dlg:repaint()
        ELSE
            aState[ 3 ] := cPath
            dlg:accept()
        ENDIF
        RETURN NIL
    }
    bOk := {||
        aState[ 3 ] := ce_DialogChoice( aState[ 1 ], oName:text, oList, aState[ 2 ] )
        IF Empty( aState[ 3 ] )
            HTMessageBox():information( cTitle, "Select a file, or type its name." )
        ELSEIF hb_DirExists( aState[ 3 ] )
            HTMessageBox():information( cTitle, "That is a folder. Select a file, or type its name." )
            aState[ 3 ] := ""
        ELSE
            dlg:accept()
        ENDIF
        RETURN NIL
    }
    oName:bEnter := {|| Eval( bTake, oName:text ) }
    oList:onActivated := {|| Eval( bTake, oList:currentData() ) }
    oList:onChanged := {||
        aState[ 2 ] := IIF( HB_ISSTRING( oList:currentData() ), oList:currentData(), "" )
        IF ! Empty( aState[ 2 ] ) .AND. !( aState[ 2 ] == ".." ) .AND. ! hb_DirExists( aState[ 1 ] + aState[ 2 ] )
            oName:setText( aState[ 2 ] )
        ELSE
            oName:setText( "" )
            aState[ 2 ] := ""
        ENDIF
        dlg:repaintChild( oName )
        RETURN NIL
    }

    aState[ 2 ] := ce_FillDir( oList, oLbl, oName, aState[ 1 ] )
    IF ! Empty( cName )
        oName:setText( cName )
        aState[ 2 ] := cName
    ENDIF
    aBtn := dlg:addButtonBar( "OK", "Cancel" )
    aBtn[ 1 ]:onClicked := bOk

    IF dlg:exec() == HT_DIALOG_ACCEPTED
        cResult := aState[ 3 ]
    ENDIF

RETURN cResult

STATIC FUNCTION ce_DialogChoice( cDir, cName, oList, cPick )

    LOCAL cTxt := AllTrim( IIF( HB_ISSTRING( cName ), cName, "" ) ), cData, cPath

    IF ! Empty( cTxt )
        cPath := IIF( ce_IsAbsolute( cTxt ), hb_DirSepToOS( cTxt ), cDir + cTxt )
        IF ! hb_DirExists( cPath )
            RETURN cPath
        ENDIF
    ENDIF
    cData := IIF( ! Empty( cPick ), cPick, oList:currentData() )
    cData := IIF( HB_ISSTRING( cData ), cData, "" )
    IF ! Empty( cData ) .AND. !( cData == ".." ) .AND. ! hb_DirExists( cDir + cData )
        RETURN cDir + cData
    ENDIF

RETURN ""

STATIC FUNCTION ce_FillDir( oList, oLbl, oName, cDir )

    LOCAL aDirs := {}, aFiles := {}, a, nFirstFile := 0, cPick := ""

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
        IF nFirstFile == 0
            nFirstFile := oList:count() + 1
        ENDIF
        oList:addItem( ht_cellPadR( a[ 1 ], Max( 10, oList:width - 14 ) ) + Str( a[ 2 ], 12 ), a[ 1 ] )
    NEXT
    oList:setCurrentIndex( IIF( nFirstFile > 0, nFirstFile, 1 ) )
    IF nFirstFile > 0
        cPick := aFiles[ 1 ][ 1 ]
        IF oName != NIL
            oName:setText( cPick )
        ENDIF
    ELSEIF oName != NIL
        oName:setText( "" )
    ENDIF
    oLbl:setText( cDir )

RETURN cPick

STATIC FUNCTION ce_ParentDir( cDir )

    LOCAL cUp := hb_FNameDir( Left( cDir, Len( cDir ) - 1 ) )

RETURN IIF( Empty( cUp ), cDir, cUp )

STATIC FUNCTION ce_IsRoot( cDir )
RETURN Len( cDir ) <= 3 .AND. ( SubStr( cDir, 2, 1 ) == ":" .OR. cDir == hb_ps() )
#endif /* ! Windows — CUI file dialog */

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

STATIC FUNCTION ce_SameFile( c1, c2 )
#ifdef __PLATFORM__WINDOWS
RETURN ! Empty( c1 ) .AND. Lower( c1 ) == Lower( c2 )
#else
RETURN ! Empty( c1 ) .AND. c1 == c2
#endif
