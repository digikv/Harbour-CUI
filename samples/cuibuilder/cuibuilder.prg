/*
 * CuiBuilder - builds Harbour programs and libraries, in character mode
 * (Harbour-CUI); the successor of HwBuilder (Alexander S. Kresin, HwGUI)
 * for console programs
 *
 * Copyright (c) 2026 Srđan Dragojlović - dual licensed, see LICENSE
 *
 * Like HwBuilder it runs the Harbour compiler, the C compiler and the
 * linker itself (cbengine.prg), from the command templates of cuibuild.ini,
 * and builds only what changed. Projects: HwBuilder's .hwprj (read and
 * written) and hbmk2's .hbp (read). cuibc is the same without the screen.
 * Written from scratch, no HwBuilder code.
 *
 *   row 0      menu bar (F10, Alt + letter, the mouse); the project
 *   left       the source files of the project (Ins add, Del remove, Enter / F4 edit)
 *   right      the options of the project (output, compiler, GT, libraries, flags)
 *   bottom     the messages of the build (Enter: the file at the line of an error)
 *   last row   the keys
 *
 * Usage: cuibuilder [project.hwprj | project.hbp]
 */

#include "hbtui.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"
#include "cuibuild.ch"

REQUEST HB_CODEPAGE_UTF8EX
REQUEST HB_CODEPAGE_SL852

#define VK_ESCAPE       0x1B

/* the option fields: label, kind, the variable of CBProject */
#define FLD_LABEL       1
#define FLD_KIND        2
#define FLD_VAR         3
#define FLD_WIDGET      4
#define FLD_LABELW      5

STATIC s_oApp, s_oMain, s_oBar
STATIC s_hSet                           /* cuibuild.ini */
STATIC s_oPF                            /* the project file shown (read to be changed) */
STATIC s_nPart := 1                     /* the part (:project) shown */
STATIC s_lChanged := .F.
STATIC s_lLoading := .F.                /* the fields are being filled: no change */
STATIC s_lShowCmd := .F.
STATIC s_oTitle, s_oPartLbl, s_oPart, s_oFrmFiles, s_oFrmOpt, s_oFrmMsg, s_oFiles, s_oMsg, s_oKeys
STATIC s_aFld := {}
STATIC s_aMsg := {}                     /* { cText, nType, cFile, nLine } */
STATIC s_aCheck := {}                   /* menu entries with a check */

PROCEDURE Main( ... )

    LOCAL cArg, cPrj := "", aSize

    hb_cdpSelect( "UTF8EX" )
    s_hSet := cb_SettingsLoad()
    s_lShowCmd := hb_HGetDef( s_hSet[ "view" ], "showcmd", "" ) == "yes"
    IF ! hb_FileExists( s_hSet[ "ini" ] )
        cb_SettingsSave( s_hSet )
    ENDIF

    s_oApp := HTApplication():new()
    /* the screen: -size=50x160, else [VIEW] rows= cols= of cuibuild.ini (40 x 132) */
    aSize := { Val( hb_HGetDef( s_hSet[ "view" ], "rows", "40" ) ), Val( hb_HGetDef( s_hSet[ "view" ], "cols", "132" ) ) }
    FOR EACH cArg IN hb_AParams()
        IF Lower( Left( cArg, 6 ) ) == "-size="
            aSize := { Val( SubStr( cArg, 7 ) ), Val( SubStr( cArg, At( "x", Lower( cArg ) ) + 1 ) ) }
        ENDIF
    NEXT
    s_oApp:setDisplaySize( Max( 25, aSize[ 1 ] ), Max( 80, aSize[ 2 ] ) )
    ht_cuiSetupDisplay( s_oApp:rows(), s_oApp:cols() )
    hb_gtInfo( HB_GTI_WINTITLE, "CuiBuilder" )
    hb_gtInfo( HB_GTI_ICONRES, 1 )
    IF ! Empty( s_hSet[ "view" ][ "theme" ] ) .AND. HTTheme():hasTheme( s_hSet[ "view" ][ "theme" ] )
        HTTheme():loadTheme( s_hSet[ "view" ][ "theme" ] )
    ENDIF

    s_oMain := CBMain():new()
    s_oMain:setWindowTitle( "CuiBuilder" )
    s_oMain:show()

    s_oTitle := HTLabel():new( "", s_oMain )
    s_oPartLbl := HTLabel():new( "Part:", s_oMain )
    s_oPart := CBCombo():new( s_oMain )
    s_oPart:onChanged := {| n | IIF( s_lLoading, NIL, ( s_nPart := n, cb_ShowPart() ) ) }
    s_oFrmFiles := HTFrame():new( "Source files", s_oMain )
    s_oFrmOpt := HTFrame():new( "Options", s_oMain )
    s_oFrmMsg := HTFrame():new( "Messages", s_oMain )
    s_oFiles := CBFileList():new( s_oMain )
    s_oFiles:onActivated := {|| cb_EditCurrent() }
    cb_MakeFields()
    s_oMsg := CBMsgList():new( s_oMain )
    s_oMsg:onActivated := {| n | cb_MsgOpen( n ) }
    s_oKeys := HTLabel():new( cb_KeysText(), s_oMain )

    cb_Arrange()
    cb_BuildMenu()

    FOR EACH cArg IN hb_AParams()
        IF !( Left( cArg, 1 ) == "-" )
            cPrj := cArg
        ENDIF
    NEXT
    IF ! Empty( cPrj )
        IF Empty( hb_FNameExt( cPrj ) ) .AND. ! hb_FileExists( cPrj )
            cPrj += IIF( hb_FileExists( cPrj + ".hwprj" ), ".hwprj", ".hbp" )
        ENDIF
        cb_Open( cPrj )
    ELSE
        cb_ShowProject()
        cb_AddMsg( "No project. F3 opens one (.hwprj, .hbp), Project - New makes one.", CB_MSG_INFO )
        cb_AddMsg( "Settings: " + s_hSet[ "ini" ], CB_MSG_INFO )
        s_oMain:repaintChild( s_oMsg )
    ENDIF
    cb_Focus( s_oFiles )
    s_oApp:exec()

    cb_SaveView()

RETURN

/*----------------------------------------------------------- main window */

CLASS CBMain FROM HTMainWindow
    METHOD keyEvent( keyEvent )
    METHOD screenResized()
    METHOD closeEvent( closeEvent )
ENDCLASS

METHOD PROCEDURE keyEvent( keyEvent ) CLASS CBMain

    LOCAL nKey := keyEvent:key, lDone := .T.

    IF s_oBar != NIL .AND. s_oBar:isActive()
        ::super:keyEvent( keyEvent )
        RETURN
    ENDIF
    SWITCH nKey
    CASE K_F2       ; cb_Save( .F. ) ; EXIT
    CASE K_F3       ; cb_OpenDlg() ; EXIT
    CASE K_F4       ; cb_EditCurrent() ; EXIT
    CASE K_F9       ; cb_DoBuild( 1 ) ; EXIT
    CASE K_CTRL_F9  ; cb_DoBuild( 4 ) ; EXIT
    CASE K_SH_F9    ; cb_DoBuild( 2 ) ; EXIT
    CASE K_ALT_F9   ; cb_DoBuild( 3 ) ; EXIT
    CASE K_F8       ; cb_NextError( .T. ) ; EXIT
    CASE K_SH_F8    ; cb_NextError( .F. ) ; EXIT
    CASE K_F10      ; s_oBar:popup( 1 ) ; EXIT
    CASE K_ALT_X    ; cb_Quit() ; EXIT
    OTHERWISE
        lDone := .F.
    ENDSWITCH
    IF lDone
        keyEvent:accept()
        RETURN
    ENDIF
    ::super:keyEvent( keyEvent )

RETURN

METHOD PROCEDURE screenResized() CLASS CBMain
    ::super:screenResized()
    cb_Arrange()
RETURN

/* the close button: as Alt-X (asks to save) */
METHOD PROCEDURE closeEvent( closeEvent ) CLASS CBMain
    IF cb_CanClose()
        ::super:closeEvent( closeEvent )
    ENDIF
RETURN

/* places the widgets on the window */
STATIC PROCEDURE cb_Arrange()

    LOCAL nW := s_oMain:width, nH := s_oMain:height, nTop := 1, nLeft, nMsg, nBody, a, nRow, nLab := 15, nFW

    nMsg := Max( 5, Int( ( nH - 3 ) / 3 ) )
    nBody := Max( 8, nH - 2 - nTop - nMsg )
    nLeft := Max( 24, Int( nW * 0.36 ) )

    s_oTitle:setGeometry( 0, 0, nW - 34, 1 )
    s_oPartLbl:setGeometry( nW - 33, 0, 6, 1 )
    s_oPart:setGeometry( nW - 27, 0, 26, 1 )
    s_oFrmFiles:setGeometry( 0, nTop, nLeft, nBody )
    s_oFiles:setGeometry( 1, nTop + 1, nLeft - 2, nBody - 2 )
    s_oFrmOpt:setGeometry( nLeft, nTop, nW - nLeft, nBody )
    nRow := nTop + 1
    nFW := nW - nLeft - nLab - 4
    FOR EACH a IN s_aFld
        IF a[ FLD_KIND ] == "C"         /* the check boxes: two on a row */
            IF a[ FLD_VAR ] == "lGuiLib"
                a[ FLD_WIDGET ]:setGeometry( nLeft + 2, nRow, Int( ( nW - nLeft - 4 ) / 2 ), 1 )
            ELSE
                a[ FLD_WIDGET ]:setGeometry( nLeft + 2 + Int( ( nW - nLeft - 4 ) / 2 ), nRow, Int( ( nW - nLeft - 4 ) / 2 ), 1 )
                nRow++
            ENDIF
            LOOP
        ENDIF
        IF nRow > nTop + nBody - 2
            a[ FLD_WIDGET ]:setGeometry( nLeft + 1, nH, 1, 1 )      /* no room: out of sight */
            a[ FLD_LABELW ]:setGeometry( nLeft + 1, nH, 1, 1 )
            LOOP
        ENDIF
        a[ FLD_LABELW ]:setGeometry( nLeft + 2, nRow, nLab, 1 )
        a[ FLD_WIDGET ]:setGeometry( nLeft + 2 + nLab, nRow, Max( 4, nFW ), 1 )
        nRow++
    NEXT
    s_oFrmMsg:setGeometry( 0, nTop + nBody, nW, nMsg )
    s_oMsg:setGeometry( 1, nTop + nBody + 1, nW - 2, nMsg - 2 )
    s_oKeys:setGeometry( 0, nH - 2, nW, 1 )

RETURN

/* the fields of the options */
STATIC PROCEDURE cb_MakeFields()

    LOCAL a, o

    s_aFld := { { "Output name",   "E", "cOutName" }, ;
                { "Output path",   "E", "cOutPath" }, ;
                { "Objects path",  "E", "cObjPath" }, ;
                { "Source path",   "E", "cSrcPath" }, ;
                { "Target",        "T", "lLib" }, ;
                { "C compiler",    "K", "cComp" }, ;
                { "GT",            "G", "cGt" }, ;
                { "Libraries",     "E", "cLibs" }, ;
                { "Library paths", "E", "cLibsPath" }, ;
                { "Include paths", "E", "cIncPath" }, ;
                { "Harbour flags", "E", "cFlagsPrg" }, ;
                { "C flags",       "E", "cFlagsC" }, ;
                { "Linker flags",  "E", "cFlagsL" }, ;
                { "Harbour-CUI library", "C", "lGuiLib" }, ;
                { "Only changed files",  "C", "lMake" } }
    FOR EACH a IN s_aFld
        ASize( a, FLD_LABELW )
        a[ FLD_LABELW ] := HTLabel():new( a[ FLD_LABEL ], s_oMain )
        SWITCH a[ FLD_KIND ]
        CASE "E"
            o := CBEdit():new( s_oMain )
            o:onChanged := cb_FieldBlock( a )
            EXIT
        CASE "C"
            o := HTCheckBox():new( a[ FLD_LABEL ], s_oMain )
            o:onToggled := cb_FieldBlock( a )
            a[ FLD_LABELW ]:setText( "" )
            EXIT
        OTHERWISE
            o := CBCombo():new( s_oMain )
            o:onChanged := cb_FieldBlock( a )
        ENDSWITCH
        a[ FLD_WIDGET ] := o
    NEXT

RETURN

STATIC FUNCTION cb_FieldBlock( a )
RETURN {| x | cb_FieldChanged( a, x ) }

/* a field was changed: into the project */
STATIC PROCEDURE cb_FieldChanged( a, x )

    LOCAL oP := cb_Part(), xVal, o := a[ FLD_WIDGET ]

    IF s_lLoading .OR. oP == NIL .OR. s_oPF:cType == "hbp"
        RETURN
    ENDIF
    SWITCH a[ FLD_KIND ]
    CASE "E" ; xVal := AllTrim( x ) ; EXIT
    CASE "C" ; xVal := x ; EXIT
    CASE "T" ; xVal := ( o:currentIndex() == 2 ) ; EXIT
    CASE "K" ; xVal := IIF( o:currentIndex() <= 1, "", s_hSet[ "comps" ][ o:currentIndex() - 1 ][ "id" ] ) ; EXIT
    CASE "G" ; xVal := IIF( o:currentIndex() <= 1, "", o:currentText() ) ; EXIT
    ENDSWITCH
    __objSendMsg( oP, "_" + a[ FLD_VAR ], xVal )
    cb_SetChanged( .T. )

RETURN

/*----------------------------------------------------------- the project */

STATIC FUNCTION cb_Part()
RETURN IIF( s_oPF == NIL .OR. s_nPart > Len( s_oPF:aProjects ), NIL, s_oPF:aProjects[ s_nPart ] )

STATIC PROCEDURE cb_Open( cFile )

    LOCAL oPF, c

    oPF := cb_ProjOpen( cFile, s_hSet, .T. )
    IF ! Empty( oPF:cError )
        HTMessageBox():error( "CuiBuilder", oPF:cError )
        RETURN
    ENDIF
    s_oPF := oPF
    s_nPart := 1
    cb_SetChanged( .F. )
    cb_AddRecent( s_hSet, oPF:cFile )
    hb_cwd( hb_FNameDir( oPF:cFile ) )
    cb_ClearMsg()
    cb_AddMsg( "Project: " + oPF:cFile + IIF( oPF:cType == "hbp", "  (hbmk2 project: the options are read only; " + ;
               "Project - Edit project file changes them)", "" ), CB_MSG_INFO )
    FOR EACH c IN oPF:aWarn
        cb_AddMsg( c, CB_MSG_WARN )
    NEXT
    cb_ShowProject()

RETURN

/* the project on the screen: title, parts, files, options */
STATIC PROCEDURE cb_ShowProject()

    LOCAL oP, n

    s_lLoading := .T.
    s_oPart:clear()
    IF s_oPF != NIL
        FOR n := 1 TO Len( s_oPF:aProjects )
            oP := s_oPF:aProjects[ n ]
            s_oPart:addItem( hb_ntos( n ) + " " + IIF( Empty( oP:cName ), IIF( n == 1, "(main)", "" ), oP:cName ) )
        NEXT
        s_oPart:setCurrentIndex( s_nPart )
    ENDIF
    s_lLoading := .F.
    cb_ShowPart()

RETURN

STATIC PROCEDURE cb_ShowPart()

    LOCAL oP := cb_Part(), a, o, x, n, lRO := s_oPF != NIL .AND. s_oPF:cType == "hbp"

    cb_ShowTitle()
    cb_FillFiles()
    s_lLoading := .T.
    FOR EACH a IN s_aFld
        o := a[ FLD_WIDGET ]
        x := IIF( oP == NIL, NIL, __objSendMsg( oP, a[ FLD_VAR ] ) )
        SWITCH a[ FLD_KIND ]
        CASE "E"
            o:setText( IIF( x == NIL, "", x ) )
            EXIT
        CASE "C"
            o:setChecked( IIF( x == NIL, .F., x ) )
            EXIT
        CASE "T"
            o:clear()
            o:addItem( "Program" )
            o:addItem( "Library" )
            o:setCurrentIndex( IIF( x == .T., 2, 1 ) )
            EXIT
        CASE "K"
            o:clear()
            o:addItem( "(default: " + cb_Compiler( s_hSet, "" )[ "id" ] + ")" )
            FOR EACH n IN s_hSet[ "comps" ]
                o:addItem( n[ "id" ] + " (" + n[ "family" ] + ")" )
            NEXT
            n := 0
            IF ! Empty( x )
                n := AScan( s_hSet[ "comps" ], {| h | Lower( h[ "id" ] ) == Lower( x ) } )
            ENDIF
            o:setCurrentIndex( n + 1 )
            EXIT
        CASE "G"
            o:clear()
            o:addItem( "(default: " + IIF( Empty( s_hSet[ "guilib" ][ "gt" ] ), IIF( "windows" $ Lower( OS() ), "gtwin", "gttrm" ), ;
                       s_hSet[ "guilib" ][ "gt" ] ) + ")" )
            FOR EACH n IN { "gtwvt", "gtwin", "gtcgi", "gtstd", "gtpca", "gtgui", "gttrm", "gtxwc" }
                o:addItem( n )
            NEXT
            n := 0
            IF ! Empty( x )
                n := hb_AScan( { "gtwvt", "gtwin", "gtcgi", "gtstd", "gtpca", "gtgui", "gttrm", "gtxwc" }, Lower( x ),,, .T. )
                IF n == 0
                    o:addItem( Lower( x ) )
                    n := o:count() - 1
                ENDIF
            ENDIF
            o:setCurrentIndex( n + 1 )
            EXIT
        ENDSWITCH
        o:setFocusPolicy( IIF( lRO .OR. oP == NIL, HT_FOCUS_NONE, IIF( a[ FLD_KIND ] == "E", HT_FOCUS_STRONG, HT_FOCUS_STRONG ) ) )
    NEXT
    s_lLoading := .F.
    s_oMain:repaint()

RETURN

STATIC PROCEDURE cb_ShowTitle()

    LOCAL c

    IF s_oPF == NIL
        c := " CuiBuilder " + CB_VERSION + " - no project"
    ELSE
        c := " " + s_oPF:cFile + IIF( s_lChanged, "  *", "" )
    ENDIF
    s_oTitle:setText( c )
    s_oMain:repaintChild( s_oTitle )
    hb_gtInfo( HB_GTI_WINTITLE, IIF( s_oPF == NIL, "CuiBuilder", hb_FNameNameExt( s_oPF:cFile ) + " - CuiBuilder" ) )

RETURN

STATIC PROCEDURE cb_SetChanged( l )
    s_lChanged := l
    cb_ShowTitle()
RETURN

/* the files of the part: sub-projects first, then the sources */
STATIC PROCEDURE cb_FillFiles()

    LOCAL oP := cb_Part(), n, nCur := s_oFiles:currentIndex()

    s_oFiles:clear()
    IF oP == NIL
        s_oMain:repaintChild( s_oFiles )
        RETURN
    ENDIF
    FOR n := 1 TO Len( oP:aSubFiles )
        s_oFiles:addItem( ht_glyph( 0x25BA, ">" ) + " " + cb_Rel( oP:aSubFiles[ n ], oP:cDir ), -n )
    NEXT
    FOR n := 1 TO Len( oP:aFiles )
        s_oFiles:addItem( cb_Rel( oP:aFiles[ n ], oP:cDir ), n )
    NEXT
    IF s_oFiles:count() > 0
        s_oFiles:setCurrentIndex( Max( 1, Min( nCur, s_oFiles:count() ) ) )
    ENDIF
    s_oFrmFiles:setTitle( "Source files (" + hb_ntos( Len( oP:aFiles ) ) + ")" )
    s_oMain:repaintChild( s_oFrmFiles )
    s_oMain:repaintChild( s_oFiles )

RETURN

STATIC PROCEDURE cb_OpenDlg()

    LOCAL a

    IF ! cb_CanClose()
        RETURN
    ENDIF
    a := cb_FileDlg( "Open project", hb_cwd(), "*.hwprj;*.hbp", .F., .F. )
    IF Len( a ) > 0
        cb_Open( a[ 1 ] )
    ENDIF

RETURN

STATIC PROCEDURE cb_Recent()

    LOCAL n

    IF Len( s_hSet[ "recent" ] ) == 0
        HTToast():show( "No recent projects", 1500, HT_CLR_TOAST_INFO )
        RETURN
    ENDIF
    n := cb_List( "Recent projects", s_hSet[ "recent" ], 1 )
    IF n > 0 .AND. cb_CanClose()
        cb_Open( s_hSet[ "recent" ][ n ] )
    ENDIF

RETURN

STATIC PROCEDURE cb_New()

    LOCAL a, cFile

    IF ! cb_CanClose()
        RETURN
    ENDIF
    a := cb_FileDlg( "New project: the name of the .hwprj file", hb_cwd(), "*.hwprj", .F., .T. )
    IF Len( a ) == 0
        RETURN
    ENDIF
    cFile := a[ 1 ]
    IF Empty( hb_FNameExt( cFile ) )
        cFile += ".hwprj"
    ENDIF
    IF hb_FileExists( cFile )
        cb_Open( cFile )
        RETURN
    ENDIF
    s_oPF := cb_ProjNew( cFile )
    s_oPF:aHeader := { "# " + hb_FNameName( cFile ) + " - a CuiBuilder / HwBuilder project" }
    s_nPart := 1
    hb_cwd( hb_FNameDir( s_oPF:cFile ) )
    cb_ClearMsg()
    cb_AddMsg( "New project " + s_oPF:cFile + ": Ins adds the source files, F2 saves.", CB_MSG_INFO )
    cb_SetChanged( .T. )
    cb_ShowProject()
    cb_Focus( s_oFiles )

RETURN

/* F2: saves the .hwprj; lAs: under another name */
STATIC FUNCTION cb_Save( lAs )

    LOCAL a, cFile

    IF s_oPF == NIL
        RETURN .F.
    ENDIF
    IF s_oPF:cType == "hbp"
        IF ! lAs
            HTToast():show( "An .hbp is changed in the editor: Project - Edit project file", 2500, HT_CLR_TOAST_INFO )
            RETURN .F.
        ENDIF
    ENDIF
    cFile := s_oPF:cFile
    IF lAs .OR. s_oPF:cType == "hbp"
        a := cb_FileDlg( "Save as .hwprj", hb_FNameDir( cFile ), "*.hwprj", .F., .T. )
        IF Len( a ) == 0
            RETURN .F.
        ENDIF
        cFile := a[ 1 ]
        IF ! Lower( hb_FNameExt( cFile ) ) == ".hwprj"
            cFile := hb_FNameExtSet( cFile, ".hwprj" )
        ENDIF
        IF hb_FileExists( cFile ) .AND. HTMessageBox():choose( "CuiBuilder", hb_FNameNameExt( cFile ) + " exists. Replace it?", ;
                                                               { "&Replace", "&Cancel" }, HT_MSG_QUESTION ) != 1
            RETURN .F.
        ENDIF
        cb_RebaseProject( s_oPF, cFile )
    ENDIF
    IF ! cb_HwprjSave( s_oPF, cFile )
        HTMessageBox():error( "CuiBuilder", "Cannot write " + cFile )
        RETURN .F.
    ENDIF
    s_oPF:cFile := cFile
    s_oPF:cType := "hwprj"
    cb_AddRecent( s_hSet, cFile )
    cb_SetChanged( .F. )
    cb_ShowPart()
    HTToast():show( "Saved: " + hb_FNameNameExt( cFile ), 1500, HT_CLR_TOAST_SUCCESS )

RETURN .T.

/* a project saved in another folder: its relative paths start there */
STATIC PROCEDURE cb_RebaseProject( oPF, cFile )

    LOCAL oP, cNew := hb_FNameDir( cb_Abs( cFile, hb_cwd() ) ), n, c

    FOR EACH oP IN oPF:aProjects
        IF ! hb_FileMatch( cb_DirSep( oP:cDir ), cb_DirSep( cNew ) )
            FOR n := 1 TO Len( oP:aFiles )
                oP:aFiles[ n ] := cb_Rel( oP:srcFile( oP:aFiles[ n ] ), cNew )
            NEXT
            FOR n := 1 TO Len( oP:aSubFiles )
                oP:aSubFiles[ n ] := cb_Rel( cb_Abs( oP:aSubFiles[ n ], oP:cDir ), cNew )
            NEXT
            FOR EACH c IN { "cOutPath", "cObjPath" }
                IF ! Empty( __objSendMsg( oP, c ) )
                    __objSendMsg( oP, "_" + c, cb_Rel( cb_Abs( __objSendMsg( oP, c ), oP:cDir ), cNew ) )
                ENDIF
            NEXT
            oP:cSrcPath := ""
            oP:cDir := cNew
        ELSE
            FOR n := 1 TO Len( oP:aFiles )         /* an .hbp has absolute paths */
                oP:aFiles[ n ] := cb_Rel( oP:aFiles[ n ], cNew )
            NEXT
        ENDIF
        oP:cFile := cFile
    NEXT

RETURN

STATIC FUNCTION cb_CanClose()

    LOCAL n

    IF s_oPF == NIL .OR. ! s_lChanged
        RETURN .T.
    ENDIF
    n := HTMessageBox():choose( "CuiBuilder", hb_FNameNameExt( s_oPF:cFile ) + " has been changed. Save it?", ;
                                { "&Save", "&Don't save", "&Cancel" }, HT_MSG_QUESTION )
    IF n == 1
        RETURN cb_Save( .F. )
    ENDIF

RETURN n == 2

STATIC PROCEDURE cb_Quit()
    IF cb_CanClose()
        s_lChanged := .F.
        HTApplication():quit()
    ENDIF
RETURN

/* the project file in the editor; read again when it was saved */
STATIC PROCEDURE cb_EditProjectFile()

    LOCAL tOld, tNew

    IF s_oPF == NIL
        RETURN
    ENDIF
    IF s_lChanged .AND. s_oPF:cType == "hwprj"
        IF HTMessageBox():choose( "CuiBuilder", "Save the changes first?", { "&Save", "&Cancel" }, HT_MSG_QUESTION ) != 1 .OR. ;
           ! cb_Save( .F. )
            RETURN
        ENDIF
    ENDIF
    hb_FGetDateTime( s_oPF:cFile, @tOld )
    cb_EditFile( s_oPF:cFile, 0 )
    hb_FGetDateTime( s_oPF:cFile, @tNew )
    IF !( tOld == tNew )
        cb_Open( s_oPF:cFile )
    ENDIF

RETURN

/*----------------------------------------------------------- files */

STATIC PROCEDURE cb_AddFiles()

    LOCAL oP := cb_Part(), a, cF, cBase, n := 0, cRel

    IF oP == NIL
        HTToast():show( "Open or make a project first", 1500, HT_CLR_TOAST_INFO )
        RETURN
    ENDIF
    IF s_oPF:cType == "hbp"
        HTToast():show( "An .hbp is changed in the editor: Project - Edit project file", 2500, HT_CLR_TOAST_INFO )
        RETURN
    ENDIF
    cBase := IIF( Empty( oP:cSrcPath ), oP:cDir, cb_Abs( oP:cSrcPath, oP:cDir ) )
    a := cb_FileDlg( "Add files (Space / Ins marks)", cBase, "*.prg;*.c;*.cpp;*.rc;*.cuiform;*.frm;*.hbc;*.hwprj;*.hbp;*.o;*.obj;*.a;*.lib", .T., .F. )
    FOR EACH cF IN a
        IF cb_ExtIn( Lower( hb_FNameExt( cF ) ), { ".hwprj", ".hbp" } )
            cRel := cb_Rel( cF, oP:cDir )
            IF AScan( oP:aSubFiles, {| c | hb_FileMatch( c, cRel ) } ) == 0
                AAdd( oP:aSubFiles, cRel )
                n++
            ENDIF
        ELSE
            cRel := cb_Rel( cF, cBase )
            IF AScan( oP:aFiles, {| c | hb_FileMatch( c, cRel ) } ) == 0
                AAdd( oP:aFiles, cRel )
                n++
            ENDIF
        ENDIF
    NEXT
    IF n > 0
        cb_SetChanged( .T. )
        cb_FillFiles()
        s_oFiles:setCurrentIndex( s_oFiles:count() )
        s_oMain:repaintChild( s_oFiles )
    ENDIF

RETURN

STATIC PROCEDURE cb_RemoveFile()

    LOCAL oP := cb_Part(), x := s_oFiles:currentData()

    IF oP == NIL .OR. x == NIL .OR. s_oPF:cType == "hbp"
        RETURN
    ENDIF
    IF HTMessageBox():choose( "CuiBuilder", "Remove " + s_oFiles:currentText() + " from the project?" + Chr( 10 ) + ;
                              "(The file stays on the disk.)", { "&Remove", "&Cancel" }, HT_MSG_QUESTION ) != 1
        RETURN
    ENDIF
    IF x < 0
        hb_ADel( oP:aSubFiles, -x, .T. )
    ELSE
        hb_ADel( oP:aFiles, x, .T. )
    ENDIF
    cb_SetChanged( .T. )
    cb_FillFiles()

RETURN

/* moves the current source file up / down (the order of linking) */
STATIC PROCEDURE cb_MoveFile( nDir )

    LOCAL oP := cb_Part(), x := s_oFiles:currentData(), n, c

    IF oP == NIL .OR. x == NIL .OR. x < 0 .OR. s_oPF:cType == "hbp"
        RETURN
    ENDIF
    n := x + nDir
    IF n < 1 .OR. n > Len( oP:aFiles )
        RETURN
    ENDIF
    c := oP:aFiles[ x ]
    oP:aFiles[ x ] := oP:aFiles[ n ]
    oP:aFiles[ n ] := c
    cb_SetChanged( .T. )
    cb_FillFiles()
    s_oFiles:setCurrentIndex( s_oFiles:currentIndex() + nDir )
    s_oMain:repaintChild( s_oFiles )

RETURN

STATIC PROCEDURE cb_EditCurrent()

    LOCAL oP := cb_Part(), x := s_oFiles:currentData()

    IF oP == NIL .OR. x == NIL
        RETURN
    ENDIF
    IF x < 0
        cb_EditFile( cb_Abs( oP:aSubFiles[ -x ], oP:cDir ), 0 )
    ELSEIF Lower( hb_FNameExt( oP:aFiles[ x ] ) ) == ".cuiform"
        cb_FormDesigner( oP:srcFile( oP:aFiles[ x ] ), s_hSet, {| a | cb_AddToProject( a ) } )
    ELSEIF Lower( hb_FNameExt( oP:aFiles[ x ] ) ) == ".frm"
        cb_ReportDesigner( oP:srcFile( oP:aFiles[ x ] ), s_hSet, {| a | cb_AddToProject( a ) } )
    ELSE
        cb_EditFile( oP:srcFile( oP:aFiles[ x ] ), 0 )
    ENDIF

RETURN

/* a new part (:project) of the .hwprj, e.g. a library the program needs */
STATIC PROCEDURE cb_NewPart()

    LOCAL cName, oP

    IF s_oPF == NIL .OR. s_oPF:cType == "hbp"
        RETURN
    ENDIF
    cName := cb_Input( "New part (:project)", "Name:", "" )
    IF Empty( cName )
        RETURN
    ENDIF
    oP := CBProject():new()
    oP:cName := cName
    oP:cFile := s_oPF:cFile
    oP:cDir := hb_FNameDir( s_oPF:cFile )
    oP:cOutName := cName
    AAdd( s_oPF:aProjects, oP )
    s_nPart := Len( s_oPF:aProjects )
    cb_SetChanged( .T. )
    cb_ShowProject()

RETURN

STATIC PROCEDURE cb_DelPart()

    IF s_oPF == NIL .OR. s_oPF:cType == "hbp" .OR. Len( s_oPF:aProjects ) < 2
        HTToast():show( "The project has only one part", 1500, HT_CLR_TOAST_INFO )
        RETURN
    ENDIF
    IF HTMessageBox():choose( "CuiBuilder", "Delete the part " + s_oPart:currentText() + " from the project?", ;
                              { "&Delete", "&Cancel" }, HT_MSG_QUESTION ) != 1
        RETURN
    ENDIF
    hb_ADel( s_oPF:aProjects, s_nPart, .T. )
    s_nPart := Max( 1, s_nPart - 1 )
    cb_SetChanged( .T. )
    cb_ShowProject()

RETURN

/*----------------------------------------------------------- reports */

/* a report (.frm) in the designer (cbrepdes.prg); a new one goes into the project */
STATIC PROCEDURE cb_Report( lNew )

    LOCAL a, cFrm, oP := cb_Part()

    a := cb_FileDlg( IIF( lNew, "New report: the name of the .frm", "Open report" ), ;
                     IIF( oP == NIL, hb_cwd(), oP:cDir ), "*.frm", .F., lNew )
    IF Len( a ) == 0
        RETURN
    ENDIF
    cFrm := a[ 1 ]
    IF Empty( hb_FNameExt( cFrm ) )
        cFrm += ".frm"
    ENDIF
    IF lNew .AND. oP != NIL .AND. s_oPF:cType == "hwprj"
        cb_AddToProject( { cFrm } )
    ENDIF
    cb_ReportDesigner( cFrm, s_hSet, {| x | cb_AddToProject( x ) } )
    cb_SaveView()

RETURN

/* a dialog (.cuiform) in the dialog designer (cbformdes.prg); a new one goes into the project */
STATIC PROCEDURE cb_Form( lNew )

    LOCAL a, cFile, oP := cb_Part()

    a := cb_FileDlg( IIF( lNew, "New dialog: the name of the .cuiform", "Open dialog" ), ;
                     IIF( oP == NIL, hb_cwd(), oP:cDir ), "*.cuiform", .F., lNew )
    IF Len( a ) == 0
        RETURN
    ENDIF
    cFile := a[ 1 ]
    IF Empty( hb_FNameExt( cFile ) )
        cFile += ".cuiform"
    ENDIF
    IF lNew .AND. oP != NIL .AND. s_oPF:cType == "hwprj"
        cb_AddToProject( { cFile } )
    ENDIF
    cb_FormDesigner( cFile, s_hSet, {| x | cb_AddToProject( x ) } )

RETURN

/* the number of rows and columns of the screen (kept in cuibuild.ini) */
STATIC PROCEDURE cb_ScreenSize()

    LOCAL aSizes := { { 25, 80 }, { 30, 100 }, { 40, 132 }, { 50, 160 }, { 60, 200 }, { 75, 240 } }, aNames := {}, a, n, c

    FOR EACH a IN aSizes
        AAdd( aNames, PadL( hb_ntos( a[ 1 ] ), 3 ) + " x " + hb_ntos( a[ 2 ] ) )
    NEXT
    AAdd( aNames, "Other ..." )
    n := cb_List( "Screen size (rows x columns), now " + hb_ntos( MaxRow() + 1 ) + " x " + hb_ntos( MaxCol() + 1 ), aNames, ;
                  Max( 1, hb_AScan( aSizes, {| x | x[ 1 ] == MaxRow() + 1 .AND. x[ 2 ] == MaxCol() + 1 } ) ) )
    IF n == 0
        RETURN
    ENDIF
    IF n > Len( aSizes )
        c := cb_Input( "Screen size", "Rows x columns:", hb_ntos( MaxRow() + 1 ) + "x" + hb_ntos( MaxCol() + 1 ) )
        IF Empty( c ) .OR. ! "x" $ Lower( c )
            RETURN
        ENDIF
        a := { Val( c ), Val( SubStr( c, At( "x", Lower( c ) ) + 1 ) ) }
    ELSE
        a := aSizes[ n ]
    ENDIF
    a[ 1 ] := Min( 256, Max( 25, a[ 1 ] ) )
    a[ 2 ] := Min( 256, Max( 80, a[ 2 ] ) )
    s_hSet[ "view" ][ "rows" ] := hb_ntos( a[ 1 ] )
    s_hSet[ "view" ][ "cols" ] := hb_ntos( a[ 2 ] )
    cb_SettingsSave( s_hSet )
    s_oApp:setDisplaySize( a[ 1 ], a[ 2 ] )
    ht_cuiSetupDisplay( a[ 1 ], a[ 2 ] )
    s_oMain:screenResized()
    HTApplication():repaintAll()
    HTToast():show( "Screen: " + hb_ntos( MaxRow() + 1 ) + " x " + hb_ntos( MaxCol() + 1 ), 2000, HT_CLR_TOAST_INFO )

RETURN

/* files into the shown part of the project (what a generated report needs) */
STATIC PROCEDURE cb_AddToProject( aFiles )

    LOCAL oP := cb_Part(), cF, cRel, n := 0, c := ""

    IF oP == NIL
        RETURN
    ENDIF
    IF s_oPF:cType == "hbp"
        AEval( aFiles, {| x | c += "   " + x + Chr( 10 ) } )
        HTMessageBox():information( "CuiBuilder", "Add to " + hb_FNameNameExt( s_oPF:cFile ) + ":" + Chr( 10 ) + c )
        RETURN
    ENDIF
    FOR EACH cF IN aFiles
        cRel := IIF( Lower( hb_FNameExt( cF ) ) == ".hbc" .AND. ! hb_ps() $ cF .AND. ! "/" $ cF, cF, cb_Rel( cb_Abs( cF, hb_cwd() ), oP:cDir ) )
        IF AScan( oP:aFiles, {| x | hb_FileMatch( x, cRel ) } ) == 0
            AAdd( oP:aFiles, cRel )
            n++
        ENDIF
    NEXT
    IF n > 0
        cb_SetChanged( .T. )
        cb_FillFiles()
    ENDIF

RETURN

/*----------------------------------------------------------- build */

/* nMode: 1 build, 2 rebuild all, 3 clean, 4 build and run */
STATIC PROCEDURE cb_DoBuild( nMode )

    LOCAL oB, lOk, nErr := 0, nWarn := 0, a, n, oP, cExe, nSec

    IF s_oPF == NIL
        HTToast():show( "Open a project first (F3)", 1500, HT_CLR_TOAST_INFO )
        RETURN
    ENDIF
    IF s_lChanged .AND. ! cb_Save( .F. )
        RETURN
    ENDIF
    oB := cb_ProjOpen( s_oPF:cFile, s_hSet )            /* filters and macros applied */
    cb_ClearMsg()
    IF ! Empty( oB:cError )
        cb_AddMsg( oB:cError, CB_MSG_ERROR )
        RETURN
    ENDIF
    cb_Status( IIF( nMode == 3, "Cleaning ...", "Building ...  Esc stops after the current step" ) )
    nSec := hb_MilliSeconds()
    IF nMode == 3
        cb_Clean( oB, s_hSet, {| c, t, f, l | cb_AddMsg( c, t, f, l ) } )
        lOk := .T.
    ELSE
        lOk := cb_Build( oB, s_hSet, nMode == 2, {| c, t, f, l | cb_AddMsg( c, t, f, l ) }, ;
                         {|| ht_keyStateAvailable() .AND. ht_keyDown( VK_ESCAPE ) } )
    ENDIF
    hb_keyClear()
    FOR EACH a IN s_aMsg
        IF a[ 2 ] == CB_MSG_ERROR
            nErr++
        ELSEIF a[ 2 ] == CB_MSG_WARN
            nWarn++
        ENDIF
    NEXT
    IF nMode != 3
        cb_AddMsg( IIF( lOk, "Done", "Failed" ) + ": " + hb_ntos( nErr ) + " errors, " + hb_ntos( nWarn ) + " warnings, " + ;
                   hb_ntos( Round( ( hb_MilliSeconds() - nSec ) / 1000, 1 ) ) + " s", IIF( lOk, CB_MSG_OK, CB_MSG_ERROR ) )
    ENDIF
    cb_Status()
    IF ! lOk
        n := AScan( s_aMsg, {| x | x[ 2 ] == CB_MSG_ERROR .AND. ! Empty( x[ 3 ] ) } )
        IF n == 0
            n := AScan( s_aMsg, {| x | x[ 2 ] == CB_MSG_ERROR } )
        ENDIF
        IF n > 0
            s_oMsg:setCurrentIndex( n )
        ENDIF
        cb_Focus( s_oMsg )
        s_oMain:repaintChild( s_oMsg )
        HTToast():show( "Build failed - Enter on an error opens the file", 2500, HT_CLR_TOAST_ERROR )
        RETURN
    ENDIF
    IF nMode == 4
        oP := ATail( oB:aProjects )
        IF oP:lLib
            HTToast():show( "The last part is a library: nothing to run", 2000, HT_CLR_TOAST_INFO )
            RETURN
        ENDIF
        cExe := oP:outFile( s_hSet )
#ifdef __PLATFORM__WINDOWS
        lOk := CB_STARTPROGRAM( cb_Quote( cExe ), hb_FNameDir( cExe ) )
#else
        lOk := hb_processOpen( cb_Quote( cExe ),,,, .T. ) != -1
#endif
        IF ! lOk
            HTMessageBox():error( "CuiBuilder", "Cannot run " + cExe )
        ENDIF
    ENDIF

RETURN

/* the keyboard to a widget of the main window */
STATIC PROCEDURE cb_Focus( o )

    LOCAL oOld := s_oMain:focusWidget()

    s_oMain:setFocusChild( o )
    IF oOld != NIL .AND. ! oOld == o
        s_oMain:repaintChild( oOld )
    ENDIF
    s_oMain:repaintChild( o )

RETURN

STATIC PROCEDURE cb_Status( cText )
    s_oKeys:setText( IIF( cText == NIL, cb_KeysText(), " " + cText ) )
    s_oMain:repaintChild( s_oKeys )
RETURN

STATIC FUNCTION cb_KeysText()
RETURN " F2 Save  F3 Open  F4 Edit  Ins Add  F9 Build  Ctrl-F9 Run  Shift-F9 Rebuild  Alt-F9 Clean  F8 Error  F10 Menu  Alt-X Exit"

STATIC PROCEDURE cb_ClearMsg()
    s_aMsg := {}
    s_oMsg:clear()
    s_oMain:repaintChild( s_oMsg )
RETURN

/* a message of the build: in the list at once */
STATIC PROCEDURE cb_AddMsg( cText, nType, cFile, nLine )

    IF nType == CB_MSG_CMD .AND. ! s_lShowCmd
        RETURN
    ENDIF
    cText := StrTran( cText, Chr( 9 ), "    " )
    AAdd( s_aMsg, { cText, nType, hb_defaultValue( cFile, "" ), hb_defaultValue( nLine, 0 ) } )
    s_oMsg:addLine( IIF( nType == CB_MSG_CMD .OR. nType == CB_MSG_OUT, "  ", "" ) + cText, nType )
    s_oMain:repaintChild( s_oMsg )

RETURN

/* Enter on a message: the file at the line of an error, else the whole text */
STATIC PROCEDURE cb_MsgOpen( n )

    LOCAL a

    IF n < 1 .OR. n > Len( s_aMsg )
        RETURN
    ENDIF
    a := s_aMsg[ n ]
    IF ! Empty( a[ 3 ] ) .AND. hb_FileExists( cb_Abs( a[ 3 ], hb_cwd() ) ) .AND. ! cb_ExtIn( Lower( hb_FNameExt( a[ 3 ] ) ), { ".exe", ".a", ".lib", ".o", ".obj" } )
        cb_EditFile( cb_Abs( a[ 3 ], hb_cwd() ), a[ 4 ] )
    ELSEIF ht_strWidth( a[ 1 ] ) > s_oMsg:width - 2
        HTMessageBox():information( "Message", cb_Wrap( a[ 1 ], Min( 100, ht_ScreenMaxCol() - 10 ) ) )
    ENDIF

RETURN

/* F8 / Shift-F8: the next / previous message with a file */
STATIC PROCEDURE cb_NextError( lNext )

    LOCAL n := s_oMsg:currentIndex(), i

    i := n
    DO WHILE .T.
        i += IIF( lNext, 1, -1 )
        IF i < 1 .OR. i > Len( s_aMsg )
            HTToast():show( "No more errors", 1200, HT_CLR_TOAST_INFO )
            RETURN
        ENDIF
        IF ( s_aMsg[ i ][ 2 ] == CB_MSG_ERROR .OR. s_aMsg[ i ][ 2 ] == CB_MSG_WARN ) .AND. ! Empty( s_aMsg[ i ][ 3 ] )
            EXIT
        ENDIF
    ENDDO
    s_oMsg:setCurrentIndex( i )
    s_oMain:repaintChild( s_oMsg )
    cb_MsgOpen( i )

RETURN

STATIC FUNCTION cb_Wrap( cText, nW )

    LOCAL cOut := ""

    DO WHILE Len( cText ) > nW
        cOut += Left( cText, nW ) + Chr( 10 )
        cText := SubStr( cText, nW + 1 )
    ENDDO

RETURN cOut + cText

/*----------------------------------------------------------- menu */

STATIC PROCEDURE cb_BuildMenu()

    LOCAL oMenu

    s_oBar := s_oMain:menuBar()
    s_oBar:setBarRow( 0 )

    oMenu := s_oBar:addMenu( "&Project" )
    cb_Item( oMenu, "New ...", "", NIL, {|| cb_New() } )
    cb_Item( oMenu, "Open ...", "F3", NIL, {|| cb_OpenDlg() } )
    cb_Item( oMenu, "Recent ...", "", NIL, {|| cb_Recent() } )
    cb_Item( oMenu, "Save", "F2", NIL, {|| cb_Save( .F. ) } )
    cb_Item( oMenu, "Save as ...", "", NIL, {|| cb_Save( .T. ) } )
    oMenu:addSeparator()
    cb_Item( oMenu, "Edit project file", "", NIL, {|| cb_EditProjectFile() } )
    cb_Item( oMenu, "New part (:project) ...", "", NIL, {|| cb_NewPart() } )
    cb_Item( oMenu, "Delete part", "", NIL, {|| cb_DelPart() } )
    oMenu:addSeparator()
    cb_Item( oMenu, "Exit", "Alt-X", NIL, {|| cb_Quit() } )

    oMenu := s_oBar:addMenu( "&Files" )
    cb_Item( oMenu, "Add files ...", "Ins", NIL, {|| cb_AddFiles() } )
    cb_Item( oMenu, "Remove file", "Del", NIL, {|| cb_RemoveFile() } )
    cb_Item( oMenu, "Edit file", "F4", NIL, {|| cb_EditCurrent() } )
    cb_Item( oMenu, "Move up", "Ctrl-Up", NIL, {|| cb_MoveFile( -1 ) } )
    cb_Item( oMenu, "Move down", "Ctrl-Down", NIL, {|| cb_MoveFile( 1 ) } )

    oMenu := s_oBar:addMenu( "&Build" )
    cb_Item( oMenu, "Build", "F9", NIL, {|| cb_DoBuild( 1 ) } )
    cb_Item( oMenu, "Build and run", "Ctrl-F9", NIL, {|| cb_DoBuild( 4 ) } )
    cb_Item( oMenu, "Rebuild all", "Shift-F9", NIL, {|| cb_DoBuild( 2 ) } )
    cb_Item( oMenu, "Clean", "Alt-F9", NIL, {|| cb_DoBuild( 3 ) } )
    oMenu:addSeparator()
    cb_Item( oMenu, "Next error", "F8", NIL, {|| cb_NextError( .T. ) } )
    cb_Item( oMenu, "Previous error", "Shift-F8", NIL, {|| cb_NextError( .F. ) } )
    cb_Item( oMenu, "Show command lines", "", NIL, {|| cb_ToggleCmd() }, {|| s_lShowCmd } )

    oMenu := s_oBar:addMenu( "&Design" )
    cb_Item( oMenu, "New dialog ...", "", NIL, {|| cb_Form( .T. ) } )
    cb_Item( oMenu, "Open dialog ...", "", NIL, {|| cb_Form( .F. ) } )
    oMenu:addSeparator()
    cb_Item( oMenu, "New report ...", "", NIL, {|| cb_Report( .T. ) } )
    cb_Item( oMenu, "Open report ...", "", NIL, {|| cb_Report( .F. ) } )

    oMenu := s_oBar:addMenu( "&Options" )
    cb_Item( oMenu, "Compilers and paths ...", "", NIL, {|| cb_SettingsDlg() } )
    cb_Item( oMenu, "Default compiler ...", "", NIL, {|| cb_DefCompiler() } )
    cb_Item( oMenu, "Edit cuibuild.ini", "", NIL, {|| cb_EditIni() } )
    cb_Item( oMenu, "Theme ...", "", NIL, {|| cb_Theme() } )
    cb_Item( oMenu, "Screen size ...", "", NIL, {|| cb_ScreenSize() } )
    oMenu:addSeparator()
    cb_Item( oMenu, "About", "", NIL, {|| cb_About() } )

RETURN

STATIC PROCEDURE cb_Item( oMenu, cText, cKey, nKey, bAction, bOn )

    LOCAL oAct := oMenu:addAction( cb_ItemText( cText, cKey, bOn ) )

    IF nKey != NIL
        oAct:setShortcut( nKey )
    ENDIF
    oAct:onTriggered := bAction
    IF bOn != NIL
        AAdd( s_aCheck, { oAct, cText, cKey, bOn } )
    ENDIF

RETURN

STATIC FUNCTION cb_ItemText( cText, cKey, bOn )
RETURN IIF( bOn != NIL .AND. Eval( bOn ), ht_glyph( 0x221A, "*" ), " " ) + " &" + PadR( cText, 25 ) + PadL( cKey, 10 )

STATIC PROCEDURE cb_Checks()

    LOCAL a

    FOR EACH a IN s_aCheck
        a[ 1 ]:setText( cb_ItemText( a[ 2 ], a[ 3 ], a[ 4 ] ) )
    NEXT

RETURN

STATIC PROCEDURE cb_ToggleCmd()
    s_lShowCmd := ! s_lShowCmd
    s_hSet[ "view" ][ "showcmd" ] := IIF( s_lShowCmd, "yes", "no" )
    cb_Checks()
RETURN

/*----------------------------------------------------------- settings */

PROCEDURE cb_SaveView()
    s_hSet[ "view" ][ "theme" ] := HTTheme():currentName()
    cb_SettingsSave( s_hSet )
RETURN

STATIC PROCEDURE cb_EditIni()

    cb_SettingsSave( s_hSet )
    cb_EditFile( s_hSet[ "ini" ], 0 )
    s_hSet := cb_SettingsLoad( s_hSet[ "ini" ] )
    s_lShowCmd := hb_HGetDef( s_hSet[ "view" ], "showcmd", "" ) == "yes"
    cb_ShowPart()

RETURN

STATIC PROCEDURE cb_DefCompiler()

    LOCAL aNames := {}, h, n

    FOR EACH h IN s_hSet[ "comps" ]
        AAdd( aNames, h[ "id" ] + " (" + h[ "family" ] + ")  " + h[ "bin_path" ] )
    NEXT
    n := cb_List( "Default C compiler", aNames, Max( 1, AScan( s_hSet[ "comps" ], {| x | x == cb_Compiler( s_hSet, "" ) } ) ) )
    IF n > 0
        s_hSet[ "view" ][ "compiler" ] := s_hSet[ "comps" ][ n ][ "id" ]
        cb_SettingsSave( s_hSet )
        cb_ShowPart()
    ENDIF

RETURN

/* Harbour, the C compiler and Harbour-CUI: the paths */
STATIC PROCEDURE cb_SettingsDlg()

    LOCAL dlg, nW := Min( 96, ht_ScreenMaxCol() - 3 ), nH := 19, aF, a, nRow := 1, oComp, hComp, nComp, nLab := 22
    LOCAL hHb := s_hSet[ "harbour" ], hGui := s_hSet[ "guilib" ], cErr

    hComp := cb_Compiler( s_hSet, "" )
    nComp := AScan( s_hSet[ "comps" ], {| x | x == hComp } )
    aF := { { "Harbour folder",       hHb, "harbour_path" }, ;
            { "Harbour bin",          hHb, "harbour_bin_path" }, ;
            { "Harbour include",      hHb, "harbour_include_path" }, ;
            { "Harbour flags",        hHb, "def_flags" }, ;
            NIL, ;
            { "C compiler bin",       hComp, "bin_path" }, ;
            { "Harbour libraries",    hComp, "harbour_lib_path" }, ;
            { "C flags",              hComp, "def_cflags" }, ;
            { "Linker flags (GUI)",   hComp, "def_linkflags" }, ;
            { "System libraries",     hComp, "def_syslibs" }, ;
            NIL, ;
            { "Harbour-CUI folder",   hGui, "path" }, ;
            { "Harbour-CUI include",  hGui, "inc_path" }, ;
            { "Harbour-CUI libraries", hGui, "lib_path" }, ;
            { "Default GT",           hGui, "gt" } }

    dlg := HTDialog():new()
    dlg:setWindowTitle( "Compilers and paths - " + s_hSet[ "ini" ] )
    dlg:resize( nW, nH )
    dlg:move( Int( ( ht_ScreenMaxCol() + 1 - nW ) / 2 ), Int( ( ht_ScreenMaxRow() + 1 - nH ) / 2 ) )
    FOR EACH a IN aF
        IF a == NIL
            IF a:__enumIndex() == 5
                HTLabel():new( "C compiler", dlg ):setGeometry( 1, nRow, nLab, 1 )
                oComp := HTLabel():new( hComp[ "id" ] + " (" + hComp[ "family" ] + ")" + ;
                                        IIF( Len( s_hSet[ "comps" ] ) > 1, "   - Options, Default compiler chooses", "" ), dlg )
                oComp:setGeometry( nLab + 2, nRow, nW - nLab - 6, 1 )
            ENDIF
            nRow++
            LOOP
        ENDIF
        HTLabel():new( a[ 1 ], dlg ):setGeometry( 1, nRow, nLab, 1 )
        AAdd( a, CBEdit():new( dlg ) )
        a[ 4 ]:setGeometry( nLab + 2, nRow, nW - nLab - 6, 1 )
        a[ 4 ]:setText( a[ 2 ][ a[ 3 ] ] )
        nRow++
    NEXT
    dlg:addButtonBar( "OK", "Cancel" )
    IF dlg:exec() != HT_DIALOG_ACCEPTED
        RETURN
    ENDIF
    FOR EACH a IN aF
        IF a != NIL
            a[ 2 ][ a[ 3 ] ] := AllTrim( a[ 4 ]:text )
        ENDIF
    NEXT
    HB_SYMBOL_UNUSED( nComp )
    cb_SettingsSave( s_hSet )
    s_hSet := cb_SettingsLoad( s_hSet[ "ini" ] )          /* the empty ones found again */
    cErr := cb_SettingsCheck( s_hSet, cb_Compiler( s_hSet, "" ) )
    IF Empty( cErr )
        HTToast():show( "Settings saved", 1500, HT_CLR_TOAST_SUCCESS )
    ELSE
        HTMessageBox():warning( "CuiBuilder", cErr )
    ENDIF
    cb_ShowPart()

RETURN

STATIC PROCEDURE cb_Theme()

    LOCAL aNames := HTTheme():themeNames(), n

    n := cb_List( "Theme", aNames, Max( 1, AScan( aNames, {| c | Lower( c ) == Lower( HTTheme():currentName() ) } ) ) )
    IF n > 0
        HTTheme():loadTheme( aNames[ n ] )
        HTApplication():repaintAll()
    ENDIF

RETURN

STATIC PROCEDURE cb_About()
    HTMessageBox():information( "CuiBuilder", "CuiBuilder " + CB_VERSION + " - builds Harbour programs and libraries" + Chr( 10 ) + ;
        "built on Harbour-CUI" + Chr( 10 ) + Chr( 10 ) + ;
        "Copyright (c) 2026 Srđan Dragojlović" + Chr( 10 ) + ;
        "GPL-3.0-or-later or a commercial license" + Chr( 10 ) + Chr( 10 ) + ;
        "Works like HwBuilder (Alexander S. Kresin): .hwprj projects," + Chr( 10 ) + ;
        "Harbour, C compiler and linker run directly; .hbp projects too" + Chr( 10 ) + Chr( 10 ) + ;
        "Settings: " + s_hSet[ "ini" ] )
RETURN

/*----------------------------------------------------------- editor */

/* a file in the editor of CEdit; nLine > 0: the cursor on that line */
STATIC PROCEDURE cb_EditFile( cFile, nLine )

    LOCAL oB := CEBuffer():new(), dlg

    IF hb_FileExists( cFile )
        IF ! oB:load( cFile, "SL852" )
            HTMessageBox():error( "CuiBuilder", oB:cError )
            RETURN
        ENDIF
    ELSE
        oB:cFile := cFile
    ENDIF
    oB:cSyntax := ce_SyntaxFor( cFile )
    oB:lHilite := .T.
    dlg := CBEditDlg():create( oB, cFile )
    IF nLine > 0
        dlg:oEdit:gotoPos( Min( nLine, oB:lineCount() ), 1 )
        dlg:oEdit:refresh()
        dlg:status()
    ENDIF
    dlg:exec()

RETURN

CLASS CBEditDlg FROM HTDialog
    DATA oEdit
    DATA oStat
    DATA cFind       INIT ""
    METHOD create( oBuf, cTitle )
    METHOD keyEvent( keyEvent )
    METHOD status()
    METHOD save()
    METHOD find( lAgain )
ENDCLASS

METHOD create( oBuf, cTitle ) CLASS CBEditDlg

    LOCAL nW := ht_ScreenMaxCol() + 1, nH := ht_ScreenMaxRow() + 1

    ::super:new()
    ::setWindowTitle( cTitle )
    ::resize( nW, nH )
    ::move( 0, 0 )
    ::oEdit := HTCodeEdit():new( self )
    ::oEdit:setGeometry( 0, 0, nW - 2, nH - 3 )
    ::oEdit:setBuffer( oBuf )
    ::oEdit:onCursor := {|| ::status() }
    ::oEdit:bCtrlKey := {| c | IIF( c == "F", ( ::find( .F. ), .T. ), .F. ) }
    ::oStat := HTLabel():new( "", self )
    ::oStat:setGeometry( 0, nH - 3, nW - 2, 1 )
    ::status()

RETURN self

METHOD status() CLASS CBEditDlg

    LOCAL oB := ::oEdit:oBuf

    ::oStat:setText( " Ln " + hb_ntos( oB:nRow ) + "/" + hb_ntos( oB:lineCount() ) + "  Col " + hb_ntos( oB:nCol ) + ;
                     IIF( oB:isModified(), "  *", "" ) + "   F2 save  F7 find  Shift-F7 next  Esc close" )
    ::repaintChild( ::oStat )

RETURN NIL

METHOD save() CLASS CBEditDlg

    LOCAL oB := ::oEdit:oBuf

    IF ! oB:save( oB:cFile )
        HTMessageBox():error( "CuiBuilder", oB:cError )
        RETURN .F.
    ENDIF
    HTToast():show( "Saved: " + hb_FNameNameExt( oB:cFile ), 1500, HT_CLR_TOAST_SUCCESS )
    ::status()

RETURN .T.

METHOD find( lAgain ) CLASS CBEditDlg

    LOCAL oB := ::oEdit:oBuf, a

    IF ! lAgain .OR. Empty( ::cFind )
        ::cFind := cb_Input( "Find", "Text:", ::cFind )
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

METHOD PROCEDURE keyEvent( keyEvent ) CLASS CBEditDlg

    LOCAL nKey := keyEvent:key, oB := ::oEdit:oBuf, n

    DO CASE
    CASE nKey == K_ESC .OR. nKey == K_F10
        keyEvent:accept()
        IF nKey == K_ESC .AND. ::oEdit:cancelMarking()
            RETURN
        ENDIF
        IF oB:isModified()
            n := HTMessageBox():choose( "CuiBuilder", hb_FNameNameExt( oB:cFile ) + " has been changed. Save it?", ;
                                        { "&Save", "&Don't save", "&Cancel" }, HT_MSG_QUESTION )
            IF n == 3 .OR. n == 0 .OR. ( n == 1 .AND. ! ::save() )
                RETURN
            ENDIF
        ENDIF
        ::accept()
        RETURN
    CASE nKey == K_F2
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

/*----------------------------------------------------------- dialogs */

/* a list, Enter / double click chooses; the number, 0 = Esc */
FUNCTION cb_List( cTitle, aItems, nCurrent )

    LOCAL dlg, oList, nW, nH, nResult := 0, c

    nW := Max( 30, ht_strWidth( cTitle ) + 12 )
    AEval( aItems, {| x | nW := Max( nW, ht_strWidth( x ) + 6 ) } )
    nW := Min( nW, ht_ScreenMaxCol() - 3 )
    nH := Min( Len( aItems ) + 4, ht_ScreenMaxRow() - 3 )
    dlg := HTDialog():new()
    dlg:setWindowTitle( cTitle )
    dlg:resize( nW, nH )
    dlg:move( Int( ( ht_ScreenMaxCol() + 1 - nW ) / 2 ), Int( ( ht_ScreenMaxRow() + 1 - nH ) / 2 ) )
    oList := CBList():new( dlg )
    oList:setGeometry( 1, 1, nW - 4, nH - 3 )
    FOR EACH c IN aItems
        oList:addItem( c )
    NEXT
    oList:setCurrentIndex( Max( 1, Min( nCurrent, Len( aItems ) ) ) )
    oList:onActivated := {| n | nResult := n, dlg:accept() }
    dlg:exec()

RETURN nResult

/* one line of input; the text, "" = cancelled */
FUNCTION cb_Input( cTitle, cLabel, cInit )

    LOCAL dlg, oEdit, nW := Min( 72, ht_ScreenMaxCol() - 3 ), nH := 6, nL := Min( 30, Len( cLabel ) + 1 )

    dlg := HTDialog():new()
    dlg:setWindowTitle( cTitle )
    dlg:resize( nW, nH )
    dlg:move( Int( ( ht_ScreenMaxCol() + 1 - nW ) / 2 ), Int( ( ht_ScreenMaxRow() + 1 - nH ) / 2 ) )
    HTLabel():new( cLabel, dlg ):setGeometry( 1, 1, nL, 1 )
    oEdit := CBEdit():new( dlg )
    oEdit:setGeometry( nL + 2, 1, nW - nL - 5, 1 )
    oEdit:setText( cInit )
    oEdit:bEnter := {|| dlg:accept() }
    dlg:addButtonBar( "OK", "Cancel" )

RETURN IIF( dlg:exec() == HT_DIALOG_ACCEPTED, AllTrim( oEdit:text ), "" )

/* chooses files: folders (Enter goes in), the files of the masks ("*.prg;*.c");
   lMulti: Space / Ins marks several; lNew: a name that does not exist yet
   may be typed. The full names, {} = cancelled */
FUNCTION cb_FileDlg( cTitle, cDir, cMasks, lMulti, lNew )

    LOCAL dlg, oList, oName, oPath, nW, nH, aResult := {}, lDone := .F.

    nW := Min( 90, ht_ScreenMaxCol() - 3 )
    nH := Min( 30, ht_ScreenMaxRow() - 3 )
    cDir := cb_DirSep( cb_PathNormal( IIF( Empty( cDir ) .OR. ! hb_DirExists( cDir ), hb_cwd(), cDir ) ) )

    dlg := HTDialog():new()
    dlg:setWindowTitle( cTitle )
    dlg:resize( nW, nH )
    dlg:move( Int( ( ht_ScreenMaxCol() + 1 - nW ) / 2 ), Int( ( ht_ScreenMaxRow() + 1 - nH ) / 2 ) )
    oPath := HTLabel():new( "", dlg )
    oPath:setGeometry( 1, 0, nW - 4, 1 )
    oList := CBFileDlgList():new( dlg )
    oList:setGeometry( 1, 1, nW - 4, nH - 7 )
    oList:lMulti := lMulti
    HTLabel():new( "Name:", dlg ):setGeometry( 1, nH - 5, 6, 1 )
    oName := CBEdit():new( dlg )
    oName:setGeometry( 8, nH - 5, nW - 11, 1 )

    oList:bFill := {| d | cDir := d, oPath:setText( ht_cellLeft( d + "  (" + cMasks + ")", nW - 4 ) ), ;
                          cb_FileDlgFill( oList, d, cMasks ), dlg:repaintChild( oPath ), dlg:repaintChild( oList ) }
    oList:onActivated := {| n | cb_FileDlgEnter( oList, n, @cDir, @aResult, @lDone, dlg, lNew ) }
    oList:onChanged := {| n | IIF( n > 0 .AND. oList:itemData( n )[ 2 ] == "F", ;
                                   ( oName:setText( oList:itemData( n )[ 1 ] ), dlg:repaintChild( oName ) ), NIL ) }
    oName:bEnter := {|| cb_FileDlgName( oList, oName, @cDir, @aResult, @lDone, dlg, lNew ) }
    Eval( oList:bFill, cDir )
    dlg:setFocusChild( IIF( lNew, oName, oList ) )
    dlg:addButtonBar( "OK", "Cancel" )
    IF dlg:exec() != HT_DIALOG_ACCEPTED
        RETURN {}
    ENDIF
    IF ! lDone                              /* OK: the marked files, the typed name, the current file */
        IF lMulti .AND. Len( oList:marked() ) > 0
            aResult := oList:marked()
        ELSEIF ! Empty( oName:text )
            aResult := { cb_Abs( AllTrim( oName:text ), cDir ) }
        ELSEIF oList:currentIndex() > 0 .AND. oList:currentData()[ 2 ] == "F"
            aResult := { cDir + oList:currentData()[ 1 ] }
        ENDIF
    ENDIF

RETURN aResult

STATIC PROCEDURE cb_FileDlgFill( oList, cDir, cMasks )

    LOCAL aDirs := {}, aFiles := {}, a, cMask, c

    oList:clear()
    oList:cDir := cDir
    FOR EACH a IN Directory( cDir + hb_osFileMask(), "D" )
        IF "D" $ a[ 5 ] .AND. ! a[ 1 ] == "." .AND. ! a[ 1 ] == ".."
            AAdd( aDirs, a[ 1 ] )
        ENDIF
    NEXT
    FOR EACH cMask IN hb_ATokens( cMasks, ";" )
        FOR EACH a IN Directory( cDir + cMask )
            IF ! "D" $ a[ 5 ] .AND. AScan( aFiles, {| x | x == a[ 1 ] } ) == 0
                AAdd( aFiles, a[ 1 ] )
            ENDIF
        NEXT
    NEXT
    ASort( aDirs,,, {| x, y | Lower( x ) < Lower( y ) } )
    ASort( aFiles,,, {| x, y | Lower( x ) < Lower( y ) } )
    IF Len( hb_ATokens( cb_DirNoSep( cDir ), hb_ps() ) ) > 1 .OR. Len( cb_DirNoSep( cDir ) ) > 3
        oList:addItem( "  [..]", { "..", "D" } )
    ENDIF
#ifdef __PLATFORM__WINDOWS
    IF Len( cb_DirNoSep( cDir ) ) <= 3
        FOR EACH c IN "CDEFGHIJKLMNOPQRSTUVWXYZ"
            IF hb_DirExists( c + ":\" )
                oList:addItem( "  [" + c + ":]", { c + ":\", "V" } )
            ENDIF
        NEXT
    ENDIF
#endif
    FOR EACH c IN aDirs
        oList:addItem( "  [" + c + "]", { c, "D" } )
    NEXT
    FOR EACH c IN aFiles
        oList:addItem( "  " + c, { c, "F" } )
    NEXT

RETURN

STATIC PROCEDURE cb_FileDlgEnter( oList, n, cDir, aResult, lDone, dlg, lNew )

    LOCAL a := oList:itemData( n ), cUp

    HB_SYMBOL_UNUSED( lNew )
    DO CASE
    CASE a[ 2 ] == "V"
        Eval( oList:bFill, a[ 1 ] )
    CASE a[ 2 ] == "D" .AND. a[ 1 ] == ".."
        cUp := cb_DirNoSep( cDir )
        cUp := cb_DirSep( hb_FNameDir( cUp ) )
        Eval( oList:bFill, cUp )
        n := oList:findText( "  [" + hb_FNameNameExt( cb_DirNoSep( cDir ) ) + "]" )
        oList:setCurrentIndex( Max( 1, n ) )
        dlg:repaintChild( oList )
    CASE a[ 2 ] == "D"
        Eval( oList:bFill, cDir + a[ 1 ] + hb_ps() )
    OTHERWISE
        aResult := IIF( oList:lMulti .AND. Len( oList:marked() ) > 0, oList:marked(), { cDir + a[ 1 ] } )
        lDone := .T.
        dlg:accept()
    ENDCASE

RETURN

/* Enter in the name: a folder goes there, a file is chosen */
STATIC PROCEDURE cb_FileDlgName( oList, oName, cDir, aResult, lDone, dlg, lNew )

    LOCAL c := AllTrim( oName:text ), cF

    IF Empty( c )
        RETURN
    ENDIF
    cF := cb_Abs( c, cDir )
    IF hb_DirExists( cF )
        oName:setText( "" )
        Eval( oList:bFill, cb_DirSep( cF ) )
        dlg:repaintChild( oName )
        RETURN
    ENDIF
    IF "*" $ c .OR. "?" $ c
        RETURN
    ENDIF
    IF lNew .OR. hb_FileExists( cF )
        aResult := { cF }
        lDone := .T.
        dlg:accept()
    ELSE
        HTToast():show( "Not found: " + cF, 1500, HT_CLR_TOAST_WARNING )
    ENDIF

RETURN

/*----------------------------------------------------------- widgets */

/* a list box that keeps its width (HTListBox grows to its longest item) */
CLASS CBList FROM HTListBox
    METHOD addItem( cText, xData )
    METHOD itemData( n ) INLINE IIF( n > 0 .AND. n <= Len( ::FitemData ), ::FitemData[ n ], NIL )
    METHOD findText( cText )
ENDCLASS

METHOD FUNCTION findText( cText ) CLASS CBList
RETURN AScan( ::Fitems, {| c | c == cText } )

METHOD PROCEDURE addItem( cText, xData ) CLASS CBList
    AAdd( ::Fitems, cText )
    AAdd( ::FitemData, xData )
    IF ::FcurrentIndex == 0
        ::FcurrentIndex := 1
    ENDIF
RETURN

/* the source files: Ins adds, Del removes, Ctrl-Up / Ctrl-Down move */
CLASS CBFileList FROM CBList
    METHOD keyEvent( keyEvent )
ENDCLASS

METHOD PROCEDURE keyEvent( keyEvent ) CLASS CBFileList

    SWITCH keyEvent:key
    CASE K_INS
        keyEvent:accept()
        cb_AddFiles()
        RETURN
    CASE K_DEL
        keyEvent:accept()
        cb_RemoveFile()
        RETURN
    CASE K_CTRL_UP
        keyEvent:accept()
        cb_MoveFile( -1 )
        RETURN
    CASE K_CTRL_DOWN
        keyEvent:accept()
        cb_MoveFile( 1 )
        RETURN
    ENDSWITCH
    ::super:keyEvent( keyEvent )

RETURN

/* the files of the file dialog: Space / Ins marks (lMulti) */
CLASS CBFileDlgList FROM CBList
    DATA lMulti    INIT .F.
    DATA aMarked   INIT {}
    DATA cDir      INIT ""
    DATA bFill
    METHOD keyEvent( keyEvent )
    METHOD clear()
    METHOD marked()
ENDCLASS

METHOD PROCEDURE clear() CLASS CBFileDlgList
    ::super:clear()
    ::aMarked := {}
RETURN

METHOD FUNCTION marked() CLASS CBFileDlgList

    LOCAL a := {}, n

    FOR EACH n IN ::aMarked
        AAdd( a, ::cDir + ::FitemData[ n ][ 1 ] )
    NEXT

RETURN a

METHOD PROCEDURE keyEvent( keyEvent ) CLASS CBFileDlgList

    LOCAL n := ::FcurrentIndex, i

    IF ::lMulti .AND. ( keyEvent:key == K_SPACE .OR. keyEvent:key == K_INS ) .AND. n > 0 .AND. ::FitemData[ n ][ 2 ] == "F"
        keyEvent:accept()
        IF ( i := AScan( ::aMarked, n ) ) > 0
            hb_ADel( ::aMarked, i, .T. )
            ::Fitems[ n ] := "  " + ::FitemData[ n ][ 1 ]
        ELSE
            AAdd( ::aMarked, n )
            ::Fitems[ n ] := ht_glyph( 0x221A, "*" ) + " " + ::FitemData[ n ][ 1 ]
        ENDIF
        IF n < Len( ::Fitems )
            ::FcurrentIndex++
        ENDIF
        ::parent():repaintChild( self )
        RETURN
    ENDIF
    ::super:keyEvent( keyEvent )

RETURN

/* the messages of the build, errors and warnings in color */
CLASS CBMsgList FROM CBList
    METHOD addLine( cText, nType )
    METHOD paintEvent( paintEvent )
ENDCLASS

METHOD PROCEDURE addLine( cText, nType ) CLASS CBMsgList
    AAdd( ::Fitems, cText )
    AAdd( ::FitemData, nType )
    ::FcurrentIndex := Len( ::Fitems )
RETURN

METHOD PROCEDURE paintEvent( paintEvent ) CLASS CBMsgList

    LOCAL nRows := MaxRow() + 1, nW := MaxCol() + 1, i, nRow := 0, cClr, lFoc := ::hasFocus()
    LOCAL cNorm := HTTheme():getColor( HT_CLR_LIST_NORMAL ), nCount := Len( ::Fitems )

    HB_SYMBOL_UNUSED( paintEvent )
    IF ::FcurrentIndex > 0
        IF ::FcurrentIndex < ::FtopIndex
            ::FtopIndex := ::FcurrentIndex
        ENDIF
        IF ::FcurrentIndex >= ::FtopIndex + nRows
            ::FtopIndex := ::FcurrentIndex - nRows + 1
        ENDIF
    ENDIF
    FOR i := ::FtopIndex TO Min( ::FtopIndex + nRows - 1, nCount )
        IF i == ::FcurrentIndex .AND. lFoc
            cClr := HTTheme():getColor( HT_CLR_LIST_SELECTED )
        ELSE
            SWITCH ::FitemData[ i ]
            CASE CB_MSG_ERROR ; cClr := HTTheme():getColor( HT_CLR_TOAST_ERROR ) ; EXIT
            CASE CB_MSG_WARN  ; cClr := HTTheme():getColor( HT_CLR_TOAST_WARNING ) ; EXIT
            CASE CB_MSG_OK    ; cClr := HTTheme():getColor( HT_CLR_TOAST_SUCCESS ) ; EXIT
            CASE CB_MSG_TITLE ; cClr := HTTheme():getColor( HT_CLR_LIST_SEL_UNFOC ) ; EXIT
            OTHERWISE         ; cClr := cNorm
            ENDSWITCH
        ENDIF
        DispOutAt( nRow, 0, ht_cellPadR( ht_cellLeft( ::Fitems[ i ], nW ), nW ), cClr )
        nRow++
    NEXT
    DO WHILE nRow < nRows
        DispOutAt( nRow, 0, Space( nW ), cNorm )
        nRow++
    ENDDO

RETURN

/* a combo box that can be emptied */
CLASS CBCombo FROM HTComboBox
    METHOD clear()
ENDCLASS

METHOD PROCEDURE clear() CLASS CBCombo
    ::Fitems := {}
    ::FitemData := {}
    ::FcurrentIndex := 0
RETURN

/* a line edit; bEnter: what Enter does (else the next field) */
CLASS CBEdit FROM HTLineEdit
    DATA bEnter
    METHOD enterMovesFocus() INLINE ::bEnter == NIL
    METHOD keyEvent( keyEvent )
ENDCLASS

METHOD PROCEDURE keyEvent( keyEvent ) CLASS CBEdit
    IF keyEvent:key == K_ENTER .AND. ::bEnter != NIL
        keyEvent:accept()
        Eval( ::bEnter )
        RETURN
    ENDIF
    ::super:keyEvent( keyEvent )
RETURN
