/*
 * cuibc - CuiBuilder on the command line (no screen; like HwBuilder's hwbc)
 *
 * Copyright (c) 2026 Srđan Dragojlović - dual licensed, see LICENSE
 *
 *   cuibc [project.hwprj | project.hbp] [options]
 *
 *   -clean       delete the objects and the output, then build everything
 *   -rebuild     build everything (the same as -clean, nothing deleted first)
 *   -cleanonly   only delete the objects and the output
 *   -comp=<id>   the C compiler of cuibuild.ini ([C_COMPILER] id=)
 *   -ini=<file>  another settings file
 *   -v           show the command lines too
 *   -q           only errors, warnings and the result
 *   -run         run the program when it was built
 *
 * Without a project: the only .hwprj (else .hbp) of the current folder.
 * Exit code: 0 = built, 1 = errors, 2 = wrong parameters / settings.
 */

#include "cuibuild.ch"

REQUEST HB_CODEPAGE_UTF8EX

STATIC s_lVerbose := .F.
STATIC s_lQuiet := .F.

PROCEDURE Main( ... )

    LOCAL cArg, cPrj := "", lClean := .F., lRebuild := .F., lOnlyClean := .F., cComp := "", cIni, lRun := .F.
    LOCAL hSet, oPF, oP, c, lOk, aF

    hb_cdpSelect( "UTF8EX" )
    FOR EACH cArg IN hb_AParams()
        c := Lower( cArg )
        DO CASE
        CASE c == "-clean"      ; lClean := .T.
        CASE c == "-rebuild"    ; lRebuild := .T.
        CASE c == "-cleanonly"  ; lOnlyClean := .T.
        CASE c == "-v"          ; s_lVerbose := .T.
        CASE c == "-q"          ; s_lQuiet := .T.
        CASE c == "-run"        ; lRun := .T.
        CASE Left( c, 6 ) == "-comp=" ; cComp := SubStr( cArg, 7 )
        CASE Left( c, 5 ) == "-ini="  ; cIni := SubStr( cArg, 6 )
        CASE c == "-h" .OR. c == "--help" .OR. c == "-?" .OR. c == "/?"
            Usage()
            RETURN
        CASE Left( c, 1 ) == "-"
            OutErr( "cuibc: unknown option " + cArg + hb_eol() )
            ErrorLevel( 2 )
            RETURN
        OTHERWISE
            cPrj := cArg
        ENDCASE
    NEXT

    IF Empty( cPrj )
        aF := Directory( "*.hwprj" )
        IF Len( aF ) == 0
            aF := Directory( "*.hbp" )
        ENDIF
        IF Len( aF ) != 1
            Usage()
            ErrorLevel( 2 )
            RETURN
        ENDIF
        cPrj := aF[ 1 ][ 1 ]
    ELSEIF Empty( hb_FNameExt( cPrj ) ) .AND. ! hb_FileExists( cPrj )
        cPrj += IIF( hb_FileExists( cPrj + ".hwprj" ), ".hwprj", ".hbp" )
    ENDIF

    hSet := cb_SettingsLoad( cIni )
    IF ! hb_FileExists( hSet[ "ini" ] )
        cb_SettingsSave( hSet )
        Say( "Settings written: " + hSet[ "ini" ] )
    ENDIF
    IF ! Empty( cComp )
        IF cb_Compiler( hSet, cComp ) == NIL
            OutErr( "cuibc: no compiler " + cComp + " in " + hSet[ "ini" ] + hb_eol() )
            ErrorLevel( 2 )
            RETURN
        ENDIF
        hSet[ "view" ][ "compiler" ] := cComp
    ENDIF

    oPF := cb_ProjOpen( cPrj, hSet )
    IF ! Empty( oPF:cError )
        OutErr( oPF:cError + hb_eol() )
        ErrorLevel( 2 )
        RETURN
    ENDIF
    FOR EACH c IN oPF:aWarn
        Out( c, CB_MSG_WARN )
    NEXT
    IF ! Empty( cComp )
        FOR EACH oP IN oPF:aProjects
            oP:cComp := cComp
        NEXT
    ENDIF

    IF lClean .OR. lOnlyClean
        cb_Clean( oPF, hSet, {| cText, nType | Out( cText, nType ) } )
        IF lOnlyClean
            RETURN
        ENDIF
    ENDIF
    lOk := cb_Build( oPF, hSet, lClean .OR. lRebuild, {| cText, nType | Out( cText, nType ) } )
    IF ! lOk
        ErrorLevel( 1 )
        RETURN
    ENDIF
    IF lRun
        oP := ATail( oPF:aProjects )
        IF ! oP:lLib
            hb_processRun( cb_Quote( oP:outFile( hSet ) ) )
        ENDIF
    ENDIF

RETURN

STATIC PROCEDURE Out( cText, nType )

    DO CASE
    CASE nType == CB_MSG_CMD
        IF s_lVerbose
            Say( "  " + cText )
        ENDIF
    CASE nType == CB_MSG_OUT .OR. nType == CB_MSG_INFO
        IF ! s_lQuiet
            Say( "  " + cText )
        ENDIF
    CASE nType == CB_MSG_STEP .OR. nType == CB_MSG_TITLE
        IF ! s_lQuiet
            Say( cText )
        ENDIF
    CASE nType == CB_MSG_ERROR .OR. nType == CB_MSG_WARN
        Say( cText )
    OTHERWISE
        Say( cText )
    ENDCASE

RETURN

STATIC PROCEDURE Say( cText )
    OutStd( cText + hb_eol() )
RETURN

STATIC PROCEDURE Usage()

    OutStd( "cuibc " + CB_VERSION + " - CuiBuilder on the command line (Harbour-CUI)" + hb_eol() + ;
            "Usage: cuibc [project.hwprj | project.hbp] [options]" + hb_eol() + ;
            "  -clean       delete the objects and the output, then build everything" + hb_eol() + ;
            "  -rebuild     build everything" + hb_eol() + ;
            "  -cleanonly   only delete the objects and the output" + hb_eol() + ;
            "  -comp=<id>   the C compiler of cuibuild.ini" + hb_eol() + ;
            "  -ini=<file>  another settings file" + hb_eol() + ;
            "  -v           show the command lines" + hb_eol() + ;
            "  -q           only errors, warnings and the result" + hb_eol() + ;
            "  -run         run the program when it was built" + hb_eol() + ;
            "Without a project: the only .hwprj (else .hbp) of the current folder." + hb_eol() + ;
            "Settings: " + cb_IniFile() + hb_eol() )

RETURN
