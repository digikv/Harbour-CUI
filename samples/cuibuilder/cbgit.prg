/*
 * CuiBuilder - Git: status, stage, diff, commit, log, pull, push
 *
 * Copyright (c) 2026 Srđan Dragojlović - dual licensed, see LICENSE
 *
 * Runs git (from the PATH, or [VIEW] git= of cuibuild.ini) in the folder of
 * the project, without a console window (CB_RUNHIDDEN on Windows) and without
 * asking in a terminal (GIT_TERMINAL_PROMPT=0: a push that needs a password
 * fails with a message instead of waiting; Windows' credential manager still
 * shows its own window).
 *
 *   cb_GitStatus( cDir, hSet, bMsg )   the changed files: Space stages / unstages,
 *                                      Enter shows the diff, F2 commit, F6 push, F7 pull
 *   cb_GitLog( cDir, hSet )            the commits, Enter shows one
 *   cb_GitDiff( cDir, hSet )           all the changes
 *   cb_GitPull / cb_GitPush( cDir, hSet, bMsg ), cb_GitInit( cDir, hSet, bMsg )
 */

#include "hbtui.ch"
#include "inkey.ch"
#include "cuibuild.ch"

/* runs git in cDir; the exit code, -1 = git did not start; stdout + stderr in cOut */
FUNCTION cb_Git( cDir, hSet, cArgs, cOut )

    LOCAL cGit := hb_HGetDef( hSet[ "view" ], "git", "" ), nExit

    IF Empty( cGit )
        cGit := "git"
    ENDIF
    hb_SetEnv( "GIT_TERMINAL_PROMPT", "0" )
    cOut := ""
    nExit := cb_Run( cb_Quote( cGit ) + " -C " + cb_Quote( cb_DirNoSep( cDir ) ) + " " + cArgs, @cOut )

RETURN nExit

/* .T. when cDir is in a git repository (else a message) */
STATIC FUNCTION cb_GitCheck( cDir, hSet, lQuiet )

    LOCAL cOut := "", n := cb_Git( cDir, hSet, "rev-parse --is-inside-work-tree", @cOut )

    IF n == -1
        HTMessageBox():error( "Git", "git was not found. Install Git, or write its path in cuibuild.ini [VIEW] git=" )
        RETURN .F.
    ENDIF
    IF n != 0 .OR. ! "true" $ cOut
        IF ! hb_defaultValue( lQuiet, .F. )
            HTMessageBox():warning( "Git", cb_DirNoSep( cDir ) + " is not in a git repository." + Chr( 10 ) + ;
                                    "Git - Init repository makes one." )
        ENDIF
        RETURN .F.
    ENDIF

RETURN .T.

/* a path of git's output: "a b.prg" in quotes when it has special characters */
STATIC FUNCTION cb_GitPath( c )

    c := AllTrim( c )
    IF Left( c, 1 ) == '"' .AND. Right( c, 1 ) == '"' .AND. Len( c ) >= 2
        c := SubStr( c, 2, Len( c ) - 2 )
    ENDIF

RETURN c

STATIC FUNCTION cb_GitArg( c )
RETURN '"' + StrTran( c, '"', '\"' ) + '"'

/*----------------------------------------------------------- status */

PROCEDURE cb_GitStatus( cDir, hSet, bMsg )

    LOCAL dlg

    IF ! cb_GitCheck( cDir, hSet )
        RETURN
    ENDIF
    dlg := CBGitDlg():create( cDir, hSet, bMsg )
    dlg:exec()

RETURN

CLASS CBGitDlg FROM HTDialog
    DATA cDir
    DATA hSet
    DATA bMsg
    DATA aFiles      INIT {}         /* { cXY, cPath } */
    DATA cBranch     INIT ""
    DATA oHead
    DATA oList
    DATA oStat
    METHOD create( cDir, hSet, bMsg )
    METHOD reload()
    METHOD toggle()
    METHOD stageAll( lStage )
    METHOD diff()
    METHOD commit()
    METHOD keyEvent( keyEvent )
ENDCLASS

METHOD create( cDir, hSet, bMsg ) CLASS CBGitDlg

    LOCAL nW := Min( 110, ht_ScreenMaxCol() - 3 ), nH := Min( 34, ht_ScreenMaxRow() - 3 )

    ::super:new()
    ::cDir := cDir
    ::hSet := hSet
    ::bMsg := bMsg
    ::setWindowTitle( "Git: " + cb_DirNoSep( cDir ) )
    ::resize( nW, nH )
    ::move( Int( ( ht_ScreenMaxCol() + 1 - nW ) / 2 ), Int( ( ht_ScreenMaxRow() + 1 - nH ) / 2 ) )
    ::oHead := HTLabel():new( "", self )
    ::oHead:setGeometry( 1, 0, nW - 4, 1 )
    ::oList := CBGitList():new( self )
    ::oList:setGeometry( 1, 1, nW - 4, nH - 5 )
    ::oList:oDlg := self
    ::oStat := HTLabel():new( " Space stage/unstage  F3 all  Shift-F3 none  Enter diff  F2 commit  F6 push  F7 pull  F5 refresh  Esc", self )
    ::oStat:setGeometry( 0, nH - 4, nW - 2, 1 )
    ::reload()
    ::setFocusChild( ::oList )

RETURN self

/* git status --porcelain -b into the list */
METHOD reload() CLASS CBGitDlg

    LOCAL cOut := "", cLine, nCur := ::oList:currentIndex(), cXY, cPath, cMark, nStaged := 0

    cb_Git( ::cDir, ::hSet, "status --porcelain -b -uall", @cOut )
    ::aFiles := {}
    ::oList:clear()
    FOR EACH cLine IN hb_ATokens( StrTran( cOut, Chr( 13 ), "" ), Chr( 10 ) )
        IF Left( cLine, 3 ) == "## "
            ::cBranch := SubStr( cLine, 4 )
            LOOP
        ENDIF
        IF Len( cLine ) < 4
            LOOP
        ENDIF
        cXY := Left( cLine, 2 )
        cPath := SubStr( cLine, 4 )
        IF " -> " $ cPath
            cPath := SubStr( cPath, At( " -> ", cPath ) + 4 )
        ENDIF
        cPath := cb_GitPath( cPath )
        AAdd( ::aFiles, { cXY, cPath } )
        cMark := IIF( ! Left( cXY, 1 ) $ " ?", ht_glyph( 0x221A, "*" ), " " )
        IF cMark != " "
            nStaged++
        ENDIF
        ::oList:addItem( cMark + " " + cXY + "  " + cPath + "   " + cb_GitKind( cXY ), Len( ::aFiles ) )
    NEXT
    IF Len( ::aFiles ) == 0
        ::oList:addItem( "  Nothing to commit, the working tree is clean", 0 )
    ENDIF
    ::oList:setCurrentIndex( Max( 1, Min( nCur, ::oList:count() ) ) )
    ::oHead:setText( "Branch: " + ::cBranch + "    " + hb_ntos( Len( ::aFiles ) ) + " changed, " + hb_ntos( nStaged ) + " staged (" + ;
                     ht_glyph( 0x221A, "*" ) + ")" )
    ::repaintChild( ::oHead )
    ::repaintChild( ::oList )

RETURN NIL

STATIC FUNCTION cb_GitKind( cXY )

    LOCAL c := IIF( Left( cXY, 1 ) $ " ?", Right( cXY, 1 ), Left( cXY, 1 ) )

    SWITCH c
    CASE "M" ; RETURN "changed"
    CASE "A" ; RETURN "new"
    CASE "D" ; RETURN "deleted"
    CASE "R" ; RETURN "renamed"
    CASE "C" ; RETURN "copied"
    CASE "U" ; RETURN "conflict"
    CASE "?" ; RETURN "not in git"
    ENDSWITCH

RETURN ""

/* Space: the current file into the commit, or out of it */
METHOD toggle() CLASS CBGitDlg

    LOCAL n := ::oList:currentData(), a, cOut := ""

    IF n == NIL .OR. n == 0
        RETURN NIL
    ENDIF
    a := ::aFiles[ n ]
    IF ! Left( a[ 1 ], 1 ) $ " ?"
        cb_Git( ::cDir, ::hSet, "reset -q -- " + cb_GitArg( a[ 2 ] ), @cOut )
    ELSE
        cb_Git( ::cDir, ::hSet, "add -A -- " + cb_GitArg( a[ 2 ] ), @cOut )
    ENDIF
    ::reload()
    IF ::oList:currentIndex() < ::oList:count()
        ::oList:setCurrentIndex( ::oList:currentIndex() + 1 )
        ::repaintChild( ::oList )
    ENDIF

RETURN NIL

METHOD stageAll( lStage ) CLASS CBGitDlg

    LOCAL cOut := ""

    cb_Git( ::cDir, ::hSet, IIF( lStage, "add -A", "reset -q" ), @cOut )
    ::reload()

RETURN NIL

/* Enter: the changes of the current file */
METHOD diff() CLASS CBGitDlg

    LOCAL n := ::oList:currentData(), a, cOut := "", cText := ""

    IF n == NIL .OR. n == 0
        RETURN NIL
    ENDIF
    a := ::aFiles[ n ]
    IF a[ 1 ] == "??"
        cText := hb_MemoRead( cb_DirSep( ::cDir ) + a[ 2 ] )
        cb_GitView( "New file: " + a[ 2 ], StrTran( "+" + StrTran( cText, Chr( 10 ), Chr( 10 ) + "+" ), Chr( 13 ), "" ) )
        RETURN NIL
    ENDIF
    IF ! Left( a[ 1 ], 1 ) == " "
        cb_Git( ::cDir, ::hSet, "diff --cached -- " + cb_GitArg( a[ 2 ] ), @cOut )
        cText += cOut
    ENDIF
    IF ! Right( a[ 1 ], 1 ) == " "
        cb_Git( ::cDir, ::hSet, "diff -- " + cb_GitArg( a[ 2 ] ), @cOut )
        cText += IIF( Empty( cText ), "", Chr( 10 ) ) + cOut
    ENDIF
    cb_GitView( "Diff: " + a[ 2 ], cText )

RETURN NIL

/* F2: the staged files as a commit (nothing staged: all the changes, when you say so) */
METHOD commit() CLASS CBGitDlg

    LOCAL lPush := .F., cMsg, cOut := "", cTmp, n, a, nStaged := 0

    AEval( ::aFiles, {| x | IIF( ! Left( x[ 1 ], 1 ) $ " ?", nStaged++, NIL ) } )
    IF Len( ::aFiles ) == 0
        HTToast():show( "Nothing to commit", 1500, HT_CLR_TOAST_INFO )
        RETURN NIL
    ENDIF
    IF nStaged == 0
        IF HTMessageBox():choose( "Git", "No file is staged. Commit all " + hb_ntos( Len( ::aFiles ) ) + " changed files?", ;
                                  { "&All", "&Cancel" }, HT_MSG_QUESTION ) != 1
            RETURN NIL
        ENDIF
        cb_Git( ::cDir, ::hSet, "add -A", @cOut )
        ::reload()
        nStaged := Len( ::aFiles )
    ENDIF
    a := cb_GitMessage( nStaged, ::cBranch, @lPush )
    IF a == NIL
        RETURN NIL
    ENDIF
    cMsg := a
    cTmp := hb_DirTemp() + "cuibuilder_commit.txt"
    hb_MemoWrit( cTmp, cMsg )
    n := cb_Git( ::cDir, ::hSet, "commit -F " + cb_Quote( cTmp ), @cOut )
    FErase( cTmp )
    cb_GitOut( ::bMsg, "git commit", cOut, n )
    IF n != 0
        HTMessageBox():error( "Git", "The commit failed:" + Chr( 10 ) + Left( cOut, 800 ) )
        ::reload()
        RETURN NIL
    ENDIF
    HTToast():show( "Committed: " + MemoLine( cMsg, 60, 1 ), 2000, HT_CLR_TOAST_SUCCESS )
    IF lPush
        cb_GitPush( ::cDir, ::hSet, ::bMsg )
    ENDIF
    ::reload()

RETURN NIL

METHOD PROCEDURE keyEvent( keyEvent ) CLASS CBGitDlg

    SWITCH keyEvent:key
    CASE K_F2     ; keyEvent:accept() ; ::commit() ; RETURN
    CASE K_F3     ; keyEvent:accept() ; ::stageAll( .T. ) ; RETURN
    CASE K_SH_F3  ; keyEvent:accept() ; ::stageAll( .F. ) ; RETURN
    CASE K_F5     ; keyEvent:accept() ; ::reload() ; RETURN
    CASE K_F6     ; keyEvent:accept() ; cb_GitPush( ::cDir, ::hSet, ::bMsg ) ; ::reload() ; RETURN
    CASE K_F7     ; keyEvent:accept() ; cb_GitPull( ::cDir, ::hSet, ::bMsg ) ; ::reload() ; RETURN
    CASE K_ESC    ; keyEvent:accept() ; ::accept() ; RETURN
    ENDSWITCH
    ::super:keyEvent( keyEvent )

RETURN

CLASS CBGitList FROM CBList
    DATA oDlg
    METHOD keyEvent( keyEvent )
ENDCLASS

METHOD PROCEDURE keyEvent( keyEvent ) CLASS CBGitList

    SWITCH keyEvent:key
    CASE K_SPACE
    CASE K_INS
        keyEvent:accept()
        ::oDlg:toggle()
        RETURN
    CASE K_ENTER
        keyEvent:accept()
        ::oDlg:diff()
        RETURN
    ENDSWITCH
    ::super:keyEvent( keyEvent )

RETURN

/* the message of a commit: the first line and more lines; NIL = cancelled */
STATIC FUNCTION cb_GitMessage( nFiles, cBranch, lPush )

    LOCAL dlg, nW := Min( 90, ht_ScreenMaxCol() - 3 ), nH := 16, oSum, oBody, oPush, cMsg

    dlg := HTDialog():new()
    dlg:setWindowTitle( "Commit " + hb_ntos( nFiles ) + " files - " + cBranch )
    dlg:resize( nW, nH )
    dlg:move( Int( ( ht_ScreenMaxCol() + 1 - nW ) / 2 ), Int( ( ht_ScreenMaxRow() + 1 - nH ) / 2 ) )
    HTLabel():new( "Summary", dlg ):setGeometry( 1, 1, 9, 1 )
    oSum := CBEdit():new( dlg )
    oSum:setGeometry( 11, 1, nW - 14, 1 )
    oSum:bEnter := {|| dlg:accept() }
    HTLabel():new( "More (optional):", dlg ):setGeometry( 1, 3, 20, 1 )
    oBody := HTTextEdit():new( dlg )
    oBody:setGeometry( 1, 4, nW - 4, nH - 9 )
    oPush := HTCheckBox():new( "Push after the commit", dlg )
    oPush:setGeometry( 1, nH - 4, 30, 1 )
    dlg:addButtonBar( "Commit", "Cancel" )
    dlg:setFocusChild( oSum )
    IF dlg:exec() != HT_DIALOG_ACCEPTED .OR. Empty( oSum:text )
        RETURN NIL
    ENDIF
    lPush := oPush:checked
    cMsg := AllTrim( oSum:text )
    IF ! Empty( oBody:text )
        cMsg += hb_eol() + hb_eol() + RTrim( oBody:text )
    ENDIF

RETURN cMsg + hb_eol()

/*----------------------------------------------------------- log, diff, pull, push */

PROCEDURE cb_GitLog( cDir, hSet )

    LOCAL cOut := "", aLines := {}, c, n

    IF ! cb_GitCheck( cDir, hSet )
        RETURN
    ENDIF
    cb_Git( cDir, hSet, 'log -200 --date=short --pretty=format:"%h  %ad  %<(16,trunc)%an  %s%d"', @cOut )
    FOR EACH c IN hb_ATokens( StrTran( cOut, Chr( 13 ), "" ), Chr( 10 ) )
        IF ! Empty( c )
            AAdd( aLines, c )
        ENDIF
    NEXT
    IF Len( aLines ) == 0
        HTToast():show( "No commits yet", 1500, HT_CLR_TOAST_INFO )
        RETURN
    ENDIF
    n := 1
    DO WHILE ( n := cb_List( "Git log - Enter shows a commit", aLines, n ) ) > 0
        cb_Git( cDir, hSet, "show --stat -p " + Left( aLines[ n ], At( " ", aLines[ n ] ) - 1 ), @cOut )
        cb_GitView( "Commit " + Left( aLines[ n ], 60 ), cOut )
    ENDDO

RETURN

PROCEDURE cb_GitDiff( cDir, hSet )

    LOCAL cOut := "", cAll := ""

    IF ! cb_GitCheck( cDir, hSet )
        RETURN
    ENDIF
    cb_Git( cDir, hSet, "diff --cached", @cOut )
    cAll += cOut
    cb_Git( cDir, hSet, "diff", @cOut )
    cAll += cOut
    IF Empty( cAll )
        HTToast():show( "No changes", 1500, HT_CLR_TOAST_INFO )
        RETURN
    ENDIF
    cb_GitView( "Git diff: " + cb_DirNoSep( cDir ), cAll )

RETURN

PROCEDURE cb_GitPush( cDir, hSet, bMsg )
    cb_GitRemote( cDir, hSet, bMsg, "push" )
RETURN

PROCEDURE cb_GitPull( cDir, hSet, bMsg )
    cb_GitRemote( cDir, hSet, bMsg, "pull --ff-only" )
RETURN

STATIC PROCEDURE cb_GitRemote( cDir, hSet, bMsg, cCmd )

    LOCAL cOut := "", n

    IF ! cb_GitCheck( cDir, hSet )
        RETURN
    ENDIF
    HTToast():show( "git " + cCmd + " ...", 1000, HT_CLR_TOAST_INFO )
    n := cb_Git( cDir, hSet, cCmd, @cOut )
    cb_GitOut( bMsg, "git " + cCmd, cOut, n )
    IF n == 0
        HTToast():show( "git " + cCmd + ": done", 2000, HT_CLR_TOAST_SUCCESS )
    ELSE
        HTMessageBox():error( "Git", "git " + cCmd + " failed:" + Chr( 10 ) + Left( cOut, 800 ) )
    ENDIF

RETURN

PROCEDURE cb_GitInit( cDir, hSet, bMsg, aIgnore )

    LOCAL cOut := "", n, c

    IF cb_GitCheck( cDir, hSet, .T. )
        HTToast():show( "Already a git repository", 1500, HT_CLR_TOAST_INFO )
        RETURN
    ENDIF
    IF HTMessageBox():choose( "Git", "Make a git repository in " + cb_DirNoSep( cDir ) + "?", { "&Yes", "&No" }, HT_MSG_QUESTION ) != 1
        RETURN
    ENDIF
    n := cb_Git( cDir, hSet, "init -b main", @cOut )
    IF n != 0                               /* git before 2.28: no -b */
        n := cb_Git( cDir, hSet, "init", @cOut )
    ENDIF
    IF n == 0 .AND. ! hb_FileExists( cb_DirSep( cDir ) + ".gitignore" )
        /* the objects, the programs and libraries the project makes, the tries of reports */
        c := "obj/" + hb_eol() + "*.exe" + hb_eol() + "*.o" + hb_eol() + "*.obj" + hb_eol() + ;
             "*_try.docx" + hb_eol() + "*_try.xlsx" + hb_eol()
        AEval( hb_defaultValue( aIgnore, {} ), {| x | c += "/" + StrTran( x, "\", "/" ) + hb_eol() } )
        hb_MemoWrit( cb_DirSep( cDir ) + ".gitignore", c )
    ENDIF
    cb_GitOut( bMsg, "git init", cOut, n )

RETURN

/* what git wrote, into the messages of CuiBuilder */
STATIC PROCEDURE cb_GitOut( bMsg, cTitle, cOut, n )

    LOCAL c

    IF bMsg == NIL
        RETURN
    ENDIF
    Eval( bMsg, cTitle, CB_MSG_TITLE )
    FOR EACH c IN hb_ATokens( StrTran( cOut, Chr( 13 ), "" ), Chr( 10 ) )
        IF ! Empty( c )
            Eval( bMsg, c, IIF( n == 0, CB_MSG_OUT, CB_MSG_ERROR ) )
        ENDIF
    NEXT
    Eval( bMsg, IIF( n == 0, cTitle + ": done", cTitle + ": failed (" + hb_ntos( n ) + ")" ), IIF( n == 0, CB_MSG_OK, CB_MSG_ERROR ) )

RETURN

/*----------------------------------------------------------- the viewer */

/* a diff (or any text) in a list: + green, - red, @@ and the file names marked */
PROCEDURE cb_GitView( cTitle, cText )

    LOCAL dlg, oL, nW := ht_ScreenMaxCol() + 1, nH := ht_ScreenMaxRow() + 1, c

    dlg := HTDialog():new()
    dlg:setWindowTitle( cTitle + "   (Esc closes)" )
    dlg:resize( nW, nH )
    dlg:move( 0, 0 )
    oL := CBDiffList():new( dlg )
    oL:setGeometry( 0, 0, nW - 2, nH - 3 )
    FOR EACH c IN hb_ATokens( StrTran( StrTran( cText, Chr( 13 ), "" ), Chr( 9 ), "    " ), Chr( 10 ) )
        oL:addItem( c )
    NEXT
    dlg:setFocusChild( oL )
    dlg:exec()

RETURN

CLASS CBDiffList FROM CBList
    DATA nLeft      INIT 0          /* horizontal scroll */
    METHOD paintEvent( paintEvent )
    METHOD keyEvent( keyEvent )
ENDCLASS

METHOD PROCEDURE keyEvent( keyEvent ) CLASS CBDiffList

    SWITCH keyEvent:key
    CASE K_LEFT
        keyEvent:accept()
        ::nLeft := Max( 0, ::nLeft - 8 )
        ::parent():repaintChild( self )
        RETURN
    CASE K_RIGHT
        keyEvent:accept()
        ::nLeft += 8
        ::parent():repaintChild( self )
        RETURN
    ENDSWITCH
    ::super:keyEvent( keyEvent )

RETURN

METHOD PROCEDURE paintEvent( paintEvent ) CLASS CBDiffList

    LOCAL nRows := MaxRow() + 1, nW := MaxCol() + 1, i, nRow := 0, c, cClr, cNorm := HTTheme():getColor( HT_CLR_LIST_NORMAL )

    HB_SYMBOL_UNUSED( paintEvent )
    IF ::FcurrentIndex > 0
        IF ::FcurrentIndex < ::FtopIndex
            ::FtopIndex := ::FcurrentIndex
        ENDIF
        IF ::FcurrentIndex >= ::FtopIndex + nRows
            ::FtopIndex := ::FcurrentIndex - nRows + 1
        ENDIF
    ENDIF
    FOR i := ::FtopIndex TO Min( ::FtopIndex + nRows - 1, Len( ::Fitems ) )
        c := ::Fitems[ i ]
        DO CASE
        CASE i == ::FcurrentIndex
            cClr := HTTheme():getColor( HT_CLR_LIST_SELECTED )
        CASE Left( c, 3 ) == "+++" .OR. Left( c, 3 ) == "---" .OR. Left( c, 5 ) == "diff " .OR. Left( c, 7 ) == "commit "
            cClr := HTTheme():getColor( HT_CLR_LIST_SEL_UNFOC )
        CASE Left( c, 2 ) == "@@"
            cClr := HTTheme():getColor( HT_CLR_TOAST_INFO )
        CASE Left( c, 1 ) == "+"
            cClr := HTTheme():getColor( HT_CLR_TOAST_SUCCESS )
        CASE Left( c, 1 ) == "-"
            cClr := HTTheme():getColor( HT_CLR_TOAST_ERROR )
        OTHERWISE
            cClr := cNorm
        ENDCASE
        DispOutAt( nRow, 0, ht_cellPadR( ht_cellLeft( SubStr( c, ::nLeft + 1 ), nW ), nW ), cClr )
        nRow++
    NEXT
    DO WHILE nRow < nRows
        DispOutAt( nRow, 0, Space( nW ), cNorm )
        nRow++
    ENDDO

RETURN
