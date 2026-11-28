/** @class HTCommandPalette
 * A filterable list of commands, opened with exec() like a menu you can type into.
 * Add commands directly or collect them from an HTMenuBar (addMenus).
 * Each hit shows its shortcut on the right. Enter runs the marked command
 * and closes the dialog. The application binds the key, usually Ctrl+P:
 *   win:onKey( K_CTRL_P, {|| oPal:exec() } )
 * Latin text finds a Cyrillic title, same as HTSearchBox.
 * @extends HTWidget
 *
 *   addCommand( cTitle, bAction, xShortcut, cDetail )
 *   addMenus( oMenuBar )  setText( c )  hitCount()  hitTitle( n )
 *   runCurrent()  exec() -> .T. when a command ran
 */

#include "hbtui.ch"
#include "inkey.ch"

CLASS HTCommandPalette FROM HTWidget

PROTECTED:

    DATA Fcmds     INIT {}     /* { cTitle, bAction, cShortcut, cDetail } */
    DATA Fhits     INIT {}     /* indexes into Fcmds */
    DATA Ftext     INIT ""
    DATA FnCursor  INIT 1
    DATA Fmark     INIT 0      /* 1-based index in Fhits */
    DATA Ftop      INIT 1
    DATA Frun      INIT .F.

    METHOD refresh()
    METHOD viewRows()
    METHOD clamp()
    METHOD moveMark( nDelta )
    METHOD repaintSelf()
    METHOD closeIfRun()

PUBLIC:

    CONSTRUCTOR new( ... )

    METHOD paintEvent( paintEvent )
    METHOD keyEvent( keyEvent )
    METHOD mouseEvent( eventMouse )
    METHOD handleKey( nKey )
    METHOD cursorPos()

    METHOD addCommand( cTitle, bAction, xShortcut, cDetail )
    METHOD addMenus( oMenuBar )
    METHOD clear()
    METHOD commandCount()      INLINE Len( ::Fcmds )
    METHOD setText( c )
    METHOD text()              INLINE ::Ftext
    METHOD hitCount()          INLINE Len( ::Fhits )
    METHOD hitTitle( n )
    METHOD hitShortcut( n )
    METHOD hitDetail( n )
    METHOD marked()            INLINE ::Fmark
    METHOD runCurrent()
    METHOD exec()

    PROPERTY placeholder READWRITE INIT "Type a command"

ENDCLASS

/** Creates an empty palette. It has no parent until exec() opens it. */
METHOD new( ... ) CLASS HTCommandPalette

    LOCAL p

    IF pCount() > 0
        p := hb_pValue( 1 )
        IF hb_isObject( p )
            ::super:new( p )
        ELSE
            ::super:new()
        ENDIF
    ELSE
        ::super:new()
    ENDIF

    ::setFocusPolicy( HT_FOCUS_STRONG )
    ::FisVisible := .T.
    ::Fheight := 14
    ::Fwidth := 56

RETURN self

/** Adds one command.
 * xShortcut is a label ("Ctrl+P") or an Inkey code (shown as Ctrl+P / F6 / …).
 * cDetail is a group, for example the menu the command came from.
 * @return 1-based index of the command
 */
METHOD FUNCTION addCommand( cTitle, bAction, xShortcut, cDetail ) CLASS HTCommandPalette

    LOCAL cShort := ""

    IF ! HB_ISSTRING( cTitle )
        cTitle := ""
    ENDIF
    IF ! HB_ISSTRING( cDetail )
        cDetail := ""
    ENDIF
    IF HB_ISNUMERIC( xShortcut )
        cShort := ht_cmdKeyLabel( xShortcut )
    ELSEIF HB_ISSTRING( xShortcut )
        cShort := xShortcut
    ENDIF

    AAdd( ::Fcmds, { cTitle, bAction, cShort, cDetail } )
    ::refresh()

RETURN Len( ::Fcmds )

/** Walks a menu bar and adds every action that is not a separator.
 * The block runs the action, so the palette and the menu stay one command. */
METHOD PROCEDURE addMenus( oMenuBar ) CLASS HTCommandPalette

    LOCAL oMenu

    IF ! hb_isObject( oMenuBar ) .OR. ! oMenuBar:isDerivedFrom( "HTMenuBar" )
        RETURN
    ENDIF
    FOR EACH oMenu IN oMenuBar:getMenus()
        ht_cmdAddMenu( self, oMenu, ht_cmdNoAmp( oMenu:title ) )
    NEXT
    ::refresh()

RETURN

/** Drops every command. */
METHOD PROCEDURE clear() CLASS HTCommandPalette
    ::Fcmds := {}
    ::Fhits := {}
    ::Ftext := ""
    ::FnCursor := 1
    ::Fmark := 0
    ::Ftop := 1
RETURN

METHOD PROCEDURE setText( c ) CLASS HTCommandPalette
    ::Ftext := IIF( HB_ISSTRING( c ), c, "" )
    ::FnCursor := Len( ::Ftext ) + 1
    ::refresh()
RETURN

/** Title of hit n (the filtered list, not the full command list). */
METHOD FUNCTION hitTitle( n ) CLASS HTCommandPalette
    IF n >= 1 .AND. n <= Len( ::Fhits )
        RETURN ::Fcmds[ ::Fhits[ n ] ][ 1 ]
    ENDIF
RETURN ""

METHOD FUNCTION hitShortcut( n ) CLASS HTCommandPalette
    IF n >= 1 .AND. n <= Len( ::Fhits )
        RETURN ::Fcmds[ ::Fhits[ n ] ][ 3 ]
    ENDIF
RETURN ""

METHOD FUNCTION hitDetail( n ) CLASS HTCommandPalette
    IF n >= 1 .AND. n <= Len( ::Fhits )
        RETURN ::Fcmds[ ::Fhits[ n ] ][ 4 ]
    ENDIF
RETURN ""

/** Runs the marked command now, without a dialog.
 * @return .T. when there was a command to run
 */
METHOD FUNCTION runCurrent() CLASS HTCommandPalette

    LOCAL b

    IF ::Fmark < 1 .OR. ::Fmark > Len( ::Fhits )
        RETURN .F.
    ENDIF
    b := ::Fcmds[ ::Fhits[ ::Fmark ] ][ 2 ]
    IF HB_ISBLOCK( b )
        Eval( b )
    ENDIF

RETURN .T.

/** Opens a modal dialog, and runs the chosen command after it closes.
 * @return .T. when a command ran
 */
METHOD FUNCTION exec() CLASS HTCommandPalette

    LOCAL oDlg, oOld, nW, nH, nRes, lRan

    ::Frun := .F.
    ::Ftext := ""
    ::FnCursor := 1
    ::refresh()

    nW := Min( 72, Max( 42, ht_ScreenMaxCol() - 6 ) )
    nH := Min( 18, Max( 10, Int( ht_ScreenMaxRow() * 2 / 3 ) ) )

    oDlg := HTDialog():new( NIL, " Commands " )
    oDlg:resize( nW, nH )
    oDlg:setTitleButtons( .F., .F., .F. )

    oOld := ::parent()
    IF oOld != NIL
        oOld:removeChild( self )
    ENDIF
    ::setParent( oDlg )
    ::setGeometry( 0, 0, nW, nH - 1 )

    nRes := oDlg:exec()
    lRan := nRes == HT_DIALOG_ACCEPTED .AND. ::Frun

    oDlg:removeChild( self )
    IF oOld != NIL
        ::setParent( oOld )
    ENDIF

    IF lRan
        ::runCurrent()
        RETURN .T.
    ENDIF

RETURN .F.

METHOD PROCEDURE paintEvent( paintEvent ) CLASS HTCommandPalette

    LOCAL nWidth := MaxCol() + 1
    LOCAL nHeight := MaxRow() + 1
    LOCAL nRow, nHit, cLabel, cShort, nRoom, cLine, cColor
    LOCAL cField := HTTheme():getColor( IIF( ::hasFocus(), HT_CLR_LINEEDIT_FOCUSED, HT_CLR_LINEEDIT_NORMAL ) )
    LOCAL cNrm := HTTheme():getColor( HT_CLR_LIST_NORMAL )
    LOCAL cSel := HTTheme():getColor( HT_CLR_LIST_SELECTED )
    LOCAL cDim := HTTheme():getColor( HT_CLR_GET_READONLY )

    HB_SYMBOL_UNUSED( paintEvent )
    ::clamp()

    DispOutAt( 0, 0, Space( nWidth ), cField )
    DispOutAt( 0, 0, "> ", cField )
    IF Len( ::Ftext ) == 0 .AND. Len( ::Fplaceholder ) > 0
        DispOutAt( 0, 2, ht_cellLeft( ::Fplaceholder, Max( 0, nWidth - 2 ) ), cDim )
    ELSE
        DispOutAt( 0, 2, ht_cellLeft( ::Ftext, Max( 0, nWidth - 2 ) ), cField )
    ENDIF

    FOR nRow := 1 TO nHeight - 1
        nHit := ::Ftop + nRow - 1
        IF nHit <= Len( ::Fhits )
            cShort := ::hitShortcut( nHit )
            cLabel := ::hitTitle( nHit )
            IF Len( ::hitDetail( nHit ) ) > 0
                cLabel := ::hitDetail( nHit ) + " / " + cLabel
            ENDIF
            nRoom := Max( 1, nWidth - ht_strWidth( cShort ) - 1 )
            cLine := ht_cellPadR( ht_cellLeft( cLabel, nRoom ), nRoom )
            IF Len( cShort ) > 0
                cLine += cShort
            ENDIF
            cColor := IIF( nHit == ::Fmark, cSel, cNrm )
            DispOutAt( nRow, 0, ht_cellPadR( cLine, nWidth ), cColor )
        ELSEIF nRow == 1 .AND. Len( ::Fhits ) == 0
            DispOutAt( nRow, 0, ht_cellPadR( "(no matches)", nWidth ), cNrm )
        ELSE
            DispOutAt( nRow, 0, Space( nWidth ), cNrm )
        ENDIF
    NEXT

RETURN

METHOD FUNCTION cursorPos() CLASS HTCommandPalette
RETURN { 0, 2 + ht_cellCol( ::Ftext, 1, ::FnCursor ) }

METHOD PROCEDURE keyEvent( keyEvent ) CLASS HTCommandPalette
    IF ::handleKey( keyEvent:key )
        keyEvent:accept()
        ::closeIfRun()
        IF ! ::Frun
            ::repaintSelf()
        ENDIF
    ENDIF
RETURN

/** Keys. .T. when the palette uses the key. Enter sets the run flag;
 * exec() performs the command after the dialog has closed. */
METHOD FUNCTION handleKey( nKey ) CLASS HTCommandPalette

    LOCAL c := ht_keyText( nKey )

    DO CASE
    CASE nKey == K_UP
        ::moveMark( -1 )
    CASE nKey == K_DOWN
        ::moveMark( 1 )
    CASE nKey == K_PGUP
        ::moveMark( -::viewRows() )
    CASE nKey == K_PGDN
        ::moveMark( ::viewRows() )
    CASE nKey == K_HOME .AND. Len( ::Ftext ) == 0
        ::moveMark( -Len( ::Fhits ) )
    CASE nKey == K_END .AND. Len( ::Ftext ) == 0
        ::moveMark( Len( ::Fhits ) )
    CASE nKey == K_HOME
        ::FnCursor := 1
    CASE nKey == K_END
        ::FnCursor := Len( ::Ftext ) + 1
    CASE nKey == K_LEFT
        IF ::FnCursor > 1
            ::FnCursor--
        ENDIF
    CASE nKey == K_RIGHT
        IF ::FnCursor <= Len( ::Ftext )
            ::FnCursor++
        ENDIF
    CASE nKey == K_BS
        IF ::FnCursor > 1
            ::Ftext := Left( ::Ftext, ::FnCursor - 2 ) + SubStr( ::Ftext, ::FnCursor )
            ::FnCursor--
            ::refresh()
        ENDIF
    CASE nKey == K_DEL
        IF ::FnCursor <= Len( ::Ftext )
            ::Ftext := Left( ::Ftext, ::FnCursor - 1 ) + SubStr( ::Ftext, ::FnCursor + 1 )
            ::refresh()
        ENDIF
    CASE nKey == K_ENTER
        IF ::Fmark >= 1 .AND. ::Fmark <= Len( ::Fhits )
            ::Frun := .T.
        ELSE
            RETURN .F.
        ENDIF
    CASE Len( c ) >= 1
        ::Ftext := Left( ::Ftext, ::FnCursor - 1 ) + c + SubStr( ::Ftext, ::FnCursor )
        ::FnCursor += Len( c )
        ::refresh()
    OTHERWISE
        RETURN .F.
    ENDCASE

RETURN .T.

METHOD PROCEDURE mouseEvent( eventMouse ) CLASS HTCommandPalette

    LOCAL nHit

    IF eventMouse:nKey == K_LBUTTONDOWN .OR. eventMouse:nKey == K_LDBLCLK
        IF eventMouse:mouseRow >= 1
            nHit := ::Ftop + eventMouse:mouseRow - 1
            IF nHit >= 1 .AND. nHit <= Len( ::Fhits )
                ::Fmark := nHit
                IF eventMouse:nKey == K_LDBLCLK
                    ::Frun := .T.
                    ::closeIfRun()
                ELSE
                    ::repaintSelf()
                ENDIF
            ENDIF
        ENDIF
    ELSEIF eventMouse:nKey == K_MWFORWARD
        ::moveMark( -1 )
        ::repaintSelf()
    ELSEIF eventMouse:nKey == K_MWBACKWARD
        ::moveMark( 1 )
        ::repaintSelf()
    ENDIF

RETURN

METHOD PROCEDURE refresh() CLASS HTCommandPalette

    LOCAL i, nKeep := 0

    IF ::Fmark >= 1 .AND. ::Fmark <= Len( ::Fhits )
        nKeep := ::Fhits[ ::Fmark ]
    ENDIF
    ::Fhits := {}
    FOR i := 1 TO Len( ::Fcmds )
        IF ht_cmdMatch( ::Fcmds[ i ][ 1 ], ::Fcmds[ i ][ 4 ], ::Ftext )
            AAdd( ::Fhits, i )
        ENDIF
    NEXT
    ht_cmdRank( ::Fcmds, ::Fhits, ::Ftext )
    ::Fmark := 0
    IF nKeep > 0
        i := AScan( ::Fhits, nKeep )
        IF i > 0
            ::Fmark := i
        ENDIF
    ENDIF
    IF ::Fmark == 0 .AND. Len( ::Fhits ) > 0
        ::Fmark := 1
    ENDIF
    ::clamp()

RETURN

METHOD FUNCTION viewRows() CLASS HTCommandPalette
    IF ::Fheight > 1
        RETURN ::Fheight - 1
    ENDIF
RETURN Max( 1, MaxRow() )

METHOD PROCEDURE clamp() CLASS HTCommandPalette

    LOCAL nMax

    IF ::Fmark > Len( ::Fhits )
        ::Fmark := Len( ::Fhits )
    ENDIF
    nMax := Max( 1, Len( ::Fhits ) - ::viewRows() + 1 )
    IF ::Ftop < 1
        ::Ftop := 1
    ELSEIF ::Ftop > nMax
        ::Ftop := nMax
    ENDIF
    IF ::Fmark > 0 .AND. ::Fmark < ::Ftop
        ::Ftop := ::Fmark
    ELSEIF ::Fmark > ::Ftop + ::viewRows() - 1
        ::Ftop := ::Fmark - ::viewRows() + 1
    ENDIF

RETURN

METHOD PROCEDURE moveMark( nDelta ) CLASS HTCommandPalette

    IF Len( ::Fhits ) == 0
        ::Fmark := 0
        RETURN
    ENDIF
    IF ::Fmark < 1
        ::Fmark := 1
    ENDIF
    ::Fmark := Max( 1, Min( Len( ::Fhits ), ::Fmark + nDelta ) )
    ::clamp()

RETURN

METHOD PROCEDURE closeIfRun() CLASS HTCommandPalette

    LOCAL parent

    IF ! ::Frun
        RETURN
    ENDIF
    parent := ::parent()
    IF parent != NIL .AND. parent:isDerivedFrom( "HTDialog" )
        parent:accept()
    ENDIF

RETURN

METHOD PROCEDURE repaintSelf() CLASS HTCommandPalette

    LOCAL parent := ::parent()

    IF parent != NIL .AND. parent:isDerivedFrom( "HTWidget" )
        parent:repaintChild( self )
    ENDIF

RETURN

STATIC PROCEDURE ht_cmdAddMenu( oPal, oMenu, cPath )

    LOCAL itm, cTitle, xShort

    FOR EACH itm IN oMenu:items()
        IF itm:isDerivedFrom( "HTMenu" )
            cTitle := ht_cmdNoAmp( itm:title )
            IF Len( cPath ) > 0 .AND. Len( cTitle ) > 0
                cTitle := cPath + " / " + cTitle
            ENDIF
            ht_cmdAddMenu( oPal, itm, cTitle )
        ELSEIF itm:isDerivedFrom( "HTAction" ) .AND. ! itm:isSeparator()
            xShort := itm:shortcut
            oPal:addCommand( ht_cmdNoAmp( itm:text ), ht_cmdActBlock( itm ), xShort, cPath )
        ENDIF
    NEXT

RETURN

/** A block that runs this action. The parameter is private to the call,
 * so the loop that collects menus does not make every block share one variable. */
STATIC FUNCTION ht_cmdActBlock( oAction )
RETURN {|| oAction:trigger() }

STATIC FUNCTION ht_cmdNoAmp( c )

    LOCAL n

    IF ! HB_ISSTRING( c )
        RETURN ""
    ENDIF
    n := At( "&", c )
    IF n > 0
        c := Left( c, n - 1 ) + SubStr( c, n + 1 )
    ENDIF

RETURN AllTrim( c )

STATIC FUNCTION ht_cmdKeyLabel( nKey )

    LOCAL c

    IF ! HB_ISNUMERIC( nKey )
        RETURN ""
    ENDIF
    /* common shortcuts; Harbour has no hb_keyName() */
    DO CASE
    CASE nKey == K_F1  ; RETURN "F1"
    CASE nKey == K_F2  ; RETURN "F2"
    CASE nKey == K_F3  ; RETURN "F3"
    CASE nKey == K_F4  ; RETURN "F4"
    CASE nKey == K_F5  ; RETURN "F5"
    CASE nKey == K_F6  ; RETURN "F6"
    CASE nKey == K_F7  ; RETURN "F7"
    CASE nKey == K_F8  ; RETURN "F8"
    CASE nKey == K_F9  ; RETURN "F9"
    CASE nKey == K_F10 ; RETURN "F10"
    CASE nKey == K_F11 ; RETURN "F11"
    CASE nKey == K_F12 ; RETURN "F12"
    CASE nKey == K_CTRL_A ; RETURN "Ctrl+A"
    CASE nKey == K_CTRL_B ; RETURN "Ctrl+B"
    CASE nKey == K_CTRL_C ; RETURN "Ctrl+C"
    CASE nKey == K_CTRL_D ; RETURN "Ctrl+D"
    CASE nKey == K_CTRL_E ; RETURN "Ctrl+E"
    CASE nKey == K_CTRL_F ; RETURN "Ctrl+F"
    CASE nKey == K_CTRL_G ; RETURN "Ctrl+G"
    CASE nKey == K_CTRL_N ; RETURN "Ctrl+N"
    CASE nKey == K_CTRL_O ; RETURN "Ctrl+O"
    CASE nKey == K_CTRL_P ; RETURN "Ctrl+P"
    CASE nKey == K_CTRL_S ; RETURN "Ctrl+S"
    CASE nKey == K_CTRL_T ; RETURN "Ctrl+T"
    CASE nKey == K_CTRL_U ; RETURN "Ctrl+U"
    CASE nKey == K_CTRL_V ; RETURN "Ctrl+V"
    CASE nKey == K_CTRL_W ; RETURN "Ctrl+W"
    CASE nKey == K_CTRL_X ; RETURN "Ctrl+X"
    CASE nKey == K_CTRL_Y ; RETURN "Ctrl+Y"
    CASE nKey == K_CTRL_Z ; RETURN "Ctrl+Z"
    ENDCASE
    c := hb_keyChar( nKey )
    IF ! Empty( c )
        RETURN Upper( c )
    ENDIF

RETURN hb_ntos( nKey )

STATIC FUNCTION ht_cmdMatch( cTitle, cDetail, cQuery )

    LOCAL cHay, cTerm, cCyr, n, nStart, nLen

    cQuery := Lower( AllTrim( cQuery ) )
    IF Len( cQuery ) == 0
        RETURN .T.
    ENDIF
    cHay := Lower( cTitle + " " + cDetail )
    nStart := 1
    nLen := Len( cQuery )
    DO WHILE nStart <= nLen
        DO WHILE nStart <= nLen .AND. SubStr( cQuery, nStart, 1 ) == " "
            nStart++
        ENDDO
        IF nStart > nLen
            EXIT
        ENDIF
        n := nStart
        DO WHILE n <= nLen .AND. SubStr( cQuery, n, 1 ) != " "
            n++
        ENDDO
        cTerm := SubStr( cQuery, nStart, n - nStart )
        cCyr := ""
        IF !( cTerm $ cHay )
            IF ht_looksLatin( cTerm )
                cCyr := Lower( ht_latinToCyrillic( cTerm ) )
            ENDIF
            IF Len( cCyr ) == 0 .OR. cCyr == cTerm .OR. !( cCyr $ cHay )
                RETURN .F.
            ENDIF
        ENDIF
        nStart := n + 1
    ENDDO

RETURN .T.

/** Prefix matches first, then the original order. */
STATIC PROCEDURE ht_cmdRank( aCmds, aHits, cQuery )

    LOCAL aFirst := {}
    LOCAL aRest := {}
    LOCAL n, i, cQ, cTitle

    cQ := Lower( AllTrim( cQuery ) )
    IF Len( cQ ) == 0 .OR. Len( aHits ) < 2
        RETURN
    ENDIF
    FOR EACH n IN aHits
        cTitle := Lower( aCmds[ n ][ 1 ] )
        IF Left( cTitle, Len( cQ ) ) == cQ .OR. Left( Lower( aCmds[ n ][ 4 ] + " " + cTitle ), Len( cQ ) ) == cQ
            AAdd( aFirst, n )
        ELSE
            AAdd( aRest, n )
        ENDIF
    NEXT
    /* write back into the caller's array; assigning aHits would only
       rebind this local */
    FOR i := 1 TO Len( aFirst ) + Len( aRest )
        IF i <= Len( aFirst )
            aHits[ i ] := aFirst[ i ]
        ELSE
            aHits[ i ] := aRest[ i - Len( aFirst ) ]
        ENDIF
    NEXT

RETURN
