/** @class HTMarkdownView
 * Read-only formatted text: a small Markdown subset for help and previews.
 * Headings, **bold**, *italic*, `code`, [links](url), lists, quotes, --- rules,
 * fenced code and simple | tables |. Words wrap to the width, arrows scroll,
 * a click on a link runs onLink.
 * @extends HTWidget
 *
 *   setMarkdown( c )  setWrapWidth( n )  lineCount()  lineText( n )
 *   spanKind( nLine, nChar )  linkAt( nLine, nCell )
 *   onLink {|cUrl|}
 */

#include "hbtui.ch"
#include "inkey.ch"

CLASS HTMarkdownView FROM HTWidget

PROTECTED:

    DATA Fsource  INIT ""
    DATA Flines   INIT {}     /* { cText, aSpans, nKind } */
    DATA Ftop     INIT 1
    DATA FnWidth  INIT 0      /* 0 = use the widget width when known */

    METHOD reflow( lKeepTop )
    METHOD viewRows()
    METHOD clampTop()
    METHOD repaintSelf()

PUBLIC:

    CONSTRUCTOR new( ... )

    METHOD paintEvent( paintEvent )
    METHOD keyEvent( keyEvent )
    METHOD mouseEvent( eventMouse )
    METHOD handleKey( nKey )

    METHOD setMarkdown( cText )
    METHOD markdown()          INLINE ::Fsource
    METHOD setWrapWidth( n )
    METHOD lineCount()         INLINE Len( ::Flines )
    METHOD lineText( n )
    METHOD lineKind( n )
    METHOD spanKind( nLine, nChar )
    METHOD linkAt( nLine, nCell )
    METHOD topLine()           INLINE ::Ftop

    PROPERTY onLink READWRITE

ENDCLASS

/** Creates a markdown view. Optional parent. */
METHOD new( ... ) CLASS HTMarkdownView

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
    ::Fheight := 12
    ::Fwidth := 40

RETURN self

/** Parses cText and wraps it. An empty string clears the view. */
METHOD PROCEDURE setMarkdown( cText ) CLASS HTMarkdownView
    ::Fsource := IIF( HB_ISSTRING( cText ), cText, "" )
    ::reflow( .F. )
RETURN

/** Wrap width in cells. Paint uses the viewport width when this was not set
 * and the widget has no width yet. */
METHOD PROCEDURE setWrapWidth( n ) CLASS HTMarkdownView
    IF HB_ISNUMERIC( n ) .AND. n >= 8
        ::FnWidth := Int( n )
        ::reflow( .T. )
    ENDIF
RETURN

/** Plain text of wrapped line n. */
METHOD FUNCTION lineText( n ) CLASS HTMarkdownView
    IF HB_ISNUMERIC( n ) .AND. n >= 1 .AND. n <= Len( ::Flines )
        RETURN ::Flines[ n ][ 1 ]
    ENDIF
RETURN ""

/** HT_MD_* kind of the whole line (heading, quote, code, text). */
METHOD FUNCTION lineKind( n ) CLASS HTMarkdownView
    IF HB_ISNUMERIC( n ) .AND. n >= 1 .AND. n <= Len( ::Flines )
        RETURN ::Flines[ n ][ 3 ]
    ENDIF
RETURN HT_MD_TEXT

/** Kind of the character nChar (1-based) on line n. A span wins over the line. */
METHOD FUNCTION spanKind( nLine, nChar ) CLASS HTMarkdownView

    LOCAL a, i, nAt, nLen

    IF ! HB_ISNUMERIC( nLine ) .OR. nLine < 1 .OR. nLine > Len( ::Flines )
        RETURN HT_MD_TEXT
    ENDIF
    a := ::Flines[ nLine ][ 2 ]
    FOR i := 1 TO Len( a )
        nAt := a[ i ][ 1 ]
        nLen := a[ i ][ 2 ]
        IF nChar >= nAt .AND. nChar < nAt + nLen
            RETURN a[ i ][ 3 ]
        ENDIF
    NEXT

RETURN ::Flines[ nLine ][ 3 ]

/** URL painted at screen cell nCell (0-based) of line n, or "". */
METHOD FUNCTION linkAt( nLine, nCell ) CLASS HTMarkdownView

    LOCAL cText, nChar, a, i

    IF ! HB_ISNUMERIC( nLine ) .OR. nLine < 1 .OR. nLine > Len( ::Flines )
        RETURN ""
    ENDIF
    cText := ::Flines[ nLine ][ 1 ]
    nChar := ht_cellPos( cText, nCell )
    a := ::Flines[ nLine ][ 2 ]
    FOR i := 1 TO Len( a )
        IF a[ i ][ 3 ] == HT_MD_LINK .AND. nChar >= a[ i ][ 1 ] .AND. nChar < a[ i ][ 1 ] + a[ i ][ 2 ]
            RETURN a[ i ][ 4 ]
        ENDIF
    NEXT

RETURN ""

METHOD PROCEDURE paintEvent( paintEvent ) CLASS HTMarkdownView

    LOCAL nRow, nLine, nCol, i, nAt, nLen, nKind, cText, cPiece, cColor
    LOCAL nMaxRow := MaxRow()
    LOCAL nMaxCol := MaxCol()
    LOCAL nWidth := nMaxCol + 1
    LOCAL cBody := HTTheme():getColor( HT_CLR_LABEL )

    HB_SYMBOL_UNUSED( paintEvent )

    /* follow the viewport unless the caller fixed the wrap width */
    IF ::FnWidth <= 0 .AND. nWidth >= 8 .AND. nWidth != ::Fwidth
        ::Fwidth := nWidth
        ::reflow( .T. )
    ENDIF

    ::clampTop()
    nRow := 0
    nLine := ::Ftop
    DO WHILE nRow <= nMaxRow
        IF nLine >= 1 .AND. nLine <= Len( ::Flines )
            cText := ::Flines[ nLine ][ 1 ]
            IF Len( ::Flines[ nLine ][ 2 ] ) == 0
                DispOutAt( nRow, 0, ht_cellPadR( cText, nWidth ), ht_mdColor( ::Flines[ nLine ][ 3 ] ) )
            ELSE
                DispOutAt( nRow, 0, Space( nWidth ), cBody )
                nCol := 0
                i := 1
                nAt := 1
                DO WHILE nAt <= Len( cText )
                    nLen := 1
                    nKind := ::spanKind( nLine, nAt )
                    DO WHILE nAt + nLen <= Len( cText ) .AND. ::spanKind( nLine, nAt + nLen ) == nKind
                        nLen++
                    ENDDO
                    cPiece := SubStr( cText, nAt, nLen )
                    cColor := ht_mdColor( nKind )
                    DispOutAt( nRow, nCol, cPiece, cColor )
                    nCol += ht_strWidth( cPiece )
                    nAt += nLen
                    i++
                    IF i > Len( cText ) + 2
                        EXIT
                    ENDIF
                ENDDO
            ENDIF
        ELSE
            DispOutAt( nRow, 0, Space( Max( 0, nWidth ) ), cBody )
        ENDIF
        nRow++
        nLine++
    ENDDO

RETURN

METHOD PROCEDURE keyEvent( keyEvent ) CLASS HTMarkdownView
    IF ::handleKey( keyEvent:key )
        keyEvent:accept()
        ::repaintSelf()
    ENDIF
RETURN

/** Scroll keys. .T. when the view uses the key. */
METHOD FUNCTION handleKey( nKey ) CLASS HTMarkdownView

    LOCAL nOld := ::Ftop

    DO CASE
    CASE nKey == K_UP
        ::Ftop--
    CASE nKey == K_DOWN
        ::Ftop++
    CASE nKey == K_PGUP
        ::Ftop -= ::viewRows()
    CASE nKey == K_PGDN
        ::Ftop += ::viewRows()
    CASE nKey == K_HOME
        ::Ftop := 1
    CASE nKey == K_END
        ::Ftop := Len( ::Flines )
    OTHERWISE
        RETURN .F.
    ENDCASE

    ::clampTop()

RETURN ::Ftop != nOld

METHOD PROCEDURE mouseEvent( eventMouse ) CLASS HTMarkdownView

    LOCAL nLine, cUrl

    DO CASE
    CASE eventMouse:nKey == K_LBUTTONDOWN
        nLine := ::Ftop + eventMouse:mouseRow
        cUrl := ::linkAt( nLine, eventMouse:mouseCol )
        IF Len( cUrl ) > 0 .AND. ::FonLink != NIL
            Eval( ::FonLink, cUrl )
        ENDIF
    CASE eventMouse:nKey == K_MWFORWARD
        ::Ftop--
        ::clampTop()
        ::repaintSelf()
    CASE eventMouse:nKey == K_MWBACKWARD
        ::Ftop++
        ::clampTop()
        ::repaintSelf()
    ENDCASE

RETURN

METHOD PROCEDURE reflow( lKeepTop ) CLASS HTMarkdownView

    LOCAL nTop := ::Ftop
    LOCAL nWidth := ::FnWidth

    IF nWidth < 8
        nWidth := IIF( ::Fwidth >= 8, ::Fwidth, 80 )
    ENDIF
    ::Flines := ht_mdRender( ::Fsource, nWidth )
    IF lKeepTop
        ::Ftop := nTop
    ELSE
        ::Ftop := 1
    ENDIF
    ::clampTop()

RETURN

METHOD FUNCTION viewRows() CLASS HTMarkdownView
    IF ::Fheight > 0
        RETURN ::Fheight
    ENDIF
RETURN Max( 1, MaxRow() + 1 )

METHOD PROCEDURE clampTop() CLASS HTMarkdownView

    LOCAL nMax := Max( 1, Len( ::Flines ) - ::viewRows() + 1 )

    IF ::Ftop < 1
        ::Ftop := 1
    ELSEIF ::Ftop > nMax
        ::Ftop := nMax
    ENDIF

RETURN

METHOD PROCEDURE repaintSelf() CLASS HTMarkdownView

    LOCAL parent := ::parent()

    IF parent != NIL .AND. parent:isDerivedFrom( "HTWidget" )
        parent:repaintChild( self )
    ENDIF

RETURN

/* ---------------------------------------------------------------- */

STATIC FUNCTION ht_mdColor( nKind )

    DO CASE
    CASE nKind == HT_MD_HEADING
        RETURN HTTheme():getColor( HT_CLR_TITLEBAR )
    CASE nKind == HT_MD_BOLD
        RETURN HTTheme():getColor( HT_CLR_GET_LABEL )
    CASE nKind == HT_MD_ITALIC .OR. nKind == HT_MD_QUOTE
        RETURN HTTheme():getColor( HT_CLR_STATUSBAR )
    CASE nKind == HT_MD_CODE
        RETURN HTTheme():getColor( HT_CLR_GET_READONLY )
    CASE nKind == HT_MD_LINK
        RETURN HTTheme():getColor( HT_CLR_MENU_HOTKEY )
    ENDCASE

RETURN HTTheme():getColor( HT_CLR_LABEL )

STATIC FUNCTION ht_mdRender( cSrc, nWidth )

    LOCAL aSrc := ht_mdSplit( cSrc )
    LOCAL aOut := {}
    LOCAL cPara := ""
    LOCAL lCode := .F.
    LOCAL i := 1
    LOCAL c, cList, aHead, aRows, aCells

    DO WHILE i <= Len( aSrc )
        c := aSrc[ i ]

        IF lCode
            IF Left( LTrim( c ), 3 ) == "```"
                lCode := .F.
            ELSE
                ht_mdAppend( @aOut, ht_mdWrap( { { c, HT_MD_CODE, "" } }, nWidth, HT_MD_CODE ) )
            ENDIF
            i++
            LOOP
        ENDIF

        IF Left( LTrim( c ), 3 ) == "```"
            ht_mdFlushPara( @cPara, @aOut, nWidth )
            lCode := .T.
            i++
            LOOP
        ENDIF

        IF AllTrim( c ) == ""
            ht_mdFlushPara( @cPara, @aOut, nWidth )
            AAdd( aOut, { "", {}, HT_MD_TEXT } )
            i++
            LOOP
        ENDIF

        IF ht_mdIsRule( c )
            ht_mdFlushPara( @cPara, @aOut, nWidth )
            AAdd( aOut, { Replicate( ht_glyph( 0x2500, "-" ), nWidth ), {}, HT_MD_TEXT } )
            i++
            LOOP
        ENDIF

        aHead := ht_mdHeading( c )
        IF aHead != NIL
            ht_mdFlushPara( @cPara, @aOut, nWidth )
            ht_mdAppend( @aOut, ht_mdWrap( ht_mdInline( aHead[ 2 ] ), nWidth, HT_MD_HEADING ) )
            i++
            LOOP
        ENDIF

        IF Left( LTrim( c ), 1 ) == ">"
            ht_mdFlushPara( @cPara, @aOut, nWidth )
            c := LTrim( SubStr( LTrim( c ), 2 ) )
            ht_mdAppend( @aOut, ht_mdWrap( ht_mdInline( c ), nWidth, HT_MD_QUOTE ) )
            i++
            LOOP
        ENDIF

        cList := ht_mdList( c )
        IF cList != NIL
            ht_mdFlushPara( @cPara, @aOut, nWidth )
            ht_mdAppend( @aOut, ht_mdWrap( ht_mdInline( cList ), nWidth, HT_MD_TEXT ) )
            i++
            LOOP
        ENDIF

        IF Left( LTrim( c ), 1 ) == "|"
            ht_mdFlushPara( @cPara, @aOut, nWidth )
            aRows := {}
            DO WHILE i <= Len( aSrc ) .AND. Left( LTrim( aSrc[ i ] ), 1 ) == "|"
                IF ! ht_mdIsSep( aSrc[ i ] )
                    aCells := ht_mdCells( aSrc[ i ] )
                    AAdd( aRows, aCells )
                ENDIF
                i++
            ENDDO
            ht_mdAppend( @aOut, ht_mdTable( aRows, nWidth ) )
            LOOP
        ENDIF

        IF Len( cPara ) > 0
            cPara += " "
        ENDIF
        cPara += AllTrim( c )
        i++
    ENDDO

    ht_mdFlushPara( @cPara, @aOut, nWidth )

RETURN aOut

STATIC PROCEDURE ht_mdFlushPara( cPara, aOut, nWidth )
    IF Len( cPara ) > 0
        ht_mdAppend( @aOut, ht_mdWrap( ht_mdInline( cPara ), nWidth, HT_MD_TEXT ) )
        cPara := ""
    ENDIF
RETURN

STATIC PROCEDURE ht_mdAppend( aOut, aMore )

    LOCAL x

    FOR EACH x IN aMore
        AAdd( aOut, x )
    NEXT

RETURN

STATIC FUNCTION ht_mdSplit( c )

    LOCAL a := {}
    LOCAL nStart := 1
    LOCAL n, nLen

    c := StrTran( c, Chr( 13 ) + Chr( 10 ), Chr( 10 ) )
    c := StrTran( c, Chr( 13 ), Chr( 10 ) )
    nLen := Len( c )
    DO WHILE nStart <= nLen + 1
        n := hb_At( Chr( 10 ), c, nStart )
        IF n == 0
            IF nStart <= nLen
                AAdd( a, SubStr( c, nStart ) )
            ENDIF
            EXIT
        ENDIF
        AAdd( a, SubStr( c, nStart, n - nStart ) )
        nStart := n + 1
        IF nStart > nLen
            EXIT
        ENDIF
    ENDDO

RETURN a

STATIC FUNCTION ht_mdIsRule( c )

    LOCAL cT := AllTrim( c )
    LOCAL n := Len( cT )

    IF n < 3
        RETURN .F.
    ENDIF

RETURN cT == Replicate( "-", n ) .OR. cT == Replicate( "*", n ) .OR. cT == Replicate( "_", n )

/** @return { nLevel, cText } or NIL */
STATIC FUNCTION ht_mdHeading( c )

    LOCAL i := 1

    DO WHILE i <= 6 .AND. i <= Len( c ) .AND. SubStr( c, i, 1 ) == "#"
        i++
    ENDDO
    IF i > 1 .AND. i <= Len( c ) .AND. SubStr( c, i, 1 ) == " "
        RETURN { i - 1, SubStr( c, i + 1 ) }
    ENDIF

RETURN NIL

/** Bullet or numbered item as a display line, or NIL. */
STATIC FUNCTION ht_mdList( c )

    LOCAL cT := LTrim( c )
    LOCAL n := 1
    LOCAL cMark := ht_glyph( 0x2022, "-" )

    IF Left( cT, 2 ) == "- " .OR. Left( cT, 2 ) == "* " .OR. Left( cT, 2 ) == "+ "
        RETURN "  " + cMark + " " + SubStr( cT, 3 )
    ENDIF

    DO WHILE n <= Len( cT ) .AND. SubStr( cT, n, 1 ) >= "0" .AND. SubStr( cT, n, 1 ) <= "9"
        n++
    ENDDO
    IF n > 1 .AND. n <= Len( cT ) .AND. SubStr( cT, n, 2 ) == ". "
        RETURN "  " + Left( cT, n + 1 ) + SubStr( cT, n + 2 )
    ENDIF

RETURN NIL

STATIC FUNCTION ht_mdIsSep( c )

    LOCAL cT := AllTrim( c )
    LOCAL i, ch

    IF !( "|" $ cT ) .OR. !( "-" $ cT )
        RETURN .F.
    ENDIF
    FOR i := 1 TO Len( cT )
        ch := SubStr( cT, i, 1 )
        IF !( ch == "|" .OR. ch == "-" .OR. ch == ":" .OR. ch == " " .OR. ch == "+" )
            RETURN .F.
        ENDIF
    NEXT

RETURN .T.

STATIC FUNCTION ht_mdCells( c )

    LOCAL a := {}
    LOCAL cT := AllTrim( c )
    LOCAL n, nStart

    IF Left( cT, 1 ) == "|"
        cT := SubStr( cT, 2 )
    ENDIF
    IF Right( cT, 1 ) == "|"
        cT := Left( cT, Len( cT ) - 1 )
    ENDIF

    nStart := 1
    DO WHILE nStart <= Len( cT ) + 1
        n := hb_At( "|", cT, nStart )
        IF n == 0
            AAdd( a, AllTrim( SubStr( cT, nStart ) ) )
            EXIT
        ENDIF
        AAdd( a, AllTrim( SubStr( cT, nStart, n - nStart ) ) )
        nStart := n + 1
        IF nStart > Len( cT )
            AAdd( a, "" )
            EXIT
        ENDIF
    ENDDO

RETURN a

STATIC FUNCTION ht_mdTable( aRows, nWidth )

    LOCAL aOut := {}
    LOCAL aW := {}
    LOCAL i, j, nCols := 0, nUsed, nGap, cLine, nKind

    IF Len( aRows ) == 0
        RETURN aOut
    ENDIF
    FOR i := 1 TO Len( aRows )
        nCols := Max( nCols, Len( aRows[ i ] ) )
    NEXT
    FOR j := 1 TO nCols
        AAdd( aW, 1 )
        FOR i := 1 TO Len( aRows )
            IF j <= Len( aRows[ i ] )
                aW[ j ] := Max( aW[ j ], ht_strWidth( aRows[ i ][ j ] ) )
            ENDIF
        NEXT
    NEXT

    nGap := Max( 0, nCols - 1 ) * 2
    nUsed := nGap
    FOR j := 1 TO nCols
        nUsed += aW[ j ]
    NEXT
    DO WHILE nUsed > nWidth .AND. nCols > 0
        j := nCols
        DO WHILE j >= 1 .AND. aW[ j ] <= 1
            j--
        ENDDO
        IF j < 1
            EXIT
        ENDIF
        aW[ j ]--
        nUsed--
    ENDDO

    FOR i := 1 TO Len( aRows )
        cLine := ""
        FOR j := 1 TO nCols
            IF j > 1
                cLine += "  "
            ENDIF
            cLine += ht_cellPadR( IIF( j <= Len( aRows[ i ] ), aRows[ i ][ j ], "" ), aW[ j ] )
        NEXT
        nKind := IIF( i == 1, HT_MD_HEADING, HT_MD_TEXT )
        AAdd( aOut, { ht_cellLeft( cLine, nWidth ), {}, nKind } )
    NEXT

RETURN aOut

/** Inline spans: { cText, nKind, cUrl }. */
STATIC FUNCTION ht_mdInline( cLine )

    LOCAL a := {}
    LOCAL cNorm := ""
    LOCAL i := 1
    LOCAL nLen := Len( cLine )
    LOCAL c, c2, n, nEnd, cBody, cUrl

    DO WHILE i <= nLen
        c := SubStr( cLine, i, 1 )
        c2 := SubStr( cLine, i, 2 )

        IF ( c2 == "**" .OR. c2 == "__" )
            n := hb_At( c2, cLine, i + 2 )
            IF n > i + 2
                ht_mdFlush( @cNorm, @a )
                AAdd( a, { SubStr( cLine, i + 2, n - ( i + 2 ) ), HT_MD_BOLD, "" } )
                i := n + 2
                LOOP
            ENDIF
        ENDIF

        IF c == "`"
            n := hb_At( "`", cLine, i + 1 )
            IF n > i
                ht_mdFlush( @cNorm, @a )
                AAdd( a, { SubStr( cLine, i + 1, n - i - 1 ), HT_MD_CODE, "" } )
                i := n + 1
                LOOP
            ENDIF
        ENDIF

        IF c == "["
            n := hb_At( "](", cLine, i + 1 )
            IF n > i
                nEnd := hb_At( ")", cLine, n + 2 )
                IF nEnd > n + 1
                    ht_mdFlush( @cNorm, @a )
                    cBody := SubStr( cLine, i + 1, n - i - 1 )
                    cUrl := SubStr( cLine, n + 2, nEnd - ( n + 2 ) )
                    AAdd( a, { cBody, HT_MD_LINK, cUrl } )
                    i := nEnd + 1
                    LOOP
                ENDIF
            ENDIF
        ENDIF

        IF ( c == "*" .OR. c == "_" ) .AND. ht_mdEmphOpen( cLine, i )
            n := ht_mdEmphClose( cLine, i + 1, c )
            IF n > i + 1
                ht_mdFlush( @cNorm, @a )
                AAdd( a, { SubStr( cLine, i + 1, n - i - 1 ), HT_MD_ITALIC, "" } )
                i := n + 1
                LOOP
            ENDIF
        ENDIF

        cNorm += c
        i++
    ENDDO
    ht_mdFlush( @cNorm, @a )

RETURN a

STATIC PROCEDURE ht_mdFlush( cNorm, a )
    IF Len( cNorm ) > 0
        AAdd( a, { cNorm, HT_MD_TEXT, "" } )
        cNorm := ""
    ENDIF
RETURN

STATIC FUNCTION ht_mdEmphOpen( c, i )

    LOCAL cPrev, cNext

    cNext := SubStr( c, i + 1, 1 )
    IF cNext == "" .OR. cNext == " " .OR. cNext == "*" .OR. cNext == "_"
        RETURN .F.
    ENDIF
    IF i == 1
        RETURN .T.
    ENDIF
    cPrev := SubStr( c, i - 1, 1 )

RETURN cPrev == " " .OR. cPrev == "(" .OR. cPrev == "[" .OR. cPrev == ">"

STATIC FUNCTION ht_mdEmphClose( c, i, cMark )

    LOCAL n, cPrev, cNext

    n := i
    DO WHILE n <= Len( c )
        IF SubStr( c, n, 1 ) == cMark
            cPrev := SubStr( c, n - 1, 1 )
            cNext := SubStr( c, n + 1, 1 )
            IF cPrev != " " .AND. cPrev != cMark .AND. ( cNext == "" .OR. cNext == " " .OR. ;
               cNext == "." .OR. cNext == "," .OR. cNext == ")" .OR. cNext == "]" )
                RETURN n
            ENDIF
        ENDIF
        n++
    ENDDO

RETURN 0

/** Wraps segments into lines { cText, aSpans, nKind }.
 * aSpans items are { nAt, nLen, nKind, cUrl } in character positions. */
STATIC FUNCTION ht_mdWrap( aSegs, nWidth, nKind )

    LOCAL aOut := {}
    LOCAL cLine := ""
    LOCAL aSpans := {}
    LOCAL seg, cRest, aTok, cTok, nAt, cFit

    FOR EACH seg IN aSegs
        cRest := seg[ 1 ]
        DO WHILE Len( cRest ) > 0
            aTok := ht_mdTake( cRest )
            cTok := aTok[ 1 ]
            cRest := aTok[ 2 ]

            IF cTok == " "
                IF Len( cLine ) == 0
                    LOOP
                ENDIF
                IF ht_strWidth( cLine ) + 1 > nWidth
                    ht_mdPush( @aOut, @cLine, @aSpans, nKind )
                ELSE
                    cLine += " "
                ENDIF
                LOOP
            ENDIF

            IF ht_strWidth( cTok ) > nWidth
                IF Len( cLine ) > 0
                    ht_mdPush( @aOut, @cLine, @aSpans, nKind )
                ENDIF
                cFit := ht_cellLeft( cTok, nWidth )
                IF Len( cFit ) == 0
                    cFit := Left( cTok, 1 )
                ENDIF
                nAt := 1
                cLine := cFit
                IF seg[ 2 ] != HT_MD_TEXT
                    AAdd( aSpans, { nAt, Len( cFit ), seg[ 2 ], seg[ 3 ] } )
                ENDIF
                ht_mdPush( @aOut, @cLine, @aSpans, nKind )
                cRest := SubStr( cTok, Len( cFit ) + 1 ) + cRest
                LOOP
            ENDIF

            IF Len( cLine ) > 0 .AND. ht_strWidth( cLine ) + ht_strWidth( cTok ) > nWidth
                ht_mdPush( @aOut, @cLine, @aSpans, nKind )
                cRest := cTok + cRest
                LOOP
            ENDIF

            nAt := Len( cLine ) + 1
            cLine += cTok
            IF seg[ 2 ] != HT_MD_TEXT
                AAdd( aSpans, { nAt, Len( cTok ), seg[ 2 ], seg[ 3 ] } )
            ENDIF
        ENDDO
    NEXT

    IF Len( cLine ) > 0 .OR. Len( aOut ) == 0
        ht_mdPush( @aOut, @cLine, @aSpans, nKind )
    ENDIF

RETURN aOut

STATIC FUNCTION ht_mdTake( c )

    LOCAL i := 1

    IF Len( c ) == 0
        RETURN { "", "" }
    ENDIF
    IF Left( c, 1 ) == " "
        RETURN { " ", SubStr( c, 2 ) }
    ENDIF
    DO WHILE i <= Len( c ) .AND. SubStr( c, i, 1 ) != " "
        i++
    ENDDO

RETURN { Left( c, i - 1 ), SubStr( c, i ) }

STATIC PROCEDURE ht_mdPush( aOut, cLine, aSpans, nKind )
    AAdd( aOut, { cLine, AClone( aSpans ), nKind } )
    cLine := ""
    aSpans := {}
RETURN
