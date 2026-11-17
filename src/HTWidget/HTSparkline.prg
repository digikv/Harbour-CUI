/** @class HTSparkline
 * Compact line of values drawn with the block characters ▁▂▃▄▅▆▇█ - one
 * column per value, eight levels per row. Taller widgets stack rows, so a
 * sparkline of height 3 has 24 levels. Idea after Ratatui's Sparkline.
 * @extends HTWidget
 *
 * Shows the last width() values, oldest on the left; addValue() appends one
 * (live data: a timer adds a value, the line scrolls left).
 * The scale runs from minimum (default: 0, or the lowest value when one is
 * negative) to maximum (default: the highest value shown).
 *
 * @example
 *   oSpark := HTSparkline():new( win )
 *   oSpark:resize( 40, 3 )
 *   oSpark:setValues( { 3, 5, 9, 4, 7, 12, 6 } )
 *   HTEventLoop():scheduleRepeat( {|| oSpark:addValue( Random() % 100 ) }, 250 )
 */

#include "hbtui.ch"
#include "hbgtinfo.ch"

CLASS HTSparkline FROM HTWidget

PROTECTED:

    DATA FaValues INIT {}

PUBLIC:

    CONSTRUCTOR new( parent )

    METHOD paintEvent( paintEvent )
    METHOD setValues( aValues )
    METHOD values() INLINE AClone( ::FaValues )
    METHOD addValue( nValue )
    METHOD clear()
    METHOD levels( nColumns, nRows )   /* eighths per column for the shown values */

    PROPERTY minimum READWRITE         /* NIL = 0 (or the lowest value when negative) */
    PROPERTY maximum READWRITE         /* NIL = the highest value shown */
    PROPERTY maxCount READWRITE INIT 1000   /* values kept by addValue() */
    PROPERTY color READWRITE           /* NIL = theme: progress fill color on the window */

ENDCLASS

/** Creates a new sparkline (40 x 1) with optional parent. */
METHOD new( parent ) CLASS HTSparkline

    IF parent != NIL
        ::super:new( parent )
    ELSE
        ::super:new()
    ENDIF

    ::setFocusPolicy( HT_FOCUS_NONE )
    ::FisVisible := .T.
    ::Fheight := 1
    ::Fwidth := 40

RETURN self

/** Replaces all values (numbers; anything else counts as 0) and repaints. */
METHOD PROCEDURE setValues( aValues ) CLASS HTSparkline

    ::FaValues := {}
    IF HB_ISARRAY( aValues )
        AEval( aValues, {| x | AAdd( ::FaValues, IIF( HB_ISNUMERIC( x ), x, 0 ) ) } )
    ENDIF
    ht_chartRepaint( self )

RETURN

/** Appends a value, drops the oldest beyond maxCount, repaints. */
METHOD PROCEDURE addValue( nValue ) CLASS HTSparkline

    AAdd( ::FaValues, IIF( HB_ISNUMERIC( nValue ), nValue, 0 ) )
    IF HB_ISNUMERIC( ::FmaxCount ) .AND. ::FmaxCount > 0
        DO WHILE Len( ::FaValues ) > ::FmaxCount
            hb_ADel( ::FaValues, 1, .T. )
        ENDDO
    ENDIF
    ht_chartRepaint( self )

RETURN

/** Removes all values and repaints. */
METHOD PROCEDURE clear() CLASS HTSparkline

    ::FaValues := {}
    ht_chartRepaint( self )

RETURN

/** Heights of the shown values, in eighths of a row.
 * @param nColumns Columns available (the last nColumns values are shown)
 * @param nRows    Rows available
 * @return Array, one entry per shown value: 0 .. nRows * 8
 */
METHOD FUNCTION levels( nColumns, nRows ) CLASS HTSparkline

    LOCAL aShown, nLo, nHi

    IF nColumns < 1 .OR. nRows < 1 .OR. Len( ::FaValues ) = 0
        RETURN {}
    ENDIF

    aShown := {}
    AEval( ::FaValues, {| x | AAdd( aShown, x ) }, Max( 1, Len( ::FaValues ) - nColumns + 1 ) )

    nLo := ::Fminimum
    IF nLo == NIL
        nLo := 0
        AEval( aShown, {| x | nLo := Min( nLo, x ) } )
    ENDIF
    nHi := ::Fmaximum
    IF nHi == NIL
        nHi := nLo
        AEval( aShown, {| x | nHi := Max( nHi, x ) } )
    ENDIF

RETURN ht_chartScale( aShown, nLo, nHi, nRows * 8 )

/** Draws the shown values row by row with the eighth blocks. */
METHOD PROCEDURE paintEvent( paintEvent ) CLASS HTSparkline

    LOCAL nRows := MaxRow() + 1
    LOCAL nCols := MaxCol() + 1
    LOCAL aLevels := ::levels( nCols, nRows )
    LOCAL cColor := ht_chartColor( ::Fcolor )
    LOCAL cFull := ht_chartFullColor( cColor )
    LOCAL nRow, nBase, nLevel, nCol, nRunCol, lFull, lRunFull, cRun

    HB_SYMBOL_UNUSED( paintEvent )

    FOR nRow := 0 TO nRows - 1
        nBase := ( nRows - 1 - nRow ) * 8     /* eighths below this row */
        /* runs of full cells (spaces in the bar color: no gaps between rows)
           and of partial cells (block characters) */
        cRun := ""
        nRunCol := 0
        lRunFull := .F.
        nCol := 0
        FOR EACH nLevel IN aLevels
            lFull := nLevel - nBase >= 8
            IF ! cRun == "" .AND. lFull != lRunFull
                DispOutAt( nRow, nRunCol, cRun, IIF( lRunFull, cFull, cColor ) )
                cRun := ""
            ENDIF
            IF cRun == ""
                nRunCol := nCol
                lRunFull := lFull
            ENDIF
            cRun += IIF( lFull, " ", ht_blockGlyph( nLevel - nBase ) )
            nCol++
        NEXT
        IF ! cRun == ""
            DispOutAt( nRow, nRunCol, cRun, IIF( lRunFull, cFull, cColor ) )
        ENDIF
        IF nCol < nCols
            DispOutAt( nRow, nCol, Space( nCols - nCol ), cColor )
        ENDIF
    NEXT

RETURN

/*----------------------------------------------------------------*/
/* Shared by the chart widgets (HTSparkline, HTBarChart)           */
/*----------------------------------------------------------------*/

/** The block character for nEighths of a cell filled from the bottom:
 * 0 = space, 1..7 = ▁▂▃▄▅▆▇, 8 or more = █ (the chart widgets draw a full
 * cell as a space in the bar color instead: █ leaves gaps between rows).
 * Only ▄ and █ when the screen font lacks the eighths (Consolas in gtwvt has
 * just the halves); ASCII when the codepage has no block characters.
 */
FUNCTION ht_blockGlyph( nEighths )

    THREAD STATIC t_aGlyphs
    THREAD STATIC t_cKey
    LOCAL cKey, cFont, c4, c8

    /* per codepage and font: HTApplication():new() may switch EN -> UTF8EX,
       ht_cuiSetupDisplay() sets the font */
    cFont := IIF( hb_gtVersion() == "WVT", hb_CStr( hb_gtInfo( HB_GTI_FONTNAME ) ), "" )
    cKey := hb_cdpSelect() + "|" + cFont
    IF t_aGlyphs == NIL .OR. ! t_cKey == cKey
        t_cKey := cKey
        c4 := ht_glyph( 0x2584, "=" )
        c8 := ht_glyph( 0x2588, "#" )
        IF ! c4 == "=" .AND. ! ht_fontHasChar( cFont, 0x2581 )
            /* halves only: 1..5 eighths -> lower half, 6..8 -> full */
            t_aGlyphs := { c4, c4, c4, c4, c4, c8, c8, c8 }
        ELSE
            /* U+2581 .. U+2588; ASCII fallback " ._-=+*##" */
            t_aGlyphs := { ht_glyph( 0x2581, "." ), ht_glyph( 0x2582, "_" ), ;
                           ht_glyph( 0x2583, "-" ), c4, ;
                           ht_glyph( 0x2585, "+" ), ht_glyph( 0x2586, "*" ), ;
                           ht_glyph( 0x2587, "#" ), c8 }
        ENDIF
    ENDIF

    IF nEighths <= 0
        RETURN " "
    ENDIF

RETURN t_aGlyphs[ Min( nEighths, 8 ) ]

/** Scales values to 0 .. nTotal (eighths): nLo -> 0, nHi -> nTotal. A value
 * above nLo gets at least 1, so small values stay visible.
 */
FUNCTION ht_chartScale( aValues, nLo, nHi, nTotal )

    LOCAL aOut := {}
    LOCAL x, n

    FOR EACH x IN aValues
        IF nHi <= nLo
            n := IIF( x > nLo, nTotal, 0 )
        ELSE
            n := Round( ( x - nLo ) * nTotal / ( nHi - nLo ), 0 )
            IF n < 1 .AND. x > nLo
                n := 1
            ENDIF
        ENDIF
        AAdd( aOut, Max( 0, Min( nTotal, n ) ) )
    NEXT

RETURN aOut

/** Color of chart bars: cColor when given ("G" alone = green on the window
 * background), else the theme's progress-bar fill color as the foreground on
 * the window background (the blocks are text).
 */
FUNCTION ht_chartColor( cColor )

    LOCAL cFill, cWin, cBar, cBack

    cWin  := ht_colorPair( HTTheme():getColor( HT_CLR_WINDOW ) )
    cBack := cWin[ 2 ]

    IF HB_ISSTRING( cColor ) .AND. ! Empty( cColor )
        RETURN IIF( "/" $ cColor, cColor, AllTrim( cColor ) + "/" + cBack )
    ENDIF

    cFill := ht_colorPair( HTTheme():getColor( HT_CLR_PROGRESS_FILL ) )
    cBar  := cFill[ 2 ]                        /* the fill is drawn as background */
    IF Upper( StrTran( cBar, "*" ) ) == Upper( StrTran( cBack, "*" ) )
        cBar := cFill[ 1 ]                     /* same as the window: use its text color */
    ENDIF
    IF Upper( StrTran( cBar, "*" ) ) == Upper( StrTran( cBack, "*" ) )
        cBar := cWin[ 1 ]
    ENDIF

RETURN StrTran( cBar, "*" ) + "/" + cBack

/** Color of a full bar cell, drawn as a space: the bar color as background
 * ("B+/W" -> "B+/B*": bright backgrounds need SetBlink( .F. ), which
 * HTApplication sets).
 */
FUNCTION ht_chartFullColor( cBarColor )

    LOCAL cFore := ht_colorPair( cBarColor )[ 1 ]
    LOCAL cBack := StrTran( StrTran( cFore, "+" ), "*" )

    IF "+" $ cFore
        cBack += "*"
    ENDIF

RETURN cFore + "/" + cBack

/** Color for chart texts (labels, values): the window colors. */
FUNCTION ht_chartTextColor()
RETURN HTTheme():getColor( HT_CLR_WINDOW )

/** { cForeground, cBackground } of the first pair of a color string. */
FUNCTION ht_colorPair( cColor )

    LOCAL nAt

    cColor := AllTrim( hb_defaultValue( cColor, "W/N" ) )
    IF ( nAt := At( ",", cColor ) ) > 0
        cColor := Left( cColor, nAt - 1 )
    ENDIF
    IF ( nAt := At( "/", cColor ) ) > 0
        RETURN { AllTrim( Left( cColor, nAt - 1 ) ), AllTrim( SubStr( cColor, nAt + 1 ) ) }
    ENDIF

RETURN { cColor, "N" }

/** Repaints a chart widget in place when it has a parent widget. */
FUNCTION ht_chartRepaint( oWidget )

    LOCAL parent := oWidget:parent()

    IF parent != NIL .AND. parent:isDerivedFrom( "HTWidget" )
        parent:repaintChild( oWidget )
    ENDIF

RETURN NIL
