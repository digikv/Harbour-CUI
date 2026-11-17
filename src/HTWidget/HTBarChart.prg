/** @class HTBarChart
 * Vertical bar chart drawn with the block characters ▁▂▃▄▅▆▇█ (eight
 * levels per row): each bar barWidth cells wide, barGap cells apart, its
 * value on the row below the bars and its label on the last row. Idea
 * after Ratatui's BarChart.
 * @extends HTWidget
 *
 * Bars are { cLabel, nValue [, cColor ] }; a bar without a color uses the
 * chart color (default: the theme's progress-bar fill color). The scale runs
 * from 0 to maximum (default: the highest value). Bars that do not fit the
 * width are left out.
 *
 * @example
 *   oChart := HTBarChart():new( win )
 *   oChart:resize( 40, 10 )
 *   oChart:setBars( { { "Jan", 120 }, { "Feb", 95 }, { "Mar", 160, "R/W" } } )
 */

#include "hbtui.ch"

CLASS HTBarChart FROM HTWidget

PROTECTED:

    DATA FaBars INIT {}

PUBLIC:

    CONSTRUCTOR new( parent )

    METHOD paintEvent( paintEvent )
    METHOD setBars( aBars )
    METHOD bars() INLINE AClone( ::FaBars )
    METHOD addBar( cLabel, nValue, cColor )
    METHOD setValue( nBar, nValue )
    METHOD clear()
    METHOD levels( nRows )             /* eighths per bar for nRows bar rows */
    METHOD barRows( nHeight )          /* rows left for the bars in a widget nHeight high */
    METHOD valueText( nValue )         /* value as shown under its bar */

    PROPERTY barWidth READWRITE INIT 3
    PROPERTY barGap READWRITE INIT 1
    PROPERTY maximum READWRITE         /* NIL = the highest value */
    PROPERTY showValues READWRITE INIT .T.
    PROPERTY showLabels READWRITE INIT .T.
    PROPERTY color READWRITE           /* NIL = theme: progress fill color on the window */
    PROPERTY valueBlock READWRITE      /* {|nValue| cText } - default: compact (1.2k, 3.4M) */

ENDCLASS

/** Creates a new bar chart (40 x 10) with optional parent. */
METHOD new( parent ) CLASS HTBarChart

    IF parent != NIL
        ::super:new( parent )
    ELSE
        ::super:new()
    ENDIF

    ::setFocusPolicy( HT_FOCUS_NONE )
    ::FisVisible := .T.
    ::Fheight := 10
    ::Fwidth := 40

RETURN self

/** Replaces all bars ({ cLabel, nValue [, cColor ] } each) and repaints. */
METHOD PROCEDURE setBars( aBars ) CLASS HTBarChart

    LOCAL aBar

    ::FaBars := {}
    IF HB_ISARRAY( aBars )
        FOR EACH aBar IN aBars
            IF HB_ISARRAY( aBar ) .AND. Len( aBar ) >= 2
                AAdd( ::FaBars, { hb_CStr( aBar[ 1 ] ), IIF( HB_ISNUMERIC( aBar[ 2 ] ), aBar[ 2 ], 0 ), ;
                                  IIF( Len( aBar ) >= 3, aBar[ 3 ], NIL ) } )
            ENDIF
        NEXT
    ENDIF
    ht_chartRepaint( self )

RETURN

/** Appends a bar and repaints. */
METHOD PROCEDURE addBar( cLabel, nValue, cColor ) CLASS HTBarChart

    AAdd( ::FaBars, { hb_CStr( cLabel ), IIF( HB_ISNUMERIC( nValue ), nValue, 0 ), cColor } )
    ht_chartRepaint( self )

RETURN

/** Changes the value of bar nBar (1-based) and repaints. */
METHOD PROCEDURE setValue( nBar, nValue ) CLASS HTBarChart

    IF nBar >= 1 .AND. nBar <= Len( ::FaBars ) .AND. HB_ISNUMERIC( nValue )
        ::FaBars[ nBar, 2 ] := nValue
        ht_chartRepaint( self )
    ENDIF

RETURN

/** Removes all bars and repaints. */
METHOD PROCEDURE clear() CLASS HTBarChart

    ::FaBars := {}
    ht_chartRepaint( self )

RETURN

/** Rows for the bars: the height without the value and label rows (at least 1). */
METHOD FUNCTION barRows( nHeight ) CLASS HTBarChart
RETURN Max( 1, nHeight - IIF( ::FshowLabels, 1, 0 ) - IIF( ::FshowValues, 1, 0 ) )

/** Heights of all bars in eighths of a row (0 .. nRows * 8). */
METHOD FUNCTION levels( nRows ) CLASS HTBarChart

    LOCAL aValues := {}
    LOCAL nHi := ::Fmaximum

    AEval( ::FaBars, {| a | AAdd( aValues, a[ 2 ] ) } )
    IF nHi == NIL
        nHi := 0
        AEval( aValues, {| x | nHi := Max( nHi, x ) } )
    ENDIF

RETURN ht_chartScale( aValues, 0, nHi, Max( 1, nRows ) * 8 )

/** Text of a value under its bar: valueBlock, or a compact number that fits
 * barWidth when it can (12345 -> "12k", 2500000 -> "2.5M").
 */
METHOD FUNCTION valueText( nValue ) CLASS HTBarChart

    LOCAL cText, nAbs, aUnit, cNum

    IF ::FvalueBlock != NIL
        RETURN hb_CStr( Eval( ::FvalueBlock, nValue ) )
    ENDIF

    cText := hb_ntos( nValue )
    IF Len( cText ) <= ::FbarWidth
        RETURN cText
    ENDIF

    nAbs := Abs( nValue )
    FOR EACH aUnit IN { { 1000000000, "G" }, { 1000000, "M" }, { 1000, "k" } }
        IF nAbs >= aUnit[ 1 ]
            cNum := hb_ntos( Round( nAbs / aUnit[ 1 ], 1 ) )
            IF Len( cNum ) + 1 > ::FbarWidth .OR. Right( cNum, 2 ) == ".0"
                cNum := hb_ntos( Round( nAbs / aUnit[ 1 ], 0 ) )
            ENDIF
            RETURN IIF( nValue < 0, "-", "" ) + cNum + aUnit[ 2 ]
        ENDIF
    NEXT

RETURN cText

/** Draws bars bottom up, then the value and label rows. */
METHOD PROCEDURE paintEvent( paintEvent ) CLASS HTBarChart

    LOCAL nRows := MaxRow() + 1
    LOCAL nCols := MaxCol() + 1
    LOCAL nBarRows := ::barRows( nRows )
    LOCAL aLevels := ::levels( nBarRows )
    LOCAL cBarColor := ht_chartColor( ::Fcolor )
    LOCAL cText := ht_chartTextColor()
    LOCAL nWidth := Max( 1, ::FbarWidth )
    LOCAL nRow, nBar, nCol, nBase, cColor, cGlyph
    LOCAL nValueRow := nBarRows
    LOCAL nLabelRow := nBarRows + IIF( ::FshowValues, 1, 0 )

    HB_SYMBOL_UNUSED( paintEvent )

    /* background: the bars are text on the window color */
    FOR nRow := 0 TO nRows - 1
        DispOutAt( nRow, 0, Space( nCols ), cText )
    NEXT

    FOR nBar := 1 TO Len( ::FaBars )
        nCol := ( nBar - 1 ) * ( nWidth + Max( 0, ::FbarGap ) )
        IF nCol + nWidth > nCols
            EXIT
        ENDIF
        cColor := IIF( Empty( ::FaBars[ nBar, 3 ] ), cBarColor, ht_chartColor( ::FaBars[ nBar, 3 ] ) )

        FOR nRow := 0 TO nBarRows - 1
            nBase := ( nBarRows - 1 - nRow ) * 8
            IF aLevels[ nBar ] - nBase >= 8
                /* full cell: a space in the bar color (█ leaves gaps between rows) */
                DispOutAt( nRow, nCol, Space( nWidth ), ht_chartFullColor( cColor ) )
            ELSE
                cGlyph := ht_blockGlyph( aLevels[ nBar ] - nBase )
                IF ! cGlyph == " "
                    DispOutAt( nRow, nCol, Replicate( cGlyph, nWidth ), cColor )
                ENDIF
            ENDIF
        NEXT

        IF ::FshowValues .AND. nValueRow < nRows
            DispOutAt( nValueRow, nCol, ht_cellPadC( ht_cellLeft( ::valueText( ::FaBars[ nBar, 2 ] ), nWidth ), nWidth ), cText )
        ENDIF
        IF ::FshowLabels .AND. nLabelRow < nRows
            DispOutAt( nLabelRow, nCol, ht_cellPadC( ht_cellLeft( ::FaBars[ nBar, 1 ], nWidth ), nWidth ), cText )
        ENDIF
    NEXT

RETURN
