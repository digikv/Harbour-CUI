/*
 * Screen-cell text helpers for wide (CJK) characters.
 *
 * Under UTF8EX a Chinese, Japanese or Korean character is one character
 * (Len() = 1) but takes two screen cells: the DispOutAt remap writes it
 * followed by a filler cell (ht_viewport.c, "wide cells"). Widgets that pad,
 * cut or position text on screen therefore measure it in cells with
 * ht_strWidth() (C, ht_viewport.c) and the helpers below, and keep using
 * Len()/SubStr()/Left() for character positions in the text itself.
 *
 * With wide cells off (text GTs, ht_wideCells( .F. )) every character is one
 * cell and all helpers behave like their Len()-based counterparts.
 */

#include "hbtui.ch"

/** A symbol by its Unicode code point when the current codepage has it,
 * else cFallback: UTF8EX (the default, see HTApplication:new()) has every
 * symbol; an application on a single-byte codepage gets what that codepage
 * can show (CP437 has the box lines and the check mark, not the window
 * button symbols).
 * @param nUnicode Unicode code point
 * @param cFallback Replacement in the current codepage (usually ASCII)
 * @return Character string
 */
FUNCTION ht_glyph( nUnicode, cFallback )

    LOCAL c := hb_UChar( nUnicode )

    IF c == "" .OR. hb_UCode( c ) != nUnicode
        RETURN cFallback
    ENDIF

RETURN c

/** Longest leading part of cText that fits in nCells screen cells.
 * @param cText Text
 * @param nCells Available cells
 * @return Leading characters of cText
 */
FUNCTION ht_cellLeft( cText, nCells )

    LOCAL i, nUsed, nW

    IF nCells <= 0
        RETURN ""
    ENDIF
    IF ht_strWidth( cText ) <= nCells
        RETURN cText
    ENDIF

    nUsed := 0
    FOR i := 1 TO Len( cText )
        nW := ht_strWidth( SubStr( cText, i, 1 ) )
        IF nUsed + nW > nCells
            EXIT
        ENDIF
        nUsed += nW
    NEXT

RETURN Left( cText, i - 1 )

/** cText cut or space-padded on the right to exactly nCells screen cells
 * (PadR() in cells). A wide character that does not fit is left out.
 * @param cText Text
 * @param nCells Cells
 * @return Text taking nCells cells
 */
FUNCTION ht_cellPadR( cText, nCells )

    LOCAL cOut

    IF nCells <= 0
        RETURN ""
    ENDIF
    cOut := ht_cellLeft( cText, nCells )

RETURN cOut + Space( nCells - ht_strWidth( cOut ) )

/** cText cut or space-padded on the left to exactly nCells cells (PadL()).
 * @param cText Text
 * @param nCells Cells
 * @return Text taking nCells cells
 */
FUNCTION ht_cellPadL( cText, nCells )

    LOCAL cOut

    IF nCells <= 0
        RETURN ""
    ENDIF
    cOut := ht_cellLeft( cText, nCells )

RETURN Space( nCells - ht_strWidth( cOut ) ) + cOut

/** cText cut or centered with spaces to exactly nCells cells (PadC()).
 * @param cText Text
 * @param nCells Cells
 * @return Text taking nCells cells
 */
FUNCTION ht_cellPadC( cText, nCells )

    LOCAL cOut, nFree

    IF nCells <= 0
        RETURN ""
    ENDIF
    cOut := ht_cellLeft( cText, nCells )
    nFree := nCells - ht_strWidth( cOut )

RETURN Space( Int( nFree / 2 ) ) + cOut + Space( nFree - Int( nFree / 2 ) )

/** Character position (1-based) in cText of the character drawn on screen
 * cell nCell (0-based, counted from the first character of cText); a cell
 * past the end gives Len( cText ) + 1. Used to map mouse clicks.
 * @param cText Text as painted from its first character
 * @param nCell Cell offset
 * @return Character position
 */
FUNCTION ht_cellPos( cText, nCell )

    LOCAL i, nUsed := 0

    FOR i := 1 TO Len( cText )
        nUsed += ht_strWidth( SubStr( cText, i, 1 ) )
        IF nUsed > nCell
            RETURN i
        ENDIF
    NEXT

RETURN Len( cText ) + 1

/** Horizontal scroll offset that keeps the cursor visible: the first
 * displayed character (nOffset) moves so the character at nPos (the
 * cursor; Len()+1 = after the end) fits inside nCells cells.
 * @param cText Edited text
 * @param nOffset Current first displayed character (1-based)
 * @param nPos Cursor character position (1-based)
 * @param nCells Visible width in cells
 * @return New first displayed character
 */
FUNCTION ht_scrollToCursor( cText, nOffset, nPos, nCells )

    LOCAL nCursorW

    IF nPos < nOffset
        nOffset := nPos
    ENDIF

    nCursorW := IIF( nPos <= Len( cText ), ht_strWidth( SubStr( cText, nPos, 1 ) ), 1 )
    DO WHILE nOffset < nPos .AND. ht_cellCol( cText, nOffset, nPos ) + nCursorW > nCells
        nOffset++
    ENDDO

RETURN Max( 1, nOffset )

/** Screen column (0-based) of the character at nPos when cText is painted
 * from character nOffset.
 * @param cText Text
 * @param nOffset First displayed character (1-based)
 * @param nPos Character position (1-based)
 * @return Cell offset
 */
FUNCTION ht_cellCol( cText, nOffset, nPos )

    IF nPos <= nOffset
        RETURN nPos - nOffset
    ENDIF

    /* positions past the end of the text count one cell each */
RETURN ht_strWidth( SubStr( cText, nOffset, nPos - nOffset ) ) + ;
       Max( 0, nPos - Max( Len( cText ) + 1, nOffset ) )
