/*
 * HTCodeEdit - Harbour-CUI text editor widget for program code (CEdit)
 *
 * Copyright (c) 2026 Srđan Dragojlović - dual licensed, see LICENSE
 *
 * Shows and edits one CEBuffer (setBuffer()); the buffer keeps the text,
 * cursor, selection and undo, so switching files only swaps the buffer.
 * Tabs are shown up to the next tab stop, wide (CJK) characters take two
 * cells. Commands with F-keys (save, search, ...) belong to the program's
 * menu; the widget handles typing, cursor keys, selection and clipboard:
 *
 *   arrows, Home (first text / line start), End, PgUp, PgDn, Ctrl-Home,
 *   Ctrl-End, Ctrl-Left/Right (word), Ctrl-Up/Down (scroll)
 *   Shift + cursor keys select; F3 / Ctrl-F3 (startMarking()) select with
 *   the cursor keys alone, a vertical block with Ctrl-F3; Esc cancels
 *   Ins insert / overwrite, Enter (keeps the indent), Tab, Backspace, Del
 *   Ctrl-A all, Ctrl-C / Ctrl-Ins copy, Ctrl-X / Shift-Del cut,
 *   Ctrl-V / Shift-Ins paste, Ctrl-Y delete line,
 *   Ctrl-Z / Alt-Backspace undo, Ctrl-R redo
 *   mouse: click, drag selects, Shift+click extends, double click a word,
 *   wheel scrolls
 */

#include "hbtui.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"
#include "setcurs.ch"
#include "cedit.ch"

#define LF  Chr( 10 )

STATIC s_cBlockClip := NIL     /* clipboard text that came from a vertical block */

CLASS HTCodeEdit FROM HTWidget

EXPORTED:

    DATA oBuf
    DATA lInsert     INIT .T.
    DATA lTabSpaces  INIT .T.         /* Tab inserts spaces up to the next tab stop */
    DATA lDragging   INIT .F.
    DATA nDragRow    INIT 0
    DATA nDragCol    INIT 0
    DATA cColor      INIT ""          /* text color, "" = the theme's (HT_CLR_LINEEDIT_FOCUSED) */
    DATA cSelColor   INIT ""          /* selection, "" = the theme's */

    PROPERTY onCursor READWRITE      /* {|| ... } after a key / click: status line */
    PROPERTY onChange READWRITE      /* {|| ... } the text changed */

    CONSTRUCTOR new( parent )
    METHOD setBuffer( oBuf )
    METHOD paintEvent( event )
    METHOD keyEvent( keyEvent )
    METHOD mouseEvent( eventMouse )
    METHOD mouseDragWanted() INLINE .T.
    METHOD cursorPos()
    METHOD cursorCell()
    METHOD ensureVisible()
    METHOD refresh( lKeepView )
    METHOD visRows() INLINE Max( 1, ::Fheight )
    METHOD visCols() INLINE Max( 1, ::Fwidth )

    METHOD navigate( nKey, lExtend )
    METHOD startMarking( nMode )
    METHOD cancelMarking()
    METHOD selectAll()
    METHOD blockRange()
    METHOD selectedText()
    METHOD deleteSelection()
    METHOD typeText( cText, cKind )
    METHOD backspace()
    METHOD deleteChar()
    METHOD tabKey()
    METHOD copy()
    METHOD cut()
    METHOD paste()
    METHOD deleteLine()
    METHOD undo()
    METHOD redo()
    METHOD gotoPos( nRow, nCol )
    METHOD selectWord( nRow, nCol )
    METHOD wordAtCursor()
    METHOD matchBracket()
    METHOD lineState( nLine, hDef, aClr )
    METHOD changed()
    METHOD canEdit() INLINE ::oBuf != NIL .AND. ! ::oBuf:lReadOnly

ENDCLASS

METHOD new( parent ) CLASS HTCodeEdit
    ::super:new( parent )
    ::setFocusPolicy( HT_FOCUS_STRONG )
    ::FisVisible := .T.
RETURN self

METHOD setBuffer( oBuf ) CLASS HTCodeEdit
    ::oBuf := oBuf
    ::lDragging := .F.
RETURN NIL

/*----------------------------------------------------------- painting */

METHOD PROCEDURE paintEvent( event ) CLASS HTCodeEdit

    LOCAL nRows := MaxRow() + 1, nCols := MaxCol() + 1, nRow, nLine
    LOCAL cBase := IIF( Empty( ::cColor ), HTTheme():getColor( HT_CLR_LINEEDIT_FOCUSED ), ::cColor )
    LOCAL cSel := IIF( Empty( ::cSelColor ), HTTheme():getColor( HT_CLR_LINEEDIT_SELECTED ), ::cSelColor )
    LOCAL oB := ::oBuf, aSel, hDef, aClr, aCh, aColors

    HB_SYMBOL_UNUSED( event )

    IF oB == NIL
        FOR nRow := 0 TO nRows - 1
            DispOutAt( nRow, 0, Space( nCols ), cBase )
        NEXT
        RETURN
    ENDIF

    /* { nMode, nR1, n1, nR2, n2 }: stream = character columns, block = cells */
    aSel := NIL
    IF oB:hasSel()
        IF oB:nSelMode == CE_SEL_BLOCK
            aSel := { CE_SEL_BLOCK }
            AEval( ::blockRange(), {| x | AAdd( aSel, x ) } )
        ELSE
            aSel := { CE_SEL_STREAM }
            AEval( oB:streamRange(), {| x | AAdd( aSel, x ) } )
        ENDIF
    ENDIF

    /* syntax highlighting (cesyntax.prg) */
    hDef := IIF( oB:lHilite .AND. ! Empty( oB:cSyntax ), ce_SyntaxDef( oB:cSyntax ), NIL )
    IF hDef != NIL
        aClr := ce_SyntaxColors( cBase )
    ENDIF

    FOR nRow := 0 TO nRows - 1
        nLine := oB:nTop + nRow
        IF nLine <= oB:lineCount()
            aCh := ce_Chars( oB:aLines[ nLine ] )
            aColors := IIF( hDef == NIL, NIL, ce_Colorize( aCh, ::lineState( nLine, hDef, aClr ), hDef, aClr )[ 1 ] )
            ce_PaintLine( nRow, aCh, aColors, nLine, oB:nLeft, nCols, oB:nTab, cBase, cSel, aSel )
        ELSE
            DispOutAt( nRow, 0, Space( nCols ), cBase )
        ENDIF
    NEXT

    SetCursor( IIF( ::lInsert, SC_NORMAL, SC_SPECIAL1 ) )

RETURN

/* the cursor in widget coordinates, NIL when it is outside the view */
METHOD cursorPos() CLASS HTCodeEdit

    LOCAL nRow, nCell

    IF ::oBuf == NIL .OR. ! ::hasFocus()
        RETURN NIL
    ENDIF
    nRow := ::oBuf:nRow - ::oBuf:nTop
    nCell := ::cursorCell() - ::oBuf:nLeft
    IF nRow >= 0 .AND. nRow < ::Fheight .AND. nCell >= 0 .AND. nCell < ::Fwidth
        RETURN { nRow, nCell }
    ENDIF

RETURN NIL

/* screen cell of the cursor in its line (0-based, before scrolling) */
METHOD cursorCell() CLASS HTCodeEdit
RETURN ce_ColToCell( ::oBuf:line( ::oBuf:nRow ), ::oBuf:nCol, ::oBuf:nTab )

/* scrolls so that the cursor is on the screen */
METHOD ensureVisible() CLASS HTCodeEdit

    LOCAL oB := ::oBuf, nRows := ::visRows(), nCols := ::visCols(), nCell

    IF oB == NIL
        RETURN NIL
    ENDIF
    IF oB:nRow < oB:nTop
        oB:nTop := oB:nRow
    ELSEIF oB:nRow >= oB:nTop + nRows
        oB:nTop := oB:nRow - nRows + 1
    ENDIF
    oB:nTop := Max( 1, oB:nTop )
    nCell := ::cursorCell()
    IF nCell < oB:nLeft
        oB:nLeft := Max( 0, nCell - Int( nCols / 4 ) )
    ELSEIF nCell >= oB:nLeft + nCols
        oB:nLeft := nCell - nCols + 1 + Int( nCols / 4 )
    ENDIF

RETURN NIL

/* repaints, keeps the cursor on the screen (unless lKeepView: wheel), updates the status */
METHOD refresh( lKeepView ) CLASS HTCodeEdit

    LOCAL parent := ::parent()

    IF ! hb_defaultValue( lKeepView, .F. )
        ::ensureVisible()
    ENDIF
    IF parent != NIL .AND. parent:isDerivedFrom( "HTWidget" )
        parent:repaintChild( self )
    ENDIF
    IF ::FonCursor != NIL
        Eval( ::FonCursor )
    ENDIF

RETURN NIL

METHOD changed() CLASS HTCodeEdit
    IF ::FonChange != NIL
        Eval( ::FonChange )
    ENDIF
RETURN NIL

/*----------------------------------------------------------- keys */

METHOD PROCEDURE keyEvent( keyEvent ) CLASS HTCodeEdit

    LOCAL nKey := keyEvent:key, oB := ::oBuf, cText
    LOCAL nShifts := hb_gtInfo( HB_GTI_KBDSHIFTS )
    LOCAL lCtrl := hb_bitAnd( nShifts, HB_GTI_KBD_CTRL ) != 0
    LOCAL lShift := hb_bitAnd( nShifts, HB_GTI_KBD_SHIFT ) != 0

    IF oB == NIL
        RETURN
    ENDIF

    /* Ctrl + a letter: these codes are also cursor keys (K_CTRL_C = K_PGDN ...) */
    IF lCtrl .AND. nKey < 32
        SWITCH nKey
        CASE 26                        /* Ctrl-Z, or Ctrl-Left (the same code) */
            IF ce_KeyDown( VK_KEY_Z )
                ::undo()
                keyEvent:accept()
                RETURN
            ENDIF
            EXIT
        CASE 2                         /* Ctrl-B (matching bracket), or Ctrl-Right */
            IF ce_KeyDown( VK_KEY_B )
                ::matchBracket()
                keyEvent:accept()
                RETURN
            ENDIF
            EXIT
        CASE K_CTRL_A
            ::selectAll()
            keyEvent:accept()
            RETURN
        CASE K_CTRL_C
            ::copy()
            keyEvent:accept()
            RETURN
        CASE K_CTRL_X
            ::cut()
            keyEvent:accept()
            RETURN
        CASE K_CTRL_V
            ::paste()
            keyEvent:accept()
            RETURN
        CASE K_CTRL_Y
            ::deleteLine()
            keyEvent:accept()
            RETURN
        CASE K_CTRL_R
            ::redo()
            keyEvent:accept()
            RETURN
        CASE K_CTRL_HOME
        CASE K_CTRL_END
        CASE K_CTRL_PGUP
        CASE K_CTRL_PGDN
            EXIT
        OTHERWISE
            RETURN                     /* other Ctrl-letters are the program's */
        ENDSWITCH
    ENDIF

    SWITCH nKey
    CASE K_UP
    CASE K_DOWN
    CASE K_LEFT
    CASE K_RIGHT
    CASE K_HOME
    CASE K_END
    CASE K_PGUP
    CASE K_PGDN
    CASE K_CTRL_HOME
    CASE K_CTRL_END
    CASE K_CTRL_LEFT
    CASE K_CTRL_RIGHT
    CASE K_CTRL_PGUP
    CASE K_CTRL_PGDN
        ::navigate( nKey, lShift .OR. oB:lMarking )
        EXIT
    CASE K_CTRL_UP
        oB:nTop := Max( 1, oB:nTop - 1 )
        oB:nRow := Min( oB:nRow, oB:nTop + ::visRows() - 1 )
        oB:nCol := Min( oB:nCol, Len( oB:line( oB:nRow ) ) + 1 )
        EXIT
    CASE K_CTRL_DOWN
        oB:nTop := Min( Max( 1, oB:lineCount() - ::visRows() + 1 ), oB:nTop + 1 )
        oB:nRow := Max( oB:nRow, oB:nTop )
        oB:nCol := Min( oB:nCol, Len( oB:line( oB:nRow ) ) + 1 )
        EXIT
    CASE K_CTRL_INS
        ::copy()
        EXIT
    CASE K_INS                         /* Shift-Ins paste, Ins insert / overwrite */
        IF lShift
            ::paste()
        ELSE
            ::lInsert := ! ::lInsert
        ENDIF
        EXIT
    CASE K_DEL                         /* Shift-Del cut */
        IF lShift
            ::cut()
        ELSE
            ::deleteChar()
        ENDIF
        EXIT
    CASE K_BS
        ::backspace()
        EXIT
    CASE K_ALT_BS
        ::undo()
        EXIT
    CASE K_ENTER
        IF ::canEdit()
            cText := oB:line( oB:nRow )
            cText := Left( cText, Min( oB:nCol - 1, Len( cText ) - Len( LTrim( cText ) ) ) )
            ::typeText( LF + cText, "edit" )
        ENDIF
        EXIT
    CASE K_TAB
        ::tabKey()
        EXIT
    OTHERWISE
        cText := ht_keyText( nKey )
        IF cText == ""
            RETURN                     /* not ours: the program's keys */
        ENDIF
        ::typeText( cText, "type" )
    ENDSWITCH

    keyEvent:accept()
    ::refresh()

RETURN

/* moves the cursor; lExtend: Shift or F3 marking extends the selection */
METHOD navigate( nKey, lExtend ) CLASS HTCodeEdit

    LOCAL oB := ::oBuf, nRows := ::visRows(), cLine, nFirst, lKeepCell := .F.

    IF lExtend
        IF oB:nSelMode == CE_SEL_NONE
            oB:nSelMode := CE_SEL_STREAM
            oB:nSelRow := oB:nRow
            oB:nSelCol := oB:nCol
        ENDIF
    ELSEIF oB:nSelMode != CE_SEL_NONE
        oB:clearSel()
    ENDIF

    cLine := oB:line( oB:nRow )
    SWITCH nKey
    CASE K_UP
        IF oB:nRow > 1
            oB:nRow--
        ENDIF
        lKeepCell := .T.
        EXIT
    CASE K_DOWN
        IF oB:nRow < oB:lineCount()
            oB:nRow++
        ENDIF
        lKeepCell := .T.
        EXIT
    CASE K_PGUP
        oB:nRow := Max( 1, oB:nRow - nRows + 1 )
        oB:nTop := Max( 1, oB:nTop - nRows + 1 )
        lKeepCell := .T.
        EXIT
    CASE K_PGDN
        oB:nRow := Min( oB:lineCount(), oB:nRow + nRows - 1 )
        oB:nTop := Min( Max( 1, oB:lineCount() - nRows + 1 ), oB:nTop + nRows - 1 )
        lKeepCell := .T.
        EXIT
    CASE K_CTRL_PGUP
        oB:nRow := oB:nTop
        lKeepCell := .T.
        EXIT
    CASE K_CTRL_PGDN
        oB:nRow := Min( oB:lineCount(), oB:nTop + nRows - 1 )
        lKeepCell := .T.
        EXIT
    CASE K_LEFT
        IF oB:nCol > 1
            oB:nCol--
        ELSEIF oB:nRow > 1
            oB:nRow--
            oB:nCol := Len( oB:line( oB:nRow ) ) + 1
        ENDIF
        EXIT
    CASE K_RIGHT
        IF oB:nCol <= Len( cLine )
            oB:nCol++
        ELSEIF oB:nRow < oB:lineCount()
            oB:nRow++
            oB:nCol := 1
        ENDIF
        EXIT
    CASE K_HOME                        /* first text of the line, again: line start */
        nFirst := Len( cLine ) - Len( LTrim( cLine ) ) + 1
        oB:nCol := IIF( oB:nCol == nFirst, 1, nFirst )
        EXIT
    CASE K_END
        oB:nCol := Len( cLine ) + 1
        EXIT
    CASE K_CTRL_HOME
        oB:nRow := 1
        oB:nCol := 1
        EXIT
    CASE K_CTRL_END
        oB:nRow := oB:lineCount()
        oB:nCol := Len( oB:line( oB:nRow ) ) + 1
        EXIT
    CASE K_CTRL_LEFT
        IF oB:nCol == 1 .AND. oB:nRow > 1
            oB:nRow--
            oB:nCol := Len( oB:line( oB:nRow ) ) + 1
        ELSE
            oB:nCol := ce_WordLeft( cLine, oB:nCol )
        ENDIF
        EXIT
    CASE K_CTRL_RIGHT
        IF oB:nCol > Len( cLine ) .AND. oB:nRow < oB:lineCount()
            oB:nRow++
            oB:nCol := 1
        ELSE
            oB:nCol := ce_WordRight( cLine, oB:nCol )
        ENDIF
        EXIT
    ENDSWITCH

    IF lKeepCell
        IF oB:nWantCell < 0
            oB:nWantCell := ce_ColToCell( cLine, oB:nCol, oB:nTab )
        ENDIF
        oB:nCol := ce_CellToCol( oB:line( oB:nRow ), oB:nWantCell, oB:nTab )
    ELSE
        oB:nWantCell := -1
    ENDIF
    oB:cLastKind := ""

RETURN NIL

/* F3 (stream) / Ctrl-F3 (vertical block): the cursor keys select; pressed
   again: the marking stops, the selection stays */
METHOD startMarking( nMode ) CLASS HTCodeEdit

    LOCAL oB := ::oBuf

    IF oB == NIL
        RETURN NIL
    ENDIF
    IF oB:lMarking
        oB:lMarking := .F.
    ELSE
        oB:lMarking := .T.
        oB:nSelMode := nMode
        oB:nSelRow := oB:nRow
        oB:nSelCol := oB:nCol
    ENDIF
    ::refresh()

RETURN NIL

/* Esc: ends the marking / removes the selection; .F. if there was none */
METHOD cancelMarking() CLASS HTCodeEdit
    IF ::oBuf != NIL .AND. ( ::oBuf:lMarking .OR. ::oBuf:nSelMode != CE_SEL_NONE )
        ::oBuf:clearSel()
        ::refresh()
        RETURN .T.
    ENDIF
RETURN .F.

METHOD selectAll() CLASS HTCodeEdit
    LOCAL oB := ::oBuf
    oB:nSelMode := CE_SEL_STREAM
    oB:nSelRow := 1
    oB:nSelCol := 1
    oB:lMarking := .F.
    oB:nRow := oB:lineCount()
    oB:nCol := Len( oB:line( oB:nRow ) ) + 1
    ::refresh()
RETURN NIL

/* vertical block { nR1, nCell1, nR2, nCell2 } (cells: nCell1 <= x < nCell2) */
METHOD blockRange() CLASS HTCodeEdit

    LOCAL oB := ::oBuf
    LOCAL nC1 := ce_ColToCell( oB:line( oB:nSelRow ), oB:nSelCol, oB:nTab )
    LOCAL nC2 := ::cursorCell()

RETURN { Min( oB:nSelRow, oB:nRow ), Min( nC1, nC2 ), Max( oB:nSelRow, oB:nRow ), Max( nC1, nC2 ) + 1 }

METHOD selectedText() CLASS HTCodeEdit

    LOCAL oB := ::oBuf, aB, a := {}, i, cLine

    IF ! oB:hasSel()
        RETURN ""
    ENDIF
    IF oB:nSelMode != CE_SEL_BLOCK
        RETURN oB:selText()
    ENDIF
    aB := ::blockRange()
    FOR i := aB[ 1 ] TO aB[ 3 ]
        cLine := oB:line( i )
        AAdd( a, SubStr( cLine, ce_CellToCol( cLine, aB[ 2 ], oB:nTab ), ;
                         ce_CellToCol( cLine, aB[ 4 ], oB:nTab ) - ce_CellToCol( cLine, aB[ 2 ], oB:nTab ) ) )
    NEXT

RETURN ce_Join( a, LF )

/* deletes the selection (inside an action already started) */
METHOD deleteSelection() CLASS HTCodeEdit

    LOCAL oB := ::oBuf, a, i, cLine, nC1, nC2

    IF ! oB:hasSel()
        oB:clearSel()
        RETURN NIL
    ENDIF
    IF oB:nSelMode == CE_SEL_BLOCK
        a := ::blockRange()
        FOR i := a[ 1 ] TO a[ 3 ]
            cLine := oB:line( i )
            nC1 := ce_CellToCol( cLine, a[ 2 ], oB:nTab )
            nC2 := ce_CellToCol( cLine, a[ 4 ], oB:nTab )
            oB:deleteRange( i, nC1, i, nC2 )
        NEXT
        oB:nRow := a[ 1 ]
        oB:nCol := ce_CellToCol( oB:line( a[ 1 ] ), a[ 2 ], oB:nTab )
    ELSE
        a := oB:streamRange()
        oB:deleteRange( a[ 1 ], a[ 2 ], a[ 3 ], a[ 4 ] )
        oB:nRow := a[ 1 ]
        oB:nCol := a[ 2 ]
    ENDIF
    oB:clearSel()

RETURN NIL

/* types / inserts cText at the cursor: replaces the selection, overwrites in
   overwrite mode (one character, not at the line end) */
METHOD typeText( cText, cKind ) CLASS HTCodeEdit

    LOCAL oB := ::oBuf, aEnd

    IF ! ::canEdit()
        RETURN NIL
    ENDIF
    IF oB:hasSel()
        cKind := "edit"
    ENDIF
    oB:startAction( cKind )
    ::deleteSelection()
    IF ! ::lInsert .AND. cKind == "type" .AND. oB:nCol <= Len( oB:line( oB:nRow ) )
        oB:deleteRange( oB:nRow, oB:nCol, oB:nRow, oB:nCol + 1 )
    ENDIF
    aEnd := oB:insertText( oB:nRow, oB:nCol, cText )
    oB:nRow := aEnd[ 1 ]
    oB:nCol := aEnd[ 2 ]
    oB:nWantCell := -1
    oB:endAction( cKind )
    ::changed()

RETURN NIL

METHOD backspace() CLASS HTCodeEdit

    LOCAL oB := ::oBuf

    IF ! ::canEdit()
        RETURN NIL
    ENDIF
    IF oB:hasSel()
        oB:startAction( "edit" )
        ::deleteSelection()
        oB:endAction( "edit" )
    ELSEIF oB:nCol > 1
        oB:startAction( "bs" )
        oB:deleteRange( oB:nRow, oB:nCol - 1, oB:nRow, oB:nCol )
        oB:nCol--
        oB:endAction( "bs" )
    ELSEIF oB:nRow > 1
        oB:startAction( "edit" )
        oB:nCol := Len( oB:line( oB:nRow - 1 ) ) + 1
        oB:deleteRange( oB:nRow - 1, oB:nCol, oB:nRow, 1 )
        oB:nRow--
        oB:endAction( "edit" )
    ELSE
        RETURN NIL
    ENDIF
    oB:nWantCell := -1
    ::changed()

RETURN NIL

METHOD deleteChar() CLASS HTCodeEdit

    LOCAL oB := ::oBuf

    IF ! ::canEdit()
        RETURN NIL
    ENDIF
    IF oB:hasSel()
        oB:startAction( "edit" )
        ::deleteSelection()
        oB:endAction( "edit" )
    ELSEIF oB:nCol <= Len( oB:line( oB:nRow ) )
        oB:startAction( "del" )
        oB:deleteRange( oB:nRow, oB:nCol, oB:nRow, oB:nCol + 1 )
        oB:endAction( "del" )
    ELSEIF oB:nRow < oB:lineCount()
        oB:startAction( "edit" )
        oB:deleteRange( oB:nRow, oB:nCol, oB:nRow + 1, 1 )
        oB:endAction( "edit" )
    ELSE
        RETURN NIL
    ENDIF
    ::changed()

RETURN NIL

/* Tab: spaces (or a tab) to the next tab stop; with a selection of several
   lines it indents them */
METHOD tabKey() CLASS HTCodeEdit

    LOCAL oB := ::oBuf, a, i, nLast, cIndent

    IF ! ::canEdit()
        RETURN NIL
    ENDIF
    cIndent := IIF( ::lTabSpaces, Space( oB:nTab ), Chr( 9 ) )
    a := oB:streamRange()
    IF a != NIL .AND. oB:nSelMode == CE_SEL_STREAM .AND. a[ 1 ] != a[ 3 ]
        nLast := IIF( a[ 4 ] == 1, a[ 3 ] - 1, a[ 3 ] )
        oB:startAction( "edit" )
        FOR i := a[ 1 ] TO nLast
            IF Len( oB:line( i ) ) > 0
                oB:insertText( i, 1, cIndent )
            ENDIF
        NEXT
        oB:endAction( "edit" )
        oB:nSelCol := IIF( oB:nSelCol > 1 .AND. Len( oB:line( oB:nSelRow ) ) > 0, oB:nSelCol + Len( cIndent ), oB:nSelCol )
        oB:nCol := IIF( oB:nCol > 1 .AND. Len( oB:line( oB:nRow ) ) > 0, oB:nCol + Len( cIndent ), oB:nCol )
        ::changed()
        RETURN NIL
    ENDIF
    IF ::lTabSpaces
        ::typeText( Space( oB:nTab - Int( ::cursorCell() % oB:nTab ) ), "edit" )
    ELSE
        ::typeText( Chr( 9 ), "edit" )
    ENDIF

RETURN NIL

/* copies the selection (without one: the current line) */
METHOD copy() CLASS HTCodeEdit

    LOCAL oB := ::oBuf, cText

    IF oB:hasSel()
        cText := ::selectedText()
        s_cBlockClip := IIF( oB:nSelMode == CE_SEL_BLOCK, cText, NIL )
    ELSE
        cText := oB:line( oB:nRow ) + LF
        s_cBlockClip := NIL
    ENDIF
    hb_gtInfo( HB_GTI_CLIPBOARDDATA, StrTran( cText, LF, hb_eol() ) )
    oB:lMarking := .F.
    ::refresh()

RETURN NIL

METHOD cut() CLASS HTCodeEdit
    IF ! ::canEdit()
        RETURN NIL
    ENDIF
    IF ::oBuf:hasSel()
        ::copy()
        ::oBuf:startAction( "edit" )
        ::deleteSelection()
        ::oBuf:endAction( "edit" )
        ::changed()
        ::refresh()
    ELSE
        ::copy()
        ::deleteLine()
    ENDIF
RETURN NIL

/* pastes the clipboard; a vertical block copied here goes in as a block */
METHOD paste() CLASS HTCodeEdit

    LOCAL oB := ::oBuf, cText, aLines, nCell, i, nRow, cLine, nCol

    IF ! ::canEdit()
        RETURN NIL
    ENDIF
    cText := hb_gtInfo( HB_GTI_CLIPBOARDDATA )
    IF ! HB_ISSTRING( cText ) .OR. Len( cText ) == 0
        RETURN NIL
    ENDIF
    cText := StrTran( StrTran( cText, Chr( 13 ) + Chr( 10 ), LF ), Chr( 13 ), LF )

    IF s_cBlockClip != NIL .AND. cText == s_cBlockClip
        oB:startAction( "edit" )
        ::deleteSelection()
        aLines := hb_ATokens( cText, LF )
        nCell := ::cursorCell()
        FOR i := 1 TO Len( aLines )
            nRow := oB:nRow + i - 1
            IF nRow > oB:lineCount()
                oB:insertText( nRow - 1, Len( oB:line( nRow - 1 ) ) + 1, LF )
            ENDIF
            cLine := oB:line( nRow )
            IF ce_ColToCell( cLine, Len( cLine ) + 1, oB:nTab ) < nCell
                oB:insertText( nRow, Len( cLine ) + 1, Space( nCell - ce_ColToCell( cLine, Len( cLine ) + 1, oB:nTab ) ) )
                cLine := oB:line( nRow )
            ENDIF
            nCol := ce_CellToCol( cLine, nCell, oB:nTab )
            oB:insertText( nRow, nCol, aLines[ i ] )
        NEXT
        oB:endAction( "edit" )
    ELSE
        ::typeText( cText, "edit" )
    ENDIF
    ::changed()
    ::refresh()

RETURN NIL

/* Ctrl-Y */
METHOD deleteLine() CLASS HTCodeEdit

    LOCAL oB := ::oBuf

    IF ! ::canEdit()
        RETURN NIL
    ENDIF
    oB:clearSel()
    oB:startAction( "edit" )
    IF oB:nRow < oB:lineCount()
        oB:deleteRange( oB:nRow, 1, oB:nRow + 1, 1 )
    ELSEIF oB:nRow > 1
        oB:deleteRange( oB:nRow - 1, Len( oB:line( oB:nRow - 1 ) ) + 1, oB:nRow, Len( oB:line( oB:nRow ) ) + 1 )
        oB:nRow--
    ELSE
        oB:deleteRange( 1, 1, 1, Len( oB:line( 1 ) ) + 1 )
    ENDIF
    oB:nCol := ce_CellToCol( oB:line( oB:nRow ), Max( oB:nWantCell, 0 ), oB:nTab )
    oB:endAction( "edit" )
    ::changed()
    ::refresh()

RETURN NIL

METHOD undo() CLASS HTCodeEdit
    IF ::canEdit() .AND. ::oBuf:undo()
        ::changed()
    ENDIF
    ::refresh()
RETURN NIL

METHOD redo() CLASS HTCodeEdit
    IF ::canEdit() .AND. ::oBuf:redo()
        ::changed()
    ENDIF
    ::refresh()
RETURN NIL

/* cursor to a line / column (clamped), without a selection */
METHOD gotoPos( nRow, nCol ) CLASS HTCodeEdit
    LOCAL oB := ::oBuf
    oB:clearSel()
    oB:nRow := Max( 1, Min( nRow, oB:lineCount() ) )
    oB:nCol := Max( 1, Min( nCol, Len( oB:line( oB:nRow ) ) + 1 ) )
    oB:nWantCell := -1
    oB:cLastKind := ""
    ::refresh()
RETURN NIL

/* selects the word at nRow, nCol (double click) */
METHOD selectWord( nRow, nCol ) CLASS HTCodeEdit

    LOCAL oB := ::oBuf, cLine := oB:line( nRow ), n1 := nCol, n2 := nCol

    IF nCol > Len( cLine ) .OR. ! ce_IsWordChar( SubStr( cLine, nCol, 1 ) )
        RETURN NIL
    ENDIF
    DO WHILE n1 > 1 .AND. ce_IsWordChar( SubStr( cLine, n1 - 1, 1 ) )
        n1--
    ENDDO
    DO WHILE n2 <= Len( cLine ) .AND. ce_IsWordChar( SubStr( cLine, n2, 1 ) )
        n2++
    ENDDO
    oB:nSelMode := CE_SEL_STREAM
    oB:nSelRow := nRow
    oB:nSelCol := n1
    oB:nRow := nRow
    oB:nCol := n2

RETURN NIL

/* the word at the cursor (or right before it), "" if none */
METHOD wordAtCursor() CLASS HTCodeEdit

    LOCAL oB := ::oBuf, cLine := oB:line( oB:nRow ), n1 := oB:nCol, n2

    IF ! ce_IsWordChar( SubStr( cLine, n1, 1 ) ) .AND. n1 > 1 .AND. ce_IsWordChar( SubStr( cLine, n1 - 1, 1 ) )
        n1--
    ENDIF
    IF ! ce_IsWordChar( SubStr( cLine, n1, 1 ) )
        RETURN ""
    ENDIF
    DO WHILE n1 > 1 .AND. ce_IsWordChar( SubStr( cLine, n1 - 1, 1 ) )
        n1--
    ENDDO
    n2 := n1
    DO WHILE n2 <= Len( cLine ) .AND. ce_IsWordChar( SubStr( cLine, n2, 1 ) )
        n2++
    ENDDO

RETURN SubStr( cLine, n1, n2 - n1 )

/* Ctrl-B: to the pair of the bracket at the cursor (or right before it) */
METHOD matchBracket() CLASS HTCodeEdit

    LOCAL oB := ::oBuf, cPairs := "()[]{}", cLine := oB:line( oB:nRow ), nCol := oB:nCol
    LOCAL c, p, cOpen, cClose, nDir, nDepth := 0, nRow, aCh, i, nLines := 0

    c := SubStr( cLine, nCol, 1 )
    IF Empty( c ) .OR. !( c $ cPairs )
        nCol--
        c := SubStr( cLine, nCol, 1 )
        IF nCol < 1 .OR. Empty( c ) .OR. !( c $ cPairs )
            RETURN .F.
        ENDIF
    ENDIF
    p := At( c, cPairs )
    IF p % 2 == 1
        cOpen := c
        cClose := SubStr( cPairs, p + 1, 1 )
        nDir := 1
    ELSE
        cOpen := SubStr( cPairs, p - 1, 1 )
        cClose := c
        nDir := -1
    ENDIF

    nRow := oB:nRow
    aCh := ce_Chars( cLine )
    i := nCol
    DO WHILE nLines < 20000
        DO WHILE i >= 1 .AND. i <= Len( aCh )
            IF aCh[ i ] == cOpen
                nDepth += nDir
            ELSEIF aCh[ i ] == cClose
                nDepth -= nDir
            ENDIF
            IF nDepth == 0
                oB:pushBack()
                ::gotoPos( nRow, i )
                RETURN .T.
            ENDIF
            i += nDir
        ENDDO
        nRow += nDir
        IF nRow < 1 .OR. nRow > oB:lineCount()
            EXIT
        ENDIF
        aCh := ce_Chars( oB:aLines[ nRow ] )
        i := IIF( nDir > 0, 1, Len( aCh ) )
        nLines++
    ENDDO

RETURN .F.

/* syntax state at the start of nLine; the states of the lines before it are
   computed once and kept in the buffer until a change above them */
METHOD lineState( nLine, hDef, aClr ) CLASS HTCodeEdit

    LOCAL oB := ::oBuf, n

    DO WHILE oB:nStateValid < nLine
        n := oB:nStateValid
        IF Len( oB:aStates ) <= n
            ASize( oB:aStates, n + 64 )
        ENDIF
        oB:aStates[ n + 1 ] := ce_Colorize( ce_Chars( oB:aLines[ n ] ), oB:aStates[ n ], hDef, aClr )[ 2 ]
        oB:nStateValid++
    ENDDO

RETURN oB:aStates[ nLine ]

/*----------------------------------------------------------- mouse */

METHOD PROCEDURE mouseEvent( eventMouse ) CLASS HTCodeEdit

    LOCAL oB := ::oBuf, nR := eventMouse:mouseRow, nC := eventMouse:mouseCol
    LOCAL nLine, nCol, lShift

    IF oB == NIL
        RETURN
    ENDIF
    nLine := Max( 1, Min( oB:nTop + nR, oB:lineCount() ) )
    nCol := ce_CellToCol( oB:line( nLine ), oB:nLeft + Max( 0, nC ), oB:nTab )

    SWITCH eventMouse:nKey
    CASE K_LBUTTONDOWN
        lShift := hb_bitAnd( hb_gtInfo( HB_GTI_KBDSHIFTS ), HB_GTI_KBD_SHIFT ) != 0
        IF lShift
            IF oB:nSelMode == CE_SEL_NONE
                oB:nSelMode := CE_SEL_STREAM
                oB:nSelRow := oB:nRow
                oB:nSelCol := oB:nCol
            ENDIF
        ELSE
            oB:clearSel()
            ::lDragging := .T.
            ::nDragRow := nLine
            ::nDragCol := nCol
        ENDIF
        oB:nRow := nLine
        oB:nCol := nCol
        oB:nWantCell := -1
        oB:cLastKind := ""
        ::refresh()
        EXIT
    CASE K_MOUSEMOVE
        IF ::lDragging
            /* above / under the view: scroll a line */
            IF nR < 0 .AND. oB:nTop > 1
                oB:nTop--
            ELSEIF nR >= ::Fheight .AND. oB:nTop + ::Fheight <= oB:lineCount()
                oB:nTop++
            ENDIF
            nLine := Max( 1, Min( oB:nTop + Max( Min( nR, ::Fheight - 1 ), 0 ), oB:lineCount() ) )
            nCol := ce_CellToCol( oB:line( nLine ), oB:nLeft + Max( 0, nC ), oB:nTab )
            IF nLine != ::nDragRow .OR. nCol != ::nDragCol
                oB:nSelMode := CE_SEL_STREAM
                oB:nSelRow := ::nDragRow
                oB:nSelCol := ::nDragCol
            ENDIF
            oB:nRow := nLine
            oB:nCol := nCol
            ::refresh()
        ENDIF
        EXIT
    CASE K_LBUTTONUP
        ::lDragging := .F.
        EXIT
    CASE K_LDBLCLK
        ::lDragging := .F.
        ::selectWord( nLine, nCol )
        ::refresh()
        EXIT
    CASE K_MWFORWARD
        oB:nTop := Max( 1, oB:nTop - 3 )
        ::refresh( .T. )
        EXIT
    CASE K_MWBACKWARD
        oB:nTop := Min( Max( 1, oB:lineCount() - ::Fheight + 1 ), oB:nTop + 3 )
        ::refresh( .T. )
        EXIT
    ENDSWITCH

RETURN

/*----------------------------------------------------------- helpers */

/* one screen row: the visible cells of cLine, the selection in cSel */
STATIC PROCEDURE ce_PaintLine( nRow, aCh, aColors, nLine, nLeft, nCols, nTab, cBase, cSel, aSel )

    LOCAL nCell := 0, i, ch, nW, cClr, cOut
    LOCAL cRun := "", cRunClr := NIL, nRunCell := 0, nEnd, lSel, nStart

    FOR i := 1 TO Len( aCh )
        ch := aCh[ i ]
        nW := IIF( ch == Chr( 9 ), nTab - Int( nCell % nTab ), IIF( hb_BLen( ch ) == 1, 1, ht_strWidth( ch ) ) )
        IF nCell + nW > nLeft
            lSel := .F.
            IF aSel != NIL .AND. nLine >= aSel[ 2 ] .AND. nLine <= aSel[ 4 ]
                IF aSel[ 1 ] == CE_SEL_BLOCK
                    lSel := nCell >= aSel[ 3 ] .AND. nCell < aSel[ 5 ]
                ELSE
                    lSel := ( nLine > aSel[ 2 ] .OR. i >= aSel[ 3 ] ) .AND. ( nLine < aSel[ 4 ] .OR. i < aSel[ 5 ] )
                ENDIF
            ENDIF
            cClr := IIF( lSel, cSel, IIF( aColors == NIL, cBase, aColors[ i ] ) )
            nStart := Max( nCell, nLeft )
            IF ch == Chr( 9 ) .OR. nCell < nLeft .OR. nCell + nW > nLeft + nCols
                /* tab, or a wide character cut by the edge: spaces */
                cOut := Space( Min( nCell + nW, nLeft + nCols ) - nStart )
            ELSE
                cOut := ch
            ENDIF
            IF cRunClr != NIL .AND. cClr == cRunClr
                cRun += cOut
            ELSE
                IF cRunClr != NIL
                    DispOutAt( nRow, nRunCell, cRun, cRunClr )
                ENDIF
                cRun := cOut
                cRunClr := cClr
                nRunCell := nStart - nLeft
            ENDIF
        ENDIF
        nCell += nW
        IF nCell >= nLeft + nCols
            EXIT
        ENDIF
    NEXT
    IF cRunClr != NIL
        DispOutAt( nRow, nRunCell, cRun, cRunClr )
    ENDIF

    nEnd := Max( nCell, nLeft ) - nLeft
    IF nEnd < nCols
        /* a selected line break: one selected cell after the text */
        IF aSel != NIL .AND. aSel[ 1 ] == CE_SEL_STREAM .AND. nLine >= aSel[ 2 ] .AND. nLine < aSel[ 4 ] .AND. ;
           ( nLine > aSel[ 2 ] .OR. Len( aCh ) + 1 >= aSel[ 3 ] )
            DispOutAt( nRow, nEnd, " ", cSel )
            nEnd++
        ENDIF
        IF nEnd < nCols
            DispOutAt( nRow, nEnd, Space( nCols - nEnd ), cBase )
        ENDIF
    ENDIF

RETURN

/* the characters of a UTF-8 string (SubStr( c, i, 1 ) in a loop would read
   the string from the start every time) */
FUNCTION ce_Chars( cLine )

    LOCAL a := {}, n := hb_BLen( cLine ), i := 1, b, k

    DO WHILE i <= n
        b := hb_BPeek( cLine, i )
        k := IIF( b < 0xC0, 1, IIF( b < 0xE0, 2, IIF( b < 0xF0, 3, 4 ) ) )
        AAdd( a, hb_BSubStr( cLine, i, k ) )
        i += k
    ENDDO

RETURN a

/* screen cell (0-based) where character nCol of cLine starts */
FUNCTION ce_ColToCell( cLine, nCol, nTab )

    LOCAL aCh, nCell := 0, i, ch

    IF hb_BLen( cLine ) == Len( cLine ) .AND. !( Chr( 9 ) $ cLine )
        RETURN nCol - 1
    ENDIF
    aCh := ce_Chars( cLine )
    FOR i := 1 TO Min( nCol - 1, Len( aCh ) )
        ch := aCh[ i ]
        nCell += IIF( ch == Chr( 9 ), nTab - Int( nCell % nTab ), IIF( hb_BLen( ch ) == 1, 1, ht_strWidth( ch ) ) )
    NEXT
    IF nCol - 1 > Len( aCh )
        nCell += nCol - 1 - Len( aCh )
    ENDIF

RETURN nCell

/* character of cLine at screen cell nCell (Len() + 1 after the end) */
FUNCTION ce_CellToCol( cLine, nCell, nTab )

    LOCAL aCh, nAt := 0, i, ch, nW

    IF hb_BLen( cLine ) == Len( cLine ) .AND. !( Chr( 9 ) $ cLine )
        RETURN Min( nCell + 1, Len( cLine ) + 1 )
    ENDIF
    aCh := ce_Chars( cLine )
    FOR i := 1 TO Len( aCh )
        ch := aCh[ i ]
        nW := IIF( ch == Chr( 9 ), nTab - Int( nAt % nTab ), IIF( hb_BLen( ch ) == 1, 1, ht_strWidth( ch ) ) )
        IF nCell < nAt + nW
            RETURN i
        ENDIF
        nAt += nW
    NEXT

RETURN Len( aCh ) + 1

FUNCTION ce_IsWordChar( c )
RETURN c == "_" .OR. IsAlpha( c ) .OR. IsDigit( c ) .OR. hb_BLen( c ) > 1

/* start of the previous word */
FUNCTION ce_WordLeft( cLine, nCol )
    DO WHILE nCol > 1 .AND. ! ce_IsWordChar( SubStr( cLine, nCol - 1, 1 ) )
        nCol--
    ENDDO
    DO WHILE nCol > 1 .AND. ce_IsWordChar( SubStr( cLine, nCol - 1, 1 ) )
        nCol--
    ENDDO
RETURN nCol

/* start of the next word */
FUNCTION ce_WordRight( cLine, nCol )
    DO WHILE nCol <= Len( cLine ) .AND. ce_IsWordChar( SubStr( cLine, nCol, 1 ) )
        nCol++
    ENDDO
    DO WHILE nCol <= Len( cLine ) .AND. ! ce_IsWordChar( SubStr( cLine, nCol, 1 ) )
        nCol++
    ENDDO
RETURN nCol

/* a key held down (Windows / X11); .F. where the state cannot be read */
STATIC FUNCTION ce_KeyDown( nVK )
RETURN ht_keyStateAvailable() .AND. ht_keyDown( nVK )
