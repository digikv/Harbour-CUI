/** @class HTGet
 * Combined label + input widget backed by Harbour's TGet class.
 * Provides full Clipper-compatible PICTURE editing, type-aware input handling,
 * validation with focus rejection, and undo support.
 * @extends HTWidget
 *
 * ## TGet Integration
 *
 * HTGet wraps Harbour's core Get class internally. All editing is delegated
 * to TGet methods (insert, overStrike, delete, left, right, home, end,
 * backSpace, wordLeft, wordRight). The formatted edit buffer (oGet:buffer)
 * is read for display; oGet:assign() + oGet:killFocus() handle value
 * write-back and cleanup on focus loss.
 *
 * **Full PICTURE support (via TGet):**
 * - All template chars: 9, #, A, N, X, L, Y, !, $, *
 * - All @ functions: @!, @A, @B, @C, @D, @E, @K, @R, @S, @T, @X, @Z, @(, @)
 * - Type-aware editing: numeric decimal alignment, date CToD validation,
 *   logical T/F toggle, timestamp fields
 * - Insert/Overwrite toggle (K_INS)
 * - Undo to original value (K_CTRL_U)
 *
 * **Validation with focus rejection:**
 * - VALID block returning .F. keeps focus on the field (user cannot leave)
 * - WHEN block returning .F. prevents field activation
 *
 * **HTWidget additions (not in TGet):**
 * - Integrated label with auto-calculated widths
 * - Password mode (hide property — displays asterisks)
 * - Theme-aware colors via HTTheme()
 * - Viewport painting via wFormat() coordinate isolation
 * - onChange callback on each keystroke modification
 * - Help line text for status bar
 *
 * @property label     Label text displayed before the input area
 * @property picture   PICTURE format string (full Clipper/Harbour syntax)
 * @property xVar      Code block {|x| IIF(x==NIL, val, val:=x)} for read/write variable access
 * @property valid     Post-validation block {|xValue| lOk}; .F. rejects focus change
 * @property when      Pre-condition block {|| lEnabled}; .F. prevents field activation
 * @property helpLine  Context help text shown in status bar when focused
 * @property onChange   Callback {|xValue| ...} fired on each value change
 * @property readOnly  .T. disables editing
 * @property hide      .T. replaces display with asterisks (password mode)
 *
 * @example
 *   /* String GET with uppercase mask */
 *   LOCAL cName := Space(30)
 *   oGet := HTGet():new( oParent )
 *   oGet:setup( {|x| IIF(x==NIL, cName, cName:=x) }, "Name:", "@!", ;
 *               {|v| !Empty(v) }, NIL, "Enter name", NIL, .F., .F., 40 )
 *
 *   /* Numeric GET with decimal */
 *   LOCAL nPrice := 0
 *   oGet2 := HTGet():new( oParent )
 *   oGet2:setup( {|x| IIF(x==NIL, nPrice, nPrice:=x) }, "Price:", "999,999.99" )
 *
 *   /* Date GET */
 *   LOCAL dDate := Date()
 *   oGet3 := HTGet():new( oParent )
 *   oGet3:setup( {|x| IIF(x==NIL, dDate, dDate:=x) }, "Date:", "@D" )
 *
 * @see HTLineEdit, HTTheme, Get (Harbour core TGet class)
 */

#include "hbtui.ch"
#include "inkey.ch"
#include "setcurs.ch"
#include "hbgtinfo.ch"


CLASS HTGet FROM HTWidget

PROTECTED:

    DATA FoGet                                          /* Harbour TGet instance */
    DATA FlGetActive     INIT .F.                       /* whether TGet currently has focus */
    DATA FdispOffset     INIT 1
    DATA FreadOnly       INIT .F.                       /* explicit: avoids READONLY keyword clash */
    DATA FselAnchor      INIT 0                         /* Shift+arrow selection: buffer position it started at, 0 = none */
    DATA FlSelLast       INIT .F.                       /* selection also covers the character under the cursor (the last one: TGet's cursor cannot go past it) */

    METHOD selectionRange()                             /* { nFrom, nTo } buffer positions, nTo exclusive, or NIL */
    METHOD deleteSelection()

PUBLIC:

    CONSTRUCTOR new( ... )

    METHOD paintEvent( event )
    METHOD keyEvent( keyEvent )
    METHOD mouseEvent( eventMouse )
    METHOD focusInEvent( eventFocus )
    METHOD focusOutEvent( eventFocus )
    METHOD cursorPos()
    METHOD enterMovesFocus() INLINE .T.   /* Enter = next field, as in READ */

    METHOD setup( xVar, cLabel, cPicture, bValid, bWhen, cHelpLine, bOnChange, lReadOnly, lHide, nWidth )
    METHOD getValue()
    METHOD setValue( x )
    METHOD setLabel( cLabel )
    METHOD setPicture( cPicture )
    METHOD setXVar( xVar )
    METHOD setReadOnly( lReadOnly )
    METHOD setLabelWidth( nWidth )
    METHOD getDisplayValue()

    METHOD editBuffer() INLINE IIF( ::FlGetActive .AND. ::FoGet != NIL, ::FoGet:buffer, ::getDisplayValue() )
    METHOD hasSelection() INLINE ::selectionRange() != NIL
    METHOD selectedText()
    METHOD setSelection( nFrom, nTo )                   /* select buffer positions nFrom .. nTo - 1 (cursor to nTo) */
    METHOD clearSelection() INLINE ::FselAnchor := 0, ::FlSelLast := .F.

    METHOD readOnly()   INLINE ::FreadOnly
    METHOD _readOnly( b ) INLINE ::FreadOnly := b

    PROPERTY label WRITE setLabel INIT ""
    PROPERTY picture WRITE setPicture INIT ""
    PROPERTY xVar WRITE setXVar                     /* {|x| IIF( x == NIL, cValue, cValue := x ) } */
    PROPERTY valid READWRITE                         /* {|xValue| lOk } — .F. rejects focus loss */
    PROPERTY when READWRITE                          /* {|| lEnabled } — .F. prevents focus entry */
    PROPERTY helpLine READWRITE INIT ""
    PROPERTY onChange READWRITE                       /* {|xValue| ... } */
    PROPERTY hide READWRITE INIT .F.                 /* password mode */
    PROPERTY labelWidth WRITE setLabelWidth INIT 0
    PROPERTY inputWidth INIT 0

ENDCLASS

/** Creates a new GET widget with optional parent. */
METHOD new( ... ) CLASS HTGet

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
    ::Fheight := 1
    IF ::Fwidth <= 0
        ::Fwidth := 30
    ENDIF

RETURN self

/** Configures all GET properties and creates the internal TGet instance.
 * @param xVar Code block for read/write access to the variable
 * @param cLabel Label text displayed before the input
 * @param cPicture PICTURE format string (full Clipper/Harbour syntax)
 * @param bValid Post-validation code block; .F. return rejects focus change
 * @param bWhen Pre-condition code block; .F. return prevents field activation
 * @param cHelpLine Help text for status bar
 * @param bOnChange Callback fired on each value change
 * @param lReadOnly Read-only mode flag
 * @param lHide Password mode (shows asterisks)
 * @param nWidth Total widget width including label
 */
METHOD PROCEDURE setup( xVar, cLabel, cPicture, bValid, bWhen, cHelpLine, bOnChange, lReadOnly, lHide, nWidth ) CLASS HTGet

    DEFAULT cLabel    := ""
    DEFAULT cPicture  := ""
    DEFAULT cHelpLine := ""
    DEFAULT lReadOnly := .F.
    DEFAULT lHide     := .F.
    DEFAULT nWidth    := 30

    ::xVar      := xVar
    ::label     := cLabel
    ::picture   := cPicture
    ::valid     := bValid
    ::when      := bWhen
    ::helpLine  := cHelpLine
    ::onChange   := bOnChange
    ::readOnly  := lReadOnly
    ::hide      := lHide

    /* calculate label width: label text + 1 space separator */
    IF Len( cLabel ) > 0
        ::FlabelWidth := ht_strWidth( cLabel ) + 1
    ELSE
        ::FlabelWidth := 0
    ENDIF

    ::Fwidth := nWidth
    ::FinputWidth := ::Fwidth - ::FlabelWidth

    IF ::FinputWidth < 1
        ::FinputWidth := 1
        ::Fwidth := ::FlabelWidth + ::FinputWidth
    ENDIF

    /* create the Harbour TGet instance */
    IF xVar != NIL
        ::FoGet := HTGetBackend():new( 0, ::FlabelWidth, xVar, NIL, cPicture )
        IF bValid != NIL
            ::FoGet:postBlock := bValid
        ENDIF
        IF bWhen != NIL
            ::FoGet:preBlock := bWhen
        ENDIF
    ENDIF

RETURN

/** Renders the label and the TGet edit buffer (or Transform-formatted value when unfocused).
 * @param event HTPaintEvent (unused)
 */
METHOD PROCEDURE paintEvent( event ) CLASS HTGet

    LOCAL cDisplay, cColor, cTransformed
    LOCAL nVisibleWidth
    LOCAL nCursorCol, nCursorPos
    LOCAL aRange, nSelFrom, nSelCol
    LOCAL nMaxCol := MaxCol()

    HB_SYMBOL_UNUSED( event )

    /* draw label at (0, 0) */
    IF ::FlabelWidth > 0
        DispOutAt( 0, 0, ht_cellPadR( ::Flabel, ::FlabelWidth ), HTTheme():getColor( HT_CLR_GET_LABEL ) )
    ENDIF

    /* determine input color */
    IF ::FreadOnly
        cColor := HTTheme():getColor( HT_CLR_GET_READONLY )
    ELSEIF ::hasFocus()
        cColor := HTTheme():getColor( HT_CLR_GET_FOCUSED )
    ELSE
        cColor := HTTheme():getColor( HT_CLR_GET_NORMAL )
    ENDIF

    /* get display text from TGet buffer (focused) or Transform (unfocused) */
    IF ::FlGetActive .AND. ::FoGet != NIL .AND. ::FoGet:buffer != NIL
        cTransformed := ::FoGet:buffer
        nCursorPos := ::FoGet:pos
    ELSE
        cTransformed := ::getDisplayValue()
        nCursorPos := 0
    ENDIF

    /* password mode: replace with asterisks */
    IF ::Fhide
        cTransformed := Replicate( "*", Len( cTransformed ) )
    ENDIF

    /* calculate visible input width */
    nVisibleWidth := Min( ::FinputWidth, nMaxCol + 1 - ::FlabelWidth )
    IF nVisibleWidth < 1
        nVisibleWidth := 1
    ENDIF

    /* ensure cursor is visible within display window */
    IF nCursorPos > 0
        ::FdispOffset := ht_scrollToCursor( cTransformed, ::FdispOffset, nCursorPos, nVisibleWidth )
    ELSE
        ::FdispOffset := 1
    ENDIF

    /* extract visible portion of text */
    cDisplay := ht_cellPadR( SubStr( cTransformed, ::FdispOffset ), nVisibleWidth )

    DispOutAt( 0, ::FlabelWidth, cDisplay, cColor )

    /* Shift+arrow selection: repaint its visible part in the selection color */
    aRange := IIF( nCursorPos > 0, ::selectionRange(), NIL )
    IF aRange != NIL
        nSelFrom := Max( aRange[ 1 ], ::FdispOffset )
        IF aRange[ 2 ] > nSelFrom
            nSelCol := ht_cellCol( cTransformed, ::FdispOffset, nSelFrom )
            IF nSelCol < nVisibleWidth
                DispOutAt( 0, ::FlabelWidth + nSelCol, ;
                    ht_cellLeft( SubStr( cTransformed, nSelFrom, aRange[ 2 ] - nSelFrom ), nVisibleWidth - nSelCol ), ;
                    HTTheme():getColor( HT_CLR_LINEEDIT_SELECTED ) )
            ENDIF
        ENDIF
    ENDIF

    /* position cursor if focused */
    IF ::hasFocus() .AND. ! ::FreadOnly .AND. nCursorPos > 0
        nCursorCol := ::FlabelWidth + ht_cellCol( cTransformed, ::FdispOffset, nCursorPos )
        SetPos( 0, nCursorCol )
    ENDIF

RETURN

/** Delegates keyboard input to the internal TGet for full PICTURE-aware editing.
 * Supports insert/overwrite toggle (Ins), undo (Ctrl+U), word navigation (Ctrl+Left/Right).
 * @param keyEvent HTKeyEvent
 */
METHOD PROCEDURE keyEvent( keyEvent ) CLASS HTGet

    LOCAL parent
    LOCAL lOldChanged
    LOCAL cChar
    LOCAL nKey, nOldPos
    LOCAL lShift, lNav

    IF ::FreadOnly .OR. ::FoGet == NIL .OR. ! ::FlGetActive
        RETURN
    ENDIF

    lOldChanged := ::FoGet:changed
    nKey := keyEvent:key

    /* Shift + Left/Right/Home/End/Ctrl+Left/Right selects; the same keys
       without Shift drop the selection. Del, Backspace and typing act on it. */
    lNav := AScan( { K_LEFT, K_RIGHT, K_HOME, K_END, K_CTRL_LEFT, K_CTRL_RIGHT }, nKey ) > 0
    IF lNav
        lShift := hb_bitAnd( hb_gtInfo( HB_GTI_KBDSHIFTS ), HB_GTI_KBD_SHIFT ) != 0
        IF lShift
            IF ::FselAnchor = 0
                ::FselAnchor := ::FoGet:pos
            ENDIF
        ELSE
            ::clearSelection()
        ENDIF
        nOldPos := ::FoGet:pos
    ENDIF

    SWITCH nKey
    CASE K_LEFT
        ::FlSelLast := .F.
        ::FoGet:left()
        EXIT
    CASE K_RIGHT
        ::FoGet:right()
        /* the cursor stays on the last character: with Shift, select it too */
        ::FlSelLast := ::FselAnchor > 0 .AND. ::FoGet:pos = nOldPos
        EXIT
    CASE K_HOME
        ::FlSelLast := .F.
        ::FoGet:home()
        EXIT
    CASE K_END
        ::FoGet:end()
        ::FlSelLast := ::FselAnchor > 0 .AND. ::FoGet:pos = nOldPos .AND. ;
                       ! Empty( SubStr( ::FoGet:buffer, ::FoGet:pos, 1 ) )
        EXIT
    CASE K_CTRL_LEFT
        ::FlSelLast := .F.
        ::FoGet:wordLeft()
        EXIT
    CASE K_CTRL_RIGHT
        ::FlSelLast := .F.
        ::FoGet:wordRight()
        EXIT
    CASE K_BS
        IF ::hasSelection()
            ::deleteSelection()
        ELSE
            ::clearSelection()
            ::FoGet:backSpace()
        ENDIF
        EXIT
    CASE K_DEL
        IF ::hasSelection()
            ::deleteSelection()
        ELSE
            ::clearSelection()
            ::FoGet:delete()
        ENDIF
        EXIT
    CASE K_CTRL_T
        ::FoGet:delWordRight()
        EXIT
    CASE K_CTRL_Y
        ::FoGet:delEnd()
        EXIT
    CASE K_CTRL_U
        /* undo: restore original value */
        ::FoGet:undo()
        EXIT
    CASE K_INS
        Set( _SET_INSERT, ! Set( _SET_INSERT ) )
        EXIT
    OTHERWISE
        cChar := ht_keyText( keyEvent:key )
        IF ! cChar == ""
            IF ::hasSelection()
                /* typing replaces the selected text */
                ::deleteSelection()
                ::FoGet:insert( cChar )
            ELSEIF Set( _SET_INSERT )
                ::FoGet:insert( cChar )
            ELSE
                ::FoGet:overStrike( cChar )
            ENDIF
        ELSE
            RETURN
        ENDIF
    ENDSWITCH

    /* any other editing key (Ctrl+Y, Ctrl+T, Ctrl+U, ...) ends the selection */
    IF ! lNav .AND. ::FselAnchor > 0
        ::clearSelection()
    ENDIF

    keyEvent:accept()

    /* fire onChange if value was modified */
    IF ! lOldChanged .AND. ::FoGet:changed .AND. ::FonChange != NIL
        ::FoGet:assign()
        Eval( ::FonChange, ::getValue() )
    ELSEIF lOldChanged .AND. ::FonChange != NIL
        ::FoGet:assign()
        Eval( ::FonChange, ::getValue() )
    ENDIF

    parent := ::parent()
    IF parent != NIL .AND. parent:isDerivedFrom( "HTWidget" )
        parent:repaintChild( self )
    ENDIF

RETURN

/** Selected buffer positions while editing.
 * @return { nFrom, nTo } with nTo exclusive, or NIL when nothing is selected
 */
METHOD FUNCTION selectionRange() CLASS HTGet

    LOCAL nPos, nFrom, nTo

    IF ::FselAnchor <= 0 .OR. ::FoGet == NIL .OR. ! ::FlGetActive .OR. ! HB_ISNUMERIC( ::FoGet:pos )
        RETURN NIL
    ENDIF

    nPos  := ::FoGet:pos
    nFrom := Min( ::FselAnchor, nPos )
    nTo   := Max( ::FselAnchor, nPos )
    IF ::FlSelLast .AND. nPos >= ::FselAnchor
        nTo++
    ENDIF
    nTo := Min( nTo, Len( ::FoGet:buffer ) + 1 )

    IF nTo <= nFrom
        RETURN NIL
    ENDIF

RETURN { nFrom, nTo }

/** @return The selected part of the edit buffer, "" when nothing is selected */
METHOD FUNCTION selectedText() CLASS HTGet

    LOCAL aRange := ::selectionRange()

    IF aRange == NIL
        RETURN ""
    ENDIF

RETURN SubStr( ::FoGet:buffer, aRange[ 1 ], aRange[ 2 ] - aRange[ 1 ] )

/** Selects buffer positions nFrom .. nTo - 1 of the field being edited, as
 * Shift+arrows would; the cursor goes to nTo (to the last character when nTo
 * is past it). Ignored while the field is not being edited.
 */
METHOD PROCEDURE setSelection( nFrom, nTo ) CLASS HTGet

    LOCAL nLen

    IF ::FoGet == NIL .OR. ! ::FlGetActive
        RETURN
    ENDIF

    nLen  := Len( ::FoGet:buffer )
    nFrom := Max( 1, Min( nFrom, nLen ) )
    nTo   := Max( 1, Min( nTo, nLen + 1 ) )
    ::FselAnchor := nFrom
    ::FlSelLast  := nTo > nLen
    ::FoGet:pos  := Min( nTo, nLen )

RETURN

/** Deletes the selected characters; the cursor goes to the start of the
 * selection. Only editable positions are deleted, right to left: TGet's
 * delete() shifts the rest of that part of the field left, so picture
 * literals (the "-" of "@R 999-999") stay where they are.
 */
METHOD PROCEDURE deleteSelection() CLASS HTGet

    LOCAL aRange := ::selectionRange()
    LOCAL nPos

    IF aRange == NIL
        ::clearSelection()
        RETURN
    ENDIF

    FOR nPos := aRange[ 2 ] - 1 TO aRange[ 1 ] STEP -1
        IF ::FoGet:editable( nPos )
            ::FoGet:pos := nPos
            ::FoGet:delete()
        ENDIF
    NEXT
    ::FoGet:pos := aRange[ 1 ]
    ::clearSelection()

RETURN

/** Handles mouse click on the input area to reposition cursor.
 * @param eventMouse HTMouseEvent
 */
METHOD PROCEDURE mouseEvent( eventMouse ) CLASS HTGet

    LOCAL nClickCol, parent, nNewPos

    IF eventMouse:nKey = K_LBUTTONDOWN
        nClickCol := eventMouse:mouseCol

        /* only react to clicks on the input area */
        IF nClickCol >= ::FlabelWidth .AND. ::FoGet != NIL .AND. ::FlGetActive
            ::clearSelection()
            nNewPos := ::FdispOffset - 1 + ht_cellPos( SubStr( ::FoGet:buffer, ::FdispOffset ), nClickCol - ::FlabelWidth )
            /* a click past the typed text lands right after it, not in the blanks */
            nNewPos := Min( nNewPos, Len( RTrim( ::FoGet:buffer ) ) + 1 )
            IF nNewPos >= 1
                ::FoGet:pos := Min( nNewPos, Len( ::FoGet:buffer ) )
            ENDIF
            parent := ::parent()
            IF parent != NIL .AND. parent:isDerivedFrom( "HTWidget" )
                parent:repaintChild( self )
            ENDIF
        ENDIF
    ENDIF

RETURN

/** Activates the internal TGet (prepares edit buffer, stores original value for undo).
 * Checks WHEN pre-condition before activation.
 * @param eventFocus HTFocusEvent
 */
METHOD PROCEDURE focusInEvent( eventFocus ) CLASS HTGet

    /* check WHEN pre-condition */
    IF ::Fwhen != NIL
        IF ! Eval( ::Fwhen )
            /* WHEN returned .F. — reject focus */
            IF eventFocus != NIL
                eventFocus:ignore()
            ENDIF
            RETURN
        ENDIF
    ENDIF

    /* activate TGet */
    IF ::FoGet != NIL .AND. ! ::FlGetActive
        ::FoGet:setFocus()
        ::FlGetActive := .T.
        ::FdispOffset := 1
    ENDIF
    ::clearSelection()

    ::super:focusInEvent( eventFocus )

RETURN

/** Writes the edited value back to the variable via TGet:assign(), runs VALID
 * post-validation. If VALID returns .F., focus is kept on this field.
 * @param eventFocus HTFocusEvent
 */
METHOD PROCEDURE focusOutEvent( eventFocus ) CLASS HTGet

    LOCAL xValue
    LOCAL parent

    ::clearSelection()

    IF ::FlGetActive .AND. ::FoGet != NIL

        /* write an edited value back to the variable - only when it was
           edited, like ReadModal(): assigning an unchanged buffer re-reads
           it through the picture, which turns a date shown with a two-digit
           year into another century, or cuts decimals the picture hides */
        IF ::FoGet:changed
            ::FoGet:assign()
        ENDIF

        /* run post-validation */
        IF ::Fvalid != NIL
            xValue := ::getValue()
            IF ! Eval( ::Fvalid, xValue )
                /* validation failed — deactivate cleanly, reject the focus change */
                ::FoGet:killFocus()
                ::FlGetActive := .F.
                IF eventFocus != NIL
                    eventFocus:ignore()
                ENDIF
                /* repaint to show validation feedback (e.g. status bar message) */
                parent := ::parent()
                IF parent != NIL .AND. parent:isDerivedFrom( "HTWidget" )
                    parent:repaintChild( self )
                ENDIF
                RETURN
            ENDIF
        ENDIF

        ::FoGet:killFocus()
        ::FlGetActive := .F.

    ENDIF

    ::super:focusOutEvent( eventFocus )

RETURN

/** Widget-local text cursor position while the GET is being edited.
 * @return { nRow, nCol } or NIL
 */
METHOD FUNCTION cursorPos() CLASS HTGet

    IF ::FlGetActive .AND. ::FoGet != NIL .AND. ! ::FreadOnly .AND. ::hasFocus() .AND. ;
       HB_ISNUMERIC( ::FoGet:pos ) .AND. ::FoGet:pos > 0
        RETURN { 0, ::FlabelWidth + ht_cellCol( IIF( ::Fhide, Replicate( "*", Len( ::FoGet:buffer ) ), ::FoGet:buffer ), ;
                                                ::FdispOffset, ::FoGet:pos ) }
    ENDIF

RETURN NIL

/** Returns the current value by evaluating the xVar code block.
 * @return Current value, or empty string if xVar is NIL
 */
METHOD FUNCTION getValue() CLASS HTGet

    IF ::FxVar != NIL
        RETURN Eval( ::FxVar )
    ENDIF

RETURN ""

/** Sets the value by evaluating the xVar code block with the new value.
 * @param x New value to store
 */
METHOD PROCEDURE setValue( x ) CLASS HTGet

    IF ::FxVar != NIL
        Eval( ::FxVar, x )
    ENDIF

RETURN

/** Sets the label text and recalculates label/input widths.
 * @param cLabel New label text
 */
METHOD PROCEDURE setLabel( cLabel ) CLASS HTGet

    ::Flabel := cLabel
    IF Len( cLabel ) > 0
        ::FlabelWidth := ht_strWidth( cLabel ) + 1
    ELSE
        ::FlabelWidth := 0
    ENDIF
    ::FinputWidth := ::Fwidth - ::FlabelWidth
    IF ::FinputWidth < 1
        ::FinputWidth := 1
    ENDIF

RETURN

/** Sets the PICTURE format string and updates the TGet if it exists.
 * @param cPicture PICTURE clause (full Clipper/Harbour syntax)
 */
METHOD PROCEDURE setPicture( cPicture ) CLASS HTGet
    ::Fpicture := cPicture
    IF ::FoGet != NIL
        ::FoGet:picture := cPicture
    ENDIF
RETURN

/** Sets the variable access code block and recreates the TGet.
 * @param xVar Code block {|x| IIF(x==NIL, val, val:=x)}
 */
METHOD PROCEDURE setXVar( xVar ) CLASS HTGet
    ::FxVar := xVar
    IF xVar != NIL
        ::FoGet := Get():new( 0, ::FlabelWidth, xVar, NIL, ::Fpicture )
        IF ::Fvalid != NIL
            ::FoGet:postBlock := ::Fvalid
        ENDIF
        IF ::Fwhen != NIL
            ::FoGet:preBlock := ::Fwhen
        ENDIF
    ELSE
        ::FoGet := NIL
    ENDIF
RETURN

/** Sets the read-only state.
 * @param lReadOnly .T. to disable editing
 */
METHOD PROCEDURE setReadOnly( lReadOnly ) CLASS HTGet
    ::FreadOnly := lReadOnly
RETURN

/** Overrides the auto-calculated label width and recalculates input width.
 * @param nWidth Label width in columns
 */
METHOD PROCEDURE setLabelWidth( nWidth ) CLASS HTGet

    ::FlabelWidth := nWidth
    ::FinputWidth := ::Fwidth - ::FlabelWidth
    IF ::FinputWidth < 1
        ::FinputWidth := 1
    ENDIF

RETURN

/** Returns the formatted display value for unfocused state.
 * @return Transformed display string
 */
METHOD FUNCTION getDisplayValue() CLASS HTGet

    LOCAL cValue

    cValue := ::getValue()

    IF cValue == NIL
        cValue := ""
    ENDIF

    /* an empty picture formats like TGet does: a date by DToC() (not
       hb_CStr()'s "0d20261115"), a number by Str(), a logical as T/F */
RETURN Transform( cValue, ::Fpicture )
