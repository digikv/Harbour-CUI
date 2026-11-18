/** @class HTBrowse
 * TBrowse wrapper widget. Embeds Harbour's TBrowse inside a CT viewport
 * so all TBrowse rendering is automatically mapped to the widget's area.
 * @extends HTWidget
 *
 * @property onActivated Callback {|nRow, nCol| ...} fired on Enter or double-click
 *
 * @example
 *   oBrw := HTBrowse():new( oParent )
 *   oBrw:setGoTopBlock( {|| dbGoTop() } )
 *   oBrw:setGoBottomBlock( {|| dbGoBottom() } )
 *   oBrw:setSkipBlock( {|n| dbSkipper(n) } )
 *   oBrw:addColumn( "Name", {|| FIELD->NAME }, 20 )
 *   oBrw:addColumn( "City", {|| FIELD->CITY }, 15 )
 *   oBrw:onActivated := {|r,c| EditRecord() }
 *
 * @see HTScrollBar, HTTheme
 */

#include "hbtui.ch"
#include "inkey.ch"


CLASS HTBrowse FROM HTWidget

PROTECTED:

    DATA FoBrowse                           /* TBrowse instance */
    DATA FlConfigured    INIT .F.
    DATA FoEditGet       INIT NIL           /* HTGetBackend editing the current cell */
    DATA FxEditVar       INIT NIL           /* value being edited (bound to FoEditGet) */
    DATA FxOldValue      INIT NIL           /* original value before edit */
    DATA FlEditing       INIT .F.           /* currently editing a cell */
    DATA FnEditRow       INIT 0             /* edited cell, browse-local row/col */
    DATA FnEditCol       INIT 0
    DATA FnEditOffset    INIT 1             /* first buffer char shown in the cell */
    DATA FcColorSpec     INIT NIL           /* setColorSpec(): own colors instead of the theme */

    METHOD configureBrowse()
    METHOD editKey( nKey )
    METHOD repaintSelf()
    METHOD clearRight( cColor )

PUBLIC:

    CONSTRUCTOR new( ... )

    METHOD paintEvent( paintEvent )
    METHOD keyEvent( keyEvent )
    METHOD mouseEvent( eventMouse )
    METHOD focusOutEvent( eventFocus )
    METHOD cursorPos()

    /* data source blocks */
    METHOD setSkipBlock( b )
    METHOD setGoTopBlock( b )
    METHOD setGoBottomBlock( b )

    /* column management */
    METHOD addColumn( ... )
    METHOD columnCount() INLINE IIF( ::FoBrowse != NIL, ::FoBrowse:colCount, 0 )

    /* configuration */
    METHOD setHeadSep( c )    INLINE ::FoBrowse:headSep := c
    METHOD setColSep( c )     INLINE ::FoBrowse:colSep := c
    METHOD setFootSep( c )    INLINE ::FoBrowse:footSep := c
    METHOD setColorSpec( c )  INLINE ::FcColorSpec := c   /* NIL = theme colors */
    METHOD setFrozen( n )     INLINE ::FoBrowse:freeze := n

    /* state */
    METHOD rowPos()    INLINE ::FoBrowse:rowPos
    METHOD colPos()    INLINE ::FoBrowse:colPos
    METHOD isStable()  INLINE ::FoBrowse:stable
    METHOD hitTop()    INLINE ::FoBrowse:hitTop
    METHOD hitBottom() INLINE ::FoBrowse:hitBottom

    /* refresh */
    METHOD refreshAll()
    METHOD refreshCurrent()

    /* inline editing: F2 / typing / double-click start, Enter or Up/Down
       save, Esc cancels. Columns need a read/write block {|x| ... } */
    METHOD beginEdit( nKey )
    METHOD endEdit( lAccept )
    METHOD isEditing() INLINE ::FlEditing

    /* direct access to TBrowse object */
    METHOD browse() INLINE ::FoBrowse
    /* show an existing TBrowse / TBrowseDB (its columns, blocks, colors) */
    METHOD setBrowse( oBrowse )
    /* bring the TBrowse up to date at its screen position; the cursor is
       left on the current cell (for a GET over the cell) */
    METHOD stabilize()

    PROPERTY onActivated READWRITE               /* {|nRow, nCol| ... } Enter/dblclick */
    PROPERTY editable READWRITE INIT .F.           /* .T. to enable inline cell editing */
    PROPERTY alignLeft READWRITE INIT .F.          /* .T.: columns start at the left edge (TBrowse centers them) */
    METHOD fitWidth( nWidth )                     /* width TBrowse gets with alignLeft */
    PROPERTY extraColors READWRITE INIT ""        /* color pairs after the theme's six (pair 7, ...): colorBlock */
    PROPERTY onBeginEdit READWRITE               /* {|nRow, nCol, xValue| lAllow} */
    PROPERTY onEndEdit READWRITE                 /* {|nRow, nCol, xOldValue, xNewValue| lAccept} */

ENDCLASS

/** Creates a new browse widget with an internal TBrowse instance. */
METHOD new( ... ) CLASS HTBrowse

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

    ::FoBrowse := TBrowseNew()

    ::setFocusPolicy( HT_FOCUS_STRONG )
    ::FisVisible := .T.
    ::Fheight := 10
    ::Fwidth := 40

RETURN self

/** Configures TBrowse to absolute screen coords for the widget's rectangle.
 * TBrowse paints via Harbour core (hb_gtWriteAt), bypassing the viewport
 * DispOutAt remap — coords must be screen-absolute, never viewport-relative.
 * Prefer window geometry over ht_ViewTop/Left so a deactivated viewport
 * cannot leave a ghost cell at (0,0) on the desktop.
 */
METHOD PROCEDURE configureBrowse() CLASS HTBrowse

    LOCAL oWin, oAnc
    LOCAL nTop, nLeft, nBottom, nRight
    LOCAL nRelRow, nRelCol

    /* find top-level CT window ancestor */
    oWin := NIL
    oAnc := Self
    DO WHILE oAnc != NIL
        /* public accessor: FwindowId is PROTECTED and a parent window class
           (e.g. a user subclass of HTWidget) is unrelated to HTBrowse */
        IF oAnc:isDerivedFrom( "HTWidget" ) .AND. oAnc:hasOwnWindow()
            oWin := oAnc
            EXIT
        ENDIF
        oAnc := oAnc:parent()
    ENDDO

    IF oWin != NIL
        wSelect( oWin:windowId, .F. )
        /* accumulate child offsets up to the window; +1 row for CUI title */
        nRelRow := 1
        nRelCol := 0
        oAnc := Self
        DO WHILE oAnc != NIL .AND. ! ( oAnc == oWin )
            nRelRow += oAnc:y
            nRelCol += oAnc:x
            oAnc := oAnc:parent()
        ENDDO
        nTop    := wRow() + nRelRow
        nLeft   := wCol() + nRelCol
        nBottom := nTop + Max( ::Fheight - 1, 0 )
        nRight  := nLeft + Max( ::Fwidth - 1, 0 )
    ELSE
        /* fallback: active viewport (paintChild path) */
        nTop    := ht_ViewTop()
        nLeft   := ht_ViewLeft()
        nBottom := nTop + ht_MaxRow()
        nRight  := nLeft + ht_MaxCol()
    ENDIF

    /* alignLeft: when all columns fit, TBrowse ends with the last one - it
       would center them, putting half of the free width on the left */
    IF ::FalignLeft
        nRight := nLeft + ::fitWidth( nRight - nLeft + 1 ) - 1
    ENDIF

    /* each assignment makes TBrowse re-read every visible row: only on a
       change (moved / resized window) */
    IF ::FoBrowse:nTop != nTop .OR. ::FoBrowse:nLeft != nLeft .OR. ;
       ::FoBrowse:nBottom != nBottom .OR. ::FoBrowse:nRight != nRight .OR. ! ::FlConfigured
        ::FoBrowse:nTop    := nTop
        ::FoBrowse:nLeft   := nLeft
        ::FoBrowse:nBottom := nBottom
        ::FoBrowse:nRight  := nRight
        ::FlConfigured := .T.
    ENDIF

RETURN

/** Configures and stabilizes the TBrowse, rendering all visible rows.
 * @param paintEvent HTPaintEvent (unused)
 */
METHOD PROCEDURE paintEvent( paintEvent ) CLASS HTBrowse

    LOCAL oCol, cBuffer, nWidth, nPos, cColor

    HB_SYMBOL_UNUSED( paintEvent )

    IF ::FoBrowse:colCount = 0
        RETURN
    ENDIF

    /* colorSpec: setColorSpec()'s own colors, else the theme's - pair 1 =
       normal cells, pair 2 = selected cell. Assigned only on a change:
       TBrowse re-reads its rows after each assignment. */
    IF ::FcColorSpec != NIL
        cColor := ::FcColorSpec
    ELSEIF ::hasFocus()
        cColor := HTTheme():getColor( HT_CLR_BROWSE_FOCUSED ) + "," + ;
            HTTheme():getColor( HT_CLR_BROWSE_SELECTED )
    ELSE
        cColor := HTTheme():getColor( HT_CLR_BROWSE_NORMAL ) + "," + ;
            HTTheme():getColor( HT_CLR_BROWSE_NORMAL )
    ENDIF
    IF ::FcColorSpec == NIL .AND. ! Empty( ::FextraColors )
        cColor += "," + ::FextraColors
    ENDIF
    IF !( ::FoBrowse:colorSpec == cColor )
        ::FoBrowse:colorSpec := cColor
    ENDIF

    /* TBrowse uses core DispOutAt (absolute coords): its position follows
       the window */
    ::configureBrowse()

    /* the window under the browse was cleared: redraw every row, from the
       rows TBrowse already read (invalidate). Data changed elsewhere is
       shown after refreshAll() / refreshCurrent(), as with a TBrowse. */
    ::FoBrowse:invalidate()
    ::FoBrowse:forceStable()

    /* alignLeft: blank the cells right of the columns (a cut column left them) */
    IF ::FalignLeft .AND. ::FoBrowse:nRight - ::FoBrowse:nLeft + 1 < ::Fwidth
        ::clearRight( hb_tokenGet( cColor, 1, "," ) )
    ENDIF

    /* TBrowse writes one character per cell: give CJK characters their
       second cell (the whole browse was just redrawn - see ht_widenRegion) */
    ht_widenRegion( ::FoBrowse:nTop, ::FoBrowse:nLeft, ::FoBrowse:nBottom, ::FoBrowse:nRight )

    /* inline editor over the current cell. TBrowse (core RTL, absolute
       coordinates) leaves the cursor on the current cell after
       forceStable(); DispOutAt here is relative to this browse's viewport,
       whose origin is the TBrowse's nTop/nLeft. */
    IF ::FlEditing .AND. ::FoEditGet != NIL
        oCol := ::FoBrowse:getColumn( ::FoBrowse:colPos )
        cBuffer := ::FoEditGet:buffer
        nWidth := IIF( oCol != NIL .AND. HB_ISNUMERIC( oCol:width ), oCol:width, Len( cBuffer ) )
        nWidth := Max( 1, nWidth )

        ::FnEditRow := Row() - ::FoBrowse:nTop
        ::FnEditCol := Col() - ::FoBrowse:nLeft

        /* scroll the buffer so the edit cursor stays inside the cell */
        nPos := ::FoEditGet:pos
        ::FnEditOffset := ht_scrollToCursor( cBuffer, ::FnEditOffset, nPos, nWidth )

        DispOutAt( ::FnEditRow, ::FnEditCol, ;
            ht_cellPadR( SubStr( cBuffer, ::FnEditOffset ), nWidth ), ;
            HTTheme():getColor( HT_CLR_GET_FOCUSED ) )
    ENDIF

RETURN

/** Dispatches navigation keys to the underlying TBrowse.
 * @param keyEvent HTKeyEvent
 */
METHOD PROCEDURE keyEvent( keyEvent ) CLASS HTBrowse

    LOCAL parent

    IF ::FoBrowse:colCount = 0
        RETURN
    ENDIF

    /* inline editing owns the keys; Up/Down save and then move */
    IF ::FlEditing .AND. ::FoEditGet != NIL
        SWITCH keyEvent:key
        CASE K_ENTER
            ::endEdit( .T. )
            keyEvent:accept()
            RETURN
        CASE K_ESC
            ::endEdit( .F. )
            keyEvent:accept()
            RETURN
        CASE K_UP
        CASE K_DOWN
            ::endEdit( .T. )
            EXIT   /* fall through to normal navigation below */
        OTHERWISE
            ::editKey( keyEvent:key )
            keyEvent:accept()
            ::repaintSelf()
            RETURN
        ENDSWITCH
    ENDIF

    SWITCH keyEvent:key
    CASE K_UP
        ::configureBrowse()
        ::FoBrowse:up()
        EXIT
    CASE K_DOWN
        ::configureBrowse()
        ::FoBrowse:down()
        EXIT
    CASE K_PGUP
        ::configureBrowse()
        ::FoBrowse:pageUp()
        EXIT
    CASE K_PGDN
        ::configureBrowse()
        ::FoBrowse:pageDown()
        EXIT
    CASE K_HOME
        ::configureBrowse()
        ::FoBrowse:home()
        EXIT
    CASE K_END
        ::configureBrowse()
        ::FoBrowse:end()
        EXIT
    CASE K_LEFT
        ::configureBrowse()
        ::FoBrowse:left()
        EXIT
    CASE K_RIGHT
        ::configureBrowse()
        ::FoBrowse:right()
        EXIT
    CASE K_ENTER
        IF ::FonActivated != NIL
            Eval( ::FonActivated, ::FoBrowse:rowPos, ::FoBrowse:colPos )
        ENDIF
        keyEvent:accept()
        RETURN
    CASE K_CTRL_HOME
        ::configureBrowse()
        ::FoBrowse:goTop()
        EXIT
    CASE K_CTRL_END
        ::configureBrowse()
        ::FoBrowse:goBottom()
        EXIT
    CASE K_CTRL_PGUP
        ::configureBrowse()
        ::FoBrowse:panHome()
        EXIT
    CASE K_CTRL_PGDN
        ::configureBrowse()
        ::FoBrowse:panEnd()
        EXIT
    CASE K_CTRL_LEFT
        ::configureBrowse()
        ::FoBrowse:panLeft()
        EXIT
    CASE K_CTRL_RIGHT
        ::configureBrowse()
        ::FoBrowse:panRight()
        EXIT
    CASE K_F2
        /* F2 starts inline editing */
        IF ::Feditable
            ::beginEdit( 0 )
        ENDIF
        keyEvent:accept()
        RETURN
    OTHERWISE
        /* start editing on printable character */
        IF ::Feditable .AND. ! ht_keyText( keyEvent:key ) == ""
            ::beginEdit( keyEvent:key )
            keyEvent:accept()
            RETURN
        ENDIF
        RETURN
    ENDSWITCH

    keyEvent:accept()

    /* repaint via parent to re-establish the viewport */
    parent := ::parent()
    IF parent != NIL .AND. parent:isDerivedFrom( "HTWidget" )
        parent:repaintChild( self )
    ENDIF

RETURN

/** Handles mouse click to navigate to a data row and mouse wheel scrolling.
 * @param eventMouse HTMouseEvent
 */
METHOD PROCEDURE mouseEvent( eventMouse ) CLASS HTBrowse

    LOCAL nMouseRow
    LOCAL nClickRow, nSkip, nColSkip
    LOCAL parent
    LOCAL nHeadHeight
    LOCAL nTargetCol

    IF ::FoBrowse:colCount = 0
        RETURN
    ENDIF

    SWITCH eventMouse:nKey
    CASE K_LDBLCLK
        /* double-click: edit the cell (the first click already selected it) */
        IF ::Feditable
            ::beginEdit( 0 )
        ELSEIF ::FonActivated != NIL
            Eval( ::FonActivated, ::FoBrowse:rowPos, ::FoBrowse:colPos )
        ENDIF
        EXIT

    CASE K_LBUTTONDOWN

        /* a click elsewhere saves the cell being edited */
        IF ::FlEditing
            ::endEdit( .T. )
        ENDIF

        /*
         * Mouse coords are relative to the CT window (margins=0).
         * The browse viewport starts at (1 + ::y, 1 + ::x) in window coords.
         * Convert to browse-local coordinates.
         */
        /* mouseRow is child-relative (translated by parent's dispatch); the
           column comes from TBrowse:mColPos below */
        nMouseRow := eventMouse:mouseRow

        IF nMouseRow < 0
            RETURN
        ENDIF

        /* header height: 1 for heading text + 1 for separator if present */
        nHeadHeight := 1
        IF ::FoBrowse:headSep != NIL .AND. ! Empty( ::FoBrowse:headSep )
            nHeadHeight := 2
        ENDIF

        /* data row index (1-based) */
        nClickRow := nMouseRow - nHeadHeight + 1

        IF nClickRow < 1
            RETURN
        ENDIF

        /* clicked column: TBrowse:mColPos maps the mouse position (MRow/MCol,
           absolute) through the column screen positions of the last stable
           paint. Read it BEFORE moving rows — configureBrowse() calls
           configure(), which drops those positions until the next
           stabilize, so mColPos would come back 0 for a click on another row */
        nTargetCol := ::FoBrowse:mColPos
        IF nTargetCol < 1
            nTargetCol := ::FoBrowse:colPos
        ENDIF

        /* skip from current rowPos to clicked row */
        nSkip := nClickRow - ::FoBrowse:rowPos

        IF nSkip > 0
            ::configureBrowse()
            DO WHILE nSkip > 0
                ::FoBrowse:down()
                nSkip--
            ENDDO
        ELSEIF nSkip < 0
            ::configureBrowse()
            DO WHILE nSkip < 0
                ::FoBrowse:up()
                nSkip++
            ENDDO
        ENDIF

        /* navigate to the target column */
        nColSkip := nTargetCol - ::FoBrowse:colPos
        IF nColSkip > 0
            DO WHILE nColSkip > 0
                ::FoBrowse:right()
                nColSkip--
            ENDDO
        ELSEIF nColSkip < 0
            DO WHILE nColSkip < 0
                ::FoBrowse:left()
                nColSkip++
            ENDDO
        ENDIF

        parent := ::parent()
        IF parent != NIL .AND. parent:isDerivedFrom( "HTWidget" )
            parent:repaintChild( self )
        ENDIF

        EXIT

    CASE K_MWFORWARD
        ::configureBrowse()
        ::FoBrowse:up()
        parent := ::parent()
        IF parent != NIL .AND. parent:isDerivedFrom( "HTWidget" )
            parent:repaintChild( self )
        ENDIF
        EXIT

    CASE K_MWBACKWARD
        ::configureBrowse()
        ::FoBrowse:down()
        parent := ::parent()
        IF parent != NIL .AND. parent:isDerivedFrom( "HTWidget" )
            parent:repaintChild( self )
        ENDIF
        EXIT

    ENDSWITCH

RETURN

/** Sets the TBrowse skip block for record navigation.
 * @param b Code block {|n| skipRows(n)}
 */
METHOD PROCEDURE setSkipBlock( b ) CLASS HTBrowse
    ::FoBrowse:skipBlock := b
RETURN

/** Sets the TBrowse go-top block.
 * @param b Code block {|| goTop()}
 */
METHOD PROCEDURE setGoTopBlock( b ) CLASS HTBrowse
    ::FoBrowse:goTopBlock := b
RETURN

/** Sets the TBrowse go-bottom block.
 * @param b Code block {|| goBottom()}
 */
METHOD PROCEDURE setGoBottomBlock( b ) CLASS HTBrowse
    ::FoBrowse:goBottomBlock := b
RETURN

/** Adds a column. Accepts a TBColumn object or (cHeading, bBlock [, nWidth [, cPicture]]).
 * @param ... TBColumn object, or heading + block + optional width + picture
 */
METHOD PROCEDURE addColumn( ... ) CLASS HTBrowse

    LOCAL oCol
    LOCAL cHeading, bBlock, nWidth

    IF pCount() = 1 .AND. hb_isObject( hb_pValue( 1 ) )
        /* TBColumn object */
        ::FoBrowse:addColumn( hb_pValue( 1 ) )
    ELSEIF pCount() >= 2
        /* convenience: heading + block [+ width [+ picture]] */
        cHeading := hb_pValue( 1 )
        bBlock   := hb_pValue( 2 )
        nWidth   := hb_pValue( 3 )

        oCol := TBColumnNew( cHeading, bBlock )

        IF nWidth != NIL
            oCol:width := nWidth
        ENDIF

        IF pCount() >= 4
            oCol:picture := hb_pValue( 4 )
        ENDIF

        ::FoBrowse:addColumn( oCol )
    ENDIF

    ::FlConfigured := .F.

RETURN

/** Shows an existing TBrowse / TBrowseDB: its columns, blocks and
 * separators stay as they are; its position follows this widget.
 * @param oBrowse TBrowse object
 */
METHOD PROCEDURE setBrowse( oBrowse ) CLASS HTBrowse
    ::FoBrowse := oBrowse
    ::FlConfigured := .F.
RETURN

/** Brings the TBrowse up to date at this widget's screen position. The
 * screen cursor is left on the current cell (TBrowse:stabilize()), so a
 * GET at Row(), Col() edits the cell in place.
 */
METHOD PROCEDURE stabilize() CLASS HTBrowse
    IF ::FoBrowse:colCount > 0
        ::configureBrowse()
        ::FoBrowse:forceStable()
    ENDIF
RETURN

/** Marks all rows for refresh and triggers a repaint via parent. */
METHOD PROCEDURE refreshAll() CLASS HTBrowse

    LOCAL parent

    ::FoBrowse:refreshAll()

    parent := ::parent()
    IF parent != NIL .AND. parent:isDerivedFrom( "HTWidget" )
        parent:repaintChild( self )
    ENDIF

RETURN

/** Starts inline editing of the current cell with a TGet (HTGetBackend), so
 * the column PICTURE applies and the value keeps its type.
 * @param nKey 0 = edit the current value (F2 / double-click / API);
 *             a printable key = replace the value, starting with that key
 */
METHOD PROCEDURE beginEdit( nKey ) CLASS HTBrowse

    LOCAL oCol, xValue
    LOCAL nColPos, nWidth
    LOCAL cPicture

    IF ::FlEditing .OR. ::FoBrowse:colCount = 0
        RETURN
    ENDIF

    nColPos := ::FoBrowse:colPos
    oCol := ::FoBrowse:getColumn( nColPos )
    IF oCol == NIL
        RETURN
    ENDIF

    xValue := Eval( oCol:block )
    /* TGet edits characters, numbers, dates, timestamps and logicals */
    IF ! ValType( xValue ) $ "CNDTL"
        RETURN
    ENDIF

    IF ::FonBeginEdit != NIL
        IF ! Eval( ::FonBeginEdit, ::FoBrowse:rowPos, nColPos, xValue )
            RETURN
        ENDIF
    ENDIF

    ::FxOldValue := xValue
    cPicture := oCol:picture

    /* strings are edited padded to the column width, so the user can type
       up to it (the value is RTrim()-ed back on save) */
    IF HB_ISSTRING( xValue )
        nWidth := IIF( HB_ISNUMERIC( oCol:width ), oCol:width, Len( xValue ) )
        ::FxEditVar := PadR( xValue, Max( Len( xValue ), nWidth ) )
    ELSE
        ::FxEditVar := xValue
    ENDIF

    ::FoEditGet := HTGetBackend():new( 0, 0, ;
        {| x | IIF( x == NIL, ::FxEditVar, ::FxEditVar := x ) }, "", cPicture )
    ::FoEditGet:setFocus()
    ::FnEditOffset := 1
    ::FlEditing := .T.

    IF nKey > 0
        /* typing starts a new value: the first key clears the cell */
        ::FoEditGet:clear := .T.
        ::editKey( nKey )
    ELSEIF HB_ISSTRING( xValue )
        /* F2 on text: cursor after the text, like a spreadsheet */
        ::FoEditGet:pos := Min( Len( RTrim( ::FoEditGet:buffer ) ) + 1, Len( ::FoEditGet:buffer ) )
    ENDIF

    ::repaintSelf()

RETURN

/** Ends inline editing.
 * @param lAccept .T. writes the typed value back through the column block
 *                (after onEndEdit agrees), .F. discards the edit
 */
METHOD PROCEDURE endEdit( lAccept ) CLASS HTBrowse

    LOCAL xNewValue, oCol

    DEFAULT lAccept := .T.

    IF ! ::FlEditing .OR. ::FoEditGet == NIL
        RETURN
    ENDIF

    IF lAccept
        /* buffer -> typed value (unTransform via the PICTURE) */
        ::FoEditGet:assign()
        xNewValue := ::FxEditVar
        IF HB_ISSTRING( xNewValue )
            xNewValue := RTrim( xNewValue )
        ENDIF

        IF ::FonEndEdit != NIL
            IF ! Eval( ::FonEndEdit, ::FoBrowse:rowPos, ::FoBrowse:colPos, ::FxOldValue, xNewValue )
                lAccept := .F.
            ENDIF
        ENDIF

        IF lAccept
            /* write back through the column block (read-only blocks ignore it) */
            oCol := ::FoBrowse:getColumn( ::FoBrowse:colPos )
            IF oCol != NIL
                Eval( oCol:block, xNewValue )
            ENDIF
        ENDIF
    ENDIF

    ::FoEditGet:killFocus()
    ::FoEditGet  := NIL
    ::FxEditVar  := NIL
    ::FxOldValue := NIL
    ::FlEditing  := .F.

    ::FoBrowse:refreshCurrent()
    ::repaintSelf()

RETURN

/** Applies one editing key to the cell editor (TGet semantics, as HTGet).
 * @return .T. if the key was used
 */
METHOD FUNCTION editKey( nKey ) CLASS HTBrowse

    LOCAL cChar

    SWITCH nKey
    CASE K_LEFT
        ::FoEditGet:left()
        EXIT
    CASE K_RIGHT
        ::FoEditGet:right()
        EXIT
    CASE K_HOME
        ::FoEditGet:home()
        EXIT
    CASE K_END
        ::FoEditGet:end()
        EXIT
    CASE K_BS
        ::FoEditGet:backSpace()
        EXIT
    CASE K_DEL
        ::FoEditGet:delete()
        EXIT
    CASE K_CTRL_Y
        ::FoEditGet:delEnd()
        EXIT
    CASE K_CTRL_U
        ::FoEditGet:undo()
        EXIT
    CASE K_INS
        Set( _SET_INSERT, ! Set( _SET_INSERT ) )
        EXIT
    OTHERWISE
        cChar := ht_keyText( nKey )
        IF ! cChar == ""
            IF Set( _SET_INSERT )
                ::FoEditGet:insert( cChar )
            ELSE
                ::FoEditGet:overStrike( cChar )
            ENDIF
        ELSE
            RETURN .F.
        ENDIF
    ENDSWITCH

RETURN .T.

/** Text cursor inside the edited cell (browse-local), for
 * HTApplication:syncCursor(); NIL when not editing. */
METHOD FUNCTION cursorPos() CLASS HTBrowse

    IF ::FlEditing .AND. ::FoEditGet != NIL .AND. ::hasFocus() .AND. ;
       HB_ISNUMERIC( ::FoEditGet:pos ) .AND. ::FoEditGet:pos > 0
        RETURN { ::FnEditRow, ::FnEditCol + ht_cellCol( ::FoEditGet:buffer, ::FnEditOffset, ::FoEditGet:pos ) }
    ENDIF

RETURN NIL

/** Leaving the browse (Tab, click on another control) saves the edit. */
METHOD PROCEDURE focusOutEvent( eventFocus ) CLASS HTBrowse

    IF ::FlEditing
        ::endEdit( .T. )
    ENDIF

    ::super:focusOutEvent( eventFocus )

RETURN

/** Repaints this browse through its parent (re-establishes the viewport). */
METHOD PROCEDURE repaintSelf() CLASS HTBrowse

    LOCAL parent := ::parent()

    IF parent != NIL .AND. parent:isDerivedFrom( "HTWidget" )
        parent:repaintChild( self )
    ENDIF

RETURN

/** Marks the current row for refresh and triggers a repaint via parent. */
METHOD PROCEDURE refreshCurrent() CLASS HTBrowse

    LOCAL parent

    ::FoBrowse:refreshCurrent()

    parent := ::parent()
    IF parent != NIL .AND. parent:isDerivedFrom( "HTWidget" )
        parent:repaintChild( self )
    ENDIF

RETURN

/** alignLeft: the width the columns take when all fit in nWidth cells
 * (column widths + separators), else nWidth.
 * @param nWidth Width of the widget
 * @return Width TBrowse should get
 */
METHOD FUNCTION fitWidth( nWidth ) CLASS HTBrowse

    LOCAL nTotal := 0, n, oCol, cSep

    FOR n := 1 TO ::FoBrowse:colCount
        oCol := ::FoBrowse:getColumn( n )
        IF n > 1
            cSep := IIF( oCol:colSep == NIL, ::FoBrowse:colSep, oCol:colSep )
            nTotal += IIF( HB_ISSTRING( cSep ), Len( cSep ), 1 )
        ENDIF
        nTotal += ::FoBrowse:colWidth( n )
        IF nTotal >= nWidth
            RETURN nWidth
        ENDIF
    NEXT

RETURN Max( 1, nTotal )

/** alignLeft: blanks the cells right of the TBrowse (viewport coordinates).
 * @param cColor Color of the blank cells
 */
METHOD PROCEDURE clearRight( cColor ) CLASS HTBrowse

    LOCAL nCol := ::FoBrowse:nRight - ::FoBrowse:nLeft + 1, nRow
    LOCAL cBlank := Space( ::Fwidth - nCol )

    FOR nRow := 0 TO ::Fheight - 1
        DispOutAt( nRow, nCol, cBlank, cColor )
    NEXT

RETURN
