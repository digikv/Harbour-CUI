/** @class HTSearchBox
 * A search box: type a part of a name (code, tax number ...), the matching items
 * open in a list under the field, the arrows move in it, Enter takes one.
 * After the searchbox of Digi knjigovodstvo (digi-angular):
 *
 *   - typing filters the items (in any part of the label, the details, the value);
 *     Serbian Latin also finds Cyrillic (nov -> нов, lj nj dž too);
 *   - Down / Up move the mark (a new search marks the first), Down opens the list
 *     again; PgUp / PgDn, Ctrl-Home / Ctrl-End;
 *   - Enter takes the marked item and moves on like Tab (advanceOnEnter); with
 *     an item taken or an empty field it only moves on; with text and no items
 *     it stays;
 *   - Esc closes only the list (the dialog stays);
 *   - typing over a taken item drops it (the value is NIL again); an empty
 *     field clears it;
 *   - leaving the field with text that matches one item only takes that item;
 *   - x on the right clears, + (when onAdd is set) adds a new item;
 *   - the mouse: a click takes an item, the mark follows the mouse.
 *
 * The items are any values; blocks say what they show:
 *   labelBlock   {|x| cLabel }            the name in the list (default: x[ 2 ] /
 *                                          x[ "naziv" ] / x[ "label" ] / x)
 *   valueBlock   {|x| xValue }            what getValue() returns (default: x[ 1 ] /
 *                                          x[ "sifra" ] / x[ "value" ] / x)
 *   displayBlock {|x| cText }             the text of the field after taking it
 *                                          (default: "<value> - <label>")
 *   detailsBlock {|x| { { cName, xVal } } } shown on the right of a row (optional)
 *   filterBlock  {|x, aTerms| lMatch }    aTerms: the lower-case text and its Cyrillic
 *
 *   onSelected {|xItem, xValue| }  onCleared {|| }  onChanged {|xValue| }
 *   onSearch {|cText| }  onEnter {|| }  onAdd {|| }
 *
 * @extends HTWidget
 */

#include "hbtui.ch"
#include "inkey.ch"

CLASS HTSearchBox FROM HTWidget

PROTECTED:

    DATA Fitems      INIT {}
    DATA Ftext       INIT ""         /* the text of the field */
    DATA FnCursor    INIT 1          /* cursor: a character position */
    DATA FnOffset    INIT 1          /* first character shown */
    DATA FnSel       INIT 0          /* the taken item, 0 = none */
    DATA Fresults    INIT {}         /* indexes of the items found */
    DATA FnHigh      INIT 0          /* the marked one in Fresults */
    DATA FnTop       INIT 1          /* the first row of the list */
    DATA FlOpen      INIT .F.        /* the list is shown */
    DATA FdropWinId  INIT NIL
    DATA FnDropRows  INIT 0
    DATA FnDropW     INIT 0
    DATA FlInLoop    INIT .F.

    METHOD textChanged()
    METHOD search( cTerm )
    METHOD terms( cTerm )
    METHOD moveMark( nDelta )
    METHOD takeMarked()
    METHOD topWindow()
    METHOD moveOn()
    METHOD repaintSelf()
    METHOD openList()
    METHOD closeList()
    METHOD paintList()
    METHOD listLoop()
    METHOD itemDetails( xItem )

PUBLIC:

    CONSTRUCTOR new( ... )

    METHOD paintEvent( paintEvent )
    METHOD keyEvent( keyEvent )
    METHOD mouseEvent( eventMouse )
    METHOD focusOutEvent( eventFocus )
    METHOD cursorPos()
    METHOD destroy()

    METHOD handleKey( nKey )
    METHOD setItems( aItems )
    METHOD items()          INLINE ::Fitems
    METHOD text()           INLINE ::Ftext
    METHOD results()        INLINE ::Fresults
    METHOD marked()         INLINE ::FnHigh
    METHOD isOpen()         INLINE ::FlOpen
    METHOD selectedIndex()  INLINE ::FnSel
    METHOD selectedItem()   INLINE IIF( ::FnSel > 0, ::Fitems[ ::FnSel ], NIL )
    METHOD getValue()
    METHOD setValue( xValue )
    METHOD selectItem( nIndex )
    METHOD clear()
    METHOD itemLabel( xItem )
    METHOD itemValue( xItem )
    METHOD itemDisplay( xItem )
    METHOD matches( xItem, aTerms )

    PROPERTY placeholder READWRITE INIT ""
    PROPERTY minSearchLength READWRITE INIT 1
    PROPERTY maxResults READWRITE INIT 20
    PROPERTY dropRows READWRITE INIT 8
    PROPERTY advanceOnEnter READWRITE INIT .T.
    PROPERTY labelBlock READWRITE
    PROPERTY valueBlock READWRITE
    PROPERTY displayBlock READWRITE
    PROPERTY detailsBlock READWRITE
    PROPERTY filterBlock READWRITE
    PROPERTY onSelected READWRITE
    PROPERTY onCleared READWRITE
    PROPERTY onChanged READWRITE
    PROPERTY onSearch READWRITE
    PROPERTY onEnter READWRITE
    PROPERTY onAdd READWRITE

ENDCLASS

METHOD new( ... ) CLASS HTSearchBox

    LOCAL p

    IF pCount() > 0 .AND. hb_isObject( p := hb_pValue( 1 ) )
        ::super:new( p )
    ELSE
        ::super:new()
    ENDIF
    ::setFocusPolicy( HT_FOCUS_STRONG )
    ::FisVisible := .T.
    ::Fheight := 1
    ::Fwidth := 30

RETURN self

METHOD PROCEDURE destroy() CLASS HTSearchBox
    ::closeList()
    ::super:destroy()
RETURN

/*----------------------------------------------------------- the items */

METHOD PROCEDURE setItems( aItems ) CLASS HTSearchBox

    LOCAL xValue := ::getValue()

    ::Fitems := hb_defaultValue( aItems, {} )
    ::FnSel := 0
    ::Fresults := {}
    ::FnHigh := 0
    ::FlOpen := .F.
    IF xValue != NIL
        ::setValue( xValue )          /* the same value in the new items */
    ENDIF

RETURN

METHOD FUNCTION itemLabel( xItem ) CLASS HTSearchBox

    IF ::FlabelBlock != NIL
        RETURN hb_CStr( Eval( ::FlabelBlock, xItem ) )
    ENDIF
    DO CASE
    CASE HB_ISARRAY( xItem )
        RETURN hb_CStr( IIF( Len( xItem ) >= 2, xItem[ 2 ], IIF( Len( xItem ) == 1, xItem[ 1 ], "" ) ) )
    CASE HB_ISHASH( xItem )
        RETURN hb_CStr( hb_HGetDef( xItem, "naziv", hb_HGetDef( xItem, "label", "" ) ) )
    ENDCASE

RETURN hb_CStr( xItem )

METHOD FUNCTION itemValue( xItem ) CLASS HTSearchBox

    IF ::FvalueBlock != NIL
        RETURN Eval( ::FvalueBlock, xItem )
    ENDIF
    DO CASE
    CASE HB_ISARRAY( xItem )
        RETURN IIF( Len( xItem ) >= 1, xItem[ 1 ], NIL )
    CASE HB_ISHASH( xItem )
        RETURN hb_HGetDef( xItem, "sifra", hb_HGetDef( xItem, "value", NIL ) )
    ENDCASE

RETURN xItem

METHOD FUNCTION itemDisplay( xItem ) CLASS HTSearchBox

    LOCAL xValue, cLabel

    IF ::FdisplayBlock != NIL
        RETURN hb_CStr( Eval( ::FdisplayBlock, xItem ) )
    ENDIF
    xValue := ::itemValue( xItem )
    cLabel := ::itemLabel( xItem )
    IF xValue == NIL .OR. ( HB_ISSTRING( xValue ) .AND. xValue == cLabel )
        RETURN cLabel
    ENDIF

RETURN AllTrim( hb_CStr( xValue ) ) + " - " + cLabel

/* "Šifra: 1  PIB: 100" of an item, "" without detailsBlock */
METHOD FUNCTION itemDetails( xItem ) CLASS HTSearchBox

    LOCAL a, c := "", d

    IF ::FdetailsBlock == NIL
        RETURN ""
    ENDIF
    a := Eval( ::FdetailsBlock, xItem )
    IF HB_ISARRAY( a )
        FOR EACH d IN a
            IF HB_ISARRAY( d ) .AND. Len( d ) >= 2 .AND. ! Empty( d[ 2 ] )
                c += IIF( Empty( c ), "", "  " ) + hb_CStr( d[ 1 ] ) + ": " + AllTrim( hb_CStr( d[ 2 ] ) )
            ENDIF
        NEXT
    ENDIF

RETURN c

/* the item matches when one of the terms is in its label, details or value */
METHOD FUNCTION matches( xItem, aTerms ) CLASS HTSearchBox

    LOCAL c, t

    IF ::FfilterBlock != NIL
        RETURN Eval( ::FfilterBlock, xItem, aTerms )
    ENDIF
    c := Lower( ::itemLabel( xItem ) + " " + ::itemDetails( xItem ) + " " + AllTrim( hb_CStr( ::itemValue( xItem ) ) ) )
    FOR EACH t IN aTerms
        IF ! Empty( t ) .AND. t $ c
            RETURN .T.
        ENDIF
    NEXT

RETURN .F.

/* the text to look for: lower case, and in Cyrillic when it was typed in Latin */
METHOD FUNCTION terms( cTerm ) CLASS HTSearchBox

    LOCAL aTerms := { Lower( AllTrim( cTerm ) ) }

    IF ht_looksLatin( cTerm )
        AAdd( aTerms, Lower( ht_latinToCyrillic( AllTrim( cTerm ) ) ) )
    ENDIF

RETURN aTerms

METHOD PROCEDURE search( cTerm ) CLASS HTSearchBox

    LOCAL aTerms := ::terms( cTerm ), x

    ::Fresults := {}
    FOR EACH x IN ::Fitems
        IF ::matches( x, aTerms )
            AAdd( ::Fresults, x:__enumIndex() )
            IF Len( ::Fresults ) >= ::FmaxResults
                EXIT
            ENDIF
        ENDIF
    NEXT
    ::FnHigh := IIF( Len( ::Fresults ) > 0, 1, 0 )
    ::FnTop := 1
    ::FlOpen := Len( ::Fresults ) > 0

RETURN

/*----------------------------------------------------------- the value */

METHOD FUNCTION getValue() CLASS HTSearchBox
RETURN IIF( ::FnSel > 0, ::itemValue( ::Fitems[ ::FnSel ] ), NIL )

/* takes the item of a value (compared as text too: 7 finds "7"); not found / NIL = clear */
METHOD PROCEDURE setValue( xValue ) CLASS HTSearchBox

    LOCAL n := 0

    IF xValue != NIL .AND. !( HB_ISSTRING( xValue ) .AND. Empty( xValue ) )
        n := AScan( ::Fitems, {| x | ht_sameValue( ::itemValue( x ), xValue ) } )
    ENDIF
    ::FnSel := n
    ::Ftext := IIF( n > 0, ::itemDisplay( ::Fitems[ n ] ), "" )
    ::FnCursor := Len( ::Ftext ) + 1
    ::FnOffset := 1
    ::Fresults := {}
    ::FnHigh := 0
    ::FlOpen := .F.
    ::repaintSelf()

RETURN

METHOD PROCEDURE selectItem( nIndex ) CLASS HTSearchBox

    IF nIndex < 1 .OR. nIndex > Len( ::Fitems )
        RETURN
    ENDIF
    ::FnSel := nIndex
    ::Ftext := ::itemDisplay( ::Fitems[ nIndex ] )
    ::FnCursor := Len( ::Ftext ) + 1
    ::FnOffset := 1
    ::Fresults := {}
    ::FnHigh := 0
    ::FlOpen := .F.
    ::closeList()
    ::repaintSelf()
    IF ::FonChanged != NIL
        Eval( ::FonChanged, ::getValue() )
    ENDIF
    IF ::FonSelected != NIL
        Eval( ::FonSelected, ::Fitems[ nIndex ], ::getValue() )
    ENDIF

RETURN

METHOD PROCEDURE clear() CLASS HTSearchBox

    LOCAL lHad := ::FnSel > 0

    ::FnSel := 0
    ::Ftext := ""
    ::FnCursor := 1
    ::FnOffset := 1
    ::Fresults := {}
    ::FnHigh := 0
    ::FlOpen := .F.
    ::closeList()
    ::repaintSelf()
    IF lHad .AND. ::FonChanged != NIL
        Eval( ::FonChanged, NIL )
    ENDIF
    IF ::FonCleared != NIL
        Eval( ::FonCleared )
    ENDIF

RETURN

/* after an edit: a text other than the taken item's drops it; search again */
METHOD PROCEDURE textChanged() CLASS HTSearchBox

    LOCAL cTerm := AllTrim( ::Ftext )

    IF ::FonSearch != NIL
        Eval( ::FonSearch, cTerm )
    ENDIF
    IF ::FnSel > 0 .AND. !( cTerm == AllTrim( ::itemDisplay( ::Fitems[ ::FnSel ] ) ) )
        ::FnSel := 0
        IF ::FonChanged != NIL
            Eval( ::FonChanged, NIL )
        ENDIF
    ENDIF
    IF Len( cTerm ) >= ::FminSearchLength .AND. Len( cTerm ) > 0
        ::search( cTerm )
    ELSE
        ::Fresults := {}
        ::FnHigh := 0
        ::FlOpen := .F.
        IF Len( cTerm ) == 0 .AND. Len( ::Ftext ) == 0
            ::clear()
        ENDIF
    ENDIF

RETURN

/*----------------------------------------------------------- keys */

METHOD PROCEDURE moveMark( nDelta ) CLASS HTSearchBox
    IF Len( ::Fresults ) > 0
        ::FnHigh := Max( 1, Min( Len( ::Fresults ), ::FnHigh + nDelta ) )
    ENDIF
RETURN

METHOD PROCEDURE takeMarked() CLASS HTSearchBox
    IF ::FnHigh >= 1 .AND. ::FnHigh <= Len( ::Fresults )
        ::selectItem( ::Fresults[ ::FnHigh ] )
    ENDIF
RETURN

/** One key in the field (and in the open list). @return .T. when the key was used */
METHOD FUNCTION handleKey( nKey ) CLASS HTSearchBox

    LOCAL cChar, lEdit := .F.

    SWITCH nKey
    CASE K_DOWN
        IF ! ::FlOpen
            /* Down opens the list again (Esc closed it) */
            IF ::FnSel == 0 .AND. Len( AllTrim( ::Ftext ) ) >= Max( 1, ::FminSearchLength )
                ::search( AllTrim( ::Ftext ) )
            ELSE
                RETURN .F.
            ENDIF
        ELSE
            ::moveMark( 1 )
        ENDIF
        EXIT
    CASE K_UP
        IF ! ::FlOpen
            RETURN .F.
        ENDIF
        ::moveMark( -1 )
        EXIT
    CASE K_PGDN
    CASE K_PGUP
        IF ! ::FlOpen
            RETURN .F.
        ENDIF
        ::moveMark( IIF( nKey == K_PGDN, 1, -1 ) * Max( 1, ::FdropRows - 1 ) )
        EXIT
    CASE K_CTRL_HOME
    CASE K_CTRL_END
        IF ! ::FlOpen
            RETURN .F.
        ENDIF
        ::FnHigh := IIF( nKey == K_CTRL_HOME, 1, Len( ::Fresults ) )
        EXIT
    CASE K_ESC
        IF ! ::FlOpen
            RETURN .F.                /* the dialog gets it */
        ENDIF
        ::FlOpen := .F.               /* only the list */
        ::closeList()
        EXIT
    CASE K_ENTER
        DO CASE
        CASE ::FlOpen .AND. Len( ::Fresults ) > 0
            ::takeMarked()
        CASE ::FnSel > 0 .OR. Empty( ::Ftext )
        OTHERWISE
            RETURN .T.                /* text and nothing found: stay */
        ENDCASE
        IF ::FonEnter != NIL
            Eval( ::FonEnter )
        ENDIF
        IF ::FadvanceOnEnter
            ::moveOn()
        ENDIF
        RETURN .T.
    CASE K_LEFT
        ::FnCursor := Max( 1, ::FnCursor - 1 )
        EXIT
    CASE K_RIGHT
        ::FnCursor := Min( Len( ::Ftext ) + 1, ::FnCursor + 1 )
        EXIT
    CASE K_HOME
        ::FnCursor := 1
        EXIT
    CASE K_END
        ::FnCursor := Len( ::Ftext ) + 1
        EXIT
    CASE K_BS
        IF ::FnCursor > 1
            ::Ftext := Left( ::Ftext, ::FnCursor - 2 ) + SubStr( ::Ftext, ::FnCursor )
            ::FnCursor--
            lEdit := .T.
        ENDIF
        EXIT
    CASE K_DEL
        IF ::FnCursor <= Len( ::Ftext )
            ::Ftext := Left( ::Ftext, ::FnCursor - 1 ) + SubStr( ::Ftext, ::FnCursor + 1 )
            lEdit := .T.
        ENDIF
        EXIT
    CASE K_CTRL_Y
        IF ! Empty( ::Ftext ) .OR. ::FnSel > 0
            ::clear()
        ENDIF
        EXIT
    OTHERWISE
        cChar := ht_keyText( nKey )
        IF cChar == ""
            RETURN .F.
        ENDIF
        ::Ftext := Left( ::Ftext, ::FnCursor - 1 ) + cChar + SubStr( ::Ftext, ::FnCursor )
        ::FnCursor += Len( cChar )
        lEdit := .T.
    ENDSWITCH

    IF lEdit
        ::textChanged()
    ENDIF
    IF ! ::FlOpen
        ::closeList()
    ENDIF
    ::repaintSelf()
    IF ::FlOpen
        ::openList()
    ENDIF

RETURN .T.

METHOD PROCEDURE keyEvent( keyEvent ) CLASS HTSearchBox

    IF ::handleKey( keyEvent:key )
        keyEvent:accept()
        /* the list opened: it takes the keys until it closes (as HTComboBox) */
        IF ::FlOpen .AND. ::FdropWinId != NIL .AND. ! ::FlInLoop
            ::listLoop()
        ENDIF
    ENDIF

RETURN

/* leaving the field: a text that matches one item only takes it */
METHOD PROCEDURE focusOutEvent( eventFocus ) CLASS HTSearchBox

    LOCAL cTerm := AllTrim( ::Ftext ), aTerms, n := 0, nFound := 0, x

    ::FlOpen := .F.
    ::closeList()
    IF ::FnSel == 0 .AND. Len( cTerm ) >= Max( 1, ::FminSearchLength )
        aTerms := ::terms( cTerm )
        FOR EACH x IN ::Fitems
            IF ::matches( x, aTerms )
                nFound++
                n := x:__enumIndex()
                IF nFound > 1
                    EXIT
                ENDIF
            ENDIF
        NEXT
        IF nFound == 1
            ::selectItem( n )
        ENDIF
    ENDIF
    ::super:focusOutEvent( eventFocus )

RETURN

/*----------------------------------------------------------- the mouse */

/* x clears, + adds; a click in the text puts the cursor there (and opens the list) */
METHOD PROCEDURE mouseEvent( eventMouse ) CLASS HTSearchBox

    LOCAL nCol := eventMouse:mouseCol

    IF eventMouse:nKey != K_LBUTTONDOWN
        RETURN
    ENDIF
    IF nCol == ::Fwidth - 1 .AND. ::Fwidth >= 4
        IF ! Empty( ::Ftext ) .OR. ::FnSel > 0
            ::clear()
        ELSEIF ::FonAdd != NIL
            Eval( ::FonAdd )
        ENDIF
        RETURN
    ENDIF
    ::FnCursor := Min( Len( ::Ftext ) + 1, ::FnOffset - 1 + ht_cellPos( SubStr( ::Ftext, ::FnOffset ), nCol ) )
    IF ::FnSel == 0 .AND. Len( AllTrim( ::Ftext ) ) >= Max( 1, ::FminSearchLength ) .AND. ! ::FlOpen
        ::search( AllTrim( ::Ftext ) )
    ENDIF
    ::repaintSelf()
    IF ::FlOpen
        ::openList()
        IF ! ::FlInLoop
            ::listLoop()
        ENDIF
    ENDIF

RETURN

/*----------------------------------------------------------- painting */

METHOD FUNCTION cursorPos() CLASS HTSearchBox
RETURN IIF( ::hasFocus(), { 0, ht_cellCol( ::Ftext, ::FnOffset, ::FnCursor ) }, NIL )

METHOD PROCEDURE paintEvent( paintEvent ) CLASS HTSearchBox

    LOCAL nW := MaxCol() + 1, nText, cColor, cBtn := " ", cDim

    HB_SYMBOL_UNUSED( paintEvent )

    cColor := HTTheme():getColor( IIF( ::hasFocus(), HT_CLR_LINEEDIT_FOCUSED, HT_CLR_LINEEDIT_NORMAL ) )
    nText := IIF( nW >= 4, nW - 2, nW )
    IF nW >= 4
        IF ! Empty( ::Ftext ) .OR. ::FnSel > 0
            cBtn := ht_glyph( 0x00D7, "x" )                  /* x clears */
        ELSEIF ::FonAdd != NIL
            cBtn := "+"                                      /* + adds */
        ENDIF
    ENDIF
    ::FnOffset := ht_scrollToCursor( ::Ftext, ::FnOffset, ::FnCursor, nText )
    IF Empty( ::Ftext ) .AND. ! Empty( ::Fplaceholder )
        /* the placeholder: dark gray on the field's background */
        cDim := "N+/" + hb_tokenGet( hb_tokenGet( cColor, 1, "," ), 2, "/" )
        DispOutAt( 0, 0, ht_cellPadR( ht_cellLeft( ::Fplaceholder, nText ), nText ), cDim )
    ELSE
        DispOutAt( 0, 0, ht_cellPadR( ht_cellLeft( SubStr( ::Ftext, ::FnOffset ), nText ), nText ), cColor )
    ENDIF
    IF nW >= 4
        DispOutAt( 0, nText, " " + cBtn, cColor )
    ENDIF
    IF ::hasFocus()
        SetPos( 0, ht_cellCol( ::Ftext, ::FnOffset, ::FnCursor ) )
    ENDIF

RETURN

/* the field again, through the window (re-establishes the viewport) */
METHOD PROCEDURE repaintSelf() CLASS HTSearchBox

    LOCAL parent := ::parent()

    IF parent != NIL .AND. parent:isDerivedFrom( "HTWidget" ) .AND. ::topWindow() != NIL
        parent:repaintChild( self )
    ENDIF

RETURN

METHOD FUNCTION topWindow() CLASS HTSearchBox

    LOCAL o := ::parent()

    DO WHILE o != NIL .AND. o:isDerivedFrom( "HTWidget" )
        IF o:hasOwnWindow()
            RETURN o
        ENDIF
        o := o:parent()
    ENDDO

RETURN NIL

/* Enter: to the next field, as Tab */
METHOD PROCEDURE moveOn() CLASS HTSearchBox

    LOCAL oWin := ::topWindow()

    IF oWin != NIL
        oWin:focusNextChild()
    ENDIF

RETURN

/*----------------------------------------------------------- the list */

/* the list in a window of its own under the field (over it when there is no room);
   made again when the number of rows changes */
METHOD PROCEDURE openList() CLASS HTSearchBox

    LOCAL oWin := ::topWindow(), aOff := ::windowOffset(), nRows, nW, nTop, nLeft, nOldShadow, i, n

    IF oWin == NIL .OR. aOff == NIL .OR. Len( ::Fresults ) == 0
        RETURN
    ENDIF
    nRows := Min( Len( ::Fresults ), ::FdropRows )
    /* as wide as the field, wider for the details (up to the screen) */
    nW := ::Fwidth
    FOR i := ::FnTop TO Min( Len( ::Fresults ), ::FnTop + nRows - 1 )
        n := ::Fresults[ i ]
        nW := Max( nW, ht_strWidth( ::itemLabel( ::Fitems[ n ] ) ) + ht_strWidth( ::itemDetails( ::Fitems[ n ] ) ) + 4 )
    NEXT
    nW := Min( nW, ht_ScreenMaxCol() + 1 )
    IF ::FdropWinId != NIL .AND. nRows == ::FnDropRows .AND. nW <= ::FnDropW
        ::paintList()
        RETURN
    ENDIF
    ::closeList()

    wSelect( oWin:windowId, .F. )
    /* windowOffset(): the field's row / column in its window, the title row counted */
    nTop := wRow() + aOff[ 1 ] + 1
    nLeft := wCol() + aOff[ 2 ]
    IF nTop + nRows - 1 > ht_ScreenMaxRow()                   /* no room under it: over it */
        nTop := Max( 0, wRow() + aOff[ 1 ] - nRows )
    ENDIF
    nLeft := Max( 0, Min( nLeft, ht_ScreenMaxCol() + 1 - nW ) )

    nOldShadow := wSetShadow( HT_SHADOW_ATTR )
    ::FdropWinId := wOpen( nTop, nLeft, nTop + nRows - 1, nLeft + nW - 1, .T. )
    wSetShadow( nOldShadow )
    ::FnDropRows := nRows
    ::FnDropW := nW
    ::paintList()
    wSelect( oWin:windowId, .F. )

RETURN

METHOD PROCEDURE closeList() CLASS HTSearchBox

    LOCAL oWin

    IF ::FdropWinId != NIL
        wClose( ::FdropWinId )
        ::FdropWinId := NIL
        ::FnDropRows := 0
        ::FnDropW := 0
        oWin := ::topWindow()
        IF oWin != NIL
            wSelect( oWin:windowId, .F. )
        ENDIF
    ENDIF

RETURN

/* a row of the list: the label on the left, the details on the right */
METHOD PROCEDURE paintList() CLASS HTSearchBox

    LOCAL nRows, nW, i, nRow := 0, n, cLabel, cDet, cColor, oWin

    IF ::FdropWinId == NIL
        RETURN
    ENDIF
    wSelect( ::FdropWinId, .T. )
    nRows := MaxRow() + 1
    nW := MaxCol() + 1
    IF ::FnHigh < ::FnTop
        ::FnTop := Max( 1, ::FnHigh )
    ELSEIF ::FnHigh >= ::FnTop + nRows
        ::FnTop := ::FnHigh - nRows + 1
    ENDIF
    FOR i := ::FnTop TO Min( Len( ::Fresults ), ::FnTop + nRows - 1 )
        n := ::Fresults[ i ]
        cColor := HTTheme():getColor( IIF( i == ::FnHigh, HT_CLR_COMBO_DROP_S, HT_CLR_COMBO_DROP_N ) )
        cDet := ::itemDetails( ::Fitems[ n ] )
        cDet := ht_cellLeft( cDet, Max( 0, nW - 4 ) )
        cLabel := " " + ht_cellLeft( ::itemLabel( ::Fitems[ n ] ), Max( 0, nW - ht_strWidth( cDet ) - 3 ) )
        DispOutAt( nRow, 0, ht_cellPadR( cLabel, nW - ht_strWidth( cDet ) - 1 ) + cDet + " ", cColor )
        nRow++
    NEXT
    DO WHILE nRow < nRows
        DispOutAt( nRow, 0, Space( nW ), HTTheme():getColor( HT_CLR_COMBO_DROP_N ) )
        nRow++
    ENDDO
    oWin := ::topWindow()
    IF oWin != NIL
        wSelect( oWin:windowId, .F. )
    ENDIF

RETURN

/* the open list takes the keys and the mouse until it closes; a key it does not use
   (Tab, F-keys ...) closes it and goes to the window */
METHOD PROCEDURE listLoop() CLASS HTSearchBox

    LOCAL event, nRow, oPending, oWin

    ::FlInLoop := .T.
    DO WHILE ::FlOpen .AND. ::FdropWinId != NIL
        event := HTEventLoop():poll( 0.05 )
        IF event == NIL
            LOOP
        ENDIF
        IF event:className() == "HTKEYEVENT"
            IF ! ::handleKey( event:key )
                oPending := event
                ::FlOpen := .F.
                ::closeList()
            ENDIF
        ELSEIF event:className() == "HTMOUSEEVENT"
            IF ht_windowAtMousePos() == ::FdropWinId
                wSelect( ::FdropWinId, .F. )
                nRow := mRow()
                oWin := ::topWindow()
                IF oWin != NIL
                    wSelect( oWin:windowId, .F. )
                ENDIF
                IF nRow >= 0 .AND. ::FnTop + nRow <= Len( ::Fresults )
                    IF event:nKey == K_LBUTTONDOWN
                        ::FnHigh := ::FnTop + nRow
                        ::takeMarked()
                        IF ::FadvanceOnEnter
                            ::moveOn()
                        ENDIF
                    ELSEIF event:nKey == K_MOUSEMOVE .AND. ::FnHigh != ::FnTop + nRow
                        ::FnHigh := ::FnTop + nRow          /* the mark follows the mouse */
                        ::paintList()
                    ENDIF
                ENDIF
            ELSEIF event:nKey == K_LBUTTONDOWN .OR. event:nKey == K_RBUTTONDOWN
                ::FlOpen := .F.                             /* a click elsewhere closes it */
                ::closeList()
                ::repaintSelf()
            ENDIF
        ENDIF
    ENDDO
    ::closeList()
    ::FlInLoop := .F.
    ::repaintSelf()
    IF oPending != NIL .AND. ( oWin := ::topWindow() ) != NIL
        oWin:keyEvent( oPending )
    ENDIF

RETURN

/*----------------------------------------------------------- helpers */

/* the same value, also as text (7 and "7", "0007" and 7 do not) */
FUNCTION ht_sameValue( x1, x2 )

    IF ValType( x1 ) == ValType( x2 )
        RETURN x1 == x2
    ENDIF

RETURN AllTrim( hb_CStr( x1 ) ) == AllTrim( hb_CStr( x2 ) )

/* .T. for text in Latin letters without Cyrillic (Serbian: č ć đ š ž too) */
FUNCTION ht_looksLatin( cText )

    LOCAL i, n, lLatin := .F.

    FOR i := 1 TO Len( cText )
        n := hb_UCode( SubStr( cText, i, 1 ) )
        IF n >= 0x0400 .AND. n <= 0x04FF
            RETURN .F.
        ENDIF
        IF ( n >= 65 .AND. n <= 90 ) .OR. ( n >= 97 .AND. n <= 122 ) .OR. hb_AScan( { 0x10C, 0x10D, 0x106, 0x107, 0x110, 0x111, 0x160, 0x161, 0x17D, 0x17E }, n ) > 0
            lLatin := .T.
        ENDIF
    NEXT

RETURN lLatin

/* Serbian Latin (Gaj) to Cyrillic: lj nj dž as one letter each; other characters stay */
FUNCTION ht_latinToCyrillic( cText )

    STATIC s_hOne, s_hTwo
    LOCAL c := "", i := 1, cTwo, cOne, nLen := Len( cText )

    IF s_hOne == NIL
        s_hOne := { => }
        s_hTwo := { => }
        hb_HCaseMatch( s_hOne, .T. )
        hb_HCaseMatch( s_hTwo, .T. )
        AEval( { { "a", 0x430 }, { "b", 0x431 }, { "c", 0x446 }, { hb_UChar( 0x10D ), 0x447 }, { hb_UChar( 0x107 ), 0x45B }, ;
                 { "d", 0x434 }, { hb_UChar( 0x111 ), 0x452 }, { "e", 0x435 }, { "f", 0x444 }, { "g", 0x433 }, { "h", 0x445 }, ;
                 { "i", 0x438 }, { "j", 0x458 }, { "k", 0x43A }, { "l", 0x43B }, { "m", 0x43C }, { "n", 0x43D }, { "o", 0x43E }, ;
                 { "p", 0x43F }, { "r", 0x440 }, { "s", 0x441 }, { hb_UChar( 0x161 ), 0x448 }, { "t", 0x442 }, { "u", 0x443 }, ;
                 { "v", 0x432 }, { "z", 0x437 }, { hb_UChar( 0x17E ), 0x436 } }, ;
              {| a | s_hOne[ a[ 1 ] ] := hb_UChar( a[ 2 ] ), s_hOne[ Upper( a[ 1 ] ) ] := Upper( hb_UChar( a[ 2 ] ) ) } )
        AEval( { { "lj", 0x459 }, { "nj", 0x45A }, { "d" + hb_UChar( 0x17E ), 0x45F } }, ;
              {| a | s_hTwo[ a[ 1 ] ] := hb_UChar( a[ 2 ] ), ;
                     s_hTwo[ Upper( Left( a[ 1 ], 1 ) ) + SubStr( a[ 1 ], 2 ) ] := Upper( hb_UChar( a[ 2 ] ) ), ;
                     s_hTwo[ Upper( a[ 1 ] ) ] := Upper( hb_UChar( a[ 2 ] ) ) } )
    ENDIF

    DO WHILE i <= nLen
        cTwo := SubStr( cText, i, 2 )
        IF Len( cTwo ) == 2 .AND. hb_HHasKey( s_hTwo, cTwo )
            c += s_hTwo[ cTwo ]
            i += 2
            LOOP
        ENDIF
        cOne := SubStr( cText, i, 1 )
        c += IIF( hb_HHasKey( s_hOne, cOne ), s_hOne[ cOne ], cOne )
        i++
    ENDDO

RETURN c
