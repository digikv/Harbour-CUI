/** @class HTTagInput
 * A field of removable tags (filters, warehouses, statuses).
 * Type and press Enter or comma to add a tag. Backspace on an empty field
 * removes the last tag. A click on the × of a tag removes that one.
 * setSuggestions() offers matches under the field when the widget is taller
 * than one row; Down / Up move the mark and Enter takes it. Latin text finds
 * a Cyrillic suggestion the same way HTSearchBox does.
 * @extends HTWidget
 *
 *   addTag( c )  removeTag( n )  tags()  setTags( a )  text()
 *   setSuggestions( a )  filtered()  marked()
 *   onChanged {| aTags |}
 */

#include "hbtui.ch"
#include "inkey.ch"

CLASS HTTagInput FROM HTWidget

PROTECTED:

    DATA Ftags     INIT {}
    DATA Ftext     INIT ""
    DATA FnCursor  INIT 1
    DATA Fsuggest  INIT {}
    DATA Fmark     INIT 0
    DATA Ffirst    INIT 1      /* first tag painted, when the row overflows */
    DATA FnTextAt  INIT 0

    METHOD commitText()
    METHOD filteredList()
    METHOD hasTag( c )
    METHOD chipBoxes( nWidth )
    METHOD textCol( nWidth )
    METHOD scrollToFit( nWidth )
    METHOD repaintSelf()

PUBLIC:

    CONSTRUCTOR new( ... )

    METHOD paintEvent( paintEvent )
    METHOD keyEvent( keyEvent )
    METHOD mouseEvent( eventMouse )
    METHOD handleKey( nKey )
    METHOD cursorPos()

    METHOD addTag( cTag )
    METHOD removeTag( nIndex )
    METHOD clear()
    METHOD tags()              INLINE AClone( ::Ftags )
    METHOD tagCount()          INLINE Len( ::Ftags )
    METHOD text()              INLINE ::Ftext
    METHOD setText( c )
    METHOD setTags( aTags )
    METHOD setSuggestions( aItems )
    METHOD suggestions()       INLINE AClone( ::Fsuggest )
    METHOD filtered()
    METHOD marked()            INLINE ::Fmark
    METHOD chipHit( nCol, nWidth )

    PROPERTY onChanged READWRITE

ENDCLASS

/** Creates a tag field. Optional parent. */
METHOD new( ... ) CLASS HTTagInput

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
    ::Fwidth := 40

RETURN self

/** Adds a tag. Empty text and an existing tag (ignoring case) are skipped.
 * @return .T. when the tag was added
 */
METHOD FUNCTION addTag( cTag ) CLASS HTTagInput

    IF ! HB_ISSTRING( cTag )
        RETURN .F.
    ENDIF
    cTag := AllTrim( cTag )
    IF Len( cTag ) == 0 .OR. ::hasTag( cTag )
        RETURN .F.
    ENDIF
    AAdd( ::Ftags, cTag )
    ::Ffirst := 1
    IF ::FonChanged != NIL
        Eval( ::FonChanged, ::tags() )
    ENDIF
    ::repaintSelf()

RETURN .T.

/** Removes tag n (1-based). */
METHOD FUNCTION removeTag( nIndex ) CLASS HTTagInput

    IF ! HB_ISNUMERIC( nIndex ) .OR. nIndex < 1 .OR. nIndex > Len( ::Ftags )
        RETURN .F.
    ENDIF
    hb_aDel( ::Ftags, nIndex, .T. )
    IF ::Ffirst > Len( ::Ftags )
        ::Ffirst := Max( 1, Len( ::Ftags ) )
    ENDIF
    IF ::FonChanged != NIL
        Eval( ::FonChanged, ::tags() )
    ENDIF
    ::repaintSelf()

RETURN .T.

/** Drops every tag and the text being typed. */
METHOD PROCEDURE clear() CLASS HTTagInput
    ::Ftags := {}
    ::Ftext := ""
    ::FnCursor := 1
    ::Fmark := 0
    ::Ffirst := 1
    IF ::FonChanged != NIL
        Eval( ::FonChanged, {} )
    ENDIF
    ::repaintSelf()
RETURN

METHOD PROCEDURE setTags( aTags ) CLASS HTTagInput

    LOCAL x

    ::Ftags := {}
    IF HB_ISARRAY( aTags )
        FOR EACH x IN aTags
            IF HB_ISSTRING( x ) .AND. Len( AllTrim( x ) ) > 0 .AND. ! ::hasTag( AllTrim( x ) )
                AAdd( ::Ftags, AllTrim( x ) )
            ENDIF
        NEXT
    ENDIF
    ::Ffirst := 1
    ::repaintSelf()

RETURN

METHOD PROCEDURE setText( c ) CLASS HTTagInput
    ::Ftext := IIF( HB_ISSTRING( c ), c, "" )
    ::FnCursor := Len( ::Ftext ) + 1
    ::Fmark := 0
    ::repaintSelf()
RETURN

METHOD PROCEDURE setSuggestions( aItems ) CLASS HTTagInput

    LOCAL x

    ::Fsuggest := {}
    IF HB_ISARRAY( aItems )
        FOR EACH x IN aItems
            IF HB_ISSTRING( x )
                AAdd( ::Fsuggest, x )
            ENDIF
        NEXT
    ENDIF
    ::Fmark := 0

RETURN

/** Suggestions that contain the text being typed. */
METHOD FUNCTION filtered() CLASS HTTagInput
RETURN ::filteredList()

/** Tag index whose × is at column nCol, given a row of nWidth cells, or 0. */
METHOD FUNCTION chipHit( nCol, nWidth ) CLASS HTTagInput

    LOCAL a := ::chipBoxes( nWidth )
    LOCAL box

    FOR EACH box IN a
        IF nCol == box[ 4 ]
            RETURN box[ 1 ]
        ENDIF
    NEXT

RETURN 0

METHOD PROCEDURE paintEvent( paintEvent ) CLASS HTTagInput

    LOCAL nWidth := MaxCol() + 1
    LOCAL nHeight := MaxRow() + 1
    LOCAL aBox, box, cText, cColor, nRow, aHits, i
    LOCAL cField, nOff, cClose
    LOCAL lFocus := ::hasFocus()
    LOCAL cEdit := HTTheme():getColor( IIF( lFocus, HT_CLR_LINEEDIT_FOCUSED, HT_CLR_LINEEDIT_NORMAL ) )
    LOCAL cChip := HTTheme():getColor( HT_CLR_BUTTON_NORMAL )
    LOCAL cHot := HTTheme():getColor( HT_CLR_MENU_HOTKEY )
    LOCAL cList := HTTheme():getColor( HT_CLR_LIST_NORMAL )
    LOCAL cSel := HTTheme():getColor( HT_CLR_LIST_SELECTED )

    HB_SYMBOL_UNUSED( paintEvent )

    ::scrollToFit( nWidth )
    DispOutAt( 0, 0, Space( nWidth ), cEdit )
    cClose := ht_glyph( 0x00D7, "x" )
    IF ::Ffirst > 1
        DispOutAt( 0, 0, "<", cHot )
    ENDIF
    aBox := ::chipBoxes( nWidth )
    FOR EACH box IN aBox
        cText := "[" + ::Ftags[ box[ 1 ] ] + " " + cClose + "]"
        DispOutAt( 0, box[ 2 ], cText, cChip )
        DispOutAt( 0, box[ 4 ], cClose, cHot )
    NEXT

    nOff := ::textCol( nWidth )
    cField := ::Ftext
    IF nOff < nWidth
        DispOutAt( 0, nOff, ht_cellLeft( cField, nWidth - nOff ), cEdit )
    ENDIF

    aHits := ::filteredList()
    FOR nRow := 1 TO nHeight - 1
        i := nRow
        IF i <= Len( aHits )
            cColor := IIF( i == ::Fmark, cSel, cList )
            DispOutAt( nRow, 0, ht_cellPadR( aHits[ i ], nWidth ), cColor )
        ELSE
            DispOutAt( nRow, 0, Space( nWidth ), cList )
        ENDIF
    NEXT

RETURN

/** Cursor on the text being typed, for the GT caret. */
METHOD FUNCTION cursorPos() CLASS HTTagInput

    LOCAL nCol

    nCol := 0
    IF ::Fwidth > 0
        nCol := ::textCol( ::Fwidth ) + ht_cellCol( ::Ftext, 1, ::FnCursor )
    ENDIF

RETURN { 0, nCol }

METHOD PROCEDURE keyEvent( keyEvent ) CLASS HTTagInput
    IF ::handleKey( keyEvent:key )
        keyEvent:accept()
        ::repaintSelf()
    ENDIF
RETURN

/** Keys. .T. when the field uses the key. */
METHOD FUNCTION handleKey( nKey ) CLASS HTTagInput

    LOCAL c, aHits

    c := ht_keyText( nKey )
    aHits := ::filteredList()

    DO CASE
    CASE nKey == K_BS
        IF ::FnCursor > 1
            ::Ftext := Left( ::Ftext, ::FnCursor - 2 ) + SubStr( ::Ftext, ::FnCursor )
            ::FnCursor--
            ::Fmark := 0
        ELSEIF Len( ::Ftags ) > 0
            ::removeTag( Len( ::Ftags ) )
        ENDIF
    CASE nKey == K_DEL
        IF ::FnCursor <= Len( ::Ftext )
            ::Ftext := Left( ::Ftext, ::FnCursor - 1 ) + SubStr( ::Ftext, ::FnCursor + 1 )
            ::Fmark := 0
        ENDIF
    CASE nKey == K_LEFT
        IF ::FnCursor > 1
            ::FnCursor--
        ENDIF
    CASE nKey == K_RIGHT
        IF ::FnCursor <= Len( ::Ftext )
            ::FnCursor++
        ENDIF
    CASE nKey == K_HOME
        ::FnCursor := 1
    CASE nKey == K_END
        ::FnCursor := Len( ::Ftext ) + 1
    CASE nKey == K_DOWN
        IF Len( aHits ) == 0
            RETURN .F.
        ENDIF
        IF ::Fmark < 1 .OR. ::Fmark >= Len( aHits )
            ::Fmark := 1
        ELSE
            ::Fmark++
        ENDIF
    CASE nKey == K_UP
        IF Len( aHits ) == 0
            RETURN .F.
        ENDIF
        IF ::Fmark > 1
            ::Fmark--
        ELSE
            ::Fmark := 1
        ENDIF
    CASE nKey == K_ENTER
        IF ::Fmark >= 1 .AND. ::Fmark <= Len( aHits ) .AND. Len( AllTrim( ::Ftext ) ) > 0
            ::addTag( aHits[ ::Fmark ] )
            ::Ftext := ""
            ::FnCursor := 1
            ::Fmark := 0
        ELSE
            ::commitText()
        ENDIF
    CASE c == ","
        ::commitText()
    CASE Len( c ) >= 1
        ::Ftext := Left( ::Ftext, ::FnCursor - 1 ) + c + SubStr( ::Ftext, ::FnCursor )
        ::FnCursor += Len( c )
        ::Fmark := 0
    OTHERWISE
        RETURN .F.
    ENDCASE

RETURN .T.

METHOD PROCEDURE mouseEvent( eventMouse ) CLASS HTTagInput

    LOCAL nHit, nWidth, aHits

    IF eventMouse:nKey == K_LBUTTONDOWN
        nWidth := IIF( ::Fwidth > 0, ::Fwidth, 40 )
        IF eventMouse:mouseRow <= 0
            nHit := ::chipHit( eventMouse:mouseCol, nWidth )
            IF nHit > 0
                ::removeTag( nHit )
            ENDIF
        ELSE
            aHits := ::filteredList()
            nHit := eventMouse:mouseRow
            IF nHit >= 1 .AND. nHit <= Len( aHits )
                ::addTag( aHits[ nHit ] )
                ::Ftext := ""
                ::FnCursor := 1
                ::Fmark := 0
                ::repaintSelf()
            ENDIF
        ENDIF
    ENDIF

RETURN

METHOD PROCEDURE commitText() CLASS HTTagInput

    LOCAL c := AllTrim( ::Ftext )

    IF Len( c ) > 0
        ::addTag( c )
    ENDIF
    ::Ftext := ""
    ::FnCursor := 1
    ::Fmark := 0

RETURN

METHOD FUNCTION filteredList() CLASS HTTagInput

    LOCAL a := {}
    LOCAL cTerm := Lower( AllTrim( ::Ftext ) )
    LOCAL cCyr := ""
    LOCAL s, cHay

    IF Len( cTerm ) == 0
        RETURN a
    ENDIF
    IF ht_looksLatin( cTerm )
        cCyr := Lower( ht_latinToCyrillic( cTerm ) )
    ENDIF
    FOR EACH s IN ::Fsuggest
        cHay := Lower( s )
        IF cTerm $ cHay .OR. ( Len( cCyr ) > 0 .AND. cCyr != cTerm .AND. cCyr $ cHay )
            AAdd( a, s )
        ENDIF
    NEXT

RETURN a

METHOD FUNCTION hasTag( c ) CLASS HTTagInput

    LOCAL s

    FOR EACH s IN ::Ftags
        IF Lower( s ) == Lower( c )
            RETURN .T.
        ENDIF
    NEXT

RETURN .F.

/** { nTag, nCol, nWidth, nCloseCol } for the tags that fit. */
METHOD FUNCTION chipBoxes( nWidth ) CLASS HTTagInput

    LOCAL a := {}
    LOCAL i, nCol := 0, nChip, nClose
    LOCAL cClose := ht_glyph( 0x00D7, "x" )

    IF ::Ffirst > 1
        nCol := 2
    ENDIF
    FOR i := Max( 1, ::Ffirst ) TO Len( ::Ftags )
        nChip := ht_strWidth( "[" + ::Ftags[ i ] + " " + cClose + "]" )
        IF Len( a ) > 0 .AND. nCol + nChip + 1 >= nWidth
            EXIT
        ENDIF
        nClose := nCol + 1 + ht_strWidth( ::Ftags[ i ] ) + 1
        AAdd( a, { i, nCol, nChip, nClose } )
        nCol += nChip + 1
    NEXT
    ::FnTextAt := nCol

RETURN a

/** Column where the typed text starts. chipBoxes() leaves it in FnTextAt. */
METHOD FUNCTION textCol( nWidth ) CLASS HTTagInput
    ::chipBoxes( nWidth )
RETURN ::FnTextAt

METHOD PROCEDURE scrollToFit( nWidth ) CLASS HTTagInput

    LOCAL nGuard := 0

    IF ::Ffirst < 1
        ::Ffirst := 1
    ENDIF
    DO WHILE ::Ffirst < Len( ::Ftags ) .AND. nGuard < Len( ::Ftags )
        ::chipBoxes( nWidth )
        IF ::FnTextAt < nWidth - 1
            EXIT
        ENDIF
        ::Ffirst++
        nGuard++
    ENDDO

RETURN

METHOD PROCEDURE repaintSelf() CLASS HTTagInput

    LOCAL parent := ::parent()

    IF parent != NIL .AND. parent:isDerivedFrom( "HTWidget" )
        parent:repaintChild( self )
    ENDIF

RETURN
