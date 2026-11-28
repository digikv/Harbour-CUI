/** @class HTPropertyGrid
 * Two columns, name and value. The value editor follows the type:
 *   text, number, logical, list, color, file  (HT_PROP_*).
 * Space toggles a logical value and cycles a list. Left / Right cycle a
 * list or a color. Enter or a typed character edits text, number, color and
 * file; Enter commits, Esc cancels. F4 on a file runs onBrowse and stores
 * the path it returns. A group row (addGroup) is a heading the cursor skips.
 * @extends HTWidget
 *
 *   addProperty( cName, cLabel, cType, xValue, aChoices )
 *   value( cName )  setValue( cName, x )  setReadOnly( cName, l )
 *   onChanged {| cName, xValue |}  onBrowse {| cName, cPath | cNew }
 */

#include "hbtui.ch"
#include "inkey.ch"

CLASS HTPropertyGrid FROM HTWidget

PROTECTED:

    /* row: { cName, cLabel, cType, xValue, aChoices, lReadOnly } */
    DATA Frows      INIT {}
    DATA Fcurrent   INIT 0
    DATA Ftop       INIT 1
    DATA FnNameW    INIT 16
    DATA Fediting   INIT .F.
    DATA Fbuffer    INIT ""
    DATA FnCursor   INIT 1

    METHOD findRow( cName )
    METHOD rowType( n )
    METHOD isGroup( n )
    METHOD editable( n )
    METHOD moveRow( nDelta )
    METHOD firstValue()
    METHOD lastValue()
    METHOD reveal()
    METHOD pageRows()
    METHOD valueText( n )
    METHOD displayText( n )
    METHOD startEdit( cReplace )
    METHOD commitEdit( lMove )
    METHOD cancelEdit()
    METHOD cycle( nDelta )
    METHOD toggleLogical()
    METHOD browseFile()
    METHOD putValue( n, xNew )
    METHOD insertChar( c )
    METHOD repaintSelf()

PUBLIC:

    CONSTRUCTOR new( ... )

    METHOD paintEvent( paintEvent )
    METHOD keyEvent( keyEvent )
    METHOD mouseEvent( eventMouse )
    METHOD handleKey( nKey )
    METHOD cursorPos()

    METHOD addProperty( cName, cLabel, cType, xValue, aChoices )
    METHOD addGroup( cTitle )
    METHOD clear()
    METHOD count()             INLINE Len( ::Frows )
    METHOD value( cName )
    METHOD setValue( cName, xValue )
    METHOD setReadOnly( cName, l )
    METHOD label( n )
    METHOD type( n )
    METHOD currentIndex()      INLINE ::Fcurrent
    METHOD setCurrentIndex( n )
    METHOD setCurrent( cName )
    METHOD currentName()
    METHOD isEditing()         INLINE ::Fediting
    METHOD setNameWidth( n )

    PROPERTY onChanged READWRITE
    PROPERTY onBrowse READWRITE

ENDCLASS

/** Creates a property grid. Optional parent. */
METHOD new( ... ) CLASS HTPropertyGrid

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
    ::Fheight := 10
    ::Fwidth := 42

RETURN self

/** Adds a value row.
 * cType is text, number, logical, list, color or file.
 * aChoices is the list of values for HT_PROP_LIST.
 * @return 1-based row index
 */
METHOD FUNCTION addProperty( cName, cLabel, cType, xValue, aChoices ) CLASS HTPropertyGrid

    LOCAL a := {}

    IF ! HB_ISSTRING( cName )
        cName := ""
    ENDIF
    IF ! HB_ISSTRING( cLabel ) .OR. Len( cLabel ) == 0
        cLabel := cName
    ENDIF
    cType := ht_propType( cType )
    IF cType == HT_PROP_LIST .AND. HB_ISARRAY( aChoices )
        a := AClone( aChoices )
        IF HB_ISNUMERIC( xValue ) .AND. xValue >= 1 .AND. xValue <= Len( a )
            xValue := a[ xValue ]
        ENDIF
    ENDIF
    IF xValue == NIL
        xValue := ht_propEmpty( cType )
    ENDIF
    AAdd( ::Frows, { cName, cLabel, cType, xValue, a, .F. } )
    IF ::Fcurrent == 0
        ::Fcurrent := Len( ::Frows )
    ENDIF

RETURN Len( ::Frows )

/** A heading the cursor skips. */
METHOD FUNCTION addGroup( cTitle ) CLASS HTPropertyGrid
    IF ! HB_ISSTRING( cTitle )
        cTitle := ""
    ENDIF
    AAdd( ::Frows, { "", cTitle, HT_PROP_GROUP, NIL, {}, .T. } )
RETURN Len( ::Frows )

METHOD PROCEDURE clear() CLASS HTPropertyGrid
    ::Frows := {}
    ::Fcurrent := 0
    ::Ftop := 1
    ::cancelEdit()
RETURN

/** Current value of the property named cName, or NIL. */
METHOD FUNCTION value( cName ) CLASS HTPropertyGrid

    LOCAL n := ::findRow( cName )

    IF n == 0
        RETURN NIL
    ENDIF

RETURN ::Frows[ n ][ 4 ]

/** Stores a value without starting the editor.
 * @return .F. when the name is unknown
 */
METHOD FUNCTION setValue( cName, xValue ) CLASS HTPropertyGrid

    LOCAL n := ::findRow( cName )

    IF n == 0
        RETURN .F.
    ENDIF
    ::putValue( n, xValue )
    IF ::Fediting .AND. n == ::Fcurrent
        ::Fbuffer := ::valueText( n )
        ::FnCursor := Len( ::Fbuffer ) + 1
    ENDIF

RETURN .T.

METHOD FUNCTION setReadOnly( cName, l ) CLASS HTPropertyGrid

    LOCAL n := ::findRow( cName )

    IF n == 0
        RETURN .F.
    ENDIF
    ::Frows[ n ][ 6 ] := HB_ISLOGICAL( l ) .AND. l
    IF ::Fediting .AND. n == ::Fcurrent .AND. ::Frows[ n ][ 6 ]
        ::cancelEdit()
    ENDIF

RETURN .T.

METHOD FUNCTION label( n ) CLASS HTPropertyGrid
    IF n >= 1 .AND. n <= Len( ::Frows )
        RETURN ::Frows[ n ][ 2 ]
    ENDIF
RETURN ""

METHOD FUNCTION type( n ) CLASS HTPropertyGrid
    IF n >= 1 .AND. n <= Len( ::Frows )
        RETURN ::Frows[ n ][ 3 ]
    ENDIF
RETURN ""

METHOD PROCEDURE setCurrentIndex( n ) CLASS HTPropertyGrid
    IF n >= 1 .AND. n <= Len( ::Frows ) .AND. ! ::isGroup( n )
        IF ::Fediting
            ::commitEdit( .F. )
        ENDIF
        ::Fcurrent := n
        ::reveal()
    ENDIF
RETURN

/** Moves the cursor to the property named cName. */
METHOD FUNCTION setCurrent( cName ) CLASS HTPropertyGrid

    LOCAL n := ::findRow( cName )

    IF n == 0
        RETURN .F.
    ENDIF
    ::setCurrentIndex( n )

RETURN .T.

METHOD FUNCTION currentName() CLASS HTPropertyGrid
    IF ::Fcurrent >= 1 .AND. ::Fcurrent <= Len( ::Frows )
        RETURN ::Frows[ ::Fcurrent ][ 1 ]
    ENDIF
RETURN ""

METHOD PROCEDURE setNameWidth( n ) CLASS HTPropertyGrid
    IF HB_ISNUMERIC( n ) .AND. n >= 6
        ::FnNameW := Int( n )
    ENDIF
RETURN

METHOD PROCEDURE paintEvent( paintEvent ) CLASS HTPropertyGrid

    LOCAL nWidth := MaxCol() + 1
    LOCAL nHeight := MaxRow() + 1
    LOCAL nName := Min( ::FnNameW, Max( 8, Int( nWidth / 2 ) ) )
    LOCAL nRow, nIdx, cLeft, cRight, cColor, cSel, cName, cVal
    LOCAL cNrm := HTTheme():getColor( HT_CLR_BROWSE_NORMAL )
    LOCAL cHead := HTTheme():getColor( HT_CLR_TITLEBAR )
    LOCAL cEdit := HTTheme():getColor( HT_CLR_GET_FOCUSED )

    HB_SYMBOL_UNUSED( paintEvent )
    ::reveal()
    cSel := HTTheme():getColor( IIF( ::hasFocus(), HT_CLR_LIST_SELECTED, HT_CLR_LIST_SEL_UNFOC ) )

    FOR nRow := 0 TO nHeight - 1
        nIdx := ::Ftop + nRow
        IF nIdx >= 1 .AND. nIdx <= Len( ::Frows )
            IF ::isGroup( nIdx )
                DispOutAt( nRow, 0, ht_cellPadR( " " + ::Frows[ nIdx ][ 2 ], nWidth ), cHead )
            ELSE
                cName := ht_cellPadR( ::Frows[ nIdx ][ 2 ], nName )
                IF ::Fediting .AND. nIdx == ::Fcurrent
                    cRight := ::Fbuffer
                ELSE
                    cRight := ::displayText( nIdx )
                ENDIF
                cVal := ht_cellPadR( cRight, Max( 0, nWidth - nName - 1 ) )
                cLeft := cName + " " + cVal
                IF nIdx == ::Fcurrent .AND. ::Fediting
                    DispOutAt( nRow, 0, ht_cellPadR( cName, nName ), cSel )
                    DispOutAt( nRow, nName, " ", cSel )
                    DispOutAt( nRow, nName + 1, cVal, cEdit )
                ELSE
                    cColor := IIF( nIdx == ::Fcurrent, cSel, cNrm )
                    DispOutAt( nRow, 0, ht_cellPadR( cLeft, nWidth ), cColor )
                ENDIF
            ENDIF
        ELSE
            DispOutAt( nRow, 0, Space( nWidth ), cNrm )
        ENDIF
    NEXT

RETURN

METHOD FUNCTION cursorPos() CLASS HTPropertyGrid

    LOCAL nName, nRow

    IF ! ::Fediting .OR. ::Fcurrent < 1
        RETURN NIL
    ENDIF
    nName := Min( ::FnNameW, Max( 8, Int( Max( ::Fwidth, 16 ) / 2 ) ) )
    nRow := ::Fcurrent - ::Ftop

RETURN { nRow, nName + 1 + ht_cellCol( ::Fbuffer, 1, ::FnCursor ) }

METHOD PROCEDURE keyEvent( keyEvent ) CLASS HTPropertyGrid
    IF ::handleKey( keyEvent:key )
        keyEvent:accept()
        ::repaintSelf()
    ENDIF
RETURN

/** Keys. .T. when the grid uses the key. */
METHOD FUNCTION handleKey( nKey ) CLASS HTPropertyGrid

    LOCAL c, cType

    IF ::Fcurrent < 1 .OR. ::Fcurrent > Len( ::Frows )
        RETURN .F.
    ENDIF
    cType := ::rowType( ::Fcurrent )

    IF ::Fediting
        c := ht_keyText( nKey )
        DO CASE
        CASE nKey == K_ESC
            ::cancelEdit()
        CASE nKey == K_ENTER
            ::commitEdit( .T. )
        CASE nKey == K_UP
            ::commitEdit( .F. )
            ::moveRow( -1 )
        CASE nKey == K_DOWN
            ::commitEdit( .F. )
            ::moveRow( 1 )
        CASE nKey == K_LEFT
            IF ::FnCursor > 1
                ::FnCursor--
            ENDIF
        CASE nKey == K_RIGHT
            IF ::FnCursor <= Len( ::Fbuffer )
                ::FnCursor++
            ENDIF
        CASE nKey == K_HOME
            ::FnCursor := 1
        CASE nKey == K_END
            ::FnCursor := Len( ::Fbuffer ) + 1
        CASE nKey == K_BS
            IF ::FnCursor > 1
                ::Fbuffer := Left( ::Fbuffer, ::FnCursor - 2 ) + SubStr( ::Fbuffer, ::FnCursor )
                ::FnCursor--
            ENDIF
        CASE nKey == K_DEL
            IF ::FnCursor <= Len( ::Fbuffer )
                ::Fbuffer := Left( ::Fbuffer, ::FnCursor - 1 ) + SubStr( ::Fbuffer, ::FnCursor + 1 )
            ENDIF
        CASE Len( c ) >= 1
            ::insertChar( c )
        OTHERWISE
            RETURN .F.
        ENDCASE
        RETURN .T.
    ENDIF

    c := ht_keyText( nKey )
    DO CASE
    CASE nKey == K_UP
        ::moveRow( -1 )
    CASE nKey == K_DOWN
        ::moveRow( 1 )
    CASE nKey == K_PGUP
        ::moveRow( -::pageRows() )
    CASE nKey == K_PGDN
        ::moveRow( ::pageRows() )
    CASE nKey == K_HOME
        ::setCurrentIndex( ::firstValue() )
    CASE nKey == K_END
        ::setCurrentIndex( ::lastValue() )
    CASE nKey == K_SPACE .AND. cType == HT_PROP_LOGICAL
        ::toggleLogical()
    CASE ( nKey == K_SPACE .OR. nKey == K_RIGHT ) .AND. cType == HT_PROP_LIST
        ::cycle( 1 )
    CASE nKey == K_LEFT .AND. cType == HT_PROP_LIST
        ::cycle( -1 )
    CASE ( nKey == K_LEFT .OR. nKey == K_RIGHT ) .AND. cType == HT_PROP_COLOR
        ::cycle( IIF( nKey == K_LEFT, -1, 1 ) )
    CASE nKey == K_F4 .OR. nKey == K_ALT_DOWN
        IF cType == HT_PROP_FILE
            ::browseFile()
        ELSEIF ::editable( ::Fcurrent )
            ::startEdit( NIL )
        ELSE
            RETURN .F.
        ENDIF
    CASE nKey == K_ENTER
        IF cType == HT_PROP_LOGICAL
            ::toggleLogical()
        ELSEIF cType == HT_PROP_LIST
            ::cycle( 1 )
        ELSEIF ::editable( ::Fcurrent )
            ::startEdit( NIL )
        ELSE
            RETURN .F.
        ENDIF
    CASE Len( c ) >= 1 .AND. c != " "
        IF ::editable( ::Fcurrent ) .AND. ( cType == HT_PROP_TEXT .OR. cType == HT_PROP_NUMBER .OR. ;
           cType == HT_PROP_COLOR .OR. cType == HT_PROP_FILE )
            ::startEdit( "" )
            ::insertChar( c )
        ELSE
            RETURN .F.
        ENDIF
    OTHERWISE
        RETURN .F.
    ENDCASE

RETURN .T.

METHOD PROCEDURE mouseEvent( eventMouse ) CLASS HTPropertyGrid

    LOCAL nIdx

    IF eventMouse:nKey == K_LBUTTONDOWN .AND. eventMouse:mouseRow >= 0
        nIdx := ::Ftop + eventMouse:mouseRow
        IF nIdx >= 1 .AND. nIdx <= Len( ::Frows ) .AND. ! ::isGroup( nIdx )
            ::setCurrentIndex( nIdx )
            ::repaintSelf()
        ENDIF
    ELSEIF eventMouse:nKey == K_MWFORWARD
        ::moveRow( -1 )
        ::repaintSelf()
    ELSEIF eventMouse:nKey == K_MWBACKWARD
        ::moveRow( 1 )
        ::repaintSelf()
    ENDIF

RETURN

METHOD FUNCTION findRow( cName ) CLASS HTPropertyGrid

    LOCAL i

    IF ! HB_ISSTRING( cName )
        RETURN 0
    ENDIF
    FOR i := 1 TO Len( ::Frows )
        IF ! ::isGroup( i ) .AND. Lower( ::Frows[ i ][ 1 ] ) == Lower( cName )
            RETURN i
        ENDIF
    NEXT

RETURN 0

METHOD FUNCTION rowType( n ) CLASS HTPropertyGrid
    IF n >= 1 .AND. n <= Len( ::Frows )
        RETURN ::Frows[ n ][ 3 ]
    ENDIF
RETURN ""

METHOD FUNCTION isGroup( n ) CLASS HTPropertyGrid
RETURN ::rowType( n ) == HT_PROP_GROUP

METHOD FUNCTION editable( n ) CLASS HTPropertyGrid
    IF n < 1 .OR. n > Len( ::Frows ) .OR. ::isGroup( n ) .OR. ::Frows[ n ][ 6 ]
        RETURN .F.
    ENDIF
RETURN ::rowType( n ) == HT_PROP_TEXT .OR. ::rowType( n ) == HT_PROP_NUMBER .OR. ;
       ::rowType( n ) == HT_PROP_COLOR .OR. ::rowType( n ) == HT_PROP_FILE

METHOD PROCEDURE moveRow( nDelta ) CLASS HTPropertyGrid

    LOCAL n, nGuard

    IF Len( ::Frows ) == 0 .OR. nDelta == 0
        RETURN
    ENDIF
    n := ::Fcurrent
    nGuard := 0
    DO WHILE nGuard < Len( ::Frows )
        n += IIF( nDelta > 0, 1, -1 )
        nGuard++
        IF n < 1 .OR. n > Len( ::Frows )
            RETURN
        ENDIF
        IF ! ::isGroup( n )
            ::Fcurrent := n
            ::reveal()
            RETURN
        ENDIF
    ENDDO

RETURN

METHOD FUNCTION firstValue() CLASS HTPropertyGrid

    LOCAL i

    FOR i := 1 TO Len( ::Frows )
        IF ! ::isGroup( i )
            RETURN i
        ENDIF
    NEXT

RETURN 0

METHOD FUNCTION lastValue() CLASS HTPropertyGrid

    LOCAL i

    FOR i := Len( ::Frows ) TO 1 STEP -1
        IF ! ::isGroup( i )
            RETURN i
        ENDIF
    NEXT

RETURN 0

METHOD PROCEDURE reveal() CLASS HTPropertyGrid

    LOCAL nPage := ::pageRows()

    IF ::Fcurrent < 1
        RETURN
    ENDIF
    IF ::Fcurrent < ::Ftop
        ::Ftop := ::Fcurrent
    ENDIF
    IF ::Fcurrent >= ::Ftop + nPage
        ::Ftop := ::Fcurrent - nPage + 1
    ENDIF
    IF ::Ftop < 1
        ::Ftop := 1
    ENDIF

RETURN

METHOD FUNCTION pageRows() CLASS HTPropertyGrid
    IF ::Fheight > 0
        RETURN ::Fheight
    ENDIF
RETURN Max( 1, MaxRow() + 1 )

METHOD FUNCTION valueText( n ) CLASS HTPropertyGrid

    LOCAL x := ::Frows[ n ][ 4 ]

    IF HB_ISSTRING( x )
        RETURN x
    ENDIF
    IF HB_ISNUMERIC( x )
        IF x == Int( x )
            RETURN hb_ntos( x )
        ENDIF
        RETURN LTrim( Str( x ) )
    ENDIF
    IF HB_ISLOGICAL( x )
        RETURN IIF( x, ".T.", ".F." )
    ENDIF

RETURN ""

METHOD FUNCTION displayText( n ) CLASS HTPropertyGrid

    LOCAL cType := ::rowType( n )
    LOCAL x := ::Frows[ n ][ 4 ]
    LOCAL cMark := ht_glyph( 0x221A, "x" )

    DO CASE
    CASE cType == HT_PROP_LOGICAL
        RETURN IIF( HB_ISLOGICAL( x ) .AND. x, "[" + cMark + "]", "[ ]" )
    CASE cType == HT_PROP_LIST
        RETURN ::valueText( n ) + " " + ht_glyph( 0x25BC, "v" )
    CASE cType == HT_PROP_FILE
        RETURN ::valueText( n )
    ENDCASE

RETURN ::valueText( n )

METHOD PROCEDURE startEdit( cReplace ) CLASS HTPropertyGrid

    IF ! ::editable( ::Fcurrent )
        RETURN
    ENDIF
    ::Fediting := .T.
    IF cReplace == NIL
        ::Fbuffer := ::valueText( ::Fcurrent )
    ELSE
        ::Fbuffer := cReplace
    ENDIF
    ::FnCursor := Len( ::Fbuffer ) + 1

RETURN

METHOD PROCEDURE commitEdit( lMove ) CLASS HTPropertyGrid

    LOCAL cType, xNew

    IF ! ::Fediting
        RETURN
    ENDIF
    cType := ::rowType( ::Fcurrent )
    IF cType == HT_PROP_NUMBER
        xNew := Val( StrTran( ::Fbuffer, ",", "." ) )
    ELSE
        xNew := ::Fbuffer
    ENDIF
    ::Fediting := .F.
    ::Fbuffer := ""
    ::putValue( ::Fcurrent, xNew )
    IF lMove
        ::moveRow( 1 )
    ENDIF

RETURN

METHOD PROCEDURE cancelEdit() CLASS HTPropertyGrid
    ::Fediting := .F.
    ::Fbuffer := ""
    ::FnCursor := 1
RETURN

METHOD PROCEDURE cycle( nDelta ) CLASS HTPropertyGrid

    LOCAL a, n, x

    IF ::rowType( ::Fcurrent ) == HT_PROP_COLOR
        a := { "W/N", "N/W", "W+/B", "B/W", "G+/N", "R+/N", "GR+/N", "N+/W" }
    ELSE
        a := ::Frows[ ::Fcurrent ][ 5 ]
    ENDIF
    IF ! HB_ISARRAY( a ) .OR. Len( a ) == 0
        RETURN
    ENDIF
    x := ::Frows[ ::Fcurrent ][ 4 ]
    n := 0
    IF HB_ISSTRING( x )
        n := hb_AScan( a, x,,, .T. )
    ENDIF
    IF n == 0
        n := IIF( nDelta < 0, Len( a ), 1 )
    ELSE
        n += nDelta
        IF n > Len( a )
            n := 1
        ELSEIF n < 1
            n := Len( a )
        ENDIF
    ENDIF
    ::putValue( ::Fcurrent, a[ n ] )

RETURN

METHOD PROCEDURE toggleLogical() CLASS HTPropertyGrid

    LOCAL x

    IF ::rowType( ::Fcurrent ) != HT_PROP_LOGICAL .OR. ::Frows[ ::Fcurrent ][ 6 ]
        RETURN
    ENDIF
    x := ::Frows[ ::Fcurrent ][ 4 ]
    ::putValue( ::Fcurrent, !( HB_ISLOGICAL( x ) .AND. x ) )

RETURN

METHOD PROCEDURE browseFile() CLASS HTPropertyGrid

    LOCAL xNew

    IF ::FonBrowse == NIL .OR. ::rowType( ::Fcurrent ) != HT_PROP_FILE
        RETURN
    ENDIF
    xNew := Eval( ::FonBrowse, ::currentName(), ::valueText( ::Fcurrent ) )
    IF HB_ISSTRING( xNew )
        ::putValue( ::Fcurrent, xNew )
    ENDIF

RETURN

METHOD PROCEDURE putValue( n, xNew ) CLASS HTPropertyGrid

    LOCAL xOld

    IF n < 1 .OR. n > Len( ::Frows ) .OR. ::isGroup( n )
        RETURN
    ENDIF
    xOld := ::Frows[ n ][ 4 ]
    IF ht_propSame( xOld, xNew )
        RETURN
    ENDIF
    ::Frows[ n ][ 4 ] := xNew
    IF ::FonChanged != NIL
        Eval( ::FonChanged, ::Frows[ n ][ 1 ], xNew )
    ENDIF

RETURN

METHOD PROCEDURE insertChar( c ) CLASS HTPropertyGrid

    IF ::rowType( ::Fcurrent ) == HT_PROP_NUMBER .AND. ! ht_propNumChar( c, ::Fbuffer, ::FnCursor )
        RETURN
    ENDIF
    ::Fbuffer := Left( ::Fbuffer, ::FnCursor - 1 ) + c + SubStr( ::Fbuffer, ::FnCursor )
    ::FnCursor += Len( c )

RETURN

METHOD PROCEDURE repaintSelf() CLASS HTPropertyGrid

    LOCAL parent := ::parent()

    IF parent != NIL .AND. parent:isDerivedFrom( "HTWidget" )
        parent:repaintChild( self )
    ENDIF

RETURN

STATIC FUNCTION ht_propType( cType )

    IF ! HB_ISSTRING( cType )
        RETURN HT_PROP_TEXT
    ENDIF
    cType := Lower( AllTrim( cType ) )
    IF cType == HT_PROP_NUMBER .OR. cType == HT_PROP_LOGICAL .OR. cType == HT_PROP_LIST .OR. ;
       cType == HT_PROP_COLOR .OR. cType == HT_PROP_FILE .OR. cType == HT_PROP_GROUP
        RETURN cType
    ENDIF

RETURN HT_PROP_TEXT

STATIC FUNCTION ht_propEmpty( cType )

    DO CASE
    CASE cType == HT_PROP_NUMBER
        RETURN 0
    CASE cType == HT_PROP_LOGICAL
        RETURN .F.
    ENDCASE

RETURN ""

STATIC FUNCTION ht_propSame( xOld, xNew )

    IF ValType( xOld ) != ValType( xNew )
        RETURN .F.
    ENDIF
    IF HB_ISSTRING( xOld )
        RETURN xOld == xNew
    ENDIF

RETURN xOld == xNew

STATIC FUNCTION ht_propNumChar( c, cBuf, nPos )

    LOCAL ch

    IF Len( c ) != 1
        RETURN .F.
    ENDIF
    ch := SubStr( c, 1, 1 )
    IF ch >= "0" .AND. ch <= "9"
        RETURN .T.
    ENDIF
    IF ( ch == "." .OR. ch == "," ) .AND. !( "." $ cBuf ) .AND. !( "," $ cBuf )
        RETURN .T.
    ENDIF
    IF ch == "-" .AND. nPos == 1 .AND. !( "-" $ cBuf )
        RETURN .T.
    ENDIF

RETURN .F.
