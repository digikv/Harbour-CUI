/** @class HTSplitter
 * Two or more panes with a one-cell bar the user can drag.
 * HT_SPLIT_VERTICAL: bar is │ and the panes sit side by side.
 * HT_SPLIT_HORIZONTAL: bar is ─ and the panes are stacked.
 * Click the bar (it is the gap between the panes) and drag; mouseDragWanted()
 * holds the mouse until the button comes up. With the bar focused, Left/Right
 * or Up/Down move it by one cell. PgUp / PgDn pick another bar when there
 * are more than two panes.
 * @extends HTWidget
 *
 *   addWidget( oPane )  setSizes( { nCells, ... } )  sizes()
 *   moveHandle( nHandle, nDelta )  handlePos( n )  setMinimum( nCells )
 */

#include "hbtui.ch"
#include "inkey.ch"

CLASS HTSplitter FROM HTWidget

PROTECTED:

    DATA Fpanes     INIT {}
    DATA Fweights   INIT {}    /* requested sizes; 0 = share equally */
    DATA Fsizes     INIT {}    /* cells actually given to each pane */
    DATA Fhandles   INIT {}    /* coordinate of each bar */
    DATA Forient    INIT HT_SPLIT_VERTICAL
    DATA FminPane   INIT 1
    DATA Fdrag      INIT 0     /* bar being dragged, 0 = none */
    DATA Factive    INIT 1

    METHOD layoutPanes()
    METHOD hitHandle( nPos )
    METHOD repaintSelf()

PUBLIC:

    CONSTRUCTOR new( ... )

    METHOD paintEvent( paintEvent )
    METHOD keyEvent( keyEvent )
    METHOD mouseEvent( eventMouse )
    METHOD mouseDragWanted()    INLINE ::Fdrag > 0
    METHOD setGeometry( nX, nY, nW, nH )
    METHOD resize( ... )
    METHOD resizeEvent( event )

    METHOD addWidget( oWidget )
    METHOD setSizes( aSizes )
    METHOD sizes()              INLINE AClone( ::Fsizes )
    METHOD setMinimum( nCells )
    METHOD moveHandle( nHandle, nDelta )
    METHOD handlePos( n )
    METHOD widget( n )
    METHOD widgetCount()        INLINE Len( ::Fpanes )
    METHOD orientation()        INLINE ::Forient
    METHOD setOrientation( n )
    METHOD activeHandle()       INLINE ::Factive

ENDCLASS

/** Creates a splitter.
 * @param ... (nOrientation) or (nOrientation, parent) or (parent)
 */
METHOD new( ... ) CLASS HTSplitter

    LOCAL nOrient := HT_SPLIT_VERTICAL
    LOCAL parent := NIL
    LOCAL x

    IF pCount() >= 1
        x := hb_pValue( 1 )
        IF HB_ISNUMERIC( x )
            nOrient := IIF( x == HT_SPLIT_HORIZONTAL, HT_SPLIT_HORIZONTAL, HT_SPLIT_VERTICAL )
            IF pCount() >= 2 .AND. hb_isObject( hb_pValue( 2 ) )
                parent := hb_pValue( 2 )
            ENDIF
        ELSEIF hb_isObject( x )
            parent := x
        ENDIF
    ENDIF

    ::super:new( parent )
    ::Forient := nOrient
    ::setFocusPolicy( HT_FOCUS_CLICK )
    ::FisVisible := .T.
    ::Fheight := 12
    ::Fwidth := 40

RETURN self

/** Parents a pane and gives it an equal share. The pane should be visible
 * (widget constructors set that). */
METHOD FUNCTION addWidget( oWidget ) CLASS HTSplitter

    IF ! hb_isObject( oWidget ) .OR. ! oWidget:isDerivedFrom( "HTWidget" )
        RETURN .F.
    ENDIF
    IF AScan( ::Fpanes, {| e | e == oWidget } ) > 0
        RETURN .T.
    ENDIF
    IF oWidget:parent() != NIL .AND. !( oWidget:parent() == self )
        oWidget:parent():removeChild( oWidget )
    ENDIF
    IF oWidget:parent() == NIL
        oWidget:setParent( self )
    ENDIF
    AAdd( ::Fpanes, oWidget )
    AAdd( ::Fweights, 0 )
    ::layoutPanes()

RETURN .T.

/** Requested size of each pane, in cells along the bar's axis.
 * Values are scaled so the panes plus the bars fill the splitter.
 * A 0 keeps that pane on an equal share. */
METHOD FUNCTION setSizes( aSizes ) CLASS HTSplitter

    LOCAL i

    IF ! HB_ISARRAY( aSizes )
        RETURN .F.
    ENDIF
    FOR i := 1 TO Min( Len( aSizes ), Len( ::Fweights ) )
        IF HB_ISNUMERIC( aSizes[ i ] ) .AND. aSizes[ i ] >= 0
            ::Fweights[ i ] := Int( aSizes[ i ] )
        ENDIF
    NEXT
    ::layoutPanes()

RETURN .T.

/** Smallest pane, in cells. The bar will not drag a pane below this. */
METHOD PROCEDURE setMinimum( nCells ) CLASS HTSplitter
    IF HB_ISNUMERIC( nCells ) .AND. nCells >= 1
        ::FminPane := Int( nCells )
        ::layoutPanes()
    ENDIF
RETURN

/** Moves bar nHandle by nDelta cells. Positive grows the pane before the bar. */
METHOD FUNCTION moveHandle( nHandle, nDelta ) CLASS HTSplitter

    LOCAL nGrow, nShrink

    IF ! HB_ISNUMERIC( nHandle ) .OR. ! HB_ISNUMERIC( nDelta )
        RETURN .F.
    ENDIF
    IF nHandle < 1 .OR. nHandle >= Len( ::Fsizes ) .OR. nDelta == 0
        RETURN .F.
    ENDIF

    nGrow := ::Fsizes[ nHandle + 1 ] - ::FminPane
    nShrink := ::Fsizes[ nHandle ] - ::FminPane
    nDelta := Max( -nShrink, Min( nGrow, Int( nDelta ) ) )
    IF nDelta == 0
        RETURN .F.
    ENDIF

    ::Fsizes[ nHandle ] += nDelta
    ::Fsizes[ nHandle + 1 ] -= nDelta
    ::Fweights := AClone( ::Fsizes )
    ::Factive := nHandle
    ::layoutPanes()

RETURN .T.

/** Cell (column or row) of bar n, or -1. */
METHOD FUNCTION handlePos( n ) CLASS HTSplitter
    IF HB_ISNUMERIC( n ) .AND. n >= 1 .AND. n <= Len( ::Fhandles )
        RETURN ::Fhandles[ n ]
    ENDIF
RETURN -1

/** Pane n, or NIL. */
METHOD FUNCTION widget( n ) CLASS HTSplitter
    IF HB_ISNUMERIC( n ) .AND. n >= 1 .AND. n <= Len( ::Fpanes )
        RETURN ::Fpanes[ n ]
    ENDIF
RETURN NIL

METHOD PROCEDURE setOrientation( n ) CLASS HTSplitter
    ::Forient := IIF( n == HT_SPLIT_HORIZONTAL, HT_SPLIT_HORIZONTAL, HT_SPLIT_VERTICAL )
    ::layoutPanes()
RETURN

METHOD PROCEDURE setGeometry( nX, nY, nW, nH ) CLASS HTSplitter
    ::super:setGeometry( nX, nY, nW, nH )
    ::layoutPanes()
RETURN

METHOD PROCEDURE resize( ... ) CLASS HTSplitter
    ::super:resize( ... )
    ::layoutPanes()
RETURN

METHOD PROCEDURE resizeEvent( event ) CLASS HTSplitter
    ::super:resizeEvent( event )
    ::layoutPanes()
RETURN

/** Paints the panes, then the bars. The grip sits in the middle of each bar. */
METHOD PROCEDURE paintEvent( paintEvent ) CLASS HTSplitter

    LOCAL i, nPos, nLen, nMid, cBar, cGrip, cLine, cColor
    LOCAL lVert := ::Forient == HT_SPLIT_VERTICAL

    ::super:paintEvent( paintEvent )

    cBar := IIF( lVert, ht_glyph( 0x2502, "|" ), ht_glyph( 0x2500, "-" ) )
    cGrip := IIF( lVert, ht_glyph( 0x2551, "#" ), ht_glyph( 0x2550, "=" ) )
    nLen := IIF( lVert, ::Fheight, ::Fwidth )

    FOR i := 1 TO Len( ::Fhandles )
        nPos := ::Fhandles[ i ]
        cColor := HTTheme():getColor( HT_CLR_SCROLLBAR )
        IF i == ::Fdrag .OR. ( ::hasFocus() .AND. i == ::Factive )
            cColor := HTTheme():getColor( HT_CLR_BUTTON_FOCUSED )
        ENDIF
        IF nLen <= 0
            LOOP
        ENDIF
        nMid := Int( nLen / 2 )
        IF lVert
            ht_splitColumn( nPos, cBar, cGrip, nMid, nLen, cColor )
        ELSE
            cLine := Replicate( cBar, nMid ) + cGrip + Replicate( cBar, Max( 0, nLen - nMid - 1 ) )
            DispOutAt( nPos, 0, ht_cellLeft( cLine, nLen ), cColor )
        ENDIF
    NEXT

RETURN

METHOD PROCEDURE keyEvent( keyEvent ) CLASS HTSplitter

    LOCAL nKey := keyEvent:key
    LOCAL lVert := ::Forient == HT_SPLIT_VERTICAL
    LOCAL lUsed := .T.

    DO CASE
    CASE lVert .AND. nKey == K_LEFT
        ::moveHandle( ::Factive, -1 )
    CASE lVert .AND. nKey == K_RIGHT
        ::moveHandle( ::Factive, 1 )
    CASE ! lVert .AND. nKey == K_UP
        ::moveHandle( ::Factive, -1 )
    CASE ! lVert .AND. nKey == K_DOWN
        ::moveHandle( ::Factive, 1 )
    CASE nKey == K_PGUP
        IF ::Factive > 1
            ::Factive--
        ENDIF
    CASE nKey == K_PGDN
        IF ::Factive < Len( ::Fpanes ) - 1
            ::Factive++
        ENDIF
    OTHERWISE
        lUsed := .F.
    ENDCASE

    IF lUsed
        keyEvent:accept()
        ::repaintSelf()
    ENDIF

RETURN

/** A button-down on a bar starts a drag. Later moves follow the pointer. */
METHOD PROCEDURE mouseEvent( eventMouse ) CLASS HTSplitter

    LOCAL nPos, nOrigin, i, nNew, nDelta

    nPos := IIF( ::Forient == HT_SPLIT_VERTICAL, eventMouse:mouseCol, eventMouse:mouseRow )

    DO CASE
    CASE eventMouse:nKey == K_LBUTTONDOWN
        ::Fdrag := ::hitHandle( nPos )
        IF ::Fdrag > 0
            ::Factive := ::Fdrag
        ENDIF
    CASE eventMouse:nKey == K_MOUSEMOVE .AND. ::Fdrag > 0
        nOrigin := 0
        FOR i := 1 TO ::Fdrag - 1
            nOrigin += ::Fsizes[ i ] + 1
        NEXT
        nNew := nPos - nOrigin
        nDelta := nNew - ::Fsizes[ ::Fdrag ]
        ::moveHandle( ::Fdrag, nDelta )
        ::repaintSelf()
    CASE eventMouse:nKey == K_LBUTTONUP .OR. eventMouse:nKey == K_RBUTTONUP
        ::Fdrag := 0
        ::repaintSelf()
    ENDCASE

RETURN

METHOD PROCEDURE layoutPanes() CLASS HTSplitter

    LOCAL nSpan, nCross, nHandles, nAvail, a, i, nPos, o
    LOCAL lVert := ::Forient == HT_SPLIT_VERTICAL

    nSpan := IIF( lVert, ::Fwidth, ::Fheight )
    nCross := IIF( lVert, ::Fheight, ::Fwidth )
    nHandles := Max( 0, Len( ::Fpanes ) - 1 )
    nAvail := nSpan - nHandles
    ::Fhandles := {}

    IF Len( ::Fpanes ) == 0 .OR. nAvail <= 0 .OR. nCross <= 0
        RETURN
    ENDIF

    a := ht_splitFit( ht_splitWeights( ::Fweights ), nAvail, ::FminPane )
    ::Fsizes := a
    IF ::Factive > Len( ::Fpanes ) - 1
        ::Factive := Max( 1, Len( ::Fpanes ) - 1 )
    ENDIF

    nPos := 0
    FOR i := 1 TO Len( ::Fpanes )
        o := ::Fpanes[ i ]
        IF lVert
            o:setGeometry( nPos, 0, a[ i ], nCross )
        ELSE
            o:setGeometry( 0, nPos, nCross, a[ i ] )
        ENDIF
        nPos += a[ i ]
        IF i < Len( ::Fpanes )
            AAdd( ::Fhandles, nPos )
            nPos++
        ENDIF
    NEXT

RETURN

/** Bar index at this column (vertical) or row (horizontal), or 0. */
METHOD FUNCTION hitHandle( nPos ) CLASS HTSplitter

    LOCAL i

    FOR i := 1 TO Len( ::Fhandles )
        IF ::Fhandles[ i ] == nPos
            RETURN i
        ENDIF
    NEXT

RETURN 0

METHOD PROCEDURE repaintSelf() CLASS HTSplitter

    LOCAL parent := ::parent()

    IF parent != NIL .AND. parent:isDerivedFrom( "HTWidget" )
        parent:repaintChild( self )
    ENDIF

RETURN

/** Zeros become an equal share. Positive weights are kept. */
STATIC FUNCTION ht_splitWeights( aWant )

    LOCAL a := {}
    LOCAL n, nSet := 0, nSum := 0, nAvg

    FOR n := 1 TO Len( aWant )
        IF HB_ISNUMERIC( aWant[ n ] ) .AND. aWant[ n ] > 0
            nSet++
            nSum += Int( aWant[ n ] )
        ENDIF
    NEXT

    IF nSet == 0
        FOR n := 1 TO Len( aWant )
            AAdd( a, 1 )
        NEXT
        RETURN a
    ENDIF

    nAvg := Max( 1, Int( nSum / nSet ) )
    FOR n := 1 TO Len( aWant )
        IF HB_ISNUMERIC( aWant[ n ] ) .AND. aWant[ n ] > 0
            AAdd( a, Int( aWant[ n ] ) )
        ELSE
            AAdd( a, nAvg )
        ENDIF
    NEXT

RETURN a

/** Scales positive weights so they sum to nAvail, each at least nMin when it fits. */
STATIC FUNCTION ht_splitFit( aWant, nAvail, nMin )

    LOCAL a := {}
    LOCAL n, nCount, nSum, nUsed, nNeed, nGive, j

    nCount := Len( aWant )
    IF nCount == 0 .OR. nAvail <= 0
        RETURN a
    ENDIF

    nMin := Max( 1, nMin )
    IF nAvail < nCount * nMin
        nMin := Max( 1, Int( nAvail / nCount ) )
    ENDIF

    nSum := 0
    FOR n := 1 TO nCount
        AAdd( a, Max( 1, Int( aWant[ n ] ) ) )
        nSum += a[ n ]
    NEXT

    nUsed := 0
    FOR n := 1 TO nCount - 1
        a[ n ] := Max( nMin, Int( a[ n ] * nAvail / nSum ) )
        nUsed += a[ n ]
    NEXT
    a[ nCount ] := nAvail - nUsed

    FOR n := 1 TO nCount
        IF a[ n ] < nMin
            nNeed := nMin - a[ n ]
            FOR j := 1 TO nCount
                IF j != n .AND. a[ j ] > nMin
                    nGive := Min( nNeed, a[ j ] - nMin )
                    a[ j ] -= nGive
                    a[ n ] += nGive
                    nNeed -= nGive
                    IF nNeed <= 0
                        EXIT
                    ENDIF
                ENDIF
            NEXT
        ENDIF
    NEXT

    nUsed := 0
    FOR n := 1 TO nCount
        IF a[ n ] < 0
            a[ n ] := 0
        ENDIF
        nUsed += a[ n ]
    NEXT
    a[ nCount ] += nAvail - nUsed
    IF a[ nCount ] < 0
        a[ nCount ] := 0
    ENDIF

RETURN a

/** A vertical bar is one column, so the line of glyphs is painted downward. */
STATIC PROCEDURE ht_splitColumn( nCol, cBar, cGrip, nMid, nLen, cColor )

    LOCAL i

    FOR i := 0 TO nLen - 1
        DispOutAt( i, nCol, IIF( i == nMid, cGrip, cBar ), cColor )
    NEXT

RETURN
