/** @class HTTreeView
 * Hierarchical list: accounts, folders, categories.
 * Guides are ├── └── │ when the codepage has them, ASCII otherwise.
 * A node with children shows [+] / [-]. Right or + expands, Left or - collapses,
 * Space / Enter / a click on the mark toggles, Enter also runs onActivated.
 * Type a letter to jump to the next visible node with that initial.
 * @extends HTWidget
 *
 *   addNode( cText, oParent, xUser ) -> HTTreeNode
 *   expand( oNode )  collapse( oNode )  toggle( oNode )  expandAll()
 *   currentNode()  currentIndex()  rowText( n )  rowCount()  count()
 *   onChanged {|oNode|}  onActivated {|oNode|}  onExpanded {|oNode, lOpen|}
 */

#include "hbtui.ch"
#include "inkey.ch"

CLASS HTTreeNode FROM HTBase

PROTECTED:

    DATA Fchildren   INIT {}
    DATA FparentNode INIT NIL
    DATA Fexpanded   INIT .F.

PUBLIC:

    CONSTRUCTOR new( cText )

    METHOD children()              INLINE ::Fchildren
    METHOD parentNode()            INLINE ::FparentNode
    METHOD setParentNode( o )      INLINE ::FparentNode := o
    METHOD isExpanded()            INLINE ::Fexpanded
    METHOD setExpanded( l )        INLINE ::Fexpanded := l
    METHOD hasChildren()           INLINE Len( ::Fchildren ) > 0
    METHOD addChildNode( oChild )
    METHOD setText( c )            INLINE ::Ftext := c

    PROPERTY text INIT ""
    PROPERTY userData READWRITE

ENDCLASS

METHOD new( cText ) CLASS HTTreeNode
    IF HB_ISSTRING( cText )
        ::Ftext := cText
    ENDIF
RETURN self

/** Attaches a child and remembers this node as its parent. */
METHOD FUNCTION addChildNode( oChild ) CLASS HTTreeNode
    IF hb_isObject( oChild )
        oChild:setParentNode( self )
        AAdd( ::Fchildren, oChild )
    ENDIF
RETURN oChild


CLASS HTTreeView FROM HTWidget

PROTECTED:

    DATA Froots    INIT {}
    DATA Frows     INIT {}     /* { oNode, cPrefix, nMarkCol } visible */
    DATA Fcurrent  INIT NIL
    DATA Ftop      INIT 1
    DATA Fpipe     INIT "|"
    DATA Ftee      INIT "+"
    DATA Felbow    INIT "\"
    DATA Fdash     INIT "-"

    METHOD rebuild()
    METHOD walk( oNode )
    METHOD prefixOf( oNode )
    METHOD isLastSibling( oNode )
    METHOD glyphs()
    METHOD indexOf( oNode )
    METHOD placeCurrent()
    METHOD reveal( nIndex )
    METHOD pageRows()
    METHOD moveBy( nDelta )
    METHOD firstLetter( cLetter )
    METHOD fireChanged()
    METHOD repaintSelf()

PUBLIC:

    CONSTRUCTOR new( ... )

    METHOD paintEvent( paintEvent )
    METHOD keyEvent( keyEvent )
    METHOD mouseEvent( eventMouse )
    METHOD handleKey( nKey )

    METHOD addNode( cText, oParent, xUser )
    METHOD removeNode( oNode )
    METHOD clear()
    METHOD expand( oNode )
    METHOD collapse( oNode )
    METHOD toggle( oNode )
    METHOD expandAll()
    METHOD findData( xUser )

    METHOD count()
    METHOD rowCount()        INLINE Len( ::Frows )
    METHOD currentNode()     INLINE ::Fcurrent
    METHOD currentIndex()
    METHOD currentText()
    METHOD setCurrentIndex( n )
    METHOD nodeAt( n )
    METHOD rowText( n )
    METHOD markColumn( n )

    PROPERTY onChanged READWRITE
    PROPERTY onActivated READWRITE
    PROPERTY onExpanded READWRITE

ENDCLASS

/** Creates a tree. Optional parent widget. */
METHOD new( ... ) CLASS HTTreeView

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
    ::Fwidth := 32
    ::glyphs()

RETURN self

/** Box-drawing characters, or ASCII where the codepage has no glyph. */
METHOD PROCEDURE glyphs() CLASS HTTreeView
    ::Fpipe  := ht_glyph( 0x2502, "|" )
    ::Ftee   := ht_glyph( 0x251C, "+" )
    ::Felbow := ht_glyph( 0x2514, "\" )
    ::Fdash  := ht_glyph( 0x2500, "-" )
RETURN

/** Adds a node under oParent (NIL = root). xUser is kept on the node.
 * @return HTTreeNode
 */
METHOD FUNCTION addNode( cText, oParent, xUser ) CLASS HTTreeView

    LOCAL oNode := HTTreeNode():new( cText )

    IF xUser != NIL
        oNode:userData := xUser
    ENDIF

    IF hb_isObject( oParent ) .AND. oParent:isDerivedFrom( "HTTreeNode" )
        oParent:addChildNode( oNode )
    ELSE
        AAdd( ::Froots, oNode )
    ENDIF

    IF ::Fcurrent == NIL
        ::Fcurrent := oNode
    ENDIF
    ::rebuild()

RETURN oNode

/** Drops a node and its children. The current row moves to the parent. */
METHOD PROCEDURE removeNode( oNode ) CLASS HTTreeView

    LOCAL oParent, a, n

    IF ! hb_isObject( oNode )
        RETURN
    ENDIF

    oParent := oNode:parentNode()
    IF oParent == NIL
        a := ::Froots
    ELSE
        a := oParent:children()
    ENDIF
    n := AScan( a, {| e | e == oNode } )
    IF n > 0
        hb_aDel( a, n, .T. )
    ENDIF
    oNode:setParentNode( NIL )

    IF ::Fcurrent == oNode .OR. ::indexOf( ::Fcurrent ) == 0
        ::Fcurrent := oParent
    ENDIF
    ::rebuild()

RETURN

/** Removes every node. */
METHOD PROCEDURE clear() CLASS HTTreeView
    ::Froots := {}
    ::Fcurrent := NIL
    ::Ftop := 1
    ::rebuild()
RETURN

/** Opens a node that has children. NIL opens the current one.
 * @return .T. when the node changed from closed to open
 */
METHOD FUNCTION expand( oNode ) CLASS HTTreeView

    IF oNode == NIL
        oNode := ::Fcurrent
    ENDIF
    IF ! hb_isObject( oNode ) .OR. ! oNode:hasChildren() .OR. oNode:isExpanded()
        RETURN .F.
    ENDIF

    oNode:setExpanded( .T. )
    ::rebuild()
    IF ::FonExpanded != NIL
        Eval( ::FonExpanded, oNode, .T. )
    ENDIF
    ::repaintSelf()

RETURN .T.

/** Closes a node. NIL closes the current one. */
METHOD FUNCTION collapse( oNode ) CLASS HTTreeView

    IF oNode == NIL
        oNode := ::Fcurrent
    ENDIF
    IF ! hb_isObject( oNode ) .OR. ! oNode:isExpanded()
        RETURN .F.
    ENDIF

    oNode:setExpanded( .F. )
    ::rebuild()
    IF ::FonExpanded != NIL
        Eval( ::FonExpanded, oNode, .F. )
    ENDIF
    ::repaintSelf()

RETURN .T.

/** Opens a closed node or closes an open one. */
METHOD FUNCTION toggle( oNode ) CLASS HTTreeView
    IF oNode == NIL
        oNode := ::Fcurrent
    ENDIF
    IF ! hb_isObject( oNode ) .OR. ! oNode:hasChildren()
        RETURN .F.
    ENDIF
    IF oNode:isExpanded()
        RETURN ::collapse( oNode )
    ENDIF
RETURN ::expand( oNode )

/** Opens every node that has children. */
METHOD PROCEDURE expandAll() CLASS HTTreeView
    ht_treeExpandAll( ::Froots )
    ::rebuild()
    ::repaintSelf()
RETURN

/** First node whose userData is xUser, or NIL. */
METHOD FUNCTION findData( xUser ) CLASS HTTreeView
RETURN ht_treeFind( ::Froots, xUser )

/** Every node, including those under a closed parent. */
METHOD FUNCTION count() CLASS HTTreeView
RETURN ht_treeCount( ::Froots )

METHOD FUNCTION currentIndex() CLASS HTTreeView
RETURN ::indexOf( ::Fcurrent )

METHOD FUNCTION currentText() CLASS HTTreeView
    IF hb_isObject( ::Fcurrent )
        RETURN ::Fcurrent:text
    ENDIF
RETURN ""

/** Moves the mark to a visible row. Does not run onChanged. */
METHOD PROCEDURE setCurrentIndex( n ) CLASS HTTreeView
    IF n >= 1 .AND. n <= Len( ::Frows )
        ::Fcurrent := ::Frows[ n ][ 1 ]
        ::reveal( n )
    ENDIF
RETURN

/** Node on visible row n, or NIL. */
METHOD FUNCTION nodeAt( n ) CLASS HTTreeView
    IF n >= 1 .AND. n <= Len( ::Frows )
        RETURN ::Frows[ n ][ 1 ]
    ENDIF
RETURN NIL

/** Guide, mark and text of visible row n, as painted. */
METHOD FUNCTION rowText( n ) CLASS HTTreeView
    IF n >= 1 .AND. n <= Len( ::Frows )
        RETURN ::Frows[ n ][ 2 ] + ::Frows[ n ][ 1 ]:text
    ENDIF
RETURN ""

/** Screen column where [+] / [-] starts on visible row n, or -1. */
METHOD FUNCTION markColumn( n ) CLASS HTTreeView
    IF n >= 1 .AND. n <= Len( ::Frows )
        RETURN ::Frows[ n ][ 3 ]
    ENDIF
RETURN -1

/** Draws the visible rows and a one-cell scrollbar when needed. */
METHOD PROCEDURE paintEvent( paintEvent ) CLASS HTTreeView

    LOCAL i, nRow, nIdx, cText, cColor, cScroll
    LOCAL nMaxRow := MaxRow()
    LOCAL nMaxCol := MaxCol()
    LOCAL nVisible := nMaxRow + 1
    LOCAL lFocused := ::hasFocus()
    LOCAL cSel := HTTheme():getColor( IIF( lFocused, HT_CLR_LIST_SELECTED, HT_CLR_LIST_SEL_UNFOC ) )
    LOCAL cNrm := HTTheme():getColor( HT_CLR_LIST_NORMAL )
    LOCAL nCount := Len( ::Frows )
    LOCAL nThumbPos := 0, nThumbSize := 0, nRange

    HB_SYMBOL_UNUSED( paintEvent )

    ::reveal( ::currentIndex() )

    IF nCount > nVisible
        nRange := nCount - nVisible
        nThumbSize := Max( 1, Int( nVisible * nVisible / nCount ) )
        nThumbPos := Int( ( ::Ftop - 1 ) * ( nVisible - nThumbSize ) / Max( 1, nRange ) )
        nThumbPos := Max( 0, Min( nThumbPos, nVisible - nThumbSize ) )
    ENDIF

    nRow := 0
    FOR i := ::Ftop TO Min( ::Ftop + nVisible - 1, nCount )
        cText := ht_cellPadR( ::Frows[ i ][ 2 ] + ::Frows[ i ][ 1 ]:text, Max( 0, nMaxCol ) )
        IF nCount > nVisible .AND. nMaxCol >= 0
            cScroll := IIF( nRow >= nThumbPos .AND. nRow < nThumbPos + nThumbSize, "#", "|" )
            cText += cScroll
        ELSEIF nMaxCol >= 0
            cText += " "
        ENDIF
        nIdx := ::indexOf( ::Fcurrent )
        cColor := IIF( i == nIdx, cSel, cNrm )
        DispOutAt( nRow, 0, cText, cColor )
        nRow++
    NEXT

    DO WHILE nRow <= nMaxRow
        DispOutAt( nRow, 0, Space( nMaxCol + 1 ), cNrm )
        nRow++
    ENDDO

RETURN

METHOD PROCEDURE keyEvent( keyEvent ) CLASS HTTreeView
    IF ::handleKey( keyEvent:key )
        keyEvent:accept()
        ::repaintSelf()
    ENDIF
RETURN

/** Keyboard. .T. when the key belongs to the tree. */
METHOD FUNCTION handleKey( nKey ) CLASS HTTreeView

    LOCAL o := ::Fcurrent
    LOCAL c

    IF Len( ::Frows ) == 0
        RETURN .F.
    ENDIF

    DO CASE
    CASE nKey == K_UP
        ::moveBy( -1 )
    CASE nKey == K_DOWN
        ::moveBy( 1 )
    CASE nKey == K_PGUP
        ::moveBy( -::pageRows() )
    CASE nKey == K_PGDN
        ::moveBy( ::pageRows() )
    CASE nKey == K_HOME
        ::moveBy( -Len( ::Frows ) )
    CASE nKey == K_END
        ::moveBy( Len( ::Frows ) )
    CASE nKey == K_RIGHT .OR. nKey == Asc( "+" )
        IF hb_isObject( o ) .AND. o:hasChildren() .AND. ! o:isExpanded()
            ::expand( o )
        ELSEIF nKey == K_RIGHT .AND. hb_isObject( o ) .AND. o:hasChildren()
            ::moveBy( 1 )
        ENDIF
    CASE nKey == K_LEFT .OR. nKey == Asc( "-" )
        IF hb_isObject( o ) .AND. o:isExpanded()
            ::collapse( o )
        ELSEIF nKey == K_LEFT .AND. hb_isObject( o ) .AND. o:parentNode() != NIL
            ::Fcurrent := o:parentNode()
            ::placeCurrent()
            ::fireChanged()
        ENDIF
    CASE nKey == Asc( "*" )
        IF hb_isObject( o )
            ht_treeExpandAll( { o } )
            ::rebuild()
        ENDIF
    CASE nKey == K_SPACE
        ::toggle( o )
    CASE nKey == K_ENTER
        IF hb_isObject( o ) .AND. o:hasChildren()
            ::toggle( o )
        ENDIF
        IF ::FonActivated != NIL .AND. hb_isObject( ::Fcurrent )
            Eval( ::FonActivated, ::Fcurrent )
        ENDIF
    OTHERWISE
        c := ht_keyText( nKey )
        IF Len( c ) == 1 .AND. c != " "
            ::firstLetter( Upper( c ) )
        ELSE
            RETURN .F.
        ENDIF
    ENDCASE

RETURN .T.

/** Click selects the row. A click on [+] / [-] toggles it.
 * Double-click on the text toggles a branch and runs onActivated. */
METHOD PROCEDURE mouseEvent( eventMouse ) CLASS HTTreeView

    LOCAL nIndex, nOld, o, lMark

    DO CASE
    CASE eventMouse:nKey == K_LBUTTONDOWN .OR. eventMouse:nKey == K_LDBLCLK
        IF eventMouse:mouseRow < 0
            RETURN
        ENDIF
        nIndex := ::Ftop + eventMouse:mouseRow
        IF nIndex < 1 .OR. nIndex > Len( ::Frows )
            RETURN
        ENDIF
        nOld := ::currentIndex()
        ::setCurrentIndex( nIndex )
        IF nIndex != nOld
            ::fireChanged()
        ENDIF
        o := ::nodeAt( nIndex )
        lMark := o:hasChildren() .AND. eventMouse:mouseCol >= ::Frows[ nIndex ][ 3 ] .AND. ;
                 eventMouse:mouseCol < ::Frows[ nIndex ][ 3 ] + 3
        IF eventMouse:nKey == K_LBUTTONDOWN
            IF lMark
                ::toggle( o )
            ENDIF
        ELSE
            IF o:hasChildren() .AND. ! lMark
                ::toggle( o )
            ENDIF
            IF ::FonActivated != NIL
                Eval( ::FonActivated, o )
            ENDIF
        ENDIF
        ::repaintSelf()
    CASE eventMouse:nKey == K_MWFORWARD
        ::moveBy( -1 )
        ::repaintSelf()
    CASE eventMouse:nKey == K_MWBACKWARD
        ::moveBy( 1 )
        ::repaintSelf()
    ENDCASE

RETURN

METHOD PROCEDURE rebuild() CLASS HTTreeView

    LOCAL o

    ::glyphs()
    ::Frows := {}
    FOR EACH o IN ::Froots
        ::walk( o )
    NEXT
    ::placeCurrent()

RETURN

METHOD PROCEDURE walk( oNode ) CLASS HTTreeView

    LOCAL aPre, oChild

    aPre := ::prefixOf( oNode )
    AAdd( ::Frows, { oNode, aPre[ 1 ], aPre[ 2 ] } )
    IF oNode:isExpanded()
        FOR EACH oChild IN oNode:children()
            ::walk( oChild )
        NEXT
    ENDIF

RETURN

/** @return { cPrefix, nMarkCol } prefix includes the mark and a trailing space */
METHOD FUNCTION prefixOf( oNode ) CLASS HTTreeView

    LOCAL aLast := {}
    LOCAL o := oNode
    LOCAL cGuide := ""
    LOCAL cBranch, cMark, i

    DO WHILE o != NIL
        AAdd( aLast, ::isLastSibling( o ) )
        o := o:parentNode()
    ENDDO

    FOR i := Len( aLast ) TO 2 STEP -1
        IF aLast[ i ]
            cGuide += "   "
        ELSE
            cGuide += ::Fpipe + "  "
        ENDIF
    NEXT

    cBranch := IIF( aLast[ 1 ], ::Felbow, ::Ftee ) + ::Fdash + ::Fdash
    IF oNode:hasChildren()
        cMark := IIF( oNode:isExpanded(), "[-]", "[+]" )
    ELSE
        cMark := "   "
    ENDIF

RETURN { cGuide + cBranch + cMark + " ", ht_strWidth( cGuide + cBranch ) }

METHOD FUNCTION isLastSibling( oNode ) CLASS HTTreeView

    LOCAL a

    IF oNode:parentNode() == NIL
        a := ::Froots
    ELSE
        a := oNode:parentNode():children()
    ENDIF

RETURN Len( a ) > 0 .AND. a[ Len( a ) ] == oNode

METHOD FUNCTION indexOf( oNode ) CLASS HTTreeView

    LOCAL i

    IF ! hb_isObject( oNode )
        RETURN 0
    ENDIF
    FOR i := 1 TO Len( ::Frows )
        IF ::Frows[ i ][ 1 ] == oNode
            RETURN i
        ENDIF
    NEXT

RETURN 0

/** If the current node is under a closed parent, select that parent. */
METHOD PROCEDURE placeCurrent() CLASS HTTreeView

    LOCAL o, n

    o := ::Fcurrent
    DO WHILE hb_isObject( o ) .AND. ::indexOf( o ) == 0
        o := o:parentNode()
    ENDDO
    IF ! hb_isObject( o ) .AND. Len( ::Frows ) > 0
        o := ::Frows[ 1 ][ 1 ]
    ENDIF
    ::Fcurrent := o
    n := ::indexOf( o )
    IF n > 0
        ::reveal( n )
    ELSEIF Len( ::Frows ) == 0
        ::Ftop := 1
    ENDIF

RETURN

METHOD PROCEDURE reveal( nIndex ) CLASS HTTreeView

    LOCAL nPage := ::pageRows()

    IF nIndex < 1
        RETURN
    ENDIF
    IF nIndex < ::Ftop
        ::Ftop := nIndex
    ENDIF
    IF nIndex >= ::Ftop + nPage
        ::Ftop := nIndex - nPage + 1
    ENDIF
    IF ::Ftop < 1
        ::Ftop := 1
    ENDIF

RETURN

METHOD FUNCTION pageRows() CLASS HTTreeView
    IF ::Fheight > 0
        RETURN ::Fheight
    ENDIF
RETURN Max( 1, MaxRow() + 1 )

METHOD PROCEDURE moveBy( nDelta ) CLASS HTTreeView

    LOCAL nOld := ::currentIndex()
    LOCAL n

    IF Len( ::Frows ) == 0
        RETURN
    ENDIF
    n := Max( 1, Min( Len( ::Frows ), nOld + nDelta ) )
    IF n == nOld
        RETURN
    ENDIF
    ::setCurrentIndex( n )
    ::fireChanged()

RETURN

METHOD PROCEDURE firstLetter( cLetter ) CLASS HTTreeView

    LOCAL i, nStart, nCur

    nCur := ::currentIndex()
    nStart := IIF( nCur < Len( ::Frows ), nCur + 1, 1 )
    FOR i := nStart TO Len( ::Frows )
        IF Upper( Left( ::Frows[ i ][ 1 ]:text, 1 ) ) == cLetter
            ::setCurrentIndex( i )
            ::fireChanged()
            RETURN
        ENDIF
    NEXT
    FOR i := 1 TO nCur
        IF Upper( Left( ::Frows[ i ][ 1 ]:text, 1 ) ) == cLetter
            ::setCurrentIndex( i )
            ::fireChanged()
            RETURN
        ENDIF
    NEXT

RETURN

METHOD PROCEDURE fireChanged() CLASS HTTreeView
    IF ::FonChanged != NIL .AND. hb_isObject( ::Fcurrent )
        Eval( ::FonChanged, ::Fcurrent )
    ENDIF
RETURN

METHOD PROCEDURE repaintSelf() CLASS HTTreeView

    LOCAL parent := ::parent()

    IF parent != NIL .AND. parent:isDerivedFrom( "HTWidget" )
        parent:repaintChild( self )
    ENDIF

RETURN

STATIC PROCEDURE ht_treeExpandAll( aNodes )

    LOCAL o

    FOR EACH o IN aNodes
        IF o:hasChildren()
            o:setExpanded( .T. )
            ht_treeExpandAll( o:children() )
        ENDIF
    NEXT

RETURN

STATIC FUNCTION ht_treeCount( aNodes )

    LOCAL n := 0
    LOCAL o

    FOR EACH o IN aNodes
        n += 1 + ht_treeCount( o:children() )
    NEXT

RETURN n

STATIC FUNCTION ht_treeFind( aNodes, xUser )

    LOCAL o, oHit

    FOR EACH o IN aNodes
        IF o:userData == xUser
            RETURN o
        ENDIF
        oHit := ht_treeFind( o:children(), xUser )
        IF oHit != NIL
            RETURN oHit
        ENDIF
    NEXT

RETURN NIL
