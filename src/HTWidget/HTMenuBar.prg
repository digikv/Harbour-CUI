/** @class HTMenuBar
 * Horizontal menu bar with keyboard activation (F10, Alt+letter accelerators).
 * Supports dropdown menus with arrow-key navigation and Enter to trigger actions.
 * @extends HTWidget
 */

#include "hbtui.ch"
#include "inkey.ch"


CLASS HTMenuBar FROM HTWidget

PROTECTED:

    DATA FactiveMenu     INIT 0
    DATA FactiveItem     INIT 0
    DATA FmenuOpen       INIT .F.
    DATA FdropWinId      INIT NIL
    DATA FdropRect       INIT NIL   /* { top, left, bottom, right } screen-absolute */

    /* open submenu (an HTMenu entry of the dropdown) */
    DATA FsubMenu        INIT NIL
    DATA FsubItem        INIT 0
    DATA FsubWinId       INIT NIL
    DATA FsubRect        INIT NIL

    METHOD openMenu( nIndex )
    METHOD closeMenu()
    METHOD openSubMenu( lSelectFirst )
    METHOD closeSubMenu()
    METHOD paintDropdown( oMenu )
    METHOD paintMenuWindow( oMenu, nWinId, nActive )
    METHOD reselectParent()
    METHOD getMenus()

PUBLIC:

    CONSTRUCTOR new( parent )

    METHOD addAction( ... )
    METHOD addMenu( ... )
    METHOD addSeparator()

    METHOD paintEvent( event )
    METHOD handleKey( nKey )
    METHOD handleClick( nRow, nCol )
    METHOD handleMouseClick( nAbsRow, nAbsCol )
    METHOD isMenuOpen() INLINE ::FmenuOpen

ENDCLASS

/** Creates a menu bar attached to the given parent widget.
 * @param parent Parent widget (typically HTMainWindow or HTDesktop)
 */
METHOD new( parent ) CLASS HTMenuBar

    LOCAL version := 0

    IF pCount() <= 1
        IF parent == NIL .OR. hb_isObject( parent )
            version := 1
        ENDIF
    ENDIF

    SWITCH version
    CASE 1
        ::super:new( parent )
        EXIT
    OTHERWISE
        ::PARAM_ERROR()
    ENDSWITCH

RETURN self

/** Adds an action to the menu bar. Accepts text, (text, receiver, member), or HTAction.
 * @param ... String text, or (text, receiver, member), or HTAction object
 * @return HTAction instance
 */
METHOD FUNCTION addAction( ... ) CLASS HTMenuBar

    LOCAL version := 0
    LOCAL action
    LOCAL text
    LOCAL receiver
    LOCAL member
    LOCAL retValue

    IF pCount() = 1
        text := hb_pValue( 1 )
        IF hb_isChar( text )
            version := 1
        ENDIF
    ENDIF

    IF pCount() = 3
        text     := hb_pValue( 1 )
        receiver := hb_pValue( 2 )
        member   := hb_pValue( 3 )
        IF hb_isChar( text ) .AND. hb_isObject( receiver ) .AND. hb_isChar( member )
            version := 2
        ENDIF
    ENDIF

    IF pCount() = 1
        action := hb_pValue( 1 )
        IF hb_isObject( action )
            version := 3
        ENDIF
    ENDIF

    SWITCH version
    CASE 1
        action := HTAction():new( text, self )
        retValue := action
        EXIT
    CASE 2
        action := HTAction():new( text, receiver )
        retValue := action
        EXIT
    CASE 3
        EXIT
    OTHERWISE
        ::PARAM_ERROR()
    ENDSWITCH

    ::super:addAction( action )

RETURN retValue

/** Adds a submenu to the menu bar. Accepts HTMenu object or title string.
 * @param ... HTMenu instance or string title
 * @return HTMenu instance
 */
METHOD FUNCTION addMenu( ... ) CLASS HTMenuBar

    LOCAL version := 0
    LOCAL menu
    LOCAL title
    LOCAL retValue

    IF pCount() = 1
        menu := hb_pValue( 1 )
        IF hb_isObject( menu ) .AND. menu:isDerivedFrom("HTMenu")
            version := 1
        ENDIF
    ENDIF

    IF pCount() = 1
        title := hb_pValue( 1 )
        IF hb_isChar( title )
            version := 2
        ENDIF
    ENDIF

    SWITCH version
    CASE 1
        menu:setParent( self )
        retValue := menu:menuAction()
        EXIT
    CASE 2
        menu := HTMenu():new( title, self )
        retValue := menu
        EXIT
    OTHERWISE
        ::PARAM_ERROR()
    ENDSWITCH

RETURN retValue

/** Adds a visual separator to the menu bar (no-op placeholder).
 * @return Self
 */
METHOD FUNCTION addSeparator() CLASS HTMenuBar

RETURN self

/** Returns array of HTMenu children.
 * @return Array of HTMenu
 */
METHOD FUNCTION getMenus() CLASS HTMenuBar

    LOCAL aMenus := {}
    LOCAL itm

    FOR EACH itm IN ::children
        IF itm:isDerivedFrom("HTMenu")
            AAdd( aMenus, itm )
        ENDIF
    NEXT

RETURN aMenus

/** Paints the menu bar row with menu titles, highlighting the active menu.
 * @param event HTPaintEvent instance
 */
METHOD PROCEDURE paintEvent( event ) CLASS HTMenuBar

    LOCAL itm
    LOCAL y := 0
    LOCAL nIdx := 0
    LOCAL cColor

    HB_SYMBOL_UNUSED( event )

    wSelect( ::parent():windowId, .F. )
    wFormat()
    wFormat( 1, 0, 0, 0 )
    DispOutAt( 0, 0, Space( ::parent():width ), HTTheme():getColor( HT_CLR_MENU_BAR ) )
    FOR EACH itm IN ::children
        IF itm:isDerivedFrom("HTMenu")
            nIdx++
            cColor := IIF( nIdx = ::FactiveMenu, HTTheme():getColor( HT_CLR_MENU_BAR_SEL ), HTTheme():getColor( HT_CLR_MENU_BAR ) )
            itm:move( 0, ++y )
            DispOutAt( 0, y, " " + itm:title + " ", cColor )
            y += ht_strWidth( itm:title ) + 2
        ENDIF
    NEXT
    wFormat()

RETURN

/** Processes keyboard input for the menu bar (F10 toggle, Alt+letter,
 * arrow navigation, Enter to trigger, ESC to close).
 * @param nKey Inkey code
 * @return .T. if the key was consumed
 */
METHOD FUNCTION handleKey( nKey ) CLASS HTMenuBar

    LOCAL aMenus := ::getMenus()
    LOCAL nMenuCount := Len( aMenus )
    LOCAL i
    LOCAL parent
    LOCAL aItems, nItemCount, oItem

    IF nMenuCount = 0
        RETURN .F.
    ENDIF

    /* F10 toggles menu activation */
    IF nKey = K_F10
        IF ::FactiveMenu = 0
            ::FactiveMenu := 1
        ELSE
            ::FactiveMenu := 0
            ::closeMenu()
        ENDIF
        parent := ::parent()
        IF parent != NIL
            parent:repaint()
        ENDIF
        RETURN .T.
    ENDIF

    /* Alt+letter accelerators (check first letter of each menu title) */
    IF ::FactiveMenu = 0
        /* check for Alt+letter: Alt key codes are typically K_ALT_A to K_ALT_Z */
        FOR i := 1 TO nMenuCount
            IF nKey = hb_keyCode( "Alt+" + Upper( Left( aMenus[ i ]:title, 1 ) ) )
                ::FactiveMenu := i
                ::openMenu( i )
                parent := ::parent()
                IF parent != NIL
                    parent:repaint()
                ENDIF
                RETURN .T.
            ENDIF
        NEXT
    ENDIF

    /* if menu bar is not active, don't consume keys */
    IF ::FactiveMenu = 0
        RETURN .F.
    ENDIF

    /* an open submenu owns the arrows, Enter and Esc */
    IF ::FsubWinId != NIL
        aItems := ::FsubMenu:items()

        SWITCH nKey
        CASE K_DOWN
            ::FsubItem := ht_menuStep( aItems, ::FsubItem, 1 )
            ::paintMenuWindow( ::FsubMenu, ::FsubWinId, ::FsubItem )
            ::reselectParent()
            EXIT
        CASE K_UP
            ::FsubItem := ht_menuStep( aItems, ::FsubItem, -1 )
            ::paintMenuWindow( ::FsubMenu, ::FsubWinId, ::FsubItem )
            ::reselectParent()
            EXIT
        CASE K_ENTER
            IF ::FsubItem >= 1 .AND. ::FsubItem <= Len( aItems ) .AND. ;
               ! ht_menuIsSub( aItems[ ::FsubItem ] )
                oItem := aItems[ ::FsubItem ]
                ::closeMenu()
                ::FactiveMenu := 0
                ::FactiveItem := 0
                oItem:trigger()
            ENDIF
            EXIT
        CASE K_LEFT
        CASE K_ESC
            /* back to the parent dropdown */
            ::closeSubMenu()
            EXIT
        CASE K_RIGHT
            EXIT
        OTHERWISE
            RETURN .F.
        ENDSWITCH

        parent := ::parent()
        IF parent != NIL
            parent:repaint()
        ENDIF
        RETURN .T.
    ENDIF

    /* menu bar is active: handle navigation */
    IF ::FmenuOpen .AND. ::FactiveMenu >= 1 .AND. ::FactiveMenu <= nMenuCount
        aItems := aMenus[ ::FactiveMenu ]:items()
        nItemCount := Len( aItems )
    ELSE
        aItems := {}
        nItemCount := 0
    ENDIF

    /* the highlighted dropdown entry, if any */
    IF ::FactiveItem >= 1 .AND. ::FactiveItem <= nItemCount
        oItem := aItems[ ::FactiveItem ]
    ENDIF

    SWITCH nKey
    CASE K_LEFT
        ::FactiveMenu := IIF( ::FactiveMenu <= 1, nMenuCount, ::FactiveMenu - 1 )
        IF ::FmenuOpen
            ::FactiveItem := 0
            ::openMenu( ::FactiveMenu )
        ENDIF
        EXIT
    CASE K_RIGHT
        IF oItem != NIL .AND. ht_menuIsSub( oItem )
            ::openSubMenu( .T. )
            EXIT
        ENDIF
        ::FactiveMenu := IIF( ::FactiveMenu >= nMenuCount, 1, ::FactiveMenu + 1 )
        IF ::FmenuOpen
            ::FactiveItem := 0
            ::openMenu( ::FactiveMenu )
        ENDIF
        EXIT
    CASE K_DOWN
        IF ::FmenuOpen
            /* navigate down in dropdown, skip separators */
            ::FactiveItem := ht_menuStep( aItems, ::FactiveItem, 1 )
            ::paintDropdown( aMenus[ ::FactiveMenu ] )
            ::reselectParent()
        ELSE
            ::openMenu( ::FactiveMenu )
        ENDIF
        EXIT
    CASE K_UP
        IF ::FmenuOpen .AND. nItemCount > 0
            ::FactiveItem := ht_menuStep( aItems, ::FactiveItem, -1 )
            ::paintDropdown( aMenus[ ::FactiveMenu ] )
            ::reselectParent()
        ENDIF
        EXIT
    CASE K_ENTER
        IF ::FmenuOpen .AND. oItem != NIL
            IF ht_menuIsSub( oItem )
                ::openSubMenu( .T. )
            ELSE
                /* close first, so the action runs on a clean screen */
                ::closeMenu()
                ::FactiveMenu := 0
                ::FactiveItem := 0
                oItem:trigger()
            ENDIF
        ELSEIF ! ::FmenuOpen
            ::openMenu( ::FactiveMenu )
        ELSE
            ::closeMenu()
            ::FactiveMenu := 0
        ENDIF
        EXIT
    CASE K_ESC
        ::closeMenu()
        ::FactiveMenu := 0
        ::FactiveItem := 0
        EXIT
    OTHERWISE
        RETURN .F.
    ENDSWITCH

    parent := ::parent()
    IF parent != NIL
        parent:repaint()
    ENDIF

RETURN .T.

/** Opens the dropdown for the menu at the given index.
 * @param nIndex 1-based menu index
 */
METHOD PROCEDURE openMenu( nIndex ) CLASS HTMenuBar

    LOCAL aMenus := ::getMenus()
    LOCAL oMenu
    LOCAL aItems, nMaxWidth
    LOCAL nDropTop, nDropLeft, nDropBottom, nDropRight
    LOCAL itm, nMenuLeft
    LOCAL nOldShadow

    IF nIndex < 1 .OR. nIndex > Len( aMenus )
        RETURN
    ENDIF

    /* close any existing dropdown */
    ::closeMenu()

    oMenu := aMenus[ nIndex ]
    ::FmenuOpen := .T.

    /* calculate dropdown size: actions, separators and submenus */
    aItems := oMenu:items()
    nMaxWidth := ht_menuWidth( aItems )

    IF Len( aItems ) = 0
        RETURN
    ENDIF

    /* find menu's horizontal position */
    nMenuLeft := 0
    FOR itm := 1 TO nIndex - 1
        nMenuLeft += ht_strWidth( aMenus[ itm ]:title ) + 3
    NEXT
    nMenuLeft++

    nDropTop    := wRow() + 2
    nDropLeft   := wCol() + nMenuLeft
    nDropBottom := nDropTop + Len( aItems ) + 1
    nDropRight  := nDropLeft + nMaxWidth

    nOldShadow := wSetShadow( HT_SHADOW_ATTR )
    ::FdropWinId := wOpen( nDropTop, nDropLeft, nDropBottom, nDropRight, .T. )
    wSetShadow( nOldShadow )
    wBox( NIL, HTTheme():getColor( HT_CLR_MENU_ITEM ) )
    wFormat()
    ::FdropRect := { nDropTop, nDropLeft, nDropBottom, nDropRight }

    ::paintDropdown( oMenu )

RETURN

/** Closes the open dropdown (and its submenu) and restores the parent window. */
METHOD PROCEDURE closeMenu() CLASS HTMenuBar

    ::closeSubMenu()

    IF ::FdropWinId != NIL
        wClose( ::FdropWinId )
        ::FdropWinId := NIL
        ::FdropRect  := NIL
    ENDIF

    ::FmenuOpen := .F.

    ::reselectParent()

RETURN

/** Opens the submenu of the highlighted dropdown entry beside the dropdown
 * (left of it when there is no room on the right).
 * @param lSelectFirst .T. highlights the first entry (keyboard opening)
 */
METHOD PROCEDURE openSubMenu( lSelectFirst ) CLASS HTMenuBar

    LOCAL aMenus := ::getMenus()
    LOCAL aItems, aSubItems
    LOCAL oSub
    LOCAL nWidth
    LOCAL nTop, nLeft, nBottom, nRight
    LOCAL nOldShadow

    IF ::FdropWinId = NIL .OR. ::FactiveMenu < 1 .OR. ::FactiveMenu > Len( aMenus )
        RETURN
    ENDIF

    aItems := aMenus[ ::FactiveMenu ]:items()
    IF ::FactiveItem < 1 .OR. ::FactiveItem > Len( aItems ) .OR. ;
       ! ht_menuIsSub( aItems[ ::FactiveItem ] )
        RETURN
    ENDIF

    oSub := aItems[ ::FactiveItem ]
    aSubItems := oSub:items()

    ::closeSubMenu()

    IF Len( aSubItems ) = 0
        RETURN
    ENDIF

    nWidth  := ht_menuWidth( aSubItems )
    /* box top one row above the entry, so the first item lines up with it */
    nTop    := ::FdropRect[ 1 ] + ::FactiveItem - 1
    nLeft   := ::FdropRect[ 4 ] + 1
    nBottom := nTop + Len( aSubItems ) + 1
    nRight  := nLeft + nWidth

    /* keep the box and its shadow (1 row, 2 cols) on screen */
    IF nRight + 2 > ht_ScreenMaxCol()
        nRight := ::FdropRect[ 2 ] - 1
        nLeft  := nRight - nWidth
    ENDIF
    IF nBottom + 1 > ht_ScreenMaxRow()
        nTop    -= nBottom + 1 - ht_ScreenMaxRow()
        nBottom := nTop + Len( aSubItems ) + 1
    ENDIF
    nTop  := Max( nTop, 0 )
    nLeft := Max( nLeft, 0 )

    nOldShadow := wSetShadow( HT_SHADOW_ATTR )
    ::FsubWinId := wOpen( nTop, nLeft, nBottom, nRight, .T. )
    wSetShadow( nOldShadow )
    wBox( NIL, HTTheme():getColor( HT_CLR_MENU_ITEM ) )
    wFormat()

    ::FsubMenu := oSub
    ::FsubRect := { nTop, nLeft, nBottom, nRight }
    ::FsubItem := IIF( HB_ISLOGICAL( lSelectFirst ) .AND. lSelectFirst, ht_menuStep( aSubItems, 0, 1 ), 0 )

    ::paintMenuWindow( oSub, ::FsubWinId, ::FsubItem )
    ::reselectParent()

RETURN

/** Closes the open submenu, if any (the parent dropdown stays open). */
METHOD PROCEDURE closeSubMenu() CLASS HTMenuBar

    IF ::FsubWinId != NIL
        wClose( ::FsubWinId )
        ::FsubWinId := NIL
        ::FsubRect  := NIL
        ::FsubMenu  := NIL
        ::FsubItem  := 0
    ENDIF

RETURN

/** Renders the dropdown menu items with highlight on the active item.
 * @param oMenu HTMenu whose entries to display
 */
METHOD PROCEDURE paintDropdown( oMenu ) CLASS HTMenuBar

    IF ::FdropWinId != NIL
        ::paintMenuWindow( oMenu, ::FdropWinId, ::FactiveItem )
    ENDIF

RETURN

/** Paints the entries of oMenu inside an open menu window (its box is
 * already drawn). Submenu entries end with a ► marker.
 * @param oMenu   HTMenu whose entries to display
 * @param nWinId  window opened for it
 * @param nActive highlighted entry (0 = none)
 */
METHOD PROCEDURE paintMenuWindow( oMenu, nWinId, nActive ) CLASS HTMenuBar

    LOCAL itm
    LOCAL nRow := 0
    LOCAL nMaxCol
    LOCAL nIdx := 0
    LOCAL cColor, cText
    LOCAL aPaint

    /* a window under another one (e.g. under a submenu's shadow) paints on
       its own layer instead of over the window above */
    aPaint := ht_winPaintBegin( nWinId )

    wSelect( nWinId, .F. )
    wFormat()
    wFormat( 1, 1, 1, 1 )

    nMaxCol := MaxCol()

    FOR EACH itm IN oMenu:items()
        nIdx++
        IF ht_menuIsSep( itm )
            DispOutAt( nRow, 0, Replicate( ht_glyph( 0x2500, "-" ), nMaxCol + 1 ), HTTheme():getColor( HT_CLR_MENU_SEP ) )
        ELSE
            cColor := IIF( nIdx = nActive, HTTheme():getColor( HT_CLR_MENU_ITEM_SEL ), HTTheme():getColor( HT_CLR_MENU_ITEM ) )
            cText  := ht_cellPadR( " " + ht_menuText( itm ), nMaxCol + 1 )
            IF ht_menuIsSub( itm )
                cText := ht_cellPadR( cText, nMaxCol - 1 ) + Chr( 16 ) + " "   /* ► */
            ENDIF
            DispOutAt( nRow, 0, cText, cColor )
        ENDIF
        nRow++
    NEXT

    wFormat()

    ht_winPaintEnd( nWinId, aPaint, .F. )

RETURN

/** Makes the menu bar's window the selected (keyboard) window again. */
METHOD PROCEDURE reselectParent() CLASS HTMenuBar

    LOCAL parent := ::parent()

    IF parent != NIL .AND. parent:isDerivedFrom( "HTWidget" )
        wSelect( parent:windowId, .F. )
    ENDIF

RETURN

/** Handles a mouse click on the menu bar row.
 * Determines which menu title was clicked and opens/closes the dropdown.
 * @param nRow Row relative to the menu bar (0 = menu bar row)
 * @param nCol Column relative to the window content area
 * @return .T. if the click was handled
 */
METHOD FUNCTION handleClick( nRow, nCol ) CLASS HTMenuBar

    LOCAL aMenus := ::getMenus()
    LOCAL nMenuCount := Len( aMenus )
    LOCAL i, nLeft, nRight, nWidth
    LOCAL parent

    IF nMenuCount = 0 .OR. nRow != 0
        RETURN .F.
    ENDIF

    /* determine which menu title was clicked */
    nLeft := 1
    FOR i := 1 TO nMenuCount
        nWidth := ht_strWidth( aMenus[ i ]:title ) + 2
        nRight := nLeft + nWidth - 1
        IF nCol >= nLeft .AND. nCol <= nRight
            IF ::FactiveMenu = i .AND. ::FmenuOpen
                ::closeMenu()
                ::FactiveMenu := 0
            ELSE
                IF ::FmenuOpen
                    ::closeMenu()
                ENDIF
                ::FactiveMenu := i
                ::openMenu( i )
            ENDIF
            parent := ::parent()
            IF parent != NIL .AND. parent:isDerivedFrom( "HTWidget" )
                parent:repaint()
            ENDIF
            RETURN .T.
        ENDIF
        nLeft += nWidth
    NEXT

RETURN .F.

/** Handles a mouse click while a dropdown menu is open.
 * Checks if the click is on a dropdown item, on the menu bar, or outside.
 * @param nAbsRow Screen-absolute row
 * @param nAbsCol Screen-absolute column
 * @return .T. if the click was handled
 */
METHOD FUNCTION handleMouseClick( nAbsRow, nAbsCol ) CLASS HTMenuBar

    LOCAL aMenus, aItems, oItem
    LOCAL nDropRow, nDropCol
    LOCAL nItemIdx
    LOCAL parent

    IF ! ::FmenuOpen .OR. ::FdropWinId = NIL
        RETURN .F.
    ENDIF

    parent := ::parent()

    /* the submenu is on top: check it first */
    IF ::FsubWinId != NIL .AND. ht_menuInside( ::FsubRect, nAbsRow, nAbsCol )
        aItems   := ::FsubMenu:items()
        nItemIdx := nAbsRow - ::FsubRect[ 1 ]
        IF nItemIdx >= 1 .AND. nItemIdx <= Len( aItems ) .AND. ;
           ! ht_menuIsSep( aItems[ nItemIdx ] ) .AND. ! ht_menuIsSub( aItems[ nItemIdx ] )
            oItem := aItems[ nItemIdx ]
            ::closeMenu()
            ::FactiveMenu := 0
            ::FactiveItem := 0
            oItem:trigger()
            IF parent != NIL .AND. parent:isDerivedFrom( "HTWidget" )
                parent:repaint()
            ENDIF
        ENDIF
        RETURN .T.  /* separator or border — consume */
    ENDIF

    /* check if click is inside the dropdown */
    IF ht_menuInside( ::FdropRect, nAbsRow, nAbsCol )

        aMenus   := ::getMenus()
        aItems   := aMenus[ ::FactiveMenu ]:items()
        nItemIdx := nAbsRow - ::FdropRect[ 1 ]

        IF nItemIdx >= 1 .AND. nItemIdx <= Len( aItems ) .AND. ;
           ! ht_menuIsSep( aItems[ nItemIdx ] )

            oItem := aItems[ nItemIdx ]

            IF ht_menuIsSub( oItem )
                /* click on a submenu entry toggles its submenu */
                IF ::FsubWinId != NIL .AND. ::FactiveItem == nItemIdx
                    ::closeSubMenu()
                ELSE
                    ::FactiveItem := nItemIdx
                    ::paintDropdown( aMenus[ ::FactiveMenu ] )
                    ::openSubMenu( .F. )
                ENDIF
                ::reselectParent()
                RETURN .T.
            ENDIF

            ::closeMenu()
            ::FactiveMenu := 0
            ::FactiveItem := 0
            oItem:trigger()

            IF parent != NIL .AND. parent:isDerivedFrom( "HTWidget" )
                parent:repaint()
            ENDIF
            RETURN .T.
        ENDIF
        RETURN .T.  /* click on separator or border — consume but don't trigger */
    ENDIF

    /* check if click is on the menu bar row — switch menus */
    parent := ::parent()
    IF parent != NIL
        wSelect( parent:windowId, .F. )
        nDropRow := nAbsRow - wRow()
        nDropCol := nAbsCol - wCol()
        IF nDropRow = 1  /* menu bar is at window row 1 (border=0, menubar=1) */
            ::closeMenu()
            ::FactiveMenu := 0
            RETURN ::handleClick( 0, nDropCol - 1 )
        ENDIF
    ENDIF

    /* click outside — close the menu */
    ::closeMenu()
    ::FactiveMenu := 0
    ::FactiveItem := 0
    IF parent != NIL .AND. parent:isDerivedFrom( "HTWidget" )
        parent:repaint()
    ENDIF

RETURN .T.

/* ---- dropdown entry helpers (entries are HTAction or HTMenu) ---- */

/** .T. when the entry is a submenu. */
STATIC FUNCTION ht_menuIsSub( itm )
RETURN itm:isDerivedFrom( "HTMenu" )

/** .T. when the entry is a separator action. */
STATIC FUNCTION ht_menuIsSep( itm )
RETURN ! ht_menuIsSub( itm ) .AND. itm:isSeparator()

/** Display text of an entry: action text or submenu title. */
STATIC FUNCTION ht_menuText( itm )

    LOCAL cText := IIF( ht_menuIsSub( itm ), itm:title, itm:text )

RETURN IIF( HB_ISSTRING( cText ), cText, "" )

/** Next selectable (non-separator) entry from nCur in direction nStep,
 * wrapping around. nCur = 0 starts before the first entry.
 * @return entry index, or 0 when there is none
 */
STATIC FUNCTION ht_menuStep( aItems, nCur, nStep )

    LOCAL nLen := Len( aItems )
    LOCAL n := nCur
    LOCAL i

    FOR i := 1 TO nLen
        n += nStep
        IF n > nLen
            n := 1
        ELSEIF n < 1
            n := nLen
        ENDIF
        IF ! ht_menuIsSep( aItems[ n ] )
            RETURN n
        ENDIF
    NEXT

RETURN 0

/** Box width (right - left) for a dropdown listing aItems; submenu
 * entries get room for the ► marker. */
STATIC FUNCTION ht_menuWidth( aItems )

    LOCAL nWidth := 10
    LOCAL itm

    FOR EACH itm IN aItems
        IF ! ht_menuIsSep( itm )
            nWidth := Max( nWidth, ht_strWidth( ht_menuText( itm ) ) + IIF( ht_menuIsSub( itm ), 6, 4 ) )
        ENDIF
    NEXT

RETURN nWidth

/** .T. when the screen cell is inside the box (border excluded). */
STATIC FUNCTION ht_menuInside( aRect, nRow, nCol )
RETURN aRect != NIL .AND. ;
    nRow >= aRect[ 1 ] + 1 .AND. nRow <= aRect[ 3 ] - 1 .AND. ;
    nCol >= aRect[ 2 ] + 1 .AND. nCol <= aRect[ 4 ] - 1
