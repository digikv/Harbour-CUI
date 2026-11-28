/** @file business.prg
 * The six widgets a business screen is built from:
 *   HTTreeView, HTSplitter, HTPropertyGrid, HTTagInput,
 *   HTMarkdownView, HTCommandPalette.
 *
 * Ctrl+P opens the command palette. Click a splitter bar, then use the
 * arrows. Space / Right opens a branch in the tree.
 */

#include "hbtui.ch"
#include "inkey.ch"

STATIC oStatus
STATIC oTree
STATIC oProps
STATIC oTags
STATIC oHelp
STATIC oPal

PROCEDURE Main()

    LOCAL app, win, oBar, oMenu, oAct
    LOCAL oSplit, oRight

    SetMode( 36, 100 )
    app := HTApplication():new()

    win := HTMainWindow():new()
    win:setWindowTitle( " Konta " )
    win:resize( 98, 34 )

    oBar := HTMenuBar():new( win )
    oMenu := oBar:addMenu( "&File" )
    oAct := oMenu:addAction( "E&xit" )
    oAct:onTriggered := {|| app:quit() }
    oMenu := oBar:addMenu( "&View" )
    oAct := oMenu:addAction( "Command &palette" )
    oAct:setShortcut( K_CTRL_P )
    oAct:onTriggered := {|| oPal:exec() }
    oMenu := oBar:addMenu( "&Help" )
    oAct := oMenu:addAction( "&About" )
    oAct:onTriggered := {|| HTMessageBox():information( "Konta", ;
        "Tree, splitter, tags, properties, markdown and a command palette." ) }

    oPal := HTCommandPalette():new()
    oPal:addMenus( oBar )
    oPal:addCommand( "Expand the tree", {|| oTree:expandAll(), win:repaint() }, "Ctrl+E", "View" )
    oPal:addCommand( "Bruto bilans", {|| oStatus:setSection( 1, "Bruto bilans" ) }, "Ctrl+B", "Izvestaji" )
    win:onKey( K_CTRL_P, {|| oPal:exec() } )

    oSplit := HTSplitter():new( HT_SPLIT_VERTICAL, win )
    oSplit:setGeometry( 0, 1, 98, 30 )

    oTree := HTTreeView():new()
    FillTree( oTree )
    oTree:onChanged := {| oNode | ShowNode( oNode ), win:repaint() }
    oTree:onActivated := {| oNode | oStatus:setSection( 1, "Open " + oNode:text ) }
    oSplit:addWidget( oTree )

    oRight := HTSplitter():new( HT_SPLIT_HORIZONTAL )
    oTags := HTTagInput():new()
    oTags:setSuggestions( { "Placeno", "Neproknjizeno", "Storno", "Magacin" } )
    oTags:onChanged := {| a | oStatus:setSection( 1, "Filter: " + hb_ntos( Len( a ) ) ) }
    oProps := HTPropertyGrid():new()
    FillProps( oProps )
    oProps:onBrowse := {| c, v | HB_SYMBOL_UNUSED( c ), HB_SYMBOL_UNUSED( v ), "konto.dbf" }
    oHelp := HTMarkdownView():new()
    oHelp:setMarkdown( HelpText() )
    oRight:addWidget( oTags )
    oRight:addWidget( oProps )
    oRight:addWidget( oHelp )
    oRight:setSizes( { 1, 14, 12 } )
    oSplit:addWidget( oRight )
    oSplit:setSizes( { 32, 64 } )

    oStatus := HTStatusBar():new( win )
    oStatus:setGeometry( 0, 32, 98, 1 )
    oStatus:addSection( "Ctrl+P commands   click a bar, then arrows" )

    ShowNode( oTree:currentNode() )
    oProps:onChanged := {| c, x | oStatus:setSection( 1, c + " = " + hb_CStr( x ) ) }
    win:show()
    app:exec()

RETURN

STATIC PROCEDURE FillTree( o )

    LOCAL oClass, oGroup

    oClass := o:addNode( "Class 0", NIL, "0" )
    o:addNode( "000 Off-balance", oClass, "000" )
    oClass := o:addNode( "Class 1", NIL, "1" )
    oGroup := o:addNode( "10 Cash", oClass, "10" )
    o:addNode( "100 Cash on hand", oGroup, "100" )
    o:addNode( "101 Checking", oGroup, "101" )
    oGroup := o:addNode( "12 Receivables", oClass, "12" )
    o:addNode( "120 Customers", oGroup, "120" )
    o:expand( oClass )

RETURN

STATIC PROCEDURE FillProps( o )

    o:addProperty( "code", "Code", HT_PROP_TEXT, "" )
    o:addProperty( "name", "Name", HT_PROP_TEXT, "" )
    o:addProperty( "active", "Active", HT_PROP_LOGICAL, .T. )
    o:addGroup( "Posting" )
    o:addProperty( "side", "Side", HT_PROP_LIST, "Debit", { "Debit", "Credit" } )
    o:addProperty( "color", "Color", HT_PROP_COLOR, "W/N" )
    o:addProperty( "card", "Card", HT_PROP_FILE, "" )

RETURN

STATIC PROCEDURE ShowNode( oNode )

    IF ! hb_isObject( oNode )
        RETURN
    ENDIF
    oProps:setValue( "code", oNode:userData )
    oProps:setValue( "name", oNode:text )
    oProps:setValue( "card", oNode:userData + ".dbf" )

RETURN

STATIC FUNCTION HelpText()
RETURN "# Account" + Chr( 10 ) + ;
    "Pick a row in the tree. **Space** opens a branch, Enter runs it." + Chr( 10 ) + ;
    "The bar between the panes is an HTSplitter: drag it, or click it and press Left / Right." + Chr( 10 ) + ;
    "Tags filter the list. Type and press Enter. Backspace removes the last tag." + Chr( 10 ) + ;
    "Ctrl+P opens the [command palette](palette)." + Chr( 10 ) + ;
    "| Key | Action |" + Chr( 10 ) + ;
    "| --- | --- |" + Chr( 10 ) + ;
    "| Right | Expand |" + Chr( 10 ) + ;
    "| Ctrl+P | Commands |"
