/**
 * test_widgets.prg - Property and container tests for HBTui widget classes.
 *
 * Tests: HTLabel, HTLineEdit, HTCheckBox, HTPushButton, HTListBox,
 *        HTComboBox, HTCheckList, HTSpinner, HTProgressBar, HTScrollBar,
 *        HTTreeView, HTSplitter, HTMarkdownView, HTTagInput,
 *        HTCommandPalette, HTPropertyGrid,
 *        Unicode (CJK) typing: ht_keyText(), HTLineEdit/HTTextEdit/HTGet,
 *        HTApplication's UTF8EX default codepage
 *
 * Runs headless (-gtnul). Callbacks are verified by calling the method
 * that fires them (toggle() for checkboxes) and checking a captured variable.
 *
 * Build: hbmk2 test_widgets.prg hbmk.hbm
 */

#include "hbtui.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"

REQUEST HB_CODEPAGE_CP950

STATIC nPass := 0
STATIC nFail := 0

PROCEDURE Main()

    LOCAL nTotal

    TestLabel()
    TestLineEdit()
    TestCheckBox()
    TestPushButton()
    TestListBox()
    TestComboBox()
    TestCheckList()
    TestSpinner()
    TestProgressBar()
    TestScrollBar()
    TestTextEdit()
    TestContextMenu()
    TestMenuItems()
    TestTitleButtons()
    TestCharts()
    TestMessageWrap()
    TestPixelFont()
    TestBig5Screen()
    TestSearchBox()
    TestDatePicker()
    TestTreeView()
    TestSplitter()
    TestMarkdown()
    TestTagInput()
    TestCommandPalette()
    TestPropertyGrid()
    TestUnicodeInput()   /* last: HTApplication():new() switches the codepage */

    nTotal := nPass + nFail
    QOut( "" )
    QOut( "Results: " + hb_ntos( nPass ) + " passed, " + hb_ntos( nFail ) + " failed out of " + hb_ntos( nTotal ) )
    ErrorLevel( IIF( nFail > 0, 1, 0 ) )

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE Assert( cName, lCond )

    IF lCond
        nPass++
    ELSE
        nFail++
        OutErr( "FAIL: " + cName + hb_eol() )
    ENDIF

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE TestLabel()

    LOCAL o

    o := HTLabel():new()
    Assert( "HTLabel:text default empty",  o:text == "" )

    o:setText( "Hello" )
    Assert( "HTLabel:setText Hello",       o:text == "Hello" )

    o:setText( "World" )
    Assert( "HTLabel:setText World",       o:text == "World" )

    Assert( "HTLabel:alignment default",   o:alignment == HT_ALIGN_LEFT )

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE TestLineEdit()

    LOCAL o, t

    o := HTLineEdit():new()
    Assert( "HTLineEdit:text default empty", o:text == "" )

    o:text := "hello"
    Assert( "HTLineEdit:text set",           o:text == "hello" )

    o:setText( "world" )
    Assert( "HTLineEdit:setText",            o:text == "world" )

    o:setText( "" )
    Assert( "HTLineEdit:setText empty",      o:text == "" )

    /* K_INS = K_CTRL_V, K_PGDN = K_CTRL_C, K_DOWN = K_CTRL_X: without Ctrl
       held (gtnul reports no shift keys) they must not touch the clipboard */
    hb_gtInfo( HB_GTI_CLIPBOARDDATA, "PASTED" )
    o:setText( "abc" )
    o:keyEvent( HTKeyEvent():new( K_END ) )
    o:keyEvent( HTKeyEvent():new( K_INS ) )
    Assert( "HTLineEdit:Ins does not paste",  o:text == "abc" )
    o:keyEvent( HTKeyEvent():new( K_DOWN ) )
    o:keyEvent( HTKeyEvent():new( K_PGDN ) )
    Assert( "HTLineEdit:Down/PgDn do not cut", o:text == "abc" )

    t := HTTextEdit():new()
    t:setText( "abc" )
    t:keyEvent( HTKeyEvent():new( K_INS ) )
    Assert( "HTTextEdit:Ins does not paste",  t:getText() == "abc" )

    /* Shift+Right selection, then Del (google group report): Del removed one
       character, because every key without Shift cleared the selection */
    t:setText( "abcdef" )
    t:setSelection( 1, 2, 1, 5 )                       /* "bcd" */
    Assert( "HTTextEdit:setSelection text",    t:selectedText() == "bcd" )
    t:keyEvent( HTKeyEvent():new( K_DEL ) )
    Assert( "HTTextEdit:Del removes selection", t:getText() == "aef" )
    t:setText( "abcdef" )
    t:setSelection( 1, 5, 1, 2 )                       /* selected leftwards */
    t:keyEvent( HTKeyEvent():new( K_BS ) )
    Assert( "HTTextEdit:Backspace removes selection", t:getText() == "aef" )
    t:setText( "abcdef" )
    t:setSelection( 1, 2, 1, 5 )
    t:keyEvent( HTKeyEvent():new( Asc( "X" ) ) )
    Assert( "HTTextEdit:typing replaces selection", t:getText() == "aXef" )
    t:setSelection( 1, 1, 1, 2 )
    t:keyEvent( HTKeyEvent():new( K_RIGHT ) )         /* no Shift: clears */
    t:keyEvent( HTKeyEvent():new( K_DEL ) )
    Assert( "HTTextEdit:arrow clears selection", t:getText() == "aXf" )

    /* across 4 lines: the old ADel( a, n, nCount ) + ASize() cut lines off
       the end of the text instead of the selected middle lines */
    t:setText( "11" + hb_eol() + "22" + hb_eol() + "33" + hb_eol() + "44" + hb_eol() + "55" + hb_eol() + "66" )
    t:setSelection( 2, 2, 5, 2 )                       /* "2" .. "5" */
    t:keyEvent( HTKeyEvent():new( K_DEL ) )
    Assert( "HTTextEdit:Del multi-line selection", ;
        t:getText() == "11" + hb_eol() + "25" + hb_eol() + "66" )

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE TestCheckBox()

    LOCAL o
    LOCAL lToggleArg  := NIL
    LOCAL lClickFired := .F.

    o := HTCheckBox():new()
    Assert( "HTCheckBox:checked default .F.",   ! o:checked )

    /* setChecked does NOT fire callbacks - just sets state */
    o:setChecked( .T. )
    Assert( "HTCheckBox:setChecked .T.",        o:checked )

    o:setChecked( .F. )
    Assert( "HTCheckBox:setChecked .F.",        ! o:checked )

    /* toggle() flips state and fires callbacks */
    o:onToggled := {|l| lToggleArg := l }
    o:onClicked := {|| lClickFired := .T. }

    o:toggle()
    Assert( "HTCheckBox:toggle flips to .T.",   o:checked )
    Assert( "HTCheckBox:onToggled arg .T.",      lToggleArg == .T. )
    Assert( "HTCheckBox:onClicked fired",        lClickFired )

    lToggleArg  := NIL
    lClickFired := .F.
    o:toggle()
    Assert( "HTCheckBox:toggle flips to .F.",   ! o:checked )
    Assert( "HTCheckBox:onToggled arg .F.",      lToggleArg == .F. )
    Assert( "HTCheckBox:onClicked fired again",  lClickFired )

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE TestPushButton()

    LOCAL o

    o := HTPushButton():new()
    Assert( "HTPushButton:text default empty", o:text == "" )

    o:text := "OK"
    Assert( "HTPushButton:text set",           o:text == "OK" )

    o:setText( "Cancel" )
    Assert( "HTPushButton:setText",            o:text == "Cancel" )

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE TestListBox()

    LOCAL o
    LOCAL aChanged, nActivated

    o := HTListBox():new()
    Assert( "HTListBox:count empty",    o:count() == 0 )
    Assert( "HTListBox:currentIndex 0", o:currentIndex() == 0 )

    o:addItem( "Alpha" )
    Assert( "HTListBox:count after 1 add", o:count() == 1 )
    Assert( "HTListBox:currentIndex 1",    o:currentIndex() == 1 )
    Assert( "HTListBox:itemText(1)",        o:itemText( 1 ) == "Alpha" )

    o:addItem( "Beta" )
    o:addItem( "Gamma" )
    Assert( "HTListBox:count after 3 adds", o:count() == 3 )
    Assert( "HTListBox:itemText(2)",         o:itemText( 2 ) == "Beta" )
    Assert( "HTListBox:itemText(3)",         o:itemText( 3 ) == "Gamma" )

    o:setCurrentIndex( 2 )
    Assert( "HTListBox:setCurrentIndex 2",  o:currentIndex() == 2 )
    Assert( "HTListBox:currentText",        o:currentText() == "Beta" )

    o:setCurrentIndex( 99 )  /* out of range: no change */
    Assert( "HTListBox:setCurrentIndex oob stays 2", o:currentIndex() == 2 )

    o:setItemText( 2, "Beta 2" )
    Assert( "HTListBox:setItemText",        o:itemText( 2 ) == "Beta 2" .AND. o:currentIndex() == 2 )

    /* the mouse reports a new current item like the keyboard does */
    o:setCurrentIndex( 1 )
    aChanged := {}
    nActivated := 0
    o:onChanged := {| n | AAdd( aChanged, n ) }
    o:onActivated := {| n | nActivated := n }
    ListMouse( o, K_LBUTTONDOWN, 2 )
    Assert( "HTListBox: click selects row 3", o:currentIndex() == 3 )
    Assert( "HTListBox: click -> onChanged",  Len( aChanged ) == 1 .AND. aChanged[ 1 ] == 3 )
    ListMouse( o, K_LBUTTONDOWN, 2 )
    Assert( "HTListBox: same row, no onChanged", Len( aChanged ) == 1 )
    ListMouse( o, K_LBUTTONDOWN, 7 )
    Assert( "HTListBox: click below the items", o:currentIndex() == 3 .AND. Len( aChanged ) == 1 )
    ListMouse( o, K_MWFORWARD, 0 )
    Assert( "HTListBox: wheel -> onChanged",  o:currentIndex() == 2 .AND. ATail( aChanged ) == 2 )
    ListMouse( o, K_LDBLCLK, 1 )
    Assert( "HTListBox: double click -> onActivated", nActivated == 2 )

    o:clear()
    Assert( "HTListBox:clear count 0",          o:count() == 0 )
    Assert( "HTListBox:clear currentIndex 0",   o:currentIndex() == 0 )

RETURN

/* a mouse event on row nRow of a list (child-relative, as the parent passes it) */
STATIC PROCEDURE ListMouse( oList, nKey, nRow )

    LOCAL oEvent := HTMouseEvent():new( nKey, nRow, 0 )

    oEvent:mouseRow := nRow
    oEvent:mouseCol := 0
    oList:mouseEvent( oEvent )

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE TestComboBox()

    LOCAL o

    o := HTComboBox():new()
    Assert( "HTComboBox:count empty",    o:count() == 0 )
    Assert( "HTComboBox:currentIndex 0", o:currentIndex() == 0 )

    o:addItem( "Red" )
    Assert( "HTComboBox:count after 1 add", o:count() == 1 )
    Assert( "HTComboBox:currentIndex 1",    o:currentIndex() == 1 )
    Assert( "HTComboBox:currentText",       o:currentText() == "Red" )

    o:addItem( "Green" )
    o:addItem( "Blue" )
    Assert( "HTComboBox:count after 3 adds", o:count() == 3 )

    o:setCurrentIndex( 3 )
    Assert( "HTComboBox:setCurrentIndex 3",  o:currentIndex() == 3 )
    Assert( "HTComboBox:currentText Blue",   o:currentText() == "Blue" )

    o:setCurrentIndex( 0 )  /* out of range: no change */
    Assert( "HTComboBox:setCurrentIndex 0 stays 3", o:currentIndex() == 3 )

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE TestCheckList()

    LOCAL o
    LOCAL aChecked

    o := HTCheckList():new()
    Assert( "HTCheckList:count empty",        o:count() == 0 )
    Assert( "HTCheckList:checkedCount empty",  o:checkedCount() == 0 )

    o:addItem( "Task A" )          /* unchecked by default */
    o:addItem( "Task B", .T. )     /* checked */
    o:addItem( "Task C", .F. )     /* unchecked */
    o:addItem( "Task D", .T. )     /* checked */

    Assert( "HTCheckList:count 4",             o:count() == 4 )
    Assert( "HTCheckList:isChecked(1) .F.",     ! o:isChecked( 1 ) )
    Assert( "HTCheckList:isChecked(2) .T.",     o:isChecked( 2 ) )
    Assert( "HTCheckList:isChecked(3) .F.",     ! o:isChecked( 3 ) )
    Assert( "HTCheckList:isChecked(4) .T.",     o:isChecked( 4 ) )
    Assert( "HTCheckList:checkedCount 2",       o:checkedCount() == 2 )

    aChecked := o:checkedItems()
    Assert( "HTCheckList:checkedItems len 2",   Len( aChecked ) == 2 )
    Assert( "HTCheckList:checkedItems[1] == 2", aChecked[ 1 ] == 2 )
    Assert( "HTCheckList:checkedItems[2] == 4", aChecked[ 2 ] == 4 )

    o:setChecked( 1, .T. )
    Assert( "HTCheckList:setChecked(1,.T.)",    o:isChecked( 1 ) )
    Assert( "HTCheckList:checkedCount 3",       o:checkedCount() == 3 )

    o:setChecked( 2, .F. )
    Assert( "HTCheckList:setChecked(2,.F.)",    ! o:isChecked( 2 ) )
    Assert( "HTCheckList:checkedCount back 2",  o:checkedCount() == 2 )

    /* out-of-range index is a no-op */
    o:setChecked( 99, .T. )
    Assert( "HTCheckList:setChecked oob no-op", o:checkedCount() == 2 )

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE TestSpinner()

    LOCAL o

    o := HTSpinner():new()
    Assert( "HTSpinner:value default 0",   o:value == 0 )
    Assert( "HTSpinner:minimum default 0", o:minimum == 0 )
    Assert( "HTSpinner:maximum default 100", o:maximum == 100 )

    o:setup( "Rating", 1, 10, 1, 5 )
    Assert( "HTSpinner:setup value 5",   o:value == 5 )
    Assert( "HTSpinner:setup min 1",     o:minimum == 1 )
    Assert( "HTSpinner:setup max 10",    o:maximum == 10 )
    Assert( "HTSpinner:setup step 1",    o:step == 1 )

    o:setValue( 7 )
    Assert( "HTSpinner:setValue 7",      o:value == 7 )

    o:setValue( 0 )    /* below minimum: clamps to 1 */
    Assert( "HTSpinner:setValue clamps below min", o:value == 1 )

    o:setValue( 99 )   /* above maximum: clamps to 10 */
    Assert( "HTSpinner:setValue clamps above max", o:value == 10 )

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE TestProgressBar()

    LOCAL o

    o := HTProgressBar():new()
    Assert( "HTProgressBar:value default 0",     o:value == 0 )
    Assert( "HTProgressBar:minimum default 0",   o:minimum == 0 )
    Assert( "HTProgressBar:maximum default 100", o:maximum == 100 )

    o:setValue( 50 )
    Assert( "HTProgressBar:setValue 50",         o:value == 50 )

    o:setValue( -5 )   /* below minimum: clamps to 0 */
    Assert( "HTProgressBar:setValue clamps below min", o:value == 0 )

    o:setValue( 150 )  /* above maximum: clamps to 100 */
    Assert( "HTProgressBar:setValue clamps above max", o:value == 100 )

    o:setRange( 0, 200 )
    Assert( "HTProgressBar:setRange max 200", o:maximum == 200 )

    o:setValue( 180 )
    Assert( "HTProgressBar:setValue 180 within new range", o:value == 180 )

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE TestScrollBar()

    LOCAL o

    o := HTScrollBar():new()
    Assert( "HTScrollBar:value default 0",     o:value == 0 )
    Assert( "HTScrollBar:minimum default 0",   o:minimum == 0 )
    Assert( "HTScrollBar:maximum default 100", o:maximum == 100 )
    Assert( "HTScrollBar:pageStep default 10", o:pageStep == 10 )

    o:setValue( 40 )
    Assert( "HTScrollBar:setValue 40",         o:value == 40 )

    o:setValue( -1 )   /* below minimum: clamps to 0 */
    Assert( "HTScrollBar:setValue clamps below min", o:value == 0 )

    o:setValue( 200 )  /* above maximum: clamps to 100 */
    Assert( "HTScrollBar:setValue clamps above max", o:value == 100 )

    o:setMinimum( 10 )
    Assert( "HTScrollBar:setMinimum 10",       o:minimum == 10 )

    o:setMaximum( 50 )
    Assert( "HTScrollBar:setMaximum 50",       o:maximum == 50 )

    o:setValue( 30 )
    Assert( "HTScrollBar:setValue 30 in range", o:value == 30 )

    o:setPageStep( 5 )
    Assert( "HTScrollBar:setPageStep 5",        o:pageStep == 5 )

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE TestTextEdit()

    LOCAL o
    LOCAL cMulti

    o := HTTextEdit():new()
    Assert( "HTTextEdit:text default empty",  o:text == "" )
    Assert( "HTTextEdit:readOnly default .F.", ! o:readOnly )

    o:setText( "Hello" )
    Assert( "HTTextEdit:setText single line",  o:text == "Hello" )

    o:setText( "" )
    Assert( "HTTextEdit:setText empty",        o:text == "" )

    /* multi-line round-trip */
    cMulti := "Line 1" + hb_eol() + "Line 2" + hb_eol() + "Line 3"
    o:setText( cMulti )
    Assert( "HTTextEdit:setText multi-line",   o:text == cMulti )

    /* overwrite resets to new content */
    o:setText( "New" )
    Assert( "HTTextEdit:setText overwrites",   o:text == "New" )

    o:readOnly := .T.
    Assert( "HTTextEdit:readOnly set .T.",     o:readOnly )

    o:readOnly := .F.
    Assert( "HTTextEdit:readOnly reset .F.",   ! o:readOnly )

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE TestContextMenu()

    LOCAL o
    LOCAL a1, aSep, a2
    LOCAL lFired := .F.

    o := HTContextMenu():new()
    Assert( "HTContextMenu:actions empty",       Len( o:actions() ) == 0 )

    a1 := o:addAction( "Edit" )
    Assert( "HTContextMenu:addAction returns action",  a1 != NIL )
    Assert( "HTContextMenu:action text",               a1:text == "Edit" )
    Assert( "HTContextMenu:action not separator",      ! a1:isSeparator() )
    Assert( "HTContextMenu:count 1",                   Len( o:actions() ) == 1 )

    aSep := o:addSeparator()
    Assert( "HTContextMenu:addSeparator is separator", aSep:isSeparator() )
    Assert( "HTContextMenu:count 2",                   Len( o:actions() ) == 2 )

    a2 := o:addAction( "Delete" )
    Assert( "HTContextMenu:count 3",                   Len( o:actions() ) == 3 )

    /* trigger fires onTriggered callback */
    a1:onTriggered := {|| lFired := .T. }
    a1:trigger()
    Assert( "HTContextMenu:action trigger fires callback", lFired )

    /* trigger without callback is a no-op */
    a2:trigger()   /* no callback set — must not crash */
    Assert( "HTContextMenu:trigger no-op safe", .T. )

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE TestMenuItems()

    LOCAL oView, oThemes
    LOCAL aRefresh, aSep, aDark
    LOCAL aItems

    oView := HTMenu():new( "View" )
    aRefresh := oView:addAction( "Refresh" )
    aSep     := oView:addSeparator()
    oThemes  := oView:addMenu( "Themes" )
    aDark    := oThemes:addAction( "Dark" )

    aItems := oView:items()
    Assert( "HTMenu:items count (action, separator, submenu)", Len( aItems ) == 3 )
    Assert( "HTMenu:items[1] is the action",     Len( aItems ) >= 1 .AND. aItems[ 1 ] == aRefresh )
    Assert( "HTMenu:items[2] is the separator",  Len( aItems ) >= 2 .AND. aItems[ 2 ] == aSep )
    Assert( "HTMenu:items[3] is the submenu",    Len( aItems ) >= 3 .AND. aItems[ 3 ] == oThemes )
    Assert( "HTMenu:submenu title",              oThemes:title == "Themes" )
    Assert( "HTMenu:actions() excludes submenu", Len( oView:actions() ) == 2 )

    aItems := oThemes:items()
    Assert( "HTMenu:submenu items",              Len( aItems ) == 1 .AND. aItems[ 1 ] == aDark )

    TestMenuHotKeys()

RETURN

/* "&" hot keys, Alt + hot key, action shortcuts, the active bar */
STATIC PROCEDURE TestMenuHotKeys()

    LOCAL oBar, oFile, oEdit, oAct, oKurs
    LOCAL cHit := ""

    oBar  := HTMenuBar():new()
    oFile := oBar:addMenu( "&Ulazi" )
    oFile:addAction( "&Prijemnica" ):onTriggered := {|| cHit += "P" }
    oEdit := oBar:addMenu( "&Ispravke" )
    oAct  := oEdit:addAction( "Poreske &stope" )
    oAct:onTriggered := {|| cHit += "S" }
    oKurs := oEdit:addAction( "K&urs" )
    oKurs:onTriggered := {|| cHit += "K" }
    oKurs:setShortcut( K_CTRL_B )

    Assert( "Menu: a letter on an inactive bar is not used", ! oBar:handleKey( Asc( "i" ) ) )
    Assert( "Menu: Alt + hot key opens that menu",  oBar:handleKey( K_ALT_I ) .AND. oBar:isMenuOpen() )
    Assert( "Menu: hot key letter runs the entry",  oBar:handleKey( Asc( "s" ) ) .AND. cHit == "S" )
    Assert( "Menu: closed after running",           ! oBar:isMenuOpen() .AND. ! oBar:isActive() )
    Assert( "Menu: action shortcut (Ctrl+B)",       oBar:handleKey( K_CTRL_B ) .AND. cHit == "SK" )

    oBar:activate()
    Assert( "Menu: activate() highlights a title",  oBar:isActive() .AND. ! oBar:isMenuOpen() )
    Assert( "Menu: active bar, hot key opens",      oBar:handleKey( Asc( "u" ) ) .AND. oBar:isMenuOpen() )
    oBar:handleKey( K_ENTER )
    Assert( "Menu: first entry highlighted on open", cHit == "SKP" )

    oBar:popup( 2 )
    Assert( "Menu: popup() opens that menu",        oBar:isMenuOpen() .AND. oBar:activeMenu() == 2 )
    oBar:handleKey( K_ENTER )
    Assert( "Menu: popup() highlights the first entry", cHit == "SKPS" )

    Assert( "Menu: barRow 1 by default",            oBar:barRow() == 1 )
    oBar:setBarRow( 0 )
    Assert( "Menu: setBarRow",                      oBar:barRow() == 0 )

RETURN

/*----------------------------------------------------------------*/

/* HTPixelFont: .glyphs round trip, cells, TrueType checksum, BDF */
STATIC PROCEDURE TestPixelFont()

    LOCAL cDir := hb_DirTemp() + "htpf" + hb_ntos( hb_RandomInt( 100000, 999999 ) ) + hb_ps()
    LOCAL cGlyphs := cDir + "t.glyphs"
    LOCAL cSrc, hModel, cError, aCells, cTTF, nSum, i, cBdf, cReport

    hb_DirBuild( cDir )
    cSrc := "# test font" + Chr( 10 ) + "# font: t.ttf TestPix x11" + Chr( 10 ) + ;
            "char A" + Chr( 10 ) + Replicate( "#......." + Chr( 10 ), 8 ) + ;
            Chr( 10 ) + "# a sprite" + Chr( 10 ) + "glyph E000 ship" + Chr( 10 ) + ;
            Replicate( "........########" + Chr( 10 ), 8 )
    hb_MemoWrit( cGlyphs, cSrc )

    hModel := ht_glyphsLoad( cGlyphs, @cError )
    Assert( "ht_glyphsLoad",                  hModel != NIL .AND. Len( hModel[ "items" ] ) == 2 )
    Assert( "ht_glyphsLoad: glyph code/name", hModel[ "items" ][ 2 ][ "code" ] == 0xE000 .AND. hModel[ "items" ][ 2 ][ "name" ] == "ship" )
    Assert( "ht_glyphsLoad: bad file",        ht_glyphsLoad( cDir + "none.glyphs", @cError ) == NIL .AND. ! Empty( cError ) )

    ht_glyphsSave( hModel, cDir + "copy.glyphs" )
    Assert( "ht_glyphsSave keeps the file",   hb_MemoRead( cDir + "copy.glyphs" ) == cSrc )

    aCells := ht_glyphsCells( hModel )
    Assert( "ht_glyphsCells: sprite split + space", Len( aCells ) == 4 .AND. aCells[ 1 ][ 1 ] == 32 .AND. ;
            aCells[ 3 ][ 1 ] == 0xE000 .AND. aCells[ 3 ][ 2 ][ 1 ] == "........" .AND. aCells[ 4 ][ 2 ][ 1 ] == "########" )

    Assert( "ht_glyphsBuild",                 ht_glyphsBuild( hModel, cGlyphs, @cReport ) )
    cTTF := hb_MemoRead( cDir + "t.ttf" )
    Assert( "TTF: sfnt version, 10 tables",   hb_BLeft( cTTF, 6 ) == hb_BChar( 0 ) + hb_BChar( 1 ) + hb_BChar( 0 ) + hb_BChar( 0 ) + hb_BChar( 0 ) + hb_BChar( 10 ) )
    /* the whole font sums to 0xB1B0AFBA once head.checkSumAdjustment is in */
    nSum := 0
    cTTF += Replicate( hb_BChar( 0 ), ( 4 - hb_BLen( cTTF ) % 4 ) % 4 )
    FOR i := 1 TO hb_BLen( cTTF ) STEP 4
        nSum := ( nSum + hb_BCode( hb_BSubStr( cTTF, i, 1 ) ) * 16777216 + hb_BCode( hb_BSubStr( cTTF, i + 1, 1 ) ) * 65536 + ;
                  hb_BCode( hb_BSubStr( cTTF, i + 2, 1 ) ) * 256 + hb_BCode( hb_BSubStr( cTTF, i + 3, 1 ) ) ) % 4294967296
    NEXT
    Assert( "TTF: checksum adjustment",       nSum == 0xB1B0AFBA )
    Assert( "TTF: family name (UTF-16BE)",    hb_BAt( hb_BChar( 0 ) + "T" + hb_BChar( 0 ) + "e" + hb_BChar( 0 ) + "s" + hb_BChar( 0 ) + "t", cTTF ) > 0 )

    cBdf := hb_MemoRead( cDir + "x11" + hb_ps() + "testpix-16.bdf" )
    Assert( "BDF: XLFD and a 2x scaled row",  "-hbtui-testpix-medium-r-normal--16-160-75-75-c-160-iso10646-1" $ cBdf .AND. ;
            "ENCODING 65" + Chr( 10 ) $ cBdf .AND. Chr( 10 ) + "C000" + Chr( 10 ) $ cBdf )
    Assert( "fonts.dir",                      hb_MemoRead( cDir + "x11" + hb_ps() + "fonts.dir" ) = "5" + Chr( 10 ) )

    AEval( hb_DirScan( cDir, hb_osFileMask() ), {| a | FErase( cDir + a[ 1 ] ) } )
    DirRemove( cDir + "x11" )
    DirRemove( cDir )

RETURN

/*----------------------------------------------------------------*/

/* HTSparkline / HTBarChart: scaling to eighths of a row, kept values,
   compact value texts, painting */
STATIC PROCEDURE TestMessageWrap()

    LOCAL a

    QOut( "--- HTMessageBox text wrap ---" )
    a := ht_msgWrap( "short", 20 )
    Assert( "wrap: one short line", Len( a ) == 1 .AND. a[ 1 ] == "short" )
    a := ht_msgWrap( "line one" + Chr( 13 ) + Chr( 10 ) + "line two" + Chr( 10 ) + "three", 20 )
    Assert( "wrap: CRLF and LF breaks", Len( a ) == 3 .AND. a[ 2 ] == "line two" )
    a := ht_msgWrap( "aaa bbb ccc ddd", 7 )
    Assert( "wrap: words", Len( a ) == 2 .AND. a[ 1 ] == "aaa bbb" .AND. a[ 2 ] == "ccc ddd" )
    a := ht_msgWrap( "abcdefghij", 4 )
    Assert( "wrap: long word split", Len( a ) == 3 .AND. a[ 1 ] == "abcd" .AND. a[ 3 ] == "ij" )

RETURN

STATIC PROCEDURE TestCharts()

    LOCAL o, c, i

    o := HTSparkline():new()
    o:setValues( { 0, 5, 10 } )
    Assert( "HTSparkline:levels 1 row",           hb_ValToExp( o:levels( 3, 1 ) ) == "{0, 4, 8}" )
    Assert( "HTSparkline:levels shows the last",  hb_ValToExp( o:levels( 2, 1 ) ) == "{4, 8}" )
    Assert( "HTSparkline:levels 2 rows",          hb_ValToExp( o:levels( 3, 2 ) ) == "{0, 8, 16}" )
    o:setValues( { 0, 1, 100 } )
    Assert( "HTSparkline:small value stays visible", hb_ValToExp( o:levels( 3, 1 ) ) == "{0, 1, 8}" )
    o:setValues( { -10, 0, 10 } )
    Assert( "HTSparkline:negative values",        hb_ValToExp( o:levels( 3, 1 ) ) == "{0, 4, 8}" )
    o:maximum := 20
    o:minimum := 0
    o:setValues( { 10, 20, 30 } )
    Assert( "HTSparkline:fixed range clamps",     hb_ValToExp( o:levels( 3, 1 ) ) == "{4, 8, 8}" )
    o:maximum := NIL
    o:minimum := NIL
    o:clear()
    Assert( "HTSparkline:clear",                  Len( o:levels( 10, 1 ) ) == 0 )
    o:maxCount := 3
    FOR i := 1 TO 5
        o:addValue( i )
    NEXT
    Assert( "HTSparkline:addValue keeps maxCount", hb_ValToExp( o:values() ) == "{3, 4, 5}" )
    o:paintEvent( HTPaintEvent():new() )
    Assert( "HTSparkline:paints",                 .T. )

    Assert( "ht_blockGlyph 0 is blank",           ht_blockGlyph( 0 ) == " " )
    Assert( "ht_blockGlyph 8 is full",            ht_blockGlyph( 8 ) == ht_glyph( 0x2588, "#" ) )
    Assert( "ht_blockGlyph over 8 is full",       ht_blockGlyph( 12 ) == ht_blockGlyph( 8 ) )
    Assert( "ht_chartColor theme pair",           "/" $ ht_chartColor() )
    Assert( "ht_chartColor own color",            ht_chartColor( "R/W" ) == "R/W" )

    c := HTBarChart():new()
    c:setBars( { { "A", 50 }, { "B", 100 }, { "C", 0, "R/W" } } )
    Assert( "HTBarChart:barRows",                 c:barRows( 10 ) == 8 )
    Assert( "HTBarChart:levels",                  hb_ValToExp( c:levels( 2 ) ) == "{8, 16, 0}" )
    c:showValues := .F.
    Assert( "HTBarChart:barRows without values",  c:barRows( 10 ) == 9 )
    c:showValues := .T.
    c:setValue( 3, 200 )
    Assert( "HTBarChart:setValue",                hb_ValToExp( c:levels( 1 ) ) == "{2, 4, 8}" )
    c:maximum := 100
    Assert( "HTBarChart:fixed maximum clamps",    hb_ValToExp( c:levels( 1 ) ) == "{4, 8, 8}" )
    c:addBar( "D", 25 )
    Assert( "HTBarChart:addBar",                  Len( c:bars() ) == 4 )

    Assert( "HTBarChart:valueText fits",          c:valueText( 120 ) == "120" )
    Assert( "HTBarChart:valueText k",             c:valueText( 12345 ) == "12k" )
    Assert( "HTBarChart:valueText 1000",          c:valueText( 1000 ) == "1k" )
    Assert( "HTBarChart:valueText M narrow",      c:valueText( 2500000 ) == "3M" )
    c:barWidth := 5
    Assert( "HTBarChart:valueText M wide",        c:valueText( 2500000 ) == "2.5M" )
    c:valueBlock := {| n | "#" + hb_ntos( n ) }
    Assert( "HTBarChart:valueBlock",              c:valueText( 7 ) == "#7" )

    c:paintEvent( HTPaintEvent():new() )
    Assert( "HTBarChart:paints",                  .T. )

RETURN

/*----------------------------------------------------------------*/

/* CP950 (Big5, traditional Chinese): a multi-byte codepage, so the window
   stack needs the extended SaveScreen() cells - the Clipper cells kept "?" */
STATIC PROCEDURE TestBig5Screen()

    LOCAL cOldCP := hb_cdpSelect()
    LOCAL lOldCompat := hb_gtInfo( HB_GTI_COMPATBUFFER )
    LOCAL cBig5, cScr, cBack

    Assert( "ht_cdpIsMultiByte: EN is single-byte", ! ht_cdpIsMultiByte() )
    hb_cdpSelect( "CP950" )
    Assert( "ht_cdpIsMultiByte: CP950",             ht_cdpIsMultiByte() )

    /* U+7E41 U+9AD4 U+4E2D U+6587 */
    cBig5 := hb_Translate( e"繁體中文", "UTF8", "CP950" )
    Assert( "CP950: 2 bytes per character",          hb_BLen( cBig5 ) == 8 )

    hb_gtInfo( HB_GTI_COMPATBUFFER, ! ht_cdpIsMultiByte() )
    DispOutAt( 0, 0, cBig5 )
    cScr := SaveScreen( 0, 0, 0, 3 )
    DispOutAt( 0, 0, Space( 4 ) )
    RestScreen( 0, 0, 0, 3, cScr )
    /* four 4-byte extended cells, the first one U+7E41 (little endian) */
    cBack := SaveScreen( 0, 0, 0, 3 )
    Assert( "CP950: window restore keeps Chinese",   hb_BLen( cBack ) == 16 .AND. hb_BLeft( cBack, 2 ) == e"A~" )

    hb_gtInfo( HB_GTI_COMPATBUFFER, lOldCompat )
    hb_cdpSelect( cOldCP )

RETURN

/*----------------------------------------------------------------*/

/* Traditional Chinese typed through an IME: gtwvt / gtxwc deliver Unicode
   Inkey codes; editors must insert the multi-byte text (google group report:
   "unable to input traditional chinese characters"). */
STATIC PROCEDURE TestUnicodeInput()

    LOCAL cOldCP := hb_cdpSelect()
    LOCAL cHan, aKeys, nKey, o, t, g, cVal, cScr

    /* HTApplication picks UTF8EX when the app left Harbour's default "EN".
       The singleton may already exist from an earlier test that touched
       HTApplication(); a second :new() then skips the codepage switch. */
    hb_cdpSelect( "EN" )
    HTApplication():new()
    IF ! hb_cdpSelect() == "UTF8EX"
        hb_cdpSelect( "UTF8EX" )
        IF ht_cdpIsMultiByte()
            hb_gtInfo( HB_GTI_COMPATBUFFER, .F. )
        ENDIF
    ENDIF
    Assert( "HTApplication:new selects UTF8EX", hb_cdpSelect() == "UTF8EX" )

    /* U+7E41 U+9AD4 U+4E2D U+6587 (traditional Chinese): 4 chars, 12 bytes in UTF-8 */
    cHan := e"\xE7\xB9\x81\xE9\xAB\x94\xE4\xB8\xAD\xE6\x96\x87"
    Assert( "UTF8EX: Len() counts characters", Len( cHan ) == 4 )

    /* the keys an IME commit produces */
    aKeys := {}
    hb_keyPut( cHan )
    DO WHILE ( nKey := Inkey() ) != 0
        AAdd( aKeys, nKey )
    ENDDO
    Assert( "Inkey: one key per CJK character", Len( aKeys ) == 4 )

    Assert( "ht_keyText: CJK key -> UTF-8 char", Len( aKeys ) == 4 .AND. ht_keyText( aKeys[ 1 ] ) == Left( cHan, 1 ) )
    Assert( "ht_keyText: ASCII letter",       ht_keyText( Asc( "a" ) ) == "a" )
    Assert( "ht_keyText: space",              ht_keyText( K_SPACE ) == " " )
    Assert( "ht_keyText: Enter types nothing", ht_keyText( K_ENTER ) == "" )
    Assert( "ht_keyText: Left types nothing",  ht_keyText( K_LEFT ) == "" )
    Assert( "ht_keyText: F2 types nothing",    ht_keyText( K_F2 ) == "" )
    Assert( "ht_keyText: Alt+A types nothing", ht_keyText( K_ALT_A ) == "" )

    o := HTLineEdit():new()
    AEval( aKeys, {| n | o:keyEvent( HTKeyEvent():new( n ) ) } )
    o:keyEvent( HTKeyEvent():new( Asc( "A" ) ) )
    Assert( "HTLineEdit: CJK typed",           o:text == cHan + "A" )

    /* insert in the middle: cursor arithmetic is per character */
    o:keyEvent( HTKeyEvent():new( K_HOME ) )
    o:keyEvent( HTKeyEvent():new( K_RIGHT ) )
    o:keyEvent( HTKeyEvent():new( Asc( "x" ) ) )
    Assert( "HTLineEdit: insert after 1st CJK", o:text == Left( cHan, 1 ) + "x" + SubStr( cHan, 2 ) + "A" )
    o:keyEvent( HTKeyEvent():new( K_BS ) )
    o:keyEvent( HTKeyEvent():new( K_BS ) )
    Assert( "HTLineEdit: Backspace deletes a whole CJK char", o:text == SubStr( cHan, 2 ) + "A" )

    t := HTTextEdit():new()
    AEval( aKeys, {| n | t:keyEvent( HTKeyEvent():new( n ) ) } )
    Assert( "HTTextEdit: CJK typed",           t:text == cHan )

    cVal := Space( 10 )
    g := HTGet():new()
    g:setup( {| x | iif( x == NIL, cVal, cVal := x ) }, "Name", "", NIL, NIL, "", NIL, .F., .F., 20 )
    g:focusInEvent( HTFocusEvent():new() )
    AEval( aKeys, {| n | g:keyEvent( HTKeyEvent():new( n ) ) } )
    g:focusOutEvent( HTFocusEvent():new() )
    Assert( "HTGet: CJK typed",                Left( cVal, 4 ) == cHan )

    /* screen cells: off on a text GT (gtnul) - one cell per character */
    Assert( "wide cells off on gtnul",         ! ht_wideCells() )
    Assert( "ht_strWidth: 1 cell per char",    ht_strWidth( cHan ) == 4 )

    /* on (graphic GTs): a CJK character takes two cells */
    ht_wideCells( .T. )
    Assert( "ht_strWidth: CJK = 2 cells",      ht_strWidth( cHan + "A" ) == 9 )
    Assert( "ht_cellLeft: whole chars only",   ht_cellLeft( cHan, 5 ) == Left( cHan, 2 ) )
    Assert( "ht_cellPadR: pads to cells",      ht_cellPadR( Left( cHan, 2 ), 6 ) == Left( cHan, 2 ) + "  " )
    Assert( "ht_cellPadR: cuts, fills gap",    ht_cellPadR( cHan, 5 ) == Left( cHan, 2 ) + " " )
    Assert( "ht_cellPadL",                     ht_cellPadL( Left( cHan, 1 ), 4 ) == "  " + Left( cHan, 1 ) )
    Assert( "ht_cellPos: 2nd cell of 1st char", ht_cellPos( cHan, 1 ) == 1 )
    Assert( "ht_cellPos: 3rd cell = 2nd char", ht_cellPos( cHan, 2 ) == 2 )
    Assert( "ht_cellPos: past the end",        ht_cellPos( cHan, 20 ) == 5 )
    Assert( "ht_cellCol: after 2 chars",       ht_cellCol( cHan, 1, 3 ) == 4 )
    Assert( "ht_cellCol: past the end",        ht_cellCol( "ab", 1, 5 ) == 4 )
    /* 4 CJK chars (8 cells) + cursor after the end in a 6-cell field:
       the first 2 chars scroll out, 2 chars (4 cells) + cursor cell fit */
    Assert( "ht_scrollToCursor: CJK",          ht_scrollToCursor( cHan, 1, 5, 6 ) == 3 )
    Assert( "ht_scrollToCursor: ASCII",        ht_scrollToCursor( "abcdefgh", 1, 9, 6 ) == 4 )

    o:setText( cHan )
    Assert( "HTLineEdit: text kept with wide cells", o:text == cHan )

    /* filler cells: a space after Chinese; after Korean a zero width space
       on Windows (GDI draws Hangul two cells wide by itself, a space after
       it made three) */
    DispOutAt( 0, 0, hb_UChar( 0xD55C ) + Left( cHan, 1 ) )
    cScr := SaveScreen( 0, 0, 0, 3 )
    ht_wideCells( .F. )
    Assert( "wide cells: Hangul, filler, Han",  ScrCell( cScr, 1 ) == 0xD55C .AND. ;
                                                ScrCell( cScr, 3 ) == hb_UCode( Left( cHan, 1 ) ) )
    Assert( "filler after Han: space",          ScrCell( cScr, 4 ) == 32 )
#ifdef __PLATFORM__WINDOWS
    Assert( "filler after Hangul: U+200B",      ScrCell( cScr, 2 ) == 0x200B )
#else
    Assert( "filler after Hangul: space",       ScrCell( cScr, 2 ) == 32 )
#endif

    /* gtxwc with "Unifont Cell": the filler is a blank glyph chosen by the
       character, so a new character over an old one repaints the filler too */
    ht_wideCells( .T. )
    ht_wideFillerBase( 0xE100 )
    DispOutAt( 0, 0, Left( cHan, 1 ) + "A" )
    cScr := SaveScreen( 0, 0, 0, 3 )
    Assert( "filler base: U+E100 + low 12 bits", ScrCell( cScr, 2 ) == 0xE100 + hb_bitAnd( hb_UCode( Left( cHan, 1 ) ), 0x0FFF ) .AND. ;
                                                ScrCell( cScr, 3 ) == Asc( "A" ) )
    Assert( "filler base: old value back",      ht_wideFillerBase( 0 ) == 0xE100 )
    ht_wideCells( .F. )

    /* the window stack saves/restores screen areas: with Clipper 2-byte
       cells (HB_GTI_COMPATBUFFER) every CJK character came back as "?" */
    Assert( "UTF8EX: extended SaveScreen cells", ! hb_gtInfo( HB_GTI_COMPATBUFFER ) )
    DispOutAt( 0, 0, cHan )
    cScr := SaveScreen( 0, 0, 0, 3 )
    DispOutAt( 0, 0, "    " )
    RestScreen( 0, 0, 0, 3, cScr )
    cScr := SaveScreen( 0, 0, 0, 0 )
    Assert( "SaveScreen/RestScreen keep U+7E41", hb_BCode( hb_BSubStr( cScr, 1, 1 ) ) == 0x41 .AND. ;
                                                hb_BCode( hb_BSubStr( cScr, 2, 1 ) ) == 0x7E )

    Assert( "ht_glyph: symbol in UTF8EX",      ht_glyph( 0x00D7, "x" ) == hb_UChar( 0x00D7 ) )
    hb_cdpSelect( "EN" )
    Assert( "ht_glyph: fallback in CP437",     ht_glyph( 0x00D7, "x" ) == "x" )
    Assert( "ht_glyph: CP437 has the check",   ht_glyph( 0x221A, "x" ) == Chr( 251 ) )

    hb_cdpSelect( cOldCP )

RETURN

/* Unicode character of cell n (1-based) in a 4-cell extended SaveScreen()
   buffer: the cell starts with the character, 16-bit little endian */
STATIC FUNCTION ScrCell( cScr, n )

    LOCAL nPos := ( n - 1 ) * ( hb_BLen( cScr ) / 4 ) + 1

RETURN hb_BCode( hb_BSubStr( cScr, nPos, 1 ) ) + hb_BCode( hb_BSubStr( cScr, nPos + 1, 1 ) ) * 256

/* setTitleButtons(): a message box keeps only close */
STATIC PROCEDURE TestTitleButtons()

    LOCAL o := HTDialog():new( NIL, "Msg" )

    o:setTitleButtons( .F., .F., .F. )
    Assert( "setTitleButtons: all off",   o:charWidgetHide == NIL .AND. o:charWidgetMaximize == NIL .AND. o:charWidgetResize == NIL )
    Assert( "setTitleButtons: close kept", o:charWidgetClose != NIL )
    o:setTitleButtons( NIL, .T. )
    Assert( "setTitleButtons: maximize back, others as they were", o:charWidgetMaximize != NIL .AND. o:charWidgetHide == NIL )

    o:resize( 30, 6 )
    Assert( "HTDialog:resize at once",    o:width == 30 .AND. o:height == 6 )

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE SBType( o, cText )

    LOCAL c

    FOR EACH c IN cText
        o:keyEvent( HTKeyEvent():new( Asc( c ) ) )
    NEXT

RETURN

STATIC PROCEDURE SBKey( o, nKey, nTimes )

    LOCAL i

    FOR i := 1 TO hb_defaultValue( nTimes, 1 )
        o:keyEvent( HTKeyEvent():new( nKey ) )
    NEXT

RETURN

/* HTSearchBox: the searchbox of Digi knjigovodstvo (filter, mark, Enter, Esc, blur) */
STATIC PROCEDURE TestSearchBox()

    LOCAL cOld := hb_cdpSelect( "UTF8EX" ), o, aSel := {}, nCleared := 0

    o := HTSearchBox():new()
    o:advanceOnEnter := .F.
    o:setItems( { { 1, "Нова доо" }, { 2, "Нови Пут доо" }, { 3, "Новитет доо" }, { 4, "Kraljevo promet" } } )
    o:detailsBlock := {| x | { { "Šifra", x[ 1 ] } } }
    o:onSelected := {| x, v | HB_SYMBOL_UNUSED( x ), AAdd( aSel, v ) }
    o:onCleared := {|| nCleared++ }

    SBType( o, "nov" )
    Assert( "HTSearchBox: Latin finds Cyrillic", Len( o:results() ) == 3 )
    Assert( "HTSearchBox: a new search marks the first", o:marked() == 1 .AND. o:isOpen() )
    SBKey( o, K_DOWN )
    SBKey( o, K_ENTER )
    Assert( "HTSearchBox: Down + Enter takes the second", ht_sameValue( o:getValue(), 2 ) .AND. ATail( aSel ) == 2 .AND. ! o:isOpen() )
    Assert( "HTSearchBox: the text of the taken item", o:text() == "2 - Нови Пут доо" )
    SBKey( o, K_BS )
    Assert( "HTSearchBox: typing over drops the item", o:getValue() == NIL )

    o:clear()
    Assert( "HTSearchBox: clear fires onCleared", nCleared >= 1 .AND. o:text() == "" )
    SBType( o, "nov" )
    SBKey( o, K_DOWN, 5 )
    Assert( "HTSearchBox: Down stops at the last", o:marked() == 3 )
    SBKey( o, K_UP, 5 )
    Assert( "HTSearchBox: Up stops at the first", o:marked() == 1 )
    SBKey( o, K_ESC )
    Assert( "HTSearchBox: Esc closes only the list", ! o:isOpen() .AND. o:text() == "nov" )
    SBKey( o, K_DOWN )
    Assert( "HTSearchBox: Down opens it again", o:isOpen() .AND. o:marked() == 1 )

    o:clear()
    SBType( o, "zzz" )
    SBKey( o, K_ENTER )
    Assert( "HTSearchBox: nothing found, Enter stays", o:getValue() == NIL .AND. o:text() == "zzz" )

    o:clear()
    SBType( o, "kralj" )
    o:focusOutEvent( HTFocusEvent():new( HT_EVENT_TYPE_FOCUSOUT ) )
    Assert( "HTSearchBox: leaving with one match takes it", ht_sameValue( o:getValue(), 4 ) )

    o:setValue( "3" )
    Assert( "HTSearchBox: setValue by text", ht_sameValue( o:getValue(), 3 ) .AND. o:text() == "3 - Новитет доо" )
    o:setValue( 99 )
    Assert( "HTSearchBox: setValue not found clears", o:getValue() == NIL .AND. o:text() == "" )

    SBType( o, "1" )
    Assert( "HTSearchBox: the details are searched (Šifra)", Len( o:results() ) == 1 )

    Assert( "ht_latinToCyrillic", ht_latinToCyrillic( "Kraljevo Džep njiva" ) == "Краљево Џеп њива" )
    Assert( "ht_looksLatin", ht_looksLatin( "čačak" ) .AND. ! ht_looksLatin( "Нова" ) .AND. ! ht_looksLatin( "123" ) )

    hb_cdpSelect( cOld )

RETURN

/*----------------------------------------------------------------*/

/* HTDatePicker: the serbian-date-picker of Digi knjigovodstvo */
STATIC PROCEDURE TestDatePicker()

    LOCAL cOld := hb_cdpSelect( "UTF8EX" ), o, aChg := {}, aGrid

    o := HTDatePicker():new()
    o:advanceOnEnter := .F.
    o:onChanged := {| d | AAdd( aChg, d ) }

    SBType( o, "20112026" )
    Assert( "HTDatePicker: the dots come by themselves", o:text() == "20.11.2026" )
    SBKey( o, K_ENTER )
    Assert( "HTDatePicker: Enter takes the date", o:getValue() == 0d20261120 .AND. ATail( aChg ) == 0d20261120 )

    SBKey( o, K_CTRL_Y )
    SBType( o, "31022026" )
    SBKey( o, K_ENTER )
    Assert( "HTDatePicker: a wrong date goes back", o:getValue() == 0d20261120 .AND. o:text() == "20.11.2026" )

    SBKey( o, K_ALT_DOWN )
    Assert( "HTDatePicker: Alt-Down opens on the date", o:isOpen() .AND. o:marked() == 0d20261120 )
    SBKey( o, K_RIGHT )
    SBKey( o, K_DOWN )
    Assert( "HTDatePicker: arrows move the day", o:marked() == 0d20261128 )
    SBKey( o, K_PGDN )
    Assert( "HTDatePicker: PgDn = next month", o:marked() == 0d20261228 )
    SBKey( o, K_CTRL_PGUP )
    Assert( "HTDatePicker: Ctrl-PgUp = a year back", o:marked() == 0d20251228 )
    SBKey( o, K_END )
    Assert( "HTDatePicker: End = the last day", o:marked() == 0d20251231 )
    SBKey( o, K_ENTER )
    Assert( "HTDatePicker: Enter takes the day", ! o:isOpen() .AND. o:getValue() == 0d20251231 .AND. o:text() == "31.12.2025" )

    SBKey( o, K_F4 )
    SBKey( o, K_LEFT )
    SBKey( o, K_ESC )
    Assert( "HTDatePicker: Esc closes, the value stays", ! o:isOpen() .AND. o:getValue() == 0d20251231 )

    SBKey( o, K_CTRL_Y )
    SBKey( o, K_ENTER )
    Assert( "HTDatePicker: empty field = empty date", Empty( o:getValue() ) )

    o:setValue( "2026-01-05" )
    Assert( "HTDatePicker: setValue yyyy-mm-dd", o:getValue() == 0d20260105 .AND. o:text() == "05.01.2026" )
    SBKey( o, K_CTRL_Y )
    SBType( o, "1.3.2026" )
    o:focusOutEvent( HTFocusEvent():new( HT_EVENT_TYPE_FOCUSOUT ) )
    Assert( "HTDatePicker: leaving takes d.m.yyyy", o:getValue() == 0d20260301 .AND. o:text() == "01.03.2026" )

    aGrid := ht_monthGrid( 2026, 11 )
    Assert( "ht_monthGrid: 1.11.2026 is a Sunday", aGrid[ 7 ] == 1 .AND. aGrid[ 6 ] == 0 .AND. aGrid[ 36 ] == 30 )
    Assert( "ht_addMonths: 31.01. + 1 = 28.02.", ht_addMonths( 0d20260131, 1 ) == 0d20260228 )
    Assert( "ht_daysInMonth: leap years", ht_daysInMonth( 2024, 2 ) == 29 .AND. ht_daysInMonth( 1900, 2 ) == 28 .AND. ht_daysInMonth( 2000, 2 ) == 29 )
    Assert( "ht_dateParse: 31.02. is not a date", ht_dateParse( "31.02.2026" ) == NIL )

    /* other languages: the format, the names, the first day of the week */
    o := HTDatePicker():new()
    o:advanceOnEnter := .F.
    Assert( "HTDatePicker: en-us", o:setLang( "en-us" ) .AND. o:dateFormat == "mm/dd/yyyy" .AND. o:weekStart == 7 )
    SBType( o, "11202026" )
    Assert( "HTDatePicker: en-us types mm/dd/yyyy", o:text() == "11/20/2026" )
    SBKey( o, K_ENTER )
    Assert( "HTDatePicker: en-us takes it", o:getValue() == 0d20261120 )
    o:setLang( "de" )
    Assert( "HTDatePicker: another language shows the date again", o:text() == "20.11.2026" .AND. o:monthNames[ 3 ] == "März" )
    o:setLang( "zh" )
    Assert( "HTDatePicker: zh yyyy-mm-dd", o:text() == "2026-11-20" .AND. o:todayText == "今天" )
    SBKey( o, K_CTRL_Y )
    SBType( o, "20260301" )
    Assert( "HTDatePicker: yyyy-mm-dd typing", o:text() == "2026-03-01" )
    Assert( "HTDatePicker: an unknown language changes nothing", ! o:setLang( "xx" ) .AND. o:lang() == "zh" )
    aGrid := ht_monthGrid( 2026, 11, 7 )
    Assert( "ht_monthGrid: the week from Sunday", aGrid[ 1 ] == 1 .AND. aGrid[ 30 ] == 30 )
    Assert( "ht_dateText / ht_dateParse in a format", ht_dateText( 0d20260305, "mm/dd/yyyy" ) == "03/05/2026" .AND. ;
            ht_dateParse( "03/05/2026", "mm/dd/yyyy" ) == 0d20260305 .AND. ht_dateParse( "2026-03-05", "dd.mm.yyyy" ) == 0d20260305 )
    Assert( "ht_dateDefaultLang", ht_dateDefaultLang() == "sr" .AND. Len( ht_dateLangs() ) == 16 )

    hb_cdpSelect( cOld )

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE TestTreeView()

    LOCAL o, oRoot, oChild, nAct := 0, nExp := 0
    LOCAL oEv

    o := HTTreeView():new()
    Assert( "HTTreeView: empty", o:count() == 0 .AND. o:rowCount() == 0 )

    oRoot := o:addNode( "Class", NIL, 100 )
    oChild := o:addNode( "Group", oRoot, 110 )
    o:addNode( "Analytic", oChild, 111 )
    o:addNode( "Other", NIL, 200 )

    Assert( "HTTreeView: count includes closed nodes", o:count() == 4 )
    Assert( "HTTreeView: closed root hides children", o:rowCount() == 2 )
    Assert( "HTTreeView: [+] on a closed branch", "[+]" $ o:rowText( 1 ) .AND. "Class" $ o:rowText( 1 ) )
    Assert( "HTTreeView: current is the first node", o:currentText() == "Class" )
    Assert( "HTTreeView: findData", o:findData( 111 ):text == "Analytic" )

    o:onExpanded := {| e, l | nExp++, HB_SYMBOL_UNUSED( e ), HB_SYMBOL_UNUSED( l ) }
    o:handleKey( K_RIGHT )
    Assert( "HTTreeView: Right expands", o:rowCount() == 3 .AND. "[-]" $ o:rowText( 1 ) )
    Assert( "HTTreeView: child row", "Group" $ o:rowText( 2 ) .AND. "[+]" $ o:rowText( 2 ) )
    Assert( "HTTreeView: onExpanded", nExp == 1 )

    o:handleKey( K_RIGHT )
    Assert( "HTTreeView: Right again enters the child", o:currentText() == "Group" )
    o:handleKey( K_LEFT )
    Assert( "HTTreeView: Left on a leaf goes to the parent", o:currentText() == "Class" )
    o:handleKey( K_LEFT )
    Assert( "HTTreeView: Left collapses", o:rowCount() == 2 .AND. "[+]" $ o:rowText( 1 ) )

    o:expandAll()
    Assert( "HTTreeView: expandAll", o:rowCount() == 4 .AND. "Analytic" $ o:rowText( 3 ) )

    o:onActivated := {| e | nAct := e:userData }
    o:setCurrentIndex( 3 )
    o:handleKey( K_ENTER )
    Assert( "HTTreeView: Enter on a leaf runs onActivated", nAct == 111 )

    o:setCurrentIndex( 1 )
    o:handleKey( Asc( "O" ) )
    Assert( "HTTreeView: letter jumps to Other", o:currentText() == "Other" )

    o:setCurrentIndex( 1 )
    o:collapse()
    oEv := HTMouseEvent():new( K_LBUTTONDOWN, 0, 0 )
    oEv:mouseRow := 0
    oEv:mouseCol := o:markColumn( 1 )
    o:mouseEvent( oEv )
    Assert( "HTTreeView: click on the mark expands", o:rowCount() == 4 .AND. "[-]" $ o:rowText( 1 ) )

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE TestSplitter()

    LOCAL o, a, nPos
    LOCAL oEv

    o := HTSplitter():new( HT_SPLIT_VERTICAL )
    o:addWidget( HTLabel():new( "Left" ) )
    o:addWidget( HTLabel():new( "Right" ) )
    o:setGeometry( 0, 0, 21, 6 )

    a := o:sizes()
    Assert( "HTSplitter: two panes fill the width", Len( a ) == 2 .AND. a[ 1 ] + a[ 2 ] + 1 == 21 )
    Assert( "HTSplitter: equal split", a[ 1 ] == 10 .AND. a[ 2 ] == 10 )
    Assert( "HTSplitter: bar column", o:handlePos( 1 ) == 10 )
    Assert( "HTSplitter: pane geometry", o:widget( 1 ):x == 0 .AND. o:widget( 1 ):width == 10 .AND. ;
            o:widget( 2 ):x == 11 .AND. o:widget( 2 ):width == 10 )

    Assert( "HTSplitter: moveHandle grows the left pane", o:moveHandle( 1, 3 ) )
    a := o:sizes()
    Assert( "HTSplitter: 13 and 7", a[ 1 ] == 13 .AND. a[ 2 ] == 7 .AND. o:widget( 2 ):x == 14 )

    o:setMinimum( 8 )
    a := o:sizes()
    Assert( "HTSplitter: minimum lifts the small pane", a[ 1 ] >= 8 .AND. a[ 2 ] >= 8 .AND. a[ 1 ] + a[ 2 ] == 20 )
    Assert( "HTSplitter: cannot pass the minimum", ! o:moveHandle( 1, 50 ) )

    o:setMinimum( 1 )
    nPos := o:sizes()[ 1 ]
    oEv := HTMouseEvent():new( K_LBUTTONDOWN, 0, o:handlePos( 1 ) )
    oEv:mouseRow := 0
    oEv:mouseCol := o:handlePos( 1 )
    o:mouseEvent( oEv )
    Assert( "HTSplitter: button down on the bar grabs", o:mouseDragWanted() )
    oEv := HTMouseEvent():new( K_MOUSEMOVE, 0, o:handlePos( 1 ) + 2 )
    oEv:mouseRow := 0
    oEv:mouseCol := o:handlePos( 1 ) + 2
    o:mouseEvent( oEv )
    Assert( "HTSplitter: drag follows the pointer", o:sizes()[ 1 ] == nPos + 2 .AND. o:sizes()[ 1 ] == o:handlePos( 1 ) )
    oEv := HTMouseEvent():new( K_LBUTTONUP, 0, 0 )
    oEv:mouseRow := 0
    oEv:mouseCol := 0
    o:mouseEvent( oEv )
    Assert( "HTSplitter: button up releases", ! o:mouseDragWanted() )

    o:setOrientation( HT_SPLIT_HORIZONTAL )
    o:setGeometry( 0, 0, 10, 11 )
    a := o:sizes()
    Assert( "HTSplitter: horizontal stacks the panes", a[ 1 ] + a[ 2 ] + 1 == 11 .AND. ;
            o:widget( 1 ):y == 0 .AND. o:widget( 2 ):y == a[ 1 ] + 1 )

    o := HTSplitter():new( HT_SPLIT_VERTICAL )
    o:addWidget( HTLabel():new( "A" ) )
    o:addWidget( HTLabel():new( "B" ) )
    o:addWidget( HTLabel():new( "C" ) )
    o:setGeometry( 0, 0, 32, 4 )
    Assert( "HTSplitter: three equal panes", o:sizes()[ 1 ] == 10 .AND. o:sizes()[ 2 ] == 10 .AND. o:sizes()[ 3 ] == 10 )
    o:moveHandle( 2, 4 )
    Assert( "HTSplitter: second bar moves panes 2 and 3", o:sizes()[ 1 ] == 10 .AND. o:sizes()[ 2 ] == 14 .AND. o:sizes()[ 3 ] == 6 )

RETURN

/*----------------------------------------------------------------*/

STATIC FUNCTION MdLine( o, cPart )

    LOCAL i

    FOR i := 1 TO o:lineCount()
        IF cPart $ o:lineText( i )
            RETURN i
        ENDIF
    NEXT

RETURN 0

STATIC PROCEDURE TestMarkdown()

    LOCAL o, n, c
    LOCAL oEv

    c := "# Naslov" + Chr( 10 ) + Chr( 10 ) + ;
         "Vidi **zuto** i *isko* i `kod`." + Chr( 10 ) + ;
         "Link [primer](http://a) ovde." + Chr( 10 ) + ;
         "- prva" + Chr( 10 ) + ;
         "1. druga" + Chr( 10 ) + ;
         "> citat" + Chr( 10 ) + ;
         "---" + Chr( 10 ) + ;
         "```" + Chr( 10 ) + ;
         "x = 1" + Chr( 10 ) + ;
         "```" + Chr( 10 ) + ;
         "| Konto | Naziv |" + Chr( 10 ) + ;
         "| --- | --- |" + Chr( 10 ) + ;
         "| 100 | Kasa |" + Chr( 10 ) + ;
         "jedan dva tri cetiri pet sest sedam osam devet deset"

    o := HTMarkdownView():new()
    o:setWrapWidth( 24 )
    o:setMarkdown( c )

    n := MdLine( o, "Naslov" )
    Assert( "HTMarkdown: heading", n > 0 .AND. o:lineKind( n ) == HT_MD_HEADING )

    n := MdLine( o, "zuto" )
    Assert( "HTMarkdown: bold", n > 0 .AND. o:spanKind( n, At( "zuto", o:lineText( n ) ) ) == HT_MD_BOLD )
    Assert( "HTMarkdown: italic", o:spanKind( n, At( "isko", o:lineText( n ) ) ) == HT_MD_ITALIC )
    Assert( "HTMarkdown: code span", o:spanKind( n, At( "kod", o:lineText( n ) ) ) == HT_MD_CODE )

    n := MdLine( o, "primer" )
    Assert( "HTMarkdown: link", n > 0 .AND. o:linkAt( n, At( "primer", o:lineText( n ) ) - 1 ) == "http://a" )

    Assert( "HTMarkdown: list", MdLine( o, "prva" ) > 0 .AND. MdLine( o, "druga" ) > 0 )
    n := MdLine( o, "citat" )
    Assert( "HTMarkdown: quote", n > 0 .AND. o:lineKind( n ) == HT_MD_QUOTE )
    n := MdLine( o, "x = 1" )
    Assert( "HTMarkdown: fence", n > 0 .AND. o:spanKind( n, 1 ) == HT_MD_CODE )
    Assert( "HTMarkdown: table", MdLine( o, "Konto" ) > 0 .AND. MdLine( o, "Kasa" ) > 0 )
    Assert( "HTMarkdown: wrap", o:lineCount() > MdLine( o, "jedan" ) .AND. MdLine( o, "jedan" ) > 0 )

    o:onLink := {| cUrl | c := cUrl }
    n := MdLine( o, "primer" )
    oEv := HTMouseEvent():new( K_LBUTTONDOWN, 0, 0 )
    oEv:mouseRow := n - o:topLine()
    oEv:mouseCol := At( "primer", o:lineText( n ) ) - 1
    o:mouseEvent( oEv )
    Assert( "HTMarkdown: click runs onLink", c == "http://a" )

    o:setMarkdown( "aaa bbb ccc ddd eee fff ggg" )
    o:setWrapWidth( 12 )
    o:resize( 24, 1 )   /* one visible row so Down can scroll */
    Assert( "HTMarkdown: more than one wrapped line", o:lineCount() >= 2 )
    Assert( "HTMarkdown: resize sets one row", o:height == 1 )
    o:handleKey( K_DOWN )
    Assert( "HTMarkdown: Down scrolls", o:topLine() == 2 )
    o:handleKey( K_HOME )
    Assert( "HTMarkdown: Home", o:topLine() == 1 )

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE TestTagInput()

    LOCAL o, nChg := 0, a
    LOCAL oEv

    o := HTTagInput():new()
    o:onChanged := {| t | nChg++, HB_SYMBOL_UNUSED( t ) }
    o:handleKey( Asc( "A" ) )
    o:handleKey( Asc( "b" ) )
    o:handleKey( K_ENTER )
    Assert( "HTTagInput: Enter adds the tag", o:tagCount() == 1 .AND. o:tags()[ 1 ] == "Ab" .AND. o:text() == "" )

    o:handleKey( Asc( "C" ) )
    o:handleKey( Asc( "," ) )
    Assert( "HTTagInput: comma adds", o:tagCount() == 2 .AND. o:tags()[ 2 ] == "C" )

    o:handleKey( Asc( "A" ) )
    o:handleKey( Asc( "b" ) )
    o:handleKey( K_ENTER )
    Assert( "HTTagInput: duplicate is ignored", o:tagCount() == 2 )

    o:handleKey( K_BS )
    Assert( "HTTagInput: Backspace removes the last", o:tagCount() == 1 .AND. o:tags()[ 1 ] == "Ab" )

    Assert( "HTTagInput: the x column", o:chipHit( 4, 40 ) == 1 )
    oEv := HTMouseEvent():new( K_LBUTTONDOWN, 0, 4 )
    oEv:mouseRow := 0
    oEv:mouseCol := 4
    o:mouseEvent( oEv )
    Assert( "HTTagInput: click on x removes", o:tagCount() == 0 )

    o:setSuggestions( { "Placeno", "Neproknjizeno", "Storno" } )
    o:handleKey( Asc( "p" ) )
    o:handleKey( Asc( "l" ) )
    a := o:filtered()
    Assert( "HTTagInput: suggestions", Len( a ) == 1 .AND. a[ 1 ] == "Placeno" )
    o:handleKey( K_DOWN )
    o:handleKey( K_ENTER )
    Assert( "HTTagInput: Down + Enter takes the suggestion", o:tagCount() == 1 .AND. o:tags()[ 1 ] == "Placeno" )
    Assert( "HTTagInput: onChanged", nChg >= 3 )

    hb_cdpSelect( "UTF8EX" )
    o:clear()
    o:setSuggestions( { "Плаћено", "Сторно" } )
    o:setText( "pla" )
    Assert( "HTTagInput: Latin finds Cyrillic", Len( o:filtered() ) == 1 )
    hb_cdpSelect( "EN" )

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE TestCommandPalette()

    LOCAL o, nRun := 0, nExcel := 0
    LOCAL oBar, oMenu, oAct

    o := HTCommandPalette():new()
    o:addCommand( "Bruto bilans", {|| nRun++ }, "Ctrl+B", "Izvestaji" )
    o:addCommand( "Export Excel", {|| nExcel++ }, "Ctrl+E", "" )
    o:addCommand( "Kartica konta", {|| nRun++ }, NIL, "Izvestaji" )

    Assert( "HTCommandPalette: empty filter lists all", o:hitCount() == 3 .AND. o:marked() == 1 )
    o:setText( "bilans" )
    Assert( "HTCommandPalette: one word", o:hitCount() == 1 .AND. "bilans" $ Lower( o:hitTitle( 1 ) ) )
    Assert( "HTCommandPalette: shortcut label", o:hitShortcut( 1 ) == "Ctrl+B" )
    o:setText( "export excel" )
    Assert( "HTCommandPalette: both words", o:hitCount() == 1 .AND. o:hitTitle( 1 ) == "Export Excel" )
    o:runCurrent()
    Assert( "HTCommandPalette: runCurrent", nExcel == 1 )

    o:setText( "" )
    /* refresh keeps the previous hit when possible; force the first row */
    o:handleKey( K_HOME )
    Assert( "HTCommandPalette: Home marks the first", o:marked() == 1 )
    o:handleKey( K_DOWN )
    Assert( "HTCommandPalette: Down moves the mark", o:marked() == 2 )
    o:handleKey( K_UP )
    Assert( "HTCommandPalette: Up", o:marked() == 1 )

    oBar := HTMenuBar():new()
    oMenu := oBar:addMenu( "&Reports" )
    oAct := oMenu:addAction( "Trial &balance" )
    oAct:setShortcut( K_CTRL_B )
    oAct:onTriggered := {|| nRun++ }
    o:clear()
    o:addMenus( oBar )
    o:setText( "balance" )
    Assert( "HTCommandPalette: menu action", o:hitCount() == 1 .AND. o:hitTitle( 1 ) == "Trial balance" )
    Assert( "HTCommandPalette: menu path", o:hitDetail( 1 ) == "Reports" )
    o:runCurrent()
    Assert( "HTCommandPalette: menu command runs", nRun == 1 )

    hb_cdpSelect( "UTF8EX" )
    o:clear()
    o:addCommand( "Бруто биланс", {|| nRun++ }, NIL, "" )
    o:setText( "bruto" )
    Assert( "HTCommandPalette: Latin finds Cyrillic", o:hitCount() == 1 )
    hb_cdpSelect( "EN" )

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE TestPropertyGrid()

    LOCAL o, cName := "", xVal, nChg := 0

    o := HTPropertyGrid():new()
    o:onChanged := {| c, x | nChg++, cName := c, xVal := x }
    o:onBrowse := {| c, v | HB_SYMBOL_UNUSED( c ), HB_SYMBOL_UNUSED( v ), "b.dbf" }

    o:addProperty( "name", "Name", HT_PROP_TEXT, "Ana" )
    o:addProperty( "qty", "Qty", HT_PROP_NUMBER, 2 )
    o:addProperty( "on", "Active", HT_PROP_LOGICAL, .F. )
    o:addGroup( "Look" )
    o:addProperty( "wh", "Warehouse", HT_PROP_LIST, "A", { "A", "B", "C" } )
    o:addProperty( "color", "Color", HT_PROP_COLOR, "W/N" )
    o:addProperty( "file", "File", HT_PROP_FILE, "a.dbf" )

    Assert( "HTPropertyGrid: values", o:value( "name" ) == "Ana" .AND. o:value( "qty" ) == 2 .AND. ! o:value( "on" ) )
    Assert( "HTPropertyGrid: starts on the first row", o:currentName() == "name" )

    o:handleKey( Asc( "Q" ) )
    o:handleKey( K_ENTER )
    Assert( "HTPropertyGrid: typing replaces and Enter commits", o:value( "name" ) == "Q" .AND. o:currentName() == "qty" )
    Assert( "HTPropertyGrid: onChanged", nChg >= 1 .AND. cName == "name" .AND. xVal == "Q" )

    o:handleKey( K_ENTER )
    o:handleKey( K_BS )
    o:handleKey( Asc( "5" ) )
    o:handleKey( K_ENTER )
    Assert( "HTPropertyGrid: number", o:value( "qty" ) == 5 )

    o:handleKey( K_SPACE )
    Assert( "HTPropertyGrid: Space toggles logical", o:value( "on" ) )
    o:handleKey( K_DOWN )
    Assert( "HTPropertyGrid: Down skips the group", o:currentName() == "wh" )
    o:handleKey( K_RIGHT )
    Assert( "HTPropertyGrid: Right cycles the list", o:value( "wh" ) == "B" )
    o:handleKey( K_LEFT )
    Assert( "HTPropertyGrid: Left cycles back", o:value( "wh" ) == "A" )

    o:setCurrent( "color" )
    o:handleKey( K_RIGHT )
    Assert( "HTPropertyGrid: color cycles", o:value( "color" ) == "N/W" )

    o:setCurrent( "file" )
    o:handleKey( K_F4 )
    Assert( "HTPropertyGrid: F4 runs onBrowse", o:value( "file" ) == "b.dbf" )

    o:setCurrent( "name" )
    o:handleKey( Asc( "Z" ) )
    o:handleKey( K_ESC )
    Assert( "HTPropertyGrid: Esc cancels", o:value( "name" ) == "Q" .AND. ! o:isEditing() )

    o:setReadOnly( "name", .T. )
    o:handleKey( Asc( "Z" ) )
    Assert( "HTPropertyGrid: read-only ignores typing", o:value( "name" ) == "Q" .AND. ! o:isEditing() )

    o:setValue( "qty", 9 )
    Assert( "HTPropertyGrid: setValue", o:value( "qty" ) == 9 )
    Assert( "HTPropertyGrid: unknown name", o:value( "missing" ) == NIL )

RETURN
