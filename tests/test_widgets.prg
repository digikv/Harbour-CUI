/**
 * test_widgets.prg - Property and container tests for HBTui widget classes.
 *
 * Tests: HTLabel, HTLineEdit, HTCheckBox, HTPushButton, HTListBox,
 *        HTComboBox, HTCheckList, HTSpinner, HTProgressBar, HTScrollBar,
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

    LOCAL o

    o := HTLineEdit():new()
    Assert( "HTLineEdit:text default empty", o:text == "" )

    o:text := "hello"
    Assert( "HTLineEdit:text set",           o:text == "hello" )

    o:setText( "world" )
    Assert( "HTLineEdit:setText",            o:text == "world" )

    o:setText( "" )
    Assert( "HTLineEdit:setText empty",      o:text == "" )

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

    o:clear()
    Assert( "HTListBox:clear count 0",          o:count() == 0 )
    Assert( "HTListBox:clear currentIndex 0",   o:currentIndex() == 0 )

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

RETURN

/*----------------------------------------------------------------*/

/* Traditional Chinese typed through an IME: gtwvt / gtxwc deliver Unicode
   Inkey codes; editors must insert the multi-byte text (google group report:
   "unable to input traditional chinese characters"). */
STATIC PROCEDURE TestUnicodeInput()

    LOCAL cOldCP := hb_cdpSelect()
    LOCAL cHan, aKeys, nKey, o, t, g, cVal, cScr

    /* HTApplication picks UTF8EX when the app left Harbour's default "EN" */
    hb_cdpSelect( "EN" )
    HTApplication():new()
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
