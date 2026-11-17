/**
 * test_get.prg - Headless tests for HTGet (TGet-backed input widget).
 *
 * Tests property defaults, setup(), getValue/setValue, label/picture/width
 * calculations, and getDisplayValue() formatting.
 * Does not test keyboard editing or TGet rendering (those require a real GT).
 *
 * Build: hbmk2 test_get.prg hbmk.hbm
 */

#include "hbtui.ch"
#include "inkey.ch"

STATIC nPass := 0
STATIC nFail := 0

PROCEDURE Main()

    LOCAL nTotal

    TestGetDefaults()
    TestGetSetup()
    TestGetValue()
    TestGetLabel()
    TestGetPicture()
    TestGetReadOnly()
    TestGetEnterMovesFocus()
    TestGetUnchangedKeepsValue()
    TestGetSelection()

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

STATIC PROCEDURE TestGetDefaults()

    LOCAL o

    o := HTGet():new()
    Assert( "HTGet:label default empty",    o:label == "" )
    Assert( "HTGet:picture default empty",  o:picture == "" )
    Assert( "HTGet:readOnly default .F.",   ! o:readOnly )
    Assert( "HTGet:hide default .F.",       ! o:hide )
    Assert( "HTGet:helpLine default empty", o:helpLine == "" )
    Assert( "HTGet:labelWidth default 0",   o:labelWidth == 0 )
    Assert( "HTGet:valid default NIL",      o:valid == NIL )
    Assert( "HTGet:when default NIL",       o:when == NIL )
    Assert( "HTGet:onChange default NIL",   o:onChange == NIL )

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE TestGetSetup()

    LOCAL o
    LOCAL cName := "Alice"
    LOCAL bVar  := {|x| IIF( x == NIL, cName, cName := x )}

    o := HTGet():new()
    o:setup( bVar, "Name", "", NIL, NIL, "Enter name", NIL, .F., .F., 30 )

    Assert( "HTGet:setup label",       o:label == "Name" )
    Assert( "HTGet:setup helpLine",    o:helpLine == "Enter name" )
    Assert( "HTGet:setup readOnly .F.", ! o:readOnly )

    /* label "Name" = 4 chars + 1 separator = labelWidth 5 */
    Assert( "HTGet:setup labelWidth",  o:labelWidth == 5 )

    /* inputWidth = total(30) - labelWidth(5) = 25 */
    Assert( "HTGet:setup inputWidth",  o:inputWidth == 25 )

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE TestGetValue()

    LOCAL o
    LOCAL cCity := "Paris"
    LOCAL bVar  := {|x| IIF( x == NIL, cCity, cCity := x )}

    o := HTGet():new()
    o:setup( bVar, "", "", NIL, NIL, "", NIL, .F., .F., 20 )

    Assert( "HTGet:getValue initial",        o:getValue() == "Paris" )

    o:setValue( "London" )
    Assert( "HTGet:setValue updates var",    o:getValue() == "London" )
    Assert( "HTGet:local var updated",       cCity == "London" )

    o:setValue( "" )
    Assert( "HTGet:setValue empty string",   o:getValue() == "" )

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE TestGetLabel()

    LOCAL o
    LOCAL cVal := "x"
    LOCAL bVar := {|x| IIF( x == NIL, cVal, cVal := x )}

    o := HTGet():new()
    o:setup( bVar, "City", "", NIL, NIL, "", NIL, .F., .F., 30 )

    /* "City" = 4 chars + 1 = 5 */
    Assert( "HTGet:setLabel initial",        o:label == "City" )
    Assert( "HTGet:labelWidth auto 5",       o:labelWidth == 5 )

    o:setLabel( "Zip" )
    /* "Zip" = 3 chars + 1 = 4 */
    Assert( "HTGet:setLabel updates label",  o:label == "Zip" )
    Assert( "HTGet:labelWidth recalc 4",     o:labelWidth == 4 )
    Assert( "HTGet:inputWidth recalc 26",    o:inputWidth == 26 )

    o:setLabel( "" )
    Assert( "HTGet:setLabel empty labelWidth 0",  o:labelWidth == 0 )
    Assert( "HTGet:setLabel empty inputWidth 30", o:inputWidth == 30 )

    /* manual override */
    o:setLabelWidth( 8 )
    Assert( "HTGet:setLabelWidth 8",         o:labelWidth == 8 )
    Assert( "HTGet:inputWidth after manual", o:inputWidth == 22 )

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE TestGetPicture()

    LOCAL o
    LOCAL cVal := "hello"
    LOCAL bVar := {|x| IIF( x == NIL, cVal, cVal := x )}

    o := HTGet():new()
    o:setup( bVar, "", "", NIL, NIL, "", NIL, .F., .F., 20 )

    /* no picture: getDisplayValue returns raw value */
    Assert( "HTGet:getDisplayValue no picture", o:getDisplayValue() == "hello" )

    /* @! converts to uppercase */
    o:setPicture( "@!" )
    Assert( "HTGet:setPicture updates picture", o:picture == "@!" )
    Assert( "HTGet:getDisplayValue @!",         o:getDisplayValue() == "HELLO" )

    /* clear picture */
    o:setPicture( "" )
    Assert( "HTGet:getDisplayValue after clear pic", o:getDisplayValue() == "hello" )

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE TestGetReadOnly()

    LOCAL o
    LOCAL cVal := "data"
    LOCAL bVar := {|x| IIF( x == NIL, cVal, cVal := x )}

    o := HTGet():new()
    o:setup( bVar, "", "", NIL, NIL, "", NIL, .F., .F., 20 )

    Assert( "HTGet:readOnly default .F.", ! o:readOnly )

    o:setReadOnly( .T. )
    Assert( "HTGet:setReadOnly .T.",      o:readOnly )

    o:setReadOnly( .F. )
    Assert( "HTGet:setReadOnly .F.",      ! o:readOnly )

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE TestGetEnterMovesFocus()

    LOCAL oForm, oGet1, oGet2
    LOCAL c1 := "a         "
    LOCAL c2 := "b         "
    LOCAL lValid := .T.

    oForm := HTWidget():new()

    oGet1 := HTGet():new( oForm )
    oGet1:setup( {|x| IIF( x == NIL, c1, c1 := x ) }, "", "", {|| lValid }, NIL, "", NIL, .F., .F., 10 )
    oGet2 := HTGet():new( oForm )
    oGet2:setup( {|x| IIF( x == NIL, c2, c2 := x ) }, "", "", NIL, NIL, "", NIL, .F., .F., 10 )

    oForm:focusNextChild()
    Assert( "Enter: first GET focused",              oForm:focusWidget() == oGet1 )

    lValid := .F.
    oForm:keyEvent( HTKeyEvent():new( K_ENTER ) )
    Assert( "Enter: failing VALID keeps the focus",  oForm:focusWidget() == oGet1 )

    lValid := .T.
    oForm:keyEvent( HTKeyEvent():new( K_ENTER ) )
    Assert( "Enter: moves to the next GET",          oForm:focusWidget() == oGet2 )

    Assert( "HTGet:enterMovesFocus",                 oGet1:enterMovesFocus() )
    Assert( "HTLineEdit:enterMovesFocus",            HTLineEdit():new():enterMovesFocus() )
    Assert( "HTPushButton keeps Enter for itself",   ! HTPushButton():new( "OK" ):enterMovesFocus() )

RETURN

/*----------------------------------------------------------------*/

/** Leaving a field that was not edited leaves its variable alone, like
 * ReadModal(); an unpictured date shows like TGet shows it. Assigning the
 * unchanged buffer read "11/15/26" back as 1926 (the default epoch). */
STATIC PROCEDURE TestGetUnchangedKeepsValue()

    LOCAL dDate := hb_SToD( "20261115" )
    LOCAL nNum  := 1.23456
    LOCAL oDlg, oDate, oNum

    oDlg := HTDialog():new( NIL, "Test" )
    oDate := HTGet():new( oDlg )
    oDate:setup( {| x | IIF( x == NIL, dDate, dDate := x ) }, "", "", NIL, NIL, "", NIL, .F., .F., 10 )
    oNum := HTGet():new( oDlg )
    oNum:setup( {| x | IIF( x == NIL, nNum, nNum := x ) }, "", "9.99", NIL, NIL, "", NIL, .F., .F., 10 )

    Assert( "HTGet: unpictured date shows like TGet", oDate:getDisplayValue() == DToC( dDate ) )

    oDate:focusInEvent( HTFocusEvent():new( HT_EVENT_TYPE_FOCUSIN ) )
    oDate:focusOutEvent( HTFocusEvent():new( HT_EVENT_TYPE_FOCUSOUT ) )
    Assert( "HTGet: unchanged date keeps its century", dDate == hb_SToD( "20261115" ) )

    oNum:focusInEvent( HTFocusEvent():new( HT_EVENT_TYPE_FOCUSIN ) )
    oNum:focusOutEvent( HTFocusEvent():new( HT_EVENT_TYPE_FOCUSOUT ) )
    Assert( "HTGet: unchanged number keeps hidden decimals", nNum == 1.23456 )

RETURN

/*----------------------------------------------------------------*/

/** Shift+arrow selection in an HTGet (google group report: Shift+Right then
 * Del did not delete the selected block - HTGet had no selection at all).
 * gtnul reports no Shift key, so the selection is set with setSelection(). */
STATIC PROCEDURE TestGetSelection()

    LOCAL cText := "abcdef    "
    LOCAL cTel  := "123456"
    LOCAL oDlg, oGet, oTel

    oDlg := HTDialog():new( NIL, "Test" )
    oGet := HTGet():new( oDlg )
    oGet:setup( {| x | IIF( x == NIL, cText, cText := x ) }, "", "", NIL, NIL, "", NIL, .F., .F., 12 )
    oTel := HTGet():new( oDlg )
    oTel:setup( {| x | IIF( x == NIL, cTel, cTel := x ) }, "", "@R 999-999", NIL, NIL, "", NIL, .F., .F., 12 )

    oGet:focusInEvent( HTFocusEvent():new( HT_EVENT_TYPE_FOCUSIN ) )
    Assert( "HTGet: no selection after focus in", ! oGet:hasSelection() )

    oGet:setSelection( 2, 5 )
    Assert( "HTGet: selectedText", oGet:selectedText() == "bcd" )
    oGet:keyEvent( HTKeyEvent():new( K_DEL ) )
    Assert( "HTGet: Del removes the selection", RTrim( HTGetBuffer( oGet ) ) == "aef" )
    Assert( "HTGet: selection gone after Del", ! oGet:hasSelection() )

    oGet:keyEvent( HTKeyEvent():new( K_DEL ) )             /* no selection: one character */
    Assert( "HTGet: Del without selection", RTrim( HTGetBuffer( oGet ) ) == "af" )

    oGet:setSelection( 1, 3 )
    oGet:keyEvent( HTKeyEvent():new( K_BS ) )
    Assert( "HTGet: Backspace removes the selection", RTrim( HTGetBuffer( oGet ) ) == "" )

    oGet:keyEvent( HTKeyEvent():new( Asc( "q" ) ) )
    oGet:keyEvent( HTKeyEvent():new( Asc( "r" ) ) )
    oGet:keyEvent( HTKeyEvent():new( Asc( "s" ) ) )
    oGet:setSelection( 2, 3 )
    oGet:keyEvent( HTKeyEvent():new( Asc( "X" ) ) )
    Assert( "HTGet: typing replaces the selection", RTrim( HTGetBuffer( oGet ) ) == "qXs" )

    /* up to and including the last character (TGet's cursor cannot pass it) */
    oGet:setSelection( 2, 11 )
    Assert( "HTGet: selection covers the last character", Len( oGet:selectedText() ) == 9 )
    oGet:keyEvent( HTKeyEvent():new( K_DEL ) )
    Assert( "HTGet: Del to the end", RTrim( HTGetBuffer( oGet ) ) == "q" )

    oGet:focusOutEvent( HTFocusEvent():new( HT_EVENT_TYPE_FOCUSOUT ) )
    Assert( "HTGet: edited value written back", RTrim( cText ) == "q" )

    /* picture literals stay: "123-456", select "23-45", Del */
    oTel:focusInEvent( HTFocusEvent():new( HT_EVENT_TYPE_FOCUSIN ) )
    Assert( "HTGet: @R buffer", HTGetBuffer( oTel ) == "123-456" )
    oTel:setSelection( 2, 7 )
    Assert( "HTGet: @R selectedText", oTel:selectedText() == "23-45" )
    oTel:paintEvent( HTPaintEvent():new() )                /* selection highlight paints without error */
    Assert( "HTGet: paint keeps the selection", oTel:selectedText() == "23-45" )
    oTel:keyEvent( HTKeyEvent():new( K_DEL ) )
    Assert( "HTGet: @R Del keeps the literal", HTGetBuffer( oTel ) == "1  -6  " )

RETURN

/* the edit buffer of a focused HTGet */
STATIC FUNCTION HTGetBuffer( oGet )
RETURN oGet:editBuffer()
