/*
 * Theme editor - change the colors of a theme and add new themes.
 *
 *   Themes   pick a theme: the whole screen (this window, the preview
 *            window, menus, toasts) switches to it at once
 *   Colors   every color category (HT_CLR_*) of the current colors.
 *            Text / Back set its first color pair; "Color" takes any color
 *            string: "0/9", "W+/B", "15/1,0/14" (several pairs: browse)
 *   Palette  what color index 0..15 looks like on screen: R G B
 *   Save     keeps the edited colors and palette as a theme under "Name".
 *            Built-in themes stay as they are - save under another name
 *   F2       writes your themes to themes.json next to the program; they
 *            are read back on the next start
 *
 * Keys: Tab / Shift+Tab next field, Up/Down in lists and spinners (digits
 * type a value), F5 toasts, F6 message box, F10 menu, Esc quit.
 * Ctrl+F6 goes to the Preview window; a click on the editor comes back.
 *
 * HTTheme API used here: themeNames(), loadTheme(), isBuiltIn(), getColor(),
 * setColor(), categoryName(), paletteColor(), setPaletteColor(), saveTheme(),
 * removeTheme(), addTheme(), themeColors(), themePalette(); then
 * HTApplication():repaintAll() shows the change.
 */

#include "hbtui.ch"
#include "inkey.ch"

#define THEMES_FILE   ( hb_DirBase() + "themes.json" )

STATIC s_oThemes, s_oName
STATIC s_oCats, s_oColor, s_oFg, s_oBg, s_oSwatch
STATIC s_oPal, s_oR, s_oG, s_oB
STATIC s_oStatus
STATIC s_lSync := .F.   /* controls are being set from code: ignore their onChanged */

/* A strip that shows one color: the current category's first pair */
CLASS ColorSwatch FROM HTWidget
    DATA cSample INIT "N/W"
    METHOD new( parent )
    METHOD paintEvent( paintEvent )
ENDCLASS

/* child widgets are visible from the start (top-level windows need show()) */
METHOD new( parent ) CLASS ColorSwatch
    ::super:new( parent )
    ::FisVisible := .T.
RETURN self

METHOD PROCEDURE paintEvent( paintEvent ) CLASS ColorSwatch

    LOCAL nRow

    HB_SYMBOL_UNUSED( paintEvent )

    FOR nRow := 0 TO MaxRow()
        DispOutAt( nRow, 0, ht_cellPadC( IIF( nRow == Int( MaxRow() / 2 ), ;
            "Sample text  " + ::cSample, "" ), MaxCol() + 1 ), ::cSample )
    NEXT

RETURN

/* The 16 palette colors as blocks, next to the palette list */
CLASS PaletteStrip FROM HTWidget
    METHOD new( parent )
    METHOD paintEvent( paintEvent )
ENDCLASS

METHOD new( parent ) CLASS PaletteStrip
    ::super:new( parent )
    ::FisVisible := .T.
RETURN self

METHOD PROCEDURE paintEvent( paintEvent ) CLASS PaletteStrip

    LOCAL n

    HB_SYMBOL_UNUSED( paintEvent )

    FOR n := 0 TO Min( 15, MaxRow() )
        DispOutAt( n, 0, Space( MaxCol() + 1 ), "0/" + hb_ntos( n ) )
    NEXT

RETURN

PROCEDURE Main()

    LOCAL app, main, menuBar, menu, act, btn
    LOCAL n

    app := HTApplication():new()
    ht_cuiSetupDisplay( app:rows(), app:cols() )

    LoadThemesFile()

    main := HTMainWindow():new()
    main:setWindowTitle( "Theme Editor" )
    main:show()

    menuBar := HTMenuBar():new( main )
    menu := menuBar:addMenu( "File" )
    act := menu:addAction( "Write themes.json   F2" )
    act:onTriggered := {|| SaveThemesFile() }
    menu:addSeparator()
    act := menu:addAction( "Quit               Esc" )
    act:onTriggered := {|| HTApplication():quit() }
    menu := menuBar:addMenu( "Preview" )
    act := menu:addAction( "Toasts         F5" )
    act:onTriggered := {|| ShowToasts() }
    act := menu:addAction( "Message box    F6" )
    act:onTriggered := {|| HTMessageBox():information( "Preview", "Message box in the current theme" ) }

    /* --- themes --- */
    HTLabel():new( "Themes", main ):move( 1, 2 )
    s_oThemes := HTListBox():new( main )
    s_oThemes:move( 1, 3 )
    s_oThemes:resize( 26, 12 )
    s_oThemes:onChanged := {|| ThemeChosen() }

    HTLabel():new( "Name", main ):move( 1, 16 )
    s_oName := HTLineEdit():new( main )
    s_oName:move( 1, 17 )
    s_oName:resize( 26, 1 )

    btn := HTPushButton():new( "Save theme", main )
    btn:move( 1, 19 )
    btn:onClicked := {|| SaveTheme() }
    btn := HTPushButton():new( "Delete theme", main )
    btn:move( 1, 21 )
    btn:onClicked := {|| DeleteTheme() }
    btn := HTPushButton():new( "Write file", main )
    btn:move( 1, 23 )
    btn:onClicked := {|| SaveThemesFile() }

    /* --- color categories --- */
    HTLabel():new( "Colors (text/background)", main ):move( 30, 2 )
    s_oCats := HTListBox():new( main )
    FOR n := 1 TO HT_CLR_MAX
        s_oCats:addItem( CategoryText( n ), n )
    NEXT
    s_oCats:move( 30, 3 )
    s_oCats:resize( 38, 20 )
    s_oCats:onChanged := {|| CategoryChosen() }

    HTLabel():new( "Color", main ):move( 30, 24 )
    s_oColor := HTLineEdit():new( main )
    s_oColor:move( 37, 24 )
    s_oColor:resize( 31, 1 )
    s_oColor:onChanged := {| c | ColorTyped( c ) }

    s_oFg := HTSpinner():new( main )
    s_oFg:setup( "Text", 0, 15, 1, 0 )
    s_oFg:move( 30, 26 )
    s_oFg:resize( 14, 1 )
    s_oFg:onChanged := {|| PairChanged() }
    s_oBg := HTSpinner():new( main )
    s_oBg:setup( "Back", 0, 15, 1, 0 )
    s_oBg:move( 48, 26 )
    s_oBg:resize( 14, 1 )
    s_oBg:onChanged := {|| PairChanged() }

    s_oSwatch := ColorSwatch():new( main )
    s_oSwatch:move( 30, 28 )
    s_oSwatch:resize( 38, 3 )

    /* --- palette --- */
    HTLabel():new( "Palette (index  R  G  B)", main ):move( 72, 2 )
    PaletteStrip():new( main ):setGeometry( 72, 3, 4, 16 )
    s_oPal := HTListBox():new( main )
    FOR n := 0 TO 15
        s_oPal:addItem( PaletteText( n ), n )
    NEXT
    s_oPal:move( 77, 3 )
    s_oPal:resize( 24, 16 )
    s_oPal:onChanged := {|| PaletteChosen() }

    s_oR := HTSpinner():new( main )
    s_oR:setup( "R", 0, 255, 1, 0 )
    s_oR:move( 72, 20 )
    s_oR:resize( 12, 1 )
    s_oR:onChanged := {|| RgbChanged() }
    s_oG := HTSpinner():new( main )
    s_oG:setup( "G", 0, 255, 1, 0 )
    s_oG:move( 85, 20 )
    s_oG:resize( 12, 1 )
    s_oG:onChanged := {|| RgbChanged() }
    s_oB := HTSpinner():new( main )
    s_oB:setup( "B", 0, 255, 1, 0 )
    s_oB:move( 98, 20 )
    s_oB:resize( 12, 1 )
    s_oB:onChanged := {|| RgbChanged() }

    s_oStatus := HTStatusBar():new( main )
    s_oStatus:setGeometry( 0, Max( 0, main:height - 2 ), main:width, 1 )
    s_oStatus:addSection( "Pick a theme, change its colors, Save theme keeps them" )
    s_oStatus:addSection( "F2 write file  F5 toasts  F6 message  F10 menu  Esc quit" )

    main:onKey( K_F2, {|| SaveThemesFile() } )
    main:onKey( K_F5, {|| ShowToasts() } )
    main:onKey( K_F6, {|| HTMessageBox():information( "Preview", "Message box in the current theme" ) } )

    FillThemes()
    SyncAll()
    main:repaint()

    BuildPreview()
    main:activate()   /* keyboard to the editor; the Preview stays on top */

    app:exec()

RETURN

/*----------------------------------------------------------------------------
 * Themes
 *--------------------------------------------------------------------------*/

/** Theme list from HTTheme, own themes marked; selects the current one. */
STATIC PROCEDURE FillThemes()

    LOCAL oTheme := HTTheme()
    LOCAL cName

    s_oThemes:clear()
    FOR EACH cName IN oTheme:themeNames()
        s_oThemes:addItem( cName + IIF( oTheme:isBuiltIn( cName ), "", "  (own)" ), cName )
        IF Lower( cName ) == Lower( oTheme:themeName() )
            s_oThemes:setCurrentIndex( cName:__enumIndex() )
        ENDIF
    NEXT

RETURN

STATIC PROCEDURE ThemeChosen()

    LOCAL cName := s_oThemes:currentData()

    IF cName != NIL .AND. HTTheme():loadTheme( cName )
        SyncAll()
        HTApplication():repaintAll()
        Status( "Theme " + cName )
    ENDIF

RETURN

STATIC PROCEDURE SaveTheme()

    LOCAL oTheme := HTTheme()
    LOCAL cName := AllTrim( s_oName:text )

    IF Empty( cName )
        Status( "Type a name for the theme first" )
    ELSEIF oTheme:isBuiltIn( cName )
        HTMessageBox():warning( "Save theme", "Built-in themes stay as they are - save under another name" )
    ELSE
        oTheme:saveTheme( cName )
        FillThemes()
        SyncAll()
        HTApplication():repaintAll()
        Status( "Saved " + cName + " - F2 writes it to " + hb_FNameNameExt( THEMES_FILE ) )
    ENDIF

RETURN

STATIC PROCEDURE DeleteTheme()

    LOCAL oTheme := HTTheme()
    LOCAL cName := s_oThemes:currentData()

    IF cName == NIL .OR. oTheme:isBuiltIn( cName )
        HTMessageBox():warning( "Delete theme", "Built-in themes cannot be deleted" )
    ELSEIF HTMessageBox():question( "Delete theme", "Delete " + cName + "?" )
        oTheme:removeTheme( cName )
        oTheme:loadDefault()
        FillThemes()
        SyncAll()
        HTApplication():repaintAll()
        Status( "Deleted " + cName + " - F2 updates the file" )
    ENDIF

RETURN

/*----------------------------------------------------------------------------
 * Colors of the categories
 *--------------------------------------------------------------------------*/

STATIC FUNCTION CategoryText( n )
RETURN PadR( HTTheme():categoryName( n ), 19 ) + HTTheme():getColor( n )

STATIC PROCEDURE CategoryChosen()

    LOCAL nCat := s_oCats:currentIndex()
    LOCAL cColor := HTTheme():getColor( nCat )

    s_lSync := .T.
    s_oColor:setText( cColor )
    SyncPair( cColor )
    s_lSync := .F.
    s_oSwatch:cSample := cColor
    RepaintControls()

RETURN

/** Text / Back spinners: the first pair of the current category */
STATIC PROCEDURE PairChanged()

    LOCAL nCat := s_oCats:currentIndex()
    LOCAL aPairs

    IF s_lSync .OR. nCat < 1
        RETURN
    ENDIF
    aPairs := hb_ATokens( HTTheme():getColor( nCat ), "," )
    aPairs[ 1 ] := hb_ntos( s_oFg:value ) + "/" + hb_ntos( s_oBg:value )
    ApplyColor( nCat, JoinPairs( aPairs ), .T. )

RETURN

/** The whole color string, typed */
STATIC PROCEDURE ColorTyped( cText )

    LOCAL nCat := s_oCats:currentIndex()

    IF s_lSync .OR. nCat < 1
        RETURN
    ENDIF
    IF ValidColor( cText )
        ApplyColor( nCat, AllTrim( cText ), .F. )
    ELSE
        Status( "Not a color yet: " + AllTrim( cText ) + "   (0/9  W+/B  15/1,0/14)" )
    ENDIF

RETURN

STATIC PROCEDURE ApplyColor( nCat, cColor, lFromSpinners )

    HTTheme():setColor( nCat, cColor )
    s_oCats:setItemText( nCat, CategoryText( nCat ) )
    s_oSwatch:cSample := cColor
    s_lSync := .T.
    IF lFromSpinners
        s_oColor:setText( cColor )
    ELSE
        SyncPair( cColor )
    ENDIF
    s_lSync := .F.
    HTApplication():repaintAll()
    Status( HTTheme():categoryName( nCat ) + " = " + cColor + "  (Save theme keeps it)" )

RETURN

/** Text / Back spinners from the first pair of a color string */
STATIC PROCEDURE SyncPair( cColor )

    LOCAL nAttr := hb_ColorToN( hb_ATokens( cColor, "," )[ 1 ] )

    IF nAttr >= 0
        s_oFg:setValue( Int( nAttr % 16 ) )
        s_oBg:setValue( Int( Int( nAttr / 16 ) % 16 ) )
    ENDIF

RETURN

STATIC FUNCTION JoinPairs( aPairs )

    LOCAL cColor := "", cPair

    FOR EACH cPair IN aPairs
        cColor += IIF( cPair:__enumIndex() == 1, "", "," ) + AllTrim( cPair )
    NEXT

RETURN cColor

/** A color string: pairs "fg/bg" separated by commas; a side is a number
 * 0-15 or letters N B G R W I U X, with "+" (bright) and "*" (bright
 * background) - from Digi's boje.prg (hb_ColorToN() accepts anything). */
STATIC FUNCTION ValidColor( cColor )

    LOCAL cPair, aSide

    IF ! HB_ISSTRING( cColor ) .OR. Empty( cColor )
        RETURN .F.
    ENDIF
    FOR EACH cPair IN hb_ATokens( AllTrim( cColor ), "," )
        aSide := hb_ATokens( cPair, "/" )
        IF Len( aSide ) < 1 .OR. Len( aSide ) > 2 .OR. ! ValidSide( aSide[ 1 ] ) .OR. ;
           ( Len( aSide ) == 2 .AND. ! ValidSide( aSide[ 2 ] ) )
            RETURN .F.
        ENDIF
    NEXT

RETURN .T.

STATIC FUNCTION ValidSide( cSide )

    LOCAL c, lLetter := .F.

    cSide := AllTrim( cSide )
    IF Empty( cSide )
        RETURN .T.
    ENDIF
    IF IsDigit( cSide )
        FOR EACH c IN cSide
            IF ! IsDigit( c ) .AND. !( c $ "+*" )
                RETURN .F.
            ENDIF
        NEXT
        RETURN Val( cSide ) <= 15
    ENDIF
    FOR EACH c IN Upper( cSide )
        IF c $ "NBGRWIUX"
            lLetter := .T.
        ELSEIF !( c $ "+*" )
            RETURN .F.
        ENDIF
    NEXT

RETURN lLetter

/*----------------------------------------------------------------------------
 * Palette
 *--------------------------------------------------------------------------*/

STATIC FUNCTION PaletteText( n )

    LOCAL nRGB := HTTheme():paletteColor( n )

RETURN Str( n, 2 ) + "  " + PadR( { "N", "B", "G", "BG", "R", "RB", "GR", "W", ;
    "N+", "B+", "G+", "BG+", "R+", "RB+", "GR+", "W+" }[ n + 1 ], 4 ) + ;
    Str( nRGB % 256, 4 ) + Str( Int( nRGB / 256 ) % 256, 4 ) + Str( Int( nRGB / 65536 ) % 256, 4 )

STATIC PROCEDURE PaletteChosen()

    LOCAL nRGB := HTTheme():paletteColor( s_oPal:currentIndex() - 1 )

    s_lSync := .T.
    s_oR:setValue( Int( nRGB % 256 ) )
    s_oG:setValue( Int( Int( nRGB / 256 ) % 256 ) )
    s_oB:setValue( Int( Int( nRGB / 65536 ) % 256 ) )
    s_lSync := .F.
    RepaintControls()

RETURN

STATIC PROCEDURE RgbChanged()

    LOCAL nIndex := s_oPal:currentIndex() - 1

    IF s_lSync .OR. nIndex < 0
        RETURN
    ENDIF
    /* the GT repaints the screen itself when the palette changes */
    HTTheme():setPaletteColor( nIndex, ht_rgb( s_oR:value, s_oG:value, s_oB:value ) )
    s_oPal:setItemText( nIndex + 1, PaletteText( nIndex ) )
    s_oPal:parent():repaintChild( s_oPal )
    Status( "Color " + hb_ntos( nIndex ) + " = " + hb_ntos( s_oR:value ) + "," + ;
        hb_ntos( s_oG:value ) + "," + hb_ntos( s_oB:value ) + "  (Save theme keeps it)" )

RETURN

/*----------------------------------------------------------------------------
 * Helpers
 *--------------------------------------------------------------------------*/

/** All editor controls from the current colors and palette */
STATIC PROCEDURE SyncAll()

    LOCAL oTheme := HTTheme()
    LOCAL n

    FOR n := 1 TO HT_CLR_MAX
        s_oCats:setItemText( n, CategoryText( n ) )
    NEXT
    FOR n := 0 TO 15
        s_oPal:setItemText( n + 1, PaletteText( n ) )
    NEXT
    s_lSync := .T.
    s_oName:setText( IIF( oTheme:isBuiltIn( oTheme:themeName() ), "My " + oTheme:themeName(), oTheme:themeName() ) )
    s_lSync := .F.
    CategoryChosen()
    PaletteChosen()

RETURN

STATIC PROCEDURE RepaintControls()

    LOCAL oParent := s_oCats:parent()

    IF oParent != NIL
        oParent:repaintChild( s_oColor )
        oParent:repaintChild( s_oFg )
        oParent:repaintChild( s_oBg )
        oParent:repaintChild( s_oSwatch )
        oParent:repaintChild( s_oR )
        oParent:repaintChild( s_oG )
        oParent:repaintChild( s_oB )
        oParent:repaintChild( s_oName )
    ENDIF

RETURN

STATIC PROCEDURE Status( cText )
    s_oStatus:setSection( 1, cText )
RETURN

STATIC PROCEDURE ShowToasts()
    HTToast():show( "Info toast", 3000, HT_CLR_TOAST_INFO )
    HTToast():show( "Saved - success toast", 3000, HT_CLR_TOAST_SUCCESS )
    HTToast():show( "Warning toast", 3000, HT_CLR_TOAST_WARNING )
    HTToast():show( "Error toast", 3000, HT_CLR_TOAST_ERROR )
RETURN

/*----------------------------------------------------------------------------
 * themes.json: { "name": { "colors": [ ... ], "palette": [ ... ] | null } }
 *--------------------------------------------------------------------------*/

STATIC PROCEDURE LoadThemesFile()

    LOCAL hFile, hTheme

    IF ! hb_FileExists( THEMES_FILE )
        RETURN
    ENDIF
    hFile := hb_jsonDecode( hb_MemoRead( THEMES_FILE ) )
    IF HB_ISHASH( hFile )
        FOR EACH hTheme IN hFile
            IF HB_ISHASH( hTheme )
                HTTheme():addTheme( hTheme:__enumKey(), hb_HGetDef( hTheme, "colors", NIL ), ;
                    hb_HGetDef( hTheme, "palette", NIL ) )
            ENDIF
        NEXT
    ENDIF

RETURN

STATIC PROCEDURE SaveThemesFile()

    LOCAL oTheme := HTTheme()
    LOCAL hFile := { => }
    LOCAL cName, nCount := 0

    hb_HKeepOrder( hFile, .T. )
    FOR EACH cName IN oTheme:themeNames()
        IF ! oTheme:isBuiltIn( cName )
            hFile[ cName ] := { "colors" => oTheme:themeColors( cName ), ;
                                "palette" => oTheme:themePalette( cName ) }
            nCount++
        ENDIF
    NEXT
    IF hb_MemoWrit( THEMES_FILE, hb_jsonEncode( hFile, .T. ) )
        Status( hb_ntos( nCount ) + " own theme(s) written to " + THEMES_FILE )
    ELSE
        Status( "Could not write " + THEMES_FILE )
    ENDIF

RETURN

/*----------------------------------------------------------------------------
 * Preview window: the controls the theme colors
 *--------------------------------------------------------------------------*/

STATIC PROCEDURE BuildPreview()

    LOCAL win, oGet, chk, cmb, btn, prg, brw
    LOCAL aRec := { 1 }
    LOCAL aData := { { "1200", "Kupci u zemlji" }, { "2410", "Tekuci racun" }, ;
                     { "4330", "Dobavljaci" }, { "6040", "Prihodi" }, { "5120", "Troskovi" } }
    LOCAL cName := Space( 20 )

    win := HTWidget():new()
    win:move( 72, 22 )
    win:resize( 56, 13 )
    win:setWindowTitle( "Preview" )
    win:shadow := HT_SHADOW_ATTR

    oGet := HTGet():new( win )
    oGet:setup( {| x | IIF( x == NIL, cName, cName := x ) }, "Name:", "@!", NIL, NIL, ;
        "A GET field", NIL, .F., .F., 6 + 20 )
    oGet:setLabelWidth( 6 )
    oGet:move( 1, 1 )

    chk := HTCheckBox():new( "Checked", win )
    chk:move( 1, 3 )

    cmb := HTComboBox():new( win )
    cmb:move( 14, 3 )
    cmb:addItem( "Combo box" )
    cmb:addItem( "Second item" )

    btn := HTPushButton():new( "OK", win )
    btn:move( 1, 5 )
    btn:onClicked := {|| HTToast():show( "OK pressed", 2000, HT_CLR_TOAST_SUCCESS ) }
    btn := HTPushButton():new( "Cancel", win )
    btn:move( 9, 5 )

    prg := HTProgressBar():new( win )
    prg:move( 1, 7 )
    prg:resize( 25, 1 )
    prg:setValue( 60 )

    brw := HTBrowse():new( win )
    brw:move( 28, 1 )
    brw:resize( 26, 9 )
    brw:setGoTopBlock( {|| aRec[ 1 ] := 1 } )
    brw:setGoBottomBlock( {|| aRec[ 1 ] := Len( aData ) } )
    brw:setSkipBlock( {| n | Skip( aRec, Len( aData ), n ) } )
    brw:addColumn( "Konto", {|| aData[ aRec[ 1 ], 1 ] }, 5 )
    brw:addColumn( "Naziv", {|| aData[ aRec[ 1 ], 2 ] }, 16 )
    brw:setHeadSep( e"\xC4" )
    brw:setColSep( e"\xB3" )

    win:show()

RETURN

STATIC FUNCTION Skip( aRec, nLen, nRequest )

    LOCAL nActual := IIF( nRequest > 0, Min( nRequest, nLen - aRec[ 1 ] ), Max( nRequest, 1 - aRec[ 1 ] ) )

    aRec[ 1 ] += nActual

RETURN nActual
