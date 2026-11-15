/**
 * test_theme.prg - Tests for the HTTheme singleton color manager.
 *
 * Verifies: singleton behavior, all HT_CLR_MAX color slots populated per theme,
 * the built-in themes (Classic, Blue, Light, Dark, Amber, HighContrast, Mono)
 * with their palettes, own themes (saveTheme/addTheme/removeTheme, built-ins
 * protected), category names, invalid index fallback.
 *
 * Build: hbmk2 test_theme.prg hbmk.hbm
 */

#include "hbtui.ch"

STATIC nPass := 0
STATIC nFail := 0

PROCEDURE Main()

    LOCAL nTotal
    LOCAL oTheme

    /* force singleton initialization */
    oTheme := HTTheme()

    /* verify the singleton actually initialized */
    IF oTheme == NIL
        QOut( "FATAL: HTTheme() returned NIL" )
        ErrorLevel( 1 )
        RETURN
    ENDIF

    TestSingleton()
    TestDefaultTheme()
    TestDarkTheme()
    TestHighContrastTheme()
    TestMonoTheme()
    TestBuiltInThemes()
    TestOwnThemes()
    TestInvalidIndex()
    TestUsePalette()

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

/** Verify singleton: two calls return same object */
STATIC PROCEDURE TestSingleton()

    LOCAL o1 := HTTheme()
    LOCAL o2 := HTTheme()

    Assert( "HTTheme singleton identity", o1 == o2 )
    Assert( "HTTheme not NIL", o1 != NIL )

RETURN

/*----------------------------------------------------------------*/

/** Helper: check that all HT_CLR_MAX slots are non-NIL strings */
STATIC PROCEDURE VerifyAllSlots( cThemeName )

    LOCAL oTheme := HTTheme()
    LOCAL i
    LOCAL cColor

    FOR i := 1 TO HT_CLR_MAX
        cColor := oTheme:getColor( i )
        Assert( cThemeName + " slot " + hb_ntos( i ) + " not NIL", cColor != NIL )
        Assert( cThemeName + " slot " + hb_ntos( i ) + " is string", hb_isString( cColor ) )
        Assert( cThemeName + " slot " + hb_ntos( i ) + " non-empty", Len( cColor ) > 0 )
    NEXT

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE TestDefaultTheme()

    LOCAL oTheme := HTTheme()

    oTheme:loadDefault()

    /* spot-check a few well-known values */
    Assert( "Default:WINDOW", oTheme:getColor( HT_CLR_WINDOW ) == "0/9" )
    Assert( "Default:DESKTOP", oTheme:getColor( HT_CLR_DESKTOP ) == "0/8" )
    Assert( "Default:BUTTON_NORMAL", oTheme:getColor( HT_CLR_BUTTON_NORMAL ) == "15/1" )
    Assert( "Default:BUTTON_FOCUSED", oTheme:getColor( HT_CLR_BUTTON_FOCUSED ) == "0/14" )
    Assert( "Default:STATUSBAR", oTheme:getColor( HT_CLR_STATUSBAR ) == "0/15" )
    Assert( "Default:TITLEBAR", oTheme:getColor( HT_CLR_TITLEBAR ) == "0/11" )
    Assert( "Default:name", oTheme:themeName() == "Classic" )
    Assert( "Default:palette 11 = title blue", oTheme:palette()[ 12 ] == ht_rgb( 153, 180, 209 ) )
    Assert( "Default:palette 15 = white", oTheme:paletteColor( 15 ) == ht_rgb( 255, 255, 255 ) )

    /* all 50 slots populated */
    VerifyAllSlots( "Default" )

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE TestDarkTheme()

    LOCAL oTheme := HTTheme()

    oTheme:loadDark()

    /* spot-check */
    Assert( "Dark:WINDOW", oTheme:getColor( HT_CLR_WINDOW ) == "15/9" )
    Assert( "Dark:DESKTOP", oTheme:getColor( HT_CLR_DESKTOP ) == "8/12" )
    Assert( "Dark:BUTTON_FOCUSED", oTheme:getColor( HT_CLR_BUTTON_FOCUSED ) == "15/14" )
    Assert( "Dark:palette 9 = window", oTheme:paletteColor( 9 ) == ht_rgb( 45, 45, 48 ) )

    /* all 50 slots populated */
    VerifyAllSlots( "Dark" )

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE TestHighContrastTheme()

    LOCAL oTheme := HTTheme()

    oTheme:loadHighContrast()

    /* spot-check */
    Assert( "HiContrast:WINDOW", oTheme:getColor( HT_CLR_WINDOW ) == "15/00" )
    Assert( "HiContrast:CHECK_FOCUSED", oTheme:getColor( HT_CLR_CHECK_FOCUSED ) == "00/15" )

    /* all 50 slots populated */
    VerifyAllSlots( "HighContrast" )

RETURN

/*----------------------------------------------------------------*/

STATIC PROCEDURE TestMonoTheme()

    LOCAL oTheme := HTTheme()

    oTheme:loadMono()

    /* spot-check */
    Assert( "Mono:WINDOW", oTheme:getColor( HT_CLR_WINDOW ) == "07/00" )
    Assert( "Mono:LABEL", oTheme:getColor( HT_CLR_LABEL ) == "07/00" )
    Assert( "Mono:LIST_SELECTED", oTheme:getColor( HT_CLR_LIST_SELECTED ) == "00/07" )

    /* all 50 slots populated */
    VerifyAllSlots( "Mono" )

    /* restore default for any subsequent tests */
    oTheme:loadDefault()

RETURN

/*----------------------------------------------------------------*/

/** getColor() with invalid indices should return fallback "N/W" */
STATIC PROCEDURE TestInvalidIndex()

    LOCAL oTheme := HTTheme()

    Assert( "Invalid index 0", oTheme:getColor( 0 ) == "N/W" )
    Assert( "Invalid index -1", oTheme:getColor( -1 ) == "N/W" )
    Assert( "Invalid index 999", oTheme:getColor( 999 ) == "N/W" )
    Assert( "Invalid index HT_CLR_MAX+1", oTheme:getColor( HT_CLR_MAX + 1 ) == "N/W" )

RETURN

/*----------------------------------------------------------------*/

/** Every built-in theme loads, fills all slots and brings a palette */
STATIC PROCEDURE TestBuiltInThemes()

    LOCAL oTheme := HTTheme()
    LOCAL aNames := oTheme:themeNames()
    LOCAL aExpect := { "Classic", "Blue", "Light", "Dark", "Amber", "HighContrast", "Mono" }
    LOCAL cName, aPal

    Assert( "built-in themes in order", Len( aNames ) >= Len( aExpect ) .AND. ;
        AScan( aExpect, {| c, i | ! aNames[ i ] == c } ) == 0 )

    FOR EACH cName IN aExpect
        Assert( cName + ": loads", oTheme:loadTheme( cName ) )
        Assert( cName + ": is current", oTheme:themeName() == cName )
        Assert( cName + ": built-in", oTheme:isBuiltIn( cName ) )
        VerifyAllSlots( cName )
        aPal := oTheme:palette()
        Assert( cName + ": 16 palette colors", HB_ISARRAY( aPal ) .AND. Len( aPal ) == 16 .AND. ;
            AScan( aPal, {| n | ! HB_ISNUMERIC( n ) } ) == 0 )
    NEXT

    /* the blue one: white on blue, green browse with a red cursor */
    oTheme:loadBlue()
    Assert( "Blue:WINDOW", oTheme:getColor( HT_CLR_WINDOW ) == "7/1" )
    Assert( "Blue:LIST_NORMAL", oTheme:getColor( HT_CLR_LIST_NORMAL ) == "0/2" )
    Assert( "Blue:LIST_SELECTED", oTheme:getColor( HT_CLR_LIST_SELECTED ) == "15/4" )
    Assert( "Blue:TITLEBAR", oTheme:getColor( HT_CLR_TITLEBAR ) == "0/6" )
    Assert( "Blue:palette 1 = DOS blue", oTheme:paletteColor( 1 ) == ht_rgb( 0, 0, 128 ) )

    Assert( "loadTheme ignores case", oTheme:loadTheme( "amber" ) .AND. oTheme:themeName() == "Amber" )
    Assert( "loadTheme unknown = .F.", ! oTheme:loadTheme( "No such theme" ) )
    Assert( "unknown keeps the current", oTheme:themeName() == "Amber" )

    oTheme:loadDefault()

RETURN

/*----------------------------------------------------------------*/

/** Own themes: save edited colors, add, replace, remove; built-ins stay */
STATIC PROCEDURE TestOwnThemes()

    LOCAL oTheme := HTTheme()
    LOCAL aColors

    oTheme:loadDefault()
    oTheme:setColor( HT_CLR_WINDOW, "14/1" )
    oTheme:setPaletteColor( 14, ht_rgb( 1, 2, 3 ) )
    Assert( "setPaletteColor", oTheme:paletteColor( 14 ) == ht_rgb( 1, 2, 3 ) )

    Assert( "saveTheme over a built-in = .F.", ! oTheme:saveTheme( "Classic" ) )
    Assert( "saveTheme", oTheme:saveTheme( "Mine" ) )
    Assert( "saved theme is current", oTheme:themeName() == "Mine" )
    Assert( "saved theme is not built-in", oTheme:hasTheme( "Mine" ) .AND. ! oTheme:isBuiltIn( "Mine" ) )
    Assert( "added after the built-ins", ATail( oTheme:themeNames() ) == "Mine" )

    /* Classic itself is untouched */
    oTheme:loadDefault()
    Assert( "built-in colors kept", oTheme:getColor( HT_CLR_WINDOW ) == "0/9" )
    Assert( "built-in palette kept", oTheme:paletteColor( 14 ) == ht_rgb( 166, 202, 240 ) )

    Assert( "load own theme", oTheme:loadTheme( "mine" ) )
    Assert( "own colors back", oTheme:getColor( HT_CLR_WINDOW ) == "14/1" )
    Assert( "own palette back", oTheme:paletteColor( 14 ) == ht_rgb( 1, 2, 3 ) )

    /* addTheme: categories left out take the current colors */
    oTheme:loadDefault()
    aColors := Array( HT_CLR_MAX )
    aColors[ HT_CLR_TITLEBAR ] := "15/4"
    Assert( "addTheme", oTheme:addTheme( "Red title", aColors, NIL ) )
    Assert( "addTheme over a built-in = .F.", ! oTheme:addTheme( "Blue", aColors, NIL ) )
    Assert( "addTheme bad palette = .F.", ! oTheme:addTheme( "Bad", aColors, { 1, 2 } ) )
    Assert( "themeColors", oTheme:themeColors( "Red title" )[ HT_CLR_TITLEBAR ] == "15/4" .AND. ;
        oTheme:themeColors( "Red title" )[ HT_CLR_WINDOW ] == "0/9" )
    Assert( "no palette stored", oTheme:themePalette( "Red title" ) == NIL )
    Assert( "load it", oTheme:loadTheme( "Red title" ) .AND. oTheme:palette() == NIL )

    /* replace an own theme */
    aColors[ HT_CLR_TITLEBAR ] := "15/2"
    Assert( "replace own theme", oTheme:addTheme( "RED TITLE", aColors, NIL ) )
    Assert( "replaced, name kept", oTheme:themeColors( "Red title" )[ HT_CLR_TITLEBAR ] == "15/2" .AND. ;
        AScan( oTheme:themeNames(), "Red title",,, .T. ) > 0 )

    Assert( "removeTheme built-in = .F.", ! oTheme:removeTheme( "Mono" ) )
    Assert( "removeTheme", oTheme:removeTheme( "Mine" ) .AND. oTheme:removeTheme( "Red title" ) )
    Assert( "removed", ! oTheme:hasTheme( "Mine" ) .AND. ! oTheme:hasTheme( "Red title" ) )

    Assert( "categoryName WINDOW", oTheme:categoryName( HT_CLR_WINDOW ) == "WINDOW" )
    Assert( "categoryName TITLEBTN", oTheme:categoryName( HT_CLR_TITLEBTN ) == "TITLEBTN" )
    Assert( "categoryName invalid", oTheme:categoryName( 0 ) == "" )

    oTheme:loadDefault()

RETURN

/* ht_usePalette( .F. ): themes keep the application's palette */
STATIC PROCEDURE TestUsePalette()

    LOCAL lOld := ht_usePalette( .F. )

    Assert( "ht_usePalette: on by default",   lOld )
    Assert( "ht_usePalette: now off",         ! ht_usePalette() )
    Assert( "loadTheme works with it off",    HTTheme():loadTheme( "Blue" ) .AND. HTTheme():themeName() == "Blue" )
    ht_usePalette( .T. )
    HTTheme():loadTheme( "Classic" )
    Assert( "ht_usePalette: back on",         ht_usePalette() )

RETURN
