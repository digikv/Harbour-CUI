/** @file theme.prg
 * HTTheme Demo - Live theme switching with F-keys
 *
 * Demonstrates:
 *   - The built-in themes, each with its own palette: Classic, Blue (the old
 *     blue DOS look), Light, Dark, Amber (night), HighContrast, Mono
 *   - Real-time switching via function keys (onKey bindings) or the list
 *   - Label, ListBox, PushButton, CheckBox rendered under each theme
 *   - StatusBar showing the current theme name
 * samples/theme_editor changes theme colors and adds new themes.
 *
 * Keyboard:
 *   F1..F7         - Classic, Blue, Light, Dark, Amber, HighContrast, Mono
 *   Tab/Shift+Tab  - cycle focus
 *   Space          - press buttons / toggle checkbox
 *   ESC            - quit
 */

#include "hbtui.ch"
#include "inkey.ch"

STATIC oStatusBar

PROCEDURE Main()

    LOCAL app, win
    LOCAL lst, btn, chk, frm

    SetMode( 25, 60 )

    app := HTApplication():new()

    win := HTMainWindow():new()
    win:setWindowTitle( " Theme Switcher " )
    win:move( 1, 1 )
    win:resize( 55, 21 )

    /* --- Frame --- */
    frm := HTFrame():new( "Sample Controls", win )
    frm:move( 1, 1 )
    frm:resize( 24, 6 )

    /* --- Label --- */
    HTLabel():new( "Select a theme with F1-F7:", win ):move( 2, 2 )

    /* --- Checkboxes --- */
    chk := HTCheckBox():new( "Option A", win )
    chk:move( 2, 4 )

    chk := HTCheckBox():new( "Option B", win )
    chk:move( 2, 5 )

    /* --- ListBox: the themes --- */
    HTLabel():new( "Themes:", win ):move( 31, 2 )

    lst := HTListBox():new( win )
    AEval( HTTheme():themeNames(), {| c | lst:addItem( c, c ) } )
    lst:move( 31, 3 )
    lst:resize( 16, 7 )
    lst:onChanged := {|| SwitchTheme( lst:currentData() ) }

    /* --- Separator --- */
    HTSeparator():new( win ):move( 1, 10 )

    /* --- Theme buttons --- */
    HTLabel():new( "Or click a button:", win ):move( 2, 12 )

    btn := HTPushButton():new( "Classic", win )
    btn:move( 2, 14 )
    btn:onClicked := {|| SwitchTheme( "Classic" ) }

    btn := HTPushButton():new( "Blue", win )
    btn:move( 14, 14 )
    btn:onClicked := {|| SwitchTheme( "Blue" ) }

    btn := HTPushButton():new( "Dark", win )
    btn:move( 23, 14 )
    btn:onClicked := {|| SwitchTheme( "Dark" ) }

    btn := HTPushButton():new( "Amber", win )
    btn:move( 32, 14 )
    btn:onClicked := {|| SwitchTheme( "Amber" ) }

    /* --- Status Bar --- */
    oStatusBar := HTStatusBar():new( win )
    oStatusBar:move( 1, 17 )
    oStatusBar:addSection( "Theme: Classic" )
    oStatusBar:addSection( "F1-F7 to switch" )

    /* --- F-key bindings for theme switching --- */
    win:onKey( K_F1, {|| SwitchTheme( "Classic" ) } )
    win:onKey( K_F2, {|| SwitchTheme( "Blue" ) } )
    win:onKey( K_F3, {|| SwitchTheme( "Light" ) } )
    win:onKey( K_F4, {|| SwitchTheme( "Dark" ) } )
    win:onKey( K_F5, {|| SwitchTheme( "Amber" ) } )
    win:onKey( K_F6, {|| SwitchTheme( "HighContrast" ) } )
    win:onKey( K_F7, {|| SwitchTheme( "Mono" ) } )

    win:show()
    app:exec()

RETURN

STATIC PROCEDURE SwitchTheme( cName )

    IF cName == NIL .OR. ! HTTheme():loadTheme( cName )
        RETURN
    ENDIF

    IF oStatusBar != NIL
        oStatusBar:setSection( 1, "Theme: " + cName )
    ENDIF

    /* widgets take the theme colors when they paint: repaint everything */
    HTApplication():repaintAll()

RETURN
