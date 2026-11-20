/** @file datepicker.prg
 * HTDatePicker demo - the date field of Digi knjigovodstvo in a character UI
 *
 * Demonstrates:
 *   - typing 20112026 gives 20.11.2026 (the dots come by themselves); a wrong
 *     date (31.02.2026) goes back when the field is left
 *   - the calendar: Alt-Down / F4 or a click on the symbol; the week from Monday,
 *     today in [ ], arrows / PgUp / PgDn / Ctrl-PgUp / Ctrl-PgDn, Enter takes the
 *     day, T = today, Esc closes
 *   - other languages: setLang( "en-us" ) (mm/dd/yyyy, the week from Sunday), setLang( "zh" );
 *     ht_dateDefaultLang( "de" ) for the whole program
 *
 * Keyboard:
 *   Tab / Shift+Tab - next / previous field
 *   Alt-X           - quit
 */

#include "hbtui.ch"
#include "inkey.ch"

STATIC s_oStatus

PROCEDURE Main()

    LOCAL oApp, oWin, oDatum, oValuta, oLbl, oFirst

    oApp := HTApplication():new()
    ht_cuiSetupDisplay( oApp:rows(), oApp:cols() )

    oWin := HTMainWindow():new()
    oWin:setWindowTitle( "HTDatePicker" )
    oWin:show()

    oLbl := HTLabel():new( "Датум:", oWin )
    oLbl:setGeometry( 2, 2, 12, 1 )
    oDatum := HTDatePicker():new( oWin )
    oDatum:setGeometry( 14, 2, 13, 1 )
    oDatum:placeholder := "дд.мм.гггг"
    oDatum:setValue( Date() )
    oDatum:onChanged := {| d | ShowStatus( "Датум: " + ht_dateText( d ) ) }
    oFirst := oDatum

    oLbl := HTLabel():new( "Due date:", oWin )
    oLbl:setGeometry( 2, 4, 12, 1 )
    oValuta := HTDatePicker():new( oWin )
    oValuta:setGeometry( 14, 4, 13, 1 )
    oValuta:setLang( "en-us" )            /* mm/dd/yyyy, the week from Sunday */
    oValuta:placeholder := "mm/dd/yyyy"
    oValuta:onChanged := {| d | ShowStatus( "Due date: " + ht_dateText( d, "mm/dd/yyyy" ) ) }

    oLbl := HTLabel():new( "日期:", oWin )
    oLbl:setGeometry( 2, 6, 12, 1 )
    oDatum := HTDatePicker():new( oWin )
    oDatum:setGeometry( 14, 6, 13, 1 )
    oDatum:setLang( "zh" )                 /* yyyy-mm-dd */
    oDatum:setValue( Date() )

    s_oStatus := HTStatusBar():new( oWin )
    s_oStatus:setGeometry( 0, oWin:height - 2, oWin:width, 1 )
    s_oStatus:addSection( " Type 20112026 ...   Alt-Down / F4 the calendar   Alt-X quit" )

    oWin:onKey( K_ALT_X, {|| HTApplication():quit() } )
    oWin:setFocusChild( oFirst )
    oApp:exec()

RETURN

STATIC PROCEDURE ShowStatus( cText )
    s_oStatus:setSection( 1, " " + cText )
    s_oStatus:parent():repaintChild( s_oStatus )
RETURN
