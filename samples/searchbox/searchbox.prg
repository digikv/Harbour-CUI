/** @file searchbox.prg
 * HTSearchBox demo - the searchbox of Digi knjigovodstvo in a character UI
 *
 * Demonstrates:
 *   - typing filters the firms (name, code, tax number); Latin finds Cyrillic
 *     ("nov" finds "Нова доо", "kralj" finds "Краљево")
 *   - Down / Up move the mark, Enter takes it and moves on, Esc closes the list
 *   - details on the right of a row (Šifra, PIB)
 *   - x clears, + adds a new firm (onAdd), the mouse takes an item
 *   - leaving the field with one match takes it
 *
 * Keyboard:
 *   Tab / Shift+Tab - next / previous field
 *   Alt-X           - quit
 */

#include "hbtui.ch"
#include "inkey.ch"

STATIC s_oStatus

PROCEDURE Main()

    LOCAL oApp, oWin, oFirma, oKonto, aFirme, aKonta, oLbl

    oApp := HTApplication():new()
    ht_cuiSetupDisplay( oApp:rows(), oApp:cols() )

    oWin := HTMainWindow():new()
    oWin:setWindowTitle( "HTSearchBox" )
    oWin:show()

    aFirme := { { 1, "Нова доо", "100234567" }, ;
                { 2, "Нови Пут доо", "101876543" }, ;
                { 3, "Новитет доо", "102345678" }, ;
                { 4, "Краљево промет", "103456789" }, ;
                { 5, "Џеп доо Чачак", "104567890" }, ;
                { 6, "Њива агро", "105678901" }, ;
                { 7, "Ђурђевак д.о.о.", "106789012" }, ;
                { 8, "Šumadija trade", "107890123" }, ;
                { 9, "Ласта а.д.", "108901234" }, ;
                { 10, "Морава експорт", "109012345" } }
    aKonta := { { "2040", "Купци у земљи" }, { "2041", "Купци у иностранству" }, ;
                { "4350", "Добављачи у земљи" }, { "4351", "Добављачи у иностранству" }, ;
                { "2410", "Текући рачуни" }, { "2430", "Благајна" } }

    oLbl := HTLabel():new( "Фирма:", oWin )
    oLbl:setGeometry( 2, 2, 10, 1 )
    oFirma := HTSearchBox():new( oWin )
    oFirma:setGeometry( 12, 2, 40, 1 )
    oFirma:placeholder := "Унесите назив, шифру или ПИБ..."
    oFirma:setItems( aFirme )
    oFirma:detailsBlock := {| x | { { "Шифра", x[ 1 ] }, { "ПИБ", x[ 3 ] } } }
    oFirma:onSelected := {| x, v | ShowStatus( "Фирма " + hb_ntos( v ) + ": " + x[ 2 ] ) }
    oFirma:onCleared := {|| ShowStatus( "Фирма обрисана" ) }
    oFirma:onAdd := {|| HTMessageBox():information( "Нова фирма", "Овде програм отвара унос нове фирме." ) }

    oLbl := HTLabel():new( "Конто:", oWin )
    oLbl:setGeometry( 2, 4, 10, 1 )
    oKonto := HTSearchBox():new( oWin )
    oKonto:setGeometry( 12, 4, 40, 1 )
    oKonto:placeholder := "Конто или назив..."
    oKonto:setItems( aKonta )
    oKonto:detailsBlock := {| x | { { "Конто", x[ 1 ] } } }
    oKonto:onSelected := {| x, v | ShowStatus( "Конто " + v + ": " + x[ 2 ] ) }

    s_oStatus := HTStatusBar():new( oWin )
    s_oStatus:setGeometry( 0, oWin:height - 2, oWin:width, 1 )
    s_oStatus:addSection( " Type: nov, kralj, 4350 ...   Down/Up, Enter, Esc   Alt-X quit" )

    oWin:onKey( K_ALT_X, {|| HTApplication():quit() } )
    oWin:setFocusChild( oFirma )
    oApp:exec()

RETURN

STATIC PROCEDURE ShowStatus( cText )
    s_oStatus:setSection( 1, " " + cText )
    s_oStatus:parent():repaintChild( s_oStatus )
RETURN
