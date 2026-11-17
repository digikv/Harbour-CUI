/** @file dialog.prg
 * Dialog Demo - Modal dialogs and message boxes
 *
 * Demonstrates:
 *   - HTMessageBox information dialog (click OK or press Enter)
 *   - HTMessageBox question dialog with result handling
 *   - Custom HTDialog with OK/Cancel buttons
 *   - HTToast feedback showing dialog results
 *
 * Keyboard:
 *   Tab/Shift+Tab - cycle buttons
 *   Enter/Space   - activate button
 *   ESC           - quit
 */

#include "hbtui.ch"
#include "inkey.ch"


PROCEDURE Main()

    LOCAL app, win, btn
    LOCAL nW, nH, nCW

    ht_cuiSetupDisplay( 40, 131 )

    nW  := MaxCol() + 1
    nH  := MaxRow() + 1
    nCW := nW
    HB_SYMBOL_UNUSED( nH )

    app := HTApplication():new()

    win := HTMainWindow():new()
    win:setWindowTitle( "Dialog Demo" )

    HTLabel():new( "Click a button to open a dialog:", win ):move( 2, 2 )

    btn := HTPushButton():new( "Information", win )
    btn:move( 2, 4 )
    btn:onClicked := {|| ;
        HTMessageBox():information( "Info", "This is an info message" ), ;
        HTToast():show( "Info dialog closed", 2000, HT_CLR_TOAST_INFO ) ;
    }

    btn := HTPushButton():new( "Question", win )
    btn:move( 18, 4 )
    btn:onClicked := {|| ShowQuestion() }

    btn := HTPushButton():new( "Custom Dialog", win )
    btn:move( 32, 4 )
    btn:onClicked := {|| ShowCustomDialog() }

    HTSeparator():new( win ):move( 1, 7 )
    HTSeparator():new( win ):resize( nCW - 2, 1 )

    HTLabel():new( "Results shown as toast notifications", win ):move( 2, 9 )

    win:show()
    app:exec()

RETURN

STATIC PROCEDURE ShowQuestion()

    LOCAL nResult

    nResult := HTMessageBox():question( "Confirm", "Do you want to proceed?" )

    IF nResult == HT_DIALOG_ACCEPTED
        HTToast():show( "User answered YES", 3000, HT_CLR_TOAST_SUCCESS )
    ELSE
        HTToast():show( "User answered NO", 3000, HT_CLR_TOAST_WARNING )
    ENDIF

RETURN

STATIC PROCEDURE ShowCustomDialog()

    LOCAL dlg, oLbl, nResult

    dlg := HTDialog():new()
    dlg:setWindowTitle( "Custom Dialog" )
    dlg:resize( 40, 10 )
    dlg:move( Int( ( ht_ScreenMaxCol() + 1 - 40 ) / 2 ), ;
              Int( ( ht_ScreenMaxRow() + 1 - 10 ) / 2 ) )

    oLbl := HTLabel():new( "Choose an action:", dlg )
    oLbl:setGeometry( 2, 2, 30, 1 )
    dlg:addButtonBar( "OK", "Cancel" )

    nResult := dlg:exec()

    IF nResult == HT_DIALOG_ACCEPTED
        HTToast():show( "Custom dialog: OK", 3000, HT_CLR_TOAST_SUCCESS )
    ELSE
        HTToast():show( "Custom dialog: Cancelled", 3000, HT_CLR_TOAST_WARNING )
    ENDIF

RETURN
