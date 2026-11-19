/*
 * CuiBuilder - the dialog designer: dialogs of Harbour-CUI widgets
 *
 * Copyright (c) 2026 Srđan Dragojlović - dual licensed, see LICENSE
 *
 * A dialog is designed on the screen and kept in a .cuiform file (JSON);
 * F9 generates dlg_<name>.prg from it: the function Dlg_<Name>( hValues ),
 * which builds the dialog with the classes of Harbour-CUI (HTDialog, HTLabel,
 * HTLineEdit, HTGet, HTPushButton, HTCheckBox, HTRadioButton, HTComboBox,
 * HTListBox, HTFrame, HTSpinner, HTProgressBar, HTTextEdit), shows it and
 * returns the values of the fields in a hash (NIL = cancelled). F5 shows the
 * dialog itself, made of the same classes. (The way of the dialog designer of
 * HbIde: a design file and the code generated from it.)
 *
 *   cb_FormDesigner( cFile, hSet, bAdd )   bAdd( aFiles ): adds files to the project
 *
 * The canvas: the dialog as it will look; the selected control in the color of
 * a selection. Ins adds a control, the arrows move it, Ctrl + arrows change its
 * size (without a selected control: the size of the dialog), Tab / Shift-Tab
 * select the next / previous one, Enter its properties, Del deletes it,
 * Ctrl-PgUp / Ctrl-PgDn move it in the Tab order; the mouse selects and drags.
 */

#include "hbtui.ch"
#include "inkey.ch"
#include "cuibuild.ch"

/* the kinds of controls: { type, name in the palette, default size } */
STATIC FUNCTION cbf_Types()
RETURN { { "label",    "Label (HTLabel)",                 10, 1 }, ;
         { "edit",     "Text field (HTLineEdit)",         20, 1 }, ;
         { "get",      "GET with PICTURE (HTGet)",        30, 1 }, ;
         { "button",   "Button (HTPushButton)",           10, 1 }, ;
         { "checkbox", "Check box (HTCheckBox)",          16, 1 }, ;
         { "radio",    "Radio button (HTRadioButton)",    16, 1 }, ;
         { "combo",    "Combo box (HTComboBox)",          20, 1 }, ;
         { "list",     "List (HTListBox)",                20, 5 }, ;
         { "memo",     "Text, several lines (HTTextEdit)", 30, 5 }, ;
         { "spinner",  "Number with arrows (HTSpinner)",  20, 1 }, ;
         { "progress", "Progress bar (HTProgressBar)",    20, 1 }, ;
         { "frame",    "Frame (HTFrame)",                 30, 6 } }

/* the properties a kind has (besides col, row, width, height) */
STATIC FUNCTION cbf_Props( cType )

    SWITCH cType
    CASE "label"    ; RETURN { "text" }
    CASE "edit"     ; RETURN { "name", "text" }
    CASE "get"      ; RETURN { "name", "text", "valtype", "len", "picture", "value", "valid", "when" }
    CASE "button"   ; RETURN { "name", "text", "action" }
    CASE "checkbox"
    CASE "radio"    ; RETURN { "name", "text", "value" }
    CASE "combo"
    CASE "list"     ; RETURN { "name", "items", "value" }
    CASE "memo"     ; RETURN { "name", "text" }
    CASE "spinner"  ; RETURN { "name", "text", "min", "max", "step", "value" }
    CASE "progress" ; RETURN { "name", "value" }
    CASE "frame"    ; RETURN { "text" }
    ENDSWITCH

RETURN {}

STATIC FUNCTION cbf_PropLabel( cProp )

    SWITCH cProp
    CASE "name"    ; RETURN "Name (variable)"
    CASE "text"    ; RETURN "Text / label"
    CASE "valtype" ; RETURN "Type C N D L"
    CASE "len"     ; RETURN "Length (C)"
    CASE "picture" ; RETURN "PICTURE"
    CASE "value"   ; RETURN "Value"
    CASE "valid"   ; RETURN "VALID (expression)"
    CASE "when"    ; RETURN "WHEN (expression)"
    CASE "action"  ; RETURN "Action: ok/cancel/Func"
    CASE "items"   ; RETURN "Items (a,b,c)"
    CASE "min"     ; RETURN "Minimum"
    CASE "max"     ; RETURN "Maximum"
    CASE "step"    ; RETURN "Step"
    ENDSWITCH

RETURN cProp

/*----------------------------------------------------------- the form */

/* an empty form */
STATIC FUNCTION cbf_New( cName )

    LOCAL h := { => }

    h[ "name" ] := cb_Ident( cName )
    h[ "title" ] := cName
    h[ "width" ] := 50
    h[ "height" ] := 14
    h[ "buttons" ] := "OK,Cancel"
    h[ "controls" ] := {}

RETURN h

STATIC FUNCTION cbf_Load( cFile, cErr )

    LOCAL h, c, k

    cErr := ""
    c := hb_MemoRead( cFile )
    IF Empty( c )
        cErr := "Cannot read " + cFile
        RETURN NIL
    ENDIF
    hb_jsonDecode( c, @h )
    IF ! HB_ISHASH( h ) .OR. ! hb_HHasKey( h, "controls" )
        cErr := cFile + " is not a dialog of CuiBuilder"
        RETURN NIL
    ENDIF
    FOR EACH c IN { { "name", "Dialog" }, { "title", "" }, { "width", 50 }, { "height", 14 }, { "buttons", "" } }
        IF ! hb_HHasKey( h, c[ 1 ] )
            h[ c[ 1 ] ] := c[ 2 ]
        ENDIF
    NEXT
    h[ "width" ] := cbf_Num( h[ "width" ] )
    h[ "height" ] := cbf_Num( h[ "height" ] )
    /* positions and sizes as numbers, the rest as text (a JSON written by hand may have both) */
    FOR EACH c IN h[ "controls" ]
        FOR EACH k IN hb_HKeys( c )
            IF hb_AScan( { "x", "y", "w", "h", "len" }, k,,, .T. ) > 0
                c[ k ] := cbf_Num( c[ k ] )
            ELSEIF ! HB_ISSTRING( c[ k ] )
                c[ k ] := IIF( HB_ISLOGICAL( c[ k ] ), IIF( c[ k ], ".T.", ".F." ), hb_CStr( c[ k ] ) )
            ENDIF
        NEXT
    NEXT

RETURN h

/* a new control of a kind, at nX, nY */
STATIC FUNCTION cbf_Control( cType, nX, nY, aForm )

    LOCAL a := cbf_Types()[ hb_AScan( cbf_Types(), {| x | x[ 1 ] == cType } ) ], h := { => }, n := 1, cName

    h[ "type" ] := cType
    h[ "x" ] := nX
    h[ "y" ] := nY
    h[ "w" ] := a[ 3 ]
    h[ "h" ] := a[ 4 ]
    IF hb_AScan( cbf_Props( cType ), "name",,, .T. ) > 0
        DO WHILE .T.                    /* cName1, nAge2 ... a name not taken yet */
            cName := cbf_Prefix( cType ) + hb_ntos( n )
            IF hb_AScan( aForm, {| x | hb_HGetDef( x, "name", "" ) == cName } ) == 0
                EXIT
            ENDIF
            n++
        ENDDO
        h[ "name" ] := cName
    ENDIF
    SWITCH cType
    CASE "label"    ; h[ "text" ] := "Label:" ; EXIT
    CASE "edit"     ; h[ "text" ] := "" ; EXIT
    CASE "get"      ; h[ "text" ] := "" ; h[ "valtype" ] := "C" ; h[ "len" ] := 20 ; h[ "picture" ] := "" ; ;
                      h[ "value" ] := "" ; h[ "valid" ] := "" ; h[ "when" ] := "" ; EXIT
    CASE "button"   ; h[ "text" ] := "Button" ; h[ "action" ] := "" ; EXIT
    CASE "checkbox" ; h[ "text" ] := "Check" ; h[ "value" ] := ".F." ; EXIT
    CASE "radio"    ; h[ "text" ] := "Option" ; h[ "value" ] := ".F." ; EXIT
    CASE "combo"
    CASE "list"     ; h[ "items" ] := "One,Two,Three" ; h[ "value" ] := "1" ; EXIT
    CASE "memo"     ; h[ "text" ] := "" ; EXIT
    CASE "spinner"  ; h[ "text" ] := "" ; h[ "min" ] := "0" ; h[ "max" ] := "100" ; h[ "step" ] := "1" ; h[ "value" ] := "0" ; EXIT
    CASE "progress" ; h[ "value" ] := "0" ; EXIT
    CASE "frame"    ; h[ "text" ] := "Group" ; EXIT
    ENDSWITCH

RETURN h

STATIC FUNCTION cbf_Prefix( cType )

    SWITCH cType
    CASE "checkbox"
    CASE "radio"    ; RETURN "l"
    CASE "combo"
    CASE "list"
    CASE "spinner"
    CASE "progress" ; RETURN "n"
    CASE "button"   ; RETURN "btn"
    ENDSWITCH

RETURN "c"

/*----------------------------------------------------------- the dialog itself */

/* the dialog of a form, of the widgets of Harbour-CUI (F5, and what the generated code makes) */
STATIC FUNCTION cbf_Build( hForm, hCtl, hVar )

    LOCAL dlg := HTDialog():new(), h, o, c, cName, aBtn

    dlg:setWindowTitle( hForm[ "title" ] )
    dlg:resize( hForm[ "width" ], hForm[ "height" ] )
    FOR EACH h IN hForm[ "controls" ]
        cName := hb_HGetDef( h, "name", "" )
        o := NIL
        SWITCH h[ "type" ]
        CASE "label"
            o := HTLabel():new( h[ "text" ], dlg )
            EXIT
        CASE "edit"
            o := HTLineEdit():new( dlg )
            o:setText( h[ "text" ] )
            EXIT
        CASE "get"
            hVar[ cName ] := cbf_GetInit( h )
            o := HTGet():new( dlg )
            o:setup( cbf_VarBlock( hVar, cName ), h[ "text" ], h[ "picture" ],,,,,,, h[ "w" ] )
            EXIT
        CASE "button"
            o := HTPushButton():new( h[ "text" ], dlg )
            c := Lower( AllTrim( h[ "action" ] ) )
            DO CASE
            CASE c == "ok"
                o:onClicked := {|| dlg:accept() }
            CASE c == "cancel"
                o:onClicked := {|| dlg:reject() }
            CASE ! Empty( c )
                o:onClicked := cbf_ToastBlock( h[ "action" ] )
            ENDCASE
            EXIT
        CASE "checkbox"
            o := HTCheckBox():new( h[ "text" ], dlg )
            o:setChecked( cbf_IsTrue( h[ "value" ] ) )
            EXIT
        CASE "radio"
            o := HTRadioButton():new( h[ "text" ], dlg )
            o:setChecked( cbf_IsTrue( h[ "value" ] ) )
            EXIT
        CASE "combo"
            o := HTComboBox():new( dlg )
            AEval( hb_ATokens( h[ "items" ], "," ), {| x | o:addItem( AllTrim( x ) ) } )
            o:setCurrentIndex( Max( 1, cbf_Num( h[ "value" ] ) ) )
            EXIT
        CASE "list"
            o := HTListBox():new( dlg )
            AEval( hb_ATokens( h[ "items" ], "," ), {| x | o:addItem( AllTrim( x ) ) } )
            o:setCurrentIndex( Max( 1, cbf_Num( h[ "value" ] ) ) )
            EXIT
        CASE "memo"
            o := HTTextEdit():new( dlg )
            o:setText( h[ "text" ] )
            EXIT
        CASE "spinner"
            o := HTSpinner():new( dlg )
            o:setup( h[ "text" ], cbf_Num( h[ "min" ] ), cbf_Num( h[ "max" ] ), cbf_Num( h[ "step" ] ), cbf_Num( h[ "value" ] ) )
            EXIT
        CASE "progress"
            o := HTProgressBar():new( dlg )
            o:setValue( cbf_Num( h[ "value" ] ) )
            EXIT
        CASE "frame"
            o := HTFrame():new( h[ "text" ], dlg )
            EXIT
        ENDSWITCH
        IF o != NIL
            o:setGeometry( h[ "x" ], h[ "y" ], h[ "w" ], h[ "h" ] )
            IF ! Empty( cName )
                hCtl[ cName ] := o
            ENDIF
        ENDIF
    NEXT
    aBtn := cbf_Buttons( hForm )
    IF Len( aBtn ) > 0
        dlg:addButtonBar( hb_ArrayToParams( aBtn ) )
    ENDIF

RETURN dlg

STATIC FUNCTION cbf_VarBlock( hVar, cName )
RETURN {| x | IIF( x == NIL, hVar[ cName ], hVar[ cName ] := x ) }

STATIC FUNCTION cbf_ToastBlock( cFunc )
RETURN {|| HTToast():show( "The program calls " + AllTrim( cFunc ) + "( dlg, hCtl ) here", 2000, HT_CLR_TOAST_INFO ) }

STATIC FUNCTION cbf_Buttons( hForm )

    LOCAL a := {}, c

    FOR EACH c IN hb_ATokens( hb_HGetDef( hForm, "buttons", "" ), "," )
        IF ! Empty( c )
            AAdd( a, AllTrim( c ) )
        ENDIF
    NEXT

RETURN a

/* the start value of a GET */
STATIC FUNCTION cbf_GetInit( h )

    LOCAL c := AllTrim( hb_HGetDef( h, "value", "" ) )

    SWITCH Upper( Left( hb_HGetDef( h, "valtype", "C" ), 1 ) )
    CASE "N" ; RETURN Val( c )
    CASE "D" ; RETURN CToD( c )
    CASE "L" ; RETURN cbf_IsTrue( c )
    ENDSWITCH

RETURN PadR( c, Max( 1, cbf_Num( hb_HGetDef( h, "len", 20 ) ) ) )

/* a number of the form: "30" or 30 (JSON keeps both) */
STATIC FUNCTION cbf_Num( x )
RETURN IIF( HB_ISNUMERIC( x ), x, Val( hb_CStr( x ) ) )

STATIC FUNCTION cbf_IsTrue( x )
RETURN IIF( HB_ISLOGICAL( x ), x, Upper( AllTrim( hb_CStr( x ) ) ) $ ".T.|T|Y|YES|1|TRUE" .AND. ! Empty( x ) )

/*----------------------------------------------------------- the code */

/* the Harbour code of a form: FUNCTION Dlg_<Name>( hValues ) -> hValues / NIL */
STATIC FUNCTION cbf_Code( hForm, cFrom, cFunc )

    LOCAL c := "", cEol := hb_eol(), h, cName, cO, aBtn, aRet := {}, aGets := {}, x

    FOR EACH h IN hForm[ "controls" ]
        IF h[ "type" ] == "get" .AND. ! Empty( hb_HGetDef( h, "name", "" ) )
            AAdd( aGets, h )
        ENDIF
    NEXT
    c += "/*" + cEol
    c += " * " + "dlg_" + Lower( hForm[ "name" ] ) + ".prg - the dialog " + cFrom + ", generated by CuiBuilder." + cEol
    c += " * Generated again from the dialog designer (F9): changes made here are lost." + cEol
    c += " *" + cEol
    c += " *   " + cFunc + "( [hValues] ) -> hValues, NIL = cancelled" + cEol
    c += " *" + cEol
    c += " * hValues: the start values of the fields by their names; the result has" + cEol
    c += " * the values of the fields: " + cbf_NameList( hForm ) + cEol
    c += " */" + cEol + cEol
    c += '#include "hbtui.ch"' + cEol + cEol
    c += "FUNCTION " + cFunc + "( hValues )" + cEol + cEol
    c += "    LOCAL dlg := HTDialog():new(), hCtl := { => }" + cEol
    FOR EACH h IN aGets
        c += "    LOCAL " + h[ "name" ] + cEol
    NEXT
    c += cEol
    c += "    hb_default( @hValues, { => } )" + cEol
    FOR EACH h IN aGets
        c += "    " + h[ "name" ] + " := hb_HGetDef( hValues, " + hb_ValToExp( h[ "name" ] ) + ", " + cbf_InitExp( h ) + " )" + cEol
        IF Upper( Left( h[ "valtype" ], 1 ) ) == "C"
            c += "    " + h[ "name" ] + " := PadR( " + h[ "name" ] + ", " + hb_ntos( Max( 1, cbf_Num( h[ "len" ] ) ) ) + " )" + cEol
        ENDIF
    NEXT
    c += cEol
    c += "    dlg:setWindowTitle( " + hb_ValToExp( hForm[ "title" ] ) + " )" + cEol
    c += "    dlg:resize( " + hb_ntos( hForm[ "width" ] ) + ", " + hb_ntos( hForm[ "height" ] ) + " )" + cEol + cEol
    FOR EACH h IN hForm[ "controls" ]
        cName := hb_HGetDef( h, "name", "" )
        cO := IIF( Empty( cName ), "", 'hCtl[ "' + cName + '" ]' )
        x := IIF( Empty( cName ), "", hb_ValToExp( cName ) )
        SWITCH h[ "type" ]
        CASE "label"
            c += "    HTLabel():new( " + hb_ValToExp( h[ "text" ] ) + ", dlg ):setGeometry( " + cbf_Geo( h ) + " )" + cEol
            LOOP
        CASE "frame"
            c += "    HTFrame():new( " + hb_ValToExp( h[ "text" ] ) + ", dlg ):setGeometry( " + cbf_Geo( h ) + " )" + cEol
            LOOP
        CASE "button"
            IF Empty( cO )
                cO := "o" + hb_ntos( h:__enumIndex() )
                c += "    " + cO + " := HTPushButton():new( " + hb_ValToExp( h[ "text" ] ) + ", dlg )" + cEol
                c := StrTran( c, "LOCAL dlg := HTDialog():new(), hCtl := { => }", "LOCAL dlg := HTDialog():new(), hCtl := { => }, " + cO )
            ELSE
                c += "    " + cO + " := HTPushButton():new( " + hb_ValToExp( h[ "text" ] ) + ", dlg )" + cEol
            ENDIF
            DO CASE
            CASE Lower( AllTrim( h[ "action" ] ) ) == "ok"
                c += "    " + cO + ":onClicked := {|| dlg:accept() }" + cEol
            CASE Lower( AllTrim( h[ "action" ] ) ) == "cancel"
                c += "    " + cO + ":onClicked := {|| dlg:reject() }" + cEol
            CASE ! Empty( h[ "action" ] )
                c += "    " + cO + ":onClicked := {|| " + AllTrim( h[ "action" ] ) + "( dlg, hCtl ) }" + cEol
            ENDCASE
            EXIT
        CASE "edit"
            c += "    " + cO + " := HTLineEdit():new( dlg )" + cEol
            c += "    " + cO + ":setText( hb_HGetDef( hValues, " + x + ", " + hb_ValToExp( h[ "text" ] ) + " ) )" + cEol
            AAdd( aRet, x + " => " + cO + ":text" )
            EXIT
        CASE "get"
            c += "    " + cO + " := HTGet():new( dlg )" + cEol
            c += "    " + cO + ":setup( {| x | IIF( x == NIL, " + cName + ", " + cName + " := x ) }, " + ;
                 hb_ValToExp( h[ "text" ] ) + ", " + hb_ValToExp( h[ "picture" ] ) + ", " + ;
                 IIF( Empty( h[ "valid" ] ), "", "{|| " + h[ "valid" ] + " }" ) + ", " + ;
                 IIF( Empty( h[ "when" ] ), "", "{|| " + h[ "when" ] + " }" ) + ",,,,, " + hb_ntos( h[ "w" ] ) + " )" + cEol
            AAdd( aRet, x + " => " + cName )
            EXIT
        CASE "checkbox"
        CASE "radio"
            c += "    " + cO + " := " + IIF( h[ "type" ] == "radio", "HTRadioButton", "HTCheckBox" ) + "():new( " + hb_ValToExp( h[ "text" ] ) + ", dlg )" + cEol
            c += "    " + cO + ":setChecked( hb_HGetDef( hValues, " + x + ", " + IIF( cbf_IsTrue( h[ "value" ] ), ".T.", ".F." ) + " ) )" + cEol
            AAdd( aRet, x + " => " + cO + ":checked" )
            EXIT
        CASE "combo"
        CASE "list"
            c += "    " + cO + " := " + IIF( h[ "type" ] == "combo", "HTComboBox", "HTListBox" ) + "():new( dlg )" + cEol
            c += "    AEval( { " + cbf_ItemsExp( h[ "items" ] ) + " }, {| x | " + cO + ":addItem( x ) } )" + cEol
            c += "    " + cO + ":setCurrentIndex( hb_HGetDef( hValues, " + x + ", " + hb_ntos( Max( 1, cbf_Num( h[ "value" ] ) ) ) + " ) )" + cEol
            AAdd( aRet, x + " => " + cO + ":currentIndex()" )
            EXIT
        CASE "memo"
            c += "    " + cO + " := HTTextEdit():new( dlg )" + cEol
            c += "    " + cO + ":setText( hb_HGetDef( hValues, " + x + ", " + hb_ValToExp( h[ "text" ] ) + " ) )" + cEol
            AAdd( aRet, x + " => " + cO + ":text" )
            EXIT
        CASE "spinner"
            c += "    " + cO + " := HTSpinner():new( dlg )" + cEol
            c += "    " + cO + ":setup( " + hb_ValToExp( h[ "text" ] ) + ", " + hb_ntos( cbf_Num( h[ "min" ] ) ) + ", " + hb_ntos( cbf_Num( h[ "max" ] ) ) + ", " + ;
                 hb_ntos( cbf_Num( h[ "step" ] ) ) + ", hb_HGetDef( hValues, " + x + ", " + hb_ntos( cbf_Num( h[ "value" ] ) ) + " ) )" + cEol
            AAdd( aRet, x + " => " + cO + ":value" )
            EXIT
        CASE "progress"
            c += "    " + cO + " := HTProgressBar():new( dlg )" + cEol
            c += "    " + cO + ":setValue( hb_HGetDef( hValues, " + x + ", " + hb_ntos( cbf_Num( h[ "value" ] ) ) + " ) )" + cEol
            EXIT
        ENDSWITCH
        c += "    " + cO + ":setGeometry( " + cbf_Geo( h ) + " )" + cEol
    NEXT
    aBtn := cbf_Buttons( hForm )
    IF Len( aBtn ) > 0
        c += cEol + "    dlg:addButtonBar( "
        AEval( aBtn, {| b, n | c += IIF( n == 1, "", ", " ) + hb_ValToExp( b ) } )
        c += " )" + cEol
    ENDIF
    c += cEol + "    IF dlg:exec() != HT_DIALOG_ACCEPTED" + cEol
    c += "        RETURN NIL" + cEol
    c += "    ENDIF" + cEol + cEol
    c += "RETURN { "
    AEval( aRet, {| r, n | c += IIF( n == 1, "", ", ;" + cEol + "         " ) + r } )
    c += IIF( Len( aRet ) == 0, "=>", "" ) + " }" + cEol

RETURN c

STATIC FUNCTION cbf_Geo( h )
RETURN hb_ntos( h[ "x" ] ) + ", " + hb_ntos( h[ "y" ] ) + ", " + hb_ntos( h[ "w" ] ) + ", " + hb_ntos( h[ "h" ] )

STATIC FUNCTION cbf_ItemsExp( cItems )

    LOCAL c := "", x

    FOR EACH x IN hb_ATokens( cItems, "," )
        c += IIF( x:__enumIndex() == 1, "", ", " ) + hb_ValToExp( AllTrim( x ) )
    NEXT

RETURN c

STATIC FUNCTION cbf_InitExp( h )

    LOCAL c := AllTrim( hb_HGetDef( h, "value", "" ) )

    SWITCH Upper( Left( h[ "valtype" ], 1 ) )
    CASE "N" ; RETURN hb_ntos( Val( c ) )
    CASE "D" ; RETURN IIF( Empty( c ), 'CToD( "" )', "CToD( " + hb_ValToExp( c ) + " )" )
    CASE "L" ; RETURN IIF( cbf_IsTrue( c ), ".T.", ".F." )
    ENDSWITCH

RETURN hb_ValToExp( c )

STATIC FUNCTION cbf_NameList( hForm )

    LOCAL c := "", h

    FOR EACH h IN hForm[ "controls" ]
        IF ! Empty( hb_HGetDef( h, "name", "" ) ) .AND. ! h[ "type" ] $ "button|progress"
            c += IIF( Empty( c ), "", ", " ) + h[ "name" ]
        ENDIF
    NEXT

RETURN IIF( Empty( c ), "(none)", c )

/*----------------------------------------------------------- the designer */

PROCEDURE cb_FormDesigner( cFile, hSet, bAdd )

    LOCAL dlg := CBFormDlg():create( cFile, hSet, bAdd )

    IF dlg:hForm != NIL
        dlg:exec()
    ENDIF

RETURN

CLASS CBFormDlg FROM HTDialog

    DATA cFile
    DATA hSet
    DATA bAdd
    DATA hForm
    DATA nSel        INIT 0          /* the selected control, 0 = the dialog */
    DATA lChanged    INIT .F.
    DATA oCanvas
    DATA oInfo
    DATA oStat

    METHOD create( cFile, hSet, bAdd )
    METHOD keyEvent( keyEvent )
    METHOD refresh()
    METHOD info()
    METHOD addControl()
    METHOD props()
    METHOD formProps()
    METHOD preview()
    METHOD save()
    METHOD generate()
    METHOD moveSel( nDX, nDY, nDW, nDH )
    METHOD controlAt( nRow, nCol )

ENDCLASS

METHOD create( cFile, hSet, bAdd ) CLASS CBFormDlg

    LOCAL nW := ht_ScreenMaxCol() + 1, nH := ht_ScreenMaxRow() + 1, nInfo := 30, cErr := ""

    ::super:new()
    ::cFile := cFile
    ::hSet := hSet
    ::bAdd := bAdd
    IF hb_FileExists( cFile )
        ::hForm := cbf_Load( cFile, @cErr )
        IF ::hForm == NIL
            HTMessageBox():error( "CuiBuilder", cErr )
            RETURN self
        ENDIF
    ELSE
        ::hForm := cbf_New( hb_FNameName( cFile ) )
        ::lChanged := .T.
    ENDIF
    ::setWindowTitle( "Dialog: " + cFile )
    ::resize( nW, nH )
    ::move( 0, 0 )
    nW -= 2
    ::oCanvas := CBFormCanvas():new( self )
    ::oCanvas:oDlg := self
    ::oCanvas:setGeometry( 0, 0, nW - nInfo - 1, nH - 3 )
    ::oInfo := CBInfoPanel():new( self )
    ::oInfo:setGeometry( nW - nInfo, 0, nInfo, nH - 3 )
    ::oStat := HTLabel():new( " Ins Add  Enter Properties  Del Delete  F3 Dialog  F5 Try  F2 Save  F9 Generate code  Esc Close", self )
    ::oStat:setGeometry( 0, nH - 3, nW, 1 )
    ::setFocusChild( ::oCanvas )
    ::info()

RETURN self

METHOD refresh() CLASS CBFormDlg
    ::repaintChild( ::oCanvas )
    ::info()
RETURN NIL

/* the right side: the dialog / the selected control, the keys */
METHOD info() CLASS CBFormDlg

    LOCAL c := "", h, p, cEol := Chr( 10 )

    IF ::nSel == 0
        c += " Dialog " + ::hForm[ "name" ] + cEol
        c += " Title   " + ::hForm[ "title" ] + cEol
        c += " Size    " + hb_ntos( ::hForm[ "width" ] ) + " x " + hb_ntos( ::hForm[ "height" ] ) + cEol
        c += " Buttons " + ::hForm[ "buttons" ] + cEol
        c += " Controls " + hb_ntos( Len( ::hForm[ "controls" ] ) ) + cEol
    ELSE
        h := ::hForm[ "controls" ][ ::nSel ]
        c += " " + hb_ntos( ::nSel ) + ". " + cbf_Types()[ hb_AScan( cbf_Types(), {| x | x[ 1 ] == h[ "type" ] } ) ][ 2 ] + cEol
        c += " Col " + hb_ntos( h[ "x" ] ) + "  Row " + hb_ntos( h[ "y" ] ) + cEol
        c += " Width " + hb_ntos( h[ "w" ] ) + "  Height " + hb_ntos( h[ "h" ] ) + cEol
        FOR EACH p IN cbf_Props( h[ "type" ] )
            c += " " + PadR( p, 8 ) + Left( hb_CStr( hb_HGetDef( h, p, "" ) ), 20 ) + cEol
        NEXT
    ENDIF
    c += cEol + " Ins      add a control" + cEol + ;
         " Arrows   move" + cEol + ;
         " Ctrl+arrows  size" + cEol + ;
         " Tab      next control" + cEol + ;
         " Enter    properties" + cEol + ;
         " Del      delete" + cEol + ;
         " Ctrl-PgUp/PgDn  Tab order" + cEol + ;
         " Esc on a control: the" + cEol + "   dialog (Ctrl+arrows: its size)" + cEol + ;
         " F3 dialog  F5 try it" + cEol + ;
         " F2 save  F9 code" + cEol + ;
         " Mouse: click, drag"
    ::oInfo:aLines := hb_ATokens( c, Chr( 10 ) )
    ::repaintChild( ::oInfo )

RETURN NIL

METHOD addControl() CLASS CBFormDlg

    LOCAL aNames := {}, n, nX := 1, nY := 1, h

    AEval( cbf_Types(), {| x | AAdd( aNames, x[ 2 ] ) } )
    n := cb_List( "Add a control", aNames, 1 )
    IF n == 0
        RETURN NIL
    ENDIF
    IF ::nSel > 0                        /* under the selected one */
        h := ::hForm[ "controls" ][ ::nSel ]
        nX := h[ "x" ]
        nY := Min( h[ "y" ] + h[ "h" ], ::hForm[ "height" ] - 5 )
    ENDIF
    AAdd( ::hForm[ "controls" ], cbf_Control( cbf_Types()[ n ][ 1 ], nX, Max( 0, nY ), ::hForm[ "controls" ] ) )
    ::nSel := Len( ::hForm[ "controls" ] )
    ::lChanged := .T.
    ::refresh()

RETURN NIL

/* Enter: the properties of the selected control */
METHOD props() CLASS CBFormDlg

    LOCAL h, aP, dlg, nW := Min( 72, ht_ScreenMaxCol() - 3 ), nH, aEd := {}, p, n := 0, o, cErr := ""

    IF ::nSel == 0
        RETURN ::formProps()
    ENDIF
    h := ::hForm[ "controls" ][ ::nSel ]
    aP := { "x", "y", "w", "h" }
    AEval( cbf_Props( h[ "type" ] ), {| x | AAdd( aP, x ) } )
    nH := Len( aP ) + 5
    dlg := HTDialog():new()
    dlg:setWindowTitle( "Properties: " + h[ "type" ] + IIF( Empty( hb_HGetDef( h, "name", "" ) ), "", " " + h[ "name" ] ) )
    dlg:resize( nW, nH )
    dlg:move( Int( ( ht_ScreenMaxCol() + 1 - nW ) / 2 ), Int( ( ht_ScreenMaxRow() + 1 - nH ) / 2 ) )
    FOR EACH p IN aP
        HTLabel():new( IIF( p == "x", "Column", IIF( p == "y", "Row", IIF( p == "w", "Width", IIF( p == "h", "Height", cbf_PropLabel( p ) ) ) ) ), dlg ):setGeometry( 1, n + 1, 24, 1 )
        o := CBEdit():new( dlg )
        o:setGeometry( 26, n + 1, nW - 29, 1 )
        o:setText( hb_CStr( hb_HGetDef( h, p, "" ) ) )
        AAdd( aEd, o )
        n++
    NEXT
    dlg:addButtonBar( "OK", "Cancel" )
    dlg:setFocusChild( aEd[ IIF( Len( aEd ) > 4, 5, 1 ) ] )
    IF dlg:exec() != HT_DIALOG_ACCEPTED
        RETURN NIL
    ENDIF
    FOR EACH p IN aP
        o := aEd[ p:__enumIndex() ]
        IF p $ "x|y|w|h|len"
            h[ p ] := Max( IIF( p $ "w|h|len", 1, 0 ), Int( Val( o:text ) ) )
        ELSEIF p == "name"
            IF ! Empty( o:text ) .AND. ! cb_Ident( AllTrim( o:text ) ) == AllTrim( o:text )
                cErr := "The name must be a Harbour name (letters, digits, _): " + o:text
            ELSE
                h[ p ] := AllTrim( o:text )
            ENDIF
        ELSEIF p == "valtype"
            h[ p ] := IIF( Upper( Left( AllTrim( o:text ), 1 ) ) $ "CNDL" .AND. ! Empty( o:text ), Upper( Left( AllTrim( o:text ), 1 ) ), "C" )
        ELSE
            h[ p ] := o:text
        ENDIF
    NEXT
    IF ! Empty( cErr )
        HTMessageBox():warning( "CuiBuilder", cErr )
    ENDIF
    ::lChanged := .T.
    ::refresh()

RETURN NIL

/* F3: the name, title, size and buttons of the dialog */
METHOD formProps() CLASS CBFormDlg

    LOCAL dlg, nW := Min( 72, ht_ScreenMaxCol() - 3 ), nH := 10, aEd := {}, aP := { "name", "title", "width", "height", "buttons" }, p, o
    LOCAL aL := { "Name (Dlg_<name>)", "Title", "Width", "Height", "Buttons (OK,Cancel)" }

    dlg := HTDialog():new()
    dlg:setWindowTitle( "Dialog" )
    dlg:resize( nW, nH )
    dlg:move( Int( ( ht_ScreenMaxCol() + 1 - nW ) / 2 ), Int( ( ht_ScreenMaxRow() + 1 - nH ) / 2 ) )
    FOR EACH p IN aP
        HTLabel():new( aL[ p:__enumIndex() ], dlg ):setGeometry( 1, p:__enumIndex(), 22, 1 )
        o := CBEdit():new( dlg )
        o:setGeometry( 24, p:__enumIndex(), nW - 27, 1 )
        o:setText( hb_CStr( ::hForm[ p ] ) )
        AAdd( aEd, o )
    NEXT
    dlg:addButtonBar( "OK", "Cancel" )
    IF dlg:exec() != HT_DIALOG_ACCEPTED
        RETURN NIL
    ENDIF
    ::hForm[ "name" ] := cb_Ident( AllTrim( aEd[ 1 ]:text ) )
    ::hForm[ "title" ] := aEd[ 2 ]:text
    ::hForm[ "width" ] := Min( ht_ScreenMaxCol() + 1, Max( 20, Int( Val( aEd[ 3 ]:text ) ) ) )
    ::hForm[ "height" ] := Min( ht_ScreenMaxRow() + 1, Max( 5, Int( Val( aEd[ 4 ]:text ) ) ) )
    ::hForm[ "buttons" ] := AllTrim( aEd[ 5 ]:text )
    ::lChanged := .T.
    ::refresh()

RETURN NIL

/* arrows / Ctrl + arrows: move or size the selection (or the dialog) */
METHOD moveSel( nDX, nDY, nDW, nDH ) CLASS CBFormDlg

    LOCAL h

    IF ::nSel == 0
        ::hForm[ "width" ] := Min( ht_ScreenMaxCol() + 1, Max( 20, ::hForm[ "width" ] + nDW ) )
        ::hForm[ "height" ] := Min( ht_ScreenMaxRow() + 1, Max( 5, ::hForm[ "height" ] + nDH ) )
    ELSE
        h := ::hForm[ "controls" ][ ::nSel ]
        h[ "x" ] := Max( 0, h[ "x" ] + nDX )
        h[ "y" ] := Max( 0, h[ "y" ] + nDY )
        h[ "w" ] := Max( 1, h[ "w" ] + nDW )
        h[ "h" ] := Max( 1, h[ "h" ] + nDH )
    ENDIF
    ::lChanged := .T.
    ::refresh()

RETURN NIL

/* the control at a cell of the dialog's content (the last one drawn wins) */
METHOD controlAt( nRow, nCol ) CLASS CBFormDlg

    LOCAL i, h

    FOR i := Len( ::hForm[ "controls" ] ) TO 1 STEP -1
        h := ::hForm[ "controls" ][ i ]
        IF nRow >= h[ "y" ] .AND. nRow < h[ "y" ] + h[ "h" ] .AND. nCol >= h[ "x" ] .AND. nCol < h[ "x" ] + h[ "w" ] .AND. ;
           ( h[ "type" ] != "frame" .OR. nRow == h[ "y" ] .OR. nRow == h[ "y" ] + h[ "h" ] - 1 .OR. nCol == h[ "x" ] .OR. nCol == h[ "x" ] + h[ "w" ] - 1 )
            RETURN i
        ENDIF
    NEXT

RETURN 0

/* F5: the dialog itself, of the widgets of Harbour-CUI */
METHOD preview() CLASS CBFormDlg

    LOCAL hCtl := { => }, hVar := { => }, dlg, n, c := "", k

    dlg := cbf_Build( ::hForm, hCtl, hVar )
    n := dlg:exec()
    IF n == HT_DIALOG_ACCEPTED
        FOR EACH k IN hb_HKeys( hVar )
            c += k + " = " + hb_ValToExp( hVar[ k ] ) + Chr( 10 )
        NEXT
        FOR EACH k IN hb_HKeys( hCtl )
            IF hCtl[ k ]:isDerivedFrom( "HTLineEdit" ) .OR. hCtl[ k ]:isDerivedFrom( "HTTextEdit" )
                c += k + " = " + hb_ValToExp( hCtl[ k ]:text ) + Chr( 10 )
            ELSEIF hCtl[ k ]:isDerivedFrom( "HTCheckBox" ) .OR. hCtl[ k ]:isDerivedFrom( "HTRadioButton" )
                c += k + " = " + hb_ValToExp( hCtl[ k ]:checked ) + Chr( 10 )
            ELSEIF hCtl[ k ]:isDerivedFrom( "HTComboBox" ) .OR. hCtl[ k ]:isDerivedFrom( "HTListBox" )
                c += k + " = " + hb_ntos( hCtl[ k ]:currentIndex() ) + Chr( 10 )
            ELSEIF hCtl[ k ]:isDerivedFrom( "HTSpinner" )
                c += k + " = " + hb_ntos( hCtl[ k ]:value ) + Chr( 10 )
            ENDIF
        NEXT
        HTMessageBox():information( "The values", IIF( Empty( c ), "(no fields)", c ) )
    ENDIF
    ::repaint()

RETURN NIL

METHOD save() CLASS CBFormDlg

    IF ! hb_MemoWrit( ::cFile, hb_jsonEncode( ::hForm, .T. ) )
        HTMessageBox():error( "CuiBuilder", "Cannot write " + ::cFile )
        RETURN .F.
    ENDIF
    ::lChanged := .F.
    HTToast():show( "Saved: " + hb_FNameNameExt( ::cFile ), 1500, HT_CLR_TOAST_SUCCESS )

RETURN .T.

/* F9: dlg_<name>.prg next to the .cuiform, added to the project */
METHOD generate() CLASS CBFormDlg

    LOCAL cFunc := "Dlg_" + ::hForm[ "name" ], cPrg := hb_FNameDir( ::cFile ) + "dlg_" + Lower( ::hForm[ "name" ] ) + ".prg"
    LOCAL h, aSeen := {}

    FOR EACH h IN ::hForm[ "controls" ]
        IF ! Empty( hb_HGetDef( h, "name", "" ) )
            IF hb_AScan( aSeen, Lower( h[ "name" ] ),,, .T. ) > 0
                HTMessageBox():warning( "CuiBuilder", "Two controls have the name " + h[ "name" ] )
                RETURN NIL
            ENDIF
            AAdd( aSeen, Lower( h[ "name" ] ) )
        ENDIF
    NEXT
    IF ! ::save()
        RETURN NIL
    ENDIF
    IF ! hb_MemoWrit( cPrg, cbf_Code( ::hForm, hb_FNameNameExt( ::cFile ), cFunc ) )
        HTMessageBox():error( "CuiBuilder", "Cannot write " + cPrg )
        RETURN NIL
    ENDIF
    IF ::bAdd != NIL
        Eval( ::bAdd, { cPrg } )
    ENDIF
    HTMessageBox():information( "CuiBuilder", "Generated: " + cPrg + Chr( 10 ) + Chr( 10 ) + ;
        "In the program:" + Chr( 10 ) + ;
        "   hVal := " + cFunc + "( { " + IIF( Len( aSeen ) > 0, '"' + ATail( ::hForm[ "controls" ] )[ "type" ] + '" => ...', "" ) + " } )" + Chr( 10 ) + ;
        "   IF hVal != NIL" + Chr( 10 ) + "      ... hVal[ " + IIF( Len( aSeen ) > 0, '"' + aSeen[ 1 ] + '"', '"name"' ) + " ] ..." + Chr( 10 ) + "   ENDIF" )

RETURN NIL

METHOD PROCEDURE keyEvent( keyEvent ) CLASS CBFormDlg

    LOCAL nKey := keyEvent:key, n, a, nCount := Len( ::hForm[ "controls" ] )

    keyEvent:accept()
    SWITCH nKey
    CASE K_ESC
        IF ::nSel > 0
            ::nSel := 0
            ::refresh()
            RETURN
        ENDIF
        IF ::lChanged
            n := HTMessageBox():choose( "CuiBuilder", hb_FNameNameExt( ::cFile ) + " has been changed. Save it?", ;
                                        { "&Save", "&Don't save", "&Cancel" }, HT_MSG_QUESTION )
            IF n == 3 .OR. n == 0 .OR. ( n == 1 .AND. ! ::save() )
                RETURN
            ENDIF
        ENDIF
        ::accept()
        RETURN
    CASE K_INS        ; ::addControl() ; RETURN
    CASE K_ENTER      ; ::props() ; RETURN
    CASE K_F3         ; ::formProps() ; RETURN
    CASE K_F5         ; ::preview() ; RETURN
    CASE K_F2         ; ::save() ; RETURN
    CASE K_F9         ; ::generate() ; RETURN
    CASE K_DEL
        IF ::nSel > 0
            hb_ADel( ::hForm[ "controls" ], ::nSel, .T. )
            ::nSel := Min( ::nSel, Len( ::hForm[ "controls" ] ) )
            ::lChanged := .T.
            ::refresh()
        ENDIF
        RETURN
    CASE K_TAB
        ::nSel := IIF( nCount == 0, 0, Int( ::nSel % nCount ) + 1 )
        ::refresh()
        RETURN
    CASE K_SH_TAB
        ::nSel := IIF( nCount == 0, 0, IIF( ::nSel <= 1, nCount, ::nSel - 1 ) )
        ::refresh()
        RETURN
    CASE K_LEFT       ; ::moveSel( -1, 0, 0, 0 ) ; RETURN
    CASE K_RIGHT      ; ::moveSel( 1, 0, 0, 0 ) ; RETURN
    CASE K_UP         ; ::moveSel( 0, -1, 0, 0 ) ; RETURN
    CASE K_DOWN       ; ::moveSel( 0, 1, 0, 0 ) ; RETURN
    CASE K_CTRL_LEFT  ; ::moveSel( 0, 0, -1, 0 ) ; RETURN
    CASE K_CTRL_RIGHT ; ::moveSel( 0, 0, 1, 0 ) ; RETURN
    CASE K_CTRL_UP    ; ::moveSel( 0, 0, 0, -1 ) ; RETURN
    CASE K_CTRL_DOWN  ; ::moveSel( 0, 0, 0, 1 ) ; RETURN
    CASE K_CTRL_PGUP
    CASE K_CTRL_PGDN
        n := ::nSel + IIF( nKey == K_CTRL_PGUP, -1, 1 )
        IF ::nSel > 0 .AND. n >= 1 .AND. n <= nCount
            a := ::hForm[ "controls" ][ n ]
            ::hForm[ "controls" ][ n ] := ::hForm[ "controls" ][ ::nSel ]
            ::hForm[ "controls" ][ ::nSel ] := a
            ::nSel := n
            ::lChanged := .T.
            ::refresh()
        ENDIF
        RETURN
    ENDSWITCH
    ::super:keyEvent( keyEvent )

RETURN

/* the text on the right of the designer, line by line */
CLASS CBInfoPanel FROM HTWidget
    DATA aLines     INIT {}
    METHOD new( parent )
    METHOD paintEvent( event )
ENDCLASS

METHOD new( parent ) CLASS CBInfoPanel
    ::super:new( parent )
    ::setFocusPolicy( HT_FOCUS_NONE )
    ::FisVisible := .T.
RETURN self

METHOD PROCEDURE paintEvent( event ) CLASS CBInfoPanel

    LOCAL n, nW := MaxCol() + 1, cClr := HTTheme():getColor( HT_CLR_LABEL )

    HB_SYMBOL_UNUSED( event )
    FOR n := 0 TO MaxRow()
        DispOutAt( n, 0, ht_cellPadR( IIF( n < Len( ::aLines ), ::aLines[ n + 1 ], "" ), nW ), cClr )
    NEXT

RETURN

/*----------------------------------------------------------- the canvas */

/* the dialog being designed, drawn as it will look */
CLASS CBFormCanvas FROM HTWidget
    DATA oDlg
    DATA nOffX      INIT 2           /* where the dialog's frame starts on the canvas */
    DATA nOffY      INIT 1
    DATA lDrag      INIT .F.
    DATA nDragX     INIT 0
    DATA nDragY     INIT 0
    METHOD new( parent )
    METHOD paintEvent( event )
    METHOD mouseEvent( eventMouse )
    METHOD mouseDragWanted() INLINE .T.
ENDCLASS

METHOD new( parent ) CLASS CBFormCanvas
    ::super:new( parent )
    ::setFocusPolicy( HT_FOCUS_STRONG )
    ::FisVisible := .T.
RETURN self

METHOD PROCEDURE paintEvent( event ) CLASS CBFormCanvas

    LOCAL d := ::oDlg, hF := d:hForm, nW := hF[ "width" ], nH := hF[ "height" ], nT := ::nOffY, nL := ::nOffX
    LOCAL cWin := HTTheme():getColor( HT_CLR_WINDOW ), cSel := HTTheme():getColor( HT_CLR_LIST_SELECTED )
    LOCAL h, cTitle, nCW := nW - 2, nCH := nH - 3, aBtn, c, nX, y, cClr, nMaxC := MaxCol(), nMaxR := MaxRow()

    HB_SYMBOL_UNUSED( event )
    DispBox( 0, 0, nMaxR, nMaxC, Replicate( " ", 9 ), HTTheme():getColor( HT_CLR_DESKTOP ) )

    /* the frame and the title of the dialog (in a dialog the content starts at col 1, row 1) */
    DispBox( nT, nL, nT + nH - 1, nL + nW - 1, Replicate( " ", 9 ), cWin )
    DispBox( nT, nL, nT + nH - 1, nL + nW - 1, , IIF( d:nSel == 0 .AND. ::hasFocus(), cSel, cWin ) )
    cTitle := " " + hF[ "title" ] + " "
    DispOutAt( nT, nL + 2, ht_cellLeft( cTitle, nW - 4 ), HTTheme():getColor( HT_CLR_TITLEBAR ) )
    nT++
    nL++

    FOR EACH h IN hF[ "controls" ]
        cClr := IIF( h:__enumIndex() == d:nSel, cSel, NIL )
        cbf_Draw( h, nT, nL, cClr, nCW, nCH )
    NEXT

    /* the buttons of addButtonBar(), right-aligned on the last row */
    aBtn := cbf_Buttons( hF )
    IF Len( aBtn ) > 0
        nX := nL + nCW
        y := nT + nCH - 1
        FOR c := Len( aBtn ) TO 1 STEP -1
            nX -= ht_strWidth( aBtn[ c ] ) + 5
            DispOutAt( y, nX, "[ " + aBtn[ c ] + " ]", HTTheme():getColor( HT_CLR_BUTTON_NORMAL ) )
        NEXT
    ENDIF

RETURN

/* one control on the canvas, clipped to the content of the dialog */
STATIC PROCEDURE cbf_Draw( h, nT, nL, cSel, nCW, nCH )

    LOCAL nY := nT + h[ "y" ], nX := nL + h[ "x" ], nW := Min( h[ "w" ], nCW - h[ "x" ] ), nHt := Min( h[ "h" ], nCH - h[ "y" ] )
    LOCAL c, cClr, i, cLab, aItems, nLab

    IF nW < 1 .OR. nHt < 1
        RETURN
    ENDIF
    SWITCH h[ "type" ]
    CASE "label"
        DispOutAt( nY, nX, ht_cellPadR( h[ "text" ], nW ), hb_defaultValue( cSel, HTTheme():getColor( HT_CLR_LABEL ) ) )
        EXIT
    CASE "edit"
        DispOutAt( nY, nX, ht_cellPadR( IIF( Empty( h[ "text" ] ), h[ "name" ], h[ "text" ] ), nW ), ;
                   hb_defaultValue( cSel, HTTheme():getColor( HT_CLR_LINEEDIT_NORMAL ) ) )
        EXIT
    CASE "get"
        cLab := h[ "text" ]
        nLab := IIF( Empty( cLab ), 0, Min( nW - 1, ht_strWidth( cLab ) + 1 ) )
        IF nLab > 0
            DispOutAt( nY, nX, ht_cellPadR( cLab, nLab ), hb_defaultValue( cSel, HTTheme():getColor( HT_CLR_GET_LABEL ) ) )
        ENDIF
        c := Transform( cbf_GetInit( h ), h[ "picture" ] )
        DispOutAt( nY, nX + nLab, ht_cellPadR( IIF( Empty( c ), h[ "name" ], c ), nW - nLab ), ;
                   hb_defaultValue( cSel, HTTheme():getColor( HT_CLR_GET_NORMAL ) ) )
        EXIT
    CASE "button"
        DispOutAt( nY, nX, ht_cellPadR( "[ " + h[ "text" ] + " ]", nW ), hb_defaultValue( cSel, HTTheme():getColor( HT_CLR_BUTTON_NORMAL ) ) )
        EXIT
    CASE "checkbox"
    CASE "radio"
        c := IIF( h[ "type" ] == "radio", IIF( cbf_IsTrue( h[ "value" ] ), "(*) ", "( ) " ), IIF( cbf_IsTrue( h[ "value" ] ), "[x] ", "[ ] " ) )
        DispOutAt( nY, nX, ht_cellPadR( c + h[ "text" ], nW ), hb_defaultValue( cSel, HTTheme():getColor( HT_CLR_CHECK_NORMAL ) ) )
        EXIT
    CASE "combo"
        aItems := hb_ATokens( h[ "items" ], "," )
        c := IIF( Len( aItems ) >= Max( 1, cbf_Num( h[ "value" ] ) ), AllTrim( aItems[ Max( 1, cbf_Num( h[ "value" ] ) ) ] ), "" )
        DispOutAt( nY, nX, ht_cellPadR( c, Max( 0, nW - 1 ) ) + ht_glyph( 0x25BC, "v" ), hb_defaultValue( cSel, HTTheme():getColor( HT_CLR_COMBO_NORMAL ) ) )
        EXIT
    CASE "list"
    CASE "memo"
        cClr := hb_defaultValue( cSel, HTTheme():getColor( IIF( h[ "type" ] == "list", HT_CLR_LIST_NORMAL, HT_CLR_LINEEDIT_NORMAL ) ) )
        aItems := IIF( h[ "type" ] == "list", hb_ATokens( h[ "items" ], "," ), hb_ATokens( h[ "text" ], Chr( 10 ) ) )
        FOR i := 1 TO nHt
            c := IIF( i <= Len( aItems ), AllTrim( aItems[ i ] ), "" )
            IF i == 1 .AND. Empty( c )
                c := h[ "name" ]
            ENDIF
            DispOutAt( nY + i - 1, nX, ht_cellPadR( c, nW ), cClr )
        NEXT
        EXIT
    CASE "spinner"
        c := IIF( Empty( h[ "text" ] ), "", h[ "text" ] + "  " ) + "[" + PadL( h[ "value" ], 5 ) + " ]" + ht_glyph( 0x2195, "^" )
        DispOutAt( nY, nX, ht_cellPadR( c, nW ), hb_defaultValue( cSel, HTTheme():getColor( HT_CLR_SPINNER_NORMAL ) ) )
        EXIT
    CASE "progress"
        i := Int( nW * Min( 100, Max( 0, cbf_Num( h[ "value" ] ) ) ) / 100 )
        DispOutAt( nY, nX, Space( i ), hb_defaultValue( cSel, HTTheme():getColor( HT_CLR_PROGRESS_FILL ) ) )
        DispOutAt( nY, nX + i, Space( nW - i ), hb_defaultValue( cSel, HTTheme():getColor( HT_CLR_PROGRESS_EMPTY ) ) )
        EXIT
    CASE "frame"
        cClr := hb_defaultValue( cSel, HTTheme():getColor( HT_CLR_FRAME ) )
        IF nHt >= 2 .AND. nW >= 2
            DispBox( nY, nX, nY + nHt - 1, nX + nW - 1, , cClr )
        ENDIF
        IF ! Empty( h[ "text" ] ) .AND. nW > 4
            DispOutAt( nY, nX + 2, ht_cellLeft( " " + h[ "text" ] + " ", nW - 4 ), cClr )
        ENDIF
        EXIT
    ENDSWITCH

RETURN

/* a click selects the control under the mouse (else the dialog), a drag moves it */
METHOD PROCEDURE mouseEvent( eventMouse ) CLASS CBFormCanvas

    LOCAL d := ::oDlg, nRow := eventMouse:mouseRow - ::nOffY - 1, nCol := eventMouse:mouseCol - ::nOffX - 1, n, h

    SWITCH eventMouse:nKey
    CASE K_LBUTTONDOWN
        n := d:controlAt( nRow, nCol )
        d:nSel := n
        ::lDrag := n > 0
        IF n > 0
            h := d:hForm[ "controls" ][ n ]
            ::nDragX := nCol - h[ "x" ]
            ::nDragY := nRow - h[ "y" ]
        ENDIF
        d:refresh()
        EXIT
    CASE K_MOUSEMOVE
        IF ::lDrag .AND. d:nSel > 0
            h := d:hForm[ "controls" ][ d:nSel ]
            IF !( h[ "x" ] == Max( 0, nCol - ::nDragX ) .AND. h[ "y" ] == Max( 0, nRow - ::nDragY ) )
                h[ "x" ] := Max( 0, nCol - ::nDragX )
                h[ "y" ] := Max( 0, nRow - ::nDragY )
                d:lChanged := .T.
                d:refresh()
            ENDIF
        ENDIF
        EXIT
    CASE K_LBUTTONUP
        ::lDrag := .F.
        EXIT
    CASE K_LDBLCLK
        IF d:nSel > 0
            d:props()
        ENDIF
        EXIT
    ENDSWITCH

RETURN
