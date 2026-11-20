/*
 * CuiBuilder - the dialog designer: dialogs of Harbour-CUI widgets
 *
 * Copyright (c) 2026 Srđan Dragojlović - dual licensed, see LICENSE
 *
 * A dialog is designed on the screen and kept in a .cuiform file (JSON);
 * F9 generates dlg_<name>.prg from it: the function Dlg_<Name>( hValues ),
 * which builds the dialog with the classes of Harbour-CUI (HTDialog, HTLabel,
 * HTLineEdit, HTGet, HTPushButton, HTCheckBox, HTRadioButton, HTComboBox,
 * HTListBox, HTFrame, HTSeparator, HTSpinner, HTProgressBar, HTTextEdit), shows it and
 * returns the values of the fields in a hash (NIL = cancelled). F5 shows the
 * dialog itself, made of the same classes. (The way of the dialog designer of
 * HbIde: a design file and the code generated from it.)
 *
 *   cb_FormDesigner( cFile, hSet, bAdd )   bAdd( aFiles ): adds files to the project
 *
 * The canvas: the dialog as it will look - its widgets are built as for F5 and
 * paint themselves, under the title bar of a dialog; the selected control in
 * the color of a selection. Ins adds a control, the arrows move it, Ctrl + arrows change its
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
         { "frame",    "Frame (HTFrame)",                 30, 6 }, ;
         { "separator", "Line (HTSeparator)",             40, 1 } }

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
    CASE "name"    ; RETURN "Name (var)"
    CASE "text"    ; RETURN "Text"
    CASE "valtype" ; RETURN "Type C/N/D/L"
    CASE "len"     ; RETURN "Length (C)"
    CASE "picture" ; RETURN "PICTURE"
    CASE "value"   ; RETURN "Value"
    CASE "valid"   ; RETURN "VALID (expr)"
    CASE "when"    ; RETURN "WHEN (expr)"
    CASE "action"  ; RETURN "Action"
    CASE "items"   ; RETURN "Items (a,b,c)"
    CASE "min"     ; RETURN "Minimum"
    CASE "max"     ; RETURN "Maximum"
    CASE "step"    ; RETURN "Step"
    ENDSWITCH

RETURN cProp

/*-------------------------------------------------------- the form */

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

/* the dialog of a form, of the widgets of Harbour-CUI (F5, the canvas, and what the generated
   code makes); aObj (optional) gets the widget of each control, in the order of the controls */
STATIC FUNCTION cbf_Build( hForm, hCtl, hVar, aObj )

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
        CASE "separator"
            o := HTSeparator():new( dlg )
            EXIT
        ENDSWITCH
        IF aObj != NIL
            AAdd( aObj, o )
        ENDIF
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
        CASE "separator"
            c += "    HTSeparator():new( dlg ):setGeometry( " + cbf_Geo( h ) + " )" + cEol
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

/* TBrowse skip over an array; aRec[ 1 ] is the current row */
STATIC FUNCTION cbf_Skip( aRec, nLen, nRequest )

    LOCAL nActual := IIF( nRequest > 0, Min( nRequest, nLen - aRec[ 1 ] ), Max( nRequest, 1 - aRec[ 1 ] ) )

    aRec[ 1 ] += nActual

RETURN nActual

/* a value for the browse: padded, so more can be typed (it is RTrim()-ed back) */
STATIC FUNCTION cbf_Room( c )
RETURN PadR( c, Len( c ) + 100 )

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
    DATA oSelLbl                     /* the selected control (or the dialog), over the properties */
    DATA oProps                      /* the properties: Property | Value, edited in place */
    DATA aRows       INIT {}         /* the rows of oProps: { label, key } */
    DATA aRec        INIT { 1 }      /* the current row of oProps */
    DATA nPropSel    INIT -1         /* the selection the rows were made for */
    DATA oInfo
    DATA oStat

    METHOD create( cFile, hSet, bAdd )
    METHOD keyEvent( keyEvent )
    METHOD refresh()
    METHOD info()
    METHOD addControl()
    METHOD propRow( n )
    METHOD propGet( cKey )
    METHOD propSet( cKey, cValue )
    METHOD focusProps( lOn )
    METHOD preview()
    METHOD save()
    METHOD generate()
    METHOD moveSel( nDX, nDY, nDW, nDH )
    METHOD controlAt( nRow, nCol )

ENDCLASS

METHOD create( cFile, hSet, bAdd ) CLASS CBFormDlg

    LOCAL nW := ht_ScreenMaxCol() + 1, nH := ht_ScreenMaxRow() + 1, nInfo := 36, cErr := "", nPH

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

    /* on the right: the selection, its properties (a browse), the keys */
    ::oSelLbl := HTLabel():new( "", self )
    ::oSelLbl:setGeometry( nW - nInfo, 0, nInfo, 1 )
    nPH := Max( 8, nH - 3 - 1 - 19 )         /* the keys below: 19 rows */
    ::oProps := HTBrowse():new( self )
    ::oProps:setGeometry( nW - nInfo, 1, nInfo, nPH )
    ::oProps:setGoTopBlock( {|| ::aRec[ 1 ] := 1 } )
    ::oProps:setGoBottomBlock( {|| ::aRec[ 1 ] := Len( ::aRows ) } )
    ::oProps:setSkipBlock( {| n | cbf_Skip( ::aRec, Len( ::aRows ), n ) } )
    ::oProps:addColumn( "Property", {|| ::propRow( 1 ) }, 13 )
    /* a string with room to type more: the browse edits a value as long as it gets it */
    ::oProps:addColumn( "Value", {| x | IIF( x == NIL, cbf_Room( ::propGet( ::propRow( 2 ) ) ), ;
        ::propSet( ::propRow( 2 ), x ) ) }, nInfo - 15 )
    ::oProps:setHeadSep( e"\xC4" )
    ::oProps:setColSep( e"\xB3" )
    ::oProps:editable := .T.
    ::oProps:onBeginEdit := {| nRow, nCol | HB_SYMBOL_UNUSED( nRow ), nCol == 2 }
    ::oProps:browse():colPos := 2

    ::oInfo := CBInfoPanel():new( self )
    ::oInfo:setGeometry( nW - nInfo, 1 + nPH, nInfo, nH - 3 - 1 - nPH )
    ::oInfo:aLines := { "", ;
         " Ins      add a control", ;
         " Arrows   move", ;
         " Ctrl+arrows  size", ;
         " Tab      next control", ;
         " Enter    its properties", ;
         " Del      delete", ;
         " Ctrl-PgUp/PgDn  Tab order", ;
         " Esc      the dialog itself", ;
         "          (Ctrl+arrows: size)", ;
         " F3 dialog  F5 try it", ;
         " F2 save  F9 code", ;
         " Mouse: click, drag", ;
         " Action of a button: ok,", ;
         "   cancel or a function", ;
         " Properties: typing, F2 or", ;
         "   Enter edits, Enter saves;", ;
         "   Esc / Tab: back" }
    ::oStat := HTLabel():new( " Ins Add  Enter Properties  Del Delete  F3 Dialog  F5 Try  F2 Save  F9 Generate code  Esc Close", self )
    ::oStat:setGeometry( 0, nH - 3, nW, 1 )
    ::setFocusChild( ::oCanvas )
    ::info()

RETURN self

METHOD refresh() CLASS CBFormDlg
    ::repaintChild( ::oCanvas )
    ::info()
RETURN NIL

/* the right side: the selected control (or the dialog) and its properties */
METHOD info() CLASS CBFormDlg

    LOCAL h, p

    IF ::nSel == 0
        ::oSelLbl:setText( " Dialog " + ::hForm[ "name" ] )
    ELSE
        h := ::hForm[ "controls" ][ ::nSel ]
        ::oSelLbl:setText( " " + hb_ntos( ::nSel ) + ". " + cbf_Types()[ hb_AScan( cbf_Types(), {| x | x[ 1 ] == h[ "type" ] } ) ][ 2 ] )
    ENDIF
    ::repaintChild( ::oSelLbl )

    /* other rows for another selection; the same rows show the new values */
    IF ::nPropSel != ::nSel
        ::nPropSel := ::nSel
        ::aRows := {}
        IF ::nSel == 0
            AEval( { { "Name", "name" }, { "Title", "title" }, { "Width", "width" }, { "Height", "height" }, ;
                     { "Buttons", "buttons" } }, {| x | AAdd( ::aRows, x ) } )
        ELSE
            AEval( { { "Column", "x" }, { "Row", "y" }, { "Width", "w" }, { "Height", "h" } }, {| x | AAdd( ::aRows, x ) } )
            FOR EACH p IN cbf_Props( h[ "type" ] )
                AAdd( ::aRows, { cbf_PropLabel( p ), p } )
            NEXT
        ENDIF
        ::aRec[ 1 ] := 1
        ::oProps:browse():goTop()
    ENDIF
    ::oProps:refreshAll()

RETURN NIL

/* the label (1) or the key (2) of the current row of the properties ("" before there are rows) */
METHOD propRow( n ) CLASS CBFormDlg
RETURN IIF( ::aRec[ 1 ] >= 1 .AND. ::aRec[ 1 ] <= Len( ::aRows ), ::aRows[ ::aRec[ 1 ] ][ n ], "" )

/* a property of the selected control (or of the dialog), as text */
METHOD propGet( cKey ) CLASS CBFormDlg

    LOCAL x

    IF Empty( cKey )
        RETURN ""
    ENDIF
    x := hb_HGetDef( IIF( ::nSel == 0, ::hForm, ::hForm[ "controls" ][ ::nSel ] ), cKey, "" )

RETURN IIF( HB_ISNUMERIC( x ), hb_ntos( x ), hb_CStr( x ) )

/* a property typed in the browse: numbers as numbers, names checked */
METHOD propSet( cKey, cValue ) CLASS CBFormDlg

    LOCAL h, c := AllTrim( cValue )

    IF Empty( cKey )
        RETURN NIL
    ELSEIF ::nSel == 0
        h := ::hForm
        SWITCH cKey
        CASE "name"
            IF Empty( c )
                RETURN NIL
            ENDIF
            h[ cKey ] := cb_Ident( c )
            ::oSelLbl:setText( " Dialog " + h[ cKey ] )
            ::repaintChild( ::oSelLbl )
            EXIT
        CASE "width"   ; h[ cKey ] := Min( ht_ScreenMaxCol() + 1, Max( 20, Int( Val( c ) ) ) ) ; EXIT
        CASE "height"  ; h[ cKey ] := Min( ht_ScreenMaxRow() + 1, Max( 5, Int( Val( c ) ) ) ) ; EXIT
        CASE "buttons" ; h[ cKey ] := c ; EXIT
        OTHERWISE      ; h[ cKey ] := cValue
        ENDSWITCH
    ELSE
        h := ::hForm[ "controls" ][ ::nSel ]
        DO CASE
        CASE hb_AScan( { "x", "y", "w", "h", "len" }, cKey,,, .T. ) > 0
            h[ cKey ] := Max( IIF( cKey == "x" .OR. cKey == "y", 0, 1 ), Int( Val( c ) ) )
        CASE cKey == "name"
            IF ! Empty( c ) .AND. ! Upper( cb_Ident( c ) ) == Upper( c )   /* cb_Ident() changes the case */
                HTToast():show( "The name must be a Harbour name (letters, digits, _): " + c, 2500, HT_CLR_TOAST_WARNING )
                RETURN NIL
            ENDIF
            h[ cKey ] := c
        CASE cKey == "valtype"
            h[ cKey ] := IIF( ! Empty( c ) .AND. Upper( Left( c, 1 ) ) $ "CNDL", Upper( Left( c, 1 ) ), "C" )
        OTHERWISE
            h[ cKey ] := cValue
        ENDCASE
    ENDIF
    ::lChanged := .T.
    ::repaintChild( ::oCanvas )

RETURN NIL

/* the keys to the properties (lOn) or back to the dialog */
METHOD focusProps( lOn ) CLASS CBFormDlg

    ::setFocusChild( IIF( lOn, ::oProps, ::oCanvas ) )
    ::oProps:browse():colPos := 2
    ::repaintChild( ::oProps )
    ::repaintChild( ::oCanvas )

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

    /* the properties: the browse edits; Esc / Tab go back to the dialog */
    IF ::focusWidget() == ::oProps
        IF ! ::oProps:isEditing()
            SWITCH nKey
            CASE K_ESC
            CASE K_TAB
            CASE K_SH_TAB
                ::focusProps( .F. )
                RETURN
            CASE K_LEFT
            CASE K_RIGHT
                RETURN
            CASE K_ENTER
                ::oProps:browse():colPos := 2
                ::oProps:beginEdit( 0 )
                RETURN
            CASE K_F5 ; ::preview() ; RETURN
            CASE K_F9 ; ::generate() ; RETURN
            ENDSWITCH
        ENDIF
        ::oProps:keyEvent( keyEvent )
        RETURN
    ENDIF

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
    CASE K_ENTER      ; ::focusProps( .T. ) ; RETURN
    CASE K_F3
        ::nSel := 0
        ::refresh()
        ::focusProps( .T. )
        RETURN
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
    DATA oPrev                       /* the dialog whose widgets were painted last */
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
    LOCAL cSel := HTTheme():getColor( HT_CLR_LIST_SELECTED ), cTitle := HTTheme():getColor( HT_CLR_TITLEBAR )
    LOCAL cBtn := HTTheme():getColor( HT_CLR_TITLEBTN ), cWin, aObj := {}, oDlg, o, g, n, nBtn, nRight
    LOCAL nMaxR := MaxRow(), nMaxC := MaxCol(), nTop, nLeft, nBot, nRgt

    HB_SYMBOL_UNUSED( event )
    DispBox( 0, 0, nMaxR, nMaxC, Replicate( " ", 9 ), HTTheme():getColor( HT_CLR_DESKTOP ) )

    /* the dialog of F5, not shown: its widgets paint themselves here. Kept until the
       next paint - a widget knows its parent by the address of the object. */
    oDlg := cbf_Build( hF, { => }, { => }, aObj )
    ::oPrev := oDlg
    cWin := oDlg:color

    /* the shadow of a dialog (hb_Shadow: 2 columns on the right, a row below) */
    DispBox( nT + 1, nL + nW, nT + nH, nL + nW + 1, Replicate( " ", 9 ), "N/N" )
    DispBox( nT + nH, nL + 2, nT + nH, nL + nW + 1, Replicate( " ", 9 ), "N/N" )

    /* the title bar and the client area, as HTWidget:drawCuiChrome() draws them */
    nBtn := IIF( nW >= 24, 3, 1 )
    nRight := nW - 1
    DispOutAt( nT, nL, Space( nW ), cTitle )
    FOR EACH g IN { oDlg:charWidgetClose, oDlg:charWidgetMaximize, oDlg:charWidgetHide }
        DispOutAt( nT, nL + nRight - nBtn + 1, ht_cellPadC( g, nBtn ), cBtn )
        nRight -= nBtn
    NEXT
    DispOutAt( nT, nL, ht_cellPadR( " " + hF[ "title" ], nRight + 1 ), IIF( d:nSel == 0, cSel, cTitle ) )
    FOR n := 1 TO nH - 1
        DispOutAt( nT + n, nL, Space( nW ), cWin )
    NEXT
    g := oDlg:charWidgetResize
    DispOutAt( nT + nH - 1, nL + nW - ht_strWidth( g ), g, cWin )

    /* the widgets, each in its viewport (HTWidget:paintChild(): the content starts
       under the title bar, at column 0), clipped to the dialog and the canvas */
    FOR EACH o IN oDlg:children
        IF o:isDerivedFrom( "HTWidget" ) .AND. o:width > 0 .AND. o:height > 0
            nTop := nT + 1 + o:y
            nLeft := nL + o:x
            nBot := Min( Min( nTop + o:height - 1, nT + nH - 1 ), nMaxR )
            nRgt := Min( Min( nLeft + o:width - 1, nL + nW - 1 ), nMaxC )
            IF nBot >= nTop .AND. nRgt >= nLeft
                ht_wFormatPush( nTop, nLeft, nMaxR - nBot, nMaxC - nRgt )
                o:paintEvent( HTPaintEvent():new() )
                ht_wFormatPop()
                /* the selected control: its own look in the color of a selection */
                IF d:nSel > 0 .AND. d:nSel <= Len( aObj ) .AND. aObj[ d:nSel ] == o
                    hb_ClrArea( ht_absRow( nTop ), ht_absCol( nLeft ), ht_absRow( nBot ), ht_absCol( nRgt ), hb_ColorToN( cSel ) )
                ENDIF
            ENDIF
        ENDIF
    NEXT

RETURN

/* a click selects the control under the mouse (else the dialog), a drag moves it */
METHOD PROCEDURE mouseEvent( eventMouse ) CLASS CBFormCanvas

    LOCAL d := ::oDlg, nRow := eventMouse:mouseRow - ::nOffY - 1, nCol := eventMouse:mouseCol - ::nOffX, n, h

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
        d:focusProps( .T. )
        EXIT
    ENDSWITCH

RETURN
