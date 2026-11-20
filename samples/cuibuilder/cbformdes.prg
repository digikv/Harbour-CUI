/*
 * CuiBuilder - the dialog designer: dialogs of Harbour-CUI widgets
 *
 * Copyright (c) 2026 Srđan Dragojlović - dual licensed, see LICENSE
 *
 * A dialog is designed on the screen and kept in a .cuiform file (JSON);
 * F9 generates dlg_<name>.prg from it: the function Dlg_<Name>( hValues ),
 * which builds the dialog with the classes of Harbour-CUI (HTDialog, HTLabel,
 * HTLineEdit, HTGet, HTPushButton, HTCheckBox, HTRadioButton, HTComboBox,
 * HTListBox, HTSearchBox, HTDatePicker, HTFrame, HTSeparator, HTSpinner, HTProgressBar, HTTextEdit), shows it and
 * returns the values of the fields in a hash (NIL = cancelled). F5 shows the
 * dialog itself, made of the same classes. (The way of the dialog designer of
 * HbIde: a design file and the code generated from it.)
 *
 *   cb_FormDesigner( cFile, hSet, bAdd )   bAdd( aFiles ): adds files to the project
 *
 * The canvas: a palette of the controls on the top (a click picks one, a click
 * on the dialog places it) and the dialog as it will look - its widgets are
 * built as for F5 and paint themselves, under the title bar of a dialog; the
 * selected control in the color of a selection. On the right an object
 * inspector as in the designer of HwGUI (Kresin): the objects in a combo box,
 * the tabs Properties / Events, a browse edited in place (a list where a
 * property has only some values, an editor for the code of an event). The
 * right button (Shift-F10) opens a menu: copy, paste, align, Tab order. Ins adds a control, the arrows move it, Ctrl + arrows change its
 * size (without a selected control: the size of the dialog), Tab / Shift-Tab
 * select the next / previous one, Enter its properties, Del deletes it,
 * Ctrl-PgUp / Ctrl-PgDn move it in the Tab order; the mouse selects and drags.
 */

#include "hbtui.ch"
#include "inkey.ch"
#include "hbgtinfo.ch"
#include "cuibuild.ch"

/* cbf_Code(): the codepage of the generated file, its texts through CpText(), a warning */
STATIC s_cGenCP := "UTF8"
STATIC s_lGenCP := .F.
STATIC s_cGenWarn := ""

/* the kinds of controls: { type, name in the list, default size, name on the palette } */
STATIC FUNCTION cbf_Types()
RETURN { { "label",    "Label (HTLabel)",                 10, 1, "Label" }, ;
         { "edit",     "Text field (HTLineEdit)",         20, 1, "Edit" }, ;
         { "get",      "GET with PICTURE (HTGet)",        30, 1, "GET" }, ;
         { "button",   "Button (HTPushButton)",           10, 1, "Button" }, ;
         { "checkbox", "Check box (HTCheckBox)",          16, 1, "Check" }, ;
         { "radio",    "Radio button (HTRadioButton)",    16, 1, "Radio" }, ;
         { "combo",    "Combo box (HTComboBox)",          20, 1, "Combo" }, ;
         { "list",     "List (HTListBox)",                20, 5, "List" }, ;
         { "searchbox", "Search box (HTSearchBox)",       30, 1, "Search" }, ;
         { "date",     "Date with a calendar (HTDatePicker)", 13, 1, "Date" }, ;
         { "memo",     "Text, several lines (HTTextEdit)", 30, 5, "Memo" }, ;
         { "spinner",  "Number with arrows (HTSpinner)",  20, 1, "Spinner" }, ;
         { "progress", "Progress bar (HTProgressBar)",    20, 1, "Progress" }, ;
         { "frame",    "Frame (HTFrame)",                 30, 6, "Frame" }, ;
         { "separator", "Line (HTSeparator)",             40, 1, "Line" } }

/* the properties a kind has (besides col, row, width, height) */
STATIC FUNCTION cbf_Props( cType )

    SWITCH cType
    CASE "label"    ; RETURN { "text" }
    CASE "edit"     ; RETURN { "name", "text" }
    CASE "get"      ; RETURN { "name", "text", "valtype", "len", "picture", "value" }
    CASE "button"   ; RETURN { "name", "text" }
    CASE "checkbox"
    CASE "radio"    ; RETURN { "name", "text", "value" }
    CASE "combo"
    CASE "list"     ; RETURN { "name", "items", "value" }
    CASE "memo"     ; RETURN { "name", "text" }
    CASE "spinner"  ; RETURN { "name", "text", "min", "max", "step", "value" }
    CASE "progress" ; RETURN { "name", "value" }
    CASE "searchbox" ; RETURN { "name", "placeholder", "source", "details", "value" }
    CASE "date"     ; RETURN { "name", "lang", "placeholder", "value" }
    CASE "frame"    ; RETURN { "text" }
    ENDSWITCH

RETURN {}

/* the events of a kind ("" = the dialog): { key in the .cuiform, name (the property of the
   widget), parameters of the code block } */
STATIC FUNCTION cbf_Events( cType )

    SWITCH cType
    CASE ""         ; RETURN { { "oninit", "onInit", "" }, { "onaccept", "onAccept", "" } }
    CASE "button"   ; RETURN { { "action", "onClicked", "" } }
    CASE "get"      ; RETURN { { "valid", "VALID", "" }, { "when", "WHEN", "" } }
    CASE "edit"
    CASE "memo"     ; RETURN { { "onchanged", "onChanged", "cText" } }
    CASE "checkbox"
    CASE "radio"    ; RETURN { { "ontoggled", "onToggled", "lChecked" } }
    CASE "combo"
    CASE "list"     ; RETURN { { "onchanged", "onChanged", "nIndex" } }
    CASE "spinner"  ; RETURN { { "onchanged", "onChanged", "nValue" } }
    CASE "date"     ; RETURN { { "onchanged", "onChanged", "dDate" } }
    CASE "searchbox" ; RETURN { { "onselected", "onSelected", "xItem, xValue" }, { "oncleared", "onCleared", "" }, ;
                                { "onadd", "onAdd", "" } }
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
    CASE "placeholder" ; RETURN "Placeholder"
    CASE "lang"    ; RETURN "Language"
    CASE "source"  ; RETURN "Items (expr)"
    CASE "details" ; RETURN "Details (x)"
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
    h[ "codepage" ] := "UTF8"
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
    FOR EACH c IN { { "name", "Dialog" }, { "title", "" }, { "width", 50 }, { "height", 14 }, { "buttons", "" }, { "codepage", "UTF8" } }
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

    LOCAL a := cbf_Types()[ hb_AScan( cbf_Types(), {| x | x[ 1 ] == cType } ) ], h := { => }

    h[ "type" ] := cType
    h[ "x" ] := nX
    h[ "y" ] := nY
    h[ "w" ] := a[ 3 ]
    h[ "h" ] := a[ 4 ]
    IF hb_AScan( cbf_Props( cType ), "name",,, .T. ) > 0
        h[ "name" ] := cbf_NewName( cType, aForm )
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
    CASE "date"     ; h[ "lang" ] := "" ; h[ "placeholder" ] := "" ; h[ "value" ] := "today" ; EXIT
    CASE "searchbox" ; h[ "placeholder" ] := "Search ..." ; h[ "source" ] := "" ; h[ "details" ] := "" ; h[ "value" ] := "" ; EXIT
    CASE "frame"    ; h[ "text" ] := "Group" ; EXIT
    ENDSWITCH

RETURN h

/* cName1, nAge2 ... a name not taken yet */
STATIC FUNCTION cbf_NewName( cType, aForm )

    LOCAL n := 1, cName

    DO WHILE .T.
        cName := cbf_Prefix( cType ) + hb_ntos( n )
        IF hb_AScan( aForm, {| x | Lower( hb_HGetDef( x, "name", "" ) ) == Lower( cName ) } ) == 0
            EXIT
        ENDIF
        n++
    ENDDO

RETURN cName

STATIC FUNCTION cbf_Prefix( cType )

    SWITCH cType
    CASE "checkbox"
    CASE "radio"    ; RETURN "l"
    CASE "combo"
    CASE "list"
    CASE "spinner"
    CASE "searchbox"
    CASE "progress" ; RETURN "n"
    CASE "date"     ; RETURN "d"
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
        CASE "date"
            o := HTDatePicker():new( dlg )
            IF ! Empty( hb_HGetDef( h, "lang", "" ) )
                o:setLang( h[ "lang" ] )
            ENDIF
            o:placeholder := hb_HGetDef( h, "placeholder", "" )
            o:setValue( cbf_DateVal( hb_HGetDef( h, "value", "" ) ) )
            EXIT
        CASE "searchbox"
            o := HTSearchBox():new( dlg )
            o:placeholder := hb_HGetDef( h, "placeholder", "" )
            o:setItems( cbf_PreviewItems( hb_HGetDef( h, "source", "" ) ) )
            IF ! Empty( hb_HGetDef( h, "details", "" ) )
                o:detailsBlock := cbf_PreviewDetails( h[ "details" ] )
            ENDIF
            o:setValue( cbf_SBValue( hb_HGetDef( h, "value", "" ) ) )
            IF ! Empty( cbf_Lines( hb_HGetDef( h, "onadd", "" ) ) )
                o:onAdd := {|| HTToast():show( "onAdd   (runs in the program)", 2000, HT_CLR_TOAST_INFO ) }
            ENDIF
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

STATIC FUNCTION cbf_ToastBlock( cCode )

    LOCAL a := cbf_Lines( cCode )

RETURN {|| HTToast():show( "onClicked: " + AllTrim( a[ 1 ] ) + IIF( Len( a ) > 1, " ..." , "" ) + "   (runs in the program)", 2000, HT_CLR_TOAST_INFO ) }

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

    LOCAL c := "", cEol := hb_eol(), h, cName, cO, aBtn, aRet := {}, aGets := {}, x, cN, e, cA, aFunc := {}, a

    s_cGenCP := Upper( hb_CStr( hb_HGetDef( hForm, "codepage", "UTF8" ) ) )
    IF hb_AScan( { "CP950", "GBK" }, s_cGenCP,,, .T. ) == 0
        s_cGenCP := "UTF8"
    ENDIF
    s_lGenCP := .F.
    s_cGenWarn := ""

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
    IF ! s_cGenCP == "UTF8"
        c += " *" + cEol
        c += " * Codepage: " + s_cGenCP + " (" + IIF( s_cGenCP == "CP950", "Big5", "GBK / CP936" ) + "). The texts go through CpText() into" + cEol
        c += " * the codepage of the program (UTF8EX, or " + s_cGenCP + " itself); a text of your own" + cEol
        c += ' * in the code of an event: CpText( "..." ).' + cEol
    ENDIF
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
    c += "    dlg:setWindowTitle( " + cbf_Lit( hForm[ "title" ] ) + " )" + cEol
    c += "    dlg:resize( " + hb_ntos( hForm[ "width" ] ) + ", " + hb_ntos( hForm[ "height" ] ) + " )" + cEol + cEol
    FOR EACH h IN hForm[ "controls" ]
        cName := hb_HGetDef( h, "name", "" )
        cO := IIF( Empty( cName ), "", 'hCtl[ "' + cName + '" ]' )
        x := IIF( Empty( cName ), "", hb_ValToExp( cName ) )
        cN := cFunc + "_" + IIF( Empty( cName ), "c" + hb_ntos( h:__enumIndex() ), cName ) + "_"
        SWITCH h[ "type" ]
        CASE "label"
            c += "    HTLabel():new( " + cbf_Lit( h[ "text" ] ) + ", dlg ):setGeometry( " + cbf_Geo( h ) + " )" + cEol
            LOOP
        CASE "frame"
            c += "    HTFrame():new( " + cbf_Lit( h[ "text" ] ) + ", dlg ):setGeometry( " + cbf_Geo( h ) + " )" + cEol
            LOOP
        CASE "separator"
            c += "    HTSeparator():new( dlg ):setGeometry( " + cbf_Geo( h ) + " )" + cEol
            LOOP
        CASE "button"
            IF Empty( cO )
                cO := "o" + hb_ntos( h:__enumIndex() )
                c += "    " + cO + " := HTPushButton():new( " + cbf_Lit( h[ "text" ] ) + ", dlg )" + cEol
                c := StrTran( c, "LOCAL dlg := HTDialog():new(), hCtl := { => }", "LOCAL dlg := HTDialog():new(), hCtl := { => }, " + cO )
            ELSE
                c += "    " + cO + " := HTPushButton():new( " + cbf_Lit( h[ "text" ] ) + ", dlg )" + cEol
            ENDIF
            cA := hb_HGetDef( h, "action", "" )
            DO CASE
            CASE Lower( AllTrim( cA ) ) == "ok"
                c += "    " + cO + ":onClicked := {|| dlg:accept() }" + cEol
            CASE Lower( AllTrim( cA ) ) == "cancel"
                c += "    " + cO + ":onClicked := {|| dlg:reject() }" + cEol
            CASE Len( cbf_Lines( cA ) ) == 0
            CASE Len( cbf_Lines( cA ) ) == 1 .AND. Upper( cb_Ident( AllTrim( cA ) ) ) == Upper( AllTrim( cA ) )
                c += "    " + cO + ":onClicked := {|| " + AllTrim( cA ) + "( dlg, hCtl ) }" + cEol   /* a function name */
            OTHERWISE
                c += "    " + cO + ":onClicked := " + cbf_EventBlock( cA, "", cN + "onClicked", aFunc, .F. ) + cEol
            ENDCASE
            EXIT
        CASE "edit"
            c += "    " + cO + " := HTLineEdit():new( dlg )" + cEol
            c += "    " + cO + ":setText( hb_HGetDef( hValues, " + x + ", " + cbf_Lit( h[ "text" ] ) + " ) )" + cEol
            AAdd( aRet, x + " => " + cO + ":text" )
            EXIT
        CASE "get"
            c += "    " + cO + " := HTGet():new( dlg )" + cEol
            c += "    " + cO + ":setup( {| x | IIF( x == NIL, " + cName + ", " + cName + " := x ) }, " + ;
                 cbf_Lit( h[ "text" ] ) + ", " + hb_ValToExp( h[ "picture" ] ) + ", " + ;
                 IIF( Empty( cbf_Lines( hb_HGetDef( h, "valid", "" ) ) ), "", cbf_EventBlock( h[ "valid" ], "", cN + "valid", aFunc, .T. ) ) + ", " + ;
                 IIF( Empty( cbf_Lines( hb_HGetDef( h, "when", "" ) ) ), "", cbf_EventBlock( h[ "when" ], "", cN + "when", aFunc, .T. ) ) + ;
                 ",,,,, " + hb_ntos( h[ "w" ] ) + " )" + cEol
            AAdd( aRet, x + " => " + cName )
            EXIT
        CASE "checkbox"
        CASE "radio"
            c += "    " + cO + " := " + IIF( h[ "type" ] == "radio", "HTRadioButton", "HTCheckBox" ) + "():new( " + cbf_Lit( h[ "text" ] ) + ", dlg )" + cEol
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
            c += "    " + cO + ":setText( hb_HGetDef( hValues, " + x + ", " + cbf_Lit( h[ "text" ] ) + " ) )" + cEol
            AAdd( aRet, x + " => " + cO + ":text" )
            EXIT
        CASE "spinner"
            c += "    " + cO + " := HTSpinner():new( dlg )" + cEol
            c += "    " + cO + ":setup( " + cbf_Lit( h[ "text" ] ) + ", " + hb_ntos( cbf_Num( h[ "min" ] ) ) + ", " + hb_ntos( cbf_Num( h[ "max" ] ) ) + ", " + ;
                 hb_ntos( cbf_Num( h[ "step" ] ) ) + ", hb_HGetDef( hValues, " + x + ", " + hb_ntos( cbf_Num( h[ "value" ] ) ) + " ) )" + cEol
            AAdd( aRet, x + " => " + cO + ":value" )
            EXIT
        CASE "progress"
            c += "    " + cO + " := HTProgressBar():new( dlg )" + cEol
            c += "    " + cO + ":setValue( hb_HGetDef( hValues, " + x + ", " + hb_ntos( cbf_Num( h[ "value" ] ) ) + " ) )" + cEol
            EXIT
        CASE "date"
            c += "    " + cO + " := HTDatePicker():new( dlg )" + cEol
            IF ! Empty( hb_HGetDef( h, "lang", "" ) )         /* else ht_dateDefaultLang() of the program */
                c += "    " + cO + ":setLang( " + hb_ValToExp( Lower( AllTrim( h[ "lang" ] ) ) ) + " )" + cEol
            ENDIF
            IF ! Empty( hb_HGetDef( h, "placeholder", "" ) )
                c += "    " + cO + ":placeholder := " + cbf_Lit( h[ "placeholder" ] ) + cEol
            ENDIF
            c += "    " + cO + ":setValue( hb_HGetDef( hValues, " + x + ", " + cbf_DateExp( hb_HGetDef( h, "value", "" ) ) + " ) )" + cEol
            AAdd( aRet, x + " => " + cO + ":getValue()" )
            EXIT
        CASE "searchbox"
            /* the items: the expression of Items, else hValues[ "<name>_items" ] */
            c += "    " + cO + " := HTSearchBox():new( dlg )" + cEol
            IF ! Empty( hb_HGetDef( h, "placeholder", "" ) )
                c += "    " + cO + ":placeholder := " + cbf_Lit( h[ "placeholder" ] ) + cEol
            ENDIF
            c += "    " + cO + ":setItems( " + IIF( Empty( hb_HGetDef( h, "source", "" ) ), ;
                 "hb_HGetDef( hValues, " + hb_ValToExp( cName + "_items" ) + ", {} )", AllTrim( h[ "source" ] ) ) + " )" + cEol
            IF ! Empty( hb_HGetDef( h, "details", "" ) )
                c += "    " + cO + ":detailsBlock := {| x | " + AllTrim( h[ "details" ] ) + " }" + cEol
            ENDIF
            c += "    " + cO + ":setValue( hb_HGetDef( hValues, " + x + ", " + cbf_SBValueExp( hb_HGetDef( h, "value", "" ) ) + " ) )" + cEol
            AAdd( aRet, x + " => " + cO + ":getValue()" )
            EXIT
        ENDSWITCH
        FOR EACH e IN cbf_Events( h[ "type" ] )
            IF hb_AScan( { "onchanged", "ontoggled", "onselected", "oncleared", "onadd" }, e[ 1 ],,, .T. ) > 0 .AND. ! Empty( cO ) .AND. ;
               ! Empty( cbf_Lines( hb_HGetDef( h, e[ 1 ], "" ) ) )
                c += "    " + cO + ":" + e[ 2 ] + " := " + cbf_EventBlock( h[ e[ 1 ] ], e[ 3 ], cN + e[ 2 ], aFunc, .F. ) + cEol
            ENDIF
        NEXT
        c += "    " + cO + ":setGeometry( " + cbf_Geo( h ) + " )" + cEol
    NEXT
    aBtn := cbf_Buttons( hForm )
    IF Len( aBtn ) > 0
        c += cEol + "    dlg:addButtonBar( "
        AEval( aBtn, {| b, n | c += IIF( n == 1, "", ", " ) + cbf_Lit( b ) } )
        c += " )" + cEol
    ENDIF
    a := cbf_Lines( hb_HGetDef( hForm, "oninit", "" ) )
    IF Len( a ) > 0
        c += cEol + "    /* onInit */" + cEol
        AEval( a, {| l | c += "    " + RTrim( l ) + cEol } )
    ENDIF
    c += cEol + "    IF dlg:exec() != HT_DIALOG_ACCEPTED" + cEol
    c += "        RETURN NIL" + cEol
    c += "    ENDIF" + cEol + cEol
    a := cbf_Lines( hb_HGetDef( hForm, "onaccept", "" ) )
    IF Len( a ) > 0
        c += "    /* onAccept */" + cEol
        AEval( a, {| l | c += "    " + RTrim( l ) + cEol } )
        c += cEol
    ENDIF
    c += "RETURN { "
    AEval( aRet, {| r, n | c += IIF( n == 1, "", ", ;" + cEol + "         " ) + r } )
    c += IIF( Len( aRet ) == 0, "=>", "" ) + " }" + cEol
    FOR EACH x IN aFunc                   /* the events of more lines */
        c += cEol + x
    NEXT
    IF ! s_cGenCP == "UTF8"
        /* CpText( "..." ) of your own in the code of an event */
        IF "CPTEXT(" $ Upper( StrTran( SubStr( c, At( '#include "hbtui.ch"', c ) ), " ", "" ) )
            s_lGenCP := .T.
        ENDIF
        IF s_lGenCP
            c := StrTran( c, '#include "hbtui.ch"' + cEol, '#include "hbtui.ch"' + cEol + cEol + "REQUEST HB_CODEPAGE_" + s_cGenCP + cEol, 1, 1 )
            c += cEol + "/* a text of this file (" + s_cGenCP + ") in the codepage of the program */" + cEol
            c += "STATIC FUNCTION CpText( c )" + cEol
            c += "RETURN hb_Translate( c, " + hb_ValToExp( s_cGenCP ) + ", hb_cdpSelect() )" + cEol
        ENDIF
        x := hb_UTF8ToStr( c, s_cGenCP )
        IF ! hb_StrToUTF8( x, s_cGenCP ) == c
            s_cGenWarn := "Some characters have no code in " + s_cGenCP + ": they are ? in the file."
        ENDIF
        c := x
    ENDIF

RETURN c

/* the code block of an event: one line = {|params| line }, more lines = a function of its own
   ( dlg, hCtl, params ) put at the end (aFunc); lRet: it RETURNs .T. when the code does not */
/* a text in the code: in a file of Big5 / GBK through CpText() (a text with other than ASCII) */
STATIC FUNCTION cbf_Lit( c )

    IF ! s_cGenCP == "UTF8" .AND. hb_BLen( c ) != Len( c )
        s_lGenCP := .T.
        RETURN "CpText( " + hb_ValToExp( c ) + " )"
    ENDIF

RETURN hb_ValToExp( c )

STATIC FUNCTION cbf_EventBlock( cCode, cParams, cFunc, aFunc, lRet )

    LOCAL a := cbf_Lines( cCode ), c, x, lHasRet := .F., cArgs := "dlg, hCtl" + IIF( Empty( cParams ), "", ", " + cParams )

    IF Len( a ) == 1
        c := ""
        FOR EACH x IN hb_ATokens( cParams, "," )
            IF ! Empty( x )
                c += "HB_SYMBOL_UNUSED( " + AllTrim( x ) + " ), "
            ENDIF
        NEXT
        RETURN "{|" + cParams + "| " + c + AllTrim( a[ 1 ] ) + " }"
    ENDIF
    c := "STATIC FUNCTION " + cFunc + "( " + cArgs + " )" + hb_eol() + hb_eol()
    FOR EACH x IN hb_ATokens( cArgs, "," )
        c += "    HB_SYMBOL_UNUSED( " + AllTrim( x ) + " )" + hb_eol()
    NEXT
    FOR EACH x IN a
        c += "    " + RTrim( x ) + hb_eol()
        IF Upper( Left( LTrim( x ), 6 ) ) == "RETURN"
            lHasRet := .T.
        ENDIF
    NEXT
    IF ! lHasRet
        c += hb_eol() + "RETURN " + IIF( lRet, ".T.", "NIL" ) + hb_eol()
    ENDIF
    AAdd( aFunc, c )

RETURN "{|" + cParams + "| " + cFunc + "( " + cArgs + " ) }"

/* the start value of a date: "today" (danas, данас, Date()), dd.mm.yyyy / yyyy-mm-dd, "" = empty */
STATIC FUNCTION cbf_DateVal( c )

    LOCAL d

    c := Lower( AllTrim( hb_CStr( c ) ) )
    IF hb_AScan( { "today", "danas", "данас", "date()" }, c,,, .T. ) > 0
        RETURN Date()
    ENDIF
    d := ht_dateParse( c )

RETURN IIF( d == NIL, CToD( "" ), d )

STATIC FUNCTION cbf_DateExp( c )

    LOCAL d

    c := Lower( AllTrim( hb_CStr( c ) ) )
    IF hb_AScan( { "today", "danas", "данас", "date()" }, c,,, .T. ) > 0
        RETURN "Date()"
    ENDIF
    d := ht_dateParse( c )

RETURN IIF( d == NIL, 'CToD( "" )', "0d" + DToS( d ) )

/* the start value of a search box: a number when it looks so, else text; "" = none */
STATIC FUNCTION cbf_SBValue( c )

    c := AllTrim( hb_CStr( c ) )
    IF Empty( c )
        RETURN NIL
    ENDIF

RETURN IIF( hb_RegExLike( "-?[0-9]+(\.[0-9]+)?", c ), Val( c ), c )

STATIC FUNCTION cbf_SBValueExp( c )

    LOCAL x := cbf_SBValue( c )

RETURN IIF( x == NIL, "NIL", IIF( HB_ISNUMERIC( x ), AllTrim( c ), cbf_Lit( x ) ) )

/* F5 and the canvas: the items of Items when it is an array written out ({ ... }),
   else three sample items (the program's functions are not here) */
STATIC FUNCTION cbf_PreviewItems( cSource )

    LOCAL a, oErr

    cSource := AllTrim( cSource )
    IF Left( cSource, 1 ) == "{"
        oErr := ErrorBlock( {| e | Break( e ) } )
        BEGIN SEQUENCE
            a := &( cSource )
        RECOVER
            a := NIL
        END SEQUENCE
        ErrorBlock( oErr )
        IF HB_ISARRAY( a )
            RETURN a
        ENDIF
    ENDIF

RETURN { { 1, "Sample one", "100000001" }, { 2, "Sample two", "100000002" }, { 3, "Sample three", "100000003" } }

/* the details of the designer's text, made safe: an error (x[ 3 ] of a short item ...) = none */
STATIC FUNCTION cbf_PreviewDetails( cDetails )

    LOCAL b, oErr := ErrorBlock( {| e | Break( e ) } )

    BEGIN SEQUENCE
        b := &( "{| x | " + AllTrim( cDetails ) + " }" )
    RECOVER
        b := NIL
    END SEQUENCE
    ErrorBlock( oErr )

RETURN {| x | cbf_SafeEval( b, x ) }

STATIC FUNCTION cbf_SafeEval( b, x )

    LOCAL xRet, oErr

    IF b == NIL
        RETURN {}
    ENDIF
    oErr := ErrorBlock( {| e | Break( e ) } )
    BEGIN SEQUENCE
        xRet := Eval( b, x )
    RECOVER
        xRet := {}
    END SEQUENCE
    ErrorBlock( oErr )

RETURN xRet

STATIC FUNCTION cbf_Geo( h )
RETURN hb_ntos( h[ "x" ] ) + ", " + hb_ntos( h[ "y" ] ) + ", " + hb_ntos( h[ "w" ] ) + ", " + hb_ntos( h[ "h" ] )

STATIC FUNCTION cbf_ItemsExp( cItems )

    LOCAL c := "", x

    FOR EACH x IN hb_ATokens( cItems, "," )
        c += IIF( x:__enumIndex() == 1, "", ", " ) + cbf_Lit( AllTrim( x ) )
    NEXT

RETURN c

STATIC FUNCTION cbf_InitExp( h )

    LOCAL c := AllTrim( hb_HGetDef( h, "value", "" ) )

    SWITCH Upper( Left( h[ "valtype" ], 1 ) )
    CASE "N" ; RETURN hb_ntos( Val( c ) )
    CASE "D" ; RETURN IIF( Empty( c ), 'CToD( "" )', "CToD( " + hb_ValToExp( c ) + " )" )
    CASE "L" ; RETURN IIF( cbf_IsTrue( c ), ".T.", ".F." )
    ENDSWITCH

RETURN cbf_Lit( c )

STATIC FUNCTION cbf_NameList( hForm )

    LOCAL c := "", h

    FOR EACH h IN hForm[ "controls" ]
        IF ! Empty( hb_HGetDef( h, "name", "" ) ) .AND. ! h[ "type" ] $ "button|progress"
            c += IIF( Empty( c ), "", ", " ) + h[ "name" ]
        ENDIF
    NEXT

RETURN IIF( Empty( c ), "(none)", c )

/* dlg_<name>.prg of a .cuiform (made again), for the skeleton of an application:
   { prg file, function, title of the dialog }; NIL = an error (cErr) */
FUNCTION cb_FormGenerate( cFile, cErr )

    LOCAL hForm := cbf_Load( cFile, @cErr ), cFunc, cPrg

    IF hForm == NIL
        RETURN NIL
    ENDIF
    cFunc := "Dlg_" + hForm[ "name" ]
    cPrg := hb_FNameDir( cFile ) + "dlg_" + Lower( hForm[ "name" ] ) + ".prg"
    IF ! hb_MemoWrit( cPrg, cbf_Code( hForm, hb_FNameNameExt( cFile ), cFunc ) )
        cErr := "Cannot write " + cPrg
        RETURN NIL
    ENDIF

RETURN { cPrg, cFunc, hForm[ "title" ] }

/*----------------------------------------------------------- the designer */

/* TBrowse skip over an array; aRec[ 1 ] is the current row */
STATIC FUNCTION cbf_Skip( aRec, nLen, nRequest )

    LOCAL nActual := IIF( nRequest > 0, Min( nRequest, nLen - aRec[ 1 ] ), Max( nRequest, 1 - aRec[ 1 ] ) )

    aRec[ 1 ] += nActual

RETURN nActual

/* the lines of an event's code, without empty lines at the end */
STATIC FUNCTION cbf_Lines( cCode )

    LOCAL a := hb_ATokens( StrTran( hb_CStr( cCode ), Chr( 13 ), "" ), Chr( 10 ) )

    DO WHILE Len( a ) > 0 .AND. Empty( ATail( a ) )
        ASize( a, Len( a ) - 1 )
    ENDDO

RETURN a

/* a value for the browse: padded, so more can be typed (it is RTrim()-ed back) */
STATIC FUNCTION cbf_Room( c )
RETURN PadR( c, Len( c ) + 100 )

/* a control in the list of objects and in messages: its name, else its text */
STATIC FUNCTION cbf_Caption( h )

    LOCAL c := hb_HGetDef( h, "name", "" )

    IF Empty( c )
        c := hb_CStr( hb_HGetDef( h, "text", "" ) )
    ENDIF

RETURN Left( StrTran( c, Chr( 10 ), " " ), 18 )

/* column n of an array of arrays */
STATIC FUNCTION cbf_Col( a, n )

    LOCAL aOut := {}

    AEval( a, {| x | AAdd( aOut, x[ n ] ) } )

RETURN aOut

/* the code of an event in an editor (Harbour colors); NIL = cancelled */
STATIC FUNCTION cbf_EditCode( cTitle, cHint, cCode )

    LOCAL nW := Min( 100, ht_ScreenMaxCol() - 3 ), nH := Min( 22, ht_ScreenMaxRow() - 3 ), dlg, oB

    oB := CEBuffer():new()
    oB:setText( cCode )
    oB:cSyntax := ce_SyntaxFor( "event.prg" )
    oB:lHilite := .T.
    dlg := CBCodeDlg():new()
    dlg:setWindowTitle( cTitle + "   F2 OK  Esc cancel" )
    dlg:resize( nW, nH )
    dlg:move( Int( ( ht_ScreenMaxCol() + 1 - nW ) / 2 ), Int( ( ht_ScreenMaxRow() + 1 - nH ) / 2 ) )
    HTLabel():new( cHint, dlg ):setGeometry( 1, 0, nW - 2, 1 )
    dlg:oEdit := HTCodeEdit():new( dlg )
    dlg:oEdit:setGeometry( 1, 1, nW - 2, nH - 6 )
    dlg:oEdit:setBuffer( oB )
    dlg:addButtonBar( "OK", "Cancel" )
    dlg:setFocusChild( dlg:oEdit )
    IF dlg:exec() != HT_DIALOG_ACCEPTED
        RETURN NIL
    ENDIF

RETURN cbf_Join( cbf_Lines( oB:getText() ) )

STATIC FUNCTION cbf_Join( a )

    LOCAL c := "", x

    FOR EACH x IN a
        c += IIF( x:__enumIndex() == 1, "", Chr( 10 ) ) + RTrim( x )
    NEXT

RETURN c

/* the editor of an event: F2 = OK, Esc = cancel, Tab stays in the code */
CLASS CBCodeDlg FROM HTDialog
    DATA oEdit
    METHOD keyEvent( keyEvent )
ENDCLASS

METHOD PROCEDURE keyEvent( keyEvent ) CLASS CBCodeDlg

    SWITCH keyEvent:key
    CASE K_F2
        keyEvent:accept()
        ::accept()
        RETURN
    CASE K_ESC
        keyEvent:accept()
        IF ! ::oEdit:cancelMarking()
            ::reject()
        ENDIF
        RETURN
    CASE K_TAB
    CASE K_SH_TAB
        IF ::focusWidget() == ::oEdit
            ::oEdit:keyEvent( keyEvent )
            RETURN
        ENDIF
        EXIT
    ENDSWITCH
    ::super:keyEvent( keyEvent )

RETURN

PROCEDURE cb_FormDesigner( cFile, hSet, bAdd )

    LOCAL dlg := CBFormDlg():create( cFile, hSet, bAdd )

    IF dlg:hForm != NIL
        dlg:exec()
    ENDIF

RETURN

/* The designer: the canvas (the palette on the top, the dialog as it will look) and on the
   right an object inspector as in the designer of HwGUI (Kresin): the objects in a combo box,
   the tabs Properties / Events, a browse of Name | Value edited in place - a list to choose
   from where there are only some values, an editor for the code of an event. */
CLASS CBFormDlg FROM HTDialog

    DATA cFile
    DATA hSet
    DATA bAdd
    DATA hForm
    DATA nSel        INIT 0          /* the selected control, 0 = the dialog */
    DATA lChanged    INIT .F.
    DATA oCanvas
    DATA oObj                        /* the objects: the dialog and its controls */
    DATA oTabs                       /* Properties | Events */
    DATA nTab        INIT 1
    DATA oProps                      /* the properties or the events of the selection */
    DATA aRows       INIT {}         /* the rows of oProps: { label, key } */
    DATA aRec        INIT { 1 }      /* the current row of oProps */
    DATA nPropSel    INIT -1         /* the selection and the tab the rows were made for */
    DATA nPropTab    INIT 0
    DATA oInfo
    DATA oStat
    DATA cStat       INIT " Ins Add  Enter Properties  Del Delete  F3 Dialog  F5 Try  F2 Save  F9 Generate code  Esc Close"
    DATA cPlace      INIT ""         /* a control picked on the palette, placed by a click */
    DATA hClip                       /* Ctrl-C */
    DATA nPattern    INIT 0          /* "Set as pattern": the control others are aligned to */

    METHOD create( cFile, hSet, bAdd )
    METHOD keyEvent( keyEvent )
    METHOD refresh()
    METHOD info()
    METHOD objList()
    METHOD selectObj( n )
    METHOD setTab( n )
    METHOD addControl()
    METHOD placeStart( cType )
    METHOD placeAt( nX, nY )
    METHOD newControl( h )
    METHOD deleteSel()
    METHOD moveOrder( nDir )
    METHOD copy()
    METHOD paste( nX, nY )
    METHOD align( cHow )
    METHOD refIndex()
    METHOD contextMenu( nAbsRow, nAbsCol, nX, nY )
    METHOD propRow( n )
    METHOD propGet( cKey )
    METHOD propSet( cKey, cValue )
    METHOD beginProp( nCol )
    METHOD choices( cKey )
    METHOD editEvent( cKey )
    METHOD focusProps( lOn )
    METHOD preview()
    METHOD save()
    METHOD generate()
    METHOD moveSel( nDX, nDY, nDW, nDH )
    METHOD controlAt( nRow, nCol )

ENDCLASS

METHOD create( cFile, hSet, bAdd ) CLASS CBFormDlg

    LOCAL nW := ht_ScreenMaxCol() + 1, nH := ht_ScreenMaxRow() + 1, nInfo := 36, cErr := "", nPH, aHelp

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

    aHelp := { "", ;
         " Ins, palette  add a control", ;
         " Arrows  move  Ctrl+arrows size", ;
         " Tab     next control", ;
         " Enter   its properties", ;
         " Del     delete", ;
         " Ctrl-C / Ctrl-V  copy, paste", ;
         " Ctrl-PgUp/PgDn  Tab order", ;
         " Esc     the dialog itself", ;
         " Right click, Shift-F10: menu", ;
         " F3 dialog  F5 try it  F2 save", ;
         " F9 generate the code", ;
         " Properties: Enter, F2, typing", ;
         "   edit; Left/Right: the tab;", ;
         "   Esc / Tab: back to the dialog" }
    nPH := Max( 6, nH - 3 - 2 - Len( aHelp ) )

    /* on the right: the objects, the tabs, the browse, the keys */
    ::oObj := CBCombo():new( self )
    ::oObj:setGeometry( nW - nInfo, 0, nInfo, 1 )
    ::oObj:onChanged := {| n | ::selectObj( n - 1 ) }
    ::oTabs := CBTabBar():new( self )
    ::oTabs:oDlg := self
    ::oTabs:setGeometry( nW - nInfo, 1, nInfo, 1 )
    ::oProps := HTBrowse():new( self )
    ::oProps:setGeometry( nW - nInfo, 2, nInfo, nPH )
    ::oProps:setGoTopBlock( {|| ::aRec[ 1 ] := 1 } )
    ::oProps:setGoBottomBlock( {|| ::aRec[ 1 ] := Len( ::aRows ) } )
    ::oProps:setSkipBlock( {| n | cbf_Skip( ::aRec, Len( ::aRows ), n ) } )
    ::oProps:addColumn( "", {|| ::propRow( 1 ) }, 13 )
    /* a string with room to type more: the browse edits a value as long as it gets it */
    ::oProps:addColumn( "", {| x | IIF( x == NIL, cbf_Room( ::propGet( ::propRow( 2 ) ) ), ;
        ::propSet( ::propRow( 2 ), x ) ) }, nInfo - 15 )
    ::oProps:setColSep( e"\xB3" )
    ::oProps:editable := .T.
    ::oProps:onBeginEdit := {| nRow, nCol | HB_SYMBOL_UNUSED( nRow ), ::beginProp( nCol ) }
    ::oProps:browse():colPos := 2

    ::oInfo := CBInfoPanel():new( self )
    ::oInfo:setGeometry( nW - nInfo, 2 + nPH, nInfo, nH - 3 - 2 - nPH )
    ::oInfo:aLines := aHelp
    ::oStat := HTLabel():new( ::cStat, self )
    ::oStat:setGeometry( 0, nH - 3, nW, 1 )
    ::setFocusChild( ::oCanvas )
    ::info()

RETURN self

METHOD refresh() CLASS CBFormDlg
    ::repaintChild( ::oCanvas )
    ::info()
RETURN NIL

/* the right side: the objects, and the rows of the selection's properties / events */
METHOD info() CLASS CBFormDlg

    LOCAL h, p, e

    ::objList()
    IF ::nPropSel != ::nSel .OR. ::nPropTab != ::nTab
        ::nPropSel := ::nSel
        ::nPropTab := ::nTab
        ::aRows := {}
        h := IIF( ::nSel == 0, NIL, ::hForm[ "controls" ][ ::nSel ] )
        IF ::nTab == 2
            FOR EACH e IN cbf_Events( IIF( h == NIL, "", h[ "type" ] ) )
                AAdd( ::aRows, { e[ 2 ], e[ 1 ] } )
            NEXT
            IF Empty( ::aRows )
                AAdd( ::aRows, { "(no events)", "" } )
            ENDIF
        ELSEIF h == NIL
            AEval( { { "Name", "name" }, { "Title", "title" }, { "Width", "width" }, { "Height", "height" }, ;
                     { "Buttons", "buttons" }, { "Codepage", "codepage" } }, {| x | AAdd( ::aRows, x ) } )
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
    ::repaintChild( ::oTabs )

RETURN NIL

/* the combo box of the objects: Dialog <name>, 1. get cName, ... */
METHOD objList() CLASS CBFormDlg

    LOCAL h

    ::oObj:clear()
    ::oObj:addItem( "Dialog " + ::hForm[ "name" ] )
    FOR EACH h IN ::hForm[ "controls" ]
        ::oObj:addItem( hb_ntos( h:__enumIndex() ) + ". " + h[ "type" ] + " " + cbf_Caption( h ) )
    NEXT
    ::oObj:setCurrentIndex( ::nSel + 1 )
    ::repaintChild( ::oObj )

RETURN NIL

METHOD selectObj( n ) CLASS CBFormDlg
    IF n >= 0 .AND. n <= Len( ::hForm[ "controls" ] ) .AND. n != ::nSel
        ::nSel := n
        ::refresh()
    ENDIF
RETURN NIL

METHOD setTab( n ) CLASS CBFormDlg
    IF n != ::nTab
        ::nTab := n
        ::info()
    ENDIF
RETURN NIL

/* Ins: a control from a list, under the selected one */
METHOD addControl() CLASS CBFormDlg

    LOCAL n, nX := 1, nY := 1, h

    n := cb_List( "Add a control", cbf_Col( cbf_Types(), 2 ), 1 )
    IF n == 0
        RETURN NIL
    ENDIF
    IF ::nSel > 0
        h := ::hForm[ "controls" ][ ::nSel ]
        nX := h[ "x" ]
        nY := Min( h[ "y" ] + h[ "h" ], ::hForm[ "height" ] - 5 )
    ENDIF
    ::newControl( cbf_Control( cbf_Types()[ n ][ 1 ], nX, Max( 0, nY ), ::hForm[ "controls" ] ) )

RETURN NIL

/* a control picked on the palette (again: no more); a click on the dialog places it */
METHOD placeStart( cType ) CLASS CBFormDlg

    ::cPlace := IIF( ::cPlace == cType, "", cType )
    ::oStat:setText( IIF( Empty( ::cPlace ), ::cStat, ;
        " Click on the dialog where the " + cbf_Types()[ hb_AScan( cbf_Types(), {| x | x[ 1 ] == ::cPlace } ) ][ 2 ] + " goes.   Esc: no" ) )
    ::repaintChild( ::oStat )
    ::repaintChild( ::oCanvas )

RETURN NIL

METHOD placeAt( nX, nY ) CLASS CBFormDlg

    LOCAL cType := ::cPlace

    ::placeStart( cType )
    ::newControl( cbf_Control( cType, nX, nY, ::hForm[ "controls" ] ) )

RETURN NIL

METHOD newControl( h ) CLASS CBFormDlg

    AAdd( ::hForm[ "controls" ], h )
    ::nSel := Len( ::hForm[ "controls" ] )
    ::lChanged := .T.
    ::refresh()

RETURN NIL

METHOD deleteSel() CLASS CBFormDlg

    IF ::nSel > 0
        hb_ADel( ::hForm[ "controls" ], ::nSel, .T. )
        ::nSel := Min( ::nSel, Len( ::hForm[ "controls" ] ) )
        ::nPropSel := -1                  /* another control has this number now */
        ::nPattern := 0
        ::lChanged := .T.
        ::refresh()
    ENDIF

RETURN NIL

/* earlier (-1) / later (1) in the Tab order */
METHOD moveOrder( nDir ) CLASS CBFormDlg

    LOCAL a := ::hForm[ "controls" ], n := ::nSel + nDir, h

    IF ::nSel > 0 .AND. n >= 1 .AND. n <= Len( a )
        h := a[ n ]
        a[ n ] := a[ ::nSel ]
        a[ ::nSel ] := h
        ::nSel := n
        ::nPattern := 0
        ::lChanged := .T.
        ::refresh()
    ENDIF

RETURN NIL

METHOD copy() CLASS CBFormDlg

    IF ::nSel > 0
        ::hClip := hb_HClone( ::hForm[ "controls" ][ ::nSel ] )
        HTToast():show( "Copied: " + ::hClip[ "type" ] + " " + cbf_Caption( ::hClip ), 1200, HT_CLR_TOAST_INFO )
    ENDIF

RETURN NIL

/* a copy of the copied control at nX, nY (else under the last copy), with a name of its own */
METHOD paste( nX, nY ) CLASS CBFormDlg

    LOCAL h, a := ::hForm[ "controls" ]

    IF ::hClip == NIL
        HTToast():show( "Nothing has been copied (Ctrl-C)", 1500, HT_CLR_TOAST_WARNING )
        RETURN NIL
    ENDIF
    h := hb_HClone( ::hClip )
    IF nX == NIL
        nX := h[ "x" ]
        nY := h[ "y" ] + h[ "h" ]
    ENDIF
    h[ "x" ] := Max( 0, nX )
    h[ "y" ] := Max( 0, nY )
    ::hClip[ "y" ] := h[ "y" ]
    IF ! Empty( hb_HGetDef( h, "name", "" ) ) .AND. ;
       hb_AScan( a, {| x | Lower( hb_HGetDef( x, "name", "" ) ) == Lower( h[ "name" ] ) } ) > 0
        h[ "name" ] := cbf_NewName( h[ "type" ], a )
    ENDIF
    ::newControl( h )

RETURN NIL

/* the control the selection is aligned to: the pattern, else the one before it */
METHOD refIndex() CLASS CBFormDlg
RETURN IIF( ::nPattern > 0 .AND. ::nPattern <= Len( ::hForm[ "controls" ] ) .AND. ::nPattern != ::nSel, ::nPattern, ::nSel - 1 )

METHOD align( cHow ) CLASS CBFormDlg

    LOCAL a := ::hForm[ "controls" ], h, p, n

    IF ::nSel == 0
        RETURN NIL
    ENDIF
    h := a[ ::nSel ]
    IF cHow == "pattern"
        ::nPattern := ::nSel
        HTToast():show( "The pattern: " + cbf_Caption( h ), 1200, HT_CLR_TOAST_INFO )
        RETURN NIL
    ELSEIF cHow == "center"
        h[ "x" ] := Max( 0, Int( ( ::hForm[ "width" ] - h[ "w" ] ) / 2 ) )
    ELSE
        n := ::refIndex()
        IF n < 1
            HTToast():show( "No control to align to (Set as pattern)", 1500, HT_CLR_TOAST_WARNING )
            RETURN NIL
        ENDIF
        p := a[ n ]
        DO CASE
        CASE cHow == "left"  ; h[ "x" ] := p[ "x" ]
        CASE cHow == "row"   ; h[ "y" ] := p[ "y" ]
        CASE cHow == "width" ; h[ "w" ] := p[ "w" ]
        ENDCASE
    ENDIF
    ::lChanged := .T.
    ::refresh()

RETURN NIL

/* right click / Shift-F10: what can be done with the selection (nX, nY: the cell of the dialog) */
METHOD contextMenu( nAbsRow, nAbsCol, nX, nY ) CLASS CBFormDlg

    LOCAL oM := HTContextMenu():new(), a, c, n, oAct, cRef

    IF ::nSel > 0
        n := ::refIndex()
        cRef := IIF( n > 0, cbf_Caption( ::hForm[ "controls" ][ n ] ), "-" )
        a := { "Properties", "Events", "-", "Copy           Ctrl-C", "Paste          Ctrl-V", "Delete         Del", "-", ;
               "Set as pattern", "Align left to " + cRef, "Same row as " + cRef, "Same width as " + cRef, ;
               "Center in the dialog", "-", "Earlier in the Tab order", "Later in the Tab order" }
    ELSE
        a := { "Dialog properties", "Dialog events", "-", "Paste          Ctrl-V", "Add a control  Ins", "-", ;
               "Try it         F5", "Generate code  F9" }
    ENDIF
    FOR EACH c IN a
        IF c == "-"
            oM:addSeparator()
        ELSE
            oM:addAction( c )
        ENDIF
    NEXT
    oAct := oM:popup( nAbsRow, nAbsCol )
    ::repaint()
    IF oAct == NIL
        RETURN NIL
    ENDIF
    n := AScan( oM:actions(), {| x | x == oAct } )
    IF n == 0
        RETURN NIL
    ENDIF
    c := a[ n ]
    DO CASE
    CASE c == "Properties" .OR. c == "Dialog properties"
        ::setTab( 1 )
        ::focusProps( .T. )
    CASE c == "Events" .OR. c == "Dialog events"
        ::setTab( 2 )
        ::focusProps( .T. )
    CASE Left( c, 4 ) == "Copy"       ; ::copy()
    CASE Left( c, 5 ) == "Paste"      ; ::paste( nX, nY )
    CASE Left( c, 6 ) == "Delete"     ; ::deleteSel()
    CASE Left( c, 3 ) == "Set"        ; ::align( "pattern" )
    CASE Left( c, 10 ) == "Align left" ; ::align( "left" )
    CASE Left( c, 8 ) == "Same row"   ; ::align( "row" )
    CASE Left( c, 10 ) == "Same width" ; ::align( "width" )
    CASE Left( c, 6 ) == "Center"     ; ::align( "center" )
    CASE Left( c, 7 ) == "Earlier"    ; ::moveOrder( -1 )
    CASE Left( c, 5 ) == "Later"      ; ::moveOrder( 1 )
    CASE Left( c, 3 ) == "Add"        ; ::addControl()
    CASE Left( c, 3 ) == "Try"        ; ::preview()
    CASE Left( c, 8 ) == "Generate"   ; ::generate()
    ENDCASE

RETURN NIL

/* the label (1) or the key (2) of the current row of the browse ("" before there are rows) */
METHOD propRow( n ) CLASS CBFormDlg
RETURN IIF( ::aRec[ 1 ] >= 1 .AND. ::aRec[ 1 ] <= Len( ::aRows ), ::aRows[ ::aRec[ 1 ] ][ n ], "" )

/* a property of the selection as text; an event: its first line */
METHOD propGet( cKey ) CLASS CBFormDlg

    LOCAL x, a

    IF Empty( cKey )
        RETURN ""
    ENDIF
    x := hb_HGetDef( IIF( ::nSel == 0, ::hForm, ::hForm[ "controls" ][ ::nSel ] ), cKey, "" )
    IF ::nTab == 2
        a := cbf_Lines( hb_CStr( x ) )
        RETURN IIF( Len( a ) == 0, "", AllTrim( a[ 1 ] ) + IIF( Len( a ) > 1, " ...", "" ) )
    ENDIF

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
    IF cKey == "name" .OR. cKey == "text"
        ::objList()
    ENDIF

RETURN NIL

/* Enter / F2 / typing / double click on a row: an event opens its editor, a property with
   some values a list to choose from; the rest is typed in the cell (.T.) */
METHOD beginProp( nCol ) CLASS CBFormDlg

    LOCAL cKey := ::propRow( 2 ), a, n, cCur

    IF nCol != 2 .OR. Empty( cKey )
        RETURN .F.
    ENDIF
    IF ::nTab == 2
        ::editEvent( cKey )
        RETURN .F.
    ENDIF
    a := ::choices( cKey )
    IF a == NIL .OR. Empty( a )
        RETURN .T.
    ENDIF
    cCur := Upper( AllTrim( ::propGet( cKey ) ) )
    n := AScan( a, {| x | x[ 2 ] != NIL .AND. Upper( x[ 2 ] ) == cCur } )
    n := cb_List( ::propRow( 1 ), cbf_Col( a, 1 ), Max( 1, n ) )
    IF n == 0
        RETURN .F.
    ENDIF
    IF a[ n ][ 2 ] == NIL                 /* other ...: typed in the cell */
        RETURN .T.
    ENDIF
    ::propSet( cKey, a[ n ][ 2 ] )
    ::oProps:refreshCurrent()

RETURN .F.

/* the values of a property to choose from: { shown, value (NIL = type another) }; NIL = typed */
METHOD choices( cKey ) CLASS CBFormDlg

    LOCAL h, a, x

    IF ::nSel == 0
        IF cKey == "buttons"
            RETURN { { "OK,Cancel", "OK,Cancel" }, { "OK", "OK" }, { "Save,Cancel", "Save,Cancel" }, ;
                     { "Yes,No", "Yes,No" }, { "(none)", "" }, { "other ...", NIL } }
        ELSEIF cKey == "codepage"
            RETURN { { "UTF8   UTF-8 (recommended)", "UTF8" }, { "CP950  Big5, Traditional Chinese", "CP950" }, ;
                     { "GBK    GBK / CP936, Simplified Chinese", "GBK" } }
        ENDIF
        RETURN NIL
    ENDIF
    h := ::hForm[ "controls" ][ ::nSel ]
    DO CASE
    CASE cKey == "lang" .AND. h[ "type" ] == "date"
        a := { { "(the program's: ht_dateDefaultLang())", "" } }
        AEval( ht_dateLangs(), {| l | AAdd( a, { PadR( l, 8 ) + ht_dateLang( l )[ "months" ][ 1 ] + "  " + ht_dateLang( l )[ "format" ], l } ) } )
        RETURN a
    CASE cKey == "valtype"
        RETURN { { "C  text", "C" }, { "N  number", "N" }, { "D  date", "D" }, { "L  logical", "L" } }
    CASE cKey == "value" .AND. ( h[ "type" ] == "checkbox" .OR. h[ "type" ] == "radio" .OR. ;
                                ( h[ "type" ] == "get" .AND. hb_HGetDef( h, "valtype", "C" ) == "L" ) )
        RETURN { { ".T.  yes", ".T." }, { ".F.  no", ".F." } }
    CASE cKey == "value" .AND. ( h[ "type" ] == "combo" .OR. h[ "type" ] == "list" )
        a := {}
        FOR EACH x IN hb_ATokens( h[ "items" ], "," )
            AAdd( a, { hb_ntos( x:__enumIndex() ) + ". " + AllTrim( x ), hb_ntos( x:__enumIndex() ) } )
        NEXT
        RETURN a
    ENDCASE

RETURN NIL

/* an event: onClicked first offers ok / cancel; the code in an editor */
METHOD editEvent( cKey ) CLASS CBFormDlg

    LOCAL h := IIF( ::nSel == 0, ::hForm, ::hForm[ "controls" ][ ::nSel ] ), e, n, c, cP, cHint, cTitle

    e := cbf_Events( IIF( ::nSel == 0, "", h[ "type" ] ) )
    n := AScan( e, {| x | x[ 1 ] == cKey } )
    IF n == 0
        RETURN NIL
    ENDIF
    cP := e[ n ][ 3 ]
    cTitle := IIF( ::nSel == 0, "Dialog " + ::hForm[ "name" ], cbf_Caption( h ) ) + ": " + e[ n ][ 2 ]
    DO CASE
    CASE cKey == "oninit"
        cHint := "Statements before the dialog is shown (dlg, hCtl = the widgets by their names)"
    CASE cKey == "onaccept"
        cHint := "Statements after OK, before the values are returned (dlg, hCtl)"
    CASE cKey == "valid" .OR. cKey == "when"
        cHint := "An expression (.T. = yes); more lines: a function ( dlg, hCtl ) that RETURNs it"
    CASE cKey == "action"
        cHint := "One line: a function name - Func( dlg, hCtl ) - or an expression; more lines: a function"
    OTHERWISE
        cHint := "{|" + cP + "| <one line> }; more lines: a function ( dlg, hCtl, " + cP + " )"
    ENDCASE
    c := hb_CStr( hb_HGetDef( h, cKey, "" ) )
    IF cKey == "action"
        n := cb_List( "onClicked", { "ok - accept the dialog", "cancel - close it", "(nothing)", "code ..." }, ;
                      IIF( Lower( AllTrim( c ) ) == "ok", 1, IIF( Lower( AllTrim( c ) ) == "cancel", 2, IIF( Empty( c ), 3, 4 ) ) ) )
        DO CASE
        CASE n == 0 ; RETURN NIL
        CASE n == 1 ; c := "ok"
        CASE n == 2 ; c := "cancel"
        CASE n == 3 ; c := ""
        OTHERWISE
            IF Lower( AllTrim( c ) ) == "ok" .OR. Lower( AllTrim( c ) ) == "cancel"
                c := ""
            ENDIF
            c := cbf_EditCode( cTitle, cHint, c )
        ENDCASE
    ELSE
        c := cbf_EditCode( cTitle, cHint, c )
    ENDIF
    IF c == NIL
        RETURN NIL
    ENDIF
    h[ cKey ] := c
    ::lChanged := .T.
    ::oProps:refreshCurrent()
    ::repaintChild( ::oCanvas )

RETURN NIL

/* the keys to the properties (lOn) or back to the dialog */
METHOD focusProps( lOn ) CLASS CBFormDlg

    ::setFocusChild( IIF( lOn, ::oProps, ::oCanvas ) )
    ::oProps:browse():colPos := 2
    ::repaintChild( ::oProps )
    ::repaintChild( ::oObj )
    ::repaintChild( ::oCanvas )

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
            ELSEIF hCtl[ k ]:isDerivedFrom( "HTDatePicker" )
                c += k + " = " + ht_dateText( hCtl[ k ]:getValue() ) + Chr( 10 )
            ELSEIF hCtl[ k ]:isDerivedFrom( "HTSearchBox" )
                c += k + " = " + hb_ValToExp( hCtl[ k ]:getValue() ) + Chr( 10 )
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
    IF ! Empty( s_cGenWarn )
        HTMessageBox():warning( "CuiBuilder", s_cGenWarn )
    ENDIF
    HTMessageBox():information( "CuiBuilder", "Generated: " + cPrg + IIF( s_cGenCP == "UTF8", "", " (" + s_cGenCP + ")" ) + Chr( 10 ) + Chr( 10 ) + ;
        "In the program:" + Chr( 10 ) + ;
        "   hVal := " + cFunc + "( { " + IIF( Len( aSeen ) > 0, '"' + ATail( ::hForm[ "controls" ] )[ "type" ] + '" => ...', "" ) + " } )" + Chr( 10 ) + ;
        "   IF hVal != NIL" + Chr( 10 ) + "      ... hVal[ " + IIF( Len( aSeen ) > 0, '"' + aSeen[ 1 ] + '"', '"name"' ) + " ] ..." + Chr( 10 ) + "   ENDIF" )

RETURN NIL

METHOD PROCEDURE keyEvent( keyEvent ) CLASS CBFormDlg

    LOCAL nKey := keyEvent:key, n, h, nCount := Len( ::hForm[ "controls" ] )
    LOCAL lCtrl := hb_bitAnd( hb_gtInfo( HB_GTI_KBDSHIFTS ), HB_GTI_KBD_CTRL ) != 0

    keyEvent:accept()

    /* a control picked on the palette: Esc puts it back */
    IF nKey == K_ESC .AND. ! Empty( ::cPlace )
        ::placeStart( ::cPlace )
        RETURN
    ENDIF

    /* the combo box of the objects: Tab to the properties, Esc to the dialog */
    IF ::focusWidget() == ::oObj
        SWITCH nKey
        CASE K_ESC ; ::focusProps( .F. ) ; RETURN
        CASE K_TAB ; ::focusProps( .T. ) ; RETURN
        ENDSWITCH
        ::oObj:keyEvent( keyEvent )
        RETURN
    ENDIF

    /* the properties: the browse edits; Esc / Tab go back to the dialog */
    IF ::focusWidget() == ::oProps
        IF ! ::oProps:isEditing()
            SWITCH nKey
            CASE K_ESC
            CASE K_TAB
                ::focusProps( .F. )
                RETURN
            CASE K_SH_TAB
                ::setFocusChild( ::oObj )
                ::repaintChild( ::oProps )
                ::repaintChild( ::oObj )
                RETURN
            CASE K_LEFT  ; ::setTab( 1 ) ; RETURN
            CASE K_RIGHT ; ::setTab( 2 ) ; RETURN
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
    CASE K_INS                            /* = Ctrl-V */
        IF lCtrl
            ::paste()
        ELSE
            ::addControl()
        ENDIF
        RETURN
    CASE K_PGDN                           /* = Ctrl-C */
        IF lCtrl
            ::copy()
        ENDIF
        RETURN
    CASE K_SH_F10                         /* the menu at the selection */
        IF ::nSel > 0
            h := ::hForm[ "controls" ][ ::nSel ]
            ::contextMenu( 1 + ::oCanvas:y + ::oCanvas:nOffY + 1 + h[ "y" ], ::oCanvas:x + ::oCanvas:nOffX + h[ "x" ] )
        ELSE
            ::contextMenu( 1 + ::oCanvas:y + ::oCanvas:nOffY, ::oCanvas:x + ::oCanvas:nOffX + 2 )
        ENDIF
        RETURN
    CASE K_ENTER      ; ::focusProps( .T. ) ; RETURN
    CASE K_F3
        ::nSel := 0
        ::refresh()
        ::focusProps( .T. )
        RETURN
    CASE K_F5         ; ::preview() ; RETURN
    CASE K_F2         ; ::save() ; RETURN
    CASE K_F9         ; ::generate() ; RETURN
    CASE K_DEL        ; ::deleteSel() ; RETURN
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
    CASE K_CTRL_PGUP  ; ::moveOrder( -1 ) ; RETURN
    CASE K_CTRL_PGDN  ; ::moveOrder( 1 ) ; RETURN
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

/* the tabs over the browse: Properties | Events (a click switches) */
CLASS CBTabBar FROM HTWidget
    DATA oDlg
    METHOD new( parent )
    METHOD paintEvent( event )
    METHOD mouseEvent( eventMouse )
ENDCLASS

METHOD new( parent ) CLASS CBTabBar
    ::super:new( parent )
    ::setFocusPolicy( HT_FOCUS_NONE )
    ::FisVisible := .T.
RETURN self

METHOD PROCEDURE paintEvent( event ) CLASS CBTabBar

    LOCAL nTab := ::oDlg:nTab, cOn := HTTheme():getColor( HT_CLR_TAB_ACTIVE ), cOff := HTTheme():getColor( HT_CLR_TAB_INACTIVE )

    HB_SYMBOL_UNUSED( event )
    DispOutAt( 0, 0, Space( MaxCol() + 1 ), HTTheme():getColor( HT_CLR_TAB_BAR ) )
    DispOutAt( 0, 0, " Properties ", IIF( nTab == 1, cOn, cOff ) )
    DispOutAt( 0, 13, " Events ", IIF( nTab == 2, cOn, cOff ) )

RETURN

METHOD PROCEDURE mouseEvent( eventMouse ) CLASS CBTabBar
    IF eventMouse:nKey == K_LBUTTONDOWN
        ::oDlg:setTab( IIF( eventMouse:mouseCol < 13, 1, 2 ) )
    ENDIF
RETURN

/*----------------------------------------------------------- the canvas */

/* the palette and the dialog being designed, drawn as it will look */
CLASS CBFormCanvas FROM HTWidget
    DATA oDlg
    DATA oPrev                       /* the dialog whose widgets were painted last */
    DATA aPal       INIT {}          /* the palette (rows 0 ..): { row, from col, to col, type } */
    DATA nOffX      INIT 2           /* where the dialog's title bar starts on the canvas */
    DATA nOffY      INIT 2
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

    LOCAL d := ::oDlg, hF := d:hForm, nW := hF[ "width" ], nH := hF[ "height" ], nT, nL := ::nOffX
    LOCAL cSel := HTTheme():getColor( HT_CLR_LIST_SELECTED ), cTitle := HTTheme():getColor( HT_CLR_TITLEBAR )
    LOCAL cBtn := HTTheme():getColor( HT_CLR_TITLEBTN ), cWin, aObj := {}, oDlg, o, g, n, nBtn, nRight
    LOCAL cBar := HTTheme():getColor( HT_CLR_MENU_BAR ), cBarSel := HTTheme():getColor( HT_CLR_MENU_BAR_SEL ), t, c, nX
    LOCAL nMaxR := MaxRow(), nMaxC := MaxCol(), nTop, nLeft, nBot, nRgt

    HB_SYMBOL_UNUSED( event )
    DispBox( 0, 0, nMaxR, nMaxC, Replicate( " ", 9 ), HTTheme():getColor( HT_CLR_DESKTOP ) )

    /* the palette: a click picks a control, a click on the dialog places it */
    ::aPal := {}
    nX := 1
    n := 0
    DispOutAt( 0, 0, Space( nMaxC + 1 ), cBar )
    FOR EACH t IN cbf_Types()
        c := " " + t[ 5 ] + " "
        IF nX + Len( c ) - 1 > nMaxC .AND. nX > 1          /* no room: on the next row */
            n++
            nX := 1
            DispOutAt( n, 0, Space( nMaxC + 1 ), cBar )
        ENDIF
        DispOutAt( n, nX, c, IIF( t[ 1 ] == d:cPlace, cBarSel, cBar ) )
        AAdd( ::aPal, { n, nX, nX + Len( c ) - 1, t[ 1 ] } )
        nX += Len( c ) + 1
    NEXT
    ::nOffY := n + 2                        /* the dialog under the palette and an empty row */
    nT := ::nOffY

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

/* the palette picks a control; a click selects the control under the mouse (else the dialog)
   or places the picked one, a drag moves it; the right button opens the menu */
METHOD PROCEDURE mouseEvent( eventMouse ) CLASS CBFormCanvas

    LOCAL d := ::oDlg, nRow := eventMouse:mouseRow - ::nOffY - 1, nCol := eventMouse:mouseCol - ::nOffX, n, h, a
    LOCAL lIn := nRow >= 0 .AND. nRow < d:hForm[ "height" ] - 1 .AND. nCol >= 0 .AND. nCol < d:hForm[ "width" ]

    SWITCH eventMouse:nKey
    CASE K_LBUTTONDOWN
        IF eventMouse:mouseRow < ::nOffY - 1
            FOR EACH a IN ::aPal
                IF eventMouse:mouseRow == a[ 1 ] .AND. eventMouse:mouseCol >= a[ 2 ] .AND. eventMouse:mouseCol <= a[ 3 ]
                    d:placeStart( a[ 4 ] )
                ENDIF
            NEXT
            EXIT
        ENDIF
        IF ! Empty( d:cPlace )
            IF lIn
                d:placeAt( nCol, nRow )
            ELSE
                d:placeStart( d:cPlace )      /* outside the dialog: no */
            ENDIF
            EXIT
        ENDIF
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
    CASE K_RBUTTONDOWN
        d:nSel := IIF( lIn, d:controlAt( nRow, nCol ), 0 )
        d:refresh()
        d:contextMenu( eventMouse:mouseAbsRow, eventMouse:mouseAbsCol, IIF( lIn, nCol, NIL ), IIF( lIn, nRow, NIL ) )
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
        IF Empty( d:cPlace ) .AND. eventMouse:mouseRow >= ::nOffY - 1
            d:focusProps( .T. )
        ENDIF
        EXIT
    ENDSWITCH

RETURN
