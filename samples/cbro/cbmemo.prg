/** @file cbmemo.prg
 * CBro - memo fields that hold more than text.
 *
 * An FPT memo (DBFCDX) stores any value: text, an array (also nested),
 * a date, a number or a logical value - as BroPlus did with SIx. Text may
 * itself be JSON. These functions show and edit such values:
 *
 *   CBM_Kind( x )                  "TEXT" "JSON" "ARRAY" "DATE" "NUMBER" "LOGICAL" "EMPTY"
 *   CBM_Cell( x, nWidth )          the browse cell (<Memo>, <Array>, ... or the value)
 *   CBM_ToText( x, cKind )         text to edit: arrays as Harbour literals, one
 *                                  element per line; JSON pretty printed
 *   CBM_FromText( cText, cKind, @cErr ) -> value (NIL + cErr when invalid)
 *   CBM_Plain( x )                 one-line text for CSV, lists and xlsx
 *
 * Copyright (c) 2026 Srđan Dragojlović
 * Dual licensed: GNU GPL version 3 or later (COPYING), or a commercial
 * license from the author - see LICENSE.
 */

#define INDENT  "   "

FUNCTION CBM_Kind( x )

    LOCAL c

    SWITCH ValType( x )
    CASE "A"
        RETURN "ARRAY"
    CASE "D"
    CASE "T"
        RETURN "DATE"
    CASE "N"
        RETURN "NUMBER"
    CASE "L"
        RETURN "LOGICAL"
    CASE "C"
    CASE "M"
        c := AllTrim( StrTran( StrTran( x, Chr( 13 ), "" ), Chr( 10 ), "" ) )
        IF c == ""
            RETURN "EMPTY"
        ENDIF
        IF ( Left( c, 1 ) == "{" .AND. Right( c, 1 ) == "}" ) .OR. ( Left( c, 1 ) == "[" .AND. Right( c, 1 ) == "]" )
            RETURN "JSON"
        ENDIF
        RETURN "TEXT"
    ENDSWITCH

RETURN "EMPTY"

/* browse cell: nWidth 0 = a tag, else the value on one line */
FUNCTION CBM_Cell( x, nWidth )

    LOCAL cKind := CBM_Kind( x )

    IF nWidth <= 0
        SWITCH cKind
        CASE "EMPTY"
            RETURN Space( 7 )
        CASE "TEXT"
            RETURN "<Memo> "
        CASE "JSON"
            RETURN "<JSON> "
        CASE "ARRAY"
            RETURN "<Array>"
        CASE "DATE"
            RETURN "<Date> "
        CASE "NUMBER"
            RETURN "<Num>  "
        ENDSWITCH
        RETURN "<Log>  "
    ENDIF

RETURN ht_cellPadR( CBM_Plain( x ), nWidth )

/* one line: text without line breaks, arrays as JSON, other values as text */
FUNCTION CBM_Plain( x )

    SWITCH ValType( x )
    CASE "C"
    CASE "M"
        RETURN StrTran( StrTran( StrTran( x, Chr( 13 ) + Chr( 10 ), " " ), Chr( 10 ), " " ), Chr( 9 ), " " )
    CASE "A"
        RETURN hb_jsonEncode( x )
    CASE "D"
        RETURN DToC( x )
    CASE "T"
        RETURN hb_TToC( x )
    CASE "N"
        RETURN LTrim( Str( x ) )
    CASE "L"
        RETURN IIF( x, ".T.", ".F." )
    ENDSWITCH

RETURN ""

/* the text the editor shows for a value of kind cKind */
FUNCTION CBM_ToText( x, cKind )

    LOCAL xJson

    SWITCH cKind
    CASE "ARRAY"
        RETURN IIF( HB_ISARRAY( x ), ArrayText( x, "" ), "{}" )
    CASE "JSON"
        IF HB_ISARRAY( x )
            RETURN hb_jsonEncode( x, .T. )
        ENDIF
        IF HB_ISSTRING( x ) .AND. hb_jsonDecode( x, @xJson ) > 0
            RETURN hb_jsonEncode( xJson, .T. )
        ENDIF
        RETURN hb_defaultValue( x, "" )
    CASE "DATE"
        RETURN IIF( HB_ISDATE( x ) .OR. HB_ISDATETIME( x ), DToC( x ), "" )
    CASE "NUMBER"
        RETURN IIF( HB_ISNUMERIC( x ), LTrim( Str( x, 30, 10 ) ), "" )
    CASE "LOGICAL"
        RETURN IIF( HB_ISLOGICAL( x ) .AND. x, ".T.", ".F." )
    ENDSWITCH

RETURN IIF( HB_ISSTRING( x ), x, CBM_Plain( x ) )

/* the value of an edited text; NIL with cErr when it is not valid */
FUNCTION CBM_FromText( cText, cKind, cErr )

    LOCAL x, oErr, n

    cErr := ""
    SWITCH cKind
    CASE "ARRAY"
        BEGIN SEQUENCE WITH {| e | Break( e ) }
            x := Eval( hb_macroBlock( StrTran( StrTran( cText, Chr( 13 ), " " ), Chr( 10 ), " " ) ) )
        RECOVER USING oErr
            cErr := "Not a valid array: " + IIF( HB_ISOBJECT( oErr ), oErr:description, "syntax error" )
            RETURN NIL
        END SEQUENCE
        IF ! HB_ISARRAY( x )
            cErr := "The text is a " + ValType( x ) + " value, not an array { ... }"
            RETURN NIL
        ENDIF
        RETURN x
    CASE "JSON"
        n := hb_jsonDecode( cText, @x )
        IF n == 0 .OR. ! Empty( SubStr( cText, n + 1 ) )
            cErr := "Not valid JSON" + IIF( n > 0, " (after position " + hb_ntos( n ) + ")", "" )
            RETURN NIL
        ENDIF
        RETURN cText
    CASE "DATE"
        x := CToD( AllTrim( cText ) )
        IF Empty( x ) .AND. ! Empty( StrTran( StrTran( AllTrim( cText ), ".", "" ), "/", "" ) )
            cErr := "Not a date in the format " + Set( _SET_DATEFORMAT )
            RETURN NIL
        ENDIF
        RETURN x
    CASE "NUMBER"
        cText := StrTran( AllTrim( cText ), ",", "." )
        IF Empty( cText ) .OR. ! IsNumText( cText )
            cErr := "Not a number"
            RETURN NIL
        ENDIF
        RETURN Val( cText )
    CASE "LOGICAL"
        cText := Upper( AllTrim( StrTran( cText, ".", "" ) ) )
        IF ! Left( cText, 1 ) $ "TFYN"
            cErr := "Not a logical value (.T. / .F.)"
            RETURN NIL
        ENDIF
        RETURN Left( cText, 1 ) $ "TY"
    ENDSWITCH

RETURN cText

STATIC FUNCTION IsNumText( c )

    LOCAL ch, lDot := .F., n

    FOR n := 1 TO Len( c )
        ch := SubStr( c, n, 1 )
        DO CASE
        CASE ch $ "+-" .AND. n == 1
        CASE ch == "." .AND. ! lDot
            lDot := .T.
        CASE ! IsDigit( ch )
            RETURN .F.
        ENDCASE
    NEXT

RETURN .T.

/* an array as a Harbour literal, one element per line, nested arrays
   indented: { 1, "two", 0d20240101, { "nested" } } */
STATIC FUNCTION ArrayText( a, cIndent )

    LOCAL cOut, x, cEol := hb_eol()

    IF Empty( a )
        RETURN "{}"
    ENDIF
    IF AScan( a, {| e | HB_ISARRAY( e ) } ) == 0 .AND. Len( hb_ValToExp( a ) ) <= 60
        RETURN hb_ValToExp( a )
    ENDIF
    cOut := "{" + cEol
    FOR EACH x IN a
        cOut += cIndent + INDENT + IIF( HB_ISARRAY( x ), ArrayText( x, cIndent + INDENT ), hb_ValToExp( x ) ) + ;
                IIF( x:__enumIsLast(), "", "," ) + cEol
    NEXT

RETURN cOut + cIndent + "}"
