/*
 * Compatibility helpers formerly provided by hbct / xhb.
 * Kept small so samples that still call these APIs link without hbct.
 */

#include "hbtui.ch"

/** CT-compatible: numeric attribute -> "fg/bg" color string. */
FUNCTION NToColor( nColor )
    LOCAL nAttr := Int( Val( hb_NToC( hb_defaultValue( nColor, 0 ) ) ) )
    LOCAL nFg := hb_bitAnd( nAttr, 0x0F )
    LOCAL nBg := hb_bitAnd( hb_bitShift( nAttr, -4 ), 0x0F )
RETURN hb_ntos( nFg ) + "/" + hb_ntos( nBg )

/** CT-compatible: color string -> numeric attribute. */
FUNCTION ColorToN( cColor )
    LOCAL n := hb_ColorToN( hb_defaultValue( cColor, "N/N" ) )
RETURN IIF( n < 0, 0, n )

/** xhb-compatible: any value to string. */
FUNCTION CStr( xVal )
    LOCAL cType := ValType( xVal )

    SWITCH cType
    CASE "C"
        RETURN xVal
    CASE "N"
        RETURN hb_NToC( xVal )
    CASE "D"
        RETURN DToC( xVal )
    CASE "L"
        RETURN IIF( xVal, ".T.", ".F." )
    CASE "A"
        RETURN "{...}"
    CASE "O"
        RETURN IIF( Empty( xVal ), "O:NIL", "O:" + xVal:className() )
    CASE "B"
        RETURN "{||...}"
    CASE "P"
        RETURN "<pointer>"
    CASE "H"
        RETURN "{=>}"
    CASE "T"
        RETURN hb_TToC( xVal )
    ENDSWITCH

RETURN "NIL"
