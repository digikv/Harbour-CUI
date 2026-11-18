/** @file cbcalc.prg
 * CBro - calculator (Help > Calculator, Ctrl-F1).
 *
 *   CBC_Calculator( xStart ) -> the last result (Use) or NIL (Close)
 *
 * The line takes any numeric expression (+ - * / ^ %, parentheses, Harbour
 * functions such as Round( x, 2 ) or Sqrt( 2 )); Enter or = computes it and
 * writes it on the tape. The keys of the pad type into the line.
 *
 * Copyright (c) 2026 Srđan Dragojlović
 * Dual licensed: GNU GPL version 3 or later (COPYING), or a commercial
 * license from the author - see LICENSE.
 */

#include "hbtui.ch"
#include "inkey.ch"

FUNCTION CBC_Calculator( xStart )

    LOCAL dlg, oLine, oTape, oResult, oBtn, aKeys, nRow, nCol, nResult := NIL, lUse := .F.
    LOCAL nW := 50, nH := 22

    dlg := CBCalcDlg():new( NIL, "Calculator  (Enter computes, Esc closes)" )
    dlg:resize( nW, nH )

    oLine := HTLineEdit():new( dlg )
    oLine:setGeometry( 2, 1, nW - 7, 1 )
    oLine:text := IIF( HB_ISNUMERIC( xStart ), hb_ntos( xStart ), "" )
    dlg:oLine := oLine

    oResult := HTLabel():new( "", dlg )
    oResult:setGeometry( 2, 2, nW - 7, 1 )
    oResult:alignment := HT_ALIGN_RIGHT
    dlg:oResult := oResult

    aKeys := { { "7", "8", "9", "/", "(" }, ;
               { "4", "5", "6", "*", ")" }, ;
               { "1", "2", "3", "-", "^" }, ;
               { "0", ".", "%", "+", "=" } }
    FOR nRow := 1 TO Len( aKeys )
        FOR nCol := 1 TO Len( aKeys[ nRow ] )
            oBtn := HTPushButton():new( aKeys[ nRow ][ nCol ], dlg )
            oBtn:setGeometry( 2 + ( nCol - 1 ) * 6, 3 + nRow * 2, 5, 1 )
            oBtn:onClicked := PadKey( dlg, aKeys[ nRow ][ nCol ] )
        NEXT
    NEXT
    oBtn := HTPushButton():new( "C", dlg )
    oBtn:setGeometry( 2, 13, 5, 1 )
    oBtn:onClicked := {|| oLine:text := "", dlg:showResult( "" ), dlg:repaintChild( oLine ) }
    oBtn := HTPushButton():new( "<-", dlg )
    oBtn:setGeometry( 8, 13, 5, 1 )
    oBtn:onClicked := {|| oLine:text := Left( oLine:text, Len( oLine:text ) - 1 ), dlg:repaintChild( oLine ) }

    HTLabel():new( "Tape", dlg ):move( 34, 4 )
    oTape := HTListBox():new( dlg )
    oTape:setGeometry( 34, 5, nW - 38, 9 )
    oTape:onActivated := {| n | oLine:text := oTape:itemText( n ), dlg:repaintChild( oLine ) }
    dlg:oTape := oTape

    oBtn := HTPushButton():new( "Use", dlg )
    oBtn:setGeometry( nW - 26, nH - 5, 10, 1 )
    oBtn:onClicked := {|| IIF( dlg:compute(), ( lUse := .T., dlg:accept() ), NIL ) }
    oBtn := HTPushButton():new( "Close", dlg )
    oBtn:setGeometry( nW - 14, nH - 5, 10, 1 )
    oBtn:onClicked := {|| dlg:reject() }

    dlg:exec()
    IF lUse
        nResult := dlg:nValue
    ENDIF

RETURN nResult

STATIC FUNCTION PadKey( dlg, cKey )
    IF cKey == "="
        RETURN {|| dlg:compute() }
    ENDIF
RETURN {|| dlg:oLine:text := dlg:oLine:text + cKey, dlg:repaintChild( dlg:oLine ) }

/* dialog that computes on Enter in the line */
CLASS CBCalcDlg FROM HTDialog

PUBLIC:
    DATA oLine
    DATA oResult
    DATA oTape
    DATA nValue

    METHOD keyEvent( keyEvent )
    METHOD compute()
    METHOD showResult( cText )

ENDCLASS

METHOD PROCEDURE keyEvent( keyEvent ) CLASS CBCalcDlg
    IF keyEvent:key == K_ENTER .AND. ::focusWidget() == ::oLine
        ::compute()
        keyEvent:accept()
        RETURN
    ENDIF
    ::super:keyEvent( keyEvent )
RETURN

/* the line's expression: result on the tape and in the line; .T. = a number */
METHOD FUNCTION compute() CLASS CBCalcDlg

    LOCAL cExpr := AllTrim( ::oLine:text ), xValue, oErr, cErr := ""

    IF Empty( cExpr )
        RETURN ::nValue != NIL
    ENDIF
    cExpr := StrTran( cExpr, ",", "." )
    cExpr := PercentOf( cExpr )
    BEGIN SEQUENCE WITH {| e | Break( e ) }
        xValue := Eval( hb_macroBlock( cExpr ) )
    RECOVER USING oErr
        cErr := IIF( HB_ISOBJECT( oErr ), oErr:description, "error" )
    END SEQUENCE
    IF ! Empty( cErr ) .OR. ! HB_ISNUMERIC( xValue )
        ::showResult( IIF( Empty( cErr ), "not a number", cErr ) )
        RETURN .F.
    ENDIF
    ::nValue := xValue
    ::oTape:addItem( ::oLine:text + " =" )
    ::oTape:addItem( NumText( xValue ) )
    ::oTape:setCurrentIndex( ::oTape:count() )
    ::oLine:text := NumText( xValue )
    ::showResult( "= " + NumText( xValue ) )
    ::repaintChild( ::oLine )
    ::repaintChild( ::oTape )

RETURN .T.

METHOD PROCEDURE showResult( cText ) CLASS CBCalcDlg
    ::oResult:setText( cText )
    ::oResult:setGeometry( 2, 2, ::Fwidth - 7, 1 )
    ::repaintChild( ::oResult )
RETURN

/* "200+10%" -> "200*(1+10/100)", "50%" -> "50/100" (calculator percent) */
STATIC FUNCTION PercentOf( cExpr )

    LOCAL nPct := At( "%", cExpr ), nStart, cNum, cLeft, cOp

    DO WHILE nPct > 0
        nStart := nPct - 1
        DO WHILE nStart > 0 .AND. SubStr( cExpr, nStart, 1 ) $ "0123456789."
            nStart--
        ENDDO
        cNum := SubStr( cExpr, nStart + 1, nPct - nStart - 1 )
        cLeft := Left( cExpr, nStart )
        cOp := Right( cLeft, 1 )
        IF cOp $ "+-" .AND. Len( cLeft ) > 1
            cExpr := "(" + Left( cLeft, Len( cLeft ) - 1 ) + ")*(1" + cOp + cNum + "/100)" + SubStr( cExpr, nPct + 1 )
        ELSE
            cExpr := cLeft + "(" + cNum + "/100)" + SubStr( cExpr, nPct + 1 )
        ENDIF
        nPct := At( "%", cExpr )
    ENDDO

RETURN cExpr

STATIC FUNCTION NumText( n )

    LOCAL c := LTrim( Str( n, 30, 10 ) )

    IF "." $ c
        DO WHILE Right( c, 1 ) == "0"
            c := Left( c, Len( c ) - 1 )
        ENDDO
        IF Right( c, 1 ) == "."
            c := Left( c, Len( c ) - 1 )
        ENDIF
    ENDIF

RETURN c
