/** @file cbreport.prg
 * Report Form to Word (.docx) and Excel (.xlsx) - shared by CBro and the
 * report designer of CuiBuilder (and the programs it generates reports for)
 *
 * A Clipper .frm report (title, columns with totals, a group and a
 * subgroup) is read by Harbour's own HBReportForm:LoadReportFile(); the
 * records are walked as REPORT FORM does (FOR, groups, subtotals, total,
 * SUMMARY), and the result is written as a Word table (the DOCX class of
 * Srđan Dragojlović, github.com/digikv/docx) or an Excel sheet (the XLSX
 * class) instead of text.
 *
 *   CBR_Load( cFrm, @cErr )                    -> aReport or NIL
 *   CBR_Save( cFrm, aReport, @cErr )           -> lOk     (writes a .frm)
 *   CBR_Rows( aReport, bFor )                  -> { { cKind, nLevel, aValues, cLabel }, ... }
 *        cKind: "G" group heading, "D" detail, "S" subtotal, "T" total
 *   CBR_Docx( cFile, aReport, aRows, cTitle, @cErr, cCdp ) -> lOk
 *   CBR_Xlsx( cFile, aReport, aRows, cTitle, @cErr, cCdp ) -> lOk
 *
 * A report made in code (what CuiBuilder generates):
 *   aRep := CBR_New( [nWidth], [lSummary] )
 *   CBR_Title( aRep, { "line 1", "line 2" } )
 *   CBR_Group( aRep, cExpr, cHeader )          (twice: group, subgroup)
 *   CBR_Column( aRep, cExpr, cHeader, nWidth, nDecimals, lTotal )   (";" in cHeader: lines)
 *   CBR_Make( cFile, aRep, [bFor], [cTitle], @cErr, [cCdp] ) -> lOk
 *        the current work area to cFile: .docx = Word, .xlsx = Excel
 *
 * Built without the DOCX / XLSX class: -DCBR_NO_DOCX / -DCBR_NO_XLSX
 * (CBro: CBRO_NO_DOCX / CBRO_NO_XLSX). The classes need hbmzip.hbc.
 *
 * Copyright (c) 2026 Srđan Dragojlović
 * Dual licensed: GNU GPL version 3 or later (COPYING), or a commercial
 * license from the author - see LICENSE.
 */

#ifdef CBR_NO_DOCX
   #define CBRO_NO_DOCX
#endif
#ifdef CBR_NO_XLSX
   #define CBRO_NO_XLSX
#endif

#include "set.ch"

/* the report array of Harbour's treport.prg */
#define RPT_HEADER      1
#define RPT_LMARGIN     3
#define RPT_RMARGIN     4
#define RPT_LINES       5
#define RPT_SPACING     6
#define RPT_BEJECT      7
#define RPT_AEJECT      8
#define RPT_PLAIN       9
#define RPT_WIDTH       2
#define RPT_SUMMARY     10
#define RPT_COLUMNS     11
#define RPT_GROUPS      12
#define RPT_HEADING     13

#define RCT_EXP         1
#define RCT_TEXT        2
#define RCT_TYPE        3
#define RCT_HEADER      4
#define RCT_WIDTH       5
#define RCT_DECIMALS    6
#define RCT_TOTAL       7
#define RCT_PICT        8

#define RGT_EXP         1
#define RGT_TEXT        2
#define RGT_HEADER      4
#define RGT_AEJECT      5

/* a .frm file -> the report array of Harbour (NIL and cErr on an error) */
FUNCTION CBR_Load( cFrm, cErr )

    LOCAL aRep, oErr

    cErr := ""
    BEGIN SEQUENCE WITH {| e | Break( e ) }
        aRep := HBReportForm():LoadReportFile( cFrm )
    RECOVER USING oErr
        cErr := "Cannot read " + hb_FNameNameExt( cFrm ) + ": " + oErr:description
        RETURN NIL
    END SEQUENCE
    IF ! HB_ISARRAY( aRep ) .OR. Len( aRep ) < RPT_COLUMNS .OR. Empty( aRep[ RPT_COLUMNS ] )
        cErr := hb_FNameNameExt( cFrm ) + " is not a report form"
        RETURN NIL
    ENDIF

RETURN aRep

/* the rows of the report from the current work area, from the top */
FUNCTION CBR_Rows( aRep, bFor )

    LOCAL aCols := aRep[ RPT_COLUMNS ], aGrp := hb_defaultValue( aRep[ RPT_GROUPS ], {} ), nG
    LOCAL aRows := {}, aLast, aSub := {}, aTot, lFirst := .T., i, nBreak, x, aVal, nLevel, aCol
    LOCAL lSummary := hb_defaultValue( aRep[ RPT_SUMMARY ], .F. )
    LOCAL lTotals := AScan( aCols, {| c | hb_defaultValue( c[ RCT_TOTAL ], .F. ) } ) > 0

    nG := Len( aGrp )
    aLast := Array( nG )
    aTot := CBR_Zero( aCols )
    FOR i := 1 TO nG
        AAdd( aSub, CBR_Zero( aCols ) )
    NEXT
    dbGoTop()
    DO WHILE ! Eof()
        IF bFor == NIL .OR. Eval( bFor )
            /* a new group or subgroup: the subtotals of the ones that end */
            nBreak := 0
            FOR i := 1 TO nG
                x := Eval( aGrp[ i ][ RGT_EXP ] )
                IF lFirst .OR. !( hb_ValToExp( x ) == hb_ValToExp( aLast[ i ] ) )
                    nBreak := i
                    EXIT
                ENDIF
            NEXT
            IF nBreak > 0
                IF ! lFirst
                    FOR nLevel := nG TO nBreak STEP -1
                        IF lTotals
                            AAdd( aRows, { "S", nLevel, aSub[ nLevel ], IIF( nLevel == 1, "** Subtotal **", "* Subsubtotal *" ) } )
                        ENDIF
                        aSub[ nLevel ] := CBR_Zero( aCols )
                    NEXT
                ENDIF
                FOR nLevel := nBreak TO nG
                    aLast[ nLevel ] := Eval( aGrp[ nLevel ][ RGT_EXP ] )
                    AAdd( aRows, { "G", nLevel, NIL, IIF( nLevel == 1, "** ", "* " ) + ;
                                   CBR_Unquote( aGrp[ nLevel ][ RGT_HEADER ] ) + " " + CBR_Text( aLast[ nLevel ] ) } )
                NEXT
            ENDIF
            lFirst := .F.

            aVal := {}
            FOR EACH aCol IN aCols
                x := Eval( aCol[ RCT_EXP ] )
                AAdd( aVal, x )
                IF hb_defaultValue( aCol[ RCT_TOTAL ], .F. ) .AND. HB_ISNUMERIC( x )
                    i := aCol:__enumIndex()
                    aTot[ i ] += x
                    FOR nLevel := 1 TO nG
                        aSub[ nLevel ][ i ] += x
                    NEXT
                ENDIF
            NEXT
            IF ! lSummary
                AAdd( aRows, { "D", 0, aVal, "" } )
            ENDIF
        ENDIF
        dbSkip()
    ENDDO
    IF ! lFirst .AND. lTotals
        FOR nLevel := nG TO 1 STEP -1
            AAdd( aRows, { "S", nLevel, aSub[ nLevel ], IIF( nLevel == 1, "** Subtotal **", "* Subsubtotal *" ) } )
        NEXT
        AAdd( aRows, { "T", 0, aTot, "*** Total ***" } )
    ENDIF

RETURN aRows

/* 0 for the columns with a total, NIL for the others */
STATIC FUNCTION CBR_Zero( aCols )

    LOCAL a := {}, c

    FOR EACH c IN aCols
        AAdd( a, IIF( hb_defaultValue( c[ RCT_TOTAL ], .F. ), 0, NIL ) )
    NEXT

RETURN a

/* a value as REPORT FORM writes it in a group heading */
STATIC FUNCTION CBR_Text( x )
    DO CASE
    CASE HB_ISSTRING( x )
        RETURN RTrim( x )
    CASE HB_ISDATETIME( x ) .AND. ! HB_ISDATE( x )
        RETURN hb_TToC( x )
    CASE HB_ISDATE( x )
        RETURN DToC( x )
    CASE HB_ISNUMERIC( x )
        RETURN hb_ntos( x )
    CASE HB_ISLOGICAL( x )
        RETURN IIF( x, "T", "F" )
    ENDCASE
RETURN ""

/* a heading kept as a string expression ("First" with its quotes) -> its text */
STATIC FUNCTION CBR_Unquote( c )
    c := AllTrim( hb_defaultValue( c, "" ) )
    IF Len( c ) >= 2 .AND. Left( c, 1 ) $ ["'] .AND. Right( c, 1 ) == Left( c, 1 )
        c := SubStr( c, 2, Len( c ) - 2 )
    ENDIF
RETURN c

/* the heading of a column: its lines joined */
STATIC FUNCTION CBR_ColHead( aCol )

    LOCAL c := "", cLine

    FOR EACH cLine IN hb_defaultValue( aCol[ RCT_HEADER ], {} )
        IF HB_ISSTRING( cLine ) .AND. ! Empty( cLine := CBR_Unquote( cLine ) )
            c += IIF( Empty( c ), "", " " ) + cLine
        ENDIF
    NEXT

RETURN IIF( Empty( c ), CBR_Unquote( aCol[ RCT_TEXT ] ), c )

/* the title lines: the page heading of the form, else cTitle */
STATIC FUNCTION CBR_Titles( aRep, cTitle )

    LOCAL aOut := {}, c

    FOR EACH c IN hb_defaultValue( aRep[ RPT_HEADER ], {} )
        IF HB_ISSTRING( c ) .AND. ! Empty( c := CBR_Unquote( c ) )
            AAdd( aOut, c )
        ENDIF
    NEXT
    IF HB_ISSTRING( aRep[ RPT_HEADING ] ) .AND. ! Empty( aRep[ RPT_HEADING ] )
        AEval( hb_ATokens( aRep[ RPT_HEADING ], ";" ), {| x | AAdd( aOut, AllTrim( x ) ) } )
    ENDIF
    IF Empty( aOut )
        AAdd( aOut, cTitle )
    ENDIF

RETURN aOut

/* the type of a column: from the form, else from its first value */
STATIC FUNCTION CBR_Type( aCol, aRows, nCol )

    LOCAL r

    IF HB_ISSTRING( aCol[ RCT_TYPE ] ) .AND. ! Empty( aCol[ RCT_TYPE ] )
        RETURN aCol[ RCT_TYPE ]
    ENDIF
    FOR EACH r IN aRows
        IF r[ 1 ] == "D" .AND. r[ 3 ][ nCol ] != NIL
            RETURN ValType( r[ 3 ][ nCol ] )
        ENDIF
    NEXT

RETURN "C"

#ifndef CBRO_NO_DOCX

/* European numbers (1.234,56) when SET DATE is a European one */
STATIC FUNCTION CBR_European()
RETURN SubStr( Set( _SET_DATEFORMAT ), 3, 1 ) $ ".-"

/* the report as a Word table: a header row repeated on every page, group
   headings shaded, subtotals and the total in bold */
FUNCTION CBR_Docx( cFile, aRep, aRows, cTitle, cErr, cCdp )

    LOCAL oDoc := WordDocument():New( cFile ), aCols := aRep[ RPT_COLUMNS ], nCols := Len( aCols )
    LOCAL aW := {}, nTotW := 0, nPageW, oT, nBold, nHead, nGroup, nSum, nTitle, aPict := {}, aHead := {}
    LOCAL r, nRow, i, x, aVal, cType, aCol, nW, cPic, cFoot

    IF HB_ISSTRING( cCdp ) .AND. ! Empty( cCdp )
        oDoc:cCodePage := cCdp
    ENDIF
    oDoc:cTitle := cTitle

    /* the widths: the characters of the column (or of its heading) */
    FOR EACH aCol IN aCols
        nW := Max( hb_defaultValue( aCol[ RCT_WIDTH ], 10 ), Min( Len( CBR_ColHead( aCol ) ), 20 ) )
        AAdd( aW, nW )
        nTotW += nW
    NEXT
    oDoc:lLandscape := nTotW > 90
    oDoc:nLeftMargin := 1.5
    oDoc:nRightMargin := 1.5
    nPageW := IIF( oDoc:lLandscape, 29.7, 21.0 ) - 3.0
    AEval( aW, {| n, k | aW[ k ] := Round( Max( 1.0, n / nTotW * nPageW ), 2 ) } )
    IF nTotW > 140
        oDoc:nFontSize := 8
    ELSEIF nTotW > 100
        oDoc:nFontSize := 9
    ENDIF

    cFoot := "Page &P of &" + "N"
    oDoc:AddHeader( cTitle, "", "&D" )
    oDoc:AddFooter( "", "", cFoot )

    nBold := oDoc:NewFont( , , .T. )
    nTitle := oDoc:NewStyle( oDoc:NewFont( , 14, .T. ), 2, , 2 )
    nHead := oDoc:NewStyle( nBold, 2 )
    nGroup := oDoc:NewStyle( nBold )
    nSum := oDoc:NewStyle( nBold, 3 )
    FOR EACH x IN CBR_Titles( aRep, cTitle )
        oDoc:Paragraph( x, nTitle )
    NEXT

    /* number pictures per column */
    FOR EACH aCol IN aCols
        i := aCol:__enumIndex()
        cType := CBR_Type( aCol, aRows, i )
        cPic := NIL
        IF cType == "N"
            cPic := Replicate( "9", Max( 1, hb_defaultValue( aCol[ RCT_WIDTH ], 10 ) - hb_defaultValue( aCol[ RCT_DECIMALS ], 0 ) ) )
            cPic := CBR_Thousands( cPic )             /* @E shows 1.234,56 */
            IF hb_defaultValue( aCol[ RCT_DECIMALS ], 0 ) > 0
                cPic += "." + Replicate( "9", aCol[ RCT_DECIMALS ] )
            ENDIF
            cPic := IIF( CBR_European(), "@E ", "" ) + cPic
        ENDIF
        AAdd( aPict, cPic )
        AAdd( aHead, CBR_ColHead( aCol ) )
    NEXT

    oT := oDoc:Table( aW )
    oT:nHeaderRows := 1
    oT:aPictures := aPict
    oT:AddRow( aHead, nHead, "D9E1F2" )
    FOR EACH r IN aRows
        SWITCH r[ 1 ]
        CASE "G"
            nRow := oT:AddRow()
            IF nCols > 1
                oT:Merge( nRow, 1, nRow, nCols )
            ENDIF
            oT:Cell( nRow, 1, r[ 4 ], nGroup )
            oT:Shade( nRow, NIL, IIF( r[ 2 ] == 1, "E2EFDA", "F2F2F2" ) )
            EXIT
        CASE "D"
            aVal := {}
            FOR EACH x IN r[ 3 ]
                AAdd( aVal, IIF( HB_ISNUMERIC( x ), x, CBR_Text( x ) ) )
            NEXT
            oT:AddRow( aVal )
            EXIT
        OTHERWISE                                    /* "S" subtotal, "T" total */
            nRow := oT:AddRow()
            FOR i := 1 TO nCols
                IF r[ 3 ][ i ] != NIL
                    oT:Cell( nRow, i, AllTrim( IIF( aPict[ i ] == NIL, hb_ntos( r[ 3 ][ i ] ), ;
                                                    Transform( r[ 3 ][ i ], aPict[ i ] ) ) ), nSum )
                ENDIF
            NEXT
            IF r[ 3 ][ 1 ] == NIL
                oT:Cell( nRow, 1, r[ 4 ], nGroup )
            ENDIF
            oT:Shade( nRow, NIL, IIF( r[ 1 ] == "T", "D9E1F2", "F2F2F2" ) )
        ENDSWITCH
    NEXT

    IF ! oDoc:Save()
        cErr := oDoc:cError
        RETURN .F.
    ENDIF

RETURN .T.

/* "9999999" -> "9,999,999" (with @E it shows 9.999.999) */
STATIC FUNCTION CBR_Thousands( cPic )

    LOCAL cOut := ""

    DO WHILE Len( cPic ) > 3
        cOut := "," + Right( cPic, 3 ) + cOut
        cPic := Left( cPic, Len( cPic ) - 3 )
    ENDDO

RETURN cPic + cOut

#else

FUNCTION CBR_Docx( cFile, aRep, aRows, cTitle, cErr, cCdp )
    HB_SYMBOL_UNUSED( cFile )
    HB_SYMBOL_UNUSED( aRep )
    HB_SYMBOL_UNUSED( aRows )
    HB_SYMBOL_UNUSED( cTitle )
    HB_SYMBOL_UNUSED( cCdp )
    cErr := "Word (.docx) is not available: the program was built without the DOCX class" + ;
            " (https://github.com/digikv/docx - clone it next to the Harbour-CUI folder)"
RETURN .F.

#endif

#ifndef CBRO_NO_XLSX

/* the report as an Excel sheet: the title, the headings, numbers as numbers */
FUNCTION CBR_Xlsx( cFile, aRep, aRows, cTitle, cErr, cCdp )

    LOCAL oBook := WorkBook():New( cFile ), oSheet, aCols := aRep[ RPT_COLUMNS ], nCols := Len( aCols )
    LOCAL nTitle, nHead, nGroup, nSum, nDate, aStyle := {}, aSumStyle := {}, nRow := 0, r, i, x, aCol, cType, nDec

    oBook:Application := "CBro"
    IF HB_ISSTRING( cCdp ) .AND. ! Empty( cCdp )
        oBook:cCodePage := cCdp
    ENDIF
    oSheet := oBook:WorkSheet( CBR_SheetName( cTitle ) )
    nTitle := oBook:NewStyle( oBook:NewFont( , 14, .T. ) )
    nHead := oBook:NewStyle( oBook:NewFont( , 11, .T. ), oBook:NewBorder( 0, 0, 0, 1 ), ;
                             oBook:NewFillPattern( 1, "FFD9E1F2", "FFD9E1F2" ) )
    nGroup := oBook:NewStyle( oBook:NewFont( , 11, .T. ), , oBook:NewFillPattern( 1, "FFE2EFDA", "FFE2EFDA" ) )
    nSum := oBook:NewStyle( oBook:NewFont( , 11, .T. ) )
    nDate := oBook:NewStyle( , , , , , oBook:NewFormat( Lower( Set( _SET_DATEFORMAT ) ) ) )
    FOR EACH aCol IN aCols
        i := aCol:__enumIndex()
        cType := CBR_Type( aCol, aRows, i )
        nDec := hb_defaultValue( aCol[ RCT_DECIMALS ], 0 )
        DO CASE
        CASE cType == "N"
            x := oBook:NewFormat( "#,##0" + IIF( nDec > 0, "." + Replicate( "0", Min( nDec, 9 ) ), "" ) )
            AAdd( aStyle, oBook:NewStyle( , , , , , x ) )
            AAdd( aSumStyle, oBook:NewStyle( oBook:NewFont( , 11, .T. ), oBook:NewBorder( 0, 1, 0, 0 ), , , , x ) )
        CASE cType $ "DT"
            AAdd( aStyle, nDate )
            AAdd( aSumStyle, nSum )
        OTHERWISE
            AAdd( aStyle, NIL )
            AAdd( aSumStyle, nSum )
        ENDCASE
        oSheet:ColumnsWidth( i, i, Min( Max( hb_defaultValue( aCol[ RCT_WIDTH ], 10 ), Len( CBR_ColHead( aCol ) ) ) + 2, 60 ) )
    NEXT

    FOR EACH x IN CBR_Titles( aRep, cTitle )
        oSheet:Cell( { ++nRow, 1 }, x, nTitle )
    NEXT
    nRow++
    nRow++
    FOR i := 1 TO nCols
        oSheet:Cell( { nRow, i }, CBR_ColHead( aCols[ i ] ), nHead )
    NEXT
    FOR EACH r IN aRows
        nRow++
        SWITCH r[ 1 ]
        CASE "G"
            oSheet:Cell( { nRow, 1 }, r[ 4 ], nGroup )
            EXIT
        CASE "D"
            FOR i := 1 TO nCols
                x := r[ 3 ][ i ]
                DO CASE
                CASE HB_ISSTRING( x )
                    IF ! ( x := RTrim( x ) ) == ""
                        oSheet:Cell( { nRow, i }, x )
                    ENDIF
                CASE HB_ISDATE( x ) .OR. HB_ISDATETIME( x )
                    IF ! Empty( x )
                        oSheet:Cell( { nRow, i }, x, aStyle[ i ] )
                    ENDIF
                CASE HB_ISNUMERIC( x ) .OR. HB_ISLOGICAL( x )
                    oSheet:Cell( { nRow, i }, x, aStyle[ i ] )
                ENDCASE
            NEXT
            EXIT
        OTHERWISE
            IF r[ 3 ][ 1 ] == NIL
                oSheet:Cell( { nRow, 1 }, r[ 4 ], nSum )
            ENDIF
            FOR i := 1 TO nCols
                IF r[ 3 ][ i ] != NIL
                    oSheet:Cell( { nRow, i }, r[ 3 ][ i ], aSumStyle[ i ] )
                ENDIF
            NEXT
        ENDSWITCH
    NEXT

    IF ! oBook:Save()
        cErr := oBook:cError
        RETURN .F.
    ENDIF

RETURN .T.

/* a sheet name: at most 31 characters, none of : \ / ? * [ ] */
STATIC FUNCTION CBR_SheetName( c )

    LOCAL ch

    FOR EACH ch IN ":\/?*[]"
        c := StrTran( c, ch, " " )
    NEXT
    c := AllTrim( Left( c, 31 ) )

RETURN IIF( Empty( c ), "Report", c )

#else

FUNCTION CBR_Xlsx( cFile, aRep, aRows, cTitle, cErr, cCdp )
    HB_SYMBOL_UNUSED( cFile )
    HB_SYMBOL_UNUSED( aRep )
    HB_SYMBOL_UNUSED( aRows )
    HB_SYMBOL_UNUSED( cTitle )
    HB_SYMBOL_UNUSED( cCdp )
    cErr := "Excel (.xlsx) is not available: the program was built without the XLSX class" + ;
            " (https://github.com/digikv/xlsx - clone it next to the Harbour-CUI folder)"
RETURN .F.

#endif

/*----------------------------------------------------------- a report in code */

/* an empty report: the defaults of REPORT FORM */
FUNCTION CBR_New( nWidth, lSummary )

    LOCAL aRep := Array( RPT_HEADING )

    aRep[ RPT_HEADER ]  := {}
    aRep[ RPT_WIDTH ]   := hb_defaultValue( nWidth, 80 )
    aRep[ RPT_LMARGIN ] := 8
    aRep[ RPT_RMARGIN ] := 0
    aRep[ RPT_LINES ]   := 58
    aRep[ RPT_SPACING ] := 1
    aRep[ RPT_BEJECT ]  := .T.
    aRep[ RPT_AEJECT ]  := .F.
    aRep[ RPT_PLAIN ]   := .F.
    aRep[ RPT_SUMMARY ] := hb_defaultValue( lSummary, .F. )
    aRep[ RPT_COLUMNS ] := {}
    aRep[ RPT_GROUPS ]  := {}
    aRep[ RPT_HEADING ] := ""

RETURN aRep

/* the title lines (at most 4) */
FUNCTION CBR_Title( aRep, aLines )
    aRep[ RPT_HEADER ] := AClone( aLines )
RETURN aRep

/* a group (the first call) or a subgroup (the second); cExpr is compiled at run time */
FUNCTION CBR_Group( aRep, cExpr, cHeader )

    LOCAL aGrp := Array( RGT_AEJECT )

    aGrp[ RGT_TEXT ]   := cExpr
    aGrp[ RGT_EXP ]    := hb_macroBlock( cExpr )
    aGrp[ RGT_HEADER ] := hb_defaultValue( cHeader, "" )
    aGrp[ RGT_AEJECT ] := .F.
    AAdd( aRep[ RPT_GROUPS ], aGrp )

RETURN aRep

/* a column; cHeader may have several lines: "Unit;price" */
FUNCTION CBR_Column( aRep, cExpr, cHeader, nWidth, nDec, lTotal )

    LOCAL aCol := Array( RCT_PICT )

    aCol[ RCT_TEXT ]     := cExpr
    aCol[ RCT_EXP ]      := hb_macroBlock( cExpr )
    aCol[ RCT_HEADER ]   := hb_ATokens( hb_defaultValue( cHeader, "" ), ";" )
    aCol[ RCT_WIDTH ]    := hb_defaultValue( nWidth, 10 )
    aCol[ RCT_DECIMALS ] := hb_defaultValue( nDec, 0 )
    aCol[ RCT_TOTAL ]    := hb_defaultValue( lTotal, .F. )
    AAdd( aRep[ RPT_COLUMNS ], aCol )

RETURN aRep

/* the report of the current work area into cFile: .xlsx = Excel, else Word */
FUNCTION CBR_Make( cFile, aRep, bFor, cTitle, cErr, cCdp )

    LOCAL aRows, lOk := .F., oErr, nRec

    cErr := ""
    IF ! Used()
        cErr := "No table is open"
        RETURN .F.
    ENDIF
    nRec := RecNo()
    BEGIN SEQUENCE WITH {| e | Break( e ) }
        aRows := CBR_Rows( aRep, bFor )
        IF Lower( hb_FNameExt( cFile ) ) == ".xlsx"
            lOk := CBR_Xlsx( cFile, aRep, aRows, hb_defaultValue( cTitle, "" ), @cErr, cCdp )
        ELSE
            lOk := CBR_Docx( cFile, aRep, aRows, hb_defaultValue( cTitle, "" ), @cErr, cCdp )
        ENDIF
    RECOVER USING oErr
        cErr := oErr:description + IIF( Empty( oErr:operation ), "", ": " + oErr:operation )
    END SEQUENCE
    dbGoto( nRec )

RETURN lOk

/*----------------------------------------------------------- .frm writer */

/* writes a report as a Clipper .frm (1990 bytes, as REPORT FORM and RL read it):
   at most 24 columns, a group and a subgroup, 4 title lines */
FUNCTION CBR_Save( cFrm, aRep, cErr )

    LOCAL aExpr := {}, cParams, cFields, cLens := "", cOffs := "", cArea := "", c, n, aCol, aGrp, nOpt := 0
    LOCAL aCols := aRep[ RPT_COLUMNS ], aGrps := hb_defaultValue( aRep[ RPT_GROUPS ], {} )
    LOCAL nTitle, nGrp, nSub, nGrpH, nSubH, cTitle := ""

    cErr := ""
    IF Len( aCols ) > 24
        cErr := "A report form has at most 24 columns"
        RETURN .F.
    ENDIF
    FOR EACH c IN hb_defaultValue( aRep[ RPT_HEADER ], {} )
        IF c:__enumIndex() <= 4
            cTitle += IIF( c:__enumIndex() == 1, "", ";" ) + c
        ENDIF
    NEXT
    nTitle := CBR_ExprAdd( aExpr, cTitle )
    nGrp := nSub := nGrpH := nSubH := 65535
    IF Len( aGrps ) >= 1
        aGrp := aGrps[ 1 ]
        nGrp := CBR_ExprAdd( aExpr, aGrp[ RGT_TEXT ] )
        nGrpH := CBR_ExprAdd( aExpr, aGrp[ RGT_HEADER ] )
    ENDIF
    IF Len( aGrps ) >= 2
        aGrp := aGrps[ 2 ]
        nSub := CBR_ExprAdd( aExpr, aGrp[ RGT_TEXT ] )
        nSubH := CBR_ExprAdd( aExpr, aGrp[ RGT_HEADER ] )
    ENDIF

    /* the columns: 12 bytes each, after an empty first block */
    cFields := Replicate( hb_BChar( 0 ), 12 )
    FOR EACH aCol IN aCols
        c := ""
        FOR EACH n IN hb_defaultValue( aCol[ RCT_HEADER ], {} )
            c += IIF( n:__enumIndex() == 1, "", ";" ) + n
        NEXT
        cFields += I2Bin( aCol[ RCT_WIDTH ] ) + Replicate( hb_BChar( 0 ), 3 ) + ;
                   IIF( hb_defaultValue( aCol[ RCT_TOTAL ], .F. ), "Y", "N" ) + ;
                   I2Bin( hb_defaultValue( aCol[ RCT_DECIMALS ], 0 ) ) + ;
                   I2Bin( CBR_ExprAdd( aExpr, aCol[ RCT_TEXT ] ) ) + ;
                   I2Bin( CBR_ExprAdd( aExpr, c ) )
    NEXT
    cFields := hb_BLeft( cFields + Replicate( hb_BChar( 0 ), 300 ), 300 )

    IF Len( aExpr ) > 55
        cErr := "Too many expressions for a report form (at most 55)"
        RETURN .F.
    ENDIF
    FOR EACH c IN aExpr
        cOffs += I2Bin( hb_BLen( cArea ) )
        cLens += I2Bin( hb_BLen( c ) + 1 )
        cArea += c + hb_BChar( 0 )
    NEXT
    IF hb_BLen( cArea ) > 1440
        cErr := "The expressions of the report are too long (a report form holds 1440 bytes)"
        RETURN .F.
    ENDIF

    IF hb_defaultValue( aRep[ RPT_PLAIN ], .F. )
        nOpt += 4
    ENDIF
    IF hb_defaultValue( aRep[ RPT_AEJECT ], .F. )
        nOpt += 2
    ENDIF
    IF ! hb_defaultValue( aRep[ RPT_BEJECT ], .T. )
        nOpt += 1
    ENDIF
    cParams := I2Bin( nTitle ) + I2Bin( nGrp ) + I2Bin( nSub ) + I2Bin( nGrpH ) + I2Bin( nSubH ) + ;
               I2Bin( hb_defaultValue( aRep[ RPT_WIDTH ], 80 ) ) + I2Bin( hb_defaultValue( aRep[ RPT_LINES ], 58 ) ) + ;
               I2Bin( hb_defaultValue( aRep[ RPT_LMARGIN ], 8 ) ) + I2Bin( hb_defaultValue( aRep[ RPT_RMARGIN ], 0 ) ) + ;
               I2Bin( Len( aCols ) ) + ;
               IIF( hb_defaultValue( aRep[ RPT_SPACING ], 1 ) == 2, "Y", "N" ) + ;
               IIF( hb_defaultValue( aRep[ RPT_SUMMARY ], .F. ), "Y", "N" ) + ;
               IIF( Len( aGrps ) >= 1 .AND. hb_defaultValue( aGrps[ 1 ][ RGT_AEJECT ], .F. ), "Y", "N" ) + ;
               hb_BChar( nOpt )

    c := I2Bin( 2 ) + I2Bin( hb_BLen( cArea ) ) + ;
         hb_BLeft( cLens + Replicate( hb_BChar( 0 ), 110 ), 110 ) + ;
         hb_BLeft( cOffs + Replicate( hb_BChar( 0 ), 110 ), 110 ) + ;
         hb_BLeft( cArea + Replicate( hb_BChar( 0 ), 1440 ), 1440 ) + ;
         cFields + cParams + I2Bin( 2 )
    IF ! hb_MemoWrit( cFrm, c )
        cErr := "Cannot write " + cFrm
        RETURN .F.
    ENDIF

RETURN .T.

/* an expression into the table of the .frm: its number, 65535 = none */
STATIC FUNCTION CBR_ExprAdd( aExpr, c )

    IF ! HB_ISSTRING( c ) .OR. Empty( c )
        RETURN 65535
    ENDIF
    AAdd( aExpr, c )

RETURN Len( aExpr ) - 1
