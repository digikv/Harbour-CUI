/** @file cbxlsx.prg
 * CBro - .xlsx through Srđan Dragojlović's XLSX class (../xlsx/XLSXClass.prg next to the repository,
 * https://github.com/digikv/xlsx): this file only adapts it to CBro.
 *
 *   CBX_Open( cFile, @cErr )                -> oWorkBook (read) or NIL
 *   CBX_SheetRows( oBook, nSheet )          -> { aRow1, aRow2, ... } (C/N/D/L values)
 *   CBX_ToStruct( aRows, lHeader )          -> { aStruct, nFirstDataRow }
 *        DBF structure that holds the sheet (types and sizes from the data)
 *   CBX_Write( cFile, cSheet, aCols, bRow, @cErr, cCdp ) -> lOk
 *        aCols = dbStruct() style { { cHeading, cType, nLen, nDec }, ... },
 *        bRow  = {|| aValues or NIL at the end }, cCdp = codepage of the text
 *        that is not UTF-8 (a DBF opened without its codepage)
 *
 * Copyright (c) 2026 Srđan Dragojlović
 * Dual licensed: GNU GPL version 3 or later (COPYING), or a commercial
 * license from the author - see LICENSE.
 */

/* reads a workbook */
FUNCTION CBX_Open( cFile, cErr )

    LOCAL oBook := WorkBook():New()

    IF ! oBook:Read( cFile )
        cErr := oBook:cError
        RETURN NIL
    ENDIF
    IF Len( oBook:SheetNames() ) == 0
        cErr := hb_FNameNameExt( cFile ) + ": the workbook has no worksheets"
        RETURN NIL
    ENDIF

RETURN oBook

/* the values of sheet nSheet, row by row; a formula gives its last value */
FUNCTION CBX_SheetRows( oBook, nSheet )

    LOCAL oSheet := oBook:GetSheet( nSheet ), aRows, aRow, x

    IF oSheet == NIL
        RETURN {}
    ENDIF
    aRows := oSheet:ToArray()
    FOR EACH aRow IN aRows
        IF ! HB_ISARRAY( aRow )
            aRow := {}
            LOOP
        ENDIF
        FOR EACH x IN aRow
            DO CASE
            CASE HB_ISARRAY( x )                  /* formula without a value */
                x := NIL
            CASE HB_ISDATETIME( x ) .AND. ! HB_ISDATE( x )
                x := hb_TToD( x )
            CASE HB_ISSTRING( x ) .AND. x == Chr( 0 )      /* merged cell */
                x := NIL
            ENDCASE
        NEXT
    NEXT
    DO WHILE Len( aRows ) > 0 .AND. Empty( ATail( aRows ) )
        ASize( aRows, Len( aRows ) - 1 )
    ENDDO

RETURN aRows

/* writes one worksheet: heading row (bold, shaded), then the rows bRow
   gives until it returns NIL; cErr gets the error or the row count */
FUNCTION CBX_Write( cFile, cSheet, aCols, bRow, cErr, cCdp )

    LOCAL oBook, oSheet, aRow, nRow := 1, nCol, x, aStyle := {}, aCol, nHead, nDate, nWidth

    oBook := WorkBook():New( cFile )
    oBook:Application := "CBro"
    IF HB_ISSTRING( cCdp ) .AND. ! Empty( cCdp )
        oBook:cCodePage := cCdp
    ENDIF
    oSheet := oBook:WorkSheet( IIF( Empty( cSheet ), "Sheet1", cSheet ) )

    nHead := oBook:NewStyle( oBook:NewFont( , 11, .T. ), oBook:NewBorder( 0, 0, 0, 1 ), ;
                             oBook:NewFillPattern( 1, "FFD9E1F2", "FFD9E1F2" ) )
    nDate := oBook:NewStyle( , , , , , oBook:NewFormat( DateCode() ) )
    FOR EACH aCol IN aCols
        DO CASE
        CASE aCol[ 2 ] $ "DT@"
            AAdd( aStyle, nDate )
            nWidth := 11
        CASE aCol[ 2 ] $ "NIBYF" .AND. aCol[ 4 ] > 0
            AAdd( aStyle, oBook:NewStyle( , , , , , oBook:NewFormat( "#,##0." + Replicate( "0", Min( aCol[ 4 ], 9 ) ) ) ) )
            nWidth := Min( Max( aCol[ 3 ], 6 ) + 2, 30 )
        CASE aCol[ 2 ] == "L"
            AAdd( aStyle, NIL )
            nWidth := 7
        CASE aCol[ 2 ] == "M"
            AAdd( aStyle, NIL )
            nWidth := 40
        OTHERWISE
            AAdd( aStyle, NIL )
            nWidth := Min( Max( aCol[ 3 ], 4 ) + 1, 60 )
        ENDCASE
        nCol := aCol:__enumIndex()
        oSheet:ColumnsWidth( nCol, nCol, Max( nWidth, Min( Len( aCol[ 1 ] ) + 2, 30 ) ) )
        oSheet:Cell( { 1, nCol }, aCol[ 1 ], nHead )
    NEXT

    DO WHILE ( aRow := Eval( bRow ) ) != NIL
        nRow++
        FOR nCol := 1 TO Min( Len( aRow ), Len( aCols ) )
            x := aRow[ nCol ]
            DO CASE
            CASE HB_ISSTRING( x )
                x := RTrim( x )
                IF ! x == ""
                    oSheet:Cell( { nRow, nCol }, x )
                ENDIF
            CASE HB_ISDATE( x ) .OR. HB_ISDATETIME( x )
                IF ! Empty( x )
                    oSheet:Cell( { nRow, nCol }, x, aStyle[ nCol ] )
                ENDIF
            CASE HB_ISNUMERIC( x ) .OR. HB_ISLOGICAL( x )
                oSheet:Cell( { nRow, nCol }, x, aStyle[ nCol ] )
            ENDCASE
        NEXT
    ENDDO

    IF ! oBook:Save()
        cErr := oBook:cError
        RETURN .F.
    ENDIF
    cErr := hb_ntos( nRow - 1 )

RETURN .T.

/* Excel date format of SET DATE FORMAT ("dd.mm.yyyy", "mm/dd/yyyy", ...) */
STATIC FUNCTION DateCode()

    LOCAL cFmt := Lower( Set( _SET_DATEFORMAT ) )

    IF ! "yy" $ cFmt .OR. ! "mm" $ cFmt .OR. ! "dd" $ cFmt
        cFmt := "dd.mm.yyyy"
    ENDIF

RETURN cFmt

/* 28 -> "AB" */
FUNCTION CBX_ColName( n )

    LOCAL c := ""

    DO WHILE n > 0
        c := Chr( 65 + ( n - 1 ) % 26 ) + c
        n := Int( ( n - 1 ) / 26 )
    ENDDO

RETURN c

/*---------------------------------------------------------------- sheet -> DBF structure */

/* { aStruct, nFirstDataRow, aNames } for the rows of a sheet: field names
   from the first row (lHeader) or COL_A, COL_B, ...; types from the values:
   all numbers N (length and decimals from the data), all dates D, all
   logical L, else C (longer than 254 -> M) */
FUNCTION CBX_ToStruct( aRows, lHeader )

    LOCAL nCols := 0, aRow, aStruct := {}, nCol, nFirst := IIF( lHeader, 2, 1 )
    LOCAL cName, aUsed := {}, cType, nLen, nDec, nInt, x, n, cNum, aTypes

    AEval( aRows, {| a | nCols := Max( nCols, Len( a ) ) } )
    FOR nCol := 1 TO nCols
        aTypes := ""
        nLen := 1
        nDec := 0
        nInt := 1
        FOR n := nFirst TO Len( aRows )
            aRow := aRows[ n ]
            IF nCol > Len( aRow ) .OR. ( x := aRow[ nCol ] ) == NIL .OR. ( HB_ISSTRING( x ) .AND. Empty( x ) )
                LOOP
            ENDIF
            cType := ValType( x )
            IF ! cType $ aTypes
                aTypes += cType
            ENDIF
            SWITCH cType
            CASE "C"
                nLen := Max( nLen, hb_BLen( x ) )
                EXIT
            CASE "N"
                cNum := LTrim( Str( x, 30, 6 ) )
                DO WHILE Right( cNum, 1 ) == "0" .AND. "." $ cNum
                    cNum := Left( cNum, Len( cNum ) - 1 )
                ENDDO
                IF Right( cNum, 1 ) == "."
                    cNum := Left( cNum, Len( cNum ) - 1 )
                ENDIF
                IF "." $ cNum
                    nDec := Max( nDec, Len( cNum ) - At( ".", cNum ) )
                    nInt := Max( nInt, At( ".", cNum ) - 1 )
                ELSE
                    nInt := Max( nInt, Len( cNum ) )
                ENDIF
                nLen := Max( nLen, Len( cNum ) )
                EXIT
            ENDSWITCH
        NEXT

        DO CASE
        CASE aTypes == "N"
            nDec := Min( nDec, 6 )
            aTypes := { "N", Min( nInt + IIF( nDec > 0, nDec + 1, 0 ), 19 ), nDec }
        CASE aTypes == "D"
            aTypes := { "D", 8, 0 }
        CASE aTypes == "L"
            aTypes := { "L", 1, 0 }
        CASE aTypes == ""
            aTypes := { "C", 10, 0 }
        OTHERWISE        /* text, or mixed: shown as text */
            FOR n := nFirst TO Len( aRows )
                aRow := aRows[ n ]
                IF nCol <= Len( aRow ) .AND. aRow[ nCol ] != NIL
                    nLen := Max( nLen, hb_BLen( CBX_AsText( aRow[ nCol ] ) ) )
                ENDIF
            NEXT
            aTypes := IIF( nLen > 254, { "M", 10, 0 }, { "C", nLen, 0 } )
        ENDCASE

        cName := ""
        IF lHeader .AND. Len( aRows ) > 0 .AND. nCol <= Len( aRows[ 1 ] ) .AND. aRows[ 1 ][ nCol ] != NIL
            cName := FieldNameOf( CBX_AsText( aRows[ 1 ][ nCol ] ) )
        ENDIF
        IF Empty( cName )
            cName := "COL_" + CBX_ColName( nCol )
        ENDIF
        cName := Left( cName, 10 )
        n := 1
        DO WHILE AScan( aUsed, cName ) > 0
            cName := Left( cName, 10 - Len( hb_ntos( ++n ) ) - 1 ) + "_" + hb_ntos( n )
        ENDDO
        AAdd( aUsed, cName )
        AAdd( aStruct, { cName, aTypes[ 1 ], aTypes[ 2 ], aTypes[ 3 ] } )
    NEXT

RETURN { aStruct, nFirst }

/* a heading as a DBF field name: letters, digits, "_" (ASCII), upper case */
STATIC FUNCTION FieldNameOf( cText )

    LOCAL cOut := "", c, n, nCode
    LOCAL hLatin := { 0x160 => "S", 0x161 => "S", 0x110 => "DJ", 0x111 => "DJ", 0x10C => "C", 0x10D => "C", ;
                      0x106 => "C", 0x107 => "C", 0x17D => "Z", 0x17E => "Z" }

    FOR n := 1 TO Len( cText )
        c := SubStr( cText, n, 1 )
        nCode := hb_UTF8Asc( c )
        IF hb_HHasKey( hLatin, nCode )
            c := hLatin[ nCode ]
            nCode := Asc( c )
        ELSE
            c := Upper( c )
            nCode := hb_UTF8Asc( c )
        ENDIF
        IF ( nCode >= 65 .AND. nCode <= 90 ) .OR. ( nCode >= 48 .AND. nCode <= 57 )
            cOut += c
        ELSEIF ! Right( cOut, 1 ) == "_" .AND. ! cOut == ""
            cOut += "_"
        ENDIF
    NEXT
    DO WHILE Right( cOut, 1 ) == "_"
        cOut := Left( cOut, Len( cOut ) - 1 )
    ENDDO
    IF ! cOut == "" .AND. IsDigit( cOut )
        cOut := "F" + cOut
    ENDIF

RETURN cOut

/* a cell value as text (mixed columns) */
FUNCTION CBX_AsText( x )

    SWITCH ValType( x )
    CASE "C"
        RETURN x
    CASE "N"
        RETURN hb_ntos( x )
    CASE "D"
        RETURN DToC( x )
    CASE "L"
        RETURN IIF( x, "T", "F" )
    ENDSWITCH

RETURN ""

