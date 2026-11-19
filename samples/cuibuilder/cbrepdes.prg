/*
 * CuiBuilder - the report designer: Report Form reports for Word (.docx) and Excel (.xlsx)
 *
 * Copyright (c) 2026 Srđan Dragojlović - dual licensed, see LICENSE
 *
 * A report is a Clipper .frm (title, a group and a subgroup, columns with
 * totals; REPORT FORM, RL and CBro read it too). The designer edits it, tries
 * it on a table (Word / Excel at once) and generates Harbour code from it:
 * rpt_<name>.prg with the function Rpt_<Name>( cFile, bFor, cTitle, @cErr, cCdp ),
 * which writes the report of the current work area into a .docx or an .xlsx.
 * The code needs the shared report module (samples/shared/cbreport.prg) and the
 * DOCX / XLSX classes (github.com/digikv/docx, /xlsx) - they are added to the
 * project with the generated file. (Design file + generated code: the way of
 * the dialog designer of HbIde.)
 *
 *   cb_ReportDesigner( cFrm, hSet, bAdd )   bAdd( aFiles ): adds files to the project
 */

#include "hbtui.ch"
#include "inkey.ch"
#include "cuibuild.ch"

/* a column of the designer */
#define RC_EXPR     1
#define RC_HEAD     2
#define RC_WIDTH    3
#define RC_DEC      4
#define RC_TOTAL    5

/* the report array of Harbour (treport.prg) */
#define RPT_HEADER      1
#define RPT_WIDTH       2
#define RPT_SUMMARY     10
#define RPT_COLUMNS     11
#define RPT_GROUPS      12
#define RCT_TEXT        2
#define RCT_HEADER      4
#define RCT_WIDTH       5
#define RCT_DECIMALS    6
#define RCT_TOTAL       7
#define RGT_TEXT        2
#define RGT_HEADER      4

PROCEDURE cb_ReportDesigner( cFrm, hSet, bAdd )

    LOCAL dlg := CBRepDlg():create( cFrm, hSet, bAdd )

    dlg:exec()

RETURN

CLASS CBRepDlg FROM HTDialog

    DATA cFrm
    DATA hSet
    DATA bAdd
    DATA aCols       INIT {}
    DATA lChanged    INIT .F.
    DATA aTitle      INIT {}         /* 4 line edits */
    DATA oGrp
    DATA oGrpH
    DATA oSub
    DATA oSubH
    DATA oWidth
    DATA oSummary
    DATA oDbf
    DATA oCdp
    DATA oList
    DATA oHead
    DATA oStat

    METHOD create( cFrm, hSet, bAdd )
    METHOD keyEvent( keyEvent )
    METHOD load()
    METHOD report()
    METHOD fillList( nCur )
    METHOD editColumn( nCol )
    METHOD fields()
    METHOD save()
    METHOD test( cExt )
    METHOD generate()
    METHOD openTable( cErr )
    METHOD changed()

ENDCLASS

METHOD create( cFrm, hSet, bAdd ) CLASS CBRepDlg

    LOCAL nW := ht_ScreenMaxCol() + 1, nH := ht_ScreenMaxRow() + 1, nHalf, i, o, nRow

    ::super:new()
    ::cFrm := cFrm
    ::hSet := hSet
    ::bAdd := bAdd
    ::setWindowTitle( "Report: " + cFrm )
    ::resize( nW, nH )
    ::move( 0, 0 )
    nW -= 2
    nHalf := Int( nW / 2 )

    FOR i := 1 TO 4
        nRow := Int( ( i - 1 ) / 2 )
        HTLabel():new( "Title " + hb_ntos( i ), self ):setGeometry( IIF( i % 2 == 1, 1, nHalf + 1 ), nRow, 9, 1 )
        o := CBEdit():new( self )
        o:setGeometry( IIF( i % 2 == 1, 11, nHalf + 11 ), nRow, nHalf - 13, 1 )
        o:onChanged := {|| ::changed() }
        AAdd( ::aTitle, o )
    NEXT
    HTLabel():new( "Group", self ):setGeometry( 1, 2, 9, 1 )
    ::oGrp := CBEdit():new( self )
    ::oGrp:setGeometry( 11, 2, nHalf - 13, 1 )
    HTLabel():new( "Heading", self ):setGeometry( nHalf + 1, 2, 9, 1 )
    ::oGrpH := CBEdit():new( self )
    ::oGrpH:setGeometry( nHalf + 11, 2, nHalf - 32, 1 )
    HTLabel():new( "Width", self ):setGeometry( nW - 19, 2, 6, 1 )
    ::oWidth := CBEdit():new( self )
    ::oWidth:setGeometry( nW - 12, 2, 6, 1 )
    HTLabel():new( "Subgroup", self ):setGeometry( 1, 3, 9, 1 )
    ::oSub := CBEdit():new( self )
    ::oSub:setGeometry( 11, 3, nHalf - 13, 1 )
    HTLabel():new( "Heading", self ):setGeometry( nHalf + 1, 3, 9, 1 )
    ::oSubH := CBEdit():new( self )
    ::oSubH:setGeometry( nHalf + 11, 3, nHalf - 32, 1 )
    ::oSummary := HTCheckBox():new( "Summary", self )
    ::oSummary:setGeometry( nW - 19, 3, 14, 1 )
    ::oSummary:onToggled := {|| ::changed() }
    FOR EACH o IN { ::oGrp, ::oGrpH, ::oSub, ::oSubH, ::oWidth }
        o:onChanged := {|| ::changed() }
    NEXT
    HTLabel():new( "Table", self ):setGeometry( 1, 4, 9, 1 )
    ::oDbf := CBEdit():new( self )
    ::oDbf:setGeometry( 11, 4, nW - 34, 1 )
    ::oDbf:setText( hb_HGetDef( hSet[ "view" ], "reportdbf", "" ) )
    HTLabel():new( "Codepage", self ):setGeometry( nW - 21, 4, 9, 1 )
    ::oCdp := CBEdit():new( self )
    ::oCdp:setGeometry( nW - 12, 4, 10, 1 )
    ::oCdp:setText( hb_HGetDef( hSet[ "view" ], "reportcdp", "SL852" ) )

    HTFrame():new( "Columns  (Ins add, Enter change, Del delete, Ctrl-Up / Ctrl-Down move, F5 fields of the table)", self ):setGeometry( 0, 5, nW, nH - 8 )
    ::oHead := HTLabel():new( "", self )
    ::oHead:setGeometry( 1, 6, nW - 2, 1 )
    ::oList := CBRepList():new( self )
    ::oList:setGeometry( 1, 7, nW - 2, nH - 11 )
    ::oList:oDlg := self
    ::oList:onActivated := {| n | ::editColumn( n ) }
    ::oStat := HTLabel():new( " F2 Save  F6 Try in Word  F7 Try in Excel  F9 Generate code  F5 Fields  Esc Close", self )
    ::oStat:setGeometry( 0, nH - 3, nW, 1 )

    ::load()
    ::setFocusChild( ::oList )

RETURN self

/* the .frm into the fields (a new report: empty) */
METHOD load() CLASS CBRepDlg

    LOCAL aRep, cErr := "", aCol, c, i

    ::oWidth:setText( "80" )
    IF hb_FileExists( ::cFrm )
        aRep := CBR_Load( ::cFrm, @cErr )
        IF aRep == NIL
            HTMessageBox():error( "CuiBuilder", cErr )
        ELSE
            FOR i := 1 TO Min( 4, Len( aRep[ RPT_HEADER ] ) )
                ::aTitle[ i ]:setText( RTrim( aRep[ RPT_HEADER ][ i ] ) )
            NEXT
            IF Len( aRep[ RPT_GROUPS ] ) >= 1
                ::oGrp:setText( aRep[ RPT_GROUPS ][ 1 ][ RGT_TEXT ] )
                ::oGrpH:setText( hb_defaultValue( aRep[ RPT_GROUPS ][ 1 ][ RGT_HEADER ], "" ) )
            ENDIF
            IF Len( aRep[ RPT_GROUPS ] ) >= 2
                ::oSub:setText( aRep[ RPT_GROUPS ][ 2 ][ RGT_TEXT ] )
                ::oSubH:setText( hb_defaultValue( aRep[ RPT_GROUPS ][ 2 ][ RGT_HEADER ], "" ) )
            ENDIF
            ::oWidth:setText( hb_ntos( aRep[ RPT_WIDTH ] ) )
            ::oSummary:setChecked( aRep[ RPT_SUMMARY ] )
            FOR EACH aCol IN aRep[ RPT_COLUMNS ]
                c := ""
                AEval( aCol[ RCT_HEADER ], {| x, n | c += IIF( n == 1, "", ";" ) + RTrim( x ) } )
                AAdd( ::aCols, { aCol[ RCT_TEXT ], c, aCol[ RCT_WIDTH ], aCol[ RCT_DECIMALS ], aCol[ RCT_TOTAL ] } )
            NEXT
        ENDIF
    ELSE
        ::aTitle[ 1 ]:setText( hb_FNameName( ::cFrm ) )
        ::lChanged := .T.
    ENDIF
    ::fillList( 1 )
    IF hb_FileExists( ::cFrm )
        ::lChanged := .F.
    ENDIF

RETURN NIL

/* the report array from the fields (CBR_New / CBR_Column of the shared module) */
METHOD report() CLASS CBRepDlg

    LOCAL aRep := CBR_New( Max( 20, Val( ::oWidth:text ) ), ::oSummary:checked ), aT := {}, o, a

    FOR EACH o IN ::aTitle
        AAdd( aT, AllTrim( o:text ) )
    NEXT
    DO WHILE Len( aT ) > 0 .AND. Empty( ATail( aT ) )
        ASize( aT, Len( aT ) - 1 )
    ENDDO
    CBR_Title( aRep, aT )
    IF ! Empty( ::oGrp:text )
        CBR_Group( aRep, AllTrim( ::oGrp:text ), AllTrim( ::oGrpH:text ) )
        IF ! Empty( ::oSub:text )
            CBR_Group( aRep, AllTrim( ::oSub:text ), AllTrim( ::oSubH:text ) )
        ENDIF
    ENDIF
    FOR EACH a IN ::aCols
        CBR_Column( aRep, a[ RC_EXPR ], a[ RC_HEAD ], a[ RC_WIDTH ], a[ RC_DEC ], a[ RC_TOTAL ] )
    NEXT

RETURN aRep

METHOD changed() CLASS CBRepDlg
    ::lChanged := .T.
RETURN NIL

METHOD fillList( nCur ) CLASS CBRepDlg

    LOCAL a, n := 0, nW := ::oList:width

    ::oList:clear()
    ::oHead:setText( PadR( " #  Expression", Int( nW * 0.4 ) ) + PadR( "Heading", Int( nW * 0.35 ) ) + "Width  Dec  Total" )
    FOR EACH a IN ::aCols
        n++
        ::oList:addItem( PadL( hb_ntos( n ), 2 ) + "  " + PadR( ht_cellLeft( a[ RC_EXPR ], Int( nW * 0.4 ) - 5 ), Int( nW * 0.4 ) - 4 ) + ;
                         PadR( ht_cellLeft( a[ RC_HEAD ], Int( nW * 0.35 ) - 1 ), Int( nW * 0.35 ) ) + ;
                         PadL( hb_ntos( a[ RC_WIDTH ] ), 5 ) + PadL( hb_ntos( a[ RC_DEC ] ), 5 ) + ;
                         IIF( a[ RC_TOTAL ], "    " + ht_glyph( 0x221A, "*" ), "" ), n )
    NEXT
    ::oList:setCurrentIndex( Max( 1, Min( nCur, Len( ::aCols ) ) ) )
    ::repaintChild( ::oHead )
    ::repaintChild( ::oList )

RETURN NIL

/* a column: new (nCol = 0) or changed */
METHOD editColumn( nCol ) CLASS CBRepDlg

    LOCAL dlg, nW := Min( 76, ht_ScreenMaxCol() - 3 ), nH := 10, oExpr, oHead, oWidth, oDec, oTotal, a, cErr := "", x, cAlias

    IF nCol > Len( ::aCols )
        RETURN NIL
    ENDIF
    a := IIF( nCol == 0, { "", "", 10, 0, .F. }, ::aCols[ nCol ] )
    dlg := HTDialog():new()
    dlg:setWindowTitle( IIF( nCol == 0, "New column", "Column " + hb_ntos( nCol ) ) )
    dlg:resize( nW, nH )
    dlg:move( Int( ( ht_ScreenMaxCol() + 1 - nW ) / 2 ), Int( ( ht_ScreenMaxRow() + 1 - nH ) / 2 ) )
    HTLabel():new( "Expression", dlg ):setGeometry( 1, 1, 11, 1 )
    oExpr := CBEdit():new( dlg )
    oExpr:setGeometry( 13, 1, nW - 16, 1 )
    oExpr:setText( a[ RC_EXPR ] )
    HTLabel():new( "Heading", dlg ):setGeometry( 1, 2, 11, 1 )
    oHead := CBEdit():new( dlg )
    oHead:setGeometry( 13, 2, nW - 16, 1 )
    oHead:setText( a[ RC_HEAD ] )
    HTLabel():new( "Width", dlg ):setGeometry( 1, 3, 11, 1 )
    oWidth := CBEdit():new( dlg )
    oWidth:setGeometry( 13, 3, 6, 1 )
    oWidth:setText( hb_ntos( a[ RC_WIDTH ] ) )
    HTLabel():new( "Decimals", dlg ):setGeometry( 22, 3, 9, 1 )
    oDec := CBEdit():new( dlg )
    oDec:setGeometry( 32, 3, 4, 1 )
    oDec:setText( hb_ntos( a[ RC_DEC ] ) )
    oTotal := HTCheckBox():new( "Total", dlg )
    oTotal:setGeometry( 40, 3, 12, 1 )
    oTotal:setChecked( a[ RC_TOTAL ] )
    HTLabel():new( "Heading: ; starts a new line. Empty width: from the table.", dlg ):setGeometry( 1, 5, nW - 4, 1 )
    dlg:addButtonBar( "OK", "Cancel" )
    dlg:setFocusChild( oExpr )
    IF dlg:exec() != HT_DIALOG_ACCEPTED .OR. Empty( oExpr:text )
        RETURN NIL
    ENDIF
    a := { AllTrim( oExpr:text ), AllTrim( oHead:text ), Val( oWidth:text ), Val( oDec:text ), oTotal:checked }

    /* the expression tried on the table, when there is one */
    IF ( cAlias := ::openTable( @cErr ) ) != NIL
        BEGIN SEQUENCE WITH {| e | Break( e ) }
            x := Eval( hb_macroBlock( a[ RC_EXPR ] ) )
            IF a[ RC_WIDTH ] <= 0
                a[ RC_WIDTH ] := Max( Len( a[ RC_HEAD ] ), IIF( HB_ISNUMERIC( x ), Len( Str( x ) ), Len( hb_CStr( x ) ) ) )
            ENDIF
            IF HB_ISNUMERIC( x ) .AND. a[ RC_DEC ] == 0 .AND. "." $ Str( x )
                a[ RC_DEC ] := Len( Str( x ) ) - At( ".", Str( x ) )
            ENDIF
        RECOVER USING x
            cErr := "The expression " + a[ RC_EXPR ] + ": " + x:description + IIF( Empty( x:operation ), "", " " + x:operation )
        END SEQUENCE
        ( cAlias )->( dbCloseArea() )
        IF ! Empty( cErr )
            HTMessageBox():warning( "CuiBuilder", cErr )
        ENDIF
    ENDIF
    a[ RC_WIDTH ] := Max( 1, a[ RC_WIDTH ] )
    IF nCol == 0
        AAdd( ::aCols, a )
        nCol := Len( ::aCols )
    ELSE
        ::aCols[ nCol ] := a
    ENDIF
    ::lChanged := .T.
    ::fillList( nCol )

RETURN NIL

/* F5: columns from the fields of the table (Space marks several) */
METHOD fields() CLASS CBRepDlg

    LOCAL cAlias, cErr := "", aStru, dlg, oL, nW, nH, a, n, aPick := {}

    IF ( cAlias := ::openTable( @cErr ) ) == NIL
        HTMessageBox():warning( "CuiBuilder", IIF( Empty( cErr ), "Write the table (.dbf) to try the report on first", cErr ) )
        RETURN NIL
    ENDIF
    aStru := ( cAlias )->( dbStruct() )
    ( cAlias )->( dbCloseArea() )
    nW := 46
    nH := Min( Len( aStru ) + 4, ht_ScreenMaxRow() - 3 )
    dlg := HTDialog():new()
    dlg:setWindowTitle( "Fields (Space marks, Enter adds)" )
    dlg:resize( nW, nH )
    dlg:move( Int( ( ht_ScreenMaxCol() + 1 - nW ) / 2 ), Int( ( ht_ScreenMaxRow() + 1 - nH ) / 2 ) )
    oL := CBFileDlgList():new( dlg )
    oL:setGeometry( 1, 1, nW - 4, nH - 3 )
    oL:lMulti := .T.
    FOR EACH a IN aStru
        oL:addItem( "  " + PadR( a[ 1 ], 12 ) + a[ 2 ] + Str( a[ 3 ], 5 ) + IIF( a[ 4 ] > 0, "," + hb_ntos( a[ 4 ] ), "" ), { a[ 1 ], "F" } )
    NEXT
    oL:onActivated := {| i | aPick := IIF( Len( oL:aMarked ) > 0, oL:aMarked, { i } ), dlg:accept() }
    dlg:setFocusChild( oL )
    IF dlg:exec() != HT_DIALOG_ACCEPTED
        RETURN NIL
    ENDIF
    ASort( aPick )
    FOR EACH n IN aPick
        a := aStru[ n ]
        AAdd( ::aCols, { a[ 1 ], hb_asciiUpper( Left( a[ 1 ], 1 ) ) + Lower( SubStr( a[ 1 ], 2 ) ), ;
                         IIF( a[ 2 ] == "D", 10, IIF( a[ 2 ] == "L", 3, IIF( a[ 2 ] == "M", 30, a[ 3 ] ) ) ), ;
                         IIF( a[ 2 ] == "N", a[ 4 ], 0 ), .F. } )
    NEXT
    ::lChanged := .T.
    ::fillList( Len( ::aCols ) )

RETURN NIL

/* the table to try the report on, opened read only; its alias, NIL = none */
METHOD openTable( cErr ) CLASS CBRepDlg

    LOCAL cFile := AllTrim( ::oDbf:text ), cAlias := "CBRTEST", cCdp := AllTrim( ::oCdp:text ), oErr, lOk := .F.

    IF Empty( cFile )
        RETURN NIL
    ENDIF
    cFile := cb_Abs( cFile, hb_FNameDir( ::cFrm ) )
    IF Empty( hb_FNameExt( cFile ) )
        cFile += ".dbf"
    ENDIF
    IF ! hb_FileExists( cFile )
        cErr := "Not found: " + cFile
        RETURN NIL
    ENDIF
    IF Select( cAlias ) > 0
        ( cAlias )->( dbCloseArea() )
    ENDIF
    BEGIN SEQUENCE WITH {| e | Break( e ) }
        IF Empty( cCdp )
            dbUseArea( .T.,, cFile, cAlias, .T., .T. )
        ELSE
            dbUseArea( .T.,, cFile, cAlias, .T., .T., cCdp )
        ENDIF
        lOk := Used()
    RECOVER USING oErr
        cErr := "Cannot open " + cFile + ": " + oErr:description
    END SEQUENCE
    IF ! lOk
        RETURN NIL
    ENDIF
    ::hSet[ "view" ][ "reportdbf" ] := AllTrim( ::oDbf:text )
    ::hSet[ "view" ][ "reportcdp" ] := cCdp

RETURN cAlias

METHOD save() CLASS CBRepDlg

    LOCAL cErr := ""

    IF Len( ::aCols ) == 0
        HTMessageBox():warning( "CuiBuilder", "The report has no columns yet (Ins adds one, F5 from the fields)" )
        RETURN .F.
    ENDIF
    IF ! CBR_Save( ::cFrm, ::report(), @cErr )
        HTMessageBox():error( "CuiBuilder", cErr )
        RETURN .F.
    ENDIF
    ::lChanged := .F.
    HTToast():show( "Saved: " + hb_FNameNameExt( ::cFrm ), 1500, HT_CLR_TOAST_SUCCESS )

RETURN .T.

/* F6 / F7: the report of the table in Word / Excel, opened at once */
METHOD test( cExt ) CLASS CBRepDlg

    LOCAL cAlias, cErr := "", cOut, lOk

    IF Len( ::aCols ) == 0
        HTMessageBox():warning( "CuiBuilder", "The report has no columns yet" )
        RETURN NIL
    ENDIF
    IF ( cAlias := ::openTable( @cErr ) ) == NIL
        HTMessageBox():warning( "CuiBuilder", IIF( Empty( cErr ), "Write the table (.dbf) to try the report on first", cErr ) )
        RETURN NIL
    ENDIF
    cOut := hb_FNameExtSet( ::cFrm, "" ) + "_try" + cExt
    lOk := ( cAlias )->( CBR_Make( cOut, ::report(), NIL, hb_FNameName( ::cFrm ), @cErr, AllTrim( ::oCdp:text ) ) )
    ( cAlias )->( dbCloseArea() )
    IF ! lOk
        HTMessageBox():error( "CuiBuilder", cErr )
        RETURN NIL
    ENDIF
    IF ! cb_OpenDocument( cOut )
        HTToast():show( "Made: " + cOut, 3000, HT_CLR_TOAST_SUCCESS )
    ENDIF

RETURN NIL

/* F9: rpt_<name>.prg next to the .frm, added to the project with what it needs */
METHOD generate() CLASS CBRepDlg

    LOCAL cName := hb_FNameName( ::cFrm ), cFunc, cPrg, c := "", a, cEol := hb_eol(), aRep, aT := {}, o, aAdd, hR := ::hSet[ "reports" ]

    IF ! ::save()
        RETURN NIL
    ENDIF
    aRep := ::report()
    cFunc := "Rpt_" + cb_Ident( cName )
    cPrg := hb_FNameDir( ::cFrm ) + "rpt_" + Lower( cb_Ident( cName ) ) + ".prg"
    FOR EACH o IN ::aTitle
        IF ! Empty( o:text )
            AAdd( aT, AllTrim( o:text ) )
        ENDIF
    NEXT

    c += "/*" + cEol
    c += " * " + hb_FNameNameExt( cPrg ) + " - the report " + hb_FNameNameExt( ::cFrm ) + ", generated by CuiBuilder." + cEol
    c += " * Generated again from the report designer (F9): changes made here are lost." + cEol
    c += " *" + cEol
    c += " *   " + cFunc + "( cFile, [bFor], [cTitle], [@cErr], [cCdp] ) -> lOk" + cEol
    c += " *" + cEol
    c += " * writes the report of the current work area into cFile: .docx (Word) or" + cEol
    c += " * .xlsx (Excel). bFor: the records to take; cCdp: the codepage of text that" + cEol
    c += " * is not UTF-8 (the table). Needs cbreport.prg (Harbour-CUI samples/shared)," + cEol
    c += " * DOCXClass.prg, XLSXClass.prg and hbziparc.hbc." + cEol
    c += " */" + cEol + cEol
    c += "FUNCTION " + cFunc + "( cFile, bFor, cTitle, cErr, cCdp )" + cEol + cEol
    c += "    LOCAL aRep := CBR_New( " + hb_ntos( aRep[ RPT_WIDTH ] ) + ", " + IIF( aRep[ RPT_SUMMARY ], ".T.", ".F." ) + " )" + cEol + cEol
    IF Len( aT ) > 0
        c += "    CBR_Title( aRep, { "
        AEval( aT, {| x, n | c += IIF( n == 1, "", ", " ) + hb_ValToExp( x ) } )
        c += " } )" + cEol
    ENDIF
    FOR EACH a IN aRep[ RPT_GROUPS ]
        c += "    CBR_Group( aRep, " + hb_ValToExp( a[ RGT_TEXT ] ) + ", " + hb_ValToExp( a[ RGT_HEADER ] ) + " )" + cEol
    NEXT
    FOR EACH a IN ::aCols
        c += "    CBR_Column( aRep, " + hb_ValToExp( a[ RC_EXPR ] ) + ", " + hb_ValToExp( a[ RC_HEAD ] ) + ", " + ;
             hb_ntos( a[ RC_WIDTH ] ) + ", " + hb_ntos( a[ RC_DEC ] ) + ", " + IIF( a[ RC_TOTAL ], ".T.", ".F." ) + " )" + cEol
    NEXT
    c += cEol + "RETURN CBR_Make( cFile, aRep, bFor, hb_defaultValue( cTitle, " + hb_ValToExp( IIF( Len( aT ) > 0, aT[ 1 ], cName ) ) + ;
         " ), @cErr, cCdp )" + cEol
    IF ! hb_MemoWrit( cPrg, c )
        HTMessageBox():error( "CuiBuilder", "Cannot write " + cPrg )
        RETURN NIL
    ENDIF

    aAdd := { cPrg }
    FOR EACH c IN { "cbreport", "docx", "xlsx" }
        IF Empty( hR[ c ] ) .OR. ! hb_FileExists( hR[ c ] )
            HTMessageBox():warning( "CuiBuilder", "Not found: " + IIF( c == "cbreport", "cbreport.prg (Harbour-CUI samples/shared)", ;
                IIF( c == "docx", "DOCXClass.prg (github.com/digikv/docx)", "XLSXClass.prg (github.com/digikv/xlsx)" ) ) + Chr( 10 ) + ;
                "Set it in cuibuild.ini [REPORTS] " + c + "=, then add it to the project." )
        ELSE
            AAdd( aAdd, hR[ c ] )
        ENDIF
    NEXT
    AAdd( aAdd, hR[ "zip_hbc" ] )
    IF ::bAdd != NIL
        Eval( ::bAdd, aAdd )
    ENDIF
    HTMessageBox():information( "CuiBuilder", "Generated: " + cPrg + Chr( 10 ) + Chr( 10 ) + ;
        "In the program:" + Chr( 10 ) + "   USE " + Upper( cName ) + Chr( 10 ) + ;
        "   IF ! " + cFunc + '( "' + cName + '.docx",,, @cErr )' + Chr( 10 ) + "      Alert( cErr )" + Chr( 10 ) + "   ENDIF" )

RETURN NIL

METHOD PROCEDURE keyEvent( keyEvent ) CLASS CBRepDlg

    LOCAL nKey := keyEvent:key, n

    SWITCH nKey
    CASE K_ESC
        keyEvent:accept()
        IF ::lChanged
            n := HTMessageBox():choose( "CuiBuilder", hb_FNameNameExt( ::cFrm ) + " has been changed. Save it?", ;
                                        { "&Save", "&Don't save", "&Cancel" }, HT_MSG_QUESTION )
            IF n == 3 .OR. n == 0 .OR. ( n == 1 .AND. ! ::save() )
                RETURN
            ENDIF
        ENDIF
        ::accept()
        RETURN
    CASE K_F2 ; keyEvent:accept() ; ::save() ; RETURN
    CASE K_F5 ; keyEvent:accept() ; ::fields() ; RETURN
    CASE K_F6 ; keyEvent:accept() ; ::test( ".docx" ) ; RETURN
    CASE K_F7 ; keyEvent:accept() ; ::test( ".xlsx" ) ; RETURN
    CASE K_F9 ; keyEvent:accept() ; ::generate() ; RETURN
    ENDSWITCH
    ::super:keyEvent( keyEvent )

RETURN

/* the columns: Ins, Del, Ctrl-Up / Ctrl-Down */
CLASS CBRepList FROM CBList
    DATA oDlg
    METHOD keyEvent( keyEvent )
ENDCLASS

METHOD PROCEDURE keyEvent( keyEvent ) CLASS CBRepList

    LOCAL d := ::oDlg, n := ::currentIndex(), a

    SWITCH keyEvent:key
    CASE K_INS
        keyEvent:accept()
        d:editColumn( 0 )
        RETURN
    CASE K_DEL
        keyEvent:accept()
        IF n >= 1 .AND. n <= Len( d:aCols )
            hb_ADel( d:aCols, n, .T. )
            d:lChanged := .T.
            d:fillList( n )
        ENDIF
        RETURN
    CASE K_CTRL_UP
    CASE K_CTRL_DOWN
        keyEvent:accept()
        a := n + IIF( keyEvent:key == K_CTRL_UP, -1, 1 )
        IF n >= 1 .AND. a >= 1 .AND. a <= Len( d:aCols )
            cb_ASwap( d:aCols, n, a )
            d:lChanged := .T.
            d:fillList( a )
        ENDIF
        RETURN
    CASE K_ENTER
        keyEvent:accept()
        IF n >= 1
            d:editColumn( n )
        ELSE
            d:editColumn( 0 )
        ENDIF
        RETURN
    ENDSWITCH
    ::super:keyEvent( keyEvent )

RETURN

STATIC FUNCTION cb_ASwap( a, n1, n2 )

    LOCAL x := a[ n1 ]

    a[ n1 ] := a[ n2 ]
    a[ n2 ] := x

RETURN a

/* a name usable in a function name: letters, digits, _ */
FUNCTION cb_Ident( c )

    LOCAL cOut := "", ch

    FOR EACH ch IN c
        IF IsAlpha( ch ) .OR. IsDigit( ch ) .OR. ch == "_"
            cOut += IIF( Len( cOut ) == 0, Upper( ch ), Lower( ch ) )
        ELSE
            cOut += "_"
        ENDIF
    NEXT

RETURN IIF( Empty( cOut ) .OR. IsDigit( Left( cOut, 1 ) ), "R" + cOut, cOut )

/* a document in its program (Word, Excel, LibreOffice); .F. = could not */
FUNCTION cb_OpenDocument( cFile )
#ifdef __PLATFORM__WINDOWS
RETURN CB_OPENDOC( cFile )
#else
RETURN hb_processOpen( "xdg-open " + cb_Quote( cFile ),,,, .T. ) != -1
#endif
