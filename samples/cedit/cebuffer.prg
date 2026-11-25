/*
 * CEBuffer - one open file of CEdit: lines, cursor, selection, undo / redo
 *
 * Copyright (c) 2026 Srđan Dragojlović - dual licensed, see LICENSE
 *
 * Small files stay in aLines (UTF-8). A file bigger than CE_MEM_LIMIT uses a
 * disk line index, an LRU page cache and a piece table (original + .add swap)
 * so it can be edited without holding the whole text in RAM.
 *
 * Positions are 1-based: nRow = line, nCol = character in the line
 * (Len() + 1 = line end). Undo: every change is a record; one user action
 * shares a group number.
 */

#include "hbclass.ch"
#include "fileio.ch"
#include "cedit.ch"

#define LF  Chr( 10 )
#define CR  Chr( 13 )

CLASS CEBuffer

EXPORTED:

    DATA aLines
    DATA lPaged     INIT .F.
    DATA nHandle    INIT -1            /* original file */
    DATA nAddHandle INIT -1            /* append-only .add swap */
    DATA nIdxHandle INIT -1            /* line-start index on disk */
    DATA cIdxPath   INIT ""
    DATA cAddPath   INIT ""
    DATA cIdxBuf    INIT ""            /* pending disk writes; length nIdxBufLen */
    DATA nIdxBufLen INIT 0
    DATA aIdx       INIT {}            /* line-start offsets in RAM (source of truth for Get) */
    DATA nIdxEntries INIT 0            /* Len(aIdx); also mirrored on .idx disk */
    DATA aPieces                       /* { { nSrc, nOff, nLen }, ... } */
    DATA nDocLen    INIT 0             /* logical document length in bytes */
    DATA aCache                        /* LRU pages: { nSrc, nPage, cData } */
    DATA nBreaks    INIT 0
    DATA nIdxPos    INIT 0             /* logical offset indexed so far */
    DATA cIdxCarry  INIT ""
    DATA lIndexed   INIT .T.
    DATA lAfterBreak INIT .F.
    DATA nDirtyFrom INIT 0             /* 0 = index matches doc; else reindex from line */
    DATA lHistMute  INIT .F.           /* .T.: insert/delete do not push undo */
    DATA nLF        INIT 0
    DATA nCR        INIT 0
    DATA nCRLF      INIT 0
    DATA hCache                            /* recent lines (UTF-8) */
    DATA cFile      INIT ""
    DATA cName      INIT ""
    DATA cCP        INIT "UTF8"
    DATA lBOM       INIT .F.
    DATA cEol       INIT Chr( 13 ) + Chr( 10 )
    DATA lFinalEol  INIT .T.
    DATA lReadOnly  INIT .F.
    DATA cError     INIT ""

    DATA nRow       INIT 1
    DATA nCol       INIT 1
    DATA nTop       INIT 1
    DATA nLeft      INIT 0
    DATA nWantCell  INIT -1
    DATA nTab       INIT 4

    DATA nSelMode   INIT CE_SEL_NONE
    DATA nSelRow    INIT 0
    DATA nSelCol    INIT 0
    DATA lMarking   INIT .F.

    DATA aUndo
    DATA aRedo
    DATA nGroup     INIT 0
    DATA nGroupSeq  INIT 0
    DATA nSavedGroup INIT 0
    DATA cLastKind  INIT ""
    DATA nLastRow   INIT 0
    DATA nLastCol   INIT 0

    DATA cSyntax    INIT ""
    DATA lHilite    INIT .T.
    DATA aStates
    DATA nStateValid INIT 1
    DATA aBack

    METHOD new()
    METHOD load( cFile, cDefCP, lForce )
    METHOD loadPaged( cFile, cDefCP, lForce )
    METHOD indexStep( nBytes )
    METHOD indexAll( bProgress )
    METHOD follow()
    METHOD closeFile()
    METHOD find( cWhat, nRow, nCol, lCase, lWord, lBack, lWrap )
    METHOD invalidate( nRow ) INLINE ::nStateValid := Min( ::nStateValid, Max( 1, nRow ) )
    METHOD pushBack()
    METHOD save( cFile )
    METHOD setText( cText )
    METHOD getText()
    METHOD title()
    METHOD isModified()
    METHOD lineCount()
    METHOD line( n )

    METHOD startAction( cKind )
    METHOD endAction( cKind )
    METHOD insertText( nRow, nCol, cText )
    METHOD deleteRange( nR1, nC1, nR2, nC2 )
    METHOD undo()
    METHOD redo()

    METHOD hasSel()
    METHOD clearSel()
    METHOD streamRange()
    METHOD selText()

ENDCLASS

METHOD new() CLASS CEBuffer
    ::aLines := { "" }
    ::aPieces := {}
    ::aCache := {}
    ::hCache := { => }
    ::aUndo := {}
    ::aRedo := {}
    ::aStates := { 0 }
    ::aBack := {}
RETURN self

METHOD lineCount() CLASS CEBuffer
    IF ::lPaged
        /* while indexing, always count the open line after the last break */
        IF ::lIndexed
            RETURN Max( 1, ::nBreaks + IIF( ::lAfterBreak, 0, 1 ) )
        ENDIF
        RETURN Max( 1, ::nBreaks + 1 )
    ENDIF
RETURN Len( ::aLines )

METHOD line( n ) CLASS CEBuffer
    IF ! ::lPaged
        IF n >= 1 .AND. n <= Len( ::aLines )
            RETURN ::aLines[ n ]
        ENDIF
        RETURN ""
    ENDIF
RETURN ce_PagedLine( Self, n )

METHOD pushBack() CLASS CEBuffer
    AAdd( ::aBack, { ::nRow, ::nCol } )
    IF Len( ::aBack ) > 50
        hb_ADel( ::aBack, 1, .T. )
    ENDIF
RETURN NIL

METHOD load( cFile, cDefCP, lForce ) CLASS CEBuffer

    LOCAL cRaw

    ::cError := ""
    ::cFile := cFile
    ::cCP := "UTF8"
    ::lBOM := .F.
    ::cEol := hb_eol()
    ::lFinalEol := .T.
    ::closeFile()

    IF ! hb_FileExists( cFile )
        ::setText( "" )
        RETURN .T.
    ENDIF

    IF hb_FSize( cFile ) > CE_MEM_LIMIT
        RETURN ::loadPaged( cFile, cDefCP, lForce )
    ENDIF

    cRaw := hb_MemoRead( cFile )
    IF cRaw == "" .AND. hb_FSize( cFile ) > 0
        ::cError := "Cannot read " + cFile
        RETURN .F.
    ENDIF

    IF hb_BLeft( cRaw, 3 ) == e"\xEF\xBB\xBF"
        ::lBOM := .T.
        cRaw := hb_BSubStr( cRaw, 4 )
    ENDIF
    IF hb_defaultValue( lForce, .F. )
        ::cCP := cDefCP
        IF !( cDefCP == "UTF8" )
            cRaw := hb_StrToUTF8( cRaw, cDefCP )
        ENDIF
    ELSEIF ! cRaw == "" .AND. ! hb_StrIsUTF8( cRaw )
        ::cCP := cDefCP
        cRaw := hb_StrToUTF8( cRaw, cDefCP )
    ENDIF

    IF Chr( 13 ) + Chr( 10 ) $ cRaw
        ::cEol := Chr( 13 ) + Chr( 10 )
    ELSEIF Chr( 10 ) $ cRaw
        ::cEol := Chr( 10 )
    ENDIF

    ::setText( cRaw )

RETURN .T.

METHOD loadPaged( cFile, cDefCP, lForce ) CLASS CEBuffer

    LOCAL cSample := Space( 8192 ), nRead, nSize, nStart

    ::nHandle := FOpen( cFile, FO_READ + FO_SHARED )
    IF ::nHandle < 0
        ::cError := "Cannot read " + cFile
        RETURN .F.
    ENDIF

    nRead := FRead( ::nHandle, @cSample, 3 )
    IF nRead >= 3 .AND. hb_BLeft( cSample, 3 ) == e"\xEF\xBB\xBF"
        ::lBOM := .T.
        nStart := 3
    ELSE
        FSeek( ::nHandle, 0, 0 )
        nStart := 0
    ENDIF

    nRead := FRead( ::nHandle, @cSample, 8192 )
    cSample := hb_BLeft( cSample, Max( 0, nRead ) )
    IF hb_defaultValue( lForce, .F. )
        ::cCP := cDefCP
    ELSEIF ::lBOM .OR. hb_StrIsUTF8( cSample ) .OR. ;
           hb_StrIsUTF8( hb_BLeft( cSample, Max( 0, hb_BLen( cSample ) - 1 ) ) ) .OR. ;
           hb_StrIsUTF8( hb_BLeft( cSample, Max( 0, hb_BLen( cSample ) - 2 ) ) ) .OR. ;
           hb_StrIsUTF8( hb_BLeft( cSample, Max( 0, hb_BLen( cSample ) - 3 ) ) )
        ::cCP := "UTF8"
    ELSE
        ::cCP := cDefCP
    ENDIF

    nSize := hb_FSize( cFile )
    ::lPaged := .T.
    ::lReadOnly := .F.
    ::lHilite := .F.
    ::aLines := { "" }
    ::nRow := 1
    ::nCol := 1
    ::nTop := 1
    ::nLeft := 0
    ::hCache := { => }
    ::aCache := {}
    ::nDocLen := Max( 0, nSize - nStart )
    ::aPieces := { { CE_SRC_ORIG, nStart, ::nDocLen } }
    ::nIdxPos := 0
    ::nBreaks := 0
    ::cIdxCarry := ""
    ::lIndexed := .F.
    ::lAfterBreak := .F.
    ::nDirtyFrom := 0
    ::nLF := ::nCR := ::nCRLF := 0
    ::nIdxEntries := 0
    ::aIdx := {}
    ::cIdxBuf := ""
    ::nIdxBufLen := 0

    IF ! ce_OpenSwap( Self )
        ::cError := "Cannot create swap files"
        ::closeFile()
        RETURN .F.
    ENDIF
    ce_IdxAppend( Self, 0 )                /* line 1 starts at logical 0 */

    ::indexStep( CE_FIRST_STEP )
    IF ! Empty( ::cError )
        ::closeFile()
        RETURN .F.
    ENDIF

RETURN .T.

METHOD indexStep( nBytes ) CLASS CEBuffer

    LOCAL cChunk, nWant, nGot, nScanPos, a, nBreaks0, nUsed

    IF ! ::lPaged .OR. ::lIndexed
        RETURN .T.
    ENDIF

    /* after an edit: drop index past the dirty line and scan from there */
    IF ::nDirtyFrom > 0
        ce_IdxTruncate( Self, Max( 1, ::nDirtyFrom ) )
        ::nIdxPos := ce_IdxGet( Self, Max( 1, ::nDirtyFrom ) )
        ::nBreaks := Max( 0, ::nIdxEntries - 1 )
        ::cIdxCarry := ""
        ::lAfterBreak := .F.
        ::nDirtyFrom := 0
        ::hCache := { => }
    ENDIF

    nWant := nBytes
    nBreaks0 := ::nBreaks
    DO WHILE nWant > 0 .AND. ! ::lIndexed
        IF ::nBreaks - nBreaks0 >= CE_IDX_MAX_BREAKS
            EXIT
        ENDIF
        nScanPos := ::nIdxPos - hb_BLen( ::cIdxCarry )
        nGot := Min( CE_INDEX_BLOCK, ::nDocLen - ::nIdxPos )
        /* Empty( Chr(13) ) is .T. in Harbour — never use Empty() on the CR carry */
        IF nGot <= 0 .AND. ::cIdxCarry == ""
            ::lIndexed := .T.
            EXIT
        ENDIF
        IF nGot > 0
            cChunk := ce_DocRead( Self, ::nIdxPos, nGot )
            IF hb_BLen( cChunk ) == 0
                ::cError := "Cannot read document"
                ::lIndexed := .T.
                EXIT
            ENDIF
            ::nIdxPos += hb_BLen( cChunk )
            nWant -= hb_BLen( cChunk )
        ELSE
            cChunk := ""
        ENDIF
        IF !( ::cIdxCarry == "" )
            cChunk := ::cIdxCarry + cChunk
            ::cIdxCarry := ""
        ENDIF
        IF hb_BRight( cChunk, 1 ) == CR .AND. ::nIdxPos < ::nDocLen
            ::cIdxCarry := CR
            cChunk := hb_BLeft( cChunk, hb_BLen( cChunk ) - 1 )
        ENDIF
        IF !( cChunk == "" )
            a := ce_ScanLines( Self, cChunk, nScanPos, CE_IDX_MAX_BREAKS - ( ::nBreaks - nBreaks0 ) )
            ::nBreaks := a[ 1 ]
            ::nLF += a[ 2 ]
            ::nCR += a[ 3 ]
            ::nCRLF += a[ 4 ]
            ::lAfterBreak := a[ 5 ]
            nUsed := a[ 6 ]
            /* scan stopped early (line cap): rewind so the rest is done next step */
            IF nUsed < hb_BLen( cChunk )
                ::nIdxPos -= ( hb_BLen( cChunk ) - nUsed )
                nWant := 0
            ENDIF
        ENDIF
        IF ::nIdxPos >= ::nDocLen .AND. ::cIdxCarry == ""
            ::lIndexed := .T.
        ENDIF
    ENDDO

    IF ::nCRLF + ::nLF + ::nCR > 0
        DO CASE
        CASE ::nCRLF >= ::nLF .AND. ::nCRLF >= ::nCR
            ::cEol := CR + LF
        CASE ::nCR > ::nLF
            ::cEol := CR
        OTHERWISE
            ::cEol := LF
        ENDCASE
    ENDIF
    ::lFinalEol := ::lAfterBreak
    ce_IdxFlush( Self )

RETURN ::lIndexed

METHOD indexAll( bProgress ) CLASS CEBuffer

    LOCAL nSize := Max( 1, ::nDocLen )

    DO WHILE ! ::indexStep( 64 * CE_BLOCK )
        IF bProgress != NIL
            Eval( bProgress, Int( 100 * ::nIdxPos / nSize ) )
        ENDIF
    ENDDO

RETURN Empty( ::cError )

METHOD follow() CLASS CEBuffer

    LOCAL nSize, nStart, nHave

    /* tail growth only — never reload: a bad hb_FSize() used to call load()
       after Idx 100% and wipe the buffer so the editor stayed blank */
    IF ! ::lPaged .OR. ! ::lIndexed .OR. Len( ::aPieces ) != 1 .OR. ;
       ::aPieces[ 1 ][ 1 ] != CE_SRC_ORIG .OR. Empty( ::cFile )
        RETURN .F.
    ENDIF
    nSize := hb_FSize( ::cFile )
    IF nSize < 0
        RETURN .F.
    ENDIF
    nStart := ::aPieces[ 1 ][ 2 ]
    nHave := nStart + ::nDocLen
    IF nSize <= nHave
        RETURN .F.
    ENDIF
    ::nDocLen := nSize - nStart
    ::aPieces[ 1 ][ 3 ] := ::nDocLen
    ::lIndexed := .F.
    ::hCache := { => }

RETURN .T.

METHOD closeFile() CLASS CEBuffer
    ce_IdxFlush( Self )
    IF ::nHandle >= 0
        FClose( ::nHandle )
        ::nHandle := -1
    ENDIF
    IF ::nAddHandle >= 0
        FClose( ::nAddHandle )
        ::nAddHandle := -1
    ENDIF
    IF ::nIdxHandle >= 0
        FClose( ::nIdxHandle )
        ::nIdxHandle := -1
    ENDIF
    IF ! Empty( ::cIdxPath )
        FErase( ::cIdxPath )
        ::cIdxPath := ""
    ENDIF
    IF ! Empty( ::cAddPath )
        FErase( ::cAddPath )
        ::cAddPath := ""
    ENDIF
    ::lPaged := .F.
    ::aPieces := {}
    ::aCache := {}
    ::nDocLen := 0
    ::nBreaks := 0
    ::nIdxEntries := 0
    ::aIdx := {}
    ::cIdxBuf := ""
    ::nIdxBufLen := 0
    ::lIndexed := .T.
    ::nDirtyFrom := 0
    ::hCache := { => }
RETURN NIL

METHOD save( cFile ) CLASS CEBuffer

    LOCAL cText, nOut, cChunk, nOff, nChunk

    ::cError := ""
    IF Empty( cFile )
        cFile := ::cFile
    ENDIF
    IF Empty( cFile )
        ::cError := "No file name"
        RETURN .F.
    ENDIF

    IF ::lPaged
        IF ! ::lIndexed
            ::indexAll()
        ENDIF
        nOut := FCreate( cFile )
        IF nOut < 0
            ::cError := "Cannot write " + cFile + " (" + hb_ntos( FError() ) + ")"
            RETURN .F.
        ENDIF
        IF ::lBOM
            FWrite( nOut, e"\xEF\xBB\xBF" )
        ENDIF
        nOff := 0
        DO WHILE nOff < ::nDocLen
            nChunk := Min( CE_BLOCK, ::nDocLen - nOff )
            cChunk := ce_DocRead( Self, nOff, nChunk )
            IF !( ::cCP == "UTF8" )
                /* document bytes are already in file codepage */
            ENDIF
            IF FWrite( nOut, cChunk ) != hb_BLen( cChunk )
                FClose( nOut )
                ::cError := "Cannot write " + cFile
                RETURN .F.
            ENDIF
            nOff += hb_BLen( cChunk )
        ENDDO
        FClose( nOut )
        ::cFile := cFile
        ::nSavedGroup := IIF( Empty( ::aUndo ), 0, ATail( ::aUndo )[ UR_GROUP ] )
        ::cLastKind := ""
        RETURN .T.
    ENDIF

    IF Len( ::aLines ) == 1 .AND. ::aLines[ 1 ] == ""
        cText := ""
    ELSE
        cText := StrTran( ::getText(), LF, ::cEol ) + IIF( ::lFinalEol, ::cEol, "" )
    ENDIF
    IF !( ::cCP == "UTF8" )
        cText := hb_UTF8ToStr( cText, ::cCP )
    ENDIF
    IF ::lBOM
        cText := e"\xEF\xBB\xBF" + cText
    ENDIF

    IF ! hb_MemoWrit( cFile, cText )
        ::cError := "Cannot write " + cFile + " (" + hb_ntos( FError() ) + ")"
        RETURN .F.
    ENDIF

    ::cFile := cFile
    ::nSavedGroup := IIF( Empty( ::aUndo ), 0, ATail( ::aUndo )[ UR_GROUP ] )
    ::cLastKind := ""

RETURN .T.

METHOD setText( cText ) CLASS CEBuffer

    ::closeFile()
    ::aLines := hb_ATokens( StrTran( StrTran( cText, Chr( 13 ) + Chr( 10 ), LF ), Chr( 13 ), LF ), LF )
    IF Empty( ::aLines )
        ::aLines := { "" }
    ENDIF
    IF Len( ::aLines ) > 1 .AND. ATail( ::aLines ) == ""
        ASize( ::aLines, Len( ::aLines ) - 1 )
        ::lFinalEol := .T.
    ELSE
        ::lFinalEol := ( Len( ::aLines ) == 1 .AND. ::aLines[ 1 ] == "" )
    ENDIF
    ::nRow := 1
    ::nCol := 1
    ::nTop := 1
    ::nLeft := 0
    ::nWantCell := -1
    ::clearSel()
    ::aUndo := {}
    ::aRedo := {}
    ::nSavedGroup := 0
    ::cLastKind := ""
    ::aStates := { 0 }
    ::nStateValid := 1

RETURN NIL

METHOD getText() CLASS CEBuffer
    IF ::lPaged
        RETURN ""
    ENDIF
RETURN ce_Join( ::aLines, LF )

METHOD title() CLASS CEBuffer
    IF ! Empty( ::cFile )
        RETURN hb_FNameNameExt( ::cFile )
    ENDIF
RETURN IIF( Empty( ::cName ), "Untitled", ::cName )

METHOD isModified() CLASS CEBuffer
RETURN IIF( Empty( ::aUndo ), 0, ATail( ::aUndo )[ UR_GROUP ] ) != ::nSavedGroup

METHOD startAction( cKind ) CLASS CEBuffer
    IF !( cKind $ "type,bs,del" ) .OR. !( cKind == ::cLastKind ) .OR. ;
       ::nRow != ::nLastRow .OR. ::nCol != ::nLastCol .OR. ::nGroup == 0
        ::nGroupSeq++
        ::nGroup := ::nGroupSeq
    ENDIF
RETURN NIL

METHOD endAction( cKind ) CLASS CEBuffer
    ::cLastKind := cKind
    ::nLastRow := ::nRow
    ::nLastCol := ::nCol
RETURN NIL

METHOD insertText( nRow, nCol, cText ) CLASS CEBuffer

    LOCAL aEnd, aLast, nOff, cRaw

    IF Len( cText ) == 0
        RETURN { nRow, nCol }
    ENDIF
    IF ::lPaged
        DO WHILE nRow > ::lineCount() .AND. ! ::lIndexed
            ::indexStep( CE_IDLE_STEP )
        ENDDO
        nOff := ce_PosToOff( Self, nRow, nCol )
        cRaw := ce_ToFileBytes( Self, cText )
        ce_DocInsert( Self, nOff, cRaw )
        aEnd := ce_EndPos( nRow, nCol, cText )
        ::nDirtyFrom := nRow
        ::lIndexed := .F.
        ::hCache := { => }
        IF ! ::lHistMute
            aLast := IIF( Empty( ::aUndo ), NIL, ATail( ::aUndo ) )
            IF aLast != NIL .AND. aLast[ UR_GROUP ] == ::nGroup .AND. aLast[ UR_TYPE ] == "I" .AND. ;
               aLast[ UR_ROW2 ] == nRow .AND. aLast[ UR_COL2 ] == nCol .AND. ;
               aLast[ UR_ROW1 ] == aLast[ UR_ROW2 ] .AND. !( LF $ cText )
                aLast[ UR_TEXT ] += cText
                aLast[ UR_COL2 ] := aEnd[ 2 ]
            ELSE
                AAdd( ::aUndo, { ::nGroup, "I", nRow, nCol, aEnd[ 1 ], aEnd[ 2 ], cText, ::nRow, ::nCol } )
            ENDIF
            ::aRedo := {}
        ENDIF
        RETURN aEnd
    ENDIF

    aEnd := ce_BufIns( ::aLines, nRow, nCol, cText )
    ::invalidate( nRow )

    aLast := IIF( Empty( ::aUndo ), NIL, ATail( ::aUndo ) )
    IF aLast != NIL .AND. aLast[ UR_GROUP ] == ::nGroup .AND. aLast[ UR_TYPE ] == "I" .AND. ;
       aLast[ UR_ROW2 ] == nRow .AND. aLast[ UR_COL2 ] == nCol .AND. ;
       aLast[ UR_ROW1 ] == aLast[ UR_ROW2 ] .AND. !( LF $ cText )
        aLast[ UR_TEXT ] += cText
        aLast[ UR_COL2 ] := aEnd[ 2 ]
    ELSE
        AAdd( ::aUndo, { ::nGroup, "I", nRow, nCol, aEnd[ 1 ], aEnd[ 2 ], cText, ::nRow, ::nCol } )
    ENDIF
    ::aRedo := {}

RETURN aEnd

METHOD deleteRange( nR1, nC1, nR2, nC2 ) CLASS CEBuffer

    LOCAL cText, aLast, nOff1, nOff2, cRaw

    IF nR1 > nR2 .OR. ( nR1 == nR2 .AND. nC1 >= nC2 )
        RETURN ""
    ENDIF
    IF ::lPaged
        DO WHILE nR2 > ::lineCount() .AND. ! ::lIndexed
            ::indexStep( CE_IDLE_STEP )
        ENDDO
        nOff1 := ce_PosToOff( Self, nR1, nC1 )
        nOff2 := ce_PosToOff( Self, nR2, nC2 )
        cRaw := ce_DocDelete( Self, nOff1, nOff2 - nOff1 )
        cText := ce_FromFileBytes( Self, cRaw )
        ::nDirtyFrom := nR1
        ::lIndexed := .F.
        ::hCache := { => }
        IF ! ::lHistMute
            aLast := IIF( Empty( ::aUndo ), NIL, ATail( ::aUndo ) )
            IF aLast != NIL .AND. aLast[ UR_GROUP ] == ::nGroup .AND. aLast[ UR_TYPE ] == "D" .AND. ;
               !( LF $ cText ) .AND. !( LF $ aLast[ UR_TEXT ] ) .AND. aLast[ UR_ROW1 ] == nR1
                IF aLast[ UR_COL1 ] == nC1
                    aLast[ UR_TEXT ] += cText
                    ::aRedo := {}
                    RETURN cText
                ELSEIF aLast[ UR_COL1 ] == nC2
                    aLast[ UR_TEXT ] := cText + aLast[ UR_TEXT ]
                    aLast[ UR_COL1 ] := nC1
                    ::aRedo := {}
                    RETURN cText
                ENDIF
            ENDIF
            AAdd( ::aUndo, { ::nGroup, "D", nR1, nC1, 0, 0, cText, ::nRow, ::nCol } )
            ::aRedo := {}
        ENDIF
        RETURN cText
    ENDIF

    cText := ce_BufDel( ::aLines, nR1, nC1, nR2, nC2 )
    ::invalidate( nR1 )

    aLast := IIF( Empty( ::aUndo ), NIL, ATail( ::aUndo ) )
    IF aLast != NIL .AND. aLast[ UR_GROUP ] == ::nGroup .AND. aLast[ UR_TYPE ] == "D" .AND. ;
       !( LF $ cText ) .AND. !( LF $ aLast[ UR_TEXT ] ) .AND. aLast[ UR_ROW1 ] == nR1
        IF aLast[ UR_COL1 ] == nC1
            aLast[ UR_TEXT ] += cText
            ::aRedo := {}
            RETURN cText
        ELSEIF aLast[ UR_COL1 ] == nC2
            aLast[ UR_TEXT ] := cText + aLast[ UR_TEXT ]
            aLast[ UR_COL1 ] := nC1
            ::aRedo := {}
            RETURN cText
        ENDIF
    ENDIF
    AAdd( ::aUndo, { ::nGroup, "D", nR1, nC1, 0, 0, cText, ::nRow, ::nCol } )
    ::aRedo := {}

RETURN cText

METHOD undo() CLASS CEBuffer

    LOCAL nGroup, aRec, aDone := {}

    IF Empty( ::aUndo )
        RETURN .F.
    ENDIF
    nGroup := ATail( ::aUndo )[ UR_GROUP ]
    ::lHistMute := .T.
    DO WHILE ! Empty( ::aUndo ) .AND. ATail( ::aUndo )[ UR_GROUP ] == nGroup
        aRec := ATail( ::aUndo )
        ASize( ::aUndo, Len( ::aUndo ) - 1 )
        ::invalidate( aRec[ UR_ROW1 ] )
        IF ::lPaged
            IF aRec[ UR_TYPE ] == "I"
                ::deleteRange( aRec[ UR_ROW1 ], aRec[ UR_COL1 ], aRec[ UR_ROW2 ], aRec[ UR_COL2 ] )
            ELSE
                ::insertText( aRec[ UR_ROW1 ], aRec[ UR_COL1 ], aRec[ UR_TEXT ] )
            ENDIF
        ELSE
            IF aRec[ UR_TYPE ] == "I"
                ce_BufDel( ::aLines, aRec[ UR_ROW1 ], aRec[ UR_COL1 ], aRec[ UR_ROW2 ], aRec[ UR_COL2 ] )
            ELSE
                ce_BufIns( ::aLines, aRec[ UR_ROW1 ], aRec[ UR_COL1 ], aRec[ UR_TEXT ] )
            ENDIF
        ENDIF
        ::nRow := aRec[ UR_CURROW ]
        ::nCol := aRec[ UR_CURCOL ]
        AAdd( aDone, aRec )
    ENDDO
    ::lHistMute := .F.
    AAdd( ::aRedo, aDone )
    ::clearSel()
    ::cLastKind := ""
    ::nGroup := 0

RETURN .T.

METHOD redo() CLASS CEBuffer

    LOCAL aDone, aRec, aEnd, i

    IF Empty( ::aRedo )
        RETURN .F.
    ENDIF
    aDone := ATail( ::aRedo )
    ASize( ::aRedo, Len( ::aRedo ) - 1 )
    ::lHistMute := .T.
    FOR i := Len( aDone ) TO 1 STEP -1
        aRec := aDone[ i ]
        ::invalidate( aRec[ UR_ROW1 ] )
        IF ::lPaged
            IF aRec[ UR_TYPE ] == "I"
                aEnd := ::insertText( aRec[ UR_ROW1 ], aRec[ UR_COL1 ], aRec[ UR_TEXT ] )
                ::nRow := aEnd[ 1 ]
                ::nCol := aEnd[ 2 ]
            ELSE
                aEnd := ce_EndPos( aRec[ UR_ROW1 ], aRec[ UR_COL1 ], aRec[ UR_TEXT ] )
                ::deleteRange( aRec[ UR_ROW1 ], aRec[ UR_COL1 ], aEnd[ 1 ], aEnd[ 2 ] )
                ::nRow := aRec[ UR_ROW1 ]
                ::nCol := aRec[ UR_COL1 ]
            ENDIF
        ELSE
            IF aRec[ UR_TYPE ] == "I"
                aEnd := ce_BufIns( ::aLines, aRec[ UR_ROW1 ], aRec[ UR_COL1 ], aRec[ UR_TEXT ] )
                ::nRow := aEnd[ 1 ]
                ::nCol := aEnd[ 2 ]
            ELSE
                aEnd := ce_EndPos( aRec[ UR_ROW1 ], aRec[ UR_COL1 ], aRec[ UR_TEXT ] )
                ce_BufDel( ::aLines, aRec[ UR_ROW1 ], aRec[ UR_COL1 ], aEnd[ 1 ], aEnd[ 2 ] )
                ::nRow := aRec[ UR_ROW1 ]
                ::nCol := aRec[ UR_COL1 ]
            ENDIF
        ENDIF
        AAdd( ::aUndo, aRec )
    NEXT
    ::lHistMute := .F.
    ::clearSel()
    ::cLastKind := ""
    ::nGroup := 0

RETURN .T.

METHOD hasSel() CLASS CEBuffer
RETURN ::nSelMode != CE_SEL_NONE .AND. ( ::nSelRow != ::nRow .OR. ::nSelCol != ::nCol )

METHOD clearSel() CLASS CEBuffer
    ::nSelMode := CE_SEL_NONE
    ::nSelRow := 0
    ::nSelCol := 0
    ::lMarking := .F.
RETURN NIL

METHOD streamRange() CLASS CEBuffer
    IF ! ::hasSel()
        RETURN NIL
    ENDIF
    IF ::nSelRow < ::nRow .OR. ( ::nSelRow == ::nRow .AND. ::nSelCol < ::nCol )
        RETURN { ::nSelRow, ::nSelCol, ::nRow, ::nCol }
    ENDIF
RETURN { ::nRow, ::nCol, ::nSelRow, ::nSelCol }

METHOD selText() CLASS CEBuffer

    LOCAL a := ::streamRange()

    IF a == NIL .OR. ::nSelMode != CE_SEL_STREAM
        RETURN ""
    ENDIF
    IF ::lPaged
        RETURN ce_PagedRange( Self, a[ 1 ], a[ 2 ], a[ 3 ], a[ 4 ] )
    ENDIF

RETURN ce_RangeText( ::aLines, a[ 1 ], a[ 2 ], a[ 3 ], a[ 4 ] )

METHOD find( cWhat, nRow, nCol, lCase, lWord, lBack, lWrap ) CLASS CEBuffer

    LOCAL nCount := Len( ::aLines ), n, nLine, p, cFind, cLine, lWrapped := .F.

    IF Len( cWhat ) == 0
        RETURN NIL
    ENDIF
    IF ::lPaged
        RETURN ce_FindPaged( Self, cWhat, nRow, nCol, lCase, lWord, lBack, lWrap )
    ENDIF
    cFind := IIF( lCase, cWhat, Upper( cWhat ) )
    FOR n := 0 TO nCount
        IF lBack
            nLine := nRow - n
            IF nLine < 1
                IF ! lWrap
                    EXIT
                ENDIF
                nLine += nCount
                lWrapped := .T.
            ENDIF
        ELSE
            nLine := nRow + n
            IF nLine > nCount
                IF ! lWrap
                    EXIT
                ENDIF
                nLine -= nCount
                lWrapped := .T.
            ENDIF
        ENDIF
        cLine := IIF( lCase, ::aLines[ nLine ], Upper( ::aLines[ nLine ] ) )
        IF lBack
            p := ce_LastMatch( cLine, cFind, IIF( n == 0, nCol - 1, Len( cLine ) ), lWord, ::aLines[ nLine ] )
        ELSE
            p := ce_NextMatch( cLine, cFind, IIF( n == 0, nCol, 1 ), lWord, ::aLines[ nLine ] )
        ENDIF
        IF p > 0
            RETURN { nLine, p, lWrapped }
        ENDIF
    NEXT

RETURN NIL

/*----------------------------------------------------------- large files: swap / index / cache */

STATIC FUNCTION ce_OpenSwap( oB )

    LOCAL cBase := hb_FNameMerge( hb_DirTemp(), ;
                    "cedit-" + hb_ntos( Int( hb_MilliSeconds() ) ) + "-" + hb_ntos( oB:nHandle ) )
    LOCAL n

    oB:cIdxPath := cBase + ".idx"
    oB:cAddPath := cBase + ".add"
    /* create, then reopen read/write so index seeks can FRead */
    n := FCreate( oB:cIdxPath )
    IF n < 0
        RETURN .F.
    ENDIF
    FClose( n )
    oB:nIdxHandle := FOpen( oB:cIdxPath, FO_READWRITE + FO_EXCLUSIVE )
    IF oB:nIdxHandle < 0
        FErase( oB:cIdxPath )
        RETURN .F.
    ENDIF
    n := FCreate( oB:cAddPath )
    IF n < 0
        FClose( oB:nIdxHandle )
        oB:nIdxHandle := -1
        FErase( oB:cIdxPath )
        RETURN .F.
    ENDIF
    FClose( n )
    oB:nAddHandle := FOpen( oB:cAddPath, FO_READWRITE + FO_EXCLUSIVE )
    IF oB:nAddHandle < 0
        FClose( oB:nIdxHandle )
        oB:nIdxHandle := -1
        FErase( oB:cIdxPath )
        FErase( oB:cAddPath )
        RETURN .F.
    ENDIF

RETURN .T.

STATIC FUNCTION ce_IdxAppend( oB, nOff )

    LOCAL cEnt := ce_U64( nOff )

    AAdd( oB:aIdx, Max( 0, Int( nOff ) ) )
    oB:nIdxEntries := Len( oB:aIdx )

    IF oB:nIdxBufLen + 8 > CE_IDX_BUF
        ce_IdxFlush( oB )
    ENDIF
    IF oB:nIdxBufLen == 0
        oB:cIdxBuf := cEnt
    ELSE
        oB:cIdxBuf := hb_BLeft( oB:cIdxBuf, oB:nIdxBufLen ) + cEnt
    ENDIF
    oB:nIdxBufLen += 8

RETURN NIL

STATIC FUNCTION ce_IdxFlush( oB )
    IF oB:nIdxHandle >= 0 .AND. oB:nIdxBufLen > 0
        FSeek( oB:nIdxHandle, 0, 2 )
        FWrite( oB:nIdxHandle, hb_BLeft( oB:cIdxBuf, oB:nIdxBufLen ) )
        oB:cIdxBuf := ""
        oB:nIdxBufLen := 0
    ENDIF
RETURN NIL

STATIC FUNCTION ce_U64( n )

    LOCAL c := Space( 8 ), i, nByte

    n := Max( 0, Int( n ) )
    FOR i := 1 TO 8
        nByte := n % 256
        hb_BPoke( @c, i, nByte )
        n := Int( ( n - nByte ) / 256 )
    NEXT

RETURN c

STATIC FUNCTION ce_IdxGet( oB, nLine )

    LOCAL nEntry := Int( ( nLine - 1 ) / CE_LINE_STEP )

    IF nLine < 1 .OR. nEntry >= Len( oB:aIdx )
        RETURN 0
    ENDIF

RETURN oB:aIdx[ nEntry + 1 ]

STATIC FUNCTION ce_IdxTruncate( oB, nKeepLines )

    LOCAL nKeep := Int( ( Max( 1, nKeepLines ) - 1 ) / CE_LINE_STEP ) + 1

    ce_IdxFlush( oB )
    IF nKeep < 1
        nKeep := 1
    ENDIF
    IF nKeep > oB:nIdxEntries
        RETURN NIL
    ENDIF
    ASize( oB:aIdx, nKeep )
    oB:nIdxEntries := nKeep
    oB:cIdxBuf := ""
    oB:nIdxBufLen := 0
    ce_IdxRewrite( oB, nKeep )

RETURN NIL

STATIC FUNCTION ce_IdxRewrite( oB, nKeep )

    LOCAL cBuf := Space( Min( CE_IDX_BUF, Max( 8, nKeep * 8 ) ) ), nLeft := nKeep * 8, nGot, cTmp, nNew

    cTmp := oB:cIdxPath + ".tmp"
    nNew := FCreate( cTmp )
    IF nNew < 0
        RETURN .F.
    ENDIF
    FSeek( oB:nIdxHandle, 0, 0 )
    DO WHILE nLeft > 0
        nGot := FRead( oB:nIdxHandle, @cBuf, Min( hb_BLen( cBuf ), nLeft ) )
        IF nGot <= 0
            EXIT
        ENDIF
        FWrite( nNew, hb_BLeft( cBuf, nGot ) )
        nLeft -= nGot
    ENDDO
    FClose( nNew )
    FClose( oB:nIdxHandle )
    FErase( oB:cIdxPath )
    FRename( cTmp, oB:cIdxPath )
    oB:nIdxHandle := FOpen( oB:cIdxPath, FO_READWRITE + FO_EXCLUSIVE )
    oB:nIdxEntries := nKeep
    oB:nIdxBufLen := 0

RETURN oB:nIdxHandle >= 0

STATIC FUNCTION ce_PageGet( oB, nSrc, nOff, nLen )

    LOCAL nPage := Int( nOff / CE_PAGE_SIZE ) * CE_PAGE_SIZE
    LOCAL i, a, nHandle, cData := Space( CE_PAGE_SIZE ), nGot

    FOR i := 1 TO Len( oB:aCache )
        a := oB:aCache[ i ]
        IF HB_ISARRAY( a ) .AND. a[ 1 ] == nSrc .AND. a[ 2 ] == nPage
            IF i > 1
                hb_ADel( oB:aCache, i, .T. )
                hb_AIns( oB:aCache, 1, a, .T. )
            ENDIF
            RETURN hb_BSubStr( a[ 3 ], ( nOff - nPage ) + 1, nLen )
        ENDIF
    NEXT

    nHandle := IIF( nSrc == CE_SRC_ORIG, oB:nHandle, oB:nAddHandle )
    FSeek( nHandle, nPage, 0 )
    nGot := FRead( nHandle, @cData, CE_PAGE_SIZE )
    cData := hb_BLeft( cData, Max( 0, nGot ) )
    hb_AIns( oB:aCache, 1, { nSrc, nPage, cData }, .T. )
    IF Len( oB:aCache ) > CE_CACHE_PAGES
        ASize( oB:aCache, CE_CACHE_PAGES )
    ENDIF

RETURN hb_BSubStr( cData, ( nOff - nPage ) + 1, nLen )

STATIC FUNCTION ce_DocRead( oB, nOff, nLen )

    LOCAL c := "", nLeft, nAt := 0, i, a, nTake, nLocal, nPart, nPageLeft, cPart

    IF nLen <= 0 .OR. nOff >= oB:nDocLen
        RETURN ""
    ENDIF
    nLeft := Min( nLen, oB:nDocLen - nOff )
    FOR i := 1 TO Len( oB:aPieces )
        a := oB:aPieces[ i ]
        IF nLeft <= 0
            EXIT
        ENDIF
        IF nOff < nAt + a[ 3 ]
            nLocal := a[ 2 ] + ( nOff - nAt )
            nTake := Min( nLeft, a[ 3 ] - ( nOff - nAt ) )
            DO WHILE nTake > 0
                nPageLeft := CE_PAGE_SIZE - ( nLocal % CE_PAGE_SIZE )
                IF nPageLeft <= 0
                    nPageLeft := CE_PAGE_SIZE
                ENDIF
                nPart := Min( nTake, nPageLeft )
                cPart := ce_PageGet( oB, a[ 1 ], nLocal, nPart )
                IF hb_BLen( cPart ) == 0
                    RETURN c
                ENDIF
                c += cPart
                nLocal += hb_BLen( cPart )
                nOff += hb_BLen( cPart )
                nLeft -= hb_BLen( cPart )
                nTake -= hb_BLen( cPart )
            ENDDO
        ENDIF
        nAt += a[ 3 ]
    NEXT

RETURN c

STATIC FUNCTION ce_DocInsert( oB, nOff, cRaw )

    LOCAL nAddOff, i, a, nAt := 0, nInto, cLeft, cRight

    IF Empty( cRaw )
        RETURN NIL
    ENDIF
    nAddOff := FSeek( oB:nAddHandle, 0, 2 )
    FWrite( oB:nAddHandle, cRaw )
    oB:aCache := {}

    IF nOff >= oB:nDocLen
        AAdd( oB:aPieces, { CE_SRC_ADD, nAddOff, hb_BLen( cRaw ) } )
        oB:nDocLen += hb_BLen( cRaw )
        RETURN NIL
    ENDIF
    IF nOff <= 0
        hb_AIns( oB:aPieces, 1, { CE_SRC_ADD, nAddOff, hb_BLen( cRaw ) }, .T. )
        oB:nDocLen += hb_BLen( cRaw )
        RETURN NIL
    ENDIF

    FOR i := 1 TO Len( oB:aPieces )
        a := oB:aPieces[ i ]
        IF nOff < nAt + a[ 3 ]
            nInto := nOff - nAt
            IF nInto > 0
                cLeft := { a[ 1 ], a[ 2 ], nInto }
                cRight := { a[ 1 ], a[ 2 ] + nInto, a[ 3 ] - nInto }
                oB:aPieces[ i ] := cLeft
                hb_AIns( oB:aPieces, i + 1, { CE_SRC_ADD, nAddOff, hb_BLen( cRaw ) }, .T. )
                hb_AIns( oB:aPieces, i + 2, cRight, .T. )
            ELSE
                hb_AIns( oB:aPieces, i, { CE_SRC_ADD, nAddOff, hb_BLen( cRaw ) }, .T. )
            ENDIF
            oB:nDocLen += hb_BLen( cRaw )
            RETURN NIL
        ENDIF
        nAt += a[ 3 ]
    NEXT
    AAdd( oB:aPieces, { CE_SRC_ADD, nAddOff, hb_BLen( cRaw ) } )
    oB:nDocLen += hb_BLen( cRaw )

RETURN NIL

STATIC FUNCTION ce_DocDelete( oB, nOff, nLen )

    LOCAL cGone, nAt := 0, i, a, aNew := {}, nDelEnd, nPieceEnd

    IF nLen <= 0
        RETURN ""
    ENDIF
    cGone := ce_DocRead( oB, nOff, nLen )
    nDelEnd := nOff + nLen
    FOR i := 1 TO Len( oB:aPieces )
        a := oB:aPieces[ i ]
        nPieceEnd := nAt + a[ 3 ]
        IF nPieceEnd <= nOff .OR. nAt >= nDelEnd
            AAdd( aNew, { a[ 1 ], a[ 2 ], a[ 3 ] } )
        ELSE
            IF nAt < nOff
                AAdd( aNew, { a[ 1 ], a[ 2 ], nOff - nAt } )
            ENDIF
            IF nPieceEnd > nDelEnd
                AAdd( aNew, { a[ 1 ], a[ 2 ] + ( nDelEnd - nAt ), nPieceEnd - nDelEnd } )
            ENDIF
        ENDIF
        nAt := nPieceEnd
    NEXT
    oB:aPieces := aNew
    oB:nDocLen := Max( 0, oB:nDocLen - nLen )
    oB:aCache := {}

RETURN cGone

STATIC FUNCTION ce_NextBreak( c, nFrom )

    LOCAL nLF := hb_BAt( LF, c, nFrom ), nCR := hb_BAt( CR, c, nFrom )

    IF nLF == 0
        RETURN nCR
    ENDIF
    IF nCR == 0 .OR. nLF < nCR
        RETURN nLF
    ENDIF

RETURN nCR

STATIC FUNCTION ce_BreakWidth( c, nAt )
    IF hb_BSubStr( c, nAt, 2 ) == CR + LF
        RETURN 2
    ENDIF
RETURN 1

STATIC FUNCTION ce_ScanLines( oB, cScan, nScanPos, nMaxNew )

    LOCAL nFrom := 1, nBreak, nWidth, nNext, nLF := 0, nCR := 0, nCRLF := 0
    LOCAL lAfterBreak := .F., nBreaks := oB:nBreaks, nNew := 0

    nMaxNew := hb_defaultValue( nMaxNew, CE_IDX_MAX_BREAKS )
    DO WHILE nFrom <= hb_BLen( cScan )
        IF nNew >= nMaxNew
            EXIT
        ENDIF
        nBreak := ce_NextBreak( cScan, nFrom )
        IF nBreak == 0
            lAfterBreak := .F.
            nFrom := hb_BLen( cScan ) + 1
            EXIT
        ENDIF
        nWidth := ce_BreakWidth( cScan, nBreak )
        IF nWidth == 2
            nCRLF++
            nCR++
            nLF++
        ELSEIF hb_BSubStr( cScan, nBreak, 1 ) == CR
            nCR++
        ELSE
            nLF++
        ENDIF
        nNext := nScanPos + nBreak - 1 + nWidth
        nBreaks++
        nNew++
        lAfterBreak := .T.
        IF nBreaks % CE_LINE_STEP == 0
            ce_IdxAppend( oB, nNext )
        ENDIF
        nFrom := nBreak + nWidth
    ENDDO

RETURN { nBreaks, nLF, nCR, nCRLF, lAfterBreak, nFrom - 1 }

STATIC FUNCTION ce_PagedLine( oB, n )

    LOCAL nOff, nNext, cRaw, cLine, nBreak, nWidth

    IF n < 1 .OR. n > oB:lineCount()
        RETURN ""
    ENDIF
    IF hb_HHasKey( oB:hCache, n )
        RETURN oB:hCache[ n ]
    ENDIF
    /* never indexAll() here: paint / cursor would freeze on a multi-GB file */
    DO WHILE n > oB:nIdxEntries .AND. ! oB:lIndexed
        IF oB:indexStep( CE_IDLE_STEP )
            EXIT
        ENDIF
        IF n <= oB:nIdxEntries
            EXIT
        ENDIF
    ENDDO
    IF n > oB:nIdxEntries
        RETURN ""
    ENDIF
    nOff := ce_IdxGet( oB, n )
    IF n < oB:lineCount() .AND. n + CE_LINE_STEP - 1 <= oB:nIdxEntries * CE_LINE_STEP
        /* may need to scan forward within step; with STEP=1 next start is known */
    ENDIF
    IF CE_LINE_STEP == 1 .AND. n < oB:nIdxEntries
        nNext := ce_IdxGet( oB, n + 1 )
        cRaw := ce_DocRead( oB, nOff, Min( CE_LINE_CAP + 4, Max( 0, nNext - nOff ) ) )
    ELSE
        cRaw := ce_DocRead( oB, nOff, CE_LINE_CAP + 4 )
    ENDIF
    nBreak := ce_NextBreak( cRaw, 1 )
    IF nBreak > 0
        nWidth := ce_BreakWidth( cRaw, nBreak )
        cRaw := hb_BLeft( cRaw, nBreak - 1 )
        HB_SYMBOL_UNUSED( nWidth )
    ENDIF
    cLine := ce_FromFileBytes( oB, cRaw )
    IF Len( cLine ) > CE_LINE_CAP
        cLine := Left( cLine, CE_LINE_CAP )
    ENDIF
    IF Len( oB:hCache ) >= 80
        oB:hCache := { => }
    ENDIF
    oB:hCache[ n ] := cLine

RETURN cLine

STATIC FUNCTION ce_ToFileBytes( oB, cUtf8 )

    LOCAL c := StrTran( StrTran( cUtf8, Chr( 13 ) + Chr( 10 ), LF ), Chr( 13 ), LF )

    c := StrTran( c, LF, oB:cEol )
    IF !( oB:cCP == "UTF8" )
        c := hb_UTF8ToStr( c, oB:cCP )
    ENDIF

RETURN c

STATIC FUNCTION ce_FromFileBytes( oB, cRaw )
    IF !( oB:cCP == "UTF8" ) .AND. !( cRaw == "" )
        RETURN hb_StrToUTF8( cRaw, oB:cCP )
    ENDIF
RETURN StrTran( StrTran( cRaw, CR + LF, LF ), CR, LF )

STATIC FUNCTION ce_PosToOff( oB, nRow, nCol )

    LOCAL cLine, cHead, nOff

    DO WHILE nRow > oB:nIdxEntries .AND. ! oB:lIndexed
        oB:indexStep( CE_IDLE_STEP )
    ENDDO
    nOff := ce_IdxGet( oB, nRow )
    IF nCol <= 1
        RETURN nOff
    ENDIF
    cLine := oB:line( nRow )
    cHead := Left( cLine, Min( Len( cLine ), nCol - 1 ) )
    IF !( oB:cCP == "UTF8" )
        RETURN nOff + hb_BLen( hb_UTF8ToStr( cHead, oB:cCP ) )
    ENDIF

RETURN nOff + hb_BLen( cHead )

STATIC FUNCTION ce_PagedRange( oB, nR1, nC1, nR2, nC2 )

    LOCAL a, i, nSize := 0

    IF nR1 == nR2
        RETURN SubStr( oB:line( nR1 ), nC1, nC2 - nC1 )
    ENDIF
    a := { SubStr( oB:line( nR1 ), nC1 ) }
    FOR i := nR1 + 1 TO nR2 - 1
        AAdd( a, oB:line( i ) )
        nSize += hb_BLen( ATail( a ) ) + 1
        IF nSize > CE_MEM_LIMIT
            RETURN ce_Join( a, LF )
        ENDIF
    NEXT
    AAdd( a, Left( oB:line( nR2 ), nC2 - 1 ) )

RETURN ce_Join( a, LF )

STATIC FUNCTION ce_FindPaged( oB, cWhat, nRow, nCol, lCase, lWord, lBack, lWrap )

    LOCAL cFind := IIF( lCase, cWhat, Upper( cWhat ) )
    LOCAL aHit := NIL, aFirst := NIL, aBefore := NIL, aLast := NIL
    LOCAL nLine, nCount, cLine, cCmp, p, nFrom, nTo

    IF ! oB:lIndexed
        oB:indexAll()
    ENDIF
    nCount := oB:lineCount()
    IF lBack
        FOR nLine := nRow TO 1 STEP -1
            cLine := oB:line( nLine )
            cCmp := IIF( lCase, cLine, Upper( cLine ) )
            nTo := IIF( nLine == nRow, nCol - 1, Len( cCmp ) )
            p := ce_LastMatch( cCmp, cFind, nTo, lWord, cLine )
            IF p > 0
                RETURN { nLine, p, .F. }
            ENDIF
        NEXT
        IF lWrap
            FOR nLine := nCount TO nRow + 1 STEP -1
                cLine := oB:line( nLine )
                cCmp := IIF( lCase, cLine, Upper( cLine ) )
                p := ce_LastMatch( cCmp, cFind, Len( cCmp ), lWord, cLine )
                IF p > 0
                    RETURN { nLine, p, .T. }
                ENDIF
            NEXT
        ENDIF
        RETURN NIL
    ENDIF
    FOR nLine := nRow TO nCount
        cLine := oB:line( nLine )
        cCmp := IIF( lCase, cLine, Upper( cLine ) )
        nFrom := IIF( nLine == nRow, nCol, 1 )
        p := ce_NextMatch( cCmp, cFind, nFrom, lWord, cLine )
        IF p > 0
            RETURN { nLine, p, .F. }
        ENDIF
        IF aFirst == NIL
            p := ce_NextMatch( cCmp, cFind, 1, lWord, cLine )
            IF p > 0 .AND. nLine < nRow
                aFirst := { nLine, p, .T. }
            ENDIF
        ENDIF
    NEXT
    IF lWrap
        FOR nLine := 1 TO nRow - 1
            cLine := oB:line( nLine )
            cCmp := IIF( lCase, cLine, Upper( cLine ) )
            p := ce_NextMatch( cCmp, cFind, 1, lWord, cLine )
            IF p > 0
                RETURN { nLine, p, .T. }
            ENDIF
        NEXT
    ENDIF
    HB_SYMBOL_UNUSED( aHit )
    HB_SYMBOL_UNUSED( aBefore )
    HB_SYMBOL_UNUSED( aLast )

RETURN aFirst

/*----------------------------------------------------------- helpers */

STATIC FUNCTION ce_NextMatch( cLine, cFind, nFrom, lWord, cOrig )

    LOCAL p := hb_At( cFind, cLine, nFrom )

    DO WHILE p > 0 .AND. lWord .AND. ! ce_WholeWord( cOrig, p, Len( cFind ) )
        p := hb_At( cFind, cLine, p + 1 )
    ENDDO

RETURN p

STATIC FUNCTION ce_LastMatch( cLine, cFind, nTo, lWord, cOrig )

    LOCAL p := hb_At( cFind, cLine ), nLast := 0

    DO WHILE p > 0 .AND. p + Len( cFind ) - 1 <= nTo
        IF ! lWord .OR. ce_WholeWord( cOrig, p, Len( cFind ) )
            nLast := p
        ENDIF
        p := hb_At( cFind, cLine, p + 1 )
    ENDDO

RETURN nLast

STATIC FUNCTION ce_WholeWord( cLine, p, nLen )
RETURN ( p == 1 .OR. ! ce_IsWordChar( SubStr( cLine, p - 1, 1 ) ) ) .AND. ;
       ! ce_IsWordChar( SubStr( cLine, p + nLen, 1 ) )

FUNCTION ce_BufIns( aLines, nRow, nCol, cText )

    LOCAL aIns := hb_ATokens( cText, LF ), cLine := aLines[ nRow ]
    LOCAL cTail := SubStr( cLine, nCol ), n := Len( aIns ), i

    IF n == 1
        aLines[ nRow ] := Left( cLine, nCol - 1 ) + aIns[ 1 ] + cTail
        RETURN { nRow, nCol + Len( aIns[ 1 ] ) }
    ENDIF
    aLines[ nRow ] := Left( cLine, nCol - 1 ) + aIns[ 1 ]
    FOR i := 2 TO n - 1
        hb_AIns( aLines, nRow + i - 1, aIns[ i ], .T. )
    NEXT
    hb_AIns( aLines, nRow + n - 1, aIns[ n ] + cTail, .T. )

RETURN { nRow + n - 1, Len( aIns[ n ] ) + 1 }

FUNCTION ce_BufDel( aLines, nR1, nC1, nR2, nC2 )

    LOCAL cText := ce_RangeText( aLines, nR1, nC1, nR2, nC2 ), i

    IF nR1 == nR2
        aLines[ nR1 ] := Left( aLines[ nR1 ], nC1 - 1 ) + SubStr( aLines[ nR1 ], nC2 )
    ELSE
        aLines[ nR1 ] := Left( aLines[ nR1 ], nC1 - 1 ) + SubStr( aLines[ nR2 ], nC2 )
        FOR i := nR1 + 1 TO nR2
            hb_ADel( aLines, nR1 + 1, .T. )
        NEXT
    ENDIF

RETURN cText

FUNCTION ce_RangeText( aLines, nR1, nC1, nR2, nC2 )

    LOCAL a, i

    IF nR1 == nR2
        RETURN SubStr( aLines[ nR1 ], nC1, nC2 - nC1 )
    ENDIF
    a := { SubStr( aLines[ nR1 ], nC1 ) }
    FOR i := nR1 + 1 TO nR2 - 1
        AAdd( a, aLines[ i ] )
    NEXT
    AAdd( a, Left( aLines[ nR2 ], nC2 - 1 ) )

RETURN ce_Join( a, LF )

FUNCTION ce_EndPos( nRow, nCol, cText )

    LOCAL a := hb_ATokens( cText, LF )

    IF Len( a ) == 1
        RETURN { nRow, nCol + Len( a[ 1 ] ) }
    ENDIF

RETURN { nRow + Len( a ) - 1, Len( ATail( a ) ) + 1 }

FUNCTION ce_Join( a, cSep, nFrom, nTo )

    LOCAL c, i, nMid

    IF nFrom == NIL
        nFrom := 1
        nTo := Len( a )
    ENDIF
    IF nTo < nFrom
        RETURN ""
    ENDIF
    IF nTo - nFrom < 64
        c := a[ nFrom ]
        FOR i := nFrom + 1 TO nTo
            c += cSep + a[ i ]
        NEXT
        RETURN c
    ENDIF
    nMid := Int( ( nFrom + nTo ) / 2 )

RETURN ce_Join( a, cSep, nFrom, nMid ) + cSep + ce_Join( a, cSep, nMid + 1, nTo )
