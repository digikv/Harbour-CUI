/*
 * CEBuffer - one open file of CEdit: lines, cursor, selection, undo / redo
 *
 * Copyright (c) 2026 Srđan Dragojlović - dual licensed, see LICENSE
 *
 * The text is kept in UTF-8 (the VM runs in UTF8EX: Len() / SubStr() count
 * characters). A file that is not valid UTF-8 is read in the codepage given
 * to load() (cCP, e.g. "SL852") and written back in it. Positions are
 * 1-based: nRow = line, nCol = character in the line (Len() + 1 = line end).
 *
 * Undo: every change is a record (insert or delete); the records of one user
 * action share a group number and are undone / redone together. Typing,
 * Backspace and Del in a row merge into one group.
 */

#include "hbclass.ch"
#include "cedit.ch"

#define LF  Chr( 10 )

CLASS CEBuffer

EXPORTED:

    DATA aLines
    DATA cFile      INIT ""            /* full path, "" = not saved yet */
    DATA cName      INIT ""            /* name of a buffer without a file */
    DATA cCP        INIT "UTF8"        /* codepage of the file */
    DATA lBOM       INIT .F.
    DATA cEol       INIT Chr( 13 ) + Chr( 10 )
    DATA lFinalEol  INIT .T.           /* the file ends with a line break */
    DATA lReadOnly  INIT .F.
    DATA cError     INIT ""

    DATA nRow       INIT 1             /* cursor */
    DATA nCol       INIT 1
    DATA nTop       INIT 1             /* first line on the screen */
    DATA nLeft      INIT 0             /* first screen cell (horizontal scroll) */
    DATA nWantCell  INIT -1            /* column kept by Up / Down */
    DATA nTab       INIT 4

    DATA nSelMode   INIT CE_SEL_NONE   /* selection from the anchor to the cursor */
    DATA nSelRow    INIT 0
    DATA nSelCol    INIT 0
    DATA lMarking   INIT .F.           /* F3 / Ctrl-F3: cursor keys extend the selection */

    DATA aUndo
    DATA aRedo
    DATA nGroup     INIT 0
    DATA nGroupSeq  INIT 0
    DATA nSavedGroup INIT 0
    DATA cLastKind  INIT ""
    DATA nLastRow   INIT 0
    DATA nLastCol   INIT 0

    METHOD new()
    METHOD load( cFile, cDefCP )
    METHOD save( cFile )
    METHOD setText( cText )
    METHOD getText()
    METHOD title()
    METHOD isModified()
    METHOD lineCount() INLINE Len( ::aLines )
    METHOD line( n ) INLINE IIF( n >= 1 .AND. n <= Len( ::aLines ), ::aLines[ n ], "" )

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
    ::aUndo := {}
    ::aRedo := {}
RETURN self

/* Reads a file. A missing file gives an empty buffer with that name (it is
   made by the first save). .F. and ::cError when the file cannot be read. */
METHOD load( cFile, cDefCP ) CLASS CEBuffer

    LOCAL cRaw

    ::cError := ""
    ::cFile := cFile
    ::cCP := "UTF8"
    ::lBOM := .F.
    ::cEol := hb_eol()
    ::lFinalEol := .T.

    IF ! hb_FileExists( cFile )
        ::setText( "" )
        RETURN .T.
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
    IF ! hb_StrIsUTF8( cRaw )
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

/* Writes the buffer (to cFile, else to ::cFile) in its codepage, line ends
   and BOM. */
METHOD save( cFile ) CLASS CEBuffer

    LOCAL cText

    ::cError := ""
    IF Empty( cFile )
        cFile := ::cFile
    ENDIF
    IF Empty( cFile )
        ::cError := "No file name"
        RETURN .F.
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

/* new text: lines, cursor home, no undo */
METHOD setText( cText ) CLASS CEBuffer

    ::aLines := hb_ATokens( StrTran( StrTran( cText, Chr( 13 ) + Chr( 10 ), LF ), Chr( 13 ), LF ), LF )
    IF Empty( ::aLines )
        ::aLines := { "" }
    ENDIF
    IF Len( ::aLines ) > 1 .AND. ATail( ::aLines ) == ""
        ASize( ::aLines, Len( ::aLines ) - 1 )
        ::lFinalEol := .T.
    ELSE
        /* a new (empty) text gets a line break at the end when saved */
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

RETURN NIL

/* all lines joined with LF */
METHOD getText() CLASS CEBuffer
RETURN ce_Join( ::aLines, LF )

METHOD title() CLASS CEBuffer
    IF ! Empty( ::cFile )
        RETURN hb_FNameNameExt( ::cFile )
    ENDIF
RETURN IIF( Empty( ::cName ), "Untitled", ::cName )

METHOD isModified() CLASS CEBuffer
RETURN IIF( Empty( ::aUndo ), 0, ATail( ::aUndo )[ UR_GROUP ] ) != ::nSavedGroup

/* A user action begins. Typing ("type"), Backspace ("bs") and Del ("del")
   continue the group of the same action right before it, at the same place;
   anything else starts a new group. */
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

/* inserts cText (LF = new line) at nRow, nCol; returns { nEndRow, nEndCol } */
METHOD insertText( nRow, nCol, cText ) CLASS CEBuffer

    LOCAL aEnd, aLast

    IF Len( cText ) == 0
        RETURN { nRow, nCol }
    ENDIF
    aEnd := ce_BufIns( ::aLines, nRow, nCol, cText )

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

/* deletes from nR1, nC1 up to (not including) nR2, nC2; returns the text */
METHOD deleteRange( nR1, nC1, nR2, nC2 ) CLASS CEBuffer

    LOCAL cText, aLast

    IF nR1 > nR2 .OR. ( nR1 == nR2 .AND. nC1 >= nC2 )
        RETURN ""
    ENDIF
    cText := ce_BufDel( ::aLines, nR1, nC1, nR2, nC2 )

    aLast := IIF( Empty( ::aUndo ), NIL, ATail( ::aUndo ) )
    IF aLast != NIL .AND. aLast[ UR_GROUP ] == ::nGroup .AND. aLast[ UR_TYPE ] == "D" .AND. ;
       !( LF $ cText ) .AND. !( LF $ aLast[ UR_TEXT ] ) .AND. aLast[ UR_ROW1 ] == nR1
        IF aLast[ UR_COL1 ] == nC1                           /* Del: the next character */
            aLast[ UR_TEXT ] += cText
            ::aRedo := {}
            RETURN cText
        ELSEIF aLast[ UR_COL1 ] == nC2                       /* Backspace: the previous one */
            aLast[ UR_TEXT ] := cText + aLast[ UR_TEXT ]
            aLast[ UR_COL1 ] := nC1
            ::aRedo := {}
            RETURN cText
        ENDIF
    ENDIF
    AAdd( ::aUndo, { ::nGroup, "D", nR1, nC1, 0, 0, cText, ::nRow, ::nCol } )
    ::aRedo := {}

RETURN cText

/* undoes the last group; .F. when there is nothing to undo */
METHOD undo() CLASS CEBuffer

    LOCAL nGroup, aRec, aDone := {}

    IF Empty( ::aUndo )
        RETURN .F.
    ENDIF
    nGroup := ATail( ::aUndo )[ UR_GROUP ]
    DO WHILE ! Empty( ::aUndo ) .AND. ATail( ::aUndo )[ UR_GROUP ] == nGroup
        aRec := ATail( ::aUndo )
        ASize( ::aUndo, Len( ::aUndo ) - 1 )
        IF aRec[ UR_TYPE ] == "I"
            ce_BufDel( ::aLines, aRec[ UR_ROW1 ], aRec[ UR_COL1 ], aRec[ UR_ROW2 ], aRec[ UR_COL2 ] )
        ELSE
            ce_BufIns( ::aLines, aRec[ UR_ROW1 ], aRec[ UR_COL1 ], aRec[ UR_TEXT ] )
        ENDIF
        ::nRow := aRec[ UR_CURROW ]
        ::nCol := aRec[ UR_CURCOL ]
        AAdd( aDone, aRec )
    ENDDO
    AAdd( ::aRedo, aDone )
    ::clearSel()
    ::cLastKind := ""
    ::nGroup := 0

RETURN .T.

/* redoes the last undone group */
METHOD redo() CLASS CEBuffer

    LOCAL aDone, aRec, aEnd, i

    IF Empty( ::aRedo )
        RETURN .F.
    ENDIF
    aDone := ATail( ::aRedo )
    ASize( ::aRedo, Len( ::aRedo ) - 1 )
    FOR i := Len( aDone ) TO 1 STEP -1
        aRec := aDone[ i ]
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
        AAdd( ::aUndo, aRec )
    NEXT
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

/* stream selection { nR1, nC1, nR2, nC2 } in text order, NIL if none */
METHOD streamRange() CLASS CEBuffer
    IF ! ::hasSel()
        RETURN NIL
    ENDIF
    IF ::nSelRow < ::nRow .OR. ( ::nSelRow == ::nRow .AND. ::nSelCol < ::nCol )
        RETURN { ::nSelRow, ::nSelCol, ::nRow, ::nCol }
    ENDIF
RETURN { ::nRow, ::nCol, ::nSelRow, ::nSelCol }

/* the selected text; a vertical block is cut in screen cells (HTCodeEdit) */
METHOD selText() CLASS CEBuffer

    LOCAL a := ::streamRange()

    IF a == NIL .OR. ::nSelMode != CE_SEL_STREAM
        RETURN ""
    ENDIF

RETURN ce_RangeText( ::aLines, a[ 1 ], a[ 2 ], a[ 3 ], a[ 4 ] )

/*----------------------------------------------------------- helpers */

/* inserts cText into aLines at nRow, nCol; returns the end position */
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

/* deletes nR1, nC1 .. nR2, nC2 (end not included) from aLines; returns the text */
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

/* end position of cText inserted at nRow, nCol */
FUNCTION ce_EndPos( nRow, nCol, cText )

    LOCAL a := hb_ATokens( cText, LF )

    IF Len( a ) == 1
        RETURN { nRow, nCol + Len( a[ 1 ] ) }
    ENDIF

RETURN { nRow + Len( a ) - 1, Len( ATail( a ) ) + 1 }

/* joins an array of strings; halves are joined recursively, because += in a
   loop copies the whole text again for every line of a big file */
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
