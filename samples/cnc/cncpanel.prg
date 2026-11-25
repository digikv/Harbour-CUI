/*
 * CNC - a file panel in the style of Norton Commander
 *
 * Copyright (c) 2026 Srđan Dragojlović - dual licensed, see LICENSE
 *
 *   ╔════════ C:\work\ ════════╗   the path; the active panel's is marked
 *   ║  Name  │Ext│ Size │  Date  ║   full mode, as Total Commander (brief: two columns of
 *                                     names as NC, or brief TC: as many columns as the
 *                                     longest name lets, folders in [ ], scrolled by columns)
 *   ║ ..        │►UP--DIR◄│ ...  ║
 *   ║ src       │►SUB-DIR◄│ ...  ║   folders first, then the files
 *   ║ main.prg  │   1234│ ...   ║
 *   ╟──────────┴──────┴────┴────╢
 *   ║ main.prg   1234 18.11.26 ║   the current file
 *   ╚═ 12,345 bytes in 3 files ═╝   the marked files
 *
 * A click on a column title (Name, Ext, Size, Date) sorts by it, a second
 * click turns the order round; the title shows it with an arrow.
 *
 * The panel moves the cursor, marks files (Ins, right click), goes into
 * folders (Enter, double click) and up (Backspace, Ctrl-PgUp); everything
 * else (copy, delete, view ...) is done by cnc.prg on ::marked().
 *
 * The tree view (CNC_VIEW_TREE, as in Norton Commander) shows the folders of
 * the drive instead of the files: ::cDir is the folder on the cursor (the
 * other panel shows what is in it, cnc.prg), there is no current file. Its
 * methods are tree*(); the other methods give way to them while ::isTree().
 */

#include "hbclass.ch"
#include "inkey.ch"
#include "hbtui.ch"
#include "cnc.ch"

CLASS CNCPanel FROM HTWidget

    DATA cDir        INIT ""
    DATA aFiles      INIT {}
    DATA nCur        INIT 1
    DATA nTop        INIT 1
    DATA nSort       INIT CNC_SORT_NAME
    DATA lDesc       INIT .F.             /* the sort order turned round */
    DATA nView       INIT CNC_VIEW_FULL   /* full: name, size, date, time; brief (NC); brief TC */
    DATA nLongest    INIT 0               /* the widest name in cells (folders with [ ]) */
    DATA lHidden     INIT .F.             /* hidden and system files */
    DATA cQuick                           /* quick search text, NIL = off */
    DATA oNet                             /* CNCFtp: the panel shows an FTP server */
    DATA cLocal      INIT ""              /* the local folder before the FTP connection */
    DATA bEnter                           /* {| oPanel, aFile | } Enter on a file */
    DATA bActivate                        /* {| oPanel | } the panel got the focus */
    DATA bChanged                         /* {| oPanel | } another folder */
    DATA bCursor                          /* {| oPanel | } -> { nRow, nCol }: where the text cursor is shown */
    DATA aRoot                            /* tree view: the root node (cnc.ch T_*) */
    DATA aTree       INIT {}              /* tree view: the rows shown, { aNode, cLines } */

    METHOD new( parent )
    METHOD read( cDir, cSelect )
    METHOD reread()                       INLINE IIF( ::isTree(), ::treeRead( ::cDir, NIL, .T. ), ::read( ::cDir ) )
    METHOD current()
    METHOD marked()
    METHOD markInfo()
    METHOD goName( cName )
    METHOD goUp()
    METHOD enter()
    METHOD markMask( cMask, lOn )
    METHOD invertMarks()
    METHOD sortFiles()
    METHOD sortBy( nSort )
    METHOD isBrief()                      INLINE ! ::isTree() .AND. ( ::nView != CNC_VIEW_FULL .OR. ::Fwidth < 44 )
    METHOD isTree()                       INLINE ::nView == CNC_VIEW_TREE .AND. ::oNet == NIL
    METHOD isColumns()                    INLINE ::nView == CNC_VIEW_TC
    METHOD columns()
    METHOD pageSize()                     INLINE ::rows() * IIF( ::isBrief(), Len( ::columns() ), 1 )
    METHOD briefName( a )
    METHOD rows()                         INLINE Max( 1, ::Fheight - 5 )
    METHOD title()
    METHOD atRoot()                       INLINE IIF( ::oNet == NIL, cnc_IsRoot( ::cDir ), ::cDir == "/" )
    METHOD isNet()                        INLINE ::oNet != NIL
    METHOD sortMark( nCol )
    METHOD ensureVisible()
    METHOD refresh()
    METHOD quickSearch( cText )
    METHOD mouseDragWanted()              INLINE .T.
    METHOD paintEvent( event )
    METHOD keyEvent( keyEvent )
    METHOD mouseEvent( eventMouse )
    METHOD focusInEvent( eventFocus )
    METHOD cursorPos()                    INLINE IIF( HB_ISEVALITEM( ::bCursor ) .AND. ::hasFocus(), Eval( ::bCursor, self ), NIL )

    METHOD treeRead( cDir, cSelect, lAll )
    METHOD treeBuild()
    METHOD treeGo( n )
    METHOD treeOpen( lOpen )
    METHOD treeKey( nKey )
    METHOD treePaint()
    METHOD treeMouse( eventMouse )

ENDCLASS

METHOD new( parent ) CLASS CNCPanel
    ::super:new( parent )
    ::setFocusPolicy( HT_FOCUS_STRONG )
    ::FisVisible := .T.
RETURN self

/* reads a folder; cSelect: the name to put the cursor on. In the same
   folder the marks stay and the cursor stays on its file. */
METHOD read( cDir, cSelect ) CLASS CNCPanel

    LOCAL aFiles := {}, hMarks := { => }, a, lSame, nOld := ::nCur, aDir

    IF ::isTree()
        RETURN ::treeRead( cDir, cSelect )
    ENDIF
    IF ::oNet != NIL                              /* FTP: "/pub/" */
        cDir := cnc_FtpDir( cDir )
        IF ( aDir := ::oNet:dir( cDir ) ) == NIL
            RETURN .F.
        ENDIF
    ELSE
        cDir := hb_DirSepAdd( hb_DirSepToOS( cDir ) )
        IF ! hb_DirExists( cDir )
            RETURN .F.
        ENDIF
        aDir := {}
        FOR EACH a IN Directory( cDir + hb_osFileMask(), IIF( ::lHidden, "DHS", "D" ) )
            IF !( a[ 1 ] == "." .OR. a[ 1 ] == ".." )
                AAdd( aDir, { a[ 1 ], a[ 2 ], a[ 3 ], a[ 4 ], a[ 5 ], "D" $ a[ 5 ], .F. } )
            ENDIF
        NEXT
    ENDIF
    lSame := cnc_SamePath( cDir, ::cDir )
    IF lSame
        AEval( ::aFiles, {| x | IIF( x[ F_MARK ], hMarks[ x[ F_NAME ] ] := .T., NIL ) } )
        IF cSelect == NIL .AND. ::nCur >= 1 .AND. ::nCur <= Len( ::aFiles )
            cSelect := ::aFiles[ ::nCur ][ F_NAME ]
        ENDIF
    ENDIF
    IF !( IIF( ::oNet == NIL, cnc_IsRoot( cDir ), cDir == "/" ) )
        AAdd( aFiles, { "..", 0, CToD( "" ), "", "D", .T., .F. } )
    ENDIF
    FOR EACH a IN aDir
        a[ F_MARK ] := hb_HHasKey( hMarks, a[ F_NAME ] )
        AAdd( aFiles, a )
    NEXT
    ::cDir := cDir
    ::aFiles := aFiles
    ::nLongest := 0
    FOR EACH a IN aFiles
        ::nLongest := Max( ::nLongest, ht_strWidth( a[ F_NAME ] ) + IIF( a[ F_DIR ], 2, 0 ) )
    NEXT
    ::sortFiles()
    ::nCur := 1
    ::nTop := 1
    IF cSelect != NIL .AND. ! ::goName( cSelect ) .AND. lSame
        ::nCur := Max( 1, Min( nOld, Len( ::aFiles ) ) )   /* the file is gone */
    ENDIF
    ::ensureVisible()
    ::cQuick := NIL
    IF HB_ISEVALITEM( ::bChanged )
        Eval( ::bChanged, self )
    ENDIF

RETURN .T.

/* ".." first, then the folders, then the files */
METHOD sortFiles() CLASS CNCPanel
    LOCAL nSort := ::nSort, lDesc := ::lDesc
    ASort( ::aFiles, , , {| a, b | cnc_Less( a, b, nSort, lDesc ) } )
RETURN NIL

/* another sort order; the same one again turns it round. The cursor stays
   on its file. */
METHOD sortBy( nSort ) CLASS CNCPanel

    LOCAL a := ::current()

    IF nSort == ::nSort
        ::lDesc := ! ::lDesc
    ELSE
        ::nSort := nSort
        ::lDesc := .F.
    ENDIF
    ::sortFiles()
    IF a != NIL
        ::goName( a[ F_NAME ] )
    ENDIF
    ::ensureVisible()
    ::refresh()

RETURN NIL

STATIC FUNCTION cnc_Less( a, b, nSort, lDesc )

    IF a[ F_NAME ] == ".."
        RETURN .T.
    ELSEIF b[ F_NAME ] == ".."
        RETURN .F.
    ELSEIF a[ F_DIR ] != b[ F_DIR ]
        RETURN a[ F_DIR ]                         /* the folders stay first */
    ENDIF

RETURN IIF( lDesc, cnc_Before( b, a, nSort ), cnc_Before( a, b, nSort ) )

/* a before b in the order nSort (folders: always by name) */
STATIC FUNCTION cnc_Before( a, b, nSort )

    LOCAL cA, cB
    IF ! a[ F_DIR ]
        SWITCH nSort
        CASE CNC_SORT_EXT
            cA := Lower( hb_FNameExt( a[ F_NAME ] ) )
            cB := Lower( hb_FNameExt( b[ F_NAME ] ) )
            IF !( cA == cB )
                RETURN cA < cB
            ENDIF
            EXIT
        CASE CNC_SORT_TIME                         /* the newest first */
            cA := DToS( a[ F_DATE ] ) + a[ F_TIME ]
            cB := DToS( b[ F_DATE ] ) + b[ F_TIME ]
            IF !( cA == cB )
                RETURN cA > cB
            ENDIF
            EXIT
        CASE CNC_SORT_SIZE                         /* the biggest first */
            IF a[ F_SIZE ] != b[ F_SIZE ]
                RETURN a[ F_SIZE ] > b[ F_SIZE ]
            ENDIF
            EXIT
        ENDSWITCH
    ENDIF

RETURN Lower( a[ F_NAME ] ) < Lower( b[ F_NAME ] )

METHOD current() CLASS CNCPanel
RETURN IIF( ::nCur >= 1 .AND. ::nCur <= Len( ::aFiles ), ::aFiles[ ::nCur ], NIL )

/* the marked entries, else the current one (never "..") */
METHOD marked() CLASS CNCPanel

    LOCAL aList := {}, a

    FOR EACH a IN ::aFiles
        IF a[ F_MARK ]
            AAdd( aList, a )
        ENDIF
    NEXT
    IF Empty( aList ) .AND. ( a := ::current() ) != NIL .AND. !( a[ F_NAME ] == ".." )
        AAdd( aList, a )
    ENDIF

RETURN aList

/* { nMarked, nBytes } */
METHOD markInfo() CLASS CNCPanel

    LOCAL n := 0, nBytes := 0, a

    FOR EACH a IN ::aFiles
        IF a[ F_MARK ]
            n++
            nBytes += a[ F_SIZE ]
        ENDIF
    NEXT

RETURN { n, nBytes }

METHOD goName( cName ) CLASS CNCPanel

    LOCAL n := AScan( ::aFiles, {| a | a[ F_NAME ] == cName } )

    IF n == 0
        n := AScan( ::aFiles, {| a | Lower( a[ F_NAME ] ) == Lower( cName ) } )
    ENDIF
    IF n > 0
        ::nCur := n
        ::ensureVisible()
    ENDIF

RETURN n > 0

/* the parent folder, the cursor on the folder we came from */
METHOD goUp() CLASS CNCPanel

    LOCAL cOld

    IF ::atRoot()
        RETURN .F.
    ENDIF
    IF ::oNet != NIL
        cOld := SubStr( ::cDir, RAt( "/", Left( ::cDir, Len( ::cDir ) - 1 ) ) + 1 )
        RETURN ::read( cnc_FtpParent( ::cDir ), Left( cOld, Len( cOld ) - 1 ) )
    ENDIF
    cOld := hb_FNameNameExt( hb_DirSepDel( ::cDir ) )

RETURN ::read( cnc_ParentDir( ::cDir ), cOld )

/* Enter: into a folder, or the program's action for a file */
METHOD enter() CLASS CNCPanel

    LOCAL a := ::current()

    IF a == NIL
        RETURN NIL
    ENDIF
    IF a[ F_NAME ] == ".."
        ::goUp()
    ELSEIF a[ F_DIR ]
        IF ! ::read( ::cDir + a[ F_NAME ] )
            HTMessageBox():error( "CNC", IIF( ::oNet == NIL, "Cannot open the folder " + a[ F_NAME ], ::oNet:cErr ) )
        ENDIF
    ELSEIF HB_ISEVALITEM( ::bEnter )
        Eval( ::bEnter, self, a )
    ENDIF
    ::refresh()

RETURN NIL

/* + / - : mark or unmark the files that fit a mask ("*.prg", "a*") */
METHOD markMask( cMask, lOn ) CLASS CNCPanel

    LOCAL a, n := 0

    FOR EACH a IN ::aFiles
        IF !( a[ F_NAME ] == ".." ) .AND. ! a[ F_DIR ] .AND. hb_FileMatch( a[ F_NAME ], cMask )
            a[ F_MARK ] := lOn
            n++
        ENDIF
    NEXT
    ::refresh()

RETURN n

METHOD invertMarks() CLASS CNCPanel
    LOCAL a
    FOR EACH a IN ::aFiles
        IF ! a[ F_DIR ]
            a[ F_MARK ] := ! a[ F_MARK ]
        ENDIF
    NEXT
    ::refresh()
RETURN NIL

/* the arrow after the title of the column the panel is sorted by: up in the
   usual order (A..Z, the newest, the biggest first), down turned round */
METHOD sortMark( nCol ) CLASS CNCPanel
    IF ::nSort == nCol .OR. ( nCol == CNC_SORT_NAME .AND. ::nSort == CNC_SORT_EXT .AND. ::isBrief() )
        RETURN IIF( ::lDesc, ht_glyph( 0x2193, "v" ), ht_glyph( 0x2191, "^" ) )
    ENDIF
RETURN ""

METHOD title() CLASS CNCPanel

    LOCAL c := IIF( ::oNet == NIL, ::cDir, ::oNet:url( ::cDir ) ), nMax := Max( 6, ::Fwidth - 6 )

    IF Len( c ) > nMax
        c := "…" + Right( c, nMax - 1 )
    ENDIF

RETURN c

/* the columns of names in the brief views: { { nCol, nWidth }, ... } between
   the frame lines, a │ after each but the last; full view: one */
METHOD columns() CLASS CNCPanel

    LOCAL nIn := ::Fwidth - 2, nCols := 2, nW, nX := 1, aCol := {}, i

    IF ! ::isBrief()
        RETURN { { 1, nIn } }
    ENDIF
    IF ::isColumns()                     /* Total Commander: the longest name decides */
        nW := Max( 8, Min( ::nLongest, 40 ) )
        nCols := Max( 1, Int( ( nIn + 1 ) / ( nW + 1 ) ) )
    ENDIF
    nW := Int( ( nIn - ( nCols - 1 ) ) / nCols )
    FOR i := 1 TO nCols
        AAdd( aCol, { nX, IIF( i == nCols, nIn - nX + 1, nW ) } )
        nX += nW + 1
    NEXT

RETURN aCol

/* the name in a brief view; Total Commander puts folders in [ ] */
METHOD briefName( a ) CLASS CNCPanel
RETURN IIF( ::isColumns() .AND. a[ F_DIR ], "[" + a[ F_NAME ] + "]", a[ F_NAME ] )

METHOD ensureVisible() CLASS CNCPanel

    LOCAL nPage := ::pageSize(), nRows := ::rows(), nColTop

    ::nCur := Max( 1, Min( ::nCur, IIF( ::isTree(), Len( ::aTree ), Len( ::aFiles ) ) ) )
    IF ::isBrief() .AND. ::isColumns()   /* scrolled by whole columns */
        nColTop := Int( ( ::nCur - 1 ) / nRows ) * nRows + 1
        IF ::nCur < ::nTop
            ::nTop := nColTop
        ELSEIF ::nCur >= ::nTop + nPage
            ::nTop := nColTop - nPage + nRows
        ENDIF
        ::nTop := Int( ( Max( 1, ::nTop ) - 1 ) / nRows ) * nRows + 1
        RETURN NIL
    ENDIF
    IF ::nCur < ::nTop
        ::nTop := ::nCur
    ELSEIF ::nCur >= ::nTop + nPage
        ::nTop := ::nCur - nPage + 1
    ENDIF
    ::nTop := Max( 1, ::nTop )

RETURN NIL

METHOD refresh() CLASS CNCPanel

    LOCAL parent := ::parent()

    IF parent != NIL .AND. parent:isDerivedFrom( "HTWidget" )
        parent:repaintChild( self )
    ENDIF

RETURN NIL

/* quick search: the cursor on the first name that starts with cText */
METHOD quickSearch( cText ) CLASS CNCPanel

    LOCAL n

    ::cQuick := cText
    IF ! Empty( cText ) .AND. ::isTree()
        IF ( n := AScan( ::aTree, {| r | Lower( Left( r[ 1 ][ T_NAME ], Len( cText ) ) ) == Lower( cText ) } ) ) > 0
            ::treeGo( n )
        ENDIF
    ELSEIF ! Empty( cText )
        n := AScan( ::aFiles, {| a | Lower( Left( a[ F_NAME ], Len( cText ) ) ) == Lower( cText ) } )
        IF n > 0
            ::nCur := n
            ::ensureVisible()
        ENDIF
    ENDIF
    ::refresh()

RETURN NIL

METHOD PROCEDURE focusInEvent( eventFocus ) CLASS CNCPanel
    ::super:focusInEvent( eventFocus )
    IF HB_ISEVALITEM( ::bActivate )
        Eval( ::bActivate, self )
    ENDIF
RETURN

/*----------------------------------------------------------- painting */

METHOD PROCEDURE paintEvent( event ) CLASS CNCPanel

    LOCAL nW := ::Fwidth, nH := ::Fheight, nRows := ::rows(), lBrief := ::isBrief()
    LOCAL lFocus := ::hasFocus(), nNameW, aCol, k, r, i, a, c, aInfo, cTitle, aNE
    LOCAL cH := ht_glyph( 0x2550, "=" ), cV := ht_glyph( 0x2551, "|" ), cS := ht_glyph( 0x2502, "|" )

    HB_SYMBOL_UNUSED( event )
    IF nW < 12 .OR. nH < 7
        RETURN
    ENDIF

    /* the frame and the path */
    DispOutAt( 0, 0, ht_glyph( 0x2554, "+" ) + Replicate( cH, nW - 2 ) + ht_glyph( 0x2557, "+" ), CNC_CLR_PANEL )
    cTitle := " " + ::title() + " "
    DispOutAt( 0, Max( 1, Int( ( nW - ht_strWidth( cTitle ) ) / 2 ) ), cTitle, IIF( lFocus, CNC_CLR_PATH, CNC_CLR_PANEL ) )
    FOR r := 1 TO nH - 2
        DispOutAt( r, 0, cV, CNC_CLR_PANEL )
        DispOutAt( r, nW - 1, cV, CNC_CLR_PANEL )
    NEXT
    DispOutAt( nH - 1, 0, ht_glyph( 0x255A, "+" ) + Replicate( cH, nW - 2 ) + ht_glyph( 0x255D, "+" ), CNC_CLR_PANEL )

    IF ::isTree()
        ::treePaint()
        RETURN
    ENDIF
    IF lBrief
        aCol := ::columns()
        c := ht_glyph( 0x255F, "|" )
        FOR EACH k IN aCol
            DispOutAt( 1, k[ 1 ], cnc_Center( "Name" + ::sortMark( CNC_SORT_NAME ), k[ 2 ] ), CNC_CLR_HEAD )
            c += Replicate( ht_glyph( 0x2500, "-" ), k[ 2 ] )
            IF ! k:__enumIsLast()
                DispOutAt( 1, k[ 1 ] + k[ 2 ], cS, CNC_CLR_PANEL )
                c += ht_glyph( 0x2534, "-" )
            ENDIF
        NEXT
        FOR r := 1 TO nRows
            FOR EACH k IN aCol
                i := ::nTop + ( k:__enumIndex() - 1 ) * nRows + r - 1
                IF i <= Len( ::aFiles )
                    DispOutAt( r + 1, k[ 1 ], cnc_Fit( ::briefName( ::aFiles[ i ] ), k[ 2 ] ), cnc_RowColor( ::aFiles[ i ], i == ::nCur .AND. lFocus ) )
                ELSE
                    DispOutAt( r + 1, k[ 1 ], Space( k[ 2 ] ), CNC_CLR_PANEL )
                ENDIF
                IF ! k:__enumIsLast()
                    DispOutAt( r + 1, k[ 1 ] + k[ 2 ], cS, CNC_CLR_PANEL )
                ENDIF
            NEXT
        NEXT
        DispOutAt( nH - 3, 0, c + ht_glyph( 0x2562, "|" ), CNC_CLR_PANEL )
    ELSE
        /* Name │ Ext │ Size │ Date (as Total Commander): 5 + 10 + 14 cells and 3 lines */
        nNameW := nW - 2 - 32
        DispOutAt( 1, 1, cnc_Center( "Name" + ::sortMark( CNC_SORT_NAME ), nNameW ) + cS + ;
                   cnc_Center( "Ext" + ::sortMark( CNC_SORT_EXT ), 5 ) + cS + ;
                   cnc_Center( "Size" + ::sortMark( CNC_SORT_SIZE ), 10 ) + cS + ;
                   cnc_Center( "Date" + ::sortMark( CNC_SORT_TIME ), 14 ), CNC_CLR_HEAD )
        AEval( { nNameW + 1, nNameW + 7, nNameW + 18 }, {| x | DispOutAt( 1, x, cS, CNC_CLR_PANEL ) } )
        FOR r := 1 TO nRows
            i := ::nTop + r - 1
            IF i <= Len( ::aFiles )
                a := ::aFiles[ i ]
                c := cnc_RowColor( a, i == ::nCur .AND. lFocus )
                aNE := cnc_NameExt( a )
                DispOutAt( r + 1, 1, cnc_Fit( aNE[ 1 ], nNameW ) + cS + cnc_Fit( aNE[ 2 ], 5 ) + cS + cnc_SizeText( a, 10 ) + cS + ;
                           cnc_DateText( a[ F_DATE ] ) + " " + PadR( Left( a[ F_TIME ], 5 ), 5 ), c )
                IF !( i == ::nCur .AND. lFocus )
                    AEval( { nNameW + 1, nNameW + 7, nNameW + 18 }, {| x | DispOutAt( r + 1, x, cS, CNC_CLR_PANEL ) } )
                ENDIF
            ELSE
                DispOutAt( r + 1, 1, Space( nNameW ) + cS + Space( 5 ) + cS + Space( 10 ) + cS + Space( 14 ), CNC_CLR_PANEL )
            ENDIF
        NEXT
        c := ht_glyph( 0x2500, "-" )
        DispOutAt( nH - 3, 0, ht_glyph( 0x255F, "|" ) + Replicate( c, nNameW ) + ht_glyph( 0x2534, "-" ) + Replicate( c, 5 ) + ;
                   ht_glyph( 0x2534, "-" ) + Replicate( c, 10 ) + ht_glyph( 0x2534, "-" ) + Replicate( c, 14 ) + ht_glyph( 0x2562, "|" ), CNC_CLR_PANEL )
    ENDIF

    /* the current file */
    a := ::current()
    IF a != NIL
        nNameW := Max( 4, nW - 2 - 26 )
        c := cnc_Fit( a[ F_NAME ], nNameW ) + " " + cnc_SizeText( a, 10 ) + " " + cnc_DateText( a[ F_DATE ] ) + " " + PadR( Left( a[ F_TIME ], 5 ), 5 )
        DispOutAt( nH - 2, 1, cnc_Fit( c, nW - 2 ), CNC_CLR_PANEL )
    ELSE
        DispOutAt( nH - 2, 1, Space( nW - 2 ), CNC_CLR_PANEL )
    ENDIF

    /* the marked files, or the quick search */
    aInfo := ::markInfo()
    IF ::cQuick != NIL
        c := " Search: " + ::cQuick + " "
        DispOutAt( nH - 1, 2, cnc_Fit( c, nW - 4 ), CNC_CLR_CURSOR )
    ELSEIF aInfo[ 1 ] > 0
        c := " " + cnc_Thousands( aInfo[ 2 ] ) + " bytes in " + hb_ntos( aInfo[ 1 ] ) + " selected file" + IIF( aInfo[ 1 ] == 1, "", "s" ) + " "
        c := cnc_Fit( c, nW - 4 )
        DispOutAt( nH - 1, Max( 1, Int( ( nW - ht_strWidth( c ) ) / 2 ) ), c, CNC_CLR_MARK )
    ENDIF

RETURN

/* { name, extension } of a file for the Name and Ext columns; a folder, "..",
   ".gitignore" (a name that only starts with a dot) stay whole */
STATIC FUNCTION cnc_NameExt( a )

    LOCAL cName := a[ F_NAME ], n

    IF a[ F_DIR ]
        RETURN { cName, "" }
    ENDIF
    n := RAt( ".", cName )
    IF n <= 1 .OR. n == Len( cName )
        RETURN { cName, "" }
    ENDIF

RETURN { Left( cName, n - 1 ), SubStr( cName, n + 1 ) }

STATIC FUNCTION cnc_RowColor( a, lCursor )
    IF lCursor
        RETURN IIF( a[ F_MARK ], CNC_CLR_CURMARK, CNC_CLR_CURSOR )
    ENDIF
RETURN IIF( a[ F_MARK ], CNC_CLR_MARK, IIF( a[ F_DIR ], CNC_CLR_DIR, CNC_CLR_PANEL ) )

/* c in exactly n screen cells; a longer one ends with » */
FUNCTION cnc_Fit( c, n )
    IF n <= 0
        RETURN ""
    ELSEIF ht_strWidth( c ) > n
        RETURN ht_cellPadR( ht_cellLeft( c, n - 1 ), n - 1 ) + ht_glyph( 0x00BB, ">" )
    ENDIF
RETURN ht_cellPadR( c, n )

STATIC FUNCTION cnc_Center( c, n )
RETURN PadR( Space( Int( ( n - Len( c ) ) / 2 ) ) + c, n )

STATIC FUNCTION cnc_SizeText( a, n )
    IF a[ F_NAME ] == ".."
        RETURN PadL( ht_glyph( 0x25BA, ">" ) + "UP--DIR" + ht_glyph( 0x25C4, "<" ), n )
    ELSEIF a[ F_DIR ]
        RETURN PadL( ht_glyph( 0x25BA, ">" ) + "SUB-DIR" + ht_glyph( 0x25C4, "<" ), n )
    ENDIF
RETURN cnc_SizeNum( a[ F_SIZE ], n )

/* 1234567 -> "1234567"; too long: K, M, G */
FUNCTION cnc_SizeNum( nSize, n )

    LOCAL c := hb_ntos( nSize ), aUnit := { "K", "M", "G", "T" }, i := 0

    DO WHILE Len( c ) > n .AND. i < Len( aUnit )
        i++
        c := hb_ntos( Int( nSize / ( 1024 ^ i ) ) ) + aUnit[ i ]
    ENDDO

RETURN PadL( c, n )

FUNCTION cnc_DateText( d )
    IF Empty( d )
        RETURN Space( 8 )
    ENDIF
RETURN StrZero( Day( d ), 2 ) + "." + StrZero( Month( d ), 2 ) + "." + Right( StrZero( Year( d ), 4 ), 2 )

/* 1234567 -> "1,234,567" */
FUNCTION cnc_Thousands( n )

    LOCAL c := hb_ntos( Int( n ) ), cOut := ""

    DO WHILE Len( c ) > 3
        cOut := "," + Right( c, 3 ) + cOut
        c := Left( c, Len( c ) - 3 )
    ENDDO

RETURN c + cOut

/*----------------------------------------------------------- keys, mouse */

METHOD PROCEDURE keyEvent( keyEvent ) CLASS CNCPanel

    LOCAL nKey := keyEvent:key, nRows := ::rows(), nPage := ::pageSize(), a

    /* quick search: letters go on, Backspace takes one back, other keys end it */
    IF ::cQuick != NIL
        DO CASE
        CASE nKey == K_BS
            ::quickSearch( hb_StrShrink( ::cQuick, 1 ) )
            keyEvent:accept()
            RETURN
        CASE nKey == K_ESC
            ::cQuick := NIL
            ::refresh()
            keyEvent:accept()
            RETURN
        CASE nKey >= 32 .AND. nKey < 256 .OR. nKey > 1000000      /* a character (also Unicode) */
            ::quickSearch( ::cQuick + hb_keyChar( nKey ) )
            keyEvent:accept()
            RETURN
        ENDCASE
        ::cQuick := NIL
    ENDIF

    IF ::isTree()
        IF ::treeKey( nKey )
            keyEvent:accept()
        ENDIF
        RETURN
    ENDIF

    SWITCH nKey
    CASE K_UP
        ::nCur--
        EXIT
    CASE K_DOWN
        ::nCur++
        EXIT
    CASE K_PGUP
        ::nCur -= nPage - 1
        EXIT
    CASE K_PGDN
        ::nCur += nPage - 1
        EXIT
    CASE K_HOME
        ::nCur := 1
        EXIT
    CASE K_END
        ::nCur := Len( ::aFiles )
        EXIT
    CASE K_LEFT
        IF ! ::isBrief()
            RETURN
        ENDIF
        ::nCur -= nRows
        EXIT
    CASE K_RIGHT
        IF ! ::isBrief()
            RETURN
        ENDIF
        ::nCur += nRows
        EXIT
    CASE K_INS
        a := ::current()
        IF a != NIL .AND. !( a[ F_NAME ] == ".." )
            a[ F_MARK ] := ! a[ F_MARK ]
        ENDIF
        ::nCur++
        EXIT
    CASE K_ENTER
        keyEvent:accept()
        ::enter()
        RETURN
    CASE K_BS
    CASE K_CTRL_PGUP
        keyEvent:accept()
        ::goUp()
        ::refresh()
        RETURN
    CASE K_CTRL_PGDN
        a := ::current()
        IF a != NIL .AND. a[ F_DIR ]
            keyEvent:accept()
            ::enter()
        ENDIF
        RETURN
    OTHERWISE
        RETURN                                   /* not ours: the program's */
    ENDSWITCH

    keyEvent:accept()
    ::ensureVisible()
    ::refresh()

RETURN

METHOD PROCEDURE mouseEvent( eventMouse ) CLASS CNCPanel

    LOCAL nR := eventMouse:mouseRow, nC := eventMouse:mouseCol, nRows := ::rows(), i := 0, k

    IF ::isTree()
        ::treeMouse( eventMouse )
        RETURN
    ENDIF
    /* the entry under the mouse (a │ belongs to the column on its left) */
    IF nR >= 2 .AND. nR <= nRows + 1 .AND. nC >= 1 .AND. nC < ::Fwidth - 1
        i := ::nTop + nR - 2
        FOR EACH k IN ::columns()
            IF nC >= k[ 1 ]
                i := ::nTop + ( k:__enumIndex() - 1 ) * nRows + nR - 2
            ENDIF
        NEXT
        IF i > Len( ::aFiles )
            i := 0
        ENDIF
    ENDIF

    SWITCH eventMouse:nKey
    CASE K_LBUTTONDOWN
        IF nR == 1 .AND. nC >= 1 .AND. nC < ::Fwidth - 1      /* the column titles */
            /* Name │ Ext │ Size │ Date: the lines at width - 33, - 27, - 16 */
            DO CASE
            CASE ::isBrief() .OR. nC <= ::Fwidth - 34
                ::sortBy( CNC_SORT_NAME )
            CASE nC <= ::Fwidth - 28
                ::sortBy( CNC_SORT_EXT )
            CASE nC <= ::Fwidth - 17
                ::sortBy( CNC_SORT_SIZE )
            OTHERWISE
                ::sortBy( CNC_SORT_TIME )
            ENDCASE
            EXIT
        ENDIF
        IF i > 0
            ::nCur := i
            ::cQuick := NIL
            ::refresh()
        ENDIF
        EXIT
    CASE K_LDBLCLK
        IF i > 0
            ::nCur := i
            ::enter()
        ENDIF
        EXIT
    CASE K_RBUTTONDOWN                          /* NC: the right button marks */
        IF i > 0 .AND. !( ::aFiles[ i ][ F_NAME ] == ".." )
            ::nCur := i
            ::aFiles[ i ][ F_MARK ] := ! ::aFiles[ i ][ F_MARK ]
            ::refresh()
        ENDIF
        EXIT
    CASE K_MWFORWARD
        ::nCur -= 3
        ::ensureVisible()
        ::refresh()
        EXIT
    CASE K_MWBACKWARD
        ::nCur += 3
        ::ensureVisible()
        ::refresh()
        EXIT
    ENDSWITCH

RETURN

/*----------------------------------------------------------- the tree view */

/* the tree of the drive (Norton Commander's Tree panel): the folders only,
   the way to ::cDir open, the cursor on it. cSelect: a folder in cDir to put
   the cursor on (F7 made it); lAll: read every open folder again (Ctrl-R).
   A folder is read when it is opened, never before. */
METHOD treeRead( cDir, cSelect, lAll ) CLASS CNCPanel

    LOCAL cTarget, cRoot, aNode, aKid, cPart, n

    cDir := hb_DirSepAdd( hb_DirSepToOS( cDir ) )
    IF ! hb_DirExists( cDir )
        RETURN .F.
    ENDIF
    cTarget := cDir
    IF ! Empty( cSelect ) .AND. hb_DirExists( cDir + cSelect )
        cTarget := hb_DirSepAdd( cDir + cSelect )
    ENDIF
    cRoot := cnc_RootOf( cTarget )
    IF ::aRoot == NIL .OR. ! cnc_SamePath( ::aRoot[ T_PATH ], cRoot )
        ::aRoot := { cRoot, cRoot, NIL, .T. }
    ELSEIF hb_defaultValue( lAll, .F. )
        cnc_TreeRefresh( ::aRoot, ::lHidden )
    ENDIF

    /* open the way from the root to the folder */
    aNode := ::aRoot
    FOR EACH cPart IN hb_ATokens( SubStr( cTarget, Len( cRoot ) + 1 ), hb_ps() )
        IF Empty( cPart )
            LOOP
        ENDIF
        IF aNode[ T_KIDS ] == NIL
            aNode[ T_KIDS ] := cnc_TreeKids( aNode[ T_PATH ], ::lHidden, NIL )
        ENDIF
        IF ( n := cnc_TreeFind( aNode[ T_KIDS ], cPart ) ) == 0      /* a new folder: read again */
            aNode[ T_KIDS ] := cnc_TreeKids( aNode[ T_PATH ], ::lHidden, aNode[ T_KIDS ] )
            n := cnc_TreeFind( aNode[ T_KIDS ], cPart )
        ENDIF
        IF n == 0                                                     /* hidden, no rights ... */
            EXIT
        ENDIF
        aNode[ T_OPEN ] := .T.
        aKid := aNode[ T_KIDS ][ n ]
        aNode := aKid
    NEXT

    ::treeBuild()
    ::nCur := Max( 1, AScan( ::aTree, {| r | r[ 1 ] == aNode } ) )
    ::cDir := aNode[ T_PATH ]
    ::aFiles := {}
    ::ensureVisible()
    ::cQuick := NIL
    IF HB_ISEVALITEM( ::bChanged )
        Eval( ::bChanged, self )
    ENDIF

RETURN .T.

/* the rows shown: { aNode, cLines } - the open folders with their subfolders */
METHOD treeBuild() CLASS CNCPanel
    ::aTree := {}
    cnc_TreeRows( ::aTree, ::aRoot, "", .T., .T. )
RETURN NIL

/* the cursor on row n: that folder is the panel's folder now */
METHOD treeGo( n ) CLASS CNCPanel

    ::nCur := Max( 1, Min( n, Len( ::aTree ) ) )
    ::ensureVisible()
    IF ::nCur <= Len( ::aTree ) .AND. ! cnc_SamePath( ::cDir, ::aTree[ ::nCur ][ 1 ][ T_PATH ] )
        ::cDir := ::aTree[ ::nCur ][ 1 ][ T_PATH ]
        IF HB_ISEVALITEM( ::bChanged )
            Eval( ::bChanged, self )
        ENDIF
    ENDIF
    ::refresh()

RETURN NIL

/* open (lOpen .T.), close (.F.) or turn round (NIL) the folder on the cursor */
METHOD treeOpen( lOpen ) CLASS CNCPanel

    LOCAL aNode

    IF ::nCur < 1 .OR. ::nCur > Len( ::aTree )
        RETURN NIL
    ENDIF
    aNode := ::aTree[ ::nCur ][ 1 ]
    IF lOpen == NIL
        lOpen := ! aNode[ T_OPEN ]
    ENDIF
    IF lOpen .AND. aNode[ T_KIDS ] == NIL
        aNode[ T_KIDS ] := cnc_TreeKids( aNode[ T_PATH ], ::lHidden, NIL )
    ENDIF
    aNode[ T_OPEN ] := lOpen .AND. ! Empty( aNode[ T_KIDS ] )
    ::treeBuild()
    ::nCur := Max( 1, AScan( ::aTree, {| r | r[ 1 ] == aNode } ) )
    ::ensureVisible()
    ::refresh()

RETURN NIL

/* keys of the tree; .F.: not a tree key (the program's) */
METHOD treeKey( nKey ) CLASS CNCPanel

    LOCAL aNode := IIF( ::nCur >= 1 .AND. ::nCur <= Len( ::aTree ), ::aTree[ ::nCur ][ 1 ], NIL ), n

    SWITCH nKey
    CASE K_UP       ; ::treeGo( ::nCur - 1 ) ; EXIT
    CASE K_DOWN     ; ::treeGo( ::nCur + 1 ) ; EXIT
    CASE K_PGUP     ; ::treeGo( ::nCur - ::rows() + 1 ) ; EXIT
    CASE K_PGDN     ; ::treeGo( ::nCur + ::rows() - 1 ) ; EXIT
    CASE K_HOME     ; ::treeGo( 1 ) ; EXIT
    CASE K_END      ; ::treeGo( Len( ::aTree ) ) ; EXIT
    CASE K_ENTER
        ::treeOpen()
        EXIT
    CASE K_RIGHT                         /* open; open already: the first subfolder */
    CASE K_CTRL_PGDN
        IF aNode != NIL .AND. aNode[ T_OPEN ]
            ::treeGo( ::nCur + 1 )
        ELSE
            ::treeOpen( .T. )
        ENDIF
        EXIT
    CASE K_LEFT                          /* close; closed: the folder above it */
    CASE K_BS
    CASE K_CTRL_PGUP
        IF aNode != NIL .AND. aNode[ T_OPEN ] .AND. nKey == K_LEFT
            ::treeOpen( .F. )
        ELSEIF aNode != NIL .AND. ( n := cnc_TreeParentRow( ::aTree, ::nCur ) ) > 0
            ::treeGo( n )
        ENDIF
        EXIT
    OTHERWISE
        RETURN .F.
    ENDSWITCH

RETURN .T.

METHOD PROCEDURE treePaint() CLASS CNCPanel

    LOCAL nW := ::Fwidth, nH := ::Fheight, nRows := ::rows(), lFocus := ::hasFocus(), r, i, a, c, cClr

    DispOutAt( 1, 1, cnc_Center( "Tree", nW - 2 ), CNC_CLR_HEAD )
    FOR r := 1 TO nRows
        i := ::nTop + r - 1
        IF i <= Len( ::aTree )
            a := ::aTree[ i ]
            c := a[ 2 ] + cnc_TreeMark( a[ 1 ] ) + a[ 1 ][ T_NAME ]
            DO CASE
            CASE i == ::nCur .AND. lFocus
                cClr := CNC_CLR_CURSOR
            CASE i == ::nCur                       /* the folder the other panel shows */
                cClr := CNC_CLR_MARK
            OTHERWISE
                cClr := CNC_CLR_DIR
            ENDCASE
            DispOutAt( r + 1, 1, cnc_Fit( " " + c, nW - 2 ), cClr )
        ELSE
            DispOutAt( r + 1, 1, Space( nW - 2 ), CNC_CLR_PANEL )
        ENDIF
    NEXT
    DispOutAt( nH - 3, 0, ht_glyph( 0x255F, "|" ) + Replicate( ht_glyph( 0x2500, "-" ), nW - 2 ) + ht_glyph( 0x2562, "|" ), CNC_CLR_PANEL )
    DispOutAt( nH - 2, 1, cnc_Fit( ::cDir, nW - 2 ), CNC_CLR_PANEL )
    IF ::cQuick != NIL
        DispOutAt( nH - 1, 2, cnc_Fit( " Search: " + ::cQuick + " ", nW - 4 ), CNC_CLR_CURSOR )
    ENDIF

RETURN

METHOD PROCEDURE treeMouse( eventMouse ) CLASS CNCPanel

    LOCAL nR := eventMouse:mouseRow, i := 0

    IF nR >= 2 .AND. nR <= ::rows() + 1 .AND. ::nTop + nR - 2 <= Len( ::aTree )
        i := ::nTop + nR - 2
    ENDIF
    SWITCH eventMouse:nKey
    CASE K_LBUTTONDOWN
        IF i > 0
            ::cQuick := NIL
            ::treeGo( i )
        ENDIF
        EXIT
    CASE K_LDBLCLK
        IF i > 0
            ::treeGo( i )
            ::treeOpen()
        ENDIF
        EXIT
    CASE K_MWFORWARD
        ::treeGo( ::nCur - 3 )
        EXIT
    CASE K_MWBACKWARD
        ::treeGo( ::nCur + 3 )
        EXIT
    ENDSWITCH

RETURN

/* "C:\work\src\" -> "C:\", "\\pc\share\a\" -> "\\pc\share\", "/home/x/" -> "/" */
STATIC FUNCTION cnc_RootOf( cDir )

    LOCAL a

    IF Left( cDir, 2 ) == "\\"
        a := hb_ATokens( SubStr( cDir, 3 ), "\" )
        RETURN "\\" + a[ 1 ] + "\" + IIF( Len( a ) > 1, a[ 2 ] + "\", "" )
    ENDIF
    IF SubStr( cDir, 2, 1 ) == ":"
        RETURN Left( cDir, 2 ) + hb_ps()
    ENDIF

RETURN hb_ps()

/* the subfolders of a folder as tree nodes, sorted by name; aOld: the nodes
   read before - a folder that is still there keeps its node (open, its
   subfolders) */
STATIC FUNCTION cnc_TreeKids( cPath, lHidden, aOld )

    LOCAL aKids := {}, a, n

    FOR EACH a IN Directory( cPath + hb_osFileMask(), IIF( lHidden, "DHS", "D" ) )
        IF "D" $ a[ 5 ] .AND. !( a[ 1 ] == "." .OR. a[ 1 ] == ".." )
            IF aOld != NIL .AND. ( n := cnc_TreeFind( aOld, a[ 1 ] ) ) > 0
                AAdd( aKids, aOld[ n ] )
            ELSE
                AAdd( aKids, { a[ 1 ], cPath + a[ 1 ] + hb_ps(), NIL, .F. } )
            ENDIF
        ENDIF
    NEXT
    ASort( aKids,,, {| x, y | Lower( x[ T_NAME ] ) < Lower( y[ T_NAME ] ) } )

RETURN aKids

/* Ctrl-R: every folder read before is read again, the open ones stay open */
STATIC PROCEDURE cnc_TreeRefresh( aNode, lHidden )

    LOCAL aKid

    IF aNode[ T_KIDS ] != NIL
        aNode[ T_KIDS ] := cnc_TreeKids( aNode[ T_PATH ], lHidden, aNode[ T_KIDS ] )
        aNode[ T_OPEN ] := aNode[ T_OPEN ] .AND. ! Empty( aNode[ T_KIDS ] )
        FOR EACH aKid IN aNode[ T_KIDS ]
            cnc_TreeRefresh( aKid, lHidden )
        NEXT
    ENDIF

RETURN

STATIC FUNCTION cnc_TreeFind( aKids, cName )

    LOCAL n := AScan( aKids, {| x | x[ T_NAME ] == cName } )

#ifdef __PLATFORM__WINDOWS
    IF n == 0
        n := AScan( aKids, {| x | Lower( x[ T_NAME ] ) == Lower( cName ) } )
    ENDIF
#endif

RETURN n

/* the rows of a folder and of its open subfolders, with the tree lines:
   cIndent the lines of the folders above it, lLast: the last in its folder */
STATIC PROCEDURE cnc_TreeRows( aRows, aNode, cIndent, lLast, lRoot )

    LOCAL aKid

    AAdd( aRows, { aNode, IIF( lRoot, "", cIndent + IIF( lLast, ht_glyph( 0x2514, "`" ), ht_glyph( 0x251C, "+" ) ) + ;
                                         ht_glyph( 0x2500, "-" ) ) } )
    IF aNode[ T_OPEN ] .AND. aNode[ T_KIDS ] != NIL
        FOR EACH aKid IN aNode[ T_KIDS ]
            cnc_TreeRows( aRows, aKid, IIF( lRoot, "", cIndent + IIF( lLast, "  ", ht_glyph( 0x2502, "|" ) + " " ) ), ;
                          aKid:__enumIsLast(), .F. )
        NEXT
    ENDIF

RETURN

/* "+" a closed folder that has (or may have) subfolders, "-" an open one */
STATIC FUNCTION cnc_TreeMark( aNode )
    DO CASE
    CASE aNode[ T_OPEN ]
        RETURN "-"
    CASE aNode[ T_KIDS ] == NIL .OR. ! Empty( aNode[ T_KIDS ] )
        RETURN "+"
    ENDCASE
RETURN " "

/* the row of the folder that holds the folder on row n; 0 = n is the root */
STATIC FUNCTION cnc_TreeParentRow( aRows, n )

    LOCAL cUp

    IF n <= 1
        RETURN 0
    ENDIF
    cUp := cnc_ParentDir( aRows[ n ][ 1 ][ T_PATH ] )

RETURN AScan( aRows, {| r | cnc_SamePath( r[ 1 ][ T_PATH ], cUp ) } )

/*----------------------------------------------------------- paths */

FUNCTION cnc_IsRoot( cDir )
    IF Left( cDir, 2 ) == "\\"                       /* \pc\share\ */
        RETURN Len( hb_ATokens( hb_DirSepDel( SubStr( cDir, 3 ) ), "\" ) ) <= 2
    ENDIF
RETURN Len( cDir ) <= 3 .AND. ( SubStr( cDir, 2, 1 ) == ":" .OR. cDir == hb_ps() )

/* "C:\a\b\" -> "C:\a\" */
FUNCTION cnc_ParentDir( cDir )

    LOCAL cUp := hb_FNameDir( hb_DirSepDel( cDir ) )

RETURN IIF( Empty( cUp ), cDir, cUp )

FUNCTION cnc_SamePath( c1, c2 )
#ifdef __PLATFORM__WINDOWS
RETURN ! Empty( c1 ) .AND. Lower( hb_DirSepAdd( c1 ) ) == Lower( hb_DirSepAdd( c2 ) )
#else
RETURN ! Empty( c1 ) .AND. hb_DirSepAdd( c1 ) == hb_DirSepAdd( c2 )
#endif
