/*
 * CELargeBuffer - a file too big for memory, shown read-only (CEdit)
 *
 * Copyright (c) 2026 Srđan Dragojlović - dual licensed, see LICENSE
 *
 * CEBuffer keeps every line in an array; a 32-bit program cannot even read a
 * 2 GB file into one string (it stopped without a word - issue #4). Above
 * CE_LARGE_FILE CEdit opens the file with this class instead:
 *
 *   - nothing of the file stays in memory but an index: the byte offset of
 *     every CEL_BLOCK-th line (40 million lines: 40 000 numbers);
 *   - the index is built in C, a few megabytes at a time from the event loop
 *     (CEdit shows the start of the file at once, "Idx n%" in the status line,
 *     the line count grows meanwhile);
 *   - lines are read from the disk when they are shown, a block of CEL_BLOCK
 *     lines at a time; the last CEL_CACHE blocks are kept;
 *   - a line longer than CEL_MAXLINE bytes is shown in pieces of that size
 *     (a file without line breaks - fsutil file createnew - is still readable),
 *     never in the middle of a UTF-8 character;
 *   - search runs in C over the bytes of the file (case: ASCII letters);
 *   - read-only: no edit, no undo, no save. The file stays where it is (no copy,
 *     no temporary file), and is opened only while a block is read.
 *
 * The same C state machine splits the lines when indexing and when reading,
 * so the index and the lines always agree.
 */

#include "hbclass.ch"
#include "fileio.ch"
#include "cedit.ch"

#define CEL_BLOCK     1024                 /* lines per index entry and per read */
#define CEL_MAXLINE   2048                 /* bytes: a longer line is shown in pieces */
#define CEL_CACHE     8                    /* blocks of lines kept in memory */
#define CEL_FIRST     ( 4 * 1024 * 1024 )  /* bytes indexed before the file is shown */
#define CEL_COPYMAX   ( 16 * 1024 * 1024 ) /* the most a selection may copy */

CLASS CELargeBuffer FROM CEBuffer

EXPORTED:

    DATA nSize      INIT 0          /* bytes of the file */
    DATA nStart     INIT 0          /* the first byte of the text (3 after a BOM) */
    DATA nScanned   INIT 0          /* bytes indexed so far */
    DATA nLines     INIT 0          /* lines found so far */
    DATA aIdx       INIT {}         /* offset of line ( n - 1 ) * CEL_BLOCK + 1 */
    DATA aState                     /* the C scanner between steps */
    DATA lIndexed   INIT .F.
    DATA lSplit     INIT .F.        /* a line was longer than CEL_MAXLINE */
    DATA aCache     INIT {}         /* { nBlock, aLines, aOffsets }, newest last */
    DATA cDefCP     INIT "UTF8"     /* a line that is not UTF-8 is read in it */

    METHOD open( cFile, cDefCP, lForce )
    METHOD load( cFile, cDefCP, lForce ) INLINE ::open( cFile, cDefCP, lForce )
    METHOD indexStep( nBytes )
    METHOD indexTo( nOffset )
    METHOD percent() INLINE IIF( ::lIndexed, 100, Int( 100 * ::nScanned / Max( 1, ::nSize ) ) )
    METHOD lineCount() INLINE Max( 1, ::nLines )
    METHOD line( n )
    METHOD lineAt( nOffset )
    METHOD find( cWhat, nRow, nCol, lCase, lWord, lBack, lWrap )
    METHOD selText()
    METHOD getText() INLINE ""
    METHOD isModified() INLINE .F.
    METHOD save( cFile )
    METHOD setText() INLINE NIL
    METHOD insertText( nRow, nCol ) INLINE { nRow, nCol }
    METHOD deleteRange() INLINE ""
    METHOD undo() INLINE .F.
    METHOD redo() INLINE .F.

PROTECTED:

    METHOD block( nBlock )
    METHOD toText( cRaw )

ENDCLASS

/* opens the file and indexes its start; .F. and ::cError when it cannot be read.
   cDefCP: the codepage of a file that is not UTF-8; lForce: in cDefCP always */
METHOD open( cFile, cDefCP, lForce ) CLASS CELargeBuffer

    LOCAL nH, cHead := Space( 65536 ), nRead, n

    ::cError := ""
    ::cFile := cFile
    ::lReadOnly := .T.
    ::lHilite := .F.
    ::cSyntax := ""
    ::cDefCP := hb_defaultValue( cDefCP, "UTF8" )
    ::aIdx := {}
    ::aCache := {}
    ::aState := NIL
    ::nLines := 0
    ::lIndexed := .F.
    ::lSplit := .F.
    ::nRow := ::nCol := ::nTop := 1
    ::nLeft := 0

    nH := FOpen( cFile, FO_READ + FO_SHARED )
    IF nH == F_ERROR
        ::cError := "Cannot open " + cFile + " (" + hb_ntos( FError() ) + ")"
        RETURN .F.
    ENDIF
    ::nSize := FSeek( nH, 0, FS_END )
    FSeek( nH, 0, FS_SET )
    nRead := FRead( nH, @cHead, hb_BLen( cHead ) )
    FClose( nH )
    cHead := hb_BLeft( cHead, nRead )

    /* BOM, codepage and line ends from the first 64 KB */
    ::lBOM := hb_BLeft( cHead, 3 ) == e"\xEF\xBB\xBF"
    ::nStart := IIF( ::lBOM, 3, 0 )
    IF ( n := hb_BRAt( Chr( 10 ), cHead ) ) > 0
        cHead := hb_BLeft( cHead, n )             /* whole lines: no character cut in two */
    ELSE
        cHead := hb_BLeft( cHead, Max( 0, hb_BLen( cHead ) - 3 ) )
    ENDIF
    IF hb_defaultValue( lForce, .F. ) .OR. ! hb_StrIsUTF8( hb_BSubStr( cHead, ::nStart + 1 ) )
        ::cCP := ::cDefCP
    ELSE
        ::cCP := "UTF8"
    ENDIF
    ::cEol := IIF( Chr( 13 ) + Chr( 10 ) $ cHead .OR. ! Chr( 10 ) $ cHead, Chr( 13 ) + Chr( 10 ), Chr( 10 ) )
    ::nScanned := ::nStart

    ::indexStep( CEL_FIRST )

RETURN .T.

/* indexes up to nBytes more of the file; .T. when the whole file is indexed */
METHOD indexStep( nBytes ) CLASS CELargeBuffer

    LOCAL a

    IF ::lIndexed
        RETURN .T.
    ENDIF
    /* { nLines, nScanned, lEnd, lSplit, aState } */
    a := CEL_SCAN( ::cFile, ::nScanned, nBytes, ::aState, CEL_BLOCK, CEL_MAXLINE, ::nLines, ::aIdx )
    IF a == NIL
        ::cError := "Cannot read " + ::cFile
        ::lIndexed := .T.
        RETURN .T.
    ENDIF
    ::nLines := a[ 1 ]
    ::nScanned := a[ 2 ]
    ::lIndexed := a[ 3 ]
    ::lSplit := ::lSplit .OR. a[ 4 ]
    ::aState := a[ 5 ]

RETURN ::lIndexed

/* the index reaches nOffset (a search found something further on) */
METHOD indexTo( nOffset ) CLASS CELargeBuffer

    DO WHILE ! ::lIndexed .AND. ::nScanned <= nOffset
        ::indexStep( Max( CEL_FIRST, nOffset - ::nScanned + 1 ) )
    ENDDO

RETURN NIL

/* line n (UTF-8); "" past the lines indexed so far */
METHOD line( n ) CLASS CELargeBuffer

    LOCAL a

    IF n < 1 .OR. n > ::nLines
        RETURN ""
    ENDIF
    a := ::block( Int( ( n - 1 ) / CEL_BLOCK ) )

RETURN IIF( a == NIL, "", ::toText( a[ 2 ][ ( n - 1 ) % CEL_BLOCK + 1 ] ) )

/* the lines of block nBlock (0-based): { nBlock, aRawLines, aOffsets }, from the cache */
METHOD block( nBlock ) CLASS CELargeBuffer

    LOCAL n, a

    IF ( n := AScan( ::aCache, {| x | x[ 1 ] == nBlock } ) ) > 0
        a := ::aCache[ n ]
        IF n < Len( ::aCache )                      /* the newest goes last */
            hb_ADel( ::aCache, n, .T. )
            AAdd( ::aCache, a )
        ENDIF
        RETURN a
    ENDIF
    IF nBlock + 1 > Len( ::aIdx )
        RETURN NIL
    ENDIF
    a := CEL_READ( ::cFile, ::aIdx[ nBlock + 1 ], CEL_BLOCK, CEL_MAXLINE )
    IF a == NIL
        RETURN NIL
    ENDIF
    a := { nBlock, a[ 1 ], a[ 2 ] }
    AAdd( ::aCache, a )
    IF Len( ::aCache ) > CEL_CACHE
        hb_ADel( ::aCache, 1, .T. )
    ENDIF

RETURN a

/* the bytes of a line as text: the file's codepage to UTF-8; in a UTF-8 file a
   line that is not UTF-8 is read in the default codepage */
METHOD toText( cRaw ) CLASS CELargeBuffer

    IF !( ::cCP == "UTF8" )
        RETURN hb_StrToUTF8( cRaw, ::cCP )
    ENDIF
    IF ! hb_StrIsUTF8( cRaw )
        RETURN hb_StrToUTF8( cRaw, IIF( ::cDefCP == "UTF8", "CP1252", ::cDefCP ) )
    ENDIF

RETURN cRaw

/* { nLine, nCol } of a byte offset of the file */
METHOD lineAt( nOffset ) CLASS CELargeBuffer

    LOCAL nLo := 1, nHi, nMid, a, n

    ::indexTo( nOffset )
    nHi := Len( ::aIdx )
    IF nHi == 0
        RETURN { 1, 1 }
    ENDIF
    DO WHILE nLo < nHi                             /* the last block starting at or before it */
        nMid := Int( ( nLo + nHi + 1 ) / 2 )
        IF ::aIdx[ nMid ] <= nOffset
            nLo := nMid
        ELSE
            nHi := nMid - 1
        ENDIF
    ENDDO
    a := ::block( nLo - 1 )
    IF a == NIL
        RETURN { 1, 1 }
    ENDIF
    n := Len( a[ 3 ] )
    DO WHILE n > 1 .AND. a[ 3 ][ n ] > nOffset
        n--
    ENDDO

RETURN { ( nLo - 1 ) * CEL_BLOCK + n, ;
         Len( ::toText( hb_BLeft( a[ 2 ][ n ], nOffset - a[ 3 ][ n ] ) ) ) + 1 }

/* as CEBuffer:find(), over the bytes of the file. Returns { nRow, nCol, lWrapped } or NIL */
METHOD find( cWhat, nRow, nCol, lCase, lWord, lBack, lWrap ) CLASS CELargeBuffer

    LOCAL cBytes, nFrom, nOff, lWrapped := .F., a, a2, nBlock, nLine

    IF Len( cWhat ) == 0
        RETURN NIL
    ENDIF
    cBytes := IIF( ::cCP == "UTF8", cWhat, hb_UTF8ToStr( cWhat, ::cCP ) )

    /* the byte offset of the cursor */
    nBlock := Int( ( Max( 1, Min( nRow, ::nLines ) ) - 1 ) / CEL_BLOCK )
    a := ::block( nBlock )
    IF a == NIL
        nFrom := ::nStart
    ELSE
        nLine := ( Max( 1, Min( nRow, ::nLines ) ) - 1 ) % CEL_BLOCK + 1
        nFrom := a[ 3 ][ nLine ] + hb_BLen( IIF( ::cCP == "UTF8", Left( ::toText( a[ 2 ][ nLine ] ), nCol - 1 ), ;
                                                  hb_UTF8ToStr( Left( ::toText( a[ 2 ][ nLine ] ), nCol - 1 ), ::cCP ) ) )
    ENDIF

    DO WHILE .T.
        IF lBack
            nOff := CEL_FIND( ::cFile, cBytes, nFrom - hb_BLen( cBytes ), ::nStart, lCase, .T. )
        ELSE
            nOff := CEL_FIND( ::cFile, cBytes, nFrom, ::nSize, lCase, .F. )
        ENDIF
        IF nOff < 0
            IF ! lWrap .OR. lWrapped
                RETURN NIL
            ENDIF
            lWrapped := .T.
            nFrom := IIF( lBack, ::nSize, ::nStart )
            LOOP
        ENDIF
        a2 := ::lineAt( nOff )
        IF ! lWord .OR. ce_LargeWholeWord( ::line( a2[ 1 ] ), a2[ 2 ], Len( cWhat ) )
            RETURN { a2[ 1 ], a2[ 2 ], lWrapped }
        ENDIF
        nFrom := IIF( lBack, nOff + hb_BLen( cBytes ) - 1, nOff + 1 )
    ENDDO

RETURN NIL

/* the selected text; "" when it is bigger than CEL_COPYMAX */
METHOD selText() CLASS CELargeBuffer

    LOCAL a, n, c := "", cLine

    IF ! ::hasSel()
        RETURN ""
    ENDIF
    a := ::streamRange()        /* { nR1, nC1, nR2, nC2 } */
    FOR n := a[ 1 ] TO a[ 3 ]
        cLine := ::line( n )
        DO CASE
        CASE a[ 1 ] == a[ 3 ]
            c += SubStr( cLine, a[ 2 ], a[ 4 ] - a[ 2 ] )
        CASE n == a[ 1 ]
            c += SubStr( cLine, a[ 2 ] ) + Chr( 10 )
        CASE n == a[ 3 ]
            c += Left( cLine, a[ 4 ] - 1 )
        OTHERWISE
            c += cLine + Chr( 10 )
        ENDCASE
        IF hb_BLen( c ) > CEL_COPYMAX
            ::cError := "The selection is too big to copy"
            RETURN ""
        ENDIF
    NEXT

RETURN c

METHOD save( cFile ) CLASS CELargeBuffer
    HB_SYMBOL_UNUSED( cFile )
    ::cError := "A large file is opened read-only"
RETURN .F.

/* a whole word at nCol of cLine, nLen characters long */
STATIC FUNCTION ce_LargeWholeWord( cLine, nCol, nLen )

    LOCAL cB := SubStr( cLine, nCol - 1, 1 ), cA := SubStr( cLine, nCol + nLen, 1 )

RETURN ! ( IsAlpha( cB ) .OR. IsDigit( cB ) .OR. cB == "_" ) .AND. ;
       ! ( IsAlpha( cA ) .OR. IsDigit( cA ) .OR. cA == "_" )

#pragma BEGINDUMP

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapifs.h"
#include "hbapierr.h"

#define CEL_CHUNK  ( 1024 * 1024 )

/* the lines of a file, byte by byte. A line ends at LF (a CR before it stays in
   the line and is taken off when it is read) or after nMax bytes; then the
   bytes of an unfinished UTF-8 character still go to that line, and a LF just
   there ends it (no empty line after a piece). */
typedef struct
{
   HB_MAXINT nLen;      /* bytes of the line so far */
   int       fStart;    /* the next byte starts a line */
   int       fSplit;    /* the line was cut at nMax */
} CELSTATE;

#define CEL_IN     0    /* in the current line */
#define CEL_NEW    1    /* starts a new line */
#define CEL_PREV   2    /* still the previous line (the rest of a UTF-8 character) */
#define CEL_SKIP   3    /* the LF right after a cut: no new line, no text */

static int cel_byte( CELSTATE * s, unsigned char b, HB_MAXINT nMax )
{
   int iRet = CEL_IN;

   if( s->fStart )
   {
      if( s->fSplit )
      {
         if( ( b & 0xC0 ) == 0x80 )
         {
            s->nLen++;
            return CEL_PREV;
         }
         if( b == '\n' )
         {
            s->fSplit = 0;
            return CEL_SKIP;
         }
      }
      s->fStart = 0;
      s->fSplit = 0;
      s->nLen = 0;
      iRet = CEL_NEW;
   }
   if( b == '\n' )
      s->fStart = 1;
   else if( ++s->nLen >= nMax )
   {
      s->fStart = 1;
      s->fSplit = 1;
   }
   return iRet;
}

static void cel_getState( PHB_ITEM pState, CELSTATE * s )
{
   s->nLen = 0;
   s->fStart = 1;
   s->fSplit = 0;
   if( pState && HB_IS_ARRAY( pState ) && hb_arrayLen( pState ) >= 3 )
   {
      s->nLen = hb_arrayGetNInt( pState, 1 );
      s->fStart = hb_arrayGetL( pState, 2 ) ? 1 : 0;
      s->fSplit = hb_arrayGetL( pState, 3 ) ? 1 : 0;
   }
}

/* CEL_SCAN( cFile, nFrom, nBytes, aState, nBlock, nMax, nLines, @aIdx )
      -> { nLines, nScanned, lEnd, lSplit, aState } or NIL (cannot read).
   Adds to aIdx the offset of every line ( k * nBlock + 1 ). */
HB_FUNC( CEL_SCAN )
{
   const char * szFile = hb_parc( 1 );
   HB_FOFFSET nPos = ( HB_FOFFSET ) hb_parnint( 2 );
   HB_MAXINT nBytes = hb_parnint( 3 );
   HB_MAXINT nBlock = hb_parnint( 5 ), nMax = hb_parnint( 6 ), nLines = hb_parnint( 7 );
   PHB_ITEM pIdx = hb_param( 8, HB_IT_ARRAY );
   HB_FHANDLE hFile;
   CELSTATE s;
   int fEnd = 0, fSplit = 0;

   if( ! szFile || ! pIdx || nBlock < 1 || nMax < 4 )
   {
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }
   hFile = hb_fsOpen( szFile, FO_READ | FO_SHARED );
   if( hFile == FS_ERROR )
   {
      hb_ret();
      return;
   }
   cel_getState( hb_param( 4, HB_IT_ANY ), &s );
   {
      unsigned char * pBuf = ( unsigned char * ) hb_xgrab( CEL_CHUNK );
      PHB_ITEM pOff = hb_itemNew( NULL );

      hb_fsSeekLarge( hFile, nPos, FS_SET );
      while( nBytes > 0 )
      {
         HB_SIZE nRead = hb_fsReadLarge( hFile, pBuf, CEL_CHUNK ), i;

         if( nRead == 0 || nRead == ( HB_SIZE ) FS_ERROR )
         {
            fEnd = 1;
            break;
         }
         for( i = 0; i < nRead; i++ )
         {
            if( cel_byte( &s, pBuf[ i ], nMax ) == CEL_NEW )
            {
               if( nLines % nBlock == 0 )
                  hb_arrayAdd( pIdx, hb_itemPutNInt( pOff, ( HB_MAXINT ) ( nPos + i ) ) );
               nLines++;
            }
            if( s.fSplit )
               fSplit = 1;
         }
         nPos += nRead;
         nBytes -= ( HB_MAXINT ) nRead;
         if( nRead < CEL_CHUNK )
         {
            fEnd = 1;
            break;
         }
      }
      hb_itemRelease( pOff );
      hb_xfree( pBuf );
   }
   hb_fsClose( hFile );
   if( ! fEnd && nPos >= hb_fsFSize( szFile, HB_TRUE ) )
      fEnd = 1;

   {
      PHB_ITEM pRet = hb_itemArrayNew( 5 ), pState = hb_itemArrayNew( 3 );

      hb_arraySetNInt( pState, 1, s.nLen );
      hb_arraySetL( pState, 2, s.fStart );
      hb_arraySetL( pState, 3, s.fSplit );
      hb_arraySetNInt( pRet, 1, nLines );
      hb_arraySetNInt( pRet, 2, ( HB_MAXINT ) nPos );
      hb_arraySetL( pRet, 3, fEnd );
      hb_arraySetL( pRet, 4, fSplit );
      hb_arraySet( pRet, 5, pState );
      hb_itemRelease( pState );
      hb_itemReturnRelease( pRet );
   }
}

/* CEL_READ( cFile, nOffset, nCount, nMax ) -> { aLines, aOffsets } or NIL.
   nCount lines from nOffset (a line start of the index), the bytes as they
   are without the line end (LF, CR LF). */
HB_FUNC( CEL_READ )
{
   const char * szFile = hb_parc( 1 );
   HB_FOFFSET nPos = ( HB_FOFFSET ) hb_parnint( 2 );
   HB_MAXINT nCount = hb_parnint( 3 ), nMax = hb_parnint( 4 ), nHave = 0;
   HB_FHANDLE hFile;
   CELSTATE s;
   PHB_ITEM pLines, pOffs, pItem;
   char * pLine;
   HB_SIZE nLine = 0;
   int fOpen = 0, fDone = 0;

   if( ! szFile || nCount < 1 || nMax < 4 )
   {
      hb_errRT_BASE( EG_ARG, 3012, NULL, HB_ERR_FUNCNAME, HB_ERR_ARGS_BASEPARAMS );
      return;
   }
   hFile = hb_fsOpen( szFile, FO_READ | FO_SHARED );
   if( hFile == FS_ERROR )
   {
      hb_ret();
      return;
   }
   pLines = hb_itemArrayNew( 0 );
   pOffs = hb_itemArrayNew( 0 );
   pItem = hb_itemNew( NULL );
   pLine = ( char * ) hb_xgrab( nMax + 8 );   /* a piece and the bytes of one character */
   s.nLen = 0;
   s.fStart = 1;
   s.fSplit = 0;
   hb_fsSeekLarge( hFile, nPos, FS_SET );
   {
      unsigned char * pBuf = ( unsigned char * ) hb_xgrab( 65536 );

      while( ! fDone )
      {
         HB_SIZE nRead = hb_fsReadLarge( hFile, pBuf, 65536 ), i;

         if( nRead == 0 || nRead == ( HB_SIZE ) FS_ERROR )
            break;
         for( i = 0; i < nRead; i++ )
         {
            unsigned char b = pBuf[ i ];

            switch( cel_byte( &s, b, nMax ) )
            {
               case CEL_NEW:
                  if( fOpen )
                  {
                     if( nLine > 0 && pLine[ nLine - 1 ] == '\r' )
                        nLine--;
                     hb_arrayAdd( pLines, hb_itemPutCL( pItem, pLine, nLine ) );
                     if( ++nHave >= nCount )
                     {
                        fOpen = 0;
                        fDone = 1;
                        break;
                     }
                  }
                  hb_arrayAdd( pOffs, hb_itemPutNInt( pItem, ( HB_MAXINT ) ( nPos + i ) ) );
                  fOpen = 1;
                  nLine = 0;
                  if( b != '\n' )
                     pLine[ nLine++ ] = ( char ) b;
                  break;
               case CEL_PREV:
                  if( nLine < ( HB_SIZE ) nMax + 8 )
                     pLine[ nLine++ ] = ( char ) b;
                  break;
               case CEL_IN:
                  if( b != '\n' && nLine < ( HB_SIZE ) nMax + 8 )
                     pLine[ nLine++ ] = ( char ) b;
                  break;
            }
            if( fDone )
               break;
         }
         nPos += nRead;
         if( nRead < 65536 )
            break;
      }
      hb_xfree( pBuf );
   }
   if( fOpen )
   {
      if( nLine > 0 && pLine[ nLine - 1 ] == '\r' )
         nLine--;
      hb_arrayAdd( pLines, hb_itemPutCL( pItem, pLine, nLine ) );
   }
   hb_xfree( pLine );
   hb_itemRelease( pItem );
   hb_fsClose( hFile );
   {
      PHB_ITEM pRet = hb_itemArrayNew( 2 );

      hb_arraySetForward( pRet, 1, pLines );
      hb_arraySetForward( pRet, 2, pOffs );
      hb_itemRelease( pLines );
      hb_itemRelease( pOffs );
      hb_itemReturnRelease( pRet );
   }
}

static int cel_eq( const unsigned char * p, const unsigned char * f, HB_SIZE n, int fCase )
{
   HB_SIZE i;

   for( i = 0; i < n; i++ )
   {
      unsigned char a = p[ i ], b = f[ i ];

      if( a != b )
      {
         if( fCase )
            return 0;
         if( a >= 'a' && a <= 'z' )
            a -= 32;
         if( b >= 'a' && b <= 'z' )
            b -= 32;
         if( a != b )
            return 0;
      }
   }
   return 1;
}

/* CEL_FIND( cFile, cBytes, nFrom, nLimit, lCase, lBack ) -> nOffset, -1 = not found.
   Forward: the first match at nFrom or after it, before nLimit (the file size).
   Backward: the last match starting at nFrom or before it, at nLimit or after it.
   Without lCase ASCII letters match in either case. */
HB_FUNC( CEL_FIND )
{
   const char * szFile = hb_parc( 1 );
   const unsigned char * pFind = ( const unsigned char * ) hb_parc( 2 );
   HB_SIZE nFind = hb_parclen( 2 );
   HB_FOFFSET nFrom = ( HB_FOFFSET ) hb_parnint( 3 ), nLimit = ( HB_FOFFSET ) hb_parnint( 4 );
   int fCase = hb_parl( 5 ), fBack = hb_parl( 6 );
   HB_FOFFSET nFound = -1;
   HB_FHANDLE hFile;

   if( ! szFile || ! pFind || nFind == 0 || nFind > CEL_CHUNK / 2 )
   {
      hb_retnint( -1 );
      return;
   }
   hFile = hb_fsOpen( szFile, FO_READ | FO_SHARED );
   if( hFile == FS_ERROR )
   {
      hb_retnint( -1 );
      return;
   }
   {
      unsigned char * pBuf = ( unsigned char * ) hb_xgrab( CEL_CHUNK );

      if( ! fBack )
      {
         HB_FOFFSET nPos = nFrom;

         while( nPos + ( HB_FOFFSET ) nFind <= nLimit && nFound < 0 )
         {
            HB_SIZE nRead, i;

            hb_fsSeekLarge( hFile, nPos, FS_SET );
            nRead = hb_fsReadLarge( hFile, pBuf, CEL_CHUNK );
            if( nRead < nFind || nRead == ( HB_SIZE ) FS_ERROR )
               break;
            for( i = 0; i + nFind <= nRead; i++ )
            {
               if( cel_eq( pBuf + i, pFind, nFind, fCase ) )
               {
                  nFound = nPos + ( HB_FOFFSET ) i;
                  break;
               }
            }
            if( nRead < CEL_CHUNK )
               break;
            nPos += ( HB_FOFFSET ) ( nRead - nFind + 1 );   /* overlap: a match across the border */
         }
      }
      else
      {
         HB_FOFFSET nEnd = nFrom + ( HB_FOFFSET ) nFind;    /* the bytes before it are looked at */

         while( nEnd - ( HB_FOFFSET ) nFind >= nLimit && nFound < 0 )
         {
            HB_FOFFSET nPos = nEnd - CEL_CHUNK;
            HB_SIZE nRead, i;

            if( nPos < nLimit )
               nPos = nLimit;
            hb_fsSeekLarge( hFile, nPos, FS_SET );
            nRead = hb_fsReadLarge( hFile, pBuf, ( HB_SIZE ) ( nEnd - nPos ) );
            if( nRead < nFind || nRead == ( HB_SIZE ) FS_ERROR )
               break;
            for( i = nRead - nFind + 1; i-- > 0; )
            {
               if( cel_eq( pBuf + i, pFind, nFind, fCase ) )
               {
                  nFound = nPos + ( HB_FOFFSET ) i;
                  break;
               }
            }
            if( nPos == nLimit )
               break;
            nEnd = nPos + ( HB_FOFFSET ) nFind - 1;
         }
      }
      hb_xfree( pBuf );
   }
   hb_fsClose( hFile );
   hb_retnint( ( HB_MAXINT ) nFound );
}

#pragma ENDDUMP
