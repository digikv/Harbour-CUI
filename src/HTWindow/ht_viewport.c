/*
 * HBTui viewport-aware GT output helpers.
 *
 * Replaces CT wFormat() coordinate remapping with explicit viewport state
 * pushed from HTWinApi.prg via HT_SETVIEWPORT().
 */

#include "hbapi.h"
#include "hbapigt.h"
#include "hbapicdp.h"
#include "hbapiitm.h"
#include "hbapistr.h"
#include "hbvm.h"
#include "hbgtinfo.ch"

#include <string.h>

static HB_BOOL s_fActive = HB_FALSE;
static int     s_iOffRow  = 0;
static int     s_iOffCol  = 0;
static int     s_iMaxRow  = 0;
static int     s_iMaxCol  = 0;
static int     s_iWinTop  = 0;
static int     s_iWinLeft = 0;

static int ht_color_param( int iParam, int iDefault )
{
   int iColor;

   if( HB_ISNUM( iParam ) )
      iColor = hb_parni( iParam );
   else if( hb_parclen( iParam ) > 0 )
   {
      iColor = hb_gtColorToN( hb_parc( iParam ) );
      if( iColor == -1 )
         iColor = iDefault;
   }
   else
      iColor = iDefault;

   return iColor;
}

static int ht_abs_row( int iRel )
{
   return s_fActive ? iRel + s_iOffRow : iRel;
}

static int ht_abs_col( int iRel )
{
   return s_fActive ? iRel + s_iOffCol : iRel;
}

/* ---- Wide (East Asian) characters ------------------------------------
 *
 * A GT keeps one character per screen cell, but Chinese, Japanese and
 * Korean characters are drawn two cells wide (like in any terminal). gtwvt
 * draws each of them into a single cell, so neighbouring glyphs overlap and
 * the text is unreadable. When "wide cells" are on, the DispOutAt remap
 * below writes every wide character followed by a filler space: the glyph
 * spills over its own filler cell and the grid stays aligned. Output is
 * clipped at the right edge of the active viewport (the painted widget),
 * so a widget never paints over its neighbours - padding is cut first.
 * Editors use HT_STRWIDTH() for cursor columns and scrolling.
 *
 * Auto mode turns wide cells on for graphic GTs (gtwvt, gtxwc) and off for
 * text GTs (gttrm, gtnul, ...), where the terminal does the job or there
 * is no display. HT_WIDECELLS( lOn ) overrides it.
 */

static int s_iWide = -1;   /* -1 = auto (graphic GT), 0 = off, 1 = on */

/* a wide character was written: ht_winmouse.c then repaints whole rows */
HB_BOOL ht_fWideWritten = HB_FALSE;

static HB_BOOL ht_wc_iswide( HB_WCHAR wc )
{
   return wc >= 0x1100 && (
          wc <= 0x115F ||                       /* Hangul Jamo initials */
          ( wc >= 0x2E80 && wc <= 0x303E ) ||   /* CJK radicals .. CJK symbols and punctuation */
          ( wc >= 0x3041 && wc <= 0x33FF ) ||   /* kana, Bopomofo, Hangul compat., enclosed/compat. CJK */
          ( wc >= 0x3400 && wc <= 0x4DBF ) ||   /* CJK extension A */
          ( wc >= 0x4E00 && wc <= 0x9FFF ) ||   /* CJK unified ideographs */
          ( wc >= 0xA000 && wc <= 0xA4CF ) ||   /* Yi */
          ( wc >= 0xA960 && wc <= 0xA97F ) ||   /* Hangul Jamo extended A */
          ( wc >= 0xAC00 && wc <= 0xD7A3 ) ||   /* Hangul syllables */
          ( wc >= 0xF900 && wc <= 0xFAFF ) ||   /* CJK compatibility ideographs */
          ( wc >= 0xFE10 && wc <= 0xFE19 ) ||   /* vertical forms */
          ( wc >= 0xFE30 && wc <= 0xFE6F ) ||   /* CJK compatibility forms, small forms */
          ( wc >= 0xFF00 && wc <= 0xFF60 ) ||   /* fullwidth forms */
          ( wc >= 0xFFE0 && wc <= 0xFFE6 ) );   /* fullwidth signs */
}

static HB_BOOL ht_wide_on( void )
{
   if( s_iWide < 0 )
   {
      HB_GT_INFO gtInfo;

      memset( &gtInfo, 0, sizeof( gtInfo ) );
      gtInfo.pResult = hb_itemNew( NULL );
      hb_gtInfo( HB_GTI_ISGRAPHIC, &gtInfo );
      s_iWide = hb_itemGetL( gtInfo.pResult ) ? 1 : 0;
      hb_itemRelease( gtInfo.pResult );
   }
   return s_iWide == 1;
}

/* screen cells taken by a string; *pfWide reports a wide character */
static HB_SIZE ht_str_cells( const char * szText, HB_SIZE nLen, HB_BOOL * pfWide )
{
   PHB_CODEPAGE cdp = hb_vmCDP();
   HB_SIZE nIndex = 0, nCells = 0;
   HB_WCHAR wc;

   *pfWide = HB_FALSE;
   if( ! HB_CDP_ISCUSTOM( cdp ) )   /* single-byte codepage: no wide chars */
      return nLen;

   while( HB_CDPCHAR_GET( cdp, szText, nLen, &nIndex, &wc ) )
   {
      if( ht_wc_iswide( wc ) )
      {
         *pfWide = HB_TRUE;
         nCells += 2;
      }
      else
         ++nCells;
   }
   return nCells;
}

/* hb_gtWriteAt() with wide cells: a filler space after each wide char,
   output clipped to iAvail cells (a wide char that does not fit becomes
   one space) */
static void ht_write_wide( int iRow, int iCol, const char * szText, HB_SIZE nLen, int iAvail )
{
   PHB_CODEPAGE cdp = hb_vmCDP();
   char * pBuf = ( char * ) hb_xgrab( nLen * 2 + 1 );
   HB_SIZE nIndex = 0, nPrev = 0, nOut = 0;
   int iCells = 0;
   HB_WCHAR wc;

   while( iCells < iAvail && HB_CDPCHAR_GET( cdp, szText, nLen, &nIndex, &wc ) )
   {
      if( ht_wc_iswide( wc ) )
      {
         if( iCells + 2 > iAvail )
         {
            pBuf[ nOut++ ] = ' ';
            ++iCells;
            break;
         }
         memcpy( pBuf + nOut, szText + nPrev, nIndex - nPrev );
         nOut += nIndex - nPrev;
         pBuf[ nOut++ ] = ' ';
         iCells += 2;
      }
      else
      {
         memcpy( pBuf + nOut, szText + nPrev, nIndex - nPrev );
         nOut += nIndex - nPrev;
         ++iCells;
      }
      nPrev = nIndex;
   }

   ht_fWideWritten = HB_TRUE;
   hb_gtWriteAt( iRow, iCol, pBuf, nOut );
   hb_xfree( pBuf );
}

/* DispOutAt body: iRelCol is the viewport-relative column */
static void ht_write_at( int iRow, int iCol, int iRelCol, const char * szText, HB_SIZE nLen )
{
   HB_BOOL fWide = HB_FALSE;

   if( ht_wide_on() )
      ht_str_cells( szText, nLen, &fWide );

   if( fWide )
   {
      int iAvail = hb_gtMaxCol() - iCol + 1;

      if( s_fActive && s_iMaxCol - iRelCol + 1 < iAvail )
         iAvail = s_iMaxCol - iRelCol + 1;
      if( iAvail > 0 )
         ht_write_wide( iRow, iCol, szText, nLen, iAvail );
   }
   else
      hb_gtWriteAt( iRow, iCol, szText, nLen );
}

/* HT_WIDECELLS( [ lOn ] ) -> lOld  (wide cells in effect before the call) */
HB_FUNC( HT_WIDECELLS )
{
   HB_BOOL fOld = ht_wide_on();

   if( HB_ISLOG( 1 ) )
      s_iWide = hb_parl( 1 ) ? 1 : 0;

   hb_retl( fOld );
}

/* HT_STRWIDTH( cText ) -> nCells  (screen cells the text takes) */
HB_FUNC( HT_STRWIDTH )
{
   const char * szText = hb_parc( 1 );

   if( szText )
   {
      HB_SIZE nLen = hb_parclen( 1 );
      HB_BOOL fWide;

      if( ht_wide_on() )
         hb_retns( ( HB_ISIZ ) ht_str_cells( szText, nLen, &fWide ) );
      else
         hb_retns( ( HB_ISIZ ) hb_cdpTextLen( hb_vmCDP(), szText, nLen ) );
   }
   else
      hb_retni( 0 );
}

/* HT_WIDENREGION( nTop, nLeft, nBottom, nRight )  (screen coordinates)
 * Wide cells for text painted by Harbour core code that bypasses the
 * DispOutAt remap (TBrowse): every wide character gets the next cell as its
 * filler; the rest of its field moves one cell right and the field's last
 * cell is dropped. A field ends at a color change or at a cell drawn with
 * DispOutAtBox() (TBrowse column separators), so columns stay in place.
 * Must run only right after the whole region was redrawn - running it
 * twice over the same text would widen it twice. */
HB_FUNC( HT_WIDENREGION )
{
   int iTop = hb_parni( 1 ), iLeft = hb_parni( 2 );
   int iBottom = hb_parni( 3 ), iRight = hb_parni( 4 );
   int iRow, iCol, iEnd, i;
   int iColor, iColor2;
   HB_BYTE bAttr, bAttr2;
   HB_USHORT usChar, usChar2;

   if( ! ht_wide_on() || ! HB_CDP_ISCUSTOM( hb_vmCDP() ) )
      return;

   if( iRight > hb_gtMaxCol() )
      iRight = hb_gtMaxCol();
   if( iBottom > hb_gtMaxRow() )
      iBottom = hb_gtMaxRow();

   for( iRow = HB_MAX( iTop, 0 ); iRow <= iBottom; ++iRow )
   {
      for( iCol = HB_MAX( iLeft, 0 ); iCol < iRight; ++iCol )
      {
         if( hb_gtGetScrChar( iRow, iCol, &iColor, &bAttr, &usChar ) != HB_SUCCESS ||
             ( bAttr & HB_GT_ATTR_BOX ) != 0 || ! ht_wc_iswide( ( HB_WCHAR ) usChar ) )
            continue;

         /* last cell of this field */
         for( iEnd = iCol + 1; iEnd <= iRight; ++iEnd )
         {
            if( hb_gtGetScrChar( iRow, iEnd, &iColor2, &bAttr2, &usChar2 ) != HB_SUCCESS ||
                iColor2 != iColor || ( bAttr2 & HB_GT_ATTR_BOX ) != 0 )
               break;
         }
         --iEnd;
         if( iEnd <= iCol )
            continue;   /* no room: the glyph stays squeezed into one cell */

         for( i = iEnd; i > iCol + 1; --i )
         {
            hb_gtGetScrChar( iRow, i - 1, &iColor2, &bAttr2, &usChar2 );
            hb_gtPutScrChar( iRow, i, iColor2, bAttr2, usChar2 );
         }
         hb_gtPutScrChar( iRow, iCol + 1, iColor, 0, ' ' );
         ht_fWideWritten = HB_TRUE;
         ++iCol;   /* skip the filler */
      }
   }
}

static char * ht_item_string_con( PHB_ITEM pItem, HB_SIZE * pnLen, HB_BOOL * pfFreeReq )
{
   if( HB_IS_LOGICAL( pItem ) )
   {
      *pnLen = 1;
      *pfFreeReq = HB_FALSE;
      return ( char * ) ( hb_itemGetL( pItem ) ? "T" : "F" );
   }
   return hb_itemString( pItem, pnLen, pfFreeReq );
}

HB_FUNC( HT_SETVIEWPORT )
{
   if( hb_pcount() >= 3 && hb_parni( 3 ) < 0 )
      s_fActive = HB_FALSE;
   else if( hb_pcount() >= 4 )
   {
      s_fActive = HB_TRUE;
      s_iOffRow = hb_parni( 1 );
      s_iOffCol = hb_parni( 2 );
      s_iMaxRow = hb_parni( 3 );
      s_iMaxCol = hb_parni( 4 );
   }
}

HB_FUNC( HT_SETWINORIGIN )
{
   if( hb_pcount() >= 2 )
   {
      s_iWinTop  = hb_parni( 1 );
      s_iWinLeft = hb_parni( 2 );
   }
}

HB_FUNC( HT_VIEWTOP )
{
   hb_retni( s_fActive ? s_iOffRow : 0 );
}

HB_FUNC( HT_VIEWLEFT )
{
   hb_retni( s_fActive ? s_iOffCol : 0 );
}

HB_FUNC( HT_DISPOUTAT )
{
   char * pszString;
   HB_SIZE nLen;
   HB_BOOL bFreeReq;
   int iRow = ht_abs_row( hb_parni( 1 ) );
   int iCol = ht_abs_col( hb_parni( 2 ) );

   if( HB_ISCHAR( 4 ) )
   {
      char szOldColor[ HB_CLRSTR_LEN ];

      hb_gtGetColorStr( szOldColor );
      hb_gtSetColorStr( hb_parc( 4 ) );

      pszString = ht_item_string_con( hb_param( 3, HB_IT_ANY ), &nLen, &bFreeReq );
      ht_write_at( iRow, iCol, hb_parni( 2 ), pszString, nLen );

      if( bFreeReq )
         hb_xfree( pszString );

      hb_gtSetColorStr( szOldColor );
   }
   else if( hb_pcount() >= 3 )
   {
      pszString = ht_item_string_con( hb_param( 3, HB_IT_ANY ), &nLen, &bFreeReq );
      ht_write_at( iRow, iCol, hb_parni( 2 ), pszString, nLen );

      if( bFreeReq )
         hb_xfree( pszString );
   }
}

HB_FUNC( HT_MAXROW )
{
   hb_retni( s_fActive ? s_iMaxRow : hb_gtMaxRow() );
}

HB_FUNC( HT_MAXCOL )
{
   hb_retni( s_fActive ? s_iMaxCol : hb_gtMaxCol() );
}

HB_FUNC( HT_SETPOS )
{
   hb_gtSetPos( ht_abs_row( hb_parni( 1 ) ), ht_abs_col( hb_parni( 2 ) ) );
}

HB_FUNC( HT_ROW )
{
   int iRow, iCol;

   hb_gtGetPos( &iRow, &iCol );

   if( s_fActive )
      iRow -= s_iOffRow;

   hb_retni( iRow );
}

HB_FUNC( HT_COL )
{
   int iRow, iCol;

   hb_gtGetPos( &iRow, &iCol );

   if( s_fActive )
      iCol -= s_iOffCol;

   hb_retni( iCol );
}

HB_FUNC( HT_DISPBOX )
{
   PHB_ITEM pTop    = hb_param( 1, HB_IT_NUMERIC );
   PHB_ITEM pLeft   = hb_param( 2, HB_IT_NUMERIC );
   PHB_ITEM pBottom = hb_param( 3, HB_IT_NUMERIC );
   PHB_ITEM pRight  = hb_param( 4, HB_IT_NUMERIC );

   if( pTop && pLeft && pBottom && pRight )
   {
      /* CP437 single-line box + space fill (same as HTWinApi _HT_BOX_SINGLE) */
      static const char s_szBoxSingle[] = "\xDA\xC4\xBF\xB3\xD9\xC4\xC0\xB3 ";
      static const char s_szBoxDouble[] = "\xC9\xCD\xBB\xBA\xBC\xCD\xC8\xBA ";
      const char * pszBox = hb_parc( 5 );
      int          iColor = ht_color_param( 6, -1 );
      int          iTop     = ht_abs_row( hb_itemGetNI( pTop ) );
      int          iLeft    = ht_abs_col( hb_itemGetNI( pLeft ) );
      int          iBottom  = ht_abs_row( hb_itemGetNI( pBottom ) );
      int          iRight   = ht_abs_col( hb_itemGetNI( pRight ) );

      if( pszBox == NULL || pszBox[ 0 ] == '\0' )
      {
         if( hb_parni( 5 ) == 2 )
            pszBox = s_szBoxDouble;
         else
            pszBox = s_szBoxSingle;
      }

      /* always apply color — bare hb_gtBoxS uses current GT color (often N/N = black) */
      hb_gtDrawBox( iTop, iLeft, iBottom, iRight, pszBox, iColor );
   }
}

HB_FUNC( HT_RAWDISPBOX )
{
   PHB_ITEM pTop    = hb_param( 1, HB_IT_NUMERIC );
   PHB_ITEM pLeft   = hb_param( 2, HB_IT_NUMERIC );
   PHB_ITEM pBottom = hb_param( 3, HB_IT_NUMERIC );
   PHB_ITEM pRight  = hb_param( 4, HB_IT_NUMERIC );

   if( pTop && pLeft && pBottom && pRight )
   {
      static const char s_szBoxSingle[] = "\xDA\xC4\xBF\xB3\xD9\xC4\xC0\xB3 ";
      static const char s_szBoxDouble[] = "\xC9\xCD\xBB\xBA\xBC\xCD\xC8\xBA ";
      const char * pszBox = hb_parc( 5 );
      int          iColor = ht_color_param( 6, -1 );

      if( pszBox == NULL || pszBox[ 0 ] == '\0' )
      {
         if( hb_parni( 5 ) == 2 )
            pszBox = s_szBoxDouble;
         else
            pszBox = s_szBoxSingle;
      }

      hb_gtDrawBox( hb_itemGetNI( pTop ), hb_itemGetNI( pLeft ),
                    hb_itemGetNI( pBottom ), hb_itemGetNI( pRight ),
                    pszBox, iColor );
   }
}

HB_FUNC( HT_SCREENMAXROW )
{
   hb_retni( hb_gtMaxRow() );
}

HB_FUNC( HT_SCREENMAXCOL )
{
   hb_retni( hb_gtMaxCol() );
}

HB_FUNC( HT_MROW )
{
   int iRow, iCol;

   hb_mouseGetPos( &iRow, &iCol );

   if( ! hb_parl( 1 ) && s_fActive )
      iRow -= s_iOffRow;

   hb_retni( iRow );
}

HB_FUNC( HT_MCOL )
{
   int iRow, iCol;

   hb_mouseGetPos( &iRow, &iCol );

   if( ! hb_parl( 1 ) && s_fActive )
      iCol -= s_iOffCol;

   hb_retni( iCol );
}

HB_FUNC( HT_ABSROW )
{
   hb_retni( ht_abs_row( hb_parni( 1 ) ) );
}

HB_FUNC( HT_ABSCOL )
{
   hb_retni( ht_abs_col( hb_parni( 1 ) ) );
}

HB_FUNC( HT_SETCLEARA )
{
   int iColor = ht_color_param( 1, -1 );

   if( iColor >= 0 )
      hb_gtSetClearColor( iColor );

   hb_retc_null();
}

HB_FUNC( HT_GETCLEARB )
{
   hb_retni( hb_gtGetClearChar() );
}

HB_FUNC( HT_SETCLEARB )
{
   HB_USHORT usNew;

   if( HB_ISNUM( 1 ) )
   {
      int iChar = hb_parni( 1 );
      PHB_CODEPAGE cdp = hb_vmCDP();

      if( ! HB_CDP_ISCHARUNI( cdp ) )
         iChar = hb_cdpGetU16( cdp, ( HB_UCHAR ) iChar );
      usNew = ( HB_USHORT ) iChar;
   }
   else if( HB_ISCHAR( 1 ) )
      usNew = hb_cdpTextGetU16( hb_vmCDP(), hb_parc( 1 ), hb_parclen( 1 ) );
   else
      usNew = ' ';

   hb_gtSetClearChar( usNew );

   hb_retc_null();
}
