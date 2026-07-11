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
      hb_gtWriteAt( iRow, iCol, pszString, nLen );

      if( bFreeReq )
         hb_xfree( pszString );

      hb_gtSetColorStr( szOldColor );
   }
   else if( hb_pcount() >= 3 )
   {
      pszString = ht_item_string_con( hb_param( 3, HB_IT_ANY ), &nLen, &bFreeReq );
      hb_gtWriteAt( iRow, iCol, pszString, nLen );

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
