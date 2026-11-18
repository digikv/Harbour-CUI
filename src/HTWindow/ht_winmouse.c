/*
 * HBTui Win32 / gtwvt mouse bridge.
 *
 * Subclasses the GT window to receive WM_MOUSEMOVE / button messages,
 * queues them for HTEventLoop. MOVE events are coalesced (only the latest
 * position is kept) — this removes poll-driven flicker on drag.
 *
 * Optional Harbour hooks (CAD/HWGUI style), called from poll drain — never
 * from WndProc (avoids VM reentrancy):
 *   RegisterHtOnEventMouseMove  -> PROCEDURE HtOnEventMouseMove( nKey, nRow, nCol )
 *   RegisterHtOnEventMouseDown  -> PROCEDURE HtOnEventMouseDown( nKey, nRow, nCol )
 *   RegisterHtOnEventLBDblClick -> PROCEDURE HtOnEventLBDblClick( nKey, nRow, nCol )
 *
 * The subclass is installed with the WIDE (...W) API on purpose. hbmk2
 * compiles user C code as ANSI by default, so a bare SetWindowLongPtr()
 * would be SetWindowLongPtrA(): Windows then marks the gtwvt window as an
 * ANSI window and narrows every WM_CHAR to the system ANSI code page before
 * gtwvt sees it. Characters outside that code page (Chinese/Japanese/Korean
 * from an IME, Cyrillic on a Western system, ...) arrive as '?'.
 *
 * IME (Chinese / Japanese / Korean input): the composition window (the
 * text being typed) and the candidate list are placed at the screen cursor.
 * Left to the IME they open at the window's top-left corner or out of view
 * on the fullscreen window. imm32 is loaded at run time (no link library).
 *
 * Non-Windows: stubs; EventLoop falls back to Inkey + position polling.
 */

#include "hbapi.h"
#include "hbapigt.h"
#include "hbapicdp.h"
#include "hbapiitm.h"
#include "hbapistr.h"
#include "hbvm.h"
#include "hbgtinfo.ch"

#include <string.h>

#if defined( HB_OS_WIN )

#include <windows.h>
#include <imm.h>

/* set by ht_viewport.c once a wide (CJK) character was written */
extern HB_BOOL ht_fWideWritten;

#ifndef K_MOUSEMOVE
#define K_MOUSEMOVE     1001
#endif
#ifndef K_LBUTTONDOWN
#define K_LBUTTONDOWN   1002
#endif
#ifndef K_LBUTTONUP
#define K_LBUTTONUP     1003
#endif
#ifndef K_RBUTTONDOWN
#define K_RBUTTONDOWN   1004
#endif
#ifndef K_RBUTTONUP
#define K_RBUTTONUP     1005
#endif
#ifndef K_LDBLCLK
#define K_LDBLCLK       1006
#endif
#ifndef K_RDBLCLK
#define K_RDBLCLK       1007
#endif

typedef struct
{
   int nKey;
   int nRow;
   int nCol;
} HT_MOUSE_EVT;

#define HT_MOUSE_QSIZE  64

static WNDPROC          s_oldProc      = NULL;
static HWND             s_hWnd         = NULL;
static HB_BOOL          s_fInstalled   = HB_FALSE;

static HB_BOOL          s_fHaveMove    = HB_FALSE;
static HT_MOUSE_EVT     s_moveEvt;

static HT_MOUSE_EVT     s_queue[ HT_MOUSE_QSIZE ];
static int              s_qHead        = 0;
static int              s_qTail        = 0;
static int              s_qCount       = 0;

static PHB_DYNS pSym_MouseMove   = NULL;
static PHB_DYNS pSym_MouseDown   = NULL;
static PHB_DYNS pSym_LBDblClick  = NULL;

static void ht_mouse_q_push( int nKey, int nRow, int nCol )
{
   if( s_qCount >= HT_MOUSE_QSIZE )
   {
      s_qHead = ( s_qHead + 1 ) % HT_MOUSE_QSIZE;
      s_qCount--;
   }
   s_queue[ s_qTail ].nKey = nKey;
   s_queue[ s_qTail ].nRow = nRow;
   s_queue[ s_qTail ].nCol = nCol;
   s_qTail = ( s_qTail + 1 ) % HT_MOUSE_QSIZE;
   s_qCount++;
}

static HB_BOOL ht_mouse_q_pop( HT_MOUSE_EVT * pEvt )
{
   if( s_qCount <= 0 )
      return HB_FALSE;
   *pEvt = s_queue[ s_qHead ];
   s_qHead = ( s_qHead + 1 ) % HT_MOUSE_QSIZE;
   s_qCount--;
   return HB_TRUE;
}

static void ht_store_mouse( int nKey, int nRow, int nCol )
{
   if( nKey == K_MOUSEMOVE )
   {
      s_moveEvt.nKey = nKey;
      s_moveEvt.nRow = nRow;
      s_moveEvt.nCol = nCol;
      s_fHaveMove = HB_TRUE;
   }
   else
      ht_mouse_q_push( nKey, nRow, nCol );
}

static int ht_msg_to_key( UINT message )
{
   switch( message )
   {
      case WM_MOUSEMOVE:     return K_MOUSEMOVE;
      case WM_LBUTTONDOWN:   return K_LBUTTONDOWN;
      case WM_LBUTTONUP:     return K_LBUTTONUP;
      case WM_RBUTTONDOWN:   return K_RBUTTONDOWN;
      case WM_RBUTTONUP:     return K_RBUTTONUP;
      case WM_LBUTTONDBLCLK: return K_LDBLCLK;
      case WM_RBUTTONDBLCLK: return K_RDBLCLK;
   }
   return 0;
}

/* rows to repaint whole once WM_PAINT returned (see ht_WndProc) */
static RECT    s_rcRows;
static HB_BOOL s_fRowsPending = HB_FALSE;

static UINT ht_rows_msg( void )
{
   static UINT s_uMsg = 0;

   if( s_uMsg == 0 )
      s_uMsg = RegisterWindowMessageW( L"HbTuiRepaintRows" );
   return s_uMsg;
}

/* adds the rows of every partial-width rectangle of the update region to
   s_rcRows and posts the repaint message once */
static void ht_rows_collect( HWND hWnd )
{
   RECT  rcClient;
   HRGN  hRgn;
   DWORD nSize;

   if( ! GetClientRect( hWnd, &rcClient ) )
      return;

   hRgn = CreateRectRgn( 0, 0, 0, 0 );
   if( hRgn == NULL )
      return;

   if( GetUpdateRgn( hWnd, hRgn, FALSE ) > NULLREGION &&
       ( nSize = GetRegionData( hRgn, 0, NULL ) ) > 0 )
   {
      RGNDATA * pData = ( RGNDATA * ) hb_xgrab( nSize );

      if( GetRegionData( hRgn, nSize, pData ) )
      {
         const RECT * pRect = ( const RECT * ) pData->Buffer;
         DWORD i;

         for( i = 0; i < pData->rdh.nCount; ++i )
         {
            if( pRect[ i ].left > rcClient.left || pRect[ i ].right < rcClient.right )
            {
               RECT rcRow = pRect[ i ];

               rcRow.left  = rcClient.left;
               rcRow.right = rcClient.right;
               if( s_fRowsPending )
                  UnionRect( &s_rcRows, &s_rcRows, &rcRow );
               else
               {
                  s_rcRows = rcRow;
                  s_fRowsPending = HB_TRUE;
                  PostMessageW( hWnd, ht_rows_msg(), 0, 0 );
               }
            }
         }
      }
      hb_xfree( pData );
   }
   DeleteObject( hRgn );
}

typedef HIMC ( WINAPI * HT_IMMGETCONTEXT )( HWND );
typedef BOOL ( WINAPI * HT_IMMRELEASECONTEXT )( HWND, HIMC );
typedef BOOL ( WINAPI * HT_IMMSETCOMPOSITIONWINDOW )( HIMC, LPCOMPOSITIONFORM );
typedef BOOL ( WINAPI * HT_IMMSETCANDIDATEWINDOW )( HIMC, LPCANDIDATEFORM );
typedef BOOL ( WINAPI * HT_IMMGETOPENSTATUS )( HIMC );
typedef BOOL ( WINAPI * HT_IMMGETCONVERSIONSTATUS )( HIMC, LPDWORD, LPDWORD );

static HT_IMMGETCONTEXT           s_pGetContext     = NULL;
static HT_IMMRELEASECONTEXT       s_pReleaseContext = NULL;
static HT_IMMSETCOMPOSITIONWINDOW s_pSetComposition = NULL;
static HT_IMMSETCANDIDATEWINDOW   s_pSetCandidate   = NULL;
static HT_IMMGETOPENSTATUS        s_pGetOpenStatus  = NULL;
static HT_IMMGETCONVERSIONSTATUS  s_pGetConversion  = NULL;

/* imm32 functions, loaded once at run time; .F. without imm32 */
static HB_BOOL ht_imm_load( void )
{
   static HB_BOOL s_fLoaded = HB_FALSE;

   if( ! s_fLoaded )
   {
      HMODULE hImm = LoadLibraryW( L"imm32.dll" );

      s_fLoaded = HB_TRUE;
      if( hImm )
      {
         s_pGetContext     = ( HT_IMMGETCONTEXT ) ( void * ) GetProcAddress( hImm, "ImmGetContext" );
         s_pReleaseContext = ( HT_IMMRELEASECONTEXT ) ( void * ) GetProcAddress( hImm, "ImmReleaseContext" );
         s_pSetComposition = ( HT_IMMSETCOMPOSITIONWINDOW ) ( void * ) GetProcAddress( hImm, "ImmSetCompositionWindow" );
         s_pSetCandidate   = ( HT_IMMSETCANDIDATEWINDOW ) ( void * ) GetProcAddress( hImm, "ImmSetCandidateWindow" );
         s_pGetOpenStatus  = ( HT_IMMGETOPENSTATUS ) ( void * ) GetProcAddress( hImm, "ImmGetOpenStatus" );
         s_pGetConversion  = ( HT_IMMGETCONVERSIONSTATUS ) ( void * ) GetProcAddress( hImm, "ImmGetConversionStatus" );
      }
   }
   return s_pGetContext && s_pReleaseContext;
}

/* Places the IME composition window at the caret (gtwvt keeps the system
   caret on the GT cursor) and the candidate list below the cursor row. */
static void ht_ime_place( HWND hWnd )
{
   POINT pt;
   RECT  rc;
   HIMC  hIMC;
   int   nRows, nCellH;

   if( ! ht_imm_load() || ! s_pSetComposition || ! s_pSetCandidate )
      return;

   /* no caret: the cursor is hidden (SC_NONE), leave the IME's own place */
   if( ! GetCaretPos( &pt ) || ! GetClientRect( hWnd, &rc ) )
      return;

   nRows = hb_gtMaxRow() + 1;
   nCellH = nRows > 0 ? ( rc.bottom - rc.top ) / nRows : 16;
   if( nCellH <= 0 )
      nCellH = 16;
   /* gtwvt puts an underline caret at the bottom of the cell */
   pt.y = ( pt.y / nCellH ) * nCellH;

   hIMC = s_pGetContext( hWnd );
   if( hIMC )
   {
      COMPOSITIONFORM cf;
      CANDIDATEFORM   cand;

      memset( &cf, 0, sizeof( cf ) );
      cf.dwStyle = CFS_POINT;
      cf.ptCurrentPos = pt;
      s_pSetComposition( hIMC, &cf );

      memset( &cand, 0, sizeof( cand ) );
      cand.dwIndex = 0;
      cand.dwStyle = CFS_EXCLUDE;
      cand.ptCurrentPos.x = pt.x;
      cand.ptCurrentPos.y = pt.y + nCellH;
      cand.rcArea.left   = pt.x;
      cand.rcArea.top    = pt.y;
      cand.rcArea.right  = pt.x + 1;
      cand.rcArea.bottom = pt.y + nCellH;
      s_pSetCandidate( hIMC, &cand );

      s_pReleaseContext( hWnd, hIMC );
   }
}

/* ---- Indic scripts (Devanagari, Bengali, Gurmukhi, Gujarati, Oriya,
 * Tamil, Telugu, Kannada, Malayalam: U+0900..U+0D7F)
 *
 * A syllable is a consonant (or vowel) with vowel signs (matras) and other
 * marks, consonants joined by a virama form one conjunct. The GT keeps one
 * code point per cell, so a syllable takes as many cells as it has code
 * points - the editors' cursor arithmetic (Len()) stays as it is.
 * gtwvt draws a run of same-colored cells with one ExtTextOut(): Windows
 * shapes the syllables but advances by the glyphs' own widths, so the rest
 * of the run (Latin text after Gujarati, too) drifts off the cell grid, and
 * a repaint starting inside a syllable draws its marks alone. After gtwvt
 * painted, every run holding Indic text is drawn again: each syllable
 * shaped and centered on its cells (narrowed when wider), every other
 * character on its own cell in the GT font. */

static HB_BOOL ht_wc_isindic( HB_WCHAR wc )
{
   return wc >= 0x0900 && wc <= 0x0D7F;
}

/* the signs that belong to the character before them; the nine scripts
   share the layout of their 128-character blocks */
static HB_BOOL ht_wc_isindicmark( HB_WCHAR wc )
{
   int o;

   if( wc == 0x200C || wc == 0x200D )   /* ZWNJ, ZWJ */
      return HB_TRUE;
   if( ! ht_wc_isindic( wc ) )
      return HB_FALSE;
   o = wc & 0x7F;
   return ( o >= 0x01 && o <= 0x03 ) || ( wc == 0x0900 || wc == 0x0C00 || wc == 0x0D00 ) ||
          ( o >= 0x3A && o <= 0x3C ) || ( o >= 0x3E && o <= 0x4F ) ||
          ( o >= 0x51 && o <= 0x57 ) || o == 0x62 || o == 0x63;
}

/* cells of the syllable starting at pwc[ i ] */
static int ht_indic_cluster( const HB_WCHAR * pwc, int i, int n )
{
   int iLen = 1;

   while( i + iLen < n && iLen < 16 )
   {
      HB_WCHAR wc = pwc[ i + iLen ], wcPrev = pwc[ i + iLen - 1 ];

      if( ht_wc_isindicmark( wc ) ||
          ( ht_wc_isindic( wc ) &&
            ( wcPrev == 0x200D || ( ht_wc_isindic( wcPrev ) && ( wcPrev & 0x7F ) == 0x4D ) ) ) )
         ++iLen;
      else
         break;
   }
   return iLen;
}

static int ht_gt_info_int( int iType )
{
   HB_GT_INFO gtInfo;
   int iValue;

   memset( &gtInfo, 0, sizeof( gtInfo ) );
   gtInfo.pResult = hb_itemNew( NULL );
   hb_gtInfo( iType, &gtInfo );
   iValue = hb_itemGetNI( gtInfo.pResult );
   hb_itemRelease( gtInfo.pResult );
   return iValue;
}

static HFONT   s_hFontBase  = NULL;
static HFONT   s_hFontIndic = NULL;
static int     s_iFontKey[ 6 ];
static WCHAR   s_wszFace[ LF_FACESIZE ];

/* the GT font (as gtwvt creates it) and the Indic font, for cell height
   iCellH; recreated when the GT font changes */
static HB_BOOL ht_indic_fonts( int iCellW, int iCellH )
{
   HB_GT_INFO gtInfo;
   WCHAR wszFace[ LF_FACESIZE ];
   int aKey[ 6 ];
   LOGFONTW lf;

   memset( &gtInfo, 0, sizeof( gtInfo ) );
   gtInfo.pResult = hb_itemNew( NULL );
   hb_gtInfo( HB_GTI_FONTNAME, &gtInfo );
   if( MultiByteToWideChar( CP_UTF8, 0, hb_itemGetCPtr( gtInfo.pResult ), -1, wszFace, LF_FACESIZE ) == 0 )
      wszFace[ 0 ] = 0;
   wszFace[ LF_FACESIZE - 1 ] = 0;
   hb_itemRelease( gtInfo.pResult );

   aKey[ 0 ] = iCellW;
   aKey[ 1 ] = iCellH;
   aKey[ 2 ] = ht_gt_info_int( HB_GTI_FONTWIDTH );
   aKey[ 3 ] = ht_gt_info_int( HB_GTI_FONTWEIGHT );
   aKey[ 4 ] = ht_gt_info_int( HB_GTI_FONTQUALITY );
   aKey[ 5 ] = ht_gt_info_int( HB_GTI_CODEPAGE );

   if( s_hFontBase && s_hFontIndic && memcmp( aKey, s_iFontKey, sizeof( aKey ) ) == 0 &&
       wcscmp( wszFace, s_wszFace ) == 0 )
      return HB_TRUE;

   if( s_hFontBase )
      DeleteObject( s_hFontBase );
   if( s_hFontIndic )
      DeleteObject( s_hFontIndic );

   memset( &lf, 0, sizeof( lf ) );
   lf.lfHeight         = iCellH;
   lf.lfWidth          = aKey[ 2 ] < 0 ? -aKey[ 2 ] : aKey[ 2 ];
   lf.lfWeight         = aKey[ 3 ];
   lf.lfCharSet        = ( BYTE ) aKey[ 5 ];
   lf.lfQuality        = ( BYTE ) aKey[ 4 ];
   lf.lfPitchAndFamily = FIXED_PITCH | FF_MODERN;
   memcpy( lf.lfFaceName, wszFace, sizeof( wszFace ) );
   s_hFontBase = CreateFontIndirectW( &lf );

   /* Nirmala UI (Windows 8+) has every Indic script */
   memset( &lf, 0, sizeof( lf ) );
   lf.lfHeight         = iCellH;
   lf.lfWeight         = aKey[ 3 ];
   lf.lfCharSet        = DEFAULT_CHARSET;
   lf.lfQuality        = ( BYTE ) aKey[ 4 ];
   lf.lfPitchAndFamily = VARIABLE_PITCH | FF_SWISS;
   wcscpy( lf.lfFaceName, L"Nirmala UI" );
   s_hFontIndic = CreateFontIndirectW( &lf );

   memcpy( s_iFontKey, aKey, sizeof( aKey ) );
   memcpy( s_wszFace, wszFace, sizeof( wszFace ) );
   return s_hFontBase && s_hFontIndic;
}

/* draws pwc[ iFrom .. iTo - 1 ] (one run, color iColor) on its cells */
static void ht_indic_run( HDC hdc, const HB_WCHAR * pwc, int iFrom, int iTo, int x0, int y,
                          int iCellW, int iCellH, COLORREF fg, COLORREF bg )
{
   RECT rc;
   HBRUSH hBrush;
   int i, iSaved;

   SetRect( &rc, x0 + iFrom * iCellW, y, x0 + iTo * iCellW, y + iCellH );
   hBrush = CreateSolidBrush( bg );
   FillRect( hdc, &rc, hBrush );
   DeleteObject( hBrush );

   iSaved = SaveDC( hdc );
   IntersectClipRect( hdc, rc.left, rc.top, rc.right, rc.bottom );
   SetBkMode( hdc, TRANSPARENT );
   SetTextColor( hdc, fg );
   SetTextAlign( hdc, TA_LEFT | TA_TOP );
   SetGraphicsMode( hdc, GM_ADVANCED );

   for( i = iFrom; i < iTo; )
   {
      int x = x0 + i * iCellW;

      if( ht_wc_isindic( pwc[ i ] ) && ! ht_wc_isindicmark( pwc[ i ] ) )
      {
         int iLen = ht_indic_cluster( pwc, i, iTo );
         int iAvail = iLen * iCellW;
         WCHAR wsz[ 16 ];
         SIZE size;
         int n;

         size.cx = 0;
         for( n = 0; n < iLen; ++n )
            wsz[ n ] = ( WCHAR ) pwc[ i + n ];

         SelectObject( hdc, s_hFontIndic );
         if( GetTextExtentPoint32W( hdc, wsz, iLen, &size ) && size.cx > iAvail )
         {
            XFORM xf;

            xf.eM11 = ( FLOAT ) iAvail / ( FLOAT ) size.cx;
            xf.eM12 = xf.eM21 = 0;
            xf.eM22 = 1;
            xf.eDx  = ( FLOAT ) x * ( 1 - xf.eM11 );
            xf.eDy  = 0;
            SetWorldTransform( hdc, &xf );
            ExtTextOutW( hdc, x, y, 0, NULL, wsz, iLen, NULL );
            ModifyWorldTransform( hdc, NULL, MWT_IDENTITY );
         }
         else
            ExtTextOutW( hdc, x + ( iAvail - size.cx ) / 2, y, 0, NULL, wsz, iLen, NULL );
         i += iLen;
      }
      else
      {
         WCHAR wc = ( WCHAR ) hb_cdpGetU16Ctrl( pwc[ i ] );

         if( wc != ' ' && wc != 0x200B )
         {
            SelectObject( hdc, s_hFontBase );
            ExtTextOutW( hdc, x, y, 0, NULL, &wc, 1, NULL );
         }
         ++i;
      }
   }
   RestoreDC( hdc, iSaved );
}

/* draws again the runs holding Indic text on the rows of rcUpdate */
static void ht_indic_paint( HWND hWnd, const RECT * prcUpdate )
{
   int iRows = hb_gtMaxRow() + 1, iCols = hb_gtMaxCol() + 1;
   int iCellW, iCellH, x0, y0, iTop, iBottom, iRow;
   HB_WCHAR * pwc = NULL;
   int * piColor = NULL;
   COLORREF aPal[ 16 ];
   HB_BOOL fPal = HB_FALSE, fCaretHidden = HB_FALSE;
   HDC hdc = NULL;
   RECT rcClient;

   if( iRows <= 0 || iCols <= 0 || ! GetClientRect( hWnd, &rcClient ) )
      return;
   iCellW = ht_gt_info_int( HB_GTI_SCREENWIDTH ) / iCols;
   iCellH = ht_gt_info_int( HB_GTI_SCREENHEIGHT ) / iRows;
   if( iCellW <= 0 || iCellH <= 0 )
      return;
   x0 = HB_MAX( 0, ( rcClient.right - iCellW * iCols ) / 2 );
   y0 = HB_MAX( 0, ( rcClient.bottom - iCellH * iRows ) / 2 );

   iTop    = HB_MAX( 0, ( prcUpdate->top - y0 ) / iCellH );
   iBottom = HB_MIN( iRows - 1, ( prcUpdate->bottom - 1 - y0 ) / iCellH );

   for( iRow = iTop; iRow <= iBottom; ++iRow )
   {
      HB_BOOL fIndic = HB_FALSE;
      int iCol, iFrom;

      if( pwc == NULL )
      {
         pwc = ( HB_WCHAR * ) hb_xgrab( sizeof( HB_WCHAR ) * iCols );
         piColor = ( int * ) hb_xgrab( sizeof( int ) * iCols );
      }
      for( iCol = 0; iCol < iCols; ++iCol )
      {
         int iColor;
         HB_BYTE bAttr;
         HB_USHORT usChar;

         if( hb_gtGetScrChar( iRow, iCol, &iColor, &bAttr, &usChar ) != HB_SUCCESS )
         {
            usChar = ' ';
            iColor = 0;
         }
         pwc[ iCol ] = ( HB_WCHAR ) usChar;
         piColor[ iCol ] = iColor & 0xFF;
         if( ht_wc_isindic( pwc[ iCol ] ) )
            fIndic = HB_TRUE;
      }
      if( ! fIndic )
         continue;

      if( hdc == NULL )
      {
         if( ! ht_indic_fonts( iCellW, iCellH ) || ( hdc = GetDC( hWnd ) ) == NULL )
            break;
         fCaretHidden = HideCaret( hWnd );
      }
      if( ! fPal )
      {
         HB_GT_INFO gtInfo;
         int i;

         memset( &gtInfo, 0, sizeof( gtInfo ) );
         gtInfo.pResult = hb_itemNew( NULL );
         hb_gtInfo( HB_GTI_PALETTE, &gtInfo );
         for( i = 0; i < 16; ++i )
            aPal[ i ] = ( COLORREF ) hb_arrayGetNL( gtInfo.pResult, i + 1 );
         hb_itemRelease( gtInfo.pResult );
         fPal = HB_TRUE;
      }

      /* runs as gtwvt draws them: same color */
      for( iFrom = 0; iFrom < iCols; )
      {
         int iTo = iFrom + 1;
         HB_BOOL fRunIndic = ht_wc_isindic( pwc[ iFrom ] );

         while( iTo < iCols && piColor[ iTo ] == piColor[ iFrom ] )
         {
            if( ht_wc_isindic( pwc[ iTo ] ) )
               fRunIndic = HB_TRUE;
            ++iTo;
         }
         if( fRunIndic )
            ht_indic_run( hdc, pwc, iFrom, iTo, x0, y0 + iRow * iCellH, iCellW, iCellH,
                          aPal[ piColor[ iFrom ] & 0x0F ], aPal[ ( piColor[ iFrom ] >> 4 ) & 0x0F ] );
         iFrom = iTo;
      }
   }

   if( hdc )
   {
      if( fCaretHidden )
         ShowCaret( hWnd );
      ReleaseDC( hWnd, hdc );
   }
   if( pwc )
   {
      hb_xfree( pwc );
      hb_xfree( piColor );
   }
}

static LRESULT CALLBACK ht_WndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   LRESULT lResult;
   int nKey, nRow, nCol;
   RECT rcPaint;
   HB_BOOL fIndicPaint = HB_FALSE;

   /* Wide (CJK) characters spill over their filler cell (ht_viewport.c).
      gtwvt repaints only the changed part of a row; when that part starts
      on a filler cell, its opaque background erases the right half of the
      glyph. Once wide text was written, the rows of each repaint are
      repainted whole - but not from inside WM_PAINT: an InvalidateRect()
      there made Windows repaint the whole window, background erase
      included, right after (every caret blink flashed the screen; an
      already full-width update looped ~20 times a second). The widened
      rows are collected and invalidated once WM_PAINT has returned.
      Each rectangle of the update region counts: the bounding rectangle
      (GetUpdateRect) is full width as soon as one whole row is in it, and
      the partial rows next to it would be left out. */
   if( message == WM_PAINT )
   {
      if( ht_fWideWritten )
         ht_rows_collect( hWnd );
      fIndicPaint = HB_CDP_ISCUSTOM( hb_vmCDP() ) && GetUpdateRect( hWnd, &rcPaint, FALSE );
   }
   else if( message != 0 && message == ht_rows_msg() )
   {
      if( s_fRowsPending )
      {
         s_fRowsPending = HB_FALSE;
         InvalidateRect( hWnd, &s_rcRows, FALSE );
      }
      return 0;
   }

   if( message == WM_IME_STARTCOMPOSITION || message == WM_IME_COMPOSITION ||
       ( message == WM_IME_NOTIFY && wParam == IMN_OPENCANDIDATE ) )
      ht_ime_place( hWnd );

   /* Let gtwvt update its mouse state first (W: keeps WM_CHAR in UTF-16) */
   if( s_oldProc )
      lResult = CallWindowProcW( s_oldProc, hWnd, message, wParam, lParam );
   else
      lResult = DefWindowProcW( hWnd, message, wParam, lParam );

   if( fIndicPaint )
      ht_indic_paint( hWnd, &rcPaint );

   nKey = ht_msg_to_key( message );
   if( nKey != 0 )
   {
      hb_mouseGetPos( &nRow, &nCol );
      ht_store_mouse( nKey, nRow, nCol );
   }

   return lResult;
}

static HWND ht_get_gt_hwnd( void )
{
   HB_GT_INFO gtInfo;
   HWND hWnd = NULL;

   memset( &gtInfo, 0, sizeof( gtInfo ) );
   gtInfo.pResult = hb_itemNew( NULL );
   hb_gtInfo( HB_GTI_WINHANDLE, &gtInfo );

   if( HB_IS_POINTER( gtInfo.pResult ) )
      hWnd = ( HWND ) hb_itemGetPtr( gtInfo.pResult );
   else if( HB_IS_NUMERIC( gtInfo.pResult ) )
      hWnd = ( HWND )( HB_PTRUINT ) hb_itemGetNInt( gtInfo.pResult );

   hb_itemRelease( gtInfo.pResult );
   return hWnd;
}

static void ht_call_harbour( PHB_DYNS pDyn, int nKey, int nRow, int nCol )
{
   if( pDyn )
   {
      hb_vmPushDynSym( pDyn );
      hb_vmPushNil();
      hb_vmPushInteger( nKey );
      hb_vmPushInteger( nRow );
      hb_vmPushInteger( nCol );
      hb_vmFunction( 3 );
   }
}

HB_FUNC( HT_WINMOUSEINSTALL )
{
   HWND hWnd;

   if( s_fInstalled )
   {
      hb_retl( HB_TRUE );
      return;
   }

   hWnd = ht_get_gt_hwnd();
   if( ! hWnd || ! IsWindow( hWnd ) )
   {
      hb_retl( HB_FALSE );
      return;
   }

   /* W, not A: an ANSI subclass turns the window into an ANSI window and
      IME / Unicode characters reach gtwvt as '?' (see the header comment) */
   s_oldProc = ( WNDPROC ) SetWindowLongPtrW( hWnd, GWLP_WNDPROC, ( LONG_PTR ) ht_WndProc );
   if( ! s_oldProc )
   {
      hb_retl( HB_FALSE );
      return;
   }

   s_hWnd = hWnd;
   s_fInstalled = HB_TRUE;
   s_fHaveMove = HB_FALSE;
   s_qHead = s_qTail = s_qCount = 0;
   hb_retl( HB_TRUE );
}

HB_FUNC( HT_WINMOUSEUNINSTALL )
{
   if( s_fInstalled && s_hWnd && s_oldProc && IsWindow( s_hWnd ) )
      SetWindowLongPtrW( s_hWnd, GWLP_WNDPROC, ( LONG_PTR ) s_oldProc );

   s_oldProc = NULL;
   s_hWnd = NULL;
   s_fInstalled = HB_FALSE;
   s_fHaveMove = HB_FALSE;
   s_qCount = 0;
   hb_retl( HB_TRUE );
}

HB_FUNC( HT_WINMOUSEACTIVE )
{
   hb_retl( s_fInstalled );
}

/*
 * ht_WinMouseNext( @nRow, @nCol ) -> nKey | 0
 * Clicks first, then coalesced move. Optional Harbour hooks fire here.
 */
HB_FUNC( HT_WINMOUSENEXT )
{
   HT_MOUSE_EVT evt;
   HB_BOOL fGot = HB_FALSE;

   if( ht_mouse_q_pop( &evt ) )
      fGot = HB_TRUE;
   else if( s_fHaveMove )
   {
      evt = s_moveEvt;
      s_fHaveMove = HB_FALSE;
      fGot = HB_TRUE;
   }

   if( ! fGot )
   {
      hb_retni( 0 );
      return;
   }

   if( HB_ISBYREF( 1 ) )
      hb_storni( evt.nRow, 1 );
   if( HB_ISBYREF( 2 ) )
      hb_storni( evt.nCol, 2 );

   if( evt.nKey == K_MOUSEMOVE )
      ht_call_harbour( pSym_MouseMove, evt.nKey, evt.nRow, evt.nCol );
   else if( evt.nKey == K_LBUTTONDOWN || evt.nKey == K_RBUTTONDOWN )
      ht_call_harbour( pSym_MouseDown, evt.nKey, evt.nRow, evt.nCol );
   else if( evt.nKey == K_LDBLCLK || evt.nKey == K_RDBLCLK )
      ht_call_harbour( pSym_LBDblClick, evt.nKey, evt.nRow, evt.nCol );

   hb_retni( evt.nKey );
}

HB_FUNC( HT_WINMOUSEPENDING )
{
   hb_retl( s_fInstalled && ( s_fHaveMove || s_qCount > 0 ) );
}

HB_FUNC( REGISTERHTONEVENTMOUSEMOVE )
{
   if( ! pSym_MouseMove )
      pSym_MouseMove = hb_dynsymFind( "HTONEVENTMOUSEMOVE" );
   hb_retl( pSym_MouseMove != NULL );
}

HB_FUNC( REGISTERHTONEVENTMOUSEDOWN )
{
   if( ! pSym_MouseDown )
      pSym_MouseDown = hb_dynsymFind( "HTONEVENTMOUSEDOWN" );
   hb_retl( pSym_MouseDown != NULL );
}

HB_FUNC( REGISTERHTONEVENTLBDBLCLICK )
{
   if( ! pSym_LBDblClick )
      pSym_LBDblClick = hb_dynsymFind( "HTONEVENTLBDBLCLICK" );
   hb_retl( pSym_LBDblClick != NULL );
}

/* HT_FONTHASCHAR( cFontName, nUnicode ) -> lHas
 * .T. when the font has a glyph for the character. gtwvt draws with GDI,
 * which for symbols (e.g. the block elements U+2581..U+2587, missing in
 * Consolas) does not fall back to another font: a missing glyph is a box.
 * .T. when it cannot tell (no font name). */
HB_FUNC( HT_FONTHASCHAR )
{
   const char * szFont = hb_parc( 1 );
   HB_BOOL fHas = HB_TRUE;

   if( szFont && *szFont && HB_ISNUM( 2 ) )
   {
      WCHAR wszFont[ LF_FACESIZE ];
      HDC   hdc = CreateCompatibleDC( NULL );

      MultiByteToWideChar( CP_ACP, 0, szFont, -1, wszFont, LF_FACESIZE );
      wszFont[ LF_FACESIZE - 1 ] = 0;
      if( hdc )
      {
         HFONT hFont = CreateFontW( 16, 0, 0, 0, FW_NORMAL, FALSE, FALSE, FALSE, DEFAULT_CHARSET,
                                    OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
                                    FIXED_PITCH | FF_MODERN, wszFont );
         if( hFont )
         {
            HGDIOBJ hOld = SelectObject( hdc, hFont );
            WCHAR wc = ( WCHAR ) hb_parni( 2 );
            WORD  wIndex = 0;

            if( GetGlyphIndicesW( hdc, &wc, 1, &wIndex, GGI_MARK_NONEXISTING_GLYPHS ) == 1 )
               fHas = wIndex != 0xFFFF;
            SelectObject( hdc, hOld );
            DeleteObject( hFont );
         }
         DeleteDC( hdc );
      }
   }
   hb_retl( fHas );
}

/* HT_FONTLOAD( cFile ) -> lOk
 * Makes a font file (.ttf/.otf/.fon) usable by this process only - no
 * installation, gone when the program ends. Then select it by its family
 * name: hb_gtInfo( HB_GTI_FONTNAME, "Nebula8" ). */
HB_FUNC( HT_FONTLOAD )
{
   const char * szFile = hb_parc( 1 );
   int nAdded = 0;

   if( szFile && *szFile )
   {
      WCHAR wszFile[ MAX_PATH ];

      if( MultiByteToWideChar( CP_UTF8, 0, szFile, -1, wszFile, MAX_PATH ) > 0 )
         nAdded = AddFontResourceExW( wszFile, FR_PRIVATE, NULL );
   }
   hb_retl( nAdded > 0 );
}

/* HT_KEYDOWN( nVirtualKey ) -> lDown
 * Whether a key is held down now (VK_LEFT = 0x25, VK_SPACE = 0x20, 'Z' = 90,
 * ...) - several keys at once, no keyboard repeat delay: games. Only while
 * the GT window is the foreground window, so keys typed into another program
 * do not count. */
HB_FUNC( HT_KEYDOWN )
{
   HWND hWnd = s_hWnd ? s_hWnd : ht_get_gt_hwnd();

   hb_retl( HB_ISNUM( 1 ) && hWnd && GetForegroundWindow() == hWnd &&
            ( GetAsyncKeyState( hb_parni( 1 ) ) & 0x8000 ) != 0 );
}

/* HT_KEYSTATEAVAILABLE() -> .T. where HT_KEYDOWN() works */
HB_FUNC( HT_KEYSTATEAVAILABLE )
{
   hb_retl( HB_TRUE );
}

/* HT_INPUTLANGUAGE() -> cLanguage
 * The input language of the program ("EN", "GU", "ZH", ...) and, while an
 * IME is on, its mode: "ZH 中" / "ZH A" (Chinese / Latin letters), "JA あ" /
 * "JA カ" / "JA A", "KO 가" / "KO A". The taskbar shows this, but not over a
 * fullscreen window: a program shows it in its status bar. "" off Windows. */
HB_FUNC( HT_INPUTLANGUAGE )
{
   LANGID lang = LOWORD( ( DWORD_PTR ) GetKeyboardLayout( 0 ) );
   WCHAR wsz[ 24 ];
   int n = GetLocaleInfoW( MAKELCID( lang, SORT_DEFAULT ), LOCALE_SISO639LANGNAME, wsz, 16 );
   HWND hWnd = s_hWnd ? s_hWnd : ht_get_gt_hwnd();
   WORD wPrimary = PRIMARYLANGID( lang );

   if( n <= 1 )
   {
      hb_retc_null();
      return;
   }
   --n;   /* without the terminating zero */
   CharUpperBuffW( wsz, ( DWORD ) n );

   if( ( wPrimary == LANG_CHINESE || wPrimary == LANG_JAPANESE || wPrimary == LANG_KOREAN ) &&
       hWnd && ht_imm_load() && s_pGetOpenStatus && s_pGetConversion )
   {
      HIMC hIMC = s_pGetContext( hWnd );

      if( hIMC )
      {
         DWORD dwConv = 0, dwSent = 0;
         WCHAR wcMode = 'A';

         if( s_pGetOpenStatus( hIMC ) && s_pGetConversion( hIMC, &dwConv, &dwSent ) &&
             ( dwConv & IME_CMODE_NATIVE ) != 0 )
         {
            if( wPrimary == LANG_JAPANESE )
               wcMode = ( dwConv & IME_CMODE_KATAKANA ) != 0 ? 0x30AB : 0x3042;   /* カ あ */
            else if( wPrimary == LANG_KOREAN )
               wcMode = 0xAC00;   /* 가 */
            else
               wcMode = 0x4E2D;   /* 中 */
         }
         wsz[ n++ ] = ' ';
         wsz[ n++ ] = wcMode;
         s_pReleaseContext( hWnd, hIMC );
      }
   }
   hb_retstrlen_u16( HB_CDP_ENDIAN_NATIVE, wsz, n );
}

#else /* ! HB_OS_WIN */

HB_FUNC( HT_INPUTLANGUAGE )
{
   hb_retc_null();
}

/* HT_FONTLOAD, HT_KEYDOWN, HT_KEYSTATEAVAILABLE outside Windows: ht_x11.c
   (a file of its own: only programs that call them link Xlib) */

/* the X fonts (fixed, Unifont Cell) and terminals have the block elements */
HB_FUNC( HT_FONTHASCHAR )
{
   hb_retl( HB_TRUE );
}

HB_FUNC( HT_WINMOUSEINSTALL )
{
   hb_retl( HB_FALSE );
}

HB_FUNC( HT_WINMOUSEUNINSTALL )
{
   hb_retl( HB_TRUE );
}

HB_FUNC( HT_WINMOUSEACTIVE )
{
   hb_retl( HB_FALSE );
}

HB_FUNC( HT_WINMOUSENEXT )
{
   hb_retni( 0 );
}

HB_FUNC( HT_WINMOUSEPENDING )
{
   hb_retl( HB_FALSE );
}

HB_FUNC( REGISTERHTONEVENTMOUSEMOVE )
{
   hb_retl( HB_FALSE );
}

HB_FUNC( REGISTERHTONEVENTMOUSEDOWN )
{
   hb_retl( HB_FALSE );
}

HB_FUNC( REGISTERHTONEVENTLBDBLCLICK )
{
   hb_retl( HB_FALSE );
}

#endif /* HB_OS_WIN */
