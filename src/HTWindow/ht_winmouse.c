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
#include "hbapiitm.h"
#include "hbvm.h"

#include <string.h>

#if defined( HB_OS_WIN )

#include <windows.h>
#include <imm.h>

/* set by ht_viewport.c once a wide (CJK) character was written */
extern HB_BOOL ht_fWideWritten;

#ifndef HB_GTI_WINHANDLE
#define HB_GTI_WINHANDLE  69
#endif

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

/* Places the IME composition window at the caret (gtwvt keeps the system
   caret on the GT cursor) and the candidate list below the cursor row. */
static void ht_ime_place( HWND hWnd )
{
   static HB_BOOL s_fLoaded = HB_FALSE;
   static HT_IMMGETCONTEXT           s_pGetContext     = NULL;
   static HT_IMMRELEASECONTEXT       s_pReleaseContext = NULL;
   static HT_IMMSETCOMPOSITIONWINDOW s_pSetComposition = NULL;
   static HT_IMMSETCANDIDATEWINDOW   s_pSetCandidate   = NULL;

   POINT pt;
   RECT  rc;
   HIMC  hIMC;
   int   nRows, nCellH;

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
      }
   }
   if( ! s_pGetContext || ! s_pReleaseContext || ! s_pSetComposition || ! s_pSetCandidate )
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

static LRESULT CALLBACK ht_WndProc( HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam )
{
   LRESULT lResult;
   int nKey, nRow, nCol;

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
   if( message == WM_PAINT && ht_fWideWritten )
      ht_rows_collect( hWnd );
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

#else /* ! HB_OS_WIN */

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
