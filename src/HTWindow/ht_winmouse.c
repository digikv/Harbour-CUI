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
      glyph. Once wide text was written, repaint whole rows instead. */
   if( message == WM_PAINT && ht_fWideWritten )
   {
      RECT rcUpd, rcClient;

      if( GetUpdateRect( hWnd, &rcUpd, FALSE ) && GetClientRect( hWnd, &rcClient ) )
      {
         rcUpd.left  = rcClient.left;
         rcUpd.right = rcClient.right;
         InvalidateRect( hWnd, &rcUpd, FALSE );
      }
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

#else /* ! HB_OS_WIN */

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
