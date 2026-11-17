/*
 * ht_x11.c - own fonts and held keys for gtxwc (X11), the counterparts of the
 * Windows functions in ht_winmouse.c.
 *
 * A file of its own: a static library links an object only when one of its
 * functions is used, so only programs that call these need Xlib (-lX11,
 * which gtxwc brings anyway). Without the X11 headers the stubs are built.
 *
 *   HT_FONTLOAD( cDir )         adds a folder with fonts.dir (BDF/PCF fonts)
 *                               to the X server's font path (xset +fp)
 *   HT_KEYDOWN( nVirtualKey )   key held down now (XQueryKeymap), only while
 *                               the window titled HB_GTI_WINTITLE has the focus
 *   HT_KEYSTATEAVAILABLE()      .T. when the X display can be opened
 *
 * Virtual key codes are the Windows ones, so a program reads keys the same
 * way on both: 0x25..0x28 arrows, 0x20 space, 0x0D enter, 0x1B escape,
 * '0'..'9', 'A'..'Z'.
 */

#include "hbapi.h"

#if ! defined( HB_OS_WIN )

#include "hbapigt.h"
#include "hbapiitm.h"
#include "hbdate.h"
#include "hbgtinfo.ch"

#include <string.h>

#if defined( __has_include )
#  if __has_include( <X11/Xlib.h> )
#     define HT_HAVE_X11
#  endif
#endif

#if defined( HT_HAVE_X11 )

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>

static Display * s_dpy = NULL;
static HB_BOOL   s_fTried = HB_FALSE;

static Display * ht_x11_display( void )
{
   if( ! s_fTried )
   {
      s_fTried = HB_TRUE;
      s_dpy = XOpenDisplay( NULL );
   }
   return s_dpy;
}

HB_FUNC( HT_FONTLOAD )
{
   const char * szPath = hb_parc( 1 );
   Display * dpy = ht_x11_display();
   HB_BOOL fOk = HB_FALSE;

   if( dpy && szPath && *szPath )
   {
      char   szDir[ 4096 ];
      char   szCheck[ 4200 ];
      char * pSlash;
      int    nPaths = 0, i;
      char ** aPaths;
      FILE * fh;

      /* a font file names its folder; the folder must hold fonts.dir */
      hb_strncpy( szDir, szPath, sizeof( szDir ) - 1 );
      pSlash = strrchr( szDir, '/' );
      if( pSlash && ( strstr( pSlash, ".bdf" ) || strstr( pSlash, ".pcf" ) ) )
         *pSlash = '\0';
      hb_snprintf( szCheck, sizeof( szCheck ), "%s/fonts.dir", szDir );
      fh = fopen( szCheck, "r" );
      if( fh )
      {
         fclose( fh );
         aPaths = XGetFontPath( dpy, &nPaths );
         for( i = 0; i < nPaths && ! fOk; ++i )
         {
            /* entries may end with "/" or ":unscaled" */
            if( strncmp( aPaths[ i ], szDir, strlen( szDir ) ) == 0 )
               fOk = HB_TRUE;
         }
         if( ! fOk )
         {
            char ** aNew = ( char ** ) hb_xgrab( sizeof( char * ) * ( nPaths + 1 ) );

            for( i = 0; i < nPaths; ++i )
               aNew[ i ] = aPaths[ i ];
            aNew[ nPaths ] = szDir;
            XSetFontPath( dpy, aNew, nPaths + 1 );
            XSync( dpy, False );
            hb_xfree( aNew );
            fOk = HB_TRUE;
         }
         if( aPaths )
            XFreeFontPath( aPaths );
      }
   }
   hb_retl( fOk );
}

/* the GT window's title (HB_GTI_WINTITLE) */
static void ht_x11_title( char * szTitle, HB_SIZE nSize )
{
   HB_GT_INFO gtInfo;

   memset( &gtInfo, 0, sizeof( gtInfo ) );
   gtInfo.pResult = hb_itemNew( NULL );
   hb_gtInfo( HB_GTI_WINTITLE, &gtInfo );
   hb_strncpy( szTitle, hb_itemGetCPtr( gtInfo.pResult ), nSize - 1 );
   hb_itemRelease( gtInfo.pResult );
}

/* .T. when window win, or one of its direct children, is titled szTitle
   (a window manager puts the program's window into a frame of its own) */
static HB_BOOL ht_x11_titled( Display * dpy, Window win, const char * szTitle, int iDown )
{
   XTextProperty text;
   HB_BOOL fSame = HB_FALSE;

   /* XGetWMName, not XFetchName: gtxwc stores WM_NAME as UTF8_STRING, and
      XFetchName reads only STRING */
   if( XGetWMName( dpy, win, &text ) && text.value )
   {
      fSame = text.format == 8 && text.nitems == strlen( szTitle ) &&
              memcmp( text.value, szTitle, text.nitems ) == 0;
      XFree( text.value );
   }
   if( ! fSame && iDown > 0 )
   {
      Window root, parent, * aChildren = NULL;
      unsigned int nChildren = 0, i;

      if( XQueryTree( dpy, win, &root, &parent, &aChildren, &nChildren ) )
      {
         for( i = 0; i < nChildren && ! fSame; ++i )
            fSame = ht_x11_titled( dpy, aChildren[ i ], szTitle, iDown - 1 );
         if( aChildren )
            XFree( aChildren );
      }
   }
   return fSame;
}

/* .T. when our window (titled HB_GTI_WINTITLE) has the keyboard: the focus
   window, one of its parents or children - or, with the focus following the
   pointer (PointerRoot: no window manager), the window under the pointer.
   Checked at most every 100 ms. */
static HB_BOOL ht_x11_focused( Display * dpy )
{
   static HB_MAXUINT s_nLast = 0;
   static HB_BOOL    s_fFocused = HB_FALSE;
   HB_MAXUINT nNow = hb_dateMilliSeconds();

   if( nNow - s_nLast >= 100 )
   {
      char   szTitle[ 256 ];
      Window win, root, parent, child, * aChildren;
      unsigned int nChildren, uMask;
      int    iRevert, nDepth = 0, iX, iY, iWX, iWY;

      s_nLast = nNow;
      s_fFocused = HB_FALSE;
      ht_x11_title( szTitle, sizeof( szTitle ) );
      if( szTitle[ 0 ] == '\0' )
         return HB_FALSE;

      XGetInputFocus( dpy, &win, &iRevert );
      if( win == PointerRoot )
      {
         win = None;
         if( XQueryPointer( dpy, DefaultRootWindow( dpy ), &root, &child, &iX, &iY, &iWX, &iWY, &uMask ) )
            win = child;
      }
      while( win != None && win != PointerRoot && nDepth++ < 8 )
      {
         if( ht_x11_titled( dpy, win, szTitle, 2 ) )
         {
            s_fFocused = HB_TRUE;
            break;
         }
         if( ! XQueryTree( dpy, win, &root, &parent, &aChildren, &nChildren ) )
            break;
         if( aChildren )
            XFree( aChildren );
         if( parent == root || parent == None )
            break;
         win = parent;
      }
   }
   return s_fFocused;
}

static KeySym ht_x11_keysym( int iVK )
{
   switch( iVK )
   {
      case 0x25: return XK_Left;
      case 0x26: return XK_Up;
      case 0x27: return XK_Right;
      case 0x28: return XK_Down;
      case 0x20: return XK_space;
      case 0x0D: return XK_Return;
      case 0x1B: return XK_Escape;
      case 0x10: return XK_Shift_L;
      case 0x11: return XK_Control_L;
   }
   if( iVK >= 'A' && iVK <= 'Z' )
      return XK_a + ( iVK - 'A' );
   if( iVK >= '0' && iVK <= '9' )
      return XK_0 + ( iVK - '0' );
   return NoSymbol;
}

HB_FUNC( HT_KEYDOWN )
{
   Display * dpy = ht_x11_display();
   HB_BOOL fDown = HB_FALSE;

   if( dpy && HB_ISNUM( 1 ) && ht_x11_focused( dpy ) )
   {
      KeySym sym = ht_x11_keysym( hb_parni( 1 ) );
      KeyCode code = sym != NoSymbol ? XKeysymToKeycode( dpy, sym ) : 0;

      if( code )
      {
         char aKeys[ 32 ];

         XQueryKeymap( dpy, aKeys );
         fDown = ( aKeys[ code >> 3 ] & ( 1 << ( code & 7 ) ) ) != 0;
      }
   }
   hb_retl( fDown );
}

HB_FUNC( HT_KEYSTATEAVAILABLE )
{
   hb_retl( ht_x11_display() != NULL );
}

#else /* no X11 headers */

HB_FUNC( HT_FONTLOAD )
{
   hb_retl( HB_FALSE );
}

HB_FUNC( HT_KEYDOWN )
{
   hb_retl( HB_FALSE );
}

HB_FUNC( HT_KEYSTATEAVAILABLE )
{
   hb_retl( HB_FALSE );
}

#endif /* HT_HAVE_X11 */

#endif /* ! HB_OS_WIN */
