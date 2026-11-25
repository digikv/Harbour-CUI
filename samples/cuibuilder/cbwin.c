/*
 * CuiBuilder - compilers without a console window (Windows)
 *
 * Copyright (c) 2026 Srđan Dragojlović - dual licensed, see LICENSE
 *
 * CuiBuilder is a window program (gtwvt) without a console, so
 * hb_processRun() opened a black console window for every compiler run.
 * CB_RUNHIDDEN() starts the command with CREATE_NO_WINDOW: it has a
 * console, but no window. What it writes comes back through a pipe.
 * (The same as CNC_RUNHIDDEN of samples/cnc/cncwin.c.)
 */

#include "hbapi.h"

#if defined( HB_OS_WIN )

#include <windows.h>
#include <shellapi.h>

/* CB_RUNHIDDEN( cCommandLine, @cOutput ) -> nExitCode, -1 = not started.
   The command line is UTF-8; stdout and stderr come together, as written. */
HB_FUNC( CB_RUNHIDDEN )
{
   const char * szCmd = hb_parc( 1 );
   long lExit = -1;
   char * pBuf = NULL;
   HB_SIZE nLen = 0, nCap = 0;
   HANDLE hRead = NULL, hWrite = NULL;
   SECURITY_ATTRIBUTES sa;

   sa.nLength = sizeof( sa );
   sa.bInheritHandle = TRUE;
   sa.lpSecurityDescriptor = NULL;

   if( szCmd && CreatePipe( &hRead, &hWrite, &sa, 0 ) )
   {
      int nW = MultiByteToWideChar( CP_UTF8, 0, szCmd, -1, NULL, 0 );
      LPWSTR wCmd = ( LPWSTR ) hb_xgrab( ( nW + 1 ) * sizeof( WCHAR ) );
      STARTUPINFOW si;
      PROCESS_INFORMATION pi;

      MultiByteToWideChar( CP_UTF8, 0, szCmd, -1, wCmd, nW );
      SetHandleInformation( hRead, HANDLE_FLAG_INHERIT, 0 );
      memset( &si, 0, sizeof( si ) );
      memset( &pi, 0, sizeof( pi ) );
      si.cb = sizeof( si );
      si.dwFlags = STARTF_USESTDHANDLES;
      si.hStdInput = NULL;
      si.hStdOutput = hWrite;
      si.hStdError = hWrite;

      if( CreateProcessW( NULL, wCmd, NULL, NULL, TRUE, CREATE_NO_WINDOW, NULL, NULL, &si, &pi ) )
      {
         DWORD dwExit = 0;

         CloseHandle( hWrite );          /* the pipe ends when the command ends */
         hWrite = NULL;
         for( ;; )
         {
            char buf[ 4096 ];
            DWORD dwRead = 0;

            if( ! ReadFile( hRead, buf, sizeof( buf ), &dwRead, NULL ) || dwRead == 0 )
               break;
            if( nLen + dwRead > nCap )
            {
               nCap = ( nLen + dwRead ) * 2;
               pBuf = ( char * ) ( pBuf ? hb_xrealloc( pBuf, nCap ) : hb_xgrab( nCap ) );
            }
            memcpy( pBuf + nLen, buf, dwRead );
            nLen += dwRead;
         }
         WaitForSingleObject( pi.hProcess, INFINITE );
         if( GetExitCodeProcess( pi.hProcess, &dwExit ) )
            lExit = ( long ) dwExit;
         CloseHandle( pi.hProcess );
         CloseHandle( pi.hThread );
      }
      hb_xfree( wCmd );
   }
   if( hWrite )
      CloseHandle( hWrite );
   if( hRead )
      CloseHandle( hRead );

   hb_storclen( pBuf ? pBuf : "", nLen, 2 );
   if( pBuf )
      hb_xfree( pBuf );
   hb_retnl( lExit );
}

/* CB_PROCSTART( cCommandLine, cOutFile ) -> nHandle, 0 = not started.
   Starts a compiler without a window and without waiting for it (parallel
   builds); stdout and stderr go to cOutFile. CB_PROCPOLL() tells when it ended. */
HB_FUNC( CB_PROCSTART )
{
   const char * szCmd = hb_parc( 1 );
   const char * szOut = hb_parc( 2 );
   HB_PTRUINT nHandle = 0;

   if( szCmd && szOut )
   {
      int nW = MultiByteToWideChar( CP_UTF8, 0, szCmd, -1, NULL, 0 );
      int nO = MultiByteToWideChar( CP_UTF8, 0, szOut, -1, NULL, 0 );
      LPWSTR wCmd = ( LPWSTR ) hb_xgrab( ( nW + 1 ) * sizeof( WCHAR ) );
      LPWSTR wOut = ( LPWSTR ) hb_xgrab( ( nO + 1 ) * sizeof( WCHAR ) );
      SECURITY_ATTRIBUTES sa;
      HANDLE hFile;

      MultiByteToWideChar( CP_UTF8, 0, szCmd, -1, wCmd, nW );
      MultiByteToWideChar( CP_UTF8, 0, szOut, -1, wOut, nO );
      sa.nLength = sizeof( sa );
      sa.bInheritHandle = TRUE;
      sa.lpSecurityDescriptor = NULL;
      hFile = CreateFileW( wOut, GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE, &sa,
                           CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL );
      if( hFile != INVALID_HANDLE_VALUE )
      {
         STARTUPINFOW si;
         PROCESS_INFORMATION pi;

         memset( &si, 0, sizeof( si ) );
         memset( &pi, 0, sizeof( pi ) );
         si.cb = sizeof( si );
         si.dwFlags = STARTF_USESTDHANDLES;
         si.hStdInput = NULL;
         si.hStdOutput = hFile;
         si.hStdError = hFile;
         if( CreateProcessW( NULL, wCmd, NULL, NULL, TRUE, CREATE_NO_WINDOW, NULL, NULL, &si, &pi ) )
         {
            CloseHandle( pi.hThread );
            nHandle = ( HB_PTRUINT ) pi.hProcess;
         }
         CloseHandle( hFile );           /* the process keeps its own copy */
      }
      hb_xfree( wCmd );
      hb_xfree( wOut );
   }
   hb_retnint( ( HB_MAXINT ) nHandle );
}

/* CB_PROCPOLL( nHandle ) -> -2 while it runs, else its exit code (-1 = unknown);
   the handle is closed once it ended */
HB_FUNC( CB_PROCPOLL )
{
   HANDLE hProc = ( HANDLE ) ( HB_PTRUINT ) hb_parnint( 1 );
   long lExit = -1;

   if( hProc )
   {
      DWORD dwExit = 0;

      if( WaitForSingleObject( hProc, 0 ) == WAIT_TIMEOUT )
      {
         hb_retnl( -2 );
         return;
      }
      if( GetExitCodeProcess( hProc, &dwExit ) )
         lExit = ( long ) dwExit;
      CloseHandle( hProc );
   }
   hb_retnl( lExit );
}

/* CB_STARTPROGRAM( cCommandLine, cFolder ) -> .T. when started: runs the
   built program on its own (not waited for) in its folder */
HB_FUNC( CB_STARTPROGRAM )
{
   const char * szCmd = hb_parc( 1 );
   const char * szDir = hb_parc( 2 );
   HB_BOOL fOk = HB_FALSE;

   if( szCmd )
   {
      int nW = MultiByteToWideChar( CP_UTF8, 0, szCmd, -1, NULL, 0 );
      int nD = szDir ? MultiByteToWideChar( CP_UTF8, 0, szDir, -1, NULL, 0 ) : 0;
      LPWSTR wCmd = ( LPWSTR ) hb_xgrab( ( nW + 1 ) * sizeof( WCHAR ) );
      LPWSTR wDir = nD > 0 ? ( LPWSTR ) hb_xgrab( ( nD + 1 ) * sizeof( WCHAR ) ) : NULL;
      STARTUPINFOW si;
      PROCESS_INFORMATION pi;

      MultiByteToWideChar( CP_UTF8, 0, szCmd, -1, wCmd, nW );
      if( wDir )
         MultiByteToWideChar( CP_UTF8, 0, szDir, -1, wDir, nD );
      memset( &si, 0, sizeof( si ) );
      memset( &pi, 0, sizeof( pi ) );
      si.cb = sizeof( si );
      if( CreateProcessW( NULL, wCmd, NULL, NULL, FALSE, CREATE_NEW_CONSOLE, NULL, wDir, &si, &pi ) )
      {
         CloseHandle( pi.hProcess );
         CloseHandle( pi.hThread );
         fOk = HB_TRUE;
      }
      hb_xfree( wCmd );
      if( wDir )
         hb_xfree( wDir );
   }
   hb_retl( fOk );
}

/* CB_OPENDOC( cFile ) -> .T. when Windows opened the document in its program */
HB_FUNC( CB_OPENDOC )
{
   const char * szFile = hb_parc( 1 );
   HB_BOOL fOk = HB_FALSE;

   if( szFile )
   {
      int nW = MultiByteToWideChar( CP_UTF8, 0, szFile, -1, NULL, 0 );
      LPWSTR wFile = ( LPWSTR ) hb_xgrab( ( nW + 1 ) * sizeof( WCHAR ) );

      MultiByteToWideChar( CP_UTF8, 0, szFile, -1, wFile, nW );
      fOk = ( INT_PTR ) ShellExecuteW( NULL, L"open", wFile, NULL, NULL, SW_SHOWNORMAL ) > 32;
      hb_xfree( wFile );
   }
   hb_retl( fOk );
}

#endif
