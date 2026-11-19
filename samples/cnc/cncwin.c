/*
 * CNC - commands without a console window (Windows)
 *
 * Copyright (c) 2026 Srđan Dragojlović - dual licensed, see LICENSE
 *
 * CNC is a window program (gtwvt) without a console, so hb_processRun()
 * of "cmd /c ..." opened a black console window for every command (and
 * the console programs cmd starts got one too). CNC_RUNHIDDEN() starts the
 * command with CREATE_NO_WINDOW: it has a console, but no window, and the
 * programs it starts share it. What they write comes back through a pipe.
 */

#include "hbapi.h"

#if defined( HB_OS_WIN )

#include <windows.h>

/* CNC_RUNHIDDEN( cCommandLine, @cOutput ) -> nExitCode, -1 = not started.
   The command line is UTF-8; stdout and stderr come together, as written
   (a console program writes in the OEM codepage). */
HB_FUNC( CNC_RUNHIDDEN )
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

#endif
