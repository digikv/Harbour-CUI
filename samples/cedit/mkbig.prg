/* Fill a file with real text lines up to nTarget bytes (default 2 GiB).
 * Usage: MkBig.exe [bytes] [outfile]
 *   MkBig.exe
 *   MkBig.exe 2147483648 test_2gb_lines.txt
 */
#include "fileio.ch"

PROCEDURE Main( cBytes, cName )
   LOCAL h, n, cLine, nTarget, nWrote := 0, nPct := -1, nNow

   nTarget := IIF( Empty( cBytes ), 2147483648, Val( cBytes ) )
   IF nTarget < 1
      nTarget := 2147483648
   ENDIF
   IF Empty( cName )
      cName := "test_2gb_lines.txt"
   ENDIF

   h := FCreate( cName )
   IF h < 0
      OutErr( "Cannot create " + cName + hb_eol() )
      ErrorLevel( 1 )
      RETURN
   ENDIF

   OutErr( "Writing " + cName + " target=" + hb_ntos( nTarget ) + hb_eol() )
   n := 1
   DO WHILE nWrote < nTarget
      cLine := "Line " + StrZero( n, 9 ) + " ABCDEFGHIJKLMNOPQRSTUVWXYZ 0123456789" + hb_eol()
      IF nWrote + hb_BLen( cLine ) > nTarget
         cLine := hb_BLeft( cLine, nTarget - nWrote )
      ENDIF
      nWrote += FWrite( h, cLine )
      n++
      nNow := Int( 100 * nWrote / nTarget )
      IF nNow != nPct .AND. ( nNow % 5 == 0 .OR. nWrote >= nTarget )
         nPct := nNow
         OutErr( hb_ntos( nPct ) + "%  " + hb_ntos( nWrote ) + " bytes  ~" + hb_ntos( n - 1 ) + " lines" + hb_eol() )
      ENDIF
   ENDDO
   FClose( h )
   OutErr( "OK " + cName + " size=" + hb_ntos( hb_FSize( cName ) ) + ;
           " lines~" + hb_ntos( n - 1 ) + hb_eol() )
RETURN
