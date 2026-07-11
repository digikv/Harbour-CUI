/*
 *
 */
#include "common.ch"
#include "error.ch"
#include "fileio.ch"

#include "hbtui.ch"
REQUEST Select,Alias,RecNo,DbFilter,DbRelation,IndexOrd,IndexKey

PROCEDURE Main()

    LOCAL app
    LOCAL win
    LOCAL menu
    altd()
    setMode( 25,80 )
    ErrorSys()
	
    app := HTApplication():new()

    win := HTMainWindow():new()

    menu := win:menuBar():addMenu("File")

    menu:addAction("New")
    menu:addAction("Open")
    menu:addAction("Print")
    menu:addSeparator()
    menu:addAction("Close")
    menu:addAction("Quit")

    menu := win:menuBar():addMenu("Edit")

    menu:addAction("Insert")
    menu:addAction("Delete")
    menu:addAction("Change")
    menu:addSeparator()
    menu:addAction("Authorize")

    win:show()

    app:exec()

RETURN

PROCEDURE ErrorSys()
     Errorblock( { | oError | DefError( oError ) } )
Return

STATIC FUNCTION DefError( oError )
   LOCAL cMessage
   LOCAL cDOSError
   LOCAL aOptions
   LOCAL nChoice
   LOCAL cMsg

   IF oError:genCode == EG_ZERODIV
      RETURN 0
   ENDIF

   IF oError:genCode == EG_LOCK .AND. ;
      oError:canRetry
      RETURN .T.
   ENDIF

   IF oError:genCode == EG_OPEN .AND. ;
      oError:osCode == 32 .AND. ;
      oError:canDefault
      NetErr( .T. )
      RETURN .F.
   ENDIF

   IF oError:genCode == EG_APPENDLOCK .AND. ;
      oError:canDefault
      NetErr( .T. )
      RETURN .F.
   ENDIF

   DO WHILE DispCount() > 0
      DispEnd()
   ENDDO

   cMessage := ErrorMessage( oError )
   If !Empty( oError:osCode )
      cDOSError := "(DOS Error " + Ltrim( Str( oError:osCode ) ) + ")"
   Endif


   If ValType( oError:Args ) == "A"
     cMessage += " Arguments: (" + Arguments( oError ) + ")"
   Endif

   IF MaxCol() > 0
       aOptions := {}

       Aadd( aOptions, "Izadji" )

       If oError:canRetry
          Aadd( aOptions, "Ponovi" )
       Endif

       If oError:canDefault
          Aadd( aOptions, "Uobicajeno" )
       Endif

       nChoice := 0
       While nChoice == 0

          If Empty( oError:osCode )
         nChoice := Alert( cMessage, aOptions, "ERROR" )
          Else
         nChoice := Alert( cMessage + ";" + cDOSError, aOptions, "ERROR" )
          Endif

       Enddo

       IF ! Empty( nChoice )
          DO CASE
          CASE aOptions[ nChoice ] == "Break"
             Break( oError )
          CASE aOptions[ nChoice ] == "Retry"
             RETURN .T.
          CASE aOptions[ nChoice ] == "Default"
             RETURN .F.
          ENDCASE
       ENDIF
   ELSE
      IF Empty( oError:osCode )
	     cMsg := cMessage + ";" + oError:ProcName + "(" + LTrim( Str( oError:ProcLine() ) ) +  ") in module: " + oError:ModuleName
         Alert( cMsg, aOptions, "ERROR" )
      ELSE
		 cMsg := cMessage + ";" + cDOSError + ";" + oError:ProcName + "(" + LTrim( Str( oError:ProcLine() ) ) +  ") in module: " + oError:ModuleName
         Alert( cMsg, aOptions, "ERROR" )
	  ENDIF
   ENDIF

   // "Quit" selected
   IF ! Empty( oError:osCode )
      cMessage += " " + cDOSError
   ENDIF

   LogError( oError )
   ErrorLevel( 1 )
   QUIT

RETURN .F.

STATIC FUNCTION ErrorMessage( oError )
     LOCAL cMessage
     cMessage := Iif( oError:severity > ES_WARNING, "Error", "Warning" ) + " "
     If Ischaracter( oError:subsystem )
        cMessage += oError:subsystem()
     Else
        cMessage += "???"
     Endif
     If Isnumber( oError:subCode )
        cMessage += "/" + Ltrim( Str( oError:subCode ) )
     Else
        cMessage += "/???"
     Endif
     If Ischaracter( oError:description )
        cMessage += "  " + oError:description
     Endif
     Do Case
         Case !Empty( oError:filename )
             cMessage += ": " + oError:filename
         Case !Empty( oError:operation )
             cMessage += ": " + oError:operation
     Endcase
Return cMessage

STATIC FUNCTION LogError( oerr )

     LOCAL cScreen
     LOCAL aLogFile    := { "error.log", .F. } // SET( _SET_ERRORLOG )
     LOCAL cLogFile    := aLogFile[1]  // error log file name
     LOCAL lAppendLog  := aLogFile[2]  // .f. = create a new error log (default) .t. = append to a existing one.
     LOCAL nStart      := 1
     LOCAL nCellSize
     LOCAL nRange
     LOCAL nCols
     LOCAL nRows
     LOCAL nCount
     LOCAL nForLoop
     LOCAL cOutString
     LOCAL cSubString
     LOCAL nHandle
     LOCAL nBytes

     LOCAL nHandle2   := -1
     LOCAL cLogFile2  := "_error.log"
     LOCAL cBuff      := ""
     LOCAL nRead      

     nCols := MaxCol()
     IF nCols > 0
        nRows := MaxRow()
        cScreen := Savescreen()
     ENDIF
     //Alert( 'An error occured, Information will be ;written to error.log' )

     If !lAppendLog
        nHandle := FCreate( cLogFile, FC_NORMAL )
     Else
        If !File( cLogFile )
           nHandle := FCreate( cLogFile, FC_NORMAL )
        Else
           nHandle  := FCreate( cLogFile2, FC_NORMAL )
           nHandle2 := FOpen( cLogFile, FO_READ )
        Endif
     Endif


     If nHandle < 3 .and. lower( cLogFile ) != 'error.log'
        // Force creating error.log in case supplied log file cannot
        // be created for any reason
        cLogFile := 'error.log'
        nHandle := Fcreate( cLogFile, FC_NORMAL )
     Endif

     If nHandle < 3
     Else

        FWriteLine( nHandle, Padc( ' Harbour Error Log ' , 79, '-' ) )
        FWriteLine( nHandle, '' )

        FWriteLine( nHandle, 'Date...............: ' + dtoc( date() )  )
        FWriteLine( nHandle, 'Time...............: ' + time()          )

        FWriteLine( nHandle, '' )
        FWriteLine( nHandle, 'Workstation name...: ' + netname() )
        FWriteLine( nHandle, 'Available memory...: ' + strvalue( Memory(0) )  )
        FWriteLine( nHandle, 'Current disk.......: ' + diskname() )
        FWriteLine( nHandle, 'Current directory..: ' + curdir() )
        FWriteLine( nHandle, 'Free disk space....: ' + strvalue( DiskSpace() ) )
        FWriteLine( nHandle, '' )
        FWriteLine( nHandle, 'Operating system...: ' + os() )
        FWriteLine( nHandle, 'Harbour version...:  ' + version() )
        FWriteLine( nHandle, 'C/C++ compiler.....: ' + hb_compiler() )


        IF Type( "Select()" ) != "UI"
        FWriteLine( nHandle, '' )
        FWriteLine( nHandle, 'Current Area ......:' + strvalue( &("Select()") ) )
        ENDIF

        FWriteLine( nHandle, '' )
        FWriteLine( nHandle, Padc( ' Environmental Information ', 79, '-' ) )
        FWriteLine( nHandle, '' )

        FWriteLine( nHandle, "SET ALTERNATE......: " + strvalue( Set( 18  ), .T. ) )
        FWriteLine( nHandle, "SET ALTFILE........: " + strvalue( Set( 19  )      ) )
        FWriteLine( nHandle, "SET AUTOPEN........: " + strvalue( Set( 45  ), .T. ) )
        FWriteLine( nHandle, "SET AUTORDER.......: " + strvalue( Set( 46  )      ) )
        FWriteLine( nHandle, "SET AUTOSHARE......: " + strvalue( Set( 47  )      ) )

        FWriteLine( nHandle, "SET BACKGROUNDTASKS: " + strvalue( Set( 111 ), .T. ) )
        FWriteLine( nHandle, "SET BACKGROUNDTICK.: " + strvalue( Set( 114 ), .T. ) )
        FWriteLine( nHandle, "SET BELL...........: " + strvalue( Set( 26  ), .T. ) )
        FWriteLine( nHandle, "SET BLINK..........: " + strvalue( SetBlink()      ) )

        FWriteLine( nHandle, "SET CANCEL.........: " + strvalue( Set( 12  ), .T. ) )
        FWriteLine( nHandle, "SET CENTURY........: " + strvalue( __SetCentury(), .T. ) )
        FWriteLine( nHandle, "SET COLOR..........: " + strvalue( Set( 15  )      ) )
        FWriteLine( nHandle, "SET CONFIRM........: " + strvalue( Set( 27  ), .T. ) )
        FWriteLine( nHandle, "SET CONSOLE........: " + strvalue( Set( 17  ), .T. ) )
        FWriteLine( nHandle, "SET COUNT..........: " + strvalue( Set( 47  )      ) )
        FWriteLine( nHandle, "SET CURSOR.........: " + strvalue( Set( 16  )      ) )

        FWriteLine( nHandle, "SET DATE FORMAT....: " + strvalue( Set( 4   )      ) )
        FWriteLine( nHandle, "SET DBFLOCKSCHEME..: " + strvalue( Set( 110 )      ) )
        FWriteLine( nHandle, "SET DEBUG..........: " + strvalue( Set( 13  ), .T. ) )
        FWriteLine( nHandle, "SET DECIMALS.......: " + strvalue( Set( 3   )      ) )
        FWriteLine( nHandle, "SET DEFAULT........: " + strvalue( Set( 7   )      ) )
        FWriteLine( nHandle, "SET DELETED........: " + strvalue( Set( 11  ), .T. ) )
        FWriteLine( nHandle, "SET DELIMCHARS.....: " + strvalue( Set( 34  )      ) )
        FWriteLine( nHandle, "SET DELIMETERS.....: " + strvalue( Set( 33  ), .T. ) )
        FWriteLine( nHandle, "SET DEVICE.........: " + strvalue( Set( 20  )      ) )
        FWriteLine( nHandle, "SET DIRCASE........: " + strvalue( Set( 106 )      ) )
        FWriteLine( nHandle, "SET DIRSEPARATOR...: " + strvalue( Set( 107 )      ) )

//        FWriteLine( nHandle, "SET EOL............: " + strvalue( Asc( Set( 118 ) ) )  )
        FWriteLine( nHandle, "SET EPOCH..........: " + strvalue( Set( 5   )      ) )
        FWriteLine( nHandle, "SET ERRORLOG.......: " + aLogFile[1] )
        FWriteLine( nHandle, "SET ERRORLOOP......: " + strvalue( Set( 108 )      ) )
        FWriteLine( nHandle, "SET ESCAPE.........: " + strvalue( Set( 28  ), .T. ) )
        FWriteLine( nHandle, "SET EVENTMASK......: " + strvalue( Set( 39  )      ) )
        FWriteLine( nHandle, "SET EXACT..........: " + strvalue( Set( 1   ), .T. ) )
        FWriteLine( nHandle, "SET EXCLUSIVE......: " + strvalue( Set( 8   ), .T. ) )
        FWriteLine( nHandle, "SET EXIT...........: " + strvalue( Set( 30  ), .T. ) )
        FWriteLine( nHandle, "SET EXTRA..........: " + strvalue( Set( 21  ), .T. ) )
        FWriteLine( nHandle, "SET EXTRAFILE......: " + strvalue( Set( 22  )      ) )

        FWriteLine( nHandle, "SET FILECASE.......: " + strvalue( Set( 105 )      ) )
        FWriteLine( nHandle, "SET FIXED..........: " + strvalue( Set( 2   ), .T. ) )
        FWriteLine( nHandle, "SET FORCEOPT.......: " + strvalue( Set( 117 ), .T. ) )

        FWriteLine( nHandle, "SET GTMODE.........: " + strvalue( Set( 113 )      ) )

        FWriteLine( nHandle, "SET HARDCOMMIT.....: " + strvalue( Set( 116 ), .T. ) )

        FWriteLine( nHandle, "SET IDLEREPEAT.....: " + strvalue( Set( 101 ), .T. ) )
        FWriteLine( nHandle, "SET INSERT.........: " + strvalue( Set( 29  ), .T. ) )
        FWriteLine( nHandle, "SET INTENSITY......: " + strvalue( Set( 31  ), .T. ) )

        FWriteLine( nHandle, "SET LANGUAGE.......: " + strvalue( Set( 100 )      ) )

        FWriteLine( nHandle, "SET MARGIN.........: " + strvalue( Set( 25  )      ) )
        FWriteLine( nHandle, "SET MBLOCKSIZE.....: " + strvalue( Set( 41  )      ) )
        FWriteLine( nHandle, "SET MCENTER........: " + strvalue( Set( 37  ), .T. ) )
        FWriteLine( nHandle, "SET MESSAGE........: " + strvalue( Set( 36  )      ) )
        FWriteLine( nHandle, "SET MFILEEXT.......: " + strvalue( Set( 42  )      ) )

        FWriteLine( nHandle, "SET OPTIMIZE.......: " + strvalue( Set( 44  ), .T. ) )
        FWriteLine( nHandle, "SET OUTPUTSAFETY...: " + strvalue( Set( 109 ), .T. ) )

        FWriteLine( nHandle, "SET PATH...........: " + strvalue( Set( 6   )      ) )
        FWriteLine( nHandle, "SET PRINTER........: " + strvalue( Set( 23  ), .T. ) )
        FWriteLine( nHandle, "SET PRINTERJOB.....: " + strvalue( Set( 115 )      ) )
        FWriteLine( nHandle, "SET PRINTFILE......: " + strvalue( Set( 24  )      ) )

        FWriteLine( nHandle, "SET SCOREBOARD.....: " + strvalue( Set( 32  ), .T. ) )
        FWriteLine( nHandle, "SET SCROLLBREAK....: " + strvalue( Set( 38  ), .T. ) )
        FWriteLine( nHandle, "SET SOFTSEEK.......: " + strvalue( Set( 9   ), .T. ) )
        FWriteLine( nHandle, "SET STRICTREAD.....: " + strvalue( Set( 43  ), .T. ) )

        FWriteLine( nHandle, "SET TRACE..........: " + strvalue( Set( 102 ), .T. ) )
        FWriteLine( nHandle, "SET TRACEFILE......: " + strvalue( Set( 103 )      ) )
        FWriteLine( nHandle, "SET TRACESTACK.....: " + strvalue( Set( 104 )      ) )
        FWriteLine( nHandle, "SET TRIMFILENAME...: " + strvalue( Set( 112 )      ) )

        FWriteLine( nHandle, "SET TYPEAHEAD......: " + strvalue( Set( 14  )      ) )

        FWriteLine( nHandle, "SET UNIQUE.........: " + strvalue( Set( 10  ), .T. ) )

        FWriteLine( nHandle, "SET VIDEOMODE......: " + strvalue( Set( 40  )      ) )

        FWriteLine( nHandle, "SET WRAP...........: " + strvalue( Set( 35  ), .T. ) )

        FWriteLine( nHandle, "" )
        IF nCols > 0
            FWriteLine( nHandle, Padc( " Internal Error Handling Information  ", nCols, "-" ) )
        ELSE
            FWriteLine( nHandle, " Internal Error Handling Information  " )
        ENDIF
        FWriteLine( nHandle, "" )
        FWriteLine( nHandle, "Subsystem Call ....: " + oErr:subsystem() )
        FWriteLine( nHandle, "System Code .......: " + strvalue( oErr:suBcode() ) )
        FWriteLine( nHandle, "Default Status ....: " + strvalue( oerr:candefault() ) )
        FWriteLine( nHandle, "Description .......: " + oErr:description() )
        FWriteLine( nHandle, "Operation .........: " + oErr:operation() )
        FWriteLine( nHandle, "Arguments .........: " + Arguments( oErr ) )
        FWriteLine( nHandle, "Involved File .....: " + oErr:filename() )
        FWriteLine( nHandle, "Dos Error Code ....: " + strvalue( oErr:oscode() ) )
        FWriteLine( nHandle, "" )
        FWriteLine( nHandle, " Trace Through:" )
        FWriteLine( nHandle, "----------------" )

        nCount := 3
        While !Empty( Procname(  nCount  ) )
          FWriteLine( nHandle, Padr( Procname( nCount ), 21 ) + ' : ' + Transform( Procline( nCount ), "999,999" ) + " in Module: " + ProcFile( nCount++ ) )
        Enddo


        FWriteLine( nHandle, "" )

        IF nCols > 0
            FWriteLine( nHandle, Padc( 'Detailed Work Area Items', nCols, '-' ) )
        ELSE
            FWriteLine( nHandle, 'Detailed Work Area Items ' )
        ENDIF
        FWriteLine( nHandle, "" )

        IF Type( "Select()" ) != "UI"
           For nCount := 1 To 600
              If !Empty( ( nCount )->( &("Alias()") ) )
                 ( nCount )->( FWriteLine( nHandle, "Work Area No ......: " + strvalue( &("Select()") ) ) )
                 ( nCount )->( FWriteLine( nHandle, "Alias .............: " + &("Alias()") ) )
                 ( nCount )->( FWriteLine( nHandle, "Current Recno .....: " + strvalue( &("RecNo()") ) ) )
                 ( nCount )->( FWriteLine( nHandle, "Current Filter ....: " + &("DbFilter()") ) )
                 ( nCount )->( FWriteLine( nHandle, "Relation Exp. .....: " + &("DbRelation()") ) )
                 ( nCount )->( FWriteLine( nHandle, "Index Order .......: " + strvalue( &("IndexOrd(0)") ) ) )
                 ( nCount )->( FWriteLine( nHandle, "Active Key ........: " + strvalue( &("IndexKey(0)") ) ) )
                 ( nCount )->( FWriteLine( nHandle, "" ) )
              Endif
           Next
        ENDIF

        FWriteLine( nHandle, "" )
        FWriteLine( nHandle, "" )

        IF valtype( cScreen ) == "C"
            FWriteLine( nHandle, Padc( " Video Screen Dump ", nCols, "#" ) )
            FWriteLine( nHandle, "" )
            //FWriteLine( nHandle, "" )
            FWriteLine( nHandle, "+" + Replicate( '-', nCols + 1 ) + "+" )
            //FWriteLine( nHandle, "" )
            nCellSize := len( Savescreen( 0, 0, 0, 0 ) )
            nRange := ( nCols + 1 ) * nCellSize
            For nCount := 1 To nRows + 1
               cOutString := ''
               cSubString := Substr( cScreen, nStart, nRange )
               For nForLoop := 1 To nRange step nCellSize
                  cOutString += Substr( cSubString, nForLoop, 1 )
               Next
               FWriteLine( nHandle, "|" + cOutString + "|" )
               nStart += nRange
            Next
            FWriteLine( nHandle, "+" + Replicate( '-', nCols + 1 ) + "+" )
            FWriteLine( nHandle, "" )
            FWriteLine( nHandle, "" )
        ELSE
            FWriteLine( nHandle, " Video Screen Dump not available" )
        ENDIF


        if lAppendLog .and. nHandle2 != -1

           nBytes := FSeek( nHandle2, 0, FS_END )

           cBuff := space(10)
           FSeek( nHandle2, 0, FS_SET )

           while nBytes > 0
             nRead := FRead( nHandle2, @cBuff, 10 )
             FWrite( nHandle, cBuff, nRead )
             nBytes -= nRead
             cBuff := space( 10 )
           enddo

           FClose( nHandle2 )
           FClose( nHandle )

           FErase( cLogFile )
           FRename( cLogFile2, cLogFile )
        else
           FClose( nHandle )
        endif

     Endif

Return .f.

STATIC FUNCTION strvalue( c, l )

     LOCAL cr := ''
     Default l To .f.
     Switch ValType( c )
         Case "C"
             cr := c
             exit
         Case "N"
             cr := Alltrim( Str( c ) )
             exit
         Case "M"
             cr := c
             exit
         Case "D"
             cr := Dtoc( c )
             exit
         Case "L"
             cr := If( l, If( c, "On", "Off" ), If( c, ".t.", ".f." ) )
             exit
     End
Return Upper( cr )

STATIC FUNCTION FWriteLine( nh, c )

   Fwrite( nh, c + HB_EOL() )
   //HB_OutDebug( c + HB_EOL() )
Return nil

STATIC FUNCTION Arguments( oErr )

   LOCAL xArg, cArguments := ""

   IF ValType( oErr:Args ) == "A"
      FOR EACH xArg IN oErr:Args
         cArguments += " [" + Str( xArg:__EnumIndex(), 2 ) + "] = Type: " + ValType( xArg )

         IF xArg != NIL
            cArguments +=  " Val: " + CStr( xArg )
         ENDIF
      NEXT
   ENDIF

RETURN cArguments

#ifdef __PLATFORM__Windows
#pragma BEGINDUMP

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hbvmpub.h"
#include "hbfast.h"
#include "hbstack.h"
#include "thread.h"

#include <windows.h>

static PHB_SYMB s_xHbFunc = NULL;

LONG WINAPI PRGUnhandledExceptionFilter( EXCEPTION_POINTERS *ExceptionInfo )
{
   if( s_xHbFunc )
   {
      HB_ITEM Exception;
      PHB_DYNS pDyn = hb_dynsymFind( "HB_CSTRUCTURE" );

      Exception.type = HB_IT_NIL;

      //TraceLog( NULL, "%s(%p)\n", pExecSym->pSymbol->szName, ExceptionInfo );

      if( pDyn )
      {
         hb_vmPushSymbol( pDyn->pSymbol );
         hb_vmPushNil();
         hb_itemPushStaticString( "EXCEPTION_POINTERS", 18 );
         hb_vmPushLong( 8 );
         hb_vmDo( 2 );

         if( hb_stackReturnItem()->type == HB_IT_OBJECT )
         {
            HB_ITEM_NEW( Buffer );
            HB_ITEM Adopt;

            hb_itemForwardValue( &Exception, hb_stackReturnItem() );

            hb_itemPutCLStatic( &Buffer, (char *) ExceptionInfo, sizeof( EXCEPTION_POINTERS ) );

            Adopt.type = HB_IT_LOGICAL;
            Adopt.item.asLogical.value = FALSE;

            hb_objSendMsg( &Exception, "Buffer", 2, &Buffer, &Adopt );
         }
      }

      hb_vmPushSymbol( s_xHbFunc );
      hb_vmPushNil();
      hb_itemPushForward( &Exception );
      hb_vmDo( 1 );

      //TraceLog( NULL, "Done\n" );
   }

   return hb_itemGetNL( hb_stackReturnItem() );
}

HB_FUNC( SETERRORMODE )
{
   hb_retni( SetErrorMode( hb_parni( 1 ) ) ) ;
}

HB_FUNC( SETUNHANDLEDEXCEPTIONFILTER )
{
   LPTOP_LEVEL_EXCEPTION_FILTER pDefaultHandler;

   s_xHbFunc = (PHB_SYMB) hb_parptr( 1 );

   pDefaultHandler = SetUnhandledExceptionFilter( PRGUnhandledExceptionFilter );
   //TraceLog( NULL, "Default: %p\n", pDefaultHandler );

   hb_retnl( (LONG) pDefaultHandler );
}

#pragma ENDDUMP

#endif