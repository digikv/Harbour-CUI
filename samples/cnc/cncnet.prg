/*
 * CNC - the Net menu (as in Total Commander): FTP in a panel, network
 * drives and the computers of the network (Windows)
 *
 * Copyright (c) 2026 Srđan Dragojlović - dual licensed, see LICENSE
 *
 * CNCFtp wraps TIPClientFTP of Harbour's hbtip: a panel with an oNet reads
 * its folders through it; cnc.prg copies, deletes, renames and makes folders
 * through it. The connections are kept in cnc.ini [FTP] without passwords:
 *     name=host|port|user|folder
 */

#include "hbclass.ch"
#include "inkey.ch"
#include "hbtui.ch"
#include "cnc.ch"

CREATE CLASS CNCFtp

    VAR oFtp
    VAR cHost       INIT ""
    VAR nPort       INIT 21
    VAR cUser       INIT ""
    VAR cErr        INIT ""

    METHOD connect( cHost, nPort, cUser, cPass )
    METHOD dir( cPath )
    METHOD get( cRemote, cLocal )
    METHOD put( cLocal, cRemote )
    METHOD del( cRemote )
    METHOD rmdir( cRemote )
    METHOD mkdir( cRemote )
    METHOD rename( cFrom, cTo )
    METHOD close()
    METHOD url( cPath )
    METHOD fail( cWhat )

ENDCLASS

METHOD connect( cHost, nPort, cUser, cPass ) CLASS CNCFtp

    LOCAL oUrl

    ::cHost := cHost
    ::nPort := IIF( Empty( nPort ), 21, nPort )
    ::cUser := IIF( Empty( cUser ), "anonymous", cUser )
    oUrl := TUrl():New( "ftp://" + cHost + ":" + hb_ntos( ::nPort ) + "/" )
    oUrl:cUserid := ::cUser
    oUrl:cPassword := IIF( Empty( cPass ), "cnc@localhost", cPass )
    oUrl:cPath := ""
    ::oFtp := TIPClientFTP():New( oUrl )
    ::oFtp:nConnTimeout := 15000
    IF ! ::oFtp:Open()
        ::fail( "Cannot connect to " + cHost )
        ::oFtp := NIL
        RETURN .F.
    ENDIF

RETURN .T.

/* the entries of a remote folder in the form of a panel, NIL = error */
METHOD dir( cPath ) CLASS CNCFtp

    LOCAL aList := {}, a, cAttr

    IF ::oFtp == NIL .OR. ! ::oFtp:Cwd( cPath )
        ::fail( "Cannot open " + cPath )
        RETURN NIL
    ENDIF
    FOR EACH a IN ::oFtp:ListFiles()
        IF ! HB_ISARRAY( a ) .OR. ! HB_ISSTRING( a[ F_NAME ] )
            LOOP
        ENDIF
        cAttr := hb_defaultValue( a[ F_ATTR ], "" )
        IF Len( cAttr ) < 10 .OR. a[ F_NAME ] == "." .OR. a[ F_NAME ] == ".." .OR. Empty( a[ F_NAME ] )
            LOOP                                 /* "total 12" and the like */
        ENDIF
        IF Left( cAttr, 1 ) == "l" .AND. " -> " $ a[ F_NAME ]
            a[ F_NAME ] := Left( a[ F_NAME ], At( " -> ", a[ F_NAME ] ) - 1 )   /* a link: its name */
        ENDIF
        AAdd( aList, { a[ F_NAME ], hb_defaultValue( a[ F_SIZE ], 0 ), hb_defaultValue( a[ F_DATE ], CToD( "" ) ), ;
                       hb_defaultValue( a[ F_TIME ], "" ), cAttr, Left( cAttr, 1 ) == "d", .F. } )
    NEXT

RETURN aList

METHOD get( cRemote, cLocal ) CLASS CNCFtp
    IF ! ::oFtp:DownloadFile( cLocal, cRemote )
        RETURN ::fail( "Cannot download " + cRemote )
    ENDIF
RETURN .T.

METHOD put( cLocal, cRemote ) CLASS CNCFtp
    IF ! ::oFtp:UploadFile( cLocal, cRemote )
        RETURN ::fail( "Cannot upload " + cRemote )
    ENDIF
RETURN .T.

METHOD del( cRemote ) CLASS CNCFtp
    IF ! ::oFtp:Dele( cRemote )
        RETURN ::fail( "Cannot delete " + cRemote )
    ENDIF
RETURN .T.

METHOD rmdir( cRemote ) CLASS CNCFtp
    IF ! ::oFtp:RMD( cRemote )
        RETURN ::fail( "Cannot delete the folder " + cRemote )
    ENDIF
RETURN .T.

METHOD mkdir( cRemote ) CLASS CNCFtp
    IF ! ::oFtp:MKD( cRemote )
        RETURN ::fail( "Cannot make the folder " + cRemote )
    ENDIF
RETURN .T.

METHOD rename( cFrom, cTo ) CLASS CNCFtp
    IF ! ::oFtp:Rename( cFrom, cTo )
        RETURN ::fail( "Cannot rename " + cFrom )
    ENDIF
RETURN .T.

METHOD close() CLASS CNCFtp
    IF ::oFtp != NIL
        ::oFtp:Close()
        ::oFtp := NIL
    ENDIF
RETURN NIL

/* "ftp://user@host/folder/" for the title of the panel */
METHOD url( cPath ) CLASS CNCFtp
RETURN "ftp://" + IIF( ::cUser == "anonymous", "", ::cUser + "@" ) + ::cHost + ;
       IIF( ::nPort == 21, "", ":" + hb_ntos( ::nPort ) ) + hb_defaultValue( cPath, "/" )

/* the error with the server's last reply; returns .F. */
METHOD fail( cWhat ) CLASS CNCFtp

    LOCAL cReply := ""

    IF ::oFtp != NIL .AND. HB_ISSTRING( ::oFtp:cReply )
        cReply := AllTrim( StrTran( StrTran( ::oFtp:cReply, Chr( 13 ), "" ), Chr( 10 ), " " ) )
    ENDIF
    ::cErr := cWhat + IIF( Empty( cReply ), "", Chr( 10 ) + cReply )

RETURN .F.

/*----------------------------------------------------------- remote paths */

/* "pub" -> "/pub/" */
FUNCTION cnc_FtpDir( cDir )
    cDir := StrTran( AllTrim( cDir ), "\", "/" )
    IF !( Left( cDir, 1 ) == "/" )
        cDir := "/" + cDir
    ENDIF
    IF !( Right( cDir, 1 ) == "/" )
        cDir += "/"
    ENDIF
RETURN cDir

/* "/pub/files/" -> "/pub/" */
FUNCTION cnc_FtpParent( cDir )

    LOCAL n := RAt( "/", Left( cDir, Len( cDir ) - 1 ) )

RETURN IIF( n <= 1, "/", Left( cDir, n ) )

/*----------------------------------------------------------- dialogs */

/* the FTP connections of cnc.ini: { { cName, cHost, nPort, cUser, cDir }, ... } */
FUNCTION cnc_FtpList( hIni )

    LOCAL aList := {}, cKey, a

    FOR EACH cKey IN hb_HKeys( hIni )
        a := hb_ATokens( hIni[ cKey ], "|" )
        ASize( a, 4 )
        AAdd( aList, { cKey, hb_defaultValue( a[ 1 ], "" ), Val( hb_defaultValue( a[ 2 ], "21" ) ), ;
                       hb_defaultValue( a[ 3 ], "" ), hb_defaultValue( a[ 4 ], "/" ) } )
    NEXT

RETURN aList

/* a connection: host, port, user, password, folder, the name to save it
   under; returns a hash, NIL = cancelled */
FUNCTION cnc_FtpDialog( cTitle, aConn )

    LOCAL dlg, nW := Min( 64, ht_ScreenMaxCol() - 3 ), nH := 12, oHost, oPort, oUser, oPass, oDir, oName, h

    hb_default( @aConn, { "", "", 21, "anonymous", "/" } )
    dlg := HTDialog():new()
    dlg:setWindowTitle( cTitle )
    dlg:resize( nW, nH )
    dlg:move( Int( ( ht_ScreenMaxCol() + 1 - nW ) / 2 ), Int( ( ht_ScreenMaxRow() + 1 - nH ) / 2 ) )

    HTLabel():new( "Host:", dlg ):setGeometry( 1, 1, 14, 1 )
    oHost := CNCLine():new( dlg )
    oHost:setGeometry( 16, 1, nW - 30, 1 )
    oHost:setText( aConn[ 2 ] )
    HTLabel():new( "Port:", dlg ):setGeometry( nW - 13, 1, 5, 1 )
    oPort := CNCLine():new( dlg )
    oPort:setGeometry( nW - 8, 1, 5, 1 )
    oPort:setText( hb_ntos( aConn[ 3 ] ) )
    HTLabel():new( "User:", dlg ):setGeometry( 1, 2, 14, 1 )
    oUser := CNCLine():new( dlg )
    oUser:setGeometry( 16, 2, nW - 19, 1 )
    oUser:setText( aConn[ 4 ] )
    HTLabel():new( "Password:", dlg ):setGeometry( 1, 3, 14, 1 )
    oPass := CNCPassLine():new( dlg )
    oPass:setGeometry( 16, 3, nW - 19, 1 )
    HTLabel():new( "Folder:", dlg ):setGeometry( 1, 4, 14, 1 )
    oDir := CNCLine():new( dlg )
    oDir:setGeometry( 16, 4, nW - 19, 1 )
    oDir:setText( aConn[ 5 ] )
    HTLabel():new( "Save as:", dlg ):setGeometry( 1, 6, 14, 1 )
    oName := CNCLine():new( dlg )
    oName:setGeometry( 16, 6, nW - 19, 1 )
    oName:setText( aConn[ 1 ] )
    HTLabel():new( "(a name keeps the connection; the password is not saved)", dlg ):setGeometry( 1, 7, nW - 4, 1 )
    AEval( { oHost, oPort, oUser, oPass, oDir, oName }, {| o | o:bEnter := {|| dlg:accept() } } )
    dlg:addButtonBar( "Connect", "Cancel" )
    IF HB_ISSTRING( aConn[ 2 ] ) .AND. ! Empty( aConn[ 2 ] )
        dlg:setFocusChild( oPass )
    ENDIF

    IF dlg:exec() != HT_DIALOG_ACCEPTED .OR. Empty( oHost:text )
        RETURN NIL
    ENDIF
    h := { "name" => AllTrim( oName:text ), "host" => AllTrim( oHost:text ), "port" => Max( 1, Val( oPort:text ) ), ;
           "user" => AllTrim( oUser:text ), "pass" => oPass:text, "dir" => cnc_FtpDir( IIF( Empty( oDir:text ), "/", oDir:text ) ) }
    /* "ftp://user@host:port/dir" typed as the host */
    IF Lower( Left( h[ "host" ], 6 ) ) == "ftp://"
        cnc_FtpParseUrl( h )
    ENDIF

RETURN h

STATIC PROCEDURE cnc_FtpParseUrl( h )

    LOCAL c := SubStr( h[ "host" ], 7 ), n

    IF ( n := At( "/", c ) ) > 0
        h[ "dir" ] := cnc_FtpDir( SubStr( c, n ) )
        c := Left( c, n - 1 )
    ENDIF
    IF ( n := RAt( "@", c ) ) > 0
        h[ "user" ] := Left( c, n - 1 )
        c := SubStr( c, n + 1 )
        IF ( n := At( ":", h[ "user" ] ) ) > 0
            h[ "pass" ] := SubStr( h[ "user" ], n + 1 )
            h[ "user" ] := Left( h[ "user" ], n - 1 )
        ENDIF
    ENDIF
    IF ( n := At( ":", c ) ) > 0
        h[ "port" ] := Max( 1, Val( SubStr( c, n + 1 ) ) )
        c := Left( c, n - 1 )
    ENDIF
    h[ "host" ] := c

RETURN

/* a line of input that shows * for every character */
CLASS CNCPassLine FROM CNCLine
    METHOD paintEvent( event )
ENDCLASS

METHOD PROCEDURE paintEvent( event ) CLASS CNCPassLine

    LOCAL cText := ::text

    ::Ftext := Replicate( "*", Len( cText ) )
    ::super:paintEvent( event )
    ::Ftext := cText

RETURN

/*----------------------------------------------------------- Windows network */

/* runs a command, returns { nExitCode, cOutput } (the output in UTF-8) */
FUNCTION cnc_Capture( cCmd )

    LOCAL cOut := "", nRes
#ifdef __PLATFORM__WINDOWS

    nRes := cnc_RunHidden( "cmd /c " + cCmd, @cOut )   /* no console window (cncwin.c) */
#else
    LOCAL cErr := ""

    nRes := hb_processRun( cCmd, , @cOut, @cErr )
    cOut += IIF( Empty( cErr ), "", hb_eol() + cErr )
#endif

RETURN { nRes, IIF( hb_StrIsUTF8( cOut ), cOut, hb_StrToUTF8( cOut, "SL852" ) ) }

/* "net view": the computers of the network, then the shares of the one
   chosen; returns "\\pc\share\" or "" */
FUNCTION cnc_NetBrowse()

    LOCAL a, aPCs := {}, aShares := {}, c, n, lList := .F.

    a := cnc_Capture( "net view" )
    FOR EACH c IN hb_ATokens( StrTran( a[ 2 ], Chr( 13 ), "" ), Chr( 10 ) )
        IF Left( c, 2 ) == "\\"
            AAdd( aPCs, AllTrim( hb_ATokens( c, " " )[ 1 ] ) )
        ENDIF
    NEXT
    IF Empty( aPCs )
        c := cnc_Input( "Network", "No computers found. Computer (\\name):", "\\" )
        IF Empty( c ) .OR. c == "\\"
            RETURN ""
        ENDIF
        AAdd( aPCs, IIF( Left( c, 2 ) == "\\", c, "\\" + c ) )
        n := 1
    ELSE
        n := cnc_List( "Computers in the network", aPCs, 1 )
        IF n == 0
            RETURN ""
        ENDIF
    ENDIF
    a := cnc_Capture( "net view " + aPCs[ n ] )
    /* the shares: the lines between the line of dashes and an empty line */
    FOR EACH c IN hb_ATokens( StrTran( a[ 2 ], Chr( 13 ), "" ), Chr( 10 ) )
        IF Left( c, 4 ) == "----"
            lList := .T.
        ELSEIF lList .AND. Empty( c )
            EXIT
        ELSEIF lList
            c := RTrim( c )
            AAdd( aShares, AllTrim( IIF( "  " $ c, Left( c, At( "  ", c ) - 1 ), c ) ) )
        ENDIF
    NEXT
    IF Empty( aShares )
        HTMessageBox():error( "Network", "No shares found on " + aPCs[ n ] + Chr( 10 ) + AllTrim( Left( a[ 2 ], 200 ) ) )
        RETURN ""
    ENDIF
    c := aPCs[ n ]
    n := cnc_List( "Shares of " + c, aShares, 1 )

RETURN IIF( n == 0, "", c + "\" + aShares[ n ] + "\" )

/* "net use X: \\server\share": returns the drive ("X:\") or "" */
FUNCTION cnc_NetMap()

    LOCAL cDrive := "", c, cPath, cUser, a

    FOR EACH c IN "ZYXWVUTSRQPONMLKJIHGFE"
        IF ! hb_DirExists( c + ":\" )
            cDrive := c
            EXIT
        ENDIF
    NEXT
    cPath := cnc_Input( "Map network drive " + cDrive + ":", "Folder (\\server\share):", "\\" )
    IF Empty( cPath ) .OR. cPath == "\\"
        RETURN ""
    ENDIF
    cUser := cnc_Input( "Map network drive " + cDrive + ":", "User (empty: the current one):", "" )
    a := cnc_Capture( "net use " + cDrive + ": " + '"' + cPath + '"' + IIF( Empty( cUser ), "", " /user:" + cUser ) + " /persistent:no" )
    IF a[ 1 ] != 0
        HTMessageBox():error( "Network", "net use " + cDrive + ": " + cPath + Chr( 10 ) + AllTrim( Left( a[ 2 ], 300 ) ) )
        RETURN ""
    ENDIF

RETURN cDrive + ":\"

/* "net use X: /delete" for a drive chosen from the mapped ones */
FUNCTION cnc_NetUnmap()

    LOCAL a := cnc_Capture( "net use" ), aDrives := {}, aItems := {}, c, n

    FOR EACH c IN hb_ATokens( StrTran( a[ 2 ], Chr( 13 ), "" ), Chr( 10 ) )
        IF ( n := hb_At( ":", c ) ) > 1 .AND. IsAlpha( SubStr( c, n - 1, 1 ) ) .AND. "\\" $ c
            AAdd( aDrives, SubStr( c, n - 1, 2 ) )
            AAdd( aItems, AllTrim( c ) )
        ENDIF
    NEXT
    IF Empty( aDrives )
        HTToast():show( "No network drives", 1500, HT_CLR_TOAST_INFO )
        RETURN ""
    ENDIF
    n := cnc_List( "Disconnect network drive", aItems, 1 )
    IF n == 0
        RETURN ""
    ENDIF
    a := cnc_Capture( "net use " + aDrives[ n ] + " /delete /y" )
    IF a[ 1 ] != 0
        HTMessageBox():error( "Network", AllTrim( Left( a[ 2 ], 300 ) ) )
        RETURN ""
    ENDIF

RETURN aDrives[ n ]
