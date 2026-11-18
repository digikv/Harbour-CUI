/*
 * CEdit - syntax highlighting: the languages and the colorizer of a line
 *
 * Copyright (c) 2026 Srđan Dragojlović - dual licensed, see LICENSE
 *
 * A language is a hash: keywords, line comments ("//"), comments at the
 * start of a line ("*" in Harbour, "REM" in .bat), a block comment ("/*" ..),
 * string quotes, escapes (\"), the preprocessor mark (#), and some flags:
 * HTML tags, INI sections and keys, Harbour's .T. / .AND. ...
 * ce_Colorize() gives the color of every character of a line and the state
 * at its end: 1 = inside a block comment that goes on in the next line.
 */

#define S_BASE   1
#define S_KW     2
#define S_STR    3
#define S_COM    4
#define S_NUM    5
#define S_PP     6

STATIC s_hDefs := NIL
STATIC s_aUser := {}       /* languages of cedit.lang: { cName, cTitle, aExt, hDef } */

FUNCTION ce_SyntaxNames()

    LOCAL aNames := { "harbour", "c", "sql", "python", "shell", "bat", "ini", "html" }, a

    FOR EACH a IN s_aUser
        IF AScan( aNames, {| c | c == a[ 1 ] } ) == 0
            AAdd( aNames, a[ 1 ] )
        ENDIF
    NEXT

RETURN aNames

FUNCTION ce_SyntaxTitle( cName )

    LOCAL n := AScan( s_aUser, {| a | a[ 1 ] == cName } )

    IF n > 0
        RETURN s_aUser[ n ][ 2 ]
    ENDIF
    SWITCH cName
    CASE "harbour" ; RETURN "Harbour / Clipper / xBase"
    CASE "c"       ; RETURN "C, C++, C#, Java, JavaScript, PHP, Go"
    CASE "sql"     ; RETURN "SQL"
    CASE "python"  ; RETURN "Python"
    CASE "shell"   ; RETURN "Shell script"
    CASE "bat"     ; RETURN "Windows batch (.bat, .cmd)"
    CASE "ini"     ; RETURN "INI, hbmk2 (.hbp, .hbc), config"
    CASE "html"    ; RETURN "HTML, XML"
    ENDSWITCH
RETURN "Plain text"

/* language of a file by its extension, "" = plain text */
FUNCTION ce_SyntaxFor( cFile )

    LOCAL cExt := Lower( hb_FNameExt( cFile ) ), a

    FOR EACH a IN s_aUser                         /* cedit.lang first */
        IF AScan( a[ 3 ], {| x | x == cExt } ) > 0
            RETURN a[ 1 ]
        ENDIF
    NEXT

RETURN ce_SyntaxBuiltin( cExt )

/* the built-in language of an extension (".prg") */
STATIC FUNCTION ce_SyntaxBuiltin( cExt )

    LOCAL aMap, a

    aMap := { ;
        { "harbour", { ".prg", ".ch", ".hb", ".ppo", ".prw", ".prx", ".fmg", ".api" } }, ;
        { "c",       { ".c", ".h", ".cpp", ".hpp", ".cc", ".cxx", ".hxx", ".cs", ".java", ".js", ".mjs", ".ts", ;
                       ".php", ".go", ".rs", ".kt", ".swift", ".json", ".rc", ".css" } }, ;
        { "sql",     { ".sql" } }, ;
        { "python",  { ".py", ".pyw" } }, ;
        { "shell",   { ".sh", ".bash", ".zsh" } }, ;
        { "bat",     { ".bat", ".cmd" } }, ;
        { "ini",     { ".ini", ".hbp", ".hbc", ".hbm", ".cfg", ".conf", ".toml", ".properties", ".reg", ".inf" } }, ;
        { "html",    { ".html", ".htm", ".xml", ".xsl", ".xslt", ".svg", ".xaml", ".csproj", ".vcxproj", ".plist" } } }

    FOR EACH a IN aMap
        IF AScan( a[ 2 ], {| x | x == cExt } ) > 0
            RETURN a[ 1 ]
        ENDIF
    NEXT

RETURN ""

/* the definition of a language (made once), NIL for an unknown name */
FUNCTION ce_SyntaxDef( cName )

    LOCAL n := AScan( s_aUser, {| a | a[ 1 ] == cName } )

    IF n > 0
        RETURN s_aUser[ n ][ 4 ]
    ENDIF

RETURN ce_BuiltinDef( cName )

STATIC FUNCTION ce_BuiltinDef( cName )

    IF s_hDefs == NIL
        s_hDefs := { => }
        s_hDefs[ "harbour" ] := ce_Def( .F., ;
            "FUNCTION PROCEDURE RETURN LOCAL STATIC PRIVATE PUBLIC MEMVAR FIELD PARAMETERS DECLARE " + ;
            "IF ELSE ELSEIF ENDIF END DO WHILE ENDDO FOR NEXT TO STEP EACH IN DESCEND CASE OTHERWISE ENDCASE " + ;
            "SWITCH ENDSWITCH EXIT LOOP BEGIN SEQUENCE RECOVER USING ALWAYS BREAK WITH OBJECT ENDWITH " + ;
            "CLASS ENDCLASS CREATE METHOD DATA VAR INIT INLINE FROM INHERIT EXPORTED EXPORT PROTECTED HIDDEN " + ;
            "VISIBLE PUBLISHED PROPERTY READ WRITE READWRITE CONSTRUCTOR DESTRUCTOR ACCESS ASSIGN " + ;
            "NIL SELF SUPER QUIT REQUEST ANNOUNCE EXTERNAL DYNAMIC THREAD AS " + ;
            "USE SELECT SKIP GO GOTO TOP BOTTOM SEEK REPLACE APPEND BLANK DELETE RECALL PACK ZAP INDEX ON " + ;
            "SET CLOSE ALL COPY SAY GET PICTURE VALID WHEN CLEAR COMMIT UNLOCK ALIAS SHARED EXCLUSIVE NEW " + ;
            "KEYBOARD ACCEPT INPUT WAIT MENU PROMPT", ;
            { "//", "&&" }, { "*", "NOTE " }, { "/*", "*/" }, ["'], .F., "#" )
        s_hDefs[ "harbour" ][ "dotkw" ] := { ".AND.", ".OR.", ".NOT.", ".T.", ".F.", ".Y.", ".N." }
        s_hDefs[ "c" ] := ce_Def( .T., ;
            "if else for while do switch case default break continue return goto struct union enum typedef " + ;
            "static extern const volatile void char short int long float double signed unsigned sizeof " + ;
            "auto register inline class public private protected virtual override final new delete this " + ;
            "namespace using template typename try catch throw finally true false NULL nullptr bool " + ;
            "function var let import export package interface extends implements instanceof typeof " + ;
            "async await yield null undefined echo fn func go defer chan map range type struct impl mut " + ;
            "pub use mod self Self match loop where string object base lock foreach in out ref", ;
            { "//" }, {}, { "/*", "*/" }, ["'`], .T., "#" )
        s_hDefs[ "sql" ] := ce_Def( .F., ;
            "SELECT FROM WHERE INSERT INTO VALUES UPDATE SET DELETE CREATE TABLE ALTER DROP INDEX VIEW " + ;
            "AND OR NOT NULL IS IN LIKE BETWEEN JOIN LEFT RIGHT INNER OUTER FULL CROSS ON GROUP BY ORDER " + ;
            "HAVING AS DISTINCT UNION ALL LIMIT OFFSET TOP PRIMARY KEY FOREIGN REFERENCES DEFAULT UNIQUE " + ;
            "CASE WHEN THEN ELSE END BEGIN COMMIT ROLLBACK TRANSACTION EXISTS COUNT SUM AVG MIN MAX ASC DESC " + ;
            "INTEGER INT VARCHAR CHAR TEXT DATE DATETIME TIMESTAMP DECIMAL NUMERIC FLOAT REAL BOOLEAN BLOB " + ;
            "CONSTRAINT CHECK TRIGGER PROCEDURE FUNCTION RETURNS DECLARE IF WHILE", ;
            { "--" }, {}, { "/*", "*/" }, ['], .F., "" )
        s_hDefs[ "python" ] := ce_Def( .T., ;
            "def class return if elif else for while in import from as with try except finally raise pass " + ;
            "break continue lambda None True False and or not is global nonlocal yield async await del assert print self", ;
            { "#" }, {}, NIL, ["'], .T., "" )
        s_hDefs[ "shell" ] := ce_Def( .T., ;
            "if then else elif fi for do done while until case esac function in select return exit " + ;
            "echo export local read shift set unset source alias cd test true false", ;
            { "#" }, {}, NIL, ["'], .T., "" )
        s_hDefs[ "bat" ] := ce_Def( .F., ;
            "ECHO SET IF ELSE GOTO CALL EXIT FOR DO IN NOT EXIST ERRORLEVEL DEFINED SETLOCAL ENDLOCAL " + ;
            "SHIFT PAUSE START CD CHDIR COPY DEL MOVE REN MD MKDIR RD RMDIR CLS TITLE OFF ON NUL EQU NEQ LSS LEQ GTR GEQ", ;
            {}, { "REM ", "REM", "@REM", "::" }, NIL, ["], .F., "" )
        s_hDefs[ "ini" ] := ce_Def( .F., "", {}, { ";", "#" }, NIL, ["], .F., "" )
        s_hDefs[ "ini" ][ "section" ] := .T.
        s_hDefs[ "html" ] := ce_Def( .F., "", {}, {}, { "<!--", "-->" }, ["'], .F., "" )
        s_hDefs[ "html" ][ "tags" ] := .T.
    ENDIF

RETURN hb_HGetDef( s_hDefs, cName, NIL )

STATIC FUNCTION ce_Def( lCase, cKeywords, aLine, aFirst, aBlock, cQuotes, lEsc, cPP )

    LOCAL h := { => }, hKw := { => }, c

    FOR EACH c IN hb_ATokens( cKeywords, " " )
        IF ! Empty( c )
            hKw[ IIF( lCase, c, Upper( c ) ) ] := .T.
        ENDIF
    NEXT
    h[ "case" ]    := lCase
    h[ "kw" ]      := hKw
    h[ "line" ]    := aLine
    h[ "first" ]   := aFirst
    h[ "block" ]   := aBlock
    h[ "quotes" ]  := cQuotes
    h[ "esc" ]     := lEsc
    h[ "pp" ]      := cPP
    h[ "dotkw" ]   := {}
    h[ "section" ] := .F.
    h[ "tags" ]    := .F.

RETURN h

/* Colors: base = the text color of the editor; keyword, string, comment,
   number, preprocessor on its background. A theme's palette can make any
   color index look like anything (Classic: 14 = light blue), so for every
   role the first candidate that stands out from the background in the
   palette of the theme is taken. */
FUNCTION ce_SyntaxColors( cBase )

    LOCAL n := At( "/", cBase ), cBg := IIF( n > 0, SubStr( cBase, n + 1 ), "0" )
    LOCAL nBg := ce_ColorIndex( cBg ), nText := ce_ColorIndex( IIF( n > 0, Left( cBase, n - 1 ), cBase ) )
    LOCAL aPal, nLBg, aRoles, aOut := { cBase }, aTaken := {}, a, nFg, nPick, nPass

    aPal := HTTheme():themePalette( HTTheme():themeName() )
    IF ! HB_ISARRAY( aPal ) .OR. Len( aPal ) < 16
        aPal := { 0, 8388608, 32768, 8421376, 128, 8388736, 32896, 12632256, ;
                  8421504, 16711680, 65280, 16776960, 255, 16711935, 65535, 16777215 }   /* VGA */
    ENDIF
    nLBg := ce_Lum( aPal[ nBg + 1 ] )
    IF nLBg > 128
        /* light background: blue, red, green, magenta, cyan (dark ones first) */
        aRoles := { { 1, 9, 0, 5, 4, 3, 2, 6, 8 }, { 4, 12, 5, 6, 1, 2 }, { 2, 10, 8, 3, 6 }, ;
                    { 5, 13, 4, 1, 3 }, { 3, 11, 6, 8, 1 } }
    ELSE
        /* dark background: white, yellow, green, magenta, cyan (bright ones first) */
        aRoles := { { 15, 14, 11, 7, 10, 9, 13, 12 }, { 14, 12, 10, 6, 11, 7, 13 }, { 10, 2, 11, 8, 7, 3, 13 }, ;
                    { 13, 11, 12, 5, 9, 7 }, { 11, 10, 3, 13, 6, 7 } }
    ENDIF
    FOR EACH a IN aRoles
        /* 1st pass: readable on the background, unlike the text and the
           other roles; 2nd: readable, not taken; else the last candidate */
        nPick := -1
        FOR nPass := 1 TO 2
            FOR EACH nFg IN a
                IF nFg != nBg .AND. AScan( aTaken, nFg ) == 0 .AND. ;
                   Abs( ce_Lum( aPal[ nFg + 1 ] ) - nLBg ) >= 90 .AND. ;
                   ( nPass == 2 .OR. ( nFg != nText .AND. ce_Dist( aPal[ nFg + 1 ], aPal[ nText + 1 ] ) >= 80 ) )
                    nPick := nFg
                    EXIT
                ENDIF
            NEXT
            IF nPick >= 0
                EXIT
            ENDIF
        NEXT
        IF nPick < 0
            nPick := ATail( a )
        ENDIF
        AAdd( aTaken, nPick )
        AAdd( aOut, hb_ntos( nPick ) + "/" + cBg )
    NEXT

RETURN aOut

/* distance of two COLORREFs */
STATIC FUNCTION ce_Dist( n1, n2 )
RETURN Sqrt( ( n1 % 256 - n2 % 256 ) ^ 2 + ;
             ( Int( n1 / 256 ) % 256 - Int( n2 / 256 ) % 256 ) ^ 2 + ;
             ( Int( n1 / 65536 ) % 256 - Int( n2 / 65536 ) % 256 ) ^ 2 )

/* "14", "GR+", "W*" -> color index 0..15 */
STATIC FUNCTION ce_ColorIndex( c )

    LOCAL lBright, n

    c := Upper( AllTrim( c ) )
    IF IsDigit( c )
        RETURN Max( 0, Min( 15, Val( c ) ) )
    ENDIF
    lBright := "+" $ c .OR. "*" $ c
    c := StrTran( StrTran( c, "+", "" ), "*", "" )
    n := AScan( { "N", "B", "G", "BG", "R", "RB", "GR", "W" }, {| x | x == c } )
    IF n == 0
        n := AScan( { "N", "B", "G", "GB", "R", "BR", "RG", "W" }, {| x | x == c } )
    ENDIF

RETURN Max( 0, n - 1 ) + IIF( lBright, 8, 0 )

/* brightness 0..255 of a COLORREF (0x00BBGGRR) */
STATIC FUNCTION ce_Lum( nRGB )

    LOCAL nR := nRGB % 256, nG := Int( nRGB / 256 ) % 256, nB := Int( nRGB / 65536 ) % 256

RETURN 0.299 * nR + 0.587 * nG + 0.114 * nB

/* { aColors, nEndState } of the characters aCh of a line, starting in nState */
FUNCTION ce_Colorize( aCh, nState, hDef, aClr )

    LOCAL n := Len( aCh ), aC := Array( n ), i, j, c, cTok, nFirst, cWord, lPP := .F., cKey

    AFill( aC, aClr[ S_BASE ] )
    nFirst := 1
    DO WHILE nFirst <= n .AND. ( aCh[ nFirst ] == " " .OR. aCh[ nFirst ] == Chr( 9 ) )
        nFirst++
    ENDDO
    i := 1

    /* the rest of a block comment from the line before */
    IF nState == 1 .AND. hDef[ "block" ] != NIL
        j := ce_FindTok( aCh, 1, hDef[ "block" ][ 2 ] )
        IF j == 0
            ce_Paint( aC, 1, n, aClr[ S_COM ] )
            RETURN { aC, 1 }
        ENDIF
        i := j + Len( hDef[ "block" ][ 2 ] )
        ce_Paint( aC, 1, i - 1, aClr[ S_COM ] )
    ENDIF

    IF i == 1 .AND. nFirst <= n
        /* a comment line: "*" in Harbour, "REM" / "::" in a .bat, ";" in an INI */
        FOR EACH cTok IN hDef[ "first" ]
            /* a word ("REM") must end there: "REMOVE" is not a comment */
            IF ce_TokAt( aCh, nFirst, cTok, .T. ) .AND. ;
               ( ! IsAlpha( Right( cTok, 1 ) ) .OR. nFirst + Len( cTok ) > n .OR. ! ce_IsWordChar( aCh[ nFirst + Len( cTok ) ] ) )
                ce_Paint( aC, nFirst, n, aClr[ S_COM ] )
                RETURN { aC, 0 }
            ENDIF
        NEXT
        /* INI: [section] */
        IF hDef[ "section" ] .AND. aCh[ nFirst ] == "["
            ce_Paint( aC, nFirst, n, aClr[ S_KW ] )
            RETURN { aC, 0 }
        ENDIF
        lPP := ! Empty( hDef[ "pp" ] ) .AND. aCh[ nFirst ] == hDef[ "pp" ]
    ENDIF

    DO WHILE i <= n
        c := aCh[ i ]

        /* block comment */
        IF hDef[ "block" ] != NIL .AND. ce_TokAt( aCh, i, hDef[ "block" ][ 1 ], .F. )
            j := ce_FindTok( aCh, i + Len( hDef[ "block" ][ 1 ] ), hDef[ "block" ][ 2 ] )
            IF j == 0
                ce_Paint( aC, i, n, aClr[ S_COM ] )
                RETURN { aC, 1 }
            ENDIF
            ce_Paint( aC, i, j + Len( hDef[ "block" ][ 2 ] ) - 1, aClr[ S_COM ] )
            i := j + Len( hDef[ "block" ][ 2 ] )
            LOOP
        ENDIF

        /* line comment */
        FOR EACH cTok IN hDef[ "line" ]
            IF ce_TokAt( aCh, i, cTok, .F. )
                ce_Paint( aC, i, n, aClr[ S_COM ] )
                RETURN { aC, 0 }
            ENDIF
        NEXT

        /* string */
        IF hb_BLen( c ) == 1 .AND. c $ hDef[ "quotes" ]
            j := i + 1
            DO WHILE j <= n .AND. !( aCh[ j ] == c )
                IF hDef[ "esc" ] .AND. aCh[ j ] == "\"
                    j++
                ENDIF
                j++
            ENDDO
            ce_Paint( aC, i, Min( j, n ), aClr[ S_STR ] )
            i := j + 1
            LOOP
        ENDIF

        /* HTML / XML tag: <name ...>, the values in quotes as strings */
        IF hDef[ "tags" ] .AND. c == "<"
            j := i
            DO WHILE j <= n .AND. !( aCh[ j ] == ">" )
                IF aCh[ j ] == '"' .OR. aCh[ j ] == "'"
                    cTok := aCh[ j ]
                    aC[ j ] := aClr[ S_STR ]
                    j++
                    DO WHILE j <= n .AND. !( aCh[ j ] == cTok )
                        aC[ j ] := aClr[ S_STR ]
                        j++
                    ENDDO
                    IF j <= n
                        aC[ j ] := aClr[ S_STR ]
                    ENDIF
                ELSE
                    aC[ j ] := aClr[ S_KW ]
                ENDIF
                j++
            ENDDO
            IF j <= n
                aC[ j ] := aClr[ S_KW ]
            ENDIF
            i := j + 1
            LOOP
        ENDIF

        /* Harbour .T. .AND. ... */
        IF c == "." .AND. ! Empty( hDef[ "dotkw" ] )
            j := 0
            FOR EACH cTok IN hDef[ "dotkw" ]
                IF ce_TokAt( aCh, i, cTok, .T. )
                    j := Len( cTok )
                    EXIT
                ENDIF
            NEXT
            IF j > 0
                ce_Paint( aC, i, i + j - 1, aClr[ S_KW ] )
                i += j
                LOOP
            ENDIF
        ENDIF

        /* number */
        IF IsDigit( c ) .AND. ( i == 1 .OR. ! ce_IsWordChar( aCh[ i - 1 ] ) )
            j := i
            DO WHILE j <= n .AND. ( ce_IsWordChar( aCh[ j ] ) .OR. ( aCh[ j ] == "." .AND. j < n .AND. IsDigit( aCh[ j + 1 ] ) ) )
                j++
            ENDDO
            ce_Paint( aC, i, j - 1, IIF( lPP, aClr[ S_PP ], aClr[ S_NUM ] ) )
            i := j
            LOOP
        ENDIF

        /* word: keyword, INI key */
        IF ce_IsWordChar( c )
            j := i
            cWord := ""
            DO WHILE j <= n .AND. ce_IsWordChar( aCh[ j ] )
                cWord += aCh[ j ]
                j++
            ENDDO
            cKey := IIF( hDef[ "case" ], cWord, Upper( cWord ) )
            DO CASE
            CASE lPP
                ce_Paint( aC, i, j - 1, aClr[ S_PP ] )
            CASE hb_HHasKey( hDef[ "kw" ], cKey )
                ce_Paint( aC, i, j - 1, aClr[ S_KW ] )
            CASE hDef[ "section" ] .AND. i == nFirst
                ce_Paint( aC, i, j - 1, aClr[ S_KW ] )
            ENDCASE
            i := j
            LOOP
        ENDIF

        IF lPP
            aC[ i ] := aClr[ S_PP ]
        ENDIF
        i++
    ENDDO

RETURN { aC, 0 }

STATIC PROCEDURE ce_Paint( aC, nFrom, nTo, cColor )
    LOCAL i
    FOR i := Max( 1, nFrom ) TO Min( nTo, Len( aC ) )
        aC[ i ] := cColor
    NEXT
RETURN

/* cTok (ASCII) at character i of aCh; lIgnoreCase: "rem" = "REM" */
STATIC FUNCTION ce_TokAt( aCh, i, cTok, lIgnoreCase )

    LOCAL j, n := Len( cTok ), c

    IF i + n - 1 > Len( aCh )
        RETURN .F.
    ENDIF
    FOR j := 1 TO n
        c := aCh[ i + j - 1 ]
        IF lIgnoreCase
            c := Upper( c )
        ENDIF
        IF !( c == SubStr( cTok, j, 1 ) )
            RETURN .F.
        ENDIF
    NEXT

RETURN .T.

/* where cTok starts at character i or after it, 0 = not in the line */
STATIC FUNCTION ce_FindTok( aCh, i, cTok )

    LOCAL n := Len( aCh )

    DO WHILE i <= n
        IF ce_TokAt( aCh, i, cTok, .F. )
            RETURN i
        ENDIF
        i++
    ENDDO

RETURN 0

/*
 * cedit.lang: languages in the format of HbEdit's hbedit.ini (Alexander S. Kresin),
 * so its [LANG_xxx] sections can be copied (or the whole hbedit.ini used):
 *
 *   [LANG_PRG]                 the name: "prg"
 *   ext=.prg.ch                extensions
 *   keywords1=if else endif    keywords1 .. keywords9 (all in the keyword color)
 *   scomm=//                   comment to the end of the line ("rem": a comment line)
 *   mcomm=<open> <close>       block comment; "< >" = HTML / XML tags, "[ ]" = INI sections
 *   startline=#                preprocessor / label mark at the line start
 *   case=off                   keywords in any case (default: on)
 *   quotes="                   string quotes (default: " and ')
 *
 * A color before a comma ("mcomm=W+/B,[ ]") is skipped; scomm= with a color is
 * not a comment (HbEdit only colors with it); plugin= and brackets= are skipped.
 * CEdit's own keys: title= (in View - Language), first= (comments at the line
 * start, e.g. "*"), dotwords= (.AND. .T. ...), escape=on|off (backslash in
 * strings, default = case), base=harbour|c|sql|python|shell|bat|ini|html|none.
 * A language for the extensions of a built-in one starts from it (or from base=):
 * its keywords, comments and quotes are added to the built-in ones.
 * Returns the number of languages read.
 */
FUNCTION ce_SyntaxLoad( cFile )

    LOCAL hIni, cSect, h, cName, aExt, cBase, hBase, hDef, c, a, lCase, n

    s_aUser := {}
    IF ! hb_FileExists( cFile )
        RETURN 0
    ENDIF
    hIni := hb_iniRead( cFile, .F., "=", .F. )
    IF ! HB_ISHASH( hIni )
        RETURN 0
    ENDIF
    FOR EACH cSect IN hb_HKeys( hIni )
        IF !( Upper( Left( cSect, 5 ) ) == "LANG_" ) .OR. Len( cSect ) < 6 .OR. ! HB_ISHASH( hIni[ cSect ] )
            LOOP
        ENDIF
        h := hIni[ cSect ]
        hb_HCaseMatch( h, .F. )
        cName := Lower( SubStr( cSect, 6 ) )

        aExt := {}
        FOR EACH c IN hb_ATokens( StrTran( StrTran( hb_HGetDef( h, "ext", "" ), ",", " " ), ".", " ." ), " " )
            IF Len( c ) > 1
                AAdd( aExt, Lower( c ) )
            ENDIF
        NEXT

        cBase := Lower( AllTrim( hb_HGetDef( h, "base", "" ) ) )
        IF Empty( cBase ) .AND. Len( aExt ) > 0
            cBase := ce_SyntaxBuiltin( aExt[ 1 ] )
        ENDIF
        hBase := IIF( Empty( cBase ) .OR. cBase == "none", NIL, ce_BuiltinDef( cBase ) )

        IF hb_HHasKey( h, "case" )
            lCase := ! ce_IsOff( h[ "case" ] )
        ELSE
            lCase := IIF( hBase == NIL, .T., hBase[ "case" ] )
        ENDIF
        IF hBase == NIL
            hDef := ce_Def( lCase, "", {}, {}, NIL, ["'], lCase, "" )
        ELSE
            hDef := ce_CopyDef( hBase, lCase )
        ENDIF

        FOR n := 1 TO 9
            FOR EACH c IN hb_ATokens( hb_HGetDef( h, "keywords" + hb_ntos( n ), "" ), " " )
                IF ! Empty( c )
                    hDef[ "kw" ][ IIF( lCase, c, Upper( c ) ) ] := .T.
                ENDIF
            NEXT
        NEXT

        /* "scomm=W+/B,-" (.hbp options) only colors, it is not a comment */
        c := ce_LangValue( h, "scomm" )
        IF ! Empty( c ) .AND. c == AllTrim( hb_HGetDef( h, "scomm", "" ) )
            IF IsAlpha( c )                      /* "rem": a comment line */
                ce_AddTok( hDef[ "first" ], c )
            ELSE
                ce_AddTok( hDef[ "line" ], c )
            ENDIF
        ENDIF
        FOR EACH c IN hb_ATokens( ce_LangValue( h, "first" ), " " )
            IF ! Empty( c )
                ce_AddTok( hDef[ "first" ], c )
            ENDIF
        NEXT

        a := ce_NoEmpty( hb_ATokens( ce_LangValue( h, "mcomm" ), " " ) )
        IF Len( a ) >= 2
            DO CASE
            CASE a[ 1 ] == "<" .AND. a[ 2 ] == ">"
                hDef[ "tags" ] := .T.
            CASE a[ 1 ] == "[" .AND. a[ 2 ] == "]"
                hDef[ "section" ] := .T.
            OTHERWISE
                hDef[ "block" ] := { a[ 1 ], a[ 2 ] }
            ENDCASE
        ENDIF

        c := ce_LangValue( h, "startline" )
        IF ! Empty( c )
            hDef[ "pp" ] := Left( c, 1 )
        ENDIF
        c := StrTran( hb_HGetDef( h, "quotes", "" ), " ", "" )
        IF ! Empty( c )
            IF hBase == NIL
                hDef[ "quotes" ] := ""
            ENDIF
            FOR n := 1 TO hb_BLen( c )                  /* quotes are one-byte characters */
                IF !( hb_BSubStr( c, n, 1 ) $ hDef[ "quotes" ] )
                    hDef[ "quotes" ] += hb_BSubStr( c, n, 1 )
                ENDIF
            NEXT
        ENDIF
        IF hb_HHasKey( h, "escape" )
            hDef[ "esc" ] := ! ce_IsOff( h[ "escape" ] )
        ENDIF
        FOR EACH c IN hb_ATokens( hb_HGetDef( h, "dotwords", "" ), " " )
            IF ! Empty( c )
                ce_AddTok( hDef[ "dotkw" ], Upper( c ) )
            ENDIF
        NEXT

        a := { cName, hb_HGetDef( h, "title", "" ), aExt, hDef }
        IF Empty( a[ 2 ] )
            a[ 2 ] := Upper( cName )
            FOR EACH c IN aExt
                a[ 2 ] += IIF( c:__enumIndex() == 1, " (", " " ) + c + IIF( c:__enumIsLast(), ")", "" )
            NEXT
            a[ 2 ] += " - cedit.lang"
        ENDIF
        n := AScan( s_aUser, {| x | x[ 1 ] == cName } )
        IF n > 0
            s_aUser[ n ] := a
        ELSE
            AAdd( s_aUser, a )
        ENDIF
    NEXT

RETURN Len( s_aUser )

/* a copy of a built-in language that the user's one adds to */
STATIC FUNCTION ce_CopyDef( hBase, lCase )

    LOCAL h := { => }, c

    FOR EACH c IN hb_HKeys( hBase )
        h[ c ] := hBase[ c ]
    NEXT
    h[ "kw" ] := { => }
    FOR EACH c IN hb_HKeys( hBase[ "kw" ] )
        h[ "kw" ][ IIF( lCase, c, Upper( c ) ) ] := .T.
    NEXT
    h[ "case" ]  := lCase
    h[ "line" ]  := AClone( hBase[ "line" ] )
    h[ "first" ] := AClone( hBase[ "first" ] )
    h[ "dotkw" ] := AClone( hBase[ "dotkw" ] )
    IF hBase[ "block" ] != NIL
        h[ "block" ] := AClone( hBase[ "block" ] )
    ENDIF

RETURN h

STATIC FUNCTION ce_IsOff( c )
RETURN AScan( { "off", "no", "0", "false" }, {| x | x == Lower( AllTrim( c ) ) } ) > 0

/* a value without HbEdit's color before a comma ("W+/B,-" -> "-") */
STATIC FUNCTION ce_LangValue( h, cKey )

    LOCAL c := AllTrim( hb_HGetDef( h, cKey, "" ) ), n := At( ",", c )

    IF n > 1 .AND. "/" $ Left( c, n - 1 )
        c := AllTrim( SubStr( c, n + 1 ) )
    ENDIF

RETURN c

STATIC PROCEDURE ce_AddTok( a, c )
    IF AScan( a, {| x | x == c } ) == 0
        AAdd( a, c )
    ENDIF
RETURN

STATIC FUNCTION ce_NoEmpty( a )

    LOCAL aR := {}, c

    FOR EACH c IN a
        IF ! Empty( c )
            AAdd( aR, c )
        ENDIF
    NEXT

RETURN aR
