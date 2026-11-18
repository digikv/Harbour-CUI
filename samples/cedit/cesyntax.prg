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

FUNCTION ce_SyntaxNames()
RETURN { "harbour", "c", "sql", "python", "shell", "bat", "ini", "html" }

FUNCTION ce_SyntaxTitle( cName )
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

    LOCAL cExt := Lower( hb_FNameExt( cFile ) ), aMap, a

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

/* colors: base = the text color of the editor; the others with its
   background, chosen for a light or a dark one */
FUNCTION ce_SyntaxColors( cBase )

    LOCAL n := At( "/", cBase ), cBg := IIF( n > 0, SubStr( cBase, n + 1 ), "N" ), aFg

    IF "*" $ cBg .OR. "+" $ cBg .OR. Upper( StrTran( cBg, "+", "" ) ) == "W"
        aFg := { "B", "R", "G", "RB", "BG" }            /* light background */
    ELSE
        aFg := { "W+", "GR+", "BG", "RB+", "G+" }       /* dark background */
    ENDIF

RETURN { cBase, aFg[ 1 ] + "/" + cBg, aFg[ 2 ] + "/" + cBg, aFg[ 3 ] + "/" + cBg, aFg[ 4 ] + "/" + cBg, aFg[ 5 ] + "/" + cBg }

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
