/*
 * CuiBuilder - the build engine (no user interface)
 *
 * Copyright (c) 2026 Srđan Dragojlović - dual licensed, see LICENSE
 *
 * Works like HwBuilder of Alexander S. Kresin (HwGUI): it runs the Harbour
 * compiler, the C compiler, the resource compiler and the linker itself,
 * from command templates of the compiler family (cuibuild.ini), and builds
 * only the files that changed. Written from scratch, no HwBuilder code.
 *
 *   cuibuild.ini   [HARBOUR] [GUILIB] [C_COMPILER] [C_COMPILER_2] ... [VIEW]
 *                  (the sections and keys of hwbuild.ini, a few more)
 *   .hwprj         HwBuilder projects (read and written)
 *   .hbp .hbm .hbc hbmk2 projects (read: sources, -o, -l, -L, -i, -gt,
 *                  Harbour / C / linker flags, .hbc files, {filters}, ${macros})
 *
 * Used by cuibuilder.prg (Harbour-CUI screen) and cuibc.prg (command line).
 */

#include "hbclass.ch"
#include "fileio.ch"
#include "cuibuild.ch"

STATIC s_aDepCache := NIL            /* include dependencies of one build: { file, tMax } */

/*=========================================================== settings */

/* where cuibuild.ini is: next to the program when it is there, else
   %APPDATA%\CuiBuilder (Windows) or ~/.config/cuibuilder */
FUNCTION cb_IniFile()

    LOCAL cIni := hb_DirBase() + "cuibuild.ini", cDir

    IF hb_FileExists( cIni )
        RETURN cIni
    ENDIF
#ifdef __PLATFORM__WINDOWS
    cDir := GetEnv( "APPDATA" )
    IF Empty( cDir )
        RETURN cIni
    ENDIF
    cDir += "\CuiBuilder"
#else
    cDir := GetEnv( "HOME" ) + "/.config/cuibuilder"
#endif
    IF ! hb_DirExists( cDir )
        hb_DirBuild( cDir )
    ENDIF

RETURN cDir + hb_ps() + "cuibuild.ini"

/* the default compiler family of this platform */
FUNCTION cb_DefFamily()
#ifdef __PLATFORM__WINDOWS
RETURN "mingw"
#else
RETURN "gcc"
#endif

/* an empty compiler entry with the defaults of a family */
FUNCTION cb_CompNew( cId, cFamily )

    LOCAL h := { => }

    hb_HCaseMatch( h, .F. )
    cFamily := Lower( hb_defaultValue( cFamily, cId ) )
    IF AScan( { "mingw", "gcc", "clang", "msvc", "bcc" }, cFamily ) == 0
        cFamily := cb_DefFamily()
    ENDIF
    h[ "id" ]         := cId
    h[ "family" ]     := cFamily
    h[ "bin_path" ]   := ""
    h[ "harbour_lib_path" ] := ""
    h[ "def_cflags" ] := cb_FamDef( cFamily, "cflags" )
    h[ "def_linkflags" ] := cb_FamDef( cFamily, "lflagsgui" )
    h[ "def_linkflagscons" ] := cb_FamDef( cFamily, "lflagscons" )
    h[ "def_libflags" ] := ""
    h[ "def_syslibs" ] := cb_FamDef( cFamily, "syslibs" )
    h[ "cmd_comp" ]   := ""
    h[ "cmd_res" ]    := ""
    h[ "cmd_lib" ]    := ""
    h[ "cmd_exe" ]    := ""
    h[ "env" ]        := { => }

RETURN h

/* what a compiler family does: command templates and formats.
   {path} = bin_path with a separator at the end ("" = from the PATH) */
FUNCTION cb_FamDef( cFamily, cKey )

    LOCAL h

    SWITCH cFamily
    CASE "msvc"
        h := { "comp" => "{path}cl.exe {f} {incs} /Fo{obj} {src}", ;
               "res"  => "{path}rc.exe /nologo {incs} /fo {out} {src}", ;
               "lib"  => "{path}lib.exe /nologo {f} /out:{out} {objs}", ;
               "exe"  => "{path}link.exe /nologo {f} /out:{out} {objs} {res} {libpaths} {libs}", ;
               "cflags" => "/nologo /c /O2 /W3", ;
               "lflagsgui" => "/SUBSYSTEM:WINDOWS", "lflagscons" => "/SUBSYSTEM:CONSOLE", ;
               "syslibs" => "user32.lib gdi32.lib comdlg32.lib shell32.lib comctl32.lib winspool.lib advapi32.lib " + ;
                            "winmm.lib ws2_32.lib iphlpapi.lib oleaut32.lib ole32.lib uuid.lib mpr.lib imm32.lib msimg32.lib wininet.lib", ;
               "inc" => "/I", "libpath" => "/LIBPATH:", "libtmpl" => "{l}.lib", "libfile" => "{l}.lib", ;
               "obj" => ".obj", "libext" => ".lib", "resobj" => ".res", "objpref" => "", "exeext" => ".exe" }
        EXIT
    CASE "bcc"
        h := { "comp" => "{path}bcc32.exe {f} {incs} -o{obj} {src}", ;
               "res"  => "{path}brc32.exe -r {incs} -fo{out} {src}", ;
               "lib"  => "{path}tlib.exe {f} {out} {objs}", ;
               "exe"  => "{path}ilink32.exe {f} {libpaths} {objs}, {out},, {libs},, {res}", ;
               "cflags" => "-c -q -d -w -O2", ;
               "lflagsgui" => "-Gn -aa -Tpe c0w32.obj", "lflagscons" => "-Gn -ap -Tpe c0x32.obj", ;
               "syslibs" => "ws2_32.lib cw32.lib import32.lib iphlpapi.lib", ;
               "inc" => "-I", "libpath" => "-L", "libtmpl" => "{l}.lib", "libfile" => "{l}.lib", ;
               "obj" => ".obj", "libext" => ".lib", "resobj" => ".res", "objpref" => "+", "exeext" => ".exe" }
        EXIT
    CASE "gcc"
    CASE "clang"
        h := { "comp" => "{path}" + cFamily + " {f} {incs} -o{obj} {src}", ;
               "res"  => "", ;
               "lib"  => "{path}ar rcs {f} {out} {objs}", ;
               "exe"  => "{path}" + cFamily + " {f} -o{out} {objs} {libpaths} -Wl,--start-group {libs} -Wl,--end-group", ;
               "cflags" => "-c -O2 -W -Wall -pipe", ;
               "lflagsgui" => "", "lflagscons" => "", ;
               "syslibs" => cb_UnixSysLibs(), ;
               "inc" => "-I", "libpath" => "-L", "libtmpl" => "-l{l}", "libfile" => "lib{l}.a", ;
               "obj" => ".o", "libext" => ".a", "resobj" => "", "objpref" => "", "exeext" => "" }
        EXIT
    OTHERWISE   /* mingw */
        h := { "comp" => "{path}gcc.exe {f} {incs} -o{obj} {src}", ;
               "res"  => "{path}windres.exe {incs} {src} -O coff -o {out}", ;
               "lib"  => "{path}ar.exe rcs {f} {out} {objs}", ;
               "exe"  => "{path}gcc.exe {f} -o{out} {objs} {res} {libpaths} -Wl,--allow-multiple-definition -Wl,--start-group {libs} -Wl,--end-group", ;
               "cflags" => "-c -O2 -W -Wall -pipe", ;
               "lflagsgui" => "-mwindows", "lflagscons" => "-mconsole", ;
               "syslibs" => "-lwinmm -lkernel32 -luser32 -lgdi32 -ladvapi32 -lws2_32 -liphlpapi -lwinspool -lcomctl32 " + ;
                            "-lcomdlg32 -lshell32 -luuid -lole32 -loleaut32 -lmpr -lmapi32 -limm32 -lmsimg32 -lwininet", ;
               "inc" => "-I", "libpath" => "-L", "libtmpl" => "-l{l}", "libfile" => "lib{l}.a", ;
               "obj" => ".o", "libext" => ".a", "resobj" => ".o", "objpref" => "", "exeext" => ".exe" }
    ENDSWITCH

RETURN IIF( hb_HHasKey( h, cKey ), h[ cKey ], "" )

STATIC FUNCTION cb_UnixSysLibs()

    LOCAL c := "-lm -ldl -lpthread -lrt", cDir

    FOR EACH cDir IN { "/usr/lib/x86_64-linux-gnu/", "/usr/lib/i386-linux-gnu/", "/usr/lib64/", "/usr/lib/" }
        IF hb_FileExists( cDir + "libgpm.so" ) .OR. hb_FileExists( cDir + "libgpm.so.2" )
            c += " -lgpm"
            EXIT
        ENDIF
    NEXT
    c += " -lX11"

RETURN c

/* the Harbour libraries of a program, the GT libraries of the platform included */
FUNCTION cb_HbDefLibs()
#ifdef __PLATFORM__WINDOWS
RETURN "hbvm hbrtl hblang hbcpage gtcgi gtpca gtstd gtwin gtwvt gtgui hbrdd hbuddall hbusrrdd " + ;
       "rddntx rddcdx rddnsx rddfpt hbhsx hbsix hbmacro hbcplr hbpp hbcommon hbpcre hbzlib"
#else
RETURN "hbvm hbrtl hblang hbcpage gtcgi gtpca gtstd gttrm gtxwc hbrdd hbuddall hbusrrdd " + ;
       "rddntx rddcdx rddnsx rddfpt hbhsx hbsix hbmacro hbcplr hbpp hbcommon hbpcre hbzlib"
#endif

/* the settings: cuibuild.ini read, what is missing found on this computer */
FUNCTION cb_SettingsLoad( cIni )

    LOCAL hSet := cb_SettingsEmpty(), aLines, cLine, cSec := "", n, cKey, cVal, hComp := NIL

    cIni := hb_defaultValue( cIni, cb_IniFile() )
    hSet[ "ini" ] := cIni
    IF hb_FileExists( cIni )
        aLines := hb_ATokens( StrTran( hb_MemoRead( cIni ), Chr( 13 ), "" ), Chr( 10 ) )
        FOR EACH cLine IN aLines
            cLine := AllTrim( cLine )
            IF Empty( cLine ) .OR. Left( cLine, 1 ) $ "#;"
                LOOP
            ENDIF
            IF Left( cLine, 1 ) == "[" .AND. Right( cLine, 1 ) == "]"
                cSec := Upper( SubStr( cLine, 2, Len( cLine ) - 2 ) )
                IF Left( cSec, 10 ) == "C_COMPILER"
                    hComp := cb_CompNew( "", cb_DefFamily() )
                    hComp[ "family" ] := ""
                    AAdd( hSet[ "comps" ], hComp )
                ENDIF
                LOOP
            ENDIF
            IF ( n := At( "=", cLine ) ) == 0
                LOOP
            ENDIF
            cKey := Lower( AllTrim( Left( cLine, n - 1 ) ) )
            cVal := AllTrim( SubStr( cLine, n + 1 ) )
            DO CASE
            CASE cSec == "HARBOUR"
                hSet[ "harbour" ][ cKey ] := cVal
            CASE cSec == "GUILIB"
                hSet[ "guilib" ][ cKey ] := cVal
            CASE Left( cSec, 10 ) == "C_COMPILER" .AND. hComp != NIL
                IF Left( cKey, 4 ) == "env_"
                    hComp[ "env" ][ SubStr( cKey, 5 ) ] := cVal
                ELSE
                    hComp[ cKey ] := cVal
                ENDIF
            CASE cSec == "VIEW"
                hSet[ "view" ][ cKey ] := cVal
            CASE cSec == "RECENT"
                AAdd( hSet[ "recent" ], cVal )
            ENDCASE
        NEXT
        /* a family that was not given: from the id (hwbuild.ini has only id=) */
        FOR EACH hComp IN hSet[ "comps" ]
            IF Empty( hComp[ "family" ] )
                hComp[ "family" ] := IIF( AScan( { "mingw", "gcc", "clang", "msvc", "bcc" }, Lower( hComp[ "id" ] ) ) > 0, ;
                                          Lower( hComp[ "id" ] ), cb_DefFamily() )
            ENDIF
            IF Empty( hComp[ "id" ] )
                hComp[ "id" ] := hComp[ "family" ]
            ENDIF
        NEXT
    ENDIF
    cb_SettingsDetect( hSet )

RETURN hSet

STATIC FUNCTION cb_SettingsEmpty()

    LOCAL hSet := { => }

    hb_HCaseMatch( hSet, .F. )
    hSet[ "ini" ] := ""
    hSet[ "harbour" ] := cb_H( { "harbour_path" => "", "harbour_bin_path" => "", ;
                                          "harbour_include_path" => "", "def_flags" => "-n -q0 -w", "libs" => "" }, .F. )
    hSet[ "guilib" ] := cb_H( { "id" => "hbtui", "path" => "", "inc_path" => "", "lib_path" => "", ;
                                         "libs" => "hbtui", "gt" => "" }, .F. )
    hSet[ "comps" ] := {}
    hSet[ "view" ] := cb_H( { "theme" => "", "compiler" => "" }, .F. )
    hSet[ "recent" ] := {}

RETURN hSet

/* fills in the empty settings: where Harbour, the C compiler and Harbour-CUI are */
STATIC PROCEDURE cb_SettingsDetect( hSet )

    LOCAL hHb := hSet[ "harbour" ], hGui := hSet[ "guilib" ], hComp, cRoot, cDir, cPlat, cComp, cExe

#ifdef __PLATFORM__WINDOWS
    cPlat := "win"
    cExe := ".exe"
#else
    cPlat := "linux"
    cExe := ""
#endif
    IF Len( hSet[ "comps" ] ) == 0
        AAdd( hSet[ "comps" ], cb_CompNew( cb_DefFamily(), cb_DefFamily() ) )
    ENDIF
    cComp := hSet[ "comps" ][ 1 ][ "family" ]

    /* Harbour: HB_ROOT / HB_PATH, harbour in the PATH, the usual folders */
    cRoot := hHb[ "harbour_path" ]
    IF Empty( cRoot )
        FOR EACH cDir IN { GetEnv( "HB_ROOT" ), GetEnv( "HB_PATH" ), cb_HbFromPath( cExe ), ;
                           "D:\tools\harbour-core", "C:\harbour", "C:\hb32", "/opt/harbour-core", "/opt/harbour", ;
                           GetEnv( "HOME" ) + "/git/core", "/usr/local", "/usr" }
            IF ! Empty( cDir ) .AND. ( hb_FileExists( cb_DirSep( cDir ) + "include" + hb_ps() + "hbapi.h" ) .OR. ;
                                       hb_FileExists( cb_DirSep( cDir ) + "include" + hb_ps() + "harbour" + hb_ps() + "hbapi.h" ) )
                cRoot := cDir
                EXIT
            ENDIF
        NEXT
        hHb[ "harbour_path" ] := cRoot
    ENDIF
    IF ! Empty( cRoot )
        cRoot := cb_DirSep( cRoot )
        IF Empty( hHb[ "harbour_bin_path" ] )
            FOR EACH cDir IN { cRoot + "bin" + hb_ps() + cPlat + hb_ps() + cComp, cRoot + "bin", ;
                               cRoot + "bin" + hb_ps() + cPlat + hb_ps() + "gcc" }
                IF hb_FileExists( cb_DirSep( cDir ) + "harbour" + cExe )
                    hHb[ "harbour_bin_path" ] := cDir
                    EXIT
                ENDIF
            NEXT
        ENDIF
        IF Empty( hHb[ "harbour_include_path" ] )
            hHb[ "harbour_include_path" ] := IIF( hb_FileExists( cRoot + "include" + hb_ps() + "hbapi.h" ), ;
                                                  cRoot + "include", cRoot + "include" + hb_ps() + "harbour" )
        ENDIF
    ENDIF
    IF Empty( hHb[ "libs" ] )
        hHb[ "libs" ] := cb_HbDefLibs()
    ENDIF

    FOR EACH hComp IN hSet[ "comps" ]
        cComp := hComp[ "family" ]
        IF Empty( hComp[ "harbour_lib_path" ] ) .AND. ! Empty( cRoot )
            FOR EACH cDir IN { cRoot + "lib" + hb_ps() + cPlat + hb_ps() + IIF( cComp == "gcc" .AND. cPlat == "win", "mingw", cComp ), ;
                               cRoot + "lib" + hb_ps() + "harbour", cRoot + "lib" }
                IF hb_DirExists( cDir ) .AND. ( hb_FileExists( cb_DirSep( cDir ) + cb_LibFile( cComp, "hbvm" ) ) .OR. ;
                                               hb_FileExists( cb_DirSep( cDir ) + "libhbvm.so" ) )
                    hComp[ "harbour_lib_path" ] := cDir
                    EXIT
                ENDIF
            NEXT
        ENDIF
#ifdef __PLATFORM__WINDOWS
        IF Empty( hComp[ "bin_path" ] ) .AND. cComp == "mingw" .AND. Empty( cb_InPath( "gcc.exe" ) )
            FOR EACH cDir IN { "C:\msys64\mingw32\bin", cRoot + "comp\mingw\bin", "C:\mingw32\bin", "C:\mingw\bin" }
                IF ! Empty( cDir ) .AND. hb_FileExists( cb_DirSep( cDir ) + "gcc.exe" )
                    hComp[ "bin_path" ] := cDir
                    EXIT
                ENDIF
            NEXT
        ENDIF
#endif
    NEXT

    /* Harbour-CUI: two folders up from the program (samples/cuibuilder), or the current one */
    IF Empty( hGui[ "path" ] )
        FOR EACH cDir IN { hb_DirBase() + ".." + hb_ps() + ".." + hb_ps(), hb_DirBase(), hb_cwd() }
            IF hb_FileExists( cDir + "include" + hb_ps() + "hbtui.ch" )
                hGui[ "path" ] := cb_DirNoSep( cb_PathNormal( cDir ) )
                EXIT
            ENDIF
        NEXT
    ENDIF
    IF ! Empty( hGui[ "path" ] )
        IF Empty( hGui[ "inc_path" ] )
            hGui[ "inc_path" ] := cb_DirSep( hGui[ "path" ] ) + "include"
        ENDIF
        IF Empty( hGui[ "lib_path" ] )
            hGui[ "lib_path" ] := cb_DirSep( hGui[ "path" ] ) + "lib" + hb_ps() + cPlat + hb_ps() + ;
                                  IIF( cPlat == "win", hSet[ "comps" ][ 1 ][ "family" ], "gcc" )
        ENDIF
    ENDIF
    IF Empty( hGui[ "gt" ] ) .AND. Lower( hGui[ "id" ] ) == "hbtui"
#ifdef __PLATFORM__WINDOWS
        hGui[ "gt" ] := "gtwvt"
#else
        hGui[ "gt" ] := "gtxwc"
#endif
    ENDIF

RETURN

/* the folder of harbour(.exe) in the PATH -> the Harbour root (bin/<plat>/<comp> or bin) */
STATIC FUNCTION cb_HbFromPath( cExe )

    LOCAL cDir := cb_InPath( "harbour" + cExe ), n

    IF Empty( cDir )
        RETURN ""
    ENDIF
    cDir := cb_DirNoSep( cDir )
    FOR n := 1 TO 3
        IF hb_FileExists( cb_DirSep( cDir ) + "include" + hb_ps() + "hbapi.h" )
            RETURN cDir
        ENDIF
        cDir := cb_DirNoSep( hb_FNameDir( cDir ) )
    NEXT

RETURN ""

/* the folder in the PATH that holds cFile, "" = none */
STATIC FUNCTION cb_InPath( cFile )

    LOCAL cDir

    FOR EACH cDir IN hb_ATokens( GetEnv( "PATH" ), hb_osPathListSeparator() )
        IF ! Empty( cDir ) .AND. hb_FileExists( cb_DirSep( cDir ) + cFile )
            RETURN cDir
        ENDIF
    NEXT

RETURN ""

FUNCTION cb_SettingsSave( hSet )

    LOCAL cOut := "", hComp, cKey, n, aKeys, cEol := hb_eol()

    cOut += "# CuiBuilder settings (the sections of HwBuilder's hwbuild.ini)" + cEol + cEol
    cOut += "[HARBOUR]" + cEol
    FOR EACH cKey IN { "harbour_path", "harbour_bin_path", "harbour_include_path", "def_flags", "libs" }
        cOut += cKey + "=" + hSet[ "harbour" ][ cKey ] + cEol
    NEXT
    cOut += cEol + "[GUILIB]" + cEol
    FOR EACH cKey IN { "id", "path", "inc_path", "lib_path", "libs", "gt" }
        cOut += cKey + "=" + hSet[ "guilib" ][ cKey ] + cEol
    NEXT
    FOR n := 1 TO Len( hSet[ "comps" ] )
        hComp := hSet[ "comps" ][ n ]
        cOut += cEol + "[C_COMPILER" + IIF( n == 1, "", "_" + hb_ntos( n ) ) + "]" + cEol
        FOR EACH cKey IN { "id", "family", "bin_path", "harbour_lib_path", "def_cflags", "def_linkflags", ;
                           "def_linkflagscons", "def_libflags", "def_syslibs", "cmd_comp", "cmd_res", "cmd_lib", "cmd_exe" }
            cOut += cKey + "=" + hComp[ cKey ] + cEol
        NEXT
        aKeys := hb_HKeys( hComp[ "env" ] )
        FOR EACH cKey IN aKeys
            cOut += "env_" + cKey + "=" + hComp[ "env" ][ cKey ] + cEol
        NEXT
    NEXT
    cOut += cEol + "[VIEW]" + cEol
    FOR EACH cKey IN hb_HKeys( hSet[ "view" ] )
        cOut += cKey + "=" + hSet[ "view" ][ cKey ] + cEol
    NEXT
    IF Len( hSet[ "recent" ] ) > 0
        cOut += cEol + "[RECENT]" + cEol
        FOR n := 1 TO Len( hSet[ "recent" ] )
            cOut += "f" + hb_ntos( n ) + "=" + hSet[ "recent" ][ n ] + cEol
        NEXT
    ENDIF

RETURN hb_MemoWrit( hSet[ "ini" ], cOut )

/* a project file at the top of the recent list (at most 12) */
PROCEDURE cb_AddRecent( hSet, cFile )

    LOCAL n

    IF ( n := AScan( hSet[ "recent" ], {| c | hb_FileMatch( c, cFile ) } ) ) > 0
        hb_ADel( hSet[ "recent" ], n, .T. )
    ENDIF
    hb_AIns( hSet[ "recent" ], 1, cFile, .T. )
    IF Len( hSet[ "recent" ] ) > 12
        ASize( hSet[ "recent" ], 12 )
    ENDIF

RETURN

/* the compiler entry: by id, else the one chosen in [VIEW] compiler=, else the first */
FUNCTION cb_Compiler( hSet, cId )

    LOCAL n := 0

    IF ! Empty( cId )
        n := AScan( hSet[ "comps" ], {| h | Lower( h[ "id" ] ) == Lower( cId ) } )
    ENDIF
    IF n == 0 .AND. ! Empty( hSet[ "view" ][ "compiler" ] )
        n := AScan( hSet[ "comps" ], {| h | Lower( h[ "id" ] ) == Lower( hSet[ "view" ][ "compiler" ] ) } )
    ENDIF

RETURN IIF( n == 0, IIF( Empty( cId ), hSet[ "comps" ][ 1 ], NIL ), hSet[ "comps" ][ n ] )

/* what is wrong with the settings, "" = nothing */
FUNCTION cb_SettingsCheck( hSet, hComp )

    LOCAL hHb := hSet[ "harbour" ], cExe := IIF( "windows" $ Lower( OS() ), ".exe", "" ), cTool

    IF ! hb_FileExists( cb_DirSep( hHb[ "harbour_bin_path" ] ) + "harbour" + cExe )
        RETURN "Harbour compiler not found: harbour_bin_path=" + hHb[ "harbour_bin_path" ]
    ENDIF
    IF ! hb_FileExists( cb_DirSep( hHb[ "harbour_include_path" ] ) + "hbapi.h" )
        RETURN "Harbour headers not found: harbour_include_path=" + hHb[ "harbour_include_path" ]
    ENDIF
    IF ! hb_DirExists( hComp[ "harbour_lib_path" ] )
        RETURN "Harbour libraries not found: harbour_lib_path=" + hComp[ "harbour_lib_path" ] + ;
               " ([C_COMPILER] " + hComp[ "id" ] + ")"
    ENDIF
    cTool := hb_ATokens( cb_Tmpl( hComp, "comp" ), " " )[ 1 ]
    cTool := StrTran( cTool, "{path}", "" )
    IF ! Empty( hComp[ "bin_path" ] )
        IF ! hb_FileExists( cb_DirSep( hComp[ "bin_path" ] ) + cTool )
            RETURN "C compiler not found: " + cb_DirSep( hComp[ "bin_path" ] ) + cTool
        ENDIF
    ELSEIF Empty( cb_InPath( cTool ) )
        RETURN "C compiler " + cTool + " is not in the PATH: set bin_path in [C_COMPILER] " + hComp[ "id" ]
    ENDIF

RETURN ""

/* a command template of the compiler: own (cmd_xxx) or the family's */
FUNCTION cb_Tmpl( hComp, cWhat )

    LOCAL c := ""

    IF hb_HHasKey( hComp, "cmd_" + cWhat )
        c := hComp[ "cmd_" + cWhat ]
    ENDIF

RETURN IIF( Empty( c ), cb_FamDef( hComp[ "family" ], cWhat ), c )

STATIC FUNCTION cb_LibFile( cFamily, cLib )
RETURN StrTran( cb_FamDef( cFamily, "libfile" ), "{l}", cLib )

/*=========================================================== projects */

CREATE CLASS CBProject

    VAR cName      INIT ""
    VAR cFile      INIT ""        /* the project file it came from */
    VAR cDir       INIT ""        /* relative paths start here */
    VAR aFiles     INIT {}        /* sources as written: .prg .c .cpp .rc, objects / libraries */
    VAR cSrcPath   INIT ""
    VAR cOutName   INIT ""
    VAR cOutPath   INIT ""
    VAR cObjPath   INIT ""
    VAR lLib       INIT .F.
    VAR lMake      INIT .T.
    VAR cComp      INIT ""        /* compiler id, "" = the default one */
    VAR lGuiLib    INIT .T.       /* link the GUI library of [GUILIB] (Harbour-CUI) */
    VAR cGt        INIT ""        /* "" = the GT of [GUILIB] (gtwvt / gtxwc) */
    VAR cGui       INIT ""        /* "yes" / "no", "" = by the GT */
    VAR cLibs      INIT ""
    VAR cLibsPath  INIT ""
    VAR cIncPath   INIT ""
    VAR cFlagsPrg  INIT ""
    VAR cFlagsC    INIT ""
    VAR cFlagsL    INIT ""
    VAR cDefFlagsC              /* NIL = the compiler's */
    VAR cDefFlagsL
    VAR cDefFlagsLib
    VAR cMain      INIT ""        /* hbp -main= */
    VAR aRequest   INIT {}        /* hbp -request= */
    VAR aKeep      INIT {}        /* .hwprj lines kept as they were ({filters}, unknown) */
    VAR aSubFiles  INIT {}        /* .hwprj / .hbp projects built first */
    VAR aWarn      INIT {}

    METHOD outFile( hSet )
    METHOD objDir( hSet )
    METHOD srcFile( cFile )
    METHOD isGui( hSet )
    METHOD gt( hSet )

ENDCLASS

/* the full name of what is built: the program or the library */
METHOD outFile( hSet ) CLASS CBProject

    LOCAL hComp := cb_Compiler( hSet, ::cComp ), cName := ::cOutName, cDir

    IF Empty( cName )
        cName := hb_FNameName( ::cFile )
    ENDIF
    cDir := IIF( Empty( ::cOutPath ), ::cDir, cb_Abs( ::cOutPath, ::cDir ) )
    IF hComp == NIL
        hComp := hSet[ "comps" ][ 1 ]
    ENDIF
    IF ::lLib
        IF hb_FNameExt( cName ) == ""
            cName := cb_LibFile( hComp[ "family" ], hb_FNameName( cName ) )
            cName := hb_FNameMerge( hb_FNameDir( ::cOutName ), cName )
        ENDIF
    ELSEIF hb_FNameExt( cName ) == ""
        cName += cb_FamDef( hComp[ "family" ], "exeext" )
    ENDIF

RETURN cb_Abs( cName, cDir )

/* where the objects go: objpath, else obj/<compiler id> next to the project */
METHOD objDir( hSet ) CLASS CBProject

    LOCAL hComp := cb_Compiler( hSet, ::cComp ), cDir

    IF ! Empty( ::cObjPath )
        cDir := cb_Abs( ::cObjPath, ::cDir )
    ELSE
        cDir := cb_DirSep( ::cDir ) + "obj" + hb_ps() + IIF( hComp == NIL, "c", hComp[ "id" ] )
        IF Len( ::cName ) > 0 .AND. ! ::cName == hb_FNameName( ::cFile )
            cDir += hb_ps() + ::cName
        ENDIF
    ENDIF

RETURN cb_DirSep( cDir )

METHOD srcFile( cFile ) CLASS CBProject

    IF cb_IsAbs( cFile )
        RETURN cb_PathNormal( cFile )
    ENDIF

RETURN cb_Abs( cFile, IIF( Empty( ::cSrcPath ), ::cDir, cb_Abs( ::cSrcPath, ::cDir ) ) )

METHOD gt( hSet ) CLASS CBProject

    IF ! Empty( ::cGt )
        RETURN Lower( ::cGt )
    ENDIF
    IF ::lGuiLib .AND. ! Empty( hSet[ "guilib" ][ "gt" ] )
        RETURN Lower( hSet[ "guilib" ][ "gt" ] )
    ENDIF

RETURN ""

/* a window program (-mwindows) or a console one */
METHOD isGui( hSet ) CLASS CBProject

    IF cb_IsYes( ::cGui )
        RETURN .T.
    ENDIF
    IF AScan( { "no", "off", "0", "false", ".f." }, Lower( AllTrim( ::cGui ) ) ) > 0
        RETURN .F.
    ENDIF

RETURN AScan( { "gtwvt", "gtgui", "gtwvg", "gtwvw", "gthwg", "gtxwc" }, ::gt( hSet ) ) > 0

/* a project file: .hwprj (one or more projects, :project) or .hbp */
CREATE CLASS CBProjFile

    VAR cFile      INIT ""
    VAR cType      INIT "hwprj"
    VAR aProjects  INIT {}
    VAR aHeader    INIT {}        /* the comments at the top (.hwprj) */
    VAR cError     INIT ""
    VAR aWarn      INIT {}

ENDCLASS

/* reads a project file; oPF:cError tells what went wrong.
   lEdit: a .hwprj to change and save - its {filter} lines and ${macros} stay
   as they are (kept in aKeep), else they are applied (to build) */
FUNCTION cb_ProjOpen( cFile, hSet, lEdit )

    LOCAL oPF := CBProjFile():new()

    oPF:cFile := cb_Abs( cFile, hb_cwd() )
    IF ! hb_FileExists( oPF:cFile )
        oPF:cError := "Project not found: " + oPF:cFile
        RETURN oPF
    ENDIF
    IF Lower( hb_FNameExt( oPF:cFile ) ) $ ".hbp|.hbm"
        oPF:cType := "hbp"
        cb_HbpRead( oPF, hSet )
    ELSE
        cb_HwprjRead( oPF, hSet, hb_defaultValue( lEdit, .F. ) )
    ENDIF

RETURN oPF

/* a new, empty .hwprj project */
FUNCTION cb_ProjNew( cFile )

    LOCAL oPF := CBProjFile():new(), oP := CBProject():new()

    oPF:cFile := cb_Abs( cFile, hb_cwd() )
    oP:cFile := oPF:cFile
    oP:cDir := hb_FNameDir( oPF:cFile )
    oP:cOutName := hb_FNameName( oPF:cFile )
    AAdd( oPF:aProjects, oP )

RETURN oPF

/*----------------------------------------------------------- .hwprj */

STATIC PROCEDURE cb_HwprjRead( oPF, hSet, lEdit )

    LOCAL aLines, cLine, oP, n, cKey, cVal, cRaw, cDir := hb_FNameDir( oPF:cFile ), lTop := .T., cF

    oP := cb_HwNewProj( oPF, "" )
    aLines := hb_ATokens( StrTran( hb_MemoRead( oPF:cFile ), Chr( 13 ), "" ), Chr( 10 ) )
    FOR EACH cLine IN aLines
        cRaw := cLine
        cLine := AllTrim( cLine )
        IF Empty( cLine )
            LOOP
        ENDIF
        IF Left( cLine, 1 ) == "#"
            IF lTop
                AAdd( oPF:aHeader, cRaw )
            ENDIF
            LOOP
        ENDIF
        lTop := .F.
        IF Left( cLine, 1 ) == "{" .AND. ( n := At( "}", cLine ) ) > 0
            IF lEdit
                AAdd( oP:aKeep, cLine )       /* written back as it was */
                LOOP
            ENDIF
            IF ! cb_Filter( SubStr( cLine, 2, n - 2 ), hSet, oP:cComp )
                LOOP
            ENDIF
            cLine := AllTrim( SubStr( cLine, n + 1 ) )
        ENDIF
        IF ! lEdit
            cLine := cb_Macros( cLine, hSet, oP:cComp )
        ENDIF
        IF Lower( Left( cLine, 8 ) ) == ":project"
            IF Len( oP:aFiles ) > 0 .OR. Len( oP:aSubFiles ) > 0 .OR. Len( oPF:aProjects ) > 1 .OR. ! Empty( oP:cOutName )
                oP := cb_HwNewProj( oPF, AllTrim( SubStr( cLine, 9 ) ) )
            ELSE
                oP:cName := AllTrim( SubStr( cLine, 9 ) )
            ENDIF
            LOOP
        ENDIF
        IF Left( cLine, 1 ) == "@"            /* a list of files */
            IF lEdit
                AAdd( oP:aKeep, cLine )
                LOOP
            ENDIF
            FOR EACH cF IN hb_ATokens( StrTran( hb_MemoRead( cb_Abs( SubStr( cLine, 2 ), cDir ) ), Chr( 13 ), "" ), Chr( 10 ) )
                IF ! Empty( cF ) .AND. ! Left( LTrim( cF ), 1 ) == "#"
                    AAdd( oP:aFiles, AllTrim( cF ) )
                ENDIF
            NEXT
            LOOP
        ENDIF
        n := At( "=", cLine )
        IF n > 1 .AND. ! " " $ RTrim( Left( cLine, n - 1 ) )
            cKey := Lower( AllTrim( Left( cLine, n - 1 ) ) )
            cVal := AllTrim( SubStr( cLine, n + 1 ) )
            SWITCH cKey
            CASE "srcpath"      ; oP:cSrcPath := cVal ; EXIT
            CASE "outname"      ; oP:cOutName := cVal ; EXIT
            CASE "outpath"      ; oP:cOutPath := cVal ; EXIT
            CASE "objpath"      ; oP:cObjPath := cVal ; EXIT
            CASE "target"       ; oP:lLib := ( Lower( cVal ) == "lib" ) ; EXIT
            CASE "makemode"     ; oP:lMake := cb_IsYes( cVal ) ; EXIT
            CASE "c_compiler"
                oP:cComp := cVal
                IF ! Empty( cVal ) .AND. cb_Compiler( hSet, cVal ) == NIL
                    AAdd( oPF:aWarn, "Unknown compiler id in " + oPF:cFile + ": " + cVal )
                ENDIF
                EXIT
            CASE "guilib"       ; oP:lGuiLib := ! Empty( cVal ) .AND. ! Lower( cVal ) $ "no|off" ; EXIT
            CASE "gtlib"        ; oP:cGt := cVal ; EXIT
            CASE "gui"          ; oP:cGui := cVal ; EXIT
            CASE "libs"         ; oP:cLibs := cb_Join( oP:cLibs, cVal ) ; EXIT
            CASE "libspath"     ; oP:cLibsPath := cb_Join( oP:cLibsPath, cVal ) ; EXIT
            CASE "incpath"      ; oP:cIncPath := cb_Join( oP:cIncPath, cVal ) ; EXIT
            CASE "prgflags"     ; oP:cFlagsPrg := cb_Join( oP:cFlagsPrg, cVal ) ; EXIT
            CASE "cflags"       ; oP:cFlagsC := cb_Join( oP:cFlagsC, cVal ) ; EXIT
            CASE "lflags"       ; oP:cFlagsL := cb_Join( oP:cFlagsL, cVal ) ; EXIT
            CASE "def_cflags"   ; oP:cDefFlagsC := cVal ; EXIT
            CASE "def_lflags"   ; oP:cDefFlagsL := cVal ; EXIT
            CASE "def_libflags" ; oP:cDefFlagsLib := cVal ; EXIT
            OTHERWISE
                AAdd( oPF:aWarn, "Unknown option in " + hb_FNameNameExt( oPF:cFile ) + ": " + cLine )
            ENDSWITCH
            LOOP
        ENDIF
        IF Lower( hb_FNameExt( cLine ) ) $ ".hwprj|.hbp"
            AAdd( oP:aSubFiles, cLine )
        ELSE
            AAdd( oP:aFiles, cLine )
        ENDIF
    NEXT

RETURN

STATIC FUNCTION cb_HwNewProj( oPF, cName )

    LOCAL oP := CBProject():new()

    oP:cName := cName
    oP:cFile := oPF:cFile
    oP:cDir := hb_FNameDir( oPF:cFile )
    AAdd( oPF:aProjects, oP )

RETURN oP

/* writes a .hwprj; the comments at the top and the {filter} lines stay */
FUNCTION cb_HwprjSave( oPF, cFile )

    LOCAL cOut := "", oP, c, cEol := hb_eol(), lFirst := .T.

    cFile := hb_defaultValue( cFile, oPF:cFile )
    FOR EACH c IN oPF:aHeader
        cOut += c + cEol
    NEXT
    IF Len( oPF:aHeader ) > 0
        cOut += cEol
    ENDIF
    FOR EACH oP IN oPF:aProjects
        IF ! lFirst .OR. ! Empty( oP:cName )
            cOut += IIF( lFirst, "", cEol ) + ":project" + IIF( Empty( oP:cName ), "", " " + oP:cName ) + cEol
        ENDIF
        lFirst := .F.
        cOut += cb_Opt( "srcpath", oP:cSrcPath )
        cOut += cb_Opt( "outname", oP:cOutName )
        cOut += cb_Opt( "outpath", oP:cOutPath )
        cOut += cb_Opt( "objpath", oP:cObjPath )
        cOut += IIF( oP:lLib, "target=lib" + cEol, "" )
        cOut += IIF( oP:lMake, "", "makemode=off" + cEol )
        cOut += cb_Opt( "c_compiler", oP:cComp )
        cOut += IIF( oP:lGuiLib, "", "guilib=" + cEol )
        cOut += cb_Opt( "gtlib", oP:cGt )
        cOut += cb_Opt( "gui", oP:cGui )
        cOut += cb_Opt( "libs", oP:cLibs )
        cOut += cb_Opt( "libspath", oP:cLibsPath )
        cOut += cb_Opt( "incpath", oP:cIncPath )
        cOut += cb_Opt( "prgflags", oP:cFlagsPrg )
        cOut += cb_Opt( "cflags", oP:cFlagsC )
        cOut += cb_Opt( "lflags", oP:cFlagsL )
        cOut += IIF( oP:cDefFlagsC == NIL, "", "def_cflags=" + oP:cDefFlagsC + cEol )
        cOut += IIF( oP:cDefFlagsL == NIL, "", "def_lflags=" + oP:cDefFlagsL + cEol )
        cOut += IIF( oP:cDefFlagsLib == NIL, "", "def_libflags=" + oP:cDefFlagsLib + cEol )
        FOR EACH c IN oP:aKeep
            cOut += c + cEol
        NEXT
        FOR EACH c IN oP:aSubFiles
            cOut += c + cEol
        NEXT
        FOR EACH c IN oP:aFiles
            cOut += c + cEol
        NEXT
    NEXT

RETURN hb_MemoWrit( cFile, cOut )

STATIC FUNCTION cb_Opt( cKey, cVal )
RETURN IIF( Empty( cVal ), "", cKey + "=" + cVal + hb_eol() )

/*----------------------------------------------------------- .hbp / .hbm / .hbc */

STATIC PROCEDURE cb_HbpRead( oPF, hSet )

    LOCAL oP := CBProject():new()

    oP:cFile := oPF:cFile
    oP:cDir := hb_FNameDir( oPF:cFile )
    oP:lGuiLib := .F.                   /* an .hbp names hbtui.hbc itself */
    oP:lMake := .T.
    AAdd( oPF:aProjects, oP )
    cb_HbmFile( oPF, oP, oPF:cFile, hSet, 0 )
    IF Empty( oP:cOutName )
        oP:cOutName := hb_FNameName( oPF:cFile )
    ENDIF

RETURN

/* the words of an .hbp/.hbm, with the paths relative to its folder */
STATIC PROCEDURE cb_HbmFile( oPF, oP, cFile, hSet, nDepth )

    LOCAL cLine, cDir := hb_FNameDir( cFile ), aWords, cW, n

    IF nDepth > 8
        RETURN
    ENDIF
    FOR EACH cLine IN hb_ATokens( StrTran( hb_MemoRead( cFile ), Chr( 13 ), "" ), Chr( 10 ) )
        cLine := AllTrim( cLine )
        IF Empty( cLine ) .OR. Left( cLine, 1 ) == "#"
            LOOP
        ENDIF
        aWords := cb_Words( cLine )
        FOR EACH cW IN aWords
            IF Left( cW, 1 ) == "#"
                EXIT
            ENDIF
            IF Left( cW, 1 ) == "{" .AND. ( n := At( "}", cW ) ) > 0
                IF ! cb_Filter( SubStr( cW, 2, n - 2 ), hSet, oP:cComp )
                    LOOP
                ENDIF
                cW := SubStr( cW, n + 1 )
            ENDIF
            IF Empty( cW ) .OR. Left( cW, 1 ) == "$"     /* $hb_pkg_dynlib.hbm: hbmk2's own */
                LOOP
            ENDIF
            cW := cb_Macros( cW, hSet, oP:cComp )
            cb_HbpWord( oPF, oP, cW, cDir, hSet, nDepth )
        NEXT
    NEXT

RETURN

STATIC PROCEDURE cb_HbpWord( oPF, oP, cW, cDir, hSet, nDepth )

    LOCAL cExt, cL, cF, aDir, c2

    IF Left( cW, 1 ) == "-"
        cL := Lower( cW )
        DO CASE
        CASE Left( cL, 2 ) == "-o" .AND. ! Left( cL, 4 ) == "-opt"
            oP:cOutName := cb_Abs( SubStr( cW, 3 ), cDir )
            IF Empty( hb_FNameExt( oP:cOutName ) ) .AND. Right( SubStr( cW, 3 ), 1 ) $ "/\"
                oP:cOutPath := oP:cOutName
                oP:cOutName := ""
            ENDIF
        CASE cL == "-hblib" .OR. cL == "-hblibs"
            oP:lLib := .T.
        CASE cL == "-hbexe"
            oP:lLib := .F.
        CASE cL == "-inc"
            oP:lMake := .T.
        CASE cL == "-inc-" .OR. cL == "-rebuild"
            oP:lMake := .F.
        CASE cL == "-gui"
            oP:cGui := "yes"
        CASE cL == "-std" .OR. cL == "-cui"
            oP:cGui := "no"
        CASE AScan( { "-mt", "-st", "-shared", "-static", "-fullstatic", "-strip", "-strip-", "-trace", "-trace-", ;
                      "-quiet", "-quiet-", "-info", "-head=off", "-head=full", "-head=native", "-head=dep", "-cpp", ;
                      "-cpp-", "-debug", "-debug-", "-nulrdd", "-nulrdd-", "-run", "-run-", "-ignore", "-hbcmp", ;
                      "-hbcc", "-clean", "-optim", "-optim-", "-map", "-map-", "-warn=yes", "-warn=no", "-warn=max", ;
                      "-warn=low", "-warn=def", "-nohblib", "-xhb", "-hb10", "-hb20", "-hb30", "-hb32", "-pause", ;
                      "-beep", "-ldflag", "-hbx-" }, cL ) > 0 .OR. Left( cL, 5 ) == "-hbx=" .OR. Left( cL, 7 ) == "-instfi"
            /* nothing to do, or the default anyway */
        CASE Left( cL, 3 ) == "-gt"
            oP:cGt := SubStr( cL, 2 )
        CASE Left( cW, 2 ) == "-L"
            oP:cLibsPath := cb_Join( oP:cLibsPath, cb_Quote( cb_Abs( SubStr( cW, 3 ), cDir ) ) )
        CASE Left( cW, 2 ) == "-l" .AND. Len( cW ) > 2 .AND. ! cW == "-l-" .AND. ! Left( cL, 7 ) == "-ldflag"
            oP:cLibs := cb_Join( oP:cLibs, SubStr( cW, 3 ) )
        CASE Left( cL, 9 ) == "-incpath="
            oP:cIncPath := cb_Join( oP:cIncPath, cb_Quote( cb_Abs( SubStr( cW, 10 ), cDir ) ) )
        CASE Left( cL, 2 ) == "-i" .AND. ! cL == "-inc"
            oP:cIncPath := cb_Join( oP:cIncPath, cb_Quote( cb_Abs( SubStr( cW, 3 ), cDir ) ) )
        CASE Left( cL, 9 ) == "-prgflag="
            oP:cFlagsPrg := cb_Join( oP:cFlagsPrg, SubStr( cW, 10 ) )
        CASE Left( cL, 7 ) == "-cflag="
            oP:cFlagsC := cb_Join( oP:cFlagsC, SubStr( cW, 8 ) )
        CASE Left( cL, 8 ) == "-ldflag="
            oP:cFlagsL := cb_Join( oP:cFlagsL, SubStr( cW, 9 ) )
        CASE Left( cL, 9 ) == "-workdir="
            oP:cObjPath := cb_Abs( SubStr( cW, 10 ), cDir )
        CASE Left( cL, 6 ) == "-main="
            oP:cMain := SubStr( cW, 7 )
        CASE Left( cL, 9 ) == "-request="
            AAdd( oP:aRequest, Upper( SubStr( cW, 10 ) ) )
        CASE cL == "-b" .OR. cL == "-l-" .OR. ;
             AScan( { "-w", "-n", "-d", "-u", "-a", "-v", "-z", "-m", "-q", "-k", "-j", "-p", "-r" }, Left( cL, 2 ) ) > 0 .OR. ;
             AScan( { "-es", "-gc", "-gh" }, Left( cL, 3 ) ) > 0
            oP:cFlagsPrg := cb_Join( oP:cFlagsPrg, cW )
        OTHERWISE
            AAdd( oPF:aWarn, "Option not supported, skipped: " + cW )
        ENDCASE
        RETURN
    ENDIF

    IF Left( cW, 1 ) == "@"
        cF := cb_Abs( SubStr( cW, 2 ), cDir )
        IF Empty( hb_FNameExt( cF ) )
            cF += ".hbm"
        ENDIF
        cb_HbmFile( oPF, oP, cF, hSet, nDepth + 1 )
        RETURN
    ENDIF

    cExt := Lower( hb_FNameExt( cW ) )
    DO CASE
    CASE cExt == ".hbm"
        cb_HbmFile( oPF, oP, cb_Abs( cW, cDir ), hSet, nDepth + 1 )
    CASE cExt == ".hbc"
        cF := cb_HbcFind( cW, cDir, hSet )
        IF Empty( cF )
            AAdd( oPF:aWarn, "Not found: " + cW )
        ELSE
            cb_HbcRead( oPF, oP, cF, hSet, nDepth + 1 )
        ENDIF
    CASE cExt == ".hbp"
        AAdd( oP:aSubFiles, cb_Abs( cW, cDir ) )
    CASE cExt == ".ch" .OR. cExt == ".h"
        /* headers: nothing to build */
    CASE "*" $ cW .OR. "?" $ cW
        cF := cb_Abs( cW, cDir )
        aDir := Directory( cF )
        ASort( aDir,,, {| a, b | a[ 1 ] < b[ 1 ] } )
        FOR EACH c2 IN aDir
            AAdd( oP:aFiles, hb_FNameDir( cF ) + c2[ 1 ] )
        NEXT
    OTHERWISE
        IF Empty( cExt )
            cW += ".prg"
        ENDIF
        AAdd( oP:aFiles, cb_Abs( cW, cDir ) )
    ENDCASE

RETURN

/* where an .hbc is: next to the file that names it, then Harbour's contrib / addons */
STATIC FUNCTION cb_HbcFind( cName, cDir, hSet )

    LOCAL cRoot := hSet[ "harbour" ][ "harbour_path" ], cN := hb_FNameName( cName ), cF

    IF Empty( hb_FNameExt( cName ) )
        cName += ".hbc"
    ENDIF
    cF := cb_Abs( cName, cDir )
    IF hb_FileExists( cF )
        RETURN cF
    ENDIF
    IF ! Empty( cRoot )
        cRoot := cb_DirSep( cRoot )
        FOR EACH cF IN { cRoot + "contrib" + hb_ps() + cN + hb_ps() + cN + ".hbc", ;
                         cRoot + "addons" + hb_ps() + cN + hb_ps() + cN + ".hbc", ;
                         cRoot + "extras" + hb_ps() + cN + hb_ps() + cN + ".hbc", ;
                         cRoot + "contrib" + hb_ps() + cName, cRoot + "addons" + hb_ps() + cName }
            IF hb_FileExists( cF )
                RETURN cF
            ENDIF
        NEXT
    ENDIF

RETURN ""

STATIC PROCEDURE cb_HbcRead( oPF, oP, cFile, hSet, nDepth )

    LOCAL cLine, cDir := hb_FNameDir( cFile ), n, cKey, cVal, cW, cF

    IF nDepth > 8
        RETURN
    ENDIF
    FOR EACH cLine IN hb_ATokens( StrTran( hb_MemoRead( cFile ), Chr( 13 ), "" ), Chr( 10 ) )
        cLine := AllTrim( cLine )
        IF Empty( cLine ) .OR. Left( cLine, 1 ) == "#"
            LOOP
        ENDIF
        IF Left( cLine, 1 ) == "{" .AND. ( n := At( "}", cLine ) ) > 0
            IF ! cb_Filter( SubStr( cLine, 2, n - 2 ), hSet, oP:cComp )
                LOOP
            ENDIF
            cLine := AllTrim( SubStr( cLine, n + 1 ) )
        ENDIF
        IF ( n := At( "=", cLine ) ) == 0
            LOOP
        ENDIF
        cKey := Lower( AllTrim( Left( cLine, n - 1 ) ) )
        cVal := cb_Macros( AllTrim( SubStr( cLine, n + 1 ) ), hSet, oP:cComp, hb_FNameName( cFile ) )
        FOR EACH cW IN cb_Words( cVal )
            SWITCH cKey
            CASE "libs"
                IF Lower( hb_FNameExt( cW ) ) == ".hbc"
                    cF := cb_HbcFind( cW, cDir, hSet )
                    IF ! Empty( cF )
                        cb_HbcRead( oPF, oP, cF, hSet, nDepth + 1 )
                    ENDIF
                ELSE
                    oP:cLibs := cb_Join( oP:cLibs, cW )
                ENDIF
                EXIT
            CASE "hbcs"
                cF := cb_HbcFind( cW, cDir, hSet )
                IF Empty( cF )
                    AAdd( oPF:aWarn, "Not found: " + cW + " (" + hb_FNameNameExt( cFile ) + ")" )
                ELSE
                    cb_HbcRead( oPF, oP, cF, hSet, nDepth + 1 )
                ENDIF
                EXIT
            CASE "libpaths"
                oP:cLibsPath := cb_Join( oP:cLibsPath, cb_Quote( cb_Abs( cW, cDir ) ) )
                EXIT
            CASE "incpaths"
                oP:cIncPath := cb_Join( oP:cIncPath, cb_Quote( cb_Abs( cW, cDir ) ) )
                EXIT
            CASE "prgflags"
                oP:cFlagsPrg := cb_Join( oP:cFlagsPrg, cW )
                EXIT
            CASE "cflags"
                oP:cFlagsC := cb_Join( oP:cFlagsC, cW )
                EXIT
            CASE "ldflags"
                oP:cFlagsL := cb_Join( oP:cFlagsL, cW )
                EXIT
            CASE "gt"
                IF Empty( oP:cGt )
                    oP:cGt := Lower( cW )
                ENDIF
                EXIT
            CASE "sources"
                AAdd( oP:aFiles, cb_Abs( cW, cDir ) )
                EXIT
            ENDSWITCH
        NEXT
    NEXT

RETURN

/* the words of a line; "a b" in quotes stays one word (the quotes go) */
STATIC FUNCTION cb_Words( cLine )

    LOCAL aW := {}, cW := "", lQ := .F., c

    FOR EACH c IN cLine
        DO CASE
        CASE c == '"'
            lQ := ! lQ
        CASE ( c == " " .OR. c == Chr( 9 ) ) .AND. ! lQ
            IF ! Empty( cW )
                AAdd( aW, cW )
            ENDIF
            cW := ""
        OTHERWISE
            cW += c
        ENDCASE
    NEXT
    IF ! Empty( cW )
        AAdd( aW, cW )
    ENDIF

RETURN aW

/* {win}, {unix}, {mingw}, {!linux}, {win&mingw}, {HBTUI_DEBUG} (an environment variable) */
STATIC FUNCTION cb_Filter( cExpr, hSet, cCompId )

    LOCAL cOr, cAnd, lAnd, lNot, c, hComp := cb_Compiler( hSet, cCompId ), aKnown

    IF hComp == NIL
        hComp := hSet[ "comps" ][ 1 ]
    ENDIF
#ifdef __PLATFORM__WINDOWS
    aKnown := { "win", "allwin", "windows", "x86" }
#else
    aKnown := { "unix", "linux", "allgcc", "x86_64" }
#endif
    AAdd( aKnown, Lower( hComp[ "family" ] ) )
    AAdd( aKnown, Lower( hComp[ "id" ] ) )
    IF hComp[ "family" ] $ "mingw|gcc|clang"
        AAdd( aKnown, "allgcc" )
    ENDIF
    FOR EACH cOr IN hb_ATokens( cExpr, "|" )
        lAnd := .T.
        FOR EACH cAnd IN hb_ATokens( cOr, "&" )
            c := AllTrim( cAnd )
            lNot := Left( c, 1 ) == "!"
            IF lNot
                c := SubStr( c, 2 )
            ENDIF
            IF ( AScan( aKnown, Lower( c ) ) > 0 .OR. ( ! Empty( GetEnv( c ) ) .AND. ! Lower( GetEnv( c ) ) $ "no|0" ) ) == lNot
                lAnd := .F.
                EXIT
            ENDIF
        NEXT
        IF lAnd
            RETURN .T.
        ENDIF
    NEXT

RETURN .F.

/* ${hb_plat} ${hb_comp} ${hb_root} ${ENV}, $(ENV) */
STATIC FUNCTION cb_Macros( cLine, hSet, cCompId, cName0 )

    LOCAL n, n2, cName, cVal, hComp := cb_Compiler( hSet, cCompId ), cOpen, cClose

    IF hComp == NIL
        hComp := hSet[ "comps" ][ 1 ]
    ENDIF
    FOR EACH cOpen IN { "${", "$(" }
        cClose := IIF( cOpen == "${", "}", ")" )
        DO WHILE ( n := At( cOpen, cLine ) ) > 0
            n2 := hb_At( cClose, cLine, n + 2 )
            IF n2 == 0
                EXIT
            ENDIF
            cName := SubStr( cLine, n + 2, n2 - n - 2 )
            SWITCH Lower( cName )
#ifdef __PLATFORM__WINDOWS
            CASE "hb_plat" ; cVal := "win" ; EXIT
#else
            CASE "hb_plat" ; cVal := "linux" ; EXIT
#endif
            CASE "hb_comp" ; cVal := hComp[ "family" ] ; EXIT
            CASE "hb_root"
            CASE "hb_dir"
                cVal := cb_DirNoSep( hSet[ "harbour" ][ "harbour_path" ] )
                EXIT
            CASE "hb_name" ; cVal := hb_defaultValue( cName0, "harbour" ) ; EXIT
            OTHERWISE
                cVal := GetEnv( cName )
            ENDSWITCH
            cLine := Left( cLine, n - 1 ) + cVal + SubStr( cLine, n2 + 1 )
        ENDDO
    NEXT

RETURN cLine

/*=========================================================== build */

/* builds the projects of a project file (the sub-projects first).
   bOut( cText, nType, cFile, nLine ): CB_MSG_xxx; bStop() -> .T. stops */
FUNCTION cb_Build( oPF, hSet, lClean, bOut, bStop )

    LOCAL oP, cSub, oSub, lOk := .T., aDone := {}

    s_aDepCache := {}
    FOR EACH cSub IN cb_AllSubs( oPF )
        IF AScan( aDone, {| c | hb_FileMatch( c, cSub ) } ) > 0
            LOOP
        ENDIF
        AAdd( aDone, cSub )
        Eval( bOut, "=== " + cSub, CB_MSG_TITLE )
        oSub := cb_ProjOpen( cSub, hSet )
        IF ! Empty( oSub:cError )
            Eval( bOut, oSub:cError, CB_MSG_ERROR )
            RETURN .F.
        ENDIF
        FOR EACH oP IN oSub:aProjects
            IF ! cb_BuildOne( oP, hSet, lClean, bOut, bStop )
                RETURN .F.
            ENDIF
        NEXT
    NEXT
    FOR EACH oP IN oPF:aProjects
        IF Len( oPF:aProjects ) > 1
            Eval( bOut, "=== " + IIF( Empty( oP:cName ), hb_FNameName( oP:outFile( hSet ) ), oP:cName ), CB_MSG_TITLE )
        ENDIF
        IF ! cb_BuildOne( oP, hSet, lClean, bOut, bStop )
            lOk := .F.
            EXIT
        ENDIF
    NEXT
    s_aDepCache := NIL

RETURN lOk

/* the sub-project files (.hwprj / .hbp lines), the deepest first */
STATIC FUNCTION cb_AllSubs( oPF )

    LOCAL aList := {}, oP, c

    FOR EACH oP IN oPF:aProjects
        FOR EACH c IN oP:aSubFiles
            AAdd( aList, cb_Abs( c, oP:cDir ) )
        NEXT
    NEXT

RETURN aList

/* deletes the objects and the output */
FUNCTION cb_Clean( oPF, hSet, bOut )

    LOCAL oP, cDir, aF, cOut, n := 0

    FOR EACH oP IN oPF:aProjects
        cDir := oP:objDir( hSet )
        FOR EACH aF IN Directory( cDir + "*.*" )
            IF Lower( hb_FNameExt( aF[ 1 ] ) ) $ ".c|.o|.obj|.res|.ppo"
                IF FErase( cDir + aF[ 1 ] ) == 0
                    n++
                ENDIF
            ENDIF
        NEXT
        cOut := oP:outFile( hSet )
        IF hb_FileExists( cOut ) .AND. FErase( cOut ) == 0
            n++
        ENDIF
    NEXT
    Eval( bOut, "Cleaned: " + hb_ntos( n ) + " files deleted", CB_MSG_INFO )

RETURN n

STATIC FUNCTION cb_BuildOne( oP, hSet, lClean, bOut, bStop )

    LOCAL hComp := cb_Compiler( hSet, oP:cComp ), hHb := hSet[ "harbour" ], hGui := hSet[ "guilib" ]
    LOCAL cErr, cObjDir, cFile, cSrc, cExt, cObj, cC, aObjs := {}, aRes := {}, aLibFiles := {}
    LOCAL lRelink := .F., cOut, tOut, cCmd, cIncs, cHbIncs, cFlags, cLibs, cLibPaths, cGt, cStub
    LOCAL nComp := 0, nSkip := 0, nRes, cP, cFam, cHbBin, aHw := {}, cRes, cName, l, cLibF, aIncDirs

    IF hComp == NIL
        Eval( bOut, "Unknown compiler: " + oP:cComp + " (cuibuild.ini [C_COMPILER] id=)", CB_MSG_ERROR )
        RETURN .F.
    ENDIF
    cErr := cb_SettingsCheck( hSet, hComp )
    IF ! Empty( cErr )
        Eval( bOut, cErr, CB_MSG_ERROR )
        Eval( bOut, "Settings: " + hSet[ "ini" ], CB_MSG_INFO )
        RETURN .F.
    ENDIF
    IF Len( oP:aFiles ) == 0
        Eval( bOut, "No source files in the project", CB_MSG_ERROR )
        RETURN .F.
    ENDIF
    cFam := hComp[ "family" ]
    cb_SetEnv( hComp )

    cObjDir := oP:objDir( hSet )
    IF ! hb_DirExists( cObjDir ) .AND. hb_DirBuild( cObjDir ) == .F.
        Eval( bOut, "Cannot create the folder " + cObjDir, CB_MSG_ERROR )
        RETURN .F.
    ENDIF
    cOut := oP:outFile( hSet )
    IF ! hb_DirExists( hb_FNameDir( cOut ) )
        hb_DirBuild( hb_FNameDir( cOut ) )
    ENDIF
    IF lClean .OR. ! oP:lMake
        lRelink := .T.
    ENDIF

    /* include folders: the project's, the GUI library's, Harbour's */
    aIncDirs := {}
    FOR EACH cP IN cb_Words( oP:cIncPath )
        AAdd( aIncDirs, cb_Abs( cP, oP:cDir ) )
    NEXT
    IF oP:lGuiLib .AND. ! Empty( hGui[ "inc_path" ] )
        AAdd( aIncDirs, cb_PathNormal( hGui[ "inc_path" ] ) )
    ENDIF
    cHbIncs := ""
    cIncs := ""
    FOR EACH cP IN aIncDirs
        cHbIncs += " -i" + cb_Quote( cb_DirNoSep( cP ) )
        cIncs += " " + cb_FamDef( cFam, "inc" ) + cb_Quote( cb_DirNoSep( cP ) )
    NEXT
    cHbIncs += " -i" + cb_Quote( cb_DirNoSep( hHb[ "harbour_include_path" ] ) )
    cIncs += " " + cb_FamDef( cFam, "inc" ) + cb_Quote( cb_DirNoSep( hHb[ "harbour_include_path" ] ) )
    cHbBin := cb_DirSep( hHb[ "harbour_bin_path" ] ) + "harbour" + IIF( "windows" $ Lower( OS() ), ".exe", "" )
    cFlags := cb_Join( IIF( oP:cDefFlagsC == NIL, hComp[ "def_cflags" ], oP:cDefFlagsC ), oP:cFlagsC )

    /* the sources */
    FOR EACH cFile IN oP:aFiles
        IF bStop != NIL .AND. Eval( bStop )
            Eval( bOut, "Stopped", CB_MSG_ERROR )
            RETURN .F.
        ENDIF
        cSrc := oP:srcFile( cFile )
        cExt := Lower( hb_FNameExt( cSrc ) )
        cName := hb_FNameName( cSrc )
        DO CASE
        CASE cExt $ ".o|.obj"
            AAdd( aObjs, cSrc )
            LOOP
        CASE cExt $ ".a|.lib"
            AAdd( aLibFiles, cSrc )
            LOOP
        CASE cExt == ".res"
            AAdd( aRes, cSrc )
            LOOP
        ENDCASE
        IF ! hb_FileExists( cSrc )
            Eval( bOut, "File not found: " + cSrc, CB_MSG_ERROR, cSrc, 0 )
            RETURN .F.
        ENDIF
        cName := cb_ObjName( aHw, cName, cExt )
        IF cExt == ".rc"
            IF oP:lLib .OR. Empty( cb_Tmpl( hComp, "res" ) )
                LOOP                            /* no resources in a library / on Linux */
            ENDIF
            cRes := cObjDir + cName + IIF( cFam == "mingw", "_rc", "" ) + cb_FamDef( cFam, "resobj" )
            IF ! lClean .AND. oP:lMake .AND. cb_UpToDate( cRes, cSrc, aIncDirs )
                nSkip++
            ELSE
                cCmd := cb_Fill( cb_Tmpl( hComp, "res" ), hComp, hSet, ;
                    { "src" => cb_Quote( cSrc ), "out" => cb_Quote( cRes ), "incs" => cIncs, "f" => "" } )
                IF ! cb_Step( cCmd, cRes, bOut, "Resources: " + hb_FNameNameExt( cSrc ) )
                    RETURN .F.
                ENDIF
                lRelink := .T.
            ENDIF
            AAdd( aRes, cRes )
            LOOP
        ENDIF
        IF ! cExt $ ".prg|.c|.cpp|.cc|.cxx"
            Eval( bOut, "Wrong source file extension: " + cSrc, CB_MSG_ERROR, cSrc, 0 )
            RETURN .F.
        ENDIF
        cObj := cObjDir + cName + cb_FamDef( cFam, "obj" )
        IF ! lClean .AND. oP:lMake .AND. cb_UpToDate( cObj, cSrc, aIncDirs )
            nSkip++
            AAdd( aObjs, cObj )
            LOOP
        ENDIF
        IF cExt == ".prg"
            cC := cObjDir + cName + ".c"
            FErase( cC )
            cCmd := cb_Quote( cHbBin ) + " " + cb_Quote( cSrc ) + " " + hHb[ "def_flags" ] + ;
                    IIF( Empty( oP:cFlagsPrg ), "", " " + oP:cFlagsPrg ) + cHbIncs + " -o" + cb_Quote( cObjDir + cName + ".c" )
            IF ! cb_Step( cCmd, cC, bOut, "Harbour: " + hb_FNameNameExt( cSrc ) )
                RETURN .F.
            ENDIF
        ELSE
            cC := cSrc
        ENDIF
        cCmd := cb_Fill( cb_Tmpl( hComp, "comp" ), hComp, hSet, ;
            { "src" => cb_Quote( cC ), "obj" => cb_Quote( cObj ), "incs" => cIncs, "f" => cFlags } )
        FErase( cObj )
        IF ! cb_Step( cCmd, cObj, bOut, "C: " + hb_FNameNameExt( cC ) )
            RETURN .F.
        ENDIF
        nComp++
        lRelink := .T.
        AAdd( aObjs, cObj )
    NEXT

    /* the GT and the main function of a program: a C stub, as hbmk2 makes */
    cGt := oP:gt( hSet )
    IF Empty( cGt )
        cGt := IIF( "windows" $ Lower( OS() ), "gtwin", "gttrm" )      /* Harbour's default */
    ENDIF
    IF ! oP:lLib .AND. ( ! Empty( cGt ) .OR. ! Empty( oP:cMain ) .OR. Len( oP:aRequest ) > 0 )
        cStub := cObjDir + "_cbstub.c"
        cC := cb_StubText( cGt, oP:cMain, oP:aRequest )
        cObj := cObjDir + "_cbstub" + cb_FamDef( cFam, "obj" )
        IF ! hb_MemoRead( cStub ) == cC .OR. ! hb_FileExists( cObj ) .OR. lClean
            hb_MemoWrit( cStub, cC )
            cCmd := cb_Fill( cb_Tmpl( hComp, "comp" ), hComp, hSet, ;
                { "src" => cb_Quote( cStub ), "obj" => cb_Quote( cObj ), "incs" => cIncs, "f" => cFlags } )
            FErase( cObj )
            IF ! cb_Step( cCmd, cObj, bOut, "C: _cbstub.c (" + IIF( Empty( cGt ), "main", cGt ) + ")" )
                RETURN .F.
            ENDIF
            lRelink := .T.
        ENDIF
        AAdd( aObjs, cObj )
    ENDIF

    /* the libraries: the project's, the GUI library's, Harbour's */
    cLibs := ""
    cLibPaths := ""
    FOR EACH cP IN cb_Words( oP:cLibsPath )
        cLibPaths += " " + cb_FamDef( cFam, "libpath" ) + cb_Quote( cb_DirNoSep( cb_Abs( cP, oP:cDir ) ) )
    NEXT
    IF ! oP:lLib
        FOR EACH l IN cb_Words( oP:cLibs )
            cLibs += " " + cb_LibArg( cFam, l, oP:cDir )
        NEXT
        IF oP:lGuiLib .AND. ! Empty( hGui[ "libs" ] )
            IF ! Empty( hGui[ "lib_path" ] )
                cLibPaths += " " + cb_FamDef( cFam, "libpath" ) + cb_Quote( cb_DirNoSep( hGui[ "lib_path" ] ) )
            ENDIF
            FOR EACH l IN cb_Words( hGui[ "libs" ] )
                cLibs += " " + cb_LibArg( cFam, l, oP:cDir )
                cLibF := cb_DirSep( hGui[ "lib_path" ] ) + cb_LibFile( cFam, l )
                IF ! hb_FileExists( cLibF )
                    Eval( bOut, "The library " + l + " is not built yet: " + cLibF, CB_MSG_ERROR )
                    RETURN .F.
                ENDIF
                AAdd( aLibFiles, cLibF )
            NEXT
        ENDIF
        FOR EACH l IN cb_Words( hHb[ "libs" ] )
            cLibs += " " + cb_LibArg( cFam, l, oP:cDir )
        NEXT
        IF cFam $ "mingw|msvc"
            cLibs += " " + cb_LibArg( cFam, IIF( oP:isGui( hSet ), "hbmainwin", "hbmainstd" ), oP:cDir )
        ENDIF
        cLibs += " " + hComp[ "def_syslibs" ]
        cLibPaths += " " + cb_FamDef( cFam, "libpath" ) + cb_Quote( cb_DirNoSep( hComp[ "harbour_lib_path" ] ) )
        /* lib/win/mingw -> lib/3rd/win/mingw (pcre, zlib of Harbour) */
        cP := cb_DirNoSep( hComp[ "harbour_lib_path" ] )
        cP := cb_DirSep( cP ) + ".." + hb_ps() + ".." + hb_ps() + "3rd" + hb_ps() + ;
              hb_FNameName( cb_DirNoSep( hb_FNameDir( cP ) ) ) + hb_ps() + hb_FNameName( cP )
        IF hb_DirExists( cP )
            cLibPaths += " " + cb_FamDef( cFam, "libpath" ) + cb_Quote( cb_PathNormal( cP ) )
        ENDIF
        FOR EACH cP IN cb_Words( oP:cLibsPath )
            FOR EACH l IN cb_Words( oP:cLibs )
                cLibF := cb_DirSep( cb_Abs( cP, oP:cDir ) ) + cb_LibFile( cFam, l )
                IF hb_FileExists( cLibF )
                    AAdd( aLibFiles, cLibF )
                ENDIF
            NEXT
        NEXT
    ENDIF

    /* link, when something is newer than the output */
    IF ! lRelink
        IF hb_FGetDateTime( cOut, @tOut )
            FOR EACH cP IN { aObjs, aRes, aLibFiles }
                FOR EACH cFile IN cP
                    IF cb_FTime( cFile ) > tOut
                        lRelink := .T.
                        EXIT
                    ENDIF
                NEXT
            NEXT
        ELSE
            lRelink := .T.
        ENDIF
    ENDIF
    IF ! lRelink
        Eval( bOut, hb_FNameNameExt( cOut ) + " is up to date (" + hb_ntos( nSkip ) + " files unchanged)", CB_MSG_OK )
        RETURN .T.
    ENDIF
    FErase( cOut )
    IF oP:lLib
        cCmd := cb_Fill( cb_Tmpl( hComp, "lib" ), hComp, hSet, ;
            { "out" => cb_Quote( cOut ), "objs" => cb_ObjList( aObjs, cFam ), ;
              "f" => IIF( oP:cDefFlagsLib == NIL, hComp[ "def_libflags" ], oP:cDefFlagsLib ) } )
        nRes := 0
    ELSE
        cCmd := cb_Fill( cb_Tmpl( hComp, "exe" ), hComp, hSet, ;
            { "out" => cb_Quote( cOut ), "objs" => cb_ObjList( aObjs, cFam ), "res" => cb_ObjList( aRes, "" ), ;
              "libs" => AllTrim( cLibs ), "libpaths" => AllTrim( cLibPaths ), ;
              "f" => cb_Join( IIF( oP:cDefFlagsL == NIL, IIF( oP:isGui( hSet ), hComp[ "def_linkflags" ], hComp[ "def_linkflagscons" ] ), ;
                                   oP:cDefFlagsL ), oP:cFlagsL ) } )
        nRes := Len( aRes )
    ENDIF
    IF ! cb_Step( cCmd, cOut, bOut, IIF( oP:lLib, "Library: ", "Link: " ) + hb_FNameNameExt( cOut ) )
        RETURN .F.
    ENDIF
    Eval( bOut, hb_FNameNameExt( cOut ) + " created successfully (" + hb_ntos( nComp ) + " compiled, " + ;
          hb_ntos( nSkip ) + " unchanged" + IIF( nRes > 0, ", " + hb_ntos( nRes ) + " resources", "" ) + ")", CB_MSG_OK, cOut, 0 )

RETURN .T.

/* an object name that is not taken yet (a.prg and a.c both give a.o) */
STATIC FUNCTION cb_ObjName( aHw, cName, cExt )

    LOCAL cN := Lower( cName ), n := 1

    IF cExt == ".rc"
        RETURN cName
    ENDIF
    DO WHILE AScan( aHw, cN ) > 0
        cN := Lower( cName ) + "_" + hb_ntos( ++n )
    ENDDO
    AAdd( aHw, cN )

RETURN IIF( n == 1, cName, cName + "_" + hb_ntos( n ) )

/* -lname / name.lib; a library given as a file stays a file */
STATIC FUNCTION cb_LibArg( cFam, cLib, cDir )

    IF Lower( hb_FNameExt( cLib ) ) $ ".a|.lib|.o|.obj" .OR. hb_ps() $ cLib .OR. "/" $ cLib
        RETURN cb_Quote( cb_Abs( cLib, cDir ) )
    ENDIF
    IF cFam $ "mingw|gcc|clang" .AND. Left( cLib, 3 ) == "lib" .AND. hb_FileExists( cb_Abs( cLib + ".a", cDir ) )
        RETURN cb_Quote( cb_Abs( cLib + ".a", cDir ) )
    ENDIF

RETURN StrTran( cb_FamDef( cFam, "libtmpl" ), "{l}", cLib )

STATIC FUNCTION cb_ObjList( aObjs, cFam )

    LOCAL c := "", cF, cPref := IIF( Empty( cFam ), "", cb_FamDef( cFam, "objpref" ) )

    FOR EACH cF IN aObjs
        c += IIF( Empty( c ), "", " " ) + cPref + cb_Quote( cF )
    NEXT

RETURN c

/* fills a template: {path} {f} {src} {obj} {out} {objs} {res} {libs} {libpaths} {incs},
   HwBuilder's {hi} {gi} {hL} {gL} {dL} too */
STATIC FUNCTION cb_Fill( cTmpl, hComp, hSet, hVal )

    LOCAL cKey, c := cTmpl

    hVal[ "path" ] := IIF( Empty( hComp[ "bin_path" ] ), "", cb_DirSep( hComp[ "bin_path" ] ) )
    hVal[ "hi" ] := cb_DirNoSep( hSet[ "harbour" ][ "harbour_include_path" ] )
    hVal[ "gi" ] := cb_DirNoSep( hSet[ "guilib" ][ "inc_path" ] )
    hVal[ "hL" ] := cb_DirNoSep( hComp[ "harbour_lib_path" ] )
    hVal[ "gL" ] := cb_DirNoSep( hSet[ "guilib" ][ "lib_path" ] )
    hVal[ "dL" ] := ""
    FOR EACH cKey IN hb_HKeys( hVal )
        c := StrTran( c, "{" + cKey + "}", hVal[ cKey ] )
    NEXT
    FOR EACH cKey IN { "{f}", "{src}", "{obj}", "{out}", "{objs}", "{res}", "{libs}", "{libpaths}", "{incs}" }
        c := StrTran( c, cKey, "" )
    NEXT
    IF Left( c, 1 ) != '"' .AND. " " $ hVal[ "path" ] .AND. Left( c, Len( hVal[ "path" ] ) ) == hVal[ "path" ]
        cb_QuoteProg( @c, Len( hVal[ "path" ] ) )
    ENDIF

RETURN AllTrim( StrTran( c, "  ", " " ) )

/* a bin folder with spaces: the program at the start of the command in quotes */
STATIC PROCEDURE cb_QuoteProg( c, nPath )

    LOCAL n := hb_At( " ", c, nPath + 1 )

    IF n == 0
        n := Len( c ) + 1
    ENDIF
    c := '"' + Left( c, n - 1 ) + '"' + SubStr( c, n )

RETURN

STATIC FUNCTION cb_StubText( cGt, cMain, aRequest )

    LOCAL c := "", cEol := Chr( 10 ), aFunc := {}, cF

    IF ! Empty( cGt )
        AAdd( aFunc, "HB_GT_" + Upper( SubStr( cGt, 3 ) ) )
    ENDIF
    AEval( aRequest, {| x | AAdd( aFunc, x ) } )
    c += "/* Made by CuiBuilder: the default GT and the main function. It can be deleted. */" + cEol
    c += '#include "hbapi.h"' + cEol + cEol
    FOR EACH cF IN aFunc
        c += "HB_FUNC_EXTERN( " + cF + " );" + cEol
    NEXT
    IF Len( aFunc ) > 0
        c += cEol + "void _hb_lnk_ForceLink_cuibuilder( void )" + cEol + "{" + cEol
        FOR EACH cF IN aFunc
            c += "   HB_FUNC_EXEC( " + cF + " );" + cEol
        NEXT
        c += "}" + cEol + cEol
    ENDIF
    c += '#include "hbinit.h"' + cEol + cEol
    c += "HB_CALL_ON_STARTUP_BEGIN( _hb_cuibuilder_setdef_ )" + cEol
    IF ! Empty( cGt )
        c += '   hb_vmSetDefaultGT( "' + Upper( SubStr( cGt, 3 ) ) + '" );' + cEol
    ENDIF
    IF ! Empty( cMain )
        c += '   hb_vmSetLinkedMain( "' + Upper( cMain ) + '" );' + cEol
    ENDIF
    c += "HB_CALL_ON_STARTUP_END( _hb_cuibuilder_setdef_ )" + cEol + cEol
    c += "#if defined( HB_PRAGMA_STARTUP )" + cEol
    c += "   #pragma startup _hb_cuibuilder_setdef_" + cEol
    c += "#elif defined( HB_DATASEG_STARTUP )" + cEol
    c += "   #define HB_DATASEG_BODY    HB_DATASEG_FUNC( _hb_cuibuilder_setdef_ )" + cEol
    c += '   #include "hbiniseg.h"' + cEol
    c += "#endif" + cEol

RETURN c

/* the environment of the compiler: env_xxx of cuibuild.ini, bin_path in front of the PATH */
STATIC PROCEDURE cb_SetEnv( hComp )

    LOCAL cKey, cPath

    FOR EACH cKey IN hb_HKeys( hComp[ "env" ] )
        hb_SetEnv( Upper( cKey ), hComp[ "env" ][ cKey ] )
    NEXT
    IF ! Empty( hComp[ "bin_path" ] )
        cPath := GetEnv( "PATH" )
        IF ! Lower( cb_DirNoSep( hComp[ "bin_path" ] ) ) + hb_osPathListSeparator() $ Lower( cPath + hb_osPathListSeparator() )
            hb_SetEnv( "PATH", cb_DirNoSep( hComp[ "bin_path" ] ) + hb_osPathListSeparator() + cPath )
        ENDIF
    ENDIF

RETURN

/* runs one command; .T. when it ended well and made cResult */
STATIC FUNCTION cb_Step( cCmd, cResult, bOut, cTitle )

    LOCAL cOut := "", nExit, cLine, a, lErr := .F.

    Eval( bOut, cTitle, CB_MSG_STEP )
    Eval( bOut, cCmd, CB_MSG_CMD )
    nExit := cb_Run( cCmd, @cOut )
    IF nExit == -1
        Eval( bOut, "Cannot start: " + cCmd, CB_MSG_ERROR )
        RETURN .F.
    ENDIF
    FOR EACH cLine IN hb_ATokens( StrTran( cOut, Chr( 13 ), "" ), Chr( 10 ) )
        IF Empty( cLine )
            LOOP
        ENDIF
        a := cb_ParseMsg( cLine )
        IF a[ 1 ] == CB_MSG_ERROR
            lErr := .T.
        ENDIF
        Eval( bOut, cLine, a[ 1 ], a[ 2 ], a[ 3 ] )
    NEXT
    IF nExit != 0 .OR. ( cResult != NIL .AND. ! hb_FileExists( cResult ) )
        IF ! lErr
            Eval( bOut, "Error: exit code " + hb_ntos( nExit ) + IIF( cResult != NIL .AND. ! hb_FileExists( cResult ), ;
                  ", not made: " + cResult, "" ), CB_MSG_ERROR )
        ENDIF
        RETURN .F.
    ENDIF

RETURN .T.

/* runs a command and returns its exit code (-1 = not started); stdout + stderr in cOut */
FUNCTION cb_Run( cCmd, cOut )

    LOCAL nExit
#ifdef __PLATFORM__WINDOWS
    nExit := CB_RUNHIDDEN( cCmd, @cOut )
#else
    LOCAL cErr := ""
    nExit := hb_processRun( cCmd,, @cOut, @cErr )
    cOut += cErr
#endif
    IF ! hb_StrIsUTF8( cOut )
        cOut := cb_AsciiOnly( cOut )
    ENDIF

RETURN nExit

/* the kind of a compiler message, the file and the line: { nType, cFile, nLine } */
FUNCTION cb_ParseMsg( cLine )

    LOCAL a, cL := Lower( cLine )

    /* Harbour:  file.prg(12) Error E0030  Syntax error ... */
    a := hb_regex( "^(.+?)\(([0-9]+)\)\s+(Error|Warning)\s+[A-Z][0-9]+", cLine )
    IF Len( a ) == 4
        RETURN { IIF( a[ 4 ] == "Error", CB_MSG_ERROR, CB_MSG_WARN ), a[ 2 ], Val( a[ 3 ] ) }
    ENDIF
    /* gcc / clang:  file.c:12:5: error: ... */
    a := hb_regex( "^(.+?):([0-9]+):(?:[0-9]+:)?\s*(fatal error|error|warning|note):", cLine )
    IF Len( a ) == 4
        RETURN { IIF( a[ 4 ] == "warning", CB_MSG_WARN, IIF( a[ 4 ] == "note", CB_MSG_OUT, CB_MSG_ERROR ) ), a[ 2 ], Val( a[ 3 ] ) }
    ENDIF
    /* msvc:  file.c(12) : error C2065: ... */
    a := hb_regex( "^(.+?)\(([0-9]+)\)\s*:\s*(fatal error|error|warning)", cLine )
    IF Len( a ) == 4
        RETURN { IIF( a[ 4 ] == "warning", CB_MSG_WARN, CB_MSG_ERROR ), a[ 2 ], Val( a[ 3 ] ) }
    ENDIF
    /* bcc:  Error E2451 file.c 12: ... */
    a := hb_regex( "^(Error|Warning) [A-Z][0-9]+ (.+?) ([0-9]+):", cLine )
    IF Len( a ) == 4
        RETURN { IIF( a[ 2 ] == "Error", CB_MSG_ERROR, CB_MSG_WARN ), a[ 3 ], Val( a[ 4 ] ) }
    ENDIF
    /* a source line of gcc ("   12 | x = 1") stays plain output */
    IF "undefined reference" $ cL .OR. "error:" $ cL .OR. "cannot find" $ cL .OR. "unresolved external" $ cL .OR. ;
       Left( cL, 6 ) == "error " .OR. "fatal" $ cL
        RETURN { CB_MSG_ERROR, "", 0 }
    ENDIF
    IF "warning" $ cL
        RETURN { CB_MSG_WARN, "", 0 }
    ENDIF

RETURN { CB_MSG_OUT, "", 0 }

/* .T. when cObj is newer than the source and the headers it includes */
STATIC FUNCTION cb_UpToDate( cObj, cSrc, aIncDirs )

    LOCAL tObj

    IF ! hb_FGetDateTime( cObj, @tObj )
        RETURN .F.
    ENDIF

RETURN tObj >= cb_DepTime( cSrc, aIncDirs, 0 )

/* the newest time of a file and of the local headers it includes (#include "x"),
   not those of Harbour */
STATIC FUNCTION cb_DepTime( cFile, aIncDirs, nDepth )

    LOCAL tMax := cb_FTime( cFile ), n, cText, aInc, cInc, cF, cDir

    IF s_aDepCache != NIL .AND. ( n := AScan( s_aDepCache, {| a | a[ 1 ] == cFile } ) ) > 0
        RETURN s_aDepCache[ n ][ 2 ]
    ENDIF
    IF nDepth < 6
        cText := hb_MemoRead( cFile )
        aInc := hb_regexAll( '#\s*include\s+"([^"]+)"', cText,,,,, .T. )
        FOR EACH cInc IN aInc
            cF := ""
            FOR EACH cDir IN hb_AIns( AClone( aIncDirs ), 1, hb_FNameDir( cFile ), .T. )
                IF hb_FileExists( cb_DirSep( cDir ) + cInc[ 2 ] )
                    cF := cb_DirSep( cDir ) + cInc[ 2 ]
                    EXIT
                ENDIF
            NEXT
            IF ! Empty( cF )
                tMax := Max( tMax, cb_DepTime( cF, aIncDirs, nDepth + 1 ) )
            ENDIF
        NEXT
    ENDIF
    IF s_aDepCache != NIL
        AAdd( s_aDepCache, { cFile, tMax } )
    ENDIF

RETURN tMax

STATIC FUNCTION cb_FTime( cFile )

    LOCAL t

    IF ! hb_FGetDateTime( cFile, @t )
        RETURN hb_SToT( "" )
    ENDIF

RETURN t

/*=========================================================== paths */

FUNCTION cb_DirSep( cDir )

    IF Empty( cDir )
        RETURN ""
    ENDIF
    IF ! Right( cDir, 1 ) $ "/\"
        cDir += hb_ps()
    ENDIF

RETURN cDir

FUNCTION cb_DirNoSep( cDir )

    DO WHILE Len( cDir ) > 1 .AND. Right( cDir, 1 ) $ "/\" .AND. ! Right( cDir, 2 ) == ":\"
        cDir := Left( cDir, Len( cDir ) - 1 )
    ENDDO

RETURN cDir

FUNCTION cb_IsAbs( cFile )
#ifdef __PLATFORM__WINDOWS
RETURN SubStr( cFile, 2, 1 ) == ":" .OR. Left( cFile, 1 ) $ "/\"
#else
RETURN Left( cFile, 1 ) == "/" .OR. Left( cFile, 1 ) == "~"
#endif

/* a path relative to cDir made absolute, with the separators of the system */
FUNCTION cb_Abs( cFile, cDir )

    cFile := AllTrim( cFile )
    IF Left( cFile, 1 ) == '"' .AND. Right( cFile, 1 ) == '"' .AND. Len( cFile ) > 1
        cFile := SubStr( cFile, 2, Len( cFile ) - 2 )
    ENDIF
    IF Empty( cFile )
        RETURN cb_PathNormal( cDir )
    ENDIF
    IF ! cb_IsAbs( cFile )
        cFile := cb_DirSep( cDir ) + cFile
    ENDIF

RETURN cb_PathNormal( cFile )

/* a/b/../c -> a/c, the separators of the system */
FUNCTION cb_PathNormal( cPath )

    LOCAL aP, aOut := {}, c, cPref := "", lEnd

    cPath := StrTran( StrTran( cPath, "/", hb_ps() ), "\", hb_ps() )
    lEnd := Right( cPath, 1 ) == hb_ps()
    IF SubStr( cPath, 2, 1 ) == ":"
        cPref := Left( cPath, 2 )
        cPath := SubStr( cPath, 3 )
    ENDIF
    IF Left( cPath, 1 ) == hb_ps()
        cPref += hb_ps()
    ENDIF
    aP := hb_ATokens( cPath, hb_ps() )
    FOR EACH c IN aP
        DO CASE
        CASE c == "" .OR. c == "."
        CASE c == ".." .AND. Len( aOut ) > 0 .AND. ! ATail( aOut ) == ".."
            ASize( aOut, Len( aOut ) - 1 )
        OTHERWISE
            AAdd( aOut, c )
        ENDCASE
    NEXT
    cPath := ""
    FOR EACH c IN aOut
        cPath += IIF( c:__enumIndex() == 1, "", hb_ps() ) + c
    NEXT

RETURN cPref + cPath + IIF( lEnd .AND. Len( aOut ) > 0, hb_ps(), "" )

/* a path relative to cDir when it lies under it */
FUNCTION cb_Rel( cFile, cDir )

    cDir := cb_DirSep( cb_PathNormal( cDir ) )
    IF hb_FileMatch( Left( cFile, Len( cDir ) ), cDir )
        RETURN SubStr( cFile, Len( cDir ) + 1 )
    ENDIF

RETURN cFile

FUNCTION cb_Quote( c )
RETURN IIF( " " $ c .AND. ! Left( c, 1 ) == '"', '"' + c + '"', c )

STATIC FUNCTION cb_Join( c1, c2 )
RETURN IIF( Empty( c1 ), AllTrim( c2 ), IIF( Empty( c2 ), c1, c1 + " " + AllTrim( c2 ) ) )

/* bytes that are not ASCII as "?" (console output in an OEM codepage) */
STATIC FUNCTION cb_AsciiOnly( cText )

    LOCAL cOut := "", n, nB

    FOR n := 1 TO hb_BLen( cText )
        nB := hb_BCode( hb_BSubStr( cText, n, 1 ) )
        cOut += IIF( nB > 127, "?", hb_BChar( nB ) )
    NEXT

RETURN cOut

/* a hash that ignores the case of its keys */
STATIC FUNCTION cb_H( h, lCase )

    hb_HCaseMatch( h, lCase )

RETURN h

STATIC FUNCTION cb_IsYes( c )
RETURN Lower( AllTrim( c ) ) $ "yes|on|1|true|.t." .AND. ! Empty( c )
