/** @file unicode.prg
 * Unicode demo - typing and showing Chinese, Japanese and Korean text,
 * and Indic scripts (Gujarati, Hindi)
 *
 * Demonstrates:
 *   - the UTF8EX codepage (HTApplication:new() selects it when the
 *     application keeps Harbour's default "EN")
 *   - IME input (Windows: Win+Space or the language bar) in HTGet,
 *     HTLineEdit, HTTextEdit and the HTBrowse inline editor
 *   - wide characters: each CJK character takes two screen cells and stays
 *     aligned with Latin, Cyrillic and Greek text
 *   - Indic scripts on Windows: a syllable (consonant + vowel signs,
 *     conjuncts) is shaped and drawn over one cell per code point
 *   - Unicode window buttons: minimize, maximize/restore, close, resize grip
 *
 * Keyboard:
 *   Tab/Shift+Tab  - next/previous field
 *   F2 or typing   - edit a browse cell, Enter saves, Esc cancels
 *   Ctrl+F6        - next window
 *   ESC            - quit (main window)
 */

#include "hbtui.ch"
#include "inkey.ch"

STATIC s_oStatus
STATIC s_cName
STATIC s_cCity
STATIC s_cLang

PROCEDURE Main()

    LOCAL app, main, win, lbl, edt, txt, oGet, layout
    LOCAL aLines, cLine, nRow

    /* 3 + 17 characters once UTF8EX is active (HTApplication():new()) */
    s_cName := "王小明" + Space( 17 )
    s_cCity := "臺北市" + Space( 17 )

    app := HTApplication():new()
    /* a maximized window instead of fullscreen: the taskbar (IME mode
       indicator) stays visible; Alt+Enter switches to fullscreen */
    ht_fullScreen( .F. )
    ht_cuiSetupDisplay( app:rows(), app:cols() )

    main := HTMainWindow():new()
    main:setWindowTitle( "Unicode 統一碼 - 中文輸入 / 中文输入 / 日本語 / 한국어" )
    main:show()
    main:onKey( K_ESC, {|| HTApplication():quit() } )

    aLines := { ;
        "繁體中文  歡迎使用 HBTui，請用輸入法輸入中文。", ;
        "简体中文  欢迎使用 HBTui，请用输入法输入中文。", ;
        "日本語    HBTui へようこそ。かな・カナ・漢字。", ;
        "한국어    HBTui에 오신 것을 환영합니다.", ;
        "ગુજરાતી   HBTui માં આપનું સ્વાગત છે. કૃપા કરીને લખો.", ;
        "हिन्दी    HBTui में आपका स्वागत है।", ;
        "Српски    Добродошли · Ελληνικά Καλώς ήρθατε · Ünïcödé" }
    nRow := 2
    FOR EACH cLine IN aLines
        lbl := HTLabel():new( cLine, main )
        lbl:move( 2, nRow++ )
    NEXT

    s_oStatus := HTStatusBar():new( main )
    s_oStatus:setGeometry( 0, Max( 0, main:height - 2 ), main:width, 1 )
    /* the input language / IME mode, as the taskbar shows it */
    s_cLang := ht_inputLanguage()
    s_oStatus:addSection( " " + s_cLang + " " )
    s_oStatus:addSection( " Win+Space = input method   Tab = next field   F2 = edit cell   Ctrl+F6 = next window   ESC = quit" )
    HTEventLoop():scheduleRepeat( {|| ShowInputLang() }, 300 )
    main:repaint()

    /* --- Window 1: form ------------------------------------------------- */
    win := HTWidget():new()
    win:shadow := HT_SHADOW_ATTR
    win:move( 4, 11 )   /* x = col, y = row */
    win:resize( 50, 17 )
    win:setWindowTitle( "表單 Form" )

    oGet := HTGet():new( win )
    oGet:setup( {|x| IIF( x == NIL, s_cName, s_cName := x ) }, ;
        "姓名 Name:", "", NIL, NIL, "Name - type with an input method", NIL, .F., .F., 13 + 24 )
    oGet:setLabelWidth( 13 )
    oGet:move( 2, 2 )

    oGet := HTGet():new( win )
    oGet:setup( {|x| IIF( x == NIL, s_cCity, s_cCity := x ) }, ;
        "城市 City:", "", NIL, NIL, "City", NIL, .F., .F., 13 + 24 )
    oGet:setLabelWidth( 13 )
    oGet:move( 2, 4 )

    lbl := HTLabel():new( "備註 Notes (HTLineEdit / HTTextEdit):", win )
    lbl:move( 2, 6 )

    edt := HTLineEdit():new( win )
    edt:move( 2, 7 )
    edt:resize( 44, 1 )
    edt:text := "單行輸入 single line - 中文 English mixed"

    txt := HTTextEdit():new( win )
    txt:move( 2, 9 )
    txt:resize( 44, 6 )
    txt:text := "多行編輯器 multi-line editor" + hb_eol() + ;
                "春眠不覺曉，處處聞啼鳥。" + hb_eol() + ;
                "夜來風雨聲，花落知多少。" + hb_eol() + ;
                "ગુજરાતી લખાણ / हिन्दी पाठ" + hb_eol() + ;
                "Shift+Arrows = select, Ctrl+C/V = copy/paste"
    win:show()

    /* --- Window 2: browse ----------------------------------------------- */
    win := HTWidget():new()
    win:shadow := HT_SHADOW_ATTR
    win:move( 58, 11 )
    win:resize( 52, 16 )
    win:setWindowTitle( "城市 Cities" )

    layout := HTVBoxLayout():new( win )
    layout:setContentsMargins( 0, 1, 1, 1 )   /* top, left, bottom, right */
    layout:addWidget( BuildBrowse( win, ;
        { { "臺北 Taipei",     "臺灣 Taiwan",    2602418 }, ;
          { "高雄 Kaohsiung",  "臺灣 Taiwan",    2731412 }, ;
          { "香港 Hong Kong",  "中國 China",     7413070 }, ;
          { "北京 Beijing",    "中国 China",    21893095 }, ;
          { "上海 Shanghai",   "中国 China",    24870895 }, ;
          { "東京 Tokyo",      "日本 Japan",    14047594 }, ;
          { "大阪 Osaka",      "日本 Japan",     2752412 }, ;
          { "서울 Seoul",      "한국 Korea",     9586195 }, ;
          { "Београд",         "Србија Serbia",  1681405 }, ;
          { "Αθήνα Athens",    "Ελλάδα Greece",   643452 }, ;
          { "અમદાવાદ",         "ભારત India",     5570585 }, ;
          { "नई दिल्ली",         "भारत India",       249998 } } ) )
    win:show()

    app:exec()

RETURN

/** HTBrowse over aData { { cCity, cCountry, nPopulation }, ... } */
STATIC FUNCTION BuildBrowse( win, aData )

    LOCAL aRec := { 1 }   /* current row, shared by the browse blocks */
    LOCAL brw

    brw := HTBrowse():new( win )
    brw:setGoTopBlock( {|| aRec[ 1 ] := 1 } )
    brw:setGoBottomBlock( {|| aRec[ 1 ] := Len( aData ) } )
    brw:setSkipBlock( {|n| ArraySkip( aRec, Len( aData ), n ) } )

    brw:addColumn( "城市 City",    ColumnBlock( aData, aRec, 1 ), 16 )
    brw:addColumn( "國家 Country", ColumnBlock( aData, aRec, 2 ), 16 )
    brw:addColumn( "人口",         ColumnBlock( aData, aRec, 3 ), 11, "99,999,999" )

    brw:setHeadSep( e"\xC4" )   /* drawn through the CP437 box codepage */
    brw:setColSep( e"\xB3" )

    brw:editable := .T.
    brw:onEndEdit := {| nRow, nCol, xOld, xNew | HB_SYMBOL_UNUSED( nRow ), HB_SYMBOL_UNUSED( nCol ), ;
        ShowStatus( AllTrim( hb_CStr( xOld ) ) + " -> " + AllTrim( hb_CStr( xNew ) ) ), .T. }
    brw:onActivated := {|| ShowStatus( aData[ aRec[ 1 ], 1 ] + " / " + aData[ aRec[ 1 ], 2 ] ) }

RETURN brw

/** Read/write column block: returns the cell, or stores x into it. */
STATIC FUNCTION ColumnBlock( aData, aRec, nCol )
RETURN {| x | IIF( x == NIL, aData[ aRec[ 1 ], nCol ], aData[ aRec[ 1 ], nCol ] := x ) }

/** TBrowse skip over an array; aRec[ 1 ] is the current row. */
STATIC FUNCTION ArraySkip( aRec, nLen, nRequest )

    LOCAL nActual

    nActual := IIF( nRequest > 0, ;
        Min( nRequest, nLen - aRec[ 1 ] ), ;
        Max( nRequest, 1 - aRec[ 1 ] ) )
    aRec[ 1 ] += nActual

RETURN nActual

/** Shows a message in the main-window status bar. */
STATIC PROCEDURE ShowStatus( cText )

    IF s_oStatus != NIL
        s_oStatus:setSection( 2, " " + cText )
    ENDIF

RETURN

/** Section 1 of the status bar follows the input language (Win+Space). */
STATIC PROCEDURE ShowInputLang()

    LOCAL cLang := ht_inputLanguage()

    IF ! cLang == s_cLang
        s_cLang := cLang
        s_oStatus:setSection( 1, " " + cLang + " " )
    ENDIF

RETURN
