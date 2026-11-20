/** @class HTDatePicker
 * A date field with a calendar, after the serbian-date-picker of Digi knjigovodstvo
 * (digi-angular):
 *
 *   - the field takes dd.mm.yyyy; typing digits puts the dots in (20112026 ->
 *     20.11.2026); Enter / leaving the field checks the date: a wrong one
 *     (31.02.2026) goes back to the last good date (or empty);
 *   - the calendar: Alt-Down / F4 or a click on the symbol on the right; the week
 *     starts on Monday, today in [ ], the month and the year with the arrows of the
 *     header, "Danas" (today) below; a click on a day takes it and moves on;
 *   - in the calendar (for the keyboard, a CUI addition): the arrows move the day,
 *     PgUp / PgDn the month, Ctrl-PgUp / Ctrl-PgDn the year, Home / End the first /
 *     last day, Enter takes the day and moves on, T today, Esc closes only the calendar.
 *
 *   getValue() -> the date (empty: CToD( "" )); setValue( dDate / text in the format /
 *   "yyyy-mm-dd" ); onChanged {|dDate| }; onEnter {|| }.
 *
 *   Languages: setLang( "en" ) - the names of the months and the days, "Today", the
 *   format (dd.mm.yyyy, dd/mm/yyyy, mm/dd/yyyy, yyyy-mm-dd) and the first day of the
 *   week. Built in (ht_dateLangs()): sr (Cyrillic, as in Digi - the default), sr-latn,
 *   hr, bs, sl, mk, en, en-us, de, fr, it, es, ru, zh, ja, ko. ht_dateDefaultLang( "de" )
 *   sets the language of the pickers made after it (the whole program). Each part can
 *   be set alone too: monthNames, dayNames (Monday first), todayText, dateFormat,
 *   weekStart (1 = Monday, 7 = Sunday).
 *
 * @extends HTWidget
 */

#include "hbtui.ch"
#include "inkey.ch"

#define _DP_W   30      /* the calendar: 7 days of 4 cells and the frame */
#define _DP_H   11      /* frame, header, day names, 6 weeks, today, frame */

CLASS HTDatePicker FROM HTWidget

PROTECTED:

    DATA Fdate       INIT CToD( "" )     /* the value */
    DATA Ftext       INIT ""             /* the text of the field */
    DATA FnCursor    INIT 1
    DATA FlOpen      INIT .F.
    DATA Fcur        INIT CToD( "" )     /* the day marked in the calendar */
    DATA FwinId      INIT NIL
    DATA FlInLoop    INIT .F.
    DATA Flang       INIT ""

    METHOD commit()
    METHOD calendarKey( nKey )
    METHOD topWindow()
    METHOD moveOn()
    METHOD repaintSelf()
    METHOD openWindow()
    METHOD closeWindow()
    METHOD paintCalendar()
    METHOD calendarLoop()
    METHOD calendarClick( nRow, nCol )

PUBLIC:

    CONSTRUCTOR new( ... )

    METHOD paintEvent( paintEvent )
    METHOD keyEvent( keyEvent )
    METHOD mouseEvent( eventMouse )
    METHOD focusOutEvent( eventFocus )
    METHOD cursorPos()
    METHOD destroy()

    METHOD handleKey( nKey )
    METHOD getValue()       INLINE ::Fdate
    METHOD setValue( xDate )
    METHOD text()           INLINE ::Ftext
    METHOD isOpen()         INLINE ::FlOpen
    METHOD marked()         INLINE ::Fcur
    METHOD openCalendar()
    METHOD closeCalendar()
    METHOD selectDate( dDate )
    METHOD setLang( cLang )
    METHOD lang()           INLINE ::Flang

    PROPERTY placeholder READWRITE INIT ""
    PROPERTY advanceOnEnter READWRITE INIT .T.
    PROPERTY monthNames READWRITE
    PROPERTY dayNames READWRITE
    PROPERTY todayText READWRITE
    PROPERTY dateFormat READWRITE INIT "dd.mm.yyyy"
    PROPERTY weekStart READWRITE INIT 1
    PROPERTY onChanged READWRITE
    PROPERTY onEnter READWRITE

ENDCLASS

METHOD new( ... ) CLASS HTDatePicker

    LOCAL p

    IF pCount() > 0 .AND. hb_isObject( p := hb_pValue( 1 ) )
        ::super:new( p )
    ELSE
        ::super:new()
    ENDIF
    ::setFocusPolicy( HT_FOCUS_STRONG )
    ::FisVisible := .T.
    ::Fheight := 1
    ::Fwidth := 13                       /* dd.mm.yyyy, a space, the symbol */
    ::setLang( ht_dateDefaultLang() )

RETURN self

/** The language of the names, the format and the first day of the week.
 * @return .F. for a language that is not built in (nothing changes) */
METHOD FUNCTION setLang( cLang ) CLASS HTDatePicker

    LOCAL h := ht_dateLang( cLang )

    IF h == NIL
        RETURN .F.
    ENDIF
    ::Flang := Lower( cLang )
    ::FmonthNames := AClone( h[ "months" ] )
    ::FdayNames := AClone( h[ "days" ] )
    ::FtodayText := h[ "today" ]
    ::FdateFormat := h[ "format" ]
    ::FweekStart := h[ "week" ]
    ::Ftext := ht_dateText( ::Fdate, ::FdateFormat )
    ::FnCursor := Len( ::Ftext ) + 1
    ::repaintSelf()

RETURN .T.

METHOD PROCEDURE destroy() CLASS HTDatePicker
    ::closeWindow()
    ::super:destroy()
RETURN

/*----------------------------------------------------------- the value */

METHOD PROCEDURE setValue( xDate ) CLASS HTDatePicker

    LOCAL d := IIF( HB_ISDATE( xDate ), xDate, ht_dateParse( hb_CStr( hb_defaultValue( xDate, "" ) ), ::FdateFormat ) )

    ::Fdate := IIF( d == NIL, CToD( "" ), d )
    ::Ftext := ht_dateText( ::Fdate, ::FdateFormat )
    ::FnCursor := Len( ::Ftext ) + 1
    ::repaintSelf()

RETURN

/* a day taken in the calendar */
METHOD PROCEDURE selectDate( dDate ) CLASS HTDatePicker

    LOCAL lChanged := !( dDate == ::Fdate )

    ::Fdate := dDate
    ::Ftext := ht_dateText( dDate, ::FdateFormat )
    ::FnCursor := Len( ::Ftext ) + 1
    ::closeCalendar()
    IF lChanged .AND. ::FonChanged != NIL
        Eval( ::FonChanged, ::Fdate )
    ENDIF

RETURN

/* Enter / leaving: the typed text as the date; a wrong one goes back */
METHOD PROCEDURE commit() CLASS HTDatePicker

    LOCAL d, dOld := ::Fdate

    IF Empty( AllTrim( ::Ftext ) )
        ::Fdate := CToD( "" )
    ELSE
        d := ht_dateParse( ::Ftext, ::FdateFormat )
        IF d != NIL
            ::Fdate := d
        ENDIF
    ENDIF
    ::Ftext := ht_dateText( ::Fdate, ::FdateFormat )
    ::FnCursor := Len( ::Ftext ) + 1
    IF !( ::Fdate == dOld ) .AND. ::FonChanged != NIL
        Eval( ::FonChanged, ::Fdate )
    ENDIF
    ::repaintSelf()

RETURN

/*----------------------------------------------------------- keys */

/** One key in the field, or in the open calendar. @return .T. when used */
METHOD FUNCTION handleKey( nKey ) CLASS HTDatePicker

    LOCAL cChar, lEnd

    IF ::FlOpen
        RETURN ::calendarKey( nKey )
    ENDIF

    SWITCH nKey
    CASE K_ALT_DOWN
    CASE K_F4
        ::openCalendar()
        RETURN .T.
    CASE K_ENTER
        ::commit()
        IF ::FonEnter != NIL
            Eval( ::FonEnter )
        ENDIF
        IF ::FadvanceOnEnter
            ::moveOn()
        ENDIF
        RETURN .T.
    CASE K_LEFT
        ::FnCursor := Max( 1, ::FnCursor - 1 )
        EXIT
    CASE K_RIGHT
        ::FnCursor := Min( Len( ::Ftext ) + 1, ::FnCursor + 1 )
        EXIT
    CASE K_HOME
        ::FnCursor := 1
        EXIT
    CASE K_END
        ::FnCursor := Len( ::Ftext ) + 1
        EXIT
    CASE K_BS
        IF ::FnCursor > 1
            ::Ftext := Left( ::Ftext, ::FnCursor - 2 ) + SubStr( ::Ftext, ::FnCursor )
            ::FnCursor--
        ENDIF
        EXIT
    CASE K_DEL
        ::Ftext := Left( ::Ftext, ::FnCursor - 1 ) + SubStr( ::Ftext, ::FnCursor + 1 )
        EXIT
    CASE K_CTRL_Y
        ::Ftext := ""
        ::FnCursor := 1
        EXIT
    OTHERWISE
        cChar := ht_keyText( nKey )
        IF cChar == ""
            RETURN .F.
        ENDIF
        /* digits and separators only; the separators come by themselves (as Digi) */
        IF IsDigit( cChar ) .OR. cChar == "." .OR. cChar == "/" .OR. cChar == "-"
            lEnd := ( ::FnCursor > Len( ::Ftext ) )
            ::Ftext := ht_dateAutoFormat( Left( ::Ftext, ::FnCursor - 1 ) + cChar + SubStr( ::Ftext, ::FnCursor ), ::FdateFormat )
            ::FnCursor := IIF( lEnd, Len( ::Ftext ) + 1, Min( Len( ::Ftext ) + 1, ::FnCursor + 1 ) )
        ENDIF
    ENDSWITCH
    ::repaintSelf()

RETURN .T.

/* the keys of the open calendar */
METHOD FUNCTION calendarKey( nKey ) CLASS HTDatePicker

    LOCAL d := ::Fcur

    SWITCH nKey
    CASE K_LEFT       ; d := d - 1 ; EXIT
    CASE K_RIGHT      ; d := d + 1 ; EXIT
    CASE K_UP         ; d := d - 7 ; EXIT
    CASE K_DOWN       ; d := d + 7 ; EXIT
    CASE K_PGUP       ; d := ht_addMonths( d, -1 ) ; EXIT
    CASE K_PGDN       ; d := ht_addMonths( d, 1 ) ; EXIT
    CASE K_CTRL_PGUP  ; d := ht_addMonths( d, -12 ) ; EXIT
    CASE K_CTRL_PGDN  ; d := ht_addMonths( d, 12 ) ; EXIT
    CASE K_HOME       ; d := d - Day( d ) + 1 ; EXIT
    CASE K_END        ; d := d - Day( d ) + ht_daysInMonth( Year( d ), Month( d ) ) ; EXIT
    CASE K_ENTER
    CASE K_SPACE
        ::selectDate( ::Fcur )
        IF ::FadvanceOnEnter
            ::moveOn()
        ENDIF
        RETURN .T.
    CASE K_ESC
    CASE K_ALT_DOWN
    CASE K_F4
        ::closeCalendar()
        RETURN .T.
    OTHERWISE
        IF Upper( ht_keyText( nKey ) ) == "T"
            ::selectDate( Date() )
            IF ::FadvanceOnEnter
                ::moveOn()
            ENDIF
            RETURN .T.
        ENDIF
        RETURN .F.
    ENDSWITCH
    ::Fcur := d
    ::paintCalendar()

RETURN .T.

METHOD PROCEDURE keyEvent( keyEvent ) CLASS HTDatePicker

    IF ::handleKey( keyEvent:key )
        keyEvent:accept()
        IF ::FlOpen .AND. ::FwinId != NIL .AND. ! ::FlInLoop
            ::calendarLoop()
        ENDIF
    ENDIF

RETURN

METHOD PROCEDURE focusOutEvent( eventFocus ) CLASS HTDatePicker
    ::closeCalendar()
    ::commit()
    ::super:focusOutEvent( eventFocus )
RETURN

/*----------------------------------------------------------- the calendar */

/* opens on the date of the field (or the typed one, else today) */
METHOD PROCEDURE openCalendar() CLASS HTDatePicker

    LOCAL d := ht_dateParse( ::Ftext, ::FdateFormat )

    ::Fcur := IIF( d != NIL, d, IIF( Empty( ::Fdate ), Date(), ::Fdate ) )
    ::FlOpen := .T.
    ::openWindow()
    ::repaintSelf()

RETURN

METHOD PROCEDURE closeCalendar() CLASS HTDatePicker
    ::FlOpen := .F.
    ::closeWindow()
    ::repaintSelf()
RETURN

/* under the field (over it when there is no room) */
METHOD PROCEDURE openWindow() CLASS HTDatePicker

    LOCAL oWin := ::topWindow(), aOff := ::windowOffset(), nTop, nLeft, nOldShadow

    IF oWin == NIL .OR. aOff == NIL .OR. ::FwinId != NIL
        RETURN
    ENDIF
    wSelect( oWin:windowId, .F. )
    nTop := wRow() + aOff[ 1 ] + 1
    nLeft := wCol() + aOff[ 2 ]
    IF nTop + _DP_H - 1 > ht_ScreenMaxRow()
        nTop := Max( 0, wRow() + aOff[ 1 ] - _DP_H )
    ENDIF
    nLeft := Max( 0, Min( nLeft, ht_ScreenMaxCol() + 1 - _DP_W ) )
    nOldShadow := wSetShadow( HT_SHADOW_ATTR )
    ::FwinId := wOpen( nTop, nLeft, nTop + _DP_H - 1, nLeft + _DP_W - 1, .T. )
    wSetShadow( nOldShadow )
    ::paintCalendar()

RETURN

METHOD PROCEDURE closeWindow() CLASS HTDatePicker

    LOCAL oWin

    IF ::FwinId != NIL
        wClose( ::FwinId )
        ::FwinId := NIL
        oWin := ::topWindow()
        IF oWin != NIL
            wSelect( oWin:windowId, .F. )
        ENDIF
    ENDIF

RETURN

/* « ◄  Novembar 2026  ► »  /  Pon Uto ...  /  the days  /  Danas */
METHOD PROCEDURE paintCalendar() CLASS HTDatePicker

    LOCAL cN := HTTheme():getColor( HT_CLR_COMBO_DROP_N ), cS := HTTheme():getColor( HT_CLR_COMBO_DROP_S )
    LOCAL cHead := HTTheme():getColor( HT_CLR_TITLEBAR ), aGrid, i, nRow, nCol, nDay, c, d, oWin, cTitle

    IF ::FwinId == NIL
        RETURN
    ENDIF
    wSelect( ::FwinId, .T. )
    DispBox( 0, 0, _DP_H - 1, _DP_W - 1, Replicate( " ", 9 ), cN )
    DispBox( 0, 0, _DP_H - 1, _DP_W - 1, , cN )

    cTitle := ::FmonthNames[ Month( ::Fcur ) ] + " " + hb_ntos( Year( ::Fcur ) )
    cTitle := ht_cellLeft( cTitle, _DP_W - 12 )
    DispOutAt( 1, 1, Space( _DP_W - 2 ), cHead )
    DispOutAt( 1, 2, ht_glyph( 0x00AB, "<" ), cHead )
    DispOutAt( 1, 4, ht_glyph( 0x25C4, "<" ), cHead )
    DispOutAt( 1, 1 + Int( ( _DP_W - 2 - ht_strWidth( cTitle ) ) / 2 ), cTitle, cHead )
    DispOutAt( 1, _DP_W - 5, ht_glyph( 0x25BA, ">" ), cHead )
    DispOutAt( 1, _DP_W - 3, ht_glyph( 0x00BB, ">" ), cHead )

    FOR i := 1 TO 7                       /* dayNames: Monday first; the week may start on Sunday */
        DispOutAt( 2, 1 + ( i - 1 ) * 4, ht_cellPadL( ht_cellLeft( ::FdayNames[ ( ::FweekStart - 1 + i - 1 ) % 7 + 1 ], 3 ), 4 ), cN )
    NEXT

    aGrid := ht_monthGrid( Year( ::Fcur ), Month( ::Fcur ), ::FweekStart )
    FOR i := 1 TO 42
        nRow := 3 + Int( ( i - 1 ) / 7 )
        nCol := 1 + ( ( i - 1 ) % 7 ) * 4
        nDay := aGrid[ i ]
        IF nDay == 0
            DispOutAt( nRow, nCol, Space( 4 ), cN )
            LOOP
        ENDIF
        d := ::Fcur - Day( ::Fcur ) + nDay
        c := Str( nDay, 2 )
        c := IIF( d == Date(), "[" + c + "]", " " + c + " " )     /* today */
        DispOutAt( nRow, nCol, c, IIF( nDay == Day( ::Fcur ), cS, cN ) )
    NEXT

    c := " " + ::FtodayText + " "
    DispOutAt( 9, 1, Space( _DP_W - 2 ), cN )
    DispOutAt( 9, 1 + Int( ( _DP_W - 2 - ht_strWidth( c ) ) / 2 ), c, cHead )

    oWin := ::topWindow()
    IF oWin != NIL
        wSelect( oWin:windowId, .F. )
    ENDIF

RETURN

/* a click in the calendar: the arrows of the header, a day, today */
METHOD PROCEDURE calendarClick( nRow, nCol ) CLASS HTDatePicker

    LOCAL nIdx, aGrid

    DO CASE
    CASE nRow == 1 .AND. nCol >= 1 .AND. nCol <= 3
        ::Fcur := ht_addMonths( ::Fcur, -12 )
    CASE nRow == 1 .AND. nCol >= 4 .AND. nCol <= 6
        ::Fcur := ht_addMonths( ::Fcur, -1 )
    CASE nRow == 1 .AND. nCol >= _DP_W - 6 .AND. nCol <= _DP_W - 4
        ::Fcur := ht_addMonths( ::Fcur, 1 )
    CASE nRow == 1 .AND. nCol >= _DP_W - 3 .AND. nCol <= _DP_W - 2
        ::Fcur := ht_addMonths( ::Fcur, 12 )
    CASE nRow >= 3 .AND. nRow <= 8 .AND. nCol >= 1 .AND. nCol <= _DP_W - 2
        nIdx := ( nRow - 3 ) * 7 + Int( ( nCol - 1 ) / 4 ) + 1
        aGrid := ht_monthGrid( Year( ::Fcur ), Month( ::Fcur ), ::FweekStart )
        IF nIdx <= 42 .AND. aGrid[ nIdx ] > 0
            ::selectDate( ::Fcur - Day( ::Fcur ) + aGrid[ nIdx ] )
            IF ::FadvanceOnEnter
                ::moveOn()
            ENDIF
            RETURN
        ENDIF
    CASE nRow == 9
        ::selectDate( Date() )
        IF ::FadvanceOnEnter
            ::moveOn()
        ENDIF
        RETURN
    ENDCASE
    ::paintCalendar()

RETURN

/* the open calendar takes the keys and the mouse until it closes; a key it does not use
   (Tab ...) closes it and goes to the window */
METHOD PROCEDURE calendarLoop() CLASS HTDatePicker

    LOCAL event, oPending, oWin, nRow, nCol

    ::FlInLoop := .T.
    DO WHILE ::FlOpen .AND. ::FwinId != NIL
        event := HTEventLoop():poll( 0.05 )
        IF event == NIL
            LOOP
        ENDIF
        IF event:className() == "HTKEYEVENT"
            IF ! ::handleKey( event:key )
                oPending := event
                ::closeCalendar()
            ENDIF
        ELSEIF event:className() == "HTMOUSEEVENT" .AND. event:nKey == K_LBUTTONDOWN
            IF ht_windowAtMousePos() == ::FwinId
                wSelect( ::FwinId, .F. )
                nRow := mRow()
                nCol := mCol()
                oWin := ::topWindow()
                IF oWin != NIL
                    wSelect( oWin:windowId, .F. )
                ENDIF
                ::calendarClick( nRow, nCol )
            ELSE
                ::closeCalendar()                       /* a click elsewhere closes it */
            ENDIF
        ENDIF
    ENDDO
    ::closeWindow()
    ::FlInLoop := .F.
    ::repaintSelf()
    IF oPending != NIL .AND. ( oWin := ::topWindow() ) != NIL
        oWin:keyEvent( oPending )
    ENDIF

RETURN

/*----------------------------------------------------------- the field */

/* the symbol on the right opens / closes the calendar */
METHOD PROCEDURE mouseEvent( eventMouse ) CLASS HTDatePicker

    IF eventMouse:nKey != K_LBUTTONDOWN
        RETURN
    ENDIF
    IF eventMouse:mouseCol >= ::Fwidth - 2
        IF ::FlOpen
            ::closeCalendar()
        ELSE
            ::openCalendar()
            IF ::FwinId != NIL .AND. ! ::FlInLoop
                ::calendarLoop()
            ENDIF
        ENDIF
        RETURN
    ENDIF
    ::FnCursor := Min( Len( ::Ftext ) + 1, eventMouse:mouseCol + 1 )
    ::repaintSelf()

RETURN

METHOD FUNCTION cursorPos() CLASS HTDatePicker
RETURN IIF( ::hasFocus(), { 0, ::FnCursor - 1 }, NIL )

METHOD PROCEDURE paintEvent( paintEvent ) CLASS HTDatePicker

    LOCAL nW := MaxCol() + 1, nText := Max( 1, nW - 2 ), cColor, cDim

    HB_SYMBOL_UNUSED( paintEvent )

    cColor := HTTheme():getColor( IIF( ::hasFocus(), HT_CLR_LINEEDIT_FOCUSED, HT_CLR_LINEEDIT_NORMAL ) )
    IF Empty( ::Ftext ) .AND. ! Empty( ::Fplaceholder )
        cDim := "N+/" + hb_tokenGet( hb_tokenGet( cColor, 1, "," ), 2, "/" )
        DispOutAt( 0, 0, ht_cellPadR( ht_cellLeft( ::Fplaceholder, nText ), nText ), cDim )
    ELSE
        DispOutAt( 0, 0, PadR( ::Ftext, nText ), cColor )
    ENDIF
    IF nW >= 3
        DispOutAt( 0, nText, " " + ht_glyph( 0x25BC, "v" ), cColor )
    ENDIF
    IF ::hasFocus()
        SetPos( 0, Min( nText - 1, ::FnCursor - 1 ) )
    ENDIF

RETURN

METHOD PROCEDURE repaintSelf() CLASS HTDatePicker

    LOCAL parent := ::parent()

    IF parent != NIL .AND. parent:isDerivedFrom( "HTWidget" ) .AND. ::topWindow() != NIL
        parent:repaintChild( self )
    ENDIF

RETURN

METHOD FUNCTION topWindow() CLASS HTDatePicker

    LOCAL o := ::parent()

    DO WHILE o != NIL .AND. o:isDerivedFrom( "HTWidget" )
        IF o:hasOwnWindow()
            RETURN o
        ENDIF
        o := o:parent()
    ENDDO

RETURN NIL

METHOD PROCEDURE moveOn() CLASS HTDatePicker

    LOCAL oWin := ::topWindow()

    IF oWin != NIL
        oWin:focusNextChild()
    ENDIF

RETURN

/*----------------------------------------------------------- dates */

/* the date in a format (dd.mm.yyyy, dd/mm/yyyy, mm/dd/yyyy, yyyy-mm-dd ...); "" when empty */
FUNCTION ht_dateText( dDate, cFormat )

    LOCAL c

    IF Empty( dDate )
        RETURN ""
    ENDIF
    c := Lower( hb_defaultValue( cFormat, "dd.mm.yyyy" ) )
    c := StrTran( c, "yyyy", StrZero( Year( dDate ), 4 ) )
    c := StrTran( c, "mm", StrZero( Month( dDate ), 2 ) )

RETURN StrTran( c, "dd", StrZero( Day( dDate ), 2 ) )

/* a typed date -> the date; NIL = not a date. Day and month of one digit too, any of . / -
   between them; yyyy-mm-dd is always taken; mm/dd/yyyy when the format starts with mm */
FUNCTION ht_dateParse( cText, cFormat )

    LOCAL a, nD, nM, nY, c := AllTrim( hb_defaultValue( cText, "" ) ), cF := Lower( hb_defaultValue( cFormat, "dd.mm.yyyy" ) )

    IF ! hb_RegExLike( "[0-9]{1,4}[./-][0-9]{1,2}[./-][0-9]{1,4}", c )
        RETURN NIL
    ENDIF
    a := hb_ATokens( StrTran( StrTran( c, "/", "." ), "-", "." ), "." )
    DO CASE
    CASE Len( a[ 1 ] ) == 4
        nY := Val( a[ 1 ] ) ; nM := Val( a[ 2 ] ) ; nD := Val( a[ 3 ] )
    CASE Len( a[ 3 ] ) != 4
        RETURN NIL
    CASE Left( cF, 2 ) == "mm"
        nM := Val( a[ 1 ] ) ; nD := Val( a[ 2 ] ) ; nY := Val( a[ 3 ] )
    OTHERWISE
        nD := Val( a[ 1 ] ) ; nM := Val( a[ 2 ] ) ; nY := Val( a[ 3 ] )
    ENDCASE
    IF nY < 1 .OR. nM < 1 .OR. nM > 12 .OR. nD < 1 .OR. nD > ht_daysInMonth( nY, nM )
        RETURN NIL                                      /* 31.02. */
    ENDIF

RETURN hb_SToD( StrZero( nY, 4 ) + StrZero( nM, 2 ) + StrZero( nD, 2 ) )

/* as the Digi field types: digits only, the separator of the format comes by itself
   after a full part (20112026 -> 20.11.2026); a typed separator ends a shorter part */
FUNCTION ht_dateAutoFormat( cText, cFormat )

    LOCAL cF := Lower( hb_defaultValue( cFormat, "dd.mm.yyyy" ) ), cSep, aLen, c := "", g := 1, n := 0, ch

    cSep := IIF( "/" $ cF, "/", IIF( "-" $ cF, "-", "." ) )
    aLen := IIF( Left( cF, 4 ) == "yyyy", { 4, 2, 2 }, { 2, 2, 4 } )
    FOR EACH ch IN cText
        IF ch == "." .OR. ch == "/" .OR. ch == "-"
            IF n > 0 .AND. g < 3
                c += cSep
                g++
                n := 0
            ENDIF
        ELSEIF IsDigit( ch ) .AND. n < aLen[ g ]
            c += ch
            n++
            IF n == aLen[ g ] .AND. g < 3
                c += cSep
                g++
                n := 0
            ENDIF
        ENDIF
    NEXT

RETURN c

FUNCTION ht_daysInMonth( nYear, nMonth )

    IF nMonth == 2
        RETURN IIF( ( nYear % 4 == 0 .AND. nYear % 100 != 0 ) .OR. nYear % 400 == 0, 29, 28 )
    ENDIF

RETURN { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }[ nMonth ]

/* the date n months later (earlier): the day stays, or the last day of a shorter month */
FUNCTION ht_addMonths( dDate, nMonths )

    LOCAL nTotal := Year( dDate ) * 12 + Month( dDate ) - 1 + nMonths, nY := Int( nTotal / 12 ), nM := nTotal % 12 + 1

RETURN hb_SToD( StrZero( nY, 4 ) + StrZero( nM, 2 ) + StrZero( Min( Day( dDate ), ht_daysInMonth( nY, nM ) ), 2 ) )

/* 42 cells (6 weeks): the day numbers, 0 = empty; nWeekStart 1 = Monday (default), 7 = Sunday */
FUNCTION ht_monthGrid( nYear, nMonth, nWeekStart )

    LOCAL a := Array( 42 ), dFirst := hb_SToD( StrZero( nYear, 4 ) + StrZero( nMonth, 2 ) + "01" ), nOff, i

    AFill( a, 0 )
    /* DoW(): 1 = Sunday */
    nOff := IIF( hb_defaultValue( nWeekStart, 1 ) == 7, DoW( dFirst ) - 1, ( DoW( dFirst ) + 5 ) % 7 )
    FOR i := 1 TO ht_daysInMonth( nYear, nMonth )
        a[ nOff + i ] := i
    NEXT

RETURN a

/* the language of new date pickers (get / set); "sr" as in Digi */
FUNCTION ht_dateDefaultLang( cLang )

    STATIC s_cLang := "sr"
    LOCAL cOld := s_cLang

    IF HB_ISSTRING( cLang ) .AND. ht_dateLang( cLang ) != NIL
        s_cLang := Lower( cLang )
    ENDIF

RETURN cOld

/* the built-in languages */
FUNCTION ht_dateLangs()
RETURN { "sr", "sr-latn", "hr", "bs", "sl", "mk", "en", "en-us", "de", "fr", "it", "es", "ru", "zh", "ja", "ko" }

/* a language: { "months" => 12 names, "days" => 7 short names from Monday, "today" => ...,
   "format" => "dd.mm.yyyy", "week" => 1 (Monday) / 7 (Sunday) }; NIL = not built in */
FUNCTION ht_dateLang( cLang )

    LOCAL aLatin := { "Januar", "Februar", "Mart", "April", "Maj", "Jun", "Jul", "Avgust", "Septembar", "Oktobar", "Novembar", "Decembar" }

    SWITCH Lower( hb_defaultValue( cLang, "" ) )
    CASE "sr"
        RETURN { "months" => { "Јануар", "Фебруар", "Март", "Април", "Мај", "Јун", "Јул", "Август", "Септембар", "Октобар", "Новембар", "Децембар" }, ;
                 "days" => { "Пон", "Уто", "Сре", "Чет", "Пет", "Суб", "Нед" }, "today" => "Данас", "format" => "dd.mm.yyyy", "week" => 1 }
    CASE "sr-latn"
    CASE "bs"
        RETURN { "months" => aLatin, "days" => { "Pon", "Uto", "Sre", "Čet", "Pet", "Sub", "Ned" }, "today" => "Danas", "format" => "dd.mm.yyyy", "week" => 1 }
    CASE "hr"
        RETURN { "months" => { "Siječanj", "Veljača", "Ožujak", "Travanj", "Svibanj", "Lipanj", "Srpanj", "Kolovoz", "Rujan", "Listopad", "Studeni", "Prosinac" }, ;
                 "days" => { "Pon", "Uto", "Sri", "Čet", "Pet", "Sub", "Ned" }, "today" => "Danas", "format" => "dd.mm.yyyy", "week" => 1 }
    CASE "sl"
        RETURN { "months" => { "Januar", "Februar", "Marec", "April", "Maj", "Junij", "Julij", "Avgust", "September", "Oktober", "November", "December" }, ;
                 "days" => { "Pon", "Tor", "Sre", "Čet", "Pet", "Sob", "Ned" }, "today" => "Danes", "format" => "dd.mm.yyyy", "week" => 1 }
    CASE "mk"
        RETURN { "months" => { "Јануари", "Февруари", "Март", "Април", "Мај", "Јуни", "Јули", "Август", "Септември", "Октомври", "Ноември", "Декември" }, ;
                 "days" => { "Пон", "Вто", "Сре", "Чет", "Пет", "Саб", "Нед" }, "today" => "Денес", "format" => "dd.mm.yyyy", "week" => 1 }
    CASE "en"
    CASE "en-us"
        RETURN { "months" => { "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December" }, ;
                 "days" => { "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun" }, "today" => "Today", ;
                 "format" => IIF( Lower( cLang ) == "en", "dd/mm/yyyy", "mm/dd/yyyy" ), "week" => IIF( Lower( cLang ) == "en", 1, 7 ) }
    CASE "de"
        RETURN { "months" => { "Januar", "Februar", "März", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Dezember" }, ;
                 "days" => { "Mo", "Di", "Mi", "Do", "Fr", "Sa", "So" }, "today" => "Heute", "format" => "dd.mm.yyyy", "week" => 1 }
    CASE "fr"
        RETURN { "months" => { "Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre" }, ;
                 "days" => { "Lun", "Mar", "Mer", "Jeu", "Ven", "Sam", "Dim" }, "today" => "Aujourd'hui", "format" => "dd/mm/yyyy", "week" => 1 }
    CASE "it"
        RETURN { "months" => { "Gennaio", "Febbraio", "Marzo", "Aprile", "Maggio", "Giugno", "Luglio", "Agosto", "Settembre", "Ottobre", "Novembre", "Dicembre" }, ;
                 "days" => { "Lun", "Mar", "Mer", "Gio", "Ven", "Sab", "Dom" }, "today" => "Oggi", "format" => "dd/mm/yyyy", "week" => 1 }
    CASE "es"
        RETURN { "months" => { "Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre" }, ;
                 "days" => { "Lun", "Mar", "Mié", "Jue", "Vie", "Sáb", "Dom" }, "today" => "Hoy", "format" => "dd/mm/yyyy", "week" => 1 }
    CASE "ru"
        RETURN { "months" => { "Январь", "Февраль", "Март", "Апрель", "Май", "Июнь", "Июль", "Август", "Сентябрь", "Октябрь", "Ноябрь", "Декабрь" }, ;
                 "days" => { "Пн", "Вт", "Ср", "Чт", "Пт", "Сб", "Вс" }, "today" => "Сегодня", "format" => "dd.mm.yyyy", "week" => 1 }
    CASE "zh"
        RETURN { "months" => { "一月", "二月", "三月", "四月", "五月", "六月", "七月", "八月", "九月", "十月", "十一月", "十二月" }, ;
                 "days" => { "一", "二", "三", "四", "五", "六", "日" }, "today" => "今天", "format" => "yyyy-mm-dd", "week" => 1 }
    CASE "ja"
        RETURN { "months" => { "1月", "2月", "3月", "4月", "5月", "6月", "7月", "8月", "9月", "10月", "11月", "12月" }, ;
                 "days" => { "月", "火", "水", "木", "金", "土", "日" }, "today" => "今日", "format" => "yyyy-mm-dd", "week" => 7 }
    CASE "ko"
        RETURN { "months" => { "1월", "2월", "3월", "4월", "5월", "6월", "7월", "8월", "9월", "10월", "11월", "12월" }, ;
                 "days" => { "월", "화", "수", "목", "금", "토", "일" }, "today" => "오늘", "format" => "yyyy-mm-dd", "week" => 7 }
    ENDSWITCH

RETURN NIL
