/** @file charts.prg
 * Chart widgets: HTSparkline (live and static) and HTBarChart.
 *
 * Demonstrates:
 *   - HTSparkline 4 rows high (32 levels) fed by a timer: addValue()
 *   - HTSparkline 1 row high with a fixed range (minimum / maximum)
 *   - HTBarChart: labels, compact values (12k, 2.5M), one bar in its own color
 *   - theme colors: the charts follow F1-F7
 *
 * Keyboard:
 *   P       - pause / resume the live data
 *   R       - new random sales figures
 *   F1-F7   - theme
 *   ESC     - quit
 */

#include "hbtui.ch"
#include "inkey.ch"

STATIC s_nTask := 0
STATIC s_oCpu, s_oNet, s_oSales, s_oStatus
STATIC s_nCpu := 40
STATIC s_nBestAt := 1

PROCEDURE Main()

    LOCAL app, win

    SetMode( 30, 80 )

    app := HTApplication():new()

    win := HTMainWindow():new()
    win:setWindowTitle( " Charts - HTSparkline / HTBarChart " )
    win:move( 1, 1 )
    win:resize( 76, 27 )

    /* --- live sparkline, 4 rows = 32 levels --- */
    HTLabel():new( "CPU % (live, 4 rows)", win ):move( 2, 1 )
    s_oCpu := HTSparkline():new( win )
    s_oCpu:move( 2, 2 )
    s_oCpu:resize( 70, 4 )
    s_oCpu:minimum := 0
    s_oCpu:maximum := 100

    /* --- one-row sparkline, own color --- */
    HTLabel():new( "Network (1 row, own color)", win ):move( 2, 7 )
    s_oNet := HTSparkline():new( win )
    s_oNet:move( 2, 8 )
    s_oNet:resize( 70, 1 )

    /* --- bar chart --- */
    HTLabel():new( "Sales 2026 (R = new figures)", win ):move( 2, 10 )
    s_oSales := HTBarChart():new( win )
    s_oSales:move( 2, 11 )
    s_oSales:resize( 70, 11 )
    s_oSales:barWidth := 4
    s_oSales:barGap := 2
    NewSales()

    /* --- status bar --- */
    s_oStatus := HTStatusBar():new( win )
    s_oStatus:move( 1, 23 )
    s_oStatus:addSection( "Live" )
    s_oStatus:addSection( "P pause  R sales  F1-F7 theme  ESC quit" )

    win:onKey( Asc( "p" ), {|| TogglePause() } )
    win:onKey( Asc( "P" ), {|| TogglePause() } )
    win:onKey( Asc( "r" ), {|| NewSales() } )
    win:onKey( Asc( "R" ), {|| NewSales() } )
    win:onKey( K_F1, {|| SwitchTheme( "Classic" ) } )
    win:onKey( K_F2, {|| SwitchTheme( "Blue" ) } )
    win:onKey( K_F3, {|| SwitchTheme( "Light" ) } )
    win:onKey( K_F4, {|| SwitchTheme( "Dark" ) } )
    win:onKey( K_F5, {|| SwitchTheme( "Amber" ) } )
    win:onKey( K_F6, {|| SwitchTheme( "HighContrast" ) } )
    win:onKey( K_F7, {|| SwitchTheme( "Mono" ) } )

    /* some history, then live */
    AEval( Array( 70 ), {|| Tick( .F. ) } )
    s_nTask := HTEventLoop():scheduleRepeat( {|| Tick( .T. ) }, 200 )

    win:show()
    app:exec()

RETURN

/* one new value per sparkline: CPU as a random walk, network as bursts */
STATIC PROCEDURE Tick( lPaint )

    s_nCpu := Max( 2, Min( 98, s_nCpu + hb_RandomInt( -12, 12 ) ) )
    IF lPaint
        s_oCpu:addValue( s_nCpu )
        s_oNet:addValue( IIF( hb_RandomInt( 1, 6 ) = 1, hb_RandomInt( 500, 2000 ), hb_RandomInt( 0, 300 ) ) )
    ELSE
        /* before show(): fill without repainting */
        s_oCpu:addValue( s_nCpu )
        s_oNet:addValue( hb_RandomInt( 0, 1200 ) )
    ENDIF

RETURN

STATIC PROCEDURE NewSales()

    LOCAL aMonths := { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" }
    LOCAL aBars := {}
    LOCAL nBest := 0, i, n

    FOR i := 1 TO Len( aMonths )
        n := hb_RandomInt( 200, 2600 ) * 1000
        AAdd( aBars, { aMonths[ i ], n } )
        IF n > nBest
            nBest := n
            s_nBestAt := i
        ENDIF
    NEXT
    AAdd( aBars[ s_nBestAt ], BestColor() )   /* the best month in its own color */

    s_oNet:color := ThemeColor( HT_CLR_TOAST_SUCCESS )
    s_oSales:setBars( aBars )

RETURN

STATIC PROCEDURE TogglePause()

    IF s_nTask != 0
        HTEventLoop():cancelTask( s_nTask )
        s_nTask := 0
        SetStatus( "Paused" )
    ELSE
        s_nTask := HTEventLoop():scheduleRepeat( {|| Tick( .T. ) }, 200 )
        SetStatus( "Live" )
    ENDIF

RETURN

STATIC PROCEDURE SetStatus( cText )

    LOCAL parent

    s_oStatus:setSection( 1, cText )
    parent := s_oStatus:parent()
    IF parent != NIL
        parent:repaintChild( s_oStatus )
    ENDIF

RETURN

STATIC PROCEDURE SwitchTheme( cName )

    LOCAL aBars

    IF HTTheme():loadTheme( cName )
        /* own colors come from the theme: take them again */
        s_oNet:color := ThemeColor( HT_CLR_TOAST_SUCCESS )
        aBars := s_oSales:bars()
        aBars[ s_nBestAt, 3 ] := BestColor()
        s_oSales:setBars( aBars )
        SetStatus( cName )
        HTApplication():repaintAll()
    ENDIF

RETURN

/* the best month: the error (red) color, or the warning color when the
   theme's bars are already that color (Blue) */
STATIC FUNCTION BestColor()

    LOCAL cBest := ThemeColor( HT_CLR_TOAST_ERROR )

    IF Upper( cBest ) == Upper( ht_colorPair( ht_chartColor() )[ 1 ] )
        cBest := ThemeColor( HT_CLR_TOAST_WARNING )
    ENDIF

RETURN cBest

/* a theme color as a chart foreground: the background of a toast style
   (green success, red error) - fixed names like "R+" depend on the palette */
STATIC FUNCTION ThemeColor( nCategory )
RETURN StrTran( ht_colorPair( HTTheme():getColor( nCategory ) )[ 2 ], "*", "+" )
