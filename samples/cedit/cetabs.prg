/*
 * CEdit - the row of tabs over the editor: one tab for every open file
 * (like Notepad++). The active tab is marked and has a close mark (x);
 * a changed file has * after its name. Click: that file; click on x:
 * close it.
 * When the tabs do not fit, < and > at the ends scroll them; the active
 * tab is always shown.
 *
 * Copyright (c) 2026 Srđan Dragojlović - dual licensed, see LICENSE
 */

#include "hbclass.ch"
#include "inkey.ch"
#include "hbtui.ch"

CLASS CETabBar FROM HTWidget

    DATA bTabs                        /* {|| { { cTitle, lModified }, ... } } */
    DATA bActive                      /* {|| nActive } */
    DATA bSelect                      /* {| n | ... } a click on tab n */
    DATA bClose                       /* {| n | ... } a click on x of tab n */
    DATA nFirst      INIT 1           /* the first tab shown */
    DATA aHits       INIT {}          /* { nFromCol, nToCol, nTab, cWhat } of the last paint */

    METHOD new( parent )
    METHOD paintEvent( event )
    METHOD mouseEvent( eventMouse )
    METHOD refresh()

ENDCLASS

METHOD new( parent ) CLASS CETabBar
    ::super:new( parent )
    ::setFocusPolicy( HT_FOCUS_NONE )
    ::FisVisible := .T.
RETURN self

/* paints the tabs again (a file opened, closed, changed) */
METHOD refresh() CLASS CETabBar

    LOCAL parent := ::parent()

    IF parent != NIL .AND. parent:isDerivedFrom( "HTWidget" )
        parent:repaintChild( self )
    ENDIF

RETURN NIL

STATIC FUNCTION ce_TabLabel( aTab, lActive )
RETURN " " + aTab[ 1 ] + IIF( aTab[ 2 ], "*", "" ) + IIF( lActive, " " + ht_glyph( 0x00D7, "x" ), "" ) + " "

METHOD PROCEDURE paintEvent( event ) CLASS CETabBar

    LOCAL aTabs := IIF( HB_ISEVALITEM( ::bTabs ), Eval( ::bTabs ), {} )
    LOCAL nActive := IIF( HB_ISEVALITEM( ::bActive ), Eval( ::bActive ), 1 )
    LOCAL nW := ::Fwidth, nCol, i, cLabel, nLen, nUsed, lMore
    LOCAL cBar := HTTheme():getColor( HT_CLR_TAB_BAR )
    LOCAL cOn := HTTheme():getColor( HT_CLR_TAB_ACTIVE )
    LOCAL cOff := HTTheme():getColor( HT_CLR_TAB_INACTIVE )

    HB_SYMBOL_UNUSED( event )
    ::aHits := {}
    DispOutAt( 0, 0, Space( nW ), cBar )
    IF Empty( aTabs ) .OR. nW < 8
        RETURN
    ENDIF
    nActive := Max( 1, Min( nActive, Len( aTabs ) ) )

    /* the active tab must be shown: from nFirst on, the tabs up to it must fit
       (2 columns for < and >) */
    ::nFirst := Max( 1, Min( ::nFirst, nActive ) )
    DO WHILE ::nFirst < nActive
        nUsed := IIF( ::nFirst > 1, 2, 0 )
        FOR i := ::nFirst TO nActive
            nUsed += ht_strWidth( ce_TabLabel( aTabs[ i ], i == nActive ) ) + 1
        NEXT
        IF nUsed <= nW - 2
            EXIT
        ENDIF
        ::nFirst++
    ENDDO

    nCol := 0
    IF ::nFirst > 1
        DispOutAt( 0, 0, "<", cBar )
        AAdd( ::aHits, { 0, 0, ::nFirst - 1, "left" } )
        nCol := 2
    ENDIF
    lMore := .F.
    FOR i := ::nFirst TO Len( aTabs )
        cLabel := ce_TabLabel( aTabs[ i ], i == nActive )
        nLen := ht_strWidth( cLabel )
        IF nCol + nLen > nW - IIF( i < Len( aTabs ), 2, 0 )
            lMore := .T.
            EXIT
        ENDIF
        DispOutAt( 0, nCol, cLabel, IIF( i == nActive, cOn, cOff ) )
        IF i == nActive
            AAdd( ::aHits, { nCol + nLen - 2, nCol + nLen - 2, i, "close" } )
        ENDIF
        AAdd( ::aHits, { nCol, nCol + nLen - 1, i, "select" } )
        nCol += nLen
        DispOutAt( 0, nCol, ht_glyph( 0x2502, "|" ), cBar )
        nCol++
    NEXT
    IF lMore
        DispOutAt( 0, nW - 1, ">", cBar )
        AAdd( ::aHits, { nW - 1, nW - 1, Min( i, Len( aTabs ) ), "right" } )
    ENDIF

RETURN

METHOD PROCEDURE mouseEvent( eventMouse ) CLASS CETabBar

    LOCAL nC := eventMouse:mouseCol, a

    SWITCH eventMouse:nKey
    CASE K_LBUTTONDOWN
        FOR EACH a IN ::aHits                  /* "close" comes before "select" */
            IF nC >= a[ 1 ] .AND. nC <= a[ 2 ]
                DO CASE
                CASE a[ 4 ] == "left"
                    ::nFirst := Max( 1, ::nFirst - 1 )
                    ::refresh()
                CASE a[ 4 ] == "right"
                    /* the next tab: painting scrolls to it */
                    IF HB_ISEVALITEM( ::bSelect )
                        Eval( ::bSelect, a[ 3 ] )
                    ENDIF
                CASE a[ 4 ] == "close"
                    IF HB_ISEVALITEM( ::bClose )
                        Eval( ::bClose, a[ 3 ] )
                    ENDIF
                OTHERWISE
                    IF HB_ISEVALITEM( ::bSelect )
                        Eval( ::bSelect, a[ 3 ] )
                    ENDIF
                ENDCASE
                eventMouse:accept()
                RETURN
            ENDIF
        NEXT
        EXIT
    ENDSWITCH

RETURN
