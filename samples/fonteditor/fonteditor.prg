/** @file fonteditor.prg
 * Pixel font editor for .glyphs files (8x8 square cells, sprites 16, 24, 32
 * pixels wide) - draws the characters and builds the fonts: a TrueType font
 * for Windows and X11 bitmap fonts for Linux, all in Harbour
 * (src/HTPixelFont.prg), no external tools.
 *
 * Usage:  fonteditor [file.glyphs]       (default: ../game/nebula.glyphs)
 *
 * Mouse:  left button draws (starting on a pixel that is set: erases),
 *         right button erases; hold and drag.
 * Grid:   arrows move, Space toggles, C clear, I invert, H / V flip,
 *         Ctrl+arrows shift the picture, U undo
 * Keys:   F2 save, F9 build fonts, F3 new character, F4 new sprite,
 *         F5 copy, F6 paste, F8 delete, Tab list <-> grid, Esc quit
 *
 * Demonstrates: an own widget drawing with the mouse (mouseDragWanted:
 * the window hands it every move while the button is held), half-block
 * preview, HTMenuBar, HTDialog with HTGet input, HTMessageBox, HTToast.
 */

#include "hbtui.ch"
#include "inkey.ch"

STATIC s_cFile, s_hModel, s_lDirty := .F.
STATIC s_oMain, s_oList, s_oGrid, s_oPreview, s_oInfo, s_oStatus
STATIC s_aClip                                /* copied rows */
STATIC s_aUndo := {}                          /* { nItem, aRows } */

PROCEDURE Main( cFile )

    LOCAL app, cError, oMenuBar, oMenu, oAct, nRows, nCols

    s_cFile := hb_defaultValue( cFile, hb_DirBase() + ".." + hb_ps() + "game" + hb_ps() + "nebula.glyphs" )

    app := HTApplication():new()
    ht_cuiSetupDisplay( app:rows(), app:cols() )
    nRows := ht_ScreenMaxRow() + 1
    nCols := ht_ScreenMaxCol() + 1

    s_hModel := ht_glyphsLoad( s_cFile, @cError )
    IF s_hModel == NIL
        s_hModel := { "items" => {}, "tail" => "" }
    ENDIF

    s_oMain := HTMainWindow():new()

    oMenuBar := HTMenuBar():new( s_oMain )
    oMenu := oMenuBar:addMenu( "&File" )
    oAct := oMenu:addAction( "&Save        F2" )
    oAct:onTriggered := {|| SaveFile() }
    oAct := oMenu:addAction( "&Build fonts F9" )
    oAct:onTriggered := {|| BuildFonts() }
    oMenu:addSeparator()
    oAct := oMenu:addAction( "&Quit       Esc" )
    oAct:onTriggered := {|| Quit() }

    oMenu := oMenuBar:addMenu( "&Glyph" )
    oAct := oMenu:addAction( "New &character  F3" )
    oAct:onTriggered := {|| NewGlyph( .F. ) }
    oAct := oMenu:addAction( "New &sprite     F4" )
    oAct:onTriggered := {|| NewGlyph( .T. ) }
    oAct := oMenu:addAction( "&Delete         F8" )
    oAct:onTriggered := {|| DeleteGlyph() }

    oMenu := oMenuBar:addMenu( "&Edit" )
    oAct := oMenu:addAction( "&Undo         U" )
    oAct:onTriggered := {|| Undo() }
    oAct := oMenu:addAction( "&Copy        F5" )
    oAct:onTriggered := {|| CopyGlyph() }
    oAct := oMenu:addAction( "&Paste       F6" )
    oAct:onTriggered := {|| PasteGlyph() }
    oMenu:addSeparator()
    oAct := oMenu:addAction( "C&lear        C" )
    oAct:onTriggered := {|| s_oGrid:transform( "clear" ) }
    oAct := oMenu:addAction( "&Invert       I" )
    oAct:onTriggered := {|| s_oGrid:transform( "invert" ) }
    oAct := oMenu:addAction( "Flip &horizontal H" )
    oAct:onTriggered := {|| s_oGrid:transform( "fliph" ) }
    oAct := oMenu:addAction( "Flip &vertical   V" )
    oAct:onTriggered := {|| s_oGrid:transform( "flipv" ) }

    /* glyph list */
    s_oList := HTListBox():new( s_oMain )
    s_oList:move( 1, 2 )
    s_oList:resize( 26, nRows - 6 )
    s_oList:onChanged := {|| ShowGlyph() }

    /* pixel grid, info line, preview */
    s_oGrid := HTPixelGrid():new( s_oMain )
    s_oGrid:move( 30, 2 )
    s_oGrid:onBeforeChange := {|| PushUndo() }
    s_oGrid:onChanged := {|| GlyphChanged() }
    s_oGrid:onCursor := {|| ShowInfo() }
    s_oGrid:maxWidth := nCols - 32

    s_oInfo := HTLabel():new( "", s_oMain )
    s_oInfo:move( 30, 19 )
    s_oInfo:resize( nCols - 32, 1 )

    HTLabel():new( "Preview  1:1 and 2:1", s_oMain ):move( 30, 21 )
    s_oPreview := HTGlyphPreview():new( s_oMain )
    s_oPreview:move( 30, 22 )
    s_oPreview:resize( nCols - 32, 9 )

    s_oStatus := HTStatusBar():new( s_oMain )
    s_oStatus:move( 0, nRows - 3 )
    s_oStatus:resize( nCols, 1 )
    s_oStatus:addSection( hb_FNameNameExt( s_cFile ) )
    s_oStatus:addSection( "F2 save  F9 build  F3/F4 new  F5/F6 copy/paste  Tab grid/list  Esc quit" )

    s_oMain:onKey( K_F2, {|| SaveFile() } )
    s_oMain:onKey( K_F9, {|| BuildFonts() } )
    s_oMain:onKey( K_F3, {|| NewGlyph( .F. ) } )
    s_oMain:onKey( K_F4, {|| NewGlyph( .T. ) } )
    s_oMain:onKey( K_F5, {|| CopyGlyph() } )
    s_oMain:onKey( K_F6, {|| PasteGlyph() } )
    s_oMain:onKey( K_F8, {|| DeleteGlyph() } )
    s_oMain:onKey( K_ESC, {|| Quit() } )

    FillList( 1 )
    UpdateTitle()

    s_oMain:show()
    IF ! Empty( cError )
        HTMessageBox():warning( "Font editor", cError )
    ENDIF
    app:exec()

RETURN

/*--------------------------------------------------------------- list */

STATIC FUNCTION ItemLabel( hItem )

    LOCAL nCells := Int( Len( hItem[ "rows" ][ 1 ] ) / 8 )

    IF hItem[ "kind" ] == "char"
        RETURN PadR( IIF( hItem[ "code" ] == 32, "space", hb_UChar( hItem[ "code" ] ) ), 7 ) + ;
               "U+" + hb_NumToHex( hItem[ "code" ], 4 )
    ENDIF

RETURN hb_NumToHex( hItem[ "code" ], 4 ) + " " + hItem[ "name" ] + IIF( nCells > 1, " (" + hb_ntos( nCells ) + ")", "" )

STATIC PROCEDURE FillList( nSelect )

    LOCAL hItem

    s_oList:clear()
    FOR EACH hItem IN s_hModel[ "items" ]
        s_oList:addItem( ItemLabel( hItem ), hItem:__enumIndex() )
    NEXT
    IF Len( s_hModel[ "items" ] ) > 0
        s_oList:setCurrentIndex( Max( 1, Min( nSelect, Len( s_hModel[ "items" ] ) ) ) )
    ENDIF
    ShowGlyph()

RETURN

STATIC FUNCTION CurrentItem()

    LOCAL n := s_oList:currentIndex()

    IF n >= 1 .AND. n <= Len( s_hModel[ "items" ] )
        RETURN s_hModel[ "items" ][ n ]
    ENDIF

RETURN NIL

STATIC PROCEDURE ShowGlyph()

    LOCAL hItem := CurrentItem()

    s_oGrid:setRows( IIF( hItem == NIL, NIL, hItem[ "rows" ] ) )
    s_oPreview:setRows( IIF( hItem == NIL, NIL, hItem[ "rows" ] ) )
    ShowInfo()
    Repaint()

RETURN

STATIC PROCEDURE ShowInfo()

    LOCAL hItem := CurrentItem()
    LOCAL cText := ""

    IF hItem != NIL
        cText := ht_glyphHeader( hItem ) + "   " + hb_ntos( Len( hItem[ "rows" ][ 1 ] ) ) + " x 8" + ;
                 "   pixel " + hb_ntos( s_oGrid:curX ) + "," + hb_ntos( s_oGrid:curY ) + ;
                 "   C clear  I invert  H/V flip  Ctrl+arrows shift  U undo"
    ENDIF
    s_oInfo:setText( PadR( cText, s_oInfo:width ) )
    s_oMain:repaintChild( s_oInfo )

RETURN

STATIC PROCEDURE Repaint()

    s_oMain:repaintChild( s_oGrid )
    s_oMain:repaintChild( s_oPreview )
    s_oMain:repaintChild( s_oInfo )

RETURN

STATIC PROCEDURE GlyphChanged()

    s_lDirty := .T.
    s_oPreview:setRows( s_oGrid:rows() )
    s_oMain:repaintChild( s_oPreview )
    UpdateTitle()

RETURN

/* title and the status bar's file section; "*" = not saved */
STATIC PROCEDURE UpdateTitle()

    s_oMain:setWindowTitle( " Font editor - " + hb_FNameNameExt( s_cFile ) + " " )
    IF s_oStatus != NIL
        s_oStatus:setSection( 1, hb_FNameNameExt( s_cFile ) + IIF( s_lDirty, " *", "  " ) )
        s_oMain:repaintChild( s_oStatus )
    ENDIF

RETURN

/*--------------------------------------------------------------- undo, clipboard */

STATIC PROCEDURE PushUndo()

    IF CurrentItem() != NIL
        AAdd( s_aUndo, { s_oList:currentIndex(), AClone( CurrentItem()[ "rows" ] ) } )
        IF Len( s_aUndo ) > 200
            hb_ADel( s_aUndo, 1, .T. )
        ENDIF
    ENDIF

RETURN

STATIC PROCEDURE Undo()

    LOCAL aLast

    IF Len( s_aUndo ) == 0
        HTToast():show( "Nothing to undo", 1500 )
        RETURN
    ENDIF
    aLast := ATail( s_aUndo )
    ASize( s_aUndo, Len( s_aUndo ) - 1 )
    IF aLast[ 1 ] <= Len( s_hModel[ "items" ] )
        ACopy( aLast[ 2 ], s_hModel[ "items" ][ aLast[ 1 ] ][ "rows" ] )
        s_oList:setCurrentIndex( aLast[ 1 ] )
        s_lDirty := .T.
        UpdateTitle()
        ShowGlyph()
    ENDIF

RETURN

STATIC PROCEDURE CopyGlyph()

    IF CurrentItem() != NIL
        s_aClip := AClone( CurrentItem()[ "rows" ] )
        HTToast():show( "Copied", 1200 )
    ENDIF

RETURN

/* pastes into the current glyph; a wider or narrower copy is cut or padded */
STATIC PROCEDURE PasteGlyph()

    LOCAL hItem := CurrentItem(), i, nW

    IF hItem == NIL .OR. s_aClip == NIL
        RETURN
    ENDIF
    PushUndo()
    nW := Len( hItem[ "rows" ][ 1 ] )
    FOR i := 1 TO 8
        hItem[ "rows" ][ i ] := PadR( Left( s_aClip[ i ], nW ), nW, "." )
    NEXT
    GlyphChanged()
    ShowGlyph()

RETURN

/*--------------------------------------------------------------- file */

STATIC PROCEDURE SaveFile()

    IF ht_glyphsSave( s_hModel, s_cFile )
        s_lDirty := .F.
        UpdateTitle()
        HTToast():show( "Saved " + hb_FNameNameExt( s_cFile ), 2000, HT_CLR_TOAST_SUCCESS )
    ELSE
        HTMessageBox():warning( "Save", "Cannot write " + s_cFile )
    ENDIF

RETURN

STATIC PROCEDURE BuildFonts()

    LOCAL cReport

    IF ht_glyphsBuild( s_hModel, s_cFile, @cReport )
        HTToast():show( cReport, 4000, HT_CLR_TOAST_SUCCESS )
    ELSE
        HTMessageBox():warning( "Build fonts", cReport )
    ENDIF

RETURN

STATIC PROCEDURE Quit()

    IF s_lDirty .AND. HTMessageBox():question( "Font editor", "Save the changes to " + hb_FNameNameExt( s_cFile ) + "?" ) == HT_DIALOG_ACCEPTED
        SaveFile()
    ENDIF
    HTApplication():quit()

RETURN

/*--------------------------------------------------------------- new, delete */

/* asks for a character (or hex code), a name and, for sprites, the width */
STATIC PROCEDURE NewGlyph( lSprite )

    LOCAL cCode := Space( 6 ), cName := Space( 16 ), nCells := IIF( lSprite, 2, 1 )
    LOCAL dlg, oGet, nCode, cKind, hItem, nAt, hOther, cError := ""

    dlg := HTDialog():new( NIL, IIF( lSprite, "New sprite", "New character" ) )
    dlg:resize( 46, IIF( lSprite, 11, 9 ) )

    oGet := HTGet():new( dlg )
    oGet:setup( {| x | IIF( x == NIL, cCode, cCode := x ) }, ;
                IIF( lSprite, "Code (hex):", "Character or hex code:" ), "@!", ;
                NIL, NIL, "", NIL, .F., .F., 40 )
    oGet:labelWidth := 24
    oGet:move( 2, 1 )
    oGet := HTGet():new( dlg )
    oGet:setup( {| x | IIF( x == NIL, cName, cName := x ) }, "Name:", "", NIL, NIL, "", NIL, .F., .F., 40 )
    oGet:labelWidth := 24
    oGet:move( 2, 3 )
    IF lSprite
        oGet := HTGet():new( dlg )
        oGet:setup( {| x | IIF( x == NIL, nCells, nCells := x ) }, "Width (cells of 8):", "9", NIL, NIL, "", NIL, .F., .F., 40 )
        oGet:labelWidth := 24
        oGet:move( 2, 5 )
    ENDIF
    dlg:addButtonBar( "OK", "Cancel" )
    IF dlg:exec() != HT_DIALOG_ACCEPTED
        RETURN
    ENDIF

    cCode := AllTrim( cCode )
    nCells := Max( 1, Min( 8, nCells ) )
    IF ! lSprite .AND. Len( cCode ) == 1
        cKind := "char"
        nCode := hb_UCode( cCode )
    ELSEIF Len( cCode ) >= 2 .AND. Empty( CharOnlyHex( cCode ) )
        cKind := IIF( lSprite .OR. hb_HexToNum( cCode ) > 126 .OR. hb_HexToNum( cCode ) < 32, "glyph", "char" )
        nCode := hb_HexToNum( cCode )
    ELSE
        cError := "Give one character or a hex code (e.g. E040)"
    ENDIF
    IF cError == "" .AND. ( nCode < 32 .OR. nCode + nCells - 1 > 0xFFFF )
        cError := "The code must be between 0020 and FFFF"
    ENDIF
    IF cError == ""
        FOR EACH hOther IN s_hModel[ "items" ]
            IF nCode <= hOther[ "code" ] + Len( hOther[ "rows" ][ 1 ] ) / 8 - 1 .AND. nCode + nCells - 1 >= hOther[ "code" ]
                cError := "U+" + hb_NumToHex( nCode, 4 ) + " is taken: " + ht_glyphHeader( hOther )
                EXIT
            ENDIF
        NEXT
    ENDIF
    IF ! cError == ""
        HTMessageBox():warning( "New", cError )
        RETURN
    ENDIF

    hItem := { "prefix" => Chr( 10 ), "kind" => cKind, "code" => nCode, ;
               "name" => AllTrim( StrTran( cName, " ", "-" ) ), ;
               "rows" => AFill( Array( 8 ), Replicate( ".", nCells * 8 ) ) }
    IF cKind == "char"
        hItem[ "name" ] := ""
    ENDIF
    nAt := Max( 1, s_oList:currentIndex() ) + 1
    IF Len( s_hModel[ "items" ] ) == 0
        nAt := 1
    ENDIF
    hb_AIns( s_hModel[ "items" ], nAt, hItem, .T. )
    s_lDirty := .T.
    UpdateTitle()
    FillList( nAt )

RETURN

STATIC FUNCTION CharOnlyHex( c )

    LOCAL i, cOut := ""

    FOR i := 1 TO Len( c )
        IF ! SubStr( c, i, 1 ) $ "0123456789ABCDEF"
            cOut += SubStr( c, i, 1 )
        ENDIF
    NEXT

RETURN cOut

STATIC PROCEDURE DeleteGlyph()

    LOCAL hItem := CurrentItem(), n := s_oList:currentIndex()

    IF hItem == NIL
        RETURN
    ENDIF
    IF HTMessageBox():question( "Delete", "Delete " + ht_glyphHeader( hItem ) + "?" ) == HT_DIALOG_ACCEPTED
        /* its comments stay with the next glyph */
        IF n < Len( s_hModel[ "items" ] )
            s_hModel[ "items" ][ n + 1 ][ "prefix" ] := hItem[ "prefix" ] + s_hModel[ "items" ][ n + 1 ][ "prefix" ]
        ENDIF
        hb_ADel( s_hModel[ "items" ], n, .T. )
        s_aUndo := {}
        s_lDirty := .T.
        UpdateTitle()
        FillList( n )
    ENDIF

RETURN

/*================================================================ HTPixelGrid */

/* Edits the rows of one glyph: every pixel pixW x pixH cells (4 x 2 is
   square with the usual 1:2 cell), the background alternates per 8-pixel
   cell of a sprite. */
CLASS HTPixelGrid FROM HTWidget

PROTECTED:

    DATA FaRows
    DATA FcDraw                               /* "#" / "." while a button is held */

PUBLIC:

    CONSTRUCTOR new( parent )

    METHOD paintEvent( event )
    METHOD keyEvent( keyEvent )
    METHOD mouseEvent( eventMouse )
    METHOD mouseDragWanted() INLINE .T.
    METHOD setRows( aRows )
    METHOD rows() INLINE ::FaRows
    METHOD transform( cHow )
    METHOD fitSize()
    METHOD setPixel( nX, nY, cPixel )
    METHOD pixelAt( nRow, nCol, nX, nY )
    METHOD changed()

    PROPERTY curX READWRITE INIT 1
    PROPERTY curY READWRITE INIT 1
    PROPERTY pixW READWRITE INIT 4
    PROPERTY pixH READWRITE INIT 2
    PROPERTY maxWidth READWRITE INIT 96
    PROPERTY onChanged READWRITE
    PROPERTY onBeforeChange READWRITE
    PROPERTY onCursor READWRITE

ENDCLASS

METHOD new( parent ) CLASS HTPixelGrid

    ::super:new( parent )
    ::setFocusPolicy( HT_FOCUS_STRONG )
    ::FisVisible := .T.
    ::Fwidth := 32
    ::Fheight := 16

RETURN self

METHOD PROCEDURE setRows( aRows ) CLASS HTPixelGrid

    ::FaRows := aRows
    ::FcDraw := NIL
    IF aRows != NIL
        ::FcurX := Min( ::FcurX, Len( aRows[ 1 ] ) )
    ENDIF
    ::fitSize()

RETURN

/* pixel size: 4 x 2 cells, smaller for wide sprites */
METHOD PROCEDURE fitSize() CLASS HTPixelGrid

    LOCAL nW := IIF( ::FaRows == NIL, 8, Len( ::FaRows[ 1 ] ) )

    ::FpixW := 4
    DO WHILE ::FpixW > 2 .AND. nW * ::FpixW > ::FmaxWidth
        ::FpixW -= 2
    ENDDO
    ::FpixH := ::FpixW / 2
    ::Fwidth := nW * ::FpixW
    ::Fheight := 8 * ::FpixH

RETURN

METHOD PROCEDURE paintEvent( event ) CLASS HTPixelGrid

    LOCAL nX, nY, cPix, cColor, cCell, nR, lCursor
    LOCAL cDot := ht_glyph( 0x00B7, "." )

    HB_SYMBOL_UNUSED( event )

    /* clear the area a wider glyph left */
    FOR nR := 0 TO MaxRow()
        DispOutAt( nR, 0, Space( MaxCol() + 1 ), HTTheme():getColor( HT_CLR_WINDOW ) )
    NEXT
    IF ::FaRows == NIL
        RETURN
    ENDIF

    FOR nY := 1 TO 8
        FOR nX := 1 TO Len( ::FaRows[ 1 ] )
            cPix := SubStr( ::FaRows[ nY ], nX, 1 )
            lCursor := ::hasFocus() .AND. nX == ::FcurX .AND. nY == ::FcurY
            IF cPix == "#"
                cColor := IIF( lCursor, "R+/N", "W/N" )
                cCell := Space( ::FpixW )
            ELSE
                /* alternate per 8-pixel cell; a dot marks the pixel */
                cColor := IIF( Int( ( nX - 1 ) / 8 ) % 2 == 0, "W/W*", "W*/W" )
                IF lCursor
                    cColor := "R+/" + SubStr( cColor, At( "/", cColor ) + 1 )
                ENDIF
                cCell := PadC( cDot, ::FpixW )
            ENDIF
            IF lCursor
                cCell := "[" + Space( ::FpixW - 2 ) + "]"
            ENDIF
            FOR nR := 0 TO ::FpixH - 1
                DispOutAt( ( nY - 1 ) * ::FpixH + nR, ( nX - 1 ) * ::FpixW, ;
                           IIF( nR == 0, cCell, Space( ::FpixW ) ), cColor )
            NEXT
        NEXT
    NEXT

RETURN

/* the pixel (1-based) under widget cell nRow, nCol; .F. outside */
METHOD FUNCTION pixelAt( nRow, nCol, nX, nY ) CLASS HTPixelGrid

    IF ::FaRows == NIL .OR. nRow < 0 .OR. nCol < 0
        RETURN .F.
    ENDIF
    nX := Int( nCol / ::FpixW ) + 1
    nY := Int( nRow / ::FpixH ) + 1

RETURN nX <= Len( ::FaRows[ 1 ] ) .AND. nY <= 8

METHOD PROCEDURE setPixel( nX, nY, cPixel ) CLASS HTPixelGrid

    IF ! SubStr( ::FaRows[ nY ], nX, 1 ) == cPixel
        ::FaRows[ nY ] := Stuff( ::FaRows[ nY ], nX, 1, cPixel )
        ::changed()
    ENDIF

RETURN

METHOD PROCEDURE changed() CLASS HTPixelGrid

    IF ::FonChanged != NIL
        Eval( ::FonChanged )
    ENDIF
    ::parent():repaintChild( self )

RETURN

METHOD PROCEDURE mouseEvent( eventMouse ) CLASS HTPixelGrid

    LOCAL nX, nY

    IF ::FaRows == NIL
        RETURN
    ENDIF

    SWITCH eventMouse:nKey
    CASE K_LBUTTONDOWN
    CASE K_RBUTTONDOWN
        IF ::pixelAt( eventMouse:mouseRow, eventMouse:mouseCol, @nX, @nY )
            IF ::FonBeforeChange != NIL
                Eval( ::FonBeforeChange )
            ENDIF
            /* left: draw - or erase when starting on a set pixel; right: erase */
            ::FcDraw := IIF( eventMouse:nKey == K_RBUTTONDOWN .OR. SubStr( ::FaRows[ nY ], nX, 1 ) == "#", ".", "#" )
            ::FcurX := nX
            ::FcurY := nY
            ::setPixel( nX, nY, ::FcDraw )
            ::parent():repaintChild( self )
            IF ::FonCursor != NIL
                Eval( ::FonCursor )
            ENDIF
        ENDIF
        EXIT
    CASE K_MOUSEMOVE
        IF ::FcDraw != NIL .AND. ::pixelAt( eventMouse:mouseRow, eventMouse:mouseCol, @nX, @nY )
            ::setPixel( nX, nY, ::FcDraw )
        ENDIF
        EXIT
    CASE K_LBUTTONUP
    CASE K_RBUTTONUP
        ::FcDraw := NIL
        EXIT
    ENDSWITCH

RETURN

METHOD PROCEDURE keyEvent( keyEvent ) CLASS HTPixelGrid

    LOCAL nW, cKey

    IF ::FaRows == NIL
        RETURN
    ENDIF
    nW := Len( ::FaRows[ 1 ] )
    cKey := Upper( hb_keyChar( keyEvent:key ) )

    DO CASE
    CASE keyEvent:key == K_LEFT
        ::FcurX := IIF( ::FcurX > 1, ::FcurX - 1, nW )
    CASE keyEvent:key == K_RIGHT
        ::FcurX := IIF( ::FcurX < nW, ::FcurX + 1, 1 )
    CASE keyEvent:key == K_UP
        ::FcurY := IIF( ::FcurY > 1, ::FcurY - 1, 8 )
    CASE keyEvent:key == K_DOWN
        ::FcurY := IIF( ::FcurY < 8, ::FcurY + 1, 1 )
    CASE keyEvent:key == K_SPACE
        IF ::FonBeforeChange != NIL
            Eval( ::FonBeforeChange )
        ENDIF
        ::setPixel( ::FcurX, ::FcurY, IIF( SubStr( ::FaRows[ ::FcurY ], ::FcurX, 1 ) == "#", ".", "#" ) )
    CASE keyEvent:key == K_CTRL_LEFT
        ::transform( "left" )
    CASE keyEvent:key == K_CTRL_RIGHT
        ::transform( "right" )
    CASE keyEvent:key == K_CTRL_UP
        ::transform( "up" )
    CASE keyEvent:key == K_CTRL_DOWN
        ::transform( "down" )
    CASE cKey == "C"
        ::transform( "clear" )
    CASE cKey == "I"
        ::transform( "invert" )
    CASE cKey == "H"
        ::transform( "fliph" )
    CASE cKey == "V"
        ::transform( "flipv" )
    CASE cKey == "U"
        Undo()
        keyEvent:accept()
        RETURN
    OTHERWISE
        RETURN
    ENDCASE

    keyEvent:accept()
    ::parent():repaintChild( self )
    IF ::FonCursor != NIL
        Eval( ::FonCursor )
    ENDIF

RETURN

/* clear, invert, fliph, flipv, left, right, up, down (shift by one pixel) */
METHOD PROCEDURE transform( cHow ) CLASS HTPixelGrid

    LOCAL i, nW, cRow, aOld

    IF ::FaRows == NIL
        RETURN
    ENDIF
    IF ::FonBeforeChange != NIL
        Eval( ::FonBeforeChange )
    ENDIF
    nW := Len( ::FaRows[ 1 ] )
    aOld := AClone( ::FaRows )

    FOR i := 1 TO 8
        cRow := ::FaRows[ i ]
        SWITCH cHow
        CASE "clear"
            cRow := Replicate( ".", nW )
            EXIT
        CASE "invert"
            cRow := hb_StrReplace( cRow, { "#" => ".", "." => "#" } )
            EXIT
        CASE "fliph"
            cRow := Reverse( cRow )
            EXIT
        CASE "flipv"
            cRow := aOld[ 9 - i ]
            EXIT
        CASE "left"
            cRow := SubStr( cRow, 2 ) + "."
            EXIT
        CASE "right"
            cRow := "." + Left( cRow, nW - 1 )
            EXIT
        CASE "up"
            cRow := IIF( i < 8, aOld[ i + 1 ], Replicate( ".", nW ) )
            EXIT
        CASE "down"
            cRow := IIF( i > 1, aOld[ i - 1 ], Replicate( ".", nW ) )
            EXIT
        ENDSWITCH
        ::FaRows[ i ] := cRow
    NEXT
    ::changed()

RETURN

STATIC FUNCTION Reverse( c )

    LOCAL cOut := "", i

    FOR i := Len( c ) TO 1 STEP -1
        cOut += SubStr( c, i, 1 )
    NEXT

RETURN cOut

/*================================================================ HTGlyphPreview */

/* The glyph at 1:1 (half blocks: two pixel rows per cell row, one pixel per
   column - square) and 2:1 (two columns and one row per pixel). */
CLASS HTGlyphPreview FROM HTWidget

PROTECTED:

    DATA FaRows

PUBLIC:

    CONSTRUCTOR new( parent )
    METHOD paintEvent( event )
    METHOD setRows( aRows ) INLINE ::FaRows := aRows

ENDCLASS

METHOD new( parent ) CLASS HTGlyphPreview

    ::super:new( parent )
    ::setFocusPolicy( HT_FOCUS_NONE )
    ::FisVisible := .T.
    ::Fheight := 9

RETURN self

METHOD PROCEDURE paintEvent( event ) CLASS HTGlyphPreview

    LOCAL nR, nX, cLine, lTop, lBottom, nW
    LOCAL cUpper := ht_glyph( 0x2580, "^" ), cLower := ht_glyph( 0x2584, "_" ), cFull := ht_glyph( 0x2588, "#" )
    LOCAL cColor := "N/W*"

    HB_SYMBOL_UNUSED( event )

    FOR nR := 0 TO MaxRow()
        DispOutAt( nR, 0, Space( MaxCol() + 1 ), HTTheme():getColor( HT_CLR_WINDOW ) )
    NEXT
    IF ::FaRows == NIL
        RETURN
    ENDIF
    nW := Len( ::FaRows[ 1 ] )

    /* 1:1 - rows 0..3 */
    FOR nR := 1 TO 7 STEP 2
        cLine := ""
        FOR nX := 1 TO nW
            lTop := SubStr( ::FaRows[ nR ], nX, 1 ) == "#"
            lBottom := SubStr( ::FaRows[ nR + 1 ], nX, 1 ) == "#"
            cLine += IIF( lTop .AND. lBottom, cFull, IIF( lTop, cUpper, IIF( lBottom, cLower, " " ) ) )
        NEXT
        DispOutAt( Int( nR / 2 ), 1, " " + cLine + " ", cColor )
    NEXT

    /* 2:1 - next to it, rows 0..7 */
    IF nW + 4 + nW * 2 + 2 <= MaxCol() + 1
        FOR nR := 1 TO 8
            cLine := ""
            FOR nX := 1 TO nW
                cLine += IIF( SubStr( ::FaRows[ nR ], nX, 1 ) == "#", cFull + cFull, "  " )
            NEXT
            DispOutAt( nR - 1, nW + 5, " " + cLine + " ", cColor )
        NEXT
    ENDIF

RETURN
