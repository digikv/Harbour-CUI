/*
 * pixelfont - builds the fonts of a .glyphs drawing file (Harbour, no
 * external tools): a TrueType font for Windows (gtwvt) and X11 bitmap fonts
 * for Linux (gtxwc). The work is done by src/HTPixelFont.prg.
 *
 * Build:  hbmk2 tools/pixelfont.prg hbtui.hbc -gtcgi
 * Use:    pixelfont file.glyphs                      what its "# font:" line says
 *         pixelfont file.glyphs out.ttf Family [x11dir]
 *
 * The font editor (samples/fonteditor) builds the same way from its menu.
 */

REQUEST HB_CODEPAGE_UTF8EX

PROCEDURE Main( cGlyphs, cTTF, cFamily, cX11 )

    LOCAL hModel, cError, aCells, cReport, lOk

    hb_cdpSelect( "UTF8EX" )               /* "char X" may be any Unicode character */

    IF Empty( cGlyphs )
        OutStd( "Usage: pixelfont file.glyphs [out.ttf Family [x11dir]]" + hb_eol() )
        ErrorLevel( 2 )
        RETURN
    ENDIF

    hModel := ht_glyphsLoad( cGlyphs, @cError )
    IF hModel == NIL
        OutErr( cError + hb_eol() )
        ErrorLevel( 1 )
        RETURN
    ENDIF

    IF Empty( cTTF )
        lOk := ht_glyphsBuild( hModel, cGlyphs, @cReport )
    ELSE
        aCells := ht_glyphsCells( hModel )
        lOk := ht_pixelFontTTF( aCells, cTTF, hb_defaultValue( cFamily, "PixelFont" ) )
        cReport := cTTF + IIF( lOk, ": " + hb_ntos( Len( aCells ) ) + " characters", ": cannot write" )
        IF lOk .AND. ! Empty( cX11 )
            lOk := ht_pixelFontX11( aCells, cX11, hb_defaultValue( cFamily, "PixelFont" ) )
            cReport += ", " + cX11 + IIF( lOk, ": X11 fonts", ": cannot write" )
        ENDIF
    ENDIF

    IF lOk
        OutStd( cReport + hb_eol() )
    ELSE
        OutErr( cReport + hb_eol() )
    ENDIF
    ErrorLevel( IIF( lOk, 0, 1 ) )

RETURN
