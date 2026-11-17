/*
 * HTPixelFont.prg - square-cell 8x8 pixel fonts, written in Harbour:
 * the .glyphs drawing file, a TrueType font (Windows / gtwvt) and X11 bitmap
 * fonts (BDF + fonts.dir, Linux / gtxwc). No external tools.
 *
 * .glyphs file: every character is 8 rows of '#' (pixel) and '.' (empty);
 * wider sprites are 16, 24, ... columns and become consecutive characters:
 *
 *     # comment                 (a line starting with "# ")
 *     # font: nebula8.ttf Nebula8 x11     (optional: what ht_glyphsBuild makes)
 *     char A                    the character itself; "char space" = U+0020
 *     ..####..
 *     ...                       (8 rows)
 *     glyph E000 ship           hex code point (Private Use Area E000..F8FF)
 *     .......##.......          16 columns -> U+E000 and U+E001
 *     ...                       (8 rows)
 *
 * Every character advances one square cell, 8 x 8 pixels, no descent.
 *
 *   ht_glyphsLoad( cFile )               -> hModel   (NIL + error text in cError)
 *   ht_glyphsSave( hModel, cFile )       -> lOk      (comments and order kept)
 *   ht_glyphsCells( hModel )             -> { { nCode, aRows8 }, ... }
 *   ht_pixelFontTTF( aCells, cFile, cFamily ) -> lOk
 *   ht_pixelFontX11( aCells, cDir, cFamily [, aSizes ] ) -> lOk
 *   ht_glyphsBuild( hModel, cGlyphsFile [, @cReport ] ) -> lOk  (the "# font:" line)
 *
 * hModel = { "items" => { hItem, ... }, "tail" => cText }
 * hItem  = { "prefix" => cCommentsBefore, "kind" => "char"|"glyph",
 *            "code" => nCode, "name" => cName, "rows" => { 8 strings } }
 */

#include "fileio.ch"

#define PIXEL   128                 /* font units per pixel */
#define CELL    1024                /* units per em = cell width = cell height */
#define LF      Chr( 10 )

/*--------------------------------------------------------------- .glyphs */

FUNCTION ht_glyphsLoad( cFile, cError )

    LOCAL cText, aLines, nLine := 1, cLine, cPrefix := "", aWords
    LOCAL aItems := {}, nCode, cKind, cName, aRows, i, nWidth

    cError := ""
    IF ! hb_FileExists( cFile )
        cError := "File not found: " + cFile
        RETURN NIL
    ENDIF
    cText := StrTran( hb_MemoRead( cFile ), Chr( 13 ) )
    aLines := hb_ATokens( cText, LF )
    IF Len( aLines ) > 0 .AND. ATail( aLines ) == ""
        ASize( aLines, Len( aLines ) - 1 )
    ENDIF

    DO WHILE nLine <= Len( aLines )
        cLine := AllTrim( aLines[ nLine ] )
        IF cLine == "" .OR. cLine == "#" .OR. Left( cLine, 2 ) == "# "
            cPrefix += aLines[ nLine ] + LF
            nLine++
            LOOP
        ENDIF
        aWords := hb_ATokens( cLine, " " )
        cName := ""
        IF aWords[ 1 ] == "char" .AND. Len( cLine ) >= 6
            cKind := "char"
            nCode := IIF( Len( aWords ) >= 2 .AND. aWords[ 2 ] == "space", 32, hb_UCode( SubStr( cLine, 6, 1 ) ) )
        ELSEIF aWords[ 1 ] == "glyph" .AND. Len( aWords ) >= 2
            cKind := "glyph"
            nCode := hb_HexToNum( aWords[ 2 ] )
            IF Len( aWords ) >= 3
                cName := AllTrim( SubStr( cLine, At( aWords[ 2 ], cLine ) + Len( aWords[ 2 ] ) ) )
            ENDIF
        ELSE
            cError := cFile + "(" + hb_ntos( nLine ) + "): expected 'char' or 'glyph': " + cLine
            RETURN NIL
        ENDIF
        aRows := {}
        FOR i := 1 TO 8
            IF nLine + i > Len( aLines )
                cError := cFile + "(" + hb_ntos( nLine ) + "): " + cLine + " needs 8 rows"
                RETURN NIL
            ENDIF
            AAdd( aRows, AllTrim( aLines[ nLine + i ] ) )
        NEXT
        nWidth := Len( aRows[ 1 ] )
        FOR i := 1 TO 8
            IF Len( aRows[ i ] ) != nWidth .OR. nWidth % 8 != 0 .OR. nWidth == 0 .OR. ;
               ! Empty( CharOnlyPixels( aRows[ i ] ) )
                cError := cFile + "(" + hb_ntos( nLine + i ) + "): rows of " + cLine + ;
                          " must be equal, a multiple of 8 long, '#' and '.' only"
                RETURN NIL
            ENDIF
        NEXT
        AAdd( aItems, { "prefix" => cPrefix, "kind" => cKind, "code" => nCode, ;
                        "name" => cName, "rows" => aRows } )
        cPrefix := ""
        nLine += 9
    ENDDO

RETURN { "items" => aItems, "tail" => cPrefix }

/* the characters of a row that are neither '#' nor '.' */
STATIC FUNCTION CharOnlyPixels( cRow )
RETURN StrTran( StrTran( cRow, "#" ), "." )

/** The header line of an item: "char A", "char space", "glyph E000 ship". */
FUNCTION ht_glyphHeader( hItem )

    IF hItem[ "kind" ] == "char"
        RETURN "char " + IIF( hItem[ "code" ] == 32, "space", hb_UChar( hItem[ "code" ] ) )
    ENDIF

RETURN "glyph " + hb_NumToHex( hItem[ "code" ], 4 ) + IIF( Empty( hItem[ "name" ] ), "", " " + hItem[ "name" ] )

FUNCTION ht_glyphsSave( hModel, cFile )

    LOCAL cOut := "", hItem

    FOR EACH hItem IN hModel[ "items" ]
        cOut += hItem[ "prefix" ] + ht_glyphHeader( hItem ) + LF
        AEval( hItem[ "rows" ], {| c | cOut += c + LF } )
    NEXT
    cOut += hModel[ "tail" ]

RETURN hb_MemoWrit( cFile, cOut )

/** Every 8-column cell as its own character: { { nCode, aRows8 }, ... },
 * sorted by code, with a space when the file has none. */
FUNCTION ht_glyphsCells( hModel )

    LOCAL aCells := {}, hItem, nCell, lSpace := .F.

    FOR EACH hItem IN hModel[ "items" ]
        FOR nCell := 0 TO Len( hItem[ "rows" ][ 1 ] ) / 8 - 1
            AAdd( aCells, { hItem[ "code" ] + nCell, {} } )
            AEval( hItem[ "rows" ], {| c | AAdd( ATail( aCells )[ 2 ], SubStr( c, nCell * 8 + 1, 8 ) ) } )
            IF hItem[ "code" ] + nCell == 32
                lSpace := .T.
            ENDIF
        NEXT
    NEXT
    IF ! lSpace
        AAdd( aCells, { 32, AFill( Array( 8 ), "........" ) } )
    ENDIF
    ASort( aCells,,, {| a, b | a[ 1 ] < b[ 1 ] } )

RETURN aCells

/** Builds what the "# font: <ttf> <family> [<x11dir>]" line of the glyph file
 * asks for (paths relative to the glyph file).
 * @param cReport receives a short report or the error
 */
FUNCTION ht_glyphsBuild( hModel, cGlyphsFile, cReport )

    LOCAL cDir := hb_FNameDir( cGlyphsFile ), cLine := "", aWords, aCells
    LOCAL hItem, aLines, c, lOk

    cReport := ""
    /* the "# font:" line is in a prefix (comments) */
    FOR EACH hItem IN hModel[ "items" ]
        aLines := hb_ATokens( hItem[ "prefix" ], LF )
        FOR EACH c IN aLines
            IF Left( LTrim( c ), 7 ) == "# font:"
                cLine := AllTrim( SubStr( LTrim( c ), 8 ) )
                EXIT
            ENDIF
        NEXT
        IF ! cLine == ""
            EXIT
        ENDIF
    NEXT
    IF cLine == ""
        cReport := 'No "# font: <file.ttf> <Family> [<x11 folder>]" line in ' + cGlyphsFile
        RETURN .F.
    ENDIF
    aWords := hb_ATokens( cLine, " " )
    IF Len( aWords ) < 2
        cReport := 'The "# font:" line needs a .ttf file and a family name'
        RETURN .F.
    ENDIF

    aCells := ht_glyphsCells( hModel )
    lOk := ht_pixelFontTTF( aCells, cDir + aWords[ 1 ], aWords[ 2 ] )
    cReport := aWords[ 1 ] + IIF( lOk, ": " + hb_ntos( Len( aCells ) ) + " characters", ": cannot write (in use?)" )
    IF Len( aWords ) >= 3
        IF ht_pixelFontX11( aCells, cDir + aWords[ 3 ], aWords[ 2 ] )
            cReport += ", " + aWords[ 3 ] + "/: X11 fonts"
        ELSE
            cReport += ", " + aWords[ 3 ] + "/: cannot write"
            lOk := .F.
        ENDIF
    ENDIF

RETURN lOk

/*--------------------------------------------------------------- binary helpers */

STATIC FUNCTION U16( n )
    n := Int( n )
    IF n < 0
        n += 65536
    ENDIF
RETURN hb_BChar( Int( n / 256 ) % 256 ) + hb_BChar( n % 256 )

STATIC FUNCTION U32( n )
    n := Int( n )
    IF n < 0
        n += 4294967296
    ENDIF
RETURN U16( Int( n / 65536 ) ) + U16( n % 65536 )

STATIC FUNCTION Pad4( c )
RETURN c + Replicate( hb_BChar( 0 ), ( 4 - hb_BLen( c ) % 4 ) % 4 )

/* TrueType table checksum: sum of big-endian uint32 words, mod 2^32 */
STATIC FUNCTION Checksum( c )

    LOCAL nSum := 0, i

    c := Pad4( c )
    FOR i := 1 TO hb_BLen( c ) STEP 4
        nSum := ( nSum + hb_BCode( hb_BSubStr( c, i, 1 ) ) * 16777216 + ;
                         hb_BCode( hb_BSubStr( c, i + 1, 1 ) ) * 65536 + ;
                         hb_BCode( hb_BSubStr( c, i + 2, 1 ) ) * 256 + ;
                         hb_BCode( hb_BSubStr( c, i + 3, 1 ) ) ) % 4294967296
    NEXT

RETURN nSum

/* UTF-16BE of an ASCII / Latin text (name table strings) */
STATIC FUNCTION Utf16BE( cText )

    LOCAL cOut := "", i

    FOR i := 1 TO Len( cText )
        cOut += U16( hb_UCode( SubStr( cText, i, 1 ) ) )
    NEXT

RETURN cOut

/*--------------------------------------------------------------- TrueType */

/* one rectangle contour per horizontal run of pixels, clockwise (TrueType
   outer contours): -> { { xLeft, yBottom, xRight, yTop }, ... } */
STATIC FUNCTION Runs( aRows )

    LOCAL aRects := {}, r, c, nStart, cRow

    FOR r := 1 TO 8
        cRow := aRows[ r ]
        c := 1
        DO WHILE c <= 8
            IF SubStr( cRow, c, 1 ) == "#"
                nStart := c
                DO WHILE c <= 8 .AND. SubStr( cRow, c, 1 ) == "#"
                    c++
                ENDDO
                AAdd( aRects, { ( nStart - 1 ) * PIXEL, ( 8 - r ) * PIXEL, ( c - 1 ) * PIXEL, ( 9 - r ) * PIXEL } )
            ELSE
                c++
            ENDIF
        ENDDO
    NEXT

RETURN aRects

/* glyf entry: simple glyph, every point on the curve, 2-byte coordinates */
STATIC FUNCTION GlyfData( aRects, aBox )

    LOCAL cEnd := "", cFlags := "", cX := "", cY := "", aR, nX := 0, nY := 0
    LOCAL aPts, aP, nPt := 0

    IF Len( aRects ) == 0
        aBox := { 0, 0, 0, 0 }
        RETURN ""
    ENDIF
    aBox := { 99999, 99999, -99999, -99999 }
    FOR EACH aR IN aRects
        /* (left,bottom) -> (left,top) -> (right,top) -> (right,bottom) */
        aPts := { { aR[ 1 ], aR[ 2 ] }, { aR[ 1 ], aR[ 4 ] }, { aR[ 3 ], aR[ 4 ] }, { aR[ 3 ], aR[ 2 ] } }
        FOR EACH aP IN aPts
            cFlags += hb_BChar( 1 )                  /* on curve; x, y as int16 deltas */
            cX += U16( aP[ 1 ] - nX )
            cY += U16( aP[ 2 ] - nY )
            nX := aP[ 1 ]
            nY := aP[ 2 ]
        NEXT
        nPt += 4
        cEnd += U16( nPt - 1 )
        aBox[ 1 ] := Min( aBox[ 1 ], aR[ 1 ] )
        aBox[ 2 ] := Min( aBox[ 2 ], aR[ 2 ] )
        aBox[ 3 ] := Max( aBox[ 3 ], aR[ 3 ] )
        aBox[ 4 ] := Max( aBox[ 4 ], aR[ 4 ] )
    NEXT

RETURN Pad4( U16( Len( aRects ) ) + U16( aBox[ 1 ] ) + U16( aBox[ 2 ] ) + U16( aBox[ 3 ] ) + U16( aBox[ 4 ] ) + ;
             cEnd + U16( 0 ) + cFlags + cX + cY )

/** Writes a TrueType font with every cell 1024 x 1024 units (8 x 8 pixels).
 * @param aCells { { nCode, aRows8 }, ... } (ht_glyphsCells)
 */
FUNCTION ht_pixelFontTTF( aCells, cFile, cFamily )

    LOCAL aGlyphs := {}, aCell, cGlyf := "", cLoca := "", cHmtx := "", aBox
    LOCAL nMaxPts := 0, nMaxCont := 0, aRects, cData, aBounds := { 0, 0, 0, 0 }
    LOCAL cCmap, cSeg, nSeg, i, nMin := 65535, nMax := 0
    LOCAL cName, aNames, nOff, cStrings
    LOCAL cPs := "", c
    LOCAL hTables := { => }, aTags, cFont, nTables, nSearch, nSelector, nOffset
    LOCAL cDir, cHead, nAdjust, n
    LOCAL aNotdef := { "########", "#......#", "#......#", "#......#", "#......#", "#......#", "#......#", "########" }

    /* glyph 0 = .notdef (a frame), then the cells in code order */
    AAdd( aGlyphs, { -1, aNotdef } )
    FOR EACH aCell IN aCells
        IF aCell[ 1 ] >= 0 .AND. aCell[ 1 ] <= 65535
            AAdd( aGlyphs, aCell )
        ENDIF
    NEXT

    /* glyf, loca (long offsets), hmtx */
    FOR EACH aCell IN aGlyphs
        aRects := Runs( aCell[ 2 ] )
        cData := GlyfData( aRects, @aBox )
        cLoca += U32( hb_BLen( cGlyf ) )
        cGlyf += cData
        cHmtx += U16( CELL ) + U16( aBox[ 1 ] )      /* advance, left side bearing = xMin */
        nMaxPts := Max( nMaxPts, Len( aRects ) * 4 )
        nMaxCont := Max( nMaxCont, Len( aRects ) )
        IF Len( aRects ) > 0
            aBounds[ 3 ] := Max( aBounds[ 3 ], aBox[ 3 ] )
            aBounds[ 4 ] := Max( aBounds[ 4 ], aBox[ 4 ] )
        ENDIF
        IF aCell[ 1 ] >= 0
            nMin := Min( nMin, aCell[ 1 ] )
            nMax := Max( nMax, aCell[ 1 ] )
        ENDIF
    NEXT
    cLoca += U32( hb_BLen( cGlyf ) )

    /* cmap: format 4, one segment per character + the closing 0xFFFF one */
    nSeg := Len( aGlyphs )                            /* (glyphs - .notdef) + 1 */
    n := 1
    DO WHILE n * 2 <= nSeg
        n *= 2
    ENDDO
    nSearch := n * 2
    nSelector := Int( Log( n ) / Log( 2 ) + 0.5 )
    cSeg := U16( 0x0004 ) + U16( 0 ) + U16( 0 ) + U16( nSeg * 2 ) + U16( nSearch ) + U16( nSelector ) + U16( nSeg * 2 - nSearch )
    FOR i := 2 TO Len( aGlyphs )
        cSeg += U16( aGlyphs[ i, 1 ] )                /* endCode */
    NEXT
    cSeg += U16( 0xFFFF ) + U16( 0 )                  /* + reservedPad */
    FOR i := 2 TO Len( aGlyphs )
        cSeg += U16( aGlyphs[ i, 1 ] )                /* startCode */
    NEXT
    cSeg += U16( 0xFFFF )
    FOR i := 2 TO Len( aGlyphs )
        cSeg += U16( ( i - 1 - aGlyphs[ i, 1 ] ) % 65536 )   /* idDelta: glyph = code + delta */
    NEXT
    cSeg += U16( 1 )
    cSeg += Replicate( U16( 0 ), nSeg )               /* idRangeOffset */
    cSeg := hb_BLeft( cSeg, 2 ) + U16( hb_BLen( cSeg ) ) + hb_BSubStr( cSeg, 5 )
    cCmap := U16( 0 ) + U16( 2 ) + ;
             U16( 0 ) + U16( 3 ) + U32( 20 ) + ;      /* Unicode BMP */
             U16( 3 ) + U16( 1 ) + U32( 20 ) + ;      /* Windows Unicode BMP */
             cSeg

    /* name: format 0, Windows Unicode English; GDI rejects a font without
       the full set (family, style, unique, full name, version, PostScript) */
    FOR EACH c IN cFamily
        IF IsAlpha( c ) .OR. IsDigit( c )
            cPs += c
        ENDIF
    NEXT
    cPs += "-Regular"
    aNames := { { 1, cFamily }, { 2, "Regular" }, { 3, cPs + ";1.000" }, ;
                { 4, cFamily + " Regular" }, { 5, "Version 1.000" }, { 6, cPs } }
    cName := U16( 0 ) + U16( Len( aNames ) ) + U16( 6 + 12 * Len( aNames ) )
    cStrings := ""
    FOR EACH c IN aNames
        nOff := hb_BLen( cStrings )
        cStrings += Utf16BE( c[ 2 ] )
        cName += U16( 3 ) + U16( 1 ) + U16( 0x0409 ) + U16( c[ 1 ] ) + U16( hb_BLen( cStrings ) - nOff ) + U16( nOff )
    NEXT
    cName += cStrings

    hTables[ "cmap" ] := cCmap
    hTables[ "glyf" ] := cGlyf
    hTables[ "loca" ] := cLoca
    hTables[ "hmtx" ] := cHmtx
    hTables[ "name" ] := cName
    hTables[ "head" ] := U32( 0x00010000 ) + U32( 0x00010000 ) + U32( 0 ) + U32( 0x5F0F3CF5 ) + ;
        U16( 0x000B ) + U16( CELL ) + ;
        U32( 0 ) + U32( 3846528000 ) + U32( 0 ) + U32( 3846528000 ) + ;   /* created, modified: fixed */
        U16( 0 ) + U16( 0 ) + U16( aBounds[ 3 ] ) + U16( aBounds[ 4 ] ) + ;
        U16( 0 ) + U16( 8 ) + U16( 2 ) + U16( 1 ) + U16( 0 )              /* macStyle, lowestPPEM, hint, long loca, format */
    hTables[ "hhea" ] := U32( 0x00010000 ) + U16( CELL ) + U16( 0 ) + U16( 0 ) + U16( CELL ) + ;
        U16( 0 ) + U16( 0 ) + U16( aBounds[ 3 ] ) + U16( 1 ) + U16( 0 ) + U16( 0 ) + ;
        U16( 0 ) + U16( 0 ) + U16( 0 ) + U16( 0 ) + U16( 0 ) + U16( Len( aGlyphs ) )
    hTables[ "maxp" ] := U32( 0x00010000 ) + U16( Len( aGlyphs ) ) + U16( nMaxPts ) + U16( nMaxCont ) + ;
        U16( 0 ) + U16( 0 ) + U16( 2 ) + Replicate( U16( 0 ), 8 )   /* maxp 1.0: 32 bytes */
    hTables[ "OS/2" ] := U16( 4 ) + U16( CELL ) + U16( 400 ) + U16( 5 ) + U16( 0 ) + ;
        U16( 650 ) + U16( 600 ) + U16( 0 ) + U16( 75 ) + ;                /* subscript */
        U16( 650 ) + U16( 600 ) + U16( 0 ) + U16( 350 ) + ;               /* superscript */
        U16( 50 ) + U16( 300 ) + U16( 0 ) + ;                             /* strikeout, family class */
        hb_BChar( 2 ) + hb_BChar( 0 ) + hb_BChar( 0 ) + hb_BChar( 9 ) + Replicate( hb_BChar( 0 ), 6 ) + ;  /* panose: monospaced */
        U32( 1 ) + U32( 0 ) + U32( 0 ) + U32( 0 ) + "HBTU" + ;
        U16( 0x0040 ) + U16( nMin ) + U16( nMax ) + ;
        U16( CELL ) + U16( 0 ) + U16( 0 ) + U16( CELL ) + U16( 0 ) + ;   /* typo, win ascent / descent */
        U32( 1 ) + U32( 0 ) + ;                                           /* code pages: Latin 1 */
        U16( 512 ) + U16( 768 ) + U16( 0 ) + U16( 32 ) + U16( 0 )
    hTables[ "post" ] := U32( 0x00030000 ) + U32( 0 ) + U16( -100 ) + U16( 50 ) + U32( 1 ) + ;
        U32( 0 ) + U32( 0 ) + U32( 0 ) + U32( 0 )

    /* sfnt directory: tags sorted, tables 4-byte aligned */
    /* tags in binary order ("OS/2" before "cmap"; a string sort would follow
       the codepage's collation) */
    aTags := { "OS/2", "cmap", "glyf", "head", "hhea", "hmtx", "loca", "maxp", "name", "post" }
    nTables := Len( aTags )
    n := 1
    DO WHILE n * 2 <= nTables
        n *= 2
    ENDDO
    cFont := U32( 0x00010000 ) + U16( nTables ) + U16( n * 16 ) + U16( Int( Log( n ) / Log( 2 ) + 0.5 ) ) + U16( nTables * 16 - n * 16 )
    nOffset := 12 + 16 * nTables
    cData := ""
    FOR EACH c IN aTags
        cFont += c + U32( Checksum( hTables[ c ] ) ) + U32( nOffset + hb_BLen( cData ) ) + U32( hb_BLen( hTables[ c ] ) )
        cData += Pad4( hTables[ c ] )
    NEXT
    cFont += cData

    /* head.checkSumAdjustment = 0xB1B0AFBA - checksum of the whole font */
    nAdjust := ( 0xB1B0AFBA - Checksum( cFont ) + 4294967296 ) % 4294967296
    n := hb_BAt( "head", hb_BLeft( cFont, 12 + 16 * nTables ) )   /* its directory entry */
    nOffset := hb_BCode( hb_BSubStr( cFont, n + 8, 1 ) ) * 16777216 + hb_BCode( hb_BSubStr( cFont, n + 9, 1 ) ) * 65536 + ;
               hb_BCode( hb_BSubStr( cFont, n + 10, 1 ) ) * 256 + hb_BCode( hb_BSubStr( cFont, n + 11, 1 ) )
    cHead := hb_BLeft( cFont, nOffset + 8 ) + U32( nAdjust ) + hb_BSubStr( cFont, nOffset + 13 )

    cDir := hb_FNameDir( cFile )
    IF ! cDir == "" .AND. ! hb_DirExists( cDir )
        hb_DirBuild( cDir )
    ENDIF
    /* a font loaded by a running program is locked: leave an unchanged one */
    IF hb_FileExists( cFile ) .AND. hb_MemoRead( cFile ) == cHead
        RETURN .T.
    ENDIF

RETURN hb_MemoWrit( cFile, cHead )

/*--------------------------------------------------------------- X11 BDF */

/** Writes X11 bitmap fonts (BDF) at the given cell sizes (multiples of 8:
 * every font pixel a whole square) and a fonts.dir for ht_fontLoad().
 * XLFD: -hbtui-<family>-medium-r-normal--<px>-*-*-*-c-*-iso10646-1
 */
FUNCTION ht_pixelFontX11( aCells, cDir, cFamily, aSizes )

    LOCAL cName := "", c, nPx, k, nBytes, cOut, aCell, cRow, cBits, i, cHex, cLine
    LOCAL aEntries := {}, cXlfd, cFileName

    hb_default( @aSizes, { 16, 24, 32, 40, 48 } )
    FOR EACH c IN cFamily
        IF IsAlpha( c ) .OR. IsDigit( c )
            cName += Lower( c )
        ENDIF
    NEXT
    cDir := hb_DirSepAdd( cDir )
    IF ! hb_DirExists( cDir )
        hb_DirBuild( cDir )
    ENDIF

    FOR EACH nPx IN aSizes
        k := Int( nPx / 8 )
        nBytes := Int( ( nPx + 7 ) / 8 )
        cXlfd := "-hbtui-" + cName + "-medium-r-normal--" + hb_ntos( nPx ) + "-" + hb_ntos( nPx * 10 ) + ;
                 "-75-75-c-" + hb_ntos( nPx * 10 ) + "-iso10646-1"
        cFileName := cName + "-" + hb_ntos( nPx ) + ".bdf"
        cOut := "STARTFONT 2.1" + LF + "FONT " + cXlfd + LF + "SIZE " + hb_ntos( nPx ) + " 75 75" + LF + ;
                "FONTBOUNDINGBOX " + hb_ntos( nPx ) + " " + hb_ntos( nPx ) + " 0 0" + LF + ;
                "STARTPROPERTIES 6" + LF + "FONT_ASCENT " + hb_ntos( nPx ) + LF + "FONT_DESCENT 0" + LF + ;
                "PIXEL_SIZE " + hb_ntos( nPx ) + LF + 'SPACING "C"' + LF + "AVERAGE_WIDTH " + hb_ntos( nPx * 10 ) + LF + ;
                "DEFAULT_CHAR 32" + LF + "ENDPROPERTIES" + LF + "CHARS " + hb_ntos( Len( aCells ) ) + LF
        FOR EACH aCell IN aCells
            cOut += "STARTCHAR uni" + hb_NumToHex( aCell[ 1 ], 4 ) + LF + "ENCODING " + hb_ntos( aCell[ 1 ] ) + LF + ;
                    "SWIDTH 1000 0" + LF + "DWIDTH " + hb_ntos( nPx ) + " 0" + LF + ;
                    "BBX " + hb_ntos( nPx ) + " " + hb_ntos( nPx ) + " 0 0" + LF + "BITMAP" + LF
            FOR EACH cRow IN aCell[ 2 ]
                cBits := ""
                FOR i := 1 TO 8
                    cBits += Replicate( IIF( SubStr( cRow, i, 1 ) == "#", "1", "0" ), k )
                NEXT
                cBits := PadR( cBits, nBytes * 8, "0" )
                cHex := ""
                FOR i := 1 TO nBytes * 8 STEP 4
                    cHex += SubStr( "0123456789ABCDEF", BinNibble( SubStr( cBits, i, 4 ) ) + 1, 1 )
                NEXT
                cLine := cHex + LF
                cOut += Replicate( cLine, k )
            NEXT
            cOut += "ENDCHAR" + LF
        NEXT
        cOut += "ENDFONT" + LF
        IF ! hb_MemoWrit( cDir + cFileName, cOut )
            RETURN .F.
        ENDIF
        AAdd( aEntries, cFileName + " " + cXlfd )
    NEXT

    cOut := hb_ntos( Len( aEntries ) ) + LF
    AEval( aEntries, {| c | cOut += c + LF } )

RETURN hb_MemoWrit( cDir + "fonts.dir", cOut )

STATIC FUNCTION BinNibble( c4 )
RETURN IIF( SubStr( c4, 1, 1 ) == "1", 8, 0 ) + IIF( SubStr( c4, 2, 1 ) == "1", 4, 0 ) + ;
       IIF( SubStr( c4, 3, 1 ) == "1", 2, 0 ) + IIF( SubStr( c4, 4, 1 ) == "1", 1, 0 )
