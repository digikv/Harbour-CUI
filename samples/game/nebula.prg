/** @file nebula.prg
 * NEBULA RAID - a fixed-screen shooter in text mode with its own font.
 *
 * Every picture on the screen is a character of nebula8.ttf: 8x8 pixel
 * cells (letters, digits and the sprites at U+E000..), 32 x 24 cells =
 * 256 x 192 pixels, a 16-color palette - the way 8-bit home computers did
 * it with a redefined character set. The font is loaded for this program
 * only (ht_fontLoad), keys are read as held down (ht_keyDown) so moving
 * and firing work together.
 *
 * Font: samples/game/nebula.glyphs -> ht_glyphsBuild() (fonteditor, tools/pixelfont) -> nebula8.ttf + x11/
 *
 * Keys:  Left/Right or Z/X move, Space or Up fire, P pause, Esc quit
 * Windows (gtwvt): nebula8.ttf. Linux (gtxwc): the X11 bitmap fonts in x11/,
 * added to the X server's font path by ht_fontLoad().
 */

#include "inkey.ch"
#include "hbgtinfo.ch"
#include "setcurs.ch"

REQUEST HB_CODEPAGE_UTF8EX

#define SCR_W         32
#define SCR_H         24
#define FRAME_MS      33              /* ~30 frames a second */

#define ROW_HUD       0
#define ROW_COMET     2
#define ROW_SHIELD    19              /* shields: rows 19 and 20 */
#define ROW_PLAYER    22
#define ROW_GROUND    23

#define ALIEN_ROWS    5
#define ALIEN_COLS    8

/* glyphs: offsets from U+E000 (see nebula.glyphs) */
#define G_SHIP        0x00
#define G_LIFE        0x02
#define G_MANTA       0x10
#define G_BEETLE      0x14
#define G_DRONE       0x18
#define G_BURST       0x1C
#define G_COMET       0x1E
#define G_SHOT_UP     0x20
#define G_SHOT_LOW    0x21
#define G_BOMB        0x22            /* +0/+1 frame a, +2/+3 frame b */
#define G_SHIELD      0x26            /* +0..+3 wear */
#define G_SHIELD_TL   0x2A
#define G_SHIELD_TR   0x2B
#define G_GROUND      0x2C
#define G_WRECK       0x30

/* game state */
STATIC s_nScore, s_nHi := 0, s_nLives, s_nLevel, s_nFrame
STATIC s_aAlive, s_nFx, s_nFy, s_nDir, s_nStepWait, s_nAnim
STATIC s_nPx, s_nMoveWait
STATIC s_lShot, s_nShotCol, s_nShotHy        /* Hy: half rows */
STATIC s_aBombs                              /* { nCol, nHy } */
STATIC s_aBursts                             /* { nCol, nRow, nTtl, cText } */
STATIC s_aShield                             /* [ row ][ col ]: 0 none, 1..4 wear */
STATIC s_lComet, s_nCometCol, s_nCometDir, s_nCometWait, s_nCometNext
STATIC s_nDead                               /* frames left of the player's wreck */
STATIC s_nBanner                             /* frames left of "WAVE n" */
STATIC s_lKeyState
STATIC s_nKeyLeft, s_nKeyRight, s_nKeyFire   /* Inkey fallback: frames a key counts */

PROCEDURE Main()

    LOCAL lX11 := hb_gtVersion() == "XWC"
    /* Windows: a TrueType font; X11 (gtxwc): a folder of bitmap fonts */
    LOCAL cFont := hb_DirBase() + IIF( lX11, "x11", "nebula8.ttf" )

    hb_cdpSelect( "UTF8EX" )

    IF ! ht_fontLoad( cFont )
        ? "Cannot load " + cFont
        ? "Build it with samples/fonteditor (Build) or tools/pixelfont nebula.glyphs"
        Inkey( 0 )
        RETURN
    ENDIF

    SetupScreen()
    s_lKeyState := ht_keyStateAvailable()

    DO WHILE TitleScreen()
        PlayGame()
    ENDDO

RETURN

/*--------------------------------------------------------------- setup */

STATIC PROCEDURE SetupScreen()

    LOCAL nDeskW := hb_gtInfo( HB_GTI_DESKTOPWIDTH )
    LOCAL nDeskH := hb_gtInfo( HB_GTI_DESKTOPHEIGHT )
    LOCAL nCell

    /* square cells, a multiple of 8 pixels (whole screen pixels per font
       pixel: crisp), the window about 85% of the desktop height */
    nCell := 24
    IF HB_ISNUMERIC( nDeskW ) .AND. HB_ISNUMERIC( nDeskH ) .AND. nDeskH > 0
        nCell := Max( 16, Int( Min( nDeskW * 0.9 / SCR_W, nDeskH * 0.85 / SCR_H ) / 8 ) * 8 )
    ENDIF

    hb_gtInfo( HB_GTI_WINTITLE, "NEBULA RAID" )   /* also how ht_keyDown() finds the window on X11 */
    IF hb_gtVersion() == "XWC"
        /* X11: bitmap fonts made for 16, 24, 32, 40 and 48 pixel cells; a
           resized gtxwc window would change the grid, so it stays fixed */
        nCell := Max( 16, Min( 48, nCell ) )
        hb_gtInfo( HB_GTI_FONTSEL, "-hbtui-nebula8-medium-r-normal--" + hb_ntos( nCell ) + "-*-*-*-c-*-iso10646-1" )
        hb_gtInfo( HB_GTI_RESIZABLE, .F. )
    ELSE
        hb_gtInfo( HB_GTI_CODEPAGE, 1 )           /* DEFAULT_CHARSET: take the font as it is */
        hb_gtInfo( HB_GTI_FONTNAME, "Nebula8" )
        hb_gtInfo( HB_GTI_FONTWIDTH, nCell )
        hb_gtInfo( HB_GTI_FONTSIZE, nCell )
        hb_gtInfo( HB_GTI_FONTQUALITY, HB_GTI_FONTQ_NORMAL )
        hb_gtInfo( HB_GTI_RESIZEMODE, HB_GTI_RESIZEMODE_FONT )
        hb_gtInfo( HB_GTI_RESIZABLE, .T. )
        hb_gtInfo( HB_GTI_ALTENTER, .T. )
    ENDIF
    SetMode( SCR_H, SCR_W )
    SetCursor( SC_NONE )
    SetBlink( .F. )

    /* an 8-bit home computer palette: plain colors at 84%, bright at 100%
       (GT order: black blue green cyan red magenta yellow white, then bright) */
    hb_gtInfo( HB_GTI_PALETTE, { ;
        Rgb(   0,   0,   0 ), Rgb(   0,   0, 215 ), Rgb(   0, 215,   0 ), Rgb(   0, 215, 215 ), ;
        Rgb( 215,   0,   0 ), Rgb( 215,   0, 215 ), Rgb( 215, 215,   0 ), Rgb( 215, 215, 215 ), ;
        Rgb(  40,  40,  40 ), Rgb(   0,   0, 255 ), Rgb(   0, 255,   0 ), Rgb(   0, 255, 255 ), ;
        Rgb( 255,   0,   0 ), Rgb( 255,   0, 255 ), Rgb( 255, 255,   0 ), Rgb( 255, 255, 255 ) } )

RETURN

STATIC FUNCTION Rgb( nR, nG, nB )
RETURN nR + nG * 256 + nB * 65536

/* glyph nOffset (from U+E000), nCells wide */
STATIC FUNCTION G( nOffset, nCells )

    LOCAL c := "", i

    FOR i := 0 TO hb_defaultValue( nCells, 1 ) - 1
        c += hb_UChar( 0xE000 + nOffset + i )
    NEXT

RETURN c

STATIC PROCEDURE Put( nRow, nCol, cText, cColor )

    IF nRow >= 0 .AND. nRow < SCR_H .AND. nCol >= 0 .AND. nCol < SCR_W
        DispOutAt( nRow, nCol, Left( cText, SCR_W - nCol ), cColor )
    ENDIF

RETURN

STATIC PROCEDURE PutC( nRow, cText, cColor )
    Put( nRow, Int( ( SCR_W - Len( cText ) ) / 2 ), cText, cColor )
RETURN

/*--------------------------------------------------------------- input */

/* keys for this frame: { lLeft, lRight, lFire, nInkey } */
STATIC FUNCTION ReadKeys()

    LOCAL nKey, nLast := 0

    /* drain the keyboard buffer (Esc, P, Space on the title; fallback moves) */
    DO WHILE ( nKey := Inkey() ) != 0
        nLast := nKey
        DO CASE
        CASE nKey == K_LEFT .OR. Upper( hb_keyChar( nKey ) ) == "Z"
            s_nKeyLeft := 6 ; s_nKeyRight := 0
        CASE nKey == K_RIGHT .OR. Upper( hb_keyChar( nKey ) ) == "X"
            s_nKeyRight := 6 ; s_nKeyLeft := 0
        CASE nKey == K_SPACE .OR. nKey == K_UP
            s_nKeyFire := 2
        ENDCASE
    ENDDO

    IF s_lKeyState
        /* held keys: VK_LEFT 0x25, VK_RIGHT 0x27, VK_UP 0x26, VK_SPACE 0x20 */
        RETURN { ht_keyDown( 0x25 ) .OR. ht_keyDown( Asc( "Z" ) ), ;
                 ht_keyDown( 0x27 ) .OR. ht_keyDown( Asc( "X" ) ), ;
                 ht_keyDown( 0x20 ) .OR. ht_keyDown( 0x26 ), nLast }
    ENDIF

    /* no key state (outside Windows): a pressed key counts for a few frames */
RETURN { s_nKeyLeft-- > 0, s_nKeyRight-- > 0, s_nKeyFire-- > 0, nLast }

/* waits for the rest of the frame (Inkey() keeps the window responsive) */
STATIC PROCEDURE FrameWait( nStart )

    LOCAL nLeft := FRAME_MS - ( hb_MilliSeconds() - nStart )

    IF nLeft > 0
        hb_idleSleep( nLeft / 1000 )
    ENDIF

RETURN

/*--------------------------------------------------------------- title */

/* .T. = play, .F. = quit */
STATIC FUNCTION TitleScreen()

    LOCAL nStart, aKeys, n := 0

    s_nKeyLeft := s_nKeyRight := s_nKeyFire := 0
    hb_keyClear()

    DO WHILE .T.
        nStart := hb_MilliSeconds()
        aKeys := ReadKeys()
        IF aKeys[ 4 ] == K_ESC
            RETURN .F.
        ELSEIF aKeys[ 4 ] == K_SPACE .OR. aKeys[ 4 ] == K_ENTER
            RETURN .T.
        ENDIF

        DispBegin()
        CLS
        PutC( 2, "NEBULA RAID", "GR+/N" )
        PutC( 3, "-----------", "R+/N" )

        Put(  6, 9, G( G_MANTA + IIF( n % 30 < 15, 0, 2 ), 2 ) + " = 30", "RB+/N" )
        Put(  8, 9, G( G_BEETLE + IIF( n % 30 < 15, 0, 2 ), 2 ) + " = 20", "BG+/N" )
        Put( 10, 9, G( G_DRONE + IIF( n % 30 < 15, 0, 2 ), 2 ) + " = 10", "G+/N" )
        Put( 12, 9, G( G_COMET, 2 ) + " = ???", "R+/N" )

        PutC( 15, "Z X OR ARROWS  MOVE", "W/N" )
        PutC( 16, "SPACE  FIRE  P  PAUSE", "W/N" )
        IF n % 40 < 26
            PutC( 19, "PRESS SPACE TO START", "W+/N" )
        ENDIF
        PutC( 22, "HI " + StrZero( s_nHi, 5 ), "GR+/N" )
        Put( ROW_GROUND, 0, Replicate( G( G_GROUND ), SCR_W ), "G/N" )
        DispEnd()

        n++
        FrameWait( nStart )
    ENDDO

RETURN .F.

/*--------------------------------------------------------------- game */

STATIC PROCEDURE PlayGame()

    LOCAL nStart, aKeys, lPaused := .F.

    s_nScore := 0
    s_nLives := 3
    s_nLevel := 0
    s_nFrame := 0
    s_nKeyLeft := s_nKeyRight := s_nKeyFire := 0
    NewShields()
    NewWave()

    DO WHILE .T.
        nStart := hb_MilliSeconds()
        aKeys := ReadKeys()

        IF aKeys[ 4 ] == K_ESC
            EXIT
        ELSEIF Upper( hb_keyChar( aKeys[ 4 ] ) ) == "P"
            lPaused := ! lPaused
        ENDIF

        IF ! lPaused
            s_nFrame++
            IF ! Update( aKeys )
                Draw( .F. )
                GameOver()
                EXIT
            ENDIF
        ENDIF
        Draw( lPaused )

        FrameWait( nStart )
    ENDDO

    s_nHi := Max( s_nHi, s_nScore )

RETURN

STATIC PROCEDURE NewWave()

    LOCAL r, c

    s_nLevel++
    s_aAlive := Array( ALIEN_ROWS, ALIEN_COLS )
    FOR r := 1 TO ALIEN_ROWS
        FOR c := 1 TO ALIEN_COLS
            s_aAlive[ r, c ] := .T.
        NEXT
    NEXT
    s_nFx := 1
    s_nFy := 4 + Min( s_nLevel - 1, 4 )          /* each wave starts lower */
    s_nDir := 1
    s_nStepWait := 0
    s_nAnim := 0
    s_nPx := Int( SCR_W / 2 ) - 1
    s_nMoveWait := 0
    s_lShot := .F.
    s_aBombs := {}
    s_aBursts := {}
    s_lComet := .F.
    s_nCometNext := 400 + hb_RandomInt( 0, 300 )
    s_nDead := 0
    s_nBanner := 60

RETURN

STATIC PROCEDURE NewShields()

    LOCAL nB, nCol, nRow

    s_aShield := Array( SCR_H, SCR_W )
    AEval( s_aShield, {| a | AFill( a, 0 ) } )
    /* four bunkers, 4 cells wide, 2 rows high */
    FOR nB := 0 TO 3
        nCol := 2 + nB * 8
        FOR nRow := ROW_SHIELD TO ROW_SHIELD + 1
            AFill( s_aShield[ nRow + 1 ], 1, nCol + 1, 4 )
        NEXT
    NEXT

RETURN

/* screen position of alien r, c (1-based) */
STATIC FUNCTION AlienRow( r )
RETURN s_nFy + ( r - 1 ) * 2

STATIC FUNCTION AlienCol( c )
RETURN s_nFx + ( c - 1 ) * 3

STATIC FUNCTION AliensLeft()

    LOCAL n := 0

    AEval( s_aAlive, {| a | AEval( a, {| l | IIF( l, n++, NIL ) } ) } )

RETURN n

/* one frame of game logic; .F. = game over */
STATIC FUNCTION Update( aKeys )

    IF s_nBanner > 0
        s_nBanner--
    ENDIF
    AgeBursts()

    IF s_nDead > 0
        IF --s_nDead == 0
            IF s_nLives <= 0
                RETURN .F.
            ENDIF
            s_nPx := Int( SCR_W / 2 ) - 1
            s_aBombs := {}
        ENDIF
    ELSE
        MovePlayer( aKeys )
    ENDIF

    MoveShot()
    IF ! MoveAliens()
        RETURN .F.                         /* they landed */
    ENDIF
    DropBombs()
    IF ! MoveBombs()
        RETURN .F.
    ENDIF
    MoveComet()

    IF AliensLeft() == 0 .AND. Len( s_aBursts ) == 0
        NewWave()
    ENDIF

RETURN .T.

STATIC PROCEDURE MovePlayer( aKeys )

    IF s_nMoveWait > 0
        s_nMoveWait--
    ELSEIF aKeys[ 1 ] .AND. ! aKeys[ 2 ] .AND. s_nPx > 0
        s_nPx--
        s_nMoveWait := 1
    ELSEIF aKeys[ 2 ] .AND. ! aKeys[ 1 ] .AND. s_nPx < SCR_W - 2
        s_nPx++
        s_nMoveWait := 1
    ENDIF

    IF aKeys[ 3 ] .AND. ! s_lShot
        s_lShot := .T.
        s_nShotCol := s_nPx                  /* the shot glyph sits at the cell's right edge */
        s_nShotHy := ROW_PLAYER * 2 - 1
    ENDIF

RETURN

STATIC PROCEDURE MoveShot()

    LOCAL nStep, nRow, r, c, nPts

    IF ! s_lShot
        RETURN
    ENDIF

    FOR nStep := 1 TO 2                      /* two half rows a frame */
        s_nShotHy--
        nRow := Int( s_nShotHy / 2 )
        IF nRow <= ROW_COMET - 1
            s_lShot := .F.
            RETURN
        ENDIF

        /* comet */
        IF s_lComet .AND. nRow == ROW_COMET .AND. ( s_nShotCol == s_nCometCol .OR. s_nShotCol == s_nCometCol + 1 )
            nPts := { 50, 100, 150, 300 }[ hb_RandomInt( 1, 4 ) ]
            s_nScore += nPts
            s_lComet := .F.
            AAdd( s_aBursts, { s_nCometCol, ROW_COMET, 45, hb_ntos( nPts ) } )
            s_lShot := .F.
            RETURN
        ENDIF

        /* aliens: 2 cells wide; the shot is at the border of its cell and the next */
        FOR r := 1 TO ALIEN_ROWS
            IF AlienRow( r ) == nRow
                FOR c := 1 TO ALIEN_COLS
                    IF s_aAlive[ r, c ] .AND. s_nShotCol == AlienCol( c )
                        s_aAlive[ r, c ] := .F.
                        s_nScore += { 30, 20, 20, 10, 10 }[ r ]
                        AAdd( s_aBursts, { AlienCol( c ), nRow, 8, NIL } )
                        s_lShot := .F.
                        RETURN
                    ENDIF
                NEXT
            ENDIF
        NEXT

        /* shields */
        IF HitShield( nRow, s_nShotCol )
            s_lShot := .F.
            RETURN
        ENDIF

        /* bombs */
        FOR r := Len( s_aBombs ) TO 1 STEP -1
            IF s_aBombs[ r, 1 ] == s_nShotCol .AND. Abs( s_aBombs[ r, 2 ] - s_nShotHy ) <= 1
                hb_ADel( s_aBombs, r, .T. )
                s_lShot := .F.
                RETURN
            ENDIF
        NEXT
    NEXT

RETURN

/* wears the shield cell at nRow, nCol away; .T. when there was one */
STATIC FUNCTION HitShield( nRow, nCol )

    IF nRow < 0 .OR. nRow >= SCR_H .OR. nCol < 0 .OR. nCol >= SCR_W
        RETURN .F.
    ENDIF
    IF s_aShield[ nRow + 1, nCol + 1 ] > 0
        IF ++s_aShield[ nRow + 1, nCol + 1 ] > 4
            s_aShield[ nRow + 1, nCol + 1 ] := 0
        ENDIF
        RETURN .T.
    ENDIF

RETURN .F.

/* .F. when the formation reached the player's row */
STATIC FUNCTION MoveAliens()

    LOCAL nLeft := AliensLeft()
    LOCAL r, c, nMinC := 99, nMaxC := 0, nMaxR := 0

    IF nLeft == 0
        RETURN .T.
    ENDIF
    IF s_nStepWait > 0
        s_nStepWait--
        RETURN .T.
    ENDIF
    /* fewer aliens, faster steps; later waves faster */
    s_nStepWait := Max( 0, Int( nLeft / 2 ) - s_nLevel )
    s_nAnim := 1 - s_nAnim

    FOR r := 1 TO ALIEN_ROWS
        FOR c := 1 TO ALIEN_COLS
            IF s_aAlive[ r, c ]
                nMinC := Min( nMinC, c )
                nMaxC := Max( nMaxC, c )
                nMaxR := Max( nMaxR, r )
            ENDIF
        NEXT
    NEXT

    IF ( s_nDir > 0 .AND. AlienCol( nMaxC ) + 1 >= SCR_W - 1 ) .OR. ;
       ( s_nDir < 0 .AND. AlienCol( nMinC ) <= 0 )
        s_nDir := -s_nDir
        s_nFy++
        /* aliens crush the shields they reach */
        IF AlienRow( nMaxR ) >= ROW_SHIELD
            FOR c := 0 TO SCR_W - 1
                s_aShield[ AlienRow( nMaxR ) + 1, c + 1 ] := 0
            NEXT
        ENDIF
        IF AlienRow( nMaxR ) >= ROW_PLAYER
            RETURN .F.
        ENDIF
    ELSE
        s_nFx += s_nDir
    ENDIF

RETURN .T.

STATIC PROCEDURE DropBombs()

    LOCAL aCols := {}, r, c, nPick

    IF s_nDead > 0 .OR. Len( s_aBombs ) >= 2 + Int( s_nLevel / 2 ) .OR. ;
       hb_RandomInt( 1, 100 ) > 4 + s_nLevel
        RETURN
    ENDIF

    /* the lowest alien of a random column drops it */
    FOR c := 1 TO ALIEN_COLS
        FOR r := ALIEN_ROWS TO 1 STEP -1
            IF s_aAlive[ r, c ]
                AAdd( aCols, { r, c } )
                EXIT
            ENDIF
        NEXT
    NEXT
    IF Len( aCols ) > 0
        nPick := hb_RandomInt( 1, Len( aCols ) )
        AAdd( s_aBombs, { AlienCol( aCols[ nPick, 2 ] ), ( AlienRow( aCols[ nPick, 1 ] ) + 1 ) * 2 } )
    ENDIF

RETURN

/* .F. when the last life is gone (after the wreck has been shown) */
STATIC FUNCTION MoveBombs()

    LOCAL i, nRow, nCol

    FOR i := Len( s_aBombs ) TO 1 STEP -1
        s_aBombs[ i, 2 ]++
        nRow := Int( s_aBombs[ i, 2 ] / 2 )
        nCol := s_aBombs[ i, 1 ]

        IF nRow >= ROW_GROUND
            hb_ADel( s_aBombs, i, .T. )
        ELSEIF HitShield( nRow, nCol )
            hb_ADel( s_aBombs, i, .T. )
        ELSEIF nRow == ROW_PLAYER .AND. s_nDead == 0 .AND. ( nCol == s_nPx .OR. nCol == s_nPx + 1 )
            hb_ADel( s_aBombs, i, .T. )
            s_nLives--
            s_nDead := 45
            s_lShot := .F.
        ENDIF
    NEXT

RETURN .T.

STATIC PROCEDURE MoveComet()

    IF ! s_lComet
        IF --s_nCometNext <= 0
            s_lComet := .T.
            s_nCometDir := IIF( hb_RandomInt( 0, 1 ) == 0, 1, -1 )
            s_nCometCol := IIF( s_nCometDir > 0, -2, SCR_W )
            s_nCometWait := 0
            s_nCometNext := 500 + hb_RandomInt( 0, 400 )
        ENDIF
        RETURN
    ENDIF

    IF s_nCometWait > 0
        s_nCometWait--
        RETURN
    ENDIF
    s_nCometWait := 2
    s_nCometCol += s_nCometDir
    IF s_nCometCol < -2 .OR. s_nCometCol > SCR_W
        s_lComet := .F.
    ENDIF

RETURN

STATIC PROCEDURE AgeBursts()

    LOCAL i

    FOR i := Len( s_aBursts ) TO 1 STEP -1
        IF --s_aBursts[ i, 3 ] <= 0
            hb_ADel( s_aBursts, i, .T. )
        ENDIF
    NEXT

RETURN

/*--------------------------------------------------------------- drawing */

STATIC PROCEDURE Draw( lPaused )

    LOCAL r, c, nRow, nCol, n, aB, nGlyph
    LOCAL aRowColor := { "RB+/N", "BG+/N", "BG+/N", "G+/N", "G+/N" }
    LOCAL aRowGlyph := { G_MANTA, G_BEETLE, G_BEETLE, G_DRONE, G_DRONE }

    DispBegin()
    CLS

    /* score line */
    Put( ROW_HUD, 0, "SCORE", "W/N" )
    Put( ROW_HUD, 6, StrZero( s_nScore, 5 ), "GR+/N" )
    Put( ROW_HUD, 13, "HI", "W/N" )
    Put( ROW_HUD, 16, StrZero( Max( s_nHi, s_nScore ), 5 ), "GR+/N" )
    FOR n := 1 TO Min( s_nLives - IIF( s_nDead > 0, 0, 1 ), 5 )
        Put( ROW_HUD, SCR_W - n, G( G_LIFE ), "W+/N" )
    NEXT

    /* comet */
    IF s_lComet
        FOR n := 0 TO 1
            Put( ROW_COMET, s_nCometCol + n, G( G_COMET + n ), "R+/N" )
        NEXT
    ENDIF

    /* aliens */
    FOR r := 1 TO ALIEN_ROWS
        nRow := AlienRow( r )
        FOR c := 1 TO ALIEN_COLS
            IF s_aAlive[ r, c ]
                Put( nRow, AlienCol( c ), G( aRowGlyph[ r ] + s_nAnim * 2, 2 ), aRowColor[ r ] )
            ENDIF
        NEXT
    NEXT

    /* shields */
    FOR nRow := ROW_SHIELD TO ROW_SHIELD + 1
        FOR nCol := 0 TO SCR_W - 1
            n := s_aShield[ nRow + 1, nCol + 1 ]
            IF n > 0
                nGlyph := G_SHIELD + n - 1
                /* untouched top corners are rounded */
                IF n == 1 .AND. nRow == ROW_SHIELD
                    IF nCol % 8 == 2                 /* bunkers: columns 2..5, 10..13, ... */
                        nGlyph := G_SHIELD_TL
                    ELSEIF nCol % 8 == 5
                        nGlyph := G_SHIELD_TR
                    ENDIF
                ENDIF
                Put( nRow, nCol, G( nGlyph ), "G/N" )
            ENDIF
        NEXT
    NEXT

    /* bursts and bonus points */
    FOR EACH aB IN s_aBursts
        IF aB[ 4 ] == NIL
            Put( aB[ 2 ], aB[ 1 ], G( G_BURST, 2 ), "GR+/N" )
        ELSE
            Put( aB[ 2 ], aB[ 1 ], aB[ 4 ], "R+/N" )
        ENDIF
    NEXT

    /* bombs (two zigzag frames) and the shot: upper / lower half of a cell */
    FOR EACH aB IN s_aBombs
        Put( Int( aB[ 2 ] / 2 ), aB[ 1 ], G( G_BOMB + IIF( s_nFrame % 8 < 4, 0, 2 ) + aB[ 2 ] % 2 ), "W+/N" )
    NEXT
    IF s_lShot
        Put( Int( s_nShotHy / 2 ), s_nShotCol, G( IIF( s_nShotHy % 2 == 0, G_SHOT_UP, G_SHOT_LOW ) ), "W+/N" )
    ENDIF

    /* player */
    IF s_nDead > 0
        Put( ROW_PLAYER, s_nPx, G( G_WRECK, 2 ), IIF( s_nDead % 6 < 3, "R+/N", "GR+/N" ) )
    ELSE
        Put( ROW_PLAYER, s_nPx, G( G_SHIP, 2 ), "W+/N" )
    ENDIF

    Put( ROW_GROUND, 0, Replicate( G( G_GROUND ), SCR_W ), "G/N" )

    IF s_nBanner > 0
        PutC( 15, "WAVE " + hb_ntos( s_nLevel ), "W+/N" )
    ENDIF
    IF lPaused
        PutC( 15, "PAUSED", "GR+/N" )
    ENDIF

    DispEnd()

RETURN

STATIC PROCEDURE GameOver()

    LOCAL n, nStart

    FOR n := 1 TO 90                         /* 3 seconds */
        nStart := hb_MilliSeconds()
        DispBegin()
        PutC( 13, "            ", "W/N" )
        PutC( 14, " GAME  OVER ", IIF( n % 20 < 14, "R+/N", "W/N" ) )
        PutC( 15, "            ", "W/N" )
        DispEnd()
        IF Inkey() == K_ESC
            EXIT
        ENDIF
        FrameWait( nStart )
    NEXT
    s_nHi := Max( s_nHi, s_nScore )

RETURN
