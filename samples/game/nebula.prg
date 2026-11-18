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
 * Keys:  Left/Right or Z/X move, Space or Up fire, P pause, S sound, Esc quit
 * Windows (gtwvt): nebula8.ttf. Linux (gtxwc): the X11 bitmap fonts in x11/,
 * added to the X server's font path by ht_fontLoad().
 *
 * Sound: square waves and noise made at start as 8-bit WAV images, played
 * without waiting (Windows: PlaySound from memory; elsewhere aplay, when
 * installed) - Tone() would stop the game for every beep.
 * High scores: nebula.hi next to the program (a read-only folder: in the
 * user's folder - %APPDATA%\nebula.hi, ~/.nebula.hi).
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

#define EXTRA_LIFE    5000            /* a life every 5000 points */
#define MAX_LIVES     6
#define DIVE_POINTS   60              /* a manta shot while diving */
#define HISCORES      5

#define SND_RATE      22050
#define SND_SHOT      1
#define SND_HIT       2
#define SND_ZAP       3               /* the shot meets a bomb */
#define SND_COMET     4
#define SND_UFO       5               /* comet passing */
#define SND_DIVE      6
#define SND_DEATH     7
#define SND_LIFE      8
#define SND_WAVE      9
#define SND_OVER      10
#define SND_BEAT      11              /* +0..+3: the formation's steps */

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
#define G_STAR        0x32
#define G_STAR_BRIGHT 0x33

/* game state */
STATIC s_nScore, s_nHi := 0, s_nLives, s_nLevel, s_nFrame, s_nNextLife
STATIC s_aAlive, s_nFx, s_nFy, s_nDir, s_nStepWait, s_nAnim, s_nBeat := 0
STATIC s_aDivers                             /* { r, c, nCol, nRow, lReturn, nWait, nSteps } */
STATIC s_nPx, s_nMoveWait, s_nHeld           /* s_nHeld: frames held, < 0 left */
STATIC s_lShot, s_nShotCol, s_nShotHy        /* Hy: half rows */
STATIC s_aBombs                              /* { nCol, nHy } */
STATIC s_aBursts                             /* { nCol, nRow, nTtl, cText } */
STATIC s_aShield                             /* [ row ][ col ]: 0 none, 1..4 wear */
STATIC s_lComet, s_nCometCol, s_nCometDir, s_nCometWait, s_nCometNext
STATIC s_nDead                               /* frames left of the player's wreck */
STATIC s_nBanner, s_cBanner, s_cBanner2      /* frames left of "WAVE n", "EXTRA LIFE", ... */
STATIC s_lKeyState
STATIC s_nKeyLeft, s_nKeyRight, s_nKeyFire   /* Inkey fallback: frames a key counts */
STATIC s_aStars, s_nStarTick := 0            /* { nCol, nRow, nWait, lNear } */
STATIC s_aHiScores, s_nLastRank := 0         /* { nScore, cInitials }, best first */
STATIC s_aSounds := {}, s_lSound := .T.      /* { nPriority, cWav, nMs, cFile } */
STATIC s_nSndPrio := 0, s_nSndEnd := 0, s_hSndProc

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
    LoadHiScores()
    InitSound()
    NewStars()

    DO WHILE TitleScreen()
        PlayGame()
    ENDDO

    DoneSound()

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
        ht_rgb(   0,   0,   0 ), ht_rgb(   0,   0, 215 ), ht_rgb(   0, 215,   0 ), ht_rgb(   0, 215, 215 ), ;
        ht_rgb( 215,   0,   0 ), ht_rgb( 215,   0, 215 ), ht_rgb( 215, 215,   0 ), ht_rgb( 215, 215, 215 ), ;
        ht_rgb(  40,  40,  40 ), ht_rgb(   0,   0, 255 ), ht_rgb(   0, 255,   0 ), ht_rgb(   0, 255, 255 ), ;
        ht_rgb( 255,   0,   0 ), ht_rgb( 255,   0, 255 ), ht_rgb( 255, 255,   0 ), ht_rgb( 255, 255, 255 ) } )

RETURN

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

    /* drain the keyboard buffer (Esc, P, Space on the title; fallback moves).
       Fallback: a single press moves one cell; a key still counting when the
       next one comes is held down (auto-repeat) and counts until the next
       repeat is due, so the ship stops soon after the key is let go */
    DO WHILE ( nKey := Inkey() ) != 0
        nLast := nKey
        DO CASE
        CASE nKey == K_LEFT .OR. Upper( hb_keyChar( nKey ) ) == "Z"
            s_nKeyLeft := IIF( s_nKeyLeft > 0, 5, 2 ) ; s_nKeyRight := 0
        CASE nKey == K_RIGHT .OR. Upper( hb_keyChar( nKey ) ) == "X"
            s_nKeyRight := IIF( s_nKeyRight > 0, 5, 2 ) ; s_nKeyLeft := 0
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

    /* no key state (ht_keyStateAvailable() .F.: not gtwvt / gtxwc, or the
       window has no focus API): a pressed key counts for a few frames */
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

    /* after a new high score the table comes first */
    LOCAL nStart, aKeys, n := IIF( s_nLastRank > 0, 180, 0 ), i

    s_nKeyLeft := s_nKeyRight := s_nKeyFire := 0
    hb_keyClear()

    DO WHILE .T.
        nStart := hb_MilliSeconds()
        aKeys := ReadKeys()
        IF aKeys[ 4 ] == K_ESC
            RETURN .F.
        ELSEIF aKeys[ 4 ] == K_SPACE .OR. aKeys[ 4 ] == K_ENTER
            RETURN .T.
        ELSEIF Upper( hb_keyChar( aKeys[ 4 ] ) ) == "S"
            ToggleSound()
        ENDIF
        MoveStars()

        DispBegin()
        CLS
        DrawStars()
        PutC( 2, "NEBULA RAID", "GR+/N" )
        PutC( 3, "-----------", "R+/N" )

        IF Int( n / 180 ) % 2 == 0           /* 6 seconds each: points, high scores */
            Put(  6, 9, G( G_MANTA + IIF( n % 30 < 15, 0, 2 ), 2 ) + " = 30", "RB+/N" )
            Put(  8, 9, G( G_BEETLE + IIF( n % 30 < 15, 0, 2 ), 2 ) + " = 20", "BG+/N" )
            Put( 10, 9, G( G_DRONE + IIF( n % 30 < 15, 0, 2 ), 2 ) + " = 10", "G+/N" )
            Put( 12, 9, G( G_COMET, 2 ) + " = ???", "R+/N" )
        ELSE
            PutC( 6, "HIGH SCORES", "W+/N" )
            FOR i := 1 TO Len( s_aHiScores )
                Put( 7 + i, 9, hb_ntos( i ) + "  " + s_aHiScores[ i, 2 ] + Str( s_aHiScores[ i, 1 ], 8 ), ;
                     IIF( i == s_nLastRank, IIF( n % 20 < 14, "GR+/N", "W/N" ), IIF( i == 1, "GR+/N", "W/N" ) ) )
            NEXT
        ENDIF

        PutC( 15, "Z X OR ARROWS  MOVE", "W/N" )
        PutC( 16, "SPACE  FIRE  P  PAUSE", "W/N" )
        PutC( 17, "S  SOUND " + IIF( s_lSound, "ON ", "OFF" ), "W/N" )
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

    LOCAL nStart, aKeys, lPaused := .F., lQuit := .F.

    s_nScore := 0
    s_nLives := 3
    s_nLevel := 0
    s_nFrame := 0
    s_nNextLife := EXTRA_LIFE
    s_nLastRank := 0
    s_nKeyLeft := s_nKeyRight := s_nKeyFire := 0
    NewShields()
    NewWave()

    DO WHILE .T.
        nStart := hb_MilliSeconds()
        aKeys := ReadKeys()

        IF aKeys[ 4 ] == K_ESC                 /* a game left with Esc does not count */
            lQuit := .T.
            EXIT
        ELSEIF Upper( hb_keyChar( aKeys[ 4 ] ) ) == "P"
            lPaused := ! lPaused
        ELSEIF Upper( hb_keyChar( aKeys[ 4 ] ) ) == "S"
            ToggleSound()
            Banner( "SOUND " + IIF( s_lSound, "ON", "OFF" ), "", 45 )
        ENDIF

        IF ! lPaused
            s_nFrame++
            MoveStars()
            IF ! Update( aKeys )
                Draw( .F. )
                GameOver()
                EXIT
            ENDIF
        ENDIF
        Draw( lPaused )

        FrameWait( nStart )
    ENDDO

    IF ! lQuit .AND. IsHiScore( s_nScore )
        s_nLastRank := AddHiScore( s_nScore, EnterInitials( s_nScore ) )
    ENDIF

RETURN

STATIC PROCEDURE Banner( cText, cText2, nFrames )

    s_cBanner := cText
    s_cBanner2 := cText2
    s_nBanner := nFrames

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
    s_nHeld := 0
    s_lShot := .F.
    s_aBombs := {}
    s_aBursts := {}
    s_aDivers := {}
    s_lComet := .F.
    s_nCometNext := 400 + hb_RandomInt( 0, 300 )
    s_nDead := 0
    Banner( "WAVE " + hb_ntos( s_nLevel ), IIF( s_nLevel > 1 .AND. RepairShields(), "SHIELDS REPAIRED", "" ), 60 )
    Sound( SND_WAVE )

RETURN

STATIC PROCEDURE NewShields()

    s_aShield := Array( SCR_H, SCR_W )
    AEval( s_aShield, {| a | AFill( a, 0 ) } )
    AEval( ShieldCells(), {| a | s_aShield[ a[ 1 ] + 1, a[ 2 ] + 1 ] := 1 } )

RETURN

/* { nRow, nCol } of every bunker cell: four bunkers, 4 cells wide, 2 rows high */
STATIC FUNCTION ShieldCells()

    LOCAL aCells := {}, nB, nCol, nRow

    FOR nB := 0 TO 3
        FOR nCol := 2 + nB * 8 TO 5 + nB * 8
            FOR nRow := ROW_SHIELD TO ROW_SHIELD + 1
                AAdd( aCells, { nRow, nCol } )
            NEXT
        NEXT
    NEXT

RETURN aCells

/* after a wave every bunker cell gets one step of wear back (a gone one
   comes back worn out); .T. when something was repaired */
STATIC FUNCTION RepairShields()

    LOCAL a, n, lRepaired := .F.

    FOR EACH a IN ShieldCells()
        n := s_aShield[ a[ 1 ] + 1, a[ 2 ] + 1 ]
        IF n != 1
            s_aShield[ a[ 1 ] + 1, a[ 2 ] + 1 ] := IIF( n == 0, 4, n - 1 )
            lRepaired := .T.
        ENDIF
    NEXT

RETURN lRepaired

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

    LOCAL aD

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
            /* divers still on their way down come back from the top */
            FOR EACH aD IN s_aDivers
                IF ! aD[ 5 ]
                    aD[ 5 ] := .T.
                    aD[ 4 ] := ROW_COMET + 1
                ENDIF
            NEXT
        ENDIF
    ELSE
        MovePlayer( aKeys )
    ENDIF

    MoveShot()
    IF ! MoveAliens()
        RETURN .F.                         /* they landed */
    ENDIF
    MoveDivers()
    DropBombs()
    MoveBombs()
    MoveComet()

    IF s_nScore >= s_nNextLife
        s_nNextLife += EXTRA_LIFE
        IF s_nLives < MAX_LIVES
            s_nLives++
            Banner( "EXTRA LIFE", "", 60 )
            Sound( SND_LIFE )
        ENDIF
    ENDIF

    IF AliensLeft() == 0 .AND. Len( s_aDivers ) == 0 .AND. Len( s_aBursts ) == 0
        NewWave()
    ENDIF

RETURN .T.

STATIC PROCEDURE MovePlayer( aKeys )

    LOCAL nDir := IIF( aKeys[ 1 ] == aKeys[ 2 ], 0, IIF( aKeys[ 1 ], -1, 1 ) )

    s_nHeld := IIF( nDir != 0 .AND. Sgn( s_nHeld ) == nDir, s_nHeld + nDir, nDir )

    IF s_nMoveWait > 0
        s_nMoveWait--
    ELSEIF nDir != 0 .AND. s_nPx + nDir >= 0 .AND. s_nPx + nDir <= SCR_W - 2
        s_nPx += nDir
        /* a cell every 2nd frame (15 a second); held for 1/3 s, 3 cells in 4 frames */
        s_nMoveWait := IIF( Abs( s_nHeld ) < 10 .OR. Abs( s_nHeld ) % 3 == 0, 1, 0 )
    ENDIF

    IF aKeys[ 3 ] .AND. ! s_lShot
        s_lShot := .T.
        s_nShotCol := s_nPx                  /* the shot glyph sits at the cell's right edge */
        s_nShotHy := ROW_PLAYER * 2 - 1
        Sound( SND_SHOT )
    ENDIF

RETURN

STATIC FUNCTION Sgn( n )
RETURN IIF( n > 0, 1, IIF( n < 0, -1, 0 ) )

/* the player is hit (by a bomb or a diving manta) */
STATIC PROCEDURE KillPlayer()

    s_nLives--
    s_nDead := 45
    s_lShot := .F.
    Sound( SND_DEATH )

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
            Sound( SND_COMET )
            RETURN
        ENDIF

        /* aliens: 2 cells wide; the shot (at the right edge of its cell) hits
           the left one in the middle, the right one at its right half */
        FOR r := 1 TO ALIEN_ROWS
            IF AlienRow( r ) == nRow
                FOR c := 1 TO ALIEN_COLS
                    IF s_aAlive[ r, c ] .AND. ;
                       ( s_nShotCol == AlienCol( c ) .OR. s_nShotCol == AlienCol( c ) + 1 )
                        s_aAlive[ r, c ] := .F.
                        s_nScore += { 30, 20, 20, 10, 10 }[ r ]
                        AAdd( s_aBursts, { AlienCol( c ), nRow, 8, NIL } )
                        s_lShot := .F.
                        Sound( SND_HIT )
                        RETURN
                    ENDIF
                NEXT
            ENDIF
        NEXT

        IF ShotHitsDiver( nRow )
            RETURN
        ENDIF

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
                Sound( SND_ZAP )
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
    LOCAL r, c, nMinC := 99, nMaxC := 0, nMaxR := 0, aD

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
    s_nBeat := s_nBeat % 4 + 1
    Sound( SND_BEAT + s_nBeat - 1 )

    FOR r := 1 TO ALIEN_ROWS
        FOR c := 1 TO ALIEN_COLS
            IF s_aAlive[ r, c ]
                nMinC := Min( nMinC, c )
                nMaxC := Max( nMaxC, c )
                nMaxR := Max( nMaxR, r )
            ENDIF
        NEXT
    NEXT
    /* keep room for the divers' places: they come back to them */
    FOR EACH aD IN s_aDivers
        nMinC := Min( nMinC, aD[ 2 ] )
        nMaxC := Max( nMaxC, aD[ 2 ] )
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

/* bombs fall; one that reaches the ship kills it (the game ends in Update()
   when the wreck of the last life has been shown) */
STATIC PROCEDURE MoveBombs()

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
            KillPlayer()
        ENDIF
    NEXT

RETURN

/* from wave 3 a manta now and then leaves the formation and dives at the
   player (Galaxian); one that misses comes back from the top to its place */
STATIC PROCEDURE MoveDivers()

    LOCAL aCols := {}, c, i, aD

    IF s_nLevel >= 3 .AND. s_nDead == 0 .AND. s_nBanner == 0 .AND. ;
       Len( s_aDivers ) < IIF( s_nLevel >= 6, 2, 1 ) .AND. ;
       hb_RandomInt( 1, Max( 60, 200 - s_nLevel * 15 ) ) == 1
        FOR c := 1 TO ALIEN_COLS
            IF s_aAlive[ 1, c ]
                AAdd( aCols, c )
            ENDIF
        NEXT
        IF Len( aCols ) > 0
            c := aCols[ hb_RandomInt( 1, Len( aCols ) ) ]
            s_aAlive[ 1, c ] := .F.
            AAdd( s_aDivers, { 1, c, AlienCol( c ), AlienRow( 1 ), .F., 2, 0 } )
            Sound( SND_DIVE )
        ENDIF
    ENDIF

    FOR i := Len( s_aDivers ) TO 1 STEP -1
        aD := s_aDivers[ i ]
        IF --aD[ 6 ] > 0
            LOOP
        ENDIF
        aD[ 6 ] := 2                         /* a row every 2nd frame */
        aD[ 7 ]++
        IF aD[ 5 ]
            aD[ 3 ] += Max( -2, Min( 2, AlienCol( aD[ 2 ] ) - aD[ 3 ] ) )
            IF ++aD[ 4 ] >= AlienRow( aD[ 1 ] )
                s_aAlive[ aD[ 1 ], aD[ 2 ] ] := .T.
                hb_ADel( s_aDivers, i, .T. )
            ENDIF
            LOOP
        ENDIF

        /* steers at half its speed down, until it is near the shields */
        IF aD[ 7 ] % 2 == 0 .AND. aD[ 4 ] < ROW_SHIELD - 2
            aD[ 3 ] += Sgn( s_nPx - aD[ 3 ] )
        ENDIF
        IF ++aD[ 4 ] > ROW_GROUND
            aD[ 5 ] := .T.
            aD[ 4 ] := ROW_COMET + 1
            LOOP
        ENDIF
        HitShield( aD[ 4 ], aD[ 3 ] )
        HitShield( aD[ 4 ], aD[ 3 ] + 1 )
        IF aD[ 4 ] < ROW_SHIELD - 1 .AND. hb_RandomInt( 1, 6 ) == 1 .AND. Len( s_aBombs ) < 3 + Int( s_nLevel / 2 )
            AAdd( s_aBombs, { aD[ 3 ], ( aD[ 4 ] + 1 ) * 2 } )
        ENDIF
        IF aD[ 4 ] == ROW_PLAYER .AND. s_nDead == 0 .AND. Abs( aD[ 3 ] - s_nPx ) <= 1
            AAdd( s_aBursts, { aD[ 3 ], aD[ 4 ], 8, NIL } )
            hb_ADel( s_aDivers, i, .T. )
            KillPlayer()
        ENDIF
    NEXT

    /* a diver may have moved onto the shot */
    IF s_lShot
        ShotHitsDiver( Int( s_nShotHy / 2 ) )
    ENDIF

RETURN

/* the shot against the divers in nRow; .T. = hit */
STATIC FUNCTION ShotHitsDiver( nRow )

    LOCAL i, aD

    FOR i := Len( s_aDivers ) TO 1 STEP -1
        aD := s_aDivers[ i ]
        IF aD[ 4 ] == nRow .AND. ( s_nShotCol == aD[ 3 ] .OR. s_nShotCol == aD[ 3 ] + 1 )
            s_nScore += IIF( aD[ 5 ], 30, DIVE_POINTS )
            AAdd( s_aBursts, { aD[ 3 ], nRow, 8, NIL } )
            hb_ADel( s_aDivers, i, .T. )
            s_lShot := .F.
            Sound( SND_HIT )
            RETURN .T.
        ENDIF
    NEXT

RETURN .F.

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
    ELSEIF s_nCometCol % 4 == 0
        Sound( SND_UFO )
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
    DrawStars()

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
    FOR EACH aB IN s_aDivers
        Put( aB[ 4 ], aB[ 3 ], G( aRowGlyph[ aB[ 1 ] ] + IIF( s_nFrame % 8 < 4, 0, 2 ), 2 ), aRowColor[ aB[ 1 ] ] )
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
        PutC( 15, s_cBanner, "W+/N" )
        IF ! Empty( s_cBanner2 )                 /* row 17: at a wave's start aliens are on even rows */
            PutC( 17, s_cBanner2, "G+/N" )
        ENDIF
    ENDIF
    IF lPaused
        PutC( 15, "PAUSED", "GR+/N" )
    ENDIF

    DispEnd()

RETURN

STATIC PROCEDURE GameOver()

    LOCAL n, nStart

    Sound( SND_OVER )
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

RETURN

/*--------------------------------------------------------------- stars */

STATIC PROCEDURE NewStars()

    LOCAL i

    s_aStars := {}
    FOR i := 1 TO 24
        AAdd( s_aStars, { hb_RandomInt( 0, SCR_W - 1 ), hb_RandomInt( 1, ROW_GROUND - 1 ), ;
                          hb_RandomInt( 0, 20 ), i % 3 == 0 } )
    NEXT

RETURN

/* far stars drift down a row in 2/3 s, near ones in 1/3 s */
STATIC PROCEDURE MoveStars()

    LOCAL a

    s_nStarTick++
    FOR EACH a IN s_aStars
        IF --a[ 3 ] < 0
            a[ 3 ] := IIF( a[ 4 ], 10, 20 )
            IF ++a[ 2 ] >= ROW_GROUND
                a[ 2 ] := 1
                a[ 1 ] := hb_RandomInt( 0, SCR_W - 1 )
            ENDIF
        ENDIF
    NEXT

RETURN

/* drawn first: anything else in the cell covers the star */
STATIC PROCEDURE DrawStars()

    LOCAL a

    FOR EACH a IN s_aStars
        IF a[ 4 ]
            Put( a[ 2 ], a[ 1 ], G( IIF( ( s_nStarTick + a:__enumIndex() * 37 ) % 120 < 8, G_STAR_BRIGHT, G_STAR ) ), "W/N" )
        ELSE
            Put( a[ 2 ], a[ 1 ], G( G_STAR ), "B+/N" )
        ENDIF
    NEXT

RETURN

/*--------------------------------------------------------------- high scores */

/* nebula.hi next to the program; when that folder cannot be written
   (Program Files, a read-only share) the user's own folder */
STATIC FUNCTION HiScoreFile( lUser )

    LOCAL cDir

    IF ! hb_defaultValue( lUser, .F. )
        RETURN hb_DirBase() + "nebula.hi"
    ENDIF
#ifdef __PLATFORM__WINDOWS
    cDir := GetEnv( "APPDATA" )
#else
    cDir := GetEnv( "HOME" )
#endif
    IF Empty( cDir )
        cDir := hb_DirTemp()
    ENDIF

RETURN hb_DirSepAdd( cDir ) + IIF( hb_ps() == "/", ".nebula.hi", "nebula.hi" )

/* nebula.hi: a line per score, "12345 ABC" */
STATIC PROCEDURE LoadHiScores()

    LOCAL cLine, aWords

    s_aHiScores := {}
    FOR EACH cLine IN hb_ATokens( StrTran( hb_MemoRead( IIF( hb_FileExists( HiScoreFile( .T. ) ), ;
                                  HiScoreFile( .T. ), HiScoreFile() ) ), Chr( 13 ) ), Chr( 10 ) )
        aWords := hb_ATokens( AllTrim( cLine ), " " )
        IF Len( aWords ) == 2 .AND. Val( aWords[ 1 ] ) > 0
            AAdd( s_aHiScores, { Val( aWords[ 1 ] ), Initials( aWords[ 2 ] ) } )
        ENDIF
    NEXT
    IF Len( s_aHiScores ) == 0
        s_aHiScores := { { 2500, "HBR" }, { 2000, "CUI" }, { 1500, "ZXS" }, { 1000, "NEB" }, { 500, "RAD" } }
    ENDIF
    ASort( s_aHiScores,,, {| a, b | a[ 1 ] > b[ 1 ] } )
    ASize( s_aHiScores, Min( Len( s_aHiScores ), HISCORES ) )
    s_nHi := s_aHiScores[ 1, 1 ]

RETURN

/* three letters A..Z (the font has no others) */
STATIC FUNCTION Initials( cText )

    LOCAL cOut := "", i, c

    cText := PadR( Upper( cText ), 3 )
    FOR i := 1 TO 3
        c := SubStr( cText, i, 1 )
        cOut += IIF( c >= "A" .AND. c <= "Z", c, "-" )
    NEXT

RETURN cOut

STATIC FUNCTION IsHiScore( nScore )
RETURN nScore > 0 .AND. ( Len( s_aHiScores ) < HISCORES .OR. nScore > ATail( s_aHiScores )[ 1 ] )

/* adds and saves the score; its rank */
STATIC FUNCTION AddHiScore( nScore, cInitials )

    LOCAL aEntry := { nScore, cInitials }, cText := "", aHi

    AAdd( s_aHiScores, aEntry )
    ASort( s_aHiScores,,, {| a, b | a[ 1 ] > b[ 1 ] } )
    ASize( s_aHiScores, Min( Len( s_aHiScores ), HISCORES ) )
    s_nHi := s_aHiScores[ 1, 1 ]

    FOR EACH aHi IN s_aHiScores
        cText += hb_ntos( aHi[ 1 ] ) + " " + aHi[ 2 ] + hb_eol()
    NEXT
    /* the user's file, once there is one, keeps the newer scores */
    IF hb_FileExists( HiScoreFile( .T. ) ) .OR. ! hb_MemoWrit( HiScoreFile(), cText )
        hb_MemoWrit( HiScoreFile( .T. ), cText )
    ENDIF

RETURN AScan( s_aHiScores, {| a | a == aEntry } )

/* three letters, the arcade way: Up/Down pick, Space next, or type them */
STATIC FUNCTION EnterInitials( nScore )

    LOCAL cLetters := "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    LOCAL aPick := { 1, 1, 1 }, nCur := 1, nKey, nAt, nStart, n := 0, i

    hb_keyClear()
    Sound( SND_LIFE )
    DO WHILE nCur <= 3
        nStart := hb_MilliSeconds()
        DO WHILE nCur <= 3 .AND. ( nKey := Inkey() ) != 0
            DO CASE
            CASE nKey == K_UP
                aPick[ nCur ] := aPick[ nCur ] % 26 + 1
            CASE nKey == K_DOWN
                aPick[ nCur ] := ( aPick[ nCur ] + 24 ) % 26 + 1
            CASE nKey == K_LEFT .OR. nKey == K_BS
                nCur := Max( 1, nCur - 1 )
            CASE nKey == K_RIGHT .OR. nKey == K_SPACE
                nCur++
            CASE nKey == K_ENTER .OR. nKey == K_ESC
                nCur := 4
            CASE ( nAt := At( Upper( hb_keyChar( nKey ) ), cLetters ) ) > 0
                aPick[ nCur ] := nAt
                nCur++
            ENDCASE
        ENDDO
        MoveStars()

        DispBegin()
        CLS
        DrawStars()
        PutC( 4, "NEW HIGH SCORE", IIF( n % 20 < 14, "GR+/N", "W/N" ) )
        PutC( 6, hb_ntos( nScore ), "W+/N" )
        PutC( 9, "ENTER YOUR INITIALS", "W/N" )
        FOR i := 1 TO 3
            Put( 12, 12 + i * 2, SubStr( cLetters, aPick[ i ], 1 ), IIF( i == nCur, "GR+/N", "W+/N" ) )
            IF i == nCur .AND. n % 16 < 10
                Put( 13, 12 + i * 2, "-", "GR+/N" )
            ENDIF
        NEXT
        PutC( 16, "UP DOWN  LETTER", "W/N" )
        PutC( 17, "SPACE  NEXT  ENTER  DONE", "W/N" )
        Put( ROW_GROUND, 0, Replicate( G( G_GROUND ), SCR_W ), "G/N" )
        DispEnd()

        n++
        FrameWait( nStart )
    ENDDO

RETURN SubStr( cLetters, aPick[ 1 ], 1 ) + SubStr( cLetters, aPick[ 2 ], 1 ) + SubStr( cLetters, aPick[ 3 ], 1 )

/*--------------------------------------------------------------- sound */

/* a WAV image (8-bit unsigned mono) of parts played one after another:
   { nFreqFrom, nFreqTo, nMs, nVolFrom, nVolTo [, lNoise ] } - a square wave
   sliding between the frequencies, or noise: a random level changed nFreq
   times a second */
STATIC FUNCTION Synth( aParts )

    LOCAL nTotal := 0, cData, aP, i, nPos := 0, nSamples, nT, nPhase := 0, nLevel := 0

    AEval( aParts, {| a | nTotal += Int( a[ 3 ] * SND_RATE / 1000 ) } )
    cData := Space( nTotal )
    FOR EACH aP IN aParts
        nSamples := Int( aP[ 3 ] * SND_RATE / 1000 )
        FOR i := 1 TO nSamples
            nT := ( i - 1 ) / nSamples
            nPhase += ( aP[ 1 ] + ( aP[ 2 ] - aP[ 1 ] ) * nT ) / SND_RATE
            IF Len( aP ) > 5 .AND. aP[ 6 ]
                IF nPhase >= 1
                    nPhase -= Int( nPhase )
                    nLevel := hb_RandomInt( -100, 100 ) / 100
                ENDIF
            ELSE
                nPhase -= Int( nPhase )
                nLevel := IIF( nPhase < 0.5, 1, -1 )
            ENDIF
            hb_BPoke( @cData, ++nPos, 128 + Int( nLevel * ( aP[ 4 ] + ( aP[ 5 ] - aP[ 4 ] ) * nT ) ) )
        NEXT
    NEXT

RETURN "RIFF" + L2Bin( 36 + nTotal ) + "WAVEfmt " + L2Bin( 16 ) + ;
       I2Bin( 1 ) + I2Bin( 1 ) + L2Bin( SND_RATE ) + L2Bin( SND_RATE ) + I2Bin( 1 ) + I2Bin( 8 ) + ;
       "data" + L2Bin( nTotal ) + cData

/* square-wave notes of nMs each */
STATIC FUNCTION Notes( aFreq, nMs, nVol )

    LOCAL aParts := {}, nFreq

    FOR EACH nFreq IN aFreq
        AAdd( aParts, { nFreq, nFreq, nMs, nVol, nVol } )
    NEXT

RETURN aParts

STATIC PROCEDURE InitSound()

    /* { nPriority, aParts }: a sound cuts one of lower or the same priority */
    LOCAL aDefs := { ;
        { 2, { { 1800, 500, 80, 40, 10 } } }, ;                                        /* SND_SHOT */
        { 3, { { 6000, 1500, 180, 60, 0, .T. } } }, ;                                  /* SND_HIT */
        { 3, { { 9000, 6000, 50, 40, 0, .T. } } }, ;                                   /* SND_ZAP */
        { 4, { { 500, 2000, 120, 45, 40 }, { 2000, 500, 120, 40, 35 }, { 500, 2500, 160, 35, 0 } } }, ; /* SND_COMET */
        { 1, { { 700, 1000, 60, 20, 20 }, { 1000, 700, 60, 20, 20 } } }, ;             /* SND_UFO */
        { 2, { { 2200, 400, 450, 30, 20 } } }, ;                                       /* SND_DIVE */
        { 5, { { 4000, 300, 700, 70, 0, .T. } } }, ;                                   /* SND_DEATH */
        { 4, Notes( { 1047, 1319, 1568, 2093, 1568, 2093 }, 60, 35 ) }, ;               /* SND_LIFE */
        { 4, Notes( { 523, 659, 784, 1047 }, 70, 30 ) }, ;                             /* SND_WAVE */
        { 5, Notes( { 392, 330, 262, 196 }, 160, 40 ) }, ;                             /* SND_OVER */
        { 1, { { 110, 110, 60, 55, 35 } } }, ;                                         /* SND_BEAT */
        { 1, { { 98, 98, 60, 55, 35 } } }, ;
        { 1, { { 87, 87, 60, 55, 35 } } }, ;
        { 1, { { 82, 82, 60, 55, 35 } } } }
    LOCAL aD, nMs, cFile
#ifndef __PLATFORM__WINDOWS
    LOCAL cOut, cErr

    IF hb_processRun( "aplay --version", , @cOut, @cErr ) != 0
        RETURN                                   /* no player: no sound */
    ENDIF
#endif

    FOR EACH aD IN aDefs
        nMs := 0
        AEval( aD[ 2 ], {| a | nMs += a[ 3 ] } )
        cFile := hb_DirTemp() + "nebula-" + hb_ntos( aD:__enumIndex() ) + ".wav"
        AAdd( s_aSounds, { aD[ 1 ], Synth( aD[ 2 ] ), nMs, cFile } )
#ifndef __PLATFORM__WINDOWS
        hb_MemoWrit( cFile, ATail( s_aSounds )[ 2 ] )
#endif
    NEXT

RETURN

STATIC PROCEDURE Sound( nSound )

    LOCAL aS

    IF ! s_lSound .OR. nSound > Len( s_aSounds )
        RETURN
    ENDIF
    aS := s_aSounds[ nSound ]
    IF hb_MilliSeconds() < s_nSndEnd .AND. aS[ 1 ] < s_nSndPrio
        RETURN
    ENDIF
#ifdef __PLATFORM__WINDOWS
    nebula_PlayWav( aS[ 2 ] )
#else
    StopSound()
    s_hSndProc := hb_processOpen( "aplay -q " + aS[ 4 ], , , , .T. )
    IF s_hSndProc == -1
        s_hSndProc := NIL
    ENDIF
#endif
    s_nSndPrio := aS[ 1 ]
    s_nSndEnd := hb_MilliSeconds() + aS[ 3 ]

RETURN

STATIC PROCEDURE StopSound()

#ifdef __PLATFORM__WINDOWS
    nebula_PlayWav()
#else
    IF s_hSndProc != NIL
        IF hb_processValue( s_hSndProc, .F. ) == -1      /* still playing */
            hb_processClose( s_hSndProc, .F. )
            hb_processValue( s_hSndProc, .T. )
        ENDIF
        s_hSndProc := NIL
    ENDIF
#endif
    s_nSndEnd := 0

RETURN

STATIC PROCEDURE ToggleSound()

    s_lSound := ! s_lSound
    IF ! s_lSound
        StopSound()
    ENDIF

RETURN

STATIC PROCEDURE DoneSound()

    LOCAL aS

    StopSound()
    FOR EACH aS IN s_aSounds
        IF hb_FileExists( aS[ 4 ] )
            FErase( aS[ 4 ] )
        ENDIF
    NEXT

RETURN

#pragma BEGINDUMP

#include "hbapi.h"

#if defined( HB_OS_WIN )
   #include <windows.h>
   #include <mmsystem.h>
#endif

/* NEBULA_PLAYWAV( [ cWav ] ): plays a WAV image without waiting; no
   argument stops it. The string must stay alive while it plays. */
HB_FUNC( NEBULA_PLAYWAV )
{
#if defined( HB_OS_WIN )
   if( HB_ISCHAR( 1 ) )
      hb_retl( PlaySoundA( hb_parc( 1 ), NULL, SND_MEMORY | SND_ASYNC | SND_NODEFAULT ) != 0 );
   else
      hb_retl( PlaySoundA( NULL, NULL, 0 ) != 0 );
#else
   hb_retl( HB_FALSE );
#endif
}

#pragma ENDDUMP
