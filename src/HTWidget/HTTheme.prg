/** @class HTTheme
 * Singleton color theme manager.
 *
 * Widgets ask it for the color of a category (HT_CLR_* constants, e.g.
 * HT_CLR_WINDOW). A theme is one color string per category plus, optionally,
 * a palette: the 16 RGB colors that color indexes 0..15 show (Windows
 * COLORREF values, see ht_rgb()). Loading a theme sets both, so the same
 * "0/9" can be gray in one theme and dark slate in another.
 *
 * Built-in themes. The first five come from Digi knjigovodstvo (boje.prg),
 * where users have long chosen among them:
 *   Classic       gray windows, pastel blue title and highlight (default)
 *   Blue          the old blue DOS look: white on blue, green browse
 *   Light         Classic with a lighter palette
 *   Dark          dark gray, blue highlight
 *   Amber         for the night: warm amber on near black, little blue light
 *   HighContrast  white and yellow on black
 *   Mono          grays only
 *
 * Own themes: change colors with setColor() / setPaletteColor() and keep the
 * result with saveTheme( cName ), or register one with addTheme(). Built-in
 * themes cannot be replaced or removed. samples/theme_editor edits, adds and
 * stores themes.
 */

#include "hbtui.ch"
#include "hbgtinfo.ch"

/* theme entry in FhThemes: { aColors, aPalette | NIL, lBuiltIn } */
#define _TH_COLORS    1
#define _TH_PALETTE   2
#define _TH_BUILTIN   3

SINGLETON CLASS HTTheme

PROTECTED:

    DATA FaColors              /* color string per HT_CLR_* category */
    DATA FaPalette  INIT NIL   /* 16 COLORREF values; NIL = keep the GT's palette */
    DATA FcName     INIT ""    /* theme last loaded or saved */
    DATA FhThemes              /* name => { aColors, aPalette, lBuiltIn } */

    METHOD addBuiltIn( cName, aColors, aPalette )

PUBLIC:

    CONSTRUCTOR new()

    /* colors of the current theme */
    METHOD getColor( nCategory )
    METHOD setColor( nCategory, cColor )
    METHOD colors()
    METHOD categoryName( nCategory )

    /* palette of the current theme */
    METHOD palette()
    METHOD setPalette( aPalette )
    METHOD paletteColor( nIndex )
    METHOD setPaletteColor( nIndex, nRGB )
    METHOD applyPalette()

    /* named themes */
    METHOD themeName()   INLINE ::FcName
    METHOD themeNames()
    METHOD hasTheme( cName )
    METHOD isBuiltIn( cName )
    METHOD loadTheme( cName )
    METHOD addTheme( cName, aColors, aPalette )
    METHOD saveTheme( cName )
    METHOD removeTheme( cName )
    METHOD themeColors( cName )
    METHOD themePalette( cName )

    METHOD loadDefault()      INLINE ::loadTheme( "Classic" )
    METHOD loadClassic()      INLINE ::loadTheme( "Classic" )
    METHOD loadBlue()         INLINE ::loadTheme( "Blue" )
    METHOD loadLight()        INLINE ::loadTheme( "Light" )
    METHOD loadDark()         INLINE ::loadTheme( "Dark" )
    METHOD loadAmber()        INLINE ::loadTheme( "Amber" )
    METHOD loadHighContrast() INLINE ::loadTheme( "HighContrast" )
    METHOD loadMono()         INLINE ::loadTheme( "Mono" )

ENDCLASS

/** Creates the singleton, registers the built-in themes, loads "Classic". */
METHOD new() CLASS HTTheme

    ::FhThemes := { => }
    hb_HKeepOrder( ::FhThemes, .T. )   /* themeNames() in registration order */
    hb_HCaseMatch( ::FhThemes, .F. )   /* "blue" finds "Blue" */

    ::addBuiltIn( "Classic",      ht_themeFromRoles( ht_rolesDigi() ),  ht_palClassic() )
    ::addBuiltIn( "Blue",         ht_themeFromRoles( ht_rolesBlue() ),  ht_palBlue() )
    ::addBuiltIn( "Light",        ht_themeFromRoles( ht_rolesLight() ), ht_palLight() )
    ::addBuiltIn( "Dark",         ht_themeFromRoles( ht_rolesDark() ),  ht_palDark() )
    ::addBuiltIn( "Amber",        ht_themeFromRoles( ht_rolesDark() ),  ht_palAmber() )
    ::addBuiltIn( "HighContrast", ht_colorsHighContrast(),              ht_palStandard() )
    ::addBuiltIn( "Mono",         ht_colorsMono(),                      ht_palStandard() )

    ::FaColors := Array( HT_CLR_MAX )
    ::loadDefault()

RETURN self

METHOD PROCEDURE addBuiltIn( cName, aColors, aPalette ) CLASS HTTheme
    ::FhThemes[ cName ] := { aColors, aPalette, .T. }
RETURN

/** Returns the color string for a given category constant.
 * @param nCategory HT_CLR_* constant
 * @return Color string (e.g. "N/W"), or "N/W" if invalid
 */
METHOD FUNCTION getColor( nCategory ) CLASS HTTheme
    IF HB_ISNUMERIC( nCategory ) .AND. nCategory >= 1 .AND. nCategory <= HT_CLR_MAX
        RETURN ::FaColors[ nCategory ]
    ENDIF
    ht_debugLog( "HTTheme:getColor: invalid category " + hb_CStr( nCategory ) )
RETURN "N/W"

/** Sets the color string for a given category of the current colors.
 * The named theme is not changed until saveTheme().
 * @param nCategory HT_CLR_* constant
 * @param cColor Color string (e.g. "15/1")
 */
METHOD PROCEDURE setColor( nCategory, cColor ) CLASS HTTheme
    IF HB_ISNUMERIC( nCategory ) .AND. nCategory >= 1 .AND. nCategory <= HT_CLR_MAX .AND. ;
       HB_ISSTRING( cColor ) .AND. ! Empty( cColor )
        ::FaColors[ nCategory ] := cColor
    ENDIF
RETURN

/** @return Copy of the current colors, one string per HT_CLR_* category */
METHOD FUNCTION colors() CLASS HTTheme
RETURN AClone( ::FaColors )

/** @return Name of a category, "WINDOW" for HT_CLR_WINDOW ("" if invalid) */
METHOD FUNCTION categoryName( nCategory ) CLASS HTTheme

    STATIC s_aNames := { ;
        "WINDOW", "DESKTOP", "LABEL", "GET_NORMAL", "GET_FOCUSED", "GET_READONLY", ;
        "GET_LABEL", "BUTTON_NORMAL", "BUTTON_FOCUSED", "CHECK_NORMAL", "CHECK_FOCUSED", ;
        "LIST_NORMAL", "LIST_SELECTED", "LIST_SEL_UNFOC", "BROWSE_NORMAL", "BROWSE_FOCUSED", ;
        "MENU_BAR", "MENU_BAR_SEL", "MENU_ITEM", "MENU_ITEM_SEL", "STATUSBAR", ;
        "PROGRESS_FILL", "PROGRESS_EMPTY", "FRAME", "LINEEDIT_NORMAL", "LINEEDIT_FOCUSED", ;
        "LINEEDIT_SELECTED", "COMBO_NORMAL", "COMBO_FOCUSED", "COMBO_DROP_N", "COMBO_DROP_S", ;
        "TAB_ACTIVE", "TAB_INACTIVE", "TAB_BAR", "SPINNER_NORMAL", "SPINNER_FOCUSED", ;
        "TOOLBAR_NORMAL", "TOOLBAR_FOCUSED", "TOOLBAR_ACTIVE", "CHECKLIST_NORMAL", ;
        "CHECKLIST_SELECTED", "CHECKLIST_SELUNFOC", "MSGBOX_INFO", "MSGBOX_WARN", ;
        "MSGBOX_QUEST", "MENU_SEP", "SCROLLBAR", "CTXMENU_NORMAL", "CTXMENU_SELECTED", ;
        "CTXMENU_SEP", "TOAST_INFO", "TOAST_SUCCESS", "TOAST_WARNING", "TOAST_ERROR", ;
        "BROWSE_SELECTED", "TITLEBAR", "TITLEBTN" }

    IF HB_ISNUMERIC( nCategory ) .AND. nCategory >= 1 .AND. nCategory <= Len( s_aNames )
        RETURN s_aNames[ nCategory ]
    ENDIF

RETURN ""

/** @return Copy of the current palette (16 COLORREF values), or NIL when the
 * theme leaves the GT's palette alone */
METHOD FUNCTION palette() CLASS HTTheme
RETURN IIF( HB_ISARRAY( ::FaPalette ), AClone( ::FaPalette ), NIL )

/** Sets the current palette and shows it.
 * @param aPalette 16 COLORREF values (ht_rgb()), or NIL to keep the GT's
 * @return .F. for anything else
 */
METHOD FUNCTION setPalette( aPalette ) CLASS HTTheme
    IF aPalette == NIL
        ::FaPalette := NIL
        RETURN .T.
    ENDIF
    IF ! ht_isPalette( aPalette )
        RETURN .F.
    ENDIF
    ::FaPalette := AClone( aPalette )
    ::applyPalette()
RETURN .T.

/** RGB value of a palette slot.
 * @param nIndex Color index 0..15
 * @return COLORREF (ht_rgb()), or NIL
 */
METHOD FUNCTION paletteColor( nIndex ) CLASS HTTheme

    LOCAL aPal := ::FaPalette

    IF ! HB_ISNUMERIC( nIndex ) .OR. nIndex < 0 .OR. nIndex > 15
        RETURN NIL
    ENDIF
    IF ! HB_ISARRAY( aPal )
        aPal := hb_gtInfo( HB_GTI_PALETTE )
        IF ! ht_isPalette( aPal )
            aPal := ht_palStandard()
        ENDIF
    ENDIF

RETURN aPal[ nIndex + 1 ]

/** Changes one palette slot of the current palette and shows it.
 * @param nIndex Color index 0..15
 * @param nRGB   COLORREF (ht_rgb( nR, nG, nB ))
 */
METHOD FUNCTION setPaletteColor( nIndex, nRGB ) CLASS HTTheme

    LOCAL aPal

    IF ! HB_ISNUMERIC( nIndex ) .OR. nIndex < 0 .OR. nIndex > 15 .OR. ! HB_ISNUMERIC( nRGB )
        RETURN .F.
    ENDIF
    IF ! HB_ISARRAY( ::FaPalette )
        aPal := hb_gtInfo( HB_GTI_PALETTE )
        ::FaPalette := IIF( ht_isPalette( aPal ), aPal, ht_palStandard() )
    ENDIF
    ::FaPalette[ nIndex + 1 ] := nRGB
    ::applyPalette()

RETURN .T.

/** Shows the current palette. GTs that do not report a palette (gtnul, some
 * terminals) keep their own colors; gtwvt and gtxwc repaint the screen. */
METHOD PROCEDURE applyPalette() CLASS HTTheme
    IF HB_ISARRAY( ::FaPalette ) .AND. ht_isPalette( hb_gtInfo( HB_GTI_PALETTE ) )
        hb_gtInfo( HB_GTI_PALETTE, ::FaPalette )
    ENDIF
RETURN

/** @return Names of all themes, built-in first, then in the order added */
METHOD FUNCTION themeNames() CLASS HTTheme
RETURN hb_HKeys( ::FhThemes )

/** @return .T. when a theme of this name exists (case-insensitive) */
METHOD FUNCTION hasTheme( cName ) CLASS HTTheme
RETURN HB_ISSTRING( cName ) .AND. hb_HHasKey( ::FhThemes, cName )

/** @return .T. for the themes that ship with the library */
METHOD FUNCTION isBuiltIn( cName ) CLASS HTTheme
RETURN ::hasTheme( cName ) .AND. ::FhThemes[ cName ][ _TH_BUILTIN ]

/** Makes a named theme current: its colors and its palette.
 * Widgets show it on their next paint (HTApplication():repaintAll()).
 * @param cName Theme name (case-insensitive)
 * @return .F. when there is no such theme
 */
METHOD FUNCTION loadTheme( cName ) CLASS HTTheme

    LOCAL aTheme

    IF ! ::hasTheme( cName )
        ht_debugLog( "HTTheme:loadTheme: no theme " + hb_CStr( cName ) )
        RETURN .F.
    ENDIF

    aTheme := ::FhThemes[ cName ]
    ::FaColors  := AClone( aTheme[ _TH_COLORS ] )
    ::FaPalette := IIF( HB_ISARRAY( aTheme[ _TH_PALETTE ] ), AClone( aTheme[ _TH_PALETTE ] ), NIL )
    ::FcName    := hb_HKeyAt( ::FhThemes, hb_HPos( ::FhThemes, cName ) )
    ::applyPalette()

RETURN .T.

/** Adds a theme, or replaces an own theme of the same name.
 * @param cName    Theme name
 * @param aColors  Color strings indexed by HT_CLR_*; categories left NIL or
 *                 empty (or past the end) take the current colors
 * @param aPalette 16 COLORREF values, or NIL: the theme keeps whatever
 *                 palette is shown when it is loaded
 * @return .F. for a built-in name or an invalid palette
 */
METHOD FUNCTION addTheme( cName, aColors, aPalette ) CLASS HTTheme

    LOCAL aNew, n

    IF ! HB_ISSTRING( cName ) .OR. Empty( cName ) .OR. ::isBuiltIn( cName ) .OR. ;
       ( aPalette != NIL .AND. ! ht_isPalette( aPalette ) )
        RETURN .F.
    ENDIF

    aNew := AClone( ::FaColors )
    IF HB_ISARRAY( aColors )
        FOR n := 1 TO Min( Len( aColors ), HT_CLR_MAX )
            IF HB_ISSTRING( aColors[ n ] ) .AND. ! Empty( aColors[ n ] )
                aNew[ n ] := aColors[ n ]
            ENDIF
        NEXT
    ENDIF

    IF ::hasTheme( cName )   /* keep the stored spelling and position */
        cName := hb_HKeyAt( ::FhThemes, hb_HPos( ::FhThemes, cName ) )
    ENDIF
    ::FhThemes[ cName ] := { aNew, IIF( aPalette == NIL, NIL, AClone( aPalette ) ), .F. }

RETURN .T.

/** Stores the current colors and palette as a theme and makes it current.
 * @return .F. for a built-in name
 */
METHOD FUNCTION saveTheme( cName ) CLASS HTTheme
    IF ! ::addTheme( cName, ::FaColors, ::FaPalette )
        RETURN .F.
    ENDIF
    ::FcName := hb_HKeyAt( ::FhThemes, hb_HPos( ::FhThemes, cName ) )
RETURN .T.

/** Removes an own theme. The current colors stay as they are.
 * @return .F. for a built-in or unknown theme
 */
METHOD FUNCTION removeTheme( cName ) CLASS HTTheme
    IF ! ::hasTheme( cName ) .OR. ::isBuiltIn( cName )
        RETURN .F.
    ENDIF
    hb_HDel( ::FhThemes, cName )
RETURN .T.

/** @return Copy of a theme's colors, or NIL */
METHOD FUNCTION themeColors( cName ) CLASS HTTheme
RETURN IIF( ::hasTheme( cName ), AClone( ::FhThemes[ cName ][ _TH_COLORS ] ), NIL )

/** @return Copy of a theme's palette, or NIL (no theme, or no palette) */
METHOD FUNCTION themePalette( cName ) CLASS HTTheme

    LOCAL aPal

    IF ! ::hasTheme( cName )
        RETURN NIL
    ENDIF
    aPal := ::FhThemes[ cName ][ _TH_PALETTE ]

RETURN IIF( HB_ISARRAY( aPal ), AClone( aPal ), NIL )

/*----------------------------------------------------------------------------
 * Theme data
 *--------------------------------------------------------------------------*/

STATIC FUNCTION ht_isPalette( aPal )

    LOCAL n

    IF ! HB_ISARRAY( aPal ) .OR. Len( aPal ) != 16
        RETURN .F.
    ENDIF
    FOR EACH n IN aPal
        IF ! HB_ISNUMERIC( n )
            RETURN .F.
        ENDIF
    NEXT

RETURN .T.

/* The categories of a theme from a few roles. Every category takes the color
   of the role it plays; "browse" / "browseSel" default to "list" / "select".
   The roles of the Digi themes follow its screen attributes: BROWSECOLOR
   (browse, browseSel), MENUCOLORN (menu, menuSel), STATUSBARC (status),
   DESKCOLOR (window, focus), HEADCOLOR (title), INFOCOLOR (info). */
STATIC FUNCTION ht_themeFromRoles( h )

    LOCAL a := Array( HT_CLR_MAX )
    LOCAL cBrowse    := hb_HGetDef( h, "browse", h[ "list" ] )
    LOCAL cBrowseSel := hb_HGetDef( h, "browseSel", h[ "select" ] )

    a[ HT_CLR_WINDOW            ] := h[ "window" ]
    a[ HT_CLR_LABEL             ] := h[ "window" ]
    a[ HT_CLR_FRAME             ] := h[ "window" ]
    a[ HT_CLR_GET_LABEL         ] := h[ "window" ]
    a[ HT_CLR_CHECK_NORMAL      ] := h[ "window" ]
    a[ HT_CLR_DESKTOP           ] := h[ "desktop" ]

    a[ HT_CLR_GET_NORMAL        ] := h[ "field" ]
    a[ HT_CLR_LINEEDIT_NORMAL   ] := h[ "field" ]
    a[ HT_CLR_COMBO_NORMAL      ] := h[ "field" ]
    a[ HT_CLR_SPINNER_NORMAL    ] := h[ "field" ]
    a[ HT_CLR_GET_FOCUSED       ] := h[ "focus" ]
    a[ HT_CLR_LINEEDIT_FOCUSED  ] := h[ "focus" ]
    a[ HT_CLR_COMBO_FOCUSED     ] := h[ "focus" ]
    a[ HT_CLR_SPINNER_FOCUSED   ] := h[ "focus" ]
    a[ HT_CLR_CHECK_FOCUSED     ] := h[ "focus" ]
    a[ HT_CLR_GET_READONLY      ] := h[ "readonly" ]

    a[ HT_CLR_BUTTON_NORMAL     ] := h[ "button" ]
    a[ HT_CLR_BUTTON_FOCUSED    ] := h[ "buttonFocus" ]
    a[ HT_CLR_TOOLBAR_NORMAL    ] := h[ "menu" ]
    a[ HT_CLR_TOOLBAR_FOCUSED   ] := h[ "buttonFocus" ]
    a[ HT_CLR_TOOLBAR_ACTIVE    ] := h[ "menuSel" ]

    a[ HT_CLR_LIST_NORMAL       ] := h[ "list" ]
    a[ HT_CLR_CHECKLIST_NORMAL  ] := h[ "list" ]
    a[ HT_CLR_COMBO_DROP_N      ] := h[ "list" ]
    a[ HT_CLR_LIST_SELECTED     ] := h[ "select" ]
    a[ HT_CLR_CHECKLIST_SELECTED ] := h[ "select" ]
    a[ HT_CLR_COMBO_DROP_S      ] := h[ "select" ]
    a[ HT_CLR_LINEEDIT_SELECTED ] := h[ "select" ]
    a[ HT_CLR_LIST_SEL_UNFOC    ] := h[ "selectDim" ]
    a[ HT_CLR_CHECKLIST_SELUNFOC ] := h[ "selectDim" ]

    /* TBrowse pairs: cells, cursor, heading, footing, other */
    a[ HT_CLR_BROWSE_FOCUSED    ] := cBrowse + "," + cBrowseSel + "," + cBrowse + "," + cBrowse + "," + cBrowseSel
    a[ HT_CLR_BROWSE_NORMAL     ] := cBrowse + "," + h[ "selectDim" ] + "," + cBrowse + "," + cBrowse + "," + cBrowseSel
    a[ HT_CLR_BROWSE_SELECTED   ] := cBrowseSel

    a[ HT_CLR_MENU_BAR          ] := h[ "menu" ]
    a[ HT_CLR_MENU_ITEM         ] := h[ "menu" ]
    a[ HT_CLR_MENU_SEP          ] := h[ "menu" ]
    a[ HT_CLR_CTXMENU_NORMAL    ] := h[ "menu" ]
    a[ HT_CLR_CTXMENU_SEP       ] := h[ "menu" ]
    a[ HT_CLR_MENU_BAR_SEL      ] := h[ "menuSel" ]
    a[ HT_CLR_MENU_ITEM_SEL     ] := h[ "menuSel" ]
    a[ HT_CLR_CTXMENU_SELECTED  ] := h[ "menuSel" ]

    a[ HT_CLR_STATUSBAR         ] := h[ "status" ]
    a[ HT_CLR_TITLEBAR          ] := h[ "title" ]
    a[ HT_CLR_TITLEBTN          ] := h[ "title" ]

    a[ HT_CLR_PROGRESS_FILL     ] := h[ "progress" ]
    a[ HT_CLR_PROGRESS_EMPTY    ] := h[ "track" ]
    a[ HT_CLR_SCROLLBAR         ] := h[ "track" ]
    a[ HT_CLR_TAB_ACTIVE        ] := h[ "tab" ]
    a[ HT_CLR_TAB_INACTIVE      ] := h[ "tabOff" ]
    a[ HT_CLR_TAB_BAR           ] := h[ "tabOff" ]

    a[ HT_CLR_MSGBOX_INFO       ] := h[ "info" ]
    a[ HT_CLR_TOAST_INFO        ] := h[ "info" ]
    a[ HT_CLR_MSGBOX_WARN       ] := h[ "warn" ]
    a[ HT_CLR_TOAST_WARNING     ] := h[ "warn" ]
    a[ HT_CLR_MSGBOX_QUEST      ] := h[ "quest" ]
    a[ HT_CLR_TOAST_SUCCESS     ] := h[ "ok" ]
    a[ HT_CLR_TOAST_ERROR       ] := h[ "error" ]

RETURN a

/* Digi "klasicna": windows on palette gray 9, menu bar 10, title 11,
   highlight 14 (pastel blue), white 15 */
STATIC FUNCTION ht_rolesDigi()
RETURN { ;
    "window"   => "0/9",  "desktop"     => "0/8", ;
    "field"    => "0/15", "focus"       => "0/14", "readonly" => "8/15", ;
    "button"   => "15/1", "buttonFocus" => "0/14", ;
    "list"     => "0/15", "select"      => "15/1", "selectDim" => "0/7", ;
    "browse"   => "0/9",  "browseSel"   => "0/15", ;
    "menu"     => "0/10", "menuSel"     => "0/14", ;
    "status"   => "0/15", "title"       => "0/11", ;
    "progress" => "15/1", "track"       => "0/7", ;
    "tab"      => "0/15", "tabOff"      => "0/7", ;
    "info"     => "15/1", "warn"        => "0/14", "quest" => "15/5", ;
    "ok"       => "15/2", "error"       => "15/4" }

/* Digi "svetla": the Classic roles; its palette is lighter, the desktop
   uses the workspace slot 12 */
STATIC FUNCTION ht_rolesLight()

    LOCAL h := ht_rolesDigi()

    h[ "desktop" ] := "0/12"

RETURN h

/* Digi "stara (plavo)": white on blue, green browse with a red cursor,
   black on brown title, menu as in Classic */
STATIC FUNCTION ht_rolesBlue()
RETURN { ;
    "window"   => "7/1",  "desktop"     => "0/8", ;
    "field"    => "0/7",  "focus"       => "4/15", "readonly" => "8/7", ;
    "button"   => "0/7",  "buttonFocus" => "15/4", ;
    "list"     => "0/2",  "select"      => "15/4", "selectDim" => "0/7", ;
    "menu"     => "0/10", "menuSel"     => "0/14", ;
    "status"   => "0/7",  "title"       => "0/6", ;
    "progress" => "15/4", "track"       => "0/7", ;
    "tab"      => "0/7",  "tabOff"      => "7/1", ;
    "info"     => "15/1", "warn"        => "0/6", "quest" => "15/3", ;
    "ok"       => "15/2", "error"       => "15/4" }

/* Digi "tamna" and "amber": light text on palette 9, highlight 14 */
STATIC FUNCTION ht_rolesDark()
RETURN { ;
    "window"   => "15/9",  "desktop"     => "8/12", ;
    "field"    => "15/13", "focus"       => "0/15", "readonly" => "8/13", ;
    "button"   => "15/13", "buttonFocus" => "15/14", ;
    "list"     => "15/13", "select"      => "15/14", "selectDim" => "7/11", ;
    "browse"   => "15/9",  "browseSel"   => "15/14", ;
    "menu"     => "15/10", "menuSel"     => "15/14", ;
    "status"   => "15/13", "title"       => "15/11", ;
    "progress" => "15/14", "track"       => "7/13", ;
    "tab"      => "15/14", "tabOff"      => "7/13", ;
    "info"     => "15/11", "warn"        => "0/6", "quest" => "15/5", ;
    "ok"       => "0/2",   "error"       => "0/4" }

STATIC FUNCTION ht_colorsHighContrast()

    LOCAL a := Array( HT_CLR_MAX )

    a[ HT_CLR_WINDOW            ] := "15/00"
    a[ HT_CLR_DESKTOP           ] := "07/00"
    a[ HT_CLR_LABEL             ] := "15/00"
    a[ HT_CLR_GET_NORMAL        ] := "15/00"
    a[ HT_CLR_GET_FOCUSED       ] := "00/15"
    a[ HT_CLR_GET_READONLY      ] := "07/00"
    a[ HT_CLR_GET_LABEL         ] := "14/00"
    a[ HT_CLR_BUTTON_NORMAL     ] := "15/00"
    a[ HT_CLR_BUTTON_FOCUSED    ] := "00/15"
    a[ HT_CLR_CHECK_NORMAL      ] := "15/00"
    a[ HT_CLR_CHECK_FOCUSED     ] := "00/15"
    a[ HT_CLR_LIST_NORMAL       ] := "15/00"
    a[ HT_CLR_LIST_SELECTED     ] := "00/15"
    a[ HT_CLR_LIST_SEL_UNFOC    ] := "00/07"
    a[ HT_CLR_BROWSE_NORMAL     ] := "15/00,00/15,14/00,14/00,11/00"
    a[ HT_CLR_BROWSE_FOCUSED    ] := "15/00,00/15,14/00,14/00,11/00"
    a[ HT_CLR_MENU_BAR          ] := "15/00"
    a[ HT_CLR_MENU_BAR_SEL      ] := "00/15"
    a[ HT_CLR_MENU_ITEM         ] := "15/00"
    a[ HT_CLR_MENU_ITEM_SEL     ] := "00/15"
    a[ HT_CLR_STATUSBAR         ] := "14/00"
    a[ HT_CLR_PROGRESS_FILL     ] := "15/00"
    a[ HT_CLR_PROGRESS_EMPTY    ] := "08/00"
    a[ HT_CLR_FRAME             ] := "15/00"
    a[ HT_CLR_LINEEDIT_NORMAL   ] := "15/00"
    a[ HT_CLR_LINEEDIT_FOCUSED  ] := "00/15"
    a[ HT_CLR_LINEEDIT_SELECTED ] := "00/14"
    a[ HT_CLR_COMBO_NORMAL      ] := "15/00"
    a[ HT_CLR_COMBO_FOCUSED     ] := "00/15"
    a[ HT_CLR_COMBO_DROP_N      ] := "15/00"
    a[ HT_CLR_COMBO_DROP_S      ] := "00/15"
    a[ HT_CLR_TAB_ACTIVE        ] := "00/15"
    a[ HT_CLR_TAB_INACTIVE      ] := "15/00"
    a[ HT_CLR_TAB_BAR           ] := "15/00"
    a[ HT_CLR_SPINNER_NORMAL    ] := "15/00"
    a[ HT_CLR_SPINNER_FOCUSED   ] := "00/15"
    a[ HT_CLR_TOOLBAR_NORMAL    ] := "15/00"
    a[ HT_CLR_TOOLBAR_FOCUSED   ] := "00/15"
    a[ HT_CLR_TOOLBAR_ACTIVE    ] := "00/15"
    a[ HT_CLR_CHECKLIST_NORMAL  ] := "15/00"
    a[ HT_CLR_CHECKLIST_SELECTED ] := "00/15"
    a[ HT_CLR_CHECKLIST_SELUNFOC ] := "00/07"
    a[ HT_CLR_MSGBOX_INFO       ] := "15/00"
    a[ HT_CLR_MSGBOX_WARN       ] := "14/00"
    a[ HT_CLR_MSGBOX_QUEST      ] := "11/00"
    a[ HT_CLR_MENU_SEP          ] := "15/00"
    a[ HT_CLR_SCROLLBAR         ] := "15/00"
    a[ HT_CLR_CTXMENU_NORMAL    ] := "15/00"
    a[ HT_CLR_CTXMENU_SELECTED  ] := "00/15"
    a[ HT_CLR_CTXMENU_SEP       ] := "15/00"
    a[ HT_CLR_TOAST_INFO        ] := "15/00"
    a[ HT_CLR_TOAST_SUCCESS     ] := "15/02"
    a[ HT_CLR_TOAST_WARNING     ] := "00/14"
    a[ HT_CLR_TOAST_ERROR       ] := "15/04"
    a[ HT_CLR_BROWSE_SELECTED   ] := "15/01"
    a[ HT_CLR_TITLEBAR          ] := "15/00"
    a[ HT_CLR_TITLEBTN          ] := "00/15"

RETURN a

STATIC FUNCTION ht_colorsMono()

    LOCAL a := Array( HT_CLR_MAX )

    a[ HT_CLR_WINDOW            ] := "07/00"
    a[ HT_CLR_DESKTOP           ] := "08/00"
    a[ HT_CLR_LABEL             ] := "07/00"
    a[ HT_CLR_GET_NORMAL        ] := "07/00"
    a[ HT_CLR_GET_FOCUSED       ] := "00/07"
    a[ HT_CLR_GET_READONLY      ] := "08/00"
    a[ HT_CLR_GET_LABEL         ] := "15/00"
    a[ HT_CLR_BUTTON_NORMAL     ] := "15/00"
    a[ HT_CLR_BUTTON_FOCUSED    ] := "00/07"
    a[ HT_CLR_CHECK_NORMAL      ] := "15/00"
    a[ HT_CLR_CHECK_FOCUSED     ] := "00/07"
    a[ HT_CLR_LIST_NORMAL       ] := "07/00"
    a[ HT_CLR_LIST_SELECTED     ] := "00/07"
    a[ HT_CLR_LIST_SEL_UNFOC    ] := "00/08"
    a[ HT_CLR_BROWSE_NORMAL     ] := "07/00,00/07,15/00,15/00,08/00"
    a[ HT_CLR_BROWSE_FOCUSED    ] := "07/00,00/07,15/00,15/00,08/00"
    a[ HT_CLR_MENU_BAR          ] := "15/00"
    a[ HT_CLR_MENU_BAR_SEL      ] := "00/07"
    a[ HT_CLR_MENU_ITEM         ] := "15/00"
    a[ HT_CLR_MENU_ITEM_SEL     ] := "00/07"
    a[ HT_CLR_STATUSBAR         ] := "15/00"
    a[ HT_CLR_PROGRESS_FILL     ] := "15/00"
    a[ HT_CLR_PROGRESS_EMPTY    ] := "08/00"
    a[ HT_CLR_FRAME             ] := "07/00"
    a[ HT_CLR_LINEEDIT_NORMAL   ] := "07/00"
    a[ HT_CLR_LINEEDIT_FOCUSED  ] := "00/07"
    a[ HT_CLR_LINEEDIT_SELECTED ] := "00/15"
    a[ HT_CLR_COMBO_NORMAL      ] := "07/00"
    a[ HT_CLR_COMBO_FOCUSED     ] := "00/07"
    a[ HT_CLR_COMBO_DROP_N      ] := "07/00"
    a[ HT_CLR_COMBO_DROP_S      ] := "00/07"
    a[ HT_CLR_TAB_ACTIVE        ] := "00/07"
    a[ HT_CLR_TAB_INACTIVE      ] := "15/00"
    a[ HT_CLR_TAB_BAR           ] := "15/00"
    a[ HT_CLR_SPINNER_NORMAL    ] := "07/00"
    a[ HT_CLR_SPINNER_FOCUSED   ] := "00/07"
    a[ HT_CLR_TOOLBAR_NORMAL    ] := "07/00"
    a[ HT_CLR_TOOLBAR_FOCUSED   ] := "00/07"
    a[ HT_CLR_TOOLBAR_ACTIVE    ] := "00/07"
    a[ HT_CLR_CHECKLIST_NORMAL  ] := "07/00"
    a[ HT_CLR_CHECKLIST_SELECTED ] := "00/07"
    a[ HT_CLR_CHECKLIST_SELUNFOC ] := "00/08"
    a[ HT_CLR_MSGBOX_INFO       ] := "15/00"
    a[ HT_CLR_MSGBOX_WARN       ] := "15/00"
    a[ HT_CLR_MSGBOX_QUEST      ] := "15/00"
    a[ HT_CLR_MENU_SEP          ] := "15/00"
    a[ HT_CLR_SCROLLBAR         ] := "07/00"
    a[ HT_CLR_CTXMENU_NORMAL    ] := "15/00"
    a[ HT_CLR_CTXMENU_SELECTED  ] := "00/07"
    a[ HT_CLR_CTXMENU_SEP       ] := "15/00"
    a[ HT_CLR_TOAST_INFO        ] := "00/07"
    a[ HT_CLR_TOAST_SUCCESS     ] := "00/07"
    a[ HT_CLR_TOAST_WARNING     ] := "00/07"
    a[ HT_CLR_TOAST_ERROR       ] := "00/07"
    a[ HT_CLR_BROWSE_SELECTED   ] := "00/07"
    a[ HT_CLR_TITLEBAR          ] := "15/00"
    a[ HT_CLR_TITLEBTN          ] := "00/07"

RETURN a

/* The 16 DOS/CGA colors (the GTs' default palette) */
STATIC FUNCTION ht_palStandard()
RETURN { ;
    ht_rgb(   0,   0,   0 ), ;   /*  0 N   black */
    ht_rgb(   0,   0, 128 ), ;   /*  1 B   blue */
    ht_rgb(   0, 128,   0 ), ;   /*  2 G   green */
    ht_rgb(   0, 128, 128 ), ;   /*  3 BG  cyan */
    ht_rgb( 128,   0,   0 ), ;   /*  4 R   red */
    ht_rgb( 128,   0, 128 ), ;   /*  5 RB  magenta */
    ht_rgb( 128, 128,   0 ), ;   /*  6 GR  brown */
    ht_rgb( 192, 192, 192 ), ;   /*  7 W   light gray */
    ht_rgb( 128, 128, 128 ), ;   /*  8 N+  dark gray */
    ht_rgb(   0,   0, 255 ), ;   /*  9 B+  bright blue */
    ht_rgb(   0, 255,   0 ), ;   /* 10 G+  bright green */
    ht_rgb(   0, 255, 255 ), ;   /* 11 BG+ bright cyan */
    ht_rgb( 255,   0,   0 ), ;   /* 12 R+  bright red */
    ht_rgb( 255,   0, 255 ), ;   /* 13 RB+ bright magenta */
    ht_rgb( 255, 255,   0 ), ;   /* 14 GR+ yellow */
    ht_rgb( 255, 255, 255 ) }    /* 15 W+  white */

/* Digi "klasicna" (awin.prg): slots 9-14 are Windows UI colors */
STATIC FUNCTION ht_palClassic()

    LOCAL aPal := ht_palStandard()

    aPal[ 10 ] := ht_rgb( 160, 160, 160 )   /*  9 window */
    aPal[ 11 ] := ht_rgb( 240, 240, 240 )   /* 10 menu bar */
    aPal[ 12 ] := ht_rgb( 153, 180, 209 )   /* 11 window title */
    aPal[ 13 ] := ht_rgb( 171, 171, 171 )   /* 12 workspace */
    aPal[ 14 ] := ht_rgb( 128, 128, 128 )   /* 13 dark gray */
    aPal[ 15 ] := ht_rgb( 166, 202, 240 )   /* 14 highlight */

RETURN aPal

/* Digi "stara (plavo)": DOS colors; menu bar and highlight as in Classic */
STATIC FUNCTION ht_palBlue()

    LOCAL aPal := ht_palStandard()

    aPal[ 11 ] := ht_rgb( 240, 240, 240 )   /* 10 menu bar */
    aPal[ 15 ] := ht_rgb( 166, 202, 240 )   /* 14 highlight */

RETURN aPal

/* Digi "svetla" */
STATIC FUNCTION ht_palLight()

    LOCAL aPal := ht_palClassic()

    aPal[  8 ] := ht_rgb( 208, 208, 208 )   /*  7 */
    aPal[ 10 ] := ht_rgb( 238, 238, 238 )   /*  9 window */
    aPal[ 11 ] := ht_rgb( 250, 250, 250 )   /* 10 menu bar */
    aPal[ 12 ] := ht_rgb( 190, 210, 235 )   /* 11 window title */
    aPal[ 13 ] := ht_rgb( 215, 215, 215 )   /* 12 workspace */
    aPal[ 14 ] := ht_rgb( 120, 120, 120 )   /* 13 */
    aPal[ 15 ] := ht_rgb( 170, 200, 240 )   /* 14 highlight */

RETURN aPal

/* Digi "tamna" */
STATIC FUNCTION ht_palDark()

    LOCAL aPal := ht_palClassic()

    aPal[  3 ] := ht_rgb( 150, 220, 150 )   /*  2 green (OK) */
    aPal[  5 ] := ht_rgb( 255, 120, 110 )   /*  4 red (errors) */
    aPal[  8 ] := ht_rgb( 200, 200, 200 )   /*  7 */
    aPal[  9 ] := ht_rgb( 140, 140, 140 )   /*  8 */
    aPal[ 10 ] := ht_rgb(  45,  45,  48 )   /*  9 window */
    aPal[ 11 ] := ht_rgb(  32,  32,  36 )   /* 10 menu bar */
    aPal[ 12 ] := ht_rgb(   0,  90, 158 )   /* 11 window title */
    aPal[ 13 ] := ht_rgb(  60,  60,  64 )   /* 12 workspace */
    aPal[ 14 ] := ht_rgb(  70,  70,  74 )   /* 13 fields, status line */
    aPal[ 15 ] := ht_rgb(   0, 120, 215 )   /* 14 highlight */
    aPal[ 16 ] := ht_rgb( 240, 240, 240 )   /* 15 white */

RETURN aPal

/* Digi "amber (nocna)": less blue light */
STATIC FUNCTION ht_palAmber()

    LOCAL aPal := ht_palClassic()

    aPal[  1 ] := ht_rgb(  20,  18,  16 )   /*  0 black */
    aPal[  3 ] := ht_rgb( 255, 183,  77 )   /*  2 amber (OK) */
    aPal[  5 ] := ht_rgb( 255, 138, 101 )   /*  4 red (errors) */
    aPal[  8 ] := ht_rgb( 200, 180, 150 )   /*  7 */
    aPal[  9 ] := ht_rgb( 150, 130, 100 )   /*  8 */
    aPal[ 10 ] := ht_rgb(  40,  34,  28 )   /*  9 window */
    aPal[ 11 ] := ht_rgb(  28,  24,  20 )   /* 10 menu bar */
    aPal[ 12 ] := ht_rgb( 120,  80,  20 )   /* 11 window title */
    aPal[ 13 ] := ht_rgb(  55,  48,  38 )   /* 12 workspace */
    aPal[ 14 ] := ht_rgb(  70,  60,  45 )   /* 13 fields, status line */
    aPal[ 15 ] := ht_rgb( 160, 110,  30 )   /* 14 highlight */
    aPal[ 16 ] := ht_rgb( 255, 214, 150 )   /* 15 "white" */

RETURN aPal
