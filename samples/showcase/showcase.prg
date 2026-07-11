/** @file showcase.prg
 * HBTui Showcase - Comprehensive demo of all controls and features
 *
 * Layout fills the fullscreen character grid (3 columns + bottom bar).
 *
 * Demonstrates:
 *   - HTToolBar with clickable buttons (top bar)
 *   - MenuBar with theme switching and context menu
 *   - HTGet with numeric, date, and string PICTURE fields (TGet-backed)
 *   - HTBrowse data grid with right-click context menu
 *   - HTListBox, HTComboBox, HTCheckBox, HTRadioButton, HTSpinner
 *   - HTProgressBar, HTStatusBar, HTFrame, HTSeparator
 *   - HTTabWidget with two tabs:
 *       "Editor": HTTextEdit + HTScrollBar (companion)
 *       "Tasks":  HTCheckList (Space to toggle items)
 *   - HTCheckList in right panel for Permissions
 *   - Theme switching (View menu or F1-F4)
 *   - Clipboard (Ctrl+C/V/X in LineEdit/TextEdit)
 *
 * Keyboard:
 *   Tab/Shift+Tab   - cycle focus
 *   F10             - activate menu bar
 *   F1-F4           - switch theme (Default/Dark/HiContrast/Mono)
 *   Ctrl+Tab        - next tab in HTTabWidget
 *   Ctrl+Shift+Tab  - previous tab
 *   Space           - toggle checkbox / open combo / press button
 *   Enter           - activate browse row / press button
 *   Arrows          - navigate within controls
 *   Ctrl+C/V/X      - clipboard in LineEdit / TextEdit
 *   Ins             - toggle insert/overwrite in GET and TextEdit
 *   Ctrl+U          - undo in GET fields
 *   ESC             - close menu / combo dropdown
 */

#include "hbtui.ch"
#include "inkey.ch"

STATIC aData
STATIC nRecNo := 1
STATIC oStatusBar
STATIC oProgressBar
STATIC oWindow

/* GET field variables */
STATIC cName   := "John Doe                      "
STATIC cEmail  := "john@example.com              "
STATIC nSalary := 45000.00
STATIC dHired  := 0d20240115

PROCEDURE Main()

    LOCAL app, w1
    LOCAL menuBar, menu, submenu, act
    LOCAL oGet, brw, lst, cmb, chk, rdo, btn, spn
    LOCAL frm, sep
    LOCAL oCtxMenu
    LOCAL oToolBar
    LOCAL oTab
    LOCAL oTextEdit, oScrollBar
    LOCAL oTaskList, oPermList
    /* responsive layout (character cells) */
    LOCAL nW, nH, nCW, nCH
    LOCAL nYTool, nYBody, nYBtn, nYStat, nBodyH, nTopH, nBotH
    LOCAL nX1, nX2, nX3, nW1, nW2, nW3, nGap
    LOCAL nGetW, nY, nInnerH

    /* awin-style before any widgets — so MaxCol()/MaxRow() are final */
    ht_cuiSetupDisplay( 40, 131 )

    nW := MaxCol() + 1
    nH := MaxRow() + 1
    /* content under CUI title row */
    nCW := nW
    nCH := Max( 20, nH - 1 )

    nYTool := 1
    nYStat := nCH - 1
    nYBtn  := nYStat - 2
    nYBody := 2
    nBodyH := Max( 14, nYBtn - nYBody - 1 )
    nTopH  := Max( 8, Int( nBodyH * 0.42 ) )
    nBotH  := Max( 6, nBodyH - nTopH - 1 )

    nGap := 1
    nW1  := Max( 28, Int( ( nCW - 2 - 2 * nGap ) * 0.24 ) )
    nW3  := Max( 26, Int( ( nCW - 2 - 2 * nGap ) * 0.26 ) )
    nW2  := Max( 36, nCW - 2 - 2 * nGap - nW1 - nW3 )
    nX1  := 1
    nX2  := nX1 + nW1 + nGap
    nX3  := nX2 + nW2 + nGap
    nGetW := Max( 18, nW1 - 4 )

    /* sample data for browse */
    aData := { ;
        { "HRB-001", "Harbour",      "Language",  85000.00 }, ;
        { "HRB-002", "HBTui",        "Framework", 12500.00 }, ;
        { "HRB-003", "TBrowse",      "Component",  8200.00 }, ;
        { "HRB-004", "CT Windows",   "Library",    6700.00 }, ;
        { "HRB-005", "GTxwc",        "Driver",     9400.00 }, ;
        { "HRB-006", "hbmk2",        "Build Tool", 11200.00 }, ;
        { "HRB-007", "hbclass",      "OOP",        7800.00 }, ;
        { "HRB-008", "hbct",         "Contrib",    5300.00 }, ;
        { "HRB-009", "hbwin",        "Contrib",    4100.00 }, ;
        { "HRB-010", "hbcurl",       "Contrib",    3800.00 }, ;
        { "HRB-011", "hbssl",        "Contrib",    2900.00 }, ;
        { "HRB-012", "hbziparc",     "Contrib",    3200.00 }  ;
    }

    app := HTApplication():new()
    app:setDisplaySize( 40, 131 )

    w1 := HTMainWindow():new()
    w1:setWindowTitle( "HBTui Showcase" )
    oWindow := w1

    /* ================================================================
       TOOLBAR (content row 1 — below menu bar at row 0)
       ================================================================ */
    oToolBar := HTToolBar():new( w1 )
    oToolBar:move( 0, nYTool )
    oToolBar:addButton( "New",     {|| UpdateStatus( "File > New" ) } )
    oToolBar:addButton( "Open",    {|| UpdateStatus( "File > Open" ) } )
    oToolBar:addButton( "Save",    {|| UpdateStatus( "Saved!" ), AdvanceProgress() } )
    oToolBar:addButton( "Refresh", {|| oWindow:repaint(), UpdateStatus( "Refreshed" ) } )
    oToolBar:addButton( "Quit",    {|| HTApplication():quit() } )
    oToolBar:resize( nCW, 1 )

    /* ================================================================
       MENU BAR
       ================================================================ */
    menuBar := w1:menuBar

    menu := menuBar:addMenu( "File" )
    act := menu:addAction( "New" )
    act:onTriggered := {|| UpdateStatus( "File > New" ) }
    act := menu:addAction( "Open" )
    act:onTriggered := {|| UpdateStatus( "File > Open" ) }
    menu:addSeparator()
    act := menu:addAction( "Exit" )
    act:onTriggered := {|| HTApplication():quit() }

    menu := menuBar:addMenu( "Edit" )
    act := menu:addAction( "Cut   Ctrl+X" )
    act:onTriggered := {|| UpdateStatus( "Edit > Cut" ) }
    act := menu:addAction( "Copy  Ctrl+C" )
    act:onTriggered := {|| UpdateStatus( "Edit > Copy" ) }
    act := menu:addAction( "Paste Ctrl+V" )
    act:onTriggered := {|| UpdateStatus( "Edit > Paste" ) }

    menu := menuBar:addMenu( "View" )
    act := menu:addAction( "Refresh" )
    act:onTriggered := {|| UpdateStatus( "Refreshed" ), oWindow:repaint() }
    menu:addSeparator()

    submenu := menu:addMenu( "Themes" )
    act := submenu:addAction( "Default (Light)  F1" )
    act:onTriggered := {|| SwitchTheme( "Default" ) }
    act := submenu:addAction( "Dark             F2" )
    act:onTriggered := {|| SwitchTheme( "Dark" ) }
    act := submenu:addAction( "High Contrast    F3" )
    act:onTriggered := {|| SwitchTheme( "HighContrast" ) }
    act := submenu:addAction( "Monochrome       F4" )
    act:onTriggered := {|| SwitchTheme( "Mono" ) }

    menu := menuBar:addMenu( "Help" )
    act := menu:addAction( "About" )
    act:onTriggered := {|| HTMessageBox():information( "About", "HBTui Showcase v0.3 - All Controls Demo" ) }

    w1:onKey( K_F1, {|| SwitchTheme( "Default" ) } )
    w1:onKey( K_F2, {|| SwitchTheme( "Dark" ) } )
    w1:onKey( K_F3, {|| SwitchTheme( "HighContrast" ) } )
    w1:onKey( K_F4, {|| SwitchTheme( "Mono" ) } )

    /* ================================================================
       LEFT — Employee form + options (fills column height)
       ================================================================ */
    frm := HTFrame():new( "Employee Form", w1 )
    frm:move( nX1, nYBody )
    frm:resize( nW1, nTopH )

    nY := nYBody + 1
    oGet := HTGet():new( w1 )
    oGet:setup( {|x| IIF( x == NIL, cName, cName := x ) }, ;
                PadR( "Name:", 7 ), "@!", NIL, NIL, "Employee name (auto uppercase)", ;
                {|v| UpdateStatus( "Name: " + AllTrim( v ) ) }, .F., .F., nGetW )
    oGet:move( nX1 + 2, nY )
    nY += 2

    oGet := HTGet():new( w1 )
    oGet:setup( {|x| IIF( x == NIL, cEmail, cEmail := x ) }, ;
                PadR( "Email:", 7 ), "@S16", NIL, NIL, "Email address", NIL, .F., .F., nGetW )
    oGet:move( nX1 + 2, nY )
    nY += 2

    oGet := HTGet():new( w1 )
    oGet:setup( {|x| IIF( x == NIL, nSalary, nSalary := x ) }, ;
                PadR( "Salary:", 7 ), "999,999.99", ;
                {|v| IIF( v > 0, .T., ( UpdateStatus( "Salary must be > 0" ), .F. ) ) }, ;
                NIL, "Annual salary (VALID > 0)", ;
                {|v| UpdateStatus( "Salary: " + LTrim( Str( v, 12, 2 ) ) ) }, .F., .F., nGetW )
    oGet:move( nX1 + 2, nY )
    nY += 2

    oGet := HTGet():new( w1 )
    oGet:setup( {|x| IIF( x == NIL, dHired, dHired := x ) }, ;
                PadR( "Hired:", 7 ), "@D", NIL, NIL, "Hire date", ;
                {|v| UpdateStatus( "Hired: " + DToC( v ) ) }, .F., .F., nGetW )
    oGet:move( nX1 + 2, nY )

    nY := nYBody + nTopH + 1
    sep := HTSeparator():new( w1 )
    sep:move( nX1, nY )
    sep:resize( nW1, 1 )
    nY++

    HTLabel():new( "Options:", w1 ):move( nX1, nY )
    nY++
    chk := HTCheckBox():new( "Auto-save", w1 )
    chk:move( nX1, nY )
    chk:onToggled := {|l| UpdateStatus( "Auto-save: " + IIF( l, "ON", "OFF" ) ) }
    nY++
    chk := HTCheckBox():new( "Dark mode", w1 )
    chk:move( nX1, nY )
    chk:onToggled := {|l| ;
        IIF( l, HTTheme():loadDark(), HTTheme():loadDefault() ), ;
        oWindow:repaint(), ;
        UpdateStatus( "Dark mode: " + IIF( l, "ON", "OFF" ) ) ;
    }
    nY++
    chk := HTCheckBox():new( "Notifications", w1 )
    chk:move( nX1, nY )
    nY += 2

    HTLabel():new( "Priority:", w1 ):move( nX1, nY )
    nY++
    rdo := HTRadioButton():new( "High", w1 )
    rdo:move( nX1, nY )
    nY++
    rdo := HTRadioButton():new( "Normal", w1 )
    rdo:move( nX1, nY )
    nY++
    rdo := HTRadioButton():new( "Low", w1 )
    rdo:move( nX1, nY )
    nY += 2

    HTLabel():new( "Category:", w1 ):move( nX1, nY )
    nY++
    cmb := HTComboBox():new( w1 )
    cmb:move( nX1, nY )
    cmb:resize( Min( nW1, 22 ), 1 )
    cmb:addItem( "Language" )
    cmb:addItem( "Framework" )
    cmb:addItem( "Library" )
    cmb:addItem( "Component" )
    cmb:addItem( "Driver" )
    cmb:addItem( "Build Tool" )
    cmb:addItem( "Contrib" )
    cmb:onChanged := {|| UpdateStatus( "Category: " + cmb:currentText() ) }
    nY += 2

    spn := HTSpinner():new( w1 )
    spn:setup( "Rating", 0, 10, 1, 5 )
    spn:move( nX1, nY )
    spn:onChanged := {|n| UpdateStatus( "Rating: " + hb_ntos( n ) ) }

    /* ================================================================
       CENTER — browse (top) + editor/tasks tabs (bottom)
       ================================================================ */
    HTLabel():new( "Project Database:", w1 ):move( nX2, nYBody )

    brw := HTBrowse():new( w1 )
    brw:move( nX2, nYBody + 1 )
    brw:resize( nW2, Max( 6, nTopH - 1 ) )

    brw:setGoTopBlock( {|| nRecNo := 1 } )
    brw:setGoBottomBlock( {|| nRecNo := Len( aData ) } )
    brw:setSkipBlock( {|n| ArraySkip( n ) } )

    brw:addColumn( "Code",   {|| aData[ nRecNo, 1 ] }, 8 )
    brw:addColumn( "Name",   {|| aData[ nRecNo, 2 ] }, Max( 12, Int( nW2 * 0.28 ) ) )
    brw:addColumn( "Type",   {|| aData[ nRecNo, 3 ] }, Max( 10, Int( nW2 * 0.22 ) ) )
    brw:addColumn( "Budget", {|| aData[ nRecNo, 4 ] }, 9, "999,999" )

    brw:setHeadSep( "-" )
    brw:setColSep( " " )

    brw:onActivated := {|r| UpdateStatus( "Activated: " + aData[ nRecNo, 2 ] + " (row " + hb_ntos( r ) + ")" ) }

    oCtxMenu := HTContextMenu():new()
    oCtxMenu:addAction( "View Details" ):onTriggered := {|| UpdateStatus( "View: " + aData[ nRecNo, 2 ] ) }
    oCtxMenu:addAction( "Edit Record"  ):onTriggered := {|| UpdateStatus( "Edit: " + aData[ nRecNo, 2 ] ) }
    oCtxMenu:addSeparator()
    oCtxMenu:addAction( "Delete" ):onTriggered := {|| ;
        IIF( HTMessageBox():question( "Confirm", "Delete " + aData[ nRecNo, 2 ] + "?" ) = 1, ;
            UpdateStatus( "Deleted: " + aData[ nRecNo, 2 ] ), ;
            UpdateStatus( "Cancelled." ) ) ;
    }
    brw:contextMenu := oCtxMenu

    nY := nYBody + nTopH + 1
    HTLabel():new( "Editor / Tasks:", w1 ):move( nX2, nY )

    oTab := HTTabWidget():new( w1 )
    oTab:move( nX2, nY + 1 )
    oTab:resize( nW2, Max( 6, nBotH - 1 ) )
    oTab:onTabChanged := {|n| UpdateStatus( "Tab: " + IIF( n = 1, "Editor", "Tasks" ) ) }

    oTab:addTab( "Editor" )
    oTab:addTab( "Tasks" )

    nInnerH := Max( 4, oTab:height - 2 )

    oTextEdit := HTTextEdit():new()
    oTextEdit:move( 0, 0 )
    oTextEdit:resize( Max( 10, nW2 - 2 ), nInnerH )
    oTextEdit:text := "HBTui multi-line text editor." + hb_eol() + ;
                      "Arrows, Insert/Delete, Home/End." + hb_eol() + ;
                      "Ctrl+C/X/V clipboard, PgUp/PgDn." + hb_eol() + ;
                      "" + hb_eol() + ;
                      "Tab here, then click to focus." + hb_eol() + ;
                      "Type to edit; Del/BS to delete."
    oTextEdit:onChanged := {|| UpdateStatus( "Text modified" ) }
    oTab:addWidgetToTab( 1, oTextEdit )

    oScrollBar := HTScrollBar():new()
    oScrollBar:move( Max( 1, nW2 - 1 ), 0 )
    oScrollBar:resize( 1, nInnerH )
    oScrollBar:minimum := 0
    oScrollBar:maximum := 100
    oScrollBar:value := 0
    oTab:addWidgetToTab( 1, oScrollBar )

    oTaskList := HTCheckList():new()
    oTaskList:move( 0, 0 )
    oTaskList:resize( nW2, nInnerH )
    oTaskList:addItem( "Add toolbar support",    .T. )
    oTaskList:addItem( "Add context menu",       .T. )
    oTaskList:addItem( "Add tab widget",         .T. )
    oTaskList:addItem( "Add text editor",        .T. )
    oTaskList:addItem( "Add scrollbar widget",   .T. )
    oTaskList:addItem( "Add checklist widget",   .T. )
    oTaskList:addItem( "Add theme system",       .T. )
    oTaskList:addItem( "Add test suite",         .F. )
    oTaskList:addItem( "Write documentation",    .F. )
    oTaskList:addItem( "Publish v0.3 release",   .F. )
    oTaskList:onChanged := {|n, l| ;
        UpdateStatus( "Task " + hb_ntos( n ) + ": " + IIF( l, "done", "pending" ) ) ;
    }
    oTab:addWidgetToTab( 2, oTaskList )

    /* ================================================================
       RIGHT — activity, progress, permissions (stretch to edge)
       ================================================================ */
    HTLabel():new( "Recent Activity:", w1 ):move( nX3, nYBody )

    lst := HTListBox():new( w1 )
    lst:move( nX3, nYBody + 1 )
    lst:resize( nW3, Max( 6, nTopH - 1 ) )
    lst:addItem( "Compiled project" )
    lst:addItem( "Ran tests" )
    lst:addItem( "Fixed bug #42" )
    lst:addItem( "Updated docs" )
    lst:addItem( "Code review" )
    lst:addItem( "Deployed v2.0" )
    lst:addItem( "Merged PR #15" )
    lst:addItem( "Released beta" )
    lst:addItem( "Fixed bug #43" )
    lst:addItem( "Updated deps" )
    lst:addItem( "Perf tuning" )
    lst:addItem( "Security audit" )
    lst:onChanged := {|nIdx| UpdateStatus( "Recent: " + lst:itemText( nIdx ) ) }

    nY := nYBody + nTopH + 1
    HTLabel():new( "Build Progress:", w1 ):move( nX3, nY )
    nY++
    oProgressBar := HTProgressBar():new( w1 )
    oProgressBar:move( nX3, nY )
    oProgressBar:resize( nW3, 1 )
    oProgressBar:setValue( 35 )
    nY += 2

    HTLabel():new( "Permissions:", w1 ):move( nX3, nY )
    nY++
    oPermList := HTCheckList():new( w1 )
    oPermList:move( nX3, nY )
    oPermList:resize( nW3, Max( 5, nYBtn - nY - 1 ) )
    oPermList:addItem( "Read files",    .T. )
    oPermList:addItem( "Write files",   .T. )
    oPermList:addItem( "Execute",       .F. )
    oPermList:addItem( "Network",       .F. )
    oPermList:addItem( "Send email",    .T. )
    oPermList:addItem( "Notifications", .T. )
    oPermList:addItem( "Admin access",  .F. )
    oPermList:addItem( "Debug mode",    .F. )
    oPermList:onChanged := {|n, l| ;
        UpdateStatus( "Permission " + hb_ntos( n ) + ": " + IIF( l, "granted", "denied" ) ) ;
    }

    /* ================================================================
       BOTTOM — action buttons + full-width status
       ================================================================ */
    sep := HTSeparator():new( w1 )
    sep:move( 1, nYBtn - 1 )
    sep:resize( nCW - 2, 1 )

    btn := HTPushButton():new( "Save", w1 )
    btn:move( nX1, nYBtn )
    btn:onClicked := {|| UpdateStatus( "Saved!" ), AdvanceProgress() }

    btn := HTPushButton():new( "Reset", w1 )
    btn:move( nX1 + 9, nYBtn )
    btn:onClicked := {|| UpdateStatus( "Reset." ), ResetProgress() }

    btn := HTPushButton():new( "Delete", w1 )
    btn:move( nX1 + 19, nYBtn )
    btn:onClicked := {|| ;
        IIF( HTMessageBox():question( "Confirm", "Are you sure you want to delete?" ) = 1, ;
            UpdateStatus( "Deleted." ), ;
            UpdateStatus( "Cancelled." ) ) ;
    }

    btn := HTPushButton():new( "Quit", w1 )
    btn:move( nX1 + 30, nYBtn )
    btn:onClicked := {|| HTApplication():quit() }

    oStatusBar := HTStatusBar():new( w1 )
    oStatusBar:addSection( "Ready" )
    oStatusBar:addSection( "F1-F4: themes" )
    oStatusBar:addSection( "HBTui v0.3" )
    oStatusBar:move( 0, nYStat )
    oStatusBar:resize( nCW, 1 )

    /* ================================================================
       SHOW
       ================================================================ */
    w1:show()
    /* re-assert chrome sizes after fitToScreen (same grid, safe) */
    oToolBar:resize( Max( 10, w1:width ), 1 )
    oStatusBar:move( 0, Max( 2, w1:height - 2 ) )
    oStatusBar:resize( Max( 10, w1:width ), 1 )
    w1:repaint()

    app:exec()

RETURN

/* ----------------------------------------------------------------
   Helper functions
   ---------------------------------------------------------------- */

STATIC PROCEDURE UpdateStatus( cText )

    LOCAL parent

    IF oStatusBar != NIL
        oStatusBar:setSection( 1, cText )
        parent := oStatusBar:parent()
        IF parent != NIL
            parent:repaintChild( oStatusBar )
        ENDIF
    ENDIF

RETURN

STATIC PROCEDURE SwitchTheme( cName )

    SWITCH cName
    CASE "Default"
        HTTheme():loadDefault()
        EXIT
    CASE "Dark"
        HTTheme():loadDark()
        EXIT
    CASE "HighContrast"
        HTTheme():loadHighContrast()
        EXIT
    CASE "Mono"
        HTTheme():loadMono()
        EXIT
    ENDSWITCH

    IF oWindow != NIL
        oWindow:repaint()
    ENDIF

    UpdateStatus( "Theme: " + cName )

RETURN

STATIC PROCEDURE AdvanceProgress()

    LOCAL nVal, parent

    IF oProgressBar != NIL
        nVal := oProgressBar:value + 10
        IF nVal > 100
            nVal := 100
        ENDIF
        oProgressBar:setValue( nVal )
        parent := oProgressBar:parent()
        IF parent != NIL
            parent:repaintChild( oProgressBar )
        ENDIF
    ENDIF

RETURN

STATIC PROCEDURE ResetProgress()

    LOCAL parent

    IF oProgressBar != NIL
        oProgressBar:setValue( 0 )
        parent := oProgressBar:parent()
        IF parent != NIL
            parent:repaintChild( oProgressBar )
        ENDIF
    ENDIF

RETURN

STATIC FUNCTION ArraySkip( nRequest )

    LOCAL nActual

    nActual := IIF( nRequest > 0, ;
        Min( nRequest, Len( aData ) - nRecNo ), ;
        Max( nRequest, 1 - nRecNo ) )

    nRecNo += nActual

RETURN nActual
