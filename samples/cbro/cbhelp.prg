/** @file cbhelp.prg
 * CBro - help window (topic index + text) and the copyright window.
 *
 *   CBH_Show( cTopic )   help window at a topic ("" = the index)
 *   CBH_About()          copyright window
 *
 * Help text markup (one string per line):
 *   "# Title"            heading (title bar color, full width)
 *   "Keys|description"   key column (key cap colors) and its description
 *   other lines          plain text
 *
 * Copyright (c) 2026 Srđan Dragojlović
 * Dual licensed: GNU GPL version 3 or later (COPYING), or a commercial
 * license from the author - see LICENSE.
 */

#include "hbtui.ch"
#include "inkey.ch"

#define CB_VERSION   "1.1.2"
#define KEY_COL      27          /* width of the key column */

/*---------------------------------------------------------------- the text view */

CLASS CBHelpView FROM HTWidget

PUBLIC:
    DATA aLines INIT {}
    DATA nTop   INIT 1

    METHOD new( parent )
    METHOD setLines( aLines )
    METHOD paintEvent( event )
    METHOD keyEvent( keyEvent )
    METHOD mouseEvent( eventMouse )
    METHOD scroll( nRows )

ENDCLASS

METHOD new( parent ) CLASS CBHelpView
    ::super:new( parent )
    ::setFocusPolicy( HT_FOCUS_STRONG )
    ::FisVisible := .T.
RETURN self

METHOD PROCEDURE setLines( aLines ) CLASS CBHelpView
    ::aLines := aLines
    ::nTop := 1
RETURN

METHOD PROCEDURE paintEvent( event ) CLASS CBHelpView

    LOCAL nRow, nLine, cLine, nBar, cText := HTTheme():getColor( HT_CLR_WINDOW )
    LOCAL cHead := HTTheme():getColor( HT_CLR_TITLEBAR ), cKey := HTTheme():getColor( HT_CLR_GET_NORMAL )
    LOCAL nW := ::Fwidth - 1, nH := ::Fheight, nThumb, nThumbH, cTrack := HTTheme():getColor( HT_CLR_SCROLLBAR )

    HB_SYMBOL_UNUSED( event )

    FOR nRow := 0 TO nH - 1
        nLine := ::nTop + nRow
        IF nLine > Len( ::aLines )
            DispOutAt( nRow, 0, Space( nW ), cText )
            LOOP
        ENDIF
        cLine := ::aLines[ nLine ]
        DO CASE
        CASE Left( cLine, 2 ) == "# "
            DispOutAt( nRow, 0, " " + ht_cellPadR( SubStr( cLine, 3 ), nW - 1 ), cHead )
        CASE "|" $ cLine
            nBar := At( "|", cLine )
            DispOutAt( nRow, 0, "  ", cText )
            DispOutAt( nRow, 2, " " + ht_cellPadR( Left( cLine, nBar - 1 ), KEY_COL - 4 ) + " ", cKey )
            DispOutAt( nRow, KEY_COL, " " + ht_cellPadR( SubStr( cLine, nBar + 1 ), Max( 0, nW - KEY_COL - 1 ) ), cText )
        OTHERWISE
            DispOutAt( nRow, 0, "  " + ht_cellPadR( cLine, nW - 2 ), cText )
        ENDCASE
    NEXT

    /* scroll bar in the last column */
    nThumbH := Max( 1, Int( nH * nH / Max( nH, Len( ::aLines ) ) ) )
    nThumb := IIF( Len( ::aLines ) <= nH, 0, Int( ( ::nTop - 1 ) * ( nH - nThumbH ) / ( Len( ::aLines ) - nH ) ) )
    FOR nRow := 0 TO nH - 1
        IF Len( ::aLines ) > nH .AND. nRow >= nThumb .AND. nRow < nThumb + nThumbH
            DispOutAt( nRow, nW, ht_glyph( 0x2588, " " ), cTrack )
        ELSE
            DispOutAt( nRow, nW, ht_glyph( 0x2591, " " ), cTrack )
        ENDIF
    NEXT

RETURN

METHOD PROCEDURE scroll( nRows ) CLASS CBHelpView

    LOCAL nMax := Max( 1, Len( ::aLines ) - ::Fheight + 1 ), nNew := Max( 1, Min( ::nTop + nRows, nMax ) )

    IF nNew != ::nTop
        ::nTop := nNew
        IF ::parent() != NIL
            ::parent():repaintChild( self )
        ENDIF
    ENDIF

RETURN

METHOD PROCEDURE keyEvent( keyEvent ) CLASS CBHelpView

    SWITCH keyEvent:key
    CASE K_UP
        ::scroll( -1 )
        EXIT
    CASE K_DOWN
        ::scroll( 1 )
        EXIT
    CASE K_PGUP
        ::scroll( -( ::Fheight - 1 ) )
        EXIT
    CASE K_PGDN
        ::scroll( ::Fheight - 1 )
        EXIT
    CASE K_HOME
    CASE K_CTRL_PGUP
        ::scroll( -Len( ::aLines ) )
        EXIT
    CASE K_END
    CASE K_CTRL_PGDN
        ::scroll( Len( ::aLines ) )
        EXIT
    OTHERWISE
        ::super:keyEvent( keyEvent )
        RETURN
    ENDSWITCH
    keyEvent:accept()

RETURN

METHOD PROCEDURE mouseEvent( eventMouse ) CLASS CBHelpView

    DO CASE
    CASE eventMouse:nKey == K_MWFORWARD
        ::scroll( -3 )
    CASE eventMouse:nKey == K_MWBACKWARD
        ::scroll( 3 )
    OTHERWISE
        ::super:mouseEvent( eventMouse )
    ENDCASE

RETURN

/*---------------------------------------------------------------- help window */

CLASS CBHelpDlg FROM HTDialog
    METHOD startAt( oChild ) INLINE ::FfocusWidget := oChild
ENDCLASS

/* help window at topic cTopic ("" = index: the topic list has the focus) */
PROCEDURE CBH_Show( cTopic )

    LOCAL aTopics := HelpTopics(), dlg, oList, oView, nW, nH, n, nStart

    nW := Min( 110, ht_ScreenMaxCol() - 2 )
    nH := Min( 40, ht_ScreenMaxRow() - 2 )
    nStart := Max( 1, AScan( aTopics, {| a | a[ 1 ] == cTopic } ) )

    dlg := CBHelpDlg():new( NIL, "CBro Help  -  Up/Down PgUp/PgDn scroll, Tab topics / text, Esc closes" )
    dlg:resize( nW, nH )

    oList := HTListBox():new( dlg )
    oList:setGeometry( 1, 1, 22, nH - 6 )
    FOR n := 1 TO Len( aTopics )
        oList:addItem( " " + aTopics[ n ][ 2 ], n )
    NEXT

    oView := CBHelpView():new( dlg )
    oView:setGeometry( 24, 1, nW - 28, nH - 6 )
    oView:setLines( aTopics[ nStart ][ 3 ] )

    oList:setCurrentIndex( nStart )
    oList:onChanged := {| n | oView:setLines( aTopics[ n ][ 3 ] ), dlg:repaintChild( oView ) }
    oList:onActivated := {|| dlg:focusNextChild() }

    dlg:addButtonBar( "Close" )
    IF ! Empty( cTopic )
        dlg:startAt( oView )
    ENDIF
    dlg:exec()

RETURN

/*---------------------------------------------------------------- copyright */

PROCEDURE CBH_About()

    LOCAL dlg, nW := 64, nH := 21, aLines, n, oLabel

    aLines := { ;
        { "C B r o", 1 }, ;
        { "xBase database browser and file manager", 0 }, ;
        { "version " + CB_VERSION, 0 }, ;
        { "", 0 }, ;
        { "Copyright " + ht_glyph( 0x00A9, "(c)" ) + " 2026 Srđan Dragojlović", 1 }, ;
        { "Dual license: GNU GPL 3+ or commercial", 0 }, ;
        { "", 0 }, ;
        { "Made as a successor to BroPlus by Peter Volz", 0 }, ;
        { "(SmartCode Programming) - same menus and keys,", 0 }, ;
        { "new code: no BroPlus source is used.", 0 }, ;
        { "GUI sibling: GBro", 0 }, ;
        { "", 0 }, ;
        { "Built with " + Left( hb_Version(), At( "(", hb_Version() + "(" ) - 1 ), 0 }, ;
        { "on Harbour-CUI (character UI library)", 0 } }

    dlg := HTDialog():new( NIL, "Copyright" )
    dlg:resize( nW, nH )
    FOR n := 1 TO Len( aLines )
        oLabel := HTLabel():new( aLines[ n ][ 1 ], dlg )
        oLabel:setGeometry( 1, n, nW - 4, 1 )
        oLabel:alignment := HT_ALIGN_HCENTER
    NEXT
    dlg:addButtonBar( "OK" )
    dlg:exec()

RETURN

/*---------------------------------------------------------------- topics */

/* { { cId, cTitle, aLines }, ... } */
STATIC FUNCTION HelpTopics()
RETURN { ;
    { "help", "Help on Help", { ;
        "# Help on Help", ;
        "", ;
        "The topics are on the left, the text of the chosen one on the right.", ;
        "", ;
        "Up / Down|choose a topic (list) or scroll the text", ;
        "Tab / Shift-Tab|from the topics to the text and back", ;
        "Enter|in the topics: go to the text", ;
        "PgUp / PgDn|scroll the text a page", ;
        "Home / End|first / last line of the text", ;
        "Mouse wheel|scroll the text", ;
        "Esc|close the help window", ;
        "", ;
        "F1 opens the Help menu from anywhere: Browse Window Help, Help on Help,", ;
        "Help Index, Command Line Parameters, the Expression Evaluator,", ;
        "the Calculator and the Copyright." } }, ;
    { "browse", "Browse Window", { ;
        "# Browse window", ;
        "", ;
        "Every open table has its own window: title (alias, its place among the", ;
        "open tables, index order, relation), the records, and the status bar:", ;
        "Rec. number/count, <Lock: n> locked columns, the open mode, softseek,", ;
        "<Filter> <Scope> <Rel: ...> <Deleted>, the data codepage and the key.", ;
        "", ;
        "Arrows PgUp PgDn|move in the browse", ;
        "Home / End|first / last column on the screen", ;
        "Ctrl-Home / Ctrl-End|first / last record", ;
        "Ctrl-PgUp / Ctrl-PgDn|first / last column", ;
        "Enter|edit the field (memo: memo window)", ;
        "Space / click|toggle a logical field (check box)", ;
        "Ins / Del|append a record / delete - recall", ;
        "Alt-BackSpace|erase the memo", ;
        "Right mouse button|context menu", ;
        "Mouse drag|move / resize the window", ;
        "Ctrl-F6|next window", ;
        "", ;
        "Protected Edit (F6): typing does not change a field, Enter starts the", ;
        "editing. Immediate Edit: typing starts it at once." } }, ;
    { "keys", "Keys", { ;
        "# All keys", ;
        "", ;
        "F1 .. F9|Help Database Index Fields AreaSet GlobalSet Record Command Exit", ;
        "F10|menu bar", ;
        "Alt-F1 / Ctrl-F1|expression evaluator / calculator", ;
        "Alt-F2 / Alt-F3|database / index file selection screen", ;
        "Ctrl-F3 / Shift-F3|tag selection / index scope", ;
        "Ctrl-L / Ctrl-Q|open from a LetoDB server / SQL query", ;
        "Alt-N / Alt-Minus|next / previous database", ;
        "Alt-O|next index order", ;
        "Alt-A / Alt-E|edit all fields / the browse fields", ;
        "Alt-S Alt-G Alt-U Alt-J|seek, goto, locate, skip", ;
        "Alt-T / Alt-B|window top / bottom", ;
        "Alt-F / Alt-R / Alt-C|filter / relation / color condition", ;
        "Alt-I|incremental seek on / off", ;
        "Alt-L / Alt-H|lock columns / column options", ;
        "Shift-F5 Ctrl-F5 Alt-F5|cut, paste, insert column", ;
        "Shift-F4 / Ctrl-F4|copy / paste field", ;
        "Shift-F7 Ctrl-F7 Alt-F7|copy, paste record, view clipboard", ;
        "Alt-D / Alt-Q / Alt-P|hide deleted / softseek / index preselection", ;
        "Alt-X|open mode SHARED / EXCLUSIVE", ;
        "Alt-M|create / modify structure", ;
        "Alt-Y / Alt-Z|change directory / run an OS command", ;
        "Alt-V / Alt-W|restore / save a view", ;
        "Alt-K|run a script", ;
        "Shift-F6 / Alt-F6|unique keys / descending order", ;
        "Ctrl-F8 / Ctrl-F9|custom index: add / remove the record", ;
        "Alt-F9|exit" } }, ;
    { "f1", "F1 Help", { ;
        "# F1 Help", ;
        "", ;
        "Browse Window Help|keys of a table window", ;
        "Help on Help|how to use this window", ;
        "Help Index|all topics", ;
        "Command Line Parameters|how to start CBro", ;
        "Expression Evaluator|any Harbour expression on the current record", ;
        "Calculator|a calculator with a tape; Use puts the result in the", ;
        "|clipboard: Ctrl-F4 pastes it into a field", ;
        "Copyright|who made CBro" } }, ;
    { "f2", "F2 Database", { ;
        "# F2 Database", ;
        "", ;
        "Database Selection|the files of a directory (.dbf and .xlsx);", ;
        "|Enter on a directory opens it", ;
        "Open from LetoDB Server|connects to a LetoDB / LetoDBf server and lists", ;
        "|its files (see LetoDB)", ;
        "SQL Query|a SQLite file or an ODBC data source (see SQL)", ;
        "Open Excel Sheet|a sheet of an .xlsx file as a table (see Excel)", ;
        "Next / Previous Database|the next / previous open table", ;
        "Close Database|closes the current table and its window", ;
        "Open Mode|SHARED or EXCLUSIVE (the table is opened again)", ;
        "Alias Name|renames the alias of the current table", ;
        "Create/Modify Structure|structure editor; another file name makes a new", ;
        "|table, the same name changes the structure (old data", ;
        "|kept in <name>_bak.dbf, tags rebuilt)", ;
        "Database FileSpec|masks of the selection screen, e.g. *.dbf;*.dat", ;
        "Change Directory|the working directory", ;
        "Run OS Command|a command; its output is shown in a window" } }, ;
    { "f3", "F3 Index", { ;
        "# F3 Index", ;
        "", ;
        "Index File Selection|opens an index file (.cdx / .ntx)", ;
        "Next / Previous Order|the next index order (0 = natural order)", ;
        "Tag Selection|all orders with key, FOR and file", ;
        "Index Scope|top / bottom key values to show", ;
        "Create Index File|tag, key, FOR, file, unique", ;
        "Verify Index Integrity|checks every order: keys in order and as many", ;
        "|as the records; Command > Reindex repairs", ;
        "Index PreSelection|opens the production / same-name index together", ;
        "|with the table (Alt-P)", ;
        "Index FileSpec|masks of the index selection screen", ;
        "", ;
        "Dynamic options (current order, no rebuild):", ;
        "Show Unique Keys|Shift-F6: only the first record of every key value", ;
        "Descending Order|Alt-F6: the order read backwards", ;
        "Make Custom Index|turns the order into a custom index, or makes an", ;
        "|empty custom tag: records are added by hand", ;
        "Custom: Add / Remove|Ctrl-F8 / Ctrl-F9: the current record into / out of", ;
        "|the custom tag (Ctrl-F3 shows the chosen set)" } }, ;
    { "f4", "F4 Fields", { ;
        "# F4 Fields", ;
        "", ;
        "Field Selection|the fields shown in the browse", ;
        "Cut / Paste / Insert|move columns; Insert adds a field or an expression", ;
        "Lock # of Fields|columns that stay on the left", ;
        "Column Options|heading, width and picture of a column", ;
        "", ;
        "Numbers are aligned right (heading too), logical fields are check boxes", ;
        "(Space or a click toggles), memos show <Memo>, <JSON>, <Array>, <Date>", ;
        "or their first characters (AreaSet > Memo Width)." } }, ;
    { "f5", "F5 AreaSet", { ;
        "# F5 AreaSet", ;
        "", ;
        "Range & Filter|records FOR a condition", ;
        "Relation|the current table becomes the parent of another one:", ;
        "|moving in the parent seeks its key in the child; with", ;
        "|related records only the child shows just those", ;
        "Color Condition|records FOR a condition in another color", ;
        "Incremental Seek|typed characters seek in the index order", ;
        "Memo Width|0 = <Memo>, else the first characters of a memo", ;
        "Next / Prev. Area|the next / previous table", ;
        "Next / Previous Window|Ctrl-F6 also moves to the next window", ;
        "Tile / Cascade|arrange the windows", ;
        "Restore / Save View|all tables with indexes, order, filter, columns,", ;
        "|colors and relations in a .cbv file", ;
        "View Info|what a view would save" } }, ;
    { "f6", "F6 GlobalSet", { ;
        "# F6 GlobalSet", ;
        "", ;
        "Database Driver|DBFCDX, DBFNTX or LETO (LetoDB server)", ;
        "Data Codepage|codepage of the data in the tables (e.g. CP852,", ;
        "|CP1250); the screen shows it translated, writes go", ;
        "|back in the table's codepage", ;
        "LetoDB Server|address //host:port/ of the server", ;
        "Default Open Mode|SHARED or EXCLUSIVE for the next tables", ;
        "Modify|DISABLED = nothing can be changed", ;
        "Protected / Immediate|whether typing starts editing", ;
        "Hide Deleted Recs|SET DELETED", ;
        "Softseek|SET SOFTSEEK for Seek and the scope", ;
        "Sound|the warning tone", ;
        "Epoch / Date Format|two-digit years, date format (Other... = own)", ;
        "Default Memo Width|memo width of the next tables", ;
        "Auto-Alias|OFF = CBro asks for the alias of each table", ;
        "Theme|the colors; BroPlus (default) = the original", ;
        "|BroPlus color set and DOS palette", ;
        "Save Global Settings|cbro.ini next to the program" } }, ;
    { "f7", "F7 Record", { ;
        "# F7 Record", ;
        "", ;
        "Seek / Goto / Locate|find a key, a record number, a condition", ;
        "Skip|move n records", ;
        "Window Top / Bottom|first / last row of the window", ;
        "All / Selected Fields|edit the record in a form", ;
        "Append / Delete|Ins / Del", ;
        "Erase Memo|empties the memo of the current column", ;
        "Import / Export Memo|a text file into the memo, the memo into a file", ;
        "Memo window|Enter on a memo column; Save, Format, Store As", ;
        "|a memo holds text, JSON text, an array (also", ;
        "|nested - edited as a Harbour literal { ... }),", ;
        "|a date, a number or a logical value (FPT memos)", ;
        "Copy / Paste Field|one value (Shift-F4 / Ctrl-F4)", ;
        "Copy / Paste Record|fields with the same name and type", ;
        "View Clipboard|the copied field and record" } }, ;
    { "f8", "F8 Command", { ;
        "# F8 Command", ;
        "", ;
        "Append From|a DBF, an .xlsx sheet, CSV or SDF text", ;
        "Copy Structure|an empty table with the same structure", ;
        "Copy Stru Extended|the structure as records", ;
        "Copy To / Export|form: file, type (DBF, Excel .xlsx, CSV, SDF),", ;
        "|fields, scope (all / rest / next n), FOR, WHILE,", ;
        "|CSV separator, skip deleted", ;
        "Count / Sum|records FOR a condition, sum and average", ;
        "Delete / Recall|records FOR a condition", ;
        "Label / Report Form|Clipper .lbl / .frm files to the screen, a file or", ;
        "|the printer", ;
        "List to File/Print|chosen fields as text columns", ;
        "Pack / Zap|remove deleted / all records (EXCLUSIVE)", ;
        "Reindex|rebuild the open indexes", ;
        "Replace|a field WITH an expression FOR a condition", ;
        "Run Script|a .prg or .hrb (see Scripts)", ;
        "Script Editor|write, save and run scripts", ;
        "SQL Query|see SQL" } }, ;
    { "leto", "LetoDB / LetoDBf", { ;
        "# LetoDB and LetoDBf servers", ;
        "", ;
        "Tables on a LetoDB or LetoDBf server are opened with the LETO driver.", ;
        "The path starts with the server address:", ;
        "", ;
        "  //192.168.1.10:2812/sales/customer.dbf", ;
        "", ;
        "Ctrl-L|asks for the server, connects and lists its files", ;
        "GlobalSet > LetoDB Server|the address CBro offers", ;
        "cbro leto://host:port/x.dbf|opens a server table from the command line", ;
        "", ;
        "Indexes, relations, filters, Copy To and the rest work as with local", ;
        "tables. Listing the server's files needs EnableFileFunc = 1 in", ;
        "letodb.ini; without it type the file name.", ;
        "", ;
        "CBro is built for one of them: LetoDBf (default) or LetoDB", ;
        "(build.bat letodb). Both register the same LETO driver." } }, ;
    { "sql", "SQL queries", { ;
        "# SQL queries (SQLite, ODBC)", ;
        "", ;
        "Ctrl-Q opens the SQL window: driver, connection and the statement.", ;
        "", ;
        "SQLITE3|the connection is the database file", ;
        "ODBC|a connection string: DSN=name;UID=user;PWD=pass", ;
        "|or Driver={...};Server=...;Database=...", ;
        "", ;
        "SELECT (also WITH, PRAGMA, SHOW) opens the rows in a window. They are a", ;
        "copy in memory: you can index, filter, relate and edit them, and save", ;
        "them with Copy To - changes do not go back to the server.", ;
        "", ;
        "Other statements (INSERT, UPDATE, DELETE, CREATE ...) run on the server;", ;
        "CBro shows the number of affected rows." } }, ;
    { "xlsx", "Excel (.xlsx)", { ;
        "# Excel workbooks", ;
        "", ;
        "Open Excel Sheet|a sheet becomes a table in memory: the first row", ;
        "|can name the fields, types (number, date, logical,", ;
        "|text) and sizes come from the data", ;
        "Database Selection|.xlsx files are listed with the tables", ;
        "Copy To > Excel|writes the chosen fields and records as .xlsx", ;
        "|(heading row, column widths, dates, decimals)", ;
        "Append From|the rows of a sheet into the current table; the", ;
        "|heading names the fields, else by position", ;
        "", ;
        "Reading and writing use the XLSX class of Srđan Dragojlović", ;
        "(github.com/digikv/xlsx). Text that is not UTF-8 is converted with the", ;
        "table's data codepage." } }, ;
    { "scripts", "Scripts", { ;
        "# Scripts", ;
        "", ;
        "A script is Harbour source (.prg), compiled in memory when it runs, or a", ;
        "compiled .hrb file (harbour -gh script.prg). Its first procedure runs;", ;
        "the current table is selected, every Harbour function can be used.", ;
        "", ;
        "Alt-K|Run Script", ;
        "Command > Script Editor|write, open, save and run", ;
        "cbro /p:file.prg|runs a script after the start", ;
        "", ;
        "Functions for scripts:", ;
        "CBro_Message( c )|message box", ;
        "CBro_Confirm( c )|.T. when OK is pressed", ;
        "CBro_Ask( cLabel, x )|asks for a value, NIL = cancelled", ;
        "CBro_ShowText( cT, x )|text window (string or array of lines)", ;
        "CBro_Open( cFile )|opens a table / sheet / server table", ;
        "CBro_Alias( [n] )|alias of the current (or n-th) table", ;
        "CBro_Tables()|number of open tables", ;
        "CBro_Refresh()|shows changes made by the script", ;
        "CBro_Sql( cDrv, cCon, cSql )|SQL (see SQL): SELECT opens a window", ;
        "", ;
        "After the script every window is refreshed." } }, ;
    { "views", "Views", { ;
        "# Views (.cbv)", ;
        "", ;
        "Alt-W saves the open tables in a view file: file, driver, alias, data", ;
        "codepage, open mode, index files, order, filter, record, columns", ;
        "(heading, width, picture, expressions), locked columns, color", ;
        "condition, memo width and relations. Alt-V closes the open tables and", ;
        "opens the ones of a view. The file is JSON and can be edited." } }, ;
    { "cmdline", "Command line", { ;
        "# Command line parameters", ;
        "", ;
        "cbro [file [index]] [options]", ;
        "", ;
        "file|a .dbf, an .xlsx or leto://host:port/path/file.dbf", ;
        "index|an index file opened with it", ;
        "/X  /S|open EXCLUSIVE / SHARED", ;
        "/R|read only", ;
        "/M|modify disabled", ;
        "/D:driver|DBFCDX, DBFNTX or LETO", ;
        "/C:codepage|data codepage, e.g. /C:CP852", ;
        "/L://host:port/|LetoDB server", ;
        "/T:theme|theme name (Classic, Blue, Dark, ...)", ;
        "/P:script.prg|run a script after the start", ;
        "/?|this help", ;
        "", ;
        "/DEMO|the demo tables CUSTOMER and ORDERS, related: made next", ;
        "|to the program when missing (read-only folder: in TEMP)", ;
        "", ;
        "Without a file CBro starts with the Database Selection screen." } }, ;
    { "expr", "Expressions", { ;
        "# Expressions", ;
        "", ;
        "Filters, FOR / WHILE conditions, keys, relations, color conditions and", ;
        "the evaluator take Harbour expressions on the fields of the table:", ;
        "", ;
        "  CITY = 'Beograd' .AND. BALANCE > 100", ;
        "  Upper( NAME ) = 'PETAR'", ;
        "  BORN >= CToD( '01.01.1970' )", ;
        "  ! Deleted()", ;
        "", ;
        "A field of another open table: ALIAS->FIELD." } }, ;
    { "f9", "F9 Exit", { ;
        "# F9 Exit", ;
        "", ;
        "Exit|closes every table and ends CBro (Alt-F9)" } } }
