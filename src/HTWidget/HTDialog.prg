/** @class HTDialog
 * Modal dialog window with a nested event loop.
 * exec() blocks until closed via accept() or reject(), returning a result code.
 * @extends HTWidget
 *
 * @example
 *   oDlg := HTDialog():new( oParent, "Confirm" )
 *   oDlg:resize( 20, 10 )
 *   /* add child widgets to oDlg... */
 *   IF oDlg:exec() = HT_DIALOG_ACCEPTED
 *      /* user pressed Enter/accepted */
 *   ENDIF
 *
 * @see HTMessageBox, HTMainWindow
 */

#include "hbtui.ch"
#include "inkey.ch"


CLASS HTDialog FROM HTWidget

PROTECTED:

    DATA FresultCode     INIT HT_DIALOG_REJECTED
    DATA Frunning        INIT .F.

PUBLIC:

    CONSTRUCTOR new( parent, cTitle )

    METHOD exec()
    METHOD accept()
    METHOD reject()
    METHOD resultCode()  INLINE ::FresultCode
    METHOD addButtonBar( ... )

    METHOD move( ... )
    METHOD resize( ... )
    METHOD mouseEvent( eventMouse )
    METHOD keyEvent( keyEvent )

ENDCLASS

/** Creates a new modal dialog.
 * @param parent Parent widget
 * @param cTitle Optional window title
 */
METHOD new( parent, cTitle ) CLASS HTDialog

    ::super:new( parent, HT_DIALOG )

    IF cTitle != NIL
        ::setWindowTitle( cTitle )
    ENDIF

    ::Fheight := 10
    ::Fwidth := 40

RETURN self

/** Runs the modal event loop. Blocks until accept() or reject() is called.
 * @return HT_DIALOG_ACCEPTED or HT_DIALOG_REJECTED
 */
METHOD FUNCTION exec() CLASS HTDialog

    LOCAL aFocusable

    ::FisVisible := .T.
    ::Frunning := .T.

    /* paint the dialog */
    ::paintEvent( HTPaintEvent():new() )

    /* auto-focus first focusable child */
    IF ::FfocusWidget = NIL
        aFocusable := ::focusableChildren()
        IF Len( aFocusable ) > 0
            ::FfocusWidget := aFocusable[ 1 ]
        ENDIF
    ENDIF

    /* modal event loop — uses shared HTEventLoop for mouse tracking + tasks */
    HTEventLoop():run( self, {|| ::Frunning } )

    /* close the dialog window and unregister from top-level windows */
    IF ::FwindowId != NIL
        HTApplication():removeTopLevelWindow( ::FwindowId )
        wClose( ::FwindowId )
        ::FwindowId := NIL
    ENDIF

    ::FisVisible := .F.

RETURN ::FresultCode

/** Closes the dialog with an accepted result code. */
METHOD PROCEDURE accept() CLASS HTDialog
    ::FresultCode := HT_DIALOG_ACCEPTED
    ::Frunning := .F.
RETURN

/** Closes the dialog with a rejected result code. */
METHOD PROCEDURE reject() CLASS HTDialog
    ::FresultCode := HT_DIALOG_REJECTED
    ::Frunning := .F.
RETURN

/** Adds a standard button bar at the bottom of the dialog.
 * With no arguments, creates OK + Cancel. Pass button labels as arguments.
 * First button triggers accept(), last button (if > 1) triggers reject().
 * @param ... Optional button labels (strings). Default: "OK", "Cancel"
 * @return Array of HTPushButton instances for customization
 */
METHOD FUNCTION addButtonBar( ... ) CLASS HTDialog

    LOCAL aLabels := {}
    LOCAL aButtons := {}
    LOCAL i, nBtnX, oBtn, nBtnWidth
    LOCAL nBarRow

    /* collect labels from varargs, or use defaults */
    IF pCount() = 0
        aLabels := { "OK", "Cancel" }
    ELSE
        FOR i := 1 TO pCount()
            IF hb_isString( hb_pValue( i ) )
                AAdd( aLabels, hb_pValue( i ) )
            ENDIF
        NEXT
    ENDIF

    IF Len( aLabels ) = 0
        RETURN aButtons
    ENDIF

    /* position: near bottom of content area */
    nBarRow := ::Fheight - 4

    /* right-align buttons */
    nBtnX := ::Fwidth - 3
    FOR i := Len( aLabels ) TO 1 STEP -1
        nBtnWidth := Max( ht_strWidth( aLabels[ i ] ) + 4, 8 )
        nBtnX -= nBtnWidth + 2
        oBtn := HTPushButton():new( aLabels[ i ], self )
        oBtn:setGeometry( Max( 1, nBtnX ), nBarRow, nBtnWidth, 1 )
        AAdd( aButtons, oBtn )
    NEXT

    /* wire first button to accept, last to reject (if more than one) */
    IF Len( aButtons ) >= 1
        aButtons[ 1 ]:onClicked := {|| ::accept() }
    ENDIF
    IF Len( aButtons ) >= 2
        ATail( aButtons ):onClicked := {|| ::reject() }
    ENDIF

RETURN aButtons

/** Synchronous move — sets position immediately (no event queuing).
 * Before exec(): sets geometry for paintTopLevelWindow; a dialog moved
 * before exec() opens there (else centered).
 * @param ... HTPoint or (x, y) numeric pair
 */
METHOD PROCEDURE move( ... ) CLASS HTDialog
    LOCAL nNewX, nNewY
    IF pCount() = 2 .AND. hb_isNumeric( hb_pValue( 1 ) ) .AND. hb_isNumeric( hb_pValue( 2 ) )
        nNewX := hb_pValue( 1 )
        nNewY := hb_pValue( 2 )
    ELSEIF pCount() = 1 .AND. hb_isObject( hb_pValue( 1 ) )
        nNewX := hb_pValue( 1 ):x
        nNewY := hb_pValue( 1 ):y
    ELSE
        RETURN
    ENDIF
    ::Fx := nNewX
    ::Fy := nNewY
    ::FlPosSet := .T.   /* exec() keeps the position instead of centering */
RETURN

/** Synchronous resize — sets dimensions immediately (no event queuing).
 * @param ... HTSize or (width, height) numeric pair
 */
METHOD PROCEDURE resize( ... ) CLASS HTDialog
    IF pCount() = 2 .AND. hb_isNumeric( hb_pValue( 1 ) ) .AND. hb_isNumeric( hb_pValue( 2 ) )
        ::Fwidth  := hb_pValue( 1 )
        ::Fheight := hb_pValue( 2 )
    ELSEIF pCount() = 1 .AND. hb_isObject( hb_pValue( 1 ) )
        ::Fwidth  := hb_pValue( 1 ):width
        ::Fheight := hb_pValue( 1 ):height
    ENDIF
RETURN

/** Handles mouse events: overrides K_MOUSEMOVE for delta-based window dragging.
 * The base class uses live mRow(.T.) which gives absolute coords incompatible
 * with wMove for dialog windows. This override uses event-captured coords
 * to compute proper deltas.
 * @param eventMouse HTMouseEvent instance
 */
METHOD PROCEDURE mouseEvent( eventMouse ) CLASS HTDialog

    LOCAL nDeltaRow, nDeltaCol

    /* [x] rejects directly: the base class queues an HTCloseEvent on the
       application queue, which the modal loop never drains */
    IF eventMouse:nKey = K_LBUTTONUP .AND. ::FwinSysBtnClose .AND. ;
       ::FbtnClosePos != NIL .AND. eventMouse:mouseRow = 0 .AND. ;
       eventMouse:mouseCol >= ::FbtnClosePos[ 1 ] .AND. ;
       eventMouse:mouseCol <= ::FbtnClosePos[ 2 ]
        ::FwinSysBtnClose := .F.
        ::FposDown := NIL
        ::reject()
        RETURN
    ENDIF

    IF eventMouse:nKey = K_MOUSEMOVE .AND. ::FposDown != NIL .AND. ::FwinSysBtnMove .AND. ::FwindowId != NIL
        /* delta = event mouse position - click offset */
        nDeltaRow := eventMouse:mouseRow - ::FposDown:y
        nDeltaCol := eventMouse:mouseCol - ::FposDown:x
        wSelect( ::FwindowId, .F. )
        wMove( wRow() + nDeltaRow, wCol() + nDeltaCol )
        ::Fy := wRow()
        ::Fx := wCol()
        RETURN
    ENDIF

    /* delegate everything else to base class */
    ::super:mouseEvent( eventMouse )

RETURN

/** Handles key events: ESC rejects the dialog, others delegate to parent.
 * @param keyEvent HTKeyEvent instance
 */
METHOD PROCEDURE keyEvent( keyEvent ) CLASS HTDialog

    /* ESC closes the dialog */
    IF keyEvent:key = K_ESC
        ::reject()
        keyEvent:accept()
        RETURN
    ENDIF

    /* delegate to parent class for Tab/focus/child dispatch */
    ::super:keyEvent( keyEvent )

RETURN
