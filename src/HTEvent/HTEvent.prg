/** @class HTEvent
 * Base event class for the HBTui event system (HB_CLS_NOTOBJECT).
 *
 * NOTE: accept()/ignore() must be real METHODs (not INLINE). Harbour does not
 * reliably export INLINE methods from HB_CLS_NOTOBJECT parents to subclasses
 * (HTPaintEvent, HTKeyEvent, …) — that caused BASE/1004 "No exported method: ACCEPT".
 */

#define HB_CLS_NOTOBJECT

#include "hbtui.ch"

CLASS HTEvent

PUBLIC:

    METHOD accept()
    METHOD ignore()
    METHOD setAccepted( accepted )
    METHOD setWidget( widget ) INLINE ::Fwidget := widget

    PROPERTY isAccepted INIT .T.
    PROPERTY type INIT HT_EVENT_TYPE_NULL
    PROPERTY widget

ENDCLASS

METHOD FUNCTION accept() CLASS HTEvent
    ::FisAccepted := .T.
RETURN .T.

METHOD FUNCTION ignore() CLASS HTEvent
    ::FisAccepted := .F.
RETURN .F.

METHOD FUNCTION setAccepted( accepted ) CLASS HTEvent
    ::FisAccepted := accepted
RETURN ::FisAccepted
