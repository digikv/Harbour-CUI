/** @class HTMouseEvent
 * Mouse input event carrying cursor coordinates and button key code.
 * @extends HTInputEvent
 */

#include "hbtui.ch"

CLASS HTMouseEvent FROM HTInputEvent
PROTECTED:
    DATA Ftype INIT HT_EVENT_TYPE_MOUSE
PUBLIC:
    CONSTRUCTOR new( nKey, nAbsRow, nAbsCol )
    PROPERTY mouseAbsRow
    PROPERTY mouseAbsCol
    PROPERTY mouseCol READWRITE
    PROPERTY mouseRow READWRITE
    PROPERTY nKey
ENDCLASS

/** Creates a new mouse event.
 * @param nKey Inkey mouse key code
 * @param nAbsRow Optional screen-absolute row (from Win mouse queue)
 * @param nAbsCol Optional screen-absolute column
 */
METHOD new( nKey, nAbsRow, nAbsCol ) CLASS HTMouseEvent
    ::FnKey := nKey
    IF nAbsRow != NIL .AND. nAbsCol != NIL
        ::FmouseAbsRow := nAbsRow
        ::FmouseAbsCol := nAbsCol
        ::FmouseRow := nAbsRow
        ::FmouseCol := nAbsCol
    ELSE
        ::FmouseAbsCol := mCol( .t. )
        ::FmouseAbsRow := mRow( .t. )
        ::FmouseCol := mCol()
        ::FmouseRow := mRow()
    ENDIF
RETURN self
