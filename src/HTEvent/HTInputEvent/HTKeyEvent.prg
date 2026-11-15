/** @class HTKeyEvent
 * Keyboard input event carrying key code and character text.
 * @extends HTInputEvent
 */

#include "hbtui.ch"

CLASS HTKeyEvent FROM HTInputEvent

PROTECTED:

    DATA Ftype INIT HT_EVENT_TYPE_KEYBOARD

PUBLIC:

    CONSTRUCTOR new( nKey )
    PROPERTY key INIT 0
    PROPERTY text

ENDCLASS

/** Creates a new key event.
 * @param nKey Inkey key code
 */
METHOD new( nKey ) CLASS HTKeyEvent
    ::Fkey := nKey
    ::Ftext := hb_keyChar( nKey )
RETURN ::super:new()

/** Text a key types into an editor, or "" when it types nothing (navigation,
 * function and Ctrl/Alt keys, or a character the current codepage cannot
 * hold - e.g. Chinese under the default "EN" codepage).
 * One character, but not always one byte: under UTF8EX a CJK key returns a
 * 3-byte UTF-8 string whose Len() is 1, so editors must insert the whole
 * string and advance the cursor by Len(), never test Len( cChar ) = 1 on a
 * byte codepage assumption.
 * @param nKey Inkey key code (standard or Unicode HB_INKEY_* code)
 * @return Character string, or ""
 */
FUNCTION ht_keyText( nKey )

    LOCAL cChar := hb_keyChar( nKey )

    IF cChar == "" .OR. hb_BCode( cChar ) < 32 .OR. cChar == Chr( 127 )
        RETURN ""
    ENDIF

RETURN cChar
