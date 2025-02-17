#include "../cui.ch"
MEMVAR oApp
REQUEST  HB_GT_WVT

Procedure Main()
Local oDialog
Local n := 10
Local cText := "Test string " 
oApp := Application():New()
oDialog:= Window():New( 5,2,10,50,"Test window")
oDialog:Show()
@ 0, 0 SAY "Test 1" GET n
@ 1, 0 SAY "Test 2" GET ctext
READ
oDialog:Hide()
Return
