/*
 * $Id$
 *
 * CUI - Harbour Character UI library source code:
 * Window class
 *
 * Copyright 2021-2025 Srđan Dragojlović
 *
*/

#INCLUDE "hbclass.ch"
#INCLUDE 'hbgtinfo.ch'
#include "hbextlng.ch"
#INCLUDE "hbclass.ch"
#include "button.ch"  

STATIC aWindows
STATIC oActiveWindow
MEMVAR GetList

CLASS Window
DATA oParent
DATA id
DATA bInit
DATA nIdle  
DATA GetList INIT {}
DATA cBackImage
DATA nTop, nLeft, nRight, nBottom
DATA colorhead INIT "0/11"
DATA colorpanel INIT "0/9,9/0"
DATA title INIT ""
DATA lShadow INIT .T.
DATA shadrow, shadcol, upshadcol
DATA nWidth, nHeight
DATA cCloseSym INIT hb_UTF8ToStr( "x" )
METHOD New(nTop, nLeft, nBottom, nRight, cTitle )
METHOD Show()
METHOD Hide()
METHOD Move( nH1, nH2, nV2, nMCol, nMRow )
METHOD MoveControls( nTop, nLeft, nBottom, nRight )                            
METHOD MouseEvent( nMRow, nMCol )
ENDCLASS

METHOD Move(nMCol, nMRow, nDeltaCol, nDeltaRow ) CLASS Window
LOCAL cImage := SaveScreen( ::nTop, ::nLeft, ::shadrow, ::upshadcol )
local nOldRow := Row(), nOldCol := Col(), nOldTop := ::nTop, nOldLeft := ::nLeft
local nCurMRow, nCurMCol
   while MLeftDown()
      nCurMRow := MRow()
      nCurMCol := MCol()
      if (nCurMRow - nMRow > 0) .AND. (nCurMCol - nMCol >= 0) .AND. (nCurMRow - nMRow + ::nHeight ) < MaxRow() .AND. (nCurMCol - nMCol + ::nWidth < MaxCol()) 
            DispBegin()
            RestScreen( ::nTop, ::nLeft, ::shadrow, ::upshadcol, ::cBackImage )
            ::nTop  = nCurMRow - nMRow
            ::nLeft = nCurMCol - nMCol
            ::nBottom = ::nTop + ::nHeight - 1
            ::nRight = ::nLeft + ::nWidth - 1
            ::shadrow   := ::nBottom + 1
            ::shadcol   := ::nLeft + 1
            ::upshadcol := ::nRight + 2           
            ::cBackImage = SaveScreen( ::nTop, ::nLeft, ::shadrow, ::upshadcol )
            RestScreen( ::nTop, ::nLeft, ::shadrow, ::upshadcol, cImage )
            DispEnd()
      endif
   end
   nDeltaRow := ::nTop-nOldTop
   nDeltaCol := ::nLeft-nOldLeft
   SetPos(nOldRow+nDeltaRow, nOldCol+nDeltaCol)
return self

METHOD MouseEvent( nMRow, nMCol ) CLASS Window
   local nPrevCursor := SetCursor( 0 )
   local nDeltaCol := 0, nDeltaRow := 0
   IF nMRow == ::nTop .and. nMCol == ::nRight -1 .and. MLeftDown() // close button
         ReadKill( .T. )
   ELSEIF MLeftDown() .and. ( nMRow == ::nTop .and. nMCol >= ::nLeft .and. nMCol <= ::nRight ) 
      ::Move( nMCol, nMRow, @nDeltaCol, @nDeltaRow )
      // alert(hb_ntos(nMCol)+","+hb_ntos(nMRow))
   ENDIF
   if (nDeltaCol<>0 .OR. nDeltaRow<>0)
      ::MoveControls( nDeltaCol, nDeltaRow )
   endif
   SetCursor( nPrevCursor )
return nil

METHOD New(nTop, nLeft, nBottom, nRight, cTitle ) CLASS Window
   if !HB_ISARRAY( aWindows ); aWindows := {}; ENDIF
      ::nTop := nTop
      ::nLeft := nLeft
      ::nBottom := nBottom
      ::nRight := max( nRight, len(alltrim(cTitle))+5 )
      ::nHeight := ::nBottom - ::nTop + 1
      ::nWidth := ::nRight - ::nLeft + 1
      ::shadrow   := nBottom + 1
      ::shadcol   := nLeft + 1
      ::upshadcol := ::nRight + 2
      IF cTitle != NIL
         ::title := cTitle
      END
	  ::ID := LEN(aWindows)+1
Return AddWindow( self )

STATIC FUNCTION AddWindow( o )
   aAdd( aWindows, o )
	oActiveWindow := o
return o

METHOD Show() CLASS Window
Local dX
IF ::title != ""
   IF ::nLeft + 3 + LEN( ::title ) > ::nRight
      dX := ( ::nLeft + 4 + LEN( ::title ) - ::nRight )
      ::nRight += dX
      ::upshadcol += dX
   END
END
::cBackImage := SaveScreen( ::nTop, ::nLeft, ::shadrow, ::upshadcol )
DISPBEGIN()
   IF ::lShadow
	   hb_Shadow( ::nTop, ::nLeft, ::nBottom, ::nRight )
   ENDIF
   @ ::nTop, ::nLeft SAY ' '+PAD( ::title, ::nRight - ::nLeft ) COLOR ::colorhead
   @ ::nTop, ::nRight-1 SAY ::cCloseSym COLOR ::colorhead
   setcolor( oApp:deskcolor )
   Scroll( ::nTop+1, ::nLeft, ::nBottom, ::nRight )   
DISPEND()
oActiveWindow := self
::nIdle = hb_IdleAdd( { || ::MouseEvent( MRow(), MCol() ) } )
return self

METHOD Hide() CLASS Window
Local i, nPOS := 0 
FOR I:=LEN( aWindows ) TO 1 STEP -1
   if aWindows[i]:id == ::id
	   nPos := i
	   EXIT
	ENDIF
NEXT I	
RestScreen( ::nTop, ::nLeft, ::shadrow, ::upshadcol, ::cBackImage )
hb_adel( aWindows, nPos, .T. )
if ::nIdle != nil
   hb_IdleDel( ::nIdle )
   ::nIdle = nil
endif
return self

METHOD MoveControls( nDeltaCol, nDeltaRow ) CLASS Window 
   local oCtrl
   if LEN(GetList)>0 .AND. (nDeltaCol<>0 .OR. nDeltaRow<>0) 
      for each oCtrl in GetList
         oCtrl:row += nDeltaRow 
         oCtrl:col += nDeltaCol 
         if ! Empty( oCtrl:Caption )
            oCtrl:CapRow += nDeltaRow 
            oCtrl:CapCol += nDeltaCol 
         endif
         if ! Empty( oCtrl:Control )
            if oCtrl:Control:IsKindOf( "PUSHBUTTON" )
               oCtrl:Control:row += nDeltaRow 
               oCtrl:Control:col += nDeltaCol 
            endif   
            if oCtrl:Control:IsKindOf( "LISTBOX" ) 
               oCtrl:Control:top    += nDeltaRow 
               oCtrl:Control:left   += nDeltaCol 
               oCtrl:Control:bottom += nDeltaRow
               oCtrl:Control:right  += nDeltaCol
               oCtrl:Control:CapRow += nDeltaRow 
               oCtrl:Control:CapCol += nDeltaCol 
            endif
         endif
      next 
   endif   
return nil   


CLASS Label
   DATA nLeft, nRight
   DATA cCaption
   DATA cColor
   METHOD New( oWin, nLeft, nRight, cCaption, cColor )
ENDCLASS

METHOD New(nLeft,nRight, cCaption, cColor )
   ::nLeft := nLeft
   ::nRight := nRight
   ::cCaption := cCaption
   ::cColor := cColor
RETURN self

FUNCTION CUI_GetActiveWindow()
Return oActiveWindow

Func WinNew( nTop, nLeft, nBottom, nRight, cTitle )
Return Window():New( nTop, nLeft, nBottom, nRight, cTitle )

Proc WinShow( aWin )
aWin:show()
Return

Proc WinHide( aWin )
aWin:Hide()
Return

Func GetTop( aWin )
Return aWin:nTop

Func GetLeft( aWin )
Return aWin:nLeft

CLASS Application
DATA oActiveWindow
DATA row INIT 30
DATA col INIT 90
DATA aWindows INIT {}
DATA activewindow INIT 0
DATA browsecolor INIT '0/9,N/W*'
DATA editcolor INIT 'G/N'
DATA infocolor INIT 'w/b,r/w'
DATA menucolorn INIT '0/10,N/14,R/10,R/14,0/10,0/10'
DATA menucolori INIT '0/10,N/14,R/10,R/14,0/10,0/10'
DATA statusbarc INIT 'n/w*,w*/n'
DATA deskcolor INIT '0/9,N/W*,0/9,N/W*'
DATA memopanel INIT 'R/G'
DATA memohead INIT 'N/B+'
DATA combocolor 
DATA checkboxcolor
METHOD New()
ENDCLASS

METHOD New() CLASS Application
LOCAL aTemp := hb_ATokens( ALLTRIM(::deskcolor), "," )
hb_gtInfo( HB_GTI_BOXCP, hb_cdpSelect() )
hb_gtInfo( HB_GTI_COMPATBUFFER, .F. )

Hb_gtinfo( HB_GTI_PALETTE, 9, rgb( 160, 160, 160 ) ) // window
Hb_gtinfo( HB_GTI_PALETTE, 10, rgb( 240, 240, 240 ) ) // menu bar
Hb_gtinfo( HB_GTI_PALETTE, 11, rgb( 153, 180, 209 ) ) // active title
Hb_gtinfo( HB_GTI_PALETTE, 12, rgb( 171, 171, 171 ) ) // Appworkspace
Hb_gtinfo( HB_GTI_PALETTE, 13, rgb( 128, 128, 128 ) ) // 248 Dark Grey
Hb_gtinfo( HB_GTI_PALETTE, 14, rgb( 166, 202, 240 ) ) // 9 Pastel blue
Hb_gtinfo( HB_GTI_PALETTE, 15, rgb( 169, 169, 169 ) ) // Dark Grey

aTemp[1]:=RIGHT(aTemp[1],3)
::combocolor := aTemp[2]+","+aTemp[1]+","+aTemp[1]+","+aTemp[2]+","+RIGHT(aTemp[1],1)+"/"+RIGHT(aTemp[1],1)+","+aTemp[1]+","+aTemp[1]+","+aTemp[1] 

hb_gtInfo( HB_GTI_FONTNAME, 'Consolas' )
hb_gtInfo( HB_GTI_SELECTCOPY, "Select and Copy" )

hb_gtInfo( HB_GTI_RESIZEMODE, HB_GTI_RESIZEMODE_FONT )
hb_gtInfo( HB_GTI_FONTQUALITY, HB_GTI_FONTQ_HIGH )
hb_gtInfo( HB_GTI_FONTWEIGHT, iif( .F., HB_GTI_FONTW_BOLD, HB_GTI_FONTW_NORMAL ) )

hb_GtInfo( HB_GTI_CLOSABLE, .T. ) 
Hb_GtInfo( HB_GTI_MAXIMIZED, .F. ) 
hb_gtInfo( HB_GTI_RESIZABLE, .F. ) 
HB_GtInfo( HB_GTI_ISFULLSCREEN, .F. ) 
HB_GtInfo( HB_GTI_ALTENTER, .T. ) 
Hb_GtInfo( HB_GTI_COMPATBUFFER, .T. ) 

setmode( ::row, ::col )
hb_gtInfo(HB_GTI_FONTWIDTH, INT( hb_gtInfo( HB_GTI_DESKTOPWIDTH ) / ( MAXCOL()+1 ) ) )
hb_gtInfo(HB_GTI_FONTSIZE , INT( hb_gtInfo( HB_GTI_DESKTOPHEIGHT ) / ( MAXROW()+1 ) ) )

//hb_gtInfo( HB_GTI_ICONFILE, "cui.ico" )
hb_gtInfo( HB_GTI_WINTITLE, "Main window" )
setcolor( ::deskcolor )
CLS
Return Self

#pragma BEGINDUMP

#include "hbapigt.h"
#include "hbgtcore.h"
#include "hbapiitm.h"
#include "hbapicdp.h"
#include "hbset.h"
#include "hbstack.h"
#include "hbvm.h"

HB_FUNC( RGB )
{  
   const unsigned char r = hb_parni( 1 );
   const unsigned char g = hb_parni( 2 );
   const unsigned char b = hb_parni( 3 );
   
   hb_retnl( (r<<16) | (g<<8) | b );
}

#pragma ENDDUMP
