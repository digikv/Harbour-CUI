#command READ           => CUI_GetActiveWindow():GetList := GetList; ReadModal( GetList ) ; Getlist := {} ; ( Getlist )
#command READ SAVE      => ReadModal( GetList )
#command CLEAR GETS     => ReadKill( .T. ) ; GetList := {} ; ( GetList )

         
#command @ <row>, <col> SAY <say> [<sayexp,...>] GET <get> [<getexp,...>] => ;
        @ <row>+CUI_GetActiveWindow():nTop+1, <col>+CUI_GetActiveWindow():nLeft SAY <say> [ <sayexp>] ;;        
        @ Row(), Col() + 1 GET <get> [ <getexp>]

#command @ <row>, <col> GET <v> [<exp,...>] RANGE <l>, <h> [<nextexp,...>] => ;
         @ <row>, <col> GET <v> [ <exp>] ;
                        VALID {| _1 | RangeCheck( _1,, <l>, <h> ) } [ <nextexp>]

#command @ <row>, <col> GET <v> [<exp,...>] COLOR <clr> [<nextexp,...>] => ;
         @ <row>, <col> GET <v> [ <exp>] SEND colorDisp( <clr> ) [ <nextexp>]

#command READ [MENU <oMenu>] [MSG AT <row>, <left>, <right> [MSG COLOR <color>]] => ;
        ReadModal( GetList, NIL, <oMenu>, <row>, <left>, <right>, <color> ) ;;
        GetList := {} ; ( GetList )

#command READ SAVE [MENU <oMenu>] [MSG AT <row>, <left>, <right> [MSG COLOR <color>]] => ;
        ReadModal( GetList, NIL, <oMenu>, <row>, <left>, <right>, <color> )

#command @ <row>, <col> GET <v> [PICTURE <pic>] ;
                   [VALID <valid>] [WHEN <when>] [SEND <snd>] ;
                   [CAPTION <cap>] [MESSAGE <msg>] => ;
    SetPos( <row>, <col> ) ;;
    AAdd( GetList, _GET_( <v>, <"v">, <pic>, <{valid}>, <{when}> ) ) ;;
    [ ATail( GetList ):Caption := <cap> ;;
    ATail( GetList ):CapRow := ATail( Getlist ):row ;;
    ATail( GetList ):CapCol := ATail( Getlist ):col - __CapLength( <cap> ) - 1 ;] ;
    [ ATail( GetList ):message := <msg> ;] [ ATail( GetList ):<snd> ;] ;
    ATail( GetList ):Display()

#command @ <row>, <col> GET <v> CHECKBOX [VALID <valid>] [WHEN <when>] ;
                   [CAPTION <cap>] [MESSAGE <msg>] [COLOR <clr>] ;
                   [FOCUS <fb>] [STATE <sb>] [STYLE <stl>] ;
                   [SEND <snd>] [GUISEND <gsnd>] [BITMAPS <bmaps>] => ;
 SetPos( <row>+CUI_GetActiveWindow():nTop+1, <col>+CUI_GetActiveWindow():nLeft ) ;;
 AAdd( GetList, _GET_( <v>, <(v)>, NIL, <{valid}>, <{when}> ) ) ;;
 ATail( GetList ):Control := _CheckBox_( <v>, <cap>, <msg>, <clr>, <{fb}>, <{sb}>, <stl>, <bmaps> ) ;;
 ATail( GetList ):reader := {| a, b, c, d | GUIReader( a, b, c, d ) };;
 [ ATail( GetList ):<snd> ;] [ ATail( GetList ):Control:<gsnd> ;] ;
 ATail( GetList ):Control:Display()

#command @ <top>, <left>, <bottom>, <right> GET <v> LISTBOX <items> ;
          [VALID <valid>] [WHEN <when>] ;
          [CAPTION <cap>] [MESSAGE <msg>] [COLOR <clr>] ;
          [FOCUS <fb>] [STATE <sb>] [<dd:DROPDOWN>] [<sbar:SCROLLBAR>] ;
          [SEND <snd>] [GUISEND <gsnd>] [BITMAP <bmap>] => ;
 SetPos( <top>+CUI_GetActiveWindow():nTop+1, <left>+CUI_GetActiveWindow():nLeft ) ;;
 AAdd( GetList, _GET_( <v>, <(v)>, NIL, <{valid}>, <{when}> ) ) ;;
 ATail( GetList ):Control := _ListBox_( ATail( Getlist ):row, ATail( Getlist ):col, <bottom>, <right>, <v>, <items>, <cap>, <msg>, <clr>, <{fb}>, <{sb}>, <.dd.>, <.sbar.>, <bmap> ) ;;
 ATail( GetList ):reader := {| a, b, c, d | GUIReader( a, b, c, d ) } ;;
[ ATail( GetList ):<snd> ;] [ ATail( GetList ):Control:<gsnd> ;] ;
 ATail( GetList ):Control:Display()

#command @ <row>, <col> GET <v> PUSHBUTTON ;
          [VALID <valid>] [WHEN <when>] ;
          [CAPTION <cap>] [MESSAGE <msg>] [COLOR <clr>] ;
          [FOCUS <fb>] [STATE <sb>] [STYLE <stl>] ;
          [SEND <snd>] [GUISEND <gsnd>] [BITMAP <bmap>] ;
          [SIZE X <sX> Y <sY>] [CAPOFF X <cX> Y <cY>] ;
          [BMPOFF X <bX> Y <bY>] => ;
 SetPos( <row>+CUI_GetActiveWindow():nTop+1, <col>+CUI_GetActiveWindow():nLeft ) ;;
 AAdd( GetList, _GET_( <v>, <(v)>, NIL, <{valid}>, <{when}> ) ) ;;
 ATail( GetList ):Control := _PushButt_( <cap>, <msg>, <clr>, <{fb}>, <{sb}>, <stl>, <sX>, <sY>, <cX>, <cY>, <bmap>, <bX>, <bY> ) ;;
 ATail( GetList ):reader := {| a, b, c, d | GUIReader( a, b, c, d ) } ;;
[ ATail( GetList ):<snd> ;] [ ATail( GetList ):Control:<gsnd> ;] ;
 ATail( GetList ):Control:Display()

#command @ <top>, <left>, <bottom>, <right> GET <v> RADIOGROUP <buttons> ;
          [VALID <valid>] [WHEN <when>] ;
          [CAPTION <cap>] [MESSAGE <msg>] [COLOR <clr>] ;
          [FOCUS <fb>] [STYLE <stl>] ;
          [SEND <snd>] [GUISEND <gsnd>] => ;
 SetPos( <top>+CUI_GetActiveWindow():nTop+1, <left>+CUI_GetActiveWindow():nLeft ) ;;
 AAdd( GetList, _GET_( <v>, <(v)>, NIL, <{valid}>, <{when}> ) ) ;;
 ATail( GetList ):Control := _RadioGrp_( ATail( Getlist ):row, ATail( Getlist ):col, <bottom>, <right>, <v>, <buttons>, <cap>, <msg>, <clr>, <{fb}>, <stl> ) ;;
 ATail( GetList ):reader := {| a, b, c, d | GUIReader( a, b, c, d ) } ;;
[ ATail( GetList ):<snd> ;] [ ATail( GetList ):Control:<gsnd> ;] ;
 ATail( GetList ):Control:Display()

#command @ <top>, <left>, <bottom>, <right> GET <v> TBROWSE <oBrowse> ;
          ALIAS <a> [VALID <valid>] [WHEN <when>] ;
          [MESSAGE <msg>] [SEND <snd>] [GUISEND <gsnd>] => ;
    SetPos( <top>+CUI_GetActiveWindow():nTop+1, <left>+CUI_GetActiveWindow():nLeft ) ;;
    AAdd( GetList, _GET_( <v>, <(v)>, NIL, <{valid}>, <{when}> ) ) ;;
    <oBrowse>:ntop := ATail( Getlist ):row ;;
    <oBrowse>:nleft := ATail( Getlist ):col ;;
    <oBrowse>:nbottom := <bottom> ; <oBrowse>:nright := <right> ;;
    <oBrowse>:Configure() ; ATail( GetList ):Control := <oBrowse> ;;
    ATail( GetList ):reader := {| a, b, c, d | <a>->( TBReader( a, b, c, d ) ) } ;
    [ ; ATail( GetList ):Control:Message := <msg>] ;
    [ ; ATail( GetList ):<snd>] [ ; ATail( GetList ):Control:<gsnd>]