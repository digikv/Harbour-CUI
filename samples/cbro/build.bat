@echo off
rem CBro build (Windows)
rem   build              LetoDBf client (default)
rem   build letodb       LetoDB client
rem   build commercial   without the Harbour compiler (GPL, no linking exception):
rem                      for programs distributed under the commercial license
rem   build commercial letodb
rem
rem HBMK2        hbmk2 to use (default: the newest Harbour, D:\tools\harbour-core)
rem CBRO_XLSXDIR folder of XLSXClass.prg (default: ..\..\..\xlsx)
if not defined HBMK2 set HBMK2=D:\tools\harbour-core\bin\win\mingw\hbmk2.exe
set OPT=
if defined CBRO_XLSXDIR set OPT=-env:CBRO_XLSXOWN=yes "%CBRO_XLSXDIR%\XLSXClass.prg"
if /i "%1"=="commercial" (
   set OPT=%OPT% -env:CBRO_NOCOMPILER=yes
   shift
)
if /i "%1"=="letodb" (
   "%HBMK2%" cbro.hbp %OPT% -env:CBRO_LETODB=yes %2 %3 %4
) else (
   "%HBMK2%" cbro.hbp %OPT% %1 %2 %3 %4
)
