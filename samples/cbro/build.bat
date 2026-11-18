@echo off
rem CBro build (Windows)
rem   build              LetoDBf client (default)
rem   build letodb       LetoDB client
rem   build commercial   without the Harbour compiler (GPL, no linking exception):
rem                      for programs distributed under the commercial license
rem   build commercial letodb
rem   build update ...   first update the XLSX class (git pull in ..\..\..\xlsx)
rem
rem The XLSX class (https://github.com/digikv/xlsx) is taken from ..\..\..\xlsx, next to
rem the Harbour-CUI folder. When it is not there, it is cloned with git; without git
rem CBro is built without Excel (.xlsx).
rem
rem HBMK2        hbmk2 to use (default: the newest Harbour, D:\tools\harbour-core)
rem CBRO_XLSXDIR folder of XLSXClass.prg (default: ..\..\..\xlsx)
if not defined HBMK2 set HBMK2=D:\tools\harbour-core\bin\win\mingw\hbmk2.exe
set OPT=
if /i "%1"=="update" (
   if exist ..\..\..\xlsx\.git git -C ..\..\..\xlsx pull --ff-only
   shift
)
if defined CBRO_XLSXDIR (
   set OPT=-env:CBRO_XLSXOWN=yes "%CBRO_XLSXDIR%\XLSXClass.prg"
) else if not exist ..\..\..\xlsx\XLSXClass.prg (
   where git >nul 2>nul && git clone https://github.com/digikv/xlsx ..\..\..\xlsx
   if not exist ..\..\..\xlsx\XLSXClass.prg (
      echo Note: ..\..\..\xlsx\XLSXClass.prg not found - CBro is built without Excel ^(.xlsx^).
      echo       For .xlsx: git clone https://github.com/digikv/xlsx next to the Harbour-CUI folder.
   )
)
if /i "%1"=="commercial" (
   set OPT=%OPT% -env:CBRO_NOCOMPILER=yes
   shift
)
if /i "%1"=="letodb" (
   "%HBMK2%" cbro.hbp %OPT% -env:CBRO_LETODB=yes %2 %3 %4
) else (
   "%HBMK2%" cbro.hbp %OPT% %1 %2 %3 %4
)
