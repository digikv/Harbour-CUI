@echo off
rem CuiBuilder build (Windows) - README.md, "Building CuiBuilder"
rem   build          the Harbour-CUI library (when it is not built yet), then cuibuilder and cuibc
rem   build all      the library again, then cuibuilder and cuibc
rem HBMK2  hbmk2.exe to use (default: D:\tools\harbour-core\bin\win\mingw\hbmk2.exe)
setlocal
cd /d "%~dp0"
if not defined HBMK2 set HBMK2=D:\tools\harbour-core\bin\win\mingw\hbmk2.exe
if not exist "%HBMK2%" (
   echo Harbour was not found: %HBMK2%
   echo Build Harbour first, or tell where it is:
   echo    set HBMK2=C:\harbour\bin\win\mingw\hbmk2.exe
   exit /b 1
)

rem the MinGW C compiler: from the PATH, else from MSYS2 in C:\msys64
where gcc >nul 2>nul
if errorlevel 1 if exist C:\msys64\mingw32\bin\gcc.exe set "PATH=C:\msys64\mingw32\bin;%PATH%"
where gcc >nul 2>nul
if errorlevel 1 (
   echo The C compiler gcc.exe was not found ^(MSYS2 and MinGW, see the README of CEdit^).
   exit /b 1
)

set ARGS=%*
set ALL=
if /i "%1"=="all" set ALL=1
if defined ALL set ARGS=%2 %3 %4 %5 %6
if not defined ALL if exist ..\..\lib\win\mingw\libhbtui.a goto builder

echo === Harbour-CUI library
pushd ..\..
"%HBMK2%" hbtui.hbp
if errorlevel 1 (
   popd
   echo The library was not built - see the messages above.
   exit /b 1
)
popd

:builder
echo === CuiBuilder
"%HBMK2%" cuibuilder.hbp %ARGS%
if errorlevel 1 (
   echo CuiBuilder was not built - see the messages above.
   exit /b 1
)
echo === cuibc
"%HBMK2%" cuibc.hbp %ARGS%
if errorlevel 1 (
   echo cuibc was not built - see the messages above.
   exit /b 1
)
echo Done: cuibuilder.exe and cuibc.exe   ^(run: cuibuilder project.hwprj^)
