@echo off
rem CEdit build (Windows)
rem HBMK2   hbmk2 to use (default: the newest Harbour, D:\tools\harbour-core)
if not defined HBMK2 set HBMK2=D:\tools\harbour-core\bin\win\mingw\hbmk2.exe
"%HBMK2%" cedit.hbp %1 %2 %3 %4
