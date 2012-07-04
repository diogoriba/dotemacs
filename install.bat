@ECHO off
REM Install script for windows
:CONFIG
ECHO Initializing git submodules
call git submodule init
call git submodule update

ECHO Creating xcopy ignore file
ECHO install.bat > xcopy.ignore
ECHO install.sh >> xcopy.ignore
ECHO README.md >> xcopy.ignore
ECHO .git >> xcopy.ignore
ECHO xcopy.ignore >> xcopy.ignore

:INSTALL
IF [%HOME%]==[] SET HOME=%USERPROFILE%
ECHO Installing emacs configuration files
XCOPY * %HOME% /H /E /Y /EXCLUDE:xcopy.ignore

:CLEANUP
ECHO Cleaning xcopy ignore file
DEL xcopy.ignore
