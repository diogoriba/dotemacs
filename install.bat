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
DEL %HOME%\.emacs.elc
DEL %HOME%\diogoriba.config.elc
DEL /S %HOME%\.elisp\*.elc
XCOPY * %HOME%\ /H /E /Y /F /G /EXCLUDE:xcopy.ignore

:CLEANUP
ECHO Cleaning xcopy ignore file
DEL xcopy.ignore
