@echo off
setlocal EnableExtensions DisableDelayedExpansion
call "%~dp0findrsvars.bat"
if not defined PV_RSVARS goto noDelphi
call "%PV_RSVARS%"
if errorlevel 1 exit /b %errorlevel%

set "project=%~1"
if not defined project set "project=%~dp0projectmanager.dproj"
set "config=%~2"
if not defined config set "config=Release"
set "platformArg="
if not "%~3"=="" set "platformArg=/p:Platform=%~3"
msbuild "%project%" /t:Rebuild /p:Config=%config% %platformArg%
exit /b %errorlevel%

:noDelphi
if not "%~1"=="" (
  echo Could not find Delphi. Set BDS to the RAD Studio installation directory. 1>&2
  exit /b 1
)
where /q fpc
if errorlevel 1 (
  echo Could not find Delphi or Free Pascal. Set BDS or add fpc.exe to PATH. 1>&2
  exit /b 1
)
call "%~dp0makefpc.bat"
exit /b %errorlevel%
