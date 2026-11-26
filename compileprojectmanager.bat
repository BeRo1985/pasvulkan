@echo off
setlocal
pushd "%~dp0src\tools\projectmanager" || exit /b 1
call make.bat
set "result=%errorlevel%"
popd
exit /b %result%
