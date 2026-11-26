@echo off
setlocal
if not exist fpctemp mkdir fpctemp
if not exist fpctemp exit /b 1
call fpc -Sd -B -FEfpctemp -FUfpctemp -o../../../projectmanager.exe projectmanager.dpr
set "result=%errorlevel%"
rmdir /s /q fpctemp
exit /b %result%
