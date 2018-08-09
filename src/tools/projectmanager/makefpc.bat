@echo off
mkdir fpctemp
fpc -Sd -B -FEfpctemp -o../../../projectmanager.exe projectmanager.dpr
rmdir /s /q fpctemp

