@echo off 
call rsvars.bat
msbuild examples.dproj /t:Rebuild /p:Config=Release
