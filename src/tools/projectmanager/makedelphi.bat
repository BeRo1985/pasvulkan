@echo off
call "%~dp0make.bat" "%~dp0projectmanager.dproj" Release
exit /b %errorlevel%
