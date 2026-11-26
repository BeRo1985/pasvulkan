@echo off
setlocal EnableExtensions DisableDelayedExpansion
set "rsvars="
if defined BDS if exist "%BDS%\bin\rsvars.bat" if exist "%BDS%\bin\dcc32.exe" (
  set "rsvars=%BDS%\bin\rsvars.bat"
  goto done
)
for %%F in (rsvars.bat) do set "rsvars=%%~$PATH:F"
if defined rsvars for %%F in ("%rsvars%") do if exist "%%~dpFdcc32.exe" goto done

rem Compare version numbers across registry views and default installation directories.
set "rsvars="
set "bestMajor=-1"
set "bestMinor=-1"
for %%H in (HKCU HKLM) do for %%V in (32 64) do for %%P in (Embarcadero CodeGear) do (
  for /f "tokens=*" %%K in ('reg query "%%H\Software\%%P\BDS" /reg:%%V 2^>nul') do (
    for /f "tokens=1,2,*" %%A in ('reg query "%%K" /v RootDir /reg:%%V 2^>nul') do (
      if /i "%%A"=="RootDir" call :consider "%%~nxK" "%%C"
    )
  )
)
for %%P in ("%ProgramFiles(x86)%" "%ProgramW6432%" "%ProgramFiles%") do if not "%%~P"=="" (
  for /d %%D in ("%%~P\Embarcadero\Studio\*") do call :consider "%%~nxD" "%%~fD"
)

:done
endlocal & set "PV_RSVARS=%rsvars%"
if defined PV_RSVARS exit /b 0
exit /b 1

:consider
if not exist "%~2\bin\rsvars.bat" exit /b
if not exist "%~2\bin\dcc32.exe" exit /b
for /f "tokens=1,2,* delims=." %%A in ("%~1") do (
  if "%%B"=="" exit /b
  if not "%%C"=="" exit /b
  for /f "delims=0123456789" %%N in ("%%A%%B") do exit /b
  if %%A lss %bestMajor% exit /b
  if %%A equ %bestMajor% if %%B leq %bestMinor% exit /b
  set "bestMajor=%%A"
  set "bestMinor=%%B"
  set "rsvars=%~2\bin\rsvars.bat"
)
exit /b
