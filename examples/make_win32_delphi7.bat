@echo off 
rem DCC call for Delphi 7, you must adjust it for newer Delphi versions 
"C:\Program Files (x86)\Borland\Delphi7\bin\dcc32.exe" -B examples.dpr
ren examples.exe examples_i386_win32_delphi_release_dynamically_linked.exe
