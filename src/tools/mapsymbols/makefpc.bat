@echo off
mkdir fpctemp
fpc -Sd -B -O2 -FUfpctemp -Fu../../../src -Fu../../../externals/pucu/src -Fu../../../externals/pasmp/src -Fi../../../src -omapsymbols.exe mapsymbols.dpr
rmdir /s /q fpctemp
