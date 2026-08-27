@echo off
mkdir fpctemp
rem The tool reads its own output back for the self check, so it needs the
rem unpacking side even though only --compress ever produces a packed table.
fpc -Sd -B -O2 -dPasVulkanSymbolTableCompression -FUfpctemp -Fu../../../src -Fu../../../externals/pucu/src -Fu../../../externals/pasmp/src -Fi../../../src -omapsymbols.exe mapsymbols.dpr
rmdir /s /q fpctemp
