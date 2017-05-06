@echo off 
del *.a
del *.ppu
del *.o
del .\..\src\*.a
del .\..\src\*.ppu
del .\..\src\*.o
ppcrossx64 -Sd -B -g -gl -dRELEASE "-oexamples_x86_64_win64_release_fpc_dynamically_linked.exe" examples.dpr
del *.a
del *.ppu
del *.o
del .\..\src\*.a
del .\..\src\*.ppu
del .\..\src\*.o
