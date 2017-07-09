@echo off 
del *.a
del *.ppu
del *.o
del .\..\src\*.a
del .\..\src\*.ppu
del .\..\src\*.o
fpc -Sd -B -g -gl -dRELEASE -dPasVulkanPasMP "-oexamples_i386_win32_release_fpc_dynamically_linked.exe" examples.dpr
del *.a
del *.ppu
del *.o
del .\..\src\*.a
del .\..\src\*.ppu
del .\..\src\*.o
