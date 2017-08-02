@echo off 
SET PATH=c:\FPC\3.1.1\bin\i386-win32\
SET nodosfilewarning=1
del *.a
del *.ppu
del *.o
del .\..\src\*.a
del .\..\src\*.ppu
del .\..\src\*.o
fpc -Sd -B -g -gl -dRELEASE -dPasVulkanPasMP -dPasVulkanUseSDL2 "-oexamples_i386_win32_release_fpc_dynamically_linked.exe" examples.dpr
del *.a
del *.ppu
del *.o
del .\..\src\*.a
del .\..\src\*.ppu
del .\..\src\*.o
