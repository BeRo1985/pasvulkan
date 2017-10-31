@echo off 
SET PATH=c:\FPC\3.1.1\bin\i386-win32\
SET nodosfilewarning=1
del *.a
del *.ppu
del *.o
del .\..\src\*.a
del .\..\src\*.ppu
del .\..\src\*.o
ppcrossx64 -Sd -B -g -gl -dRELEASE -dPasVulkanPasMP -dPasVulkanUseSDL2 -dPasVulkanUseSDL2WithVulkanSupport -O- -O1 "-oexamples_x86_64_win64_release_fpc_dynamically_linked.exe" examples.dpr
del *.a
del *.ppu
del *.o
del .\..\src\*.a
del .\..\src\*.ppu
del .\..\src\*.o
