@echo off 
SET PATH=c:\FPC\3.1.1\bin\i386-win32\
SET nodosfilewarning=1
del examples_i386_win32_release_fpc_statically_linked.exe
del *.a
del *.ppu
del *.o
del .\..\src\*.a
del .\..\src\*.ppu
del .\..\src\*.o
copy ..\libs\sdl20win32\*.a *.a
"c:\FPC\3.1.1\bin\i386-win32\fpc" -Sd -B -O3 -g -gl -CfSSE -Sd -dSTATICLINK -dRELEASE -dPasVulkanPasMP -dPasVulkanUseSDL2 -dc_int64 "-oexamples_i386_win32_release_fpc_statically_linked.exe" examples.dpr
del *.a
del *.ppu
del *.o
del .\..\src\*.a
del .\..\src\*.ppu
del .\..\src\*.o
