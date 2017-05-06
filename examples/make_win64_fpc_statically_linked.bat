@echo off 
del .\..\src\*.a
del .\..\src\*.ppu
del .\..\src\*.o
SET PATH=c:\FPC\3.1.1\bin\i386-win32\
SET nodosfilewarning=1
del examples_x86_64_win64_release_fpc_dynamically_linked.exe
del *.a
del *.ppu
del *.o
copy ..\libs\sdl20win64\*.a *.a
"c:\FPC\3.1.1\bin\i386-win32\ppcrossx64" -Sd -B -O3 -g -gl -dSDL20 -dSTATICLINK -dUseCThreads -dRELEASE -k--allow-multiple-definition "-oexamples_x86_64_win64_release_fpc_statically_linked.exe" examples.dpr
del *.a
del *.ppu
del *.o
del .\..\src\*.a
del .\..\src\*.ppu
del .\..\src\*.o

