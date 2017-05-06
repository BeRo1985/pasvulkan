@echo off 
SET PATH=c:\FPC\3.1.1\bin\i386-win32\
SET nodosfilewarning=1
del *.a
del *.ppu
del *.o
del .\..\src\*.a
del .\..\src\*.ppu
del .\..\src\*.o
del .\libSDL2.so
copy .\..\libs\sdl20androidarm32\libSDL2.so libSDL2.so
"c:\FPC\3.1.1\bin\i386-win32\ppcrossarm.exe" -Tandroid -Sd -B -g -gl -dRELEASE -CpARMv7A -CfVFPv3 -O2 "-olibexamples.so" examples.dpr
del *.a
del *.ppu
del *.o
del .\..\src\*.a
del .\..\src\*.ppu
del .\..\src\*.o
del .\libSDL2.so
