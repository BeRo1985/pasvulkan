@echo off 
SET PATH=c:\FPC\3.1.1\bin\i386-win32\
SET nodosfilewarning=1
del *.a
del *.ppu
del *.o
del .\..\src\*.a
del .\..\src\*.ppu
del .\..\src\*.o
del lib\i386-android\*.a
del lib\i386-android\*.ppu
del lib\i386-android\*.o
del libmain.so 
del libSDL2.so
del "android\\app\\src\\main\\jniLibs\\x86\\libmain.so"
del libmain.so
copy .\..\libs\sdl20androidi386\libSDL2.so libSDL2.so
copy .\..\libs\libpngandroid\obj\local\x86\libpng.a libpng.a
"c:\FPC\3.1.1\bin\i386-win32\ppcross386.exe" -Tandroid -Sd -B -b -g -gl -gw3 -O- -O1 -XX -Xm -CX -Cg "-olibmain.so" "-FUlib\i386-android" -dDEBUG -dPasVulkanPasMP -dPasVulkanUseSDL2 -dCompileForWithPIC examples.dpr
copy libmain.so "android\\app\\src\\main\\jniLibs\\x86\\libmain.so"
del libpng.a
rem del *.a
del *.ppu
rem del *.o
del .\..\src\*.a
del .\..\src\*.ppu
rem del .\..\src\*.o
rem del libSDL2.so