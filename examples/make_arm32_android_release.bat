@echo off 
SET PATH=c:\FPC\3.1.1\bin\i386-win32\
SET nodosfilewarning=1
del *.a
del *.ppu
del *.o
del .\..\src\*.a
del .\..\src\*.ppu
del .\..\src\*.o
del libmain.so 
del libSDL2.so
del "android\\app\\src\\main\\jniLibs\\armeabi-v7a\\libmain.so"
del libmain.so
copy .\..\libs\sdl20androidarm32\libSDL2.so libSDL2.so
"c:\FPC\3.1.1\bin\i386-win32\ppcrossarm.exe" -Tandroid -Sd -B -CpARMv7A -CfVFPv3 -OpARMv7a -OoFastMath -O3 -XX -Xs -CX -Cg "-olibmain.so" -dPasVulkanPasMP -dPasVulkanUseSDL2 examples.dpr
copy libmain.so "android\\app\\src\\main\\jniLibs\\armeabi-v7a\\libmain.so"
del *.a
del *.ppu
del *.o
del .\..\src\*.a
del .\..\src\*.ppu
del .\..\src\*.o
del libSDL2.so