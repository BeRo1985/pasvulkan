@echo off 
SET PATH=c:\FPC\3.1.1\bin\i386-win32\
SET nodosfilewarning=1
del *.a
del *.ppu
del *.o
del .\..\src\*.a
del .\..\src\*.ppu
del .\..\src\*.o
del lib\arm-android\*.a
del lib\arm-android\*.ppu
del lib\arm-android\*.o
del libmain.so 
del libSDL2.so
del "android\\app\\src\\main\\jniLibs\\armeabi-v7a\\libmain.so"
del libmain.so
copy .\..\libs\sdl20androidarm32\libSDL2.so libSDL2.so
copy .\..\libs\libpngandroid\obj\local\armeabi-v7a\libpng.a libpng.a
"c:\FPC\3.1.1\bin\i386-win32\ppcrossarm.exe" -Tandroid -Sd -B -b -g -gl -gw3 -CpARMv7A -CfVFPv3 -OpARMv7a -O- -O1 -XX -Xm -CX -Cg "-olibmain.so" "-FUlib\arm-android" -dDEBUG -dSamsungDebug -dPasVulkanPasMP -dPasVulkanUseSDL2 examples.dpr
copy libmain.so "android\\app\\src\\main\\jniLibs\\armeabi-v7a\\libmain.so"
del libpng.a
rem del *.a
del *.ppu
rem del *.o
del .\..\src\*.a
del .\..\src\*.ppu
rem del .\..\src\*.o
rem del libSDL2.so