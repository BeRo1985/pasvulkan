@echo off 
SET PATH=c:\FPC\3.1.1\bin\i386-win32\
SET nodosfilewarning=1
"c:\FPC\3.1.1\bin\i386-win32\ppcross386.exe" -Tandroid -Sd -B -b -g -gl -gw3 -O- -O1 -XX -Xm -CX -Cg "-olibmain.so" "-FUFPCOutput\i386-android" "-FEFPCOutput\i386-android" "-Fl.\..\libs\libpngandroid\obj\local\x86" "-Fo.\..\libs\libpngandroid\obj\local\x86" "-Fl.\..\libs\sdl20androidi386" "-Fo.\..\libs\sdl20androidi386" -dDEBUG -dPasVulkanPasMP -dPasVulkanUseSDL2 -dCompileForWithPIC examples.dpr
copy /y "FPCOutput\\i386-android\\libmain.so" "android\\app\\src\\main\\jniLibs\\x86\\libmain.so"
