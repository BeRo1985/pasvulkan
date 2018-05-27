@echo off 
SET PATH=c:\FPC\3.1.1\bin\i386-win32\
SET nodosfilewarning=1
"c:\FPC\3.1.1\bin\i386-win32\ppcrossarm.exe" -Tandroid -Sd -B -CpARMv7A -CfVFPv3 -OpARMv7a -OoFastMath -O1 -XX -Xs -CX -Cg "-olibmain.so" "-FUFPCOutput\arm-android" "-FEFPCOutput\arm-android" "-Fl.\..\libs\libpngandroid\obj\local\armeabi-v7a" "-Fo.\..\libs\libpngandroid\obj\local\armeabi-v7a" "-Fl.\..\libs\sdl20androidarm32" "-Fo.\..\libs\sdl20androidarm32" -dPasVulkanPasMP -dPasVulkanUseSDL2 examples.dpr
copy /y "FPCOutput\\arm-android\\libmain.so" "android\\app\\src\\main\\jniLibs\\armeabi-v7a\\libmain.so"
