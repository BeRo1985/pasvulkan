@echo off 
SET PATH=c:\FPC\3.1.1\bin\i386-win32\
SET nodosfilewarning=1
"c:\FPC\3.1.1\bin\i386-win32\ppcrossx64" -Sd -B -O- -O1 -g -gl -Sd "-FUFPCOutput\x86_32-win32" "-FEFPCOutput\x86_32-win32" "-Fl..\libs\sdl20win64" "-Fo..\libs\sdl20win64" -dSTATICLINK -dRELEASE -dPasVulkanPasMP -dPasVulkanUseSDL2 -dc_int64 -k--allow-multiple-definition "-oexamples_x86_64_win64_release_fpc_statically_linked.exe" examples.dpr
copy /y "FPCOutput\\x86_32-win32\\examples_x86_64_win64_release_fpc_statically_linked.exe" examples_x86_64_win64_release_fpc_statically_linked.exe
