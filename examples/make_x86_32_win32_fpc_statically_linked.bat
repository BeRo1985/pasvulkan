@echo off 
SET PATH=c:\FPC\3.1.1\bin\i386-win32\
SET nodosfilewarning=1
"c:\FPC\3.1.1\bin\i386-win32\fpc" -Sd -B -O3 -g -gl -CfSSE -Sd "-FUFPCOutput\x86_32-win32" "-FEFPCOutput\x86_32-win32" "-Fl..\libs\sdl20win32" "-Fo..\libs\sdl20win32" -dSTATICLINK -dRELEASE -dPasVulkanPasMP -dPasVulkanUseSDL2 -dc_int64 "-oexamples_x86_32_win32_release_fpc_statically_linked.exe" examples.dpr
copy /y "FPCOutput\\x86_32-win32\\examples_x86_32_win32_release_fpc_statically_linked.exe" examples_x86_32_win32_release_fpc_statically_linked.exe

