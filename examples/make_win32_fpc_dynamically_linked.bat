@echo off 
SET PATH=c:\FPC\3.1.1\bin\i386-win32\
SET nodosfilewarning=1
fpc -Sd -B -g -gl "-FUFPCOutput\x86_32-win32" "-FEFPCOutput\x86_32-win32" -dRELEASE -dPasVulkanPasMP -dPasVulkanUseSDL2 -dPasVulkanUseSDL2WithVulkanSupport "-oexamples_i386_win32_release_fpc_dynamically_linked.exe" examples.dpr
copy /y "FPCOutput\\x86_32-win32\\examples_i386_win32_release_fpc_dynamically_linked.exe" examples_i386_win32_release_fpc_dynamically_linked.exe
