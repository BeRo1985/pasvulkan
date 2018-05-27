@echo off 
SET PATH=c:\FPC\3.1.1\bin\i386-win32\
SET nodosfilewarning=1
ppcrossx64 -Sd -B -g -gl -O- -O1 "-FUFPCOutput\x86_64-win64" "-FEFPCOutput\x86_64-win64" -dRELEASE -dPasVulkanPasMP -dPasVulkanUseSDL2 -dPasVulkanUseSDL2WithVulkanSupport "-oexamples_x86_64_win64_release_fpc_dynamically_linked.exe" examples.dpr
copy /y "FPCOutput\\x86_64-win64\\examples_x86_64_win64_release_fpc_dynamically_linked.exe" examples_x86_64_win64_release_fpc_dynamically_linked.exe

