@echo off 
FOR /f "tokens=*" %%G IN ('dir /b FPCOutput') DO (
  set TARGET=%%G
  call del FPCOutput\%%TARGET%%\*.a
  call del FPCOutput\%%TARGET%%\*.ppu
  call del FPCOutput\%%TARGET%%\*.o
  call del FPCOutput\%%TARGET%%\*.so
  call del FPCOutput\%%TARGET%%\*.map
)
