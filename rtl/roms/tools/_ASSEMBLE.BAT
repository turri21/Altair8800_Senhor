tasm -85 -g3 ..\source\empty.asm ..\games\empty.rom
tasm -85 -g3 ..\source\zeroToSeven.asm ..\games\zeroToSeven.rom
tasm -85 -g3 ..\source\killbits.asm ..\games\killbits.rom
tasm -85 -g3 ..\source\StatusLights.asm ..\games\StatusLights.rom
tasm -85 -g3 ..\source\serialporttest.asm ..\games\serialporttest.rom
tasm -85 -g3 ..\source\serialporttest00.asm ..\games\serialporttest00.rom


tasm -85 -g3 ..\source\SIOEcho.asm ..\games\SIOEcho.rom
tasm -85 -g3 ..\source\SIOEchoInt.asm ..\games\SIOEchoInt.rom

zasm --asm8080 --bin -i ..\source\basic4k32.asm -o ..\games\basic4k32.rom

pause
