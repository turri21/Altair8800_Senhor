; Program to fill 4K of ROM with zeros
; For TASM assembler, 8080 instruction set
; 4K = 4096 bytes = 0x1000 bytes

        .org    0000h           ; Start at address 0x0000

        .fill   1000h,00h       ; Fill 0x1000 (4096) bytes with 0x00

        .end                    ; End of program