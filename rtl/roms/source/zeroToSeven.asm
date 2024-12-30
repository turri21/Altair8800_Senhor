; Program to generate bytes 0-7 starting at 0x0000
; For TASM assembler, 8080 instruction set

        .org    0000h           ; Start at address 0x0000

        .byte   00h             ; Byte 0
        .byte   01h             ; Byte 1
        .byte   02h             ; Byte 2
        .byte   03h             ; Byte 3
        .byte   04h             ; Byte 4
        .byte   05h             ; Byte 5
        .byte   06h             ; Byte 6
        .byte   07h             ; Byte 7

        .end                    ; End of program