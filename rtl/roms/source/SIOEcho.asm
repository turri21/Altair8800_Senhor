; Simple SIO (not 2SIO) echo test
;
; Program in octal for front panel entry into the Altair:
; 000: 333 000 017 332 000 000 333 001
; 010: 323 001 303 000 000

        .org    0               ; Start at address 0

LOOP:   in      00h             ; Wait for character
        rrc                     
        jc      LOOP            ; Nothing yet (negative logic)
        in      01h             ; Read the character
        out     01h             ; Echo it
        jmp     LOOP

        .end