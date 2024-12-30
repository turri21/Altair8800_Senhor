; SIO (not 2SIO) echo test with receive interrupts
;
; Program in octal for front panel entry into the Altair:
; 0: 061 000 001 076 001 323 000 373
; 10: 000 000 000 303 010 000
; 70: 365 333 001 323 001 361 373 311

        .org    0               ; Start at address 0
        lxi     sp,0100h        ; Init stack pointer
        mvi     a,01h           ; Receive ints on
        out     00h
        ei                      ; Enable 8080 interrupts

; The loop below represents "normal" processing a program may
; be doing. We also verify that the accumulator does not
; get changed by the interrupt routine.
LOOP:   nop
        nop
        nop
        jmp     LOOP

; Interrupt service routine for RST7 is at 038h. This routine
; saves the accumulator and PSW, then echoes the character.
; If this interrupt is shared, then the commented statements
; to check for a character would be included and used
; to branch to a a 2nd device to check.
        .org    038h            ; RST7 entry address
        push    a               ; Save A and status flags
        ;in     00h            ; Verify a new character present
        ;rrc                   ; Lsb has new data flag
        ;jc     nextDev        ; No character, try next device
        in      01h            ; Read the character
        out     01h            ; Echo it
        pop     a              ; Restore A and status flags
        ei                     ; Re-enable 8080 interrupts
        ret

        .end