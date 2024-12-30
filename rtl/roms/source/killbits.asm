; Kill the Bit game by Dean McDaniel, May 15, 1975
; Adapted for TASM 8080
;
; Object: Kill the rotating bit. If you miss the lit bit, another
; bit turns on leaving two bits to destroy. Quickly
; toggle the switch, don't leave the switch in the up
; position. Before starting, make sure all the switches
; are in the down position.
;
; Program in octal for front panel entry into the Altair:
; 000: 041 000 000 026 200 001 016 000
; 010: 032 032 032 032 011 322 010 000
; 020: 333 377 252 017 127 303 010 000

        .org    0               ; Start at address 0

START:  lxi     h,0             ; Initialize counter
        mvi     d,80h           ; Set up initial display bit
        lxi     b,0eh           ; Higher value = faster

BEG:    ldax    d               ; Display bit pattern on
        ldax    d               ; ...upper 8 address lights
        ldax    d               
        ldax    d
        dad     b               ; Increment display counter
        jnc     BEG             ; If no carry, continue display loop
        
        in      0ffh            ; Input data from sense switches
        xra     d               ; Exclusive or with A
        rrc                     ; Rotate display right one bit
        mov     d,a             ; Move data to display reg
        jmp     BEG             ; Repeat sequence

        .end