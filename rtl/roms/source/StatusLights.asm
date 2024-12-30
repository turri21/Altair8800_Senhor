; Demonstrate status light combinations
;
; you should single step this code
;
; Program in octal for front panel entry into the Altair:
; 0: 072 040 000 062 041 000 061 040
; 10: 000 365 361 333 020 323 020 373
; 20: 363 166

        .org    0               ; Start at address 0

        lda     40Q             ; Opcode fetch, memory read x 3
        sta     41Q             ; Opcode fetch, mem read x 2, mem write
        lxi     sp,40Q          ; Opcode fetch, mem read x 2
        push    a               ; Opcode fetch, stack write x 2
        pop     a               ; Opcode fetch, stack read x 2
        in      20Q             ; Opcode fetch, mem read, I/O input
        out     20Q             ; Opcode fetch, mem read, I/O output
        ei                      ; Interrupts enabled
        di                      ; Interrupts disabled
        hlt                     ; Halt

        .end