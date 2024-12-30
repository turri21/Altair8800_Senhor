; ----------------------------------------------------------------------------
; Micro-Soft Altair BASIC 3.2 (4K) - TASM Version
;	
; Copyright 1975, Bill Gates, Paul Allen, Monte Davidoff
; Source: http://altairbasic.org/ compiled by Reuben Harris
; Additional cleanup, relocation by Charles Mangin, March, 2019
; Conversion to use TASM by Fred VanEijk, December, 2024
; ----------------------------------------------------------------------------

; Forward declarations for labels

        .org    0000h               ; Start at address 0

Start:  di                      ; Disable interrupts
        jmp     Init            ; Jump to initialization

        .word   0490h           ; Initial constants
        .word   07F9h

SyntaxCheck:
        mov     a,m             ; A=Byte of BASIC program
        xthl                    ; HL=return address
        cmp     m               ; Compare to byte expected
        inx     h               ; Return address++
        xthl
        jnz     SyntaxError     ; Error if not what was expected
NextChar:
        inx     h
        mov     a,m
        cpi     3Ah
        rnc
        jmp     NextChar_tail

OutChar:
        push    psw
        lda     TERMINAL_X
        jmp     OutChar_tail
        nop

CompareHLDE:
        mov     a,h
        sub     d
        rnz
        mov     a,l
        sub     e
        ret

TERMINAL_Y:     .byte   01
TERMINAL_X:     .byte   00

FTestSign:
        lda     FACCUM+3
        ora     a
        jnz     FTestSign_tail
        ret

PushNextWord:
        xthl
        shld    ModifiedJump+1    ; Store in address field of jump instruction
        pop     h
        mov     c,m
        inx     h
        mov     b,m
        inx     h
        push    b
ModifiedJump:
        jmp     ModifiedJump      ; Self-modifying code - target address is patched

KW_INLINE_FNS:
        .word   Sgn
        .word   Int
        .word   Abs
        .word   FunctionCallError
        .word   Sqr
        .word   Rnd
        .word   Sin

KW_ARITH_OP_FNS:
        .byte   79h
        .word   FAdd            ; +
        .byte   79h
        .word   FSub            ; -
        .byte   7Ch
        .word   FMul            ; *
        .byte   7Ch
        .word   FDiv            ; /

; Keywords table - each entry is the ASCII of the keyword followed by its token value
KEYWORDS:
        .byte   045h,04Eh,0C4h          ; "END"   80
        .byte   046h,04Fh,0D2h          ; "FOR"   81
        .byte   04Eh,045h,058h,0D4h     ; "NEXT"  82
        .byte   044h,041h,054h,0C1h     ; "DATA"  83
        .byte   049h,04Eh,050h,055h,0D4h ; "INPUT" 84
        .byte   044h,049h,0CDh          ; "DIM"   85
        .byte   052h,045h,041h,0C4h     ; "READ"  86
        .byte   04Ch,045h,0D4h          ; "LET"   87
        .byte   047h,04Fh,054h,0CFh     ; "GOTO"  88
        .byte   052h,055h,0CEh          ; "RUN"   89
        .byte   049h,0C6h               ; "IF"    8A
        .byte   052h,045h,053h,054h,04Fh,052h,0C5h ; "RESTORE" 8B
        .byte   047h,04Fh,053h,055h,0C2h ; "GOSUB" 8C
        .byte   052h,045h,054h,055h,052h,0CEh ; "RETURN" 8D
        .byte   052h,045h,0CDh          ; "REM"   8E
        .byte   053h,054h,04Fh,0D0h     ; "STOP"  8F
        .byte   050h,052h,049h,04Eh,0D4h ; "PRINT" 90
        .byte   04Ch,049h,053h,0D4h     ; "LIST"  91
        .byte   043h,04Ch,045h,041h,0D2h ; "CLEAR" 92
        .byte   04Eh,045h,0D7h          ; "NEW"   93

        .byte   054h,041h,042h,0A8h     ; "TAB("  94
        .byte   054h,0CFh               ; "TO"    95
        .byte   054h,048h,045h,0CEh     ; "THEN"  96
        .byte   053h,054h,045h,0D0h     ; "STEP"  97
        .byte   0ABh                    ; "+"     98
        .byte   0ADh                    ; "-"     99
        .byte   0AAh                    ; "*"     9A
        .byte   0AFh                    ; "/"     9B
        .byte   0BEh                    ; ">"     9C
        .byte   0BDh                    ; "="     9D
        .byte   0BCh                    ; "<"     9E
        .byte   053h,047h,0CEh          ; "SGN"   9F
        .byte   049h,04Eh,054h          ; "INT"   A0
        .byte   041h,042h,0D3h          ; "ABS"   A1
        .byte   055h,053h,0D2h          ; "USR"   A2
        .byte   053h,051h,0D2h          ; "SQR"   A3
        .byte   052h,04Eh,0C4h          ; "RND"   A4
        .byte   053h,049h,0CEh          ; "SIN"   A5
        .byte   00h                     ; End of keywords

; Function pointers for general keywords
KW_GENERAL_FNS:
        .word   Stop            ; END
        .word   For             ; FOR
        .word   Next            ; NEXT
        .word   FindNextStatement ; DATA
        .word   Input           ; INPUT
        .word   Dim             ; DIM
        .word   Read            ; READ
        .word   Let             ; LET
        .word   Goto            ; GOTO
        .word   Run             ; RUN
        .word   If              ; IF
        .word   Restore         ; RESTORE
        .word   Gosub           ; GOSUB
        .word   Return          ; RETURN
        .word   Rem             ; REM
        .word   Stop            ; STOP
        .word   Print           ; PRINT
        .word   List            ; LIST
        .word   Clear           ; CLEAR
        .word   New             ; NEW

; Error message codes
ERROR_CODES:
        .byte   04Eh,0C6h       ; "NF" NEXT without FOR
        .byte   053h,0CEh       ; "SN" Syntax Error
        .byte   052h,0C7h       ; "RG" RETURN without GOSUB
        .byte   04Fh,0C4h       ; "OD" Out of Data
        .byte   046h,0C3h       ; "FC" Illegal Function Call
        .byte   04Fh,0D6h       ; "OV" Overflow
        .byte   04Fh,0CDh       ; "OM" Out of memory
        .byte   055h,0D3h       ; "US" Undefined Subroutine
        .byte   042h,0D3h       ; "BS" Bad Subscript
        .byte   044h,0C4h       ; "DD" Duplicate Definition
        .byte   02Fh,0B0h       ; "\0" Division by zero
        .byte   049h,0C4h       ; "ID" Invalid in Direct mode

        .byte   ','             ; Comma separator

; Line input buffer - 72 characters
LINE_BUFFER:
        .word   0000h,0000h,0000h,0000h
        .word   0000h,0000h,0000h,0000h
        .word   0000h,0000h,0000h,0000h
        .word   0000h,0000h,0000h,0000h
        .word   0000h,0000h,0000h,0000h
        .word   0000h,0000h,0000h,0000h
        .word   0000h,0000h,0000h,0000h
        .word   0000h,0000h,0000h,0000h
        .word   0000h,0000h,0000h,0000h

; Program state variables
DIM_OR_EVAL:    .byte   00h
INPUT_OR_READ:  .byte   00h
PROG_PTR_TEMP:  .word   0000h
L015F:          .word   0000h
CURRENT_LINE:   .word   0000h
STACK_TOP:      .word   0F1Ah    ; *** RELOCATE ***
PROGRAM_BASE:   .word   0000h
VAR_BASE:       .word   0000h
VAR_ARRAY_BASE: .word   0000h
VAR_TOP:        .word   0000h
DATA_PROG_PTR:  .word   0000h
FACCUM:         .byte   00h,00h,00h,00h
FTEMP:          .byte   00h
FBUFFER:        .word   0000h,0000h,0000h
                .word   0000h,0000h,0000h
                .byte   00h

; Error messages and prompts
szError:        .byte   020h,045h,052h,052h,04Fh,0D2h,00h  ; " ERROR\0"
szIn:           .byte   020h,049h,04Eh,0A0h,00h            ; " IN \0"
szOK:           .byte   00Dh,04Fh,0CBh,00Dh,00h            ; "\rOK\r\0"

GetFlowPtr:
        lxi     h,0004h        ; HL=SP+4 (ie get word
        dad     sp             ; just past return addr)
        mov     a,m
        inx     h
        cpi     81h           ; 'FOR'?
        rnz                   ; Return if not 'FOR'
        rst     6             ; RST PushNextWord
        xthl                  ; POP HL (ie HL=(HL))
        rst     4             ; RST CompareHLDE
        lxi     b,000Dh
        pop     h             ; Restore HL
        rz                    ; Return if var ptrs match
        dad     b             ; HL+=000D
        jmp     GetFlowPtr+4  ; Loop

CopyMemoryUp:
        call    CheckEnoughMem
        push    b              ; Exchange BC with HL
        xthl
        pop     b

CopyMemLoop:
        rst     4             ; HL==DE?
        mov     a,m
        stax    b
        rz                    ; Exit if DE reached
        dcx     b
        dcx     h
        jmp     CopyMemLoop

CheckEnoughVarSpace:
        push    h
        lhld    VAR_TOP
        mvi     b,00h         ; BC=C*4
        dad     b
        dad     b
        call    CheckEnoughMem
        pop     h
        ret

CheckEnoughMem:
        push    d
        xchg
        lxi     h,0FFDEh      ; HL=-34 (extra 2 bytes for return address)
        dad     sp
        rst     4
        xchg
        pop     d
        rnc

OutOfMemory:
        mvi     e,0Ch
        .byte   01h           ; LXI B,....

SyntaxError:
        mvi     e,02h
        .byte   01h           ; LXI B,....

DivideByZero:
        mvi     e,14h

Error:
        call    ResetStack
        call    NewLine
        lxi     h,ERROR_CODES
        mov     d,a
        mvi     a,'?'         ; Print '?'
        rst     3             ; RST OutChar
        dad     d             ; HL points to error code
        mov     a,m
        rst     3             ; RST OutChar
        rst     2             ; RST NextChar
        rst     3             ; RST OutChar
        lxi     h,szError     ; Print " ERROR"
        call    PrintString
        lhld    CURRENT_LINE
        mov     a,h
        ana     l
        inr     a
        cnz     PrintIN
        .byte   01h           ; LXI B,....

Stop:
        rnz                   ; Syntax Error if args
        pop     b             ; Lose return address

Main:
        lxi     h,szOK
        call    Init

GetNonBlankLine:
        lxi     h,0FFFFh
        shld    CURRENT_LINE
        call    InputLine
        rst     2             ; RST NextChar
        inr     a
        dcr     a
        jz      GetNonBlankLine
        push    psw
        call    LineNumberFromStr
        push    d
        call    Tokenize
        mov     b,a
        pop     d
        pop     psw
        jnc     Exec

StoreProgramLine:
        push    d             ; Push line number
        push    b             ; Push line length
        rst     2             ; RST NextChar
        ora     a             ; Zero set if line is empty
        push    psw           ; Preserve line-empty flag
        call    FindProgramLine ; Get nearest program line address in BC
        push    b             ; Push line address
        jnc     InsertProgramLine ; If line doesn't exist, jump ahead

RemoveProgramLine:
        xchg                  ; DE=Next line address
        lhld    VAR_BASE

RemoveLine:
        ldax    d             ; Move byte of program remainder down
        stax    b             ; in memory
        inx     b
        inx     d
        rst     4             ; Loop until DE==VAR_BASE, ie whole
        jnz     RemoveLine    ; program remainder done
        mov     h,b
        mov     l,c           ; Update VAR_BASE from BC
        shld    VAR_BASE

InsertProgramLine:
        pop     d             ; DE=Line address
        pop     psw           ; Restore line-empty flag
        jz      UpdateLinkedList ; If line is empty, jump ahead
        lhld    VAR_BASE
        xthl                  ; HL = Line length
        pop     b             ; BC = VAR_BASE
        dad     b             ; HL = VAR_BASE + line length
        push    h
        call    CopyMemoryUp  ; Move remainder of program
        pop     h
        shld    VAR_BASE      ; Update VAR_BASE
        xchg                  ; HL=Line address, DE=VAR_BASE
        mov     m,h           ; Write next line pointer
        inx     h
        inx     h
        pop     d             ; DE = line number
        mov     m,e           ; Write line number
        inx     h
        mov     m,d
        inx     h

CopyFromBuffer:
        lxi     d,LINE_BUFFER ; Copy the line into the program
        ldax    d
        mov     m,a
        inx     h
        inx     d
        ora     a
        jnz     CopyFromBuffer+3

UpdateLinkedList:
        call    ResetAll
        inx     h
        xchg

UpdateProgramLinks:           ; Was L0265
        mov     h,d
        mov     l,e
        mov     a,m           ; If pointer to next line is null
        inx     h
        ora     m             ; word then we've reached end of
        jz      GetNonBlankLine ; program, get next line
        inx     h             ; Skip over line number
        inx     h
        inx     h
        xra     a

FindLineEnd:                 ; Was L0271 - searching for end of current line
        cmp     m            ; Look for null terminator
        inx     h
        jnz     FindLineEnd  ; Keep searching until end of line found
        xchg                 ; Switch to DE to update pointer
        mov     m,e          ; Store pointer to next line
        inx     h
        mov     m,d
        jmp     UpdateProgramLinks  ; Continue with next line

FindProgramLine:
        lhld    PROGRAM_BASE
        mov     b,h           ; BC=this line
        mov     c,l
        mov     a,m           ; If we've found two consecutive
        inx     h             ; null bytes, then we've reached end
        ora     m             ; of the program and return
        dcx     h
        rz
        push    b             ; Push this line address
        rst     6             ; Push (next line address)
        rst     6             ; Push (this line number)
        pop     h             ; HL = this line number
        rst     4             ; Compare line numbers
        pop     h             ; HL = next line address
        pop     b             ; BC = this line address
        cmc
        rz                    ; Return carry set if line numbers match
        cmc
        rnc                   ; Return if line number greater than required
        jmp     FindProgramLine+3        

New:
        rnz
        lhld    PROGRAM_BASE
        xra     a
        mov     m,a
        inx     h
        mov     m,a
        inx     h
        shld    VAR_BASE

Run:
        rnz
ResetAll:
        lhld    PROGRAM_BASE
        dcx     h
Clear:
        shld    PROG_PTR_TEMP
        call    Restore
        lhld    VAR_BASE
        shld    VAR_ARRAY_BASE
        shld    VAR_TOP

ResetStack:
        pop     b
        lhld    STACK_TOP
        sphl
        xra     a
        mov     l,a
        push    h
        push    b
        lhld    PROG_PTR_TEMP
        ret

InputLineWith:
        mvi     a,'?'         ; Print '?'
        rst     3             ; RST OutChar
        mvi     a,' '         ; Print ' '
        rst     3             ; RST OutChar
        call    InputLine
        inx     h

Tokenize:
        mvi     c,05          ; Initialize line length to 5
        lxi     d,LINE_BUFFER ; Output ptr same as input ptr at start
        mov     a,m
        cpi     ' '
        jz      WriteChar
        mov     b,a
        cpi     '"'
        jz      FreeCopy
        ora     a
        jz      Exit
        push    d             ; Preserve output ptr
        mvi     b,00          ; Initialize Keyword ID to 0
        lxi     d,KEYWORDS-1
        push    h             ; Preserve input ptr
        .byte   3Eh           ; LXI over get-next-char

KwCompare:
        rst     2             ; RST NextChar
        inx     d
        ldax    d             ; Get keyword char to compare with
        ani     7Fh           ; Ignore bit 7 of keyword char
        jz      NotAKeyword   ; If keyword char==0, end of keywords reached
        cmp     m             ; Keyword char matches input char?
        jnz     NextKeyword   ; If not, jump to get next keyword
        ldax    d
        ora     a
        jp      KwCompare
        pop     psw           ; Remove input ptr from stack
        mov     a,b           ; A=Keyword ID
        ori     80h           ; Set bit 7 (indicates a keyword)
        .byte   0F2h          ; JP .... LXI trick again

NotAKeyword:
        pop     h             ; Restore input ptr
        mov     a,m           ; and get input char
        pop     d             ; Restore output ptr

WriteChar:
        inx     h             ; Advance input ptr
        stax    d             ; Store output char
        inx     d             ; Advance output ptr
        inr     c             ; C++ (Line length counter)
        sui     8Eh           ; If it's not the
        jnz     Tokenize+5
        mov     b,a           ; B=0

FreeCopyLoop:
        mov     a,m           ; A=Input char
        ora     a             ; If char is null then exit
        jz      Exit
        cmp     b             ; If input char is term char then
        jz      WriteChar     ; we're done free copying

FreeCopy:
        inx     h
        stax    d
        inr     c
        inx     d
        jmp     FreeCopyLoop

NextKeyword:
        pop     h             ; Restore input ptr
        push    h
        inr     b             ; Keyword ID ++
        xchg                  ; HL=keyword table ptr

NextKwLoop:
        ora     m             ; Loop until
        inx     h             ; bit 7 of previous
        jp      NextKwLoop    ; keyword char is set
        xchg                  ; DE=keyword ptr, HL=input ptr
        jmp     KwCompare+2

Exit:
        lxi     h,LINE_BUFFER-1
        stax    d
        inx     d
        stax    d
        inx     d
        stax    d
        ret

Backspace:
        dcr     b             ; Char count--
        dcx     h             ; Input ptr--
        rst     3             ; RST OutChar
        jnz     InputNext

ResetInput:
        rst     3             ; RST OutChar
        call    NewLine

InputLine:
        lxi     h,LINE_BUFFER
        mvi     b,01

InputNext:
        call    InputChar
        cpi     0Dh           ; Carriage return?
        jz      TerminateInput
        cpi     ' '           ; If < ' '
        jc      InputNext     ; or
        cpi     7Dh           ; > '}'
        jnc     InputNext     ; then loop back
        cpi     '@'
        jz      ResetInput
        cpi     '_'
        jz      Backspace
        mov     c,a
        mov     a,b
        cpi     48h           ; Line buffer full?
        mvi     a,07h         ; Bell character
        jnc     OutputBellAndContinue
        mov     a,c           ; Write char to LINE_BUFFER
        mov     m,c
        inx     h
        inr     b

OutputBellAndContinue:       ; Was L036A - Output bell character when buffer full
        rst     3             ; RST OutChar
        jmp     InputNext     ; Continue getting input

OutChar_tail:
        cpi     48h
        cz      NewLine
        inr     a
        sta     TERMINAL_X

WaitTermReady:
        in      00h
        ani     80h
        jnz     WaitTermReady
        pop     psw
        out     01h
        ret

InputChar:
        in      00h
        ani     01h
        jnz     InputChar
        in      01h
        ani     7Fh
        ret

List:
        call    LineNumberFromStr
        rnz
        pop     b             ; Get return address
        call    FindProgramLine
        push    b

ListNextLine:
        pop     h
        rst     6
        pop     b
        mov     a,b
        ora     c
        jz      Main
        call    TestBreakKey
        push    b
        call    NewLine
        rst     6
        xthl
        call    PrintInt
        mvi     a,' '
        pop     h

ListChar:
        rst     3             ; RST OutChar
        mov     a,m
        ora     a
        inx     h
        jz      ListNextLine
        jp      ListChar
        sui     7Fh           ; A is now keyword index + 1
        mov     c,a
        push    h
        lxi     d,KEYWORDS
        push    d

ToNextKeyword:
        ldax    d
        inx     d
        ora     a
        jp      ToNextKeyword
        dcr     c
        pop     h
        jnz     ToNextKeyword-1

PrintKeyword:
        mov     a,m
        ora     a
        jm      ListChar-1
        rst     3             ; RST OutChar
        inx     h
        jmp     PrintKeyword

For:
        call    Let
        xthl
        call    GetFlowPtr
        pop     d
        jnz     ForLoopInit
        dad     b
        sphl

ForLoopInit:
        xchg
        mvi     c,08
        call    CheckEnoughVarSpace
        push    h
        call    FindNextStatement
        xthl
        push    h
        lhld    CURRENT_LINE
        xthl
        rst     1             ; SyntaxCheck
        .byte   95h           ; KWID_TO
        call    EvalExpression
        push    h
        call    FCopyToBCDE
        pop     h
        push    b
        push    d
        lxi     b,8100h
        mov     d,c
        mov     e,d
        mov     a,m
        cpi     97h           ; KWID_STEP
        mvi     a,01h
        jnz     PushStepValue
        call    EvalExpression+1
        push    h
        call    FCopyToBCDE
        rst     5             ; FTestSign
        pop     h

PushStepValue:
        push    b
        push    d
        push    psw
        inx     sp
        push    h
        lhld    PROG_PTR_TEMP
        xthl

EndOfForHandler:
        mvi     b,81h
        push    b
        inx     sp

ExecNext:
        call    TestBreakKey
        mov     a,m
        cpi     ':'
        jz      Exec
        ora     a
        jnz     SyntaxError
        inx     h
        mov     a,m
        inx     h
        ora     m
        inx     h
        jz      Main
        mov     e,m
        inx     h
        mov     d,m
        xchg
        shld    CURRENT_LINE
        xchg

Exec:
        rst     2             ; RST NextChar
        lxi     d,ExecNext
        push    d
        rz
        sui     80h
        jc      Let
        cpi     14h
        jnc     SyntaxError
        rlc                   ; BC = A*2
        mov     c,a
        mvi     b,00h
        xchg
        lxi     h,KW_GENERAL_FNS
        dad     b
        mov     c,m
        inx     h
        mov     b,m
        push    b
        xchg
        rst     2             ; RST NextChar
        ret

NextChar_tail:
        cpi     ' '
        jz      NextChar
        cpi     '0'
        cmc
        inr     a
        dcr     a
        ret

Restore:
        xchg
        lhld    PROGRAM_BASE
        dcx     h

UpdateDataPtr:
        shld    DATA_PROG_PTR
        xchg
        ret

TestBreakKey:
        in      00h           ; Exit if no key pressed
        ani     01h
        rnz
        call    InputChar
        cpi     03h           ; Break key?
        jmp     Stop

CharIsAlpha:
        mov     a,m
        cpi     'A'
        rc
        cpi     'Z'+1
        cmc
        ret

GetSubscript:
        rst     2             ; RST NextChar
        call    EvalExpression
        rst     5             ; FTestSign
        jm      FunctionCallError
        lda     FACCUM+3
        cpi     90h
        jc      FAsInteger

FunctionCallError:
        mvi     e,08h
        jmp     Error

LineNumberFromStr:
        dcx     h
        lxi     d,0000h

NextLineNumChar:
        rst     2             ; RST NextChar
        rnc
        push    h
        push    psw           ; Preserve flags
        lxi     h,1998h       ; Decimal 6552
        rst     4
        jc      SyntaxError
        mov     h,d
        mov     l,e
        dad     d
        dad     h
        dad     d
        dad     h
        pop     psw
        sui     '0'
        mov     e,a
        mvi     d,00h
        dad     d
        xchg
        pop     h
        jmp     NextLineNumChar

Gosub:
        mvi     c,03h
        call    CheckEnoughVarSpace
        pop     b
        push    h
        push    h
        lhld    CURRENT_LINE
        xthl
        mvi     d,8Ch
        push    d
        inx     sp
        push    b

Goto:
        call    LineNumberFromStr
        rnz
        call    FindProgramLine
        mov     h,b
        mov     l,c
        dcx     h
        rc
        mvi     e,0Eh
        jmp     Error

Return:
        rnz
        mvi     d,0FFh
        call    GetFlowPtr
        sphl
        cpi     8Ch
        mvi     e,04h
        jnz     Error
        pop     h
        shld    CURRENT_LINE
        lxi     h,ExecNext
        xthl

FindNextStatement:
        .byte   013Ah         ; LXI B,..3A
Rem:
        .byte   10h
        nop

FindNextStatementLoop:
        mov     a,m
        ora     a
        rz
        cmp     c
        rz
        inx     h
        jmp     FindNextStatementLoop

Let:
        call    GetVar
        rst     1             ; SyntaxCheck
        .byte   9Dh

AssignVar:
        push    d
        call    EvalExpression
        xthl
        shld    PROG_PTR_TEMP
        push    h
        call    FCopyToMem
        pop     d
        pop     h
        ret

If:
        call    EvalExpression
        mov     a,m
        call    FPush
        mvi     d,00h

GetCompareOpLoop:
        sui     9Ch           ; KWID_>
        jc      GotCompareOp
        cpi     03h
        jnc     GotCompareOp
        cpi     01h
        ral
        ora     d
        mov     d,a
        rst     2             ; RST NextChar
        jmp     GetCompareOpLoop

GotCompareOp:
        mov     a,d
        ora     a
        jz      SyntaxError
        push    psw
        call    EvalExpression
        rst     1             ; SyntaxCheck
        .byte   96h           ; KWID_THEN
        dcx     h
        pop     psw
        pop     b
        pop     d
        push    h
        push    psw
        call    FCompare
        inr     a
        ral
        pop     b
        ana     b
        pop     h
        jz      Rem
        rst     2             ; RST NextChar
        jc      Goto
        jmp     Exec+5

Print:
        jz      NewLine        ; If zero flag, go to NewLine
        rz                     ; Return if zero
        cpi     22h           ; Compare with quote character (changed from '"')
        cz      PrintString-1  ; If match and zero, go to PrintString-1
        jz      Print-2        ; If zero flag, go to Print-2
        cpi     94h           ; Compare with TAB token
        jz      Tab           ; If match, go to Tab
        push    h             ; Save HL
        cpi     2Ch           ; Compare with comma (changed from ',')
        jz      ToNextTabBreak ; If match, go to ToNextTabBreak
        cpi     3Bh           ; Compare with semicolon (changed from ';')
        jz      ExitTab       ; If match, go to ExitTab
        pop     b             ; Restore to BC
        call    EvalExpression ; Evaluate expression
        push    h             ; Save HL
        call    FOut          ; Output floating point number
        call    PrintString   ; Print the string
        mvi     a,20h         ; Space character
        rst     3             ; Output character
        pop     h             ; Restore HL
        jmp     Print-2       ; Jump back

TerminateInput:
        mvi     m,00h
        lxi     h,LINE_BUFFER-1

NewLine:
        mvi     a,0Dh
        sta     TERMINAL_X
        rst     3             ; RST OutChar
        mvi     a,0Ah
        rst     3             ; RST OutChar
        lda     TERMINAL_Y

PrintNullLoop:
        dcr     a
        sta     TERMINAL_X
        rz
        push    psw
        xra     a
        rst     3             ; RST OutChar
        pop     psw
        jmp     PrintNullLoop

PrintString:
        mov     a,m
        ora     a
        rz
        inx     h
        cpi     '"'
        rz
        rst     3             ; RST OutChar
        cpi     0Dh
        cz      NewLine
        jmp     PrintString

ToNextTabBreak:
        lda     TERMINAL_X
        cpi     38h
        cnc     NewLine
        jnc     ExitTab

CalcSpaceCount:
        sui     0Eh
        jnc     CalcSpaceCount
        cma
        jmp     PrintSpaces

Tab:
        call    GetSubscript
        rst     1             ; SyntaxCheck
        .byte   29h           ; ')'
        dcx     h
        push    h
        lda     TERMINAL_X
        cma
        add     e
        jnc     ExitTab

PrintSpaces:
        inr     a
        mov     b,a
        mvi     a,' '

PrintSpaceLoop:
        rst     3             ; RST OutChar
        dcr     b
        jnz     PrintSpaceLoop

ExitTab:
        pop     h
        rst     2             ; RST NextChar
        jmp     Print+3

Input:
        push    h
        lhld    CURRENT_LINE
        mvi     e,16h
        inx     h
        mov     a,l
        ora     h
        jz      Error
        call    InputLineWith
        jmp     L05FA+1

Read:
        push    h
        lhld    DATA_PROG_PTR

L05FA:
        ori     0AFh
        ;xra    a
        sta     INPUT_OR_READ
        xthl
        .byte   01h           ; LXI B,....

ReadNext:
        rst     1             ; SyntaxCheck
        .byte   2Ch           ; ','
        call    GetVar
        xthl
        push    d
        mov     a,m
        cpi     ','
        jz      GotDataItem
        ora     a
        jnz     SyntaxError
        lda     INPUT_OR_READ
        ora     a
        inx     h
        jnz     NextDataLine+1
        mvi     a,'?'
        rst     3             ; RST OutChar
        call    InputLineWith

GotDataItem:
        pop     d
        inx     h
        call    AssignVar
        xthl
        dcx     h
        rst     2             ; RST NextChar
        jnz     ReadNext
        pop     d
        lda     INPUT_OR_READ
        ora     a
        rz
        xchg
        jnz     UpdateDataPtr

NextDataLine:
        pop     h
        rst     6
        mov     a,c
        ora     b
        mvi     e,06h
        jz      Error
        inx     h
        rst     2             ; RST NextChar
        cpi     83h           ; KWID_DATA
        jnz     NextDataLine
        pop     b
        jmp     GotDataItem

Next:
        call    GetVar
        shld    PROG_PTR_TEMP
        call    GetFlowPtr
        sphl
        push    d
        mov     a,m
        inx     h
        push    psw
        push    d
        mvi     e,00h
        jnz     Error
        call    FLoadFromMem
        xthl
        push    h
        call    FAddMem
        pop     h
        call    FCopyToMem
        pop     h
        call    FLoadBCDEfromMem
        push    h
        call    FCompare
        pop     h
        pop     b
        sub     b
        call    FLoadBCDEfromMem
        jz      ForLoopIsComplete
        xchg
        shld    CURRENT_LINE
        mov     l,c
        mov     h,b
        jmp     EndOfForHandler

ForLoopIsComplete:
        sphl
        lhld    PROG_PTR_TEMP
        jmp     ExecNext

EvalExpression:
        dcx     h
        mvi     d,00h
        push    d
        mvi     c,01h
        call    CheckEnoughVarSpace
        call    EvalTerm
        shld    L015F

ArithParse:
        lhld    L015F
        pop     b
        mov     a,m
        mvi     d,00h
        sui     98h           ; KWID_PLUS
        rc
        cpi     04h
        rnc
        mov     e,a
        rlc
        add     e
        mov     e,a
        lxi     h,KW_ARITH_OP_FNS
        dad     d
        mov     a,b
        mov     d,m
        cmp     d
        rnc
        inx     h
        push    b
        lxi     b,ArithParse
        push    b
        mov     c,d           ; ???
        call    FPush
        mov     d,c
        rst     6
        lhld    L015F
        jmp     EvalExpression+3

EvalTerm:
        rst     2             ; RST NextChar
        jc      FIn
        call    CharIsAlpha
        jnc     EvalVarTerm
        cpi     98h           ; KWID_PLUS
        jz      EvalTerm
        cpi     '.'
        jz      FIn
        cpi     99h           ; KWID_MINUS
        jz      EvalMinusTerm
        sui     9Fh
        jnc     EvalInlineFn

EvalBracketed:
        rst     1             ; SyntaxCheck
        .byte   28h           ; '('
        call    EvalExpression
        rst     1             ; SyntaxCheck
        .byte   29h           ; ')'
        ret

EvalMinusTerm:
        call    EvalTerm
        push    h
        call    FNegate
        pop     h
        ret

EvalVarTerm:
        call    GetVar
        push    h
        xchg
        call    FLoadFromMem
        pop     h
        ret

EvalInlineFn:
        mvi     b,00h
        rlc
        mov     c,a
        push    b
        rst     2             ; RST NextChar
        call    EvalBracketed
        xthl
        lxi     d,06F1h
        push    d
        lxi     b,KW_INLINE_FNS
        dad     b
        rst     6
        ret

DimContd:
        dcx     h
        rst     2             ; RST NextChar
        rz
        rst     1             ; SyntaxCheck
        .byte   2Ch           ; ','

Dim:
        lxi     b,DimContd
        push    b
        .byte   0F6h

GetVar:
        xra     a
        sta     DIM_OR_EVAL
        mov     b,m
        call    CharIsAlpha
        jc      SyntaxError
        xra     a
        mov     c,a
        rst     2             ; RST NextChar
        jnc     072Eh
        mov     c,a
        rst     2             ; RST NextChar
        sui     '('
        jz      GetArrayVar
        push    h
        lhld    VAR_ARRAY_BASE
        xchg
        lhld    VAR_BASE

FindVarLoop:
        rst     4
        jz      AllocNewVar
        mov     a,c
        sub     m
        inx     h
        jnz     VarSearchContinue
        mov     a,b
        sub     m

VarSearchContinue:
        inx     h
        jz      VarFound
        inx     h
        inx     h
        inx     h
        inx     h
        jmp     FindVarLoop

AllocNewVar:
        pop     h             ; HL=prog ptr
        xthl                  ; (SP)=prog ptr, HL=ret.addr.
        push    d
        lxi     d,06F6h       ; Address inside EvalTerm
        rst     4
        pop     d
        jz      AlreadyAllocd
        xthl                  ; (SP)=ret.addr, HL=prog ptr
        push    h             ; Prog ptr back on stack
        push    b             ; Preserve var name on stack
        lxi     b,0006h
        lhld    VAR_TOP
        push    h
        dad     b
        pop     b
        push    h
        call    CopyMemoryUp
        pop     h
        shld    VAR_TOP
        mov     h,b
        mov     l,c
        shld    VAR_ARRAY_BASE

InitVarLoop:
        dcx     h
        mvi     m,00h
        rst     4
        jnz     InitVarLoop
        pop     d
        mov     m,e
        inx     h
        mov     m,d
        inx     h

VarFound:
        xchg
        pop     h
        ret

AlreadyAllocd:
        sta     FACCUM+3      ; A was set to zero at 075A
        pop     h
        ret

GetArrayVar:
        push    b
        lda     DIM_OR_EVAL
        push    psw
        call    GetSubscript
        rst     1             ; SyntaxCheck
        .byte   29h           ; ')'
        pop     psw
        sta     DIM_OR_EVAL
        xthl
        xchg
        dad     h
        dad     h
        push    h
        lhld    VAR_ARRAY_BASE
        .byte   01h           ; LXI B,...

FindArray:
        pop     b
        dad     b
        xchg
        push    h
        lhld    VAR_TOP
        rst     4
        xchg
        pop     d
        jz      AllocArray
        rst     6
        xthl
        rst     4
        pop     h
        rst     6
        jnz     FindArray
        lda     DIM_OR_EVAL
        ora     a
        mvi     e,12h
        jnz     Error

ArrayIndexCalc:
        pop     d
        dcx     d
        xthl
        rst     4
        mvi     e,10h
        jnc     Error
        pop     d
        dad     d
        pop     d
        xchg
        ret

AllocArray:
        mov     m,e
        inx     h
        mov     m,d
        inx     h
        lxi     d,002Ch
        lda     DIM_OR_EVAL
        ora     a
        jz      ArrayInit
        pop     d
        push    d
        inx     d
        inx     d
        inx     d
        inx     d

ArrayInit:
        push    d
        mov     m,e
        inx     h
        mov     m,d
        inx     h
        push    h
        dad     d
        call    CheckEnoughMem
        shld    VAR_TOP
        pop     d

InitElements:
        dcx     h
        mvi     m,00h
        rst     4
        jnz     InitElements
        jmp     ArrayIndexCalc

FWordToFloat:
        mov     d,b
        mvi     e,00h
        mvi     b,90h         ; Exponent=2^16
        jmp     FCharToFloat+5

FAddOneHalf:
        lxi     h,ONE_HALF    ; Load BCDE with (float) 0.5

FAddMem:
        call    FLoadBCDEfromMem
        jmp     FAdd+2

FSub:
        pop     b             ; Get lhs in BCDE
        pop     d
        call    FNegate       ; Negate rhs and slimily
        .byte   21h           ; LXI H,.... LXI into FAdd + 2

FAdd:
        pop     b             ; Get lhs in BCDE
        pop     d
        mov     a,b           ; If lhs==0 then we don't need
        ora     a             ; to do anything and can just
        rz                    ; exit
        lda     FACCUM+3      ; If rhs==0 then exit via a copy
        ora     a             ; of lhs to FACCUM
        jz      FLoadFromBCDE

        sub     b             ; A=rhs.exponent-lhs.exponent
        jnc     L082C         ; If rhs' exponent >= lhs'exponent, jump ahead
        cma                   ; Two's complement the exponent
        inr     a             ; difference, so it's correct
        xchg
        call    FPush         ; Push old rhs
        xchg
        call    FLoadFromBCDE ; rhs = old lhs
        pop     b             ; lhs = old rhs
        pop     d

L082C:
        push    psw           ; Preserve exponent diff
        call    FUnpackMantissas
        mov     h,a           ; H=sign relationship
        pop     psw           ; A=exponent diff
        call    FMantissaRtMult ; Shift lhs mantissa right by (exponent diff) places
        ora     h             ; A=0 after last call, so this tests
        lxi     h,FACCUM      ; the sign relationship
        jp      FSubMantissas ; Jump ahead if we need to subtract
        call    FAddMantissas
        jnc     FRoundUp      ; Jump ahead if that didn't overflow
        inx     h             ; Flip the sign in FTEMP_SIGN
        inr     m
        jz      Overflow      ; Error out if exponent overflowed
        call    FMantissaRtOnce ; Shift mantissa one place right
        jmp     FRoundUp      ; Jump ahead

FSubMantissas:
        xra     a             ; B=0-B
        sub     b
        mov     b,a
        mov     a,m           ; E=(FACCUM)-E
        sbb     e
        mov     e,a
        inx     h
        mov     a,m           ; D=(FACCUM+1)-D
        sbb     d
        mov     d,a
        inx     h
        mov     a,m           ; C=(FACCUM+2)-C
        sbb     c
        mov     c,a

FNormalise:
        cc      FNegateInt
        mvi     h,00h
        mov     a,c           ; Test most-significant bit of mantissa
        ora     a             ; and jump ahead if it's 1
        jm      FRoundUp

NormLoop:
        cpi     0E0h          ; If we've shifted 32 times,
        jz      FZero         ; then the number is 0
        dcr     h
        mov     a,b           ; Left-shift extra mantissa byte
        add     a
        mov     b,a
        call    FMantissaLeft ; Left-shift mantissa
        mov     a,h
        jp      NormLoop
        lxi     h,FACCUM+3
        add     m
        mov     m,a           ; Since A was a -ve number, that certainly should
        jnc     FZero         ; have carried, hence the extra check for zero
        rz                    ; ?why?

FRoundUp:
        mov     a,b           ; A=extra mantissa byte
        lxi     h,FACCUM+3
        ora     a             ; If bit 7 of the extra mantissa byte
        cm      FMantissaInc  ; is set, then round up the mantissa
        mov     b,m           ; B=exponent
        inx     h
        mov     a,m           ; A=FTEMP_SIGN
        ani     80h
        xra     c             ; Bit 7 of C is always 1
        mov     c,a
        jmp     FLoadFromBCDE ; Exit via copying BCDE to FACCUM

FMantissaLeft:
        mov     a,e
        ral
        mov     e,a
        mov     a,d
        ral
        mov     d,a
        mov     a,c
        adc     a
        mov     c,a
        ret

FMantissaInc:
        inr     e
        rnz
        inr     d
        rnz
        inr     c
        rnz
        mvi     c,80h         ; Mantissa overflowed to zero, so set it
        inr     m             ; to 1 and increment the exponent
        rnz                   ; And if the exponent overflows...

Overflow:
        mvi     e,0Ah
        jmp     Error

FAddMantissas:
        mov     a,m
        add     e
        mov     e,a
        inx     h
        mov     a,m
        adc     d
        mov     d,a
        inx     h
        mov     a,m
        adc     c
        mov     c,a
        ret

FNegateInt:
        lxi     h,FTEMP
        mov     a,m
        cma
        mov     m,a
        xra     a
        mov     l,a
        sub     b
        mov     b,a
        mov     a,l
        sbb     e
        mov     e,a
        mov     a,l
        sbb     d
        mov     d,a
        mov     a,l
        sbb     c
        mov     c,a
        ret

FMantissaRtMult:
        mvi     b,00h         ; Initialize extra mantissa byte
        inr     a
        mov     l,a

RtMultLoop:
        xra     a
        dcr     l
        rz
        call    FMantissaRtOnce
        jmp     RtMultLoop

FMantissaRtOnce:
        mov     a,c
        rar
        mov     c,a
        mov     a,d
        rar
        mov     d,a
        mov     a,e
        rar
        mov     e,a
        mov     a,b           ; NB: B is the extra
        rar                   ; mantissa byte
        mov     b,a
        ret

FMul:
        pop     b             ; Get lhs in BCDE
        pop     d
        rst     5             ; FTestSign
        rz                    ; If rhs==0 then exit
        mvi     l,00h         ; L=0 to signify exponent add
        call    FExponentAdd
        mov     a,c
        sta     FMulInnerLoop+13
        xchg
        shld    FMulInnerLoop+8
        lxi     b,0000h
        mov     d,b
        mov     e,b
        lxi     h,FNormalise+3
        push    h
        lxi     h,FMulOuterLoop
        push    h
        push    h
        lxi     h,FACCUM

FMulOuterLoop:
        mov     a,m           ; A=FACCUM mantissa byte
        inx     h
        push    h             ; Preserve FACCUM ptr
        mvi     l,08h         ; 8 bits to do

FMulInnerLoop:
        rar                   ; Test lowest bit of mantissa byte
        mov     h,a           ; Preserve mantissa byte
        mov     a,c           ; A=result mantissa's high byte
        jnc     L0919         ; If that bit of multiplicand was 0, skip over adding mantissas
        push    h
        lxi     h,0000h
        dad     d
        pop     d
        aci     00h           ; A=result mantissa high byte, gets back to C

L0919:
        call    FMantissaRtOnce+1
        dcr     l
        mov     a,h           ; Restore mantissa byte and
        jnz     FMulInnerLoop ; jump back if L is not yet 0

PopHLandReturn:
        pop     h             ; Restore FACCUM ptr
        ret                   ; Return to FMulOuterLoop, or if finished that then exit to FNormalise

FDivByTen:
        call    FPush
        lxi     b,8420h       ; BCDE=(float)10
        lxi     d,0000h
        call    FLoadFromBCDE

FDiv:
        pop     b
        pop     d
        rst     5             ; FTestSign
        jz      DivideByZero
        mvi     l,0FFh
        call    FExponentAdd
        inr     m
        inr     m
        dcx     h
        mov     a,m
        sta     L095F+1
        dcx     h
        mov     a,m
        sta     L095F-3
        dcx     h
        mov     a,m
        sta     L095F-7
        mov     b,c
        xchg
        xra     a
        mov     c,a
        mov     d,a
        mov     e,a
        sta     L095F+4

FDivLoop:
        push    h
        push    b
        mov     a,l
        sui     00h
        mov     l,a
        mov     a,h
        sbi     00h
        mov     h,a
        mov     a,b

L095F:
        sbi     00h
        mov     b,a
        mvi     a,00h
        sbi     00h
        cmc
        jnc     L0971
        sta     L095F+4h
        pop     psw
        pop     psw
        stc
        .byte   0D2h          ; JNC ....

L0971:
        pop     b
        pop     h
        mov     a,c
        inr     a
        dcr     a
        rar
        jm      FRoundUp+1
        ral
        call    FMantissaLeft
        dad     h
        mov     a,b
        ral
        mov     b,a
        lda     L095F+4h
        ral
        sta     L095F+4h
        mov     a,c
        ora     d
        ora     e
        jnz     FDivLoop
        push    h
        lxi     h,FACCUM+3
        dcr     m
        pop     h
        jnz     FDivLoop
        jmp     Overflow

FExponentAdd:
        mov     a,b
        ora     a
        jz      FExponentAdd+31
        mov     a,l           ; A=0 for add, FF for subtract
        lxi     h,FACCUM+3
        xra     m             ; XOR with FAccum's exponent
        add     b             ; Add exponents
        mov     b,a
        rar                   ; Carry (after the add) into bit 7
        xra     b             ; XOR with old bit 7
        mov     a,b
        jp      FExponentAdd+30 ; If
        adi     80h
        mov     m,a
        jz      PopHLandReturn
        call    FUnpackMantissas
        mov     m,a
        dcx     h
        ret
        ora     a
        pop     h             ; Ignore return address so we'll end
        jm      Overflow

FZero:
        xra     a
        sta     FACCUM+3
        ret

FMulByTen:
        call    FCopyToBCDE
        mov     a,b
        ora     a
        rz
        adi     02h
        jc      Overflow
        mov     b,a
        call    FAdd+2
        lxi     h,FACCUM+3
        inr     m
        rnz
        jmp     Overflow

FTestSign_tail:
        lda     FACCUM+2
        .byte   0FEh
InvSignToInt:
        cma
SignToInt:
        ral
        sbb     a
        rnz
        inr     a
        ret

Sgn:
        rst     5             ; FTestSign
FCharToFloat:
        mvi     b,88h         ; ie 2^8
        lxi     d,0000h
        lxi     h,FACCUM+3
        mov     c,a
        mov     m,b
        mvi     b,00h
        inx     h
        mvi     m,80h
        ral
        jmp     FNormalise

Abs:
        rst     5             ; FTestSign
        rp
FNegate:
        lxi     h,FACCUM+2
        mov     a,m
        xri     80h
        mov     m,a
        ret

FPush:
        xchg
        lhld    FACCUM
        xthl
        push    h
        lhld    FACCUM+2
        xthl
        push    h
        xchg
        ret

FLoadFromMem:
        call    FLoadBCDEfromMem
FLoadFromBCDE:
        xchg
        shld    FACCUM
        mov     h,b
        mov     l,c
        shld    FACCUM+2
        xchg
        ret

; Trigonometric functions and constants

Sqr:
        rst     5             ; FTestSign
        jm      FunctionCallError
        rz
        lxi     h,FACCUM+3
        mov     a,m
        rar
        push    psw
        push    h
        mvi     a,40h
        ral
        mov     m,a
        lxi     h,FBUFFER
        call    FCopyToMem
        mvi     a,04h

SqrLoop:
        push    psw
        call    FPush
        lxi     h,FBUFFER
        call    FLoadBCDEfromMem
        call    FDiv+2
        pop     b
        pop     d
        call    FAdd+2
        lxi     b,8000h
        mov     d,c
        mov     e,c
        call    FMul+2
        pop     psw
        dcr     a
        jnz     SqrLoop
        pop     h
        pop     psw
        adi     0C0h
        add     m
        mov     m,a
        ret

Sin:
        call    FPush         ; Push x
        lxi     b,8349h       ; BCDE=2π
        lxi     d,0FDBh
        call    FLoadFromBCDE ; rhs = 2π
        pop     b             ; rhs = x
        pop     d
        call    FDiv+2        ; =x/2π
        call    FPush
        call    Int           ; rhs = INT(u)
        pop     b             ; rhs = u
        pop     d
        call    FSub+2        ; =u-INT(u)
        lxi     b,7F00h       ; BCDE=0.25
        mov     d,c
        mov     e,c
        call    FSub+2
        rst     5             ; FTestSign
        stc                   ; Set carry (ie no later negate)
        jp      NegateIfPositive

        call    FAddOneHalf
        rst     5
        ora     a             ; Resets carry (ie later negate)

NegateIfPositive:
        push    psw
        cp      FNegate
        lxi     b,7F00h       ; BCDE=0.25
        mov     d,c
        mov     e,c
        call    FAdd+2
        pop     psw
        cnc     FNegate
        call    FPush
        call    FCopyToBCDE
        call    FMul+2        ; = x*x
        call    FPush         ; Push x*x
        lxi     h,TAYLOR_SERIES
        call    FLoadFromMem
        pop     b
        pop     d
        mvi     a,04h

TaylorLoop:
        push    psw           ; Push #terms remaining
        push    d             ; Push BCDE
        push    b
        push    h
        call    FMul+2
        pop     h
        call    FLoadBCDEfromMem
        push    h
        call    FAdd+2
        pop     h
        pop     b
        pop     d
        pop     psw           ; Pop #terms remaining into A
        dcr     a             ; Decrement #terms and loop back if not
        jnz     TaylorLoop    ; done all 4 of them
        jmp     FMul

Rnd:
        rst     5             ; FTestSign
        jm      L0C7C
        lxi     h,RND_SEED
        call    FLoadFromMem
        rz
        lxi     b,9835h
        lxi     d,447Ah
        call    FMul+2
        lxi     b,6828h
        lxi     d,0B146h
        call    FAdd+2

L0C7C:
        call    FCopyToBCDE
        mov     a,e
        mov     e,c
        mov     c,a
        mvi     m,80h
        dcx     h
        mov     b,m
        mvi     m,80h
        call    FNormalise+3
        lxi     h,RND_SEED
        jmp     FCopyToMem

; Constants and lookup tables
TAYLOR_SERIES:
        .byte   0BAh,0D7h,1Eh,86h    ; DD 39.710670
        .byte   64h,26h,99h,87h      ; DD -76.574982
        .byte   58h,34h,23h,87h      ; DD 81.602234
        .byte   0E0h,5Dh,0A5h,86h    ; DD -41.341675
        .byte   0DAh,0Fh,49h,83h     ; DD 6.283185

RND_SEED:
        .byte   52h,0C7h,4Fh,80h     ; Initial random seed

ONE_HALF:
        .byte   00h,00h,00h,80h      ; DD 0.5

DECIMAL_POWERS:                       ; Powers of 10 table
        .byte   0A0h,86h,01h         ; DT 100000
        .byte   10h,27h,00h          ; DT 10000
        .byte   0E8h,03h,00h         ; DT 1000
        .byte   64h,00h,00h          ; DT 100
        .byte   0Ah,00h,00h          ; DT 10
        .byte   01h,00h,00h          ; DT 1

; Program initialization code
Init:
        lxi     h,0F1Ah              ; *** STACK_TOP RELOCATE
        sphl
        shld    STACK_TOP
        in      01h
        mvi     c,0FFh
        lxi     d,ConfigIOcode
        push    d
        lda     0FFFh                ; *** RELOCATE
        mov     b,a
        in      0FFh
        rar
        jc      L0D42-1
        ani     0Ch
        jz      L0D42
        mvi     b,10h
        mov     a,b

L0D42:
        sta     L0D8D-1
        in      0FFh
        ral
        ral
        mvi     b,20h

L0D4B:
        lxi     d,0CA02h
        rc
        ral
        mov     b,e
        dcr     e
        rc
        ral
        jc      L0D6F
        mov     b,e
        lxi     d,0C280h
        ral
        rnc
        ral
        mvi     a,03h
        call    L0D8B
        dcr     a
        adc     a
        add     a
        add     a
        inr     a
        call    L0D8B
        stc
        jmp     L0D4B

L0D6F:
        xra     a
        call    L0D8B
        call    L0D87
        call    L0D87
        mov     c,e
        cma
        call    L0D87
        mvi     a,04h
        dcr     m
        call    L0D8B
        dcr     m
        dcr     m
        dcr     m

L0D87:
        lxi     h,L0D8D-1
        inr     m

L0D8B:
        out     00h

L0D8D:
        ret

ConfigIOcode:
        mov     h,d
        mov     l,b
        shld    InputChar+3
        mov     a,h
        ani     0C8h
        mov     h,a
        shld    TestBreakKey+3
        xchg
        shld    WaitTermReady+3
        lda     L0D8D-1
        sta     InputChar+1
        sta     TestBreakKey+1
        inr     a
        sta     InputChar+8
        add     c
        sta     WaitTermReady+1
        inr     a
        sta     InputChar-2
        lxi     h,0FFFFh
        shld    CURRENT_LINE
        call    NewLine
        lxi     h,szMemorySize
        call    PrintString
        call    InputLineWith
        rst     2             ; RST NextChar
        ora     a
        jnz     L0DDE
        lxi     h,UnusedMemory

FindMemTopLoop:
        inx     h
        mvi     a,37h
        mov     m,a
        cmp     m
        jnz     DoneMemSize
        dcr     a
        mov     m,a
        cmp     m
        jz      FindMemTopLoop
        jmp     DoneMemSize

L0DDE:
        lxi     h,LINE_BUFFER
        call    LineNumberFromStr
        ora     a
        jnz     SyntaxError
        xchg
        dcx     h

DoneMemSize:
        dcx     h
        push    h

GetTerminalWidth:
        lxi     h,szTerminalWidth
        call    PrintString
        call    InputLineWith
        rst     2             ; RST NextChar
        ora     a
        jz      DoOptionalFns
        lxi     h,LINE_BUFFER
        call    LineNumberFromStr
        mov     a,d
        ora     a
        jnz     GetTerminalWidth
        mov     a,e
        cpi     10h
        jc      GetTerminalWidth
        sta     OutChar_tail+1

CalcTabBrkSize:
        sui     0Eh
        jnc     CalcTabBrkSize
        adi     1Ch
        cma
        inr     a
        add     e
        sta     ToNextTabBreak+4

DoOptionalFns:
        lxi     h,OPT_FN_DESCS
OptionalFnsLoop:
        rst     6
        lxi     d,szWantSin
        rst     4
        jz      L0E32
        rst     6
        xthl
        call    PrintString
        call    InputLineWith
        rst     2             ; RST NextChar
        pop     h
        cpi     'Y'

L0E32:
        pop     d
        jz      InitProgramBase
        cpi     'N'
        jnz     DoOptionalFns
        rst     6
        xthl
        lxi     d,FunctionCallError
        mov     m,e
        inx     h
        mov     m,d
        pop     h
        jmp     OptionalFnsLoop

InitProgramBase:
        xchg
        mvi     m,00h
        inx     h
        shld    PROGRAM_BASE
        xthl
        lxi     d,0F1Ah              ; *** RELOCATE STACK_TOP
        rst     4
        jc      OutOfMemory
        pop     d
        sphl
        shld    STACK_TOP
        xchg
        call    CheckEnoughMem
        mov     a,e
        sub     l
        mov     l,a
        mov     a,d
        sbb     h
        mov     h,a
        lxi     b,0FFF0h
        dad     b
        call    NewLine
        call    PrintInt
        lxi     h,szVersionInfo
        call    PrintString
        lxi     h,PrintString
        shld    Main+4
        call    New+1
        lxi     h,Main
        shld    Start+2
        pchl

; Optional function descriptors
OPT_FN_DESCS:
        .word   L0D17
        .word   szWantSin
        .word   KW_INLINE_FNS+12
        .word   Sin
        .word   szWantRnd
        .word   KW_INLINE_FNS+10
        .word   Rnd
        .word   szWantSqr
        .word   KW_INLINE_FNS+8
        .word   Sqr

; String constants for optional functions
szWantSin:
        .byte   57h,41h,4Eh,54h,20h,53h,49h,4Eh,00h     ; "WANT SIN\0"
szWantRnd:
        .byte   57h,41h,4Eh,54h,20h,52h,4Eh,44h,00h     ; "WANT RND\0"
szWantSqr:
        .byte   57h,41h,4Eh,54h,20h,53h,51h,52h,00h     ; "WANT SQR\0"
; Terminal configuration strings
szTerminalWidth:
        .byte   54h,45h,52h,4Dh,49h,4Eh,41h,4Ch,20h     ; "TERMINAL "
        .byte   57h,49h,44h,54h,48h,00h                  ; "WIDTH\0"

; Version and system information
szVersionInfo:
        .byte   20h,42h,59h,54h,45h,53h,20h,46h,52h,45h,0C5h,0Dh,0Dh    ; " BYTES FREE\r\r"
        .byte   42h,41h,53h,49h,43h,20h,56h,45h,52h,53h,49h,4Fh,4Eh,20h  ; "BASIC VERSION "
        .byte   33h,2Eh                                                    ; "3."
        .byte   0B2h,0Dh                                                   ; "2\r"
        .byte   5Bh,34h,4Bh,20h,56h,45h,52h,53h,49h,4Fh,4Eh,0DDh,0Dh,00h ; "[4K VERSION]\r\0"

; Memory size prompt
szMemorySize:
        .byte   4Dh,45h,4Dh,4Fh,52h,59h,20h,53h,49h,5Ah,0C5h,00h         ; "MEMORY SIZE\0"

; Unused memory space - end of program
UnusedMemory:
        .byte   00h

        .end