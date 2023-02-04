        .include "apple2.inc"

;;; Monitor Equates
A1L     =       $3c     ;general purpose
A1H     =       $3d     ;general purpose
A2L     =       $3e     ;general purpose
A2H     =       $3f     ;general purpose
A4L     =       $42     ;general purpose
A4H     =       $43     ;general purpose
MOVE    =       $FE2C

;;; ProDOS Equates
MLI             := $BF00
BITMAP          := $BF58
BITMAP_SIZE     =  24


;;; Filing Calls
OPEN            = $C8
NEWLINE         = $C9
READ            = $CA
WRITE           = $CB
CLOSE           = $CC
FLUSH           = $CD
SET_MARK        = $CE
GET_MARK        = $CF
SET_EOF         = $D0
GET_EOF         = $D1
SET_BUF         = $D2
GET_BUF         = $D3

.macro SET16 addr, const
        lda     #<(const)
        sta     addr
        lda     #>(const)
        sta     addr+1
.endmacro

;;; ============================================================
;;; ProDOS Interpreter Protocol
;;; ProDOS 8 Technical Reference Manual
;;; 5.1.5.1 - Starting System Programs
        .org $2000
        jmp     start
        .byte   $EE, $EE        ; Interpreter signature
        .byte   $41             ; path buffer length
path:   .res    $41,0           ; path buffer
start:

;;; ============================================================

;;; --------------------------------------------------
;;; Configure system bitmap
        ldx     #BITMAP_SIZE-1
        lda     #0
:       sta     BITMAP,x
        dex
        bpl     :-

        lda     #%11001111      ; ZP, Stack, Text Page 1
        sta     BITMAP

        lda     #%11111111
        sta     BITMAP+$A*2     ; Pages $A0-$A7
        sta     BITMAP+$A*2+1   ; Pages $A8-$AF
        sta     BITMAP+$B*2     ; Pages $B0-$B7
        sta     BITMAP+$B*2+1   ; Pages $B8-$BF (last is ProDOS GP)

;;; --------------------------------------------------
;;; Relocate INTBASIC up to target

        SET16   A1L, intbasic
        SET16   A2L, intbasic+sizeof_intbasic-1
        SET16   A4L, BASIC
        ldy     #0
        jsr     MOVE

;;; --------------------------------------------------
;;; Relocate rest of this to $300

        relo := $300
        SET16   A1L, proc
        SET16   A2L, proc+sizeof_proc-1
        SET16   A4L, relo
        ldy     #0
        jsr     MOVE
        jmp     relo

;;; --------------------------------------------------
;;; Load program (if given) and invoke Integer BASIC

        .proc proc

        lda     path
        bne     :+
        ;; No path - do cold start
        jmp     BASIC
:

        ;; Have path

        ;; TODO: Do as much of COLD/WARM as needed
        ;; TODO: Load program
        ;; TODO: Twiddle pointers
        ;; TODO: Set WARM to return here and QUIT
        ;; TODO: Invoke RUNWARM

        brk

        .endproc
        sizeof_proc = .sizeof(proc)

;;; ============================================================
;;; Integer BASIC Implementation
;;; ============================================================

        .proc intbasic
        .include "IntegerBASIC_cc65.s"
        .endproc
        sizeof_intbasic = .sizeof(intbasic)
        BASIC = intbasic::BASIC ; jsr COLD ; jmp WARM
        COLD = intbasic::COLD
        WARM = intbasic::WARM
        RUNWARM = intbasic::RUNWARM
