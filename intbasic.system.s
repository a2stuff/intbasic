        .include "apple2.inc"
        .include "opcodes.inc"

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

QUIT            = $65
GET_FILE_INFO   = $C4
OPEN            = $C8
READ            = $CA
CLOSE           = $CC
GET_EOF         = $D1

.macro SET16 addr, const
        lda     #<(const)
        sta     addr
        lda     #>(const)
        sta     addr+1
.endmacro

.macro MLI_CALL call, params
        jsr     MLI
        .byte   call
        .addr   params
.endmacro

;;; ============================================================
;;; System Program
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
;;; Relocate rest of this to $280

        relo := $300
        SET16   A1L, proc
        SET16   A2L, proc+sizeof_proc-1
        SET16   A4L, relo
        ldy     #0
        jsr     MOVE
        jmp     relo

;;; ============================================================
;;; Load program (if given) and invoke Integer BASIC
;;; ============================================================

        __saved_org__ .set *
        .proc proc
        .org ::relo

        ;; Make LOAD/SAVE just QUIT to ProDOS
        lda     #OPC_JMP_abs
        ldx     #<quit
        ldy     #>quit
        sta     LOAD+0
        stx     LOAD+1
        sty     LOAD+2
        sta     SAVE+0
        stx     SAVE+1
        sty     SAVE+2

        lda     path
        bne     have_path

        ;; ----------------------------------------
        ;; No path - do cold start
        jmp     BASIC

        ;; ----------------------------------------
        ;; Have path
have_path:
        ;; Check type, bail if not INT
        MLI_CALL GET_FILE_INFO, gfi_params
        bcs     quit
        lda     gfi_file_type
        cmp     #$FA            ; INT
        bne     quit

        ;; Open file
        MLI_CALL OPEN, open_params
        bcs     quit
        lda     open_ref_num
        sta     geteof_ref_num
        sta     read_ref_num
        sta     close_ref_num

        ;; Get size
        MLI_CALL GET_EOF, geteof_params
        bcs     close

        ;; TODO: Calculate load address

        MLI_CALL READ, read_params
        bcs     close

        ;; TODO: Do as much of COLD/WARM as needed
        ;;   * When is it safe to jsr COLD?
        ;; TODO: Load program
        ;; TODO: Twiddle pointers

        ;; When END is invoked, just QUIT
        SET16   END+1, quit     ; replaces: JMP WARM

        ;; Also capture ERRMESS, just QUIT
        SET16   ERRMESS+1, quit ; replaces: JSR PRINTERR

        ;; Run the program
        jmp     RUNWARM

close:
        php
        MLI_CALL CLOSE, close_params
        plp
        bcs     quit

        ;; TODO: Invoke BASIC!
        brk

quit:
        MLI_CALL QUIT, quit_params
        brk


gfi_params:
gfi_param_count:        .byte   $A   ; in
gfi_pathname:           .addr   path ; in
gfi_access:             .byte   0    ; out
gfi_file_type:          .byte   0    ; out
gfi_aux_type:           .word   0    ; out
gfi_storage_type:       .byte   0    ; out
gfi_blocks_used:        .word   0    ; out
gfi_mod_date:           .word   0    ; out
gfi_mod_time:           .word   0    ; out
gfi_create_date:        .word   0    ; out
gfi_create_time:        .word   0    ; out

        IO_BUFFER := $800       ; ???

open_params:
open_param_count:       .byte   3         ; in
open_pathname:          .addr   path      ; in
open_io_buffer:         .addr   IO_BUFFER ; in
open_ref_num:           .byte   0         ; out

geteof_params:
geteof_param_count:     .byte   2 ; in
geteof_ref_num:         .byte   0 ; in, populated at runtime
geteof_eof:             .res 3, 0 ; out

read_params:
read_param_count:       .byte   5 ; in
read_ref_num:           .byte   0 ; in, populated at runtime
read_data_buffer:       .addr   0 ; in, populated at runtime
read_request_count:     .word   0 ; in, populated at runtime
read_trans_count:       .word   0 ; out

close_params:
close_param_count:      .byte   1 ; in
close_ref_num:          .byte   0 ; in, populated at runtime

quit_params:
quit_param_count:       .byte   4 ; in
quit_type:              .byte   0 ; in
quit_res1:              .word   0 ; reserved
quit_res2:              .byte   0 ; reserved
quit_res3:              .word   0 ; reserved

        .assert * < $3E0, error, "proc too big"
        .endproc
        sizeof_proc = .sizeof(proc)
        .org __saved_org__ + sizeof_proc

;;; ============================================================
;;; Integer BASIC Implementation
;;; ============================================================

        .proc intbasic
        .include "IntegerBASIC_cc65.s"
        .endproc
        sizeof_intbasic = .sizeof(intbasic)
        BASIC   = intbasic::BASIC ; jsr COLD ; jmp WARM
        COLD    = intbasic::COLD
        WARM    = intbasic::WARM
        RUNWARM = intbasic::RUNWARM
        END     = intbasic::END
        ERRMESS = intbasic::ERRMESS
        LOAD    = intbasic::LOAD
        SAVE    = intbasic::SAVE
