        .include "apple2.inc"
        .include "opcodes.inc"

;;; ============================================================
;;; Memory map
;;; ============================================================

;;;          Main Memory           ROM
;;;  $FFFF   +-----------+         +-----------+
;;;  $F800   | ProDOS    |         | Monitor   |
;;;          |           |         +-----------+
;;;          |           |         | Applesoft |
;;;  $E000   |    +-----------+    |           |
;;;          |    | ProDOS    |    |           |
;;;  $D000   +----+-----------+    +-----------+
;;;                                | Firmware  |
;;;                                | I/O       |
;;;  $C000   +-----------+         +-----------+
;;;          | ProDOS GP |
;;;  $BF00   +-----------+
;;;          | IO_BUFFER |
;;;  $BB00   +-----------+
;;;          | (free)... |
;;;          | ......... |
;;;          | ......... |
;;; ~$B600   +-----------+
;;;          | Init      |  Program loader
;;;  $B425   +-----------+
;;;          | IntBASIC  |
;;;          |           |
;;;          |           |
;;;          |           |
;;;  $A000   +-----------+  HIMEM
;;;          | Program   |
;;;          |     |     |
;;;          |     v     |
;;;          :           :
;;;          :           :
;;;          |     ^     |
;;;          |     |     |
;;;          | Variables |
;;;  $0800   +-----------+  LOMEM
;;;          | Text Pg.1 |
;;;  $0400   +-----------+
;;;          | (free)... |
;;;  $0300   +-----------+
;;;          | Input Buf |
;;;  $0200   +-----------+
;;;          | Stack     |
;;;  $0100   +-----------+
;;;          | Zero Page |
;;;  $0000   +-----------+
;;;

;;; ============================================================
;;; Equates
;;; ============================================================

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

ZP_SAVE_ADDR    := $3A          ; ProDOS owns this ZP chunk
ZP_SAVE_LEN     =  $15

;;; ============================================================
;;; Macros
;;; ============================================================

.define _is_immediate(arg)       (.match (.mid (0, 1, {arg}), #))
.define _immediate_value(arg)    (.right (.tcount ({arg})-1, {arg}))

.macro LDXY arg1
        .if _is_immediate {arg1}
        ldx     #<_immediate_value {arg1}
        ldy     #>_immediate_value {arg1}
        .else
        ldx     arg1
        ldy     arg1+1
        .endif
.endmacro

.macro STXY addr
        stx     addr
        sty     addr+1
.endmacro

.macro COPY16 arg1, arg2
        LDXY    arg1
        STXY    arg2
.endmacro

.macro MLI_CALL call, params
        jsr     MLI
        .byte   call
        .addr   params
.endmacro

.macro PASCAL_STRING str, res
        .local  data
        .local  end
        .byte   end - data
data:   .byte   str
end:
        .if .paramcount > 1
        .res    res - (end - data), 0
        .endif
.endmacro

;;; ============================================================
;;; System Program
;;; ============================================================

        IO_BUFFER := $BB00

;;; ProDOS Interpreter Protocol
;;; ProDOS 8 Technical Reference Manual
;;; 5.1.5.1 - Starting System Programs
        .org $2000
        jmp     start
        .byte   $EE, $EE        ; Interpreter signature
        .byte   start - path    ; path buffer length
path:   PASCAL_STRING "", $40   ; path buffer
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
        lda     #%00000001
        sta     BITMAP+$B*2+1   ; ProDOS global page ($BF)

;;; --------------------------------------------------
;;; Relocate INTBASIC and our stub up to target

        COPY16  #reloc, A1L
        COPY16  #reloc+sizeof_reloc-1, A2L
        COPY16  #reloc_target, A4L
        ldy     #0
        jsr     MOVE

        jmp     Initialize

;;; ============================================================
;;; Integer BASIC Implementation
;;; ============================================================

        reloc_target := $A000
        .proc reloc
        .org ::reloc_target
        .scope intbasic
        .include "IntegerBASIC_cc65.s"
        .endscope

        ;; Entry points
        BASIC   = intbasic::BASIC ; jsr COLD ; jmp WARM
        COLD    = intbasic::COLD
        WARM    = intbasic::WARM
        RUNWARM = intbasic::RUNWARM
        END     = intbasic::END
        ERRMESS = intbasic::ERRMESS
        LOAD    = intbasic::LOAD
        SAVE    = intbasic::SAVE

        ;; Zero page locations, used during loading
        ACC     = intbasic::ACC
        AUX     = intbasic::AUX
        PP      = intbasic::PP
        PV      = intbasic::PV
        iHIMEM  = intbasic::HIMEM

;;; ============================================================
;;; Initializer
;;; ============================================================

;;; Load program (if given) and invoke Integer BASIC

Initialize:

        ;; Make LOAD/SAVE just QUIT to ProDOS
        lda     #OPC_JMP_abs
        LDXY    #quit
        sta     LOAD+0
        STXY    LOAD+1
        sta     SAVE+0
        STXY    SAVE+1

        ;; Cold start - initialize Integer BASIC
        jsr     SwapZP          ; ProDOS > IntBASIC
        jsr     COLD
        LDXY    #BASIC
        STXY    iHIMEM
        STXY    PP
        jsr     SwapZP          ; IntBASIC > ProDOS

        ;; Do we have a path?
        lda     path
        bne     have_path

        ;; No, just show with prompt
        jsr     SwapZP          ; ProDOS > IntBASIC
        jmp     WARM

        ;; --------------------------------------------------

        ;; Proc located here within range of branches
quit:   MLI_CALL QUIT, quit_params

        ;; --------------------------------------------------
        ;; Have path

have_path:
        ;; Check type, bail if not INT
        MLI_CALL GET_FILE_INFO, gfi_params
        bcs     quit
        lda     gfi_file_type
        cmp     #$FA            ; INT
        bne     quit

        ;; Open the file
        MLI_CALL OPEN, open_params
        bcs     quit
        lda     open_ref_num
        sta     geteof_ref_num
        sta     read_ref_num
        sta     close_ref_num

        ;; --------------------------------------------------
        ;; Compute the load address

        ;; Get file size
        MLI_CALL GET_EOF, geteof_params
        bcs     close

        ;; In theory we should check geteof_eof+2 and fail
        ;; if > 64k, but how would such a file be created?

        ;; Set up zero page locations for the calculation
        jsr     SwapZP          ; ProDOS > IntBASIC
        LDXY    geteof_eof
        STXY    ACC
        STXY    read_request_count

        ;; On any error, just fail back to ProDOS
        JmpMEMFULL      := close_and_quit
        BcsJmpMEMFULL   := close_and_quit
        @LF118          := close_and_quit

        ;; ..................................................
        ;; Logic c/o IntBASIC's LOAD routine
        ;; (z: addressing to ensure desired wrap-around)

        ldx     #$ff
        sec
@Loop:  lda     z:iHIMEM+1,x ;AUX = HIMEM - ACC
        sbc     z:ACC+1,x
        sta     z:AUX+1,x
        inx
        beq     @Loop
        bcc     JmpMEMFULL
        lda     PV      ;compare PV to AUX
        cmp     AUX
        lda     PV+1
        sbc     AUX+1
        bcs     BcsJmpMEMFULL
        lda     ACC     ;is ACC zero?
        bne     @LF107
        lda     ACC+1
        beq     @LF118  ;yes
@LF107: lda     AUX     ;PP = AUX
        sta     PP
        lda     AUX+1
        sta     PP+1

        ;; ..................................................

        ;; Load address c/o IntBASIC's program pointer
        COPY16  PP, read_data_buffer
        jsr     SwapZP          ; IntBASIC -> ProDOS

        MLI_CALL READ, read_params

close:
        php
        MLI_CALL CLOSE, close_params
        plp
        bcc     :+
        jmp     quit
:

        ;; When END or ERRMESS invoked, just QUIT
        lda     #OPC_JMP_abs
        LDXY    #quit
        sta     WARM
        STXY    WARM+1
        sta     ERRMESS
        STXY    ERRMESS+1       ; patches JSR PRINTERR

        ;; Run the program
        jsr     SwapZP          ; ProDOS > IntBASIC
        jmp     RUNWARM

close_and_quit:
        sec
        jmp     close

;;; ============================================================
;;; ProDOS Parameter Blocks

;;; GET_FILE_INFO
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

;;; OPEN
open_params:
open_param_count:       .byte   3         ; in
open_pathname:          .addr   path      ; in
open_io_buffer:         .addr   IO_BUFFER ; in
open_ref_num:           .byte   0         ; out

;;; GET_EOF
geteof_params:
geteof_param_count:     .byte   2 ; in
geteof_ref_num:         .byte   0 ; in, populated at runtime
geteof_eof:             .res 3, 0 ; out

;;; READ
read_params:
read_param_count:       .byte   4 ; in
read_ref_num:           .byte   0 ; in, populated at runtime
read_data_buffer:       .addr   0 ; in, populated at runtime
read_request_count:     .word   0 ; in, populated at runtime
read_trans_count:       .word   0 ; out

;;; CLOSE
close_params:
close_param_count:      .byte   1 ; in
close_ref_num:          .byte   0 ; in, populated at runtime

;;; QUIT
quit_params:
quit_param_count:       .byte   4 ; in
quit_type:              .byte   0 ; in
quit_res1:              .word   0 ; reserved
quit_res2:              .byte   0 ; reserved
quit_res3:              .word   0 ; reserved

;;; ============================================================

;;; Swap a chunk of the zero page that both IntBASIC and ProDOS use
.proc SwapZP
        ldx     #ZP_SAVE_LEN-1
:       lda     ZP_SAVE_ADDR,x
        ldy     zp_stash,x
        sta     zp_stash,x
        tya
        sta     ZP_SAVE_ADDR,x
        dex
        bpl     :-
        rts

zp_stash:
        .res    ::ZP_SAVE_LEN
.endproc

;;; ============================================================

        .endproc ; reloc
        sizeof_reloc = .sizeof(reloc)
        .assert * <= IO_BUFFER, error, "collision"
        Initialize := reloc::Initialize
