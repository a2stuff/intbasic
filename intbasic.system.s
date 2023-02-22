        .include "apple2.inc"
        .include "apple2.mac"
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
PRBYTE  =       $FDDA

;;; ProDOS Equates
MLI             := $BF00
BITMAP          := $BF58
BITMAP_SIZE     =  24
DATELO          := $BF90
TIMELO          := $BF92

QUIT            = $65
CREATE          = $C0
GET_FILE_INFO   = $C4
OPEN            = $C8
READ            = $CA
WRITE           = $CB
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
        PATHBUF := $280

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
;;; Copy path somewhere safe

        ldx     path
        stx     PATHBUF
        beq     skip
:       lda     path,x
        sta     PATHBUF,x
        dex
        bpl     :-
skip:

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

;;; ============================================================
;;; Initializer
;;; ============================================================

;;; Load program (if given) and invoke Integer BASIC

Initialize:

        ;; Make LOAD/SAVE just QUIT to ProDOS
        lda     #OPC_JMP_abs
        LDXY    #quit
        sta     intbasic::LOAD+0
        STXY    intbasic::LOAD+1
        sta     intbasic::SAVE+0
        STXY    intbasic::SAVE+1

        ;; Hook the command parser
        LDXY    #CommandHook
        STXY    intbasic::GETCMD+3

        ;; Cold start - initialize Integer BASIC
        jsr     SwapZP          ; ProDOS > IntBASIC
        jsr     intbasic::COLD
        LDXY    #intbasic::BASIC
        STXY    intbasic::HIMEM
        STXY    intbasic::PP
        jsr     SwapZP          ; IntBASIC > ProDOS

        ;; Do we have a path?
        lda     PATHBUF
        bne     have_path

        ;; No, just show with prompt
        jsr     SwapZP          ; ProDOS > IntBASIC
        jmp     intbasic::WARM

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
        STXY    intbasic::ACC
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
@Loop:  lda     z:intbasic::HIMEM+1,x ;AUX = HIMEM - ACC
        sbc     z:intbasic::ACC+1,x
        sta     z:intbasic::AUX+1,x
        inx
        beq     @Loop
        bcc     JmpMEMFULL
        lda     intbasic::PV      ;compare PV to AUX
        cmp     intbasic::AUX
        lda     intbasic::PV+1
        sbc     intbasic::AUX+1
        bcs     BcsJmpMEMFULL
        lda     intbasic::ACC     ;is ACC zero?
        bne     @LF107
        lda     intbasic::ACC+1
        beq     @LF118  ;yes
@LF107: lda     intbasic::AUX     ;PP = AUX
        sta     intbasic::PP
        lda     intbasic::AUX+1
        sta     intbasic::PP+1

        ;; ..................................................

        ;; Load address c/o IntBASIC's program pointer
        COPY16  intbasic::PP, read_data_buffer
        jsr     SwapZP          ; IntBASIC -> ProDOS

        MLI_CALL READ, read_params

close:
        php
        MLI_CALL CLOSE, close_params
        plp
        bcc     :+
        jmp     quit
:

        ;; Hold Open- or Solid-Apple to allow returning to prompt
        lda     BUTN0
        ora     BUTN1
        bmi     :+

        ;; When END or ERRMESS invoked, just QUIT
        lda     #OPC_JMP_abs
        LDXY    #quit
        sta     intbasic::WARM
        STXY    intbasic::WARM+1
        sta     intbasic::ERRMESS
        STXY    intbasic::ERRMESS+1       ; patches JSR PRINTERR
:

        ;; Run the program
        jsr     SwapZP          ; ProDOS > IntBASIC
        jmp     intbasic::RUNWARM

close_and_quit:
        sec
        jmp     close

;;; ============================================================
;;; ProDOS Parameter Blocks

;;; GET_FILE_INFO
gfi_params:
gfi_param_count:        .byte   $A      ; in
gfi_pathname:           .addr   PATHBUF ; in
gfi_access:             .byte   0       ; out
gfi_file_type:          .byte   0       ; out
gfi_aux_type:           .word   0       ; out
gfi_storage_type:       .byte   0       ; out
gfi_blocks_used:        .word   0       ; out
gfi_mod_date:           .word   0       ; out
gfi_mod_time:           .word   0       ; out
gfi_create_date:        .word   0       ; out
gfi_create_time:        .word   0       ; out

;;; OPEN
open_params:
open_param_count:       .byte   3         ; in
open_pathname:          .addr   PATHBUF   ; in
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

;;; WRITE
write_params:
write_param_count:      .byte   4 ; in
write_ref_num:          .byte   1 ; in
write_data_buffer:      .addr   0 ; in
write_request_count:    .word   0 ; in
write_trans_count:      .word   0 ; out

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

;;; CREATE
create_params:
create_param_count:     .byte   7       ; in
create_pathname:        .addr   PATHBUF ; in
create_access:          .byte   $C3     ; in
create_file_type:       .byte   $FA     ; in INT
create_aux_type:        .word   0       ; in
create_storage_type:    .byte   0       ; in
create_date:            .word   0       ; in
create_time:            .word   0       ; in

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

;;; Command Hook - replaces MON_NXTCHAR call in GETCMD to
;;; allow extra commands to be added.
.proc CommandHook
        jsr     intbasic::MON_NXTCHAR
        sta     save_a                ; last char pressed
        stx     save_x                ; position in input buffer

        ldx     #0
        stx     cmdnum
        dex                     ; -1; immediately incremented to 0

        ;; Check command
loop:   ldy     #$FF            ; -1, immediately incremented to 0
:       iny
        inx
        lda     cmdtable,x
        beq     dispatch
        cmp     intbasic::IN,y
        beq     :-              ; next character

        ;; Next command
next:   inx
        lda     cmdtable,x
        bne     next
        inc     cmdnum
        lda     cmdtable+1,x
        bne     loop

        ;; No match
        save_a := *+1
        lda     #$00            ; self-modified
        save_x := *+1
        ldx     #$00            ; self-modified
        rts

        ;; Dispatch to matching command
dispatch:
        cmdnum := *+1
        ldx     #$00            ; self-modified
        lda     cmdproclo,x
        sta     disp
        lda     cmdprochi,x
        sta     disp+1
        disp := *+1
        jsr     $0000           ; self-modified

        ;; If it returns with C=0, pass empty command line back
        bcs     :+
        lda     #$8D
        ldx     #0
        sta     intbasic::IN,x
        rts
:
        ;; TODO: Show a better error than syntax error
        lda     #'!'|$80
        sta     intbasic::IN
        lda     #$8D
        sta     intbasic::IN+1
        ldx     #1
        rts

cmdtable:
        scrcode "BYE"
        .byte   0
        scrcode "ECHO"
        .byte   0
        scrcode "SAVE"
        .byte   0
        .byte   0               ; sentinel

cmdproclo:
        .byte   <ByeCmd,<EchoCmd,<SaveCmd
cmdprochi:
        .byte   >ByeCmd,>EchoCmd,>SaveCmd

ByeCmd := quit

;;; Proof of concept command - echos back rest of command line.
.proc EchoCmd
        dey
:       iny
        lda     intbasic::IN,y
        jsr     intbasic::MON_COUT
        cmp     #$8D
        bne     :-

        clc
        rts
.endproc

.proc SaveCmd
        jsr     GetPathname
        lda     PATHBUF
        bne     :+
        sec                     ; syntax error
        rts
:
        ;; Prepare the parameters
        ldx     #3
:       lda     DATELO,x
        sta     create_date,x
        dex
        bpl     :-

        lda     intbasic::PP
        sta     write_data_buffer
        lda     intbasic::PP+1
        sta     write_data_buffer+1

        sec
        lda     intbasic::HIMEM
        sbc     intbasic::PP
        sta     write_request_count
        lda     intbasic::HIMEM+1
        sbc     intbasic::PP+1
        sta     write_request_count+1

        jsr     SwapZP          ; IntBASIC > ProDOS

        ;; If it exists, is it INT?
        MLI_CALL GET_FILE_INFO, gfi_params
        beq     check
        cmp     #$46            ; error "File not found"
        beq     create
        bne     finish          ; other error
check:  lda     gfi_file_type   ; check type
        cmp     #$FA            ; INT
        beq     write           ; okay to overwrite
        lda     #$4A            ; error "Incompatible file format"
        bne     finish          ; always

        ;; Create the file
create:
        MLI_CALL CREATE, create_params
        beq     write           ; success
        cmp     #$47            ; ignore "Duplicate filename"
        bne     finish          ; otherwise fail

        ;; Write the file
write:
        MLI_CALL OPEN, open_params
        bne     finish
        lda     open_ref_num
        sta     write_ref_num
        sta     close_ref_num
        MLI_CALL WRITE, write_params
        pha
        MLI_CALL CLOSE, close_params
        pla

finish:
        pha
        jsr     SwapZP          ; ProDOS > IntBASIC
        pla
        bne     ShowError
        clc
        rts

.endproc

;;; Input: Y = end of command in `intbasic::IN`
;;; Output: `pathbuf` is length-prefixed path
.proc GetPathname
        ;; Skip spaces
:       lda     intbasic::IN,y
        cmp     #$A0            ; space
        bne     :+
        iny
        bne     :-              ; always
:
        ;; Copy path
        ldx     #0
:       lda     intbasic::IN,y
        cmp     #$8D            ; CR
        beq     done
        and     #$7F
        sta     PATHBUF+1,x
        iny
        inx
        bne     :-              ; always

done:   stx     PATHBUF
        rts
.endproc ; GetPathname

.proc ShowError
        pha
        ldx     #0
:       lda     message,x
        beq     :+
        jsr     intbasic::MON_COUT
        inx
        bne     :-              ; always
:       pla
        jsr     PRBYTE
        clc
        rts

message:
        scrcode "PRODOS ERROR #$"
        .byte   0
.endproc ; ShowError

.endproc ; CommandHook

;;; ============================================================

.endproc ; reloc
        sizeof_reloc = .sizeof(reloc)
        .assert * <= IO_BUFFER, error, "collision"
        Initialize := reloc::Initialize
