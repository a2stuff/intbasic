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
;;;          | Command   |
;;;          | Processor |
;;;  $B625   +-----------+  Initialize
;;;          | IntBASIC  |
;;;          |           |
;;;          |           |
;;;  $A000   +-----------+  BASIC_START
;;;          | IO_BUFFER |
;;;  $9C00   +-----------+  HIMEM
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
CSWL    =       $36
A1L     =       $3c     ;general purpose
A1H     =       $3d     ;general purpose
A2L     =       $3e     ;general purpose
A2H     =       $3f     ;general purpose
A4L     =       $42     ;general purpose
A4H     =       $43     ;general purpose
HOME    =       $FC58
MOVE    =       $FE2C
PRBYTE  =       $FDDA
COUT    =       $FDED

;;; ProDOS Equates
MLI             := $BF00
DEVNUM          := $BF30
BITMAP          := $BF58
BITMAP_SIZE     =  24
DATELO          := $BF90
TIMELO          := $BF92

QUIT            = $65
CREATE          = $C0
DESTROY         = $C1
RENAME          = $C2
SET_FILE_INFO   = $C3
GET_FILE_INFO   = $C4
ON_LINE         = $C5
SET_PREFIX      = $C6
GET_PREFIX      = $C7
OPEN            = $C8
READ            = $CA
WRITE           = $CB
CLOSE           = $CC
GET_EOF         = $D1

FILE_ENTRY_SIZE = $27

FT_TXT          = $04
FT_BIN          = $06
FT_DIR          = $0F
FT_INT          = $FA
FT_IVR          = $FB
FT_BAS          = $FC
FT_SYS          = $FF

ERR_FILE_NOT_FOUND              = $46
ERR_DUPLICATE_FILENAME          = $47
ERR_INCOMPATIBLE_FILE_FORMAT    = $4A

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

;;; ============================================================
;;; System Program
;;; ============================================================

        IO_BUFFER := $9C00
        OUR_HIMEM := IO_BUFFER
        BASIC_START := $A000    ; Update `BITMAP` code if this changes

        PATHBUF := $280
        PATH2   := $2C0

.define STARTUP_FILE_NAME "HELLO"

;;; ProDOS Interpreter Protocol
;;; ProDOS 8 Technical Reference Manual
;;; 5.1.5.1 - Starting System Programs
        .org $2000
        jmp     start
        .byte   $EE, $EE        ; Interpreter signature
        .byte   $41             ; path buffer length
path:   .byte   .strlen(STARTUP_FILE_NAME), STARTUP_FILE_NAME
        .res    path+$41-*,0    ; path buffer

;;; Just used to test if startup file exists
gfi_startup:
        .byte   $A      ; param_count (in)
        .addr   PATHBUF ; pathname (in)
        .byte   0       ; access (out)
        .byte   0       ; file_type (out)
        .word   0       ; aux_type (out)
        .byte   0       ; storage_type (out)
        .word   0       ; blocks_used (out)
        .word   0       ; mod_date (out)
        .word   0       ; mod_time (out)
        .word   0       ; create_date (out)
        .word   0       ; create_time (out)

;;; Shown if startup file does not exist
banner:
        ;;      "----------------------------------------"
        scrcode "              INTEGER BASIC"
        .byte   $8D
        scrcode "   COPYRIGHT 1977, APPLE COMPUTER INC."
        .byte   $8D
        .byte   0

start:

;;; --------------------------------------------------
;;; Copy path somewhere safe

        ldx     path
        stx     PATHBUF
        beq     done_path

        ;; Non-empty path - copy it
:       lda     path,x
        sta     PATHBUF,x
        dex
        bpl     :-

done_path:

;;; --------------------------------------------------
;;; Set PREFIX if blank

        ON_LINE_BUF = PATH2+1
        MLI_CALL GET_PREFIX, prefix_params
        lda     PATH2
        bne     prefix_ok
        lda     DEVNUM
        sta     on_line_unit_num
        MLI_CALL ON_LINE, on_line_params
        lda     ON_LINE_BUF
        and     #$0F            ; mask off length
        tax
        inx
        stx     PATH2
        lda     #'/'
        sta     PATH2+1
        MLI_CALL SET_PREFIX, prefix_params
prefix_ok:

;;; --------------------------------------------------
;;; Display banner (if startup file not present)

        MLI_CALL GET_FILE_INFO, gfi_startup
        beq     skip_banner

do_banner:
        jsr     HOME
        ldx     #0
:       lda     banner,x
        beq     done_banner
        jsr     COUT
        inx
        bne     :-              ; always

skip_banner:
        ;; Unless Open- or Solid-Apple is down, hook END/ERRMESS to QUIT
        lda     BUTN0
        ora     BUTN1
        bmi     done_banner

        lda     #OPC_JMP_abs
        LDXY    #reloc__QuitFromIntBASIC
        sta     reloc + (intbasic__WARM   - BASIC_START)
        STXY    reloc + (intbasic__WARM+1 - BASIC_START)
done_banner:

;;; --------------------------------------------------
;;; Bug fixes

        GR_TOKEN = 76
        LDXY    #reloc__OurSETGR
        stx     reloc + (intbasic__VERBADRL - BASIC_START) + GR_TOKEN
        sty     reloc + (intbasic__VERBADRH - BASIC_START) + GR_TOKEN

;;; --------------------------------------------------
;;; Hook CALL instruction

        lda     #OPC_JMP_abs
        LDXY    #reloc__OurCALL
        sta     reloc + (intbasic__CALL - BASIC_START) + 3
        STXY    reloc + (intbasic__CALL - BASIC_START) + 4

;;; --------------------------------------------------
;;; Configure system bitmap

        ldx     #BITMAP_SIZE-1
        lda     #0
:       sta     BITMAP,x
        dex
        bpl     :-

        lda     #%11001111      ; ZP, Stack, Text Page 1
        sta     BITMAP

        .assert BASIC_START = $A000, error, "Keep BASIC_START and BITMAP in sync"
        lda     #%11111111
        sta     BITMAP+$A*2     ; Pages $A0-$A7
        sta     BITMAP+$A*2+1   ; Pages $A8-$AF
        sta     BITMAP+$B*2     ; Pages $B0-$B7
        sta     BITMAP+$B*2+1   ; Pages $B8-$BF (ProDOS global page)

;;; --------------------------------------------------
;;; Relocate INTBASIC and our stub up to target

        COPY16  #reloc, A1L
        COPY16  #reloc+sizeof_reloc-1, A2L
        COPY16  #BASIC_START, A4L
        ldy     #0
        jsr     MOVE

        ;; Hook the command parser
        LDXY    #reloc__CommandHook
        STXY    intbasic__GETCMD+3

        ;; Hook the output routine
        jsr     reloc__HookCSW

        ;; Start it up
        jmp     reloc__Initialize

        .out .sprintf("MEM: Bootstrap is $%04X bytes", * - $2000)

;;; --------------------------------------------------

;;; GET/SET_PREFIX
prefix_params:
prefix_param_count:     .byte   1 ; in
prefix_pathname:        .addr   PATH2 ; in

;;; ON_LINE
on_line_params:
on_line_param_count:    .byte   2 ; in
on_line_unit_num:       .byte   1 ; in
on_line_data_buffer:    .addr   ON_LINE_BUF ; in

;;; ============================================================
;;; Integer BASIC Implementation
;;; ============================================================

        .proc reloc
        .org ::BASIC_START
        .assert * .mod $200 = 0, error, "must be even-page aligned"

        .out .sprintf("MEM: $%04X BASIC_START", *)

.scope intbasic
        .include "IntegerBASIC_cc65.s"
.endscope ; intbasic

;;; ============================================================
;;; Initializer
;;; ============================================================

        .out .sprintf("MEM: $%04X Command Handler", *)

;;; Load program (if given) and invoke Integer BASIC

.proc Initialize
        jsr     SwapZP          ; ProDOS > IntBASIC
        jsr     intbasic::COLD
        LDXY    #OUR_HIMEM
        STXY    intbasic::HIMEM
        jsr     intbasic::NEW   ; reset PP, PV, stacks
        jsr     SwapZP          ; IntBASIC > ProDOS

        ;; Do we have a path?
        lda     PATHBUF
        beq     warm
        jsr     LoadINTFile
        bne     warm

        ;; Run it
        jsr     SwapZP          ; ProDOS > IntBASIC
        jmp     intbasic::RUN

        ;; Show prompt
warm:   jsr     SwapZP          ; ProDOS > IntBASIC
        jmp     intbasic::WARM
.endproc ; Initialize

;;; ============================================================

;;; Input: Path to load in `PATHBUF`
;;; Output: ProDOS error code in A ($00 = success)
;;; Assert: ProDOS ZP swapped in
.proc LoadINTFile
        ;; Check type, bail if not INT
        lda     #FT_INT
        jsr     GetFileInfoRequireType
        bne     finish

        ;; Open the file
        jsr     Open
        bne     finish

        ;; --------------------------------------------------
        ;; Compute the load address

        ;; Get file size
        MLI_CALL GET_EOF, geteof_params
        bne     close

        ;; In theory we should check geteof_eof+2 and fail
        ;; if > 64k, but how would such a file be created?

        ;; At this point we're committed - reset HIMEM
        LDXY    #OUR_HIMEM
        STXY    intbasic::HIMEM

        ;; Set up zero page locations for the calculation
        jsr     SwapZP          ; ProDOS > IntBASIC
        LDXY    geteof_eof
        STXY    intbasic::ACC
        STXY    rw_request_count

        ;; On any error, fail the load
        JmpMEMFULL      := intbasic_err
        BcsJmpMEMFULL   := intbasic_err
        @LF118          := intbasic_err

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
        COPY16  intbasic::PP, rw_data_buffer
        jsr     SwapZP          ; IntBASIC > ProDOS

        jsr     Read
close:  jsr     Close
finish: rts

        ;; Failure with IntBASIC ZP swapped in - restore ProDOS and flag error
intbasic_err:
        jsr     SwapZP          ; IntBASIC > ProDOS
        lda     #ERR_INCOMPATIBLE_FILE_FORMAT
        bne     close           ; always
.endproc ; LoadINTFile

;;; ============================================================
;;; Helpers, to save space

.proc GetFileInfo
        lda     #$A             ; param count for GET_FILE_INFO
        sta     gfi_param_count
        MLI_CALL GET_FILE_INFO, gfi_params
        rts
.endproc

.proc Open
        MLI_CALL OPEN, open_params
        pha
        lda     open_ref_num
        sta     geteof_ref_num
        sta     rw_ref_num
        sta     close_ref_num
        pla
        rts
.endproc

.proc Read
        MLI_CALL READ, rw_params
        rts
.endproc

.proc Close
        pha
        MLI_CALL CLOSE, close_params
        pla
        rts
.endproc

;;; ============================================================
;;; ProDOS Parameter Blocks

;;; GET_FILE_INFO / SET_FILE_INFO
gfi_params:
gfi_param_count:        .byte   0       ; in, populated at runtime
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

;;; READ/WRITE
rw_params:
rw_param_count:         .byte   4 ; in
rw_ref_num:             .byte   0 ; in, populated at runtime
rw_data_buffer:         .addr   0 ; in, populated at runtime
rw_request_count:       .word   0 ; in, populated at runtime
rw_trans_count:         .word   0 ; out

;;; CLOSE
close_params:
close_param_count:      .byte   1 ; in
close_ref_num:          .byte   0 ; in, populated at runtime

;;; GET/SET_PREFIX
prefix_params:
prefix_param_count:     .byte   1 ; in
prefix_pathname:        .addr   PATHBUF ; in

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
create_file_type:       .byte   0       ; in, populated at runtime
create_aux_type:        .word   0       ; in
create_storage_type:    .byte   0       ; in
create_date:            .word   0       ; in
create_time:            .word   0       ; in

;;; DESTROY
destroy_params:
destroy_param_count:    .byte   1       ; in
destroy_pathname:       .addr   PATHBUF ; in

;;; RENAME
rename_params:
rename_param_count:    .byte   2       ; in
rename_pathname:       .addr   PATH2   ; in
rename_new_pathname:   .addr   PATHBUF ; in

;;; ============================================================

;;; Swap a chunk of the zero page that both IntBASIC and ProDOS use
;;; Preserves: A,P
.proc SwapZP
        php
        pha

        ldx     #ZP_SAVE_LEN-1
:       lda     ZP_SAVE_ADDR,x
        ldy     zp_stash,x
        sta     zp_stash,x
        tya
        sta     ZP_SAVE_ADDR,x
        dex
        bpl     :-

        pla
        plp
        rts

zp_stash:
        .res    ::ZP_SAVE_LEN
.endproc ; SwapZP

;;; ============================================================
;;; Command Hook
;;; ============================================================

.enum ParseFlags
        path     = %00000001    ; parse path
        path_opt = %00000010    ; path is optional (not an error if empty)
        path2    = %00000100    ; parse second path (for RENAME)
        slotnum  = %00001000    ; slot number (for PR#)
        address  = %00010000    ; parse An
        length   = %00100000    ; parse Ln
        ignore   = %10000000    ; ignore all (for MON/NOMON)
.endenum

;;; Command Hook - replaces MON_NXTCHAR call in GETCMD to
;;; allow extra commands to be added.
;;; Assert: Called with IntBASIC ZP swapped in
.proc CommandHook
        jsr     intbasic::MON_NXTCHAR
        sta     save_a          ; last char pressed
        stx     save_x          ; position in input buffer

        jsr     HookCSW         ; needed after IN#3

        jsr     ExecBuffer
        bcc     :+

        ;; Pass buffer on to IntBASIC to parse
        save_a := *+1
        lda     #$00            ; self-modified
        save_x := *+1
        ldx     #$00            ; self-modified
        rts
:
        ;; Pass an empty buffer on to IntBASIC
        ldx     #0
        lda     #$8D            ; CR
        sta     intbasic::IN,x
        rts
.endproc ; CommandHook

;;; ============================================================
;;; Executes the command in the input buffer ($200)
;;; Output: C=0 if valid, C=1 if not (e.g. let IntBASIC take it)
;;; Note: On syntax error, calls `intbasic::ERRMESS` (resets stack, etc)
;;; Assert: Called with IntBASIC ZP swapped in

.proc ExecBuffer
        ldx     #0              ; X = offset in cmdtable
        stx     cmdnum
        stx     PATHBUF         ; initialize to zero on each parse

        ;; Check command
loop:   ldy     #0              ; Y = offset in buffer
        jsr     SkipSpaces
:       lda     cmdtable,x
        beq     dispatch
        cmp     intbasic::IN,y
        bne     next
        inx
        iny
        bne     :-              ; always

        ;; Next command
:       inx
next:   lda     cmdtable,x
        bne     :-
        inc     cmdnum
        inx
        lda     cmdtable,x
        bne     loop

        ;; No match
pass:   sec
        rts

        ;; Dispatch to matching command
dispatch:
        cmdnum := *+1
        ldx     #$00            ; self-modified

        lda     cmdproclo,x
        sta     disp
        lda     cmdprochi,x
        sta     disp+1
        lda     cmdparse,x
        sta     parse_flags

        ;; ..............................
        ;; Parse arguments

        lda     parse_flags
        .assert ParseFlags::ignore = $80, error, "enum mismatch"
        bpl     :+
        clc
        rts
:
        and     #ParseFlags::path
        beq     :+
        jsr     ParsePath
        bne     :+
        ora     cmdnum          ; if RUN (idx 0), yield to IntBASIC
        beq     pass
        lda     parse_flags     ; was it optional?
        and     #ParseFlags::path_opt
        beq     syn             ; required, so error
:
        lda     parse_flags
        and     #ParseFlags::path2
        beq     :+
        jsr     ParseComma
        bne     syn
        jsr     ParsePath
        beq     syn
:
        jsr     ParseParams
        bcs     syn

        lda     parse_flags
        and     #ParseFlags::slotnum
        beq     :+
        jsr     ParseSlotNum
        bcs     syn
:
        ;; Anything remaining (except spaces) is an error
        jsr     SkipSpaces
        cmp     #$8D
        bne     syn

        ;; ..............................
        ;; Actual dispatch

        jsr     SwapZP          ; IntBASIC > ProDOS
        disp := *+1
        jsr     $FFFF           ; self-modified
        jsr     SwapZP          ; ProDOS > IntBASIC
        bmi     syn
        bne     :+
        clc
        rts
:
        ;; ..............................
        ;; Show error messages

        ;; ProDOS error
        pha
        ldx     #0
:       lda     message,x
        beq     :+
        jsr     intbasic::MON_COUT
        inx
        bne     :-              ; always
:       pla
        jsr     PRBYTE
        jsr     intbasic::MON_CROUT
        jmp     intbasic::ERRMESS+3

        ;; Syntax error
syn:    ldy     #<intbasic::ErrMsg02 ;"SYNTAX"
        jmp     intbasic::ERRMESS

message:
        .byte   $87             ; BELL
        scrcode "*** PRODOS ERR $"
        .byte   0

NUM_CMDS = 21

cmdtable:
        scrcode "RUN"           ; must be 0 for special handling
        .byte   0
        scrcode "BYE"
        .byte   0
        scrcode "SAVE"
        .byte   0
        scrcode "LOAD"
        .byte   0
        scrcode "CHAIN"
        .byte   0
        scrcode "PREFIX"
        .byte   0
        scrcode "CATALOG"       ; must precede "CAT"
        .byte   0
        scrcode "CAT"
        .byte   0
        scrcode "DELETE"
        .byte   0
        scrcode "RENAME"
        .byte   0
        scrcode "BSAVE"
        .byte   0
        scrcode "BLOAD"
        .byte   0
        scrcode "BRUN"
        .byte   0
        scrcode "PR#"
        .byte   0
        scrcode "MON"
        .byte   0
        scrcode "NOMON"
        .byte   0
        scrcode "LOCK"
        .byte   0
        scrcode "UNLOCK"
        .byte   0
        scrcode "STORE"
        .byte   0
        scrcode "RESTORE"
        .byte   0
        scrcode "-"
        .byte   0
        .byte   0               ; sentinel

        MonCmd := 0             ; ignored
        NomonCmd := 0
cmdproclo:
        .byte   <RunCmd,<QuitCmd,<SaveCmd,<LoadCmd,<ChainCmd,<PrefixCmd,<CatCmd,<CatCmd,<DeleteCmd,<RenameCmd,<BSaveCmd,<BLoadCmd,<BRunCmd,<PRCmd,<MonCmd,<NomonCmd,<LockCmd,<UnlockCmd,<StoreCmd,<RestoreCmd,<DashCmd
cmdprochi:
        .byte   >RunCmd,>QuitCmd,>SaveCmd,>LoadCmd,>ChainCmd,>PrefixCmd,>CatCmd,>CatCmd,>DeleteCmd,>RenameCmd,>BSaveCmd,>BLoadCmd,>BRunCmd,>PRCmd,>MonCmd,>NomonCmd,>LockCmd,>UnlockCmd,>StoreCmd,>RestoreCmd,>DashCmd
        .assert * - cmdproclo = NUM_CMDS * 2, error, "table size"

cmdparse:
        .byte   ParseFlags::path | ParseFlags::path_opt ; RUN
        .byte   0                                       ; BYE
        .byte   ParseFlags::path                        ; SAVE
        .byte   ParseFlags::path                        ; LOAD
        .byte   ParseFlags::path                        ; CHAIN
        .byte   ParseFlags::path | ParseFlags::path_opt ; PREFIX
        .byte   ParseFlags::path | ParseFlags::path_opt ; CATALOG
        .byte   ParseFlags::path | ParseFlags::path_opt ; CAT
        .byte   ParseFlags::path                        ; DELETE
        .byte   ParseFlags::path | ParseFlags::path2    ; RENAME
        .byte   ParseFlags::path | ParseFlags::address | ParseFlags::length ; BSAVE
        .byte   ParseFlags::path | ParseFlags::address  ; BLOAD
        .byte   ParseFlags::path | ParseFlags::address  ; BRUN
        .byte   ParseFlags::slotnum                     ; PR#
        .byte   ParseFlags::ignore                      ; MON
        .byte   ParseFlags::ignore                      ; NOMON
        .byte   ParseFlags::path                        ; LOCK
        .byte   ParseFlags::path                        ; UNLOCK
        .byte   ParseFlags::path                        ; STORE
        .byte   ParseFlags::path                        ; RESTORE
        .byte   ParseFlags::path                        ; -
        .assert * - cmdparse = NUM_CMDS, error, "table size"

parse_flags:
        .byte   0

;;; ============================================================
;;; Advance Y, and get next character.
;;; Output: A = char, Z=1 if CR or ',' or ' '

.proc AdvanceAndGetNextChar
        iny
        .assert * = GetNextChar, error, "fall through"
.endproc

;;; ============================================================
;;; Note: Doesn't advance Y
;;; Output: A = char, Z=1 if CR or ',' or ' '

.proc GetNextChar
        lda     intbasic::IN,y
        cmp     #','|$80
        beq     ret
        cmp     #' '|$80
        beq     ret
        cmp     #$8D            ; CR
ret:    rts
.endproc ; GetNextChar

;;; ============================================================
;;; Parse path from command line into `PATHBUF`; skips leading spaces,
;;; stops on newline or comma.

;;; Input: Y = end of command in `intbasic::IN`
;;; Output: `PATHBUF` is length-prefixed path, A=length, w/ Z set
;;;         Previous `PATHBUF` copied to `PATH2`
;;; Assert: `PATHBUF` is valid
.proc ParsePath
        ;; Copy first path to PATH2
        ldx     PATHBUF
        stx     PATH2
        beq     start
:       lda     PATHBUF,x
        sta     PATH2,x
        dex
        bne     :-

start:
        ;; Get next path
        jsr     SkipSpaces
        ldx     #0
        jsr     GetNextChar
        cmp     #'/'|$80        ; must start with /
        beq     loop            ; or alpha
        cmp     #'A'|$80
        bcc     done
loop:   jsr     GetNextChar
        beq     done            ; if CR or ','
        and     #$7F
        sta     PATHBUF+1,x
        inx
        iny
        bne     loop            ; always

done:   stx     PATHBUF
        txa
        rts
.endproc ; ParsePath

;;; ============================================================
;;; Skip over spaces in input buffer
;;; Input: Y = current position
;;; Output: Y = new position, A = char at new position, X unchanged

.proc SkipSpaces
:       lda     intbasic::IN,y
        iny
        cmp     #' '|$80
        beq     :-
        dey
        rts
.endproc ; SkipSpaces

;;; ============================================================
;;; Tries to consume a comma from the input; skips leading spaces
;;; Output: Z=1 if comma seen, Z=0 otherwise
.proc ParseComma
        jsr     SkipSpaces
        cmp     #','|$80
        bne     ret
        iny
        lda     #0              ; set Z=1 after INY
ret:    rts
.endproc ; ParseComma

;;; ============================================================

;;; Output: C=1 on syntax error, C=0 and `slotnum` populated otherwise
.proc ParseSlotNum
        jsr     GetNextChar
        cmp     #'0'|$80
        bcc     syn
        cmp     #'7'|$80+1
        bcs     syn
        and     #$0F
        sta     slotnum
        iny
        clc
        rts

syn:    sec
        rts
.endproc ; ParseSlotNum

;;; ============================================================
;;; Parse ,A<addr> and ,L<len> params if present (and ignore ,V<vol>)
;;;
;;; Input: Y = parse position in `intbasic::IN`
;;; Output: `param_addr` and `param_len` populated (or $0000)
;;;         C=1 on syntax error, C=0 otherwise

.proc ParseParams
        ;; Init all params to 0
        ldx     #(param_end - param_start)-1
        lda     #0
        sta     seen_params
:       sta     param_start,x
        dex
        bpl     :-

        ;; Parse an arg
loop:   jsr     ParseComma
        bne     ok              ; nope - we're done
        jsr     SkipSpaces

NUM_PARAMS = 3

        ldx     #NUM_PARAMS-1
:       cmp     param_table,x
        beq     get
        dex
        bpl     :-

syn:    sec
        rts

ok:     clc
        rts

param_table:                    ; parameter name
        .byte   'V'|$80, 'A'|$80, 'L'|$80
        .assert * - param_table = NUM_PARAMS, error, "table size"
flag_table:                     ; flag in `parse_flags` (0=ignored)
        .byte   0, ParseFlags::address, ParseFlags::length
        .assert * - flag_table = NUM_PARAMS, error, "table size"
offset_table:                   ; offset from `param_start` to store value
        .byte   0, param_addr - param_start, param_len - param_start
        .assert * - offset_table = NUM_PARAMS, error, "table size"

get:    txa                     ; A = table offset
        pha
        jsr     GetVal
        pla
        bcs     syn
        tax                     ; X = table offset

        ;; Validate we want this argument, note it was seen
        lda     flag_table,x
        beq     loop            ; ignored (V)
        and     parse_flags
        beq     syn             ; not wanted
        ora     seen_params
        sta     seen_params

        ;; Move acc into appropriate arg word
        lda     offset_table,x
        tax
        lda     acc
        sta     param_start,x
        lda     acc+1
        sta     param_start+1,x
        jmp     loop

;;; Parse decimal or hex word, populate `acc`
.proc GetVal
        lda     #0
        sta     acc
        sta     acc+1
        jsr     AdvanceAndGetNextChar
        cmp     #'$'|$80
        beq     hex


.proc decimal
        jsr     GetNextChar
        beq     syn             ; err if no digits
:       jsr     digit           ; convert if digit
        bcs     syn             ; not a digit
        jsr     AdvanceAndGetNextChar
        bne     :-              ; get another
        clc
        rts

        ;; A=char, shifts digit into `acc`
        ;; or return C=1 if invalid

digit:  cmp     #'0'|$80
        bcc     syn
        cmp     #'9'|$80+1
        bcs     syn
        and     #$0F
        pha

        ;; Multiply acc by 10
        lda     acc
        sta     tmpw
        lda     acc+1
        sta     tmpw+1
        ldx     #9
:       jsr     do_add
        dex
        bne     :-

        ;; Add in new units
        pla
        sta     tmpw
        lda     #0
        sta     tmpw+1
        jsr     do_add
        clc
        rts

        ;; Add `tmpw` into `acc`
do_add: clc
        lda     acc
        adc     tmpw
        sta     acc
        lda     acc+1
        adc     tmpw+1
        sta     acc+1
        rts

.endproc ; decimal

.proc hex
        jsr     AdvanceAndGetNextChar ; past '$'
        beq     syn             ; err if no digits
:       jsr     digit           ; convert if digit
        bcs     syn             ; not a digit
        jsr     AdvanceAndGetNextChar
        bne     :-              ; get another
        clc
        rts

        ;; A=char, shifts digit into `acc`
        ;; or return C=1 if invalid

digit:  cmp     #'0'|$80
        bcc     syn
        cmp     #'9'|$80+1
        bcc     :+
        cmp     #'A'|$80
        bcc     syn
        cmp     #'F'|$80+1
        bcs     syn
        sbc     #6              ; adjust to A -> 10
:       and     #$0F

        ;; Multiply `acc` by 16
        ldx     #3
:       asl     acc
        rol     acc+1
        dex
        bpl     :-

        ;; Add in new units
        ora     acc
        sta     acc
        clc
        rts

syn:    sec
        rts
.endproc ; hex

tmpw:   .word   0

.endproc ; GetVal

acc:    .word   0

.endproc ; ParseParams
.endproc ; ExecBuffer

;;; ============================================================
;;; Parsed Arguments
;;; ============================================================

param_start:
param_addr:
        .word   0
param_len:
        .word   0
param_end:

slotnum:
        .byte   0
seen_params:
        .byte   0

;;; ============================================================
;;; Commands return with ProDOS error code, $00 for success,
;;; or high bit set for SYNTAX ERR.
;;; Commands are run with ProDOS ZP swapped in
;;; ============================================================

;;; ============================================================
;;; "BYE"

.proc QuitFromIntBASIC
        jsr     SwapZP          ; IntBASIC > ProDOS
        .assert * = QuitCmd, error, "fall through"
.endproc

.proc QuitCmd
        MLI_CALL QUIT, quit_params
        brk
.endproc ; QuitCmd

;;; ============================================================
;;; "SAVE pathname"

.proc SaveCmd
        ;; Set file type, aux type, data address and length
        lda     #FT_INT
        sta     create_file_type

        lda     #0
        sta     create_aux_type
        sta     create_aux_type+1

        jsr     SwapZP          ; IntBASIC > ProDOS

        LDXY    intbasic::PP
        STXY    rw_data_buffer

        sec
        lda     intbasic::HIMEM
        sbc     intbasic::PP
        sta     rw_request_count
        lda     intbasic::HIMEM+1
        sbc     intbasic::PP+1
        sta     rw_request_count+1

        jsr     SwapZP          ; ProDOS > IntBASIC

        jmp     WriteFileCommon
.endproc ; SaveCmd

;;; ============================================================
;;; "LOAD pathname"

.proc LoadCmd
        jsr     LoadINTFile
        bne     ret

        jsr     SwapZP          ; ProDOS > IntBASIC
        jmp     intbasic::WARM

ret:    rts
.endproc ; LoadCmd

;;; ============================================================
;;; "CHAIN pathname"

.proc ChainCmd
        jsr     LoadINTFile
        bne     ret

        jsr     SwapZP          ; ProDOS > IntBASIC
        jmp     intbasic::RUNWARM

ret:    rts
.endproc ; LoadCmd

;;; ============================================================
;;; "STORE pathname"

.proc StoreCmd
        ;; Set file type, aux type, data address and length
        lda     #FT_IVR
        sta     create_file_type

        LDXY    #0
        STXY    create_aux_type

        jsr     SwapZP          ; IntBASIC > ProDOS

        LDXY    intbasic::LOMEM
        STXY    rw_data_buffer

        sec
        lda     intbasic::PV
        sbc     intbasic::LOMEM
        sta     rw_request_count
        lda     intbasic::PV+1
        sbc     intbasic::LOMEM+1
        sta     rw_request_count+1

        jsr     SwapZP          ; ProDOS > IntBASIC

        jmp     WriteFileCommon
.endproc ; StoreCmd

;;; ============================================================
;;; "STORE pathname"

.proc RestoreCmd
        ;; Check type, bail if not IVR
        lda     #FT_IVR
        jsr     GetFileInfoRequireType
        bne     finish

        ;; Set file type, aux type, data address and length
        jsr     SwapZP          ; IntBASIC > ProDOS
        LDXY    intbasic::LOMEM
        STXY    rw_data_buffer
        LDXY    #$FFFF          ; read everything
        STXY    rw_request_count
        jsr     SwapZP          ; ProDOS > IntBASIC

        jsr     OpenReadClose
        bne     finish

        ;; Success - update IB zero page
        jsr     SwapZP          ; IntBASIC > ProDOS
        clc
        lda     rw_trans_count
        adc     intbasic::LOMEM
        sta     intbasic::PV
        lda     rw_trans_count+1
        adc     intbasic::LOMEM+1
        sta     intbasic::PV+1
        jsr     SwapZP          ; ProDOS > IntBASIC

        lda     #0              ; success
finish:
        rts
.endproc ; RestoreCmd

;;; ============================================================
;;; "PREFIX" or "PREFIX pathname"

.proc PrefixCmd
        lda     PATHBUF
        beq     show

        ;; Set prefix
        MLI_CALL SET_PREFIX, prefix_params
        rts

        ;; Show current prefix
show:
        MLI_CALL GET_PREFIX, prefix_params
        bne     ret
        ldx     #0
:       cpx     PATHBUF
        beq     :+
        lda     PATHBUF+1,x
        ora     #$80
        jsr     intbasic::MON_COUT
        inx
        bne     :-              ; always
:       lda     #0              ; success
ret:    rts
.endproc ; PrefixCmd

;;; ============================================================
;;; "CAT" or "CAT path"

.proc CatCmd
        lda     PATHBUF
        beq     use_prefix

        ;; Verify file is a directory
        lda     #FT_DIR
        jsr     GetFileInfoRequireType
        beq     open
ret1:   rts

        ;; Use current prefix
use_prefix:
        MLI_CALL GET_PREFIX, prefix_params
        bne     ret1

        ENTRY_BUFFER := PATHBUF

open:   jsr     Open
        bne     ret1

        COPY16  #ENTRY_BUFFER, rw_data_buffer

        ;; Skip block pointers
        COPY16  #4, rw_request_count
        jsr     Read
        bne     ret1

        ;; Read header
        COPY16  #FILE_ENTRY_SIZE, rw_request_count
        jsr     Read
        bne     ret1

        jsr     intbasic::MON_CROUT
        lda     ENTRY_BUFFER + $00 ; storage_type / name_length
        and     #$F0
        cmp     #$F0            ; is volume dir?
        bne     :+
        lda     #'/'|$80        ; show leading '/'
        jsr     intbasic::MON_COUT
:
        jsr     print_entry_name

        ldx     #0
:       lda     header_str,x
        jsr     COUT
        inx
        cpx     #kHeaderStrLen
        bne     :-

        lda     ENTRY_BUFFER + $24 - 4; entries_per_block
        sta     entries_per_block
        sta     entries_this_block
        dec     entries_this_block ; this header counts as one

        COPY16  ENTRY_BUFFER + $25 - 4, file_count

next_file:
        ;; More files?
        lda     file_count
        ora     file_count+1
        beq     close
        lda     file_count
        bne     :+
        dec     file_count+1
:       dec     file_count

next_entry:
        ;; Advance to next entry (and next block if needed)
        lda     entries_this_block
        bne     :+
        COPY16  #5, rw_request_count
        jsr     Read            ; TODO: Handle EOF?
        bne     ret
        lda     entries_per_block
        sta     entries_this_block
:
        dec     entries_this_block
        COPY16  #FILE_ENTRY_SIZE, rw_request_count
        jsr     Read            ; TODO: Handle EOF?
        bne     ret

        ;; Active entry?
        lda     ENTRY_BUFFER + $00 ; storage_type / name_length
        beq     next_entry         ; inactive - skip

        ;; Entry display: locked, name, type, block count, date
        ldx     #' '|$80
        lda     ENTRY_BUFFER + $1E ; access
        and     #%11000010         ; destroy, rename, write
        bne     :+
        ldx     #'*'|$80        ; locked
:       txa
        jsr     intbasic::MON_COUT

        ;; Name
        jsr     print_entry_name

        ;; Type
        lda     #17
        sta     CH
        jsr     print_entry_type

        ;; Blocks
        lda     #22
        sta     CH
        ldx     ENTRY_BUFFER + $13 ; blocks_used lo
        lda     ENTRY_BUFFER + $14 ; blocks_used hi
        jsr     intbasic::PRDEC

        ;; Date
        lda     #28
        sta     CH
        jsr     print_entry_date

        jsr     intbasic::MON_CROUT

        ;; Next file, unless the use cancelled
        lda     KBD
        cmp     #$83            ; Ctrl+C
        beq     :+
        jmp     next_file
:       sta     KBDSTRB

close:  jsr     Close
ret:    rts

.proc print_entry_name
        ;; Print the name
        lda     ENTRY_BUFFER + $00 ; storage_type / name_length
        and     #$0F            ; name_length
        sta     @len
        ldx     #0
:       lda     ENTRY_BUFFER + $01,x ; file_name
        ora     #$80
        jsr     intbasic::MON_COUT
        inx
        @len := *+1
        cpx     #$00            ; self-modified
        bne     :-
        rts
.endproc ; print_entry_name

.proc print_entry_type
        ldx     #0
:       lda     types,x
        beq     :+
        cmp     ENTRY_BUFFER + $10
        beq     show
        inx
        inx
        inx
        inx
        bne     :-              ; always
:
        ;; Fallback
        lda     #'$'|$80
        jsr     intbasic::MON_COUT
        lda     ENTRY_BUFFER + $10
        jmp     PRBYTE

        ;; Show type
show:   ldy     #3
:       inx
        lda     types+0,x
        jsr     intbasic::MON_COUT
        dey
        bne     :-
        rts

types:
        .byte   FT_TXT
        scrcode "TXT"
        .byte   FT_BIN
        scrcode "BIN"
        .byte   FT_DIR
        scrcode "DIR"
        .byte   FT_INT
        scrcode "INT"
        .byte   FT_IVR
        scrcode "IVR"
        .byte   FT_BAS
        scrcode "BAS"
        .byte   FT_SYS
        scrcode "SYS"
        .byte   0               ; sentinel
.endproc ; print_entry_type

.proc print_entry_date
        DATE := ENTRY_BUFFER + $21

        lda     DATE
        ora     DATE+1
        beq     no_date

        ;; Day
        lda     DATE            ; MMMDDDDD
        and     #%00011111      ; 000DDDDD
        cmp     #10
        bcs     :+
        inc     CH
:       tax
        lda     #0
        jsr     intbasic::PRDEC

        lda     #'-'|$80
        jsr     COUT

        ;; Month
        lda     DATE+1          ; YYYYYYYM
        lsr                     ; high bit of M into C
        pha
        lda     DATE            ; MMMDDDDD
        ror                     ; MMMMDDDD
        lsr                     ; 0MMMMDDD
        lsr                     ; 00MMMMDD
        lsr                     ; 000MMMMD
        lsr                     ; 0000MMMM
        tax                     ; 1-based to 0-based
        dex
        lda     months,x
        jsr     COUT
        lda     months+12,x
        jsr     COUT
        lda     months+24,x
        jsr     COUT

        lda     #'-'|$80
        jsr     COUT

        ;; Year

        pla
        cmp     #40             ; 0-39 is 2000-2039
        bcs     :+              ; Per Technical Note: ProDOS #28:
        adc     #100            ; ProDOS Dates -- 2000 and Beyond
:       clc
        adc     #<1900
        tax
        lda     #0
        adc     #>1900
        jmp     intbasic::PRDEC

months:                         ; swizzled
        scrcode "JFMAMJJASOND"
        scrcode "AEAPAUUUECOE"
        scrcode "NBRRYNLGPTVC"

no_date:
        ldx     #0
:       lda     no_date_str,x
        jsr     COUT
        inx
        cpx     #kNoDateStrLen
        bne     :-
        rts

no_date_str:
        scrcode "<NO DATE>"
        kNoDateStrLen := * - no_date_str

.endproc ; print_entry_date

file_count:
        .word   0
entries_per_block:
        .byte   0
entries_this_block:
        .byte   0

header_str:
        .byte   $8D, $8D        ; CR
        scrcode " NAME           TYPE BLOCKS MODIFIED"
        .byte   $8D, $8D        ; CR
        kHeaderStrLen := * - header_str

.endproc ; CatCmd

;;; ============================================================
;;; "DELETE pathname"

.proc DeleteCmd
        MLI_CALL DESTROY, destroy_params
        rts
.endproc ; DeleteCmd

;;; ============================================================
;;; "RENAME pathname,pathname2"

.proc RenameCmd
        MLI_CALL RENAME, rename_params
        rts
.endproc ; RenameCmd

;;; ============================================================
;;; "BSAVE pathname,A<address>,L<length>"

.proc BSaveCmd
        ;; Validate params
        lda     seen_params
        and     #ParseFlags::address | ParseFlags::length
        cmp     #ParseFlags::address | ParseFlags::length
        bne     syn
        lda     param_len
        ora     param_len+1
        bne     :+
syn:    lda     #$FF            ; syntax error
        rts
:
        ;; Set file type, aux type, data address and length
        lda     #FT_BIN
        sta     create_file_type
        LDXY    param_addr
        STXY    create_aux_type
        STXY    rw_data_buffer
        LDXY    param_len
        STXY    rw_request_count

        jmp     WriteFileCommon
.endproc ; BSaveCmd

;;; ============================================================
;;; "BLOAD pathname[,A<address>]"

.proc BLoadCmd
        .assert * = LoadBINFile, error, "fall through"
.endproc ; BLoadCmd

.proc LoadBINFile
        ;; Check type, bail if not BIN
        lda     #FT_BIN
        jsr     GetFileInfoRequireType
        bne     finish

        LDXY    gfi_aux_type    ; default load address
        lda     seen_params
        and     #ParseFlags::address
        beq     :+
        LDXY    param_addr      ; arg override
:       STXY    rw_data_buffer

        LDXY    #$FFFF          ; read everything
        STXY    rw_request_count

        jsr     OpenReadClose

finish:
        rts
.endproc ; LoadBINFile

;;; ============================================================
;;; "-<file>"

.proc DashCmd
        jsr     GetFileInfo
        bne     ret
        lda     gfi_file_type
        cmp     #FT_BIN
        beq     BRunCmd
        cmp     #FT_INT
        beq     RunCmd
        cmp     #FT_SYS
        bne     err

        LDXY    CSWHook__orig
        STXY    CSWL

        LDXY    #$2000
        STXY    rw_data_buffer
        LDXY    #$FFFF          ; read everything
        STXY    rw_request_count
        jsr     OpenReadClose
        bne     ret
        jmp     $2000

err:    lda     #ERR_INCOMPATIBLE_FILE_FORMAT
ret:    rts
.endproc ; DashCmd

;;; ============================================================
;;; "RUN pathname"

.proc RunCmd
        jsr     LoadINTFile
        bne     DashCmd::ret

        jsr     SwapZP          ; ProDOS > IntBASIC
        LDXY    intbasic::LOMEM ; reset vars
        STXY    intbasic::PV
        jmp     intbasic::RUN
.endproc ; RunCmd

;;; ============================================================
;;; "BRUN pathname[,A<address>]"

.proc BRunCmd
        jsr     LoadBINFile
        bne     :+
        jsr     run
        lda     #0
:       rts

run:    jmp     (rw_data_buffer)
.endproc ; BRunCmd

;;; ============================================================
;;; "LOCK path" and "UNLOCK path"

.proc LockCmd
        jsr     GetFileInfo
        bne     ret

        lda     gfi_access
        and     #%00111101      ; clear destroy, rename, write
store:  sta     gfi_access

        lda     #7              ; param count for SET_FILE_INFO
        sta     gfi_param_count
        MLI_CALL SET_FILE_INFO, gfi_params
ret:    rts
.endproc ; LockCmd

.proc UnlockCmd
        jsr     GetFileInfo
        bne     LockCmd::ret

        lda     gfi_access
        ora     #%11000010      ; set destroy, rename, write
        bne     LockCmd::store  ; always
.endproc ; UnlockCmd

;;; ============================================================
;;; "PR#<slot>"

.proc PRCmd
        lda     slotnum
        jsr     SwapZP          ; ProDOS > IntBASIC
        jsr     intbasic::MON_OUTPORT
        jsr     SwapZP          ; IntBASIC > ProDOS
        jsr     HookCSW
        lda     #0
        rts
.endproc ; PRCmd

;;; ============================================================

;;; Redirect CSW to `CSWHook`
;;; Output: CSW will be hooked (if not already)
;;; Preserves: A,X,Y
.proc HookCSW
        stx     save_x
        sty     save_y

        LDXY    CSWL
        cpx     #<CSWHook
        bne     hook
        cpy     #>CSWHook
        beq     skip            ; already hooked
hook:
        STXY    CSWHook__orig
        LDXY    #CSWHook
        STXY    CSWL

skip:
        save_x := *+1
        ldx     #$00            ; self-modified
        save_y := *+1
        ldy     #$00            ; self-modified
        rts
.endproc ; HookCSW

;;; ============================================================

.proc WriteFileCommon
        ;; If it exists, is it the desired type?
        jsr     GetFileInfo
        beq     check           ; exists, check type
        cmp     #ERR_FILE_NOT_FOUND
        beq     create          ; doesn't exist, create it
        bne     ret             ; other error

check:  lda     gfi_file_type   ; check type
        cmp     create_file_type
        beq     delete          ; okay to overwrite
        lda     #ERR_INCOMPATIBLE_FILE_FORMAT
        bne     ret             ; always

        ;; Delete so size/addr/etc updated
delete: MLI_CALL DESTROY, destroy_params
        bne     ret

        ;; Create the file
create:
        ldx     #3              ; set creation date
:       lda     DATELO,x
        sta     create_date,x
        dex
        bpl     :-

        MLI_CALL CREATE, create_params

        ;; Write the file
        jsr     Open
        bne     ret
        MLI_CALL WRITE, rw_params
        jsr     Close

ret:    rts
.endproc ; WriteFileCommon

;;; ============================================================
;;; Input: A = required type, path to load in `PATHBUF`
;;; Output: Z=1 if file exists and matching type, Z=0 and A=err otherwise
;;;         `gfi_params` left populated
.proc GetFileInfoRequireType
        sta     type
        jsr     GetFileInfo
        bne     ret
        lda     gfi_file_type
        type := *+1
        cmp     #$00            ; self-modified
        beq     ret
        lda     #ERR_INCOMPATIBLE_FILE_FORMAT
ret:    rts
.endproc ; GetFileInfoRequireType

;;; ============================================================

.proc OpenReadClose
        jsr     Open
        bne     ret
        jsr     Read
        jsr     Close
ret:    rts
.endproc ; OpenReadClose

;;; ============================================================

;;; Assert: Called with IntBASIC ZP swapped in

.proc CSWHook
        stx     saved_x

        bit     output_state
        bpl     look_for_cr     ; $00
        bvs     capture         ; $C0

        ;; State 1: ($80) Previous was CR; if Ctrl-D goto state 2, else state 3
        cmp     #$84            ; Ctrl-D
        bne     :+

        ldx     #0              ; yes, start capture
        stx     outbuf_index
        ldx     #$C0
        stx     output_state
        ldx     saved_x
        rts
:
        ldx     #$00            ; no, wait for another CR
        beq     set_state_and_cout ; always

        ;; State 2: ($C0) Capturing; on CR execute & reset, goto state
capture:
        ldx     outbuf_index
        sta     intbasic::IN,x
        inc     outbuf_index
        cmp     #$8D            ; CR
        beq     :+
        ldx     saved_x
        rts
:
        ldx     #$80            ; back to state 1
        stx     output_state

        ;; TODO: Do we need a special state to avoid recursion?
        jsr     ExecBuffer
        bcs     syn

        lda     #$8D
        ldx     saved_x
        rts

syn:    ldy     #<intbasic::ErrMsg02 ;"SYNTAX"
        jmp     intbasic::ERRMESS

        ;; State 3: ($00) Pass through; on CR, enter state 1
look_for_cr:
        cmp     #$8D            ; CR
        bne     chain           ; no
        ldx     #$80
set_state_and_cout:
        stx     output_state

chain:
        saved_x := *+1
        ldx     #$00            ; self-modified
        orig := *+1
        jsr     $FFFF           ; self-modified

        jmp     HookCSW         ; rehook necessary e.g. after PR#3

outbuf_index:
        .byte   0

;;; bit 7 & 6 = 1 1 => saw ctrl-D, now capturing
;;; bit 7 & 6 = 1 0 => saw CR, looking for ctrl-D
;;; bit 7 & 6 = 0 0 => normal output, look for CR
output_state:
        .byte   0

.endproc ; CSWHook
        CSWHook__orig := CSWHook::orig

;;; ============================================================
;;; Patch for GR issue

;;; IntBASIC: GR calls MON_GR which hits TXTCLR and MIXSET but not
;;; LORES, so if the previous program hit HIRES the lores screen
;;; will not show.

.proc OurSETGR
        sta     LORES
        jmp     intbasic::MON_SETGR
.endproc

;;; ============================================================
;;; Hook for CALL
;;;
;;; Replaces `JMP (ACC)`; instead, this checks if ACC is a call
;;; into the Programmer's Aid 1.6 sound routine. If so, it uses
;;; our copy instead.

;;; TODO: Consider making this table driven, to support more
;;; relocated Progarmmer's Aid routines.
.proc OurCALL
        ;; Override?
        lda     intbasic__ACC
        cmp     #<PA16
        bne     :+
        lda     intbasic__ACC+1
        cmp     #>PA16
        beq     PA16_COPY
:
        ;; Finish original intbasic::CALL
        jmp     (intbasic__ACC)
.endproc

        ;; TODO: Relocate this into LCBANK2 above $D400
PA16 := $D717
.proc PA16_COPY
        .include "Programmers_Aid_1.6_ca65.s"
.endproc

;;; ============================================================

.endproc ; reloc
        sizeof_reloc = .sizeof(reloc)

        ;; Exports
        reloc__Initialize := reloc::Initialize
        reloc__QuitFromIntBASIC := reloc::QuitFromIntBASIC
        reloc__CommandHook := reloc::CommandHook
        reloc__HookCSW := reloc::HookCSW
        reloc__OurSETGR := reloc::OurSETGR
        reloc__OurCALL := reloc::OurCALL

        intbasic__GETCMD := reloc::intbasic::GETCMD
        intbasic__WARM := reloc::intbasic::WARM
        intbasic__VERBADRL := reloc::intbasic::VERBADRL
        intbasic__VERBADRH := reloc::intbasic::VERBADRH
        intbasic__CALL := reloc::intbasic::CALL
        intbasic__ACC := reloc::intbasic::ACC

        .assert * <= MLI, error, "collision"
        .out .sprintf("MEM: $%04X end of Command Handler", *)
        .out .sprintf("MEM: $%04X bytes remaining before $BF00", $BF00 - *)
