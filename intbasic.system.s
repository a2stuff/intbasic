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
;;;  $A200   +-----------+  BASIC_START
;;;          | IO_BUFFER |
;;;  $9E00   +-----------+  HIMEM
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
BITMAP          := $BF58
BITMAP_SIZE     =  24
DATELO          := $BF90
TIMELO          := $BF92

QUIT            = $65
CREATE          = $C0
DESTROY         = $C1
RENAME          = $C2
GET_FILE_INFO   = $C4
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

        IO_BUFFER := $9E00
        OUR_HIMEM := IO_BUFFER
        BASIC_START := $A200

        PATHBUF := $280
        PATH2   := $2C0

;;; ProDOS Interpreter Protocol
;;; ProDOS 8 Technical Reference Manual
;;; 5.1.5.1 - Starting System Programs
        .org $2000
        jmp     start
        .byte   $EE, $EE        ; Interpreter signature
        .byte   $41             ; path buffer length
path:   .res    $41,0           ; path buffer

banner:
        ;;      "----------------------------------------"
        scrcode "              INTEGER BASIC"
        .byte   $8D
        scrcode "   COPYRIGHT 1977, APPLE COMPUTER INC."
        .byte   $8D
        .byte   0

start:

;;; --------------------------------------------------
;;; Display banner

        jsr     HOME
        lda     path
        bne     done_banner
        ldx     #0
:       lda     banner,x
        beq     done_banner
        jsr     COUT
        inx
        bne     :-              ; always
done_banner:

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

        ;; Unless Open- or Solid-Apple is down, hook END/ERRMESS to QUIT
        lda     BUTN0
        ora     BUTN1
        bmi     done_path

        lda     #OPC_JMP_abs
        LDXY    #reloc__QuitCmd
        sta     reloc + (intbasic__WARM      - BASIC_START)
        STXY    reloc + (intbasic__WARM+1    - BASIC_START)
        sta     reloc + (intbasic__ERRMESS   - BASIC_START)
        STXY    reloc + (intbasic__ERRMESS+1 - BASIC_START)

done_path:

;;; --------------------------------------------------
;;; Configure system bitmap

        ldx     #BITMAP_SIZE-1
        lda     #0
:       sta     BITMAP,x
        dex
        bpl     :-

        lda     #%11001111      ; ZP, Stack, Text Page 1
        sta     BITMAP

        lda     #%00111111
        sta     BITMAP+$A*2     ; Pages $A2-$A7
        lda     #%11111111
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
        ;; Cold start - initialize Integer BASIC
        jsr     SwapZP          ; ProDOS > IntBASIC
        jsr     ColdStart

        ;; Do we have a path?
        lda     PATHBUF
        bne     have_path

        ;; No, just show with prompt
        jmp     intbasic::WARM

        ;; --------------------------------------------------
        ;; Have path

have_path:
        jsr     LoadINTFile
        beq     :+
        jmp     QuitCmd         ; fail - just QUIT back to ProDOS
:
        ;; Run the program
        jmp     intbasic::RUN
.endproc ; Initialize

;;; ============================================================

.proc ColdStart
        jsr     intbasic::COLD
        LDXY    #OUR_HIMEM
        STXY    intbasic::HIMEM
        jmp     intbasic::NEW   ; reset PP, PV, stacks
.endproc ; ColdStart

;;; ============================================================

;;; Input: Path to load in `PATHBUF`
;;; Output: ProDOS error code in A ($00 = success)
;;; Assert: IntBASIC ZP swapped in
.proc LoadINTFile
        jsr     SwapZP          ; IntBASIC > ProDOS

        ;; Check type, bail if not INT
        MLI_CALL GET_FILE_INFO, gfi_params
        bne     finish
        lda     gfi_file_type
        cmp     #FT_INT
        beq     open
        lda     #ERR_INCOMPATIBLE_FILE_FORMAT
        bne     finish          ; always

        ;; Open the file
open:
        MLI_CALL OPEN, open_params
        bne     finish
        lda     open_ref_num
        sta     geteof_ref_num
        sta     read_ref_num
        sta     close_ref_num

        ;; --------------------------------------------------
        ;; Compute the load address

        ;; Get file size
        MLI_CALL GET_EOF, geteof_params
        bne     close

        ;; In theory we should check geteof_eof+2 and fail
        ;; if > 64k, but how would such a file be created?

        ;; Set up zero page locations for the calculation
        jsr     SwapZP          ; ProDOS > IntBASIC
        LDXY    geteof_eof
        STXY    intbasic::ACC
        STXY    read_request_count

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
        COPY16  intbasic::PP, read_data_buffer
        jsr     SwapZP          ; IntBASIC > ProDOS

        MLI_CALL READ, read_params
        ;; TODO: Verify `read_trans_count`

close:
        pha
        MLI_CALL CLOSE, close_params
        pla

finish:
        jmp     SwapZP          ; ProDOS > IntBASIC

        ;; Failure with IntBASIC ZP swapped in - restore ProDOS and flag error
intbasic_err:
        jsr     SwapZP          ; IntBASIC > ProDOS
        lda     #ERR_INCOMPATIBLE_FILE_FORMAT
        bne     close           ; always
.endproc ; LoadINTFile

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

.enum ExecResult
        executed
        no_match
        syntax_error
.endenum

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
.proc CommandHook
        jsr     intbasic::MON_NXTCHAR
        sta     save_a          ; last char pressed
        stx     save_x          ; position in input buffer

        jsr     HookCSW         ; needed after IN#3

        jsr     ExecBuffer
        ldx     #0

        cmp     #ExecResult::no_match
        bne     :+
        save_a := *+1
        lda     #$00            ; self-modified
        save_x := *+1
        ldx     #$00            ; self-modified
        rts
:
        cmp     #ExecResult::syntax_error
        bne     :+
        lda     #'!'|$80
        sta     intbasic::IN,x
        inx
        lda     #$8D
        sta     intbasic::IN,x
        rts
:
        ;;      #ExecResult::executed
        lda     #$8D
        sta     intbasic::IN,x
        rts
.endproc ; CommandHook

.proc ExecBuffer
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
        lda     #ExecResult::no_match
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

        lda     #0
        sta     PATHBUF

        lda     parse_flags
        .assert ParseFlags::ignore = $80, error, "enum mismatch"
        bmi     done_parse

        and     #ParseFlags::path
        beq     :+
        jsr     GetPathname
        bne     :+
        lda     parse_flags     ; no path - was it optional?
        and     #ParseFlags::path_opt
        beq     syn             ; required, so error
:
        lda     parse_flags
        and     #ParseFlags::path2
        beq     :+
        jsr     ParseComma
        bne     syn
        jsr     GetPathname
        beq     syn
:
        lda     parse_flags
        and     #ParseFlags::address|ParseFlags::length
        beq     :+
        jsr     ParseArgs
        bcs     syn
:
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
done_parse:

        ;; ..............................
        ;; Actual dispatch

        disp := *+1
        jsr     $FFFF           ; self-modified
        bcs     syn

        lda     #ExecResult::executed
        rts

syn:    lda     #ExecResult::syntax_error
        rts

NUM_CMDS = 15

cmdtable:
        scrcode "BYE"
        .byte   0
        scrcode "SAVE"
        .byte   0
        scrcode "LOAD"
        .byte   0
        scrcode "RUN"
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
        .byte   0               ; sentinel

cmdproclo:
        .byte   <QuitCmd,<SaveCmd,<LoadCmd,<RunCmd,<PrefixCmd,<CatCmd,<CatCmd,<DeleteCmd,<RenameCmd,<BSaveCmd,<BLoadCmd,<BRunCmd,<PRCmd,<MonCmd,<MonCmd
cmdprochi:
        .byte   >QuitCmd,>SaveCmd,>LoadCmd,>RunCmd,>PrefixCmd,>CatCmd,>CatCmd,>DeleteCmd,>RenameCmd,>BSaveCmd,>BLoadCmd,>BRunCmd,>PRCmd,>MonCmd,>MonCmd
        .assert * - cmdproclo = NUM_CMDS * 2, error, "table size"

cmdparse:
        .byte   0                                       ; BYE
        .byte   ParseFlags::path                        ; SAVE
        .byte   ParseFlags::path                        ; LOAD
        .byte   ParseFlags::path | ParseFlags::path_opt ; RUN
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
        .assert * - cmdparse = NUM_CMDS, error, "table size"

parse_flags:
        .byte   0

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
.proc GetPathname
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
.endproc ; GetPathname

;;; ============================================================
;;; Skip over spaces in input buffer
;;; Input: Y = current position
;;; Output: Y = new position, A = char at new position

.proc SkipSpaces
:       lda     intbasic::IN,y
        iny
        cmp     #' '|$80
        beq     :-
        dey
        rts
.endproc

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
;;; Parse ,A<addr> and ,L<len> args if present
;;;
;;; Input: Y = parse position in `intbasic::IN`
;;; Output: `arg_addr` and `arg_len` populated (or $0000)
;;;         C=1 on syntax error, C=0 otherwise

.proc ParseArgs
        ;; Init all args to 0
        ldx     #(arg_end - arg_start)-1
        lda     #0
        sta     seen_args
:       sta     arg_start,x
        dex
        bpl     :-

        ;; Parse an arg
loop:   jsr     ParseComma
        bne     ok              ; nope - we're done
        jsr     SkipSpaces
        cmp     #'A'|$80
        beq     addr
        cmp     #'L'|$80
        beq     len

syn:    sec
        rts

ok:     clc
        rts

addr:   jsr     GetVal
        bcs     syn
        lda     #ParseFlags::address
        ldx     #arg_addr - arg_start
        bpl     apply           ; always

len:    jsr     GetVal
        bcs     syn
        lda     #ParseFlags::length
        ldx     #arg_len - arg_start
        .assert * = apply, error, "fall through"

apply:
        ;; Validate we want this argument, note it was seen
        and     parse_flags
        beq     syn
        ora     seen_args
        sta     seen_args
        ;; Move acc into appropriate arg word
        lda     acc
        sta     arg_start,x
        lda     acc+1
        sta     arg_start+1,x
        jmp     loop

;;; Parse decimal or hex word, populate `acc`
.proc GetVal
        lda     #0
        sta     acc
        sta     acc+1
        iny
        lda     intbasic::IN,y
        cmp     #'$'|$80
        beq     hex


.proc decimal
        jsr     GetNextChar
        beq     syn             ; err if no digits
:       jsr     digit           ; convert if digit
        bcs     syn             ; not a digit
        iny                     ; advance
        jsr     GetNextChar
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
        iny                     ; past '$'
        jsr     GetNextChar
        beq     syn             ; err if no digits
:       jsr     digit           ; convert if digit
        bcs     syn             ; not a digit
        iny                     ; advance
        jsr     GetNextChar
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

.endproc ; ParseArgs
.endproc ; ExecBuffer

;;; ============================================================
;;; Parsed Arguments
;;; ============================================================

arg_start:
arg_addr:
        .word   0
arg_len:
        .word   0
arg_end:

slotnum:
        .byte   0
seen_args:
        .byte   0

;;; ============================================================
;;; Commands should return with:
;;;   C=0 if command consumed (successful or otherwise)
;;;   C=1 on syntax error (e.g. no filename given)
;;; Commands are run with IntBASIC ZP swapped in
;;; ============================================================

;;; ============================================================
;;; "BYE"

.proc QuitCmd
        jsr     SwapZP          ; IntBASIC > ProDOS
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

        LDXY    intbasic::PP
        STXY    write_data_buffer

        sec
        lda     intbasic::HIMEM
        sbc     intbasic::PP
        sta     write_request_count
        lda     intbasic::HIMEM+1
        sbc     intbasic::PP+1
        sta     write_request_count+1

        jmp     WriteFileCommon
.endproc ; SaveCmd

;;; ============================================================
;;; "LOAD pathname"

.proc LoadCmd
        ;; Pop out of command hook - no going back now
        pla
        pla

        jsr     ColdStart
        jsr     LoadINTFile
        bne     err
        jmp     intbasic::WARM

err:
        jsr     ColdStart
        jmp     ShowError
.endproc ; LoadCmd

;;; ============================================================
;;; "RUN" or "RUN pathname"

.proc RunCmd
        ;; Pop out of command hook - no going back now
        pla
        pla

        lda     PATHBUF
        beq     run             ; no path, just RUN

        jsr     ColdStart
        jsr     LoadINTFile
        bne     LoadCmd::err
run:    jmp     intbasic::RUN
.endproc ; RunCmd

;;; ============================================================
;;; "PREFIX" or "PREFIX pathname"

.proc PrefixCmd
        lda     PATHBUF
        bne     set

        ;; Show current prefix
        jsr     SwapZP          ; IntBASIC > ProDOS
        MLI_CALL GET_PREFIX, prefix_params
        jsr     SwapZP          ; ProDOS > IntBASIC
        bne     err
        ldx     #0
:       cpx     PATHBUF
        beq     :+
        lda     PATHBUF+1,x
        ora     #$80
        jsr     intbasic::MON_COUT
        inx
        bne     :-              ; always
:       clc
        rts

        ;; Set prefix
set:
        jsr     SwapZP          ; IntBASIC > ProDOS
        MLI_CALL SET_PREFIX, prefix_params
        jsr     SwapZP          ; ProDOS > IntBASIC
err:    jmp     FinishCommand
.endproc ; PrefixCmd

;;; ============================================================
;;; "CAT" or "CAT path"

.proc CatCmd
        jsr     SwapZP          ; IntBASIC > ProDOS

        lda     PATHBUF
        beq     use_prefix

        ;; Verify file is a directory
        MLI_CALL GET_FILE_INFO, gfi_params
        beq     :+
        jmp     err
:
        lda     gfi_file_type
        cmp     #FT_DIR
        beq     open
        lda     #ERR_INCOMPATIBLE_FILE_FORMAT
        jmp     err

        ;; Use current prefix
use_prefix:
        MLI_CALL GET_PREFIX, prefix_params
        beq     :+
        jmp     err
:

        ENTRY_BUFFER := PATHBUF

open:   MLI_CALL OPEN, open_params
        beq     :+
        jmp     err
:
        lda     open_ref_num
        sta     read_ref_num
        sta     close_ref_num

        COPY16  #ENTRY_BUFFER, read_data_buffer

        ;; Skip block pointers
        COPY16  #4, read_request_count
        MLI_CALL READ, read_params
        beq     :+
        jmp     err
:
        ;; Read header
        COPY16  #FILE_ENTRY_SIZE, read_request_count
        MLI_CALL READ, read_params
        beq     :+
        jmp     err
:
        jsr     intbasic::MON_CROUT
        lda     ENTRY_BUFFER + $00 ; storage_type / name_length
        and     #$F0
        cmp     #$F0            ; is volume dir?
        bne     :+
        lda     #'/'|$80        ; show leading '/'
        jsr     intbasic::MON_COUT
:
        jsr     print_entry_name
        jsr     intbasic::MON_CROUT
        jsr     intbasic::MON_CROUT

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
        COPY16  #5, read_request_count
        MLI_CALL READ, read_params ; TODO: Handle EOF?
        bne     err
        lda     entries_per_block
        sta     entries_this_block
:
        dec     entries_this_block
        COPY16  #FILE_ENTRY_SIZE, read_request_count
        MLI_CALL READ, read_params ; TODO: Handle EOF?
        bne     err

        ;; Active entry?
        lda     ENTRY_BUFFER + $00 ; storage_type / name_length
        beq     next_entry         ; inactive - skip

        ;; Entry display: name, type, block count
        lda     #' '|$80
        jsr     intbasic::MON_COUT
        jsr     print_entry_name
        jsr     print_entry_type
        lda     #' '|$80
        jsr     intbasic::MON_COUT
        ldx     ENTRY_BUFFER + $13
        lda     ENTRY_BUFFER + $14
        jsr     intbasic::PRDEC
        jsr     intbasic::MON_CROUT

        jmp     next_file

close:  MLI_CALL CLOSE, close_params
        jsr     SwapZP          ; ProDOS > IntBASIC
        clc
        rts

err:
        jsr     SwapZP          ; ProDOS > IntBASIC
        jmp     ShowError

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
        ;; Pad with spaces
        lda     #' '|$80
:       jsr     intbasic::MON_COUT
        inx
        cpx     #16
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
        .byte   FT_BAS
        scrcode "BAS"
        .byte   FT_SYS
        scrcode "SYS"
        .byte   0               ; sentinel
.endproc ; print_entry_type

file_count:
        .word   0
entries_per_block:
        .byte   0
entries_this_block:
        .byte   0

.endproc ; CatCmd

;;; ============================================================
;;; "DELETE pathname"

.proc DeleteCmd
        jsr     SwapZP          ; IntBASIC > ProDOS
        MLI_CALL DESTROY, destroy_params
        jsr     SwapZP          ; ProDOS > IntBASIC
        jmp     FinishCommand
.endproc ; DeleteCmd

;;; ============================================================
;;; "RENAME pathname,pathname2"

.proc RenameCmd
        jsr     SwapZP          ; IntBASIC > ProDOS
        MLI_CALL RENAME, rename_params
        jsr     SwapZP          ; ProDOS > IntBASIC
        jmp     FinishCommand
.endproc ; RenameCmd

;;; ============================================================
;;; "BSAVE pathname,A<address>,L<length>"

.proc BSaveCmd
        ;; Validate args
        lda     seen_args
        and     #ParseFlags::address | ParseFlags::length
        cmp     #ParseFlags::address | ParseFlags::length
        bne     syn
        lda     arg_len
        ora     arg_len+1
        beq     syn

        ;; Set file type, aux type, data address and length
        lda     #FT_BIN
        sta     create_file_type
        LDXY    arg_addr
        STXY    create_aux_type
        STXY    write_data_buffer
        LDXY    arg_len
        STXY    write_request_count

        jmp     WriteFileCommon

syn:    sec
        rts
.endproc ; BSaveCmd

;;; ============================================================
;;; "BLOAD pathname[,A<address>]"

.proc BLoadCmd
        jsr     LoadBINFile
        jmp     FinishCommand

.endproc ; BLoadCmd

.proc LoadBINFile
        jsr     SwapZP          ; IntBASIC > ProDOS

        ;; Check type, bail if not BIN
        MLI_CALL GET_FILE_INFO, gfi_params
        bne     finish
        lda     gfi_file_type
        cmp     #FT_BIN
        beq     :+
        lda     #ERR_INCOMPATIBLE_FILE_FORMAT
        bne     finish          ; always
:
        LDXY    gfi_aux_type    ; default load address
        lda     seen_args
        and     #ParseFlags::address
        beq     :+
        LDXY    arg_addr        ; arg override
:       STXY    read_data_buffer

        LDXY    #$FFFF          ; read everything
        STXY    read_request_count

        MLI_CALL OPEN, open_params
        bne     finish
        lda     open_ref_num
        sta     read_ref_num
        sta     close_ref_num
        MLI_CALL READ, read_params
        pha
        MLI_CALL CLOSE, close_params
        pla

finish:
        jmp     SwapZP          ; ProDOS > IntBASIC
.endproc ; LoadBINFile

;;; ============================================================
;;; "BRUN pathname[,A<address>]"

.proc BRunCmd
        jsr     LoadBINFile
        bne     :+
        jsr     run
        lda     #0
:       jmp     FinishCommand

run:    jmp     (read_data_buffer)
.endproc ; BRunCmd

;;; ============================================================
;;; "MON" and "NOMON"

.proc MonCmd
        clc
        rts
.endproc ; MonCmd

;;; ============================================================
;;; "PR#<slot>"

.proc PRCmd
        lda     slotnum
        jsr     intbasic::MON_OUTPORT
        .assert * = HookCSW, error, "fall through"
.endproc ; PRCmd

;;; ============================================================

;;; Redirect CSW to `CSWHook`
;;; Output: CSW will be hooked (if not already), C=0
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
        clc
        rts
.endproc ; HookCSW

;;; ============================================================

.proc WriteFileCommon
        jsr     SwapZP          ; IntBASIC > ProDOS

        ;; If it exists, is it the desired type?
        MLI_CALL GET_FILE_INFO, gfi_params
        beq     check           ; exists, check type
        cmp     #ERR_FILE_NOT_FOUND
        beq     create          ; doesn't exist, create it
        bne     finish          ; other error

check:  lda     gfi_file_type   ; check type
        cmp     create_file_type
        beq     delete          ; okay to overwrite
        lda     #ERR_INCOMPATIBLE_FILE_FORMAT
        bne     finish          ; always

        ;; Delete so size/addr/etc updated
delete: MLI_CALL DESTROY, destroy_params
        bne     finish

        ;; Create the file
create:
        ldx     #3              ; set creation date
:       lda     DATELO,x
        sta     create_date,x
        dex
        bpl     :-

        MLI_CALL CREATE, create_params

        ;; Write the file
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
        jsr     SwapZP          ; ProDOS > IntBASIC
        .assert * = FinishCommand, error, "fall through"
.endproc ; WriteFileCommon

;;; ============================================================
;;; Jump to at the end of a command; if Z=0 ShowError, else
;;; returns with C=0

.proc FinishCommand
        bne     ShowError
        clc
        rts
.endproc ; FinishCommand

;;; ============================================================
;;; Show ProDOS error message / number

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
        jsr     intbasic::MON_CROUT
        jmp     intbasic::ERRMESS+3

message:
        .byte   $87             ; BELL
        scrcode "*** PRODOS ERR $"
        .byte   0
.endproc ; ShowError

;;; ============================================================

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
        cmp     #ExecResult::executed
        bne     :+
        lda     #$8D
        ldx     saved_x
        rts
:
        ldy     #<intbasic::ErrMsg02 ;"SYNTAX"
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

.endproc ; reloc
        sizeof_reloc = .sizeof(reloc)

        ;; Exports
        reloc__Initialize := reloc::Initialize
        reloc__QuitCmd := reloc::QuitCmd
        reloc__CommandHook := reloc::CommandHook
        reloc__HookCSW := reloc::HookCSW
        intbasic__GETCMD := reloc::intbasic::GETCMD
        intbasic__WARM := reloc::intbasic::WARM
        intbasic__ERRMESS := reloc::intbasic::ERRMESS

        .assert * <= MLI, error, "collision"
        .out .sprintf("MEM: $%04X end of Command Handler", *)
        .out .sprintf("MEM: $%04X bytes remaining before $BF00", $BF00 - *)
