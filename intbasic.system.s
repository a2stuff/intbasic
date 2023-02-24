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
;;;          | Init &    |
;;;          | Command   |
;;;          | Processor |
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

        IO_BUFFER := $BB00
        PATHBUF := $280

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
        beq     done_copy
:       lda     path,x
        sta     PATHBUF,x
        dex
        bpl     :-
done_copy:

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
        lda     #%11100001
        sta     BITMAP+$B*2+1   ; Pages $B8-$BA, ProDOS global page ($BF)

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

.proc Initialize

        ;; Make LOAD/SAVE just QUIT to ProDOS
        lda     #OPC_JMP_abs
        LDXY    #Quit
        sta     intbasic::LOAD+0
        STXY    intbasic::LOAD+1
        sta     intbasic::SAVE+0
        STXY    intbasic::SAVE+1

        ;; Hook the command parser
        LDXY    #CommandHook
        STXY    intbasic::GETCMD+3

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
        bne     Quit            ; fail - just QUIT back to ProDOS

        ;; Hold Open- or Solid-Apple to allow returning to prompt
        lda     BUTN0
        ora     BUTN1
        bmi     :+

        ;; When END or ERRMESS invoked, just QUIT
        lda     #OPC_JMP_abs
        LDXY    #Quit
        sta     intbasic::WARM
        STXY    intbasic::WARM+1
        sta     intbasic::ERRMESS
        STXY    intbasic::ERRMESS+1       ; patches JSR PRINTERR
:

        ;; Run the program
        jmp     intbasic::RUNWARM
.endproc

;;; ============================================================

.proc ColdStart
        jsr     intbasic::COLD
        LDXY    #intbasic::BASIC
        STXY    intbasic::HIMEM
        jmp     intbasic::NEW   ; reset PP, PV, stacks
.endproc

;;; ============================================================

.proc Quit
        MLI_CALL QUIT, quit_params
        brk
.endproc

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
.endproc

;;; ============================================================

;;; Input: Path to load in `PATHBUF`
;;; Output: ProDOS error code in A ($00 = success)
;;; Assert: IntBASIC ZP is swapped in

.proc SaveINTFile
        ;; Prepare the parameters
        ldx     #3
:       lda     DATELO,x
        sta     create_date,x
        dex
        bpl     :-

        LDXY    intbasic::PP
        STXY    write_data_buffer

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
        cmp     #ERR_FILE_NOT_FOUND
        beq     create
        bne     finish          ; other error
check:  lda     gfi_file_type   ; check type
        cmp     #FT_INT
        beq     write           ; okay to overwrite
        lda     #ERR_INCOMPATIBLE_FILE_FORMAT
        bne     finish          ; always

        ;; Create the file
create:
        MLI_CALL CREATE, create_params
        beq     write           ; success
        cmp     #ERR_DUPLICATE_FILENAME
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
        jmp     SwapZP          ; ProDOS > IntBASIC
.endproc

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
create_file_type:       .byte   $FA     ; in INT
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
rename_pathname:       .addr   PATHBUF ; in
rename_new_pathname:   .addr   0       ; in, populated at runtime

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
        sta     @disp
        lda     cmdprochi,x
        sta     @disp+1
        @disp := *+1
        jsr     $0000           ; self-modified

        ;; If it returns with C=0, pass empty command line back
        bcs     :+
        lda     #$8D
        ldx     #0
        sta     intbasic::IN,x
        rts
:
        ;; Force a syntax error
        lda     #'!'|$80
        sta     intbasic::IN
        lda     #$8D
        sta     intbasic::IN+1
        ldx     #1
        rts

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
        .byte   0               ; sentinel

cmdproclo:
        .byte   <ByeCmd,<SaveCmd,<LoadCmd,<RunCmd,<PrefixCmd,<CatCmd,<CatCmd,<DeleteCmd,<RenameCmd
cmdprochi:
        .byte   >ByeCmd,>SaveCmd,>LoadCmd,>RunCmd,>PrefixCmd,>CatCmd,>CatCmd,>DeleteCmd,>RenameCmd

;;; ============================================================
;;; Commands should return with:
;;;   C=0 if command consumed (successful or otherwise)
;;;   C=1 on syntax error (e.g. no filename given)
;;; Commands are run with IntBASIC ZP swapped in
;;; ============================================================

ByeCmd := Quit

;;; ============================================================
;;; "SAVE pathname"

.proc SaveCmd
        jsr     GetPathname
        lda     PATHBUF
        bne     :+
        sec                     ; syntax error
        rts
:
        jsr     SaveINTFile
        bne     err
        clc
        rts

err:    jmp     ShowError
.endproc

;;; ============================================================
;;; "LOAD pathname"

.proc LoadCmd
        jsr     GetPathname
        lda     PATHBUF
        bne     :+
        sec                     ; syntax error
        rts
:
        ;; Pop out of command hook - no going back now
        pla
        pla

        jsr     ColdStart
        jsr     LoadINTFile
        bne     err
        jmp     intbasic::WARM

err:
        jsr     ShowError
        jsr     ColdStart
        jmp     intbasic::WARM
.endproc

;;; ============================================================
;;; "RUN" or "RUN pathname"

.proc RunCmd
        ;; Pop out of command hook - no going back now
        pla
        pla

        jsr     GetPathname
        lda     PATHBUF
        bne     :+
        jmp     intbasic::RUNWARM
:
        jsr     ColdStart
        jsr     LoadINTFile
        bne     LoadCmd::err
        jmp     intbasic::RUNWARM
.endproc

;;; ============================================================
;;; "PREFIX" or "PREFIX pathname"

.proc PrefixCmd
        jsr     GetPathname
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
        bne     :-
:       clc
        rts

        ;; Set prefix
set:
        jsr     SwapZP          ; IntBASIC > ProDOS
        MLI_CALL SET_PREFIX, prefix_params
        jsr     SwapZP          ; ProDOS > IntBASIC
        bne     err
        clc
        rts

err:    jmp     ShowError
.endproc

;;; ============================================================
;;; "CAT" or "CAT path"

.proc CatCmd
        jsr     GetPathname

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
.endproc

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
.endproc

file_count:
        .word   0
entries_per_block:
        .byte   0
entries_this_block:
        .byte   0

.endproc

;;; ============================================================
;;; "DELETE pathname"

.proc DeleteCmd
        jsr     GetPathname
        lda     PATHBUF
        bne     :+
        sec                     ; syntax error
        rts
:

        jsr     SwapZP          ; IntBASIC > ProDOS
        MLI_CALL DESTROY, destroy_params
        jsr     SwapZP          ; ProDOS > IntBASIC
        bne     ShowError
        clc
        rts
.endproc

;;; ============================================================
;;; "RENAME pathname,pathname2"

.proc RenameCmd
        jsr     GetPathname
        lda     PATHBUF
        bne     :+
        sec                     ; syntax error
        rts
:
        ;; Find ','
        tax
        ldy     #0
:       lda     PATHBUF,x
        cmp     #','
        beq     found
        iny
        dex
        bpl     :-
        sec                     ; syntax error
        rts

found:
        ;; Length of second string
        tya
        sta     PATHBUF,x
        iny
        sty     @sub

        ;; Length of first string
        sec
        lda     PATHBUF
        @sub = *+1
        sbc     #$00            ; self-modified
        sta     PATHBUF

        clc
        lda     PATHBUF
        adc     #<(PATHBUF+1)
        sta     rename_new_pathname
        lda     #0
        adc     #>(PATHBUF+1)
        sta     rename_new_pathname+1

        jsr     SwapZP          ; IntBASIC > ProDOS
        MLI_CALL RENAME, rename_params
        jsr     SwapZP          ; ProDOS > IntBASIC
        bne     ShowError
        clc
        rts
.endproc

;;; ============================================================
;;; Populate `PATHBUF` with rest of command line (skipping spaces)

;;; Input: Y = end of command in `intbasic::IN`
;;; Output: `PATHBUF` is length-prefixed path
.proc GetPathname
        ldx     #0
loop:   lda     intbasic::IN,y
        cmp     #$8D            ; CR
        beq     done
        cmp     #$A0            ; space
        beq     skip
        and     #$7F
        sta     PATHBUF+1,x
        inx
skip:   iny
        bne     loop            ; always

done:   stx     PATHBUF
        rts
.endproc ; GetPathname

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
        clc
        rts

message:
        .byte   $87             ; BELL
        scrcode "*** PRODOS ERR $"
        .byte   0
.endproc ; ShowError

.endproc ; CommandHook

;;; ============================================================

.endproc ; reloc
        sizeof_reloc = .sizeof(reloc)
        .assert * <= IO_BUFFER, error, "collision"
        Initialize := reloc::Initialize
