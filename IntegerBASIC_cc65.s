; Target assembler: cc65 v2.18.0 [--target none -C IntegerBASIC_cc65.cfg]
;*******************************************************************************
;* Integer BASIC for the Apple II                                              *
;* By Steve Wozniak                                                            *
;* Copyright 1977 by Apple Computer, Inc.                                      *
;*******************************************************************************
;* Disassembly by Paul R. Santa-Maria.                                         *
;*                                                                             *
;* "Ported" to SourceGen format by Andy McFadden, using 6502bench SourceGen    *
;* v1.8.  The listing here generally follows the original, except for a few    *
;* places where SourceGen's limited expressions make it awkward.  I added a    *
;* few additional labels, mostly on loops.                                     *
;*                                                                             *
;* Last updated 2021/10/17                                                     *
;*******************************************************************************
;* The binary was extracted from the INTBASIC file on the DOS 3.3 system       *
;* master, for the region spanning $e000-f7ff.  That file also includes PA#1   *
;* from $d000-dfff, and the original F8 monitor ROM from $f800-ffff.           *
;*                                                                             *
;* As noted in Paul R. Santa-Maria's article, there is no original assembly    *
;* file for Integer BASIC.  The entire program was written out by hand.        *
;*******************************************************************************
        .setcpu "6502"
ETX     =       $03     ;Ctrl+C
CR      =       $0d     ;carriage return
BLANK   =       $20     ;space
DQT     =       $22     ;double quote

MON_WNDWDTH =   $21     ;width of scroll window
MON_CH  =       $24     ;cursor horizontal displacement
MON_CV  =       $25     ;cursor vertical displacement
MON_GBASL =     $26     ;base address for lo-res drawing (lo)
MON_H2  =       $2c     ;right end of horizontal line drawn by HLINE
MON_V2  =       $2d     ;bottom of vertical line drawn by VLINE
MON_PROMPT =    $33     ;prompt character, used by GETLN
MON_A1L =       $3c     ;general purpose
MON_A1H =       $3d     ;general purpose
MON_A2L =       $3e     ;general purpose
MON_A2H =       $3f     ;general purpose
LOMEM   =       $4a     ;pointer to start of variables
HIMEM   =       $4c     ;pointer to end of program
MON_RNDL =      $4e     ;low byte of KEYIN "random" value
MON_RNDH =      $4f     ;high byte of KEYIN "random" value
NOUNSTKL =      $50     ;noun stack low bytes
SYNSTKH =       $58     ;syntax stack high byte
NOUNSTKH =      $78     ;noun stack high bytes
SYNSTKL =       $80     ;syntax stack low bytes
NOUNSTKC =      $a0     ;noun stack counter
TXTNDXSTK =     $a8     ;text index stack
TXTNDX  =       $c8     ;text index val (OUTVAL)
LEADBL  =       $c9     ;leading blanks index (YTEMP)
PP      =       $ca     ;pointer to start of program
PV      =       $cc     ;pointer to end of vars
ACC     =       $ce     ;main accumulator
SRCH    =       $d0     ;pointer to search var table
TOKNDXSTK =     $d1     ;token index stack
SRCH2   =       $d2     ;second var search pointer
IFFLAG  =       $d4     ;IF/THEN fail flag
CRFLAG  =       $d5     ;carriage return flag
VERBNOW =       $d6     ;verb currently in use
PRFLAG  =       $d7     ;print it now flag
XSAVE   =       $d8     ;temp X-reg save
RUNFLAG =       $d9     ;run mode flag
AUX     =       $da     ;aux counter
PR      =       $dc     ;current line value
PX      =       $e0     ;pointer to current verb
P1      =       $e2     ;aux pointer 1 (delete line ptr)
P2      =       $e4     ;aux pointer 2 (line num , next num, ...)
P3      =       $e6     ;aux pointer 3 (next ptr)
TOKNDX  =       $f1     ;token index val
PCON    =       $f2     ;continue pointer (PRDEC low/hi)
AUTOINC =       $f4     ;auto line increment
AUTOLN  =       $f6     ;current auto line
AUTOFLAG =      $f8     ;auto line mode flag ($ff=on)
CHAR    =       $f9     ;current char
LEADZR  =       $fa     ;leading zeroes index ($00,$a0,$b0)
FORNDX  =       $fb     ;FOR-NEXT loop index
GOSUBNDX =      $fc     ;GOSUB index
SYNSTKDX =      $fd     ;syntax stack index val
SYNPAG  =       $fe     ;pointer to syntax page
STK_00  =       $0100
STK_10  =       $0110
STK_20  =       $0120
STK_30  =       $0130
STK_40  =       $0140
STK_50  =       $0150
STK_60  =       $0160
STK_70  =       $0170
STK_80  =       $0180
STK_90  =       $0190
STK_A0  =       $01a0
STK_B0  =       $01b0
STK_C0  =       $01c0
STK_D0  =       $01d0
IN      =       $0200   ;input buffer
KBD     =       $c000   ;R last key pressed + 128
KBDSTRB =       $c010   ;RW keyboard strobe
MON_PLOT =      $f800   ;lo-res plot at X=Y-reg, Y=Acc
MON_HLINE =     $f819   ;lo-res horiz line at Y=Acc with X from $2c
MON_VLINE =     $f828   ;lo-res vert line at X=Y-reg and Y from Acc to $2b
MON_GBASCALC =  $f847   ;compute gfx base addr for line in Acc
MON_SETCOL =    $f864   ;set lo-res color to Acc
MON_PREAD =     $fb1e   ;read paddle specifed by X-reg, return in Y-reg
MON_SETTXT =    $fb39   ;set screen to text mode
MON_SETGR =     $fb40   ;set screen to graphics mode
MON_VTAB =      $fc22   ;tab to row specified in CV
MON_NXTCHAR =   $fd75
MON_CROUT =     $fd8e   ;print a carriage return
MON_COUT =      $fded   ;print Acc to output device via $36-37
MON_INPORT =    $fe8b   ;set char input handler to slot in A-reg
MON_OUTPORT =   $fe95   ;set char output handler to slot in A-reg
MON_WRITE =     $fecd   ;write data to cassette
MON_WRITE0 =    $fecf
MON_READ =      $fefd   ;read data from cassette
MON_BELL =      $ff3a   ;sound bell

        .org    $e000
BASIC:  jsr     COLD
BASIC2: jmp     WARM

SetPrompt:
        sta     MON_PROMPT
        jmp     MON_COUT

        .byte   $60

LE00C:  txa             ;print a trailing blank?
        and     #$20
        beq     Return
LE011:  lda     #BLANK+128
        sta     P2
        jmp     MON_COUT

LE018:  lda     #32
LE01A:  cmp     MON_CH  ;check line length
        bcs     NextByte ;line too short
        lda     #CR+128 ;print CR, then 7 blanks
        ldy     #$07
@Loop:  jsr     MON_COUT
        lda     #BLANK+128
        dey
        bne     @Loop
; 
NextByte:
        ldy     #$00    ;get next byte 16-bit ptr
        lda     (P1),y
        inc     P1
        bne     Return
        inc     P1+1
Return: rts

; 
; Token $75 , (with token $74 LIST)
;  LIST 5,30
; 
COMMA_LIST:
        jsr     GET16BIT
        jsr     LE576
LE03B:  lda     P1
        cmp     P3
        lda     P1+1
        sbc     P3+1
        bcs     Return
        jsr     UNPACK
        jmp     LE03B

; 
; Token $76 LIST
;  list entire program
; 
LIST:   lda     PP      ;P1 = PP
        sta     P1
        lda     PP+1
        sta     P1+1
        lda     HIMEM   ;P3 = HIMEM
        sta     P3
        lda     HIMEM+1
        sta     P3+1
        bne     LE03B   ;(always)
; 
; Token $74 LIST
;  specific line number or range of numbers
;  LIST 10: LIST 5,30
; 
LISTNUM:
        jsr     GET16BIT
        jsr     LE56D
        lda     P2      ;P1 = P2
        sta     P1
        lda     P2+1
        sta     P1+1
        bcs     Return
; Unpack tokens to mnemonics.
UNPACK: stx     XSAVE
        lda     #BLANK+128
        sta     LEADZR
        jsr     NextByte
        tya
@LE077: sta     P2
        jsr     NextByte
        tax
        jsr     NextByte
        jsr     PRDEC
@Loop:  jsr     LE018
        sty     LEADZR
        tax
        bpl     @LE0A3
        asl     A
        bpl     @LE077
        lda     P2
        bne     @NotEq
        jsr     LE011
@NotEq: txa
@Loop1: jsr     MON_COUT
@LE099: lda     #$25
        jsr     LE01A
        tax
        bmi     @Loop1
        sta     P2
@LE0A3: cmp     #$01
        bne     @NotEq1
        ldx     XSAVE
        jmp     MON_CROUT

@NotEq1:
        pha
        sty     ACC
        ldx     #>SYNTABL2
        stx     ACC+1
        cmp     #$51    ;END token
        bcc     @Loop11
        dec     ACC+1   ;in SYNTABL
        sbc     #$50    ;TAB tkn
@Loop11:
        pha
        lda     (ACC),y
@Loop2: tax
        dey
        lda     (ACC),y
        bpl     @Loop2
        cpx     #$c0
        bcs     @LE0CC
        cpx     #$00
        bmi     @Loop2
@LE0CC: tax
        pla
        sbc     #$01    ;carry is set
        bne     @Loop11
        bit     P2
        bmi     @Loop3
        jsr     LEFF8
@Loop3: lda     (ACC),y
        bpl     @LE0ED
        tax
        and     #$3f
        sta     P2
        clc
        adc     #BLANK+128
        jsr     MON_COUT
        dey
        cpx     #$c0
        bcc     @Loop3
@LE0ED: jsr     LE00C
        pla
        cmp     #']'
        beq     @LE099
        cmp     #'('
        bne     @Loop
        beq     @LE099  ;(always)

; 
; Token $2a (
;   substring
;   PRINT A$(12,14)
; 
PAREN_SUBSTR:
        jsr     LE118
        sta     NOUNSTKL,x
        cmp     NOUNSTKH,x
LE102:  bcc     LE115
PrintErr05:
        ldy     #<ErrMsg05 ;"STRING"
LE106:  jmp     ERRMESS

; 
; Token $23 ,
;   substring
;   PRINT A$(3,3)
; 
COMMA_SUBSTR:
        jsr     GETBYTE
        cmp     NOUNSTKL,x
        bcc     PrintErr05 ;"STRING"
        jsr     LEFE4
        sta     NOUNSTKH,x
LE115:  jmp     TE823

LE118:  jsr     GETBYTE
        beq     PrintErr05 ;"STRING"
        sec
        sbc     #$01
        rts

; 
; Token $42 (
;   string array is destination of the data
;   A$(1)="HELLO"
; 
TE121:  jsr     LE118
        sta     NOUNSTKL,x
        clc
        sbc     NOUNSTKH,x
        jmp     LE102

PrintErr03:
        ldy     #<ErrMsg03 ;"MEM FULL"
        bne     LE106   ;(always)

; 
; Token $43 ,
;   next var in DIM statement is string
;   DIM X(5),A$(5)
; 
; token $4e DIM
;   string var uses token $22 (
;   DIM A$(5)
; 
DIMSTR: jsr     LE118
        inx
LE134:  lda     NOUNSTKL,x
        sta     AUX
        adc     ACC
        pha
        tay
        lda     NOUNSTKH,x
        sta     AUX+1
        adc     ACC+1
        pha
        cpy     PP
        sbc     PP+1
        bcs     PrintErr03 ;"MEM FULL"
        lda     AUX     ;AUX = AUX - 2
        adc     #$fe
        sta     AUX
        lda     #$ff
        tay
        adc     AUX+1
        sta     AUX+1
@Loop1: iny
        lda     (AUX),y
        cmp     PV,y
        bne     DimErr
        tya
        beq     @Loop1
@Loop2: pla
        sta     (AUX),y
        sta     PV,y
        dey
        bpl     @Loop2
        inx
        rts

        .byte   $ea

DimErr: ldy     #<ErrMsg17 ;"DIM"
LE16F:  bne     LE106   ;(always)

; 
; Input a string.
; 
INPUTSTR:
        lda     #$00
        jsr     LE70A
        ldy     #$02
        sty     NOUNSTKH,x
        jsr     LE70A
        stx     XSAVE
        tax
        inc     MON_PROMPT ;change '>' to '?'
        jsr     RDKEY
        dec     MON_PROMPT ;change '?' to '>'
        txa
        ldx     XSAVE
        sta     NOUNSTKH,x
; 
; Token $70 =
;   string - non-conditional
;   A$ = "HELLO"
; 
TE18C:  lda     NOUNSTKL+1,x
        sta     ACC
        lda     NOUNSTKH+1,x
        sta     ACC+1
        inx
        inx
        jsr     @LE1BC
@Loop:  lda     NOUNSTKL-2,x
        cmp     NOUNSTKH-2,x
        bcs     @LE1B4
        inc     NOUNSTKL-2,x
        tay
        lda     (ACC),y
        ldy     NOUNSTKL,x
        cpy     P2
        bcc     @LE1AE
        ldy     #<ErrMsg18 ;"STR OVFL"
        bne     LE16F   ;(always)

@LE1AE: sta     (AUX),y
        inc     NOUNSTKL,x
        bcc     @Loop

@LE1B4: ldy     NOUNSTKL,x
        txa
        sta     (AUX),y
        jmp     LF223

@LE1BC: lda     NOUNSTKL+1,x
        sta     AUX
        sec
        sbc     #$02
        sta     P2
        lda     NOUNSTKH+1,x
        sta     AUX+1
        sbc     #$00
        sta     P2+1
        ldy     #$00
        lda     (P2),y
        clc
        sbc     AUX
        sta     P2
        rts

; 
; Token $39 =
;   string logic op
;   IF A$ = "CAT" THEN END
; 
TE1D7:  lda     NOUNSTKL+3,x
        sta     ACC
        lda     NOUNSTKH+3,x
        sta     ACC+1
        lda     NOUNSTKL+1,x
        sta     AUX
        lda     NOUNSTKH+1,x
        sta     AUX+1
        inx
        inx
        inx
        ldy     #$00
        sty     NOUNSTKH,x
        sty     NOUNSTKC,x
        iny
        sty     NOUNSTKL,x
@LE1F3: lda     HIMEM+1,x
        cmp     NOUNSTKH-3,x
        php
        pha
        lda     NOUNSTKL-1,x
        cmp     NOUNSTKH-1,x
        bcc     @LE206
        pla
        plp
        bcs     @Return
@LoopExit:
        lsr     NOUNSTKL,x
@Return:
        rts

@LE206: tay
        lda     (ACC),y
        sta     P2
        pla
        tay
        plp
        bcs     @LoopExit
        lda     (AUX),y
        cmp     P2
        bne     @LoopExit
        inc     NOUNSTKL-1,x
        inc     HIMEM+1,x
        bcs     @LE1F3  ;(always)
; 
; Token $3a #
;   string logic op
;   IF A$ # "CAT" THEN END
; 
TE21C:  jsr     TE1D7
        jmp     NOT

; 
; Token $14 *
;   num math op
;   A = 27 * 2
; 
MULT:   jsr     LE254
@Loop:  asl     ACC
        rol     ACC+1   ;add partial product if C flag set
        bcc     @LE238
        clc
        lda     P3
        adc     AUX
        sta     P3
        lda     P3+1
        adc     AUX+1
        sta     P3+1
@LE238: dey
        beq     MultEnd ;exit loop
        asl     P3
        rol     P3+1
        bpl     @Loop
        jmp     Err32767

MultEnd:
        lda     P3
        jsr     LE708
        lda     P3+1
        sta     NOUNSTKC,x
        asl     P2+1
        bcc     Return1
        jmp     NEGATE

LE254:  lda     #$55
        sta     P2+1
        jsr     @LE25B
@LE25B: lda     ACC     ;AUX = ACC
        sta     AUX
        lda     ACC+1
        sta     AUX+1
        jsr     GET16BIT
        sty     P3
        sty     P3+1
        lda     ACC+1
        bpl     @LE277
        dex
        asl     P2+1
        jsr     NEGATE
        jsr     GET16BIT
@LE277: ldy     #$10
Return1:
        rts

; 
; Token $1f MOD
;   num op
;   IF X MOD 13 THEN END
; 
MOD:    jsr     LEE6C
        beq     MultEnd ;(always)
        .byte   $ff

LE280:  inc     MON_PROMPT ;change '>' to '?'
        ldy     #$00
        jsr     GETCMD
        dec     MON_PROMPT ;change '?' to '>'
        rts

; 
; Token $3d SCRN(
;   PRINT SCRN(X,Y)
; 
SCRN:   jsr     GETBYTE
        lsr     A       ;A-reg = A-reg / 2
        php             ;stash carry (lsb)
        jsr     MON_GBASCALC
        jsr     GETBYTE
        tay
        lda     (MON_GBASL),y ;get screen byte
        plp             ;retrieve carry
        bcc     @LE29F
        lsr     A       ;odd; upper half
        lsr     A
        lsr     A
        lsr     A
@LE29F: and     #$0f    ;A-reg = color number
        ldy     #$00
        jsr     LE708
        sty     NOUNSTKC,x
        dey
        sty     PRFLAG  ;PRFLAG = $ff
; 
; Token $3e ,
;   PRINT SCRN(X,Y)
; 
COMMA_SCRN:
        rts

        .byte   $ff,$ff,$ff,$ff

unref_e2b0:
        jsr     LEFD3   ;old 4K cold start
; 
; Warm start.  Main compile / execute code.
; 
WARM:   jsr     MON_CROUT ;emit blank line
LE2B6:  lsr     RUNFLAG ;not running
        lda     #'>' | $80
        jsr     SetPrompt ;set and print prompt char
        ldy     #$00
        sty     LEADZR  ;no leading zeroes for AUTOLN
        bit     AUTOFLAG ;AUTO?
        bpl     @LE2D1
        ldx     AUTOLN  ;yes, print line number
        lda     AUTOLN+1
        jsr     PRDEC
        lda     #BLANK+128
        jsr     MON_COUT
@LE2D1: ldx     #$ff    ;init S-reg
        txs
        jsr     GETCMD
        sty     TOKNDX
        txa
        sta     TXTNDX
        ldx     #$20
        jsr     LE491
        lda     TXTNDX  ;PX = TXTNDX + $0200 + C-flag
        adc     #$00
        sta     PX
        lda     #$00
        tax
        adc     #$02
        sta     PX+1
        lda     (PX,x)
        and     #$f0
        cmp     #'0' | $80
        beq     @LE2F9
        jmp     LE883

@LE2F9: ldy     #$02    ;move two bytes
@LE2FB: lda     (PX),y
        sta     ACC-1,y
        dey
        bne     @LE2FB
        jsr     LE38A
        lda     TOKNDX
        sbc     TXTNDX
        cmp     #$04
        beq     LE2B6
        sta     (PX),y
        lda     PP      ;P2 = PP - (PX),Y
        sbc     (PX),y
        sta     P2
        lda     PP+1
        sbc     #$00
        sta     P2+1
        lda     P2
        cmp     PV
        lda     P2+1
        sbc     PV+1
        bcc     MEMFULL
@LE326: lda     PP      ;P3 = PP - (PX),Y
        sbc     (PX),y
        sta     P3
        lda     PP+1
        sbc     #$00
        sta     P3+1
        lda     (PP),y
        sta     (P3),y
        inc     PP
        bne     @LE33C
        inc     PP+1
@LE33C: lda     P1
        cmp     PP
        lda     P1+1
        sbc     PP+1
        bcs     @LE326
@LE346: lda     P2,x
        sta     PP,x
        dex
        bpl     @LE346
        lda     (PX),y
        tay
@LE350: dey
        lda     (PX),y
        sta     (P3),y
        tya
        bne     @LE350
        bit     AUTOFLAG ;auto line?
        bpl     @LE365  ;no, branch
@Loop:  lda     AUTOLN+1,x ;AUTOLN = AUTOLN + AUTOINC
        adc     AUTOINC+1,x
        sta     AUTOLN+1,x
        inx
        beq     @Loop
@LE365: bpl     LE3E5
        brk

        .byte   $00,$00,$00

MEMFULL:
        ldy     #<ErrMsg03 ;"MEM FULL"
        bne     ERRMESS ;(always)

; 
; Token $0a ,
;   DEL 0,10
; 
COMMA_DEL:
        jsr     GET16BIT
        lda     P1      ;P3 = P1
        sta     P3
        lda     P1+1
        sta     P3+1
        jsr     LE575
        lda     P1      ;P2 = P1
        sta     P2
        lda     P1+1
        sta     P2+1
        bne     LE395   ;(always?)
; 
; Token $09 DEL
; 
DEL:    jsr     GET16BIT
LE38A:  jsr     LE56D
        lda     P3      ;P1 = P3
        sta     P1
        lda     P3+1
        sta     P1+1
; Memory move: P3 < PP.P2 backwards.
LE395:  ldy     #$00
@Loop:  lda     PP
        cmp     P2
        lda     PP+1
        sbc     P2+1
        bcs     @LE3B7
        lda     P2
        bne     @LE3A7
        dec     P2+1
@LE3A7: dec     P2
        lda     P3
        bne     @LE3AF
        dec     P3+1
@LE3AF: dec     P3
        lda     (P2),y
        sta     (P3),y
        bcc     @Loop

@LE3B7: lda     P3      ;PP = P3
        sta     PP
        lda     P3+1
        sta     PP+1
        rts

Loop:   jsr     MON_COUT ;print error message
        iny
; 
; Print error message routine entry point.
; 
ERRORMESS:
        lda     ErrMsg00,y
        bmi     Loop
        ora     #$80
        jmp     MON_COUT

GETCMD: tya
        tax
        jsr     MON_NXTCHAR
        txa
        tay
        lda     #'_' | $80 ;underline problem?
        sta     IN,y
        ldx     #$ff
        rts

        .byte   $60

ErrTooLong:
        ldy     #<ErrMsg01 ;"TOO LONG"
; 
; Print error message and goto mainline.
; 
ERRMESS:
        jsr     PRINTERR
; DOS 3.3 chains here when processing errors.
        bit     RUNFLAG
LE3E5:  bmi     @LE3EA
        jmp     LE2B6

@LE3EA: jmp     LEB9A

LE3ED:  rol     A
        adc     #$a0
        cmp     IN,x
        bne     LE448
        lda     (SYNPAG),y
        asl     A
        bmi     @LE400
        dey
        lda     (SYNPAG),y
        bmi     LE428
        iny
@LE400: stx     TXTNDX
        tya
        pha
        ldx     #$00
        lda     (SYNPAG,x)
        tax
@Loop:  lsr     A
        eor     #$40
        ora     (SYNPAG),y
        cmp     #$c0
        bcc     @LE413
        inx
@LE413: iny
        bne     @Loop
        pla
        tay
        txa
        jmp     LF2F8

LE41C:  inc     TOKNDX
        ldx     TOKNDX
        beq     ErrTooLong
        sta     IN,x
Return2:
        rts

LE426:  ldx     TXTNDX
LE428:  lda     #BLANK+128
@LE42A: inx
        cmp     IN,x
        bcs     @LE42A
        lda     (SYNPAG),y
        and     #$3f
        lsr     A
        bne     LE3ED
        lda     IN,x
        bcs     @LE442
        adc     #$3f
        cmp     #$1a
        bcc     LE4B1
@LE442: adc     #$4f
        cmp     #$0a
        bcc     LE4B1
LE448:  ldx     SYNSTKDX
@Loop1: iny
        lda     (SYNPAG),y
        and     #$e0
        cmp     #$20
        beq     LE4CD
        lda     TXTNDXSTK,x
        sta     TXTNDX
        lda     TOKNDXSTK,x
        sta     TOKNDX
@Loop2: dey
        lda     (SYNPAG),y
        asl     A       ;double
        bpl     @Loop2
        dey
        bcs     LE49C
        asl     A
        bmi     LE49C
        ldy     SYNSTKH,x
        sty     SYNPAG+1
        ldy     SYNSTKL,x
        inx
        bpl     @Loop1
LE470:  beq     Return2
        cmp     #$7e
        bcs     LE498
        dex
        bpl     @LE47D
        ldy     #<ErrMsg01 ;"TOO LONG"
; BUG: above line should be ErrMsg04, "TOO MANY PARENS".  See Call-A.P.P.L.E.
; Mar 1983 p.114.
        bpl     LE4A6   ;(always)

@LE47D: sty     SYNSTKL,x
        ldy     SYNPAG+1
        sty     SYNSTKH,x
        ldy     TXTNDX
        sty     TXTNDXSTK,x
        ldy     TOKNDX
        sty     TOKNDXSTK,x
        and     #$1f
        tay
        lda     SYNTABLNDX,y
LE491:  asl     A       ;double
        tay
        lda     #>SYNTABL-118 ;[#>SYNTABL/2]
        rol     A
        sta     SYNPAG+1
LE498:  bne     @LE49B
        iny
@LE49B: iny
LE49C:  stx     SYNSTKDX
        lda     (SYNPAG),y
        bmi     LE426
        bne     LE4A9
        ldy     #<ErrMsg02 ;"SYNTAX"
LE4A6:  jmp     ERRMESS

LE4A9:  cmp     #$03
        bcs     LE470
        lsr     A       ;halve
        ldx     TXTNDX
        inx
LE4B1:  lda     IN,x
        bcc     @LE4BA
        cmp     #DQT+128
        beq     LE4C4
@LE4BA: cmp     #'_' | $80 ;underline problem?
        beq     LE4C4
        stx     TXTNDX
LE4C0:  jsr     LE41C
        iny
LE4C4:  dey
        ldx     SYNSTKDX
Loop1:  lda     (SYNPAG),y
        dey
        asl     A
        bpl     LE49C
LE4CD:  ldy     SYNSTKH,x
        sty     SYNPAG+1
        ldy     SYNSTKL,x
        inx
        lda     (SYNPAG),y
        and     #%10011111
        bne     Loop1
        sta     PCON
        sta     PCON+1
        tya
        pha
        stx     SYNSTKDX
        ldy     TOKNDXSTK-1,x
        sty     LEADBL
        clc
@Loop1: lda     #$0a
        sta     CHAR
        ldx     #$00
        iny
        lda     IN,y
        and     #$0f
@Loop2: adc     PCON
        pha
        txa
        adc     PCON+1
        bmi     @LE517
        tax
        pla
        dec     CHAR
        bne     @Loop2
        sta     PCON
        stx     PCON+1
        cpy     TOKNDX
        bne     @Loop1
        ldy     LEADBL
        iny
        sty     TOKNDX
        jsr     LE41C
        pla
        tay
        lda     PCON+1
        bcs     LE4C0
@LE517: ldy     #<ErrMsg00 ;">32767"
        bpl     LE4A6   ;(always)

; 
; Prints a 16-bit number in decimal.
; 
; On entry:
;   A-reg: high byte
;   X-reg: low byte
; 
PRDEC:  sta     PCON+1
        stx     PCON
        ldx     #$04
        stx     LEADBL
Loop11: lda     #'0' | $80
        sta     CHAR
@Loop2: lda     PCON
        cmp     NUMLOW,x
        lda     PCON+1
        sbc     NUMHI,x
        bcc     @LE540
        sta     PCON+1
        lda     PCON
        sbc     NUMLOW,x
        sta     PCON
        inc     CHAR
        bne     @Loop2
@LE540: lda     CHAR
        inx
        dex
        beq     PRDEC5
        cmp     #'0' | $80
        beq     @LE54C
        sta     LEADBL
@LE54C: bit     LEADBL
        bmi     PRDEC5
        lda     LEADZR
        beq     PRDEC6
; PRINT
PRDEC5: jsr     MON_COUT
        bit     AUTOFLAG ;auto line?
        bpl     PRDEC6
        sta     IN,y
        iny
; NXTX
PRDEC6: dex
        bpl     Loop11
        rts

NUMLOW: .byte   1       ;1
        .byte   10      ;10
        .byte   100     ;100
        .byte   $e8     ;1000 ($3e8)
        .byte   $10     ;10000 ($2710
NUMHI:  .byte   0       ;1
        .byte   0       ;10
        .byte   0       ;100
        .byte   $03     ;1000 ($03e8)
        .byte   $27     ;10000 ($2710)

LE56D:  lda     PP      ;P3 = PP
        sta     P3
        lda     PP+1
        sta     P3+1
LE575:  inx
LE576:  lda     P3+1    ;P2 = P3
        sta     P2+1
        lda     P3
        sta     P2
        cmp     HIMEM   ;compare P2 and HIMEM
        lda     P2+1
        sbc     HIMEM+1
        bcs     @Return
; 
        ldy     #$01
        lda     (P2),y
        sbc     ACC
        iny
        lda     (P2),y
        sbc     ACC+1
        bcs     @Return
        ldy     #$00
        lda     P3      ;P3 = P3.W + (P2).B
        adc     (P2),y
        sta     P3
        bcc     @LE5A0
        inc     P3+1
        clc
@LE5A0: iny
        lda     ACC     ;is ACC+1 <HS> (P2),Y?
        sbc     (P2),y
        iny
        lda     ACC+1
        sbc     (P2),y
        bcs     LE576
@Return:
        rts

; 
; Token $0b NEW
;   turn off AUTO
;   remove program
;   fall into CLR
; 
NEW:    lsr     AUTOFLAG ;manual
        lda     HIMEM   ;PP = HIMEM
        sta     PP
        lda     HIMEM+1
        sta     PP+1
; 
; Token $0c CLR
;   remove variables
;   remove FOR loops and GOSUBs
; 
CLR:    lda     LOMEM   ;PV = LOMEM
        sta     PV
        lda     LOMEM+1
        sta     PV+1
        lda     #$00
        sta     FORNDX  ;no FORs
        sta     GOSUBNDX ;no GOSUBs
        sta     SYNPAG
        lda     #$00    ;?
        sta     $1d     ;?
        rts

unref_e5cc:
        lda     SRCH
; 
ErrMemFull:
        jmp     MEMFULL

Loop12: ldy     #$ff
Loop2:  sty     XSAVE
@Loop3: iny
        lda     (PX),y
        bmi     @LE5E0
        cmp     #$40
        bne     LE646   ;exit loop
        sta     XSAVE
@LE5E0: cmp     (SRCH),y
        beq     @Loop3
LE5E4:  lda     (SRCH),y
LE5E6:  iny
        lsr     A
        bne     LE5E4
        lda     (SRCH),y
        pha
        iny
        lda     (SRCH),y
        tay
        pla
LE5F2:  sta     SRCH
        sty     SRCH+1
        cmp     PV
        bne     Loop12
        cpy     PV+1
        bne     Loop12
        ldy     #$00
@Loop:  iny
        lda     (PX),y
        bmi     @Loop
        eor     #$40
        beq     @Loop
        tya
        adc     #$04
        pha
        adc     SRCH
        tay
        lda     SRCH+1
        adc     #$00
        pha
        cpy     PP
        sbc     PP+1
        bcs     ErrMemFull ;"MEM FULL"
        sty     PV
        pla
        sta     PV+1
        pla
        tay
        lda     #$00
        dey
        sta     (SRCH),y
        dey
        sta     (SRCH),y
        dey
        lda     PV+1
        sta     (SRCH),y
        dey
        lda     PV
        sta     (SRCH),y
        dey
        lda     #$00
@Loop1: sta     (SRCH),y
        dey
        bmi     Loop2
        lda     (PX),y
        bne     @Loop1  ;(always)

LE640:  lda     LOMEM
        ldy     LOMEM+1
        bne     LE5F2   ;(always)

LE646:  lda     (SRCH),y
        cmp     #$40
        bcs     LE5E6
        sta     NOUNSTKC-1,x
        tya
        adc     #$03
        pha
        adc     SRCH
        jsr     LE70A
@Loop:  jsr     GETVERB
        dey
        bne     @Loop
        tya
        adc     SRCH+1
        sta     NOUNSTKH,x
        pla
        bit     XSAVE
        bmi     LE684
        tay
        lda     #$00
        jsr     LE70A
        sta     NOUNSTKH,x
@LE66F: lda     (SRCH),y
        bpl     LE682   ;exit loop
        inc     NOUNSTKH,x
        iny
        bne     @LE66F  ;(always)

        .byte   $09

LE679:  lda     #$00
        sta     IFFLAG
        sta     CRFLAG
        ldx     #$20
LE681:  pha
LE682:  ldy     #$00
LE684:  lda     (PX),y
@LE686: bpl     @LE6A0  ;exit loop
        asl     A
        bmi     LE640
        jsr     GETVERB
        jsr     LE708
        jsr     GETVERB
        sta     NOUNSTKC,x
@LE696: bit     IFFLAG
        bpl     @LE69B
        dex
@LE69B: jsr     GETVERB
        bcs     @LE686
@LE6A0: cmp     #$28
        bne     @LE6C3
        lda     PX
        jsr     LE70A
        lda     PX+1
        sta     NOUNSTKH,x
        bit     IFFLAG
        bmi     @LE6BC
        lda     #$01
        jsr     LE70A
        lda     #$00
        sta     NOUNSTKH,x
@Loop:  inc     NOUNSTKH,x
@LE6BC: jsr     GETVERB
        bmi     @Loop
        bcs     @LE696
@LE6C3: bit     IFFLAG
        bpl     @LE6CD
        cmp     #$04
        bcs     @LE69B
        lsr     IFFLAG
@LE6CD: tay
        sta     VERBNOW
        lda     TABLE1,y
        and     #%01010101 ;even bits only
        asl     A
        sta     PRFLAG  ;temp
@LE6D8: pla
        tay
        lda     TABLE1,y
        and     #%10101010 ;odd bits only
        cmp     PRFLAG
        bcs     @LE6EC
        tya
        pha
        jsr     LF3EB
        lda     VERBNOW
        bcc     LE681   ;(always)
; BRANCH: get high/low then JSR.
@LE6EC: lda     VERBADRL,y
        sta     ACC
        lda     VERBADRH,y
        sta     ACC+1
        jsr     @LE6FC
        jmp     @LE6D8

@LE6FC: jmp     (ACC)

; 
; Get next verb to use.
; 
GETVERB:
        inc     PX
        bne     @NoInc
        inc     PX+1
@NoInc: lda     (PX),y
        rts

LE708:  sty     NOUNSTKH-1,x
LE70A:  dex
        bmi     @LE710
        sta     NOUNSTKL,x
        rts

@LE710: ldy     #<ErrMsg12+3 ;"PPED AT"
LE712:  jmp     ERRMESS

; 
; Get a 16-bit value.
; 
GET16BIT:
        ldy     #$00
        lda     NOUNSTKL,x
        sta     ACC
        lda     NOUNSTKC,x
        sta     ACC+1
        lda     NOUNSTKH,x
        beq     @LE731
        sta     ACC+1
        lda     (ACC),y ;ACC = (ACC),Y
        pha             ;save low byte
        iny             ;Y-reg = 1
        lda     (ACC),y
        sta     ACC+1
        pla             ;restore low byte
        sta     ACC
        dey             ;Y-reg = 0
@LE731: inx
        rts

; 
; Token $16 =
;   num var logic op
;   IF X = 13 THEN END
; 
TE733:  jsr     TE74A
; 
; Token $37 NOT
;   numeric
;   IF NOT X THEN END
; 
NOT:    jsr     GET16BIT
        tya             ;A-reg = 0
        jsr     LE708
        sta     NOUNSTKC,x
        cmp     ACC
        bne     @Return
        cmp     ACC+1
        bne     @Return
        inc     NOUNSTKL,x
@Return:
        rts

; 
; Token $17 #
;   num var logic op
;   IF X # 13 THEN END
; 
; Token $1b <>
;   num var logic op
;   IF X <> 13 THEN END
; 
TE74A:  jsr     SUBTRACT
        jsr     SGN
; 
; Token $31 ABS
; 
ABS:    jsr     GET16BIT
        bit     ACC+1
        bmi     LE772
LE757:  dex
Return3:
        rts

; 
; Token $30 SGN
; 
SGN:    jsr     GET16BIT
        lda     ACC+1   ;is ACC zero?
        bne     @LE764
        lda     ACC
        beq     LE757   ;yes
@LE764: lda     #$ff
        jsr     LE708
        sta     NOUNSTKC,x
        bit     ACC+1
        bmi     Return3
; 
; Token $36 -
;   unary sign of number
;   X = -5
; 
NEGATE: jsr     GET16BIT
LE772:  tya             ;A-reg = 0
        sec
        sbc     ACC
        jsr     LE708
        tya
        sbc     ACC+1
        bvc     LE7A1
Err32767:
        ldy     #<ErrMsg00 ;">32767"
        bpl     LE712   ;(always)

; 
; Token $13 -
;   num op
;   X = 27 - 2
; 
SUBTRACT:
        jsr     NEGATE  ;negate, then add
; 
; Token $12 +
;   num op
;   X = 27 + 2
; 
TE785:  jsr     GET16BIT
        lda     ACC     ;AUX = ACC
        sta     AUX
        lda     ACC+1
        sta     AUX+1
        jsr     GET16BIT
LE793:  clc
        lda     ACC
        adc     AUX
        jsr     LE708
        lda     ACC+1
        adc     AUX+1
        bvs     Err32767
LE7A1:  sta     NOUNSTKC,x
; 
; Token $35 +
;   unary sign of number
;   X = +5
; 
POSITIVE:
        rts

; 
; Token $50 TAB
; 
TAB:    jsr     GETBYTE
        tay
        bne     @LE7AD
        jmp     BcsRANGERR ;range error?

@LE7AD: dey
LE7AE:  jmp     LF3F4

; 
; Comma tab to next tab posn (every 8 spaces).
; 
LE7B1:  lda     MON_CH  ;get horiz posn
        ora     #$07    ;set bits 0-2
        tay
        iny             ;incr, is it zero? [only if CH=$f8-ff (invalid)]
unref_branch:
        bne     LE7AE   ;no, adjust CH [always?]
        iny             ;yes, go to next tab posn [sets Y-reg=1]
        bne     LE7B1   ;(always)

unref_e7bc:
        bcs     unref_branch
        rts

        .byte   $00,$00

; 
; Token $49 ,
;   num print follows
;   PRINT A$,X
; 
TE7C1:  jsr     LE7B1
; 
; Token $46 ;
;   num print follows
;   PRINT A$ ; X
; 
; Token $62 PRINT
;   num value
;   PRINT 123: PRINT X: PRINT ASC(A$)
; 
PRINTNUM:
        jsr     GET16BIT
LE7C7:  lda     ACC+1   ;is it positive?
        bpl     @LE7D5
        lda     #'-' | $80 ;no, print minus sign
        jsr     MON_COUT
        jsr     LE772
        bvc     PRINTNUM ;(always)

@LE7D5: dey             ;Y-reg = $ff
        sty     CRFLAG  ;CRFLAG = $ff
        stx     ACC+1   ;save X-reg
        ldx     ACC
        jsr     PRDEC
        ldx     ACC+1   ;restore X-reg
        rts

; 
; Token $0d AUTO
; 
AUTO:   jsr     GET16BIT
        lda     ACC     ;AUTOLN = ACC
        sta     AUTOLN
        lda     ACC+1
        sta     AUTOLN+1
        dey
        sty     AUTOFLAG ;AUTOFLAG = $ff
        iny
        lda     #10     ;default increment
LE7F3:  sta     AUTOINC
        sty     AUTOINC+1
        rts

; 
; Token $0e ,
;   AUTO 10,20
; 
COMMA_AUTO:
        jsr     GET16BIT
        lda     ACC
        ldy     ACC+1
        bpl     LE7F3   ;(always)

; 
; Token $56 =
;   FOR X = 5 TO 10
; 
; Token $71 =
;   num - non-conditional
;   X = 5
; 
TE801:  jsr     GET16BIT
        lda     NOUNSTKL,x
        sta     AUX
        lda     NOUNSTKH,x
        sta     AUX+1
        lda     ACC
        sta     (AUX),y
        iny
        lda     ACC+1
        jmp     LF207

; 
; Token $25 THEN
;   IF X = 3 THEN Y = 5
; 
; Token $5e LET
; 
LET:    rts

; 
; Token $00
;   internal begin-of-line
; 
BEGIN_LINE:
        pla
        pla
; 
; Token $03 :
;   statement separation
;   X = 5: A$ = "HELLO"
; 
COLON:  bit     CRFLAG
        bpl     Return4
; 
; Token $63 PRINT
;   dummy print
;   PRINT: PRINT
; 
PRINT_CR:
        jsr     MON_CROUT
; 
; Token $47 ;
;   end of print statement
;   PRINT A$;
; 
TE820:  lsr     CRFLAG
Return4:
        rts

; 
; Token $22 (
;   string DIM
;   DIM A$(X)
; 
; Token $34 (
;   num DIM
;   DIM X(5)
; 
; Token $38 (
;   logic statements and num operations
;   IF C and (A=14 OR B=12) THEN X=(27+3)/13
; 
; Token $3f (
;   used after PEEK, RND, SGN, ABS, and PDL
; 
TE823:  ldy     #$ff
        sty     PRFLAG  ;PRFLAG = $ff
; 
; Token $72 )
;   the only right parenthesis token
; 
RIGHT_PAREN:
        rts

; 
; Token $60 IF
; 
IF:     jsr     LEFCD
        beq     @LE834
        lda     #$25    ;THEN token?
        sta     VERBNOW
        dey
        sty     IFFLAG
@LE834: inx
        rts

; 
; Run without CLR
;   DOS 3.3 chains here to run a program
; 
RUNWARM:
        lda     PP
        ldy     PP+1
        bne     LE896   ;(always)

; 
; Token $5c GOSUB
; 
GOSUB:  ldy     #<ErrMsg08 ;"16 GOSUBS"
        lda     GOSUBNDX
        cmp     #16     ;sixteen GOSUBs?
        bcs     JmpERRMESS1 ;yes, error
        tay
        inc     GOSUBNDX
; 
        lda     PX
        sta     STK_00,y
        lda     PX+1
        sta     STK_10,y
; 
        lda     PR
        sta     STK_20,y
        lda     PR+1
        sta     STK_30,y
; 
; Token $24 THEN
;   followed by a line number
;   IF X=3 THEN 10
; 
; Token $5f GOTO
; 
GOTO:   jsr     GET16BIT
        jsr     LE56D
        bcc     @LE867
        ldy     #<ErrMsg07 ;"BAD BRANCH"
        bne     JmpERRMESS1

@LE867: lda     P2
        ldy     P2+1
; Main loop for running Integer BASIC programs.
LE86B:  sta     PR
        sty     PR+1
        clc
        adc     #$03
        bcc     GETNEXT
        iny
; Fetch next statement from text source.
GETNEXT:
        ldx     #$ff
        stx     RUNFLAG
        txs
        sta     PX
        sty     PX+1
        jsr     LF02E   ;test for Ctrl+C and TRACE mode
        ldy     #$00
LE883:  jsr     LE679   ;execute statement
        bit     RUNFLAG
        bpl     END
        clc
        ldy     #$00
        lda     PR
        adc     (PR),y
        ldy     PR+1
        bcc     LE896
        iny
LE896:  cmp     HIMEM
        bne     LE86B
        cpy     HIMEM+1
        bne     LE86B
        ldy     #<ErrMsg06 ;"NO END"
        lsr     RUNFLAG ;pos
JmpERRMESS1:
        jmp     ERRMESS

; 
; Token $5b RETURN
; 
RETURN: ldy     #<ErrMsg09 ;"BAD RETURN"
        lda     GOSUBNDX
        beq     JmpERRMESS1
        dec     GOSUBNDX
        tay
        lda     STK_20-1,y
        sta     PR
        lda     STK_30-1,y
        sta     PR+1
        ldx     a:STK_00-1,y
        lda     STK_10-1,y
LE8BE:  tay
        txa
        jmp     GETNEXT

STOPPED_AT:
        ldy     #<ErrMsg12 ;"STOPPED AT"
        jsr     ERRORMESS
        ldy     #$01
        lda     (PR),y
        tax
        iny
        lda     (PR),y
        jsr     PRDEC
; 
; Token $51 END
; 
END:    jmp     WARM

LE8D6:  dec     FORNDX
; 
; Token $59 NEXT
; 
; Token $5a ,
;   NEXT X,Y
; 
NEXT:   ldy     #<ErrMsg11 ;"BAD NEXT"
        lda     FORNDX
JmpERRMESS2:
        beq     JmpERRMESS1 ;no more FORs
        tay
        lda     NOUNSTKL,x
        cmp     STK_40-1,y
        bne     LE8D6
        lda     NOUNSTKH,x
        cmp     STK_50-1,y
        bne     LE8D6
        lda     STK_60-1,y
        sta     AUX
        lda     STK_70-1,y
        sta     AUX+1
; 
        jsr     GET16BIT
        dex
        jsr     LE793
        jsr     TE801
        dex
        ldy     FORNDX
        lda     STK_D0-1,y
        sta     NOUNSTKC-1,x
        lda     STK_C0-1,y
        ldy     #$00
        jsr     LE708
        jsr     SUBTRACT
        jsr     SGN
        jsr     GET16BIT
        ldy     FORNDX
        lda     ACC
        beq     @LE925
        eor     STK_70-1,y
        bpl     @LE937
; 
@LE925: lda     STK_80-1,y
        sta     PR
        lda     STK_90-1,y
        sta     PR+1
; 
        ldx     STK_A0-1,y
        lda     STK_B0-1,y
        bne     LE8BE
@LE937: dec     FORNDX
        rts

; 
; Token $55 FOR
; 
FOR:    ldy     #<ErrMsg10 ;"16 FORS"
        lda     FORNDX
        cmp     #16     ;sixteen FORs?
        beq     JmpERRMESS2 ;yes, error
        inc     FORNDX
        tay
        lda     NOUNSTKL,x
        sta     STK_40,y
        lda     NOUNSTKH,x
        jmp     LF288

        .byte   $60

; 
; Token $57 TO
; 
TO:     jsr     GET16BIT
        ldy     FORNDX
; 
        lda     ACC
        sta     STK_C0-1,y
        lda     ACC+1
        sta     STK_D0-1,y
; 
        lda     #$01
        sta     STK_60-1,y
        lda     #$00
LE966:  sta     STK_70-1,y
; 
        lda     PR
        sta     STK_80-1,y
        lda     PR+1
        sta     STK_90-1,y
; 
        lda     PX
        sta     STK_A0-1,y
        lda     PX+1
        sta     STK_B0-1,y
        rts

        .byte   $20
        .byte   $15
; 
TABLE1: .byte   $00,$00,$00,$ab,$03,$03,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $03,$03,$3f,$3f,$c0,$c0,$3c,$3c
        .byte   $3c,$3c,$3c,$3c,$3c,$30,$0f,$c0
        .byte   $c3,$ff,$55,$00,$ab,$ab,$03,$03
        .byte   $ff,$ff,$55,$ff,$ff,$55,$cf,$cf
        .byte   $cf,$cf,$cf,$ff,$55,$c6,$c6,$c6
        .byte   $55,$f0,$f0,$cf,$cf,$55,$01,$55
        .byte   $ff,$ff,$55,$03,$03,$03,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $03,$03,$03,$03,$03,$00,$ab,$03
        .byte   $57,$03,$03,$03,$03,$07,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
        .byte   $03,$03,$aa,$ff,$03,$03,$03,$03
        .byte   $03,$03,$03,$03,$03,$03,$03,$03
; 
; Token address tables (verb dispatch tables).
; 
VERBADRL:
        .byte   <BEGIN_LINE
        .byte   $ff
        .byte   $ff
        .byte   <COLON
        .byte   <LOAD
        .byte   <SAVE
        .byte   <CON
        .byte   <RUNNUM
        .byte   <RUN
        .byte   <DEL
        .byte   <COMMA_DEL
        .byte   <NEW
        .byte   <CLR
        .byte   <AUTO
        .byte   <COMMA_AUTO
        .byte   <TEE54
        .byte   <VHIMEM
        .byte   <VLOMEM
        .byte   <TE785
        .byte   <SUBTRACT
        .byte   <MULT
        .byte   <DIVIDE
        .byte   <TE733
        .byte   <TE74A
        .byte   <TF25B
        .byte   <TF24E
        .byte   <TF253
        .byte   <TE74A
        .byte   <TF249
        .byte   <VAND
        .byte   <VOR
        .byte   <MOD
        .byte   <EXP
        .byte   $ff
        .byte   <TE823
        .byte   <COMMA_SUBSTR
        .byte   <GOTO
        .byte   <LET
        .byte   <TEFB6
        .byte   <TEBCB
        .byte   $ff
        .byte   $ff
        .byte   <PAREN_SUBSTR
        .byte   $ff
        .byte   $ff
        .byte   <TEF24
        .byte   <PEEK
        .byte   <RND
        .byte   <SGN
        .byte   <ABS
        .byte   <PDL
        .byte   $ff
        .byte   <TE823
        .byte   <POSITIVE
        .byte   <NEGATE
        .byte   <NOT
        .byte   <TE823
        .byte   <TE1D7
        .byte   <TE21C
        .byte   <LEN
        .byte   <ASC
        .byte   <SCRN
        .byte   <COMMA_SCRN
        .byte   <TE823
        .byte   $ff
        .byte   $ff
        .byte   <TE121
        .byte   <DIMSTR
        .byte   <DIMNUM
        .byte   <PRINTSTR
        .byte   <PRINTNUM
        .byte   <TE820
        .byte   <TEE00
        .byte   <TE7C1
        .byte   <TF3BA
        .byte   <MON_SETTXT
        .byte   <MON_SETGR
        .byte   <CALL
        .byte   <DIMSTR
        .byte   <DIMNUM
        .byte   <TAB
        .byte   <END
        .byte   <TEFB6
        .byte   <INPUT_PROMPT
        .byte   <TEBAA
        .byte   <FOR
        .byte   <TE801
        .byte   <TO
        .byte   <STEP
        .byte   <NEXT
        .byte   <NEXT
        .byte   <RETURN
        .byte   <GOSUB
        .byte   $ff
        .byte   <LET
        .byte   <GOTO
        .byte   <IF
        .byte   <PRINTSTR
        .byte   <PRINTNUM
        .byte   <PRINT_CR
        .byte   <POKE
        .byte   <GETVAL255
        .byte   <TEE4E
        .byte   <GETVAL255
        .byte   <COMMA_PLOT
        .byte   <GETVAL255
        .byte   <COMMA_HLIN
        .byte   <AT_HLIN
        .byte   <GETVAL255
        .byte   <COMMA_VLIN
        .byte   <AT_VLIN
        .byte   <IVTAB
        .byte   <TE18C
        .byte   <TE801
        .byte   <RIGHT_PAREN
        .byte   $ff
        .byte   <LISTNUM
        .byte   <COMMA_LIST
        .byte   <LIST
        .byte   <POP
        .byte   <NODSP_STR
        .byte   <NODSP_NUM
        .byte   <NOTRACE
        .byte   <DSP_STR
        .byte   <DSP_NUM
        .byte   <TRACE
        .byte   <PRSLOT
        .byte   <INSLOT
VERBADRH:
        .byte   >BEGIN_LINE
        .byte   $ff
        .byte   $ff
        .byte   >COLON
        .byte   >LOAD
        .byte   >SAVE
        .byte   >CON
        .byte   >RUNNUM
        .byte   >RUN
        .byte   >DEL
        .byte   >COMMA_DEL
        .byte   >NEW
        .byte   >CLR
        .byte   >AUTO
        .byte   >COMMA_AUTO
        .byte   >TEE54
        .byte   >VHIMEM
        .byte   >VLOMEM
        .byte   >TE785
        .byte   >SUBTRACT
        .byte   >MULT
        .byte   >DIVIDE
        .byte   >TE733
        .byte   >TE74A
        .byte   >TF25B
        .byte   >TF24E
        .byte   >TF253
        .byte   >TE74A
        .byte   >TF249
        .byte   >VAND
        .byte   >VOR
        .byte   >MOD
        .byte   >EXP
        .byte   $ff
        .byte   >TE823
        .byte   >COMMA_SUBSTR
        .byte   >GOTO
        .byte   >LET
        .byte   >TEFB6
        .byte   >TEBCB
        .byte   $ff
        .byte   $ff
        .byte   >PAREN_SUBSTR
        .byte   $ff
        .byte   $ff
        .byte   >TEF24
        .byte   >PEEK
        .byte   >RND
        .byte   >SGN
        .byte   >ABS
        .byte   >PDL
        .byte   $ff
        .byte   >TE823
        .byte   >POSITIVE
        .byte   >NEGATE
        .byte   >NOT
        .byte   >TE823
        .byte   >TE1D7
        .byte   >TE21C
        .byte   >LEN
        .byte   >ASC
        .byte   >SCRN
        .byte   >COMMA_SCRN
        .byte   >TE823
        .byte   $ff
        .byte   $ff
        .byte   >TE121
        .byte   >DIMSTR
        .byte   >DIMNUM
        .byte   >PRINTSTR
        .byte   >PRINTNUM
        .byte   >TE820
        .byte   >TEE00
        .byte   >TE7C1
        .byte   >TF3BA
        .byte   >MON_SETTXT
        .byte   >MON_SETGR
        .byte   >CALL
        .byte   >DIMSTR
        .byte   >DIMNUM
        .byte   >TAB
        .byte   >END
        .byte   >TEFB6
        .byte   >INPUT_PROMPT
        .byte   >TEBAA
        .byte   >FOR
        .byte   >TE801
        .byte   >TO
        .byte   >STEP
        .byte   >NEXT
        .byte   >NEXT
        .byte   >RETURN
        .byte   >GOSUB
        .byte   $ff
        .byte   >LET
        .byte   >GOTO
        .byte   >IF
        .byte   >PRINTSTR
        .byte   >PRINTNUM
        .byte   >PRINT_CR
        .byte   >POKE
        .byte   >GETVAL255
        .byte   >TEE4E
        .byte   >GETVAL255
        .byte   >COMMA_PLOT
        .byte   >GETVAL255
        .byte   >COMMA_HLIN
        .byte   >AT_HLIN
        .byte   >GETVAL255
        .byte   >COMMA_VLIN
        .byte   >AT_VLIN
        .byte   >IVTAB
        .byte   >TE18C
        .byte   >TE801
        .byte   >RIGHT_PAREN
        .byte   $ff
        .byte   >LISTNUM
        .byte   >COMMA_LIST
        .byte   >LIST
        .byte   >POP
        .byte   >NODSP_STR
        .byte   >NODSP_NUM
        .byte   >NOTRACE
        .byte   >DSP_STR
        .byte   >DSP_NUM
        .byte   >TRACE
        .byte   >PRSLOT
        .byte   >INSLOT
; 
; Error messages.
; 
ErrMsg00:
        .byte   $be,$b3,$b2,$b7,$b6,$37
ErrMsg01:
        .byte   $d4,$cf,$cf,$a0,$cc,$cf,$ce,$47
ErrMsg02:
        .byte   $d3,$d9,$ce,$d4,$c1,$58
ErrMsg03:
        .byte   $cd,$c5,$cd,$a0,$c6,$d5,$cc,$4c
ErrMsg04:
        .byte   $d4,$cf,$cf,$a0,$cd,$c1,$ce,$d9,$a0,$d0,$c1,$d2,$c5,$ce,$53
ErrMsg05:
        .byte   $d3,$d4,$d2,$c9,$ce,$47
ErrMsg06:
        .byte   $ce,$cf,$a0,$c5,$ce,$44
ErrMsg07:
        .byte   $c2,$c1,$c4,$a0,$c2,$d2,$c1,$ce,$c3,$48
ErrMsg08:
        .byte   $b1,$b6,$a0,$c7,$cf,$d3,$d5,$c2,$53
ErrMsg09:
        .byte   $c2,$c1,$c4,$a0,$d2,$c5,$d4,$d5,$d2,$4e
ErrMsg10:
        .byte   $b1,$b6,$a0,$c6,$cf,$d2,$53
ErrMsg11:
        .byte   $c2,$c1,$c4,$a0,$ce,$c5,$d8,$54
ErrMsg12:
        .byte   $d3,$d4,$cf,$d0,$d0,$c5,$c4,$a0,$c1,$d4,$20
ErrMsg13:
        .byte   $aa,$aa,$aa,$20
ErrMsg14:
        .byte   $a0,$c5,$d2,$d2,$0d
ErrMsg15:
        .byte   $be,$b2,$b5,$35
ErrMsg16:
        .byte   $d2,$c1,$ce,$c7,$45
ErrMsg17:
        .byte   $c4,$c9,$4d
ErrMsg18:
        .byte   $d3,$d4,$d2,$a0,$cf,$d6,$c6,$4c
        .byte   $dc,$0d
ErrMsg20:
        .byte   $d2,$c5,$d4,$d9,$d0,$c5,$a0,$cc,$c9,$ce,$c5,$8d
ErrMsg21:
        .byte   '?'

; Continue run w/o deleting vars?
LEB9A:  lsr     RUNFLAG ;pos
        bcc     @LEBA1
        jmp     STOPPED_AT

@LEBA1: ldx     ACC+1
        txs
        ldx     ACC
        ldy     #<ErrMsg20 ;"RETYPE LINE\r?"
        bne     LEBAC   ;(always)

; 
; Token $54 INPUT
;   num with no prompt
;   INPUT X
; 
TEBAA:  ldy     #<ErrMsg21 ;"?" for INPUT
LEBAC:  jsr     ERRORMESS
        stx     ACC
        tsx
        stx     ACC+1
        jsr     LF366
        sty     TOKNDX
        lda     #$ff
        sta     TXTNDX
        asl     A
        sta     RUNFLAG ;neg
        ldx     #$20
        lda     #$15
        jsr     LE491
        inc     RUNFLAG
        ldx     ACC
; 
; Token $27 ,
;   num inputs
;   INPUT "QUANTITY",Q
; 
TEBCB:  ldy     TXTNDX
        asl     A
@LEBCE: sta     ACC
        iny
        lda     IN,y
        cmp     #$80
        beq     TEBAA   ;end of input?
        eor     #'0' | $80
        cmp     #10
        bcs     @LEBCE
        iny
        iny
        sty     TXTNDX
        lda     IN,y
        pha
        lda     IN-1,y
        ldy     #$00
        jsr     LE708
        pla
        sta     NOUNSTKC,x
        lda     ACC
        cmp     #$33
        bne     @LEBFA
        jsr     NEGATE
@LEBFA: jmp     TE801

        .res    3,$ff
; 
; Token / syntax table.
; 
; Each entry has a token, stored in reverse, with character values adjusted. 
; For example, "TRACE" is stored:
;   "E"+32, "C"-32, "A"-32, "R"-32", "T"-32
;   $e5     $a3     $a1     $b2      $b4
; 
SYNTABL:
        .byte   $50,$20,$4f,$c0,$f4,$a1,$e4,$af
        .byte   $ad,$f2,$af,$e4,$ae,$a1,$f0,$a5
        .byte   $b4,$b3,$ef,$b4,$ee,$a5,$a8,$b4
; 
        .byte   $5c,$80,$00,$40,$60,$8d,$60,$8b
        .byte   $7f,$1d,$20,$7e,$8c,$33,$00,$00
        .byte   $60,$03,$bf,$12
; 
        .byte   $47,$83,$ae,$a9,$67,$83,$b2,$b0
        .byte   $e5,$a3,$a1,$b2,$b4,$79,$b0,$b3
        .byte   $a4,$69,$b0,$b3,$a4,$e5,$a3,$a1
        .byte   $b2,$b4,$af,$ae,$79,$b0,$b3,$a4
        .byte   $af,$ae,$69,$b0,$b3,$a4,$af,$ae
        .byte   $f0,$af,$b0,$f4,$b3,$a9,$ac,$60
        .byte   $8c,$20,$b4,$b3,$a9,$ac
        .byte   $00
        .byte   $40,$89,$c9,$47,$9d,$17,$68,$9d
        .byte   $0a,$58,$7b,$67,$a2,$a1,$b4,$b6
        .byte   $67,$b4,$a1,$07,$8c,$07,$ae,$a9
        .byte   $ac,$b6,$67,$b4,$a1,$07,$8c,$07
        .byte   $ae,$a9,$ac,$a8,$67,$8c,$07,$b4
        .byte   $af,$ac,$b0,$67,$9d,$b2,$af,$ac
        .byte   $af,$a3,$67,$8c,$07,$a5,$ab,$af
        .byte   $b0,$f4,$ae,$a9,$b2,$b0,$7f,$0e
        .byte   $27,$b4,$ae,$a9,$b2,$b0,$7f,$0e
        .byte   $28,$b4,$ae,$a9,$b2,$b0,$64,$07
        .byte   $a6,$a9,$67,$af,$b4,$af,$a7,$78
        .byte   $b4,$a5,$ac,$6b,$7f,$02,$ad,$a5
        .byte   $b2,$67,$a2,$b5,$b3,$af,$a7,$ee
        .byte   $b2,$b5,$b4,$a5,$b2,$7e,$8c,$39
        .byte   $b4,$b8,$a5,$ae,$67,$b0,$a5,$b4
        .byte   $b3,$27,$af,$b4,$07,$9d,$19,$b2
        .byte   $af,$a6,$7f,$05,$37,$b4,$b5,$b0
        .byte   $ae,$a9,$7f,$05,$28,$b4,$b5,$b0
        .byte   $ae,$a9,$7f,$05,$2a,$b4,$b5,$b0
        .byte   $ae,$a9,$e4,$ae,$a5
; 
SYNTABL2:
        .byte   $00
        .byte   $47,$a2,$a1,$b4,$7f,$0d,$30,$ad
        .byte   $a9,$a4,$7f,$0d,$23,$ad,$a9,$a4
        .byte   $67,$ac,$ac,$a1,$a3,$f2,$a7,$f4
        .byte   $b8,$a5,$b4
        .byte   $00     ;above are statements
        .byte   $4d,$cc,$67,$8c,$68,$8c,$db,$67
        .byte   $9b,$68,$9b,$50,$8c,$63,$8c,$7f
        .byte   $01,$51,$07,$88,$29,$84,$80,$c4
        .byte   $19,$57,$71,$07,$88,$14,$71,$07
        .byte   $8c,$07,$88,$ae,$b2,$a3,$b3,$71
        .byte   $08,$88,$a3,$b3,$a1,$71,$08,$88
        .byte   $ae,$a5,$ac,$68,$83,$08,$68,$9d
        .byte   $08,$71,$07,$88,$60,$75,$b4,$af
        .byte   $ae,$75,$8d,$75,$8b,$51,$07,$88
        .byte   $19,$b8,$a4,$ae,$b2,$ec,$a4,$b0
        .byte   $f3,$a2,$a1,$ee,$a7,$b3,$e4,$ae
        .byte   $b2,$eb,$a5,$a5,$b0,$51,$07,$88
        .byte   $39,$81,$c1,$4f,$7f,$0f,$2f,$00
        .byte   $51,$06,$88,$29,$c2,$0c,$82,$57
        .byte   $8c,$6a,$8c,$42,$ae,$a5,$a8,$b4
        .byte   $60,$ae,$a5,$a8,$b4,$4f,$7e,$1e
        .byte   $35,$8c,$27,$51,$07,$88,$09,$8b
        .byte   $fe,$e4,$af,$ad,$f2,$af,$e4,$ae
        .byte   $a1,$dc,$de,$9c,$dd,$9c,$de,$dd
        .byte   $9e,$c3,$dd,$cf,$ca,$cd,$cb
        .byte   $00     ;above 4 are num ops
        .byte   $47,$9a,$ad,$a5,$ad,$af,$ac,$67
        .byte   $9a,$ad,$a5,$ad,$a9,$a8,$ee,$a1
        .byte   $ad,$60,$8c,$20,$af,$b4,$b5,$a1
        .byte   $f2,$ac,$a3,$f7,$a5,$ae,$60,$8c
        .byte   $20,$ac,$a5,$a4,$ee,$b5,$b2,$60
        .byte   $ae,$b5,$b2,$ee,$af,$a3,$e5,$b6
        .byte   $a1,$b3,$e4,$a1,$af,$ac
; Above are commands.
        .byte   $7a,$7e,$9a,$22,$20,$00,$60,$03
        .byte   $bf,$60,$03,$bf,$1f

; 
; Token $48 ,
;   string prints
;   PRINT T,A$
; 
TEE00:  jsr     LE7B1
; 
; Token $45 ;
;   string prints
;   PRINT anytype ; string
; 
; Token $61 PRINT
;   string var or literal
;   PRINT A$: PRINT "HELLO"
; 
PRINTSTR:
        inx
        inx
        lda     NOUNSTKL-1,x
        sta     AUX
        lda     NOUNSTKH-1,x
        sta     AUX+1
        ldy     NOUNSTKL-2,x
@Loop:  tya
        cmp     NOUNSTKH-2,x
        bcs     @LEE1D  ;exit loop
        lda     (AUX),y
        jsr     MON_COUT
        iny
        jmp     @Loop

@LEE1D: lda     #$ff
        sta     CRFLAG  ;CRFLAG = $ff
        rts

; 
; Token $3b LEN(
; 
LEN:    inx
        lda     #$00
        sta     NOUNSTKH,x
        sta     NOUNSTKC,x
        lda     NOUNSTKH-1,x
        sec
        sbc     NOUNSTKL-1,x
        sta     NOUNSTKL,x
        jmp     TE823

        .byte   $ff

GETBYTE:
        jsr     GET16BIT
        lda     ACC+1
        bne     HI255ERR ;">255"
        lda     ACC
        rts

; 
; Token $68 ,
;   PLOT 20,15
; 
COMMA_PLOT:
        jsr     GETBYTE
        ldy     TXTNDX
        cmp     #48
        bcs     RANGERR
        cpy     #40
        bcs     RANGERR
        jmp     MON_PLOT

; 
; Token $66 COLOR=
; 
TEE4E:  jsr     GETBYTE
        jmp     MON_SETCOL

; 
; Token $0f MAN
; 
TEE54:  lsr     AUTOFLAG ;manual
        rts

; 
; Token $6f VTAB
; 
IVTAB:  jsr     LF3B3
        cmp     #24
        bcs     RANGERR
        sta     MON_CV
        jmp     MON_VTAB

HI255ERR:
        ldy     #<ErrMsg15 ;">255"
JmpERRMESS3:
        jmp     ERRMESS

RANGERR:
        ldy     #<ErrMsg16 ;"RANGE"
        bne     JmpERRMESS3

; 
; Divide routine.
; 
LEE6C:  jsr     LE254
        lda     AUX     ;is AUX zero?
        bne     @LEE7A
        lda     AUX+1
        bne     @LEE7A
        jmp     Err32767 ;yes, ">32767" error

@LEE7A: asl     ACC
        rol     ACC+1
        rol     P3
        rol     P3+1
        lda     P3
        cmp     AUX     ;compare P3 to AUX
        lda     P3+1
        sbc     AUX+1
        bcc     @LEE96
        sta     P3+1    ;P3 = P3 - AUX
        lda     P3
        sbc     AUX
        sta     P3
        inc     ACC
@LEE96: dey
        bne     @LEE7A
        rts

        .byte   $ff,$ff,$ff,$ff,$ff,$ff

; 
; Token $4d CALL
; 
CALL:   jsr     GET16BIT
        jmp     (ACC)

; 
; Token $6a ,
;   HLIN 10,20 AT 30
; 
COMMA_HLIN:
        jsr     GETBYTE
        cmp     TXTNDX
        bcc     RANGERR
        sta     MON_H2
        rts

; 
; Token $6b AT
;   HLIN 10,20 AT 30
; 
AT_HLIN:
        jsr     GETBYTE
        cmp     #48
        bcs     RANGERR
        ldy     TXTNDX
        jmp     MON_HLINE

; 
; Token $6d ,
;   VLIN 10,20 AT 30
; 
COMMA_VLIN:
        jsr     GETBYTE
        cmp     TXTNDX
        bcc     RANGERR
        sta     MON_V2
        rts

; 
; Token $6e AT
;   VLIN 10,20 AT 30
; 
AT_VLIN:
        jsr     GETBYTE
        cmp     #40
BcsRANGERR:
        bcs     RANGERR
        tay
        lda     TXTNDX
        jmp     MON_VLINE

PRINTERR:
        tya
        tax
        ldy     #<ErrMsg13 ;"*** "
        jsr     ERRORMESS
        txa
        tay
        jsr     ERRORMESS
        ldy     #<ErrMsg14 ;" ERR"
        jmp     PRTERR

LEEE4:  jsr     LF23F
@LEEE7: asl     ACC
        rol     ACC+1
        bmi     @LEEE7
        bcs     BcsRANGERR
        bne     @LEEF5
        cmp     ACC
        bcs     BcsRANGERR
@LEEF5: rts

; 
; Token $2e PEEK
;   uses tken $3f (
; 
PEEK:   jsr     GET16BIT
        lda     (ACC),y
        sty     NOUNSTKC-1,x
        jmp     LE708

; 
; Token $65 ,
;   POKE 20000,5
; 
; Token $67 PLOT
; 
; Token $69 HLIN
; 
; Token $6c VLIN
; 
GETVAL255:
        jsr     GETBYTE
        lda     ACC
        sta     TXTNDX
        rts

; 
; Token $64 POKE
; 
POKE:   jsr     GET16BIT
        lda     TXTNDX
        sta     (ACC),y
        rts

; 
; Token $15 /
;   num op. uses $38 (
;   A = 27 / 2
; 
DIVIDE: jsr     LEE6C
        lda     ACC     ;P3 = ACC
        sta     P3
        lda     ACC+1
        sta     P3+1
        jmp     MultEnd

; 
; Token $44 ,
;   next var in DIM is num
;   DIM X(5),A(5)
; 
; Token $4f DIM
;   num var. uses tkn $22 (
;   DIM A(5)
; 
DIMNUM: jsr     LEEE4
        jmp     LE134

; 
; Token $2d (
;   var array
;   X(12)
; 
TEF24:  jsr     LEEE4
        ldy     NOUNSTKH,x
        lda     NOUNSTKL,x
        adc     #$fe
        bcs     @LEF30
        dey
@LEF30: sta     AUX
        sty     AUX+1
        clc
        adc     ACC
        sta     NOUNSTKL,x
        tya
        adc     ACC+1
        sta     NOUNSTKH,x
        ldy     #$00
        lda     NOUNSTKL,x
        cmp     (AUX),y
        iny
        lda     NOUNSTKH,x
        sbc     (AUX),y
        bcs     BcsRANGERR ;"RANGE"
        jmp     TE823

; 
; Token $2f RND
;   uses tken $3f (
; 
RND:    jsr     GET16BIT
        lda     MON_RNDL
        jsr     LE708
        lda     MON_RNDH
        bne     @LEF5E
        cmp     MON_RNDL
        adc     #$00
@LEF5E: and     #$7f
        sta     MON_RNDH
        sta     NOUNSTKC,x
        ldy     #$11
@LEF66: lda     MON_RNDH
        asl     A
        clc
        adc     #$40
        asl     A
        rol     MON_RNDL
        rol     MON_RNDH
        dey
        bne     @LEF66
        lda     ACC
        jsr     LE708
        lda     ACC+1
        sta     NOUNSTKC,x
        jmp     MOD

unref_ef80:
        jsr     GET16BIT
        ldy     ACC     ;compare ACC and LOMEM
        cpy     LOMEM
        lda     ACC+1
        sbc     LOMEM+1
        bcc     JmpBcsRANGEERR ;[bad idea]
        sty     HIMEM   ;HIMEM = ACC
        lda     ACC+1
        sta     HIMEM+1
unref_branch1:
        jmp     NEW

unref_ef96:
        jsr     GET16BIT
        ldy     ACC     ;compare ACC and LOMEM
        cpy     HIMEM
        lda     ACC+1
        sbc     HIMEM+1
        bcs     JmpBcsRANGEERR
        sty     LOMEM   ;LOMEM = ACC
        lda     ACC+1
        sta     LOMEM+1
        bcc     unref_branch1 ;(always)

JmpBcsRANGEERR:
        jmp     BcsRANGERR

        .byte   $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff

; 
; Token $26 ,
;   string inputs
;   INPUT "WHO",W$
; 
; Token $52 INPUT
;   string with no prompt
;   INPUT S$
; 
TEFB6:  jsr     INPUTSTR
        jmp     LEFBF

; 
; Token $53 INPUT
;   string or num with prompt
;   INPUT "WHO",W$: INPUT "QUANTITY",Q
; 
INPUT_PROMPT:
        jsr     PRINTSTR
LEFBF:  lda     #$ff
        sta     TXTNDX
        lda     #$80
        sta     IN
        rts

LEFC9:  jsr     NOT
        inx
LEFCD:  jsr     NOT
        lda     NOUNSTKL,x
        rts

; Old 4K cold start.
LEFD3:  lda     #$00
        sta     LOMEM   ;LOMEM = $0800
        sta     HIMEM   ;HIMEM = $1000
        lda     #$08
        sta     LOMEM+1
        lda     #$10
        sta     HIMEM+1
        jmp     NEW

LEFE4:  cmp     NOUNSTKH,x
        bne     @LEFE9
        clc
@LEFE9: jmp     LE102

; 
; Token $08 RUN
;   run from first line of program
; 
RUN:    jsr     CLR
        jmp     RUNWARM

; 
; Token $07 RUN
;   RUN 100
; 
RUNNUM: jsr     CLR
        jmp     GOTO

LEFF8:  cpx     #$80
        bne     @LEFFD
        dey
@LEFFD: jmp     LE00C

; 
; Cold start.
;   set LOMEM, find HIMEM
;   fall into NEW
; 
COLD:   ldy     #$00
        sty     NOUNSTKC
        sty     LOMEM   ;LOMEM = $0800
        sty     HIMEM   ;HIMEM = $0800
        lda     #$08
        sta     LOMEM+1
        sta     HIMEM+1
@Loop:  inc     HIMEM+1 ;find top of RAM
        lda     (HIMEM),y
        eor     #$ff
        sta     (HIMEM),y
        cmp     (HIMEM),y
        bne     @LF022
        eor     #$ff
        sta     (HIMEM),y
        cmp     (HIMEM),y
        beq     @Loop   ;(always, unless RAM is broken)

@LF022: jmp     NEW

LF025:  jmp     LF179

unref_f028:
        jsr     from_unref
        jmp     LE8BE

LF02E:  ldx     PX
        lda     PX+1
from_unref:
        ldy     KBD     ;get keypress
        cpy     #ETX+128 ;is it Ctrl+C?
        bne     LF025   ;no
        bit     KBDSTRB ;yes, clear keypress
        stx     NOUNSTKL
        sta     NOUNSTKL+1
        lda     PR      ;NOUNSTKH = PR
        sta     NOUNSTKH
        lda     PR+1
        sta     NOUNSTKH+1
        jmp     STOPPED_AT

        .byte   $ff,$ff

; 
; Token $10 HIMEM:
; 
VHIMEM: jsr     GET16BIT
        stx     XSAVE
        ldx     #$fe
        sec
@Loop1: lda     ACC+2,x ;P2 = ACC
        sta     P2+2,x
        lda     HIMEM+2,x ;AUX = HIMEM - ACC
        sbc     ACC+2,x
        sta     AUX+2,x
        inx
        bne     @Loop1
        bcc     LF0AF
; 
        dex             ;X-reg = $ff
@Loop2: lda     PP+1,x  ;P3 = PP
        sta     P3+1,x
        sbc     AUX+1,x ;P2 = PP - AUX
        sta     P2+1,x
        inx
        beq     @Loop2  ;compare PV to P2
        bcc     JmpMEMFULL
        lda     PV
        cmp     P2
        lda     PV+1
        sbc     P2+1
        bcc     NoInc3
JmpMEMFULL:
        jmp     MEMFULL

Loop3:  lda     (P3),y
        sta     (P2),y
        inc     P2
        bne     @NoInc2
        inc     P2+1
@NoInc2:
        inc     P3
        bne     NoInc3
        inc     P3+1
NoInc3: lda     P3      ;compare P3 and HIMEM
        cmp     HIMEM
        lda     P3+1
        sbc     HIMEM+1
        bcc     Loop3
; 
LF099:  ldx     #$fe
@Loop:  lda     P2+2,x  ;P2 = HIMEM
        sta     HIMEM+2,x
        lda     PP+2,x  ;PP = PP - AUX
        sbc     AUX+2,x
        sta     PP+2,x
        inx
        bne     @Loop
        ldx     XSAVE
        rts

LF0AB:  lda     (HIMEM),y
        sta     (ACC),y
LF0AF:  lda     ACC
        bne     @NoInc
        dec     ACC+1
@NoInc: dec     ACC
        lda     HIMEM
        bne     @NoInc1
        dec     HIMEM+1
@NoInc1:
        dec     HIMEM
        cmp     PP      ;compare PP to HIMEM
        lda     HIMEM+1
        sbc     PP+1
        bcc     LF0AB
        bcs     LF099   ;(always)

; 
; Token $11 LOMEM:
; 
VLOMEM: jsr     GET16BIT
        ldy     ACC
        cpy     #PP
; BUG: above line should be CPY PP
        lda     ACC+1
        sbc     PP+1
BcsJmpMEMFULL:
        bcs     JmpMEMFULL
        sty     LOMEM
        lda     ACC+1
        sta     LOMEM+1
        jmp     CLR

; 
; Token $04 LOAD
; 
LOAD:   stx     XSAVE
        jsr     SETHDR
        jsr     MON_READ
        ldx     #$ff
        sec
@Loop:  lda     HIMEM+1,x ;AUX = HIMEM - ACC
        sbc     ACC+1,x
        sta     AUX+1,x
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
        jsr     SETPRG
        jsr     MON_READ
@LF115: ldx     XSAVE
        rts

@LF118: jsr     MON_BELL
        jmp     @LF115

SETHDR: ldy     #$ce
        sty     MON_A1L ;A1 = $00ce
        iny
        sty     MON_A2L ;A2 = $00cd
        ldy     #$00
        sty     MON_A1H
        sty     MON_A2H
        rts

SETPRG: lda     PP,x
        sta     MON_A1L,x
        ldy     HIMEM,x
        sty     MON_A2L,x
        dex
        bpl     SETPRG
        lda     MON_A2L ;decrement A2
        bne     @LF13D
        dec     MON_A2H
@LF13D: dec     MON_A2L
        rts

unref_f140:
        stx     XSAVE
; 
; Token $05 SAVE
; 
SAVE:   sec             ;ACC = HIMEM - PP
        ldx     #$ff
@Loop:  lda     HIMEM+1,x
        sbc     PP+1,x
        sta     ACC+1,x
        inx
        beq     @Loop
; 
        jsr     SETHDR
        jsr     MON_WRITE
        ldx     #$01
        jsr     SETPRG
        lda     #$1a
        jsr     MON_WRITE0
        ldx     XSAVE
        rts

PRTERR: jsr     ERRORMESS
        jmp     MON_BELL

; 
; Token $77 POP
; 
POP:    lda     GOSUBNDX
        bne     @LF16E
        jmp     RETURN

@LF16E: dec     GOSUBNDX
        rts

; 
; Token $7d TRACE
; 
TRACE:  lda     #$ff
        sta     NOUNSTKC
        rts

; 
; Token $7a NOTRACE
; 
NOTRACE:
        lsr     NOUNSTKC ;clear bit 7
        rts

LF179:  bit     NOUNSTKC ;trace mode?
        bpl     Return5 ;no
LF17D:  lda     #'#' | $80 ;yes, print line number
        jsr     MON_COUT
        ldy     #$01
        lda     (PR),y
        tax
        iny
        lda     (PR),y
        jsr     PRDEC
        lda     #BLANK+128
        jmp     MON_COUT

unref_f192:
        lda     PR
        ldy     PR+1
Return5:
        rts

; 
; Indices into SYNTABL.
; 
SYNTABLNDX:
        .byte   $c1,$00,$7f,$d1,$cc,$c7,$cf,$ce
        .byte   $c5,$9a,$98,$8d,$96,$95,$93,$bf
        .byte   $b2,$32,$12,$0f,$bc,$b0,$ac,$be
        .byte   $35,$0c,$61,$30,$10,$0b,$dd,$fb

LF1B7:  ldy     #$00
        jsr     LE7C7
        lda     #$a0
        jmp     MON_COUT

        .byte   $00,$00,$00,$00,$00,$00,$00,$00

LF1C9:  ldy     LOMEM
        lda     LOMEM+1
@Loop1: pha
        cpy     AUX     ;compare LOMEM to AUX
        sbc     AUX+1
        bcs     @LF1F0
        pla
        sty     SRCH    ;SRCH = LOMEM
        sta     TOKNDXSTK
        ldy     #$ff
@Loop2: iny
        lda     (SRCH),y
        bmi     @Loop2
        cmp     #$40
        beq     @Loop2
        iny
        iny
        lda     (SRCH),y
        pha
        dey
        lda     (SRCH),y
        tay
        pla
        bne     @Loop1
; 
@LF1F0: pla
        ldy     #$00
@LF1F3: lda     (SRCH),y
        bmi     @LF1FC
        lsr     A
        beq     @LF202
        lda     #'$' | $80
@LF1FC: jsr     MON_COUT
        iny
        bne     @LF1F3
@LF202: lda     #'=' | $80
        jmp     MON_COUT

LF207:  sta     (AUX),y
        inx
        lda     NOUNSTKC-1,x
        beq     Return6
        jmp     LF3D5

        .byte   $a0

LF212:  bmi     @LF21B
        lda     PR
        ldy     PR+1
        jsr     LF17D
@LF21B: jsr     LF1C9
        ldx     XSAVE
        jmp     LF1B7

LF223:  inx
        inx
        lda     NOUNSTKC-1,x
        beq     Return7
        jmp     LF3E0

LF22C:  bmi     @LF235
        lda     PR
        ldy     PR+1
        jsr     LF17D
@LF235: jsr     LF1C9
        ldx     XSAVE
        jmp     LF409

        .byte   $e8

Return6:
        rts

LF23F:  jsr     GET16BIT
        inc     ACC
        bne     Return7
        inc     ACC+1
Return7:
        rts

; 
; Token $1c <
;   IF X < 13 THEN END
; 
TF249:  jsr     TF25B
        bne     JmpNOT
; 
; Token $19 >
;   IF X > 13 THEN END
; 
TF24E:  jsr     TF253
        bne     JmpNOT
; 
; Token $1a <=
;   IF X <= 13 THEN END
; 
TF253:  jsr     SUBTRACT
        jsr     NEGATE
        bvc     LF25E
; 
; Token $18 >=
;   IF X >= 13 THEN END
; 
TF25B:  jsr     SUBTRACT
LF25E:  jsr     SGN
        lsr     NOUNSTKL,x
JmpNOT: jmp     NOT

; 
; Token $1d AND
; 
VAND:   jsr     LEFC9
        ora     NOUNSTKL-1,x
        bpl     LF272   ;(always?)
; 
; Token $1e OR
; 
VOR:    jsr     LEFC9
        and     NOUNSTKL-1,x
LF272:  sta     NOUNSTKL,x
        bpl     JmpNOT
        jmp     LEFC9

; 
; Token $58 STEP
; 
STEP:   jsr     GET16BIT
        ldy     FORNDX
        lda     ACC
        sta     STK_60-1,y
        lda     ACC+1
        jmp     LE966

LF288:  sta     STK_50,y
@Loop1: dey
        bmi     @Return
        lda     STK_40,y
        cmp     NOUNSTKL,x
        bne     @Loop1
        lda     STK_50,y
        cmp     NOUNSTKH,x
        bne     @Loop1
        dec     FORNDX
@Loop2: lda     STK_40+1,y
        sta     STK_40,y
        lda     STK_50+1,y
        sta     STK_50,y
        lda     STK_C0+1,y
        sta     STK_C0,y
        lda     STK_D0+1,y
        sta     STK_D0,y
        lda     STK_60+1,y
        sta     STK_60,y
        lda     STK_70+1,y
        sta     STK_70,y
        lda     STK_80+1,y
        sta     STK_80,y
        lda     STK_90+1,y
        sta     STK_90,y
        lda     STK_A0+1,y
        sta     STK_A0,y
        lda     STK_A0+1,y
        sta     STK_A0,y
; BUG: above two lines should be:
;   LDA STK_B0+1,Y
;   STA STK_B0,Y
        iny
        cpy     FORNDX
        bcc     @Loop2
@Return:
        rts

; 
; Token $78 NODSP
;   string var
; 
NODSP_STR:
        inx
; 
; Token $79 NODSP
;   num var
; 
NODSP_NUM:
        lda     #$00
LF2E3:  pha
        lda     NOUNSTKL,x
        sec
        sbc     #$03
        sta     ACC
        lda     NOUNSTKH,x
        sbc     #$00
        sta     ACC+1
        pla
        ldy     #$00
        sta     (ACC),y
        inx
        rts

LF2F8:  cmp     #$85
        bcs     @LF2FF
        jmp     LE4C0

@LF2FF: ldy     #$02
        jmp     LE448

; 
; Token $7b DSP
;   string var
; 
DSP_STR:
        inx             ;[DSP_NUM in paulrsm disasm]
; 
; Token $7c DSP
;   num var
; 
DSP_NUM:
        lda     #$01    ;[DSP_STR in paulrsm disasm]
        bne     LF2E3   ;(always)

        .byte   $e8

; 
; Token $06 CON
; 
CON:    lda     NOUNSTKH ;PR = NOUNSTKH
        sta     PR
        lda     NOUNSTKH+1
        sta     PR+1
        lda     NOUNSTKL
        ldy     NOUNSTKL+1
        jmp     GETNEXT

unref_f319:
        lda     #$01
        bne     LF2E3

; 
; Token $3c ASC(
; 
ASC:    lda     NOUNSTKL,x
        cmp     NOUNSTKH,x
        bcc     @LF326
        jmp     RANGERR

@LF326: tay
        lda     NOUNSTKL+1,x
        sta     ACC
        lda     NOUNSTKH+1,x
        sta     ACC+1
        lda     (ACC),y
        ldy     #$00
        inx
        inx
        jsr     LE708
        jmp     LF404

; 
; Token $32 PDL
; 
PDL:    jsr     GETBYTE
        stx     XSAVE
        and     #$03
        tax
        jsr     MON_PREAD
        ldx     XSAVE
        tya
        ldy     #$00
        jsr     LE708
        sty     NOUNSTKC,x
        rts

RDKEY:  jsr     MON_NXTCHAR
LF354:  txa
        pha
@Loop:  lda     IN,x
        cmp     #ETX+128 ;is it Ctrl+C?
        bne     @LF360
        jmp     BASIC2

@LF360: dex
        bpl     @Loop
        pla
        tax
        rts

LF366:  jsr     LE280
        tya
        tax
        jsr     LF354
        txa
        tay
        rts

; 
; Token $20 ^
; 
EXP:    jsr     GET16BIT
        lda     ACC+1
        bpl     @LF380
        tya             ;A-reg = 0
        dex
        jsr     LE708
        sty     NOUNSTKC,x
@Return:
        rts

@LF380: sta     SRCH+1  ;SRCH = ACC
        lda     ACC
        sta     SRCH
        jsr     GET16BIT
        lda     ACC     ;SRCH2 = ACC
        sta     SRCH2
        lda     ACC+1
        sta     SRCH2+1
        lda     #$01
        jsr     LE708
        sty     NOUNSTKC,x
; 
@Loop:  lda     SRCH    ;srch = SRCH - 1
        bne     @NoDec
        dec     SRCH+1  ;is SRCH negative?
        bmi     @Return ;yes, return
@NoDec: dec     SRCH
        lda     SRCH2
        ldy     #$00
        jsr     LE708
        lda     SRCH2+1
        sta     NOUNSTKC,x
        jsr     MULT
        jmp     @Loop

LF3B3:  jsr     GETBYTE
        clc             ;A-reg = A-reg - 1
        adc     #$ff
Return8:
        rts

; 
; Token $4a ,
;   end of PRINT statement
;   PRINT A$,
; 
TF3BA:  jsr     LE7B1
        lsr     CRFLAG  ;pos
        rts

unref_f3c0:
        stx     RUNFLAG
        txs
        jsr     LF02E
        jmp     LE883

; 
; Token $7e PR#
; 
PRSLOT: jsr     GETBYTE
        stx     XSAVE
        jsr     MON_OUTPORT
        ldx     XSAVE
        rts

        .byte   $fe

LF3D5:  bit     RUNFLAG
        bpl     Return8
        stx     XSAVE
        bit     NOUNSTKC
        jmp     LF212

LF3E0:  bit     RUNFLAG
        bpl     Return8
        stx     XSAVE
        bit     NOUNSTKC
        jmp     LF22C

LF3EB:  ldy     #$00
        jmp     GETVERB

Loop4:  tay
        jsr     MON_CROUT
LF3F4:  tya
        sec
        sbc     MON_WNDWDTH
        bcs     Loop4
        sty     MON_CH
        rts

        .byte   $00,$00,$00,$ff,$ff,$ff,$ff

LF404:  sty     NOUNSTKC,x
        jmp     TE823

LF409:  ldy     #$00
        beq     @LF411  ;(always)

@Loop:  jsr     MON_COUT
        iny
@LF411: lda     (AUX),y
        bmi     @Loop
        lda     #$ff
        sta     CRFLAG  ;CRFLAG = $ff
        rts

; 
; Token $7f IN#
; 
INSLOT: jsr     GETBYTE
        stx     XSAVE
        jsr     MON_INPORT
        ldx     XSAVE
        rts

        .byte   $18,$a2,$02,$b5,$f9,$75,$f5,$95,$f9,$ca,$10,$f7,$60,$06,$f3,$20 ;binary ends at $f7ff
        .byte   $37,$f4,$24,$f9,$10,$05,$20,$a4,$f4,$e6,$f3,$38,$a2,$04,$94,$fb
        .byte   $b5,$f7,$b4,$f3,$94,$f7,$95,$f3,$ca,$d0,$f3,$60,$a9,$8e,$85,$f8
        .byte   $a5,$f9,$c9,$c0,$30,$0c,$c6,$f8,$06,$fb,$26,$fa,$26,$f9,$a5,$f8
        .byte   $d0,$ee,$60,$20,$a4,$f4,$20,$7b,$f4,$a5,$f4,$c5,$f8,$d0,$f7,$20
        .byte   $25,$f4,$50,$ea,$70,$05,$90,$c4,$a5,$f9,$0a,$e6,$f8,$f0,$75,$a2
        .byte   $fa,$76,$ff,$e8,$d0,$fb,$60,$20,$32,$f4,$65,$f8,$20,$e2,$f4,$18
        .byte   $20,$84,$f4,$90,$03,$20,$25,$f4,$88,$10,$f5,$46,$f3,$90,$bf,$38
        .byte   $a2,$03,$a9,$00,$f5,$f8,$95,$f8,$ca,$d0,$f7,$f0,$c5,$20,$32,$f4
        .byte   $e5,$f8,$20,$e2,$f4,$38,$a2,$02,$b5,$f5,$f5,$fc,$48,$ca,$10,$f8
        .byte   $a2,$fd,$68,$90,$02,$95,$f8,$e8,$d0,$f8,$26,$fb,$26,$fa,$26,$f9
        .byte   $06,$f7,$26,$f6,$26,$f5,$b0,$1c,$88,$d0,$da,$f0,$be,$86,$fb,$86
        .byte   $fa,$86,$f9,$b0,$0d,$30,$04,$68,$68,$90,$b2,$49,$80,$85,$f8,$a0
        .byte   $17,$60,$10,$f7,$4c,$f5,$03,$ff,$ff,$ff,$ff,$e9,$81,$4a,$d0,$14
        .byte   $a4,$3f,$a6,$3e,$d0,$01,$88,$ca,$8a,$18,$e5,$3a,$85,$3e,$10,$01
        .byte   $c8,$98,$e5,$3b,$d0,$6b,$a4,$2f,$b9,$3d,$00,$91,$3a,$88,$10,$f8
        .byte   $20,$1a,$fc,$20,$1a,$fc,$20,$d0,$f8,$20,$53,$f9,$84,$3b,$85,$3a
        .byte   $4c,$95,$f5,$20,$be,$ff,$a4,$34,$20,$a7,$ff,$84,$34,$a0,$17,$88
        .byte   $30,$4b,$d9,$cc,$ff,$d0,$f8,$c0,$15,$d0,$e8,$a5,$31,$a0,$00,$c6
        .byte   $34,$20,$00,$fe,$4c,$95,$f5,$a5,$3d,$20,$8e,$f8,$aa,$bd,$00,$fa
        .byte   $c5,$42,$d0,$13,$bd,$c0,$f9,$c5,$43,$d0,$0c,$a5,$44,$a4,$2e,$c0
        .byte   $9d,$f0,$88,$c5,$2e,$f0,$9f,$c6,$3d,$d0,$dc,$e6,$44,$c6,$35,$f0
        .byte   $d6,$a4,$34,$98,$aa,$20,$4a,$f9,$a9,$de,$20,$ed,$fd,$20,$3a,$ff
        .byte   $a9,$a1,$85,$33,$20,$67,$fd,$20,$c7,$ff,$ad,$00,$02,$c9,$a0,$f0
        .byte   $13,$c8,$c9,$a4,$f0,$92,$88,$20,$a7,$ff,$c9,$93,$d0,$d5,$8a,$f0
        .byte   $d2,$20,$78,$fe,$a9,$03,$85,$3d,$20,$34,$f6,$0a,$e9,$be,$c9,$c2
        .byte   $90,$c1,$0a,$0a,$a2,$04,$0a,$26,$42,$26,$43,$ca,$10,$f8,$c6,$3d
        .byte   $f0,$f4,$10,$e4,$a2,$05,$20,$34,$f6,$84,$34,$dd,$b4,$f9,$d0,$13
        .byte   $20,$34,$f6,$dd,$ba,$f9,$f0,$0d,$bd,$ba,$f9,$f0,$07,$c9,$a4,$f0
        .byte   $03,$a4,$34,$18,$88,$26,$44,$e0,$03,$d0,$0d,$20,$a7,$ff,$a5,$3f
        .byte   $f0,$01,$e8,$86,$35,$a2,$03,$88,$86,$3d,$ca,$10,$c9,$a5,$44,$0a
        .byte   $0a,$05,$35,$c9,$20,$b0,$06,$a6,$35,$f0,$02,$09,$80,$85,$44,$84
        .byte   $34,$b9,$00,$02,$c9,$bb,$f0,$04,$c9,$8d,$d0,$80,$4c,$5c,$f5,$b9
        .byte   $00,$02,$c8,$c9,$a0,$f0,$f8,$60,$20,$7d,$f4,$a5,$f8,$10,$13,$c9
        .byte   $8e,$d0,$f5,$24,$f9,$10,$0a,$a5,$fb,$f0,$06,$e6,$fa,$d0,$02,$e6
        .byte   $f9,$60,$a9,$00,$85,$f9,$85,$fa,$60,$ff,$ff,$ff,$ff,$ff,$ff,$ff
        .byte   $ff,$4c,$92,$f5,$84,$58,$86,$57,$85,$56,$08,$68,$85,$59,$ba,$e8
        .byte   $e8,$bd,$00,$01,$0a,$0a,$0a,$0a,$60,$a4,$58,$a6,$57,$a5,$59,$48
        .byte   $a5,$56,$28,$60,$20,$4a,$ff,$68,$85,$1e,$68,$85,$1f,$20,$98,$f6
        .byte   $4c,$92,$f6,$e6,$1e,$d0,$02,$e6,$1f,$a9,$f7,$48,$a0,$00,$b1,$1e
        .byte   $29,$0f,$0a,$aa,$4a,$51,$1e,$f0,$0b,$86,$1d,$4a,$4a,$4a,$a8,$b9
        .byte   $e1,$f6,$48,$60,$e6,$1e,$d0,$02,$e6,$1f,$bd,$e4,$f6,$48,$a5,$1d
        .byte   $4a,$60,$68,$68,$20,$3f,$ff,$6c,$1e,$00,$b1,$1e,$95,$01,$88,$b1
        .byte   $1e,$95,$00,$98,$38,$65,$1e,$85,$1e,$90,$02,$e6,$1f,$60,$02,$f9
        .byte   $04,$9d,$0d,$9e,$25,$af,$16,$b2,$47,$b9,$51,$c0,$2f,$c9,$5b,$d2
        .byte   $85,$dd,$6e,$05,$33,$e8,$70,$93,$1e,$e7,$65,$e7,$e7,$e7,$10,$ca
        .byte   $b5,$00,$85,$00,$b5,$01,$85,$01,$60,$a5,$00,$95,$00,$a5,$01,$95
        .byte   $01,$60,$a5,$00,$81,$00,$a0,$00,$84,$1d,$f6,$00,$d0,$02,$f6,$01
        .byte   $60,$a1,$00,$85,$00,$a0,$00,$84,$01,$f0,$ed,$a0,$00,$f0,$06,$20
        .byte   $66,$f7,$a1,$00,$a8,$20,$66,$f7,$a1,$00,$85,$00,$84,$01,$a0,$00
        .byte   $84,$1d,$60,$20,$26,$f7,$a1,$00,$85,$01,$4c,$1f,$f7,$20,$17,$f7
        .byte   $a5,$01,$81,$00,$4c,$1f,$f7,$20,$66,$f7,$a5,$00,$81,$00,$4c,$43
        .byte   $f7,$b5,$00,$d0,$02,$d6,$01,$d6,$00,$60,$a0,$00,$38,$a5,$00,$f5
        .byte   $00,$99,$00,$00,$a5,$01,$f5,$01,$99,$01,$00,$98,$69,$00,$85,$1d
        .byte   $60,$a5,$00,$75,$00,$85,$00,$a5,$01,$75,$01,$a0,$00,$f0,$e9,$a5
        .byte   $1e,$20,$19,$f7,$a5,$1f,$20,$19,$f7,$18,$b0,$0e,$b1,$1e,$10,$01
        .byte   $88,$65,$1e,$85,$1e,$98,$65,$1f,$85,$1f,$60,$b0,$ec,$60,$0a,$aa
        .byte   $b5,$01,$10,$e8,$60,$0a,$aa,$b5,$01,$30,$e1,$60,$0a,$aa,$b5,$00
        .byte   $15,$01,$f0,$d8,$60,$0a,$aa,$b5,$00,$15,$01,$d0,$cf,$60,$0a,$aa
        .byte   $b5,$00,$35,$01,$49,$ff,$f0,$c4,$60,$0a,$aa,$b5,$00,$35,$01,$49
        .byte   $ff,$d0,$b9,$60,$a2,$18,$20,$66,$f7,$a1,$00,$85,$1f,$20,$66,$f7
        .byte   $a1,$00,$85,$1e,$60,$4c,$c7,$f6,$f6,$ff,$ff
