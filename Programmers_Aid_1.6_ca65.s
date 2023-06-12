; Target assembler: cc65 v2.18.0 [--target none -C Apple II Programmers Aid 1.6.$D717~$D7FF - Music.BIN_cc65.cfg]
; ==============================================================================
; Apple II [$D0 ROM] (341-0016) - Programmer's Aid #1 [1978]
; ------------------------------------------------------------------------------
; Part 6 [$D717~$D7FF]: Music Subroutine by Gary J. Shannon;
; Copyright (c) 1978 by Apple Computer Inc.  All Rights Reserved
; ------------------------------------------------------------------------------
; Instructions are in the Programmer's Aid #1 Installation and Operating Manual
; ==============================================================================
; Analyzed (via McFadden's SourceGen) by James Davis  [Last Updated: 2020-07-10]
; ==============================================================================
; 
; ==============================================================================
; Music Routine 6502 Equates: Zero Page Work Areas; & Parameter Passing Areas
; ==============================================================================
; 
        .setcpu "6502"
DOWNTIME =      $00     ;Speaker Negative Pulse Width
UPTIME  =       $01     ;Speaker Positive Pulse Width
DURATION =      $02     ;Musical Note Time-Duration Counter
TIMBRE  =       $02fd   ;Musical Note Timbre Value (Poke 765)
TIME    =       $02fe   ;Musical Note Time Value   (Poke 766)
PITCH   =       $02ff   ;Musical Note Pitch Value  (Poke 767)
SPEAKER =       $c030   ;Speaker Data Output Toggle Switch

; 
; ==============================================================================
; Music Subroutine by Gary J. Shannon    [Do Pokes, then Call -10473 from BASIC]
; ==============================================================================
; DURATION   EQU   $02     ;Renamed to match Instructions; Original Name: LENGTH
; TIMBRE     EQU   $02FD   ;Renamed to match Instructions; Original Name: VOICE
; TIME       EQU   $02FE   ;Renamed to match Instructions; Original Name: LONG
; PITCH      EQU   $02FF   ;Renamed to match Instructions; Original Name: NOTE
; ------------------------------------------------------------------------------
; 

        ;;         .org    $d717
ENTRY:  jmp     LOOKUP  ;Get Pulse Widths (Duty Cycle Data)

; ------------------------------------------------------------------------------
; Play One Note: Musical Note Cycles are divided into UPTIME & DOWNTIME halves;
;                Musical Note Duty Cycle Data is from NOTES: UPTIME & DOWNTIME;
;                Musical Note Time-Duration Count is kept in DURATION
; ------------------------------------------------------------------------------
; 
; ----------------------------------- ;UPTIME Half-Cycle:
PLAY1:  ldy     UPTIME  ;Get Positive Pulse Width
        lda     SPEAKER ;Toggle Speaker Data Output
PLAY2:  inc     DURATION ;Advance Note Time-Duration Counter, Low
        bne     PATH1   ;Branch if Duration (Low) is Not Expired
        inc     DURATION+1 ;Advance Note Time-Duration Counter, High
        bne     PATH2   ;Branch if Duration (High) is Not Expired
        rts             ;Return to Caller; Time-Duration Expired

;                                     ;Do Time Adjustments:
PATH1:  nop             ;Delay (2 Machine Cycles)
        jmp     PATH2   ;Delay (3 Machine Cycles) More

PATH2:  dey             ;Reduce Pulse Counter (Width)
        beq     PLAY0   ;Toggle if Pulse Count (Width) is Expired
        jmp     PATH3   ;Continue UPTIME if Count is Not Expired

;                                     ;Jump Delays (3 Machine Cycles) More
PATH3:  bne     PLAY2   ;UPTIME: Same # of Cyles; Always Taken

; ----------------------------------- ;DOWNTIME Half-Cycle:
PLAY0:  ldy     DOWNTIME ;Get Negative Pulse Width
        lda     SPEAKER ;Toggle Speaker Data Output
PLAY3:  inc     DURATION ;Advance Note Time-Duration Counter, Low
        bne     PATH4   ;Branch if Duration (Low) is Not Expired
        inc     DURATION+1 ;Advance Note Time-Duration Counter, High
        bne     PATH5   ;Branch if Duration (High) is Not Expired
        rts             ;Return to Caller; Time-Duration Expired

;                                     ;Do Time Adjustments:
PATH4:  nop             ;Delay (2 Machine Cycles)
        jmp     PATH5   ;Delay (3 Machine Cycles) More

PATH5:  dey             ;Reduce Pulse Counter (Width)
        beq     PLAY1   ;Toggle if Pulse Count (Width) is Expired
        jmp     PATH6   ;Continue DOWNTIME if Count is Not Expired

;                                     ;Jump Delays (3 Machine Cycles) More
PATH6:  bne     PLAY3   ;DOWNTIME: Same # of Cyles; Always Taken

; ------------------------------------------------------------------------------
; Note Table Lookup Subroutine: Gets a Note's UPTIME & DOWNTIME from its PITCH &
;                               Sets its DURATION in accordance with its TIMBRE
; ------------------------------------------------------------------------------
; DURATION   EQU    $02               ;Musical Note Time-Duration Counter
; TIMBRE     EQU    $02FD             ;Musical Note Timbre Value (Poke 765)
; TIME       EQU    $02FE             ;Musical Note Time Value   (Poke 766)
; PITCH      EQU    $02FF             ;Musical Note Pitch Value  (Poke 767)
; ------------------------------------------------------------------------------
LOOKUP: lda     PITCH   ;Get Musical Note Pitch Value (User Poked)
        asl     A       ;Double Musical Note Pitch Value (1 of 2)
        tay             ;Set Indexed Addressing Pointer
        lda     NOTES,y ;Get Note  UPTIME  (Positive Pulse Width)=
        sta     DOWNTIME ;Set Note DOWNTIME (Negative Pulse Width)=
; ----------------------------------- ;Shift Time According to Note's TIMBRE:
        lda     TIMBRE  ;Get Note's Timbre Value (User Poked)
SHIFT:  lsr     A       ;Halve Note Timbre Value (Duty Cycle)
        beq     DONE    ;Exit if Halving Results in a Zero Value
        lsr     DOWNTIME ;Else, Halve Negative Pulse Width, too
        bne     SHIFT   ;Loop if Halving Result is Not Zero yet
; ----------------------------------- ;Compute New UPTIME/DOWNTIME Pulse Widths:
DONE:   lda     NOTES,y ;Get Original Note's Positive Pulse Width
;                                     ;Compute Difference:
        sec             ;Prep to Subtract w/o Borrow [A-Data-!C]
        sbc     DOWNTIME ;Subtract Shifted Negative Pulse Width
        sta     UPTIME  ;*** Set New Positive Pulse Width ***
        iny             ;Advance Indexed Addressing Pointer
        lda     NOTES,y ;Get Original Note's Negative Pulse Width
;                                     ;Add Difference [Add w/ Carry: A+Data+C]:
        adc     DOWNTIME ;Add Shifted Negative Pulse Width
        sta     DOWNTIME ;*** Set New Negative Pulse Width ***
; ----------------------------------- ;Compute Compliment of Duration Count:
        lda     #0      ;Prepare to Subtract from Zero
        sec             ;Prep to Subtract w/o Borrow [A-Data-!C]
        sbc     TIME    ;Subtract Musical Note Time Value
        sta     DURATION+1 ;Set Note Time-Duration Counter, High
        lda     #0      ;Clear Accumulator
        sta     DURATION ;Set Note Time-Duration Counter, Low
; ----------------------------------- ;Prepare to Play Musical Note:
        lda     UPTIME  ;Get Positive Pulse Width
        bne     PLAY1   ;If Note is Not a Rest Note, Play it ...
; 
; ------------------------------------------------------------------------------
; Rest Note Subroutine: Plays Note #0 Silently, with same/regular Note Durations
; ------------------------------------------------------------------------------
; 
; Rest Note Iterations: Do UPTIME 1st, DOWNTIME 2nd; == Same # of UP/DOWN Cyles
; 
;                                     ;Do Time Adjustments:
REST:   nop             ;Delay (2 Machine Cycles)
        nop             ;Delay (2 Machine Cycles) More
        jmp     REST2   ;Delay (3 Machine Cycles) More

REST2:  inc     DURATION ;Advance Note Time-Duration Counter, Low
        bne     REST3   ;Branch if Duration (Low) is Not Expired
        inc     DURATION+1 ;Advance Note Time-Duration Counter, High
        bne     REST4   ;Branch if Duration (High) is Not Expired
        rts             ;Return to Caller; Time-Duration Expired

REST3:  nop             ;Delay (2 Machine Cycles)
        jmp     REST4   ;Delay (3 Machine Cycles) More

REST4:  bne     REST    ;Do DOWNTIME: Same Cyle; Always Taken

; ------------------------------------------------------------------------------
; Notes Table: Values are in UP/DOWN Pairs; e.g., 1st Note is a Rest [#0 =(0,0)]
; ------------------------------------------------------------------------------
; 
NOTES:  .byte   $00,$00 ;Note #00: A Rest (A Silence, Not a Pitch)
; Chromatic Octave: "Contra" [Below Bass Clef] - [Low End of Partial Octave]
        .byte   $f6,$f6 ;Note #01: Contra F or (uppercase) FF
        .byte   $e8,$e8 ;Note #02: Contra F# (F-sharp or G-flat)
        .byte   $db,$db ;Note #03: Contra G or (uppercase) GG
        .byte   $cf,$cf ;Note #04: Contra G# (G-sharp or A-flat)
        .byte   $c3,$c3 ;Note #05: Contra A or (uppercase) AA
        .byte   $b8,$b8 ;Note #06: Contra A# (A-sharp or B-flat)
        .byte   $ae,$ae ;Note #07: Contra B or (uppercase) BB
; Chromatic Octave: "Great" [Bottom of & below Bass Clef]
        .byte   $a4,$a4 ;Note #08: Great C or (uppercase) C
        .byte   $9b,$9b ;Note #09: Great C# (C-sharp or D-flat)
        .byte   $92,$92 ;Note #10: Great D or (uppercase) D
        .byte   $8a,$8a ;Note #11: Great D# (D-sharp or E-flat)
        .byte   $82,$82 ;Note #12: Great E or (uppercase) E
        .byte   $7b,$7b ;Note #13: Great F or (uppercase) F
        .byte   $74,$74 ;Note #14: Great F# (F-sharp or G-flat)
        .byte   $6d,$6e ;Note #15: Great G or (uppercase) G
        .byte   $67,$68 ;Note #16: Great G# (G-sharp or A-flat)
        .byte   $61,$62 ;Note #17: Great A or (uppercase) A
        .byte   $5c,$5c ;Note #18: Great A# (A-sharp or B-flat)
        .byte   $57,$57 ;Note #19: Great B or (uppercase) B
; Chromatic Octave: "Small" [Top of Bass Clef] (letters may be lowercase)
        .byte   $52,$52 ;Note #20: Small C or (lowercase) c
        .byte   $4d,$4e ;Note #21: Small C# (C-sharp or D-flat)
        .byte   $49,$49 ;Note #22: Small D or (lowercase) d
        .byte   $45,$45 ;Note #23: Small D# (D-sharp or E-flat)
        .byte   $41,$41 ;Note #24: Small E or (lowercase) e
        .byte   $3d,$3e ;Note #25: Small F or (lowercase) f
        .byte   $3a,$3a ;Note #26: Small F# (F-sharp or G-flat)
        .byte   $36,$37 ;Note #27: Small G or (lowercase) g
        .byte   $33,$34 ;Note #28: Small G# (G-sharp or A-flat)
        .byte   $30,$31 ;Note #29: Small A or (lowercase) a
        .byte   $2e,$2e ;Note #30: Small A# (A-sharp or B-flat)
        .byte   $2b,$2c ;Note #31: Small B or (lowercase) b
; Chromatic Octave: "One-Line" [Most of Treble Clef] (letters may be lowercase)
        .byte   $29,$29 ;Note #32: One-line C, C-one, or C^1
;                                     ;^[Middle C is half-way between the Clefs]
        .byte   $26,$27 ;Note #33: One-line C# (C-sharp or D-flat)
        .byte   $24,$25 ;Note #34: One-line D, D-one, or D^1
        .byte   $22,$23 ;Note #35: One-line D# (D-sharp or E-flat)
        .byte   $20,$21 ;Note #36: One-line E, E-one, or E^1
        .byte   $1e,$1f ;Note #37: One-line F, F-one, or F^1
        .byte   $1d,$1d ;Note #38: One-line F# (F-sharp or G-flat)
        .byte   $1b,$1c ;Note #39: One-line G, G-one, or G^1
        .byte   $1a,$1a ;Note #40: One-line G# (G-sharp or A-flat)
        .byte   $18,$19 ;Note #41: One-line A, A-one, or A^1
        .byte   $17,$17 ;Note #42: One-line A# (A-sharp or B-flat)
        .byte   $15,$16 ;Note #43: One-line B, B-one, or B^1
; Chromatic Octave: "Two-Line" [Top of & above Treble Clef] (may be lowercase)
        .byte   $14,$15 ;Note #43: Two-line C, C-two, or C^2
        .byte   $13,$14 ;Note #33: Two-line C# (C-sharp or D-flat)
        .byte   $12,$12 ;Note #46: Two-line D, D-two, or D^2
        .byte   $11,$11 ;Note #47: Two-line D# (D-sharp or E-flat)
        .byte   $10,$10 ;Note #48: Two-line E, E-two, or E^2
        .byte   $0f,$10 ;Note #49: Two-line F, F-two, or F^2
        .byte   $0e,$0f ;Note #50: Two-line F# (F-sharp or G-flat)
; ----------------------------------- ;^[High End of Partial Octave]
.ifdef INCLUDE_DEAD_CODE
        .byte   $ff,$ff ;Junk Bytes
        .byte   $ff,$ff ;Junk Bytes
.endif
