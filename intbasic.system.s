        .include "apple2.inc"

A1L     =       $3c     ;general purpose
A1H     =       $3d     ;general purpose
A2L     =       $3e     ;general purpose
A2H     =       $3f     ;general purpose
A4L     =       $42     ;general purpose
A4H     =       $43     ;general purpose
MOVE    =       $FE2C

        .org $2000


        ;; TODO: Adjust system bitmap


        ;; Relocate up to target
        target := $A000
        lda     #<impl
        sta     A1L
        lda     #>impl
        sta     A1H

        lda     #<(impl+sizeof_impl-1)
        sta     A2L
        lda     #>(impl+sizeof_impl-1)
        sta     A2H

        lda     #<target
        sta     A4L
        lda     #>target
        sta     A4H

        ldy     #0
        jsr     MOVE

        jmp     target



        ;; NOTE: Org set to $A000
        .proc impl
        .include "IntegerBASIC_cc65.s"
        .endproc
        sizeof_impl = .sizeof(impl)
