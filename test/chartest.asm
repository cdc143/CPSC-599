;Oct 24, 2016
  processor 6502
  org $1001              ; Unexpanded VIC

; BASIC stub (unexpanded vic)
  dc.w $100b            ; Pointer to next BASIC line
  dc.w 1981               ; BASIC Line#
  dc.b $9e                ; BASIC SYS token
  dc.b $34,$31,$30,$39    ; 4109 (ML start)
  dc.b 0                  ; End of BASIC line
  dc.w 0                  ; End of BASIC program

  ldx #$00
copyLoop:
  lda smiley,x
  sta $1c08,x
  inx
  cpx #$08
  bne copyLoop

  lda #$ff
  sta $9005
  jsr $e55f
  jsr $ffcf
  rts

smiley: dc.b #60,#66,#165,#129,#165,#153,#66,#60
