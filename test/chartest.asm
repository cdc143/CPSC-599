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
  lda leftpose,x
  sta $1c00,x
  inx
  cpx #$50
  bne copyLoop
  ldx #$00
copyNums:
  lda $8180,x
  sta $1c58,x
  inx
  cpx #$50
  bne copyNums
  ldx #$00
copyAlpha:
  lda $8008,x
  sta $1ca8,x
  inx
  cpx #$d0
  bne copyAlpha
  lda #$ff
  sta $9005
  jsr $e55f ; clear screen
  jsr $ffcf ; wait for enter to be clicked

leftpose: dc.b #00,#60,#60,#00,#216,#24,#24,#00
rightpose: dc.b #00,#60,#60,$00,#27,#24,#24,#00
downpose: dc.b #00,#60,#60,#00,#88,#88,#88,#64
uppose:   dc.b #64,#92,#92,#64,#24,#24,#24,#00
space:    dc.b #00,#00,#00,#00,#00,#00,#00,#00
wall:     dc.b #255,#255,#255,#255,#255,#255,#255,#255
portal:   dc.b #240,#240,#240,#240,#15,#15,#15,#15
sword:    dc.b #24,#24,#24,#24,#24,#00,#24,#24
heart:    dc.b $36,$7f,$7f,$7f,$3e,$1c,$08,$00
door:     dc.b $81,$42,$24,$18,$18,$24,$42,$81
enemy:    dc.b $00,$3c,$42,$42,$42,$42,$3c,$00
