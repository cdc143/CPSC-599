  ;This test changes the text colour and border/background
  processor 6502
  org $1001               ; Unexpanded VIC

 ; BASIC stub (unexpanded vic)
 dc.w $100b              ; Pointer to next BASIC line
 dc.w 1981               ; BASIC Line#
 dc.b $9e                ; BASIC SYS token
 dc.b $34,$31,$30,$39    ; 4109 (ML start)
 dc.b 0                  ; End of BASIC line
 dc.w 0                  ; End of BASIC program


write_text:

  ;Clear the screen

  lda #$93
  jsr $ffd2
  lda #$0d
  jsr $ffd2
  jsr $ffd2

  ;Setup new location for characters
  lda $9005
  pha
  ora #$0f
  sta $9005
  ldx #$01
	stx $1e00
	inx
	stx $1e01
	inx
	stx $1e16
	inx
	stx $1e17
  ldx #$0
	stx $9600
	stx $9601
	stx $9616
	stx $9617

  ;Wait for user to press enter, and restore the character set
  jsr $ffcf
  pla
	sta $9005
  rts

modchars:
  	.dsb $1c00 - modchars, 0
  	.byt 0, 0, 0, 0, 0, 0, 0, 0
  	.byt #60, #66, #165, #129, #165, #153, #66, #60
  	.byt #60, #66, #165, #129, #165, #153, #66, #60
    .byt #60, #66, #165, #129, #165, #153, #66, #60
    .byt #60, #66, #165, #129, #165, #153, #66, #60
