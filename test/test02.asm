  ;This test prints F 4 times onto the screen
  processor 6502
  org $1001               ; Unexpanded VIC

 ; BASIC stub (unexpanded vic)
 dc.w $100b              ; Pointer to next BASIC line
 dc.w 1981               ; BASIC Line#
 dc.b $9e                ; BASIC SYS token
 dc.b $34,$31,$30,$39    ; 4109 (ML start)
 dc.b 0                  ; End of BASIC line
 dc.w 0                  ; End of BASIC program

 jsr $e55f       ; clear screen
 ldx #$04		 ; load 4 into x reg
top:			 ;needs to be all the way to left, no spaces 
  lda #70		 ; F
  jsr $ffd2
  dex			 ; x-1
  bne top		 ; loop to top
  rts			 ; done