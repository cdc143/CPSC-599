  ;This test tests basic keyboard input
  ;Oct 9, 2016
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
 ldx #$01		 ; load 1 into x reg
top:
  lda $00c5		 ; current key held down -> page 179 of vic20 manual
  cmp #17		 ;a pressed
  beq goleft		 
  cmp #18		 ;d pressed
  beq goright		
  cmp #9		 ;w pressed
  beq goup		
  cmp #41		 ;s pressed
  beq godown		
  bne next
  
;all these branches jump to their respective print-to-screen branch
;and then go to the branch "next"
goleft:
  jsr left
  cpx #$00		; basically a branch always scenario because X is 1
  bne next
  
goright:
  jsr right
  cpx #$00
  bne next
  
goup:
  jsr up
  cpx #$00
  bne next
  
godown:
  jsr down
  
next:
  cmp #48		 ; check if Q is pressed -> quit
  bne top		 ;continue input
  rts			 ; quit
  
;These subroutines print the next letter of W,A,S,D (X,B,T,E) to make sure
;that we aren't just seeing W,A,S,D being typed without the code 
;working.
left:
  lda #66		 ; B
  jsr $ffd2	
  rts			 ; done
  
right:
  lda #69		 ; D
  jsr $ffd2
  rts			 ; done
  
up:
  lda #88		 ; X
  jsr $ffd2
  rts			 ; done
  
down:
  lda #84		 ; T
  jsr $ffd2
  rts			 ; done