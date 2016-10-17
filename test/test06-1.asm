;This test tests basic sound 
  ; vic pages used (page 98-102, 175)
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
 lda #$0f		 ; 15
 sta $900e		; set sound bits/turn on volume (see)

 ;note: $900a - low freq
 ; $900b - med freq
 ; $900c - high freq
 
top:
  ldy #$0f		 ; duration of sound (15 loops)
  lda $00c5		 ; current key held down -> page 179 of vic20 manual
  cmp #17		 ;a pressed
  beq playleft		 
  cmp #18		 ;d pressed
  beq playright		
  cmp #9		 ;w pressed
  beq playup		
  cmp #41		 ;s pressed
  beq playdown		
  bne next
  
;all these branches jump to their respective print-to-screen branch
;and then play to the branch "next"
playleft:
  jsr turnoff
  jsr left
  jmp next
  
playright:
  jsr turnoff
  jsr right
  jmp next
  
playup:
  jsr turnoff
  jsr up
  jmp next
  
playdown:
  jsr turnoff
  jsr down
  
next:
  cmp #48		 ; check if Q is pressed -> quit
  bne top		 ;continue input
  jsr turnoff
  rts			 ; quit
  
;These subroutines print the next letter of W,A,S,D (X,B,T,E) to make sure
;that we aren't just seeing W,A,S,D being typed without the code 
;working.
left:
  lda #$f0	 ; C  (240) high C
  sta $900c
  
  lda #$87	 ; C  (135) low C
  ;jsr $ffd2	
  sta $900a		 ; store sound
  ;lda #$e1			; C 225
  ;sta $900b
  jsr timer		 ; jump to loop for duration of sound
  rts			 ; done
  
right:
  lda #$9f		 ; E (159)
  sta $900c			;store sound
  jsr timer
  ;jsr $ffd2
  rts			 ; done
  
up:
  lda #$af		 ; G (175)
  ;jsr $ffd2
  sta $900a
  sta $900b
  sta $900c			 ; store sound
  
  jsr timer
  rts			 ; done
  
down:
  lda #$b7		 ; A (183)
  ;jsr $ffd2
  sta $900c 	 ;store sound
  sta $900a		; have low and high sounds play
  jsr timer
  rts			 ; done
  
timer:			; plays sound for certain amount of time
  dey
  cpy #$00
  bne timer
  rts
turnoff:	; turn off sound for all registers
 lda #$00
 sta $900a
 sta $900b
 sta $900c
 rts