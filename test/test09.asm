  ;This test tests basic left/right movement
  ; NOTE: 'Q' quit functionality stopped working on me for some reason,
  ;so just exit the emulator once satisfied.  It's not the main focus of
  ;this test.
  
  
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

   ;Setup new location for characters
 jsr $e55f       ; clear screen

 lda $9005		; load character memory
 pha			; push acc onto stack
 ora #$0f		; set character memory (bits 0-3)
 sta $9005 		; store result
 ldy #$02		; 'B'
 sty $1e16		; store far left second row
 ldx #$00		;black/initializing character location on row (just convenience that it's also black)
 stx $9616		; char colour
 pla			; pull acc from stack
 sta $9005		; store in char memory
 
  ; screen registers 1e00-1fff -> 7680-8191 -> 511 
  
top:			; top of loop
 lda $9005		; load char memory
 pha			; push to stack

 lda $00c5		 ; current key held down -> page 179 of vic20 manual
 cmp #17		 ;a pressed
 beq playleft	 ; move left	 
 cmp #18		 ;d pressed
 beq playright	 ; move right		
 bne next		 ; neither pressed
  

playleft:
 jsr left		 ; subroutine to move left
 jmp next
  
playright:
 jsr right		; subroutine to move right

next:
  ;Wait for user to press enter, and restore the character set
 pla			; pull acc from stack
 sta $9005 		; store in char mem
 cmp #48		 ; check if Q is pressed -> quit
 bne top		 ;continue input
 rts			 ; quit
  
;These subroutines print the next letter of W,A,S,D (X,B,T,E) to make sure
;that we aren't just seeing W,A,S,D being typed without the code 
;working.
left:
 cpx #$00		; check if end of screen on left
 beq leftnext	; don't move character because it is at end of left screen
 lda #$00	 	;  @ char to signify previous move
 sta $1e16,x	; store @ in previous location
 lda #$00		; colour to black
 sta $9616,x	; store colour in new location too
 dex			;x-1 -> move 'B' left

leftnext: 
 lda #$02		; 'B'
 sta $1e16,x	; store in new index
 lda #$00		; colour
 sta $9616,x	; store colour
 lda #$4f		; arbitrary number for timer
 jsr timerLoop	; jump to timer
 rts			; return
  
right:
 cpx #$15		 ; check if x =21 (end of right)
 beq rightnext	 ; at end of screen right
 lda #$00		 ; '@'
 sta $1e16,x	 ; store in current location 
 lda #$00		 ; colour
 sta $9616,x	 ; store colour
 inx			 ; x+1 -> move right

rightnext:
 lda #$02		 ; 'B'
 sta $1e16,x	 ; store in new location
 lda #$00		 ; colour
 sta $9616,x	 ; store colour in new location

 lda #$4f		 ; arbitrary number for loop
 jsr timerLoop	 ; jump to loop
 rts			 ; done, return
 
timerLoop:		 ; super simple loop to slow down movement of 'B' (not have it fly across screen)
 ldy #$ff		 ; 255 (basically the biggest number possible)
 jsr timer		 ; jump to timer loop
 sbc #$01		 ; acc - 1 
 bpl timerLoop   ; branch if positive (N not set)
 rts			 ; N set, return
 
timer:
 dey			 ; y-1
 cpy #$00		 ; check if y=0
 bne timer		 ; if not, loop
 rts			 ; return