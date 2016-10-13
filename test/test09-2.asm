  ;This test tests basic left/right/up/down movement
  ; (FIXED) NOTE: 'Q' quit functionality stopped working on me for some reason,
  ;so just exit the emulator once satisfied.  It's not the main focus of
  ;this test.
  
  
  ;Oct 9, 2016
  processor 6502
  org $1001               ; Unexpanded VIC

  ;ROW = $1000  <- this isn't correct, just for reference
  ;COL = $1001
  
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
 sty $1e00		; store far left second row
 ldx #$00		;black/initializing character location on row (just convenience that it's also black)
 stx $9600		; char colour
 ldy #$03		; 'C' for collision
 sty $1e1f		; store in the middle of the second row
 lda #$00	
 sta $1000		; row
 sta $1001		; col
 ldx #$00		;black/initializing character location on row (just convenience that it's also black)
 stx $961f		; char colour
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
 cmp #9		 ;w pressed
 beq playup		
 cmp #41		 ;s pressed
 beq playdown		
 bne next		 ; neither pressed
  

playleft:
 jsr left		 ; subroutine to move left
 jmp next
  
playright:
 jsr right		; subroutine to move right
 jmp next

playup:
 jsr up
 jmp next
 
playdown:
 jsr down
 
next:
  ;Wait for user to press enter, and restore the character set
 pla			; pull acc from stack
 sta $9005 		; store in char mem
 lda $00c5		 ; current key held down -> page 179 of vic20 manual
 cmp #48		 ; check if Q is pressed -> quit
 bne top		 ;continue input
 rts			 ; quit
  
;These subroutines print the next letter of W,A,S,D (X,B,T,E) to make sure
;that we aren't just seeing W,A,S,D being typed without the code 
;working.
left:
 ldx $1000
 cpx #$00		; check if end of screen on left
 beq updateNewLoc	; don't move character because it is at end of left screen
 jsr updatePrevLoc
 dec $1000		; rows -1
 ldx $1000
 cpx #$00		;branch regardless of =0 or >0
 beq updateNewLoc
 bne updateNewLoc
 rts

right:
 ldx $1000
 cpx #$15		 ; check if x =21 (end of right)
 beq updateNewLoc	 ; at end of screen right
 jsr updatePrevLoc
 inc $1000 		 ; rows +1
 ldx $1000
 cpx #$00		 ; basically do a branch always to get to updateNewLoc
 bne updateNewLoc
 rts
 
up:
 ldx $1001
 cpx #$00		 ; check if x < 21 (on top row)
 beq updateNewLoc	 ; at end of screen top
 jsr updatePrevLoc
 dec $1001 		 ; cols -1
 ldx $1001
 cpx #$00		 ; basically do a branch always to get to updateNewLoc
 beq updateNewLoc
 bne updateNewLoc
 rts
 
down:
 ldx $1001
 cpx #$0b		 ; check if bottom (greater than last spot on second to bottom row)
 beq updateNewLoc	 ; at end of screen right
 jsr updatePrevLoc
 inc $1001 		 ; cols + 1
 ldx $1001
 cpx #$00		 ; basically do a branch always to get to updateNewLoc
 bne updateNewLoc
 rts
 
 
updateNewLoc:
 ldy #$00		; clear y
 ldx $1001		; load column
 jsr addCols	; get num of x squares per column
 tya
 clc
 adc $1000		; add rows
 tax
 lda #$02		; 'B'
 jsr collision	; collision detection
 sta $1e00,x	; store in new index
 lda #$00		; colour
 sta $9600,x	; store colour
 lda #$4f		; arbitrary number for timer
 jsr timerLoop	; jump to timer
 rts			; return
 
updatePrevLoc:
 ldy #$00
 ldx $1001
 jsr addCols
 tya
 clc
 adc $1000
 tax
 lda #$00	 	;  @ char to signify previous move
 jsr resetcollision	; check to change 'D' back to 'C'
 sta $1e00,x	; store @ in previous location
 lda #$00		; colour to black
 sta $9600,x	; store colour in new location too
 rts
 
addCols:		
 cpx #$00
 beq colsnext
 tya
 clc
 adc #$16		; amount of spaces it takes to get to row below
 tay			
 dex 
 cpx #$00
 bne addCols
colsnext:
 rts

collision:		 ; detect collision between B and C
 ldy $1e00,x	; load current char
 cpy #$03		; check if character is 'C'
 bne collbot	; not C
 lda #$04 		; load 'D' into acc to indicate collision
collbot: 
 rts
 
resetcollision:	; reset D back to C
 ldy $1e00,x	; load current char
 cpy #$04		; check if char is 'D'
 bne resetcollbot
 lda #$03 		; ;load 'C' into reg if D is char
resetcollbot: 
 rts
 
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
