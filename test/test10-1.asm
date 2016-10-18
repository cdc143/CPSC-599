  ;This tests animation of collision occurrence
  ;This program uses W,A,S,D,Q as inputs.
  
  
  ;Oct 9, 2016
 processor 6502
 org $1001               ; Unexpanded VIC

 ;row=$1000  
 ;col=$1001
 ;loop counter for collisions = $1002
 ;collision colour = $1003
 ;key pressed = $1004
 ;colour = $1005

 
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
 sty $1e76		; store in the middle of the second row
 lda #$00	
 sta $1000		; row
 sta $1001		; col
 sta $1005
 ldx $1005		;black/initializing character location on row (just convenience that it's also black)
 stx $9676		; char colour
 pla			; pull acc from stack
 sta $9005		; store in char memory
 
  ; screen registers 1e00-1fff -> 7680-8191 -> 511 
  
top:			; top of loop
 lda $9005		; load char memory
 pha			; push to stack

 lda $00c5		 ; current key held down -> page 179 of vic20 manual
 sta $1004
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
 inx			;x++ to make sure is positive 
 cpx #$00		;branch regardless of =0 or >0
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
 inx			;just make sure x is positive
 cpx #$00		 ; basically do a branch always to get to updateNewLoc
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
 jsr getRowColForm
 jsr collision	; collision detection
 rts			; return
  
updatePrevLoc:
 jsr getRowColForm
 lda #32	 	;  space 
 jsr drawToScreen
 rts
 
getRowColForm:		;get coord in row +column
 ldy #$00
 ldx $1001
 jsr addCols
 tya
 clc
 adc $1000
 tax
 rts
 
addCols:		;converts to row spacing
 cpx #$00
 beq colsnext
 tya
 clc
 adc #$16		; amount of spaces it takes to get to row below
 tay			
 dex 			; decrement number of rows left
 cpx #$00
 bne addCols
colsnext:
 rts

collision:		 ; detect collision between B and C
 ldy $1e00,x	; load current char
 cpy #$03		; check if character is 'C'
 bne drawCharacter	; not C
 ldy #$05
 sty $1002	;reg for coll animation
 lda #$06
 sta $1003	; colour
 jsr resetcollision
 jsr collAnimationLoop
 jsr drawCharacter
 ;lda #$04 		; load 'D' into acc to indicate collision
 rts
 
drawCharacter:
 ldy #$00
 ldx $1001
 jsr addCols
 tya
 clc
 adc $1000
 tax
 lda #$02		; 'B'
 jsr drawToScreen
 lda #$4f		; arbitrary number for timer
 jsr timerLoop	; jump to timer
 rts
 
resetcollision:	; reset D back to C
 ldy $1e00,x	; load current char
 cpy #$04		; check if char is 'D'
 bne resetcollbot
 lda #$03 		; ;load 'C' into reg if D is char'
 jsr drawToScreen
resetcollbot: 
 rts
 
collAnimationLoop:
 jsr collMovementCheck
 jsr collisionAnimation 
 lda #32
 jsr drawToScreen
 dec $1003
 ldx $1002
 dex
 stx $1002
 cpx #$00
 bne collAnimationLoop
 rts
 
collisionAnimation:
 ldy #$00
 ldx $1001
 jsr addCols
 tya
 clc
 adc $1000
 tax 
 lda #$02
 sta $1e00,x	; store in new index
 lda $1003		    ; colour
 sta $9600,x	; store colour
 lda #$40		; arbitrary number for timer
 jsr timerLoop	; jump to timer
 rts
 
drawToScreen:
 sta $1e00,x	; store space
 lda $1005		; colour to black
 sta $9600,x	; store colour in new location too 
 rts
 
collMovementCheck:
 lda $1004		 ; current key held down -> page 179 of vic20 manual
 cmp #17		 ;a pressed
 beq collLeft	 ; move left	

 cmp #18		 ;d pressed
 ;beq playright	 ; move right
 beq collRight
 cmp #9	 ;w pressed
 beq collUp
 ;beq playup		
 cmp #41		 ;s pressed
 beq collDown
 bne collRight ;default
 

collLeft:
 ldx $1000
 inx
 stx $1000
 rts
collRight:
 ldx $1000
 dex
 stx $1000 
 rts
collUp:
 ldx $1001
 inx
 stx $1001 
 rts
collDown:
 ldx $1001
 dex
 stx $1001 
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
