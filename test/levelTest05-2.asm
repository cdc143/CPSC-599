;Test 02 for level
;TODO: Fix how level is loaded - doesn't look like test 08
;get character moving to bottom screen
;collision animation - prevent character from breaking wall - FIXED (But code is trashy)
;loselife activated because players drop all over screen because updatePrev doesn't draw 
;over them -Solved (somehow)?
;player runs over walls on bottom; TODO: collision detection

;Things added: lives at top left 
;lost lives if collide with enemy (circle)
;changed icon of characters 
;ability to reset game if F1 pressed
;die after run out of lives
;character is moving around bottom, super duper glitchy and not well implemented

;Oct 27, 2016: TODO
;random enemies onto screen -> maybe moving
;fix collision detection for bottom
;using the carry bit for movement up and down
;make a game loop -> character moves, enemy moves, ...
;dasm -> header for smaller files? 
;macros
  
  ;Oct 24, 2016
 processor 6502
 org $1001              ; Unexpanded VIC
 
;ctrl to attack
 
 ; BASIC stub (unexpanded vic)
 dc.w $100b            ; Pointer to next BASIC line
 dc.w 1981               ; BASIC Line#
 dc.b $9e                ; BASIC SYS token
 dc.b $34,$31,$30,$39    ; 4109 (ML start)
 dc.b 0                  ; End of BASIC line
 dc.w 0                  ; End of BASIC program
 
 lda #$0f		 ; 15
 sta $900e		; set sound bits/turn on volume (see)
 ;changes the char_colour of text -> page 173 vic manual
 
gameLoopTop:
   ;Setup new location for characters
 jsr $e55f       		; clear screen
 lda char_colour		; load new char_colour to acc
 sta $0286		 		; change text char_colour

;changes the border and background char_colour -> page 175 of vic do manual
 lda screen_colour		 ; 0f ->this makes a green border and black background
 sta $900f		 		; store in screen and border register
 ldx #$0
 
loadLevel:
 lda level1,x
 sta $1e00,x	; store space
 lda wall_colour		; char_colour to black
 sta $9600,x	; store char_colour in new location too 
 ;jsr $ffd2
 inx
 cpx #level1end-level1
 bne loadLevel
 ldy #level1end-level1
loop2:
 lda #level1,y
 ;jsr $ffd2
 sta $1f1e,y	; store space
 lda wall_colour		; char_colour to black
 sta $971e,y	; store char_colour in new location too 
 dey
 cpy level1
 bne loop2
 ldx #$03
 stx lives
 
drawLives:		;draw lives to screen
 lda lives_sprite
 sta $1e00,x		
 lda char_colour
 sta $9600,x
 dex
 cpx #$00
 bne drawLives
;return

 lda $9005		; load character memory
 pha			; push acc onto stack
 ora #$0f		; set character memory (bits 0-3)
 sta $9005 		; store result
 bne initChars	; just a branch always over
 
gameOver: ;
;TODO: Print "Game over"
 ;jsr $e55f
 lda #$3e		 ; this makes a green border and black background
 sta $900f		 ; store in screen and border register
 ;print game over to screen
gameOverEnd:	 ; bounce branch to get other subroutines to top of gameLoopTop
 lda $00c5		 ; current key held down -> page 179 of vic20 manual
 cmp #48 ;quit
 beq quit
 cmp #39 ;f1 to restart
 beq gameLoopTop	
 bne gameOverEnd
 
initChars:
 ldy enemy_sprite		; 'C' for collision
 sty $1e76		; store in the middle of the second row
 lda #$00	
 sta row		; row
 sta col_bot
 lda #$0b
 sta col		; col
 ldx char_colour		;black/initializing character location on row (just convenience that it's also black)
 stx $9676		; char char_colour
 jsr getRowColForm
 lda p1_sprite		; 'B'
 sta $1e00,x		; store far left second row
 lda char_colour		;black/initializing character location on row (just convenience that it's also black)
 sta $9600,x		; char char_colour
 pla			; pull acc from stack
 sta $9005		; store in char memory
 
  ; screen registers 1e00-1fff -> 7680-8191 -> 511  
top:			; top of loop
 lda #$00
 sta $900b			 ; store sound
 lda $9005			; load char memory
 pha				; push to stack
 lda $00c5		 	; current key held down -> page 179 of vic20 manual
 sta current_key
 cmp #39 			;f1 to restart
 beq gameOverEnd	;bounce to game over to take us to gameLoopTop
 cmp #17		 	;a pressed
 beq playleft	 	; move left	 
 cmp #18		 	;d pressed
 beq playright	 	; move right		
 cmp #9		 		;w pressed
 beq playup		
 cmp #41			 ;s pressed
 beq playdown		
; cmp #33
; beq attack
 bne next			 ; neither pressed
  
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
quit:
 jsr $e55f       ; clear screen before exiting
 brk			 ; quit
  
;These subroutines print the next letter of W,A,S,D (X,B,T,E) to make sure
;that we aren't just seeing W,A,S,D being typed without the code 
;working.
attack:
 lda #$87		 	; f# (175)
  ;jsr $ffd2
 sta $900b			 ; store sound
 rts
 
left:
 lda #$00			;reset bottom collision 
 sta col_bot
 ldx row			;load rows
 ldy col			;load cols
 cpy #$0b			;check if cols > 11 (bottom half of screen)
 bmi updateLeft		;not bottom of screen
 beq leftCheck		; =11, so check if far right side
 bpl leftBot		; for sure bottom
leftCheck:
 cpx #$0f			 ; check if row > 15, (bottom of screen)
 bmi updateLeft		;not bottom of screen
leftBot:
 lda #$01			;set col_bot flag
 sta col_bot
updateLeft:
 cpx #$00				; check if end of screen on left
 beq updateNewLocBounce	; don't move character because it is at end of left screen
 jsr updatePrevLoc
 dec row				; rows -1
 jmp updateNewLocBounce
 
right:			;similar as above left subroutine
 lda #$00
 sta col_bot
 ldx row		;load rows
 ldy col
 cpy #$0b
 bmi updateRight
 beq rightCheck
 bpl rightBot
rightCheck:
 cpx #$0d		 	; check if x >13
 bmi updateRight
rightBot:
 lda #$01			;set col_bot flag
 sta col_bot
updateRight:
 cpx #$15		 ; check if x =21 (end of right)
 beq updateNewLocBounce	 ; at end of screen right
 jsr updatePrevLoc
 inc row 		 ; rows +1
 jmp updateNewLocBounce

updateNewLocBounce:	;fix branch out of range
 jmp updateNewLoc

up:
 lda #$00
 sta col_bot
 ldx col			;top cols
 cpx #$00		 ; check if x < 21 (on top row)
 beq updateNewLoc	 ; at end of screen top
 cpx #$0d			;else, check if in top half of screen
 bmi updateUp		;yes, in top half of screen
 lda #$01			;no, in bottom half of screen; set col_bot flag
 sta col_bot
updateUp:
 jsr updatePrevLoc
 dec col 			 ; cols -1
 jmp updateNewLoc

down:
 lda #$00
 sta col_bot
 ldx col
 cpx #$0b	 ; check if bottom (greater than last spot on second to bottom row)
 bmi updateDown
 lda #$01
 sta col_bot
 cpx #$16			;check very bottom of screen
 beq updateNewLoc
updateDown:
 jsr updatePrevLoc
 inc col 		 ; cols + 1
 jmp updateNewLoc

updateNewLoc:
 jsr getRowColForm	;get row/col
 lda col_bot		;check if in second half of screen
 cmp #$00
 bne updateNewLocBot
 jsr collision	; collision detection
 rts			; return
  
updateNewLocBot:
 lda #01		;just storing A for debugging; replace with p1_sprite when done 
 sta $1f00,x			; store bottom half
 lda char_colour		; char_colour 
 sta $9700,x	; store char_colour in new location too 
 jsr timerLoop
 rts
 
updatePrevLoc:
 jsr getRowColForm
 lda col_bot
 cmp #$01
 beq prevBot		;check if character is on bottom
 lda #32	 	;  space 
 jsr drawToScreen
 rts 
 
 ;Note: also need to check for border from top to bottom:
 ;if bottom register =1 and 1004 (key pressed) is down, then
 ;draw top 
prevBot:
 ;jsr getRowColForm
 ;jsr getRowColFormBot	;bottom half of screen
;lastRowTop:
 lda #32
 jsr drawToScreenBot	;draw space over previous move
prevLocNext:
 rts
  
gameOverBounce:	;just to fix branch out of range
 jmp gameOver
 
getRowColForm:		;get coord in row +column
 ldy #$00
 ldx col
 jsr addCols
 tya
 clc
 adc row
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
 ldy #$01
 sty coll_loop_count	;reg for coll animation
 ldy $1e00,x	; load current char
 cpy wall_sprite		; check if is # (wall)
 beq drawcoll
 cpy enemy_sprite			; check if character is enemy
 bne drawCharacter	; not C
 ldy #$05			;set up all this for collision
 sty coll_loop_count			;reg for coll animation loops
 lda #$06
 sta coll_char_colour	; char_colour
 ;jsr resetcollision
 jsr loseLife		;
drawcoll:
 jsr collAnimationLoop
 jsr drawCharacter
collisionBottom:
 rts
 
loseLife:			;player dies 
 lda #32
 ldx lives
 jsr drawTop
 dec lives
 lda lives
 cmp #$00
 bmi gameOverBounce
 rts

drawCharacter:
 ldy #$00
 ldx col
 jsr addCols
 tya
 clc
 adc row
 tax
 lda p1_sprite		; 'B'
 bcs drawBottom
 bcc drawTop
 ;jsr drawToScreen
 lda #$4f		; arbitrary number for timer
 jsr timerLoop	; jump to timer
 rts
 
drawBottom:
 ;ldy #$01
 ;sty p1_sprite			;set bottom bit

 ldy #$00
 ldx col
 jsr addCols
 tya 
 clc 
 adc row
 ;sbc #$0d		;subtract 13
 tax
 lda p1_sprite
 jsr drawToScreenBot
 lda #$4f		; arbitrary number for timer
 jsr timerLoop	; jump to timer
 rts
 
drawTop:
 jsr drawToScreen
 lda #$4f		; arbitrary number for timer
 jsr timerLoop	; jump to timer
 rts
;resetcollision:	; reset D back to C
; ldy $1e00,x	; load current char
; cpy #$04		; check if char is 'D'
; bne resetcollbot
; lda enemy_sprite 		; ;load 'C' into reg if D is char'
; jsr drawToScreen
;resetcollbot: 
; rts
 
drawToScreen:
 sta $1e00,x	; store space
 lda char_colour		; char_colour to black
 sta $9600,x	; store char_colour in new location too 
 rts
 
drawToScreenBot:
 sta $1f00,x	; store space
 lda char_colour		; char_colour to black
 sta $9700,x	; store char_colour in new location too 
 rts
 
collAnimationLoop:
 jsr collMovementCheck
 jsr collisionAnimation 
 lda #32
 jsr drawToScreen
 dec coll_char_colour
 ldx coll_loop_count
 dex
 stx coll_loop_count
 cpx #$00
 bne collAnimationLoop
doneCollAnimLoop:
 rts
 
collisionAnimation:
 ldy #$00
 ldx col
 jsr addCols
 tya
 clc
 adc row
 tax 
 lda p1_sprite
 sta $1e00,x	; store in new index
 lda coll_char_colour		    ; char_colour
 sta $9600,x	; store char_colour
 lda #$40		; arbitrary number for timer
 jsr timerLoop	; jump to timer
 rts

collMovementCheck:
 lda current_key		 ; current key held down -> page 179 of vic20 manual
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
 ;ldx row
 ldy #$00
 ldx col
 jsr addCols
 tya 
 clc 
 adc row
 
 ;sbc #$0d		;subtract 13
 tax
 inx
 lda $1e00,x
 cmp wall_sprite
 beq collRet
 ldx row
 inx
 stx row
 rts
collRight:
 ldy #$00
 ldx col
 jsr addCols
 tya 
 clc 
 adc row
 
 ;sbc #$0d		;subtract 13
 tax
 dex
 lda $1e00,x
 cmp wall_sprite
 beq collRet
 ldx row
 dex
 stx row
 rts
 
collUp:
 ldy #$00
 ldx col
 inx
 jsr addCols
 tya 
 clc 
 adc row
 ;sbc #$0d		;subtract 13
 tax
 lda $1e00,x
 cmp wall_sprite
 beq collRet
 ldx col
 inx
 stx col
 rts
collDown:
 ldy #$00
 ldx col
 dex
 jsr addCols
 tya 
 clc 
 adc row
 
 ;sbc #$0d		;subtract 13
 tax
 lda $1e00,x
 cmp wall_sprite
 beq collRet
 ldx col
 dex
 stx col
 rts
collRet:
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
 



 ;text
 ;44 = purple, 55 = green; ff = blurry and black

 ;border/background -> 0f = green border, black background

level1:
  dc.b $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  dc.b $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  dc.b $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  dc.b $20,$20,$20,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66
  dc.b $20,$20,$20,$66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  dc.b $20,$20,$20,$66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  dc.b $20,$20,$20,$66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  dc.b $20,$20,$20,$66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  dc.b $20,$20,$20,$66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  dc.b $66,$66,$66,$66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  dc.b $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
level1end

row:				dc.b 0
col:				dc.b 0
col_bot:			dc.b 0
p1_sprite:			dc.b #81
lives_sprite:		dc.b #83		;heart
enemy_sprite:		dc.b #87		;circle
wall_sprite:		dc.b $66
coll_loop_count:	dc.b 0
lives:				dc.b 0
current_key:		dc.b 0
inventory:			dc.b 0
char_colour:		dc.b #$55
wall_colour:		dc.b #$44
screen_colour:		dc.b #$0f
coll_char_colour:	dc.b 0