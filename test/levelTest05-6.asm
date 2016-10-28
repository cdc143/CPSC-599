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
;fix collision animation bottom -> no disappearing walls, player 
;is not removed every part, bottom-top movement/top-bottom movement
;;collision animation edge detection -> not going off screen
;using the carry bit for movement up and down
;make a game loop -> character moves, enemy moves, ...
;dasm -> header for smaller files? 
;macros
  
  ;Oct 24, 2016
 processor 6502
 org $1001              ; Unexpanded VIC
 include "header.h"
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
 lda #$5f		; arbitrary number for timer
 jsr timerLoop
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
 sta graphics_top,x	; store space
 lda wall_colour		; char_colour to black
 sta char_colour_loc_top,x	; store char_colour in new location too 
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
 ldx init_lives
 stx lives
 
drawLives:		;draw lives to screen
 lda lives_sprite
 sta graphics_top,x		
 lda char_colour
 sta char_colour_loc_top,x
 dex
 cpx #$00
 bne drawLives
;return
 lda $9005		; load character memory
 pha			; push acc onto stack
 ora #$0f		; set character memory (bits 0-3)
 sta $9005 		; store result
 bne initChars	; just a branch always over

getRandom:
 lda #$00 ;clear the accumulator
 adc seed
 adc $9003 ;add random number from raster memory (can change this to somewhere else if needed)
 sta seed
 rts 
 
gameOver: ;
;TODO: Print "Game over"
 ;jsr $e55f
 lda #$3e		 ; this makes a green border and black background
 sta $900f		 ; store in screen and border register
 ;print game over to screen
gameOverEnd:	 ; bounce branch to get other subroutines to top of gameLoopTop
 lda $00c5		 ; current key held down -> page 179 of vic20 manual
 cmp q ;quit
 beq quitBounce
 cmp f1 ;f1 to restart
 beq gameLoopTop	
 bne gameOverEnd
 
initEnemyLocation:
  jsr getRandom
  tax 
  lda enemy_sprite
  sta graphics_bot,x
  sta graphics_top,x
  lda char_colour
  adc #$30
  sta char_colour_loc_top,x
  sta char_colour_loc_bot,x
  dey 
  cpy #$00
  bne initEnemyLocation
  rts
 
initChars:
 lda #$00	
 sta row		; row
 sta col_bot
 lda #$0b
 sta col		; col
 jsr getRandom
 and #$03
 tay
 jsr initEnemyLocation
 jsr getRowColForm
 lda p1_sprite		; 'B'
 sta graphics_top,x		; store far left second row
 lda char_colour		;black/initializing character location on row (just convenience that it's also black)
 sta char_colour_loc_top,x		; char char_colour
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
 cmp f1 			;f1 to restart
 beq gameOverEnd	;bounce to game over to take us to gameLoopTop
 cmp a		 	;a pressed
 beq playleft	 	; move left	 
 cmp d		 	;d pressed
 beq playright	 	; move right		
 cmp w	 		;w pressed
 beq playup		
 cmp s			 ;s pressed
 beq playdown		
; cmp #33
; beq attack
 bne next			 ; neither pressed
  
quitBounce:
 jmp quit
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
 cmp q		 ; check if Q is pressed -> quit
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
 cpy col_mid_down			;check if cols > 11 (bottom half of screen)
 bmi updateLeft		;not bottom of screen
 beq leftCheck		; =11, so check if far right side
 bpl leftBot		; for sure bottom
leftCheck:
 cpx row_mid_left			 ; check if row > 15, (bottom of screen)
 bmi updateLeft		;not bottom of screen
leftBot:
 lda #$01			;set col_bot flag
 sta col_bot
updateLeft:
 cpx row_begin				; check if end of screen on left
 beq updateNewLocBounce	; don't move character because it is at end of left screen
 jsr updatePrevLoc
 dec row				; rows -1
 jmp updateNewLocBounce
 
right:			;similar as above left subroutine
 lda #$00
 sta col_bot
 ldx row		;load rows
 ldy col
 cpy col_mid_down
 bmi updateRight
 beq rightCheck
 bpl rightBot
rightCheck:
 cpx row_mid_right		 	; check if x >13
 bmi updateRight
rightBot:
 lda #$01			;set col_bot flag
 sta col_bot
updateRight:
 cpx row_end		 ; check if x =21 (end of right)
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
 cpx col_begin		 ; check if x < 21 (on top row)
 beq updateNewLoc	 ; at end of screen top
 cpx col_mid_up			;else, check if in top half of screen
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
 cpx col_mid_down	 ; check if bottom (greater than last spot on second to bottom row)
 bmi updateDown
 lda #$01
 sta col_bot
 cpx col_end			;check very bottom of screen
 beq updateNewLoc
updateDown:
 jsr updatePrevLoc
 inc col 		 ; cols + 1
 jmp updateNewLoc

updateNewLoc:
 jsr getRowColForm	;get row/col
 ;lda col_bot		;check if in second half of screen
 ;cmp #$00
 ;bne updateNewLocBot
 jsr collision	; collision detection
 rts			; return
   
updatePrevLoc:
 jsr getRowColForm
 lda col_bot
 cmp #$01
 bne checkColToTop
 beq checkColToBot
 
checkColToTop:	;going from bottom to top
 lda row
 cmp #$0d
 beq checkLeftToTop
 bpl checkRowsToTop
 lda col
 cmp #$0c		;going from bottom to top
 beq prevBot	;prev location was bottom
 bne prevLocIsTop
  
checkColToBot:	; going from top to bottom
 lda row
 cmp #$0d
 beq checkRightToBot	;check 255th byte
 bpl checkRowsToBot
 lda col
 cmp col_mid_down		;check if col is 11
 beq prevLocIsTop	;going from top to bottom
 bne prevBot

checkLeftToTop:	;check row 0b going from bottom to top from left
 lda col
 cmp col_mid_down			;check if in row 0b
 beq prevLocIsTop	;yes, 
 bne checkRowsToTop	;no, so check for another row
 
checkRightToBot:	;check moving right to bottom around 255th byte
 lda col
 cmp col_mid_down			
 beq prevLocIsTop	;moving from top to bot
 bne prevBot		;nope, on bottom
 
checkRowsToTop:		;check moving from 0c to 0b
 lda col
 cmp col_mid_down
 bpl prevBot		; on bottom if > 0b
 bne prevLocIsTop
 
checkRowsToBot:		;check moving from 0b to 0c
 lda col
 cmp #$0a
 beq prevLocIsTop
 bne prevBot
 
prevLocIsTop:
 lda space_sprite	 	;  space 
 jsr drawToScreen
 rts 
 
 ;Note: also need to check for border from top to bottom:
 ;if bottom register =1 and 1004 (key pressed) is down, then
 ;draw top 
prevBot:
 lda space_sprite
 jsr drawToScreenBot	;draw space over previous move
 rts
 
getRowColForm:		;get coord in row +column
 ldy #$00
 ldx col
 jsr addCols
 tya
 clc
 adc row
 tax
 rts
    
gameOverBounce:	;just to fix branch out of range
 jmp gameOver
 
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
 lda col_bot
 cmp #$01
 beq collision_bot
 ldy graphics_top,x	; load current char
 cpy wall_sprite	; check if is # (wall)
 beq drawcoll
 cpy enemy_sprite		; check if character is enemy
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
 rts
 
collision_bot:
 ldy graphics_bot,x	; load current char
 cpy wall_sprite		; check if is # (wall)
 beq drawcoll_bot
 cpy enemy_sprite			; check if character is enemy
 bne drawCharacter	; not C
 ldy #$05			;set up all this for collision
 sty coll_loop_count			;reg for coll animation loops
 lda #$06
 sta coll_char_colour	; char_colour
 ;jsr resetcollision
 jsr loseLife		;
drawcoll_bot:
 jsr collAnimationLoop
 jsr drawCharacter
 rts
 
loseLife:			;player dies 
 lda space_sprite
 ldx lives
 jsr drawTop
 dec lives
 lda lives
 cmp #$00
 bmi gameOverBounce
 rts

drawCharacter:
 ldy col_bot
 cpy #$01
 beq drawBottom
 jsr getXCoord
 lda p1_sprite		; 'B'
 bcs drawBottom
 bcc drawTop
 lda #$4f		; arbitrary number for timer
 jsr timerLoop	; jump to timer
 rts

 
getXCoord: 
 ldy #$00
 ldx col
 jsr addColsAndTransfY
 rts
 
drawBottom:
 jsr getXCoord
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
 
drawToScreen:
 sta graphics_top,x	; store space
 lda char_colour		; char_colour to black
 sta char_colour_loc_top,x	; store char_colour in new location too 
 rts
 
drawToScreenBot:
 sta graphics_bot,x	; store space
 lda char_colour		; char_colour to black
 sta char_colour_loc_bot,x	; store char_colour in new location too 
 rts
 
collAnimationLoop:
 jsr collMovementCheck
 jsr collisionAnimation 
 lda space_sprite
 ldy col_bot
 cpy #$01
 beq drawSpaceBot
 jsr drawToScreen
 jmp animLoopNext
drawSpaceBot:
 jsr drawToScreenBot
animLoopNext:
 dec coll_char_colour
 ldx coll_loop_count
 dex
 stx coll_loop_count
 cpx #$00
 bne collAnimationLoop
doneCollAnimLoop:
 rts
 
collisionAnimation:
 jsr getXCoord
 lda p1_sprite
 ldy col_bot 
 cpy #$01
 beq storeNewCollBot
 sta graphics_top,x	; store in new index
 lda coll_char_colour		    ; char_colour
 sta char_colour_loc_top,x	; store char_colour
 jmp collAnimNext
storeNewCollBot:
 sta graphics_bot,x	; store in new index
 lda coll_char_colour		    ; char_colour
 sta char_colour_loc_bot,x	; store char_colour
collAnimNext:
 lda #$40		; arbitrary number for timer
 jsr timerLoop	; jump to timer
 rts

collMovementCheck:
 lda current_key		 ; current key held down -> page 179 of vic20 manual
 cmp a		 ;a pressed
 beq collLeft	 ; move left	
 cmp d		 ;d pressed
 beq collRight
 cmp w	 ;w pressed
 beq collUp
 cmp s		 ;s pressed
 beq collDownBounce
 bne collRight ;default

collLeft:		;a pressed
 ldy #$00
 ldx col
 jsr addColsAndTransfY
 inx
 lda col_bot 
 cmp #$01
 bne collLeftTop
 lda graphics_bot,x
 jmp compareCollLeft
collLeftTop:
 lda graphics_top,x
compareCollLeft:
 cmp wall_sprite	;hit a wall
 beq collRet
 cmp enemy_sprite
 beq collRet
 ldx row
 cpx row_end		;check if far left
 beq collRet
 inc row
 rts
 
checkCollLeftForBot:
 
collRight:	;d pressed
 ldy #$00
 ldx col
 jsr addColsAndTransfY
 dex
 lda col_bot 
 cmp #$01
 bne collRightTop
 ;jsr subRowForBot
 lda graphics_bot,x
 jmp compareCollRight
collRightTop:
 lda graphics_top,x
compareCollRight:
 cmp wall_sprite
 beq collRet
 cmp enemy_sprite
 beq collRet
 ldx row
 cpx row_begin
 beq collRet
 dec row
 rts
 
collDownBounce:
 jsr collDown
 rts
 
collRet:
 rts
 
collUp:	;w pressed
 ldy #$00
 ldx col
 inx
 jsr addColsAndTransfY
 lda col_bot 
 cmp #$01
 bne collUpTop
 ;jsr subRowForBot
 lda graphics_bot,x
 jmp compareCollUp
collUpTop:
 lda graphics_top,x
compareCollUp:
 cmp wall_sprite
 beq collRet
 cmp enemy_sprite
 beq collRet
 ldx col
 cpx col_end
 beq collRet
 inc col
 rts
 
collDown:	;s pressed
 ldy #$00
 ldx col
 dex
 jsr addColsAndTransfY
 lda col_bot 
 cmp #$01
 bne collDownTop
 ;jsr subRowForBot
 lda graphics_bot,x
 jmp compareCollDown
collDownTop:
 lda graphics_top,x
compareCollDown:
 cmp wall_sprite
 beq collRet
 cmp enemy_sprite
 beq collRet
 ldx col
 cpx col_begin
 dec col
 rts

subRowForBot:
 txa 
 sbc row_mid_right
 tax
 rts
 
addColsAndTransfY:
 jsr addCols
 tya 
 clc 
 adc row
 tax
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
coll_loop_count:	dc.b 0
init_lives:			dc.b #$08
lives:				dc.b 0
current_key:		dc.b 0
inventory:			dc.b 0
pi_weapon:			dc.b #94
char_colour:		dc.b #$55
wall_colour:		dc.b #$44
screen_colour:		dc.b #$0f
coll_char_colour:	dc.b 0
w:					dc.b #9
a:					dc.b #17
s:					dc.b #41
d:					dc.b #18
q:					dc.b #48
f1:					dc.b #39
p1_sprite:			dc.b #81		;81 = circle
lives_sprite:		dc.b #83		;heart
enemy_sprite:		dc.b #87		;circle
wall_sprite:		dc.b $66		;weird checkered square thingy
portal_sprite:		dc.b #209
space_sprite:		dc.b #32
row_begin:			dc.b #$00
row_mid_left:		dc.b #$0f
row_mid_right:		dc.b #$0d
row_end:			dc.b #$15
col_begin:			dc.b #$00
col_mid_down:		dc.b #$0b
col_mid_up:			dc.b #$0d
col_end:			dc.b #$16	
seed:				dc.b #74 ;constant seed	