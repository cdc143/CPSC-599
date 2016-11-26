;Test 02 for level

;Enimies no longer spawn outside of playfield
;Changed the way playfield is put into memory. Makes collision detection WAYY easier
;Seems to have broken most coloring code
;Code to color enemies broke somehow
;TODO: Fix how level is loaded - doesn't look like test 0
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

;changes the border and background char_colour -> page 175 of vic do manual
 lda #screen_colour		 ; 0f ->this makes a green border and black background
 sta $900f		 		; store in screen and border register

 jsr genRoom
 jsr initPRowAddr
 jsr initRoomAddr
 jsr loadLevel
 ldx init_lives
 stx lives
 jmp initChars

 ;########################## METHODS TO INIT DATA AND UTILITY METHODS HERE #####

getRandom:
 lda $9114
 adc $9004
 sta seed
 rts

genRoom:
 ldy #$00
genRoomLoop:
 jsr getRandom
 and #$03
 sta rooms,y
 iny
 cpy #$04
 bne genRoomLoop
 rts

initRoomAddr:
  ldx #$0
  lda #<level0 ;low byte
  sta room_addr,x
  inx
  lda #>level0
  sta room_addr,x
  inx
  lda #<level1 ;low byte
  sta room_addr,x
  inx
  lda #>level1
  sta room_addr,x
  inx
  lda #<level2 ;low byte
  sta room_addr,x
  inx
  lda #>level2
  sta room_addr,x
  inx
  lda #<level3 ;low byte
  sta room_addr,x
  inx
  lda #>level3
  sta room_addr,x
  inx
  lda #<level4 ;low byte
  sta room_addr,x
  inx
  lda #>level4
  sta room_addr,x
  rts

initPRowAddr:
  ldx #$0
  lda #<prow0
  sta prow_addr,x
  inx
  lda #>prow0
  sta prow_addr,x
  inx
  lda #<prow1
  sta prow_addr,x
  inx
  lda #>prow1
  sta prow_addr,x
  inx
  lda #<prow2
  sta prow_addr,x
  inx
  lda #>prow2
  sta prow_addr,x
  inx
  lda #<prow3
  sta prow_addr,x
  inx
  lda #>prow3
  sta prow_addr,x
  inx
  lda #<prow4
  sta prow_addr,x
  inx
  lda #>prow4
  sta prow_addr,x
  rts

 ;######################### END INIT AND UTILITY METHODS #######################



;###########################################################################
;###########################################################################
;##########                                                DRAW FUNCTIONS                                                ##########
;###########################################################################
;###########################################################################
					;  used to draw anything in the playfield area (player, walls, enemies, items, doors, portals, etc)
 											; takes 4 inputs

											; Accumulator = character you want to draw
											; X register = the X coordinate for the object to be drawn at (0-21)
											; Y register = the Y coordinate for the object to be drawn at (0-19)
											; drawColour = the colour you want the character to be drawn in
drawToPlayfield:
 sta drawChar					; store the Character to draw
 cpy #$0a
 BPL drawToPlayfieldBot		; if the Y coordinate is in the bootom then go to that method (y >= 10)
 jsr drawMath
 lda drawChar					; get the character back
 sta graphics_top,y				; draw it to the screen
 lda drawColour					; get the colour for the character
 sta char_colour_loc_top,y	; put the colour on the screen
 rts

drawToPlayfieldBot:
 tya									; put Y in Accumulator
 sec
 sbc #$0a							; subtract 10
 tay									; put Y back
 jsr drawMath
 lda drawChar					; get the character back
 sta graphics_bot,y				; draw it to the screen
 lda drawColour					; get the colour for the character
 sta char_colour_loc_bot,y	; put the colour on the screen
 rts

 ; multiplies Y by 22 and adds X to it returns the answer in the Y register
drawMath:
 lda #$00								; clear Accumulator
 cpy #$00								; see if Y is 0 if it is then go to end of method
 BEQ drawMathEnd
drawMathLoop:
 dey										; decrement y
 clc
 adc #$16							; iadd 22
 cpy #$00
 BNE drawMathLoop				; if Y is greater than 0 then loop
drawMathEnd:
 stx Scratch							; put X in memory
 clc
 adc Scratch							; add
 tay										; put the answer in Y
 rts

 ; 			Similar to the playfield method it takes 4 inputs
 ; 			the only difference is that the Y register is from 0-2
 ; 						0 = top status bar
 ;						1 = first bottom status bar (upper)
 ;						2 = second bottom status bar (lowwer)
drawToStatus:
 sta drawChar					; store the Character to draw
 cpy #$00
 BMI drawToStatusBot
 sta status_loc_top,x			; draw it to the screen
 lda drawColour					; get the colour for the character
 sta status_colour_top,x		; put the colour on the screen
 rts

drawToStatusBot:
 dey
 jsr drawMath
 lda drawChar					; get the character back
 sta status_loc_bot,y			; draw it to the screen
 lda drawColour					; get the colour for the character
 sta status_colour_bot,y		; put the colour on the screen
 rts

;##########                                                GET FROM SCREEN                                                ##########
;				used to get the character on the playfield from and X, Y location
;				Takes 2 inputs returns a character in the Accumulator
;				inputs:		X register = X location
;								Y register = Y location
getFromScreen:
 cpy #$0a
 BPL getFromScreenBot		; if the Y coordinate is in the bootom then go to that method (y >= 10)
 jsr drawMath
 lda graphics_top,y				; get the character back from the screen
 rts

getFromScreenBot:
 tya									; put Y in Accumulator
 sec
 sbc #$0a							; subtract 10
 tay									; put Y back
 jsr drawMath
 lda graphics_bot,y				; get the character back from the screen
 rts

;########################## LEVEL LOADING CODE ################################

loadLevel:
 ldx current_room
 lda rooms,x
 asl
 tax
 lda room_addr,x
 sta $fb
 inx
 lda room_addr,x
 sta $fc
 ldy #$0
 ldx #$0

loadLevelLoop:
 lda ($fb),y
 asl
 tax
 lda prow_addr,x
 sta $fd
 inx
 lda prow_addr,x
 sta $fe
 jsr drawPRow
 iny
 inc Ycoor
 cpy #$14 ;try to draw only 2 rows for now
 bne loadLevelLoop

 ldx #$0
 jsr putDoors
 rts

putDoors:
  lda #wall_colour
  sta drawColour
  lda current_room
  cmp #$00
  beq roomZeroorThree
  brk
roomZeroorThree:
  ldy #$0a ;Should be 9, glitch in drawing code
  ldx #$15
  lda #door_sprite
  jsr drawToPlayfield
  ldx #$0a
  ldy #$00
  lda #door_sprite
  jsr drawToPlayfield
  rts

drawPRow: ;Expects address of row to draw in $fd. Saves callers y reg
  ldx #$00
  lda #wall_colour
  sta drawColour
  sty tempY
drawPRowLoop:
  ldy yOffset ;offset into row data
  lda ($fd),y ;char to draw
  ldy Ycoor ; row to draw
  jsr drawToPlayfield ; draw playfield with char, x=0, y=row
  inx
  inc yOffset
  cpx #row_end+1
  bne drawPRowLoop
  ldy #$00
  sty yOffset ;reset y offset
  ldy tempY ;restore callers y reg
  rts

;############################# END OF LEVEL LOADING CODE ######################

drawLives:		;draw lives to screen
 lda #lives_sprite
 sta status_loc_top,x
 lda #life_colour
 sta status_colour_top,x
 dex
 cpx #$00
 bne drawLives
 rts

gameOver: ;
;TODO: Print "Game over"
 lda #$3e		 ; game over border/background colour blue/light blue
 sta $900f		 ; store in screen and border register
 ;print game over to screen
gameOverEnd:	 ; bounce branch to get other subroutines to top of gameLoopTop
 lda $00c5		 ; current key held down -> page 179 of vic20 manual
 cmp #f5 ;quit
 beq quitBounce
 cmp #f1 ;f1 to restart
 bne gameOverEnd
 jmp gameLoopTop

initEnemyLocation:
  jsr getRandom
  tax
  lda graphics_playfield_start,x
  cmp #space_sprite
  bne initEnemyLocation
  lda #enemy_sprite
  sta graphics_playfield_start,x ;potential problem/not problem: enemies
							;can only spawn 255 spaces past graphics_playfield_start
  lda #char_colour
  adc #$10
  sta color_playfield_start,x
  dey
  cpy #$00
  bne initEnemyLocation
  rts
quitBounce:
   jmp quit

gameOverEndBounce:
 jmp gameOverEnd

initChars:
 lda #$01
 sta row		; row
 lda #$0b
 sta col		; col
initCharsNextLevel:	;this is a branch so it skips over assigning row and col to
;player -> this assumes that it is done before this subroutine is done.
 ldx lives	;load index of where to draw lives to screen
;these lines are here so that they refresh the screen every time
 jsr drawLives	;draw current lives to screen
 lda $9005		; load character memory
 pha			; push acc onto stack
 ora #$0f		; set character memory (bits 0-3)
 sta $9005 		; store result

 jsr getRandom
 and #$07
 tay
 jsr initEnemyLocation
 jsr getRowColForm
 lda #p1_sprite		; 'B'
 jsr drawToScreen
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
 cmp #f1 			;f1 to restart
 beq gameOverEndBounce	;bounce to game over to take us to gameLoopTop
 cmp #a		 	;a pressed
 beq playleft	 	; move left
 cmp #d		 	;d pressed
 beq playright	 	; move right
 cmp #w	 		;w pressed
 beq playup
 cmp #s			 ;s pressed
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
 cmp f5		 ; check if Q is pressed -> quit
 beq quit		 ;continue input
 jmp top
quit:
 jsr $e55f       ; clear screen before exiting
 brk			 ; quit

;These subroutines print the next letter of W,A,S,D (X,B,T,E) to make sure
;that we aren't just seeing W,A,S,D being typed without the code
;working.
attack:
 lda #$87		 	; f# (175)
 sta $900b			 ; store sound
 rts

loadRowColLR:
 jsr col_bot_set_0
 ldx row			;load rows
 ldy col			;load cols
 rts
left:
 jsr loadRowColLR
 cpy col_mid_down			;check if cols > 11 (bottom half of screen)
 bmi updateLeft		;not bottom of screen
 beq leftCheck		; =11, so check if far right side
 bpl leftBot		; for sure bottom
leftCheck:
 cpx row_mid_left			 ; check if row > 15, (bottom of screen)
 bmi updateLeft		;not bottom of screen
leftBot:
 jsr col_bot_set_1
updateLeft:
 cpx row_begin				; check if end of screen on left
 beq updateNewLocBounce	; don't move character because it is at end of left screen
 jsr updatePrevLoc
 dec row				; rows -1
 jmp updateNewLocBounce

right:			;similar as above left subroutine
 jsr loadRowColLR
 cpy col_mid_down
 bmi updateRight
 beq rightCheck
 bpl rightBot
rightCheck:
 cpx row_mid_right		 	; check if x >13
 bmi updateRight
rightBot:
 jsr col_bot_set_1
updateRight:
 cpx #row_end		 ; check if x =21 (end of right)
 beq updateNewLocBounce	 ; at end of screen right
 jsr updatePrevLoc
 inc row 		 ; rows +1
 jmp updateNewLocBounce

updateNewLocBounce:	;fix branch out of range
 jmp updateNewLoc

up:
 jsr col_bot_set_0
 ldx col			;top cols
 cpx col_begin		 ; check if x < 21 (on top row)
 beq updateNewLoc	 ; at end of screen top
 cpx col_mid_up			;else, check if in top half of screen
 bmi updateUp		;yes, in top half of screen
 jsr col_bot_set_1
updateUp:
 jsr updatePrevLoc
 dec col 			 ; cols -1
 jmp updateNewLoc

down:
 jsr col_bot_set_0
 ldx col
 cpx col_mid_down	 ; check if bottom (greater than last spot on second to bottom row)
 bmi updateDown
 jsr col_bot_set_1
 cpx col_end			;check very bottom of screen
 beq updateNewLoc
updateDown:
 jsr updatePrevLoc
 inc col 		 ; cols + 1
 jmp updateNewLoc

updateNewLoc:
 jsr getRowColForm	;get row/col
 jsr collision	; collision detection
 rts			; return

updatePrevLoc:		;updates where player previously was
 jsr getRowColForm
 lda col_bot		;check if bottom or top of screen
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
 lda #space_sprite	 	;  space
 jsr drawToScreen
 rts

 ;Note: also need to check for border from top to bottom:
 ;if bottom register =1 and 1004 (key pressed) is down, then
 ;draw top
prevBot:
 lda #space_sprite
 jsr drawToScreenBot	;draw space over previous move
 rts

getRowColForm:		;get coord in row +column
 ldy #$00
 ldx col
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
 tya
 clc
 adc row
 tax
colsnext:
 rts

loseLife:			;player dies
 lda #space_sprite
 ldx lives		;this
 jsr drawToScreen
 dec lives
 lda lives
 cmp #$00
 bpl loseLifeNext
 jmp gameOver
loseLifeNext:
 rts

collision:		 ; detect collision between B and C
 ldy #$01
 sty coll_loop_count	;reg for coll animation
 lda col_bot
 cmp #$01
 beq collision_bot
 ldy graphics_top,x	; load current char
 bne collision_compares	;branch instead of using jmp
collision_bot:
 ldy graphics_bot,x
collision_compares:
 cpy #wall_sprite
 beq drawcoll
 jsr checkPortalOrLevel
 cpy #enemy_sprite		; check if character is enemy
 bne drawCharacter	; not C
 ldy #$05			;set up all this for collision
 sty coll_loop_count			;reg for coll animation loops
 ldy #$06
 sty coll_char_colour	; char_colour
 jsr loseLife		;lose a life
drawcoll:
 jsr collAnimationLoop
 jsr drawCharacter
 rts

checkPortalOrLevel:
 cpy #portal_sprite
 beq portalAnimation
 cpy #door_sprite
 bne notNewLevel
 jsr loadNewLevel
 jmp initCharsNextLevel	;loads character, lives, and enemies into next level
notNewLevel:
 rts

portalAnimation:
 jsr $e55f
 lda #screen_colour
 sta temp_colour
 ldx #$08				;number of iterations
portalAnimTop:
 lda temp_colour
 sta $900f		 		; store in screen and border register
 adc #$33				;add random number to change colour
 sta temp_colour
 jsr drawTimer
 dex
 cpx #$00
 bne portalAnimTop
 jmp gameLoopTop		;note: this currently resets game.
					;TODO: not completely reset game (like switching rooms)

drawCharacter:
 ldy col_bot
 cpy #$01
 beq drawBottom
 jsr getRowColForm
 lda #p1_sprite		; 'B'
 bcs drawBottom	;check for row #$12/divider row between top and bottom
 bcc drawTop

drawTimer:	;timer to draw objects
 lda #$4f		; arbitrary number for timer
 jsr timerLoop	; jump to timer
 rts

drawBottom:
 jsr getRowColForm
 lda #p1_sprite ;#$01
 jsr drawToScreenBot
 jsr drawTimer
 rts

drawTop:
 jsr drawToScreen
 jsr drawTimer
 rts

drawToScreen:
 sta graphics_top,x	; store space
 lda #char_colour		; char_colour to black
 sta char_colour_loc_top,x	; store char_colour in new location too
 rts

drawToScreenBot:
 sta graphics_bot,x	; store space
 lda #char_colour		; char_colour to black
 sta char_colour_loc_bot,x	; store char_colour in new location too
 rts

collAnimationLoop:
 jsr collMovementCheck	;check where collision moves piece
 ;jsr checkColBot		;check top or bottom
 jsr collisionAnimation	;collision animation
 lda #space_sprite
 ldy col_bot
 cpy #$01
 beq drawSpaceBot
 jsr drawToScreen
 jmp animLoopNext
drawSpaceBot:
 jsr drawToScreenBot
animLoopNext:
 dec coll_char_colour
 dec coll_loop_count
 ldx coll_loop_count
 cpx #$00
 bne collAnimationLoop
 rts

col_bot_set_1:		;sets col_bot to 1
 lda #$01
 sta col_bot
 rts

col_bot_set_0:		;sets col_bot to 0
 lda #$00
 sta col_bot
 rts

check0or1:
 stx pos_to_compare
 cmp pos_to_compare
 bmi col_bot_set_0
 bpl col_bot_set_1
 rts

checkHorizontalColBot:	;animation moves right or left
 ldx #$0e			;check if in middle of row
 jmp check0or1
checkUpColBot:			;animation moves down. This works
 ldx row
 cpx #$0e				;check if in middle of row
 bpl checkUpColBotLeft
 cmp col_mid_down		;animation moves down right side of screen
 bpl col_bot_set_0
 bmi col_bot_set_1
checkUpColBotLeft:		;animation moves down left side of screen
 ldx #$0a			;check if in middle of col
 jmp check0or1
checkDownColBot:		;animation moves up
 ldx row
 cpx #$0e			;check if in middle of row
 bpl checkDownColBotLeft
 ldx col_mid_up			;up and right side of screen
 jmp check0or1
checkDownColBotLeft:	;up and left side of screen
 ldx #$0a
 jmp check0or1

collisionAnimation:
 jsr getRowColForm
 lda #p1_sprite
 ldy col_bot
 cpy #$01
 beq storeNewCollBot
 sta graphics_top,x	; store in new index
 lda coll_char_colour		    ; char_colour
 sta char_colour_loc_top,x	; store char_colour
 jmp collAnimNext
storeNewCollBot:
 lda #p1_sprite	;#$02
 sta graphics_bot,x	; store in new index
 lda coll_char_colour		    ; char_colour
 sta char_colour_loc_bot,x	; store char_colour
collAnimNext:
 lda anim_time;#$6e		; arbitrary number for timer
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
 jsr getRowColForm
 inx
 jsr collTop
 cmp #$01
 beq collRet
 ldx row
 cpx #row_end		;check if far left
 beq collRet
 inc row			;else increment row
checkCol_Bot_hori:
 lda col
 cmp col_mid_down	;check for left/right movement
 bmi set_0
 beq checkColH	;awkward middle transition point
 bpl set_1
checkColH:
 lda row
 jsr checkHorizontalColBot
 ;perhaps check col_bot here
 rts

collRight:	;d pressed
 jsr getRowColForm
 dex
 jsr collTop
 cmp #$01
 beq collRet
 ldx row
 cpx row_begin
 beq collRet
 dec row
 bne checkCol_Bot_hori	;branch over

set_0:
 jsr col_bot_set_0
 rts
set_1:
 jsr col_bot_set_1
 rts

collDownBounce:
 jsr collDown
collRet:
 rts

collUp:	;w pressed
 jsr getRowColForm
 txa
 clc
 adc #$16	;add another row
 tax
 jsr collTop
 cmp #$01
 beq collRet
 ldx col
 cpx col_end
 beq collRet
 inc col
checkCol_Bot_vert:
 lda col
 cmp col_mid_down	;check for left/right movement
 bmi set_0
 beq checkColV	;awkward middle transition point
 bpl set_1
checkColV:
 lda col
 jsr checkDownColBot
 rts

collDown:	;s pressed
 jsr getRowColForm
 txa
 sec
 sbc #$16	;add another row
 tax
 jsr collTop
 cmp #$01
 beq collRet
 ldx col
 cpx col_begin
 beq collRet
 dec col
 bne checkCol_Bot_vert	;this seems to work even though it probably shouldn't

collTop:
 lda col_bot
 cmp #$01
 bne collRightTop
 lda graphics_bot,x
 jmp compareCollRight
collRightTop:
 lda graphics_top,x
compareCollRight:
 cmp #wall_sprite
 beq collTopSet1
 cmp #enemy_sprite
 beq collTopSet1
 tay
 jsr checkPortalOrLevel	;check if you hit portal or doorway during collision animation
 lda #$00
 rts
collTopSet1:
 lda #$01
 rts

timerLoop:		 ; super simple loop to slow down movement of 'B' (not have it fly across screen)
 ldy #$ff		 ; 255 (basically the biggest number possible)
timer:
 dey			 ; y-1
 cpy #$00		 ; check if y=0
 bne timer		 ; if not, loop
 sbc #$01		 ; acc - 1
 bpl timerLoop   ; branch if positive (N not set)
 rts			 ; N set, return

loadNewLevel:
  jsr $e55f	;clear screen
  lda col
  cmp row_newLevel_begin		;checking top
  bne checkright
  inc current_room
  inc current_room
  lda col_newLevel_end
  sta col
  dec col
  jsr loadLevel
  rts
checkright:
  lda row
  cmp #row_end
  bne checkbottom
  inc current_room
  lda row_newLevel_begin
  sta row
  inc row
  jsr loadLevel
  rts
checkbottom:
  lda col
  cmp col_newLevel_end
  bne checkLeft
  dec current_room
  dec current_room
  lda col_begin
  sta col
  inc col
  inc col
  jsr loadLevel
  rts
checkLeft:
  lda row
  cmp row_begin
  bne error
  dec current_room
  lda #row_end
  sta row
  dec row
  jsr loadLevel

error: ;shouldn't happen
  rts

level0: dc.b $01,$04,$04,$04,$04,$04,$04,$04,$04,$02,$02,$02,$04,$04,$04,$04,$04,$04,$04,$01
level1: dc.b $01,$02,$02,$02,$03,$03,$03,$02,$02,$03,$03,$03,$02,$02,$03,$03,$03,$02,$02,$01
level2: dc.b $01,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$02,$04,$04,$04,$04,$04,$04,$04,$01
level3: dc.b $01,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$01
level4: dc.b 0
level5: dc.b 0
level6: dc.b 0
level7: dc.b 0
prow0: dc.b $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
prow1: dc.b $66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66
prow2: dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
prow3: dc.b $66,$20,$20,$66,$66,$66,$20,$20,$20,$66,$66,$66,$20,$20,$20,$66,$66,$66,$20,$20,$20,$66
prow4: dc.b $66,$66,$66,$66,$66,$66,$66,$66,$66,$20,$20,$20,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66
prow_addr: dc.b 0,0,0,0,0,0,0,0,0,0

row:						dc.b 0
col:						dc.b 0
col_bot:				dc.b 0
coll_loop_count:		dc.b 0
portal_counter:		dc.b 0
init_lives:				dc.b #$08
lives:					dc.b 0
current_key:			dc.b 0
inventory:				dc.b 0
pi_weapon:			dc.b #94
temp_colour:			dc.b 0
timer_loop:			dc.b 0
coll_char_colour:	dc.b 0
row_begin:			dc.b #$00
row_newLevel_begin: dc.b #$01
col_newLevel_end:   dc.b #$140e
row_mid_left:		dc.b #$0f
row_mid_right:		dc.b #$0d

col_begin:				dc.b #$00
col_mid_down:		dc.b #$0b
col_mid_up:			dc.b #$0d
;col_mid           dc.b #$0a
col_end:				dc.b #$16
seed:					dc.b 0 ;store seed for rand number
current_room:		dc.b 0
rooms:          dc.b 0,0,0,0

isCollision		dc.b #$00
anim_time:		dc.b #$40	;time for animation
pos_to_compare dc.b 0
room_addr: dc.b 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

;drawToPlayfield vars
drawChar:				dc.b 0
drawColour:			dc.b 0
Scratch:				dc.b 0
Ycoor:        dc.b #$00
yOffset:      dc.b 0
tempY:        dc.b 0
