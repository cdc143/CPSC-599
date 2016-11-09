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

 ;we can put music and stuff
 ;This is not the prettiest title page, it is just a placeholder for a better title page.
 ;also, need a better title than "boop"!
 lda #$00
 ldx #$00
 ldy #$00
 jsr $ffbd	;initialize clock
 lda char_colour
 sta graphics_colour
titleScreen:
 jsr $e55f		;clear screen
 lda #$0f		;temporary colour start
 sta temp_colour
 jsr loadLevel3 	;load level 3 to screen
 ldx #$90			;screen coord for title; too lazy to use x,y coords
 ldy #titleNameEnd-titleName-1	;length
drawTitleLoop:		;draw title to screen
 lda titleName,y
 jsr drawToScreen
 dey		;length -1
 dex		;index -1
 cpy #$00
 bpl drawTitleLoop
 ldx #$be		;screen coord for author names
 ldy #titleAuthorsEnd-titleAuthors-1
drawAuthorLoop:
 lda titleAuthors,y
 jsr drawToScreen
 dey
 dex
 cpy #$00
 bpl drawAuthorLoop

drawTitleAnimation:	;this is a loop because don't need to constantly redraw title names
 lda temp_colour
 sta $900f		 		; store in screen and border register
 jsr $ffde 		;read clock
 cmp #$01
 bne titleInput
 dec temp_colour	;random colour, pretty much
 inc graphics_colour
 ;lda temp_colour	;this code does checking, but don't necessarily need it
					;unless we want certain screen colours
 ;cmp #$00
 ;bne titleInput
 ;lda #$ff
 ;ora temp_colour
 ;jsr getRandom	;drawing enemies to screen breaks after a while.  Runs out of memory probably.
 ;and #$01
 ;sta draw_num_enemies
 ;jsr initEnemyLocation
titleInput:
 lda $00c5			;check for start.  Only can press start right now. Probably should
 sta current_key
 cmp f3
 beq gameLoopTop
 jsr checkRestartQuit
 bne drawTitleAnimation

;checks keys f1 and f5.  This subroutine gets rid of some stuff down below in code.
checkRestartQuit:
 lda current_key
 cmp f1
 beq titleScreen
 cmp f5
 bne retRestQuit
 jsr $e55f
 brk
retRestQuit:
 rts

gameLoopTop:
 lda #$5f		; arbitrary number for timer
 jsr timerLoop
 ;Setup new location for characters
 jsr $e55f       		; clear screen

;changes the border and background char_colour -> page 175 of vic do manual
 lda screen_colour		 ; 0f ->this makes a green border and black background
 sta $900f		 		; store in screen and border register
 ;ldx #$03
 ;stx current_room
 ldx #$00
 ;jsr loadLevel
 ldy #$00
 lda #$03
 sta rooms,y
 iny
 lda #$04
 sta rooms,y
 iny
 lda #$02
 sta rooms,y
 iny
 lda #$01
 sta rooms,y
 jsr loadLevel3
 ldx init_lives
 stx lives
 jmp initChars

; loads level 1
; loading a level overwrites the A and X registers at the moment
; might be a good idea to write a couple lines to save the A and X regs somewhere and then swap them back before rts
loadLevel:	;loads a level based on random number generator
 jsr getRandom
 and #$03	;option of 4 rooms at moment
 ;sta current_room
 jsr loadLevel1
 jsr loadLevel2
 jsr loadLevel3
 jsr loadLevel4
 rts

loadLevel1:		;default room for now
 ldx #$00					; reset x to use as a loop counter
loadLevel1Loop1:       ; loop that loads the top half of the level
 lda level1top,x          ; load the character from the x offset of the map
 sta graphics_top,x
 lda wall_colour
 sta char_colour_loc_top,x
 inx                           ; increment x
 cpx #level1topend-level1top           ; check to see if all of the top half has been loaded
 bne loadLevel1Loop1                      ; branch back to the top of the loop if it hasnt finished yet
 ldx #$00					; reset x to use as a loop counter
loadLevel1Loop2:       ; loop that loads the bottom half of the level
 lda level1bottom,x
 sta $1edc,x
 lda wall_colour
 sta $96dc,x
 inx
 cpx #level1bottomend-level1bottom
 bne loadLevel1Loop2
loadLevel1End:
 rts

; loads level 2

loadLevel2:
 ldx #$00
loadLevel2Loop1:
 lda level2top,x
 sta graphics_top,x
 lda wall_colour
 sta char_colour_loc_top,x
 inx
 cpx #level2topend-level2top
 bne loadLevel2Loop1
 ldx #$00
loadLevel2Loop2:
 lda level2bottom,x
 sta $1edc,x
 lda wall_colour
 sta $96dc,x
 inx
 cpx #level2bottomend-level2bottom
 bne loadLevel2Loop2
loadLevel2End:
 rts

; loads level 3

loadLevel3:
 ldx #$0
loadLevel3Loop1:
 lda level3top,x
 sta graphics_top,x
 lda wall_colour
 sta char_colour_loc_top,x
 inx
 cpx #level3topend-level3top
 bne loadLevel3Loop1
 ldx #$0
loadLevel3Loop2:
 lda level3bottom,x
 sta $1edc,x
 lda wall_colour
 sta $96dc,x
 inx
 cpx #level3bottomend-level3bottom
 bne loadLevel3Loop2
loadLevel3End:
 rts

; loads level 4

loadLevel4:
 ldx #$00
loadLevel4Loop1:
 lda level4top,x
 sta graphics_top,x
 lda wall_colour
 sta char_colour_loc_top,x
 inx
 cpx #level4topend-level4top
 bne loadLevel4Loop1
 ldx #$00
loadLevel4Loop2:
 lda level4bottom,x
 sta $1edc,x
 lda wall_colour
 sta $96dc,x
 inx
 cpx #level4bottomend-level4bottom
 bne loadLevel4Loop2
loadLevel4End:
 rts

drawLives:		;draw lives to screen
 lda life_colour
 sta graphics_colour
 lda lives_sprite
 jsr drawToScreen
 dex
 cpx #$00
 bne drawLives
 rts

getRandom:
 lda seed
 adc $9003 ;add random number from raster memory (can change this to somewhere else if needed)
 sta seed
 rts

gameOver:
;TODO: Print "Game over"
 lda #$3e		 ; this makes a green border and black background
 sta $900f		 ; store in screen and border register
gameOverScreenTop:
 ldx #$93			;screen coord for title; too lazy to use x,y coords
 ldy #gameOverMessageEnd-gameOverMessage-1	;length
 lda #$00
 sta graphics_colour
gameOverScreenLoop:		;draw title to screen
 lda gameOverMessage,y
 jsr drawToScreen
 dey		;length -1
 dex		;index -1
 cpy #$00
 bpl gameOverScreenLoop
 ;print game over to screen
gameOverEnd:	 ; bounce branch to get other subroutines to top of gameLoopTop
 lda $00c5			;check for start.  Only can press start right now. Probably should
 sta current_key
 jsr checkRestartQuit
 jmp gameOverEnd

initEnemyLocation:
  jsr getRandom
  tax
  lda graphics_playfield_start,x
  cmp space_sprite
  bne initEnemyLocation
  lda enemy_sprite
  sta graphics_playfield_start,x ;potential problem/not problem: enemies
							;can only spawn 255 spaces past graphics_playfield_start
  lda char_colour
  adc #$10
  sta color_playfield_start,x
  jsr convertOffset
  dec draw_num_enemies
  ldy draw_num_enemies
  cpy #$00
  bne initEnemyLocation
  rts

convertOffset:
  txa
  ldx draw_num_enemies
  dex
divisionLoop:
  tay
  sbc #$09
  bcc remainder
  inc enemyypos,x
  bpl divisionLoop
remainder:
  tya
  sta enemyxpos,x
  rts

gameOverEndBounce:
 jmp gameOverEnd

initChars:
 lda #$01
 sta row		; row
 sta col_bot
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
 lda #$01
 sta draw_num_enemies
 jsr initEnemyLocation
 lda char_colour
 sta graphics_colour
 lda p1_sprite		; 'B'
 jsr drawToScreen
 ;sta graphics_top,x	; store far left second row
 ;lda char_colour		;black/initializing character location on row (just convenience that it's also black)
 ;sta char_colour_loc_top,x		; char char_colour
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
 jsr checkRestartQuit
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
 jmp top		 ;continue input

;These subroutines print the next letter of W,A,S,D (X,B,T,E) to make sure
;that we aren't just seeing W,A,S,D being typed without the code
;working.
attack:
 lda #$87		 	; f# (175)
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

moveEnemy:
  lda enemyxpos
  cmp row
  beq checkColumn
  bcc rowLess
  dec enemyxpos
  bpl end
rowLess:
  inc enemyxpos
  bpl end
checkColumn
  cmp col
  beq end
  bcc columnLess
  dec enemyypos
  bpl end
columnLess:
  inc enemyypos
end:
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

loseLife:			;player dies
 lda space_sprite
 ldx lives
 jsr drawTop
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
 cmp #$00		;just using this cmp to jump over collision_bot
 beq collision_compares	;branch instead of using jmp
collision_bot:
 ldy graphics_bot,x
collision_compares:
 cpy wall_sprite
 beq drawcoll
 cpy portal_sprite
 beq portalAnimation
 cpy door_sprite
 bne checkEnemy
 jsr $e55f
 jsr loadNewLevel
 jsr initCharsNextLevel	;loads character, lives, and enemies into next level
checkEnemy:
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

portalAnimation:
 jsr $e55f
 lda screen_colour
 sta temp_colour
 ldx #$08
portalAnimTop:
 lda temp_colour
 sta $900f		 		; store in screen and border register
 adc #$33
 sta temp_colour
 lda #$4f
 jsr timerLoop
 dex
 cpx #$00
 bne portalAnimTop
 jmp gameLoopTop

drawCharacter:
 lda char_colour
 sta graphics_colour
 ldy col_bot
 cpy #$01
 beq drawBottom
 jsr getRowColForm
 lda p1_sprite		; 'B'
 bcs drawBottom
 bcc drawTop
 lda #$4f		; arbitrary number for timer
 jsr timerLoop	; jump to timer
 rts

drawBottom:
 jsr getRowColForm
 lda p1_sprite ;#$01
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
 lda graphics_colour		; char_colour to black
 sta char_colour_loc_top,x	; store char_colour in new location too
 rts

drawToScreenBot:
 sta graphics_bot,x	; store space
 lda graphics_colour		; char_colour to black
 sta char_colour_loc_bot,x	; store char_colour in new location too
 rts

collAnimationLoop:
 jsr collMovementCheck
 jsr checkColBot
 jsr collisionAnimation
 lda coll_char_colour
 sta graphics_colour
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
 rts

checkColBot:
 lda col
 cmp col_mid_down
 bmi col_bot_set_0
 beq checkColMid
 bpl col_bot_set_1

checkColMid:
 lda row
 ldx current_key
 cpx a
 beq checkLeftColBot
 cpx d
 beq checkRightColBot
 lda col
 cpx w
 beq checkUpColBot
 cpx s
 beq checkDownColBot
 rts

checkLeftColBot:	;animation moves right
 cmp row_mid_right
 bmi col_bot_set_0
 bpl col_bot_set_1
checkRightColBot:		;animation moves left
 cmp row_mid_right
 bmi col_bot_set_0
 bpl col_bot_set_1
checkUpColBot:			;animation moves down
 cmp col_mid_down
 bmi col_bot_set_0
 bpl col_bot_set_1
checkDownColBot:		;animation moves up
 cmp col_mid_up
 bmi col_bot_set_0
 bpl col_bot_set_1
col_bot_set_1:
 lda #$01
 sta col_bot
 rts
col_bot_set_0:
 lda #$00
 sta col_bot
 rts

collisionAnimation:
 jsr getRowColForm
 ldy col_bot
 cpy #$01
 beq storeNewCollBot
 lda coll_char_colour
 sta graphics_colour
 lda p1_sprite
 jsr drawToScreen
 jmp collAnimNext
storeNewCollBot:
 lda coll_char_colour
 sta graphics_colour
 lda p1_sprite;#$02
 jsr drawToScreenBot
collAnimNext:
 lda #$40;#$6e		; arbitrary number for timer
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
 cpx row_end		;check if far left
 beq collRet
 inc row			;else increment row
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
 dec col
 rts

collTop:
 lda col_bot
 cmp #$01
 bne collRightTop
 lda graphics_bot,x
 jmp compareCollRight
collRightTop:
 lda graphics_top,x
compareCollRight:
 cmp wall_sprite
 beq collTopSet1
 cmp enemy_sprite
 beq collTopSet1
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
  lda col
  cmp row_newLevel_begin		;checking top
  bne checkright
  inc current_room
  inc current_room
  lda col_newLevel_end
  sta col
  dec col
  jsr levelDispatch
  rts
checkright:
  lda row
  cmp row_end
  bne checkbottom
  inc current_room
  lda row_newLevel_begin
  sta row
  inc row
  jsr levelDispatch
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
  jsr levelDispatch
  rts
checkLeft:
  lda row
  cmp row_begin
  bne error
  dec current_room
  lda row_end
  sta row
  dec row
  jsr levelDispatch

error: ;shouldn't happen
  rts
levelDispatch:
  ldy current_room
  lda rooms,y
  cmp #$01
  bne check3
  jsr loadLevel1
  rts
check3:
  cmp #$02
  bne check4
  jsr loadLevel2
  rts
check4:
  cmp #$03
  bne check5
  jsr loadLevel3
  rts
check5:
  cmp #$04
  bne check6
  jsr loadLevel4
  rts
check6:
	;temporarily goes to room 1 just for continuity.
 ;lda #$00
 ;sta current_room	;store in current room variables
 ;jsr loadLevel3
 rts

 ;changed all the $a6 to $66 because the character changes depending on whether
 ;we use jsr ffd2 or sta $96xx/97xx
level1top:
  dc.b $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
level1topend

level1bottom:
  dc.b $5b,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$66,$66,$66,$66,$66,$66,$66,$66,$20,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$5b,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
level1bottomend


level2top:
  dc.b $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  dc.b $20,$20,$66,$66,$66,$66,$66,$66,$66,$20,$20,$20,$20,$20,$66,$66,$66,$66,$66,$66,$66,$66
  dc.b $20,$20,$20,$20,$20,$20,$20,$20,$66,$20,$20,$20,$20,$20,$66,$20,$20,$20,$20,$20,$20,$20
  dc.b $20,$20,$20,$20,$20,$20,$20,$20,$66,$20,$20,$20,$20,$20,$66,$20,$20,$20,$20,$20,$20,$20
  dc.b $20,$20,$20,$20,$20,$20,$20,$20,$66,$20,$20,$20,$20,$20,$66,$20,$20,$20,$20,$20,$20,$20
  dc.b $20,$20,$20,$20,$20,$20,$20,$20,$66,$20,$20,$20,$20,$20,$66,$20,$20,$20,$20,$20,$20,$20
  dc.b $20,$20,$20,$20,$20,$20,$20,$20,$66,$20,$20,$20,$20,$20,$66,$20,$20,$20,$20,$20,$20,$20
  dc.b $20,$20,$20,$20,$20,$20,$20,$20,$66,$20,$20,$20,$20,$20,$66,$20,$20,$20,$20,$20,$20,$20
  dc.b $20,$20,$20,$20,$20,$20,$20,$20,$66,$20,$20,$20,$20,$20,$66,$20,$20,$20,$20,$20,$20,$20
  dc.b $20,$20,$20,$20,$20,$20,$20,$20,$66,$20,$20,$20,$20,$20,$66,$20,$20,$20,$20,$20,$20,$20
  dc.b $20,$20,$20,$20,$20,$20,$20,$20,$66,$20,$20,$20,$20,$20,$66,$66,$66,$66,$66,$66,$66,$66
level2topend

level2bottom:
  dc.b $20,$20,$20,$20,$20,$20,$20,$20,$66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$5b
  dc.b $20,$20,$20,$20,$20,$20,$20,$20,$66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  dc.b $20,$20,$20,$20,$20,$20,$20,$20,$66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  dc.b $20,$20,$20,$20,$20,$20,$20,$20,$66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  dc.b $20,$20,$20,$20,$20,$20,$20,$20,$66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  dc.b $20,$20,$20,$20,$20,$20,$20,$20,$66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  dc.b $20,$20,$20,$20,$20,$20,$20,$20,$66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  dc.b $20,$20,$20,$20,$20,$20,$20,$20,$66,$66,$20,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66
  dc.b $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  dc.b $66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$20,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66
  dc.b $66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$5b,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66
level2bottomend

level3top:
  dc.b $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  dc.b $66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$5b,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
level3topend

level3bottom:
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$5b
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$7f,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66
level3bottomend

level4top:
  dc.b $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
  dc.b $66,$66,$66,$66,$66,$66,$66,$66,$66,$5b,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$76,$76,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$76,$76,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$76,$76,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$76,$76,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
level4topend

level4bottom:
  dc.b $5b,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$76,$76,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$76,$76,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$76,$76,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$76,$76,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
  dc.b $66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66
level4bottomend

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
char_colour:			dc.b #$55
wall_colour:			dc.b #$44
life_colour:			dc.b 2
screen_colour:		dc.b #$0f
graphics_colour: 	dc.b 0
timer_loop:			dc.b 0
coll_char_colour:	dc.b 0
w:						dc.b #9
a:							dc.b #17
s:							dc.b #41
d:							dc.b #18
f1:						dc.b #39
f3:				dc.b #47
f5:							dc.b #55
p1_sprite:				dc.b #81		;81 = circle
lives_sprite:			dc.b #83		;heart
enemy_sprite:		dc.b #87		;circle
wall_sprite:			dc.b $66		;weird checkered square thingy
portal_sprite:			dc.b $7F 		;#209
space_sprite:			dc.b #32    ;$20
row_begin:			dc.b #$00
row_newLevel_begin: dc.b #$01
col_newLevel_end:   dc.b #$14
row_mid_left:		dc.b #$0f
row_mid_right:		dc.b #$0d
row_end:				dc.b #$15
col_begin:				dc.b #$00
col_mid_down:		dc.b #$0b
col_mid_up:			dc.b #$0d
col_end:				dc.b #$16
seed:					dc.b #$74 ;constant seed
current_room:		dc.b 0
rooms:          dc.b 0,0,0,0
draw_num_enemies:	dc.b 0
door_sprite       dc.b $5b
col_mid           dc.b #$0a
isCollision		dc.b #$00
titleName:		dc.b #02, #15, #15, #16	;boop.  TODO: Better title
titleNameEnd
titleAuthors	dc.b #03,#04, #32, #12, #13, #32, #11, #13	;cd lm km
titleAuthorsEnd
gameOverMessage: dc.b #07, #01, #13, #05, #32, #15, #22, #05, #18, #33
gameOverMessageEnd
enemystatus: dc.b 0,0,0,0
enemyypos: dc.b 0,0,0,0
enemyxpos: dc.b 0,0,0,0
;in future: full names, etc.
temp dc.b 0
