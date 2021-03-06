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
 jsr $ffbd 		;initialize clock

;########################TITLE PAGE#######################
 ;Change character set
  ldx #$00
initCharset:
copyLoop:
 lda leftpose,x
 sta $1c50,x
 inx
 cpx #$50
 bne copyLoop
 ldx #$00
copyNums:
 lda $8180,x
 sta $1c00,x
 inx
 cpx #$50
 bne copyNums
 ldx #$00
 lda #$ff
 sta $9005

 lda #$55	;green
 sta drawColour

;Super crappy title page.  TODO: Fix and make legible.
;I blasted away the names, but will put back later.
titleScreen:
 jsr $e55f		;clear screen
 ;sta $900f
 lda #160
 jsr SOUNDONMID
 ;jsr levLoadMain 	;load level 3 to screen
 ;lda temp_colour
 ;sta $900f
 ; lda #$09
 ; sta Scratch2
 ; lda #$05			;screen coord for title; too lazy to use x,y coords
 ; sta col
 ; lda #titleNameEnd-titleName-1	;length
 ; sta row
; drawTitleLoop:		;draw title to screen
 ; ldx row
 ; lda titleName,x
 ; jsr incIndex
 ; ldy col
 ; jsr drawToPlayfield
 ; dec row		;index -1
 ; lda row
 ; cmp #$00
 ; bpl drawTitleLoop
 ; lda #$07
 ; sta Scratch2
 ; lda #$06		;screen coord for author names
 ; sta col
 ; lda #titleAuthorsEnd-titleAuthors-1
 ; sta row
; drawAuthorLoop:
 ; ldx row
 ; ldy col
 ; lda titleAuthors,x
 ; jsr incIndex
 ; jsr drawToPlayfield
 ; dec row
 ; lda row
 ; cmp #$00
 ; bpl drawAuthorLoop
 ; lda temp_colour
 ; sta $900f		 		; store in screen and border register
 ; lda #$00
 ; sta Scratch
drawTitleAnimation:	;this is a loop because don't need to constantly redraw title names
 jsr playTheme
 jsr scrColTheme
 ;jsr playTheme

titleInput:
 lda $00c5			;check for start.  Only can press start right now. Probably should
 sta current_key
 cmp #64			;no key pressed
 beq drawTitleAnimation
 cmp #f1			;have this or else resetting the game immediately puts you back into game
 beq drawTitleAnimation
 cmp #f5			;exit key pressed
 bne gameLoopTop
 jsr $e55f
 brk


scrColTheme:
 jsr $ffde 		;read clock
 cmp #$01
 bne titleInput
 sec	;set carry bit
 dec drawColour	;random colour, pretty much
 lda drawColour
 sta $900f		 		; store in screen and border register
 rts

playTheme:
 jsr $ffde
 and #$07	;can mess around with this bitmask to change time
 cmp #$00	;can mess around with this to also change time
 bne playThemeEnd	;can also mess around with this
 ;jsr SOUNDOFFMID
 ldx Scratch	;use Scratch because it isn't being used for anything else
 lda theme,x	;load theme
 jsr SOUNDONMID	;load to sound
 inc Scratch
 lda Scratch
 cmp #themeEnd-theme	;check bounds
 bne playThemeEnd
 lda #$00	;reset index
 sta Scratch
playThemeEnd:
 rts

incIndex:
 sty Scratch
 ldy Scratch2
incIndexLoop:
 inx
 dey
 cpy #$00
 bne incIndexLoop
 ldy Scratch
 rts
;#########################END TITLE PAGE##########################

gameLoopTop:
 lda #$00
 sta prev_note
 ;lda #$2f
 ;lda #$00
 lda #00
 sta score_ones
 sta score_tens
 lda #$5f		; arbitrary number for timer
 jsr timerLoop
 ;Setup new location for characters
 jsr $e55f       		; clear screen

;changes the border and background char_colour -> page 175 of vic do manual
 lda #screen_colour		 ; 0f ->this makes a green border and black background
 sta $900f		 		; store in screen and border register

 ;jsr genRoom		;these have been moved to initCharsNextLevel
 ;jsr initPRowAddr
 ;jsr initRoomAddr
 ;jsr loadLevel
 jsr SOUNDOFFMID
 ldx #init_lives
 stx lives
 jmp initChars

 ;########################## METHODS TO INIT DATA AND UTILITY METHODS HERE #####

getRandom:
 lda $9114
 adc $9004
 clc
 sta seed
 rts

genRoom:
 ldy #$00
genRoomLoop:
 jsr getRandom
 and #$07
 sta rooms,y
 iny
 cpy #$09
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
  inx
  lda #<level5 ;low byte
  sta room_addr,x
  inx
  lda #>level5
  sta room_addr,x
  inx
  lda #<level6 ;low byte
  sta room_addr,x
  inx
  lda #>level6
  sta room_addr,x
  inx
  lda #<level7 ;low byte
  sta room_addr,x
  inx
  lda #>level7
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
  inx
  lda #<prow5
  sta prow_addr,x
  inx
  lda #>prow5
  sta prow_addr,x
  inx
  lda #<prow6
  sta prow_addr,x
  inx
  lda #>prow6
  sta prow_addr,x
  inx
  lda #<prow7
  sta prow_addr,x
  inx
  lda #>prow7
  sta prow_addr,x
  inx
  lda #<prow8
  sta prow_addr,x
  inx
  lda #>prow8
  sta prow_addr,x
  inx
  lda #<prow9
  sta prow_addr,x
  inx
  lda #>prow9
  sta prow_addr,x
  inx
  lda #<prowa
  sta prow_addr,x
  inx
  lda #>prowa
  sta prow_addr,x
  inx
  lda #<prowb
  sta prow_addr,x
  inx
  lda #>prowb
  sta prow_addr,x
  inx
  lda #<prowc
  sta prow_addr,x
  inx
  lda #>prowc
  sta prow_addr,x
  rts

 ; mod function
 ; input is X(register) mod Y(register)
 ; output is returned in the Accumulator
mod:
 txa					; put the dividend in the Accumulator
 sty divisor		; store the divisor in a variable since you can't subtract y from a
 cmp divisor		; check to see if the dividend is less than the divisor
 bmi modEnd		; already have the answer, return it
modLoop:
 sec					; set the carry for math
 sbc divisor		; subtract the divisor from the dividend
 cmp divisor		; check to see if the remainder is less than the divisor
 bpl modLoop		; if it isnt then subtract again, else return answer
modEnd:
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
 sty drawY
 sta drawChar					; store the Character to draw
 cpy #$0a
 BPL drawToPlayfieldBot		; if the Y coordinate is in the bootom then go to that method (y >= 10)
 jsr drawMath
 lda drawColour					; get the colour for the character
 sta char_colour_loc_top,y	; put the colour on the screen
 lda drawChar					; get the character back
 sta graphics_top,y				; draw it to the screen
 ldy drawY
 rts

drawToPlayfieldBot:
 tya									; put Y in Accumulator
 sec
 sbc #$0a							; subtract 10
 tay									; put Y back
 jsr drawMath
 lda drawColour					; get the colour for the character
 sta char_colour_loc_bot,y	; put the colour on the screen
 lda drawChar					; get the character back
 sta graphics_bot,y				; draw it to the screen
 ldy drawY
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
 lda drawColour					; get the colour for the character
 sta status_colour_top,x		; put the colour on the screen
 lda drawChar					; get the character back
 sta status_loc_top,x			; draw it to the screen
 rts

drawToStatusBot:
 sty drawY
 dey
 jsr drawMath
 lda drawColour					; get the colour for the character
 sta status_colour_bot,y		; put the colour on the screen
 lda drawChar					; get the character back
 sta status_loc_bot,y			; draw it to the screen
 ldy drawY
 rts

drawScore:
 lda cur_wall_col
 sta drawColour
 lda score_tens
 ldx #$14
 ldy #$00
 jsr drawToStatus
 ldx #$15
 lda score_ones
 jsr drawToStatus
 rts

;##########                                                GET FROM SCREEN                                                ##########
;				used to get the character on the playfield from and X, Y location
;				Takes 2 inputs returns a character in the Accumulator
;				inputs:		X register = X location
;								Y register = Y location
getFromScreen:
 cpy #$0a
 sty drawY
 BPL getFromScreenBot		; if the Y coordinate is in the bootom then go to that method (y >= 10)
 jsr drawMath
 lda char_colour_loc_top,y
 sta drawColour
 lda graphics_top,y				; get the character back from the screen
 ldy drawY
 rts

getFromScreenBot:
 tya									; put Y in Accumulator
 sec
 sbc #$0a							; subtract 10
 tay									; put Y back
 jsr drawMath
 lda char_colour_loc_bot,y
 sta drawColour
 lda graphics_bot,y				; get the character back from the screen
 ldy drawY
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
 ldx #$00
 stx Ycoor
 jsr putDoors
 jsr putDrops
 rts

putDoors:
  lda cur_wall_col
  sta drawColour
  lda current_room
  cmp #$00
  bne room1
  jsr putRightDoor
  jsr putTopDoor
  rts
room1:
  lda current_room
  cmp #$01
  bne room2
  jsr putTopDoor
  jsr putRightDoor
  jsr putLeftDoor
  rts
room2:
  lda current_room
  cmp #$02
  bne room3
  jsr putTopDoor
  jsr putLeftDoor
  rts
room3:
  lda current_room
  cmp #$03
  bne room4
  jsr putBottomDoor
  jsr putRightDoor
  jsr putTopDoor
  rts
room4:
  lda current_room
  cmp #$04
  bne room5
  jsr putLeftDoor
  jsr putTopDoor
  jsr putRightDoor
  jsr putBottomDoor
  rts
room5:
  lda current_room
  cmp #$05
  bne room6
  jsr putLeftDoor
  jsr putTopDoor
  jsr putBottomDoor
  rts
room6:
  lda current_room
  cmp #$06
  bne room7
  jsr putBottomDoor
  jsr putRightDoor
  rts
room7:
  lda current_room
  cmp #$07
  bne room8
  jsr putLeftDoor
  jsr putBottomDoor
  jsr putRightDoor
  rts
room8:
  lda current_room
  cmp #$08
  bne roomError
  jsr putLeftDoor
  jsr putBottomDoor
  jsr putPortal
  rts
roomError:
  brk ;Shouldn't happe
  rts


putRightDoor:
 ldy #$09
 ldx #$15
 lda #door_sprite
 jsr drawToPlayfield
 iny
 jsr drawToPlayfield
 rts
putTopDoor:
 ldx #$0a
 ldy #$00
 lda #door_sprite
 jsr drawToPlayfield
 inx
 jsr drawToPlayfield
 rts
putBottomDoor:
  ldx #$0a
  ldy #$13
  lda #door_sprite
  jsr drawToPlayfield
  inx
  jsr drawToPlayfield
  rts
putLeftDoor:
 ldy #$09
 ldx #$00
 lda #door_sprite
 jsr drawToPlayfield
 iny
 jsr drawToPlayfield
 rts
putPortal:
 ldy #$0a
 ldx #$0b
 lda #portal_sprite
 jsr drawToPlayfield
 rts
putDrops: ;Randomly decide if portal, potion or sword or none
dropLoop:
 jsr getRandom
 and #$0f
 tay
 jsr getRandom
 and #$0f
 tax
 jsr getFromScreen
 cmp #space_sprite
 bne dropLoop
 jsr getRandom
 cmp #$04
 bcc drop1
 cmp #$c8
 bcs drop2
 cmp #$39
 bne dropEnd
 jsr putPortal
 rts
drop1:
 jsr putSword
 rts
drop2:
 jsr putPotion
 rts
dropEnd:
 rts

putSword: ;Assumes x coordinate and y coordinate to draw will be passed in
  lda #$01
  sta drawColour	;white sword
  lda #sword_sprite
  jsr drawToPlayfield
  rts

putPotion: ;Assumes x coordinate and y coordinate to draw will be passed in
  lda #life_colour
  sta drawColour
  lda #lives_sprite
  jsr drawToPlayfield
  rts

drawPRow: ;Expects address of row to draw in $fd. Saves callers y reg
  ldx #$00
  lda cur_wall_col
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
;input: x-reg: number of lives to draw to screen
drawLives:		;draw lives to screen
 lda #life_colour
 sta drawColour
 lda #lives_sprite
 jsr drawToStatus
 dex
 cpx #$01
 bpl drawLives
 rts

gameOver: ;
;TODO: Print "Game over"
 lda #$3e		 ; game over border/background colour blue/light blue
 sta $900f		 ; store in screen and border register
 jsr SOUNDOFFALL
 lda #135
 jsr SOUNDONHIGH	;play "lose sound"
gameOverEnd:	 ; bounce branch to get other subroutines to top of gameLoopTop
 lda $00c5		 ; current key held down -> page 179 of vic20 manual
 cmp #f5 ;quit
 beq quitBounce
 cmp #f1 ;f1 to restart
 bne gameOverEnd
 jsr SOUNDOFFHIGH	;turn sound off
 jmp titleScreen

initEnemyLocation:
 jsr getRandom	;just gets a random colour for now, change this when have more memory
 and #$07
 sta enemyCount
 lda #$0
 sta enemyLoopCount
initEnemyLoop:
 jsr getRandom
 and #$0f
 tax
 jsr getRandom
 and #$0f
 tay
 jsr getFromScreen
 cmp #space_sprite
 bne initEnemyLoop
 txa
 ldx enemyLoopCount
 sta enemyxpos,x
 pha
 tya
 sta enemyypos,x
 pla
 tax
 lda #enemy_sprite
 jsr drawToPlayfield
 inc enemyLoopCount
 lda enemyLoopCount
 cmp enemyCount
 bcc initEnemyLoop
 rts
quitBounce:
 jmp quit

initChars:
 lda #init_char_col
 sta char_colour		;initial colour for character
 lda #wall_colour
 sta cur_wall_col		;initial colour for walls
 lda #$01
 sta row		; row
 lda #$0b
 sta col		; col
initCharsNextLevel:	;this is a branch so it skips over assigning row and col to
 ;lda #init_char_col
 ;sta char_colour		;we don't want to reset the character colour every level
 jsr drawScore
 jsr genRoom
 jsr initPRowAddr
 jsr initRoomAddr
 jsr loadLevel
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
 lda char_colour
 sta cur_char_col
 sta drawColour
 lda p1_sprite		; 'B'
 ;jsr drawToScreen
 ldx row
 ldy col
 jsr drawToPlayfield
 pla			; pull acc from stack
 sta $9005		; store in char memory

top:			; top of loop
 ;jsr playTheme	;this isn't really working atm
 jsr isInvincible	;check if player still "injured"
 jsr chktim
 lda $9005			; load char memory
 pha				; push to stack
 lda $00c5		 	; current key held down -> page 179 of vic20 manual
 sta current_key
 jsr move		;move also handles collision detection, but not as bad as before

;--------drawing player to screen. TODO: make into a subroutine or smth
 ldx row
 ldy col
 lda cur_char_col
 sta drawColour
 lda p1_sprite
 jsr drawToPlayfield
 jsr drawTimer
 jsr EnemyMove
;;--------------------End of drawing character to screen


next:
  ;Wait for user to press enter, and restore the character set
 pla			; pull acc from stack
 sta $9005 		; store in char mem
 lda current_key		 ; current key held down -> page 179 of vic20 manual
 cmp f5		 ; check if Q is pressed -> quit
 beq quit		 ;continue input
 bne top
quit:
 jsr $e55f       ; clear screen before exiting
 brk			 ; quit

EnemyMove:
 lda enemyCount
 sta enemyLoopCount
EnMove1:
 ldx enemyLoopCount
 lda enemyypos,x
 sta enemyY			; store temp Y value
 lda enemyxpos,x
 sta enemyX			; store temp X value
 jsr clearEnemy
 jsr getRandom
 and #$01
 cmp #$00
 beq CheckX
 lda enemyypos,x ; else check Y
 cmp col
 bcc LessY
 dec enemyY
 clc
 bcc enemyCheckColl
LessY:
 inc enemyY
 clc
 bcc enemyCheckColl
CheckX:
 ldx enemyLoopCount
 lda row
 sta $1000
 lda enemyypos,x ; else check Y
 cmp row
 bcc LessX
 dec enemyX
 clc
 bcc enemyCheckColl
LessX:
 inc enemyX
enemyCheckColl:
 ldx enemyX
 ldy enemyY
 jsr getFromScreen
 cmp #space_sprite			; check for space
 bne endMove
 lda enemyX						; update location
 ldx enemyLoopCount
 sta enemyxpos,x
 lda enemyY
 sta enemyypos,x
endMove:
 jsr drawEnemy
 dec enemyLoopCount
 lda enemyLoopCount
 bpl EnMove1
 rts

clearEnemy:				; clears the enemy from the screen and also gets its colour
 ldy enemyY
 ldx enemyX
 jsr getFromScreen			; to get the enemies colour
 lda drawColour
 sta enemyColour		; save enemy colour
 lda #space_sprite
 jsr drawToPlayfield
 rts
drawEnemy:
 ldx enemyLoopCount
 ldy enemyypos,x
 tya
 ldy enemyLoopCount
 ldx enemyxpos,y
 tay
 lda enemyColour
 sta drawColour
 lda #enemy_sprite
 jsr drawToPlayfield
 rts

; screen registers 1e00-1fff -> 7680-8191 -> 511
;INPUT: accumulator: current key
move:
 cmp #f1 			;f1 to restart
 beq gameOverEndBounce	;bounce to game over to take us to gameLoopTop
 ldx row
 ldy col
 cmp #a		 	;a pressed
 beq playleft	 	; move left
 cmp #d		 	;d pressed
 beq playright	 	; move right
 cmp #w	 		;w pressed
 beq playup
 cmp #s			 ;s pressed
 beq playdown
 cmp #atk
 beq playattack
 rts
gameOverEndBounce:
 jmp gameOverEnd

;INPUT: current direction pressed
playleft:
  sta prev_direction	;store for attack purposes
  jsr left		 ; subroutine to move left
  rts
;INPUT: current direction pressed
playright:
 sta prev_direction
 jsr right		; subroutine to move right
 rts
;INPUT: current direction pressed
playup:
 sta prev_direction
 jsr up
 rts
;INPUT: current direction pressed
playdown:
 sta prev_direction
 jsr down
 rts

;INPUT: none
;overwrites accumulator, x, y registers
playattack:
 ldx row
 ldy col
 ;draw sword in direction
 jsr attackDirection	;get player + direction they currently are in
 jsr getFromScreen 		;get what is at that coordinate
 ldx row				;need to reload x and y to potentially draw sword
 ldy col
 cmp #space_sprite		;nothing there
 beq drawSwordAttack	;draw sword to screen
 ;draw sword
 cmp #enemy_sprite		;hit enemy
 beq hitEnemy
 ;draw sword and decrease enemy health
 ;check for collision
 ;play sound
 rts

;INPUT: X - x directoin
;		Y - y direction
;TODO: remove sword from screen after time
;different attack directions depending on attack direction
drawSwordAttack:
 jsr attackDirection	;get player + direction moved
 lda prev_direction
 cmp #w
 beq swordW
 cmp #a
 beq swordA
 cmp #s
 beq swordS
 cmp #d
 beq swordD
swordW:
 lda #0
 jmp drawSwordAttack2
swordA:
 lda #1
 jmp drawSwordAttack2
swordS:
 lda #2
 jmp drawSwordAttack2
swordD:
 lda #3
drawSwordAttack2:
 ; lda char_colour
 ; sta drawColour
 ; jsr drawToPlayfield	;draw to screen
 lda #241				;play sword sound
 jsr SOUNDONLOW
 rts

hitEnemy:
 jsr attackDirection	;get location of enemy
 lda char_colour	;p1 colour
 and #$07		;get value between 0-8 for colour
 ;lsr		;/2
 ;clc
 ;adc #$01	;add 1 so that lowest colour hits for one
 sta Scratch
 lda drawColour	;enemy colour
 and #$07		;mask because drawColour returns values >8
 sec
 sbc Scratch	;enemy colour - p1 colour
 cmp #$01		;white, the weakest
 bmi enemyDead
 sta drawColour	;store enemy colour if not dead
 ;dec drawColour		;decrement enemy colour
 lda #enemy_sprite
 jsr drawToPlayfield	;draw enemy to playfield
 lda #200				;player hits an enemy
 jsr SOUNDONLOW
 rts

enemyDead:
 dec enemyCount
 lda #space_sprite
 jsr drawToPlayfield	;draw spacea and erase enemy
 lda #175				;different sound when enemy is dead
 jsr SOUNDONLOW
 rts

;INPUT: X - x direction
;		Y - y direction
;Does not overwrite x and y
attackDirection:
 lda prev_direction
 cmp #w
 beq atkw
 cmp #a
 beq atka
 cmp #s
 beq atks
 cmp #d
 beq atkd
 rts

atkw:
 dey
 rts

atka:
 dex
 rts

atks:
 iny
 rts

atkd:
 inx
 rts

;INPUT: accumulator - note to play
SOUNDONLOW:
 sta $900a
 rts

SOUNDOFFLOW:
 lda #0
 sta $900a
 rts

;INPUT: accumulator - note to play
SOUNDONHIGH:
 sta $900c
 rts

SOUNDOFFHIGH:
 lda #0
 sta $900c
 rts

SOUNDONMID:
 sta $900b
 rts

SOUNDOFFMID:
 lda #0
 sta $900b
 rts

SOUNDOFFALL:
 lda #0
 sta $900a
 sta $900b
 sta $900c
 rts

;INPUT: none
chktim:
 jsr $ffde ;call timer
 and #$0f
 cmp #$0f
 bpl chktimEnd
 jsr SOUNDOFFLOW
 jsr SOUNDOFFHIGH
chktimEnd:
 rts
;checks space you want to move into
;if there is something there, return 1 in y
;else, return 0 in y
left:
 lda #$0a
 sta p1_sprite
 lda #space_sprite
 cpx #row_begin				; check if end of screen on left
 beq moveEnd
 jsr drawToPlayfield	;removes previous location
 dec row				; rows -1
 jsr checkCollision	;checks if object in space
 jsr collisionAction
 cmp #$00	;space
 beq moveEnd
 inc row		;put character back into original location
 rts

right:			;similar as above left subroutine
 lda #$0b
 sta p1_sprite
 lda #space_sprite
 cpx #row_end		 ; check if x =21 (end of right)
 beq moveEnd
 jsr drawToPlayfield
 inc row 		 ; rows +1
 jsr checkCollision
 jsr collisionAction
 cmp #$00
 beq moveEnd
 dec row
 rts

up:
 lda #$0d
 sta p1_sprite
 lda #space_sprite
 cpy #col_begin
 beq moveEnd
 jsr drawToPlayfield
 dec col 			 ; cols -1
 jsr checkCollision		;checks if next space is occupied
 jsr collisionAction
 ;checks for various characters
 ;potential animation loop
 ;at the moment, it just doesn't let the player eat the objects
 cmp #$00		;if there is an obstacle, then move back to previous space
 beq moveEnd
 inc col
 rts

down:
 lda #$0c
 sta p1_sprite
 lda #space_sprite
 cpy #col_end
 beq moveEnd
 jsr drawToPlayfield
 inc col 		 ; cols + 1
 jsr checkCollision
 jsr collisionAction
 cmp #$00
 beq moveEnd
 dec col
 rts

moveEnd:
 rts


;============================COLLISION DETECTION===================
;Checks for collisions between items
;returns value in accumulator based on what player hits
;X-reg: returns sprite in space
checkCollision:
 ldx row
 ldy col
 lda #$00
 jsr getFromScreen
 tax
 cpx #space_sprite	;everything else but space is collision at moment
 beq noColl
 lda #$01
 cpx #wall_sprite	;collision with wall
 beq endColl
 lda #$02
 cpx #portal_sprite	;portal
 beq endColl
 lda #$03
 cpx #enemy_sprite	;enemy
 beq endColl
 lda #$04	;door sprite
 cpx #door_sprite
 beq endColl
 lda #$05
 rts

noColl:		;no collision
 lda #$00
endColl:
 rts

;Subroutine that takes value in accumulator and
;does collision based on value
;input x-reg: sprite in current space
collisionAction:
 cmp #$01	;wall
 beq wallColl
 cmp #$02	;portal
 beq portalColl
 cmp #$03	;enemy
 beq enemyColl
 cmp #$04	;door
 beq doorColl
 cmp #$05	;drop
 beq dropColl
 bne noColl	;default

;changes colour of player and prevents them from running into wall
wallColl:
 lda #$ef
 sta cur_char_col
 lda #$00
 sta invinc_time
 lda #140				;play sword sound
 jsr SOUNDONHIGH
 lda #$01		;for no move function
 rts

					;TODO: not completely reset game (like switching rooms)

;crash into enemy: change player colour lose life if not invincible
enemyColl:
 ;lda #$02
; sta cur_char_col
 jsr checkInvincible
 lda #$01		;no move
 rts


;TODO: DOOR COLLISION
doorColl:
 jsr loadNewLevel
 jsr initEnemyLocation		;load enemies
 rts

;input: x-reg: sprite in current space
dropColl:
 cpx #lives_sprite		;check if heart
 bne swordCheck
 lda lives
 cmp #init_lives	;check if has max amount of lives already
 beq dropCollSound
 inc lives				;increase number of lives
 ldx lives			;load to draw to screen0
 jsr drawLives	;re-draw lives
dropCollSound:
 lda #220		;action sound
 jsr SOUNDONHIGH
 lda #$00
 rts

swordCheck:		;check if sword
 cpx #sword_sprite
 bne dropCollEnd
 lda char_colour
 and #$07
 tax
 jsr getRandom	;random number to check whether sword is "good" or "bad"
 and #$01
 cmp #$00
 bne negSword		;bad sword
 lda #210			;good sword sound
 cpx #$07		;check if already have max sword value
 beq dropSwordEnd
 inc char_colour
 inc cur_char_col
 bne dropSwordEnd
negSword:
 lda #135	;bad sword sound
 cpx #$01
 beq dropSwordEnd		;already have weakest sword value
 dec char_colour
 dec cur_char_col
dropSwordEnd:
 ;lda #210		;action sound
 jsr SOUNDONHIGH
dropCollEnd:
 lda #$00	;move over drop
 rts
;portal animation
portalColl:
 lda cur_wall_col	;increment wall colour
 and #$07
 cmp #$07
 beq resetWallCol
 inc cur_wall_col
 bne portalColl2
resetWallCol:
; lda #$00		;allows for "invisible" level; use lda #$01 to reset to white
 lda #$01	;white
 sta cur_wall_col
portalColl2:
 jsr increaseScore
 lda #$01
 sta row
 lda #$0a
 sta col
 jsr genRoom
 lda #$00
 sta current_room
 jsr loadLevel
 lda #175
 jsr SOUNDONHIGH
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
 jsr SOUNDOFFMID
 lda #screen_colour
 sta $900f
; jmp gameLoopTop		;note: this currently resets game.
 jmp initCharsNextLevel

;check if invincible
checkInvincible:
 lda cur_char_col
 cmp char_colour
 beq notInvincible
 rts

notInvincible:
 ldx row
 ldy col
 jsr getFromScreen	;get colour value
 lda drawColour
 and #$07		;get colour value between 0-7
 lsr			;divide colour by 2
 sta Scratch	;store in Scratch variable
 jsr loseLife
 lda #135
 jsr SOUNDONLOW
 lda #$06
 sta invinc_time
 lda #$02
 sta cur_char_col
 rts

isInvincible:
 lda cur_char_col
 cmp char_colour
 beq stillInvincible 	;jump over
 lda invinc_time
 cmp #$00
 bne stillInvincible
 lda char_colour
 sta cur_char_col
stillInvincible:
 dec invinc_time
 sta cur_char_col
invinRet:
 rts

;ARGUMENTS: Scratch: damage given to enemy
loseLife:
 lda #space_sprite
 ldx lives		;x value
 ldy #$00
 jsr drawToStatus	;draw over rightmost life, then decrement
 dec lives
 dec Scratch		;Scratch used to loop until done giving damage
 lda Scratch
 cmp #$00
 bpl loseLife		;loop until Scratch = 0
 lda lives
 cmp #$00			;check if player out of lives
 bpl loseLifeNext	;still alive
 jmp gameOver		;rip
loseLifeNext:
 rts

increaseScore:
 lda score_ones
 cmp #9
 bne incOnes
 inc score_tens
 lda #0
 sta score_ones
 jsr drawScore
 rts
 ;jmp portalColl
incOnes:
 inc score_ones
 jsr drawScore
 rts

drawTimer:	;timer to draw objects
 lda #$4f		; arbitrary number for timer
 jsr timerLoop	; jump to timer
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
  cmp #00		;checking top
  bne checkright
  inc current_room
  inc current_room
  inc current_room
  lda #col_newLevel_end-3
  sta col
  jsr loadLevel
  rts
checkright:
  lda row
  cmp #$15
  bne checkbottom
  inc current_room
  lda #$01
  sta row
  inc row
  jsr loadLevel
  rts
checkbottom:
  lda col
  cmp #$13
  bne checkLeft
  dec current_room
  dec current_room
  dec current_room
  lda #col_begin
  sta col
  inc col
  inc col
  jsr loadLevel
  rts
checkLeft:
  lda row
  cmp #$00
  bne error
  dec current_room
  lda #row_end-2
  sta row
  jsr loadLevel
  rts

error: ;shouldn't happen
  brk
  rts

enemyxpos: dc.b 0,0,0,0,0,0,0,0
enemyypos: dc.b 0,0,0,0,0,0,0,0

level0: dc.b $01,$04,$04,$04,$04,$04,$04,$04,$02,$02,$02,$02,$04,$04,$04,$04,$04,$04,$04,$01
level1: dc.b $01,$02,$02,$02,$03,$03,$03,$02,$02,$03,$03,$03,$02,$02,$03,$03,$03,$02,$02,$01
level2: dc.b $01,$04,$04,$04,$04,$04,$04,$04,$02,$02,$02,$02,$04,$04,$04,$04,$04,$04,$04,$01
level3: dc.b $01,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$01
level4: dc.b $01,$05,$05,$05,$05,$05,$05,$05,$02,$02,$02,$02,$05,$05,$05,$05,$05,$05,$05,$01
level5: dc.b $01,$05,$05,$02,$02,$06,$06,$06,$02,$02,$02,$02,$06,$06,$06,$02,$02,$05,$05,$01
level6: dc.b $01,$05,$05,$02,$02,$07,$07,$07,$07,$02,$02,$07,$07,$07,$07,$02,$02,$05,$05,$01
level7: dc.b $01,$05,$04,$08,$09,$0a,$0b,$0c,$02,$02,$02,$02,$0c,$0b,$0a,$09,$08,$04,$05,$01
;				   0   1     2     3    4    5     6    7     8    9     0    1     2    3     4    5     6    7     8    9     0     1     for sanity XD
prow0: dc.b $0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e
prow1: dc.b $0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f
prow2: dc.b $0f,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0f
prow3: dc.b $0f,$0e,$0e,$0f,$0f,$0f,$0e,$0e,$0e,$0f,$0f,$0f,$0e,$0e,$0e,$0f,$0f,$0f,$0e,$0e,$0e,$0f
prow4: dc.b $0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0e,$0e,$0e,$0e,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f
prow5: dc.b $0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0e,$0e,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f
prow6: dc.b $0f,$0e,$0e,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0e,$0e,$0f
prow7: dc.b $0f,$0e,$0e,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0e,$0e,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0e,$0e,$0f
prow8: dc.b $0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0e,$0e,$0e,$0e,$0e,$0e,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f
prow9: dc.b $0f,$0f,$0f,$0f,$0f,$0f,$0f,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0f,$0f,$0f,$0f,$0f,$0f,$0f
prowa: dc.b $0f,$0f,$0f,$0f,$0f,$0f,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0f,$0f,$0f,$0f,$0f,$0f
prowb: dc.b $0f,$0f,$0f,$0f,$0f,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0f,$0f,$0f,$0f,$0f
prowc: dc.b $0f,$0f,$0f,$0f,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0e,$0f,$0f,$0f,$0f
prow_addr: dc.b 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

row:						dc.b 0
col:						dc.b 0
invinc_time:		dc.b 0
; init_lives:				dc.b #$08
lives:					dc.b 0
current_key:			dc.b 0
prev_direction:			dc.b 0
inventory:				dc.b 0
score_ones:					dc.b 0
score_tens:					dc.b 0
char_colour:	dc.b 0
; score_init:				dc.b #$30	;#48 ; 0
pi_weapon:			dc.b #94
prev_note:			dc.b 0
temp_colour:			dc.b #$08
timer_loop:			dc.b 0
cur_char_col:	dc.b 0
cur_wall_col:	dc.b 0
; row_begin:			dc.b #$00
; row_newLevel_begin: dc.b #$01
; col_newLevel_end:   dc.b #$14
; col_begin:				dc.b #$00
; col_end:				dc.b #$16
seed:					dc.b 0 ;store seed for rand number
current_room:		dc.b 0
rooms:          dc.b 0,0,0,0,0,0,0,0,0
room_addr: dc.b 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

enemyCount: dc.b 0
enemyLoopCount: dc.b 0
enemyX:				dc.b 0
enemyY:				dc.b 0
enemyColour:		dc.b 0

;mod vars
divisor					dc.b 0

;drawToPlayfield vars
drawChar:				dc.b 0		; used to tell the draw method what you want to draw
drawColour:			dc.b 0		; used to tell the draw method what colour to draw the object
drawY:       		 	dc.b 0		; used to store the Y coordinate for safe keeping to return to the calling method

; scratch memory, if you use these make sure you are ok with losing them if you call another method
Scratch:				dc.b 0
Scratch2:				dc.b 0

Ycoor: 		 dc.b #$00
yOffset:      dc.b 0
tempY:        dc.b 0
temp1:        dc.b 0
titleName:
titleNameEnd
titleAuthors:
titleAuthorsEnd
p1_sprite: dc.b $0b
theme:		dc.b #165, #180, #131, #158, #185, #145	;these are completely random, so please change if desired!
themeEnd

leftpose: dc.b #00,#60,#60,#00,#216,#24,#24,#00
rightpose: dc.b #00,#60,#60,$00,#27,#24,#24,#00
downpose: dc.b #00,#60,#60,#00,#88,#88,#88,#64
uppose:   dc.b #64,#92,#92,#64,#24,#24,#24,#00
space:    dc.b #00,#00,#00,#00,#00,#00,#00,#00
wall:     dc.b $aa,$55,$aa,$55,$aa,$55,$aa,$55
door:     dc.b $81,$42,$24,$18,$18,$24,$42,$81
portal:   dc.b #240,#240,#240,#240,#15,#15,#15,#15
sword:    dc.b #24,#24,#24,#24,#24,#00,#24,#24
heart:    dc.b $36,$7f,$7f,$7f,$3e,$1c,$08,$00
