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
 lda #char_colour
 sta drawColour

;Super crappy title page.  TODO: Fix and make legible.
;I blasted away the names, but will put back later.
titleScreen:
 jsr $e55f		;clear screen
 lda #char_colour		;temporary colour start
 sta drawColour
 sta $900f
 ;jsr levLoadMain 	;load level 3 to screen
 ;lda temp_colour
 ;sta $900f
 lda #$09 
 sta Scratch2
 lda #$05			;screen coord for title; too lazy to use x,y coords
 sta col
 lda #titleNameEnd-titleName-1	;length
 sta row
drawTitleLoop:		;draw title to screen
 ldx row
 lda titleName,x
 jsr incIndex
 ldy col
 jsr drawToPlayfield
 dec row		;index -1
 lda row
 cmp #$00
 bpl drawTitleLoop
 lda #$07
 sta Scratch2
 lda #$06		;screen coord for author names
 sta col
 lda #titleAuthorsEnd-titleAuthors-1	
 sta row
drawAuthorLoop:
 ldx row
 ldy col
 lda titleAuthors,x
 jsr incIndex
 jsr drawToPlayfield
 dec row
 lda row
 cmp #$00
 bpl drawAuthorLoop
 lda temp_colour
 sta $900f		 		; store in screen and border register  
drawTitleAnimation:	;this is a loop because don't need to constantly redraw title names
 jsr $ffde 		;read clock
 cmp #$01		
 bne titleInput
 sec	;set carry bit
 dec drawColour	;random colour, pretty much
 lda drawColour
 sta $900f		 		; store in screen and border register  
 
titleInput:
 lda $00c5			;check for start.  Only can press start right now. Probably should 
 sta current_key
 cmp #f3
 beq gameLoopTop
 cmp #f1
 beq titleScreen
 cmp #f5
 bne drawTitleAnimation
 jsr $e55f
 brk

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
 stx Scratch
 sty Scratch2
 jsr $ffde 	;call timer
 txa
 adc $9114
 adc $9004 
 sta seed
 ldx Scratch
 ldy Scratch2
 rts

genRoom:
 ldy #$00
genRoomLoop:
 jsr getRandom
 and #$03
 sta rooms,y
 iny
 cpy #$09
 bne genRoomLoop
 lda #$07
 sta rooms
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

 ldx #$01
 stx Ycoor
 jsr putDoors
 jsr putDrops
 rts

putDoors:
  lda #wall_colour
  sta drawColour
  lda current_room
  sta $1000
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
  ldy #$0a ;Should be 9, glitch in drawing code
  ldx #$15
  lda #door_sprite
  jsr drawToPlayfield
  rts
putTopDoor:
  ldx #$0a
  ldy #$01
  lda #door_sprite
  jsr drawToPlayfield
  rts
putBottomDoor:
  ldx #$0a
  ldy #$14
  lda #door_sprite
  jsr drawToPlayfield
  rts
putLeftDoor:
  ldy #$0a ;Should be 9, glitch in drawing code
  ldx #$00
  lda #door_sprite
  jsr drawToPlayfield
  rts
putPortal:
  pha
  ldy #$0a
  ldx #$0b
  lda #$7f
  jsr drawToPlayfield
  pla
  rts
putDrops: ;Randomly decide if portal, potion or sword or none
dropLoop:
  jsr getRandom
  and #$0f
  tay
  jsr getRandom
  and #$0f
  tax
  ;jsr getFromScreen
  ;cmp #$20
  ;bne dropLoop
  jsr getRandom
  and #$0f
  cmp #$00
  bne drop1
  jsr putPortal
  rts
drop1:
  cmp #$01
  bne drop2
  jsr putSword
  rts
drop2:
  cmp #$02
  bne dropEnd
  jsr putPotion
dropEnd:
  rts

putSword: ;Assumes x coordinate and y coordinate to draw will be passed in
  pha
  lda #$58
  ldy #$03
  ldx #$02
  jsr drawToPlayfield
  pla
  rts

putPotion: ;Assumes x coordinate and y coordinate to draw will be passed in
  pha
  lda #$5a
  ldy #$03
  ldx #$01
  jsr drawToPlayfield
  pla
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
 lda #135
 jsr SOUNDONHIGH	;play "lose sound" 
 ;print game over to screen
gameOverEnd:	 ; bounce branch to get other subroutines to top of gameLoopTop
 lda $00c5		 ; current key held down -> page 179 of vic20 manual
 cmp #f5 ;quit
 beq quitBounce
 cmp #f1 ;f1 to restart
 bne gameOverEnd
 jsr SOUNDOFFHIGH	;turn sound off
 jmp titleScreen

initEnemyLocation:
  jsr getRandom
  tax
  lda graphics_playfield_start,x
  cmp #space_sprite
  bne initEnemyLocation
  lda #enemy_sprite
  sta graphics_playfield_start,x ;potential problem/not problem: enemies
							;can only spawn 255 spaces past graphics_playfield_start
  ;lda #char_colour
  jsr getRandom	;just gets a random colour for now, change this when have more memory
  and #$07
  ;adc #$0b
  ;and #$3c ;00111100
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
 lda #char_colour
 sta cur_char_col
 sta drawColour
 lda #p1_sprite		; 'B'
 ;jsr drawToScreen
 ldx row
 ldy col
 jsr drawToPlayfield
 pla			; pull acc from stack
 sta $9005		; store in char memory

top:			; top of loop
 jsr isInvincible	;check if player still "injured"
 jsr chktim
 lda #$00
 sta $900b			 ; store sound
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
 lda #p1_sprite
 jsr drawToPlayfield
 jsr drawTimer
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
 jsr drawToPlayfield	;draw to screen
 lda #241				;play sword sound
 jsr SOUNDONLOW
 rts
 
hitEnemy:				
 lda #175				;different sound if player hits an enemy
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
 lda #space_sprite
 cpx row_begin				; check if end of screen on left
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
 lda #space_sprite
 cpy col_begin
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
 lda #space_sprite
 cpy col_end
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
 lda #$04	;drop sprite
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
 lda #$01		;for no move function
 rts

;portal animation
portalColl:
 jmp portalAnimation
 
;crash into enemy: change player colour lose life if not invincible
enemyColl:
 ;lda #$02
; sta cur_char_col
 jsr checkInvincible
 lda #$01		;no move
 rts
 
;check if invincible 
checkInvincible:
 lda cur_char_col
 cmp #char_colour
 beq notInvincible
 rts
 
notInvincible:
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
 cmp #char_colour
 beq stillInvincible 	;jump over
 lda invinc_time
 cmp #$00
 bne stillInvincible
 lda #char_colour
 sta cur_char_col
stillInvincible:
 dec invinc_time
 cmp #$03
 bpl invinRet
 lda #$01
 sta cur_char_col
invinRet:
 rts
 
;TODO: DOOR COLLISION
doorColl:
 ;INSERT CODE HERE
 ;
 ;
 ;
 ;
 lda #$00
 rts
 
dropColl:
 lda #$03	;indicate picked up a drop
 ;TODO: subroutine to decide what to do with drop
 ;
 ;
 ;
 ;
 lda #$00	;move over drop
 rts
 
loseLife:			;player dies
 lda #space_sprite
 ldx lives		;this
 ldy #$00
 jsr drawToStatus
 dec lives
 lda lives
 cmp #$00
 bpl loseLifeNext
 jmp gameOver
loseLifeNext:
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
  cmp row_newLevel_begin		;checking top
  bne checkright
  inc current_room
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
  rts

error: ;shouldn't happen
  brk
  rts

level0: dc.b $01,$04,$04,$04,$04,$04,$04,$04,$04,$02,$02,$02,$04,$04,$04,$04,$04,$04,$04,$01
level1: dc.b $01,$02,$02,$02,$03,$03,$03,$02,$02,$03,$03,$03,$02,$02,$03,$03,$03,$02,$02,$01
level2: dc.b $01,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$02,$04,$04,$04,$04,$04,$04,$04,$01
level3: dc.b $01,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$02,$01
level4: dc.b $01,$05,$05,$05,$05,$05,$05,$05,$05,$02,$02,$02,$05,$05,$05,$05,$05,$05,$05,$01
level5: dc.b $01,$05,$05,$05,$05,$02,$06,$06,$06,$06,$06,$02,$06,$06,$06,$06,$02,$05,$05,$01
level6: dc.b $01,$05,$05,$05,$02,$07,$07,$07,$07,$07,$07,$02,$07,$07,$07,$07,$07,$07,$02,$01
level7: dc.b $01,$05,$04,$08,$09,$0a,$0b,$0c,$02,$02,$02,$02,$0c,$0b,$0a,$09,$08,$04,$05,$01

prow0: dc.b $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
prow1: dc.b $66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66
prow2: dc.b $66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66
prow3: dc.b $66,$20,$20,$66,$66,$66,$20,$20,$20,$66,$66,$66,$20,$20,$20,$66,$66,$66,$20,$20,$20,$66
prow4: dc.b $66,$66,$66,$66,$66,$66,$66,$66,$66,$20,$20,$20,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66
prow5: dc.b $66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$20,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66
prow6: dc.b $66,$20,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$66,$20,$66
prow7: dc.b $66,$20,$66,$66,$66,$66,$66,$66,$66,$66,$20,$66,$66,$66,$66,$66,$66,$66,$66,$66,$20,$66
prow8: dc.b $66,$66,$66,$66,$66,$66,$66,$66,$20,$20,$20,$20,$20,$66,$66,$66,$66,$66,$66,$66,$66,$66
prow9: dc.b $66,$66,$66,$66,$66,$66,$66,$20,$20,$20,$20,$20,$20,$20,$66,$66,$66,$66,$66,$66,$66,$66
prowa: dc.b $66,$66,$66,$66,$66,$66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66,$66,$66,$66,$66,$66,$66
prowb: dc.b $66,$66,$66,$66,$66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66,$66,$66,$66,$66,$66
prowc: dc.b $66,$66,$66,$66,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$66,$66,$66,$66,$66
prow_addr: dc.b 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

row:						dc.b 0
col:						dc.b 0
invinc_time:		dc.b 0
init_lives:				dc.b #$08
lives:					dc.b 0
current_key:			dc.b 0
prev_direction:			dc.b 0
inventory:				dc.b 0
pi_weapon:			dc.b #94
temp_colour:			dc.b 0
timer_loop:			dc.b 0
cur_char_col:	dc.b 0
row_begin:			dc.b #$00
row_newLevel_begin: dc.b #$01
col_newLevel_end:   dc.b #$14

col_begin:				dc.b #$00
col_end:				dc.b #$16
seed:					dc.b 0 ;store seed for rand number
current_room:		dc.b 0
rooms:          dc.b 0,0,0,0,0,0,0,0,0
room_addr: dc.b 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

;drawToPlayfield vars
drawChar:				dc.b 0
drawColour:			dc.b 0
Scratch:				dc.b 0
Scratch2:				dc.b 0
Ycoor:        dc.b #$00
yOffset:      dc.b 0
tempY:        dc.b 0
titleName:		dc.b #02, #15, #15, #16	;boop.  TODO: Better title
titleNameEnd
titleAuthors	dc.b #03,#04, #32, #12, #13, #32, #11, #13	;cd lm km
titleAuthorsEnd
gameOverMessage: dc.b #07, #01, #13, #05, #32, #15, #22, #05, #18
gameOverMessageEnd 
