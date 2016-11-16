;test for a more memory-efficient level design

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
 sta $900f
 jsr levLoadMain 	;load level 3 to screen
 ;lda temp_colour
 ;sta $900f
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
 lda temp_colour
 sta $900f		 		; store in screen and border register  
drawTitleAnimation:	;this is a loop because don't need to constantly redraw title names
 jsr $ffde 		;read clock
 cmp #$01		
 bne titleInput
 sec	;set carry bit
 dec temp_colour	;random colour, pretty much
 lda temp_colour
 sta $900f		 		; store in screen and border register  
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
 brk

;each level stored as 3 bytes-last two bits of 3rd byte ysed for door/portal in location (0-16)
levLoadMain:
 lda wall_colour
 sta graphics_colour
 ldx #$00
 stx lev_index
levLoadMainLoop:
 lda #$00
 sta lev_byte 
 sta byte_index
iterateBytes:
 lda #$00
 sta lev_bit_ind
 ldx lev_byte
 lda level1Top,x
 sta lev_bit
iterateBits:	;iterating bits seems to work
 lda byte_index
 cmp #$02
 bne iterateBitsNormal
 lda lev_bit_ind
 cmp #$06
 bpl doorOrPortal
iterateBitsNormal:
 lda lev_bit
 and #$01	;00000001 NOTE: bit pattern will be reversed on screen, so arrange bits accordingly
 ;cmp #$00
 bne iterateBitsWall
 jsr levDrawSpace
 jmp iterateBitsEnd
iterateBitsWall:
 jsr levDrawWall
 ;check if 0 or 1 (3rd byte only)
 ;draw wall or space
iterateBitsEnd:
 inc lev_index
 lsr lev_bit
 inc lev_bit_ind
 lda lev_bit_ind
 cmp #$08
 bne iterateBits
itBitsEnd:
 inc byte_index
 lda byte_index
 cmp #$03
 bne levelEndCheck	;don't reset byte index
 lda #$00
 sta byte_index	;reset byte index
levelEndCheck:
 inc lev_byte
 lda lev_byte
 cmp #level1End-level1Top
 bne iterateBytes
 rts

;this is where doors are being drawn
;limitations: only draws from 0-3 spaces from current location
doorOrPortal:
 lda lev_bit	;this will have pattern 0000000x
 cmp #$00		;bitmask
 beq itBitsEnd
 lda lev_index
 sec
 sbc lev_bit	;subtracts so that does get drawn over
 tax
 jsr levDrawDoor
 ;rts
 jmp itBitsEnd
 
;code from 159-248 is total cowboy code.  Please make it pretty. <3
levToporBot:
 ldy lev_byte
 cpy #$22 ;#$22;#$0b	;check if top or bottom of row ;#$23 (35)
 beq checkLorR
 bpl levBot
 bmi levTop	;top row
 rts
 
checkLorR:		;this check is for crossover from top to bottom
 ldy lev_bit_ind
 cpy #$06		;Need to check if on 6th or 7th bit (0-7) 
 bpl levBot		;is on bottom
 bmi levTop
 rts

levBot:
 ldy #$00
 rts
 
levTop:
 ldy #$01
 rts
 
drawLevTop:
 ;ldx lev_index
 jsr drawToScreen
 rts
 
drawLevBottom:
 ;ldx lev_index
 jsr drawToScreenBot
 rts
 
levDrawDoor:
 lda door_sprite
 jsr levDrawSprite
 rts
 
levDrawWall:
 ldx lev_index
 lda wall_sprite
 jsr levDrawSprite
 ;check y reg to see if 1 or 0 (1 = top, 0 = bot)
 ;load wall into acc
 ;load wall colour into acc
 ;draw to screen
 rts
 
levDrawSpace:
 ldx lev_index
 lda space_sprite
 jsr levDrawSprite
 rts

levDrawSprite:
 jsr levToporBot
 cpy #$01
 beq drawLevTop
 bne drawLevBottom
 
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

timerLoop:		 ; super simple loop to slow down movement of 'B' (not have it fly across screen)
 ldy #$ff		 ; 255 (basically the biggest number possible)
timer:
 dey			 ; y-1
 cpy #$00		 ; check if y=0
 bne timer		 ; if not, loop
 sbc #$01		 ; acc - 1
 bpl timerLoop   ; branch if positive (N not set)
 rts			 ; N set, return

;levels: stored in 8 bytes:
;rows stored as 7 bits, 0xxxxxxx (can use the msb for other data) because 22/7 = 3 blocks per bit
;7 bytes for floor (same principle as above for columns)
;use 8th byte for data such as items in room, door direction, etc
;level11Top: dc.b $41, $41, $7f, $41, $41, $7f, $41, $01
;level11Top: dc.b $7f, $41, $41, $41, $41, $41, $41, $41, $41, $41, $41, $41, $41, $41, $41, $41, $41, $41, $41, $41, $7f, $01
;this isn't 8 bytes, but it turned out funny otherwise.  We can put it down to less bytes and be clever with algorithms, but
;this still cuts down on a lot of the memory costs

;Store each row as 3 bytes
;first three bytes store row layout: bits 0-1 of 3rd byte indicate whether
;there is a door (0 bit) or whether there is a portal (1 bit) -> value will be
;between 1-15 (not considering 0, 0 is considered no portal or door)
level1Top: 	dc.b $00, $00, $00
			dc.b $ff, $ff, $bf, $01, $00, $20
			dc.b $01, $00, $20, $01, $00, $20
			dc.b $01, $00, $20, $01, $00, $60
			dc.b $01, $00, $20, $01, $00, $20
			dc.b $01, $00, $20, $01, $00, $60
			dc.b $01, $00, $20, $01, $00, $20
			dc.b $01, $00, $20, $01, $00, $60
			dc.b $01, $00, $20, $01, $00, $20
			dc.b $01, $00, $20, $01, $00, $20
			dc.b $01, $00, $20, $ff, $ff, $ff
			dc.b $00, $00, $00, $00, $00, $00
level1End

;levelTop: dc.b 0
;levelEnd: dc.b 0 
row: dc.b 0
col: dc.b 0
lev_byte:	dc.b 0
byte_index: dc.b 0
lev_bit: dc.b 0
lev_row: dc.b 0
lev_index: dc.b 0
lev_bit_ind: dc.b 0
index: dc.b 0
col_index: dc.b 0

current_key:			dc.b 0
current_level_part:		dc.b 0

temp_colour:			dc.b 0
char_colour:			dc.b #$55
wall_colour:			dc.b #$44
life_colour:			dc.b 2
screen_colour:		dc.b #$0f
graphics_colour: 	dc.b 0
coll_char_colour:	dc.b 0

f1:						dc.b #39
f3:				dc.b #47
f5:							dc.b #55
p1_sprite:				dc.b #81		;81 = circle
lives_sprite:			dc.b #83		;heart
enemy_sprite:		dc.b #87		;circle
wall_sprite:			dc.b $66		;weird checkered square thingy
portal_sprite:			dc.b $7F 		;#209
space_sprite:			dc.b #32    ;$20
door_sprite: dc.b $5b
titleName:		dc.b #02, #15, #15, #16	;boop.  TODO: Better title
titleNameEnd
titleAuthors	dc.b #03,#04, #32, #12, #13, #32, #11, #13	;cd lm km
titleAuthorsEnd
gameOverMessage: dc.b #07, #01, #13, #05, #32, #15, #22, #05, #18
gameOverMessageEnd 
level_char: dc.b 0
;in future: full names, etc.
