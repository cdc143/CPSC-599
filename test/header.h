char_colour_loc_bot equ $96f2
char_colour_loc_top equ $9616
graphics_top equ $1e16
graphics_bot equ $1ef2
graphics_playfield_start equ $1e17
color_playfield_start equ $9617
status_colour_top equ $9600
status_colour_bot equ $97ce
status_loc_top equ $1e00
status_loc_bot equ $1fce
space_sprite = #32
wall_sprite = $66
portal_sprite = $7f
p1_sprite = #81 ;81 = circle
lives_sprite = #83		;heart
enemy_sprite = #87		;circle
door_sprite = $5b
sword_sprite = $58
w = #9
a = #17
s = #41
d = #18
f5 = #55
f1 = #39
f3 = #47
atk = #48	;q
init_char_col = #$01	;weakest sword
wall_colour = #$44
life_colour = 2
screen_colour = #$0f
row_end = #$15
;score_init = #$30
init_lives = #$08
row_begin = #$00
row_newLevel_begin = #$01
col_newLevel_end = #$14
col_begin = #$00
col_end = #$16
