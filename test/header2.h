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
space_sprite = #$0e
wall_sprite = #$0f
portal_sprite = #$11
p1_sprite = #$0b
lives_sprite = #$13		;heart
door_sprite = #$10
enemy_sprite = #$0c		;circle

sword_sprite = #$12
w = #9
a = #17
s = #41
d = #18
f5 = #55
f1 = #39
f3 = #47
atk = #32	;space
init_char_col = #$01	;weakest sword
wall_colour = #$44
life_colour = 2
screen_colour = #$0f
row_end = #$15
;score_init = #$00
init_lives = #$08
row_begin = #$00
row_newLevel_begin = #$01
col_newLevel_end = #$14
col_begin = #$00
col_end = #$16
