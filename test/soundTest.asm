;This test tests basic sound using W,A,S,D keys for user input to choose sound.
  ; vic pages used (page 98-102, 175)
  ;Oct 9, 2016
  processor 6502
  org $1001               ; Unexpanded VIC

 ; BASIC stub (unexpanded vic)
 dc.w $100b              ; Pointer to next BASIC line
 dc.w 1981               ; BASIC Line#
 dc.b $9e                ; BASIC SYS token
 dc.b $34,$31,$30,$39    ; 4109 (ML start)
 dc.b 0                  ; End of BASIC line
 dc.w 0                  ; End of BASIC program

 jsr $ffbd	;initialize clock
 jsr $e55f       ; clear screen
 ldx #$01		 ; load 1 into x reg
 lda #$0f		 ; 15
 sta $900e		; set sound bits/turn on volume (see)

 ;note: $900a - low freq
 ; $900b - med freq
 ; $900c - high freq
 
 ;timer: acc = fast x = med y = slow 
top:
  ;ldy #$0f		 ; duration of sound (15 loops)
 lda $00c5		 ; current key held down -> page 179 of vic20 manual
 cmp #17		 ;a pressed
 bne chktim
 cmp #48
 beq next
 jsr SOUNDON
chktim:
 jsr $ffde ;call timer
 sty $1e00	;just time-testing purposes to see what interval
 sty $9600
 stx $1e01
 stx $9601
 sta $1e02
 sta $9602
 and #$0f
 cmp #$0f

 bne top
 jsr SOUNDOFF
 jmp top
 
next:
  jsr SOUNDOFF
  rts			 ; quit

SOUNDON:
 lda #$af
 sta $900c
 rts
SOUNDOFF:
 lda #0
 sta $900c
 rts