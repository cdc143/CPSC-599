  ;This test changes the text colour and border/background
  processor 6502
  org $1001               ; Unexpanded VIC

 ; BASIC stub (unexpanded vic)
 dc.w $100b              ; Pointer to next BASIC line
 dc.w 1981               ; BASIC Line#
 dc.b $9e                ; BASIC SYS token
 dc.b $34,$31,$30,$39    ; 4109 (ML start)
 dc.b 0                  ; End of BASIC line
 dc.w 0                  ; End of BASIC program

 jsr $e55f       ; clear screen
;changes the colour of text -> page 173 vic manual
 lda #$44		 ; load new colour to acc
 sta $0286		 ; change text colour
 
;changes the border and background colour -> page 175 of vic do manual
 lda #$0f		 ; this makes a green border and black background
 sta $900f		 ; store in screen and border register

;return
 rts
 
 ;text
 ;44 = purple, 55 = green; ff = blurry and black
 
 ;border/background -> 0f = green border, black background