	processor 6502

	include "vcs.h"
	include "macro.h"

;;;;  begin Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; DIGIT_L - draw a digit on the left half of the screen
;;;; usage: DIGIT_L PF#
;;;; - x is the vertical digit line
	MAC DIGIT_L
.Register	SET {1}
.DigitTable	SET {2}
.DigitLoop	
	sta WSYNC	; 3|0 wait for scanline at beginning so end-of
			;     loop logic is pre-scanline
;;;;;; start frame
;;; left playfield digit (18)
	lda digitLine		; 3|
	clc			; 2|
	adc DigitOffsetL0,X	; 4|
	tay			; 2|
	lda .DigitTable,Y	; 4|
	sta .Register		; 3|
; 18
;;; draw P0 (23)
	ldy scanLine	; 3|
	sec		; 2| 5
	tya		; 2| 7
	sbc P0y		; 3| 10
	adc #P0HEIGHT	; 2| 12
	bcs .DrawP0a	; 2/3| 14/15
	nop		; 2|
	nop		; 2|
	sec		; 2|
	bcs .NoDrawP0a	; 3| 23
.DrawP0a
	lda (P0spritePtr),Y	; 5| 20
	sta GRP0	; 3| 23
.NoDrawP0a

; 41
;;; clear PF Register for right digit (5)
	lda #0		; 2|
	nop		; 2|
	sta .Register	; 3| 48
	nop
	sta GRP1	; 3| 51 trigger P0 draw (and delay so PF2 isn't cleared too early)

; 51
;;; digit cleanup (15)
.DigitCleanA
	dec scanLine		; 5| scanLine--
	dec digitLine		; 5|5
	beq .NewDigitA		; 2/3| 7/8
	jmp .DigitLoop		; 3| 15 (used 66 of ~72)
.NewDigitA:
	inx			; 2| 10 digit row ++
	lda #16			; 2| 12
	sta digitLine		; 3| 15 digit line reset
	ENDM			

;;;; DIGIT_R - draw a digit on the right half of the screen
;;;; usage: DIGIT_R PF# DigitTable(LeftRight)(Rev)
;;;; - x is the vertical digit line
	MAC DIGIT_R
.Register	SET {1}
.DigitTable	SET {2}
.DigitLoop	
	sta WSYNC 	; 3| 0
;;; clear PF Register for left digit (6)
	lda #0		; 3|
	sta .Register	; 3|

; 6
;;; draw P0 (23)
	ldy scanLine	; 3|
	sec		; 2| set carry
	tya		; 2|
	sbc P0y		; 3|
	adc #P0HEIGHT	; 2|
	bcs .DrawP0b	; 2/3| 14/15
	nop		; 2|
	nop		; 2|
	sec		; 2|
	bcs .NoDrawP0b	; 3| 23
.DrawP0b
	lda (P0spritePtr),Y	; 5| 20
	sta GRP0	; 3| 23
.NoDrawP0b

	nop
; 29
;;; right playfield digit (18)
	lda digitLine		; 3|
	clc			; 2|
	adc DigitOffsetR0,X	; 4|
	tay			; 2|
	lda .DigitTable,Y	; 4|
	sta .Register		; 3|

; 47
;;; strobe GRP1 to trigger P0
	lda #0			; 2| 
	sta GRP1		; 3| 52

;;; digit cleanup (15)
.DigitClean
	dec scanLine		; 5| 
	dec digitLine		; 5|
	beq .NewDigit		; 2/3| 7/8
	jmp .DigitLoop		; 3| 10 (used 65 of ~72)
.NewDigit:
	inx			; 2| 10 digit row ++
	lda #16			; 2| 12
	sta digitLine		; 3| 15 digit line reset
	ENDM


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  end Macros

;;;; start constant declarations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
P0HEIGHT equ 9
PLAYAREALINES equ 176
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; end constant declarations

;;; $80 to $FF for variables, minus some at end if using stack
	seg.u variables
	org $80
;;;;  start variable declarations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
P0x	byte	; (80) P0 x
P0y	byte	; (81) P0 y
CTRLPF_shadow	byte	; (82) track content of CTRLPF
digitLine	byte	; (83) track the line of digit being drawn
frameOdd	byte	; (84) odd or even frame
PFcolor		byte	; (85) track color of Playfield
HoleIndex	byte	; (86) index for current hole
scanLine	byte	; (87) track current scan line
Scroll0		byte	; (88) digit 0 of scroll
Scroll1		byte	; (89) digit 1 of scroll
Scroll2		byte	; (8a) digit 2 of scroll
Scroll3		byte	; (8b) digit 3 of scroll
Scroll4		byte	; (8c) digit 4 of scroll
Score		byte	; (8d) Actual Score
KeyIndex	byte	; (8e) Index into digit key

	org $90
DigitOffsetL0	byte	;  digit array offset for left digit 0
DigitOffsetL1	byte	;  digit array offset for left digit 1
DigitOffsetL2	byte	;  digit array offset for left digit 2
DigitOffsetL3	byte	;  digit array offset for left digit 3
DigitOffsetL4	byte	;  digit array offset for left digit 4
DigitOffsetL5	byte	;  digit array offset for left digit 5
DigitOffsetL6	byte	;  digit array offset for left digit 6
DigitOffsetL7	byte	;  digit array offset for left digit 7
DigitOffsetL8	byte	;  digit array offset for left digit 8
DigitOffsetL9	byte	;  digit array offset for left digit 9
DigitOffsetL10	byte	;  digit array offset for left digit 10 
notePtrC0	byte	;  note pointer for Channel 0

	org $a0
DigitOffsetR0	byte	;  digit array offset for left digit 0
DigitOffsetR1	byte	;  digit array offset for left digit 1
DigitOffsetR2	byte	;  digit array offset for left digit 2
DigitOffsetR3	byte	;  digit array offset for left digit 3
DigitOffsetR4	byte	;  digit array offset for left digit 4
DigitOffsetR5	byte	;  digit array offset for left digit 5
DigitOffsetR6	byte	;  digit array offset for left digit 6
DigitOffsetR7	byte	;  digit array offset for left digit 7
DigitOffsetR8	byte	;  digit array offset for left digit 8
DigitOffsetR9	byte	;  digit array offset for left digit 9
DigitOffsetR10	byte	;  digit array offset for left digit 10 
notePtrC1	byte	;  note pointer for Channel 1

; Pointer block
	org $b0
P0spritePtr	word	; (b0/b1) y-adjusted sprite pointer

; Scroll Pointers
ScrollPtr0	word	; (b4/b5)
ScrollPtr1	word	; (b6/b7)
ScrollPtr2	word	; (b8/b9)
ScrollPtr3	word	; (ba/bb)
ScrollPtr4	word	; (bc/bd)
ScrollPtr5	word	; (be/bf)
ScrollPtr6	word	; (c0/c1)
ScrollPtr7	word	; (c2/c3)
ScorePtr0	word	; (c4/c5)
ScorePtr1	word	; (c6/c7)
KeyTablePtr	word	; (c8/c9)

Scroll5		byte	; (ca) digit 0 of score
Scroll6		byte	; (cb) digit 1 of score
Scroll7		byte	; (cd) digit 2 of score
Scroll8		byte	; (ce) digit 3 of score
Scroll9		byte	; (cf) digit 4 of score

durC0		byte	; (d0) channel 0 duration
durC1		byte	; (d1) channel 2 duration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  end variables

;;; Begin code segment in ROM at $F000
	seg code
	org $F000

Start:
	CLEAN_START

;;;;  start variable initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda #79
	sta P0x
	lda #106
	sta P0y
	sta COLUBK

	lda #0
	sta digitLine
	sta Score
	sta notePtrC0
	sta notePtrC1

	lda #%00000000
	sta frameOdd

	lda #55
	sta PFcolor

	lda #10
	sta HoleIndex		; initial hole left bottom

;;; Initialize digits
	lda #$a0		; blank
	sta DigitOffsetL10	; bottom left
	lda #$c0		; star
	sta DigitOffsetR0	; top right
	lda #$00		; 0
	sta DigitOffsetL0
	sta DigitOffsetR10
	lda #$10		; 1
	sta DigitOffsetL1
	sta DigitOffsetR9
	lda #$20		; 2
	sta DigitOffsetL2
	sta DigitOffsetR8
	lda #$30		; 3
	sta DigitOffsetL3	
	sta DigitOffsetR7
	lda #$40
	sta DigitOffsetL4
	sta DigitOffsetR6
	lda #$50
	sta DigitOffsetL5
	sta DigitOffsetR5
	lda #$60
	sta DigitOffsetL6
	sta DigitOffsetR4
	lda #$70
	sta DigitOffsetL7
	sta DigitOffsetR3
	lda #$80
	sta DigitOffsetL8
	sta DigitOffsetR2
	lda #$90
	sta DigitOffsetL9
	sta DigitOffsetR1
	lda #$a0
	
;; initialize scroll digits
	lda #$a		; blank
	sta Scroll0
	sta Scroll1
	sta Scroll2
	sta Scroll3
	lda #3		; start with pi 3.
	sta Scroll4
	lda #$b		; decimal point
	sta Scroll5

;; initalize score digits
	lda #0	; score 10s
	sta Scroll8
	sta Scroll9

;; set up Scroll Pointers high bytes
	lda #>digitTableLeftRev
	sta ScrollPtr0+1
	sta ScrollPtr5+1
	lda #>digitTableLeft
	sta ScrollPtr1+1
	sta ScrollPtr6+1
	lda #>digitTableRight
	sta ScrollPtr2+1
	sta ScrollPtr7+1
	lda #>digitTableRightRev
	sta ScrollPtr3+1
	sta ScorePtr0+1
	lda #>digitTableLeftRev
	sta ScrollPtr4+1
	sta ScorePtr1+1

;;; Set high byte of P0spritePtr (low byte updated per frame)
	lda #>P0bitmap
	sta P0spritePtr+1

;;; Set key table to Pi digits initially
	lda #>piTable
	sta KeyTablePtr+1

;;; Start playing soundtrack on C0
	lda #AUD_SOUNDTRACK
	jsr PlaySoundC0

;;; Initialize CTRLPF
	; D0 = REF (reflect playfield)
	; D1 - SCORE (color left/right of playfield like P0/P1)
	; D2 - PFP (1 playfield over players)
	; D4/D5 - Ball Size 00 = 1 / 01 = 2 / 10 = 4 / 11 = 8
	lda #000000000	; don't reflect playfield
	sta CTRLPF
;;; Enable "vertical delay" for P0
	lda #1		; only bit 0 matters
	sta VDELP0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  end variable initialization

StartFrame:
	lda #2
	sta VSYNC

;;; 3 lines of VSYNC
	sta WSYNC	; store halts until scanline complete
	sta WSYNC	; 2nd
	sta WSYNC	; 3rd

;;;; set timer for VBLANK
	LDA #44
	STA	TIM64T

	lda #0
	sta VSYNC	; turn off VSYNC

;;;;  start game vblank logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; handle input
CheckP0Up:
	lda #%00010000
	bit SWCHA
	bne CheckP0Down
	inc P0y
CheckP0Down:
	lda #%00100000
	bit SWCHA
	bne CheckP0Right
	dec P0y
CheckP0Right:
	lda #%10000000
	bit SWCHA
	bne CheckP0Left
	inc P0x
CheckP0Left:
	lda #%01000000
	bit SWCHA
	bne NoInput
	dec P0x
NoInput:

;;; P0x bounds checking
	lda P0x
	cmp #4
	beq P0xLow
	cmp #156	; 160 pixels minus 1/2 of 4-wide sprite
	beq P0xHigh
	jmp P0xDone
P0xLow:
	lda #5
	sta P0x
	jmp P0xDone
P0xHigh:
	lda #155
	sta P0x
P0xDone:
;;; update horizontal position
	ldx #0
	lda P0x 
	jsr PosObject
	sta WSYNC
	sta HMOVE

;;; P0 y bounds checking
	lda P0y
	cmp #8
	beq P0yLow
	cmp #PLAYAREALINES
	beq P0yHigh
	jmp P0yDone
P0yLow:
	lda #9
	sta P0y
	jmp P0yDone
P0yHigh:
	lda #PLAYAREALINES-1
	sta P0y
P0yDone:
	
;;; P0 y pointer
	lda #<P0bitmap+P0HEIGHT
	sec
	sbc P0y
	sta P0spritePtr

;;; Check collisions
	lda #%10000000
	bit CXP0FB	; bit 7 = P0/PF
	beq .NoP0Collision
	lda PFcolor
	sta COLUPF
;;; check if it's overlapping 2 digits
	lda #%00001000		; don't count position if top of 8-line sprite
	bit P0y			; is in 2nd half of 16-line digit (collision could be on next digit)
	beq .NoP0Collision	; 
;;;; find out where the collision happened
	lda #PLAYAREALINES+P0HEIGHT/2	; 
	sec
	sbc P0y	; subtract y to get top-indexed P0y
	lsr	; divide by 16
	lsr
	lsr
	lsr
	tax	; put in X
;	lda #%01000000	; bit mask for bit 6 (64)
;	bit P0x	; means between 64 and 128 (right side)
	lda #80		; 80 is horizontal middle of 160
        cmp P0x		; compare with X
	bpl .LeftCollision
	txa		; right is +16 of left, so
	ora #%00010000	; add 16 (x starts 0-15)
	tax		; and store back in X
.LeftCollision
; handle digit capture logic (needs X from above)
	jsr DigitCapture
	jmp .DoneCollision
.NoP0Collision
	lda #$FF
	sta COLUPF
.DoneCollision
	sta CXCLR	; clear collisions

;;; toggle frameOdd
	lda frameOdd
	eor #%01000000
	sta frameOdd

	jsr LoadScrollPointers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  end game vblank logic


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  start kernel prep
	lda #0
	sta COLUBK	; set background to black for score line
	ldx #0		; row number starts at 0 every page
	lda #PLAYAREALINES
	sta scanLine	; counter
	lda #15
	sta digitLine	; digit line counter

;;;;  end kernel prep
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Wait for rest of VBLANK
.VblankWaitLoop
	lda INTIM 	; load timer interrupt
	bne	.VblankWaitLoop
	sta WSYNC 	; wait for next wsync
	sta VBLANK	; turn off VBlank. A is zero because of bne above

;;;; Playfield Register Update Cycles
;;;; PF0L - 54-22
;;;; PF1L - 65-28
;;;; PF2L -  0-38
;;;; PF0R - 28-49
;;;; PF1R - 39-54
;;;; PF2R - 49-65
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; kernel (192 visible scan lines)

	ldy #14
;;; display 12 rows of "score"
.ScrollLoop
	sta WSYNC		; 3| 0
	lda (ScrollPtr0),Y	; 5| 5 PF0 is single digit
	sta PF0			; 3| 8
	lda (ScrollPtr1),Y	; 4|12
	ora (ScrollPtr2),Y	; 5|17
	sta PF1			; 3|20
	lda (ScrollPtr3),Y	; 5|25
	ora (ScrollPtr4),Y	; 5|30
	sta PF2			; 3|33
	lda (ScrollPtr5),Y	; 5|38
	sta PF0			; 3|41
;	lda (ScrollPtr6),Y	; 5|46
;	ora (ScrollPtr7),Y	; 5|51
	lda #0
	sta PF1			; 3|54
	lda (ScorePtr0),Y	; 5|59
	ora (ScorePtr1),Y	; 5|64
	sta PF2			; 3|67
	dey			; 2|69
	bne .ScrollLoop		; 3|75/76

;;; one more black line before moving to play area
	sta WSYNC

;; set background color back
	lda #106
	sta COLUBK

;; initialize x
	ldx #0
;;; pick which loop
	bit frameOdd
	bvc .EvenLoop
	jmp OddLoop

.EvenLoop
	DIGIT_L PF1, digitTableRight	; 0
	DIGIT_L PF2, digitTableRightRev	; 1
	DIGIT_L PF2, digitTableLeftRev	; 2
	DIGIT_L PF0, digitTableLeftRev	; 3
	DIGIT_L PF1, digitTableLeft	; 4
	DIGIT_L PF1, digitTableRight	; 5
	DIGIT_L PF1, digitTableLeft	; 6
	DIGIT_L PF0, digitTableLeftRev	; 7
	DIGIT_L PF2, digitTableLeftRev	; 8
	DIGIT_L PF2, digitTableRightRev	; 9
	DIGIT_L PF1, digitTableRight	; 10
	jmp Overscan

;;;; start odd frame
OddLoop
	DIGIT_R PF1, digitTableRight	; 0
	DIGIT_R PF1, digitTableLeft	; 1
	DIGIT_R PF0, digitTableLeftRev	; 2
	DIGIT_R PF2, digitTableLeftRev	; 3
	DIGIT_R PF2, digitTableRightRev	; 4
	DIGIT_R PF1, digitTableRight	; 5
	DIGIT_R PF2, digitTableRightRev	; 6
	DIGIT_R PF2, digitTableLeftRev	; 7
	DIGIT_R PF0, digitTableLeftRev	; 8
	DIGIT_R PF1, digitTableLeft	; 9
	DIGIT_R PF1, digitTableRight	; 10
	jmp Overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; end kernel

Overscan
;;;; set timer for OVERSCAN
	lda #2
	sta WSYNC
	sta VBLANK
	lda #36
	sta TIM64T

;;;;  start game overscan logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; clear player and playfield graphics
	lda #0
	sta GRP0
	sta PF0
	sta PF1
	sta PF2
; check if any sounds need to be stopped
	jsr SoundCheck

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  end game overscan logic


;;;; Wait for rest of OVERSCAN
.OverscanWaitLoop:
	lda INTIM
	bne .OverscanWaitLoop
	lda #2
	sta WSYNC

;;; new frame
	jmp StartFrame

;;;;   start subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; PosObject from https://www.biglist.com/lists/stella/archives/200403/msg00260.html
;;; but that didn't work right so traced back to https://www.biglist.com/lists/stella/archives/200311/msg00039.html
; Positions an object horizontally
; Inputs: A = Desired position.
; X = Desired object to be positioned (0-5). *jh* (P0, P1, M0, M1, Ball)
; scanlines: If control comes on or before cycle 73 then 1 scanline is consumed.
; If control comes after cycle 73 then 2 scanlines are consumed.
; Outputs: X = unchanged
; A = Fine Adjustment value.
; Y = the "remainder" of the division by 15 minus an additional 15.
; control is returned on cycle 6 of the next scanline.
PosObject SUBROUTINE
	STA WSYNC ; 00 Sync to start of scanline.
	SEC ; 02 Set the carry flag so no borrow will be applied during the division.
.divideby15
	SBC #15 ; 04 ; Waste the necessary amount of time dividing X-pos by 15!
	BCS .divideby15 ; 06/07 - 11/16/21/26/31/36/41/46/51/56/61/66

	EOR #$0F
	ASL
	ASL
	ASL
	ASL

	ADC #$90
	STA RESP0,X
	STA WSYNC
	STA HMP0,X

	RTS

;;; end PosObject from https://www.biglist.com/lists/stella/archives/200311/msg00039.html

;;; Handle digit capture
;;; X = offset to digit in question from DigitOffsetL0
;   left = 0-11, right = 16-27
DigitCapture SUBROUTINE
; save current contents
	ldy DigitOffsetL0,X	; put current contents in Y
; blank the spot
	lda #$a0		; (blank digit)
	sta DigitOffsetL0,X	; store it in captured position
; fill previous hole
	txa			; 
	ldx HoleIndex		; 
	sta HoleIndex		; 
	tya
	sta DigitOffsetL0,X	; store it in current hole
; check if input was star
	cpy #$c0		; star character
        bne .CheckInput
	lda AUD_STAR
	jsr PlaySoundC1
	ldy #0
	lda (KeyTablePtr),Y
	tax
; decrement score (will be incremented later for net 0)
	sec
	sed
	lda Score
	sbc #1
	sta Score
	jmp .RippleDigits
; check input digit vs key
.CheckInput
	tya
	lsr
	lsr
	lsr
	lsr
	tax
	ldy #0
	cmp (KeyTablePtr),Y
	bne .IncorrectDigit
; play good sound
	lda #AUD_GOODNUMBER
	jsr PlaySoundC1
; ripple scroll digits
.RippleDigits
	lda Scroll1
	sta Scroll0
	lda Scroll2
	sta Scroll1
	lda Scroll3
	sta Scroll2
	lda Scroll4
	sta Scroll3
	lda Scroll5
	sta Scroll4
	txa
	sta Scroll5
; increment lsb of KeyTablePtr
	inc KeyTablePtr
; increment score
	clc
	sed
	lda Score
	adc #1
	sta Score
	cld
	jmp .IncorrectDone
.IncorrectDigit
; play collision sound
	lda #AUD_BADNUMBER
	jsr PlaySoundC1
; decrement score
	sec
	sed
	lda Score
	sbc #1
	bmi .ScoreZero	
	sta Score
.ScoreZero
	cld
.IncorrectDone
	RTS

;;; Load Scroll/Score Pointers based on corresponding values
LoadScrollPointers SUBROUTINE
	lda Scroll0	; load the digit
	asl		; 
	asl		; 
	asl		; 
	asl		; multiply by 16
	sta ScrollPtr0	; Put in LSB of ScrollPtr

	lda Scroll1	; load the digit
	asl		; 
	asl		; 
	asl		; 
	asl		; multiply by 16
	sta ScrollPtr1	; Put in LSB of ScrollPtr

	lda Scroll2	; load the digit
	asl		; 
	asl		; 
	asl		; 
	asl		; multiply by 16
	sta ScrollPtr2	; Put in LSB of ScrollPtr

	lda Scroll3	; load the digit
	asl		; 
	asl		; 
	asl		; 
	asl		; multiply by 16
	sta ScrollPtr3	; Put in LSB of ScrollPtr

	lda Scroll4	; load the digit
	asl		; 
	asl		; 
	asl		; 
	asl		; multiply by 16
	sta ScrollPtr4	; Put in LSB of ScrollPtr

	lda Scroll5	; load the digit
	asl		; 
	asl		; 
	asl		; 
	asl		; multiply by 16
	sta ScrollPtr5	; Put in LSB of ScrollPtr

	lda #%11110000 	; mask for first decimal digit
	and Score	;
	sta ScorePtr0	; Put in LSB of ScorePtr

	lda #%00001111	; load the digit
	and Score	; load 2nd decimal digit
	asl		; 
	asl		; 
	asl		; 
	asl		; multiply by 16
	sta ScorePtr1	; Put in LSB of ScorePtr

	rts

;;; Start playing a sound on C0
;;; A (0 - 255) = where in sound data to start playing
PlaySoundC0 SUBROUTINE
	sta notePtrC0
	lda #1		; set duration to 1 so 
	sta durC0	; note will change on next check
	rts

;;; Start playing a sound on C1
;;; A (0 - 255) = where in sound data to start playing
PlaySoundC1 SUBROUTINE
	sta notePtrC1
	lda #1
	sta durC1
	rts

;;; Check C0/C1 duration and see if tone is done
;;; - if duration is 0, no change
;;; - if duration decrements to 0, switch to next note
;;; - if next note duration = 0 stop
;;; - if next note duration = 255
;;;   - stop current note
;;;   - set note pointer to value of Frequency
SoundCheck SUBROUTINE
	lda durC0		; load duration
	beq .checkC1		; if 0 do nothing
	dec durC0		; decrement it
	bne .checkC1		; if not zero, no change
	inc notePtrC0		; zero: move pointer to next note
        ldx notePtrC0		; load note pointer to X
	lda AudioDuration,X	; load next Duration
	beq .stopC0		; if 0, stop this channel
	cmp #$FF		; check if 255
	bne .playC0		; if not, ready to load audio data
	lda AudioFrequency,X	; if so, read frequency to get next note pointer
	sta notePtrC0		; store in notePtrC0
	tax			; and in x
	lda AudioDuration,X	; load new Duration
.playC0
	sta durC0		; A is new C0 Duration
	lda AudioFrequency,X	; load freq
	sta AUDF0		; and store in register
	lda AudioControl,X	; load control
	sta AUDC0		; and store in register
	lda AudioVolume,X	; load volume
	sta AUDV0		; and store in register
	jmp .checkC1
.stopC0
	lda #0
	sta durC0
	sta AUDV0
.checkC1
	lda durC1		; load duration
	beq .endSub		; if 0 do nothing
	dec durC1		; decrement it
	bne .endSub		; if not zero, no change
	inc notePtrC1		; zero: move pointer to next note
        ldx notePtrC1		; load note pointer to X
	lda AudioDuration,X	; load next Duration
	beq .stopC1		; if 0, stop this channel
	cmp #$FF		; check if 255
	bne .playC1		; if not, ready to load audio data
	lda AudioFrequency,X	; if so, read frequency to get next note pointer
	sta notePtrC1		; store in notePtrC1
	tax			; and in x
	lda AudioDuration,X	; load new Duration
.playC1
	sta durC1		; A is new C1 Duration
	lda AudioFrequency,X	; load freq
	sta AUDF1		; and store in register
	lda AudioControl,X	; load control
	sta AUDC1		; and store in register
	lda AudioVolume,X	; load volume
	sta AUDV1		; and store in register
	jmp .endSub
.stopC1
	lda #0
	sta durC1
	sta AUDV1
.endSub
	rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   end subroutines

;;;;  start ROM lookup tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; digits.h should set digitTable at the beginning followed by
;;;          an array of 16 bytes for each digit 0-9
	include "digitTableRight.h"
	include "digitTableLeft.h"
	include "digitTableRightRev.h"
	include "digitTableLeftRev.h"
	include "numberTables.h"
	include "audioData.h"

	org $fef6
P0bitmap:
;;; pi bitmap
	byte #%00000000
	byte #%01000010
	byte #%00100101
	byte #%00100100
	byte #%00100100
	byte #%00100100
	byte #%00100100
	byte #%10100100
	byte #%01111111
;;; sqrt(2) bitmap
/*
	byte #%00000000
	byte #%11011111
	byte #%01010000
	byte #%01011000
	byte #%01000110
	byte #%01010010
	byte #%01001100
	byte #%01000000
	byte #%01111111
*/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  end ROM lookup tables

;;; Complete to 4kB
	org $FFFC
	.word Start
	.word Start

