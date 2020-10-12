	processor 6502

	include "vcs.h"
	include "macro.h"

;;;; start constant declarations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
P0HEIGHT equ 9
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
PFcolor	byte	; (85) track color of Playfield
leftDigitOffset0	byte	; (86) digit array offset for left digit 0
leftDigitOffset1	byte	; (87) digit array offset for left digit 1
leftDigitOffset2	byte	; (88) digit array offset for left digit 2
leftDigitOffset3	byte	; (89) digit array offset for left digit 3
leftDigitOffset4	byte	; (8a) digit array offset for left digit 4
leftDigitOffset5	byte	; (8b) digit array offset for left digit 5
leftDigitOffset6	byte	; (8c) digit array offset for left digit 6
leftDigitOffset7	byte	; (8d) digit array offset for left digit 7
leftDigitOffset8	byte	; (8e) digit array offset for left digit 8
leftDigitOffset9	byte	; (8f) digit array offset for left digit 9
leftDigitOffset10	byte	; (90) digit array offset for left digit 10 
leftDigitOffset11	byte	; (91) digit array offset for left digit 11 
P0spritePtr	word	; (92) y-adjusted sprite pointer
digitTablePointer	word	; (94) pointer to digit table
scanLine	byte	; track current scan line
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

	lda #55
	sta PFcolor

;;; Initially fill offsets with consecutive digits
	lda #$a0
	sta leftDigitOffset0
	lda #$00
	sta leftDigitOffset1
	lda #$10
	sta leftDigitOffset2
	lda #$20
	sta leftDigitOffset3
	lda #$30
	sta leftDigitOffset4
	lda #$40
	sta leftDigitOffset5
	lda #$50
	sta leftDigitOffset6
	lda #$60
	sta leftDigitOffset7
	lda #$70
	sta leftDigitOffset8
	lda #$80
	sta leftDigitOffset9
	lda #$90
	sta leftDigitOffset10
	lda #$a0
	sta leftDigitOffset11
	
;;; Set digitTablePointer
	lda #<digitTable
	sta digitTablePointer
	lda #>digitTable
	sta digitTablePointer+1

;;; Set high byte of P0spritePtr (low byte updated per frame)
	lda #>P0bitmap
	sta P0spritePtr+1

;;; Initialize CTRLPF
	; D0 = REF (reflect playfield)
	; D1 - SCORE (color left/right of playfield like P0/P1)
	; D2 - PFP (1 playfield over players)
	; D4/D5 - Ball Size 00 = 1 / 01 = 2 / 10 = 4 / 11 = 8
	lda #000000000	; don't reflect playfield
	sta CTRLPF
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
	cmp #192
	beq P0yHigh
	jmp P0yDone
P0yLow:
	lda #9
	sta P0y
	jmp P0yDone
P0yHigh:
	lda #191
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
	beq NoP0Collision
;	inc PFcolor	; add one to PFcolor
	lda PFcolor
	sta COLUPF
	jmp DoneCollision
NoP0Collision
	lda #$FF
	sta COLUPF
DoneCollision
	sta CXCLR	; clear collisions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  end game vblank logic


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  start kernel prep
	lda #0
	ldx #$0		; row number starts at 0 every page
	lda #192	; 
	sta scanLine	; counter
;;;;  end kernel prep
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Wait for rest of VBLANK
.VblankWaitLoop
	lda INTIM 	; load timer interrupt
	bne	.VblankWaitLoop
	sta WSYNC 	; wait for next wsync
	sta VBLANK	; turn off VBlank. A is zero because of bne above

;;;; kernel (192 visible scan lines)
;;;; Playfield Register Update Cycles
;;;; PF0L - 54-22
;;;; PF1L - 65-28
;;;; PF2L -  0-38
;;;; PF0R - 28-49
;;;; PF1R - 39-54
;;;; PF2R - 49-65
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
.LoopVisible:
	sta WSYNC	; 3|0 wait for scanline at beginning so end-of
			;     loop logic is pre-scanline
;;; left playfield digit (18)
	lda digitLine		; 3|
	clc			; 2|
	adc leftDigitOffset0,X	; 4|
	tay			; 2|
	lda digitTable,Y	; 4|
	sta PF1			; 3|

;;; draw P0 (24)
	ldy scanLine	; 3|
	sec		; 2| set carry
	tya		; 2|
	sbc P0y		; 3|
	adc P0HEIGHT	; 3|
	bcs .DrawP0	; 2/3|
	nop		; 2|
	nop		; 2|
	sec		; 2|
	bcs .NoDrawP0	; 3|
.DrawP0
	lda (P0spritePtr),Y	; 5|
	sta GRP0	; 3|
.NoDrawP0

;;; right playfield

;;; digit cleanup (19)
	inc digitLine	; 5|
	lda #%00010000	; 2|
	and digitLine	; 3|
	beq stillInDigit; 2/3|
	inx		; 2| digit row ++
	lda #0		; 2| 
	sta digitLine	; 3| digit line reset
stillInDigit:

;;; end loop (cycles <= 67 here to avoid wrap)
	dec scanLine		; 5| scanLine--
	bne .LoopVisible	; 2/3/4| go back until x = 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; end kernel

;;;; set timer for OVERSCAN
	lda #2
	sta WSYNC
	sta VBLANK
	lda #36
	sta TIM64T

;;;;  start game overscan logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	lda #0
	sta GRP0

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
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   end subroutines

;;;;  start ROM lookup tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; digits.h should set digitTable at the beginning followed by
;;;          an array of 16 bytes for each digit 0-9
	include "digits.h"

	org $fef6
P0bitmap:
	byte #%00000000
	byte #%11000010
	byte #%00100101
	byte #%00100100
	byte #%00100100
	byte #%00100100
	byte #%00100100
	byte #%00100100
	byte #%11111111

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  end ROM lookup tables

;;; Complete to 4kB
	org $FFFC
	.word Start
	.word Start
