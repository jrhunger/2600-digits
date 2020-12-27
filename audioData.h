;;; contants
;; initial channel sounds
AUD_INITC0 equ 0
;; - sound effects pointers (need to be 1 lower than the actual place due to program logic)
AUD_SOUNDTRACK equ 0
AUD_BADNUMBER equ 16
AUD_GOODNUMBER equ 18
AUD_STAR equ 22

;;; Actual data
	align 256
AudioDuration:
	.byte 0		; 0 DISABLE
	.byte 16	; 1 SOUNDTRACK
	.byte 8		; 2
	.byte 16	; 3
	.byte 16	; 4
	.byte 16	; 5
	.byte 8		; 6
	.byte 16	; 7
	.byte 16	; 8
	.byte 16	; 9
	.byte 8		; 10
	.byte 16	; 11
	.byte 16	; 12
	.byte 16	; 13
	.byte 16	; 14
	.byte 32 	; 15
	.byte 255	; 16 loop to (AudioFrequency:16)

	.byte 16	; 17 BADNUMBER
	.byte 0		; 18

	.byte 4 	; 19 GOODNUMBER
	.byte 6 	; 20 
	.byte 4 	; 21
	.byte 0		; 22

	.byte 16	; 23 STAR
	.byte 0  	; 24

AudioFrequency:
	.byte 0		; 0 DISABLE
	.byte 5		; 1 SOUNDTRACK
	.byte 6		; 2
	.byte 7		; 3
	.byte 8 	; 4
	.byte 5		; 5
	.byte 6		; 6
	.byte 7		; 7
	.byte 8 	; 8
	.byte 5		; 9
	.byte 6		; 10
	.byte 7		; 11
	.byte 8 	; 12
	.byte 10	; 13
	.byte 8		; 14
	.byte 7 	; 15
	.byte 1		; 16 (loop to) 1

	.byte 20	; 17 BADNUMBER
	.byte 0		; 18 

	.byte 12	; 19 GOODNUMBER
	.byte 10	; 20 
	.byte 7 	; 21
	.byte 0		; 22

	.byte 1 	; 23 STAR
	.byte 0  	; 24

AudioControl:
	.byte 0		; 0
	.byte 12	; 1 SOUNDTRACK
	.byte 12	; 2
	.byte 12	; 3
	.byte 12	; 4
	.byte 12	; 5
	.byte 12	; 6
	.byte 12	; 7
	.byte 12	; 8
	.byte 12	; 9
	.byte 12	; 10
	.byte 12	; 11
	.byte 12	; 12
	.byte 12	; 13
	.byte 12	; 14
	.byte 12	; 15
	.byte 0		; 16

	.byte 7		; 17 BADNUMBER
	.byte 0		; 18 

	.byte 12	; 19 GOODNUMBER
	.byte 12	; 20 
	.byte 12	; 21
	.byte 0		; 22

	.byte 3 	; 23 STAR
	.byte 0  	; 24

AudioVolume:
	.byte 0		; 0
	.byte 2 	; 1 SOUNDTRACK
	.byte 2 	; 2
	.byte 2 	; 3
	.byte 2 	; 4
	.byte 2 	; 5
	.byte 2 	; 6
	.byte 2 	; 7
	.byte 2 	; 8
	.byte 2 	; 9
	.byte 2 	; 10
	.byte 2 	; 11
	.byte 2 	; 12
	.byte 2 	; 13
	.byte 2 	; 14
	.byte 2 	; 15
	.byte 0		; 16

	.byte 12	; 17 BADNUMBER
	.byte 0		; 18

	.byte 10	; 19 GOODNUMBER
	.byte 12	; 20
	.byte 10	; 21
	.byte 0		; 22

	.byte 12	; 23 STAR
	.byte 0  	; 24

