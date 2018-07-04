
	org 0h
section .text
	[BITS 16]

	; Jump Table - "origin+4"
jump_table:
	jmp word sound_init
	nop
	jmp word sound_uninit
	nop



; MACROS
; Initialize the PIT2 to make the speaker output a square wave
%macro SPEAKER_PIT2_INIT 0
	mov al, 10110110b
	out 43h, al
%endmacro

; Set the frequency of the square wave
; @param ax Frequency (1193182/18.2 = 65536)
%macro SPEAKER_PIT2_FREQ 0
	out 42h, al			; write low to PIT counter 2's port
	mov al, ah			; move ah to low
	out 42h, al			; write low (high) to same port
%endmacro

; Set the frequency of the timer interrupt function
; @param ax Frequency (1193182/18.2 = 65536)
%macro SPEAKER_PIT0_FREQ 0
	out 40h, al			; write low to PIT counter 0's port
	mov al, ah			; move ah to low
	out 40h, al			; write low (high) to same port
%endmacro

; turn on the speaker
%macro SPEAKER_ON 0
	in al, 61h			; read PPI/Keyboard port B (because speaker bit is here)
	or al, 00000011b	; mask-in the speaker and the "timer 2 gate" bits
	out 61h, al			; re-write it to the PPI/Keyboard port B (i.e. TURN ON SOUND)
%endmacro

; turn off the speaker
%macro SPEAKER_OFF 0
	in al, 61h			; read the PPI/Keyboard port B again
	and al, 11111100b	; mask-out the speaker and "timer 2 gate" bits
	out 61h, al			; re-write it to PPI/Keyboard port B (i.e. TURN SOUND OFF)
%endmacro



; Used to initialize the sound interface
sound_init:
	; store DS
	push ds
	push es

	; read the original timer interrupt
	mov ax, 0
	mov ds, ax
	mov ax, [ds:(1Ch*4)+0]
	mov bx, ax
	mov ax, [ds:(1Ch*4)+2]
	; store the original timer interrupt
	mov [old_interrupt+0], bx
	mov [old_interrupt+2], ax

	; from here on, use CS as the address of DS
	mov ax, cs
	mov ds, ax

	; switch to our custom timer interrupt
	cli
	mov ax, 0
	mov es, ax
	mov ax, sound_interrupt
	mov [es:(1Ch*4)+0], ax
	mov ax, cs
	mov [es:(1Ch*4)+2], ax
	sti

	; Configure PIT2 to modulate a square wave
	SPEAKER_PIT2_INIT

	; restore DS, ES and return
	pop es
	pop ds
	ret


; Used to shutdown the sound interface
sound_uninit:
	; store DS and use CS as the address of DS
	push ds
	mov ax, cs
	mov ds, ax

	; restore the original timer interrupt
	cli
	mov ax, 0
	mov es, ax
	mov ax, [old_interrupt+0]
	mov [es:(1Ch*4)+0], ax
	mov ax, [old_interrupt+2]
	mov [es:(1Ch*4)+2], ax
	sti

	; turn off the speaker
	SPEAKER_OFF

	; restore DS and return
	pop ds
	ret


; Never called directly, but ticked many times per second for music playback
sound_interrupt:
	iret


; ----------------------------------------------------------------------------------------------- ;
; Initialized Data
section .data

; Placeholder song for when no song is currently set -
empty_song:
	; HEADER SECTION
	dw 2		; SECTION SIZE
	dw 120		; BPM
	dw 0		; Loop Point (or FFFFh if oneshot)

	; ORDER SECTION
	dw 2		; SECTION SIZE
	; Data
	dw 0

	; PATTERN SECTION
	dw 8		; SECTION SIZE
	dw 1		; Width
	dw 4		; Height
	; Data
	db 24h, 7fh
	db 28h, 7fh


; Note lookup table: C1-B8 (i.e. 96 possible values) - 192 bytes
note_table:
	; 	C,				C#,				D,				D#,				E,				F,				F#,				G,				G#,				A,				A#,				B
	dw	1193182/32,		1193182/34,		1193182/36,		1193182/38,		1193182/41,		1193182/43,		1193182/46,		1193182/49,		1193182/51,		1193182/55,		1193182/58,		1193182/61;
	dw	1193182/65,		1193182/69,		1193182/73,		1193182/77,		1193182/82,		1193182/87,		1193182/92,		1193182/98,		1193182/103,	1193182/110,	1193182/116,	1193182/123;
	dw	1193182/130,	1193182/138,	1193182/146,	1193182/155,	1193182/164,	1193182/174,	1193182/185,	1193182/196,	1193182/207,	1193182/220,	1193182/233,	1193182/246;
	dw	1193182/261,	1193182/277,	1193182/293,	1193182/311,	1193182/329,	1193182/349,	1193182/370,	1193182/392,	1193182/415,	1193182/440,	1193182/466,	1193182/493;
	dw	1193182/523,	1193182/554,	1193182/587,	1193182/622,	1193182/659,	1193182/698,	1193182/740,	1193182/784,	1193182/830,	1193182/880,	1193182/932,	1193182/987;
	dw	1193182/1047,	1193182/1109,	1193182/1175,	1193182/1245,	1193182/1319,	1193182/1397,	1193182/1480,	1193182/1568,	1193182/1661,	1193182/1760,	1193182/1865,	1193182/1976;
	dw	1193182/2093,	1193182/2217,	1193182/2349,	1193182/2489,	1193182/2637,	1193182/2794,	1193182/2960,	1193182/3136,	1193182/3322,	1193182/3520,	1193182/3729,	1193182/3951;
	dw	1193182/4186,	1193182/4435,	1193182/4699,	1193182/4978,	1193182/5274,	1193182/5588,	1193182/5920,	1193182/6272,	1193182/6645,	1193182/7040,	1193182/7459,	1193182/7902;


; ----------------------------------------------------------------------------------------------- ;
; Uninitialized Data
section .bss
old_interrupt:
	resb 4