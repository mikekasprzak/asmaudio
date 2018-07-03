
%macro print 1
	mov ah, 9
	mov dx, %1
	int 21h
%endmacro

%macro keywait 0
	mov ah, 0
	int 16h
%endmacro

%macro keyget 0
	mov ah, 1
	int 16h
%endmacro

%macro speaker_init 0
	mov al, 10110110b
	out 43h, al
%endmacro

%macro speaker_on 0
	in al, 61h			; read PPI/Keyboard port B (because speaker bit is here)
	or al, 00000011b	; mask-in the speaker and the "timer 2 gate" bits
	out 61h, al			; re-write it to the PPI/Keyboard port B (i.e. TURN ON SOUND)
%endmacro

%macro speaker_off 0
	in al, 61h			; read the PPI/Keyboard port B again
	and al, 11111100b	; mask-out the speaker and "timer 2 gate" bits
	out 61h, al			; re-write it to PPI/Keyboard port B (i.e. TURN SOUND OFF)
%endmacro

%macro speaker_toggle 0
	in al, 61h			; read the PPI/Keyboard port B again
	xor al, 00000011b	; mask-out the speaker and "timer 2 gate" bits
	out 61h, al			; re-write it to PPI/Keyboard port B (i.e. TOGGLE SOUND)
%endmacro

%macro speaker_freq 1
	mov ax, %1			; Frequency
	out 42h, al			; write low to PIT counter 2's port
	mov al, ah			; move ah to low
	out 42h, al			; write low (high) to same port
%endmacro

%macro _speaker_freq 0
	;mov ax, %1			; Frequency
	out 42h, al			; write low to PIT counter 2's port
	mov al, ah			; move ah to low
	out 42h, al			; write low (high) to same port
%endmacro


	org 100h 
section .text
	[BITS 16]

start:
	speaker_init
	speaker_freq(4560)
	;speaker_on
	
	push ds
	mov ax, 0
	mov ds, ax
	mov ax, [ds:(1Ch*4)+0]
	mov bx, ax
	mov ax, [ds:(1Ch*4)+2]
	pop ds
	
	mov [old_int], bx
	mov [old_int+2], ax
	
	; https://wiki.osdev.org/Programmable_Interval_Timer
	cli
	mov ax, 0
	mov es, ax
	mov ax, speaker_int
	mov [es:(1Ch*4)+0], ax
	mov ax, cs
	mov [es:(1Ch*4)+2], ax
	
	; 1.193182 MHz
	; 1193182/65536=18.2065 (default)
	; 1193182/18.2065=65536 (reverse)
	; thus, divide by the number of steps you want

	; IMPORTANT: To avoid division by zero, 0 is treated as 65536 (i.e. the largest value).
	; WARNING: Use too small a value (1), and there wont be enough CPU time to do anything but interrupts.
	mov ax, 1193182/20;//50
	out 40h, al				; Frequency Divider 0 (low)
	mov al, ah
	out 40h, al				; Frequency Divider 0 (high)
	
	;mov al, 00110110b	; doesn't work
	;out 43h, al
	;in al, 43h ; 0xff for some reason

	sti

;	mov bx, 40
;woo:
;	mov ax, -1
;woop:
;	nop
;	dec ax
;	jnz woop
;	dec bx
;	jnz woo

	print(pressakey)

	;keywait
loop:
	keyget
	jz loop

	cli
	mov ax, 0
	mov es, ax
	mov ax, [old_int]
	mov [es:(1Ch*4)+0], ax
	mov ax, [old_int+2]
	mov [es:(1Ch*4)+2], ax
	sti

	speaker_off
	
	ret

speaker_int:
	push ax
	push bx
	push si
	mov ax, [song_pos]
	;mov bx, ax
	mov si, song+4
	add si, ax*1
	mov al, [si]
	
	cmp al, 0
	jz int_done
	
	cmp al, 7fh
	jz int_note_off
	jle int_note_on
	
	jmp int_done
int_note_on:
	mov bx, notes
	xor ah, ah		; zero ah
	dec al			; use 1 less
	add bx, ax
	mov ax, [bx]
	
	_speaker_freq
	speaker_on
	
	jmp int_done
int_note_off:
	speaker_off
	;jmp int_done
int_done:
	mov ax, [song_pos]
	inc ax
	;and ax, 63h
	mov [song_pos], ax
	
	;inc word [song_pos]

;	speaker_toggle
	pop si
	pop bx
	pop ax
	iret

section .data 
	; put data items here
pressakey:
	db "Press a key to continue.", 10, '$';

notes:
	; IMPORTANT: Can't do first row because the smallest timer tick is 18.2 (i.e. 65536)
	; 	C,				C#,				D,				D#,				E,				F,				F#,				G,				G#,				A,				A#,				B
;	dw	1193182/16,		1193182/17,		1193182/18,		1193182/19,	1193182/20,	1193182/21,	1193182/23,	1193182/24,	1193182/25,	1193182/27,	1193182/29,	1193182/30;
	dw	1193182/32,		1193182/34,		1193182/36,		1193182/38,	1193182/41,	1193182/43,	1193182/46,	1193182/49,	1193182/51,	1193182/55,	1193182/58,	1193182/61;
	dw	1193182/65,		1193182/69,		1193182/73,		1193182/77,	1193182/82,	1193182/87,	1193182/92,	1193182/98,	1193182/103,	1193182/110,	1193182/116,	1193182/123;
	dw	1193182/130,	1193182/138,	1193182/146,	1193182/155,	1193182/164,	1193182/174,	1193182/185,	1193182/196,	1193182/207,	1193182/220,	1193182/233,	1193182/246;
	dw	1193182/261,	1193182/277,	1193182/293,	1193182/311,	1193182/329,	1193182/349,	1193182/370,	1193182/392,	1193182/415,	1193182/440,	1193182/466,	1193182/493;
	dw	1193182/523,	1193182/554,	1193182/587,	1193182/622,	1193182/659,	1193182/698,	1193182/740,	1193182/784,	1193182/830,	1193182/880,	1193182/932,	1193182/987;
	dw	1193182/1047,	1193182/1109,	1193182/1175,	1193182/1245,	1193182/1319,	1193182/1397,	1193182/1480,	1193182/1568,	1193182/1661,	1193182/1760,	1193182/1865,	1193182/1976;
	dw	1193182/2093,	1193182/2217,	1193182/2349,	1193182/2489,	1193182/2637,	1193182/2794,	1193182/2960,	1193182/3136,	1193182/3322,	1193182/3520,	1193182/3729,	1193182/3951;
	dw	1193182/4186,	1193182/4435,	1193182/4699,	1193182/4978,	1193182/5274,	1193182/5588,	1193182/5920,	1193182/6272,	1193182/6645,	1193182/7040,	1193182/7459,	1193182/7902;

;;	dw	1193182/16.35,	1193182/17.32,	1193182/18.35,	1193182/19.45,	1193182/20.60,	1193182/21.83,	1193182/23.12,	1193182/24.50,	1193182/25.96,	1193182/27.50,	1193182/29.14,	1193182/30.87;
;	dw	1193182/32.70,	1193182/34.65,	1193182/36.71,	1193182/38.89,	1193182/41.20,	1193182/43.65,	1193182/46.25,	1193182/49.00,	1193182/51.91,	1193182/55.00,	1193182/58.27,	1193182/61.74;
;	dw	1193182/65.41,	1193182/69.30,	1193182/73.42,	1193182/77.78,	1193182/82.41,	1193182/87.31,	1193182/92.50,	1193182/98.00,	1193182/103.8,	1193182/110.0,	1193182/116.5,	1193182/123.5;
;	dw	1193182/130.8,	1193182/138.6,	1193182/146.8,	1193182/155.6,	1193182/164.8,	1193182/174.6,	1193182/185.0,	1193182/196.0,	1193182/207.7,	1193182/220.0,	1193182/233.1,	1193182/246.9;
;	dw	1193182/261.6,	1193182/277.2,	1193182/293.7,	1193182/311.1,	1193182/329.6,	1193182/349.2,	1193182/370.0,	1193182/392.0,	1193182/415.3,	1193182/440.0,	1193182/466.2,	1193182/493.9;
;	dw	1193182/523.3,	1193182/554.4,	1193182/587.3,	1193182/622.3,	1193182/659.3,	1193182/698.5,	1193182/740.0,	1193182/784.0,	1193182/830.6,	1193182/880.0,	1193182/932.3,	1193182/987.8;
;	dw	1193182/1047,	1193182/1109,	1193182/1175,	1193182/1245,	1193182/1319,	1193182/1397,	1193182/1480,	1193182/1568,	1193182/1661,	1193182/1760,	1193182/1865,	1193182/1976;
;	dw	1193182/2093,	1193182/2217,	1193182/2349,	1193182/2489,	1193182/2637,	1193182/2794,	1193182/2960,	1193182/3136,	1193182/3322,	1193182/3520,	1193182/3729,	1193182/3951;
;	dw	1193182/4186,	1193182/4435,	1193182/4699,	1193182/4978,	1193182/5274,	1193182/5588,	1193182/5920,	1193182/6272,	1193182/6645,	1193182/7040,	1193182/7459,	1193182/7902;

song_pattern:
	dw 0
song_pos:
	dw 0
song:
	incbin "src/out.bin";

section .bss
	; put uninitialized data here
old_int: resb 4

