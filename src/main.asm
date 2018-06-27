
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
	
	
	cli
	mov ax, 0
	mov es, ax
	mov ax, speaker_int
	mov [es:(1Ch*4)+0], ax
	mov ax, cs
	mov [es:(1Ch*4)+2], ax

	; IMPORTANT: To avoid division by zero, 0 is treated as 65536 (i.e. the largest value).
	; WARNING: Use too small a value (1), and there wont be enough CPU time to do anything but interrupts.
	mov ax, 40000
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
	speaker_toggle
	pop ax
	iret

section .data 
	; put data items here
pressakey:
	db "Press a key to continue.", 10, '$';

section .bss
	; put uninitialized data here
old_int: resb 4
