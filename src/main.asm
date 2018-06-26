
%macro print 1
	mov ah, 9
	mov dx, %1
	int 21h
%endmacro

%macro keywait 0
	mov ah, 0
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
	
	;mov ax, 0
	;out 40h, al
	;mov al, ah
	;out 40h, al
	mov al, 00110110b
	out 43h, al
	
	; need to store the section, then the function address
	; also need to figure out where we are (section), heh
	;cli
	mov ax, cs
	shr ax, 2
	mov [1Ch*4], ax
	mov ax, speaker_int
	mov [(1Ch*4)+2], ax
	;sti

	print(pressakey)
	keywait

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
oldint: resw 1
