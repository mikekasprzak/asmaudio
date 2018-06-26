
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

%macro speaker_freq 1
	mov ax, %1		; Frequency
	out 42h, al		; write low to PIT counter 2's port
	mov al, ah		; move ah to low
	out 42h, al		; write low (high) to same port
%endmacro


	org 100h 
section .text
	[BITS 16]

start:
	speaker_init
	speaker_freq(4560)
	speaker_on

	print(pressakey)
	keywait

	speaker_off
	
	ret

section .data 
	; put data items here
pressakey:
	db "Press a key to continue.", 10, '$';

section .bss
	; put uninitialized data here
oldint: resw 1
