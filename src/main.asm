
%macro print 1
	mov ah, 9
	mov dx, %1
	int 21h
%endmacro

;%macro print2 1
	;section .text
	;mov ah, 9
	;mov dx, %%text
	;int 21h	
;%%text:
	;section .data
	;db %1, 10, '$';
;%endmacro

	org 100h 
section .text
	[BITS 16]

start:
	print(hello)
;	print2("I don't like you");

	
	ret

section .data 
	; put data items here
hello:
	db "Hello Wuurm!", 10, '$';

section .bss 
	; put uninitialized data here
