
print MACRO
	mov ah, 9
	mov dx, #1
	int 21h
#EM

CODE SEGMENT
ORG 0100
start:
	print(hello)
	mov [myvar], ah


	ret

; Include other files
;INCLUDE blah.asm

; Hardcoded data is stored in the code segment
EVEN 16		; align to the 16 byte boundary
hello:
	db "Hello Wuurm!", 10, '$';

; to A86, the DATA segment is actually the BSS segment (i.e. uninitialized memory)
DATA SEGMENT
myvar	db	?
DATA ENDS	; optional
