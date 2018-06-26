
print MACRO
	mov ah, 9
	mov dx, #1
	int 21h
#EM

start:
	print(hello)
	mov [myvar], ah


	ret

; To store hardcoded data, just store it
hello:
	db "Hello Wuurm!", 10, '$';

; to A86, the DATA segment is actually the BSS segment (i.e. uninitialized memory)
DATA SEGMENT
myvar	db	?
DATA ENDS	; optional
