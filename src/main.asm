; ----------------------------------------------------------------------------------------------- ;
org 100h
; ----------------------------------------------------------------------------------------------- ;
section .text
	[BITS 16]
; ----------------------------------------------------------------------------------------------- ;


; ----------------------------------------------------------------------------------------------- ;
; MACROS
; ----------------------------------------------------------------------------------------------- ;
%macro PRINT 1
	mov ah, 9
	mov dx, %1
	int 21h
%endmacro


%macro KEY_GET 0
	mov ah, 1
	int 16h
%endmacro

; @returns AX File Handle
; @returns CF if error
%macro FILE_OPEN 1
	mov dx, %1
	mov ah, 3Dh
	mov al, 0h
	int 21h
%endmacro

; @param %1 File Handle
%macro FILE_CLOSE 1
	mov bx, %1
	mov ax, 3E00h
	int 21h
%endmacro

; @param %1 File Handle
; @param dx Address to store data in
; @param ds Segment to store data in
%macro FILE_READ 1
	mov bx, %1
	mov cx, -1
	mov ax, 3F00h
	int 21h
%endmacro

;%macro CALL_SOUND_INIT
;
;%endmacro


; ----------------------------------------------------------------------------------------------- ;
main:
	FILE_OPEN(tSpeakerFile)

	jnc main_open
	PRINT(tError)
	jmp done
main_open:
	mov [file_handle], ax				; Remember the file handle
	mov dx, sound_engine
	FILE_READ(ax)

	jnc main_read
	PRINT(tError)
	PRINT(tError)
	jmp done

main_read:
	mov ax, [file_handle]				; Fetch the file handle
	FILE_CLOSE(ax)

	jnc done
	PRINT(tError)
	PRINT(tError)
	PRINT(tError)
	jmp done
done:

	mov ax, sound_engine
	shr ax, 4
	mov bx, ds
	add ax, bx
	mov [sound_init+2], ax
	mov [sound_uninit+2], ax

	call far [sound_init]

	PRINT(tPressAKey)

loop:
	KEY_GET
	jz loop

	call far [sound_uninit]


	ret


; ----------------------------------------------------------------------------------------------- ;
section .data
; ----------------------------------------------------------------------------------------------- ;
tPressAKey:
	db "Press a key to Exit", 10, '$';

tError:
	db "Error", 10, "$";

tSpeakerFile:
	db "speaker.bin", 0;


sound_init:
	dw 4, 0
sound_uninit:
	dw 8, 0

section .bss
align 16
sound_engine:
	resb 2048

file_handle:
	resw 1
