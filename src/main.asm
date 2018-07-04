; ----------------------------------------------------------------------------------------------- ;
org 100h
[BITS 16]
; ----------------------------------------------------------------------------------------------- ;
section .text
; ----------------------------------------------------------------------------------------------- ;
	jmp main
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
%macro _FILE_OPEN 0
	mov ah, 3Dh
	mov al, 0h
	int 21h
%endmacro

; @param %1 File Handle (bx)
%macro FILE_CLOSE 1
	mov bx, %1
	mov ax, 3E00h
	int 21h
%endmacro
%macro _FILE_CLOSE 0
	mov ax, 3E00h
	int 21h
%endmacro

; @param %1 File Handle (bx)
; @param dx Address to store data in
; @param ds Segment to store data in
%macro FILE_READ 1
	mov bx, %1
	mov cx, -1
	mov ax, 3F00h
	int 21h
%endmacro
%macro _FILE_READ 0
	mov cx, -1
	mov ax, 3F00h
	int 21h
%endmacro



; ----------------------------------------------------------------------------------------------- ;
main:
	; Read the Sound Engine
	mov dx, tSpeakerFile
	mov cx, sound_engine
	call read_file
	jc error

	; Hook-up the jump table for the active Sound Engine
	mov ax, sound_engine
	shr ax, 4
	mov bx, ds
	add ax, bx
	mov [sound_init+2], ax
	mov [sound_uninit+2], ax
	
	; Read the Song
	mov dx, tSongFile
	mov cx, song
	call read_file
	jc error

	; Initialize the Sound Engine
	call far [sound_init]
	
	; Play the song
	;mov ax, song
	;call far [sound_playSong]

	PRINT(tPressAKey)

loop:
	KEY_GET
	jz loop

	; Uninitialize the Sound Engine
	call far [sound_uninit]


	ret
error:
	PRINT(tError);
	ret

; ----------------------------------------------------------------------------------------------- ;
; @param dx ASCIIZ filename address
; @param cx Destination address
; @returns cf Error
; @returns ax Size of data read
read_file:
	_FILE_OPEN

	jnc _read_file_open
	jmp _read_file_done
_read_file_open:
	mov [_read_file_handle], ax				; Remember the file handle
	mov dx, cx
	FILE_READ(ax)

	jnc _read_file_read
	jmp _read_file_done
_read_file_read:
	mov cx, ax
	mov ax, [_read_file_handle]				; Fetch the file handle
	FILE_CLOSE(ax)

	jnc _read_file_success
	jmp _read_file_done
_read_file_success:
	mov ax, cx
_read_file_done:
	ret
	
; ----------------------------------------------------------------------------------------------- ;
section .data
; ----------------------------------------------------------------------------------------------- ;
align 16
; Functions
sound_init:
	dw 4, 0
sound_uninit:
	dw 8, 0

; Strings
tPressAKey:
	db "Press a key to Exit", 10, '$';

tError:
	db "Error", 10, "$";

tSpeakerFile:
	db "speaker.bin", 0;
tSongFile:
	db "sp-song.bin", 0;



; ----------------------------------------------------------------------------------------------- ;
section .bss
; ----------------------------------------------------------------------------------------------- ;
align 16
sound_engine:
	resb 2048

song:
	resb 2048

_read_file_handle:
	resw 1
	
