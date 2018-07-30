; ----------------------------------------------------------------------------------------------- ;
CODE SEGMENT
ORG 0100
; ----------------------------------------------------------------------------------------------- ;
	jmp main
; ----------------------------------------------------------------------------------------------- ;

; ----------------------------------------------------------------------------------------------- ;
; MACROS
; ----------------------------------------------------------------------------------------------- ;
PRINT MACRO
	mov ah, 9
	mov dx, #1
	int 21h
#EM

KEY_GET MACRO
	mov ah, 1
	int 16h
#EM

KEY_CONSUME MACRO
	mov ah, 0
	int 16h
#EM

; @returns AX File Handle
; @returns CF if error
FILE_OPEN MACRO
	mov dx, #1
	mov ah, 3Dh
	mov al, 0h
	int 21h
#EM
_FILE_OPEN MACRO
	mov ah, 3Dh
	mov al, 0h
	int 21h
#EM

; @param #1 File Handle (bx)
FILE_CLOSE MACRO
	mov bx, #1
	mov ax, 3E00h
	int 21h
#EM
_FILE_CLOSE MACRO
	mov ax, 3E00h
	int 21h
#EM

; @param #1 File Handle (bx)
; @param dx Address to store data in
; @param ds Segment to store data in
FILE_READ MACRO
	mov bx, #1
	mov cx, -1
	mov ax, 3F00h
	int 21h
#EM
_FILE_READ MACRO
	mov cx, -1
	mov ax, 3F00h
	int 21h
#EM
; ----------------------------------------------------------------------------------------------- ;

; ----------------------------------------------------------------------------------------------- ;
error:
	PRINT(tError);
	ret
; ----------------------------------------------------------------------------------------------- ;
main:
	PRINT(tLoading)
	PRINT(tSongFile)
	PRINT(tNewline)
	; Read the Song
	mov dx, tSongFile
	mov cx, song
	call read_file
	jc error


	PRINT(tLoading)
	PRINT(tSpeakerDriverFile0)
	PRINT(tNewline)
	; Read the Sound Engine
	mov dx, tSpeakerDriverFile0
	mov cx, audio_engine
	call read_file
	jc error
	call play_engine


	PRINT(tNewline)
	PRINT(tLoading)
	PRINT(tSpeakerDriverFile1)
	PRINT(tNewline)
	; Read the Sound Engine
	mov dx, tSpeakerDriverFile1
	mov cx, audio_engine
	call read_file
	jc error
	call play_engine


	PRINT(tNewline)
	PRINT(tLoading)
	PRINT(tSpeakerDriverFile2)
	PRINT(tNewline)
	; Read the Sound Engine
	mov dx, tSpeakerDriverFile2
	mov cx, audio_engine
	call read_file
	jc error2
	call play_engine


	PRINT(tNewline)
	PRINT(tLoading)
	PRINT(tSpeakerDriverFile3)
	PRINT(tNewline)
	; Read the Sound Engine
	mov dx, tSpeakerDriverFile3
	mov cx, audio_engine
	call read_file
	jc error2
	call play_engine

	ret

; ----------------------------------------------------------------------------------------------- ;
error2:
	PRINT(tError);
	ret
; ----------------------------------------------------------------------------------------------- ;
play_engine:
	; Hook-up the jump table for the active Sound Engine
	mov ax, audio_engine
	;shr ax, 4		; According to A86 this is unsupported on 8086/8088.
	shr ax, 1
	shr ax, 1
	shr ax, 1
	shr ax, 1

	mov bx, ds
	add ax, bx
	mov [audio_init+2], ax
	mov [audio_uninit+2], ax
	mov [audio_playMusic+2], ax
	mov [audio_stopMusic+2], ax
	mov [audio_pauseMusic+2], ax
	mov [audio_resumeMusic+2], ax
	mov [audio_playSound+2], ax
	mov [audio_stopSound+2], ax
	mov [audio_pauseSound+2], ax
	mov [audio_resumeSound+2], ax

	; Initialize the Sound Engine
	call far [audio_init]

	; Play the song
	mov ax, (song - audio_engine)
	call far [audio_playMusic]

	PRINT(tPressAKey)

_loop:
	KEY_GET
	jz _loop

	KEY_CONSUME

	; Uninitialize the Sound Engine
	call far [audio_uninit]
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


_read_file_handle dw ?			;resw 1
; ----------------------------------------------------------------------------------------------- ;
; A86 has its own concept of a DATA and BSS section
;section .data
; ----------------------------------------------------------------------------------------------- ;
EVEN 16 ;align 16
; Functions
audio_init:
	dw 4, 0
audio_uninit:
	dw 8, 0

audio_playMusic:
	dw 16, 0
audio_stopMusic:
	dw 20, 0
audio_pauseMusic:
	dw 24, 0
audio_resumeMusic:
	dw 28, 0

audio_playSound:
	dw 32, 0
audio_stopSound:
	dw 36, 0
audio_pauseSound:
	dw 40, 0
audio_resumeSound:
	dw 44, 0

; Strings
tPressAKey:
	db "Press a key", 13, 10, '$'
tLoading:
	db "Loading: ", '$'
tNewline:
	db 13, 10, '$'

tError:
	db "Error", 13, 10, "$"

tSpeakerDriverFile:
tSpeakerDriverFile0:
	db "speaker.drv", 0, " - PC Speaker$"
tSpeakerDriverFile1:
	db "tandy.drv", 0, " - Tandy 1000$"
tSpeakerDriverFile2:
	db "opl.drv", 0, " - Adlib/Sound Blaster/OPL$"
tSpeakerDriverFile3:
	db "roland.drv", 0, " - MPU-401/Roland Sound Canvas$"


tSongFile:
	db "melody.bin", 0, "$"



; ----------------------------------------------------------------------------------------------- ;
; to A86, the DATA segment is actually the BSS segment (i.e. uninitialized memory)
DATA SEGMENT
; ----------------------------------------------------------------------------------------------- ;
EVEN 16	; align 16
audio_engine:
	db 2048 DUP ?	; resb 2048

song:
	db 2048 DUP ?	; resb 2048
