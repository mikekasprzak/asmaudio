; ----------------------------------------------------------------------------------------------- ;
org 0h							; org 0 because we are not a program and we only care about ourselves
[BITS 16]
; ----------------------------------------------------------------------------------------------- ;
section .text
; ----------------------------------------------------------------------------------------------- ;
; Jump Table - "origin+4"
jump_table:
	; function 0
	ret							; just returns (which makes running us do nothing)
	nop
	nop
	nop
	; function 1
	jmp word audio_init			; initializes audio
	nop
	; function 2
	jmp word audio_uninit		; shuts down audio
	nop
	; function 3
	ret							; ??
	nop
	nop
	nop

	; function 4
	jmp word audio_playMusic	; plays a song
	nop
	; function 5
	jmp word audio_stopMusic	; stops a song
	nop
	; function 6
	jmp word audio_pauseMusic	; pauses the song
	nop
	; function 7
	jmp word audio_resumeMusic	; resumes the song
	nop


	; function 8
	jmp word audio_playSound	; plays a sound
	nop
	; function 9
	jmp word audio_stopSound	; stop a sound
	nop
	; function 10
	jmp word audio_pauseSound	; pause a sound
	nop
	; function 11
	jmp word audio_resumeSound	; resume a sound
	nop

; ----------------------------------------------------------------------------------------------- ;
; MACROS
; ----------------------------------------------------------------------------------------------- ;
; Initialize the PIT2 to make the speaker output a square wave
%macro SPEAKER_PIT2_INIT 0
	mov al, 10110110b
	out 43h, al
%endmacro

; Set the frequency of the square wave
; @param ax Frequency (1193182/18.2 = 65536)
%macro SPEAKER_PIT2_FREQ 0
	out 42h, al					; write low to PIT counter 2's port
	mov al, ah					; move ah to low
	out 42h, al					; write low (high) to same port
%endmacro

; Set the frequency of the timer interrupt function
; @param ax Frequency (1193182/18.2 = 65536)
%macro SPEAKER_PIT0_FREQ 0
	out 40h, al					; write low to PIT counter 0's port
	mov al, ah					; move ah to low
	out 40h, al					; write low (high) to same port
%endmacro

; turn on the speaker
%macro SPEAKER_ON 0
	in al, 61h					; read PPI/Keyboard port B (because speaker bit is here)
	or al, 00000011b			; mask-in the speaker and the "timer 2 gate" bits
	out 61h, al					; re-write it to the PPI/Keyboard port B (i.e. TURN ON SOUND)
%endmacro

; turn off the speaker
%macro SPEAKER_OFF 0
	in al, 61h					; read the PPI/Keyboard port B again
	and al, 11111100b			; mask-out the speaker and "timer 2 gate" bits
	out 61h, al					; re-write it to PPI/Keyboard port B (i.e. TURN SOUND OFF)
%endmacro

; toggle the speaker
%macro SPEAKER_TOGGLE 0
	in al, 61h					; read the PPI/Keyboard port B again
	xor al, 00000011b			; mask-out the speaker and "timer 2 gate" bits
	out 61h, al					; re-write it to PPI/Keyboard port B (i.e. TURN SOUND OFF)
%endmacro



is_input:
     ;---Return state of DATA SET READY. MPU clears this line when it
     ;   has a byte waiting to be read from its DATA port.
     mov   dx,0331h
     in    al,dx
     and   al,80h
     ret

get_mpu_in:
     mov   dx,0330h
     in    al,dx
     ret

is_output:
     ;---Return state of DATA READ READY. MPU clears this line when it's
     ;	 OK for us to write to the MPU's ports.
     mov   dx,0331h
     in    al,dx
     and   al,40h
     ret

put_mpu_out:
     mov   dx,0330h
     out   dx,al
     ret

set_uart:
     ;---Wait until it's OK to write to the MPU's ports. Note: if there's
     ;   something wrong with the MPU, we could be locked in this loop
     ;   forever. You really should add a little "escape code" within this
     ;   first loop, or at least a timeout of let's say 1 second.
sr1: call  is_output
     jnz   SHORT sr1
     ;---Send FF command to the MPU.
     mov   al,0FFh
     out   dx,al
     ;---Wait for the MPU to make a byte available for reading from its
     ;   DATA port. You could also lock up here if you're dealing with
     ;   a game card that doesn't even implement a bi-directional
     ;   COMMAND/STATUS port. Therefore, another few seconds time-out is
     ;   appropriate here, and if a time-out occurs, just jump to sr3.
again:
     call  is_input
     jnz   SHORT again
     ;---Get the byte and check for an ACK (FE) to the cmd we sent. If
     ;   not an ACK, discard this and keep looking for that ACK.
     call  get_mpu_in
     cmp   al,0FEh
     jne   SHORT again
     ;---Wait until it's OK to write to the MPU's ports.
sr3: call  is_output
     jnz   SHORT sr3
     ;---Send 3F command to the MPU
     mov   al,03Fh
     out   dx,al
     ret

; ----------------------------------------------------------------------------------------------- ;
; Used to initialize the sound interface
audio_init:
	; store DS and ES
	push ds
	push es

	; read the original timer interrupt
	mov ax, 0
	mov ds, ax
	mov ax, [ds:(1Ch*4)+0]
	mov bx, ax
	mov ax, [ds:(1Ch*4)+2]
	mov cx, ax
	; from here on, use CS as the address of DS
	mov ax, cs
	mov ds, ax
	; store the original timer interrupt
	mov [old_interrupt+0], bx
	mov [old_interrupt+2], cx

	; switch to our custom timer interrupt
	cli
	mov ax, 0
	mov es, ax
	mov ax, audio_interrupt
	mov [es:(1Ch*4)+0], ax
	mov ax, cs
	mov [es:(1Ch*4)+2], ax
	sti

	; Configure PIT2 to modulate a square wave
;	SPEAKER_PIT2_INIT

;	mov ax, 4444
;	SPEAKER_PIT2_FREQ
;	SPEAKER_ON


	call set_uart
	
hee:
	call is_output
	jnz hee
	
	mov al, 10010000b
	call put_mpu_out

haa:
	call is_output
	jnz haa

	mov al, 55
	call put_mpu_out

hoo:
	call is_output
	jnz hoo

	mov al, 127
	call put_mpu_out

huu:
	call is_output
	jnz huu


	; restore DS, ES and return
	pop es
	pop ds
	retf

; ----------------------------------------------------------------------------------------------- ;
; Used to shutdown the sound interface
audio_uninit:
	; store DS and use CS as the address of DS
	push ds
	push es
	mov ax, cs
	mov ds, ax

	; restore the original timer interrupt
	cli
	mov ax, 0
	mov es, ax
	mov ax, [old_interrupt+0]
	mov [es:(1Ch*4)+0], ax
	mov ax, [old_interrupt+2]
	mov [es:(1Ch*4)+2], ax
	sti	

	; Set frequency back to default (0, aka 65536)
	;mov ax, 0
	;SPEAKER_PIT0_FREQ

	; turn off the speaker
	;SPEAKER_OFF

	; restore DS, ES and return
	pop es
	pop ds
	retf

; ----------------------------------------------------------------------------------------------- ;
; ----------------------------------------------------------------------------------------------- ;
; @param %1 State address
%macro SONG_DECODE_PAT 1
	mov word si, %1 + State.data

	mov word ax, [si]	; ax = State.data address. auto-inc, si = State.seq
	mov word bx, [si+2]	; bx = State.seq index. auto-inc, si = State.pat
	mov word di, si		; di = State.pat address
	mov word si, ax		; si = State.data address
	mov word ax, [si]	; ax = sizeof the Header section
	add word si, ax		; si = Sequence section
	mov word dx, [si]	; dx = sizeof the Sequence section (in bytes). auto-inc, si = Sequence #0 address
	; *** expecting si+2
		add si, 2
	mov word ax, si		; ax = Sequence #0 address
	shl word bx, 1		; bx = State.seq * 2
	add word si, bx		; si = Sequence State.seq address
	mov word cx, [si]	; cx = Sequency Index
	;mov word [di], ax	; State.pat = Index of the Pattern in the Sequence

	mov word si, ax		; si = Sequency #0 address
	add word si, dx		; si = Pattern #0 address
	cmp word cx, 0		; zf = (cx == 0)
	jz %%done
%%loop:
	mov word ax, [si]	; Read and Auto-Inc
	; *** expecting si+2
		add si, 2
	add word si, ax		; Add Section Size (moving us to the next section)

	dec word cx			; Decrement counter
	jnz %%loop			; loop until we reach the section
%%done:
	mov word ax, si
	mov word [di], ax	; State.pat = Pattern CX's address
%endmacro
; ----------------------------------------------------------------------------------------------- ;
; @param %1 DEST: state base address
; @param %2 SRC: address
%macro SONG_DECODE 2
	mov word bx, %2
	mov word di, %1
	mov word [%1+0], bx				; State.data
	mov word [%1+2], 0				; State.seq
	mov word [%1+4], 0				; State.pat (dummy)
	mov byte [%1+6], 0				; State.pos
	mov byte [%1+7], 00000001b		; State.flags
%endmacro


; ----------------------------------------------------------------------------------------------- ;
; Never called directly, but ticked many times per second for music playback
audio_interrupt:
	push ax

	;SPEAKER_TOGGLE

	pop ax
	iret

; ----------------------------------------------------------------------------------------------- ;
STRUC State
.data:	resw 1			; Data Offset
.seq:	resw 1			; Sequence
.pat	resw 1			; Pattern Offset (store the offset rather than the index)
.pos:	resb 1			; Position (max pattern size is thusly 256)
.flags:	resb 1			; Flags
.size:					; Should be 8 bytes
ENDSTRUC
; ----------------------------------------------------------------------------------------------- ;
state:
; ----------------------------------------------------------------------------------------------- ;
; Global Music Object
state0:
ISTRUC State
	AT State.data,	dw 0
	AT State.seq,	dw 0
	AT State.pat,	dw 0
	AT State.pos,	db 0
	AT State.flags,	db 0
IEND
; ----------------------------------------------------------------------------------------------- ;
; Sound Effect 1
state1:
ISTRUC State
	AT State.data,	dw 0
	AT State.seq,	dw 0
	AT State.pat,	dw 0
	AT State.pos,	db 0
	AT State.flags,	db 0
IEND
; ----------------------------------------------------------------------------------------------- ;
; Sound Effect 2
state2:
ISTRUC State
	AT State.data,	dw 0
	AT State.seq,	dw 0
	AT State.pat,	dw 0
	AT State.pos,	db 0
	AT State.flags,	db 0
IEND
; ----------------------------------------------------------------------------------------------- ;

; ----------------------------------------------------------------------------------------------- ;
_currentStep:		; Sub-steps of a Pattern
	dw 0
; ----------------------------------------------------------------------------------------------- ;
_currentBPM:
	dw 0
; ----------------------------------------------------------------------------------------------- ;



; ----------------------------------------------------------------------------------------------- ;
; @param ax address of song to play
audio_playMusic:
	; store DS, ES
	push ds
	push es

	SONG_DECODE state0, ax
	SONG_DECODE_PAT state0

	; restore DS, ES and return
	pop es
	pop ds
	retf

audio_stopMusic:
	; store DS, ES
	push ds
	push es

	; restore DS, ES and return
	pop es
	pop ds
	retf

audio_pauseMusic:
	; store DS, ES
	push ds
	push es

	; restore DS, ES and return
	pop es
	pop ds
	retf

audio_resumeMusic:
	; store DS, ES
	push ds
	push es

	; restore DS, ES and return
	pop es
	pop ds
	retf

; ----------------------------------------------------------------------------------------------- ;
audio_playSound:
	retf
audio_stopSound:
	retf
audio_pauseSound:
	retf
audio_resumeSound:
	retf



; ----------------------------------------------------------------------------------------------- ;
; Initialized Data
; ----------------------------------------------------------------------------------------------- ;
section .data
; ----------------------------------------------------------------------------------------------- ;
; Placeholder song for when no song is currently set -
empty_song:
	; HEADER SECTION
	dw 2		; SECTION SIZE
	dw 120		; BPM
	dw 0		; Loop Point (or FFFFh if oneshot)

	; ORDER SECTION
	dw 2		; SECTION SIZE
	; Data
	dw 0

	; PATTERN SECTION
	dw 10		; SECTION SIZE
	dw 6		; Height
	db 1		; Width
	db 1		; Flags
	; Data
	db 0, 0
	db 24h, 7fh
	db 28h, 7fh

; ----------------------------------------------------------------------------------------------- ;
; Uninitialized Data (i.e. does not take up space in the binary)
; ----------------------------------------------------------------------------------------------- ;
section .bss
; ----------------------------------------------------------------------------------------------- ;
old_interrupt:
	resb 4
; ----------------------------------------------------------------------------------------------- ;
