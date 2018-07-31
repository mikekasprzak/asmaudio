; ----------------------------------------------------------------------------------------------- ;
org 0h							; org 0 because we are not a program and we only care about ourselves
[BITS 16]
; ----------------------------------------------------------------------------------------------- ;
section .text
; ----------------------------------------------------------------------------------------------- ;
; Jump Table - begins at "origin+4"
jump_table:
	; function 0				; interrupt
	jmp word audio_interrupt
	retf
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

; Set the frequency of the timer interrupt function
; @param ax Frequency (1193182/18.2 = 65536)
%macro SPEAKER_PIT0_FREQ 0
	out 40h, al					; write low to PIT counter 0's port
	mov al, ah					; move ah to low
	out 40h, al					; write low (high) to same port
%endmacro

%macro TANDY_INIT 0
    in      al, 61h
    or      al, 60h
    out     61h, al
%endmacro

; @param ax Frequency
; @param cl Channel (0-3)
; @param ch Volume (0-15, where 0 is loudest, 15 is off)
%macro TANDY_OUT 0
	; prepare the channel select bits
	shl cl, 5

	; chop up the 10-bit freqency bits in to a 4-bit and a 6-bit part
	mov dx, ax
	and al, 0Fh
	or al, 10000000b
	or al, cl
	shr dx, 4
	mov ah, dl
	and ah, 03Fh

	; set the frequency
	out 0c0h, al
	mov al, ah
	out 0c0h, al

	; set the volume
	mov al, 10010000b
	or al, cl
	or al, ch
	out 0c0h, al
%endmacro


%macro TANDY_OFF 0
	mov al, 10011111b
	out 0c0h, al
	mov al, 10111111b
	out 0c0h, al
	mov al, 11011111b
	out 0c0h, al
	mov al, 11111111b
	out 0c0h, al
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
PLAYER_CHANNEL_MAX				equ	4
PLAYER_TICKS_PER_LINE			equ 6
PLAYER_NOTE_OFF					equ 07Fh
;FREQ							3579000/32
;FREQ							111844
; ----------------------------------------------------------------------------------------------- ;
STRUC SongHeader
.chunk:			resw 1
.beats:			resw 1
.data:
ENDSTRUC

STRUC SongSequence
.chunk:			resw 1
.data:
ENDSTRUC

STRUC SongPattern
.chunk:			resw 1
.heightWidth:	resw 1
.data:
ENDSTRUC
; ----------------------------------------------------------------------------------------------- ;
STRUC PlayerState
.bpm:			resw 1			; Beats per Minute
.lpb:			resb 1			; Lines per Beat
.tpl:			resb 1			; Ticks per Line
;.tps:			resw 1			; Ticks per Second (i.e. BPM*LPB*TPL/60)
.maxChannels:	resb 1			; How many Channels to decode
.padding:   	resb 1
; 6
.dataAddr:		resw 1			; Data Address
.seqAddr:		resw 1			; Sequence Address
.seqLength:		resw 1			; Sequence Length
.seqPos:		resw 1			; * Sequence Position
.patStartAddr:	resw 1			; Pattern Start Address
.patAddr:		resw 1			; * Pattern Address
.patLength:		resw 1			; * Pattern Length
.patPos:		resw 1			; * Pattern Position (Line)
.tick:			resb 1			; * Tick (Line sub-samples)
.channels:		resb 1			; * Total number of channels in track
.channelLimit:	resb 1			; * Total number of channels to decode, limited by the channel max
.channelWidth:	resb 1			; * Width of a channel (bytes)
.bytesPerLine:	resb 1			; * Total number of bytes per line
.padding2:		resb 1
.size:
ENDSTRUC

STRUC PlayerChannel
.note:			resb 1			; * Note
.instr:			resb 1			; * Instrument
.vol:			resb 1			; * Volume
;.hold:			resb 1			; * Hold (how long since the note was last changed)
;FX go here
.size:
ENDSTRUC
; ----------------------------------------------------------------------------------------------- ;

; ----------------------------------------------------------------------------------------------- ;
; @param %1 DEST: state base address
; @param %2 SRC: address
%macro SONG_DECODE 2
	mov word di, %1												; PlayerState
	mov word si, %2												; SongHeader

	; Decode the Beats section
	mov word dx, [si+SongHeader.beats]
	mov word ax, dx
	and word ax, 0FFFh
	mov word bx, dx
	shr word bx, 12
	inc word bx
	mov word [di+PlayerState.bpm], ax							; Beats per Minute
	mov byte [di+PlayerState.lpb], bl							; Lines per Beat
	mov byte [di+PlayerState.tpl], PLAYER_TICKS_PER_LINE		; Ticks per Line
	mov byte [di+PlayerState.maxChannels], PLAYER_CHANNEL_MAX	; Maximum Channels (implementation limit)

	; Store the Song in the Data Address
	mov word [di+PlayerState.dataAddr], si

	; Next chunk is the Sequence
	mov word cx, [si+SongHeader.chunk]							; previous section's chunk size
	add si, cx													; next chunk (Sequence)
	mov word cx, [si+SongSequence.chunk]						; cx: Sequence section's chunk size

	mov word [di+PlayerState.seqAddr], si						; Sequence Address
	mov word ax, cx
	shr word ax, 1
	dec ax
	mov word [di+PlayerState.seqLength], ax						; Sequence Length = ((chunksize >> 1) - 1)
	mov word [di+PlayerState.seqPos], 0							; Sequence Position

	mov word dx, [si+SongSequence.data]							; dx: Sequence[0]

	; Next chunk (and every chunk herein) are Patterns
	add si, cx													; next chunk (Pattern 0)

	; Store the 1st Pattern's address
	mov word [di+PlayerState.patStartAddr], si

	; check if we're already at the beginning
	cmp dx, 0
	jz %%done

	; loop until we reach the desired chunk
%%loop:
	mov word cx, [si+SongPattern.chunk]
	add si, cx
	dec dx
	jnz %%loop
%%done:
	mov word [di+PlayerState.patAddr], si		; Pattern Address

	; Decode the Height+Width section
	mov word bx, [si+SongPattern.heightWidth]	; bx: Height+Width... i.e. Height+Channels+ChannelWidth
	mov word ax, bx
	and word ax, 03FFh
	inc word ax
	mov word [di+PlayerState.patLength], ax		; Height (12 bits)
	mov word [di+PlayerState.patPos], 0			; Position (start at zero)
	mov byte [di+PlayerState.tick], 6			; Tick (start at TickPerLine)
	mov word ax, bx
	shr byte ah, 2								; Instead of shifting by 10, shift the high-bit by 2 and use it
	and byte ah, 0Fh
	inc byte ah
	mov byte [di+PlayerState.channels], ah		; Channels (4 bit) = channels + 1, i.e. max 16 (min 1)
	mov byte dh, ah
	cmp byte ah, PLAYER_CHANNEL_MAX				; 2 = Max Channels
	jle %%limit
	mov byte ah, PLAYER_CHANNEL_MAX
%%limit:
	mov byte [di+PlayerState.channelLimit], ah	; The pre-clamped channel limit
	mov byte ah, bh
	shr byte ah, 6
	inc byte ah
	mov byte [di+PlayerState.channelWidth], ah	; Width of each channel = channelWidth + 1, i.e. max 4 (min 1)
	mov al, ah
	mul dh										; ax = al * dh
	mov byte [di+PlayerState.bytesPerLine], al	; Number of bytes per line
%endmacro

; ----------------------------------------------------------------------------------------------- ;
; @param %1 DEST: state base address
%macro SONG_CHANNEL_RESET 1
	mov word di, %1
	mov byte [di+PlayerChannel.note], PLAYER_NOTE_OFF
	mov byte [di+PlayerChannel.instr], 0
	mov byte [di+PlayerChannel.vol], 0
;	mov byte [di+PlayerChannel.hold], 0
%endmacro

; ----------------------------------------------------------------------------------------------- ;
; @param %1 SRC: state base address
%macro SONG_STEP 1
	mov word si, %1								; PlayerState
	mov word di, %1								; PlayerState

	; Step the tick
	dec byte [di+PlayerState.tick]
	; jump if we haven't exhausted our ticks
	jnz %%step_done

	; TICK EXHAUSTED, STEP THE PATTERN (LINE)
%%step_pattern:
	; Since tick has finished, reset the tick
	mov byte al, [si+PlayerState.tpl]
	mov byte [di+PlayerState.tick], al

	; Step the pattern position (line)
	inc word [di+PlayerState.patPos]
	; jump to decode if we haven't exhausted the pattern
	mov ax, [si+PlayerState.patLength]
	cmp word [di+PlayerState.patPos], ax
	jnz %%step_done

	; PATTERN EXHAUSTED, STEP THE SEQUENCE
%%step_sequence:
	; reset pattern position to top
	mov word [di+PlayerState.patPos], 0

	; Step the sequence
	inc word [di+PlayerState.seqPos]

	; jump to decode if we haven't exhausted the sequence
	mov ax, [si+PlayerState.seqLength]
	cmp word [di+PlayerState.seqPos], ax
	jnz %%decode_sequence

	; SEQUENCE EXHAUSTED, LOOP THE SONG
%%loop_sequence:
	; loop the song... for now go back to zero
	mov word [di+PlayerState.seqPos], 0

	; DECODE THE SEQUENCE
%%decode_sequence:
	mov bx, [si+PlayerState.seqPos]
	inc bx
	shl bx, 1
	add bx, [si+PlayerState.seqAddr]
	mov dx, [bx]								; dx: Pattern Number = seqAddr + (seqPos << 1)

	; IMPORTANT: si has changed to SongPattern
	mov si, [si+PlayerState.patStartAddr]		; si: Pattern Start Address

	; check if we're already at the beginning
	cmp dx, 0
	jz %%decode_sequence_done					; jz is a synonym for je

	; loop until we reach the desired chunk
%%decode_sequence_loop:
	mov word cx, [si+SongPattern.chunk]
	add si, cx
	dec dx
	jnz %%decode_sequence_loop
%%decode_sequence_done:
	mov word [di+PlayerState.patAddr], si

	; Decode the Height+Width section
	mov word bx, [si+SongPattern.heightWidth]	; bx: Height+Width... i.e. Height+Channels+ChannelWidth
	mov word ax, bx
	and word ax, 03FFh
	inc word ax
	mov word [di+PlayerState.patLength], ax		; Height (12 bits)
	;mov word [di+PlayerState.patPos], 0		; Position (start at zero) --------- REDUNDANT
	;mov byte [di+PlayerState.tick], 6			; Tick (start at TickPerLine) ------ REDUNDANT
	mov word ax, bx
	shr byte ah, 2								; Instead of shifting by 10, shift the high-bit by 2 and use it
	and byte ah, 0Fh
	inc byte ah
	mov byte [di+PlayerState.channels], ah		; Channels (4 bit) = channels + 1, i.e. max 16 (min 1)
	mov byte dh, ah
	cmp byte ah, PLAYER_CHANNEL_MAX				; 2 = Max Channels
	jle %%limit
	mov byte ah, PLAYER_CHANNEL_MAX
%%limit:
	mov byte [di+PlayerState.channelLimit], ah	; The pre-clamped channel limit
	mov byte ah, bh
	shr byte ah, 6
	inc byte ah
	mov byte [di+PlayerState.channelWidth], ah	; Width of each channel = channelWidth + 1, i.e. max 4 (min 1)
	mov al, ah
	mul dh										; ax = al * dh
	mov byte [di+PlayerState.bytesPerLine], al	; Number of bytes per line
%%step_done:
%endmacro

; ----------------------------------------------------------------------------------------------- ;
%macro SONG_DECODE_PATTERN 1
	mov word si, %1								; PlayerState
	mov word di, %1+PlayerState.size			; PlayerState
;	add di, PlayerState.size					; di: address of 1st channel

	mov cl, [si+PlayerState.channelLimit]
	mov ch, [si+PlayerState.channelWidth]
	mov dh, 0
	mov dl, [si+PlayerState.bytesPerLine]
	mov ax, [si+PlayerState.patPos]
	mul dx										; dx:ax = ax * dx
	mov bx, ax
	add bx, [si+PlayerState.patAddr]
	add bx, SongPattern.data					; bx: address of line

%%fetch_note:
	mov dx, [bx]
	mov al, dl
	and al, 07Fh
	jz %%done									; if even after anding we get zero, then the note should not change
%%play_note:
	mov [di+PlayerChannel.note], al
%%next_channel:
	add di, PlayerChannel.size
	mov ah, 0
	mov al, ch
	add bx, ax
	dec cl
	jnz %%fetch_note
%%done:
%endmacro

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

	; Setup Roland Sound
	call set_uart
	
	; setup player
	SONG_DECODE player0, empty_song
	SONG_CHANNEL_RESET player0channel0
	SONG_CHANNEL_RESET player0channel1
	SONG_CHANNEL_RESET player0channel2
	SONG_CHANNEL_RESET player0channel3
	SONG_DECODE_PATTERN player0
	mov si, player0
	call roland_decode
	call setbpm

;	mov al, 080h | ((4444 >> 4) & 0Fh)
;	mov ah, (4444 >> 8) & 03Fh;
;	TANDY_OUT

;	mov al, 0A0h | ((3333 >> 4) & 0Fh)
;	mov ah, (3333 >> 8) & 03Fh;
;	TANDY_OUT2

	; switch to our custom timer interrupt
	cli
	mov ax, 0
	mov es, ax
	mov ax, audio_interrupt
	mov [es:(1Ch*4)+0], ax
	mov ax, cs
	mov [es:(1Ch*4)+2], ax
	sti

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

	; Set frequency back to default (0, aka 65536)
	mov ax, 0
	SPEAKER_PIT0_FREQ

	; turn off the sound
	;TANDY_OFF

	; restore the original timer interrupt
	cli
	mov ax, 0
	mov es, ax
	mov ax, [old_interrupt+0]
	mov [es:(1Ch*4)+0], ax
	mov ax, [old_interrupt+2]
	mov [es:(1Ch*4)+2], ax
	sti

	; restore DS, ES and return
	pop es
	pop ds
	retf

; ----------------------------------------------------------------------------------------------- ;
; @param ax address of song to play
audio_playMusic:
	; store DS
	push ds

	push ax
	mov ax, cs
	mov ds, ax
	pop ax

	cli
	SONG_DECODE player0, ax
	SONG_CHANNEL_RESET player0channel0
	SONG_CHANNEL_RESET player0channel1
	SONG_CHANNEL_RESET player0channel2
	SONG_CHANNEL_RESET player0channel3
	SONG_DECODE_PATTERN player0
	mov si, player0
	call roland_decode
	call setbpm
	sti

	; restore DS and return
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



; Play a sound
; @param si address of player
roland_decode:
	add si, PlayerState.size
	mov cl, 0
	call roland_decode_channel

	add si, PlayerChannel.size
	mov cl, 1
	call roland_decode_channel

;	add si, PlayerChannel.size
;	mov cl, 2
;	call roland_decode_channel

	ret

; Play a sound
; @param si address of player
roland_decode_one:
	add si, PlayerState.size
	mov cl, 0

roland_decode_channel:
	mov bl, [si]

	cmp bl, 07fh					; if note is a "note off", jump to note-off
	jz sd_note_off
	cmp bl, 12						; if note is below valid range, jump to note-off
	jl sd_note_off
	cmp bl, 12+(12*8)				; if note is above valid range, jump to note-off
	jge sd_note_off

sd_note_on:
;	xor bh, bh
;	sub bx, 12						; lower one octave
;	add bx, bx						; bx*2
;	add bx, note_table
;	mov ax, [bx]
;
;	mov ch, 0
;	TANDY_OUT

hee:
	call is_output
	jnz hee
	
	mov al, 10010000b
	call put_mpu_out

haa:
	call is_output
	jnz haa

	mov al, bl
	call put_mpu_out

hoo:
	call is_output
	jnz hoo

	mov al, 127
	call put_mpu_out

huu:
	call is_output
	jnz huu



	jmp sd_done
sd_note_off:
;	mov ax, 0
;	mov ch, 15
;	TANDY_OUT
sd_done:
	ret


setbpm:
	mov si, player0

;	mov ax, [si+PlayerState.bpm]
;	mov bx, [si+PlayerState.lpb]
;	mul bx
;	mov bx, [si+PlayerState.tpl]
;	mul bx
;	mov bx, 18+18+18+18+18+18+18+6;+18+18+18+18+18+18+18
;	mul bx

	mov ax, [si+PlayerState.bpm]
	mov bx, [si+PlayerState.lpb]
	mul bx
	mov bx, [si+PlayerState.tpl]
	mul bx
	mov ax, bx
	mov ax, 1193182/19
	div bx
	mov bx, 19+19+18;19*3;19+19+19
	mul bx

	SPEAKER_PIT0_FREQ

	ret

; ----------------------------------------------------------------------------------------------- ;
; Never called directly, but ticked many times per second for music playback
audio_interrupt:
	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push ds

	mov ax, cs
	mov ds, ax

	SONG_STEP player0
	cmp byte [di+PlayerState.tick], PLAYER_TICKS_PER_LINE
	jnz ai_done

	SONG_DECODE_PATTERN player0
ai_done:
	mov si, player0
	call roland_decode

	pop ds
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	iret
; ----------------------------------------------------------------------------------------------- ;

; ----------------------------------------------------------------------------------------------- ;
; Initialized Data
; ----------------------------------------------------------------------------------------------- ;
section .data
; ----------------------------------------------------------------------------------------------- ;
; Placeholder song for when no song is currently set -
empty_song:
	; HEADER SECTION
	dw 2+4										; SECTION SIZE
	dw (120<<0) | ((4-1)<<12)					; BeatsPerMinute [12], LinesPerBeat [4]
	dw 0										; Padding... could be whatever we want

	; ORDER SECTION
	dw 2+6										; SECTION SIZE
	; Data
	dw 0
	dw 0
	dw 0

	; PATTERN SECTION
	dw 2+2+6									; SECTION SIZE
	dw ((57-1)<<0) | ((1-1)<<10) | ((1-1)<<14)	; Height [10], Channels [4], Channel Width [2]
	; Data
	db 24h
	db 0
	db 25h
	db 0
	db 26h
	db 0
	db 27h
	db 0
	db 28h
	db 0
	db 29h
	db 0
	db 2Ah
	db 0
	db 2Bh
	db 0
	db 2Ch
	db 0
	db 2Dh
	db 0
	db 2Eh
	db 0
	db 2Fh
	db 0
	db 7fh
	db 0
	db 0
	db 0
	db 0
	db 24h
	db 7fh
	db 25h
	db 7fh
	db 26h
	db 7fh
	db 27h
	db 7fh
	db 28h
	db 7fh
	db 29h
	db 7fh
	db 2Ah
	db 7fh
	db 2Bh
	db 7fh
	db 2Ch
	db 7fh
	db 2Dh
	db 7fh
	db 2Eh
	db 7fh
	db 2Fh
	db 7fh
	db 0
	db 0
	db 0
	db 0

; ----------------------------------------------------------------------------------------------- ;
; Note lookup table: C1-B8 (i.e. 96 possible values) - 192 bytes
note_table:
	; 	C,			C#,			D,			D#,			E,			F,			F#,			G,			G#,			A,			A#,			B
	dw	111844/32,	111844/34,	111844/36,	111844/38,	111844/41,	111844/43,	111844/46,	111844/49,	111844/51,	111844/55,	111844/58,	111844/61;
	dw	111844/65,	111844/69,	111844/73,	111844/77,	111844/82,	111844/87,	111844/92,	111844/98,	111844/103,	111844/110,	111844/116,	111844/123;
	dw	111844/130,	111844/138,	111844/146,	111844/155,	111844/164,	111844/174,	111844/185,	111844/196,	111844/207,	111844/220,	111844/233,	111844/246;
	dw	111844/261,	111844/277,	111844/293,	111844/311,	111844/329,	111844/349,	111844/370,	111844/392,	111844/415,	111844/440,	111844/466,	111844/493;
	dw	111844/523,	111844/554,	111844/587,	111844/622,	111844/659,	111844/698,	111844/740,	111844/784,	111844/830,	111844/880,	111844/932,	111844/987;
	dw	111844/1047,111844/1109,111844/1175,111844/1245,111844/1319,111844/1397,111844/1480,111844/1568,111844/1661,111844/1760,111844/1865,111844/1976;
	dw	111844/2093,111844/2217,111844/2349,111844/2489,111844/2637,111844/2794,111844/2960,111844/3136,111844/3322,111844/3520,111844/3729,111844/3951;
	dw	111844/4186,111844/4435,111844/4699,111844/4978,111844/5274,111844/5588,111844/5920,111844/6272,111844/6645,111844/7040,111844/7459,111844/7902;


; ----------------------------------------------------------------------------------------------- ;
; Uninitialized Data (i.e. does not take up space in the binary)
; ----------------------------------------------------------------------------------------------- ;
section .bss
; ----------------------------------------------------------------------------------------------- ;
old_interrupt:
	resb 4
; ----------------------------------------------------------------------------------------------- ;
player0:
	resb PlayerState.size
player0channel:
player0channel0:
	resb PlayerChannel.size
player0channel1:
	resb PlayerChannel.size
player0channel2:
	resb PlayerChannel.size
player0channel3:
	resb PlayerChannel.size
; ----------------------------------------------------------------------------------------------- ;
player1:
	resb PlayerState.size
player1channel:
player1channel0:
	resb PlayerChannel.size
player1channel1:
	resb PlayerChannel.size
player1channel2:
	resb PlayerChannel.size
player1channel3:
	resb PlayerChannel.size
; ----------------------------------------------------------------------------------------------- ;
player2:
	resb PlayerState.size
player2channel:
player2channel0:
	resb PlayerChannel.size
player2channel1:
	resb PlayerChannel.size
player2channel2:
	resb PlayerChannel.size
player2channel3:
	resb PlayerChannel.size
; ----------------------------------------------------------------------------------------------- ;
