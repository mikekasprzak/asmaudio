; ----------------------------------------------------------------------------------------------- ;
org 0h							; org 0 because we are not a program and we only care about ourselves
[BITS 16]
; ----------------------------------------------------------------------------------------------- ;
section .text
; ----------------------------------------------------------------------------------------------- ;
; Jump Table - begins at "origin+4"
jump_table:
	; function 0
	ret							; just returns (which makes running us as a .com file do nothing)
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


; ----------------------------------------------------------------------------------------------- ;
PLAYER_CHANNEL_MAX				equ	2
PLAYER_TICKS_PER_LINE			equ 6
PLAYER_NOTE_OFF					equ 07Fh
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
.size:
ENDSTRUC

STRUC PlayerChannel
.note:			resb 1			; * Note
.instr:			resb 1			; * Instrument
.vol:			resb 1			; * Volume
.hold:			resb 1			; * Hold (how long since the note was last changed)
;FX go here
.size:
ENDSTRUC
; ----------------------------------------------------------------------------------------------- ;
; @param %1 State address
;%macro SONG_DECODE_PAT 1
;	mov word si, %1 + State.data
;
;	mov word ax, [si]	; ax = State.data address. auto-inc, si = State.seq
;	mov word bx, [si+2]	; bx = State.seq index. auto-inc, si = State.pat
;	mov word di, si		; di = State.pat address
;	mov word si, ax		; si = State.data address
;	mov word ax, [si]	; ax = sizeof the Header section
;	add word si, ax		; si = Sequence section
;	mov word dx, [si]	; dx = sizeof the Sequence section (in bytes). auto-inc, si = Sequence #0 address
;	; *** expecting si+2
;		add si, 2
;	mov word ax, si		; ax = Sequence #0 address
;	shl word bx, 1		; bx = State.seq * 2
;	add word si, bx		; si = Sequence State.seq address
;	mov word cx, [si]	; cx = Sequency Index
;	;mov word [di], ax	; State.pat = Index of the Pattern in the Sequence
;
;	mov word si, ax		; si = Sequency #0 address
;	add word si, dx		; si = Pattern #0 address
;	cmp word cx, 0		; zf = (cx == 0)
;	jz %%done
;%%loop:
;	mov word ax, [si]	; Read and Auto-Inc
;	; *** expecting si+2
;		add si, 2
;	add word si, ax		; Add Section Size (moving us to the next section)
;
;	dec word cx			; Decrement counter
;	jnz %%loop			; loop until we reach the section
;%%done:
;	mov word ax, si
;	mov word [di], ax	; State.pat = Pattern CX's address
;%endmacro
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
	mov word [di+PlayerState.tick], 6			; Tick (start at TickPerLine)
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
	mul dh
	mov byte [di+PlayerState.bytesPerLine], al	; Number of bytes per line
%endmacro


; @param %1 DEST: state base address
%macro SONG_CHANNEL_RESET 1
	mov word di, %1
	mov byte [di+PlayerChannel.note], PLAYER_NOTE_OFF
	mov byte [di+PlayerChannel.instr], 0
	mov byte [di+PlayerChannel.vol], 0
	mov byte [di+PlayerChannel.hold], 0
%endmacro

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
	shl bx, 1
	add bx, [si+PlayerState.seqAddr]
	mov dx, [bx]								; dx: Pattern Number = seqAddr + (seqPos << 1)

	mov si, [si+PlayerState.patStartAddr]		; si: Pattern Start Address

	; check if we're already at the beginning
	cmp dx, 0
	jz %%decode_sequence_done

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
	mov word [di+PlayerState.patPos], 0			; Position (start at zero)
	mov word [di+PlayerState.tick], 6			; Tick (start at TickPerLine)
	mov word ax, bx
	shr byte ah, 2								; Instead of shifting by 10, shift the high-bit by 2 and use it
	and byte ah, 0Fh
	inc byte ah
	mov byte [di+PlayerState.channels], ah		; Channels (4 bit) = channels + 1, i.e. max 16 (min 1)
	mov byte dh, ah
	cmp byte ah, 2								; 2 = Max Channels
	jle %%limit
	mov byte ah, 2
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




;	; DECODE THE PATTERN
;%%decode_pattern:;
;
;
;
;	; Limit the number of channels we decode to the maxChannels
;%%clamp_channels:
;	mov ah, [si+PlayerState.channels]
;	cmp [di+PlayerState.maxChannels], ah
;	jle %%clamp_channels_next
;	mov ah, [si+PlayerState.maxChannels]
;%%clamp_channels_next:
;	mov al, [si+PlayerState.channelWidth]
;	mul ah								; ax = al * %1
;	mov bl, al							; bl: channels to render
;	mov bh, [si+PlayerState.channels]	; bh: number of channels per line
;
;	mov al, PlayerChannel.size
;	mul bh
;	mov cl, al							; cl: bytes per line
;	mov ch, 0
;
;	mov ax, [si+PlayerState.patPos]
;	mul cx								; dx:ax = ax * %1
;	mov dx, ax							; dx: byte offset to current line
;
;	;mov ch, PlayerChannel.size			; ch: bytes per channel
;
;	; Write to channels!
;	mov si, [si+PlayerState.patAddr]
;	add si, dx
;	mov di, %1+PlayerState.size
;
;;	mov al, [si+0]
;
;
;%%step_almost_done:
;;	mov di, %1+PlayState.size

%macro SONG_DECODE_PATTERN 1
	mov word si, %1								; PlayerState
	mov word di, %1								; PlayerState

	mov cl, [si+PlayerState.channelLimit]
	mov ch, [si+PlayerState.channelWidth]
	mov dh, 0
	mov dl, [si+PlayerState.bytesPerLine]
	mov ax, [si+PlayerState.patPos]
	mul dx										; dx:ax = ax * dx
	mov bx, ax
	add bx, [si+PlayerState.patAddr]			
	add bx, SongPattern.data					; bx: address of line

	add di, PlayerState.size					; di: address of 1st channel

	mov dx, [bx]
	mov al, dl
	and al, 07Fh
	jz %%done
%%play_note:
	mov byte [di+PlayerChannel.note], al
%%next_channel:
	add di, PlayerChannel.size
	mov ah, 0
	mov al, ch
	add bx, ax
	dec cl
	jnz %%play_note
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

	nop
	nop

	; setup player
	SONG_DECODE player0, empty_song
	SONG_CHANNEL_RESET player0channel0
	SONG_CHANNEL_RESET player0channel1
	SONG_DECODE_PATTERN player0

	nop
	nop

bakk:	
	SONG_STEP player0
	nop
	nop
	SONG_DECODE_PATTERN player0
	jmp bakk
	
	nop
	nop

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
	SPEAKER_PIT2_INIT

	mov ax, 4444
	SPEAKER_PIT2_FREQ
	SPEAKER_ON

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
	mov ax, 0
	SPEAKER_PIT0_FREQ

	; turn off the speaker
	SPEAKER_OFF

	; restore DS, ES and return
	pop es
	pop ds
	retf

; ----------------------------------------------------------------------------------------------- ;
; @param ax address of song to play
audio_playMusic:
	; store DS, ES
	push ds
	push es

	SONG_DECODE player0, ax
	SONG_CHANNEL_RESET player0channel0
	SONG_CHANNEL_RESET player0channel1

	; Set PIT frequency to 1MHz / BPM*LPB*TPL
	;mov ax, 0
	;SPEAKER_PIT0_FREQ

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

;	SONG_STEP player0

	;SPEAKER_TOGGLE

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
	dw 2+2										; SECTION SIZE
	; Data
	dw 0

	; PATTERN SECTION
	dw 2+2+6									; SECTION SIZE
	dw ((6-1)<<0) | ((1-1)<<10) | ((1-1)<<14)	; Height [10], Channels [4], Channel Width [2]
	; Data
	db 24h
	db 7fh
	db 0
	db 28h
	db 7fh
	db 0

; ----------------------------------------------------------------------------------------------- ;
; Note lookup table: C1-B8 (i.e. 96 possible values) - 192 bytes
note_table:
	; 	C,				C#,				D,				D#,				E,				F,				F#,				G,				G#,				A,				A#,				B
	dw	1193182/32,		1193182/34,		1193182/36,		1193182/38,		1193182/41,		1193182/43,		1193182/46,		1193182/49,		1193182/51,		1193182/55,		1193182/58,		1193182/61;
	dw	1193182/65,		1193182/69,		1193182/73,		1193182/77,		1193182/82,		1193182/87,		1193182/92,		1193182/98,		1193182/103,	1193182/110,	1193182/116,	1193182/123;
	dw	1193182/130,	1193182/138,	1193182/146,	1193182/155,	1193182/164,	1193182/174,	1193182/185,	1193182/196,	1193182/207,	1193182/220,	1193182/233,	1193182/246;
	dw	1193182/261,	1193182/277,	1193182/293,	1193182/311,	1193182/329,	1193182/349,	1193182/370,	1193182/392,	1193182/415,	1193182/440,	1193182/466,	1193182/493;
	dw	1193182/523,	1193182/554,	1193182/587,	1193182/622,	1193182/659,	1193182/698,	1193182/740,	1193182/784,	1193182/830,	1193182/880,	1193182/932,	1193182/987;
	dw	1193182/1047,	1193182/1109,	1193182/1175,	1193182/1245,	1193182/1319,	1193182/1397,	1193182/1480,	1193182/1568,	1193182/1661,	1193182/1760,	1193182/1865,	1193182/1976;
	dw	1193182/2093,	1193182/2217,	1193182/2349,	1193182/2489,	1193182/2637,	1193182/2794,	1193182/2960,	1193182/3136,	1193182/3322,	1193182/3520,	1193182/3729,	1193182/3951;
	dw	1193182/4186,	1193182/4435,	1193182/4699,	1193182/4978,	1193182/5274,	1193182/5588,	1193182/5920,	1193182/6272,	1193182/6645,	1193182/7040,	1193182/7459,	1193182/7902;


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
; ----------------------------------------------------------------------------------------------- ;
player1:
	resb PlayerState.size
player1channel:
player1channel0:
	resb PlayerChannel.size
player1channel1:
	resb PlayerChannel.size
; ----------------------------------------------------------------------------------------------- ;
player2:
	resb PlayerState.size
player2channel:
player2channel0:
	resb PlayerChannel.size
player2channel1:
	resb PlayerChannel.size
; ----------------------------------------------------------------------------------------------- ;
