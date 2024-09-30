BYPASS   = 0    ; bypass the license screen
WIPE_RAM = 1    ; wipes unused portion of FDS RAM at startup

; setting the file count 1 higher than files on disk for the license "bypass" technique
FILE_COUNT = 5 + BYPASS

.segment "HEADER"
.byte 'F','D','S',$1A
.byte 1 ; side count

.segment "SIDE1A"
; block 1
.byte $01
.byte "*NINTENDO-HVC*"
.byte $00 ; manufacturer
.byte "TST"
.byte $20 ; normal disk
.byte $00 ; game version
.byte $00 ; side
.byte $00 ; disk
.byte $00 ; disk type
.byte $00 ; unknown
.byte FILE_COUNT ; boot file count
.byte $FF,$FF,$FF,$FF,$FF
.byte $06 ; yy
.byte $09 ; mm
.byte $24 ; dd
.byte $49 ; country
.byte $61, $00, $00, $02, $00, $00, $00, $00, $00 ; unknown
.byte $06 ; yy
.byte $09 ; mm
.byte $24 ; dd
.byte $00, $80 ; unknown
.byte $FF, $FF ; disk writer serial number
.byte $07 ; unknown
.byte $00 ; disk write count
.byte $00 ; actual disk side
.byte $00 ; unknown
.byte $00 ; price
; block 2
.byte $02
.byte FILE_COUNT

.segment "FILE0_HDR"
; block 3
.import __FILE0_DAT_RUN__
.import __FILE0_DAT_SIZE__
.byte $03
.byte 0,0
.byte "FILE0..."
.word __FILE0_DAT_RUN__
.word __FILE0_DAT_SIZE__
.byte 0 ; PRG
; block 4
.byte $04
;.segment "FILE0_DAT"
;.incbin "" ; this is code below

.segment "FILE1_HDR"
; block 3
.import __FILE1_DAT_RUN__
.import __FILE1_DAT_SIZE__
.byte $03
.byte 1,1
.byte "FILE1..."
.word __FILE1_DAT_RUN__
.word __FILE1_DAT_SIZE__
.byte 0 ; PRG
; block 4
.byte $04
;.segment "FILE1_DAT"
;.incbin "" ; this is code below

.segment "FILE2_HDR"
; block 3
.import __FILE2_DAT_SIZE__
.import __FILE2_DAT_RUN__
.byte $03
.byte 2,2
.byte "FILE2..."
.word __FILE2_DAT_RUN__
.word __FILE2_DAT_SIZE__
.byte 1 ; CHR
; block 4
.byte $04
.segment "FILE2_DAT"
.incbin "background.chr"

.segment "FILE3_HDR"
; block 3
.import __FILE3_DAT_SIZE__
.import __FILE3_DAT_RUN__
.byte $03
.byte 3,3
.byte "FILE3..."
.word __FILE3_DAT_RUN__
.word __FILE3_DAT_SIZE__
.byte 1 ; CHR
; block 4
.byte $04
.segment "FILE3_DAT"
.incbin "sprite.chr"

.if (BYPASS <> 0)
; This block is the last to load, and enables NMI by "loading" the NMI enable value
; directly into the PPU control register at $2000.
; While the disk loader continues searching for one more boot file,
; eventually an NMI fires, allowing us to take control of the CPU before the
; license screen is displayed.
.segment "FILE4_HDR"
; block 3
.import __FILE4_DAT_SIZE__
.import __FILE4_DAT_RUN__
.byte $03
.byte 4,4
.byte "FILE4..."
.word $2000
.word __FILE4_DAT_SIZE__
.byte 0 ; PRG (CPU:$2000)
; block 4
.byte $04
.segment "FILE4_DAT"
.byte $90 ; enable NMI byte sent to $2000

.else
; Alternative to the license screen bypass, just put the required copyright message at PPU:$2800.
.segment "FILE4_HDR"
; block 3
.import __FILE4_DAT_SIZE__
.import __FILE4_DAT_RUN__
.byte $03
.byte 4,4
.byte "KYODAKU-"
.word $2800
.word __FILE4_DAT_SIZE__
.byte 2 ; nametable (PPU:$2800)
; block 4
.byte $04
.segment "FILE4_DAT"
.incbin "check.bin"
.endif

.segment "FILE5_HDR"
; block 3
.import __FILE5_DAT_SIZE__
.import __FILE5_DAT_RUN__
.byte $03
.byte 5,5
.byte "SAV....."
.word $2001
.word $1000
.byte 0 ; PRG (CPU:$2000)
; block 4
.byte $04
.segment "FILE5_DAT"
.byte $90 ; enable NMI byte sent to $2000

;
; FDS vectors
;

.segment "FILE1_DAT"
.word nmi
.word nmi

.if (BYPASS <> 0)
	.word bypass
.else
	.word nmi
.endif

.word reset
.word irq

;
; reset routine
;

.segment "FILE0_DAT"

.if (BYPASS <> 0)
; this routine is entered by interrupting the last boot file load
; by forcing an NMI not expected by the BIOS, allowing the license
; screen to be skipped entirely.
;
; The last file writes $90 to $2000, enabling NMI during the file load.
; The "extra" file in the FILE_COUNT causes the disk to keep seeking
; past the last file, giving enough delay for an NMI to fire and interrupt
; the process.
bypass:
	; disable NMI
	lda #0
	sta $2000
	; replace NMI 3 "bypass" vector at $DFFA
	lda #<nmi
	sta $DFFA
	lda #>nmi
	sta $DFFB
	; tell the FDS reset routine that the BIOS initialized correctly
	lda #$35
	sta $0102
	lda #$AC
	sta $0103
	; reset the FDS to begin our program properly
	jmp ($FFFC)
.endif

reset:
	; set FDS to use vertical mirroring
	lda $FA
	and #%11110111
	sta $4025
	;
	sei       ; mask interrupts
	lda #0
	sta $2000 ; disable NMI
	sta $2001 ; disable rendering
	sta $4015 ; disable APU sound
	sta $4010 ; disable DMC IRQ
	lda #$40
	sta $4017 ; disable APU IRQ
	cld       ; disable decimal mode
	ldx #$FF
	txs       ; initialize stack
	; wait for first vblank
	bit $2002
	:
		bit $2002
		bpl :-
	; clear not-quite all RAM to 0
	lda #0
	tax
	:
		;sta $0000, X ; the FDS uses part of the stack and ZP,
		;sta $0100, X ; so only partially clearing them.
		sta $0200, X
		sta $0300, X
		sta $0400, X
		sta $0500, X
		sta $0600, X
		sta $0700, X
		inx
		bne :-
	;ldx #$00
	:
		sta $00, X
		inx
		cpx #$F9 ; $F9-FF used by FDS BIOS
		bcc :-
	ldx #$04 ; $0100-$0103 used by FDS BIOS
	:
		sta $100, X
		inx
		bne :-
	; place all sprites offscreen at Y=255
	lda #255
	;ldx #0
	:
		sta oam, X
		inx
		bne :-
	; wipe unused portion of FDS RAM (between FILE0 and FILE1)
	.if (WIPE_RAM <> 0)
		WIPE_ADDR = __FILE0_DAT_RUN__ + __FILE0_DAT_SIZE__
		WIPE_SIZE = __FILE1_DAT_RUN__ - WIPE_ADDR
		lda #<WIPE_ADDR
		sta $00
		lda #>WIPE_ADDR
		sta $01
		lda #0
		tay
		ldx #>WIPE_SIZE
		beq :++
		: ; 256 byte blocks
			sta ($00), Y
			iny
			bne :-
			inc $01
			dex
			bne :-
		: ; leftover
		ldy #<WIPE_SIZE
		beq :++
		:
			dey
			sta ($00), Y
			bne :-
		:
		sta $00
		sta $01
	.endif
	; wait for second vblank
	:
		bit $2002
		bpl :-
	; NES is initialized, ready to begin!
	; enable the NMI for graphical updates, and jump to our main program
	lda #%10001000
	sta $2000
	jmp main

;
; nmi routine
;

.segment "ZEROPAGE"
nmi_lock:       .res 1 ; prevents NMI re-entry
nmi_count:      .res 1 ; is incremented every NMI
nmi_ready:      .res 1 ; set to 1 to push a PPU frame update, 2 to turn rendering off next NMI
nmt_update_len: .res 1 ; number of bytes in nmt_update buffer
scroll_x:       .res 1 ; x scroll position
scroll_y:       .res 1 ; y scroll position
scroll_nmt:     .res 1 ; nametable select (0-3 = $2000,$2400,$2800,$2C00)
temp:           .res 1 ; temporary variable
FileHeader: .res 17

.segment "BSS"
nmt_update: .res 256 ; nametable update entry buffer for PPU update
palette:    .res 32  ; palette buffer for PPU update

.segment "OAM"
oam: .res 256        ; sprite OAM data to be uploaded by DMA

.segment "FILE0_DAT"
nmi:
	; save registers
	pha
	txa
	pha
	tya
	pha
	; prevent NMI re-entry
	lda nmi_lock
	beq :+
		jmp @nmi_end
	:
	lda #1
	sta nmi_lock
	; increment frame counter
	inc nmi_count
	;
	lda nmi_ready
	bne :+ ; nmi_ready == 0 not ready to update PPU
		jmp @ppu_update_end
	:
	cmp #2 ; nmi_ready == 2 turns rendering off
	bne :+
		lda #%00000000
		sta $2001
		ldx #0
		stx nmi_ready
		jmp @ppu_update_end
	:
	; sprite OAM DMA
	ldx #0
	stx $2003
	lda #>oam
	sta $4014
	; palettes
	lda #%10001000
	sta $2000 ; set horizontal nametable increment
	lda $2002
	lda #$3F
	sta $2006
	stx $2006 ; set PPU address to $3F00
	ldx #0
	:
		lda palette, X
		sta $2007
		inx
		cpx #32
		bcc :-
	; nametable update
	ldx #0
	cpx nmt_update_len
	bcs @scroll
	@nmt_update_loop:
		lda nmt_update, X
		sta $2006
		inx
		lda nmt_update, X
		sta $2006
		inx
		lda nmt_update, X
		sta $2007
		inx
		cpx nmt_update_len
		bcc @nmt_update_loop
	lda #0
	sta nmt_update_len
@scroll:
	lda scroll_nmt
	and #%00000011 ; keep only lowest 2 bits to prevent error
	ora #%10001000
	sta $2000
	lda scroll_x
	sta $2005
	lda scroll_y
	sta $2005
	; enable rendering
	lda #%00011110
	sta $2001
	; flag PPU update complete
	ldx #0
	stx nmi_ready
@ppu_update_end:
	; if this engine had music/sound, this would be a good place to play it
	; unlock re-entry flag
	lda #0
	sta nmi_lock
@nmi_end:
	; restore registers and return
	pla
	tay
	pla
	tax
	pla
	rti

;
; irq
;

.segment "FILE0_DAT"
irq:
	rti

;
; drawing utilities
;

.segment "FILE0_DAT"

; ppu_update: waits until next NMI, turns rendering on (if not already), uploads OAM, palette, and nametable update to PPU
ppu_update:
	lda #1
	sta nmi_ready
	:
		lda nmi_ready
		bne :-
	rts

; ppu_skip: waits until next NMI, does not update PPU
ppu_skip:
	lda nmi_count
	:
		cmp nmi_count
		beq :-
	rts

; ppu_off: waits until next NMI, turns rendering off (now safe to write PPU directly via $2007)
ppu_off:
	lda #2
	sta nmi_ready
	:
		lda nmi_ready
		bne :-
	rts

; ppu_address_tile: use with rendering off, sets memory address to tile at X/Y, ready for a $2007 write
;   Y =  0- 31 nametable $2000
;   Y = 32- 63 nametable $2400
;   Y = 64- 95 nametable $2800
;   Y = 96-127 nametable $2C00
ppu_address_tile:
	lda $2002 ; reset latch
	tya
	lsr
	lsr
	lsr
	ora #$20 ; high bits of Y + $20
	sta $2006
	tya
	asl
	asl
	asl
	asl
	asl
	sta temp
	txa
	ora temp
	sta $2006 ; low bits of Y + X
	rts

; ppu_update_tile: can be used with rendering on, sets the tile at X/Y to tile A next time you call ppu_update
ppu_update_tile:
	pha ; temporarily store A on stack
	txa
	pha ; temporarily store X on stack
	ldx nmt_update_len
	tya
	lsr
	lsr
	lsr
	ora #$20 ; high bits of Y + $20
	sta nmt_update, X
	inx
	tya
	asl
	asl
	asl
	asl
	asl
	sta temp
	pla ; recover X value (but put in A)
	ora temp
	sta nmt_update, X
	inx
	pla ; recover A value (tile)
	sta nmt_update, X
	inx
	stx nmt_update_len
	rts

; ppu_update_byte: like ppu_update_tile, but X/Y makes the high/low bytes of the PPU address to write
;    this may be useful for updating attribute tiles
ppu_update_byte:
	pha ; temporarily store A on stack
	tya
	pha ; temporarily store Y on stack
	ldy nmt_update_len
	txa
	sta nmt_update, Y
	iny
	pla ; recover Y value (but put in Y)
	sta nmt_update, Y
	iny
	pla ; recover A value (byte)
	sta nmt_update, Y
	iny
	sty nmt_update_len
	rts

;
; gamepad
;

PAD_A      = $01
PAD_B      = $02
PAD_SELECT = $04
PAD_START  = $08
PAD_U      = $10
PAD_D      = $20
PAD_L      = $40
PAD_R      = $80

.segment "ZEROPAGE"
gamepad: .res 1

.segment "FILE0_DAT"
; gamepad_poll: this reads the gamepad state into the variable labelled "gamepad"
;   This only reads the first gamepad, and also if DPCM samples are played they can
;   conflict with gamepad reading, which may give incorrect results.
gamepad_poll:
	; strobe the gamepad to latch current button state
	lda #1
	sta $4016
	lda #0
	sta $4016
	; read 8 bytes from the interface at $4016
	ldx #8
	:
		pha
		lda $4016
		; combine low two bits and store in carry bit
		and #%00000011
		cmp #%00000001
		pla
		; rotate carry into gamepad variable
		ror
		dex
		bne :-
	sta gamepad
	rts

;
; This is main code:
;

.segment "FILE0_DAT"
main:

	jsr as_clear_20191111		; This is my compiler's code

	jsr as_setaspal_20191111		; This is my compiler's code


	NUM1 = $000A
	NUM2 = $000B
	NUM3 = $000C
	NUM4 = $000D

DataLoad:
	jsr $E1B2			; VINTWait
    jsr $E1F8           ; Load Data
	.word DiskID
	.word LoadList 
	bne Error
    beq NoData          ; If data cannot find, variables will set 0		

    lda $4031			; If data found, set variables
    sta NUM1
    lda $4031
    sta NUM2
    lda $4031
    sta NUM3
    lda $4031
    sta NUM4
    jmp Disp

Error:	;Error Beep
	lda $4015
	ora #%00000001
	sta $4015
	lda #%10111111
	sta $4000
	lda #%00101011
	sta $4001
	lda #%00000100
	sta $4002
	lda #%11111011
	sta $4003
	jmp Error

DiskID:
	.byte $00 ; manufacturer
	.byte "TST "
	.byte $00 ; game version
	.byte $00 ; side
	.byte $00 ; disk
	.byte $00 ; disk type
	.byte $00 ; unknown

LoadList:
    .byte $05, $FF

NoData:
    lda #$00
    sta NUM1
    sta NUM2
    sta NUM3
    sta NUM4
	jmp Disp

Disp:			;Display variables
	sta $07FF
	lda NUM1
	sta as_point
	lda $07FF

	sta $07FF
	lda #$21
	sta $042E
	lda #$4a
	sta $042F
	jsr as_point_20191111
	lda $07FF

	sta $07FF
	lda NUM2
	sta as_point
	lda $07FF

	sta $07FF
	lda #$21
	sta $042E
	lda #$6a
	sta $042F
	jsr as_point_20191111
	lda $07FF

	sta $07FF
	lda NUM3
	sta as_point
	lda $07FF

	sta $07FF
	lda #$21
	sta $042E
	lda #$8a
	sta $042F
	jsr as_point_20191111
	lda $07FF

	sta $07FF
	lda NUM4
	sta as_point
	lda $07FF

	sta $07FF
	lda #$21
	sta $042E
	lda #$aa
	sta $042F
	jsr as_point_20191111
	lda $07FF

wait_seconds:		;Wait
    lda $2002 
    bpl wait_seconds 
    ldx #0
loop:
    lda $2002
    bpl loop 
    inx
    cpx #180
    bne loop


DataSave:
    inc NUM1
    lda NUM1
    sta $4024
    inc NUM2
    lda NUM2
    sta $4024
    inc NUM3
    lda NUM3
    sta $4024
    inc NUM4
    lda NUM4
    sta $4024

  	jsr $E1B2			;VINTWait
	lda #5				;FILE ID
    jsr $E239           ;LoadData
	.word DiskID
	.word FileHeader 
	bne Error2
    jmp end   

Error2:	;Error Beep
	lda $4015
	ora #%00000001
	sta $4015
	lda #%10111111
	sta $4000
	lda #%00101011
	sta $4001
	lda #%00000100
	sta $4002
	lda #%11111011
	sta $4003
	jmp Error2

end:
    jmp end

;End of my code


;no10
as_drawbg_20191111:

as_drawbg_20191111_a = $07FF
as_drawbg_20191111_x = $07FE
as_drawbg_20191111_y = $07FD

	sta as_drawbg_20191111_a
	stx as_drawbg_20191111_x
	sty as_drawbg_20191111_y

as_drawbg001_20191111:
	lda $2002
	bmi as_drawbg001_20191111 

as_drawbg002_20191111:
	lda $2002
	bpl as_drawbg002_20191111

	lda #%00001000 
	sta $2000
	lda #%00000110
	sta $2001

	lda #$20
	sta $2006
	lda #$00
	sta $2006

	ldx #0

as_drawbg003_20191111:

	lda $0435,x
	sta $2007
	txa
	cmp #255
	beq as_drawbg004_20191111
	inx
	jmp as_drawbg003_20191111

as_drawbg004_20191111:
	ldx #0
as_drawbg005_20191111:
	lda $0535,x
	sta $2007
	txa
	cmp #255
	beq as_drawbg006_20191111
	inx
	jmp as_drawbg005_20191111

as_drawbg006_20191111:
	ldx #0
as_drawbg007_20191111:
	lda $0635,x
	sta $2007
	txa
	cmp #255
	beq as_drawbg008_20191111
	inx
	jmp as_drawbg007_20191111

as_drawbg008_20191111:
	ldx #0
as_drawbg009_20191111:
	lda $0735,x
	sta $2007
	txa
	cmp #191
	beq as_drawbg010_20191111
	inx
	jmp as_drawbg009_20191111


as_drawbg010_20191111:

	lda #$20
	sta $2006
	lda #$00
	sta $2006

	lda #%00011110
	sta $2001

	lda as_drawbg_20191111_a
	ldx as_drawbg_20191111_x
	ldy as_drawbg_20191111_y

	rts

;no11
as_drawspr_20191111:

as_drawspr_20191111_a = $07FF

Sprite01_Y = $0300
Sprite01_T = $0301
Sprite01_S = $0302
Sprite01_X = $0303

Sprite02_Y = $0304
Sprite02_T = $0305
Sprite02_S = $0306
Sprite02_X = $0307

Sprite03_Y = $0308
Sprite03_T = $0309
Sprite03_S = $030A
Sprite03_X = $030B

Sprite04_Y = $030C
Sprite04_T = $030D
Sprite04_S = $030E
Sprite04_X = $030F

Sprite05_Y = $0310
Sprite05_T = $0311
Sprite05_S = $0312
Sprite05_X = $0313

Sprite06_Y = $0314
Sprite06_T = $0315
Sprite06_S = $0316
Sprite06_X = $0317

Sprite07_Y = $0318
Sprite07_T = $0319
Sprite07_S = $031A
Sprite07_X = $031B

Sprite08_Y = $031C
Sprite08_T = $031D
Sprite08_S = $031E
Sprite08_X = $031F

Sprite09_Y = $0320
Sprite09_T = $0321
Sprite09_S = $0322
Sprite09_X = $0323

Sprite10_Y = $0324
Sprite10_T = $0325
Sprite10_S = $0326
Sprite10_X = $0327

Sprite11_Y = $0328
Sprite11_T = $0329
Sprite11_S = $032A
Sprite11_X = $032B

Sprite12_Y = $032C
Sprite12_T = $032D
Sprite12_S = $032E
Sprite12_X = $032F

Sprite13_Y = $0330
Sprite13_T = $0331
Sprite13_S = $0332
Sprite13_X = $0333

Sprite14_Y = $0334
Sprite14_T = $0335
Sprite14_S = $0336
Sprite14_X = $0337

Sprite15_Y = $0338
Sprite15_T = $0339
Sprite15_S = $033A
Sprite15_X = $033B

Sprite16_Y = $033C
Sprite16_T = $033D
Sprite16_S = $033E
Sprite16_X = $033F

Sprite17_Y = $0340
Sprite17_T = $0341
Sprite17_S = $0342
Sprite17_X = $0343

Sprite18_Y = $0344
Sprite18_T = $0345
Sprite18_S = $0346
Sprite18_X = $0347

Sprite19_Y = $0348
Sprite19_T = $0349
Sprite19_S = $034A
Sprite19_X = $034B

Sprite20_Y = $034C
Sprite20_T = $034D
Sprite20_S = $034E
Sprite20_X = $034F

Sprite21_Y = $0350
Sprite21_T = $0351
Sprite21_S = $0352
Sprite21_X = $0353

Sprite22_Y = $0354
Sprite22_T = $0355
Sprite22_S = $0356
Sprite22_X = $0357

Sprite23_Y = $0358
Sprite23_T = $0359
Sprite23_S = $035A
Sprite23_X = $035B

Sprite24_Y = $035C
Sprite24_T = $035D
Sprite24_S = $035E
Sprite24_X = $035F

Sprite25_Y = $0360
Sprite25_T = $0361
Sprite25_S = $0362
Sprite25_X = $0363

Sprite26_Y = $0364
Sprite26_T = $0365
Sprite26_S = $0366
Sprite26_X = $0367

Sprite27_Y = $0368
Sprite27_T = $0369
Sprite27_S = $036A
Sprite27_X = $036B

Sprite28_Y = $036C
Sprite28_T = $036D
Sprite28_S = $036E
Sprite28_X = $036F

Sprite29_Y = $0370
Sprite29_T = $0371
Sprite29_S = $0372
Sprite29_X = $0373

Sprite30_Y = $0374
Sprite30_T = $0375
Sprite30_S = $0376
Sprite30_X = $0377

Sprite31_Y = $0378
Sprite31_T = $0379
Sprite31_S = $037A
Sprite31_X = $037B

Sprite32_Y = $037C
Sprite32_T = $037D
Sprite32_S = $037E
Sprite32_X = $037F

Sprite33_Y = $0380
Sprite33_T = $0381
Sprite33_S = $0382
Sprite33_X = $0383

Sprite34_Y = $0384
Sprite34_T = $0385
Sprite34_S = $0386
Sprite34_X = $0387

Sprite35_Y = $0388
Sprite35_T = $0389
Sprite35_S = $038A
Sprite35_X = $038B

Sprite36_Y = $038C
Sprite36_T = $038D
Sprite36_S = $038E
Sprite36_X = $038F

Sprite37_Y = $0390
Sprite37_T = $0391
Sprite37_S = $0392
Sprite37_X = $0393

Sprite38_Y = $0394
Sprite38_T = $0395
Sprite38_S = $0396
Sprite38_X = $0397

Sprite39_Y = $0398
Sprite39_T = $0399
Sprite39_S = $039A
Sprite39_X = $039B

Sprite40_Y = $039C
Sprite40_T = $039D
Sprite40_S = $039E
Sprite40_X = $039F

Sprite41_Y = $03A0
Sprite41_T = $03A1
Sprite41_S = $03A2
Sprite41_X = $03A3

Sprite42_Y = $03A4
Sprite42_T = $03A5
Sprite42_S = $03A6
Sprite42_X = $03A7

Sprite43_Y = $03A8
Sprite43_T = $03A9
Sprite43_S = $03AA
Sprite43_X = $03AB

Sprite44_Y = $03AC
Sprite44_T = $03AD
Sprite44_S = $03AE
Sprite44_X = $03AF

Sprite45_Y = $03B0
Sprite45_T = $03B1
Sprite45_S = $03B2
Sprite45_X = $03B3

Sprite46_Y = $03B4
Sprite46_T = $03B5
Sprite46_S = $03B6
Sprite46_X = $03B7

Sprite47_Y = $03B8
Sprite47_T = $03B9
Sprite47_S = $03BA
Sprite47_X = $03BB

Sprite48_Y = $03BC
Sprite48_T = $03BD
Sprite48_S = $03BE
Sprite48_X = $03BF

Sprite49_Y = $03C0
Sprite49_T = $03C1
Sprite49_S = $03C2
Sprite49_X = $03C3

Sprite50_Y = $03C4
Sprite50_T = $03C5
Sprite50_S = $03C6
Sprite50_X = $03C7

Sprite51_Y = $03C8
Sprite51_T = $03C9
Sprite51_S = $03CA
Sprite51_X = $03CB

Sprite52_Y = $03CC
Sprite52_T = $03CD
Sprite52_S = $03CE
Sprite52_X = $03CF

Sprite53_Y = $03D0
Sprite53_T = $03D1
Sprite53_S = $03D2
Sprite53_X = $03D3

Sprite54_Y = $03D4
Sprite54_T = $03D5
Sprite54_S = $03D6
Sprite54_X = $03D7

Sprite55_Y = $03D8
Sprite55_T = $03D9
Sprite55_S = $03DA
Sprite55_X = $03DB

Sprite56_Y = $03DC
Sprite56_T = $03DD
Sprite56_S = $03DE
Sprite56_X = $03DF

Sprite57_Y = $03E0
Sprite57_T = $03E1
Sprite57_S = $03E2
Sprite57_X = $03E3

Sprite58_Y = $03E4
Sprite58_T = $03E5
Sprite58_S = $03E6
Sprite58_X = $03E7

Sprite59_Y = $03E8
Sprite59_T = $03E9
Sprite59_S = $03EA
Sprite59_X = $03EB

Sprite60_Y = $03EC
Sprite60_T = $03ED
Sprite60_S = $03EE
Sprite60_X = $03EF

Sprite61_Y = $03F0
Sprite61_T = $03F1
Sprite61_S = $03F2
Sprite61_X = $03F3

Sprite62_Y = $03F4
Sprite62_T = $03F5
Sprite62_S = $03F6
Sprite62_X = $03F7

Sprite63_Y = $03F8
Sprite63_T = $03F9
Sprite63_S = $03FA
Sprite63_X = $03FB

Sprite64_Y = $03FC
Sprite64_T = $03FD
Sprite64_S = $03FE
Sprite64_X = $03FF


	sta as_drawspr_20191111_a

as_drawspr001_20191111:
	lda $2002
	bmi as_drawspr001_20191111 

as_drawspr002_20191111:
	lda $2002
	bpl as_drawspr002_20191111

	lda #%00001000 
	sta $2000
	lda #%00000110
	sta $2001

	lda #0
	sta $2003
	lda #3
	sta $4014

	lda #%00011110
	sta $2001

	lda as_drawspr_20191111_a

	rts

;no12
as_setaspal_20191111:

as_setaspal_20191111_a = $07FF

	sta as_setaspal_20191111_a

as_setaspal001_20191111:
	lda $2002
	bmi as_setaspal001_20191111 

as_setaspal002_20191111:
	lda $2002
	bpl as_setaspal002_20191111

	lda	#$00
	sta	$2000
	sta	$2001

	lda	#$3f
	sta	$2006
	lda	#$00
	sta	$2006

	lda	#$0F
	sta	$2007
	
	lda	#$1A
	sta	$2007
	
	lda	#$06
	sta	$2007

	lda	#$20
	sta	$2007

	lda	#$30
	sta	$2007

	lda	#$0F
	sta	$2007

	lda	#$05
	sta	$2007

	lda	#$26
	sta	$2007

	lda	#$30
	sta	$2007

	lda	#$26
	sta	$2007

	lda	#$16
	sta	$2007

	lda	#$06
	sta	$2007

	lda	#$30
	sta	$2007

	lda	#$1A
	sta	$2007

	lda	#$11
	sta	$2007

	lda	#$06
	sta	$2007

	lda	#$0F
	sta	$2007

	lda	#$02
	sta	$2007

	lda	#$26
	sta	$2007

	lda	#$15
	sta	$2007

	lda	#$30
	sta	$2007

	lda	#$2D
	sta	$2007

	lda	#$17
	sta	$2007

	lda	#$07
	sta	$2007

	lda	#$30
	sta	$2007

	lda	#$11
	sta	$2007

	lda	#$27
	sta	$2007

	lda	#$05
	sta	$2007

	lda	#$30
	sta	$2007

	lda	#$1A
	sta	$2007

	lda	#$13
	sta	$2007

	lda	#$25
	sta	$2007

	lda	#$08
	sta	$2000
	lda	#$1e
	sta	$2001

	lda as_setaspal_20191111_a

	rts

;no13
as_mes_20191111:

as_mes_20191111_x = $07FE
as_mes_20191111_y = $07FD

	stx as_mes_20191111_x
	sty as_mes_20191111_y

as_mes001_20191111:
	lda $2002
	bmi as_mes001_20191111 

as_mes002_20191111:
	lda $2002
	bpl as_mes002_20191111

	lda #%00001000 
	sta $2000
	lda #%00000110
	sta $2001

	lda $0400
	sta $2006
	lda $0401
	sta $2006

	ldx #0

as_mes003_20191111:

	lda $0402,x

	cmp #$FF
	beq as_mes004_20191111

	sta $2007
	inx
	jmp as_mes003_20191111

as_mes004_20191111:
	lda #$20
	sta $2006
	lda #$00
	sta $2006

	lda #%00011110
	sta $2001

	ldx as_mes_20191111_x
	ldy as_mes_20191111_y

	rts

;no14
as_point_20191111:
as_point = $0430
as_point_20191111_x = $07FE

	stx as_point_20191111_x

	lda #00
	sta $0431
	sta $0432
	sta $0433

as100tmp0:
	lda $0430
	sec
	sbc #100
	bcc as10tmp0
	sta $0430
	lda $0431
	clc
	adc #1
	sta $0431
	jmp as100tmp0

as10tmp0:
	lda $0430
	sec
	sbc #10
	bcc as1tmp0
	sta $0430
	lda $0432
	clc
	adc #1
	sta $0432
	jmp as10tmp0

as1tmp0:
	lda $0430
	sta $0433


as_point001_20191111:
	lda $2002
	bmi as_point001_20191111 

as_point002_20191111:
	lda $2002
	bpl as_point002_20191111

	lda #%00001000 
	sta $2000
	lda #%00000110
	sta $2001

	lda $042E
	sta $2006
	lda $042F
	sta $2006

	lda #$30
	clc
	adc $0431
	sta $2007

	lda #$30
	clc
	adc $0432
	sta $2007
	
	lda #$30
	clc
	adc $0433
	sta $2007

	lda #$20
	sta $2006
	lda #$00
	sta $2006

	lda #%00011110
	sta $2001

	ldx as_point_20191111_x

	rts

;no15
as_getkey_20191111:

as_getkey_20191111_a = $07FF
as_buttonA = $07FC
as_buttonB = $07FB
as_buttonSE = $07FA
as_buttonST = $07F9
as_buttonU = $07F8
as_buttonD = $07F7
as_buttonL = $07F6
as_buttonR = $07F5

	sta as_getkey_20191111_a

	lda #$01
	sta $4016
	lda #$00
	sta $4016

	lda $4016
	and #1
	sta as_buttonA
	lda $4016
	and #1
	sta as_buttonB
	lda $4016
	and #1
	sta as_buttonSE
	lda $4016
	and #1
	sta as_buttonST
	lda $4016
	and #1
	sta as_buttonU
	lda $4016
	and #1
	sta as_buttonD
	lda $4016
	and #1
	sta as_buttonL
	lda $4016
	and #1
	sta as_buttonR

	lda as_getkey_20191111_a

	rts

;no16
as_hit = $0427
as_hit_20191111:
	lda $0423
	clc
	adc #7
	sec
	sbc $0425
	bcs as_hit001_20191111
	jmp as_hit005_20191111

as_hit001_20191111:
	lda $0425
	clc
	adc #7
	sec
	sbc $0423
	bcs as_hit002_20191111
	jmp as_hit005_20191111

as_hit002_20191111:
	lda $0424
	clc
	adc #7
	sec
	sbc $0426
	bcs as_hit003_20191111
	jmp as_hit005_20191111

as_hit003_20191111:
	lda $0426
	clc
	adc #7
	sec
	sbc $0424
	bcs as_hit004_20191111
	jmp as_hit005_20191111

as_hit004_20191111:
	lda #1
	sta $0427
	jmp as_hit006_20191111

as_hit005_20191111:
	lda #0
	sta $0427
	jmp as_hit006_20191111

as_hit006_20191111:
	rts

;no17
as_clear_20191111:

as_clear_20191111_a = $07FF
as_clear_20191111_x = $07FE
as_clear_20191111_y = $07FD

	sta as_clear_20191111_a
	stx as_clear_20191111_x
	sty as_clear_20191111_y

	ldx #0
as_clear001_20191111:
	lda #0
	sta $0000,x
	inx
	cpx #255
	bne as_clear001_20191111

;	ldx #0
;as_clear002_20191111:
;	lda #0
;	sta $0100,x
;	inx
;	cpx #255
;	bne as_clear002_20191111

	ldx #0
as_clear003_20191111:
	lda #0
	sta $0200,x
	inx
	cpx #255
	bne as_clear003_20191111

	ldx #0
as_clear004_20191111:
	lda #0
	sta $0300,x
	inx
	cpx #255
	bne as_clear004_20191111

	ldx #0
as_clear005_20191111:
	lda #0
	sta $0400,X
	inx
	cpx #255
	bne as_clear005_20191111

	ldx #0
as_clear006_20191111:
	lda #0
	sta $0500,x
	inx
	cpx #255
	bne as_clear006_20191111

	ldx #0
as_clear007_20191111:
	lda #0
	sta $0600,x
	inx
	cpx #255
	bne as_clear007_20191111

	ldx #0
as_clear008_20191111:
	lda #0
	sta $0700,x
	inx
	cpx #255
	bne as_clear008_20191111

as_clear009_20191111:
	lda $2002
	bmi as_clear009_20191111

as_clear010_20191111:
	lda $2002
	bpl as_clear010_20191111

	lda #%00001000 
	sta $2000
	lda #%00000110
	sta $2001

	lda #$20
	sta $2006
	lda #$00
	sta $2006

	ldx #0
as_clear012_20191111:
	ldy #0
as_clear011_20191111:
	lda #0
	sta $2007
	iny
	cpy #255
	bne as_clear011_20191111

	inx
	cpx #16
	bne as_clear012_20191111

	lda #$3F
	sta $2006
	lda #$00
	sta $2006

	ldy #0
as_clear013_20191111:
	lda #0
	sta $2007
	iny
	cpy #32
	bne as_clear013_20191111

	lda	#$00
	sta	$2005
	sta	$2005

	lda #0
	sta $2003
	lda #2
	sta $4014

	lda #$20
	sta $2006
	lda #$00
	sta $2006

	lda #%00011110
	sta $2001

	lda as_clear_20191111_a
	ldx as_clear_20191111_x
	ldy as_clear_20191111_y

	rts

;no19
as_end_20191111:
	jmp as_end_20191111
