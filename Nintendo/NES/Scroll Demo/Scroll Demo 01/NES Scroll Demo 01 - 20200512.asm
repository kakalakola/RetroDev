;----------------------------------------------------
;NES SCROLL DEMO 0.1 - 2020.05.12
;(Mucking about with zero sprite hit, and timed interrupts)
;by Saad Azim
;----------------------------------------------------

;Tested with, and works on MAME, FCEUX (with graphical glitches), No$NES, and Mesen.

;FILL_COLOR=$0f              ;Black
FILL_COLOR=$2d              ;Dark grey

PPU_CONF_01=%10010000
            ;|:|:|:|:
            ;|:|:|:base nametable address (00: $2000, 01: $2400, 10: $2800, 11: $2c00)
            ;|:|:|VRAM address increment per read/write of PPU data (0: 1 drawing rows, 1: 32, drawing columns)
            ;|:|:sprite pattern table address(0: $0000, 1: $1000)
            ;|:|background pattern table address(0: $0000, 1: $1000)
            ;|:enable 8x16 sprites
            ;|layer select (used in Nintendo Playchoice boards, should always be set to 0 for NES programming)
            ;generate NMI at the start of VBLANK

PPU_CONF_02=%00011111
            ;|:|:|:|:
            ;|:|:|:|Enable grayscale 
            ;|:|:|:Disable background clipping in leftmost 8 pixels of screen
            ;|:|:|Disable sprite clipping in leftmost 8 pixels of screen
            ;|:|:Enable backgrounds
            ;|:|Enable sprites
            ;|:Intensify reds
            ;|Intensify greens
            ;Intensify blues

;Masks for toggling different color & greyscale modes
;PPU_BLUE=%10000000
;PPU_GREEN=%01000000
;PPU_RED=%00100000
PPU_GREY=%00000001

ZERO_SPRITE_Y=#$1e          ;Zero sprite hit detection *should* happen at scanline #$1e,
                            ;but the update happens mid-scanline. For this demo, a series of NOPs will be used to time the scroll updates to the beginning of the next line

;Using a flag to prevent graphical errors, compared to using loading $2002 and then using bpl to check for an VBlank
flagFrame=$0200

screenXHigh01=$0204
screenXHigh02=$0205
screenXHigh03=$0206
screenXHigh04=$0207

ppuConf011=$0208
ppuConf012=$0209
ppuConf013=$020a
ppuConf014=$020b

ppuConf02=$020c

spriteData=$0300


;iNES+NES 2.0 header
.db "NES",$1a               ;iNES header                                 (Byte $00-$03)
.db $01,$01                 ;Number of PRG & CHR ROM blocks              (Byte $04-$05)

.db %00000001               ;mappers, mirroring, SRAM, trainer, etc.         (Byte $06)
    ;|:|:|:|:
    ;|:|:|:|Mirrorring (0: vertical, 1: horizontal)
    ;|:|:|:SRAM present at $6000-7fff
    ;|:|:|512-byte trainer present at $7000-$71ff before PRG data)
    ;|:|:Ignore mirrorring bit, and provide four-screen VRAM
    ;Bytes 3-0 of mapper number

.db %00001000               ;Using NES 2.0 header                            (Byte $07)
    ;|:|:|:|:
    ;|:|:|:Console type (00: NES/Famicom, 01: Vs. System, 10: Playchoice 10, 11: Extended Console Type)
    ;|:|:NES 2.0 header identifier
    ;Bytes 4-7 of mapper number

.db $00                     ;Mapper & Submapper number                       (Byte $08)
.db $00                     ;PRG & CHR ROM blocks, MSB                       (Byte $09)
.db $00,$00                 ;PRG & CHR RAM                               (Byte $0a-$0b)
.db $00                     ;CPU/PPU timing                                  (Byte $0c)
.db $00                     ;PPU & Vs. hardware type                         (Byte $0d)
.db $00,$00                 ;Misc. ROMs & default expansio devices       (Byte $0e-$0f)


;PRG-ROM
.org $c000

reset:

  sei                       ;Disable IRQs
  dlc                       ;Disable decimal mode
  ldx #$40
  stx $4017                 ;Disable APU frame IRQ
  ldx #$ff
  txs                       ;Reset stack pointer
  inx                       ;X=0
  stx $2000                 ;Disable NMI
  stx $2001                 ;Disable rendering

- lda $2002                	;Wait for VBLANK
  bpl -                     ;Branch on PLus (Negative flag clear)

  ;A local loop to clear the RAM
  lda #$00
- sta $000, x
  sta $100, x
  sta $200, x
  sta $300, x
  sta $400, x
  sta $500, x
  sta $600, x
  sta $700, x
  inx
  bne -

  ;Point the PPU to PPU RAM at $3f00, the BG fill color
  lda #$3f
  sta $2006
  lda #$00
  sta $2006
  ;Copy colors
  ldx #0
- lda palettes,x
  sta $2007
  inx
  cpx #$20
  bne-

  ;Unlike the last demo, there's no need to copy tile data to VRAM, since the data is stored in CHR-ROM

  ;Point $2007 to $2000 in VRAM, location of our first nametable
  lda #$20
  ldx #0
  ldy #4
  sta $2006
  stx $2006

  ldx #$40                  ;Offset, to make sure this loop repeats exactly #$03c0 times

  ;Fill the data for first nametable with the value at location bgNameTable (the tile "A")
- lda bgNameTable
  sta $2007
  inx
  bne-

  dey
  bne-                      ;BOTH bne jumps back to "-", which makes life SO much easier ^_^

  ;Since $2007 is now pointing to $23c0, location of the first attribute table, all that's needed here is to copy the attribute data.
  ldx #$40
- lda bgAttributeTable
  sta $2007
  dex
  bne-

  ;#$03c0 of nametable data and #$0040 attribute data amounts to a total of exactly #$0400 bytes. The size of one of the two available pages of data in the NES VRAM for backgrounds.

  ;At this point, $2007 is automatically pointed to $2400 in VRAM, location of the second nametable. So there's no need to manually set things with writes to $2006. All that's needed is to set the counters, and the data is copied to the right location
  ldy #4
  ldx #$40

- lda bgNameTable+1
  sta $2007
  inx
  bne-

  dey
  bne-

  ;Copy the attribute table for the second nametable
  ldx #$40
- lda bgAttributeTable+1
  sta $2007
  dex
  bne-

  ;Manually writing sprite attribute data to RAM. The sprite data (for ALL sprites) will be copied to the PPU OAM (Object Attribute Memory) during VBlank
  lda #ZERO_SPRITE_Y        ;Sprite Y
  sta spriteData
  lda #$01                  ;Tile number
  sta spriteData+1
  lda #$00                  ;Attributes
  sta spriteData+2
  lda #$00                  ;Sprite X
  sta spriteData+3

  lda #PPU_CONF_01
  sta ppuConf011             ;Needed for nametable flipping later. :D
  sta ppuConf012
  sta ppuConf013
  sta ppuConf014

  lda #PPU_CONF_02
  sta ppuConf02

- lda $2002                 ;Wait for another VBLANK
  bpl -                     ;Branch on PLus
  ;The PPU is now ready

  ldx #0
  ldy #0

  stx flagFrame

  ;Enabling PPU by writing a value
  lda ppuConf011
  sta $2000
  lda ppuConf02
  sta $2001

loop:
  ;If flagFrame is set (i.e. its value is > 0), then the the progam should loop indefinitely until VBlank. 
  lda flagFrame
  bne loop

  ;Store a copy of #PPU_CONF_02 with the greyscale bit set to 0 (off)
  lda #PPU_CONF_02
  eor #PPU_GREY
  sta ppuConf02

  ;Calculate position & scroll bit for strip 01
  lda screenXHigh01
  adc #1
  sta screenXHigh01

  bcc calcXPos02            ;Should be familiar territory
  clc
  lda ppuConf011
  eor #1
  sta ppuConf011


  ;Calculate position & scroll bit for strip 02
calcXPos02:
  clc
  lda screenXHigh02
  adc #2
  sta screenXHigh02

  bcc calcXPos03:
  clc
  lda ppuConf012
  eor #1
  sta ppuConf012

  ;Calculate position & scroll bit for strip 03
calcXPos03:
  lda screenXHigh03
  adc #3
  sta screenXHigh03

  bcc calcXPos04
  clc
  lda ppuConf013
  eor #1
  sta ppuConf013

  ;Calculate position & scroll bit for strip 04
  ;lda screenXHigh04
calcXPos04:
  lda screenXHigh04
  adc #4
  sta screenXHigh04

  ;bcc +
  bcc setFlagFrame
  clc
  lda ppuConf014
  eor #1
  sta ppuConf014

setFlagFrame:               ;Having done everything, it's time to set the flagFrame
  lda #1
  sta flagFrame

  jmp loop

vblank:
  pha                       ;Backup registers
  txa
  pha
  tya
  pha

  ;lda #0                    ;Disable register $2000
  ;sta $2000
  ;lda #0                    ;Disable register $2001 (Maybe for effects like flashing...?)
  ;sta $2001

  lda #PPU_CONF_01          ;Reset the PPU configurations
  sta $2000
  lda #PPU_CONF_02
  sta $2001

  ldx #0
  stx $2003                 ;Point OAM write register to address $00
  lda #$03                  ;DMA sprite attributes from RAM to OAM
  sta $4014

  stx $2005                 ;Set X scroll to 0
  stx $2005                 ;Set Y scroll to 0

  ;Wait for sprite zero hit. **NEEDS TO BE DONE WHILE IN VBLANK**
  ;Also, it looks like two checks are required to detect the sprite zero hit.

- lda $2002                 ;Wait for Sprite Zero non-hit (...?)
  and #%01000000            ;Mask everything but the Sprite Zero Hit bit
  bne -                     ;Loop while Sprite Zero is NOT hit

- lda $2002                 ;Wait for Sprite Zero hit
  and #%01000000            ;Mask everything but the Sprite Zero Hit bit
  beq -                     ;Wait until Sprite Zero IS hit

  ;Wait until the beam almost reaches the end of the screen. Though, timing seems to vary between different emulators. -_-
  ldx #8
- nop
  dex
  bne -

  ;Manual NOPs, for a bit more control over timing :D
  nop
  nop

  ;**NOTE**
  ;Things look as intended on MAME, Mesen, and No$NES. But the updates happen mid-draw in FCEUX

  ;Update the modified PPU configurations for the first scroll
  ;ppuConf01x basically updates the scroll toggle bit. ppuConf02 is used to disable greyscale mode
  lda ppuConf011
  sta $2000
  lda ppuConf02
  sta $2001

  lda screenXHigh01
  sta $2005                 ;Set X scroll
  lda #0
  sta $2005                 ;Set Y scroll to 0

  ;Since there's no way to *repeat* sprite zero hit, the next scroll updates have to be manually delayed/timed
  ldy #3
  ldx #4
- nop
  dex
  bne -

  dey
  bne -

  lda ppuConf012
  sta $2000

  lda screenXHigh02
  sta $2005
  lda #0
  sta $2005

  ;Delay for second manually timed scroll
  ldy #2
  ldx #0
- nop
  dex
  bne -

  dey
  bne -

  lda ppuConf013
  sta $2000

  lda screenXHigh03
  sta $2005
  lda #0
  sta $2005

  ;Delay for third manually timed scroll
  ldy #3
  ldx #4
- nop
  dex
  bne -

  dey
  bne -

  lda ppuConf014
  sta $2000

  lda screenXHigh04
  sta $2005
  lda #0
  sta $2005

  ;Delay for the fourth manually timed scroll (or the lack there of)
  ldy #6
  ldx #$0c
- nop
  dex
  bne -

  dey
  bne -

  lda #PPU_CONF_01
  sta $2000

  lda #0
  sta $2005
  sta $2005

  ;Clear flagFrame
  sta flagFrame

  pla                       ;Restore registers
  tay
  pla
  tax
  pla
  rti

irq:
  rti

palettes:
;BG palettes
.db #FILL_COLOR,$05,$0a,$30
.db #FILL_COLOR,$01,$26,$30
.db #FILL_COLOR,$04,$27,$30
.db #FILL_COLOR,$30,$30,$30
;Sprite palettes (not really needed for this demo)
.db #FILL_COLOR,$0f,$01,$25
.db #FILL_COLOR,$00,$00,$00
.db #FILL_COLOR,$00,$00,$00
.db #FILL_COLOR,$00,$00,$00

bgNameTable:
.db $01                     ;A
.db $02                     ;B
.db $03                     ;C
.db $04                     ;D
.db $05                     ;E
.db $06                     ;F

bgAttributeTable:
.db %00000000               ;Palette 01
.db %01010101               ;Palette 02
.db %10101010               ;Palette 03

.pad $fffa                  ;Pad the rest of the file until $fffa 
.word vblank, reset,irq     ;Jump table for vBlank/NMI, reset, & IRQ

;CHR ROM
;Since the tiles are stored in CHR ROM, they're available to the PPU on boot. There's no need to copy them manually to VRAM.

;Tiles used for sprites are stored in $0000-$0fff, as defined in #PPU_CONF_01
.base $0000                 ;.org doesn't work (?)

.dsb $0010,$00              ;Blank tile
.dsb $0008,$ff              ;Zero sprite tile
.dsb $0008,$00

.pad $1000

;Tiles used for background are stored in $1000-$1fff, as defined in #PPU_CONF_01
.dsb $0010,$00              ;Blank tile
.db $7e,$ff,$ff,$ff,$ff,$ff,$ff,$7e,$7e,$ef,$d7,$83,$bb,$bb,$bb,$7e ;A
.db $7e,$c7,$db,$c3,$dd,$dd,$c3,$7e,$7e,$ff,$ff,$ff,$ff,$ff,$ff,$7e ;B
.db $7e,$ff,$ff,$ff,$ff,$ff,$ff,$7e,$7e,$e3,$dd,$df,$df,$dd,$e3,$7e ;C
.db $7e,$c7,$db,$dd,$dd,$dd,$c3,$7e,$7e,$ff,$ff,$ff,$ff,$ff,$ff,$7e ;D
.db $7e,$ff,$ff,$ff,$ff,$ff,$ff,$7e,$7e,$c3,$df,$c7,$df,$df,$c3,$7e ;E
.db $7e,$c3,$df,$c7,$df,$df,$df,$7e,$7e,$ff,$ff,$ff,$ff,$ff,$ff,$7e ;F

.pad $2000
