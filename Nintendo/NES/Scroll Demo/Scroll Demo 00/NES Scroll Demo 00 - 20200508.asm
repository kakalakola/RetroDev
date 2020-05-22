;----------------------------------------------------
;NES SCROLL DEMO 0.0 - 2020.05.08
;(A simple scrolling demo)
;by Saad Azim
;----------------------------------------------------

;Compiled with ASM6
;Tested with, and works on MAME, FCEUX, No$NES, and Mesen

;FILL_COLOR=$0f              ;Black
FILL_COLOR=$2d              ;Dark grey

PPU_CONF_01=%10000000
            ;|:|:|:|:
            ;|:|:|:base nametable address (00: $2000, 01: $2400, 10: $2800, 11: $2c00)
            ;|:|:|VRAM address increment per read/write of PPU data (0: 1 drawing rows, 1: 32, drawing columns)
            ;|:|:sprite pattern table address(0: $0000, 1: $1000)
            ;|:|background pattern table address(0: $0000, 1: $1000)
            ;|:enable 8x16 sprites
            ;|layer select (used in Nintendo Playchoice boards, should always be set to 0 for NES programming)
            ;generate NMI at the start of VBLANK

;**NOTE**
;Bits 0 and 1 act as the x and y scroll toggle bits, basically. When bit 0 is 0, for example, the screen will only scroll horizontally across the left half of the nametable. When set to 1, the screen will only scroll horizontally across the right half of the name table.

PPU_CONF_02=%00001110
            ;|:|:|:|:
            ;|:|:|:|Enable grayscale 
            ;|:|:|:Disable background clipping in leftmost 8 pixels of screen
            ;|:|:|Disable sprite clipping in leftmost 8 pixels of screen
            ;|:|:Enable backgrounds
            ;|:|Enable sprites
            ;|:Intensify reds
            ;|Intensify greens
            ;Intensify blues

SCREEN_X_SPEED_HIGH=#$01
SCREEN_X_SPEED_LOW=#$00


screenXHigh=$0200
screenXLow=$0201
ppuConf01=$0202
ppuFlag=$0203


;iNES header                                                             (Byte $00-$03)
.db "NES",$1a

;number of PRG-ROM blocks, LSB                                               (Byte $04)
.db $01

;number of CHR-ROM banks, LSB                                                (Byte $05)
.db $00

;mappers, mirroring, SRAM, trainer, etc.                                 (Byte $06-$07)
.db %00000001
    ;|:|:|:|:
    ;|:|:|:|Mirrorring (0: vertical, 1: horizontal)
    ;|:|:|:SRAM present at $6000-7fff
    ;|:|:|512-byte trainer present at $7000-$71ff before PRG data)
    ;|:|:Ignore mirrorring bit, and provide four-screen VRAM
    ;Bytes 3-0 of mapper number

;When using NES 2.0 header, CHR RAM must be specified if no CHR ROM is present
.db %00001000
    ;|:|:|:|:
    ;|:|:|:Console type (00: NES/Famicom, 01: Vs. System, 10: Playchoice 10, 11: Extended Console Type)
    ;|:|:NES 2.0 header identifier
    ;Bytes 4-7 of mapper number

.db %00000000                                                               ;(Byte $08)
    ;|:|:|:|:
    ;|:|:Bytes 8-11 of mapper number
    ;Submapper number

.db %00000000                                                               ;(Byte $09)
    ;|:|:|:|:
    ;|:|:Number of PRG-ROM blocks, MSB
    ;Number of CHR-ROM blocks, MSB

.db %00000000                                                               ;(Byte $0a)
    ;|:|:|:|:
    ;|:|:PRG RAM (volatile) shift count
    ;PRG RAM (non-volatile) shift count, AKA SRAM

.db %00000111                                                               ;(Byte $0b)
    ;|:|:|:|:
    ;|:|:CHR RAM (volatile) shift count
    ;CHR RAM (non-volatile) shift count, AKA SRAM

;RAM amount = 64<<[shift count]bytes

.db %00000000                                                               ;(Byte $0c)
    ;|:|:|:|:
    ;|:|:|:CPU/PPU timing (00: NTSC NES, 01: Licensed PAL NES, 02: Multi-Region, 03: Dendy)
    ;xxxxxx

.db %00000000                                                               ;(Byte $0d)
    ;|:|:|:|:
    ;|:|:Vs. PPU type (Extended console type, if bits 0-1 in byte 7 == 3)
    ;Vs. Hardware type (Unused, if bits 0-1 in byte 7 == 3)

.db %00000000                                                               ;(Byte $0e)
    ;|:|:|:|:
    ;|:|:|:Number of miscelaneous ROMs present
    ;xxxxxx

.db %00000000                                                               ;(Byte $0f)
    ;|:|:|:|:
    ;|:Default expansion device
    ;xx

;PRG-ROM
.org $c000

reset:

  sei                       ;Disable IRQs
  dlc                       ;Disable decimal mode
  ldx #$40
  stx $4017                 ;Disable APU frame IRQ
  ldx #$ff
  txs                       ;Reset stack pointer
                            ;The NES stack resides at $01ff-$0100 in RAM

  inx                       ;X=0
  stx $2000                 ;Disable NMI
  stx $2001                 ;Disable rendering

- lda $2002                	;Wait for VBLANK
  bpl -                     ;Branch on PLus (Negative flag clear)

  ;A local loop to clear the RAM ($0000-$07ff)
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

  ;Point $2007 to $0000 in VRAM, start of tile data
  ldx #0
  stx $2006
  stx $2006
  ;Copy tiles
- lda bgTiles,x
  sta $2007
  inx
  cpx #$70
  bne-

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
  
  lda #PPU_CONF_01
  sta ppuConf01             ;Needed for nametable flipping later. :D

- lda $2002                 ;Wait for another VBLANK
  bpl -                     ;Branch on PLus
  ;The PPU is now ready

  ;Enable PPU by writing a value
  lda ppuConf01
  sta $2000
  lda #PPU_CONF_02
  sta $2001

loop:

  clc
  lda screenXLow
  adc #SCREEN_X_SPEED_LOW
  sta screenXLow

  lda screenXHigh
  adc #SCREEN_X_SPEED_HIGH
  sta screenXHigh

  ;**NOTE**
  ;The flag needs to be checked following the ADC to see if the carry flag has been set

  bcc -                     ;Branch to the wait for VBlank if the carry flag is clear (aka not set) ...

  lda ppuConf01             ;Otherwise toggle the x scroll bit
  eor #1
  sta ppuConf01

  ;**NOTE FROM FUTURE PROJECTS**
  ;Checking $2002 to wait for a VBlank is NOT a good way to wait in the loop. It works fine for simple things like this demo. But when dealing with more complicated timing related actions, like multi-line-scrolling; things get all wonky.

- lda $2002                 ;Wait for VBlank
  bpl -

  jmp loop

vblank:
  pha                       ;Backup registers
  ;txa
  ;pha
  ;tya
  ;pha

  lda screenXHigh
  sta $2005                 ;Set X scroll to screenXHigh
  lda #0
  sta $2005                 ;Set Y scroll to 0

  lda ppuConf01
  sta $2000

  ;Not needed for this demo
  ;lda #PPU_CONF_02
  ;sta $2001

  pla                       ;Restore registers
  ;tay
  ;pla
  ;tax
  ;pla
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
.db #FILL_COLOR,$00,$00,$00
.db #FILL_COLOR,$00,$00,$00
.db #FILL_COLOR,$00,$00,$00
.db #FILL_COLOR,$00,$00,$00


;tiles (112, or $70 bytes in size)
bgTiles:
.db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ;Blank
.db $7e,$ff,$ff,$ff,$ff,$ff,$ff,$7e,$7e,$ef,$d7,$83,$bb,$bb,$bb,$7e ;A
.db $7e,$c7,$db,$c3,$dd,$dd,$c3,$7e,$7e,$ff,$ff,$ff,$ff,$ff,$ff,$7e ;B
.db $7e,$ff,$ff,$ff,$ff,$ff,$ff,$7e,$7e,$e3,$dd,$df,$df,$dd,$e3,$7e ;C
.db $7e,$c7,$db,$dd,$dd,$dd,$c3,$7e,$7e,$ff,$ff,$ff,$ff,$ff,$ff,$7e ;D
.db $7e,$ff,$ff,$ff,$ff,$ff,$ff,$7e,$7e,$c3,$df,$c7,$df,$df,$c3,$7e ;E
.db $7e,$c3,$df,$c7,$df,$df,$df,$7e,$7e,$ff,$ff,$ff,$ff,$ff,$ff,$7e ;F

bgNameTable:
.db $01                     ;A
.db $02                     ;B
.db $03                     ;C
.db $04                     ;D
.db $05                     ;E
.db $06                     ;F

bgAttributeTable:
.db %00000000               ;Use palette 01 for all tiles
.db %01010101               ;Use palette 02 for all tiles
.db %10101010               ;Use palette 03 for all tiles

.pad $fffa                  ;Pad the rest of the file until $fffa 
.word vblank, reset,irq     ;Jump table for vBlank/NMI, reset, & IRQ
