;----------------------------------------------------
;NES INPUT HANDLING 0.2 - 2022.08.30
;(Moving a sprite on screen)
;by Saad Azim
;----------------------------------------------------

;This project takes a blunt-force approach to processing input data and moving a sprite on screen. To that extent, only player 1 sprite is moved on screen.

;iNES header 2.0, 16 bytes                                               (Byte $00-$03)
.db "NES",$1a,$01,$01,$01,$08,$00,$00,$00,$00,$00,$00,$00,$01
; - 2 PRG-ROM bank (16 Kb)
; - 1 CHR-ROM (8 Kb)
; - Horizontal mirrorring, no mappers, mirrorring, SRAM, trainer, etc
; - No PRG-RAM, no CHR-RAM
; - NTSC timing
; - No expansion device present/used
; - Input type - Standard Controllers

;----------------------------------------------------
;PRG-ROM
;$c000-$ffff - 16 Kb
;----------------------------------------------------
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

  ;Point the PPU to PPU RAM at $3f00
  lda #$3f
  sta $2006
  lda #$00
  sta $2006

  ldx #0
- lda palette,x
  sta $2007
  inx
  cpx #$1f
  bne -

  ;Reset scroll
  lda #00
  sta $2005                 ;Reset X scroll
  sta $2005                 ;Reset Y scroll

  ;Copy sprite data to work RAM
  ldx #0
- lda OAM,x
  sta _OAM_WRAM,x
  inx
  cpx #$10
  bne -

  ;Copy sprite data from work RAM to OAM RAM
  lda #0                    ;OAM DMA destination address (low byte)
                            ;Using values other than 0 seems to lead to weird behavior. -_-'
  sta $2003
  lda #>_OAM_WRAM           ;OAM DMA source address high byte
  sta $4014

  ;Having completed setting up the PPU, it's time to enable video

- lda $2002                 ;Wait for another VBLANK
  bpl -                     ;Branch on PLus

  ;Enable/Start the PPU by writing a value
  lda #_PPU_CONF_01
  sta $2000
  lda #_PPU_CONF_02          ;Enable rendering
  sta $2001

loop:
  ;Update "base" sprites, based on user inputs
  lda _P1_INPUT
  tax                       ;Create a backup copy of player 1 input status
  and #_INPUT_MASK_DOWN      ;Mask out everything but the bit for the up input
  beq p1TestUp              ;Branch if EQual to zero, IE up is not pressed on the d-pad
  inc _OAM_P1_SPRITE_V      ;Otherwise, increment the Y value of P1 sprite
p1TestUp:
  txa
  and #_INPUT_MASK_UP
  beq p1TestRight
  dec _OAM_P1_SPRITE_V
p1TestRight:
  txa
  and #_INPUT_MASK_RIGHT
  beq p1TestLeft
  inc _OAM_P1_SPRITE_H
p1TestLeft:
  txa
  and #_INPUT_MASK_LEFT
  beq p1InputDone
  dec _OAM_P1_SPRITE_H
p1InputDone:

  ;**NOTE**
  ;Branching instructions have a range of 127 bytes. Putting too many instrction between BEQ, and the destination label will result in an "out of range" error.

  ;update "pair" sprites, IE the second half
  lda #<_OAM_WRAM
  sta $04
  lda #>_OAM_WRAM
  sta $05

  ldx #2
- ldy #0
  lda ($04),y
  ldy #4
  sta ($04),y

  ldy #3
  lda ($04),y
  ldy #7
  clc
  adc #8
  sta ($04),y

  lda $04
  clc
  adc #8
  sta $04

  dex
  bne -

  ;A counter to ensure the main loop is working properly
  ;IE the main loop is synched with VBlank
  clc
  lda $00
  adc #1
  sta $00

  lda $01
  adc #0
  sta $01

  stx $0b

  inc _LOOP_FLAG

- lda _LOOP_FLAG
  bne -

  jmp loop

nmi:
  pha                       ;Backup registers
  txa
  pha

  ;Update sprite data
  lda #0
  sta $2003
  lda #>_OAM_WRAM
  sta $4014

  ;Initialize both controller ports
  lda #1
  sta $4016
  sta $4017
  lda #0
  sta $4016
  sta $4017

  ;Use a simple loop to read input
  ldx #8
- lda $4016
  lsr a
  rol _P1_INPUT
  lda $4017
  lsr a
  rol _P2_INPUT
  dex
  bne -

  ;A counter to ensure the VBlank loop is working properly
  clc
  lda $08
  adc #1
  sta $08
  lda $09
  adc #0
  sta $09

  lda #0
  sta _LOOP_FLAG

  pla
  tax
  pla
  rti

irq:
  rti

palette:
.db $21,$0d,$00,$20 ,$00,$00,$00,$00 ,$00,$00,$00,$00 ,$00,$00,$00,$00
.db $21,$06,$25,$20 ,$00,$02,$21,$20 ,$00,$00,$00,$00 ,$00,$00,$00,$00

OAM:
  ;Sprite Y, tile index, attributes, X
.db     $40,        $02,  %00000000, $40
.db     $00,        $04,  %00000000, $00
.db     $60,        $02,  %00000001, $40
.db     $00,        $06,  %00000001, $00
                          ;|:|:|:|:
                          ;|:|:|:Palette
                          ;|:|xxx
                          ;|:Priority (0: in front of BG, 1: behind BG)
                          ;|Horizontal flip
                          ;Vertical flip

;The lowest bit of tile index is ignored in 8x16 mode. Instead, it's used to select banks, technically allowing 8x16 sprites to access 512 8x8 tiles

.pad $fffa                  ;Pad the rest of the file until $fffa 
.word nmi,reset,irq         ;Jump table for vBlank/NMI, reset, & IRQ

;Definitions for PRG-ROM

;Modified from previous examples
; - Enable 8x16 sprites
; - Sprite pattern table address set to $0000
; - Background pattern table address set to $0000

_PPU_CONF_01=%10111000
             ;|:|:|:|:
             ;|:|:|:base nametable address (00: $2000, 01: $2400, 10: $2800, 11: $2c00)
             ;|:|:|VRAM address increment per read/write of PPU data (0: 1 drawing rows, 1: 32, drawing columns)
             ;|:|:sprite pattern table address(0: $0000, 1: $1000), ignored in 8x16 mode?
             ;|:|background pattern table address(0: $0000, 1: $1000)
             ;|:enable 8x16 sprites
             ;|layer select (used in Nintendo Playchoice boards, should always be set to 0 for NES programming)
             ;generate NMI at the start of VBLANK

;Modified from previous examples
; - Sprites & BG enabled

_PPU_CONF_02=%00011110
             ;|:|:|:|:
             ;|:|:|:|Enable grayscale 
             ;|:|:|:Disable background clipping in leftmost 8 pixels of screen
             ;|:|:|Disable sprite clipping in leftmost 8 pixels of screen
             ;|:|:Enable backgrounds
             ;|:|Enable sprites
             ;|:Intensify reds
             ;|Intensify greens
             ;Intensify blues

_LOOP_FLAG         = $0f

_P1_INPUT          = $10
_P2_INPUT          = _P1_INPUT+4

_OAM_WRAM          = $0400
_OAM_P1_SPRITE_V   = _OAM_WRAM
_OAM_P1_SPRITE_H   = _OAM_P1_SPRITE_V+3

;Input masks to isolate specific button & direction
_INPUT_MASK_B      = %10000000
_INPUT_MASK_A      = %01000000
_INPUT_MASK_SELECT = %00100000
_INPUT_MASK_START  = %00010000
_INPUT_MASK_UP     = %00001000
_INPUT_MASK_DOWN   = %00000100
_INPUT_MASK_LEFT   = %00000010
_INPUT_MASK_RIGHT  = %00000001

;----------------------------------------------------
;CHR-ROM
;8 Kb
;----------------------------------------------------
.base $0000                 ;Reset program counter to $0000
.org $20                    ;$20 bytes for the first 8x16 blank tiles

;Second 8x16 tile, common to player 1 and player 2 tile
.db $ff,$9f,$bf,$ff,$f1,$f6,$f6,$f5,$00,$7f,$7f,$7f,$7f,$7f,$7f,$7f
.db $f3,$f7,$f7,$f7,$ff,$bf,$9f,$ff,$7f,$7f,$7f,$7f,$7f,$7f,$7f,$00

;8x16 "1" tile
.db $ff,$f9,$fd,$ff,$df,$9f,$df,$df,$00,$fe,$fe,$fe,$fe,$fe,$fe,$fe
.db $df,$df,$df,$8f,$ff,$fd,$f9,$ff,$fe,$fe,$fe,$fe,$fe,$fe,$fe,$00

;8x16 "2" tile
.db $ff,$f9,$fd,$ff,$9f,$af,$ef,$ef,$00,$fe,$fe,$fe,$fe,$fe,$fe,$fe
.db $df,$bf,$bf,$8f,$ff,$fd,$f9,$ff,$fe,$fe,$fe,$fe,$fe,$fe,$fe,$00

.org $1000                  ;Starting point for BG tiles

;8x8 "A" tile
.db $7f,$ff,$ef,$f7,$ff,$f7,$f7,$ff,$7f,$ef,$d7,$db,$c3,$db,$db,$ff

.org $2000                  ;Pad with $2000 bytes (8 Kb) of blank data
