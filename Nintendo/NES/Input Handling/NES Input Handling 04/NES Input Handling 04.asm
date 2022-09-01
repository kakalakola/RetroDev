;----------------------------------------------------
;NES INPUT HANDLING 0.4 - 2022.08.31
;(Motion vectors and soft resets)
;by Saad Azim
;----------------------------------------------------

;Uses slightly cleaner code

;iNES header 2.0, 16 bytes                                               (Byte $00-$03)
.db "NES",$1a,$34,$01,$01,$08,$00,$0f,$00,$00,$00,$00,$00,$00
; - 1 PRG-ROM bank (8 Kb), 1 CHR-ROM (8 Kb)
; - Horizontal mirrorring, no mappers, mirrorring, SRAM, trainer, etc
; - No PRG-RAM, no CHR-RAM
; - NTSC timing
; - No expansion device present/used

;----------------------------------------------------
;PRG-ROM
;$e000-$ffff - 8 Kb
;----------------------------------------------------
.org $e000

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

;  lda #_FILL_COLOR
  sta $2007                 ;Copy fill color to BG palette 0, color 0

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

  ;Soft reset by pressing A+B+Start+Select on either controller
  lda _P1_INPUT             ;Load controller status
  and #$f0                  ;Mask out d-pad input
  cmp #$f0                  ;Check to see if A, B, Start, and Select are simultaneously pressed
  beq softReset             ;If so, jump to softReset

  lda _P2_INPUT
  and #$f0
  cmp #$f0
  bne testInput

softReset:
  ldx #0
- lda OAM,x
  sta _OAM_WRAM,x
  inx
  cpx #$10
  bne -
  jmp skipInput

testInput:
  lda _P1_INPUT             ;Get player 1 input status
  ldy #0
  jsr moveSprite

  lda _P2_INPUT
  ldy #8
  jsr moveSprite

skipInput:

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
  pha
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
  clc
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

;Using a 16-bit vector table, with the d-pad state as the index to move sprite. The low byte works as a faction, while the high byte is used for on screen position. It's a bit more complicated, but the results are more accurate. Especially when dealing with diagonal movement. :)

moveSprite:
  and #_INPUT_MASK_DPAD
  tax

  ;Process low byte of sprite Y vector
  lda spriteVectorVLo,x
  sta _SPRITE_POS_TMP
  clc
  lda _OAM_SPRITE_POS_LO,y
  adc _SPRITE_POS_TMP
  sta _OAM_SPRITE_POS_LO,y

  ;Process high byte of sprite Y vector
  lda spriteVectorVHi,x
  sta _SPRITE_POS_TMP
  lda _OAM_WRAM,y
  adc _SPRITE_POS_TMP
  sta _OAM_WRAM,y

  ;Process sprite X vector
  lda spriteVectorHLo,x
  sta _SPRITE_POS_TMP
  clc
  lda _OAM_SPRITE_POS_LO+3,y
  adc _SPRITE_POS_TMP
  sta _OAM_SPRITE_POS_LO+3,y

  lda spriteVectorHHi,x
  sta _SPRITE_POS_TMP
  lda _OAM_WRAM+3,y
  adc _SPRITE_POS_TMP
  sta _OAM_WRAM+3,y
  rts

;**NOTE**
;$0100 ==  1
;$ff00 == -1
;$00b5 == cos(2)
;$ff4b == -cos(2)

spriteVectorVLo:
;   N/A   R   L N/A    D D+R D+L N/A    U U+R U+L N/A    N/A N/A N/A N/A
.db $00,$00,$00,$00 ,$00,$b5,$b5,$00  ,$00,$4b,$4b,$00  ,$00,$00,$00,$00
spriteVectorVHi:
.db $00,$00,$00,$00 ,$01,$00,$00,$00  ,$ff,$ff,$ff,$00  ,$00,$00,$00,$00

spriteVectorHLo:
.db $00,$00,$00,$00 ,$00,$b5,$4b,$00  ,$00,$b5,$4b,$00  ,$00,$00,$00,$00
spriteVectorHHi:
.db $00,$01,$ff,$00 ,$00,$00,$ff,$00  ,$00,$00,$ff,$00  ,$00,$00,$00,$00

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
;_FILL_COLOR=$0f              ;Black
;_FILL_COLOR=$2d              ;Dark grey
;_FILL_COLOR=$24              ;Magenta
_FILL_COLOR=$3a              ;Green

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
_INPUT_HANDLER_JUMP_CMD     = _P2_INPUT+4
_INPUT_HANDLER_JUMP_ADDR_LO = _INPUT_HANDLER_JUMP_CMD+1
_INPUT_HANDLER_JUMP_ADDR_HI = _INPUT_HANDLER_JUMP_ADDR_LO+1

_OAM_WRAM          = $0400
_OAM_P1_SPRITE_V   = _OAM_WRAM
_OAM_P1_SPRITE_H   = _OAM_P1_SPRITE_V+3
_OAM_P2_SPRITE_V   = _OAM_P1_SPRITE_H+5
_OAM_P2_SPRITE_H   = _OAM_P2_SPRITE_V+3

;A new block of RAM to hold the low byte of sprite position, that works as a fraction
_OAM_SPRITE_POS_LO = _OAM_WRAM-$10
_SPRITE_POS_TMP = $18

;A single mask to check the input status
_INPUT_MASK_DPAD   = %00001111

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