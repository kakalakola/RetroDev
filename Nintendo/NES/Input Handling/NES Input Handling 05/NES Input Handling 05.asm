;----------------------------------------------------
;NES INPUT HANDLING 0.5 - 2022.10.31
;(Working with the NES Four Score)
;by Saad Azim
;----------------------------------------------------

;Using the NES Four Score™

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
  cpx #$20
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
  ;Modified soft reset to check for Start+Select

  ;Reset code for player 1 and player 2 inputs are straight forward
  lda _P1_INPUT
  and #$30
  cmp #$30
  beq softReset

  lda _P2_INPUT
  and #$30
  cmp #$30
  beq softReset

  lda _P3_INPUT
  cmp #$ff                  ;Additional test needed for controllers 3 & 4
                            ;Without a multitap, they'll read $FF, which, in turn will always trigger the soft reset
  beq checkController4
  and #$30
  cmp #$30
  beq softReset

checkController4:
  lda _P4_INPUT
  cmp #$ff
  beq testInput
  and #$30
  cmp #$30
  bne testInput

softReset:
  ldx #0
- lda OAM,x
  sta _OAM_WRAM,x
  inx
  cpx #$20
  bne -
  jmp skipInput

testInput:
  lda _P1_INPUT             ;Get player 1 input status
  ldy #0
  jsr moveSprite

  lda _P2_INPUT
  ldy #8
  jsr moveSprite

  lda _P3_INPUT
  ldy #$10
  jsr moveSprite

  lda _P4_INPUT
  ldy #$18
  jsr moveSprite

skipInput:
  ;update "pair" sprites, IE the second half
  lda #<_OAM_WRAM
  sta $04
  lda #>_OAM_WRAM
  sta $05

  ldx #4
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
  pha                       ;Backup all the registers
  txa
  pha
  tya
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

  ;Use a 16-bit nested loop, to read input data
  ldx #0
-- ldy #8
- lda $4016
  lsr a
  rol _P1_INPUT,x
  lda $4017
  lsr a
  rol _P2_INPUT,x
  dey
  bne -
  inx
  inx
  cpx #6
  bne --


  ;If _MULTITAP_ID_0 == 0, then controller 1 is not connected
  ;If _MULTITAP_ID_1 == 0, then controller 2 is not connected

  ;If _P3_INPUT == $FF and _MULTITAP_ID_0 == $FF, then controller 1 is connected
  ;If _P4_INPUT == $FF and _MULTITAP_ID_1 == $FF, then controller 2 is connected
  ;If _P4_INPUT == $00 and _MULTITAP_ID_1 == $FF, then the Zapper is connected on controller port 2

  ;If _MULTITAP_ID_0 == $10 and _MULTITAP_ID_1 == $20
  ;An NES Four Score is connected

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
  tay
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
.db $21,$06,$02,$20 ,$00,$0b,$04,$20 ,$00,$00,$00,$00 ,$00,$00,$00,$00

OAM:
  ;Sprite Y, tile index, attributes, X
.db     $40,        $02,  %00000000, $40
.db     $00,        $04,  %00000000, $00
.db     $60,        $06,  %00000000, $40
.db     $00,        $08,  %00000000, $00
.db     $80,        $0a,  %00000001, $40
.db     $00,        $0c,  %00000001, $00
.db     $a0,        $0e,  %00000001, $40
.db     $00,        $10,  %00000001, $00
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

;Modified code, placed the player input and multitap ID bytes closer

_P1_INPUT          = $10
_P2_INPUT          = _P1_INPUT+1
_P3_INPUT          = _P2_INPUT+1
_P4_INPUT          = _P3_INPUT+1

_MULTITAP_ID_0     =_P4_INPUT+1
_MULTITAP_ID_1     =_MULTITAP_ID_0+1

_INPUT_HANDLER_JUMP_CMD     = _MULTITAP_ID_1+4
_INPUT_HANDLER_JUMP_ADDR_LO = _INPUT_HANDLER_JUMP_CMD+1
_INPUT_HANDLER_JUMP_ADDR_HI = _INPUT_HANDLER_JUMP_ADDR_LO+1

_OAM_WRAM          = $0400

;A new block of RAM to hold the low byte of sprite position, that works as a fraction
;Modified _OAM_SPRITE_POS_LO, to allow for a bit more space
_OAM_SPRITE_POS_LO = _OAM_WRAM-$40
_SPRITE_POS_TMP = $20

;A single mask to check the input status
_INPUT_MASK_DPAD   = %00001111

;----------------------------------------------------
;CHR-ROM
;8 Kb
;----------------------------------------------------
.base $0000                 ;Reset program counter to $0000
.org $20                    ;$20 bytes for the first 8x16 blank tiles

.db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$00,$3f,$7f,$7f,$71,$76,$76,$75
.db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$73,$77,$77,$77,$7f,$7f,$3f,$00
.db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$00,$fc,$fe,$fe,$de,$9e,$de,$de
.db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$de,$de,$de,$8e,$fe,$fe,$fc,$00

.db $00,$3f,$7f,$7f,$71,$76,$76,$75,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.db $73,$77,$77,$77,$7f,$7f,$3f,$00,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.db $00,$fc,$fe,$fe,$9e,$ae,$ee,$ee,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.db $de,$be,$be,$8e,$fe,$fe,$fc,$00,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff

.db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$00,$3f,$7f,$7f,$71,$76,$76,$75
.db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$73,$77,$77,$77,$7f,$7f,$3f,$00
.db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$00,$fc,$fe,$fe,$9e,$ee,$ee,$de
.db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ee,$ee,$ee,$9e,$fe,$fe,$fc,$00

.db $00,$3f,$7f,$7f,$71,$76,$76,$75,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.db $73,$77,$77,$77,$7f,$7f,$3f,$00,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.db $00,$fc,$fe,$fe,$ee,$ce,$ae,$8e,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
.db $ee,$ee,$ee,$ee,$fe,$fe,$fc,$00,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff

.org $1000                  ;Starting point for BG tiles

;8x8 "A" tile
.db $7f,$ff,$ef,$f7,$ff,$f7,$f7,$ff,$7f,$ef,$d7,$db,$c3,$db,$db,$ff

.org $2000                  ;Pad with $2000 bytes (8 Kb) of blank data
