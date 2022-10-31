;----------------------------------------------------
;NES INPUT HANDLING 0.3 - 2022.10.31
;(A better way to handle inputs)
;by Saad Azim
;----------------------------------------------------

;Uses slightly cleaner code

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
  ;Using X as the index of sprite data to modify, since INC only accepts X as an indirect offset
  ldx #0

  lda _P1_INPUT             ;Get player 1 input status
  and #_INPUT_MASK_DPAD     ;Mask out everything but the d-pad status, the value should be between 0-15
  tay                       ;Copy d-pad status to Y

  lda inputHandlerLo,y
  sta _INPUT_HANDLER_JUMP_ADDR_LO
  lda inputHandlerHi,y
  sta _INPUT_HANDLER_JUMP_ADDR_HI

  ;**Note**
  ;JMP can be used with indirect addressing IE JMP (pointerToAddress), but returning requires a specific target
  ;JSR requires absolute address IE JSR targetAddress, but once the target subrouting is done, it can be returned from with RTS. By setting up a manual "JMP targetAddress", and using JSR to get there (so to speak), it's possible to get the best of both

  lda #$4c
  sta _INPUT_HANDLER_JUMP_CMD
  jsr _INPUT_HANDLER_JUMP_CMD

  ;Process player 2 input
  lda _P2_INPUT
  and #_INPUT_MASK_DPAD
  tay

  ldx #8

  lda inputHandlerLo,y
  sta _INPUT_HANDLER_JUMP_ADDR_LO
  lda inputHandlerHi,y
  sta _INPUT_HANDLER_JUMP_ADDR_HI

  jsr _INPUT_HANDLER_JUMP_CMD

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

;Using costom subroutines to handle individual input (like up), or input combionation (down+left), requires a slightly more complicated setup, but the overall code is a little faster because it doesn't have to go through multiple AND and comparisons. The limited 127 byte range of using branching instructions like BEQ are also removed. 

doNothing:
  rts

moveSpriteDown:
  inc _OAM_WRAM,x
  rts
moveSpriteDownRight:
  inc _OAM_WRAM,x
  inc _OAM_WRAM+3,x
  rts
moveSpriteDownLeft:
  inc _OAM_WRAM,x
  dec _OAM_WRAM+3,x
  rts

moveSpriteUp:
  dec _OAM_WRAM,x
  rts
moveSpriteUpRight:
  dec _OAM_WRAM,x
  inc _OAM_WRAM+3,x
  rts
moveSpriteUpLeft:
  dec _OAM_WRAM,x
  dec _OAM_WRAM+3,x
  rts

moveSpriteRight:
  inc _OAM_WRAM+3,x
  rts
moveSpriteLeft:
  dec _OAM_WRAM+3,x
  rts

;Input handler table order
; - 0000 - nothing - doNothing
; - 0001 - right   - moveSpriteRight
; - 0010 - left    - moveSpriteLeft
; - 0011 - r+l     - doNothing
; - 0100 - up      - moveSpriteDown
; - 0101 - u+r     - moveSpriteDownRight
; - 0110 - u+l     - moveSpriteDownLeft
; - 0111 - u+r+l   - doNothing
; - 1000 - down    - moveSpriteUp
; - 1001 - d+r     - moveSpriteUpRight
; - 1010 - d+l     - moveSpriteUpLeft
; - 1011 - d+r+l   - doNothing
; - 1100 - d+u     - doNothing
; - 1101 - d+u+r   - doNothing
; - 1110 - d+u+l   - doNothing
; - 1111 - d+u+r+l - doNothing

inputHandlerLo:
;Input matrix
;  R/L not pressed  Right is pressed     Left is pressed     R+L is pressed
.db <doNothing     ,<moveSpriteRight    ,<moveSpriteLeft    ,<doNothing ;D/U is not pressed
.db <moveSpriteDown,<moveSpriteDownRight,<moveSpriteDownLeft,<doNothing ;Down is pressed
.db <moveSpriteUp  ,<moveSpriteUpRight  ,<moveSpriteUpLeft  ,<doNothing ;Up is pressed
.db <doNothing     ,<doNothing          ,<doNothing         ,<doNothing ;D+U is pressed

inputHandlerHi:
.db >doNothing     ,>moveSpriteRight    ,>moveSpriteLeft    ,>doNothing
.db >moveSpriteDown,>moveSpriteDownRight,>moveSpriteDownLeft,>doNothing
.db >moveSpriteUp  ,>moveSpriteUpRight  ,>moveSpriteUpLeft  ,>doNothing
.db >doNothing     ,>doNothing          ,>doNothing         ,>doNothing

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

_PPU_CONF_01=%10110000
             ;|:|:|:|:
             ;|:|:|:base nametable address (00: $2000, 01: $2400, 10: $2800, 11: $2c00)
             ;|:|:|VRAM address increment per read/write of PPU data (0: 1 drawing rows, 1: 32, drawing columns)
             ;|:|:sprite pattern table address(0: $0000, 1: $1000), ignored in 8x16 mode
             ;|:|background pattern table address(0: $0000, 1: $1000)
             ;|:enable 8x16 sprites
             ;|layer select (used in Nintendo Playchoice boards, should always be set to 0 for NES programming)
             ;generate NMI at the start of VBLANK

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

_LOOP_FLAG                  = $0f

_P1_INPUT                   = $10
_P2_INPUT                   = _P1_INPUT+4

_INPUT_HANDLER_JUMP_CMD     = _P2_INPUT+4
_INPUT_HANDLER_JUMP_ADDR_LO = _INPUT_HANDLER_JUMP_CMD+1
_INPUT_HANDLER_JUMP_ADDR_HI = _INPUT_HANDLER_JUMP_ADDR_LO+1

_OAM_WRAM                   = $0400

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
