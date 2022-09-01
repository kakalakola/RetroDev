;----------------------------------------------------
;NES INPUT HANDLING 0.1 - 2022.08.30
;(Reading input on the NES from both controllers)
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

  ;Point the PPU to PPU RAM at $3f00, the BG fill color
  lda #$3f
  sta $2006
  lda #$00
  sta $2006
  lda #_FILL_COLOR
  sta $2007                 ;Copy fill color to BG palette 0, color 0

  ;Reset scroll
  lda #00
  sta $2005                 ;Reset X scroll
  sta $2005                 ;Reset Y scroll

- lda $2002                 ;Wait for another VBLANK
  bpl -                     ;Branch on PLus

  ;Enable/Start the PPU by writing a value
  lda #_PPU_CONF_01
  sta $2000
  lda #_PPU_CONF_02          ;Enable rendering
  sta $2001

loop:

  stx $21
  ldx #$ab
  stx $20

  ;A counter to ensure the main loop is working properly
  ;IE the main loop is synched with VBlank
  clc
  lda $00
  adc #1
  sta $00

  lda $01
  adc #0
  sta $01

  inc _LOOP_FLAG

- lda _LOOP_FLAG
  bne -

  jmp loop

nmi:
  pha                       ;Backup accumulator
  txa                       ;Back up X register, by moving it to accumulator
  pha                       ;... and then backing up the accumulator again

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

  pla                       ;Restore X register, and Accumulator
  tax
  pla
  rti

irq:
  rti

.pad $fffa                  ;Pad the rest of the file until $fffa 
.word nmi,reset,irq         ;Jump table for vBlank/NMI, reset, & IRQ

;Definitions for PRG-ROM
;_FILL_COLOR=$0f              ;Black
;_FILL_COLOR=$2d              ;Dark grey
;_FILL_COLOR=$24              ;Magenta
_FILL_COLOR=$3a              ;Green

_PPU_CONF_01=%10000000
            ;|:|:|:|:
            ;|:|:|:base nametable address (00: $2000, 01: $2400, 10: $2800, 11: $2c00)
            ;|:|:|VRAM address increment per read/write of PPU data (0: 1 drawing rows, 1: 32, drawing columns)
            ;|:|:sprite pattern table address(0: $0000, 1: $1000)
            ;|:|background pattern table address(0: $0000, 1: $1000)
            ;|:enable 8x16 sprites
            ;|layer select (used in Nintendo Playchoice boards, should always be set to 0 for NES programming)
            ;generate NMI at the start of VBLANK

_PPU_CONF_02=%00001110
            ;|:|:|:|:
            ;|:|:|:|Enable grayscale 
            ;|:|:|:Disable background clipping in leftmost 8 pixels of screen
            ;|:|:|Disable sprite clipping in leftmost 8 pixels of screen
            ;|:|:Enable backgrounds
            ;|:|Enable sprites
            ;|:Intensify reds
            ;|Intensify greens
            ;Intensify blues

_LOOP_FLAG=$0f

_P1_INPUT=$10
_P2_INPUT=_P1_INPUT+4

;----------------------------------------------------
;CHR-ROM
;8 Kb
;----------------------------------------------------
.base $0000                 ;Reset program counter to $0000
.org $2000                  ;Pad with $2000 bytes (8 Kb) of blank data