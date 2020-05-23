;----------------------------------------------------
;SEGA Master System Bubsy Scroll Demo 0.1 - 2020.05.16
;Fun with HBlank
;by Saad Azim
;----------------------------------------------------

;This demo uses a modified version of the Bubsy emote created by Hungry Goriya
;You can check out her streams & videos at:
; - https://www.twitch.tv/hungrygoriya
; - https://www.youtube.com/c/hungrygoriya

;Compiled with tniASM
;Header calculated with I'm using SMSandGGHeaderReader
;Tested with, and works on MAME, and Fusion

;Specify the Z80 CPU, which is, hopefully, what powers the SMS
cpu z80

SCROLL_X_SPEED_LOW: equ $80
SCROLL_X_SPEED_HIGH: equ $00
SCROLL_Y_SPEED_LOW: equ $80
SCROLL_Y_SPEED_HIGH: equ $00

flagLoop: equ $c000

screenScroll: equ $c001     ;4 bytes, X scroll, X scroll register, Y scroll, Y scroll register

scrollXBase: equ $c005
scrollXLow: equ $c006
scrollYLow: equ $c007

wavePointer: equ $c00c      ;2 bytes
waveScroll: equ $c00e       ;2 bytes

waveTable: equ $c010

  ;Z80 Jump list                                        $0000-$00ff...?

  ;Reset code, for system power-on
  di                        ;Disable interrupts
  jp Reset
  ds $0038-$,$00            ;Pad from "here" ($0004) until $0038 with $00

  ;Interrupt handler, for HBlank, and VBlank
  jp IRQ
  ds $0066-$,$00

  ;NMI handler, for the Reset Button
  jp NMI
  ds $0100-$,$00

Reset:                 ;Self explanatory           $0100...
  ;Clear registers
  xor a                     ;Bitwise XOR on A with A, essentially resulting in $00...?
                            ;Resets Carry Negative-carry and Half-carry flag
                            ;Detects Parity/oVerflow flag
                            ;Zero/Sign flags are affected as defined
  ld bc,$0000
  ld de,$0000
  ld hl,$0000

  ;Clear shadow registers
  ex af,af'                 ;EXchange the contents of AF and AF'
  exx                       ;EXchange the contents of BC, DE & HL
                            ;with BC', DE' and HL'

  ;The registers don't need to be "cleaned" since the next immediate action is to change their values anyway


  ;Clear RAM with fancy block copy
  xor a
  ;ld a,$42                  ;For testing RAM & VRAM

  ld ($c000),a              ;Starting value

  ld hl,$c000
  ld de,$c001
  ld bc,$2000
  ldir

  ;Set  stack pointer
  ld sp,$dff0

  ld a,$00
  out ($bf),a               ;OUTput (write) the contentent of A to port $bf, VDP write port
  ld a,$40
  out ($bf),a               ;Point VDP write port to $4000 in VRAM

  ;Copy tile data to VRAM with block copy
  ld hl,tiles               ;Set source to the location in tiles
  ld b,$40                  ;Set amount to be copied ($20 per tile)
  ld c,$be                  ;Set destination port
  otir                      ;Block copy $40 bytes of data to VRAM
  otir                      ;Block copy $100 more bytes of data to VRAM
  otir                      ;Block copy $100 more bytes of data for a total of $240 bytes

  ;Block copy tile map to VRAM ($1800), though the write port *needs* to be set to $17ff for ... ressons. -_-'
  ld a,$ff
  out ($bf),a
  ld a,$37
  out ($bf),a

  ld hl,nameTable
  ld b,$00
  ld c,$be
  otir

  otir                      ;OTIR can be called a second time to copy an additional $100 bytes
  otir
  otir
  otir
  otir
  otir

  ;**NOTE**
  ;The tile map is $0700 bytes in size. This leaves $3f00 free to be used for sprite attribute data.

  ;Copy BG colors to VRAM with block copy
  ld a,$00
  out ($bf),a
  ld a,$c0
  out ($bf),a

  ld hl,bgColor
  ld b,$20
  ld c,$be
  otir

  ;Copy wave table to RAM for better control
  ld hl,waveOffset
  ld de,waveTable
  ld bc,$40
  ldir
  ld hl,waveTable           ;At this point, DE is pointing to the right address
                            ;So all that needs to be done are to set HL, and BC. The $40 bytes of data at waveOffset gets copied a total of 3 times. Having a table with $C0 bytes of data saves a little bit of calculation during HBlank IRQ
  ld bc,$80
  ldir

  ;Set a pointer to the wave table in RAM
  ld hl,waveTable
  ld (wavePointer),hl

  ;Configure screen scroll register number. Makes it easy to copy things with a block copy
  ld a,$88
  ld (screenScroll+1),a
  ld (waveScroll+1),a
  inc a
  ld (screenScroll+3),a

  ;Configure VDP
  ld hl,vdpInit
  ld b,$16
  ld c,$bf
  otir

  im 1                      ;Interrupt mode 1, jumps to $0038 when an interrupt is called
  ei                        ;Enable interrupts

;Main loop with counter
loop:

  ld a,(flagLoop)
  cp 0
  jp nz,loop

  xor a

  ld a,(scrollXLow)
  add a,SCROLL_X_SPEED_LOW
  ld (scrollXLow),a
  ld a,(screenScroll)
  adc SCROLL_X_SPEED_HIGH
  ld (screenScroll),a
  ld (waveScroll),a
  ld (scrollXBase),a

  xor a                     ;Clear the carry flag
  ld a,(scrollYLow)
  add a,SCROLL_Y_SPEED_LOW
  ld (scrollYLow),a
  ld a,(screenScroll+2)
  adc a,SCROLL_Y_SPEED_HIGH

  cp $e0
  jp c,storeYScroll
  sbc a,$e0

storeYScroll:
  ld (screenScroll+2),a

  ld a,$10
  ld (wavePointer),a

  ;halt                      ;Wait for interrupt
  ld a,1
  ld (flagLoop),a

  jp loop

;VInt/HInt
IRQ:

  exx
  ex af,af'                 ;EXchange the contents of AF and AF'

  in a,($bf)                ;Acknowledge interrupt
  bit 7,a
  jp nz,noHBlank            ;Check to see if this is an HBlank IRQ or a VBlank IRQ

  ld a,(waveScroll)         ;Update scroll info
  out ($bf),a
  ld a,$88
  out ($bf),a

  ;Having updated the scroll info, it's time to calculate the next X scroll value
  xor a                     ;Clear A & all flags, just to be safe

  ;Increments to wavePointer needs to be tied to values from the line counter register $7E. The line counter seems to randomly skip a few number every frame, i.e. jumping from $BE to $C0. Somehow this ... works, for reason's I'm not entirely sure of.

  ;Manually incrementing the wave pointer by adding 1 to a counter seems to cause visible graphical glitches.

  in a,($7e)                ;Get current line from register $7E
  add $10                   ;Add $10, since the wave table starts at $c010
  ld (wavePointer),a        ;Store the updated pointer

  ;Manually incrementing the wave pointer by adding 1 to a counter seems to cause visible graphical glitches. Most noticeable when X & Y scroll values are set to 0
  ;ld a,(wavePointer)
  ;add 1
  ;ld (wavePointer),a

  ld hl,(wavePointer)       ;Load the address stored in wavePointer
  ld b,(hl)                 ;Load the data stored at the address stored in wavePointer
  ld a,(scrollXBase)        ;Load the base X scroll data
  add a,b                   ;Add A & B
  ld (waveScroll),a         ;Store the updates X scroll data to waveScroll for the next update

  jp irqEnd

noHBlank:

  ;Copy screen scroll with block copy
  ld hl,screenScroll
  ld b,$4
  ld c,$bf
  otir

  xor a
  ld (flagLoop),a           ;Reset flagLoop

irqEnd:

  ex af,af'
  exx
  ei                        ;Enable interrupts, which *should* be disabled automatically per interrupt
  reti                      ;Return from interrupt

;NMI handler
NMI:
  retn                      ;RETurn from Non-maskable interrupt

;The wave offest is horizontal only, since the Master System doesn't allow vertical scrolling during line interrupts

waveOffset:
  db $00,$01,$02,$03,$03,$03,$04,$05,$05,$05,$05,$06,$06,$06,$06,$06
  db $06,$06,$06,$06,$06,$05,$05,$05,$05,$04,$03,$03,$03,$02,$01,$00
  db $00,$ff,$fe,$fd,$fd,$fd,$fd,$fc,$fc,$fc,$fc,$fb,$fb,$fb,$fb,$fb
  db $fb,$fb,$fb,$fb,$fb,$fc,$fc,$fc,$fc,$fd,$fd,$fd,$fd,$fe,$ff,$00

bgColor:                    ;Size $20
  ;db %00000011
  ;   ;|:|:|:|:
  ;   ;|:|:|:Red
  ;   ;|:|:Green
  ;   ;|:Blue
  ;   ;xx
  ;Now defined in easier to manage blocks of BG/Sprite palettes
  db $3f,$3f,$3a,$2f,$2b,$16,$07,$03,$02,$10,$01,$00,$04,$00,$00,$00
  db $00,$12,$13,$14,$15,$16,$17,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,$20

tiles:
  db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
  db $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
  db $23,$01,$00,$23,$dc,$20,$20,$df,$ff,$ff,$00,$38,$ff,$ff,$00,$38
  db $fe,$03,$fd,$00,$fe,$03,$fd,$00,$fc,$ff,$02,$00,$00,$ff,$f8,$04
  db $7f,$90,$00,$10,$ff,$93,$00,$93,$7c,$97,$00,$97,$78,$9a,$00,$9f
  db $70,$96,$80,$1f,$70,$9d,$8d,$12,$70,$90,$00,$1f,$d0,$31,$01,$1e
  db $f7,$39,$00,$31,$7b,$4d,$00,$c9,$3b,$3c,$00,$f8,$05,$ee,$c0,$3c
  db $02,$ef,$c4,$3a,$01,$cf,$ce,$31,$cb,$db,$10,$ef,$ef,$11,$10,$ef
  db $02,$c4,$c4,$3a,$bf,$fe,$40,$81,$df,$df,$00,$e0,$ef,$7f,$10,$60
  db $ef,$3f,$10,$20,$67,$be,$19,$20,$73,$9f,$0c,$10,$a0,$df,$03,$9c
  db $07,$f8,$00,$f8,$ff,$00,$00,$00,$ff,$0e,$0e,$00,$ff,$1f,$1f,$00
  db $ff,$1f,$1f,$00,$ff,$1f,$1f,$00,$ff,$0f,$0f,$00,$ff,$07,$07,$00
  db $a1,$62,$02,$3d,$41,$c2,$02,$7d,$c1,$62,$22,$5d,$bf,$7f,$00,$20
  db $bf,$7f,$00,$30,$d7,$bf,$88,$10,$cf,$bf,$80,$08,$eb,$9f,$84,$08
  db $11,$00,$00,$11,$55,$44,$00,$55,$11,$00,$00,$11,$ee,$10,$10,$ef
  db $ff,$ff,$00,$1c,$ff,$ff,$00,$1c,$ff,$81,$7e,$00,$ff,$01,$fe,$00
  db $b8,$c7,$00,$87,$bf,$c0,$00,$80,$cf,$f0,$00,$c0,$4f,$70,$00,$c0
  db $af,$f0,$00,$60,$af,$f0,$00,$60,$2f,$f0,$c0,$20,$2f,$f0,$c0,$20
  db $ff,$01,$01,$00,$ff,$00,$00,$00,$ff,$00,$00,$00,$ff,$01,$00,$01
  db $fe,$07,$01,$06,$fa,$0b,$05,$0a,$f8,$0b,$07,$08,$f4,$94,$03,$9c
  db $f9,$bf,$86,$38,$c8,$7f,$31,$4e,$8c,$cb,$70,$8b,$1f,$98,$e0,$18
  db $0f,$08,$f0,$08,$0f,$08,$f0,$08,$17,$78,$e0,$10,$37,$78,$c0,$30
  db $fe,$ff,$01,$0e,$32,$f3,$cc,$32,$47,$e6,$38,$c6,$81,$c3,$7e,$81
  db $87,$c7,$78,$87,$86,$cf,$7b,$84,$82,$ce,$7d,$82,$80,$c8,$7f,$80
  db $2f,$f0,$00,$20,$6f,$b0,$00,$20,$ef,$30,$00,$20,$af,$70,$00,$20
  db $ef,$30,$00,$20,$ef,$b0,$00,$a0,$df,$b8,$00,$98,$ee,$de,$40,$8f
  db $62,$72,$01,$fe,$02,$df,$8d,$72,$02,$df,$8d,$72,$03,$9e,$9c,$62
  db $96,$b7,$20,$de,$de,$23,$20,$de,$23,$01,$00,$23,$ab,$89,$00,$ab
  db $37,$38,$c0,$30,$37,$f8,$c0,$30,$37,$f8,$c0,$30,$f7,$38,$00,$30
  db $f7,$38,$00,$30,$f7,$18,$00,$10,$f7,$18,$00,$10,$f7,$18,$00,$10
  db $81,$c1,$7e,$81,$c3,$7f,$3d,$42,$c3,$7e,$3c,$42,$df,$62,$00,$42
  db $df,$62,$00,$42,$df,$62,$00,$42,$df,$63,$00,$43,$cf,$71,$00,$41
  db $e8,$de,$c0,$0f,$f0,$d5,$c1,$1e,$e0,$ed,$c1,$3e,$e0,$3b,$1b,$24
  db $c1,$41,$00,$7f,$81,$82,$02,$fd,$82,$84,$04,$fa,$82,$84,$04,$fa

nameTable:
  ;Row 01-04
  db 1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0
  db 1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0
  db 5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0
  db 5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0
  db 9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0
  db 9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0
  db 13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0
  db 13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0
  ;Row 05-08
  db 1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0
  db 1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0
  db 5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0
  db 5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0
  db 9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0
  db 9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0
  db 13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0
  db 13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0
  ;Row 09-12
  db 1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0
  db 1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0
  db 5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0
  db 5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0
  db 9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0
  db 9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0
  db 13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0
  db 13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0
  ;Row 13-16
  db 1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0
  db 1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0
  db 5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0
  db 5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0
  db 9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0
  db 9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0
  db 13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0
  db 13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0
  ;Row 13-16
  db 1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0
  db 1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0
  db 5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0
  db 5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0
  db 9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0
  db 9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0
  db 13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0
  db 13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0
  ;Row 17-20
  db 1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0
  db 1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0
  db 5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0
  db 5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0
  db 9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0
  db 9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0
  db 13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0
  db 13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0
  ;Row 20-24
  db 1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0
  db 1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0,1,0,2,0,3,0,4,0
  db 5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0
  db 5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0,5,0,6,0,7,0,8,0
  db 9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0
  db 9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0,9,0,10,0,11,0,12,0
  db 13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0
  db 13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0,13,0,14,0,15,0,16,0



vdpInit:                    ;Size $16
  db %00110110,$80          ;Register 0, VDP mode register 1
     ;|:|:|:|:
     ;|:|:|:|Normal||Monochrome display, should always be set to 0
     ;|:|:|:M2 - Must be set for M1/M3 to change screen height in mode 4, should always be set to 1
     ;|:|:|M4 - Use TMSS9918 Mode||Mode 4 (SMS), should always be set to 1
     ;|:|:Shift sprites left by 8 pixels
     ;|:|Line interrupt disable||enable
     ;|:Mask column 0 with overscan color from register 7
     ;|Disable horizontal scrolling for rows 0-1
     ;Disable horizontal scrolling for columns 24-31

  db %00001111,$82          ;Reg 2, tile map address ($3800), tile map mirrorring disabled
     ;|:|:|:|:
     ;|:|:|:|Address bit 10 for TMS9918, tile map mirroring SMS should always be set to 1
     ;|:|:Address bits 13-11
     ;xxxx

     ;Possible addresses are $0000,$0800,$1000,$1800, $2000,$2800,$3000,$3800

  db $ff,$83                ;Reg 3, tile map base address for TMS9918a, should be $ff
  db $ff,$84                ;Reg 4, color table base address for TMS9918a, should be $ff

  db %01111111,$85          ;Reg 5, sprite attribute table base address ($3f00)
     ;|:|:|:|:
     ;|:|:|:|Mask bit, and should always be set to 1
     ;|Address bits 13-8
     ;x

  db %00000011,$86          ;Reg 6, sprite tile base address
     ;|:|:|:|:
     ;|:|:|:Mask bits, and should be set for SMS
     ;|:|:|Address bit 13 (0: $0000, 1: $2000)
     ;xxxxx

  db $00,$87                ;Reg 7, overscan/border color
  db $00,$88                ;Reg 8, background X scroll
  db $00,$89                ;Reg 9, background Y scroll, wraps after $e0
  db $00,$8a                ;Reg a, line counter ($00 amounts to generating an IRQ every single line)
  
  ;Enable VDP *AFTER* configuring everything
  db %11100000,$81          ;Register 1, VDP mode register 2
     ;|:|:|:|:
     ;|:|:|:|2x sprite pixels
     ;|:|:|:Sprites are 8x8||16x16
     ;|:|:|x
     ;|:|:M3 - Selects 240 line screen if M2 is set to 1, valid for SMS2 only
     ;|:|M1 - Selects 224 line screen if M2 is set to 1, valid for SMS2 only
     ;|:Frame Interrupt disable||enable
     ;|Disable||Enable display
     ;Should always be set to 1, according to SEGA

  ;Information used by SDSC header. Needs to be zero-terminated ASCII string
authorInfo:
  db "Saad Azim",$00
nameInfo:
  db "SMS Bubsy Scroll Demo 01",$00
descriptionInfo:
  db "A wavy background scrolling demo for the Master System, featuring the Bubsy emote by Hungry Goriya",$00

  ds $7fe0-$,$00            ;Pad to 32k ($7fe0)

;Homebrew SDSC ROM header
  db "SDSC"                 ;SDSC in ASCII, to imply presense of an SDSC header
  db $01,$00                ;Software version, in binary coded decimal (01.00)
  db $23,$05,$20,$20        ;Date of release/recompilation, in BCD DD MM yyYY (22 05 2020)
  dw authorInfo             ;Pointer to author name
  dw nameInfo               ;Pointer to software name
  dw descriptionInfo        ;Pointer to software description

;Header can be offset at $1ff0, $3ff0, or $7ff0

  db "TMR SEGA"       ;Required for Export SMS & GG       $7ff0-$7ff7
  db $00,$00          ;Reserved space, can be $00||$20    $7ff8-$7ff9
  dw $cd16            ;Checksum for Export (US?) MS Bios  $7ffa-$7ffb

  db $00,$00,$00      ;BCD product & version code         $7ffc-7ffe
                      ;$27,$50,$10 results in product code 15027, version 0
                      ;(Bits 7-4) in byte 3 is high number of product code
                      ;(Bits 3-0) in byte 3 is version number

  db $4c              ;Region & rom size (SMS Exp, 32KB)  $7fff
                      ;Bits 7-4 used for region
                      ;(3:SMS Japan||4:SMS Export||5:GG Japan||6:GG Export||7:GG International)
                      ;Bits 3-0 used for cart size
                      ;($c:32KB||$e:64KB -rarely used-||$f:128KB||0:256KB||1:512KB -rarely used-)
