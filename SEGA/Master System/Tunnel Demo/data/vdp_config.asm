;================================================================
;Rotating Tunnel Demo v2 - 2023.06.12
;For the SEGA Master System
;by Saad Azim
;
; Released under GNU General Public License v3.0
;================================================================

vdpConfig:                  ;Size $16
  ;According to the Mark III software reference manual, certain bits and registers should always be set to 1 and $FF, respectively.
  ;Reference: https://segaretro.org/File:SoftwareReferenceManualForSegaMarkIIIEU.pdf

  db %00110100,$80          ;Register 0, VDP mode register 1
     ;|:|:|:|:
     ;|:|:|:|Enable external video sync (not used)
     ;|:|:|:M3 - Must be set for M1/M2 to change screen height in mode 4, should always be set to 1
     ;|:|:|M4 - Use TMSS9918 Mode|Mode 4 (SMS), should always be set to 1
     ;|:|:ShifEnable external video sync (not used)t sprites left by 8 pixels
     ;|:|Line interrupt enable
     ;|:Mask column 0 with overscan color from register 7
     ;|Disable horizontal scrolling for rows 0-1
     ;Disable horizontal scrolling for columns 24-31

  db %11111111,$82          ;Reg 2, tile map address ($3800), tile map mirrorring disabled
     ;|:|:|:|:
     ;|:|:|:|Address bit 10 for TMS9918, tile map mirroring for SMS, should always be set to 1
     ;|:|:Address bits 13-11
     ;xxxx

     ;Possible addresses are $0000,$0800,$1000,$1800, $2000,$2800,$3000,$3800

  db $ff,$83                ;Reg 3, tile map base address for TMS9918a, should be $FF
  db $ff,$84                ;Reg 4, color table base address for TMS9918a, should be $FF

  db %11111111,$85          ;Reg 5, sprite attribute table base address ($3F00)
     ;|:|:|:|:
     ;|:|:|:|Mask bit, and should always be set to 1
     ;|Address bits 13-8
     ;x

  db %11111111,$86          ;Reg 6, sprite tile base address
     ;|:|:|:|:
     ;|:|:|:Mask bits, and should always be set to 11
     ;|:|:|Address bit 13 (0: $0000, 1: $2000)
     ;xxxxx

  db $ff,$87                ;Reg 7, overscan/border color, first nybble should always be $F
  db $00,$88                ;Reg 8, background X scroll

  db $00,$89                ;Reg 9, background Y scroll, wraps after $e0

  db $ff,$8a                ;Reg a, line counter ($00 amounts to generating an IRQ every single line)
  
enableRendering:
  ;Enable VDP *AFTER* configuring everything
  db %11100010,$81          ;Register 1, VDP mode register 2
     ;|:|:|:|:
     ;|:|:|:|2x sprite magnification (buggy)
     ;|:|:|:Sprites size (8x8|8x16)
     ;|:|:|x
     ;|:|:M2 - Selects 240 line screen if M2 is set to 1, valid for SMS2 only
     ;|:|M1 - Selects 224 line screen if M2 is set to 1, valid for SMS2 only
     ;|:Frame Interrupt disable||enable
     ;|Disable||Enable display
     ;Should always be set to 1
