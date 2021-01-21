;----------------------------------------------------
;GameBoy ASM Project 0.0 - 2020.10.29
;Compiling a working ROM
;by Saad Azim
;----------------------------------------------------

;Tested, and works in MAME and BGB.

;Specify the Gameboy CPU, which is a little different from the stock Z80 CPU
cpu gbz80

;ROM Header
  ds $0100-$,$00            ;Padding *until* $0100                                  $0000-$00FF

  nop                       ;Beginning of code execution point                      $0100-$0103
  jp CodeStart              ;Usually a NOP followed by JP...?

  ;Nintendo logo. GB won't start if this is altered                                 $0104-$0133
  db $ce,$ed,$66,$66,$cc,$0d,$00,$0b,$03,$73,$00,$83,$00,$0c,$00,$0d
  db $00,$08,$11,$1f,$88,$89,$00,$0e,$dc,$cc,$6e,$e6,$dd,$dd,$d9,$99
  db $bb,$bb,$67,$63,$6e,$0e,$ec,$cc,$dd,$dc,$99,$9f,$bb,$b9,$33,$3e

  db "GB INIT 0.0    "      ;Game Title 15 bytes                                    $0134-$0142
  
  ds $0150-$,$00            ;Padding until $0100                                    $0143-$014F

CodeStart:                                                              ;Start of code at $0150
  di                        ;Disable interrupts

  ;Invert the Nintendo logo colors, which uses colors 00 & 01
  ld a,%00000011            ;Load a color value into A
       ;|:|:|:|:
       ;|:|:|:Color 00 (fill/transparency)
       ;|:|:Color 01
       ;|:Color 02
       ;Color 03
  ld ($ff47),a              ;Store A to $FF47, the background color register

loop:
  jr loop                   ;Loop infinitely

  ds $8000-$,$00            ;Pad to 32k