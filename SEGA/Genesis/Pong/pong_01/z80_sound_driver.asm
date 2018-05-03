;----------------------------------------------------
;GENESIS ASM: Pong 0.1
;Z80 Sound Driver
;by Saad Azim
;----------------------------------------------------
;  org $0000

;Format for defining variables in tniasm.exe
;For the time being $0800 will be the start of the work RAM
_ramFlagRunOnce: equ $0800
_ramAudioFlag: equ $0801
_ramFrameCounter: equ $080f
_ramCounterTestLoop: equ $0810
_ramCounterTestFrame: equ $0812
_ramCounterTestVBlank: equ $0814

;;org $0000

;Until I know better, all (attempted) reverse engineered initialization routines are OUT...
  di                        ;Disable interrupts
  jp code                   ;Jump to code, leaving an area for
  ds $0038-$,$00            ;Pad from "here" ($) until $0038 (interrupt vector for vblank) with $00
  jp vblank                 ;Interrupt vectors need to be prefaced with a JP command
  ds $0100-$,$00            ;Pad from "here" ($) until $0100 with $00

code:                       ;Starting from $0100
  xor a                     ;Bitwise XOR on A with A, essentially resulting in $00...?
                            ;Resets Carry Negative-carry and Half-carry flag
                            ;Detects Parity/oVerflow flag
                            ;Zero/Sign flags are affected as defined
  ld bc,$0000
  ld de,$0000
  ld hl,$0000

  exx                       ;EXchange the contents of BC, DE & HL with BC', DE' and HL'
  
  ;Set up stack...?
  ld sp,$1000

  ld ix,$6000               ;Load the address of the bank register to IX
  xor a                     ;A==0
  ld (ix),a                 ;Store bit 15 to bank register, 0
  inc a                     ;A==1
  ld (ix),a                 ;Store bit 16 to bank register, 1
  ld (ix),a                 ;Store bit 17 to bank register, 1
  ld (ix),a                 ;Store bit 18 to bank register, 1
  ld (ix),a                 ;Store bit 19 to bank register, 1
  ld (ix),a                 ;Store bit 20 to bank register, 1
  ld (ix),a                 ;Store bit 21 to bank register, 1
  ld (ix),a                 ;Store bit 22 to bank register, 1
  ld (ix),a                 ;Store bit 23 to bank register, 1

  ;Z80 $8000-$ffff is now mapped to 68000 $ff0000-$ff7fff

  ex af,af'                 ;EXchange the contents of AF and AF'
  exx                       ;EXchange the contents of BC, DE & HL with BC', DE' and HL'

  ld a,(vblank)
  ld i,a

  ei                        ;Enable Interrupts
  im 1                      ;Interrupt Mode 1

;;  ld a,$ff
;;  ld (_ramFrameCounter),a


mainLoop:                   ;A simple loop to see if things are working
  ld hl,_ramCounterTestLoop
  inc (hl)

  ;Test _ramFlagRunOnce
  xor a                     ;A==0
  ld hl,_ramFlagRunOnce     ;Load _ramFlagRunOnce to HL
  cp (hl)                   ;Compare the value of _ramFlagRunOnce
  jr nz,mainLoop            ;Jump Relative on Not Zero, to mainLoop (shorter bytecode than jp)

  ;Otherwise...
  ld hl,_ramFlagRunOnce     ;Load _ramFlagRunOnce to HL
  inc (hl)                  ;Increment _ramFlagRunOnce

;;  ld a,(_ramAudioFlag)
;;
;;  cp 2
;;  jp z,playSFX01

;;  cp 4
;;  jp z,playSFX02


noSFX:



  nop
  nop
returnToMainLoop:

  ld hl,_ramCounterTestFrame
  inc (hl)
  jp mainLoop


vBlank:
  di                        ;Disable interrupts
  ex af,af'                 ;EXchange the contents of AF and AF'
  exx                       ;EXchange the contents of BC, DE & HL with BC', DE' and HL'

  ;A simple counter in RAM
  ld hl,_ramCounterTestVBlank
  inc (hl)

  ;Explicitly update the PSG during VBlank, when the 68K doesn't have bus...?
  
  ld a,(_ramAudioFlag)
  cp 2
  jp z,playSFX01
;;  cp 4
;;  jp z,playSFX02

  xor a
  ld hl,_ramFrameCounter
  cp (hl)
  jp z,muteSFX
  dec (hl)

sfxDone:
  xor a                     ;A'=0
  ld (_ramFlagRunOnce),a    ;Reset _ramFlagRunOnce
  ex af,af'                 ;EXchange the contents of AF and AF'
  exx                       ;EXchange the contents of BC, DE & HL with BC', DE' and HL'
  ei                        ;Enable Interrupts, again
  ;im 1                      ;Interrupt mode 1
  reti

irq:                         ;IRQ
  reti

;Subroutines
playSFX01:                  ;Score, duration 8 frames
  ;Activating the Square 1 Channel on the PSG
  ;Set the tone (now with indexed addressing)
  ld a,(channel0Tone0)
  ld ($7f11),a
  ld a,(channel0Tone0+1)
  ld ($7f11),a
  ;Set the volume
  ld a,(channel0Tone0+2)
  ld ($7f11),a

  ld a,$00                   ;Load duration of note
  ld (_ramFrameCounter),a   ;Store duration of note to _ramFrameCounter

  xor a                     ;A==0
  ld (_ramAudioFlag),a      ;Reset _ramAudioFlag
  
  jp sfxDone

playSFX02:                  ;Bounce
  ld a,(channel0Tone1)      ;Set note
  ld ($7f11),a
  ld a,(channel0Tone1+1)
  ld ($7f11),a
  ld a,(channel0Tone1+2)
  ld ($7f11),a
  ld a,$2                   ;Load duration of note
  ld (_ramFrameCounter),a
  xor a                     ;A==0
  ld (_ramAudioFlag),a      ;Reset _ramAudioFlag
  jp sfxDone

muteSFX:                    ;Mute SFX channel (s)
  ld a,%10000000
  ld ($7f11),a
  ld a,%00000000
  ld ($7f11),a
  ld a,%10000000
  ld ($7f11),a
  jp sfxDone

jumpTable:
  dw noSFX,playSFX01,playSFX02


channel0Tone0:
  ;Tone
  db %10001111
     ;|:|:|:|:
     ;|:|:Data, lo-nibble
     ;|:|Data type (0==square wave counter reset||1==attenuation value/volume)
     ;|Channel ID
     ;Latch. (1==first, or only byte being written,0==second byte)
  db %00011100
     ;|:|:|:|:
     ;|:Data, hi-nibble
     ;|x
     ;Latch. Indicates first, or only byte being written
  ;Volume
  db %10010000
     ;|:|:|:|:
     ;|:|:Data, lo-nibble
     ;|:|Data type (0==square wave counter reset||1==attenuation value/volume)
     ;|Channel ID
     ;Latch. (1==first, or only byte being written,0==second byte)

channel0Tone1:
  ;Tone
  db %10000000
     ;|:|:|:|:
     ;|:|:Data, lo-nibble
     ;|:|Data type (0==square wave counter reset||1==attenuation value/volume)
     ;|Channel ID
     ;Latch. (1==first, or only byte being written,0==second byte)
  db %00011100
     ;|:|:|:|:
     ;|:Data, hi-nibble
     ;|x
     ;Latch. Indicates first, or only byte being written
  ;Volume
  db %10010000
     ;|:|:|:|:
     ;|:|:Data, lo-nibble
     ;|:|Data type (0==square wave counter reset||1==attenuation value/volume)
     ;|Channel ID
     ;Latch. (1==first, or only byte being written,0==second byte)
  db " END OF CODE"

endOfRom:
  ds $1000-$,$00            ;Pad untill 1kb