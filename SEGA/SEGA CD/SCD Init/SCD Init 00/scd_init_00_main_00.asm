;----------------------------------------------------
;SEGA CD ASM Project 0.0 - 2021.12.18
;Getting things to work... -_-'
;by Saad Azim
;----------------------------------------------------

loopCounter equ $ff0010

  org $200000               ;Since the program will be running from Word RAM ($200000)

codeStart:
  move.w #$2700,sr          ;Disable all interrupts

  ;Status register bits
  ;#%0000000000000000
    ;|:|:|:|:|:|:|:|:
    ;|:|:|:|:|:|:|:|Carry
    ;|:|:|:|:|:|:|:Overflow
    ;|:|:|:|:|:|:|Zero
    ;|:|:|:|:|:|:Negative
    ;|:|:|:|:|:|Extend
    ;|:|:|:|:xxx
    ;|:|:|3 byte interrupt priority mask, ranging from 0-7
    ;|:|xx
    ;|:Supervisor state (status register can only be modified in supervisor state)
    ;|x
    ;Trace mode

  ;Interrupt masking works by ignoring ALL interrupts *equal* or below the level specified. So if/when the mask is set to 7, the highest available level, ALL interrupts will be ignored. Including, but not limited to VBlank (IRQ 6) & HBlank (IRQ 4).

  ;Check TMSS
  move.b $a10001,d0
  andi.b #$f,d0
  beq noTMSS
  move.l #"SEGA",$a14000

noTMSS:

  ;Having dealt with TMSS, whether it's there or not, it's time to clean up system RAM. Unlike the Genesis, $fffd00 and onwards contains jump vectors for the SEGA CD. So the amount and starting address are altered accordingly

  moveq #0,d0
  move.w #$3f3f,d1
  lea $fffd00,a0
  move.l a0,usp             ;...?
clearRamLoop:
  move.l d0,-(a0)
  dbf d1,clearRamLoop

  ;Mind you, it's not a requirement per se, but it's good practice to tidy things up before begnning the main bits

;----------------------------------------------------
;Initialize the VDP
;----------------------------------------------------

  ;NOTE: ALL registers should be initialized, regardless of whether they're used or not. Otherwise, different emulators (since I haven't been able to test this on actual hardware) might or might not run this properly.

  ;Setup address registers for VDP ports, so the code structure is along the lines of "write to a0, then write to a1"
  lea $c00004,a0  
  lea $c00000,a1           

  ;At this point the VDP has already been initialized by the BIOS.

  move.l #$c0000000,$c00004 ;Point VDP control to palette 0, color 0 in CRAM
  move.w #$000e,$c00000     ;Write send a color value to P0C0 in CRAM

mainLoop:  
  add.l #1,(loopCounter)
  jmp mainLoop

  cnop $00,$0800              ;Pad to end of nearest sector
