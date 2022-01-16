;----------------------------------------------------
;32x ASM Project 0.3 - Z80 - 2022.01.16
;(DMA, frame swapping, and line table optimizations)
;by Saad Azim
;----------------------------------------------------

;Nothing too fancy, a simple pair of 16-bit counters to make sure the main loop & IRQ/VBlank are working

;Variables can be declared anywhere
mainCounter: equ $0200
irqCounter:  equ $0210

  ;Start of code. $38 bytes is enough to clear all registers and set the stack pointer. :)
  di                        ;Disable interrupts
  xor a                     ;Bitwise XOR on A with A, essentially resulting in $00...?
                            ;Resets Carry Negative-carry and Half-carry flag
                            ;Detects Parity/oVerflow flag
                            ;Zero/Sign flags are affected as defined
  ld bc,$0000
  ld de,$0000
  ld hl,$0000

  ld ix,$0000
  ld iy,$0000

  ;Clear shadow registers
  ex af,af'                 ;EXchange the contents of AF and AF'
  exx                       ;EXchange the contents of BC, DE & HL
                            ;with BC', DE' and HL'

  xor a
  ld bc,$0000
  ld de,$0000
  ld hl,$0000

  ld sp,$1000

  jp initZ80                ;Jump to code, leaving an area for
  ds $0038-$,$00            ;Pad from "here" ($) until $0038 (interrupt vector for IRQ) with $00

  ;Interrupt vector/handler
  ;The interrupt routine is small enough to be placed here in the header :)
  exx                       ;Exchange registers
  ex af,af'

  ld bc,(irqCounter)
  inc bc
  ld (irqCounter),bc

  ex af,af'                 ;Restore registers
  exx

  ei                        ;Enable interrupts
                            ;Interrupts are disabled everytime an IRQ is triggered. If they're not enabled before RETI, the CPU is stuck on HALT indefinitely
  reti
  ds $0100-$,$00            ;Pad from "here" ($) until $0100 with $00

initZ80:                    ;Starting from $0100
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
  ld sp,$1000

  ei                        ;Enable Interrupts
  im 1                      ;Interrupt Mode 1

main:                       ;A simple loop to see if things are working

  xor a
  ld bc,(mainCounter)
  inc bc
  ld (mainCounter),bc
  ;halt                      ;Wait for interrupt
                            ;Enabling this will result in mainCounter==irqCounter
  jp main
