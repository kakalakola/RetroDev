;----------------------------------------------------
;32X Initialization 0.0 - 2021.12.18
;Z80 Code
;by Saad Azim
;----------------------------------------------------

;Nothing too fancy, a simple pair of counters to make sure the main loop & IRQ/VBlank are working

;Variables can be declared anywhere
foo: equ $0200

;Until I know better, all (attempted) reverse engineered initialization routines are OUT...
  di                        ;Disable interrupts
  jp initZ80                ;Jump to code, leaving an area for
  ds $0038-$,$00            ;Pad from "here" ($) until $0038 (interrupt vector for IRQ) with $00

  jp IRQ                    ;Interrupt vector, effectively VBlank
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

  ld a,(foo)
  inc a
  ld (foo),a
  halt                      ;Wait for interrupt
  jp main

IRQ:                        ;VBlank
  exx                       ;Exchange registers
  ex af,af'

  ld a,(bar)
  inc a
  ld (bar),a

  ex af,af'                 ;Restore registers
  exx

  ei                        ;Enable interrupts

  ;Interrupts are disabled everytime an IRQ is triggered. If they're not enabled before RETI, the CPU is stuck on HALT indefinitely

  reti

bar: equ $0208