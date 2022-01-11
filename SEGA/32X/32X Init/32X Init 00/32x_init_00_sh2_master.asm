;----------------------------------------------------
;32x ASM Project 0.0 - SH2 Master - 2022.01.11
;(Compiling a working file)
;by Saad Azim
;----------------------------------------------------
;Master SH2 code
SH2_M_STACK equ $0603f000

  org $06000000

  ;Jump table $000000-$000120
  dc.l masterColdStart      ;Location of start code for cold start    $000000
  dc.l SH2_M_STACK          ;Stack pointer for cold start             $000004
                            ;set to $0603f000-$0603ffff
  dc.l error                ;Location of start code for manual reset  $000008
  dc.l SH2_M_STACK          ;Stack pointer for manual reset           $00000c
                            ;(Same as cold reset...)

  dc.l error                ;Illegal Instruction                      $000010
  dc.l $00000000            ;Reserved                                 $000014
  dc.l error                ;Invalid Slot Instruction                 $000018
  dc.l $20100400            ;System Reserved (ICE Vector)             $00001c
  dc.l $20100420            ;System Reserved (ICE Vector)             $000020
  dc.l error                ;CPU Address Error                        $000024
  dc.l error                ;DMA Address Error                        $000028
  dc.l error                ;NMI                                      $00002c
  dc.l error                ;User Break                               $000030

  dcb.l 19,$00000000        ;Reserved                         $000034-$00007f
  dcb.l 32,error            ;Trap vectors                     $000080-$0000ff

  ;Interrupt vectors. From the looks of things, byte 0 doesn't seem to do much in the 32X.

  dc.l error                ;IRQ 1                                    $000100
  dc.l error                ;IRQ 2 & 3                                $000104
  dc.l error                ;IRQ 4 & 5                                $000108
  dc.l error                ;PWM interrupt (IRQ 6 & 7)                $00010c
  dc.l error                ;Command interrupt (IRQ 8 & 9)            $000110
  dc.l error                ;HBlank interrupt (IRQ 10 & 11)           $000114
  dc.l error                ;VBlank interrupt (IRQ 12 & 13)           $000118
  dc.l error                ;VRes interrupt (IRQ 14 & 15)             $00011c

  ;Start of code at $000120
masterColdStart:

  ;**NOTE**
  ;The SH2 only supports 8-bit values for immediate MOV instructions, i.e. MOV #$E2,Rn
  ;The assembler deals with 16-bit and 32-bit immediate MOV instructions by adding the number at the end of the ROM, and then loading the data by reference. i.e. MOV #$20004000,R14 results in the assembler appending $20004000 to the end of the ROM, and then using MOV [addressTo$20004000],R14 in the binary file

  mov #SH2_M_STACK,r15      ;Load stack address for master to r15
  mov #$20004000,r14        ;Load cache through address for system register to r14
  
  ldc r14,gbr               ;LoaD to Control register
                            ;Load the address stored in r14 to Global Base Register

  ;$4000 - System Register/Interrupt mask
  mov #%0000000000000000,r0
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|PWM time interrupt mask (mask||effective)
       ;|:|:|:|:|:|:|:CMD - Command interrupt mask (mask||effective) 
       ;|:|:|:|:|:|:|H Int mask (mask||effective)
       ;|:|:|:|:|:|:V Int mask (mask||effective)
       ;|:|:|:|:|xxx
       ;|:|:|:|:HEN - H Interrupt, read only, (not approved||approved)
       ;|:|:|:|CART - Cartridge, read only (inserted||not inserted)
       ;|:|:|:ADEN - ADapter Enabled, read only, (32X disabled||32X enabled)
       ;|xxxxx
       ;FM - Framebuffer access Mode (68k||SH2)
  
  mov.w r0,@(0,gbr)         ;Write to $20004000

mainLoop:
  mov #$26000800,r0         ;Load an address (non-cache-through) into R0
  mov @r0,r1                ;Load the value at the address in R0 into R1
  add #1,r1                 ;Add #1 to the value in R1
  mov r1,@r0                ;Store the value in R1 back into the address at R0

  bra mainLoop
  nop                       ;Because of how the SH2 handles things, BRA needs to be followed with a NOP for safety
  .align 4

error:
  bra error
  nop
  .align 4
