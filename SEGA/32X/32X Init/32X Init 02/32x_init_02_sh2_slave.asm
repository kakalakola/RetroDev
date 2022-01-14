;----------------------------------------------------
;32x ASM Project 0.2 - SH2 Slave - 2022.01.11
;(Drawing an image)
;by Saad Azim
;----------------------------------------------------

;STILL not much for the Slave SH2 to do
  org $06000000

  dc.l slaveColdStart       ;Location of start code for cold start    $000000
  dc.l SH2_S_STACK          ;Stack pointer for cold start             $000004
                            ;set to $0603ef00-$0603efff
  dc.l slaveHotStart        ;Location of start code for manual reset  $000008
  dc.l SH2_S_STACK          ;Stack pointer for manual reset           $00000c
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

  dc.l error                ;IRQ 1                                    $000100
  dc.l error                ;IRQ 2 & 3                                $000104
  dc.l error                ;IRQ 4 & 5                                $000108
  dc.l error                ;PWM interrupt (IRQ 6 & 7)                $00010c
  dc.l error                ;Command interrupt (IRQ 8 & 9)            $000110
  dc.l error                ;IRQ 10 & 11                              $000114
  dc.l error                ;IRQ 12 & 13                              $000118
  dc.l error                ;IRQ 14 & 15                              $00011c

  ;Start of code at $000120
slaveColdStart:
  mov #0,r0                 ;Clear R0

  mov SH2_S_STACK,r15       ;Load stack address for master to r15
  mov CACHE_THROUGH,r14     ;Load cache through address for system register to r14

  ldc r14,gbr               ;LoaD to Control register
                            ;Load the address stored in r14 to Global Base Register
slaveHotStart:
mainLoop:
  mov LOOP_COUNTER,r0
  mov @r0,r1
  add #1,r1
  mov r1,@r0
  bra mainLoop
  nop
  .align 4

error:
  bra error
  nop
  .align 4

;Moving 32-bit values to their own data section

SH2_S_STACK:    dc.l $0603ef00
CACHE_THROUGH:  dc.l $20004000
LOOP_COUNTER:   dc.l $26000240
  .org $06000300