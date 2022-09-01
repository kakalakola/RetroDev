;----------------------------------------------------
;32x ASM Project 0.1 - SH2 Master - 2022.01.11
;(Basic interrupt handling)
;by Saad Azim
;----------------------------------------------------
;Master SH2 code
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
  dc.l VBlank               ;VBlank interrupt (IRQ 12 & 13)           $000118
  dc.l error                ;VRes interrupt (IRQ 14 & 15)             $00011c

  ;Start of code at $000120
masterColdStart:

  ;**NOTE**
  ;The SH2 only supports 8-bit values for immediate MOV instructions, i.e. MOV #$E2,Rn
  ;The assembler deals with 16-bit and 32-bit immediate MOV instructions by adding the number at the end of the ROM, and then loading the data by reference. i.e. MOV #$0100,Rn results in $0100 being added to the end of ROM, and the instruction MOV addressOf0100,Rn being used instead.

  ;I prefer to manually specify data blocks at the end of code. But from a programming POV, there's nothing particularly wrong with MOV #$20004000,R15 :)

  mov SH2_M_STACK,r15      ;Load stack address for master to r15
  mov CT_SYS_REG_ADDRESS,r14 ;Load cache through address for system register to r14
  
  ldc r14,gbr               ;LoaD to Control register
                            ;Load the address stored in r14 to Global Base Register

  ;Doing things properly now, waiting for the Slave SH2 and Genesis to signal they're ready before doing anything else.
  ;$40xx-$40yy is a 4-byte block of address used by the Slave SH2 to communicate with the Master SH2

waitForSlaveSH2:
  mov.l @($24,gbr),r0
  mov.l STAT_CHECK,r1       ;"S_OK"
  cmp/eq r1,r0              ;CoMPare if the value in R0 is EQual to value in R1
  bf waitForSlaveSH2        ;Branch if False to waitForSlaveSH2

  ;Disable interrupts
  mov.w INT_DISABLE,r0
  mov.w r0,@(0,gbr)         ;Write #$8000 to GBR ($4000) to disable interrupts
  mov.w r0,@($0100,gbr)     ;Write $8000 to $4100 to disable video

  ;Clear interrupt flags. Even though they're not all used, it's good practice

  mov.l ZERO,r0

  mov.w r0,@($14,gbr)       ;Clear VRES interrupt
  mov.w r0,@($14,gbr)
  mov.w r0,@($16,gbr)       ;Clear VBlank
  mov.w r0,@($16,gbr)
  mov.w r0,@($18,gbr)       ;Clear HBlank
  mov.w r0,@($18,gbr)
  mov.w r0,@($1a,gbr)       ;Clear CMD interrupt
  mov.w r0,@($1a,gbr)
  mov.w r0,@($1c,gbr)       ;Clear PWM interrupt
  mov.w r0,@($1c,gbr)

  ;Enable VBlank
  mov.w INT_ENABLE,r0
  mov.w r0,@(0,gbr)         ;Write to $20004000

  mov.w SYS_REG_IRQ_EN,r0
  ldc r0,sr                 ;Write to System Register, which controls IRQ masking

mainLoop:
  mov LOOP_COUNTER,r0       ;Load an address (non-cache-through) into R0
  mov @r0,r1                ;Load the value at the address in R0 into R1
  add #1,r1                 ;Add #1 to the value in R1
  mov r1,@r0                ;Store the value in R1 back into the address at R0

  bra mainLoop
  nop                       ;Because of how the SH2 handles things, BRA needs to be followed with a NOP for safety
  .align 4

VBlank:
  mov VBLANK_COUNTER,r0     ;Load an address (non-cache-through) into R0
  mov @r0,r1                ;Load the value at the address in R0 into R1
  add #1,r1                 ;Add #1 to the value in R1
  mov r1,@r0                ;Store the value in R1 back into the address at R0

  mov CT_SYS_REG_ADDRESS,r14
  ldc r14,gbr

  ;Clear VInt flag
  mov.l ZERO,r0
  mov.w r0,@($16,gbr)       ;Write twice to register $4016, V interrupt clear
  mov.w r0,@($16,gbr)

  rte                       ;ReTurn from intErrupt
  nop
  .align 4

error:
  bra error
  nop
  .align 16

;----------------------------------------------------
;Data block
;----------------------------------------------------

STAT_CHECK:         dc.b "S_OK","G_OK"
INT_DISABLE:        dc.w $8000

;Interrupt control, written to $4000
INT_ENABLE:         dc.w %0000000000001000
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

;System register, written to SR
SYS_REG_IRQ_EN:     dc.w %0000000010110000
                         ;|:|:|:|:|:|:|:|:
                         ;|:|:|:|:|:|:|:|T bit
                         ;|:|:|:|:|:|:|:S bit
                         ;|:|:|:|:|:|:Always set to 0
                         ;|:|:|:|:IRQ Mask (Uses 4 bit number to determine highest level, like 68K)
                         ;|:|:|:|Q Bit
                         ;|:|:|:M Bit
                         ;Always set to 0

;**NOTE**
;An IRQ mask of %1011 will trigger all IRQs *higher* than %1011, so IRQs 12, 13, 14, and 15


ZERO:               dc.l 0

SH2_M_STACK:        dc.l $0603f000
CT_SYS_REG_ADDRESS: dc.l $20004000
LOOP_COUNTER:       dc.l $26000800
VBLANK_COUNTER:     dc.l $26000808