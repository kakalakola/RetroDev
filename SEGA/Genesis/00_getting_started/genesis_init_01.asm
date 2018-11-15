;----------------------------------------------------
;GENESIS ASM: Init 0.1
;(Better loops, a CPU counter, and vBlank timings)
;by Saad Azim
;----------------------------------------------------

  ;A buttload of thanks to https://darkdust.net/index.php/writings/megadrive, http://www.genny4ever.net, http://www.hacking-cult.org, https://segaretro.org, https://www.romhacking.net, and https://en.wikibooks.org/wiki/Genesis_Programming

  ;The purpose of this code is to boot a MegaDrive/Genesis *PROPERLY*, and diplay a color on screen. And set up a pair of 32 bit counters, that increment once during every CPU loop, and vertical blanks, respectively.

  ;Modified the vectors to be problem specific.

  dc.l $fffe00              ;Stack pointer (DC=Data Copy?)        $0000
  dc.l codeStart            ;Code start, best left as a label     $0004
  dc.l busError             ;Bus error                            $0008
  dc.l addressError         ;Address error                        $000c
  dc.l illegalInstruction   ;Illegal instruction                  $0010
  dc.l divideByZero         ;Division by zero                     $0014
  dc.l chkException         ;CHK exception                        $0018
  dc.l trapVException       ;TRAPV exception                      $001c
  dc.l privilegeViolation   ;Privilege violation                  $0020
  dc.l traceException       ;Trace Exception                      $0024
  dc.l line1010Emulator     ;LINE 1010 EMULATOR                   $0028
  dc.l line1111Emulator     ;LINE 1111 EMULATOR                   $002c

  ;Reserved by Motorola, 12 entries                         $0030-$005f
  dc.l reservedByMotorola,reservedByMotorola,reservedByMotorola,reservedByMotorola,reservedByMotorola,reservedByMotorola,reservedByMotorola,reservedByMotorola,reservedByMotorola,reservedByMotorola,reservedByMotorola,reservedByMotorola

  dc.l spuriousException    ;Spurious exception                   $0060
  dc.l irqLevel1            ;IRQ level 1                          $0064
  dc.l irqLevel2            ;IRQ level 2                          $0068
  dc.l irqLevel3            ;IRQ level 3                          $006c
  dc.l hBlank               ;IRQ level 4 (VDP interrupt/hblank)   $0070
  dc.l irqLevel5            ;IRQ level 5                          $0074
  dc.l vBlank               ;IRQ level 6 (vblank)                 $0078
  dc.l irqLevel7            ;IRQ level 7                          $007c
  dc.l trap00Exception      ;TRAP #00 exception                   $0080
  dc.l trap01Exception      ;TRAP #01 exception                   $0084
  dc.l trap02Exception      ;TRAP #02 exception                   $0088
  dc.l trap03Exception      ;TRAP #03 exception                   $008c
  dc.l trap04Exception      ;TRAP #04 exception                   $0090
  dc.l trap05Exception      ;TRAP #05 exception                   $0094
  dc.l trap06Exception      ;TRAP #06 exception                   $0098
  dc.l trap07Exception      ;TRAP #07 exception                   $009c
  dc.l trap08Exception      ;TRAP #08 exception                   $00a0
  dc.l trap09Exception      ;TRAP #09 exception                   $00a4
  dc.l trap0AException      ;TRAP #10 exception                   $0018
  dc.l trap0BException      ;TRAP #11 exception                   $00ac
  dc.l trap0CException      ;TRAP #12 exception                   $00b0
  dc.l trap0DException      ;TRAP #13 exception                   $00b4
  dc.l trap0EException      ;TRAP #14 exception                   $00b8
  dc.l trap0FException      ;TRAP #15 exception                   $00bc

  ;Reserved by Motorola, 16 entries                         $00c0-$00ff
  dc.l reservedByMotorola,reservedByMotorola,reservedByMotorola,reservedByMotorola,reservedByMotorola,reservedByMotorola,reservedByMotorola,reservedByMotorola,reservedByMotorola,reservedByMotorola,reservedByMotorola,reservedByMotorola,reservedByMotorola,reservedByMotorola,reservedByMotorola,reservedByMotorola

  ;ROM header, starting from $000100. Also 256 bytes long-word.
  dc.b "SEGA GENESIS    "   ;Console name Genesis/Mega Drive      $0100
                            ;Must be 16 characters long-word
  dc.b "(C)LBP  "           ;(C)+Firm name/code, 8 characters     $0110
  dc.b "2018.NOV"           ;Build date, 8 characters             $0118   

  dc.b "GENESIS COMPILATION/INITIALIZATION TEST         ";        $0120
                            ;Domestic name, 48 characters
  dc.b "MEGADRIVE COMPILATION/INITIALIZATION TEST       ";        $0150
                            ;Foreign name,48 characters

  dc.b "GM 00000000-00"     ;ROM Type, serial & version number    $0180

  dc.w $ffff                ;Checksum, 2 bytes                    $018e

  dc.b "J               "   ;Input support, 16 characters         $0190

  dc.l $00000000,romEnd-1,$00ff0000,$00ffffff;ROM Info 16 bytes   $01a0
  
  dc.b "                        "    ;SRAM & Modem 24 bytes       $01b0

  dc.b "AN INITILIAZTION ROUTINE FOR THE GENESIS";Notes, 40 char. $01c8

  dc.b "JUE             "   ;Country code+13 character padding    $01f0

;Beginning of code at $0200
codeStart:
  move.w #$2700,sr          ;Disable ALL interrupts

  ;Check TMSS
  move.b $a10001,d0
  andi.b #$f,d0
  beq noTMSS
  move.l #"SEGA",$a14000

  ;Having dealt with TMSS, whether it's there or not, it's time to clean up system RAM

  ;Mind you, it's not a requirement per se, but it's good practice to tidy things up before begnning the main bits

noTMSS:

  moveq #0,d0               ;Clear data register D0
                            ;MOVE Quick #0 fills the destination register with 0s

  move.w #$3fff,d1          ;Length of RAM/4 (ffff/4==3fff)
  movea.l d0,a6             ;copy $00000000 as an address to
                            ;the address register A6
  move.l a6,usp             ;Copy the address stored in A6 ($00000000)
                            ;to the user stack pointer
clearRamLoop:
  move.l d0,-(a6)           ;Decrement the address stored in A6,
                            ;then copy the contents of D0 to said address. The contents of D0, #$00000000 gets copied to $fffffc in the first loop, $fffff8 in the second loop, and so forth; until the counter in D1 reaches 0, at which point the last value is copied to $ff0000

                            ;Address increment/decrements are size sensitive, so move.l d0,-(a6) decrements A6 by 4 bytes, move.w d0,-(a6) would decrement A6 by 2 bytes, and move.b d0,-(a6) would decrement A6 by 1 byte

  dbf d1,clearRamLoop       ;Decrease and Branch if False (i.e.
                            ;not equal to 0) to clearRamLoop.

                            ;Basically, as long as the value in D1 is not equal to 0, continue to decrease the value in D1 and jump back to clearRamLoop

  ;Initializing the VDP

  ;NOTE: ALL registers SHOULD BE initialized, regardless of whether they're used or not. Otherwise, different emulators (since I haven't been able to test this on actual hardware) might or might not run this properly.

  ;Setup address registers for VDP ports, so the code structure is along the lines of "write to a0, then write to a1".
  lea $c00004,a0  
  lea $c00000,a1           


  move.w (a0),d0            ;Read the contents of the VDP control port
                            ;to initialize the VDP. The actual contents of D0 doesn't matter for this project

  move.l #$800081ec,(a0)    ;Enable vBlank

  move.l #$82008300,(a0)    ;Nametable address for Scroll A & Window
                            ;are set to $0000 in VRAM
  move.l #$84008500,(a0)    ;Nametable address for Scroll B & Sprite 
                            ;Attribute Table are also set to $0000 in VRAM

  move.w #$8a00,(a0)        ;hBlank counter (raster lines)

  move.l #$8b008c00,(a0)    ;Mode set registers 3 & 4

  move.l #$8d008f00,(a0)    ;H scroll data table is set to $0000
                            ;Address Auto Increment is set to 0 bytes

  move.w #$9000,(a0)        ;Screen size 32x32 cells, no interlacing

  move.l #$91009200,(a0)    ;Window h & v position

  move.l #$93009400,(a0)    ;(DMA length/2) low & high

  move.l #$95009600,(a0)    ;(DMA source/2) low & mid

  move.w #$9700,(a0)        ;(DMA source/2) high

  ;This should look familiar
  move.l #$c0000000,(a0)    ;Set the control port $c00004 to
                            ;point to $00 in VRAM
  move.w #$000e,(a1)        ;Send a color value to the data port $c00000
                            ;which gets written to $00 in CRAM

  move #$2000,sr            ;Enable all interrupts

;For the sake of this project, the data in RAM (located at address $ff0000) will be increased by 1, then the 68K will stop and wait a VInt

;A VInt (Vertical Interrupt) signal is basically the VDP telling the CPU that it has finished drawing one frame, and is free for a short amount of time. This, in turn, acts like a timer that "ticks" once every 1/60th of a second (or 1/50th of a second, depending of the region/setting)

;The address vector defined in the header tells the CPU to jump to the label "vBlank" when a VInt is triggered

main:
  addq.l #1,$ff0000.l       ;Increase $ff0000 by one, every loop
                            ;By default, the value would get added to a 16 bit word, with a maximum value of $ffff before it reverted back to $0000. But by specifying a 32 bit long-word, the number has to hit $ffffffff before it loops back to $00000000

  stop #$2000               ;Enable interrupts and wait for IRQ
                            ;Similar to move #$2000,sr, but with a wait instruction for the 68K

                            ;Since the CPU stops after incrementing the data once, the data stored in $ff0000, and $ff0010 should increment at the same rate. In fact, the difference between them should be 1, at most.

  jmp main


;Vertical Interrupt subroutine
vBlank:                     ;A.K.A. IRQ 6

  move.w #$2700,sr          ;Disable all interrupts

  ;The time vBlank has to update the VDP, before the VDP has to start rendering the screen is EXTREMELY limited. So it's good practice to disable anything else that might interrupt it

  movem.l d0-a6,-(sp)       ;Copy registers to Stack pointer

  ;This is a quick way to back up all the registers, so that anything done with them during vBlank doesn't interfere with anything they were doing previously

  addq #1,$ff0010.l         ;Increase $ff0010 by one. Stores as a word

  movem.l (sp)+,d0-a6       ;Restore registers from stack pointer
  
  rte                       ;ReTurn from intErrupt

;Creating separate error loops with individual labels instead of just one "error". Makes it human-readable to test errors. Using labels that are 16 characters wide for the sake of keeping things constant

  dc.b "BUS_ERROR       "
busError:
  jmp busError

  dc.b "ASDDRESS_ERROR  "
addressError:
  jmp addressError

  dc.b "ILLEGAL_INSTRUCT"
illegalInstruction:
  jmp illegalInstruction

  dc.b "DIVIDE_BY_ZERO  "
divideByZero:
  jmp divideByZero

  dc.b "CHK_EXCEPTION   "
chkException:
  jmp chkException

  dc.b "TRAP_V_EXCEPTION"
trapVException:
  jmp trapVException

  dc.b "PRIV_VIOLATION  "
privilegeViolation:
  jmp privilegeViolation

  dc.b "TRACE_EXCEPTION "
traceException:
  jmp traceException

  dc.b "LINE_1010_EMU   "
line1010Emulator:
  jmp line1010Emulator

  dc.b "LINE_1111_EMU   "
line1111Emulator:
  jmp line1111Emulator

  dc.b "RESERVED_BY_MOTO"
reservedByMotorola:
  jmp reservedByMotorola

  dc.b "SPURIOUS_EXCPTN "
spuriousException:
  jmp spuriousException

  dc.b "IRQ_1           "
irqLevel1:
  jmp irqLevel1

  dc.b "IRQ_2           "
irqLevel2:
  jmp irqLevel2

  dc.b "IRQ_3           "
irqLevel3:
  jmp irqLevel3

;HBlank
  dc.b "IRQ_4           "
irqLevel4:
hBlank:
  jmp irqLevel4

  dc.b "IRQ_5           "
irqLevel5:
  jmp irqLevel5

;VBlank
;  dc.b "IRQ_6           "
;irqLevel6:
;  jmp irqLevel6

  dc.b "IRQ_7           "
irqLevel7:
  jmp irqLevel7

  dc.b "TRP_00_EXCEPTION"
trap00Exception:
  jmp trap00Exception

  dc.b "TRP_01_EXCEPTION"
trap01Exception:
  jmp trap01Exception

  dc.b "TRP_02_EXCEPTION"
trap02Exception:
  jmp trap02Exception

  dc.b "TRP_03_EXCEPTION"
trap03Exception:
  jmp trap03Exception

  dc.b "TRP_04_EXCEPTION"
trap04Exception:
  jmp trap04Exception

  dc.b "TRP_05_EXCEPTION"
trap05Exception:
  jmp trap05Exception

  dc.b "TRP_06_EXCEPTION"
trap06Exception:
  jmp trap06Exception

  dc.b "TRP_07_EXCEPTION"
trap07Exception:
  jmp trap07Exception

  dc.b "TRP_08_EXCEPTION"
trap08Exception:
  jmp trap08Exception

  dc.b "TRP_09_EXCEPTION"
trap09Exception:
  jmp trap09Exception

  dc.b "TRP_0A_EXCEPTION"
trap0AException:
  jmp trap0AException

  dc.b "TRP_0B_EXCEPTION"
trap0BException:
  jmp trap0BException

  dc.b "TRP_0C_EXCEPTION"
trap0CException:
  jmp trap0CException

  dc.b "TRP_0D_EXCEPTION"
trap0DException:
  jmp trap0DException

  dc.b "TRP_0E_EXCEPTION"
trap0EException:
  jmp trap0EException

  dc.b "TRP_0F_EXCEPTION"
trap0FException:
  jmp trap0FException

romEnd:
