;----------------------------------------------------
;GENESIS ASM: Pong 0.0
;(A basic game of pong)
;by Saad Azim
;----------------------------------------------------


;Defining variables, of sort
;Numbers, basically...
_intXMin equ $0080
_intXMax equ $01b8
_intXCenter equ $011c
_intYMin equ $0080
_intYMax: equ $0140
_intYCenter equ $00e0
_intNumSprites equ $02      ;Something to keep track of the number of sprites for now

_intPlayerSpeed equ $02
_intBallSpeedX equ $04
_intBallSpeedY equ $00

;System bits
_ramFlagRunOnce equ $ff0000

_ramController01 equ $ff0002
_ramController02 equ $ff0004


;Offsets in RAM for sprites
_ramSpriteStart equ $ff0010 ;Redundant, but here for the sake of ease of use

_ramPlayer1Sprite equ $ff0010
_ramPlayer1X equ $ff0016
_ramPlayer1Y equ $ff0010    ;Redundant, but here for the sake of ease of use, again

_ramPlayer2Sprite equ $ff0018
_ramPlayer2X equ $ff001e
_ramPlayer2Y equ $ff0018

_ramBallSprite equ $ff0020
_ramBallX equ $ff0026
_ramBallY equ $ff0020

;Offsets in RAM for non-sprite related player & ball info
_ramPlayer1SpeedY equ $ff0100
_ramPlayer2SpeedY equ $ff0102
_ramBallFlags equ $ff0110
_ramBallSpeedX equ $ff0112
_ramBallSpeedY equ $ff0114

  ;Setting up the vectors.  Basically creating a list of things to do when
  ;one of the bits in the $000000-$0000ff range is triggered.  I'm putting
  ;a hex address at the end of the comment because it's every easy to lose
  ;track of the order of instructions.  I ended up having to re-type all the
  ;vectors because of an extra line of "dc.l error" :'(
  ;
  ;It's a pain, but whan I was starting out, it was more important understanding
  ;which byte stood for what.  While it is easier to type "dc.l $fffe00, entry, error, error"
  ;it is also a little harder to understand that the instructions do.

vectors:
  dc.l $fffe00              ;Stack pointer (DC=Data Copy?)        $00
  dc.l codeStart            ;Code start, best left as a label     $04
  dc.l error                ;Bus error                            $08
  dc.l error                ;Address error                        $0c
  dc.l error                ;Illegal instruction                  $10
  dc.l error                ;Division by zero                     $14
  dc.l error                ;CHK exception                        $18
  dc.l error                ;TRAPV exception                      $1c
  dc.l error                ;Privilege violation                  $20
  dc.l error                ;Trace Exception                      $24
  dc.l error                ;LINE 1010 EMULATOR                   $28
  dc.l error                ;LINE 1111 EMULATOR                   $2c
  dc.l error                ;Reserved by Motorola                 $30
  dc.l error                ;Reserved by Motorola                 $34
  dc.l error                ;Reserved by Motorola                 $38
  dc.l error                ;Reserved by Motorola                 $3c
  dc.l error                ;Reserved by Motorola                 $40
  dc.l error                ;Reserved by Motorola                 $44
  dc.l error                ;Reserved by Motorola                 $48
  dc.l error                ;Reserved by Motorola                 $4c
  dc.l error                ;Reserved by Motorola                 $50
  dc.l error                ;Reserved by Motorola                 $54
  dc.l error                ;Reserved by Motorola                 $58
  dc.l error                ;Reserved by Motorola                 $5c
  dc.l error                ;Spurious exception                   $60
  dc.l error                ;IRQ level 1                          $64
  dc.l error                ;IRQ level 2                          $68
  dc.l error                ;IRQ level 3                          $6c
  dc.l hblank               ;IRQ level 4 (VDP interrupt/hblank)   $70
  dc.l error                ;IRQ level 5                          $74
  dc.l vblank               ;IRQ level 6 (vblank)                 $78
  dc.l error                ;IRQ level 7                          $7c
  dc.l error                ;TRAP #00 exception                   $80
  dc.l error                ;TRAP #01 exception                   $84
  dc.l error                ;TRAP #02 exception                   $88
  dc.l error                ;TRAP #03 exception                   $8c
  dc.l error                ;TRAP #04 exception                   $90
  dc.l error                ;TRAP #05 exception                   $94
  dc.l error                ;TRAP #06 exception                   $98
  dc.l error                ;TRAP #07 exception                   $9c
  dc.l error                ;TRAP #08 exception                   $a0
  dc.l error                ;TRAP #09 exception                   $a4
  dc.l error                ;TRAP #10 exception                   $18
  dc.l error                ;TRAP #11 exception                   $ac
  dc.l error                ;TRAP #12 exception                   $b0
  dc.l error                ;TRAP #13 exception                   $b4
  dc.l error                ;TRAP #14 exception                   $b8
  dc.l error                ;TRAP #15 exception                   $bc
  dc.l error                ;Reserved by Motorola                 $c0
  dc.l error                ;Reserved by Motorola                 $c4
  dc.l error                ;Reserved by Motorola                 $c8
  dc.l error                ;Reserved by Motorola                 $cc
  dc.l error                ;Reserved by Motorola                 $d0
  dc.l error                ;Reserved by Motorola                 $d4
  dc.l error                ;Reserved by Motorola                 $d8
  dc.l error                ;Reserved by Motorola                 $dc
  dc.l error                ;Reserved by Motorola                 $e0
  dc.l error                ;Reserved by Motorola                 $e4
  dc.l error                ;Reserved by Motorola                 $e8
  dc.l error                ;Reserved by Motorola                 $ec
  dc.l error                ;Reserved by Motorola                 $f0
  dc.l error                ;Reserved by Motorola                 $f4
  dc.l error                ;Reserved by Motorola                 $f8
  dc.l error                ;Reserved by Motorola                 $fc

  ;ROM header, starting from $000100

  dc.b "SEGA GENESIS    "   ;Console name Genesis/Mega Drive, must be 16 characters long
  dc.b "(C)SA  2018.APR "   ;Firm name & date, also 16 characters long
  dc.b "GENESIS PONG                                     " ;Domestic name, must be 48 characters long
  dc.b "MEGADRIVE PONG                                   " ;Foreign name, must be 48 characters long
  dc.b "GM 00000000-00"     ;Type ("GM" = game, "AL" = educational), 8 digit serial number & 2 digit version number
  dc.w $ffff                ;Checksum >_<'
  dc.b "J6              "   ;Input support, padded to 16 characters
  dc.l $00000000            ;Beginning of ROM
  dc.l romEnd               ;End of ROM, it's best to use a label
  dc.l $00ff0000            ;Beginning of RAM
  dc.l $00ffffff            ;End of RAM
  dc.b $20,$20,$20          ;SRAM ID (Usuually "RA" & $f8 if enabled)
  dc.b " "                  ;Unused
  dc.b "    "               ;Beginning of SRAM  (For games with battery powered backup)
  dc.b "    "               ;End of SRAM
  dc.b "            "       ;Modem support
  dc.b "PONG FOR THE SEGA GENESIS/MEGADRIVE     ";Notes.  Must be 40 characters
  dc.b "JUE             "   ;Country code, must be 16 characters

;Beginning of code for Actual program
codeStart:
  tst.l ($a10008).l         ;Check & see if anything is connected to controller 1 control port
  bne.s controller1Ok        ;Go to "controller1_OK" if the result is negative
                            ;(BNE == Branch [if] Not Equal [to zero]) bne.s == use 1 byte for branch length

  tst.w ($a1000c).l         ;If controller 1 is not connected, check the expansion control port
  bne.s skipSetup            ;Move on over to skipSetup if the test returns a negative

controller1Ok:
  move.b ($a10001).l,d0     ;Move the first from the version register to data register "d0"
  andi.b #$f,d0             ;"andi" (AND I[mmediate]) with $f
                            ;should result in all 0-s, on a genesis 1
  beq.s skipSecurity        ;If the result is equal to zero, move on to "skipSecurity"
                            ;(BEQ = Branch [if] Equal [to zero])

  move.l #"SEGA",($a14000).l;If the result is not equal to one then it's a Genesis model 2+
                            ;Copy the ascii characters for "SEGA" to $00a14000
                            ;(TMSS register in Genesis 2+, unwritable in model 1)

skipSecurity:               ;Exactly what it sounds like ^_^


skipSetup:                  ;Exactly what it sounds like ^_^

  move.w #$2700,sr          ;Disable NMI

	move.w ($c00004).l,d0		;Read the contents of the VDP control port to initialize the VDP.

	;clear RAM
	moveq #0,d0					;Clear data register D0 (Move Q[uick] #0 fills the destination with 0-s)
  move.w #$3fff,d1    ;Length of RAM/4 (ffff/4==3fff)
	movea.l d0,a6				;copy the data to the address pointer
	move.l a6,usp				;Copy the "address" ($00000000) to the user stack pointer
                      ;(The file I copied this off of leaves out the ".l".  Ths lead to quite a bit of a headache with asm68k)
clearRamLoop:
  move.l d0,-(a6)
  dbf d1,clearRamLoop

  ;Initialize player inputs with #$0fff == no buttons are being pressed
  move.w #$0fff,(_ramController01)
  move.w #$0fff,(_ramController02)

;Setup ports, and reset the Z80
  move.b ($a11101),d6

  move.b #$01,($a11100)       ;Request Z80 Bus, same as move.w #$0100
  move.w #$0100,($a11200)   ;Request Z80 reset
waitZ80BusReqLoop:
  move.b ($a11101),d7
  btst #0,($a11101)         ;Check bit 0 of Z80 bus request register
  bne.s waitZ80BusReqLoop   ;Wait until the Z80 bus is available

  moveq #$40,d0             ;Copy #$40 to d0
  move.b d0,$a10009         ;Configure controller 1 port
  move.b d0,$a1000b         ;Configure controller 2 port
  move.b d0,$a1000d         ;Configure expansion port

  move.b #$0,($a11200)      ;Disable Z80 reset
  move.b #$0,($a11100)      ;Disable the bus
  ;The Z80 doesn't start up until its been reset
  move.b #$1,($a11200)      ;Reset the Z80 again

  ;Copying sprite attributes to RAM
  move.b #_intNumSprites,d0
  lea (spriteDataStart),a0
  lea (_ramPlayer1Sprite),a1

initSpriteToRAMLoop:        ;Going with a rolling loop for the sake of simplicity, I think -_-'
	move.l (a0)+,d1
	move.l d1,(a1)+
	move.l (a0)+,d1
	move.l d1,(a1)+
  dbf d0,initSpriteToRAMLoop

  ;VDP stuff
  lea (vdpConfigStart),a0   ;VDP Config data
  move.w #((vdpConfigEnd-vdpConfigStart)/2)-1,d4          ;Config data length in words

  lea (tileDataStart),a1    ;Tile data
  move.w #((tileDataEnd-tileDataStart)/4)-1,d1          ;Palette data length in longwords

  ;Copy sprite data from RAM instead of ROM
  lea (_ramPlayer1Sprite),a2;Sprite attribute data in RAM
  move.b #_intNumSprites,d2           ;Palette data length in longwords

	lea (paletteDataStart),a3 ;Palette data
  move.w #$001f,d3          ;Palette data length in longwords

	lea ($c00000).l,a5			  ;load c00000 to to address pointer a5
	lea ($c00004).l,a6			  ;load c00004 to to address pointer a6

;Initialize the VDP, and set the respective registers
initVDPLoop:
  move.w (a0)+,d0
  move.w d0,(a6)
  dbf d4,initVDPLoop

	move.w #$8f02,($c00004).l	;auto increment data by 2 bytes

;Copy tiles from ROM to VDP RAM
	move.l #$40000000,(a6)    ;point the data port to $0000, start of the vram i.e. write stuff sent to $c00000 to $0000
initTileLoop:
	move.l (a1)+,d0
	move.l d0,(a5)
  dbf d1,initTileLoop

;Copy sprite attributes from RAM to VDP RAM
  move.l #$44000000,(a6)    ;Set the control port $00c00004 to point to $0400 in VRAM
initSpriteLoop:
	move.l (a2)+,d0
	move.l d0,(a5)
	move.l (a2)+,d0
	move.l d0,(a5)
  dbf d2,initSpriteLoop

;Copy palette from ROM to VDP RAM
  move.l #$c0000000,(a6)    ;Set the control port $00c00004 to point to $c0000000 in VRAM
initPaletteLoop:
	move.l (a3)+,d0						;Using long to write 32 bits of data, with 1f ^_^
	move.l d0,(a5)
  dbf d3,initPaletteLoop

  ;Copy game variables to RAM
  move.w #_intPlayerSpeed,(_ramPlayer1SpeedY)
  move.w #_intPlayerSpeed,(_ramPlayer2SpeedY)
  move.w #0,(_ramBallFlags)
  move.w #_intBallSpeedX,(_ramBallSpeedX)
  move.w #_intBallSpeedY,(_ramBallSpeedY)

;Having done everything, it's time to enable NMI...?
  move.w #$2000,sr

mainLoop:
  addq #1,($ff0060)
  ;-- If runOnce flag set, jump to main
  btst #0,_ramFlagRunOnce   ;BitTeST - test bit 0 of _ramFlagRunOnce
  bne.s mainLoop

  ;Otherwise continue
  bset #0,_ramFlagRunOnce   ;BitSET - set bit 0 of _ramFlagRunOnce
  addq #1,($ff0064)

  ;-- Calculate things in game
  ;Check the ... er ... ball flags

  move.w _ramBallSpeedX,d0  ;Copy the ball's X speed to d0
  btst #0,_ramBallFlags     ;Test bit 0 of _ramBallFlag (movement on the X axis)
  beq.s moveBallRight      ;If the bit == 0, move the ball right

  ;Move ball left
  sub.w d0,_ramBallX        ;Subtract the speed from ball's X position
  ;Check for collision against player 1 sprite
  move.w (_ramBallX),d1     ;Get ball X
  move.w (_ramBallY),d2     ;Get ball Y min
  move.w d2,d3              ;Get ball Y max
  addi.w #$10,d3
  move.w (_ramPlayer1Y),d4  ;Get player 1 Y min
  move.w d4,d5              ;Get player 1 Y max
  addi.w #$20,d5
  cmpi.w #_intXMin+8,d1     ;Compare ball X to player 1 X, offset with player 1 width
  bgt.s checkBoundaryLeft   ;Greater than == ball in front of player, branch to boundary check
  cmp.w d3,d4               ;Otherwise compare ball Y max with player 1 Y min
  bgt.s checkBoundaryLeft   ;Greater than == ball above player, branch to bouncary check
  cmp.w d2,d5               ;Otherwise, compare ball Y min with player 1 Y max
  blt.s checkBoundaryLeft   ;Less than == Ball below player, branch to boundary check
  bchg #0,_ramBallFlags     ;Otherwise, flip ball X direction
  ;Implement angular movement
  cmp d2,d4                 ;Compare ball Y min with player 1 Y min
  blt.s subPlayerBall       ;Less than == subtract player Y from ball Y
  sub.w d2,d4               ;Otherwise subtract ball Y from player Y
  asr.w #2,d4               ;Divide by 4
  move.w d4,(_ramBallSpeedY);Store to ball speed Y
  bset #1,_ramBallFlags     ;Set ball Y direction
  bra.w testBallY             ;...and skip the boundary test
subPlayerBall:
  sub.w d4,d2               ;Subtract player Y from ball Y
  asr.w #2,d2
  move.w d2,(_ramBallSpeedY)
  bclr #1,_ramBallFlags     ;Set ball Y direction
  bra.w testBallY           ;...and skip the boundary test

checkBoundaryLeft:
  cmpi.w #_intXMin-16,(_ramBallX);Compare the ball's X position to screen boundary
  blt.s resetBallX       ;Branch on Less Than...
  bra.w testBallY          ;Otherwise, jump to the Y position calculations

  ;Move ball right
moveBallRight:
  add.w d0,_ramBallX
  ;Check for collision against player 2 sprite
  move.w (_ramBallX),d1     ;Get ball X
  move.w (_ramBallY),d2     ;Get ball Y min
  move.w d2,d3              ;Get ball Y max
  addi.w #$10,d3
  move.w (_ramPlayer2Y),d4  ;Get player 1 Y min
  move.w d4,d5              ;Get player 1 Y max
  addi.w #$20,d5
  cmpi.w #_intXMax-16,d1    ;Compare ball X to player 2 X, offset with ball width
  blt.s checkBoundaryRight  ;Greater than == ball in front of player, branch to boundary check
  cmp.w d3,d4               ;Otherwise compare ball Y max with player 1 Y min
  bgt.s checkBoundaryRight   ;Greater than == ball above player, branch to bouncary check
  cmp.w d2,d5               ;Otherwise, compare ball Y min with player 1 Y max
  blt.s checkBoundaryRight   ;Less than == Ball below player, branch to boundary check
  bchg #0,_ramBallFlags     ;Otherwise, flip ball X direction
  ;Implement angular movement
  cmp d2,d4                 ;Compare ball Y min with player 1 Y min
  blt.s subPlayer2Ball       ;Less than == subtract player Y from ball Y
  sub.w d2,d4               ;Otherwise subtract ball Y from player Y
  asr.w #2,d4               ;Divide by 4
  move.w d4,(_ramBallSpeedY);Store to ball speed Y
  bset #1,_ramBallFlags     ;Set ball Y direction
  bra.s testBallY             ;...and skip the boundary test
subPlayer2Ball:
  sub.w d4,d2               ;Subtract player Y from ball Y
  asr.w #2,d2
  move.w d2,(_ramBallSpeedY)
  bclr #1,_ramBallFlags     ;Set ball Y direction
  bra.s testBallY           ;...and skip the boundary test

checkBoundaryRight:
  cmpi.w #_intXMax+16,(_ramBallX)
  bge.s resetBallX       ;Branch on GrEater than...
  jmp testBallY

resetBallX:
  bchg #0,_ramBallFlags     ;BitCHanGe - change bit 0 of _ramBallFlag
                            ;Oh GOD, this would've been SO much work on a 6502!! >.<'
  move.w #_intXCenter,_ramBallX
  move.w #_intYCenter,_ramBallY

testBallY:
  btst #1,_ramBallFlags
  beq moveBallDown

moveBallUp:
  move.w _ramBallSpeedY,d0
  sub.w d0,_ramBallY
  cmpi.w #_intYMin,(_ramBallY)
  blt.s flipBallY
  jmp player2AI

moveBallDown:
  move.w _ramBallSpeedY,d0
  add.w d0,_ramBallY
  cmpi.w #_intYMax+16-_intPlayerSpeed,(_ramBallY)
  bge.s flipBallY
  jmp player2AI

flipBallY:
  bchg #1,_ramBallFlags

player2AI:
  move.w _ramBallY,d1
  move.w _ramPlayer2Y,d2
  addi.w #8,d2              ;Offset by 8 to try and align the centers of ball and player 2
  cmp.w d1,d2
  bge.s movePlayer2Up
  move.w _ramPlayer2SpeedY,d0
  add.w d0,_ramPlayer2Y
  jmp readController1

movePlayer2Up:
  move.w _ramPlayer2SpeedY,d0
  sub.w d0,_ramPlayer2Y
  jmp readController1


readController1:
  moveq.l #0,d5
  moveq.l #0,d6             ;Clear d6
  moveq.l #0,d7             ;Clear d7

  move.b #$01,$a11100       ;Request Z80 Bus before reading the controller
waitZ80BusReqControllerLoop:
  btst #0,($a11101)
  bne.s waitZ80BusReqControllerLoop

  ;Read controller bits
  lea $a10003,a0            ;Store address of controller port 1 to address register

  ;Hit 00 returns #$7f
  move.b #$40,(a0)          ;Set bit 7 (hex $40) in controller 1 data port.
                            ;This returns (x1CBRLDU), which you dont really need to read, since it repeats
  nop                       ;Wait for two cycles
  nop                       ;Since this is the first hit, can't skip the NOPs

  ;Hit 01 returns #$33
  move.b #0,(a0)            ;Move $00 to controller port 1 to read (00SA00DU)
  nop                       ;Still nothing to do
  nop
  move.b (a0),d5            ;Copy the results to d5 (00SA00DU)

  ;Hit 02 returns #$7f
  move.b #$40,(a0)          ;Set bit 7 again to read (01CBRLDU)
  rol.b #2,d5               ;ROtate Left 2 bytes d5: SA00DU??
  andi.b #%11000000,d5      ;Mask out bits 5-0  d5:SA000000
  move.b (a0),d6            ;Store the results to d6 (01CBRLDU)

  ;Hit 03 returns #$33 for 3 button controller, $30 for 6 button controllers
  move.b #0,(a0)            ;Move $00 to controller port for (x0SA0000), which you can ignore, again
  andi.b #%00111111,d6      ;Mask out bits 7-6 d6: 00CBRLDU
  or.b d6,d5                ;Or d6 into d5 (SACBRLDU)
  move.b (a0),d6            ;Copy the result to D6, to test whether the controller is 3 button or 6 button

  ;Hit 04 returns #$7f, start it anyway, though we might not need it
  move.b #$40,(a0)          ;Set bit 7 (hex $40) for (01CBMXYZ)
  rol.w #4,d5               ;ROtate Left by 4 bytes (as word) d5: 0000SACBRLDU0000
  nop

;;  move.b #$0f,d7
;;  btst #0,d6
;;  bne.s skipMXYZ


  move.b (a0),d7            ;Store the results to d7 (01CBMXYZ)
;;  ;Debug bit
;;  move.b (a0),$ff2008
;;  ;Hit 05 returns #$33 for 3 button controller, $30 for 6 button controllers
;;  move.b #0,(a0)

  andi.b #%00001111,d7      ;Mask out bits 7-4 d7:0000MXYZ
skipMXYZ:
  or.b d7,d5                ;Or d7 into d5: 0000SACBRLDUMXYZ

;;  nop
;;  nop
;;  move.b (a0),d7
;;  move.b (a0),$ff200a
;;  ;Hit 06 returns #$7f
;;  move.b #$40,(a0)
;;  nop
;;  nop
;;  move.b (a0),$ff200c
;;  ;Hit 07 returns #$33
;;  move.b #0,(a0)
;;  nop
;;  nop
;;  move.b (a0),$ff200e



  move.w d5,(_ramController01);Store controller state to RAM

  move.b #0,(a0)            ;Set bit 7 to $00 in controller 1 data port to reset things (x0SA1111).?
  move.w #0,($a11100)       ;Release Z80 bus after reading the controller

;Calculate player movement

  move.w (_ramPlayer1SpeedY),d0
  move.w (_ramController01),d1
  lea (_ramPlayer1Y),a0
  lea (_ramPlayer2Y),a1
  btst #4,d1
  bne.s testP1Up
  sub.w d0,(a0)
testP1Up:
  btst #5,d1
  bne.s skipP1Movement
  add.w d0,(a0)

skipP1Movement:

;Reset player position as needed

  ;Check player 1 Y min
  cmpi.w #_intYMin,(a0)
  bgt.s checkP1YMax
  move.w #_intYMin,(a0)
  ;Check player 1 Y max
checkP1YMax:
  cmpi.w #_intYMax,(a0)
  blt.s checkP2YMin
  move.w #_intYMax,(a0)
  ;Check player 2 Y min
checkP2YMin:
  cmpi.w #_intYMin,(a1)
  bgt.s checkP2YMax
  move.w #_intYMin,(a1)
  ;Check player 2 Y max
checkP2YMax:
  cmpi.w #_intYMax,(a1)
  blt.s subNext
  move.w #_intYMax,(a1)


subNext:
  jmp mainLoop              ;Loop indefinitely

hblank:
  rte

vblank:

  ;Move the contents of d0-d7 & a0-a6 to the stack
  movem.l d0-d7/a0-a6,-(a7)

  ;Clear d1
  moveq.l #0,d1

  addq #1,($ff0068)


  ;Reset runOnce flag
  ;Copy sprite data from RAM to VDP RAM
  ;Setting up the address registers again, just to be safe

  lea (_ramPlayer1Sprite),a0;Sprite attribute data in RAM
  move.b #_intNumSprites,d1 ;For some reason d2 works, but d1 doesn't...?


	lea ($c00000).l,a1			  ;load c00000 to to address pointer a5
	lea ($c00004).l,a2			  ;load c00004 to to address pointer a6
  move.w #$8f02,(a2)


;Copy sprite attributes from RAM to VDP RAM
  move.l #$44000000,(a2)    ;Set the control port $00c00004 to point to $0400 in VRAM
initSpriteLoopVBlank:
	move.l (a0)+,d0
	move.l d0,(a1)
	move.l (a0)+,d0
	move.l d0,(a1)
  dbf d1,initSpriteLoopVBlank

  ;Having done all the time-critical things in during VBlank, it's time to reset runOnce flag
  bclr #0,_ramFlagRunOnce
  ;Recover the contents of d0-d7 & a0-a6 from the stack
  movem.l (a7)+,d0-d7/a0-a6 
  rte

;Error vectors
error:
  rte

;Data

vdpConfigStart:

  ;Instructions for the VDP registers

  ;dc.w $8004
  dc.w %1000000000000100    ;bits ***4*210 - Video Mode Set Register 01
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|Enable display (video signal)
       ;|:|:|:|:|:|:|:HV counter latch enable (?)
       ;|:|:|:|:|:|:|Use 3 bits per color channel
       ;|:|:|:|:|:|:x
       ;|:|:|:|:|:|Horizontal interupt enable
       ;xxxxxxxxxxx

  ;dc.w $8164
  dc.w %1000000101100100    ;Bits 765432** - Video Mode Set Register 02
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|x
       ;|:|:|:|:|:|:|:x
       ;|:|:|:|:|:|:|SMS mode (1: Toggle SMS/Genesis display mode)
       ;|:|:|:|:|:|:PAL/NTSC (1/0)
       ;|:|:|:|:|:|DMA enable
       ;|:|:|:|:|:Vertical interrupt enable
       ;|:|:|:|:|Enable display (0: Disable display, draw backdrop color only)
       ;|:|:|:|:Toggle Genesis/SMS display mode
       ;xxxxxxxxx

  ;dc.w $8228
  dc.w %1000001000101000    ;Bits **543*** - Pattern Name Table Address - Scroll A - $a000
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|x
       ;|:|:|:|:|:|:|:x
       ;|:|:|:|:|:|:|x
       ;|:|:|:|:|:|:|
       ;|:|:|:|:|:|:|
       ;|:|:|:|:|:Bits fed************* of pattern nametable addr. for Scroll A (bet. 2000-e000)
       ;xxxxxxxxxx

  ;dc.w $8340
  dc.w %1000001101000000    ;Bits **54321* - Pattern Name Table Address - Window   - $b000
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|x
       ;|:|:|:|:|:|:|:|
       ;|:|:|:|:|:|:|:|
       ;|:|:|:|:|:|:|:|
       ;|:|:|:|:|:|:|:|
       ;|:|:|:|:|:Bits fedcb*********** pattern name table addr. for Window (bit 0 ignored in 40 cell mode)
       ;xxxxxxxxxx

  ;dc.w $8406
  dc.w %1000010000000110    ;Bits *****210 - Pattern Name Table Address - Scroll B - $c000
       ;|:|:|:|:|:|:|Bits fed************* of pattern name table addr. for Scroll B
       ;xxxxxxxxxxxxx

  ;dc.w $8502
  dc.w %1000010100000010    ;Bits *6543210 - Attribute Table Base Address - Sprites  - $0200
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|Bits fedcba9********* of for the spr. attr. table addr
       ;xxxxxxxxx

  ;dc.w $8600               ;Unused
  ;dc.w %1000011000000000

  ;dc.w $8700
  dc.w %1000011100000000    ;bits **543210 - Backdrop Color
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:Selects one of the 16 available colors from color palette
       ;|:|:|:|:|:|:
       ;|:|:|:|:|:Selects one of the four available color palettes
       ;xxxxxxxxxx

  ;dc.w $8800                ;Unused
  ;dc.w %1000100000000000

  ;dc.w $8900                ;Unused
  ;dc.w %1000100100000000

  ;dc.w $8a00                ;bits 76543210 - H Interrupt register
  dc.w %1000101000000000
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|Determines the number of lines it takes to generate an interrupt
       ;xxxxxxxxx
  ;A value of n means an interrupt is created every n+1 line. (After the line n has been drawn.)

  ;dc.w $8b00
  dc.w %1000101100000000    ;bits ****3210 - Video Mode Set Register 03
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:Horizontal Scroll Mode (00:Screen, 01:Line, 10:Cell, 11:Line)
       ;|:|:|:|:|:|:|Vertical Scroll Mode (0:Screen, 1:2 Cells)
       ;|:|:|:|:|:|:External interrupt (0:Disable, 1:Enable)
       ;xxxxxxxxxxxx

  ;dc.w $8c81
  dc.w %1000110010000001    ;bits 7***3210 - Video Mode Set Register 04 40-cell display
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|Cell mode (0: 32 cell mode, 1: 40 cell mode)
       ;|:|:|:|:|:|:|:|
       ;|:|:|:|:|:|:|Interlace mode (00:None [rec], 01:Interlaced, 10:Invalid, 11:Interlace 2x res)
       ;|:|:|:|:|:|:Enable shadow and highlighting
       ;|:|:|:|:|:|Horizontal interrupt enable
       ;|:|:|:|:|:x
       ;|:|:|:|:|x
       ;|:|:|:|:Cell mode, must have same value as bit 0
       ;xxxxxxxx

  ;dc.w $8d3f
  dc.w %1000110100111111    ;bits **543210 - Horizontal Scroll Data Table Base Address
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:Bits fedcba********** for horizontal scroll table addr.
       ;xxxxxxxxxx

  ;dc.w $8e00                ;Unused
  ;dc.w %1000111000000000

  ;dc.w $8f02                ;bits 76543210 - VDP Auto Increment Data
  dc.w %1000111100000010
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:Value to be added to the VDP's address register after every R/W to the data port
       ;xxxxxxxx

  ;dc.w $9001
  dc.w %1001000000000001    ;bits 76543210 - Scroll Size (The size of name tables for planes A and B)
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:Horizontal Scroll Size (00:32 cells, 01:64 cells, 10:Invalid, 11:128 cells)
       ;|:|:|:|:|:|:|x
       ;|:|:|:|:|:|:x
       ;|:|:|:|:|:|:
       ;|:|:|:|:|:Vertical scroll size (00:32 cells, 01:64 cells, 10:Invalid, 11:128 cells)
       ;xxxxxxxxxx

 ;dc.w $9100
  dc.w %1001000100000000    ;bits 7**43210 - Window H position
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|Window horizontal position (in 2 cols. Eg. 1: 2 columns, 2: 4 columns, etc. )
       ;|:|:|:|:|:x
       ;|:|:|:|:|x
       ;|:|:|:|:Direction (0:Draw win. to the left of position, 1:Draw win. to right of position)

  ;dc.w $9200
  dc.w %1001001000000000    ;bits 7**43210 - Window V position
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|:|Window vertical position (in units of 8 lines. Eg. 1: 8 lines, 2: 16 lines, etc. )
       ;|:|:|:|:|:x
       ;|:|:|:|:|x
       ;|:|:|:|:(0:Draw win. above position, 1:Draw win. below position)

  dc.w $9300                ;DMA Counter Lo
  dc.w $9400                ;DMA Counter Hi 
  dc.w $9500                ;DMA Source Lo
  dc.w $9600                ;DMA Source Mid
  dc.w $9700                ;DMA Source Hi


vdpConfigEnd:

tileDataStart:
  ;;;Tile 01
  ;;dc.l $00000000
  ;;dc.l $00000000
  ;;dc.l $00111100
  ;;dc.l $00111100
  ;;dc.l $00111100
  ;;dc.l $00111100
  ;;dc.l $00000000
  ;;dc.l $00000000
  ;;;Tile 01
  ;;dc.l $00001234
  ;;dc.l $00000003
  ;;dc.l $00000002
  ;;dc.l $00000001
  ;;dc.l $00000000
  ;;dc.l $00000000
  ;;dc.l $00000000
  ;;dc.l $fedcba90
  ;Alt Tile 01
  dc.l $00000004
  dc.l $00000000
  dc.l $00000000
  dc.l $00000000
  dc.l $00000000
  dc.l $00000000
  dc.l $00000000
  dc.l $00000000
  ;Tiles for ball & paddles
  ;Tile 01
  dc.l $11111111
  dc.l $11111111
  dc.l $11111111
  dc.l $11111111
  dc.l $11111111
  dc.l $11111111
  dc.l $11111111
  dc.l $11111111
  ;Tile 02
  dc.l $11111111
  dc.l $11111111
  dc.l $11111111
  dc.l $11111111
  dc.l $11111111
  dc.l $11111111
  dc.l $11111111
  dc.l $11111111
  ;Tile 03
  dc.l $11111111
  dc.l $11111111
  dc.l $11111111
  dc.l $11111111
  dc.l $11111111
  dc.l $11111111
  dc.l $11111111
  dc.l $11111111
  ;Tile 04
  dc.l $11111111
  dc.l $11111111
  dc.l $11111111
  dc.l $11111111
  dc.l $11111111
  dc.l $11111111
  dc.l $11111111
  dc.l $11111111
  
tileDataEnd:

spriteDataStart:
  ;Paddle 1
  dc.w _intYCenter
  ;;dc.w _intYMin
  ;;dc.w $00e0    ;$80==$00 on screen
  ;;dc.w %000000001000101    ;bits ******9876543210 - Sprite y
  ;;     ;|:|:|:|:|:|:|:|:
  ;;     ;|:|:|:Sprite y
  ;;     ;xxxxxx
  dc.w %0000001100000001    ;bits ****ba98*6543210 - Sprite size x, size y, and link
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|Link data
       ;|:|:|:|:x
       ;|:|:|:Size y
       ;|:|:Size x
       ;xxxx
  dc.w %0000000000000001    ;bits fedcba9876543210 - Sprite priority, palette, flip y, flip x, tile 
       ;|:|:|:|:|:|:|:|:
       ;|:|:|Tile no
       ;|:|:Flip x
       ;|:|Flip y
       ;|Palette
       ;Priority
  dc.w _intXMin
  ;;dc.w $0080    ;bits *******876543210 - Sprite X
  ;;dc.w %000000011010001    ;bits *******876543210 - Sprite X
  ;;     ;|:|:|:|:|:|:|:|:
  ;;     ;|:|:|:|Sprite X
  ;;     ;xxxxxxx

  ;Paddle 2
  dc.w _intYCenter
  ;;dc.w _intYMin
  ;;dc.w $0080
  ;;dc.w %0000000011010001    ;bits ******9876543210 - Sprite y
  ;;     ;|:|:|:|:|:|:|:|:
  ;;     ;|:|:|:Sprite y
  ;;     ;xxxxxx
  dc.w %0000001100000010    ;bits ****ba98*6543210 - Sprite size x, size y, and link
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|Link data
       ;|:|:|:|:x
       ;|:|:|:Size y
       ;|:|:Size x
       ;xxxx
  dc.w %0000000000000001    ;bits fedcba9876543210 - Sprite priority, palette, flip y, flip x, tile 
       ;|:|:|:|:|:|:|:|:
       ;|:|:|Tile no
       ;|:|:Flip x
       ;|:|Flip y
       ;|Palette
       ;Priority
  dc.w _intXMax
  ;;dc.w $01b8
  ;;dc.w %0000000110100010   ;bits *******876543210 - Sprite X
  ;;     ;|:|:|:|:|:|:|:|:
  ;;     ;|:|:|:|Sprite X
  ;;     ;xxxxxxx

  ;Ball
  dc.w _intYCenter+8
  ;;dc.w %0000000011010001    ;bits ******9876543210 - Sprite y
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:Sprite y
       ;xxxxxx
  dc.w %0000010100000011    ;bits ****ba98*6543210 - Sprite size x, size y, and link
       ;|:|:|:|:|:|:|:|:
       ;|:|:|:|:|Link data
       ;|:|:|:|:x
       ;|:|:|:Size y
       ;|:|:Size x
       ;xxxx
  dc.w %0000000000000001    ;bits fedcba9876543210 - Sprite priority, palette, flip y, flip x, tile 
       ;|:|:|:|:|:|:|:|:
       ;|:|:|Tile no
       ;|:|:Flip x
       ;|:|Flip y
       ;|Palette
       ;Priority
  dc.w _intXCenter
  ;;dc.w %0000000110000010   ;bits *******876543210 - Sprite X
  ;;     ;|:|:|:|:|:|:|:|:
  ;;     ;|:|:|:|Sprite X
  ;;     ;xxxxxxx
spriteDataEnd:

paletteDataStart:
  dc.w $0222,$0fff,$00ff,$0066,$0088,$00aa,$00cc,$00ee,$0002,$0004,$0006,$0008,$000a,$000c,$000e,$0fff
  dc.w $00ff,$0f00,$0f22,$0f44,$0f66,$0f88,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
  dc.w $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000
  dc.w $0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0000,$0f99,$0fbb,$0fdd,$0fff
paletteDataEnd:




romEnd:
