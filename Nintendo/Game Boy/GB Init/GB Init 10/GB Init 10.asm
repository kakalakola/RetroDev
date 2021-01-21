;----------------------------------------------------
;GameBoy ASM Project 0.10 - 2020.11.09
;CALLs, data compression, and motion vectors
;by Saad Azim
;----------------------------------------------------

;Tested, and works in MAME and BGB.

;Variables
_SPRITE_COLOR_0:     equ %11000000
_SPRITE_COLOR_1:     equ %01001000
_SPRITE_COLOR_1_ALT: equ %00000100

_BG_COLOR:           equ %00110110
_BG_COLOR_ALT:       equ %01001000
_BG_COLOR_HBLANK:    equ %10000101
                        ;|:|:|:|:
                        ;|:|:|:Color 00 (fill/transparency)
                        ;|:|:Color 01
                        ;|:Color 02
                        ;Color 03

;Using an address in RAM as the VBlank & HBlank subroutines. This way, it's no longer a static block of code in ROM
_VBLANK:             equ $c000
_LCD_STAT:           equ $c004

_SPRITE_DATA:        equ $c100
_SPRITE_01_DATA:     equ $c108

_MAP_DATA:           equ $d000

;Using HRAM to store screen scroll values, and input status
_MOTION_VECTOR_Y:    equ $80
_MOTION_VECTOR_X:    equ $81

_SCREEN_SCROLL_Y:    equ $82
_SCREEN_SCROLL_X:    equ $83

_PALETTE_BG:         equ $84
_PALETTE_SPRITE_0:   equ $85
_PALETTE_SPRITE_1:   equ $86

_FLAGS:              equ $8d

_INPUT_RAW:          equ $8e
_INPUT_HOLD:         equ $8f

_OAM_ROUTINE:        equ $ff90 ;Using OAM DMA to copy sprite data from RAM

cpu gbz80

  ;TNIASM uses "|" as a line separator, which can be used to tidy up the multi-line header code into something more compact. :D

  ;Vector table                                                                     $0000-$00FF
  jp rst0    | ds $05,$00 | jp rst1      | ds $05,$00 | jp rst2  | ds $05,$00 | jp rst3   | ds $05,$00
  jp rst4    | ds $05,$00 | jp rst5      | ds $05,$00 | jp rst6  | ds $05,$00 | jp rst7   | ds $05,$00
  jp _VBLANK | ds $05,$00 | jp _LCD_STAT | ds $05,$00 | jp Timer | ds $05,$00 | jp Serial | ds $05,$00
  jp Joypad  | ds $0100-$,$00

  nop | jp CodeStart        ;Beginning of code execution point                      $0100-$0103

  ;Nintendo logo                                                                    $0104-$0133
  db $ce,$ed,$66,$66,$cc,$0d,$00,$0b,$03,$73,$00,$83,$00,$0c,$00,$0d
  db $00,$08,$11,$1f,$88,$89,$00,$0e,$dc,$cc,$6e,$e6,$dd,$dd,$d9,$99
  db $bb,$bb,$67,$63,$6e,$0e,$ec,$cc,$dd,$dc,$99,$9f,$bb,$b9,$33,$3e

  db "GB INIT 0.10   "      ;Game Title 15 bytes                                    $0134-$0142

  ;GB only, New Licensee Code (H,L), No SGB, ROM only, 32Kb ROM, 0Kb (S)RAM, JP     $0143-$014A
  dw $0000,$0000,$0000,$0000

  db $01                    ;Licensee code: Nintendo :3                                   $014B

  ;Mask ROM version: 0, Header Checksum, ROM Checksum (H,L)                         $014C-$014F
  dw $0000,$0000

CodeStart:                                                                               ;$0150
  di                        ;Disable interrupt
  xor a

  ;Clear RAM ($C000-$DFFF) by calling the subroutine "blockCopy"
  ld hl,$c000
  ld (hl),a                 ;Clear first byte of RAM
  ld bc,$1ffe               ;$2000-$0001 bytes to copy, since the first byte was manually cleared :D
  ld de,$c001               ;Copy from C000 to C0001, then increment both resigsters
  call blockCopy            ;Block copy format: HL==source, BC==count, DE==destination

  ;Calls work sort of like functions. The return address is pushed to the stack, before the CPU jumps to blockCopy. Once blockCopy has finished, RET jumps to the return address, and the return address is pulled from the stack. It adds a few cycles to the overhead, but allows for reusing repeating code. ^_^

  ld c,$80
loopClearHRAM:
  ldh (c),a
  inc c
  jp nz,loopClearHRAM

  ;Copy OAM DMA code to HRAM. The code *has* to be executed from HRAM.
  ld hl,oamDMA              ;Source address
  ld b,oamDMAEnd-oamDMA     ;Size
  ld c,_OAM_ROUTINE and $ff ;Destination address, same as LD C,$90
loopCopyDMARoutine:
  ldi a,(hl)
  ldh (c),a
  inc c
  dec b
  jr nz,loopCopyDMARoutine

  ;Copy sprite data to RAM
  ld hl,sprites
  ld bc,$20                 ;8 sprites, 4 bytes per sprite
  ld de,_SPRITE_DATA
  call blockCopy

  ;Decompress map data to RAM
  ld hl,tileMap
  ld b,windowMapEnd/2-tileMap/2
  ld de,_MAP_DATA
loopParseMapData:
  ld c,(hl)                 ;Load byte count
  inc hl
  ldi a,(hl)                ;Load byte
loopDecompressMapData:
  ld (de),a
  inc de
  dec c
  jr nz,loopDecompressMapData
  dec b
  jr nz,loopParseMapData
  
  ;Setup stack pointer
  ld sp,$ffff

  ;VRAM in Game Boy should only be accessed by the CPU when the LCD is turned off. The LCD should ONLY be turned off during VBlank, otherwise it will PERMANENTLY DAMAGE THE SCREEN ON ACTUAL HADWARE.

  ld hl,_VBLANK             ;Using an alternate VBlank subroutine to disable LCD, and copy tiles & map
  ld  a,$c3                 ;$C3 is the byte code for JP
  ldi (hl),a
  ld a,updateVRAM and $ff
  ldi (hl),a
  ld a,updateVRAM>>8
  ldi (hl),a                ;JP instructions are encoded as JP LOW BYTE HIGH BYTE

  ld a,%00000001            ;Enable VBlank, enable interrupts, wait for VBlank
  ldh ($ff),a
  ei
  halt
  nop

  ;Having copied tile & map data to VRAM, boot should continue as usual

  ;Copy VBlank & LCDStat (HBlank) code to RAM
  ld hl,VBlankJump
  ld bc,LCDStatEnd01-VBlankJump
  ld de,_VBLANK
  call blockCopy

  ;Initialize registers
  ld hl,bootInit
  ld b,bootInitEnd/2-bootInit/2
  call blockCopyReg

  ei                        ;Enable Interrupts

MainLoop:
  xor a

  ;Check _FLAGS
  ldh a,(_FLAGS)
  bit 0,a
  jr nz,MainLoop

  set 0,a
  ldh (_FLAGS),a

  ;Reset motion vectors
  xor a
  ldh (_MOTION_VECTOR_Y),a
  ldh (_MOTION_VECTOR_X),a

  ;Check input
  ldh a,(_INPUT_RAW)
  ld b,a 

  ;Change BG color if A is pressed
  ld a,_BG_COLOR
  bit 0,b
  jr nz,skipPalette
  ld a,_BG_COLOR_ALT

skipPalette:
  ldh (_PALETTE_BG),a

  ;Play a sound clip B button is pressed, and no sound is currently playing
  bit 1,b                   
  jr nz,checkRight

  ;Check to see if the sound is already playing. Without this bit of code, holding down B continuously resets the sound, creating something rather grating... -_-
  ldh a,($26)
  bit 3,a
  jr nz,checkRight

  ;Register $FF26
  ;00000000
  ;|:|:|:|:
  ;|:|:|:|Channel 1 read-only (0==No sound||1==Sound is currently playing)
  ;|:|:|:Channel 2 read-only (0==No sound||1==Sound is currently playing)
  ;|:|:|Channel 3 read-only (0==No sound||1==Sound is currently playing)
  ;|:|:Channel 4 read-only (0==No sound||1==Sound is currently playing)
  ;|xxx
  ;All sound (R/W) (0==off||1==On)

  ;Reset timer and restart noise channel only if the audio has finished playing
  ld a,%00000001
  ldh ($20),a
  ld a,%11000000
  ldh ($23),a

checkRight:
  ;Set BG scroll values if the d-pad was pressed in any direction
  ;Also, ADD and SUB swapped because it looks like the sprites are moving "backwards" otherwise
  
  bit 4,b
  jr nz,checkLeft

  ld a,-1                   ;8-bit -1 is the same as 255
                            ;The idea being that adding 255 to 1 rolls the value over to 0, same as adding -1 to 1
  ldh (_MOTION_VECTOR_X),a

  ;Update meta sprite 0 X flip, and tiles
  ld a,(_SPRITE_DATA+3)
  res 5,a
  ld (_SPRITE_DATA+3),a
  ld (_SPRITE_DATA+7),a
  ld a,$08
  ld (_SPRITE_DATA+2),a
  ld a,$0a
  ld (_SPRITE_DATA+6),a

checkLeft:
  bit 5,b
  jr nz,checkUp

  ld a,1
  ldh (_MOTION_VECTOR_X),a

  ;Update meta sprite 0 X flip, and tiles
  ld a,(_SPRITE_DATA+3)
  set 5,a
  ld (_SPRITE_DATA+3),a
  ld (_SPRITE_DATA+7),a
  ld a,$0a
  ld (_SPRITE_DATA+2),a
  ld a,$08
  ld (_SPRITE_DATA+6),a

checkUp:
  bit 6,b
  jr nz,checkDown

  ld a,1
  ldh (_MOTION_VECTOR_Y),a

checkDown:
  bit 7,b
  jr nz,processVectors

  ld a,-1
  ldh (_MOTION_VECTOR_Y),a

processVectors:
  ldh a,(_MOTION_VECTOR_Y)  ;Copy motion vectors Y & X to registers B & C, respectively
  ld b,a
  ldh a,(_MOTION_VECTOR_X)
  ld c,a

  ;Update screen scroll
  ldh a,(_SCREEN_SCROLL_Y)
  sub b
  ldh (_SCREEN_SCROLL_Y),a

  ldh a,(_SCREEN_SCROLL_X)
  sub c
  ldh (_SCREEN_SCROLL_X),a

  ;Update sprites
  ld hl,_SPRITE_01_DATA     ;Load address of first sprite data
  ld d,$06                  ;6 hardware sprites to update for 3 meta sprites
loopUpdateSpriteMotion:
  ld a,(hl)                 ;Load Y position of sprite
  add b                     ;Update & store Y position of sprite
  ldi (hl),a

  ld a,(hl)                 ;Load, update, and store X position of sprite
  add c
  ldi (hl),a

  inc hl                    ;Increment HL until it points to the Y position of next sprite
  inc hl

  dec d                     ;Loop as needed
  jr nz,loopUpdateSpriteMotion

  jp MainLoop

VBlank:
  push af
  push bc

  ld a,%11110111            ;Enable sprites
  ldh ($40),a

  jp _OAM_ROUTINE
endOAM:

  ;Update scroll
  ldh a,(_SCREEN_SCROLL_Y)
  ldh ($42),a
  ldh a,(_SCREEN_SCROLL_X)
  ldh ($43),a

  ;Update sprite & BG color palettes
  ldh a,(_PALETTE_BG)
  ldh ($47),a
  ldh a,(_PALETTE_SPRITE_0)
  ldh ($48),a
  ldh a,(_PALETTE_SPRITE_1)
  ldh ($49),a

  ;Reset HBlank routine
  ld hl,LCDStat01
  ld bc,LCDStatEnd01-LCDStat01
  ld de,_LCD_STAT
  call blockCopy


  ;Initialize line counter to trigger LCDStat (HBlank)
  ld a,$4c
  ldh ($45),a

  ;Check button input
  ld a,%00100000
       ;|:|:|:|:
       ;|:|:|:|Right||Button A, read only
       ;|:|:|:Left||Button B, read only
       ;|:|:|Up||Select, read only
       ;|:|:Down||Start, read only
       ;|:Button/Direction select (00==?||01==Direction||10==Button||11==Reset?)
       ;xx
  ldh ($00),a               ;Set port ($FF00) to check for button input
  ldh a,($00)
  ldh a,($00)               ;Read data repeatedly to stabilize input

  and $0f
  swap a
  ld b,a

  ld a,%00010000
  ldh ($00),a               ;Set port to check for directional input

  ldh a,($00)
  ldh a,($00)
  ldh a,($00)
  ldh a,($00)
  ldh a,($00)
  ldh a,($00)               ;Read data repeatedly to stabilize input

  and $0f
  or b

  ;Process input bits before storing
  ld b,a
  ldh a,(_INPUT_RAW)
  or d

  ldh (_INPUT_HOLD),a
  ld a,b
  ldh (_INPUT_RAW),a

  ld a,%00110000            ;Reset port
  ldh ($00),a

  ldh a,(_FLAGS)
  res 0,a                   ;RESet bit 0 in A (setting it to 0)
  ldh (_FLAGS),a

  pop bc
  pop af

  reti

;Interrupt routines to be copied to to RAM
VBlankJump:
  jp VBlank
VBlankJumpEnd:
  db $00                    ;Padding, so LCDStat01 starts at $c004
LCDStat01:
  ;This section needs to be dealt with ASAP, so only AF is backed up at start
  push af
  ld a,_BG_COLOR_HBLANK     ;Change background color when HBlank occurs
  ldh ($47),a

  ;Having changed the BG colors, it's time to setup the next HBlank
  push hl
  push bc
  push de

  ;Copy routine to run during the next HBlank
  ld hl,LCDStat02
  ld bc,LCDStatEnd02-LCDStat02
  ld de,_LCD_STAT
  call blockCopy

  ;Modify the line counter, so it looks like the sprites are going "behind" the window. :D
  ld a,$80
  ldh ($45),a

  pop de
  pop bc
  pop hl
  pop af

  reti
LCDStatEnd01:

LCDStat02:
  push af

  ld a,_BG_COLOR            ;Reset BG color
  ldh ($47),a

  ld a,%11110101            ;Disable sprites
  ldh ($40),a

  pop af
  reti
LCDStatEnd02:

;DMA routine, to be copied to HRAM
oamDMA:
  ld a,_SPRITE_DATA>>8
  ldh ($46),a               ;The DMA start register is $FF46. Writing a the higy byte of
                            ;the DMA source address starts the DMA
  ld a,$40                  ;Wait for DMA to finish via a loop
waitDMA:
  dec a
  jr nz,waitDMA
  jp endOAM
oamDMAEnd:

;Code for copying data where C!=$00
blockCopy:                  ;HL==source, BC==counter, DE==destination
  inc b                     ;There's a glitch of sorts, with using 16-bit counters
                            ;For example, setting BC to $0100, and looping as normal will cause the routine to copy $0100 bytes of data. But setting BC to $0101 will cause the same routine to copy exactly ONE byte of data. The key to dealing with values where C != $00 is to increment B by one.

;Code for copying data where C==$00
blockCopyNoFix:
  ldi a,(hl)
  ld (de),a
  inc de
  dec c
  jr nz,blockCopyNoFix
  dec b
  jr nz,blockCopyNoFix
  ret

;Code for copying data/register pair from data blocks
blockCopyReg:              ;HL==source, B==counter
  ldi a,(hl)
  ld c,(hl)
  ldh (c),a
  inc hl
  dec b
  jr nz,blockCopyReg
  ret

;Unused interrupt pointers
Timer: | Serial:| Joypad: | rst0: | rst1: | rst2: | rst3: | rst4: | rst5: | rst6: | rst7:
  reti

updateVRAM:                 ;There's not much of a point in backing up any registers here

  ;Enable LCD Control register, disable display, disable control status
  ld hl,vramInit
  ld b,$03
  call blockCopyReg
  push hl                   ;Push HL after setting 3 registers, for latter use

  ;Copy tiles to VRAM
  ld hl,tiles
  ld bc,$f0
  ld de,$8010
  call blockCopy

  ;Copy tile and window maps to VRAM by calling a subroutine
  ld hl,_MAP_DATA
  ld bc,$0800
  ld de,$9800
  call blockCopyNoFix

  pop hl                    ;Once tiles & maps have been copied to VRAM, resume setting registers to
                            ;enable things like BG, Sprites, and Window
  ld b,$04
  call blockCopyReg

  ret                       ;Don't want to enable interrupts yet :D

;Data block (format: value,register)
vramInit:
  ;First half, before copying tile & map to VRAM
  db %00000010,$ff          ;Enable LCD Control Register, disable VBlank
     ;|:|:|:|:
     ;|:|:|:|vBlank
     ;|:|:|:LCD Control enable
     ;|:|:|Timer Overflow
     ;|:|:End of serial I/O transfer
     ;|:|Transition high>low pins p10-p13 ...?
     ;xxx

  db %00000000,$40          ;LCD Control - Disable everything
     ;|:|:|:|:
     ;|:|:|:|Enable BG (and Window) (off||on)
     ;|:|:|:Enable Sprite (off||on)
     ;|:|:|Sprite Size (8x8||8x16)
     ;|:|:BG Tile Map Location ($9800-$9bff||$9c00-$9fff)
     ;|:|BG Tile Location ($8800-$97ff||$8000-$8fff)
     ;|:Enable Window (off||on)
     ;|Window Tile Map Location ($9800-$9bff||$9c00-$9fff)
     ;Enable LCD (off||on)

  db %00000000,$41          ;LCD Control Status - Disable everything
     ;|:|:|:|:
     ;|:|:|:xx - Mode flag, read only
     ;|:|:|Coincidence Flag (LYC!=LY||LYC==LY), read only
     ;|:|:Mode 0, HBlank interrupt check (disable||enable)
     ;|:|Mode 1, VBlank interrupt check (disable||enable)
     ;|:Mode 2, OAM interrupt check (disable||enable)
     ;|LYC=LY Coincidencd (scanline counter) interrupt check (disable||enable)
     ;x

  ;Second half, after copying tile & map to VRAM

  ;Looks like Window Y & X positions need to be set BEFORE setting LCD control register & LCD control status. Otherwise, the program crashes in BGB, but not MAME or No$gmb - 

  db $80,$4a                ;Set window Y position

  db $07,$4b                ;Set Window X position. Values of 1-6, and 166 ($A6) are unreliable
                            ;Setting Window X positon to 0 causes the causes windows to scroll with screen scroll X%8

  db %11110111,$40          ;Modified to display 8x16 sprites
     ;|:|:|:|:
     ;|:|:|:|Enable BG (and Window) (off||on)
     ;|:|:|:Enable Sprite (off||on)
     ;|:|:|Sprite Size (8x8||8x16)
     ;|:|:BG Tile Map Location ($9800-$9bff||$9c00-$9fff)
     ;|:|BG Tile Location ($8800-$97ff||$8000-$8fff)
     ;|:Enable Window (off||on)
     ;|Window Tile Map Location ($9800-$9bff||$9c00-$9fff)
     ;Enable LCD (off||on)

  db %00000000,$41          ;LCD Control Status
     ;|:|:|:|:
     ;|:|:|:xx - Mode flag, read only
     ;|:|:|Coincidence Flag (LYC!=LY||LYC==LY), read only
     ;|:|:Mode 0, HBlank interrupt check (disable||enable)
     ;|:|Mode 1, VBlank interrupt check (disable||enable)
     ;|:Mode 2, OAM interrupt check (disable||enable)
     ;|LYC=LY Coincidencd (scanline counter) interrupt check (disable||enable)
     ;x

  ;Mode flag:
  ; - 00 - HBlank in progress
  ; - 01 - VBlank in progress
  ; - 10 - OAM being accessed by LDC Controller
  ; - 11 - Both OAM and VRAM are being accessed by LCD Controller

bootInit:
  db %10101111,$24          ;Initialize audio channel volume
     ;|:|:|:|:
     ;|:|:|Volume for left channel
     ;|:|:Left channel (0==off||1==on)
     ;|Volume for right channel
     ;Right channel (0==off||1==on)

  db %10001000,$25          ;Initialize audio channels
     ;|:|:|:|:
     ;|:|:|:|Enable audio channel 1 R
     ;|:|:|:Enable audio channel 2 R
     ;|:|:|Enable audio channel 3 R
     ;|:|:Enable audio channel 4 R
     ;|:|Enable audio channel 1 L
     ;|:Enable audio channel 2 L
     ;|Enable audio channel 3 L
     ;Enable audio channel 4 L
  
  ;Channel 4 (noise) configuration
  db %00000001,$20          ;Sound duration
     ;|:|:|:|:
     ;|:Sound length, lower value == longer sound
     ;xx

  db %00111010,$21          ;Envelope settings
     ;|:|:|:|:
     ;|:|:|Number of envelope sweep (0==Stop Operation)
     ;|:|:Envelope mode (0==Atteunuate||1==Amplify)
     ;Initial volume of envelope

  ;Atteunuation lowers the volume with each envelope sweep. Amplication increases the volume. Number of envelope sweeps determing how long it takes the sound to go from full volume to zero with atteunuation, or zero to full volume with amplification. 

  db %00101010,$22          ;Noise settings
     ;|:|:|:|:
     ;|:|:|Dividing ratio frequency (roughness)
     ;|:|:Polynomial counter step (0==15 bit||1==7 bit, sounds electronic)
     ;Shift clock frequency of the polynomial counter (pitch), 1110 and 1111 are prohibited

  db %11000000,$23          ;Sound initialization & counter enable/disable
     ;|:|:|:|:
     ;|:xxxxxx
     ;|Counter (0==Sound plays forever||1==Sound plays for the duration of time set with $FF20)
     ;Initialize (1==Restart sound)

  db $ff,_INPUT_RAW         ;Set initial input values
  db $ff,_INPUT_HOLD

  ;Save palettes in HRAM
  db _BG_COLOR,_PALETTE_BG
  db _SPRITE_COLOR_0,_PALETTE_SPRITE_0
  db _SPRITE_COLOR_1,_PALETTE_SPRITE_1

  db $40,$44                ;Initialize line counter

  db %01000000,$41          ;Enable scanline counter
     ;|:|:|:|:
     ;|:|:|:xx - Mode flag, read only
     ;|:|:|Coincidence Flag (LYC!=LY||LYC==LY), read only
     ;|:|:Mode 0, LCD Stat interrupt check (disable||enable)
     ;|:|Mode 1, VBlank interrupt check (disable||enable)
     ;|:Mode 2, OAM interrupt check (disable||enable)
     ;|LYC=LY Coincidencd (scanline counter) interrupt check (disable||enable)
     ;x

  db %00000011,$ff          ;Enable interrupts
     ;|:|:|:|:
     ;|:|:|:|VBlank
     ;|:|:|:LCDStat
     ;|:|:|Timer Overflow
     ;|:|:End of serial I/O transfer
     ;|:|Transition high>low pins p10-p13 ...?
     ;xxx
bootInitEnd:

;Tile data, $F0 bytes
tiles:
  db $7e,$7e,$ff,$ef,$ff,$d7,$ff,$83,$ff,$bb,$ff,$bb,$ff,$bb,$7e,$7e ;A
  db $7e,$7e,$ff,$c7,$ff,$db,$ff,$c3,$ff,$dd,$ff,$dd,$ff,$c3,$7e,$7e ;B
  db $7e,$7e,$ff,$e3,$ff,$df,$ff,$df,$ff,$df,$ff,$df,$ff,$e3,$7e,$7e ;C
  db $7e,$7e,$ff,$c7,$ff,$db,$ff,$dd,$ff,$dd,$ff,$dd,$ff,$c3,$7e,$7e ;D

  db $7e,$ff,$ff,$dd,$ff,$dd,$ff,$d5,$ff,$d5,$ff,$c9,$ff,$dd,$7e,$ff ;W

  db $7e,$7e,$ff,$c3,$ff,$df,$ff,$c7,$ff,$df,$ff,$df,$ff,$c3,$7e,$7e ;E
  db $7e,$7e,$ff,$c3,$ff,$df,$ff,$c7,$ff,$df,$ff,$df,$ff,$df,$7e,$7e ;F

  db $7f,$7f,$c0,$bf,$bf,$ff,$bf,$f0,$bf,$f0,$bf,$f3,$bf,$f3,$bf,$f0 ;16x16 E Tile 0
  db $bf,$f0,$bf,$f3,$bf,$f3,$bf,$f0,$bf,$f0,$bf,$ff,$c0,$bf,$7f,$7f ;16x16 E Tile 1
  db $fe,$fe,$03,$fd,$fd,$ff,$fd,$0f,$fd,$0f,$fd,$ff,$fd,$ff,$fd,$3f ;16x16 E Tile 2
  db $fd,$3f,$fd,$ff,$fd,$ff,$fd,$0f,$fd,$0f,$fd,$ff,$03,$fd,$fe,$fe ;16x16 E Tile 3

  db $7f,$7f,$c0,$bf,$bf,$ff,$bf,$f0,$bf,$f0,$bf,$f3,$bf,$f3,$bf,$f0 ;16x16 F Tile 0
  db $bf,$f0,$bf,$f3,$bf,$f3,$bf,$f3,$bf,$f3,$bf,$ff,$c0,$bf,$7f,$7f ;16x16 F Tile 1
  db $fe,$fe,$03,$fd,$fd,$ff,$fd,$0f,$fd,$0f,$fd,$ff,$fd,$ff,$fd,$3f ;16x16 F Tile 2
  db $fd,$3f,$fd,$ff,$fd,$ff,$fd,$ff,$fd,$ff,$fd,$ff,$03,$fd,$fe,$fe ;16x16 F Tile 3

sprites:
  ;    Y    X  Tile   RYXP     (pRiority, Y flip, X flip, Palette)
  db $50, $50,  $08, %00000000 ;E, left half, using palette 0
  db $50, $58,  $0a, %00000000 ;E, right half, using palette 0
                               ;Bit 0 of tile index is ignored when in 8x16 sprite mode. So setting tile index to 9 has the same effect as setting tile index to 8

  db $43, $37,  $08, %00010000 ;E, using palette 1
  db $43, $3f,  $0a, %00010000 ;E, using palette 1
  db $69, $45,  $0c, %00010000 ;F, using palette 1
  db $69, $4d,  $0e, %00010000 ;F, using palette 1
  db $48, $6a,  $0e, %00110000 ;F, using palette 1, X flip
  db $48, $72,  $0c, %00110000 ;F, using palette 1, X flip

;Tile map, $0400 bytes. The tile map area in Game Boy VRAM 32x32 tiles (256x256 pixels) in size. Though only 160x144 pixels are visible at any given time.
tileMap:                  ;RLE compression: byte count, byte
  db $10,1,$10,2,$10,1,$10,2,$10,1,$10,2,$10,1,$10,2
  db $10,1,$10,2,$10,1,$10,2,$10,1,$10,2,$10,1,$10,2
  db $10,1,$10,2,$10,1,$10,2,$10,1,$10,2,$10,1,$10,2
  db $10,1,$10,2,$10,1,$10,2,$10,1,$10,2,$10,1,$10,2
  db $10,3,$10,4,$10,3,$10,4,$10,3,$10,4,$10,3,$10,4
  db $10,3,$10,4,$10,3,$10,4,$10,3,$10,4,$10,3,$10,4
  db $10,3,$10,4,$10,3,$10,4,$10,3,$10,4,$10,3,$10,4
  db $10,3,$10,4,$10,3,$10,4,$10,3,$10,4,$10,3,$10,4
tileMapEnd:

windowMap:                  ;RLE compression: byte count, byte
  db 0,5,0,5,0,5,0,5
windowMapEnd:

  ds $8000-$,$00            ;Pad to 32k