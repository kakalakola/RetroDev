;================================================================
;Rotating Tunnel Demo v2 - 2023.06.12
;For the SEGA Master System
;by Saad Azim
;
; Released under GNU General Public License v3.0
;================================================================

;Seems to work as intended on Kega Fusion and MAME.
;Causes graphical issues in Emulicious.

;----------------------------------------------------------------
;Variables
;----------------------------------------------------------------


_PALETTE_Y_SCROLL: equ $c048  ;Controls the vertical "scroll" of the palettes
_PALETTE_X_SCROLL: equ $c04a  ;Controls the horizontal "scroll" of the palettes
                            ;Kinda' clueless as to what to do here -_-

                            ;Updates every $7F frames (slightly over 2 seconds)
                            ; 0 - no movement
                            ; 1 - slow scroll up
                            ; 2 - fast scroll up
                            ; 3 - fast scroll up
                            ; 4 - fast scroll up
                            ; 5 - fast scroll up
                            ; 6 - fast scroll up
                            ; 7 - slow scroll up
                            ; 8 - no movement
                            ; 9 - slow scroll down
                            ; a - fast scroll down
                            ; b - fast scroll down
                            ; c - fast scroll down
                            ; d - fast scroll down
                            ; e - fast scroll down
                            ; f - slow scroll down

_STATUS_FLAG:       equ $c00f ;Something to keep track of the program state
                              ; %00000000
                              ;  |:|:|:|:
                              ;  |:|:|:|Wait for VBlank
                              ;  |:|:|:|:
                              ;  |:|:|:|:
                              ;  |:|:|:|:
                              ;  |:|:|:|:
                              ;  |:|:|:|:
                              ;  |:|:|:|:
                              ;  |:|:|:|:
  org $c010
_FRAME_COUNTER:     rw 1    ;Drives most of the animation & updates
                            ;Going with a 16 bit counter at the moment

_FRAME_COUNTER_X2:  rw 1    ;Like frame counter, but increments by 2
                            ;Mainly used for 16 bit addresses

_FRAME_COUNTER_X4_LOOP_8: rw 1
;Hopefully, this makes dealing with jump tables easier. The counter increments by 4 (for each JP nnnn followed by NOP). It's actually a LOT easier dealing with numbers that are a power of 4 than anything else. :D

_TUNNEL_ANIMATION:  rw 1
_TUNNEL_Y_SPEED:    rw 1
_TUNNEL_X_SPEED:    rw 1


_RAW_INPUT:         rw 1    ;Raw user input, 2 bytes
_P1_INPUT_D:        rw 1    ;Processed d-pad input for player 1
                            ;Left shifted, so it can act as an index into a jump table
                            ;Needs to be a 16-bit word, so that it can be loadeded into BC with a single instruction (and then added to HL, for an address to jump to)

_P2_INPUT_D:        rw 1

_LINE_COUNTER:      equ $c040                 ;Something to keep track of, and disable HBlank IRQ with


  org $c100
_PALETTE_FIXED:     rw 0
_PALETTE_FIXED_BG:  rw 8
_PALETTE_FIXED_SP:  rw 8

  org $c120
_P1_SCORE:          rw 2
_P2_SCORE:          rw 2
_HIGH_SCORE:        rw 2
_P1_LIFE:           rb 1
_P2_LIFE:           rb 1
_P1_VELOCITY:       rb 1
_P2_VELOCITY:       rb 1
_P1_SHIELD:         rb 1
_P2_SHIELD:         rb 1
_BOSS_SHIELD:       rb 1

;UI elements, padded out to 8 words ($10 bytes) each
_UI_P1_SCORE:       rw 8
_UI_P2_SCORE:       rw 8
_UI_HIGH_SCORE:     rw 8
_UI_P1_LIFE:        rw 8
_UI_P2_LIFE:        rw 8
_UI_P1_STAT:        rw 8
_UI_P1_VELOCITY: equ _UI_P1_STAT+2
_UI_P1_SHIELD:   equ _UI_P1_STAT+8
_UI_P2_STAT:        rw 8
_UI_P2_VELOCITY: equ _UI_P2_STAT+2
_UI_P2_SHIELD:   equ _UI_P2_STAT+8
_UI_BOSS_SHIELD:    rw 8

;Player variables (low & high bytes of player Y & X)
_P1_YX:             rw 2
_P2_YX:             rw 2


  org $c1d0
_PLAYER_YX_TEMP:    rw 2
_PLAYER_VECTOR_TEMP: rw 2
_PLAYER_INPUT_TEMP: rb 1

  org $c200
_OAM_Y:     rb 0
_P1_OAM_Y:  rw 1            ;Reserve 2 bytes (1 word) for player 1 Y position
_P2_OAM_Y:  rw 1
_E_OAM_Y:   rw 1

  org $c300
_OAM_XT:    rb 0
_P1_OAM_XT: rw 2            ;Reserve 2 bytes (2 words) for player 1 X position and tile index
_P2_OAM_XT: rw 2

_E_OAM_XT:  rw 2

  org $c380                 ;Stuff for palette cycling
_OAM_Y_SRC_0:  rw 1
_OAM_Y_AMT_0: rw 1
_OAM_Y_SRC_1:  rw 1
_OAM_Y_AMT_1: rw 1
_OAM_XT_SRC_0:  rw 1
_OAM_XT_AMT_0: rw 1
_OAM_XT_SRC_1:  rw 1
_OAM_XT_AMT_1: rw 1


  org $c400                 ;Reserve RAM for basic sprite info
                            ;Sprite Y, X, tile index, animation frame 
_P1_SPRITE: rb 4
_P2_SPRITE: rb 4

;Large enemies (2x2 + tail)
_E1XL_SPRITE: rb 4
_E2XL_SPRITE: rb 4
_E3XL_SPRITE: rb 4
_E4XL_SPRITE: rb 4

_E1_SPRITE: rb 4
_E2_SPRITE: rb 4 
_E3_SPRITE: rb 4 
_E4_SPRITE: rb 4 

_IRQ_RAM:           equ $d800                 ;Location of IRQ handler in RAM


;Specify the stock Z80 CPU
cpu z80

  org $0000

;Z80 boot area. Unlike boot vectors on the 6502 or 68000 which contain [addresse] or JMP [address], the Z80 has "blocks" which contain code. $0000, $0008, $0010, $0018, $0020, $0028, $0030, and $0038 are special in that they can be accessed by the RST n.

;RST is similar to CALL, except it takes up 1 byte. CALL takes 3 bytes.

  ;Reset code                                                         $0000
  di                        ;Disable interrupts
  jp InitSystem
  ds $0038-$,$00            ;Pad from "here" ($) until $0037 with $00

  ;Interrupt handler                                                  $0038
  jp _IRQ_RAM               ;Modify IRQ handler to jump to RAM
  ds $0066-$,$00

  ;NMI handler, for the Reset Button                                  $0066
  jp nmi
  ds $0100-$,$00

;Read/Writes to $00-ff does not affect the rom in address       $0000-$00FF
  ;For $00-$$3f (traditionally $3e & $3f):
  ; - writes to even addresses go to memory control register
  ; - writes to odd addresses go to I/O control register
  ; - read return the last byte of instruction which read the port

  ;For $40-7f (traditionally $7e & $7f):
  ; - writes to any address goes to SN76489 PSG
  ; - reads from even addresses return V counter
  ; - reads from odd addresses return H Counter

  ;For $80-$bf (traditionally $be & $bf):
  ; - writes to even addresses go to VDP data port
  ; - writes to odd addresses go to VDP control port
  ; - reads from even addresses return VDP data port content
  ; - reads from odd addresses return VDP status flags

  ;For $90-$ff (traditionally $dc & $dd ...?):
  ; - writes have no effect
  ; - reads from even addresses return A/B register of I/O port 
  ; - reads from odd addresses return B/Misc register of I/O port

InitSystem:                 ;Self explanatory                         $0100

  ;Disable rendering. Some BIOS leave rendering enabled, which interferes with copying things to VRAM. -_-'
  ld a,%10100000
  out ($bf),a
  ld a,$81
  out ($bf),a

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
  xor a
  ld bc,$0000
  ld de,$0000
  ld hl,$0000

  ;Clear RAM
  ld a,$00                  ;Load A with the value ($00)
  ld bc,$2000               ;Load BC with amount of writes ($2000=-8KB)
  ld ix,$c000               ;Load IX with starting address

loopClearRam:
  ld (ix),a                 ;Write data to address
  inc ix                    ;Increment 16-bit address
  dec c                     ;Decrement counter, lo-byte
  jp nz,loopClearRam
  dec b                     ;Decrement counter, hi-byte
  jp nz,loopClearRam

  ld sp,$dff0               ;Set stack pointer at $DFF0

  ;Copy initial sprite data
  ld hl,spriteInit
  ld de,_P1_SPRITE
  ld bc,$0028
  ldir

  ld a,(_P1_SPRITE)
  ld (_P1_YX+1),a
  ld a,(_P1_SPRITE+1)
  ld (_P1_YX+3),a

  ld a,(_P2_SPRITE)
  ld (_P2_YX+1),a
  ld a,(_P2_SPRITE+1)
  ld (_P2_YX+3),a

  ;Setup palette Y scroll pointer
  ld hl,bgPalette
  ld (_PALETTE_Y_SCROLL),hl

  ;Initialize player data, and enemy shield in RAM
  ld hl,initData
  ld de,_P1_SCORE
  ld bc,$0013
  ldir

  ;Copy UI elements, like icons, to RAM
  ld de,_UI_P1_LIFE 
  ld bc,$0040
  ldir

  ;Copy the UI & sprite palette to ram
  ld hl,paletteUI
  ld de,_PALETTE_FIXED
  ld bc,$0020
  ldir

  ;Copy IRQ handler to RAM
  ld hl,irq
  ld de,_IRQ_RAM
  ld bc,irqEnd-irq
  ldir

  ;Initialize OAM stuff for palette cycling
  ld hl,oamInit
  ld de,_OAM_Y_SRC_0
  ld bc,$0010
  ldir

  ;Initialize tunnel Y speed
  ld hl,$0020
  ld (_TUNNEL_Y_SPEED),hl

  ld a,1
  ld (_TUNNEL_X_SPEED),a

;--FANCY Z80 NOTES--
;   -BLOCK COPY-

; H/L source address, D/E destination address, B/C byte counter
; - LDIR - LoaD Increment Repeat (from start)
; - LDDR - LoaD Decrement Repeat (from end)

; H/L source address, B/C byte counter, A byte to be compared
; - CPIR - ComPare (A) Increment Repeat (from start)
; - CPDR - ComPare (A) Decrement Repeat (from end)

; H/L source/destination address, B byte counter, C port
; - INIR - IN Increment Repeat (from start)
; - INDR - IN Decrement Repeat (from end)
; - OTIR - OuTput Increment Repeat (from start)
; - OTDR - OuTput Decrement Repeat (from end)

;Clear VRAM
  ;SMS VRAM Map:
  ; - $0000-$1fff - BG tiles
  ; - $2000-$2fff - Sprite tiles
  ; - $3800-$3eff - Tile map
  ; - $3f00-$3fff - Sprite attributes
  ; - $c000-$c020 - Color

  ;With this project, the tiles end up overlapping. There are too many BG tiles to fit into 

  ld a,$00
  out ($bf),a               ;OUTput (write) the contentent of A to port $bf, VDP write port
  ld a,$40
  out ($bf),a               ;Point VDP write port to $4000 in VRAM

  ld bc,$4000               ;16KB of VRAM
  ld a,$00

loopClearVRAM:
  out ($be),a               ;Write the contentnts of A ($00) to $be, which auto increments
  dec c
  jp nz,loopClearVRAM       ;8-bit loop for C
  dec b
  jp nz,loopClearVRAM       ;16-Bit loop for B


  ;Copy tiles to VRAM
  ;After $4000 writes to VRAM, the write address is a mirror of $00

  ld hl,bgTile
  ld bc,$40be               ;$2900 bytes of tiles need to be copied
  otir                      ;Copy $40 bytes of tile

  ld a,$2a
.loop:
  otir
  dec a
  cp 0
  jp nz,.loop


  ;Copy tilemap to VRAM 
  ;Technically, the tilemap should exist at $3800-$37FF ($0800 bytes), however since the screen doesn't actually scroll, only the visible area $3800-$3DFF ($0600 bytes) need to be copied.
  
  xor a
  out ($bf),a
  ld a,$78
  out ($bf),a

  ld hl,bgTileMap
  ld bc,$00be
  otir                      ;Copy $(1)00 bytes of data to port $BE
  otir
  otir
  otir
  otir
  otir                      ;Copy $600 bytes of data

  ;Configure VDP
  ld hl,vdpInit
  ld b,$16
  ld c,$bf
  otir

  ld ix,hBlank16

  im 1                      ;Interrupt mode 1, jumps to $0038 when an interrupt is called
  ei                        ;Enable interrupts

;Main loop with counter
loop:
  ;Set flag to make sure main loop only runs once per frame
  ld a,1
  ld (_STATUS_FLAG),a

  ;Input handling
  ;Player ships can be moved around with the d-pad
  ;To do:
  ; - Fire, or at least initiate firing animation with button 1/Start
  ; - Cycle through ship speeds with button 2

  ;Process P1 movement
  xor a
  ld b,a
  ld a,(_P1_VELOCITY)
  and %00001100             ;Clean up P1 velocity, just in case
  ld (_P1_VELOCITY),a
  ld c,a
  ld hl,movementVector
  add hl,bc
  ld de,_PLAYER_VECTOR_TEMP
  ld bc,$0004
  ldir

  ;Prep P1 Y & X for processing
  ld hl,_P1_YX
  ld de,_PLAYER_YX_TEMP
  ld bc,$0004
  ldir

  ;Process input for player 1
  ld bc,(_P1_INPUT_D)
  ld hl,playerMoveJumpTable
  add hl,bc
  ld (playerInputHandler+1),hl
  call playerInputHandler

  ;Set P1 Y & X
  ld hl,_PLAYER_YX_TEMP
  ld de,_P1_YX
  ld bc,$0004
  ldir

  ;Update sprite data for P1
  ld a,(_P1_YX+1)
  ld (_P1_SPRITE),a
  ld a,(_P1_YX+3)
  ld (_P1_SPRITE+1),a

  ;Process P2 movement
  xor a
  ld b,a
  ld a,(_P2_VELOCITY)
  and %00001100
  ld (_P2_VELOCITY),a
  ld c,a
  ld hl,movementVector
  add hl,bc
  ld de,_PLAYER_VECTOR_TEMP
  ld bc,$0004
  ldir

  ;Prep P2 Y & X for processing
  ld hl,_P2_YX
  ld de,_PLAYER_YX_TEMP
  ld bc,$0004
  ldir

  ;Process input for player 2
  ld bc,(_P2_INPUT_D)
  ld hl,playerMoveJumpTable
  add hl,bc
  ld (playerInputHandler+1),hl
  call playerInputHandler

  ;Set P2 Y & X
  ld hl,_PLAYER_YX_TEMP
  ld de,_P2_YX
  ld bc,$0004
  ldir

  ;Update sprite data for P2
  ld a,(_P2_YX+1)
  ld (_P2_SPRITE),a
  ld a,(_P2_YX+3)
  ld (_P2_SPRITE+1),a

  ;Process shields
  ;Though at this point, it's just making sure there are no *weird* values
  ld a,(_P1_SHIELD)
  and %00011100             ;P1 (and P2) shield ranges from $00-$1C, in increments of 4
  ld (_P1_SHIELD),a

  ld a,(_P2_SHIELD)
  and %00011100
  ld (_P2_SHIELD),a

  ld a,(_BOSS_SHIELD)
  and %01111000             ;Boss shield ranges from $00-$78, in increments of 8
  ld (_BOSS_SHIELD),a

  ;To do:
  ; - Collision
  ;   - Player ships fire hitscan shots?
  ; - Enemy movements
  ; - Enemy animation
  ; - Sprite manager (?), all sprites are hardcoded at the moment

  ;Process 16x8 sprites (player sprites, eyeballs)

  ;P1 & P2 sprites are a single tile tall, so their Y coordinates can be duplicated without any issue.

  ld hl,(_P1_SPRITE)
  ld h,l
  ld (_P1_OAM_Y),hl

  ld hl,(_P1_SPRITE+1)
  ld (_P1_OAM_XT),hl

  ld bc,$0108
  ld a,(_P1_SPRITE+3)
  add a,b
  ld b,a
  add hl,bc
  ld (_P1_OAM_XT+2),hl

  ;Process player 2 sprite
  ld hl,(_P2_SPRITE)
  ld h,l
  ld (_P2_OAM_Y),hl
  ld hl,(_P2_SPRITE+1)
  ld (_P2_OAM_XT),hl
  ld bc,$0108
  ld a,(_P2_SPRITE+3)
  add a,b
  ld b,a
  add hl,bc
  ld (_P2_OAM_XT+2),hl

  ;Process eyeballs
  ld hl,(_E1_SPRITE)
  ld h,l
  ld (_E_OAM_Y),hl
  ld hl,(_E1_SPRITE+1)
  ld (_E_OAM_XT),hl
  ld bc,$0108
  ld a,(_E1_SPRITE+3)
  add a,b
  ld b,a
  add hl,bc
  ld (_E_OAM_XT+2),hl


  ld hl,(_E2_SPRITE)
  ld h,l
  ld (_E_OAM_Y+2),hl
  ld hl,(_E2_SPRITE+1)
  ld (_E_OAM_XT+4),hl
  ld bc,$0108
  ld a,(_E2_SPRITE+3)
  add a,b
  ld b,a
  add hl,bc
  ld (_E_OAM_XT+6),hl

  ld hl,(_E3_SPRITE)
  ld h,l
  ld (_E_OAM_Y+4),hl
  ld hl,(_E3_SPRITE+1)
  ld (_E_OAM_XT+8),hl
  ld bc,$0108
  ld a,(_E1_SPRITE+11)
  add a,b
  ld b,a
  add hl,bc
  ld (_E_OAM_XT+10),hl

  ld hl,(_E4_SPRITE)
  ld h,l
  ld (_E_OAM_Y+6),hl
  ld hl,(_E4_SPRITE+1)
  ld (_E_OAM_XT+12),hl
  ld bc,$0108
  ld a,(_E4_SPRITE+3)
  add a,b
  ld b,a
  add hl,bc
  ld (_E_OAM_XT+14),hl

  ;Process XL enemy 1
  ld hl,(_E1XL_SPRITE)
  ld h,l
  ld (_E_OAM_Y+8),hl
  ld bc,$0808
  add hl,bc
  ld (_E_OAM_Y+10),hl

  ld a,$fb
  add a,l
  ld (_E_OAM_Y+12),a
  
  ld hl,(_E1XL_SPRITE+1)
  ld (_E_OAM_xt+16),hl
  ld bc,$0108
  ld de,$00f8               ;Adding $00F8 effectively decreases X position AND increase tile index ^_^
  add hl,bc
  ld (_E_OAM_XT+18),hl
  add hl,de
  ld (_E_OAM_XT+20),hl
  add hl,bc
  ld (_E_OAM_XT+22),hl
  
  ld h,$4f
  add hl,bc
  ld (_E_OAM_XT+24),hl


  ;Process XL enemy 2
  ld hl,(_E2XL_SPRITE)
  ld h,l
  ld (_E_OAM_Y+13),hl
  ld bc,$0808
  add hl,bc
  ld (_E_OAM_Y+15),hl
  ld a,$fb
  add a,l
  ld (_E_OAM_Y+17),a
  ld hl,(_E2XL_SPRITE+1)
  ld (_E_OAM_xt+26),hl
  ld bc,$0108
  ld de,$00f8
  add hl,bc
  ld (_E_OAM_XT+28),hl
  add hl,de
  ld (_E_OAM_XT+30),hl
  add hl,bc
  ld (_E_OAM_XT+32),hl
  ld h,$4f
  add hl,bc
  ld (_E_OAM_XT+34),hl

  ;Process XL enemy 3 
  ld hl,(_E3XL_SPRITE)
  ld h,l
  ld (_E_OAM_Y+18),hl
  ld bc,$0808
  add hl,bc
  ld (_E_OAM_Y+20),hl
  ld a,$fb
  add a,l
  ld (_E_OAM_Y+22),a
  ld hl,(_E3XL_SPRITE+1)
  ld (_E_OAM_xt+36),hl
  ld bc,$0108
  ld de,$00f8
  add hl,bc
  ld (_E_OAM_XT+38),hl
  add hl,de
  ld (_E_OAM_XT+40),hl
  add hl,bc
  ld (_E_OAM_XT+42),hl
  ld h,$4f
  add hl,bc
  ld (_E_OAM_XT+44),hl

  ;Process XL enemy 3 
  ld hl,(_E4XL_SPRITE)
  ld h,l
  ld (_E_OAM_Y+23),hl
  ld bc,$0808
  add hl,bc
  ld (_E_OAM_Y+25),hl
  ld a,$fb
  add a,l
  ld (_E_OAM_Y+27),a
  ld hl,(_E4XL_SPRITE+1)
  ld (_E_OAM_xt+46),hl
  ld bc,$0108
  ld de,$00f8
  add hl,bc
  ld (_E_OAM_XT+48),hl
  add hl,de
  ld (_E_OAM_XT+50),hl
  add hl,bc
  ld (_E_OAM_XT+52),hl
  ld h,$4f
  add hl,bc
  ld (_E_OAM_XT+54),hl

  ;A decimal counter, keeping track of how many times the main loop has run. A *has* to be used when working with decimal numbers. On the upside, this also makes for an easy way of tracking scores & lives.

  xor a
  ld bc,$c000
  ld a,(bc)
  add 1
  daa
  ld (bc),a
  inc bc 
  ld a,(bc)
  adc 0
  daa
  ld (bc),a
  inc bc 
  ld a,(bc)
  adc 0
  daa
  ld (bc),a
  inc bc
  ld a,(bc)
  adc 0
  daa
  ld (bc),a

  ;Animate exhaust flame
  ;Seems to look better at 30 FPS than at 60 FPS, oh well :)
  ld a,(_FRAME_COUNTER)
  and %00000011
  jr nz,exhaustColor
  ld a,0
  jr exhaustDone

exhaustColor:
  ld a,$3f

exhaustDone:
  ld (_PALETTE_FIXED_SP+1),a

.wait:
  ld a,(_STATUS_FLAG)
  bit 0,a
  jr nz,.wait

  jp loop


;----------------------------------------------------------------
;Interrupt handlers
;----------------------------------------------------------------
irq:                        ;The Master System VDP generates an IRQ for HBlank AND VBlank
                            ;It's up to the software to determine what happened

  phase _IRQ_RAM            ;Kind of like ORG, but for code
                            ;Basically, it sets the starting point of the code to be what's defined as _IRQ_RAM ($D800), so that labels work when this is run from $D800

  ex af,af'                 ;EXchange the contents of AF and AF'
  exx                       ;Back up the rest of the registers

  in a,($bf)                ;Acknowledge interrupt

  bit 7,a                   ;Check bit 7
  jr nz,vBlank              ;If bit 7 is set, it's a VBlank IRQ

  ;**NOTE**
  ;Looks like the JR NZ,vBlank is kinda' crucial. I tried replacing it with JP NZ,vBlank and it caused graphical issues. -_-'

.hBlankJump:
  jp hBlank16

hBlank1:
  ld a,$05
  out ($bf),a
  ld a,$c0
  out ($bf),a

  ;Labels in the IRQ handler starting with a "." are more likely than not, labels for self modifying code.
.loadAddress:
  ld hl,$0000               ;Initializing with $0000
                            ;This should be changed to a proper address by the time the CPU executes this :)
  
  ;The original code was copying all 16 colors to CRAM. My best guess is, that didn't leave the CPU with enough time to process anything else.

  ld bc,$0bbe
  otir

  ld bc,$0015               ;The block copy increments HL by $0B (11) bytes
                            ;Since each "row" of colors is $20 (32) bytes in size, the amount to increment is $20-$0B, or $15 (21).
                            ;I was incrementing the palette by $20, and the results were pretty glitchy.
  add hl,bc
  ld (hBlank1.loadAddress+1),hl

  ;Using IYL to track how many lines got drawn. Saves a few cycles by way of not having to load/store things. This keeps track of how many lines got drawn before disabling line interrupts.
  dec iyl
  jr nz,endHBlank

  ld a,$ff
  out ($bf),a
  ld a,$8a
  out ($bf),a

endHBlank:
  exx
  ex af,af'
  ei
  reti


vBlank:
  ;Update UI elements
  ;Instead of updating all eight elements, the idea is to update them one at a time. This cuts down the amount of data that has to be copied to the tile map on any given frame. Plus, UI elements refreshing once every eight frame might not be all that noticeable. :3

.updateUI:
  call uiUpdateJumpTable

  ;Update CALL code for the next frame
  ld hl,uiUpdateJumpTable
  ld bc,(_FRAME_COUNTER_X4_LOOP_8)
  add hl,bc
  ld (vBlank.updateUI+1),hl


  ;Setup sprite cycling by using the frame counter to update pointers for block copy. The one hiccup is that if the counter is $00, one of the pointers will be set to copy $00 (AKA 256) bytes. I opted to reset the counter to $20, the midway point. :)

  ld a,(_FRAME_COUNTER)
  and %00111111
  jr nz,vBlank.skipIncrement
  ld a,$20
.skipIncrement:

  ;Each of the OAM block copy instruction is done twice, following the outline below.
  ;For example, if the frame counter is at $10:
  ; - _OAM_Y_SRC_0 is set to $10
  ; - _OAM_Y_AMT_0 is set to $30 ($40-$10)
  ; - _OAM_Y_SRC_1 is fixed at $00
  ; - _OAM_Y_AMT_1 is set to $10
  
  ;On execution:
  ; - VRAM write addres is set to $3F00
  ; - The first block copy moves $30 bytes of data, from $C210 to $C240 to VRAM $3F00-$3F2F
  ; - VRAM write address is automatically set to $3F30
  ; - The second block copy moves $10 bytes of data from $C200 to $C210, to VRAM $3F30-$3F3F

  ld (_OAM_Y_SRC_0),a
  ld (_OAM_Y_AMT_1+1),a
  ld b,a
  ld a,$40
  sub a,b
  ld (_OAM_Y_AMT_0+1),a
  ld a,b 
  rlca
  ld b,a
  ld (_OAM_XT_SRC_0),a
  ld (_OAM_XT_AMT_1+1),a
  ld a,$80
  sub a,b
  ld (_OAM_XT_AMT_0+1),a

  ;Update sprite Y data
  xor a
  out ($bf),a
  ld a,$7f
  out ($bf),a
  
  ld hl,(_OAM_Y_SRC_0)
  ld bc,(_OAM_Y_AMT_0)
  otir

  ld hl,(_OAM_Y_SRC_1)
  ld bc,(_OAM_Y_AMT_1)
  otir

  ;Update sprite X data and tile index
  ld a,$80
  out ($bf),a
  ld a,$7f
  out ($bf),a

  ld hl,(_OAM_XT_SRC_0)
  ld bc,(_OAM_XT_AMT_0)
  otir

  ld hl,(_OAM_XT_SRC_1)
  ld bc,(_OAM_XT_AMT_1)
  otir

.skipOAMXT1:
  ;Copy UI and sprite color palette from RAM
  ld a,$00
  out ($bf),a
  ld a,$c0
  out ($bf),a

  ld hl,_PALETTE_FIXED
  ld bc,$20be               ;Same as LD B,$20 and LC C,$BE
  otir

  ;Modified frame counter to be a 16 bit number. I doubt any of the animation counters will go above $FF, let alone $FFFF. I might even make the frame counter an 8 bit number, eventually. :)

  xor a                     ;Clear A & flags
  ld hl,(_FRAME_COUNTER)
  ld bc,1
  add hl,bc
  ld (_FRAME_COUNTER),hl

  ld a,(_FRAME_COUNTER_X2)
  add a,2
  ld (_FRAME_COUNTER_X2),a

  ld a,(_FRAME_COUNTER_X4_LOOP_8)
  add a,4
  and %00011100
  ld (_FRAME_COUNTER_X4_LOOP_8),a

  ;The ui calculation process needs to be "one ahead" of the ui update process. Basically UI element 0 is copied to VRAM, UI element 1 is calculated in one VBlank. In the next VBlank, UI element 1 is copied to VRAM, and UI element 2 is calculated. And so on.

  ;Configure the CALL function below
  ld hl,uiCalculateJumpTable
  ld bc,(_FRAME_COUNTER_X4_LOOP_8)
  add hl,bc
  ld (vBlank.calculateUI+1),hl

.calculateUI:
  call uiCalculateJumpTable

  ;Reset line counter
  ld a,$0e
  out ($bf),a
  ld a,$8a
  out ($bf),a

  ;Reset HBlank IRQ line counter
  ld iy,$54

  ld ix,hBlank16
  ld (irq.hBlankJump+1),ix

  ;Reset the VBlank wait bit in _STATUS_FLAG
  ld a,(_STATUS_FLAG)
  and %11111110
  ld (_STATUS_FLAG),a


;----------------------------------------------------------------
;Update/prep tunnel animation
;----------------------------------------------------------------

  ;Tunnel animation is now driven by a table. There's probably a better way to implement this, but at the moment, I'm happy with the ability to control the vertical & horizontal scroll :)

  ld a,(_FRAME_COUNTER)
  and %01111111
  jr nz,vBlank.skipTunnelAnimationUpdate

  ld a,(_TUNNEL_ANIMATION)
  add 2
  and %00011110             ;Going with a maximum of 16 animation states at the moment
                            ;However, since each state is a word, the actual number is 32
  ld (_TUNNEL_ANIMATION),a
  
  ld hl,tunnelScrollY
  ld bc,(_TUNNEL_ANIMATION)
  add hl,bc
  ld de,_TUNNEL_Y_SPEED
  ld bc,$0002
  ldir
  
  ld bc,$001e
  add hl,bc
  ld de,_TUNNEL_X_SPEED
  ld c,$02
  ldir

.skipTunnelAnimationUpdate:

  ;Setup pointer to palettes to copy during HBlank. Palette X & Y scrolls need to be processed and stored separately. It's a bit more work, but provides flexibility.

  ld hl,(_PALETTE_Y_SCROLL)
  ld bc,(_TUNNEL_Y_SPEED)
  add hl,bc
  ld a,h

  ;Make sure the palette pointer stays within the $4000-$5FFF range
  and %11011111             ;Caps the max to $5FFF
  or %01000000              ;Floors the min to $4000
  ld h,a
  ld (_PALETTE_Y_SCROLL),hl

  ld a,(_PALETTE_X_SCROLL)
  ld b,a
  ld a,(_TUNNEL_X_SPEED)
  add b
  cp $0c
  jr c,noReset
  xor a
noReset:
  ld (_PALETTE_X_SCROLL),a

  ;HL still contains _PALETTE_Y_SCROLL. L needs to be adjusted to implement the X scroll.
  add a,l
  ld l,a
  ld (hBlank1.loadAddress+1),hl

;----------------------------------------------------------------
;Read user input
;----------------------------------------------------------------

  in a,($dc)                ;Raw input from $DC - %00000000
                                                  ;|:|:|:|:
                                                  ;|:|:|:|Controller 1 up
                                                  ;|:|:|:Controller 1 down
                                                  ;|:|:|Controller 1 left
                                                  ;|:|:Controller 1 right
                                                  ;|:|Controller 1 button 1
                                                  ;|:Controller 1 button 2
                                                  ;|Controller 2 up
                                                  ;Controller 2 down
  
  xor %11111111             ;Raw input data reads "0" for pressed, and "1" for not pressed
                            ;XOR-ing with %11111111 reverses the bit
  ld (_RAW_INPUT),a
  
  ld b,a                    ;Backup A to B
  rlca                      ;Left shift A twice
  rlca
  and %00111100             ;A now contains xxRLDUxx
  ld (_P1_INPUT_D),a        ;_P1_INPUT_D now works as a jump table offset

  xor a                     ;Clear flags
  ld a,b
  rlca
  rlca
  and %00000011
  ld b,a

  in a,($dd)                ;Raw input from $DD - %00000000
                                                  ;|:|:|:|:
                                                  ;|:|:|:|Controller 2 left
                                                  ;|:|:|:Controller 2 right
                                                  ;|:|:|Controller 2 button 1
                                                  ;|:|:Controller 2 button 2
                                                  ;|:|Reset*
                                                  ;|:Cartridge**
                                                  ;|Port A TH pin input
                                                  ;Port B TH pin input
  xor %11111111
  
  ;*Might not be mapped in some hardware like the SMS2
  ;** "CONT" (?), generally returns "1" on 8-bit hardware, "0" on a Genesis

  ld (_RAW_INPUT+1),a
  and %00000011
  rlca
  rlca
  or a,b
  rlca
  rlca
  ld (_P2_INPUT_D),a

  exx                       ;Restore registers
  ex af,af'
  ei
  reti

  ;*SIGH*
  ;Since CALL works with direct addresses only, I'm adding a function here to call to. This will jump to the player input handler subroutine.

playerInputHandler:
  jp playerMoveJumpTable    ;This should be modified by the main loop to point to where to go

  dephase                   ;Sets the address/label positioning back to the way they were
irqEnd:                     ;Label to calculate total size of IRQ (and other) subroutines

  ;Technically, HBlank and VBlank are both handled by the same IRQ handler. I'm splitting the HBlank handlers into two. "hBlank16" is called twice, when the HUD is being rendered. There's not a lot happening here, so it's left as a static subroutine in ROM.

  ;With "hBlank1" however, modifying bits of code as it runs seems to be the best way to get the most performance, so it's gets copied to RAM.

hBlank16:

  ;Change line counter
  ;This won't take effect until *AFTER* the next IRQ, which will also be triggered after 16 lines
  ld a,$00
  out ($bf),a
  ld a,$8a
  out ($bf),a

  ld a,($C020)
  bit 1,a
  jp nz,endHBlank

  ld ix,hBlank1
  ld (irq.hBlankJump+1),ix

  jp endHBlank

nmi:
  retn                      ;RETurn from Nmi

;----------------------------------------------------------------
;Subroutines
;----------------------------------------------------------------

processNumbers:
  ;Converts byte encoded decimal numbers (i.e., $12) into tilemap entries (i.e., $0011, $0012)
  ;Requires:
  ; - HL - data source
  ; - DE - destination address
  ; - B  - repeat count

  ld a,(hl)
  and $f0
  rra
  rra
  rra
  rra
  or $10
  ld (de),a
  inc de
  inc de
  ld a,(hl)
  inc hl
  and $0f
  or $10
  ld (de),a
  inc de
  inc de
  djnz processNumbers
  
  ret

processVelocity:
  ;Converts player velocity to  UI element
  ;Uses:
  ; - A  - Velocity
  ; - DE - Destination
  ; - BC - As index into table

  ld b,0
  ld c,a
  ld hl,uiSpeed
  add hl,bc
  ld bc,$0004
  ldir

  ret

processShield:
  ;Generates UI element for shield, based on number
  ;Uses:
  ; - HL - Tile map
  ; - A  - Index
  ; - DE - Destination
  ; - BC - Offset into table, and repeat count for LDIR

  push bc                   ;Backup the value in BC
                            ;It will be needed near the end of the subroutine
  rla

  ld b,0 
  rl b
  
  ;If boss shield is at $10, the 4th RLA will move bit 4 into the carry flag, effectively making the value in A $00. RL B is used to roll the carry bit into B.
  
  ld c,a
  add hl,bc
  pop bc
  ldir

  ret

  ;Calculate UI elements----------------------------------------
  ;The variables/registers are hardcoded, and don't require any values to be passed via registers

calculateUIBossShield:
  ld a,(_BOSS_SHIELD)
  ld hl,uiBossShield
  ld de,_UI_BOSS_SHIELD
  ld bc,$0010
  call processShield
  ret

calculateUIHighScore:
  ld hl,_HIGH_SCORE
  ld de,_UI_HIGH_SCORE
  ld b,$04
  call processNumbers
  ret

calculateUIP1Stat:
  ld a,(_P1_VELOCITY)
  ld de,_UI_P1_VELOCITY
  call processVelocity
  
  ld a,(_P1_SHIELD)
  ld hl,uiPlayerShield
  ld de,_UI_P1_SHIELD
  ld bc,$0008
  call processShield
  ret

calculateUIP2Stat:
  ld a,(_P2_VELOCITY)
  ld de,_UI_P2_VELOCITY
  call processVelocity
  
  ld a,(_P2_SHIELD)
  ld hl,uiPlayerShield
  ld de,_UI_P2_SHIELD
  ld bc,$0008
  call processShield
  
  ret

calculateUIP1Life:
  ld hl,_P1_LIFE
  ld de,_UI_P1_LIFE+6
  ld b,$01
  call processNumbers
  ret

calculateUIP2Life:
  ld hl,_P2_LIFE
  ld de,_UI_P2_LIFE+6
  ld b,$01
  call processNumbers
  ret

calculateUIP1Score:
  ld hl,_P1_SCORE
  ld de,_UI_P1_SCORE
  ld b,$04
  call processNumbers
  ret

calculateUIP2Score:
  ld hl,_P2_SCORE
  ld de,_UI_P2_SCORE
  ld b,$04
  call processNumbers
  ret

  ;Update UI elements--------------------------------------------

updateUIBossShield:
  ld a,$dc
  out ($bf),a
  ld a,$78
  out ($bf),a
  ld hl,_UI_BOSS_SHIELD
  ld bc,$10be
  otir
  ret

updateUIHighScore:
  ld a,$1c
  out ($bf),a
  ld a,$78
  out ($bf),a
  ld hl,_UI_HIGH_SCORE
  ld bc,$10be
  otir
  ret

updateUIP1Stat:
  ld a,$84
  out ($bf),a
  ld a,$78
  out ($bf),a
  ld hl,_UI_P1_STAT
  ld bc,$10be
  otir
  ret

updateUIP2Stat:
  ld a,$ae
  out ($bf),a
  ld a,$78
  out ($bf),a
  ld hl,_UI_P2_STAT
  ld bc,$10be
  otir
  ret

updateUIP1Life:
  ld a,$44
  out ($bf),a
  ld a,$78
  out ($bf),a
  ld hl,_UI_P1_LIFE
  ld bc,$10be
  otir
  ret

updateUIP2Life:
  ld a,$6e
  out ($bf),a
  ld a,$78
  out ($bf),a
  ld hl,_UI_P2_LIFE
  ld bc,$10be
  otir
  ret

updateUIP1Score:
  ld a,$c4
  out ($bf),a
  ld a,$78
  out ($bf),a
  ld hl,_UI_P1_SCORE
  ld bc,$10be
  otir
  ret

updateUIP2Score:
  ld a,$ee
  out ($bf),a
  ld a,$78
  out ($bf),a
  ld hl,_UI_P2_SCORE
  ld bc,$10be
  otir
  ret

  ;Move player ships---------------------------------------------

moveShipUp:
  ;Check against Y lower limit $20
  ld hl,(_PLAYER_YX_TEMP)
  ld bc,(_PLAYER_VECTOR_TEMP)
  xor a
  sbc hl,bc
  ld a,h
  cp $20
  jr nc,moveShipUp.skipReset ;NC == A >= $20
  ld hl,$2000
.skipReset:
  ld (_PLAYER_YX_TEMP),hl
  ret
moveShipDown:
  ;Check against Y upper limit $B7
  ld hl,(_PLAYER_YX_TEMP)
  ld bc,(_PLAYER_VECTOR_TEMP)
  add hl,bc
  ld a,h
  cp $b7
  jr c,moveShipDown.skipReset ;C == A < $B7
  ld hl,$b700
.skipReset:
  ld (_PLAYER_YX_TEMP),hl
  ret
moveShipLeft:
  ;Check against X lower limit $08
  ld hl,(_PLAYER_YX_TEMP+2)
  ld bc,(_PLAYER_VECTOR_TEMP)
  xor a
  sbc hl,bc
  ld a,h
  cp $08
  jr nc,moveShipLeft.skipReset ;NC == A >= $08
  ld hl,$0800
.skipReset:
  ld (_PLAYER_YX_TEMP+2),hl
  ret
moveShipLeftUp:
  ;Check against Y lower limit $20
  ld hl,(_PLAYER_YX_TEMP)
  ld bc,(_PLAYER_VECTOR_TEMP+2)
  xor a
  sbc hl,bc
  ld a,h
  cp $20
  jr nc,moveShipLeftUp.skipResetY ;NC == A >= $20
  ld hl,$2000
.skipResetY:
  ld (_PLAYER_YX_TEMP),hl
  ;Check against X lower limit $08
  ld hl,(_PLAYER_YX_TEMP+2)
  ld bc,(_PLAYER_VECTOR_TEMP+2)
  xor a
  sbc hl,bc
  ld a,h
  cp $08
  jr nc,moveShipLeftUp.skipResetX ;NC == A >= $08
  ld hl,$0800
.skipResetX:
  ld (_PLAYER_YX_TEMP+2),hl
  ret
moveShipLeftDown:
  ;Check against Y upper limit $B7
  ld hl,(_PLAYER_YX_TEMP)
  ld bc,(_PLAYER_VECTOR_TEMP+2)
  add hl,bc
  ld a,h
  cp $b7
  jr c,moveShipLeftDown.skipResetY ;C == A < $B7
  ld hl,$b700
.skipResetY:
  ld (_PLAYER_YX_TEMP),hl
  ;Check against X lower limit $08
  ld hl,(_PLAYER_YX_TEMP+2)
  ld bc,(_PLAYER_VECTOR_TEMP+2)
  xor a
  sbc hl,bc
  ld a,h
  cp $08
  jr nc,moveShipLeftDown.skipResetX ;NC == A >= $08
  ld hl,$0800
.skipResetX:
  ld (_PLAYER_YX_TEMP+2),hl
  ret
moveShipRight:
  ;Check against X upper limit $F0
  ld hl,(_PLAYER_YX_TEMP+2)
  ld bc,(_PLAYER_VECTOR_TEMP)
  add hl,bc
  ld a,h
  cp $f0
  jr c,moveShipRight.skipReset ;C == A < $F0
  ld hl,$f000
.skipReset:
  ld (_PLAYER_YX_TEMP+2),hl
  ret
moveShipRightUp:
  ;Check against Y lower limit $20
  ld hl,(_PLAYER_YX_TEMP)
  ld bc,(_PLAYER_VECTOR_TEMP+2)
  xor a
  sbc hl,bc
  ld a,h
  cp $20
  jr nc,moveShipRightUp.skipResetY ;NC == A >= $20
  ld hl,$2000
.skipResetY:
  ld (_PLAYER_YX_TEMP),hl
  ;Check against X upper limit $F0
  ld hl,(_PLAYER_YX_TEMP+2)
  ld bc,(_PLAYER_VECTOR_TEMP+2)
  add hl,bc
  ld a,h
  cp $f0
  jr c,moveShipRightUp.skipResetX ;C == A < $F0
  ld hl,$f000
.skipResetX:
  ld (_PLAYER_YX_TEMP+2),hl
  ret
moveShipRightDown:
  ;Check against Y upper limit $B7
  ld hl,(_PLAYER_YX_TEMP)
  ld bc,(_PLAYER_VECTOR_TEMP+2)
  add hl,bc
  ld a,h
  cp $b7
  jr c,moveShipRightDown.skipResetY ;C == A < $B7
  ld hl,$b700
.skipResetY:
  ld (_PLAYER_YX_TEMP),hl
  ;Check against X upper limit $F0
  ld hl,(_PLAYER_YX_TEMP+2)
  ld bc,(_PLAYER_VECTOR_TEMP+2)
  add hl,bc
  ld a,h
  cp $f0
  jr c,moveShipRightDown.skipResetX ;C == A < $F0
  ld hl,$f000
.skipResetX:
  ld (_PLAYER_YX_TEMP+2),hl
  ret



;----------------------------------------------------------------
;Data block
;----------------------------------------------------------------

vdpInit:                    ;Size $16
  ;According to the Mark III software reference manual, certain bits and registers should always be set to 1 and $FF, respectively.
  ;Reference: https://segaretro.org/File:SoftwareReferenceManualForSegaMarkIIIEU.pdf

  db %00110100,$80          ;Register 0, VDP mode register 1
     ;|:|:|:|:
     ;|:|:|:|Enable external video sync (not used)
     ;|:|:|:M3 - Must be set for M1/M2 to change screen height in mode 4, should always be set to 1
     ;|:|:|M4 - Use TMSS9918 Mode|Mode 4 (SMS), should always be set to 1
     ;|:|:ShifEnable external video sync (not used)t sprites left by 8 pixels
     ;|:|Line interrupt enable
     ;|:Mask column 0 with overscan color from register 7
     ;|Disable horizontal scrolling for rows 0-1
     ;Disable horizontal scrolling for columns 24-31

  db %11111111,$82          ;Reg 2, tile map address ($3800), tile map mirrorring disabled
     ;|:|:|:|:
     ;|:|:|:|Address bit 10 for TMS9918, tile map mirroring for SMS, should always be set to 1
     ;|:|:Address bits 13-11
     ;xxxx

     ;Possible addresses are $0000,$0800,$1000,$1800, $2000,$2800,$3000,$3800

  db $ff,$83                ;Reg 3, tile map base address for TMS9918a, should be $FF
  db $ff,$84                ;Reg 4, color table base address for TMS9918a, should be $FF

  db %11111111,$85          ;Reg 5, sprite attribute table base address ($3F00)
     ;|:|:|:|:
     ;|:|:|:|Mask bit, and should always be set to 1
     ;|Address bits 13-8
     ;x

  db %11111111,$86          ;Reg 6, sprite tile base address
     ;|:|:|:|:
     ;|:|:|:Mask bits, and should always be set to 11
     ;|:|:|Address bit 13 (0: $0000, 1: $2000)
     ;xxxxx

  db $ff,$87                ;Reg 7, overscan/border color, first nybble should always be $F
  db $00,$88                ;Reg 8, background X scroll

  db $00,$89                ;Reg 9, background Y scroll, wraps after $e0

  db $ff,$8a                ;Reg a, line counter ($00 amounts to generating an IRQ every single line)
  
  ;Enable VDP *AFTER* configuring everything
  db %11100000,$81          ;Register 1, VDP mode register 2
     ;|:|:|:|:
     ;|:|:|:|2x sprite magnification (buggy)
     ;|:|:|:Sprites size (8x8|8x16)
     ;|:|:|x
     ;|:|:M2 - Selects 240 line screen if M2 is set to 1, valid for SMS2 only
     ;|:|M1 - Selects 224 line screen if M2 is set to 1, valid for SMS2 only
     ;|:Frame Interrupt disable||enable
     ;|Disable||Enable display
     ;Should always be set to 1

  ;Static palette for UI
paletteUI:
  db $00,$3f,$2a,$17,$13,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
;paletteSprite:
  db $00,$3f,$24,$2a,$3f,$3d,$02,$14,$18,$1e,$1f,$05,$15,$01,$00,$00

  ;Controls the rotation & movement speed of the tunnel
tunnelScrollY:
  dw $0020,$0020,$0020,$0020 ,$0020,$0020,$0020,$0020,$0000 ,-$0020,-$0020,-$0020 ,-$0020,-$0020,-$0020,$0000
;tunnelScrollX:
  dw $0001,$0001,$0002,$0001 ,$0001,$0001,$0001,$0001,$0001 ,$0001,$0001,$0002,$0002, $0002,$0002,$0001,$0001


initData:
  ;Init data $13 bytes
  db $00,$00,$00,$00        ;P1 score
  db $00,$00,$00,$00        ;P2 score
  db $53,$45,$47,$41        ;High score
  db $04,$04                ;P1 & P2 life

  ;Working on this project, it became clear that things like player velocity and shields needed to be bit shifted in order ot convert their values into UI elements. So I opted to just use the bit shifted values to start with.

  db $04,$04                ;P1 & P2 velocity - Increments in units of 4
  db $1c,$1c                ;P1 & P2 shield - Increments in units of 4
  db $00                    ;Boss shield - Increments in units of 8

  ;Formatting for player stats
  dw $093e,$093f,$0007,$0000,$0000,$0000,$0000,$0000 ;P1 life
  dw $0942,$0943,$0007,$0000,$0000,$0000,$0000,$0000 ;P2 life
  dw $0008,$0000,$0000,$0009,$0000,$0000,$0000,$0000 ;P1 stats
  dw $0008,$0000,$0000,$0009,$0000,$0000,$0000,$0000 ;P2 stats

 ;Movement vectors cardinal (up, down, left right), diagonal
movementVector:
  dw $0100,$00b5            ;Velocity 0
  dw $0180,$010f            ;Velocity 1
  dw $0200,$016a            ;Velocity 2
  dw $0280,$01c4            ;Velocity 3

  ;OAM stuff for sprite cycling
oamInit:
  dw _OAM_Y,$20be,_OAM_Y,$20be,_OAM_XT,$40be,_OAM_XT,$40be
;----------------------------------------------------------------
;Tilemap data for player UI
;----------------------------------------------------------------

uiSpeed:
  dw $000b,$000c            ;Lowest speed
  dw $000a,$000c
  dw $000a,$000b
  dw $000a,$000a            ;Highest speed

uiPlayerShield:
  dw $000f,$000f,$000f,$001b
  dw $000e,$000f,$000f,$001b
  dw $000d,$000f,$000f,$001b
  dw $000d,$000e,$000f,$001b
  dw $000d,$000d,$000f,$001b
  dw $000d,$000d,$000e,$001b
  dw $000d,$000d,$000d,$001b
  dw $000d,$000d,$000d,$001a

uiBossShield:
  dw $000f,$000f,$000f,$000f,$000f,$000f,$000f,$001b
  dw $000e,$000f,$000f,$000f,$000f,$000f,$000f,$001b
  dw $000d,$000f,$000f,$000f,$000f,$000f,$000f,$001b
  dw $000d,$000e,$000f,$000f,$000f,$000f,$000f,$001b
  dw $000d,$000d,$000f,$000f,$000f,$000f,$000f,$001b
  dw $000d,$000d,$000e,$000f,$000f,$000f,$000f,$001b
  dw $000d,$000d,$000d,$000f,$000f,$000f,$000f,$001b
  dw $000d,$000d,$000d,$000e,$000f,$000f,$000f,$001b
  dw $000d,$000d,$000d,$000d,$000f,$000f,$000f,$001b
  dw $000d,$000d,$000d,$000d,$000e,$000f,$000f,$001b
  dw $000d,$000d,$000d,$000d,$000d,$000f,$000f,$001b
  dw $000d,$000d,$000d,$000d,$000d,$000e,$000f,$001b
  dw $000d,$000d,$000d,$000d,$000d,$000d,$000f,$001b
  dw $000d,$000d,$000d,$000d,$000d,$000d,$000e,$001b
  dw $000d,$000d,$000d,$000d,$000d,$000d,$000d,$001b
  dw $000d,$000d,$000d,$000d,$000d,$000d,$000d,$001a
  dw $000d,$000d,$000d,$000d,$000d,$000d,$000d,$001a


  ;Jump tables---------------------------------------------------
  ;Padded with NOP to round things up to 4 bytes. Makes life a little easier. Instead of checking to see if the counter has exceeded an arbitrary number like 24 (8x3), the number can be simply AND-ed with 31. :)

uiCalculateJumpTable:
  jp calculateUIBossShield
  nop
  jp calculateUIHighScore
  nop
  jp calculateUIP1Stat
  nop
  jp calculateUIP2Stat
  nop
  jp calculateUIP1Life
  nop
  jp calculateUIP2Life
  nop
  jp calculateUIP1Score
  nop
  jp calculateUIP2Score
  nop
  
uiUpdateJumpTable:
  jp updateUIBossShield
  nop
  jp updateUIHighScore
  nop
  jp updateUIP1Stat
  nop
  jp updateUIP2Stat
  nop
  jp updateUIP1Life
  nop
  jp updateUIP2Life
  nop
  jp updateUIP1Score
  nop
  jp updateUIP2Score
  nop

playerMoveJumpTable:
  db $c9,$00,$00,$00        ;%0000 - Nothing is pressed
                            ;$C9 is opcode for RET
                            ;$00 is opcode for NOP
  
  jp moveShipUp             ;%0001 - Up
  nop
  jp moveShipDown           ;%0010 - Down
  nop
  db $c9,$00,$00,$00        ;%0011 - Up+Down

  jp moveShipLeft           ;%0100 - Left
  nop
  jp moveShipLeftUp         ;%0101 - Left+Up
  nop
  jp moveShipLeftDown       ;%0110 - Left+Down
  nop
  db $c9,$00,$00,$00        ;%0111 - Left+Down+Up
  
  jp moveShipRight          ;%1000 - Right
  nop
  jp moveShipRightUp        ;%1001 - Right+Up
  nop 
  jp moveShipRightDown      ;%1010 - Right+Down
  nop
  db $c9,$00,$00,$00        ;%1011 - Right+Down+Up
  db $c9,$00,$00,$00        ;%1100 - Right+Left
  db $c9,$00,$00,$00        ;%1101 - Right+Left+Up
  db $c9,$00,$00,$00        ;%1110 - Right+Left+Down
  db $c9,$00,$00,$00        ;%1111 - Right+Left+Down+Up
  

spriteInit:                 ;$22 bytes total
  db $41,$40,$3e,$00
  db $53,$53,$42,$00

  ;Animation frame ranges from $00-$1f
  db $50,$90,$4a,$00
  db $68,$90,$46,$01
  db $80,$99,$46,$01
  db $77,$75,$46,$00

  db $39,$13,$4e,$00
  db $90,$80,$4e,$01
  db $98,$53,$4e,$01
  db $78,$98,$4e,$01

enemyAnimationFrame:
  db $4d,$4d,$4d,$4d
  db $4d,$4d,$4d,$4d
  db $4e,$4e,$4e,$4e
  db $4e,$4e,$4e,$4e

;----------------------------------------------------------------
;SDSC header data
;----------------------------------------------------------------

  ;Header info, needs to be stored as zero terminated ASCII string
authorName:
  db "Saad Azim",$00
softwareName:
  db "SMS Tunnel Demo v2",$00
description:
  db "An attempt to re-create the mode 7 spinning room from Super Castlevania IV, on the SEGA Master System/Mark III",$00

;----------------------------------------------------------------
;Tile & tilemap
;----------------------------------------------------------------

bgTileMap:
  include "SMS Tunnel Demo V2 - Tilemap.asm"

bgTile:
  include "SMS Tunnel Demo V2 - Tile.asm"


;----------------------------------------------------------------
;Palette - $4000-$6000 ...?
;----------------------------------------------------------------

  ds $4000-$,$00

bgPalette:
  include "SMS Tunnel Demo V2 - Palette.asm"


;----------------------------------------------------------------
;SDSC header
;----------------------------------------------------------------

  ds $7fe0-$,$00            ;SDSC header starts $10 bytes before the SEGA header
  
  db "SDSC"                 ;"SDSC" in ASCII, implies SDSC header is present
  db $02,$00                ;Software version, in binary coded decimal (2.0)
  db $16,$06,$20,$22        ;Date of release/compilation in BCD DD MM yyYY (16 06 2023)
  dw authorName             ;Pointer to author name
  dw softwareName           ;Pointer to software name
  dw description            ;Pointer to software description

;Header can be offset at $1ff0, $3ff0, or $7ff0

  db "TMR SEGA"             ;Required for Export SMS & GG       $7FF0-$7FF7
  db $00,$00                ;Reserved space, can be $00||$20    $7FF8-$7FF9
  dw $0000                  ;Checksum for Export (US?) MS Bios  $7FFA-$7FFB

  db $00,$00,$00            ;BCD product & version code         $7FFC-7FFE
                            ;$27,$50,$10 results in product code 15027, version 0
                            ;(Bits 7-4) in byte 3 is high number of product code
                            ;(Bits 3-0) in byte 3 is version number

  db $4c                    ;Region & rom size (SMS Exp, 32KB)        $7FFF
                            ;Bits 7-4 used for region
                            ;(3:SMS Japan||4:SMS Export||5:GG Japan||6:GG Export||7:GG International)
                            ;Bits 3-0 used for cart size
                            ;($C:32KB||$e:64KB -rarely used-||$f:128KB||0:256KB||1:512KB -rarely used-)
