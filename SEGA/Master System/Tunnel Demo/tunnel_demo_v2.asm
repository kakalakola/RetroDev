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

_FRAME_COUNTER_X2:  rb 1    ;Like frame counter, but increments by 2
                            ;Mainly used for 16 bit addresses

_FRAME_COUNTER_X4_LOOP_8: rw 1
;Hopefully, this makes dealing with jump tables easier. The counter increments by 4 (for each JP $llhh followed by NOP). It's actually a LOT easier dealing with numbers that are a power of 4 than anything else. :D

_TUNNEL_ANIMATION:  rw 1
_TUNNEL_Y_SPEED:    rw 1
_TUNNEL_X_SPEED:    rw 1

  org $c028

_RAW_INPUT:         rw 1    ;Raw user input, 2 bytes
_RAW_INPUT_HOLD:    rw 1    ;See if the input's being continuously held down
_RAW_INPUT_TOGGLE:  rw 1    ;Button toggles
                            ;So, for example, pressing button 2 only changes the player's speed by one unit


_P1_INPUT_D:        rw 1    ;Processed d-pad input for player 1
                            ;Left shifted, so it can act as an index into a jump table
                            ;Needs to be a 16-bit word, so that it can be loadeded into BC with a single instruction (and then added to HL, for an address to jump to)

_P2_INPUT_D:        rw 1

_LINE_COUNTER:      equ $c040                 ;Something to keep track of, and disable HBlank IRQ with


  org $c100
_PALETTE_FIXED:     rw 0
_PALETTE_FIXED_BG:  rw 8
_PALETTE_FIXED_SP:  rw 8

;  org $c120
_P1_SCORE:          rw 2
_P2_SCORE:          rw 2
_HIGH_SCORE:        rw 2
_P1_LIFE:           rb 1
_P2_LIFE:           rb 1
_P1_VELOCITY:       rw 1
_P2_VELOCITY:       rw 1
_P1_SHIELD:         rw 1
_P2_SHIELD:         rw 1
_BOSS_SHIELD:       rw 1

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

;;;Player variables (low & high bytes of player Y & X)
;;_P1_YX:             rw 2
;;_P2_YX:             rw 2


;  org $c1d0
;_PLAYER_YX_TEMP:    rw 2      ;Used by input handler, need to remove this eventually
;_PLAYER_VECTOR_TEMP: rw 2
;_PLAYER_INPUT_TEMP: rb 1

  ;OAM stuff in RAM
  ;Split into their own variables because their order might not be sequential, eventually. For better sprite cycling, I hope.

  org $c200
_OAM_Y:     rb 0
_P1_OAM_Y_1: rb 1            ;Reserve 2 bytes (1 word) for player 1 Y position
_P1_OAM_Y_2: rb 1
_P2_OAM_Y_1: rb 1
_P2_OAM_Y_2: rb 1
;_E_OAM_Y:   rw 1

  org $c300
_OAM_XT:    rb 0
_P1_OAM_XT_1: rw 1
_P1_OAM_XT_2: rw 1
_P2_OAM_XT_1: rw 1
_P2_OAM_XT_2: rw 1
;
;_E_OAM_XT:  rw 2

  org $c380                 ;Stuff for sprite cycling
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
_P1_SPRITE: rw 4
_P2_SPRITE: rw 4
_P1_MOVEMENT_VECTOR: rw 2
_P2_MOVEMENT_VECTOR: rw 2
_P1_UP_DURATION: rb 1
_P1_DOWN_DURATION: rb 1
_P1_LEFT_DURATION: rb 1
_P1_RIGHT_DURATION: rb 1
_P2_UP_DURATION: rb 1
_P2_DOWN_DURATION: rb 1
_P2_LEFT_DURATION: rb 1
_P2_RIGHT_DURATION: rb 1

_PLAYER_POSITION_TMP: rw 2
_PLAYER_VECTOR_TMP: rw 2


_ENEMY_SPRITE_PAGE_1_FRAME_COUNT: rw 1

_ENEMY_SPRITE_PAGE_1_Y_SRC: rw 1
_ENEMY_SPRITE_PAGE_1_XT_SRC: rw 1

_ENEMY_SPRITE_PAGE_1_Y: equ _OAM_Y+$10        ;$C210
_ENEMY_SPRITE_PAGE_1_XT: equ _OAM_XT+$20       ;$C320


_IRQ_RAM:           equ $d800                 ;Location of IRQ handler in RAM

;;;;;Variables for sprite manager
;;;;_ENEMY_SPRITE_Y_START: equ $08                 ;Offset for start of enemy sprite Y position in OAM Y RAM
;;;;                                              ;Basically, 4 bytes are for the player sprites, and 4 bytes are reserved for special sprites, like explosion, or power ups
;;;;
;;;;_ENEMY_SPRITE_XT_START: equ $10                 ;Offset for start of eny sprite X position and tile index
                                              ;$10 bytes are reserved for playre sprites and special sprites














;Specify the stock Z80 CPU
cpu z80

  org $0000

;Z80 boot area. Unlike boot vectors on the 6502 or 68000 which contain [address] or JMP [address], the Z80 has "blocks" which contain code. $0000, $0008, $0010, $0018, $0020, $0028, $0030, and $0038 are special in that they can be accessed by the RST n.

;RST is similar to CALL, except it takes up 1 byte. CALL takes 3 bytes.

  ;RST $00 - Reset code                                               $0000
  di                        ;Disable interrupts
  jp InitSystem

  ;RST $08                                                            $0008
  ;RST $10                                                            $0010
  ;RST $18                                                            $0018
  ;RST $20                                                            $0020
  ;RST $28                                                            $0028
  ;RST $30                                                            $0030

  ds $0038-$,$00            ;Pad from "here" ($) until $0037 with $00

  ;RST $38 - Interrupt handler                                        $0038
  jp _IRQ_RAM               ;Modify IRQ handler to jump to RAM
  ds $0066-$,$00

  ;NMI handler, for the Reset Button                                  $0066
  jp nmi
  ds $0100-$,$00

InitSystem:                 ;Self explanatory                         $0100

  ;Refactor
  ; - Clear RAM
  ; - Clear VRAM
  ; - Copy stuff to RAM
  ;   - Call update functions for UI data
  ; - Copy stuff to VRAM
  ;   - Call update functions for UI elements

  ;Disable rendering. Some BIOS leave rendering enabled, which interferes with copying things to VRAM. -_-'
  ld a,%10000000            ;Bit 7 *HAS* to be set
                            ;It toggles between 4Kb and 16 Kb VRAM mode :)
  out ($bf),a
  ld a,$81
  out ($bf),a

  ;Initialization code moved to their own segment. Makes the main block of code a little easier to navigate, IMHO.

  call clearRegisters
  call clearRAM
  ld sp,$dff0               ;Set stack pointer at $DFF0
  call updateRAM



  ;SMS VRAM Map (as configured by this demo):
  ; - $0000-$1fff - BG tiles*
  ; - $2000-$2fff - Sprite tiles*
  ; - $3800-$3eff - Tile map
  ; - $3f00-$3fff - Sprite attributes
  ; - $c000-$c020 - Color

  ;*With this project, the tiles end up overlapping. There are too many BG tiles to fit into 

  ;**TO DO**
  ;Merge "clearVRAM" and updateVRAM? "updateVRAM" is overwriting most of VRAM anyway.

  call clearVRAM
  call updateVRAM            ;Copy stuff to newly cleared VRAM ...?

  ;Initialize sprite page 1 for enemy pattern 1
  ;Initialize sprite page 2 for enemy pattern 2 inverse

  ;Setup addresses in RAM
  ld hl,enemyPage01Pattern01Y
  ld (_ENEMY_SPRITE_PAGE_1_Y_SRC),hl
  ld hl,enemyPage01Pattern01XT
  ld (_ENEMY_SPRITE_PAGE_1_XT_SRC),hl

  ld hl,_ENEMY_SPRITE_PAGE_1_Y  ;Used for reference, not actually used
  ld (_ENEMY_SPRITE_PAGE_1_XT_SRC+2),hl

  ;Initialize tiles for page 1 of enemy sprites
  ld a,$68
  ld (_ENEMY_SPRITE_PAGE_1_XT+3),a
  ld (_ENEMY_SPRITE_PAGE_1_XT+5),a
  ld (_ENEMY_SPRITE_PAGE_1_XT+7),a
  ld a,$6c
  ld (_ENEMY_SPRITE_PAGE_1_XT+1),a

  ld a,$6a
  ld (_ENEMY_SPRITE_PAGE_1_XT+$13),a
  ld (_ENEMY_SPRITE_PAGE_1_XT+$15),a
  ld (_ENEMY_SPRITE_PAGE_1_XT+$17),a
  ld a,$6e
  ld (_ENEMY_SPRITE_PAGE_1_XT+$11),a


  ld ix,hBlank16

  call initVDP


  im 1                      ;Interrupt mode 1, jumps to $0038 when an interrupt is called
  ei                        ;Enable interrupts

;Main loop with counter
loop:
  ;Set flag to make sure main loop only runs once per frame
  ld a,(_STATUS_FLAG)
  set 0,a                   ;Set bit 0 of A
  ld (_STATUS_FLAG),a

  ;Input handling
  ;Player ships can be moved around with the d-pad
  ;To do:
  ; - Fire, or at least initiate firing animation with button 1/Start
  ; - Cycle through ship speeds with button 2


  ;Update page 1 of enemy sprites

  ;Test counter
  ld hl,(_ENEMY_SPRITE_PAGE_1_FRAME_COUNT)
  inc hl
  ld (_ENEMY_SPRITE_PAGE_1_FRAME_COUNT),hl

  ;Check/reset counter, and address pointers, if HL-BC==0, then the limit has been reached
  xor a
  ld bc,$0200
  sbc hl,bc
  add hl,bc
  jr nz,loop.skipReset
  ld hl,$0000
  ld (_ENEMY_SPRITE_PAGE_1_FRAME_COUNT),hl

  ld hl,enemyPage01Pattern01Y
  ld (_ENEMY_SPRITE_PAGE_1_Y_SRC),hl
  ld hl,enemyPage01Pattern01XT
  ld (_ENEMY_SPRITE_PAGE_1_XT_SRC),hl

.skipReset:
  xor a                     ;Clear flags, just in case
  ;Update sprite page 1 Y positions
  ld bc,$0007
  ld hl,(_ENEMY_SPRITE_PAGE_1_Y_SRC)
  ld a,(hl)
  ld (_ENEMY_SPRITE_PAGE_1_Y),a
  ld (_ENEMY_SPRITE_PAGE_1_Y+8),a
  ;Update _ENEMY_SPRITE_PAGE_1_Y_SRC for the next frame of animation
  inc hl
  ld (_ENEMY_SPRITE_PAGE_1_Y_SRC),hl

  add hl,bc
  inc c                     ;From this point, BC should be 8
  ld a,(hl)
  ld (_ENEMY_SPRITE_PAGE_1_Y+1),a
  ld (_ENEMY_SPRITE_PAGE_1_Y+9),a

  add hl,bc
  ld a,(hl)
  ld (_ENEMY_SPRITE_PAGE_1_Y+2),a
  ld (_ENEMY_SPRITE_PAGE_1_Y+10),a

  ;Reducing the total number of enemies from 4 to 3 gets rid of flicker
;  add hl,bc
;  ld a,(hl)
;  ld (_ENEMY_SPRITE_PAGE_1_Y+3),a
;  ld (_ENEMY_SPRITE_PAGE_1_Y+11),a


  ;Update sprite page 1 X positions
  ld bc,$000e
  ld hl,(_ENEMY_SPRITE_PAGE_1_XT_SRC)
  
  ld a,(hl)
  ld (_ENEMY_SPRITE_PAGE_1_XT),a
  inc hl
  ld a,(hl)
  ld (_ENEMY_SPRITE_PAGE_1_XT+$10),a
  
  inc hl
  ld (_ENEMY_SPRITE_PAGE_1_XT_SRC),hl

  add hl,bc
  ld c,$0f
  ld a,(hl)
  ld (_ENEMY_SPRITE_PAGE_1_XT+2),a
  inc hl
  ld a,(hl)
  ld (_ENEMY_SPRITE_PAGE_1_XT+$12),a

  add hl,bc
  ld a,(hl)
  ld (_ENEMY_SPRITE_PAGE_1_XT+4),a
  inc hl
  ld a,(hl)
  ld (_ENEMY_SPRITE_PAGE_1_XT+$14),a

;  add hl,bc
;  ld a,(hl)
;  ld (_ENEMY_SPRITE_PAGE_1_XT+6),a
;  inc hl
;  ld a,(hl)
;  ld (_ENEMY_SPRITE_PAGE_1_XT+$16),a
;  inc hl



  ;Update the explosion animation
  xor a
  ld b,a
  ld a,(_FRAME_COUNTER)
  and %00111000             ;Update animation once every 8 frames
  rra                       ;A needs to be divided by 4 for the proper frame index
  rra
  ld c,a
  ld hl,animationBOM
  add hl,bc
  ld a,(hl)
  ;For the time being, the data for the explosion animation is copied to a hardcoded location in RAM
  ld ($c371),a
  inc hl
  ld a,(hl)
  ld ($c373),a

  ;Process shields
  ;Though at this point, it's just making sure there are no *weird* values
  ld a,(_P1_SHIELD)
  and %00111000             ;P1 (and P2) shield ranges from $00-$38, in increments of 8
  ld (_P1_SHIELD),a

  ld a,(_P2_SHIELD)
  and %00111000
  ld (_P2_SHIELD),a

  ld a,(_BOSS_SHIELD)
  and %11110000             ;Boss shield ranges from $00-$F0, in increments of $10
  ld (_BOSS_SHIELD),a

  ;Clean up unused bits
  xor a
  ld (_P1_VELOCITY+1),a
  ld (_P2_VELOCITY+1),a
  ld (_P1_SHIELD+1),a
  ld (_P2_SHIELD+1),a
  ld (_BOSS_SHIELD+1),a
  ;To do:
  ; - Collision
  ;   - Player ships fire hitscan shots?
  ; - Enemy movements
  ; - Enemy animation
  ; - Sprite manager (?), all sprites are hardcoded at the moment

  ;Check P1 inputs

  ;Check to see if button 2 has been pressed
  ld a,(_RAW_INPUT_TOGGLE)  ;Using toggle to pressing button 2 increases speed only once
  and %00100000
      ;|:|:|:|:
      ;|:|:|:|Controller 1 up
      ;|:|:|:Controller 1 down
      ;|:|:|Controller 1 left
      ;|:|:Controller 1 right
      ;|:|Controller 1 button 1
      ;|:Controller 1 button 2
      ;|Controller 2 up

  ;Rather than check to see if the bit is set, it might be more consistent (performance wise) to shift the bit, and add it directly to player velocity.
  rra
  ld b,a
  ld a,(_P1_VELOCITY)
  add b
  and %00110000
  ld (_P1_VELOCITY),a


.checkPlayer2Inputs:
  ld a,(_RAW_INPUT_TOGGLE+1)
  and %00001000
      ;|:|:|:|:
      ;|:|:|:|Controller 2 left
      ;|:|:|:Controller 2 right
      ;|:|:|Controller 2 button 1
      ;|:|:Controller 2 button 2
      ;|:|Reset*
      ;|:Cartridge**
      ;|Port A TH pin input
      ;Port B TH pin input
  rlca
  ld b,a
  ld a,(_P2_VELOCITY)
  add b
  and %00110000
  ld (_P2_VELOCITY),a

  ;Use playerInputHandler to setup tmp X & Y offsets, and add them here, afterwards
  ; - _PLAYER_VECTOR_TMP is set with the player velocity
  ; - playerInputHandler copies X & Y vectors _PLAYER_VECTOR_TMP, based on player velocity, which can then be added to the player's position

  ld hl,(_P1_VELOCITY)
  ld (_PLAYER_VECTOR_TMP),hl
  ld (_PLAYER_VECTOR_TMP+2),hl

  ld bc,(_P1_INPUT_D)
  ld hl,playerMoveJumpTable
  add hl,bc
  ld (playerInputHandler+1),hl
  call playerInputHandler

  ld hl,(_PLAYER_VECTOR_TMP)
  ld bc,(_P1_SPRITE)
  add hl,bc
  ld (_P1_SPRITE),hl
  
  ld hl,(_PLAYER_VECTOR_TMP+2)
  ld bc,(_P1_SPRITE+2)
  add hl,bc
  ld (_P1_SPRITE+2),hl
 
  ld a,(_P1_SPRITE+3)
  add $08
  ld (_P1_SPRITE+5),a
  
  ;Reset _PLAYER_VECTOR_TMP

  ld hl,(_P2_VELOCITY)
  ld (_PLAYER_VECTOR_TMP),hl
  ld (_PLAYER_VECTOR_TMP+2),hl

  ld bc,(_P2_INPUT_D)
  ld hl,playerMoveJumpTable
  add hl,bc
  ld (playerInputHandler+1),hl
  call playerInputHandler

  ld hl,(_PLAYER_VECTOR_TMP)
  ld bc,(_P2_SPRITE)
  add hl,bc
  ld (_P2_SPRITE),hl
  
  ld hl,(_PLAYER_VECTOR_TMP+2)
  ld bc,(_P2_SPRITE+2)
  add hl,bc
  ld (_P2_SPRITE+2),hl

  ld a,(_P2_SPRITE+3)
  add $08
  ld (_P2_SPRITE+5),a

  ;Update OAM data for player sprites

  ld a,(_P1_SPRITE+1)       ;Player 1 Y position, same for both sprites
  ld (_P1_OAM_Y_1),a
  ld (_P1_OAM_Y_1+8),a

  ld hl,(_P1_SPRITE+3)      ;Player 1 X position & tile, different for different sprites
  ld (_P1_OAM_XT_1),hl
  ld hl,(_P1_SPRITE+5)
  ld (_P1_OAM_XT_1+$10),hl

  ld a,(_P2_SPRITE+1)
  ld (_P2_OAM_Y_1),a
  ld (_P2_OAM_Y_1+8),a
  ld hl,(_P2_SPRITE+3)
  ld (_P2_OAM_XT_1),hl
  ld hl,(_P2_SPRITE+5)
  ld (_P2_OAM_XT_1+$10),hl

  ;Animate exhaust flame/blinking light
  ;(Seems to look better at 30 FPS than at 60 FPS)
  ld a,(_FRAME_COUNTER)
  and %00000011
  jr nz,exhaustColor
  xor a                     ;LD A,0 takes 7 cycles, XOR A takes 4
  jr exhaustDone

exhaustColor:
  ld a,$3f

exhaustDone:
  ld (_PALETTE_FIXED_SP+1),a

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

  ;xor a
  ld a,$05
  out ($bf),a
  ld a,$c0
  out ($bf),a

  ;Labels in the IRQ handler starting with a "." are more likely than not, labels for self modifying code.
.loadAddress:
  ld hl,$0000               ;Initializing with $0000
                            ;This should be changed to a proper address by the time the CPU executes this :)
  
  ;OUTI (at 16 cycles) is a little TOO fast for the VDP, resulting in colors not being updated.

  ;OTIR (at 21/16 cycles) is also *slightly* too fast, resulting in the palette being updated, but the actual on screen colors not showing.

  ;The *updated* solution seems to be an OUTI followed by JR NZ,label (27 cycles). OUTI decrements B, and the delay seems to be enough.

  ld bc,$0bbe

.loop:
  outi
  jr nz,hBlank1.loop

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
  ;Disable rendering, which should make data transfer to VDP a little faster (?)
  ld a,$80
  out ($bf),a
  ld a,$81
  out ($bf),a

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
  ; - The first OTIR copies $30 bytes, from $C210 to $C240 to VRAM $3F00-$3F2F
  ; - VRAM write address is automatically incremented to $3F30 at this point
  ; - The second OTIR copies $10 bytes of data from $C200 to $C210, to VRAM $3F30-$3F3F

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

  ;Copy UI and sprite color palette from RAM
  ld a,$00
  out ($bf),a
  ld a,$c0
  out ($bf),a
  ld hl,_PALETTE_FIXED
  ld bc,$20be
  otir

  ;Since VDP updates are finished, it's time to enable rendering
  ld a,(enableRendering)
  out ($bf),a
  ld a,$81
  out ($bf),a


  ;Update frame counters
  ;_FRAME_COUNTER is used as a general timer of sorts
  ;_FRAME_COUNTER_X2 is used to deal with 16 bit numbers
  ;_FRAME_COUNTER_X4_LOOP_8 increments in 4 byte units, and loops every 8 frames

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
  and %00011100             ;Makes the counter loop every 8 frames
  ld (_FRAME_COUNTER_X4_LOOP_8),a

  ;The ui calculation process needs to be "one ahead" of the ui update process. Basically UI element 0 is copied to VRAM, UI element 1 is calculated in one VBlank. In the next VBlank, UI element 1 is copied to VRAM, and UI element 2 is calculated. And so on.

  ;Configure which UI calculation to CALL next
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
  ld iy,$46

  ld ix,hBlank16
  ld (irq.hBlankJump+1),ix


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

  ld hl,(_RAW_INPUT)
  ld (_RAW_INPUT_HOLD),hl

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
  ld b,a                    ;Copy B to A for input toggle processing
  rlca
  rlca
  ld c,a
  and %00111100
  ld (_P1_INPUT_D),a

  ;XOR between A and B only sets a bit if an set bit has been set, or a set bit has been unset. Basically, it's a way to check if a button was just pressed, or released.

  ;The following AND between A and C masks out button releases. So if a button or direction has been pressed, it'll be detected for one frame only. This is useful for something like changing the player's speed.

  ld a,b 
  xor l 
  and b
  ld (_RAW_INPUT_TOGGLE),a


  xor a
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
  ld b,a

  rlca
  rlca
  and %00001100
  ld d,a
  ld a,c
  and %00000011
  or d
  rlca
  rlca
  ld (_P2_INPUT_D),a

  ld a,b
  xor h 
  and b
  ld (_RAW_INPUT_TOGGLE+1),a

  ;Having done everything, it's time to reset the VBlank wait bit in _STATUS_FLAG
  ;Reset the VBlank wait bit in _STATUS_FLAG
  ld a,(_STATUS_FLAG)
  res 0,a
  ld (_STATUS_FLAG),a


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

  ;The label is called "hBlank16" because the original plan was to call the IRQ at line 16 & line 32. However, they ended up being called every 15 lines due to graphical issues being introduced 

  ;With "hBlank1" however, modifying bits of code as it runs seems to be the best way to get the most performance, so it's gets copied to RAM.

hBlank16:

  ;Change line counter so IRQ is triggered every two lines. One scanline isn't enough time to copy all 11 colors.

  ;The update won't take effect until *AFTER* the next IRQ, which will also be triggered after 14 lines.

  ld a,$01
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

  include "routine/utility.asm"
  include "routine/ui_update.asm" 
  include "routine/input_handler.asm"

;----------------------------------------------------------------
;Data block
;----------------------------------------------------------------

  include "data/vdp_config.asm"

  ;Static palette for UI
paletteUI:
  db $00,$3f,$2a,$17,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
  db $00,$3f,$24,$2a,$3f,$3d,$02,$14,$18,$1e,$1f,$05,$15,$01,$00,$00

  ;Controls the rotation & movement speed of the tunnel
tunnelScrollY:
  ;Remove all scroll
  ;dw 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0

  dw $0020,$0020,$0020,$0020 ,$0020,$0020,$0020,$0020,$0000 ,-$0020,-$0020,-$0020 ,-$0020,-$0020,-$0020,$0000
;tunnelScrollX:
  dw $0001,$0001,$0001,$0001 ,$0001,$0001,$0001,$0001,$0001 ,$0001,$0001,$0001,$0001, $0001,$0001,$0001,$0001


initData:
  ;Init data $13 bytes
  db $00,$00,$00,$00        ;P1 score
  db $00,$00,$00,$00        ;P2 score
  db $53,$45,$47,$41        ;High score
  db $04,$04                ;P1 & P2 life

  ;Working on this project, it became clear that things like player velocity and shields needed to be bit shifted in order ot convert their values into UI elements. So I opted to just use the bit shifted values to start with.

  ;Modified to be 16 bit values. It simplifies things during processing, by basically turning most processes into ADD HL,BC :3

  dw $0010,$0010            ;P1 & P2 velocity - Increments in units of $10
  dw $0038,$0038            ;P1 & P2 shield - Increments in units of 8 
  dw $0000                  ;Boss shield - Increments in units of 8

  ;Formatting for player stats
  dw $1940,$1942,$1007,$1012,$1012,$1012,$1012,$1012 ;P1 life
  dw $1948,$194a,$1007,$1012,$1012,$1012,$1012,$1012 ;P2 life
  dw $1008,$1012,$1120,$1009,$1012,$1012,$1012,$1012 ;P1 stats
  dw $1008,$1012,$1120,$1009,$1012,$1012,$1012,$1012 ;P2 stats

  ;Movement vector (maxium?) cardinal (up, down, left right), diagonal

  ;WIP idea, movement vectors actually work as a maximum directional vector. Pressing up/down/etc actually increments the player speed by a small granular unit, like $0040 or something.

movementVectorPositive:
  dw $0040,$0080,$00c0,$0100 ;Velocity 0
  dw $002d,$005a,$0087,$00b5
  dw $0060,$00c0,$0120,$0180 ;Velocity 1
  dw $0043,$0087,$00cb,$010f
  dw $0080,$0100,$0180,$0200 ;Velocity 2
  dw $005a,$00b5,$010f,$016a
  dw $00a0,$0140,$01e0,$0280 ;Velocity 3
  dw $0071,$00e2,$0153,$01c4

movementVectorNegative:
  dw -$0040,-$0080,-$00c0,-$0100 ;Velocity 0
  dw -$002d,-$005a,-$0087,-$00b5
  dw -$0060,-$00c0,-$0120,-$0180 ;Velocity 1
  dw -$0043,-$0087,-$00cb,-$010f
  dw -$0080,-$0100,-$0180,-$0200 ;Velocity 2
  dw -$005a,-$00b5,-$010f,-$016a
  dw -$00a0,-$0140,-$01e0,-$0280 ;Velocity 3
  dw -$0071,-$00e2,-$0153,-$01c4




  ;OAM stuff for sprite cycling
oamInit:
  dw _OAM_Y,$20be,_OAM_Y,$20be,_OAM_XT,$40be,_OAM_XT,$40be
;----------------------------------------------------------------
;Tilemap data for player UI
;----------------------------------------------------------------

  include "data/ui_tilemap.asm"

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

  jp noMovement
  nop
  ;db $c9,$00,$00,$00        ;%0000 - Nothing is pressed
                            ;$C9 is opcode for RET
                            ;$00 is opcode for NOP
  
  jp moveShipUp             ;%0001 - Up
  nop
  jp moveShipDown           ;%0010 - Down
  nop
  jp noMovement             ;%0011 - Up+Down
  nop
  jp moveShipLeft           ;%0100 - Left
  nop
  jp moveShipLeftUp         ;%0101 - Left+Up
  nop
  jp moveShipLeftDown       ;%0110 - Left+Down
  nop
  jp noMovement             ;%0111 - Left+Down+Up
  nop
  jp moveShipRight          ;%1000 - Right
  nop
  jp moveShipRightUp        ;%1001 - Right+Up
  nop 
  jp moveShipRightDown      ;%1010 - Right+Down
  nop
  jp noMovement             ;%1011 - Right+Down+Up
  nop
  jp noMovement             ;%1100 - Right+Left
  nop
  jp noMovement             ;%1101 - Right+Left+Up
  nop
  jp noMovement             ;%1110 - Right+Left+Down
  nop
  jp noMovement             ;%1111 - Right+Left+Down+Up
  nop

playerSpriteInit:
  ;Y low, Y high, X low, X high, tile 0, X high 2, tile 1, something
  db $00,    $41,   $00,    $40,    $40,      $48,    $42,       $00
  db $00,    $53,   $00,    $40,    $48,      $48,    $4a,       $00

spriteInitY:
  ;Reserved for player and special(?) sprites
  ;$FF puts sprites at the top of the screen $C0 seems like a safe place to put sprits not being used
  db $c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0, $c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0
  ;Used by enemy sprites
  ;  Page 1                           Page 2
  db $c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0, $c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0
  ;  Page 3                           Page 4
  db $c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0, $c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0
  ;  Page 5                           Page 6
  db $c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0, $68,$68,$c0,$c0,$80,$80,$c0,$c0

spriteInitXT:
  ;Player/special (?) sprites
  db $ff,$be,$ff,$be,$ff,$be,$ff,$be, $ff,$be,$ff,$be,$ff,$be,$ff,$be ;Sprite XT 1
  db $ff,$be,$ff,$be,$ff,$be,$ff,$be, $ff,$be,$ff,$be,$ff,$be,$ff,$be ;Sprite XT 2
  
  ;Enemy sprites 
  ;  Page 1                           Page 2
  db $ff,$be,$ff,$be,$ff,$be,$ff,$be, $ff,$be,$ff,$be,$ff,$be,$ff,$be
  db $ff,$be,$ff,$be,$ff,$be,$ff,$be, $ff,$be,$ff,$be,$ff,$be,$ff,$be
  ;  Page 3                           Page 4
  db $ff,$be,$ff,$be,$ff,$be,$ff,$be, $ff,$be,$ff,$be,$ff,$be,$ff,$be
  db $ff,$be,$ff,$be,$ff,$be,$ff,$be, $ff,$be,$ff,$be,$ff,$be,$ff,$be
  ;  Page 5                           Page 6
  db $ff,$be,$ff,$be,$ff,$be,$ff,$be, $ff,$be,$ff,$be,$ff,$be,$ff,$be
  db $40,$50,$48,$52,$ff,$be,$ff,$be, $40,$64,$48,$66,$ff,$be,$ff,$be


  ;Explosion animation test
animationBOM:
  db $50,$52
  db $54,$56
  db $54,$56
  db $58,$5a
  db $5c,$5e
  db $60,$62
  db $be,$be
  db $be,$be



;----------------------------------------------------------------
;SDSC header data
;----------------------------------------------------------------
  include "data/sdsc_header.asm"

;----------------------------------------------------------------
;Tiles
;----------------------------------------------------------------

bgTile:
  include "graphics/tile.asm"

;----------------------------------------------------------------
;Palette - $4000-$6000 ...?
;----------------------------------------------------------------

  ds $4000-$,$00

bgPalette:
  include "graphics/palette.asm"

bgTileMap:
  include "graphics/tilemap.asm"

;----------------------------------------------------------------
;Sprite movement patterns
;----------------------------------------------------------------

  include "data/enemy_pattern_page_1.asm"

;----------------------------------------------------------------
;ROM (& SDSC) headers
;----------------------------------------------------------------

  include "data/rom_header.asm"