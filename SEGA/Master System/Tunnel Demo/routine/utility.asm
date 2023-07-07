;================================================================
;Rotating Tunnel Demo v2 - 2023.06.12
;For the SEGA Master System
;by Saad Azim
;
; Released under GNU General Public License v3.0
;================================================================

;----------------------------------------------------------------
;Utility functions
;----------------------------------------------------------------

  ;Stuff from boot sequence, moved here mainly for clear code

clearRegisters:
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

  ret


clearRAM:
  pop hl                    ;Copy the return address to HL
                            ;Clear RAM loop will clear everything in RAM, including the return address for RET

  xor a                     ;Clear A
  ld bc,$2000               ;Load BC with amount of writes ($2000==8KB)
  ld ix,$c000               ;Load IX with starting address
.loop:
  ld (ix),a                 ;Write data to address
  inc ix                    ;Increment 16-bit address
  dec c                     ;Decrement counter, lo-byte
  jp nz,clearRAM.loop
  djnz clearRAM.loop        ;Decrement counter, hi-byte, loop if not equal to zero
                            ;DJNZ combines DEC B > JP NZ,llhh into a single instruction

  push hl                   ;Push the return address back to stack
  ret


clearVRAM:
  ld a,$00
  out ($bf),a               ;OUTput (write) the contentent of A to port $bf, VDP write port
  ld a,$40
  out ($bf),a               ;Point VDP write port to $4000 in VRAM

  ld bc,$4000               ;16KB of VRAM
  ld a,$00

.loop:
  out ($be),a               ;Write the contentnts of A ($00) to $be, which auto increments
  dec c
  jp nz,clearVRAM.loop      ;8-bit loop for C
  dec b
  jp nz,clearVRAM.loop      ;16-Bit loop for B

  ret



updateRAM:
  ;Data for player sprite needs to be stored in its own separate are in RAM because it's going to be updated by the player, which THEN needs to be used to modify the sprite data. By comparison, enemy sprite behavior is hardcoded. :)

  ;Copy initial player sprite data
  ld hl,playerSpriteInit
  ld de,_P1_SPRITE
  ld bc,$0010
  ldir

  ;Initialize OAM RAM
  ld hl,spriteInitY
  ld de,_OAM_Y
  ld bc,$40
  ldir

  ld de,_OAM_XT
  ld bc,$80
  ldir

  ;Setup palette Y scroll pointer
  ld hl,bgPalette
  ld (_PALETTE_Y_SCROLL),hl

  ;Initialize player data, and enemy shield in RAM
  ld hl,initData
  ld de,_P1_SCORE
  ld bc,$0018
  ldir

  ;Copy UI elements, like icons, to RAM
  ld de,_UI_P1_LIFE 
  ld bc,$0040
  ldir
  
  ;Setup tile priority for player score in the UI
  ld hl,_UI_P1_SCORE
  ld de,_UI_P1_SCORE+2
  ld bc,$1000
  ld (_UI_P1_SCORE),bc
  ld bc,$2e
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

  ret


updateVRAM:
  ;Copy tiles to VRAM
  ;After $4000 writes to VRAM, the write address is a mirror of $00

  ld hl,bgTile
  ld bc,$00be               ;$3000 bytes of tiles need to be copied
  otir                      ;Copy the first $0100 bytes of tile

  ld a,$2f
.loop:
  otir
  dec a
  cp 0
  jp nz,updateVRAM.loop

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

  ret

initVDP:
  ld hl,vdpConfig
  ld b,$16
  ld c,$bf
  otir

  ret



  ;General utilities


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
  or $20
  ld (de),a
  inc de
  inc de
  ld a,(hl)
  inc hl
  and $0f
  or $20
  ld (de),a
  inc de
  inc de
  djnz processNumbers
  
  ret
