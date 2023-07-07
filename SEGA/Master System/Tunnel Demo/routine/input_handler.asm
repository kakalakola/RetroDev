;================================================================
;Rotating Tunnel Demo v2 - 2023.06.12
;For the SEGA Master System
;by Saad Azim
;
; Released under GNU General Public License v3.0
;================================================================

;Input handlers for both players. Sets 16 bit values in _PLAYER_VECTOR_TMP and _PLAYER_VECTOR_TMP+2 that need to be added to player Y & X, respectively.

noMovement:
  ld hl,$0000
  ld (_PLAYER_VECTOR_TMP),hl
  ld (_PLAYER_VECTOR_TMP+2),hl
  ret
moveShipUp:
  ld hl,movementVectorNegative
  ld bc,(_PLAYER_VECTOR_TMP)
  ld de,$0006
  add hl,bc
  add hl,de
  ld de,_PLAYER_VECTOR_TMP
  ldi
  ldi
  ld hl,$0000
  ld (_PLAYER_VECTOR_TMP+2),hl
  ;Check against Y lower limit $1F
  ret
moveShipDown:
  ld hl,movementVectorPositive
  ld bc,(_PLAYER_VECTOR_TMP)
  ld de,$0006
  add hl,bc
  add hl,de
  ld de,_PLAYER_VECTOR_TMP
  ldi
  ldi
  ld hl,$0000
  ld (_PLAYER_VECTOR_TMP+2),hl
  ;Check against Y upper limit $B7
  ret
moveShipLeft:
  ld hl,$0006
  ld (_PLAYER_VECTOR_TMP),hl
  ld hl,movementVectorNegative
  ld bc,(_PLAYER_VECTOR_TMP+2)
  ld de,$000e
  add hl,bc
  add hl,de
  ld de,_PLAYER_VECTOR_TMP+2
  ldi
  ldi
  ;Check against X lower limit $08
  ret
moveShipLeftUp:
  ld hl,movementVectorNegative
  ld bc,(_PLAYER_VECTOR_TMP)
  ld de,$000e
  add hl,bc
  add hl,de
  ld de,_PLAYER_VECTOR_TMP
  ldi
  ldi
  ld hl,movementVectorNegative
  ld bc,(_PLAYER_VECTOR_TMP+2)
  ld de,$000e
  add hl,bc
  add hl,de
  ld de,_PLAYER_VECTOR_TMP+2
  ldi
  ldi
  ;Check against Y lower limit $1F
  ret
moveShipLeftDown:
  ld hl,movementVectorPositive
  ld bc,(_PLAYER_VECTOR_TMP)
  ld de,$000e
  add hl,bc
  add hl,de
  ld de,_PLAYER_VECTOR_TMP
  ldi
  ldi
  ld hl,movementVectorNegative
  ld bc,(_PLAYER_VECTOR_TMP+2)
  ld de,$000e
  add hl,bc
  add hl,de
  ld de,_PLAYER_VECTOR_TMP+2
  ldi
  ldi
  ;Check against Y upper limit $B7
  ;Check against X lower limit $08
  ret
moveShipRight:
  ld hl,$0000
  ld (_PLAYER_VECTOR_TMP),hl
  ld hl,movementVectorPositive
  ld bc,(_PLAYER_VECTOR_TMP+2)
  ld de,$0006
  add hl,bc
  add hl,de
  ld de,_PLAYER_VECTOR_TMP+2
  ldi
  ldi
  ;Check against X upper limit $F0
  ret
moveShipRightUp:
  ld hl,movementVectorNegative
  ld bc,(_PLAYER_VECTOR_TMP)
  ld de,$000e
  add hl,bc
  add hl,de
  ld de,_PLAYER_VECTOR_TMP
  ldi
  ldi
  ld hl,movementVectorPositive
  ld bc,(_PLAYER_VECTOR_TMP+2)
  ld de,$000e
  add hl,bc
  add hl,de
  ld de,_PLAYER_VECTOR_TMP+2
  ldi
  ldi
  ;Check against Y lower limit $1F
  ;Check against X upper limit $F0
  ret
moveShipRightDown:
  ld hl,movementVectorPositive
  ld bc,(_PLAYER_VECTOR_TMP)
  ld de,$000e
  add hl,bc
  add hl,de
  ld de,_PLAYER_VECTOR_TMP
  ldi
  ldi
  ld hl,movementVectorPositive
  ld bc,(_PLAYER_VECTOR_TMP+2)
  ld de,$000e
  add hl,bc
  add hl,de
  ld de,_PLAYER_VECTOR_TMP+2
  ldi
  ldi
  ;Check against Y upper limit $B7
  ;Check against X upper limit $F0
  ret
