;================================================================
;Rotating Tunnel Demo v2 - 2023.06.12
;For the SEGA Master System
;by Saad Azim
;
; Released under GNU General Public License v3.0
;================================================================

;----------------------------------------------------------------
;Subroutines to update UI stuff
;----------------------------------------------------------------

processVelocity:
  ;Converts player velocity to  UI element
  ;Input:
  ; - BC - Velocity
  ; - DE - Destination
  ; - BC - As index into table

  ld hl,uiSpeed
  add hl,bc
  ldi
  ldi
  ldi
  ldi

  ret

processPlayerShield:
  ;Generates UI element for player shield, based on number
  ;Input:
  ; - HL - Tile map
  ; - BC - Offset into table
  ; - DE - Destination

  add hl,bc
  ldi
  ldi
  ldi
  ldi
  ldi
  ldi
  ldi
  ldi

  ret

processBossShield:
  ;Generates UI element for boss shield, based on number
  ;Input:
  ; - HL - Tile map
  ; - BC - Offset into table
  ; - DE - Destination

  add hl,bc
  ldi
  ldi
  ldi
  ldi
  ldi
  ldi
  ldi
  ldi
  ldi
  ldi
  ldi
  ldi
  ldi
  ldi
  ldi
  ldi

  ret

  ;Calculate UI elements----------------------------------------
  ;The variables/registers are hardcoded, and don't require any values to be passed via registers

calculateUIBossShield:
  ld bc,(_BOSS_SHIELD)
  ld hl,uiBossShield
  ld de,_UI_BOSS_SHIELD
  call processBossShield
  ret

calculateUIHighScore:
  ld hl,_HIGH_SCORE
  ld de,_UI_HIGH_SCORE
  ld b,$04
  call processNumbers
  ret

calculateUIP1Stat:
  ld bc,(_P1_VELOCITY)
  ld de,_UI_P1_VELOCITY
  call processVelocity
  
  ld bc,(_P1_SHIELD)
  ld hl,uiPlayerShield
  ld de,_UI_P1_SHIELD
  call processPlayerShield
  ret

calculateUIP2Stat:
  ld bc,(_P2_VELOCITY)
  ld de,_UI_P2_VELOCITY
  call processVelocity
  
  ld bc,(_P2_SHIELD)
  ld hl,uiPlayerShield
  ld de,_UI_P2_SHIELD
  call processPlayerShield
  
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
  ld c,$be
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  ret

updateUIHighScore:
  ld a,$1c
  out ($bf),a
  ld a,$78
  out ($bf),a
  ld hl,_UI_HIGH_SCORE
  ld c,$be
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  ret

updateUIP1Stat:
  ld a,$84
  out ($bf),a
  ld a,$78
  out ($bf),a
  ld hl,_UI_P1_STAT
  ld c,$be
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  ret

updateUIP2Stat:
  ld a,$ae
  out ($bf),a
  ld a,$78
  out ($bf),a
  ld hl,_UI_P2_STAT
  ld c,$be
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  ret

updateUIP1Life:
  ld a,$44
  out ($bf),a
  ld a,$78
  out ($bf),a
  ld hl,_UI_P1_LIFE
  ld c,$be
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  ret

updateUIP2Life:
  ld a,$6e
  out ($bf),a
  ld a,$78
  out ($bf),a
  ld hl,_UI_P2_LIFE
  ld c,$be
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  ret

updateUIP1Score:
  ld a,$c4
  out ($bf),a
  ld a,$78
  out ($bf),a
  ld hl,_UI_P1_SCORE
  ld c,$be
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  ret

updateUIP2Score:
  ld a,$ee
  out ($bf),a
  ld a,$78
  out ($bf),a
  ld hl,_UI_P2_SCORE
  ld c,$be
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  outi
  ret
