;================================================================
;Rotating Tunnel Demo v2 - 2023.06.12
;For the SEGA Master System
;by Saad Azim
;
; Released under GNU General Public License v3.0
;================================================================

  ;Enemy sprite patterns  
  ;Organized as Y, Y inverted, X1, and X2

enemyPattern01Y:
  ;16 byte padding, to account for 4 sprites, 4 frames "behind" the previous
  ;$FF puts the sprites on top of the screen. $D0 seems to put them below the visible area
  db $d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0
  
  db $be,$bc,$b9,$b6,$b4,$b1,$ae,$ab,$a7,$a4,$a1,$9d,$9a,$96,$92,$8f
  db $8b,$87,$83,$7f,$7a,$76,$72,$6e,$6a,$66,$62,$5e,$5a,$56,$53,$50
  db $4d,$4c,$4a,$49,$49,$49,$4a,$4b,$4d,$4f,$52,$55,$58,$5c,$60,$64
  db $68,$6c,$70,$74,$78,$7b,$7e,$80,$82,$84,$85,$86,$87,$87,$87,$87
  db $87,$87,$86,$86,$85,$85,$84,$83,$82,$81,$80,$7f,$7e,$7d,$7c,$7b
  db $79,$78,$77,$76,$75,$74,$73,$72,$72,$71,$71,$71,$71,$70,$71,$71
  db $71,$71,$72,$72,$73,$73,$74,$75,$76,$77,$78,$79,$7b,$7c,$7e,$7f

enemyPattern01YInverted:
  db $d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0
  db $12,$14,$17,$1a,$1c,$1f,$22,$25,$29,$2c,$2f,$33,$36,$3a,$3e,$41
  db $45,$49,$4d,$51,$56,$5a,$5e,$62,$66,$6a,$6e,$72,$76,$7a,$7d,$80
  db $83,$84,$86,$87,$87,$87,$86,$85,$83,$81,$7e,$7b,$78,$74,$70,$6c
  db $68,$64,$60,$5c,$58,$55,$52,$50,$4e,$4c,$4b,$4a,$49,$49,$49,$49
  db $49,$49,$4a,$4a,$4b,$4b,$4c,$4d,$4e,$4f,$50,$51,$52,$53,$54,$55
  db $57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5e,$5f,$5f,$5f,$5f,$60,$5f,$5f
  db $5f,$5f,$5e,$5e,$5d,$5d,$5c,$5b,$5a,$59,$58,$57,$55,$54,$52,$51

  ;Padding to ensure sprites don't show up on screen
  db $d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0
  db $d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0,$d0

enemyPattern01X1:
  db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
  db $db,$d8,$d5,$d2,$ce,$cb,$c8,$c6,$c3,$c0,$be,$bc,$b9,$b7,$b6,$b4
  db $b2,$b1,$b0,$af,$ae,$ae,$ae,$ae,$ae,$ae,$af,$b0,$b2,$b4,$b7,$ba
  db $bd,$c1,$c5,$c9,$cd,$d1,$d5,$d9,$dd,$e0,$e4,$e7,$e9,$eb,$ec,$ed
  db $ed,$ed,$ec,$eb,$e9,$e6,$e3,$e0,$dc,$d8,$d4,$d0,$cc,$c8,$c4,$c0
  db $bb,$b7,$b3,$af,$ab,$a7,$a3,$9e,$9a,$96,$92,$8e,$8a,$86,$82,$7e
  db $7a,$76,$72,$6e,$6a,$66,$62,$5e,$5a,$56,$51,$4d,$49,$45,$41,$3d
  db $38,$34,$30,$2c,$28,$24,$20,$1b,$17,$13,$0f,$0b,$07,$03,$ff,$ff
  
  ;Repeat X data for inverted enemy pattern
  db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
  db $db,$d8,$d5,$d2,$ce,$cb,$c8,$c6,$c3,$c0,$be,$bc,$b9,$b7,$b6,$b4
  db $b2,$b1,$b0,$af,$ae,$ae,$ae,$ae,$ae,$ae,$af,$b0,$b2,$b4,$b7,$ba
  db $bd,$c1,$c5,$c9,$cd,$d1,$d5,$d9,$dd,$e0,$e4,$e7,$e9,$eb,$ec,$ed
  db $ed,$ed,$ec,$eb,$e9,$e6,$e3,$e0,$dc,$d8,$d4,$d0,$cc,$c8,$c4,$c0
  db $bb,$b7,$b3,$af,$ab,$a7,$a3,$9e,$9a,$96,$92,$8e,$8a,$86,$82,$7e
  db $7a,$76,$72,$6e,$6a,$66,$62,$5e,$5a,$56,$51,$4d,$49,$45,$41,$3d
  db $38,$34,$30,$2c,$28,$24,$20,$1b,$17,$13,$0f,$0b,$07,$03,$ff,$ff

enemyPattern01X2:
  db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
  db $e3,$e0,$dd,$da,$d6,$d3,$d0,$ce,$cb,$c8,$c6,$c4,$c1,$bf,$be,$bc
  db $ba,$b9,$b8,$b7,$b6,$b6,$b6,$b6,$b6,$b6,$b7,$b8,$ba,$bc,$bf,$c2
  db $c5,$c9,$cd,$d1,$d5,$d9,$dd,$e1,$e5,$e8,$ec,$ef,$f1,$f3,$f4,$f5
  db $f5,$f5,$f4,$f3,$f1,$ee,$eb,$e8,$e4,$e0,$dc,$d8,$d4,$d0,$cc,$c8
  db $c3,$bf,$bb,$b7,$b3,$af,$ab,$a6,$a2,$9e,$9a,$96,$92,$8e,$8a,$86
  db $82,$7e,$7a,$76,$72,$6e,$6a,$66,$62,$5e,$59,$55,$51,$4d,$49,$45
  db $40,$3c,$38,$34,$30,$2c,$28,$23,$1f,$1b,$17,$13,$0f,$0b,$08,$04

  db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
  db $e3,$e0,$dd,$da,$d6,$d3,$d0,$ce,$cb,$c8,$c6,$c4,$c1,$bf,$be,$bc
  db $ba,$b9,$b8,$b7,$b6,$b6,$b6,$b6,$b6,$b6,$b7,$b8,$ba,$bc,$bf,$c2
  db $c5,$c9,$cd,$d1,$d5,$d9,$dd,$e1,$e5,$e8,$ec,$ef,$f1,$f3,$f4,$f5
  db $f5,$f5,$f4,$f3,$f1,$ee,$eb,$e8,$e4,$e0,$dc,$d8,$d4,$d0,$cc,$c8
  db $c3,$bf,$bb,$b7,$b3,$af,$ab,$a6,$a2,$9e,$9a,$96,$92,$8e,$8a,$86
  db $82,$7e,$7a,$76,$72,$6e,$6a,$66,$62,$5e,$59,$55,$51,$4d,$49,$45
  db $40,$3c,$38,$34,$30,$2c,$28,$23,$1f,$1b,$17,$13,$0f,$0b,$08,$04

  ;Padding to ensure sprites dont end up where they shouldn't
  db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
  db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
