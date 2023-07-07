;================================================================
;Rotating Tunnel Demo v2 - 2023.06.12
;For the SEGA Master System
;by Saad Azim
;
; Released under GNU General Public License v3.0
;================================================================

;Enemy patterns for page 1 of enemy sprites made from a pair of enemy movement patterns. I get the feeling that adding any more patterns *might* require expanding the ROM to 64 Kb in size. -_-'

enemyPage01Pattern01Y:
;Sprite Y position - Array size: 112 bytes (code size 288 bytes)
  ;Movement 01
  ;Padding
  db $c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0
  db $c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0

  db $be,$bc,$b9,$b6,$b4,$b1,$ae,$ab,$a7,$a4,$a1,$9d,$9a,$96,$92,$8f
  db $8b,$87,$83,$7f,$7a,$76,$72,$6e,$6a,$66,$62,$5e,$5a,$56,$53,$50
  db $4d,$4c,$4a,$49,$49,$49,$4a,$4b,$4d,$4f,$52,$55,$58,$5c,$60,$64
  db $68,$6c,$70,$74,$78,$7b,$7e,$80,$82,$84,$85,$86,$87,$87,$87,$87
  db $87,$87,$86,$86,$85,$85,$84,$83,$82,$81,$80,$7f,$7e,$7d,$7c,$7b
  db $79,$78,$77,$76,$75,$74,$73,$72,$72,$71,$71,$71,$71,$70,$71,$71
  db $71,$71,$72,$72,$73,$73,$74,$75,$76,$77,$78,$79,$7b,$7c,$7e,$7f

  ;Movement 02 - No need for padding at this point 
  db $b1,$a9,$a3,$9d,$99,$94,$90,$8d,$89,$86,$83,$80,$7d,$7b,$78,$76
  db $74,$72,$70,$6e,$6c,$6a,$69,$67,$66,$64,$63,$61,$60,$5f,$5e,$5d
  db $5c,$5b,$5a,$59,$58,$58,$57,$56,$56,$55,$55,$55,$54,$54,$54,$53
  db $53,$53,$53,$53,$53,$53,$53,$53,$53,$53,$53,$54,$54,$54,$54,$55
  db $55,$56,$56,$57,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f,$61,$62,$63
  db $65,$66,$68,$6a,$6b,$6d,$6f,$71,$73,$75,$77,$79,$7b,$7d,$7f,$81
  db $83,$86,$88,$8a,$8d,$8f,$92,$95,$97,$9a,$9e,$a1,$a4,$a8,$ac,$b1

  ;Movement 03 - Y flipped version of Movement 01
  db $12,$14,$17,$1a,$1c,$1f,$22,$25,$29,$2c,$2f,$33,$36,$3a,$3e,$41
  db $45,$49,$4d,$51,$56,$5a,$5e,$62,$66,$6a,$6e,$72,$76,$7a,$7d,$80
  db $83,$84,$86,$87,$87,$87,$86,$85,$83,$81,$7e,$7b,$78,$74,$70,$6c
  db $68,$64,$60,$5c,$58,$55,$52,$50,$4e,$4c,$4b,$4a,$49,$49,$49,$49
  db $49,$49,$4a,$4a,$4b,$4b,$4c,$4d,$4e,$4f,$50,$51,$52,$53,$54,$55
  db $57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5e,$5f,$5f,$5f,$5f,$60,$5f,$5f
  db $5f,$5f,$5e,$5e,$5d,$5d,$5c,$5b,$5a,$59,$58,$57,$55,$54,$52,$51

  ;Movement 04 - Y flipped version of Movement 02
  db $1f,$27,$2d,$33,$37,$3c,$40,$43,$47,$4a,$4d,$50,$53,$55,$58,$5a
  db $5c,$5e,$60,$62,$64,$66,$67,$69,$6a,$6c,$6d,$6f,$70,$71,$72,$73
  db $74,$75,$76,$77,$78,$78,$79,$7a,$7a,$7b,$7b,$7b,$7c,$7c,$7c,$7d
  db $7d,$7d,$7d,$7d,$7d,$7d,$7d,$7d,$7d,$7d,$7d,$7c,$7c,$7c,$7c,$7b
  db $7b,$7a,$7a,$79,$79,$78,$77,$76,$75,$74,$73,$72,$71,$6f,$6e,$6c
  db $6b,$69,$68,$66,$64,$62,$60,$5f,$5d,$5b,$59,$57,$55,$52,$50,$4e
  db $4c,$49,$47,$44,$42,$3f,$3c,$39,$36,$33,$30,$2c,$29,$24,$20,$1a

  ;Padding, to give sprites a chance to move off screen
  db $c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0
  db $c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0
  db $c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0
  db $c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0,$c0


;Sprite X position - Array size: 224 bytes (code size 576 bytes)
enemyPage01Pattern01XT:
  ;Movement 01
  ;Padding
  db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
  db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
  db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
  db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff

  db $db,$e3,$d8,$e0,$d5,$dd,$d2,$da,$ce,$d6,$cb,$d3,$c8,$d0,$c6,$ce
  db $c3,$cb,$c0,$c8,$be,$c6,$bc,$c4,$b9,$c1,$b7,$bf,$b6,$be,$b4,$bc
  db $b2,$ba,$b1,$b9,$b0,$b8,$af,$b7,$ae,$b6,$ae,$b6,$ae,$b6,$ae,$b6
  db $ae,$b6,$ae,$b6,$af,$b7,$b0,$b8,$b2,$ba,$b4,$bc,$b7,$bf,$ba,$c2
  db $bd,$c5,$c1,$c9,$c5,$cd,$c9,$d1,$cd,$d5,$d1,$d9,$d5,$dd,$d9,$e1
  db $dd,$e5,$e0,$e8,$e4,$ec,$e7,$ef,$e9,$f1,$eb,$f3,$ec,$f4,$ed,$f5
  db $ed,$f5,$ed,$f5,$ec,$f4,$eb,$f3,$e9,$f1,$e6,$ee,$e3,$eb,$e0,$e8
  db $dc,$e4,$d8,$e0,$d4,$dc,$d0,$d8,$cc,$d4,$c8,$d0,$c4,$cc,$c0,$c8
  db $bb,$c3,$b7,$bf,$b3,$bb,$af,$b7,$ab,$b3,$a7,$af,$a3,$ab,$9e,$a6
  db $9a,$a2,$96,$9e,$92,$9a,$8e,$96,$8a,$92,$86,$8e,$82,$8a,$7e,$86
  db $7a,$82,$76,$7e,$72,$7a,$6e,$76,$6a,$72,$66,$6e,$62,$6a,$5e,$66
  db $5a,$62,$56,$5e,$51,$59,$4d,$55,$49,$51,$45,$4d,$41,$49,$3d,$45
  db $38,$40,$34,$3c,$30,$38,$2c,$34,$28,$30,$24,$2c,$20,$28,$1b,$23
  db $17,$1f,$13,$1b,$0f,$17,$0b,$13,$07,$0f,$03,$0b,$ff,$08,$ff,$04
  ;Movement 02
  db $6d,$75,$71,$79,$74,$7c,$77,$7f,$79,$81,$7c,$84,$7e,$86,$80,$88
  db $83,$8b,$85,$8d,$87,$8f,$89,$91,$8b,$93,$8d,$95,$8f,$97,$91,$99
  db $92,$9a,$94,$9c,$96,$9e,$98,$a0,$9a,$a2,$9b,$a3,$9d,$a5,$9f,$a7
  db $a1,$a9,$a2,$aa,$a4,$ac,$a6,$ae,$a7,$af,$a9,$b1,$ab,$b3,$ac,$b4
  db $ae,$b6,$b0,$b8,$b1,$b9,$b3,$bb,$b4,$bc,$b6,$be,$b8,$c0,$b9,$c1
  db $bb,$c3,$bc,$c4,$be,$c6,$bf,$c7,$c1,$c9,$c2,$ca,$c4,$cc,$c5,$cd
  db $c6,$ce,$c8,$d0,$c9,$d1,$cb,$d3,$cc,$d4,$cd,$d5,$cf,$d7,$d0,$d8
  db $d1,$d9,$d2,$da,$d4,$dc,$d5,$dd,$d6,$de,$d8,$e0,$d9,$e1,$da,$e2
  db $db,$e3,$dd,$e5,$de,$e6,$df,$e7,$e1,$e9,$e2,$ea,$e3,$eb,$e4,$ec
  db $e5,$ed,$e7,$ef,$e8,$f0,$e9,$f1,$ea,$f2,$eb,$f3,$eb,$f3,$ec,$f4
  db $ed,$f5,$ed,$f5,$ee,$f6,$ef,$f7,$ef,$f7,$ef,$f7,$f0,$f8,$f0,$f8
  db $f0,$f8,$f0,$f8,$f0,$f8,$f0,$f8,$f0,$f8,$f0,$f8,$f0,$f8,$f0,$f8
  db $f0,$f8,$ef,$f7,$ef,$f7,$ef,$f7,$ee,$f6,$ee,$f6,$ed,$f5,$ec,$f4
  db $ec,$f4,$eb,$f3,$ea,$f2,$e9,$f1,$e8,$f0,$e7,$ef,$e5,$ed,$e3,$eb
  ;Movement 03 - Copy of movement 01
  db $db,$e3,$d8,$e0,$d5,$dd,$d2,$da,$ce,$d6,$cb,$d3,$c8,$d0,$c6,$ce
  db $c3,$cb,$c0,$c8,$be,$c6,$bc,$c4,$b9,$c1,$b7,$bf,$b6,$be,$b4,$bc
  db $b2,$ba,$b1,$b9,$b0,$b8,$af,$b7,$ae,$b6,$ae,$b6,$ae,$b6,$ae,$b6
  db $ae,$b6,$ae,$b6,$af,$b7,$b0,$b8,$b2,$ba,$b4,$bc,$b7,$bf,$ba,$c2
  db $bd,$c5,$c1,$c9,$c5,$cd,$c9,$d1,$cd,$d5,$d1,$d9,$d5,$dd,$d9,$e1
  db $dd,$e5,$e0,$e8,$e4,$ec,$e7,$ef,$e9,$f1,$eb,$f3,$ec,$f4,$ed,$f5
  db $ed,$f5,$ed,$f5,$ec,$f4,$eb,$f3,$e9,$f1,$e6,$ee,$e3,$eb,$e0,$e8
  db $dc,$e4,$d8,$e0,$d4,$dc,$d0,$d8,$cc,$d4,$c8,$d0,$c4,$cc,$c0,$c8
  db $bb,$c3,$b7,$bf,$b3,$bb,$af,$b7,$ab,$b3,$a7,$af,$a3,$ab,$9e,$a6
  db $9a,$a2,$96,$9e,$92,$9a,$8e,$96,$8a,$92,$86,$8e,$82,$8a,$7e,$86
  db $7a,$82,$76,$7e,$72,$7a,$6e,$76,$6a,$72,$66,$6e,$62,$6a,$5e,$66
  db $5a,$62,$56,$5e,$51,$59,$4d,$55,$49,$51,$45,$4d,$41,$49,$3d,$45
  db $38,$40,$34,$3c,$30,$38,$2c,$34,$28,$30,$24,$2c,$20,$28,$1b,$23
  db $17,$1f,$13,$1b,$0f,$17,$0b,$13,$07,$0f,$03,$0b,$ff,$08,$ff,$04
  ;Movement 04 - Copy of movement 02
  db $6d,$75,$71,$79,$74,$7c,$77,$7f,$79,$81,$7c,$84,$7e,$86,$80,$88
  db $83,$8b,$85,$8d,$87,$8f,$89,$91,$8b,$93,$8d,$95,$8f,$97,$91,$99
  db $92,$9a,$94,$9c,$96,$9e,$98,$a0,$9a,$a2,$9b,$a3,$9d,$a5,$9f,$a7
  db $a1,$a9,$a2,$aa,$a4,$ac,$a6,$ae,$a7,$af,$a9,$b1,$ab,$b3,$ac,$b4
  db $ae,$b6,$b0,$b8,$b1,$b9,$b3,$bb,$b4,$bc,$b6,$be,$b8,$c0,$b9,$c1
  db $bb,$c3,$bc,$c4,$be,$c6,$bf,$c7,$c1,$c9,$c2,$ca,$c4,$cc,$c5,$cd
  db $c6,$ce,$c8,$d0,$c9,$d1,$cb,$d3,$cc,$d4,$cd,$d5,$cf,$d7,$d0,$d8
  db $d1,$d9,$d2,$da,$d4,$dc,$d5,$dd,$d6,$de,$d8,$e0,$d9,$e1,$da,$e2
  db $db,$e3,$dd,$e5,$de,$e6,$df,$e7,$e1,$e9,$e2,$ea,$e3,$eb,$e4,$ec
  db $e5,$ed,$e7,$ef,$e8,$f0,$e9,$f1,$ea,$f2,$eb,$f3,$eb,$f3,$ec,$f4
  db $ed,$f5,$ed,$f5,$ee,$f6,$ef,$f7,$ef,$f7,$ef,$f7,$f0,$f8,$f0,$f8
  db $f0,$f8,$f0,$f8,$f0,$f8,$f0,$f8,$f0,$f8,$f0,$f8,$f0,$f8,$f0,$f8
  db $f0,$f8,$ef,$f7,$ef,$f7,$ef,$f7,$ee,$f6,$ee,$f6,$ed,$f5,$ec,$f4
  db $ec,$f4,$eb,$f3,$ea,$f2,$e9,$f1,$e8,$f0,$e7,$ef,$e5,$ed,$e3,$eb

  ;Padding
  db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
  db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
  db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
  db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
  db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
  db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
  db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
  db $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff