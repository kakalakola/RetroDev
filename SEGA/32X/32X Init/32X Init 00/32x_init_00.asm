;----------------------------------------------------
;32x ASM Project 0.0 - 68000 - 2021.12.18
;(Compiling a working file)
;by Saad Azim
;Reference:
;http://devster.monkeeh.com/segapage.html
; - http://devster.monkeeh.com/sega/32x/maincode.68k
;http://info.sonicretro.org/Sonic_the_Hedgehog_32X
; - http://info.sonicretro.org/File:Sonic32_1.0-src.rar
;http://www.valpocl.com/SuperVDP/
; - www.valpocl.com/SuperVDP/SVDP.zip
;----------------------------------------------------

;This is a *very* quick way to get the 32X to boot. In MAME, at any rate, since I don't have any way of testing on original hardware. All this does is setup 3 counters. The 68000, SH2 Master, and SH2 Slave increment their respective work RAM addresses at $ff0000, $06000800, and $06000810, respectively.

;Beyond that things seem to be somewhat glitchy. Trying to read OR write to D0 seems to crash MAME. Still, this seems to be the minimum amount (or close to it) of code needed to get a bootable 32X ROM.

romSize equ (romEnd-$880001)

  ;Considerations for a 32X ROM: On the 68000 side, all addresses need to be incremented by $880000. The 512kb cartridge area of $000000-$07ffff is remapped mapped to $880000-$9fffff. When RV bit in DMA request control register ($a15106) is set to 0, 4Mb of additional cartridge area can be accessed at $90000-$9fffff as a set of 4 pages (n00000-nfffff)

  org $880000

  dc.l $fffe00              ;Stack pointer                            $0000
  dc.l $0003f0              ;32X boot code, $03f0 , labels also work  $0004

  ;Vectors for Bus Error to LineFmulator, 10 entries            $0008-$002f
  dc.l error,error,error,error,error,error,error,error,error,error

  ;Reserved by Motorola, 12 entries                             $0030-$005f
  dc.l 0,0,0,0,0,0,0,0,0,0,0,0

  dc.l error                ;Spurious Exception                       $0060

  ;IRQs, 7 entries                                              $0064-$007f
  dc.l error
  dc.l error
  dc.l error
  dc.l error
  dc.l error
  dc.l error
  dc.l error

  ;Trap 00 Exception - Trap 15 Exception, 16 entries            $0080-$00bf
  dc.l error,error,error,error,error,error,error,error
  dc.l error,error,error,error,error,error,error,error

  ;Reserved by Motorola, 16 entries                             $00c0-$00ff
  dc.l 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

  ;Genesis header
  dc.b "SEGA 32X        "   ;Console name Genesis/Mega Drive          $0100
                            ;Must be 16 characters
  dc.b "(C)LBP  "           ;(C)+Firm name/code, 8 characters         $0110
  dc.b "2021.DEC"           ;Build date (YYYY.MMM), 8 characters      $0118   

  dc.b "SEGA 32X COMPILATION/INITIALIZATION TEST 01     ";            $0120
                            ;Domestic name, 48 characters
  dc.b "SEGA 32X COMPILATION/INITIALIZATION TEST 01     ";            $0150
                            ;Foreign name,48 characters
  dc.b "GM 00000000-01"     ;ROM Type, serial & version number        $0180
  dc.w $ffff                ;Checksum, 2 bytes                        $018e
  dc.b "J"                  ;Input support, up to 16 bytes,           $0190

  dcb.b $8801a0-*," "       ;Variable padding until the *start of*$0001a0

  ;ROM Info: ROM start, ROM end, RAM start, RAM end; 16 bytes         $01a0
  dc.l $00000000,romSize,$00ff0000,$00ffffff

  dcb.b $8801c8-*," "         ;Padding

  dc.b "AN INITILIAZTION ROUTINE FOR THE 32X    ";Notes, 40 char.     $01c8

  ;Region support                                                     $01f0
  dc.b "D"                  ;New country code, 1 byte

  dcb.b $880200-*," "       ;Padding

  ;32X vector table, JMP+address combo, like the Z80 :D
  jmp start68K              ;68K reset code                           $0200

  ;Vectors for Bus Error to LineFEmulator, 10 entries           $0206-$0241
  jmp error
  jmp error
  jmp error
  jmp error
  jmp error
  jmp error
  jmp error
  jmp error
  jmp error
  jmp error

  ;Reserved by Motorola, 12 entries                             $0242-$0289
  dc.l 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

  ;Spurious Exception                                                 $028a
  jmp error

  ;IRQs, IRQs, 7 entries   $0290-$02b9
  jmp error
  jmp error
  jmp error
  jmp error
  jmp error
  jmp error
  jmp error

  ;Trap 00 Exception - Trap 15 Exception, 16 entries            $02ba-$0301
  jmp error
  jmp error
  jmp error
  jmp error
  jmp error
  jmp error
  jmp error
  jmp error
  jmp error
  jmp error
  jmp error
  jmp error

  ;Reserved by Motorola, 16 entries                             $0302-$0362
  dc.l 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

  ;Padding
  dcb.b $8803c0-*,0
  dc.b "32X INIT TEST   "   ;32X header 16 bytes                $03c0-$03cf
  dc.l $00000000            ;Version number                           $03d0

  ;32X RAM offsets
  dc.l (sh2Master-$880000)  ;Location of SH2 Master ROM in cart       $03d0
  dc.l $00000000            ;RAM offset to copy SH2 code into         $03d8
  dc.l romEnd-sh2Master     ;Size of SH2 code (for Master & Slave)    $03dc

  dc.l $06000120            ;Master SH2 code start location           $03e0
                            ;(start of 32X RAM + $120), Address the Master SH2 jumps to, when booting

  dc.l $06000120+(sh2Slave-sh2Master);Slave SH2 code start location   $03e4
                            ;Address the Slave SH2 jumps to, when booting

  dc.l $06000000            ;Master SH2 VBR                           $03e8
                            ;The Vector Base Register basically tells the SH2 where to look for the jump vectors, i.e. code start location, stack address, and interrupt handlers

  dc.l $06000000+(sh2Slave-sh2Master);Slave SH2 VBR                   $03ec

  ;32X initialization code, starting at                               $03f0
  move.l #$ffffffc0,a4
  move.l #0,($a15128)

  ;32x security code                                            $0400-$07ff
  dc.l $46fc2700,$4bf900a1,$00007001,$0cad4d41,$525330ec,$660003e6,$082d0007,$510167f8
  dc.l $4aad0008,$67104a6d,$000c670a,$082d0000,$51016600,$03b8102d,$00010200,$000f6706
  dc.l $2b78055a,$40007200,$2c414e66,$41f90000,$04d46100,$01526100,$017647f9,$000004e8
  dc.l $43f900a0,$000045f9,$00c00011,$3e3c0100,$70003b47,$11003b47,$1200012d,$110066fa
  dc.l $742512db,$51cafffc,$3b401200,$3b401100,$3b471200,$149b149b,$149b149b,$41f90000
  dc.l $04c043f9,$00ff0000,$22d822d8,$22d822d8,$22d822d8,$22d822d8,$41f900ff,$00004ed0
  dc.l $1b7c0001,$510141f9,$000006bc,$d1fc0088,$00004ed0,$0404303c,$076c0000,$0000ff00
  dc.l $81370002,$01000000,$af01d91f,$11270021,$2600f977,$edb0dde1,$fde1ed47,$ed4fd1e1
  dc.l $f108d9c1,$d1e1f1f9,$f3ed5636,$e9e99fbf,$dfff4d41,$52532049,$6e697469,$616c2026
  dc.l $20536563,$75726974,$79205072,$6f677261,$6d202020,$20202020,$20202043,$61727472
  dc.l $69646765,$20566572,$73696f6e,$20202020,$436f7079,$72696768,$74205345,$47412045
  dc.l $4e544552,$50524953,$45532c4c,$54442e20,$31393934,$20202020,$20202020,$20202020
  dc.l $20202020,$20202020,$20202020,$20202020,$20202020,$2020524f,$4d205665,$7273696f
  dc.l $6e20312e,$300048e7,$c04043f9,$00c00004,$3011303c,$8000323c,$01003e3c,$00121018
  dc.l $3280d041,$51cffff8,$4cdf0203,$4e7548e7,$81c041f9,$0000063e,$43f900c0,$00043298
  dc.l $32983298,$32983298,$32983298,$22983341,$fffc3011,$08000001,$66f83298,$32987000
  dc.l $22bcc000,$00007e0f,$3340fffc,$3340fffc,$3340fffc,$3340fffc,$51cfffee,$22bc4000
  dc.l $00107e09,$3340fffc,$3340fffc,$3340fffc,$3340fffc,$51cfffee,$4cdf0381,$4e758114
  dc.l $8f0193ff,$94ff9500,$96009780,$40000080,$81048f02,$48e7c140,$43f900a1,$518008a9
  dc.l $0007ff80,$66f83e3c,$00ff7000,$7200337c,$00ff0004,$33410006,$33400008,$4e710829
  dc.l $0001000b,$66f80641,$010051cf,$ffe84cdf,$02834e75,$48e78180,$41f900a1,$520008a8
  dc.l $0007ff00,$66f83e3c,$001f20c0,$20c020c0,$20c051cf,$fff64cdf,$01814e75,$41f900ff
  dc.l $00003e3c,$07ff7000,$20c020c0,$20c020c0,$20c020c0,$20c020c0,$51cfffee,$3b7c0000
  dc.l $12007e0a,$51cffffe,$43f900a1,$51007000,$23400020,$23400024,$1b7c0003,$51012e79
  dc.l $00880000,$08910007,$66fa7000,$33400002,$33400004,$33400006,$23400008,$2340000c
  dc.l $33400010,$33400030,$33400032,$33400038,$33400080,$33400082,$08a90000,$008b66f8
  dc.l $6100ff12,$08e90000,$008b67f8,$6100ff06,$08a90000,$008b6100,$ff3c303c,$00402229
  dc.l $00200c81,$53514552,$67000092,$303c0080,$22290020,$0c815344,$45526700,$008021fc
  dc.l $008802a2,$0070303c,$00027200,$122d0001,$14290080,$e14a8242,$0801000f,$660a0801
  dc.l $00066700,$00586008,$08010006,$6600004e,$702041f9,$00880000,$3c28018e,$4a466700
  dc.l $00103429,$00280c42,$000067f6,$b446662c,$70002340,$00282340,$002c3e14,$2c7cffff
  dc.l $ffc04cd6,$7ff944fc,$00006014,$43f900a1,$51003340,$0006303c,$80006004,$44fc0001

start68K:
  move.l #"M_OK",d0
  move.l #"S_OK",d1

waitFor32x:                 ;Check to see if the 32X has initialized properly
  cmp.l ($a15120),d0        ;Check com port for status from Master SH2
  bne waitFor32x

  cmp.l ($a15124),d1        ;heck com port for status from Slave SH2
  bne waitFor32x

  move.w #$2700,sr          ;Disable ALL interrupts

  ;Check TMSS
  move.b $a10001,d0
  andi.b #$f,d0
  beq noTMSS
  move.l #"SEGA",$a14000

noTMSS:

  ;Clear a long word in RAM, then increment it continuously
  move.l #0,$ff0000
mainLoop:
  move.l $ff0000,d0
  addq.l #1,$ff0000
  jmp mainLoop

error:
  nop
  jmp error

  cnop 0,4                  ;Data for SH2 need to be aligned by long word

sh2Master:
  incbin  "32x_init_00_sh2_master.bin"
sh2Slave:
  incbin  "32x_init_00_sh2_slave.bin"
romEnd: