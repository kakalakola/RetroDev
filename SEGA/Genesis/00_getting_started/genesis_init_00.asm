;----------------------------------------------------
;GENESIS ASM: Init 0.0
;(Compiling a working file)
;by Saad Azim
;----------------------------------------------------

  ;A buttload of thanks to https://darkdust.net/index.php/writings/megadrive, http://www.genny4ever.net, http://www.hacking-cult.org, https://segaretro.org, https://www.romhacking.net, and https://en.wikibooks.org/wiki/Genesis_Programming

  ;The purpose of this code is to boot a MegaDrive/Genesis, and diplay a color on screen. It's definitely not the best way to go about doing this, but it's certainly the quickest. Hopefully, this helps someone get accustomed to the code structure of an MD/Gen. ROM.

  ;All ROMs start with a 256 byte ($100 bytes, in hex) vector table, which tells the 68K things like where the code starts. Or which subroutine to jump to, when something like an interrupt or address error is triggered

  ;There's an easier, more compact way of defining vectors. Something along the lines of dc.l $fffe00,codeStart,error,error,error... However, it's also pretty easy to lose track of which vector is suppose to be the called for which situation, i.e. an address error, or a TRAPV exception


  ;Begining of vector table at $0000
  dc.l $fffe00              ;Stack pointer                        $0000
  dc.l codeStart            ;Code start, best left as a label     $0004
  dc.l error                ;Bus error                            $0008
  dc.l error                ;Address error                        $000c
  dc.l error                ;Illegal instruction                  $0010
  dc.l error                ;Division by zero                     $0014
  dc.l error                ;CHK exception                        $0018
  dc.l error                ;TRAPV exception                      $001c
  dc.l error                ;Privilege violation                  $0020
  dc.l error                ;Trace Exception                      $0024
  dc.l error                ;LINE 1010 EMULATOR                   $0028
  dc.l error                ;LINE 1111 EMULATOR                   $002c
  dc.l error                ;Reserved by Motorola                 $0030
  dc.l error                ;Reserved by Motorola                 $0034
  dc.l error                ;Reserved by Motorola                 $0038
  dc.l error                ;Reserved by Motorola                 $003c
  dc.l error                ;Reserved by Motorola                 $0040
  dc.l error                ;Reserved by Motorola                 $0044
  dc.l error                ;Reserved by Motorola                 $0048
  dc.l error                ;Reserved by Motorola                 $004c
  dc.l error                ;Reserved by Motorola                 $0050
  dc.l error                ;Reserved by Motorola                 $0054
  dc.l error                ;Reserved by Motorola                 $0058
  dc.l error                ;Reserved by Motorola                 $005c
  dc.l error                ;Spurious exception                   $0060
  dc.l error                ;IRQ level 1                          $0064
  dc.l error                ;IRQ level 2                          $0068
  dc.l error                ;IRQ level 3                          $006c
  dc.l error                ;IRQ level 4 (VDP interrupt/hblank)   $0070
  dc.l error                ;IRQ level 5                          $0074
  dc.l error                ;IRQ level 6 (vblank)                 $0078
  dc.l error                ;IRQ level 7                          $007c
  dc.l error                ;TRAP #00 exception                   $0080
  dc.l error                ;TRAP #01 exception                   $0084
  dc.l error                ;TRAP #02 exception                   $0088
  dc.l error                ;TRAP #03 exception                   $008c
  dc.l error                ;TRAP #04 exception                   $0090
  dc.l error                ;TRAP #05 exception                   $0094
  dc.l error                ;TRAP #06 exception                   $0098
  dc.l error                ;TRAP #07 exception                   $009c
  dc.l error                ;TRAP #08 exception                   $00a0
  dc.l error                ;TRAP #09 exception                   $00a4
  dc.l error                ;TRAP #10 exception                   $0018
  dc.l error                ;TRAP #11 exception                   $00ac
  dc.l error                ;TRAP #12 exception                   $00b0
  dc.l error                ;TRAP #13 exception                   $00b4
  dc.l error                ;TRAP #14 exception                   $00b8
  dc.l error                ;TRAP #15 exception                   $00bc
  dc.l error                ;Reserved by Motorola                 $00c0
  dc.l error                ;Reserved by Motorola                 $00c4
  dc.l error                ;Reserved by Motorola                 $00c8
  dc.l error                ;Reserved by Motorola                 $00cc
  dc.l error                ;Reserved by Motorola                 $00d0
  dc.l error                ;Reserved by Motorola                 $00d4
  dc.l error                ;Reserved by Motorola                 $00d8
  dc.l error                ;Reserved by Motorola                 $00dc
  dc.l error                ;Reserved by Motorola                 $00e0
  dc.l error                ;Reserved by Motorola                 $00e4
  dc.l error                ;Reserved by Motorola                 $00e8
  dc.l error                ;Reserved by Motorola                 $00ec
  dc.l error                ;Reserved by Motorola                 $00f0
  dc.l error                ;Reserved by Motorola                 $00f4
  dc.l error                ;Reserved by Motorola                 $00f8
  dc.l error                ;Reserved by Motorola                 $00fc

  ;ROM header, starting from $000100. Also 256 bytes long-word.
  dc.b "SEGA GENESIS    "   ;Console name Genesis/Mega Drive      $0100
                            ;Must be 16 characters long-word
  dc.b "(C)LBP  "           ;(C)+Firm name/code, 8 characters     $0110
  dc.b "2018.NOV"           ;Build date, 8 characters             $0118   

  dc.b "GENESIS COMPILATION/INITIALIZATION TEST         ";        $0120
                            ;Domestic name, 48 characters
  dc.b "MEGADRIVE COMPILATION/INITIALIZATION TEST       ";        $0150
                            ;Foreign name,48 characters

  dc.b "GM 00000000-00"     ;ROM Type, serial & version number    $0180
                            ;(GM: game, AL: Educational) + " " + 8 digit serial number + "-" + 2 digit version number for a total of 14 characters

  dc.w $ffff                ;Checksum, 2 bytes                    $018e
                            ;Best to fix this with an extrnal program after the ROM has been compiled

  dc.b "J               "   ;Input support, 16 characters         $0190
  ;Said inputs, in order of appearance in the header, are as follows.
  ;"J": 3 button joypad     "6": 6 button joypad    "K": keyboard
  ;"P": printer             "B": trackball          "4": Team Play
  ;"0": SMS Joystick        "R": serial rs232c      "T": tablet
  ;"V": paddle controller   "L": Activator          "M": Mega Mouse
  ;"F": floppy disk drive   "C": CD-ROM

  dc.l $00000000            ;Beginning of ROM, 4 bytes            $01a0
  dc.l romEnd-1             ;End of ROM, 4 bytes                  $01a4
                            ;(Best to use label-1)

  dc.l $00ff0000            ;Beginning of RAM, 4 bytes            $01a8
  dc.l $00ffffff            ;End of RAM                           $01ac

  dc.b "            "       ;SRAM (not used in this ROM)          $01b0
  ;When not used, all 12 bytes of SRAM data is padded with blank spaces ($20)

  dc.b "            "       ;Modem support, 12 characters         $01bc

  dc.b "AN INITILIAZTION ROUTINE FOR THE GENESIS";Notes, 40 char. $01c8

  dc.b "JUE"                ;Country code, 3 characters           $01f0
  ;Though only 3 country codes can be selected at any given time, the available codes are, in order of appearance in header, as follows.
  ;"J": Japan       "U": United States    "E": Europe     "A": Asia
  ;"B": Brazil      "4": Brazil 4(?!)     "F": France     "8": Hong Kong

  dc.b "             "   ;Padding 13 characters                   $01f3

;Beginning of code at $0200
codeStart:
  move.w #$2700,sr          ;Disable ALL interrupts

  ;NOTE: ($a10008).l is the same as ($a10008), which is the same as ($00a10008). Addresses are technically 32 bit, though the highest 8 bits are ignored effectively making the address range $000000-$ffffff

  ;While the 68K has a 16 bit data bus, 32 bit read/write commands are interpreted as 2 16 bit reads/writes.

  ;Check TMSS
  move.b $a10001,d0         ;MOVE 1 Byte of data from $a10001
                            ;(version register) to the data register D0

  andi.b #$f,d0             ;AND Immediate the first Byte of data
                            ;in D0 with $f, which should result in all 0-s on a genesis 1

  beq noTMSS                ;Branch if EQual to 0, to noTMSS
                            ;Otherwise, we're dealing with a Model 2+

  move.l #"SEGA",$a14000    ;So it's time to write the long-word "SEGA"
                            ;($53454741) to the TMSS register

  ;Having dealt with TMSS, whether it's there or not, it's time to set a background color 

noTMSS:

  move.l #$c0000000,$c00004 ;Tell the control port we want to
                            ;write data to palette 0, color 0, in CRAM

  move.w #$0eee,$c00000     ;Send the color value, which is the 
                            ;brightest white supported by the Genesis VDP, to the data port, which gets written to p0c0 in CRAM

  ;The color value is stored as Blue, Green, and Red. So $0e00 results in a blue background, $00e0 results in a green background, and $000e results in a red background

  ;A binary representation of #$0eee would look like:
  ;#%0000111011101110
    ;|:|:|:|:|:|:|:|:
    ;|:|:|:|:|:|:|:|x
    ;|:|:|:|:|:|:3 bit value for red
    ;|:|:|:|:|:|x
    ;|:|:|:|:3 bit value for green
    ;|:|:|:|x
    ;|:|:3 bit value for blue
    ;xxxx
  ;The bits marked with "x" are ignored.

  ;Essentially "active" values for color are 0, 2, 4, 6, 8, a, c, and e
  ;With 0 and 1 resulting in the same color, 2 and 3 resulting in the same color, all the way to e and f resulting in the same color.

main:
  jmp main                  ;Loop indefinitely

error:
  jmp error                 ;Loop indefinitely... again?

  ;Fun to think all this compiles to 54 bytes of code ^_^

romEnd:                     ;That's all folks.
