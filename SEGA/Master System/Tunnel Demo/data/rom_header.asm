;================================================================
;Rotating Tunnel Demo v2 - 2023.06.12
;For the SEGA Master System
;by Saad Azim
;
; Released under GNU General Public License v3.0
;================================================================

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

;----------------------------------------------------------------
;ROM header - can be at $1FF0, $3FF0, or $7FFO
;----------------------------------------------------------------

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
                            ;Bits 3-0 used for cart size and checksum range
                            ;($C:32KB||$e:64KB -rarely used-||$f:128KB||0:256KB||1:512KB -rarely used-)
