;----------------------------------------------------
;SEGA CD ASM Project 0.0 - 2021.12.18
;Getting things to work... -_-'
;by Saad Azim
;----------------------------------------------------

;This project boots the SEGA CD, changes the background color, halts the Sub CPUs from the Main CPU
;The main CPU increments a counter at $ff0010
;The sub CPU increments various counters at $006300-$006314 while it's running

;Tested, and works in MAME

;SEGA CD header (Based on Sonic CD header)
;Sector 00: $00000000-$000007FF
;(Each sector is $800 bytes in size)


  org $000000               ;Optional

  dc.b "SEGADISCSYSTEM  "   ;Disc Type (Must be "SEGADISCSYSTEM  ", 16 characters)  $0000-$000f
  dc.b "SEGACDGAME  "       ;Disc ID, 11 characters + 1 space                       $0010-$001b
                            ;Disc Volume name in SCD Tech. Bulletin
  dc.w $0100                ;System ID                                              $001c-$001f
                            ;Volume version in SCD Tech. Bulletin
  dc.w $0001                ;System ID Type                                         $0020-$0023
                            ;Volume Type in SCD Tech. Bulletin
  dc.b "SEGACDGAME  "       ;System Name, 11 characters + 1 space                   $0024-$002f

  dc.w $0000                ;System Version                                         $0030-$0033

  dc.w $0000                ;System Version Type                                    $0034-$0037

  ;Initialization program info

  dc.l $00000800            ;Main CPU IP CD location/offset ($00000800)             $0038-$003f
  dc.l $00000800            ;Main CPU IP CD size ($00000800)                        $0040-$0047
  dc.l $00000000            ;Main CPU IP entry offset                               $0048-$004f
  dc.l $00000000            ;Main CPU IP work RAM size                              $0050-$0057

  dc.l $00001000            ;Sub CPU IP CD offset ($00001000)                       $0058-$005f
  dc.l $00007000            ;Sub CPU IP CD size ($00007000)                         $0060-$0067
  dc.l $00000000            ;Sub CPU IP entry offset                                $0068-$006f
  dc.l $00000000            ;Sub CPU IP work RAM size                               $0070-$0077

  dc.b "01082021"           ;Date - MMDDYYYY, 16 characters                         $0078-$0087

  dcb.b $0100-*," "         ;Define Constant Block. Basically, padding until $0100.

;----------------------------------------------------
;Game header, similar to, but not exactly like the Genesis
;$00000100-$000001FF
;Main CPU
;----------------------------------------------------
  dc.b "SEGA GENESIS    "   ;Console name Genesis/Mega Drive                              $0100
                            ;Must be 16 characters
  dc.b "(C)LBP  "           ;(C)+Firm name/code, 8 characters                             $0110
  dc.b "2021.JAN"           ;Build date (YYYY.MMM), 8 characters                          $0118   

  dc.b "SEGA CD COMPILATION/INITIALIZATION TEST         ";                                $0120
                            ;Domestic name, 48 characters
  dc.b "MEGA CD COMPILATION/INITIALIZATION TEST         ";                                $0150
                            ;Foreign name,48 characters

  dc.b "GM 00000000-00"     ;ROM Type, serial & version number                            $0180

  dcb.b $0190-*," "         ;Padding until $0190, since checksum is not needed

  dc.b "J"                  ;Input support, up to 16 bytes                                $0190

  ;Input type:
  ;"0": SMS controller      "4": Multitap       "6": 6 button controller
  ;"A": Analog controller   "B": Trackball      "C": CD-ROM (SEGA CD...?)
  ;"D": Download (?)        "F": Floppy drive   "G": Light gun
  ;"K": Keyboard/Keypad     "L": Activator      "J":3 button controller
  ;"M": Mouse               "P": Printer        "R": RS-232
  ;"T": Tablet              "V": Paddle

  dcb.b $01bc-*," "         ;Padding
                            ;ROM start, ROM end, RAM start, RAM end, and SRAM info are not needed

  ;Modem info, 12 bytes, pad with space if nothing's present
  ;dc.b "MO"                 ;MOdem present, 2 bytes                   $01bc
  ;dc.b "LBP "               ;Firm name, same as $0113(?), 4 bytes     $01be
  ;dc.b "00.0"               ;Modem number.version, 4 bytes            $01c2
  ;dc.b "00"                 ;Region & microphone, 2 bytes             $01c2
  dc.b "            "

  dcb.b $0200-*," "         ;Padding
                            ;Microphone info is not needed (?)

;----------------------------------------------------
;Region specific BIOS (Some data remains in the register after running)
;$00000200-$000007FF
;----------------------------------------------------
  incbin "usa.bin"

  bra initProgram           ;BRAnch to the start of the initProgram
                            ;Using JMP causes issues
  dcb.b $0800-*,0

  ;DCB uses direct addressing, i.e. pad until $0800 has been reached. This is great for the CD header, or cartridge games where addresses tend to stay relative.

  ;For relative padding, CNOP offset,amount seems to be a better option. CNOP 0,$0800 will pad the binary file to the nearest $0800 byte "block", rather than explicitly to $0800

initProgram:                ;Corresponds with code at initSP: on the Sub CPU side

;----------------------------------------------------
;SEGA CD Initialization program:
;Runs in the Main CPU RAM at $ff0600
;Sector-01-02==$00000800-$00000fff
;----------------------------------------------------
  bset #1, $a12003          ;Give Word RAM access to Sub CPU
                            ;Using BSET #1 == MOVE.B #2

  ;$a12003==%00000000
            ;|:|:|:|:
            ;|:|:|:|Reset (R/W) (reset|run)
            ;|:|:|:Sub CPU Bus Request (R/W) (cancel|request)
            ;xxxxxx

loopLoadSubCPUInit:  
  tst.b $a1200f             ;TeST for acknowledgement from Sub CPU
  bne loopLoadSubCPUInit    ;Loop until the Sub CPU acknowledges it has access to Word RAM

  ;SEGA CD CPU Communication Registers:

  ;$a12003==%00000000
            ;|:|:|:|:
            ;Main CPU Communication Flag (R/W)

  ;$a1200f==%00000000
            ;|:|:|:|:
            ;Sub CPU Communication Flag (R)

;Loop once more, just to see if $a1200f is 0...?
;The following part can be reduced in size via JSR/RTS to and from subroutines, but for the sake of simplicity & understanding, this is done manually for now

  ;Commands issued from Main CPU need to be set command/clear command pairs. The SubCPU can't clear bits in $a1200e. The way to communicate is as follows:
  ; - Set command via $a1200e
  ; - Wait for Sub CPU to to acknowledge command by writing 1 to $a1200f
  ; - Clear command via $a1200e
  ; - Wait for Sub CPU to to acknowledge clear command by clearing $a1200f

loopCommand_0_0:
  tst.b $a1200f             ;TeST one byte of data in $a1200f to see if Sub CPU is ready
  bne loopCommand_0_0

  move.b #0,$a1200e         ;Clear command  -> Write 0, wait for $a1200f to be not 0
                            ;Corresponds with mainSP: looping if $a1200e is NOT 0
loopCommandResponse_0_0:    ;Wait for Sub CPU
  tst.b $a1200f
  beq loopCommandResponse_0_0

  ;Set command/clear command pair for command #1 -> load file
  move.b #1,$a1200e
loopCommand_0_1:
  tst.b $a1200f
  bne loopCommand_0_1

  move.b #0,$a1200e
loopCommandResponse_0_1:
  tst.b $a1200f
  beq loopCommandResponse_0_1


  ;Set command/clear command pair for command #2 -> Give Word RAM to Main CPU
  ;There's no reason to clear the command since this particular DEMO doesn't require any future
  move.b #2,$a1200e
loopCommand_0_2:
  tst.b $a1200f
  bne loopCommand_0_2

  move.b #0,$a1200e
loopCommandResponse_0_2:
  tst.b $a1200f
  beq loopCommandResponse_0_2

  ;Once everything is ready, it's time to disable the Sub CPU by requesting the Sub CPU bus. This DOES have the weird side effect of leaving the Red LED (access) on.

  bset #1,$a12001           ;Request Sub CPU
                            ;It's best to set the exact bit needed

  ;$a12001==%00000000
            ;|:|:|:|:
            ;|:|:|:|Reset (R/W) (reset|run)
            ;|:|:|:Sub CPU Bus Request (R/W) (cancel|request)
            ;xxxxxx

  ;Sub CPU Bus Request:
  ; - Write (Give bus to Sub CPU|Request bus from Sub CPU)
  ; - Read (Sub CPU has bus|Main CPU has bus)

subCPUWaitLoop:
  btst #1,$a12001
  beq subCPUWaitLoop

  jmp  $200000              ;Jump to Word RAM and execute Sub CPU code from there

  dcb.b $1000-*,0

;----------------------------------------------------
;Sega CD Sub Program
;This is basically the OS that runs on the Segs CD CPU
;Sector-03-0f==$00001000-$00008000
;----------------------------------------------------

;Visible at $026000 and $006000 in Main CPU and Sub CPU program space memory, respectively
  org $1000                 ;The PC NEEDS to be $1000

mainSubCPU:
  dc.b "MAIN-SUBCPU"        ;Module name, 11 characters                             ;$1000-$100a
  dc.b $00                  ;NULL terminated string                                       ;$100b
  dc.w $0000                ;Version                                                ;$100c-$100d
                            ;$0000-$0099: Pre-release version
                            ;$0100-$ffff: Release version

  dc.w $0000                ;Type                                                   ;$100e-$100f
                            ;$0000:       Normal type

  dc.l $00000000            ;Next header (0 if no header)                           ;$1010-$1013

  ;The following values could be hard coded, or relative. It doesn't matter, since they're fixed.
  ;dc.l (mainSubCPUEnd-mainSubCPU) ;Module size (including header)                   ;$1014-$1017
  ;dc.l (jumpTableSP-mainSubCPU) ;Module start address                               ;$1018-$101b

  dc.l $00007000            ;Module/ROM size (including header)                     ;$1014-$1017
                            ;Total number of bytes to be initialized with the program code

  dc.l $00000020            ;Module start address (jumpTableSP)                     ;$1018-$101b

  dc.l $00000000            ;Module work RAM                                        ;$101c-$101f
                            ;Total bytes of initialized & uninitialized data
jumpTableSP:
  dc.w initSP-jumpTableSP   ;Address of init subroutine
  dc.w mainSP-jumpTableSP   ;Address of main subroutine
  dc.w int2SP-jumpTableSP   ;Address of lvl. 2 interrupt (vblank)
  dc.w userRoutineSP-jumpTableSP ;User defined routine, not used in this demo
  dc.w $0000                ;End of jumptable
                            ;Without this, the program continues to read data until it hits $0000 or crashes

;----------------------------------------------------
;Initialize Sub CPU
;Called once on intial boot, as soon as it is loaded by the Sega CD. 
;----------------------------------------------------
initSP:
  andi.b #$fa, $ff8003      ;Set Word RAM to 2M Mode, set Return to 0 (Sub CPU has Word RAM access)

      ;$fa==%11111010
  ;$ff8003==%00000000
            ;|:|:|:|:
            ;|:|:|:|Return (R/W)
            ;|:|:|:Decleration/Designation of main RAM access (DMNA) (R)
            ;|:|:|RAM Mode (256 Kb|128 kb) (R/W)
            ;|:|Word RAM priority mode (R/W)
            ;xxx

  addq.l #1,$006300

  clr.b $ff800f             ;Clear status flag, for loopLoadSubCPUInit: to continue
  rts                       ;Return to BIOS (which will then call mainSP)

;----------------------------------------------------
;mainSP
;Main routine, this is the core of the SP Operating System
;WARNING: Does NOT check validitiy of input, invalid function calls WILL crash the CPU
;----------------------------------------------------
mainSP:
  tst.b $ff800e             ;Check command
  bne mainSP                ;If NOT clear, loop 
  move.b #1,$ff800f         ;Else, set status to ready
loop:

  addq.l #1,$006304

  tst.b $ff800e             ;Pretty much CMPI.B #0,$FF800E
  beq loop                  ;If the result is equal to 0, aka no command has been issued, loop

  moveq #0,d0               ;Clear D0
  move.b $ff800e,d0         ;Store command to D0
  lsl.w #2,d0               ;Shift D0 by 2, multiplying the value by 4
                            ;Using LSL.W because the range is effectively from $0000-$03fc, in increments of 4
  jsr opTableSP(pc,d0)      ;Execute function from jumptable
  move.b #0,$ff800f         ;Mark as done

  bra mainSP                ;Loop

;----------------------------------------------------
;opTableSP
;Jumptable of all functions provided by the SP Operating System
;These must be within the range of the bra.w, as each entry is limited to 4 bytes
;4 bytes = bra opcode (2 bytes) and a relative offset as a word.
;----------------------------------------------------    
opTableSP:
  bra.w opNullSP            ;Mainly here to pad opTableSP
  bra.w opLoadBootFileSP    ;Load File, directly
  bra.w opGetWordRAMSP      ;Give Word RAM to Main CPU

opNullSP:
  addq.l #1,$00630c
  rts
    
opLoadBootFileSP:
  movem.l d0-d7/a0-a6,-(a7) ;Backup registers to stack

  lea biosPacketSP(pc),a5   ;load the address of "biosPacketSP"

  move.l #$00000010,(a5)    ;Manually writing the start sector number
                            ;As oppposed to looking it up in a table.)

  move.l #$00000001,4(a5)   ;Number of sectors to copy (min. 1 AKA 800 Kb)

  move.l #$00080000,8(a5)   ;Destination address ($080000, AKA Word RAM)

  ;At this point, the contents of biosPacketSP are the sector number to start copying data from, total number of sectors to copy, and the address to copy the data to.

  movea.l a5,a0             ;Put packet to a0 (for BIOS)

  move.w #$0090,d0          ;#$0090: Stop the CD
  jsr $5f22                 ;Jump to CD BIOS subroutine to execute the command stored in D0

  move.w #$0020,d0          ;#$0020: Read requested number sectors and stop
  jsr $00005f22             ;Jump to CD BIOS

waitStatSP:
  move.w #$008a,d0          ;#$008a: Query the CDC buffer
  jsr $5f22
  bcs waitStatSP            ;If not ready, loop

waitReadSP:
  move.w #$008b,d0          ;#$008b: Prepare to send a sector of data to a predefined destination
  jsr $5f22
  bcc waitReadSP

waitTransferSP:
  movea.l 8(a5),a0          ;Get destination address
  lea 12(a5),a1             ;Get destination header address
  move.w #$008c,d0          ;#$008c: Use SubCPU to copy a sector into RAM
  jsr $5f22
  bcc  waitTransferSP
 
  move.w #$008d,d0          ;#$008d: Informing CDC the current sector has been read
                            ;Needs to be called after one frame of data has been read
  jsr $5f22

  addq.l #1,(a5)            ;Increment starting sector
  addi.l #$0800,8(a5)       ;Increment destination address
  subq.l #1,4(a5)           ;Decrement sectors left
  bne waitStatSP            ;If not finished, branch

  movem.l (a7)+,d0-d7/a0-a6 ;Restore registers from stack
  rts

opGetWordRAMSP:
  bset #0,$ff8003           ;Give Word RAM to Main CPU
  rts

;----------------------------------------------------
;Level 2 Interrupt routine for SubCPU 
;----------------------------------------------------
int2SP:
  addq.l #1,$006310
  rts

;----------------------------------------------------
;User Interrupt routine for SubCPU 
;----------------------------------------------------
userRoutineSP:
  addq.l #1,$006314
  rts

;----------------------------------------------------
;biosPacketSP
;An on-RAM container, of sorts, for BIOS calls
;----------------------------------------------------
biosPacketSP:
  ds.l 3                    ;Define Storage - Reserve 4 longwords here
                            ; - Start sector number
                            ; - Number of sectors to copy
                            ; - Destination address
                            ; - Destination header address (?)

  dc.l $00000200

  ;MEGA CD BIOS Manual defines that CDC Transfer ($008c) requires a destination address (A0), and a destination header address (A1) in order to copy data from the CDC data area to the destination.
  
  ;I have NO idea what the destination header address does -_-

mainSubCPUEnd:
  dcb.b $8000-*,0

;----------------------------------------------------
;GameData - Program 00
;Sector 10==$00008000-$000087FF
;----------------------------------------------------
  incbin "sega_cd_00_main_00.bin"

;----------------------------------------------------
;  Pad until end of data (just to be safe)
;----------------------------------------------------
  cnop $00,$00008800