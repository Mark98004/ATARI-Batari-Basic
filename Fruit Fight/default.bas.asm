; Provided under the CC0 license. See the included LICENSE.txt for details.

 processor 6502
 include "vcs.h"
 include "macro.h"
 include "DPCplus.h"
 include "DPCplusbB.h"
 include "2600basic_variable_redefs.h"
 ORG $400
 RORG $0
 incbin DPCplus.arm
     ORG $1000
     RORG $1000
 incbin custom/bin/custom2.bin
; assume custom2.bin > 128 bytes
; repeat $80
; .byte 0
; repend
; Provided under the CC0 license. See the included LICENSE.txt for details.

; feel free to modify the score graphics - just keep each digit 8 high
; and keep the conditional compilation stuff intact
 ifconst ROM2k
   ORG $F7AC-8
 else
   ifconst bankswitch
     if bankswitch == 8
       ORG $2F94-bscode_length
       RORG $FF94-bscode_length
     endif
     if bankswitch == 16
       ORG $4F94-bscode_length
       RORG $FF94-bscode_length
     endif
     if bankswitch == 32
       ORG $8F94-bscode_length
       RORG $FF94-bscode_length
     endif
     if bankswitch == 64
       ORG  $10F80-bscode_length
       RORG $1FF80-bscode_length
     endif
   else
     ORG $FF9C
   endif
 endif

; font equates
.21stcentury = 1
alarmclock = 2     
handwritten = 3    
interrupted = 4    
retroputer = 5    
whimsey = 6
tiny = 7
hex = 8

 ifconst font
   if font == hex
     ORG . - 48
   endif
 endif

scoretable

 ifconst font
  if font == .21stcentury
    include "score_graphics.asm.21stcentury"
  endif
  if font == alarmclock
    include "score_graphics.asm.alarmclock"
  endif
  if font == handwritten
    include "score_graphics.asm.handwritten"
  endif
  if font == interrupted
    include "score_graphics.asm.interrupted"
  endif
  if font == retroputer
    include "score_graphics.asm.retroputer"
  endif
  if font == whimsey
    include "score_graphics.asm.whimsey"
  endif
  if font == tiny
    include "score_graphics.asm.tiny"
  endif
  if font == hex
    include "score_graphics.asm.hex"
  endif
 else ; default font

       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %00111100

       .byte %01111110
       .byte %00011000
       .byte %00011000
       .byte %00011000
       .byte %00011000
       .byte %00111000
       .byte %00011000
       .byte %00001000

       .byte %01111110
       .byte %01100000
       .byte %01100000
       .byte %00111100
       .byte %00000110
       .byte %00000110
       .byte %01000110
       .byte %00111100

       .byte %00111100
       .byte %01000110
       .byte %00000110
       .byte %00000110
       .byte %00011100
       .byte %00000110
       .byte %01000110
       .byte %00111100

       .byte %00001100
       .byte %00001100
       .byte %01111110
       .byte %01001100
       .byte %01001100
       .byte %00101100
       .byte %00011100
       .byte %00001100

       .byte %00111100
       .byte %01000110
       .byte %00000110
       .byte %00000110
       .byte %00111100
       .byte %01100000
       .byte %01100000
       .byte %01111110

       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %01111100
       .byte %01100000
       .byte %01100010
       .byte %00111100

       .byte %00110000
       .byte %00110000
       .byte %00110000
       .byte %00011000
       .byte %00001100
       .byte %00000110
       .byte %01000010
       .byte %00111110

       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %00111100
       .byte %01100110
       .byte %01100110
       .byte %00111100

       .byte %00111100
       .byte %01000110
       .byte %00000110
       .byte %00111110
       .byte %01100110
       .byte %01100110
       .byte %01100110
       .byte %00111100 

       ifnconst DPC_kernel_options
 
         .byte %00000000
         .byte %00000000
         .byte %00000000
         .byte %00000000
         .byte %00000000
         .byte %00000000
         .byte %00000000
         .byte %00000000 

       endif

 endif

 ifconst ROM2k
   ORG $F7FC
 else
   ifconst bankswitch
     if bankswitch == 8
       ORG $2FF4-bscode_length
       RORG $FFF4-bscode_length
     endif
     if bankswitch == 16
       ORG $4FF4-bscode_length
       RORG $FFF4-bscode_length
     endif
     if bankswitch == 32
       ORG $8FF4-bscode_length
       RORG $FFF4-bscode_length
     endif
     if bankswitch == 64
       ORG  $10FE0-bscode_length
       RORG $1FFE0-bscode_length
     endif
   else
     ORG $FFFC
   endif
 endif
; Provided under the CC0 license. See the included LICENSE.txt for details.

start
 sei
 cld
 ldy #0
 lda $D0
 cmp #$2C               ;check RAM location #1
 bne MachineIs2600
 lda $D1
 cmp #$A9               ;check RAM location #2
 bne MachineIs2600
 dey
MachineIs2600
 ldx #0
 txa
clearmem
 inx
 txs
 pha
 bne clearmem
 sty temp1
 ldx #8
 stx playfieldpos
 stx FASTFETCH
 ldx #8
 lda #224
inityloop
 sta player1y,x
 dex
 bpl inityloop

 lda #1
 sta CTRLPF
 lda INTIM
 sta RWRITE0
 lda #0
 STA DF0FRACINC
 STA DF1FRACINC
 STA DF2FRACINC
 STA DF3FRACINC
 STA DF4FRACINC
 STA DF6FRACINC
 lda #<USERSTACK
 STA DF7LOW
 lda #(>USERSTACK) & $0F
 STA DF7HI
 lda #255
 sta CALLFUNCTION ; zero-fill fetcher

   lda #>(game-1)
   pha
   lda #<(game-1)
   pha
   pha
   pha
   ldx #1
   jmp BS_jsr
drawscreen
     lda #1
     sta CXCLR
     sta COLUBK ; REVENG - don't start with the lastline color

fufu
     lda INTIM
     bmi fufu

     VERTICAL_SYNC

     lda #41+128;was 37 - do more w/c code
     sta TIM64T

     ; adjust for pfpos?

     ; set zero to properly enter C code
     lda #<C_function
     sta DF0LOW
     lda #(>C_function) & $0F
     sta DF0HI
     lda #0
     sta DF0WRITE

     ; REVENG - pass the number of vsprites we want...
     ifnconst dpcspritemax
       ifconst readpaddle
          lda #8
       else
          lda #9
       endif
     else
       lda #dpcspritemax
     endif
     sta DF0WRITE 

     lda player0x
     sta player0xcoll ; detect p0x colls

     ; copy RAM to fetcher for C-code
     lda #<(CcodeData + RAMcopylength)
     sta DF0LOW
     lda #(>(CcodeData + RAMcopylength)) & $0F
     sta DF0HI
     ldx #RAMcopylength-1
copy2fetcherloop
     lda RAMcopybegin,x
     sta DF0PUSH
     dex
     bpl copy2fetcherloop

     lda #255
     sta CALLFUNCTION

     ; copy modified data back (just need first 6 bytes, which is sprite sort data)
     ldx #256-19
copyfromfetcherloop
     lda DF0DATA
     sta RAMcopybegin+19,x
     inx
     bmi copyfromfetcherloop

     jsr kernel_setup
     sta WSYNC
     ldy #$80
     sty HMP0
     sty HMP1
     sty HMM0 
     sty HMM1
     sty HMBL

     ; run possible vblank bB code
     ifconst vblank_bB_code
         jsr vblank_bB_code
     endif

     jsr set_fetchers

     ldx #7
setloopfrac
     lda dffraclow,x
     sta DF0FRACLOW,x
     lda dffrachi,x
     sta DF0FRACHI,x
     dex
     bpl setloopfrac
     ; lda #255
     STx DF5FRACINC ; x=255 right now
     STx DF7FRACINC
     lda DF5FRACDATA ; priming read
     lda DF7FRACDATA ; priming read

     ldx SpriteGfxIndex
     lda _NUSIZ1,x ; top NUSIZ/REFP
     sta NUSIZ1
     sta REFP1

     ;REVENG - allow P0 to wrap at the top
startwrapfix
     lda #255
     sta temp2
     clc
     lda player0y
     adc player0height
     sec
     cmp player0height
     bcc skipwrapfix
     lda #0
     sta temp2
skipwrapfix

     sec
     lda #<P0GFX
     sbc player0y
     sta DF2LOW
     lda #>P0GFX
     ;sbc #0
     sbc temp2
     sta DF2HI
     lda #<(P0GFX-1)
     sta DF2TOP
     sec
     adc player0height
     sta DF2BOT

     ;REVENG - 1/2 of the COLUM0 fix. the rest is in main.c
     lda #<(P0COLOR)
     sta DF0LOW
     sta temp2
     lda #>(P0COLOR)
     sta DF0HI

     ; ball
     lda #<(P1GFX-1)
     clc
     adc bally
     sta DF3TOP
     sec
     adc ballheight
     sta DF3BOT

     ; missile0
     lda temp2
     clc
     adc missile0y
     sta DF0TOP
     sec
     adc missile0height
     sta DF0BOT


fuu
     lda INTIM
     bmi fuu
     sta WSYNC
;     ldy #$80
;     sty HMP0
;     sty HMP1
;     sty HMM0 
;     sty HMM1
;     sty HMBL
; relocated code above prior to vblank, to allow for Cosmic Ark starfield
; and/or skewed players
 sleep 17 

     lda #KERNEL_LINES
     sta TIM64T
     lda #1
     sta VDELBL
     sta VDELP0

     ; missile1
     lda #<(P1COLOR-1)
     clc
     adc missile1y
     sta DF1TOP
     sec
     adc missile1height
     sta DF1BOT

     lda #0
     sta VBLANK
     sta FASTFETCH
     ;sleep 7
     lda #<DF2DATAW         ; REVENG - added so GRP0 is at TOP
     STA GRP0 ; 36 (VDEL)   ; ""
     sleep 2                ; ""

     lda #<DF0FRACDATA
     sta PF1 ; (PF1L)

     ; enter at cycle ??
loop:
     lda #<DF0DATA ;74
     STA COLUP0 ; 1
     lda #<DF1DATA ;3
loop2
     STA COLUP1 ; 6
     lda #<DF3DATA
     STA GRP1 ; 11
     lda #<DF0FLAG
     STA ENAM0 ; 16

     lda #<DF6FRACDATA
     sta COLUBK ; 21
     lda #<DF4FRACDATA
     sta COLUPF ; 26
     lda #<DF1FRACDATA
     sta PF2 ; 31 (PF2L)
loop3
     lda #<DF2DATAW
     STA GRP0 ; 36 (VDEL)
     lda #<DF3FLAG
     STA ENABL ; 41 (VDEL)
     ldx #$70 ;in case we get kernel 6
     lda #<DF2FRACDATA ;45
     sta PF2 ; 48
     sty HMP1 ; 51 ; from prev. cycle: $80=nomove
     lda #<DF3FRACDATA ;53
     sta PF1 ; 56
     lda #<DF4DATA ; 58 this is the repos info
     beq repo ;60/61
norepo     ; 60
     tay ; 62
     lda #<DF0DATA ; 64

     ldx INTIM ; 68 timed for 192 lines
     beq exitkernel; 70/71
     sta HMOVE ; 73

     STA COLUP0 ; 0
     lda #<DF1DATA ;2
     STA COLUP1 ;5
     lda #<DF3DATA
     STA GRP1 ; 10
     lda #<DF1FLAG
     STA ENAM1 ; 15
     lda #<DF0FRACDATA
     sta PF1 ; 20 (PF1L)
     lda #<DF1FRACDATA
     sta PF2 ; 25 (PF2L)
     lda #<DF2DATAW
     STA GRP0 ; 30 (VDEL)
     lda #<DF3FLAG
     STA ENABL ; 35 (VDEL)
     dey ; 37
     STY DF4PUSH ; 41
     ldy #$80 ; 43 no movement next line
     lda #<DF2FRACDATA ;45
     sta PF2 ; 48
     sty HMP1 ; 51 ; from prev. cycle: $80=nomove
     lda #<DF3FRACDATA ;53
     sta PF1 ; 56
     ifnconst DPC_kernel_options
         ;sleep 8 ; REVENG - timing is off - results in a garbled screen
         sleep 5 ; this is better
     else
         bit DPC_kernel_options
         if (DPC_kernel_options > $3F)
             bmi COLfound
         else
             bpl COLfound
         endif
     endif
     stx temp4 ; +3

getbackearly
     lda #<DF0FRACDATA ; +2
     sta PF1 ; 69 (PF1L) too early?
     JMP loop+$4000 ; 72

     ifconst DPC_kernel_options
COLfound
         lda DF0FRACDATA
         sta PF1 ; 69 (PF1L) too early?
         JMP loop+$4000 ; 72
     endif

repo     
     ldy DF7FRACDATA ; 65
     lda #<DF0FRACDATA ; 67 preload PF1L for next line
     if ((>repo) > (>norepo))
         STA PF1
     else
         STA.w PF1 ; 71 ; sta.w if page doesn't wrap
     endif
     lda #<DF0DATA ;73
     STA COLUP0 ; 0
     lda #<DF1DATA 
     STA COLUP1 ;5
     lda #<DF3DATA
     STA GRP1 ; 10
     lda #<DF1FLAG
     STA ENAM1 ; 15
     ; repos info holds HMMx
     jmp (DF5DATA) ; 20 grabs df6/df7=lo/hi

exitkernel     ; exit the kernel
     jsr scorekernel+$4000 ; 1
exit
     ldx #255
     stx FASTFETCH
     sta WSYNC
     ifconst qtcontroller
        lda qtcontroller
        lsr    ; bit 0 in carry
        lda #4
        ror    ; carry into top of A
     else
        lda #2
     endif ; qtcontroller
     STA VBLANK
     lda #OVERSCAN_LINES
     sta TIM64T
     sec
     lda #KERNEL_LINES
     sbc temp4
     tax
     lsr
     lsr 
     sta temp3 ; div4
     lsr
     lsr
     sta temp2 ; div16
     lsr
     sta temp1 ; div32
     clc
     txa
     adc temp2
     adc temp1
     sec
     sbc temp3
     sta temp4 ; approx line of first pf coll
     RETURN

     ; jmp exit

     ; kernels resp1 23/28/33/38/43/48/53/58/63/68/73

kernel1
     sta RESP1 ; 23
     lda #<DF2DATAW
     STA GRP0 ; 28 (VDEL)
     lda #<DF1FRACDATA
     STA PF2 ; 33
     lda #<DF3FLAG
     STA ENABL ; 38 (VDEL)
     sleep 5
     lda #<DF2FRACDATA ;45
     sta PF2 ; 48
     lda #<DF3FRACDATA ;50
     sta PF1 ; 53
     lda #<DF5FRACDATA ; NUSIZ/RESP info (OK here, GRP1 off)
     STA NUSIZ1 ; 58
     STA REFP1 ; 61
     jmp getbackearly ;64

kernel2
     lda #<DF2DATAW
     STA GRP0 ; 25 (VDEL)
     sta RESP1 ;28
     lda #<DF1FRACDATA
     STA PF2 ; 33
     lda #<DF3FLAG
     STA ENABL ; 38 (VDEL)
     sleep 5
     lda #<DF2FRACDATA;45
     sta PF2 ; 48
     lda #<DF3FRACDATA;50
     sta PF1 ; 53
     lda #<DF5FRACDATA ; NUSIZ/RESP info (OK here, GRP1 off)
     STA NUSIZ1
     STA REFP1
     jmp getbackearly ;64

kernel3
     lda #<DF2DATAW
     STA GRP0 ; 25 (VDEL)
     lda #<DF1FRACDATA
     STA PF2 ; 30
     sta RESP1 ;33
     lda #<DF3FLAG
     STA ENABL ; 38 (VDEL)
     sleep 5
     lda #<DF2FRACDATA;45
     sta PF2 ; 48
     lda #<DF3FRACDATA;50
     sta PF1 ; 53
     lda #<DF5FRACDATA ; NUSIZ/RESP info (OK here, GRP1 off)
     STA NUSIZ1
     STA REFP1
     JMP getbackearly ; 64

kernel4
     lda #<DF2DATAW
     STA GRP0 ; 25 (VDEL)
     lda #<DF3FLAG
     STA ENABL ; 30(VDEL)
     lda #<DF1FRACDATA
     STA PF2 ; 35
     sta RESP1 ;38
     sleep 5
     lda #<DF2FRACDATA;45
     sta PF2 ; 48
     lda #<DF3FRACDATA;50
     sta PF1 ; 53
     lda #<DF5FRACDATA ; NUSIZ/RESP info (OK here, GRP1 off)
     STA NUSIZ1 ; 58
     STA REFP1 ; 61
     JMP getbackearly ; 64

kernel5
     lda #<DF2DATAW
     STA GRP0 ; (VDEL)
     lda #<DF3FLAG
     STA ENABL ; (VDEL)
     lda #<DF1FRACDATA
     STA PF2 ; 35
     sleep 5
     sta RESP1 ;43
     lda #<DF2FRACDATA;45
     sta PF2 ; 48
     lda #<DF3FRACDATA;50
     sta PF1 ; 53
     lda #<DF5FRACDATA ; NUSIZ/RESP info (OK here, GRP1 off)
     STA NUSIZ1
     STA REFP1
     JMP getbackearly ; 64

kernel6
     lda #<DF2DATAW
     STA GRP0 ; 25 (VDEL)
     lda #<DF3FLAG
     STA ENABL ; 30 (VDEL)
     lda #<DF1FRACDATA
     STA PF2 ; 35
     lda #<DF5FRACDATA ; 37 NUSIZ/RESP info (OK here, GRP1 off)
     STA NUSIZ1 ; 40
     STA REFP1 ; 43
     lda #<DF2FRACDATA;45
     sta PF2 ; 48
     lda #<DF3FRACDATA;50
     sta RESP1 ;53
     ; do a move right by 15
     sta PF1 ; 56
     stx HMP1 ; 59
     lda #<DF1FRACDATA
     sta PF2 ; 64 (PF2L)
     lda #<DF0FRACDATA
     sta PF1 ; 69 (PF1L) too early?
     lda #<DF0DATA ; 71
     sta HMOVE ; 74 adjust to +15 right

     STA COLUP0 ; 1
     lda #<DF1DATA
     sta COLUP1 ; 6
     lda #<DF3DATA
     STA GRP1 ; 11
     lda #<DF0FLAG
     STA ENAM0 ; 16
     lda #<DF6FRACDATA
     STA COLUBK ; 21
     lda #<DF4FRACDATA
     sta COLUPF ; 26
     sleep 2
     jmp loop3 ; 31

kernel7
     lda #<DF2DATAW
     STA GRP0 ; 25 (VDEL)
     lda #<DF3FLAG
     STA ENABL ; 30 (VDEL)
     lda #<DF1FRACDATA
     STA PF2 ; 35
     lda #<DF5FRACDATA ; 37 NUSIZ/RESP info (OK here, GRP1 off)
     STA NUSIZ1 ; 40
     STA REFP1 ; 43
     lda #<DF2FRACDATA;45
     sta PF2 ; 48
     sleep 2
     sta RESP1 ;53
     lda #<DF3FRACDATA;55
     sta PF1 ; 58
     sleep 3
     JMP getbackearly ; 64

kernel8
     lda #<DF2DATAW
     STA GRP0 ; (VDEL)
     lda #<DF3FLAG
     STA ENABL ; (VDEL)
     lda #<DF1FRACDATA
     STA PF2 ; 35
     lda #<DF5FRACDATA ; 37 NUSIZ/RESP info (OK here, GRP1 off)
     STA NUSIZ1 ; 40
     STA REFP1 ; 43
     lda #<DF2FRACDATA;45
     sta PF2 ; 48
     lda #<DF3FRACDATA;50
     sta PF1 ; 53
     sleep 2
     sta RESP1 ;58
     sleep 3
     JMP getbackearly ; 64

kernel9
     lda #<DF2DATAW
     STA GRP0 ; (VDEL)
     lda #<DF3FLAG
     STA ENABL ; (VDEL)
     lda #<DF1FRACDATA
     STA PF2 ; 35
     lda #<DF5FRACDATA ; 37 NUSIZ/RESP info (OK here, GRP1 off)
     STA NUSIZ1 ; 40
     STA REFP1 ; 43
     lda #<DF2FRACDATA;45
     sta PF2 ; 48
     lda #<DF3FRACDATA;50
     sta PF1 ; 53
     sleep 5
     lda #<DF0FRACDATA
     sta RESP1 ;63
     sleep 3
     sta PF1 ; 69 (PF1L) too early?
     jmp loop ;72

kernel10
     lda #<DF2DATAW
     STA GRP0 ; 25 (VDEL)
     lda #<DF3FLAG
     STA ENABL ; 30 (VDEL)
     lda #<DF1FRACDATA
     STA PF2 ; 35
     lda #<DF5FRACDATA ; 37 NUSIZ/RESP info (OK here, GRP1 off)
     STA NUSIZ1 ; 40
     STA REFP1 ; 43
     lda #<DF2FRACDATA;45
     sta PF2 ; 48
     lda #<DF3FRACDATA;50
     sta PF1 ; 53
     sleep 6
     lda #<DF0FRACDATA
     LDX DF0DATA ; 65
     sta RESP1 ; 68
     STA PF1 ; 71
     lda #<DF1DATA ; 74
     STX COLUP0 ; 0
     jmp loop2 ; 3

kernel11
     lda #<DF2DATAW
     STA GRP0 ; (VDEL)
     lda #<DF3FLAG
     STA ENABL ; (VDEL)
     lda #<DF1FRACDATA
     STA PF2 ; 35
     lda #<DF5FRACDATA ; NUSIZ/RESP info (OK here, GRP1 off)
     STA NUSIZ1
     STA REFP1
     lda #<DF2FRACDATA;45
     sta PF2 ; 48
     lda #<DF3FRACDATA;50
     sta PF1 ; 53
     sleep 3
     lda #<DF1FRACDATA;45
     sta PF2 ; 61
     LDX DF0DATA ; 65

     lda #<DF0FRACDATA ; 67
     sta PF1 ; 70
     sta RESP1 ; 73
     STX COLUP0 ; 0
     lda #<DF1DATA ; 2
     sta COLUP1 ; 5
     lda #<DF3DATA
     STA GRP1 ; 10
     lda #<DF0FLAG
     STA ENAM0 ; 25
     lda #<DF6FRACDATA
     STA COLUBK ; 20
     lda #<DF4FRACDATA
     sta COLUPF ; 25
     sleep 3
     jmp loop3 ; 31

set_fetchers
     lda dflow
     sta DF0LOW
     lda dfhigh
     sta DF0HI

     lda dflow+1
     sta DF1LOW
     lda dfhigh+1
     sta DF1HI

     lda dflow+2
     sta DF2LOW
     lda dfhigh+2
     sta DF2HI

set_fetchers36 ; sets just 3-6
     lda dflow+3
     sta DF3LOW
     lda dfhigh+3
     sta DF3HI

     lda dflow+4
     sta DF4LOW
     lda dfhigh+4
     sta DF4HI

     lda dflow+5
     sta DF5LOW
     lda dfhigh+5
     sta DF5HI

     lda dflow+6
     sta DF6LOW
     lda dfhigh+6
     sta DF6HI

     rts

     ;9d bad
     ; the below isn't quite right
     ;DF0DATA: COLUP0
     ;DF1DATA: COLUP1
     ;DF2DATAW: GRP0
     ;DF3DATA: GRP1 
     ;DF4DATA: 2lk lines until repos/HMP1
     ;DF5DATA: low byte of repo kernels (xpos mod 15)
     ;DF6DATA: High byte of repo kernels (x pos div 15)
     ;DF7DATA: Programmer's stack
     ;DF0FRACDATA: PF1L
     ;DF1FRACDATA: PF2L
     ;DF4FRACDATA: COLUPF
     ;DF2FRACDATA: PF2R
     ;DF3FRACDATA: PF2L
     ;DF5FRACDATA: Sprite NUSIZ1/REFP1 (only during repos)
     ;DF6FRACDATA: COLUBK
     ;DF7FRACDATA: HMP1
     ;DF3FLAG: kernel exit loop ?? (use flags instead?)
     ;DF0FLAG: ENAM0
     ;DF1FLAG: ENAM1 
     ;DF3FLAG: ENABL 

fetcher_address_table
kernello
     .byte <kernel1
     .byte <kernel2
     .byte <kernel3
     .byte <kernel4
     .byte <kernel5
     .byte <kernel6
     .byte <kernel7
     .byte <kernel8
     .byte <kernel9
     .byte <kernel10
     .byte <kernel11
kernelhi
     .byte >kernel1
     .byte >kernel2
     .byte >kernel3
     .byte >kernel4
     .byte >kernel5
     .byte >kernel6
     .byte >kernel7
     .byte >kernel8
     .byte >kernel9
     .byte >kernel10
     .byte >kernel11
dflow     
     .byte <P0COLOR
     .byte <P1COLOR
     .byte <P0GFX
     .byte <P1GFX
     .byte <P1SKIP
     .byte <JUMPTABLELO
     .byte <JUMPTABLEHI
     .byte <USERSTACK
dfhigh
     .byte (>P0COLOR) & $0F
     .byte (>P1COLOR) & $0F
     .byte (>P0GFX) & $0F
     .byte (>P1GFX) & $0F
     .byte (>P1SKIP) & $0F
     .byte (>JUMPTABLELO) & $0F
     .byte (>JUMPTABLEHI) & $0F
     .byte (>USERSTACK) & $0F
dffraclow
     .byte <PF1L
     .byte <PF2L
     .byte <PF1R
     .byte <PF2R
     .byte <PFCOLS
     .byte <NUSIZREFP
     .byte <BKCOLS
     .byte <P1HMP
dffrachi
     .byte (>PF1L) & $0F
     .byte (>PF2L) & $0F
     .byte (>PF1R) & $0F
     .byte (>PF2R) & $0F
     .byte (>PFCOLS) & $0F
     .byte (>NUSIZREFP) & $0F 
     .byte (>BKCOLS) & $0F
     .byte (>P1HMP) & $0F
scorepointer
     .byte <scoretable
     .byte ((>scoretable) & $0f) | (((>scoretable) / 2) & $70)
scoresetup     ; pointers to digit graphics
     .byte <scoredata
     .byte (>scoredata) & $0F
Hmval; 112 wuz first
     .byte 96, 80, 64, 48, 32, 16, 1, 240
Hmval74
     .byte 224, 208, 192, 176, 160, 144, 128
     .byte 96, 80, 64, 48, 32, 16, 1, 240, 224, 208, 192, 176, 160, 144, 128, 96
     .byte 80, 64, 48, 32, 16, 1, 240, 224, 208, 192, 176, 160, 144, 128, 96, 80
     .byte 64, 48, 32, 16, 1, 240, 224, 208, 192, 176, 160, 144, 128, 96, 80, 64
     .byte 48, 32, 16, 1, 240, 224, 208, 192, 176, 160, 144, 128, 96, 80, 64, 48
     .byte 32, 16, 1, 240, 224, 208, 192, 176, 160, 144, 128, 96, 80, 64, 48, 32
     .byte 16, 1, 240, 224, 208, 192, 176, 160, 144, 128, 96, 80, 64, 48, 32, 16
     .byte 1, 240, 224, 208, 192, 176, 160, 144, 128, 96, 80, 64, 48, 32, 16, 1
     .byte 240, 224, 208, 192, 176, 160, 144, 128, 96, 80, 64, 48, 32, 16, 1, 240
     .byte 224, 208, 192, 176, 160, 144, 128, 96, 80, 64, 48, 32, 16, 1, 240, 224, 208, 192
     .byte 176,160,144,128,16,1,240,224
     

kernel_setup
     ;--position P0, top P1, M0, M1, BL
     ldx #0 ; first sprite displayed
     lda SpriteGfxIndex,x
     tax
     lda player1x,x
     cmp #160
     bcc nostorep1
     cmp #208
     bcs ksadjustdown
     ; 160-208: minus 160
     ;add 160 is like minus 96
     ; so minus 64
     sbc #63 ;cc
ksadjustdown
     ; 209-255: add 160 
     adc #159 ; cs
     sta player1x,x
nostorep1
     sta WSYNC
     ldx #4
     sta topP1x ; cache top p1
HorPosLoop
     lda player0x,X
     sec
DivideLoop
     sbc #15
     bcs DivideLoop
     sleep 4
     sta RESP0,X
     sta WSYNC
     dex ;2
     bpl HorPosLoop ;4/5

     ldy player0x ; 7
     lda Hmval,y ; 11
     sta HMP0 ; 14

     ldy player0x+1 
     lda Hmval,y
     sta HMP0+1 ; 24

     ldy player0x+2 
     lda Hmval,y
     sta HMP0+2 ; 34

     ldy player0x+3
     lda Hmval,y
     sta HMP0+3 ; 44

     ldy player0x+4 
     lda Hmval,y
     sta HMP0+4 ; 54

     sta WSYNC
     sta HMOVE

myrts
     rts


pfsetup     
     
     sty temp1 
     sta temp2
     stx temp3
     ldx #3
pfsetupp
     lda dffraclow,x
     sta DF0LOW,x
     lda dffrachi,x
     sta DF0HI,x 
     lda temp2
     sta PARAMETER
     lda temp3
     sta PARAMETER
     stx PARAMETER
     sty PARAMETER 
     LDA #1
     sta CALLFUNCTION
     clc
     lda temp2
     adc temp1
     sta temp2
     lda temp3
     adc #0
     sta temp3
     dex
     bpl pfsetupp
     RETURN


scorekernel
     ifconst minikernel
         ;; disable fast fetch, call the minikernel, and re-enable fast fetch
         lda #255
         sta FASTFETCH
         jsr minikernel
         lda #0
         sta.w FASTFETCH
     endif
     ldx scorecolor
     stx COLUP0
     stx COLUP1
     ldx #0
     STx PF1
     stx REFP0
     stx REFP1
     STx GRP0
     STx GRP1
     STx PF2
     stx HMCLR
     stx ENAM0
     stx ENAM1
     stx ENABL


     ifconst pfscore
         lda pfscorecolor
         sta COLUPF
     endif

     ifconst noscore
         ldx #10
noscoreloop
         sta WSYNC
         dex
         bpl noscoreloop
         rts
     else

     sta HMCLR
     ldx #$f0
     stx HMP0

     ; set up fetchers 0-5 to handle score digits
     ldx #<(scoredata)
     stx DF6LOW
     ldx #(>(scoredata)) & $0F
     stx DF6HI
     ldx #<(scoredata+8)
     stx DF0LOW
     ldx #(>(scoredata+8)) & $0F
     stx DF0HI
     ldx #<(scoredata+16)
     stx DF1LOW
     ; cycle 0??
     ldx #(>(scoredata+16)) & $0F
     stx DF1HI
     ldx #<(scoredata+24)
     stx DF2LOW
     ldx #(>(scoredata+24)) & $0F
     stx DF2HI

     sta WSYNC
     ldx #0
     STx GRP0
     STx GRP1 ; seems to be needed because of vdel

     ldx #<(scoredata+32)
     stx DF3LOW
     ldx #(>(scoredata+32)) & $0F
     stx DF3HI
     ldx #<(scoredata+40)
     stx DF4LOW
     ldx #(>(scoredata+40)) & $0F
     stx DF4HI

     LDY #7
     LDx #$03
     STY VDELP0
     STA RESP0
     STA RESP1
     sty temp1

     STx NUSIZ0
     STx NUSIZ1
     STx VDELP1
     ldx #<(scoredata+48)
     stx DF5LOW
     ldx #(>(scoredata+48)) & $0F
     stx DF5HI
     STA.w HMOVE ; cycle 73 ?
scoreloop
     lda #<DF6DATA ;59
     sta COLUP0 ;62
     sta COLUP1 ;65
     lda #<DF1DATA;75
     sta GRP0 ;2
     lda #<DF0DATA ;4
     sta GRP1 ;7
     lda #<DF3DATA ;9
     sta GRP0 ;12

     ; REVENG - rearranged to correct pf write timing and A register overwrite
     ifconst pfscore
         lda pfscore1
         sta PF1
     else
         sleep 6
     endif
     sleep 5 
     ldx DF2DATA;16
     ldy DF5DATA;20
     lda #<DF4DATA;22 

     stx GRP1;40
     sty GRP0;43
     sta GRP1;46
     sta GRP0;49
     ifconst pfscore
         lda pfscore2
         sta PF1
     else
         sleep 6
     endif
     ; sleep 2 ;57
     sleep 6
     dec temp1;70
     bpl scoreloop;72/73
     LDx #0 
     stx PF1
     STx GRP0
     STx GRP1
     STx VDELP0
     STx VDELP1;do we need these
     STx NUSIZ0
     STx NUSIZ1

     rts

     
     endif ; noscore
game
.L00 ;  set kernel DPC + 

.L01 ;  set tv ntsc

.L02 ;  goto _Bank_2 bank2

 sta temp7
 lda #>(._Bank_2-1)
 pha
 lda #<(._Bank_2-1)
 pha
 lda temp7
 pha
 txa
 pha
 ldx #2
 jmp BS_jsr
.L03 ;  bank 2

 if ECHO1
 echo "    ",[(start_bank1 - *)]d , "bytes of ROM space left in bank 1")
 endif
ECHO1 = 1
 ORG $1FF4-bscode_length
 RORG $1FF4-bscode_length
start_bank1 ldx #$ff
 ifconst FASTFETCH ; using DPC+
 stx FASTFETCH
 endif 
 txs
 if bankswitch == 64
   lda #(((>(start-1)) & $0F) | $F0)
 else
   lda #>(start-1)
 endif
 pha
 lda #<(start-1)
 pha
 pha
 txa
 pha
 tsx
 if bankswitch != 64
   lda 4,x ; get high byte of return address
   rol
   rol
   rol
   rol
   and #bs_mask ;1 3 or 7 for F8/F6/F4
   tax
   inx
 else
   lda 4,x ; get high byte of return address
   tay
   ora #$10 ; change our bank nibble into a valid rom mirror
   sta 4,x
   tya
   lsr 
   lsr 
   lsr 
   lsr 
   tax
   inx
 endif
 lda bankswitch_hotspot-1,x
 pla
 tax
 pla
 rts
 if ((* & $1FFF) > ((bankswitch_hotspot & $1FFF) - 1))
   echo "WARNING: size parameter in banksw.asm too small - the program probably will not work."
   echo "Change to",[(*-begin_bscode+1)&$FF]d,"and try again."
 endif
 ORG $1FFC
 RORG $1FFC
 .word (start_bank1 & $ffff)
 .word (start_bank1 & $ffff)
 ORG $2000
 RORG $3000
HMdiv
  .byte 0, 0, 0, 0, 0, 0, 0
  .byte 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2
  .byte 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3
  .byte 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4
  .byte 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5
  .byte 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6
  .byte 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7
  .byte 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8
  .byte 8, 8, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9
  .byte 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10
  .byte 10,10,10,10,10,10,0,0,0
.L04 ;  temp1 = temp1

	LDA temp1
	STA temp1
._Bank_2
 ; _Bank_2

.
 ; 

.L05 ;  dim musicPointer = a

.L06 ;  dim musicTimer = b

.L07 ;  AUDV0 = 0

	LDA #0
	STA AUDV0
.L08 ;  AUDC0 = 4

	LDA #4
	STA AUDC0
.L09 ;  musicPointer = 0

	LDA #0
	STA musicPointer
.L010 ;  musicTimer = 0

	LDA #0
	STA musicTimer
.
 ; 

.
 ; 

.L011 ;  DF0FRACINC  =  64

	LDA #64
	STA DF0FRACINC
.L012 ;  DF1FRACINC  =  64

	LDA #64
	STA DF1FRACINC
.L013 ;  DF2FRACINC  =  64

	LDA #64
	STA DF2FRACINC
.L014 ;  DF3FRACINC  =  64

	LDA #64
	STA DF3FRACINC
.L015 ;  DF4FRACINC  =  128

	LDA #128
	STA DF4FRACINC
.L016 ;  DF6FRACINC  =  128

	LDA #128
	STA DF6FRACINC
.L017 ;  bkcolors:

	LDA #<BKCOLS
	STA DF0LOW
	LDA #(>BKCOLS) & $0F
	STA DF0HI
	LDA #<backgroundcolorL017
	STA PARAMETER
	LDA #((>backgroundcolorL017) & $0f) | (((>backgroundcolorL017) / 2) & $70)
	STA PARAMETER
	LDA #0
	STA PARAMETER
	LDA #44
	STA PARAMETER
	LDA #1
	STA CALLFUNCTION
.L018 ;  pfcolors:

	LDA #<PFCOLS
	STA DF0LOW
	LDA #(>PFCOLS) & $0F
	STA DF0HI
	LDA #<playfieldcolorL018
	STA PARAMETER
	LDA #((>playfieldcolorL018) & $0f) | (((>playfieldcolorL018) / 2) & $70)
	STA PARAMETER
	LDA #0
	STA PARAMETER
	LDA #44
	STA PARAMETER
	LDA #1
	STA CALLFUNCTION
.L019 ;  playfield:

 ldy #44
	LDA #<PF_data1
	LDX #((>PF_data1) & $0f) | (((>PF_data1) / 2) & $70)
 sta temp7
 lda #>(ret_point1-1)
 pha
 lda #<(ret_point1-1)
 pha
 lda #>(pfsetup-1)
 pha
 lda #<(pfsetup-1)
 pha
 lda temp7
 pha
 txa
 pha
 ldx #1
 jmp BS_jsr
ret_point1
.
 ; 

.L020 ;  scorecolors:

	lda #<scoredata
	STA DF0LOW
	lda #((>scoredata) & $0f)
	STA DF0HI
	lda #$44

	sta DF0WRITE
	lda #$44

	sta DF0WRITE
	lda #$44

	sta DF0WRITE
	lda #$44

	sta DF0WRITE
	lda #$44

	sta DF0WRITE
	lda #$44

	sta DF0WRITE
	lda #$44

	sta DF0WRITE
	lda #$44

	sta DF0WRITE
.
 ; 

.L021 ;  player1color:

	lda #<(playerpointers+18)
	sta DF0LOW
	lda #(>(playerpointers+18)) & $0F
	sta DF0HI
	LDX #<playercolorL021_1
	STX DF0WRITE
	LDA #((>playercolorL021_1) & $0f) | (((>playercolorL021_1) / 2) & $70)
	STA DF0WRITE
.L022 ;  player1:

	lda #<(playerpointers+0)
	sta DF0LOW
	lda #(>(playerpointers+0)) & $0F
	sta DF0HI
	LDX #<playerL022_1
	STX DF0WRITE
	LDA #((>playerL022_1) & $0f) | (((>playerL022_1) / 2) & $70)
	STA DF0WRITE
	LDA #8
	STA player1height
.L023 ;  player1x = rand / 2 : player1y = 70

        lda rand
	lsr
	STA player1x
	LDA #70
	STA player1y
.
 ; 

.L024 ;  player2color:

	lda #<(playerpointers+20)
	sta DF0LOW
	lda #(>(playerpointers+20)) & $0F
	sta DF0HI
	LDX #<playercolorL024_2
	STX DF0WRITE
	LDA #((>playercolorL024_2) & $0f) | (((>playercolorL024_2) / 2) & $70)
	STA DF0WRITE
.L025 ;  player2:

	lda #<(playerpointers+2)
	sta DF0LOW
	lda #(>(playerpointers+2)) & $0F
	sta DF0HI
	LDX #<playerL025_2
	STX DF0WRITE
	LDA #((>playerL025_2) & $0f) | (((>playerL025_2) / 2) & $70)
	STA DF0WRITE
	LDA #8
	STA player2height
.L026 ;  player2x = rand / 2 : player2y = 50

        lda rand
	lsr
	STA player2x
	LDA #50
	STA player2y
.
 ; 

.L027 ;  player3color:

	lda #<(playerpointers+22)
	sta DF0LOW
	lda #(>(playerpointers+22)) & $0F
	sta DF0HI
	LDX #<playercolorL027_3
	STX DF0WRITE
	LDA #((>playercolorL027_3) & $0f) | (((>playercolorL027_3) / 2) & $70)
	STA DF0WRITE
.L028 ;  player3:

	lda #<(playerpointers+4)
	sta DF0LOW
	lda #(>(playerpointers+4)) & $0F
	sta DF0HI
	LDX #<playerL028_3
	STX DF0WRITE
	LDA #((>playerL028_3) & $0f) | (((>playerL028_3) / 2) & $70)
	STA DF0WRITE
	LDA #8
	STA player3height
.L029 ;  player3x = rand / 2 : player3y = 30

        lda rand
	lsr
	STA player3x
	LDA #30
	STA player3y
.
 ; 

.L030 ;  player0x = 80 : player0y = 145

	LDA #80
	STA player0x
	LDA #145
	STA player0y
.
 ; 

.mainLoop
 ; mainLoop

.
 ; 

.L031 ;  if musicTimer  =  0 then gosub changeMusicNote

	LDA musicTimer
	CMP #0
     BNE .skipL031
.condpart0
 jsr .changeMusicNote

.skipL031
.L032 ;  musicTimer  =  musicTimer  -  1

	DEC musicTimer
.
 ; 

.L033 ;  player1y = player1y + 1

	INC player1y
.L034 ;  player2y = player2y + 1

	INC player2y
.L035 ;  player3y = player3y + 1

	INC player3y
.L036 ;  player3x = player0x

	LDA player0x
	STA player3x
.L037 ;  if player1x  < 20  ||  player1x  >  130 then player1x = 100

	LDA player1x
	CMP #20
     BCS .skipL037
.condpart1
 jmp .condpart2
.skipL037
	LDA #130
	CMP player1x
     BCS .skip0OR
.condpart2
	LDA #100
	STA player1x
.skip0OR
.L038 ;  if player1y > 240 then player1x = rand / 2 : player1y = 80

	LDA #240
	CMP player1y
     BCS .skipL038
.condpart3
        lda rand
	lsr
	STA player1x
	LDA #80
	STA player1y
.skipL038
.L039 ;  if player2x  < 20  ||  player2x  >  130 then player2x = 90

	LDA player2x
	CMP #20
     BCS .skipL039
.condpart4
 jmp .condpart5
.skipL039
	LDA #130
	CMP player2x
     BCS .skip1OR
.condpart5
	LDA #90
	STA player2x
.skip1OR
.L040 ;  if player2y > 240 then player2x = rand / 2 : player2y = 60

	LDA #240
	CMP player2y
     BCS .skipL040
.condpart6
        lda rand
	lsr
	STA player2x
	LDA #60
	STA player2y
.skipL040
.L041 ;  if player3x  < 20  ||  player3x  >  130 then player3x = 30

	LDA player3x
	CMP #20
     BCS .skipL041
.condpart7
 jmp .condpart8
.skipL041
	LDA #130
	CMP player3x
     BCS .skip2OR
.condpart8
	LDA #30
	STA player3x
.skip2OR
.L042 ;  if player3y > 240 then player3x = rand / 2 : player3y = 30

	LDA #240
	CMP player3y
     BCS .skipL042
.condpart9
        lda rand
	lsr
	STA player3x
	LDA #30
	STA player3y
.skipL042
.L043 ;  player0color:

	LDX #<playercolorL043_0
	STX player0color
	LDA #((>playercolorL043_0) & $0f) | (((>playercolorL043_0) / 2) & $70)
	STA player0color+1
.L044 ;  player0:

	LDX #<playerL044_0
	STX player0pointerlo
	LDA #((>playerL044_0) & $0f) | (((>playerL044_0) / 2) & $70)
	STA player0pointerhi
	LDA #8
	STA player0height
.L045 ;  drawscreen

 sta temp7
 lda #>(ret_point2-1)
 pha
 lda #<(ret_point2-1)
 pha
 lda #>(drawscreen-1)
 pha
 lda #<(drawscreen-1)
 pha
 lda temp7
 pha
 txa
 pha
 ldx #1
 jmp BS_jsr
ret_point2
.L046 ;  if joy0right then player0x = player0x + 2

 bit SWCHA
	BMI .skipL046
.condpart10
	LDA player0x
	CLC
	ADC #2
	STA player0x
.skipL046
.L047 ;  if joy0left then player0x = player0x - 2

 bit SWCHA
	BVS .skipL047
.condpart11
	LDA player0x
	SEC
	SBC #2
	STA player0x
.skipL047
.L048 ;  if collision(player0,player1) then score = score + 1 : player1x = rand / 2 : player1y = 80

	bit 	CXPPMM
	BPL .skipL048
.condpart12
	SED
	CLC
	LDA score+2
	ADC #$01
	STA score+2
	LDA score+1
	ADC #$00
	STA score+1
	LDA score
	ADC #$00
	STA score
	CLD
        lda rand
	lsr
	STA player1x
	LDA #80
	STA player1y
.skipL048
.L049 ;  if collision(player0,player2) then score = score + 2 : player2x = rand / 2 : player2y = 60

	lda #<C_function
	sta DF0LOW
	lda #(>C_function) & $0F
	sta DF0HI
  lda #20
  sta DF0WRITE
  lda #0
  sta DF0WRITE
  lda #2
  sta DF0WRITE
  lda #255
  sta CALLFUNCTION
  BIT DF0DATA
	BPL .skipL049
.condpart13
	SED
	CLC
	LDA score+2
	ADC #$02
	STA score+2
	LDA score+1
	ADC #$00
	STA score+1
	LDA score
	ADC #$00
	STA score
	CLD
        lda rand
	lsr
	STA player2x
	LDA #60
	STA player2y
.skipL049
.L050 ;  if collision(player0,player3) then score = 0 : player3x = rand / 2 : player3y = 30

	lda #<C_function
	sta DF0LOW
	lda #(>C_function) & $0F
	sta DF0HI
  lda #20
  sta DF0WRITE
  lda #0
  sta DF0WRITE
  lda #3
  sta DF0WRITE
  lda #255
  sta CALLFUNCTION
  BIT DF0DATA
	BPL .skipL050
.condpart14
	LDA #$00
	STA score+2
	LDA #$00
	STA score+1
	LDA #$00
	STA score
        lda rand
	lsr
	STA player3x
	LDA #30
	STA player3y
.skipL050
.L051 ;  goto mainLoop

 jmp .mainLoop

.
 ; 

.L052 ;  rem Music

.changeMusicNote
 ; changeMusicNote

.L053 ;  AUDF0  =  musicData[musicPointer]

	LDX musicPointer
	LDA musicData,x
	STA AUDF0
.L054 ;  if musicData[musicPointer]  =  $FF then AUDV0  =  0 else AUDV0  =  8

	LDX musicPointer
	LDA musicData,x
	CMP #$FF
     BNE .skipL054
.condpart15
	LDA #0
	STA AUDV0
 jmp .skipelse0
.skipL054
	LDA #8
	STA AUDV0
.skipelse0
.L055 ;  musicPointer  =  musicPointer  +  1

	INC musicPointer
.L056 ;  musicTimer  =  musicData[musicPointer]

	LDX musicPointer
	LDA musicData,x
	STA musicTimer
.L057 ;  musicPointer  =  musicPointer  +  1

	INC musicPointer
.L058 ;  if musicPointer  >  31 then musicPointer  =  0

	LDA #31
	CMP musicPointer
     BCS .skipL058
.condpart16
	LDA #0
	STA musicPointer
.skipL058
.L059 ;  return

	tsx
	lda 2,x ; check return address
	eor #(>*) ; vs. current PCH
	and #$E0 ;  mask off all but top 3 bits
	beq *+5 ; if equal, do normal return
	JMP BS_return
	RTS
.
 ; 

.L060 ;  data musicData

	JMP .skipL060
musicData
	.byte  29,26,23,24,19,29,24,23 

	.byte  29,26,23,24,19,29,24,23 

	.byte  26,19,23,29,19,29,24,23 

	.byte  26,19,23,29,19,29,24,23

.skipL060
.L061 ;  rem end Music
 if ECHO2
 echo "    ",[(start_bank2 - *)]d , "bytes of ROM space left in bank 2")
 endif
ECHO2 = 1
 ORG $2FF4-bscode_length
 RORG $3FF4-bscode_length
start_bank2 ldx #$ff
 ifconst FASTFETCH ; using DPC+
 stx FASTFETCH
 endif 
 txs
 if bankswitch == 64
   lda #(((>(start-1)) & $0F) | $F0)
 else
   lda #>(start-1)
 endif
 pha
 lda #<(start-1)
 pha
 pha
 txa
 pha
 tsx
 if bankswitch != 64
   lda 4,x ; get high byte of return address
   rol
   rol
   rol
   rol
   and #bs_mask ;1 3 or 7 for F8/F6/F4
   tax
   inx
 else
   lda 4,x ; get high byte of return address
   tay
   ora #$10 ; change our bank nibble into a valid rom mirror
   sta 4,x
   tya
   lsr 
   lsr 
   lsr 
   lsr 
   tax
   inx
 endif
 lda bankswitch_hotspot-1,x
 pla
 tax
 pla
 rts
 if ((* & $1FFF) > ((bankswitch_hotspot & $1FFF) - 1))
   echo "WARNING: size parameter in banksw.asm too small - the program probably will not work."
   echo "Change to",[(*-begin_bscode+1)&$FF]d,"and try again."
 endif
 ORG $2FFC
 RORG $3FFC
 .word (start_bank2 & $ffff)
 .word (start_bank2 & $ffff)
 ORG $3000
 RORG $5000
 repeat 129
 .byte 0
 repend
 if ECHO3
 echo "    ",[(start_bank3 - *)]d , "bytes of ROM space left in bank 3")
 endif
ECHO3 = 1
 ORG $3FF4-bscode_length
 RORG $5FF4-bscode_length
start_bank3 ldx #$ff
 ifconst FASTFETCH ; using DPC+
 stx FASTFETCH
 endif 
 txs
 if bankswitch == 64
   lda #(((>(start-1)) & $0F) | $F0)
 else
   lda #>(start-1)
 endif
 pha
 lda #<(start-1)
 pha
 pha
 txa
 pha
 tsx
 if bankswitch != 64
   lda 4,x ; get high byte of return address
   rol
   rol
   rol
   rol
   and #bs_mask ;1 3 or 7 for F8/F6/F4
   tax
   inx
 else
   lda 4,x ; get high byte of return address
   tay
   ora #$10 ; change our bank nibble into a valid rom mirror
   sta 4,x
   tya
   lsr 
   lsr 
   lsr 
   lsr 
   tax
   inx
 endif
 lda bankswitch_hotspot-1,x
 pla
 tax
 pla
 rts
 if ((* & $1FFF) > ((bankswitch_hotspot & $1FFF) - 1))
   echo "WARNING: size parameter in banksw.asm too small - the program probably will not work."
   echo "Change to",[(*-begin_bscode+1)&$FF]d,"and try again."
 endif
 ORG $3FFC
 RORG $5FFC
 .word (start_bank3 & $ffff)
 .word (start_bank3 & $ffff)
 ORG $4000
 RORG $7000
 repeat 129
 .byte 0
 repend
 if ECHO4
 echo "    ",[(start_bank4 - *)]d , "bytes of ROM space left in bank 4")
 endif
ECHO4 = 1
 ORG $4FF4-bscode_length
 RORG $7FF4-bscode_length
start_bank4 ldx #$ff
 ifconst FASTFETCH ; using DPC+
 stx FASTFETCH
 endif 
 txs
 if bankswitch == 64
   lda #(((>(start-1)) & $0F) | $F0)
 else
   lda #>(start-1)
 endif
 pha
 lda #<(start-1)
 pha
 pha
 txa
 pha
 tsx
 if bankswitch != 64
   lda 4,x ; get high byte of return address
   rol
   rol
   rol
   rol
   and #bs_mask ;1 3 or 7 for F8/F6/F4
   tax
   inx
 else
   lda 4,x ; get high byte of return address
   tay
   ora #$10 ; change our bank nibble into a valid rom mirror
   sta 4,x
   tya
   lsr 
   lsr 
   lsr 
   lsr 
   tax
   inx
 endif
 lda bankswitch_hotspot-1,x
 pla
 tax
 pla
 rts
 if ((* & $1FFF) > ((bankswitch_hotspot & $1FFF) - 1))
   echo "WARNING: size parameter in banksw.asm too small - the program probably will not work."
   echo "Change to",[(*-begin_bscode+1)&$FF]d,"and try again."
 endif
 ORG $4FFC
 RORG $7FFC
 .word (start_bank4 & $ffff)
 .word (start_bank4 & $ffff)
 ORG $5000
 RORG $9000
 repeat 129
 .byte 0
 repend
 if ECHO5
 echo "    ",[(start_bank5 - *)]d , "bytes of ROM space left in bank 5")
 endif
ECHO5 = 1
 ORG $5FF4-bscode_length
 RORG $9FF4-bscode_length
start_bank5 ldx #$ff
 ifconst FASTFETCH ; using DPC+
 stx FASTFETCH
 endif 
 txs
 if bankswitch == 64
   lda #(((>(start-1)) & $0F) | $F0)
 else
   lda #>(start-1)
 endif
 pha
 lda #<(start-1)
 pha
 pha
 txa
 pha
 tsx
 if bankswitch != 64
   lda 4,x ; get high byte of return address
   rol
   rol
   rol
   rol
   and #bs_mask ;1 3 or 7 for F8/F6/F4
   tax
   inx
 else
   lda 4,x ; get high byte of return address
   tay
   ora #$10 ; change our bank nibble into a valid rom mirror
   sta 4,x
   tya
   lsr 
   lsr 
   lsr 
   lsr 
   tax
   inx
 endif
 lda bankswitch_hotspot-1,x
 pla
 tax
 pla
 rts
 if ((* & $1FFF) > ((bankswitch_hotspot & $1FFF) - 1))
   echo "WARNING: size parameter in banksw.asm too small - the program probably will not work."
   echo "Change to",[(*-begin_bscode+1)&$FF]d,"and try again."
 endif
 ORG $5FFC
 RORG $9FFC
 .word (start_bank5 & $ffff)
 .word (start_bank5 & $ffff)
 ORG $6000
 RORG $B000
 repeat 129
 .byte 0
 repend
 if ECHO6
 echo "    ",[(start_bank6 - *)]d , "bytes of ROM space left in bank 6")
 endif
ECHO6 = 1
 ORG $6FF4-bscode_length
 RORG $BFF4-bscode_length
start_bank6 ldx #$ff
 ifconst FASTFETCH ; using DPC+
 stx FASTFETCH
 endif 
 txs
 if bankswitch == 64
   lda #(((>(start-1)) & $0F) | $F0)
 else
   lda #>(start-1)
 endif
 pha
 lda #<(start-1)
 pha
 pha
 txa
 pha
 tsx
 if bankswitch != 64
   lda 4,x ; get high byte of return address
   rol
   rol
   rol
   rol
   and #bs_mask ;1 3 or 7 for F8/F6/F4
   tax
   inx
 else
   lda 4,x ; get high byte of return address
   tay
   ora #$10 ; change our bank nibble into a valid rom mirror
   sta 4,x
   tya
   lsr 
   lsr 
   lsr 
   lsr 
   tax
   inx
 endif
 lda bankswitch_hotspot-1,x
 pla
 tax
 pla
 rts
 if ((* & $1FFF) > ((bankswitch_hotspot & $1FFF) - 1))
   echo "WARNING: size parameter in banksw.asm too small - the program probably will not work."
   echo "Change to",[(*-begin_bscode+1)&$FF]d,"and try again."
 endif
 ORG $6FFC
 RORG $BFFC
 .word (start_bank6 & $ffff)
 .word (start_bank6 & $ffff)
 ORG $7000
 RORG $D000
 repeat 129
 .byte 0
 repend
; Provided under the CC0 license. See the included LICENSE.txt for details.

;----------------------------------------
; Display Data
;----------------------------------------
; The Display Data bank is copied into RAM when DPC+ initializes the cartridge.
; This allows us to manipulate the data during run-time, but have a known
; starting state when the Atari is first turned on.
;
; Unlike normal Atari VCS/2600 sprite definitions, the sprite data in the
; Display Data bank is stored right-side-up.
;
;----------------------------------------

Zeros32:
SOUND_OFF = (* & $1fff)/32
DisplayDataDigitBlank:
        .byte 0;--
        .byte 0;--
        .byte 0;--
        .byte 0;--
        .byte 0;--
        .byte 0;--
        .byte 0;--
        .byte 0;--
        
;	align 32
;Zeros32:
;SOUND_OFF = (* & $1fff)/32
;	.byte 0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0    
	
SINE_WAVE = (* & $1fff)/32
	.byte 3,3,3,4,4,5,5,5
	.byte 5,5,5,5,4,4,3,3
	.byte 3,2,2,1,1,0,0,0
	.byte 0,0,0,0,1,1,2,2 

	align 32
TRIANGLE_WAVE = (* & $1fff)/32
	.byte 0,0,1,1,1,2,2,2
	.byte 3,3,3,4,4,4,5,5
	.byte 5,5,4,4,4,3,3,3
	.byte 2,2,2,1,1,1,0,0
	
 	align 32
SAWTOOTH_WAVE = (* & $1fff)/32
	.byte 0,0,0,0,1,1,1,1
	.byte 1,1,2,2,2,2,2,2
	.byte 3,3,3,3,3,3,4,4
	.byte 4,4,4,4,5,5,5,5
	
	align 32
SQUARE_WAVE_VOL5 = (* & $1fff)/32
	.byte 0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0
	.byte 5,5,5,5,5,5,5,5
	.byte 5,5,5,5,5,5,5,5

	align 32
SQUARE_WAVE_VOL4 = (* & $1fff)/32
	.byte 0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0
	.byte 4,4,4,4,4,4,4,4
	.byte 4,4,4,4,4,4,4,4

	align 32
SQUARE_WAVE_VOL3 = (* & $1fff)/32
	.byte 0,0,0,0,0,0,0,0
	.byte 0,0,0,0,0,0,0,0
	.byte 3,3,3,3,3,3,3,3
	.byte 3,3,3,3,3,3,3,3
	
	align 32
NOISE_WAVE = (* & $1fff)/32
	.byte  7, 1, 9,10, 2, 8, 8,14
	.byte  3,13, 8, 5,12, 2, 3, 7
	.byte  7, 1, 8, 4,15, 1,13, 5
	.byte  8, 5,11, 6, 8, 7, 9, 2

; low and high byte of address table (for ROMdata array in C)
        .byte <fetcher_address_table
        .byte ((>fetcher_address_table) & $0f) | (((>fetcher_address_table) / 2) & $70)
 .byte 0
 .byte 0
FETCHER_BEGIN
 .byte 16
 .byte 16
 .byte 16
 .byte 16 ; to zero-fill on boot
;bB.asm
; bB.asm file is split here
backgroundcolorL017
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $96
	.byte  $C0
	.byte  $C0
	.byte  $F0
	.byte  $92
	.byte  $92
	.byte  $92
playfieldcolorL018
	.byte  $70
	.byte  $70
	.byte  $70
	.byte  $70
	.byte  $70
	.byte  $70
	.byte  $70
	.byte  $70
	.byte  $70
	.byte  $D2
	.byte  $D2
	.byte  $D2
	.byte  $D2
	.byte  $D2
	.byte  $D2
	.byte  $D2
	.byte  $D2
	.byte  $D2
	.byte  $D2
	.byte  $D2
	.byte  $D2
	.byte  $D2
	.byte  $D2
	.byte  $F2
	.byte  $F2
	.byte  $F2
	.byte  $F2
	.byte  $F2
	.byte  $F2
	.byte  $F2
	.byte  $F2
	.byte  $F2
	.byte  $F2
	.byte  $F2
	.byte  $F2
	.byte  $F2
	.byte  $F2
	.byte  $F2
	.byte  $C0
	.byte  $C0
	.byte  $F0
	.byte  $F0
	.byte  $F0
	.byte  $F0
PF_data1
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00001111
	.byte %00011111
	.byte %00111111
	.byte %01111111
	.byte %01111111
	.byte %01111111
	.byte %01111111
	.byte %01111111
	.byte %01111111
	.byte %00111111
	.byte %00011111
	.byte %00000001
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000100
	.byte %00000100
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %11111100
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111110
	.byte %11111100
	.byte %11111100
	.byte %11111100
	.byte %11111100
	.byte %11111100
	.byte %11111100
	.byte %11111100
	.byte %11111100
	.byte %11111100
	.byte %11111100
	.byte %11111100
	.byte %11111100
	.byte %11111110
	.byte %11111111
	.byte %11111111
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111110
	.byte %11111100
	.byte %11111100
	.byte %11111100
	.byte %11111100
	.byte %11111100
	.byte %11111100
	.byte %11111100
	.byte %11111100
	.byte %11111100
	.byte %11111100
	.byte %11111100
	.byte %11111100
	.byte %11111110
	.byte %11111111
	.byte %11111111
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000001
	.byte %00000111
	.byte %00011111
	.byte %00111111
	.byte %01111111
	.byte %01111111
	.byte %01111111
	.byte %01111111
	.byte %01111111
	.byte %00111111
	.byte %00011111
	.byte %00000011
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000100
	.byte %00000100
	.byte %00000000
playercolorL021_1
	.byte  $70
	.byte  $70
	.byte  $F0
	.byte  $40
	.byte  $40
	.byte  $40
	.byte  $40
	.byte  $70
playerL022_1
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00111000
	.byte  %00111000
	.byte  %00111000
	.byte  %00000000
	.byte  %00000000
playercolorL024_2
	.byte  $70
	.byte  $70
	.byte  $F0
	.byte  $1E
	.byte  $1E
	.byte  $1E
	.byte  $1E
	.byte  $1E
playerL025_2
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00111000
	.byte  %00111000
	.byte  %00111000
	.byte  %00000000
	.byte  %00000000
playercolorL027_3
	.byte  $70
	.byte  $70
	.byte  $00
	.byte  $00
	.byte  $00
	.byte  $00
	.byte  $1E
	.byte  $1E
playerL028_3
	.byte  %00000000
	.byte  %00000000
	.byte  %00000000
	.byte  %00111000
	.byte  %00111000
	.byte  %00111000
	.byte  %00000000
	.byte  %00000000
playercolorL043_0
	.byte  $F0
	.byte  $F0
	.byte  $F0
	.byte  $F0
	.byte  $F0
	.byte  $F0
	.byte  $F0
	.byte  $00
playerL044_0
	.byte  %10000001
	.byte  %10100101
	.byte  %11111111
	.byte  %10100101
	.byte  %11111111
	.byte  %10100101
	.byte  %11111111
	.byte  %01000010
 if ECHOFIRST
       echo "    ",[(DPC_graphics_end - *)]d , "bytes of ROM space left in graphics bank")
 endif 
ECHOFIRST = 1
 
 
; Provided under the CC0 license. See the included LICENSE.txt for details.

       ORG $7FF4-bscode_length
       RORG $DFF4-bscode_length
DPC_graphics_end

; Provided under the CC0 license. See the included LICENSE.txt for details.

; every bank has this stuff at the same place
; this code can switch to/from any bank at any entry point
; and can preserve register values
; note: lines not starting with a space are not placed in all banks
;
; line below tells the compiler how long this is - do not remove
;size=32

begin_bscode
 ldx #$ff
 ifconst FASTFETCH ; using DPC+
 stx FASTFETCH
 endif 
 txs
 if bankswitch == 64
   lda #(((>(start-1)) & $0F) | $F0)
 else
   lda #>(start-1)
 endif
 pha
 lda #<(start-1)
 pha

BS_return
 pha
 txa
 pha
 tsx

 if bankswitch != 64
   lda 4,x ; get high byte of return address

   rol
   rol
   rol
   rol
   and #bs_mask ;1 3 or 7 for F8/F6/F4
   tax
   inx
 else
   lda 4,x ; get high byte of return address
   tay
   ora #$10 ; change our bank nibble into a valid rom mirror
   sta 4,x
   tya
   lsr 
   lsr 
   lsr 
   lsr 
   tax
   inx
 endif

BS_jsr
 lda bankswitch_hotspot-1,x
 pla
 tax
 pla
 rts
 if ((* & $1FFF) > ((bankswitch_hotspot & $1FFF) - 1))
   echo "WARNING: size parameter in banksw.asm too small - the program probably will not work."
   echo "Change to",[(*-begin_bscode+1)&$FF]d,"and try again."
 endif
; Provided under the CC0 license. See the included LICENSE.txt for details.

 org $8000
 rorg $1000
; Provided under the CC0 license. See the included LICENSE.txt for details.

; 1K Frequency Table.
; Fred Quimby, Darrell Spice Jr, Chris Walton 2010
;
; The 1K Frequency Table can contain up to 256 frequency values
;
; Table entries are defined as 2^32*freq/20000
;
; If User ARM code is being used, then the last 512 bytes of the frequency
; table will no longer be available, reducing the number of frequencies you can
; use to 128.

; piano key frequencies (s = sharp)

.freq_table_start

			DC.L 0
A0  = (* & $3ff)/4
			DC.L 5905580
			
A0s = (* & $3ff)/4
			DC.L 6256744

B0  = (* & $3ff)/4	
			DC.L 6628789
			
C1  = (* & $3ff)/4
			DC.L 7022958
			
C1s = (* & $3ff)/4			
			DC.L 7440565
			
D1  = (* & $3ff)/4			
			DC.L 7883004
			
D1s = (* & $3ff)/4		
			DC.L 8351751
			
E1  = (* & $3ff)/4			
			DC.L 8848372
			
F1  = (* & $3ff)/4			
			DC.L 9374524
			
F1s = (* & $3ff)/4			
			DC.L 9931962
			
G1  = (* & $3ff)/4			
			DC.L 10522547
			
G1s = (* & $3ff)/4			
			DC.L 11148251
			
A1  = (* & $3ff)/4			
			DC.L 11811160
			
A1s = (* & $3ff)/4			
			DC.L 12513488
			
B1  = (* & $3ff)/4			
			DC.L 13257579
			
C2  = (* & $3ff)/4			
			DC.L 14045916
			
C2s = (* & $3ff)/4			
			DC.L 14881129
			
D2  = (* & $3ff)/4			
			DC.L 15766007
			
D2s = (* & $3ff)/4			
			DC.L 16703503
			
E2  = (* & $3ff)/4			
			DC.L 17696745
			
F2  = (* & $3ff)/4			
			DC.L 18749048
			
F2s = (* & $3ff)/4			
			DC.L 19863924
			
G2  = (* & $3ff)/4			
			DC.L 21045095
			
G2s = (* & $3ff)/4			
			DC.L 22296501
			
A2  = (* & $3ff)/4			
			DC.L 23622320
			
A2s = (* & $3ff)/4			
			DC.L 25026976
			
B2  = (* & $3ff)/4			
			DC.L 26515158
			
C3  = (* & $3ff)/4			
			DC.L 28091831
			
C3s = (* & $3ff)/4			
			DC.L 29762258
			
D3  = (* & $3ff)/4			
			DC.L 31532014
			
D3s = (* & $3ff)/4			
			DC.L 33407005
			
E3  = (* & $3ff)/4			
			DC.L 35393489
			
F3  = (* & $3ff)/4			
			DC.L 37498096
			
F3s = (* & $3ff)/4			
			DC.L 39727849
			
G3  = (* & $3ff)/4			
			DC.L 42090189
			
G3s = (* & $3ff)/4			
			DC.L 44593002
			
A3  = (* & $3ff)/4			
			DC.L 47244640
			
A3s = (* & $3ff)/4			
			DC.L 50053953
			
B3  = (* & $3ff)/4			
			DC.L 53030316
			
C4  = (* & $3ff)/4			
			DC.L 56183662
			
C4s = (* & $3ff)/4			
			DC.L 59524517
			
D4  = (* & $3ff)/4			
			DC.L 63064029
			
D4s = (* & $3ff)/4			
			DC.L 66814011
			
E4  = (* & $3ff)/4			
			DC.L 70786979
			
F4  = (* & $3ff)/4			
			DC.L 74996192
			
F4s = (* & $3ff)/4			
			DC.L 79455697
			
G4  = (* & $3ff)/4			
			DC.L 84180379
			
G4s = (* & $3ff)/4			
			DC.L 89186005
			
A4  = (* & $3ff)/4			
			DC.L 94489281
			
A4s = (* & $3ff)/4			
			DC.L 100107906
			
B4  = (* & $3ff)/4			
			DC.L 106060631
			
C5  = (* & $3ff)/4			
			DC.L 112367325
			
C5s = (* & $3ff)/4			
			DC.L 119049034
			
D5  = (* & $3ff)/4			
			DC.L 126128057
			
D5s = (* & $3ff)/4			
			DC.L 133628022
			
E5  = (* & $3ff)/4			
			DC.L 141573958
			
F5  = (* & $3ff)/4			
			DC.L 149992383
			
F5s = (* & $3ff)/4			
			DC.L 158911395
			
G5  = (* & $3ff)/4			
			DC.L 168360758
			
G5s = (* & $3ff)/4			
			DC.L 178372009
			
A5  = (* & $3ff)/4			
			DC.L 188978561
			
A5s = (* & $3ff)/4			
			DC.L 200215811
			
B5  = (* & $3ff)/4			
			DC.L 212121263
			
C6  = (* & $3ff)/4			
			DC.L 224734649
			
C6s = (* & $3ff)/4			
			DC.L 238098067
			
D6  = (* & $3ff)/4			
			DC.L 252256115
			
D6s = (* & $3ff)/4			
			DC.L 267256044
			
E6  = (* & $3ff)/4			
			DC.L 283147915
			
F6  = (* & $3ff)/4			
			DC.L 299984767
			
F6s = (* & $3ff)/4			
			DC.L 317822789
			
G6  = (* & $3ff)/4			
			DC.L 336721516
			
G6s = (* & $3ff)/4			
			DC.L 356744019
			
A6  = (* & $3ff)/4			
			DC.L 377957122
			
A6s = (* & $3ff)/4			
			DC.L 400431622
			
B6  = (* & $3ff)/4			
			DC.L 424242525
			
C7  = (* & $3ff)/4			
			DC.L 449469299
			
C7s = (* & $3ff)/4			
			DC.L 476196134
			
D7  = (* & $3ff)/4			
			DC.L 504512230
			
D7s = (* & $3ff)/4			
			DC.L 534512088
			
E7  = (* & $3ff)/4			
			DC.L 566295831
			
F7  = (* & $3ff)/4			
			DC.L 599969533
			
F7s = (* & $3ff)/4			
			DC.L 635645578
			
G7  = (* & $3ff)/4			
			DC.L 673443031
			
G7s = (* & $3ff)/4			
			DC.L 713488038
			
A7  = (* & $3ff)/4			
			DC.L 755914244
			
A7s = (* & $3ff)/4			
			DC.L 800863244
			
B7  = (* & $3ff)/4			
			DC.L 848485051
			
C8  = (* & $3ff)/4			
			DC.L 898938597

;values for 89-255 may go here 

	if (* <= $1400)
	  ds ($1400-*) ; pad out remaining space in frequency table
	else
	  echo "FATAL ERROR - Frequency table exceeds 1K"
	  err
	endif
