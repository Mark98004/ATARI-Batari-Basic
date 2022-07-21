 set romsize = 2k
 set tv ntsc
 include 6lives.asm
 playfield:
 ................................
 ................................
 ................................
 ................................
 ................................
 ................................
 ................................
 ................................
 ................................
 ................................
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
end
 COLUBK=$96
 COLUPF=$F8
start
 drawscreen
 if joy0fire then goto game
 goto start
game
 dim _SC_Back = x
 dim _Ch0_Sound = c
 dim _Ch0_Duration = d
 dim _C0 = e
 dim _V0 = f
 dim _F0 = g
 x=80:y=80
 missile0height=4:missile0y=255
 player0x=rand/2
 lives=160
 lives:
 %00000000
 %01010100
 %01111100
 %00111000
 %00101000
 %00111000
 %00010000
 %00010000
end
mainloop
 player0y=player0y+1
 player1x=x:player1y=y
 if missile0y > 240 then goto skip
 missile0y = missile0y-2:goto draw_loop
skip
 player1:
 %0000000
 %0101010
 %0111110
 %0011100
 %0010100
 %0011100
 %0001000
 %0001000
end

 player0:
 %11010001
 %01011001
 %11001011
 %11111111
 %01111110
 %01011010
 %01111110
 %00111100
end

draw_loop

 drawscreen
 if player0y>80 then player0y=0:player0x=rand/2:lives = lives-32
jump
 if joy0left then x=x-2
 if joy0right then x=x+2
 if joy0fire then if !_Ch0_Sound then _Ch0_Sound = 1 : _Ch0_Duration = 15 : missile0y=player1y-2
 if !_Ch0_Sound then goto __Skip_Ch_0
 _Ch0_Duration = _Ch0_Duration - 1
 if !_Ch0_Duration then goto __Clear_Ch_0
 if _Ch0_Sound <> 1 then goto __Skip_Ch0_Sound_001
 AUDC0 = 4 : AUDV0 = 8 : AUDF0 = 19
 goto __Skip_Ch_0
__Skip_Ch0_Sound_001
 goto __Skip_Ch_0
__Clear_Ch_0  
 _Ch0_Sound = 0 : AUDV0 = 0
__Skip_Ch_0
 missile0x = player1x+4
 if collision(player0, missile0) then score=score+1:player0x=rand/2:player0y=0
 if lives<32 then goto over
 goto mainloop

over
 player0y=255:player1y=255:missile0y=255:scorecolor=$44
 playfield:
 ................................
 ...X........XXX.....XXXX...XXXXX
 ...X.......X...X...X.......X....
 ...X......X.....X..X.......X....
 ...X......X.....X..X.......X....
 ...X......X.....X..X.......XXXXX
 ...X......X.....X...XXXX...X....
 ...X......X.....X.......X..X....
 ...X......X.....X.......X..X....
 ...X.......X...X........X..X....
 ...XXXXXX...XXX.....XXXX...XXXXX
end
 COLUBK=$00
 COLUPF=$44
 drawscreen
 goto over
