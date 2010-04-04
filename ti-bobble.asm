;
; ti-bobble
;
; A clone of Bubble Bobble for the Texas Instruments TI-85 series of
; calculators.
;
; Copyright Colin Patrick McCabe, 2000
;
; This software is licensed under the GPL version 2.
; See LICENSE for details.
;

#include "ti-85.h"
 ;********** Name **************
 .org 0 ;need this
 .db "by Colin McCabe",0
 ;********** 'Variables' *********
;Intro stuff
kDinoPicStart =   $FC60
kLastLevel =      22 ;last level +1

;Player Constants
kRealScreenDown =            51
kFirstUnsafeScreenDown =     51
kMaxDown =                   58
kMaxDown_x2 =                116
kMaxRight =                  121
kMaxRight_x2 =               242

;Player Variables
;TEXT_MEM =       $80DF
lavab =           $80DF
lavaa =           $80E0 
playerOri =       $80E1
playerJump =      $80E2
deathFlag =       $80E3
CurBoltAddr =     $80E4
CurBoltNum =      $80E6
kFirstBolt =      $80E7
kNumBolts =       6 ;15

kFirstBubble =    $814F
kMaxBubs =        6      ;4 b per bubble
kFirstMonster =   $8107  ;leaving space for 10 bubbles
kMaxMonsters =    5      ;kMaxBubs must be more than                         ;kMaxMonsters!!

CurBubbleAddr =   $8119  ;two-byte address of current addr for shooting bubs
CurBubNum =       $8121  ;one-byte number of current 6-bub number
CurRefire =       $8122
RefillRefire =    $8123
kTickTock =       $8124
kTempByte =       $8125
kInvincibility =  $8126

;High Memory
lives =           $8176
LevelAddr =       $8177 ;2-byte value!
level =           $8179
blankByte =       $817A
;END OF TEXT_MEM  $8187

kMaxBubLifeD =    %00000001
kMaxBubLifeE =    %00000000
kBubZoom =        %11111110
kBubShootTime =   16

_16pGRAPH_MEM =   $8650
EndoGraph =       $8A40
EndoVid =         $FFFF
;GRAPH_MEM =      $8641 ;This is practically the whole graph mem
WorkMap =         $8641
_1mBrickMap =     $8970
_16mBrickMap =    $8961
BrickMap =        $8971
_16pBrickMap =    $8981
LBrickMap =       $8981
EndoBrickMap =    $8A31
;END OF GRAPH_MEM $8A41

;TEXT_MEM2 =      $8A6B
_8mBubbleMap =    $8A63 ;needed for technical reasons
BubbleMap =       $8A6B

;System Things
textCursor =      $8333

;VIDEO_MEM =      $FC00
_16pVIDEO_MEM =   $FC10
;********** Initialization **********
 ld a,4
 out (5),a
;********** Program ***********
ShowIntro:
 ROM_CALL(CLEARLCD)
 ld de,(PROGRAM_ADDR)
 ld hl,IntroPic
 add hl,de
 ld de,kDinoPicStart
 ld a,24
DrawDinoLoop
 ldi
 ldi
 ldi
 ex de,hl
 ld bc,13
 add hl,bc
 ex de,hl
 sub 1
 jr nz, DrawDinoLoop
 ld hl,$0600
 ld de,Sidebar
 CALL_(ColinDisp)
 ld hl,$0501
 ld de,AuthorString
 CALL_(ColinDisp)
 ld hl,$0007
 ld de,GreetString
 CALL_(ColinDisp)
 ld hl,$0907
 ld de,GreetStringTwo
 CALL_(ColinDisp)
 ld hl,$1718
 ld de,HelpString
 CALL_(ColinDispSmall)
 ld hl,$1D18
 ld de,HOne
 CALL_(ColinDispSmall)
 ld hl,$2318
 ld de,HTwo
 CALL_(ColinDispSmall)
 ld hl,$234A
 ld de,HThree
 CALL_(ColinDispSmall)
 ld hl,$2918
 ld de,HFour
 CALL_(ColinDispSmall)
 ld hl,$294A
 ld de,HFive
 CALL_(ColinDispSmall)
 ld hl,$2F18
 ld de,HSix
 CALL_(ColinDispSmall)

WaitForInput:
 call GET_KEY
 cp K_EXIT
 ret z
 cp K_F2
 jr z, NewGame
 cp K_F1
 jr nz, WaitForInput
LoadGame:
 CALL_(SetupFile)
 ldir
 jr InitGame
NewGame:
 CALL_(NewData)
InitGame:
 ld a,(level)
 srl a
 jr c,ContinueInit
 srl a
 jr c,ContinueInit
 ld a,(lives)
 inc a
 ld (lives),a
ContinueInit:
 CALL_(DrawScreen)
 CALL_(BlitFromBrickMap)
;ClearLowerScreen:
 ld bc,240
 ld hl,_16pBrickMap
 CALL_(BlockClearMem)
Scrolling:
 ld b,64
 ld hl,GRAPH_MEM
ScrollLoop
 push bc
 push hl
 ld hl,1000
 CALL_(Delay)
 ld de,VIDEO_MEM
 ld hl,_16pVIDEO_MEM
 ld bc,1008
 ldir
 pop hl
 ld de,$FFF0
 ld bc,$10
 ldir
 pop bc
 djnz ScrollLoop

LoadBubbleMap:
 ld hl,(LevelAddr)
 ld bc,24
 add hl,bc ;length of brick data
 ld de,(PROGRAM_ADDR)
 add hl,de
 ex de,hl
 ld hl, BubbleMap
 ld b,16
LoadOneByte:
 ld a,(de)
 inc de
 push bc
 ld b,4
DecompressByte:
 add a,a
 ld c,%00000000
 jr nc, ResumeBap
 ld c,%00000010
ResumeBap:
 add a,a
 jr nc, EndBap
 inc c
EndBap:
 ld (hl),c
 inc hl
 djnz DecompressByte
 pop bc
 djnz LoadOneByte

 ld hl,TEXT_MEM         ;Clear the memory to avoid confusion
 ld bc,150              ;this should be done _before_ any other loading
 CALL_(BlockClearMem)

DecompressMonsters: ;the sleep of compression produces monsters!
 ld hl, (LevelAddr)
 ld bc,40
 add hl,bc        ;level data+breeze data
 ld de, (PROGRAM_ADDR)
 add hl,de
 ex de, hl
 ld b, kMaxMonsters
 ld hl, kFirstMonster
MonsterDecoLoop:
 ld a,(de)
 inc de
 push de
 CALL_(ExpandHalfByte)
 ld (hl),d
 inc hl
 ld (hl),e
 inc hl
 pop de
 ld a,(de)
 inc de
 ld (hl),a
 inc hl
 djnz MonsterDecoLoop

 ld de,kFirstBubble     ;InitBubbles
 ld (CurBubbleAddr),de
 ld a,kMaxBubs
 ld (CurBubNum),a
 ld a,kNumBolts         ;InitBolts:
 ld (CurBoltNum),a
 ld bc, kFirstBolt
 ld (CurBoltAddr),bc

SetPlayerData:
 ld hl,(LevelAddr)
 ld bc,50
 add hl,bc
 ld de,(PROGRAM_ADDR)
 add hl,de
 ld a,(hl)
 CALL_(ExpandHalfByte)
 ld a,d
 ld (lavaa),a
 ld a,e
 ld (lavab),a
 ld a,17        ;Set player refire rate
 ld (refillRefire),a
 ld a,30
 ld (kInvincibility),a
 CALL_(BlitFromBrickMap)

DrawTextSidebar:
 CALL_(DrawScreen)
 CALL_(RefreshIndicator)

StartCycle:        ;Every game cycle begins here, with a full blit to screen
 CALL_(BlitFromBrickMap)

ChangeTickTock:
 ld a,(kTickTock)
 cpl
 ld (kTickTock),a
 ld a,(kInvincibility)
 sub 1
 jr c,MainLoop
 ld (kInvincibility),a

MainLoop:       ;First, delay
 ld hl,1000
 CALL_(Delay)

LoadPlayerPos:
 ld a,(playerJump)
 sub 12
 ld bc,(lavab)
 CALL_C(Gravity)
 ld (lavab),bc

LoadPlayerOrientation:
 ld a,(playerOri)
 jr c, EndLoadPlayerOrientation 
 set 2,a                            ;the gravity sub gave us
 ld (playerOri),a           ;a solid ground carry flag
EndLoadPlayerOrientation:

Bubbles:
 ld ix,kFirstBubble
 ld b,kMaxBubs
BubbleLoop:
 push bc
LoadBubblePos: ;yea! a useful shift byte ;)
 ld b,(ix)
 ld c,(ix+1)
 ld d,(ix+2)
 ld e,(ix+3)
FindBubbleType:
 ld a,d
 cp %01000000
 jr c, OlderBubble
NewBubble:
 ld a,e
 sub 1
 jr nc, ContinueWithNewBubble
 ld d,kMaxBubLifeD
 ld e,kMaxBubLifeE
 ld a,d
 jr OlderBubble
ContinueWithNewBubble:
 ld (ix+3),a
 bit 0,d
 ld a,b
 jr nz,NewRight
NewLeft:
 sub 4
 jr c, CheckEntrapment
 ld b,a
 jr CheckEntrapment
NewRight:
 add a,4
 cp kMaxRight_x2
 jr nc, CheckEntrapment
 ld b,a
CheckEntrapment:
 push bc
 ld d,(ix)
 ld e,(ix+1)
 srl d
 srl e
 ld b,kMaxMonsters
 ld hl,kFirstMonster
TrapLoop: ;Horizontal
 ld a,(hl)
 add a,8
 sub d
 jr c, NextAddThree
 cp 16
 jr nc, NextAddThree
 inc hl
 ld a,(hl) ;vertical
 add a,7
 sub e
 jr c, NextAddTwo
 cp 16
 jr nc, NextAddTwo
ActuallyTrapMonster:
 inc hl
 ld a,(hl)
 and %00000011
 jr z,NextAddOne
 add a,a
 add a,a
 inc a
 ld (ix+2),a
 ld a,kMaxBubLifeE ;zzz
 ld (ix+3),a
 ld (hl),0
 jr EndOfTraps
NextAddThree:
 inc hl
NextAddTwo:
 inc hl
NextAddOne:
 inc hl
 djnz TrapLoop
EndOfTraps:
 pop bc
FinishNew:
 ld hl,SmallBubbleSprite
 JUMP_(SaveBubblePos)

OlderBubble:
 and %00000001
 or e
 JUMP_Z(EndLoopAndDelay)
 dec de
 ld (ix+2),d
 ld (ix+3),e
 ld a,d
 and %00001100 ;keep out regular bubbles from this area
 jr z, MoveBubble
PossibleMonsterPop:
 ld a,d
 and %00000001
 or e
 jr nz, EndUnbubbling
 ld hl,kFirstMonster
 srl b
 ld a,b
 ld (kTempByte),a
 ld b,kMaxMonsters
UnbubbleMonsterLoop:
 inc hl
 inc hl
 ld a,(hl)
 and %00001111
 jr nz, EndUnbubbleMonsterLoop
 srl d
 srl d
 ld (hl),d
 dec hl
 srl c
 ld (hl),c
 dec hl
 ld a,(kTempByte)
 ld (hl),a
 ld (ix+2),0
 ld (ix+3),0
 JUMP_(EndLoop)
EndUnbubbleMonsterLoop:
 inc hl
 djnz UnbubbleMonsterLoop
 JUMP_(EndLoop)
EndUnbubbling:
MoveBubble:
 ld a,b
 ld b,c
 srl b
 srl b
 srl b
 srl b
 ld de, 8
 ld hl,_8mBubbleMap
 inc b
FindBubbleOnMap:
 add hl, de
 djnz FindBubbleOnMap
 ld e,a
 srl e
 srl e
 srl e
 srl e
 srl e
 add hl,de
 ld b,a
 ld e,(hl)
 bit 1,e
 jr nz, MoveVertical
MoveHorizontal:
 ld a,b
 bit 0,e
 jr nz,MoveRight
MoveLeft:
 sub 1
 jr c,ContinueWBubble
 ld a,b
 CALL_(BubHorizontal)
 jr nz, ContinueWBubble
 dec b
 jr ContinueWBubble
MoveRight:
 inc a
 cp kMaxRight_x2
 jr nc,ContinueWBubble
 ld a,b
 add a,7
 CALL_(BubHorizontal)
 jr nz, ContinueWBubble
 inc b
 jr ContinueWBubble
MoveVertical:
 ld a,c
 bit 0,e
 jr nz,MoveDown
MoveUp:
 push bc
 sub 1
 jr nc,ContinueWBubUp
 ld a,kMaxDown_x2
ContinueWBubUp:
 sub 15
 jr nc, ImmanuelKant
 ld a,0
ImmanuelKant:
 ld c,a
 ld a,b
 srl a
 srl c
 CALL_(CheckTile)
 pop bc
 jr nz, ContinueWBubble
 ld a,c
 sub 1
 ld c,a
 jr nc,ContinueWBubble
 ld c,kMaxDown_x2
 jr ContinueWBubble
MoveDown:
 push bc
 inc a
 ld c,a
 cp kMaxDown_x2
 jr c,ContinueWBubDown
 ld c,0
ContinueWBubDown:
 ld a,b
 srl a
 srl c
 CALL_(CheckTile)
 pop bc
 jr nz, ContinueWBubble
 ld a,c
 inc a
 ld c,a
 cp kMaxDown_x2
 jr c,ContinueWBubble
 ld c,0
ContinueWBubble:        ;pop/jump subroutine based on player loc
 ld a,(lavaa)           ;horizontal check
 add a,a
 add a,15
 sub b
 jr c,EndContinueWithBubble
 sub 32;sub 16
 jr nc,EndContinueWithBubble
;HorzOk:
 ld a,(lavab)
 add a,a
 add a,16
 sub c
 jr c,EndContinueWithBubble
 cp 6
 jr c,BoostPlayer
 cp 32
 jr nc,EndContinueWithBubble
PopBubble:
 ld (ix+2),0
 ld (ix+3),0
 JUMP_(EndLoop)
BoostPlayer:
 ld a,(playerOri)
 or %00001100
 ld (playerOri),a
EndContinueWithBubble:
 ld hl,RegularBubbleSprite
 ld a,(ix+2)
 cp %00000100
 jr c,SaveBubblePos
 ld hl,CaptureSprite
SaveBubblePos:
 ld (ix),b
 ld (ix+1),c
DrawBubble:
 push ix
 srl b
 srl c
 ld de,(PROGRAM_ADDR)
 add hl,de
 CALL_(PutSprite)
 pop ix
 jr EndLoop
EndLoopAndDelay:
 ld hl,100 ;470?200?
 CALL_(Delay)
EndLoop:
 ld de,4
 add ix,de ;prepare for next bubble
 pop bc
 ld a,b
 sub 1
 ld b,a
 JUMP_NZ(BubbleLoop)
EndBubbles:

Monsters:
 ld a,kMaxMonsters
 ld hl,kFirstMonster
MonstrousLoop:
 push af
 push hl
 ld b,(hl)
 inc hl
 ld c,(hl)
 inc hl
 ld a,(hl)
 ld (kTempByte),a
 and %00000011
 JUMP_Z(EndMonsterLoop)
 cp 1
 JUMP_Z(CogMovement)
 cp 3
 JUMP_Z(BelugaMovement)

; 0=off
; 1=cog
; 2=zapper
; 3=beluga

ZapperIntro:  ;bolt firing mechanism: only for zappers
 ld a,(kTickTock)
 or a
 jr nz,ZapperMovement
 ld a,r
 and   %00001000
 add a,%00010000
 ld d,a
 ld a,(kTempByte)
 sub d
 ld (kTempByte),a
 jr nc,ZapperMovement
 or %11111000
 ld a,(kTempByte)
MakeBolt:
 ld hl,(CurBoltAddr)
 ld (hl),b
 inc hl
 ld (hl),c
 ld a,(CurBoltNum)
 sub 1
 jr z,ScrollToFirstBolt
 ld (CurBoltNum),a
 inc hl
 ld (CurBoltAddr),hl
 jr ZapperMovement
ScrollToFirstBolt:
 ld hl,kFirstBolt
 ld (CurBoltAddr),hl
 ld a,kNumBolts
 ld (CurBoltNum),a
ZapperMovement:
 CALL_(Gravity)
ZapCore:
 ld a,(kTempByte)
 bit 2,a
 jr nz,MoveMonRight
 CALL_(PlayerLeft)
 JUMP_NC(EndMove)
 ld a,(kTempByte)
 set 2,a
 ld (kTempByte),a
MoveMonRight:
 CALL_(PlayerRight)
 JUMP_NC(EndMove)
 ld a,(kTempByte)
 res 2,a
 ld (kTempByte),a
 CALL_(PlayerLeft)
 JUMP_(EndMove)

BelugaMovement:
 ld a,r
 and %00001001
 jr z,ZapCore
 ld a,(kTempByte)
 and %00001000   ;bit 3 is up/down (first bit is 0)
 jr z,AntiGravity
 CALL_(Gravity)
 jr c,ZapCore
InvertBitThree:
 ld a,(kTempByte)
 xor %00001000    ;'or' first?
 ld (kTempByte),a
 jr ZapCore
AntiGravity:
 ld a,c
 cp 10
 jr c,AntiGravWrap
 push bc
 sub 7
 ld c,a
 ld a,(kTempByte)
 and %00001000
 ld a,b
 jr z,AGravity
 add a,8
AGravity:
 CALL_(CheckTile) ;checks a,c
 pop bc
 dec c
 jr nc, ZapCore
 inc c
 jr InvertBitThree
AntiGravWrap:
 dec c
 cp 1
 jr nc,ZapCore
 ld c,kMaxDown
 jr ZapCore

CogMovement:
 ld a,(kTempByte)
 sub %1000
 jr c, NormalCogMove
 ld (kTempByte),a
 cp %1100111
 JUMP_C(ZapperMovement)
 ld a,c
 sub 1
 ld c,a
 JUMP_NC(ZapCore)
 ld c,kMaxDown
 JUMP_(ZapCore)
NormalCogMove:
 CALL_(Gravity)
 JUMP_C(ZapCore)
 ld a,r
 and %00111111
 cp 1
 jr c, MakeCogDoJump
 cp 2
 jr c, ConsiderJump
 cp 3
 jr c, ReasonedTurn
 cp 4
 jr c, RandomTurn
 JUMP_(ZapCore)
RandomTurn:
 ld a,(kTempByte)
 xor %00000100
 ld (kTempByte),a 
 JUMP_(ZapCore)
ConsiderJump:
 ld a,(lavab)
 cp c
 JUMP_NC(ZapCore)
MakeCogDoJump:
 ld a,(kTempByte)
 or %11100000
 ld (kTempByte),a
 JUMP_(ZapCore)
ReasonedTurn:
 ld a,(lavaa)
 cp b
 jr c, TurnCogLeft
TurnCogRight:
 ld a,(kTempByte)
 set 2,a
 ld (kTempByte),a
 JUMP_(ZapCore)
TurnCogLeft:
 ld a,(kTempByte)
 res 2,a
 ld (kTempByte),a
 JUMP_(ZapCore)

EndMove: ;in other words,
CheckPlayerIntersection:
 ld a,(lavaa)
 add a,7
 sub b
 jr c,EndCheckPI
 cp 14
 jr nc,EndCheckPI
 ld a,(lavab)
 add a,7
 sub c
 jr c, EndCheckPI
 cp 14
 jr nc, EndCheckPI
ReallyHitPlayer:
 CALL_(SetDeathFlag)
EndCheckPI:
TransToMem:
 pop hl
 push hl
 ld (hl),b
 inc hl
 ld (hl),c
 inc hl
 ld a,(kTempByte)
 ld (hl),a
 and %00000111
 bit 0,a
 jr z,DrawVolt
 bit 1,a
 jr nz,DrawBeluga
DrawCog:
 ld hl,CogPic
 bit 2,a
 jr z, DrawMonster
 ld hl,CogRightPic
 jr DrawMonster
DrawVolt:
 ld hl,VoltPic
 jr DrawMonster
DrawBeluga:
 ld hl,BelugaPic
 bit 2,a
 jr z, DrawMonster
 ld hl,BelugaRightPic
DrawMonster:
 ld de,(PROGRAM_ADDR)
 add hl,de
 CALL_(PutSprite)
EndMonsterLoop:
 pop hl
 pop af
 inc hl
 inc hl
 inc hl
 sub 1
 JUMP_NC(MonstrousLoop)

BoltRoutine:
 ld b,kNumBolts
 ld hl,kFirstBolt
DoBolts:
 push bc
 ld b,(hl)
 inc hl
 ld a,(hl)
 or a
 jr z, NextBolt
 add a,3
 cp kMaxDown
 jr c, CheckBoltColl
 ld a,0
 ld (hl),a
 jr NextBolt
CheckBoltColl:
 ld (hl),a
 ld c,a
 ld a,(lavaa)
 add a,5
 sub b
 jr c,EndCheckBolt
 cp 10
 jr nc, EndCheckBolt
 cp 2
 jr c, EndCheckBolt
 ld a,(lavab)
 add a,7
 sub c
 jr c, EndCheckBolt
 cp 14
 CALL_C(SetDeathFlag)
EndCheckBolt:
DrawBolt:
 push hl
 ld hl,BoltPic
 ld de,(PROGRAM_ADDR)
 add hl,de
 CALL_(PutSprite)
 pop hl
NextBolt:
 inc hl
 pop bc
 djnz DoBolts

LoadPlayerDataIn:
 ld a,(playerOri)
 ld e,a
 ld bc,(lavab) 

ReadKeys:
 ld a,%10111110 ; Check the arrow keys and 2nd, exit
 out (1),a
 in a,(1)
 ld d,a
 bit 1,d
 CALL_Z(PlayerLeft)
 bit 2,d
 CALL_Z(PlayerRight)
 bit 5,d
 CALL_Z(MakeBubble)
 bit 7,d
 CALL_Z(PauseGame)
BubbleRefireAdjustment:
 ld a,(CurRefire)
 or a
 jr z,NoAdjust
 dec a
 ld (CurRefire),a
NoAdjust:
 bit 6,d
 JUMP_Z(EndNSaveProgram)

Jump:
 ld a,(playerJump) 
 or a
 jr nz, Ascend
 bit 2,e             ;check on-ground bit
 jr z, EndJump
BeginJump:
 bit 3,d
 jr nz, EndJump      ;check up-key-is-down bit
 ld a,24             ;ok, we really can jump- let's do it
Ascend:
 dec a
 ld (playerJump),a
 sub 12
 jr nc,DoAscend
 bit 3,e  ;superjump flag
 jr nz,BeginJump
 jr EndJump
DoAscend:
 dec c
 ld a,c ;;don't know if necessary or not
 sub 1
 jr nc, EndJump
 ld c, kMaxDown
EndJump:

PlayerDataToMem:
 ld (lavab),bc
 ld a,e
 and %00000010
 ld (playerOri),a
DrawPlayer:
 ld hl,PlayerLeftSprite
 bit 1,a
 jr z, DrawPlayer2
 ld hl,PlayerRightSprite
DrawPlayer2:
 ld de,(PROGRAM_ADDR)
 add hl,de
 CALL_(PutSprite)

BlitWorkToScreen:
 ld hl,WorkMap
 ld de,VIDEO_MEM
 ld bc,832 ;896 ;b from graph to screen
 ldir

LevelCheck:
 ld a,r
 and %00001111
 CALL_Z(LevelDoneCheck)
 jr z, IncLevel
CheckForDeath:
 ld a,(deathFlag)
 or a
 JUMP_Z(StartCycle)

;******************* Subroutines *******************;
 CALL_(FlashScreen)
 ld hl,50000
 CALL_(Delay)
 CALL_(FlashScreen)
 ld a,(lives)
 sub 1
 ld (lives),a
 ld a,0
 ld (deathFlag),a
 JUMP_NZ(SetPlayerData)
 CALL_(NewData)
 jr Save

EndNSaveProgram:
 ld a,(lives)
 sub 1
 ld (lives),a
 CALL_Z(NewData)
Save:
 CALL_(SetupFile)
 ex de,hl
 ldir
 ld a,(ZS_BITS) ;must tell zshell to recalc the checksum
 set 0,a
 ld (ZS_BITS),a

EndProgram:  ;This one you should figure out yourself
 call GET_KEY
 call STORE_KEY; Clear the key buffer
 ret

IncLevel:
 ld a,(level)
 inc a
 cp kLastLevel
 jr z, EndProgram ;Display message later?
 ld (level),a
 ld hl,(levelAddr)
 ld bc,51
 add hl,bc
 ld (levelAddr),hl
 JUMP_(InitGame)

PauseGame:
 push de
 ld hl,$0103 ;0
 ld de,PauseString
 CALL_(ColinDisp)
 pop de ;extra call to GET_KEY eliminated
PauseLoop:
 call GET_KEY
 cp K_ENTER
 ret z
 jr PauseLoop

FlashScreen:    ;Invert Screen
 ld hl,VIDEO_MEM
 dec hl ;yuck!
 ld bc,1024
FlashLoop:
 inc hl
 dec bc
 ld a,(hl)
 cpl
 ld (hl),a 
 ld a,c
 or b
 ret z
 jr FlashLoop

PlayerLeft:
 ld a,b
 sub 1
 res 1,e
 ret c
 push bc
 push de
 CALL_(CheckTile)
 pop de
 pop bc
 ret c
 dec b
 ret

PlayerRight:
 set 1,e
 ld a,b
 inc a
 cp kMaxRight
 jr nc,Retwcarry
 push bc
 push de
 add a,7
 CALL_(CheckTile)
 pop de
 pop bc
 ret c
 inc b
 ret
Retwcarry:
 scf
 ret

CheckTile: ;checks the tile at a,c
 srl a    ;carry flag is on if the block is clear
 srl a
 srl a
 ld b,c
 ld c,a
 srl b
 srl b
 ld de, $10
 ld hl, _16mBrickMap
 inc b
GetTileLoop:
 add hl,de
 djnz GetTileLoop
 add hl,bc
 ld a,(hl)
 or a
 ret z
 scf
 ret

PutSprite: ; Puts a sprite pointed to by HL at B,C
 ld e,8
ScrollUpCheck:
 ld a,6
 sub c
 jr c, ScrollDownCheck
 ld d,0
 ld e,a
 inc e
 add hl,de
 ld e,c
 inc e
 ld c,7
 jr StartActualPS
ScrollDownCheck:
 ld a,kRealScreenDown
 sub c
 jr nc, StartActualPS
 add a,8
 ld e,a
StartActualPS: 
 ld d,8
 ld a,70
 sub c
 ret c
 push de
 ld c,a
 push hl
 ROM_CALL(FIND_PIXEL)
 ld de,WorkMap
 add hl,de
 pop ix
 pop bc  ;pop number of pixels each way
PS_NewRow:
 push bc
 ld d,(ix)
 inc ix
 push af
 push hl
PS_NewCol:
 rl d
 ld e,a
 jr nc,PS_NextPixel
 or (hl)
 ld (hl),a
 jr PS_NextPixel
PS_NextPixel:
 ld a,e
 rrca
 jr nc,PS_SameByte
 inc hl
PS_SameByte:
 djnz PS_NewCol
 pop hl
 pop af
 ld de,16
 add hl,de
 pop bc
 dec c
 jr nz,PS_NewRow
 ret

Gravity:
 ld a,c                     ;Check if creature is outside bounds
 cp kFirstUnsafeScreenDown
 jr c, BeginBrickTests
 inc c
 cp kMaxDown
 jr c, EndGravityButRememberItWasDone
 ld c,0
 jr EndGravityButRememberItWasDone
BeginBrickTests:
 sub 3 ;remember, rows 0-7 are offscreen
 srl a
 jr c, DoGravity
 srl a
 jr c, DoGravity
 push bc
 ld c,b
 ld b,a
 srl c
 jr c, TwoLeftCarry
 srl c
 jr c, OneLeftCarry
 srl c
 jr c, NoneLeftCarry
 jr LoadBrickData ;well, a won't be zero, and we'll only load one byte
TwoLeftCarry:     ;while checking block data
 srl c
OneLeftCarry:
 srl c
NoneLeftCarry:
 ld a,0         ;we can safely use a as a flag register
LoadBrickData:  ;w/value of zero because zeros already weeded out
 ld hl, _16mBrickMap
 ld de, $10
 inc b
FindVertLoop:
 add hl, de
 djnz FindVertLoop ;after this, b is 0
 add hl, bc
CheckBlocks:
 pop bc
 ld d,a
 or a
 jr z,InterblockCase
IntrablockCase:
 ld a,(hl)
 or a
 jr nz, DoGravity
 ld de,$10
 add hl,de
 ld a,(hl)
 or a
 jr nz, EndGravity
 jr DoGravity
InterblockCase:
 ld d,(hl)
 inc hl
 ld e,(hl)
 ld a,d
 or e
 jr nz,DoGravity
 ld de,$0F
 add hl,de
 ld d,(hl)
 inc hl
 ld e,(hl)
 ld a,d
 or e
 jr nz,EndGravity
DoGravity:
 inc c
EndGravityButRememberItWasDone:
 SCF
EndGravity:
 ret   ;nothing to see here

DrawScreen:       ;This destroys pretty much every register ;)
 ld ix, (LevelAddr)
 ld bc, (PROGRAM_ADDR)
 add ix, bc
 ld de, LBrickMap
 ld b,12 ;13
StartRow:
 ld h,(ix)
 ld l,(ix+1)      ;oh well, might as well pretend the shift byte is useful
 inc ix 
 inc ix
 ld c,b
 ld b,16
StartColumn:
 add hl, hl       ;a good way to shift 16 bits left
 ld a,0
 jr nc, BrickLoop
 ld a,$FF
BrickLoop:
 ld (de),a
 inc de
 djnz StartColumn
 ld b,c
 djnz StartRow
 ret

BlitFromBrickMap:  ;expand BrickMap to playing field
 ld hl,EndoBrickMap  ;first/last row thing
 ld bc,16
 ld de,BrickMap
 ldir
ActualBlit:
 ld hl,BrickMap
 ld de,WorkMap
 ld b,13
BlitRowLoop:
 ld a,3
 push bc
 ld b,0
BlitBrickLoop:
 push hl
 ld c,16
 ldir
 pop hl
 dec a
 jr nz, BlitBrickLoop
 ld c,16
 ldir
 pop bc
 djnz BlitRowLoop
 ret

ExpandHalfByte: ;expand a to (d,e)
 ld c,a
 and %00001111
 add a,a
 add a,a
 add a,7
 ld e,a
 ld a,c
 and %11110000
 srl a
 ld d,a
 ret

SetDeathFlag:
 ld a,(kInvincibility)
 or a
 ret nz
 ld a,(deathFlag)
 or $FF
 ld (deathFlag),a
 ret

BubHorizontal:
 push bc
 srl a
 srl c
 CALL_(CheckTile)
 pop bc
 ret

MakeBubble:
 ld a,(CurRefire)
 or a
 ret nz
DoMakeBubble:
 ld hl,(CurBubbleAddr)
 push bc
DoMakeLoop:          ;this loop may be infinite if kMaxMonsters>=kMaxBubs
 inc hl              ;do NOT allow this!!
 inc hl
 ld a,(hl)
 and %00001100
 jr z,UseBubAddr
 inc hl
 inc hl
 CALL_(ResetBubs)
 jr DoMakeLoop
UseBubAddr:
 dec hl
 dec hl
 ld (CurBubbleAddr), hl
 ld hl,(CurBubbleAddr)
 sla b
 ld (hl),b
 inc hl
 sla c
 ld (hl),c
 inc hl
 ld a,kBubZoom
 bit 1,e
 jr z,ContMakeBubble
 or 1
ContMakeBubble:
 ld (hl),a
 inc hl
 ld (hl),kBubShootTime
 inc hl
 pop bc
SetCounter:
 ld a,(RefillRefire)
 ld (CurRefire),a
PrepareForNext:
 ld (CurBubbleAddr),hl
 CALL_(ResetBubs)
 ret

SetupFile:
 ld bc,SavedGame
 ld hl,(PROGRAM_ADDR)
 add hl,bc
 ld de,lives
 ld bc,5
 ret

NewData:
 ld a,1
 ld (level),a
 ld a,4
 ld (lives),a
 ld hl,LevelDataStart
 ld (LevelAddr),hl
 ret

ResetBubs:
 ld a,(CurBubNum)
 sub 1
 ld (CurBubNum),a
 ret nz
 ld hl,kFirstBubble
 ld (CurBubbleAddr),hl
 ld a,kMaxBubs
 ld (CurBubNum),a
 ret

RefreshIndicator:
 ld a,(lives)
 ld de,$0307
 CALL_(ColinDispNum)
 
 ld a,(level)
 ld de,$0D07
 CALL_(ColinDispNum)

 ld hl,$0007
 ld de,IndicatorString
 CALL_(ColinDisp)

 ld hl,$0A07
 ld de,IndicatorStringTwo
 CALL_(ColinDisp)
 ret

LevelDoneCheck:
 ld hl,kFirstMonster
 ld b,kMaxMonsters
LDMonsterLoop:
 inc hl
 inc hl
 ld a,(hl)
 and %00001111
 ret nz
 inc hl
 djnz LDMonsterLoop
 ld hl,kFirstBubble
 ld b,kMaxBubs
LDBubbleLoop:
 inc hl
 inc hl
 ld a,(hl)
 and %00001110
 ret nz
 inc hl
 inc hl
 djnz LDBubbleLoop
 and 0
 ret

ColinDisp:           ;two-byte value of where to draw text in hl
 ld (CURSOR_ROW),hl  ;two-byte relative address of text in de
 ld hl,(PROGRAM_ADDR)
 add hl,de
 ROM_CALL(D_ZT_STR)
 ret

ColinDispSmall:
 ld (textCursor),hl
 ld hl,(PROGRAM_ADDR)
 add hl,de
 ROM_CALL(D_ZM_STR)
 ret

ColinDispNum: ;a-number de-row/column number
 ld h,0
 ld l,a
 ld (CURSOR_ROW),de
 ROM_CALL(D_HL_DECI)
 ret

Pause:         ;pause until a key is pressed
 call GET_KEY
 cp K_NOKEY
 jr z,Pause
 ret

Delay: ;Delay for hl cycles
 nop
 dec hl
 ld a,h
 or l
 jr nz,Delay
 ret

BlockClearMem: ;clear bc bytes from hl
 ld (hl),0
 inc hl
 dec bc
 ld a,b
 or c
 jr nz,BlockClearMem
 ret

;********** Storage ********* ;permanent program storage
SavedGame:
.db $00      ;lives
.db $00      ;Level Addr
.db $00
.db $00      ;Level
.db $00
;*****************************************************
;                          Strings                    
;*****************************************************
GreetString:
.db "F1:Load",0
GreetStringTwo:
.db "F2:New",0

Sidebar:
.db "Ti-Bobble",0
AuthorString:
.db "by Colin McCabe",0

PauseString:
.db "Paused- "   ;intentional- I can use the string completely or partially
SimplePauseString:
.db "Press Enter",0
Gameover:
.db "Game Over",0

HelpString:
.db "To win, bubble & pop monsters.",0
HOne:
.db "Hold down up to jump"
.db "on bubbles",0
;.db "You can jump on tops "
;.db "of bubbles.",0
HTwo:
.db "Arrows:Move",0
HThree:
.db "Up:Jump",0
HFour:
.db "2nd:Bubble",0
HFive:
.db "More:Pause",0
HSix:
.db "Exit:Save and Quit",0

IndicatorString:
.db "Lives:",0
IndicatorStringTwo:
.db "Level:",0
;**********************************************************
;                        Sprites                           
;**********************************************************
IntroPic:
.db %00000000,%00000001,%00000000
.db %00000000,%00000010,%10000000
.db %00000000,%00000010,%01111000
.db %00000000,%00000010,%11001100
.db %00000000,%00000011,%10000010
.db %00000000,%00011011,%00010001
.db %00000000,%00010110,%00010001
.db %00000000,%00010010,%00000001
.db %00000000,%00010010,%00000110
.db %00000000,%00010110,%00000010
.db %00000000,%11010100,%00001100
.db %00000000,%10101100,%00001000
.db %00000000,%10011000,%00001000
.db %00000000,%10010000,%00001110
.db %00000001,%10010000,%00000001
.db %00000010,%10110000,%01111111
.db %00000010,%01100000,%00001000
.db %00001110,%11000000,%00001000
.db %00010011,%10000000,%00001000
.db %01110110,%00000000,%00001000
.db %10011100,%00000000,%01001000
.db %11110000,%00011111,%11001100
.db %10000000,%11110000,%01000100
.db %11111111,%10000000,%01111100

PlayerLeftSprite:
.db %01110000
.db %11011000
.db %11111000
.db %01111100
.db %00111110
.db %11111110
.db %00111111
.db %01100111

PlayerRightSprite:
.db %00001110
.db %00011011
.db %00011111
.db %00111110
.db %01111100
.db %01111111
.db %11111100
.db %11100110

VoltPic:
.db %00111100
.db %01000010
.db %10101001
.db %10101001
.db %10000001
.db %11111111
.db %01100110
.db %01100110

BoltPic:
.db %00111000
.db %00111000
.db %00111000
.db %00111000
.db %00111100
.db %00011100
.db %00011100
.db %00011100

CogPic:
.db %01111000
.db %11011101
.db %11011101
.db %11111111
.db %01111101
.db %11111101
.db %11111100
.db %01001000

CogRightPic:
.db %00011110
.db %10111011
.db %10111011
.db %11111111
.db %10111110
.db %10111111
.db %00111111
.db %00010010

BelugaPic:
.db %01111000
.db %11111100
.db %11001110
.db %11111110
.db %00111111
.db %11011111
.db %01111111
.db %00011111

BelugaRightPic:
.db %00011110
.db %00111111
.db %01110011
.db %01111111
.db %11111100
.db %11111011
.db %11111110
.db %11111000

RegularBubbleSprite:
.db %00111100
.db %01000010
.db %10011001
.db %10000101
.db %10000101
.db %10000001
.db %01000010
.db %00111100

SmallBubbleSprite:
.db %00000000
.db %00111100
.db %01100110
.db %01001010
.db %01000010
.db %01100110
.db %00111100
.db %00000000

CaptureSprite:
.db %00111100
.db %01110110
.db %11011111
.db %11110101
.db %11011011
.db %11110111
.db %01101110
.db %00111100

;**************************************************************
LevelDataStart: 
;**************************************************************
;24-byte screen data first
;16-byte breeze data
;10-byte monster data
;1-byte player start location data

;**************************************************************
Levels
;**************************************************************
;Ti-bobble levels
;by Colin McCabe

.db %00000000,%00000000
.db %00000000,%00000000
.db %11100000,%00000111
.db %00000000,%00000000
.db %00000000,%00000000
.db %11111100,%00011111
.db %00000000,%00000000
.db %00000000,%11000000
.db %11111100,%00000000
.db %00000000,%00000000
.db %00000000,%01111111
.db %11111111,%11111111

.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111

.db $5A, 2, $0A, 2, 0, 0, $00, 0, $00, 0
.db $E9  ;1b player start location
;----------------------;
.db %00000000,%00000000
.db %00000000,%00000000
.db %01111100,%11111100
.db %00000000,%00000000
.db %00000000,%00000000
.db %01111110,%11111100
.db %00000000,%00000000
.db %00000000,%00000000
.db %00111110,%11111110
.db %00000000,%00000000
.db %00000000,%00000000
.db %11111111,%11111111

.db %01010101,%01010111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11000000,%00000000
.db %11000000,%00000000

.db $11, 1, $A1, 1, $54, 1, $00, 0, $00, 0
.db $FA  ;1b player start location
;----------------------;
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000000,%00000000
.db %00100000,%00000000
.db %11100010,%11111011
.db %00111110,%00000000
.db %10000000,%00000100
.db %00000000,%00000000
.db %10000000,%00000100
.db %00000000,%00000000
.db %11111111,%11111111

.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010

.db $04, 2, $45, 2, $0A, 1, $00, 0, $00, 0
.db $F4  ;1b player start location
;----------------------;
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000000,%10000000
.db %00000001,%01000000
.db %00000011,%11100000
.db %00000100,%00010000
.db %00001111,%11111000
.db %00010000,%00000100
.db %00111111,%11111110
.db %00000000,%00000000
.db %01111111,%11111111

.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111

.db $11, 2, $F1, 1, $86, 1, $F3, 1, $00, 0
.db $5A  ;1b player start location
;----------------------;
.db %00000000,%00000000
.db %00000000,%00000000
.db %11000000,%00000011
.db %00000000,%00000000
.db %00000000,%00000000
.db %11000000,%00000011
.db %00000000,%00000000
.db %00000000,%00000000
.db %11000000,%00000011
.db %00000000,%00000000
.db %00000000,%00000000
.db %11111111,%11111111

.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111

.db $E2, 3, $88, 3, $00, 0, $00, 0, $00, 0
.db $0A  ;1b player start location
;----------------------;
.db %01100100,%01010001
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000001,%00000000
.db %01000001,%10010000
.db %11100101,%10011001
.db %11101101,%11011011
.db %11101101,%11011011
.db %11101101,%11011011
.db %11101101,%11011011

.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010

.db $5A, 2, $56, 1, $85, 1, $B5, 2, $00, 0
.db $05  ;1b player start location
;----------------------;
.db %00000000,%00000000
.db %01101010,%01000000
.db %00101010,%10100000
.db %01101110,%11100000
.db %00100010,%10100000
.db %01100010,%10100000
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000000,%00000000
.db %11111111,%11100000

.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010

.db $F1, 2, $9A, 1, $E1, 1, $A1, 1, $00, 0
.db $0A  ;1b player start location
;----------------------;
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000000,%00011110
.db %00000011,%11000000
.db %11110000,%00011110
.db %00000011,%11000000
.db %11110000,%00000000
.db %00000000,%00000000
.db %00000000,%00000000

.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010

.db $11, 3, $F1, 3, $85, 1, $C6, 1, $00, 0
.db $08  ;1b player start location
;----------------------;
.db %00000000,%00000000
.db %00000000,%00000000
.db %11100000,%00000111
.db %00000011,%11000000
.db %00000000,%00000000
.db %00000000,%00000000
.db %00011111,%11111000
.db %00000000,%00000000
.db %00000000,%00000000
.db %11100011,%11000111
.db %00000000,%00000000
.db %00000000,%00000000

.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111

.db $11, 2, $F1, 2, $85, 1, $08, 1, $00, 0
.db $F8  ;1b player start location
;----------------------;
.db %00000000,%00000000
.db %00000111,%11000000
.db %00001100,%01100000
.db %00011000,%00000000
.db %00010000,%00000000
.db %00010000,%00000000
.db %00010000,%00000000
.db %00011000,%00000000
.db %00001100,%01100000
.db %00000111,%11000000
.db %00000000,%00000000
.db %00011111,%11111000

.db %11110000,%01011111
.db %11110000,%01011111
.db %11110010,%10011111
.db %11110010,%10011111
.db %10100010,%10011010
.db %10100010,%10011010
.db %10100000,%01011010
.db %10100000,%01011010

.db $11, 3, $F1, 3, $88, 1, $A3, 1, $08, 2
.db $AA  ;1b player start location
;----------------------;
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000111,%11100000
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000111,%11100000
.db %00000000,%00000000
.db %00001111,%11110000
.db %00000000,%00000000
.db %00000000,%00000000
.db %00011110,%01111000

.db %10000000,%01010110
.db %10000000,%01010110
.db %10000000,%01010110
.db %10000000,%01010110
.db %10000000,%01010110
.db %10000000,%01010110
.db %10000000,%01010110
.db %10000000,%01010110

.db $11, 3, $95, 1, $97, 1, $99, 1, $F1, 3
.db $39  ;1b player start location
;----------------------;
.db %10111110,%00111111
.db %10101110,%00110011
.db %00000110,%00100000
.db %00000000,%00101100
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000000,%00000000
.db %10100000,%00000001
.db %10111000,%00000111
.db %10111100,%00001111
.db %10111110,%00111111
.db %10111110,%00111111

.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010

.db $06, 2, $F6, 2, $23, 1, $9A, 1, $11, 2
.db $D2  ;1b player start location
;----------------------;
.db %00000000,%00000000
.db %00001111,%00001111
.db %00000000,%00000000
.db %00000000,%00000000
.db %11110000,%11110000
.db %00000000,%00000000
.db %00000000,%00000000
.db %00001111,%00001111
.db %00000000,%00000000
.db %00000000,%00000000
.db %11110000,%11110000
.db %00000000,%00000000

.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111

.db $31, %00000110, $F6, 2, $83, 1, $89, 1, $F9, 3
.db $09  ;1b player start location
;----------------------;
.db %00000000,%00000000
.db %00111011,%10110110
.db %01100010,%10101010
.db %01000011,%10101010
.db %01000010,%00101010
.db %01100010,%00101010
.db %00111010,%00101010
.db %00000000,%00000000
.db %11110000,%00001111
.db %00000000,%00000000
.db %00000000,%00000000
.db %00110011,%00110011

.db %00000011,%00000000
.db %00000011,%00100000
.db %00000011,%01100000
.db %00000011,%01100000
.db %00000011,%01100000
.db %00000011,%01100000
.db %00000011,%01010101
.db %00000011,%01010101

.db $45, 2, $1D, 1, $39, 3, $EA, 3, $12, 1
.db $D7  ;1b player start location
;----------------------;
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000000,%00000000
.db %00010000,%00000000
.db %00000000,%00000000
.db %00000000,%00000100
.db %00000001,%00000000
.db %00000000,%00000000
.db %00000000,%00000000

.db %01101010,%10101000
.db %01101010,%10101000
.db %01101010,%10101000
.db %01101010,%10101000
.db %01101010,%10101000
.db %01101010,%10101000
.db %01101010,%10101000
.db %01101010,%10101000

.db $11, 3, $55, %00000111, $A3, %00000111, $F1, 3, $FA, %00000111
.db $78  ;1b player start location
;----------------------;
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000001,%10000000
.db %10000001,%10000001
.db %00000001,%10000000
.db %00000000,%00000000
.db %10000000,%00000001
.db %00000000,%00000000
.db %00000000,%00000000
.db %11111000,%00011111

.db %01011000,%00000000
.db %10010000,%00100000
.db %10000000,%01100000
.db %10000000,%01100000
.db %10000000,%01100000
.db %10000000,%01100000
.db %01010101,%01100000
.db %01010101,%10101010

.db $3A, 2, $50, 3, $25, %00001011, $A1, 1, $55, 3 ;5 2b monsters
.db $B3  ;1b player start location
;----------------------;
.db %00000000,%00000000
.db %01000000,%11111011
.db %01111111,%10000000
.db %00000000,%00000000
.db %00001000,%00000010
.db %00001111,%11111110
.db %11111000,%00000000
.db %00000000,%00000111
.db %00000000,%00000000
.db %00000000,%00111100
.db %00000000,%00000000
.db %11111111,%11111111

.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111

.db $31, 2, $54, 2, $F4, 2, $05, 2, $98, 1
.db $09  ;1b player start location
;----------------------;
.db %11110000,%00001111
.db %11110000,%00001111
.db %11110011,%11001111
.db %11110000,%00001111
.db %11110000,%00001111
.db %11110001,%11101111
.db %11110000,%00001111
.db %11110000,%00001111
.db %11110111,%10001111
.db %11110000,%00001111
.db %11110000,%00001111
.db %11111111,%11111111

.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111
.db %11111111,%11111111

.db $51, 1, $91, 1, $84, 1, $97, 1, $A1, 1
.db $4A  ;1b player start location
;----------------------;
.db %00000000,%00000000
.db %00011111,%11111111
.db %00000000,%00000000
.db %11111111,%11111000
.db %00000000,%00000000
.db %00011111,%11111111
.db %00000000,%00000000
.db %11111111,%11111000
.db %00000000,%00000000
.db %00011111,%11111111
.db %00000000,%00000000
.db %11111111,%11111000

.db %00000000,%00000000
.db %00000000,%00000000
.db %01010101,%01010101
.db %01010101,%01010101
.db %00000000,%00000000
.db %00000000,%00000000
.db %01010101,%01010101
.db %01010101,%01010101

.db $F1, 2, $E2, 2, $D2, 2, $C2, 2, $B2, 2
.db $06  ;1b player start location
;----------------------;
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000000,%00000000
.db %00111000,%00000000
.db %00000000,%00000000
.db %00000000,%00000011
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000000,%00000000
.db %11111111,%11111111

.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010

.db $83, 1, $83, 1, $83, 1, $83, 1, $83, 1
.db $0A  ;1b player start location
;----------------------;
.db %00000000,%00000000
.db %00000001,%01000000
.db %11111111,%11111111
.db %00000001,%01000000
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000000,%00000000
.db %00000000,%00000000
.db %11111111,%11111111

.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010
.db %10101010,%10101010

.db $01, %00000110, $61, 2, $F1, 2, $B1, %00000110, $D7, 3
.db $0A  ;1b player start location

.end
