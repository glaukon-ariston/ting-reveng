{-# OPTIONS_GHC -fno-warn-orphans #-}
{- 
Script generated code using the follwing script:

import Text.Printf (printf)
template :: String
template = "r%d :: Operand\nr%d = register R%d\n"
mapM_ (\i -> printf template i i i) [0..199]

import Text.Printf (printf)
mapM_ (\i -> printf "R%d |" i) [0..199]

-}
module Ting.Registers where

import Data.List (find)
import Data.Char (toLower)
import Data.Word (Word16)
import Text.Printf (printf)
import Control.Exception (assert)

import Ting.Operand (Operand(..))
import Ting.Common (e2i)

maxRegisterIndex :: Num a => a
maxRegisterIndex = 199

-- Stack
memoryTop :: Num a => a
memoryTop = 99


data Register = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 
    | R16 | R17 | R18 | R19 | R20 | R21 | R22 | R23 | R24 | R25 | R26 | R27 | R28 | R29 | R30 | R31 
    | R32 | R33 | R34 | R35 | R36 | R37 | R38 | R39 | R40 | R41 | R42 | R43 | R44 | R45 | R46 | R47 
    | R48 | R49 | R50 | R51 | R52 | R53 | R54 | R55 | R56 | R57 | R58 | R59 | R60 | R61 | R62 | R63 
    | R64 | R65 | R66 | R67 | R68 | R69 | R70 | R71 | R72 | R73 | R74 | R75 | R76 | R77 | R78 | R79 
    | R80 | R81 | R82 | R83 | R84 | R85 | R86 | R87 | R88 | R89 | R90 | R91 | R92 | R93 | R94 | R95 
    | R96 | R97 | R98 | R99 | R100 | R101 | R102 | R103 | R104 | R105 | R106 | R107 | R108 | R109 | R110 | R111 
    | R112 | R113 | R114 | R115 | R116 | R117 | R118 | R119 | R120 | R121 | R122 | R123 | R124 | R125 | R126 | R127 
    | R128 | R129 | R130 | R131 | R132 | R133 | R134 | R135 | R136 | R137 | R138 | R139 | R140 | R141 | R142 | R143 
    | R144 | R145 | R146 | R147 | R148 | R149 | R150 | R151 | R152 | R153 | R154 | R155 | R156 | R157 | R158 | R159 
    | R160 | R161 | R162 | R163 | R164 | R165 | R166 | R167 | R168 | R169 | R170 | R171 | R172 | R173 | R174 | R175 
    | R176 | R177 | R178 | R179 | R180 | R181 | R182 | R183 | R184 | R185 | R186 | R187 | R188 | R189 | R190 | R191 
    | R192 | R193 | R194 | R195 | R196 | R197 | R198 | R199
    deriving (Show, Read, Enum)

data ScratchRegister = 
    RTIMER | RRANDOM | RTIMEOUT | RTIMEROID | RLASTOID | RVOLUME | RLANG | RLOCK | R_91 | R_90 -- System registers
    | RA | RB | RC | RD | RE | RF  -- Volatile Registers
    | RS | RT | RU | RV | RW | RX | RY | RZ  -- Non-Volatile Registers (Must be preserved by the callee)
    | RSP | RSS -- Stack related registers
    deriving (Show, Read, Enum)


instance Show Operand where
    show r@(R k) = case find (\scratch -> r == scratchRegister scratch) [RTIMER .. ] of
        Just scratch -> map toLower $ (show scratch)
        Nothing -> printf "r%02d" k
    show (I a) = printf "%04x" a
    show (A a) = printf "@%04x" a
    show (P a) = printf "@%s" a
    show (S a) = printf "#%s" a


toRegIndex :: Operand -> Word16
toRegIndex (R r) = r
toRegIndex _ = assert False undefined

register :: Register -> Operand
register r = R $ e2i r

scratchRegister :: ScratchRegister -> Operand
scratchRegister r = R $ memoryTop - e2i r

stackSize :: Num a => a
stackSize = 64

stackTop :: Num a => a
stackTop = memoryTop - e2i RSS - 1

stackBottom :: Num a => a
stackBottom = stackTop - stackSize + 1

ra :: Operand
ra = scratchRegister RA

rb :: Operand
rb = scratchRegister RB

rc :: Operand
rc = scratchRegister RC

rd :: Operand
rd = scratchRegister RD

re :: Operand
re = scratchRegister RE

rf :: Operand
rf = scratchRegister RF

rs :: Operand
rs = scratchRegister RS

rt :: Operand
rt = scratchRegister RT

ru :: Operand
ru = scratchRegister RU

rv :: Operand
rv = scratchRegister RV

rw :: Operand
rw = scratchRegister RW

rx :: Operand
rx = scratchRegister RX

ry :: Operand
ry = scratchRegister RY

rz :: Operand
rz = scratchRegister RZ

rsp :: Operand
rsp = scratchRegister RSP

rss :: Operand
rss = scratchRegister RSS


{-
ReadWrite
Lock/unlock OID selection. When set to 1, it prevents the current playing track from being interrupted. Remains on after the END command.
-}
rlock :: Operand
rlock = scratchRegister RLOCK

{-
ReadOnly
Language code.
-}
rlang :: Operand
rlang = scratchRegister RLANG

{-
ReadWrite
Volume control.
-}
rvolume :: Operand
rvolume = scratchRegister RVOLUME

{-
ReadOnly
Last selected OID.
-}
rlastoid :: Operand
rlastoid = scratchRegister RLASTOID

{-
ReadWrite
The OID to execute on timer interrupt.
-}
rtimeroid :: Operand
rtimeroid = scratchRegister RTIMEROID

{-
ReadWrite
Time out value in tenths of a second. Write to the register to set off timer interrupt. [To be verified]The timer is started after the execution of the current script is finished (the execution reaches either the explicit or implicit END command). It seems that you turn the timer off by setting it to zero, i.e. `set v97,0`
-}
rtimeout :: Operand
rtimeout = scratchRegister RTIMEOUT

{-
ReadOnly
A random value between 0 and 0x7FFF.
-}
rrandom :: Operand
rrandom = scratchRegister RRANDOM

{-
ReadOnly
Current timer value.
-}
rtimer :: Operand
rtimer = scratchRegister RTIMER


-- Script generated code
r0 :: Operand
r0 = register R0
r1 :: Operand
r1 = register R1
r2 :: Operand
r2 = register R2
r3 :: Operand
r3 = register R3
r4 :: Operand
r4 = register R4
r5 :: Operand
r5 = register R5
r6 :: Operand
r6 = register R6
r7 :: Operand
r7 = register R7
r8 :: Operand
r8 = register R8
r9 :: Operand
r9 = register R9
r10 :: Operand
r10 = register R10
r11 :: Operand
r11 = register R11
r12 :: Operand
r12 = register R12
r13 :: Operand
r13 = register R13
r14 :: Operand
r14 = register R14
r15 :: Operand
r15 = register R15
r16 :: Operand
r16 = register R16
r17 :: Operand
r17 = register R17
r18 :: Operand
r18 = register R18
r19 :: Operand
r19 = register R19
r20 :: Operand
r20 = register R20
r21 :: Operand
r21 = register R21
r22 :: Operand
r22 = register R22
r23 :: Operand
r23 = register R23
r24 :: Operand
r24 = register R24
r25 :: Operand
r25 = register R25
r26 :: Operand
r26 = register R26
r27 :: Operand
r27 = register R27
r28 :: Operand
r28 = register R28
r29 :: Operand
r29 = register R29
r30 :: Operand
r30 = register R30
r31 :: Operand
r31 = register R31
r32 :: Operand
r32 = register R32
r33 :: Operand
r33 = register R33
r34 :: Operand
r34 = register R34
r35 :: Operand
r35 = register R35
r36 :: Operand
r36 = register R36
r37 :: Operand
r37 = register R37
r38 :: Operand
r38 = register R38
r39 :: Operand
r39 = register R39
r40 :: Operand
r40 = register R40
r41 :: Operand
r41 = register R41
r42 :: Operand
r42 = register R42
r43 :: Operand
r43 = register R43
r44 :: Operand
r44 = register R44
r45 :: Operand
r45 = register R45
r46 :: Operand
r46 = register R46
r47 :: Operand
r47 = register R47
r48 :: Operand
r48 = register R48
r49 :: Operand
r49 = register R49
r50 :: Operand
r50 = register R50
r51 :: Operand
r51 = register R51
r52 :: Operand
r52 = register R52
r53 :: Operand
r53 = register R53
r54 :: Operand
r54 = register R54
r55 :: Operand
r55 = register R55
r56 :: Operand
r56 = register R56
r57 :: Operand
r57 = register R57
r58 :: Operand
r58 = register R58
r59 :: Operand
r59 = register R59
r60 :: Operand
r60 = register R60
r61 :: Operand
r61 = register R61
r62 :: Operand
r62 = register R62
r63 :: Operand
r63 = register R63
r64 :: Operand
r64 = register R64
r65 :: Operand
r65 = register R65
r66 :: Operand
r66 = register R66
r67 :: Operand
r67 = register R67
r68 :: Operand
r68 = register R68
r69 :: Operand
r69 = register R69
r70 :: Operand
r70 = register R70
r71 :: Operand
r71 = register R71
r72 :: Operand
r72 = register R72
r73 :: Operand
r73 = register R73
r74 :: Operand
r74 = register R74
r75 :: Operand
r75 = register R75
r76 :: Operand
r76 = register R76
r77 :: Operand
r77 = register R77
r78 :: Operand
r78 = register R78
r79 :: Operand
r79 = register R79
r80 :: Operand
r80 = register R80
r81 :: Operand
r81 = register R81
r82 :: Operand
r82 = register R82
r83 :: Operand
r83 = register R83
r84 :: Operand
r84 = register R84
r85 :: Operand
r85 = register R85
r86 :: Operand
r86 = register R86
r87 :: Operand
r87 = register R87
r88 :: Operand
r88 = register R88
r89 :: Operand
r89 = register R89
r90 :: Operand
r90 = register R90
r91 :: Operand
r91 = register R91
r92 :: Operand
r92 = register R92
r93 :: Operand
r93 = register R93
r94 :: Operand
r94 = register R94
r95 :: Operand
r95 = register R95
r96 :: Operand
r96 = register R96
r97 :: Operand
r97 = register R97
r98 :: Operand
r98 = register R98
r99 :: Operand
r99 = register R99
r100 :: Operand
r100 = register R100
r101 :: Operand
r101 = register R101
r102 :: Operand
r102 = register R102
r103 :: Operand
r103 = register R103
r104 :: Operand
r104 = register R104
r105 :: Operand
r105 = register R105
r106 :: Operand
r106 = register R106
r107 :: Operand
r107 = register R107
r108 :: Operand
r108 = register R108
r109 :: Operand
r109 = register R109
r110 :: Operand
r110 = register R110
r111 :: Operand
r111 = register R111
r112 :: Operand
r112 = register R112
r113 :: Operand
r113 = register R113
r114 :: Operand
r114 = register R114
r115 :: Operand
r115 = register R115
r116 :: Operand
r116 = register R116
r117 :: Operand
r117 = register R117
r118 :: Operand
r118 = register R118
r119 :: Operand
r119 = register R119
r120 :: Operand
r120 = register R120
r121 :: Operand
r121 = register R121
r122 :: Operand
r122 = register R122
r123 :: Operand
r123 = register R123
r124 :: Operand
r124 = register R124
r125 :: Operand
r125 = register R125
r126 :: Operand
r126 = register R126
r127 :: Operand
r127 = register R127
r128 :: Operand
r128 = register R128
r129 :: Operand
r129 = register R129
r130 :: Operand
r130 = register R130
r131 :: Operand
r131 = register R131
r132 :: Operand
r132 = register R132
r133 :: Operand
r133 = register R133
r134 :: Operand
r134 = register R134
r135 :: Operand
r135 = register R135
r136 :: Operand
r136 = register R136
r137 :: Operand
r137 = register R137
r138 :: Operand
r138 = register R138
r139 :: Operand
r139 = register R139
r140 :: Operand
r140 = register R140
r141 :: Operand
r141 = register R141
r142 :: Operand
r142 = register R142
r143 :: Operand
r143 = register R143
r144 :: Operand
r144 = register R144
r145 :: Operand
r145 = register R145
r146 :: Operand
r146 = register R146
r147 :: Operand
r147 = register R147
r148 :: Operand
r148 = register R148
r149 :: Operand
r149 = register R149
r150 :: Operand
r150 = register R150
r151 :: Operand
r151 = register R151
r152 :: Operand
r152 = register R152
r153 :: Operand
r153 = register R153
r154 :: Operand
r154 = register R154
r155 :: Operand
r155 = register R155
r156 :: Operand
r156 = register R156
r157 :: Operand
r157 = register R157
r158 :: Operand
r158 = register R158
r159 :: Operand
r159 = register R159
r160 :: Operand
r160 = register R160
r161 :: Operand
r161 = register R161
r162 :: Operand
r162 = register R162
r163 :: Operand
r163 = register R163
r164 :: Operand
r164 = register R164
r165 :: Operand
r165 = register R165
r166 :: Operand
r166 = register R166
r167 :: Operand
r167 = register R167
r168 :: Operand
r168 = register R168
r169 :: Operand
r169 = register R169
r170 :: Operand
r170 = register R170
r171 :: Operand
r171 = register R171
r172 :: Operand
r172 = register R172
r173 :: Operand
r173 = register R173
r174 :: Operand
r174 = register R174
r175 :: Operand
r175 = register R175
r176 :: Operand
r176 = register R176
r177 :: Operand
r177 = register R177
r178 :: Operand
r178 = register R178
r179 :: Operand
r179 = register R179
r180 :: Operand
r180 = register R180
r181 :: Operand
r181 = register R181
r182 :: Operand
r182 = register R182
r183 :: Operand
r183 = register R183
r184 :: Operand
r184 = register R184
r185 :: Operand
r185 = register R185
r186 :: Operand
r186 = register R186
r187 :: Operand
r187 = register R187
r188 :: Operand
r188 = register R188
r189 :: Operand
r189 = register R189
r190 :: Operand
r190 = register R190
r191 :: Operand
r191 = register R191
r192 :: Operand
r192 = register R192
r193 :: Operand
r193 = register R193
r194 :: Operand
r194 = register R194
r195 :: Operand
r195 = register R195
r196 :: Operand
r196 = register R196
r197 :: Operand
r197 = register R197
r198 :: Operand
r198 = register R198
r199 :: Operand
r199 = register R199
