# Special purpose registers

## TODO
- Is v94 (volume control) readable?
- Is v97 (timer) readable? It should be because it is used in `add v97,30`.
- Is this a one-off timer? Or is it a cyclic timer?
- What is the meaning of the current timer value in v99?


Register | Access     | Comment
-------- | ------     | -------
v92      | ReadWrite  | Lock/unlock OID selection. When set to 1, it prevents the current playing track from being interrupted. Remains on after the END command.
v93      | ReadOnly   | Language code.
v94      | Read?Write | Volume control.
v95      | ReadOnly   | Last selected OID.
v96      | ReadOnly   | The OID to execute on timer interrupt.
v97      | Read?Write | Time out value in tenths of a second. Write to the register to set off timer interrupt. [To be verified]The timer is started after the execution of the current script is finished (the execution reaches either the explicit or implicit END command). It seems that you turn the timer off by setting it to zero, i.e. `set v97,0`
v98      | ReadOnly   | A random value between 0 and 0x7FFF.
v99      | ReadOnly   | Current timer value.



## Register v92

Lock/unlock OID recognition. When set to 1, it prevents the current playing track from being interrupted. Remains on after the END command.

### Examples
**00015_en.src**
```
Precode=16006
TYPE=0
[Note]
STOP
[Content]
SET V92,1
SET V59,0
SET V60,0
SET V5,15
AND V5,V4
CMP V5,0
JE STOP_NORMAL
SET V4,0
PLAYOID 31050
SET V92,0
END
:STOP_NORMAL
PLAYOID 31006
SET V92,0
END
```

## Register v93

Language code

### Examples
**00015_en.src**
```
Precode=15018
TYPE=0
[Note]
read out language
[Content]
cmp v93,49
je german
cmp v93,44
je english
cmp v93,33
je french
cmp v93,86
je chinese
playoid 15010
jmp end
:german
playoid 15004
playoid 15009
jmp end
:english
playoid 15004
playoid 15004
jmp end
:french
playoid 15003
playoid 15003
jmp end
:chinese
playoid 15008
playoid 15006
:end
end
```

## Register v94

Volume control

### Examples
**00015_en.src**
```
Precode=15017
TYPE=0
[Note]
set volume
[Content]
set v94,2
playoid 15010
set v94,4
playoid 15001
set v94,6
playoid 15002
set v94,8
playoid 15003
set v94,10
playoid 15004
set v94,12
playoid 15005
set v94,14
playoid 15006
set v94,16
playoid 15007
set v94,18
playoid 15008
set v94,20
playoid 15009
```


## Register v95

Last selected OID.

### Examples
**00015_en.src**
```
Precode=15020
TYPE=0
[Note]
read last digit of oid through subroutine
[Content]
set v1,v95
cmp v1,15019
je nine
cmp v1, 15020
je zero
cmp v1, 15023
je three
:zero
playoid 15010
jmp end
:three
playoid 15003
jmp end
:nine
playoid 15009
:end
return
```


## Register v98

A random value between 0 and 0x7FFF.

### Examples
**00015_en.src**
```
Precode=15011
TYPE=0
[Note]
roll the dice
[Content]
set v1,v98
cmp v1, 5461
jb one
cmp v1, 10922
jb two
cmp v1, 16383
jb three
cmp v1, 21844
jb four
cmp v1, 27305
jb five
jmp six
:one
playoid 15001
jmp end
:two
playoid 15002
jmp end
:three
playoid 15003
jmp end
:four
playoid 15004
jmp end
:five
playoid 15005
jmp end
:six
playoid 15006
jmp end
:end
end
```

## Registers v96, v97 and v99 -- Interrupts

Register | Access     | Comment
-------- | ------     | -------
v96      | ReadOnly   | The OID to execute on timer interrupt.
v97      | Read?Write | Time out value in tenths of a second. Write to the register to set off timer interrupt. [To be verified]The timer is started after the execution of the current script is finished (the execution reaches either the explicit or implicit END command). It seems that you turn the timer off by setting it to zero, i.e. `set v97,0`
v99      | ReadOnly   | Current timer value.

### Examples

**05171_en.src**
```
Precode=48501
TYPE=0
[Note]
[Content]
//CLEAR ALL REGS
CLEARVER
PLAYOID 55359
//SET OID TIMER
SET V96,48502
//GET CURRENT TIMER VALUE
SET V1,V99
//ADD 2 SEC
ADD V1,20
//SET TIMER INTERRUPT
SET V97,V1
END

Precode=48502
TYPE=0
[Note]
[Content]
//CLEAR ALL REGS
CLEARVER
PLAYOID 55802

Precode=49001
TYPE=0
[Note]
CLICKED ON ANSWER 49001
[Content]
CMP V80,2
JNE END
SET V92,1
CMP V25,1
JNE NCONT
:BRIDGE
CMP V40,17200
JE CB0
CMP V40,17201
JE CB1
CMP V40,17202
JE CB2
CMP V40,17203
JE CB3
CMP V40,17204
JE CB4
CMP V40,17205
JE CB5
CMP V40,17206
JE CB6
CMP V40,17207
JE CB7
CMP V40,17208
JE CB8
CMP V40,17209
JE CB9
CMP V40,17210
JE CB10
CMP V40,17211
JE CB11
CMP V40,17212
JE CB12
:CB0
CALL 17200
JMP END
:CB1
CALL 17201
JMP END
:CB2
CALL 17202
JMP END
:CB3
CALL 17203
JMP END
:CB4
CALL 17204
JMP END
:CB5
CALL 17205
JMP END
:CB6
CALL 17206
JMP END
:CB7
CALL 17207
JMP END
:CB8
CALL 17208
JMP END
:CB9
CALL 17209
JMP END
:CB10
CALL 17210
JMP END
:CB11
CALL 17211
JMP END
:CB12
CALL 17212
JMP END
:NCONT
//CHECK FOR CORRECT ANSWER
CMP V87,49001
JE CORRECT_ANS
JMP WRONG_ANS
:CORRECT_ANS
//REINITIALIZE
SET V23,0
//00005_KOPFRECHNEN_RICHTIG_TON.MP3
PLAYOID 55335
SET V21,0
JMP BRIDGE
//JMP END
:WRONG_ANS
//V23 1ST AND 2ND TIME
ADD V23,1
CMP V23,1
JE FIRST_T
CMP V23,2
JE SECOND_T
JMP END
:FIRST_T
//WRONG ANS PROCEDURE FOR 1ST TIME
CALL 17300
//REPEAT Q
PLAYOID V21
SET V92,1
JMP END
:SECOND_T
//00005_KOPFRECHNEN_NEIN.MP3
PLAYOID 55332
CMP V70,2
JE RBYPASS
CMP V70,3
JE RBYPASS
//REPEAT Q
PLAYOID V21
:RBYPASS
PLAYOID V79
PAUSE 10
//NEXT Q ENABLED
SET V21,0
SET V23,0
//PLAY NEXT QUESTION
JMP BRIDGE
:END
END
```


**00015_en.src**
OID | Source Assembly Language | Disassembled Binary Script
--- | ------------------------ | --------------------------
15012 |
```
Precode=15012
TYPE=0
[Note]
start interrupt for count down
[Content]
clearver
set v96,15013
set v97,100
set v1,9
```
|
```
.ORIGIN 0h
.OID 15012
00000000: 01 00           clearver
00000002: 02 01 0060 3AA5 set v96,15013
00000008: 02 01 0061 0064 set v97,100
0000000E: 02 01 0001 0009 set v1,9
00000014: 00                end
```

15013 |
```
Precode=15013
TYPE=0
[Note]
count down by interrupt
[Content]
cmp v1,9
je nine
cmp v1,8
je eight
cmp v1,7
je seven
cmp v1,6
je six
cmp v1,5
je five
cmp v1,4
je four
cmp v1,3
je three
cmp v1,2
je two
cmp v1,1
je one
cmp v1, 0
je zero
jmp minusone
:nine
playoid 15009
jmp minusone
:eight
playoid 15008
jmp minusone
:seven
playoid 15007
jmp minusone
:six
playoid 15006
jmp minusone
:five
playoid 15005
jmp minusone
:four
playoid 15004
jmp minusone
:three
playoid 15003
jmp minusone
:two
playoid 15002
jmp minusone
:one
playoid 15001
jmp minusone
:zero
playoid 15010
set v97,0
jmp end
:minusone
add v97,30
sub v1,1
:end
end
```
|
```
.ORIGIN 0h
.OID 15013
00000000: 03 01 0001 0009 cmp v1,9
00000006: 09 00 0068      je 68
0000000A: 03 01 0001 0008 cmp v1,8
00000010: 09 00 0070      je 70
00000014: 03 01 0001 0007 cmp v1,7
0000001A: 09 00 0078      je 78
0000001E: 03 01 0001 0006 cmp v1,6
00000024: 09 00 0080      je 80
00000028: 03 01 0001 0005 cmp v1,5
0000002E: 09 00 0088      je 88
00000032: 03 01 0001 0004 cmp v1,4
00000038: 09 00 0090      je 90
0000003C: 03 01 0001 0003 cmp v1,3
00000042: 09 00 0098      je 98
00000046: 03 01 0001 0002 cmp v1,2
0000004C: 09 00 00A0      je A0
00000050: 03 01 0001 0001 cmp v1,1
00000056: 09 00 00A8      je A8
0000005A: 03 01 0001 0000 cmp v1,0
00000060: 09 00 00B0      je B0
00000064: 08 00 00BE      jmp BE
00000068: 16 01 3AA1      playoid 15009
0000006C: 08 00 00BE      jmp BE
00000070: 16 01 3AA0      playoid 15008
00000074: 08 00 00BE      jmp BE
00000078: 16 01 3A9F      playoid 15007
0000007C: 08 00 00BE      jmp BE
00000080: 16 01 3A9E      playoid 15006
00000084: 08 00 00BE      jmp BE
00000088: 16 01 3A9D      playoid 15005
0000008C: 08 00 00BE      jmp BE
00000090: 16 01 3A9C      playoid 15004
00000094: 08 00 00BE      jmp BE
00000098: 16 01 3A9B      playoid 15003
0000009C: 08 00 00BE      jmp BE
000000A0: 16 01 3A9A      playoid 15002
000000A4: 08 00 00BE      jmp BE
000000A8: 16 01 3A99      playoid 15001
000000AC: 08 00 00BE      jmp BE
000000B0: 16 01 3AA2      playoid 15010
000000B4: 02 01 0061 0000 set v97,0
000000BA: 08 00 00CA      jmp CA
000000BE: 0F 01 0061 001E add v97,30
000000C4: 10 01 0001 0001 sub v1,1
000000CA: 00                end
000000CC: 00                end
```

15021 |
```
Precode=15021
TYPE=0
[Note]
save variable 1 over switch off and then  play 0
[Content]
set v1,1
set v96,1
playoid 15010
```
|
```
.ORIGIN 0h
.OID 15021
00000000: 02 01 0001 0001 set v1,1
00000006: 02 01 0060 0001 set v96,1
0000000C: 16 01 3AA2      playoid 15010
00000010: 00                end
```

15022 |
```
Precode=15022
TYPE=0
[Note]
check variable 1
[Content]
cmp v1,1
je one
cmp v1,0
je zero
jmp end
:zero
playoid 15010
jmp end
:one
playoid 15001
:end
set v96,0
end
```
|
```
.ORIGIN 0h
.OID 15022
00000000: 03 01 0001 0001 cmp v1,1
00000006: 09 00 0020      je 20
0000000A: 03 01 0001 0000 cmp v1,0
00000010: 09 00 0018      je 18
00000014: 08 00 0024      jmp 24
00000018: 16 01 3AA2      playoid 15010
0000001C: 08 00 0024      jmp 24
00000020: 16 01 3A99      playoid 15001
00000024: 02 01 0060 0000 set v96,0
0000002A: 00                end
0000002C: 00                end
```


