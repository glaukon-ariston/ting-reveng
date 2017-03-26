# ting-reveng
Ting pen development framework as an eDSL in Haskell

## Setup

Install [Haskell Platform](https://www.haskell.org/platform/).

## Build

    ting-reveng> stack build
    ting-0.1.0.0: configure
    Configuring ting-0.1.0.0...
    ting-0.1.0.0: build
    Preprocessing library ting-0.1.0.0...
    [ 1 of 10] Compiling Ting.Common      ( src\Ting\Common.hs, .stack-work\dist\2fae85dd\build\Ting\Common.o )
    [ 2 of 10] Compiling Ting.Operand     ( src\Ting\Operand.hs, .stack-work\dist\2fae85dd\build\Ting\Operand.o )
    [ 3 of 10] Compiling Ting.Registers   ( src\Ting\Registers.hs, .stack-work\dist\2fae85dd\build\Ting\Registers.o )
    [ 4 of 10] Compiling Ting.Instructions ( src\Ting\Instructions.hs, .stack-work\dist\2fae85dd\build\Ting\Instructions.o )
    [ 5 of 10] Compiling Ting.Assembler   ( src\Ting\Assembler.hs, .stack-work\dist\2fae85dd\build\Ting\Assembler.o )
    [ 6 of 10] Compiling Ting.Assembler2  ( src\Ting\Assembler2.hs, .stack-work\dist\2fae85dd\build\Ting\Assembler2.o )
    [ 7 of 10] Compiling Ting.Emulator    ( src\Ting\Emulator.hs, .stack-work\dist\2fae85dd\build\Ting\Emulator.o )
    [ 8 of 10] Compiling Ting.Test        ( src\Ting\Test.hs, .stack-work\dist\2fae85dd\build\Ting\Test.o )
    [ 9 of 10] Compiling Ting.Linker      ( src\Ting\Linker.hs, .stack-work\dist\2fae85dd\build\Ting\Linker.o )
    [10 of 10] Compiling Ting.Repl        ( src\Ting\Repl.hs, .stack-work\dist\2fae85dd\build\Ting\Repl.o )
    Preprocessing executable 'ting-calc' for ting-0.1.0.0...
    [1 of 1] Compiling Main             ( apps\calculator\Main.hs, .stack-work\dist\2fae85dd\build\ting-calc\ting-calc-tmp\Main.o )
    Linking .stack-work\dist\2fae85dd\build\ting-calc\ting-calc.exe ...
    Preprocessing executable 'ting-emu' for ting-0.1.0.0...
    [1 of 1] Compiling Main             ( apps\generic\Main.hs, .stack-work\dist\2fae85dd\build\ting-emu\ting-emu-tmp\Main.o )
    Linking .stack-work\dist\2fae85dd\build\ting-emu\ting-emu.exe ...
    ting-0.1.0.0: copy/register
    Installing library in
    H:\app\dev\hp\projects\github\ting-reveng\.stack-work\install\38bb855b\lib\i386-windows-ghc-8.0.1\ting-0.1.0.0-CKC7CdzRlEzF8CzDUjAf3h
    Installing executable(s) in
    ting-reveng\.stack-work\install\38bb855b\bin
    Registering ting-0.1.0.0...

    ting-reveng>

## Execute

    ting-reveng> stack exec ting-calc
    Ting Calculator
    Making book '8001' (resourceDir='books') ...
    Linking books\08001\08001_en.ouf (bookID 8001, tingIdBase 15001) ...
            Assembled actions #60
            Sounds #368
    Book description:
        Name: Ting Calculator
        Publisher: Glaukon Ariston
        Author: Glaukon Ariston
        Book Version: 1
        URL: http://github.com
        ThumbMD5: 7ce755daa2c7ff6c7e7b3c47167191da
        FileMD5: 6e78ed4efc0a094a4f01a2ad17565560
        ScriptMD5:
        Book Area Code: en
    Building map file books\08001\08001_en.map ...
    Copying files to J:/$ting ...
    08001_en.ouf
    08001_en.png
    08001_en.txt
    08001_en.map
    Running REPL for book 8001 (tingBaseId 15001). Type `help` for help.
    repl> help
    ProgramInfo
      dasm        tingId | proc name               Disassemble the specified procedure
      symbol      tingId | proc name | sound name  Show symbol info
      procedures  <None>                           Show the names of all procedures with their associated tingId
      sounds      <None>                           Show the names of all sounds with their associated tingId

    Tools
      radix  i  Convert the supplied number to different number bases using the Word16 container

    Registers
      read   r      Show the current value of the register r
      write  r val  Set the value of the register r

    Execution
      eval       tingId | proc name  Execute the procedure
      here       <None>              Show the current context
      callStack  <None>              Show the call stack

    Debugging
      debug     tingId | proc name  Prepare the procedure for execution
      step      [n]                 'step into', execute the next 'n' instructions (step into in case of callid)
      next      [n]                 'step over', execute the next 'n' instructions (step over in case of callid)
      continue  <None>              Continue execution until the program ends or it hits a breakpoint

    Breakpoints
      break  (tingId|proc name):address +|-  Break execution at the specified location
      watch  register r|w|rw +|-             Break execution when access on register is detected
      show   break|watch                     Show the currently set breakpoints/watches

    Miscellaneous
      help     [cmd]   Show help
      quit     <None>  Exit the repl
      <ENTER>  <None>  Repeat the last command


    repl> procedures
    @15001 0x3a99 0
    @15002 0x3a9a 1
    @15003 0x3a9b 2
    @15004 0x3a9c 3
    @15005 0x3a9d 4
    @15006 0x3a9e 5
    @15007 0x3a9f 6
    @15008 0x3aa0 7
    @15009 0x3aa1 8
    @15010 0x3aa2 9
    @15011 0x3aa3 __initialise__
    @15012 0x3aa4 +
    @15013 0x3aa5 -
    @15014 0x3aa6 *
    @15015 0x3aa7 /
    @15016 0x3aa8 =
    @15017 0x3aa9 C
    @15018 0x3aaa CE
    @15019 0x3aab LCD
    @15020 0x3aac .
    @15021 0x3aad ON-OFF
    @15022 0x3aae L.
    @15023 0x3aaf V.
    @15024 0x3ab0 M.
    @15025 0x3ab1 T.
    @15026 0x3ab2 M2.
    @15027 0x3ab3 N2.
    @15028 0x3ab4 initStack
    @15029 0x3ab5 boot
    @15030 0x3ab6 set_rc_0
    @15031 0x3ab7 inc_rc
    @15032 0x3ab8 add_rc_100
    @15033 0x3ab9 dec_rc
    @15034 0x3aba sub_rc_100
    @15035 0x3abb set_rd_0
    @15036 0x3abc inc_rd
    @15037 0x3abd add_rd_100
    @15038 0x3abe dec_rd
    @15039 0x3abf sub_rd_100
    @15040 0x3ac0 mutual1
    @15041 0x3ac1 mutual2
    @15042 0x3ac2 recurse
    @15043 0x3ac3 mutual1Play
    @15044 0x3ac4 mutual2Play
    @15045 0x3ac5 recursePlay
    @15046 0x3ac6 lastOid1
    @15047 0x3ac7 lastOid2
    @15048 0x3ac8 sanityCheck
    @15049 0x3ac9 sanityCheck2
    @15050 0x3aca sanityCheck3
    @15051 0x3acb readReg
    @15052 0x3acc writeReg
    @15053 0x3acd push_ss
    @15054 0x3ace peek_ss
    @15055 0x3acf speakRegister
    @15056 0x3ad0 dumpSystemRegs
    @15057 0x3ad1 opCode07
    @15058 0x3ad2 opCode11
    @15059 0x3ad3 opCode12
    @15060 0x3ad4 opCode13

    repl> sounds
    #15061 0x3ad5 L.00
    #15062 0x3ad6 L.01
    #15063 0x3ad7 L.02
    #15064 0x3ad8 L.03
    #15065 0x3ad9 L.04
    #15066 0x3ada L.05
    #15067 0x3adb L.06
    #15068 0x3adc L.07
    #15069 0x3add L.08
    #15070 0x3ade L.09
    #15071 0x3adf L.10
    #15072 0x3ae0 L.11
    #15073 0x3ae1 L.12
    #15074 0x3ae2 L.13
    #15075 0x3ae3 L.14
    #15076 0x3ae4 L.15
    #15077 0x3ae5 L.16
    #15078 0x3ae6 L.17
    #15079 0x3ae7 L.18
    #15080 0x3ae8 L.19
    #15081 0x3ae9 L.20
    #15082 0x3aea L.30
    #15083 0x3aeb L.40
    #15084 0x3aec L.50
    #15085 0x3aed L.60
    #15086 0x3aee L.70
    #15087 0x3aef L.80
    #15088 0x3af0 L.90
    #15089 0x3af1 L.100
    #15090 0x3af2 L.200
    #15091 0x3af3 L.300
    #15092 0x3af4 L.400
    #15093 0x3af5 L.500
    #15094 0x3af6 L.600
    #15095 0x3af7 L.700
    #15096 0x3af8 L.800
    #15097 0x3af9 L.900
    #15098 0x3afa L.1000
    #15099 0x3afb L.1000p
    #15100 0x3afc L.1e6
    #15101 0x3afd L.1e6p
    #15102 0x3afe L.1e9
    #15103 0x3aff L.1e9p
    #15104 0x3b00 L.1e12
    #15105 0x3b01 L.1e12p
    #15106 0x3b02 L.01f
    #15107 0x3b03 L.02f
    #15108 0x3b04 L.+
    #15109 0x3b05 L.-
    #15110 0x3b06 L.*
    #15111 0x3b07 L./
    #15112 0x3b08 L.=
    #15113 0x3b09 L.C
    #15114 0x3b0a L.CE
    #15115 0x3b0b L.!error
    #15116 0x3b0c L.click
    #15117 0x3b0d L.speaker
    #15118 0x3b0e V.00
    #15119 0x3b0f V.01
    #15120 0x3b10 V.02
    #15121 0x3b11 V.03
    #15122 0x3b12 V.04
    #15123 0x3b13 V.05
    #15124 0x3b14 V.06
    #15125 0x3b15 V.07
    #15126 0x3b16 V.08
    #15127 0x3b17 V.09
    #15128 0x3b18 V.10
    #15129 0x3b19 V.11
    #15130 0x3b1a V.12
    #15131 0x3b1b V.13
    #15132 0x3b1c V.14
    #15133 0x3b1d V.15
    #15134 0x3b1e V.16
    #15135 0x3b1f V.17
    #15136 0x3b20 V.18
    #15137 0x3b21 V.19
    #15138 0x3b22 V.20
    #15139 0x3b23 V.30
    #15140 0x3b24 V.40
    #15141 0x3b25 V.50
    #15142 0x3b26 V.60
    #15143 0x3b27 V.70
    #15144 0x3b28 V.80
    #15145 0x3b29 V.90
    #15146 0x3b2a V.100
    #15147 0x3b2b V.200
    #15148 0x3b2c V.300
    #15149 0x3b2d V.400
    #15150 0x3b2e V.500
    #15151 0x3b2f V.600
    #15152 0x3b30 V.700
    #15153 0x3b31 V.800
    #15154 0x3b32 V.900
    #15155 0x3b33 V.1000
    #15156 0x3b34 V.1000p
    #15157 0x3b35 V.1e6
    #15158 0x3b36 V.1e6p
    #15159 0x3b37 V.1e9
    #15160 0x3b38 V.1e9p
    #15161 0x3b39 V.1e12
    #15162 0x3b3a V.1e12p
    #15163 0x3b3b V.01f
    #15164 0x3b3c V.02f
    #15165 0x3b3d V.+
    #15166 0x3b3e V.-
    #15167 0x3b3f V.*
    #15168 0x3b40 V./
    #15169 0x3b41 V.=
    #15170 0x3b42 V.C
    #15171 0x3b43 V.CE
    #15172 0x3b44 V.!error
    #15173 0x3b45 V.click
    #15174 0x3b46 V.speaker
    #15175 0x3b47 M.00
    #15176 0x3b48 M.01
    #15177 0x3b49 M.02
    #15178 0x3b4a M.03
    #15179 0x3b4b M.04
    #15180 0x3b4c M.05
    #15181 0x3b4d M.06
    #15182 0x3b4e M.07
    #15183 0x3b4f M.08
    #15184 0x3b50 M.09
    #15185 0x3b51 M.10
    #15186 0x3b52 M.11
    #15187 0x3b53 M.12
    #15188 0x3b54 M.13
    #15189 0x3b55 M.14
    #15190 0x3b56 M.15
    #15191 0x3b57 M.16
    #15192 0x3b58 M.17
    #15193 0x3b59 M.18
    #15194 0x3b5a M.19
    #15195 0x3b5b M.20
    #15196 0x3b5c M.30
    #15197 0x3b5d M.40
    #15198 0x3b5e M.50
    #15199 0x3b5f M.60
    #15200 0x3b60 M.70
    #15201 0x3b61 M.80
    #15202 0x3b62 M.90
    #15203 0x3b63 M.100
    #15204 0x3b64 M.200
    #15205 0x3b65 M.300
    #15206 0x3b66 M.400
    #15207 0x3b67 M.500
    #15208 0x3b68 M.600
    #15209 0x3b69 M.700
    #15210 0x3b6a M.800
    #15211 0x3b6b M.900
    #15212 0x3b6c M.1000
    #15213 0x3b6d M.1000p
    #15214 0x3b6e M.1e6
    #15215 0x3b6f M.1e6p
    #15216 0x3b70 M.1e9
    #15217 0x3b71 M.1e9p
    #15218 0x3b72 M.1e12
    #15219 0x3b73 M.1e12p
    #15220 0x3b74 M.01f
    #15221 0x3b75 M.02f
    #15222 0x3b76 M.+
    #15223 0x3b77 M.-
    #15224 0x3b78 M.*
    #15225 0x3b79 M./
    #15226 0x3b7a M.=
    #15227 0x3b7b M.C
    #15228 0x3b7c M.CE
    #15229 0x3b7d M.!error
    #15230 0x3b7e M.click
    #15231 0x3b7f M.speaker
    #15232 0x3b80 T.00
    #15233 0x3b81 T.01
    #15234 0x3b82 T.02
    #15235 0x3b83 T.03
    #15236 0x3b84 T.04
    #15237 0x3b85 T.05
    #15238 0x3b86 T.06
    #15239 0x3b87 T.07
    #15240 0x3b88 T.08
    #15241 0x3b89 T.09
    #15242 0x3b8a T.10
    #15243 0x3b8b T.11
    #15244 0x3b8c T.12
    #15245 0x3b8d T.13
    #15246 0x3b8e T.14
    #15247 0x3b8f T.15
    #15248 0x3b90 T.16
    #15249 0x3b91 T.17
    #15250 0x3b92 T.18
    #15251 0x3b93 T.19
    #15252 0x3b94 T.20
    #15253 0x3b95 T.30
    #15254 0x3b96 T.40
    #15255 0x3b97 T.50
    #15256 0x3b98 T.60
    #15257 0x3b99 T.70
    #15258 0x3b9a T.80
    #15259 0x3b9b T.90
    #15260 0x3b9c T.100
    #15261 0x3b9d T.200
    #15262 0x3b9e T.300
    #15263 0x3b9f T.400
    #15264 0x3ba0 T.500
    #15265 0x3ba1 T.600
    #15266 0x3ba2 T.700
    #15267 0x3ba3 T.800
    #15268 0x3ba4 T.900
    #15269 0x3ba5 T.1000
    #15270 0x3ba6 T.1000p
    #15271 0x3ba7 T.1e6
    #15272 0x3ba8 T.1e6p
    #15273 0x3ba9 T.1e9
    #15274 0x3baa T.1e9p
    #15275 0x3bab T.1e12
    #15276 0x3bac T.1e12p
    #15277 0x3bad T.01f
    #15278 0x3bae T.02f
    #15279 0x3baf T.+
    #15280 0x3bb0 T.-
    #15281 0x3bb1 T.*
    #15282 0x3bb2 T./
    #15283 0x3bb3 T.=
    #15284 0x3bb4 T.C
    #15285 0x3bb5 T.CE
    #15286 0x3bb6 T.!error
    #15287 0x3bb7 T.click
    #15288 0x3bb8 T.speaker
    #15289 0x3bb9 M2.00
    #15290 0x3bba M2.01
    #15291 0x3bbb M2.02
    #15292 0x3bbc M2.03
    #15293 0x3bbd M2.04
    #15294 0x3bbe M2.05
    #15295 0x3bbf M2.06
    #15296 0x3bc0 M2.07
    #15297 0x3bc1 M2.08
    #15298 0x3bc2 M2.09
    #15299 0x3bc3 M2.10
    #15300 0x3bc4 M2.11
    #15301 0x3bc5 M2.12
    #15302 0x3bc6 M2.13
    #15303 0x3bc7 M2.14
    #15304 0x3bc8 M2.15
    #15305 0x3bc9 M2.16
    #15306 0x3bca M2.17
    #15307 0x3bcb M2.18
    #15308 0x3bcc M2.19
    #15309 0x3bcd M2.20
    #15310 0x3bce M2.30
    #15311 0x3bcf M2.40
    #15312 0x3bd0 M2.50
    #15313 0x3bd1 M2.60
    #15314 0x3bd2 M2.70
    #15315 0x3bd3 M2.80
    #15316 0x3bd4 M2.90
    #15317 0x3bd5 M2.100
    #15318 0x3bd6 M2.200
    #15319 0x3bd7 M2.300
    #15320 0x3bd8 M2.400
    #15321 0x3bd9 M2.500
    #15322 0x3bda M2.600
    #15323 0x3bdb M2.700
    #15324 0x3bdc M2.800
    #15325 0x3bdd M2.900
    #15326 0x3bde M2.1000
    #15327 0x3bdf M2.1000p
    #15328 0x3be0 M2.1e6
    #15329 0x3be1 M2.1e6p
    #15330 0x3be2 M2.1e9
    #15331 0x3be3 M2.1e9p
    #15332 0x3be4 M2.1e12
    #15333 0x3be5 M2.1e12p
    #15334 0x3be6 M2.01f
    #15335 0x3be7 M2.02f
    #15336 0x3be8 M2.+
    #15337 0x3be9 M2.-
    #15338 0x3bea M2.*
    #15339 0x3beb M2./
    #15340 0x3bec M2.=
    #15341 0x3bed M2.C
    #15342 0x3bee M2.CE
    #15343 0x3bef M2.!error
    #15344 0x3bf0 M2.click
    #15345 0x3bf1 M2.speaker
    #15346 0x3bf2 N2.00
    #15347 0x3bf3 N2.01
    #15348 0x3bf4 N2.02
    #15349 0x3bf5 N2.03
    #15350 0x3bf6 N2.04
    #15351 0x3bf7 N2.05
    #15352 0x3bf8 N2.06
    #15353 0x3bf9 N2.07
    #15354 0x3bfa N2.08
    #15355 0x3bfb N2.09
    #15356 0x3bfc N2.10
    #15357 0x3bfd N2.11
    #15358 0x3bfe N2.12
    #15359 0x3bff N2.13
    #15360 0x3c00 N2.14
    #15361 0x3c01 N2.15
    #15362 0x3c02 N2.16
    #15363 0x3c03 N2.17
    #15364 0x3c04 N2.18
    #15365 0x3c05 N2.19
    #15366 0x3c06 N2.20
    #15367 0x3c07 N2.30
    #15368 0x3c08 N2.40
    #15369 0x3c09 N2.50
    #15370 0x3c0a N2.60
    #15371 0x3c0b N2.70
    #15372 0x3c0c N2.80
    #15373 0x3c0d N2.90
    #15374 0x3c0e N2.100
    #15375 0x3c0f N2.200
    #15376 0x3c10 N2.300
    #15377 0x3c11 N2.400
    #15378 0x3c12 N2.500
    #15379 0x3c13 N2.600
    #15380 0x3c14 N2.700
    #15381 0x3c15 N2.800
    #15382 0x3c16 N2.900
    #15383 0x3c17 N2.1000
    #15384 0x3c18 N2.1000p
    #15385 0x3c19 N2.1e6
    #15386 0x3c1a N2.1e6p
    #15387 0x3c1b N2.1e9
    #15388 0x3c1c N2.1e9p
    #15389 0x3c1d N2.1e12
    #15390 0x3c1e N2.1e12p
    #15391 0x3c1f N2.01f
    #15392 0x3c20 N2.02f
    #15393 0x3c21 N2.+
    #15394 0x3c22 N2.-
    #15395 0x3c23 N2.*
    #15396 0x3c24 N2./
    #15397 0x3c25 N2.=
    #15398 0x3c26 N2.C
    #15399 0x3c27 N2.CE
    #15400 0x3c28 N2.!error
    #15401 0x3c29 N2.click
    #15402 0x3c2a N2.speaker
    #15403 0x3c2b !stackOverflow
    #15404 0x3c2c !stackUnderflow
    #15405 0x3c2d !outOfRange
    #15406 0x3c2e line
    #15407 0x3c2f register
    #15408 0x3c30 value
    #15409 0x3c31 equals
    #15410 0x3c32 sanityCheck
    #15411 0x3c33 pass
    #15412 0x3c34 !fail
    #15413 0x3c35 0
    #15414 0x3c36 1
    #15415 0x3c37 2
    #15416 0x3c38 3
    #15417 0x3c39 4
    #15418 0x3c3a 5
    #15419 0x3c3b 6
    #15420 0x3c3c 7
    #15421 0x3c3d 8
    #15422 0x3c3e 9
    #15423 0x3c3f a
    #15424 0x3c40 b
    #15425 0x3c41 c
    #15426 0x3c42 d
    #15427 0x3c43 e
    #15428 0x3c44 f

    repl> quit
    Exiting now...

