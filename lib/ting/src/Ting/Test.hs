{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}
{-
Author: Glaukon Ariston
Date: 03.08.2016
Abstract:
    A collection of automated test cases.
-}

module Ting.Test where

import Prelude hiding (and, or, not)
import qualified Prelude as P (not)
import Data.Word (Word16)
import Data.Bits (shiftL, shiftR, rotateL, rotateR, complement, (.&.), (.|.))
import qualified Data.Bits as B (setBit, clearBit, popCount)

import Control.Monad (liftM)
import Control.Monad.Fix (MonadFix(..))

import Ting.Instructions
import Ting.Registers
import Ting.Operand (Operand(..))
import Ting.Emulator (Emulator, Flags(..), value, readReg, output, testFlag)
import Ting.Common (i#)


type Test m =  (String, CodeSnippet m (), Emulator Bool)

data TestBattery m = TestBattery
    { _bookId :: Int
    , _tingIdBase :: Int
    , _tests :: [Test m]
    , _library :: Library m ()
    , _soundLib :: SoundLib
    }

{- Check for failure reports like: !stackOverflow, !stackUnderflow, !outOfRange, !fail ... 
They all start with an exclamation mark.
-}
testOutput :: [String] -> Bool
testOutput = P.not . any (\(bang:_) -> bang == '!')

testLibrary :: (MonadFix m, Instructions m) => Program m ()
testLibrary =
    [("__initialise__", SubroutineX, mdo
        nop
    )
    , ("adds_r0_1", SubroutineX, mdo
        adds r0 1
    )
    , ("callid3", SubroutineX, mdo
        callid "adds_r0_1"
        adds r0 10
    )
    , ("callidMutual1", SubroutineX, mdo
        subs r0 1
        cmp r0 0
        je _end
        callid "callidMutual2"
        _end <- label
        nop
    )
    , ("callidMutual2", SubroutineX, mdo
        subs r0 1
        cmp r0 0
        je _end
        callid "callidMutual1"
        _end <- label
        nop
    )
    ]


nativeInstructions :: (MonadFix m, Instructions m) => [Test m]
nativeInstructions = 
    [ ("set_r0_5a5a"
        , mdo
            set r0 0x5a5a
        , do
            r0' <- value r0
            o <- output
            return $ r0' == 0x5a5a && last o == "pass"
    )
    , ("set_rc_0x0"
        , mdo
            set rc 0
        , do
            rc' <- value rc
            o <- output
            return $ rc' == 0 && testOutput o
    )
    , ("set_rc_90"
        , mdo
            set rc 90
        , do
            rc' <- value rc
            o <- output
            return $ rc' == 90 && testOutput o
    )
    , ("adds_rc_0x1"
        , mdo
            adds rc 1
        , do
            o <- output
            return $ testOutput o
    )
    , ("adds_r0_r1"
        , mdo
            set r0 0x1234
            set r1 0x1
            adds r0 r1
        , do
            r0' <- value r0
            o <- output
            return $ r0' == 0x1235 && testOutput o
    )
    , ("subs_rc_1"
        , mdo
            subs rc 1
        , do
            o <- output
            return $ testOutput o
    )
    , ("adds_rc_1_ffff"
        , mdo
            set rc 0xFFFF
            adds rc 1
        , do
            rc' <- value rc
            o <- output
            return $ rc' == 0xffff && testOutput o
    )
    , ("subs_r0_1_0"
        , mdo
            set r0 0
            subs r0 1
        , do
            r0' <- value r0
            o <- output
            return $ r0' == 0 && testOutput o
    )
    , ("and_r0_5A5A"
        , mdo
            set r0 0xffff
            and r0 0x5a5a
        , do
            r0' <- value r0
            return $ r0' == 0xffff .&. 0x5a5a
    )
    , ("or_r0_5A5A"
        , mdo
            set r0 0xa5a5
            or r0 0x5a5a
        , do
            r0' <- value r0
            return $ r0' == 0xa5a5 .|. 0x5a5a
    )
    , ("not_r0_a5a5"
        , mdo
            set r0 0xa5a5
            not r0
        , do
            r0' <- value r0
            return $ r0' == (complement 0xa5a5)
    )
    , ("cmp_zero"
        , mdo
            set r0 0xa5a5
            cmp r0 0xa5a5
        , do
            testFlag ZeroF
    )
    , ("cmp_non_zero"
        , mdo
            set r0 0xa5a5
            cmp r0 0x5a5a
        , do
            liftM P.not (testFlag ZeroF)
    )
    , ("cmp_sign"
        , mdo
            set r0 0x5a5a
            cmp r0 0xa5a5
        , do
            testFlag Sign
    )
    , ("cmp_non_sign"
        , mdo
            set r0 0xa5a5
            cmp r0 0x5a5a
        , do
            liftM P.not (testFlag Sign)
    )
    , ("clearver"
        , mdo
            set r0 0xa5a5
            clearver
        , do
            r0' <- value r0
            return $ r0' == 0
    )
    , ("jmp"
        , mdo
            set r0 0xa5a5
            jmp _skip
            set r0 0x5a5a
            _skip <- label
            nop
        , do
            r0' <- value r0
            return $ r0' == 0xa5a5
    )
    , ("je"
        , mdo
            set r0 0xa5a5
            cmp r0 0xa5a5
            je _skip
            set r0 0x5a5a
            _skip <- label
            nop
        , do
            r0' <- value r0
            return $ r0' == 0xa5a5
    )
    , ("je2"
        , mdo
            set r0 0xa5a5
            cmp r0 0x5a5a
            je _skip
            set r0 0x5a5a
            _skip <- label
            nop
        , do
            r0' <- value r0
            return $ r0' == 0x5a5a
    )
    , ("jne"
        , mdo
            set r0 0xa5a5
            cmp r0 0xa5a5
            jne _skip
            set r0 0x5a5a
            _skip <- label
            nop
        , do
            r0' <- value r0
            return $ r0' == 0x5a5a
    )
    , ("jne2"
        , mdo
            set r0 0xa5a5
            cmp r0 0x5a5a
            jne _skip
            set r0 0x5a5a
            _skip <- label
            nop
        , do
            r0' <- value r0
            return $ r0' == 0xa5a5
    )
    , ("jg"
        , mdo
            set r0 0xa5a5
            cmp r0 0x5a5a
            jg _skip
            set r0 0x5a5a
            _skip <- label
            nop
        , do
            r0' <- value r0
            return $ r0' == 0xa5a5
    )
    , ("jg2"
        , mdo
            set r0 0xa5a5
            cmp r0 0xa5a5
            jg _skip
            set r0 0x5a5a
            _skip <- label
            nop
        , do
            r0' <- value r0
            return $ r0' == 0x5a5a
    )
    , ("jge"
        , mdo
            set r0 0xa5a5
            cmp r0 0xa5a5
            jge _skip
            set r0 0x5a5a
            _skip <- label
            nop
        , do
            r0' <- value r0
            return $ r0' == 0xa5a5
    )
    , ("jge2"
        , mdo
            set r0 0x5a5a
            cmp r0 0xa5a5
            jge _skip
            set r0 0xa5a5
            _skip <- label
            nop
        , do
            r0' <- value r0
            return $ r0' == 0xa5a5
    )
    , ("jb"
        , mdo
            set r0 0x5a5a
            cmp r0 0xa5a5
            jb _skip
            set r0 0xa5a5
            _skip <- label
            nop
        , do
            r0' <- value r0
            return $ r0' == 0x5a5a
    )
    , ("jb2"
        , mdo
            set r0 0xa5a5
            cmp r0 0xa5a5
            jb _skip
            set r0 0x5a5a
            _skip <- label
            nop
        , do
            r0' <- value r0
            return $ r0' == 0x5a5a
    )
    , ("jbe"
        , mdo
            set r0 0xa5a5
            cmp r0 0xa5a5
            jbe _skip
            set r0 0x5a5a
            _skip <- label
            nop
        , do
            r0' <- value r0
            return $ r0' == 0xa5a5
    )
    , ("jbe2"
        , mdo
            set r0 0xa5a5
            cmp r0 0x5a5a
            jbe _skip
            set r0 0x5a5a
            _skip <- label
            nop
        , do
            r0' <- value r0
            return $ r0' == 0x5a5a
    )
    , ("callid"
        , mdo
            set r0 0
            callid "adds_r0_1"
        , do
            r0' <- value r0
            return $ r0' == 1
    )
    , ("callid2"
        , mdo
            set r0 0
            callid "adds_r0_1"
            callid "adds_r0_1"
            adds r0 1
        , do
            r0' <- value r0
            return $ r0' == 3
    )
    , ("callid4"
        , mdo
            set r0 0
            callid "callid3"
        , do
            r0' <- value r0
            return $ r0' == 11
    )
    , ("callidMutual"
        , mdo
            set r0 10
            callid "callidMutual1"
        , do
            r0' <- value r0
            return $ r0' == 0
    )
    {-
    Non-immediate jumps are not supported. (Sigh)
    , ("jmp_indirect"
        , mdo
            set r0 0
            set ra 3
            add ra ra -- x2  <= Assembler2: sizeof add + sizeof end
            add ra _table
            jmp ra

            _table <- label
            add r0 0
            end
            add r0 1
            end
            add r0 2
            end
            add r0 3
            end
            add r0 4
            end
            add r0 5
            end
            add r0 6
            end
            add r0 7
            end
            add r0 8
            end
            add r0 9
            end
        , do
            r0' <- value r0
            return $ r0' == (0+3)
    )
    -}
    ]


macros :: (MonadFix m, Instructions m) => [Test m]
macros = 
    [ ("mdo-shl"
        , mdo
            set r0 0x5a5a
            set rz 1
            _loop <- label
            cmp rz 0
            je _end
            add r0 r0
            sub rz 1
            jmp _loop
            _end <- label
            nop
        , do
            r0' <- value r0
            return $ r0' == (0x5a5a `shiftL` 1)
    ) 
    , ("shl_0x5a5a_1"
        , mdo
            set r0 0x5a5a
            shl r0 1
        , do
            r0' <- value r0
            o <- output
            return $ r0' == (0x5a5a `shiftL` 1) && testOutput o
    )
    , ("shl0"
        , mdo
            set r0 0x5a5a
            shl r0 0
        , do
            r0' <- value r0
            o <- output
            return $ r0' == (0x5a5a `shiftL` 0) && testOutput o
    )
    , ("rol_0xaaaa_1"
        , mdo
            set r0 0xaaaa
            rol r0 1
        , do
            r0' <- value r0
            o <- output
            return $ r0' == (0xaaaa `rotateL` 1) && testOutput o
    )
    , ("rol_0xaaaa_0"
        , mdo
            set r0 0xaaaa
            rol r0 0
        , do
            r0' <- value r0
            o <- output
            return $ r0' == (0xaaaa `rotateL` 0) && testOutput o
    )
    , ("ror_0xa5a5_1"
        , mdo
            set r0 0xa5a5
            ror r0 1
        , do
            r0' <- value r0
            o <- output
            return $ r0' == (0xa5a5 `rotateR` 1) && testOutput o
    )
    , ("ror_0x5a5a_1"
        , mdo
            set r0 0x5a5a
            ror r0 1
        , do
            r0' <- value r0
            o <- output
            return $ r0' == (0x5a5a `rotateR` 1) && testOutput o
    )
    , ("ror_0xa5a5_0"
        , mdo
            set r0 0xa5a5
            ror r0 0
        , do
            r0' <- value r0
            o <- output
            return $ r0' == (0xa5a5 `rotateR` 0) && testOutput o
    )
    , ("shr-prelude"
        , mdo
            let bits = 1
            set ry 0xFFFF
            shl ry bits
            not ry

            set r0 0x5a5a
            and r0 ry
        , do
            r0' <- value r0
            ry' <- value ry
            o <- output
            return $ 
                ry' == complement (0xFFFF `shiftL` 1)
                && r0' == 0x5a5a .&. ry'
                && testOutput o
    )
    , ("shr_0x5a5a_1"
        , mdo
            set r0 0x5a5a
            shr r0 1
        , do
            r0' <- value r0
            o <- output
            return $ r0' == (0x5a5a `shiftR` 1)
                && testOutput o
    )
    , ("shr_0x5a5a_0"
        , mdo
            set r0 0x5a5a
            shr r0 0
        , do
            r0' <- value r0
            o <- output
            return $ r0' == (0x5a5a `shiftR` 0)
                && testOutput o
    )
    , ("testBit_0x5a5a_14"
        , mdo
            set r0 0x5a5a
            testBit r0 14
        , do
            liftM P.not (testFlag ZeroF)
    )
    , ("setBit_0x5a5a_15"
        , mdo
            set r0 0x5a5a
            setBit r0 15
        , do
            r0' <- value r0
            return $ r0' == (B.setBit 0x5a5a 15)
    )
    , ("clearBit_0x5a5a_14"
        , mdo
            set r0 0x5a5a
            clearBit r0 14
        , do
            r0' <- value r0
            return $ r0' == (B.clearBit 0x5a5a 14)
    )
    , ("popCount_ra_0x5a5a"
        , mdo
            popCount r0 0x5a5a
        , do
            r0' <- value r0
            return $ r0' == i# (B.popCount (0x5a5a::Word16))
    )
    , ("countLeadingZeros_ra_0x5a5a"
        , mdo
            countLeadingZeros r0 0x5a5a
        , do
            r0' <- value r0
            return $ r0' == 1 -- (B.countLeadingZeros 0x5a5a)
    )
    , ("countTrailingZeros_ra_0x5a5a"
        , mdo
            countTrailingZeros r0 0x5a5a
        , do
            r0' <- value r0
            return $ r0' == 1 -- (B.countTrailingZeros 0x5a5a)
    )
    , ("divMod_0_4"
        , mdo
            set r0 0
            divModulo r0 4
        , do
            ra' <- value ra
            r0' <- value r0
            return $ (r0',ra') == divMod 0 4
    )
    , ("divMod_10_4"
        , mdo
            set r0 10
            divModulo r0 4
        , do
            ra' <- value ra
            r0' <- value r0
            return $ (r0',ra') == divMod 10 4
    )
    , ("divMod_ffff_fff"
        , mdo
            set r0 0xffff
            divModulo r0 0x0fff
        , do
            ra' <- value ra
            r0' <- value r0
            return $ (r0',ra') == divMod 0xffff 0x0fff
    )
    , ("divMod_0_fff"
        , mdo
            set r0 0xffff
            divModulo r0 0x0fff
            set r0 0
            divModulo r0 0x0fff
        , do
            ra' <- value ra
            r0' <- value r0
            return $ (r0',ra') == divMod 0 0x0fff
    )
    , ("add32_0xffff_0xffff_0_1"
        , mdo
            set r0 0xffff
            set r1 0xffff
            add32 r1 r0 0 1
        , do
            r0' <- value r0
            r1' <- value r1
            return $ (r1',r0') == (0,0)
    )
    , ("add32_0xfffe_0xffff_0_1"
        , mdo
            set r0 0xfffe
            set r1 0xffff
            add32 r1 r0 0 1
        , do
            r0' <- value r0
            r1' <- value r1
            return $ (r1',r0') == (0xffff,0xffff)
    )
    , ("mul2_0xffff_0xffff"
        , mdo
            set r0 0xffff
            set r1 0xffff
            mul2ex r1 r0
        , do
            r0' <- value r0
            r1' <- value r1
            return $ (r1',r0') == (0xffff,0xfffe)
    )
    , ("mul2_1_1"
        , mdo
            set r0 1
            set r1 1
            mul2ex r1 r0
        , do
            r0' <- value r0
            r1' <- value r1
            return $ (r1',r0') == (2,2)
    )
    , ("mul_5_3"
        , mdo
            set r0 5
            set r1 3
            mul r1 r0
        , do
            ra' <- value ra
            r1' <- value r1
            return $ (i# ra', i# r1') == divMod (5*3 :: Int) 0x10000
    )
    , ("mul_0x5a5a_0xa5a5"
        , mdo
            set r0 0x5a5a
            set r1 0xa5a5
            mul r1 r0
        , do
            ra' <- value ra
            r1' <- value r1
            return $ (i# ra', i# r1') == divMod (0x5a5a*0xa5a5 :: Int) 0x10000
    )
    , ("mul_r0_0x5a5a_0xa5a5"
        , mdo
            set r0 0x5a5a
            mul r0 0xa5a5
        , do
            ra' <- value ra
            r0' <- value r0
            return $ (i# ra', i# r0') == divMod (0x5a5a*0xa5a5 :: Int) 0x10000
    )
    , ("mul_0_0xa5a5"
        , mdo
            set r0 0xa5a5
            set r1 0
            mul r1 r0
        , do
            ra' <- value ra
            r1' <- value r1
            return $ (i# ra', i# r1') == divMod (0*0xa5a5 :: Int) 0x10000
    )
    , ("mul_0xa5a5_0"
        , mdo
            set r0 0
            set r1 0xa5a5
            mul r1 r0
        , do
            ra' <- value ra
            r1' <- value r1
            return $ (i# ra', i# r1') == divMod (0*0xa5a5 :: Int) 0x10000
    )
    , ("push_0x5a5a_0xa5a5"
        , mdo
            push 0x5a5a
            push 0xa5a5
        , do
            rsp' <- value rsp
            val0 <- readReg (rsp'+1)
            val1 <- readReg rsp'
            return $ val0 == 0x5a5a && val1 == 0xa5a5
    )
    , ("pushAll[0x5a5a,0xa5a5]"
        , mdo
            pushAll [0x5a5a, 0xa5a5]
        , do
            rsp' <- value rsp
            val0 <- readReg (rsp'+1)
            val1 <- readReg rsp'
            return $ val0 == 0x5a5a && val1 == 0xa5a5
    )
    , ("peek_0x5a5a_0xa5a5"
        , mdo
            push 0x5a5a
            push 0xa5a5
            peek r0
        , do
            rsp' <- value rsp
            val0 <- readReg (rsp'+1)
            val1 <- readReg rsp'
            r0' <- value r0
            return $ val0 == 0x5a5a && val1 == 0xa5a5 && r0' == 0xa5a5
    )
    , ("pop_r0"
        , mdo
            push 0x5a5a
            pop r0
            push 0xa5a5
        , do
            rsp' <- value rsp
            val <- readReg rsp'
            r0' <- value r0
            return $ r0' == 0x5a5a && val == 0xa5a5
    )
    , ("popAll[0x5a5a,0xa5a5]"
        , mdo
            pushAll [0x5a5a, 0xa5a5]
            popAll [r0, r1]
        , do
            r0' <- value r0
            r1' <- value r1
            return $ r0' == 0x5a5a && r1' == 0xa5a5
    )
    , ("speakDigit_a"
        , mdo
            playIndex 0xa (S "0")
        , do
            o <- output
            return $ o == ["a"]
    )
    , ("speakDigit_0_9"
        , mdo
            set rc 0
            playIndex rc (S "0")
            playIndex 9 (S "0")
        , do
            o <- output
            return $ o == ["0", "9"]
    )
    , ("speakHexW8_0xa5"
        , mdo
            speakHex8 0xa5 (S "0")
        , do
            o <- output
            return $ o == ["a", "5"]
    )
    , ("speakHexW16_0x0a5b"
        , mdo
            speakHex 0x0a5b (S "0")
        , do
            o <- output
            return $ o == ["0", "a", "5", "b"]
    )
    , ("speakDecimal_0"
        , mdo
            speakDecimal 0 (S "0")
        , do
            o <- output
            return $ o == ["0"]
    )
    , ("speakDecimal_1"
        , mdo
            speakDecimal 1 (S "0")
        , do
            o <- output
            return $ o == ["1"]
    )
    , ("speakDecimal_12"
        , mdo
            speakDecimal 12 (S "0")
        , do
            o <- output
            return $ o == ["1", "2"]
    )
    , ("speakDecimal_123"
        , mdo
            speakDecimal 123 (S "0")
        , do
            o <- output
            return $ o == ["1", "2", "3"]
    )
    , ("speakDecimal_1234"
        , mdo
            speakDecimal 1234 (S "0")
        , do
            o <- output
            return $ o == ["1", "2", "3", "4"]
    )
    , ("doubleDabble16_65244"
        , mdo
            doubleDabble16 65244
        , do
            ra' <- value ra
            rb' <- value rb
            return $ ra' == 0x0006 && rb' == 0x5244
    )
    , ("doubleDabble16_243"
        , mdo
            doubleDabble16 243
        , do
            ra' <- value ra
            rb' <- value rb
            return $ ra' == 0x0000 && rb' == 0x0243
    )
    , ("doubleDabble16_ffff"
        , mdo
            doubleDabble16 65535
        , do
            ra' <- value ra
            rb' <- value rb
            return $ ra' == 0x0006 && rb' == 0x5535
    )
    , ("doubleDabble16_12345"
        , mdo
            set r0 12345
            doubleDabble16 r0
        , do
            ra' <- value ra
            rb' <- value rb
            return $ ra' == 0x0001 && rb' == 0x2345
    )
    , ("doubleDabble16_0"
        , mdo
            set r0 0
            doubleDabble16 r0
        , do
            ra' <- value ra
            rb' <- value rb
            return $ ra' == 0x0000 && rb' == 0x0000
    )
    , ("dumpSystemRegs#"
        , mdo
            callid "dumpSystemRegs"
        , do
            o <- output
            return $ o /= []
    )
    ]

soundLib :: SoundLib
soundLib = [
    ]

testBattery :: (MonadFix m, Instructions m) => TestBattery m
testBattery = TestBattery 
    { _bookId = 8011
    , _tingIdBase = 15001
    , _tests = nativeInstructions ++ macros
    , _library = prelude ++ testLibrary
    , _soundLib = preludeSoundLib ++ soundLib
    }

