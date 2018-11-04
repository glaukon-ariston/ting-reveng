{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE MagicHash #-}
{-
Author: Glaukon Ariston
Date: 11.07.2016
Abstract:

    Ting Pen -- Assembly Languge Instructions

enum <uint8> OpCode {
      END       = 0x00
    , CLEARVER  = 0x01
    , SET       = 0x02
    , CMP       = 0x03
    , AND       = 0x04
    , OR        = 0x05
    , NOT       = 0x06

    , JMP       = 0x08
    , JE        = 0x09
    , JNE       = 0x0A
    , JG        = 0x0B
    , JGE       = 0x0C
    , JB        = 0x0D
    , JBE       = 0x0E
    , ADD       = 0x0F
    , SUB       = 0x10

    , RETURN    = 0x14
    , CALLID    = 0x15
    , PLAYOID   = 0x16
    , PAUSE     = 0x17
    , OpCode_LAST = 0x19
};

enum <uint8> OperandType {
      OPERAND_NONE      = 0x00
    , REGISTER_VALUE    = 0x01
    , REGISTER_REGISTER = 0x02
    , OperandType_LAST = 0x02
};

Register Usage
https://msdn.microsoft.com/en-us/library/9z1stfyw.aspx
-}

module Ting.Instructions where

import Prelude hiding (and, or, not)
import Data.Word (Word16)
import Data.Bits (shiftL, shiftR, bit, complement)
import qualified Data.Bits as B (testBit)
import Data.List (findIndices)
import Control.Exception (assert)
import Control.Monad (forM_, replicateM_, when)
import Control.Monad.Fix (MonadFix(..))
import Text.Printf (printf)

import Ting.Operand
import Ting.Registers
import Ting.Common (i#, e2i)


data NumbersEn = 
    Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
    | Ten | Eleven | Twelve | Thirteen | Fourteen | Fifteen | Sixteen | Seventeen | Eighteen | Nineteen
    | Twenty | Thirty | Fourty | Fifty | Sixty | Seventy | Eighty | Ninety
    | Hundred | Thousand | Million | Billion 
    deriving (Enum)

data NumbersHr = 
    Nula | Jedan | Dva | Tri | Cetiri | Pet | Sest | Sedam | Osam | Devet
    | Deset | Jedanaest | Dvanaest | Trinaest | Cetrnaest | Petnaest | Sesnaest | Sedamnaest | Osamnaest | Devetnaest
    | Dvadeset | Trideset | Cetrdeset | Pedeset | Sezdeset | Sedamdeset | Osamdeset | Devedeset
    | Sto | Dvjesto | Tristo | Cetristo | Petsto | Sesto | Sedamsto | Osamsto | Devesto
    | Tisuca | Tisuce 
    | Milijun | Milijuna 
    | Milijarda | Milijarde 
    | Bilijun | Bilijuna
    | Jedna | Dvije 
    deriving (Enum)

data Gender = Male | Female deriving (Enum)


#define UNDEFINED1(name) (error . printf "undefined: " ++ name ++ " %s" . show)
#define UNDEFINED2(name) (\x y -> error $ printf "undefined: " ++ name ++ " %s %s" (show x) (show y))
#define UNDEFINED4(name) (\x y x2 y2 -> error $ printf "undefined: " ++ name ++ " %s %s %s %s" (show x) (show y) (show x2) (show y2))

{-
Use this to insert a trace in the code. The pen will 
speak out the current source line number, digit by digit (msd first).
e.g.
    The following code
        L(shl a bits)
    is translated to
        15099:000a:  PLAYOID #7
        15099:000b:  PLAYOID #3
        15099:000c:  PLAYOID #9
        15099:000d:  SET rc rs
        15099:000e:  SET rd ru
        15099:000f:  CALLID @shl
-}
-- speak out the current source line
#define L(e) mdo{speakInt __LINE__; e}
-- speak out the current source line and the supplied hex value
#define V(v,e) mdo{play ["line"]; speakInt __LINE__; play ["value"]; speakHex (v) (S "0"); e}
#define V2(v1,v2,e) mdo{play ["line"]; speakInt __LINE__; play ["value"]; speakHex (v1) (S "0"); play ["value"]; speakHex (v2) (S "0"); e}
#define R(r,e) mdo{play ["line"]; speakInt __LINE__; play ["register"]; speakHex (I (toRegIndex r)) (S "0"); play ["value"]; speakHex r (S "0"); e}

type CodeSnippet m a = m a
data SnippetType = Subroutine | SubroutineX | TopLevel 
type Program m a = [(String, SnippetType, CodeSnippet m a)]
type Library m a = Program m a
type SoundLib = [(String, String)]


{- Maps the procedure name to a unique tingId. -}
resolveProcedure :: String -> Word16 -> Program m a -> Either String (String, Word16)
resolveProcedure proc tingIdBase program = case findIndices (\(s,_,_) -> s == proc) program of
    [] -> Left $ "Error: The procedure '" ++ proc ++ "' is undefined."
    [i] -> Right $ (proc, tingIdBase + i# i)
    xs -> Left $ printf "Error: Multiple definitions found for the procedure '%s' found at the following locations: %s"
        proc (show xs)

{- Maps the sound name to a unique tingId. -}
resolveSound :: String -> Word16 -> Program m a -> SoundLib -> Either String (String, Word16)
resolveSound sound tingIdBase program soundLib = case findIndices (\(s, _) -> s == sound) soundLib of
    [] -> Left $ "Error: The sound '" ++ sound ++ "' is undefined."
    [i] -> Right $ (sound, tingIdBase + i# (length program + i))
    xs -> Left $ printf "Error: Multiple definitions found for the sound '%s' found at the following locations: %s"
        sound (show xs)


-- Instructions
class (Monad m) => Instructions m where
    end :: m ()
    clearver :: m ()
    set :: Operand -> Operand -> m ()
    cmp :: Operand -> Operand -> m ()
    and :: Operand -> Operand -> m ()
    or :: Operand -> Operand -> m ()
    not :: Operand -> m ()
    opCode07 :: Operand -> m ()
    jmp :: Operand -> m ()
    je :: Operand -> m ()
    jne :: Operand -> m ()
    jg :: Operand -> m ()
    jge :: Operand -> m ()
    jb :: Operand -> m ()
    jbe :: Operand -> m ()
    -- adds saturates to 0xffff, e.g. 0xffff+1==0xffff, 0xa000+0xa000==0xffff
    adds :: Operand -> Operand -> m ()
    -- subs saturates to 0, e.g. 0-1==0, 0-0xffff==0
    subs :: Operand -> Operand -> m ()
    opCode11 :: Operand -> m ()
    opCode12 :: Operand -> m ()
    opCode13 :: Operand -> m ()
    ret :: m ()
    callid :: Operand -> m ()
    playoid :: Operand -> m ()
    pause :: Operand -> m ()

    label :: m Location
    sizeof :: OperandCode -> m Int
    nop :: m ()
    nop = return ()


{- Code combinators -}
guardRegs :: (MonadFix m, Instructions m) => [Operand] -> m a -> m a
guardRegs regs body = mdo
    pushAll regs
    retval <- body
    popAll regs
    return retval

infixr 3  .&&.

(.&&.) :: (MonadFix m, Instructions m) => (Location -> m a) -> (Location -> m b) -> (Location -> m ())
(brancherL .&&. brancherR) loc = mdo
    brancherL _next
    jmp _end
    _next <- label
    brancherR loc
    _end <- label
    nop

infixr 2  .||.

(.||.) :: (MonadFix m, Instructions m) => (Location -> m a) -> (Location -> m b) -> (Location -> m b)
(brancherL .||. brancherR) loc = mdo
    brancherL loc
    brancherR loc

disableInterrupt :: (MonadFix m, Instructions m) => m b -> m b
disableInterrupt body = mdo
    set rlock 1
    retval <- body
    set rlock 0
    return retval

skip :: (MonadFix m, Instructions m) => (Location -> m ()) -> m b -> m b
skip brancher body = mdo
    brancher _end
    retval <- body
    _end <- label
    return retval

skipThen :: (MonadFix m, Instructions m) => (Location -> m ()) -> m () -> m ()-> m ()
skipThen brancher body1 body2 = mdo
    brancher _skip
    body1
    jmp _end
    _skip <- label
    body2
    _end <- label
    nop

loopPre :: (MonadFix m, Instructions m) => (Location -> m ()) -> m b -> m ()
loopPre brancher body = mdo
    _loop <- label
    skip brancher $ mdo
        jmp _end
    _ <- body
    jmp _loop
    _end <- label
    nop

loopPost :: (MonadFix m, Instructions m) => (Location -> m ()) -> m b -> m b
loopPost brancher body = mdo
    _loop <- label
    retval <- body
    brancher _loop
    return retval

repeatN :: (MonadFix m, Instructions m) => Operand -> m a -> m a
repeatN n@(R _) body = mdo
    _loop <- label
    cmp n 0
    je _end
    retval <- body
    subs n 1
    jmp _loop
    _end <- label
    return retval
repeatN x _ = error $ printf "undefined: repeatN %s ?" (show x)

section :: (MonadFix m, Instructions m) => m () -> m Location
section body = mdo
    _start <- label
    body
    return _start

(>>.) :: (Monad m) => m a -> (Location -> m b) -> Location -> m b
(pre >>. branch) loc = pre >> branch loc


{- 
    Derived (non-native) instructions, aka macros 
-}


add :: (MonadFix m, Instructions m) => Operand -> Operand -> m ()
add x@(R _) y = mdo
    set ra 0xffff
    subs ra x
    skipThen (cmp ra y >>. jge) ( mdo
            set x y
            subs x 1
            subs x ra
            set ra 1
        ) ( mdo
            adds x y
            set ra 0
        )
add x y = error $ printf "undefined: add %s %s" (show x) (show y)

sub :: (MonadFix m, Instructions m) => Operand -> Operand -> m ()
sub x@(R _) y = mdo
    skipThen (cmp x y >>. jge) ( mdo
            set ra 0xffff
            subs ra y
            adds ra x
            adds ra 1
            set x ra
            set ra 1
        ) ( mdo
            subs x y
            set ra 0
        )
sub x y = error $ printf "undefined: sub %s %s" (show x) (show y)

add32 :: (MonadFix m, Instructions m) => Operand -> Operand -> Operand -> Operand -> m ()
add32 xMsb@(R _) xLsb@(R _) yMsb yLsb = mdo
    add xMsb yMsb
    add xLsb yLsb
    skip (cmp ra 0 >>. je) $ mdo
        add xMsb 1
add32 x y x2 y2 = error $ printf "undefined: add32 %s %s %s %s" (show x) (show y) (show x2) (show y2)

mul2 :: (MonadFix m, Instructions m) => Operand -> m ()
mul2 r@(R _) = mdo
    and r 0x7fff    -- avoid saturation
    adds r r
mul2 x = error $ printf "undefined: mul2 %s" (show x)

mul2ex :: (MonadFix m, Instructions m) => Operand -> Operand -> m ()
mul2ex msb@(R _) lsb@(R _) = mdo
    mul2 msb
    skip (cmp lsb 0x8000 >>. jb) $ mdo
        or msb 1
    mul2 lsb
mul2ex x y = error $ printf "undefined: mul2ex %s %s" (show x) (show y)

mul10 :: (MonadFix m, Instructions m) => Operand -> m ()
mul10 r@(R _) = mdo
    let t = rc

    -- x10 = 2*(1+2*2)
    mul2 r      -- x2
    set t r
    mul2 t      -- x4
    mul2 t      -- x8
    add r t     -- x(2+8)
mul10 x = error $ printf "undefined: mul10s %s" (show x)

mul10s :: (MonadFix m, Instructions m) => Operand -> m ()
mul10s r@(R _) = mdo
    let t = rc

    -- x10 = 2*(1+2*2)
    adds r r    -- x2
    set t r
    adds t t    -- x4
    adds t t    -- x8
    adds r t    -- x(2+8)
mul10s x = error $ printf "undefined: mul10s %s" (show x)

shl :: (MonadFix m, Instructions m) => Operand -> Operand -> m ()
shl r@(R _) (I count) = mdo
    and r (I (0xffff `shiftR` (i# count)))
    replicateM_ (i# count) $ 
        adds r r
shl r@(R _) bits = mdo
    let count = rb

    set count bits
    repeatN count $ mdo
        mul2 r
shl x y = error $ printf "undefined: shl %s %s" (show x) (show y)

rol :: (MonadFix m, Instructions m) => Operand -> Operand -> m ()
rol r@(R _) bits = mdo
    let count = rb
    let t = rc

    set count bits
    repeatN count $ mdo
        set t r
        and t 0x8000
        mul2 r
        skip (cmp t 0 >>. je) $ mdo
            or r 1
rol x y = error $ printf "undefined: rol %s %s" (show x) (show y)

ror :: (MonadFix m, Instructions m) => Operand -> Operand -> m ()
ror r@(R _) bits = mdo
    let bits' = rd
    
    set bits' 16
    subs bits' bits
    skip (cmp bits' 16 >>. je) $ mdo
        rol r bits'
ror x y = error $ printf "undefined: ror %s %s" (show x) (show y)

shr :: (MonadFix m, Instructions m) => Operand -> Operand -> m ()
shr r@(R _) bits = mdo
    let a = re

    set a 0xFFFF
    shl a bits
    and r a
    ror r bits
shr x y = error $ printf "undefined: shr %s %s" (show x) (show y)

testBit :: (MonadFix m, Instructions m) => Operand -> Operand -> m ()
testBit r@(R _) (I i) = mdo
    let a = ra

    set a r
    and a (I $ bit $ i# i)
    cmp a 0
testBit r@(R _) i@(R _) = mdo
    let a = rc
    let mask = rd

    set a r
    set mask 1
    shl mask i
    and a mask
    cmp a 0
testBit x y = error $ printf "undefined: testBit %s %s" (show x) (show y)

testMask :: (MonadFix m, Instructions m) => Operand -> Operand -> m ()
testMask n mask = mdo
    let a = ra

    set a n
    and a mask
    cmp a 0

setBit :: (MonadFix m, Instructions m) => Operand -> Operand -> m ()
setBit r@(R _) (I i) = mdo
    or r (I $ bit $ i# i)
setBit r@(R _) i@(R _) = mdo
    let mask = rc

    set mask 1
    shl mask i
    or r mask
setBit x y = error $ printf "undefined: setBit %s %s" (show x) (show y)

clearBit :: (MonadFix m, Instructions m) => Operand -> Operand -> m ()
clearBit r@(R _) (I i) = mdo
    and r (I (complement . bit . i# $ i))
clearBit r@(R _) i@(R _) = mdo
    let mask = rc

    set mask 1
    shl mask i
    not mask
    and r mask
clearBit x y = error $ printf "undefined: clearBit %s %s" (show x) (show y)

popCount :: (MonadFix m, Instructions m) => Operand -> Operand -> m ()
popCount result@(R _) x = mdo
    let a = rb
    
    set result 0
    forM_ (map (1 `shiftL`) [0..15]) $ \mask -> mdo
        set a x
        and a (I mask)
        skip (cmp a 0 >>. je) $ mdo
            adds result 1
popCount x y = error $ printf "undefined: popCount %s %s" (show x) (show y)

countLeadingZeros :: (MonadFix m, Instructions m) => Operand -> Operand -> m ()
countLeadingZeros result@(R _) x = mdo
    let a = rb
    let n = rc
    
    set n x
    set result 0
    replicateM_ 16 $ mdo
        set a n
        and a 0x8000
        skip (cmp a 0 >>. je) $ mdo
            jmp _break
        adds result 1
        mul2 n
    _break <- label
    nop
countLeadingZeros x y = error $ printf "undefined: countLeadingZeros %s %s" (show x) (show y)
 
countTrailingZeros :: (MonadFix m, Instructions m) => Operand -> Operand -> m ()
countTrailingZeros result@(R _) x = mdo
    let a = rb
    
    set result 0
    forM_ (map (1 `shiftL`) [0..15]) $ \mask -> mdo
        set a x
        and a (I mask)
        skip (cmp a 0 >>. je) $ mdo
            jmp _break
        adds result 1
    _break <- label
    nop
countTrailingZeros x y = error $ printf "undefined: countTrailingZeros %s %s" (show x) (show y)
 
testAny :: (MonadFix m, Instructions m, Enum e) => Operand -> [e] -> m ()
testAny r@(R _) es = mdo
    forM_ es $ \e -> mdo
        cmp r (I . e2i $ e)
        je _end
    _end <- label
    nop

testAll :: (MonadFix m, Instructions m, Enum e) => Operand -> [e] -> m ()
testAll r@(R _) es = mdo
    forM_ es $ \e -> mdo
        cmp r (I . e2i $ e)
        jne _end
    _end <- label
    nop

{-
mul16s :: (MonadFix m, Instructions m) => Operand -> Operand -> m ()
mul16s r@(R _) (I y) = mdo
    let t = rc

    let trailingZeros = B.countTrailingZeros y
    let leadingZeros = B.countLeadingZeros y

    replicateM_ trailingZeros $ mdo
        adds r r
    set t r
    forM_ [(trailingZeros+1)..(15-leadingZeros)] $ \bit -> mdo
        if (B.testBit y bit) 
            then mdo
                adds r t
                set t r
            else mdo
                adds t t
mul16s x y = error $ printf "undefined: mul16s %s %s" (show x) (show y)

x10=2*(1+2*2)
adds r r -- x2
set t r
adds t t -- x4
adds t t -- x8
adds r t
-}

mul :: (MonadFix m, Instructions m) => Operand -> Operand -> m ()
mul x@(R _) (I y) = mdo
    let xMsb = rb
    let resMsb = rc
    let resLsb = rd

    set xMsb 0
    set resMsb 0
    set resLsb 0
    forM_ [0..15] $ \bit -> mdo
        when (B.testBit y bit) $ mdo
            add32 resMsb resLsb xMsb x
        mul2ex xMsb x
    set x resLsb
    set ra resMsb

mul x@(R _) y = mdo
    let xMsb = rb
    let resMsb = rc
    let resLsb = rd

    set xMsb 0
    set resMsb 0
    set resLsb 0
    forM_ (map (1 `shiftL`) [0..15]) $ \mask -> mdo
        skip (testMask y (I mask) >>. je) $ mdo
            add32 resMsb resLsb xMsb x
        mul2ex xMsb x
    set x resLsb
    set ra resMsb
mul x y = error $ printf "undefined: mul %s %s" (show x) (show y)

divModulo :: (MonadFix m, Instructions m) => Operand -> Operand -> m ()
divModulo nominator@(R _) denominator = mdo
    let quotient = rb
    let remainder = rc

    set ra denominator
    skipThen (cmp ra 0 >>. jne) ( mdo
        -- if D == 0 then error(DivisionByZeroException) end
        set ra 0xFFFF
        set rb 0xFFFF
        ) ( mdo ----
        -- https://en.wikipedia.org/wiki/Division_algorithm#Integer_division_.28unsigned.29_with_remainder
        set quotient 0      -- initialize quotient to zero
        set remainder 0     -- initialize remainder to zero

        forM_ (map (1 `shiftL`) [15, 14 .. 0]) $ \mask -> mdo
            mul2 remainder    -- left-shift remaninder by 1 bit
            -- r(0) := n(i) // set the least-significant bit of r equal to bit i of the numerator
            skip (testMask nominator (I mask) >>. je) $ mdo
                or remainder 1
            skip (cmp remainder denominator >>. jb) $ mdo
                subs remainder denominator
                or quotient (I mask)
        set ra remainder
        set nominator quotient
        )
divModulo x y = error $ printf "undefined: divModulo %s %s" (show x) (show y)

---------------------------------------------------------------------- Poor Man's Stack

stackInit :: (Instructions m, MonadFix m) => [Register] -> m ()
stackInit [] = error $ printf "undefined: stackInit []"
stackInit (sp':_) = mdo
    set sp 0
    where
        sp = register sp'

stackPush :: (Instructions m, MonadFix m) => Operand -> [Register] -> m ()
stackPush _ [] = error $ printf "undefined: stackPush []"
stackPush val (sp':stack') = mdo
    skip (cmp sp (I . i# . length $ stack) >>. jb) $ mdo
        L(play ["!stackOverflow"])
        end
    onLookup (\_ r -> set r val) sp [0 ..] stack
    adds sp 1
    where
        sp = register sp'
        stack = map register stack'

stackPop :: (Instructions m, MonadFix m) => Operand -> [Register] -> m ()
stackPop _ [] = error $ printf "undefined: stackPop []"
stackPop val@(R _) (sp':stack') = mdo
    skip (cmp sp 0 >>. jg) $ mdo
        L(play ["!stackUnderflow"])
        end
    subs sp 1
    onLookup (\_ r -> set val r) sp [0 ..] stack
    where
        sp = register sp'
        stack = map register stack'

stackTopWith :: (Instructions m, MonadFix m) => (Operand -> m ()) -> [Register] -> m ()
stackTopWith _ [] = error $ printf "undefined: stackTopWith []"
stackTopWith f (sp':stack') = mdo
    set rss sp
    subs rss 1
    onLookup (\_ r -> f r) rss [0 ..] stack
    where
        sp = register sp'
        stack = map register stack'

stackPointer :: [Register] -> Operand
stackPointer [] = error $ printf "undefined: stackPointer []"
stackPointer (sp':_) = register sp'

stackEmpty :: (Instructions m, MonadFix m) => [Register] -> m ()
stackEmpty [] = error $ printf "undefined: stackEmpty []"
stackEmpty (sp':_) = cmp sp 0
    where
        sp = register sp'

stackFull :: (Instructions m, MonadFix m) => [Register] -> m ()
stackFull [] = error $ printf "undefined: stackFull []"
stackFull (sp':stack') = cmp sp (I . i# . length $ stack)
    where
        sp = register sp'
        stack = map register stack'

----------------------------------------------------------------------

initStack :: (Instructions m) => m ()
initStack = set rsp (stackTop+1)

boot :: (MonadFix m, Instructions m) => m ()
boot = mdo
    skip (cmp rsp 0 >>. jne) $ mdo
        callid "sanityCheck"
        initStack

push :: (MonadFix m, Instructions m) => Operand -> m ()
push x = mdo
    set rss x
    callid "push_ss"

pushAll :: (MonadFix m, Instructions m) => [Operand] -> m ()
pushAll = mapM_ push

peek :: (MonadFix m, Instructions m) => Operand -> m ()
peek x@(R _) = mdo
    callid "peek_ss"
    set x rss
peek x = error $ printf "undefined: peek %s" (show x)

pop :: (MonadFix m, Instructions m) => Operand -> m ()
pop x@(R _) = mdo
    peek x
    adds rsp 1
pop x = error $ printf "undefined: pop %s" (show x)

popAll :: (MonadFix m, Instructions m) => [Operand] -> m ()
popAll = mapM_ pop . reverse

play :: (MonadFix m, Instructions m) => [String] -> m ()
play = mapM_ (\sound -> playoid (S sound))

speakInt :: (MonadFix m, Instructions m) => Int -> m ()
speakInt = play . map (:[]) . show

playIndex :: (MonadFix m, Instructions m) => Operand -> Operand -> m ()
playIndex n baseId = mdo
    set rss baseId
    adds rss n
    playoid rss

speakNibbles :: (MonadFix m, Instructions m) => Operand -> Operand -> [Int] -> m ()
speakNibbles x zeroId nibbles = mapM_ selectNibble nibbles
    where
        playHexDigit _ digit = playIndex (I digit) zeroId
        selectNibble i = mdo
            set rss x
            and rss (I $ shiftL 0xf (4*i))
            onLookup playHexDigit rss [0, shiftL 1 (4*i) .. ] [0 .. 15]

speakHex :: (MonadFix m, Instructions m) => Operand -> Operand -> m ()
speakHex word zeroId = mdo
    speakNibbles word zeroId [3, 2 .. 0]

speakHex8 :: (MonadFix m, Instructions m) => Operand -> Operand -> m ()
speakHex8 word zeroId = mdo
    speakNibbles word zeroId [1, 0]

{- 
The double dabble algorithm is used to convert binary numbers 
into binary-coded decimal (BCD) notation.
https://en.wikipedia.org/wiki/Double_dabble 

Returns the BCD representation in the ra:rb registers.
-}
doubleDabble16 :: (MonadFix m, Instructions m) => Operand -> m ()
doubleDabble16 binary = mdo
    let bcd1 = ra
    let bcd0 = rb
    let n = rc
    let t = rd
    -- bcd1:bcd0:n together form a 3*16=48 bit scratch register

    set n binary
    set bcd0 0
    set bcd1 0
    -- it takes at least three shifts to get a BCD digit
    -- greater than 4 (that needs to be fixed)
    shiftLeft2 bcd0 n      -- 1
    shiftLeft2 bcd0 n      -- 2
    shiftLeft2 bcd0 n      -- 3
    fixDigits [0] bcd0 t
    shiftLeft2 bcd0 n      -- 4
    fixDigits [0] bcd0 t
    shiftLeft2 bcd0 n      -- 5
    fixDigits [0] bcd0 t
    shiftLeft2 bcd0 n      -- 6
    fixDigits [0..1] bcd0 t
    shiftLeft2 bcd0 n      -- 7
    fixDigits [0..1] bcd0 t
    shiftLeft2 bcd0 n -- 8
    fixDigits [0..1] bcd0 t
    shiftLeft2 bcd0 n -- 9
    fixDigits [0..2] bcd0 t
    shiftLeft2 bcd0 n -- 10
    fixDigits [0..2] bcd0 t
    shiftLeft2 bcd0 n -- 11
    fixDigits [0..2] bcd0 t
    shiftLeft2 bcd0 n -- 12
    fixDigits [0..2] bcd0 t
    shiftLeft3 bcd1 bcd0 n -- 13
    fixDigits [0..3] bcd0 t
    shiftLeft3 bcd1 bcd0 n -- 14
    fixDigits [0..3] bcd0 t
    shiftLeft3 bcd1 bcd0 n -- 15
    fixDigits [0..3] bcd0 t
    shiftLeft3 bcd1 bcd0 n -- 16
    where
        nibblize i n = I $ n `shiftL` (4*i)
        
        -- fix all digits greater than 4 (add 3)
        fixDigits digits bcd t = mdo
            forM_ digits $ \i -> mdo
                set t bcd
                and t (nibblize i 0xf)
                skip (cmp t (nibblize i 4) >>. jbe) $ mdo
                    adds bcd (nibblize i 3)
        
        shiftLeft2' msw lsw = mdo
            adds msw msw        -- shiftL 1

            skip (cmp lsw 0x8000 >>. jb) $ mdo
                or msw 1       -- carry over the MSB from lsw
            and lsw 0x7fff     -- avoid saturation
        
        shiftLeft2 msw lsw = mdo
            shiftLeft2' msw lsw
            adds lsw lsw       -- shiftL 1

        shiftLeft3 msw1 msw0 lsw = mdo
            -- start shifting from the most significant part (msw1)
            shiftLeft2' msw1 msw0
            shiftLeft2' msw0 lsw
            adds lsw lsw       -- shiftL 1

{- Speaks up three digit number, from 0 to 999 -}
speakNatural3 :: (MonadFix m, Instructions m) => Operand -> Operand -> m ()
speakNatural3 n zeroId = mdo
    let nominator = rs

    guardRegs [nominator] $ mdo
        -- hundreds
        set nominator n
        divModulo nominator (I 100)
        skip (cmp nominator 0 >>. je) $ mdo
            playIndex nominator zeroId
            playIndex (e2i Hundred) zeroId
        set nominator ra

        -- tens
        skip (cmp nominator 20 >>. jb) $ mdo
            divModulo nominator (I 10)
            skip (cmp nominator 0 >>. je) $ mdo
                subs nominator 2
                adds nominator (e2i Twenty)
                playIndex nominator zeroId
            set nominator ra

        -- ones
        skip (cmp nominator 0 >>. je) $ mdo
            playIndex nominator zeroId -- [0, 1 .. 19]

        set ra nominator

speakNatural :: (MonadFix m, Instructions m) => Operand -> Operand -> m ()
speakNatural n zeroId = mdo
    let nominator = rt
    let remainder = ru

    guardRegs [nominator, remainder] $ mdo
        -- thousands
        set nominator n
        divModulo nominator (I 1000)
        set remainder ra
        skip (cmp nominator 0 >>. je) $ mdo
            speakNatural3 nominator zeroId 
            playIndex (e2i Thousand) zeroId
        set nominator remainder

        -- [0 .. 999]
        skip (cmp nominator 0 >>. je) $ mdo
            speakNatural3 nominator zeroId 

{- Speaks up three digit number, from 0 to 999, using the Croatian inflection rules. -}
speakNatural3Hr_divModulo :: (MonadFix m, Instructions m) => Operand -> Operand -> Gender -> m ()
speakNatural3Hr_divModulo n zeroId gender = mdo
    let nominator = rs

    guardRegs [nominator] $ mdo
        -- hundreds
        set nominator n
        divModulo nominator (I 100)
        skip (cmp nominator 0 >>. je) $ mdo
            subs nominator 1
            adds nominator (e2i Sto)
            playIndex nominator zeroId
        set nominator ra

        -- tens
        skip (cmp nominator 20 >>. jb) $ mdo
            divModulo nominator (I 10)
            skip (cmp nominator 0 >>. je) $ mdo
                subs nominator 2
                adds nominator (e2i Dvadeset)
                playIndex nominator zeroId
            set nominator ra

        -- ones
        skip (cmp nominator 0 >>. je) $ mdo
            case gender of
                Female -> mdo 
                    skipThen (cmp nominator 2 >>. jg)
                        ( mdo
                            subs nominator 1
                            adds nominator (e2i Jedna)
                            playIndex nominator zeroId
                        ) -- [1,2]
                        (playIndex nominator zeroId) -- [0, 1 .. 19]
                Male -> mdo 
                    (playIndex nominator zeroId) -- [0, 1 .. 19]

speakNaturalHr_divModulo :: (MonadFix m, Instructions m) => Operand -> Operand -> m ()
speakNaturalHr_divModulo n zeroId = mdo
    let nominator = rt
    let remainder = ru

    guardRegs [nominator, remainder] $ mdo
        -- thousands
        set nominator n
        divModulo nominator (I 1000)
        set remainder ra
        skip (cmp nominator 0 >>. je) $ mdo
            speakNatural3Hr_divModulo nominator zeroId Female
            skipThen ((cmp ra 2 >>. jge) .&&. (cmp ra 4 >>. jbe))
                (playIndex (e2i Tisuca) zeroId)
                (playIndex (e2i Tisuce) zeroId)
        set nominator remainder

        -- [0 .. 999]
        skip (cmp nominator 0 >>. je) $ mdo
            speakNatural3Hr_divModulo nominator zeroId Male

{- Speaks up the five digit number, from 0 to 65535, using the Croatian inflection rules. -}
speakNaturalHr :: (MonadFix m, Instructions m) => Operand -> Operand -> m ()
speakNaturalHr n zeroId = mdo
    let t = rc

    skipThen ((set t n >> cmp t 0) >>. jne) ( mdo
        playoid zeroId
        ) ( mdo --- ## ---
        doubleDabble16 n
        -- thousands
        skip ((cmp ra 0 >>. je) .&&. (cmp rb 0x1000 >>. jb)) $ mdo
            -- tens
            skip (cmp ra 2 >>. jb) $ mdo
                -- tens
                subs ra 2
                adds ra (e2i Dvadeset)
                playIndex ra zeroId
                set ra 0

            -- ones
            -- calculate tens*10 + ones
            mul10s ra
            -- get the ones digit
            set t rb
            and t 0xf000
            onLookup (\_ v -> set t (i# v)) t (map (`shiftL` (3*4)) [0..9]) [0..9]
            adds ra t

            skip (cmp ra 0 >>. je) $ mdo
                skipThen (cmp ra 2 >>. jg)
                    ( mdo
                        subs ra 1
                        adds ra (e2i Jedna)
                        playIndex ra zeroId
                    ) -- [1,2]
                    (playIndex ra zeroId) -- [0, 1 .. 19]

            skipThen ((cmp t 2 >>. jge) .&&. (cmp t 4 >>. jbe))
                (playIndex (e2i Tisuca) zeroId)
                (playIndex (e2i Tisuce) zeroId)

        -- hundreds
        -- get the hundreds digit
        set ra rb
        and ra 0x0f00
        onLookup (\_ v -> set ra (i# v)) ra (map (`shiftL` (2*4)) [0..9]) [0..9]
        skip (cmp ra 0 >>. je) $ mdo
            subs ra 1
            adds ra (e2i Sto)
            playIndex ra zeroId

        -- get the tens digit
        set ra rb
        and ra 0x00f0
        onLookup (\_ v -> set ra (i# v)) ra (map (`shiftL` (1*4)) [0..9]) [0..9]
        -- tens
        skip (cmp ra 2 >>. jb) $ mdo
            subs ra 2
            adds ra (e2i Dvadeset)
            playIndex ra zeroId
            set ra 0

        -- ones
        -- calculate tens*10 + ones
        mul10s ra
        -- get the ones digit
        and rb 0x000f
        adds ra rb
        skip (cmp ra 0 >>. je) $ mdo
            playIndex ra zeroId     -- [0, 1 .. 19]
        )

speakDecimal :: (MonadFix m, Instructions m) => Operand -> Operand -> m ()
speakDecimal n zeroId = mdo
    let nominator = rs
    let leadingZero = rt

    set ra n    
    skipThen (cmp ra 0 >>. jne) ( mdo
        playoid zeroId
        ) ( mdo ----
        guardRegs [nominator, leadingZero] $ mdo
            set nominator n
            set leadingZero 1

            forM_ (map (10 ^) [4,3 .. 0]) $ \tenX -> mdo
                divModulo nominator (I tenX)
                skipThen (cmp nominator 0 >>. je) ( mdo
                    set leadingZero 0
                    playIndex nominator zeroId
                    ) ( mdo ----
                    skip (cmp leadingZero 0 >>. jne) $ mdo
                        playoid zeroId
                    )
                set nominator ra
        )

caseOf :: (MonadFix m, Instructions m) => Operand -> (a -> Operand) -> [(a, m ())] -> m ()
caseOf x transform lookupMap = mdo
    set rss x
    forM_ lookupMap $ \(key, action) -> mdo
        skip (cmp rss (transform key) >>. jne) $ mdo
            action

onLookup :: (MonadFix m, Instructions m, Show a) => (Word16 -> a -> m ()) -> Operand -> [Word16] -> [a] -> m ()
onLookup action r@(R _) keys values = mdo
        forM_ (zip keys values) $ \(key,val) -> mdo
            skip (cmp r (I key) >>. jne) $ mdo 
                action key val
                jmp _end
        L(playoid (S "!outOfRange"))
        --R(r, nop)
        end
        _end <- label
        nop
onLookup _ r keys values = error $ printf "undefined: onLookup ? %s %s %s" (show r) (show keys) (show values)

charToString :: Char -> String
charToString = (:[])

{-
Generate labels with EnumerateSelection.py SublimeText plugin.
Column select {- xx -}, replace with 0, select the inserted 0s and then press Ctrl+Shift+e
-}
sanityCheckBasic :: (MonadFix m, Instructions m) => m ()
sanityCheckBasic = mdo
    {-
    play ["sanityCheck"]
    set rc (S "1")
    playoid rc
    -}
    mapM_ (\(i,brancher) -> mdo
        skip brancher $ mdo
            play ["sanityCheck", "1", "!fail"]
            play $ map charToString (printf "%x" (i::Int))
            end
        ) . zip [0 ..] 
        $ {- 0-} [(set r0 0xa5a5 >> cmp r0 0xa5a5) >>. je
        , {- 1-} (cmp r0 0xface) >>. jne
        , {- 2-} (cmp r0 r0) >>. je
        , {- 3-} (set r0 r0 >> cmp r0 r0) >>. je
        , {- 4-} (adds r0 1 >> cmp r0 0xa5a6) >>. je
        , {- 5-} (adds r0 0 >> cmp r0 0xa5a6) >>. je
        , {- 6-} (subs r0 1 >> cmp r0 0xa5a5) >>. je
        , {- 7-} (subs r0 0 >> cmp r0 0xa5a5) >>. je
        , {- 8-} (and r0 0x00ff >> cmp r0 0x00a5) >>. je
        , {- 9-} (or r0 0xa500 >> cmp r0 0xa5a5) >>. je
        , {- a-} (not r0 >> cmp r0 0x5a5a) >>. je
        , {- b-} (set r1 0xa5a5 >> cmp r0 r1) >>. jb
        , {- c-} jbe
        , {- d-} cmp r1 r0 >>. jg
        , {- e-} jge
        , {- f-} (set rc 0x7ffe >> adds rc 1 >> cmp rc 0x7fff) >>. je
        , {-10-} (adds rc 1 >> cmp rc 0x8000) >>. je
        , {-11-} (adds rc 1 >> cmp rc 0x8001) >>. je
        , {-12-} (subs rc 2 >> cmp rc 0x7fff) >>. je
        , {-13-} (set rc 0xfffe >> adds rc 1 >> cmp rc 0xffff) >>. je
        , {-14-} (set rc 0x7fff >> adds rc 0x8000 >> cmp rc 0xffff) >>. je
        , {-15-} (adds rc 1 >> cmp rc 0xffff) >>. je
        , {-16-} (adds rc 2 >> cmp rc 0xffff) >>. je
        , {-17-} (adds rc 3 >> cmp rc 0xffff) >>. je
        , {-18-} (set rc 0x8000 >> adds rc 0x8000 >> cmp rc 0xffff) >>. je
        , {-19-} (set rc 0xA000 >> adds rc 0xA000 >> cmp rc 0xffff) >>. je
        , {-1a-} (set rc 0 >> subs rc 1 >> cmp rc 0) >>. je
        , {-1b-} (set rc 0 >> subs rc 0xffff >> cmp rc 0) >>. je
        , {-1c-} (set rc 0xfffe >> callid "inc_rc" >> cmp rc 0xffff) >>. je
        , {-1d-} (set rc 0x8000 >> set rd "dec_rc" >> callid rd >> cmp rc 0x7fff) >>. je
        , {-1e-} (set rc 0x8000 >> cmp rc 0x7fff) >>. jg
        , {-1f-} (set rc 0x7fff >> cmp rc 0x8000) >>. jb
        , {-20-} (set rc 0 >> cmp rc 0xffff) >>. jb
        , {-21-} (set rc 0xffff >> cmp rc 0) >>. jg
        , {-22-} (set rc 1 >> callid "mutual1" >> cmp rc 1) >>. je
        , {-23-} (set rc 16 >> callid "mutual1" >> cmp rc 16) >>. je
        , {-24-} (set rc 16 >> callid "recurse" >> cmp rc 16) >>. je
        ]
    play ["pass"]


prelude :: (MonadFix m, Instructions m) => Program m ()
prelude = [
    -- IMPORTANT: All procedures that do not start with '$' get prepended with the `boot` macro
    ("initStack", SubroutineX, mdo
        initStack
    )
    , ("boot", SubroutineX, mdo
        boot
    )
    , ("set_rc_0", SubroutineX, mdo
        set rc 0
    )
    , ("inc_rc", SubroutineX, mdo
        add rc 1
    )
    , ("add_rc_100", SubroutineX, mdo
        add rc 100
    )
    , ("dec_rc", SubroutineX, mdo
        sub rc 1
    )
    , ("sub_rc_100", SubroutineX, mdo
        sub rc 100
    )
    , ("set_rd_0", SubroutineX, mdo
        set rd 0
    )
    , ("inc_rd", SubroutineX, mdo
        add rd 1
    )
    , ("add_rd_100", SubroutineX, mdo
        add rd 100
    )
    , ("dec_rd", SubroutineX, mdo
        sub rd 1
    )
    , ("sub_rd_100", SubroutineX, mdo
        sub rd 100
    )
    , ("mutual1", SubroutineX, mdo
        skip (cmp rc 0 >>. je) $ mdo
            subs rc 1
            callid "mutual2"
            adds rc 1
    )
    , ("mutual2", SubroutineX, mdo
        skip (cmp rc 0 >>. je) $ mdo
            subs rc 1
            callid "mutual1"
            adds rc 1
    )
    , ("recurse", SubroutineX, mdo
        skip (cmp rc 0 >>. je) $ mdo
            subs rc 1
            callid "recurse"
            adds rc 1
    )
    , ("mutual1Play", SubroutineX, mdo
        play ["value"]
        --speakNibbles rc (S "0") [0]
        skip (cmp rc 0 >>. je) $ mdo
            subs rc 1
            callid "mutual2Play"
            adds rc 1
    )
    , ("mutual2Play", SubroutineX, mdo
        play ["value"]
        --speakNibbles rc (S "0") [0]
        skip (cmp rc 0 >>. je) $ mdo
            subs rc 1
            callid "mutual1Play"
            adds rc 1
    )
    , ("recursePlay", SubroutineX, mdo
        play ["value"]
        --speakNibbles rc (S "0") [0]
        skip (cmp rc 0 >>. je) $ mdo
            subs rc 1
            --play ["value"]
            callid "recursePlay"
            --play ["value"]
            adds rc 1
    )
    , ("lastOid1", SubroutineX, mdo
        speakHex rlastoid (S "0")
    )
    , ("lastOid2", SubroutineX, mdo
        callid "lastOid1"   -- speaks tingId of "lastOid2"
        speakHex rlastoid (S "0")   -- speaks tingId of "lastOid2"
    )
    , ("sanityCheck", SubroutineX, mdo
        sanityCheckBasic
    )
    , ("sanityCheck2", SubroutineX, mdo
        initStack
        mapM_ (\(i,brancher) -> mdo
            skip brancher $ mdo
                play ["sanityCheck", "2", "!fail"]
                play $ map charToString (printf "%x" (i::Int))
                end
            ) . zip [0 ..] 
            -- Locks up when I try to recursivly do callid *and* do playoid, e.g.:
            -- callid "mutual1Play"
            --      playoid
            --      callid "mutual1Play"
            --          playoid
            --          callid "mutual1Play" -- LOCKS UP HERE
            $ {- 0-} [(set rc 1 >> callid "mutual1Play" >> cmp rc 1) >>. je
            , {- 1-} (set rc 16 >> callid "mutual1Play" >> cmp rc 16) >>. je
            , {- 2-} (set rc 16 >> callid "recursePlay" >> cmp rc 16) >>. je
            , {- 3-} (set rc 11 >> set rd 0xdead >> callid "writeReg" >> cmp r11 0xdead) >>. je
            , {- 4-} (set r11 0xface >> set rc 11 >> callid "readReg" >> cmp ra 0xface) >>. je
            , {- 5-} (set r0 0xffff >> add r0 0 >> cmp r0 0xffff) >>. je
            , {- 6-} (cmp ra 0) >>. je
            , {- 7-} (set r0 0xffff >> add r0 1 >> cmp r0 0) >>. je
            , {- 8-} (cmp ra 1) >>. je
            , {- 9-} (set r0 0x7fff >> add r0 0x8000 >> cmp r0 0xffff) >>. je
            , {- a-} (cmp ra 0) >>. je
            , {- b-} (set r0 0xffff >> add r0 0xffff >> cmp r0 0xfffe) >>. je
            , {- c-} (cmp ra 1) >>. je
            , {- d-} (set r0 0xffff >> sub r0 0xffff >> cmp r0 0) >>. je
            , {- e-} (cmp ra 0) >>. je
            , {- f-} (set r0 0 >> sub r0 1 >> cmp r0 0xffff) >>. je
            , {-10-} (cmp ra 1) >>. je
            , {-11-} (set r0 0 >> sub r0 0xffff >> cmp r0 1) >>. je
            , {-12-} (cmp ra 1) >>. je
            , {-13-} (set r0 0xffff >> mul2 r0 >> cmp r0 0xfffe) >>. je
            , {-14-} (cmp ra 1) >>. je
            , {-15-} (set r0 0xffff >> set r1 0xffff >> add32 r0 r1 0 1 >> cmp r0 0) >>. je
            , {-16-} (cmp r1 0) >>. je
            , {-17-} (cmp ra 1) >>. je
            , {-18-} (set r0 0xffff >> set r1 0xffff >> mul2ex r0 r1 >> cmp r0 0xffff) >>. je
            , {-19-} (cmp r1 0xfffe) >>. je
            , {-1a-} (set r0 0xffff >> shl r0 4 >> cmp r0 (I ((0xffff::Word16) `shiftL` 4))) >>. je
            , {-1b-} (set r0 0xa5a5 >> rol r0 4 >> cmp r0 0x5a5a) >>. je
            , {-1c-} (set r0 0xa5a5 >> ror r0 4 >> cmp r0 0x5a5a) >>. je
            , {-1d-} (set r0 0xa5a5 >> shr r0 4 >> cmp r0 0x0a5a) >>. je
            , {-1e-} (set r0 0x8000 >> testBit r0 15) >>. jne
            , {-1f-} (set r0 0xf000 >> testMask r0 0x8000) >>. jne
            , {-20-} (set r0 0xf0f0 >> setBit r0 0 >> cmp r0 0xf0f1) >>. je
            , {-21-} (clearBit r0 14 >> cmp r0 0xb0f1) >>. je
            , {-22-} (popCount r0 0xa5a5 >> cmp r0 8) >>. je
            , {-23-} (popCount r0 0xa5a4 >> cmp r0 7) >>. je
            , {-24-} (countLeadingZeros r0 0xf0f0 >> cmp r0 0) >>. je
            , {-25-} (countLeadingZeros r0 0x0f0f >> cmp r0 4) >>. je
            , {-26-} (countTrailingZeros r0 0x0f0f >> cmp r0 0) >>. je
            , {-27-} (countTrailingZeros r0 0xf0f0 >> cmp r0 4) >>. je
            , {-28-} (set r0 0 >> mul r0 0 >> cmp r0 0) >>. je
            , {-29-} (cmp ra 0) >>. je
            , {-2a-} (set r0 0x5a5a >> mul r0 0xa5a5 >> cmp r0 (i# . snd . divMod (0x5a5a*0xa5a5 :: Int) $ 0x10000)) >>. je
            , {-2b-} (cmp ra (i# . fst . divMod (0x5a5a*0xa5a5 :: Int) $ 0x10000)) >>. je
            , {-2c-} (set r0 0 >> divModulo r0 0xf >> cmp r0 (i# . fst . divMod (0::Int) $ 0xf)) >>. je
            , {-2d-} (cmp ra (i# . snd . divMod (0::Int) $ 0xf)) >>. je
            , {-2e-} (set r0 0xffff >> divModulo r0 0x0fff >> cmp r0 (i# . fst . divMod (0xffff::Int) $ 0x0fff)) >>. je
            , {-2f-} (cmp ra (i# . snd . divMod (0xffff::Int) $ 0x0fff)) >>. je
            ]
        play ["pass"]
    )
    , ("sanityCheck3", SubroutineX, mdo
        initStack
        mapM_ (\(i,brancher) -> mdo
            skip brancher $ mdo
                play ["sanityCheck", "3", "!fail"]
                play $ map charToString (printf "%x" (i::Int))
                end
            ) . zip [0 ..] 
            -- Locks up when I try to recursivly do callid *and* do playoid, e.g.:
            -- callid "mutual1Play"
            --      playoid
            --      callid "mutual1Play"
            --          playoid
            --          callid "mutual1Play" -- LOCKS UP HERE
            $ {- 0-} [(set r0 0xffff >> set r1 0 >> add r0 r1 >> cmp r0 0xffff) >>. je
            , {- 1-} (cmp ra 0) >>. je
            , {- 2-} (set r0 0xffff >> set r1 1 >> add r0 r1 >> cmp r0 0) >>. je
            , {- 3-} (cmp ra 1) >>. je
            , {- 4-} (set r0 0x7fff >> set r1 0x8000 >> add r0 r1 >> cmp r0 0xffff) >>. je
            , {- 5-} (cmp ra 0) >>. je
            , {- 6-} (set r0 0xffff >> set r1 0xffff >> add r0 r1 >> cmp r0 0xfffe) >>. je
            , {- 7-} (cmp ra 1) >>. je
            , {- 8-} (set r0 0xffff >> set r1 0xffff >> sub r0 r1 >> cmp r0 0) >>. je
            , {- 9-} (cmp ra 0) >>. je
            , {- a-} (set r0 0 >> set r1 1 >> sub r0 r1 >> cmp r0 0xffff) >>. je
            , {- b-} (cmp ra 1) >>. je
            , {- c-} (set r0 0 >> set r1 0xffff >> sub r0 r1 >> cmp r0 1) >>. je
            , {- d-} (cmp ra 1) >>. je
            , {- e-} (set r0 0xffff >> mul2 r0 >> cmp r0 0xfffe) >>. je
            , {- f-} (cmp ra 1) >>. je
            , {-10-} (set r0 0xffff >> set r1 0xffff >> set r2 0 >> set r3 1 >> add32 r0 r1 r2 r3 >> cmp r0 0) >>. je
            , {-11-} (cmp r1 0) >>. je
            , {-12-} (cmp ra 1) >>. je
            , {-13-} (set r0 0xffff >> set r1 4 >> shl r0 r1 >> cmp r0 (I ((0xffff::Word16) `shiftL` 4))) >>. je
            , {-14-} (set r0 0xa5a5 >> set r1 4 >> rol r0 r1 >> cmp r0 0x5a5a) >>. je
            , {-15-} (set r0 0xa5a5 >> set r1 4 >> ror r0 r1 >> cmp r0 0x5a5a) >>. je
            , {-16-} (set r0 0xa5a5 >> set r1 4 >> shr r0 r1 >> cmp r0 0x0a5a) >>. je
            , {-17-} (set r0 0x8000 >> set r1 15 >> testBit r0 r1) >>. jne
            , {-18-} (set r0 0xf000 >> set r1 0x8000 >> testMask r0 r1) >>. jne
            , {-19-} (set r0 0xf0f0 >> set r1 0 >> setBit r0 r1 >> cmp r0 0xf0f1) >>. je
            , {-1a-} (set r1 14 >> clearBit r0 r1 >> cmp r0 0xb0f1) >>. je
            , {-1b-} (set r1 0xa5a5 >> popCount r0 r1 >> cmp r0 8) >>. je
            , {-1c-} (set r1 0xa5a4 >> popCount r0 r1 >> cmp r0 7) >>. je
            , {-1d-} (set r1 0xf0f0 >> countLeadingZeros r0 r1 >> cmp r0 0) >>. je
            , {-1e-} (set r1 0x0f0f >> countLeadingZeros r0 r1 >> cmp r0 4) >>. je
            , {-1f-} (set r1 0x0f0f >> countTrailingZeros r0 r1 >> cmp r0 0) >>. je
            , {-20-} (set r1 0xf0f0 >> countTrailingZeros r0 r1 >> cmp r0 4) >>. je
            , {-21-} (set r0 0 >> set r1 0 >> mul r0 r1 >> cmp r0 0) >>. je
            , {-22-} (cmp ra 0) >>. je
            , {-23-} (set r0 0x5a5a >> set r1 0xa5a5 >> mul r0 r1 >> cmp r0 (i# . snd . divMod (0x5a5a*0xa5a5 :: Int) $ 0x10000)) >>. je
            , {-24-} (cmp ra (i# . fst . divMod (0x5a5a*0xa5a5 :: Int) $ 0x10000)) >>. je
            , {-25-} (set r0 0 >> set r1 0xf >> divModulo r0 r1 >> cmp r0 (i# . fst . divMod (0::Int) $ 0xf)) >>. je
            , {-26-} (cmp ra (i# . snd . divMod (0::Int) $ 0xf)) >>. je
            , {-27-} (set r0 0xffff >> set r1 0x0fff >> divModulo r0 r1 >> cmp r0 (i# . fst . divMod (0xffff::Int) $ 0x0fff)) >>. je
            , {-28-} (cmp ra (i# . snd . divMod (0xffff::Int) $ 0x0fff)) >>. je
            ]
        play ["pass"]
    )
    , ("readReg", Subroutine, mdo
        let range = reverse [0 .. maxRegisterIndex]
        onLookup (\_ r -> mdo
            set ra (R r)
            ) rc range range
    )
    , ("writeReg", Subroutine, mdo
        let range = reverse [0 .. maxRegisterIndex]
        onLookup (\_ r -> mdo
            set (R r) rd
            ) rc range range
    )
    , ("push_ss", Subroutine, mdo
        skipThen (cmp rsp stackBottom >>. jg) ( mdo
                L(playoid (S "!stackOverflow"))
                --R(rsp, nop)
                end
            ) ( mdo
                subs rsp 1
                let range = reverse [stackBottom .. stackTop]
                onLookup (\_ r -> mdo
                    set (R r) rss
                    ) rsp range range
            )
    )
    , ("peek_ss", Subroutine, mdo
        skipThen (cmp rsp stackTop >>. jbe) ( mdo
                L(playoid (S "!stackUnderflow"))
                --R(rsp, nop)
                end
            ) ( mdo
                let range = reverse [stackBottom .. stackTop]
                onLookup (\_ r -> mdo
                    set rss (R r)
                    ) rsp range range
            )
    )
    , ("speakRegister", Subroutine, mdo
        playoid (S "register")
        speakHex8 rc (S "0")
        playoid (S "equals")
        callid "readReg"
        speakHex ra (S "0")
    )
    , ("dumpSystemRegs", Subroutine, mdo
        forM_ [90..99] $ \r -> mdo
            set rc (I r)
            callid "speakRegister"
    )
    , ("opCode07", SubroutineX, mdo
        playoid (S "0")
        set r0 0
        opCode07 r0
        playoid (S "register")
        playoid (S "equals")
        speakHex r0 (S "0")
    )
    , ("opCode11", SubroutineX, mdo
        playoid (S "0")
        set r0 0
        opCode11 r0
        playoid (S "register")
        playoid (S "equals")
        speakHex r0 (S "0")
    )
    , ("opCode12", SubroutineX, mdo
        playoid (S "0")
        set r0 0
        opCode12 r0
        playoid (S "register")
        playoid (S "equals")
        speakHex r0 (S "0")
    )
    , ("opCode13", SubroutineX, mdo
        playoid (S "0")
        set r0 0
        opCode13 r0
        playoid (S "register")
        playoid (S "equals")
        speakHex r0 (S "0")
    )
    ]


preludeSoundLib :: SoundLib
preludeSoundLib = 
    [ ("!stackOverflow", dir2 ++ "stackOverflow.mp3")
    , ("!stackUnderflow", dir2 ++ "stackUnderflow.mp3")
    , ("!outOfRange", dir2 ++ "outOfRange.mp3")
    , ("line", dir2 ++ "line.mp3")
    , ("register", dir2 ++ "register.mp3")
    , ("value", dir2 ++ "value.mp3")
    , ("equals", dir2 ++ "equals.mp3")
    , ("sanityCheck", dir2 ++ "sanityCheck.mp3")
    , ("pass", dir2 ++ "pass.mp3")
    , ("!fail", dir2 ++ "fail.mp3")
    , ("0", dir ++ "0.mp3")
    , ("1", dir ++ "1.mp3")
    , ("2", dir ++ "2.mp3")
    , ("3", dir ++ "3.mp3")
    , ("4", dir ++ "4.mp3")
    , ("5", dir ++ "5.mp3")
    , ("6", dir ++ "6.mp3")
    , ("7", dir ++ "7.mp3")
    , ("8", dir ++ "8.mp3")
    , ("9", dir ++ "9.mp3")
    , ("a", dir2 ++ "a.mp3")
    , ("b", dir2 ++ "b.mp3")
    , ("c", dir2 ++ "c.mp3")
    , ("d", dir2 ++ "d.mp3")
    , ("e", dir2 ++ "e.mp3")
    , ("f", dir2 ++ "f.mp3")
    ]
    where
        language = "hr"
        speaker = "vjeko"
        dir = "./sounds/" ++ language ++ "/" ++ speaker ++ "/" 
        speaker2 = "boris"
        dir2 = "./sounds/" ++ language ++ "/" ++ speaker2 ++ "/" 

{-
Tests made:
    Callid == Callid && Playoid == Playoid
        Locks up on 
            (set rc 16 >> callid "mutual1Play" >> cmp rc 16) >>. je
        after two sounds (iterations).

    Callid == Playoid
        - sanityCheckBasic
            Lock up on `(set rc 16 >> callid "recurse" >> cmp rc 16) >>. je`

    Playoid == Callid
        - sanityCheckBasic
            similar to when Playoid == Playoid
            slow callid
            `(set rc 16 >> callid "recurse" >> cmp rc 16) >>. je` very slow ~2 sec
        - sanityCheck2
            `(set rc 0xa5a5 >> shr rc 4 >> cmp rc 0x0a5a) >>. je` yields `stackUnderflow`
            Both
                (set rc 16 >> callid "mutual1Play" >> cmp rc 16) >>. je
            and
                (set rc 16 >> callid "recursePlay" >> cmp rc 16) >>. je
            succeed, but only two sounds can be heard each.

Conclusion
    Use Callid only. Do not use Playoid. It behaves slightly better - it doesn't lock up 
    as easily. Use subroutines as sparsely as possible. Use macros instead.

-}