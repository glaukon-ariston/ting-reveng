{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MagicHash #-}
{-
Author: Glaukon Ariston
Date: 16.06.2016
Abstract:

Ting Pen -- Assembly Languge as Haskell EDSL

Based on:
Russell O’Connor: ICFP 2014 Post-Mortem
http://r6.ca/blog/20140803T030905Z.html

Lewis: An Asm Monad
http://wall.org/~lewis/2013/10/15/asm-monad.html
https://github.com/quietfanatic/neskell

Monads to Machine Code (Part 1)
http://www.stephendiehl.com/posts/monads_machine_code.html
https://github.com/sdiehl/tinyjit

24 Days of GHC Extensions: Overloaded Strings
https://ocharles.org.uk/blog/posts/2014-12-17-overloaded-strings.html
https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions/basic-syntax-extensions#overloadedstrings
https://kwangyulseo.com/tag/overloadedstrings/

-}

module Ting.Assembler where

import Prelude hiding (and, or, not)
import Data.Word (Word8, Word16)
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString.Lazy as B
import Text.Printf (printf)

import Control.Applicative (Applicative(..), (<$>))
import Control.Monad.RWS (RWS, execRWS, rws, get)
import Control.Monad.Fix (MonadFix(..))
import qualified Data.Foldable as F (toList)
import qualified Data.Sequence as S (Seq, fromList, empty)

import Debug.Trace (trace)

import Ting.Operand (Operand(..), OperandType(..), OperandCode(..))
import Ting.Registers (rsp)
import Ting.Instructions (Instructions(..), SnippetType(Subroutine, SubroutineX), Program, SoundLib, skip, initStack, (>>.))
import qualified Ting.Instructions as I (resolveProcedure, resolveSound)


i# :: (Integral a, Num b) => a -> b
i# = fromIntegral

{- Convert from enum to word -}
e2i :: (Enum a, Num b) => a -> b
e2i = fromIntegral . fromEnum

-- ================================================================= Assembler

data AsmResult = AsmResult
    { _code   :: [Word8]
    , _location :: Word16
    }

data AsmEnv = AsmEnv
    { _tingIdBase :: Word16
    , _program :: Program Assembler ()
    , _soundLib :: SoundLib
    }

newtype Assembler a = Assembler (RWS AsmEnv (S.Seq Word8) Word16 a)
    deriving (Functor, Applicative, Monad, MonadFix)

{- Append the list of instructions to the code stream. -}
append :: [Word8] -> Assembler ()
append xs = Assembler . rws $ \_ loc -> ((), loc + (i# . length $ xs), S.fromList xs)

{- Inject bytes into the output stream. -}
bytes :: [Word8] -> Assembler ()
bytes = append

{- Inject one byte into the output stream. -}
byte :: Word8 -> Assembler ()
byte x = bytes [x]

{- Inject two bytes (BigEndian layout) into the output stream. -}
word16 :: Word16 -> Assembler ()
word16 x = bytes $ map fromIntegral [(x `shiftR` 8) .&. 0xFF, x .&. 0xFF]

{- Maps the procedure name to a unique tingId. -}
resolveProcedure :: String -> Assembler (String, Word16)
resolveProcedure proc = Assembler . rws $ go
    where
        go env loc = 
            ( case I.resolveProcedure proc (_tingIdBase env) (_program env) of
                Right location -> location
                Left msg -> error msg
            , loc
            , S.empty )

{- Maps the sound name to a unique tingId. -}
resolveSound :: String -> Assembler (String, Word16)
resolveSound sound = Assembler . rws $ go
    where
        go env loc = 
            ( case I.resolveSound sound (_tingIdBase env) (_program env) (_soundLib env) of
                Right location -> location
                Left msg -> error msg
            , loc
            , S.empty )

{- Assemble the program, by generating the machine code and fixing the referenced locations. 
Also generates the tingId(s) and fixes them in the call site. -}
asm :: Int -> Program Assembler () -> SoundLib -> [B.ByteString]
asm tingIdBase program soundLib = map go program
    where
        go (_, snippetType, snippet) = 
            let (Assembler snippet') = appendEpilogue snippetType . prependProlog snippetType $ snippet 
            in B.pack . F.toList . snd . execRWS snippet' env $ 0
        prependProlog SubroutineX snippet = snippet
        prependProlog _ snippet = mdo
            skip (cmp rsp 0 >>. jne) $ mdo
                callid "sanityCheck"
                callid "__initialise__"
                initStack
            snippet
        appendEpilogue SubroutineX snippet = subroutine snippet
        appendEpilogue Subroutine snippet = subroutine snippet
        appendEpilogue _ snippet = mdo
            _ <- snippet
            end
        subroutine snippet = mdo
            _ <- snippet
            ret
            end
        env = AsmEnv 
            { _tingIdBase = fromIntegral tingIdBase
            , _program = program 
            , _soundLib = soundLib }

-- ========================================================= Instructions

binaryOp :: OperandCode -> Operand -> Operand -> Assembler ()
binaryOp opcode (R r) (I v) = do
    byte $ e2i opcode
    byte $ e2i OpRegVal
    word16 r
    word16 v
binaryOp opcode (R r) (A v) = do
    byte $ e2i opcode
    byte $ e2i OpRegVal
    word16 r
    word16 v
binaryOp opcode (R rd) (R rs) = do
    byte $ e2i opcode
    byte $ e2i OpRegReg
    word16 rd
    word16 rs
binaryOp opcode (R r) (S sound) = do
    (_,tingId) <- resolveSound sound
    binaryOp opcode (R r) (I tingId)
binaryOp opcode (R r) (P proc) = do
    (_, tingId) <- resolveProcedure proc
    binaryOp opcode (R r) (I tingId)
binaryOp opcode op1 op2 = error $ printf "undefined: binaryOp %s %s %s" (show opcode) (show op1) (show op2)


unaryOp :: OperandCode -> Operand -> Assembler ()
unaryOp opcode (I v) = do
    byte $ e2i opcode
    byte $ e2i OpRegVal
    word16 v
unaryOp opcode (R r) = do
    byte $ e2i opcode
    byte $ e2i OpRegReg
    word16 r
unaryOp opcode op1 = error $ printf "undefined: unaryOp %s %s" (show opcode) (show op1)


branchOp :: OperandCode -> Operand -> Assembler ()
branchOp opcode (A v) = do
    byte $ e2i opcode
    byte $ e2i OpNone
    word16 v
branchOp opcode (I v) = do
    byte $ e2i opcode
    byte $ e2i OpNone
    word16 v
{- 
Non-immediate jumps are not supported. (Sigh)

branchOp opcode (R r) = do
    byte $ e2i opcode
    byte $ e2i OpRegReg
    word16 r
-}
branchOp opcode op1 = error $ printf "undefined: branchOp %s %s" (show opcode) (show op1)


{- | (Ab)using some ad-hoc polymorphism in order to be able to write e.g.
>   and r1 0xFE
instead of
>   and r1 (I 0xFE)

https://wiki.haskell.org/Ad-hoc_polymorphism#Ad-hoc_polymorphism
-}
instance Instructions Assembler where 
    end = do
        byte $ e2i End
        byte $ e2i OpNone
    clearver = do
        byte $ e2i Clearver
        byte $ e2i OpNone
    set = binaryOp Set
    cmp = binaryOp Cmp
    and = binaryOp And
    or = binaryOp Or
    not r@(R _) = binaryOp Not r (R 0)
    not op1 = error $ printf "undefined: not %s" (show op1)
    opCode07 = unaryOp OpCode07
    
    jmp op1 = branchOp Jmp op1
    je op1 = branchOp Je op1
    jne op1 = branchOp Jne op1
    jg op1 = branchOp Jg op1
    jge op1 = branchOp Jge op1
    jb op1 = branchOp Jb op1
    jbe op1 = branchOp Jbe op1
    
    adds = binaryOp Adds
    subs = binaryOp Subs
    opCode11 = unaryOp OpCode11
    opCode12 = unaryOp OpCode12
    opCode13 = unaryOp OpCode13
    
    ret = do
        byte $ e2i Return
        byte $ e2i OpNone
        {- IMPORTANT: it seems that all code resources (things behind a tingId) must end 
        with the END command. Not sure if the RET or END command is allowed in the 
        middle of the code (it looks like it is not -- to be verified).
        
        There are RETURNs in the middle of the code:
        "05097";"20000";0x14;0x00;"return";"None";0x00;0x000022EE
        00002302: 14 00           return
        
        These are called by PLAYOID not by CALLID:
        "05091";"20012";0x14;0x00;"return";"None";0x00;0x00000060
        "05091";"20013";0x14;0x00;"return";"None";0x00;0x000003F0
        "05091";"20014";0x14;0x00;"return";"None";0x00;0x000003F0

        "05135";"30001";0x14;0x00;"return";"None";0x00;0x0000002E
        "05135";"30002";0x14;0x00;"return";"None";0x00;0x0000001E
        -}
        byte $ e2i End
    callid (P proc) = do
        (_, tingId) <- resolveProcedure proc
        unaryOp Callid (I tingId)
    callid i@(I _) = unaryOp Callid i
    callid r@(R _) = unaryOp Callid r
    callid op1 = error $ printf "undefined: callid %s" (show op1)
    playoid (S sound) = do
        (_,tingId) <- resolveSound sound
        unaryOp Callid (I tingId)
    playoid i@(I _) = unaryOp Callid i
    playoid r@(R _) = unaryOp Callid r
    playoid op1 = error $ printf "undefined: playoid %s" (show op1)
    pause = unaryOp Pause

    {- The label function returns the current index of the output stream. -}
    label = A <$> Assembler get

    sizeof (End) = return 2
    sizeof (Clearver) = return 2
    sizeof (Set) = return 6
    sizeof (Cmp) = return 6
    sizeof (And) = return 6
    sizeof (Or) = return 6
    sizeof (Not) = return 6
    sizeof (OpCode07) = error $ printf "undefined: sizeof OpCode07"
    sizeof (Jmp) = return 4
    sizeof (Je) = return 4
    sizeof (Jne) = return 4
    sizeof (Jg) = return 4
    sizeof (Jge) = return 4
    sizeof (Jb) = return 4
    sizeof (Jbe) = return 4
    sizeof (Adds) = return 6
    sizeof (Subs) = return 6
    sizeof (OpCode11) = error $ printf "undefined: sizeof OpCode11"
    sizeof (OpCode12) = error $ printf "undefined: sizeof OpCode12"
    sizeof (OpCode13) = error $ printf "undefined: sizeof OpCode13"
    sizeof (Return) = return 2
    sizeof (Callid) = return 4
    sizeof (Playoid) = return 4
    sizeof (Pause) = return 4

