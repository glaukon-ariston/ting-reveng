{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}
{-
Author: Glaukon Ariston
Date: 27.07.2016
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

module Ting.Assembler2 where

import Prelude hiding (and, or, not)
import Data.Word (Word16)
import Control.Exception (assert)

import Control.Applicative (Applicative(..), (<$>))
import Control.Monad.RWS (RWS, execRWS, rws, get)
import Control.Monad.Fix (MonadFix(..))
import qualified Data.Foldable as F (toList)

import Text.Printf (printf)

import Ting.Operand  (Operand(..), OperandCode(..))
import Ting.Registers (rsp)
import Ting.Instructions (Instructions(..), SnippetType(Subroutine, SubroutineX), Program, SoundLib, skip, initStack, (>>.))
import qualified Ting.Instructions as I (resolveProcedure, resolveSound)


i# :: (Integral a, Num b) => a -> b
i# = fromIntegral

-- ================================================================= Assembler

data Instruction = END
    | CLEARVER
    | SET Operand Operand
    | CMP Operand Operand
    | AND Operand Operand
    | OR Operand Operand
    | NOT Operand
    | OPCODE07 Operand
    | JMP Operand
    | JE Operand
    | JNE Operand
    | JG Operand
    | JGE Operand
    | JB Operand
    | JBE Operand
    | ADDS Operand Operand
    | SUBS Operand Operand
    | OPCODE11 Operand
    | OPCODE12 Operand
    | OPCODE13 Operand
    | RETURN
    | CALLID Operand
    | PLAYOID Operand
    | PAUSE Operand
    {- … -}
    deriving Show


data AsmResult = AsmResult
    { _name :: String
    , _code :: [Instruction]
    , _location :: Word16
    , _tingIdBase' :: Word16
    }

data AsmEnv = AsmEnv
    { _tingIdBase :: Word16
    , _program :: Program Assembler ()
    , _soundLib :: SoundLib
    }

newtype Assembler a = Assembler (RWS AsmEnv [Instruction] Word16 a)
    deriving (Functor, Applicative, Monad, MonadFix)


disasmCode :: String -> [Instruction] -> Int -> Int -> [String]
disasmCode name code tingId pc = [name] ++ (map disasm1 $ zip [0..] code)
    where
        disasm1 :: (Int, Instruction) -> String
        disasm1 (addr, instr) = printf "%05d:%04x:%s %s" tingId addr (pointer addr) (show instr)
        pointer :: Int -> String
        pointer addr = if addr == i# pc then ">" else " "

disasmWindow :: String -> [Instruction] -> Int -> Int -> Int -> [String]
disasmWindow name code tingId pc window = take window . snd . splitAt startAt $ disasmCode name code tingId pc
    where
        startAt = max 0 $ i# pc - (window `div` 2)

instance Show AsmResult where
    show (AsmResult {..}) = 
        unlines
        [ unlines $ disasmCode _name _code (i# _tingIdBase') 0
        , show _location
        , show _tingIdBase'
        ]


{- Append the list of instructions to the code stream. -}
append :: [Instruction] -> Assembler ()
append xs = Assembler . rws $ \_ loc -> ((), loc + (i# . length $ xs), xs)

{- Maps the procedure name to a unique tingId. -}
resolveProcedure :: String -> Assembler (String, Word16)
resolveProcedure proc = Assembler . rws $ go
    where
        go env loc = 
            ( case I.resolveProcedure proc (_tingIdBase env) (_program env) of
                Right location -> location
                Left msg -> error msg
            , loc
            , [] )

{- Maps the sound name to a unique tingId. -}
resolveSound :: String -> Assembler (String, Word16)
resolveSound sound = Assembler . rws $ go
    where
        go env loc = 
            ( case I.resolveSound sound (_tingIdBase env) (_program env) (_soundLib env) of
                Right location -> location
                Left msg -> error msg
            , loc
            , [] )

{- Assemble the program, by generating the machine code and fixing the referenced locations. 
Also generates the tingId(s) and fixes them in the call site. -}
asm :: Bool -> Int -> Program Assembler () -> SoundLib -> [[Instruction]]
asm naked tingIdBase program soundLib = map (go naked) program
    where
        go False (_, snippetType, snippet) = 
            let (Assembler snippet') = appendEpilogue snippetType . prependProlog snippetType $ snippet 
            in F.toList . snd . execRWS snippet' env $ 0
        go True (_, _, (Assembler snippet)) = F.toList . snd . execRWS snippet env $ 0
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

asm2 :: Int -> Program Assembler () -> SoundLib -> [AsmResult]
asm2 tingIdBase program soundLib = map go program
    where
        go (name, snippetType, snippet) = 
            let (Assembler snippet') = appendEpilogue snippetType . prependProlog snippetType $ snippet 
                (location,code) = execRWS snippet' env $ 0
            in AsmResult 
                { _name = name 
                , _code = code
                , _location = location
                , _tingIdBase' = fromIntegral tingIdBase }
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

instance Instructions Assembler where 
    end = append [END]
    clearver = append [CLEARVER]
    set op1 op2 = append [SET op1 op2]
    cmp op1 op2 = append [CMP op1 op2]
    and op1 op2 = append [AND op1 op2]
    or op1 op2 = append [OR op1 op2]
    not op1 = append [NOT op1]
    opCode07 op1 = append [OPCODE07 op1]
    
    jmp op1 = append [JMP op1]
    je op1 = append [JE op1]
    jne op1 = append [JNE op1]
    jg op1 = append [JG op1]
    jge op1 = append [JGE op1]
    jb op1 = append [JB op1]
    jbe op1 = append [JBE op1]
    
    adds op1 op2 = append [ADDS op1 op2]
    subs op1 op2 = append [SUBS op1 op2]
    opCode11 op1 = append [OPCODE11 op1]
    opCode12 op1 = append [OPCODE12 op1]
    opCode13 op1 = append [OPCODE13 op1]
    
    ret = append [RETURN]
    callid op1 = append [CALLID op1]
    playoid op1 = append [PLAYOID op1]
    pause op1 = append [PAUSE op1]

    {- The label function returns the current index of the output stream. -}
    label = A <$> Assembler get

    sizeof (End) = return 1
    sizeof (Clearver) = return 1
    sizeof (Set) = return 1
    sizeof (Cmp) = return 1
    sizeof (And) = return 1
    sizeof (Or) = return 1
    sizeof (Not) = return 1
    sizeof (OpCode07) = assert False undefined
    sizeof (Jmp) = return 1
    sizeof (Je) = return 1
    sizeof (Jne) = return 1
    sizeof (Jg) = return 1
    sizeof (Jge) = return 1
    sizeof (Jb) = return 1
    sizeof (Jbe) = return 1
    sizeof (Adds) = return 1
    sizeof (Subs) = return 1
    sizeof (OpCode11) = assert False undefined
    sizeof (OpCode12) = assert False undefined
    sizeof (OpCode13) = assert False undefined
    sizeof (Return) = return 1
    sizeof (Callid) = return 1
    sizeof (Playoid) = return 1
    sizeof (Pause) = return 1
