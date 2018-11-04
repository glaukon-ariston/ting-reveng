{-# LANGUAGE OverloadedStrings #-}
{-
Author: Glaukon Ariston
Date: 26.07.2016
Abstract:
-}

module Ting.Operand where

import Data.Word (Word16)
import Data.String (IsString(..))


data OperandCode = End | Clearver | Set | Cmp | And | Or | Not | OpCode07 | Jmp | Je | Jne | Jg | Jge | Jb | Jbe | Adds | Subs | OpCode11 | OpCode12 | OpCode13 | Return | Callid | Playoid | Pause deriving (Enum, Show)
data OperandType = OpNone | OpRegVal | OpRegReg deriving (Enum)
type Location = Operand

data Operand = 
    R Word16    -- registers
    | I Word16  -- immediate value (integer)
    | A Word16  -- address (Location)
    | P String  -- procedure
    | S String  -- sound
    deriving (Eq)


{-
See TingRegister.hs for 

instance Show Operand where
    ...
-}


instance Num Operand where
    (I x) + (I y) = I (x + y)
    _ + _ = undefined   
    (I x) - (I y) = I (x - y)
    _ - _ = undefined   
    (I x) * (I y) = I (x * y)
    _ * _ = undefined   
    negate (I x) = I (x)
    negate _ = undefined    
    abs x = x
    signum 0 = 0
    signum _ = 1
    fromInteger i = I (fromInteger i)
{-
    (+) (R _) _ = undefined
    (+) (S _) _ = undefined
    (+) (I _) (R _) = undefined
    (+) (I _) (S _) = undefined

    (-) (R _) _ = undefined
    (-) (S _) _ = undefined
    (-) (I _) (R _) = undefined
    (-) (I _) (S _) = undefined

    (*) (R _) _ = undefined
    (*) (S _) _ = undefined
    (*) (I _) (R _) = undefined
    (*) (I _) (S _) = undefined

    negate (R _) = undefined
    negate (S _) = undefined
-}

{- The string operand is assumed to represent a procedure name -}
instance IsString Operand where
    fromString = P . fromString



