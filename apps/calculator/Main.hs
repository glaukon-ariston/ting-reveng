{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-
Author: Glaukon Ariston
Date: 21.11.2016
Abstract:
    A simple paper + Ting calculator, inspired by 'Ein Tiptoi-Taschenrechner' by Joachim Breitner.

    https://www.joachim-breitner.de/blog/669-Ein_Tiptoi-Taschenrechner
-}

module Main where

import Prelude hiding (and, or, not, return, getContents, putStr, putStrLn)
import IOUtil (putStr, putStrLn)

import Control.Monad.Fix (MonadFix(..))
import Data.List (findIndex)
import Data.Word (Word16)
import Text.Printf (printf)

import Ting.Instructions
import Ting.Registers
import Ting.Operand (Operand(..))
import Ting.Linker (makeBook, uploadBook, BookDescription(..))
import Ting.Assembler2 (Assembler)
import Ting.Repl (repl)
import Ting.Common (e2i, i#)

bookId :: Int
bookId = 8001

tingBaseId :: Int
tingBaseId = 15001

bookDesc :: BookDescription 
bookDesc = BookDescription 
    { _name = "Ting Calculator"
    , _publisher = "Glaukon Ariston"
    , _author = "Glaukon Ariston"
    , _bookVersion = "1"
    , _url = "http://github.com"
    , _thumb = ""
    , _file = ""
    }

speakers :: [(String, FilePath)]
speakers = [("L.", lovre), ("V.", vjeko), ("M.", mikula), ("T.", tata), ("M2.", marijana), ("N2.", neven)]
    where
        rootDir = "./apps/calculator/sounds/"
        language = "hr"
        lovre = rootDir ++ language ++ "/lovre/" 
        vjeko = rootDir ++ language ++ "/vjeko/" 
        mikula = rootDir ++ language ++ "/mikula/" 
        tata = rootDir ++ language ++ "/tata/" 
        marijana = rootDir ++ language ++ "/marijana/" 
        neven = rootDir ++ language ++ "/neven/" 

sounds :: [(String, FilePath)]
sounds = 
    [ ("00", "00.mp3" )
    , ("01", "01.mp3" )
    , ("02", "02.mp3" )
    , ("03", "03.mp3" )
    , ("04", "04.mp3" )
    , ("05", "05.mp3" )
    , ("06", "06.mp3" )
    , ("07", "07.mp3" )
    , ("08", "08.mp3" )
    , ("09", "09.mp3" )
    , ("10", "10.mp3" )
    , ("11", "11.mp3" )
    , ("12", "12.mp3" )
    , ("13", "13.mp3" )
    , ("14", "14.mp3" )
    , ("15", "15.mp3" )
    , ("16", "16.mp3" )
    , ("17", "17.mp3" )
    , ("18", "18.mp3" )
    , ("19", "19.mp3" )
    , ("20", "20.mp3" )
    , ("30", "30.mp3" )
    , ("40", "40.mp3" )
    , ("50", "50.mp3" )
    , ("60", "60.mp3" )
    , ("70", "70.mp3" )
    , ("80", "80.mp3" )
    , ("90", "90.mp3" )
    , ("100", "100.mp3" )
    , ("200", "200.mp3" )
    , ("300", "300.mp3" )
    , ("400", "400.mp3" )
    , ("500", "500.mp3" )
    , ("600", "600.mp3" )
    , ("700", "700.mp3" )
    , ("800", "800.mp3" )
    , ("900", "900.mp3" )
    , ("1000", "1000.mp3" )
    , ("1000p", "1000p.mp3" )
    , ("1e6", "1e6.mp3" )
    , ("1e6p", "1e6p.mp3" )
    , ("1e9", "1e9.mp3" )
    , ("1e9p", "1e9p.mp3" )
    , ("1e12", "1e12.mp3" )
    , ("1e12p", "1e12p.mp3" )
    , ("01f", "01f.mp3" )
    , ("02f", "02f.mp3" )
    , ("+", "plus.mp3")
    , ("-", "minus.mp3")
    , ("*", "multiply.mp3")
    , ("/", "divide.mp3")
    , ("=", "equals.mp3")
    , ("C", "clear.mp3")
    , ("CE", "cancelEntry.mp3")
    , ("!error", "error.mp3")
    , ("click", "click.mp3")
    , ("speaker", "speaker.mp3")
    ]

soundLib :: SoundLib
soundLib = 
    [ (prefix ++ tag, filepath ++ filename) 
    | (prefix, filepath) <- speakers
    , (tag, filename) <- sounds ]

soundIndex :: String -> Word16
soundIndex s = case findIndex (\(tag,_) -> tag == s) sounds of
    Nothing -> error $ printf "soundIndex: unknown sound '%s'" s
    Just i -> i# i

---------------------------------------------------------------- Code

data ArithmeticOperation = Nop | Plus | Minus | Multiply | Divide deriving (Eq, Enum)

-- allocate global data (registers)
speaker :: Operand
speaker = r0

operandStack :: [Register]
operandStack = [R1 .. R4] -- three elements deep (sp + data)

methodStack :: [Register]
methodStack = [R5 .. R7] -- two elements deep (sp + data)

localRegisters :: Register
localRegisters = succ $ last methodStack

applyOp :: (MonadFix m, Instructions m) => Operand -> Operand -> Operand -> m ()
applyOp op x y = mdo
    caseOf op (I . e2i)
        [ (Nop, nop)
        , (Plus, add x y)
        , (Minus, sub x y)
        , (Multiply, mul x y)
        , (Divide, divModulo x y)
        ]

evalTop :: (MonadFix m, Instructions m) => m ()
evalTop = mdo
    let f = register localRegisters
    let x = register $ succ localRegisters
    let y = register $ succ . succ $ localRegisters

    stackPop f methodStack
    stackPop y operandStack
    stackPop x operandStack
    applyOp f x y
    stackPush x operandStack

evalFull :: (MonadFix m, Instructions m) => m ()
evalFull = mdo
    loopPre (stackEmpty methodStack >>. jne) $ mdo
        evalTop

pushOp :: (MonadFix m, Instructions m) => ArithmeticOperation -> m ()
pushOp op = mdo
    if any (== op) [Plus, Minus]
        then evalFull
        else mdo
            -- When the method is not one of [+,-] then
            -- partially evaluate the method stack if:
                -- the stack is not empty 
                -- and the most recent method is not one of [+,-]
            skip (stackEmpty methodStack >>. je) $ mdo
                let f = register localRegisters

                stackTopWith (\r -> set f r) methodStack
                skip (testAny f [Plus, Minus] >>. je) $ mdo
                    evalTop
    skip (cmp (stackPointer operandStack) 1 >>. jg) $ mdo
        stackPush 0 operandStack
    stackPush (I . e2i $ op) methodStack

clearStack :: (MonadFix m, Instructions m) => m ()
clearStack = mdo
        stackInit operandStack
        stackPush 0 operandStack
        stackInit methodStack

clearAll :: (MonadFix m, Instructions m) => m ()
clearAll = mdo
        set speaker (S . fst . head $ soundLib)
        clearStack

playSound :: (MonadFix m, Instructions m) => String -> m ()
playSound s = playIndex (I . soundIndex $ s) speaker

program :: (MonadFix m, Instructions m) => Program m ()
program = map prependPrefix $ 
    --------------------------------------------------- Digits
    map processDigit ['0' .. '9']
    --------------------------------------------------- Operations
    ++ [ 
    ("__initialise__", SubroutineX, mdo
        clearAll
    )
    , ("+", TopLevel, mdo
        pushOp Plus
        playSound "+"
    )
    , ("-", TopLevel, mdo
        pushOp Minus
        playSound "-"
    )
    , ("*", TopLevel, mdo
        pushOp Multiply
        playSound "*"
    )
    , ("/", TopLevel, mdo
        pushOp Divide
        playSound "/"
    )
    , ("=", TopLevel, mdo
        let accumulator = register localRegisters
        let zeroId = register . succ $ localRegisters

        playSound "="
        evalFull
        stackTopWith (\r -> set accumulator r) operandStack
        -- speak
        set zeroId (I . soundIndex $ "00")
        add zeroId speaker
        speakNaturalHr accumulator zeroId

        stackPush 0 operandStack
    )
    , ("C", TopLevel, mdo
        clearStack
        playSound "C"
    )
    , ("CE", TopLevel, mdo
        stackTopWith (\r -> set r 0) operandStack
        playSound "CE"
    )
    ] 
    --------------------------------------------------- Extra
    ++ [ ("LCD", TopLevel, mdo
        let operand = register localRegisters
        let zeroId = register . succ $ localRegisters
        
        stackTopWith (\r -> set operand r) operandStack
        -- speak
        set zeroId (I . soundIndex $ "00")
        add zeroId speaker
        speakNaturalHr operand zeroId
    )
    , (".", TopLevel, mdo
        playSound "!error"
    )
    , ("ON-OFF", TopLevel, mdo
        playSound "!error"
    )]
    --------------------------------------------------- Speakers
    ++ map selectSpeaker speakers
    where
        prependPrefix (name,TopLevel,code) = (name, TopLevel, mdo
            disableInterrupt $ mdo
                playSound "click"
                code
            )
        prependPrefix x = x

        selectSpeaker (prefix,_) = (prefix, TopLevel, mdo
            let key = register localRegisters

            set key rlastoid
            subs key (P . fst . head $ speakers)
            mul key (I . i# . length $ sounds)

            set speaker (S . fst . head $ soundLib)
            adds speaker key
            playSound "speaker"
            )

        processDigit d = ((d:[]), TopLevel, mdo
            let key = register localRegisters
            let operand = register . succ $ localRegisters
            let zeroId = register . succ . succ $ localRegisters

            set key rlastoid
            subs key (P "0")
            stackTopWith (\r -> set operand r) operandStack
            mul10s operand
            adds operand key
            stackTopWith (\r -> set r operand) operandStack
            -- speak
            set zeroId (I . soundIndex $ "00")
            add zeroId speaker
            speakNaturalHr operand zeroId
            )

saveBook :: IO ()
saveBook = do
    makeBook naked bookId bookDesc tingBaseId (program++prelude) (soundLib++preludeSoundLib) "books"
    uploadBook bookId "books"
    where naked = False

evalBook :: IO ()
evalBook = repl bookId tingBaseId (program++prelude) (soundLib++preludeSoundLib)

main :: IO ()
main = do
    putStrLn "Ting Calculator"
    saveBook
    evalBook

